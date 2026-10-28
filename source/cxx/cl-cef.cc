/*
 * SPDX-FileCopyrightText: Atlas Engineer LLC
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * In-process CEF bridge. One UI thread runs CefRunMessageLoop. Every other
 * thread posts work onto it. Object ids are the handles Lisp holds.
 */

#include "cl-cef.h"
#include "schemes.hh"

#include "include/cef_app.h"
#include "include/cef_browser.h"
#include "include/cef_client.h"
#include "include/cef_command_handler.h"
#include "include/cef_command_ids.h"
#include "include/cef_parser.h"
#include "include/cef_response_filter.h"
#include "include/base/cef_build.h"
#include "include/views/cef_box_layout.h"
#include "include/views/cef_browser_view.h"
#include "include/views/cef_browser_view_delegate.h"
#include "include/views/cef_window.h"
#include "include/views/cef_window_delegate.h"
#include "include/wrapper/cef_helpers.h"
#include "include/wrapper/cef_message_router.h"

#include <algorithm>
#include <atomic>
#if defined(__x86_64__)
#include <cstdint>
#endif
#include <cmath>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#ifndef _WIN32
#include <dirent.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#endif
#if defined(__linux__)
#include <sys/prctl.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
// X.h defines this as 0, which breaks CefCallback::Success.
#undef Success
#endif

struct cl_cef_response {
  std::string data;
  std::string mime = "text/html";
  int status = 200;
  std::string status_text = "OK";
  std::string headers;
};

struct cl_cef_request_decision {
  int block = 0;
  std::string css;
  // Length-prefixed needle/replacement pairs. Empty when the body passes through.
  std::string replacements;
};

// SBCL leaves the invalid, overflow, and divide-by-zero traps unmasked for the
// whole process. Chromium threads inherit that and a later floating-point
// operation (the prompt's layout hit one) is delivered as SIGFPE to Lisp.
void MaskFpTraps() {
#if defined(__x86_64__)
  unsigned int mxcsr = 0;
  asm volatile("stmxcsr %0" : "=m"(mxcsr));
  mxcsr |= 0x3fu << 7;
  asm volatile("ldmxcsr %0" ::"m"(mxcsr));
  unsigned short control = 0;
  asm volatile("fnstcw %0" : "=m"(control));
  control = static_cast<unsigned short>(control | 0x3fu);
  asm volatile("fldcw %0" ::"m"(control));
#else
#endif
}

class ClosureTask : public CefTask {
 public:
  explicit ClosureTask(std::function<void()> fn) : fn_(std::move(fn)) {}
  void Execute() override {
    MaskFpTraps();
    fn_();
  }

 private:
  std::function<void()> fn_;
  IMPLEMENT_REFCOUNTING(ClosureTask);
};

// CefCallback::Continue resumes a load only when it runs on the UI thread.
void PostCefCallback(CefRefPtr<CefCallback> async, bool cancel) {
  if (!async)
    return;
  CefPostTask(TID_UI, new ClosureTask([async, cancel]() {
    if (cancel)
      async->Cancel();
    else
      async->Continue();
  }));
}

namespace {

struct UserScript {
  int id = 0;
  std::string code;
  bool at_start = false;
  bool all_frames = true;
};

struct JsWaiter {
  bool done = false;
  bool success = false;
  std::string json = "null";
  std::mutex mu;
  std::condition_variable cv;
};

class DevToolsObserver : public CefDevToolsMessageObserver {
 public:
  void Expect(int id, std::shared_ptr<JsWaiter> waiter) {
    std::lock_guard<std::mutex> lock(mu_);
    waiters_[id] = std::move(waiter);
  }

  void OnDevToolsMethodResult(CefRefPtr<CefBrowser> browser,
                              int message_id,
                              bool success,
                              const void* result,
                              size_t result_size) override {
    (void)browser;
    std::shared_ptr<JsWaiter> waiter;
    {
      std::lock_guard<std::mutex> lock(mu_);
      auto found = waiters_.find(message_id);
      if (found == waiters_.end())
        return;
      waiter = found->second;
      waiters_.erase(found);
    }
    std::string json = "null";
    bool ok = success && Decode(result, result_size, json);
    {
      std::lock_guard<std::mutex> lock(waiter->mu);
      waiter->done = true;
      waiter->success = ok;
      waiter->json = std::move(json);
    }
    waiter->cv.notify_one();
  }

 private:
  static bool Decode(const void* result, size_t result_size, std::string& json) {
    if (!result || result_size == 0)
      return false;
    std::string text(static_cast<const char*>(result), result_size);
    CefRefPtr<CefValue> root = CefParseJSON(text, JSON_PARSER_RFC);
    if (!root || root->GetType() != VTYPE_DICTIONARY)
      return false;
    CefRefPtr<CefDictionaryValue> dict = root->GetDictionary();
    if (!dict || dict->HasKey("exceptionDetails"))
      return false;
    if (!dict->HasKey("result") || dict->GetType("result") != VTYPE_DICTIONARY)
      return false;
    CefRefPtr<CefDictionaryValue> body = dict->GetDictionary("result");
    if (!body)
      return false;
    if (body->HasKey("value")) {
      CefString written = CefWriteJSON(body->GetValue("value"), JSON_WRITER_DEFAULT);
      json = written.ToString();
      if (json.empty())
        json = "null";
      return true;
    }
    json = "null";
    return body->GetString("type") == "undefined";
  }

  std::mutex mu_;
  std::unordered_map<int, std::shared_ptr<JsWaiter>> waiters_;
  IMPLEMENT_REFCOUNTING(DevToolsObserver);
};

struct ViewRecord {
  int id = 0;
  CefRefPtr<CefBrowserView> widget;
  CefRefPtr<CefBrowser> browser;
  CefRefPtr<CefRequestContext> context;
  CefRefPtr<DevToolsObserver> devtools_observer;
  CefRefPtr<CefRegistration> devtools_registration;
  std::vector<std::function<void(CefRefPtr<CefBrowser>)>> pending;
  std::string title;
  std::string user_agent;
  std::string proxy;
  bool focused = false;
  bool javascript = true;
  bool images = true;
  bool webgl = true;
  bool muted = false;
  int x = 0;
  int y = 0;
  int width = 0;
  int height = 0;
  // A bar view keeps this height. The page view stays flexible and fills
  // whatever the bars do not use. Browser views otherwise prefer a window-sized
  // rectangle, which is why the status text (sized in vh) became huge.
  bool bar = false;
  // Status, message, and prompt documents are about:blank. The shared context
  // remembers a zoom for that URL, and every about:blank view inherits it.
  bool isolated = false;
  int bar_height = 0;
  bool flex_set = false;
  // Set while this view is being destroyed. DoClose uses it to close the
  // browser without also closing the window that hosts the other views.
  bool destroying = false;
  // CloseBrowser has been asked. A later turn must not drop the view out
  // from under that close.
  bool close_started = false;
  int cookie_policy = 0;
  int next_script_id = 1;
  int next_css_id = 1;
  int devtools_message_id = 0;
  std::vector<UserScript> scripts;
  std::unordered_map<std::string, std::string> css;
};

struct WindowRecord {
  int id = 0;
  CefRefPtr<CefWindow> window;
  CefRefPtr<CefWindowDelegate> delegate;
  CefRefPtr<CefBoxLayout> layout;
  int width = 1600;
  int height = 1200;
  std::string title;
};

std::mutex g_mu;
int g_next_id = 1;
std::unordered_map<int, std::shared_ptr<ViewRecord>> g_views;
std::unordered_map<int, std::shared_ptr<WindowRecord>> g_windows;
std::unordered_map<int, int> g_browser_to_view;
std::unordered_map<uint32_t, CefRefPtr<CefDownloadItemCallback>> g_download_callbacks;
std::string g_download_directory;
CefRefPtr<CefMenuModel> g_menu;
std::atomic<cl_cef_event_callback> g_event_cb{nullptr};
std::atomic<cl_cef_scheme_callback> g_scheme_cb{nullptr};
// Names registered with the global request context. An isolated context does
// not inherit those factories, so a panel view would fail every lisp: fetch.
std::vector<std::string> g_scheme_names;
std::atomic<bool> g_initialized{false};
std::atomic<bool> g_quitting{false};
std::atomic<bool> g_loop_done{false};
// Browsers still alive. Released on the UI thread, before CefShutdown.
std::mutex g_browser_mu;
std::vector<CefRefPtr<CefBrowser>> g_browsers;
// True while CloseBrowser is running, so a synchronous OnBeforeClose does not
// quit the message loop before the rest of the browsers are closed.
bool g_closing_browsers = false;

// Scheme handlers are entered on CEF's IO thread, which is not a Lisp thread.
// The request is queued and continued later. Waiting here blocks every other
// load, including adblock decisions, because they share that thread.
struct SchemeJob {
  std::string scheme;
  std::string url;
  cl_cef_response response;
  std::promise<void> done;
  std::mutex mu;
  CefRefPtr<CefCallback> async;
};

std::mutex g_scheme_mu;
std::condition_variable g_scheme_cv;
std::queue<std::shared_ptr<SchemeJob>> g_scheme_queue;
std::atomic<bool> g_scheme_stop{false};

// Adblock decisions also run on a Lisp thread. The IO thread cannot enter SBCL.
struct RequestJob {
  std::string url;
  std::string document_url;
  std::string type;
  uint64_t id = 0;
  cl_cef_request_decision decision;
  std::promise<void> done;
  // Set for resource loads. Continue or Cancel is called on the IO thread.
  CefRefPtr<CefCallback> async;
  std::atomic<bool> settled{false};
};

std::mutex g_request_mu;
std::condition_variable g_request_cv;
std::queue<std::shared_ptr<RequestJob>> g_request_queue;
std::atomic<bool> g_request_stop{false};
std::atomic<cl_cef_request_callback> g_request_cb{nullptr};

std::mutex g_cosmetic_mu;
std::unordered_map<std::string, std::shared_ptr<const std::string>> g_cosmetic_by_url;
std::unordered_map<std::string, std::shared_ptr<const std::string>> g_cosmetic_by_host;
std::unordered_map<std::size_t, std::vector<std::shared_ptr<const std::string>>> g_cosmetic_intern;

std::vector<std::string> g_args;
std::vector<char*> g_argv;

void PostUI(std::function<void()> fn) {
  if (CefCurrentlyOn(TID_UI)) {
    MaskFpTraps();
    fn();
  } else {
    CefPostTask(TID_UI, new ClosureTask(std::move(fn)));
  }
}

template <typename F>
auto CallUI(F&& fn) -> decltype(fn()) {
  using R = decltype(fn());
  if (CefCurrentlyOn(TID_UI)) {
    MaskFpTraps();
    return fn();
  }
  auto promise = std::make_shared<std::promise<R>>();
  CefPostTask(TID_UI, new ClosureTask([promise, fn = std::function<R()>(std::forward<F>(fn))]() {
                try {
                  if constexpr (std::is_void_v<R>) {
                    fn();
                    promise->set_value();
                  } else {
                    promise->set_value(fn());
                  }
                } catch (...) {
                  promise->set_exception(std::current_exception());
                }
              }));
  return promise->get_future().get();
}

char* Dup(const std::string& text) {
  char* copy = static_cast<char*>(std::malloc(text.size() + 1));
  if (!copy)
    return nullptr;
  std::memcpy(copy, text.data(), text.size() + 1);
  return copy;
}

std::string JsonEscape(const std::string& text) {
  std::string out;
  out.reserve(text.size() + 8);
  for (unsigned char character : text) {
    switch (character) {
      case '"': out += "\\\""; break;
      case '\\': out += "\\\\"; break;
      case '\n': out += "\\n"; break;
      case '\r': out += "\\r"; break;
      case '\t': out += "\\t"; break;
      default:
        if (character < 0x20) {
          char buffer[8];
          std::snprintf(buffer, sizeof buffer, "\\u%04x", character);
          out += buffer;
        } else {
          out += static_cast<char>(character);
        }
    }
  }
  return out;
}

std::string JsonString(const std::string& text) {
  return "\"" + JsonEscape(text) + "\"";
}

int Emit(int object_id, const std::string& json) {
  cl_cef_event_callback callback = g_event_cb.load();
  if (!callback)
    return 0;
  return callback(object_id, json.c_str());
}

std::shared_ptr<ViewRecord> FindView(int id) {
  std::lock_guard<std::mutex> lock(g_mu);
  auto found = g_views.find(id);
  return found == g_views.end() ? nullptr : found->second;
}

// Release the native view. The CefBrowserView has to outlive its browser:
// dropping it while the browser is still closing runs ~CefViewView, which
// CHECKs on the UI thread. Call this from OnBrowserDestroyed, after close.
void DropView(const std::shared_ptr<ViewRecord>& record) {
  if (!record)
    return;
  {
    std::lock_guard<std::mutex> lock(g_mu);
    auto found = g_views.find(record->id);
    if (found == g_views.end() || found->second.get() != record.get())
      return;
    g_views.erase(found);
  }
  record->pending.clear();
  record->browser = nullptr;
  record->devtools_registration = nullptr;
  record->devtools_observer = nullptr;
  record->widget = nullptr;
  record->context = nullptr;
}

std::shared_ptr<WindowRecord> FindWindow(int id) {
  std::lock_guard<std::mutex> lock(g_mu);
  auto found = g_windows.find(id);
  return found == g_windows.end() ? nullptr : found->second;
}

int AllocateId() {
  std::lock_guard<std::mutex> lock(g_mu);
  return g_next_id++;
}

void WithBrowser(const std::shared_ptr<ViewRecord>& view,
                 const std::function<void(CefRefPtr<CefBrowser>)>& fn) {
  // A view being destroyed must not queue work that runs against a browser
  // that is about to close.
  if (!view || view->destroying)
    return;
  if (view->browser)
    fn(view->browser);
  else
    view->pending.push_back(fn);
}

std::string Base64(const std::string& input) {
  static const char table[] =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  std::string out;
  out.reserve(((input.size() + 2) / 3) * 4);
  unsigned value = 0;
  int bits = -6;
  for (unsigned char character : input) {
    value = (value << 8) + character;
    bits += 8;
    while (bits >= 0) {
      out.push_back(table[(value >> bits) & 0x3f]);
      bits -= 6;
    }
  }
  if (bits > -6)
    out.push_back(table[((value << 8) >> (bits + 8)) & 0x3f]);
  while (out.size() % 4)
    out.push_back('=');
  return out;
}

double FactorToLevel(double factor) {
  if (!(factor > 0.0))
    return 0.0;
  return std::log(factor) / std::log(1.2);
}

double LevelToFactor(double level) {
  return std::pow(1.2, level);
}

void ApplyCss(CefRefPtr<CefFrame> frame, const std::string& key, const std::string& css) {
  if (!frame)
    return;
  std::string script =
      "(()=>{var id=" + JsonString(key) +
      ";var node=document.getElementById(id);"
      "if(!node){node=document.createElement('style');node.id=id;"
      "(document.head||document.documentElement).appendChild(node);}"
      "node.textContent=" + JsonString(css) + ";})()";
  frame->ExecuteJavaScript(script, frame->GetURL(), 0);
}

void ApplyScripts(const std::shared_ptr<ViewRecord>& view,
                  CefRefPtr<CefFrame> frame,
                  bool at_start) {
  if (!view || !frame)
    return;
  for (const auto& script : view->scripts) {
    if (script.at_start != at_start)
      continue;
    if (!script.all_frames && !frame->IsMain())
      continue;
    frame->ExecuteJavaScript(script.code, frame->GetURL(), 0);
  }
  if (at_start && frame->IsMain()) {
    for (const auto& entry : view->css)
      ApplyCss(frame, entry.first, entry.second);
  }
}

void SetBoolPreference(CefRefPtr<CefRequestContext> context,
                       const char* name,
                       bool enabled) {
  if (!context)
    return;
  CefRefPtr<CefValue> value = CefValue::Create();
  value->SetBool(enabled);
  CefString error;
  context->SetPreference(name, value, error);
}

void SetProxyPreference(CefRefPtr<CefRequestContext> context,
                        const std::string& server,
                        const std::string& bypass) {
  if (!context)
    return;
  CefRefPtr<CefDictionaryValue> proxy = CefDictionaryValue::Create();
  if (server.empty()) {
    proxy->SetString("mode", "direct");
  } else {
    proxy->SetString("mode", "fixed_servers");
    proxy->SetString("server", server);
    if (!bypass.empty())
      proxy->SetString("bypass_list", bypass);
  }
  CefRefPtr<CefValue> value = CefValue::Create();
  value->SetDictionary(proxy);
  CefString error;
  context->SetPreference("proxy", value, error);
}

void SetCookiePreference(CefRefPtr<CefRequestContext> context, int policy) {
  if (!context)
    return;
  // Replacing the whole dictionary drops the other content settings. Chromium
  // then reads those missing entries as strings and faults.
  CefRefPtr<CefValue> existing =
      context->GetPreference("profile.default_content_setting_values");
  CefRefPtr<CefDictionaryValue> content;
  if (existing && existing->GetType() == VTYPE_DICTIONARY && existing->GetDictionary())
    content = existing->GetDictionary()->Copy(false);
  else
    content = CefDictionaryValue::Create();
  // Content setting 1 is allow, 2 is block. Third-party blocking is separate.
  content->SetInt("cookies", policy == 1 ? 2 : 1);
  CefRefPtr<CefValue> content_value = CefValue::Create();
  content_value->SetDictionary(content);
  CefString error;
  context->SetPreference("profile.default_content_setting_values", content_value, error);
  CefRefPtr<CefValue> third_party = CefValue::Create();
  third_party->SetBool(policy == 2);
  context->SetPreference("profile.block_third_party_cookies", third_party, error);
}

class SchemeHandler : public CefResourceHandler {
 public:
  explicit SchemeHandler(std::string scheme) : scheme_(std::move(scheme)) {}

  bool Open(CefRefPtr<CefRequest> request,
            bool& handle_request,
            CefRefPtr<CefCallback> callback) override {
    if (!request || !callback) {
      handle_request = true;
      return false;
    }
    // No Lisp handler yet. Answer an empty document, matching the previous
    // synchronous path, instead of failing the navigation.
    if (!g_scheme_cb.load()) {
      handle_request = true;
      return true;
    }
    auto job = std::make_shared<SchemeJob>();
    job->scheme = scheme_;
    job->url = request->GetURL().ToString();
    job->async = callback;
    // Publish the job before queueing it. The pump can finish and CEF can ask
    // for headers as soon as the continuation runs.
    job_ = job;
    {
      std::lock_guard<std::mutex> lock(g_scheme_mu);
      if (g_scheme_stop.load()) {
        handle_request = true;
        return false;
      }
      g_scheme_queue.push(job);
    }
    g_scheme_cv.notify_one();
    handle_request = false;
    return true;
  }

  void GetResponseHeaders(CefRefPtr<CefResponse> response,
                          int64_t& response_length,
                          CefString& redirect_url) override {
    (void)redirect_url;
    TakeResponse();
    if (response) {
      response->SetStatus(status_);
      response->SetStatusText(status_text_);
      // SetMimeType rejects parameters. "text/html;charset=utf-8" is not HTML
      // to Chromium, so the tags are drawn as text.
      std::string mime = mime_;
      std::string charset;
      std::size_t semi = mime.find(';');
      if (semi != std::string::npos) {
        std::string params = mime.substr(semi + 1);
        mime.resize(semi);
        const std::string key = "charset=";
        std::size_t at = params.find(key);
        if (at != std::string::npos) {
          charset = params.substr(at + key.size());
          std::size_t end = charset.find_first_of(" ;");
          if (end != std::string::npos)
            charset.resize(end);
        }
      }
      while (!mime.empty() && mime.back() == ' ')
        mime.pop_back();
      response->SetMimeType(mime.empty() ? "text/html" : mime);
      if (charset.empty() && (mime.rfind("text/", 0) == 0 || mime == "application/javascript" ||
                              mime == "application/json"))
        charset = "utf-8";
      if (!charset.empty())
        response->SetCharset(charset);
      std::size_t start = 0;
      while (start < headers_.size()) {
        std::size_t end = headers_.find('\n', start);
        if (end == std::string::npos)
          end = headers_.size();
        std::string line = headers_.substr(start, end - start);
        if (!line.empty() && line.back() == '\r')
          line.pop_back();
        std::size_t colon = line.find(':');
        if (colon != std::string::npos) {
          std::string name = line.substr(0, colon);
          std::string value = line.substr(colon + 1);
          if (!value.empty() && value.front() == ' ')
            value.erase(value.begin());
          response->SetHeaderByName(name, value, true);
        }
        start = end + 1;
      }
    }
    response_length = static_cast<int64_t>(data_.size());
  }

  bool Read(void* data_out,
            int bytes_to_read,
            int& bytes_read,
            CefRefPtr<CefResourceReadCallback> callback) override {
    (void)callback;
    if (offset_ >= data_.size()) {
      bytes_read = 0;
      return false;
    }
    std::size_t remaining = data_.size() - offset_;
    std::size_t count = static_cast<std::size_t>(bytes_to_read) < remaining
                            ? static_cast<std::size_t>(bytes_to_read)
                            : remaining;
    std::memcpy(data_out, data_.data() + offset_, count);
    offset_ += count;
    bytes_read = static_cast<int>(count);
    return true;
  }

  void Cancel() override {}

  void TakeResponse() {
    if (!job_)
      return;
    // The pump writes the body, then unlocks this mutex, then continues the
    // request. Locking here makes those writes visible.
    std::lock_guard<std::mutex> lock(job_->mu);
    data_ = job_->response.data;
    mime_ = job_->response.mime.empty() ? "text/html" : job_->response.mime;
    status_ = job_->response.status > 0 ? job_->response.status : 200;
    status_text_ = job_->response.status_text.empty() ? "OK" : job_->response.status_text;
    headers_ = job_->response.headers;
  }

 private:
  std::string scheme_;
  std::shared_ptr<SchemeJob> job_;
  std::string data_;
  std::string mime_;
  std::string status_text_;
  std::string headers_;
  int status_ = 200;
  std::size_t offset_ = 0;
  IMPLEMENT_REFCOUNTING(SchemeHandler);
};

class SchemeFactory : public CefSchemeHandlerFactory {
 public:
  explicit SchemeFactory(std::string scheme) : scheme_(std::move(scheme)) {}

  CefRefPtr<CefResourceHandler> Create(CefRefPtr<CefBrowser> browser,
                                       CefRefPtr<CefFrame> frame,
                                       const CefString& scheme_name,
                                       CefRefPtr<CefRequest> request) override {
    (void)browser;
    (void)frame;
    (void)scheme_name;
    (void)request;
    return new SchemeHandler(scheme_);
  }

 private:
  std::string scheme_;
  IMPLEMENT_REFCOUNTING(SchemeFactory);
};

// CONTEXT null registers on the global request context. A panel view passes
// its own context: that context starts empty, and CefRegisterSchemeHandlerFactory
// only fills the global one.
void RegisterSchemeFactories(CefRefPtr<CefRequestContext> context) {
  for (const auto& name : g_scheme_names) {
    if (context)
      context->RegisterSchemeHandlerFactory(name, "", new SchemeFactory(name));
    else
      CefRegisterSchemeHandlerFactory(name, "", new SchemeFactory(name));
  }
}

class QueryHandler : public CefMessageRouterBrowserSide::Handler {
 public:
  explicit QueryHandler(int view_id) : view_id_(view_id) {}

  bool OnQuery(CefRefPtr<CefBrowser> browser,
               CefRefPtr<CefFrame> frame,
               int64_t query_id,
               const CefString& request,
               bool persistent,
               CefRefPtr<Callback> callback) override {
    (void)browser;
    (void)frame;
    (void)query_id;
    (void)persistent;
    Handle(request.ToString());
    if (callback)
      callback->Success("");
    return true;
  }

 private:
  static bool Flag(CefRefPtr<CefDictionaryValue> dict, const char* key) {
    return dict && dict->HasKey(key) && dict->GetBool(key);
  }

  static double Number(CefRefPtr<CefDictionaryValue> dict, const char* key) {
    if (!dict || !dict->HasKey(key))
      return 0;
    if (dict->GetType(key) == VTYPE_INT)
      return dict->GetInt(key);
    if (dict->GetType(key) == VTYPE_DOUBLE)
      return dict->GetDouble(key);
    return 0;
  }

  static std::string Text(CefRefPtr<CefDictionaryValue> dict, const char* key) {
    if (!dict || !dict->HasKey(key) || dict->GetType(key) != VTYPE_STRING)
      return {};
    return dict->GetString(key).ToString();
  }

  void Handle(const std::string& request) {
    CefRefPtr<CefValue> root = CefParseJSON(request, JSON_PARSER_RFC);
    if (!root || root->GetType() != VTYPE_DICTIONARY)
      return;
    CefRefPtr<CefDictionaryValue> dict = root->GetDictionary();
    std::string kind = dict->GetString("kind").ToString();
    std::string mods =
        std::string(",\"shift\":") + (Flag(dict, "shift") ? "true" : "false") +
        ",\"control\":" + (Flag(dict, "control") ? "true" : "false") +
        ",\"alt\":" + (Flag(dict, "alt") ? "true" : "false") +
        ",\"meta\":" + (Flag(dict, "meta") ? "true" : "false") + "}";
    if (kind == "button") {
      int button = dict->GetInt("button") + 1;
      // Click targets in the status bar. Empty when the page has no marker.
      std::string marked;
      std::string action = Text(dict, "action");
      std::string aux = Text(dict, "aux");
      if (!action.empty())
        marked += ",\"action\":" + JsonString(action);
      if (!aux.empty())
        marked += ",\"aux\":" + JsonString(aux);
      Emit(view_id_,
           "{\"event\":\"button-press\",\"button\":" + std::to_string(button) + marked + mods);
    } else if (kind == "wheel") {
      double dx = Number(dict, "dx");
      double dy = Number(dict, "dy");
      int button = 0;
      if (dy > 0)
        button = 5;
      else if (dy < 0)
        button = 4;
      else if (dx > 0)
        button = 7;
      else if (dx < 0)
        button = 6;
      if (button)
        Emit(view_id_,
             "{\"event\":\"scroll\",\"button\":" + std::to_string(button) + mods);
    } else if (kind == "hover") {
      Emit(view_id_,
           "{\"event\":\"hover\",\"url\":" + JsonString(dict->GetString("url").ToString()) + "}");
    }
  }

  int view_id_;
};

std::string HostOf(const std::string& url) {
  auto scheme = url.find("://");
  std::size_t start = scheme == std::string::npos ? 0 : scheme + 3;
  auto end = url.find_first_of("/?#", start);
  auto at = url.find('@', start);
  if (at != std::string::npos && (end == std::string::npos || at < end))
    start = at + 1;
  if (end == std::string::npos)
    end = url.size();
  auto colon = url.find(':', start);
  if (colon != std::string::npos && colon < end)
    end = colon;
  std::string host = url.substr(start, end - start);
  if (!host.empty() && host.front() == '[') {
    auto close = host.find(']');
    if (close != std::string::npos)
      host = host.substr(1, close - 1);
  }
  for (char& character : host)
    character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
  return host;
}

std::string UrlKey(const std::string& url) {
  auto hash = url.find('#');
  std::string key = hash == std::string::npos ? url : url.substr(0, hash);
  for (char& character : key)
    character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
  return key;
}

const char* ResourceTypeName(cef_resource_type_t type) {
  switch (type) {
    case RT_MAIN_FRAME:
      return "document";
    case RT_SUB_FRAME:
      return "subdocument";
    case RT_STYLESHEET:
      return "stylesheet";
    case RT_SCRIPT:
      return "script";
    case RT_IMAGE:
    case RT_FAVICON:
      return "image";
    case RT_FONT_RESOURCE:
      return "font";
    case RT_OBJECT:
    case RT_PLUGIN_RESOURCE:
      return "object";
    case RT_MEDIA:
      return "media";
    case RT_XHR:
      return "xmlhttprequest";
    case RT_PING:
      return "ping";
    default:
      return "other";
  }
}

std::string DocumentUrl(CefRefPtr<CefRequest> request, CefRefPtr<CefFrame> frame) {
  if (!request)
    return {};
  if (request->GetResourceType() == RT_MAIN_FRAME)
    return request->GetURL().ToString();
  if (request->GetResourceType() == RT_SUB_FRAME && frame) {
    auto parent = frame->GetParent();
    if (parent) {
      std::string url = parent->GetURL().ToString();
      if (!url.empty())
        return url;
    }
  }
  if (frame) {
    std::string url = frame->GetURL().ToString();
    if (!url.empty() && url != "about:blank")
      return url;
  }
  return request->GetURL().ToString();
}

std::shared_ptr<const std::string> InternCosmetic(const std::string& css) {
  std::size_t hash = std::hash<std::string>{}(css);
  auto& bucket = g_cosmetic_intern[hash];
  for (const auto& entry : bucket) {
    if (*entry == css)
      return entry;
  }
  auto shared = std::make_shared<const std::string>(css);
  bucket.push_back(shared);
  return shared;
}

void RememberCosmetic(const std::string& url, const std::string& css) {
  std::lock_guard<std::mutex> lock(g_cosmetic_mu);
  auto shared = InternCosmetic(css);
  g_cosmetic_by_url[UrlKey(url)] = shared;
  std::string host = HostOf(url);
  if (!host.empty())
    g_cosmetic_by_host[host] = shared;
}

std::string CosmeticFor(const std::string& url) {
  std::lock_guard<std::mutex> lock(g_cosmetic_mu);
  auto exact = g_cosmetic_by_url.find(UrlKey(url));
  if (exact != g_cosmetic_by_url.end() && exact->second)
    return *exact->second;
  auto fallback = g_cosmetic_by_host.find(HostOf(url));
  if (fallback != g_cosmetic_by_host.end() && fallback->second)
    return *fallback->second;
  return {};
}

void ResetCosmetic() {
  std::lock_guard<std::mutex> lock(g_cosmetic_mu);
  g_cosmetic_by_url.clear();
  g_cosmetic_by_host.clear();
  g_cosmetic_intern.clear();
}

// CEF's own async resource test resumes CefCallback on the UI thread. Calling
// Continue from the Lisp thread, or from a raw std::thread, never starts the
// load. ClosureTask also masks the FP traps SBCL leaves on.
void ResumeCallback(const CefRefPtr<CefCallback>& async, bool block) {
  PostCefCallback(async, block);
}

// Encoded substitutions, keyed by CefRequest::GetIdentifier. Parsed when the
// response headers arrive. Defined up here because FinishRequest runs first.
std::mutex g_replace_mu;
std::unordered_map<uint64_t, std::string> g_replace_text;

void RememberReplacements(uint64_t id, const std::string& text) {
  if (id == 0 || text.empty())
    return;
  std::lock_guard<std::mutex> lock(g_replace_mu);
  if (g_replace_text.size() > 2048)
    g_replace_text.clear();
  g_replace_text[id] = text;
}

std::string ReplacementsText(uint64_t id) {
  std::lock_guard<std::mutex> lock(g_replace_mu);
  auto found = g_replace_text.find(id);
  if (found == g_replace_text.end())
    return {};
  return found->second;
}

void ForgetReplacements(uint64_t id) {
  std::lock_guard<std::mutex> lock(g_replace_mu);
  g_replace_text.erase(id);
}

void FinishRequest(const std::shared_ptr<RequestJob>& job) {
  if (!job || job->settled.exchange(true))
    return;
  bool block = job->decision.block != 0;
  // The response filter runs after Continue, so the substitutions have to be
  // recorded first. A blocked request has no body to rewrite.
  if (!block)
    RememberReplacements(job->id, job->decision.replacements);
  // Resume the load before any further work. A throw while storing CSS must
  // not leave the request parked in OnBeforeResourceLoad.
  ResumeCallback(job->async, block);
  if (!block && (job->type == "document" || job->type == "subdocument"))
    RememberCosmetic(job->url, job->decision.css);
}

void ArmRequestTimeout(const std::shared_ptr<RequestJob>& job) {
  if (!job || !job->async)
    return;
  std::thread([job]() {
    std::this_thread::sleep_for(std::chrono::seconds(2));
    if (job->settled.exchange(true))
      return;
    ResumeCallback(job->async, false);
  }).detach();
}

bool HttpUrl(const std::string& url) {
  return url.compare(0, 7, "http://") == 0 ||
         (url.size() >= 8 && url.compare(0, 8, "https://") == 0);
}

// Literal substitutions for one response. YouTube's pre-roll keys are short,
// so a request holds only a handful of these.
struct BodyReplacement {
  std::string from;
  std::string to;
};

std::vector<BodyReplacement> ParseReplacements(const std::string& text) {
  std::vector<BodyReplacement> out;
  if (text.empty())
    return out;
  size_t index = 0;
  auto read_line = [&](std::string* line) -> bool {
    if (index >= text.size())
      return false;
    size_t end = text.find('\n', index);
    if (end == std::string::npos)
      return false;
    *line = text.substr(index, end - index);
    index = end + 1;
    return true;
  };
  auto read_bytes = [&](size_t size, std::string* bytes) -> bool {
    if (size > text.size() - index)
      return false;
    *bytes = text.substr(index, size);
    index += size;
    return true;
  };
  try {
    std::string count_line;
    if (!read_line(&count_line))
      return {};
    int count = std::stoi(count_line);
    if (count < 0 || count > 32)
      return {};
    for (int n = 0; n < count; ++n) {
      std::string from_line;
      std::string to_line;
      std::string from;
      std::string to;
      if (!read_line(&from_line))
        return {};
      size_t from_size = static_cast<size_t>(std::stoul(from_line));
      if (!read_bytes(from_size, &from) || !read_line(&to_line))
        return {};
      size_t to_size = static_cast<size_t>(std::stoul(to_line));
      if (!read_bytes(to_size, &to))
        return {};
      if (!from.empty() && from.size() <= 256 && to.size() <= 256)
        out.push_back(BodyReplacement{std::move(from), std::move(to)});
    }
  } catch (...) {
    return {};
  }
  return out;
}

bool BinaryMime(const std::string& mime) {
  return mime.rfind("image/", 0) == 0 || mime.rfind("video/", 0) == 0 ||
         mime.rfind("audio/", 0) == 0 || mime.rfind("font/", 0) == 0 ||
         mime == "application/octet-stream" || mime == "application/wasm";
}

// Streaming literal replace. A needle that straddles two chunks stays in
// `pending` until the next chunk, or until `flush` at the end of the body.
// Needles are applied leftmost, longest first, and the replacement is not
// searched again.
void StreamReplace(const std::string& chunk,
                   bool flush,
                   const std::vector<BodyReplacement>& replacements,
                   std::string* pending,
                   std::string* produced) {
  std::string work = *pending;
  work.append(chunk);
  size_t max_len = 1;
  for (const auto& replacement : replacements)
    max_len = std::max(max_len, replacement.from.size());
  size_t limit = work.size();
  if (!flush)
    limit = work.size() > max_len - 1 ? work.size() - (max_len - 1) : 0;
  size_t cursor = 0;
  size_t index = 0;
  std::string out;
  while (index < limit) {
    const BodyReplacement* hit = nullptr;
    for (const auto& replacement : replacements) {
      if (replacement.from.empty() || index + replacement.from.size() > work.size())
        continue;
      if (work.compare(index, replacement.from.size(), replacement.from) == 0 &&
          (!hit || replacement.from.size() > hit->from.size()))
        hit = &replacement;
    }
    if (hit) {
      out.append(work, cursor, index - cursor);
      out.append(hit->to);
      index += hit->from.size();
      cursor = index;
    } else {
      ++index;
    }
  }
  size_t emitted = cursor > limit ? cursor : limit;
  if (cursor < emitted)
    out.append(work, cursor, emitted - cursor);
  *pending = work.substr(emitted);
  produced->append(out);
}

// Rewrites a response the way Firefox's filterResponseData does for `$replace`.
// The player then parses JSON that has no ad-placement keys.
class BodyReplaceFilter : public CefResponseFilter {
 public:
  explicit BodyReplaceFilter(std::vector<BodyReplacement> replacements)
      : replacements_(std::move(replacements)) {}

  bool InitFilter() override { return !replacements_.empty(); }

  FilterStatus Filter(void* data_in,
                      size_t data_in_size,
                      size_t& data_in_read,
                      void* data_out,
                      size_t data_out_size,
                      size_t& data_out_written) override {
    if (!data_out || data_out_size == 0)
      return RESPONSE_FILTER_ERROR;
    // A full output buffer makes the wrapper call again with an empty input.
    // That call is not the end of the body. The end is an empty call that was
    // not asked for that way, which is when the pending tail has to be emitted.
    bool continuation = continuation_;
    continuation_ = false;
    bool end = data_in_size == 0 && !continuation;
    data_in_read = data_in_size;
    if (data_in_size > 0 && data_in) {
      StreamReplace(std::string(static_cast<char*>(data_in), data_in_size), false,
                    replacements_, &pending_, &overflow_);
    } else if (end) {
      StreamReplace("", true, replacements_, &pending_, &overflow_);
    }
    size_t written = std::min(overflow_.size(), data_out_size);
    if (written > 0) {
      std::memcpy(data_out, overflow_.data(), written);
      overflow_.erase(0, written);
    }
    data_out_written = written;
    bool full = written == data_out_size;
    if (!overflow_.empty() && full) {
      continuation_ = true;
      return RESPONSE_FILTER_NEED_MORE_DATA;
    }
    if (!end && !pending_.empty())
      return RESPONSE_FILTER_NEED_MORE_DATA;
    if (!overflow_.empty())
      return RESPONSE_FILTER_NEED_MORE_DATA;
    return RESPONSE_FILTER_DONE;
  }

 private:
  std::vector<BodyReplacement> replacements_;
  std::string pending_;
  std::string overflow_;
  bool continuation_ = false;

  IMPLEMENT_REFCOUNTING(BodyReplaceFilter);
};

// Returns true when the request should be cancelled. CSS is filled for every
// call that reaches Lisp, including an empty sheet, so a later navigation can
// replace an older one for the same host.
bool ConsultBlocker(const std::string& url,
                    const std::string& document_url,
                    const char* type,
                    std::string* css) {
  if (!g_request_cb.load() || !type || !HttpUrl(url))
    return false;
  auto job = std::make_shared<RequestJob>();
  job->url = url;
  job->document_url = document_url;
  job->type = type;
  {
    std::lock_guard<std::mutex> lock(g_request_mu);
    if (g_request_stop.load())
      return false;
    g_request_queue.push(job);
  }
  g_request_cv.notify_one();
  if (job->done.get_future().wait_for(std::chrono::seconds(2)) != std::future_status::ready)
    return false;
  if (css)
    *css = job->decision.css;
  return job->decision.block != 0;
}

void TrackBrowser(const CefRefPtr<CefBrowser>& browser) {
  if (!browser)
    return;
  std::lock_guard<std::mutex> lock(g_browser_mu);
  for (const auto& existing : g_browsers) {
    if (existing && existing->IsSame(browser))
      return;
  }
  g_browsers.push_back(browser);
}

void UntrackBrowser(const CefRefPtr<CefBrowser>& browser) {
  std::lock_guard<std::mutex> lock(g_browser_mu);
  g_browsers.erase(std::remove_if(g_browsers.begin(), g_browsers.end(),
                                  [&](const CefRefPtr<CefBrowser>& existing) {
                                    return !existing || (browser && existing->IsSame(browser));
                                  }),
                   g_browsers.end());
}

bool BrowsersOpen() {
  std::lock_guard<std::mutex> lock(g_browser_mu);
  return !g_browsers.empty();
}

// Views that have not reported a browser yet must keep the loop alive, or the
// browser is created after CefShutdown and its process is orphaned.
bool ViewsAwaitingBrowser() {
  std::lock_guard<std::mutex> lock(g_mu);
  for (const auto& entry : g_views) {
    const auto& view = entry.second;
    if (view && !view->close_started)
      return true;
  }
  return false;
}

void MaybeFinishShutdown() {
  if (!g_quitting.load() || g_closing_browsers || g_loop_done.load())
    return;
  if (BrowsersOpen() || ViewsAwaitingBrowser())
    return;
  // DropView is posted from OnBrowserDestroyed. Quitting on the next turn lets
  // that release finish before CefShutdown.
  CefPostTask(TID_UI, new ClosureTask([] {
                if (g_loop_done.load() || !g_quitting.load())
                  return;
                if (BrowsersOpen() || ViewsAwaitingBrowser())
                  return;
                CefQuitMessageLoop();
              }));
}

// Icon bytes and WM_CLASS supplied by the host before the first window. A
// Chrome-style window otherwise publishes Chromium's product logo and class.
struct AppIconPng {
  float scale = 1.f;
  std::vector<unsigned char> bytes;
};

std::mutex g_icon_mu;
std::vector<AppIconPng> g_icon_pngs;
int g_icon_generation = 0;
std::string g_window_class;

void RememberAppIcon(float scale, const void* png, size_t size) {
  if (!png || size == 0 || !(scale > 0.f))
    return;
  AppIconPng icon;
  icon.scale = scale;
  const auto* data = static_cast<const unsigned char*>(png);
  icon.bytes.assign(data, data + size);
  std::lock_guard<std::mutex> lock(g_icon_mu);
  g_icon_pngs.push_back(std::move(icon));
  ++g_icon_generation;
}

void RememberWindowClass(const char* name) {
  std::lock_guard<std::mutex> lock(g_icon_mu);
  g_window_class = name ? name : "";
}

#if defined(__linux__)
// Chrome-style windows register with the desktop as Chromium. When the host
// set a class, apply it so the desktop entry matches. The native window also
// has to be unmapped as soon as quit starts. CefWindow::Close finishes later,
// on the message loop, which a REPL does not exit.
void ForEachAncestor(CefWindowHandle handle, void (*visit)(Display*, Window)) {
  if (!handle || !visit)
    return;
  Display* display = XOpenDisplay(nullptr);
  if (!display)
    return;
  Window window = handle;
  for (int hop = 0; hop < 8; ++hop) {
    visit(display, window);
    Window root = 0;
    Window parent = 0;
    Window* children = nullptr;
    unsigned count = 0;
    if (!XQueryTree(display, window, &root, &parent, &children, &count))
      break;
    if (children)
      XFree(children);
    if (!parent || parent == root)
      break;
    window = parent;
  }
  XSync(display, False);
  XCloseDisplay(display);
}

struct BuiltIcon {
  CefRefPtr<CefImage> image;
  std::vector<unsigned long> property;
  int generation = -1;
};

bool& AppIconReleased() {
  static bool released = false;
  return released;
}

BuiltIcon& BuiltAppIcon() {
  static BuiltIcon built;
  // Drop the image before CefShutdown. Recreating it afterwards uses a
  // library that has already been torn down.
  if (AppIconReleased()) {
    built.image = nullptr;
    built.property.clear();
    return built;
  }
  int generation = 0;
  std::vector<AppIconPng> pngs;
  {
    std::lock_guard<std::mutex> lock(g_icon_mu);
    generation = g_icon_generation;
    pngs = g_icon_pngs;
  }
  if (built.generation == generation && (built.image || pngs.empty()))
    return built;
  if (pngs.empty()) {
    built.image = nullptr;
    built.property.clear();
    built.generation = generation;
    return built;
  }
  // Decode each file alone. One CefImage keeps a single representation, and
  // the desktop then scales that image to the taskbar size.
  struct DecodedIcon {
    int width = 0;
    int height = 0;
    float scale = 1.f;
    std::vector<unsigned char> png;
    std::vector<unsigned char> pixels;
  };
  std::vector<DecodedIcon> decoded;
  for (const auto& png : pngs) {
    if (png.bytes.size() < 8)
      continue;
    CefRefPtr<CefImage> one = CefImage::CreateImage();
    if (!one->AddPNG(1.f, png.bytes.data(), png.bytes.size()))
      continue;
    int width = 0;
    int height = 0;
    CefRefPtr<CefBinaryValue> bitmap = one->GetAsBitmap(
        1.f, CEF_COLOR_TYPE_RGBA_8888, CEF_ALPHA_TYPE_POSTMULTIPLIED, width, height);
    if (!bitmap || width <= 0 || height <= 0)
      continue;
    size_t nbytes = bitmap->GetSize();
    size_t count = static_cast<size_t>(width) * static_cast<size_t>(height);
    if (nbytes < count * 4)
      continue;
    DecodedIcon icon;
    icon.width = width;
    icon.height = height;
    icon.png = png.bytes;
    icon.pixels.resize(nbytes);
    bitmap->GetData(icon.pixels.data(), nbytes, 0);
    decoded.push_back(std::move(icon));
  }
  std::sort(decoded.begin(), decoded.end(), [](const DecodedIcon& a, const DecodedIcon& b) {
    return a.width > b.width;
  });
  decoded.erase(std::unique(decoded.begin(), decoded.end(),
                            [](const DecodedIcon& a, const DecodedIcon& b) {
                              return a.width == b.width && a.height == b.height;
                            }),
                decoded.end());
  int base = decoded.empty() ? 16 : decoded.back().width;
  if (base <= 0)
    base = 16;
  CefRefPtr<CefImage> image = CefImage::CreateImage();
  for (auto it = decoded.rbegin(); it != decoded.rend(); ++it) {
    it->scale = static_cast<float>(it->width) / static_cast<float>(base);
    image->AddPNG(it->scale, it->png.data(), it->png.size());
  }
  std::vector<unsigned long> property;
  // Largest first. Some window managers use the first icon in the property.
  for (const DecodedIcon& icon : decoded) {
    size_t count = static_cast<size_t>(icon.width) * static_cast<size_t>(icon.height);
    property.push_back(static_cast<unsigned long>(icon.width));
    property.push_back(static_cast<unsigned long>(icon.height));
    for (size_t i = 0; i < count; ++i) {
      size_t offset = i * 4;
      unsigned long argb = (static_cast<unsigned long>(icon.pixels[offset + 3]) << 24) |
                           (static_cast<unsigned long>(icon.pixels[offset]) << 16) |
                           (static_cast<unsigned long>(icon.pixels[offset + 1]) << 8) |
                           static_cast<unsigned long>(icon.pixels[offset + 2]);
      property.push_back(argb);
    }
  }
  built.image = image->IsEmpty() ? nullptr : image;
  built.property = std::move(property);
  built.generation = generation;
  return built;
}

void SetHostClass(Display* display, Window window) {
  std::string name;
  {
    std::lock_guard<std::mutex> lock(g_icon_mu);
    name = g_window_class;
  }
  if (name.empty())
    return;
  // Both fields are the desktop id. StartupWMClass is matched as given.
  XClassHint hint;
  hint.res_name = const_cast<char*>(name.c_str());
  hint.res_class = const_cast<char*>(name.c_str());
  XSetClassHint(display, window, &hint);
}

void SetHostIcon(Display* display, Window window) {
  const std::vector<unsigned long>& property = BuiltAppIcon().property;
  if (property.empty())
    return;
  Atom atom = XInternAtom(display, "_NET_WM_ICON", False);
  XChangeProperty(display, window, atom, XA_CARDINAL, 32, PropModeReplace,
                  reinterpret_cast<unsigned char*>(const_cast<unsigned long*>(property.data())),
                  static_cast<int>(property.size()));
}

void SetHostIdentity(Display* display, Window window) {
  SetHostClass(display, window);
  SetHostIcon(display, window);
}

void UnmapNative(Display* display, Window window) { XUnmapWindow(display, window); }

void NameNativeWindow(CefRefPtr<CefWindow> window) {
  if (window)
    ForEachAncestor(window->GetWindowHandle(), SetHostIdentity);
}

void UnmapNativeWindow(CefRefPtr<CefWindow> window) {
  if (window)
    ForEachAncestor(window->GetWindowHandle(), UnmapNative);
}

void ApplyHostIdentity(CefRefPtr<CefWindow> window) {
  if (!window)
    return;
  BuiltIcon& icon = BuiltAppIcon();
  if (icon.image) {
    window->SetWindowIcon(icon.image);
    window->SetWindowAppIcon(icon.image);
  }
  NameNativeWindow(window);
}

void ReleaseAppIcon() {
  AppIconReleased() = true;
  BuiltIcon& icon = BuiltAppIcon();
  icon.image = nullptr;
  icon.property.clear();
}
#else
void NameNativeWindow(CefRefPtr<CefWindow> window) { (void)window; }
void UnmapNativeWindow(CefRefPtr<CefWindow> window) { (void)window; }
void ApplyHostIdentity(CefRefPtr<CefWindow> window) { (void)window; }
void ReleaseAppIcon() {}
#endif

void CloseTrackedBrowsers() {
  g_closing_browsers = true;
  std::vector<CefRefPtr<CefBrowser>> browsers;
  {
    std::lock_guard<std::mutex> lock(g_browser_mu);
    browsers = g_browsers;
  }
  std::vector<std::shared_ptr<ViewRecord>> views;
  {
    std::lock_guard<std::mutex> lock(g_mu);
    for (const auto& entry : g_views)
      views.push_back(entry.second);
  }
  for (const auto& view : views) {
    if (!view)
      continue;
    view->destroying = true;
    view->pending.clear();
    CefRefPtr<CefBrowser> browser = view->browser;
    if (!browser && view->widget)
      browser = view->widget->GetBrowser();
    if (!browser)
      continue;
    view->browser = browser;
    view->close_started = true;
    browsers.push_back(browser);
  }
  {
    std::lock_guard<std::mutex> lock(g_mu);
    for (const auto& entry : g_windows)
      if (entry.second)
        UnmapNativeWindow(entry.second->window);
  }
  for (const auto& browser : browsers) {
    if (!browser || !browser->GetHost())
      continue;
    browser->GetHost()->SetAudioMuted(true);
    browser->GetHost()->CloseBrowser(true);
  }
  g_closing_browsers = false;
  {
    std::lock_guard<std::mutex> lock(g_mu);
    for (const auto& entry : g_views) {
      if (entry.second)
        entry.second->close_started = true;
    }
  }
  // Release the CEF objects on the next turn, while the loop can still run
  // their destructors. Releasing them after the loop returns deadlocks, and
  // leaving them for CefShutdown destroys an observer list that is not empty.
  CefPostTask(TID_UI, new ClosureTask([] {
                if (g_loop_done.load())
                  return;
                g_menu = nullptr;
                ReleaseAppIcon();
                {
                  std::lock_guard<std::mutex> lock(g_browser_mu);
                  g_browsers.clear();
                }
                {
                  std::lock_guard<std::mutex> lock(g_mu);
                  for (auto& entry : g_views) {
                    if (!entry.second)
                      continue;
                    entry.second->pending.clear();
                    entry.second->browser = nullptr;
                    entry.second->widget = nullptr;
                    entry.second->context = nullptr;
                    entry.second->devtools_observer = nullptr;
                    entry.second->devtools_registration = nullptr;
                  }
                  g_views.clear();
                  for (auto& entry : g_windows) {
                    if (!entry.second)
                      continue;
                    entry.second->window = nullptr;
                    entry.second->delegate = nullptr;
                    entry.second->layout = nullptr;
                  }
                  g_windows.clear();
                }
                CefQuitMessageLoop();
              }));
}

void BeginShutdown() {
  if (!g_initialized.load())
    return;
  bool already = false;
  if (!g_quitting.compare_exchange_strong(already, true))
    return;
  std::fprintf(stderr, "cl-cef prepare shutdown\n");
  std::fflush(stderr);
  // CloseBrowser can stall. Quit anyway so the process is not stuck.
  std::thread([] {
    std::this_thread::sleep_for(std::chrono::seconds(2));
    if (!g_quitting.load() || g_loop_done.load() || !g_initialized.load())
      return;
    PostUI([] {
      if (!g_loop_done.load())
        CefQuitMessageLoop();
    });
  }).detach();
  PostUI([] { CloseTrackedBrowsers(); });
}

class Client : public CefClient,
               public CefLifeSpanHandler,
               public CefLoadHandler,
               public CefDisplayHandler,
               public CefKeyboardHandler,
               public CefCommandHandler,
               public CefRequestHandler,
               public CefResourceRequestHandler,
               public CefDownloadHandler,
               public CefContextMenuHandler,
               public CefFocusHandler {
 public:
  explicit Client(int view_id)
      : view_id_(view_id),
        router_(CefMessageRouterBrowserSide::Create(CefMessageRouterConfig())),
        queries_(new QueryHandler(view_id)) {
    if (router_)
      router_->AddHandler(queries_.get(), false);
  }

  CefRefPtr<CefLifeSpanHandler> GetLifeSpanHandler() override { return this; }
  CefRefPtr<CefLoadHandler> GetLoadHandler() override { return this; }
  CefRefPtr<CefDisplayHandler> GetDisplayHandler() override { return this; }
  CefRefPtr<CefKeyboardHandler> GetKeyboardHandler() override { return this; }
  CefRefPtr<CefCommandHandler> GetCommandHandler() override { return this; }
  CefRefPtr<CefRequestHandler> GetRequestHandler() override { return this; }
  CefRefPtr<CefDownloadHandler> GetDownloadHandler() override { return this; }
  CefRefPtr<CefContextMenuHandler> GetContextMenuHandler() override { return this; }
  CefRefPtr<CefFocusHandler> GetFocusHandler() override { return this; }

  bool OnProcessMessageReceived(CefRefPtr<CefBrowser> browser,
                                CefRefPtr<CefFrame> frame,
                                CefProcessId source_process,
                                CefRefPtr<CefProcessMessage> message) override {
    return router_ && router_->OnProcessMessageReceived(browser, frame, source_process, message);
  }

  void OnAfterCreated(CefRefPtr<CefBrowser> browser) override {
    if (!browser)
      return;
    TrackBrowser(browser);
    auto view = FindView(view_id_);
    // The view was dropped while creation was in flight. Close the browser
    // without the window: DoClose treats a missing view as already detached.
    // A shutdown that starts before this callback has to close the browser too,
    // or the render process outlives Nyxt.
    if (!view || g_quitting.load()) {
      if (view) {
        view->browser = browser;
        view->destroying = true;
        view->close_started = true;
        g_browser_to_view[browser->GetIdentifier()] = view_id_;
      }
      browser->GetHost()->SetAudioMuted(true);
      browser->GetHost()->CloseBrowser(true);
      return;
    }
    view->browser = browser;
    g_browser_to_view[browser->GetIdentifier()] = view_id_;
    // Level 0 is 100%. SetZoomLevel(0) asks for the context default, which is
    // 100% in a context that has no stored about:blank zoom.
    if (view->isolated)
      browser->GetHost()->SetZoomLevel(0.0);
    // Delete can win the race against creation. Close now that a browser
    // exists; DropView runs from OnBrowserDestroyed.
    if (view->destroying) {
      view->pending.clear();
      if (!view->close_started) {
        view->close_started = true;
        browser->GetHost()->SetAudioMuted(true);
        browser->GetHost()->CloseBrowser(true);
      }
      return;
    }
    auto pending = std::move(view->pending);
    view->pending.clear();
    // These run while the view is being attached. Calling them inline re-enters
    // AddChildView (a queued LoadURL is the usual case) and the message loop
    // never returns.
    for (const auto& fn : pending)
      CefPostTask(TID_UI, new ClosureTask([fn, browser]() { fn(browser); }));
  }

  bool OnBeforePopup(CefRefPtr<CefBrowser> browser,
                     CefRefPtr<CefFrame> frame,
                     int popup_id,
                     const CefString& target_url,
                     const CefString& target_frame_name,
                     CefLifeSpanHandler::WindowOpenDisposition target_disposition,
                     bool user_gesture,
                     const CefPopupFeatures& popup_features,
                     CefWindowInfo& window_info,
                     CefRefPtr<CefClient>& client,
                     CefBrowserSettings& settings,
                     CefRefPtr<CefDictionaryValue>& extra_info,
                     bool* no_javascript_access) override {
    (void)browser;
    (void)popup_id;
    (void)frame;
    // Deciding here would wait on the UI thread. Nyxt cancels the popup below
    // and opens the URL itself.
    (void)target_frame_name;
    (void)target_disposition;
    (void)user_gesture;
    (void)popup_features;
    (void)window_info;
    (void)client;
    (void)settings;
    (void)extra_info;
    (void)no_javascript_access;
    int consumed = Emit(view_id_,
                        "{\"event\":\"window-open\",\"url\":" +
                            JsonString(target_url.ToString()) + "}");
    return consumed != 0;
  }

  // Returning false asks CEF to close the top-level window. That removes the
  // page from the UI and leaves the browser, and its audio, running.
  // During process shutdown the window is going away too, and returning true
  // cancels the browser close, so the Chromium processes stay up.
  bool DoClose(CefRefPtr<CefBrowser> browser) override {
    (void)browser;
    if (g_quitting.load())
      return false;
    auto view = FindView(view_id_);
    // A missing view was already removed from the window. Returning false
    // would close that window and every other view it still holds.
    return !view || view->destroying;
  }

  void OnBeforeClose(CefRefPtr<CefBrowser> browser) override {
    if (router_) {
      router_->RemoveHandler(queries_.get());
      router_->OnBeforeClose(browser);
    }
    auto view = FindView(view_id_);
    if (view) {
      view->browser = nullptr;
      view->close_started = true;
    }
    if (browser) {
      g_browser_to_view.erase(browser->GetIdentifier());
      UntrackBrowser(browser);
    }
    Emit(view_id_, "{\"event\":\"closed\"}");
    MaybeFinishShutdown();
  }

  void OnLoadingStateChange(CefRefPtr<CefBrowser> browser,
                            bool is_loading,
                            bool can_go_back,
                            bool can_go_forward) override {
    (void)browser;
    (void)can_go_back;
    (void)can_go_forward;
    (void)is_loading;
  }

  void OnLoadStart(CefRefPtr<CefBrowser> browser,
                   CefRefPtr<CefFrame> frame,
                   TransitionType transition_type) override {
    (void)browser;
    (void)transition_type;
    if (!frame)
      return;
    auto view = FindView(view_id_);
    if (!view || view->destroying)
      return;
    ApplyScripts(view, frame, true);
    std::string cosmetic = CosmeticFor(frame->GetURL().ToString());
    if (!cosmetic.empty())
      ApplyCss(frame, "cl-cef-adblock", cosmetic);
    if (!frame->IsMain())
      return;
    Emit(view_id_,
         "{\"event\":\"did-start-loading\",\"url\":" + JsonString(frame->GetURL().ToString()) + "}");
  }

  void OnLoadEnd(CefRefPtr<CefBrowser> browser,
                 CefRefPtr<CefFrame> frame,
                 int http_status_code) override {
    (void)browser;
    (void)http_status_code;
    if (!frame)
      return;
    auto view = FindView(view_id_);
    if (!view || view->destroying)
      return;
    ApplyScripts(view, frame, false);
    if (!frame->IsMain())
      return;
    Emit(view_id_,
         "{\"event\":\"did-finish-load\",\"url\":" + JsonString(frame->GetURL().ToString()) + "}");
  }

  void OnLoadError(CefRefPtr<CefBrowser> browser,
                   CefRefPtr<CefFrame> frame,
                   ErrorCode error_code,
                   const CefString& error_text,
                   const CefString& failed_url) override {
    (void)browser;
    if (!frame || !frame->IsMain())
      return;
    auto view = FindView(view_id_);
    if (!view || view->destroying)
      return;
    Emit(view_id_,
         "{\"event\":\"did-fail-load\",\"url\":" + JsonString(failed_url.ToString()) +
             ",\"errorCode\":" + std::to_string(static_cast<int>(error_code)) +
             ",\"errorText\":" + JsonString(error_text.ToString()) + "}");
  }

  void OnAddressChange(CefRefPtr<CefBrowser> browser,
                       CefRefPtr<CefFrame> frame,
                       const CefString& url) override {
    (void)browser;
    if (!frame || !frame->IsMain())
      return;
    auto view = FindView(view_id_);
    if (!view || view->destroying)
      return;
    Emit(view_id_, "{\"event\":\"address-change\",\"url\":" + JsonString(url.ToString()) + "}");
  }

  void OnTitleChange(CefRefPtr<CefBrowser> browser, const CefString& title) override {
    (void)browser;
    auto view = FindView(view_id_);
    if (!view || view->destroying)
      return;
    view->title = title.ToString();
    Emit(view_id_, "{\"event\":\"page-title-updated\",\"title\":" + JsonString(title.ToString()) + "}");
  }

  void OnFullscreenModeChange(CefRefPtr<CefBrowser> browser, bool fullscreen) override {
    (void)browser;
    Emit(view_id_,
         std::string("{\"event\":\"fullscreen\",\"fullscreen\":") +
             (fullscreen ? "true}" : "false}"));
  }

  // Ctrl+K and Ctrl+L focus a toolbar this Chrome window does not have.
  // The default command destroys a panel whose delegate is already gone, and
  // CefViewView's destructor then CHECKs on the UI thread.
  bool OnChromeCommand(CefRefPtr<CefBrowser> browser,
                       int command_id,
                       cef_window_open_disposition_t disposition) override {
    (void)browser;
    (void)disposition;
    switch (command_id) {
      case IDC_FOCUS_TOOLBAR:
      case IDC_FOCUS_LOCATION:
      case IDC_FOCUS_SEARCH:
      case IDC_FOCUS_MENU_BAR:
      case IDC_FOCUS_BOOKMARKS:
      case IDC_FOCUS_NEXT_PANE:
      case IDC_FOCUS_PREVIOUS_PANE:
      case IDC_FOCUS_WEB_CONTENTS_PANE:
      case IDC_FOCUS_INACTIVE_POPUP_FOR_ACCESSIBILITY:
      case IDC_FOCUS_THIS_TAB:
      case IDC_FOCUS_NEXT_TAB_GROUP:
      case IDC_FOCUS_PREV_TAB_GROUP:
        return true;
      default:
        return false;
    }
  }

  bool OnPreKeyEvent(CefRefPtr<CefBrowser> browser,
                     const CefKeyEvent& event,
                     CefEventHandle os_event,
                     bool* is_keyboard_shortcut) override {
    (void)browser;
    (void)os_event;
    (void)is_keyboard_shortcut;
    if (event.type != KEYEVENT_RAWKEYDOWN && event.type != KEYEVENT_KEYUP)
      return false;
    const char* type = event.type == KEYEVENT_KEYUP ? "keyUp" : "keyDown";
    std::string character;
    if (event.character >= 32 && event.character < 127)
      character.assign(1, static_cast<char>(event.character));
    int consumed = Emit(
        view_id_,
        std::string("{\"event\":\"before-input-event\",\"type\":") + JsonString(type) +
            ",\"windowsKeyCode\":" + std::to_string(event.windows_key_code) +
            ",\"modifiers\":" + std::to_string(event.modifiers) +
            ",\"character\":" + JsonString(character) +
            ",\"shift\":" + ((event.modifiers & EVENTFLAG_SHIFT_DOWN) ? "true" : "false") +
            ",\"control\":" + ((event.modifiers & EVENTFLAG_CONTROL_DOWN) ? "true" : "false") +
            ",\"alt\":" + ((event.modifiers & EVENTFLAG_ALT_DOWN) ? "true" : "false") +
            ",\"meta\":" + ((event.modifiers & EVENTFLAG_COMMAND_DOWN) ? "true" : "false") + "}");
    return consumed != 0;
  }

  // Called after the page has seen the key. Chrome then runs browser
  // accelerators. Ctrl+K focuses a toolbar this window does not have, and
  // that destroys a panel whose delegate is already gone.
  bool OnKeyEvent(CefRefPtr<CefBrowser> browser,
                  const CefKeyEvent& event,
                  CefEventHandle os_event) override {
    (void)browser;
    (void)os_event;
    if (event.type != KEYEVENT_RAWKEYDOWN)
      return false;
    return (event.modifiers & (EVENTFLAG_CONTROL_DOWN | EVENTFLAG_ALT_DOWN |
                               EVENTFLAG_COMMAND_DOWN)) != 0;
  }

  // Middle-click and Control-click ask for a new tab. This window has no tab
  // strip. Report the gesture and cancel it. Returning false would navigate
  // the page that was clicked.
  bool OnOpenURLFromTab(CefRefPtr<CefBrowser> browser,
                        CefRefPtr<CefFrame> frame,
                        const CefString& target_url,
                        CefRequestHandler::WindowOpenDisposition target_disposition,
                        bool user_gesture) override {
    (void)browser;
    (void)frame;
    (void)user_gesture;
    const char* disposition = nullptr;
    switch (target_disposition) {
      case CEF_WOD_NEW_BACKGROUND_TAB:
        disposition = "background-tab";
        break;
      case CEF_WOD_NEW_FOREGROUND_TAB:
        disposition = "foreground-tab";
        break;
      default:
        return false;
    }
    int consumed = Emit(view_id_,
                        std::string("{\"event\":\"open-url-from-tab\",\"url\":") +
                            JsonString(target_url.ToString()) +
                            ",\"disposition\":" + JsonString(disposition) + "}");
    return consumed != 0;
  }

  bool OnBeforeBrowse(CefRefPtr<CefBrowser> browser,
                      CefRefPtr<CefFrame> frame,
                      CefRefPtr<CefRequest> request,
                      bool user_gesture,
                      bool is_redirect) override {
    (void)request;
    (void)user_gesture;
    if (router_)
      router_->OnBeforeBrowse(browser, frame);
    if (is_redirect && frame && frame->IsMain() && request)
      Emit(view_id_,
           "{\"event\":\"did-redirect-navigation\",\"url\":" +
               JsonString(request->GetURL().ToString()) + "}");
    return false;
  }

  CefRefPtr<CefResourceRequestHandler> GetResourceRequestHandler(
      CefRefPtr<CefBrowser> browser,
      CefRefPtr<CefFrame> frame,
      CefRefPtr<CefRequest> request,
      bool is_navigation,
      bool is_download,
      const CefString& request_initiator,
      bool& disable_default_handling) override {
    (void)browser;
    (void)frame;
    (void)request;
    (void)is_navigation;
    (void)is_download;
    (void)request_initiator;
    (void)disable_default_handling;
    return this;
  }

  CefResourceRequestHandler::ReturnValue OnBeforeResourceLoad(
      CefRefPtr<CefBrowser> browser,
      CefRefPtr<CefFrame> frame,
      CefRefPtr<CefRequest> request,
      CefRefPtr<CefCallback> callback) override {
    (void)browser;
    if (!request || !callback)
      return RV_CONTINUE;
    std::string url = request->GetURL().ToString();
    const char* type = ResourceTypeName(request->GetResourceType());
    if (!g_request_cb.load() || !type || !HttpUrl(url))
      return RV_CONTINUE;
    auto job = std::make_shared<RequestJob>();
    job->url = url;
    job->document_url = DocumentUrl(request, frame);
    job->type = type;
    job->id = request->GetIdentifier();
    job->async = callback;
    {
      std::lock_guard<std::mutex> lock(g_request_mu);
      if (g_request_stop.load())
        return RV_CONTINUE;
      g_request_queue.push(job);
    }
    g_request_cv.notify_one();
    ArmRequestTimeout(job);
    return RV_CONTINUE_ASYNC;
  }

  CefRefPtr<CefResponseFilter> GetResourceResponseFilter(
      CefRefPtr<CefBrowser> browser,
      CefRefPtr<CefFrame> frame,
      CefRefPtr<CefRequest> request,
      CefRefPtr<CefResponse> response) override {
    (void)browser;
    (void)frame;
    if (!request)
      return nullptr;
    auto replacements = ParseReplacements(ReplacementsText(request->GetIdentifier()));
    if (replacements.empty())
      return nullptr;
    std::string mime = response ? response->GetMimeType().ToString() : std::string();
    for (char& character : mime)
      character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    if (BinaryMime(mime))
      return nullptr;
    return new BodyReplaceFilter(std::move(replacements));
  }

  void OnResourceLoadComplete(CefRefPtr<CefBrowser> browser,
                              CefRefPtr<CefFrame> frame,
                              CefRefPtr<CefRequest> request,
                              CefRefPtr<CefResponse> response,
                              URLRequestStatus status,
                              int64_t received_content_length) override {
    (void)browser;
    (void)frame;
    (void)response;
    (void)status;
    (void)received_content_length;
    if (request)
      ForgetReplacements(request->GetIdentifier());
  }

  void OnResourceRedirect(CefRefPtr<CefBrowser> browser,
                          CefRefPtr<CefFrame> frame,
                          CefRefPtr<CefRequest> request,
                          CefRefPtr<CefResponse> response,
                          CefString& new_url) override {
    (void)browser;
    (void)request;
    (void)response;
    if (!frame || !frame->IsMain())
      return;
    Emit(view_id_,
         "{\"event\":\"did-redirect-navigation\",\"url\":" + JsonString(new_url.ToString()) + "}");
  }

  bool CanDownload(CefRefPtr<CefBrowser> browser,
                   const CefString& url,
                   const CefString& request_method) override {
    (void)browser;
    (void)url;
    (void)request_method;
    return true;
  }

  bool OnBeforeDownload(CefRefPtr<CefBrowser> browser,
                        CefRefPtr<CefDownloadItem> download_item,
                        const CefString& suggested_name,
                        CefRefPtr<CefBeforeDownloadCallback> callback) override {
    (void)browser;
    if (callback && download_item) {
      std::string name = suggested_name.ToString();
      for (char& character : name) {
        if (character == '/' || character == '\\')
          character = '_';
      }
      if (name.empty())
        name = "download";
      std::string directory = g_download_directory.empty() ? "/tmp" : g_download_directory;
      callback->Continue(directory + "/" + name, false);
    }
    return true;
  }

  void OnDownloadUpdated(CefRefPtr<CefBrowser> browser,
                         CefRefPtr<CefDownloadItem> download_item,
                         CefRefPtr<CefDownloadItemCallback> callback) override {
    (void)browser;
    if (!download_item)
      return;
    uint32_t id = download_item->GetId();
    if (callback)
      g_download_callbacks[id] = callback;
    const char* state = "interrupted";
    if (download_item->IsComplete())
      state = "completed";
    else if (download_item->IsCanceled())
      state = "cancelled";
    else if (download_item->IsInProgress())
      state = "progressing";
    int percent = download_item->GetPercentComplete();
    if (percent < 0)
      percent = download_item->IsComplete() ? 100 : 0;
    Emit(-1,
         std::string("{\"event\":\"download-item-updated\",\"id\":") + std::to_string(id) +
             ",\"url\":" + JsonString(download_item->GetURL().ToString()) +
             ",\"state\":" + JsonString(state) +
             ",\"receivedBytes\":" + std::to_string(download_item->GetReceivedBytes()) +
             ",\"totalBytes\":" + std::to_string(download_item->GetTotalBytes()) +
             ",\"percentComplete\":" + std::to_string(percent) +
             ",\"savePath\":" + JsonString(download_item->GetFullPath().ToString()) + "}");
  }

  void OnBeforeContextMenu(CefRefPtr<CefBrowser> browser,
                           CefRefPtr<CefFrame> frame,
                           CefRefPtr<CefContextMenuParams> params,
                           CefRefPtr<CefMenuModel> model) override {
    (void)browser;
    (void)frame;
    (void)params;
    if (!model)
      return;
    model->AddSeparator();
    g_menu = model;
    Emit(view_id_, "{\"event\":\"context-menu\"}");
    g_menu = nullptr;
  }

  bool OnContextMenuCommand(CefRefPtr<CefBrowser> browser,
                            CefRefPtr<CefFrame> frame,
                            CefRefPtr<CefContextMenuParams> params,
                            int command_id,
                            EventFlags event_flags) override {
    (void)browser;
    (void)frame;
    (void)params;
    (void)event_flags;
    if (command_id < MENU_ID_USER_FIRST)
      return false;
    Emit(view_id_,
         "{\"event\":\"context-menu-command\",\"commandId\":" + std::to_string(command_id) + "}");
    return true;
  }

  void OnTakeFocus(CefRefPtr<CefBrowser> browser, bool next) override {
    (void)browser;
    (void)next;
    auto view = FindView(view_id_);
    if (view)
      view->focused = false;
  }

  void OnGotFocus(CefRefPtr<CefBrowser> browser) override {
    (void)browser;
    auto view = FindView(view_id_);
    if (view)
      view->focused = true;
    Emit(view_id_, "{\"event\":\"focus\"}");
  }

 private:
  int view_id_;
  CefRefPtr<CefMessageRouterBrowserSide> router_;
  std::unique_ptr<QueryHandler> queries_;
  IMPLEMENT_REFCOUNTING(Client);
};

class BrowserViewDelegate : public CefBrowserViewDelegate {
 public:
  BrowserViewDelegate(cef_runtime_style_t style, int view_id)
      : style_(style), view_id_(view_id) {}

  // A Chrome window hosts one Chrome browser view. Further views (status,
  // message, prompt) must be Alloy or CEF refuses to add them.
  cef_runtime_style_t GetBrowserRuntimeStyle() override { return style_; }

  CefSize GetPreferredSize(CefRefPtr<CefView> view) override { return BarSize(view); }

  CefSize GetMinimumSize(CefRefPtr<CefView> view) override { return BarSize(view); }

  CefSize GetMaximumSize(CefRefPtr<CefView> view) override {
    (void)view;
    auto record = FindView(view_id_);
    if (!record || !record->bar)
      return CefSize();
    // Width may grow with the window. Height stays the bar height, so vh
    // units in the status document resolve against that strip.
    return CefSize(100000, record->bar_height);
  }

  int GetHeightForWidth(CefRefPtr<CefView> view, int width) override {
    (void)view;
    (void)width;
    auto record = FindView(view_id_);
    if (!record || !record->bar)
      return 0;
    return record->bar_height;
  }

  // Runs before OnBeforeClose. The browser is done; the view reference has to
  // be dropped after this callback returns, or the delegate is destroyed
  // while it is still on the stack.
  void OnBrowserDestroyed(CefRefPtr<CefBrowserView> browser_view,
                          CefRefPtr<CefBrowser> browser) override {
    (void)browser_view;
    (void)browser;
    auto record = FindView(view_id_);
    if (!record)
      return;
    record->browser = nullptr;
    CefPostTask(TID_UI, new ClosureTask([record]() { DropView(record); }));
  }

 private:
  CefSize BarSize(CefRefPtr<CefView> view) const {
    (void)view;
    auto record = FindView(view_id_);
    if (!record || !record->bar)
      return CefSize();
    return CefSize(1, record->bar_height);
  }

  cef_runtime_style_t style_;
  int view_id_;
  IMPLEMENT_REFCOUNTING(BrowserViewDelegate);
};

class WindowDelegate : public CefWindowDelegate {
 public:
  WindowDelegate(int id, int width, int height) : id_(id), width_(width), height_(height) {}

  // A Chrome-style window can host several Alloy browser views. An Alloy
  // window cannot, and Nyxt needs one view each for the page, status bar,
  // and message bar.
  cef_runtime_style_t GetWindowRuntimeStyle() override {
    return CEF_RUNTIME_STYLE_CHROME;
  }

  CefRect GetInitialBounds(CefRefPtr<CefWindow> window) override {
    (void)window;
    return CefRect(100, 100, width_, height_);
  }

  void OnWindowCreated(CefRefPtr<CefWindow> window) override {
    // Set the icon before the window is mapped. Chromium then installs its
    // product logo, so the host icon is applied again on later turns.
    ApplyHostIdentity(window);
    if (window)
      window->Show();
    int id = id_;
    CefPostTask(TID_UI, new ClosureTask([id]() {
                  if (auto record = FindWindow(id))
                    ApplyHostIdentity(record->window);
                }));
    CefPostDelayedTask(TID_UI,
                       new ClosureTask([id]() {
                         if (auto record = FindWindow(id))
                           ApplyHostIdentity(record->window);
                       }),
                       300);
  }

  void OnWindowDestroyed(CefRefPtr<CefWindow> window) override {
    (void)window;
    Emit(id_, "{\"event\":\"close\"}");
  }

  void OnWindowBoundsChanged(CefRefPtr<CefWindow> window, const CefRect& new_bounds) override {
    (void)window;
    // SetBounds from a resize listener calls back into this method. A nested
    // emit runs that listener again and overflows the UI thread.
    if (auto record = FindWindow(id_)) {
      record->width = new_bounds.width;
      record->height = new_bounds.height;
    }
    if (emitting_bounds_)
      return;
    emitting_bounds_ = true;
    Emit(id_,
         std::string("{\"event\":\"resize\",\"x\":") + std::to_string(new_bounds.x) +
             ",\"y\":" + std::to_string(new_bounds.y) +
             ",\"width\":" + std::to_string(new_bounds.width) +
             ",\"height\":" + std::to_string(new_bounds.height) + "}");
    emitting_bounds_ = false;
  }

  bool CanClose(CefRefPtr<CefWindow> window) override {
    (void)window;
    return true;
  }

 private:
  int id_;
  int width_;
  int height_;
  bool emitting_bounds_ = false;
  IMPLEMENT_REFCOUNTING(WindowDelegate);
};

class BrowserApp : public CefApp, public CefBrowserProcessHandler {
 public:
  void OnRegisterCustomSchemes(CefRawPtr<CefSchemeRegistrar> registrar) override {
    cl_cef_register_schemes(registrar);
  }

  CefRefPtr<CefBrowserProcessHandler> GetBrowserProcessHandler() override { return this; }

  // The browser command line disables the crash handler. Child processes are
  // launched with it turned back on, and that handler is meant to outlive the
  // browser, so it is still running after Nyxt has exited.
  void OnBeforeChildProcessLaunch(CefRefPtr<CefCommandLine> command_line) override {
    if (!command_line)
      return;
    command_line->RemoveSwitch("enable-crash-reporter");
    command_line->AppendSwitch("disable-breakpad");
    command_line->AppendSwitch("disable-crash-reporter");
    command_line->AppendSwitch("disable-in-process-stack-traces");
  }

 private:
  IMPLEMENT_REFCOUNTING(BrowserApp);
};

void RememberArgs(int argc, char** argv) {
  g_args.clear();
  g_args.reserve(static_cast<std::size_t>(argc));
  for (int i = 0; i < argc; ++i)
    g_args.emplace_back(argv && argv[i] ? argv[i] : "");
  g_argv.clear();
  g_argv.reserve(g_args.size());
  for (std::string& argument : g_args)
    g_argv.push_back(argument.data());
}

// CefShutdown returns while the helpers are still running when this process
// stays up, which is what a REPL quit does. The zygote leaves our process
// group, and a helper whose parent has already exited is reparented to this
// process (see the child-subreaper set before CefInitialize). Walk that tree
// and signal it. A second pass catches helpers that get reparented by the
// first.
void ReapHelpers() {
#if !defined(_WIN32)
  const pid_t self = getpid();
  for (int pass = 0; pass < 4; ++pass) {
    std::unordered_map<pid_t, std::vector<pid_t>> children;
    std::vector<pid_t> helpers;
    DIR* proc = opendir("/proc");
    if (!proc)
      return;
    while (dirent* entry = readdir(proc)) {
      if (entry->d_name[0] < '1' || entry->d_name[0] > '9')
        continue;
      pid_t pid = static_cast<pid_t>(std::atoi(entry->d_name));
      if (pid <= 0 || pid == self)
        continue;
      std::string dir = std::string("/proc/") + entry->d_name;
      FILE* status = std::fopen((dir + "/status").c_str(), "r");
      pid_t parent = 0;
      if (status) {
        char line[256];
        while (std::fgets(line, sizeof line, status)) {
          if (std::strncmp(line, "PPid:", 5) == 0) {
            parent = static_cast<pid_t>(std::atoi(line + 5));
            break;
          }
        }
        std::fclose(status);
      }
      if (parent > 0)
        children[parent].push_back(pid);
      char exe[4096];
      ssize_t length = ::readlink((dir + "/exe").c_str(), exe, sizeof exe - 1);
      bool helper = false;
      if (length > 0) {
        exe[length] = '\0';
        helper = std::strstr(exe, "cl-cef-helper") != nullptr;
      }
      if (!helper) {
        FILE* command = std::fopen((dir + "/cmdline").c_str(), "r");
        if (command) {
          char buffer[4096];
          size_t count = std::fread(buffer, 1, sizeof buffer - 1, command);
          for (size_t i = 0; i < count; ++i) {
            if (buffer[i] == '\0')
              buffer[i] = ' ';
          }
          buffer[count] = '\0';
          helper = std::strstr(buffer, "cl-cef-helper") != nullptr;
          std::fclose(command);
        }
      }
      if (helper)
        helpers.push_back(pid);
    }
    closedir(proc);
    std::vector<pid_t> pending{self};
    std::vector<pid_t> victims;
    for (size_t index = 0; index < pending.size(); ++index) {
      auto found = children.find(pending[index]);
      if (found == children.end())
        continue;
      for (pid_t child : found->second) {
        pending.push_back(child);
        for (pid_t helper : helpers) {
          if (helper == child)
            victims.push_back(child);
        }
      }
    }
    if (victims.empty())
      return;
    for (pid_t pid : victims)
      ::kill(pid, SIGKILL);
    for (pid_t pid : victims)
      ::waitpid(pid, nullptr, WNOHANG);
    ::usleep(20 * 1000);
  }
#endif
}

}  // namespace

extern "C" {

void cl_cef_response_set(cl_cef_response* response,
                         const char* data,
                         size_t size,
                         const char* mime_type,
                         int status,
                         const char* status_text,
                         const char* headers) {
  if (!response)
    return;
  if (data && size)
    response->data.assign(data, size);
  else
    response->data.clear();
  response->mime = mime_type && *mime_type ? mime_type : "text/html";
  response->status = status > 0 ? status : 200;
  response->status_text = status_text && *status_text ? status_text : "OK";
  response->headers = headers ? headers : "";
}

void cl_cef_set_event_callback(cl_cef_event_callback callback) { g_event_cb.store(callback); }

void cl_cef_set_scheme_callback(cl_cef_scheme_callback callback) { g_scheme_cb.store(callback); }

int cl_cef_scheme_pump(void) {
  std::shared_ptr<SchemeJob> job;
  {
    std::unique_lock<std::mutex> lock(g_scheme_mu);
    g_scheme_cv.wait(lock, [] { return g_scheme_stop.load() || !g_scheme_queue.empty(); });
    if (g_scheme_queue.empty())
      return 0;
    job = std::move(g_scheme_queue.front());
    g_scheme_queue.pop();
  }
  cl_cef_scheme_callback callback = g_scheme_cb.load();
  if (callback && job) {
    try {
      callback(job->scheme.c_str(), job->url.c_str(), &job->response);
    } catch (...) {
      job->response.status = 500;
      job->response.status_text = "Error";
      job->response.mime = "text/plain";
      job->response.data = "scheme handler failed";
    }
  }
  if (job) {
    // Publish the body before Continue. GetResponseHeaders locks this mutex.
    job->mu.lock();
    job->mu.unlock();
    if (job->async)
      PostCefCallback(job->async, false);
    try {
      job->done.set_value();
    } catch (...) {
    }
  }
  return 1;
}

void cl_cef_scheme_stop(void) {
  std::lock_guard<std::mutex> lock(g_scheme_mu);
  g_scheme_stop.store(true);
  while (!g_scheme_queue.empty()) {
    auto pending = g_scheme_queue.front();
    g_scheme_queue.pop();
    if (pending->async)
      PostCefCallback(pending->async, true);
    try {
      pending->done.set_value();
    } catch (...) {
    }
  }
  g_scheme_cv.notify_all();
}

void cl_cef_request_decision_set(cl_cef_request_decision* decision,
                                 int block,
                                 const char* css) {
  if (!decision)
    return;
  decision->block = block ? 1 : 0;
  decision->css = css ? css : "";
  decision->replacements.clear();
}

void cl_cef_request_decision_replace(cl_cef_request_decision* decision,
                                     const char* replacements) {
  if (!decision)
    return;
  decision->replacements = replacements ? replacements : "";
}

void cl_cef_set_request_callback(cl_cef_request_callback callback) {
  g_request_stop.store(false);
  g_request_cb.store(callback);
  g_request_cv.notify_all();
}

int cl_cef_request_pump(void) {
  std::shared_ptr<RequestJob> job;
  {
    std::unique_lock<std::mutex> lock(g_request_mu);
    g_request_cv.wait(lock, [] { return g_request_stop.load() || !g_request_queue.empty(); });
    if (g_request_queue.empty())
      return 0;
    job = std::move(g_request_queue.front());
    g_request_queue.pop();
  }
  cl_cef_request_callback callback = g_request_cb.load();
  if (callback && job) {
    try {
      callback(job->url.c_str(), job->document_url.c_str(), job->type.c_str(), &job->decision);
    } catch (...) {
      job->decision.block = 0;
      job->decision.css.clear();
      job->decision.replacements.clear();
    }
  }
  if (job) {
    try {
      FinishRequest(job);
    } catch (...) {
      job->decision.block = 0;
      job->decision.replacements.clear();
      if (!job->settled.load())
        FinishRequest(job);
    }
    try {
      job->done.set_value();
    } catch (...) {
    }
  }
  return 1;
}

void cl_cef_request_stop(void) {
  std::lock_guard<std::mutex> lock(g_request_mu);
  g_request_stop.store(true);
  while (!g_request_queue.empty()) {
    std::shared_ptr<RequestJob> job = g_request_queue.front();
    g_request_queue.pop();
    FinishRequest(job);
    job->done.set_value();
  }
  g_request_cv.notify_all();
}

void cl_cef_adblock_reset_cosmetic(void) { ResetCosmetic(); }

int cl_cef_initialize(int argc,
                      char** argv,
                      const char* subprocess_path,
                      const char* cache_path,
                      const char* resources_dir_path,
                      const char* locales_dir_path,
                      const char* user_agent,
                      const char* schemes,
                      int no_sandbox,
                      int remote_debugging_port) {
#if defined(OS_WIN)
  (void)argc;
  (void)argv;
  (void)subprocess_path;
  (void)cache_path;
  (void)resources_dir_path;
  (void)locales_dir_path;
  (void)user_agent;
  (void)schemes;
  (void)no_sandbox;
  (void)remote_debugging_port;
  return 0;
#else
  cl_cef_note_schemes(schemes);
  cl_cef_note_schemes_from_argv(argc, argv);
  MaskFpTraps();
  cl_cef_write_scheme_file(cache_path);
  RememberArgs(argc, argv);
  CefMainArgs main_args(static_cast<int>(g_argv.size()), g_argv.data());
  CefRefPtr<BrowserApp> app = new BrowserApp();
  int subprocess = CefExecuteProcess(main_args, app, nullptr);
  if (subprocess >= 0)
    std::_Exit(subprocess);

#if defined(__linux__)
  // Orphans from the zygote must stay descendants of this process. Otherwise
  // a REPL quit, which does not exit Lisp, loses them to init and they keep
  // running.
  prctl(PR_SET_CHILD_SUBREAPER, 1);
#endif

  CefSettings settings;
  settings.no_sandbox = no_sandbox ? 1 : 0;
  settings.windowless_rendering_enabled = 0;
  settings.persist_session_cookies = 1;
  settings.log_severity = LOGSEVERITY_WARNING;
  if (remote_debugging_port > 0)
    settings.remote_debugging_port = remote_debugging_port;
  if (subprocess_path && *subprocess_path)
    CefString(&settings.browser_subprocess_path) = subprocess_path;
  if (cache_path && *cache_path) {
    CefString(&settings.root_cache_path) = cache_path;
    CefString(&settings.cache_path) = std::string(cache_path) + "/cache";
  }
  if (resources_dir_path && *resources_dir_path)
    CefString(&settings.resources_dir_path) = resources_dir_path;
  if (locales_dir_path && *locales_dir_path)
    CefString(&settings.locales_dir_path) = locales_dir_path;
  if (user_agent && *user_agent)
    CefString(&settings.user_agent) = user_agent;
  if (!CefInitialize(main_args, settings, app, nullptr))
    return 0;
  g_scheme_names.clear();
  for (const auto& scheme : cl_cef_parse_schemes(schemes))
    g_scheme_names.push_back(scheme.name);
  RegisterSchemeFactories(nullptr);
  g_initialized.store(true);
  return 1;
#endif
}

void cl_cef_run_message_loop(cl_cef_ready_callback ready) {
  MaskFpTraps();
  if (!g_initialized.load())
    return;
  if (ready)
    CefPostTask(TID_UI, new ClosureTask([ready]() { ready(); }));
  CefRunMessageLoop();
  g_loop_done.store(true);
  std::fprintf(stderr, "cl-cef message loop returned\n");
  std::fflush(stderr);
}

void cl_cef_prepare_shutdown(void) { BeginShutdown(); }

void cl_cef_quit_message_loop(void) {
  std::fprintf(stderr, "cl-cef quit message loop\n");
  std::fflush(stderr);
  if (g_initialized.load())
    CefQuitMessageLoop();
}

void cl_cef_post(cl_cef_task_callback callback) {
  if (!callback || !g_initialized.load())
    return;
  // Always queue. An inline call re-enters CEF from the callback that posted
  // the task, which is how resize listeners overflow the UI thread.
  CefPostTask(TID_UI, new ClosureTask([callback]() { callback(); }));
}

void cl_cef_shutdown(void) {
  if (g_initialized.exchange(false)) {
    // Browsers are already closed. Dropping the view widgets here deadlocks:
    // their destructors wait on the message loop, which has returned.
    CefShutdown();
  }
  // The helpers outlive CefShutdown when the Lisp process keeps running.
  ReapHelpers();
}

void cl_cef_reap_helpers(void) { ReapHelpers(); }

int cl_cef_on_ui_thread(void) { return CefCurrentlyOn(TID_UI) ? 1 : 0; }

void cl_cef_string_free(char* string) { std::free(string); }

void cl_cef_set_download_directory(const char* path) {
  g_download_directory = path ? path : "";
}

void cl_cef_add_app_icon_png(float scale_factor, const void* png, size_t size) {
  RememberAppIcon(scale_factor, png, size);
}

void cl_cef_set_window_class(const char* name) { RememberWindowClass(name); }

int cl_cef_window_create(int width, int height, const char* title) {
  int id = AllocateId();
  auto record = std::make_shared<WindowRecord>();
  record->id = id;
  record->width = width > 0 ? width : 1600;
  record->height = height > 0 ? height : 1200;
  record->title = title ? title : "Nyxt";
  {
    std::lock_guard<std::mutex> lock(g_mu);
    g_windows[id] = record;
  }
  CallUI([&]() {
    record->delegate = new WindowDelegate(id, record->width, record->height);
    record->window = CefWindow::CreateTopLevelWindow(record->delegate);
    if (record->window) {
      // Vertical stack: the flexible page, then the fixed bars. Without this,
      // a browser view's preferred size is the whole window and Chrome parks
      // it at the top.
      CefBoxLayoutSettings settings;
      settings.horizontal = 0;
      settings.between_child_spacing = 0;
      settings.default_flex = 0;
      settings.cross_axis_alignment = CEF_AXIS_ALIGNMENT_STRETCH;
      record->layout = record->window->SetToBoxLayout(settings);
      record->window->SetTitle(record->title);
      record->window->Show();
      ApplyHostIdentity(record->window);
    }
  });
  if (!record->window) {
    std::lock_guard<std::mutex> lock(g_mu);
    g_windows.erase(id);
    return 0;
  }
  return id;
}

void cl_cef_window_close(int id) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window) {
      UnmapNativeWindow(record->window);
      record->window->Close();
    }
  });
  std::lock_guard<std::mutex> lock(g_mu);
  g_windows.erase(id);
}

void cl_cef_window_set_title(int id, const char* title) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window && title)
      record->window->SetTitle(title);
  });
}

char* cl_cef_window_get_title(int id) {
  auto record = FindWindow(id);
  std::string title = CallUI([&]() {
    return record && record->window ? record->window->GetTitle().ToString() : std::string();
  });
  return Dup(title);
}

void cl_cef_window_fullscreen(int id, int on) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window)
      record->window->SetFullscreen(on != 0);
  });
}

void cl_cef_window_maximize(int id) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window)
      record->window->Maximize();
  });
}

void cl_cef_window_restore(int id) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window)
      record->window->Restore();
  });
}

void cl_cef_window_focus(int id) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (!record || !record->window)
      return;
    record->window->Show();
    record->window->Activate();
    record->window->BringToTop();
  });
}

int cl_cef_window_is_focused(int id) {
  auto record = FindWindow(id);
  return CallUI([&]() { return record && record->window && record->window->IsActive() ? 1 : 0; });
}

void cl_cef_window_bounds(int id, int* x, int* y, int* width, int* height) {
  auto record = FindWindow(id);
  CefRect bounds = CallUI([&]() {
    return record && record->window ? record->window->GetBounds() : CefRect();
  });
  if (x) *x = bounds.x;
  if (y) *y = bounds.y;
  if (width) *width = bounds.width;
  if (height) *height = bounds.height;
}

void cl_cef_window_content_bounds(int id, int* x, int* y, int* width, int* height) {
  // GetClientAreaBoundsInScreen faults once a browser view has been added.
  auto record = FindWindow(id);
  if (x) *x = 0;
  if (y) *y = 0;
  if (width) *width = record ? record->width : 0;
  if (height) *height = record ? record->height : 0;
}

void cl_cef_window_set_bounds(int id, int x, int y, int width, int height) {
  auto record = FindWindow(id);
  CallUI([&]() {
    if (record && record->window)
      record->window->SetBounds(CefRect(x, y, width, height));
  });
}

void PlaceView(const std::shared_ptr<WindowRecord>& window,
               const std::shared_ptr<ViewRecord>& view,
               int index) {
  if (!window || !window->window || !view || !view->widget)
    return;
  // Remove + Add on a view that is already parented registers its widget
  // observer a second time. Chromium then aborts the UI thread.
  CefRefPtr<CefView> parent = view->widget->GetParentView();
  if (parent && !parent->IsSame(window->window)) {
    if (auto panel = parent->AsPanel())
      panel->RemoveChildView(view->widget);
    parent = view->widget->GetParentView();
  }
  bool inserted = false;
  if (!parent) {
    if (index < 0)
      window->window->AddChildView(view->widget);
    else
      window->window->AddChildViewAt(view->widget, index);
    inserted = true;
  } else if (index >= 0) {
    window->window->ReorderChildView(view->widget, index);
  }
  if (window->layout && (inserted || !view->flex_set)) {
    window->layout->SetFlexForView(view->widget, view->bar ? 0 : 1);
    view->flex_set = true;
  }
  window->window->Layout();
  // Adding a browser view is when Chrome paints the product logo again.
  ApplyHostIdentity(window->window);
}

void cl_cef_window_add_view(int window_id, int view_id) {
  auto window = FindWindow(window_id);
  auto view = FindView(view_id);
  CallUI([window, view]() { PlaceView(window, view, -1); });
}

void cl_cef_window_add_view_at(int window_id, int view_id, int index) {
  auto window = FindWindow(window_id);
  auto view = FindView(view_id);
  CallUI([window, view, index]() { PlaceView(window, view, index); });
}

void cl_cef_window_remove_view(int window_id, int view_id) {
  auto window = FindWindow(window_id);
  auto view = FindView(view_id);
  CallUI([&]() {
    if (window && window->window && view && view->widget)
      window->window->RemoveChildView(view->widget);
  });
}

int cl_cef_view_create(const char* url, int alloy, int isolated) {
  int id = AllocateId();
  auto record = std::make_shared<ViewRecord>();
  record->id = id;
  record->isolated = isolated != 0;
  {
    std::lock_guard<std::mutex> lock(g_mu);
    g_views[id] = record;
  }
  std::string initial = url && *url ? url : "about:blank";
  cef_runtime_style_t style = alloy ? CEF_RUNTIME_STYLE_ALLOY : CEF_RUNTIME_STYLE_CHROME;
  CallUI([record, initial, id, style]() {
    CefRequestContextSettings context_settings;
    record->context = CefRequestContext::CreateContext(context_settings, nullptr);
    // The status, message, and prompt views use this context. Their buttons
    // fetch lisp: URLs, which only resolve where a factory is registered.
    if (record->isolated)
      RegisterSchemeFactories(record->context);
    CefBrowserSettings browser_settings;
    browser_settings.javascript = STATE_ENABLED;
    browser_settings.image_loading = STATE_ENABLED;
    browser_settings.webgl = STATE_ENABLED;
    CefRefPtr<Client> client = new Client(id);
    record->widget = CefBrowserView::CreateBrowserView(
        client, initial, browser_settings, nullptr,
        record->isolated ? record->context : nullptr,
        new BrowserViewDelegate(style, id));
  });
  if (!record->widget) {
    std::lock_guard<std::mutex> lock(g_mu);
    g_views.erase(id);
    return 0;
  }
  return id;
}

void cl_cef_view_destroy(int id) {
  auto record = FindView(id);
  if (!record)
    return;
  // CefRefPtrs have to be released on the UI thread. Releasing the browser
  // view here, before CloseBrowser has destroyed the browser, hits
  // ~CefViewView's CHECK. OnBrowserDestroyed drops the view once that is done.
  // A browser that is still being created is closed from OnAfterCreated.
  CallUI([record]() {
    if (record->destroying)
      return;
    record->destroying = true;
    record->pending.clear();
    CefRefPtr<CefBrowser> browser = record->browser;
    if (!browser && record->widget)
      browser = record->widget->GetBrowser();
    if (record->widget) {
      if (CefRefPtr<CefView> parent = record->widget->GetParentView()) {
        if (auto panel = parent->AsPanel())
          panel->RemoveChildView(record->widget);
      }
    }
    if (browser) {
      record->browser = browser;
      record->close_started = true;
      browser->GetHost()->SetAudioMuted(true);
      browser->GetHost()->CloseBrowser(true);
      return;
    }
    // Creation has not reported a browser yet. Give OnAfterCreated one turn,
    // then drop the view if none appeared.
    CefPostTask(TID_UI, new ClosureTask([record]() {
                  if (record->close_started)
                    return;
                  CefRefPtr<CefBrowser> late = record->browser;
                  if (!late && record->widget)
                    late = record->widget->GetBrowser();
                  if (late) {
                    record->browser = late;
                    record->close_started = true;
                    late->GetHost()->SetAudioMuted(true);
                    late->GetHost()->CloseBrowser(true);
                    return;
                  }
                  DropView(record);
                }));
  });
}

void cl_cef_view_set_bar_height(int id, int height) {
  auto record = FindView(id);
  if (!record)
    return;
  record->bar = true;
  record->bar_height = height > 0 ? height : 0;
  record->height = record->bar_height;
  CallUI([record]() {
    if (!record->widget)
      return;
    // A preferred height of 0 is an empty size, and the box layout then keeps
    // the view's own size. Hiding the view drops it from the layout.
    record->widget->SetVisible(record->bar_height > 0);
    record->widget->InvalidateLayout();
    if (auto window = record->widget->GetWindow())
      window->Layout();
  });
}

void cl_cef_view_set_bounds(int id, int x, int y, int width, int height) {
  auto record = FindView(id);
  if (record) {
    record->x = x;
    record->y = y;
    record->width = width;
    record->height = height;
    if (record->bar)
      record->bar_height = height > 0 ? height : 0;
  }
  // CefView::GetBounds faults once the view is in a window. The box layout
  // owns child rectangles; SetBounds on a bar expands it back to the
  // browser's preferred size.
  CallUI([record, x, y, width, height]() {
    if (!record || !record->widget)
      return;
    if (record->bar)
      record->widget->SetVisible(height > 0);
    if (auto window = record->widget->GetWindow()) {
      if (window->GetLayout()) {
        record->widget->InvalidateLayout();
        window->Layout();
        return;
      }
    }
    record->widget->SetBounds(CefRect(x, y, width, height));
  });
}

void cl_cef_view_get_bounds(int id, int* x, int* y, int* width, int* height) {
  auto record = FindView(id);
  if (x) *x = record ? record->x : 0;
  if (y) *y = record ? record->y : 0;
  if (width) *width = record ? record->width : 0;
  if (height) *height = record ? record->height : 0;
}

void cl_cef_view_focus(int id) {
  auto record = FindView(id);
  CallUI([record]() {
    if (record && record->widget)
      record->widget->RequestFocus();
  });
}

int cl_cef_view_is_focused(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record && record->focused ? 1 : 0; });
}

void cl_cef_view_load_url(int id, const char* url) {
  auto record = FindView(id);
  std::string target = url ? url : "";
  // WithBrowser may run the callback after this function returns, so the
  // callback owns its copy of the URL.
  CallUI([record, target]() {
    WithBrowser(record, [target](CefRefPtr<CefBrowser> browser) {
      if (browser && browser->GetMainFrame())
        browser->GetMainFrame()->LoadURL(target);
    });
  });
}

void cl_cef_view_load_html(int id, const char* html) {
  auto record = FindView(id);
  std::string target = std::string("data:text/html;base64,") + Base64(html ? html : "");
  CallUI([record, target]() {
    WithBrowser(record, [target](CefRefPtr<CefBrowser> browser) {
      if (browser && browser->GetMainFrame())
        browser->GetMainFrame()->LoadURL(target);
    });
  });
}

char* cl_cef_view_get_url(int id) {
  auto record = FindView(id);
  std::string url = CallUI([&]() {
    if (record && record->browser && record->browser->GetMainFrame())
      return record->browser->GetMainFrame()->GetURL().ToString();
    return std::string();
  });
  return Dup(url);
}

char* cl_cef_view_get_title(int id) {
  auto record = FindView(id);
  std::string title = CallUI([&]() {
    return record ? record->title : std::string();
  });
  return Dup(title);
}

void cl_cef_view_reload(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser)
      record->browser->Reload();
  });
}

void cl_cef_view_stop(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser)
      record->browser->StopLoad();
  });
}

void cl_cef_view_go_back(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser && record->browser->CanGoBack())
      record->browser->GoBack();
  });
}

void cl_cef_view_go_forward(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser && record->browser->CanGoForward())
      record->browser->GoForward();
  });
}

int cl_cef_view_can_go_back(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record && record->browser && record->browser->CanGoBack() ? 1 : 0; });
}

int cl_cef_view_can_go_forward(int id) {
  auto record = FindView(id);
  return CallUI(
      [&]() { return record && record->browser && record->browser->CanGoForward() ? 1 : 0; });
}

void cl_cef_view_set_zoom_factor(int id, double factor) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser && factor > 0.0)
      record->browser->GetHost()->SetZoomLevel(FactorToLevel(factor));
  });
}

double cl_cef_view_get_zoom_factor(int id) {
  auto record = FindView(id);
  return CallUI([&]() {
    if (!record || !record->browser)
      return 1.0;
    return LevelToFactor(record->browser->GetHost()->GetZoomLevel());
  });
}

void cl_cef_view_set_audio_muted(int id, int muted) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    record->muted = muted != 0;
    if (record->browser)
      record->browser->GetHost()->SetAudioMuted(record->muted);
  });
}

int cl_cef_view_is_audio_muted(int id) {
  auto record = FindView(id);
  return CallUI([&]() {
    if (!record)
      return 0;
    if (record->browser)
      record->muted = record->browser->GetHost()->IsAudioMuted();
    return record->muted ? 1 : 0;
  });
}

void cl_cef_view_show_devtools(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record || !record->browser)
      return;
    CefWindowInfo info;
    CefBrowserSettings settings;
    record->browser->GetHost()->ShowDevTools(info, nullptr, settings, CefPoint());
  });
}

void cl_cef_view_close_devtools(int id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser)
      record->browser->GetHost()->CloseDevTools();
  });
}

void Edit(int id, void (CefFrame::*method)()) {
  auto record = FindView(id);
  CallUI([&]() {
    if (record && record->browser && record->browser->GetFocusedFrame())
      (record->browser->GetFocusedFrame().get()->*method)();
  });
}

void cl_cef_view_undo(int id) { Edit(id, &CefFrame::Undo); }
void cl_cef_view_redo(int id) { Edit(id, &CefFrame::Redo); }
void cl_cef_view_cut(int id) { Edit(id, &CefFrame::Cut); }
void cl_cef_view_copy(int id) { Edit(id, &CefFrame::Copy); }
void cl_cef_view_paste(int id) { Edit(id, &CefFrame::Paste); }
void cl_cef_view_select_all(int id) { Edit(id, &CefFrame::SelectAll); }

void cl_cef_view_insert_text(int id, const char* text) {
  auto record = FindView(id);
  std::string script = "document.execCommand('insertText', false, " + JsonString(text ? text : "") + ");";
  CallUI([record, script]() {
    WithBrowser(record, [script](CefRefPtr<CefBrowser> browser) {
      if (browser && browser->GetFocusedFrame())
        browser->GetFocusedFrame()->ExecuteJavaScript(script, "", 0);
    });
  });
}

void cl_cef_view_execute_javascript(int id, const char* code) {
  auto record = FindView(id);
  std::string script = code ? code : "";
  CallUI([record, script]() {
    WithBrowser(record, [script](CefRefPtr<CefBrowser> browser) {
      if (browser && browser->GetMainFrame())
        browser->GetMainFrame()->ExecuteJavaScript(script, "", 0);
    });
  });
}

char* cl_cef_view_execute_javascript_sync(int id, const char* code, int* success) {
  if (success)
    *success = 0;
  if (CefCurrentlyOn(TID_UI))
    return nullptr;
  auto record = FindView(id);
  if (!record)
    return nullptr;
  auto waiter = std::make_shared<JsWaiter>();
  int message_id = CallUI([&]() {
    if (!record->browser)
      return 0;
    CefRefPtr<CefBrowserHost> host = record->browser->GetHost();
    if (!record->devtools_observer) {
      record->devtools_observer = new DevToolsObserver();
      record->devtools_registration = host->AddDevToolsMessageObserver(record->devtools_observer);
    }
    CefRefPtr<CefDictionaryValue> params = CefDictionaryValue::Create();
    params->SetString("expression", code ? code : "");
    params->SetBool("returnByValue", true);
    params->SetBool("awaitPromise", true);
    int assigned = ++record->devtools_message_id;
    record->devtools_observer->Expect(assigned, waiter);
    host->ExecuteDevToolsMethod(assigned, "Runtime.evaluate", params);
    return assigned;
  });
  if (message_id <= 0)
    return nullptr;
  {
    std::unique_lock<std::mutex> lock(waiter->mu);
    waiter->cv.wait_for(lock, std::chrono::seconds(30), [&]() { return waiter->done; });
  }
  if (success)
    *success = waiter->success ? 1 : 0;
  return waiter->success ? Dup(waiter->json) : nullptr;
}

char* cl_cef_view_insert_css(int id, const char* css) {
  auto record = FindView(id);
  if (!record || !css)
    return nullptr;
  std::string key = CallUI([record, css = std::string(css)]() {
    std::string identifier = "cl-cef-css-" + std::to_string(record->next_css_id++);
    record->css[identifier] = css;
    WithBrowser(record, [identifier, css](CefRefPtr<CefBrowser> browser) {
      if (browser)
        ApplyCss(browser->GetMainFrame(), identifier, css);
    });
    return identifier;
  });
  return Dup(key);
}

void cl_cef_view_remove_inserted_css(int id, const char* key) {
  auto record = FindView(id);
  if (!record || !key)
    return;
  std::string identifier(key);
  CallUI([record, identifier]() {
    record->css.erase(identifier);
    std::string script =
        "(()=>{var node=document.getElementById(" + JsonString(identifier) +
        ");if(node)node.remove();})()";
    WithBrowser(record, [script](CefRefPtr<CefBrowser> browser) {
      if (browser && browser->GetMainFrame())
        browser->GetMainFrame()->ExecuteJavaScript(script, "", 0);
    });
  });
}

int cl_cef_view_add_user_script(int id, const char* code, int document_start, int all_frames) {
  auto record = FindView(id);
  if (!record || !code)
    return 0;
  return CallUI([&]() {
    UserScript script;
    script.id = record->next_script_id++;
    script.code = code;
    script.at_start = document_start != 0;
    script.all_frames = all_frames != 0;
    record->scripts.push_back(script);
    if (!script.at_start) {
      WithBrowser(record, [code = script.code](CefRefPtr<CefBrowser> browser) {
        if (browser && browser->GetMainFrame())
          browser->GetMainFrame()->ExecuteJavaScript(code, "", 0);
      });
    }
    return script.id;
  });
}

void cl_cef_view_remove_user_script(int id, int script_id) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    auto& scripts = record->scripts;
    scripts.erase(std::remove_if(scripts.begin(), scripts.end(),
                                 [&](const UserScript& script) { return script.id == script_id; }),
                  scripts.end());
  });
}

void cl_cef_view_download_url(int id, const char* url) {
  auto record = FindView(id);
  std::string target = url ? url : "";
  CallUI([&]() {
    WithBrowser(record, [&](CefRefPtr<CefBrowser> browser) {
      browser->GetHost()->StartDownload(target);
    });
  });
}

void cl_cef_view_set_javascript_enabled(int id, int enabled) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    record->javascript = enabled != 0;
    SetBoolPreference(record->context, "webkit.webprefs.javascript_enabled", record->javascript);
  });
}

int cl_cef_view_javascript_enabled(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record && record->javascript ? 1 : 0; });
}

void cl_cef_view_set_images_enabled(int id, int enabled) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    record->images = enabled != 0;
    SetBoolPreference(record->context, "webkit.webprefs.loads_images_automatically", record->images);
  });
}

int cl_cef_view_images_enabled(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record && record->images ? 1 : 0; });
}

void cl_cef_view_set_webgl_enabled(int id, int enabled) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    record->webgl = enabled != 0;
    SetBoolPreference(record->context, "webkit.webprefs.webgl_enabled", record->webgl);
  });
}

int cl_cef_view_webgl_enabled(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record && record->webgl ? 1 : 0; });
}

void cl_cef_view_set_user_agent(int id, const char* user_agent) {
  auto record = FindView(id);
  std::string agent = user_agent ? user_agent : "";
  CallUI([&]() {
    if (!record)
      return;
    record->user_agent = agent;
    WithBrowser(record, [&](CefRefPtr<CefBrowser> browser) {
      CefRefPtr<CefDictionaryValue> params = CefDictionaryValue::Create();
      params->SetString("userAgent", agent);
      browser->GetHost()->ExecuteDevToolsMethod(0, "Network.setUserAgentOverride", params);
    });
  });
}

char* cl_cef_view_get_user_agent(int id) {
  auto record = FindView(id);
  std::string agent = CallUI([&]() { return record ? record->user_agent : std::string(); });
  return Dup(agent);
}

void cl_cef_view_set_proxy(int id, const char* proxy_server, const char* bypass_list) {
  auto record = FindView(id);
  std::string server = proxy_server ? proxy_server : "";
  std::string bypass = bypass_list ? bypass_list : "";
  CallUI([&]() {
    if (!record)
      return;
    record->proxy = server;
    SetProxyPreference(record->context, server, bypass);
  });
}

char* cl_cef_view_get_proxy(int id) {
  auto record = FindView(id);
  std::string proxy = CallUI([&]() { return record ? record->proxy : std::string(); });
  return Dup(proxy);
}

void cl_cef_view_set_cookie_policy(int id, int policy) {
  auto record = FindView(id);
  CallUI([&]() {
    if (!record)
      return;
    record->cookie_policy = policy;
    SetCookiePreference(record->context, policy);
  });
}

int cl_cef_view_cookie_policy(int id) {
  auto record = FindView(id);
  return CallUI([&]() { return record ? record->cookie_policy : 0; });
}

void cl_cef_download_cancel(unsigned int download_id) {
  CallUI([&]() {
    auto found = g_download_callbacks.find(download_id);
    if (found != g_download_callbacks.end() && found->second)
      found->second->Cancel();
  });
}

void cl_cef_menu_add_separator(void) {
  if (CefCurrentlyOn(TID_UI) && g_menu)
    g_menu->AddSeparator();
}

void cl_cef_menu_add_item(int command_id, const char* label) {
  if (CefCurrentlyOn(TID_UI) && g_menu && label)
    g_menu->AddItem(command_id, label);
}

}  // extern "C"
