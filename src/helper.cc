/*
 * SPDX-FileCopyrightText: Atlas Engineer LLC
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * CEF subprocess. The browser process points browser_subprocess_path here.
 * Custom schemes are read from CL_CEF_SCHEMES, which the parent publishes
 * before CefInitialize. The render-process side of CefMessageRouter installs
 * window.cefQuery so page scripts can report input to the browser process.
 */

#include "schemes.hh"

#include "include/cef_app.h"
#include "include/cef_render_process_handler.h"
#include "include/wrapper/cef_message_router.h"

#include <cstdlib>

namespace {

const char kInputScript[] = R"JS(
(function () {
  if (window.__clCefInput) return;
  window.__clCefInput = true;
  function send(payload) {
    if (!window.cefQuery) return;
    window.cefQuery({
      request: JSON.stringify(payload),
      persistent: false,
      onSuccess: function () {},
      onFailure: function () {}
    });
  }
  document.addEventListener("mousedown", function (event) {
    send({
      kind: "button",
      button: event.button,
      shift: !!event.shiftKey,
      control: !!event.ctrlKey,
      alt: !!event.altKey,
      meta: !!event.metaKey
    });
  }, true);
  document.addEventListener("wheel", function (event) {
    send({
      kind: "wheel",
      dx: event.deltaX,
      dy: event.deltaY,
      shift: !!event.shiftKey,
      control: !!event.ctrlKey,
      alt: !!event.altKey,
      meta: !!event.metaKey
    });
  }, true);
  document.addEventListener("mouseover", function (event) {
    var node = event.target;
    var href = "";
    while (node) {
      if (node.href) { href = node.href; break; }
      node = node.parentElement;
    }
    if (href) send({kind: "hover", url: href});
  }, true);
})();
)JS";

class HelperApp : public CefApp, public CefRenderProcessHandler {
 public:
  void OnRegisterCustomSchemes(CefRawPtr<CefSchemeRegistrar> registrar) override {
    cl_cef_register_schemes(registrar);
  }

  CefRefPtr<CefRenderProcessHandler> GetRenderProcessHandler() override { return this; }

  void OnWebKitInitialized() override {
    router_ = CefMessageRouterRendererSide::Create(CefMessageRouterConfig());
  }

  void OnContextCreated(CefRefPtr<CefBrowser> browser,
                        CefRefPtr<CefFrame> frame,
                        CefRefPtr<CefV8Context> context) override {
    if (router_)
      router_->OnContextCreated(browser, frame, context);
    if (frame)
      frame->ExecuteJavaScript(kInputScript, frame->GetURL(), 0);
  }

  void OnContextReleased(CefRefPtr<CefBrowser> browser,
                         CefRefPtr<CefFrame> frame,
                         CefRefPtr<CefV8Context> context) override {
    if (router_)
      router_->OnContextReleased(browser, frame, context);
  }

  bool OnProcessMessageReceived(CefRefPtr<CefBrowser> browser,
                                CefRefPtr<CefFrame> frame,
                                CefProcessId source_process,
                                CefRefPtr<CefProcessMessage> message) override {
    return router_ && router_->OnProcessMessageReceived(browser, frame, source_process, message);
  }

 private:
  CefRefPtr<CefMessageRouterRendererSide> router_;
  IMPLEMENT_REFCOUNTING(HelperApp);
};

}  // namespace

int main(int argc, char* argv[]) {
  cl_cef_note_schemes_from_argv(argc, argv);
  cl_cef_note_schemes_from_user_data(argc, argv);
  CefMainArgs main_args(argc, argv);
  CefRefPtr<HelperApp> app = new HelperApp();
  int code = CefExecuteProcess(main_args, app, nullptr);
  if (code >= 0)
    return code;
  /* Launched as a browser process by mistake. */
  return 1;
}
