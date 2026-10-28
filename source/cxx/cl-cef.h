/*
 * SPDX-FileCopyrightText: Atlas Engineer LLC
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * C ABI over the Chromium Embedded Framework.
 * Common Lisp binds this header with CFFI; CEF's own API is C++.
 *
 * Build against CEF 124 or newer (Views framework, Alloy runtime).
 */

#ifndef CL_CEF_H
#define CL_CEF_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque response filled by cl_cef_response_set from a scheme callback. */
typedef struct cl_cef_response cl_cef_response;

/*
 * SCHEME and URL are NUL-terminated. RESPONSE stays valid for the duration of
 * the call. Implementations must copy any data they need and should return
 * quickly; the callback runs on CEF's IO thread.
 */
typedef void (*cl_cef_scheme_callback)(const char* scheme,
                                       const char* url,
                                       cl_cef_response* response);

/*
 * JSON object describing one event. The return value is meaningful for
 * synchronous events (:before-input-event, :window-open): non-zero consumes
 * the event. The callback runs on the CEF UI thread and must not wait on that
 * thread.
 */
typedef int (*cl_cef_event_callback)(int object_id, const char* json);

typedef void (*cl_cef_ready_callback)(void);
typedef void (*cl_cef_task_callback)(void);

/* Copies DATA into RESPONSE. DATA may be NULL when SIZE is 0. */
void cl_cef_response_set(cl_cef_response* response,
                         const char* data,
                         size_t size,
                         const char* mime_type,
                         int status,
                         const char* status_text,
                         const char* headers);

void cl_cef_set_event_callback(cl_cef_event_callback callback);
void cl_cef_set_scheme_callback(cl_cef_scheme_callback callback);

/* Blocks until one scheme request has been handled, then returns 1.
 * Returns 0 after cl_cef_scheme_stop. Call from a Lisp thread. */
int cl_cef_scheme_pump(void);
void cl_cef_scheme_stop(void);

/* Opaque decision filled by cl_cef_request_decision_set. */
typedef struct cl_cef_request_decision cl_cef_request_decision;

/*
 * URL, DOCUMENT_URL, and RESOURCE_TYPE are NUL-terminated. RESOURCE_TYPE is an
 * adblock type such as "script" or "document". DECISION stays valid for the
 * call. The callback runs on the thread that pumps cl_cef_request_pump, not
 * on CEF's IO thread.
 */
typedef void (*cl_cef_request_callback)(const char* url,
                                        const char* document_url,
                                        const char* resource_type,
                                        cl_cef_request_decision* decision);

void cl_cef_request_decision_set(cl_cef_request_decision* decision,
                                 int block,
                                 const char* css);

/* REPLACEMENTS is the length-prefixed body rewrite from the adblocker.
 * Empty leaves the body unchanged. Call after cl_cef_request_decision_set. */
void cl_cef_request_decision_replace(cl_cef_request_decision* decision,
                                     const char* replacements);
void cl_cef_set_request_callback(cl_cef_request_callback callback);

/* Blocks until one request has been decided, then returns 1.
 * Returns 0 after cl_cef_request_stop. Call from a Lisp thread. */
int cl_cef_request_pump(void);
void cl_cef_request_stop(void);

/* Drops stylesheets remembered for element hiding. */
void cl_cef_adblock_reset_cosmetic(void);

/*
 * ARGV is copied. SCHEMES is "name:bitmask,name:bitmask" and is published in
 * the CL_CEF_SCHEMES environment variable before CEF starts, so the subprocess
 * registers the same schemes.
 *
 * Returns 1 on success and 0 on failure. A CEF subprocess never returns.
 */
int cl_cef_initialize(int argc,
                      char** argv,
                      const char* subprocess_path,
                      const char* cache_path,
                      const char* resources_dir_path,
                      const char* locales_dir_path,
                      const char* user_agent,
                      const char* schemes,
                      int no_sandbox,
                      int remote_debugging_port);

/* Blocks on the calling thread, which becomes the CEF UI thread. */
void cl_cef_run_message_loop(cl_cef_ready_callback ready);
/* Close every browser, then quit the message loop once they are gone.
 * CefShutdown runs on the UI thread after the loop returns. Quitting the loop
 * while browsers are still open leaves the Chromium subprocesses alive. */
void cl_cef_prepare_shutdown(void);
void cl_cef_quit_message_loop(void);
void cl_cef_post(cl_cef_task_callback callback);
void cl_cef_shutdown(void);
/* Kill cl-cef-helper processes descended from this one. CefShutdown leaves
 * them behind when the Lisp process stays up, as it does from a REPL. */
void cl_cef_reap_helpers(void);
int cl_cef_on_ui_thread(void);

void cl_cef_string_free(char* string);
void cl_cef_set_download_directory(const char* path);

/* One PNG representation of the window icon. scale_factor 1 is the base
 * size; a 32px image of a 16px base is 2. Call before the first window.
 * A Chrome-style window otherwise advertises the Chromium logo. */
void cl_cef_add_app_icon_png(float scale_factor, const void* png, size_t size);

/* WM_CLASS resource name and class for windows created afterwards.
 * Empty leaves Chromium's default. Call before the first window. */
void cl_cef_set_window_class(const char* name);

int cl_cef_window_create(int width, int height, const char* title);
void cl_cef_window_close(int id);
void cl_cef_window_set_title(int id, const char* title);
char* cl_cef_window_get_title(int id);
void cl_cef_window_fullscreen(int id, int on);
void cl_cef_window_maximize(int id);
void cl_cef_window_restore(int id);
void cl_cef_window_focus(int id);
int cl_cef_window_is_focused(int id);
void cl_cef_window_bounds(int id, int* x, int* y, int* width, int* height);
void cl_cef_window_content_bounds(int id, int* x, int* y, int* width, int* height);
void cl_cef_window_set_bounds(int id, int x, int y, int width, int height);
void cl_cef_window_add_view(int window_id, int view_id);
void cl_cef_window_add_view_at(int window_id, int view_id, int index);
void cl_cef_window_remove_view(int window_id, int view_id);

int cl_cef_view_create(const char* url, int alloy, int isolated);
void cl_cef_view_destroy(int id);
void cl_cef_view_set_bar_height(int id, int height);
void cl_cef_view_set_bounds(int id, int x, int y, int width, int height);
void cl_cef_view_get_bounds(int id, int* x, int* y, int* width, int* height);
void cl_cef_view_focus(int id);
int cl_cef_view_is_focused(int id);
void cl_cef_view_load_url(int id, const char* url);
void cl_cef_view_load_html(int id, const char* html);
char* cl_cef_view_get_url(int id);
char* cl_cef_view_get_title(int id);
void cl_cef_view_reload(int id);
void cl_cef_view_stop(int id);
void cl_cef_view_go_back(int id);
void cl_cef_view_go_forward(int id);
int cl_cef_view_can_go_back(int id);
int cl_cef_view_can_go_forward(int id);
void cl_cef_view_set_zoom_factor(int id, double factor);
double cl_cef_view_get_zoom_factor(int id);
void cl_cef_view_set_audio_muted(int id, int muted);
int cl_cef_view_is_audio_muted(int id);
void cl_cef_view_show_devtools(int id);
void cl_cef_view_close_devtools(int id);
void cl_cef_view_undo(int id);
void cl_cef_view_redo(int id);
void cl_cef_view_cut(int id);
void cl_cef_view_copy(int id);
void cl_cef_view_paste(int id);
void cl_cef_view_select_all(int id);
void cl_cef_view_insert_text(int id, const char* text);

/* Fire-and-forget execution in the page. */
void cl_cef_view_execute_javascript(int id, const char* code);

/*
 * Evaluate CODE and return a malloc'd JSON encoding of the result.
 * *SUCCESS is 1 when evaluation succeeded. Must not be called on the UI thread.
 * Caller frees the string with cl_cef_string_free.
 */
char* cl_cef_view_execute_javascript_sync(int id, const char* code, int* success);

/* Returns a malloc'd identifier. CSS is reapplied after each navigation. */
char* cl_cef_view_insert_css(int id, const char* css);
void cl_cef_view_remove_inserted_css(int id, const char* key);

/* DOCUMENT_START and ALL_FRAMES are booleans. Returns a script id, or 0. */
int cl_cef_view_add_user_script(int id, const char* code, int document_start, int all_frames);
void cl_cef_view_remove_user_script(int id, int script_id);

void cl_cef_view_download_url(int id, const char* url);
void cl_cef_view_set_javascript_enabled(int id, int enabled);
int cl_cef_view_javascript_enabled(int id);
void cl_cef_view_set_images_enabled(int id, int enabled);
int cl_cef_view_images_enabled(int id);
void cl_cef_view_set_webgl_enabled(int id, int enabled);
int cl_cef_view_webgl_enabled(int id);
void cl_cef_view_set_user_agent(int id, const char* user_agent);
char* cl_cef_view_get_user_agent(int id);
void cl_cef_view_set_proxy(int id, const char* proxy_server, const char* bypass_list);
char* cl_cef_view_get_proxy(int id);
/* 0 accept, 1 never, 2 block third-party cookies. */
void cl_cef_view_set_cookie_policy(int id, int policy);
int cl_cef_view_cookie_policy(int id);

void cl_cef_download_cancel(unsigned int download_id);

/* Valid only while a context-menu event callback is on the stack. */
void cl_cef_menu_add_separator(void);
void cl_cef_menu_add_item(int command_id, const char* label);

#ifdef __cplusplus
}
#endif

#endif /* CL_CEF_H */
