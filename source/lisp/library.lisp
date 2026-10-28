;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; CFFI bindings for the C bridge in source/cxx/cl-cef.h.
;;;; The shared library is loaded on `launch', not at system load time, so the
;;;; Lisp system compiles without a CEF binary distribution.

(in-package :cef)

;; This CFFI accepts only strings for :string. Optional C strings in the
;; bridge are NULL, which the rest of cl-cef represents as NIL.
(defmethod cffi:translate-to-foreign ((object null) (type cffi::foreign-string-type))
  (values (cffi:null-pointer) nil))

(cffi:define-foreign-library libcl-cef
  (:darwin "libcl-cef.dylib")
  (:unix "libcl-cef.so")
  (t (:default "libcl-cef")))

(defvar *library-loaded-p* nil
  "Non-nil once libcl-cef has been loaded into this image.")

(defun library-namestring (&optional path)
  "Return the shared library PATH, or the build output next to this system."
  (or path
      (uiop:getenv "CL_CEF_LIBRARY")
      (let ((built (asdf:system-relative-pathname :cl-cef "build/libcl-cef.so")))
        (when (uiop:file-exists-p built)
          (uiop:native-namestring built)))
      "libcl-cef.so"))

(defun ensure-library (&optional path)
  "Load libcl-cef. Signal an error that says how to build it when it is missing."
  (unless *library-loaded-p*
    (let ((namestring (library-namestring path)))
      (handler-case (cffi:load-foreign-library namestring)
        (error (condition)
          (error "Could not load cl-cef from ~a.~%Build cl-cef against a CEF binary distribution (see CMakeLists.txt).~%~a"
                 namestring condition))))
    (setf *library-loaded-p* t))
  *library-loaded-p*)

(defmacro defcef (name return-type &body arguments)
  "Define a foreign function exported by libcl-cef."
  `(cffi:defcfun ,(string-downcase name) ,return-type
     ,@arguments))

(defcef "cl_cef_response_set" :void
  (response :pointer)
  (data :pointer)
  (size :unsigned-long)
  (mime-type :string)
  (status :int)
  (status-text :string)
  (headers :string))

(defcef "cl_cef_set_event_callback" :void
  (callback :pointer))

(defcef "cl_cef_set_scheme_callback" :void
  (callback :pointer))

(defcef "cl_cef_scheme_pump" :int)

(defcef "cl_cef_scheme_stop" :void)

(defcef "cl_cef_request_decision_set" :void
  (decision :pointer)
  (block :int)
  (css :string))

(defcef "cl_cef_request_decision_replace" :void
  (decision :pointer)
  (replacements :string))

(defcef "cl_cef_set_request_callback" :void
  (callback :pointer))

(defcef "cl_cef_request_pump" :int)

(defcef "cl_cef_request_stop" :void)

(defcef "cl_cef_adblock_reset_cosmetic" :void)

(defcef "cl_cef_initialize" :int
  (argc :int)
  (argv :pointer)
  (subprocess-path :string)
  (cache-path :string)
  (resources-dir-path :string)
  (locales-dir-path :string)
  (user-agent :string)
  (schemes :string)
  (no-sandbox :int)
  (remote-debugging-port :int))

(defcef "cl_cef_run_message_loop" :void
  (ready :pointer))

(defcef "cl_cef_prepare_shutdown" :void)

(defcef "cl_cef_quit_message_loop" :void)

(defcef "cl_cef_post" :void
  (callback :pointer))

(defcef "cl_cef_shutdown" :void)

(defcef "cl_cef_reap_helpers" :void)

(defcef "cl_cef_on_ui_thread" :int)

(defcef "cl_cef_string_free" :void
  (string :pointer))

(defcef "cl_cef_set_download_directory" :void
  (path :string))

(defcef "cl_cef_add_app_icon_png" :void
  (scale :float)
  (png :pointer)
  (size :unsigned-long))

(defcef "cl_cef_set_window_class" :void
  (name :string))

(defcef "cl_cef_window_create" :int
  (width :int)
  (height :int)
  (title :string))

(defcef "cl_cef_window_close" :void
  (id :int))

(defcef "cl_cef_window_set_title" :void
  (id :int)
  (title :string))

(defcef "cl_cef_window_get_title" :pointer
  (id :int))

(defcef "cl_cef_window_fullscreen" :void
  (id :int)
  (on :int))

(defcef "cl_cef_window_maximize" :void
  (id :int))

(defcef "cl_cef_window_restore" :void
  (id :int))

(defcef "cl_cef_window_focus" :void
  (id :int))

(defcef "cl_cef_window_is_focused" :int
  (id :int))

(defcef "cl_cef_window_bounds" :void
  (id :int)
  (x :pointer)
  (y :pointer)
  (width :pointer)
  (height :pointer))

(defcef "cl_cef_window_content_bounds" :void
  (id :int)
  (x :pointer)
  (y :pointer)
  (width :pointer)
  (height :pointer))

(defcef "cl_cef_window_set_bounds" :void
  (id :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcef "cl_cef_window_add_view" :void
  (window-id :int)
  (view-id :int))

(defcef "cl_cef_window_add_view_at" :void
  (window-id :int)
  (view-id :int)
  (index :int))

(defcef "cl_cef_window_remove_view" :void
  (window-id :int)
  (view-id :int))

(defcef "cl_cef_view_create" :int
  (url :string)
  (alloy :int)
  (isolated :int))

(defcef "cl_cef_view_destroy" :void
  (id :int))

(defcef "cl_cef_view_set_bar_height" :void
  (id :int)
  (height :int))

(defcef "cl_cef_view_set_bounds" :void
  (id :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcef "cl_cef_view_get_bounds" :void
  (id :int)
  (x :pointer)
  (y :pointer)
  (width :pointer)
  (height :pointer))

(defcef "cl_cef_view_focus" :void
  (id :int))

(defcef "cl_cef_view_is_focused" :int
  (id :int))

(defcef "cl_cef_view_load_url" :void
  (id :int)
  (url :string))

(defcef "cl_cef_view_load_html" :void
  (id :int)
  (html :string))

(defcef "cl_cef_view_get_url" :pointer
  (id :int))

(defcef "cl_cef_view_get_title" :pointer
  (id :int))

(defcef "cl_cef_view_reload" :void
  (id :int))

(defcef "cl_cef_view_stop" :void
  (id :int))

(defcef "cl_cef_view_go_back" :void
  (id :int))

(defcef "cl_cef_view_go_forward" :void
  (id :int))

(defcef "cl_cef_view_can_go_back" :int
  (id :int))

(defcef "cl_cef_view_can_go_forward" :int
  (id :int))

(defcef "cl_cef_view_set_zoom_factor" :void
  (id :int)
  (factor :double))

(defcef "cl_cef_view_get_zoom_factor" :double
  (id :int))

(defcef "cl_cef_view_set_audio_muted" :void
  (id :int)
  (muted :int))

(defcef "cl_cef_view_is_audio_muted" :int
  (id :int))

(defcef "cl_cef_view_show_devtools" :void
  (id :int))

(defcef "cl_cef_view_close_devtools" :void
  (id :int))

(defcef "cl_cef_view_undo" :void
  (id :int))

(defcef "cl_cef_view_redo" :void
  (id :int))

(defcef "cl_cef_view_cut" :void
  (id :int))

(defcef "cl_cef_view_copy" :void
  (id :int))

(defcef "cl_cef_view_paste" :void
  (id :int))

(defcef "cl_cef_view_select_all" :void
  (id :int))

(defcef "cl_cef_view_insert_text" :void
  (id :int)
  (text :string))

(defcef "cl_cef_view_execute_javascript" :void
  (id :int)
  (code :string))

(defcef "cl_cef_view_execute_javascript_sync" :pointer
  (id :int)
  (code :string)
  (success :pointer))

(defcef "cl_cef_view_insert_css" :pointer
  (id :int)
  (css :string))

(defcef "cl_cef_view_remove_inserted_css" :void
  (id :int)
  (key :string))

(defcef "cl_cef_view_add_user_script" :int
  (id :int)
  (code :string)
  (document-start :int)
  (all-frames :int))

(defcef "cl_cef_view_remove_user_script" :void
  (id :int)
  (script-id :int))

(defcef "cl_cef_view_download_url" :void
  (id :int)
  (url :string))

(defcef "cl_cef_view_set_javascript_enabled" :void
  (id :int)
  (enabled :int))

(defcef "cl_cef_view_javascript_enabled" :int
  (id :int))

(defcef "cl_cef_view_set_images_enabled" :void
  (id :int)
  (enabled :int))

(defcef "cl_cef_view_images_enabled" :int
  (id :int))

(defcef "cl_cef_view_set_webgl_enabled" :void
  (id :int)
  (enabled :int))

(defcef "cl_cef_view_webgl_enabled" :int
  (id :int))

(defcef "cl_cef_view_set_user_agent" :void
  (id :int)
  (user-agent :string))

(defcef "cl_cef_view_get_user_agent" :pointer
  (id :int))

(defcef "cl_cef_view_set_proxy" :void
  (id :int)
  (proxy-server :string)
  (bypass-list :string))

(defcef "cl_cef_view_get_proxy" :pointer
  (id :int))

(defcef "cl_cef_view_set_cookie_policy" :void
  (id :int)
  (policy :int))

(defcef "cl_cef_view_cookie_policy" :int
  (id :int))

(defcef "cl_cef_download_cancel" :void
  (download-id :unsigned-int))

(defcef "cl_cef_menu_add_separator" :void)

(defcef "cl_cef_menu_add_item" :void
  (command-id :int)
  (label :string))

(defun take-string (pointer)
  "Copy a bridge-owned C string and free it. NIL when POINTER is NULL."
  (when (and pointer (not (cffi:null-pointer-p pointer)))
    (unwind-protect (cffi:foreign-string-to-lisp pointer)
      (cl-cef-string-free pointer))))

(defun bounds-alist (caller id)
  "Call CALLER, which writes x, y, width and height through pointers."
  (cffi:with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (funcall caller id x y width height)
    `((:x . ,(cffi:mem-ref x :int))
      (:y . ,(cffi:mem-ref y :int))
      (:width . ,(cffi:mem-ref width :int))
      (:height . ,(cffi:mem-ref height :int)))))

(defun flag (value)
  "Encode a generalized boolean as a C int."
  (if value 1 0))
