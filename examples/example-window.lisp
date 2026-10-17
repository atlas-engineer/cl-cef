;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Open one window. Requires libcl-cef and cl-cef-helper from the CMake build.
;;;;
;;;;   (asdf:load-system :cl-cef/examples)
;;;;   (cef::example-window)

(in-package :cef)

(defun example-window ()
  (setf *interface* (make-instance 'interface))
  ;; CEF's Wayland backend faults when Vulkan is the GL implementation.
  (when (uiop:getenv "WAYLAND_DISPLAY")
    (setf (launch-options *interface*)
          (append (launch-options *interface*) '("--ozone-platform=x11"))))
  (launch *interface*)
  (let* ((window (make-instance 'window :title "cl-cef"))
         (view (make-instance 'view :url "https://nyxt-browser.com")))
    (add-bounded-view window view
                      :window-bounds-alist-var bounds
                      :x 0
                      :y 0
                      :width (assoc-value bounds :width)
                      :height (assoc-value bounds :height)))
  (join))
