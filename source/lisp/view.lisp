;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; A browser view embedded in a window. Page operations live on `web-contents'
;;;; and are also available here, as they are in cl-electron.

(in-package :cef)

(define-class view (remote-object)
  ((web-contents
    nil
    :export t
    :accessor t
    :documentation "The `web-contents' bound to this view.")
   (options
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "Accepted for cl-electron compatibility and ignored.
CEF views are configured through the bridge rather than a JavaScript options
string.")
   (alloy-p
    nil
    :export t
    :reader t
    :documentation "Use Alloy style. A Chrome window can host one Chrome view
and any number of Alloy views, which is how the status and prompt panels share
the window with the page.")
   (isolated-context-p
    nil
    :type boolean
    :export t
    :reader t
    :documentation "Use a request context that does not share site zoom.
The profile stores a zoom for about:blank. Panel documents are about:blank, so
they would render at that zoom."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Embed a web page in a `window'."))

(defun view-url-string (url)
  (if (stringp url)
      (if (uiop:emptyp url) "about:blank" url)
      "about:blank"))

(defun install-native-view (view url)
  (let ((id (cl-cef-view-create (view-url-string url)
                                (if (alloy-p view) 1 0)
                                (if (isolated-context-p view) 1 0))))
    (unless (plusp id)
      (error "CEF failed to create a view."))
    (setf (slot-value view 'id) id)
    (setf (web-contents view)
          (make-instance 'web-contents :id id :interface (interface view)))
    ;; The view is what Nyxt specializes on, so it wins the id lookup.
    (register-object (web-contents view))
    (register-object view)
    view))

(defmethod initialize-instance :after ((view view) &key (url "about:blank"))
  (install-native-view view url))

(export-always 'ensure-view)
(defmethod ensure-view ((view view) &key (url "about:blank"))
  "Create the native view when `kill' has cleared it."
  (unless (web-contents view)
    (install-native-view view url))
  view)

(export-always 'kill)
(defmethod kill ((view view))
  (cl-cef-view-destroy (id view))
  (forget-object view)
  (when (web-contents view)
    (forget-object (web-contents view))
    (setf (web-contents view) nil)))

(export-always 'set-bar-height)
(defmethod set-bar-height ((view view) height)
  "Keep VIEW at HEIGHT pixels. The page view stays flexible; bars do not.
The status document sizes itself with vh, so the view has to be the bar, not
the window."
  (cl-cef-view-set-bar-height (id view) (or height 0))
  height)

(export-always 'set-bounds)
(defmethod set-bounds ((view view) &key x y width height)
  (let ((bounds (get-bounds view)))
    ;; 0 is a real coordinate. `or' would treat it as missing.
    (flet ((pick (value fallback)
             (if (null value) fallback value)))
      (cl-cef-view-set-bounds (id view)
                              (pick x (assoc-value bounds :x))
                              (pick y (assoc-value bounds :y))
                              (pick width (assoc-value bounds :width))
                              (pick height (assoc-value bounds :height))))))

(export-always 'get-bounds)
(defmethod get-bounds ((view view))
  (bounds-alist #'cl-cef-view-get-bounds (id view)))

(export-always 'focus)
(defmethod focus ((view view))
  (cl-cef-view-focus (id view)))

(export-always 'is-focused)
(defmethod is-focused ((view view))
  (plusp (cl-cef-view-is-focused (id view))))

(export-always 'load-url)
(defmethod load-url ((view view) url)
  (cl-cef-view-load-url (id view) (if (stringp url) url (princ-to-string url))))

(export-always 'load-html)
(defmethod load-html ((view view) html)
  (cl-cef-view-load-html (id view) html))

(export-always 'get-url)
(defmethod get-url ((view view))
  (or (take-string (cl-cef-view-get-url (id view))) ""))

(export-always 'get-title)
(defmethod get-title ((view view))
  (or (take-string (cl-cef-view-get-title (id view))) ""))

(export-always 'reload)
(defmethod reload ((view view))
  (cl-cef-view-reload (id view)))

(export-always 'open-dev-tools)
(defmethod open-dev-tools ((view view) &key options)
  (declare (ignore options))
  (cl-cef-view-show-devtools (id view)))

(export-always 'close-dev-tools)
(defmethod close-dev-tools ((view view))
  (cl-cef-view-close-devtools (id view)))
