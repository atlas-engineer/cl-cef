;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Top-level CEF Views window.

(in-package :cef)

(define-class window (remote-object)
  ((title
    "Nyxt"
    :export t
    :accessor t
    :type string)
   (width
    1600
    :export t
    :accessor t
    :type integer)
   (height
    1200
    :export t
    :accessor t
    :type integer)
   (views
    nil
    :export t
    :accessor t
    :documentation "Views currently added to this window."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A top-level native window that hosts `view's."))

(defmethod initialize-instance :after ((window window) &key)
  (let ((id (cl-cef-window-create (width window) (height window) (title window))))
    (unless (plusp id)
      (error "CEF failed to create a window."))
    (setf (slot-value window 'native-id) id)
    (register-object window)))

(export-always 'kill)
(defmethod kill ((window window))
  (cl-cef-window-close (id window))
  (forget-object window)
  (setf (views window) nil))

(export-always 'get-title)
(defmethod get-title ((window window))
  (or (take-string (cl-cef-window-get-title (id window))) ""))

(export-always 'set-title)
(defmethod set-title ((window window) title)
  (setf (title window) title)
  (cl-cef-window-set-title (id window) title))

(export-always 'fullscreen)
(defmethod fullscreen ((window window))
  (cl-cef-window-fullscreen (id window) 1))

(export-always 'unfullscreen)
(defmethod unfullscreen ((window window))
  (cl-cef-window-fullscreen (id window) 0))

(export-always 'maximize)
(defmethod maximize ((window window))
  (cl-cef-window-maximize (id window)))

(export-always 'unmaximize)
(defmethod unmaximize ((window window))
  (cl-cef-window-restore (id window)))

(export-always 'focus)
(defmethod focus ((window window))
  (cl-cef-window-focus (id window)))

(export-always 'is-focused)
(defmethod is-focused ((window window))
  (plusp (cl-cef-window-is-focused (id window))))

(export-always 'get-bounds)
(defmethod get-bounds ((window window))
  (bounds-alist #'cl-cef-window-bounds (id window)))

(export-always 'get-content-bounds)
(defmethod get-content-bounds ((window window))
  "Client area size. x and y are 0; views are positioned inside that area."
  (bounds-alist #'cl-cef-window-content-bounds (id window)))

(export-always 'set-bounds)
(defmethod set-bounds ((window window) &key x y width height)
  (let ((bounds (get-bounds window)))
    (cl-cef-window-set-bounds (id window)
                              (or x (assoc-value bounds :x))
                              (or y (assoc-value bounds :y))
                              (or width (assoc-value bounds :width))
                              (or height (assoc-value bounds :height)))))

(export-always 'remove-menu)
(defmethod remove-menu ((window window))
  "CEF Views windows have no application menu. This is a no-op kept for callers
ported from cl-electron."
  (declare (ignore window))
  nil)

(defun remember-view (window view)
  (setf (views window) (append (remove view (views window)) (list view)))
  view)

(export-always 'add-view)
(defmethod add-view ((window window) (view view) &key z-index)
  "Add VIEW to WINDOW. Z-INDEX is accepted for cl-electron compatibility.
Re-adding a view moves it to the top."
  (declare (ignore z-index))
  (cl-cef-window-add-view (id window) (id view))
  (remember-view window view))

(export-always 'add-view-at)
(defmethod add-view-at ((window window) (view view) index)
  "Insert VIEW at INDEX in WINDOW's vertical stack. 0 is the top."
  (cl-cef-window-add-view-at (id window) (id view) index)
  (remember-view window view))

(export-always 'remove-view)
(defmethod remove-view ((window window) (view view) &key (kill-view-p t))
  (cl-cef-window-remove-view (id window) (id view))
  (setf (views window) (remove view (views window)))
  (when kill-view-p
    (kill view)))

(export-always 'add-bounded-view)
(defmacro add-bounded-view (window view &key z-index window-bounds-alist-var x y width height)
  "Place VIEW inside WINDOW at X, Y, WIDTH, HEIGHT and keep it there on resize.

WINDOW-BOUNDS-ALIST-VAR is bound to the window content bounds while computing
the coordinates, matching cl-electron."
  `(progn
     (unless (find ,view (views ,window))
       (add-listener ,window :resize
                     (lambda (win data)
                       (declare (ignore data))
                       ;; The event is delivered on the UI thread, inside CEF's
                       ;; bounds notification. Applying the new bounds there
                       ;; re-enters that notification.
                       (post-ui
                        (lambda ()
                          (let ((,window-bounds-alist-var (get-content-bounds win)))
                            (set-bounds ,view :x ,x :y ,y :width ,width :height ,height)))))))
     ;; Parent the view before moving it. Setting bounds on a view that is not
     ;; in a window yet leaves CEF unable to lay it out once it is added.
     (add-view ,window ,view :z-index ,z-index)
     (let ((,window-bounds-alist-var (get-content-bounds ,window)))
       (set-bounds ,view :x ,x :y ,y :width ,width :height ,height))))
