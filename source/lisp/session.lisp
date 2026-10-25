;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Downloads are reported on the default session, the same way cl-electron
;;;; reports them on session.defaultSession.

(in-package :cef)

(define-class download-item ()
  ((id
    0
    :export t
    :reader t
    :writer nil
    :type integer)
   (url
    ""
    :export t
    :accessor t
    :type string)
   (state
    ""
    :export t
    :accessor t
    :type string)
   (received-bytes
    0
    :export t
    :accessor t
    :type integer)
   (total-bytes
    0
    :export t
    :accessor t
    :type integer)
   (percent-complete
    0
    :export t
    :accessor t
    :type integer)
   (save-path
    ""
    :export t
    :accessor t
    :type string))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "One file being downloaded."))

(define-class session (remote-object)
  ((download-items
    (make-hash-table)
    :documentation "Map from download id to `download-item'."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Browser-wide session. Downloads are delivered here."))

(defmethod initialize-instance :after ((session session) &key)
  (setf (slot-value session 'native-id) +session-object-id+))

(export-always 'default-session)
(defmethod default-session ((interface interface))
  (or (session interface)
      (setf (session interface) (make-instance 'session :interface interface))))

(defmethod add-listener ((session session) (event (eql :download-item-updated))
                         (callback function) &key once-p)
  "CALLBACK is called as (callback session download-item) on each update."
  (add-listener (make-instance 'remote-object :id +session-object-id+ :interface (interface session))
                event
                (lambda (object data)
                  (declare (ignore object))
                  (funcall callback session (update-download session data)))
                :once-p once-p))

(defun update-download (session data)
  (let* ((id (assoc-value data :id))
         (item (or (gethash id (download-items session))
                   (setf (gethash id (download-items session))
                         (make-instance 'download-item :id id)))))
    (setf (slot-value item 'id) id
          (url item) (or (assoc-value data :url) "")
          (state item) (or (assoc-value data :state) "")
          (received-bytes item) (or (assoc-value data :received-bytes) 0)
          (total-bytes item) (or (assoc-value data :total-bytes) 0)
          (percent-complete item) (or (assoc-value data :percent-complete) 0)
          (save-path item) (or (assoc-value data :save-path) ""))
    item))

(export-always 'cancel)
(defmethod cancel ((download-item download-item))
  (cl-cef-download-cancel (id download-item))
  (setf (state download-item) "cancelled"))

(export-always 'menu-add-item)
(defun menu-add-item (command-id label)
  "Append a context-menu item. Only has an effect inside a `:context-menu' listener."
  (cl-cef-menu-add-item command-id label))

(export-always 'menu-add-separator)
(defun menu-add-separator ()
  "Append a context-menu separator. Only has an effect inside a `:context-menu' listener."
  (cl-cef-menu-add-separator))

(export-always 'windows-key-code-to-code)
(defun windows-key-code-to-code (code)
  "Return a DOM `KeyboardEvent.code' string for a Windows virtual key CODE.
Return NIL for a code this mapping does not recognize."
  (cond
    ((not (integerp code)) nil)
    ((<= 65 code 90) (format nil "Key~C" (code-char code)))
    ((<= 48 code 57) (format nil "Digit~C" (code-char code)))
    ((<= #x70 code #x7B) (format nil "F~D" (1+ (- code #x70))))
    ((<= #x60 code #x69) (format nil "keypad~D" (- code #x60)))
    (t (case code
         (#x08 "Backspace")
         (#x09 "Tab")
         (#x0D "Enter")
         (#x1B "Escape")
         (#x20 "Space")
         (#x21 "PageUp")
         (#x22 "PageDown")
         (#x23 "End")
         (#x24 "Home")
         (#x25 "ArrowLeft")
         (#x26 "ArrowUp")
         (#x27 "ArrowRight")
         (#x28 "ArrowDown")
         (#x2E "Delete")
         (#xBA "Semicolon")
         (#xBB "Equal")
         (#xBC "Comma")
         (#xBD "Minus")
         (#xBE "Period")
         (#xBF "Slash")
         (#xC0 "Backquote")
         (#xDB "BracketLeft")
         (#xDC "Backslash")
         (#xDD "BracketRight")
         (#xDE "Quote")
         (t nil)))))
