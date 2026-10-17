;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Page control. A `web-contents' shares its id with the `view' that owns it.

(in-package :cef)

(define-class web-contents (remote-object)
  ()
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Rendering and control of one web page."))

(defmethod initialize-instance :after ((web-contents web-contents) &key id interface)
  (when id
    (setf (slot-value web-contents 'id) id))
  (when interface
    (setf (slot-value web-contents 'interface) interface)))

(export-always 'load-url)
(defmethod load-url ((web-contents web-contents) url)
  (cl-cef-view-load-url (id web-contents) (if (stringp url) url (princ-to-string url))))

(export-always 'get-url)
(defmethod get-url ((web-contents web-contents))
  (or (take-string (cl-cef-view-get-url (id web-contents))) ""))

(export-always 'get-title)
(defmethod get-title ((web-contents web-contents))
  (or (take-string (cl-cef-view-get-title (id web-contents))) ""))

(export-always 'reload)
(defmethod reload ((web-contents web-contents))
  (cl-cef-view-reload (id web-contents)))

(export-always 'stop)
(defmethod stop ((web-contents web-contents))
  (cl-cef-view-stop (id web-contents)))

(export-always 'go-back)
(defmethod go-back ((web-contents web-contents))
  (cl-cef-view-go-back (id web-contents)))

(export-always 'go-forward)
(defmethod go-forward ((web-contents web-contents))
  (cl-cef-view-go-forward (id web-contents)))

(export-always 'can-go-back-p)
(defmethod can-go-back-p ((web-contents web-contents))
  (plusp (cl-cef-view-can-go-back (id web-contents))))

(export-always 'can-go-forward-p)
(defmethod can-go-forward-p ((web-contents web-contents))
  (plusp (cl-cef-view-can-go-forward (id web-contents))))

(export-always 'set-zoom-factor)
(defmethod set-zoom-factor ((web-contents web-contents) factor)
  (cl-cef-view-set-zoom-factor (id web-contents) (coerce factor 'double-float)))

(export-always 'get-zoom-factor)
(defmethod get-zoom-factor ((web-contents web-contents))
  (cl-cef-view-get-zoom-factor (id web-contents)))

(export-always 'set-audio-muted)
(defmethod set-audio-muted ((web-contents web-contents) muted)
  (cl-cef-view-set-audio-muted (id web-contents) (flag muted)))

(export-always 'muted-p)
(defmethod muted-p ((web-contents web-contents))
  (plusp (cl-cef-view-is-audio-muted (id web-contents))))

(export-always 'focus)
(defmethod focus ((web-contents web-contents))
  (cl-cef-view-focus (id web-contents)))

(export-always 'is-focused)
(defmethod is-focused ((web-contents web-contents))
  (plusp (cl-cef-view-is-focused (id web-contents))))

(export-always 'undo)
(defmethod undo ((web-contents web-contents))
  (cl-cef-view-undo (id web-contents)))

(export-always 'redo)
(defmethod redo ((web-contents web-contents))
  (cl-cef-view-redo (id web-contents)))

(export-always 'cut)
(defmethod cut ((web-contents web-contents))
  (cl-cef-view-cut (id web-contents)))

(export-always 'copy)
(defmethod copy ((web-contents web-contents))
  (cl-cef-view-copy (id web-contents)))

(export-always 'paste)
(defmethod paste ((web-contents web-contents))
  (cl-cef-view-paste (id web-contents)))

(export-always 'select-all)
(defmethod select-all ((web-contents web-contents))
  (cl-cef-view-select-all (id web-contents)))

(export-always 'insert-text)
(defmethod insert-text ((web-contents web-contents) text)
  (cl-cef-view-insert-text (id web-contents) text))

(export-always 'execute-javascript)
(defmethod execute-javascript ((web-contents web-contents) code &key user-gesture)
  "Evaluate CODE without waiting for the result. USER-GESTURE is ignored."
  (declare (ignore user-gesture))
  (cl-cef-view-execute-javascript (id web-contents) code))

(export-always 'execute-javascript-synchronous)
(defmethod execute-javascript-synchronous ((web-contents web-contents) code &key user-gesture)
  "Evaluate CODE and return the JSON value decoded into Lisp.
Must be called off the CEF UI thread. USER-GESTURE is ignored."
  (declare (ignore user-gesture))
  (cffi:with-foreign-object (success :int)
    (let ((pointer (cl-cef-view-execute-javascript-sync (id web-contents) code success)))
      (unless (plusp (cffi:mem-ref success :int))
        (warn "CEF failed to evaluate JavaScript:~%~a" code)
        (return-from execute-javascript-synchronous nil))
      (let ((json (take-string pointer)))
        (when json
          (cl-json:decode-json-from-string json))))))

(export-always 'insert-css)
(defmethod insert-css ((web-contents web-contents) css)
  "Insert CSS and reapply it after each navigation. Return an identifier."
  (take-string (cl-cef-view-insert-css (id web-contents) css)))

(export-always 'remove-inserted-css)
(defmethod remove-inserted-css ((web-contents web-contents) key)
  (cl-cef-view-remove-inserted-css (id web-contents) key))

(export-always 'add-user-script)
(defmethod add-user-script ((web-contents web-contents) code
                            &key (run-at :document-end) (all-frames-p t))
  "Install CODE to run on each load. Return a script id.
RUN-AT is `:document-start' or anything else for document end. Scripts run in
the page world; CEF isolated worlds are not exposed here."
  (cl-cef-view-add-user-script (id web-contents)
                               code
                               (flag (eq run-at :document-start))
                               (flag all-frames-p)))

(export-always 'remove-user-script)
(defmethod remove-user-script ((web-contents web-contents) script-id)
  (cl-cef-view-remove-user-script (id web-contents) script-id))

(export-always 'download-url)
(defmethod download-url ((web-contents web-contents) url)
  (cl-cef-view-download-url (id web-contents) (if (stringp url) url (princ-to-string url))))

(export-always 'open-dev-tools)
(defmethod open-dev-tools ((web-contents web-contents) &key options)
  (declare (ignore options))
  (cl-cef-view-show-devtools (id web-contents)))

(export-always 'close-dev-tools)
(defmethod close-dev-tools ((web-contents web-contents))
  (cl-cef-view-close-devtools (id web-contents)))

(export-always 'set-javascript-enabled)
(defmethod set-javascript-enabled ((web-contents web-contents) enabled)
  (cl-cef-view-set-javascript-enabled (id web-contents) (flag enabled)))

(export-always 'javascript-enabled-p)
(defmethod javascript-enabled-p ((web-contents web-contents))
  (plusp (cl-cef-view-javascript-enabled (id web-contents))))

(export-always 'set-images-enabled)
(defmethod set-images-enabled ((web-contents web-contents) enabled)
  (cl-cef-view-set-images-enabled (id web-contents) (flag enabled)))

(export-always 'images-enabled-p)
(defmethod images-enabled-p ((web-contents web-contents))
  (plusp (cl-cef-view-images-enabled (id web-contents))))

(export-always 'set-webgl-enabled)
(defmethod set-webgl-enabled ((web-contents web-contents) enabled)
  (cl-cef-view-set-webgl-enabled (id web-contents) (flag enabled)))

(export-always 'webgl-enabled-p)
(defmethod webgl-enabled-p ((web-contents web-contents))
  (plusp (cl-cef-view-webgl-enabled (id web-contents))))

(export-always 'set-user-agent)
(defmethod set-user-agent ((web-contents web-contents) user-agent)
  (cl-cef-view-set-user-agent (id web-contents) user-agent))

(export-always 'get-user-agent)
(defmethod get-user-agent ((web-contents web-contents))
  (or (take-string (cl-cef-view-get-user-agent (id web-contents))) ""))

(export-always 'set-proxy)
(defmethod set-proxy ((web-contents web-contents) proxy-server &optional bypass-list)
  "Send traffic through PROXY-SERVER, a string such as \"http://localhost:8080\".
An empty server selects a direct connection. BYPASS-LIST is a comma-separated
host list."
  (cl-cef-view-set-proxy (id web-contents)
                         (or proxy-server "")
                         (or bypass-list "")))

(export-always 'get-proxy)
(defmethod get-proxy ((web-contents web-contents))
  (or (take-string (cl-cef-view-get-proxy (id web-contents))) ""))

(export-always 'set-cookie-policy)
(defmethod set-cookie-policy ((web-contents web-contents) policy)
  "Set POLICY to `:accept', `:never', or `:no-third-party'."
  (cl-cef-view-set-cookie-policy (id web-contents)
                                 (case policy
                                   (:never 1)
                                   (:no-third-party 2)
                                   (t 0))))

(export-always 'cookie-policy)
(defmethod cookie-policy ((web-contents web-contents))
  (case (cl-cef-view-cookie-policy (id web-contents))
    (1 :never)
    (2 :no-third-party)
    (t :accept)))

(export-always 'override-window-open-handler)
(defmethod override-window-open-handler ((web-contents web-contents) callback)
  "Call CALLBACK with the popup's URL alist and cancel the CEF popup.
CALLBACK receives the web-contents and an alist with `:url'."
  (add-listener web-contents :window-open
                (lambda (object data)
                  (funcall callback object data)
                  t)))

(export-always 'kill)
(defmethod kill ((web-contents web-contents))
  (cl-cef-view-destroy (id web-contents))
  (forget-object web-contents))
