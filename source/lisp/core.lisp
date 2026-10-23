;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Process lifetime, object registry, and event dispatch.
;;;; The shape follows cl-electron: an `interface' owns the runtime, and
;;;; windows, views, and web-contents are remote objects addressed by id.

(in-package :cef)

(defvar *objects* (make-hash-table)
  "Map from bridge object id to the Lisp object that owns it.")

(defvar *listeners* (make-hash-table :test 'equal)
  "Map from (id . event) to a list of (callback object once-p) entries.")

(defvar *launch-ready* nil
  "Semaphore signaled once the CEF UI thread is inside its message loop.")

(defvar *launch-error* nil
  "Condition signaled by the UI thread when CEF fails to start.")

(defconstant +session-object-id+ -1
  "Object id used for browser-wide download events.")

(export-always '*interface*)
(defvar *interface* nil)

(define-class interface ()
  ((library-path
    nil
    :export t
    :reader t
    :writer nil
    :type (or null string pathname)
    :documentation "Path to libcl-cef. NIL uses the build output or CL_CEF_LIBRARY.")
   (subprocess-path
    nil
    :export t
    :accessor t
    :type (or null string pathname)
    :documentation "Path to the cl-cef-helper executable. NIL uses the build output.")
   (cache-path
    nil
    :export t
    :accessor t
    :type (or null string pathname)
    :documentation "CEF root cache directory. NIL uses the XDG cache home.")
   (resources-dir-path
    nil
    :export t
    :accessor t
    :type (or null string pathname)
    :documentation "Directory containing CEF .pak resources and icudtl.dat.")
   (locales-dir-path
    nil
    :export t
    :accessor t
    :type (or null string pathname)
    :documentation "Directory containing CEF locale packs.")
   (user-agent
    nil
    :export t
    :accessor t
    :type (or null string)
    :documentation "Default user agent. NIL keeps CEF's own default.")
   (download-directory
    nil
    :export t
    :accessor t
    :type (or null string pathname)
    :documentation "Directory downloads are saved into.")
   (no-sandbox-p
    t
    :export t
    :accessor t
    :type boolean
    :documentation "When non-nil, start CEF with the sandbox disabled.
The sandbox needs extra setup (chrome-sandbox) that this bridge does not perform.")
   (remote-debugging-port
    0
    :export t
    :accessor t
    :type integer
    :documentation "DevTools port. 0 disables remote debugging.")
   (launch-options
    '()
    :export t
    :accessor t
    :type list
    :documentation "Extra command-line switches appended to the process argv.")
   (protocols
    nil
    :initarg nil
    :export t
    :reader t
    :writer nil
    :documentation "Custom schemes. Set them before `launch'.")
   (session
    nil
    :export t
    :accessor t
    :documentation "The default `session', created on first use.")
   (ui-thread
    nil
    :reader t
    :writer nil
    :documentation "Thread running the CEF message loop.")
   (argv-pointers
    nil
    :documentation "Foreign argv kept alive for the CEF process."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A running CEF browser process."))

(define-class remote-object ()
  ((id
    0
    :export t
    :reader t
    :writer nil
    :type integer
    :documentation "Bridge object id. 0 means the native object was not created.")
   (interface
    *interface*
    :export t
    :reader t
    :writer nil
    :type (or interface null)
    :documentation "The `interface' this object talks to."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A CEF object addressed by an integer id."))

(defmethod initialize-instance :after ((object remote-object) &key id interface)
  ;; `:writer nil' still leaves the initarg, but set the slots explicitly so a
  ;; caller can force an id such as the session object id.
  (when id
    (setf (slot-value object 'id) id))
  (when interface
    (setf (slot-value object 'interface) interface)))

(defun register-object (object)
  (setf (gethash (id object) *objects*) object))

(defun forget-object (object)
  (remhash (id object) *objects*)
  (let ((stale '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (eql (car key) (id object))
                 (push key stale)))
             *listeners*)
    (dolist (key stale)
      (remhash key *listeners*))))

(defun find-object (id)
  (if (= id +session-object-id+)
      (and *interface* (session *interface*))
      (gethash id *objects*)))

(export-always 'on-ui-thread-p)
(defun on-ui-thread-p ()
  "Whether the current thread is the CEF UI thread."
  (and *library-loaded-p*
       (plusp (cl-cef-on-ui-thread))))

(defun native-name (path)
  (when path
    (uiop:native-namestring path)))

(defun ensure-directory (path)
  "Create PATH as a directory and return it.
The implementation UIOP may not export `ensure-directories-exist'."
  (ensure-directories-exist (uiop:ensure-directory-pathname path)))

(defun default-cache-path ()
  (ensure-directory (uiop:xdg-cache-home "cl-cef/")))

(defun default-subprocess-path ()
  (asdf:system-relative-pathname :cl-cef "build/cl-cef-helper"))

(defun default-resources-path ()
  (asdf:system-relative-pathname :cl-cef "build/"))

(defun command-line-argv (extra)
  "Process argv plus EXTRA switches. Includes argv0.

Chromium installs a SIGSEGV handler for crash dumps. SBCL uses SIGSEGV as a
write barrier, so that handler has to stay disabled."
  (append (or (ignore-errors (uiop:raw-command-line-arguments))
              (cons (or (uiop:argv0) "cl-cef")
                    (uiop:command-line-arguments)))
          '("--disable-in-process-stack-traces"
            "--disable-breakpad"
            "--disable-crash-reporter")
          extra))

(defun foreign-argv (arguments)
  "Allocate a NULL-terminated foreign argv. Caller owns the memory."
  (let* ((argc (length arguments))
         (argv (cffi:foreign-alloc :pointer :count (1+ argc) :initial-element (cffi:null-pointer))))
    (loop for argument in arguments
          for index from 0
          do (setf (cffi:mem-aref argv :pointer index)
                   (cffi:foreign-string-alloc argument :encoding :utf-8)))
    (values argc argv)))

(defun free-argv (argc argv)
  (when argv
    (dotimes (index argc)
      (cffi:foreign-string-free (cffi:mem-aref argv :pointer index)))
    (cffi:foreign-free argv)))

(export-always 'alive-p)
(defmethod alive-p ((interface interface))
  "Whether INTERFACE's UI thread is still running."
  (and (ui-thread interface)
       (bt:thread-alive-p (ui-thread interface))))

(defmethod (setf protocols) (value (interface interface))
  (when (alive-p interface)
    (error "Protocols need to be set before launching ~a." interface))
  (setf (slot-value interface 'protocols) value))

(cffi:defcallback event-callback :int ((object-id :int) (json :string))
  (handler-case (dispatch-event object-id json)
    (error (condition)
      (warn "cl-cef event error: ~a" condition)
      0)))

(cffi:defcallback scheme-callback :void ((scheme :string) (url :string) (response :pointer))
  (handler-case (dispatch-scheme scheme url response)
    (error (condition)
      (cl-cef-response-set response (cffi:null-pointer) 0 "text/plain" 500 "Error"
                           (format nil "content-type: text/plain~%"))
      (warn "cl-cef scheme error: ~a" condition))))

(cffi:defcallback ready-callback :void ()
  (when *launch-ready*
    (bt:signal-semaphore *launch-ready*)))

(defun dispatch-event (object-id json)
  "Run listeners for the event encoded in JSON. Return 1 when one of them did."
  (let* ((data (cl-json:decode-json-from-string json))
         (event-name (assoc-value data :event))
         (event (when (stringp event-name)
                  (intern (string-upcase event-name) :keyword)))
         (key (cons object-id event))
         (entries (copy-list (gethash key *listeners*)))
         (consumed nil))
    (dolist (entry entries)
      (destructuring-bind (callback object once-p) entry
        (let ((result (funcall callback object data)))
          (when result
            (setf consumed t))
          (when once-p
            (setf (gethash key *listeners*)
                  (remove entry (gethash key *listeners*) :test #'eq))))))
    (if consumed 1 0)))

(defvar *scheme-callbacks* (make-hash-table :test 'equal)
  "Scheme name to request handler. Defined here, filled by `handle-callback'.")

(defun dispatch-scheme (scheme url response)
  (let ((callback (gethash scheme *scheme-callbacks*)))
    (if callback
        (multiple-value-bind (data mime status headers reason)
            (funcall callback url)
          (fill-response response data mime status headers reason))
        (cl-cef-response-set response (cffi:null-pointer) 0 "text/plain" 404 "Not Found" ""))))

(defun fill-response (response data mime status headers reason)
  (let* ((octets (etypecase data
                   (null (make-array 0 :element-type '(unsigned-byte 8)))
                   (string (babel:string-to-octets data :encoding :utf-8))
                   ((vector (unsigned-byte 8)) data)))
         (size (length octets))
         (pointer (cffi:foreign-alloc :unsigned-char :count (max size 1))))
    (loop for index below size
          do (setf (cffi:mem-aref pointer :unsigned-char index)
                   (aref octets index)))
    (unwind-protect
         (cl-cef-response-set response
                              (if (plusp size) pointer (cffi:null-pointer))
                              size
                              (or mime "text/html")
                              (or status 200)
                              (or reason "OK")
                              (format-headers headers))
      (cffi:foreign-free pointer))))

(defun format-headers (headers)
  (with-output-to-string (stream)
    (dolist (header headers)
      (format stream "~a: ~a~%" (car header) (cdr header)))))

(export-always 'add-listener)
(defgeneric add-listener (object event callback &key once-p)
  (:method ((object remote-object) (event symbol) (callback function) &key once-p)
    (let ((key (cons (id object) event)))
      (setf (gethash key *listeners*)
            (append (gethash key *listeners*)
                    (list (list callback object once-p)))))
    callback)
  (:documentation "Register CALLBACK to run for EVENT on OBJECT.

CALLBACK is called as (callback object data). DATA is an alist decoded from the
bridge event. A non-nil return value consumes synchronous events such as
`:before-input-event' and `:window-open'.

Callbacks run on the CEF UI thread. They must return without waiting on that
thread. When ONCE-P is non-nil, CALLBACK is removed after it runs."))

(export-always 'within-ui-thread)
(defun within-ui-thread (thunk)
  "Run THUNK on the CEF UI thread and return its value."
  (if (on-ui-thread-p)
      (funcall thunk)
      (let ((semaphore (bt:make-semaphore))
            (value nil)
            (condition nil))
        (post-ui (lambda ()
                   (handler-case (setf value (funcall thunk))
                     (error (caught) (setf condition caught)))
                   (bt:signal-semaphore semaphore)))
        (bt:wait-on-semaphore semaphore)
        (if condition
            (error condition)
            value))))

(defvar *ui-queue-lock* (bt:make-lock "cl-cef-ui-queue"))
(defvar *ui-queue* nil)
(defvar *ui-queue-tail* nil)

(defun enqueue-ui (thunk)
  (bt:with-lock-held (*ui-queue-lock*)
    (let ((cell (list thunk)))
      (if *ui-queue-tail*
          (setf (cdr *ui-queue-tail*) cell)
          (setf *ui-queue* cell))
      (setf *ui-queue-tail* cell))))

(defun dequeue-ui ()
  (bt:with-lock-held (*ui-queue-lock*)
    (when *ui-queue*
      (prog1 (pop *ui-queue*)
        (unless *ui-queue*
          (setf *ui-queue-tail* nil))))))

(cffi:defcallback drain-ui-queue :void ()
  (loop for task = (dequeue-ui)
        while task
        do (handler-case (funcall task)
             (error (condition)
               (warn "cl-cef UI task error: ~a" condition)))))

(defun post-ui (thunk)
  "Queue THUNK and ask the UI thread to drain the queue."
  (enqueue-ui thunk)
  (cl-cef-post (cffi:callback drain-ui-queue)))

(defun resolve-path (path default)
  (native-name (or path (funcall default))))

(defun start-ui-thread (interface)
  (let ((subprocess (resolve-path (subprocess-path interface) #'default-subprocess-path))
        (cache (resolve-path (cache-path interface) #'default-cache-path))
        (resources (resolve-path (resources-dir-path interface) #'default-resources-path))
        (locales (native-name (or (locales-dir-path interface)
                                  (uiop:merge-pathnames* "locales/"
                                                         (or (resources-dir-path interface)
                                                             (default-resources-path))))))
        (schemes (format-scheme-spec (protocols interface)))
        (downloads (native-name (or (download-directory interface)
                                    (ensure-directory
                                     (uiop:xdg-data-home "cl-cef/downloads/"))))))
    (unless (uiop:file-exists-p subprocess)
      (error "CEF helper ~a does not exist. Build cl-cef with CMake first."
             subprocess))
    (ensure-directory cache)
    (multiple-value-bind (argc argv)
        (foreign-argv (command-line-argv
                       (append (launch-options interface)
                               (unless (string= schemes "")
                                 (list (format nil "--cl-cef-schemes=~a" schemes))))))
      (setf (slot-value interface 'argv-pointers) (list argc argv))
      (unless (plusp (cl-cef-initialize argc argv
                                        subprocess
                                        cache
                                        resources
                                        locales
                                        (user-agent interface)
                                        schemes
                                        (flag (no-sandbox-p interface))
                                        (remote-debugging-port interface)))
        (free-argv argc argv)
        (setf (slot-value interface 'argv-pointers) nil)
        (error "CEF failed to initialize."))
      (cl-cef-set-download-directory downloads)
      (cl-cef-run-message-loop (cffi:callback ready-callback))
      (cl-cef-shutdown)
      (free-argv argc argv)
      (setf (slot-value interface 'argv-pointers) nil))))

(defvar *request-decider* nil
  "Function of (url document-url type-name). Returns blockp, a stylesheet, and
a replacement encoding. The stylesheet is injected into document and
subdocument loads. The replacement encoding rewrites the response body.
NIL allows every request.")

(cffi:defcallback request-callback :void
    ((url :string) (document-url :string) (resource-type :string) (decision :pointer))
  (handler-case
      (if *request-decider*
          (multiple-value-bind (blockp css replacements)
              (funcall *request-decider* url document-url resource-type)
            (cl-cef-request-decision-set decision (if blockp 1 0) (or css ""))
            (cl-cef-request-decision-replace decision (or replacements "")))
          (cl-cef-request-decision-set decision 0 ""))
    (error (condition)
      (warn "cl-cef request filter error: ~a" condition)
      (cl-cef-request-decision-set decision 0 ""))))

(defvar *request-thread* nil
  "Lisp thread that decides whether a network request is blocked.
CEF calls the handler on its IO thread, which cannot enter Lisp.")

(defun start-request-thread ()
  (when (and *request-thread* (bt:thread-alive-p *request-thread*))
    (return-from start-request-thread *request-thread*))
  (setf *request-thread*
        (bt:make-thread
         (lambda ()
           (loop while (plusp (cl-cef-request-pump))))
         :name "cl-cef request thread")))

(defun install-request-blocker ()
  "Register `*request-decider*' and start the thread that answers requests."
  (cl-cef-set-request-callback (cffi:callback request-callback))
  (start-request-thread))

(defun reset-adblock-cosmetic ()
  "Forget element-hiding stylesheets held by the bridge."
  (when *library-loaded-p*
    (cl-cef-adblock-reset-cosmetic)))

(defvar *scheme-thread* nil
  "Lisp thread that serves custom-scheme requests.
CEF calls scheme handlers on its IO thread, which cannot enter Lisp.")

(defun start-scheme-thread ()
  (when (and *scheme-thread* (bt:thread-alive-p *scheme-thread*))
    (return-from start-scheme-thread *scheme-thread*))
  (setf *scheme-thread*
        (bt:make-thread
         (lambda ()
           (loop while (plusp (cl-cef-scheme-pump))))
         :name "cl-cef scheme thread")))

(export-always 'launch)
(defun launch (&optional (interface *interface*))
  "Start CEF for INTERFACE. Return NIL when it is already running."
  (unless interface
    (error "No CEF interface to launch."))
  (when (alive-p interface)
    (warn "CEF is already running, nothing to do.")
    (return-from launch nil))
  (ensure-library (library-path interface))
  (cl-cef-set-event-callback (cffi:callback event-callback))
  (cl-cef-set-scheme-callback (cffi:callback scheme-callback))
  (setf *launch-error* nil)
  (setf *launch-ready* (bt:make-semaphore))
  (setf (slot-value interface 'ui-thread)
        (bt:make-thread
         (lambda ()
           (handler-case (start-ui-thread interface)
             (serious-condition (condition)
               (format t "~&cl-cef ui thread error: ~a~%" condition)
               (finish-output)
               (setf *launch-error* condition)
               (bt:signal-semaphore *launch-ready*))))
         :name "cl-cef ui thread"))
  (bt:wait-on-semaphore *launch-ready*)
  (when *launch-error*
    (error *launch-error*))
  (unless (alive-p interface)
    (error "CEF UI thread exited during startup."))
  ;; After CefInitialize. Chromium forks its subprocess around that call, so
  ;; this thread must not exist yet.
  (start-scheme-thread)
  (when *request-decider*
    (install-request-blocker))
  #+sbcl
  (pushnew 'terminate sb-ext:*exit-hooks*)
  t)

(export-always 'join)
(defun join (&optional (interface *interface*))
  "Block until INTERFACE's UI thread exits."
  (when (and interface (ui-thread interface))
    (bt:join-thread (ui-thread interface))))

(export-always 'terminate)
(defun terminate (&optional (interface *interface*))
  "Quit the CEF message loop. The UI thread shuts CEF down as it exits."
  (unless (and interface (alive-p interface))
    (warn "Already terminated, nothing to do.")
    (return-from terminate nil))
  (cl-cef-request-stop)
  (cl-cef-scheme-stop)
  (cl-cef-quit-message-loop)
  t)
