;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Custom schemes. Names and privilege bits must be registered before
;;;; `launch'; handlers may be attached afterwards.

(in-package :cef)

(export-always 'parse-privileges)
(defun parse-privileges (privileges)
  "Return the CEF scheme-option bitmask for an Electron-style PRIVILEGES string.

Recognized tokens are standard, local, displayIsolated, secure, corsEnabled,
bypassCSP, and supportFetchAPI. A missing or empty string yields 0.

The bit values match cef_scheme_options_t:
standard 1, local 2, display-isolated 4, secure 8, cors 16, bypass-csp 32,
fetch 64."
  (let ((text (string-downcase (or privileges ""))))
    (flet ((enabled (token bit)
             (if (or (search (format nil "~a:true" token) text)
                     (search (format nil "~a: true" token) text))
                 bit
                 0)))
      (logior (enabled "standard" 1)
              (enabled "local" 2)
              (enabled "displayisolated" 4)
              (enabled "display-isolated" 4)
              (enabled "secure" 8)
              (enabled "corsenabled" 16)
              (enabled "cors-enabled" 16)
              (enabled "bypasscsp" 32)
              (enabled "bypass-csp" 32)
              (enabled "supportfetchapi" 64)
              (enabled "support-fetch-api" 64)))))

(export-always 'format-scheme-spec)
(defun format-scheme-spec (protocols)
  "Encode PROTOCOLS as name:bits pairs for --cl-cef-schemes."
  (format nil "~{~a:~d~^,~}"
          (loop for protocol in protocols
                append (list (scheme-name protocol)
                             (parse-privileges (privileges protocol))))))

(define-class protocol ()
  ((scheme-name
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "Custom scheme name, without the colon. \"nyxt\" is an example.")
   (privileges
    "{standard:true,secure:true,supportFetchAPI:true,corsEnabled:true}"
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "Electron-style privilege string. See `parse-privileges'.")
   (interface
    *interface*
    :export t
    :reader t
    :writer nil
    :type (or interface null)))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A custom scheme handled by Lisp."))

(export-always 'handle-callback)
(defmethod handle-callback ((protocol protocol) callback)
  "Serve requests for PROTOCOL by calling CALLBACK.
CALLBACK takes the URL string and returns up to five values: body (string or
octet vector), MIME type, status code, response headers as an alist, and a
reason phrase. The scheme itself must already be in `protocols' at `launch'."
  (setf (gethash (scheme-name protocol) *scheme-callbacks*) callback)
  (when *interface*
    (setf (slot-value *interface* 'protocols)
          (cons protocol
                (remove (scheme-name protocol)
                        (protocols *interface*)
                        :key #'scheme-name
                        :test #'string=))))
  protocol)

(export-always 'handle-content)
(defmethod handle-content ((protocol protocol) content)
  "Serve the constant string CONTENT for every request to PROTOCOL."
  (handle-callback protocol (lambda (url)
                              (declare (ignore url))
                              content)))
