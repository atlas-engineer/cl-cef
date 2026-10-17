;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package cl-user)
(uiop:define-package :cef/tests
  (:use :cl :lisp-unit2)
  (:import-from :cef))
(in-package :cef/tests)

(define-test parse-privileges-bits ()
  (assert-eql 0 (cef:parse-privileges nil))
  (assert-eql 0 (cef:parse-privileges ""))
  (assert-eql 88 (cef:parse-privileges
                  "{secure:true,supportFetchAPI:true,corsEnabled:true}"))
  (assert-eql 3 (cef:parse-privileges "{standard:true,local:true}"))
  (assert-eql 36 (cef:parse-privileges "{displayIsolated:true,bypassCSP:true}")))

(define-test format-scheme-spec-joins-protocols ()
  (let ((protocols
          (list (make-instance 'cef:protocol
                               :scheme-name "nyxt"
                               :privileges "{standard:true}")
                (make-instance 'cef:protocol
                               :scheme-name "lisp"
                               :privileges "{secure:true,supportFetchAPI:true,corsEnabled:true}"))))
    (assert-string= "nyxt:1,lisp:88" (cef:format-scheme-spec protocols))
    (assert-string= "" (cef:format-scheme-spec nil))))

(define-test windows-key-code-to-code ()
  (assert-string= "KeyA" (cef:windows-key-code-to-code 65))
  (assert-string= "Digit0" (cef:windows-key-code-to-code 48))
  (assert-string= "Enter" (cef:windows-key-code-to-code #x0D))
  (assert-string= "F1" (cef:windows-key-code-to-code #x70))
  (assert-string= "keypad0" (cef:windows-key-code-to-code #x60))
  (assert-false (cef:windows-key-code-to-code nil))
  (assert-false (cef:windows-key-code-to-code 1)))
