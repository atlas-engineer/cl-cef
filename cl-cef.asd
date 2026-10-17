;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-cef"
  :version "0.0.0"
  :author "Atlas Engineer LLC"
  :description "Common Lisp interface to the Chromium Embedded Framework."
  :license "BSD 3-Clause"
  :depends-on (alexandria
               babel
               bordeaux-threads
               cffi
               cl-json
               nclasses
               uiop)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "library" :depends-on ("package"))
                 (:file "core" :depends-on ("package" "library"))
                 (:file "protocol" :depends-on ("package" "core"))
                 (:file "view" :depends-on ("package" "core"))
                 (:file "window" :depends-on ("package" "core" "view"))
                 (:file "web-contents" :depends-on ("package" "core" "view"))
                 (:file "session" :depends-on ("package" "core")))))
  :in-order-to ((test-op (test-op "cl-cef/tests"))))

(defsystem "cl-cef/tests"
  :pathname "tests"
  :depends-on (cl-cef lisp-unit2)
  :components ((:file "tests"))
  :perform (test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :lisp-unit2 :run-tests
                               :package :cef/tests)))
