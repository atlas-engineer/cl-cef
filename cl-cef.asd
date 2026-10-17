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
                (
                 (:file "package")
                 (:file "library" :depends-on ("package"))
                 (:file "core" :depends-on ("package" "library"))
                 (:file "protocol" :depends-on ("package" "core"))
                 (:file "view" :depends-on ("package" "core"))
                 (:file "window" :depends-on ("package" "core" "view")))))
