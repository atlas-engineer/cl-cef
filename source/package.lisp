;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package #:cef
  (:nicknames :cl-cef)
  (:use :cl)
  (:import-from :nclasses :define-class)
  (:import-from :alexandria :assoc-value :if-let :when-let))

(in-package :cef)

(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))
