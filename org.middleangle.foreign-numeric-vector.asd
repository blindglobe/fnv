;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(in-package :cl-user)

(defpackage :fnv-asd
  (:use :cl :asdf))

(in-package :fnv-asd)

(defsystem :org.middleangle.foreign-numeric-vector
  :depends-on (:cffi :iterate)
  :components
  ((:file "utils")
   (:file "lowlevel-copy" :depends-on ("utils"))
   (:file "foreign-numeric-vector" :depends-on ("utils" "lowlevel-copy"))))
