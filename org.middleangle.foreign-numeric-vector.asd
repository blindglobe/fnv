;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(in-package :cl-user)

(defpackage :fnv-asd
  (:use :cl :asdf))

(in-package :fnv-asd)

(defsystem :org.middleangle.foreign-numeric-vector
  :depends-on (:cffi :iterate #+ccl :trivial-garbage)
  :components
  ((:file "packages")
   (:file "utils"
	  :depends-on ("packages"))
   (:file "lowlevel-copy"
	  :depends-on ("packages" "utils"))
   (:file "foreign-numeric-vector"
	  :depends-on ("packages" "utils" "lowlevel-copy"))))
