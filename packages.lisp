;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

;; Copyright 2008--2012, AJ Rossini <blindglobe@gmail.com>.  Same license as Rif.
;; (mostly to move package statements into a single file, as per CLHS
;; recommendation.

(defpackage :org.middleangle.foreign-numeric-vector-utils
  (:nicknames :fnv-utils)
  (:use :common-lisp :cffi :iterate)
  (:export :complex-float :complex-double
           :ncat :with-gensyms :fixfor :fixtimes
	   :fix+ fix* :fix- :fix/
	   :df+ :df* :df- :df/
	   :sf+ :sf* :sf- :sf/
	   :cdf+ :cdf* :cdf- :cdf/
	   :csf+ :csf* :csf- :csf/))

(defpackage :org.middleangle.foreign-numeric-vector-lowlevel-copy
  (:nicknames :fnv-lowlevel-copy)
  (:use :common-lisp :cffi :fnv-utils :iter)
  (:export :make-lowlevel-copier 
	   :lowlevel-copy-float 
	   :lowlevel-copy-double
	   :lowlevel-copy-int
	   :lowlevel-copy-int32
	   :lowlevel-copy-int64))

(defpackage :org.middleangle.foreign-numeric-vector
  (:nicknames :foreign-numeric-vector :fnv)
  (:use :common-lisp :cffi :iterate :fnv-utils :fnv-lowlevel-copy)
  #+ccl (:shadowing-import-from :trivial-garbage)
  (:export :fnv-foreign-pointer :fnv-length :fnv-copy 
	   :*fnv-print-length* :fnv-allset :cffi-type-to-fnv-type

	   :cffi-fnv-complex-double
	   :copy-fnv-complex-double
	   :sort-fnv-complex-double-abs!
	   :over-fnv-float
	   :in-fnv-complex-float
	   :fnv-int32-ptr-ref
	   :fnv-complex-float-ptr-ref
	   :sort-fnv-complex-double-realpart-<!
	   :cffi-fnv-float
	   :fnv-complex-float-length
	   :copy-fnv-float
	   :fnv-complex-float


	   :make-fnv-int32
	   :with-fnv-complex-float-ptr
	   :copy-fnv-complex-float
	   :fnv-double-ptr-ref
	   :sort-fnv-double->!
	   :fnv-double-length
	   :fnv-complex-float-foreign-pointer
	   :fnv-int32
	   :fnv-complex-double-length
	   :fnv-float
	   :make-fnv-complex-float
	   :fnv-double-ref
	   :sort-fnv-double-<!
	   :over-fnv-complex-double
	   :over-fnv-int32
	   :copy-fnv-int32
	   :with-fnv-float-ptr
	   :fnv-complex-float-ref
	   :make-fnv-float
	   :sort-fnv-complex-double-imagpart-<!
	   :fnv-double-foreign-pointer
	   :fnv-double
	   :in-fnv-double
	   :fnv-float-ref
	   :sort-fnv-float->!
	   :fnv-float-ptr-ref
	   :sort-fnv-complex-float-realpart-<!
	   :cffi-fnv-int32
	   :sort-fnv-complex-float-abs!
	   :in-fnv-int32
	   :make-fnv-double
	   :over-fnv-complex-float
	   :sort-fnv-complex-float-imagpart-<!
	   :fnv-complex-double-foreign-pointer
	   :in-fnv-float
	   :cffi-fnv-double
	   :sort-fnv-int32-<!
	   :sort-fnv-int32->!
	   :with-fnv-double-ptr
	   :fnv-int32-ref
	   :over-fnv-double
	   :with-fnv-complex-double-ptr
	   :fnv-float-length
	   :sort-fnv-float-<!
	   :fnv-complex-double
	   :cffi-fnv-complex-float
	   :fnv-complex-double-ptr-ref
	   :fnv-complex-double-ref
	   :with-fnv-int32-ptr
	   :make-fnv-complex-double
	   :fnv-float-foreign-pointer
	   :fnv-int32-foreign-pointer
	   :in-fnv-complex-double
	   :copy-fnv-double
	   :fnv-int32-length

	   :FNV-INT64-REF
	   :FNV-INT64-PTR-REF
	   :FNV-INT64-FOREIGN-POINTER
	   :SORT-FNV-INT64-<!
	   :COPY-FNV-INT64
	   :MAKE-FNV-INT64
	   :IN-FNV-INT64
	   :OVER-FNV-INT64
	   :SORT-FNV-INT64->!
	   :CFFI-FNV-INT64
	   :FNV-INT64-LENGTH
	   :WITH-FNV-INT64-PTR
	   :FNV-INT64	   ))

