;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

;; repackaged according to CLHS suggestions, AJ Rossini <blindglobe@gmail.com>
;; Copyright 2008.

(in-package :org.middleangle.foreign-numeric-vector-utils)

(eval-when (:compile-toplevel :load-toplevel)
  (defun ncat (&rest args)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name args))))
  
  (defmacro with-gensyms (names &body body)
    `(let (,@(mapcar (lambda (name)
		       (list name `(gensym ,(symbol-name name))))
		     names))
      ,@body)))

(defmacro fixfor ((var from to) &body body)
  `(iter (for ,var from (the fixnum ,from) below (the fixnum ,to))
    (declare (iterate:declare-variables))
    (declare (type fixnum ,var))
    ,@body))

;; This would be nice, but iterate's for isn't quite as efficient.
;; (defmacro fixtimes ((var to) &body body)
;;   `(fixfor (,var 0 ,to) ,@body))

(defmacro fixtimes ((var to) &body body)
  `(dotimes (,var (the fixnum ,to))
    (declare (type fixnum ,var))
    ,@body))

(defmacro make-typed-op (name function type)
  `(defmacro ,name (&rest args)
    `(the ,',type (,',function 
		     ,@(mapcar (lambda (a) `(the ,',type ,a)) args)))))

(defmacro make-typed-ops (type basename)
  `(progn
    ,@(mapcar (lambda (op)
		(let ((name (ncat basename op)))
		  `(make-typed-op ,name ,op ,type)))
	      '(+ * - /))))

(make-typed-ops fixnum fix)
(make-typed-ops double-float df)
(make-typed-ops single-float sf)
(make-typed-ops (complex double-float) cdf)
(make-typed-ops (complex single-float) csf)


(defcstruct complex-float
  (real :float)
  (imag :float))

(defcstruct complex-double
  (real :double)
  (imag :double))
