;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

;; repackaged according to CLHS suggestions, AJ Rossini <blindglobe@gmail.com>
;; Copyright 2008.

(in-package :org.middleangle.foreign-numeric-vector-lowlevel-copy)

(declaim (optimize (speed 3) (safety 0) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel)

  ;; This code should be about as efficient as a C loop if it's
  ;; working properly and the compiler behaves.  It may be better to
  ;; define the low-level copiers via a foreign function that does
  ;; memory copies.
  (defmacro make-lowlevel-copier (type)
    (let* ((copier-name (ncat 'lowlevel-copy- type))
	   (ref-type (intern (symbol-name type) :keyword))
	   (size (foreign-type-size ref-type)))
      `(progn
	(defun ,copier-name (from-vec to-vec num-elts &optional 
			     (from-start 0) (to-start 0))

	  (let ((from-ind from-start)
		(to-ind to-start))
	    (declare (type fixnum from-ind to-ind num-elts))
	    (fixtimes (i num-elts)
	      (setf (mem-ref to-vec ,ref-type (fix* from-ind ,size))
		    (mem-ref from-vec ,ref-type (fix* to-ind ,size)))
	      
	      (setf to-ind (fix+ to-ind 1))
	      (setf from-ind (fix+ from-ind 1)))))
	
	
		(define-compiler-macro ,copier-name (from-vec to-vec num-elts 
						     &optional 
						     (from-start 0) (to-start 0))
		  (let ((from-sym (gensym "from"))
			(to-sym (gensym "to"))
			(num-elts-sym (gensym "num-elts"))
			(from-start-sym (gensym "from-start"))
			(to-start-sym (gensym "to-start"))
			(i-sym (gensym "i"))
			(j-sym (gensym "j")))
		    `(let ((,,'from-sym ,,'from-vec)
			   (,,'to-sym ,,'to-vec)
			   (,,'num-elts-sym ,,'num-elts)
			   (,,'from-start-sym ,,'from-start)
			   (,,'to-start-sym ,,'to-start))
		      (iter (for ,,'i-sym from ,,'from-start-sym below 
				 (fix+ ,,'from-start-sym ,,'num-elts-sym))
			    (for ,,'j-sym from ,,'to-start-sym)
			    (declare (type fixnum ,,'i-sym)
				     (type fixnum ,,'from-start-sym)
				     (type fixnum ,,'j-sym)
				     (type fixnum ,,'to-start-sym))
			    (setf (mem-ref ,,'to-sym ,',ref-type 
					   (fix* ,,'i-sym ,,size))
				  (mem-ref ,,'from-sym ,',ref-type 
					   (fix* ,,'j-sym ,,size)))))))
	
	))))
  
(make-lowlevel-copier float)
(make-lowlevel-copier double)
(make-lowlevel-copier int)
(make-lowlevel-copier int32)
#+cffi-features:x86-64 (make-lowlevel-copier int64)
