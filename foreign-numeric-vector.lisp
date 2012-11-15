;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

;; repackaged according to CLHS suggestions, AJ Rossini <blindglobe@gmail.com>
;; Copyright 2008.

(in-package :org.middleangle.foreign-numeric-vector)

;; (declaim (optimize (speed 1) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 1)))

(defun finalize (object function)
  #+sbcl (sb-ext:finalize object function)
  #+cmu (extensions:finalize object function)
  #+clisp (ext:finalize object function)
  #+ccl (trivial-garbage:finalize object function)
  #-(or sbcl cmu clisp ccl)
  (error "Please add an equivalent to FINALIZE for your lisp"))

(defgeneric fnv-foreign-pointer (fnv))
(defgeneric fnv-copy (fnv))
(defgeneric fnv-length (fnv))
(defgeneric fnv-allset (fnv val &optional try-to-coerce-p))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *cffi-type-to-fnv-type* (make-hash-table :test #'equal))

  (defun cffi-type-to-fnv-type (cffi-type)
    (gethash cffi-type *cffi-type-to-fnv-type*))

  (defun set-cffi-type-to-fnv-type (cffi-type fnv-type)
    (setf (gethash cffi-type *cffi-type-to-fnv-type*) fnv-type))

  (defsetf cffi-type-to-fnv-type set-cffi-type-to-fnv-type)
  
  (defparameter *fnv-print-length* 6)

  (defmacro make-fnv-typed-vector 
      (name 
       &key lisp-type
       cffi-underlying-type (cffi-elts-per-lisp-elt 1)
       (lisp-underlying-type lisp-type)
       (part-extractors '(identity))
       (lisp-constructor 'identity)
       (sorters '((< <) (> >)))
       cffi-vector-name)
    ;; Argument checking
    (declare (type fixnum cffi-elts-per-lisp-elt)
	     (type cons part-extractors)
	     (type cons sorters))
    (when (null lisp-type)
      (error "lisp-type is a required argument."))
    (when (null cffi-underlying-type)
      (error "cffi-underlying-type is a required argument."))
    (when (not (= (the fixnum (length part-extractors))
		  cffi-elts-per-lisp-elt))
      (error "(= (length part-extractors) cffi-elts-per-lisp-elt) required."))
    ;; Argument filling
    (let* ((cffi-vector-name (or cffi-vector-name cffi-underlying-type))
	   (fnv-name (ncat 'fnv- name))
	   (make-fnv-name (ncat 'make- fnv-name))
	   (lowlevel-copier (ncat 'lowlevel-copy- cffi-underlying-type))
	   (copy-fnv-name (ncat 'copy- fnv-name))
	   (constructor (ncat 'make- fnv-name '-internal))
	   (key-name name)
	   (fnv-ref (ncat fnv-name '-ref))
	   (fnv-ptr-ref (ncat fnv-name '-ptr-ref))
	   (set-fnv-ref (ncat 'set- fnv-ref))
	   (set-fnv-ptr-ref (ncat 'set- fnv-ptr-ref))
	   (fnv-ptr (ncat fnv-name '-foreign-pointer))
	   (fnv-name-length (ncat fnv-name '-length))
	   (in-fnv-name (ncat 'in- fnv-name))
	   (over-fnv-name (ncat 'over- fnv-name))
	   (with-fnv-ptr-name (ncat 'with- fnv-name '-ptr))
	   (cffi-type (intern (symbol-name (ncat 'cffi- fnv-name))))
           (cffi-type-class (intern (symbol-name (ncat cffi-type '-type))))
	   (foreign-elt-size (foreign-type-size cffi-underlying-type)))
      (with-gensyms (index-sym val-sym fnv-ptr-ref-sym)
	`(progn
	  (defstruct (,fnv-name (:constructor ,constructor))
	    (foreign-pointer nil :read-only t)
	    (length 0 :type fixnum :read-only t))
	  
	  (defun ,make-fnv-name (length &key foreign-ptr initial-value)
	    (let ((foreign-ptr (or foreign-ptr 
				   (foreign-alloc ',key-name :count length))))
	      (let ((,fnv-name
		     (,constructor :foreign-pointer foreign-ptr
				   :length (if (null-pointer-p foreign-ptr)
					     -1
					     length))))
		(when initial-value
		  (fnv-allset ,fnv-name initial-value))
                ;; clisp gives arguments to the function passed to
                ;; finalize
		(finalize ,fnv-name (lambda (&rest args)
                                      (declare (ignore args))
                                      (foreign-free foreign-ptr)))
		,fnv-name)))
	  (declaim (inline ,fnv-ptr))
	  
	  (defun ,copy-fnv-name (fnv)
	    (let* ((n (fnv-length fnv))
		   (new-fnv (,make-fnv-name (fnv-length fnv))))
	      (,lowlevel-copier (,fnv-ptr fnv) (,fnv-ptr new-fnv) 
				(fix* n ,cffi-elts-per-lisp-elt))
	      new-fnv))
	  
	  (defmethod fnv-copy ((f ,fnv-name))
	    (,copy-fnv-name f))
	  
	  (defun ,fnv-ref (,fnv-name i)
	    (,fnv-ptr-ref (,fnv-ptr ,fnv-name) i))
	  
	  (define-compiler-macro ,fnv-ref (,fnv-name i)
	    `(,',fnv-ptr-ref (,',fnv-ptr ,,fnv-name) ,,'i))
	  
	  (defun ,fnv-ptr-ref (,fnv-ptr i)
	    (,lisp-constructor 
	     ,@(iter (for ind from 0 below cffi-elts-per-lisp-elt)
		     (declare (type fixnum ind cffi-elts-per-lisp-elt))
		     (collect 
			 `(mem-ref ,fnv-ptr
			   ,cffi-underlying-type
			   (fix* (fix+ (fix* ,cffi-elts-per-lisp-elt i) ,ind)
			    ,foreign-elt-size))))))
	  
	  (define-compiler-macro ,fnv-ptr-ref (,fnv-ptr i)
	    `(let ((,',fnv-ptr-ref-sym ,,fnv-ptr)
		   (,',index-sym ,,'i))
	      (,',lisp-constructor 
	       ,,@(iter (for ind from 0 below cffi-elts-per-lisp-elt)
			(declare (type fixnum ind cffi-elts-per-lisp-elt))
			(collect
			    ``(mem-ref ,',fnv-ptr-ref-sym
			       ,',cffi-underlying-type 
			       (fix* (fix+ (fix* ,',cffi-elts-per-lisp-elt 
						 ,',index-sym)
				      ,',ind)
				,',foreign-elt-size)))))))
	  
	  (defun ,set-fnv-ref (,fnv-name i val)
	    (,set-fnv-ptr-ref (,fnv-ptr ,fnv-name) i val))

	  (define-compiler-macro ,set-fnv-ref (,fnv-name i val)
	    `(,',set-fnv-ptr-ref (,',fnv-ptr ,,fnv-name) ,,'i ,,'val))

	  (defsetf ,fnv-ref ,set-fnv-ref)

	  (defun ,set-fnv-ptr-ref (,fnv-ptr i val)
	    ,@(iter (for cnt from 0)
		    (declare (type fixnum cnt))
		    (for p-e in part-extractors)
		    (collect
 			`(setf 
 			  (mem-ref ,fnv-ptr ,cffi-underlying-type 
 			   (fix* (fix+ (fix* ,cffi-elts-per-lisp-elt i) ,cnt)
			    ,foreign-elt-size))
			  (the ,lisp-underlying-type (,p-e val))))))

	  
	  (define-compiler-macro ,set-fnv-ptr-ref (,fnv-ptr i val)
	    `(let ((,',fnv-ptr-ref-sym ,,fnv-ptr)
		   (,',index-sym ,,'i)
		   (,',val-sym ,,'val))
	      ,,@(iter 
		  (for ind from 0)
		  (declare (type fixnum ind))
		  (for p-e in part-extractors)
		  (collect 
		      ``(setf 
			 (mem-ref ,',fnv-ptr-ref-sym
			  ,',cffi-underlying-type
			  (fix* (fix+ (fix* ,',cffi-elts-per-lisp-elt 
				      ,',index-sym)
				      ,',ind)
			   ,',foreign-elt-size))
			 (the ,',lisp-underlying-type
			   (,',p-e ,',val-sym)))))))
	  

	  (defsetf ,fnv-ptr-ref ,set-fnv-ptr-ref)

	  (defmacro ,over-fnv-name ((var &optional (index (gensym)))
				    fnv
				    &body body)
	    (let ((fnv-sym (gensym))
		  (ptr-sym (gensym)))
	      `(let* ((,,'fnv-sym ,,'fnv)
		      (,,'ptr-sym (,',fnv-ptr ,,'fnv-sym)))
		(fixtimes (,index (,',fnv-name-length ,,'fnv-sym))
		  (symbol-macrolet 
			((,var (,',fnv-ptr-ref 
				,,'ptr-sym 
				,index)))
		      ,@body)))))
	
	  (defmacro ,with-fnv-ptr-name ((var ,fnv-name) &body body)
	    `(let ((,var (,',fnv-ptr ,,fnv-name)))
	      ,@body))
	  
	  (defmethod fnv-foreign-pointer ((f ,fnv-name))
	    (,fnv-ptr f))

	  
	  (defmethod fnv-length ((f ,fnv-name))
	    (,fnv-name-length f))
	  
	  (defmethod fnv-allset ((f ,fnv-name) val 
				 &optional (try-to-coerce-p t))
	    (let ((val (cond ((typep val ',lisp-type) val)
			     (try-to-coerce-p (coerce val ',lisp-type))
			     (t (error "Wrong type.")))))
	      (fixtimes (i (fnv-length f))
		(setf (,fnv-ref f i) val))
	      f))

	  
	  (defmethod print-object ((f ,fnv-name) str)
	    (when *print-readably*
	      (error 'print-not-readable :object f))
	    (let ((length (fnv-length f)))
	      (declare (type fixnum length)
		       (type fixnum *fnv-print-length*))
	      (format str "#<~A (~A)" (type-of f) length)
	      (iter (for i from 0 to (1- (min length *fnv-print-length*)))
		    (declare (type fixnum i))
		    (format str " ~A" (,fnv-ref f i)))
	      (when (> length *fnv-print-length*)
		(format str " ... ~A" (,fnv-ref f (1- length))))
	      (format str ">")))
	  
          (define-foreign-type ,cffi-type-class ()
            ()
            (:actual-type :pointer)
            (:simple-parser ,cffi-type))
	  (defmethod translate-to-foreign ((val ,fnv-name) 
					   (name ,cffi-type-class))
	    (fnv-foreign-pointer val))
	  
	  (setf (cffi-type-to-fnv-type ',cffi-vector-name) ',cffi-type)

	  ,@(mapcar 
	     (lambda (function-name)
	       (destructuring-bind (less-than name) function-name 
		 (let ((vqs-name (ncat 'sort- fnv-name '- name '!)))
		   
		   `(progn
		     (defun ,vqs-name (v) 
		       (labels 
			   ((vqs-int (k m) ; Quicksort vector v from k up to m.
			      (declare (type fixnum k m))
			      (if (>= k m) v
				  (let* ((x (,fnv-ref v k))        
					 (i (split1 k (1- m) x)))  
				    (setf (,fnv-ref v i) x)        
				    (vqs-int k i)                  
				    (vqs-int (1+ i) m))))          
			    (split1 (i j x)
			      (declare (type fixnum i j)
				       (type ,lisp-type x))
			      (if (= i j) i
				  (let* ((vj (,fnv-ref v j)))      
				    (if (,less-than vj x)
					(progn (setf (,fnv-ref v i) vj) 
					       (split2 (1+ i) j x))
					(split1 i (1- j) x))))) 
			    (split2 (i j x)
			      (declare (type fixnum i j)
				       (type ,lisp-type x))
			      (if (= i j) i
				  (let* ((vi (,fnv-ref v i)))      
				    (if (not (,less-than vi x))
					(progn (setf (,fnv-ref v j) vi) 
					       (split1 i (1- j) x))
					(split2 (1+ i) j x))))))
			 (vqs-int 0 (fnv-length v))))

		     (export (list ',vqs-name))))))
	     sorters)
	  
	  
	  (export (list ',fnv-name ',make-fnv-name ',copy-fnv-name
		   ',fnv-name-length ',fnv-ref
		   ',in-fnv-name ',over-fnv-name ',fnv-ptr ',fnv-ptr-ref
		   ',with-fnv-ptr-name ',cffi-type)))))))


(make-fnv-typed-vector :int32
		       :lisp-type (signed-byte 32)
		       :cffi-underlying-type :int32)

#+cffi-features:x86-64 
(make-fnv-typed-vector :int64
		       :lisp-type (signed-byte 64)
		       :cffi-underlying-type :int64)

(deftype :fnv-int () 
    #+cffi-features:x86 :fnv-int64
    #-cffi-features:x86 :fnv-int32)

(make-fnv-typed-vector :float
		       :lisp-type single-float
		       :cffi-underlying-type :float)

(make-fnv-typed-vector :double
		       :lisp-type double-float
		       :cffi-underlying-type :double)

(make-fnv-typed-vector complex-float 
		       :lisp-type (complex single-float)
		       :lisp-underlying-type single-float
		       :cffi-underlying-type :float
		       :cffi-elts-per-lisp-elt 2
		       :lisp-constructor complex
		       :part-extractors (realpart imagpart)
		       :cffi-vector-name complex-float
		       :sorters (((lambda (i j)
				    (flet ((mag^2 (i)
					     (+ (* (realpart i) (realpart i))
						(* (imagpart i) (imagpart i)))))
				      (< (mag^2 i) (mag^2 j))))
				  abs)
				 ((lambda (i j)
				    (< (realpart i) (realpart j)))
				  realpart-<)
				 ((lambda (i j)
				    (< (imagpart i) (imagpart j)))
				  imagpart-<)))

(make-fnv-typed-vector complex-double 
		       :lisp-type (complex double-float)
		       :lisp-underlying-type double-float
		       :cffi-underlying-type :double
		       :cffi-elts-per-lisp-elt 2
		       :lisp-constructor complex
		       :part-extractors (realpart imagpart)
		       :cffi-vector-name complex-double
		       :sorters (((lambda (i j)
				    (flet ((mag^2 (i)
					     (+ (* (realpart i) (realpart i))
						(* (imagpart i) (imagpart i)))))
				      (< (mag^2 i) (mag^2 j))))
				  abs)
				 ((lambda (i j)
				    (< (realpart i) (realpart j)))
				  realpart-<)
				 ((lambda (i j)
				    (< (imagpart i) (imagpart j)))
				  imagpart-<)))


