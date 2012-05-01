;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defpackage :org.middleangle.foreign-numeric-vector-examples
  (:nicknames :fnv-examples)
  (:use :common-lisp :fnv :fnv-utils :cffi)
  (:export :double-float-basic-examples 
	   :double-float-write-tests
	   :double-float-read-tests
	   :complex-double-float-basic-examples
	   :complex-double-float-write-tests
	   :complex-double-float-read-tests
	   ))

(in-package :org.middleangle.foreign-numeric-vector-examples)

(declaim (optimize (speed 3) (safety 0) (debug 1)))
; (declaim (optimize (speed 1) (safety 3) (debug 3)))


(defun double-float-basic-examples ()
  (let* ((length 3)
	 (fnv (make-fnv-double length)))
    (format t "Alloc'ed foreign array: ~A~%" fnv)

    ;; This is pretty vanilla.
    (dotimes (i length)
      (setf (fnv-double-ref fnv i) (random 1.0d0)))
    (format t "Filled array using fnv-double-ref: ~A~%" fnv)

    ;; This is faster, because it separates out the pointer.
    (with-fnv-double-ptr (p fnv)
      (dotimes (i length)
	(setf (fnv-double-ptr-ref p i) (random 1.0d0)))
      (format t "Filled array using fnv-double-ptr-ref: ~A~%" fnv))

    ;; This separates out the pointer, and uses symbol-macrolet
    ;; to DTRT for both reading and writing!
    (over-fnv-double (v) fnv
      (setf v (random 1.0d0)))
    (format t "Filled array using over-fnv-double: ~A~%" fnv)

    (let ((sum 0.0d0))
      (declare (type double-float sum))
      (over-fnv-double (v) fnv
	(incf sum v))
      (format t "Sum=~A~%" sum))
    (format t "Sorted <: ~A~%" (sort-fnv-double-<! (copy-fnv-double fnv)))
    (format t "Sorted >: ~A~%" (sort-fnv-double->! (copy-fnv-double fnv)))
    ;; Coming soon... sorting with indexing.
    ))


(defun complex-double-float-basic-examples ()
  (let* ((length 3)
	 (fnv (make-fnv-complex-double length)))
    (format t "Alloc'ed foreign array: ~A~%" fnv)

    ;; This is pretty vanilla.
    (dotimes (i length)
      (setf (fnv-complex-double-ref fnv i) (complex (random 1.0d0) (random 1.0d0))))
    (format t "Filled array using fnv-complex-double-ref: ~A~%" fnv)

    ;; This is faster, because it separates out the pointer.
    (with-fnv-complex-double-ptr (p fnv)
      (dotimes (i length)
	(setf (fnv-complex-double-ptr-ref p i) 
	      (complex (random 1.0d0) (random 1.0d0))))
      (format t "Filled array using fnv-complex-double-ptr-ref: ~A~%" fnv))

    ;; This separates out the pointer, and uses symbol-macrolet
    ;; to DTRT for both reading and writing!
    (over-fnv-complex-double (v) fnv
      (setf v (complex (random 1.0d0) (random 1.0d0))))
    (format t "Filled array using over-fnv-complex-double: ~A~%" fnv)
    
    (let ((sum #C(0.0d0 0.0d0)))
      (declare (type (complex double-float) sum))
      (over-fnv-complex-double (v) fnv
	(incf sum v))
      (format t "Sum=~A~%" sum))
    
    (format t "Copy=~A~%" (copy-fnv-complex-double fnv))
    (format t "Sorted Realpart <: ~A~%" 
    	    (sort-fnv-complex-double-realpart-<!  
    	     (copy-fnv-complex-double fnv)))
    (format t "Sorted abs: ~A~%" 
    	    (sort-fnv-complex-double-abs!  
    	     (copy-fnv-complex-double fnv)))
    ))


;; On the author's machine (SBCL 0.9.14, Linux), for SUFFICIENTLY
;; LARGE experiments (e.g. length = reps = 100000), for reading, the
;; native, mem-ref, with-fnv-double-ptr and over-fnv-double versions
;; take about the same time, athe mem-aref version is about 40%
;; slower, and the fnv-double-ref is almost twice as slow.  (All bets
;; are off for much smaller (though still quite large seeming)
;; experiments).

(defun double-float-write-native (length reps)
  (let ((native (make-array length :element-type 'double-float)))
    (declare (type fixnum length)
	     (type fixnum reps)
	     (type (simple-array double-float (*)) native))

    (fixtimes (r reps)
      (fixtimes (i length)
	(setf (aref native i) 1.0d0)))
    (format t "Filled native array with 1.0d0.")))

(defun double-float-write-mem-aref (length reps)
  (let ((foreign (foreign-alloc :double :count length)))
    (declare (type fixnum length)
	     (type fixnum reps))

    (fixtimes (r reps)
      (fixtimes (i length)
	(setf (mem-aref foreign :double i) 1.0d0)))
    (format t "Filled foreign array with 1.0d0, using mem-aref.")))
    
(defun double-float-write-mem-ref (length reps)
  (let ((foreign (foreign-alloc :double :count length)))
    (declare (type fixnum length)
	     (type fixnum reps))
    
    (fixtimes (r reps)
      (fixtimes (i length)
	(setf (mem-ref foreign 
		       :double 
		       (fix* #.(foreign-type-size :double) i))
	      1.0d0)))
    (format t "Filled foreign array with 1.0d0, using mem-ref.")))

(defun double-float-write-fnv-double-ref (length reps)
  (let ((fnv (make-fnv-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))
    
    (fixtimes (r reps)
      (fixtimes (i (fnv-double-length fnv))
	(setf (fnv-double-ref fnv i) 1.0d0)))
    
    (format t "Filled foreign array with 1.0d0, using fixtimes and fnv-double-ref.~%")))


(defun double-float-write-with-fnv-double-ptr (length reps)
  (let ((fnv (make-fnv-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))

    (fixtimes (r reps)
      (with-fnv-double-ptr (p fnv)
	(fixtimes (i (fnv-double-length fnv))
	  (setf (fnv-double-ptr-ref p i) 1.0d0))))
    (format t "Filled foreign array with 1.0d0, using with-fnv-double-ptr.~%")))

(defun double-float-write-over-fnv-double (length reps)
  (let ((fnv (make-fnv-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))
    
    (fixtimes (r reps)
      (over-fnv-double (v) fnv
	(setf v 1.0d0)))
    (format t "Filled foreign array with 1.0d0, using over-fnv-double.~%")))

(defun double-float-write-tests (length reps)
  (time (double-float-write-native length reps))
  (time (double-float-write-mem-aref length reps))
  (time (double-float-write-mem-ref length reps))
  (time (double-float-write-fnv-double-ref length reps))
  (time (double-float-write-with-fnv-double-ptr length reps))
  (time (double-float-write-over-fnv-double length reps)))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro dfinc (var amt)
    `(setf ,var (df+ ,var ,amt))))

(defun double-float-read-native (length reps native)
  (declare (type fixnum length)
	   (type fixnum reps)
	   (type (simple-array double-float (*)) native))
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (fixtimes (i length)
	(dfinc sum (aref native i))))
    (format t "Summed native array, sum=~A~%" sum)))


(defun double-float-read-mem-aref (length reps foreign)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (fixtimes (i length)
	(dfinc sum (mem-aref foreign :double i))))
    (format t "Summed foreign array with mem-aref, sum=~A~%" sum)))

(defun double-float-read-mem-ref (length reps foreign)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (fixtimes (i length)
	(dfinc sum 
	       (mem-ref foreign :double 
			(fix* #.(foreign-type-size :double) i)))))
    (format t "Summed foreign array with mem-ref, sum=~A~%" sum)))


(defun double-float-read-fnv-double-ref (length reps fnv)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (fixtimes (i length)
	(dfinc sum (fnv-double-ref fnv i))))
    (format t "Summed foreign array with fnv-double-ref, sum=~A~%" sum)))


(defun double-float-read-with-fnv-double-ptr (length reps fnv)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (with-fnv-double-ptr (p fnv)
	(fixtimes (i length)
	  (dfinc sum (fnv-double-ptr-ref p i)))))
    (format t "Summed foreign array with fnv-double-ptr-ref, sum=~A~%" sum)))

  
(defun double-float-read-over-fnv-double (length reps fnv)
  (declare (ignore length))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (fixtimes (r reps)
      (setf sum 0.0d0)
      (over-fnv-double (d) fnv
	  (dfinc sum d)))
    (format t "Summed foreign array with fnv-double-ptr-ref, sum=~A~%" sum)))


(defun double-float-read-tests (length reps)
  (let* ((foreign (foreign-alloc :double :count length))
	 (native (make-array length :element-type 'double-float))
	 (fnv (make-fnv-double length)))
    (declare (type fixnum length)
	     (type fixnum reps)
	     (type (simple-array double-float (*)) native))

    ;; Populate array.
    (fixtimes (i length)
      (let ((r (random 1.0d0)))
	(setf (aref native i) r
	      (fnv-double-ref fnv i) r
	      (mem-ref foreign :double 
		       (fix* #.(foreign-type-size :double) i))
	      r)))

  (time (double-float-read-native length reps native))
  (time (double-float-read-mem-aref length reps foreign))
  (time (double-float-read-mem-ref length reps foreign))
  (time (double-float-read-fnv-double-ref length reps fnv))
  (time (double-float-read-with-fnv-double-ptr length reps fnv))
  (time (double-float-read-over-fnv-double length reps fnv))
  
  (foreign-free foreign)))

(defun complex-double-float-write-native (length reps)
  (let ((native (make-array length :element-type '(complex double-float))))
    (declare (type fixnum length)
	     (type fixnum reps)
	     (type (simple-array (complex double-float) (*)) native))

    (fixtimes (r reps)
      (fixtimes (i length)
	(setf (aref native i) #C(1.0d0 1.0d0))))
    (format t "Filled native array with #C(1.0d0 1.0d0).")))

;; This isn't valid, we don't have the right type converter in place.    

;; (defun complex-double-float-write-mem-aref (length reps)
;;   (let ((foreign (foreign-alloc 'complex-double :count length)))
;;     (declare (type fixnum length)
;; 	     (type fixnum reps))

;;     (fixtimes (r reps)
;;       (fixtimes (i length)
;; 	(setf (mem-aref foreign 'complex-double i) #C(1.0d0 1.0d0))))
;;     (format t "Filled foreign array with #C(1.0d0 1.0d0), using mem-aref.")))

;; (defun complex-double-float-write-mem-ref (length reps)
;;   (let ((foreign (foreign-alloc 'complex-double :count length)))
;;     (declare (type fixnum length)
;; 	     (type fixnum reps))
    
;;     (fixtimes (r reps)
;;       (fixtimes (i length)
;; 	(setf (mem-ref foreign 
;; 		       'complex-double 
;; 		       (fix* #.(foreign-type-size 'complex-double) i))
;; 	      #C(1.0d0 1.0d0))))
;;     (format t "Filled foreign array with #C(1.0d0 1.0d0), using mem-ref.")))

(defun complex-double-float-write-fnv-complex-double-ref (length reps)
  (let ((fnv (make-fnv-complex-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))
    
    (fixtimes (r reps)
      (fixtimes (i (fnv-complex-double-length fnv))
	(setf (fnv-complex-double-ref fnv i) #C(1.0d0 1.0d0))))
    
    (format t "Filled foreign array with #C(1.0d0 1.0d0), using fixtimes and fnv-complex-double-ref.~%")))



(defun complex-double-float-write-with-fnv-complex-double-ptr (length reps)
  (let ((fnv (make-fnv-complex-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))

    (fixtimes (r reps)
      (with-fnv-complex-double-ptr (p fnv)
	(fixtimes (i (fnv-complex-double-length fnv))
	  (setf (fnv-complex-double-ptr-ref p i) #C(1.0d0 1.0d0)))))
    (format t "Filled foreign array with #C(1.0d0 1.0d0), using with-fnv-complex-double-ptr.~%")))


(defun complex-double-float-write-over-fnv-complex-double (length reps)
  (let ((fnv (make-fnv-complex-double length)))
    (declare (type fixnum length)
	     (type fixnum reps))
    
    (fixtimes (r reps)
      (over-fnv-complex-double (v) fnv
	(setf v #C(1.0d0 1.0d0))))
    (format t "Filled foreign array with #C(1.0d0 1.0d0), using over-fnv-complex-double.~%")))

(defun complex-double-float-write-tests (length reps)
  (time (complex-double-float-write-native length reps))
  ;; (time (complex-double-float-write-mem-aref length reps))
  ;; (time (complex-double-float-write-mem-ref length reps))
  (time (complex-double-float-write-fnv-complex-double-ref length reps))
  (time (complex-double-float-write-with-fnv-complex-double-ptr length reps))
  (time (complex-double-float-write-over-fnv-complex-double length reps)))


(eval-when (:compile-toplevel :load-toplevel)
  (defmacro cdfinc (var amt)
    `(setf ,var (cdf+ ,var ,amt))))

(defun complex-double-float-read-native (length reps native)
  (declare (type fixnum length)
	   (type fixnum reps)
	   (type (simple-array (complex double-float) (*)) native))
  
  (let ((sum #C(0.0d0 0.0d0)))
    (declare (type (complex double-float) sum))
    (fixtimes (r reps)
      (setf sum #C(0.0d0 0.0d0))
      (fixtimes (i length)
	(cdfinc sum (aref native i))))
    (format t "Summed native array, sum=~A~%" sum)))


;; Complex analogs of these are not yet implemented.
;; (defun double-float-read-mem-aref (length reps foreign)
;;   (declare (type fixnum length)
;; 	   (type fixnum reps))
  
;;   (let ((sum 0.0d0))
;;     (declare (type double-float sum))
;;     (fixtimes (r reps)
;;       (setf sum 0.0d0)
;;       (fixtimes (i length)
;; 	(dfinc sum (mem-aref foreign :double i))))
;;     (format t "Summed foreign array with mem-aref, sum=~A~%" sum)))

;; (defun double-float-read-mem-ref (length reps foreign)
;;   (declare (type fixnum length)
;; 	   (type fixnum reps))
  
;;   (let ((sum 0.0d0))
;;     (declare (type double-float sum))
;;     (fixtimes (r reps)
;;       (setf sum 0.0d0)
;;       (fixtimes (i length)
;; 	(dfinc sum 
;; 	       (mem-ref foreign :double 
;; 			(fix* #.(foreign-type-size :double) i)))))
;;     (format t "Summed foreign array with mem-ref, sum=~A~%" sum)))


(defun complex-double-float-read-fnv-complex-double-ref (length reps fnv)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum #C(0.0d0 0.0d0)))
    (declare (type (complex double-float) sum))
    (fixtimes (r reps)
      (setf sum #C(0.0d0 0.0d0))
      (fixtimes (i length)
	(cdfinc sum (fnv-complex-double-ref fnv i))))
    (format t "Summed foreign array with fnv-complex-double-ref, sum=~A~%" sum)))


(defun complex-double-float-read-with-fnv-complex-double-ptr (length reps fnv)
  (declare (type fixnum length)
	   (type fixnum reps))
  
  (let ((sum #C(0.0d0 0.0d0)))
    (declare (type (complex double-float) sum))
    (fixtimes (r reps)
      (setf sum #C(0.0d0 0.0d0))
      (with-fnv-complex-double-ptr (p fnv)
	(fixtimes (i length)
	  (cdfinc sum (fnv-complex-double-ptr-ref p i)))))
    (format t "Summed foreign array with fnv-complex-double-ptr-ref, sum=~A~%" sum)))

  
(defun complex-double-float-read-over-fnv-complex-double (length reps fnv)
  (declare (ignore length))
  (let ((sum #C(0.0d0 0.0d0)))
    (declare (type (complex double-float) sum))
    (fixtimes (r reps)
      (setf sum #C(0.0d0 0.0d0))
      (over-fnv-complex-double (d) fnv
	  (cdfinc sum d)))
    (format t "Summed foreign array with fnv-double-ptr-ref, sum=~A~%" sum)))


(defun complex-double-float-read-tests (length reps)
  (let* ((native (make-array length :element-type '(complex double-float)))
	 (fnv (make-fnv-complex-double length)))
    (declare (type fixnum length)
	     (type fixnum reps)
	     (type (simple-array (complex double-float) (*)) native))

    ;; Populate array.
    (fixtimes (i length)
      (let ((r (complex (random 1.0d0) (random 1.0d0))))
	(setf (aref native i) r
	      (fnv-complex-double-ref fnv i) r)))

    (time (complex-double-float-read-native length reps native))
    ;;(time (double-float-read-mem-aref length reps foreign))
    ;;(time (double-float-read-mem-ref length reps foreign))
    (time (complex-double-float-read-fnv-complex-double-ref length reps fnv))
    (time (complex-double-float-read-with-fnv-complex-double-ptr length reps fnv))
    (time (complex-double-float-read-over-fnv-complex-double length reps fnv))))

    
(defun double-float-readme-example (length)
  (let ((fnv (make-fnv-double length)))
    (fixtimes (i length)
      (setf (fnv-double-ref fnv i) (random 1.0d0)))
    
    (let ((sum 0.0d0))
      (declare (type double-float sum))
      (over-fnv-double (v) fnv
	(dfinc sum v))
      (format t "Sum of array is ~A~%" sum))

    (sort-fnv-double-<! fnv)
    (format t "The smallest few entries: ~A~%" fnv)
    (sort-fnv-double->! fnv)
    (format t "The largest few entries: ~A~%" fnv)
    ))
