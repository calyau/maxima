;; -*- Lisp -*-

;; Maxima interface to the FFT routines and some various related
;; utilities.

(in-package :maxima)

;; These functions perhaps need revision if they are
;; needed at all. Output ignores the type of input.
(defun $recttopolar (rary iary)
  (let ((fast-rary (maxima-fft:fft-arg-check '$recttopolar rary))
	(fast-iary (maxima-fft:fft-arg-check '$recttopolar iary)))
    (maxima-fft:complex-to-polar fast-rary fast-iary)
    (list '(mlist) rary iary)))

(defun $polartorect (rary iary)
  (let ((fast-rary (maxima-fft:fft-arg-check '$polartorect rary))
	(fast-iary (maxima-fft:fft-arg-check '$polartorect iary)))
    (maxima-fft:polar-to-complex fast-rary fast-iary)
    (list '(mlist) rary iary)))

;; Maxima's forward FFT routine.
(defun $fft (input)
  (multiple-value-bind (z from-lisp)
      (maxima-fft:find-complex-fft-converters input)
    (let* ((size (length z))
	   (order (maxima-fft:log-base2 size)))
      (unless (= size (ash 1 order))
	(merror "fft: size of input must be a power of 2, not ~M" size))
      (when (> (length z) 1)
	(setf z (maxima-fft:fft-r2-nn z)))
      (funcall from-lisp z))))

;; Maxima's inverse FFT routine.
(defun $inverse_fft (input)
  (multiple-value-bind (z from-lisp)
      (maxima-fft:find-complex-fft-converters input "inverse_fft")
    (let* ((size (length z))
	   (order (maxima-fft:log-base2 size)))
      (unless (= size (ash 1 order))
	(merror "inverse_fft: size of input must be a power of 2, not ~M" size))
      (when (> (length z) 1)
	(setf z (maxima-fft:fft-r2-nn z :inverse-fft-p t)))
      (funcall from-lisp z))))

;;; Maxima's RFFT function.
;;;
;;; The input is assumed to consist of objects that can be converted
;;; to a real-valued float.  If the input has length N, then the
;;; output is a complex reuslt of length N/2 + 1 because of the
;;; symmetry of the FFT for real inputs.
(defun $real_fft (input)
  (multiple-value-bind (z from-lisp input-length)
      (maxima-fft:find-rfft-converters input)
    (declare (type (simple-array (complex double-float) (*)) z))
    (unless (= input-length (ash 1 (maxima-fft:log-base2 input-length)))
      (merror "real_fft: size of input must be a power of 2, not ~M" input-length))
    (let* ((n (ash (length z) 1))
	   (result (make-array (1+ (length z)) :element-type '(complex double-float))))
      ;; This declaration causes CCL to generate incorrect code for at
      ;; least version 1.11 DarwinX8664 on OSX 10.11.3.  Disable it.
      #-ccl
      (declare (type (simple-array (complex double-float) (*)) result))

      ;; We don't handle input of length 4 or less.  Use the complex
      ;; FFT routine to produce the desired (correct) output.
      (when (< n 3)
	(return-from $real_fft ($fft input)))
      (locally
	  ;; Setting safety to 0 causes an internal compiler error with CCL 1.11.
	  (declare (optimize (speed 3) (safety #-ccl 0 #+ccl 1)))
	;; Compute FFT of shorter complex vector.
	(setf z (maxima-fft:fft-r2-nn z))

	;; Reconstruct the FFT of the original from the parts
	(setf (aref result 0)
	      (complex (* 0.5
			  (+ (realpart (aref z 0))
			     (imagpart (aref z 0))))))
	  
	(let ((sincos (maxima-fft:sincos-table (maxima-fft:log-base2 n)))
	      (n/2 (length z)))
	  (declare (type (simple-array (complex double-float) (*)) sincos))

	  ;;(format t "n/2 = ~A~%" n/2)
	  (loop for k of-type fixnum from 1 below (length z) do
	    (setf (aref result k)
		  (* 0.25 (+ (+ (aref z k)
				(conjugate (aref z (- n/2 k))))
			     (* #c(0 -1d0)
				(aref sincos k)
				(- (aref z k)
				   (conjugate (aref z (- n/2 k)))))))))
	  (setf (aref result (length z))
		(complex 
		 (* 0.5
		    (- (realpart (aref z 0))
		       (imagpart (aref z 0))))))))
	(funcall from-lisp result))))

;;; Maxima's inverse RFFT routine.
;;;
;;; The input is assumed to consist of objects that can be converted
;;; to a complex-valued floats.  The input MUST have a length that is
;;; one more than a power of two, as is returned by RFFT.
(defun $inverse_real_fft (input)
  (multiple-value-bind (ft from-lisp)
      (maxima-fft:find-irfft-converters input)
    ;; declarations + (SPEED 3) tickles bug: https://bugs.launchpad.net/sbcl/+bug/1776091
    #-sbcl (declare (type (simple-array (complex double-float) (*)) ft))
    (let* ((n (1- (length ft))))
      (when (< n 2)
	;; Just use the regular inverse fft to compute these values
	;; because inverse_real_fft below doesn't work for these cases.
	(return-from $inverse_real_fft ($inverse_fft input)))
      (let* ((order (maxima-fft:log-base2 n))
	     (sincos (maxima-fft:sincos-table (1+ order)))
	     (z (make-array n :element-type '(complex double-float))))
	#-sbcl (declare (type (simple-array (complex double-float) (*)) sincos))

	(unless (= n (ash 1 order))
	  (merror "inverse_real_fft: input length must be one more than a power of two, not ~M" (1+ n)))

	(locally
	    #+sbcl nil #-sbcl (declare (optimize (speed 3)))
	  (loop for k from 0 below n
		do
		   (let ((evenpart (+ (aref ft k)
				      (conjugate (aref ft (- n k)))))
			 (oddpart (* (- (aref ft k)
					(conjugate (aref ft (- n k))))
				     (conjugate (aref sincos k)))))
		     (setf (aref z k) (+ evenpart
					 (* #c(0 1d0) oddpart)))))

	  (setf z (maxima-fft:fft-r2-nn z :inverse-fft-p t)))
	(funcall from-lisp z)))))

;; Bigfloat forward and inverse FFTs.  Length of the input must be a
;; power of 2.
(defun $bf_fft (input)
  (multiple-value-bind (z from-lisp)
      (maxima-fft:find-bf-fft-converter input)
    (let ((n (length z)))
      (unless (= n (ash 1 (maxima-fft:log-base2 n)))
	(merror "bf_fft: size of input must be a power of 2, not ~M" n))
      (if (> n 1)
	  (funcall from-lisp (bigfloat::fft-r2-nn z))
	  (funcall from-lisp z)))))

(defun $bf_inverse_fft (input)
  (multiple-value-bind (x unconvert)
      (maxima-fft:find-bf-fft-converter input "bf_inverse_fft")
    (let ((n (length x)))
      (unless (= n (ash 1 (maxima-fft:log-base2 n)))
	(merror "bf_inverse_fft: size of input must be a power of 2, not ~M" n))
      (if (> n 1)
	  (funcall unconvert (bigfloat::fft-r2-nn x :inverse-fft-p t))
	  (funcall unconvert x)))))

(defun $bf_real_fft (input)
  (multiple-value-bind (z from-lisp)
      (maxima-fft:find-bf-rfft-converters input)
    (let* ((n (ash (length z) 1))
	   (result (make-array (1+ (length z)))))

      (when (< n 3)
	(return-from $bf_real_fft ($bf_fft input)))
    
      ;; Compute FFT of shorter complex vector.  NOTE: the result
      ;; returned by bigfloat:fft has scaled the output by the length of
      ;; z.  That is, divided by n/2.  For our final result, we want to
      ;;     divide by n, so in the following bits of code, we have an
      ;;     extra factor of 2 to divide by.
      (setf z (bigfloat::fft-r2-nn z))

      ;;(format t "z = ~A~%" z)
      ;; Reconstruct the FFT of the original from the parts
      (setf (aref result 0)
	    (bigfloat:* 0.5
			(bigfloat:+ (bigfloat:realpart (aref z 0))
				    (bigfloat:imagpart (aref z 0)))))

      (let ((sincos (bigfloat::sincos-table (maxima-fft:log-base2 n)))
	    (n/2 (length z)))
	;;(format t "n/2 = ~A~%" n/2)
	(loop for k from 1 below (length z) do
	  (setf (aref result k)
		(bigfloat:* 0.25 (bigfloat:+ (bigfloat:+ (aref z k)
							 (bigfloat:conjugate (aref z (- n/2 k))))
					     (bigfloat:* #c(0 -1)
							 (aref sincos k)
							 (bigfloat:- (aref z k)
								     (bigfloat:conjugate (aref z (- n/2 k)))))))))
	(setf (aref result (length z))
	      (bigfloat:* 0.5
			  (bigfloat:- (bigfloat:realpart (aref z 0))
				      (bigfloat:imagpart (aref z 0)))))
	(funcall from-lisp result)))))

(defun $bf_inverse_real_fft (input)
  (multiple-value-bind (ft from-lisp)
      (maxima-fft:find-bf-irfft-converters input)
  (let* ((n (1- (length ft))))
    (when (< n 2)
      ;; Just use the regular inverse fft to compute these values
      ;; because inverse_real_fft below doesn't work for these cases.
      (return-from $bf_inverse_real_fft ($bf_inverse_fft input)))

    (let* ((z (make-array n))
	   (order (maxima-fft:log-base2 n))
	   (sincos (bigfloat::sincos-table (1+ order))))

      (unless (= n (ash 1 order))
	(merror "bf_inverse_real_fft: input length must be one more than a power of two, not ~M" (1+ n)))

      (loop for k from 0 below n
	    do
	       (let ((evenpart (bigfloat:+ (aref ft k)
					   (bigfloat:conjugate (aref ft (- n k)))))
		     (oddpart (bigfloat:* (bigfloat:- (aref ft k)
						      (bigfloat:conjugate (aref ft (- n k))))
					  (bigfloat:conjugate (aref sincos k)))))
		 (setf (aref z k)
		       (bigfloat:+ evenpart
				   (bigfloat:* #c(0 1) oddpart)))))

      ;;(format t "z = ~A~%" z)
      (let ((inverse (bigfloat::fft-r2-nn z :inverse-fft-p t)))
	;;(format t "inverse = ~A~%" inverse)
	(funcall from-lisp inverse))))))


