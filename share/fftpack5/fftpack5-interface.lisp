;;; -*- Mode: lisp -*-

(in-package #:maxima)

(in-package :fftpack5)

(defun mlist->complex-cl-vector (object)
  "Convert Maxima list to a (simple-array (complex double-float) (*))
  with the same values."
  (let ((z (make-array (1- (length object)) :element-type '(complex double-float))))
    (loop for k of-type fixnum from 0
	  for x in (cdr object)
	  while x
	  do
	     (let ((fl (maxima::risplit (maxima::$float x))))
	       (setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun complex-cl-vector->mlist (array)
  "Convert a CL array of complex values to a Maxima list containing
  the same values."
  (declare (type (simple-array (complex double-float) (*)) array))
  (cons '(maxima::mlist maxima::simp)
	(loop for w of-type (complex double-float) across array
	      collect (maxima::add (realpart w)
				   (maxima::mul (imagpart w) 'maxima::$%i)))))

(defun array->complex-cl-vector (array)
  "Convert a CL vector of maxima values to a (simsple-array (complex
  double-float) (*)) with the same complex values."
  (let* ((n (length array))
	 (z (make-array n :element-type '(complex double-float))))
    (dotimes (k n)
      (let ((fl (maxima::risplit (maxima::$float (aref array k)))))
	(setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun complex-cl-vector->vector (array)
  "Convert (simple-array (complex double-float) (*)) to a CL vector of
  maxima expressions of the same value."
  (declare (type (simple-array (complex double-float) (*)) array))
  (let* ((n (length array))
	 (z (make-array n)))
    (dotimes (k n)
      (let ((item (aref array k)))
	(setf (aref z k) (maxima::add (realpart item)
				      (maxima::mul 'maxima::$%i (imagpart item))))))
    z))
  
(defun find-complex-fft-converters (object &optional (maxima-function-name "fftpack5"))
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  complex FFT routines.  Two values are returned: The specialized
  vector and a function that will convert the result of the FFT
  routine to a Maxima object of the same type as
  OBJECT. MAXIMA-FUNCTION-NAME is a string to use for error messages."
  ;;(format t "object = ~S~%" object)
  ;;(format t "arrayp = ~S~%" (arrayp object))
  ;;(describe 'arrayp)
  (cond ((maxima::$listp object)
	 (values
	  (mlist->complex-cl-vector object)
	  #'complex-cl-vector->mlist))
	((arrayp object)
	 (values
	  (array->complex-cl-vector object)
	  #'complex-cl-vector->vector))
	((and (symbolp object) (maxima::symbol-array (maxima::mget object 'maxima::array)))
	 (values
	  (let* ((sym-array (maxima::symbol-array (maxima::mget object 'maxima::array))))
	    (array->complex-cl-vector sym-array))
	  #'(lambda (array)
	      (let ((ar (complex-cl-vector->vector array))
		    (sym (maxima::meval `((maxima::$array)
					  ,(intern (symbol-name (gensym "$G")) "MAXIMA")
					  maxima::$float
					  ,(1- (length array))))))
		(setf (maxima::symbol-array (maxima::mget sym 'maxima::array))
		      ar)
		sym))))
	(t
	 (maxima::merror "~A: input is not a list or an array: ~M." maxima-function-name object))))

(defun mlist->rfft-vector (object)
  "Convert Maxima list to a (simple-array (complex double-float) (*))
  with the same values suitable for as the input for the RFFT routine.
  The maxima list is assumed to consist only of real values."
  (let* ((n (length (cdr object)))
	 (z (make-array n :element-type 'double-float)))
    (loop for k from 0
	  for x in (cdr (maxima::$float object))
	  do
	     (setf (aref z k) x))
    z))

(defun array->rfft-vector (array)
  "Convert CL array of real (Maxima) values to a (simple-array
  (complex double-float) (*)) with the same values suitable for as the
  input for the RFFT routine.  The CL array is assumed to consist only
  of real values."
  (let* ((n (length array))
	 (z (make-array n :element-type 'double-float)))
    (loop for k from 0 below n
	  for m from 0
	  do
	     (setf (aref z m) (maxima::$float (aref array k))))
    z))
  
(defun find-rfft-converters (object)
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  RFFT routines.  Two values are returned: The specialized vector and
  a function that will convert the result of the RFFT routine to a
  Maxima object of the same type as OBJECT. MAXIMA-FUNCTION-NAME is a
  string to use for error messages."
  (cond ((maxima::$listp object)
	 (values
	  (mlist->rfft-vector object)
	  #'complex-cl-vector->mlist))
	((arrayp object)
	 (values
	  (let ((z (make-array  (length object) :element-type '(complex double-float))))
	    (loop for k of-type fixnum from 0
		  for x across object
		  do
		     (let ((fl (maxima::risplit (maxima::$float x))))
		       (setf (aref z k) (complex (car fl) (cdr fl)))))
	    z)
	  #'complex-cl-vector->vector))
	((and (symbolp object) (maxima::symbol-array (maxima::mget object 'maxima::array)))
	 (let ((sym-array (maxima::symbol-array (maxima::mget object 'maxima::array))))
	   (values
	    (array->rfft-vector sym-array)
	    #'(lambda (array)
		(let ((ar (complex-cl-vector->vector array))
		      (sym (maxima::meval `((maxima::$array)
				    ,(intern (symbol-name (gensym "$G")))
				    maxima::$float
				    ,(1- (length array))))))
		  (setf (maxima::symbol-array (maxima::mget sym 'maxima::array))
			ar))))))
	(t
	 (maxima::merror "real_fft: input is not a list or an array: ~M." object))))

(defun find-irfft-converters (object)
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  inverse RFFT routines.  Two values are returned: The specialized
  vector and a function that will convert the result of the inverse
  RFFT routine to a Maxima object of the same type as
  OBJECT. MAXIMA-FUNCTION-NAME is a string to use for error messages."
  (cond ((maxima::$listp object)
	 (values
	  (mlist->complex-cl-vector object)
	  #'(lambda (obj)
	      (let (result)
		(map nil #'(lambda (z)
			     (push z result))
		     obj)
		(cons '(maxima::mlist maxima::simp) (nreverse result))))))
	((arrayp object)
	 (values
	  (array->complex-cl-vector object)
	  #'(lambda (obj)
	      (let ((result (make-array (length obj))))
		(loop for k from 0 by 2
		      for z across obj
		      do
			 (progn
			   (setf (aref result k) z)))
		result))))
	(t
	 (maxima::merror "inverse_real_fft: input is not a list or an array: ~M." object))))

(in-package :maxima)

(defmfun $fftpack5_fft (input)
  "fftpack5_fft(z)

  Computes the FFT of the complex-valued vector <input>.  The <input>
  may have any length, but the FFT is most efficient if the length has
  the form 2^r*3^s*5^t.

  Let x be the input and y be the transformed value.  Then the FFT is
  defined by

  y[k] = (1/n) sum(x[j] exp(+2 %i %pi j k / n), j, 0, n - 1)"

  (multiple-value-bind (z from-lisp)
      (fftpack5::find-complex-fft-converters input)
    ;; To match the fft package, we want to do the inverse FFT here
    ;; and then we need to scale the result by 1/N.
    (fftpack5:inverse-cfft z)
    (let* ((n (length z)) (n-float (coerce n 'double-float)))
      (dotimes (k n)
	(setf (aref z k) (/ (aref z k) n-float)))
      (funcall from-lisp z))))

(defmfun $fftpack5_inverse_fft (input)
  "fftpack5_inverse_fft(z)

  Computes the inverse FFT of the complex-valued vector <input>.  The <input>
  may have any length, but the FFT is most efficient if the length has
  the form 2^r*3^s*5^t.

  Let y be the input and x be the inverse-transformed value.  Then the
  inverse FFT is defined by

  x[j] = sum(y[k] exp(-2 %i %pi j k / n), k, 0, n - 1)"

  (multiple-value-bind (z from-lisp)
      (fftpack5::find-complex-fft-converters input)
    ;; To match the fft package, we want to do the forward FFT here
    ;; and scale the result by N.
    (fftpack5:cfft z)
    (let* ((n (length z)) (n-float (coerce n 'double-float)))
      (dotimes (k n)
	(setf (aref z k) (* n-float (aref z k))))
      (funcall from-lisp z))))

(defmfun $fftpack5_real_fft (input)
  "fftpack5_real_fft(input)

  Computes the FFT of the real-valued vector <input>.  The <input>
  may have any length, but the FFT is most efficient if the length has
  the form 2^r*3^s*5^t.

  No check is made to ensure that the input contains only real values.

  Let x be the input and y be the transformed value.  Then the FFT is
  defined by

  y[k] = (1/n) sum(x[j] exp(+2 %i %pi j k / n), j, 0, n - 1)

  for k = 0, 1, ... floor(n/2).  Because of the symmetries of the FFT,
  y[0] is real.  If n is even y[n/2] is real.  If n is odd,
  y[floor(n/2)] may be complex.

  Use fftpack5_inverse_rfft to compute the inverse FFT"

  (multiple-value-bind (z from-lisp)
      (fftpack5::find-rfft-converters input)
    (funcall from-lisp (fftpack5::rfft z))))

(defmfun $fftpack5_inverse_real_fft (input len)
  "fftpack5_inverse_real_fft(input, len)

  Computes the inverse FFT of <input>.  The <input> must be in the
  form returned by fftpack5_real_fft.  Note that <len> MUST be
  provided and should be the length of the inverse transform (which
  must be the length of the original vector that was transformed).
  This is needed because the length of <input> cannot not uniquely
  determine the length of the output.
"

  (multiple-value-bind (z from-lisp)
      (fftpack5::find-irfft-converters input)
    (funcall from-lisp (fftpack5::inverse-rfft z len))))
