;; -*- Lisp -*-

(in-package :maxima-fft)
(use-package :maxima)

(defun mgetarray (marg) 
"Return the lisp array which is somehow attached to MARG."
  (or (and (symbolp marg) (symbol-array (mget marg 'maxima::array)))
      (and ($listp marg) (make-array ($length marg) :initial-contents (rest marg)))
      (and (arrayp marg) marg)))

(defun fft-arg-check (user-fcn-name ary)
  ;; I don't check here if this is really a floating point array.  For maxima
  ;; arrays which are symbols this would be no problem since the type is on
  ;; the property list.  On the other hand, for "fast" arrays (i.e. lisp
  ;; arrays), using ARRAY-ELEMENT-TYPE might not be too useful.
  (or (mgetarray ary)
      (merror "~M: argument must a list or array; instead found ~:M" user-fcn-name ary)))

(defun make-empty-copy (a)
  (cond
    (($listp a)
     (cons '(mlist) (make-list ($length a) :initial-element 0e0)))
    ((arrayp a)
     (make-array (length a) :initial-element 0e0))
    ((symbolp a)
     (meval
       `(($array)
         ,(intern (symbol-name (gensym "$G")))
         $float
         ,(1- (length (mgetarray a))))))))

(defun complex-to-polar (re im)
  (dotimes (k (min (length re) (length im)))
    (let ((rp (aref re k))
	  (ip (aref im k)))
      (setf (aref re k) (abs (complex rp ip)))
      (setf (aref im k) (atan ip rp)))))

(defun polar-to-complex (mag phase)
  (dotimes (k (min (length mag) (length phase)))
    (let ((r (aref mag k))
	  (p (aref phase k)))
      (setf (aref mag k) (* r (cos p)))
      (setf (aref phase k) (* r (sin p))))))

(declaim (inline log-base2))
(defun log-base2 (n)
  "Return the M such that 2^m <= N.  If N is a power of two, M =
  log2(N).  Expected usage is for the case of N a power of two."
  (declare (type (and fixnum (integer 0)) n)
	   (optimize (speed 3)))
  ;; Just find m such that 2^m <= n < 2^(m+1).  It's up to the caller
  ;; to make sure that n is a power of two.  (The previous
  ;; implementation using (/ (log n) (log 2)) has roundoff errors.
  ;; This doesn't.)
  (1- (integer-length n)))

(defvar *sincos-tables*
  (make-hash-table) 
  "Hash table mapping log2 of the FFT size to an
  array of exp(2*pi*i/N), where N is the FFT size.")

(defun sincos-table (m)
  "For an FFT order of M, return an array of complex twiddle factors
  for the FFT. The array contains the values exp(i*pi*k/2^(m-1))."
  (declare (type (and fixnum unsigned-byte) m))
  (cond ((gethash m *sincos-tables*))
	(t
	 ;; Need to create the sincos table.  Only need to have a half
	 ;; period.
	 (let* ((n (ash 1 (1- m)))
		(p (/ #+(and cmu flonum-double-double) kernel:dd-pi
		      #-flonum-double-double (coerce pi 'double-float)
		      n))
		(table (make-array n :element-type
				   #+(and cmu flonum-double-double) '(complex double-double-float)
				   #-flonum-double-double '(complex double-float))))
	   (dotimes (k n)
	     (setf (aref table k) (cis (* k p))))
	   ;; Make the half point exactly correct
	   (when (> n 1)
	     (setf (aref table (ash n -1))
		   (coerce #c(0 1) #+(and cmu flonum-double-double) '(complex double-double-float)
			   #-flonum-double-double '(complex double-float))))
	   (setf (gethash m *sincos-tables*) table)
	   table))))

(defun mlist->complex-cl-vector (object)
  "Convert Maxima list to a (simple-array (complex double-float) (*))
  with the same values."
  (let ((z (make-array (1- (length object)) :element-type '(complex double-float))))
    (loop for k of-type fixnum from 0
	  for x in (cdr object)
	  while x
	  do
	     (let ((fl (risplit ($float x))))
	       (setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun complex-cl-vector->mlist (array)
  "Convert a CL array of complex values to a Maxima list containing
  the same values."
  (declare (type (simple-array (complex double-float) (*)) array))
  (cons '(mlist simp)
	(loop for w of-type (complex double-float) across array
	      collect (add (realpart w)
			   (mul (imagpart w) '$%i)))))

(defun array->complex-cl-vector (array)
  "Convert a CL vector of maxima values to a (simsple-array (complex
  double-float) (*)) with the same complex values."
  (let* ((n (length array))
	 (z (make-array n :element-type '(complex double-float))))
    (dotimes (k n)
      (let ((fl (risplit ($float (aref array k)))))
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
	(setf (aref z k) (add (realpart item)
			      (mul '$%i (imagpart item))))))
    z))
  
(defun find-complex-fft-converters (object &optional (maxima-function-name "fft"))
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  complex FFT routines.  Two values are returned: The specialized
  vector and a function that will convert the result of the FFT
  routine to a Maxima object of the same type as
  OBJECT. MAXIMA-FUNCTION-NAME is a string to use for error messages."
  ;;(format t "object = ~S~%" object)
  ;;(format t "arrayp = ~S~%" (arrayp object))
  ;;(describe 'arrayp)
  (cond (($listp object)
	 (values
	  (mlist->complex-cl-vector object)
	  #'complex-cl-vector->mlist))
	((arrayp object)
	 (values
	  (array->complex-cl-vector object)
	  #'complex-cl-vector->vector))
	((and (symbolp object) (symbol-array (mget object 'maxima::array)))
	 (values
	  (let* ((sym-array (symbol-array (mget object 'maxima::array))))
	    (array->complex-cl-vector sym-array))
	  #'(lambda (array)
	      (let ((ar (complex-cl-vector->vector array))
		    (sym (meval `(($array)
				  ,(intern (symbol-name (gensym "$G")) "MAXIMA")
				  $float
				  ,(1- (length array))))))
		(setf (symbol-array (mget sym 'maxima::array))
		      ar)
		sym))))
	(t
	 (merror "~A: input is not a list or an array: ~M." maxima-function-name object))))

;; Fast Fourier Transform using a simple radix-2 implementation with
;; inputs and outputs in natural order.  The definition of the forward
;; FFT follows Maxima's definition of the forward FFT.
(defun fft-r2-nn (x &key (inverse-fft-p nil))
  "Compute the FFT of X which MUST be a specialzed vector of complex
  double-float's whose length MUST be a power of 2.  If INVERSE-FFT-P
  is non-NIL, the inverse FFT is computed.  The scaling by 1/N is
  included in the inverse FFT.

  The contents of the input X may be destroyed."
  (declare (type (simple-array (complex double-float) (*)) x))
  (let* ((n (length x))
	 (half-n (ash n -1))
	 (pairs-in-group (ash n -1))
	 (number-of-groups 1)
	 (distance (ash n -1))
	 (not-switch-input t)
	 (sincos (sincos-table (maxima-fft:log-base2 n)))
	 (a x)
	 (b (make-array (length x) :element-type '(complex double-float))))
    (declare (fixnum n half-n pairs-in-group number-of-groups distance)
	     (type (simple-array (complex double-float) (*)) a b sincos))
    (flet ((fft ()
	     (declare (optimize (speed 3) (safety 0)))
	     (let ((index 0))
	       (declare (fixnum index))
	       (dotimes (k number-of-groups)
		 (declare (fixnum k))
		 (let* ((jfirst (* 2 k pairs-in-group))
			(jlast (+ jfirst pairs-in-group -1))
			(jtwiddle (* k pairs-in-group))
			(w (let ((w (aref sincos jtwiddle)))
			     (if inverse-fft-p
			       (conjugate w)
			       w))))
		   (declare (fixnum jfirst jlast jtwiddle)
			    (type (complex double-float) w))
		   #+nil
		   (format t  "k = ~D, jfirst/last = ~D ~D jtwiddle = ~D dist ~D index ~D, W ~S~%"
			   k jfirst jlast jtwiddle distance index w)
		   (loop for j of-type fixnum from jfirst upto jlast do
		     (let ((temp (* w (aref a (+ j distance)))))
		       (setf (aref b index) (+ (aref a j) temp))
		       (setf (aref b (+ index half-n)) (- (aref a j) temp))
		       (incf index))))))))
      (loop while (< number-of-groups n) do
	(fft)
	
	#+nil
	(progn
	  (format t "number-of-groups = ~D~%" number-of-groups)
	  (format t "Output = ~S~%" b))

	(rotatef a b)
	(setf not-switch-input (not not-switch-input))
	(setf pairs-in-group (ash pairs-in-group -1))
	(setf number-of-groups (ash number-of-groups 1))
	(setf distance (ash distance -1)))

      (if inverse-fft-p
	  a
	  (dotimes (k n a)
	    (declare (fixnum k)
		     (optimize (speed 3) (safety 0)))
	    (let ((w (aref a k)))
	      (setf (aref a k) (/ w n))))))))

;;; RFFT
;;;
;;; The FFT of a real signal of length N can be computed using complex
;;; FFT of length N/2.  For large values of N, this can represent
;;; significant savings in computation.
;;;
;;; See http://www.engineeringproductivitytools.com/stuff/T0001/PT10.HTM.

(defun mlist->rfft-vector (object)
  "Convert Maxima list to a (simple-array (complex double-float) (*))
  with the same values suitable for as the input for the RFFT routine.
  The maxima list is assumed to consist only of real values."
  (let* ((n (length (cdr object)))
	 (z (make-array (ash n -1) :element-type '(complex double-float))))
    (loop for k from 0
	  for (re im) on (cdr object) by #'cddr
	  while im
	  do
	     (setf (aref z k) (complex ($float re) ($float im))))
    z))

(defun array->rfft-vector (array)
  "Convert CL array of real (Maxima) values to a (simple-array
  (complex double-float) (*)) with the same values suitable for as the
  input for the RFFT routine.  The CL array is assumed to consist only
  of real values."
  (let* ((n (length array))
	 (z (make-array (ash n -1) :element-type '(complex double-float))))
    (loop for k from 0 below n by 2
	  for m from 0
	  do
	     (setf (aref z m) (complex ($float (aref array k))
				       ($float (aref array (1+ k))))))
    z))
  
(defun find-rfft-converters (object)
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  RFFT routines.  Two values are returned: The specialized vector and
  a function that will convert the result of the RFFT routine to a
  Maxima object of the same type as OBJECT. MAXIMA-FUNCTION-NAME is a
  string to use for error messages."
  (cond (($listp object)
	 (values
	  (mlist->rfft-vector object)
	  #'complex-cl-vector->mlist
	  (1- (length object))))
	((arrayp object)
	 (values
	  (let ((z (make-array  (length object) :element-type '(complex double-float))))
	    (loop for k of-type fixnum from 0
		  for x across object
		  do
		     (let ((fl (risplit ($float x))))
		       (setf (aref z k) (complex (car fl) (cdr fl)))))
	    z)
	  #'complex-cl-vector->vector
	  (length object)))
	((and (symbolp object) (symbol-array (mget object 'maxima::array)))
	 (let ((sym-array (symbol-array (mget object 'maxima::array))))
	   (values
	    (array->rfft-vector sym-array)
	    #'(lambda (array)
		(let ((ar (complex-cl-vector->vector array))
		      (sym (meval `(($array)
				    ,(intern (symbol-name (gensym "$G")))
				    $float
				    ,(1- (length array))))))
		  (setf (symbol-array (mget sym 'maxima::array))
			ar)))
	    (length sym-array))))
	(t
	 (merror "real_fft: input is not a list or an array: ~M." object))))

(defun find-irfft-converters (object)
  "Convert a Maxima OBJECT to a specialized CL vector suitable for the
  inverse RFFT routines.  Two values are returned: The specialized
  vector and a function that will convert the result of the inverse
  RFFT routine to a Maxima object of the same type as
  OBJECT. MAXIMA-FUNCTION-NAME is a string to use for error messages."
  (cond (($listp object)
	 (values
	  (mlist->complex-cl-vector object)
	  #'(lambda (obj)
	      (let (result)
		(map nil #'(lambda (z)
			     (push (realpart z) result)
			     (push (imagpart z) result))
		     obj)
		(cons '(mlist simp) (nreverse result))))))
	((arrayp object)
	 (values
	  (array->complex-cl-vector object)
	  #'(lambda (obj)
	      (let ((result (make-array (* 2 (length obj)))))
		(loop for k from 0 by 2
		      for z across obj
		      do
			 (progn
			   (setf (aref result k) (realpart z))
			   (setf (aref result (1+ k)) (imagpart z))))
		result))))
	(t
	 (merror "inverse_real_fft: input is not a list or an array: ~M." object))))


;;; Bigfloat FFT
;;;
;;; The same set of FFT routines as for floats, but modified to return
;;; a bigfloat result.

;; Convert a sequence to an array of complex bigfloat numbers.  These
;; probably don't have to be very fast because the conversions to
;; bigfloat and back are relatively slow.
(defun seq->bfft-vector (seq)
  (map 'vector #'(lambda (z)
		   (destructuring-bind (rp . ip)
		       (risplit ($bfloat z))
		     (bigfloat:bigfloat rp ip)))
       seq))

(defun mlist->bfft-vector (mlist)
  (seq->bfft-vector (cdr mlist)))

(defun lisp-array->bfft-vector (arr)
  (seq->bfft-vector arr))

(defun bfft-vector->seq (arr seqtype)
    (map seqtype #'to arr))

(defun bfft-vector->mlist (arr)
  (cons '(mlist simp) (bfft-vector->seq arr 'list)))

(defun bfft-vector->lisp-array (arr)
  (bfft-vector->seq arr 'vector))

(defun bfft-vector->maxima-symbol-array (arr)
  "Outputs a Maxima array as does Maxima's 'array()' function."
  (let ((lisp-array (bfft-vector->lisp-array arr))
        (maxima-symbol (meval `(($array)
                                ,(intern (symbol-name (gensym "$G")))
                                $float
                                ,(1- (length arr))))))
    (setf (symbol-array (mget maxima-symbol 'maxima::array)) lisp-array)
    maxima-symbol))

(defun find-bf-fft-converter (input &optional (name "bf_fft"))
  (cond (($listp input)
	 (values
	  (mlist->bfft-vector input)
	  #'bfft-vector->mlist))
	((arrayp input)
	 (values
	  (lisp-array->bfft-vector input)
	  #'bfft-vector->lisp-array))
	((and (symbolp input) (symbol-array (mget input 'maxima::array)))
	 (values
	  (lisp-array->bfft-vector (symbol-array (mget input 'maxima::array)))
	  #'bfft-vector->maxima-symbol-array))
	(t
	 (merror "~A: input is not a list or an array: ~M." name input))))



(defun mlist->bf-rfft-vector (object)
  (let* ((n (length (cdr object)))
	 (z (make-array (ash n -1))))
    (loop for k from 0
	  for (re im) on (cdr object) by #'cddr
	  while im
	  do
	     (setf (aref z k) (complex ($float re) ($float im))))
    z))

(defun lisp-array->bf-rfft-vector (object)
  (let ((z (make-array (length object))))
    (loop for k of-type fixnum from 0
	  for x across object
	  do
	     (let ((fl (risplit ($float x))))
	       (setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun find-bf-rfft-converters (object)
  (cond (($listp object)
	 (values
	  (mlist->bf-rfft-vector object)
	  #'bfft-vector->mlist))
	((arrayp object)
	 (values
	  (lisp-array->bf-rfft-vector object)
	  #'bfft-vector->lisp-array))
	((and (symbolp object) (symbol-array (mget object 'maxima::array)))
	 (values
	  (lisp-array->bf-rfft-vector (symbol-array (mget object 'maxima::array)))
	  #'(lambda (array)
	      (let ((ar (bfft-vector->lisp-array array))
		    (sym (meval `(($array)
				  ,(intern (symbol-name (gensym "$G")))
				  $float
				  ,(1- (length array))))))
		(setf (symbol-array (mget sym 'maxima::array))
		      ar)))))
	(t
	 (merror "bf_rfft: input is not a list or an array: ~M." object))))

(defun find-bf-irfft-converters (object)
  (cond (($listp object)
	 (values
	  (mlist->bfft-vector object)
	  #'(lambda (obj)
	      (let (result)
		(map nil #'(lambda (z)
			     (push (to (bigfloat:realpart z)) result)
			     (push (to (bigfloat:imagpart z)) result))
		     obj)
		(cons '(mlist simp) (nreverse result))))))
	((arrayp object)
	 (values
	  (lisp-array->bfft-vector object)
	  #'(lambda (obj)
	      (let ((result (make-array (* 2 (length obj)))))
		(loop for k from 0 by 2
		      for z across obj
		      do
			 (progn
			   (setf (aref result k) (realpart z))
			   (setf (aref result (1+ k)) (imagpart z))))
		result))))
	(t
	 (merror "bf_inverse_real_fft: input is not a list or an array: ~M." object))))
  
;;; Bigfloat implementation of Fast Fourier Transform using a radix-2
;;; algorithm with inputs and outputs in natural order.
(in-package "BIGFLOAT")

;; This is an equal hash table with a key being a list of the order of
;; the FFT and fpprec.  We need fpprec in case the caller changes the
;; requested precision.
(defvar *bf-sincos-tables*
  (make-hash-table :test 'equal)
  "Hash table mapping log2 of the FFT size to an
  array of exp(2*pi*i/N), where N is the FFT size.")

(defun sincos-table (m)
  "For an FFT order of M, return an array of complex bigfloat twiddle
  factors for the FFT. The array contains the values
  exp(i*pi*k/2^(m-1))."
  (cond ((gethash (list m maxima::fpprec) *bf-sincos-tables*))
	(t
	 ;; Need to create the sincos table.  Only need to have a half
	 ;; period.
	 (let* ((n (ash 1 (1- m)))
		(p (/ (%pi (bigfloat:bigfloat 1))
		      n))
		(table (make-array n)))
	   (dotimes (k n)
	     (setf (aref table k) (cis (* k p))))
	   ;; Make the half point exactly correct
	   (when (> n 1)
	     (setf (aref table (ash n -1))
		   (bigfloat:bigfloat 0 1)))
	   (setf (gethash (list m maxima::fpprec) *bf-sincos-tables*)
		 table)
	   table))))
  
  
;; Fast Fourier Transform using a simple radix-2 implementation with
;; inputs and outputs in natural order.  The definition of the forward
;; FFT follows Maxima's definition of the forward FFT.  The input and
;; output are assumed to be bigfloats objects.
(defun fft-r2-nn (x &key (inverse-fft-p nil))
  "Compute the FFT of X which MUST be a specialzed vector of complex
  bigfloat's whose length MUST be a power of 2.  If INVERSE-FFT-P is
  non-NIL, the inverse FFT is computed.  The scaling by 1/N is
  included in the inverse FFT.

  The contents of the input X may be destroyed."
  (let* ((n (length x))
	 (half-n (ash n -1))
	 (pairs-in-group (ash n -1))
	 (number-of-groups 1)
	 (distance (ash n -1))
	 (not-switch-input t)
	 (sincos (sincos-table (maxima-fft:log-base2 n)))
	 (a (copy-seq x))
	 (b (make-array (length x))))

    (flet ((fft ()
	     (let ((index 0))
	       (dotimes (k number-of-groups)
		 (let* ((jfirst (* 2 k pairs-in-group))
			(jlast (+ jfirst pairs-in-group -1))
			(jtwiddle (* k pairs-in-group))
			(w (let ((w (aref sincos jtwiddle)))
			     (if inverse-fft-p
			       (conjugate w)
			       w))))

		   (loop for j from jfirst upto jlast do
		     (let ((temp (* w (aref a (+ j distance)))))
		       (setf (aref b index) (+ (aref a j) temp))
		       (setf (aref b (+ index half-n)) (- (aref a j) temp))
		       (incf index))))))))
      (loop while (< number-of-groups n) do
	(fft)
	(rotatef a b)
	(setf not-switch-input (not not-switch-input))
	(setf pairs-in-group (ash pairs-in-group -1))
	(setf number-of-groups (ash number-of-groups 1))
	(setf distance (ash distance -1)))
      (if inverse-fft-p
	  a
	  (map-into a #'(lambda (z)
			       (/ z n))
		    a)))))

