;; -*- Lisp -*-

(in-package :maxima)

(defun mgetarray (marg) 
"Return the lisp array which is somehow attached to MARG."
  (or (and (symbolp marg) (symbol-array (mget marg 'array)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DATA CONVERSION FUNCTIONS for fft
;;
;; These convert various possible arguments for $fft:
;; 1. maxima list
;; 2. lisp array
;; 3. 'maxima array'
;; into two 'flonum' lisp arrays ('fft-arrays') holding
;; real and imaginary parts. After fft is done, these
;; two arrays are converted back into the same data type
;; the $fft function was given.
;;
;; Real and imaginary parts are extracted with:
;; (risplit ($float `maxima-expression'))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mlist->fft-arrays (mlist)
  "Converts a maxima list into two Lisp flonum
arrays - to be used by FFT algorithm"
  (let* ((lst (rest mlist))
         (N (length lst))
         (realparts (make-array N :element-type 'flonum))
         (imagparts (make-array N :element-type 'flonum)))
        (do ((element lst (rest element))
             (index 0 (1+ index)))
          ;; iteration end:
          ((>= index N) (list realparts imagparts)) ;; return arrays
          ;; do this every iteration
          (let ((fl (risplit ($float (first element)))))
            (setf (aref realparts index) (coerce (car fl) 'flonum))
            (setf (aref imagparts index) (coerce (cdr fl) 'flonum))))))

(defun lisp-array->fft-arrays (arr)
  (let* ((N (length arr))
         (realparts (make-array N :element-type 'flonum))
         (imagparts (make-array N :element-type 'flonum)))
    (dotimes (index N)
      (let ((fl (risplit ($float (aref arr index)))))
        (setf (aref realparts index) (coerce (car fl) 'flonum))
        (setf (aref imagparts index) (coerce (cdr fl) 'flonum))))
    (list realparts imagparts)))

;; Backwards data conversion (from fft arrays)

(defun fft-arrays->mlist (realparts imagparts)
  "Takes two Lisp arrays with real and imaginary
parts and returns a Maxima list of complex numbers
in Maxima's 'format'."
  (let (ans)
    (dotimes (i (length realparts))
      (push (add (aref realparts i) (mul (aref imagparts i) '$%i))
            ans))
    (cons '(mlist simp) (nreverse ans))))

(defun fft-arrays->lisp-array (realparts imagparts)
  "Outputs a Lisp array of Maxima expressions."
  (let ((ans (make-array (length realparts))))
    (dotimes (i (length realparts))
      (setf (aref ans i)
            (add (aref realparts i) (mul (aref imagparts i) '$%i))))
    ans))

(defun fft-arrays->maxima-symbol-array (realparts imagparts)
  "Outputs a Maxima array as does Maxima's 'array()' function."
  (let ((lisp-array (fft-arrays->lisp-array realparts imagparts))
        (maxima-symbol (meval `(($array)
                                ,(intern (symbol-name (gensym "$G")))
                                $float
                                ,(1- (length realparts))))))
    (setf (symbol-array (mget maxima-symbol 'array)) lisp-array)
    maxima-symbol))

;;
;; main function used by both fft() and inverse_fft()
;;
(defun fft+ifft-common (input lisp-function-to-call maxima-function-name)
  "This function checks the type of input argument,
does the appropriate conversion to `fft arrays', calls
the list function given and converts the result back
into the original datatype of `input'"
  (multiple-value-bind (convert reverse-convert)
      ;; set the conversion functions
      (cond (($listp input)
	     (values #'mlist->fft-arrays
		     #'fft-arrays->mlist))
	    ((arrayp input)
	     (values  #'lisp-array->fft-arrays
		      #'fft-arrays->lisp-array))
	    ((and (symbolp input) (symbol-array (mget input 'array)))
	     (values #'(lambda (x)
			 (lisp-array->fft-arrays
			  (symbol-array (mget x 'array))))
		     #'fft-arrays->maxima-symbol-array))
	    (t
	     (merror "~A: input is not a list or an array." maxima-function-name)))
    (multiple-value-bind (realparts imagparts)
	;; perform fft or inverse fft
	(apply lisp-function-to-call (funcall convert input))
      ;; return the same data type as was the input
      (funcall reverse-convert realparts imagparts))))

;;
;; Maxima functions fft() and inverse_fft()
;;

#+nil
(defun $fft (input)
  (fft+ifft-common input #'forward-fft "fft"))

#+nil
(defun $inverse_fft (input)
  (fft+ifft-common input #'inverse-fft "inverse_fft"))

;; These functions perhaps need revision if they are
;; needed at all. Output ignores the type of input.
(defun $recttopolar (rary iary)
  (let ((fast-rary (fft-arg-check '$recttopolar rary))
	(fast-iary (fft-arg-check '$recttopolar iary)))
    (complex-to-polar fast-rary fast-iary)
    (list '(mlist) rary iary)))

(defun $polartorect (rary iary)
  (let ((fast-rary (fft-arg-check '$polartorect rary))
	(fast-iary (fft-arg-check '$polartorect iary)))
    (polar-to-complex fast-rary fast-iary)
    (list '(mlist) rary iary)))

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

;;; FFT written by Raymond Toy, based on the Fortran version in
;;; Oppenheim and Schaffer.  The original version used an array of
;;; complex numbers, but this causes quite a bit of consing if your
;;; Lisp doesn't handle them well.  (CMUCL does handle complex numbers
;;; well.)  So, take two separate arrays, one for the real part and
;;; one for the imaginary part.

(declaim (inline log-base2))
(defun log-base2 (n)
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

;; Warning: What this routine thinks is the foward or inverse
;; direction is the "engineering" definition used in Oppenheim and
;; Schafer.  This is usually the opposite of the definitions used in
;; math, and is the opposite used by maxima.
;;
;; Scaling and bit-reversed ordering is not done by this routine.
#+nil
(defun fft-dif-internal (vec-r vec-i &optional direction)
  "Internal FFT routine for decimation-in-frequency FFT
fft-dif-internal (vec &optional direction)

	vec		-- simple-array of elements.
	direction	-- NIL for forward, non-NIL for inverse
			   Default is forward.

The result is returned in vec."
  (declare (type (simple-array flonum (*)) vec-r vec-i)
	   (optimize speed))
  (let* ((size (length vec-r))
	 (le size)
	 (m (log-base2 le))
	 (sincos (sincos-table m))
	 (dir (if direction 1 -1)))
    (declare (fixnum size le)
	     (type (simple-array #+(and cmu flonum-double-double) (complex double-double-float)
				 #-flonum-double-double (complex double-float)
				 (*))
		   sincos))
    (unless (= size (ash 1 m))
      (merror "fft: size of array must be a power of 2; found: ~:M" size))
    (loop for level of-type fixnum from 0 below m
	  for repetition of-type fixnum = 1 then (ash repetition 1)
	  do
	  (let* ((le1 (truncate le 2)))
	    (declare (type fixnum le1))
	    (loop for j of-type fixnum from 0 below le1
	       for phase of-type fixnum from 0 by repetition
	       do
	       (let* ((u (aref sincos phase))
		      (u-r (realpart u))
		      (u-i (* dir (imagpart u))))
		 (declare (type #+(and cmu flonum-double-double) (complex double-double-float)
				#-flonum-double-double (complex double-float)
				u)
			  (type flonum u-r u-i))
		 (loop for k of-type fixnum from j below size by le
		    do
		    (let* ((kp (+ k le1))
			   (tmp-r (+ (aref vec-r k) (aref vec-r kp)))
			   (tmp-i (+ (aref vec-i k) (aref vec-i kp)))
			   (diff-r (- (aref vec-r k) (aref vec-r kp)))
			   (diff-i (- (aref vec-i k) (aref vec-i kp))))
		      (declare (fixnum kp)
			       (type flonum tmp-r tmp-i))
		      (psetf (aref vec-r kp) (- (* u-r diff-r) (* u-i diff-i))
			     (aref vec-i kp) (+ (* u-r diff-i) (* u-i diff-r)))
		      (setf (aref vec-r k) tmp-r)
		      (setf (aref vec-i k) tmp-i)))))
	    (setq le le1))))
  (values vec-r vec-i))

#+nil
(defun fft-bit-reverse (vec-r vec-i)
  "fft-bit-reverse (vec)

Reorder vec in bit-reversed order.  The length of vec
must be a power of 2."
  (declare (type (simple-array flonum (*)) vec-r vec-i)
	   (optimize speed))
  (let* ((size (length vec-r))
	 (n/2 (/ size 2))
	 (j 0)
	 (k 0))
    (declare (type fixnum size n/2 j k))
    (dotimes (i (- size 1))
      (declare (fixnum i))
      (when (< i j)
	(rotatef (aref vec-r i) (aref vec-r j))
	(rotatef (aref vec-i i) (aref vec-i j)))
      (setq k n/2)
      (do* ()
	  ((> k j))
	(setq j (- j k))
	(setq k (ash k -1)))
      (setq j (+ j k)))))

#+nil
(defun forward-fft (x-real x-imag)
  "forward-fft
Takes two lisp arrays, one for real parts
and the other for imaginary parts.
Returns transformed real and imaginary arrays.
A normalisation is performed."
  (let ((size (length x-real)))
    (when (> size 1)
      (fft-dif-internal x-real x-imag t)
      (fft-bit-reverse x-real x-imag)
      (let ((1/N (/ (coerce 1 'flonum) size)))
        (dotimes (k size) (setf (aref x-real k) (* (aref x-real k) 1/N)))
        (dotimes (k size) (setf (aref x-imag k) (* (aref x-imag k) 1/N)))))
    (values x-real x-imag)))

#+nil
(defun inverse-fft (x-real x-imag)
  "inverse-fft
Takes two lisp arrays, one for real parts
and the other for imaginary parts.
Returns transformed real and imaginary arrays."
  (let ((size (length x-real)))
    (when (> size 1)
      (fft-dif-internal x-real x-imag nil)
      (fft-bit-reverse x-real x-imag)))
  (values x-real x-imag))

(defun mlist->complex-cl-array (object)
  (let ((z (make-array (1- (length object)) :element-type '(complex double-float))))
    (loop for k of-type fixnum from 0
	  for x in (cdr object)
	  while x
	  do
	     (let ((fl (risplit ($float x))))
	       (setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun complex-cl-array->mlist (array)
  (declare (type (simple-array (complex double-float) (*)) array))
  (cons '(mlist simp)
	(loop for w of-type (complex double-float) across array
	      collect (add (realpart w)
			   (mul (imagpart w) '$%i)))))

(defun array->complex-cl-array (array)
  (let* ((n (length array))
	 (z (make-array n :element-type '(complex double-float))))
    (dotimes (k n)
      (let ((fl (risplit ($float (aref array k)))))
	(setf (aref z k) (complex (car fl) (cdr fl)))))
    z))

(defun complex-cl-array->vector (array)
  (declare (type (simple-array (complex double-float) (*)) array))
  (let* ((n (length array))
	 (z (make-array n)))
    (dotimes (k n)
      (let ((item (aref array k)))
	(setf (aref z k) (add (realpart item)
			      (mul '$%i (imagpart item))))))
    z))
  
(defun find-complex-converters (object maxima-function-name)
  (cond (($listp object)
	 (values
	  (mlist->complex-cl-array object)
	  #'complex-cl-array->mlist))
	((arrayp object)
	 (values
	  (array->complex-cl-array object)
	  #'complex-cl-array->vector))
	((and (symbolp object) (symbol-array (mget object 'array)))
	 (values
	  (let* ((sym-array (symbol-array (mget object 'array))))
	    (array->complex-cl-array sym-array))
	  #'(lambda (array)
	      (let ((ar (complex-cl-array->vector array))
		    (sym (meval `(($array)
				  ,(intern (symbol-name (gensym "$G")))
				  $float
				  ,(1- (length array))))))
		(setf (symbol-array (mget sym 'array))
		      ar)
		sym))))
	(t
	 (merror "~A: input is not a list or an array." maxima-function-name))))

(defun fft-r2-nn (x &key (inverse-fft-p nil))
  (declare (type (simple-array (complex double-float) (*)) x))
  (let* ((n (length x))
	 (half-n (ash n -1))
	 (pairs-in-group (ash n -1))
	 (number-of-groups 1)
	 (distance (ash n -1))
	 (not-switch-input t)
	 (sincos (sincos-table (log-base2 n)))
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

(defun $fft (input)
  (multiple-value-bind (z from-lisp)
      (find-complex-converters input "$fftip")
    (let* ((size (length z))
	   (order (log-base2 size)))
      (unless (= size (ash 1 order))
	(merror "fft: size of input must be a power of 2, not ~M" size))
      (when (> (length z) 1)
	(setf z (fft-r2-nn z)))
      (funcall from-lisp z))))

(defun $inverse_fft (input)
  (multiple-value-bind (z from-lisp)
      (find-complex-converters input "$fftip")
    (let* ((size (length z))
	   (order (log-base2 size)))
      (unless (= size (ash 1 order))
	(merror "inverse_fft: size of input must be a power of 2, not ~M" size))
      (when (> (length z) 1)
	(setf z (fft-r2-nn z :inverse-fft-p t)))
      (funcall from-lisp z))))

(defun mlist->rfft-array (object)
  (let* ((n (length (cdr object)))
	 (z (make-array (ash n -1) :element-type '(complex double-float))))
    (loop for k from 0
	  for (re im) on (cdr object) by #'cddr
	  while im
	  do
	     (setf (aref z k) (complex ($float re) ($float im))))
    z))

(defun rfft-array->mlist (array)
  (cons '(mlist simp)
	(map 'list #'(lambda (z)
		       (add (realpart z)
			    (mul '$%i (imagpart z))))
	     array)))

(defun array->rfft-array (array)
  (let* ((n (length array))
	 (z (make-array (ash n -1) :element-type '(complex double-float))))
    (loop for k from 0 below n by 2
	  for m from 0
	  do
	     (setf (aref z m) (complex ($float (aref array k))
				       ($float (aref array (1+ k))))))
    z))
  
(defun find-rfft-converters (object maxima-function-name)
  (cond (($listp object)
	 (values
	  (mlist->rfft-array object)
	  #'complex-cl-array->mlist))
	((arrayp object)
	 (values
	  (let ((z (make-array  (length object) :element-type '(complex double-float))))
	    (loop for k of-type fixnum from 0
		  for x across object
		  do
		     (let ((fl (risplit ($float x))))
		       (setf (aref z k) (complex (car fl) (cdr fl)))))
	    z)
	  #'complex-cl-array->vector))
	((and (symbolp object) (symbol-array (mget object 'array)))
	 (values
	  (array->rfft-array (symbol-array (mget object 'array)))
	  #'(lambda (array)
	      (let ((ar (complex-cl-array->vector array))
		    (sym (meval `(($array)
				  ,(intern (symbol-name (gensym "$G")))
				  $float
				  ,(1- (length array))))))
		(setf (symbol-array (mget sym 'array))
		      ar)))))
	(t
	 (merror "~A: input is not a list or an array." maxima-function-name))))

(defun $rfft (input)
  (multiple-value-bind (z from-lisp)
      (find-rfft-converters input "rfft")
    (declare (type (simple-array (complex double-float) (*)) z))
    (let* ((n (ash (length z) 1))
	   (result (make-array (1+ (length z)) :element-type '(complex double-float))))
      (declare (type (simple-array (complex double-float) (*)) result))

      (when (< n 3)
	(return-from $rfft ($fft input)))
      (locally
	  (declare (optimize (speed 3) (safety 0)))
	;; Compute FFT of shorter complex vector.  NOTE: the result
	;; returned by fft has scaled the output by the length of
	;; z.  That is, divided by n/2.  For our final result, we want to
	;;     divide by n, so in the following bits of code, we have an
	;;     extra factor of 2 to divide by.
	(setf z (fft-r2-nn z))

	;;(format t "z = ~A~%" z)
	;; Reconstruct the FFT of the original from the parts
	(setf (aref result 0)
	      (complex (* 0.5
			  (+ (realpart (aref z 0))
			     (imagpart (aref z 0))))))

	(let ((sincos (sincos-table (log-base2 n)))
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

(defun find-irfft-converters (object maxima-function-name)
  (cond (($listp object)
	 (values
	  (mlist->complex-cl-array object)
	  #'(lambda (obj)
	      (let (result)
		(map nil #'(lambda (z)
			     (push (realpart z) result)
			     (push (imagpart z) result))
		     obj)
		(cons '(mlist simp) (nreverse result))))))
	((arrayp object)
	 (values
	  (array->complex-cl-array object)
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
	 (merror "~A: input is not a list or an array." maxima-function-name))))

(defun $inverse_rfft (input)
  (multiple-value-bind (ft from-lisp)
      (find-irfft-converters input "inverse_rfft")
    (declare (type (simple-array (complex double-float) (*)) ft))
    (let* ((n (1- (length ft)))
	   (order (log-base2 n))
	   (sincos (sincos-table (1+ order)))
	   (z (make-array n :element-type '(complex double-float))))
      (declare (type (simple-array (complex double-float) (*)) sincos))

      (unless (= n (ash 1 order))
	(merror "inverse_rfft: input length must be one more than a power of two, not ~M" (1+ n)))

      (locally
	  (declare (optimize (speed 3)))
	(loop for k from 0 below n
	      do
		 (let ((evenpart (+ (aref ft k)
				    (conjugate (aref ft (- n k)))))
		       (oddpart (* (- (aref ft k)
				      (conjugate (aref ft (- n k))))
				   (conjugate (aref sincos k)))))
		   (setf (aref z k) (+ evenpart
				       (* #c(0 1d0) oddpart)))))

	(setf z (fft-r2-nn z :inverse-fft-p t)))
      (funcall from-lisp z))))

;;; Bigfloat FFT

;; Convert a sequence to an array of complex bigfloat numbers.
(defun seq->bfft-array (seq)
  (map 'vector #'(lambda (z)
		   (destructuring-bind (rp . ip)
		       (risplit ($bfloat z))
		     (bigfloat:bigfloat rp ip)))
       seq))

(defun mlist->bfft-array (mlist)
  (seq->bfft-array (cdr mlist)))

(defun lisp-array->bfft-array (arr)
  (seq->bfft-array arr))

(defun bfft-array->seq (arr seqtype)
    (map seqtype #'to arr))

(defun bfft-array->mlist (arr)
  (cons '(mlist simp) (bfft-array->seq arr 'list)))

(defun bfft-array->lisp-array (arr)
  (bfft-array->seq arr 'vector))

(defun bfft-array->maxima-symbol-array (arr)
  "Outputs a Maxima array as does Maxima's 'array()' function."
  (let ((lisp-array (bfft-array->lisp-array arr))
        (maxima-symbol (meval `(($array)
                                ,(intern (symbol-name (gensym "$G")))
                                $float
                                ,(1- (length arr))))))
    (setf (symbol-array (mget maxima-symbol 'array)) lisp-array)
    maxima-symbol))

;; Bigfloat forward and inverse FFTs.  Length of the input must be a
;; power of 2.
(defun $bf_fft (input)
  (multiple-value-bind (convert unconvert)
      (cond (($listp input)
	     (values #'mlist->bfft-array
		     #'bfft-array->mlist))
	    ((arrayp input)
	     (values #'lisp-array->bfft-array
		     #'bfft-array->lisp-array))
	    ((and (symbolp input) (symbol-array (mget input 'array)))
	     (values #'(lambda (x)
			 (lisp-array->bfft-array
			  (symbol-array (mget x 'array))))
		     #'bfft-array->maxima-symbol-array))
	    (t
	     (merror "bf_fft: input is not a list or an array.")))
    (let* ((x (funcall convert input)))
      (if (> (length x) 1)
	  (funcall unconvert (bigfloat::fft-r2-nn x))
	  (funcall unconvert x)))))

(defun $bf_inverse_fft (input)
  (multiple-value-bind (convert unconvert)
      (cond (($listp input)
	     (values #'mlist->bfft-array
		     #'bfft-array->mlist))
	    ((arrayp input)
	     (values #'lisp-array->bfft-array
		     #'bfft-array->lisp-array))
	    ((and (symbolp input) (symbol-array (mget input 'array)))
	     (values #'(lambda (x)
			 (lisp-array->bfft-array
			  (symbol-array (mget x 'array))))
		     #'bfft-array->maxima-symbol-array))
	    (t
	     (merror "bf_fft: input is not a list or an array.")))
    (let ((x (funcall convert input)))
      (if (> (length x) 1)
	  (funcall unconvert (bigfloat::fft-r2-nn x :inverse-fft-p t))
	  (funcall unconvert x)))))

(defun $bf_rfft (input)
  (let* ((n (length (cdr input)))
	 (z (make-array (ash n -1)))
	 (result (make-array (1+ (length z)))))
    (loop for k from 0
	  for (re im) on (cdr input) by #'cddr
	  while im
	  do (setf (aref z k) (bigfloat:bigfloat re im)))
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

    (let ((sincos (bigfloat::sincos-table (maxima::log-base2 n)))
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
      (bfft-array->mlist result))))

(defun $bf_inverse_rfft (input)
  (let* ((n (1- (length (cdr input))))
	 (ft (mlist->bfft-array input))
	 (z (make-array n))
	 (order (log-base2 n))
	 (sincos (sincos-table (1+ order))))

    (unless (= n (ash order 1))
      (merror "bf_inverse_fft: input length must be one more than a power of two, not ~M" (1+ n)))

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
    (let ((inverse (bigfloat::fft-r2-nn z :inverse-fft-p t))
	  result)
      (map nil #'(lambda (z)
		   (push (bigfloat:realpart z) result)
		   (push (bigfloat:imagpart z) result))
	   inverse)
      (cons '(mlist simp) (nreverse result)))))

(in-package "BIGFLOAT")

(defvar *bf-sincos-tables*
  (make-hash-table :test 'equal)
  "Hash table mapping log2 of the FFT size to an
  array of exp(2*pi*i/N), where N is the FFT size.")

(defun sincos-table (m)
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
  
  
;; Simple Radix-2 out-of-place FFT with in-order input and output.
(defun fft-r2-nn (x &key (inverse-fft-p nil))
  (let* ((n (length x))
	 (half-n (ash n -1))
	 (pairs-in-group (ash n -1))
	 (number-of-groups 1)
	 (distance (ash n -1))
	 (not-switch-input t)
	 (sincos (sincos-table (maxima::log-base2 n)))
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

