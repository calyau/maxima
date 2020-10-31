(defpackage :fftpack5
  (:use :common-lisp)
  (:export "RFFT"
	   "INVERSE-RFFT"
	   "CFFT"
	   "INVERSE-CFFT"))

(in-package :fftpack5)

(defvar *wsave-cache*
  (make-hash-table)
  "Cache for different wsave tables for single-precision.  The key is
  the FFT size; the value is a the wsave table needed by the FFT
  routines.")

(defvar *wsave-cache-double*
  (make-hash-table)
  "Cache for different wsave tables for double-precision FFTs.  The
  key is the FFT size; the value is a the wsave table needed by the
  FFT routines.")
  
(defun get-wsave-entry (n)
  "Get the wsave array and it's length that is needed for computing
  the (forward and inverse) FFTs.  The value is cached, so if it's the
  cache, return it.  Otherwise compute a new value and save it in the
  cache."
  (let ((entry (gethash n *wsave-cache*)))
    (if entry
	entry
	(let* ((lensav (+ n 4 (floor (log n 2))))
	       (wsave (make-array lensav :element-type 'double-float)))
	  (multiple-value-bind (ignore-0 ignore-1 ignore-2 ier)
	      (rfft1i n wsave lensav 0)
	    (declare (ignore ignore-0 ignore-1 ignore-2))
	    (unless (zerop ier)
	      ;; This shouldn't really ever happen.
	      (error "lensav is not big enough"))
	    (setf (gethash n *wsave-cache*) wsave)
	    wsave)))))

(defun convert-rfft (x)
  "Convert the output of FFTPACK RFFT1F (forward real FFT) into a more
  user-friendly format with complex values"
  (declare (type (simple-array double-float (*)) x))
  (let* ((n (length x))
	 (nhalf (floor (/ n 2)))
	 (out (make-array (+ 1 nhalf) :element-type '(complex double-float))))
    ;; If X is the transformed value, then the output from rfftf is:
    ;; 0: X(0) / N
    ;; 1: 2*realpart(X(1))/N
    ;; 2: 2*imagpart(X(1))/N
    ;; 3: 2*realpart(X(2))/N
    ;; 4: 2*realpart(X(2))/N
    ;; ...
    ;; N-1: X(N-1)/N
    ;; The last term exists only if N is even.
    (setf (aref out 0) (complex (aref x 0) 0.0))
    (loop for j from 1 to (if (evenp n) (1- nhalf) nhalf)
	  for k from 1 by 2
	  do
	     ;; Need to remove the factor of 2 that rfftf added.
	     (setf (aref out j) (complex (* 0.5 (aref x k))
					 (* 0.5 (aref x (1+ k))))))
    (when (evenp n)
      (setf (aref out nhalf)
	    (complex (aref x (1- n)) 0.0)))
    out))

(defun convert-inverse-rfft (x n)
  "Convert the complex-valued input, X, (that was produced by rfft)
  into the form needed by rfft1b.  The length of the transform, N, is
  needed because it cannot be uniquely determined from the length of
  X."
  (declare (type (simple-array (complex double-float) (*)) x))
  (let ((res (make-array n :element-type 'double-float)))
    (setf (aref res 0) (realpart (aref x 0)))
    (loop for j from 1 below (if (evenp n) (1- (length x)) (length x))
	  for k from 1 by 2
	  do
	     (let ((z (aref x j)))
	       ;; Put back the factor of 2 that we removed in rfft but
	       ;; is needed by rfft1b.
	       (setf (aref res k) (* 2 (realpart z)))
	       (setf (aref res (1+ k)) (* 2 (imagpart z)))))
    (when (evenp n)
      (setf (aref res (1- n)) (realpart (aref x (1- (length x))))))
    res))

(defun rfft (x)
  "Compute the real FFT of X.

   Let N be the length of X.  The FFT is:

    Y[n] = 1/N*sum(x[k] * exp(2*%i*%pi*k*n/N), k, 0, N-1)
  
   for n = 0, 1,...,floor(N/2)+1

   WARNING: This definition differs from the formula used for CFFT
   which uses exp(-2*%pi*%i*k*n/N)!"

  (declare (type (simple-array double-float (*)) x))
  (let* ((n (length x))
	 (work (make-array n :element-type 'double-float))
	 (wsave (get-wsave-entry n))
	 (lensav (length wsave)))
    (let ((ier
	    (nth-value 8
		       (rfft1f n 1 x n wsave lensav work n 0))))
      (unless (zerop ier)
	;; This should never happen because we always allocate enough
	;; space for x, wsave, and the work array.
	(error "rfft1f failed with code ~A" ier))
      (convert-rfft x))))

(defun inverse-rfft (x n)
  "Compute the inverse real FFT of X. N is the length of the transform
  (and not the length of X!).

   See RFFT for the definition of the FFT used in these routines."
  (declare (type (simple-array (complex double-float) (*)) x))

  ;; Check to see if N is consistent with the length of X.  However,
  ;; this isn't foolproof.  For a real FFT of length 5, the FFT array
  ;; has length 3.  But a real FFT of length 4 also has an array
  ;; length of 3.
  (unless (= (length x) (1+ (floor n 2)))
    (error "Length of X (~A) is not compatible with N (~D)"
	   (length x) n))

  (let* ((inv (convert-inverse-rfft x n))
	 (work (make-array n :element-type 'double-float))
	 (wsave (get-wsave-entry n))
	 (lensav (length wsave)))
    (let* ((ier
	     (progn
	       (nth-value 8
			  (rfft1b n 1 inv n wsave lensav work n 0)))))
      (unless (zerop ier)
	;; This should never happen because we should have always
	;; allocated the correct size of the arrays.
	(error "rfft1b failed with code ~A" ier))
      inv)))

(defun test-rfft (n &key verbose)
  "Test the rfft and inverse-rfft routines using a simple ramp of
  length n"
  ;; For testing, use a simple ramp: 1, 2, 3, ..., N.
  ;; With some care, we can derive the FFT for this is
  ;;
  ;; X[0] = (N + 1) / 2
  ;; X[n] = -1/2 - cot(%pi*n/N)/2, n = 1, N

  (let* ((x (make-array n :element-type 'double-float)))
    (loop for k from 0 below n
	  do
	     (setf (aref x k) (float (+ k 1) 1d0)))
    (let* ((xfrm (rfft x))
	   (xfrm-len (length xfrm))
	   (expected (make-array xfrm-len :element-type '(complex double-float)))
	   (noise-pwr 0d0)
	   (signal-pwr 0d0))
      (declare (double-float noise-pwr signal-pwr))

      (setf (aref expected 0) (complex (* 0.5d0 (+ n 1)) 0d0))
      (loop for k from 1 below xfrm-len
	    with omega = (coerce (/ pi n) 'double-float)
	    do
	       (setf (aref expected k)
		     (complex -0.5d0 (/ -0.5d0 (tan (* omega k))))))
      (when verbose
	(format t "Forward transform; actual vs expected~%")
	(loop for k from 0 below xfrm-len
	      do
		 (format t "~4d: ~A ~A~%" k (aref xfrm k) (aref expected k))))
      (incf noise-pwr (expt (abs (- (aref xfrm 0) (* 0.5 (+ n 1)))) 2))
      (incf signal-pwr (expt (* 0.5 (+ n 1)) 2))
      (loop for k from 1 below (length xfrm)
	    do
	       (incf noise-pwr (expt (abs (- (aref xfrm k)
					     (aref expected k)))
				     2))
	       (incf signal-pwr (expt (abs (aref expected k)) 2)))
      (let ((inv (inverse-rfft xfrm n))
	    (inv-noise-pwr 0.0)
	    (inv-signal-pwr 0.0))
	(when verbose
	  (format t "Inverse transform; actual vs expected~%")
	  (loop for k from 0 below n
		do
		   (format t "~4d: ~A ~A~%" k (aref inv k) (+ k 1))))
	(loop for k from 0 below n
	      do
		 (incf inv-noise-pwr (expt (- (aref inv k) (+ k 1)) 2))
		 (incf inv-signal-pwr (expt (coerce (+ k 1) 'double-float) 2)))
	(flet ((db (s n)
		 (if (zerop n)
		     1000d0
		     (* 10 (log (/ s n) 10)))))
	  (values (db signal-pwr noise-pwr) (db inv-signal-pwr inv-noise-pwr) noise-pwr signal-pwr inv-noise-pwr inv-signal-pwr))))))


(defun get-double-wsave-entry (n)
  "Get the wsave array and it's length that is needed for computing
  the (forward and inverse) FFTs.  The value is cached, so if it's the
  cache, return it.  Otherwise compute a new value and save it in the
  cache."
  (let ((entry (gethash n *wsave-cache-double*)))
    (if entry
	entry
	(let* ((lensav (+ (* 2 n) 4 (floor (log n 2))))
	       (wsave (make-array lensav :element-type 'double-float)))
	  (multiple-value-bind (ignore-0 ignore-1 ignore-2 ier)
	      (cfft1i n wsave lensav 0)
	    (declare (ignore ignore-0 ignore-1 ignore-2))
	    (unless (zerop ier)
	      ;; This shouldn't really ever happen.
	      (error "lensav is not big enough"))
	    (setf (gethash n *wsave-cache-double*) wsave)
	    wsave)))))

(defun cfft (x)
  "Compute the FFT of a complex array X

   Let N be the length of  X.  The FFT is:

     Y[n] = 1/N*sum(x[k] * exp(-2*%pi*%i*n*k/N), k = 0, N-1)

   for n = 0, 1,...,N -1

   WARNING: This definition differs from RFFT which has
   exp(+2*%pi*%i*n*k/N)"

  (declare (type (simple-array (complex double-float) (*)) x))
  (let* ((n (length x))
	 (lenwrk (* 2 n))
	 (work (make-array lenwrk :element-type 'double-float))
	 (wsave (get-double-wsave-entry n))
	 (lensav (length wsave)))
    (let ((ier
	    (nth-value 8
		       (cfft1f n 1 x n wsave lensav work lenwrk 0))))
      (unless (zerop ier)
	;; This should never happen because we always allocate enough
	;; space for x, wsave, and the work array.
	(error "rfft1f failed with code ~A" ier))
      x)))

(defun inverse-cfft (x)
  (declare (type (simple-array (complex double-float) (*)) x))

  (let* ((n (length x))
	 (lenwrk (* 2 n))
	 (work (make-array lenwrk :element-type 'double-float))
	 (wsave (get-double-wsave-entry n))
	 (lensav (length wsave)))
    (let* ((ier
	     (progn
	       (nth-value 8
			  (cfft1b n 1 x n wsave lensav work lenwrk 0)))))
      (unless (zerop ier)
	;; This should never happen because we should have always
	;; allocated the correct size of the arrays.
	(error "rfft1b failed with code ~A" ier))
      x)))

(defun test-cfft (n &key verbose)
  (let* ((x (make-array n :element-type '(complex double-float))))
    ;; The test signal is a simple ramp: 1, 2, 3,..., N.
    ;;
    ;; The analytical FFT for this is:
    ;;
    ;; X[0] = (N+1)/2
    ;; X[n] = -1/2 + %i*cot(%pi*n/N)/2
    (loop for k from 0 below n
	  do
	     (setf (aref x k) (complex (+ k 1) 0d0)))
    (let* ((xfrm (cfft x))
	   (xfrm-len (length xfrm))
	   (expected (make-array xfrm-len :element-type '(complex double-float)))
	   (noise-pwr 0d0)
	   (signal-pwr 0d0))
      (declare (double-float noise-pwr signal-pwr))

      (setf (aref expected 0) (complex (* 0.5d0 (+ n 1)) 0d0))
      (loop for k from 1 below xfrm-len
	    with omega = (coerce (/ pi n) 'double-float)
	    do
	       (setf (aref expected k)
		     (complex -0.5d0 (/ 0.5d0 (tan (* omega k))))))
      (when verbose
	(format t "Forward transform; actual vs expected~%")
	(loop for k from 0 below xfrm-len
	      do
		 (format t "~4d: ~A ~A~%" k (aref xfrm k) (aref expected k))))
      (incf noise-pwr (expt (abs (- (aref xfrm 0) (* 0.5 (+ n 1)))) 2))
      (incf signal-pwr (expt (* 0.5 (+ n 1)) 2))
      (loop for k from 1 below (length xfrm)
	    do
	       (incf noise-pwr (expt (abs (- (aref xfrm k)
					     (aref expected k)))
				     2))
	       (incf signal-pwr (expt (abs (aref expected k)) 2)))
      (let ((inv (inverse-cfft xfrm))
	    (inv-noise-pwr 0.0)
	    (inv-signal-pwr 0.0))
	(when verbose
	  (format t "Inverse transform; actual vs expected~%")
	  (loop for k from 0 below n
		do
		   (format t "~4d: ~A ~A~%" k (aref inv k) (+ k 1))))
	(loop for k from 0 below n
	      do
		 (incf inv-noise-pwr (expt (abs (- (aref inv k) (+ k 1))) 2))
		 (incf inv-signal-pwr (expt (coerce (+ k 1) 'double-float) 2)))
	(flet ((db (s n)
		 (if (zerop n)
		     1000d0
		     (* 10 (log (/ s n) 10)))))
	  (values (db signal-pwr noise-pwr) (db inv-signal-pwr inv-noise-pwr) noise-pwr signal-pwr inv-noise-pwr inv-signal-pwr))))))
  