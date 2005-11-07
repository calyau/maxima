;; -*- Lisp -*-

(in-package :maxima)

(defun mgetarray (marg) 
"Return the lisp array which is somehow attached to MARG."
  (or (and (symbolp marg) (symbol-array (mget marg 'array)))
      (and (arrayp marg) marg)))

(defun fft-arg-check (ary)
  ;; I don't check here if this is really a floating point array.  For maxima
  ;; arrays which are symbols this would be no problem since the type is on
  ;; the property list.  On the other hand, for "fast" arrays (i.e. lisp
  ;; arrays), using ARRAY-ELEMENT-TYPE might not be too useful.
  (or (mgetarray ary)
      (merror
       "arg ~M to fft//ift//recttopolar//polartorect must be floating point array" ary)))

;; I assume that the aguments of $fft are maxima arrays to be modified,
;; whereas the arguments of fft are lisp arrays to be modified.
(defun $fft (rary iary)
  (let ((fast_rary (fft-arg-check rary))
	(fast_iary (fft-arg-check iary)))
    ;; fast_rary and fast_iary are lisp arrays (which is the same thing
    ;; as "fast" maxima arrays) and fft is supposed to modify them.
    (ifft fast_rary fast_iary)
    ;; return the modified arrays in their original form (i.e. either "fast"
    ;; or not, depending) 
    (list '(mlist) rary iary)))

(defun $ift (rary iary)
  (let ((fast_rary (fft-arg-check rary))
	(fast_iary (fft-arg-check iary)))
    ;; fast_rary and fast_iary are lisp arrays (which is the same thing
    ;; as "fast" maxima arrays) and fft is supposed to modify them.
    (fft fast_rary fast_iary)
    ;; return the modified arrays in their original form (i.e. either "fast"
    ;; or not, depending) 
    (list '(mlist) rary iary)))

(defun $recttopolar (rary iary)
  (let ((fast-rary (fft-arg-check rary))
	(fast-iary (fft-arg-check iary)))
    (complex-to-polar fast-rary fast-iary)
    (list '(mlist) rary iary)))

(defun $polartorect (rary iary)
  (let ((fast-rary (fft-arg-check rary))
	(fast-iary (fft-arg-check iary)))
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

(defun log-base2 (n)
  (declare (type (and fixnum (integer 1)) n)
	   (optimize (speed 3)))
  (values (truncate (/ (log (float n 1d0)) #.(log 2d0)))))

;; Warning: What this routine thinks is the foward or inverse
;; direction is the "engineering" definition used in Oppenheim and
;; Schafer.  This is usually the opposite of the definitions used in
;; math, and is the opposite used by maxima.
;;
;; Scaling and bit-reversed ordering is not done by this routine.
(defun fft-dif-internal (vec-r vec-i &optional direction)
  "Internal FFT routine for decimation-in-frequency FFT
fft-dif-internal (vec &optional direction)

	vec		-- simple-array of elements.
	direction	-- NIL for forward, non-NIL for inverse
			   Default is forward.

The result is returned in vec."
  (declare (type (array t (*)) vec-r vec-i))
  (let* ((size (length vec-r))
	 (le size)
	 (m (log-base2 le))
	 (dir (if direction 1d0 -1d0)))
    (assert (= size (ash 1 m)))
    (dotimes (level m)
      (declare (fixnum level))
      (let* ((le1 (truncate le 2))
	     (ang (/ pi le1))
	     (w-r (cos ang))
	     (w-i (- (* dir (sin ang))))
	     (u-r 1d0)
	     (u-i 0d0)
	     (tmp-r 0d0)
	     (tmp-i 0d0)
	     (kp 0)
	     )
	(declare (type fixnum le1)
		 (type double-float ang)
		 (type double-float w-r w-i u-r u-i tmp-r tmp-i)
		 (type fixnum kp))
	(dotimes (j le1)
	  (declare (fixnum j))
	  (do ((k j (+ k le)))
	      ((>= k size))
	    (declare (fixnum k))
	    (setq kp (+ k le1))
	    #|
	    (setq tmp (+ (aref vec k) (aref vec kp)))
	    (setf (aref vec kp) (* u (- (aref vec k) (aref vec kp))))
	    (setf (aref vec k) tmp)
	    |#
	    (setq tmp-r (+ (aref vec-r k) (aref vec-r kp)))
	    (setq tmp-i (+ (aref vec-i k) (aref vec-i kp)))
	    (let* ((diff-r (- (aref vec-r k) (aref vec-r kp)))
		   (diff-i (- (aref vec-i k) (aref vec-i kp))))
	    
	      (psetf (aref vec-r kp) (- (* u-r diff-r) (* u-i diff-i))
		     (aref vec-i kp) (+ (* u-r diff-i) (* u-i diff-r))))
	    (setf (aref vec-r k) tmp-r)
	    (setf (aref vec-i k) tmp-i)
	    )
	  (psetq u-r (- (* u-r w-r) (* u-i w-i))
		 u-i (+ (* u-r w-i) (* u-i w-r)))
	  )
	(setq le le1))))
  (values vec-r vec-i))


(defun fft-bit-reverse (vec-r vec-i)
  "fft-bit-reverse (vec)

Reorder vec in bit-reversed order.  The length of vec
must be a power of 2."
  (let* ((size (length vec-r))
	 (n/2 (/ size 2))
	 (j 0)
	 (k 0))
    (declare (type fixnum size)
	     (type fixnum n/2)
	     (type fixnum j)
	     (type fixnum k))
    (dotimes (i (- size 1))
      (declare (fixnum i))
      (when (< i j)
	(rotatef (aref vec-r i) (aref vec-r j))
	(rotatef (aref vec-i i) (aref vec-i j)))
      (setq k n/2)
      (do* ()
	  ((> k j))
	(setq j (- j k))
	(setq k (ash k -1))
	)
      (setq j (+ j k)))))

(defun fft (x-r x-i)
  "fft (x)

Forward DFT of x.  Result is returned in x."
  (let ((size (length x-r)))
    (fft-dif-internal x-r x-i nil)
    (fft-bit-reverse x-r x-i)
    (values x-r x-i)
    ))

(defun ifft (x-r x-i)
  "ifft (x)

Inverse DFT of x.  Result is returned in x."
  (let* ((len (length x-r))
	 (flen (/ (float len 1d0))))
    (fft-dif-internal x-r x-i t)
    (fft-bit-reverse x-r x-i)
    (dotimes (k len)
      (setf (aref x-r k) (* (aref x-r k) flen))
      (setf (aref x-i k) (* (aref x-i k) flen))))
  (values x-r x-i))
  
