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

(defun $fft (a)
  (let*
    ((a1 (cond
           ((and (symbolp a) (mget a 'array))
            (copy-seq (symbol-array (mget a 'array))))
           ((arrayp a)
            (copy-seq a))
           ((listp a)
            (copy-list a))
           (t (merror "fft: expected an array or list, found ~M instead." (type-of a)))))
     (a2 (make-empty-copy a1)))
    (cond
      ((arrayp a1)
       (dotimes (i (length a1))
         (let ((ri (risplit (aref a1 i))))
           (setf (aref a1 i) (car ri))
           (setf (aref a2 i) (cdr ri)))))
      (t
        (dotimes (i ($length a1))
          (let ((ri (risplit (nth (1+ i) a1))))
            (setf (nth (1+ i) a1) (car ri))
            (setf (nth (1+ i) a2) (cdr ri))))))
    (fft a1 a2)
    (cond
      ((arrayp a1)
       (dotimes (i (length a1))
         (setf (aref a1 i) (m+ (aref a1 i) (m* '$%i (aref a2 i)))))
       (if (symbolp a)
         (let ((b (make-empty-copy a)))
           (setf (symbol-array (mget b 'array)) a1)
           (setq a1 b))))
      (t
        (dotimes (i ($length a1))
          (setf (nth (1+ i) a1) (m+ (nth (1+ i) a1) (m* '$%i (nth (1+ i) a2)))))))
    a1))

(defun fft (a1 a2)
  (fft+ifft-common '$fft 'ifft a1 a2))

(defun fft+ifft-common (user-fcn-name lisp-fcn-name rary iary)
  (let*
    ((fast_rary (fft-arg-check user-fcn-name rary))
     (fast_iary (if iary (fft-arg-check user-fcn-name iary))))

    (when (null iary)
      (setq iary (make-empty-copy rary))
      (setq fast_iary (fft-arg-check user-fcn-name iary))
      ;; Only RARY was given as an argument.
      ;; Put imaginary part into IARY.
      (dotimes (i (length fast_rary))
        (let ((ri (risplit (aref fast_rary i))))
          (setf (aref fast_rary i) (car ri))
          (setf (aref fast_iary i) (cdr ri)))))

    ;; Try to ensure that all values are floating point numbers.
    (dotimes (i (length fast_rary))
      (setf (aref fast_rary i) ($float (aref fast_rary i)))
      (setf (aref fast_iary i) ($float (aref fast_iary i))))
    
    ;; fast_rary and fast_iary are lisp arrays (which is the same thing
    ;; as "fast" maxima arrays) and fft is supposed to modify them.
    (funcall lisp-fcn-name fast_rary fast_iary)

    ;; return the modified arrays in their original form (i.e. either "fast"
    ;; or not, depending) 
    (when ($listp rary)
      (copy-into-mlist fast_rary rary)
      (copy-into-mlist fast_iary iary))
    (list '(mlist) rary iary)))

(defun copy-into-mlist (a b)
  ;; I wonder if there's a better way. Oh well.
  (dotimes (i (length a)) (setf (nth (1+ i) b) (aref a i))))

(defun $inverse_fft (a)
  (let*
    ((a1 (cond
           ((and (symbolp a) (mget a 'array))
            (copy-seq (symbol-array (mget a 'array))))
           ((arrayp a)
            (copy-seq a))
           ((listp a)
            (copy-list a))
           (t (merror "fft: expected an array or list, found ~M instead." (type-of a)))))
     (a2 (make-empty-copy a1)))
    (cond
      ((arrayp a1)
       (dotimes (i (length a1))
         (let ((ri (risplit (aref a1 i))))
           (setf (aref a1 i) (car ri))
           (setf (aref a2 i) (cdr ri)))))
      (t
        (dotimes (i ($length a1))
          (let ((ri (risplit (nth (1+ i) a1))))
            (setf (nth (1+ i) a1) (car ri))
            (setf (nth (1+ i) a2) (cdr ri))))))
    (inverse_fft a1 a2)
    (cond
      ((arrayp a1)
       (dotimes (i (length a1))
         (setf (aref a1 i) (m+ (aref a1 i) (m* '$%i (aref a2 i)))))
       (if (symbolp a)
         (let ((b (make-empty-copy a)))
           (setf (symbol-array (mget b 'array)) a1)
           (setq a1 b))))
      (t
        (dotimes (i ($length a1))
          (setf (nth (1+ i) a1) (m+ (nth (1+ i) a1) (m* '$%i (nth (1+ i) a2)))))))
    a1))

(defun inverse_fft (a1 a2)
  (fft+ifft-common '$inverse_fft 'forward-fft a1 a2))

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

(defun log-base2 (n)
  (declare (type (and fixnum (integer 1)) n)
	   (optimize (speed 3)))
  ;; Just find m such that 2^m <= n < 2^(m+1).  It's up to the caller
  ;; to make sure that n is a power of two.  (The previous
  ;; implementation using (/ (log n) (log 2)) has roundoff errors.
  ;; This doesn't.)
  (1- (integer-length n)))

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
  (declare (type (cl:array t (*)) vec-r vec-i))
  (let* ((size (length vec-r))
	 (le size)
	 (m (log-base2 le))
	 (dir (if direction 1.0 -1.0)))
    (unless (= size (ash 1 m))
      (merror "fft: size of array must be a power of 2; found: ~:M" size))
    (dotimes (level m)
      (declare (fixnum level))
      (let* ((le1 (truncate le 2))
	     (ang (/ pi le1))
	     (w-r (cos ang))
	     (w-i (- (* dir (sin ang))))
	     (u-r 1.0)
	     (u-i 0.0)
	     (tmp-r 0.0)
	     (tmp-i 0.0)
	     (kp 0))
	(declare (type fixnum le1)
		 (type flonum ang)
		 (type flonum w-r w-i u-r u-i tmp-r tmp-i)
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
	    (setf (aref vec-i k) tmp-i))
	  (psetq u-r (- (* u-r w-r) (* u-i w-i))
		 u-i (+ (* u-r w-i) (* u-i w-r))))
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
	(setq k (ash k -1)))
      (setq j (+ j k)))))

(defun forward-fft (x-r x-i)
  "fft (x)

Forward DFT of x.  Result is returned in x."
  (let ((size (length x-r)))
    (when (>= size 2)
      (fft-dif-internal x-r x-i nil)
      (fft-bit-reverse x-r x-i))
    (values x-r x-i)))

(defun ifft (x-r x-i)
  "ifft (x)

Inverse DFT of x.  Result is returned in x."
  (let ((size (length x-r)))
    (if (>= size 2)
      (let ((f1size (/ (float size))))
        (fft-dif-internal x-r x-i t)
        (fft-bit-reverse x-r x-i)
        (dotimes (k size)
          (setf (aref x-r k) (* (aref x-r k) f1size))
          (setf (aref x-i k) (* (aref x-i k) f1size))))))
  (values x-r x-i))
  
