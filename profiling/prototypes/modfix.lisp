;; Round 2. NORMALIZED-MODULUS: fixnum fast path for MOD, which otherwise is a
;; full call to generic FLOOR.
(in-package :maxima)
(defun normalized-modulus (n)
  "Normalizes the number N with respect to MODULUS,
  returning a number in (-MODULUS/2, MODULUS/2]."
  (let* ((m modulus)
	 (rem (if (and (typep n 'fixnum) (typep m 'fixnum))
		  (mod (the fixnum n) (the fixnum m))
		  (mod n m))))
    (if (<= (* 2 rem) m)
      rem
      (- rem m))))
