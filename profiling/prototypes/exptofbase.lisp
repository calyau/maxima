;; Round 2. SIGNDIFF-SPECIAL calls EXPT-OF-BASE, which costs a MEQP each, only
;; after the cheaper sign tests that decide whether its result matters: Q and S
;; positive for the Q^R - S rule, Q positive and not 1 for the Q^m - Q^n rule.
(in-package :maxima)
(defun signdiff-special (xlhs xrhs)
  ;; xlhs may be a constant
  (let ((sgn nil) flip-sign)
    (when (or (and (realp xrhs) (minusp xrhs)
		   (not (atom xlhs)) (eq (sign* xlhs) '$pos))
					; e.g. sign(a^3+%pi-1) where a>0
	      (and (mexptp xlhs)
		   ;; e.g. sign(%e^x-1) where x>0
		   (eq (sign* (caddr xlhs)) '$pos)
		   (or (and
			;; Q^Rpos - S, S<=1, Q>1
			(member (sign* (sub 1 xrhs)) '($pos $zero $pz) :test #'eq)
			(eq (sign* (sub (cadr xlhs) 1)) '$pos))
		       (and (not (eq $domain '$complex))
			;; Qpos ^ Rpos - Spos => Qpos - Spos^(1/Rpos).
			;; Do NOT apply when Spos is itself a power of Qpos (e.g. S = Q):
			;; The reduction would just toggle the exponent R <-> 1/R and recurse forever.
			;; That same-base case is handled by the exponent-comparison rule further below.
			(eq (sign* (cadr xlhs)) '$pos)
			(eq (sign* xrhs) '$pos)
			(not (expt-of-base xrhs (cadr xlhs)))
			(eq (sign* (sub (cadr xlhs)
					(power xrhs (div 1 (caddr xlhs)))))
			    '$pos))))
	      (and (mexptp xlhs) (mexptp xrhs)
		   ;; Q^R - Q^T, Q>1, (R-T) > 0, with real R and T
		   ;; e.g. sign(2^x-2^y) where x>y
		   (alike1 (cadr xlhs) (cadr xrhs))
		   (zerop1 ($imagpart (caddr xlhs)))
		   (zerop1 ($imagpart (caddr xrhs)))
		   (eq (sign* (sub (cadr xlhs) 1)) '$pos)
		   (eq (sign* (sub (caddr xlhs) (caddr xrhs))) '$pos)))
      (setq sgn '$pos))
    
    ;; Swap XLHS and XRHS, if necessary, so that XLHS is the function with a
    ;; known range, or the abs for the clause further below, and remember to
    ;; flip the result.
    (flet ((ranged-p (e)
             (and (not (atom e))
                  (or (get (caar e) 'real-range) (eq (caar e) 'mabs)))))
      (when (and (ranged-p xrhs) (not (ranged-p xlhs)))
        (psetq xlhs xrhs xrhs xlhs flip-sign (not flip-sign))))

    ;; sign(f(x) - c) for a function f whose REAL-RANGE property gives the
    ;; bounds of its values.
    (when (and (null sgn) (not (atom xlhs)) (get (caar xlhs) 'real-range)
               (zerop1 ($imagpart (cadr xlhs))))
      (setq sgn (sign-from-range (get (caar xlhs) 'real-range) xrhs)))

    ;; signum takes no value strictly between -1 and 1 but 0, so
    ;; signum(x) - c is nonzero for a c known to lie there and to be nonzero.
    (when (and (null sgn) (not (atom xlhs)) (eq (caar xlhs) '%signum)
               (zerop1 ($imagpart (cadr xlhs)))
               (eq (sign* (add xrhs 1)) '$pos)
               (eq (sign* (sub 1 xrhs)) '$pos)
               (member (sign* xrhs) '($pos $neg $pn)))
      (setq sgn '$pn))
    
    ;; sign(abs(a) - b) = sign_max(sign(a - b), sign(-a - b)) with real a, real b
    (when (and (null sgn)
               (not (atom xlhs))
               (eq (caar xlhs) 'mabs)
               (zerop1 ($imagpart (cadr xlhs)))
               (zerop1 ($imagpart xrhs)))
      (let* ((a (cadr xlhs))
             (b xrhs)
             (s1 (sign* (sub a b)))
             (s2 (sign* (sub (neg a) b)))
             (max-sign (sminmax '$max s1 s2)))
        ;; abs(a) - b >= -b, which the signs of a - b and -a - b need not show:
        ;; for abs(abs(x) - 1) + 1, they are pz and pnz.
        (cond ((eq (sign* (neg b)) '$pos)
               (setq sgn '$pos))
              ((not (eq max-sign '$pnz))
               (setq sgn max-sign)))))
    
    ;; For the following test, swap XLHS and XRHS, if necessary, so that XRHS is
    ;; the number, e.g. x^2 - 3 -> 3 - x^2, and remember to flip the result.
    (when (and (null sgn) (mnump xlhs) (not (mnump xrhs)))
      (psetq xlhs xrhs xrhs xlhs flip-sign (not flip-sign)))
    
    ;; sign(a^pos_int - b) = sign((if evenp(pos_int) then abs(a) else a) - b^(1/pos_int))
    ;; with real a, real b (>= 0 for evenp(pos_int)), and b^(1/pos_int) being the real root
    (when (and (null sgn)
               (mnump xrhs)
               (mexptp xlhs)
               (integerp (caddr xlhs))
               (> (caddr xlhs) 0)
               (or (oddp (caddr xlhs)) (not (mnegp xrhs)))
               (zerop1 ($imagpart (cadr xlhs))))
      (let* ((exponent (caddr xlhs))
             (root (mul (if (mnegp xrhs) -1 1) (power (ftake 'mabs xrhs) (div 1 exponent))))
             (base (cadr xlhs))
             (maybe-abs-base (if (evenp exponent) (ftake 'mabs base) base))
             (diff-sign (sign* (sub maybe-abs-base root))))
        (when (not (eq diff-sign '$pnz))
          (setq sgn diff-sign))))
    
    ;; sign(Q^m - Q^n) for Q > 0 and real m, n, treating a bare base B as B^1.
    ;;     Q > 1 => sign(m - n),
    ;; 0 < Q < 1 => sign(n - m).
    ;; Resolves e.g. sign(x^a - x) = pos for x > 1, a > 1, and always terminates.
    (when (null sgn)
      (let ((q (cond ((mexptp xlhs) (cadr xlhs))
                     ((mexptp xrhs) (cadr xrhs)))))
        (when q
          (let ((qcmp (and (eq (sign* q) '$pos) (sign* (sub q 1)))))
            (when (member qcmp '($pos $neg))
              (let ((m (expt-of-base xlhs q))
                    (n (expt-of-base xrhs q)))
                (when (and m n
                           (zerop1 ($imagpart m))
                           (zerop1 ($imagpart n)))
                  (let ((diff-sign (if (eq qcmp '$pos)
                                       (sign* (sub m n))
                                       (sign* (sub n m)))))
                    (unless (eq diff-sign '$pnz)
                      (setq sgn diff-sign))))))))))

    (when (and (null sgn) $useminmax (or (minmaxp xlhs) (minmaxp xrhs)))
      (setq sgn (signdiff-minmax xlhs xrhs)))
    (when sgn (setq sign (if flip-sign (flip sgn) sgn) minus nil odds nil evens nil)
	  t)))

;;; Look for symbols with an assumption a > n or a < -n, where n is a number.
;;; For this case shift the symbol a -> a+n in a summation and multiplication.
;;; This handles cases like a>1 and b>1 gives sign(a+b-2) -> pos.

