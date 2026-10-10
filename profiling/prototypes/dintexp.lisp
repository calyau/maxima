;; Round 3. INTCV inverts a change of variable with SOLVE. When the exponent
;; (DINTEXP) or log argument (LOGX1) is a cubic or quartic in the variable,
;; SOLVE answers with the cubic or quartic formula, and INTCV2 then spends
;; seconds ratsimping the integrand rewritten in those radicals, only for the
;; integral to fail. Skip roots with nested radicals in YX.
(in-package :maxima)
(declare-top (special *roots *failures))

(defun nested-radical-p (e var)
  "True if E contains a radical in VAR, that is a power of an expression in
  VAR to a non-integer rational exponent, whose base contains another one."
  (labels ((radical-p (e)
             (and (mexptp e) (ratnump (caddr e)) (among var (cadr e))))
           (has-radical-p (e)
             (and (consp e) (consp (car e))
                  (or (radical-p e) (some #'has-radical-p (cdr e)))))
           (nested-p (e)
             (and (consp e) (consp (car e))
                  (or (and (radical-p e) (has-radical-p (cadr e)))
                      (some #'nested-p (cdr e))))))
    (nested-p e)))

(defun intcv (nv flag ivar ll ul)
  (let ((d (bx**n+a nv ivar))
	(*roots ())  (*failures ())  ($breakup ()))
    (cond ((and (eq ul '$inf)
		(equal ll 0)
		(equal (cadr d) 1)) ())
	  ((eq ivar 'yx)		; new ivar cannot be same as old ivar
	   ())
	  (t
	   ;; This is a hack!  If nv is of the form b*x^n+a, we can
	   ;; solve the equation manually instead of using solve.
	   ;; Why?  Because solve asks us for the sign of yx and
	   ;; that's bogus.
	   (cond (d
		  ;; Solve yx = b*x^n+a, for x. This gives one root,
		  ;; x = ((yx-a)/b)^(1/n); INTCV-ROOT picks a usable root, if any.
		  (destructuring-bind (a n b)
		      d
		    (let ((root (power* (div (sub 'yx a) b) (inv n))))
		      (when (setq d (intcv-root nv ivar root n ll ul))
			     (cond (flag (intcv2 d nv ivar ll ul))
				   (t (intcv1 d nv ivar ll ul))))
			    )))
		 (t
		  (putprop 'yx t 'internal);; keep ivar from appearing in questions to user
		  (solve (m+t 'yx (m*t -1 nv)) ivar 1.)
		  (cond ((setq d	;; look for root that is inverse of nv
			       (do* ((roots *roots (cddr roots))
				     (root (caddar roots) (caddar roots)))
				    ((null root) nil)
				    ;; Roots with nested radicals in YX come
				    ;; from the cubic or quartic formula, and
				    ;; the integrand rewritten in them is too
				    ;; big to integrate.
				    (if (and (not (nested-radical-p root 'yx))
					     (or (real-infinityp ll)
						 (test-inverse nv ivar root 'yx ll))
					     (or (real-infinityp ul)
						 (test-inverse nv ivar root 'yx ul)))
					(return root))))
			 (cond (flag (intcv2 d nv ivar ll ul))
			       (t (intcv1 d nv ivar ll ul))))
			(t ()))))))))
