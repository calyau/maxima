;; Round 3. DINTEXP substitutes yx = exp(p(x)), and INTCV inverts that with
;; SOLVE. For a cubic or quartic p, SOLVE answers with the cubic or quartic
;; formula, and INTCV2 then spends seconds ratsimping the integrand rewritten
;; in those radicals, only for the integral to fail. DINTEXP's integrand is a
;; function of exp(p(x)) alone, so nothing in it can cancel the radicals the
;; inverse brings in. INTCV now takes SKIP-NESTED, which only DINTEXP passes,
;; and then skips roots with nested radicals in YX. LOGX1 must not pass it:
;; its integrand can contain p'(x), which cancels them (README, round 3).
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

(defun intcv (nv flag ivar ll ul &optional skip-nested)
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
				    ;; For DINTEXP, roots with nested radicals
				    ;; in YX leave the new integrand too big
				    ;; to integrate.
				    (if (and (not (and skip-nested
							(nested-radical-p root 'yx)))
					     (or (real-infinityp ll)
						 (test-inverse nv ivar root 'yx ll))
					     (or (real-infinityp ul)
						 (test-inverse nv ivar root 'yx ul)))
					(return root))))
			 (cond (flag (intcv2 d nv ivar ll ul))
			       (t (intcv1 d nv ivar ll ul))))
			(t ()))))))))

(defun dintexp (exp ivar ll ul &aux ans)
  (declare (special exp))
  (let ((*dintexp-recur* t))		;recursion stopper
    (cond ((and (sinintp exp ivar)     ;To be moved higher in the code.
		(setq ans (antideriv exp ivar))
		(setq ans (intsubs ans ll ul ivar)))
	   ;; If we can integrate it directly, do so and take the appropriate
	   ;; limits. INTSUBS builds the difference of two endpoint limits without
	   ;; simplifying across it, so the parts that cancel have to be brought
	   ;; together here.
	   (setq ans ($expand ans)))
	  ((setq ans (funclogor%e exp ivar))
	   ;; ans is the list (f(x) exp(k*x)).
	   (cond ((and (equal ll 0.)
		       (eq ul '$inf))
		  ;; Use the substitution s + 1 = exp(k*x).  The
		  ;; integral becomes integrate(f(s+1)/(s+1),s,0,inf)
		  (setq ans (m+t -1 (cadr ans))))
		 (t
		  ;; Use the substitution y=exp(k*x) because the
		  ;; limits are minf to inf.
		  (setq ans (cadr ans))))
	   ;; Apply the substitution and integrate it.
	   (intcv ans nil ivar ll ul t)))))
