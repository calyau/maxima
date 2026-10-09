;; Round 2. SIGN-MABS: when SIGN of the argument E is $PNZ, call MNQP only if
;; ratsimp (and the equality facts) rewrite -E, since otherwise its csign
;; repeats the sign computation that just failed. The cheap checks MEQP
;; makes before that, structural and fact database, stay.
(in-package :maxima)
(defun sign-mabs (x)
  (let ((*complexsign* t))
    (sign (cadr x))
    (cond ((member sign '($pos $zero) :test #'eq))
	  ((member sign '($neg $pn) :test #'eq) (setq sign '$pos))
	  ((if (eq sign '$pnz)
	       (let ((e (specrepcheck (cadr x))))
		 (or (provably-nonzero-p e)
		     (let (sign minus odds evens)
		       (dcompare 0 e)
		       (member sign '($pos $neg $pn) :test #'eq))
		     (and (not (alike1 (let ($ratprint)
					 (equal-facts-simp (sratsimp (neg e))))
				       (neg e)))
			  (eq t (mnqp 0 e)))))
	       (eq t (mnqp 0 (cadr x))))
	   (setq sign '$pos))		; abs(nonzero) > 0
	  (t (setq sign '$pz minus nil evens (nconc odds evens) odds nil)))))
