;; Round 2. SIGN-LOG: compute the signs of 1 - ARG and ARG - 1 and MEQP of ARG
;; and 1 once each, instead of up to twice.
(in-package :maxima)
(defun sign-log (x)
 (let* ((arg (cadr x))
        (dummy (sign1 arg)) ;; SIGN sets SIGN, MINUS, ODDS, EVENS, describing ARG.
        (arg-sign sign))   ;; Its return value is meaningless.
  (declare (ignore dummy))
  (setq sign
	(cond ((eq sign '$zero) (log0-err x)) ; log(0) is undefined.
          ((member sign '($pos $pz)) ; accept $PZ - we already handled definitely 0
	       ;; The signs of 1 - ARG and ARG - 1, and whether ARG = 1, as
	       ;; MGRP, MGQP and MEQP would find them, each computed once.
	       (let* ((below (csign (sub 1 arg)))
		      (one (unless (eq below '$pos) (meqp arg 1)))
		      (above (unless (or (eq below '$pos) (eq one t)
					 (member below '($zero $pz)))
			       (csign (sub arg 1)))))
		 (cond ((eq below '$pos) '$neg)
		       ((eq one t) '$zero) ; log(1) = 0.
		       ((member below '($zero $pz)) '$nz)
		       ((eq above '$pos) '$pos)
		       ((member above '($zero $pz)) '$pz)
		       ((null one) '$pn)
		       (t '$pnz))))
	      ((and  *complexsign* (eql 1 (cabs arg))) '$imaginary)
	      (*complexsign* '$complex)
	      ((member sign '($pnz $pn)) '$pnz)
	      (t (imag-err x))))
  ;; If SIGN isn't '$POS, '$NEG or '$ZERO, $ASKSIGN will ask for the sign of the
  ;; expression described by MINUS, ODDS and EVENS. Set them to name an
  ;; expression of the same sign as log(ARG). For a nonnegative ARG, that is
  ;; ARG - 1, and the fact stored by $ASKSIGN then is about ARG itself, and not
  ;; only the logarithm. Where ARG may be negative, it has to be log(ARG) itself.
  ;; (It would be nice if $ASKSIGN could store different facts based on what the
  ;; user answers: Answering log(ARG) > 0 could then store ARG > 1.)
  (setq minus nil evens nil
        odds (unless (member sign '($pos $neg $zero))
               (ncons (if (member arg-sign '($pos $pz)) (sub arg 1) x))))))

