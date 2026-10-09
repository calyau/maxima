;; Round 2, item 2 as in the handover. MEQP and MNQP take an optional CSIGN.
;; When it is false, MEQP skips its final csign unless ratsimp and the
;; equality facts rewrite A - B. SIGN-MABS passes false when SIGN of the
;; argument is $PNZ.
(in-package :maxima)
(defun meqp (a b &optional (csign t))
  ;; Check for some particular types before falling into the general case.
  (cond ((stringp a)
	 (and (stringp b) (equal a b)))
	((stringp b) nil)
	((arrayp a)
	 (and (arrayp b) (array-meqp a b)))
	((arrayp b) nil)
	((maxima-declared-arrayp a)
	 (and (maxima-declared-arrayp b) (maxima-declared-array-meqp a b)))
	((maxima-declared-arrayp b) nil)
	((maxima-undeclared-arrayp a)
	 (and (maxima-undeclared-arrayp b) (maxima-undeclared-array-meqp a b)))
	((maxima-undeclared-arrayp b) nil)
	(t
	 ;; Bind the SIGN specials: DCOMPARE sets them, and MEQP is called from
	 ;; inside sign computations that must keep their own.
	 (let ((z) sign minus odds evens)
	   (setq a (specrepcheck a))
	   (setq b (specrepcheck b))
	   (cond ((or (like a b)) (not (member a indefinites)))
		 ((or (member a indefinites) (member b indefinites)
		      (member a *infinities*) (member b *infinities*)) nil)
		 ((and (symbolp a) (or (eq t a) (eq nil a) (get a 'sysconst))
		       (symbolp b) (or (eq t b) (eq nil b) (get b 'sysconst))) nil)
		 ((or (mbagp a) (mrelationp a) (mbagp b) (mrelationp b))
		  (cond ((and (or (and (mbagp a) (mbagp b)) (and (mrelationp a) (mrelationp b)))
			      (eq (mop a) (mop b)) (= (length (margs a)) (length (margs b))))
			 (setq z (list-meqp (margs a) (margs b)))
			 (if (or (eq z t) (eq z nil)) z `(($equal) ,a ,b)))
			(t nil)))
		 ((and (op-equalp a 'lambda) (op-equalp b 'lambda)) (lambda-meqp a b))
		 (($setp a) (set-meqp a b))
		 ;; 0 isn't in the range of an exponential function, and a power
		 ;; with a negative exponent is undefined at a zero base, not zero.
		 ((or (and (zerop1 b) (provably-nonzero-p a))
		      (and (zerop1 a) (provably-nonzero-p b)))
		  nil)

		;; Two numbers: Answer arithmetically. Not merely a shortcut - ZEROP1 of the
		;; simplified difference uses float contagion, so equal(0.1, 1/10) is true,
		;; whereas the database compares exactly via RGRP and would call them different.
		;; Also keeps DCOMPARE from clobbering ODDS/EVENS/MINUS, since MEQP is reached
		;; from inside sign computations.
		 ((and (mnump a) (mnump b)) (zerop1 (sub a b)))

		 ;; lookup in assumption database
		 ((and (dcompare a b) (eq '$zero sign)))	; dcompare sets sign
		 ((memq sign '($pos $neg $pn)) nil)

		 ;; if database lookup failed, apply all equality facts
		 (t (let* ((d (sub a b))
			    (z (equal-facts-simp (sratsimp d))))
		      ;; Without CSIGN, give up unless that rewrote A - B.
		      (if (or csign (not (alike1 z d)))
			  (meqp-by-csign z a b)
			  `(($equal) ,a ,b)))))))))

(defun mnqp (x y &optional (csign t))
  (let ((b (meqp x y csign)))
    (cond ((eq b '$unknown) b)
	  ((or (eq b t) (eq b nil)) (not b))
	  (t `(($notequal) ,x ,y)))))

(defun sign-mabs (x)
  (let ((*complexsign* t))
    (sign (cadr x))
    (cond ((member sign '($pos $zero) :test #'eq))
	  ((member sign '($neg $pn) :test #'eq) (setq sign '$pos))
	  ;; abs(nonzero) > 0. If SIGN found nothing, the csign in MEQP would
	  ;; only repeat it, unless ratsimp rewrites the argument.
	  ((eq t (mnqp 0 (cadr x) (not (eq sign '$pnz)))) (setq sign '$pos))
	  (t (setq sign '$pz minus nil evens (nconc odds evens) odds nil)))))
