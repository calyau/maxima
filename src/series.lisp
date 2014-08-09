;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module series)

(declare-top (special var *n *a *m *c *index $cauchysum *gcd*
		      nn* dn* $ratsimpexpons *infsumsimp *roots *failures
		      *ratexp *var usexp $verbose ans *trigred
		      *form indl *noexpand $ratexpand))

(load-macsyma-macros rzmac)

;;******************************************************************************
;;				driver 	stage
;;******************************************************************************
;;
;;		the following throw labels are used
;;
;;psex -- for throws on failure to expand
;;

(defmfun $powerseries (expr var pt)
  (when (numberp var)
    (improper-arg-err var '$powerseries))

  (if (and (signp e pt) (symbolp var))
      (seriesexpand* expr)
      (cond
        ((signp e pt)
         (sbstpt expr 'x var var))
        ((eq pt '$inf)
         (sbstpt expr (m// 1 'x) var (div* 1 var)))
        ((eq pt '$minf)
         (sbstpt expr (m- (m// 1 'x)) var (m- (div* 1 var))))
        (t
         (sbstpt expr (m+ 'x pt) var (simplifya (m- var pt) nil))))))

;; Return a list of the terms in the expression that are used as integration or
;; differentiation variables.
(defun intdiff-vars-in-expr (expr)
  (cond
    ((atom expr) nil)

    ;; Arguably, if this is a definite integral, the variable isn't free so we
    ;; shouldn't return it. But maxima-substitute doesn't know about bound
    ;; variables and this is a helper function to avoid silly substitutions.
    ((eq (caar expr) '%integrate) (list (caddr expr)))

    ((eq (caar expr) '%derivative)
     (loop for x in (cddr expr) by #'cddr collecting x))

    (t
     (reduce #'union (cdr expr) :key #'intdiff-vars-in-expr))))

;; Series expand EXP after substituting in SEXP for VAR1, the main
;; variable. SEXP should be an expression in 'X and 'X will be replaced by a
;; gensym for the calculation. When the expansion is done, substitute USEXP for
;; the gensym.
(defun sbstpt (exp sexp var1 usexp)
  (let* ((var (gensym))
         (sub-exp (maxima-substitute (subst var 'x sexp) var1 exp))
         (expanded (seriesexpand* sub-exp)))
    ;; If we've ended up with diff(foo, var) then we give up and return the
    ;; original contents (we can't substitute arbitrary expressions for the
    ;; differentiation / integration variable).
    (if (memq var (intdiff-vars-in-expr expanded))
        exp
        (maxima-substitute usexp var expanded))))

(defun seriesexpand* (x)
  (let (*n *a *m *c
	   (*index (gensumindex))
	   ($cauchysum t)
	   ($ratsimpexpons t) $ratexpand
	   *infsumsimp *ratexp *trigred *noexpand)
    (meval `(($declare) ,*index $integer))
    (setq x (catch 'psex (sp2expand (seriespass1 x))))
    (cond ((and x (atom x)) x)
	  ((and x (not (eq (car x) 'err))) x)
	  ($verbose
	   (mtell (intl:gettext "powerseries: unable to expand for the following reason: "))
	   (cond ((null x) (mtell (intl:gettext "no reason given")) (intl:gettext "Failed to expand"))
		 (t (cdr x))))
	  (t (intl:gettext "Failed to expand")))))

(defun out-of (e)
  (let  ((e      (cond ((and (boundp '*var) *var)
			(subst (list '(mexpt) *var *gcd*) var e))
		       (t e)))
	 (var      (cond ((and (boundp '*var) *var)) (t var))))
    (cond ((and (boundp 'usexp) usexp)
	   (subst usexp var e))
	  (t e))))

(defun show-exp (e)
  (mtell "~%~%~M~%~%"
	 (list '(mlabel) nil (out-of e))))

(defun seriespass1 (e)
  (let ((w (sratsimp (sp1 e))))
    (when $verbose
      (terpri)
      (mtell (intl:gettext "powerseries: first simplification returned ~%"))
      (show-exp w))
    w))

;;
;;*****************************************************************************
;;	pass two		expansion phase
;;*****************************************************************************
;;

(defun sp2expand (exp)
  (cond ((or (free exp var) (atom exp)) exp)
	((mbagp exp) (cons (car exp) (mapcar #'sp2expand (cdr exp))))
	((eq (caar exp) 'mplus) (m+l (mapcar #'sp2expand (cdr exp)))) ;was below 'mtimes test--fixes powerseries(1+x^n,x,0)
	((sratp exp var) (ratexp exp))
	((eq (caar exp) 'mexpt) (sp2expt exp))
	((oldget (caar exp) 'sp2) (sp2sub (sp2trig exp) (cadr exp)))
	((poly? exp var) exp)
	((eq (caar exp) 'mtimes) (m*l (mapcar #'sp2expand (cdr exp))))
	((eq (caar exp) '%log) (sp2log (cadr exp)))
	((eq (caar exp) '%derivative) (sp2diff (cadr exp) (cddr exp)))
	((eq (caar exp) '%integrate)
	 (sp2integ (cadr exp) (caddr exp) (cdddr exp)))
	((member (caar exp) '(%product %sum) :test #'eq)
	 (list* (car exp) (sp2expand (cadr exp)) (cddr exp)))
	(t (list '(%sum)
		 (m* (m^ var *index)
		     (m^ (list '(mfactorial) *index) -1)
		     (list '(%at) (list '(%derivative) exp var *index)
			   (list '(mequal) var 0)))
		 *index 0 '$inf))))

(defun sp2sub (s exp)
  (cond ((smono exp var) (maxima-substitute exp 'sp2var (simplify s)))
	(t (throw 'psex (list 'err '(mtext)
			      "Tried to `maxima-substitute' the expansion of  "
			      (out-of exp)
			      " into an expansion")))))

(defun ratexp (exp)
  (let (nn* dn* *gcd*)
    (if $verbose
	(mtell (intl:gettext "powerseries: attempt rational function expansion of~%~M")
	       (list '(mlabel) nil exp)))
    (numden exp)
    (sratexpnd nn* dn*)))

(defun sratexpnd (n d)
  (let ((ans (list nil))
        (*splist*)
        (linpat
         '((mtimes) ((coefftt) (cc not-zero-free var))
                  ((mexpt) ((mplus) ((coeffpt)
                               (w m1 ((mexpt) (x equal var)
                                          (m not-zero-free var)))
                               (c freevar))
                              ((coeffpp) (a freevar)))
                         (n not-zero-free var)))))
    (declare (special *splist*))
      (cond ((and (not (equal n 1)) (smono n var))
             (m* n (sratexpnd 1 d)))
            ((free d var)
             (cond ((poly? n var)
		    (m// n d))
		   ((m1 n linpat)
		    (m* (srbinexpnd (cdr ans)) (div* 1 d)))
		   ((throw 'psex nil))))
            ((smonop d var)
             (cond ((mplusp n)
                  (m+l (mapcar #'(lambda (q) (div* q d)) (cdr n))))
                 (t (m// n d))))
            ((not (equal 1 (setq *gcd* (ggcd (nconc (exlist n) (exlist d))))))
             (sratsubst *gcd* n d))

	    ; rational expansion theorem for distinct roots
	    ((and (poly? n var)
		  (poly? d var)
		  (> ($hipow d var) ($hipow n var))
		  (has-distinct-nonzero-roots-p d var))
	     (expand-distinct-roots n d))

            ((and (equal n 1)
                (prog2 (setq d (let (($ratfac t))
                              (ratdisrep ($rat (factor d) var))))
                     (m1 d linpat)))

             ;; negate exponent because pattern matched denominator
	     (setf (cdadr ans) (mul -1 (cdadr ans)))

             (m// (srbinexpnd (cdr ans)) (cdr (assoc 'cc (cdr ans) :test #'eq))))
            (t
             (and *ratexp (throw 'psex nil))
             (if (not (eq (caar d) 'mtimes)) 
		 (ratexand1 n d)
	       (do ((p (cdr d) (cdr p)))	; denom is a product
               ((null p) (ratexand1 n d))
		 ; look for power of var (zero root) as term of denom
               (cond ((or (eq (car p) var)
                        (and (mexptp (car p)) (eq (cadaar p) var)))
                    (return (m* (sratexpnd n (meval (div* d (car p))))
				    (list '(mexpt) (car p) -1)))))))))))

; is a sum with index and bounds from psp2form
(defun psp2formp (exp)
  (and (listp exp) 
       (listp (car exp))
       (eq (caar exp) '%sum)
       (eq (caddr exp) *index)
       (eq (cadddr exp) 0)
       (eq (cadr (cdddr exp)) '$inf)))

; turns (%sum ...) + (%sum ...) + (%sum ...)
; into   %sum ... + ... + ...
(defun psp2foldsum (exp)
  (and $verbose
       (prog2 (mtell (intl:gettext "powerseries: preparing to fold sums~%"))
	   (show-exp exp)))
  (if (and (eq (caar exp) 'mplus)
	   (every #'(lambda (e) 
		      (or (psp2formp e)
			  (and (mtimesp e)
			       (psp2formp (caddr e)))))
		  (cdr exp)))
      (list '(%sum) (m+l (mapcar #'(lambda (e)
				     (if (eq (caar e) 'mtimes) 
					 (m* (cadr e) (cadr (caddr e)))
				       (cadr e)))
				 (cdr exp)))
	    *index 0 '$inf)
    exp))

; solve returns a list: (soln mult soln mult ...)
; distinct-nonzero-roots-p returns true if every
;  soln is not nonzero and every mult is 1
(defun distinct-nonzero-roots-p (roots)
  (or (null roots)
      (and (not (zerop1 (caddar roots)))	; root must not be zero
	   (eq 1 (cadr roots))			; multiplicity of root must be one
	   (distinct-nonzero-roots-p (cddr roots)))))

; returns t if polynomial poly in variable var has all distinct roots
(defun has-distinct-nonzero-roots-p (poly var)
  (let ((*roots nil)
	(*failures nil))
    (solve poly var 1)
    (cond (*failures nil)
	  ((distinct-nonzero-roots-p *roots) t)
	  (t nil))))

; Rational Expansion Theorem for Distinct Roots
; Graham, Knuth, Patashnik, "Concrete Math" 2nd ed, p 340
; 
; If R(z) = P(z)/Q(z), where Q(z) = q0*(1-r_1*z)*...*(1-r_l*z) and the
; numbers (r_1 ... r_l) are distinct, and if P(z) is a polynomial of degree less
; than l, then
;  [z^n]R(z) = a_1*r_1^n + ... + a_l*r_l^n,  where a_k = -r_k*P(1/r_k)/Q'(1/r_k)
(defun expand-distinct-roots (num den)
  (let ((*roots nil)
	(*failures nil))
    (solve den var 1)
    (cond (*failures (error "EXPAND-DISTINCT-ROOTS: failed to solve for roots."))
	  ((distinct-nonzero-roots-p *roots)
	   (psp2form (m+l (mapcar #'(lambda (r) 
				      (and $verbose
					   (prog2
					       (mtell (intl:gettext "powerseries: for root:~%"))
					       (show-exp r)
					       (mtell (intl:gettext "powerseries: numerator at root =~%"))
					       (show-exp (maxima-substitute r var num))
					       (mtell (intl:gettext "powerseries: first derivative of denominator at root =~%"))
					       (show-exp (maxima-substitute r var ($diff den var)))))
				      (m* -1
					  (m// 1 r)
					  (maxima-substitute r var num)
					  (m// 1 (maxima-substitute r var ($diff den var)))
					  (m^ (m// 1 r) *index)))
				  (mapcar #'caddr (deletmult *roots))))
		     *index 0))
	  (t (error "EXPAND-DISTINCT-ROOTS: roots are not distinct.~%")))))

(defun ratexand1 (n d)
  (and $verbose
       (prog2 (mtell (intl:gettext "powerseries: attempt partial fraction expansion of "))
	   (show-exp (list '(mquotient) n d))
	 (terpri)))
  (funcall #'(lambda (*ratexp) 
	       (let ((l ($partfrac (div* n d) var)))
					 (cond ((eq (caar l) 'mplus)
						(and $verbose
			     (prog2 (mtell (intl:gettext "which is ~%"))
				 (show-exp l)))
			(psp2foldsum
			 (m+l (mapcar #'ratexp
				      (cdr l)))))
		       ((poly? n var)
			(and $verbose
			     (mtell (intl:gettext "powerseries: partial fraction expansion failed, expanding denominator only.~%")))
			(m* n (ratexp (m// 1 d))))
					       (t (throw 'psex
						    '(err (mtext)
				       (intl:gettext "powerseries: partial fraction expansion failed")))))))
	   t))

(defun sratsubst (gcd num den)
  (and $verbose
       (prog2 (mtell (intl:gettext "powerseries: substituting for the occurrences of"))
	   (show-exp (list '(mexpt) var gcd))
	 (mtell (intl:gettext "in"))
	 (show-exp (list '(mquotient) num den))
	 (terpri)))
  (funcall #'(lambda (var* *var)
	       (setq num (maxima-substitute (m^ var* (m^ gcd -1)) *var num)
		     den (maxima-substitute (m^ var* (m^ gcd -1)) *var den))
	       (maxima-substitute (m^ *var gcd) var*
				  (funcall #'(lambda (var) (sratexpnd num den)) var*)))
	   (gensym) var))

(defun ggcd (l)
  (cond ((null l) 1)
	((null (cdr l)) (car l))
	((equal 1 (car l)) 1)
	(t ($gcd (ggcd (cdr l)) (car l)))))

(defun exlist (exp)
  (cond ((null exp) nil)
	((atom exp)
	 (and (eq exp var) (ncons 1)))
	((and (not (atom (car exp))) (eq (caar exp) 'mplus))
	 (exlist (cdr exp)))
	((smono (car exp) var)
	 (cond ((equal *n 0) (exlist (cdr exp)))
	       (t (cons *n (exlist (cdr exp))))))
	(t (exlist (cdr exp)))))

(defun srbinexpnd (ans)
  (let ((n (cdr (assoc 'n ans :test #'eq)))
	(a (cdr (assoc 'a ans :test #'eq)))
	(m (cdr (assoc 'm ans :test #'eq)))
	(c (cdr (assoc 'c ans :test #'eq))))
    (cond ((and (integerp n) (minusp n))
	   (srintegexpd (neg n) a m c))
	  (t (list '(%sum)
		   (m// (m* (m^ (m* c var) (m* m *index))
			    (m^ a (m- n *index)))
			(m* (list '($beta) (m- n (m1- *index)) (m1+ *index))
			    (m1+ n)))
		   *index 0 '$inf)))))

(defun psp2form (coeff exp bas)
  (list '(%sum) (m* coeff (m^ var exp)) *index bas '$inf))

(defun srintegexpd (n a m c)
  (and $verbose
       (prog2 (mtell (intl:gettext "powerseries: apply rule for expressions of ~%"))
	   (show-exp '((mexpt) ((mplus) $a ((mtimes) $c ((mexpt) $var $m)))
		       ((mminus) $n)))
	 (mtell (intl:gettext "powerseries: here we have"))
	 (show-exp (list '(mlist) (list '(mequal) '$n n) (list '(mequal) '$a a)
			 (list '(mequal) '$c c) (list '(mequal) '$m m)))))
  (cond ((= n 1)
	 (psp2form
	  (m* (m^ a (m* -1 (m+ 1 *index)))
	      (m^ (m* -1 c) *index))
	  (if (equal m 1) *index (m* *index m))
	  0))
	((= 2 n)
	 (psp2form (m* (m+ 1 *index)
		       (m^ a (m* -1 (m+ 2 *index)))
		       (m^ (m* -1 c) *index))
		   (if (equal m 1) *index (m* *index m))
		   0))
	(t (psp2form (m* (do ((nn (1- n) (1- nn))
			      (l nil (cons (list '(mplus) *index nn) l)))
			     ((= nn 0)
			      (m*l (cons (m// 1 (factorial (1- n))) l))))
			 (m^ (m* -1 c (m^ a -1)) *index)
			 (m^ a (- n)))
		     (if (equal m 1) *index (m* *index m))
		     0))))

(defun sratp (a var)
  (cond ((atom a) t)
	((member (caar a) '(mplus mtimes) :test #'eq) (sandmap (cdr a)))
	((eq (caar a) 'mexpt)
	 (cond ((free (cadr a) var) (free (caddr a) var))
	       ((smono a var) t)
	       ((and (free (caddr a) var) (sratp (cadr a) var)))))
	(t (and (free (mop a) var) (every #'(lambda (s) (free s var)) (margs a))))))

(defun sandmap (l) (or (null l) (and (sratp (car l) var) (sandmap (cdr l)))))

(defun sp2trig (exp) (subst *index '*index (oldget (caar exp) 'sp2)))

(defun sp2log (e)
  (funcall #'(lambda (exp *a *n)
	       (cond ((or (atom e) (free e var)) (list '(%log) e))
		     ((null (smono exp var)) (throw 'psex nil))
		     ((or (and (numberp *a)
			       (minusp *a)
			       (setq *a (- *a)))
			  (and (mtimesp *a)
			       (numberp (cadr *a))
			       (minusp (cadr *a))
			       (setq *a (simptimes
					 (list* (car *a) (- (cadr *a)) (cddr *a))
					 1 t))))
		      (list '(%sum)
			    (m* -1
				(m^ (m* (m^ var *n) *a) *index)
				(m^ *index -1))
			    *index 1 '$inf))
		     (t (list '(%sum)
			      (m* -1
				  (m^ (m* -1 *a (m^ var *n)) *index)
				  (m^ *index -1))
			      *index 1 '$inf))))
	   (m- e 1) nil nil))

(defun sp2expt (exp)
  (let ((base (cadr exp))
        (power (caddr exp)))
    (cond
      ;; Do (a^b)^c = a^(bc) when c is a number
      ((and (numberp power) (mexptp base))
       (sp2expt (m^ (cadr base)
                    (m* power (caddr base)))))

      ;; If a positive power which is free of the expansion variable and less
      ;; than the maximum expansion power then expand the base and do the
      ;; multiplication explicitly.
      ((and (free power var)
            (signp g power)
            (< power $maxposex))
       (m*l (dup (sp2expand base) power)))

      ;; If the base is free of VAR, rewrite as a^b = e^(b log(a)) if necessary
      ;; and then use the tabulated expansion of exp(x). If POWER is a sum then
      ;; do the rewrite a^(b+c) = a^b a^c
      ((free base var)
       (let ((expansion
              (subst *index '*index
                     (if (eq base '$%e)
                         (get 'mexpt 'sp2)
                         (subst `((mtimes) ((mlog) ,base) sp2var) 'sp2var
                                (get 'mexpt 'sp2))))))
         (cond
           ((not (mplusp power))
            (sp2sub expansion power))

           (t
            (m*l (mapcar (lambda (summand) (sp2sub expansion summand))
                         (cdr power)))))))

      (t (throw 'psex nil)))))

(defun dup (x %n)
  (if (= %n 1)
      (ncons x)
      (cons x (dup x (1- %n)))))

(defun sp2diff (exp l)
  (cond
    ((free exp var)
     (sp3form (sp2expand exp) (cons '(%derivative) l)))
    (t
     ;; If the expression isn't free of VAR, we know how to expand the result if
     ;; the orders of differentiation are all explicit non-negative
     ;; integers. Otherwise, give up.
     (let ((indl) (remaining-derivs))
       (loop
          for (y order) on l by #'cddr
          do
            (cond
              ((not (typep order '(integer 0)))
               (throw 'psex nil))

              ((not (eq y var))
               (setf remaining-derivs (list* order y remaining-derivs)))

              (t
               (dotimes (i order)
                 (setf indl nil)
                 (setf exp (sp2diff1 (sp2expand exp) nil nil))))))

       (if remaining-derivs
           (sp3form exp `((%derivative)
                          ,@(nreverse remaining-derivs)))
           exp)))))

(defun sp2diff1 (exp ind lol)		;ind is a list of the indices
					;lol is a list of the lower limits
  (cond ((atom exp) (sdiff exp var))
	((eq (caar exp) 'mplus)
	 (cons '(mplus)
	       (mapcar #'(lambda (q) (sp2diff1 q ind lol))
		       (cdr exp))))
	((eq (caar exp) '%sum)
	 (setq indl (cons (copy-list (cddr exp)) indl))
	 (sp2diff1 (cadr exp)
		   (cons (caddr exp) ind)
		   (cons (cadddr exp) lol)))
	(t (sp2diff2 exp ind lol))))

(defun sp2diff2 (exp ind lol)
  (let (e fr)
    (setq e (m2 exp '((mtimes) ((coefftt) (fr freevar))
		      ((coefftt) (e true))))
	  fr (cdr (assoc 'fr e :test #'eq))
	  e  (cdr (assoc 'e e :test #'eq)))
    (sp3reconst
     (cond ((and (mexptp e) (eq (cadr e) var))
	    (cond ((equal 0 (mbinding (ind lol)
				      (meval (m* fr (caddr e)))))
		   (m* (sp3substp1 ind ind (m* fr (caddr e))) e))
		  ((mgrp 1 (mbinding (ind lol)
				     (simplify (mevalatoms (caddr e)))))
		   (m* fr (caddr e) (m^ (cadr e) (m- (caddr e) 1))))
		  (t (sdiff exp var))))
	   (t (sdiff exp var))))))

(defun sp2integ (exp v l)
  (if (null l)
      (if (eq var v)
	  (sp2integ1 ($expand (sp2expand exp)))
	  (sp3form (sp2expand exp) (list '(%integrate) v)))
      (sp2integ2 exp v (car l) (cadr l))))

(defun sp2integ1 (exp)
  (let (pair)
    (cond ((ratp exp var) (ratint exp var))
	  ((eq (caar exp) 'mplus)
	   (cons '(mplus) (mapcar #'sp2integ1 (cdr exp))))
	  ((and (eq (caar exp) 'mtimes)
		(prog2 (setq pair (partition exp var 1))
		    (not (equal (car pair) 1))))
	   (mul2* (car pair) (sp2integ1 (cdr pair))))
	  ((and (eq (caar exp) 'mtimes)
		(prog2 (setq exp ($intosum exp)) nil)))
	  ((or (not (eq (caar exp) '%sum)) (not (isinop (cadr exp) '%sum)))
	   (sinint exp var))
	  (t (let ((indl (ncons (cddr exp))))
	       (sp2integ12 (cadr exp) (ncons (caddr exp)) (ncons (cadddr exp))))))))

(defun sp2integ12 (exp ind lol)
  (cond ((atom exp)
	 (sp3reconst (ratint exp var)))
	((eq (caar exp) 'mplus)
	 (sp3reconst
	  (m+l (mapcar #'(lambda (q) (sp2integ13 q ind lol))
		       (cdr exp)))))
	((eq (caar exp) '%sum)
	 (setq indl (cons (cddr exp) indl))
	 (sp2integ12 (cadr exp)
		     (cons (caddr exp) ind)
		     (cons (cadddr exp) lol)))
	(t (sp3reconst (sp2integ13 exp ind lol)))))

(defun sp2integ13 (exp ind lol)
  (let (e fr)
    (setq e (m2 exp '((mtimes) ((coefftt) (fr freevar))
		      ((coefftt) (e true))))
	  fr (cdr (assoc 'fr e :test #'eq))
	  e  (cdr (assoc 'e e :test #'eq)))
    (cond ((and (mexptp e) (eq (cadr e) var))
	   (cond ((mgrp -1 (mbinding (ind lol)
				     (meval (caddr e))))
		  (m* (sp3substpn ind ind (m* fr (caddr e)) -1) e))
		 (t (sinint exp var))))
	  (t (sinint exp var)))))

;; Expand a definite integral, integrate(exp, v, lo, hi).
(defun sp2integ2 (exp v lo hi)
  ;; Deal with the case where the integration variable is also the expansion
  ;; variable. Something's a bit odd if we're doing this, but we assume that the
  ;; aliasing was accidental and that the (bound) integration variable should be
  ;; called something else.
  (when (eq v var)
    (setq v (gensym)
          exp (subst v var exp)))

  (when (boundp '*sp2integ-recursion-guard*)
    (throw 'psex
      (list 'err '(mtext)
            "Recursion when trying to expand the definite integral "
            (out-of (symbol-value '*sp2integ-recursion-guard*)))))

  (cond
    ((not (and (free lo var) (free hi var)))
     ;; Suppose one or both of the endpoints involves VAR. We'll give up unless
     ;; they are monomials in VAR (because substituting a non-monomial into a
     ;; power series is more difficult). If they *are* monomials in VAR, then we
     ;; try to expand the integral as a power series and find an antiderivative.
     (let ((lo-exp (sp2expand lo))
           (hi-exp (sp2expand hi))
           (*sp2integ-recursion-guard* exp))
       (declare (special *sp2integ-recursion-guard*))

       (unless (and (smono lo-exp var) (smono hi-exp var))
         (throw 'psex
           (list 'err '(mtext)
                 "Endpoints of definite integral " (out-of exp)
                 " are not monomials in the expansion variable.")))

       ;; Since this is a formal powerseries calculation, we needn't concern
       ;; ourselves with radii of convergence here. Just expand the integrand
       ;; about zero. (Is there something cleverer we could do?)
       (let ((anti-derivative (sinint ($powerseries exp v 0) v)))
         ;; If the expansion had failed, we'd have thrown an error to top-level,
         ;; so we can assume that ANTI-DERIVATIVE is a sum of monomials in
         ;; V. Substituting in LO-EXP and HI-EXP, we'll get two sums of
         ;; monomials in VAR. The difference of the two expressions isn't quite
         ;; of the right form, but hopefully it's close enough for any other
         ;; code that uses it.
         (m- (maxima-substitute hi-exp v anti-derivative)
             (maxima-substitute lo-exp v anti-derivative)))))

    ((free exp var)
     (list '(%integrate) (subst var v exp) var lo hi))

    (t
     (sp3form (sp2expand exp)
              (list '(%integrate) v lo hi)))))

;;************************************************************************************
;;	phase three		miscellaneous garbage and final simplification
;;************************************************************************************

(defun sp3reconst (e)
  (do ((l indl (cdr l)) (e e (list* '(%sum) e (car l))))
      ((null l) e)))

(defun sp3substpn (vars vals exp n)
  (sp3subst vars (mapcar #'(lambda (q) (add2* q n)) vals) exp))

(defun sp3substp1 (vars vals exp) (sp3substpn vars vals exp 1))

(defun sp3subst (vars vals exp)
  (simplify (sublis (mapcar #'cons (cdr vars) (cdr vals))
		    (subst (car vals) (car vars) exp))))

(defun sp3form (e *form) (sp3form1 e))

(defun sp3form1 (e)
  (cond ((atom e) (list* (car *form) e (cdr *form)))
	((eq (caar e) 'mplus)
	 (cons '(mplus) (mapcar #'sp3form1 (cdr e))))
	((eq (caar e) '%sum)
	 (list* '(%sum) (sp3form1 (cadr e)) (cddr e)))
	(t (list* (car *form) e (cdr *form)))))

;; These are the series expansions for circular functions

(defprop %sin
    ((%sum) ((mtimes)
	     ((mexpt) -1 *index)
	     ((mexpt) ((mfactorial) ((mplus) ((mtimes) 2 *index) 1)) -1)
	     ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) 1)))
     *index 0 $inf)
  sp2)

(defprop %cos
    ((%sum) ((mtimes) ((mexpt) -1 *index)
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     ((mexpt) sp2var ((mtimes) 2 *index)))
     *index 0 $inf)
  sp2)

(defprop %tan
    ((%sum) ((mtimes) ((mexpt) -1 ((mplus) *index -1))
	     ((mexpt) 2 ((mtimes) 2 *index))
	     ((mplus) ((mexpt) 2 ((mtimes) 2 *index)) -1)
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     (($bern) ((mtimes) 2 *index))
	     ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1)))
     *index 0 $inf)
  sp2)

(defprop %csc
    ((%sum) ((mtimes) 2 
	     ((mexpt) -1 ((mplus) *index -1))
	     ((mplus) ((mexpt) 2 ((mplus) ((mtimes) 2 *index) -1)) -1)
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     (($bern) ((mtimes) 2 *index))
	     ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1)))
     *index 0 $inf)
  sp2)

(defprop %cot
    ((%sum) ((mtimes)
	     ((mexpt) -1 *index)
	     ((mexpt) 2 ((mtimes) 2 *index))
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     (($bern) ((mtimes) 2 *index))
	     ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1)))
     *index 0 $inf)
  sp2)

(defprop %sec
    ((%sum) ((mtimes) ((mexpt) -1 *index)
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     (($euler) ((mtimes) 2 *index))
	     ((mexpt) sp2var ((mtimes) 2 *index)))
     *index 0 $inf)
  sp2)

;; These are the series definitions of exponential functions.

(defprop mexpt
    ((%sum)
     ((mtimes) ((mexpt) ((mfactorial) *index) -1) ((mexpt) sp2var *index))
     *index 0 $inf)
  sp2)

(defprop %sinh
    ((%sum) ((mtimes)
	     ((mexpt) ((mfactorial) ((mplus) ((mtimes) 2 *index) 1)) -1)
	     ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) 1)))
     *index 0 $inf)
  sp2)

(defprop %cosh
    ((%sum) ((mtimes)
	     ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
	     ((mexpt) sp2var ((mtimes) 2 *index)))
     *index 0 $inf)
  sp2)

(defprop %tanh
    ((%sum)
     ((mtimes) ((mexpt) 4 *index)
      ((mplus) ((mexpt) 4 *index) -1)
      (($bern) ((mtimes) 2 *index))
      ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1))
      ((mexpt)
       ((mfactorial) ((mtimes) 2 *index))
       -1))
     *index 0 $inf)
  sp2)

(defprop %coth
    ((%sum)
     ((mtimes) ((mexpt) 4 *index)
      (($bern) ((mtimes) 2 *index))
      ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
      ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1)))
     *index 0 $inf)
  sp2)

(defprop %sech 
    ((%sum)
     ((mtimes) (($euler) ((mtimes) 2 *index))
      ((mexpt) ((mfactorial) ((mtimes) 2 *index)) -1)
      ((mexpt) sp2var ((mtimes) 2 *index)))
     *index 0 $inf)
  sp2)

(defprop %csch
    ((%sum)
     ((mtimes) -2 ((mplus) ((mexpt) 2 ((mplus) ((mtimes) 2 *index) -1)) -1)
      ((mexpt) ((mfactorial) ((mtimes) *index 2)) -1)
      (($bern) ((mtimes) 2 *index))
      ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) -1)))
     *index 0 $inf)
  sp2)

;;arc trigonometric function expansions

(defprop %asin
    ((%sum)
     ((mtimes) ((%genfact) ((mplus) ((mtimes) 2 *index) -1) *index 2)
      ((mexpt) ((%genfact) ((mtimes) 2 *index) *index 2) -1)
      ((mexpt) ((mplus) ((mtimes) 2 *index) 1) -1)
      ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) 1)))
     *index 0 $inf)
  sp2)

(defprop %atan
    ((%sum)
     ((mtimes) ((mexpt) -1 *index)
      ((mexpt) ((mplus) ((mtimes) 2 *index) 1) -1)
      ((mexpt) sp2var ((mplus) ((mtimes) 2 *index) 1)))
     *index 0 $inf)
  sp2)
