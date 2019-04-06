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

(defmfun $powerseries (expr var pt)
  (when (and (atom var) (not (symbolp var)))
    (improper-arg-err var '$powerseries))
  (powerseries expr var pt))

;; An error that can be raised deep in the bowels of POWERSERIES that we'll
;; catch and return a noun form.
(define-condition powerseries-expansion-error (error)
  ((message :initarg :message :initform nil)))

(defun powerseries-expansion-error (&optional message &rest arguments)
  (error 'powerseries-expansion-error
         :message (when message (cons message arguments))))

;; The top-level routine for $powerseries, which calls this function after
;; spotting invalid arguments.
(defun powerseries (expr var pt)
  (handler-case
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
             (sbstpt expr (m+ 'x pt) var (simplifya (m- var pt) nil)))))

    ;; If expansion fails, print a message in verbose mode but then return a
    ;; noun form.
    (powerseries-expansion-error (e)
      (when $verbose
        (mtell (intl:gettext "Failed to expand ~M with respect to ~M at ~M.~%")
               expr var pt)
        (with-slots (message) e
          (when message
            (terpri)
            (finish-output)
            (apply #'mtell message))))
      `((%powerseries) ,expr ,var ,pt))))

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
    ;; If we've ended up with diff(foo, var) then we give up (we can't
    ;; substitute arbitrary expressions for the differentiation / integration
    ;; variable).
    (if (memq var (intdiff-vars-in-expr expanded))
        (powerseries-expansion-error
         (intl:gettext "~
Couldn't make substitution to evaluate at the given point because the~%~
power series expansion contained the expansion variable as an~%~
integration / differentiation variable."))
        (maxima-substitute usexp var expanded))))

(defun seriesexpand* (x)
  (let ((*index (gensumindex)) *n *a *m *c
        ($cauchysum t) ($ratsimpexpons t)
        $ratexpand *infsumsimp *ratexp *trigred *noexpand)
    (meval `(($declare) ,*index $integer))

    (sp2expand (seriespass1 x))))

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
      (finish-output)
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
	((zl-get (caar exp) 'sp2) (sp2sub (sp2trig exp) (cadr exp)))
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
  (unless (smono exp var)
    (powerseries-expansion-error
     (intl:gettext "Can't substitute the value~%~M~%~
                    into another expansion because it isn't a monomial in the~
                    expansion variable.")
     (out-of exp)))
  (maxima-substitute exp 'sp2var (simplify s)))

(defun ratexp (exp)
  (let (*gcd*)
    (if $verbose
	(mtell (intl:gettext "powerseries: attempt rational function expansion of~%~M")
	       (list '(mlabel) nil exp)))
    (multiple-value-call #'sratexpnd (numden exp))))

(defun sratexpnd (n d)
  (let ((ans (list nil))
        (*splist*)
        ;; A pattern that matches cc*(c*x^m + a)^n
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
		    (m// (srbinexpnd (cdr ans)) d))
		   (t
                    (powerseries-expansion-error))))
            ((smonop d var)
             (cond ((mplusp n)
                  (m+l (mapcar #'(lambda (q) (div* q d)) (cdr n))))
                 (t (m// n d))))
            ((not (equal 1 (setq *gcd* (ggcd (nconc (exlist n) (exlist d))))))
             (sratsubst *gcd* n d))

	    ;; Rational expansion theorem for distinct roots. The main point of
	    ;; the POLY? test here is that it guarantees $HIPOW will give the
	    ;; correct degrees (in particular, it only allows known non-negative
	    ;; integer exponents)
	    ((and (poly? n var)
		  (poly? d var)
		  (> ($hipow d var) ($hipow n var))
		  (has-distinct-nonzero-roots-p d var))
	     (expand-distinct-roots n d))

            ;; The SRBINEXPND call above dealt with expressions of the form
            ;; cc*(c*x^m+a)^n. Here, we deal with b/(cc*(c*x^m+a)^n). If you
            ;; explicitly write a polynomial like that, the simplifier will
            ;; rewrite it as (..)^(-n) before it gets here, but things like
            ;; 1/sqrt(x+1) won't have been rewritten successfully, so we catch
            ;; them here.
            ((and (free n var)
                  (prog2 (setq d (let (($ratfac t))
                                   (ratdisrep ($rat (factor d) var))))
                      (m1 d linpat)))
             ;; We had num/den and LINPAT matched den. We need to replace cc
             ;; with num/cc and n with -n.
             (setf (cdr (assoc 'n ans)) (m- (cdr (assoc 'n ans)))
                   (cdr (assoc 'cc ans)) (m// n (cdr (assoc 'cc ans))))
             (srbinexpnd (cdr ans)))

            (t
             ;; *RATEXP is set by RATEXPAND1, which we call to do the general
             ;; expansion below. This check makes sure we can't end up recursing
             ;; infinitely.
             (when *ratexp
               (powerseries-expansion-error))

             ;; Look for a power of var (a zero root) as a term. We already know
             ;; that d isn't a monomial, so we can only have a zero root if d is
             ;; a product. We also know that d is not atomic because otherwise
             ;; we'd have taken the (free d var) clause above.
             (let ((zero-root
                    (and (eq (caar d) 'mtimes)
                         (find-if (lambda (factor)
                                    (or (eq factor var)
                                        (and (mexptp factor)
                                             (eq (cadr factor) var))))
                                  (cdr d)))))
               (if (not zero-root)
                   (ratexpand1 n d)
                   (let ((other-factors (remove zero-root (cdr d)
                                                :test #'eq :count 1)))
                     (m* (sratexpnd n (m*l other-factors))
                         `((mexpt) ,zero-root -1)))))))))

; is a sum with index and bounds from psp2form
(defun psp2formp (exp)
  (and (listp exp) 
       (listp (car exp))
       (eq (caar exp) '%sum)
       (eq (caddr exp) *index)
       (eql (cadddr exp) 0)
       (eq (cadr (cdddr exp)) '$inf)))

;; A stripped down version of (sumcontract (intosum EXP)). Coalesce occurrences
;; of ((%SUM) <FOO> *INDEX 0 '$INF), which are allowed to start with
;; multiplicative constants. So something like
;;
;;    sum(a(i), i, 0, inf) + 2*sum(b(i), i, 0, inf) + c
;;
;; turns into
;;
;;    sum(a(i) + 2*b(i), i, 0, inf) + c
;;
;; This is good enough to collect up the multiple infinite sums that you get
;; when expanding a power series by partial fractions. It's (obviously) not
;; clever enough to rename index variables or adjust ranges.

(defun psp2foldsum (exp)
  (when $verbose
    (mtell (intl:gettext "powerseries: preparing to fold sums~%"))
    (show-exp exp))

  (multiple-value-bind (sums others)
      (partition-by (lambda (e)
                      (or (psp2formp e)
                          (and (mtimesp e)
                               (psp2formp (caddr e)))))
                    (cdr exp))
    (if (null sums)
        ;; If there were no sums, just return the original expression.
        exp
        ;; Otherwise contract the sums
        (let ((contracted
               (list '(%sum)
                     (m+l (mapcar (lambda (e)
                                    (if (eq (caar e) 'mtimes)
                                        (m* (cadr e) (cadr (caddr e)))
                                        (cadr e)))
                                  sums))
                     *index 0 '$inf)))
          (if (null others)
              contracted
              (m+l (cons contracted others)))))))

; solve returns a list: (soln mult soln mult ...)
; distinct-nonzero-roots-p returns true if every
;  soln is not nonzero and every mult is 1
(defun distinct-nonzero-roots-p (roots)
  (or (null roots)
      (and
       ;; Check all roots have multiplicity one and are nonzero.
       (loop
          for (root mult) on roots by #'cddr
          always (and (eql 1 mult)
                      (eq ($sign `((mabs) ,(caddr root))) '$pos)))

       ;; Now check that the roots are genuinely different from one another
       ;; (eg. if I have two symbolic roots, A and B, then I don't need to know
       ;; the values of A and B, but I do need to know that they aren't equal)
       (loop
          for rest on roots by #'cddr
          always (loop
                    for b in (cddr rest) by #'cddr
                    with a = (car rest)
                    always (eq '$pos
                               ($sign `((mabs) ,(m- (caddr b) (caddr a))))))))))

; returns t if polynomial poly in variable var has all distinct roots
(defun has-distinct-nonzero-roots-p (poly var)
  (let ((*roots nil)
	(*failures nil))
    (solve poly var 1)
    (and (not *failures)
         (distinct-nonzero-roots-p *roots))))

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
				  (mapcar #'caddr (remove-mult *roots))))
		     *index 0))
	  (t (error "EXPAND-DISTINCT-ROOTS: roots are not distinct.~%")))))

;; Try to expand N/D as a power series in VAR about 0.
;;
;; Can safely assume that D has no zero roots (we remove them in SRATEXPND
;; before calling this function).
(defun ratexpand1 (n d)
  (when $verbose
    (mtell (intl:gettext
            "powerseries: attempt partial fraction expansion of "))
    (show-exp (list '(mquotient) n d))
    (terpri)
    (finish-output))

  ;; *RATEXP serves as a recursion guard: if SRATEXPND is about to call us
  ;; *recursively, the flag will be set and it gives up instead.
  (let* ((*ratexp t)
         (fractions ($partfrac (div* n d) var)))
    (cond
      ;; If $PARTFRAC succeeds, it will return a sum of terms. In that case,
      ;; expand each one (which we assume is going to be easier than what we
      ;; started with) and try to fold the result into a single power series sum
      ;; again.
      ((mplusp fractions)
       (when $verbose
         (mtell (intl:gettext "which is ~%"))
         (show-exp fractions))
       (psp2foldsum (m+l (mapcar #'ratexp (cdr fractions)))))

      ;; If that didn't work, maybe it's because the numerator messed things
      ;; up. If we're really lucky, the numerator is actually a polynomial
      ;; though. In that case, factor it out and do an expansion of 1/d on its
      ;; own.
      ((poly? n var)
       (when $verbose
         (mtell (intl:gettext
                 "powerseries: partial fraction expansion failed, ~
                  expanding denominator only.~%")))
       (m* n (ratexp (m// 1 d))))

      ;; If n is complicated, there's not really much we can do to make further
      ;; progress, so give up and return a noun form.
      (t (powerseries-expansion-error
          (intl:gettext "Partial fraction expansion failed"))))))

(defun sratsubst (gcd num den)
  (and $verbose
       (prog2 (mtell (intl:gettext "powerseries: substituting for the occurrences of"))
	   (show-exp (list '(mexpt) var gcd))
	 (mtell (intl:gettext "in"))
	 (show-exp (list '(mquotient) num den))
	 (terpri)
	 (finish-output)))
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

;; Perform a binomial expansion of (c x^m + a)^n.
;;
;; If n isn't known to be an integer, we'd like to write a sum of terms
;;
;;    x^(mk) c^k a^(n-k) / ((n+1) beta(n-k+1, k+1))
;;
;; But we have to be a bit careful: it only works if c, x and a are
;; nonzero. (Otherwise the simplifier quite reasonably simplifies each of the
;; terms to zero, because the only nonzero term has the undefined term 0^0 in
;; it).
;;
;; If we're not sure whether cx is zero, we can just split out the first term
;; from the sum. If a is definitely zero, we can just write down a single
;; term. If we're not sure about a, it's a bit more difficult: if we know that n
;; is a positive integer, the sum is finite (and has at least two terms) and we
;; can just split off the last term. Otherwise, give up.
(defun srbinexpnd (ans)
  (alist-bind (n a m c cc x) ans
    (m* cc
        (if (and (integerp n) (minusp n))
            (srintegexpd (neg n) a m c)
            (let ((sgn-a ($sign (list '(mabs) a)))
                  (sgn-cx ($sign `((mabs) ((mtimes) ,c ,x))))
                  (sgn-n ($sign n))
                  (general-term
                   (m// (m* (m^ var (m* m *index))
                            (m^ c *index)
                            (m^ a (m- n *index)))
                        (m* (list '($beta) (m- n (m1- *index)) (m1+ *index))
                            (m1+ n)))))
              (cond
                ((eq sgn-n '$zero) 1)
                ((eq sgn-cx '$zero) (m^ a n))
                ((eq sgn-a '$zero) (m* (m^ c n) (m^ x (m* m n))))

                ((and (eq sgn-a '$pos) (eq sgn-cx '$pos))
                 (if (and ($featurep n '$integer)
                          (memq sgn-n '($pos $pz)))
                     `((%sum) ,general-term ,*index 0 ,n)
                     `((%sum) ,general-term ,*index 0 $inf)))

                ((eq sgn-a '$pos)
                 (m+ (m^ a n) `((%sum) ,general-term ,*index 1 $inf)))

                ((and ($featurep n '$integer) (eq sgn-n '$pos))
                 (m+ `((%sum) ,general-term ,*index 0 ,(m1- n))
                     (m* (m^ c n) (m^ x (m* n m)))))

                (t
                 (powerseries-expansion-error
                  (intl:gettext
                   "Couldn't expand binomial~%~M~%~
                    as we didn't know which terms were nonzero.")))))))))

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

(defun sp2trig (exp) (subst *index '*index (zl-get (caar exp) 'sp2)))

;; Take an expression, EXPR, and try to write it as a + b*VAR^c. On success,
;; returns (VALUES A B C). On failure, raise a powerseries-expansion-error.
;;
;; One way to do this would be to call $EXPAND and look at the results, but that
;; might be rather slow - if the expression is something like (1+x)^10, expand
;; will take ages and won't help very much. Another approach would be to put it
;; into CRE form, but that would be a mistake if the input was something like
;; (1+x)^100...
;;
;; Instead, we're a bit stupider: we walk through the expression tree. If we see
;; anything other than +,* and ^, we give up. If we find we have more than one
;; different exponent in our terms, we give up.
;;
;; Obviously, there are always examples where this won't work, but $EXPAND will
;; (something like (x+1)^2 - x^2, for example), but I'm assuming that this won't
;; be something we encounter in practice.
(defun split-two-term-poly (expr)
  (cond
    ((atom expr)
     (if (eq expr var)
         (values 0 1 1)
         (values expr 0 1)))

    ((freeof var expr)
     (values expr 0 1))

    ((eq (caar expr) 'mplus)
     (let ((a 0) (b) (c))
       (dolist (arg (cdr expr))
         (multiple-value-bind (aa bb cc) (split-two-term-poly arg)
           (setf a (m+ a aa))
           (unless (eql bb 0)
             (cond
               ;; Is this the first nonzero power we've seen?
               ((not b)
                (setf b bb c cc))
               ;; Is this another term with the same power as one we've seen
               ;; already?
               ((eql c cc)
                (setf b (m+ b bb)))
               ;; Otherwise, give up.
               (t
                (powerseries-expansion-error))))))
       (values a b c)))

    ((eq (caar expr) 'mtimes)
     (let ((prod 1) a b c)
       (dolist (arg (cdr expr))
         (multiple-value-bind (aa bb cc) (split-two-term-poly arg)
           (cond
             ((eql bb 0)
              (setf prod (m* prod aa)))
             ((not a)
              (setf a aa b bb c cc))
             ;; This is the second term (a + b*x^c), so we know we'll end up
             ;; with mixed terms. (We don't try to spot e.g. (1-x)*(1+x))
             (t
              (powerseries-expansion-error)))))

       (if a
           (values (m* a prod) (m* b prod) c)
           (values prod 0 1))))

    ((eq (caar expr) 'mexpt)
     ;; We know that EXPR isn't free of VAR. Check that VAR isn't in the
     ;; exponent.
     (unless (freeof var (caddr expr))
       (powerseries-expansion-error))
     (multiple-value-bind (a b c) (split-two-term-poly (cadr expr))
       (cond
         ((eql a 0)
          (values 0 (m^ b (caddr expr)) (m* c (caddr expr))))
         ((eql b 0)
          (values (m^ a (caddr expr)) 0 1))
         (t
          (powerseries-expansion-error)))))

    (t
     (powerseries-expansion-error))))

;; Try to expand log(e) using the series for log(1+x).
;;
;; The basic idea is that if a is nonzero then
;;
;;   log(a + b*x^k) = log(a*(1 + b/a*x^k))
;;                  = log(a) + log(1 + b/a*x^k)
;;
;; and we know a series for that.
(defun sp2log (e)
  (cond
    ((or (atom e) (free e var))
     (list '(%log) e))
    (t
     (multiple-value-bind (a b k)
         (split-two-term-poly (specdisrep e))
       ;; If a is zero, we can't expand
       (unless (eq '$positive
                   (asksign (list '(mabs) a)))
         (powerseries-expansion-error))

       (let* ((coeff (m* b (m^ a -1)))
              (coeff-sign ($sign coeff))
              (negate-coeff-p))
         ;; If we know that the coefficient is not positive, switch the series
         ;; around (which gets rid of some ugly minus signs). If we're not sure,
         ;; but the cofficient "looks negative" (so is something like -7*k),
         ;; switch it around too.
         (cond
           ((member coeff-sign '($neg $nz))
            (setf negate-coeff-p t
                  coeff (m- coeff)))
           ((and (not (member coeff-sign '($pos $pz)))
                 (mtimesp coeff)
                 (numberp (cadr coeff))
                 (minusp (cadr coeff)))
            (setq negate-coeff-p t
                  coeff (simptimes
                         (list* (car coeff) (- (cadr coeff)) (cddr coeff))
                         1 t))))
         (list '(%sum)
               (m* -1
                   (m^ (if negate-coeff-p
                           (m* coeff (m^ var k))
                           (m* -1 coeff (m^ var k)))
                       *index)
                   (m^ *index -1))
               *index 1 '$inf))))))

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

      (t (powerseries-expansion-error)))))

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
               (powerseries-expansion-error))

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

;; Substitute NEW for OLD in the expression tree EXPRESSION. This is a bit like
;; MAXIMA-SUBSTITUTE, except it assumes OLD is a symbol. But the important bit
;; is that it is bright enough to avoid substituting for bound variables in
;; %INTEGRATE and %AT forms: even though the symbol might have the same name,
;; it's thought of as a logically different variable.
;;
;; The function returns two values: the new expression and a flag that says
;; whether that expression has changed. If the flag is true on a recursive call,
;; int-diff-substitute resimplifies the result.
(defun int-diff-substitute (new old expression)
  (cond
    ((eq expression old) (values new t))
    ((atom expression) (values expression nil))
    (t
     (let ((op (caar expression))
           (args (cdr expression)))
       (if (or (and (eq op '%integrate) (eq old (second args)))
               (and (eq op '%at)
                    (not (atom (second args)))
                    (if (eq (caar (second args)) 'mlist)
                      ;; (second args) looks like ((mlist) ((mequal) ...) ...)
                      (memq old (mapcar #'second (rest (second args))))
                      ;; (second args) looks like ((mequal) ...)
                      (eq old (second (second args))))))
           (values expression nil)
           (let* ((some-changed-p nil)
                  (new-args
                   (mapcar (lambda (x)
                             (multiple-value-bind (new-val changed-p)
                                 (int-diff-substitute new old x)
                               (when changed-p
                                 (setf some-changed-p t))
                               new-val))
                           (cdr expression))))
             (if (not some-changed-p)
                 (values expression nil)
                 (values
                  (simplifya (cons (list op) new-args) nil) t))))))))

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
    (powerseries-expansion-error
     (intl:gettext "Recursion when trying to expand the definite integral: ~M")
     (out-of (symbol-value '*sp2integ-recursion-guard*))))

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
         (powerseries-expansion-error
          (intl:gettext "Endpoints of definite integral ~M are not monomials in ~
                         the expansion variable.") (out-of exp)))

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
         (m- (int-diff-substitute hi-exp v anti-derivative)
             (int-diff-substitute lo-exp v anti-derivative)))))

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
