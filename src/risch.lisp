;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module risch)

(load-macsyma-macros rzmac ratmac)

(declare-top (special *mosesflag
                      context *in-risch-p*))

(defmvar $erfflag t "Controls whether `risch' generates `erfs'")

(defvar *changevp* t
  "When nil prevents changevar hack")

(defvar *risch-partial* nil
  "T lets RISCHEXPVAR return partial solutions, which can leave integrals for
  HYPERTRIGINT1 to evaluate.")

(defvar *risch-partial-count* 0
  "The number of times RISCHEXPVAR could have returned a partial solution.")

(defmacro pair (al bl) `(mapcar #'cons ,al ,bl))

;; internal representation of risch expressions: list with canonical rational
;; expression (CRE) as first element, standard maxima expressions as remaining
;; elements.  risch expression is sum of CRE and remaining elements.
(defmacro rischzero () ''((0 . 1) 0))

(defun rischnoun (exp1 risch-ratform risch-intvar &optional (exp2 exp1 exp2p))
  (unless exp2p (setq exp1 (rzero)))
  `(,exp1 ((%integrate) ,(disrep exp2 risch-ratform) ,risch-intvar)))

(defun getrischvar ()
  (do ((vl varlist (cdr vl))
       (gl genvar (cdr gl)))
      ((null (cdr vl)) (car gl))))

;; test whether CRE p is constant with respect to variable of integration.
;; requires variables in varlist and genvar
;; to be ordered as by intsetup, with var of integration ordered before
;; any other expressions that contain it.
(defun risch-pconstp (p risch-mainvar)
  (or (pcoefp p) (pointergp risch-mainvar (car p))))

(defun risch-constp (r risch-mainvar)
  (setq r (ratfix r))
  (and (risch-pconstp (car r) risch-mainvar)
       (risch-pconstp (cdr r) risch-mainvar)))

;; adds two risch expressions (defined above).
(defun rischadd (x y)
  (destructuring-let (((a . b) x) ((c . d) y))
    (cons (r+ a c) (append b d))))

(defmfun $risch (exp risch-var)
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))
    (with-new-context (context)
      (rischint exp risch-var))))

(defun spderivative (p risch-var)
  (cond ((pcoefp p) '(0 . 1))
	((null (cdr p)) '(0 . 1))
	((or (not (atom (car p))) (numberp (car p))) ;P IS A RATFORM
	 (let ((denprime (spderivative (cdr p) risch-var)))
	   (cond ((rzerop denprime)
		  (ratqu (spderivative (car p) risch-var) (cdr p)))
		 (t (ratqu (r- (r* (spderivative (car p) risch-var)
				   (cdr p))
			       (r* (car p) denprime))
			   (r* (cdr p) (cdr p)))))))
	(t (r+ (spderivative1 (car p)
			      (cadr p)
			      (caddr p)
			      risch-var)
	       (spderivative (cons (car p) (cdddr p))
			     risch-var)))))

(defun spderivative1 (var1 deg coeff risch-var)
  (cond ((eq var1 risch-var)
	 (r* (ratexpt (cons (list risch-var 1 1) 1) (1- deg))
	     (pctimes deg coeff)))
	((pointergp risch-var var1) '(0 . 1))
	((equal deg 0) (spderivative coeff risch-var))
	(t (r+ (r* (ratexpt (cons (list var1 1 1) 1) deg)
		   (spderivative coeff risch-var))
	       (r* (cond ((equal deg 1) coeff)
			 (t (r* deg
				coeff
				(ratexpt (cons (list var1 1 1) 1)
					 (1- deg)))))
		   (get var1 'rischdiff) )))))

(defun polylogp (exp &optional sub)
  (and (mqapplyp exp) (eq (subfunname exp) '$li)
       (or (null sub) (equal sub (car (subfunsubs exp))))))

;; True if NEW, which RADCAN1 made of EXP, has %i or a power free of VAR with
;; an exponent other than an integer, whose base is not %e, not positive and
;; not the base of a power with such an exponent in EXP, as when
;; sqrt(c-c*x^2) becomes sqrt(c)*sqrt(1-x)*sqrt(x+1). Taking a positive
;; factor out of a root is exact, and so is taking a sum apart in an
;; exponent, as d^(x+c) into d^x*d^c. For a variable declared complex, such
;; as one that the integrator substitutes, only the exponent must be free of
;; VAR, as when sqrt(1/t^2) becomes 1/t, and so while *BRANCH-VAR-ROOTS* is
;; set, as when sqrt((1-t^2)*(3+2*t-3*t^2)) becomes
;; sqrt(t-1)*sqrt(t+1)*sqrt(3*t^2-2*t-3). Then %e^(log(u)/2), as RADCAN1
;; writes sqrt(u), counts as sqrt(u).
(defun new-const-roots-p (exp new var)
  (let ((all (or *branch-var-roots* (decl-complexp var))))
    (flet ((roots (e)
             (let ((l nil))
               (labels ((walk (e)
                          (when (or (eq e '$%i)
                                    (and (mexptp e) (not (integerp (caddr e)))))
                            (pushnew e l :test #'alike1))
                          (unless (atom e) (mapc #'walk (cdr e)))))
                 (walk (if all
                           (let (($%e_to_numlog t) ($logsimp t)) (resimplify e))
                           e)))
               l))
           (base (r) (if (atom r) r (cadr r))))
      (let ((bases (mapcar #'base (roots exp))))
        (some #'(lambda (r)
                  (not (or (member (base r) bases :test #'alike1)
                           (and (mexptp r)
                                (or (eq (cadr r) '$%e)
                                    (not (freeof var (if all (caddr r) r)))
                                    (eq (csign (cadr r)) '$pos))))))
              (roots new))))))

;; RADCAN1 in INTSETUP can take a power of a product apart, as
;; sqrt(c-c*x^2) into sqrt(c)*sqrt(1-x)*sqrt(x+1), which holds only up to a
;; factor such as -1 that is constant where the powers are continuous.
;; Return that factor EXP/NEW if it is constant and not 1, else NIL. It is
;; constant if its logarithmic derivative EXP'/EXP-NEW'/NEW is zero, which
;; RATSIMP finds without taking powers apart.
(defun branch-factor (exp new var)
  (and (not (zerop1 exp)) (not (zerop1 new)) (new-const-roots-p exp new var)
       (let* (($%e_to_numlog t) ($logsimp t) ($algebraic t)
              ;; Write the %e^(log(u)/4) that RADCAN1 makes of roots of U as
              ;; roots again, which the zero test below needs.
              (new (resimplify new))
              (k (sratsimp (resimplify (div exp new)))))
         (and (not (equal k 1))
              (car (errcatch
                    (zerop1 (sratsimp (sub (div (sdiff exp var) exp)
                                           (div (sdiff new var) new))))))
              k))))

;; T if E has a power %e^u with a log in U and U depending on VAR, such as
;; the %e^(log(x)/3) that INTSETUP writes for x^(1/3), or if ODD, one for
;; which ODD-ROOT-LOG-TERM-P of u is true.
(defun exp-log-power-p (e var &optional odd)
  (cond ((atom e) nil)
        ((specrepp e) (exp-log-power-p (specdisrep e) var odd))
        ((and (mexptp e) (eq (cadr e) '$%e)
              (if odd
                  (odd-root-log-term-p (caddr e))
                  (and (isinop (caddr e) '%log) (not (freeof var e))))))
        (t (some #'(lambda (a) (exp-log-power-p a var odd)) (cdr e)))))

(defun rischint (exp risch-intvar &optional trig
                     &aux ($logarc nil) ($exponentialize nil)
                     ($gcd '$algebraic) ($algebraic t) (implicit-real t)
                     ($float nil) ($numer nil)
                     (*alias-expressions* *alias-expressions*)
                     ;; The risch integrator expects $logexpand T. Otherwise,
                     ;; the integrator hangs for special types of integrals
                     ;; (See bug report ID:3039452)
                     ($logexpand t))
  (prog ($%e_to_numlog $logsimp risch-y z risch-var risch-ratform risch-liflag
	 branch risch-mainvar varlist genvar $ratfac $ratalgdenom risch-degree
	 rischform-value risch-trigint risch-hypertrigint risch-operator)
     (declare (ignorable rischform-value))
     (if (specrepp exp)
	 (setq exp (specdisrep exp)))
     (if (specrepp risch-intvar)
	 (setq risch-intvar (specdisrep risch-intvar)))
     (if (mnump risch-intvar)
	 (merror (intl:gettext "risch: attempt to integrate wrt a number: ~:M") risch-intvar))
     ;; As in SININT, integrate with respect to an atomic gensym instead of a
     ;; non-atomic variable like r[1] or r(x).
     (when (and (not (atom risch-intvar))
                (not (member (caar risch-intvar) '(mplus mtimes mexpt))))
       (let ((dummy-intvar (make-new-var "rischint" risch-intvar)))
         (return (maxima-substitute risch-intvar dummy-intvar
                   (rischint (maxima-substitute dummy-intvar risch-intvar exp)
                             dummy-intvar)))))
     (if (and (atom risch-intvar)
	      (isinop exp risch-intvar))
	 (go noun))
     (multiple-value-setq (rischform-value risch-trigint risch-hypertrigint risch-operator)
       (rischform exp risch-intvar))
     (cond (risch-trigint
	    (return (trigin1 exp risch-intvar)))
	   (risch-hypertrigint
	    (let* ((n *risch-partial-count*)
		   (r (hypertrigint1 exp risch-intvar t)) p)
	      ;; If that leaves an integral and RISCHEXPVAR could have used a
	      ;; partial solution, try again with partial solutions, and take
	      ;; that result if it leaves none. Otherwise the second try would
	      ;; repeat the first.
	      (when (and (isinop r '%integrate) (> *risch-partial-count* n))
		(setq p (let ((*risch-partial* t) ($errormsg nil))
			  (car (errcatch
				(hypertrigint1 exp risch-intvar t))))))
	      (return (if (and p (not (isinop p '%integrate))) p r))))
	   (risch-operator
	    (go noun)))
     (multiple-value-setq (risch-y risch-operator branch)
       (intsetup exp risch-intvar))
     (if risch-operator
	 (go noun))
     (setq risch-ratform (car risch-y))
     (setq varlist (caddr risch-ratform))
     (setq risch-mainvar (caadr (ratf risch-intvar)))
     (setq genvar (cadddr risch-ratform))
     (unless (some #'algpget varlist)
       (setq $algebraic nil)
       (setq $gcd (car *gcdl*)))
     (setq risch-var (getrischvar))
     (setq z (tryrisch (cdr risch-y) risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var))
     (setf (caddr risch-ratform) varlist)
     (setf (cadddr risch-ratform) genvar)
     (setq z (cond ((atom (cdr z))
		    (disrep (car z) risch-ratform))
		   (t
		    ;; INTSETUP writes a radical such as x^(1/3) as
		    ;; %e^(log(x)/3), which %e_to_numlog turns back, but SIMPLIFY
		    ;; does so only in the parts of the answer that are not
		    ;; simplified yet. So resimplify all of it, unless the
		    ;; integrand has such a power on the principal branch, see
		    ;; ODD-ROOT-LOG-TERM-P, which must stay as it is.
		    (let* (($logsimp t)
			   ($%e_to_numlog (not (exp-log-power-p exp nil t)))
			   (ans (list* '(mplus)
				       (disrep (car z) risch-ratform)
				       (cdr z))))
		      (if (and $%e_to_numlog (exp-log-power-p ans risch-intvar))
			  (resimplify ans)
			  (simplify ans))))))
     ;; Multiply a result free of integrals by the factor by which INTSETUP
     ;; changed the integrand, if it is constant, but not for the exponential
     ;; form of a trigonometric integrand, where it makes the result very
     ;; large.
     (when (and (not trig) (freeof '%integrate z)
                (setq branch (let (varlist genvar ($gcd '$algebraic))
                               (branch-factor exp branch risch-intvar))))
       (setq z (let (($%e_to_numlog t) ($logsimp t) ($algebraic nil))
                 (sratsimp (mul branch z)))))
     (return z)
   noun
     (return (list '(%integrate) exp risch-intvar))))

(defun rischform (l risch-intvar)
  (let (risch-trigint risch-hypertrigint risch-operator)
    (labels
	((rischform-impl (l risch-intvar)
	   (cond ((or (atom l)
		      (alike1 risch-intvar l)
		      (freeof risch-intvar l))
		  nil)
		 ((polylogp l)
		  (if (and (integerp (car (subfunsubs l)))
			   (signp g (car (subfunsubs l))))
		      (rischform-impl (car (subfunargs l)) risch-intvar)
		      (setq risch-operator t)))
		 ((atom (caar l))
		  (case (caar l)
		    ((%sin %cos %tan %cot %sec %csc)
		     (setq risch-trigint t $exponentialize t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%asin %acos %atan %acot %asec %acsc)
		     (setq risch-trigint t $logarc t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%sinh %cosh %tanh %coth %sech %csch)
		     (setq risch-hypertrigint t $exponentialize t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%asinh %acosh %atanh %acoth %asech %acsch)
		     (setq risch-hypertrigint t $logarc t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((mtimes mplus mexpt rat %erf %log)
		     (mapc #'(lambda (e)
			       (rischform-impl e risch-intvar))
			   (cdr l)))
		    (t
		     (setq risch-operator (caar l)))))
		 (t
		  (setq risch-operator (caar l))))))
      (values (rischform-impl l risch-intvar)
	      risch-trigint
	      risch-hypertrigint
	      risch-operator))))

(defun hypertrigint1 (exp risch-var hyperfunc)
  (let ((result (if hyperfunc
                    (sinint (resimplify exp) risch-var)
                    (rischint (resimplify exp) risch-var t))))
    ;; The result can contain solvable integrals. Look for this case.
    (if (isinop result '%integrate)
        ;; Found an integral. Evaluate the result again.
        ;; Set the flag *in-risch-p* to make sure that we do not call
        ;; rischint again from the integrator. This avoids endless loops.
        (let ((*in-risch-p* t)) 
          (meval (list '($ev) result '$nouns)))
        result)))

(defun trigin1 (risch-*exp risch-var)
  (let ((yyy (hypertrigint1 risch-*exp risch-var nil)))
    (setq yyy (multiple-value-bind (num denom)
		  (with-default-quotient-dispflags (num-denom-split yyy))
		(div ($expand num) ($expand denom))))
    (let ((rischp risch-var)
	  (rp-polylogp t)
	  (*risplit-noting* nil) (*risplit-assumed* nil)
	  (*risplit-var-pos* nil) parts
	  $logarc $exponentialize result)
     ;; Split YYY first, as the IF below needs *RISPLIT-VAR-POS*.
     (unless (decl-complexp risch-var)
       (setq parts (let (sign-imag-errp (*risplit-noting* t))
                     (catch 'sign-imag-err (risplit yyy)))))
     (if (or (decl-complexp risch-var)
             (and (eq $domain '$complex) *risplit-var-pos*
                  (freeof '%integrate yyy) (freeof '$li yyy)))
      ;; RISPLIT's parts are the real and imaginary parts of YYY only for a
      ;; real variable, and only for a positive one if ABSARG takes it to be
      ;; positive. For a variable declared complex or imaginary, or with
      ;; domain complex if the split takes the variable to be positive and
      ;; YYY holds no integral, rewrite the exponentials with demoivre
      ;; instead, which holds for all complex values, and use that if it is
      ;; free of %i up to a constant. Not if YYY has a polylogarithm, as
      ;; for log(tan(x)): written with tan(x), it holds on one period only.
      (let ((d (cdr (partition (sratsimp ($demoivre yyy)) risch-var 0))))
        (setq result (if (freeof '$%i d) d (sratsimp yyy))))
     ;; Split into real and imaginary part. This automatically converts back to
     ;; trigonometric functions, which were converted to exponential form before.
     ;; RISPLIT can ask sign() about an expression that is not real, which
     ;; throws to SIGN-IMAG-ERR above. The integral is then left unevaluated.
     (destructuring-let (((yyy-re . yyy-im)
                          (if (consp parts)
                              parts
                              (return-from trigin1
                                (list '(%integrate) risch-*exp risch-var)))))
      (flet ((locally-const-p (e f)
        "E is locally constant iff its derivative vanishes."
        ;; If RISPLIT left a noun-form %REALPART or %IMAGPART, the result is
        ;; YYY below, so do not test the parts.
        (unless (and (freeof '%realpart yyy-re) (freeof '%imagpart yyy-im))
          (return-from locally-const-p nil))
        ;; F, the same part of the integrand, is the derivative of E. Keep E
        ;; if RADCAN1 leaves F nonzero, which is always safe. Drop it if
        ;; RADCAN1 reduces F to zero, unless a split took something to be
        ;; real or nonnegative without proof.
        (let ((d (and f (freeof '%realpart f) (freeof '%imagpart f)
                      (let (($errormsg nil) ($exponentialize t) ($logarc t))
                        (car (errcatch (radcan1 (resimplify f) risch-var)))))))
          (when d
            (cond ((not (zerop1 d)) (return-from locally-const-p nil))
                  ((not *risplit-assumed*) (return-from locally-const-p t)))))
        (let ((e (let (($exponentialize t) ($logarc t))
                   (radcan1 (resimplify e) risch-var))))
          (or (freeof risch-var e)
              ;; RISCHINT binds $LOGSIMP to false, which keeps forms such as
              ;; (%e^(log(u)/2))^2 in the derivative of a part. Write them as
              ;; powers of U first if the part holds an integral, which
              ;; would otherwise stay in the result.
              (zerop1 (radcan1 (if (freeof '%integrate e)
                                   (sdiff e risch-var)
                                   (let (($logsimp t) ($%e_to_numlog t))
                                     (resimplify (sdiff e risch-var))))
                               risch-var))))))
      ;; Strip locally constant parts (unsolved integrals survive this).
      ;; ERRCATCH: In case of an error (occurs during test suite, treat the part
      ;; as non-constant - keeping is always safe, dropping is not.
      ;; F holds the parts of the integrand, or NIL.
      (let* ((f (let ((s (let (sign-imag-errp (*risplit-noting* t))
                           (catch 'sign-imag-err (risplit risch-*exp)))))
                  (and (consp s) s)))
             (yyy-re (if (car (errcatch (locally-const-p yyy-re (car f))))
                         0 yyy-re))
             (yyy-im (if (car (errcatch (locally-const-p yyy-im (cdr f))))
                         0 yyy-im)))
      ;; If YYY-RE contains a noun-form %REALPART or YYY-IM contains a noun-form
      ;; %IMAGPART, then just work with YYY instead of YYY-RE + %i * YYY-IM.
      (setq result (if (and (freeof '%realpart yyy-re)
                            (freeof '%imagpart yyy-im))
                       ;; With both parts, the polynomials of the sum hold
                       ;; %i, and RISCHINT's algebraic gcd then takes all
                       ;; their gcds by the subresultant algorithm, which
                       ;; can exhaust the heap. The default gcd does so only
                       ;; for gcds without cofactors.
                       (let (($gcd (if (or (zerop1 yyy-re) (zerop1 yyy-im))
                                       $gcd
                                       '$spmod)))
                         (sratsimp (add yyy-re (mul '$%i yyy-im))))
                       (sratsimp yyy)))))))
      ;; The result can contain solvable integrals. Look for this case.
      (if (isinop result '%integrate)
          ;; Found an integral. Evaluate the result again.
          ;; Set the flag *in-risch-p* to make sure that we do not call
          ;; rischint again from the integrator. This avoids endless loops.
          ;; Simplify %e^(r*log(u)) to u^r, which RISCHINT's bindings prevent.
          (let ((*in-risch-p* t) ($logsimp t) ($%e_to_numlog t))
            (sratsimp (meval (list '($ev) result '$nouns))))
          result))))

(defun tryrisch (exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var)
  (prog (risch-logptdx risch-expflag risch-expstuff risch-expint risch-y)
     (setq risch-expstuff '(0 . 1))
     (cond ((eq risch-mainvar risch-var)
	    (return (rischfprog exp risch-ratform risch-var)))
	   ((eq (get risch-var 'leadop)
		'mexpt)
	    (setq risch-expflag t)))
     (multiple-value-setq (risch-y risch-logptdx risch-expint)
       (rischlogdprog exp risch-ratform risch-intvar risch-liflag risch-var
		      risch-expflag risch-mainvar risch-expint risch-degree))
     (dolist (rat risch-logptdx)
       (let (rischlogeprog-value)
         (declare (ignorable rischlogeprog-value))
	 (setq risch-y
	       (rischadd (multiple-value-setq (rischlogeprog-value risch-expint risch-expstuff)
			   (rischlogeprog rat risch-ratform nil risch-intvar risch-expstuff
					  risch-var risch-expflag risch-mainvar risch-expint))
			 risch-y))))
     (if varlist
	 (setq risch-y (rischadd (tryrisch1 risch-expstuff risch-mainvar
					    risch-ratform risch-intvar risch-liflag
					    risch-degree)
			   risch-y)))
     (return (if risch-expint
		 (rischadd (rischexppoly risch-expint risch-var
					 risch-ratform risch-intvar risch-liflag
					 risch-degree risch-mainvar)
			   risch-y)
		 risch-y))))

(defun tryrisch1 (exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree)
  (let* ((varlist (reverse (cdr (reverse varlist))))
	 (risch-var (getrischvar)))
    (tryrisch exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var)))

(defun rischfprog (rat risch-ratform risch-var)
  (multiple-value-bind (dprog-ret risch-logptdx)
      (dprog rat risch-ratform risch-var)
    (cons (cdr (ratrep* dprog-ret))
	  (let ((varlist varlist)
		(genvar (subseq genvar 0 (length varlist))))
	    (mapcar #'(lambda (p)
			(eprog p risch-ratform risch-var nil))
		    risch-logptdx)))))

(defun rischlogdprog (ratarg risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree)
  (prog (arootf deriv thebpg thetop thebot prod1 prod2 ans
	 risch-wholepart risch-logptdx risch-parnumer risch-pardenom
	 risch-rootfactor rischlogpoly-value)
     (setq ans '(0 . 1))
     (cond ((or (pcoefp (cdr ratarg))
		(pointergp risch-var (cadr ratarg)))
	    (multiple-value-setq (rischlogpoly-value risch-expint)
	      (rischlogpoly ratarg risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
	    (return (values rischlogpoly-value
			    risch-logptdx
			    risch-expint))))

     (multiple-value-setq (risch-rootfactor risch-pardenom)
       (aprog (ratdenominator ratarg) risch-var))
     (multiple-value-setq (risch-parnumer risch-wholepart)
       (cprog (ratnumerator ratarg)
	      (ratdenominator ratarg)
	      risch-var
	      risch-pardenom))
     (do ((risch-rootfactor (reverse risch-rootfactor) (cdr risch-rootfactor))
	  (risch-parnumer (reverse risch-parnumer) (cdr risch-parnumer))
	  (risch-klth (length risch-rootfactor) (1- risch-klth)))
	 ((= risch-klth 1))
       (setq arootf (car risch-rootfactor))
       (cond
	 ((pcoefp arootf))
	 ((and (eq (get (car arootf) 'leadop) 'mexpt)
	       (null (cdddr arootf)))
	  (setq
	   risch-expint
	   (append
	    (cond ((and (not (atom (car risch-parnumer)))
			(not (atom (caar risch-parnumer)))
			(eq (caaar risch-parnumer) (car arootf)))
		   (gennegs arootf (cdaar risch-parnumer) (cdar risch-parnumer) risch-klth))
		  (t (list
		      (list 'neg (car risch-parnumer)
			    (car arootf) risch-klth (cadr arootf)))))
	    risch-expint)))
	 ((not (zerop (pdegree arootf risch-var)))
	  (setq deriv (spderivative arootf risch-mainvar))
	  (setq thebpg (bprog arootf (ratnumerator deriv) risch-var))
	  (setq thetop (car risch-parnumer))
	  (do ((kx (1- risch-klth) (1- kx))) ((= kx 0))
	    (setq prod1 (r* thetop (car thebpg)))
	    (setq prod2 (r* thetop (cdr thebpg) (ratdenominator deriv)))
	    (setq thebot (pexpt arootf kx))
	    (setq ans (r+ ans (ratqu (r- prod2) (r* kx thebot))))
	    (setq thetop
		  (r+ prod1 (ratqu (spderivative prod2 risch-mainvar) kx)))
	    (setq thetop (cdr (ratdivide thetop thebot))))
	  (push (ratqu thetop arootf) risch-logptdx))))
     (push (ratqu (car risch-parnumer) (car risch-rootfactor)) risch-logptdx)
     (cond ((or (pzerop ans) (pzerop (car ans)))
	    (multiple-value-setq (rischlogpoly-value risch-expint)
	      (rischlogpoly risch-wholepart risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
	    (return (values rischlogpoly-value
			    risch-logptdx
			    risch-expint))))
     (setq thetop (cadr (pdivide (ratnumerator ans)
				 (ratdenominator ans))))
     (multiple-value-setq (rischlogpoly-value risch-expint)
       (rischlogpoly risch-wholepart risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
     (return (values (rischadd (ncons (ratqu thetop (ratdenominator ans)))
			       rischlogpoly-value)
		     risch-logptdx
		     risch-expint))))

(defun gennegs (denom num numdenom risch-klth)
  (cond ((null num) nil)
	(t (cons (list 'neg (cadr num)
		       (car denom)
		       (- risch-klth (car num))
		       (r* numdenom (caddr denom) ))
		 (gennegs denom (cddr num) numdenom risch-klth)))))

(defun rischlogeprog (p risch-ratform risch-switch1 risch-intvar risch-expstuff
		      risch-var risch-expflag risch-mainvar risch-expint)
  (labels
      ((impl (p risch-switch1)
	 (prog (p1e p2e p2deriv logcoef ncc dcc allcc expcoef my-divisor
		risch-parnumer risch-pardenom)
	    (if (or (pzerop p) (pzerop (car p)))
		(return (rischzero)))
	    (setq p1e (ratnumerator p))
	    (desetq (dcc p2e) (oldcontent (ratdenominator p)))
	    (cond ((and (not risch-switch1)
			(cdr (setq risch-pardenom (intfactor p2e))))
		   (setq risch-parnumer nil)
		   (setq risch-switch1 t)
		   (desetq (ncc p1e) (oldcontent p1e))
		   (multiple-value-setq (risch-parnumer)
		     (cprog p1e p2e risch-var risch-pardenom))
		   (setq allcc (ratqu ncc dcc))
		   (return (do ((pnum risch-parnumer (cdr pnum))
				(pden risch-pardenom (cdr pden))
				(ans (rischzero)))
			       ((or (null pnum) (null pden))
				(setq risch-switch1 nil) ans)
			     (setq ans (rischadd
					(impl
					 (r* allcc (ratqu (car pnum) (car pden)))
					 risch-switch1)
					ans))))))
	    (when (and risch-expflag (null (p-red p2e)))
	      (push (cons 'neg p) risch-expint)
	      (return (rischzero)))
	    (if risch-expflag
		(setq expcoef (r* (p-le p2e) (ratqu (get risch-var 'rischdiff)
						    (make-poly risch-var)))))
	    (setq p1e (ratqu p1e (ptimes dcc (p-lc p2e)))
		  p2e (ratqu p2e (p-lc p2e))) ;MAKE DENOM MONIC
	    (setq p2deriv (spderivative p2e risch-mainvar))
	    (setq my-divisor (if risch-expflag
				 (r- p2deriv (r* p2e expcoef))
				 p2deriv))
	    (when (equal my-divisor '(0 . 1))
	      ;; (format t "HEY RISCHLOGEPROG, FOUND ZERO DIVISOR; GIVE UP.~%")
	      (return (rischnoun p risch-ratform risch-intvar)))
	    (setq logcoef (ratqu p1e my-divisor))
	    (when (risch-constp logcoef risch-mainvar)
	      (if risch-expflag
		  (setq risch-expstuff (r- risch-expstuff (r* expcoef logcoef))))
	      (return
		(list
		 '(0 . 1)
		 (list '(mtimes)
		       (disrep logcoef risch-ratform)
		       (logmabs (disrep p2e risch-ratform))))))
	    (if (and risch-expflag
		     $liflag
		     *changevp*)
		(let* ((newvar (gensym))
		       (new-int ($changevar
				 `((%integrate) ,(simplify (disrep p risch-ratform)) ,risch-intvar)
				 (sub newvar (get risch-var 'rischexpr))
				 newvar risch-intvar))
		       (*changevp* nil)) ;prevents recursive changevar
		  (if (and (freeof risch-intvar new-int)
			   (freeof '%integrate
				   (setq new-int (rischint (sdiff new-int newvar)
							   newvar))))
		      (return
			(list (rzero)
			      (maxima-substitute (get risch-var 'rischexpr) newvar new-int))))))
	    (return (rischnoun p risch-ratform risch-intvar)))))
    (values (impl p risch-switch1)
	    risch-expint risch-expstuff)))


(defun findint (exp)
  (cond ((atom exp) nil)
	((atom (car exp)) (findint (cdr exp)))
	((eq (caaar exp) '%integrate) t)
	(t (findint (cdr exp)))))

(defun logequiv (fn1 fn2 risch-intvar)
  (freeof risch-intvar ($ratsimp (div* (remabs (leadarg fn1))
				 (remabs (leadarg fn2))))))

(defun remabs (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mabs) (cadr exp))
	(t exp)))

(defun getfnsplit (l risch-intvar)
  (let (coef fn)
    (dolist (x l (values (muln coef nil) (muln fn nil)))
      (if (free x risch-intvar)
          (push x coef)
          (push x fn)))))

(defun getfncoeff (a form risch-intvar risch-liflag risch-degree risch-cary risch-nogood risch-lians)
  (labels
      ((getfncoeff-impl (a)
	 (cond ((null a) 0)
	       ((equal (car a) 0)
		(getfncoeff-impl (cdr a)))
	       ((and (listp (car a))
		     (eq (caaar a) 'mplus)
		     (ratpl (getfncoeff-impl (cdar a))
			    (getfncoeff-impl (cdr a)))))
	       ((and (listp (car a))
		     (eq (caaar a) 'mtimes))
		(multiple-value-bind (coef newfn)
		    (getfnsplit (cdar a) risch-intvar)
		  ;; (car a) is a mtimes expression. We insert coef and newfn as the
		  ;; new arguments to the mtimes expression. This causes problems if
		  ;;   (1) coef is a mtimes expression too and
		  ;;   (2) (car a) has already a simp flag
		  ;; We get a nested mtimes expression, which does not sgetfncoeff-implify.
		  ;; We comment out the following code (DK 09/2009):
		  ;; (setf (cdar a) (list coef newfn))
	   
		  ;; Insert a complete mtimes expression without simpflag.
		  ;; Nested mtimes expressions sgetfncoeff-implify further.
		  (setf (car a) (list '(mtimes) coef newfn))
	   
		  (setf (cdar a) (list coef newfn))
		  (cond ((zerop1 coef)
			 (getfncoeff-impl (cdr a)))
			((and (matanp newfn)
			      (member '$%i varlist :test #'eq))
			 (let (($logarc t) ($logexpand '$all))
			   (rplaca a ($expand (resimplify (car a)))))
			 (getfncoeff-impl a))
			((and (alike1 (leadop newfn) (leadop form))
			      (or (alike1 (leadarg newfn) (leadarg form))
				  (and (mlogp newfn)
				       (logequiv form newfn risch-intvar))))
			 (ratpl (rform coef)
				(prog2 (rplaca a 0)
				    (getfncoeff-impl (cdr a)))))
			((do ((vl varlist (cdr vl)))
			     ((null vl))
			   (and (not (atom (car vl)))
				(alike1 (leadop (car vl)) (leadop newfn))
				(if (mlogp newfn)
				    (logequiv (car vl) newfn risch-intvar)
				    (alike1 (car vl) newfn))
				(rplaca (cddar a) (car vl))
				(return nil))))
			((let (vlist)
			   (declare (special vlist))
			   ;; newvar1 accesses the special var vlist.
			   (newvar1 (car a))
			   (null vlist))
			 (setq risch-cary
			       (ratpl (cdr (ratrep* (car a)))
				      risch-cary))
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			((and risch-liflag
			      (mlogp form)
			      (mlogp newfn))
			 (let (res)
			   (multiple-value-setq (res risch-nogood)
			     (dilog (cons (car a) form) risch-intvar risch-degree risch-nogood))
			   (push res risch-lians))
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			((and risch-liflag
			      (polylogp form)
			      (mlogp newfn)
			      (logequiv form newfn risch-intvar))
			 (push (mul* (cadar a) (make-li (1+ (car (subfunsubs form)))
							(leadarg form)))
			       risch-lians)
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			(t (setq risch-nogood t) 0))))
	       (t
		(rplaca a (list '(mtimes) 1 (car a)))
		(getfncoeff-impl a)))))
    (values (getfncoeff-impl a) risch-cary risch-nogood
	    risch-lians)))


(defun rischlogpoly (exp risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree)
  (let
      ((result
	 (cond ((equal exp '(0 . 1))
		(rischzero))
	       (risch-expflag
		(push (cons 'poly exp) risch-expint)
		(rischzero))
	       ((not (among risch-var exp))
		(tryrisch1 exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
	       (t
		(do ((risch-degree (pdegree (car exp) risch-var) (1- risch-degree))
		     (p (car exp))
		     (den (cdr exp))
		     (risch-lians ())
		     (sum (rzero))
		     (risch-cary (rzero))
		     (risch-y)
		     (z)
		     (ak)
		     (risch-nogood)
		     (lbkpl1)
		     (lians-prev))
		    ((minusp risch-degree)
		     (cons sum (append risch-lians (cdr risch-y))))
		  (setq ak (r- (ratqu (polcoef p risch-degree risch-var) den)
			       (r* (cons (1+ risch-degree) 1)
				   risch-cary
				   (get risch-var 'rischdiff))))
		  (if (not (pzerop (polcoef p risch-degree risch-var)))
		      (setq p (if (pcoefp p)
				  (pzero)
				  (psimp risch-var (p-red p)))))
		  (setq risch-y (tryrisch1 ak risch-mainvar
					   risch-ratform risch-intvar risch-liflag risch-degree))
		  (setq risch-cary (car risch-y))
		  (and (> risch-degree 0)
		       (setq risch-liflag $liflag))
	   
		  (setq lians-prev risch-lians)
		  (multiple-value-setq (z risch-cary risch-nogood risch-lians)
		    (getfncoeff (cdr risch-y)
				(get risch-var 'rischexpr)
				risch-intvar risch-liflag risch-degree risch-cary risch-nogood
				risch-lians))
		  (setq risch-liflag nil)
		  (cond ((and (> risch-degree 0)
			      (or risch-nogood
				  (findint (cdr risch-y))))
			 (return (rischadd
				  (rischnoun sum risch-ratform risch-intvar
					    (r+ (r* ak
						    (make-poly risch-var risch-degree 1))
						(ratqu p den)))
						(cons (rzero) lians-prev)))))
		  (setq lbkpl1 (ratqu z (cons (1+ risch-degree) 1)))
		  (setq sum (r+ (r* lbkpl1 (make-poly risch-var (1+ risch-degree) 1))
				(r* risch-cary (if (zerop risch-degree) 1
						   (make-poly risch-var risch-degree 1)))
				sum)))))))
    (values result risch-expint)))

(defun make-li (sub arg)
  (subfunmake '$li (ncons sub) (ncons arg)))

;;integrates log(ro)^risch-degree*log(rn)' in terms of polylogs
;;finds constants c,d and integers j,k such that
;;c*ro^j+d=rn^k  If ro and rn are poly's then can assume either j=1 or k=1
(defun dilog (l risch-intvar risch-degree risch-nogood)
  (destructuring-let* ((((nil coef nlog) . olog) l)
		       (narg (remabs (cadr nlog)))
		       (varlist varlist)
		       (genvar genvar)
		       (rn (rform narg))	;; can add new vars to varlist
		       (ro (rform (cadr olog)))
		       (risch-var (caar ro))
		       ((j . k) (ratreduce (pdegree (car rn) risch-var) (pdegree (car ro) risch-var)))
		       (idx (gensym))
		       (rc) (rd))
    (cond ((and (= j 1) (> k 1))
	   (setq rn (ratexpt rn k)
		 coef (div coef k)
		 narg (rdis rn)))
	  ((and (= k 1) (> j 1))
	   (setq ro (ratexpt ro j)
		 coef (div coef (power j risch-degree))
		 olog (mul j olog))))
    (desetq (rc . rd) (ratdivide rn ro))
    (let
	((result
	  ;; RN must be a polynomial in RISCH-VAR as RO is, as RATDIVIDE divides by
	  ;; their main variables.
	  (cond ((and (not (pcoefp (car rn)))
		      (eq (p-var (car rn)) risch-var)
		      (freeof risch-intvar (rdis rc)) ;; can't use risch-constp because varlist
		      (freeof risch-intvar (rdis rd))) ;; is not set up with vars in correct order.
		 (setq narg ($ratsimp (sub 1 (div narg (rdis rd)))))
		 (mul* coef (power -1 (1+ risch-degree))
		       `((mfactorial) ,risch-degree)
		       (dosum (mul* (power -1 idx)
				    (div* (power olog idx)
					  `((mfactorial) ,idx))
				    (make-li (add risch-degree (neg idx) 1) narg))
			      idx 0 risch-degree t)))
		(t
		 (setq risch-nogood t)
		 0))))
      (values result risch-nogood))))

(defun exppolycontrol (flag f a expg n risch-ratform risch-intvar risch-liflag risch-degree risch-mainvar)
  (let (risch-y l risch-var (varlist varlist) (genvar genvar))
    (setq varlist (reverse (cdr (reverse varlist))))
    (setq risch-var (getrischvar))
    (setq risch-y (get risch-var 'leadop))
    (cond ((and (not (pzerop (ratnumerator f)))
		(risch-constp (setq l (ratqu a f)) risch-mainvar))
	   (cond (flag		;; multiply in expg^n - n may be negative
		  (list (r* l (ratexpt (cons (list expg 1 1) 1) n))
			0))
		 (t l)))
	  ((eq risch-y risch-intvar)
	   (rischexpvar flag (list f a expg n) risch-ratform risch-intvar risch-y risch-var risch-mainvar))
	  (t
	   (rischexplog (eq risch-y 'mexpt) flag f a
			(list expg n (get risch-var 'rischarg)
			      risch-var (get risch-var 'rischdiff))
			risch-ratform risch-intvar
			risch-liflag
			risch-degree
			risch-y
			risch-var
			risch-mainvar)))))

(defun rischexppoly (risch-expint risch-var risch-ratform risch-intvar risch-liflag risch-degree risch-mainvar)
  (let (risch-y
	w
	num
	denom type
	(ans (rischzero))
	(expdiff (ratqu (get risch-var 'rischdiff) (list risch-var 1 1))))
    (do ((risch-expint risch-expint (cdr risch-expint)))
	((null risch-expint)
	 ans)
      (desetq (type . risch-y) (car risch-expint))
      (desetq (num . denom) (ratfix risch-y))
      (cond ((eq type 'neg)
	     (setq w (exppolycontrol t
				     (r* (- (cadr denom))
					 expdiff)
				     (ratqu num (caddr denom))
				     risch-var
				     (- (cadr denom))
				     risch-ratform
				     risch-intvar
				     risch-liflag
				     risch-degree
				     risch-mainvar)))
	    ((or (numberp num)
		 (not (eq (car num) risch-var)))
	     (setq w (tryrisch1 risch-y risch-mainvar
				risch-ratform risch-intvar risch-liflag risch-degree)))
	    (t
	     (setq w (rischzero))
	     (do ((num (cdr num) (cddr num)))
		 ((null num))
	       (cond ((equal (car num) 0)
		      (setq w (rischadd
			       (tryrisch1 (ratqu (cadr num) denom)
					  risch-mainvar
					  risch-ratform risch-intvar risch-liflag risch-degree)
			       w)))
		     (t
		      (setq w (rischadd (exppolycontrol
					 t
					 (r* (car num) expdiff)
					 (ratqu (cadr num) denom)
					 risch-var
					 (car num)
					 risch-ratform
					 risch-intvar
					 risch-liflag
					 risch-degree
					 risch-mainvar)
					w)))))))
      (setq ans (rischadd w ans)))))

(defun rischexpvar (flag l risch-ratform risch-intvar risch-y risch-var risch-mainvar)
  (prog (lcm risch-m p risch-alphar risch-gamma delta r s fromlsa
	 tt denom k wl wv i ytemp ttemp yalpha f a expg n yn yd
	 risch-beta)
     (desetq (f a expg n) l)
     (cond ((or (pzerop a) (pzerop (car a)))
	    (return (cond ((null flag) (rzero))
			  (t (rischzero))))))
     (setq denom (ratdenominator f))
     (multiple-value-setq (p risch-alphar)
       (findpr (cdr (partfrac a risch-mainvar))
	       (cdr (partfrac f risch-mainvar))
	       risch-y
	       risch-mainvar))
     (setq lcm (plcm (ratdenominator a) p))
     (setq risch-y (ratpl (spderivative (cons 1 p) risch-mainvar)
			  (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator risch-y)))
     (setq r (pquotient lcm p))
     (setq s (ptimes (ratnumerator risch-y)
                     (pquotient lcm (ratdenominator risch-y))))
     (setq tt (ptimes (ratnumerator a) (pquotient lcm (ratdenominator a))))
     (setq risch-beta (pdegree r risch-mainvar))
     (setq risch-gamma (pdegree s risch-mainvar))
     (setq delta (pdegree tt risch-mainvar))
     (setq risch-alphar (max (- (1+ delta) risch-beta)
			     (- delta risch-gamma)))
     (setq risch-m 0)
     (cond ((equal (1- risch-beta) risch-gamma)
	    (setq risch-y (r* -1
			      (ratqu (polcoef s risch-gamma risch-var)
				     (polcoef r risch-beta risch-var))))
	    (and (equal (cdr risch-y) 1)
		 (numberp (car risch-y))
		 (setq risch-m (car risch-y)))))
     (setq risch-alphar (max risch-alphar risch-m))
     (if (minusp risch-alphar)
	 (return (if flag
		     (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)
		     nil)))
     (cond ((not (and (equal risch-alphar risch-m)
		      (not (zerop risch-m))))
	    (go down2)))
     (setq k (+ risch-alphar risch-beta -1))
     (setq wl nil)
   l2
     (setq wv (list (cons (polcoef tt k risch-var) 1)))
     (setq i risch-alphar)
   l1
     (setq wv
	   (cons (r+ (r* (cons i 1)
			 (polcoef r (+ k 1 (- i)) risch-var))
		     (cons (polcoef s (+ k (- i)) risch-var) 1))
		 wv))
     (decf i)
     (cond ((> i -1)
	    (go l1)))
     (setq wl (cons wv wl))
     (decf k)
     (cond ((> k -1)
	    (go l2)))
     (multiple-value-setq (risch-y risch-m)
       (lsa wl))
     ;; If the top equation, which has no unknowns, fails, solve the others.
     ;; RISCHEXPLOG, which calls this via EXPPOLYCONTROL, checks the remainder.
     (when (and (null flag) (eq risch-y 'inconsistent))
       (incf *risch-partial-count*)
       (when *risch-partial*
	 (multiple-value-setq (risch-y risch-m) (lsa (butlast wl)))))
     ;; LSA calls the system singular when it has more than one solution.
     ;; Here the antiderivatives those solutions give differ only by a
     ;; constant, so any one of them will do, and the DOWN2 walk finds one.
     ;; The walk answers only when it solves the equation exactly, so it is
     ;; also safe to let it try when LSA calls the system inconsistent.
     (if (or (eq risch-y 'singular)
	     (eq risch-y 'inconsistent))
	 (progn (setq fromlsa t)
		(go down2)))
     (setq k 0)
     (setq lcm 0)
     (setq risch-y (cdr risch-y))
   l3
     (setq lcm
	   (r+ (r* (car risch-y) (pexpt (list risch-mainvar 1 1) k))
	       lcm))
     (incf k)
     (setq risch-y (cdr risch-y))
     (cond ((null risch-y)
	    (return (cond ((null flag)
			   (ratqu lcm p))
			  (t
			   (list (r* (ratqu lcm p)
				     (ratexpt (cons (list expg 1 1) 1) n))
				 0))))))
     (go l3)
   down2
     (cond ((or (pzerop s) (> (1- risch-beta) risch-gamma))
	    (setq k (+ risch-alphar (1- risch-beta)))
	    (setq denom #'(lambda ()
			    (ratti risch-alphar (polcoef r risch-beta risch-var) t))))
	   ((< (1- risch-beta) risch-gamma)
	    (setq k (+ risch-alphar risch-gamma))
	    (setq denom #'(lambda ()
			    (polcoef s risch-gamma risch-var))))
	   (t
	    (setq k (+ risch-alphar risch-gamma))
	    (setq denom
		  #'(lambda ()
		      (ratpl (ratti risch-alphar (polcoef r risch-beta risch-var) t)
			     (polcoef s risch-gamma risch-var))))))
     (setq risch-y (rzero))
   loop
     (setq yn (polcoef (ratnumerator tt) (if (zerop risch-alphar) risch-gamma k) risch-var)
      yd (r* (ratdenominator tt) ;DENOM MAY BE 0
             (cond ((zerop risch-alphar) (polcoef s risch-gamma risch-var))
                   (t (funcall denom)))))
     (cond ((rzerop yd)
	    (cond ((pzerop yn)
		   (setq k (1- k) risch-alphar (1- risch-alphar))
		   (go loop))		;need more constraints?
		  (t
		   (cond
		     ((null flag)
		      (return nil))
		     (t
		      (return (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)))))))
	   (t
	    (setq yalpha (ratqu yn yd))))
     (setq ytemp (r+ risch-y (r* yalpha
				 (cons (list risch-mainvar risch-alphar 1) 1) )))
     (setq ttemp (r- tt (r* yalpha
			    (r+ (r* s (cons (list risch-mainvar risch-alphar 1) 1))
				(r* r risch-alphar
				    (list risch-mainvar (1- risch-alphar) 1))))))
     (decf k)
     (decf risch-alphar)
     (cond ((< risch-alphar 0)
	    (cond
	      ((rzerop ttemp)
	       (cond
		 ((null flag)
		  (return (ratqu ytemp p)))
		 (t
		  (return (list (ratqu (r* ytemp (ratexpt (cons (list expg 1 1) 1) n))
				       p)
				0)))))
	      ((null flag)
	       (return nil))
	      ;; The LSA branch returns either a complete answer or none. A walk
	      ;; entered from there does the same and leaves the integral
	      ;; unevaluated when it does not finish.
	      (fromlsa
	       (return (cxerfarg (rzero) expg n a risch-ratform risch-intvar
				 risch-mainvar)))
	      ((and (risch-constp (setq ttemp (ratqu ttemp lcm)) risch-mainvar)
		    $erfflag
		    (equal (pdegree (car (get expg 'rischarg)) risch-mainvar) 2)
		    (equal (pdegree (cdr (get expg 'rischarg)) risch-mainvar) 0))
	       (return (list (ratqu (r* ytemp (ratexpt (cons (list expg 1 1) 1) n)) p)
			     (erfarg2 (r* n (get expg 'rischarg))
				      ttemp risch-ratform risch-intvar risch-mainvar))))
	      ;; With nothing accumulated in RISCH-Y, the last step alone is a
	      ;; single integration by parts, which usually leaves an integral no
	      ;; easier than the one it started from. Hand that one back unevaluated
	      ;; instead of splitting it.
	      ((rzerop risch-y)
	       (return (cxerfarg (rzero) expg n a risch-ratform risch-intvar
				 risch-mainvar)))
	      (t
	       ;; YTEMP and TTEMP are the pair the last step reached, one
	       ;; term further along than RISCH-Y and TT and just as
	       ;; consistent. TTEMP is already divided by LCM: the
	       ;; ERFARG2 clause above divides it in its own test.
	       (return
		 (cxerfarg
		  (ratqu (r* ytemp (ratexpt (cons (list expg 1 1) 1) n)) p)
		  expg
		  n
		  ttemp
		  risch-ratform
		  risch-intvar
		  risch-mainvar))))))
     (setq risch-y ytemp)
     (setq tt ttemp)
     (go loop)))


;; *JM should be declared as an array, although it is not created
;; by this file. -- cwh

(defun lsa (mm)
  (prog (d *mosesflag m2 risch-m)
     (setq d (length (car mm)))
     ;; MTOA stands for MATRIX-TO-ARRAY.  An array is created and
     ;; associated functionally with the symbol *JM.  The elements
     ;; of the array are initialized from the matrix MM.
     (mtoa '*jm* (length mm) d mm)
     (setq risch-m (tfgeli '*jm*  (length mm) d))
     (cond ((or (and (null (car risch-m)) (null (cadr risch-m))
		     (< (length mm) (1- d)))
		(and (car risch-m)
		     (> (length (car risch-m)) (- (length mm) (1- d)))))
	    (return (values 'singular risch-m)))
	   ((cadr risch-m) (return (values 'inconsistent risch-m))))
     (setq *mosesflag t)
     (ptorat '*jm* (1- d) d)
     (setq m2 (xrutout '*jm* (1- d) d nil nil))
     (setq m2 (lsafix (cdr m2) (caddr risch-m)))
     (return (values m2 risch-m))))

(defun lsafix (l n)
  (declare (special *jm*))
  (do ((n n (cdr n))
       (l l (cdr l)))
      ((null l))
    (setf (aref *jm* 1 (car n)) (car l)))
  (do ((s (length l) (1- s))
       (ans))
      ((= s 0) (cons '(list) ans))
    (setq ans (cons (aref *jm* 1 s) ans))))


(defun findpr (alist flist risch-y risch-mainvar &aux (p 1) fterm)
  (let (risch-alphar)
    (do ((alist alist (cdr alist))) ((null alist))
      (setq fterm (findflist (cadar alist) flist))
      (if fterm (setq flist (remove risch-y flist :count 1 :test #'eq)))
      (setq risch-alphar
	    (cond ((null fterm) (caddar alist))
		  ((equal (caddr fterm) 1)
		   (fpr-dif (car flist) (caddar alist) risch-mainvar))
		  (t (max (- (caddar alist) (caddr fterm)) 0))))
      (if (not (zerop risch-alphar))
	  (setq p (ptimes p (pexpt (cadar alist) risch-alphar)))))
    (do ((flist flist (cdr flist)))
	((null flist))
      (when (equal (caddar flist) 1)
	(setq risch-alphar (fpr-dif (car flist) 0 risch-mainvar))
	(setq p (ptimes p (pexpt (cadar flist) risch-alphar)))))
    (values p risch-alphar)))

(defun fpr-dif (fterm alpha risch-mainvar)
  (destructuring-let* (((num den mult) fterm)
		       (risch-m (spderivative den risch-mainvar))
		       (n))
    (cond ((rzerop risch-m) alpha)
	  (t (setq n (ratqu (cdr (ratdivide num den))
			    risch-m))
	     (if (and (equal (cdr n) 1) (numberp (car n)))
		 (max (car n) alpha)
		 alpha)))))

(defun findflist (a llist)
  (cond ((null llist) nil)
	((equal (cadar llist) a) (car llist))
	(t (findflist a (cdr llist)))))


(defun rischexplog (expexpflag flag f a l
		    risch-ratform risch-intvar risch-liflag risch-degree risch-y risch-var risch-mainvar)
  (prog (lcm yy p risch-gamma delta
	 mu r s tt denom ymu rbeta expg n eta logeta logdiff
	 temp risch-cary risch-nogood vector aarray rmu rrmu rarray
	 risch-beta
	 risch-lians
	 constmu secondpass firsty firsttt startmu starttt fromlsa)
     (desetq (expg n eta logeta logdiff) l)
     (cond ((or (pzerop a)
		(pzerop (car a)))
	    (return (cond ((null flag)
			   (rzero))
			  (t
			   (rischzero))))))
     (setq p
       (findpr (cdr (partfrac a risch-var))
	       (cdr (partfrac f risch-var))
	       risch-y
	       risch-mainvar))
     (setq lcm (plcm (ratdenominator a) p))
     (setq risch-y (ratpl (spderivative (cons 1 p) risch-mainvar)
			  (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator risch-y)))
     (setq r (pquotient lcm p))
     (setq s (ptimes (ratnumerator risch-y)
                     (pquotient lcm (ratdenominator risch-y))))
     (setq tt (ptimes (ratnumerator a) (pquotient lcm (ratdenominator a))))
     (setq risch-beta (pdegree r risch-var))
     (setq risch-gamma (pdegree s risch-var))
     (setq delta (pdegree tt risch-var))
     (cond (expexpflag
	    (setq mu (max (- delta risch-beta)
			  (- delta risch-gamma)))
	    (go expcase)))
     (setq mu (max (- (1+ delta) risch-beta)
		   (- (1+ delta) risch-gamma)))
     (cond ((< risch-beta risch-gamma)
	    (go back))
	   ((= (1- risch-beta) risch-gamma)
	    (go down1)))
     (setq risch-y (tryrisch1 (ratqu (r- (r* (polcoef r (1- risch-beta) risch-var)
					     (polcoef s risch-gamma risch-var))
					 (r* (polcoef r risch-beta risch-var)
					     (polcoef s (1- risch-gamma) risch-var)))
				     (r* (polcoef r risch-beta risch-var)
					 (polcoef r risch-beta risch-var) ))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (setq risch-cary (car risch-y))
     (multiple-value-setq (yy risch-cary risch-nogood risch-lians)
       (getfncoeff (cdr risch-y)
		   (get risch-var 'rischexpr)
		   risch-intvar risch-liflag risch-degree risch-cary risch-nogood
		   risch-lians))
     (cond ((and (not (findint (cdr risch-y)))
		 (not risch-nogood)
		 (not (atom yy))
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
   expcase
     (cond ((not (equal risch-beta risch-gamma))
	    (go back)))
     (setq risch-y (tryrisch1 (ratqu (polcoef s risch-gamma risch-var) (polcoef r risch-beta risch-var))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (cond ((findint (cdr risch-y))
	    (go back)))
     (setq yy (ratqu (r* -1 (car risch-y)) eta))
     (cond ((and (not (atom yy))
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
   down1
     (setq risch-y (tryrisch1 (ratqu (polcoef s risch-gamma risch-var) (polcoef r risch-beta risch-var))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (setq risch-cary (car risch-y))
     (multiple-value-setq (yy risch-cary risch-nogood risch-lians)
       (getfncoeff (cdr risch-y)
		   (get risch-var 'rischexpr)
		   risch-intvar risch-liflag risch-degree risch-cary risch-nogood
		   risch-lians))
     (cond ((and (not (findint (cdr risch-y)))
		 (not risch-nogood)
		 (not (atom yy))
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (- (car yy)) mu))
	    (setq mu (- (car yy)))))
   back
     (if (minusp mu)
	 (return (if flag
		     (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)
		     nil)))
     (cond ((> risch-beta risch-gamma)
	    (go lsacall))
	   ((= risch-beta risch-gamma)
	    (go recurse)))
     (setq denom (polcoef s risch-gamma risch-var))
     (setq risch-y '(0 . 1))
   linearloop
     (setq ymu (ratqu (polcoef (ratnumerator tt) (+ mu risch-gamma) risch-var)
		      (r* (ratdenominator tt) denom)))
     (setq risch-y (r+ risch-y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu) ))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu risch-mainvar))))
     (decf mu)
     (cond  ((not (< mu 0))
	     (go linearloop))
	    ((not flag)
	     (return (if (rzerop tt)
			 (ratqu risch-y p)
			 nil)))
	    ((rzerop tt)
	     (return (cons (ratqu (r* risch-y (ratexpt (cons (list expg 1 1) 1) n)) p) '(0))))
	    (t
	     (return (cxerfarg (ratqu (r* risch-y (ratexpt (cons (list expg 1 1) 1) n)) p)
			       expg
			       n
			       (ratqu tt lcm)
			       risch-ratform
			       risch-intvar
			       risch-mainvar))))
   recurse
     (setq rbeta (polcoef r risch-beta risch-var))
     (setq risch-y '(0 . 1))
     (setq startmu mu)
     (setq starttt tt)
   recurseloop
     ;; The equation for the coefficient of the MU-th power only involves
     ;; the term of S of the same degree as the leading term of R. That is
     ;; the leading term of S when both degrees agree, and zero when S has
     ;; the lower degree, as it can when LSACALL falls back on this walk.
     (setq f (r+ (ratqu (polcoef s risch-beta risch-var) rbeta)
		 (if expexpflag
		     (r* mu (spderivative eta risch-mainvar))
		     0)))
     ;; F is affine in MU with slope ETA', so for an exponential over an
     ;; exponential it vanishes at no more than one MU. There the coefficient
     ;; equation is the quadrature Y' = A, and EXPPOLYCONTROL answers with one
     ;; of its solutions. The others differ from that one by a constant.
     (when (and expexpflag (null constmu) (rzerop f))
       (setq constmu mu))
     (setq ymu (exppolycontrol nil
			       f
			       (ratqu (polcoef (ratnumerator tt)
					       (+ risch-beta mu)
					       risch-var)
				      (r* (ratdenominator tt) rbeta))
			       expg
			       n
			       risch-ratform
			       risch-intvar
			       risch-liflag
			       risch-degree
			       risch-mainvar))
     (when (null ymu)
       (go recursefail))
     (when (and secondpass (eql mu constmu))
       (setq ymu (r+ ymu (cons 1 1))))
     (setq risch-y (r+ risch-y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu)))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu risch-mainvar))))
     (decf mu)
     (cond
       ((not (< mu 0))
	(go recurseloop))
       ((rzerop tt)
	(go recursedone))
       ((and constmu (null secondpass))
	;; The constant left open at CONSTMU is fixed by one of the coefficient
	;; equations below it, which this walk has already passed. Walk again
	;; with that constant one higher.
	(setq firsty risch-y)
	(setq firsttt tt)
	(setq secondpass t)
	(setq risch-y '(0 . 1))
	(setq mu startmu)
	(setq tt starttt)
	(go recurseloop))
       (secondpass
	;; Both walks keep R*Y' + S*Y + TT equal to STARTTT, so the combination
	;; with a constant CC leaves FIRSTTT - CC*SPREAD = 0 and solves the
	;; equation.
	(let* ((spread (r- firsttt tt))
	       (cc (and (not (rzerop spread))
			(ratqu firsttt spread))))
	  (when (and cc (risch-constp cc risch-mainvar))
	    (setq risch-y (r+ firsty (r* cc (r- risch-y firsty))))
	    (go recursedone)))))
   recursefail
     ;; A second walk that came to nothing leaves the first walk's answer.
     (when secondpass
       (setq risch-y firsty)
       (setq tt firsttt))
     (return (cond ((null flag)
		    nil)
		   ;; LSACALL returns either a complete answer or none.
		   ;; A walk entered from there does the same and leaves the integral
		   ;; unevaluated when it does not finish.
		   (fromlsa
		    (cxerfarg (rzero) expg n a risch-ratform risch-intvar
			      risch-mainvar))
		   (t
		    (cxerfarg (ratqu (r* risch-y (ratexpt (cons (list expg 1 1) 1) n)) p)
			      expg
			      n
			      (ratqu tt lcm)
			      risch-ratform
			      risch-intvar
			      risch-mainvar))))
   recursedone
     (return (if (null flag)
		 (ratqu risch-y p)
		 (cons (ratqu (r* risch-y (ratexpt (cons (list expg 1 1) 1) n)) p)
		       '(0))))
   lsacall
     (setq rrmu mu)
   muloop
     (setq temp (r* (ratexpt (cons (list logeta 1 1) 1) (1- mu))
		    (r+ (r* s (cons (list logeta 1 1) 1))
			(r* mu r logdiff ))))
   mu1
     (setq vector nil)
     (setq rmu (+ rrmu risch-beta))
   rmuloop
     (setq vector (cons (ratqu (polcoef (ratnumerator temp) rmu risch-var)
			       (ratdenominator temp)) vector))
     (decf rmu)
     (unless (< rmu 0)
       (go rmuloop))
     (decf mu)
     (setq aarray (append aarray (list (reverse vector))))
     (cond ((not (< mu 0))
	    (go muloop))
	   ((equal mu -2)
	    (go skipmu)))
     (setq temp tt)
     (go mu1)
   skipmu
     (setq rarray nil)
   arrayloop
     (setq vector nil)
     (setq vector (mapcar 'car aarray))
     (setq aarray (mapcar 'cdr aarray))
     (setq rarray (append rarray (list vector)))
     (unless (null (car aarray)) (go arrayloop))
     (setq rmu (1+ rrmu))
     (setq vector nil)
   array1loop
     (setq vector (cons '(0 . 1) vector))
     (decf rmu)
     (unless (< rmu 0) (go array1loop))
     (setq aarray nil)
   array2loop
     (cond ((equal (car rarray) vector)
	    nil)
	   (t
	    (setq aarray (cons (car rarray) aarray))))
     (setq rarray (cdr rarray))
     (when rarray (go array2loop))
     (setq rarray (reverse aarray))
     (setq temp
       (lsa rarray))
     ;; LSACALL only looks for coefficients that are constants. When LSA finds
     ;; no single such solution, let the RECURSE walk try. It also finds
     ;; coefficients that depend on the variable of integration.
     (when (or (eq temp 'singular)
	       (eq temp 'inconsistent))
       (setq mu rrmu)
       (setq fromlsa t)
       (go recurse))
     (setq temp (reverse  (cdr temp)))
     (setq rmu 0)
     (setq risch-y 0)
   l3
     (setq risch-y (r+ risch-y (r* (car temp) (pexpt (list logeta 1 1) rmu))))
     (setq temp (cdr temp))
     (incf rmu)
     (unless (> rmu rrmu)
       (go l3))
     ;; LSA solves these equations over a field that contains the variable of
     ;; integration, but they hold only for coefficients that are constants.
     ;; A solution that depends on that variable does not solve the
     ;; differential equation, so walk for coefficients that are functions.
     (unless (rzerop (r- tt
			 (r* s risch-y)
			 (r* r (spderivative risch-y risch-mainvar))))
       (setq mu rrmu)
       (setq fromlsa t)
       (go recurse))
     (return (if (null flag)
		 (ratqu risch-y p)
		 (cons (r* (ratexpt (cons (list expg 1 1) 1) n) (ratqu risch-y p)) '(0))))))


(defun erfarg (exparg coef risch-ratform risch-mainvar)
  (prog (num denom erfarg)
     (setq exparg (r- exparg))
     (unless (and (setq num (pnthrootp (ratnumerator exparg) 2))
		  (setq denom (pnthrootp (ratdenominator exparg) 2)))
       (return nil))
     (setq erfarg (cons num denom))
     (if (risch-constp
	  (setq coef (ratqu coef (spderivative erfarg risch-mainvar)))
	  risch-mainvar)
	 (return (simplify `((mtimes) ((rat) 1 2)
			     ((mexpt) $%pi ((rat) 1 2))
			     ,(disrep coef risch-ratform)
			     ((%erf) ,(disrep erfarg risch-ratform))))))))

(defun erfarg2 (exparg coeff risch-ratform risch-intvar risch-mainvar)
  (let ((risch-var risch-mainvar)
	a b c d)
    (when (and (= (pdegree (car exparg) risch-var) 2)
	       (eq (caar exparg) risch-var)
	       (risch-pconstp (cdr exparg) risch-mainvar)
	       (risch-constp coeff risch-mainvar))
      (setq a (ratqu (r* -1 (caddar exparg))
		     (cdr exparg)))
      (setq b (disrep (ratqu (r* -1 (polcoef (car exparg) 1 risch-var))
			     (cdr exparg))
		      risch-ratform))
      (setq c (disrep (ratqu (r* (polcoef (car exparg) 0 risch-var))
			     (cdr exparg))
		      risch-ratform))
      (setq d (ratsqrt a risch-ratform))
      (setq a (disrep a risch-ratform))
      (simplify `((mtimes)
		  ((mtimes)
		   ((mexpt) $%e ((mplus) ,c
				 ((mquotient) ((mexpt) ,b 2)
				  ((mtimes) 4 ,a))))
		   ((rat) 1 2)
		   ,(disrep coeff risch-ratform)
		   ((mexpt) ,d -1)
		   ((mexpt) $%pi ((rat) 1 2)))
		  ((%erf) ((mplus)
			   ((mtimes) ,d ,risch-intvar)
			   ((mtimes) ,b ((rat) 1 2) ((mexpt) ,d -1)))))))))


(defun cxerfarg (ans expg n numdenom risch-ratform risch-intvar risch-mainvar
		 &aux (arg (r* n (get expg 'rischarg)))
		   (fails 0))
  (prog (denom erfans num nerf)
     (desetq (num . denom) numdenom)
     (unless $erfflag
       (setq fails num)
       (go lose))
     (if (setq erfans (erfarg arg numdenom risch-ratform risch-mainvar))
	 (return (list ans erfans)))
   again
     (when (and (not (pcoefp denom))
		(null (p-red denom))
		(eq (get (car denom) 'leadop) 'mexpt))
       (setq arg (r+ arg (r* (- (p-le denom))
			     (get (p-var denom) 'rischarg)))
	     denom (p-lc denom))
       (go again))
     (loop for (coef exparg exppoly) in (explist num arg 1)
	   do (setq coef (ratqu coef denom)
		    nerf (or (erfarg2 exparg coef risch-ratform risch-intvar risch-mainvar)
			     (erfarg exparg coef risch-ratform risch-mainvar)))
	      (if nerf
		  (push nerf erfans)
		  (setq fails (pplus fails exppoly))))
   lose
     (return
       (if (pzerop fails)
	   (cons ans erfans)
	   (rischadd (cons ans erfans)
		     (rischnoun (r* (ratexpt (cons (make-poly expg) 1) n)
				    (ratqu fails (cdr numdenom)))
				risch-ratform
				risch-intvar))))))

(defun explist (p oarg exps)
  (cond ((or (pcoefp p)
	     (not (eq 'mexpt
		      (get (p-var p) 'leadop))))
	 (list (list p oarg (ptimes p exps))))
	(t
	 (loop with narg = (get (p-var p) 'rischarg)
	       for (exp coef) on (p-terms p) by #'cddr
	       nconc (explist coef
			      (r+ oarg (r* exp narg))
			      (ptimes exps
				      (make-poly (p-var p) exp 1)))))))


(defun intsetup (exp risch-*var)
  (prog (varlist clist $factorflag dlist genpairs old risch-y z $ratfac $keepfloat
	 *fnewvarsw)
   y
     (setq exp (radcan1 exp risch-*var))
     (fnewvar exp)
     (setq *fnewvarsw t)
   a
     (setq clist nil)
     (setq dlist nil)
     (setq z varlist)
   up
     (setq risch-y (pop z))
     (cond ((freeof risch-*var risch-y)
	    (push risch-y clist))
	   ((eq risch-y risch-*var)
	    nil)
	   ((and (mexptp risch-y)
		 (not (eq (cadr risch-y) '$%e)))
	    (cond ((not (freeof risch-*var (caddr risch-y)))
		   (setq dlist `((mexpt simp)
				 $%e
				 ,(mul2* (caddr risch-y)
					 `((%log) ,(cadr risch-y)))))
		   (setq exp (maxima-substitute dlist risch-y exp))
		   (setq varlist nil)
		   (go y))
		  ((atom (caddr risch-y))
		   (cond ((numberp (caddr risch-y))
			  (push risch-y dlist))
			 (t
			  ;;(setq operator t)
			  (return (values nil t)))))
		  (t
		   (push risch-y dlist))))
	   (t
	    (push risch-y dlist)))
     (if z
	 (go up))
     (if (member '$%i clist :test #'eq)
	 (setq clist (cons '$%i (delete '$%i clist :test #'equal))))
     (setq varlist (append clist
			   (cons risch-*var
				 (nreverse (sort (append dlist nil)
						 #'(lambda (a b)
						     (intgreat a b risch-*var)))))))
     (orderpointer varlist)
     (setq old varlist)
     (mapc #'(lambda (b)
	       (intset1 b risch-*var))
	   (cons risch-*var dlist))
     (cond ((alike old varlist)
	    (return (values (ratrep* exp)
			    nil
			    exp)))
	   (t (go a)))))

(defun leadop (exp)
  (cond ((atom exp)
	 exp)
	((mqapplyp exp)
	 (cadr exp))
	(t
	 (caar exp))))

(defun leadarg (exp)
  (cond ((atom exp)
	 0)
	((and (mexptp exp) (eq (cadr exp) '$%e))
	 (caddr exp))
	((mqapplyp exp)
	 (car (subfunargs exp)))
	(t
	 (cadr exp))))

(defun intset1 (b risch-*var)
  (let (e c d)
    (fnewvar
     (setq d (if (mexptp b)		;needed for radicals
		 `((mtimes simp)
		   ,b
		   ,(radcan1 (sdiff (simplify (caddr b)) risch-*var)
			     risch-*var))
		 (radcan1 (sdiff (simplify b) risch-*var)
			  risch-*var))))
    (setq d (ratrep* d))
    (setq c (ratrep* (leadarg b)))
    (setq e (cdr (assoc b (pair varlist genvar) :test #'equal)))
    (putprop e (leadop b) 'leadop)
    (putprop e b 'rischexpr)
    (putprop e (cdr d) 'rischdiff)
    (putprop e (cdr c) 'rischarg)))

;; order of expressions for risch.
;; expressions containing erf and li last.
;; then order by size of expression to guarantee that
;; any subexpressions are considered smaller.
;; this relation should be transitive, since it is called by sort. 
(defun intgreat (a b risch-*var)
  (cond ((and (not (atom a))
	      (not (atom b)))
	 (cond ((and (not (freeof '%erf a))
		     (freeof '%erf b))
		t)
	       ((and (not (freeof '$li a))
		     (freeof '$li b))
		t)
	       ((and (freeof '$li a)
		     (not (freeof '$li b)))
		nil)
	       ((and (freeof '%erf a)
		     (not (freeof '%erf b)))
		nil)
	       ((> (conssize a) (conssize b))
		t)
	       ((< (conssize a) (conssize b))
		nil)
	       (t
		(great (resimplify (fixintgreat a risch-*var))
		       (resimplify (fixintgreat b risch-*var))))))
	(t
	 (great (resimplify (fixintgreat a risch-*var))
		(resimplify (fixintgreat b risch-*var))))))

(defun fixintgreat (a risch-*var)
  (subst '/_101x risch-*var a))
