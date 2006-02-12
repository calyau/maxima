;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;


;;    ** (c) Copyright 1976, 1983 Massachusetts Institute of Technology **
(in-package :maxima)

;;These are the main routines for finding the Laplace Transform
;; of special functions   --- written by Yannis Avgoustis
;;                        --- modified by Edward Lafferty
;;                       Latest mod by jpg 8/21/81
;;
;;   This program uses the programs on ELL;HYP FASL.

(macsyma-module hypgeo)

(declare-top (special var *par* zerosigntest productcase checkcoefsignlist
		      $exponentialize $radexpand))

(load-macsyma-macros rzmac)

;; Return the maxima presentation of a bessel function with order v
;; and arg z.  If flg is 'J, the ti's the J function; otherwise, the I
;; function.
(defun bess (v z flg)
  `((,(if (eq flg 'j)
	  '%bessel_j
	  '%bessel_i))
    ,v ,z))


;;(DEFUN CDRAS(A L)(CDR (ZL-ASSOC A L)))

;; Gamma function
(defun gm (expr)
  (simplifya (list '(%gamma) expr) nil))

;; sin(x)
(defun sin% (arg)
  (list '(%sin) arg))

;; Test if X is a number, either Lisp number or a maxima rational.
(defun nump (x)
  (cond ((atom x)
	 (numberp x))
	((not (atom x))
	 (eq (caar (simplifya x nil)) 'rat))))

;; cos(x)
(defun cos% (arg)
  (list '(%cos) arg))

;; Return T if a is a non-positive integer.
;; (Do we really want maxima-integerp or hyp-integerp here?)
(defun neginp (a)
  (cond ((maxima-integerp a)
	 (or (zerp a) (minusp a)))))

(defun notnump (x)
  (not (nump x)))

#+nil
(defun negnump (x)
  (cond ((not (maxima-integerp x))
	 (minusp (cadr (simplifya x nil))))
	(t (minusp x))))


;; Test if EXP is 1 or %e.
(defun expor1p (exp)
  (or (equal exp 1)
      (eq exp '$%e)))

(defun parp (a)
  (eq a *par*))



;;(DEFUN HASVAR(EXP)(COND ((FREEVAR EXP) NIL)(T T)))



(defun arbpow1
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (c nonzerp)
	 ((mexpt)(u hasvar)(v freevar)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize u*asin(x)
(defun u*asinx
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt) (u nonzerp)((%asin)(x hasvar)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize u*atan(x)
(defun u*atanx
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%atan)(x hasvar)))
	((coeffpp)(a zerp)))
      nil))



;; I (rtoy) think this is the tail of the incomplete gamma function.
(defun gminc (a b)
  (list '($gammaincomplete) a b))

;; Lommel's little s[u,v](z) function.
(defun littleslommel
    (m n z)
  (list '(mqapply)(list '($%s array) m n) z))

;; Whittaker's M function
(defun mwhit (a i1 i2)
  (list '(mqapply) (list '($%m array) i1 i2) a))

;; Whittaker's W function
(defun wwhit (a i1 i2)
  (list '(mqapply) (list '($%w array) i1 i2) a))

;; Jacobi P
(defun pjac (x n a b)
  (list '(mqapply) (list '($%p array) n a b) x))

;; Parabolic cylinder function D
(defun parcyl (x n)
  (list '(mqapply) (list '($%d array) n) x))


;;...HOPEFULLY AMONG WHATEVER GARBAGE IT RECOGNIZES J[V](W).

(defun onej
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_j) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))


;; Recognize bessel_j(v1,w1)*bessel_j(v2,w2)
(defun twoj (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_j) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*bessel_y(v2,w2)
(defun twoy (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_k(v2,w2)
(defun twok (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_k) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_y(v2,w2)
(defun onekoney (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_j(v,w)^2
(defun onej^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_j) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v,w)^2
(defun oney^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_y) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v,w)^2
(defun onek^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_k) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v,w)
(defun onei (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_i(v2,w2)
(defun twoi (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_i) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize %h[v1,v11](w1)*%h[v2,v21](w2), the product of 2 Hankel
;; functions.
(defun twoh
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%h array)(v1 true)(v11 true))
	  (w1 true))
	 ((mqapply)
	  (($%h array)(v2 true)(v21 true))
	  (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*bessel_j(v2,w2)
(defun oneyonej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_j(v2,w2)
(defun onekonej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*%h[v2,v21](w2)
(defun oneyoneh (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((mqapply)
	  (($%h array)(v2 true)(v21 true))
	  (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*%h[v2,v21](w2)
(defun onekoneh
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((mqapply)
	  (($%h array)(v2 true)(v21 true))
	  (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_j(v2,w2)
(defun oneionej
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*%h[v2,v21](w2)
(defun oneioneh
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((mqapply)
	  (($%h array)(v2 true)(v21 true))
	  (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize %h[v1,v11](w1)*bessel_j(v2,w2)
(defun onehonej
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%h array)(v1 true)(v11 true))
	  (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_y(v2,w2)
(defun oneioney
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_k(v2,w2)
(defun oneionek
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_k) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v,w)^2
(defun onei^2
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_i) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize %h[v1,v2](w)^2
(defun oneh^2
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((mqapply)
	   (($%h array)(v1 true)(v2 true))
	   (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize erf(w)
(defun onerf
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%erf)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize log(w)
(defun onelog
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%log)(w hasvar)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize erfc(w)
(defun onerfc
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($erfc)(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun oneei
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($%ei)(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun onekelliptic
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($kelliptic)(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun onee
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($%e)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize gammaincomplete(w1, w2), the tail of the incomplete gamma
;; function.
(defun onegammaincomplete
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($gammaincomplete)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize gammagreek(w1,w2), the incomplete gamma function,
;; integrate(t^(v-1)*exp(-t),t,0,x)
(defun onegammagreek
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($gammagreek)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize hstruve[v](w), Struve's H function.
(defun onehstruve
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($hstruve array)(v true))(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize lstruve[v](w), Struve's L function.
(defun onelstruve
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($lstruve array)(v true))(w true)))
	((coeffpp)(a zerp)))
      nil))


;; Recognize Lommel s[v1,v2](w) function.
(defun ones
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%s array)(v1 true)(v2 true))(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Lommel S[v1,v2](w) function
(defun oneslommel
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($slommel array)(v1 true)(v2 true))
	  (w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize u*bessel_y(v,w)
(defun oney (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize u*bessel_k(v,w)
(defun onek
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %d[v](w), parabolic cylinder function
(defun oned
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%d array) (v true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onekbateman
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($kbateman array) (v true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %h[v1,v2](w)
(defun oneh
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%h array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %m[v1,v2](w), Whittaker M function
(defun onem
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%m array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onel
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%l array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onec
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%c array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onet
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%t array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun oneu
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%u array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onepjac
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%p array) (v1 true)(v2 true)(v3 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onehe
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%he array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun oneq
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%q array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onep0
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%p array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun hyp-onep
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%p array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %w[v1,v2](w), Whittaker W function.
(defun onew
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%w array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

 


 



;;...RECOGNIZES L.T.E. "U*%E^(A*X+E*F(X)-P*X+C)+D".

(defun ltep
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  $%e
	  ((mplus)
	   ((coeffpt) (a freevarpar) (x varp))
	   ((coeffpt) (e freevarpar) (f hasvar))
	   ((mtimes) -1. (p parp) (x varp))
	   ((coeffpp) (c freevarpar)))))
	((coeffpp) (d zerp)))
      nil)) 

;;(DEFUN ZERP(A)(EQUAL A 0))

;;(DEFUN NONZERP(A)(NOT (ZERP A)))

(defmfun $specint (exp var)
  (prog ($radexpand checkcoefsignlist)
     (progn (find-function 'sinint))
     (setq $radexpand '$all)
     (return (grasp-some-trigs exp))))

(defun grasp-some-trigs (exp)
  (let ((*asinx* nil)
	(*atanx* nil))
    (declare (special *asinx* *atanx*))
    (prog (u x l)
       (cond ((setq l (u*asinx exp))
	      (setq u (cdras 'u l)
		    x (cdras 'x l)
		    *asinx* 't)
	      (return (defintegrate u))))
       (cond ((setq l (u*atanx exp))
	      (setq u (cdras 'u l)
		    x (cdras 'x l)
		    *atanx* 't)
	      (return (defintegrate u))))
       (return (defintegrate exp)))))



#+nil
(defun defintegrate
    (exp)
  (prog ($exponentialize)
     (setq $exponentialize t)
     (return (distrdefexecinit ($expand (ssimplifya exp))))))

(defun defintegrate (exp)
  ;; This used to have $exponentialize enabled for everything, but I
  ;; don't think we should do that.  If various routines want
  ;; $exponentialize, let them set it themselves.  So, for here, we
  ;; want to expand the form with $exponentialize to convert trig and
  ;; hyperbolic functions to exponential functions that we can handle.
  (let ((form (let (($exponentialize t))
		($expand (ssimplifya exp)))))
    (distrdefexecinit form)))


;; Compute transform of EXP wrt the variable of integration VAR.
#+nil
(defun defexec (exp var)
  (prog(l a)
     (setq exp (simplifya exp nil))
     (cond ((setq l (defltep exp))
	    (setq a (cdras 'a l))
	    (return (negtest l a))))
     (return 'other-defint-to-follow-defexec)))

(defun defexec (exp var)
  (let* ((exp (simplifya exp nil))
	 (l (defltep exp)))
    (cond (l
	   ;; EXP is an expression of the form u*%e^(a*x=e*f+c).  So a
	   ;; is basically the parameter of the Laplace transform.
	   (let ((a (cdras 'a l)))
	     (negtest l a)))
	  (t
	   'other-defint-to-follow-defexec))))

;; L is the integrand of the transform, after pattern matching.  A is
;; the parameter (p) of the transform.
(defun negtest (l a)
  (prog (u e f c)
     (cond ((eq (checksigntm ($realpart a)) '$negative)
	    ;; The parameter of transform must have a negative
	    ;; realpart.  Break out the integrand into its various
	    ;; components.
	    (setq u (cdras 'u l)
		  e (cdras 'e l)
		  f (cdras 'f l)
		  c (cdras 'c l))
	    (cond ((zerp e) (setq f 1)))
	    ;; To compute the transform, we replace A with PSEY for
	    ;; simplicity.  After the transform is computed, replace
	    ;; PSEY with A.
	    ;;
	    ;; FIXME: Sometimes maxima will ask for the sign of PSEY.
	    ;; But that doesn't occur in the original expression, so
	    ;; it's very confusing.  What should we do?
	    (return (maxima-substitute (mul -1 a)
				       'psey
				       (ltscale u
						var
						'psey
						c
						0
						e
						f)))))
     (return 'other-defint-to-follow-negtest)))

;; Compute the transform of
;;
;;  EXP * %E^(-VAR * (*PAR* - PAR0) + E*F + C)
(defun ltscale (exp var *par* c par0 e f)
  (mul* (power '$%e c)
	(substl (sub *par* par0) *par* (lt-exec exp e f))))

;; I think this is trying to match EXP to u*%e^(a*x+e*f+c)+d
;; where a, c, and e are free of x, f is free of p, and d is 0.
(defun defltep (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  $%e
	  ((mplus)
	   ((coeffpt) (a freevar) (x varp))
	   ((coeffpt) (e freevar) (f hasvarnovarp))
	   ((coeffpp) (c freevar)))))
	((coeffpp) (d zerp)))
      nil))

(defun hasvarnovarp (a) (and (hasvar a) (not (varp a))))
;;it dispatches according to the kind of transform it matches


(defun hypgeo-exec (exp var *par*)
  (prog (l u a c e f)
     (setq exp (simplifya exp nil))
     (cond ((setq l (ltep exp))
	    (setq u (cdras 'u l)
		  a (cdras 'a l)
		  c (cdras 'c l)
		  e (cdras 'e l)
		  f (cdras 'f l))
	    (return (ltscale u var *par* c a e f))))
     (return 'other-trans-to-follow)))

(defun substl
    (p1 p2 p3)
  (cond ((eq p1 p2) p3)(t (maxima-substitute p1 p2 p3)))) 

;; Compute the transform of u*%e^(-p*t+e*f)
#+nil
(defun lt-exec (u e f)
  (declare (special *asinx* *atanx*))
  (prog (l)
     (cond ((or *asinx* *atanx*)
	    (return (lt-asinatan u e))))
     (cond ((zerp e)(return (lt-sf-log u))))
     (cond ((and (not (zerp e))(setq l (c*t^v u)))
	    (return (lt-exp l e f))))
     (return (lt-sf-log (mul* u (power '$%e (mul e f)))))))

(defun lt-exec (u e f)
  (declare (special *asinx* *atanx*))
  (let (l)
    (cond ((or *asinx* *atanx*)
	   ;; We've already determined that we have an asin or atan
	   ;; expression, so use lt-asinatan to find the transform.
	   (lt-asinatan u e))
	  ((zerp e)
	   ;; The simple case of u*%e^(-p*t)
	   (lt-sf-log u))
	  ((and (not (zerp e))
		(setq l (c*t^v u)))
	   ;; We have u*%e^(-p*t+e*f).  Try to see if U is of the form
	   ;; c*t^v.  If so, we can handle it here.
	   (lt-exp l e f))
	  (t
	   ;; The complicated case.  Remove the e*f term and move it
	   ;; to u.
	   (lt-sf-log (mul* u (power '$%e (mul e f))))))))

;; Match c*t^v
(defun c*t^v
    (exp)
  (m2 exp
      '((mtimes)
	((coefftt)(c freevar))
	((mexpt)(t varp)(v freevar)))
      nil))

;; Laplace transform of u*%e^(-p*t + e*f).  But we can't handle the
;; case where e isn't zero.
(defun lt-asinatan (u e)
  (declare (special *asinx* *atanx*))
  (cond ((zerp e)
	 (cond (*asinx* (lt-ltp 'asin u var nil))
	       (*atanx* (lt-ltp 'atan u var nil))
	       (t 'lt-asinatan-failed-1)))
	(t 'lt-asinatan-failed-2)))

;; Laplace transform of c*t^v*exp(-p*t+e*f).  L contains the pattern
;; for c*t^v.
(defun lt-exp (l e f)
  (let ((c (cdras 'c l))
	(v (cdras 'v l)))
    (cond ((t^2 f)
	   (setq e (inv (mul -8 e)) v (add v 1))
	   (f24p146test c v e))
	  ((sqroott f)
	   (setq e (mul* e e (inv 4)) v (add v 1))
	   (f35p147test c v e))
	  ((t^-1 f)
	   (setq e (mul -4 e) v (add v 1))
	   (f29p146test v e))
	  ((e^-t f)
	   (f36p147 e))
	  ((e^t f)
	   (f37p147 e))
	  (t 'other-lt-exponential-to-follow))))

(defun t^2(exp)(m2 exp '((mexpt)(t varp) 2) nil))

(defun sqroott(exp)(m2 exp '((mexpt)(t varp)((rat) 1 2)) nil))

(defun t^-1(exp)(m2 exp '((mexpt)(t varp) -1) nil))

(defun e^-t (exp)
  (m2 exp
      '((mexpt) $%e ((mtimes) -1 (t varp)))
      nil))

(defun e^t (exp)
  (m2 exp
      '((mexpt) $%e (t varp))
      nil))

;; Check if conditions for f24p146 hold
(defun f24p146test (c v a)
  (cond ((and (eq (asksign a) '$positive)
	      (eq (asksign v) '$positive))
	;; Both a and v must be positive
	 (f24p146 c v a))
	(t 'fail-on-f24p146test)))

;; Check if conditions for f35p147 hold
(defun f35p147test (c v a)
  (cond ((eq (asksign v) '$positive)
	 ;; v must be positive
	 (f35p147 c v a))
	(t 'fail-on-f35p147test)))

;; Check if conditions for f29p146test hold
(defun f29p146test (v a)
  (cond ((eq (asksign a) '$positive)
	 (f29p146 v a))
	(t 'fail-on-f29p146test)))

;; Check if conditions for f1p137 hold
(defun f1p137test (pow)
  (cond ((eq (asksign (add pow 1)) '$positive)
	 (f1p137 pow))
	(t 'fail-in-arbpow))) 

;; Table of Integral Transforms
;;
;; p. 137, formula 1:
;;
;; t^u*exp(-p*t)
;;   -> gamma(u+1)*p^(-u-1)
;;
(defun f1p137 (pow)
  (mul* (gm (add pow 1))
	(power *par* (sub (mul -1 pow) 1))))

;; Table of Integral Transforms
;;
;; p. 146, formula 24:
;;
;; t^(v-1)*exp(-t^2/8/a)
;;   -> gamma(v)*2^v*a^(v/2)*exp(a*p^2)*D[-v](2*p*sqrt(a))
;;
;; Re(a) > 0, Re(v) > 0
(defun f24p146 (c v a)
  (mul* c
	(gm v)
	(power 2 v)
	(power a (div v 2))
	(power '$%e (mul* a *par* *par*))
	(dtford (mul* 2 *par* (power a (1//2)))
		(mul -1 v))))

;; Table of Integral Transforms
;;
;; p. 147, formula 35:
;;
;; (2*t)^(v-1)*exp(-2*sqrt(a)*sqrt(t))
;;    -> gamma(2*v)*p^(-v)*exp(a/p/2)*D[-2*v](sqrt(2*a/p))
;;
;; Re(v) > 0, Re(p) > 0
(defun f35p147 (c v a)
  (mul* c
	(gm (add v v))
	(power 2 (sub 1 v))		; Is this supposed to be here?
	(power *par* (mul -1 v))
	(power '$%e (mul* a (1//2) (inv *par*)))
	(dtford (power (mul* 2 a (inv *par*))
		       (1//2))
		(mul -2 v))))

;; Table of Integral Transforms
;;
;; p. 147, formula 36:
;;
;; exp(-a*exp(-t))
;;   -> a^(-p)*gamma(p,a)
(defun f36p147 (a)
  (let ((-a (mul -1 a)))
    (mul* (power -a (mul -1 *par*))
	  `(($gammagreek) ,*par* ,-a))))

;; Table of Integral Transforms
;;
;; p. 147, formula 36:
;;
;; exp(-a*exp(t))
;;   -> a^(-p)*gammaincomplete(-p,a)
(defun f37p147 (a)
  (let ((-a (mul -1 a)))
    (mul* (power -a *par*)
	  `(($gammaincomplete) ,(mul -1 *par*) ,-a))))


;; Table of Integral Transforms
;;
;; p. 146, formula 29:
;;
;; t^(v-1)*exp(-a/t/4)
;;    -> 2*(a/p/4)^(v/2)*bessel_k(v, sqrt(a)*sqrt(p))
;;
;; Re(a) > 0
(defun f29p146 (v a)
  (mul* 2
	(power (mul* a (inv 4) (inv *par*))
	       (div v 2))
	(ktfork a v)))

;; bessel_k(v, sqrt(a)*sqrt(p)) in terms of bessel_k or in terms of
;; hypergeometric functions.
;;
;; Choose bessel_k if the order v is an integer.  (Why?)
(defun ktfork (a v)
  (let ((z (power (mul* a *par*) (1//2))))
    (cond ((maxima-integerp v)
	   (kmodbes z v))
	  (t
	   (simpktf z v)))))

;; Express a parabolic cylinder function as either a parabolic
;; cylinder function or as hypergeometric function.
;;
;; Tables of Integral Transforms, p. 386 gives this definition:
;;
;; D[v](z) = 2^(v/2+1/4)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)
;;
#+nil
(defun dtford (z v)
  (cond (((lambda(inv4)
	    (whittindtest (add (div v 2) inv4) inv4))
	  (inv 4))
	 (parcyl z v))
	(t (simpdtf z v))))

(defmvar $prefer_d nil)

(defun dtford (z v)
  (let ((inv4 (inv 4)))
    (cond ((or $prefer_d (whittindtest (add (div v 2) inv4) inv4))
	   (parcyl z v))
	  (t (simpdtf z v)))))


;; Express parabolic cylinder function as a hypergeometric function.
;;
;; A&S 19.3.1 says
;;
;; U(a,x) = D[-a-1/2](x)
;;
;; and A&S 19.12.3 gives
;;
;; U(a,+/-x) = sqrt(%pi)*2^(-1/4-a/2)*exp(-x^2/4)/gamma(3/4+a/2)*M(a/2+1/4,1/2,x^2/2)
;;              -/+ sqrt(%pi)*2^(1/4-a/2)*x*exp(-x^2/4)/gamma(1/4+a/2)*M(a/2+3/4,3/2,x^2/2)
;;
;; So
;;
;; D[v](z) = U(-v-1/2,z)
;;         = sqrt(%pi)*2^(v/2+1/2)*x*exp(-x^2/4)*M(1/2-v/2,3/2,x^2/2)/gamma(-v/2)
;;             + sqrt(%pi)*2^(v/2)*exp(-x^2/4)/gamma(1/2-v/2)*M(-v/2,1/2,x^2/2)
;;
(defun simpdtf (z v)
  (let ((inv2 (1//2))
	(pow (power '$%e (mul* z z (inv -4)))))
    (add (mul* (power 2 (div (sub v 1) 2))
	       z
	       (gm (inv -2))
	       (inv (gm (mul* v -1 inv2)))
	       pow
	       (hgfsimp-exec (list (sub inv2
					(div v
					     2)))
			     (list (div 3 2))
			     (mul* z z inv2)))
	 (mul* (power 2 (div v 2))
	       (gm inv2)
	       pow
	       (inv (gm (sub inv2 (mul v inv2))))
	       (hgfsimp-exec (list (mul* v
					 -1
					 inv2))
			     (list inv2)
			     (mul* z z inv2))))))

;; Express the Bessel K function in terms of hypergeometric functions.
;;
;; K[v](z) = %pi/2*(bessel_i(-v,z)-bessel(i,z))/sin(v*%pi)
;;
;; and
;;
;; bessel_i(v,z) = (z/2)^v/gamma(v+1)*0F1(;v+1;z^2/4)
(defun simpktf (z v)
  (let ((dz2 (div z 2)))
    (mul* '$%pi
	  (1//2)
	  (inv (sin% (mul v '$%pi)))
	  (sub (mul* (power  dz2 (mul -1 v))
		     (inv (gm (sub 1 v)))
		     (hgfsimp-exec nil
				   (list (sub 1
					      v))
				   (mul* z
					 z
					 (inv 4))))
	       (mul* (power dz2 v)
		     (inv (gm (add v 1)))
		     (hgfsimp-exec nil
				   (list (add v
					      1))
				   (mul* z
					 z
					 (inv 4))))))))

;; Dispatches according to the special functions involved in the
;; Laplace transformable expression

(defun lt-sf-log (u)
  (prog (l index1 index11 index2 index21 arg1 arg2 rest)
     (cond ((setq l (twoj u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (twoh u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v11 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v21 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      index11
			      index2
			      index21
			      '2htjory))))
     (cond ((setq l (twoy u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      '2ytj))))
     (cond ((setq l (twok u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      '2kti))))
     (cond ((setq l (onekoney u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      'ktiytj))))
     (cond ((setq l (oneionej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v21 l)
		  arg1 (mul* (1fact t t)(cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (oneioneh u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v21 l)
		  arg1 (mul* (1fact t t)(cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       index21
			       'besshtjory))))
     (cond ((setq l (oneyonej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       nil
			       'bessytj))))
     (cond ((setq l (onekonej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       nil
			       'besskti))))
     (cond ((setq l (onehonej u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v11 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'besshtjory))))
     (cond ((setq l (oneyoneh u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index11 (cdras 'v21 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'htjoryytj))))
     (cond ((setq l (onekoneh u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index11 (cdras 'v21 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'htjorykti))))
     (cond ((setq l (oneioney u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul* (1fact t t)(cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       nil
			       'bessytj))))
     (cond ((setq l (oneionek u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul* (1fact t t)(cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       nil
			       'besskti))))
     (cond ((setq l (onehstruve u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1hstruve rest arg1 index1))))
     (cond ((setq l (onelstruve u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1lstruve rest arg1 index1))))
     (cond ((setq l (ones u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1s rest arg1 index1 index2))))
     (cond ((setq l (oneslommel u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       index2
			       'slommel))))
     (cond ((setq l (oney u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1yref rest arg1 index1))))
     (cond ((setq l (onek u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       nil
			       'kti))))
     (cond ((setq l (oned u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 index1 nil 'd))))
     (cond ((setq l (onegammaincomplete u))
	    (setq arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       arg2
			       nil
			       'gammaincomplete))))
     (cond ((setq l (onekbateman u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       nil
			       'kbateman))))
     (cond ((setq l (onej u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1j rest arg1 index1))))
     (cond ((setq l (onegammagreek u))
	    (setq arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (lt1gammagreek rest arg1 arg2))))
     (cond ((setq l (oneh u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       index11
			       'htjory))))
     (cond ((setq l (onem u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1m rest arg1 index1 index11))))
     (cond ((setq l (onel u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 index11
				 'l))))
     (cond ((setq l (onec u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 index11
				 'c))))
     (cond ((setq l (onet u))
	    (setq index1 (cdras 'v1 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 't))))
     (cond ((setq l (oneu u))
	    (setq index1 (cdras 'v1 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 'u))))
     (cond ((setq l (onehe u))
	    (setq index1 (cdras 'v1 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 'he))))
     (cond ((setq l (hyp-onep u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     (cond ((setq l (onepjac u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v3 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (pjactest rest
			      arg1
			      index1
			      index2
			      index21))))
     (cond ((setq l (oneq u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1q rest arg1 index1 index11))))
     (cond ((setq l (onep0 u))
	    (setq index1 (cdras 'v1 l)
		  index11 0
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     (cond ((setq l (onew u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (whittest rest arg1 index1 index11))))
     (cond ((setq l (onej^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1j^2 rest arg1 index1)))) (cond ((setq l (oneh^2 u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      index11
			      index1
			      index11
			      '2htjory))))
     (cond ((setq l (oney^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      nil
			      index1
			      nil
			      '2ytj))))
     (cond ((setq l (onek^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      nil
			      index1
			      nil
			      '2kti))))
     (cond ((setq l (twoi u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul* (1fact t t)(cdras 'w1 l))
		  arg2 (mul* (1fact t t) (cdras 'w2 l))
		  rest (mul* (1fact nil index1)
			     (1fact nil index2)
			     (cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (onei u))
	    (setq index1 (cdras 'v l)
		  arg1 (mul* (1fact t t)(cdras 'w l))
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt1j rest arg1 index1))))
     (cond ((setq l (onei^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (mul* (1fact t t)(cdras 'w l))
		  rest (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt1j^2 rest arg1 index1))))
     (cond ((setq l (onerf u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1erf rest arg1))))
     (cond ((setq l (onelog u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1log rest arg1))))
     (cond ((setq l (onerfc u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 nil nil 'erfc))))
     (cond ((setq l (oneei u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 nil nil 'ei))))
     (cond ((setq l (onekelliptic u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1kelliptic rest arg1))))
     (cond ((setq l (onee u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1e rest arg1))))
     (cond ((setq l (arbpow1 u))
	    (setq arg1 (cdras 'u l)
		  arg2 (cdras 'c l)
		  index1 (cdras 'v l))
	    (return (mul arg2 (lt-arbpow arg1 index1)))))
     (return 'other-j-cases-next)))

;; Laplace transform of c*t^v*%e(-p*t)
;;
;; EXP = t, POW = v.
(defun lt-arbpow (exp pow)
  (cond ((or (eq exp var) (zerp pow))
	 (f1p137test pow))
	(t 'lt-arbpow-failed)))

;; Laplace transform of a product of Bessel functions.  A1, A2 are
;; the args of the two functions. I1, I2 are the indices of each
;; function.  I11, I21 are secondary indices of each function, if any.
;; FLG is a symbol indicating how we should handle the special
;; functions (and also indicates what the special functions are.)
;;
;; I11 and I21 are for the Hankel functions.
(defun fractest (r a1 a2 i1 i11 i2 i21 flg)
  (cond ((or (and (equal (caar i1) 'rat)
		  (equal (caar i2) 'rat))
	     (eq flg '2htjory))
	 ;; Why do we only execute this for 2htjory, but the code
	 ;; below checks for 2ytj, ktiytj and 2kti?  Shouldn't we
	 ;; allow that as well?
	 (sendexec r
		   (cond ((eq flg '2ytj)
			  (mul (ytj i1 a1)
			       (ytj i2 a2)))
			 ((eq flg '2htjory)
			  (mul (htjory i1 i11 a1)
			       (htjory i2 i21 a2)))
			 ((eq flg 'ktiytj)
			  (mul (kti i1 a1)
			       (ytj i2 a2)))
			 ((eq flg '2kti)
			  (mul (kti i1 a1)
			       (kti i2 a2))))))
	(t 'product-of-y-with-nofract-indices)))

;; Laplace transform of a product of Bessel functions.  A1, A2 are
;; the args of the two functions. I1, I2 are the indices of each
;; function.  I is a secondary index to one function, if any.
;; FLG is a symbol indicating how we should handle the special
;; functions (and also indicates what the special functions are.)
;;
;; I is for the kind of Hankel function.
(defun fractest1 (r a1 a2 i1 i2 i flg)
  (cond ((or (and (listp i2)
		  (equal (caar i2) 'rat))
	     (eq flg 'besshtjory))
	 ;; Why do we only execute this for besshtjory when the code
	 ;; has transformations for bessytj, htjoryytj, besskti, and
	 ;; htjorykti?
	 (sendexec r
		   (cond ((eq flg 'bessytj)
			  (mul (bess i1 a1 'j)
			       (ytj i2 a2)))
			 ((eq flg 'besshtjory)
			  (mul (bess i1 a1 'j)
			       (htjory i2 i a2)))
			 ((eq flg 'htjoryytj)
			  (mul (htjory i1 i a1)
			       (ytj i2 a2)))
			 ((eq flg 'besskti)
			  (mul (bess i1 a1 'j)
			       (kti i2 a2)))
			 ((eq flg 'htjorykti)
			  (mul (htjory i1 i a1)
			       (kti i2 a2))))))
	(t 'product-of-i-y-of-nofract-index)))

;; Laplace transform of a single special function.  A is the arg of
;; the special function. I1, I11 are the indices of the function.  FLG
;; is a symbol indicating how we should handle the special functions
;; (and also indicates what the special functions are.)
;;
;; I11 is the kind of Hankel function
(defun fractest2 (r a1 i1 i11 flg)
  (cond ((or (and (listp i1)
		  (equal (caar i1) 'rat))
	     (eq flg 'd)
	     (eq flg 'kbateman)
	     (eq flg 'gammaincomplete)
	     (eq flg 'htjory)
	     (eq flg 'erfc)
	     (eq flg 'ei)
	     (eq flg 'slommel)
	     (eq flg 'ytj))
	 (sendexec r
		   (cond ((eq flg 'ytj)
			  (ytj i1 a1))
			 ((eq flg 'htjory)
			  (htjory i1 i11 a1))
			 ((eq flg 'd)
			  (dtw i1 a1))
			 ((eq flg 'kbateman)
			  (kbatemantw i1 a1))
			 ((eq flg 'gammaincomplete)
			  (gammaincompletetw a1 i1))
			 ((eq flg 'kti)
			  (kti i1 a1))
			 ((eq flg 'erfc)
			  (erfctd a1))
			 ((eq flg 'ei)
			  (eitgammaincomplete a1))
			 ((eq flg 'slommel)
			  (slommeltjandy i1 i11 a1)))))
	(t 'y-of-nofract-index)))

;; Laplace transform of a single Bessel Y function.
;;
;; REST is the multiplier, ARG1 is the arg, and INDEX1 is the order of
;; the Bessel Y function.
(defun lt1yref (rest arg1 index1)
  ;; If the index is an integer, use LT1Y.  Otherwise, convert Bessel
  ;; Y to Bessel J and compute the transform of that.  We do this
  ;; because converting Y to J for an integer index doesn't work so
  ;; well without taking limits.
  (cond ((maxima-integerp index1)
	 (lt1y rest arg1  index1))
	(t (fractest2 rest arg1 index1 nil 'ytj))))

(defun pjactest
    (rest arg index1 index2 index3)
  (cond ((maxima-integerp index1)
	 (lt-ltp 'onepjac
		 rest
		 arg
		 (list index1 index2 index3)))
	(t 'ind-should-be-an-integer-in-polys)))

(defun eqrat(a)(cond ((numberp a) nil)(t (equal (caar a) 'rat)))) 

(defun integertest
    (r arg i1 i2 flg)
  (cond ((maxima-integerp i1)(dispatchpoltrans r arg i1 i2 flg))
	(t 'index-should-be-an-integer-in-polys)))

(defun dispatchpoltrans
    (r x i1 i2 flg)
  (sendexec r
	    (cond ((eq flg 'l)(ltw x i1 i2))
		  ((eq flg 'he)(hetd x i1))
		  ((eq flg 'c)(ctpjac x i1 i2))
		  ((eq flg 't)(ttpjac x i1))
		  ((eq flg 'u)(utpjac x i1)))))

(defun sendexec(r a)(distrexecinit ($expand (mul (init r) a)))) 

;; Test for Whittaker W function.  Simplify this if possible, or
;; convert to Whittaker M function.
;;
;; We have r * %w[i1,i2](a)
(defun whittest (r a i1 i2)
  (cond ((f16p217test r a i1 i2))
	(t
	 ;; Convert to M function and try again.
	 (distrexecinit ($expand (mul (init r)
				      (wtm a i1 i2)))))))

;; Formula 16, p. 217
;;
;; t^(v-1)*%w[k,u](a*t)
;;   -> gamma(u+v+1/2)*gamma(v-u+1/2)*a^(u+1/2)/
;;          (gamma(v-k+1)*(p+a/2)^(u+v+1/2)
;;        *2f1(u+v+1/2,u-k+1/2;v-k+1;(p-a/2)/(p+a/2))
;;
;; For Re(v +/- mu) > -1/2
(defun f16p217test (r a i1 i2)
  ;; We have r*%w[i1,i2](a)
  (let ((l (c*t^v r)))
    ;; Make sure r is of the form c*t^v
    (when l
      (let* ((v (add (cdras 'v l) 1))
	     (c (cdras 'c l)))
	;; Check that v + i2 + 1/2 > 0 and v - i2 + 1/2 > 0.
	(when (and (eq (asksign (add (add v i2) 1//2)) '$positive)
		   (eq (asksign (add (sub v i2) 1//2)) '$positive))
	  ;; Ok, we satisfy the conditions.  Now extract the arg.
	  (let ((l (m2 a
		       '((mplus)
			 ((coeffpt) (f hasvar) (a freevar))
			 ((coeffpp) (c zerp)))
		       nil)))
	    (when l
	      (let ((a (cdras 'a l)))
		;; We're ready now to compute the transform.
		(mul* c
		      (power a (add i2 1//2))
		      (gm (add (add v i2) 1//2))
		      (gm (add (sub v i2) 1//2))
		      (inv (mul* (gm (add (sub v i1) 1))
				 (power (add *par* (div a 2))
					(add (add i2 v) 1//2))))
		      (hgfsimp-exec (list (add (add i2 v 1//2))
					  (add (sub i2 i1) 1//2))
				    (list (add (sub v i1) 1))
				    (div (sub *par* (div a 2))
					 (add *par* (div a 2)))))))))))))
  
(defun whittindtest (i1 i2)
  (or (maxima-integerp (add i2 i2))
      (neginp (sub (sub (1//2) i2) i1))
      (neginp (sub (add (1//2) i2) i1))))

;; Compute r*exp(-var**par*).
;;
;; (Probably r*exp(-p*t), where t is the variable of integration and p
;; is the parameter of the Laplace transform.)
(defun init (r)
  (mul* r (power '$%e (mul* -1 var *par*))))

;; (-1)^n*n!*laguerre(n,a,x) = U(-n,a+1,x)
;;
;; W[k,u](z) = exp(-z/2)*z^(u+1/2)*U(1/2+u-k,1+2*u,z)
;;
;; So
;;
;; laguerre(n,a,x) = (-1)^n*U(-n,a+1,x)/n!
;;
;; U(-n,a+1,x) = exp(z/2)*z^(-a/2-1/2)*W[1/2+a/2+n,a/2](z)
;;
;; Finally,
;;
;; laguerre(n,a,x) = (-1)^n/n!*exp(z/2)*z^(-a/2-1/2)*W[1/2+a/2+n,a/2](z)
(defun ltw (x n a)
  ((lambda (diva2)
     (mul* (power -1 n)
	   (inv (factorial n))
	   (power x (sub (inv -2) diva2))
	   (power '$%e (div x 2))
	   (wwhit x (add (1//2) diva2 n) diva2)))
   (div a 2)))

(defun ctpjac
    (x n v)
  ((lambda(inv2)
     (mul* (gm (add v v n))
	   (inv (gm (add v v)))
	   (gm (add inv2 v))
	   (inv (gm (add v inv2 n)))
	   (pjac x n (sub v inv2)(sub v inv2))))
   (1//2)))

(defun ttpjac
    (x n)
  ((lambda(inv2)
     (mul* (factorial n)
	   (gm inv2)
	   (inv (gm (add inv2 n)))
	   (pjac x n (mul -1 inv2)(mul -1 inv2))))
   (1//2)))

(defun utpjac
    (x n)
  ((lambda(inv2)
     (mul* (factorial (add n 1))
	   inv2
	   (gm inv2)
	   (inv (gm (add inv2 n 1)))
	   (pjac x n inv2 inv2)))
   (1//2)))

;; Hermite He function as a parabolic cylinder function
;; 
;; Tables of Integral Transforms
;;
;; p. 386
;;
;; D[n](z) = (-1)^n*exp(z^2/4)*diff(exp(-z^2/2),z,n);
;;
;; p. 369
;;
;; He[n](x) = (-1)^n*exp(x^2/2)*diff(exp(-x^2/2),x,n)
;;
(defun hetd (x n)
  (mul* (power '$%e (mul* x x (inv 4)))
	(parcyl x n)))

;; erfc in terms of D, parabolic cylinder function
;;
;; Tables of Integral Transforms
;;
;; p 387:
;; erfc(x) = (%pi*x)^(-1/2)*exp(-x^2/2)*W[-1/4,1/4](x^2)
;;
;; p 386:
;; D[v](z) = 2^(v/2+1/2)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)
;;
;; So
;;
;; erfc(x) = %pi^(-1/2)*2^(1/4)*exp(-x^2/2)*D[-1](x*sqrt(2))
(defun erfctd
    (x)
  ((lambda(inv2)
     (mul* (power 2 inv2)		; Should this be 2^(1/4)?
	   (power '$%pi (mul* -1 inv2))
	   (power '$%e (mul* -1 inv2 x x))
	   (parcyl (mul* (power 2 inv2) x) -1)))
   (1//2)))

;; The exponential integral Ei can be written in terms of the
;; incomplete gamma function.
;;
;; See Table of Integral Transforms, p. 386:
;;
;; -Ei(-x) = E1(x) = integrate(exp(-t)/t,t,x,inf)
;;
;;         = gammaincomplete(0,x)
;;
(defun eitgammaincomplete (x)
  (mul* -1 (gminc 0 (mul -1 x))))

;; Express Lommel S function in terms of J and Y.
;; Luke gives
;;
;; S[u,v](z) = s[u,v](z) + {2^(u-1)*gamma((u-v+1)/2)*gamma((u+v+1)/2)}
;;                 * {sin[(u-v)*%pi/2]*bessel_j(v,z)
;;                     - cos[(u-v)*%pi/2]*bessel_y(v,z)
(defun slommeltjandy
    (m n z)
  ((lambda(arg)
     (add (littleslommel m n z)
	  (mul* (power 2 (sub m 1))
		(gm (div (sub (add m 1) n) 2))
		(gm (div (add m n 1) 2))
		(sub (mul* (sin% arg)(bess n z 'j))
		     (mul* (cos% arg)(bess n z 'y))))))
   (mul* (1//2) '$%pi (sub m n))))

;; Whittaker W function in terms of Whittaker M function
;;
;; A&S 13.1.34
;;
;; W[k,u](z) = gamma(-2*u)/gamma(1/2-u-k)*M[k,u](z)
;;              + gamma(2*u)/gamma(1/2+u-k)*M[k,-u](z)
(defun wtm (a i1 i2)
  (add (mul* (gm (mul -2 i2))
	     (mwhit a i1 i2)
	     (inv (gm (sub (sub (1//2) i2) i1))))
       (mul* (gm (add i2 i2))
	     (mwhit a i1 (mul -1 i2))
	     (inv (gm (sub (add (1//2) i2) i1))))))

;; Tail of the incomplete gamma function as a Whittaker W function
;;
;; Tables of Integral Transforms, p. 387
;;
;; gammaincomplete(a,x) = x^((a-1)/2)*exp(-x/2)*W[(a-1)/2,a/2](x)
(defun gammaincompletetw (a x)
  (mul* (power x (div (sub a 1) 2))
	(power '$%e (div x -2))
	(wwhit x (div (sub a 1) 2)(div a 2))))

(defun distrexecinit (fun)
  (cond ((and (consp fun)
	      (consp (car fun))
	      (equal (caar fun) 'mplus))
	 (distrexec (cdr fun)))
	(t (hypgeo-exec fun var *par*))))

;; Evaluate the transform of a sum as sum of transforms.
(defun distrdefexecinit (fun)
  (cond ((equal (caar fun) 'mplus)
	 (distrdefexec (cdr fun)))
	(t (defexec fun var))))

(defun distrexec (fun)
  (cond ((null fun) 0)
	(t (add (hypgeo-exec (car fun) var *par*)
		(distrexec (cdr fun))))))

;; FUN is a list of addends.  Compute the transform of each addend and
;; add them up.
(defun distrdefexec (fun)
  (cond ((null fun) 0)
	(t (add (defexec (car fun) var)
		(distrdefexec (cdr fun))))))

;; Express bessel_y in terms of bessel_j.
;;
;; A&S 9.1.2:
;;
;; bessel_y(v,z) = bessel_j(v,z)*cot(v*%pi)-bessel_j(-v,z)/sin(v*%pi)
(defun ytj (i a)
  (sub (mul* (bess i a 'j) (list '(%cot) (mul i '$%pi)))
       (mul* (bess (mul -1 i) a 'j)
	     (inv (sin% (mul i '$%pi))))))

;; Express parabolic cylinder function as a Whittaker W function.
;;
;; See Table of Integral Transforms, p.386:
;;
;; D[v](z) = 2^(v/2+1/4)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)
;;
(defun dtw (i a)
  (mul* (power 2 (add (div i 2) (inv 4)))
	(power a (inv -2))
	(wwhit (mul* a a (1//2))
	       (add (div i 2) (inv 4))
	       (inv 4))))

;; Bateman's function as a Whittaker W function
;;
;; See Table of Integral Transforms, p.386:
;;
;; k[2*v](z) = 1/gamma(v+1)*W[v,1/2](2*z)
;;
(defun kbatemantw (v a)
  (div (wwhit (add a a) (div v 2) (1//2))
       (gm (add (div v 2) 1))))

;; Bessel K in terms of Bessel I.
;;
;; A&S 9.6.2
;;
;; bessel_k(v,z) = %pi/2*(bessel_i(-v,z)-bessel_i(v,z))/sin(v*%pi)
(defun kti (i a)
  (mul* '$%pi
	(1//2)
	(inv (sin% (mul i '$%pi)))
	(sub (bess (mul -1 i) a 'i)
	     (bess i a 'i))))

;; If FLG is non-NIL, return exp(%pi*%i/2).  Otherwise, return
;; exp(-%pi*%i*v/2)
(defun 1fact (flg v)
  (power '$%e
	 (mul* '$%pi
	       '$%i
	       (1//2)
	       (cond (flg 1)
		     (t (mul -1 v))))))

;; Bessel Y
(defun bessy (v z)
  `((%bessel_y) ,v ,z))

;; Bessel K
(defun kmodbes(z v)
  `((%bessel_k) ,v ,z))



(defun tan% (arg)
  (list '(%tan) arg))

;; Bessel J or Y, depending on if FLG is 'J or not.
(defun desjy (v z flg)
  (cond ((eq flg 'j)
	 (bess v z 'j))
	(t
	 (bessy v z))))

(defun numjory (v sort z flg)
  (cond ((equal sort 1)
	 ;; bessel(-v, z) - exp(-v*%pi*%i)*bessel(v, z)
	 ;;
	 ;; Where bessel is bessel_j if FLG is 'j.  Otherwise, bessel
	 ;; is bessel_y.
	 ;;
	 ;; bessel_y(-v, z) - exp(-v*%pi*%i)*bessel_y(v, z)
	 (sub (desjy (mul -1 v) z flg)
	      (mul* (power '$%e (mul* -1 v '$%pi '$%i))
		    (desjy v z flg))))
	(t
	 ;; exp(-v*%pi*%i)*bessel(v,z) - bessel(-v,z), where bessel is
	 ;; bessel_j or bessel_y, depending on if FLG is 'j or not.
	 (sub (mul* (power '$%e (mul* v '$%pi '$%i))
		    (desmjy v z flg))
	      (desmjy (mul -1 v) z flg)))))

(defun desmjy (v z flg)
  (cond ((eq flg 'j)
	 ;; bessel_j(v,z)
	 (bess v z 'j))
	(t
	 ;; -bessel_y(v,z)
	 (mul -1 (bessy v z)))))

;; Express Hankel function in terms of Bessel J or Y function.
;;
;; A&S 9.1.3
;;
;; H[v,1](z) = %i*csc(v*%pi)*(exp(-v*%pi*%i)*bessel_j(v,z) - bessel_j(-v,z))
;;
;; A&S 9.1.4:
;; H[v,2](z) = %i*csc(v*%pi)*(bessel_j(-v,z) - exp(-v*%pi*%i)*bessel_j(v,z))
;;
(defun htjory (v sort z)
  ;; V is the order, SORT is the kind of Hankel function (1 or 2), Z
  ;; is the arg.
  (cond ((and (consp v)
	      (consp (car v))
	      (equal (caar v) 'rat))
	 ;; If the order is a rational number of some sort,
	 ;;
	 ;; (bessel_j(-v,z) - bessel_j(v,z)*exp(-v*%pi*%i))/(%i*sin(v*%pi*%i))
	 (div (numjory v sort z 'j)
	      (mul* '$%i (sin% (mul v '$%pi)))))
	(t
	 ;; Otherwise, express it in terms of bessel_y.  (Why?)
	 ;;
	 ;; (bessel_y(-v,z) - exp(v*%pi*%i)*bessel_y(v,z))/sin(v*%pi)
	 (div (numjory v sort z 'y)
	      (sin% (mul v '$%pi))))))

;;; LT<foo> functions are various experts on Laplace transforms of the
;;; function <foo>.  The expression being transformed is
;;; r*<foo>(args).  The first arg of each expert is r, The remaining
;;; args are the arg(s) and/or parameters.

;; Expert on Laplace transform expressions containing one bessel
;; function of the first kind

(defun lt1j (rest arg index)
  (lt-ltp 'onej rest arg index))

(defun lt1y (rest arg index)
  (lt-ltp 'oney rest arg index))

;; Transform of a product of Bessel J functions.  The argument of each
;; must be the same, but the orders may be different.
(defun lt2j (rest arg1 arg2 index1 index2)
  (cond ((not (equal arg1 arg2))
	 'product-of-bessel-with-different-args)
	(t (lt-ltp 'twoj
		   rest
		   arg1
		   (list 'list index1 index2)))))

;; Transform of a square of a Bessel J function
(defun lt1j^2
    (rest arg index)
  (lt-ltp 'twoj rest arg (list 'list index index)))

;; Transform of incomplete Gamma function
(defun lt1gammagreek
    (rest arg1 arg2)
  (lt-ltp 'gammagreek rest arg2 arg1))

;; Laplace transform of r*%m[i1,i2](a)
(defun lt1m (r a i1 i2)
  (lt-ltp 'onem r a (list i1 i2)))

(defun lt1p(r a i1 i2)(lt-ltp 'hyp-onep r a (list i1 i2)))

(defun lt1q(r a i1 i2)(lt-ltp 'oneq r a (list i1 i2)))

(defun lt1erf(rest arg)(lt-ltp 'onerf rest arg nil))

(defun lt1log(rest arg)(lt-ltp 'onelog rest arg nil))

(defun lt1kelliptic(rest arg)(lt-ltp 'onekelliptic rest arg nil))

(defun lt1e(rest arg)(lt-ltp 'onee rest arg nil))

(defun lt1hstruve(rest arg1 index1)(lt-ltp 'hs rest arg1 index1))

(defun lt1lstruve(rest arg1 index1)(lt-ltp 'hl rest arg1 index1))

(defun lt1s
    (rest arg1 index1 index2)
  (lt-ltp 's rest arg1 (list index1 index2)))

;; Express the Struve H function as a hypergeometric function.
;;
;; A&S 12.1.2 gives the following series for the Struve H function:
;;
;;                       inf
;; H[v](z) = (z/2)^(v+1)*sum (-1)^k*(z/2)^(2*k)/gamma(k+3/2)/gamma(k+v+3/2)
;;                       k=0
;;
;; We can write this in the form
;;
;; H[v](z) = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2)
;;
;;             inf
;;           * sum n!/poch(3/2,n)/poch(v+3/2,n)*(-z^2/4)^n/n!
;;             n=0
;;
;;         = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2) * 1F2(1;3/2,v+3/2;(-z^2/4))
;;
;; See also A&S 12.1.21.
;;
(defun hstf (v z)
  (let ((d32 (div 3 2)))
    (list (mul* (power (div z 2)(add v 1))
		(inv (gm d32))
		(inv (gm (add v d32))))
	  (ref-fpq (list 1)
		   (list d32 (add v d32))
		   (mul* (inv -4) z z)))))

;; Struve L function
;;
;; A&S 12.2.1:
;;
;; L[v](z) = -%i*exp(-v*%i*%pi/2)*H[v](%i*z)
;;
;; This function computes exactly this way.  (But why is %i written as
;; exp(%i*%pi/2) instead of just %i)
;;
;; A&S 12.2.1 gives the series expansion as
;;
;;                       inf
;; L[v](z) = (z/2)^(v+1)*sum (z/2)^(2*k)/gamma(k+3/2)/gamma(k+v+3/2)
;;                       k=0
;;
;; It's quite easy to derive
;;
;; L[v](z) = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2) * 1F2(1;3/2,v+3/2;(z^2/4))

#+nil
(defun lstf (v z)
  (prog (hst)
     (return (list (mul* (power '$%e
				(mul* (div (add v 1)
					   -2)
				      '$%pi
				      '$%i))
			 (car (setq hst
				    (hstf v
					  (mul* z
						(power '$%e
						       (mul*
							(1//2)
							'$%i
							'$%pi)))))))
		   (cadr hst)))))

(defun lstf (v z)
  (let ((d32 (div 3 2)))
    (list (mul* (power (div z 2) (add v 1))
		(inv (gm d32))
		(inv (gm (add v d32))))
	  (ref-fpq (list 1)
		   (list d32 (add v d32))
		   (mul* (inv 4) z z)))))

;; Lommel s function
;;
;; See Y. L. Luke, p 217, formula 1
;;
;; s(u,v,z) = z^(u+1)/(u-v+1)/(u+v+1)*1F2(1; (u-v+3)/2, (u+v+3)/2; -z^2/4)
(defun stf (m n z)
  (list (mul* (power z (add m 1))
	      (inv (sub (add m 1) n))
	      (inv (add m n 1)))
	(ref-fpq (list 1)
		 (list (div (sub (add m 3) n) 2)
		       (div (add* m n 3) 2))
		 (mul* (inv -4) z z))))

;; FLG = special function we're transforming
;; REST = other stuff
;; ARG = arg of special function
;; INDEX = index of special function.
;;
;; So we're transforming REST*FLG(INDEX, ARG).
(defun lt-ltp (flg rest arg index)
  (prog (index1 index2 argl const l l1)
     (when (or (zerp index)
	       (eq flg 'onerf)
	       (eq flg 'onekelliptic)
	       (eq flg 'onee)
	       (eq flg 'onepjac)
	       (eq flg 'd)
	       (eq flg 's)
	       (eq flg 'hs)
	       (eq flg 'ls)
	       (eq flg 'onem)
	       (eq flg 'oneq)
	       (eq flg 'gammagreek)
	       (eq flg 'asin)
	       (eq flg 'atan))
	    (go labl))
     (cond ((or (eq flg 'hyp-onep)
		(eq flg 'onelog))
	    ;; Go to labl1 if we've got %p or log.
	    (go labl1)))
     ;; Skip this if we have exactly one index or if INDEX doesn't
     ;; look like '(LIST i1 i2)
     (cond ((not (consp index))
	    (go lab)))
     (cond ((not (eq (car index) 'list))
	    (go lab)))
     ;; If the first index is exactly 0, set INDEX1 to it and go to
     ;; LA.
     (cond ((zerp (setq index1 (cadr index)))(go la)))
     ;; If we're here, the index is of the form '(LIST i1 i2), which
     ;; means this is the product of two Bessel functions.
     ;;
     ;; Ok.  I think this is wrong, in general.  I think we get here
     ;; if we're doing the produce to two Bessel functions.  This code
     ;; seems to be applying the property that bessel_j(-n,x) =
     ;; (-1)^n*bessel_j(n,x).  But this only works if n is an integer.
     #+nil
     (cond ((eq (checksigntm (simplifya (inv (setq index1
						   (cadr
						    index)))
					nil))
		'$negative)
	    ;; FIXME: What is this supposed to do?  We take the
	    ;; reciprocal of the first index and see if it's negative.
	    ;; If so, we change the sign of index1 and divide REST by
	    ;; the index.  What is this for?
	    (setq index1
		  (mul -1 index1)
		  rest
		  (mul* (power -1 index1) rest))))
     la
     ;; If the second index is zero, skip over this.
     (cond ((zerp (setq index2 (caddr index)))
	    (go la2)))
     ;; Wrong too.  See comment above about bessel_j(-n,x).
     #+nil
     (cond ((eq (checksigntm (simplifya (inv (setq index2
						   (caddr
						    index)))
					nil))
		'$negative)
	    ;; FIXME: This does the same for index2 as for index1
	    ;; above.  Why?
	    (setq index2
		  (mul -1 index2)
		  rest
		  (mul* (power -1 index2) rest))))
     la2
     ;; Put the 2 indices in a list and go on.
     (setq index (list index1 index2))
     (go labl)
     lab
     ;; We're here if we have one index, and it's not one of the
     ;; special cases.
     ;;
     ;; FIXME:  Find out what functions trigger this.

     ;; I think this is the one bessel function case with a negative
     ;; index.  At least here we check that the index is an integer.
     ;; We can either leave it here or take it out.  It seems
     ;; bessel_j(-n,x) is converted by the Bessel simplifiers before
     ;; we get here.
     (cond ((and (eq (checksigntm (simplifya (inv index)
					     nil))
		     '$negative)
		 (maxima-integerp index))
	    ;; FIXME: Same as the 2 index thing above, but we need an
	    ;; (numeric) integer too.  Why?
	    (setq index (mul -1 index))
	    (setq rest (mul (power -1 index) rest))))
     labl
     ;; Handle index = 0 or one of the special functions erf,
     ;; kelliptic, E, Jacobi, %d, %s, hstruve, lstruve, %m, Q,
     ;; incomplete gamma, asin, atan.
     (setq argl (f+c arg))
     (setq const (cdras 'c argl)
	   arg (cdras 'f argl))
     ;; See if the arg is f + c, and replace arg with f.
     (cond ((null const)(go labl1)))
     ;; This handles the case of when the const term is actually there.
     (cond ((not (eq (checksigntm (simplifya (power const
						    2)
					     nil))
		     '$zero))
	    ;; I guess prop4 handles the case when square of the
	    ;; constant term is not zero.  Too bad I (rtoy) don't know
	    ;; what prop4 is.
	    ;;
	    ;; FIXME:  Implement prop4.
	    (format t "const = ~A~%" const)
	    (format t "f = ~A~%" arg)
	    (return 'prop4-to-be-applied)))
     labl1
     ;; No const term, if we're here.
     (cond ((eq flg 'oney)
	    ;; Handle bessel_y here.  We're done.
	    (return (lty rest arg index))))
     ;; Try to express the function in terms of hypergeometric
     ;; functions that we can handle.
     (cond ((setq l
		  (d*x^m*%e^a*x ($factor (mul* rest
					       (car (setq
						     l1
						     (ref
						      flg
						      index
						      arg)))))))
	    ;; Convert the special function to a hypgergeometric
	    ;; function.  L1 is the special function converted to the
	    ;; hypergeometric function.  d*x^m*%e^a*x looks for that
	    ;; factor in the expanded form.
	    (return (%$etest l l1))))
     ;; We currently don't know how to handle this yet.
     (return 'other-ca-later)))

(defun lty
    (rest arg index)
  (prog(l)
     (cond ((setq l (d*x^m*%e^a*x rest))
	    (return (execfy l arg index))))
     (return 'fail-in-lty)))

(defun %$etest
    (l l1)
  (prog(a q)
     (setq q (cdras 'q l))
     (cond ((equal q 1)(setq a 0)(go loop)))
     (setq a (cdras 'a l))
     loop
     (return (substl (sub *par* a)
		     *par*
		     (execf19 l (cadr l1))))))

;; Dispatch function to convert the given function to a hypergeometric
;; function.
;;
;; The first arg is a symbol naming the function; the last is the
;; argument to the function.  The second arg is the index (or list of
;; indices) to the function.  Not used if the function doesn't have
;; any indices
;;
;; The result is a list of 2 elements: The first element is a
;; multiplier; the second, the hypergeometric function itself.
#+nil
(defun ref (flg index arg)
  (cond ((eq flg 'onej)(j1tf index arg))
	((eq flg 'twoj)(j2tf (car index)(cadr index) arg))
	((eq flg 'hs)(hstf index arg))
	((eq flg 'hl)(lstf index arg))
	((eq flg 's)(stf (car index)(cadr index) arg))
	((eq flg 'onerf)(erftf arg))
	((eq flg 'onelog)(logtf arg))
	((eq flg 'onekelliptic)(kelliptictf arg))
	((eq flg 'onee)(etf arg))
	((eq flg 'onem)(mtf (car index)(cadr index) arg))
	((eq flg 'hyp-onep)(ptf (car index)(cadr index) arg))
	((eq flg 'oneq)(qtf (car index)(cadr index) arg))
	((eq flg 'gammagreek)(gammagreektf index arg))
	((eq flg 'onepjac)
	 (pjactf (car index)(cadr index)(caddr index) arg))
	((eq flg 'asin)(asintf arg))
	((eq flg 'atan)(atantf arg))))

(defun ref (flg index arg)
  (case flg
    (onej (j1tf index arg))
    (twoj (j2tf (car index) (cadr index) arg))
    (hs (hstf index arg))
    (hl (lstf index arg))
    (s (stf (car index) (cadr index) arg))
    (onerf (erftf arg))
    (onelog (logtf arg))
    (onekelliptic (kelliptictf arg))
    (onee (etf arg))
    (onem (mtf (car index) (cadr index) arg))
    (hyp-onep (ptf (car index) (cadr index) arg))
    (oneq (qtf (car index) (cadr index) arg))
    (gammagreek (gammagreektf index arg))
    (onepjac 
     (pjactf (car index) (cadr index) (caddr index) arg))
    (asin (asintf arg))
    (atan (atantf arg))))

;; Create a hypergeometric form that we recognize.  The function is
;; %f[n,m](p; q; arg).  We represent this as a list of the form
;; (fpq (<length p> <length q>) <p> <q> <arg>)
(defun ref-fpq (p q arg)
  (list 'fpq (list (length p) (length q))
	p q arg))

;; Whittaker M function.
;;
;; A&S 13.1.32:
;;
;; M[k,u](z) = exp(-z/2)*z^(1/2+u)*M(1/2+u-k,1+2*u,z)
;;
(defun mtf (i1 i2 arg)
  (list (mul (power arg (add i2 (1//2)))
	     (power '$%e (div arg -2)))
	(ref-fpq (list (add* (1//2) i2 (mul -1 i1)))
		 (list (add* i2 i2 1))
		 arg)))

;; Jacobi P in terms of hypergeometric function
;;
;; A&S 15.4.6:
;;
;; F(-n,a+1+b+n; a+1; x) = n!/poch(a+1,n)*jacobi_p(n,a,b,1-2*x)
;;
;; jacobi_p(n,a,b,x) = poch(a+1,n)/n!*F(-n,a+1+b+n; a+1; (1-x)/2)
;;                   = gamma(a+n+1)/gamma(a+1)/n!*F(-n,a+1+b+n; a+1; (1-x)/2)
(defun pjactf (n a b x)
  (list (mul* (gm (add n a 1))
	      (inv (gm (add a 1)))
	      (inv (factorial n)))
	(ref-fpq (list (mul -1 n) (add* n a b 1))
		 (list (add a 1))
		 (sub (1//2) (div x 2)))))

;; asin(x)
;;
;; A&S 15.1.6:
;;
;; F(1/2,1/2; 3/2; z^2) = asin(z)/z
;;
;; asin(z) = z*F(1/2,1/2; 3/2; z^2)
(defun asintf (arg)
  ((lambda(inv2)
     (list arg
	   (ref-fpq (list inv2 inv2)
		    (list (div 3 2))
		    (mul arg arg))))
   (1//2)))

;; atan(x)
;;
;; A&S 15.1.5
;;
;; F(1/2,1; 3/2; -z^2) = atan(z)/z
;;
;; atan(z) = z*F(1/2,1; 3/2; -z^2)
(defun atantf (arg)
  (list arg
	(ref-fpq (list (inv 2) 1)
		 (list (div 3 2))
		 (mul* -1 arg arg))))

;; Associated Legendre function P
;;
;; A&S 8.1.2
;;
;; assoc_legendre_p(v,u,z) = ((z+1)/(z-2))^(u/2)/gamma(1-u)*F(-v,v+1;1-u,(1-z)/2)
;;
;; FIXME: What about the branch cut?  8.1.2 is for z not on the real
;; line with -1 < z < 1.
(defun ptf (n m z)
  (list (mul (inv (gm (sub 1 m)))
	     (power (div (add z 1)
			 (sub z 1))
		    (div m 2)))
	(ref-fpq (list (mul -1 n) (add n 1))
		 (list (sub 1 m))
		 (sub (1//2) (div z 2)))))

;; Associated Legendre function Q
;;
;; A&S 8.1.3:
;;
;; assoc_legendre_q(v,u,z)
;;    = exp(%i*u*%pi)*2^(-v-1)*sqrt(%pi) *
;;       gamma(v+u+1)/gamma(v+3/2)*z^(-v-u-1)*(z^2-1)^(u/2) * 
;;        F(1+v/2+u/2, 1/2+v/2+u/2; v+3/2; 1/z^2)
;;
;; FIXME:  What about the branch cut?
(defun qtf (n m z)
  (list (mul* (power '$%e (mul* m '$%pi '$%i))
	      (power '$%pi (1//2))
	      (gm (add* m n 1))
	      (power 2 (sub -1 n))
	      (inv (gm (add n (div 3 2))))
	      (power z (mul -1 (add* m n 1)))
	      (power (sub (mul z z) 1)
		     (div m 2)))
	(ref-fpq (list (div (add* m n 1) 2)
		       (div (add* m n 2) 2))
		 (list (add n (div 3 2)))
		 (power z -2))))

;; Incomplete gamma function, integrate(t^(v-1)*exp(-t),t,0,x)
;;
;; A&S 13.6.10:
;;
;; M(a,a+1,-x) = a*x^(-a)*gammagreek(a,x)
;;
;; gammagreek(a,x) = x^a/a*M(a,a+1,-x)
(defun gammagreektf (a x)
  (list (mul (inv a) (power x a))
	(ref-fpq (list a)
		 (list (add a 1))
		 (mul -1 x))))

;; Complete elliptic K
;;
;; A&S 17.3.9
;;
;; K(k) = %pi/2*F(1/2,1/2; 1; k^2)
;;
(defun kelliptictf (k)
  (let ((inv2 (1//2)))
    (list (mul inv2 '$%pi)
	  (ref-fpq (list inv2 inv2)
		   (list 1)
		   (mul k k)))))

;; Complete elliptic E
;;
;; A&S 17.3.10
;;
;; E(k) = %pi/2*F(-1/2,1/2;1;k^2)
(defun etf (k)
  (let ((inv2 (1//2)))
    (list (mul inv2 '$%pi)
	  (list 'fpq
		(list  2 1)
		(list (mul -1 inv2) inv2)
		(list 1)
		(mul k k)))))

;; erf expressed as a hypgeometric function.
;;
;; A&S 7.1.21 gives
;;
;; erf(z) = 2*z/sqrt(%pi)*M(1/2,3/2,-z^2) = 2*z/sqrt(%pi)*exp(-z^2)*M(1,3/2,z^2)
(defun erftf (arg)
  (list (mul* 2 arg (power '$%pi (inv -2)))
	(ref-fpq (list (1//2))
		 (list (div 3 2))
		 (mul* -1 arg arg))))

;; We know from A&S 15.1.3 that
;;
;; F(1,1;2;z) = -log(1-z)/z.
;;
;; So log(z) = (z-1)*F(1,1;2;1-z)
;;
(defun logtf (arg)
  ;; This seems wrong.   Why is the multipler 1 instead of (z-1)?
  (list #+nil 1
	(sub arg 1)
	(ref-fpq (list 1 1)
		 (list 2)
		 (sub 1 arg))))

;; Product of 2 Bessel J functions.
;;
;; See Y. L. Luke, formula 39, page 216:
;;
;; bessel_j(u,z)*bessel_j(v,z)
;;    = (z/2)^(u+v)/gamma(u+1)/gamma(v+1) *
;;        2F3((u+v+1)/2, (u+v+2)/2; u+1, v+1, u+v+1; -z^2)
(defun j2tf (n m arg)
  (list (mul* (inv (gm (add n 1)))
	      (inv (gm (add m 1)))
	      (inv (power 2 (add n m)))
	      (power arg (add n m)))
	(ref-fpq (list (add* (1//2) (div n 2) (div m 2))
		       (add* 1 (div n 2) (div m 2)))
		 (list (add 1 n) (add 1 m) (add* 1 n m))
		 (mul -1 (power arg 2)))))

;; Match d*x^m*%e^(a*x).  If we match, Q is the e^(a*x) part, A is a,
;; M is M, and D is d.
(defun d*x^m*%e^a*x
    (exp)
  (m2 exp
      '((mtimes)
	((coefftt)(d freevarpar))
	((mexpt) (x varp) (m freevarpar))
	((mexpt)
	 (q expor1p)
	 ((mtimes)((coefftt)(a freevarpar)) (x varp))))
      nil)) 

(defun execf19
    (l1 l2)
  (prog(ans)
     (setq ans (execargmatch (car (cddddr l2))))
     (cond ((eq (car ans) 'dionimo)
	    (return (dionarghyp l1 l2 (cadr ans)))))
     (return 'next-for-other-args)))

(defun execfy
    (l arg index)
  (prog(ans)
     (setq ans (execargmatch arg))
     (cond ((eq (car ans) 'dionimo)
	    (return (dionarghyp-y l index (cadr ans)))))
     (return 'fail-in-execfy)))

;; Executive for recognizing the sort of argument to the
;; hypergeometric function.  We look to see if the arg is of the form
;; a*x^m + c.  Return a list of 'dionimo (what does that mean?) and
;; the match.
(defun execargmatch (arg)
  (prog(l1)
     (cond ((setq l1 (a*x^m+c ($factor arg)))
	    (return (list 'dionimo l1))))
     (cond ((setq l1 (a*x^m+c ($expand arg)))
	    (return (list 'dionimo l1))))
     (return 'other-case-args-to-follow)))

;; We have hypergeometric function whose arg looks like a*x^m+c.  L1
;; matches the d*x^m... part, L2 is the hypergeometric function and
;; arg is the match for a*x^m+c.
(defun dionarghyp
    (l1 l2 arg)
  (prog(a m c)
     (setq a
	   (cdras 'a arg)
	   m
	   (cdras 'm arg)
	   c
	   (cdras 'c arg))
     (cond ((and (maxima-integerp m)(zerp c))
	    (return (f19cond a m l1 l2))))
     (return 'prop4-and-aother-cases-to-folow)))


;; Match f(x)+c
(defun f+c
    (exp)
  (m2 exp
      '((mplus)((coeffpt)(f hasvar))((coeffpp)(c freevar)))
      nil))

;; Match a*x^m+c.
(defun a*x^m+c
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (a freevar)
	 ((mexpt) (x varp) (m freevar0)))
	((coeffpp) (c freevar)))
      nil))

(defun freevar0(m)(cond ((equal m 0) nil)(t (freevar m))))

#+nil
(defun addarglist
    (s k)
  (prog(k1 l)
     (setq k1 (sub k 1))
     loop
     (cond ((zerp k1)
	    (return (append (list (div s k)) l))))
     (setq l
	   (append (list (div (add s k1) k)) l)
	   k1
	   (sub k1 1))
     (go loop)))

;; Return a list of s/k, (s+1)/k, ..., (s+|k|-1)/k
(defun addarglist (s k)
  (let ((abs-k (abs k))
	(res '()))
    (dotimes (n abs-k)
      (push (div (add s n) k) res))
    (nreverse res)))
    

(defun f19cond
    (a m l1 l2)
  (prog(p q s d)
     (setq p (caadr l2)
	   q (cadadr l2)
	   s (cdras 'm l1)
	   d (cdras 'd l1)
	   l1 (caddr l2)
	   l2 (cadddr l2))
     ;; At this point, we have the function d*x^s*%f[p,q](l1, l2, (a*t)^m).
     ;; Check to see if Formula 19, p 220 applies.
     (cond ((and (not (eq (checksigntm (sub (add* p
						  m
						  -1)
					    q))
			  '$positive))
		 (eq (checksigntm (add s 1))
		     '$positive))
	    (return (mul d
			 (f19p220-simp (add s 1)
				       l1
				       l2
				       a
				       m)))))
     (return 'failed-on-f19cond-multiply-the-other-cases-with-d)))

;; Table of Laplace transforms, p 220, formula 19:
;;
;; If m + k <= n + 1, and Re(s) > 0, the Laplace transform of
;;
;;    t^(s-1)*%f[m,n]([a1,...,am],[p1,...,pn],(c*t)^k)
;; is
;; 
;;    gamma(s)/p^s*%f[m+k,n]([a1,...,am,s/k,(s+1)/k,...,(s+k-1)/k],[p1,...,pm],(k*c/p)^k)
;;
;; with Re(p) > 0 if m + k <= n, Re(p+k*c*exp(2*%pi*%i*r/k)) > 0 for r
;; = 0, 1,...,k-1, if m + k = n + 1.
;;
;; The args below are s, [a's], [p's], c^k, k.
(defun f19p220-simp (s l1 l2 cf k)
  (mul* (gm s)
	(inv (power *par* s))
	(hgfsimp-exec (append l1 (addarglist s k))
		      l2
		      (mul* cf
			    (power k k)
			    (power (inv *par*) k))))) 

;; Bessel function expressed as a hypergeometric function.
;;
;; A&S 9.1.10:
;;                         inf
;; bessel_j(v,z) = (z/2)^v*sum (-z^2/4)^k/k!/gamma(v+k+1)
;;                         k=0
;;
;;               = (z/2)^v/gamma(v+1)*sum 1/poch(v+1,k)*(-z^2/4)^k/k!
;;
;;               = (z/2)^v/gamma(v+1) * 0F1(; v+1; -z^2/4)
(defun j1tf (v z)
  (list (mul* (inv (power 2 v))
	      (power z v)
	      (inv (gm (add v 1))))
	(ref-fpq nil
		 (list (add v 1))
		 (mul (inv -4)(power z 2)))))

(defun dionarghyp-y (l index arg) 
  (prog (a m c) 
     (setq a (cdras 'a arg) 
	   m (cdras 'm arg) 
	   c (cdras 'c arg))
     (cond ((and (zerp c) (equal m 1.))
	    (return (f2p105v2cond a l index))))
     (cond ((and (zerp c) (equal m (inv 2.)))
	    (return (f50cond a l index))))
     (return 'fail-in-dionarghyp-y))) 

(defun f2p105v2cond (a l index) 
  (prog (d m) 
     (setq d (cdras 'd l) m (cdras 'm l))
     (setq m (add m 1.))
     (cond ((eq (checksigntm ($realpart (sub m index)))
		'$positive)
	    (return (f2p105v2cond-simp m index a))))
     (return 'fail-in-f2p105v2cond))) 

(defun f50cond (a l v) 
  (prog (d m) 
     (setq d (cdras 'd l) 
	   m (cdras 'm l) 
	   m (add m (inv 2.)) 
	   v (div v 2.))
     (cond
       ((and (eq (checksigntm ($realpart (add m v (inv 2.))))
		 '$positive)
	     (eq (checksigntm ($realpart (sub (add m (inv 2.))
					      v)))
		 '$positive)
	     (not (maxima-integerp (mul (sub (add m m) (add v v 1.))
					(inv 2.)))))
	(setq a (mul a a (inv 4.)))
	(return (f50p188-simp d m v a))))
     (return 'fail-in-f50cond))) 

;; Table of Integral Transforms
;;
;; p. 188, formula 50:
;;
;; t^(u-1/2)*bessel_y(2*v,2*sqrt(a)*sqrt(t))
;;    -> a^(-1/2)*p^(-u)*exp(-a/2/p)
;;       * [tan((u-v)*%pi)*gamma(u+v+1/2)/gamma(2*v+1)*M[u,v](a/p)
;;          -sec((u-v)*%pi)*W[u,v](a/p)]
(defun f50p188-simp (d u v a)
  (mul d
       (power a (inv -2))
       (power *par* (mul -1 u))
       (power '$%e (div a (mul -2 *par*)))
       (sub (mul (tan% (mul '$%pi (sub u v)))
		 (gm (add u v (inv 2)))
		 (inv (gm (add v v 1)))
		 (mwhit (div a *par*) u v))
	    (mul `((%sec) ,(mul '$%pi (sub u v)))
		 (wwhit (div a *par*) u v)))))

;; Table of Integral Transforms
;;
;; Volume 2, p 105, formula 2 is a formula for the Y-transform of
;;
;;    f(x) = x^(u-3/2)*exp(-a*x)
;;
;; where the Y-transform is defined by
;;
;;    integrate(f(x)*bessel_y(v,x*y)*sqrt(x*y), x, 0, inf)
;;
;; which is
;;
;;    -2/%pi*gamma(u+v)*sqrt(y)*(y^2+a^2)^(-u/2)*assoc_legendre_q(u-1,-v,a/sqrt(y^2+a^2))
;;
;; with a > 0, Re u > |Re v|.
;;
;; In particular, with a slight change of notation, we have
;;
;;    integrate(x^(u-1)*exp(-p*x)*bessel_y(v,a*x)*sqrt(a), x, 0, inf)
;;
;; which is the Laplace transform of x^(u-1/2)*bessel_y(v,x).
;;
;; Thus, the Laplace transform is
;;
;;    -2/%pi*gamma(u+v)*sqrt(a)*(a^2+p^2)^(-u/2)*assoc_legendre_q(u-1,-v,p/sqrt(a^2+p^2))
;;
;; 
(defun f2p105v2cond-simp (m v a) 
  (mul -2.
       (power '$%pi -1.)
       (gm (add m v))
       (power (add (mul a a) (mul *par* *par*))
	      (mul -1. (inv 2.) m))
       (leg2fsimp (sub m 1.)
		  (mul -1. v)
		  (mul *par*
		       (power (add (mul a a) (mul *par* *par*))
			      (inv -2.)))))) 

;; This doesn't seem to be used anywhere.
;;
;; A&S 8.1.2:
;;
;; assoc_legendre_p(v,m,z)
;;    = 1/gamma(1-m)*((z+1)/(z-1))^(m/2)*F(-v,v+1;1-m;(1-z)/2)
;;
;; for |1-z|<2
;;
;; Note: The args here are reversed from our definition of
;; assoc_legendre_p!
#+nil
(defun leg1fsimp (m v z) 
  (mul (inv (gm (sub 1. m)))
       (power (div (add z 1.) (sub z 1.)) (div m 2.))
       (hgfsimp-exec (list (mul -1. v) (add v 1.))
		     (list (sub 1. m))
		     (sub (inv 2.) (div z 2.))))) 

;; A&S 8.1.3:
;;
;; assoc_legendre_q(v,m,z)
;;    = exp(%i*%pi*m)*2^(-v-1)*sqrt(%pi)*gamma(v+m+1)/gamma(v+3/2)*z^(-v-u-1)
;;        *(z^2-1)^(m/2)*F(1+v/2+u/2,1/2+v/2+u/2;v+3/2;1/z^2)
;;
;; for |z| > 1.
;;
;; But note that we are called with z = p/sqrt(p^2+a^2) so |z| < 1 for
;; all real p and a.  So I (rtoy) don't think this is the right thing
;; to use.
;;
;; So, for now, just return the Legendre Q function and hope that
;; someone else can simplify it.
(defun leg2fsimp (m v z)
  (cond (t
	 (legen m v z '$q))
	(nil
	 (mul (power '$%e (mul m '$%pi '$%i))
	      (power '$%pi (inv 2.))
	      (gm (add m v 1.))
	      (inv (power 2. (add v 1.)))
	      (inv (gm (add v (div 3. 2.))))
	      (power z (sub -1. (add m v)))
	      (power (sub (mul z z) 1.) (mul (inv 2.) m))
	      (hgfsimp-exec (list (div (add m v 1.) 2.)
				  (div (add m v 2.) 2.))
			    (list (add v (mul 3. (inv 2.))))
			    (inv (mul z z)))))))

