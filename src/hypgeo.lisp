;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;


;;    ** (c) Copyright 1976, 1983 Massachusetts Institute of Technology **
(in-package "MAXIMA")

;;These are the main routines for finding the Laplace Transform
;; of special functions   --- written by Yannis Avgoustis
;;                        --- modified by Edward Lafferty
;;                       Latest mod by jpg 8/21/81
;;
;;   This program uses the programs on ELL;HYP FASL.

(macsyma-module hypgeo)

(declare-top (special var par zerosigntest productcase checkcoefsignlist
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

(defun gm (expr)
  (simplifya (list '(%gamma) expr) nil))

(defun sin%(arg)(list '(%sin) arg))

(defun nump
    (x)
  (cond ((atom x)(numberp x))
	((not (atom x))(eq (caar (simplifya x nil)) 'rat))))

(defun cos%(arg)(list '(%cos) arg))

(defun neginp (a) (cond ((maxima-integerp a)(or (zerp a)(minusp a)))))

(defun notnump(x)(not (nump x)))

(defun negnump
    (x)
  (cond ((not (maxima-integerp x))
	 (minusp (cadr (simplifya x nil))))
	(t (minusp x))))



(defun expor1p(exp)(or (equal exp 1)(eq exp '$%e)))

(defun parp(a)(eq a par))



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

(defun u*asinx
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt) (u nonzerp)((%asin)(x hasvar)))
	((coeffpp)(a zerp)))
      nil))

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

;;...AMONG GARBAGE RECOGNIZES J[V1](W1)*J[V2](W2)


(defun twoj
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_j) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun twoy
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun twok
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_k) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun onekoney
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;;...AMONG GARBAGE RECOGNIZES J[V](W)^2.


(defun onej^2
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_j) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

(defun oney^2
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_y) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

(defun onek^2
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_k) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

(defun onei
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun twoi
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_i) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

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

(defun oneyonej
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun onekonej
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun oneyoneh
    (exp)
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

(defun onerf
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%erf)(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun onelog
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%log)(w hasvar)))
	((coeffpp)(a zerp)))
      nil))

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

(defun onegammaincomplete
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($gammaincomplete)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Incomplete gamma function, integrate(t^(v-1)*exp(-t),t,0,x)
(defun onegammagreek
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($gammagreek)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

(defun onehstruve
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($hstruve array)(v true))(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun onelstruve
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($lstruve array)(v true))(w true)))
	((coeffpp)(a zerp)))
      nil))

(defun ones
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%s array)(v1 true)(v2 true))(w true)))
	((coeffpp)(a zerp)))
      nil))

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

(defun oney
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

(defun onek
    (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

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



(defun defintegrate
    (exp)
  (prog ($exponentialize)
     (setq $exponentialize t)
     (return (distrdefexecinit ($expand (ssimplifya exp))))))


(defun defexec
    (exp var)
  (prog(l a)
     (setq exp (simplifya exp nil))
     (cond ((setq l (defltep exp))
	    (setq a (cdras 'a l))
	    (return (negtest l a))))
     (return 'other-defint-to-follow-defexec)))

(defun negtest
    (l a)
  (prog(u e f c)
     (cond ((eq (checksigntm ($realpart a)) '$negative)
	    (setq u
		  (cdras 'u l)
		  e
		  (cdras 'e l)
		  f
		  (cdras 'f l)
		  c
		  (cdras 'c l))
	    (cond ((zerp e)(setq f 1)))
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

(defun ltscale
    (exp var par c par0 e f)
  (mul* (power '$%e c)
	(substl (sub par par0) par (lt-exec exp e f))))

(defun defltep
    (exp)
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


(defun hypgeo-exec (exp var par)
  (prog (l u a c e f)
     (setq exp (simplifya exp nil))
     (cond ((setq l (ltep exp))
	    (setq u (cdras 'u l)
		  a (cdras 'a l)
		  c (cdras 'c l)
		  e (cdras 'e l)
		  f (cdras 'f l))
	    (return (ltscale u var par c a e f))))
     (return 'other-trans-to-follow)))

(defun substl
    (p1 p2 p3)
  (cond ((eq p1 p2) p3)(t (maxima-substitute p1 p2 p3)))) 

(defun lt-exec (u e f)
  (declare (special *asinx* *atanx*))
  (prog (l)
     (cond ((or *asinx* *atanx*)
	    (return (lt-asinatan u e))))
     (cond ((zerp e)(return (lt-sf-log u))))
     (cond ((and (not (zerp e))(setq l (c*t^v u)))
	    (return (lt-exp l e f))))
     (return (lt-sf-log (mul* u (power '$%e (mul e f)))))))

(defun c*t^v
    (exp)
  (m2 exp
      '((mtimes)
	((coefftt)(c freevar))
	((mexpt)(t varp)(v freevar)))
      nil))

(defun lt-asinatan (u e)
  (declare (special *asinx* *atanx*))
  (cond ((zerp e)
	 (cond (*asinx* (lt-ltp 'asin u var nil))
	       (*atanx* (lt-ltp 'atan u var nil))
	       (t 'lt-asinatan-failed-1)))
	(t 'lt-asinatan-failed-2)))

(defun lt-exp
    (l e f)
  (prog(c v)
     (setq c (cdras 'c l) v (cdras 'v l))
     (cond ((t^2 f)
	    (setq e (inv (mul -8 e)) v (add v 1))
	    (return (f24p146test c v e))))
     (cond ((sqroott f)
	    (setq e (mul* e e (inv 4)) v (add v 1))
	    (return (f35p147test c v e))))
     (cond ((t^-1 f)
	    (setq e (mul -4 e) v (add v 1))
	    (return (f29p146test v e))))
     (return 'other-lt-exponential-to-follow)))

(defun t^2(exp)(m2 exp '((mexpt)(t varp) 2) nil))

(defun sqroott(exp)(m2 exp '((mexpt)(t varp)((rat) 1 2)) nil))

(defun t^-1(exp)(m2 exp '((mexpt)(t varp) -1) nil))

(defun f24p146test
    (c v a)
  (cond ((not (or (neginp a)(neginp v)))(f24p146 c v a))
	(t 'fail-on-f24p146test)))

(defun f35p147test
    (c v a)
  (cond ((not (neginp v))(f35p147 c v a))
	(t 'fail-on-f35p147test)))

(defun f29p146test (v a)
  (cond ((not (neginp a))
	 (f29p146 v a))
	(t 'fail-on-f29p146test)))

(defun f1p137test
    (pow)
  (cond ((not (neginp (add pow 1)))(f1p137 pow))
	(t 'fail-in-arbpow))) 

(defun f1p137
    (pow)
  (mul* (gm (add pow 1))(power par (sub (mul -1 pow) 1))))

(defun f24p146
    (c v a)
  (mul* c
	(gm v)
	(power 2 v)
	(power a (div v 2))
	(power '$%e (mul* a par par))
	(dtford (mul* 2 par (power a (1//2)))(mul -1 v))))

(defun f35p147
    (c v a)
  (mul* c
	(gm (add v v))
	(power 2 (sub 1 v))
	(power par (mul -1 v))
	(power '$%e (mul* a (1//2)(inv par)))
	(dtford (power (mul* 2 a (inv par))(1//2))(mul -2 v))))

(defun f29p146 (v a)
  (mul* 2
	(power (mul* a (inv 4)(inv par))(div v 2))
	(ktfork a v)))

(defun ktfork
    (a v)
  ((lambda(z)
     (cond ((maxima-integerp v)(kmodbes z v))
	   (t (simpktf z v))))
   (power (mul* a par)(1//2))))

(defun dtford
    (z v)
  (cond (((lambda(inv4)
	    (whittindtest (add (div v 2) inv4) inv4))
	  (inv 4))
	 (parcyl z v))
	(t (simpdtf z v))))


(defun simpdtf
    (z v)
  ((lambda(inv2 pow)
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
			      (mul* z z inv2)))))
   (1//2)
   (power '$%e (mul* z z (inv -4)))))

(defun simpktf
    (z v)
  ((lambda(dz2)
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
					  (inv 4)))))))
   (div z 2))) 
;;dispatches according to the special functions involved in the laplace transformable expression

(defun lt-sf-log
    (u)
  (prog(l index1 index11 index2 index21 arg1 arg2 rest)
     (cond ((setq l (twoj u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (twoh u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v11 l)
		  index2
		  (cdras 'v2 l)
		  index21
		  (cdras 'v21 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      index11
			      index2
			      index21
			      '2htjory))))
     (cond ((setq l (twoy u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      '2ytj))))
     (cond ((setq l (twok u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      '2kti))))
     (cond ((setq l (onekoney u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg2
			      index1
			      nil
			      index2
			      nil
			      'ktiytj))))
     (cond ((setq l (oneionej u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  index21
		  (cdras 'v21 l)
		  arg1
		  (mul* (1fact t t)(cdras 'w1 l))
		  arg2
		  (cdras 'w2 l)
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (oneioneh u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  index21
		  (cdras 'v21 l)
		  arg1
		  (mul* (1fact t t)(cdras 'w1 l))
		  arg2
		  (cdras 'w2 l)
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       index21
			       'besshtjory))))
     (cond ((setq l (oneyonej u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       nil
			       'bessytj))))
     (cond ((setq l (onekonej u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       nil
			       'besskti))))
     (cond ((setq l (onehonej u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v11 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'besshtjory))))
     (cond ((setq l (oneyoneh u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  index11
		  (cdras 'v21 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'htjoryytj))))
     (cond ((setq l (onekoneh u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  index11
		  (cdras 'v21 l)
		  arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest1 rest
			       arg2
			       arg1
			       index2
			       index1
			       index11
			       'htjorykti))))
     (cond ((setq l (oneioney u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (mul* (1fact t t)(cdras 'w1 l))
		  arg2
		  (cdras 'w2 l)
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       nil
			       'bessytj))))
     (cond ((setq l (oneionek u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (mul* (1fact t t)(cdras 'w1 l))
		  arg2
		  (cdras 'w2 l)
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (fractest1 rest
			       arg1
			       arg2
			       index1
			       index2
			       nil
			       'besskti))))
     (cond ((setq l (onehstruve u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1hstruve rest arg1 index1))))
     (cond ((setq l (onelstruve u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1lstruve rest arg1 index1))))
     (cond ((setq l (ones u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1s rest arg1 index1 index2))))
     (cond ((setq l (oneslommel u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       index2
			       'slommel))))
     (cond ((setq l (oney u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1yref rest arg1 index1))))
     (cond ((setq l (onek u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       nil
			       'kti))))
     (cond ((setq l (oned u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest arg1 index1 nil 'd))))
     (cond ((setq l (onegammaincomplete u))
	    (setq arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       arg2
			       nil
			       'gammaincomplete))))
     (cond ((setq l (onekbateman u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       nil
			       'kbateman))))
     (cond ((setq l (onej u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1j rest arg1 index1))))
     (cond ((setq l (onegammagreek u))
	    (setq arg1
		  (cdras 'w1 l)
		  arg2
		  (cdras 'w2 l)
		  rest
		  (cdras 'u l))
	    (return (lt1gammagreek rest arg1 arg2))))
     (cond ((setq l (oneh u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest2 rest
			       arg1
			       index1
			       index11
			       'htjory))))
     (cond ((setq l (onem u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1m rest arg1 index1 index11))))
     (cond ((setq l (onel u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 index11
				 'l))))
     (cond ((setq l (onec u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 index11
				 'c))))
     (cond ((setq l (onet u))
	    (setq index1
		  (cdras 'v1 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 't))))
     (cond ((setq l (oneu u))
	    (setq index1
		  (cdras 'v1 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 'u))))
     (cond ((setq l (onehe u))
	    (setq index1
		  (cdras 'v1 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (integertest rest
				 arg1
				 index1
				 nil
				 'he))))
     (cond ((setq l (hyp-onep u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     (cond ((setq l (onepjac u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  index21
		  (cdras 'v3 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (pjactest rest
			      arg1
			      index1
			      index2
			      index21))))
     (cond ((setq l (oneq u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1q rest arg1 index1 index11))))
     (cond ((setq l (onep0 u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  0
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     (cond ((setq l (onew u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (whittest rest arg1 index1 index11))))
     (cond ((setq l (onej^2 u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (lt1j^2 rest arg1 index1))))
     (cond ((setq l (oneh^2 u))
	    (setq index1
		  (cdras 'v1 l)
		  index11
		  (cdras 'v2 l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      index11
			      index1
			      index11
			      '2htjory))))
     (cond ((setq l (oney^2 u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      nil
			      index1
			      nil
			      '2ytj))))
     (cond ((setq l (onek^2 u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (cdras 'w l)
		  rest
		  (cdras 'u l))
	    (return (fractest rest
			      arg1
			      arg1
			      index1
			      nil
			      index1
			      nil
			      '2kti))))
     (cond ((setq l (twoi u))
	    (setq index1
		  (cdras 'v1 l)
		  index2
		  (cdras 'v2 l)
		  arg1
		  (mul* (1fact t t)(cdras 'w1 l))
		  arg2
		  (mul* (1fact t t) (cdras 'w2 l))
		  rest
		  (mul* (1fact nil index1)
			(1fact nil index2)
			(cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     (cond ((setq l (onei u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (mul* (1fact t t)(cdras 'w l))
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt1j rest arg1 index1))))
     (cond ((setq l (onei^2 u))
	    (setq index1
		  (cdras 'v l)
		  arg1
		  (mul* (1fact t t)(cdras 'w l))
		  rest
		  (mul* (1fact nil index1)(cdras 'u l)))
	    (return (lt1j^2 rest arg1 index1))))
     (cond ((setq l (onerf u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (lt1erf rest arg1))))
     (cond ((setq l (onelog u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (lt1log rest arg1))))
     (cond ((setq l (onerfc u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (fractest2 rest arg1 nil nil 'erfc))))
     (cond ((setq l (oneei u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (fractest2 rest arg1 nil nil 'ei))))
     (cond ((setq l (onekelliptic u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (lt1kelliptic rest arg1))))
     (cond ((setq l (onee u))
	    (setq arg1 (cdras 'w l) rest (cdras 'u l))
	    (return (lt1e rest arg1))))
     (cond ((setq l (arbpow1 u))
	    (setq arg1
		  (cdras 'u l)
		  arg2
		  (cdras 'c l)
		  index1
		  (cdras 'v l))
	    (return (mul arg2 (lt-arbpow arg1 index1)))))
     (return 'other-j-cases-next)))

(defun lt-arbpow
    (exp pow)
  (cond ((or (eq exp var)(zerp pow))(f1p137test pow))))

(defun fractest
    (r a1 a2 i1 i11 i2 i21 flg)
  (cond ((or (and (equal (caar i1) 'rat)
		  (equal (caar i2) 'rat))
	     (eq flg '2htjory))
	 (sendexec r
		   (cond ((eq flg '2ytj)
			  (mul (ytj i1 a1)(ytj i2 a2)))
			 ((eq flg '2htjory)
			  (mul (htjory i1 i11 a1)
			       (htjory i2 i21 a2)))
			 ((eq flg 'ktiytj)
			  (mul (kti i1 a1)(ytj i2 a2)))
			 ((eq flg '2kti)
			  (mul (kti i1 a1)(kti i2 a2))))))
	(t 'product-of-y-with-nofract-indices)))

(defun fractest1 (r a1 a2 i1 i2 i flg)
  (cond ((or (and (listp i2)
		  (equal (caar i2) 'rat))
	     (eq flg 'besshtjory))
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

(defun fractest2 (r a1 i1 i11 flg)
  (cond ((or (and (listp i1)
		  (equal (caar i1) 'rat))
	     (eq flg 'd)
	     (eq flg 'kbateman)
	     (eq flg 'gammaincomplete)
	     (eq flg 'htjory)
	     (eq flg 'erfc)
	     (eq flg 'ei)
	     (eq flg 'slommel))
	 (sendexec r
		   (cond ((eq flg 'ytj)(ytj i1 a1))
			 ((eq flg 'htjory)
			  (htjory i1 i11 a1))
			 ((eq flg 'd)(dtw i1 a1))
			 ((eq flg 'kbateman)
			  (kbatemantw a1))
			 ((eq flg 'gammaincomplete)
			  (gammaincompletetw a1 i1))
			 ((eq flg 'kti)(kti i1 a1))
			 ((eq flg 'erfc)(erfctd a1))
			 ((eq flg 'ei)
			  (eitgammaincomplete a1))
			 ((eq flg 'slommel)
			  (slommeltjandy i1 i11 a1)))))
	(t 'y-of-nofract-index)))

(defun lt1yref
    (rest arg1 index1)
  (cond ((maxima-integerp index1)(lt1y rest arg1  index1))
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

(defun whittest
    (r a i1 i2)
  (cond ((whittindtest i1 i2) 'formula-for-confl-needed)
	(t (distrexecinit ($expand (mul (init r)
					(wtm a i1 i2)))))))

(defun whittindtest
    (i1 i2)
  (or (maxima-integerp (add i2 i2))
      (neginp (sub (sub (1//2) i2) i1))
      (neginp (sub (add (1//2) i2) i1))))

(defun init(r)(mul* r (power '$%e (mul* -1 var par))))

(defun ltw
    (x n a)
  ((lambda(diva2)
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

(defun hetd(x n)(mul* (power '$%e (mul* x x (inv 4)))(parcyl x n)))

(defun erfctd
    (x)
  ((lambda(inv2)
     (mul* (power 2 inv2)
	   (power '$%pi (mul* -1 inv2))
	   (power '$%e (mul* -1 inv2 x x))
	   (parcyl (mul* (power 2 inv2) x) -1)))
   (1//2)))

(defun eitgammaincomplete(x)(mul* -1 (gminc 0 (mul -1 x))))

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

(defun wtm
    (a i1 i2)
  (add (mul* (gm (mul -2 i2))
	     (mwhit a i1 i2)
	     (inv (gm (sub (sub (1//2) i2) i1))))
       (mul* (gm (add i2 i2))
	     (mwhit a i1 (mul -1 i2))
	     (inv (gm (sub (add (1//2) i2) i1))))))

(defun gammaincompletetw
    (a x)
  (mul* (power x (div (sub a 1) 2))
	(power '$%e (div x -2))
	(wwhit x (div (sub a 1) 2)(div a 2))))

(defun distrexecinit (fun)
  (cond ((equal (caar fun) 'mplus) (distrexec (cdr fun)))
	(t (hypgeo-exec fun var par))))

(defun distrdefexecinit (fun)
  (cond ((equal (caar fun) 'mplus) (distrdefexec (cdr fun)))
	(t (defexec fun var))))

(defun distrexec (fun)
  (cond ((null fun) 0)
	(t (add (hypgeo-exec (car fun) var par)
		(distrexec (cdr fun))))))

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

(defun dtw (i a)
  (mul* (power 2 (add (div i 2)(inv 4)))
	(power a (inv -2))
	(wwhit (mul* a a (1//2))
	       (add (div i 2)(inv 4))
	       (inv 4))))

(defun kbatemantw (a)
  ((lambda(ind)
     (div (wwhit (add a a) ind (1//2))
	  (gm (add ind 1))))
   (div 1 2)))

(defun kti
    (i a)
  (mul* '$%pi
	(1//2)
	(inv (sin% (mul i '$%pi)))
	(sub (bess (mul -1 i) a 'i)(bess i a 'i))))

(defun 1fact
    (flg v)
  (power '$%e
	 (mul* '$%pi
	       '$%i
	       (1//2)
	       (cond (flg 1)(t (mul -1 v))))))

;; Bessel Y
(defun bessy (v z)
  `((%bessel_y) ,v ,z))

;; Bessel K
(defun kmodbes(z v)
  `((%bessel_k) ,v ,z))



(defun tan%(arg)(list  '(%tan) arg))

;; Bessel J or Y, depending on if FLG is 'J or not.
(defun desjy (v z flg)
  (cond ((eq flg 'j)
	 (bess v z 'j))
	(t
	 (bessy v z))))

(defun numjory
    (v sort z flg)
  (cond ((equal sort 1)
	 (sub (desjy (mul -1 v) z flg)
	      (mul* (power '$%e (mul* -1 v '$%pi '$%i))
		    (desjy v z flg))))
	(t (sub (mul* (power '$%e (mul* v '$%pi '$%i))
		      (desmjy v z flg))
		(desmjy (mul -1 v) z flg)))))

(defun desmjy
    (v z flg)
  (cond ((eq flg 'j)(bess v z 'j))(t (mul -1 (bessy v z)))))

(defun htjory
    (v sort z)
  (cond ((equal (caar v) 'rat)
	 (div (numjory v sort z 'j)
	      (mul* '$%i (sin% (mul v '$%pi)))))
	(t (div (numjory v sort z 'y)(sin% (mul v '$%pi)))))) 
;;expert on l.t. expressions containing one bessel function of the first kind

(defun lt1j(rest arg index)(lt-ltp 'onej rest arg index))

(defun lt1y(rest arg index)(lt-ltp 'oney rest arg index))

(defun lt2j
    (rest arg1 arg2 index1 index2)
  (cond ((not (equal arg1 arg2))
	 'product-of-bessel-with-different-args)
	(t (lt-ltp 'twoj
		   rest
		   arg1
		   (list 'list index1 index2)))))

(defun lt1j^2
    (rest arg index)
  (lt-ltp 'twoj rest arg (list 'list index index)))

(defun lt1gammagreek
    (rest arg1 arg2)
  (lt-ltp 'gammagreek rest arg2 arg1))

(defun lt1m(r a i1 i2)(lt-ltp 'onem r a (list i1 i2)))

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
	  (list 'fpq
		(list 1 2)
		(list 1)
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
    (list (mul* (power (div z 2)(add v 1))
		(inv (gm d32))
		(inv (gm (add v d32))))
	  (list 'fpq
		(list 1 2)
		(list 1)
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

(defun lt-ltp
    (flg rest arg index)
  (prog(index1 index2 argl const l l1)
     (cond ((or (zerp index)
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
	    (go labl)))
     (cond ((or (eq flg 'hyp-onep)(eq flg 'onelog))
	    (go labl1)))
     (cond ((not (consp index)) (go lab)))
     (cond ((not (eq (car index) 'list))(go lab)))
     (cond ((zerp (setq index1 (cadr index)))(go la)))
     (cond ((eq (checksigntm (simplifya (inv (setq index1
						   (cadr
						    index)))
					nil))
		'$negative)
	    (setq index1
		  (mul -1 index1)
		  rest
		  (mul* (power -1 index1) rest))))
     la
     (cond ((zerp (setq index2 (caddr index)))(go la2)))
     (cond ((eq (checksigntm (simplifya (inv (setq index2
						   (caddr
						    index)))
					nil))
		'$negative)
	    (setq index2
		  (mul -1 index2)
		  rest
		  (mul* (power -1 index2) rest))))
     la2
     (setq index (list index1 index2))
     (go labl)
     lab
     (cond ((and (eq (checksigntm (simplifya (inv index)
					     nil))
		     '$negative)
		 (maxima-integerp index))
	    (setq index (mul -1 index))
	    (setq rest (mul (power -1 index) rest))))
     labl
     (setq argl (f+c arg))
     (setq const (cdras 'c argl) arg (cdras 'f argl))
     (cond ((null const)(go labl1)))
     (cond ((not (eq (checksigntm (simplifya (power const
						    2)
					     nil))
		     '$zero))
	    (return 'prop4-to-be-applied)))
     labl1
     (cond ((eq flg 'oney)(return (lty rest arg index))))
     (cond ((setq l
		  (d*x^m*%e^a*x ($factor (mul* rest
					       (car (setq
						     l1
						     (ref
						      flg
						      index
						      arg)))))))
	    (return (%$etest l l1))))
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
     (return (substl (sub par a)
		     par
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
    (twoj (j2tf (car index)(cadr index) arg))
    (hs (hstf index arg))
    (hl (lstf index arg))
    (s (stf (car index)(cadr index) arg))
    (onerf (erftf arg))
    (onelog (logtf arg))
    (onekelliptic (kelliptictf arg))
    (onee (etf arg))
    (onem (mtf (car index)(cadr index) arg))
    (hyp-onep (ptf (car index)(cadr index) arg))
    (oneq (qtf (car index)(cadr index) arg))
    (gammagreek (gammagreektf index arg))
    (onepjac 
     (pjactf (car index)(cadr index)(caddr index) arg))
    (asin (asintf arg))
    (atan (atantf arg))))

(defun ref-fpq (p q arg)
  (list 'fpq (list (length p) (length q))
	p q arg))

;; Whittaker M function.
;;
;; A&S 13.1.32:
;;
;; M[k,u](z) = exp(-z/2)*z^(1/2+u)*M(1/2+u-k,1+2*u,z)
;;
(defun mtf
    (i1 i2 arg)
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
	(ref-fp1 (list (mul -1 n) (add* n a b 1))
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
(defun kelliptictf
    (k)
  ((lambda(inv2)
     (list (mul inv2 '$%pi)
	   (ref-fpq (list inv2 inv2)
		    (list 1)
		    (mul k k))))
   (1//2)))

(defun etf
    (k)
  ((lambda(inv2)
     (list (mul inv2 '$%pi)
	   (list 'fpq
		 (list  2 1)
		 (list (mul -1 inv2) inv2)
		 (list 1)
		 (mul k k))))
   (1//2)))

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
;;executive  for recognizing the sort of argument

(defun execargmatch
    (arg)
  (prog(l1)
     (cond ((setq l1 (a*x^m+c ($factor arg)))
	    (return (list 'dionimo l1))))
     (cond ((setq l1 (a*x^m+c ($expand arg)))
	    (return (list 'dionimo l1))))
     (return 'other-case-args-to-follow)))

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

 
(defun f+c
    (exp)
  (m2 exp
      '((mplus)((coeffpt)(f hasvar))((coeffpp)(c freevar)))
      nil))

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

(defun f19cond
    (a m l1 l2)
  (prog(p q s d)
     (setq p
	   (caadr l2)
	   q
	   (cadadr l2)
	   s
	   (cdras 'm l1)
	   d
	   (cdras 'd l1)
	   l1
	   (caddr l2)
	   l2
	   (cadddr l2))
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
	(inv (power par s))
	(hgfsimp-exec (append l1 (addarglist s k))
		      l2
		      (mul* cf
			    (power k k)
			    (power (inv par) k))))) 

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

(defun f2p105v2cond-simp (m v a) 
  (mul -2.
       (power '$%pi -1.)
       (gm (add m v))
       (power (add (mul a a) (mul par par)) (mul -1. (inv 2.) m))
       (leg2fsimp (sub m 1.)
		  (mul -1. v)
		  (mul par
		       (power (add (mul a a) (mul par par))
			      (inv -2.)))))) 

(defun leg1fsimp (m v z) 
  (mul (inv (gm (sub 1. m)))
       (power (div (add z 1.) (sub z 1.)) (div m 2.))
       (hgfsimp-exec (list (mul -1. v) (add v 1.))
		     (list (sub 1. m))
		     (sub (inv 2.) (div z 2.))))) 

(defun leg2fsimp (m v z) 
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
		     (inv (mul z z))))) 

