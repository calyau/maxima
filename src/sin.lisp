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
(macsyma-module sin)

;;; Reference:  J. Moses, Symbolic Integration, MIT-LCS-TR-047, 12-1-1967.
;;; http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-047.pdf.


(declare-top (special ratform exptsum $radexpand $%e_to_numlog
		      exptind quotind splist l ans splist arcpart coef
		      aa dict exptflag base* powerlist a b k stack
		      ratroot rootlist square e w y expres arg var
		      *powerl* c d exp chebyform ratrootform trigarg
		      #+nil notsame
		      #+nil yy
		      #+nil b1
		      #+nil yz
		      varlist genvar repswitch $liflag
		      noparts top maxparts numparts blank $opsubst)
	     (*expr powerlist ratroot)
	     (*lexpr $factor $expand)
	     (genprefix sin))

(defmvar $integration_constant_counter 0)

(defun sassq1 (arg list fn)
  (or (zl-assoc arg list) (funcall fn))
  #|
  (COND ((NULL LIST) (FUNCALL FN))
	((EQUAL (CAAR LIST) ARG) (CAR LIST))
	(T (SASSQ1 ARG (CDR LIST) FN)))
  |#
  )
	 

(defmacro op (frob)
  `(get ,frob 'operators))

(defun integerp1 (x)
  (integerp2 (mul2* 2 x)))

(defun superexpt (exp var base*) 
  (prog (exptflag y w) 
	(setq y (elemxpt exp))
	(cond (exptflag (return nil)))
	(return
	 (substint
	  (list '(mexpt) base* var)
	  var
	  (integrator (div y (mul2 var (simplog (list base*)))) var)))))
 
(defun elemxpt (exp)
  (cond ((freevar exp) exp)
	((atom exp) (setq exptflag t))
	((not (eq (caar exp) 'mexpt))
	 (cons (car exp)
	       (mapcar 
		(function (lambda (c) (elemxpt c)))
		(cdr exp))))
	((not (freevar (cadr exp)))
	 (list '(mexpt)
	       (elemxpt (cadr exp))
	       (elemxpt (caddr exp))))
	((not (eq (cadr exp) base*))
	 (elemxpt (list '(mexpt)
			base*
			(simplify (list '(mtimes)
					(list '(mexpt)
					      (list '(%log) base*) -1)					   (list '(%log)
					      (cadr exp))
					(caddr exp))))))
	((not (setq w
		    (m2 (caddr exp)
			'((mplus)
			  ((coeffpt) (a freevar) (var varp))
			  ((coeffpt) (b freevar)))
			nil)))
	 (list (car exp) base* (elemxpt (caddr exp))))
	(t (maxima-substitute base*
			      'base*
			      (subliss w
				       '((mtimes)
					 ((mexpt) base* b)
					 ((mexpt) var a)))))))

(defun subst10 (ex) 
  (cond ((atom ex) ex)
	((and (eq (caar ex) 'mexpt) (eq (cadr ex) var))
	 (list '(mexpt) var (integerp2 (quotient (caddr ex) d))))
	(t (cons (ncons (caar ex))
		 (mapcar #'(lambda (c) (subst10 c)) (cdr ex))))))

(defun choicesin (x1 x2) 
  (if (eq x1 (car x2))
      (cdr x2)
      (cons (car x2)
	    (choicesin x1 (cdr x2)))))
	 
(defun rationalizer (x)
  (let ((ex (simplify ($factor x))))
    (if (not (alike1 ex x)) ex)))

(defun intform (expres) 
  (cond
   ((freevar expres) nil)
   ((atom expres) nil)
   ((memq (caar expres) '(mplus mtimes))
    ((lambda (l) (prog (y) 
		  loop (cond ((setq y (intform (car l))) (return y))
			     ((not (setq l (cdr l))) (return nil))
			     (t (go loop)))))
     (cdr expres)))
   ((or (eq (caar expres) '%log) (arcp (caar expres)))
    (cond
     ((setq arg (m2 exp 
		    `((mtimes) ((,(caar expres)) (b rat8))
		      ((coefftt) (c rat8prime)))
		    nil))
      (ratlog exp var (cons (cons 'a expres) arg)))
     (t
      (prog (y z) 
	 (cond
	   ((setq y (intform (cadr expres))) (return y))
	   ((and (eq (caar expres) '%log)
		 (setq z (m2 (cadr expres) c nil))
		 (setq y (m2 exp
			     '((mtimes)
			       ((coefftt) (c rat8))
			       ((coefftt) (d elem)))
			     nil)))
	    (return
	      ((lambda (a b c d) 
		 (substint
		  expres
		  var
		  (integrator
		   (muln
		    (list (maxima-substitute
			   `((mquotient) ((mplus) ((mexpt) $%e ,var)
					  ((mtimes) -1 ,a))
			     ,b)
			   var
			   c)
                          `((mquotient) ((mexpt) $%e ,var) ,b)
			  (maxima-substitute var expres d))
		    nil)
		   var)))
	       (cdr (sassq 'a z 'nill))
	       (cdr (sassq 'b z 'nill))
	       (cdr (sassq 'c y 'nill))
	       (cdr (sassq 'd y 'nill)))))
	   (t (return nil)))))))
   ((optrig (caar expres))
    (cond ((not (setq w (m2 (cadr expres) c nil)))
	   (intform (cadr expres)))
	  (t
	   (prog2
	       (setq *powerl* t)
	       (monstertrig exp var (cadr expres))))))
   ((and (eq (caar expres) '%derivative)
	 (eq (caar exp) (caar expres))
	 (or (atom (cadr exp))
	     (not (eq (caaadr exp) 'mqapply))
	     (merror "Invalid arg to `integrate':~%~M" exp))
	 (checkderiv exp)))
   ((not (eq (caar expres) 'mexpt)) nil)
   ((integerp (caddr expres)) (intform (cadr expres)))
   ((freevar (cadr expres))
    (cond ((m2 (caddr expres) c nil)
	   (superexpt exp var (cadr expres)))
	  ((intform (caddr expres)))
	  (t (let* (($%e_to_numlog t)
		    (nexp (resimplify exp)))
		   (cond ((alike1 exp nexp) nil)
			 (t (intform (setq exp nexp))))))))
   ((not (rat8 (cadr expres)))
    (intform (cadr expres)))
   ((and (setq w (m2 (cadr expres) ratrootform nil))	;e*(a*x+b) / (c*x+d)
	 (denomfind (caddr expres)))			;expon is ratnum
    (cond ((setq w (prog2
		       (setq *powerl* t)
		       (ratroot exp var (cadr expres) w))) w)
	  (t (inte exp var))))
   ((not (integerp1 (caddr expres)))	;2*exponent not integer
    (cond ((m2 exp chebyform nil)
	   (chebyf exp var))
	  (t (intform (cadr expres)))))
   ((setq w (m2 (cadr expres) d nil))			;sqrt(c*x^2+b*x+a)
    (inte exp var))
   ((m2 exp chebyform nil)
    (chebyf exp var))
   ((not (m2 (setq w ($expand (cadr expres)))
	     (cadr expres) nil))
    (prog2 (setq exp (maxima-substitute w (cadr expres) exp))
	   (intform (simplify (list '(mexpt) w (caddr expres))))))
   ((setq w (rationalizer (cadr expres)))
    (prog2 (setq exp (let (($radexpand '$all))
			  (maxima-substitute w (cadr expres) exp)))
	   (intform (let (($radexpand '$all))
			 (simplify (list '(mexpt) w (caddr expres)))))))))
 
(defun separc (ex)
  (cond ((arcfuncp ex) (setq arcpart ex coef 1))
	((eq (caar ex) 'mtimes)
	 (arclist (cdr ex))
	 (setq coef (cond ((null (cdr coef)) (car coef))
			  (t (setq coef (cons (car ex) coef))))))))
(defun arclist (list)
  (cond ((null list) nil)
	((and (arcfuncp (car list)) (null arcpart))
	 (setq arcpart (car list)) (arclist (cdr list)))
	(t (setq coef (cons (car list) coef))
	   (arclist (cdr list)))))

(defun arcfuncp (ex)
  (and (not (atom ex))
       (or (arcp (caar ex))
	   (eq (caar ex) '%log)	     ; Experimentally treat logs also.
	   (and (eq (caar ex) 'mexpt)
		(integerp2 (caddr ex))
		(greaterp (integerp2 (caddr ex)) 0)
		(arcfuncp (cadr ex))))))

(defun integrator (exp var)
  (prog (y arg *powerl* const b w c d e ratrootform
	 chebyform arcpart coef integrand)
     (if (freevar exp) (return (mul2* exp var)))
     (setq w (partition exp var 1))
     (setq const (car w))
     (setq exp (cdr w))
     #+nil
     (progn
       (format t "w = ~A~%" w)
       (format t "const = ~A~%" const)
       (format t "exp = ~A~%" exp))
     (cond ((mplusp exp)
	    (return (mul2* const (integrate1 (cdr exp)))))
	   ((and (not (atom exp))
		 (eq (caar exp) '$atan2))
	    (return (mul2* const (integrator
				  (simplifya (list '(%atan) (div (cadr exp) (caddr exp))) t)
				  var))))
	   ((and (not (atom exp))
		 (eq (caar exp) '%sum))
	    (return (mul2* const (intsum exp var)))))
     (cond ((setq y (diffdiv exp var))
	    (return (mul2* const y))))
     (setq y (cond ((eq (caar exp) 'mtimes)
		    (cdr exp))
		   (t
		    (list exp))))
     #+nil
     (format t "y = ~S~%" y)
     ;; Pattern to match b*x + a
     (setq c '((mplus)
	       ((coeffpt) (b freevar) (x varp))
	       ((coeffpt) (a freevar))))
     ;; Pattern to match ?
     (setq ratrootform '((mtimes)
			 ((coefftt) (e freevar))
			 ((mplus)
			  ((coeffpt) (a freevar) (var varp))
			  ((coeffpt) (b freevar)))
			 ((mexpt)
			  ((mplus)
			   ((coeffpt) (c freevar) (var varp))
			   ((coeffpt) (d freevar)))
			  -1)))
     (setq chebyform '((mtimes)
		       ((mexpt) (var varp) (r1 numberp))
		       ((mexpt)
			((mplus)
			 ((mtimes)
			  ((coefftt) (c2 freevar))
			  ((mexpt) (var varp) (q free1)))
			 ((coeffpp) (c1 freevar)))
			(r2 numberp))
		       ((coefftt) (a freevar))))
     (setq d '((mplus)
	       ((coeffpt) (c freevar) ((mexpt) (x varp) 2))
	       ((coeffpt) (b freevar) (x varp))
	       ((coeffpt) (a freevar))))
     (setq e '((mtimes)
	       ((mplus)
		((coeffpt) (a freevar) (var varp))
		((coeffpt) (b freevar)))
	       ((mplus)
		((coeffpt) (c freevar) (var varp))
		((coeffpt) (d freevar)))))
     loop
     (cond ((rat8 (car y))
	    (go skip))
	   ((setq w (intform (car y)))
	    (return (mul2* const w)))
	   (t
	    (go special)))
     skip
     (setq y (cdr y))
     (cond ((null y)
	    (return (mul2* const (cond ((setq y (powerlist exp var)) y)
				       (t (ratint exp var)))))))
     (go loop)
     special
     (separc exp)	      ;SEPARC SETQS ARCPART AND COEF SUCH THAT
					;COEF*ARCEXP=EXP WHERE ARCEXP IS OF THE FORM
					;ARCFUNC^N AND COEF IS ITS ALGEBRAIC COEFFICIENT
     #+nil
     (progn
       (format t "arcpart = ~A~%" arcpart)
       (format t "coef = ~A~%" coef))
     (cond ((and (not (null arcpart))
		 (do  ((stacklist stack (cdr stacklist)))
		      ((null stacklist) t)
		   (cond ((alike1 (car stacklist) coef)
			  (return nil))))
		 (not (isinop (setq w ((lambda (stack)
					 (integrator coef var))
				       (cons coef stack)))
			      '%integrate))
		 (setq integrand (mul2 w (sdiff arcpart var)))
		 (do ((stacklist stack (cdr stacklist)))
		     ((null stacklist) t)
		   (cond ((alike1 (car stacklist) integrand)
			  (return nil))))
		 (not (isinop
		       (setq y
			     ((lambda (stack integ)
				(integrator integ var))
			      (cons integrand stack)
			      integrand))
		       '%integrate)))
	    (return (add2* (list '(mtimes) const w arcpart)
			   (list '(mtimes) -1 const y))))
	   (t (return
		(mul2 const
		      (cond ((setq y (scep exp var))
			     (cond ((cddr y)
				    (integrator ($trigreduce exp) var))
				   (t (sce-int (car y) (cadr y) var))))
			    ((not (alike1 exp (setq y ($expand exp))))
			     (integrator y var))
			    ((and (not *powerl*)
				  (setq y (powerlist exp var)))
			     y)
			    ((setq y (rischint exp var)) y)
			    (t (list '(%integrate) exp var)))))))))
 
(defun rat8 (ex)
  (cond ((or (alike1 ex var) (freevar ex))
	 t)
	((memq (caar ex) '(mplus mtimes))
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat8 (car u)))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 nil)
	((integerp (caddr ex))
	 (rat8 (cadr ex)))))
	 
(defun optrig (x) (memq x '(%sin %cos %sec %tan %csc %cot)))
	 
;;after finding a non-integrable summand usually better to pass rest to risch
(defun integrate1 (exp)
  (do ((terms exp (cdr terms)) (ans))
      ((null terms) (addn ans nil))
    (let ($liflag)					;don't gen li's for
      (push (integrator (car terms) var) ans))		;parts of integrand
    (when (and (not (free (car ans) '%integrate)) (cdr terms))
	  (return (addn (cons (rischint (cons '(mplus) terms) var) (cdr ans))
			nil)))))

;;(DEFUN ABSSUBST (EXP)
;; (COND ((ATOM EXP) EXP)
;;       ((EQ (CAAR EXP) 'MABS) (CADR EXP))
;;       (T (CONS (CAR EXP) (MAPCAR #'ABSSUBST (CDR EXP))))))

(defun scep (expr var &aux trigl exp)	; Product of SIN, COS, EXP
  (and (mtimesp expr)			;	of linear args.
       (loop for fac in (cdr expr) do
	     (cond ((atom fac) (return nil))
		   ((trig1 (car fac))
		    (if (linearp (cadr fac) var) (push fac trigl)
			(return nil)))
		   ((and (mexptp fac)
			 (eq (cadr fac) '$%e)
			 (linearp (caddr fac) var))
		    ;; should be only one exponential factor
		    (setq exp fac))
		   (t (return nil)))
	     finally (return (cons exp trigl)))))

;; Integrates exponential * sin or cos, all with linear args.
(defun sce-int (exp s-c var)		; EXP is non-trivial
  (let ((e-coef (car (islinear (caddr exp) var)))
	(sc-coef (car (islinear (cadr s-c) var)))
	(sc-arg (cadr s-c)))
       (mul (div exp (add (power e-coef 2) (power sc-coef 2)))
	    (add (mul e-coef s-c)
		 (if (eq (caar s-c) '%sin)
		     (mul* (neg sc-coef) `((%cos) ,sc-arg))
		     (mul* sc-coef `((%sin) ,sc-arg)))))))

(defun checkderiv (expr)
  (checkderiv1 (cadr expr) (cddr expr) () ))

;; CHECKDERIV1 gets called on the expression being differentiated,
;; an alternating list of variables being differentiated with
;; respect to and powers thereof, and a reversed list of the latter
;; that have already been examined.  It returns either the antiderivative
;; or (), saying this derivative isn't wrt the variable of integration.

(defun checkderiv1 (expr wrt old-wrt)
  (cond ((alike1 (car wrt) var)
	 (if (equal (cadr wrt) 1)	;Power = 1?
	     (if (null (cddr wrt))	;single or partial
		 (if (null old-wrt)
		     expr		;single
		     `((%derivative), expr ;partial in old-wrt
		       ,.(nreverse old-wrt)))
		 `((%derivative) ,expr	;Partial, return rest
		   ,.(nreverse old-wrt)
		   ,@(cddr wrt)))
	     `((%derivative) ,expr	;Higher order, reduce order
	       ,.(nreverse old-wrt)
	       ,(car wrt) ,(add2* (cadr wrt) -1)
	       ,@ (cddr wrt))))
	((null (cddr wrt)) () )		;Say it doesn't apply here
	(t (checkderiv1 expr (cddr wrt)	;Else we check later terms
			(list* (cadr wrt) (car wrt) old-wrt)))))

(defun elem (a) 
  (cond ((freevar a) t)
	((atom a) nil)
	((m2 a expres nil) t)
	(t (eval (cons 'and (mapcar #'elem (cdr a)))))))

(defun freevar (a) 
  (cond ((atom a) (not (eq a var)))
	((alike1 a var) nil)
	((and (not (atom (car a)))
	      (memq 'array (cdar a)))
	 (cond ((freevar (cdr a)) t)
	       (t (merror "Variable of integration appeared in subscript"))))
	(t (and (freevar (car a)) (freevar (cdr a))))))

(defun varp (x)
  (alike1 x var))

(defun integrallookups (exp) 
  (cond ((eq (caar exp) '%log)
	 (maxima-substitute (cadr exp)
			    'x
			    '((mplus)
			      ((mtimes) x ((%log) x))
			      ((mtimes) -1 x))))
	((eq (caar exp) 'mplus)
	 (muln (list '((rat simp) 1 2) exp exp) nil))
	((eq (caar exp) 'mexpt)
	 (cond ((freevar (cadr exp))
		(simplifya (maxima-substitute exp
					      'a
					      (maxima-substitute (cadr exp)
								 'b
								 '((mtimes)
								   a
								   ((mexpt)
								    ((%log)
								     b)
								    -1))))
			   nil))
	       ((or (equal (caddr exp) -1)
		    (and (not (mnump (caddr exp)))
			 (freeof '$%i (caddr exp))
			 (eq (asksign (power (add2 (caddr exp) 1) 2)) '$zero)))
		(maxima-substitute (cadr exp) 'x (logmabs 'x)))
	       (t (maxima-substitute (add2* (caddr exp) 1)
				     'n
				     (maxima-substitute (cadr exp)
							'x
							'((mtimes)
							  ((mexpt) n -1)
							  ((mexpt) x n)))))))
	(t (maxima-substitute (cadr exp)
			      'x
			      (cdr (sassq (caar exp)
					  '((%sin (mtimes) -1 ((%cos) x))
					    (%cos (%sin) x)
					    (%tan (%log)
					     ((%sec) x))
					    (%sec (%log) ((mplus) ((%sec) x) ((%tan) x)))
					    (%cot (%log)
					     ((%sin) x))
					    (%sinh (%cosh) x)
					    (%cosh (%sinh) x)
					    (%tanh (%log)
					     ((%cosh) x))
					    (%coth (%log) ((%sinh) x))
					    (%sech (%atan)
					     ((%sinh) x))
					    (%csch
					     (%log) ((%tanh) ((mtimes) ((rat simp) 1 2) x)))
					    (%csc (mtimes)
					     -1
					     ((%log)
					      ((mplus)
					       ((%csc) x)
					       ((%cot)
						x)))))
					  'nill))))))

(defun true (ignor) ignor t) 

(defun rat10 (ex) 
  (cond ((freevar ex) t)
	((alike1 ex var) nil)
	((eq (caar ex) 'mexpt)
	 (if (alike1 (cadr ex) var)
	     (if (integerp2 (caddr ex))
		 (setq powerlist (cons (caddr ex) powerlist)))
	     (and (rat10 (cadr ex)) (rat10 (caddr ex)))))
	((memq (caar ex) '(mplus mtimes))
	 (do ((u (cdr ex) (cdr u))) ((null u) t)
	     (if (not (rat10 (car u))) (return nil))))
	(t
	 (let ((examine (margs ex)))
	   (if (atom (first examine))
	       (do* ((element examine (rest element))
		     (result (rat10 (first examine))
			     (and result (rat10 (first element)))))
		   ((or (null result) (null element)) result))
	     (rat10 (first examine)))))))

(defun listgcd (powerlist)
  (prog (p)
     (setq p (car powerlist))
   loop
     (setq powerlist (cdr powerlist))
     (if (equal p 1) (return nil))
     (if (null powerlist) (return p))
     (setq p (gcd p (car powerlist)))
     (go loop)))
	 
(defun integrate5 (ex var)
  (if (rat8 ex)
      (ratint ex var)
      (integrator ex var)))
	 
(defun integerp2 (x)
  (let (u)
    (cond ((not (numberp x)) nil)
	  ((not (floatp x)) x)
	  ((prog2 (setq u (maxima-rationalize x))
	       (equal (cdr u) 1)) (car u)))))

(defun rat3 (ex ind) 
  (cond ((freevar ex) t)
	((atom ex) ind)
	((memq (caar ex) '(mtimes mplus))
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat3 (car u) ind))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 (rat3 (car (margs ex)) t))
	((freevar (cadr ex))
	 (rat3 (caddr ex) t))
	((integerp (caddr ex))
	 (rat3 (cadr ex) ind))
	((and (m2 (cadr ex) ratroot nil)
	      (denomfind (caddr ex)))
	 (setq rootlist (cons (denomfind (caddr ex)) rootlist)))
	(t (rat3 (cadr ex) nil))))

(defun subst4 (ex) 
  (cond ((freevar ex) ex)
	((atom ex) a)
	((not (eq (caar ex) 'mexpt))
	 (mapcar #'(lambda (u) (subst4 u)) ex))
	((m2 (cadr ex) ratroot nil)
	 (list (car ex) b (integerp2 (timesk k (caddr ex)))))
	(t (list (car ex) (subst4 (cadr ex)) (subst4 (caddr ex))))))

(defun findingk (list)
  (do ((kk 1) (l list (cdr l)))
      ((null l) kk)
    (setq kk (lcm kk (car l)))))

(defun denomfind (x) 
  (cond ((ratnump x) (caddr x))
	((not (numberp x)) nil)
	((not (floatp x)) 1)
	(t (cdr (maxima-rationalize x)))))

;; EXP = f(t,u) where f is some function with, say, VAR = t,
;; u^k = RATROOT = e*(a*t+b)/(c*t+d), where the smallest possible k
;; is calculated below.
;; As always, W is an alist which associates to the coefficients
;; a, b... (and to VAR) their values.
(defun ratroot (exp var ratroot w) 
  (prog (rootlist k y w1) 
     (cond ((setq y (chebyf exp var)) (return y)))
     (cond ((not (rat3 exp t)) (return nil)))
     (setq k (findingk rootlist))
     (setq w1 (cons (cons 'k k) w))
     (setq y
	   (subst41
	    exp
	    (simplify
	     (subliss w1
		      '((mquotient)
			((mplus) ((mtimes) b e)
			 ((mtimes) -1 d ((mexpt) var k)))
			((mplus) ((mtimes) c ((mexpt) var k))
			 ((mtimes) -1 e a)))))
	    var))
     (setq y
	   (integrator
	    (simplify
	     (list '(mtimes)
		   y
		   (subliss
		    w1 '((mquotient)
			 ((mtimes)
			  e ((mplus)
			     ((mtimes) a d k
			      ((mexpt) var ((mplus) -1 k)))
			     ((mtimes)
			      -1
			      ((mtimes) b c k
			       ((mexpt) var ((mplus) -1 k))))))
			 ((mexpt) ((mplus)
				   ((mtimes) c ((mexpt) var k))
				   ((mtimes) -1 a e))
			  2)))))
	    var))
     (return (substint (simplify (list '(mexpt)
				       ratroot
				       (list '(mexpt) k -1)))
		       var
		       y))))

(defun subst41 (exp a b)
  (subst4 exp))

;; exp = a*t^r1*(c1+c2*t^q)^r2, where var = t.
(defun chebyf (exp var) 
  (prog (r1 r2 d1 d2 n1 n2 w q) 
     (cond ((not (setq w
		       (m2 exp
			   '((mtimes)
			     ((mexpt) (var varp) (r1 numberp))
			     ((mexpt)
			      ((mplus)
			       ((mtimes)
				((coefftt) (c2 freevar))
				((mexpt) (var varp) (q free1)))
			       ((coeffpp) (c1 freevar)))
			      (r2 numberp))
			     ((coefftt) (a freevar)))
			   nil)))
	    (return nil)))
     (when (zerop1 (cdr (sassq 'c1 w #'nill)))
       (return
	 (mul*
	  ;; This factor is locally constant as long as t and
	  ;; c2*t^q avoid log's branch cut.
	  (subliss w '((mtimes) a ((mexpt) var ((mtimes) -1 q r2))
		       ((mexpt) ((mtimes) c2 ((mexpt) var q)) r2)))
	  (integrator
	   (subliss w '((mexpt) var ((mplus) r1 ((mtimes) q r2)))) var))))
     (setq q (cdr (sassq 'q w 'nill)))
     (setq w
	   (list* (cons 'a (div* (cdr (sassq 'a w 'nill)) q))
		  (cons
		   'r1
		   (div* (addn (list 1 (neg (simplify q)) (cdr (sassq 'r1 w 'nill))) nil)
			 q))
		  w))
     (setq r1 (cdr (sassq 'r1 w 'nill))
	   r2 (cdr (sassq 'r2 w 'nill)))
     (cond
       ((not (and (setq d1 (denomfind r1))
		  (setq d2 (denomfind r2))
		  (setq n1 (integerp2 (timesk r1 d1)))
		  (setq n2 (integerp2 (timesk r2 d2)))
		  (setq w (list* (cons 'd1 d1) (cons 'd2 d2)
				 (cons 'n1 n1) (cons 'n2 n2)
				 w))))
	(return nil))
       ((and (integerp2 r1) (greaterp r1 0))
	(return
	  (substint
	   (subliss w '((mplus) c1 ((mtimes) c2 ((mexpt) var q))))
	   var
	   (integrator
	    (expands (list (subliss w
				    '((mtimes)
				      a
				      ((mexpt) var r2)
				      ((mexpt)
				       c2
				       ((mtimes)
					-1
					((mplus) r1 1))))))
		     (cdr (expandexpt (subliss w
					       '((mplus)
						 var
						 ((mtimes) -1 c1)))
				      r1)))
	    var))))
       ((integerp2 r2)
	(return
	  (substint (subliss w '((mexpt) var ((mquotient) q d1)))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 d1 a
						 ((mexpt)
						  var
						  ((mplus)
						   n1 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mtimes)
						    c2
						    ((mexpt)
						     var d1))
						   c1)
						  r2))))
			    var))))
       ((and (integerp2 r1) (lessp r1 0))
	(return
	  (substint (subliss w
			     '((mexpt)
			       ((mplus)
				c1
				((mtimes) c2 ((mexpt) var q)))
			       ((mquotient) 1 d2)))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 a d2
						 ((mexpt)
						  c2
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 1)))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d2 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d2)
						   ((mtimes) -1 c1))
						  r1))))
			    var))))
       ((integerp2 (add2* r1 r2))
	(return
	  (substint (subliss w
			     '((mexpt)
			       ((mquotient)
				((mplus)
				 c1
				 ((mtimes) c2 ((mexpt) var q)))
				((mexpt) var q))
			       ((mquotient) 1 d1)))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 -1 a d1
						 ((mexpt)
						  c1
						  ((mplus)
						   r1 r2 1))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d1)
						   ((mtimes)
						    -1 c2))
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 r2
						    2))))))
			    var))))
       (t (return (list '(%integrate) exp var))))))

(defun greaterratp (x1 x2) 
  (cond ((and (numberp x1) (numberp x2))
	 (greaterp x1 x2))
	((ratnump x1)
	 (greaterratp (quotient (float (cadr x1))
				(caddr x1))
		      x2))
	((ratnump x2)
	 (greaterratp x1
		      (quotient (float (cadr x2))
				(caddr x2))))))

(defun trig1 (x)
  (memq (car x) '(%sin %cos)))

(defun supertrig (exp)
  (declare (special *notsame*))
  (cond ((freevar exp) t)
	((atom exp) nil)
	((memq (caar exp) '(mplus mtimes))
	 (and (supertrig (cadr exp))
	      (or (null (cddr exp))
		  (supertrig (cons (car exp)
				   (cddr exp))))))
	((eq (caar exp) 'mexpt)
	 (and (supertrig (cadr exp))
	      (supertrig (caddr exp))))
	((eq (caar exp) '%log)
	 (supertrig (cadr exp)))
	((memq (caar exp)
	       '(%sin %cos %tan %sec %cot %csc))
	 (cond ((m2 (cadr exp) trigarg nil) t)
	       ((m2 (cadr exp)
		    '((mplus)
		      ((coeffpt) (b freevar) (x varp))
		      ((coeffpt) (a freevar)))
		    nil)
		(and (setq *notsame* t) nil))
	       (t (supertrig (cadr exp)))))
	(t (supertrig (cadr exp)))))
	 
(defun subst2s (ex pat)
  (cond ((null ex) nil)
	((m2 ex pat nil) var)
	((atom ex) ex)
	(t (cons (subst2s (car ex) pat)
		 (subst2s (cdr ex) pat)))))

(defun monstertrig (exp var trigarg)
  (if (not (atom trigarg))
      (return-from monstertrig (rischint exp var)))
  (prog (*notsame* w a b y d) 
	(cond
	 ((supertrig exp) (go a))
	 ((null *notsame*) (return nil))
	 ((not (setq y (m2 exp
			   '((mtimes)
			     ((coefftt) (a freevar))
			     (((b trig1))
			      ((mtimes)
			       (x varp)
			       ((coefftt) (m freevar))))
			     (((d trig1))
			      ((mtimes)
			       (x varp)
			       ((coefftt) (n freevar)))))
			   nil)))
	  (go b))
	 ((not (and (memq (car (setq b
				     (cdr (sassq 'b
						 y
						 'nill))))
			  '(%sin %cos))
		    (memq (car (setq d
				     (cdr (sassq 'd
						 y
						 'nill))))
			  '(%sin %cos))))
	  (return nil))
	 ((and (eq (car b) '%sin) (eq (car d) '%sin))
	  (return (subvar (subliss y
				   '((mtimes)
				     a
				     ((mplus)
				      ((mquotient)
				       ((%sin)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mtimes)
				       -1
				       ((mquotient)
					((%sin)
					 ((mtimes) ((mplus) m n) x))
					((mtimes)
					 2
					 ((mplus) m n))))))))))
	 ((and (eq (car b) '%cos) (eq (car d) '%cos))
	  (return (subvar (subliss y
				   '((mtimes)
				     a
				     ((mplus)
				      ((mquotient)
				       ((%sin)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mquotient)
				       ((%sin)
					((mtimes) ((mplus) m n) x))
				       ((mtimes)
					2
					((mplus) m n)))))))))
	 ((or (and (eq (car b) '%cos)
		   (setq w (cdr (sassq 'm y 'nill)))
		   (rplacd (sassq 'm y 'nill)
			   (cdr (sassq 'n y 'nill)))
		   (rplacd (sassq 'n y 'nill) w))
	      t)
	  (return (subvar (subliss y
				   '((mtimes)
				     -1
				     a
				     ((mplus)
				      ((mquotient)
				       ((%cos)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mquotient)
				       ((%cos)
					((mtimes) ((mplus) m n) x))
				       ((mtimes)
					2
					((mplus) m n))))))))))
   b    (cond ((not (setq y (prog2 (setq trigarg var)
				   (m2 exp
				       '((mtimes)
					 ((coefftt) (a freevar))
					 (((b trig1))
					  ((mtimes)
					   (x varp)
					   ((coefftt) (n integerp2))))
					 ((coefftt) (c supertrig)))
				       nil))))
	       (return nil)))
	(return
	 (integrator
	  ($expand
	   (list '(mtimes)
		 (sch-replace y 'a)
		 (sch-replace y 'c)
		 (cond ((eq (car (setq b (sch-replace y 'b))) '%cos)
			(maxima-substitute var
				    'x
				    (supercosnx (sch-replace y 'n))))
		       (t (maxima-substitute var
				      'x
				      (supersinx (sch-replace y 'n)))))))
	  var))
   a    (setq w (subst2s exp trigarg))
	(setq b (cdr (sassq 'b
			    (m2 trigarg
				'((mplus)
				  ((coeffpt) (b freevar) (x varp))
				  ((coeffpt) (a freevar)))
				nil)
			    'nill)))
	(setq a (substint trigarg
			    var
			    (trigint (div* w b) var)))
   (cond((m2 a '((mtimes)((coefftt)(d freevar))
		 ((%integrate ) (b true) (c true)))nil) 
	 (return(list '(%integrate) exp var))))
   (return a)))

(defun trig2 (x)
  (memq (car x) '(%sin %cos %tan %cot %sec %csc)))

(defun supersinx (n)
  ((lambda (i) 
     ($expand (list '(mtimes)
		    i
		    (sinnx (timesk i n)))))
   (cond ((lessp n 0) -1) (t 1))))
	 

(defun supercosnx (n)
  ((lambda (i)
     ($expand (cosnx (timesk i n))))
   (cond ((lessp n 0) -1) (t 1))))
	 

(defun sinnx (n)
  (cond ((equal n 1) '((%sin) x))
	(t (list '(mplus)
		 (list '(mtimes)
		       '((%sin) x)
		       (cosnx (sub1 n)))
		 (list '(mtimes)
		       '((%cos) x)
		       (sinnx (sub1 n)))))))
	 

(defun cosnx (n)
  (cond ((equal n 1) '((%cos) x))
	(t (list '(mplus)
		 (list '(mtimes)
		       '((%cos) x)
		       (cosnx (sub1 n)))
		 (list '(mtimes)
		       -1
		       '((%sin) x)
		       (sinnx (sub1 n)))))))
	 

(defun poseven (x)
  (and (even x) (greaterp x -1)))

(defun trigfree (x) 
  (cond ((atom x)
	 (not (memq x '(sin* cos* sec* tan*))))
	(t (and (trigfree (car x))
		(trigfree (cdr x))))))

(defun rat1 (exp)
  (prog (*b1* *notsame*) 
     (declare (special *yy* *b1* *notsame*))
     (cond ((and (numberp exp) (zerop exp))
	    (return nil)))
     (setq *b1* (subst b 'b '((mexpt) b (n even))))
     (return (prog2 (setq *yy* (rats exp))
		 (cond ((not *notsame*) *yy*))))))

(defun rats (exp)
  (prog (y) 
     (declare (special *notsame* *b1*))
     (return
       (cond ((eq exp a) 'x)
	     ((atom exp)
	      (cond ((memq exp '(sin* cos* sec* tan*))
		     (setq *notsame* t))
		    (t exp)))
	     ((setq y (m2 exp *b1* nil))
	      (f3 y))
	     (t (cons (car exp)
		      (mapcar 
		       (function (lambda (g) (rats g)))
		       (cdr exp))))))))
 

(defun f3 (y)
  (maxima-substitute c
		     'c
		     (maxima-substitute (quotient (cdr (sassq 'n y nil)) 2)
					'n
					'((mexpt)
					  ((mplus)
					   1
					   ((mtimes)
					    c
					    ((mexpt) x 2)))
					  n))))

(defun odd1 (n)
  (declare (special *yz*))
  (cond ((not (numberp n)) nil)
	((not (equal (remainder n 2) 0))
	 (setq *yz*
	       (maxima-substitute c
				  'c
				  (list '(mexpt)
					'((mplus)
					  1
					  ((mtimes)
					   c
					   ((mexpt) x 2)))
					(quotient (sub1 n) 2)))))
	(t nil)))

(defun subvar (x)
  (maxima-substitute var 'x x))

(defun subvardlg (x) 
  (mapcar #'(lambda (m)
	      (cons (maxima-substitute var 'x (car m))
		    (cdr m)))
	  x))

;; This appears to be the implementation of Method 6, pp.82 in Moses'
;; thesis.

(defun trigint (exp var) 
  (prog (y repl y1 y2 *yy* z m n c *yz* a b )
     (declare (special *yy* *yz*))
     ;; Transform trig(x) into trig* (for simplicity?)  Convert cot to
     ;; tan and csc to sin.
     (setq y2
	   (subliss (subvardlg '((((%sin) x) . sin*)
				 (((%cos) x) . cos*)
				 (((%tan) x) . tan*)
				 (((%cot) x) . ((mexpt) tan* -1))
				 (((%sec) x) . sec*)
				 (((%csc) x) . ((mexpt) sin* -1))))
		    (simplifya exp nil)))
     #+nil
     (progn
       (format t "y2 = ~%")
       (maxima-display y2))
     ;; Now transform tan to sin/cos and sec to 1/cos.
     (setq y1 (setq y (simplify (subliss '((tan* . ((mtimes) sin*
						    ((mexpt) cos* -1)))
					   (sec* . ((mexpt) cos* -1)))
					 y2))))
     #+nil
     (progn 
       (format t "y  =~%")
       (maxima-display y))
     (cond ((null (setq z (m2 y
			      '((mtimes)
				((coefftt) (b trigfree))
				((mexpt) sin* (m poseven))
				((mexpt) cos* (n poseven)))
			      nil)))
	    ;; Go if y is not of the form sin^m*cos^n for positive
	    ;; even m and n.
	    #+nil
	    (format t "Not of form sin^m*cos^n, for m, n non-negative and even.~%")
	    (go l1)))

     ;; Case III:
     ;;
     ;; Handle the case of sin^m*cos^n, m, n both non-negative and
     ;; even.
     
     #+nil
     (format t "Case III~%")
     (setq m (cdr (sassq 'm z 'nill)))
     (setq n (cdr (sassq 'n z 'nill)))
     (setq a (integerp2 (times 0.5
			       (cond ((lessp m n) 1) (t -1))
			       (plus n (times -1 m)))))
     (setq z (cons (cons 'a a) z))
     #+nil
     (progn
       (format t "m, n = ~A ~A~%" m n)
       (format t "a = ~A~%" a)
       (format t "z = ~A~%" z))
     ;; integrate(sin(y)^m*cos(y)^n,y) is transformed to the following form:
     ;;
     ;; m < n:
     ;;   integrate((sin(2*y)/2)^n*(1/2+1/2*cos(2*y)^((n-m)/2),y)
     ;;
     ;; m >= n:
     ;;
     ;;   integrate((sin(2*y)/2)^n*(1/2-1/2*cos(2*y)^((m-n)/2),y)
     (return
       (simplify
	(list
	 '(mtimes)
	 (cdr (sassq 'b z 'nill))
	 '((rat simp) 1 2)
	 (substint
	  (list '(mtimes) 2 var)
	  'x
	  (integrator (simplify (cond ((lessp m n)
				       (subliss z
						'((mtimes)
						  ((mexpt)
						   ((mtimes)
						    ((rat simp) 1 2)
						    ((%sin) x))
						   m)
						  ((mexpt)
						   ((mplus)
						    ((rat simp) 1 2)
						    ((mtimes)
						     ((rat simp) 1 2)
						     ((%cos) x)))
						   a))))
				      (t (subliss z
						  '((mtimes)
						    ((mexpt)
						     ((mtimes)
						      ((rat simp) 1 2)
						      ((%sin) x))
						     n)
						    ((mexpt)
						     ((mplus)
						      ((rat simp) 1 2)
						      ((mtimes)
						       ((rat simp)
							-1
							2)
						       ((%cos) x)))
						     a))))))
		      'x)))))
     l1
     ;; I think this is case IV, working on the expression in terms of
     ;; sin and cos.

     ;; Elem(x) means constants, x, trig functions of x, log and
     ;; inverse trig functions of x, and which are closed under
     ;; addition, multiplication, exponentiation, and substitution.
     ;;
     ;; Elem(f(x)) is the same as Elem(x), but f(x) replaces x in the
     ;; definition.

     #+nil
     (format t "Case IV~%")
     (setq c -1)
     (setq a 'sin*)
     (setq b 'cos*)
     (cond ((and (m2 y
		     '((coeffpt) (c rat1) ((mexpt) cos* (n odd1)))
		     nil)
		 (setq repl (list '(%sin) var)))
	    ;; The case cos^(2*n+1)*Elem(cos^2,sin).  Use the
	    ;; substitution z = sin.
	    #+nil
	    (format t "Case cos^(2*n+1)*Elem(cos^2,sin)~%")
	    (go getout)))
     (setq a b)
     (setq b 'sin*)
     (cond ((and (m2 y
		     '((coeffpt) (c rat1) ((mexpt) sin* (n odd1)))
		     nil)
		 (setq repl (list '(%cos) var)))
	    ;; The case sin^(2*n+1)*Elem(sin^2,cos).  Use the
	    ;; substitution z = cos.
	    #+nil
	    (format t "Case sin^(2*n+1)*Elem(sin^2,cos)~%")
	    (go get3)))
     ;; Case V
     ;;
     ;; Transform sin and cos to tan and sec to see if the integral is
     ;; of the form Elem(tan, sec^2).  If so, use the substitution z =
     ;; tan.
     #+nil
     (format t "Case V~%")
     (setq y
	   (simplify (subliss '((sin* (mtimes) tan* ((mexpt) sec* -1))
				(cos* (mexpt) sec* -1))
			      y2)))
     (setq c 1)
     (setq a 'tan*)
     (setq b 'sec*)
     (cond ((and (rat1 y) (setq repl (list '(%tan) var)))
	    (go get1)))
     (setq a b)
     (setq b 'tan*)
     (cond ((and (m2 y
		     '((coeffpt) (c rat1) ((mexpt) tan* (n odd1)))
		     nil)
		 (setq repl (list '(%sec) var)))
	    (go getout)))
     (cond((not (alike1 (setq repl ($expand exp))
			exp))
	   (return(integrator repl var))))
     (setq y
	   (simplify (subliss '((sin* (mtimes)
				 2
				 x
				 ((mexpt)
				  ((mplus) 1 ((mexpt) x 2))
				  -1))
				(cos* (mtimes)
				 ((mplus)
				  1
				  ((mtimes) -1 ((mexpt) x 2)))
				 ((mexpt)
				  ((mplus) 1 ((mexpt) x 2))
				  -1)))
			      y1)))
     (setq y (list '(mtimes)
		   y
		   '((mtimes)
		     2
		     ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))))
     (setq repl (subvar '((mquotient)
			  ((%sin) x)
			  ((mplus) 1 ((%cos) x)))))
     (go get2)
     get3 (setq y (list '(mtimes) -1 *yy* *yz*))
     (go get2)
     get1 (setq y (list '(mtimes)
			'((mexpt) ((mplus) 1 ((mexpt) x 2)) -1)
			*yy*))
     (go get2)
     getout
     (setq y (list '(mtimes) *yy* *yz*))
     get2 (setq y (simplify y))
     (return (substint repl 'x (integrator y 'x)))))

(defmfun sinint (exp var)
  (find-function 'ratint)	   ; Make sure that RATINT is in core.
  (cond ((mnump var) (merror "Attempt to integrate wrt a number: ~:M" var))
	(($ratp var) (sinint exp (ratdisrep var)))
	(($ratp exp) (sinint (ratdisrep exp) var))
	((mxorlistp exp)
	 (cons (car exp)
	       (mapcar #'(lambda (y) (sinint y var)) (cdr exp))))
	((mequalp exp)
	 (list (car exp) (sinint (cadr exp) var)
	       (add2 (sinint (caddr exp) var)
		     (concat '$integrationconstant
			     (setq $integration_constant_counter 
				   (f1+ $integration_constant_counter))))))
	((and (atom var)
	      (isinop exp var))
	 (list '(%integrate) exp var))
	((let ((ans (simplify
		     (let ($opsubst varlist genvar stack)
		       (integrator exp var)))))
	   (if (sum-of-intsp ans)
	       (list '(%integrate) exp var)
	       ans)))))

(defun sum-of-intsp (ans)
  (cond ((atom ans) (not (eq ans var)))
	((mplusp ans) (andmapc #'sum-of-intsp (cdr ans)))
	((eq (caar ans) '%integrate) t)
	((mtimesp ans)
	 (do ((facs (cdr ans) (cdr facs))
	      (ints))
	     ((null facs)
	      (< (length ints) 2))
	   (unless (freeof var (car facs))
	     (if (sum-of-intsp (car facs))
		 (push (car facs) ints)
		 (return nil)))))
	((freeof var ans) t)
	(t nil)))

(defun intsum (form var)
  (prog (exp idx ll ul pair val)
     (setq exp (cadr form)
	   idx (caddr form)
	   ll (cadddr form)
	   ul (car (cddddr form)))
     (if (or (not (atom var))
	     (not (free idx var))
	     (not (free ll var))
	     (not (free ul var)))
	 (return (list '(%integrate) form var)))
     (setq pair (partition exp var 1))
     (when (and (mexptp (cdr pair))
		(eq (caddr pair) var))
       (setq val (maxima-substitute ll idx (cadddr pair)))
       (cond ((equal val -1)
	      (return (add2 (integrator (maxima-substitute ll idx exp) var)
			    (intsum1 exp idx (add2 1 ll) ul var))))
	     ((mlsp val -1)
	      (return (list '(%integrate) form var)))))
     (return (intsum1 exp idx ll ul var))))

(defun intsum1 (exp idx ll ul var)
  (assume (list '(mgeqp) idx ll))
  (if (not (eq ul '$inf))
      (assume (list '(mgeqp) ul idx)))
  (simplifya (list '(%sum) (integrator exp var) idx ll ul) t))

(defun rat8prime (c)
  (and (rat8 c)
       (or (not (mnump c))
	   (not (zerop1 c)))))

(defun finds (x) 
  (if (atom x)
      (memq x '(%log %integrate %atan))
      (or (finds (car x)) (finds (cdr x)))))

(defun ratlog (exp var form)
  (prog (a b c d y z w) 
     (setq y form)
     (setq b (cdr (sassq 'b y 'nill)))
     (setq c (cdr (sassq 'c y 'nill)))
     (setq y (integrator c var))
     (cond ((finds y) (return nil)))
     (setq d (sdiff (cdr (sassq 'a form 'nill))
		    var))

     (setq z (integrator (mul2* y d) var))
     (setq d (cdr (sassq 'a form 'nill)))
     (return (simplify (list '(mplus)
			     (list '(mtimes) y d)
			     (list '(mtimes) -1 z))))))

(defun find1 (y a) 
  (cond ((eq y a) t)
	((atom y) nil)
	(t (or (find1 (car y) a) (find1 (cdr y) a)))))

(defun matchsum (alist blist) 
  (prog (r s c d) 
     (setq s (m2 (car alist)
		 '((mtimes)
		   ((coefftt) (a freevar))
		   ((coefftt) (c true)))
		 nil))
     (setq c (cdr (sassq 'c s 'nill)))
     (cond ((not (setq r
		       (m2 (cons '(mplus) blist)
			   (list '(mplus)
				 (cons '(mtimes)
				       (cons '((coefftt) (b free1))
					     (cond ((mtimesp c)
						    (cdr c))
						   (t (list c)))))
				 '(d true))
			   nil)))
	    (return nil)))
     (setq d (simplify (list '(mtimes)
			     (subliss s 'a)
			     (list '(mexpt)
				   (subliss r 'b)
				   -1))))
     (cond ((m2 (cons '(mplus) alist)
		(timesloop d blist)
		nil)
	    (return d))
	   (t (return nil)))))
 
(defun timesloop (a b)
  (cons '(mplus) (mapcar (function (lambda (c) (mul2* a c))) b)))   

(defun simplog (a)
  (simplifya (cons '(%log) a) nil))

(defun expands (aa b) 
  (addn (mapcar (function (lambda (c) (timesloop c aa))) b) nil))

(defun powerlist (exp var) 
  (prog (y z c d powerlist b) 
     (setq y (m2 exp
		 '((mtimes)
		   ((mexpt) (var varp) (c integerp2))
		   ((coefftt) (a freevar))
		   ((coefftt) (b true)))
		 nil))
     (setq b (cdr (sassq 'b y 'nill)))
     (setq c (cdr (sassq 'c y 'nill)))
     (cond ((not (setq z (rat10 b))) (return nil)))
     (setq d (listgcd (cons (add1 c) powerlist)))
     (cond ((or (null d) (zerop d)) (return nil)))
     (return
       (substint
	(list '(mexpt) var d)
	var
	(integrate5 (simplify (list '(mtimes)
				    (power* d -1)
				    (cdr (sassq 'a
						y
						'nill))
				    (list '(mexpt)
					  var
					  (sub1 (quotient (add1 c) d)))
				    (subst10 b)))
		    var)))))

(defun diffdiv (exp var) 
  (prog (y a x v d z w r) 
     (cond
       ((and (mexptp exp)
	     (mplusp (cadr exp))
	     (integerp2 (caddr exp))
	     (lessp (caddr exp) 6)
	     (greaterp (caddr exp) 0))
	(return (integrator (expandexpt (cadr exp) (caddr exp)) var))))
     (setq exp (cond ((mtimesp exp) exp) (t (list '(mtimes) exp))))
     (setq z (cdr exp))
     a    (setq y (car z))
     (setq r (list '(mplus)
		   (cons '(coeffpt)
			 (cons '(c free1)
			       (choicesin y (cdr exp))))))
     (cond
       ((setq w (m2 (sdiff y var) r nil))
	(return (muln (list y y (power* (mul2* 2 (cdr (sassq 'c w 'nill))) -1)) nil))))
     (setq w (cond ((or (atom y) (memq (caar y) '(mplus mtimes))) y)
		   ((eq (caar y) 'mexpt)
		    (cond ((freevar (cadr y)) (caddr y))
			  ((freevar (caddr y)) (cadr y))
			  (t 0)))
		   (t (cadr y))))
     (cond
       ((setq w (cond ((and (setq x (sdiff w var))
			    (mplusp x)
			    (setq d (choicesin y (cdr exp)))
			    (setq v (car d))
			    (mplusp v)
			    (not (cdr d)))
		       (cond ((setq d (matchsum (cdr x) (cdr v)))
			      (list (cons 'c d)))
			     (t nil)))
		      (t (m2 x r nil))))
	(return (cond ((null (setq x (integrallookups y))) nil)
		      ((eq w t) x)
		      (t (mul2* x (power* (cdr (sassq 'c w 'nill)) -1)))))))
     (setq z (cdr z))
     (cond ((null z) (return nil)))
     (go a)))
 
(defun subliss (a b) 
  (prog (x y z) 
     (setq x b)
     (setq z a)
   loop
     (cond ((null z) (return x)))
     (setq y (car z))
     (setq x (maxima-substitute (cdr y) (car y) x))
     (setq z (cdr z))
     (go loop)))

(defun substint (x y expres)
  (cond ((and (not (atom expres))
	      (eq (caar expres) '%integrate))
	 (list (car expres) exp var))
	(t (substint1 (maxima-substitute x y expres)))))

(defun substint1 (exp)
  (cond ((atom exp) exp)
	((and (eq (caar exp) '%integrate)
	      (null (cdddr exp))
	      (not (symbolp (caddr exp)))
	      (not (free (caddr exp) var)))
	 (simplify (list '(%integrate)
			 (mul2 (cadr exp) (sdiff (caddr exp) var))
			 var)))
	(t (recur-apply #'substint1 exp))))
