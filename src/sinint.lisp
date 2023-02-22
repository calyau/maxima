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

(macsyma-module sinint)

(load-macsyma-macros ratmac)

(declare-top (special *checkfactors*))

(defun rootfac (q sinint-var)
  (prog (nthdq nthdq1 simproots ans n-loops)
     (setq nthdq (pgcd q (pderivative q sinint-var)))
     (setq simproots (pquotient q nthdq))
     (setq ans (list (pquotient simproots (pgcd nthdq simproots))))
     (setq n-loops 0)
   amen
     (cond
       ((= n-loops $factor_max_degree)
        (return (list q)))
       ((or (pcoefp nthdq) (pointergp sinint-var (car nthdq)))
        (return (reverse ans))))
     (setq nthdq1 (pgcd (pderivative nthdq sinint-var) nthdq))
     (push (pquotient (pgcd nthdq simproots)
		      (pgcd nthdq1 simproots))
	   ans)
     (setq nthdq nthdq1)
     (incf n-loops)
     (go amen)))

(defun aprog (q sinint-var)
  (setq q (oldcontent q))
  (let ((sinint-rootfactor
	  (rootfac (cadr q) sinint-var)))
    (setq sinint-rootfactor
	  (cons (ptimes (car q) (car sinint-rootfactor))
		(cdr sinint-rootfactor)))
    (let (sinint-pardenom)
      (do ((pd (list (car sinint-rootfactor)))
	   (rf (cdr sinint-rootfactor) (cdr rf))
	   (n 2 (1+ n)))
	  ((null rf)
	   (setq sinint-pardenom (reverse pd)))
	(push (pexpt (car rf) n)
	      pd))
      (values sinint-rootfactor sinint-pardenom))))

(defun cprog (top bottom sinint-var sinint-pardenom)
  (prog (frpart pardenomc ppdenom thebpg sinint-parnumer sinint-wholepart)
     (setq frpart (pdivide top bottom))
     (setq sinint-wholepart (car frpart))
     (setq frpart (cadr frpart))
     (if (= (length sinint-pardenom) 1)
	 (return (values (list frpart) sinint-wholepart)))
     (setq pardenomc (cdr sinint-pardenom))
     (setq ppdenom (list (car sinint-pardenom)))
   dseq
     (if (= (length pardenomc) 1)
	 (go ok))
     (setq ppdenom (cons (ptimes (car ppdenom) (car pardenomc))
			 ppdenom))
     (setq pardenomc (cdr pardenomc))
     (go dseq)
   ok
     (setq pardenomc (reverse sinint-pardenom))
   numc
     (setq thebpg (bprog (car pardenomc)
			 (car ppdenom)
			 sinint-var))
     (setq sinint-parnumer
	   (cons (cdr (ratdivide (ratti frpart (cdr thebpg) t)
				 (car pardenomc)))
		 sinint-parnumer))
     (setq frpart
	   (cdr (ratdivide (ratti frpart (car thebpg) t)
			   (car ppdenom))))
     (setq pardenomc (cdr pardenomc))
     (setq ppdenom (cdr ppdenom))
     (if (null ppdenom)
	 (return (values (cons frpart sinint-parnumer)
			 sinint-wholepart)))
     (go numc)))

(defun polyint (p sinint-var)
  (labels
      ((polyint1 (p sinint-var)
	 (cond ((or (null p) (equal p 0))
		(cons 0 1))
	       ((atom p)
		(list sinint-var 1 p))
	       ((not (numberp (car p)))
		(if (pointergp sinint-var (car p))
		    (list sinint-var 1 p)
		    (polyint1 (cdr p) sinint-var)))
	       (t
		(ratplus (polyint2 p sinint-var)
			 (polyint1 (cddr p) sinint-var)))))

       (polyint2 (p sinint-var)
	 (cons (list sinint-var
		     (1+ (car p))
		     (cadr p))
	       (1+ (car p)))))
    (ratqu (polyint1 (ratnumerator p) sinint-var)
	   (ratdenominator p))))
	 

(defun dprog (ratarg sinint-ratform sinint-var)
  (prog (klth kx arootf deriv thebpg thetop thebot prod1 prod2 ans
	 sinint-logptdx sinint-parnumer sinint-pardenom sinint-rootfactor sinint-wholepart)
     (setq ans (cons 0 1))
     (if (or (pcoefp (cdr ratarg)) (pointergp sinint-var (cadr ratarg)))
	 (return (values (disrep (polyint ratarg sinint-var) sinint-ratform)
			 sinint-logptdx)))
     (multiple-value-setq (sinint-rootfactor sinint-pardenom)
       (aprog (ratdenominator ratarg) sinint-var))
     (multiple-value-setq (sinint-parnumer sinint-wholepart)
       (cprog (ratnumerator ratarg)
	      (ratdenominator ratarg)
	      sinint-var
	      sinint-pardenom))
     (setq sinint-rootfactor (reverse sinint-rootfactor))
     (setq sinint-parnumer (reverse sinint-parnumer))
     (setq klth (length sinint-rootfactor))
   intg
     (if (= klth 1) (go simp))
     (setq arootf (car sinint-rootfactor))
     (if (zerop (pdegree arootf sinint-var))
	 (go reset))
     (setq deriv (pderivative arootf sinint-var))
     (setq thebpg (bprog arootf deriv sinint-var))
     (setq kx (1- klth))
     (setq thetop (car sinint-parnumer))
   iter
     (setq prod1 (ratti thetop (car thebpg) t))
     (setq prod2 (ratti thetop (cdr thebpg) t))
     (setq thebot (pexpt arootf kx))
     (setq ans
	   (ratplus ans (ratqu (ratminus prod2)
			       (ratti kx thebot t))))
     (setq thetop
	   (ratplus prod1
		    (ratqu (ratreduce (pderivative (car prod2) sinint-var)
				      (cdr prod2))
			   kx)))
     (setq thetop (cdr (ratdivide thetop thebot)))
     (cond ((= kx 1)
	    (setq sinint-logptdx (cons (ratqu thetop arootf)
				       sinint-logptdx))
	    (go reset)))
     (setq kx (1- kx))
     (go iter)
   reset
     (setq sinint-rootfactor (cdr sinint-rootfactor))
     (setq sinint-parnumer (cdr sinint-parnumer))
     (decf klth)
     (go intg)
   simp
     (push (ratqu (car sinint-parnumer) (car sinint-rootfactor))
	   sinint-logptdx)
     (if (equal ans 0)
	 (return (values (disrep (polyint sinint-wholepart sinint-var) sinint-ratform)
			 sinint-logptdx)))
     (setq thetop
	   (cadr (pdivide (ratnumerator ans) (ratdenominator ans))))
     (return (values (list '(mplus)
			   (disrep (polyint sinint-wholepart sinint-var) sinint-ratform)
			   (disrep (ratqu thetop (ratdenominator ans)) sinint-ratform))
		     sinint-logptdx))))

(defun logmabs (x)
  (list '(%log) (if $logabs
		    (simplify (list '(mabs) x))
		    x)))

(defun npask (npask-exp)
  (cond ((freeof '$%i npask-exp)
	 (learn `((mnotequal) ,npask-exp 0)
		t)
	 (asksign npask-exp))
	(t '$positive)))

(defvar $integrate_use_rootsof nil
  "Use the rootsof form for integrals when denominator does not factor")

(defun integrate-use-rootsof (f q variable sinint-ratform)
  (let ((dummy (make-param))
	(qprime (disrep (pderivative q (p-var q)) sinint-ratform))
	(ff (disrep f sinint-ratform))
	(qq (disrep q sinint-ratform)))
    ;; This basically does a partial fraction expansion and integrates
    ;; the result.  Let r be one (simple) root of the denominator
    ;; polynomial q.  Then the partial fraction expansion is
    ;;
    ;;   f(x)/q(x) = A/(x-r) + similar terms.
    ;;
    ;; Then
    ;;
    ;;   f(x) = A*q(x)/(x-r) + others
    ;;
    ;; Take the limit as x -> r.
    ;;
    ;;   f(r) = A*limit(q(x)/(x-r),x,r) + others
    ;;        = A*at(diff(q(x),r), [x=r])
    ;;
    ;; Hence, A = f(r)/at(diff(q(x),x),[x=r])
    ;;
    ;; Then it follows that the integral is
    ;;
    ;;    A*log(x-r)
    ;;
    ;; Note that we don't express the polynomial in terms of the
    ;; variable of integration, but in our dummy variable instead.
    ;; Using the variable of integration results in a wrong answer
    ;; when a substitution was done previously, since when the
    ;; substitution is finally undone, that modifies the polynomial.
    `((%lsum) ((mtimes)
	       ,(div* (subst dummy variable ff)
		      (subst dummy variable qprime))
	       ((%log) ,(sub* variable dummy)))
      ,dummy
      (($rootsof) ,(subst dummy variable qq) ,dummy))))

(defun eprog (p sinint-ratform sinint-var sinint-switch1)
  (prog (p1e p2e a1e a2e a3e discrim repart sign ncc dcc allcc xx deg
	 sinint-parnumer sinint-pardenom sinint-wholepart)
     (if (or (equal p 0) (equal (car p) 0))
	 (return 0))
     (setq p1e (ratnumerator p)
	   p2e (ratdenominator p))
     (cond ((or sinint-switch1
		(and (not (atom p2e))
		     (eq (car (setq xx (cadr (oldcontent p2e))))
			 sinint-var)
		     (member (setq deg (pdegree xx sinint-var)) '(5 6) :test #'equal)
		     (zerocoefl xx deg sinint-var)
		     (or (equal deg 5)
			 (not (pminusp (car (last xx)))))))
	    (go efac)))
     (setq a1e (intfactor p2e))
     (if (> (length a1e) 1)
	 (go e40))
   efac
     (setq ncc (oldcontent p1e))
     (setq p1e (cadr ncc))
     (setq dcc (oldcontent p2e))
     (setq p2e (cadr dcc))
     (setq allcc (ratqu (car ncc) (car dcc)))
     (setq deg (pdegree p2e sinint-var))
     (setq a1e (pderivative p2e sinint-var))
     (setq a2e (ratqu (polcoef p1e (pdegree p1e sinint-var) sinint-var)
		      (polcoef a1e (pdegree a1e sinint-var) sinint-var)))
     (cond ((equal (ratti a2e a1e t) (cons p1e 1))
	    (return (list '(mtimes)
			  (disrep (ratti allcc a2e t) sinint-ratform )
			  (logmabs (disrep p2e sinint-ratform))))))
     (cond ((equal deg 1) (go e10))
	   ((equal deg 2) (go e20))
	   ((and (equal deg 3) (equal (polcoef p2e 2 sinint-var) 0)
		 (equal (polcoef p2e 1 sinint-var) 0))
	    (return (e3prog p1e p2e allcc sinint-ratform sinint-var sinint-switch1)))
	   ((and (member deg '(4 5 6) :test #'equal)
		 (zerocoefl p2e deg sinint-var))
	    (return (enprog p1e p2e allcc deg sinint-ratform sinint-var))))
     (cond ((and $integrate_use_rootsof
		 (equal (car (psqfr p2e)) p2e))
	    (return (list '(mtimes)
			  (disrep allcc sinint-ratform)
			  (integrate-use-rootsof p1e p2e
						 (car (last varlist))
						 sinint-ratform)))))
     (return (list '(mtimes)
		   (disrep allcc sinint-ratform)
		   (list '(%integrate)
			 (list '(mquotient)
			       (disrep p1e  sinint-ratform)
			       (disrep p2e sinint-ratform))
			 (car (last varlist)))))
   e10
     (return (list '(mtimes)
		   (disrep (ratti allcc
				  (ratqu (polcoef p1e (pdegree p1e sinint-var) sinint-var)
					 (polcoef p2e 1 sinint-var))
				  t)
			   sinint-ratform)
		   (logmabs (disrep p2e sinint-ratform))))
   e20
     (setq discrim
	   (ratdifference (cons (pexpt (polcoef p2e 1 sinint-var) 2)
				1)
			  (ratti 4
				 (ratti (polcoef p2e 2 sinint-var)
					(polcoef p2e 0 sinint-var)
					t)
				 t)))
     (setq a2e (ratti (polcoef p2e (pdegree p2e sinint-var) sinint-var) 2 t))
     (setq xx (simplify (disrep discrim sinint-ratform)))
     (when (equal ($imagpart xx) 0)
       (setq sign (npask xx))
       (cond ((eq sign '$negative) (go e30))
	     ((eq sign '$zero) (go zip))))
     (setq a1e (ratsqrt discrim sinint-ratform))
     (setq a3e (logmabs
		(list '(mquotient)
		      (list '(mplus)
			    (list '(mtimes)
				  (disrep a2e sinint-ratform)
				  (disrep (list sinint-var 1 1) sinint-ratform))
			    (disrep (polcoef p2e 1 sinint-var) sinint-ratform)
			    (list '(mminus) a1e))
		      (list '(mplus)
			    (list '(mtimes)
				  (disrep a2e sinint-ratform)
				  (disrep (list sinint-var 1 1) sinint-ratform))
			    (disrep (polcoef p2e 1 sinint-var) sinint-ratform)
			    a1e))))
     (cond ((zerop (pdegree p1e sinint-var))
	    (return (list '(mtimes)
			  (disrep allcc sinint-ratform)
			  (list '(mquotient)
				(disrep (polcoef p1e 0 sinint-var) sinint-ratform)
				a1e)
			  a3e))))
     (return
       (list
	'(mplus)
	(list '(mtimes)
	      (disrep (ratti allcc
			     (ratqu (polcoef p1e (pdegree p1e sinint-var) sinint-var) a2e)
			     t)
		      sinint-ratform)
	      (logmabs (disrep p2e sinint-ratform)))
	(list
	 '(mtimes)
	 (list
	  '(mquotient)
	  (disrep (ratti allcc
			 (ratqu (eprogratd a2e p1e p2e sinint-var) a2e)
			 t)
		  sinint-ratform)
	  a1e)
	 a3e)))
   e30
     (setq a1e (ratsqrt (ratminus discrim) sinint-ratform))
     (setq
      repart
      (ratqu (cond ((zerop (pdegree p1e sinint-var))
		    (ratti a2e (polcoef p1e 0 sinint-var) t))
		   (t
		    (eprogratd a2e p1e p2e sinint-var)))
	     (polcoef p2e (pdegree p2e sinint-var) sinint-var)))
     (setq a3e (cond ((equal 0 (car repart))
		      0)
		     (t
		      `((mtimes) ((mquotient)
				  ,(disrep (ratti allcc repart t) sinint-ratform)
				  ,a1e)
			((%atan)
			 ((mquotient)
			  ,(disrep (pderivative p2e sinint-var) sinint-ratform)
			  ,a1e))))))
     (if (zerop (pdegree p1e sinint-var))
	 (return a3e))
     (return (list '(mplus)
		   (list '(mtimes)
			 (disrep (ratti allcc
					(ratqu (polcoef p1e (pdegree p1e sinint-var) sinint-var) a2e)
					t)
				 sinint-ratform)
			 (logmabs (disrep p2e sinint-ratform)))
		   a3e))
   zip
     (setq
	 p2e
	 (ratqu
	  (psimp
	   (p-var p2e)
	   (pcoefadd 2 
		     (pexpt (ptimes 2 (polcoef p2e 2 sinint-var)) 2)
		     (pcoefadd 1
			       (ptimes 4 (ptimes (polcoef p2e 2 sinint-var)
						 (polcoef p2e 1 sinint-var)))
			       (pcoefadd 0 (pexpt (polcoef p2e 1 sinint-var) 2) ()))))
	  (ptimes 4 (polcoef p2e 2 sinint-var))))
     (return (fprog (ratti allcc (ratqu p1e p2e) t)
		    sinint-ratform
		    sinint-var))
   e40
     (setq sinint-parnumer nil
	   sinint-pardenom a1e
	   sinint-switch1 t)
     (multiple-value-setq (sinint-parnumer sinint-wholepart)
       (cprog p1e p2e sinint-var sinint-pardenom))
     (setq a2e
	   (mapcar #'(lambda (j k)
		       (eprog (ratqu j k) sinint-ratform sinint-var sinint-switch1))
		   sinint-parnumer sinint-pardenom))
     (setq sinint-switch1 nil)
     (return (cons '(mplus) a2e))))
 
(defun e3prog (num denom cont sinint-ratform sinint-var sinint-switch1)
  (prog (a b c d e r ratr var* x)
     (setq a (polcoef num 2 sinint-var)
	   b (polcoef num 1 sinint-var)
	   c (polcoef num 0 sinint-var)
	   d (polcoef denom 3 sinint-var)
	   e (polcoef denom 0 sinint-var))
     (setq r (cond ((eq (npask (simplify (disrep (ratqu e d) sinint-ratform)))
			'$negative)
		    (simpnrt (disrep (ratqu (ratti -1 e t) d) sinint-ratform) 3))
		   (t
		    (neg (simpnrt (disrep (ratqu e d) sinint-ratform) 3)))))
     (setq var* (list sinint-var 1 1))
     (newvar r)
     (orderpointer varlist)
     (setq x (ratf r))
     (setq sinint-ratform (car x) ratr (cdr x))
     (return
       (simplify
	(list '(mplus)
	      (list '(mtimes)
		    (disrep (ratqu (r* cont (r+ (r* a ratr ratr) (r* b ratr) c))
				   (r* ratr ratr 3 d))
			    sinint-ratform)
		    (logmabs (disrep (ratpl (ratti -1 ratr t) var*) sinint-ratform)))
	      (eprog (r* cont (ratqu (r+ (r* (r+ (r* 2 a ratr ratr)
						 (r* -1 b ratr)
						 (r* -1 c))
					     var*)
					 (r+ (ratqu (r* -1 a e) d)
					     (r* b ratr ratr)
					     (r* -1 2 c ratr)))
				     (r* 3 d ratr ratr
					 (r+ (ratti var* var* t)
					     (ratti ratr var* t)
					     (ratti ratr ratr t)))))
		     sinint-ratform
		     sinint-var
		     sinint-switch1)
	      )))))

(defun eprogratd (a2e p1e p2e sinint-var)
  (ratdifference (ratti a2e
			(polcoef p1e (1- (pdegree p1e sinint-var)) sinint-var)
			t)
		 (ratti (polcoef p2e (1- (pdegree p2e sinint-var)) sinint-var)
			(polcoef p1e (pdegree p1e sinint-var) sinint-var)
			t)))

(defun enprog (num denom cont deg sinint-ratform sinint-var)
  ;; Denominator is (A*VAR^4+B) = 
  ;;   if B<0 then (SQRT(A)*VAR^2 - SQRT(-B)) (SQRT(A)*VAR^2 + SQRT(-B))
  ;;	     else
  ;;	(SQRT(A)*VAR^2 - SQRT(2)*A^(1/4)*B^(1/4)*VAR + SQRT(B)) * 
  ;;	(SQRT(A)*VAR^2 + SQRT(2)*A^(1/4)*B^(1/4)*VAR + SQRT(B))
  ;; or (A*VAR^5+B) = 
  ;;	(1/4) * (A^(1/5)*VAR + B^(1/5)) *
  ;;	(2*A^(2/5)*VAR^2 + (-SQRT(5)-1)*A^(1/5)*B^(1/5)*VAR + 2*B^(2/5)) *
  ;;	(2*A^(2/5)*VAR^2 + (+SQRT(5)-1)*A^(1/5)*B^(1/5)*VAR + 2*B^(2/5))
  ;; or (A*VAR^6+B) = 
  ;;   if B<0 then (SQRT(A)*VAR^3 - SQRT(-B)) (SQRT(A)*VAR^3 + SQRT(-B))
  ;;	     else
  ;;	(A^(1/3)*VAR^2 + B^(1/3)) *
  ;;	(A^(1/3)*VAR^2 - SQRT(3)*A^(1/6)*B^(1/6)*VAR + B^(1/3)) *
  ;;	(A^(1/3)*VAR^2 + SQRT(3)*A^(1/6)*B^(1/6)*VAR + B^(1/3))
  (prog ($expop $expon a b term disvar $algebraic)
     (setq $expop 0 $expon 0)
     (setq a (simplify (disrep (polcoef denom deg sinint-var) sinint-ratform))
	   b (simplify (disrep (polcoef denom 0 sinint-var) sinint-ratform))
	   disvar (simplify (get sinint-var 'disrep))
	   num (simplify (disrep num sinint-ratform))
	   cont (simplify (disrep cont sinint-ratform)))
     (cond ((= deg 4)
	    (if (eq '$neg ($asksign b))
		(setq denom
		      (mul2 (add2 (mul2 (power a '((rat simp) 1 2)) (power disvar 2))
				  (power (mul -1 b) '((rat simp) 1 2)))
			    (add2 (mul2 (power a '((rat simp) 1 2)) (power disvar 2))
				  (mul -1 (power (mul -1 b) '((rat simp) 1 2))))))
		(progn
		  (setq denom (add2 (mul2 (power a '((rat simp) 1 2)) (power disvar 2))
				    (power b '((rat simp) 1 2)))
			term (muln (list (power 2 '((rat simp) 1 2))
					 (power a '((rat simp) 1 4))
					 (power b '((rat simp) 1 4))
					 disvar)
				   t))
		  (setq denom (mul2 (add2 denom term) (sub denom term))))))
	   ((= deg 5)
	    (setq term (mul3 (power a '((rat simp) 1 5))
			     (power b '((rat simp) 1 5))
			     disvar))
	    (setq denom (add2 (mul3 2 (power a '((rat simp) 2 5))
				    (power disvar 2))
			      (sub (mul2 2 (power b '((rat simp) 2 5))) term)))
	    (setq term (mul2 (power 5 '((rat simp) 1 2)) term))
	    (setq denom (muln (list '((rat simp) 1 4)
				    (add2 (mul2 (power a '((rat simp) 1 5)) disvar)
					  (power b '((rat simp) 1 5)))
				    (add2 denom term) (sub denom term))
			      t)))
	   (t
	    (if (eq '$neg ($asksign b))
		(setq denom
		      (mul2 (add2 (mul2 (power a '((rat simp) 1 2)) (power disvar 3))
				  (power (mul -1 b) '((rat simp) 1 2)))
			    (add2 (mul2 (power a '((rat simp) 1 2)) (power disvar 3))
				  (mul -1 (power (mul -1 b) '((rat simp) 1 2))))))
		(progn
		  (setq denom (add2 (mul2 (power a '((rat simp) 1 3)) (power disvar 2))
				    (power b '((rat simp) 1 3)))
			term (muln (list (power 3 '((rat simp) 1 2))
					 (power a '((rat simp) 1 6))
					 (power b '((rat simp) 1 6))
					 disvar)
				   t))
		  (setq denom (mul3 denom (add2 denom term) (sub denom term))))
		)))
     ;;Needs $ALGEBRAIC NIL so next call to RATF will preserve factorization.
     (return (mul2 cont (ratint (div num denom) disvar)))))

(defun zerocoefl (e n sinint-var)
  (do ((i 1 (1+ i)))
      ((= i n) t)
    (if (not (equal (polcoef e i sinint-var) 0))
	(return nil))))

(defun ratsqrt (a sinint-ratform)
  (let (varlist)
    (simpnrt (disrep a sinint-ratform) 2)))

(defun fprog (rat* sinint-ratform sinint-var)
  (multiple-value-bind (dprog-ret sinint-logptdx)
      (dprog rat* sinint-ratform sinint-var)
    (addn (cons dprog-ret
		(mapcar #'(lambda (p)
			    (eprog p sinint-ratform sinint-var nil))
			sinint-logptdx))
	  nil)))

(defun ratint (sinint-exp sinint-var)
  (prog (genvar *checkfactors* varlist ratarg $keepfloat)
     (setq varlist (list sinint-var))
     (setq ratarg (ratf sinint-exp))
     (let ((sinint-ratform (car ratarg))
	   (sinint-var (caadr (ratf sinint-var))))
       (return (fprog (cdr ratarg) sinint-ratform sinint-var)))))

(defun intfactor (l)
  (labels ((everysecond (a)
	     (if a (cons (if (numberp (car a))
			     (pexpt (car a) (cadr a))
			     (car a))
			 (everysecond (cddr a))))))
	   
    (prog ($factorflag a b)
       (setq a (oldcontent l)
	     b (everysecond (pfactor (cadr a))))
       (return (if (equal (car a) 1)
		   b
		   (cons (car a) b))))))
