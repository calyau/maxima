;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;; whole file revised to avoid conflict with CRE forms. 4/27/2016 Richard Fateman

(in-package :maxima)

(macsyma-module psolve)

(declare-top (special mult *roots *failures $solvefactors))
(declare-top (special expsumsplit $dispflag checkfactors *g
		      $algebraic equations ;List of E-labels
		      *power *varb *flg $derivsubst
		      $%emode genvar genpairs varlist broken-not-freeof
		      mult    ;Some crock which tracks multiplicities.
		      *roots ;alternating list of solutions and multiplicities
		      *failures	;alternating list of equations and multiplicities
		      *myvar $listconstvars
		      *has*var *var $dontfactor
		      $keepfloat $ratfac
		      xm* xn* mul*))

(defmvar flag4 nil)

(defun solvecubic (x) 
  (prog (s1 a0 a1 a2 discr lcoef adiv3 omega^2 pdiv3 qdiv-2
	 omega y1 u y2) 
    
     (setq x (cdr x))
     (setq lcoef (cadr x))
     (setq adiv3
	 (mul
		 '((rat) -1 3)
		 (rdis (setq a2 (ratreduce (ptterm x 2)
					   lcoef)))))
     (setq a1 (ratreduce (ptterm x 1) lcoef))
     (setq a0 (ratreduce (ptterm x 0) lcoef))
      
    ;;      coefficients now a0,a1,a2,  and leading coef 1.
      
   (setf a2 (rdis a2) a1 (rdis a1) a0 (rdis a0))
    
    
      
   (setq s1 (mul' ((rat) 1 2) '$%i (power 3 '((rat) 1 2))))
   (setq omega (add '((rat) -1 2)  s1) 
	 omega^2 (add '((rat) -1 2) (mul -1 s1)))
     (setq pdiv3
	 (add (mul a1 '( (rat) 1  3))
	      (mul  (power	a2 2) '((rat) -1 9))))
     (and (not (equal pdiv3 0)) (go harder))
     (setq y1
	 (mul
	     '((rat) 1 3)
	  (add
	   (simpnrt  (setq y2 (add (power a2 3)
				   (mul -27 a0)))
		     3)			; cube root
	   (mul -1 a2 ))))
    
     (and flag4 (return (solve3 y1 mult)))
   (setq y2 (simpnrt  (mul  y2 '((rat) 1  27)) 3))
     (return (mapc #'(lambda (j) (solve3 j mult))
		   (list y1
			(add (mul omega y2) adiv3)
			(add (mul omega^2 y2) adiv3))))
     harder
     (setq qdiv-2
	 (add (mul (add (mul a1 a2) (mul -3  a0))
		   '((rat) 1 6))
	      (mul (power a2 3) '((rat) -1  27) )))
    
     (cond ((equal qdiv-2 0)
	    (setq u (simpnrt pdiv3 2))
	    (setq y1 adiv3))
	 (t (setq discr (add 
			 (power pdiv3 3)
			 (power qdiv-2 2)))
				   
	      (cond ((equal discr 0)
		     (setq u (simpnrt qdiv-2 3)))
		    (t (setq discr (simpnrt discr 2))
		       (and (complicated discr)
			    (setq discr (adispline discr)))
		     (setq u (power
			      (add
						     qdiv-2
						     discr)
			      '((rat) 1 3)))
		       (and (complicated u)
			    (setq u (adispline u)))))))
     (if (equal u 0) (merror (intl:gettext "SOLVECUBIC: arithmetic overflow.")))
     (or y1
       (setq y1 (add adiv3  u  (mul  -1 pdiv3 (power u -1)))))
     (return
       (cond (flag4 (solve3 y1 mult))
	     (t (mapc 
		 #'(lambda (j) (solve3 j mult))
		 (list y1
		    (add  adiv3 (mul omega u)  (mul -1 pdiv3 omega^2 (power u -1)))
		    (add  adiv3 (mul omega^2 u)(mul -1 pdiv3 omega   (power u -1))))))))))

(defun solvequartic (x) 
  (prog (a0 a1 a2 b1 b2 b3 b0 lcoef z1 r tr1 tr2 d d1 e sqb3) 
     (setq x (cdr x) lcoef (cadr x))
   (setq b3 (rdis(ratreduce (ptterm x 3) lcoef)))
   (setq b2 (rdis(ratreduce (ptterm x 2) lcoef)))
   (setq b1 (rdis(ratreduce (ptterm x 1) lcoef)))
   (setq b0 (rdis(ratreduce (ptterm x 0) lcoef)))
   (setq a2 (mul -1 b2))
   (setq a1 (sub (mul b1 b3) (setq a0 (mul b0 4))))
     (setq a0
	 (sub (sub (mul b2 a0) (mul (setq sqb3(power b3 2)) b0 )) (power b1 2)))
   (setq tr2   (mul'((rat) 1 4)
		(sub (sub (mul b3  b2 4)
			  (mul 8 b1))
		     (mul sqb3 b3 )) ))
     (setq z1 (resolvent a2 a1 a0))
     (setq r
	 (add
			  z1
	  (sub (mul sqb3 '((rat) 1 4))
					b2)))
     (setq r (simpnrt r 2))
     (and (equal r 0) (go l0))
     (and (complicated r) (setq r (adispline r)))
     (and (complicated tr2) (setq tr2 (adispline tr2)))
     (setq tr1
	 (sub
	  (sub (mul sqb3 '((rat) 1  2))
	       b2)
	   z1))
     (and (complicated tr1) (setq tr1 (adispline tr1)))
     (setq tr2 (div* tr2 r))
     (go lb1)
   l0   (setq d1 (simpnrt (add (power z1 2) (mul -4 b0)) 2))
   (setq tr2 (mul 2 d1))
     (and (complicated tr2) (setq tr2 (adispline tr2)))
   (setq tr1 (sub (mul sqb3 '((rat) 3 4))  (mul b2 2)))
     (and (complicated tr1) (setq tr1 (adispline tr1)))
  lb1
     (setq d (div (power (add tr1 tr2) '((rat simp) 1 2)) 2))
     (setq e (div (power (sub tr1 tr2) '((rat simp) 1 2)) 2))
     (and (complicated d) (setq d (adispline d)))
     (and (complicated e) (setq e (adispline e)))
   (setq a2 (mul b3 '((rat) -1 4)))
     (setq a1 (div* r 2))
    
     (setq z1
     (list (add a2 a1 d)		;1
	   (add a2 a1 (mul -1 d))	;2
	   (add a2 (mul -1 a1) e)	;3
	   (add	a2 (mul -1 a1) (mul -1 e)) ;4
	       ))
     (return (mapc #'(lambda (j) (solve3 j mult)) z1))))

;;; SOLVES RESOLVENT CUBIC EQUATION
;;; GENERATED FROM QUARTIC

(defun resolvent (a2 a1 a0) 
  (prog (*roots flag4 *failures $solvefactors yy) ;undoes binding in
     (setq flag4 t $solvefactors t)	       ;algsys
    (setf yy (gensym))
     (solve (add
	     (power yy 3)
	     (mul a2   (power yy 2))
	     (mul a1  yy)
	     a0)
	    yy
	    1)
     (when (member 0 *roots :test #'equal) (return 0))
     (return (caddar (cdr (reverse *roots))))))

(declare-top (unspecial mult))
