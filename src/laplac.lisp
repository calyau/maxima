;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module laplac)

(declare-top (special context))

;;; The properties NOUN and VERB give correct linear display

(defprop $laplace %laplace verb)
(defprop %laplace $laplace noun)

(defprop $ilt %ilt verb)
(defprop %ilt $ilt noun)

(defun exponentiate (pow)
  ;;COMPUTES %E**Z WHERE Z IS AN ARBITRARY EXPRESSION TAKING SOME OF THE WORK AWAY FROM SIMPEXPT
  (cond ((zerop1 pow) 1)
	((equal pow 1) '$%e)
	(t (power '$%e pow))))

(defun fixuprest (rest)
  ;;REST IS A PRODUCT WITHOUT THE MTIMES.FIXUPREST PUTS BACK THE MTIMES
  (cond ((null rest) 1)
	((cdr rest) (cons '(mtimes) rest))
	(t (car rest))))

(defun isquadraticp (e x)
  (let ((b (sdiff e x)))
    (cond ((zerop1 b) (list 0 0 e))
	  ((freeof x b) (list 0 b (maxima-substitute 0 x e)))
	  ((setq b (islinear b x))
	   (list (div* (car b) 2) (cdr b) (maxima-substitute 0 x e))))))

;;;INITIALIZES SOME GLOBAL VARIABLES THEN CALLS THE DISPATCHING FUNCTION

(defmfun $laplace (fun time-var parm)
  (setq fun (mratcheck fun))
  (cond ((or *nounsflag* (member '%laplace *nounl* :test #'eq))
         (setq fun (remlaplace fun))))
  (laplace fun time-var parm))

;;;LAMBDA BINDS SOME SPECIAL VARIABLES TO NIL AND DISPATCHES

(defun remlaplace (e)
  (if (atom e)
      e
      (cons (delete 'laplace (append (car e) nil) :count 1 :test #'eq)
	    (mapcar #'remlaplace (cdr e)))))

(defun laplace (fun time-var parm &optional (dvar nil))
  (let ()
    ;; Handles easy cases and calls appropriate function on others.
    (cond ((mbagp fun) (cons (car fun) (mapcar #'(lambda (e) (laplace e time-var parm)) (cdr fun))))
	  ((equal fun 0) 0)
	  ((equal fun 1)
	   (cond ((zerop1 parm) (simplify (list '($delta) 0)))
		 (t (power parm -1))))
	  ((alike1 fun time-var) (power parm -2))
	  ((or (atom fun) (freeof time-var fun))
	   (cond ((zerop1 parm) (mul2 fun (simplify (list '($delta) 0))))
		 (t (mul2 fun (power parm -1)))))
	  (t 
           (let ((op (caar fun)))
             (let ((result ; We store the result of laplace for further work.
		     (cond ((eq op 'mplus)
			    (laplus fun time-var parm))
			   ((eq op 'mtimes)
			    (laptimes (cdr fun) time-var parm))
			   ((eq op 'mexpt)
			    (lapexpt fun nil time-var parm))
			   ((eq op '%sin)
			    (lapsin fun nil nil time-var parm))
			   ((eq op '%cos)
			    (lapsin fun nil t time-var parm))
			   ((eq op '%sinh)
			    (lapsinh fun nil nil time-var parm))
			   ((eq op '%cosh)
			    (lapsinh fun nil t time-var parm))
			   ((eq op '%log)
			    (laplog fun time-var parm))
			   ((eq op '%derivative)
			    (lapdiff fun time-var parm))
			   ((eq op '%integrate)
			    (lapint fun time-var parm dvar))
			   ((eq op '%sum)
			    (list '(%sum)
				  (laplace (cadr fun) time-var parm)
				  (caddr fun)
				  (cadddr fun)
				  (car (cddddr fun))))
			   ((eq op '%erf)
			    (laperf fun time-var parm))
			   ((and (eq op '%ilt)(eq (cadddr fun) time-var))
			    (cond ((eq parm (caddr fun))(cadr fun))
				  (t (subst parm (caddr fun)(cadr fun)))))
			   ((eq op '$delta)
			    (lapdelta fun nil time-var parm))
 			   ((member op '(%hstep $unit_step))
 			    (laphstep fun nil time-var parm))
			   ((setq op ($get op '$laplace))
			    (mcall op fun time-var parm))
			   (t
			    (lapdefint fun time-var parm)))))
               (when (isinop result '%integrate)
                 ;; Laplace has not found a result but returns a definit
                 ;; integral. This integral can contain internal integration 
                 ;; variables. Replace such a result with the noun form.
                 (setq result (list '(%laplace) fun time-var parm)))
               ;; Check if we have a result, when not call $specint.
               (check-call-to-$specint result fun time-var parm)))))))

;;; Check if laplace has found a result, when not try $specint.

(defun check-call-to-$specint (result fun time-var parm)
  (cond 
    ((or (isinop result '%laplace)
         (isinop result '%limit) ; Try $specint for incomplete results
         (isinop result '%at))	 ; which contain %limit or %at too.
     ;; laplace returns a noun form or a result which contains %limit or %at.
     ;; We pass the function to $specint to look for more results.
     (let (res)
       ;; laplace assumes the parameter s to be positive and does a
       ;; declaration before an integration is done. Therefore we declare
       ;; the parameter of the Laplace transform to be positive before 
       ;; we call $specint too.
       (with-new-context (context)
         (meval `(($assume) ,@(list (list '(mgreaterp) parm 0))))
         (setq res ($specint (mul fun (power '$%e (mul -1 time-var parm))) time-var)))
       (if (or (isinop res '%specint) ; Both symobls are possible, that is
               (isinop res '$specint)) ; not consistent! Check it! 02/2009
           ;; $specint has not found a result.
           result
           ;; $specint has found a result
           res)))
    (t result)))

(defun laplus (fun time-var parm)
  (simplus (cons '(mplus) (mapcar #'(lambda (e) (laplace e time-var parm)) (cdr fun))) 1 t))

(defun laptimes (fun time-var parm)
  ;;EXPECTS A LIST (PERHAPS EMPTY) OF FUNCTIONS MULTIPLIED TOGETHER WITHOUT THE MTIMES
  ;;SEES IF IT CAN APPLY THE FIRST AS A TRANSFORMATION ON THE REST OF THE FUNCTIONS
  (cond ((null fun) (list '(mexpt) parm -1.))
	((null (cdr fun)) (laplace (car fun) time-var parm))
	((freeof time-var (car fun))
	 (simptimes (list '(mtimes)
			  (car fun)
			  (laptimes (cdr fun) time-var parm))
		    1
		    t))
	((eq (car fun) time-var)
	 (simptimes (list '(mtimes) -1 (sdiff (laptimes (cdr fun) time-var parm) parm))
		    1
		    t))
	(t
	 (let ((op (caaar fun)))
	   (cond ((eq op 'mexpt)
		  (lapexpt (car fun) (cdr fun) time-var parm))
		 ((eq op 'mplus)
		  (laplus ($multthru (fixuprest (cdr fun)) (car fun)) time-var parm))
		 ((eq op '%sin)
		  (lapsin (car fun) (cdr fun) nil time-var parm))
		 ((eq op '%cos)
		  (lapsin (car fun) (cdr fun) t time-var parm))
		 ((eq op '%sinh)
		  (lapsinh (car fun) (cdr fun) nil time-var parm))
		 ((eq op '%cosh)
		  (lapsinh (car fun) (cdr fun) t time-var parm))
		 ((eq op '$delta)
		  (lapdelta (car fun) (cdr fun) time-var parm))
 		 ((member op '(%hstep $unit_step))
 		  (laphstep (car fun) (cdr fun) time-var parm))
		 (t
		  (lapshift (car fun) (cdr fun) time-var parm)))))))

(defun lapexpt (fun rest time-var parm)
  ;;HANDLES %E**(A*T+B)*REST(T), %E**(A*T**2+B*T+C),
  ;; 1/SQRT(A*T+B), OR T**K*REST(T)
  (prog (ab base-of-fun power result)
     (setq base-of-fun (cadr fun) power (caddr fun))
     (cond
       ((and
	 (freeof time-var base-of-fun)
	 (setq
	  ab
	  (isquadraticp
	   (cond ((eq base-of-fun '$%e) power)
		 (t (simptimes (list '(mtimes)
				     power
				     (list '(%log)
					   base-of-fun))
			       1
			       nil)))
	   time-var)))
	(cond ((equal (car ab) 0) (go %e-case-lin))
	      ((null rest) (go %e-case-quad))
	      (t (go noluck))))
       ((and (eq base-of-fun time-var) (freeof time-var power))
	(go var-case))
       ((and (alike1 '((rat) -1 2) power) (null rest)
	     (setq ab (islinear base-of-fun time-var)))
	(setq result (div* (cdr ab) (car ab)))
	(return (simptimes
		 (list '(mtimes)
		       (list '(mexpt)
			     (div* '$%pi
				   (list '(mtimes)
					 (car ab)
					 parm))
			     '((rat) 1 2))
		       (exponentiate (list '(mtimes) result parm))
		       (list '(mplus)
			     1
			     (list '(mtimes)
				   -1
				   (list '(%erf)
					 (list '(mexpt)
					       (list '(mtimes)
						     result
						     parm)
					       '((rat) 1 2)))
				   ))) 1 nil)))
       (t (go noluck)))
   %e-case-lin
     (setq result
	   (cond
	     (rest (sratsimp ($at (laptimes rest time-var parm)
				  (list '(mequal)
					parm
					(list '(mplus)
					      parm
					      (afixsign (cadr ab)
							nil))))))
	     (t (list '(mexpt)
		      (list '(mplus)
			    parm
			    (afixsign (cadr ab) nil))
		      -1))))
     (return (simptimes (list '(mtimes)
			      (exponentiate (caddr ab))
			      result)
			1
			nil))
   %e-case-quad
     (setq result (afixsign (car ab) nil))
     (setq
      result
      (list
       '(mtimes)
       (div* (list '(mexpt)
		   (div* '$%pi result)
		   '((rat) 1 2))
	     2)
       (exponentiate (div* (list '(mexpt) parm 2)
			   (list '(mtimes) 4 result)))
       (list '(mplus)
	     1
	     (list '(mtimes)
		   -1
		   (list '(%erf)
			 (div* parm
			       (list '(mtimes)
				     2
				     (list '(mexpt)
					   result
					   '((rat) 1 2)))))
		   ))))
     (and (null (equal (cadr ab) 0))
	  (setq result
		(maxima-substitute (list '(mplus)
					 parm
					 (list '(mtimes)
					       -1
					       (cadr ab)))
				   parm
				   result)))
     (return (simptimes  (list '(mtimes)
			       (exponentiate (caddr ab))
			       result) 1 nil))
   var-case
     (cond ((or (null rest) (freeof time-var (fixuprest rest)))
	    (go var-easy-case)))
     (cond ((posint power)
	    (return (afixsign (apply '$diff
				     (list (laptimes rest time-var parm)
					   parm
					   power))
			      (even power))))
	   ((negint power)
	    (return (mydefint (hackit power rest time-var parm)
			      (createname parm (- power))
			      parm parm)))
	   (t (go noluck)))
   var-easy-case
     (setq power
	   (simplus (list '(mplus) 1 power) 1 t))
     (or (eq (asksign ($realpart power)) '$positive) (go noluck))
     (setq result (list (list '(%gamma) power)
			(list '(mexpt)
			      parm
			      (afixsign power nil))))
     (and rest (setq result (nconc result rest)))
     (return (simptimes (cons '(mtimes) result) 1 nil))
   noluck
     (return
       (cond
	 ((and (posint power)
	       (member (caar base-of-fun)
		       '(mplus %sin %cos %sinh %cosh) :test #'eq))
	  (laptimes (cons base-of-fun
			  (cons (cond ((= power 2) base-of-fun)
				      (t (list '(mexpt)
					       base-of-fun
					       (1- power))))
				rest))
		    time-var parm))
	 (t (lapshift fun rest time-var parm))))))

;;;INTEGRAL FROM A TO INFINITY OF F(X)
(defun mydefint (f x a parm)
  (let ((tryint (and (not ($unknown f))
                     ;; We don't want any errors thrown by $defint to propagate,
                     ;; so we set errset, errcatch, $errormsg and *mdebug*.  If
                     ;; $defint throws a error, then no error message is printed
                     ;; and errset simply returns nil below.
                     (with-new-context (context)
                       (meval `(($assume) ,@(list (list '(mgreaterp) parm 0))))
                       (meval `(($assume) ,@(list (list '(mgreaterp) x 0))))
                       (meval `(($assume) ,@(list (list '(mgreaterp) a 0))))
                       (let ((errset nil)
                             (errcatch t)
                             ($errormsg nil)
                             (*mdebug* nil))
                         (errset ($defint f x a '$inf)))))))
    (if tryint
	(car tryint)
	(list '(%integrate) f x a '$inf))))

 ;;;CREATES UNIQUE NAMES FOR VARIABLE OF INTEGRATION
(defun createname (head tail)
  (intern (format nil "~S~S" head tail)))

;;;REDUCES LAPLACE(F(T)/T**N,T,S) CASE TO LAPLACE(F(T)/T**(N-1),T,S) CASE
(defun hackit (exponent rest time-var parm)
  (cond ((equal exponent -1)
	 (let ((parm (createname parm 1)))
	   (laptimes rest parm time-var)))
	(t
	 (mydefint (hackit (1+ exponent) rest time-var parm)
		   (createname parm (- -1 exponent))
		   (createname parm (- exponent)) parm))))

(defun afixsign (funct signswitch)
  ;;MULTIPLIES FUNCT BY -1 IF SIGNSWITCH IS NIL
  (cond (signswitch funct)
	(t (simptimes (list '(mtimes) -1 funct) 1 t))))

(defun lapshift (fun rest time-var parm)
  (cond ((atom fun)
	 (merror "LAPSHIFT: expected a cons, not ~M" fun))
	((or (member 'laplace (car fun) :test #'eq)
	     (null rest))
	 (lapdefint (cond (rest (simptimes (cons '(mtimes)
						 (cons fun rest)) 1 t))
			  (t fun))
		    time-var parm))
	(t
	 (laptimes (append rest
			   (ncons (cons (append (car fun)
						'(laplace))
					(cdr fun))))
		   time-var parm))))

;;;COMPUTES %E**(W*B*%I)*F(S-W*A*%I) WHERE W=-1 IF SIGN IS T ELSE W=1
(defun mostpart (f parm sign a b)
  (let ((substinfun ($at f
			 (list '(mequal)
			       parm
			       (list '(mplus) parm (afixsign (list '(mtimes) a '$%i) sign))))))
    (if (zerop1 b)
	substinfun
	(list '(mtimes)
	      (exponentiate (afixsign (list '(mtimes) b '$%i) (null sign)))
	      substinfun))))

 ;;;IF WHICHSIGN IS NIL THEN SIN TRANSFORM ELSE COS TRANSFORM
(defun compose (fun parm whichsign a b)
  (let ((result (list '((rat) 1 2)
		      (list '(mplus)
			    (mostpart fun parm t a b)
			    (afixsign (mostpart fun parm nil a b)
				      whichsign)))))
    (sratsimp (simptimes (cons '(mtimes)
			       (if whichsign
				   result
				   (cons '$%i result)))
			 1 nil))))

 ;;;FUN IS OF THE FORM SIN(A*T+B)*REST(T) OR COS
(defun lapsin (fun rest trigswitch time-var parm)
  (let ((ab (islinear (cadr fun) time-var)))
    (cond (ab
	   (cond (rest
		  (compose (laptimes rest parm time-var)
			   parm
			   trigswitch
			   (car ab)
			   (cdr ab)))
		 (t
		  (simptimes
		   (list '(mtimes)
		    (cond ((zerop1 (cdr ab))
			   (if trigswitch parm (car ab)))
			  (t
			   (cond (trigswitch
				  (list '(mplus)
					(list '(mtimes)
					      parm
					      (list '(%cos) (cdr ab)))
					(list '(mtimes)
					      -1
					      (car ab)
					      (list '(%sin) (cdr ab)))))
				 (t
				  (list '(mplus)
					(list '(mtimes)
					      parm
					      (list '(%sin) (cdr ab)))
					(list '(mtimes)
					      (car ab)
					      (list '(%cos) (cdr ab))))))))
		    (list '(mexpt)
			  (list '(mplus)
				(list '(mexpt) parm 2)
				(list '(mexpt) (car ab) 2))
			  -1))
		   1 nil))))
	  (t
	   (lapshift fun rest time-var parm)))))

 ;;;FUN IS OF THE FORM SINH(A*T+B)*REST(T) OR IS COSH
(defun lapsinh (fun rest switch time-var parm)
  (cond ((islinear (cadr fun) time-var)
	 (sratsimp
	  (laplus
	   (simplus
	    (list '(mplus)
		  (nconc (list '(mtimes)
			       (list '(mexpt)
				     '$%e
				     (cadr fun))
			       '((rat) 1 2))
			 rest)
		  (afixsign (nconc (list '(mtimes)
					 (list '(mexpt)
					       '$%e
					       (afixsign (cadr fun)
							 nil))
					 '((rat) 1 2))
				   rest)
			    switch))
	    1
	    nil)
	   time-var parm)))
	(t
	 (lapshift fun rest time-var parm))))

 ;;;FUN IS OF THE FORM LOG(A*T)
(defun laplog (fun time-var parm)
  (let ((ab (islinear (cadr fun) time-var)))
    (cond ((and ab (zerop1 (cdr ab)))
	   (simptimes (list '(mtimes)
			    (list '(mplus)
				  (subfunmake '$psi '(0) (ncons 1))
				  (list '(%log) (car ab))
				  (list '(mtimes) -1 (list '(%log) parm)))
			    (list '(mexpt) parm -1))
		      1 nil))
	  (t
	   (lapdefint fun time-var parm)))))

(defun raiseup (fbase exponent)
  (if (equal exponent 1)
      fbase
      (list '(mexpt) fbase exponent)))

;;TAKES TRANSFORM OF HSTEP(A*T+B)*F(T)
(defun laphstep (fun rest time-var parm)
  (let ((ab (islinear (cadr fun) time-var)))
    (if ab
	(let* ((a (car ab))
	       (b (cdr ab))
	       (offset (div b a)))
	  (case (asksign a)
	    ($positive
	     (if (eq (asksign offset) '$negative)
		 (mul (exponentiate (mul offset parm))
		      (laplace (maxima-substitute
				(sub time-var offset) time-var
				(fixuprest rest))
			       time-var parm))
		 (laptimes rest time-var parm)))
	    ($negative
	     (if (eq (asksign offset) '$negative)
		 (sub (laptimes rest time-var parm)
		      (mul (exponentiate (mul offset parm))
			   (laplace (maxima-substitute
				     (sub time-var offset) time-var
				     (fixuprest rest))
				    time-var parm)))
		 0))
	    (t
	     (mul fun (laptimes rest time-var parm)))))
      (lapshift fun rest time-var parm))))
			  
;;TAKES TRANSFORM OF DELTA(A*T+B)*F(T)
(defun lapdelta (fun rest time-var parm)
  (let ((ab (islinear (cadr fun) time-var))
	(sign nil)
	(recipa nil))
    (cond (ab
	   (setq recipa (power (car ab) -1) ab (div (cdr ab) (car ab)))
	   (setq sign (asksign ab) recipa (simplifya (list '(mabs) recipa) nil))
	   (simplifya (cond ((eq sign '$positive)
			     0)
			    ((eq sign '$zero)
			     (list '(mtimes)
				   (maxima-substitute 0 time-var (fixuprest rest))
				   recipa))
			    (t
			     (list '(mtimes)
				   (maxima-substitute (neg ab) time-var (fixuprest rest))
				   (list '(mexpt) '$%e (cons '(mtimes) (cons parm (ncons ab))))
				   recipa)))
		      nil))
	  (t
	   (lapshift fun rest time-var parm)))))

(defun laperf (fun time-var parm)
  (let ((ab (islinear (cadr fun) time-var)))
    (cond ((and ab (equal (cdr ab) 0))
	   (simptimes (list '(mtimes)
			    (div* (exponentiate (div* (list '(mexpt) parm 2)
						      (list '(mtimes)
							    4
							    (list '(mexpt) (car ab) 2))))
				  parm)
			    (list '(mplus)
				  1
				  (list '(mtimes)
					-1
					(list '(%erf) (div* parm (list '(mtimes) 2 (car ab)))))))
		      1
		      nil))
	  (t
	   (lapdefint fun time-var parm)))))

(defun lapdefint (fun time-var parm)
  (prog (tryint mult)
     (and ($unknown fun)(go skip))
     (setq mult (simptimes (list '(mtimes) (exponentiate
					    (list '(mtimes) -1 time-var parm)) fun) 1 nil))
     (with-new-context (context)
       (meval `(($assume) ,@(list (list '(mgreaterp) parm 0))))
       (setq tryint
             ;; We don't want any errors thrown by $defint to propagate,
             ;; so we set errset, errcatch, $errormsg and *mdebug*.  If
             ;; $defint throws a error, then no error message is printed
             ;; and errset simply returns nil below.
             (let ((errset nil)
                   (errcatch t)
                   ($errormsg nil)
                   (*mdebug* nil))
               (errset ($defint mult time-var 0 '$inf)))))
     (and tryint (not (eq (and (listp (car tryint))
			       (caaar tryint))
			  '%integrate))
	  (return (car tryint)))
   skip
     (return (list '(%laplace) fun time-var parm))))


(defun lapdiff (fun time-var parm)
  ;;FUN IS OF THE FORM DIFF(F(T),T,N) WHERE N IS A POSITIVE INTEGER
  (prog (difflist degree frontend resultlist newdlist order arg2)
     (setq newdlist (setq difflist (copy-tree (cddr fun))))
     (setq arg2 (list '(mequal) time-var 0))
   a
     (cond ((null difflist)
	    (return (cons '(%derivative)
			  (cons (list '(%laplace)
				      (cadr fun)
				      time-var
				      parm)
				newdlist))))
	   ((eq (car difflist) time-var)
	    (setq degree (cadr difflist)
		  difflist (cddr difflist))
	    (go out)))
     (setq difflist (cdr (setq frontend (cdr difflist))))
     (go a)
   out
     (cond ((null (posint degree))
	    (return (list '(%laplace) fun time-var parm))))
     (cond (frontend (rplacd frontend difflist))
	   (t (setq newdlist difflist)))
     (cond (newdlist (setq fun (cons '(%derivative)
				     (cons (cadr fun)
					   newdlist))))
	   (t (setq fun (cadr fun))))
     (setq order 0)
   loop
     (decf degree)
     (setq resultlist
	   (cons (list '(mtimes)
		       (raiseup parm degree)
		       ($at ($diff fun time-var order) arg2))
		 resultlist))
     (incf order)
     (and (> degree 0) (go loop))
     (setq resultlist (cond ((cdr resultlist)
			     (cons '(mplus)
				   resultlist))
			    (t (car resultlist))))
     (return (simplus (list '(mplus)
			    (list '(mtimes)
				  (raiseup parm order)
				  (laplace fun time-var parm))
			    (list '(mtimes)
				  -1
				  resultlist))
		      1 nil))))

 ;;;FUN IS OF THE FORM INTEGRATE(F(X)*G(T)*H(T-X),X,0,T)
(defun lapint (fun time-var parm dvar)
  (prog (newfun parm-list f var-list var-parm-list)
     (and dvar (go convolution))
     (setq dvar (cadr (setq newfun (cdr fun))))
     (and (cddr newfun)
	  (zerop1 (caddr newfun))
	  (eq (cadddr newfun) time-var)
	  (go convolutiontest))
   notcon
     (setq newfun (cdr fun))
     (cond ((cddr newfun)
	    (cond ((and (freeof time-var (caddr newfun))
			(freeof time-var (cadddr newfun)))
		   (return (list '(%integrate)
				 (laplace (car newfun) time-var parm dvar)
				 dvar
				 (caddr newfun)
				 (cadddr newfun))))
		  (t (go giveup))))
	   (t (return (list '(%integrate)
			    (laplace (car newfun) time-var parm dvar)
			    dvar))))
   giveup
     (return (list '(%laplace) fun time-var parm))
   convolutiontest
     (setq newfun ($factor (car newfun)))
     (cond ((eq (caar newfun) 'mtimes)
	    (setq f (cadr newfun) newfun (cddr newfun)))
	   (t (setq f newfun newfun nil)))
   gothrulist
     (cond ((freeof dvar f)
	    (setq parm-list (cons f parm-list)))
	   ((freeof time-var f) (setq var-list (cons f var-list)))
	   ((freeof dvar
		    (sratsimp (maxima-substitute (list '(mplus)
						       time-var
						       dvar)
						 time-var
						 f)))
	    (setq var-parm-list (cons f var-parm-list)))
	   (t (go notcon)))
     (cond (newfun
	    (setq f (car newfun) newfun (cdr newfun))
	    (go gothrulist)))
     (and
      parm-list
      (return
	(laplace
	 (cons
	  '(mtimes)
	  (nconc parm-list
		 (ncons (list '(%integrate)
			      (cons '(mtimes)
				    (append var-list
					    var-parm-list))
			      dvar
			      0
			      time-var))))
	 time-var parm dvar)))
   convolution
     (return
       (simptimes
	(list
	 '(mtimes)
	 (laplace ($expand (maxima-substitute time-var
					      dvar
					      (fixuprest var-list)))
		  time-var parm dvar)
	 (laplace
	  ($expand (maxima-substitute 0 dvar (fixuprest var-parm-list)))
	  time-var parm dvar))
	1
	t))))

(defmfun $ilt (exp ils ilt)
  ;;EXP IS F(S)/G(S) WHERE F AND G ARE POLYNOMIALS IN S AND DEGR(F) < DEGR(G)
  (let (varlist ($savefactors t) *checkfactors* $ratfac $keepfloat
		s-var)
    ;; MAKES ILS THE MAIN VARIABLE
    (setq varlist (list ils))
    (newvar exp)
    (orderpointer varlist)
    (setq s-var (caadr (ratrep* ils)))
    (cond ((and (null (atom exp))
		(eq (caar exp) 'mequal))
	   (list '(mequal)
		 ($ilt (cadr exp) ils ilt)
		 ($ilt (caddr exp) ils ilt)))
	  ((zerop1 exp) 0)
	  ((freeof ils exp)
 	   (list '(mtimes) exp (list '($delta) ilt)))
	  (t (ilt0 exp ils ilt s-var)))))

(defun maxima-rationalp (le v)
  (cond ((null le))
	((and (null (atom (car le))) (null (freeof v (car le))))
	 nil)
	(t
	 (maxima-rationalp (cdr le) v))))

 ;;;THIS FUNCTION DOES THE PARTIAL FRACTION DECOMPOSITION
(defun ilt0 (exp ils ilt s-var)
  (prog (wholepart frpart num denom y content real factor
	 apart bpart parnumer ratarg laplac-ratform)
     (and (mplusp exp)
	  (return (simplus  (cons '(mplus)
				  (mapcar #'(lambda (f) ($ilt f ils ilt)) (cdr exp))) 1 t)))
     (and (null (atom exp))
	  (eq (caar exp) '%laplace)
	  (eq (cadddr exp) ils)
	  (return (cond ((eq (caddr exp) ilt) (cadr exp))
			(t (subst ilt
				  (caddr exp)
				  (cadr exp))))))
     (setq ratarg (ratrep* exp))
     (or (maxima-rationalp varlist ils)
	 (return (list '(%ilt) exp ils ilt)))
     (setq laplac-ratform (car ratarg))
     (setq denom (ratdenominator (cdr ratarg)))
     (setq frpart (pdivide (ratnumerator (cdr ratarg)) denom))
     (setq wholepart (car frpart))
     (setq frpart (ratqu (cadr frpart) denom))
     (cond ((not (zerop1 (car wholepart)))
	    (return (list '(%ilt) exp ils ilt)))
	   ((zerop1 (car frpart)) (return 0)))
     (setq num (car frpart) denom (cdr frpart))
     (setq y (oldcontent denom))
     (setq content (car y))
     (setq real (cadr y))
     (setq factor (pfactor real))
   loop
     (cond ((null (cddr factor))
	    (setq apart real
		  bpart 1
		  y '((0 . 1) 1 . 1))
	    (go skip)))
     (setq apart (pexpt (car factor) (cadr factor)))
     (setq bpart (car (ratqu real apart)))
     (setq y (bprog apart bpart s-var))
   skip
     (setq frpart
	   (cdr (ratdivide (ratti (ratnumerator num)
				  (cdr y)
				  t)
			   (ratti (ratdenominator num)
				  (ratti content apart t)
				  t))))
     (setq
      parnumer
      (cons (ilt1 (ratqu (ratnumerator frpart)
			 (ratti (ratdenominator frpart)
				(ratti (ratdenominator num)
				       content
				       t)
				t))
		  (car factor)
		  (cadr factor)
		  laplac-ratform
		  ils
		  ilt
		  s-var)
	    parnumer))
     (setq factor (cddr factor))
     (cond ((null factor)
	    (return (simplus (cons '(mplus) parnumer) 1 t))))
     (setq num (cdr (ratdivide (ratti num (car y) t)
			       (ratti content bpart t))))
     (setq real bpart)
     (go loop)))

(defun ilt1 (p q k laplac-ratform ils ilt s-var)
  (let (z)
    (cond ((onep1 k)
	   (ilt3 p laplac-ratform ils ilt q s-var))
	  (t
	   (setq z (bprog q (pderivative q s-var) s-var))
	   (ilt2 p k laplac-ratform ils ilt z q s-var)))))


 ;;;INVERTS P(S)/Q(S)**K WHERE Q(S)  IS IRREDUCIBLE
 ;;;DOESN'T CALL ILT3 IF Q(S) IS LINEAR
(defun ilt2 (p k laplac-ratform ils ilt z q s-var)
  (prog (y a b)
     (and (onep1 k) (return (ilt3 p laplac-ratform ils ilt q s-var)))
     (decf k)
     (setq a (ratti p (car z) t))
     (setq b (ratti p (cdr z) t))
     (setq y (pexpt q k))
     (cond
       ((or (null (equal (pdegree q s-var) 1))
	    (> (pdegree (car p) s-var) 0))
	(return
	  (simplus
	   (list
	    '(mplus)
	    (ilt2
	     (cdr (ratdivide (ratplus a (ratqu (ratderivative b s-var) k)) y))
	     k
	     laplac-ratform
	     ils
	     ilt
	     z
	     q
	     s-var)
	    ($multthru (simptimes (list '(mtimes)
					ilt
					(power k -1)
					(ilt2 (cdr (ratdivide b y))
					      k
					      laplac-ratform
					      ils
					      ilt
					      z
					      q
					      s-var))
				  1
				  t)))
	   1
	   t))))
     (setq a (disrep (polcoef q 1 s-var) laplac-ratform)
	   b (disrep (polcoef q 0 s-var) laplac-ratform))
     (return
       (simptimes (list '(mtimes)
			(disrep p laplac-ratform)
			(raiseup ilt k)
			(simpexpt (list '(mexpt)
					'$%e
					(list '(mtimes)
					      -1
					      ilt
					      b
					      (list '(mexpt) a -1)))
				  1
				  nil)
			(list '(mexpt)
			      a
			      (- -1 k))
			(list '(mexpt)
			      (factorial k)
			      -1))
		  1
		  nil))))

;;(DEFUN COEF MACRO (POL) (SUBST (CADR POL) (QUOTE DEG)
;;  '(DISREP (RATQU (POLCOEF (CAR P) DEG) (CDR P)))))

;;;INVERTS P(S)/Q(S) WHERE Q(S) IS IRREDUCIBLE
(defun ilt3 (p laplac-ratform ils ilt q s-var)
  (labels
      ((coef (pol coef-ratform)
	 (disrep (ratqu (polcoef (car p) pol s-var) (cdr p)) coef-ratform))
       ;; FIXME: Consider replacing these 3 functions with
       ;; make-mplus-l, make-mtimes-l, and make-expt-l.
       (lapsum (&rest args)
	 (cons '(mplus) args))
       (lapprod (&rest args)
	 (cons '(mtimes) args))
       (expo (&rest args)
	 (cons '(mexpt) args)))
    (prog (discrim sign a c d e b1 b0 r term1 term2 degr)
       (setq e (disrep (polcoef q 0 s-var) laplac-ratform)
	     d (disrep (polcoef q 1 s-var) laplac-ratform)
	     degr (pdegree q s-var))
       (and (equal degr 1)
	    (return
	      (simptimes (lapprod (disrep p laplac-ratform)
				  (expo d -1)
				  (expo '$%e (lapprod -1 ilt e (expo d -1))))
			 1
			 nil)))
       (setq c (disrep (polcoef q 2 s-var) laplac-ratform))
       (and (equal degr 2) (go quadratic))
       (and (equal degr 3) (zerop1 c) (zerop1 d)
	    (go cubic))
       (return (list '(%ilt) (div* (disrep p laplac-ratform)
				   (disrep q laplac-ratform))
		     ils ilt))
     cubic
       (setq  a (disrep (polcoef q 3 s-var) laplac-ratform)
	      r (simpnrt (div* e a) 3))
       (setq d (div* (disrep p laplac-ratform)
		     (lapprod a (lapsum (expo ils 3)
					(expo '%r 3)))))
       (return (ilt0 (maxima-substitute r '%r ($partfrac d ils)) ils ilt s-var))
     quadratic
       (setq b0 (coef 0 laplac-ratform) b1 (coef 1 laplac-ratform))

       (setq discrim
	     (simplus (lapsum
		       (lapprod 4 e c)
		       (lapprod -1 d d))
		      1
		      nil))
       (setq sign (cond ((free discrim '$%i) (asksign discrim)) (t '$positive))
	     term1 '(%cos)
	     term2 '(%sin))
       (setq degr (expo '$%e (lapprod ilt d (power c -1) '((rat) -1 2))))
       (cond ((eq sign '$zero)
	      (return (simptimes (lapprod degr
					  (lapsum (div* b1 c)
						  (lapprod
						   (div* (lapsum (lapprod 2 b0 c)
								 (lapprod -1 b1 d))
							 (lapprod 2 c c))
						   ilt)))
				 1 nil)))
	     ((eq sign '$negative)
	      (setq term1 '(%cosh)
		    term2 '(%sinh)
		    discrim (simptimes (lapprod -1 discrim) 1 t))))
       (setq discrim (simpnrt discrim 2))
       (setq sign
	     (simptimes
	      (lapprod
	       (lapsum
		(lapprod 2 b0 c)
		(lapprod -1 b1 d))
	       (expo discrim -1))
	      1
	      nil))
       (setq c (power c -1))
       (setq discrim (simptimes (lapprod discrim
				 ilt
				 '((rat) 1 2)
				 c)
				1
				t))
       (return
	 (simptimes
	  (lapprod c degr
		   (lapsum
		    (lapprod b1 (list term1 discrim))
		    (lapprod sign (list term2 discrim))))
	  1
	  nil)))))
