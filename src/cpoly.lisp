;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module cpoly)

;;;This is a lisp version of algorithm 419 from the Communications of
;;;the ACM (p 97 vol 15 feb 1972) by Jenkins and Traub.
;;;That algorithm is followed very closely.
;;;Note the following modifications: arrays are indexed from 0 instead
;;;of 1.  This means that the variables n and nn are one less than the 
;;;acm verson.  The zeros are put into the arrays pr-sl and pi-sl, rather than
;;;into their own arrays.  The algorithm seems to benefit be taking are 
;;;mre 0.01 times the published values.

(declare-top
 (*expr displa $listofvars meqhk displine)
 (special logbas infin smalno are mre cr ci sr si tr ti zr zi n nn bool conv pvr
	  pvi $partswitch $keepfloat $demoivre $listconstvars $algebraic acp-sl
	  $polyfactor polysc polysc1 $ratfac $programmode)
 (flonum logbas infin smalno are mre cr ci sr si tr ti zr zi xx yy cosr sinr bnd
	 xni t1 t2 otr oti svsr svsi pvr pvi mp ms omp relstp tp hvr hvi e ar
	 ai br bi x xm f dx df r1 $t hi lo max min acp-sl)
 (fixnum degree n nn j l l1 l2 l3 cnt1 cnt2 jj polysc polysc1))

(declare-top(notype ($cpoly notype) (noshft-sl fixnum) (fxshft-sl fixnum)
		 (vrshft-sl fixnum) (calct-sl) (nexth-sl) (polyev-sl)
		 (cdivid-sl flonum flonum flonum flonum) (scale-sl))
	 (flonum (errev-sl flonum flonum) (cauchy-sl) (cmod-sl flonum flonum))
	 (fixnum (cpoly-sl fixnum))
  #-PDP10(flonum (*f flonum flonum) (//f flonum flonum) (_f flonum fixnum))
  #-PDP10(*expr *f //f _f)
	 (*lexpr $rat MAXIMA-ERROR)) 

;;I changed these to unnamed arrays so as to be common lisp compatible
;;In older lisps use (defmacro aref (ar &rest dims) `(arraycall t ,@dims))
;;Really we should have non special variables for inner loop references to the
;;arrays, but.. later --wfs.

;(declare 
; (array* (flonum *PR-SL* 1. *PI-SL* 1. *SHR-SL* 1. *SHI-SL* 1. *QPR-SL* 1. *QPI-SL* 1. *HR-SL* 1. *HI-SL*
;		 1. *QHR-SL* 1. *QHI-SL* 1.)))

(declare-top
 (special *PR-SL* *PI-SL* *SHR-SL* *SHI-SL* *QPR-SL* *QPI-SL* *HR-SL* *HI-SL*
		 *QHR-SL* *QHI-SL*))
;; Fixed for Twenex systems?

#+PDP10
(and (not (get '*f 'subr)) 
     (mapc '(lambda (x) (putprop x '(arith fasl dsk macsym) 'autoload))
	   '(*f //f +f -f _f)))

;;; It is harder to underflow on lisp machine, but I suppose someday -BEE

#-PDP10
(eval-when (compile load)
       (defmacro *f  (a b) `(*$ ,a ,b))
       (defmacro //f (a b) `(//$ ,a ,b))
       (defmacro +f  (a b) `(+$ ,a ,b))
       (defmacro -f  (a b) `(-$ ,a ,b))
       )

;(defmacro cpoly-large-flonum ()
;  #+LISPM (let ((a (float 0)))
;	    (%p-dpb -1 1013 a) (%p-dpb -1 0007 a)
;	    (%p-dpb-offset -1 0030 a 1) a)
;  #+NIL most-positive-double-float
;  #-(or LISPM NIL) '(fsc (lsh -1 -1) 0.)
;)
;
;(defmacro cpoly-small-flonum ()
;  #+LISPM (%p-dpb 100 0007 (float 0))
;  #+NIL least-positive-double-float
;  #-(or LISPM NIL) '(fsc (rot 1. -10.) 0.)
;  )
;
;(defmacro float-precision (pres)
;  pres ;Ignored on Lisp Machine
;  #+LISPM (let ((a (float 1)))
;	    (%p-dpb-offset 1 0001 a 1) (f- a 1.0))
;  #+NIL (progn ;I wish i knew exactly what this meant. --gsb
;	       `(*$ double-float-epsilon ,pres)
;	       )
;  #-(or LISPM NIL) `(*$ (fsc (lsh 205. 26.) 0.) ,pres)
;  )
;;;
#+Multics(defmacro _f  (a b) `(fsc ,a ,b))
       

;;; There were no comments on the following macros and functions, that on the
;;; Lisp Machine try to use %P-xxx subprimitives, and just used the #+LISPM
;;; conditional.  Of course, trying to transport this code to any other kind
;;; of Lisp Machine referneces random memory!  I've changed around the 
;;; conditionalization some, to make the macros and function only have definitions for
;;; machines that are known about, and I supported the 3600 as best as I could,
;;; since I had to intuit what the desired functionality is.  --HIC  3/5/83

;;; Returns largest possible flonum?  (why not just a constant?)
#-cl
(defmacro cpoly-large-flonum ()
	  #-LISPM '(fsc (lsh -1 -1) 0.)
	  #+(and LISPM CADR)
	    (let ((a (float 0)))
	      (%p-dpb -1 1013 a) (%p-dpb -1 0007 a)
	      (%p-dpb-offset -1 0030 a 1) a)
	  #+(and LISPM 3600)
	    (times 2.0 1.45e38))  ;; was 2.9e38 but then file won't load 
			;; into non-Lispm systems.  E.g. on MC gives 
			;; NUMERIC-OVERFLOW - READ  - JPG
;; we are using long-float in maxima for flonum
#+cl
(defmacro cpoly-large-flonum () (float most-positive-long-float))

;;; Returns smallest possible flonum?  (why not just a constant?)
#-cl
(defmacro cpoly-small-flonum ()
	  #-LISPM '(fsc (rot 1. -10.) 0.)
	  #+(and LISPM CADR) (%p-dpb 100 0007 (float 0))
	  #+(and LISPM 3600) 2.0e-38)
#+cl
(defmacro cpoly-small-flonum () (float least-positive-long-float))

;;; Returns precision of floating point representation as least significant possible
;;; floating point number?  (again, why not just a constant?)
#-cl
(defmacro MAXIMA-float-precision (pres)
	  pres ;Ignored on Lisp Machine
	  #-LISPM `(*$ (fsc (lsh 205. 26.) 0.) ,pres)
	  #+(and LISPM CADR)
	    (let ((a (float 1)))
	      (%p-dpb-offset 1 0001 a 1)
	      (- a 1.0))
	  #+(and LISPM 3600)
	    (let* ((a (float 1))
		   (b (si:flonum-mantissa a nil))
		   (c (si:flonum-exponent a nil)))
	      (- (si:%flonum (dpb (f+ c (f+ 126. 24.)) si:%%float-exponent
				  (dpb (logior b 1) si:%%float-fraction 0)))
		 1.0)))
;I doubt this is what was intended but I don't know --wfs.
  #. (let ((e double-float-epsilon))
       (cond ((= (float 1 e) (+ (float 1 e) e))
	      (format t
		      "Warning your double-float-epsilon is too small by CLtL"
		      ))))

#+cl
(defun MAXIMA-float-precision (pres)
  ;; Check double-float-epsilon is really ok for this lisp,
  ;; according to the common lisp manual it should have.

     (cond ((eq (type-of pres) (type-of double-float-epsilon))
	    double-float-epsilon)
	   ((eq (type-of pres) (type-of short-float-epsilon))
	    short-float-epsilon)
	   (t (error "unknown type ~a" pres))))

;;; _F scales its floating point argument by its scale argument, which is 
;;; a power of two to scale by.

#+cl
(defun dfloat (x) (coerce x 'double-float))

;; #+Franz
;; (defun _f (number scale) somebody-write-this)


;;; I don't think that this is called enough to justify making it quick.
;;; Ought to be faster--wfs
#+(or Franz)
(defun _f (number scale)
    (cond ((zerop number) 0.0)
	  (t (f* number (cond ((zerop scale) 1)
			     ((plusp scale)
			      (^ 2.0 scale))
			     ((// 1.0 (^ 2.0 (minus scale)))))))))

#+cl ;;should condition it to switch to double floats if too big
(defun _f (number scale) (scale-float number scale))


;#+(and cl ti) ;;bug in microcoded
;(defun _f (f scale) (f* f (expt (float (float-radix f) f) scale)))


;#+(and LISPM CADR)
;(defun _f (number scale)			
;    (let ((ans (+ 0.0 number))			
;	  (exp))
;      (setq exp (+ scale (%p-ldb 1013 ans)))	
;      (cond ((zerop number) 0.0)
;	    ((> exp 3777) (error  "_F Overflow -- see MC:CFFK;CPOLY"))
;	    ;; Should check zunderflow
;	    ((< exp 0) (error  "_F Underflow -- see MC:CFFK;CPOLY"))
;	    (t (%p-dpb exp 1013 ans) ans))))

;#+(and LISPM 3600)
;(defun _f (number scale)
;  (let ((new-exp (f+ (f+ 126. 24.) (si:flonum-exponent number nil) scale))	
;	(mant (si:flonum-mantissa number nil)))
;    (cond ((zerop number) 0.0)
;	  ((> new-exp 377) (error  "_F Overflow -- see MC:CFFK;CPOLY"))
;	  ;; Should check zunderflow
;	  ((< new-exp 0) (error  "_F Underflow -- see MC:CFFK;CPOLY"))
;	  (t (si:%flonum (dpb new-exp si:%%float-exponent
;			      (dpb mant si:%%float-fraction 0)))))))

(setq acp-sl 0.2) 

(DEFMFUN $allroots (expr)
       (prog (degree nn var res $partswitch $keepfloat $demoivre $listconstvars
	      $algebraic complex $ratfac den expr1)
	     (setq $keepfloat t $listconstvars t $algebraic t)
	     (setq expr1 (setq expr (meqhk expr)))
	     (setq var (delq '$%i (cdr ($listofvars expr))))
	     (or var (setq var (list (gensym))))
	     (cond ((not (= (length var) 1.))
		    (merror "ALLROOTS: polynomial not univariate: ~M" var))
		   ((setq var (car var))))
	     (setq expr ($rat expr '$%i var)
		   res (reverse (car (cdddar expr))))
	     (do ((i (f- (length res) (length (caddar expr))) (f1- i)))
		 ((= i 0.))
		 (setq res (cdr res)))
	     (setq den (cddr expr) expr (cadr expr))
	     ;;;check denominator is a complex number
	     (cond ((numberp den) (setq den (list den 0)))
		   ((eq (car den) (cadr res))
		    (setq den (cddr den))
		    (cond ((numberp (car den))
			   (cond ((null (cddr den)) (setq den (list 0 (car den))))
				 ((numberp (caddr den))
				  (setq den (list (caddr den) (car den))))
				 (t (cpoly-err expr1))))
			  (t (cpoly-err expr1))))
		   (t (cpoly-err expr1)))
	     ;;;if the name variable has disappeared, this is caught here
	     (setq nn 0)
	     (cond ((numberp expr) (setq expr (list expr 0)))
		   ((eq (car expr) (car res)) (setq nn 1))
		   ((eq (car expr) (cadr res))
		    (setq expr (cddr expr))
		    (cond ((numberp (car expr))
			   (cond ((null (cddr expr)) (setq expr (list 0 (car expr))))
				 ((numberp (caddr expr))
				  (setq expr (list (caddr expr) (car expr))))
				 (t (cpoly-err expr1))))
			  (t (cpoly-err expr1))))
		   (t (cpoly-err expr1)))
	     (cond ((= nn 0)
		    (cond ($polyfactor
			   ((lambda (cr ci)
				    (cdivid-sl (float (car expr)) (float (cadr expr))
					     (float (car den)) (float (cadr den)))
				    (return (simplify (list '(mplus)
							    (simplify (list '(mtimes)
									    '$%i ci))
							    cr))))
			    0.0 0.0))
			  (t (return (list '(mlist simp)))))))
	     (setq degree (cadr expr) nn (f1+ degree))
	     (setq *pr-sl* (*array nil 'flonum nn))
	     (setq *pi-sl* (*array nil 'flonum nn))
	     (or (catch 'notpoly
			(errset (do ((expr (cdr expr) (cddr expr)) (l) (%i (cadr res)))
				    ((null expr))
				    (setq l (f- degree (car expr)) res (cadr expr))
				    (cond ((numberp res) (store (aref *PR-SL* l) (float res)))
					  (t 
					     (or (eq (car res) %i)
						 (throw 'notpoly nil))
					     (setq res (cddr res))
					     (store (aref *PI-SL* l) (float (car res)))
					     (setq res (caddr res))
					     (and res (store (aref *PR-SL* l) (float res)))
					     (setq complex t))))))
		 ;;;this should catch expressions like sin(x)-x
		 (progn (*rearray '*PR-SL*)
			(*rearray '*PI-SL*)
			(cpoly-err expr1)))
	     (setq *shr-sl* (*array nil 'flonum nn))
	     (setq *shi-sl* (*array nil 'flonum nn))
	     (setq *qpr-sl* (*array nil 'flonum nn))
	     (setq *hr-sl* (*array nil 'flonum degree))
	     (setq *qhr-sl*(*array nil 'flonum degree))
	     (cond
	       (complex
	     (setq *qpi-sl*(*array nil 'flonum nn))
	     (setq *hi-sl*(*array nil 'flonum degree))
	     (setq *qhi-sl*(*array nil 'flonum degree))
			    ))
	     (setq nn degree)
	     (cond (complex (setq res (errset (cpoly-sl degree))))
		   ((setq res (errset (rpoly-sl degree)))))
	     (*rearray '*SHR-SL*)
	     (*rearray '*SHI-SL*)
	     (*rearray '*QPR-SL*)
	     (*rearray '*HR-SL*)
	     (*rearray '*QHR-SL*)
	     (cond (complex (*rearray '*QPI-SL*)
			    (*rearray '*HI-SL*)
			    (*rearray '*QHI-SL*)))
	     (or res
		 (mtell "~%Unexpected error. Treat results with caution."))
	     (cond ((= nn degree)
		    (*rearray '*PR-SL*)
		    (*rearray '*PI-SL*)
		    (merror "~%No roots found")))
	     (setq res nil)
	     (cond
	      ((not (zerop nn))
	       (mtell "~%Only ~S out of ~S roots found "
		      (f- degree nn) degree)
	       (setq expr 0.0)
	       (do
		((i 0. (f1+ i)))
		((> i nn))
		(setq 
		 expr
		 (simplify
		  (list '(mplus)
			expr
			(simplify (list '(mtimes)
					(simplify (list '(mplus)
							(simplify (list '(mtimes)
									'$%i
									(aref *PI-SL* i)))
							(aref *PR-SL* i)))
					(simplify (list '(mexpt)
							var
							(f- nn i)))))))))
	       (setq res (cons expr res)))
	      ($polyfactor
	       (setq expr ((lambda (cr ci)
				   (cdivid-sl (aref *PR-SL* 0) (aref *PI-SL* 0)
					    (float (car den))
					    (float (cadr den)))
				   (simplify (list '(mplus)
						   (simplify (list '(mtimes)
								   '$%i ci))
						   cr)))
			   0.0 0.0)
		     res (cons expr res))))
	     (do
	      ((i degree (f1- i)))
	      ((= i nn))
	      (setq expr (simplify (list '(mplus)
					 (simplify (list '(mtimes)
							 '$%i
							 (aref *PI-SL* i)))
					 (aref *PR-SL* i))))
	      (setq 
	       res
	       (cond
		($polyfactor (cons (cond ((or complex (zerop (aref *PI-SL* i) ))
					  (simplify (list '(mplus)
							  var
							  (simplify (list '(mminus)
									  expr)))))
					 (t (setq i (f1- i))
					    (simplify (list '(mplus)
							    (simplify (list '(mexpt)
									    var
									    2.))
							    (simplify (list '(mtimes)
									    var
									    (aref *PR-SL* i)))
							    (aref *PR-SL* (f1+ i))))))
				   res))
		((cons ((lambda (expr) (cond ($programmode expr)
					     (t (displine expr))))
			(simplify (list '(mequal) var expr)))
		       res)))))
	     (*rearray '*PR-SL*)
	     (*rearray '*PI-SL*)
	     (return (simplify (cond ($polyfactor (cons '(mtimes) res))
				     ((cons '(mlist) (nreverse res)))))))) 

(defun cpoly-err (expr)
  (merror "ALLROOTS: not a polynomial:~%~M" expr))

(defun cpoly-sl (degree) 
       ((lambda (logbas infin smalno are mre xx yy cosr sinr cr ci sr si tr ti zr zi bnd
		 n polysc polysc1 conv) 
		(setq mre (*$ 2.0 (sqrt 2.0) are) yy (-$ xx))
		(do ((i degree (f1- i)))
		    ((not (and (zerop (aref *PR-SL* i)) (zerop (aref *PI-SL* i)))) (setq nn i n (f1- i))))
		(setq degree nn)
		(do ((i 0. (f1+ i)))
		    ((> i nn))
		    (store (aref *SHR-SL* i) (cmod-sl (aref *PR-SL* i) (aref *PI-SL* i))))
		(scale-sl )
		(do nil
		    ((> 2. nn)
		     (cdivid-sl (-$ (aref *PR-SL* 1.)) (-$ (aref *PI-SL* 1.)) (aref *PR-SL* 0.) (aref *PI-SL* 0.))
		     (store (aref *PR-SL* 1.) cr)
		     (store (aref *PI-SL* 1.) ci)
		     (setq nn 0.))
		    (do ((i 0. (f1+ i)))
			((> i nn))
			(store (aref *SHR-SL* i) (cmod-sl (aref *PR-SL* i) (aref *PI-SL* i))))
		    (setq bnd (cauchy-sl ))
		    (catch 'newroot
			   (do ((cnt1 1. (f1+ cnt1)))
			       ((> cnt1 2.))
			       (noshft-sl 5.)
			       (do ((cnt2 1. (f1+ cnt2)))
				   ((> cnt2 9.))
				   (setq xx (prog2 nil
						   (-$ (*$ cosr xx) (*$ sinr yy))
						   (setq yy (+$ (*$ sinr xx)
								(*$ cosr yy)))) 
					 sr (*$ bnd xx) 
					 si (*$ bnd yy))
				   (fxshft-sl (f* 10. cnt2))
				   (cond (conv (store (aref *PR-SL* nn) zr)
					       (store (aref *PI-SL* nn) zi)
					       (setq nn n n (f1- n))
					       (do ((i 0. (f1+ i)))
						   ((> i nn))
						   (store (aref *PR-SL* i) (aref *QPR-SL* i))
						   (store (aref *PI-SL* i) (aref *QPI-SL* i)))
					       (throw 'newroot t))))))
		    (or conv (return t)))
		(do ((i (f1+ nn) (f1+ i)))
		    ((> i degree))
		    (store (aref *PR-SL* i) (_f (aref *PR-SL* i) polysc1))
		    (store (aref *PI-SL* i) (_f (aref *PI-SL* i) polysc1)))
		(do ((i 0. (f1+ i)) (j (f- polysc (f* polysc1 degree)) (f+ j polysc1)))
		    ((> i nn))
		    (store (aref *PR-SL* i) (_f (aref *PR-SL* i) j))
		    (store (aref *PI-SL* i) (_f (aref *PI-SL* i) j)))
		nn)
	(log 2.0) (cpoly-large-flonum)
	(cpoly-small-flonum) (MAXIMA-float-precision acp-sl )
	0.0 0.70710677 0.0 -0.069756474 0.99756405
	0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0. 0. 0. nil)) 

(defun noshft-sl (l1) 
       (do ((i 0. (f1+ i)) (xni (float nn) (1-$ xni)) (t1 (//$ (float nn))))
	   ((> i n))
	   (store (aref *HR-SL* i) (*$ (aref *PR-SL* i) xni t1))
	   (store (aref *HI-SL* i) (*$ (aref *PI-SL* i) xni t1)))
       (do ((jj 1. (f1+ jj)))
	   ((> jj l1))
	   (cond ((> (cmod-sl (aref *HR-SL* n) (aref *HI-SL* n)) (*$ 10.0 are (cmod-sl (aref *PR-SL* n) (aref *PI-SL* n))))
		  (cdivid-sl (-$ (aref *PR-SL* nn)) (-$ (aref *PI-SL* nn)) (aref *HR-SL* n) (aref *HI-SL* n))
		  (setq tr cr ti ci)
		  (do ((j n (f1- j)) (t1) (t2))
		      ((> 1. j))
		      (setq t1 (aref *HR-SL* (f1- j)) t2 (aref *HI-SL* (f1- j)))
		      (store (aref *HR-SL* j) (-$ (+$ (aref *PR-SL* j) (*f t1 tr)) (*f t2 ti)))
		      (store (aref *HI-SL* j) (+$ (aref *PI-SL* j) (*f t1 ti) (*f t2 tr))))
		  (store (aref *HR-SL* 0.) (aref *PR-SL* 0.))
		  (store (aref *HI-SL* 0.) (aref *PI-SL* 0.)))
		 (t (do ((j n (f1- j)))
			((> 1. j))
			(store (aref *HR-SL* j) (aref *HR-SL* (f1- j)))
			(store (aref *HI-SL* j) (aref *HI-SL* (f1- j))))
		    (store (aref *HR-SL* 0.) 0.0)
		    (store (aref *HI-SL* 0.) 0.0))))) 

(defun fxshft-sl (l2) 
       ((lambda (test pasd otr oti svsr svsi bool pvr pvi) 
		(polyev-sl)
		(setq conv nil)
		(calct-sl)
		(do ((j 1. (f1+ j)))
		    ((> j l2))
		    (setq otr tr oti ti)
		    (nexth-sl)
		    (calct-sl)
		    (setq zr (+$ sr tr) zi (+$ si ti))
		    (cond ((and (not bool) test (not (= j l2)))
			   (cond ((> (*$ 0.5 (cmod-sl zr zi))
				     (cmod-sl (-$ tr otr) (-$ ti oti)))
				  (cond (pasd (do ((i 0. (f1+ i)))
						  ((> i n))
						  (store (aref *SHR-SL* i) (aref *HR-SL* i))
						  (store (aref *SHI-SL* i) (aref *HI-SL* i)))
					      (setq svsr sr svsi si)
					      (vrshft-sl 10.)
					      (and conv (return nil))
					      (setq test nil)
					      (do ((i 0. (f1+ i)))
						  ((> i n))
						  (store (aref *HR-SL* i) (aref *SHR-SL* i))
						  (store (aref *HI-SL* i) (aref *SHI-SL* i)))
					      (setq sr svsr si svsi)
					      (polyev-sl)
					      (calct-sl))
					((setq pasd t))))
				 ((setq pasd nil))))))
		(or conv (vrshft-sl 10.))
		nil)
	t nil 0.0 0.0 0.0 0.0 nil 0.0 0.0)) 

(defun vrshft-sl (l3) 
       (setq conv nil sr zr si zi)
       (do ((i 1. (f1+ i)) (bool1 nil) (mp) (ms) (omp) (relstp) (tp) (r1))
	   ((> i l3))
	   (polyev-sl)
	   (setq mp (cmod-sl pvr pvi) ms (cmod-sl sr si))
	   (cond ((> (*$ 20.0 (errev-sl ms mp)) mp)
		  (setq conv t zr sr zi si)
		  (return t)))
	   (cond ((= i 1.) (setq omp mp))
		 ((or bool1 (> omp mp) (not (< relstp 0.05)))
		  (cond ((> (*$ 0.1 mp) omp) (return t)) (t (setq omp mp))))
		 (t (setq tp relstp bool1 t)
		    (cond ((> are relstp) (setq tp are)))
		    (setq r1 (sqrt tp) 
			  sr (prog2 nil
				    (-$ (*$ (1+$ r1) sr) (*f r1 si))
				    (setq si (+$ (*$ (1+$ r1) si) (*f r1 sr)))))
		    (polyev-sl)
		    (do ((j 1. (f1+ j))) ((> j 5.)) (calct-sl) (nexth-sl))
		    (setq omp infin)))
	   (calct-sl)
	   (nexth-sl)
	   (calct-sl)
	   (or bool
	       (setq relstp (//$ (cmod-sl tr ti) (cmod-sl sr si)) 
		     sr (+$ sr tr) 
		     si (+$ si ti))))) 

(defun calct-sl nil 
       (do ((i 1. (f1+ i))
	    ($t)
	    (hvr (store (aref *QHR-SL* 0.) (aref *HR-SL* 0.)))
	    (hvi (store (aref *QHI-SL* 0.) (aref *HI-SL* 0.))))
	   ((> i n)
	    (setq bool (not (> (cmod-sl hvr hvi) (*$ 10.0 are (cmod-sl (aref *HR-SL* n) (aref *HI-SL* n))))))
	    (cond ((not bool) (cdivid-sl (-$ pvr) (-$ pvi) hvr hvi) (setq tr cr ti ci))
		  (t (setq tr 0.0 ti 0.0)))
	    nil)
	   (setq $t (-$ (+$ (aref *HR-SL* i) (*f hvr sr)) (*f hvi si)))
	   (store (aref *QHI-SL* i) (setq hvi (+$ (aref *HI-SL* i) (*f hvr si) (*f hvi sr))))
	   (store (aref *QHR-SL* i) (setq hvr $t)))) 

(defun nexth-sl nil 
       (cond (bool (do ((j 1. (f1+ j)))
		       ((> j n))
		       (store (aref *HR-SL* j) (aref *QHR-SL* (f1- j)))
		       (store (aref *HI-SL* j) (aref *QHI-SL* (f1- j))))
		   (store (aref *HR-SL* 0.) 0.0)
		   (store (aref *HI-SL* 0.) 0.0))
	     (t (do ((j 1. (f1+ j)) (t1) (t2))
		    ((> j n))
		    (setq t1 (aref *QHR-SL* (f1- j)) t2 (aref *QHI-SL* (f1- j)))
		    (store (aref *HR-SL* j) (-$ (+$ (aref *QPR-SL* j) (*f t1 tr)) (*f t2 ti)))
		    (store (aref *HI-SL* j) (+$ (aref *QPI-SL* j) (*f t1 ti) (*f t2 tr))))
		(store (aref *HR-SL* 0.) (aref *QPR-SL* 0.))
		(store (aref *HI-SL* 0.) (aref *QPI-SL* 0.))))
       nil) 

(defun polyev-sl nil 
       (setq pvr (store (aref *QPR-SL* 0.) (aref *PR-SL* 0.)) pvi (store (aref *QPI-SL* 0.) (aref *PI-SL* 0.)))
       (do ((i 1. (f1+ i)) ($t))
	   ((> i nn))
	   (setq $t (-$ (+$ (aref *PR-SL* i) (*f pvr sr)) (*f pvi si)))
	   (store (aref *QPI-SL* i) (setq pvi (+$ (aref *PI-SL* i) (*f pvr si) (*f pvi sr))))
	   (store (aref *QPR-SL* i) (setq pvr $t)))) 

(defun errev-sl (ms mp) 
       (-$ (*$ (do ((j 0. (f1+ j))
		    (e (//$ (*$ (cmod-sl (aref *QPR-SL* 0.) (aref *QPI-SL* 0.)) mre) (+$ are mre))))
		   ((> j nn) e)
		   (setq e (+$ (cmod-sl (aref *QPR-SL* j) (aref *QPI-SL* j)) (*$ e ms))))
	       (+$ are mre))
	   (*$ mp mre))) 

(defun cauchy-sl nil 
       ((lambda (x xm) 
		(store (aref *SHR-SL* nn) (-$ (aref *SHR-SL* nn)))
		(cond ((not (zerop (aref *SHR-SL* n)))
		       (setq xm (-$ (//$ (aref *SHR-SL* nn) (aref *SHR-SL* n))))
		       (cond ((> x xm) (setq x xm)))))
		(do ((f))
		    (nil)
		    (setq xm (*$ 0.1 x) f (aref *SHR-SL* 0.))
		    (do ((i 1. (f1+ i))) ((> i nn)) (setq f (+$ (aref *SHR-SL* i) (*f f xm))))
		    (cond ((not (< 0.0 f)) (return t)))
		    (setq x xm))
		(do ((dx x) (df) (f))
		    ((> 5.0e-3 (abs (//$ dx x))) x)
		    (setq f (aref *SHR-SL* 0.) df f)
		    (do ((i 1. (f1+ i)))
			((> i n))
			(setq f (+$ (*$ f x) (aref *SHR-SL* i)) df (+$ (*$ df x) f)))
		    (setq f (+$ (*$ f x) (aref *SHR-SL* nn)) dx (//$ f df) x (-$ x dx))))
	(exp (//$ (-$ (log (aref *SHR-SL* nn)) (log (aref *SHR-SL* 0.))) (float nn)))
	0.0)) 

(defun scale-sl nil
       (do ((i 0. (f1+ i)) (j 0.) (x 0.0) (dx 0.0))
	   ((> i nn)
	    (setq x (//$ x (float (f- (f1+ nn) j)))
		  dx (//$ (-$ (log (aref *SHR-SL* nn)) (log (aref *SHR-SL* 0.))) (float nn))
		  polysc1 (fix (+$ 0.5 (//$ dx logbas)))
		  x (+$ x (*$ (float (f* polysc1 nn)) logbas 0.5))
		  polysc (fix (+$ 0.5 (//$ x logbas)))))
	   (cond ((zerop (aref *SHR-SL* i)) (setq j (f1+ j)))
		 (t (setq x (+$ x (log (aref *SHR-SL* i)))))))
       (do ((i nn (f1- i)) (j (f- polysc) (f+ j polysc1)))
	   ((< i 0.))
	   (store (aref *PR-SL* i) (_f (aref *PR-SL* i) j))
	   (store (aref *PI-SL* i) (_f (aref *PI-SL* i) j))))

;; (defun scale-sl nil 
;;        ((lambda (hi lo max min x l) 
;; 		(do ((i 0. (f1+ i)))
;; 		    ((> i nn))
;; 		    (setq x (shr-sl i))
;; 		    (cond ((> x max) (setq max x)))
;; 		    (cond ((and (not (= x 0.0)) (< x min)) (setq min x))))
;; 		(cond ((or (> lo min) (> max hi))
;; 		       (setq x (//$ lo min))
;; 		       (cond ((> x 1.0)
;; 			      (cond ((> max (//$ infin x))
;; 				     ;;;acm has < here but imsl agrees with me
;; 				     (setq x 1.0))))
;; 			     ((setq x (//$ (*$ (sqrt max) (sqrt min))))))
;; 		       (setq l (fix (+$ 0.5 (//$ (log x) logbas))))
;; 		       (cond ((not (= l 0.))
;; 			      (do ((i 0. (f1+ i)))
;; 				  ((> i nn))
;; 				  (store (pr-sl i) (_f (pr-sl i) l))
;; 				  (store (pi-sl i) (_f (pi-sl i) l)))))))
;; 		l)
;; 	(sqrt infin) (//$ smalno are) 0.0 infin 0.0 0.))

(defun cdivid-sl (ar ai br bi) 
       ((lambda (r1) (cond ((and (zerop br) (zerop bi)) (setq cr (setq ci infin)))
			   ((> (abs bi) (abs br))
			    (setq r1 (//f br bi) 
				  bi (+$ bi (*f br r1)) 
				  br (+$ ai (*f ar r1)) 
				  cr (//f br bi) 
				  br (-$ (*f ai r1) ar) 
				  ci (//f br bi)))
			   ((setq r1 (//f bi br) 
				  bi (+$ br (*f bi r1)) 
				  br (+$ ar (*f ai r1)) 
				  cr (//f br bi) 
				  br (-$ ai (*f ar r1)) 
				  ci (//f br bi)))))
	0.0)
       nil) 

(defun cmod-sl (ar ai) 
       (setq ar (abs ar) ai (abs ai))
       (cond ((> ai ar) (setq ar (//f ar ai)) (*$ ai (sqrt (1+$ (*f ar ar)))))
	     ((> ar ai) (setq ai (//f ai ar)) (*$ ar (sqrt (1+$ (*f ai ai)))))
	     ((*$ 1.41421357 ar)))) 

;;*page

;;;this is the algorithm for doing real polynomials.  it is algorithm 493 from
;;;acm toms vol 1 p 178 (1975) by jenkins.  note that array indexing starts from 0.
;;;the names of the arrays have been changed to be the same as for cpoly.
;;;the correspondence is:  p - pr-sl, qp - qpr-sl, k - hr-sl, qk - qhr-sl, svk - shr-sl,
;;;temp - shi-sl.  the roots are put in pr-sl and pi-sl.
;;;the variable si appears not to be used here

(declare-top(special sr u v a b c d a1 a3 a7 e f g h szr szi lzr lzi are mre n nn nz
		  type ui vi s $polyfactor arp-sl)
	 (flonum a a0 a1 a3 a4 a5 a6 a7 aa are b b0 b1 b2 logbas bb betas betav bnd c c0
		 c1 c2 c3 c4 cc cosr d d0 e ee f g h infin kv lzi lzr mp mre ms omp
		 oss ots otv ovv pv relstp s sinr smalno sr ss svu svv szi szr t1 ts
		 tss tv tvv u ui v vi vv xx yy zm arp-sl)
	 (fixnum cnt degree i iflag j jj l l2 n nn nz type)) 

(declare-top(fixnum (realit-sl))
	 (notype (rpoly-sl fixnum) (fxshfr-sl fixnum) (quadit-sl) (calcsc-sl) (nextk-sl)
		 (newest-sl) (quadsd-sl) (quad-sl flonum flonum flonum))) 

(setq arp-sl 1.0) 

(defun rpoly-sl (degree) 
       ((lambda (logbas infin smalno are mre xx yy cosr sinr aa cc bb bnd sr u v t1 szr
		 szi lzr lzi nz n polysc polysc1 zerok conv1) 
		(setq mre are yy (-$ xx))
		(do ((i degree (f1- i))) ((not (zerop (aref *PR-SL* i))) (setq nn i n (f1- i))))
		(setq degree nn)
		(do ((i 0. (f1+ i))) ((> i nn)) (store (aref *SHR-SL* i) (abs (aref *PR-SL* i))))
		(scale-sl)
		(do nil
		    ((< nn 3.)
		     (cond ((= nn 2.)
			    (quad-sl (aref *PR-SL* 0.) (aref *PR-SL* 1.) (aref *PR-SL* 2.))
			    (cond ((and $polyfactor (not (zerop szi)))
				   (store (aref *PR-SL* 2.) (//$ (aref *PR-SL* 2.) (aref *PR-SL* 0.)))
				   (store (aref *PR-SL* 1.) (//$ (aref *PR-SL* 1.) (aref *PR-SL* 0.)))
				   (store (aref *PI-SL* 2.) 1.0))
				  (t (store (aref *PR-SL* 2.) szr)
				     (store (aref *PI-SL* 2.) szi)
				     (store (aref *PR-SL* 1.) lzr)
				     (store (aref *PI-SL* 1.) lzi))))
			   (t (store (aref *PR-SL* 1.) (-$ (//$ (aref *PR-SL* 1.) (aref *PR-SL* 0.))))))
		     (setq nn 0.))
		    (do ((i 0. (f1+ i))) ((> i nn)) (store (aref *SHR-SL* i) (abs (aref *PR-SL* i))))
		    (setq bnd (cauchy-sl))
		    (do ((i 1. (f1+ i)))
			((> i n))
			(store (aref *HR-SL* i) (//$ (*$ (float (f- n i)) (aref *PR-SL* i)) (float n))))
		    (store (aref *HR-SL* 0.) (aref *PR-SL* 0.))
		    (setq aa (aref *PR-SL* nn) bb (aref *PR-SL* n) zerok (zerop (aref *HR-SL* n)))
		    (do ((jj 1. (f1+ jj)))
			((> jj 5.))
			(setq cc (aref *HR-SL* n))
			(cond (zerok (do ((j n (f1- j)))
					 ((< j 1.))
					 (store (aref *HR-SL* j) (aref *HR-SL* (f1- j))))
				     (store (aref *HR-SL* 0.) 0.0)
				     (setq zerok (zerop (aref *HR-SL* n))))
			      (t (setq t1 (-$ (//$ aa cc)))
				 (do ((j n (f1- j)))
				     ((< j 1.))
				     (store (aref *HR-SL* j) (+$ (*$ t1 (aref *HR-SL* (f1- j))) (aref *PR-SL* j))))
				 (store (aref *HR-SL* 0.) (aref *PR-SL* 0.))
				 (setq zerok (not (> (abs (aref *HR-SL* n))
						     (*$ (abs bb) are 10.0)))))))
		    (do ((i 0. (f1+ i))) ((> i n)) (store (aref *SHI-SL* i) (aref *HR-SL* i)))
		    (do ((cnt 1. (f1+ cnt)))
			((> cnt 20.) (setq conv1 nil))
			(setq xx (prog2 nil
					(-$ (*$ cosr xx) (*$ sinr yy))
					(setq yy (+$ (*$ sinr xx) (*$ cosr yy)))) 
			      sr (*$ bnd xx) 
			      u (*$ -2.0 sr) 
			      v bnd)
			(fxshfr-sl (f* 20. cnt))
			(cond ((> nz 0.)
			       (store (aref *PR-SL* nn) szr)
			       (store (aref *PI-SL* nn) szi)
			       (cond ((= nz 2.)
				      (store (aref *PR-SL* n) lzr)
				      (store (aref *PI-SL* n) lzi)
				      (cond ((and $polyfactor (not (zerop szi)))
					     (store (aref *PR-SL* nn) v)
					     (store (aref *PR-SL* n) u)
					     (store (aref *PI-SL* nn) 1.0)))))
			       (setq nn (f- nn nz) n (f1- nn))
			       (do ((i 0. (f1+ i))) ((> i nn)) (store (aref *PR-SL* i) (aref *QPR-SL* i)))
			       (return nil)))
			(do ((i 0. (f1+ i))) ((> i n)) (store (aref *HR-SL* i) (aref *SHI-SL* i))))
		    (or conv1 (return nil)))
		(cond ($polyfactor
		       (do ((i degree (f1- i)))
			   ((= i nn))
			   (cond ((zerop (aref *PI-SL* i))
				  (store (aref *PR-SL* i) (_f (aref *PR-SL* i) polysc1)))
				 (t (store (aref *PR-SL* i) (_f (aref *PR-SL* i) (f* 2. polysc1)))
				    (setq i (f1- i))
				    (store (aref *PR-SL* i) (_f (aref *PR-SL* i) polysc1))))))
		      (t (do ((i (f1+ nn) (f1+ i)))
			     ((> i degree))
			     (store (aref *PR-SL* i) (_f (aref *PR-SL* i) polysc1))
			     (store (aref *PI-SL* i) (_f (aref *PI-SL* i) polysc1)))))
		(do ((i 0. (f1+ i)) (j (f- polysc (f* polysc1 degree)) (f+ j polysc1)))
		    ((> i nn))
		    (store (aref *PR-SL* i) (_f (aref *PR-SL* i) j))))
	(log 2.0) (cpoly-large-flonum)
	(cpoly-small-flonum) (MAXIMA-float-precision arp-sl)
	0.0 0.70710677 0.0 -0.069756474 0.99756405
	0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0. 0. 0. 0. 0. t))  

(defun fxshfr-sl (l2) 
       ((lambda (type a b c d e f g h a1 a3 a7) 
	 (setq nz 0.)
	 (quadsd-sl )
	 (calcsc-sl )
	 (do ((j 1. (f1+ j)) (betav 0.25) (betas 0.25)
	      (oss sr) (ovv v) (tvv) (tss) (ss) (vv) (tv) (ts) (ots) (otv)
	      (ui) (vi) (s) (svv) (svu) (iflag) (vpass) (spass) (vtry) (stry))
	     ((> j l2))
	     (nextk-sl )
	     (calcsc-sl )
	     (newest-sl )
	     (setq vv vi ss 0.0)
	     (or (zerop (aref *HR-SL* n)) (setq ss (-$ (//$ (aref *PR-SL* nn) (aref *HR-SL* n)))))
	     (setq tv 1.0 ts 1.0)
	     (cond ((not (or (= j 1.) (= type 3.)))
		    (or (zerop vv) (setq tv (abs (//$ (-$ vv ovv) vv))))
		    (or (zerop ss ) (setq ts (abs (//$ (-$ ss oss) ss))))
		    (setq tvv 1.0)
		    (and (< tv otv) (setq tvv (*$ tv otv)))
		    (setq tss 1.0)
		    (and (< ts ots) (setq tss (*$ ts ots)))
		    (setq vpass (< tvv betav) spass (< tss betas))
		    (cond ((or spass vpass)
			   (setq svu u svv v)
			   (do ((i 0. (f1+ i))) ((> i n)) (store (aref *SHR-SL* i) (aref *HR-SL* i)))
			   (setq s ss vtry nil stry nil)
			   (and (do ((bool (not (and spass
						     (or (not vpass) (< tss tvv))))
					   t)
				     (l50 nil nil))
				    (nil)
				    (cond (bool (quadit-sl )
						(and (> nz 0.) (return t))
						(setq vtry t betav (*$ 0.25 betav))
						(cond ((or stry (not spass))
						       (setq l50 t))
						      (t (do ((i 0. (f1+ i)))
							     ((> i n))
							     (store (aref *HR-SL* i)
								    (aref *SHR-SL* i)))))))
				    (cond ((not l50)
					   (setq iflag (realit-sl ))
					   (and (> nz 0.) (return t))
					   (setq stry t betas (*$ 0.25 betas))
					   (cond ((zerop iflag) (setq l50 t))
						 (t (setq ui (-$ (+$ s s)) 
							  vi (*$ s s))))))
				    (cond (l50 (setq u svu v svv)
					       (do ((i 0. (f1+ i)))
						   ((> i n))
						   (store (aref *HR-SL* i) (aref *SHR-SL* i)))
					       (and (or (not vpass) vtry)
						    (return nil)))))
				(return nil))
			   (quadsd-sl )
			   (calcsc-sl )))))
	     (setq ovv vv oss ss otv tv ots ts)))
	0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)) 

(defun quadit-sl nil 
       (setq nz 0. u ui v vi)
       (do ((tried) (j 0.) (ee) (zm) (t1) (mp) (relstp) (omp))
	   (nil)
	   (quad-sl 1.0 u v)
	   (and (> (abs (-$ (abs szr) (abs lzr))) (*$ 0.01 (abs lzr))) (return nil))
	   (quadsd-sl )
	   (setq mp (+$ (abs (-$ a (*$ szr b))) (abs (*$ szi b))) 
		 zm (sqrt (abs v)) 
		 ee (*$ 2.0 (abs (aref *QPR-SL* 0.))) 
		 t1 (-$ (*$ szr b)))
	   (do ((i 1. (f1+ n))) ((> i n)) (setq ee (+$ (*$ ee zm) (abs (aref *QPR-SL* i)))))
	   (setq ee (+$ (*$ ee zm) (abs (+$ a t1))) 
		 ee (-$ (*$ (+$ (*$ 5.0 mre) (*$ 4.0 are)) ee)
			(*$ (+$ (*$ 5.0 mre) (*$ 2.0 are))
			    (+$ (abs (+$ a t1)) (*$ (abs b) zm)))
			(*$ -2.0 are (abs t1))))
	   (cond ((not (> mp (*$ 20.0 ee))) (setq nz 2.) (return nil)))
	   (setq j (f1+ j))
	   (and (> j 20.) (return nil))
	   (cond ((not (or (< j 2.) (> relstp 0.01) (< mp omp) tried))
		  (and (< relstp are) (setq relstp are))
		  (setq relstp (sqrt relstp) 
			u (-$ u (*$ u relstp)) 
			v (+$ v (*$ v relstp)))
		  (quadsd-sl )
		  (do ((i 1. (f1+ i))) ((> i 5.)) (calcsc-sl ) (nextk-sl ))
		  (setq tried t j 0.)))
	   (setq omp mp)
	   (calcsc-sl )
	   (nextk-sl )
	   (calcsc-sl )
	   (newest-sl )
	   (and (zerop vi) (return nil))
	   (setq relstp (abs (//$ (-$ vi v) vi)) u ui v vi))) 

(defun realit-sl nil 
       (setq nz 0.)
       (do ((j 0.) (pv) (ee) (ms) (mp) (kv) (t1) (omp))
	   (nil)
	   (setq pv (aref *PR-SL* 0.))
	   (store (aref *QPR-SL* 0.) pv)
	   (do ((i 1. (f1+ i)))
	       ((> i nn))
	       (setq pv (+$ (*$ pv s) (aref *PR-SL* i)))
	       (store (aref *QPR-SL* i) pv))
	   (setq mp (abs pv) ms (abs s) ee (*$ (//$ mre (+$ are mre)) (abs (aref *QPR-SL* 0.))))
	   (do ((i 1. (f1+ i))) ((> i nn)) (setq ee (+$ (*$ ee ms) (abs (aref *QPR-SL* i)))))
	   (cond ((not (> mp (*$ 20.0 (-$ (*$ (+$ are mre) ee) (*$ mre mp)))))
		  (setq nz 1. szr s szi 0.0)
		  (return 0.)))
	   (setq j (f1+ j))
	   (and (> j 10.) (return 0.))
	   (cond ((not (or (< j 2.)
			   (> (abs t1) (*$ 1.0e-3 (abs (-$ s t1))))
			   (not (> mp omp))))
		  (return 1.)))
	   (setq omp mp kv (aref *HR-SL* 0.))
	   (store (aref *QHR-SL* 0.) kv)
	   (do ((i 1. (f1+ i)))
	       ((> i n))
	       (setq kv (+$ (*$ kv s) (aref *HR-SL* i)))
	       (store (aref *QHR-SL* i) kv))
	   (cond ((> (abs kv) (*$ (abs (aref *HR-SL* n)) 10.0 are))
		  (setq t1 (-$ (//$ pv kv)))
		  (store (aref *HR-SL* 0.) (aref *QPR-SL* 0.))
		  (do ((i 1. (f1+ i)))
		      ((> i n))
		      (store (aref *HR-SL* i) (+$ (*$ t1 (aref *QHR-SL* (f1- i))) (aref *QPR-SL* i)))))
		 (t (store (aref *HR-SL* 0.) 0.0)
		    (do ((i 1. (f1+ i))) ((> i n)) (store (aref *HR-SL* i) (aref *QHR-SL* (f1- i))))))
	   (setq kv (aref *HR-SL* 0.))
	   (do ((i 1. (f1+ i))) ((> i n)) (setq kv (+$ (*$ kv s) (aref *HR-SL* i))))
	   (setq t1 0.0)
	   (and (> (abs kv) (*$ (abs (aref *HR-SL* n)) 10.0 are)) (setq t1 (-$ (//$ pv kv))))
	   (setq s (+$ s t1)))) 

(defun calcsc-sl nil 
       (setq d (aref *HR-SL* 0.))
       (store (aref *QHR-SL* 0.) d)
       (setq c (-$ (aref *HR-SL* 1.) (*$ u d)))
       (store (aref *QHR-SL* 1.) c)
       (do ((i 2. (f1+ i)) (c0))
	   ((> i n))
	   (setq c0 (-$ (aref *HR-SL* i) (*$ u c) (*$ v d)))
	   (store (aref *QHR-SL* i) c0)
	   (setq d c c c0))
       (cond ((not (or (> (abs c) (*$ (abs (aref *HR-SL* n)) 100.0 are))
		       (> (abs d) (*$ (abs (aref *HR-SL* (f1- n))) 100.0 are))))
	      (setq type 3.))
	     ((not (< (abs d) (abs c)))
	      (setq type 2. 
		    e (//$ a d) 
		    f (//$ c d) 
		    g (*$ u b) 
		    h (*$ v b) 
		    a3 (+$ (*$ (+$ a g) e) (*$ h (//$ b d))) 
		    a1 (-$ (*$ b f) a) 
		    a7 (+$ (*$ (+$ f u) a) h)))
	     (t (setq type 1. 
		      e (//$ a c) 
		      f (//$ d c) 
		      g (*$ u e) 
		      h (*$ v b) 
		      a3 (+$ (*$ a e) (*$ (+$ (//$ h c) g) b)) 
		      a1 (-$ b (*$ a (//$ d c))) 
		      a7 (+$ a (*$ g d) (*$ h f)))))
       nil) 

(defun nextk-sl nil 
       (cond ((= type 3.)
	      (store (aref *HR-SL* 0.) 0.0)
	      (store (aref *HR-SL* 1.) 0.0)
	      (do ((i 2. (f1+ i))) ((> i n)) (store (aref *HR-SL* i) (aref *QHR-SL* (f- i 2.)))))
	     ((> (abs a1) (*$ (abs (cond ((= type 1.) b) (a))) 10.0 are))
	      (setq a7 (//$ a7 a1) a3 (//$ a3 a1))
	      (store (aref *HR-SL* 0.) (aref *QPR-SL* 0.))
	      (store (aref *HR-SL* 1.) (-$ (aref *QPR-SL* 1.) (*$ a7 (aref *QPR-SL* 0.))))
	      (do ((i 2. (f1+ i)))
		  ((> i n))
		  (store (aref *HR-SL* i)
			 (+$ (*$ a3 (aref *QHR-SL* (f- i 2.)))
			     (-$ (*$ a7 (aref *QPR-SL* (f1- i))))
			     (aref *QPR-SL* i)))))
	     (t (store (aref *HR-SL* 0.) 0.0)
		(store (aref *HR-SL* 1.) (-$ (*$ a7 (aref *QPR-SL* 0.))))
		(do ((i 2. (f1+ i)))
		    ((> i n))
		    (store (aref *HR-SL* i)
			   (-$ (*$ a3 (aref *QHR-SL* (f- i 2.))) (*$ a7 (aref *QPR-SL* (f1- i))))))))
       nil) 

(defun newest-sl nil 
       ((lambda (a4 a5 b1 b2 c1 c2 c3 c4) 
		(cond ((= type 3.) (setq ui 0.0 vi 0.0))
		      (t (cond ((= type 2.)
				(setq a4 (+$ (*$ (+$ a g) f) h) 
				      a5 (+$ (*$ (+$ f u) c) (*$ v d))))
			       (t (setq a4 (+$ a (*$ u b) (*$ h f)) 
					a5 (+$ c (*$ (+$ u (*$ v f)) d)))))
			 (setq b1 (-$ (//$ (aref *HR-SL* n) (aref *PR-SL* nn))) 
			       b2 (-$ (//$ (+$ (aref *HR-SL* (f1- n)) (*$ b1 (aref *PR-SL* n))) (aref *PR-SL* nn))) 
			       c1 (*$ v b2 a1) 
			       c2 (*$ b1 a7) 
			       c3 (*$ b1 b1 a3) 
			       c4 (-$ c1 c2 c3) 
			       c1 (+$ a5 (*$ b1 a4) (-$ c4)))
			 (cond ((zerop c1) (setq ui 0.0 vi 0.0))
			       (t (setq ui (-$ u
					       (//$ (+$ (*$ u (+$ c3 c2))
							(*$ v
							    (+$ (*$ b1 a1)
								(*$ b2 a7))))
						    c1)) 
					vi (*$ v (1+$ (//$ c4 c1))))))))
		nil)
	0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)) 

(defun quadsd-sl nil 
       (setq b (aref *PR-SL* 0.))
       (store (aref *QPR-SL* 0.) b)
       (setq a (-$ (aref *PR-SL* 1.) (*$ u b)))
       (store (aref *QPR-SL* 1.) a)
       (do ((i 2. (f1+ i)) (c0))
	   ((> i nn))
	   (setq c0 (-$ (aref *PR-SL* i) (*$ u a) (*$ v b)))
	   (store (aref *QPR-SL* i) c0)
	   (setq b a a c0))) 

(defun quad-sl (a0 b1 c0) 
       (setq szr 0.0 szi 0.0 lzr 0.0 lzi 0.0)
       ((lambda (b0 d0 e) 
		(cond ((zerop a0 ) (or (zerop b1 ) (setq szr (-$ (//$ c0 b1)))))
		      ((zerop c0 ) (setq lzr (-$ (//$ b1 a0))))
		      (t (setq b0 (//$ b1 2.0))
			 (cond ((< (abs b0) (abs c0))
				(setq e a0)
				(and (< c0 0.0) (setq e (-$ a0)))
				(setq e (-$ (*$ b0 (//$ b0 (abs c0))) e) 
				      d0 (*$ (sqrt (abs e)) (sqrt (abs c0)))))
			       (t (setq e (-$ 1.0 (*$ (//$ a0 b0) (//$ c0 b0))) 
					d0 (*$ (sqrt (abs e)) (abs b0)))))
			 (cond ((< e 0.0)
				(setq szr (-$ (//$ b0 a0)) 
				      lzr szr 
				      szi (abs (//$ d0 a0)) 
				      lzi (-$ szi)))
			       (t (or (< b0 0.0) (setq d0 (-$ d0)))
				  (setq lzr (//$ (-$ d0 b0) a0))
				  (or (zerop lzr ) (setq szr (//$ (//$ c0 lzr) a0)))))))
		nil)
	0.0 0.0 0.0)) 

#-NIL
(declare-top(unspecial logbas infin smalno are mre cr ci sr si tr ti zr zi
		    n nn bool conv pvr pvi acp-sl polysc polysc1 sr u v a
		    b c d a1 a3 a7 e f g h szr szi lzr lzi are mre n nn nz
		    type ui vi s arp-sl ))

(declare-top(unspecial logbas infin smalno are mre cr ci sr si tr ti zr zi
		    n nn bool conv pvr pvi acp-sl polysc polysc1 sr u v a
		    b c d a1 a3 a7 e f g h szr szi lzr lzi are mre n nn nz
		    type ui vi s arp-sl))

