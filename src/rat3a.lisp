;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module rat3a)

;; This is the new Rational Function Package Part 1.
;; It includes most of the coefficient and polynomial routines required
;; by the rest of the functions.  Certain specialized functions are found
;; elsewhere.  Most low-level utility programs are also in this file.

(declare-top(special u* *a* *var* *x* v*)
	    (genprefix a_1))

(declare-top (unspecial coef var exp p y ))  
;;These do not need to be special for this file and they
;;slow it down on lispm. We also eliminated the special
;;from ptimes2--wfs

(load-macsyma-macros ratmac)

;; Global variables referenced throughout the rational function package.

(defmvar modulus nil "Global switch for doing modular arithmetic")
(defmvar hmodulus nil "Half of `modulus'")
(defmvar errrjfflag nil "Controls action of `errrjf' (`maxima-error' or throw)")

;;(DEFsubst POINTERGP (A B) (> (VALGET A) (VALGET B))) ;;better a macro in ratmac.

(defmacro bctimes (&rest l)
  `(remainder (times . ,l) modulus))

(defun cquotient (a b)
  (cond ((eqn a 0) 0)
	((null modulus)
	 (cond ((eqn 0 (cremainder a b)) (quotient a b))
	       (t (errrjf "quotient is not exact"))))
	(t (ctimes a (crecip b)))))

(defun alg (l) (and $algebraic (not (atom l)) (get (car l) 'tellrat)))

(defun pacoefp (x) (or (pcoefp x) (alg x)))

(defun leadterm (poly)
  (cond ((pcoefp poly) poly)
	(t (make-poly (p-var poly) (p-le poly) (p-lc poly)))))

;; (DEFUN LCF (POLY)
;;        (COND ((PCOEFP POLY) POLY)
;; 	     (T (P-LC POLY))))

;; (DEFUN LEXP (POLY) (COND ((PCOEFP POLY) 0) (T (P-LE POLY))))

(defun cremainder (a b)
  (cond ((or modulus (floatp a) (floatp b)) 0) 
	((remainder a b))))

;;(DEFUN CBEXPT (P N)
;;  (DO ((N (ASH N -1) (ASH N -1)) (S (IF (LOGTEST N 1) P 1)))
;;      ((ZEROP N) S)
;;    (SETQ P (BCTIMES P P))
;;    (AND (ODDP N) (SETQ S (BCTIMES S P)))))

(defun cbexpt (p n)
  (do ((n (quotient n 2) (quotient n 2))
       (s (cond ((oddp n) p) (t 1))))
      ((zerop n) s)
    (setq p (bctimes p p))
    (and (oddp n) (setq s (bctimes s p)))))


#+pdp10
(defun cdifference (x y) (cplus x (cminus y)))

;; Coefficient Arithmetic -- coefficients are assumed to be something
;; that is NUMBERP in lisp.  If MODULUS is non-NIL, then all coefficients
;; are assumed to be less than its value.  Some functions use LOGAND
;; when MODULUS = 2.  This will not work with bignum coefficients.

;; The following five routines have been hand coded in RAT;RATLAP >.  Please
;; do not delete these definitions.  They are needed for Macsymas which run
;; on non-PDP10 systems

#-(or pdp10 cl)
(progn 'compile

       (defun cmod (n)
	 (cond ((null modulus) n)
	       ((bigp modulus)
		(setq n (remainder n modulus))
		(cond ((lessp n 0)
		       (if (lessp (times 2 n) (minus modulus))
			   (setq n (plus n modulus))))
		      ((greaterp (times 2 n) modulus) (setq n (difference n modulus)))) 
		n)
	       ((bigp n)
		(setq n (remainder n modulus))
		(cond ((f< n 0)
		       (if (< n (f- (lsh modulus -1))) (setq n (f+ n modulus))))
		      ((> n (lsh modulus -1)) (setq n (f- n modulus))))
		n)
	       ((= modulus 2) (logand n 1))
	       (t (let ((nn (fixnum-remainder n modulus)))
		    (declare (fixnum nn))
		    (cond ((f< nn 0)
			   (and (f< nn (f- (ash modulus -1))) (setq nn (f+ nn modulus))))
			  ((f> nn (ash modulus -1)) (setq nn (f- nn modulus))))
		    nn))))

       (defmfun cplus (a b)
	 (cond ((null modulus) (plus a b))
	       ((bigp modulus) (cmod (plus a b)))
	       ((= modulus 2) (logand 1 (logxor a b)))
	       (t (let ((nn (fixnum-remainder (f+ a b) modulus)))
		    (declare (fixnum nn))
		    ;; This is an efficiency hack which assumes that MINUSP is
		    ;; faster than <.  The first clause could simply be
		    ;; ((< NN (f- (LSH MODULUS -1))) (f+ NN MODULUS))
		    (cond ((minusp nn)
			   (if (f< nn (f- (ash modulus -1))) (f+ nn modulus) nn))
			  ((f> nn (ash modulus -1)) (f- nn modulus))
			  (t nn))))))

       (defun cdifference (a b)
	 (cond ((null modulus) (difference a b))
	       ((bigp modulus) (cmod (difference a b)))
	       ((= modulus 2) (logand 1 (logxor a b)))

	       (t (let ((nn (fixnum-remainder (f- a b) modulus)))
		    ;;Note: the above code is invalid since the difference
		    ;;of two fixnum's may well be a bignum.
		    ;;now if they were already in the -Modulus/2 <=x < Modulus/2
		    ;;range then this would be ok.
		    (declare (fixnum nn))
		    ;; This is an efficiency hack which assumes that MINUSP is
		    ;; faster than <.  The first clause could simply be
		    ;; ((< NN (f- (ASH MODULUS -1))) (f+ NN MODULUS))
		    (cond ((minusp nn)
			   (if (f< nn (f- (ash modulus -1))) (f+ nn modulus) nn))
			  ((f> nn (ash modulus -1)) (f- nn modulus))
			  (t nn))))))

       ;; (*DBLREM A B M) computes (A * B)/M in one swell foop so as not to
       ;; cons up a bignum intermediate for A*B.  MODULUS is known to be a fixnum
       ;; at the point where the call occurs, and thus A and B are known to be
       ;; fixnums.

       #-cl
       (declare-top(fixnum (*dblrem fixnum fixnum fixnum)))

       #+nil
       (defun *dblrem (a b m)
					;(REMAINDER (TIMES A B) M)
					;(si:%dblrem a b c d) is (\ (f+ (f* a b) c) d).
					;This should probably be open-compiled...  Later, when we know it works.
					; Anyway, there is no interpreted si:%dblrem function, only the compiler's
					; hack.
	 (si:%dblrem a b 0 m))

       #+cl
       (defun *dblrem (a b m)
	 ;; remind me to put the required primitives into NIL. -GJC
	 (remainder (times a b) m))

       #+nil
       (defun *dblrem (a b m)
	 ;; This generates the EMUL and EDIV instructions.
	 (compiler:%dblrem a b 0 m))

       (defmfun ctimes (a b)
	 (cond ((null modulus) (times a b))
	       ((bigp modulus) (cmod (times a b)))
	       ((= modulus 2) (logand 1 a b))
	       (t (let ((nn (*dblrem a b modulus)))
		    (declare (fixnum nn))
		    (cond ((minusp nn)
			   (if (f< nn (f- (ash modulus -1))) (f+ nn modulus) nn))
			  ((f> nn (ash modulus -1)) (f- nn modulus))
			  (t nn)))))))

;; Takes the inverse of an integer N mod P.  Solve N*X + P*Y = 1 
;; I suspect that N is guaranteed to be less than P, since in the case
;; where P is a fixnum, N is also assumed to be one.

(defun invmod (n modulus) (crecip n))

#-lispm
(defmfun crecip (n)
  (cond
    ;; Have to use bignum arithmetic if modulus is a bignum
    ((bigp modulus) 
     (prog (a1 a2 y1 y2 q (big-n n))
	(if (minusp big-n) (setq big-n (plus big-n modulus)))
	(setq a1 modulus a2 big-n)
	(setq y1 0 y2 1)
	(go step3)
	step2 (setq q (quotient a1 a2))
	(psetq a1 a2 a2 (difference a1 (times a2 q)))
	(psetq y1 y2 y2 (difference y1 (times y2 q)))
	step3 (cond ((zerop a2) (merror "Inverse of zero divisor?"))
		    ((not (equal a2 1)) (go step2)))
	(return (cmod y2))))
    ;; Here we can use fixnum arithmetic
    (t (prog ((a1 0) (a2 0) (y1 0) (y2 0) (q 0) (nn 0) (pp 0))
	  (declare (fixnum a1 a2 y1 y2 q nn pp))
	  (setq nn n pp modulus)
	  (cond ((minusp nn) (setq nn (f+ nn pp))))
	  (setq a1 pp a2 nn)
	  (setq y1 0 y2 1)
	  (go step3)
	  step2 (setq q (// a1 a2))
	  (psetq a1 a2 a2 (fixnum-remainder a1 a2))
	  (psetq y1 y2 y2 (f- y1 (f* y2 q)))
	  step3 (cond ((f= a2 0) (merror "Inverse of zero divisor?"))
		      ((not (f= a2 1)) (go step2)))
	  ;; Is there any reason why this can't be (RETURN (CMOD Y2)) ? -cwh
	  (return  (cmod y2)
		   ;;                    (COND ((= PP 2) (LOGAND 1 Y2))
		   ;;			   (T (LET ((NN (fixnum-remainder Y2 PP)))
		   ;;				(DECLARE (FIXNUM NN))
		   ;;				(COND ((MINUSP NN)
		   ;;				       (AND (f< NN (f- (ASH PP -1)))
		   ;;					    (SETQ NN (f+ NN PP))))
		   ;;				      ((f> NN (ASH PP -1))
		   ;;				       (SETQ NN (f- NN PP))))
		   ;;				NN)))
		   )
	  ))))

(defun cexpt (n e)
  (cond	((null modulus) (expt n e))
	(t (cmod (cbexpt n e)))))

;; End of non-PDP10 definitions of CMOD, CPLUS, CDIFFERENCE, CTIMES, CRECIP, CEXPT.



;; Notice how much cleaner things are on the Lisp Machine.
;; @@@@@ This definition may be better for NIL also, as long as the
;; arithmetic is hacked to not use the shadowed maclisp-fixnum functions,
;; = changes to eql, and (< n 0) -> (minusp n).
#+lispm
(defun crecip (n &aux (a1 modulus) a2 (y1 0) (y2 1) q)
  (setq a2 (if (< n 0) (+ n modulus) n))
  (do ()
      ((= a2 1) (cmod y2))
    (if (= a2 0)
	(error "Inverse of zero divisor -- ~D modulo ~D" n modulus))
    (setq q (// a1 a2))
    (psetq a1 a2 a2 (- a1 (* a2 q)))
    (psetq y1 y2 y2 (- y1 (* y2 q)))))

;;the following definitions are ok for 3600 and more transparent
;;and quicker.  Note for kcl, we provide some c definitions.

#+cl
#-kcl
(progn
  (defmacro mcmod (n) ;;; gives answers from -modulus/2 ...0 1 2 +modulus/2
    `(let ((.n. (mod ,n modulus)))
      (cond ((<= (times 2 .n.) modulus) .n.)
	    (t 
	     (difference .n. modulus)))))


  (defun cplus (a b)
    (cond ((null modulus)(plus a b))
	  (t (mcmod (plus a b)))))

  (defun cmod (n)
    (cond ((null modulus ) n)
	  (t (mcmod n))))

  (defun ctimes (a b)
    (cond ((null modulus) (times a b))
	  (t (mcmod (times a b)))))

  (defun cdifference (a b)
    (cond ((null modulus) (- a b))
	  (t (mcmod (- a b)))))

  )




(defun setqmodulus (m)
  (cond ((numberp m)
	 (cond ((greaterp m 0)
		(setq hmodulus (quotient m 2))
		(setq modulus m))
	       (t (merror "Modulus must be a number > 0"))))
	(t (setq hmodulus (setq modulus nil)))))

(defmfun pcoefadd (e c x)
  (cond ((pzerop c) x)
	((bigp e) (merror "Exponent out of range"))
	(t (cons e (cons c x)))))

(defmfun pplus (x y)
  (cond ((pcoefp x) (pcplus x y))
	((pcoefp y) (pcplus y x))
	((eq (p-var x) (p-var y))
	 (psimp (p-var x) (pplus1 (p-terms y) (p-terms x))))
	((pointergp (p-var x) (p-var y))
	 (psimp (p-var x) (pcplus1 y (p-terms x))))
	(t (psimp (p-var y) (pcplus1 x (p-terms y))))))

(defun pplus1 (x y)
  (cond ((ptzerop x) y)
	((ptzerop y) x)
	((= (pt-le x) (pt-le y))
	 (pcoefadd (pt-le x)
		   (pplus (pt-lc x) (pt-lc y))
		   (pplus1 (pt-red x) (pt-red y))))
	((f> (pt-le x) (pt-le y))
	 (cons (pt-le x) (cons (pt-lc x) (pplus1 (pt-red x) y))))
	(t (cons (pt-le y) (cons (pt-lc y) (pplus1 x (pt-red y))))))) 

(defun pcplus (c p) (cond ((pcoefp p) (cplus p c))
			  (t (psimp (p-var p) (pcplus1 c (p-terms p))))))

(defun pcplus1 (c x)
  (cond ((null x)
	 (cond ((pzerop c) nil) (t (cons 0 (cons c nil)))))
	((pzerop (car x)) (pcoefadd 0 (pplus c (cadr x)) nil))
	(t (cons (car x) (cons (cadr x) (pcplus1 c (cddr x)))))))
	 

(defmfun pdifference (x y)
  (cond ((pcoefp x) (pcdiffer x y))
	((pcoefp y) (pcplus (cminus y) x))
	((eq (p-var x) (p-var y))
	 (psimp (p-var x) (pdiffer1 (p-terms x) (p-terms y))))
	((pointergp (p-var x) (p-var y))
	 (psimp (p-var x) (pcdiffer2 (p-terms x) y)))
	(t (psimp (p-var y) (pcdiffer1 x (p-terms y))))))

(defun pdiffer1 (x y)
  (cond ((ptzerop x) (pminus1 y))
	((ptzerop y) x)
	((= (pt-le x) (pt-le y))
	 (pcoefadd (pt-le x)
		   (pdifference (pt-lc x) (pt-lc y))
		   (pdiffer1 (pt-red x) (pt-red y))))
	((f> (pt-le x) (pt-le y))
	 (cons (pt-le x) (cons (pt-lc x) (pdiffer1 (pt-red x) y))))
	(t (cons (pt-le y) (cons (pminus (pt-lc y))
				 (pdiffer1 x (pt-red y)))))))

(defun pcdiffer (c p)
  (cond ((pcoefp p) (cdifference c p))
	(t (psimp (car p) (pcdiffer1 c (cdr p))))))	 

(defun pcdiffer1 (c x)
  (cond ((null x) (cond ((pzerop c) nil) (t (list 0 c))))
	((pzerop (car x))
	 (pcoefadd 0 (pdifference c (cadr x)) nil))
	(t (cons (car x)
		 (cons (pminus (cadr x)) (pcdiffer1 c (cddr x)))))))

(defun pcdiffer2 (x c)
  (cond ((null x) (cond ((pzerop c) nil) (t (list 0 (pminus c)))))
	((pzerop (car x)) (pcoefadd 0 (pdifference (cadr x) c) nil))
	(t (cons (car x) (cons (cadr x) (pcdiffer2 (cddr x) c))))))

(defun pcsubsty (vals vars p)		;list of vals for vars
  (cond ((null vars) p)
	((atom vars) (pcsub p (list vals) (list vars)))	;one val hack
	(t (setq vars (sortcar (mapcar #'cons vars vals) #'pointergp))
	   (pcsub p (mapcar (function cdr) vars)
		  (mapcar (function car) vars)))))

(defun pcsubst (p val var)		;one val for one var
  (cond ((pcoefp p) p)
	((eq (car p) var) (pcsub1 (cdr p) val () ()))
	((pointergp var (car p)) p)
	(t (psimp (car p) (pcsub2 (cdr p) (ncons val) (ncons var)))))) 

(defun pcsub1 (a val vals vars)
  (if (equal val 0) (pcsub (pterm a 0) vals vars)
      (do ((p (pt-red a) (pt-red p))
	   (ans (pcsub (pt-lc a) vals vars)
		(pplus (ptimes ans
			       (pexpt val (f- ld (pt-le p))))
		       (pcsub (pt-lc p) vals vars)))
	   (ld (pt-le a) (pt-le p)))
	  ((null p) (ptimes ans (pexpt val ld))))))

(defun pcsub (p vals vars)
  (cond ((null vals) p)
	((pcoefp p) p)
	((eq (p-var p) (car vars)) (pcsub1 (p-terms p) (car vals)
					   (cdr vals) (cdr vars)))
	((pointergp (car vars) (p-var p))
	 (pcsub p (cdr vals) (cdr vars)))
	(t (psimp (p-var p) (pcsub2 (p-terms p) vals vars)))))

(defun pcsub2 (terms vals vars)
  (loop for (exp coef) on terms by #'pt-red
	 unless (pzerop (setq coef (pcsub coef vals vars)))
	 nconc (list exp coef)))



;;	THIS DERIVATIVE PROGRAM ASSUMES NO DEPENDENCIES HIDDEN ON VARLIST
;;	OR ELSWHERE.  COMPARE TO RATDX.

;;(DEFMFUN PDERIVATIVE (P *VAR*)
;;  (IF (PCOEFP P) (CDERIVATIVE P *VAR*)
;;      (PSIMP (P-VAR P) (COND ((EQ *VAR* (P-VAR P))
;;			      (PDERIVATIVE2 (P-TERMS P)))
;;			     ((POINTERGP *VAR* (P-VAR P))
;;			      (PTZERO))
;;			     (T (PDERIVATIVE3 (P-TERMS P)))))))
;;
;;(DEFUN PDERIVATIVE2 (X)
;;  (COND ((NULL X) NIL)
;;	((ZEROP (PT-LE X)) NIL)
;;	(T (PCOEFADD (f1- (PT-LE X))
;;		     (PCTIMES (CMOD (PT-LE X)) (PT-LC X))
;;		     (PDERIVATIVE2 (PT-RED X))))))
;;
;;(DEFUN PDERIVATIVE3 (X)
;;       (COND ((NULL X) NIL)
;;	     (T (PCOEFADD
;;		 (PT-LE X)
;;		 (PDERIVATIVE (PT-LC X) *VAR*)
;;		 (PDERIVATIVE3 (PT-RED X))))))

;;replaced above by version without special bindings.
(defmfun pderivative (p vari)
  (if (pcoefp p) (cderivative p vari)
      (psimp (p-var p) (cond ((eq vari (p-var p))
			      (pderivative2 (p-terms p)))
			     ((pointergp vari (p-var p))
			      (ptzero))
			     (t (pderivative3 (p-terms p) vari))))))

(defun pderivative2 (x)
  (cond ((null x) nil)
	((zerop (pt-le x)) nil)
	(t (pcoefadd (f1- (pt-le x))
		     (pctimes (cmod (pt-le x)) (pt-lc x))
		     (pderivative2 (pt-red x))))))

(defun pderivative3 (x vari)
  (cond ((null x) nil)
	(t (pcoefadd
	    (pt-le x)
	    (pderivative (pt-lc x) vari)
	    (pderivative3 (pt-red x) vari)))))

(defmfun pdivide (x y)
  (cond ((pzerop y) (errrjf "Quotient by zero"))
	((pacoefp y) (list (ratreduce x y) (rzero)))
	((pacoefp x) (list (rzero) (cons x 1)))
	((pointergp (car x) (car y)) (list (ratreduce x y) (rzero)))
	(t (pdivide1 x y))))

(defun pdivide1 (u v)
  (prog (k inc lcu lcv q r)
     (setq lcv (cons (caddr v) 1))
     (setq q (rzero))
     (setq r (cons u 1) )
     a    (setq k (f- (pdegree (car r) (p-var v)) (p-le v)))
     (if (minusp k) (return (list q r)))
     (setq lcu (cons (p-lc (car r)) (cdr r)))
     (setq inc (ratquotient lcu lcv))
     (setq inc (cons (psimp (car v) (list k (car inc)))
		     (cdr inc)))
     (setq q (ratplus q inc))
     (setq r (ratplus r  (rattimes (cons (pminus v) 1) inc t)))
     (go a)))
	 

(defmfun pexpt (p n)
  (cond ((= n 0) 1)
	((= n 1) p)
	((minusp n) (pquotient 1 (pexpt p (f- n))))
	((pcoefp p) (cexpt p n))
	((alg p) (pexptsq p n))
	((null (p-red p))
	 (psimp (p-var p)
		(pcoefadd (f* n (p-le p)) (pexpt (p-lc p) n) nil)))
	(t (let ((*a* (f1+ n))
		 (*x* (psimp (p-var p) (p-red p)))
		 (b (make-poly (p-var p) (p-le p) (p-lc p))))
	     (do ((bl (list (pexpt b 2) b)
		      (cons (ptimes b (car bl)) bl))
		  (m 2 (f1+ m)))
		 ((= m n) (pplus (car bl)
				 (pexpt2 1 1 *x* (cdr bl)))))))))

(defun nxtbincoef (m nom) (quotient (times nom (difference *a* m)) m))

(defun pexpt2 (m nom b l)
  (if (null l) b
      (pplus (ptimes (pctimes (cmod (setq nom (nxtbincoef m nom))) b) (car l))
	     (pexpt2 (f1+ m)
		     nom
		     (if (eq *x* b) (pexpt b 2)
			 (ptimes *x* b))
		     (cdr l)))))

(defmfun pminusp (p) (if (numberp p) (minusp p)
			 (pminusp (p-lc p))))

(defmfun pminus (p) (if (pcoefp p) (cminus p)
			(cons (p-var p) (pminus1 (p-terms p)))))

(defun pminus1 (x)
  (loop for (exp coef) on x by #'pt-red
	 nconc (list exp (pminus coef))))

(defmfun pmod (p)
  (if (pcoefp p) (cmod p)
      (psimp (car p)
	     (loop for (exp coef) on (p-terms p) by #'pt-red
		    unless (pzerop (setq coef (pmod coef)))
		    nconc (list exp coef)))))

(defmfun pquotient (x y)
  (cond ((pcoefp x)
	 (cond ((pzerop x) (pzero))
	       ((pcoefp y) (cquotient x y))
	       ((alg y) (paquo x y))
	       (t (errrjf "Quotient by a polynomial of higher degree"))))
	((pcoefp y) (cond ((pzerop y) (errrjf "Quotient by zero"))
			  (modulus (pctimes (crecip y) x))
			  (t (pcquotient x y))))
	((alg y) (or (let ((errrjfflag t) $algebraic)
		       (catch 'raterr (pquotient x y)))
		     (patimes x (rainv y))))
	((pointergp (p-var x) (p-var y)) (pcquotient x y))
	((or (pointergp (p-var y) (p-var x)) (f> (p-le y) (p-le x)))
	 (errrjf "Quotient by a polynomial of higher degree"))
	(t (psimp (p-var x) (pquotient1 (p-terms x) (p-terms y))))))

(defun pcquotient (p q)
  (psimp (p-var p) (pcquotient1 (p-terms p) q)))

(defun pcquotient1 (p1 q)
  (loop for (exp coef) on p1 by #'pt-red
	 nconc (list exp (pquotient coef q))))

(declare-top(special k q*)
	    (fixnum k i))

(defun pquotient1 (u v &aux q* (k 0))
  (declare (fixnum k))
  (loop do (setq  k (f- (pt-le u) (pt-le v)))
	 when (minusp k) do (errrjf "Polynomial quotient is not exact")
	 nconc (list k (setq q* (pquotient (pt-lc u) (pt-lc v))))
	 until (ptzerop (setq u (pquotient2 (pt-red u) (pt-red v))))))

(defun pquotient2 (x y &aux (i 0))	;X-v^k*Y*Q*
  (cond ((null y) x)
	((null x) (pcetimes1 y k (pminus q*)))
	((minusp (setq i (f- (pt-le x) (pt-le y) k)))
	 (pcoefadd (f+ (pt-le y) k)
		   (ptimes q* (pminus (pt-lc y)))
		   (pquotient2 x (pt-red y))))
	((zerop i) (pcoefadd (pt-le x)
			     (pdifference (pt-lc x) (ptimes q* (pt-lc y)))
			     (pquotient2 (pt-red x) (pt-red y))))
	(t (cons (pt-le x) (cons (pt-lc x) (pquotient2 (pt-red x) y))))))

(declare-top(unspecial k q*)
	    (notype k i))

(defun algord (var) (and $algebraic (get var 'algord)))

(defun psimp (var x)
  (cond ((ptzerop x) 0)
	((atom x) x)
	((zerop (pt-le x)) (pt-lc x))
	((algord var)			;wrong alg ordering
	 (do ((p x (cddr p)) (sum 0))
	     ((null p) (cond ((pzerop sum) (cons var x))
			     (t (pplus sum (psimp2 var x)))))
	   (unless (or (pcoefp (cadr p)) (pointergp var (caadr p)))
	     (setq sum (pplus sum
			      (if (zerop (pt-le p)) (pt-lc p)
				  (ptimes 
				   (make-poly var (pt-le p) 1)
				   (pt-lc p)))))
	     (setf (pt-lc p) 0))))
	(t (cons var x))))

(defun psimp1 (var x) (let ($algebraic) (psimp var x)))

(defun psimp2 (var x)
  (do ((p (setq x (cons nil x)) ))
      ((null (cdr p)) (psimp1 var (cdr x)))
    (cond ((pzerop (caddr p)) (rplacd p (cdddr p)))
	  (t (setq p (cddr p))))))

(defun pterm  (p n)
  (do ((p p (pt-red p)))
      ((ptzerop p) (pzero))
    (cond ((f< (pt-le p) n) (return (pzero)))
	  ((f= (pt-le p) n) (return (pt-lc p))))))


(defmfun ptimes (x y)
  (cond ((pcoefp x) (if (pzerop x) (pzero) (pctimes x y)))
	((pcoefp y) (if (pzerop y) (pzero) (pctimes y x)))
	((eq (p-var x) (p-var y))
	 (palgsimp (p-var x) (ptimes1 (p-terms x) (p-terms y)) (alg x)))
	((pointergp (p-var x) (p-var y))
	 (psimp (p-var x) (pctimes1 y (p-terms x))))
	(t (psimp (p-var y) (pctimes1 x (p-terms y))))))
;;
;;(DEFUN PTIMES1 (X Y &AUX U*)
;;  (DO ((V* (SETQ U* (PCETIMES1 Y (PT-LE X) (PT-LC X))))
;;       (X (PT-RED X) (PT-RED X)))
;;      ((PTZEROP X) U*)
;;    (PTIMES2 Y (PT-LE X) (PT-LC X))))
;;
;;
;;
;;(DEFUN PTIMES2 (Y XE XC) 
;;  (PROG (E U C) 
;;     A1 (COND ((NULL Y) (RETURN NIL)))
;;	(SETQ E (f+ XE (CAR Y)))
;;	(SETQ C (PTIMES (CADR Y) XC))
;;	(COND ((PZEROP C) (SETQ Y (CDDR Y)) (GO A1))
;;	      ((OR (NULL V*) (> E (CAR V*)))
;;	       (SETQ U* (SETQ V* (PPLUS1 U* (LIST E C))))
;;	       (SETQ Y (CDDR Y)) (GO A1))
;;	      ((= E (CAR V*))
;;	       (SETQ C (PPLUS C (CADR V*)))
;;	       (COND ((PZEROP C)
;;		      (SETQ U* (SETQ V* (PDIFFER1 U* (LIST (CAR V*) (CADR V*))))))
;;		     (T (RPLACA (CDR V*) C)))
;;	       (SETQ Y (CDDR Y))
;;	       (GO A1)))
;;     A  (COND ((AND (CDDR V*) (> (CADDR V*) E)) (SETQ V* (CDDR V*)) (GO A)))
;;	(SETQ U (CDR V*))
;;     B  (COND ((OR (NULL (CDR U)) (< (CADR U) E))
;;	       (RPLACD U (CONS E (CONS C (CDR U)))) (GO E)))
;;	(COND ((PZEROP (SETQ C (PPLUS (CADDR U) C)))
;;	       (RPLACD U (CDDDR U)) (GO D))
;;	      (T (RPLACA (CDDR U) C)))
;;     E  (SETQ U (CDDR U))
;;     D  (SETQ Y (CDDR Y))
;;	(COND ((NULL Y) (RETURN NIL)))
;;	(SETQ E (f+ XE (CAR Y)))
;;	(SETQ C (PTIMES (CADR Y) XC))
;;     C  (COND ((AND (CDR U) (> (CADR U) E)) (SETQ U (CDDR U)) (GO C)))
;;	(GO B)))


(defun   ptimes1 (x y-orig &aux uuu  )
  (do ((vvv (setq uuu (pcetimes1 y-orig (pt-le x) (pt-lc x))))
       (x (pt-red x) (pt-red x)))
      ((ptzerop x) uuu)
    (let ((y y-orig) (xe (pt-le x)) (xc (pt-lc x)))
      (prog (e u c) 
       a1 (cond ((null y) (return nil)))
       (setq e (f+ xe (car y)))
       (setq c (ptimes (cadr y) xc))
       (cond ((pzerop c) (setq y (cddr y)) (go a1))
	     ((or (null vvv) (f> e (car vvv)))
	      (setq uuu (setq vvv (pplus1 uuu (list e c))))
	      (setq y (cddr y)) (go a1))
	     ((f= e (car vvv))
	      (setq c (pplus c (cadr vvv)))
	      (cond ((pzerop c)
		     (setq uuu (setq vvv (pdiffer1 uuu (list (car vvv) (cadr vvv))))))
		    (t (rplaca (cdr vvv) c)))
	      (setq y (cddr y))
	      (go a1)))
       a  
       (cond ((and (cddr vvv) (f> (caddr vvv) e))
	      (setq vvv (cddr vvv)) (go a)))
       (setq u (cdr vvv ))
       b  (cond ((or (null (cdr u)) (f< (cadr u) e))
		 (rplacd u (cons e (cons c (cdr u)))) (go e)))
       (cond ((pzerop (setq c (pplus (caddr u) c)))
	      (rplacd u (cdddr u)) (go d))
	     (t (rplaca (cddr u) c)))
       e  (setq u (cddr u))
       d  (setq y (cddr y))
       (cond ((null y) (return nil)))
       (setq e (f+ xe (car y)))
       (setq c (ptimes (cadr y) xc))
       c  (cond ((and (cdr u) (f> (cadr u) e)) (setq u (cddr u)) (go c)))
       (go b))))
  uuu)

(defun pcetimes1 (y e c)		;C*V^E*Y
  (loop for (exp coef) on y by #'pt-red
	 unless (pzerop (setq coef (ptimes c coef)))
	 nconc (list (+ e exp) coef)))

(defun pctimes (c p)
  (if (pcoefp p) (ctimes c p)
      (psimp (p-var p) (pctimes1 c (p-terms p)))))

(defun pctimes1 (c terms)
  (loop for (exp coef) on terms by #'pt-red
	 unless (pzerop (setq coef (ptimes c coef)))
	 nconc (list exp coef)))

(defun leadalgcoef (p)
  (cond ((pacoefp p) p)
	(t (leadalgcoef (p-lc p))) ))

(defun painvmod (q)
  (cond ((pcoefp q) (crecip q))
	(t (paquo (list (car q) 0 1) q ))))

(defun palgsimp (var p tell)		;TELL=(N X) -> X^(1/N)
  (psimp var (cond ((or (null tell) (null p)
			(< (car p) (car tell))) p)
		   ((null (cddr tell)) (pasimp1 p (car tell) (cadr tell)))
		   (t (pgcd1 p tell)) )))

(defun pasimp1 (p deg kernel)		;assumes deg>=(car p)
  (do ((a p (pt-red a))
       (b p a))
      ((or (null a) (< (pt-le a) deg))
       (rplacd (cdr b) nil)
       (pplus1 (pctimes1 kernel p) a))
    (rplaca a (- (pt-le a) deg))))

(defmfun monize (p) 
  (cond ((pcoefp p) (if (pzerop p) p 1))
	(t (cons (p-var p) (pmonicize (copy1 (p-terms p)))))))

(defun pmonicize (p)			;CLOBBERS POLY
  (cond ((eqn (pt-lc p) 1) p)
	(t (pmon1 (painvmod (leadalgcoef (pt-lc p))) p) p)))

(defun pmon1 (mult l)
  (cond (l (pmon1 mult (pt-red l))
	   (setf (pt-lc l) (ptimes mult (pt-lc l))))))

(defun pmonz (poly &aux lc)		;A^(N-1)*P(X/A)
  (setq poly (pabs poly))       
  (cond ((equal (setq lc (p-lc poly)) 1) poly)
	(t (do ((p (p-red poly) (pt-red p))
		(p1 (make-poly (p-var poly) (p-le poly) 1))
		(mult 1)
		(deg (f1- (p-le poly)) (pt-le p)))
	       ((null p) p1)
	     (setq mult (ptimes mult (pexpt lc (f- deg (pt-le p)))))
	     (nconc p1 (list (pt-le p) (ptimes mult (pt-lc p))))))))

;;	THESE ARE ROUTINES FOR MANIPULATING ALGEBRAIC NUMBERS

(defun algnormal (p) (car (rquotient p (leadalgcoef p))))

(defun algcontent (p)
  (destructuring-let* ((lcf (leadalgcoef p))
		       ((prim . denom) (rquotient p lcf)))
    (list (ratreduce lcf denom) prim)))

(defun rquotient (p q &aux algfac* a e)	;FINDS PSEUDO QUOTIENT IF PSEUDOREM=0
  (cond ((equal p q) (cons 1 1))
	((pcoefp q) (ratreduce p q))
	((setq a (testdivide p q)) (cons a 1))
	((alg q) (rattimes (cons p 1) (rainv q) t))
	(t (cond ((alg (setq a (leadalgcoef q)))
		  (setq a (rainv a))
		  (setq p (ptimes p (car a)))
		  (setq q (ptimes q (car a)))
		  (setq a (cdr a)) ))
	   (cond ((minusp (setq e (f+ 1 (f- (cadr q)) (pdegree p (car q)))))
		  (errrjf "Quotient by a polynomial of higher degree")))
	   (setq a (pexpt a e))
	   (ratreduce (or (testdivide (ptimes a p) q)
			  (prog2 (setq a (pexpt (p-lc q) e))
			      (pquotient (ptimes a p) q)))
		      a)) ))

(defun patimes (x r) (pquotientchk (ptimes x (car r)) (cdr r)))

(defun paquo (x y) (patimes x (rainv y)))

(defun mpget (var)
  (cond ((null (setq var (alg var))) nil)
	((cddr var) var)
	(t (list (car var) 1 0 (pminus (cadr var))))))


(defun rainv (q)
  (cond ((pcoefp q)
	 (cond (modulus (cons (crecip q) 1))
	       (t (cons 1 q))))
	(t (let ((var (car q)) (p (mpget q)))
	     (declare (special var))	;who uses this? --gsb
	     (cond ((null p) (cons 1 q))
		   (t (setq p (car (let ($ratalgdenom)
				     (bprog q (cons var p)))))
		      (rattimes (cons (car p) 1) (rainv (cdr p)) t)))))))

(defun pexptsq (p n)
  (do ((n (quotient n 2) (quotient n 2))
       (s (cond ((oddp n) p) (t 1))))
      ((zerop n) s)
    (setq p (ptimes p p))
    (and (oddp n) (setq s (ptimes s p))) ))

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 1.

;; Someone should determine which of the variables special in this file
;; (and others) are really special and which are used just for dynamic
;; scoping.  -- cwh

#-nil					;Minimize complaints.
(declare-top(unspecial v* *a* u* *var*))
