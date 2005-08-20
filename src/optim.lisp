;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module optim)

(declare-top (special vars setqs optimcount xvars)
	     (fixnum n (opt-hash))
	     (array* (notype (subexp 1)))
	     #-nil (unspecial args))

;;(ARRAY *SUBEXP* T 64.)
(defvar *subexp* (*array nil t 64.))

(defmvar $optimprefix '$%)

(defmvar $optimwarn t "warns if `optimize' encounters a special form.")

;; $OPTIMIZE takes a Macsyma expression and returns a BLOCK form which is
;; equivalent, but which uses local variables to store the results of computing
;; common subexpressions.  These subexpressions are found by hashing them.

(defmfun $optimize (x0)
  (let (($optimwarn $optimwarn))
    (prog (vars setqs optimcount xvars x)
       (setq optimcount 0 xvars (cdr ($listofvars x0)))
       (fillarray *subexp* '(nil))
       (setq x (collapse (opformat (collapse x0))))
       (if (atom x) (return x))
       (comexp x)
       (setq x (optim x))
       (return (prog1 (cond ((null vars) x0)
			    (t (if (or (not (eq (caar x) 'mprog))
				       (and ($listp (cadr x)) (cdadr x)))
				   (setq x (nreverse (cons x setqs)))
				   (setq x ;(NCONC (NREVERSE SETQS) (CDDR X))
					 (nreconc setqs (cddr x))))
			       `((mprog simp) ((mlist) . ,(nreverse vars)) . ,x)))
		 (fillarray *subexp* '(nil)))))))

(defun opformat (x)
  (cond ((atom x) x)
	((specrepp x) (opformat (specdisrep x)))
	((and $optimwarn
	      (mspecfunp (caar x))
	      (prog2 (mtell "`optimize' has met up with a special form - ~
			     answer may be wrong.")
		  (setq $optimwarn nil))))
	((eq (caar x) 'mexpt) (opmexpt x))
	(t (let ((newargs (mapcar #'opformat (cdr x))))
	     (if (alike newargs (cdr x)) x (cons (car x) newargs))))))

(defun opmexpt (x)
  (let ((*base (opformat (cadr x))) (exp (opformat (caddr x))) xnew negexp)
    (setq negexp
	  (cond ((and (numberp exp) (minusp exp)) (minus exp))
		((and (ratnump exp) (minusp (cadr exp)))
		 (list (car exp) (minus (cadr exp)) (caddr exp)))
		((and (mtimesp exp) (numberp (cadr exp)) (minusp (cadr exp)))
		 (if (equal (cadr exp) -1)
		     (if (null (cdddr exp)) (caddr exp)
			 (cons (car exp) (cddr exp)))
		     (list* (car exp) (minus (cadr exp)) (cddr exp))))
		((and (mtimesp exp) (ratnump (cadr exp)) (minusp (cadadr exp)))
		 (list* (car exp)
			(list (caadr exp) (minus (cadadr exp)) (caddr (cadr exp)))
			(cddr exp)))))
    (setq xnew
	  (cond (negexp
		 `((mquotient)
		   1
		   ,(cond ((equal negexp 1) *base)
			  (t (setq xnew (list (car x) *base negexp))
			     (if (and (ratnump negexp) (equal (caddr negexp) 2))
				 (opmexpt xnew)
				 xnew)))))
		((and (ratnump exp) (equal (caddr exp) 2)) 
		 (setq exp (cadr exp))
		 (if (equal exp 1) `((%sqrt) ,*base)
		     `((mexpt) ((%sqrt) ,*base) ,exp)))
		(t (list (car x) *base exp))))
    (if (alike1 x xnew) x xnew)))

(defmfun $collapse (x)
  (fillarray *subexp* '(nil))
  (prog1 (collapse x) (fillarray *subexp* '(nil))))
       
(defun collapse (x)
  (cond ((atom x) x)
	((specrepp x) (collapse (specdisrep x)))
	(t (let ((n (opt-hash (caar x))))
	     (do ((l (cdr x) (cdr l)))
		 ((null l))
	       (if (not (eq (collapse (car l)) (car l)))
		   (rplaca l (collapse (car l))))
	       (setq n (fixnum-remainder (f+ (opt-hash (car l)) n) 12553.)))
	     (setq n (logand 63. n))
	     (do ((l (aref *subexp* n) (cdr l)))
		 ((null l) (store (aref *subexp* n) (cons (list x) (aref *subexp* n))) x)
	       (if (alike1 x (caar l)) (return (caar l))))))))

(defun comexp (x)
  (if (not (or (atom x) (eq (caar x) 'rat)))
      (let ((n (opt-hash (caar x))))
	(dolist (u (cdr x)) (setq n (fixnum-remainder (f+ (opt-hash u) n) 12553.)))
	(setq x (assol x (aref *subexp* (logand 63. n))))
	(cond ((null (cdr x)) (rplacd x 'seen) (mapc #'comexp (cdar x)))
	      (t (rplacd x 'comexp))))))

(defun optim (x)
  (cond ((atom x) x)
	((and (memq 'array (cdar x))
	      (not (eq (caar x) 'mqapply))
	      (not (mget (caar x) 'arrayfun-mode)))
	 x)
	((eq (caar x) 'rat) x)
	(t (let ((n (opt-hash (caar x))) (nx (list (car x))))
	     (dolist (u (cdr x))
	       (setq n (fixnum-remainder (f+ (opt-hash u) n) 12553.)
		     nx (cons (optim u) nx)))
	     (setq x (assol x (aref *subexp* (logand 63. n))) nx (nreverse nx))
	     (cond ((eq (cdr x) 'seen) nx)
		   ((eq (cdr x) 'comexp)
		    (rplacd x (getoptimvar))
		    (setq setqs (cons `((msetq) ,(cdr x) ,nx) setqs))
		    (cdr x))
		   (t (cdr x)))))))

(defun opt-hash (exp)		   ; EXP is in general representation.
  (fixnum-remainder (if (atom exp)
			(sxhash exp)
			(do ((n (opt-hash (caar exp)))
			     (args (cdr exp) (cdr args)))
			    ((null args) n)
			  (setq n (fixnum-remainder (f+ (opt-hash (car args)) n) 12553.))))
		    12553.))   ; a prime number < 2^14 ; = PRIME(1500)


(defun getoptimvar ()
  (loop with var
	 do
	 (increment optimcount)
	 (setq var
	       #-(or nil cl) (intern  (maknam (nconc (exploden $optimprefix)
						     (mexploden optimcount))))
	       #+cl (make-symbol
		     (format nil "~A~D"
			     $optimprefix optimcount))
	       #+nil (symbolconc $optimprefix optimcount))
	 while (memq var xvars)
	 finally
	 (setq vars (cons var vars))
	 (return var)))



