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

(macsyma-module ratout)

;; THIS IS THE OUT-OF-CORE SEGMENT OF THE RATIONAL FUNCTION PACKAGE.

(declare-top (special *y*))

;;	NEWGCD (X,Y) RETURNS A LIST OF THREE ITEMS,
;;	(GCD, X/GCD, Y/GCD)

(defun newgcd (ratout-x ratout-y modulus)
  (set-modulus modulus)
  (let ((a (cond ((pcoefp ratout-x)
		  (cond ((zerop ratout-x)
			 ratout-y)
			((pcoefp ratout-y)
			 (cgcd ratout-x ratout-y))
			(t
			 (pcontent1 (cdr ratout-y) ratout-x))))
		 ((pcoefp ratout-y)
		  (cond ((zerop ratout-y)
			 ratout-x)
			(t
			 (pcontent1 (cdr ratout-x) ratout-y))))
		 ((pointergp (p-var ratout-x) (p-var ratout-y))
		  (oldcontent1 (cdr ratout-x) ratout-y))
		 ((pointergp (p-var ratout-y) (p-var ratout-x))
		  (oldcontent1 (cdr ratout-y) ratout-x))
		 (t nil))))
    (cond (a
	   (list a (pquotient ratout-x a) (pquotient ratout-y a)))
	  (modulus
	   (pgcdp ratout-x ratout-y modulus))
	  (t
	   (pgcdm ratout-x ratout-y)))))

(defun pgathercoef (p ratout-*chk ratout-*res)
  (if (not (eq (car p) ratout-*chk))
      1
      (labels
	  ((pgath2 (p vmax)
	     (prog (v2)
		(cond ((null p)
		       (return ratout-*res))
		      ((pcoefp (cadr p))
		       nil)
		      ((vgreat (setq v2 (pdegreer (cadr p))) vmax)
		       (setq ratout-*res (psimp ratout-*chk
					 (list (car p) (leadcoefficient (cadr p)))))
		       (setq vmax v2))
		      ((equal vmax v2)
		       (setq ratout-*res
			     (pplus ratout-*res
				    (psimp ratout-*chk
					   (list (car p) (leadcoefficient (cadr p))))))))
		(return (pgath2 (cddr p) vmax)))))
	(pgath2 (cdr p) nil))))

(defun pgath1 (p ratout-*max ratout-*var)
  (prog nil
     (cond ((null p)
	    (return ratout-*max))
	   ((pcoefp (cadr p))
	    nil)
	   ((eq (caadr p) ratout-*var)
	    (setq ratout-*max (max ratout-*max (cadadr p)))))
     (return (pgath1 (cddr p) ratout-*max ratout-*var))))

(defun pnext (ratout-x ratout-*l)
  (labels
      ((pnext1 (ratout-x)
	 (prog nil
	    (cond ((null ratout-x)
		   (return ratout-*l))
		  ((or (pcoefp (cadr ratout-x))
		       (member (caadr ratout-x) ratout-*l :test #'eq))
		   nil)
		  (t
		   (setq ratout-*l (cons (caadr ratout-x) ratout-*l))))
	    (return (pnext1 (cddr ratout-x))))))
    (pnext1 ratout-x)
    (cond ((null ratout-*l)
	   nil)
	  (t
	   (car (sort ratout-*l #'pointergp))))))

(defun vgreat (ratout-x ratout-y)
  (cond ((null ratout-x)
	 nil)
	((null ratout-y)
	 t)
	((pointergp (car ratout-x) (car ratout-y))
	 t)
	((not (eq (car ratout-x) (car ratout-y)))
	 nil)
	((> (cadr ratout-x) (cadr ratout-y))
	 t)
	((equal (cadr ratout-x) (cadr ratout-y))
	 (vgreat (cddr ratout-x) (cddr ratout-y)))
	(t
	 nil)))

(defun pdegreer (ratout-x)
  (if (pcoefp ratout-x)
      ()
      (cons (car ratout-x)
	    (cons (cadr ratout-x)
		  (pdegreer (caddr ratout-x))))))

;;***	PGCDP CORRESPONDS TO BROWN'S ALGORITHM P

(defun pgcdp (ratout-bigf1 ratout-bigf2 modulus)
  (labels
      ((pmodcontent (p ratout-xv)
	 ;;***	PMODCONTENT COMPUTES CONTENT OF
	 ;;	P IN
	 ;;	Z [X ] [X , X , ..., X   ]
	 ;;        P  V    1   2        V-1

	 ;;	PMODCONTENT OF 3*A*X IS A, IF MAINVAR IS X (=X )
	 ;;						      V
	 (prog (ratout-*var ratout-*chk ratout-*res ratout-*max ratout-gcd)
	    (setq ratout-*chk (car p))
	    (setq ratout-*max 0)
	    (setq ratout-*var (pnext (cdr p) nil))
	    (cond ((pointergp ratout-xv ratout-*chk)
		   (go ret1))
		  ((null ratout-*var)
		   (return (list p 1))))
	    (pgath1 (cdr p) ratout-*max ratout-*var)
	  a
	    (setq ratout-*res 0)
	    (labels
		((pgath3 (p ratout-*chk ratout-*max)
		   (prog (zz)
		      (cond ((null p)
			     (return ratout-*res))
			    ((pcoefp (cadr p))
			     (cond ((equal ratout-*max 0)
				    (setq zz (cadr p))
				    (go add))
				   (t
				    (go ret))))
			    ((eq (caadr p) ratout-*var)
			     (setq zz (ptterm (cdadr p) ratout-*max))
			     (go add)))
		      (cond ((equal ratout-*max 0)
			     (setq zz (cadr p)))
			    (t
			     (go ret)))
		    add
		      (cond ((equal zz 0)
			     (go ret)))
		      (setq ratout-*res (pplus ratout-*res (psimp ratout-*chk (list (car p) zz))))
		    ret
		      (return (pgath3 (cddr p) ratout-*chk ratout-*max)))))
	    (pgath3 (cdr p) ratout-*chk ratout-*max))
	  a2
	    (cond ((pcoefp ratout-*res)
		   (cond ((pzerop ratout-*res)
			  nil)
			 (t
			  (go ret1))))
		  ((not (eq (car ratout-*res) ratout-*chk))
		   (go ret1))
		  ((not (univar (cdr ratout-*res)))
		   (setq ratout-*res (car (pmodcontent ratout-*res ratout-xv)))
		   (go a2))
		  (ratout-gcd
		   (setq ratout-gcd (pgcdu ratout-gcd ratout-*res)))
		  (t
		   (setq ratout-gcd ratout-*res)))
	    (cond ((pcoefp ratout-gcd)
		   (go ret1))
		  ((minusp (setq ratout-*max (1- ratout-*max)))
		   (return (list ratout-gcd (pquotient p ratout-gcd)))))
	    (go a)
	  ret1
	    (return (list 1 p)))))
    (prog (c c1		c2		ratout-n		q
	   h1tilde	h2tilde		gstar		h1star
	   h2star	ratout-xv		e		b
	   gbar		nubar		nu1bar		nu2bar
	   gtilde		f1tilde		f2tilde		biggtilde
	   degree		f1		f1f2)
       (set-modulus modulus)
       (cond ((and (univar (cdr ratout-bigf1))
		   (univar (cdr ratout-bigf2)))
	      (setq q (pgcdu ratout-bigf1 ratout-bigf2))
	      (return (list q (pquotient ratout-bigf1 q) (pquotient ratout-bigf2 q)))))
       (setq ratout-xv (car ratout-bigf1))
       (setq ratout-bigf1 (pmodcontent ratout-bigf1 ratout-xv))
       (setq ratout-bigf2 (pmodcontent ratout-bigf2 ratout-xv))
       (setq c (pgcdu (setq c1 (car ratout-bigf1)) (setq c2 (car ratout-bigf2))))
       (setq ratout-bigf1 (cadr ratout-bigf1))
       (setq ratout-bigf2 (cadr ratout-bigf2))
       (setq ratout-n 0)
       (setq e (pdegreer ratout-bigf2))
       (setq degree (pdegreer ratout-bigf1))
       (cond ((vgreat e degree)
	      (setq e degree)))
       (setq b (ash modulus -1))
       (setq gbar
	     (pgcdu (setq f1 (pgathercoef ratout-bigf1 ratout-xv 0))
		    (setq f1f2
			  (pgathercoef ratout-bigf2 ratout-xv 0))))
       (cond ((equal 0 f1f2)
	      (go step15a)))
       (setq nubar (pdegree gbar ratout-xv))
       (setq nu1bar (+ nubar (pdegree ratout-bigf1 ratout-xv)))
       (setq nu2bar (+ nubar (pdegree ratout-bigf2 ratout-xv)))
       (setq f1f2 (ptimes f1 f1f2))
       (setq nubar (max nu1bar nu2bar))
     step6
       (setq b (cplus b 1))
       (cond ((equal (pcsubst f1f2 b ratout-xv) 0)
	      (go step6)))
       ;; Step 7
       (setq gtilde (pcsubst gbar b ratout-xv))
       (setq f1tilde (pcsubst ratout-bigf1 b ratout-xv))
       (setq f2tilde (pcsubst ratout-bigf2 b ratout-xv))
       (setq biggtilde
	     (ptimeschk gtilde
			(car (setq h2tilde (newgcd f1tilde f2tilde modulus)))))
       (cond ((pcoefp biggtilde)
	      (go step15a)))
       (setq h1tilde (cadr h2tilde))
       (setq h2tilde (caddr h2tilde))
       (setq degree (pdegreer biggtilde))
       (cond ((vgreat degree e)
	      (go step6))
	     ((vgreat e degree)
	      (setq ratout-n 0)
	      (setq e degree)))
       (setq ratout-n (1+ ratout-n))
       (cond ((equal ratout-n 1)
	      (setq q (list ratout-xv 1 1 0 (cminus b)))
	      (setq gstar biggtilde)
	      (setq h1star h1tilde)
	      (setq h2star h2tilde))
	     (t
	      (setq gstar (lagrange33 gstar biggtilde q b))
	      (setq h1star (lagrange33 h1star h1tilde q b))
	      (setq h2star (lagrange33 h2star h2tilde q b))
	      (setq q (ptimes q (list ratout-xv 1 1 0 (cminus b))))))
       ;; Step 12
       (cond ((not (> ratout-n nubar))
	      (go step6)))
       ;; Step 13
       (cond ((or (not (= nu1bar (+ (setq degree (pdegree gstar ratout-xv))
				    (pdegree h1star ratout-xv))))
		  (not (= nu2bar (+ degree (pdegree h2star ratout-xv)))))
	      (setq ratout-n 0)
	      (go step6)))
       (setq gstar (cadr (pmodcontent gstar ratout-xv)))
       ;; Step 15
       (setq q (pgathercoef gstar ratout-xv 0))
       (return (monicgcd  (ptimeschk c gstar)
			  (ptimeschk (pquotient c1 c) (pquotientchk h1star q))
			  (ptimeschk (pquotient c2 c) (pquotientchk h2star q))
			  (leadcoefficient gstar)))
     step15a
       (return (list c
		     (ptimeschk (pquotient c1 c) ratout-bigf1)
		     (ptimeschk (pquotient c2 c) ratout-bigf2))) )))

(defun monicgcd (ratout-gcd ratout-x ratout-y lcf)
  (cond ((equal lcf 1)
	 (list ratout-gcd ratout-x ratout-y))
	(t (list (ptimes (crecip lcf) ratout-gcd)
		 (ptimes lcf ratout-x)
		 (ptimes lcf ratout-y)))))

;;***	PGCDM CORRESPONDS TO BROWN'S ALGORITHM M


(defun pgcdm (ratout-bigf1 ratout-bigf2)
  (prog (c c1		c2		f1		f2	ratout-n
	 e		degree		mubar		p
	 gtilde		h1tilde		h2tilde
	 modulus
	 biggtilde	q		h1star		h2star
	 gstar		gbar)
     (setq p *alpha)
     ;; Step 1
     (setq f1 (pcontent ratout-bigf1))
     (setq f2 (pcontent ratout-bigf2))
     (setq c (cgcd (setq c1 (car f1)) (setq c2 (car f2))))
     (setq ratout-bigf1 (cadr f1))
     (setq ratout-bigf2 (cadr f2))
     ;; Step 3
     (setq f1 (leadcoefficient ratout-bigf1))
     (setq f2 (leadcoefficient ratout-bigf2))
     (setq gbar (cgcd f1 f2))
     ;; Step 4
     (setq ratout-n 0)
     (setq degree (pdegreer ratout-bigf1))
     (setq e (pdegreer ratout-bigf2))
     (cond ((vgreat e degree)
	    (setq e degree)))
     ;; Step 5
     (setq mubar
	   (* 2 gbar (max (maxcoefficient ratout-bigf1)
			  (maxcoefficient ratout-bigf2))))
     (go step6a)
   step6
     (setq p (newprime p))
   step6a
     (cond ((or (zerop (rem f1 p))
		(zerop (rem f2 p)))
	    (go step6)))
     (set-modulus p)
     ;; Step 7
     (setq gtilde (pmod gbar))
     ;; Step 8
     (setq biggtilde
	   (ptimeschk gtilde
		      (car (setq h2tilde
				 (newgcd (pmod ratout-bigf1) (pmod ratout-bigf2)
					 modulus)))))
     (cond ((pcoefp biggtilde)
	    (setq modulus nil)
	    (setq gstar 1)
	    (setq h1star ratout-bigf1)
	    (setq h2star ratout-bigf2)
	    (go step15)))
     (cond ((null (cdr h2tilde))
	    (setq h1tilde (pquotient (pmod ratout-bigf1) (car h2tilde)))
	    (setq h2tilde (pquotient (pmod ratout-bigf2) (car h2tilde))))
	   (t
	    (setq h1tilde (cadr h2tilde))
	    (setq h2tilde (caddr h2tilde))))
     (setq degree (pdegreer biggtilde))
     (cond ((vgreat degree e)
	    (go step6))
	   ((vgreat e degree)
	    (setq ratout-n 0)
	    (setq e degree)))
     (setq ratout-n (1+ ratout-n))
     ;; Step 11
     (set-modulus nil)
     (cond ((equal ratout-n 1)
	    (setq q p)
	    (setq gstar biggtilde)
	    (setq h1star h1tilde)
	    (setq h2star h2tilde))
	   (t
	    (setq gstar (lagrange3 gstar biggtilde p q))
	    (setq h1star (lagrange3 h1star h1tilde p q))
	    (setq h2star (lagrange3 h2star h2tilde p q))
	    (setq q (* p q))))
     ;; Step 12
     (cond ((> mubar q)
	    (go step6)))
     (cond ((> (* 2 (max (* (setq gtilde (norm gstar))
			    (maxcoefficient h1star))
			 (* gtilde (maxcoefficient h2star))))
	       q)
	    (go step6)))
     (set-modulus nil)
     (setq gstar (cadr (pcontent gstar)))
   step15
     (setq q (leadcoefficient gstar))
     (return (list (ptimeschk c gstar)
		   (ptimeschk (cquotient c1 c) (pquotientchk h1star q))
		   (ptimeschk (cquotient c2 c) (pquotientchk h2star q))))))

;;	THE FUNCTIONS ON THIS PAGE ARE USED BY KRONECKER FACTORING

(defun pkroneck (p)
  (prog (maxexp i l ratout-*p factors factor ratout-*l)
     (setq maxexp (quotient (cadr p) 2))
     (setq i 1)
   a
     (when (> i maxexp)
       (return (cons p factors)))
     (setq l (p1 (reverse (let ((p p) (i i) ($factorflag t))
			    (pfactor2 p i)))))
   b
     (when (null l)
       (go d))
     (setq ratout-*l (car l))
     (setq ratout-*p (car p))
     (ignore-rat-err
       (setq factor (errset (pinterpolate ratout-*l ratout-*p))))
     (setq l (cdr l))
     (if (atom factor)
	 (go b)
	 (setq factor (car factor)))
     (when (or (pcoefp factor)
	       (not (equal (car p) (car factor)))
	       (not (pzerop (prem p factor))))
       (go b))
     (cond (modulus
	    (pmonicize (cdr factor)))
	   ((pminusp factor)
	    (setq factor (pminus factor))))
     (setq p (pquotient p factor))
     (setq maxexp (quotient (cadr p) 2))
     (setq factors (cons factor factors))
     (go a)
   d
     (incf i)
     (go a)))

(defun pfactor2 (p i)
  (cond ((< i 0) nil)
	(t (cons (pfactor (pcsubst p i (car p)))
		 (pfactor2 p (1- i))))))

(defun rpowerset (ratout-x ratout-n)
  (cond ((null ratout-x)
	 (quote (1 nil)))
	((equal ratout-x 1)
	 (quote (1)))
	(t
	 (cons 1 (ptts1 ratout-x ratout-n ratout-x)))))


(defun allprods (ratout-x ratout-y)
  (cond ((null ratout-x)
	 nil)
	((null ratout-y)
	 nil)
	(t
	 (nconc (ap1 (car ratout-x) ratout-y)
		(allprods (cdr ratout-x) ratout-y)))))

;; NOTE: As best as I (rtoy) can tell, this function is never called
;; from the testsuite (including the share testsuite).  This function
;; can be called from pkroneck which is called from pfactorany in
;; rat3d.lisp.  Probably best not to modify this until we have some
;; test coverage of this function.
(defun al1 (ratout-f r len)
  (prog (ratout-ss)
     (cond
       ((equal len 1)
	(return (mapcar #'(lambda (*y*)
			    (cons *y* nil))
			ratout-f)))
       ((null r)
	(return nil))
       (t
	(mapc #'(lambda (*y*)
		  (setq ratout-ss
			(nconc ratout-ss
			       (mapcar #'(lambda (z) (cons z *y*))
				       ratout-f))))
	      (al1 (car r) (cdr r) (1- len)))
	(return ratout-ss)))))


(defun ap1 (ratout-x l)
  (cond ((null l)
	 nil)
	(t
	 (cons (ptimes ratout-x (car l))
	       (ap1 ratout-x (cdr l))))))

(defun ptts1 (ratout-x ratout-n ratout-y)
  (cond ((equal ratout-n 1)
	 (list ratout-y))
	(t
	 (cons ratout-y (ptts1 ratout-x (1- ratout-n) (ptimes ratout-x ratout-y))))))

(defun p1 (l)
  (prog (a)
     (setq a (mapcar #'p11 l))
     (return (cond ((null l)
		    nil)
		   (t
		    (cdr (al1 (car a)
			      (cdr a)
			      (length a))))))))

(defun p11 (ele)
  (cond ((null (cddr ele))
	 (rpowerset (car ele) (cadr ele)))
	(t
	 (allprods (rpowerset (car ele) (cadr ele))
		   (p11 (cddr ele))))))

(defun pinterpolate (l var)
  (psimp var (pinterpolate1 (pinterpolate2 l 1)
			    (- (length l) 2))))

(defun pinterpolate1 (ratout-x ratout-n)
  (pinterpolate4 (pinterpolate5 (reverse ratout-x) 1 ratout-n ratout-n)
		 (1+ ratout-n)))

(defun pinterpolate2 (ratout-x ratout-n)
  (cond ((null (cdr ratout-x))
	 ratout-x)
	(t
	 (cons (car ratout-x)
	       (pinterpolate2 (pinterpolate3 ratout-x ratout-n) (1+ ratout-n))))))

(defun pinterpolate3 (ratout-x ratout-n)
  (cond ((null (cdr ratout-x))
	 nil)
	(t
	 (cons (pquotient (pdifference (cadr ratout-x) (car ratout-x)) ratout-n)
	       (pinterpolate3 (cdr ratout-x) ratout-n)))))

(defun pinterpolate4 (ratout-x ratout-n)
  (cond ((null ratout-x)
	 nil)
	((pzerop (car ratout-x))
	 (pinterpolate4 (cdr ratout-x) (1- ratout-n)))
	(t
	 (cons ratout-n (cons (car ratout-x)
		       (pinterpolate4 (cdr ratout-x) (1- ratout-n)))))))

(defun pinterpolate5 (ratout-x i j ratout-n)
  (cond ((> i ratout-n)
	 ratout-x)
	(t
	 (pinterpolate5 (cons (car ratout-x) (pinterpolate6 ratout-x i j))
			(1+ i)
			(1- j)
			ratout-n))))

(defun pinterpolate6 (ratout-x i j)
  (cond ((zerop i)
	 (cdr ratout-x))
	(t
	 (cons (pdifference (cadr ratout-x) (pctimes j (car ratout-x)))
	       (pinterpolate6 (cdr ratout-x) (1- i) j)))))

;; THE N**(1.585) MULTIPLICATION SCHEME
;;FOLLOWS.  IT SHOULD BE USED ONLY WHEN BOTH INPUTS ARE MULTIVARIATE,
;;DENSE, AND OF NEARLY THE SAME SIZE.  OR ABSOLUTELY TREMENDOUS.
;;(THE CLASSICAL MULTIPLICATION SCHEME IS N**2 WHERE N IS SIZE OF
;;POLYNOMIAL   (OR N*M FOR DIFFERENT SIZES).  FOR THIS
;;CASE, N IS APPX. THE SIZE OF LARGER.

(defmfun $fasttimes (ratout-x ratout-y)
  (cond ((and (not (atom ratout-x))
	      (not (atom ratout-y))
	      (equal (car ratout-x) (car ratout-y))
	      (equal (caar ratout-x) 'mrat)
	      (equal (cddr ratout-x) 1)
	      (equal (cddr ratout-y) 1))
	 (cons (car ratout-x)
	       (cons (fptimes (cadr ratout-x)(cadr ratout-y))
		     1)))
	(t
	 (merror (intl:gettext "fasttimes: arguments must be CRE polynomials with same variables.")))))

(defun fptimes (ratout-x ratout-y)
  (cond ((or (pzerop ratout-x) (pzerop ratout-y))
	 (pzero))
	((pcoefp ratout-x)
	 (pctimes ratout-x ratout-y))
	((pcoefp ratout-y)
	 (pctimes ratout-y ratout-x))
	((eq (car ratout-x) (car ratout-y))
	 (cond ((or (univar(cdr ratout-x))
		    (univar(cdr ratout-y)))
		(cons (car ratout-x)
		      (ptimes1 (cdr ratout-x) (cdr ratout-y))))
	       (t
		(cons (car ratout-x)
		      (fptimes1 (cdr ratout-x)(cdr ratout-y))))))
	((pointergp (car ratout-x) (car ratout-y))
	 (cons (car ratout-x)
	       (pctimes1 ratout-y (cdr ratout-x))))
	(t
	 (cons (car ratout-y)
	       (pctimes1 ratout-x (cdr ratout-y))))))

(defun fptimes1 (ratout-f g)
  (prog (a b c d)
     (cond ((or (null ratout-f) (null g))
	    (return nil))
	   ((null (cddr ratout-f))
	    (return (lsft (pctimes1 (cadr ratout-f) g) (car ratout-f))))
	   ((null (cddr g))
	    (return (lsft (pctimes1 (cadr g) ratout-f) (car g)))))
     (setq d (ash (1+ (max (car ratout-f) (car g))) -1))
     (setq ratout-f (halfsplit ratout-f d) g (halfsplit g d))
     (setq a (fptimes1 (car ratout-f) (car g)))
     (setq b
	   (fptimes1 (ptptplus (car ratout-f) (cdr ratout-f)) (ptptplus (car g) (cdr g))))
     (setq c (fptimes1 (cdr ratout-f) (cdr g)))
     (setq b (ptptdiffer (ptptdiffer b a) c))
     (return (ptptplus (lsft a (ash d 1)) (ptptplus (lsft b d) c)))))

(defun halfsplit (p d)
  (do ((a)
       (p p (cddr p)))
      ((or (null p) (< (car p) d))
       (cons (nreverse a) p))
    (setq a (cons (cadr p)
		  (cons (- (car p) d) a)))))

(defun lsft (p ratout-n)
  (do ((q p (cddr (rplaca q (+ (car q) ratout-n)))))
      ((null q)))
  p)

;;; TO TRUNCATE ON E, DO RATWEIGHT(E,1);
;;;THEN DO RATWTLVL:N.  ALL POWERS >N GO TO 0.

(defmfun $ratweight (&rest args)
  (when (oddp (length args))
    (merror (intl:gettext "ratweight: number of arguments must be a multiple of 2.")))
  (do ((l args (cddr l)))
      ((null l))
    (rplacd (or (assoc (first l) *ratweights :test #'equal)
		(car (push (list (first l)) *ratweights)))
	    (second l)))
  (setq $ratweights (cons '(mlist simp) (dot2l *ratweights)))
  (if (null args)
      $ratweights
      (cons '(mlist) args)))

(defun pweight (ratout-x)
  (or (get ratout-x '$ratweight) 0))

(defun wtptimes (ratout-x ratout-y ratout-wtsofar)
  (cond ((or (pzerop ratout-x)
	     (pzerop ratout-y)
	     (> ratout-wtsofar $ratwtlvl))
	 (pzero))
	((pcoefp ratout-x)
	 (wtpctimes ratout-x ratout-y ratout-wtsofar))
	((pcoefp ratout-y)
	 (wtpctimes ratout-y ratout-x ratout-wtsofar))
	((eq (car ratout-x) (car ratout-y))
	 (palgsimp (car ratout-x)
		   (wtptimes1 (cdr ratout-x)
			      (cdr ratout-y)
			      (pweight (car ratout-x))
			      ratout-wtsofar)
		   (alg ratout-x)))
	((pointergp (car ratout-x) (car ratout-y))
	 (psimp (car ratout-x)
		(wtpctimes1 ratout-y (cdr ratout-x) (pweight (car ratout-x)) ratout-wtsofar)))
	(t
	 (psimp (car ratout-y)
		(wtpctimes1 ratout-x (cdr ratout-y) (pweight (car ratout-y)) ratout-wtsofar)))))

(defun wtptimes1 (ratout-x ratout-y ratout-xweight ratout-wtsofar)
  (let (ratout-v ratout-u*)
    (labels
	((wtptimes2 (ratout-y)
	   (if (null ratout-y)
	       nil
	       (let ((ii (+ (* ratout-xweight (+ (car ratout-x) (car ratout-y)))
			    ratout-wtsofar)))
		 (if (> ii $ratwtlvl)
		     (wtptimes2 (cddr ratout-y))
		     (pcoefadd (+ (car ratout-x) (car ratout-y))
			       (wtptimes (cadr ratout-x) (cadr ratout-y) ii)
			       (wtptimes2 (cddr ratout-y)))))))

	 (wtptimes3 (ratout-y)
	   (prog ((e 0) u c)
	    a1
	      (cond ((null ratout-y)
		     (return nil)))
	      (setq e (+ (car ratout-x) (car ratout-y)))
	      (setq c (wtptimes (cadr ratout-y)
				(cadr ratout-x)
				(+ ratout-wtsofar (* ratout-xweight e))))
	      (cond ((pzerop c)
		     (setq ratout-y (cddr ratout-y))
		     (go a1))
		    ((or (null ratout-v)
			 (> e (car ratout-v)))
		     (setq ratout-u* (setq ratout-v (ptptplus ratout-u* (list e c))))
		     (setq ratout-y (cddr ratout-y))
		     (go a1))
		    ((equal e (car ratout-v))
		     (setq c (pplus c (cadr ratout-v)))
		     (cond ((pzerop c)
			    (setq ratout-u*
				  (setq ratout-v (ptptdiffer ratout-u*
							     (list (car ratout-v)
								   (cadr ratout-v))))))
			   (t
			    (rplaca (cdr ratout-v) c)))
		     (setq ratout-y (cddr ratout-y))
		     (go a1)))
	    a
	      (cond ((and (cddr ratout-v)
			  (> (caddr ratout-v) e))
		     (setq ratout-v (cddr ratout-v))
		     (go a)))
	      (setq u (cdr ratout-v))
	    b
	      (cond ((or (null (cdr u))
			 (< (cadr u) e))
		     (rplacd u (cons e (cons c (cdr u))))
		     (go e)))
	      (cond ((pzerop (setq c (pplus (caddr u) c)))
		     (rplacd u (cdddr u))
		     (go d))
		    (t
		     (rplaca (cddr u) c)))
	    e
	      (setq u (cddr u))
	    d
	      (setq ratout-y (cddr ratout-y))
	      (cond ((null ratout-y)
		     (return nil))
		    ((pzerop
		      (setq c (wtptimes (cadr ratout-x) (cadr ratout-y)
					(+ ratout-wtsofar (* ratout-xweight
						      (setq e (+ (car ratout-x) (car ratout-y))))))))
		     (go d)))
	    c
	      (cond ((and (cdr u)
			  (> (cadr u) e))
		     (setq u (cddr u))
		     (go c)))
	      (go b))))
      (prog ()
	 (setq ratout-v (setq ratout-u* (wtptimes2 ratout-y)))
       a
	 (setq ratout-x (cddr ratout-x))
	 (cond ((null ratout-x)
		(return ratout-u*)))
	 (wtptimes3 ratout-y)
	 (go a)))))

(defun wtpctimes (c p ratout-wtsofar)
  (cond ((pcoefp p)
	 (ctimes c p))
	(t
	 (psimp (car p) (wtpctimes1 c (cdr p) (pweight (car p)) ratout-wtsofar)))))

(defun wtpctimes1 (c ratout-x xwt ratout-wtsofar)
  (prog (cc)
     (return
       (cond ((null ratout-x)
	      nil)
	     (t
	      (setq cc (wtptimes c
				 (cadr ratout-x)
				 (+ ratout-wtsofar (* xwt (car ratout-x)))))
	      (cond ((pzerop cc)
		     (wtpctimes1 c (cddr ratout-x) xwt ratout-wtsofar))
		    (t
		     (cons (car ratout-x)
			   (cons cc
				 (wtpctimes1 c
					     (cddr ratout-x)
					     xwt
					     ratout-wtsofar))))))))))

(defun wtpexpt (ratout-x ratout-n)
  (cond ((= ratout-n 0)
	 1)
	((= ratout-n 1)
	 ratout-x)
	((evenp ratout-n)
	 (let ((xn2 (wtpexpt ratout-x (/ ratout-n 2))))
	   (wtptimes xn2 xn2 0)))
	(t
	 (wtptimes ratout-x (wtpexpt ratout-x (1- ratout-n)) 0))))

(defmfun $horner (e &rest l)
  (let (($ratfac nil)
	(varlist (cdr $ratvars))
	genvar
	(ratout-x nil)
	(arg1 (taychk2rat e)))
    (cond ((mbagp arg1)
	   (cons (car arg1)
		 (mapcar #'(lambda (u) (apply '$horner (cons u l))) (cdr arg1))))
	  (t
	   (setq ratout-x (apply #'$rat (cons arg1 l)))
	   (mapc #'(lambda (ratout-y z) (putprop ratout-y z 'disrep)) (cadddr (car ratout-x)) (caddar ratout-x))
	   (div* (hornrep (cadr ratout-x)) (hornrep (cddr ratout-x)))))))

(defun hornrep (p)
  (if (pcoefp p)
      p
      (horn+ (cdr p) (get (car p) 'disrep))))

(defun horn+ (l var)
  (prog (ans last)
     (setq ans (hornrep (cadr l)))
   a
     (setq last (car l) l (cddr l))
     (cond ((null l)
	    (return (cond ((equal last 0)
			   ans)
			  (t
			   (list '(mtimes)
				 (list '(mexpt) var last) ans)))))
	   (t
	    (setq ans (list '(mplus)
			    (hornrep (cadr l))
			    (list '(mtimes)
				  (list '(mexpt) var (- last (car l)))
				  ans)))))
     (go a)))

(declare-top (special *checkfactors*
		      $factorflag
		      varlist))

(defmfun $partfrac (ratout-exp ratout-var)
  (cond ((mbagp ratout-exp)
	 (cons (car ratout-exp)
	       (mapcar #'(lambda (u)
			   ($partfrac u ratout-var))
		       (cdr ratout-exp))))
	((and (atom ratout-var)
	      (not (among ratout-var ratout-exp)))
	 ratout-exp)
	(t
	 (let (($savefactors t) (*checkfactors* ()) (varlist (list ratout-var))
	       $ratfac $algebraic $keepfloat ratform genvar)
	   (desetq (ratform . ratout-exp) (taychk2rat ratout-exp))
	   (setq ratout-var (caadr (ratf ratout-var)))
	   (setq ratout-exp (partfrac ratout-exp ratout-var))
	   (setq ratout-exp (cons (car ratout-exp) ;FULL DECOMP?
				  (mapcan #'partfraca (cdr ratout-exp))))
	   (add2* (disrep (car ratout-exp) ratform)
		  (cons '(mplus)
			(mapcar #'(lambda (l)
				    (destructuring-let (((coef poly ratout-exp) l))
				      (list '(mtimes)
					    (disrep  coef ratform)
					    (list '(mexpt)
						  (disrep poly ratform)
						  (- ratout-exp)))))
				(cdr ratout-exp))))))))

(defun partfraca (llist)
  (destructuring-let (((coef poly ratout-exp) llist))
    (do ((nc (ratdivide coef poly) (ratdivide (car nc) poly))
	 (ratout-n ratout-exp (1- ratout-n))
	 (ans))
	((rzerop (car nc)) (cons (list (cdr nc) poly ratout-n) ans))
      (push (list (cdr nc) poly ratout-n) ans))))

(defun partfrac (rat ratout-var)
  (destructuring-let* (((ratout-wholepart frpart) (pdivide (car rat) (cdr rat)))
		       ((num . denom) (ratqu frpart (cdr rat))))
    (cond
      ((pzerop num)
       (cons ratout-wholepart nil))
      ((or (pcoefp denom)
	   (pointergp ratout-var (car denom)))
       (cons rat nil))
      (t
       (destructuring-let (((content bpart) (oldcontent denom)))
         (let (apart ratout-y ratout-parnumer)
           (loop
             for (factor multiplicity)
               on (pfactor bpart) by #'cddr
             unless (zerop (pdegree factor ratout-var))
               do
                  (setq apart (pexpt factor multiplicity)
                        bpart (pquotient bpart apart)
                        ratout-y (bprog apart bpart ratout-var)
                        frpart (cdr (ratdivide (ratti num (cdr ratout-y) t)
                                               apart)))
                  (push (list (ratqu frpart content) factor multiplicity)
                        ratout-parnumer)
                  (desetq (num . content)
                          (cdr (ratdivide (ratqu (ratti num (car ratout-y) t)
                                                 content)
                                          bpart))))
           (cons ratout-wholepart ratout-parnumer)))))))

(declare-top (unspecial *y*))

;; $RATDIFF TAKES DERIVATIVES FAST.  IT ASSUMES THAT THE
;; ONLY ENTITY WHICH DEPENDS ON X IS X ITSELF.
;; THAT IS, DEPENDENCIES DECLARED EXPLICITLY OR IMPLICITLY ARE
;; TOTALLY IGNORED.  RATDIFF(F(X),X) IS 0.  RATDIFF(Y,X) IS 0.
;; ANY OTHER USAGE MUST GO THROUGH $DIFF.
;; FURTHERMORE, X IS ASSUMED TO BE AN ATOM OR A SINGLE ITEM ON
;; VARLIST.  E.G. X MIGHT BE SIN(U), BUT NOT 2*SIN(U).

(defmfun $ratdiff (p ratout-x)
  (if ($ratp p)
      (setq p (minimize-varlist
	       (if (member 'trunc (cdar p) :test #'eq)
		   ($taytorat p)
		   p))))
  (let ((formflag ($ratp p))
	(varlist)
	(genvar))
    (newvar ratout-x)
    (newvar p)
    (or (every #'(lambda (exp)
		     (or (alike1 ratout-x exp) (free exp ratout-x)))
		 varlist)
	(merror (intl:gettext "ratdiff: first argument must be a polynomial in ~M; found: ~M") ratout-x p))
    (setq p (ratf p))
    (setq ratout-x (caadr (ratf ratout-x)))
    (setq p (cons (car p) (ratderivative (cdr p) ratout-x)))
    (if formflag
	p
	($ratdisrep p))))


(defmfun $pfet (m)
  (labels
      ((sssqfr (x)
	 (let ((dosimp t))
	   (simplify ($sqfr x))))

       (pfet1 (m v)
	 (cond ((atom m) m)
	       ((eq (caar m) 'mplus)
		(cons '(mplus)
		      (mapcar #'(lambda (s) ($partfrac s v))
			      (cdr m))))
	       (t
		($partfrac m v)))))
    (prog (listov $pfeformat varlist $factorflag)
       (setq $pfeformat t)
       (newvar m)
       (setq listov varlist)
       (mapc #'(lambda (r) (setq m (pfet1 m r)))
	     listov)
       (setq m (simplify m))
       (setq m (cond ((atom m) m)
		     ((eq (caar m) 'mplus)
		      (cons '(mplus)
			    (mapcar #'$ratexpand (cdr m))))
		     (t ($ratexpand m))))
       (return (cond ((atom m) m)
		     ((eq (caar m) 'mplus)
		      (cons '(mplus)
			    (mapcar #'sssqfr (cdr m))))
		     (t (sssqfr m)))))))

