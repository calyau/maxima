;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module homog)

(LOAD-MACSYMA-MACROS RATMAC)

(declare-top (special *hvar *hmat)
	 (*lexpr hreduce hexpand))

(defun addvardeg (n l lt)
       (mapc (fn (x) (push (cons n x) lt)) l)
       lt)

(defun ptermvec (p)
       (ltermvec p (sort (listovars p) #'pointergp) nil))

(defun ltermvec (p vl coef?)
       (cond ((null vl) (list (if coef? p nil)))
	     ((pcoefp p) (list (nzeros (length vl) (if coef? p nil))))
	     ((pointergp (car vl) (car p))
	      (addvardeg 0 (ltermvec p (cdr vl) coef?) nil))
	     (t (do ((p (cdr p) (cddr p))
		     (lt nil (addvardeg (car p) (ltermvec (cadr p) (cdr vl)
							  coef?)
					lt)))
		    ((null p) lt)))))

;car(lv) = list of dependent equations
;caddr (lv) = correspondence between new columns and old ones.

(defun hlinsolve (mat)
       (let ((n (f1- (length mat))) (m (length (car mat))) arr ndepvar
	      (mat (mapcar (fn (x) (mapcar '- (car mat) x)) (cdr mat))))
	     (setq arr (array nil t (f1+ (max m n)) (f+ 2 m)))
	     (do ((ml mat (cdr ml))			;solving for m vars
		  (i 1 (f1+ i)))
		 ((null ml))
		 (do ((l (car ml) (cdr l))
		      (j 1 (f1+ j)))
		     ((null l) (store (arraycall t arr i j) 0))
		     (store (arraycall t arr i j) (car l))))
	     (setq mat (tfgeli1 arr n (f1+ m)))
	     (and (cadr mat) (merror "Inconsistent"))	;shouldn't happen
; # indep equations = n - (car mat)
; # dependent vars = # indep equations
; # indep vars = m - # dependent vars
	     (setq ndepvar (f- n (length (car mat))))
	     (do ((i (f1+ n) (f1+ i))) ((> i m))
		 (do ((j (f1+ ndepvar) (f1+ j))) ((> j m))
		     (store (arraycall t arr i j) 0)))
	     (do ((i (f1+ ndepvar) (f1+ i))
		  (det (abs (arraycall t arr 1 1))))
		 ((> i m))
		 (store (arraycall t arr i i) det))
	     (cond ((signp g (arraycall t arr 1 1))
		    (do ((i 1 (f1+ i))) ((> i ndepvar))
			(do ((j (f1+ ndepvar) (f1+ j))) ((> j m))
			    (store (arraycall t arr i j)
				   (f- (arraycall t arr i j)))))))
	     (do ((l (caddr mat) (cdr l))		;invert var permutation
		  (i 1 (f1+ i)))
		 ((null l))
		 (store (arraycall t arr 0 (car l)) i))
	     (do ((varord (caddr mat) (cdr varord))
		  (i 1 (f1+ i)))
		 ((> i ndepvar)
		  (do ((varord varord (cdr varord))
		       (i i (f1+ i)))
		      ((> i m)
		       (do ((ans)
			    (i m (f1- i)))
			   ((< i 1) (*rearray arr) ans)
			   (push (arraycall t arr i 0) ans)))
		      (do ((vecl)
			   (j m (f1- j)))
			  ((< j 1)
			   (store (arraycall t arr (car varord) 0) vecl))
			  (push (arraycall t arr
					   (arraycall t arr 0 j) i) vecl))))
		 (do ((gcd 0)
		      (j i (f1+ j)))
		     ((or (= gcd 1) (> j m))
		      (store (arraycall t arr (car varord) 0)
			     (abs (// (arraycall t arr i i) gcd))))
		     (setq gcd (gcd gcd (arraycall t arr i j)))))
	     ))						
; returns (mixed list of <reduced exp> and <basis vector for null space>)
; <reduced expon> corresponds to dependent var
; <basis vector> corresponds to independent var

(defun hreduce (p &optional (vl (setq *hvar (sort (listovars p) 'pointergp)))
		      (hl (setq *hmat (hlinsolve (ltermvec p *hvar nil)))))
       (cond ((pcoefp p) p)
	     ((pointergp (car vl) (car p))
	      (hreduce p (cdr vl) (cdr hl)))
	     ((numberp (car hl))
	      (cons (car p)
		    (do ((p (cdr p) (cddr p))
			 (red (car hl))
			 (ans))
			((null p) (nreverse ans))
			(push (// (car p) red) ans)
			(push (hreduce (cadr p) (cdr vl) (cdr hl)) ans))))
	     (t (do ((p (cdddr p) (cddr p))
		     (sum (hreduce (caddr p) (cdr vl) (cdr hl))
			  (pplus sum (hreduce (cadr p) (cdr vl) (cdr hl)))))
		    ((null p) sum)))))

(defun hexpand (p &optional (hl *hmat) (vl *hvar))
  (if (andmapc #'onep hl) p
    (progn
       (do ((hl hl (cdr hl))
	   (i 1 (f1+ i))
	   (pl (ltermvec p vl t)))
	  ((null hl) (setq p pl))
	(if (and (numberp (car hl)) (not (onep (car hl))))
	    (do ((pl pl (cdr pl))) ((null pl))
		(do ((term (car pl) (cdr term))
		     (j (f1- i) (f1- j)))
		    ((= j 0) (rplaca term (f* (car term) (car hl))))))))
      (do ((hl hl (cdr hl))
	   (i 1 (f1+ i))
	   (wtlist)
	   (newwt))
	  ((null hl) (hsimp p vl))
	(unless (numberp (car hl))
		(setq wtlist (mapcar (fn (x) (hdot (car hl) x)) p))
		(setq newwt (nth (f1- i) (car hl)))
		(do ((maxwt (apply 'max wtlist))
		     (pl p (cdr pl))
		     (wtlist wtlist (cdr wtlist)))
		    ((null pl))
		  (do ((term (car pl) (cdr term))
		       (j (f1- i) (f1- j)))
		      ((= j 0) (rplaca term (// (f- maxwt (car wtlist))
						newwt))))))))))

(defun hdot (ht pt)
       (do ((ht (cdr ht) (cdr ht))
	    (pt (cdr pt) (cdr pt))
	    (sum (f* (car ht) (car pt))
		 (f+ sum (f* (car ht) (car pt)))))
	   ((null ht) sum)))

(defun hsimp (pl vl)
       (do ((pl (cdr pl) (cdr pl))
	    (p (hsimp1 (car pl) vl)
	       (pplus p (hsimp1 (car pl) vl))))
	   ((null pl) p)))

(defun hsimp1 (tl vl)
       (cond ((null vl) tl)
	     ((> (car tl) 0)
	      (list (car vl) (car tl) (hsimp1 (cdr tl) (cdr vl))))
	     (t (hsimp1 (cdr tl) (cdr vl)))))
