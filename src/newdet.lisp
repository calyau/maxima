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
(macsyma-module newdet)

;; THIS IS A VERSION OF THE GENTLEMAN-JOHNSON TREE-MINOR DETERMINANT
;; USING RATIONAL FUNCTIONS.  "A" CAN BE A MATRIX OR AN ARRAY.
;; ANSWER IS IN RATIONAL FORM.
;; RJF  5/2/73

(declare-top(special vlist varlist genvar aryp)
	    #-cl
	    (fixnum rr k j old new *binom* *i* pascal n m))
;;	 (ARRAY* (NOTYPE *INPUT* 2 *BINOM* 2 *MINOR1* 2 *i* 1)

;;these are general type arrays
(declare-top  (special *input*  *binom*  *minor1*  *i* ))

(defmfun $newdet n
  ((lambda (a)
     (cond ((= n 2)
	    (cond ((not (integerp (arg 2)))
		   (merror "Wrong arg to `newdet': ~M" (arg 2))))
	    (setq a (arg 1) n (arg 2)))
	   ((and (= n 1) ($matrixp (setq a (arg 1))))
	    (setq n (length (cdr (arg 1)))))
	   (t (merror "Wrong number of args to `newdet'")))
     (newdet a n nil))
   nil))

(defmfun $permanent n		
  ((lambda (a)
     (cond ((= n 2)
	    (cond ((not (integerp (arg 2)))
		   (merror "Wrong arg to `perm': ~M" (arg 2))))
	    (setq a (arg 1) n (arg 2)))
	   ((and (= n 1) ($matrixp (setq a (arg 1))))
	    (setq n (length (cdr (arg 1)))))
	   (t (merror "Wrong number of args to `perm'")))
     (newdet a n t))
   nil))

(defun newdet (a n perm)
  (prog (rr r k j old new vlist m loc addr sign) 
     (cond ((> n 50.)
	    (merror "Array too big - `newdet': ~M" n)))
     (setq  *binom* (*array nil t (add1 n) (add1 n)))
     (setq  *minor1* (*array nil t 2. (add1 (setq rr (pascal n)))))
     (setq  *i* (*array nil t (plus 2. n)))
     (do ((k
	   0.
	   (add1 k)))
	 ((> k 1.))
       (do ((j
	     0.
	     (add1 j)))
	   ((> j rr))
	 (store (aref *minor1* k j) '(0. . 1.))))
     (do ((k 0. (add1 k))) ((> k (add1 n))) (store (aref *i* k) -1.))
     (setq  *input* (*array nil t (add1 n) (add1 n)))
     (do ((k
	   1.
	   (add1 k)))
	 ((> k n))
       (do ((j
	     1.
	     (add1 j)))
	   ((> j n))
	 (newvar1 (store (aref *input* k j)
			 ((lambda (aryp)
			    #+cl
			    (maref a k j)
			    #-cl
			    (meval (list (list a 'array) k j))
			    )
			  t)))))
     (newvar (cons '(mtimes) vlist))
     (do ((k
	   1.
	   (add1 k)))
	 ((> k n))
       (do ((j
	     1.
	     (add1 j)))
	   ((> j n))
	 (store (aref *input* k j)
		(cdr (ratrep* (aref *input* k j))))))
     (setq new 1.)
     (setq old 0.)
     (store (aref *i* 0.) n)
     (do ((loc
	   1.
	   (add1 loc)))
	 ((> loc n))
       (store (aref *minor1* old (sub1 loc)) (aref *input* 1. loc)))
     (setq m 1.)
     g0193(cond ((> m (sub1 n)) (go ret)))
     (setq loc 0.)
     (setq j 1.)
     g0189(cond ((> j m) (go nextminor)))
     (store (aref *i* j) (difference m j))
     (setq j (f+ j 1.))
     (go g0189)
     nextminor
     (cond ((not (equal (aref *minor1* old loc) '(0. . 1.)))
	    (setq k (sub1 n))
	    (setq j 0.)
	    (setq addr (plus loc (aref *binom* k (add1 m))))
	    (setq sign 1.))
	   (t (go over)))
     nextuse
     (cond
       ((equal k (aref *i* (add1 j)))
	(setq j (add1 j))
	(setq sign (minus sign)))
       (t
	(store
	 (aref *minor1* new addr)
	 (ratplus
	  (aref *minor1* new addr)
	  (rattimes (aref *minor1* old loc)
		    (cond ((or (equal sign 1.) perm)
			   (aref *input* (add1 m) (add1 k)))
			  (t (ratminus (aref *input* (add1 m)
					     (add1 k)))))
		    t)))))
     (cond ((> k 0.)
	    (setq k (sub1 k))
	    (setq addr
		  (difference addr
			      (aref *binom* k (difference m j))))
	    (go nextuse)))
     (store (aref *minor1* old loc)  '(0 . 1))
     over (setq loc (add1 loc))
     (setq j m)
     back (cond ((> 1. j) (setq m (add1 m))(setq old(difference 1 old))(setq new (difference 1 new))(go g0193)))
     (store (aref *i* j) (add1 (aref *i* j)))
     (cond ((> (aref *i* (sub1 j)) (aref *i* j)) (go nextminor))
	   (t (store (aref *i* j) (difference m j))))
	
     (setq j (sub1 j))
     (go back)
     ret(*rearray '*binom*)
     (*rearray '*input*)
     (setq r (cons (list 'mrat
			 'simp
			 varlist
			 genvar)
		   (aref *minor1* old 0.)))
     (*rearray '*minor1*)
     (return r)))

(defun pascal (n) 
  (prog nil 
     (store (aref *binom* 0. 0.) 1.)
     (do ((h
	   1.
	   (add1 h)))
	 ((> h n))
       (store (aref *binom* h 0.) 1.)
       (store (aref *binom* (sub1 h) h) 0.)
       (do ((j
	     1.
	     (add1 j)))
	   ((> j h))
	 (store (aref *binom* h j)
		(plus (aref *binom* (sub1 h) (sub1 j))
		      (aref *binom* (sub1 h) j)))))
     (return (sub1 (aref *binom* n (lsh n -1.))))))

;;these need to be special in so many places please dont unspecial them..
;;(DECLARE (UNSPECIAL VLIST VARLIST GENVAR ARYP))
