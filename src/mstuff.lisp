;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module mstuff)

(declare-top(splitfile msort) (fixnum n))

(defmfun $sort n
  (if (or (= n 0) (> n 2)) (merror "`sort' takes 1 or 2 arguments."))
  (let ((llist (arg 1)) comparfun bfun)
    (if (not ($listp llist))
	(merror "The first argument to `sort' must be a list:~%~M" llist))
    (setq llist (copy-top-level (cdr llist) )
	  comparfun 
	  (mfunction1 (setq bfun (if (= n 2) (getopr (arg 2)) 'lessthan))))
    (if (memq bfun '(lessthan great))
	(setq llist (mapcar #'ratdisrep llist)))
    (cons '(mlist simp) (sort llist comparfun))))

;; old non closure version
;;(DEFUN MFUNCTION1 (FUN)
;;  `(LAMBDA (X Y) (MEVALP `((,',FUN) ((MQUOTE) ,X) ((MQUOTE) ,Y)))))

;; cmulisp does not like the closure version.  Clisp insists on the
;; closure version.  Gcl likes either...  For the moment we will
;; leave a conditional here.
(defun mfunction1 (fun)
  #+(or cmu scl)
  (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))
  #-(or cmu scl)
  (function (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y)))))
  )

(defun lessthan (a b) (if (great b a) t))

(declare-top (splitfile makel))

(defmspec $makelist (x) (setq x (cdr x))
	  (prog (n form arg a b lv d)
	     (setq n (length x))
	     (if (or (< n 3) (> n 4))
		 (merror "`makelist' takes 3 or 4 arguments."))
	     (setq form (car x)
		   arg (cadr x)
		   a (meval (caddr x))
		   lv (cond ((= n 3) 
			     (if ($listp a)
				 (mapcar #'(lambda (u) (list '(mquote) u)) (cdr a))
				 (merror "
If 3 arguments are given to MAKELIST,
the 3rd argument should evaluate to a list:~%~M" a)))
			    (t
			     (setq b (meval (cadddr x)))
			     (if (or (not (fixnump (setq d (sub* b a)))) (< d -1))
				 (merror "
If 4 arguments are given to MAKELIST, the difference of the 3rd
and 4th arguments should evaluate to a non-negative integer:~%~M" d)
				 (interval a b)))))
	     (return 
	       (do ((lv lv (cdr lv)) (ans))
		   ((null lv) (cons '(mlist simp) (nreverse ans)))
		 (setq ans (cons (meval `(($ev)
					  ,@(list (list '(mquote) form)
						  (list '(mequal simp) 
							arg 
							(car lv)))))
				 ans))))))

(defun interval (i j)
  (do ((nn i (add2 1 nn)) (m 0 (f1+ m)) (k (sub* j i)) (ans))
      ((> m k) (nreverse ans))
    (setq ans (cons nn ans))))

(defmfun $sublist (a f)
  (if ($listp a)
      (do ((a (cdr a) (cdr a)) (x))
	  ((null a) (cons '(mlist simp) (nreverse x)))
	(if (mevalp (list (ncons f) (car a)))
	    (setq x (cons (car a) x))))
      (merror "The first argument to `sublist' must be a list:~%~M" a)))

;; Undeclarations for the file:
#-nil
(declare-top(notype n))


