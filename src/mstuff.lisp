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

(defmfun $sort (l &optional (f 'lessthan))
  (let ((llist l) comparfun bfun)
    (unless ($listp llist)
      (merror "The first argument to `sort' must be a list:~%~M" llist))
    (setq llist (copy-list (cdr llist))
	  comparfun 
	  (mfunction1 (setq bfun (getopr f))))
    (when (member bfun '(lessthan great) :test #'eq)
      (setq llist (mapcar #'ratdisrep llist)))
    (cons '(mlist simp) (sort llist comparfun))))

;; cmulisp does not like the closure version.  Clisp insists on the
;; closure version.  Gcl likes either...  For the moment we will
;; leave a conditional here.
(defun mfunction1 (fun)
  #+(or cmu scl)
  (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))
  #-(or cmu scl)
  #'(lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y)))))

(defun lessthan (a b)
  (great b a))

(defmspec $makelist (x)
  (setq x (cdr x))
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
			 (merror "If 3 arguments are given to MAKELIST, ~
the 3rd argument should evaluate to a list:~%~M" a)))
		    (t
		     (setq b (meval (cadddr x)))
		     (if (or (not (fixnump (setq d (sub* b a)))) (< d -1))
			 (merror "If 4 arguments are given to MAKELIST, the difference of the 3rd ~
and 4th arguments should evaluate to a non-negative integer:~%~M" d)
			 (interval a b)))))
     (return 
       (do ((lv lv (cdr lv))
	    (ans))
	   ((null lv) (cons '(mlist simp) (nreverse ans)))
	 (push (meval `(($ev)
			,@(list (list '(mquote) form)
				(list '(mequal simp) arg (car lv)))))
	       ans)))))

(defun interval (i j)
  (do ((nn i (add2 1 nn))
       (m 0 (1+ m))
       (k (sub* j i))
       (ans))
      ((> m k) (nreverse ans))
    (push nn ans)))

(defmfun $sublist (a f)
  (unless ($listp a)
    (merror "The first argument to `sublist' must be a list:~%~M" a) )
  (do ((a (cdr a) (cdr a))
       (x))
      ((null a) (cons '(mlist simp) (nreverse x)))
    (if (mevalp (list (ncons f) (car a)))
	(push (car a) x))))
