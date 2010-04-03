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
      (merror (intl:gettext "sort: first argument must be a list; found: ~M") llist))
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
  (if (functionp fun)
      fun
      #+(or cmu scl)
      (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))
      #-(or cmu scl)
      #'(lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))))

(defun lessthan (a b)
  (great b a))

(defmspec $makelist (x)
  (setq x (cdr x))
  (prog (n form arg a b lv d)
     (setq n (length x))
     (if (or (< n 3) (> n 4))
	 (merror (intl:gettext "makelist: must have 3 or 4 arguments.")))
     (setq form (car x)
	   arg (cadr x)
	   a (meval (caddr x))
	   lv (cond ((= n 3) 
		     (if ($listp a)
			 (mapcar #'(lambda (u) (list '(mquote) u)) (cdr a))
			 (merror (intl:gettext "makelist: third argument must evaluate to a list; found: ~M") a)))
		    (t
		     (setq b (meval (cadddr x)))
		     (if (or (not (integerp (setq d (sub* b a)))) (< d -1))
			 (merror (intl:gettext "makelist: fourth argument minus third must be a nonnegative integer; found: ~M") d)
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
    (merror (intl:gettext "sublist: first argument must be a list; found: ~M") a) )
  (do ((a (cdr a) (cdr a))
       (x))
      ((null a) (cons '(mlist simp) (nreverse x)))
    (if (definitely-so (mfuncall f (car a)))
	(push (car a) x))))
