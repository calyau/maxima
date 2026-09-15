;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mstuff)

(defmfun $sort (l &optional (f 'lessthan))
  (let ((llist l) comparfun bfun ($prederror t))
    (unless ($listp llist)
      (merror (intl:gettext "sort: first argument must be a list; found: ~M") llist))
    (setq llist (copy-list (cdr llist))
	  comparfun 
	  (mfunction1 (setq bfun (getopr f))))
    (when (member bfun '(lessthan great) :test #'eq)
      (setq llist (mapcar #'ratdisrep llist)))
    (simplifya (cons '(mlist) (stable-sort llist comparfun)) t)))

;; cmulisp does not like the closure version.  Clisp insists on the
;; closure version.  Gcl likes either...  For the moment we will
;; leave a conditional here.
(defun mfunction1 (fun)
  (if (functionp fun)
      (lambda (x y) (mevalp (funcall fun x y)))
      #+(or cmu scl)
      (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))
      #-(or cmu scl)
      #'(lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))))

(defun lessthan (a b)
  (great b a))

;; The argument shapes of MAKELIST, reduced to one plan: the body, the
;; variable to bind, and the values to bind it to in turn.  The one- and
;; two-argument shapes bind no variable, so ARG comes back NIL and the
;; values are placeholders whose only content is how many there are.
;;
;; $MAKELIST and $PARALLEL_MAKELIST share this so that the two cannot
;; come to disagree about what they accept or how many elements they
;; produce -- which is exactly what a test comparing them would have to
;; assume, and could not check.
(defun makelist-plan (x)
  (let ((n (length x)) form arg a b c d lv)
    (cond
      ((= n 0) (values nil nil '()))
      ((= n 1) (values (first x) nil '(nil)))
      ((= n 2)
       (setq form (first x))
       (setq b (let (($simp t)) ($float (meval (second x)))))
       (if (numberp b)
           (values form nil (loop for m from 1 while (not (> m b))
                                  collect nil))
           (merror (intl:gettext "makelist: second argument must evaluate to a number; found: ~M") b)))
      ((= n 3)
       (setq form (first x))
       (setq arg (second x))
       (setq b (meval (third x)))
       (if ($listp b)
           (values form arg (mapcar #'(lambda (u) (list '(mquote) u)) (cdr b)))
           (progn
             (setq b (let (($simp t)) ($float (meval b))))
             (if ($numberp b)
                 (values form arg (loop for m from 1 while (not (> m b))
                                        collect m))
                 (merror (intl:gettext "makelist: third argument must be a number or a list; found: ~M") b)))))
      ((= n 4)
       (setq form (first x))
       (setq arg (second x))
       (setq a (meval (third x)))
       (setq b (meval (fourth x)))
       (setq d (let (($simp t)) ($float (meval `((mplus) ,b ((mtimes) ,a -1))))))
       (if (numberp d)
           (values form arg (interval2 a 1 d))
           (merror (intl:gettext "makelist: the fourth argument minus the third one must evaluate to a number; found: ~M") d)))
      ((= n 5)
       (setq form (first x))
       (setq arg (second x))
       (setq a (meval (third x)))
       (setq b (meval (fourth x)))
       (setq c (meval (fifth x)))
       (setq d (let (($simp t)) ($float
                (meval
                 `((mtimes) ((mplus) ,b ((mtimes) ,a -1)) ((mexpt) ,c -1))))))
       (if (numberp d)
           (values form arg (interval2 a c d))
           (merror (intl:gettext "makelist: the fourth argument minus the third one, divided by the fifth one must evaluate to a number; found: ~M") d)))
      (t (merror (intl:gettext "makelist: maximum 5 arguments allowed; found: ~M.~%To create a list with sublists, use nested makelist commands.") n)))))

;; One element of a MAKELIST, as a form to be MEVALed.  Built in the
;; calling thread rather than in the worker that evaluates it, so that a
;; parallel run and a serial run evaluate identical forms.
(defun makelist-element-form (form arg value)
  (if arg
      `(($ev) ,(list '(mquote) form) ,(list '(mequal) arg value))
      `(($ev) ,(list '(mquote) form))))

(defmspec $makelist (x)
  (multiple-value-bind (form arg lv) (makelist-plan (cdr x))
    (simplifya
     (cons '(mlist)
           (mapcar #'(lambda (value)
                       (meval (makelist-element-form form arg value)))
                   lv))
     t)))

(defmspec $parallel_makelist (x)
  (multiple-value-bind (form arg lv) (makelist-plan (cdr x))
    (simplifya
     (cons '(mlist)
           (call-in-parallel
            (mapcar #'(lambda (value)
                        (let ((element (makelist-element-form form arg value)))
                          #'(lambda () (meval element))))
                    lv)
            ;; The loop variable is bound by MSET into the symbol's one
            ;; global value cell, so every runner needs its own binding
            ;; of it or they overwrite each other's iteration.
            (and arg (atom arg) (list arg))))
     t)))

(defun interval2 (i s d)
  (do ((nn i (let (($simp t)) (meval `((mplus) ,s ,nn))))
       (m 0 (1+ m))
       (ans))
      ((> m d) (nreverse ans))
    (push nn ans)))

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
      ((null a) (simplifya (cons '(mlist) (nreverse x)) t))
    (if (definitely-so (mfuncall f (car a)))
	(push (car a) x))))
