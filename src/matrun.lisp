;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module matrun)

;;; TRANSLATION properties for the FSUBRs in this file
;;; can be found in MAXSRC;TRANS5 >.  Be sure to check on those
;;; if any semantic changes are made.

(declare-top (genprefix m_)
	     (*lexpr $factor $ldisp)
	     (special *expr *rulelist $rules $factorflag
		      $maxapplyheight $maxapplydepth)
	     (fixnum $maxapplyheight $maxapplydepth max depth))

;;  $MAXAPPLYDEPTH is the maximum depth within an expression to which
;;  APPLYi will delve.  If $MAXAPPLYDEPTH is 0, it is applied only to 
;;  top level.
(defmvar $maxapplydepth 10000.)

;;  If $MAXAPPLYHEIGHT is 0, only atoms are affected by $APPLYB1 and 
;;  $APPLYB2.
(defmvar $maxapplyheight 10000.)

(defmvar matchreverse nil)

(defmspec $disprule (l) (setq l (cdr l))
	  (cond ((cdr l) (disprule1 l))
		((not (eq (car l) '$all)) (consrule (car l)))
		(t (disprule1 (cdr $rules)))))

(defun disprule1 (l)
  (do ((l l (cdr l))) ((null l)) ($ldisp (consrule (car l))))
  '$done)

(defun consrule (x)
  (let ((rule (mget x '$rule)))
    (if rule (list '(msetq simp) x (cons '(marrow simp) (cdr rule)))
	(merror "~:M not a user rule" x))))

(defmfun $remrule (op rule) 
  (prog (rules) 
     (setq op (getopr op))
     (cond ((not (eq rule '$all))
	    (removerule op rule) (return (getop op)))
	   ((null (setq rules (mget op 'oldrules)))
	    (merror "~:@M has no rules" op)))
     next (cond ((or (null rules) (null (cdr rules)))
		 (mputprop op 1 'rulenum) (return (getop op)))
		(t (removerule op (car rules))
		   (setq rules (cdr rules)) (go next)))))

(defun removerule (op rule) 
  (prog (oldrules old othrulename othrule) 
     (setq oldrules (mget op 'oldrules))
     (cond ((or (null rule) (null (setq oldrules (zl-member rule oldrules))))
	    (merror "~:M - no such rule." rule))
	   ((null (car (setq oldrules (cdr oldrules))))
	    (setq oldrules (cdr oldrules))
	    (setq othrulename 'simpargs1)
	    (setq othrule #'(lambda (a b c) (simpargs a c))))
	   (t (setq othrulename (car oldrules))
	      (setq othrule (cadr (getl (car oldrules) '(expr subr))))))
     (putprop rule othrule 'expr)
     (setq old (cdr (zl-member rule (reverse (mget op 'oldrules)))))
     (if old (putprop (car old)
		      (subst othrulename rule (get (car old) 'expr))
		      'expr))
     (if (boundp rule) (makunbound rule))
     (mremprop rule '$rule)
     (mremprop rule '$ruletype)
     (mremprop rule 'ruleof)
     (remprop rule 'expr)
     (delq rule $rules 1)
     (putprop rule othrulename 'expr)
     (if (eq (get op 'operators) rule)
	 (putprop op othrulename 'operators))
     (return (mputprop op (delq rule (mget op 'oldrules)) 'oldrules))))

(defmfun findbe (e)
  (cond ((equal e 1) '(1 . 0))
	((equal e 0) '(0 . 1))
	((atom e) (cons e 1))
	((eq (caar e) 'mexpt) (cons (cadr e) (caddr e)))
	(t (cons e 1))))

(defmfun findfun (e p c)
  (prog nil
     (cond ((and (null (atom e)) (eq (caar e) p)) (return e))
	   ((or (atom e) (not (eq (caar e) c))) (matcherr))
	   ((and (null matchreverse) (memq c '(mplus mtimes)))
	    (setq e (reverse (cdr e))) (go b)))
     a    (setq e (cdr e))
     b    (cond ((null e) (matcherr))
		((and (not (atom (car e))) (eq (caaar e) p)) (return (car e))))
     (go a)))

(defmfun findexpon (e1 base* c)
  (prog (e)
     (setq e e1)
     (cond ((and (mexptp e) (alike1 base* (cadr e)))
	    (return (caddr e)))
	   ((or (atom e) (not (eq (caar e) c))) (go c))
	   ((and (null matchreverse) (memq c '(mplus mtimes)))
	    (setq e (reverse (cdr e))) (go b)))
     a    (setq e (cdr e))
     b    (cond ((null e) (go c))
		((and (mexptp (car e)) (alike1 base* (cadar e)))
		 (return (caddar e))))
     (go a)
     c    (cond ((or (and (not (atom e1)) (memq c '(mplus mtimes))
			  (eq c (caar e1)) (memalike base* e1))
		     (alike1 e1 base*)
		     (and (not (atom base*)) (eq c (caar base*))))
		 (return 1))
		((eq c 'mexpt) (matcherr))
		(t (return 0)))))

(defmfun findbase (e expon c)
  (prog nil
     (cond ((equal expon 0)
	    (if (and (eq c 'mexpt) (not (equal 1 e))) (matcherr))
	    (return 1))
	   ((equal expon 1) (return e))
	   ((and (numberp expon) (greaterp expon 0) (equal e 0))
	    (return 0))
	   ((and (mexptp e) (alike1 expon (caddr e)))
	    (return (cadr e)))
	   ((or (atom e) (not (eq (caar e) c))) (matcherr))
	   ((and (null matchreverse) (memq c '(mplus mtimes)))
	    (setq e (reverse (cdr e))) (go b)))
     a    (setq e (cdr e))
     b    (cond ((null e)
		 (return (if (and (numberp expon) (minusp expon)) 1 0)))
		((and (mexptp (car e)) (alike1 expon (caddar e)))
		 (return (cadar e))))
     (go a)))

(defmfun part+ (e p preds) 
  (prog (flag saved val) 
     (cond ((> (length p) (length preds))
	    (setq p (reverse p))
	    (setq p (nthkdr p (f- (length p) (length preds))))
	    (setq p (nreverse p))))
     (setq e ($ratexpand e))
     (setq e (cond ((not (mplusp e)) (ncons e)) (t (cdr e))))
     a    (cond ((null p) (cond ((null e) (return t)) (t (matcherr))))
		((and (cdr preds) (memq (car (caddar preds)) '(msetq setq)))
		 (cond (flag (merror "Two or more pattern variables TRUE"))
		       (t (setq flag t p (reverse p) preds (reverse preds))
			  (go a))))
		(t (mset (car p) 0)))
     (setq saved 0)
     (mapc 
      #'(lambda (z) 
	  (cond ((null (setq val (catch 'match (mcall (car preds) z)))) nil)
		(t (setq saved (add2* saved val))
		   (setq e (zl-delete z e 1)))))
      e)
     (cond ((and (equal saved 0)
		 (null (setq val (catch 'match (mcall (car preds) 0)))))
	    (matcherr)))
     (mset (car p) saved)
     (setq preds (cdr preds) p (cdr p))
     (go a)))

(defmfun part* (e p preds) 
  (prog (flag saved val $factorflag) 
     (cond ((> (length p) (length preds))
	    (setq p (reverse p))
	    (setq p (nthkdr p (f- (length p) (length preds))))
	    (setq p (nreverse p))))
     (setq e ($factor e))
     (setq e (cond ((not (mtimesp e)) (ncons e)) (t (cdr e))))
     a    (cond ((null p) (cond ((null e) (return t)) (t (matcherr))))
		((and (cdr preds) (memq (car (caddar preds)) '(msetq setq)))
		 (cond (flag (merror "Two or more pattern variables TRUE"))
		       (t (setq flag t p (reverse p) preds (reverse preds))
			  (go a))))
		((not (atom (car p)))
		 (prog (mye) 
		    (setq mye e)
		    loop (cond ((null mye) (matcherr)))
		    (setq val (catch 'match (mcall (car preds) (car mye))))
		    (cond ((null val)
			   (setq mye (cdr mye)) (go loop))
			  (t (return (setq e (zl-delete (car mye) e 1))))))
		 (go b))
		(t (mset (car p) 1)))
     (setq saved 1)
     (mapc 
      #'(lambda (z) (setq val (catch 'match (mcall (car preds) z)))
		(cond ((null val) nil)
		      (t (setq saved (mul2* saved val))
			 (setq e (zl-delete z e 1)))))
      e)
     (cond ((and (equal saved 1)
		 (null (setq val (catch 'match (mcall (car preds) 1)))))
	    (matcherr)))
     (mset (car p) saved)
     b    (setq preds (cdr preds) p (cdr p))
     (go a)))

;;; TRANSLATE property in MAXSRC;TRANS5 >

(defmspec $apply1 (l) (setq l (cdr l))
	  (let ((*expr (meval (car l))))
	    (mapc #'(lambda (z) (setq *expr (apply1 *expr z 0))) (cdr l))
	    *expr))

(defmfun apply1 (expr *rule depth) 
  (cond
    ((> depth $maxapplydepth) expr)
    (t
     (prog nil 
	(*rulechk *rule)
	(setq expr (rule-apply *rule expr))
	b    (cond
	       ((or (atom expr) (mnump expr)) (return expr))
	       ((eq (caar expr) 'mrat)
		(setq expr (ratdisrep expr)) (go b))
	       (t
		(return
		  (simplifya
		   (cons
		    (delsimp (car expr))
		    (mapcar #'(lambda (z) (apply1 z *rule (f1+ depth)))
			    (cdr expr)))
		   t))))))))

(defmspec $applyb1 (l)  (setq l (cdr l))
	  (let ((*expr (meval (car l))))
	    (mapc #'(lambda (z) (setq *expr (car (apply1hack *expr z)))) (cdr l))
	    *expr))

(defmfun apply1hack (expr *rule) 
  (prog (pairs max) 
     (*rulechk *rule)
     (setq max 0)
     b    (cond
	    ((atom expr) (return (cons (or (mcall *rule expr) expr) 0)))
	    ((specrepp expr) (setq expr (specdisrep expr)) (go b)))
     (setq pairs (mapcar #'(lambda (z) (apply1hack z *rule))
			 (cdr expr)))
     (setq max 0)
     (mapc #'(lambda (l) (setq max (max max (cdr l)))) pairs)
     (setq expr (simplifya (cons (delsimp (car expr))
				 (mapcar #'car pairs))
			   t))
     (cond ((= max $maxapplyheight) (return (cons expr max))))
     (setq expr (rule-apply *rule expr))
     (return (cons expr (f1+ max)))))

(defun *rulechk (*rule)
  (if (and (symbolp *rule) (not (fboundp *rule)) (not (mfboundp *rule)))
      (merror "~:M not found" *rule)))

(defun rule-apply (*rule expr)
  (prog (ans)
   loop (setq ans (mcall *rule expr))
   (cond ((and ans (not (alike1 ans expr)))
	  (setq expr ans) (go loop)))
   (return expr)))

(defmspec $apply2 (l) (setq l (cdr l))
	  (let ((*rulelist (cdr l))) (apply2 (meval (car l)) 0)))

(defmfun apply2 (expr depth) 
  (cond
    ((> depth $maxapplydepth) expr)
    (t
     (prog (ans ruleptr) 
      a    (setq ruleptr *rulelist)
      b    (cond
	     ((null ruleptr)
	      (cond
		((atom expr) (return expr))
		((eq (caar expr) 'mrat)
		 (setq expr (ratdisrep expr)) (go b))
		(t
		 (return
		   (simplifya
		    (cons
		     (delsimp (car expr))
		     (mapcar #'(lambda (z) (apply2 z (f1+ depth)))
			     (cdr expr)))
		    t))))))
      (cond ((setq ans (mcall (car ruleptr) expr))
	     (setq expr ans)
	     (go a))
	    (t (setq ruleptr (cdr ruleptr)) (go b)))))))

(defmspec $applyb2 (l) (setq l (cdr l))
	  (let ((*rulelist (cdr l))) (car (apply2hack (meval (car l))))))

(defmfun apply2hack (e) 
  (prog (pairs max) 
     (setq max 0)
     (cond ((atom e) (return (cons (apply2 e -1) 0)))
	   ((specrepp e) (return (apply2hack (specdisrep e)))))
     (setq pairs (mapcar #'apply2hack (cdr e)))
     (setq max 0)
     (mapc #'(lambda (l) (setq max (max max (cdr l)))) pairs)
     (setq e (simplifya (cons (delsimp (car e)) (mapcar #'car pairs)) t))
     (cond ((= max $maxapplyheight) (return (cons e max)))
	   (t (return (cons (apply2 e -1) (f1+ max)))))))

