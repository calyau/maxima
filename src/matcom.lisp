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
(macsyma-module matcom)

;; This is the Match Compiler.

(declare-top  (genprefix mc_)
	      (special *expr *rules *rulelist $rules alist $props 
		       *afterflag args boundlist *a* pt
		       reflist topreflist program $noundisp))

(setq *afterflag nil)

(defmspec $matchdeclare (form)
  (let ((meta-prop-p nil))
    (proc-$matchdeclare (cdr form))))

(defun proc-$matchdeclare (x)
  (if (oddp (length x))
      (merror "MATCHDECLARE takes an even number of arguments."))
  (do ((x x (cddr x))) ((null x))
    (cond ((symbolp (car x))
	   (cond ((and (not (symbolp (cadr x)))
		       (or (numberp (cadr x))
			   (memq (caaadr x) '(mand mor mnot mcond mprog))))
		  (improper-arg-err (cadr x) '$matchdeclare)))
	   (meta-add2lnc (car x) '$props)
	   (meta-mputprop (car x) (ncons (cadr x)) 'matchdeclare))
	  ((not ($listp (car x)))
	   (improper-arg-err (car x) '$matchdeclare))
	  (t (do ((l (cdar x) (cdr l))) ((null l))
	       (proc-$matchdeclare (list (car l) (cadr x)))))))
  '$done)

(defun compileatom (e p) 
  (prog (d) 
     (setq d (getdec p e))
     (return (cond ((null d)
		    (emit (list 'cond
				(list (list 'not
					    (list 'equal
						  e
						  (list 'quote p)))
				      '(matcherr)))))
		   ((memq p boundlist)
		    (emit (list 'cond
				(list (list 'not (list 'equal e p))
				      '((matcherr))))))
		   (t (setq boundlist (cons p boundlist)) (emit d))))))

(defun emit (x) (setq program (nconc program (list x))))

(defun memqargs (x)
  (cond ((or (numberp x) (memq x boundlist)) x)
	((and (symbolp x) (get x 'operators)) `(quote ,x))
	;; ((NULL BOUNDLIST) (LIST 'SIMPLIFYA (LIST 'QUOTE X) NIL))
	(t `(meval (quote ,x)))))

(defun makepreds (l gg) 
  (cond ((null l) nil)
	(t (cons (cond ((atom (car l))
			(list 'lambda (list (setq gg (gensym)))
			      `(declare (special ,gg))
			      (getdec (car l) gg)))
		       (t (defmatch1 (car l) (gensym))))
		 (makepreds (cdr l) nil)))))

(defun defmatch1 (pt e) 
  (prog (topreflist program) 
     (setq topreflist (list e))
     (cond ((atom (errset (compilematch e pt)))
	    (merror "Match processing aborted~%"))
	   (t (mtell
	       "~M Will be matched uniquely since sub-parts would otherwise be ambigious.~%" 
  
	       pt)
	      (return (list 'lambda
			    (list e)
			    `(declare (special ,e))
			    (list 'catch ''match
				  (nconc (list 'prog)
					 (list (cdr (reverse topreflist)))
					 program
					 (list (list 'return t))))))))))

(defun compileplus (e p) 
  (prog (reflist f g h flag leftover) 
   a    (setq p (cdr p))
   a1   (cond ((null p)
	       (cond ((null leftover)
		      (return (emit (list 'cond
					  (list (list 'not (list 'equal e 0.))
						'(matcherr))))))
		     ((null (cdr leftover)) (return (compilematch e (car leftover))))
		     ((setq f (intersect leftover boundlist))
		      (emit (list 'setq
				  e
				  (list 'meval
					(list 'quote
					      (list '(mplus)
						    e
						    (list '(mminus) (car f)))))))
		      (zl-delete (car f) leftover)
		      (go a1))
		     (t
		      (mtell "~M partitions SUM"
			     (cons '(mplus) leftover)
			     )
		      (setq boundlist (append boundlist (atomson leftover)))
		      (return (emit (list 'cond
					  (list (list 'part+
						      e
						      (list 'quote leftover)
						      (list 'quote
							    (makepreds leftover nil))))
					  '(t (matcherr))))))))
	      ((fixedmatchp (car p))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mplus)
					     e
					     (list '(mminus) (car p))))))))
	      ((atom (car p))
	       (cond ((cdr p) (setq leftover (cons (car p) leftover)) (setq p (cdr p)) (go a1))
		     (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (car p) boundlist))
	       (emit (getdec (car p) e))
	       (cond ((null (cdr p)) (return nil)) (t (go a))))
	      ((eq (caaar p) 'mtimes)
	       (cond ((and (not (or (numberp (cadar p))
				    (and (not (atom (cadar p)))
					 (eq (caar (cadar p)) 'rat))))
			   (fixedmatchp (cadar p)))
		      (setq flag nil)
		      (emit `(setq ,(genref)
			      (ratdisrep
			       (ratcoef ,e ,(memqargs (cadar p))))))
		      (compiletimes (car reflist) (cons '(mtimes) (cddar p)))
		      (emit `(setq ,e (meval
				       (quote
					(($ratsimp)
					 ((mplus) ,e
					  ((mtimes) -1 ,(car reflist)
					   ,(cadar p)))))))))
		     ((null flag)
		      (setq flag t) (rplacd (car p) (reverse (cdar p))) (go a1))
		     (t (setq leftover (cons (car p) leftover)) (go a))))
	      ((eq (caaar p) 'mexpt)
	       (cond ((fixedmatchp (cadar p))
		      (setq f 'findexpon)
		      (setq g (cadar p))
		      (setq h (caddar p)))
		     ((fixedmatchp (caddar p))
		      (setq f 'findbase)
		      (setq g (caddar p))
		      (setq h (cadar p)))
		     (t (go functionmatch)))
	       (emit (list 'setq
			   (genref)
			   (list f e (setq g (memqargs g)) ''mplus)))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mplus)
					     e
					     (list '(mminus)
						   (cond ((eq f 'findexpon)
							  (list '(mexpt)
								g
								(car reflist)))
							 (t (list '(mexpt)
								  (car reflist)
								  g)))))))))
	       (compilematch (car reflist) h))
	      ((not (fixedmatchp (caaar p)))
	       (cond ((cdr p)
		      (setq leftover (cons (car p) leftover))
		      (setq p (cdr p))
		      (go a1)))
	       (setq boundlist (cons (caaar p) boundlist))
	       (emit (list 'msetq
			   (caaar p)
			   (list 'kar (list 'kar (genref)))))
	       (go functionmatch))
	      (t (go functionmatch)))
   (go a)
   functionmatch
   (emit (list 'setq
	       (genref)
	       (list 'findfun e (memqargs (caaar p)) ''mplus)))
   (cond ((eq (caaar p) 'mplus)
	  (mtell "~M~%Warning: + within +~%" (car p))
	  (compileplus (car reflist) (car p)))
	 (t (emit (list 'setq (genref) (list 'kdr (cadr reflist))))
	    (compileeach (car reflist) (cdar p))))
   (emit (list 'setq
	       e
	       (list 'meval
		     (list 'quote
			   (list '(mplus) e (list '(mminus) (car p)))))))
   (go a)))

(defun compiletimes (e p) 
  (prog (reflist f g h leftover) 
   a    (setq p (cdr p))
   a1   (cond ((null p)
	       (cond ((null leftover)
		      (return (emit (list 'cond
					  (list (list 'not (list 'equal e 1.))
						'(matcherr))))))
		     ((null (cdr leftover)) (return (compilematch e (car leftover))))
		     ((setq f (intersect leftover boundlist))
		      (emit (list 'setq
				  e
				  (list 'meval
					(list 'quote
					      (list '(mquotient) e (car f))))))
		      (zl-delete (car f) leftover)
		      (go a1))
		     (t
		      (mtell "~M partitions PRODUCT"
			     (cons '(mtimes) leftover)
			     )
		      (setq boundlist (append boundlist (atomson leftover)))
		      (return (emit (list 'cond
					  (list (list 'part*
						      e
						      (list 'quote leftover)
						      (list 'quote
							    (makepreds leftover nil))))
					  '(t (matcherr))))))))
	      ((fixedmatchp (car p))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote (list '(mquotient) e (car p)))))))
	      ((atom (car p))
	       (cond ((cdr p) (setq leftover (cons (car p) leftover)) (setq p (cdr p)) (go a1))
		     (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (car p) boundlist))
	       (emit (getdec (car p) e))
	       (cond ((null (cdr p)) (return nil)) (t (go a))))
	      ((eq (caaar p) 'mexpt)
	       (cond ((fixedmatchp (cadar p))
		      (setq f 'findexpon)
		      (setq g (cadar p))
		      (setq h (caddar p)))
		     ((fixedmatchp (caddar p))
		      (setq f 'findbase)
		      (setq g (caddar p))
		      (setq h (cadar p)))
		     (t (go functionmatch)))
	       (emit (list 'setq
			   (genref)
			   (list f e (setq g (memqargs g)) ''mtimes)))
	       (cond ((eq f 'findbase)
		      (emit (list 'cond
				  (list (list 'equal (car reflist) 0)
					'(matcherr))))))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mquotient)
					     e
					     (cond ((eq f 'findexpon)
						    (list '(mexpt) g (car reflist)))
						   (t (list '(mexpt)
							    (car reflist)
							    g))))))))
	       (compilematch (car reflist) h))
	      ((not (fixedmatchp (caaar p)))
	       (cond ((cdr p)
		      (setq leftover (cons (car p) leftover))
		      (setq p (cdr p))
		      (go a1)))
	       (setq boundlist (cons (caaar p) boundlist))
	       (emit (list 'msetq
			   (caaar p)
			   (list 'kar (list 'kar (genref)))))
	       (go functionmatch))
	      (t (go functionmatch)))
   (go a)
   functionmatch
   (emit (list 'setq
	       (genref)
	       (list 'findfun e (memqargs (caaar p)) ''mtimes)))
   (cond ((eq (caaar p) 'mtimes)
	  (mtell "~M~%Warning: * within *" (car p))
	  (compiletimes (car reflist) (car p)))
	 (t (emit (list 'setq (genref) (list 'kdr (cadr reflist))))
	    (compileeach (car reflist) (cdar p))))
   (emit (list 'setq
	       e
	       (list 'meval
		     (list 'quote (list '(mquotient) e (car p))))))
   (go a)))


(defmspec $defmatch (form)
  (let ((meta-prop-p nil))
    (proc-$defmatch (cdr form))))

(defun proc-$defmatch (l) 
  (prog (pt pt* args *a* boundlist reflist topreflist program name tem) 
     (setq name (car l))
     (setq pt (copy (setq pt* (simplify (cadr l)))))
     (cond ((atom pt)
	    (setq pt (copy (setq pt* (meval pt))))
	    (mtell "~M~%Is the pattern~%" pt)
	    ))
     (setq args (cddr l))
     (cond ((null (allatoms args)) (mtell "Non-atomic pattern variables")
	    (return nil)))
     (setq boundlist args)
     (setq *a* (genref))
     (cond ((atom (errset (compilematch *a* pt)))
	    (merror "Match processing aborted~%"))
	   (t (meta-fset name
			 (list 'lambda
			       (cons *a* args)
			       `(declare (special ,*a* ,@ args))
			       (list 'catch ''match
				     (nconc (list 'prog)
					    (list (setq tem  (cdr (reverse topreflist))))
					    `((declare (special ,@ tem)))
					    program
					    (list (list 'return
							(cond (boundlist (cons 'retlist
									       boundlist))
							      (t t))))))))
	      (meta-add2lnc name '$rules) 
	      (meta-mputprop name (list '(mlist) pt* (cons '(mlist) args)) '$rule)
	      (return name)))))


(defun atomson (l) 
  (cond ((null l) nil)
	((atom (car l)) (cons (car l) (atomson (cdr l))))
	(t (atomson (cdr l)))))


(defmspec $tellsimp (form)
  (let ((meta-prop-p nil))
    (proc-$tellsimp (cdr form))))

(defun $clear_rules ()
  (mapc 'kill1 (cdr $rules))
  (sloop for v in '(mexpt mplus mtimes)
	 do (setf (mget v 'rulenum) nil)))

(defun proc-$tellsimp (l) 
  (prog (pt rhs boundlist reflist topreflist *a* program name tem
	 oldstuff pgname oname rulenum) 
     (setq pt (copy (simplifya (car l) nil)))
     (setq name pt) 
     (setq rhs (copy (simplifya (cadr l) nil)))
     (cond ((alike1 pt rhs) (merror "Circular rule attempted - TELLSIMP"))
	   ((or (atom pt) (mget (setq name (caar pt)) 'matchdeclare))
	    (merror "~%~A unsuitable~%" (fullstrip1 (getop name))))
	   ((memq name '(mplus mtimes))
	    (mtell "Warning: Putting rules on '+' or '*' is inefficient, and may not work.~%")))
     (setq *a* (genref))
     (cond ((atom (errset (compileeach *a* (cdr pt))))
	    (merror "Match processing aborted~%")))
     (setq oldstuff (get name 'operators))
     (setq rulenum (mget name 'rulenum))
     (cond ((null rulenum) (setq rulenum 1.)))
     (setq oname (getop name))
     (setq pgname (implode (append (%to$ (explodec oname))
				   '(|r| |u| |l| |e|)
				   (mexploden rulenum))))
     (meta-mputprop pgname name 'ruleof)
     (meta-add2lnc pgname '$rules)
     (meta-mputprop name (f1+ rulenum) 'rulenum)
     (meta-fset pgname
		(list 'lambda '(x a2 a3)
		      `(declare (special x a2 a3))
		      (list 'prog
			    (list 'ans *a*)
			    `(declare (special ans ,*a*))
			    (list 'setq
				  'x
				  (list 'cons
					'(car x)
					(list 'setq
					      *a*
					      '(cond (a3 (cdr x)) 
						(t (mapcar #'(lambda (h) (simplifya h a3))
						    (cdr x)))))))
			    (list
			     'setq
			     'ans
			     (list 'catch ''match
				   (nconc (list 'prog)
					  (list (setq tem (nconc boundlist
								 (cdr (reverse topreflist)))))
					  #+cl
					  `((declare (special ,@ tem)))
					  program
					  (list (list 'return
						      (memqargs rhs))))))
			    (cond ((not (memq name '(mtimes mplus)))
				   (list 'return
					 (list 'cond
					       '(ans) '((and (not dosimp) (memq 'simp (cdar x)))x)
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 t)))
							   (t '(eqtest x x)))))))
				  ((eq name 'mtimes)
				   (list 'return
					 (list 'cond
					       '((and (equal 1. a2) ans))
					       '(ans (meval '((mexpt) ans a2)))
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 a3)))
							   (t '(eqtest x x)))))))
				  ((eq name 'mplus)
				   (list 'return
					 (list 'cond
					       '((and (equal 1. a2) ans))
					       '(ans (meval '((mtimes) ans a2)))
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 a3)))
							   (t '(eqtest x x)))))))))))
     (meta-mputprop pgname (list '(mequal) pt rhs) '$rule)
     (cond ((null (mget name 'oldrules))
	    (meta-mputprop name
			   (list (get name 'operators))
			   'oldrules)))
     (meta-putprop name pgname 'operators)
     (return (cons '(mlist)
		   (meta-mputprop name
				  (cons pgname (mget name 'oldrules))
				  'oldrules)))))

(defun %to$ (l) (cond ((eq (car l) '%) (rplaca l '$)) (l)))


(defmspec $tellsimpafter (form)
  (let ((meta-prop-p nil))
    (proc-$tellsimpafter (cdr form))))

(defun proc-$tellsimpafter (l) 
  (prog (pt rhs boundlist reflist topreflist *a* program name oldstuff plustimes pgname oname tem
	 rulenum) 
     (setq pt (copy (simplifya (car l) nil)))
     (setq name pt)
     (setq rhs (copy (simplifya (cadr l) nil)))
     (cond ((alike1 pt rhs) (merror "Circular rule attempted - TELLSIMPAFTER"))
	   ((or (atom pt) (mget (setq name (caar pt)) 'matchdeclare))
	    (merror "~%~A unsuitable~%" (fullstrip1 (getop name)))))
     (setq *a* (genref))
     (setq plustimes (memq name '(mplus mtimes)))
     (if (atom (if plustimes (errset (compilematch *a* pt))
		   (errset (compileeach *a* (cdr pt)))))
	 (merror "Match processing aborted~%"))
     (setq oldstuff (get name 'operators))
     (setq rulenum (mget name 'rulenum))
     (if (null rulenum) (setq rulenum 1))
     (setq oname (getop name))
     (setq pgname (implode (append (%to$ (explodec oname))
				   '(r u l e) (mexploden rulenum))))
     (meta-mputprop pgname name 'ruleof)
     (meta-add2lnc pgname '$rules)
     (meta-mputprop name (f1+ rulenum) 'rulenum)
     (meta-fset
      pgname
      (list
       'lambda
       '(x ans a3)
       (if oldstuff (list 'setq 'x (list oldstuff 'x 'ans 'a3)))
       (list
	'cond
	'(*afterflag x)
	(list 't
	      (nconc (list 'prog)
		     (list (cons *a* '(*afterflag)))
		     `((declare (special ,*a* *afterflag)))
		     (list '(setq *afterflag t))
		     (cond (oldstuff (subst (list 'quote name)
					    'name
					    '((cond ((or (atom x) (not (eq (caar x) name)))
						     (return x)))))))
		     (list (list 'setq
				 *a*
				 (cond (plustimes 'x) (t '(cdr x)))))
		     (list (list 'setq
				 'ans
				 (list 'catch ''match
				       (nconc (list 'prog)
					      (list (setq tem(nconc boundlist
								    (cdr (reverse topreflist)))))
					      #+cl
					      `((declare (special ,@ tem)))
					      program
					      (list (list 'return
							  (memqargs rhs)))))))
		     (list '(return (or ans (eqtest x x)))))))))
     (meta-mputprop pgname (list '(mequal) pt rhs) '$rule)
     (cond ((null (mget name 'oldrules))
	    (meta-mputprop name (list (get name 'operators)) 'oldrules)))
     (meta-putprop name pgname 'operators)
     (return (cons '(mlist)
		   (meta-mputprop name
				  (cons pgname (mget name 'oldrules))
				  'oldrules)))))

(defmspec $defrule (form)
  (let ((meta-prop-p nil))
    (proc-$defrule (cdr form))))

;;(defvar *match-specials* nil);;Hell lets declare them all special, its safer--wfs
(defun proc-$defrule (l) 
  (prog (pt rhs boundlist reflist topreflist name *a* program lhs* rhs*   tem) 
     (if (not (= (length l) 3)) (wna-err '$defrule))
     (setq name (car l))
     (if (or (not (symbolp name)) (mopp name) (memq name '($all $%)))
	 (merror "Improper rule name:~%~M" name))
     (setq pt (copy (setq lhs* (simplify (cadr l)))))
     (setq rhs (copy (setq rhs* (simplify (caddr l)))))
     (setq *a* (genref))
     (cond ((atom (errset (compilematch *a* pt)))
	    (merror "Match processing aborted~%"))
	   (t (meta-fset name
			 (list 'lambda
			       (list *a*)
			       `(declare (special ,*a*))
			       (list 'catch ''match
				     (nconc (list 'prog)
					    (list (setq tem (nconc boundlist
								   (cdr (reverse topreflist)))))
					    #+cl
					    `((declare (special ,@ tem)))
					    program
					    (list (list 'return
							(memqargs rhs)))))))
	      (meta-add2lnc name '$rules)
	      (meta-mputprop name (setq l (list '(mequal) lhs* rhs*)) '$rule)
	      (meta-mputprop name '$defrule '$ruletype)
	      (return (list '(msetq) name (cons '(marrow) (cdr l))))))))

(defun getdec (p e) 
  (let (x z) 
    (cond ((setq x (mget p 'matchdeclare))
	   (cond ((not (atom (car x))) (setq x (car x))))
	   (setq z (nconc (mapcar 'memqargs (cdr x)) (ncons e)))
	   (setq x (car x))
	   (cond ((not (atom x)) (setq x (car x))))
	   (setq z
		 (cond ((or (memq x '($true t $all))
			    (and (fboundp x) (not (get x 'translated))))
			(cons x z))
		       (t	   ;(push (second z) *match-specials*)
			(list 'is (list 'quote (cons (ncons x) z))))))
	   (cond ((memq (car z) '($true t $all)) (list 'msetq p e))
		 (t (list 'cond
			  (list z (list 'msetq p e))
			  '((matcherr)))))))))

(defun compilematch (e p) 
  (prog (reflist) 
     (cond ((fixedmatchp p)
	    (emit (list 'cond
			(list (list 'not
				    (list 'alike1
					  e
					  (list 'meval (list 'quote
							     p))))
			      '(matcherr)))))
	   ((atom p) (compileatom e p))
	   ((eq (caar p) 'mplus) (compileplus e p))
	   ((eq (caar p) 'mtimes) (compiletimes e p))
	   ((and (eq (caar p) 'mexpt)
		 (fixedmatchp (cadr p)))
	    (emit (list 'setq
			(genref)
			(list 'findexpon
			      e
			      (memqargs (cadr p))
			      ''mexpt)))
	    (compilematch (car reflist) (caddr p)))
	   ((and (eq (caar p) 'mexpt)
		 (fixedmatchp (cadr p)))
	    (emit (list 'setq
			(genref)
			(list 'findbase
			      e
			      (memqargs (caddr p))
			      ''mexpt)))
	    (compilematch (car reflist) (cadr p)))
	   ((eq (caar p) 'mexpt)
	    (emit (list 'setq
			(genref)
			(list 'findbe e)))
	    (emit (list 'setq
			(genref)
			(list 'kar (cadr reflist))))
	    (compilematch (car reflist) (cadr p))
	    (emit (list 'setq
			(cadr reflist)
			(list 'kdr (cadr reflist))))
	    (compilematch (cadr reflist) (caddr p)))
	   (t (compileatom (list 'kar
				 (list 'kar e))
			   (caar p))
	      (emit (list 'setq
			  (genref)
			  (list 'kdr e)))
	      (compileeach (car reflist) (cdr p))))
     (return program)))

(defun genref nil 
  (prog (a) 
     (setq a (tr-gensym))
     (setq topreflist (cons a topreflist))
     (return (car (setq reflist (cons a reflist))))))
(defun compileeach (elist plist) 
    (prog (reflist count) 
       (setq count 0)
       (setq reflist (cons elist reflist))
       a    (setq count (f1+ count))
       (cond ((null plist)
	      (return (emit (list 'cond
				  (list (list 'nthkdr elist (f1- count))
					'(matcherr)))))))
       (emit (list 'setq (genref) (list 'kar (cadr reflist))))
       (compilematch (car reflist) (car plist))
       (setq plist (cdr plist))
       (setq reflist (cons (list 'kdr (cadr reflist)) reflist))
       (go a)))

(defun fixedmatchp (x)
  (cond ((numberp x) t)
	((atom x)
	 (if (or (memq x boundlist) (null (mget x 'matchdeclare))) t))
	(t (and (or (memq (caar x) boundlist)
		    (null (mget (caar x) 'matchdeclare)))
		(fmp1 (cdr x))))))

(defun fmp1 (x) (if (null x) t (and (fixedmatchp (car x)) (fmp1 (cdr x)))))

