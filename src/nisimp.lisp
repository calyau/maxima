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

(macsyma-module nisimp)

;;;programs for the LET LETSIMP LETRULES and REMLET commands
;;;these programs use the names LETSIMPTREE and LETRULES on the 
;;;property list of atoms
;;;except for the top level programs all program names have the prefix NIS

(declare-top (special nistree nisrules nisflag)) 

(defmvar $letvarsimp nil)

(defmvar $letrat nil
  nil
  :properties ((evflag t))) 

(setq nisrules nil nistree nil) 

(defun let-rule-setter (var val)
  (cond ((eq var '$default_let_rule_package)
	 (merror (intl:gettext "assignment: cannot assign to default_let_rule_package.")))
	((and (eq var '$current_let_rule_package)
	      (not (memalike val (cdr $let_rule_packages))))
	 (merror (intl:gettext "assignment: ~M is not a rule package.") val))
	((eq var '$let_rule_packages)
	 (merror (intl:gettext "assignment: cannot assign to let_rule_packages.~%assignment: call 'let' to create let rule packages.")))))
	 
(defmspec $let (l) (setq l (cdr l))
	  (if (null (cdr l)) (wna-err '$let))
	  ;;LET([PATTERN,REPL,PRED,ARG1,...,ARGN],NAME)
	  (prog (pattern pat replacement treename text $ratfac) 
	     ;;LET(PATTERN,REPL,PRED,ARG1,...,ARGN)
	     (cond ((atom (car l))
		    (setq treename $current_let_rule_package))
		   ((eq 'mlist (caaar l))
		    (setq treename (cadr l))
		    (if (not (symbolp treename))
			(improper-arg-err treename '$let))
		    (setq l (cdar l)))
		   (t (setq treename $current_let_rule_package)))
	     (let ((nistree (mget treename 'letsimptree))
		   (nisrules (mget treename 'letrules)))
	       (setq pat (strip-lineinfo (meval (car l))))
	       (setq replacement (cdr l))
	       (setq pattern (cond ((atom pat) (list pat))
				   ((eq (caar pat) 'mtimes)
				    (cdr pat))
				   (t (list pat))))
	       (setq nistree (nislet nistree pattern replacement))
	       (cond (treename (mputprop treename
					 nistree
					 'letsimptree)
			       (add2lnc treename $let_rule_packages)))
	       (nonsymchk (caddr l) '$let)
	       (setq text
		     (append (list '(mtext) pat '| --> | )
			     (cond ((cddr l)
				    (list (cadr l)
					  '#.(intern (format nil " ~A " 'where))
					  (cons (list (caddr l))
						(cdddr l))))
				   (t replacement))))
	       (setq nisrules (append (list text) nisrules))
	       (cond (treename (mputprop treename
					 nisrules
					 'letrules)))
	       (return text))))

(defun nislet (tree list function) 
  (prog (permlist) 
     (setq permlist (nispermutations list))
     step (cond ((eq nil permlist) (return tree)))
     (setq tree (nistreebuilder tree (car permlist) function))
     (setq permlist (cdr permlist))
     (go step))) 

(defun nispermutations (llist) 
  (cond
    ((null (cdr llist)) (list llist))
    (t
     (prog (permlist a) 
      step (setq permlist
		 (append
		  (nisaddon (car llist)
			    (nispermutations (append a (cdr llist))))
		  permlist))
      (if (null (cdr llist)) (return permlist))
      (push (car llist) a)
      (setq llist (cdr llist))
      (go step))))) 

(defun nisaddon (x llist) 
  (if llist (cons (cons x (car llist)) (nisaddon x (cdr llist))))) 

(defun nistreebuilder (tree perm function) 
  (cond ((null perm) (cons (list function) tree))
	((null tree)
	 (list (cons (car perm)
		     (nistreebuilder nil (cdr perm) function))))
	((equal (car perm) (caar tree))
	 (nisswcar tree
		   (nisswcdr (car tree)
			     (nistreebuilder (cdar tree)
					     (cdr perm)
					     function))))
	(t (nisswcdr tree
		     (nistreebuilder (cdr tree)
				     perm
				     function))))) 

(defun nisswcar (x y)
  (cons y (cdr x))) 

(defun nisswcdr (x y)
  (cons (car x) y)) 

(defmspec $remlet (x)
  (setq x (cdr x))
  ;; REMLET(PROD,NAME) REMLET(PROD) REMLET() REMLET(FALSE,NAME)
  (prog (pattern text treename)
     (cond ((cddr x) (wna-err '$remlet))
	   ((null (cdr x)) (setq treename $current_let_rule_package))
	   (t (setq treename (cadr x))
	      (if (not (symbolp treename))
		  (improper-arg-err treename '$remlet))))
     (setq pattern (strip-lineinfo (meval (car x))))
     (when (or (not pattern) (eq '$all pattern))
       (setq nisrules nil nistree nil)
       (unless (eq treename '$default_let_rule_package)
	 (setq $let_rule_packages (delete treename $let_rule_packages :count 1 :test #'eq)))
       (go a))
     (setq nistree (mget treename 'letsimptree))
     (if (setq text (nisremlet pattern)) (return text))
     (if nistree
	 (setq nisrules
	       (nistreelister (mget treename 'letrules) pattern))
	 (setq nisrules nil))
     a    (mputprop treename nistree 'letsimptree)
     (mputprop treename nisrules 'letrules)
     (return '$done)))

(defun nistreelister (llist pattern) 
  (prog (x) 
   a    (if (alike1 pattern (cadar llist)) (return (append x (cdr llist))))
   (setq x (append x (list (car llist))) llist (cdr llist))
   (go a))) 

(defun nisremlet (pat) 
  (prog (llist permlist x) 
     (setq llist (if (mtimesp pat) (cdr pat) (ncons pat)))
     (setq nisflag t x nistree)
     (setq permlist (nispermutations llist))
     step (when (null permlist) (setq nistree x) (return nil))
     (setq x (nistreetrimmer (car permlist) x))
     (if (null nisflag) (merror (intl:gettext "remlet: no rule found: ~M") pat))
     (setq permlist (cdr permlist))
     (go step))) 

(defun nistreetrimmer (perm tree) 
  (cond ((null perm)
	 (cond ((null tree) (setq nisflag nil))
	       ((null (cdar tree))
		(setq nisflag (caar tree)) (cdr tree))
	       (t (nisswcdr tree (nistreetrimmer nil (cdr tree))))))
	((null tree) (setq nisflag nil))
	((equal (car perm) (caar tree))
	 (prog (x) 
	    (setq x (nistreetrimmer (cdr perm) (cdar tree)))
	    (if (null x) (return (cdr tree)))
	    (return (nisswcar tree (nisswcdr (car tree) x)))))
	(t (nisswcdr tree (nistreetrimmer perm (cdr tree)))))) 

(defmspec $letrules (name)
  (setq name (cdr name))		;LETRULES(NAME)
  (let ((treename (if name (car name) $current_let_rule_package)))
    (if (not (symbolp treename)) (improper-arg-err treename '$letrules))
    (setq nistree (mget treename 'letsimptree)
	  nisrules (mget treename 'letrules))
    (apply #'$disp nisrules)))

(defmspec $letsimp (form)		;letsimp(expr,tree1,...,treen)
  (setq form (cdr form))
  (let* ((expr (strip-lineinfo (meval (pop form))))
	 (sw ($ratp expr))
	 $ratfac)
    (progv (unless sw '(varlist genvar))
	(unless sw (list varlist genvar))
      (when (and sw (member 'trunc (cdar expr) :test #'eq))
	(setq expr ($taytorat expr)))
      (dolist (rulepackage (or form (list $current_let_rule_package))
	       (if sw (ratf expr) expr))
	(unless (symbolp rulepackage)
	  (improper-arg-err rulepackage '$letsimp))
	(when (setq nistree (mget rulepackage 'letsimptree))
	  ;; Whereas nisletsimp returns an expression in general
	  ;; representation, the original expr might be in CRE form.
	  ;; Regardless, we use ratf to make sure varlist and genvar
	  ;; know of expr's kernels.
	  (setq expr (nisletsimp (nformat (if (atom expr)
				     expr
				     (ratf expr))))))))))

(defun nisletsimp (e) 
  (let (x)
    (cond ((mnump e) e)
	  ((or (and (atom e) (setq x (ncons e)))
	       (and (eq (caar e) 'mtimes) (setq x (cdr e))))
	   (setq x (nisnewlist x))
	   (if x (nisletsimp ($ratexpand (cons '(mtimes) x))) e))
	  ((member (caar e) '(mplus mequal mlist $matrix mminus) :test #'eq)
	   (cons (remove 'simp (car e))
		 (mapcar #'nisletsimp (cdr e))))
	  ((or (eq (caar e) 'mrat) 
	       (and (eq (caar e) 'mquotient) (setq e (ratf e))))
	   (nisletsimprat e))
	  (t ;; A kernel (= product of 1 element)
	   (setq x (nisnewlist (ncons e)))
	   (if x (nisletsimp ($ratexpand (cons '(mtimes) x))) e)))))

(defun nisletsimprat (e)
  (let ((num (cadr e)) (denom (cddr e)) $ratexpand)
    (if $letvarsimp (setq varlist (mapcar #'nisletsimp varlist)))
    (let (($ratexpand t))
      ; Construct new CREs based on the numerator and denominator
      ; of E and disrep them in the VARLIST and GENVAR context from
      ; E.
      ;
      ; NISLETSIMP can change VARLIST and GENVAR, so the order of
      ; the PDIS and NISLETSIMP forms matter here.  PDISing and
      ; NISLETSIMPing the numerator before moving on to the
      ; denominator is not correct.
      (let ((varlist (mrat-varlist e))
            (genvar (mrat-genvar e)))
        (setq num (pdis num)
              denom (pdis denom)))
      (setq num (nisletsimp num)
            denom (nisletsimp denom)))
    (setq e (list '(mquotient) num denom))
    (if $letrat (nisletsimp ($ratexpand e)) e)))

(defun nisnewlist (llist)
  (let ((x (nissearch llist nistree nil))) (if x (nisreplace llist x))))

(defun nissearch (x y z) 
  (cond ((null y) nil)
	((nisinnernull y) (nisfix (nisinnernull y) z))
	((null x) nil)
	(t (prog (xx yy path bind) 
	      (setq yy y)
	      a    (setq xx x)
	      b    (cond ((and (setq bind (nismatch (car xx)
						    (caar yy)
						    z))
			       (setq path
				     (nissearch (cdr xx)
						(cdar yy)
						(cdr bind))))
			  (return (cons (car bind) path))))
	      (setq xx (cdr xx))
	      (cond (xx (go b)))
	      (setq yy (cdr yy))
	      (cond ((null yy) (return nil)))
	      (go a))))) 

(defun nisinnernull (x) 
  (cond ((null x) nil)
	((null (cdar x)) (caar x))
	(t (nisinnernull (cdr x))))) 

(defun nisfix (funperd argasslist) 
  (prog (function args bindings perd flag) 
     (if (not argasslist) (return (car funperd)))
     (setq argasslist (nisnumberpicker argasslist))
     (setq args (maplist 'caar argasslist))
     (setq bindings (maplist 'cdar argasslist))
     (mbinding (args bindings)
	       (setq function (car funperd))
	       (if (setq perd (cdr funperd))
		   (if (not (meval perd)) (setq flag t)))
	       (if (null flag) (setq function (meval function))))
     (return (if flag nil (list function)))))

(defun nisnumberpicker (x) 
  (cond ((null x) nil)
        ((or (not (symbolp (caar x)))
             (kindp (caar x) '$constant))
         ;; Skip over numbers and constants
         (nisnumberpicker (cdr x)))
        (t (nisswcdr x (nisnumberpicker (cdr x))))))

(defun nismatch (a b c) 
  (prog (x y newexpt) 
     (setq x (nisextract a))
     (setq y (nisextract b))
     (cond
       ((listp (cadr y))
	(cond ((and (listp (cadr x))
		    (equal (car x) (car y))
		    (setq c (cons (cons (car x) (car y)) c))
		    (setq c (nisargschecker (cadr x)
					    (cadr y)
					    c))
		    (setq newexpt (nisexpocheck (cddr x)
						(cddr y)
						c)))
	       (cond ((equal '(rat) (car newexpt))
		      (return (cons (cons a (nisbuild x newexpt))
				    c)))
		     (t (return (cons (cons a '(dummy 0 (0 0)))
				      newexpt)))))
	      (t (return nil)))))
     (cond ((and (setq c (nisargmatch (niskernel a) (car y) c))
		 (setq newexpt (nisexpocheck (cddr x)
					     (cddr y)
					     c)))
	    (cond ((equal '(rat) (car newexpt))
		   (return (cons (cons a (nisbuild x newexpt))
				 c)))
		  (t (return (cons (cons a '(dummy 0 (0 0)))
				   newexpt))))))
     (return nil))) 

(defun niskernel (a)
  (if (mexptp a) (cadr a) a))

(defun nisextract (x)
  (cond ((or (atom x) (eq (caar x) 'rat))
	 (cons x (cons t 1)))
	((eq 'mexpt (caar x))
	 (cond ((atom (cadr x))
		(cons (cadr x) (cons t (caddr x))))
	       (t (cons (if (member 'array (cdaadr x) :test #'eq)
			    (list (caaadr x) 'array)
			    (caaadr x))
			(cons (cdadr x) (caddr x))))))
	(t (cons (if (member 'array (cdar x) :test #'eq)
		     (list (caar x) 'array)
		     (caar x))
		 (cons (cdr x) 1))))) 

(defun nisargschecker (listargs treeargs argasslist) 
  (prog (c) 
     (cond ((and listargs treeargs) (go check))
	   ((or listargs treeargs) (return nil))
	   (t (return argasslist)))
     check(setq c (nisargmatch (car listargs)
			       (car treeargs)
			       argasslist))
     (cond (c (return (nisargschecker (cdr listargs)
				      (cdr treeargs)
				      c)))
	   (t (return nil))))) 

(defun nisexpocheck (listpower treepower argasslist) 
  (prog (p q r s a b xx) 
     (cond ((atom treepower)
	    (cond ((numberp treepower)
		   (prog2 (setq r treepower s 1) (go math)))
		  (t (return (nisargmatch listpower
					  treepower
					  argasslist))))))
     (setq r (cadr treepower) s (caddr treepower))
     (if (not (numberp s)) (return nil))
     math (cond ((numberp listpower) (setq p listpower q 1))
		((atom listpower) (return nil))
		((eq 'rat (caar listpower))
		 (setq p (cadr listpower) q (caddr listpower)))
		(t (return nil)))
     (setq xx (* (* q s) (- (* p s) (* q r))))
     (setq a (< (* r s) 0))
     (setq b (< xx 0))
     (cond ((or (not (or a b)) (and a (or b (equal 0 xx))))
	    (return (list '(rat) xx (* q s)))))
     (return nil))) 

(defun nisargmatch (x y c) 
  (prog (w) 
     (setq w c)
     up   (if (null w) (go down))
     (cond ((eq (caar w) y)
	    (cond ((alike1 (cdar w) x) (return c))
		  (t (return nil)))))
     (setq w (cdr w))
     (go up)
     down (setq w (mget y 'matchdeclare))
     (cond ((null w) (if (equal x y) (go out) (return nil)))
	   ((member (car w) '($true t) :test #'eq) (go out))
	   ((and (atom (car w))
		 (meval (cons (ncons (car w))
			      (append (cdr w) (list x)))))
	    (go out))
	   ((and (not (atom (car w)))
		 (not (atom (caar w)))
		 (atom (caaar w))
         ; If we arrive here, (CAR W) is a Maxima expression like ((FOO) ...)
         ; If (CAR W) is a Maxima lambda expression, evaluate it via MFUNCALL.
         ; Otherwise, append X and call MEVAL.
         ; Note that "otherwise" includes Maxima lambda expressions with missing arguments;
         ; in that case the expression is ((MQAPPLY) ((LAMBDA) ...)) and MEVAL is the way to go.
		 (if (eq (caaar w) 'lambda)
           (mfuncall (car w) x)
           (meval (append (car w) (list x)))))
	    (go out))
	   (t (return nil)))
     out  (return (cons (cons y x) c)))) 

(defun nisbuild (x newexpt) 
  (list '(mexpt)
	(if (listp (cadr x))
	    (cons (if (symbolp (car x)) (ncons (car x)) (car x))
		  (cadr x))
	    (car x))
	newexpt)) 

(defun nisreplace (llist asslist) 
  (cond ((eq (cdr asslist) nil) (cons (car asslist) llist))
	((equal (car llist) (caar asslist))
	 (cond ((equal 0 (cadar (cdddar asslist)))
		(nisreplace (cdr llist) (cdr asslist)))
	       (t (cons (cdar asslist)
			(nisreplace (cdr llist) (cdr asslist))))))
	(t (cons (car llist) (nisreplace (cdr llist) asslist))))) 
