;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module mlisp)

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (defvar *old-read-base* *read-base*)
    (setq *read-base* 10.))

(declare-top
 (special mspeclist mproplist bindlist loclist nounsflag
	  derivflag derivlist mprogp mdop evp aexprp mlocp $labels
	  $values $functions $arrays $rules $gradefs $dependencies $aliases
	  $myoptions $props genvar $maxposex $maxnegex $expop $expon
	  $float $numer aryp msump state-pdl evarrp $setval nounl
	  $setcheckbreak $refcheck *mdebug* refchkl baktrcl maplp
	  $norepeat $detout $doallmxops $doscmxops opers factlist opexprp
	  $translate $transrun $maperror outargs1 outargs2 fmaplvl mopl
	  $powerdisp $subscrmap $dispflag $optionset dsksetp fexprerrp
	  $features *alphabet* $%enumer $infeval $savedef $%% %e-val
	  $mapprint featurel outfiles fundefsimp mfexprp transp
	  $macros linel $ratfac $ratwtlvl
	  $operators noevalargs $piece $partswitch *gcdl*
	  scanmapp *builtin-$props* $infolists))

(declare-top (unspecial args))

(setq mspeclist nil bindlist nil loclist nil mproplist nil $%enumer nil
      $float nil nounl nil $refcheck nil scanmapp nil maplp nil
      mprogp nil evp nil mdop nil mlocp nil
      $subscrmap nil $translate nil $transrun t $savedef t aexprp nil
      $maperror t fmaplvl 0 $optionset nil
      $setcheckbreak nil dsksetp nil aryp nil msump nil evarrp nil
      $infeval nil factlist nil $mapprint t fundefsimp nil
      mfexprp t nounsflag nil opexprp nil
      transp nil noevalargs nil
      $piece '$piece $setval '$setval fexprerrp nil rulefcnl nil
      featurel  '($integer $noninteger $even $odd $rational $irrational $real $imaginary
		  $complex $analytic $increasing $decreasing $oddfun $evenfun $posfun
		  $commutative $lassociative $rassociative $symmetric $antisymmetric)
      $features (cons '(mlist simp) (append featurel nil)))

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(defvar munbound '|#####|)

(defvar fixunbound most-negative-fixnum)

(defvar flounbound most-negative-double-float)

(defmvar munbindp nil
  "Used for safely `munbind'ing incorrectly-bound variables."
  no-reset)

(defmvar $setcheck nil)

(mapc #'(lambda (x) (setf (symbol-value x) (ncons '(mlist simp))))
      '($values $functions $macros $arrays $myoptions $rules $props))

(defmfun mapply1 (fn args fnname form)
  (declare (special aryp))
  (cond ((atom fn)
	 (cond ((atom fn)
		(cond ((functionp fn)
		       (apply fn args))
		      ;;better be a macro or an array.
		      ((fboundp fn)
		       (if (macro-function fn)
			   (progn
			     (merror
			      "~M is a lisp level macro and cannot be applied at maxima level" fn)
			     (eval (cons fn args)))
			   (mapply1 (symbol-function fn) args fn form)))
		      ((symbol-array fn)
		       (mapply1 (symbol-array fn) args fn form))
		      (t
		       (setq fn (getopr fn))
		       (badfunchk fnname fn nil)
		       (let ((noevalargs t))
			 (meval (cons (ncons fn) args))))))))
	((functionp fn)
	 (apply fn args))
	((eq (car fn) 'lambda)
	 (apply (coerce fn 'function) args))
	((eq (caar fn) 'lambda) (mlambda fn args fnname t form))
	((eq (caar fn) 'mquote) (cons (cdr fn) args))
	((and aryp (member (caar fn) '(mlist $matrix) :test #'eq))
	 (if (not (or (= (length args) 1)
		      (and (eq (caar fn) '$matrix) (= (length args) 2))))
	     (merror "Wrong number of indices:~%~M" (cons '(mlist) args)))
	 (if (member 0 args :test #'eq)
	     (merror "No such ~M element: ~M~%" (if (eq (caar fn) 'mlist) "list" "matrix")
		     `((mlist) ,@args)))
	 (do ((args1 args (cdr args1)))
	     ((null args1) (let (($piece $piece) ($partswitch 'mapply))
			     (apply #'$inpart (cons fn args))))
	   (unless (fixnump (car args1))
	     (if evarrp (throw 'evarrp 'notexist))
	     (merror "Subscript must be an integer:~%~M" (car args1)))))
	(aryp
	 (cons '(mqapply array) (cons fn args)))
	((member 'array (cdar fn) :test #'eq)
	 (cons '(mqapply) (cons fn args)))
	(t
	 (badfunchk fnname fn t))))

;; the last argument to mapply1 for the lineinfo is not correct here..
(defmfun mcall n
  (mapply1 (arg 1) (listify (- 1 n)) (arg 1) nil))

(defun mevalargs (args)
  (cond (noevalargs (setq noevalargs nil) args) (t (mapcar #'meval args))))

;;Function Call stack each element is
;; (fname . bindlist) where bindlist was the value at time of entry.
;; So you can use this to compute what the bindings were at any
;; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

;;; The frame info for a function call consists of 5 consecutive
;;; entries in *MLAMBDA-CALL-STACK*.  I call the topmost object of
;;; such a quintuple the `function designator' belonging to this
;;; frame.

(defun pop-mlambda-call-stack (&optional fnname)
  "Deactivate the topmost function call frame info.
Return the function designator for this frame and check that it
is EQ to FNNAME if the latter is non-NIL."
  (let ((ar *mlambda-call-stack*) mlambda)
    (symbol-macrolet ((mlambda-pointer (fill-pointer ar)))
      (prog1
	  (setq mlambda (aref ar (1- mlambda-pointer)))
	(when fnname
	  ;; Different frames can have the same function designator,
	  ;; so this doesn't prove anything, it's just a check.
	  (assert (eq mlambda fnname)
		  (*mlambda-call-stack*)
		  "Expected ~a but got ~a on mlambda call stack."
		  fnname mlambda))
	(decf mlambda-pointer 5)))))

(defun mlambda (fn args fnname noeval form)
  (cond ((not ($listp (cadr fn)))
	 (merror "First argument to `lambda' must be a list:~%~M" (cadr fn))))
  (setq noevalargs nil)
  (let ((params  (cdadr fn))( mlocp  t))
    (setq loclist (cons nil loclist))
    (do ((a) (p))
	((or (null params) (and (null args) (not (mdeflistp params))))
	 (setq args (nreconc a args) params (nreconc p params)))
      (cond ((mdeflistp params)
	     (setq params (cdar params) args (ncons (cons '(mlist) args)))))
      (cond ((and mfexprp (mquotep (car params)))
	     (setq a (cons (car args) a) p (cons (cadar params) p)))
	    ((atom (car params))
	     (setq p (cons (car params) p)
		   a (cons (cond (noeval (car args))
				 (t (meval (car args)))) a)))
	    (t (merror "Illegal `lambda' parameter:~%~M" (car params))))
      (setq args (cdr args) params (cdr params)))
    (let (finish2033 (finish2032 params) (ar *mlambda-call-stack*))
      (declare (type (vector t) ar))
      (unwind-protect
	   (progn
	     (or (> (array-total-size ar) (+ (fill-pointer ar) 10))
		 (adjust-array ar (+ (array-total-size ar) 50)
			       :fill-pointer (fill-pointer ar)))
	     (vector-push bindlist ar)
	     ;; rather than pushing all on baktrcl it might be good
	     ;; to make a *last-form* global that is set in meval1
	     ;; and is pushed here.
	     (vector-push form ar)
	     (vector-push params ar)
	     (vector-push args ar)
	     (vector-push fnname ar)
	     (mbind finish2032 args fnname)
	     (setq finish2033 t)
	     (prog1
		 (let ((aexprp (and aexprp (not (atom (caddr fn)))
				    (eq (caar (caddr fn)) 'lambda))))
		   (cond ((null (cddr fn)) (merror "No `lambda' body present"))
			 ((cdddr fn) (mevaln (cddr fn)))
			 (t (meval (caddr fn)))))
	       nil))
	(if finish2033
	    (progn
	      (incf (fill-pointer *mlambda-call-stack*) -5)
	      (munlocal)
	      (munbind finish2032)))))))


(defmspec mprogn (form)
  (mevaln (cdr form)))

(defmfun mevaln (l) ;; called in a few places externally.
  (do ((body l (cdr body)) ($%% '$%%)) ((null (cdr body)) (meval (car body)))
    (setq $%% (meval (car body)))))

(defun mqapply1 (form)
  (declare (special aryp))
  (destructuring-let (((fn . argl) (cdr form)) (aexprp))
    (cond ((not (mquotep fn)) (setq fn (meval fn))))
    (cond ((atom fn) (meval (cons (cons fn aryp) argl)))
	  ((eq (caar fn) 'lambda)
	   (cond (aryp (merror "Improper array call"))
		 (t (mlambda fn argl (cadr form) noevalargs form))))
	  (t (mapply1 fn (mevalargs argl) (cadr form) form)))))

(defmfun meval (form)
  (simplifya (meval1 form) nil))

;;temporary hack to see what's going on:
(defmfun safe-mgetl (atom inds)
  (and (symbolp atom)
       (let ((props (get atom 'mprops)))
	 (and props (getl props inds)))))

(defmfun safe-mget (atom inds)
  (and (symbolp atom)
       (let ((props (get atom 'mprops)))
	 (and props (getf (cdr props) inds)))))

(defvar *last-meval1-form* nil)

(defmfun meval1 (form)
  (declare (special nounl *break-points* *break-step*))
  (cond ((atom form)
	 (prog (val)
	    (cond ((not (symbolp form)) (return form))
		  ((and $numer (setq val (safe-mget form '$numer))
			(or (not (eq form '$%e)) $%enumer))
		   (return (meval1 val)))
		  ((not (boundp form))
		   (if (safe-get form 'bindtest)
		       (merror "~:M unbound variable" form)
		       (return form)))
		  ((mfilep (setq val (symbol-value form)))
		   (setq val
			 (eval (dskget (cadr val) (caddr val) 'value nil)))))
	    (when (and $refcheck (member form (cdr $values) :test #'eq)
		       (not (member form refchkl :test #'eq)))
	      (setq refchkl (cons form refchkl))
	      (mtell "~:M has value.~%" form))
	    (return val)))
	((or (and (atom (car form))
		  (setq form (cons (ncons (car form)) (cdr form))))
	     (atom (caar form)))
	 (let ((baktrcl baktrcl) transp)
	   (prog (u aryp)
	      (declare (special aryp))
	      (setq *last-meval1-form* form)
	      (setq aryp (member 'array (cdar form) :test #'eq))
	      (cond ((and (not opexprp) (not aryp)
			  (member (caar form) '(mplus mtimes mexpt mnctimes) :test #'eq))
		     (go c))
		    ;; dont bother pushing mplus and friends on baktrcl
		    ;; should maybe even go below aryp.
		    ((and *mdebug*
			  (progn
			    ;; if wanting to step, the *break-points*
			    ;; variable will be set to a vector (possibly empty).
			    (when (and *break-points*
				       (or (null  *break-step*)
					   (null (funcall *break-step* form))))
			      (let ((ar *break-points*))
				(declare (type (vector t) ar))
				(loop for i below (fill-pointer ar)
				   when (eq (car (aref ar i)) form)
				   do (*break-points* form)
				   (loop-finish))))
			    nil)))
		    ((and $subscrmap aryp
			  (do ((x (margs form) (cdr x)))
			      ((or (null x) (mxorlistp (car x))) x)))
		     (setq noevalargs nil) (return (subgen form)))
		    ((eq (caar form) 'mqapply) (return (mqapply1 form))))
	      (badfunchk (caar form) (caar form) nil)
	      a    (setq u (or (safe-getl (caar form) '(noun))
			       (and nounsflag (eq (getchar (caar form) 1) '%)
				    (not (or (getl-fun (caar form) '(subr fsubr lsubr))
					     (safe-getl (caar form) '(mfexpr* mfexpr*s))))
				    (prog2 ($verbify (caar form))
					(safe-getl (caar form) '(noun))))
			       (and (not aryp) $transrun
				    (setq transp
					  (or (safe-mgetl (caar form) '(t-mfexpr))
					      (safe-getl (caar form) '(translated-mmacro)))))
			       (and (not aryp)
				    (setq u
					  (or (safe-mget (caar form) 'trace)
					      (and $transrun
						   (safe-get (caar form) 'translated)
						   (not (safe-mget (caar form) 'local-fun))
						   (setq transp t) (caar form))))
				    (getl-fun u '(expr subr lsubr)))
			       (cond (aryp (safe-mgetl (caar form) '(hashar array)))
				     ((safe-mgetl (caar form) '(mexpr mmacro)))
				     ((safe-mgetl (caar form) '(t-mfexpr)))
				     (t (or (safe-getl (caar form)
						       '(mfexpr* mfexpr*s))
					    (getl-fun (caar form)
						      '(subr fsubr expr fexpr macro lsubr)))))))
	      (cond ((null u) (go b))
		    ((and (member (car u) '(mexpr mmacro) :test #'eq) (mfilep (cadr u)))
		     (setq u (list (car u)
				   (dskget (cadadr u) (car (cddadr u))
					   (car u) nil))))
		    ((and (member (car u) '(array hashar) :test #'eq) (mfilep (cadr u)))
		     (i-$unstore (ncons (caar form)))
		     (return (meval1 form))))
	      (return
		(cond ((eq (car u) 'hashar)
		       (harrfind (cons (car form) (mevalargs (cdr form)))))
		      ((member (car u) '(fexpr fsubr) :test #'eq)
		       (if fexprerrp
			   (merror "Attempt to call ~A ~A from Maxima level.~
				 ~%Send a bug note."
				   (car u) (caar form)))
		       (setq noevalargs nil) (apply (caar form) (cdr form)))
		      ((or (eq (car u) 'subr)
			   (eq (car u) 'lsubr))
		       (apply (caar form) (mevalargs (cdr form))))
		      ((eq (car u) 'noun)
		       (cond ((or (member (caar form) nounl :test #'eq) nounsflag)
			      (setq form (cons (cons (cadr u) (cdar form))
					       (cdr form)))
			      (go a))
			     (aryp (go b))
			     ((member (caar form) '(%sum %product) :test #'eq)
			      (setq u (do%sum (cdr form) (caar form))
				    noevalargs nil)
			      (cons (ncons (caar form)) u))
			     (t (meval2 (mevalargs (cdr form)) form))))
		      ((eq (car u) 'array)
		       (arrfind (cons (car form) (mevalargs (cdr form)))))
		      ((eq (car u) 'mexpr)
		       (mlambda (cadr u) (cdr form) (caar form) noevalargs form))
		      ((member (car u) '(mmacro translated-mmacro) :test #'eq)
		       (setq noevalargs nil)
		       (meval (mmacro-apply (cadr u) form)))
		      ((eq (car u) 'mfexpr*)
		       (setq noevalargs nil)  (apply (cadr u) (ncons form)))
		      ((eq (car u) 'macro)
		       (setq noevalargs nil)
		       (setq form (cons(caar form) (cdr form)))
		       (eval form))
		      ((eq (car u) 't-mfexpr)
		       (apply (cadr u) (cdr form)))
		      (t
		       (apply (cadr u) (mevalargs (cdr form))))))
	      b   (if (and (not aryp) (load-function (caar form) t)) (go a))
	      (badfunchk (caar form) (caar form) nil)
	      (if (symbolp (caar form))
		  (setq u (boundp (caar form)))
		  (return (meval1-extend form)))
	      c   (cond ((or (null u)
			     (and (safe-get (caar form) 'operators) (not aryp))
			     (eq (caar form) (setq u (symbol-value (caar form)))))
			 (setq form (meval2 (mevalargs (cdr form)) form))
			 (return (or (and (safe-mget (caar form) 'atvalues)
					  (at1 form)) form)))
			((and aryp (safe-get (caar form) 'nonarray))
			 (return (cons (cons (caar form) aryp)
				       (mevalargs (cdr form)))))
			((atom u)
			 (badfunchk (caar form) u nil)
			 (setq form (cons (cons (getopr u) aryp) (cdr form)))
			 (go a))
			((eq (caar u) 'lambda)
			 (if aryp
			     (merror "Improper array call")
			     (return (mlambda u (cdr form)
					      (caar form) noevalargs form))))
			(t (return (mapply1 u (mevalargs (cdr form))
					    (caar form) form)))))))
	(t (mapply1 (caar form) (mevalargs (cdr form)) (caar form) form))))

(defun getl-lm-fcn-prop (sym props &aux fn typ)
  (check-arg sym symbolp "symbol")
  (setq fn sym)
  (cond ((functionp fn)
	 (setq typ 'subr))
	((macro-function sym)
	 (setq typ 'macro))
	((setq fn (symbol-array sym))
	 (setq typ 'array))
	((setq fn (get sym 'mfexpr*))
	 (setq typ 'mfexpr*)))
  (and typ (member typ props :test #'eq) (list typ fn)))


(defmfun meval2 (newargs old)
  (declare (special aryp))
  (let ((new (cons (car old) newargs)) nosimp)
    (cond ((not (member 'simp (cdar old) :test #'eq))
	   (if (and (not (eq (caar new) 'mlist)) (equal new old)) old new))
	  ((prog2 (setq nosimp (not (get (caar new) 'operators))) (alike1 new old))
	   (if nosimp old (cons (delsimp (car old)) (cdr old))))
	  (nosimp (if aryp new (cons (cons (caar new) '(simp)) newargs)))
	  (t (cons (cons (caar new) aryp) newargs)))))

(defun mparams (vars)
  (mapcar #'(lambda (x)
	      (cond ((atom x) x)
		    ((atom (cadr x)) (cadr x))
		    (t (cadadr x))))
	  (cdr vars)))

(defmfun mop (form)
  (if (eq (caar form) 'mqapply)
      (cadr form)
      (caar form)))

(defmfun margs (form)
  (if (eq (caar form) 'mqapply)
      (cddr form)
      (cdr form)))

(defun badfunchk (name val flag)
  (if (or flag (numberp val) (member val '(t nil $%e $%pi $%i) :test #'eq))
      (if (and (atom name) (not (equal val name)))
	  (merror "~:M evaluates to ~M~
		  ~%Improper name or value in functional position."  name val)
	  (merror "Improper name or value in functional position:~%~M" val))))


(defun mbind-doit (lamvars fnargs fnname)
  "Makes a new frame where the variables in the list LAMVARS are bound
to the corresponding elements in FNARGS.  Note that these elements are
used tels quels, without calling MEVAL.
If FNNAME is non-NIL, it designates a function call frame.
This function does not handle errors properly, use the MBIND
wrapper for this."
  (declare (special bindlist mspeclist))
  (do ((vars lamvars (cdr vars))
       (args fnargs (cdr args)))
      ((cond ((and vars args) nil)
	     ((and (null vars) (null args)))
	     (t (assert fnname (fnname)
			"Expected a maxima function designator but got NIL.")
		(merror "Too ~M arguments supplied to ~M:~%~M"
			(if vars "few" "many")
			(cons (ncons fnname) lamvars)
			(cons '(mlist) fnargs)))))
    (let ((var (car vars)))
      (if (not (symbolp var))
	  (merror "Only symbolic atoms can be bound:~%~M" var))
      (let ((value (if (boundp var) (symbol-value var) munbound)))
	(mset var (car args))
	(psetq bindlist (cons var bindlist)
	       mspeclist (cons value mspeclist))))))

(defun mbind (lamvars fnargs fnname)
  "Error-handling wrapper around MBIND-DOIT."
  (handler-case
      (let ((old-bindlist bindlist) win)
	(declare (special bindlist))
	(unwind-protect
	     (prog1
		 (with-$error (mbind-doit lamvars fnargs fnname))
	       (setq win t))
	  (unless win
	    (unless (eq bindlist old-bindlist)
	      (munbind (nreverse (ldiff bindlist old-bindlist))))
	    (when fnname
	      (pop-mlambda-call-stack fnname)))))
    (maxima-$error (c)
      (apply #'merror (cdr (the-$error c)))
      ;; Make absolutely sure that this handler (and mbind) doesn't
      ;; return in this situation since other code depends on this
      ;; behaviour.
      (throw 'macsyma-quit t))))

;;; For testing purposes

#+ignore
(defmfun $show_mbind_data ()
  (format t "~&~{~a = ~a~%~}"
	  (mapcan #'(lambda (x) (list x (symbol-value x)))
		  '(bindlist mspeclist $values *mlambda-call-stack*)))
  (finish-output)
  (values))

(defmfun munbind (vars)
  (dolist (var (reverse vars))
    (cond ((eq (car mspeclist) munbound)
	   (makunbound var)
	   (setf $values (delete var $values :count 1 :test #'eq)))
	  (t (let ((munbindp t)) (mset var (car mspeclist)))))
    (setq mspeclist (cdr mspeclist) bindlist (cdr bindlist))))

;;This takes the place of something like
;; (DELETE (ASSOC (NCONS VAR) $DEPENDENCIES) $DEPENDENCIES 1)

(defun mfunction-delete (var fn-a-list)
  (delete (assoc (ncons var) fn-a-list :test #'equal) fn-a-list :count 1 :test #'equal))

(defmspec mlocal (l)
  (setq loclist (cons nil loclist))
  (let ((mlocp t)) (meval `(($local) ,@(cdr l)))))

(defmspec $local (l)
  (setq l (cdr l))
  (if (not mlocp) (merror "Improper call to `local'"))
  (dolist (var l)
    (cond ((not (symbolp var))
	   (improper-arg-err var '$local))
	  ((and (mget var 'array)
		(arrayp (symbol-array var)))
	   (merror "Attempt to bind a complete array ~M" var)))
    (setq mproplist (cons (get var 'mprops) mproplist)
	  factlist (cons (get var 'data) factlist))
    (dolist (fact (car factlist))
      (putprop fact -1 'ulabs))
    (progn
      (mfunction-delete var $functions)
      (mfunction-delete var $macros)
      (mfunction-delete var $dependencies))
    (setf $arrays (delete var $arrays :count 1 :test #'eq))
    (zl-remprop var 'mprops)
    (zl-remprop var 'data))
  (rplaca loclist (reverse l))
  (setq mlocp nil)
  '$done)

(defun munlocal ()
  (dolist (var (car loclist))
    (let ((mprop (car mproplist))
	  (y nil)
	  (fact (car factlist)))
      (remcompary var)
      (cput var mprop 'mprops)
      (cond ((setq y (old-get mprop 'mexpr))
	     (add2lnc (cons (ncons var) (cdadr y)) $functions))
	    (t (mfunction-delete var $functions)))
      (cond ((setq y (old-get mprop 'mmacro))
	     (add2lnc (cons (ncons var) (cdadr y)) $macros))
	    (t (mfunction-delete var $macros)))
      (cond ((or (old-get mprop 'array) (old-get mprop 'hashar))
	     (add2lnc var $arrays))
	    (t (setf $arrays (delete var $arrays :count 1 :test #'eq))))
      (cond ((setq y (old-get mprop 'depends))
	     (add2lnc (cons (ncons var) y) $dependencies))
	    (t (mfunction-delete var $dependencies)))
      (rempropchk var)
      (mapc #'remov (get var 'data))
      (cput var fact 'data)
      (dolist (u fact)
	(zl-remprop u 'ulabs))
      (setq mproplist (cdr mproplist)
	    factlist (cdr factlist))))
  (setq loclist (cdr loclist)))

(defmacro msetq (a b)
  `(mset ',a ,b))

;; A "run-time macro" needed by MATCOM/MATRUN.
;;works with the defms
(defmspec msetq (l)
  (twoargcheck l)
  (mset (simplifya (cadr l) nil) (meval (caddr l))))

(defun mset (x y)
  (prog nil
     (cond ((or (null $setcheck)
		(eq $setcheck '$setcheck)))
	   ((and (or (atom $setcheck)
		     (memalike x (cdr $setcheck))
		     (and (not (atom x))
			  (memalike (caar x) (cdr $setcheck))))
		 (not (eq x y)))
	    (displa (list '(mtext) (disp2 x) '| set to | y))
	    (if $setcheckbreak
		(let (($setval y))
		  (merrbreak t)
		  (setq y $setval)))))
     (cond ((atom x)
	    (when (or (not (symbolp x))
		      (member x '(t nil) :test #'eq)
		      (mget x '$numer)
		      (char= (getcharn x 1) #\&))
	      (if munbindp (return nil))
	      (if (mget x '$numer)
		  (merror "~:M improper value assignment to a numerical quantity" x)
		  (merror "~:M improper value assignment" x)))
	    (let ((f (get x 'assign)))
	      (if (and f (or (not (eq x y))
			     (member f '(neverset read-only-assign) :test #'eq)))
		  (if (eq (funcall f x y) 'munbindp) (return nil))))
	    (cond ((and (not (boundp x))
			(not dsksetp))
		   (add2lnc x $values))
		  ((and (not (eq x y))
			(optionp x))
		   (if $optionset (mtell "~:M option is being set.~%" x))
		   (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	    (return (setf (symbol-value x) y)))

           ;; ---------- begin code copied & modified from defstruct.lisp

           ;; Check to see if the operator has an mset_extension_operator.
           ;; If so, this says how to do assignments. Examples, a@b:x. Put mset_extension_operator
           ;; of mrecord-assign on the atom $@.  To allow [a,b]:[3,4] put op on mlist.
           ;; arguably we could use mget, mfuncall, and $mset_extension_operator  and
           ;; allow this to be done at the maxima level instead of lisp.

           ;; X is could be something like (($FOO ARRAY) 42), in which case it is meaningful
           ;; to look for an assignment operator associated either with $FOO itself or with
           ;; $FOO's object type, with "object type" = (CAAR (SYMBOL-VALUE '$FOO)).

           ((let*
              ((x-value (if (boundp (caar x)) (symbol-value (caar x))))
               (mset-extension-op
                 (cond
                   ((get (caar x) 'mset_extension_operator))
                   ((and
                      (not (atom x-value))
                      (get (caar x-value) 'defstruct-template)
                      (get (caar x-value) 'mset_extension_operator))))))
              (if mset-extension-op
                (return-from mset (funcall mset-extension-op x y)))))

           ;; ---------- end code copied & modified from defstruct.lisp

	   ((member 'array (cdar x) :test #'eq)
	    (return (arrstore x y)))
	   (t (merror "Improper value assignment:~%~M" x)))))

;; ---------- begin code copied & modified from defstruct.lisp

;; CHANGES WRT FATEMAN'S STUFF.
;; (1) $NEW BARFS IF #ARGUMENTS != 1, OR ARGUMENT HAS NO DEFSTRUCT, OR WRONG NUMBER OF INITIALIZERS.
;; (2) $DEFSTRUCT ALLOWS 1 OR MORE ARGUMENTS, RETURNS A LIST OF DEFSTRUCTS.
;; (3) USE $PUT AND $GET TO MAINTAIN DEFSTRUCT PROPERTIES 
;;     (RENAMED TO $DEFSTRUCT_DEFAULT AND $DEFSTRUCT_TEMPLATE).
;;     THIS MAKES DEFSTRUCT PROPERTIES VISIBLE TO USER VIA GET AND PROPVARS.
;;     ALSO, THIS MAKES `KILL' KILL DEFSTRUCTS.
;; (4) @ EVALUATES LHS AND QUOTES RHS
;; (5) $STRUCTURES INFOLIST
;; (6) LBP = 190, RBP = 191 (HIGHER PRECEDENCE, LEFT-ASSOCIATIVE)
;; (7) A@B => A@B WHEN B IS NOT BOUND TO SOMETHING OTHER THAN ITSELF
;; (8) DISALLOW @ APPLIED TO EXPRESSIONS W/ OPERATOR NOT DECLARED BY DEFSTRUCT
;; (9) MAKE RECORD AND LIST ASSIGNMENT FUNCTIONS LISP FUNCTIONS (STRIP OFF $ FROM NAME)
;;     ALSO MAKE PROPERTY SYMBOLS LISP SYMBOLS (STRIP OFF $ FROM NAME)
;; (10) EXTEND KILL TO TAKE ITEMS OFF $STRUCTURES AND REMOVE DEFSTRUCT PROPERTIES
;; (11) EXTEND KILL TO RECOGNIZE KILL(X@Y)
;; (12) EVALUATE INITIALIZERS IN $DEFSTRUCT AND IN $NEW
;; (13) DISPLAY FIELDS WHICH HAVE BEEN ASSIGNED VALUES AS FOO(X = BAR, Y = BAZ)

(setf (get '$@ 'mset_extension_operator) 'mrecord-assign)

;;  defstruct(f(x,y,z));
;;  myrecord: new(f);
;;  myrecord@y:45;
;;  myrecord;  ==>   f(x,45,z)

;; initializers are possible
;; defstruct(f(x,y=3.14159, z));
;; ff:new(f)  ==>   f(x,3.14159,z)
;; ff@y:2.71828 ==>  ff is  f(x,2.71828,z).

;; the @ syntax can also be used instead of substinpart.

;; k:  h(g(aa,bb),cc);
;; k@1@2:dd; change aa to dd.
;; k;

(defmfun mrecord-assign (@-expr value)
  ;; assume @-expr is  (($@..)  instance-name  field-name)
  (let*
    ((instance (cadr @-expr))
	 (field (caddr @-expr))
	 (object (meval instance))
     template)
    (if (not (and (consp object) (consp (car object)) (setq template (get (caar object) 'defstruct-template))))
      (merror "Left-hand side doesn't appear to be a defstruct object:~%~M" instance)
      (let
        ((index
           (if (integerp field)
             field ;;; allow foo@3, also
             (position field template)))) ;field->integer
        (if (null index) (merror "Unknown field in record:~%~M" field))
        (if (< 0 index (length object)) (setf (elt object index) value)
          (merror "Illegal instance:~%~M @ ~M" instance field))
        value))))

;; MRECORD-KILL is very similar to MRECORD-ASSIGN. Might consider merging the two somehow.

(defmfun mrecord-kill (@-expr)
  (let*
    ((instance (cadr @-expr))
     (field (caddr @-expr))
     (object (meval instance))
     template)
    (if (not (and (consp object) (consp (car object)) (setq template (get (caar object) 'defstruct-template))))
      (merror "Left-hand side doesn't appear to be a defstruct object:~%~M" instance)
      (let
        ((index
           (if (integerp field)
             field
             (position field template))))
        (if (null index) (merror "Unknown field in record:~%~M" field))
        (if (< 0 index (length object)) (setf (elt object index) (elt template index))
          (merror "Illegal instance:~%~M @ ~M" instance field))))))

(defmspec $@ (L)
  (let*
    ((a (cadr L))
     (b (caddr L))
     (e ($@-function (meval a) b)))
    (if (eq e b) L e)))

(defun $@-function (in fn)
  (if (not (listp in))(list '(%@) in fn) ;noun form
    (let* ((index  
	    (if (integerp fn) fn ;;; allow foo@3, also
	      (position fn (get (caar in) 'defstruct-template))))) ;field->integer
    (if (null index) (merror "Unknown field in record:~%~M" fn))
    (if  (< 0 index (length in))
	(elt in index) (merror "Illegal instance:~%~M @ ~M" in fn))
   )))

(defun dimension-defstruct (form result)
  (let
    ((L1 (cdr (get (caar form) 'defstruct-template)))
     (L2 (cdr form)))
    (dimension-function (cons (car form) (mapcar #'(lambda (e1 e2) (if (eq e1 e2) e1 `((mequal) ,e1 ,e2))) L1 L2)) result)))

;; L looks like defstruct (foo(...), bar(...), baz(...)).
;; Process each argument and return a list of declared structures.

(defmspec $defstruct (L)
  `((mlist) ,@(mapcar 'defstruct1 (cdr L))))

(defvar $structures '((mlist)))

(defun defstruct1 (z) ;; z will look like (($whatever) $a $b $c)
   ;; store the template
  (putprop (caar z) (namesonly z) 'defstruct-template)
  ;; set the initialization
  (putprop (caar z) (initializersmostly z) 'defstruct-default)
  (setf (get (caar z) 'dimension) 'dimension-defstruct)
  (nconc $structures (list (get (caar z) 'defstruct-default)))
  (get (caar z) 'defstruct-default))

(defun namesonly(r)			; f(a,b,c) unchanged, f(a=3,b=4,c=5) -> f(a,b,c)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal)(second z))
				(t (merror "~% Expected record initializer, not ~M." z))))
		       (cdr r))))

(defun initializersmostly(r);; f(a=3,b,c=5) -> f(3,b,5)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal) (meval (third z)))
				(t (merror "~% Expected record initializer, not ~M." z))))
		       (cdr r))))

(defmspec $new (h)
  (if (not (= (length (cdr h)) 1))
    (merror "~% new: expected exactly one argument, not ~M." (length (cdr h))))

  (let ((recordname (cadr h)))
    (cond
      ((symbolp recordname)  ;; the case of, e.g.  new(f);
       (if (null (get recordname 'defstruct-default))
         (merror "~% new: don't know anything about `~M'." recordname))

       (copy-tree (get recordname 'defstruct-default)))

      ;; assume there is some initialization here e.g. new (f(5,6,7))
      (t
        (let ((recordop (caar recordname)) (recordargs (cdr recordname)))
          (if (null (get recordop 'defstruct-default))
            (merror "~% new: don't know anything about `~M'." recordop))

          (if (not (= (length recordargs) (length (cdr (get recordop 'defstruct-default)))))
            (merror "~% new: wrong number of arguments in initializer; expected ~M, not ~M."
                    (length (cdr (get recordop 'defstruct-default))) (length recordargs)))

          `(,(car recordname) ,@(mapcar #'meval (cdr recordname))))))))

;; Following property assignments comprise the Lisp code equivalent to infix("@", 190, 191)

(defprop $@ %@ verb) 
(defprop $@ &@ op) 
(defprop &@ $@ opr) 
;; !! FOLLOWING LINE MOVED TO NPARSE.LISP TO AVOID COMPILER ERROR
;; !! (MOVING SUPRV1.LISP HIGHER IN MAXIMA.SYSTEM CAUSES MYSTERIOUS ERROR)
;; !! (define-symbol '&@) 
(defprop $@ dimension-infix dimension) 
(defprop $@ (#\@) dissym) 
(defprop $@ msize-infix grind) 
(defprop $@ 190 lbp) 
(defprop $@ 191 rbp) 
(defprop $@ parse-infix led) 
(defprop %@ dimension-infix dimension) 
(defprop %@ (#\@) dissym) 
(defprop %@ $@ noun) 

;; The follow code implements PARALLEL LIST assignment.
;; it is consistent with commercial macsyma.  [a,b,c]:[x,y,z] means
;;  about the same as a:x, b:y, c:z.  Actually it
;; evaluates x,y,z  BEFORE any assignments to a,b,c, hence parallel.
;; Also implemented is [a,b,c]:x  which evaluates x once and assigns
;; to a,b,c.
;; value returned is (evaluated x to ex)  [ex,ex,ex].

;; quiz .  [a,b]:[b,2*a].  produces values a=b, b= 2*a.
;; re-execute the statement 4 times. what do you get?  [4b, 8a]
;;         
;; a neat application of parallel assignment is this version of
;; a gcd algorithm (for integers)...
;; kgcd(a,b):=(while b#0 do [a,b]:[b,remainder(a,b)], abs(a));
;; The extended euclidean algorithm looks even better with parallel
;; assignment.

;; add MLIST to possible operators on the left hand side of 
;; an assignment statement.

(setf (get 'mlist 'mset_extension_operator) 'mlist-assign)

(defmfun mlist-assign (tlist vlist)
  ;;  tlist is  ((mlist..)  var[0]... var[n])  of targets
  ;; vlist is either((mlist..)  val[0]... val[n]) of values
  ;; or possibly just one value.
  ;; should insert some checking code here
  (if (and (listp vlist)
	   (eq (caar vlist) 'mlist)
	   (not (= (length tlist)(length vlist))))
      (merror "Illegal list assignment: different lengths of ~M and ~M." tlist vlist))
  (setq tlist
        `((mlist)
          ,@(mapcar
              #'(lambda (x) (if (symbolp x) x `(,(car x) ((mquote) ,@(mapcar #'meval (cdr x))))))
              (cdr tlist))))
  (unless (and (listp vlist)
	   (eq (caar vlist) 'mlist))
    (setf vlist (cons (car tlist) ;; if [a,b,c]:v  then make a list [v,v,v]
		      (make-sequence 'list (1-(length tlist)) :initial-element vlist))))
  (map nil #'mset (cdr tlist)(cdr vlist))
   vlist)

;; ---------- end code copied & modified from defstruct.lisp

(defmspec $ev (l)
  (setq l (cdr l))
  (let ((evp t) (nounl nounl) ($float $float) ($numer $numer)
	($expop $expop) ($expon $expon) ($doallmxops $doallmxops)
	($doscmxops $doscmxops) (derivflag derivflag) ($detout $detout)
	(nounsflag nounsflag) (rulefcnl rulefcnl))
    (if (and (cdr l) (null (cddr l)) (eq (car l) '$%e) (eq (cadr l) '$numer))
	(setq l (append l '($%enumer))))
    (do ((l (cdr l) (cdr l)) (bndvars) (bndvals) (locvars) (exp (car l))
	 (subsl) (evflg 0) (ratf) (derivlist) (evfunl) (funcl) (predflg)
	 (noeval (member '$noeval (cdr l) :test #'eq)))
	((null l)
	 (mbinding (bndvars bndvars)
		   (meval `((mlocal) ,@locvars))
		   (let ($translate) (mapc #'meval1 funcl))
		   (let ($numer) (setq exp (mevalatoms exp)))
		   (if ($ratp exp) (setq ratf t exp ($ratdisrep exp)))
		   (if (specrepp exp) (setq exp (specdisrep exp)))
		   (when subsl
		     (setq exp (simplify exp))
		     (dolist (item subsl)
		       (setq exp (maxima-substitute (meval (car item))
						    (meval (cdr item))
						    exp)))))
	 (mbinding (bndvars bndvals)
		   (if (and $numer noeval $%enumer)
		       (setq exp (maxima-substitute %e-val '$%e exp)))
		   (setq exp (if noeval
				 (resimplify exp)
				 (simplify (if predflg (mevalp exp) (meval1 exp)))))
		   (if (or (> evflg 0) $infeval)
		       (prog (exp1)
			  (setq exp (specrepcheck exp))
			  loop (do ((l evfunl (cdr l)) (exp2 exp))
				   ((null l) (setq exp1 (meval exp2)))
				 (setq exp2 (list (ncons (car l)) exp2)))
			  (dolist (item subsl)
			    (setq exp1 (maxima-substitute (meval (car item))
							  (meval (cdr item))
							  exp1)))
			  (cond ((or (and (not $infeval)
					  (= (setq evflg (1- evflg)) 0))
				     (prog2 (setq exp1 (specrepcheck exp1))
					 (alike1 exp exp1)))
				 (setq exp exp1))
				(t (setq exp exp1) (go loop)))))
		   (if (and ratf (not $numer) (not $float))
		       (setq exp (let ($norepeat) (ratf exp)))))
	 (munlocal)
	 exp)
      (if (not (or (atom (car l))
		   (member 'array (cdaar l) :test #'eq)
		   (member (caaar l) '(mquote msetq mlist mequal mdefine mset
				     mdefmacro $expand $local $derivlist) :test #'eq)))
	  (setq l (cons (meval (car l)) (cdr l))))
      (cond ((or (atom (car l)) (member 'array (cdaar l) :test #'eq) (eq (caaar l) 'mquote))
	     (or (and (symbolp (car l))
		      (cond ((eq (car l) '$eval) (setq evflg (1+ evflg)))
			    ((member (car l) '($noeval $rescan) :test #'eq))
			    ((eq (car l) '$detout)
			     (setq $doallmxops nil $doscmxops nil $detout t))
			    ((eq (car l) '$numer) (setq $numer t $float t))
			    ((eq (car l) '$nouns) (setq nounsflag t))
			    ((eq (car l) '$pred) (setq predflg t))
			    ((eq (car l) '$expand)
			     (setq $expop $maxposex $expon $maxnegex))
			    ((eq (car l) '%derivative)
			     (setq derivflag t derivlist nil))
			    ((get (car l) 'evflag)
			     (setq bndvars (cons (car l) bndvars)
				   bndvals (cons (get (car l) 'evflag) bndvals)))
			    ((get (car l) 'evfun)
			     (setq exp (evfunmake (car l) exp)
				   evfunl (nconc evfunl (ncons (car l)))))))
		 (let ((fl (meval (car l))))
		   (cond ((symbolp fl)
			  (cond ((eq fl '$diff)
				 (setq l (list* nil '$del (cdr l))))
				((eq fl '$risch)
				 (setq l (list* nil '$integrate (cdr l)))))
			  (setq nounl (cons ($nounify fl) nounl)))
			 ((numberp fl) (improper-arg-err (car l) '$ev))
			 ((stringp fl) (improper-arg-err (car l) '$ev))
			 ((eq (caar fl) 'mlist)
			  (setq l (append fl (cdr l))))
			 ((member (caar fl)
				'(msetq mequal mdefine mdefmacro mset) :test #'eq)
			  (setq l (list* nil fl (cdr l))))
			 (t (improper-arg-err (car l) '$ev))))))
	    ((not (member (caaar l) '(msetq mlist mequal mdefine mdefmacro
				    $expand $local $derivlist mset) :test #'eq))
	     (improper-arg-err (car l) '$ev))
	    ((eq (caaar l) '$expand)
	     (cond ((null (cdar l)) (setq $expop $maxposex $expon $maxnegex))
		   ((null (cddar l)) (setq $expop (cadar l) $expon $maxnegex))
		   (t (setq $expop (cadar l) $expon (caddar l)))))
	    ((member (caaar l) '(mdefine mdefmacro) :test #'eq)
	     (let ((fun (cadar l)) $use_fast_arrays)
	       (if (eq (caar fun) 'mqapply) (setq fun (cadr fun)))
	       (setq fun ($verbify (caar fun)))
	       (setq funcl (nconc funcl (ncons (car l)))
		     locvars (append locvars (ncons fun)))
	       (if (rulechk fun) (setq rulefcnl (cons fun rulefcnl)))))
	    ((eq (caaar l) '$local) (setq locvars (append locvars (cdar l))))
	    ((eq (caaar l) '$derivlist) (setq derivflag t derivlist (cdar l)))
	    ((and (eq (caaar l) 'mset)
		  (setq l (cons (list '(msetq) (meval (cadar l)) (caddar l))
				(cdr l)))
		  nil))
	    ((member (caaar l) '(msetq mequal) :test #'eq)
	     (if (and (msetqp (car l)) (msetqp (caddar l)))
		 (setq l (nconc (|:SPREAD| (car l)) (cdr l))))
	     (if (or noeval (not (atom (cadar l))))
		 (setq subsl (nconc subsl (list (cons (caddar l) (cadar l))))))
	     (if (atom (cadar l))
		 (setq bndvars (cons (cadar l) bndvars)
		       bndvals (cons (meval (specrepcheck (caddar l))) bndvals))))
	    (t (setq l (append (car l) (cdr l))))))))
(defmfun mevalatoms (exp)
  (cond ((atom exp) (meval1 exp))
	((member 'array (cdar exp) :test #'eq)
	 (let (exp1)
	   (let ((evarrp t)) (setq exp1 (catch 'evarrp (meval1 exp))))
	   (if (eq exp1 'notexist)
	       (cons (car exp) (mapcar #'mevalatoms (cdr exp)))
	       exp1)))
	((eq (caar exp) 'mquote) (cadr exp))
	((member (caar exp) '(msetq $define) :test #'eq)
	 (list (car exp) (cadr exp) (mevalatoms (caddr exp))))
	((or (and (eq (caar exp) '$ev)
		  (cdr exp)
		  (or (null (cddr exp)) (equal (cddr exp) '($eval))))
	     (eq (caar exp) 'mprogn))
	 (cons (car exp) (cons (mevalatoms (cadr exp)) (cddr exp))))
	((member (caar exp) '($sum $product %sum %product) :test #'eq)
	 (if msump
	     (meval exp)
	     (list (car exp) (cadr exp) (caddr exp)
		   (mevalatoms (cadddr exp)) (mevalatoms (car (cddddr exp))))))
	((and (eq (caar exp) '$%th) (eq (ml-typep (simplify (cadr exp))) 'fixnum))
	 (meval1 exp))
	((prog2 (autoldchk (caar exp))
	     (and (or (getl-fun (caar exp) '(fsubr fexpr))
		      (getl (caar exp) '(mfexpr* mfexpr*s)))
		  (not (get (caar exp) 'evok))))
	 exp)
	((mgetl (caar exp) '(mfexprp t-mfexpr))
	 (cons (car exp)
	       (do ((a (or (cdr (mget (caar exp) 't-mfexpr))
			   (cdadr (mget (caar exp) 'mexpr)))
		       (cdr a))
		    (b (cdr exp) (cdr b)) (l))
		   ((not (and a b)) (nreverse l))
		 (cond ((mdeflistp a)
			(return (nreconc l (if (mquotep (cadar a))
					       b
					       (mapcar #'mevalatoms b)))))
		       ((mquotep (car a)) (setq l (cons (car b) l)))
		       (t (setq l (cons (mevalatoms (car b)) l)))))))
	((or (eq (caar exp) 'mmacroexpanded)
	     (and $transrun (get (caar exp) 'translated-mmacro))
	     (mget (caar exp) 'mmacro))
	 (mevalatoms (mmacroexpand exp)))
	(t (cons (car exp) (mapcar #'mevalatoms (cdr exp))))))

;; evok properties
(mapc #'(lambda (x) (putprop x t 'evok))
      '($map $maplist $fullmap $matrixmap $fullmapl $outermap $scanmap $apply))

(defun evfunmake (fun exp)
  (if (msetqp exp)
      (list (car exp) (cadr exp) (evfunmake fun (caddr exp)))
      (list (ncons fun) exp)))

(defun |:SPREAD| (x)
  (do ((val (do ((x x (caddr x))) (nil)
	      (if (not (msetqp (caddr x))) (return (caddr x)))))
       (x x (caddr x)) (l))
      ((not (msetqp x)) l)
    (setq l (cons (list (car x) (cadr x) val) l))))

(defmfun msetqp (x)
  (and (not (atom x)) (eq (caar x) 'msetq)))

(defmfun mquotep (x)
  (and (not (atom x)) (eq (caar x) 'mquote)))

(defmspec mquote (form)
  (cadr form))

(defmfun $subvarp (x)
  (and (not (atom x)) (member 'array (cdar x) :test #'eq) t))

(defmfun mseterr (x y)
  (if munbindp
      'munbindp
      (merror "Attempt to set ~:M to ~M~%Improper value assignment" x y)))

;; assign properties
(mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
      '(($linel msetchk) (*read-base* msetchk) (*print-base* msetchk) (modulus msetchk)
	($infolists neverset) ($trace neverset) ($ratweights msetchk)
	($ratvars msetchk) ($setcheck msetchk) ($gcd msetchk)
	($dotassoc msetchk) ($ratwtlvl msetchk) ($ratfac msetchk)
	($all neverset) ($numer numerset) ($fortindent msetchk)
	($gensumnum msetchk) ($genindex msetchk) ($fpprintprec msetchk)
	($floatwidth msetchk) ($parsewindow msetchk) ($optimprefix msetchk)
	($ttyintnum msetchk)))

(defmfun msetchk (x y)
  (cond ((member x '(*read-base* *print-base*) :test #'eq)
	 (cond ((eq y 'roman))
	       ((or (not (fixnump y)) (< y 2) (> y 35)) (mseterr x y))
	       ((eq x '*read-base*))))
	((member x '($linel $fortindent $gensumnum $fpprintprec $floatwidth
		   $parsewindow $ttyintnum) :test #'eq)
	 (if (not (fixnump y)) (mseterr x y))
	 (if (eq x '$linel) (setq linel y))
	 (cond ((and (member x '($fortindent $gensumnum $floatwidth $ttyintnum) :test #'eq) (< y 0))
		(mseterr x y))
	       ((and (eq x '$parsewindow) (< y -1)) (mseterr x y))
	       ((and (eq x '$fpprintprec) (or (< y 0) (= y 1))) (mseterr x y))))
	((member x '($genindex $optimprefix) :test #'eq) (if (not (symbolp y)) (mseterr x y)))
	((eq x '$dotassoc) (cput 'mnctimes y 'associative))
	((eq x 'modulus)
	 (cond ((null y))
	       ((integerp y)
		(if (or (not (primep y)) (member y '(1 0 -1)))
		    (mtell "Warning: `modulus' being set to ~:M, a non-prime.~%" y)))
	       (t (mseterr x y))))
	((eq x '$setcheck)
	 (if (not (or (member y '($all t nil) :test #'eq) ($listp y))) (mseterr x y)))
	((eq x '$gcd) (if (not (or (null y) (member y *gcdl* :test #'eq))) (mseterr x y)))
	((eq x '$ratvars)
	 (if ($listp y) (apply #'$ratvars (cdr y)) (mseterr x y)))
	((eq x '$ratfac)
	 (if (and y $ratwtlvl)
	     (merror "`ratfac' and `ratwtlvl' may not both be used at the same time.")))
	((eq x '$ratweights)
	 (cond ((not ($listp y)) (mseterr x y))
	       ((null (cdr y)) (kill1 '$ratweights))
	       (t (apply #'$ratweight (cdr y)))))
	((eq x '$ratwtlvl)
	 (if (and y (not (fixnump y))) (mseterr x y))
	 (if (and y $ratfac)
	     (merror "`ratfac' and `ratwtlvl' may not both be used at the same time.")))))

(defmfun numerset (assign-var y)
  (declare (ignore assign-var))
  (mset '$float y))

(defmfun neverset (x assign-val)
  (declare (ignore assign-val))
  (if munbindp 'munbindp (merror "Improper value assignment to ~:M" x)))

(defmfun mmapev (l)
  (if (null (cddr l))
      (merror "~:M called with fewer than two arguments." (caar l)))
  (let ((op (getopr (meval (cadr l)))))
    (autoldchk op)
    (badfunchk (cadr l) op nil)
    (cons op (mapcar #'meval (cddr l)))))

(defmspec $map (l)
  (apply #'map1 (mmapev l)))

(defmfun map1 n
  (do ((i n (1- i))
       (argi (setarg n (format1 (arg n))) (format1 (arg (1- i))))
       (op (or (mapatom (arg n)) (mop (arg n))))
       (flag (mapatom (arg n))
	     (or flag (setq flag (mapatom argi))
		 (and (not maplp) (not (alike1 (mop argi) op)))))
       (argl nil (cons argi argl))
       (cdrl nil (or flag (cons (margs argi) cdrl))))
      ((= i 1) (if flag
		   (cond ((not $maperror)
			  (if $mapprint (mtell "`map' is doing an `apply'.~%"))
			  (funcer (arg 1) argl))
			 ((and (= n 2) (mapatom (arg 2)))
			  (improper-arg-err (arg 2) '$map))
			 (t (merror "Arguments to `mapl' not uniform - cannot map.")))
		   (mcons-op-args
		    op (apply #'mmapcar (cons (arg 1) cdrl)))))))

(defmspec $maplist (l)
  (let ((maplp t) res)
    (setq res (apply #'map1 (mmapev l)))
    (cond ((atom res) (list '(mlist) res))
	  ((eq (caar res) 'mlist) res)
	  (t (cons '(mlist) (margs res))))))

(defmfun mmapcar n
  (do ((ans nil (cons (funcer (arg 1) argl) ans))
       (argl nil nil))
      ((do ((i n (1- i))) ((= i 1) nil)
	 (when (null (arg i))
	   (when (or (< i n) (do ((j 2 (1+ j))) ((= j n) nil)
			       (if (arg j) (return t))))
	     (if $maperror
		 (merror "Arguments to `mapl' are not of the same length."))
	     (if $mapprint (mtell "`map' is truncating.~%")))
	   (return t))
	 (setq argl (cons (car (arg i)) argl))
	 (setarg i (cdr (arg i))))
       (nreverse ans))))

(defun mapatom (x)
  (or (symbolp x) (mnump x) ($subvarp x)))

(defmfun $mapatom (x)
  (if (mapatom (specrepcheck x)) t))

(defmspec $fullmap (l)
  (setq l (mmapev l))
  (fmap1 (car l) (cdr l) nil))

(defun fmap1 (fn argl fmapcaarl)
  (setq argl (mapcar #'format1 argl))
  (do ((op (or (mapatom (car argl)) (mop (car argl))))
       (fmaplvl (1- fmaplvl)) (cdr1 argl (cdr cdr1)) (argi nil nil)
       (cdrl nil (cons (margs (car cdr1)) cdrl)))
      ((null cdr1)
       (do ((ans nil (cons (if bottom (funcer fn carargl)
			       (fmap1 fn carargl fmapcaarl))
			   ans))
	    (carargl nil nil) (cdrargl nil nil)
	    (cdrl cdrl cdrargl) (bottom nil nil)
	    (done (when (member nil cdrl :test #'eq)
		    (when (dolist (e cdrl) (if e (return t)))
		      (if $maperror
			  (merror
			   "`fullmap' found arguments with incompatible structure."))
		      (if $mapprint (mtell "`fullmap' is truncating.~%")))
		    t)))
	   (done (mcons-op-args op (nreverse ans)))
	 (do ((op (or (setq bottom (or (zerop fmaplvl) (mapatom (caar cdrl))))
		      (mop (caar cdrl))))
	      (eleml cdrl (cdr eleml)) (caareleml nil nil))
	     ((null eleml)
	      (when (and done (dolist (e cdrargl) (if e (return t))))
		(if $maperror
		    (merror "`fullmap' found arguments with incompatible structure."))
		(if $mapprint (mtell "`fullmap' is truncating.~%"))))
	   (setq caareleml (caar eleml))
	   (or bottom
	       (setq bottom
		     (or (mapatom caareleml)
			 (not (alike1 op (mop caareleml)))
			 (and fmapcaarl (not (eq (caar caareleml) fmapcaarl))))))
	   (or done (setq done (null (cdar eleml))))
	   (setq carargl (nconc (ncons caareleml) carargl)
		 cdrargl (nconc cdrargl (ncons (cdar eleml)))))))
    (setq argi (car cdr1))
    (if (or (mapatom argi)
	    (not (alike1 op (mop argi)))
	    (and fmapcaarl (not (eq (caar argi) fmapcaarl))))
	(cond ($maperror (merror "Incorrect call to `fullmap'."))
	      (t (if $mapprint (mtell "`fullmap' is doing an `apply'.~%"))
		 (return (funcer fn argl)))))))

(defmspec $matrixmap (l)
  (let ((fmaplvl 2))
    (apply #'fmapl1 (mmapev l))))

(defmspec $fullmapl (l)
  (apply #'fmapl1 (mmapev l)))

(defmfun fmapl1 n
  (let ((header '(mlist)) argl)
    (setq argl (fmap1 (arg 1)
		      (mapcar
		       #'(lambda (z)
			   (cond ((not (mxorlistp z))
				  (merror "Argument to `fullmapl' is not a list or matrix."))
				 ((eq (caar z) '$matrix)
				  (setq header '($matrix))
				  (cons '(mlist simp) (cdr z)))
				 (t z)))
		       (cdr (listify n)))
		      'mlist))
    (if (dolist (e (cdr argl)) (if (not ($listp e)) (return t)))
	argl
	(cons header (cdr argl)))))

(defmspec $outermap (l)
  (apply (if (= (length l) 3) #'fmapl1 #'outermap1) (mmapev l)))

(defmfun outermap1 n
  (let (outargs1 outargs2)
    (cond ((mxorlistp (arg 2))
	   (setq outargs1 (ncons (arg 1)) outargs2 (listify (- 2 n)))
	   (fmapl1 'outermap2 (arg 2)))
	  (t (do ((i 3 (1+ i)))
		 ((> i n) (funcer (arg 1) (listify (- 1 n))))
	       (when (mxorlistp (arg i))
		 (setq outargs1 (listify (1- i))
		       outargs2 (if (< i n) (listify (- i n))))
		 (return (fmapl1 'outermap2 (arg i)))))))))

(defmfun outermap2 n
  (if (not (zerop n))
      (apply #'outermap1 (append outargs1 (listify 1) outargs2))))

(defmfun funcer (fn args)
  (cond ((and (not opexprp) (member fn '(mplus mtimes mexpt mnctimes) :test #'eq))
	 (simplify (cons (ncons fn) args)))
	((or (member fn '(outermap2 constfun) :test #'eq)
	     (and $transrun (symbolp fn) (get fn 'translated)
		  (not (mget fn 'local-fun)) (fboundp fn)))
	 (apply fn (mapcar #'simplify args)))
	(t (mapply1 fn (mapcar #'simplify args) fn
		    nil	;; try to get more info to pass
		    ))))

(defmspec $qput (l)
  (setq l (cdr l))
  (if (not (= (length l) 3)) (wna-err '$qput))
  ($put (car l) (cadr l) (caddr l)))

(defmfun $rem (atom ind)
  (prop1 '$rem atom nil ind))

(defmfun $put (atom val ind)
  (prog1
      (prop1 '$put atom val ind)
    (add2lnc atom $props)))

(defun prop1 (fun atom val ind)
  (nonsymchk atom fun)
  (nonsymchk ind fun)
  (let ((u (mget atom '$props)))
    (cond ((eq fun '$get) (and u (old-get u ind)))
	  ((eq fun '$rem) (and u (zl-remprop u ind) '$done))
	  ((not u) (mputprop atom (list nil ind val) '$props) val)
	  (t (putprop u val ind)))))

(defmspec $declare (l)
  (setq l (cdr l))
  (if (oddp (length l))
      (merror "`declare' takes an even number of arguments."))
  (do ((l l (cddr l)) (vars) (flag nil nil))
      ((null l)
       '$done)
    (cond (($listp (cadr l))
	   (do ((l1 (cdadr l) (cdr l1))) ((if (null l1) (setq flag t)))
	     (meval `(($declare) ,(car l) ,(car l1)))))
	  ((nonsymchk (cadr l) '$declare))
	  (t (setq vars (declsetup (car l) '$declare))))
    (cond (flag)
	  ((member (cadr l) '($evfun $evflag $special $nonarray $bindtest) :test #'eq)
	   (declare1 vars t (stripdollar (cadr l)) nil))
	  ((eq (cadr l) '$noun)
	   (dolist (var vars) (alias (getopr var) ($nounify var))))
	  ((member (cadr l) '($constant $nonscalar $scalar $mainvar) :test #'eq)
	   (declare1 vars t (cadr l) t))
	  ((eq (cadr l) '$alphabetic) (declare1 vars t t '$alphabetic))
	  ((member (cadr l) opers :test #'eq)
	   (if (member (cadr l) (cdr $features) :test #'eq) (declare1 vars t (cadr l) 'kind))
	   (declare1 (mapcar #'getopr vars) t (cadr l) 'opers))
	  ((member (cadr l) (cdr $features) :test #'eq) (declare1 vars t (cadr l) 'kind))
	  ((eq (cadr l) '$feature)
	   (dolist (var vars) (nonsymchk var '$declare) (add2lnc var $features)))
	  (t (merror "Unknown property to `declare': ~:M" (cadr l))))))

(defun declare1 (vars val prop mpropp)
  (dolist (var vars)
    (setq var (getopr var))
    (nonsymchk var '$declare)
    (cond ((eq mpropp 'kind) (declarekind var prop))
	  ((eq mpropp 'opers)
	   (putprop (setq var (linchk var)) t prop) (putprop var t 'opers)
	   (if (not (get var 'operators)) (putprop var 'simpargs1 'operators)))

	  ((eq mpropp '$alphabetic)
       ; Explode var into characters and put each one on the *alphabet* list,
       ; which is used by src/nparse.lisp .
       (dolist (1-char (cdr (coerce (print-invert-case var) 'list)))
	 (add2lnc 1-char *alphabet*)))

	  ((eq prop 'special)(proclaim (list 'special var))
	   (fluidize var))
	  (mpropp
	   (if (and (member prop '($scalar $nonscalar) :test #'eq)
		    (mget var (if (eq prop '$scalar) '$nonscalar '$scalar)))
	       (merror "Inconsistent Declaration: ~:M" `(($declare) ,var ,prop)))
	   (mputprop var val prop))
	  (t (putprop var val prop)))
    (if (and (get var 'op) (operatorp1 var)
	     (not (member (setq var (get var 'op)) (cdr $props) :test #'eq)))
	(setq mopl (cons var mopl)))
    (add2lnc (getop var) $props)))

(defun linchk (var)
  (if (member var '($sum $integrate $limit $diff $transpose) :test #'eq)
      ($nounify var) var))

(defmspec $remove (form)
  (i-$remove (cdr form)))

(defmfun i-$remove (l)
  (if (oddp (length l)) (merror "`remove' takes an even number of arguments."))
  (do ((l l (cddr l)) (vars) (flag nil nil)) ((null l) '$done)
    (cond (($listp (cadr l))
	   (do ((l1 (cdadr l) (cdr l1))) ((if (null l1) (setq flag t)))
	     (i-$remove (list (car l) (car l1)))))
	  ((nonsymchk (cadr l) '$remove))
	  (t (setq vars (declsetup (car l) '$remove))))
    (cond (flag)
	  ((eq (cadr l) '$value) (i-$remvalue vars))
	  ((eq (cadr l) '$function)
	   (remove1 (mapcar #'rem-verbify vars) 'mexpr t $functions t))
	  ((eq (cadr l) '$macro)
	   (remove1 (mapcar #'rem-verbify vars) 'mmacro t $macros t))
	  ((eq (cadr l) '$array) (meval `(($remarray) ,@vars)))
	  ((member (cadr l) '($alias $noun) :test #'eq) (remalias1 vars (eq (cadr l) '$alias)))
	  ((eq (cadr l) '$matchdeclare) (remove1 vars 'matchdeclare t t nil))
	  ((eq (cadr l) '$rule) (remrule vars))
	  ((member (cadr l) '($evfun $evflag $special $nonarray $bindtest
			    $autoload $assign) :test #'eq)
	   (remove1 vars (stripdollar (cadr l)) nil t nil))
	  ((member (cadr l) '($mode $modedeclare) :test #'eq) (remove1 vars 'mode nil 'foo nil))
	  ((eq (cadr l) '$atvalue) (remove1 vars 'atvalues t t nil))
	  ((member (cadr l) '($constant $nonscalar $scalar $mainvar $numer $atomgrad) :test #'eq)
	   (remove1 vars (cadr l) t t nil))
	  ((member (cadr l) opers :test #'eq) (remove1 (mapcar #'linchk vars) (cadr l) nil t nil))
	  ((member (cadr l) (cdr $features) :test #'eq) (remove1 vars (cadr l) nil t nil))
	  ((eq (cadr l) '$feature)
	   (dolist (var vars)
	     (setf $features (delete var $features :count 1 :test #'eq))))
	  ((member (cadr l) '($alphabetic $transfun) :test #'eq)
	   (remove1 vars (cadr l) nil t nil))
	  ((member (cadr l) '($gradef $grad) :test #'eq) (remove1 vars 'grad nil $gradefs t))
	  ((member (cadr l) '($dependency $depend $depends) :test #'eq)
	   (remove1 vars 'depends t $dependencies t))
	  ((member (cadr l) '($op $operator) :test #'eq) (remove1 vars '$op nil 'foo nil))
	  ((member (cadr l) '($deftaylor $taylordef) :test #'eq) (remove1 vars 'sp2 nil t nil))
	  (t (merror "Unknown property to `remove': ~:M" (cadr l))))))

(defun declsetup (x fn)
  (cond ((atom x) (ncons x))
	((eq (caar x) '$nounify) (ncons (meval x)))
	((eq (caar x) 'mlist)
	 (mapcar #'(lambda (var)
		     (cond ((atom var) var)
			   ((eq (caar var) '$nounify) (meval var))
			   (t (improper-arg-err var fn))))
		 (cdr x)))
	(t (improper-arg-err x fn))))

(defmfun remove1 (vars prop mpropp info funp)
  (do ((vars vars (cdr vars)) (allflg))
      ((null vars))
    (nonsymchk (car vars) '$remove)
    (cond ((and (eq (car vars) '$all) (null allflg))
	   (setq vars (append vars (cond ((atom info) (cdr $props))
					 (funp (mapcar #'caar (cdr info)))
					 (t (cdr info))))
		 allflg t))
	  (t
	   (let ((var  (getopr (car vars)))( flag  nil))

	     (cond (mpropp (mremprop var prop)
			   (when (member prop '(mexpr mmacro) :test #'eq)
			     (mremprop var 'mlexprp)
			     (mremprop var 'mfexprp)
			     (if (mget var 'trace)
				 (macsyma-untrace var))))
		   ((eq prop '$op) (kill-operator var))
		   ((and (eq prop '$alphabetic) (mstringp var))
	    (dolist (1-char (cdr (coerce (print-invert-case var) 'list)))
	      (setf *alphabet* (delete 1-char *alphabet* :count 1 :test #'equal))))
		   ((eq prop '$transfun)
		    (remove-transl-fun-props var)
		    (remove-transl-array-fun-props var))
		   ((or (setq flag (member prop (cdr $features) :test #'eq)) (member prop opers :test #'eq))
		    (if flag (unkind var prop))
		    (zl-remprop var prop)
		    (if (not (getl var (delete prop (copy-list opers) :count 1 :test #'eq)))
			(zl-remprop var 'opers)))
		   (t (zl-remprop var prop)))
	     (cond ((eq info t) (rempropchk var))
		   ((eq info 'foo))
		   (funp
		    (mfunction-delete var info))
		   (t
		    (setf info (delete var info :count 1 :test #'eq)))))))))

(defun remove-transl-fun-props (fun)
  (if (mget fun 'trace)
      (macsyma-untrace fun))
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    (fmakunbound fun)
    (zl-remprop fun 'translated-mmacro)
    (mremprop fun 't-mfexpr)
    (zl-remprop fun 'function-mode)
    (if (not (getl fun '(a-expr a-subr)))
	(zl-remprop fun 'translated))))

(defun remove-transl-array-fun-props (fun)
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    (zl-remprop fun 'a-expr)
    (zl-remprop fun 'a-subr)
    (if (not (fboundp fun)) (zl-remprop fun 'translated))))

(defmfun rempropchk (var)
  (if (and (not (mgetl var '($constant $nonscalar $scalar $mainvar $numer
			     matchdeclare $atomgrad atvalues t-mfexpr)))
	   (not (getl var '(evfun evflag translated nonarray bindtest
			    opr sp2 operators opers special data autoload mode)))
	   (not (member var *builtin-$props* :test #'eq)))
      (delete var $props :count 1 :test #'eq)))

(defun rem-verbify (fnname)
  (nonsymchk fnname '$remove)
  ($verbify fnname))

(defmspec $remfunction (l)
  (setq l (cdr l))
  (cond ((member '$all l :test #'eq)
	 (setq l (nconc (mapcar #'caar (cdr $functions))
			(mapcar #'caar (cdr $macros)))))
	(t (setq l (mapcar #'rem-verbify l))
	   (do ((l1 l (cdr l1))) ((null l1) t)
	     (if (not (or (assoc (ncons (car l1)) (cdr $functions) :test #'equal)
			  (assoc (ncons (car l1)) (cdr $macros) :test #'equal)))
		 (rplaca l1 nil)))))
  (remove1 l 'mexpr t $functions t)
  (remove1 l 'mmacro t $macros t)
  (cons '(mlist) l))

(defmspec $remarray (l)
  (setq l (cdr l))
  (cons '(mlist)
	(do ((l l (cdr l)) (x) (pred)) ((null l) (nreverse x))
	  (cond ((eq (car l) '$all) (setq l (append l (cdr $arrays))))
		(t (remcompary (car l)) (setq pred (mremprop (car l) 'array))
		   (setq pred (or (mremprop (car l) 'hashar) pred))
		   (setq pred (or (mremprop (car l) 'aexpr) pred))
		   (setq x (cons (and pred (prog2
					       (setf $arrays (delete (car l) $arrays :count 1 :test #'eq))
					       (car l)))
				 x)))))))

(defun remcompary (x)
  (cond ((eq x (mget x 'array))
	 (zl-remprop x 'array-mode)
	 (zl-remprop x 'array))))

(defmspec $remvalue (form)
  (i-$remvalue (cdr form)))

(defmfun i-$remvalue (l)
  (cons '(mlist)
	(do ((l l (cdr l)) (x) (y)) ((null l) (nreverse x))
	  (cond ((eq (car l) '$all) (setq l (append l (cdr $values))))
		(t (setq x (cons (cond ((atom (car l))
					(if (remvalue (car l) '$remvalue) (car l)))
				       ((setq y (mgetl (caaar l) '(hashar array)))
					(remarrelem y (car l)) (car l)))
				 x)))))))

(defmfun remarrelem (ary form)
  (if (mfilep (cadr ary))
      (i-$unstore (ncons (caar form))))
  (let ((y (car (arraydims (cadr ary)))))
    (arrstore form (cond ((eq y 'fixnum) 0) ((eq y 'flonum) 0.0d0) (t munbound)))))

(defmfun remrule (l)
  (do ((l l (cdr l)) (u))
      ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $rules))))
	  ((get (car l) 'operators) ($remrule (car l) '$all))
	  ((setq u (ruleof (car l))) ($remrule u (car l)))
	  ((mget (car l) '$rule)
	   (zl-remprop (car l) 'expr) (mremprop (car l) '$rule)
	   (setf $rules (delete (car l) $rules :count 1 :test #'eq))))))

(defmfun remalias1 (l aliasp)
  (do ((l l (cdr l)) (u)) ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $aliases))))
	  ((or aliasp (get (car l) 'noun)) (remalias (car l) t))
	  ((setq u (get (car l) 'verb))
	   (zl-remprop (car l) 'verb) (zl-remprop u 'noun)))))

(defmfun mremprop (atom ind)
  (let ((props (get atom 'mprops))) (and props (zl-remprop props ind))))

(defmfun mgetl (atom inds)
  (let ((props (get atom 'mprops))) (and props (getl props inds))))

(defmspec $matrix (L)
  (if (null (cdr L))
    '(($matrix))
    (let ((rows (mapcar #'meval (cdr L))))
      (dolist (row rows)
	(if (not ($listp row)) (merror "matrix: invalid row:~%~M" row)))
      (matcheck rows)
      (cons '($matrix) rows))))

(defmfun matcheck (l)
  (do ((l1 (cdr l) (cdr l1)) (n (length (car l)))) ((null l1))
    (if (not (= n (length (car l1))))
	(merror "All matrix rows are not of the same length."))))

(defun harrfind (form)
  (prog (ary y lispsub iteml sub ncells nitems)
     (setq ary (symbol-array (mget (caar form) 'hashar)))
     (cond ((not (= (aref ary 2) (length (cdr form))))
	    (merror "Array ~:M already has dimension ~:M~%~M"
		    (caar form) (aref ary 2) form)))
     (setq sub (cdr form))
     (setq iteml (aref ary (setq lispsub (+ 3 (rem (hasher sub) (aref ary 0))))))
     a    (cond ((null iteml) (go b))
		((alike (caar iteml) sub) (return (cdar iteml))))
     (setq iteml (cdr iteml))
     (go a)
     b    (cond (evarrp (throw 'evarrp 'notexist))
		((null (setq y (arrfunp (caar form)))) (return (meval2 sub form))))
     (setq y (arrfuncall y sub form))
     (setq ary (symbol-array (mget (caar form) 'hashar)))
     (setq iteml (aref ary (setq lispsub (+ 3 (rem (hasher sub) (aref ary 0))))))
     (setq sub (ncons (cons sub y)))
     (cond (iteml (nconc iteml sub)) (t (setf (aref ary lispsub) sub)))
     (setf (aref ary 1) (setq nitems (1+ (aref ary 1))))
     (cond ((> nitems (setq ncells (aref ary 0)))
	    (arraysize (caar form) (+ ncells ncells))))
     (return y)))

(defun arrfind (form)
  (let ((sub (cdr form)) u v type)
    (setq v (dimcheck (caar form) sub nil))
    (cond (v (setq type (car (arraydims (mget (caar form) 'array))))))
    (cond ((and v (prog2
		      (setq u (apply 'aref (symbol-array (mget (caar form) 'array)) sub))
		      (cond ((eq type 'flonum) (not (= u flounbound)))
			    ((eq type 'fixnum) (not (= u fixunbound)))
			    (t (not (eq u munbound))))))
	   u)
	  (evarrp (throw 'evarrp 'notexist))
	  ((or (not v) (null (setq u (arrfunp (caar form)))))
	   (cond ((eq type 'flonum) 0.0d0)
		 ((eq type 'fixnum) 0)
		 (t (meval2 sub form))))
	  (t (setq u (arrfuncall u sub form))
	     (setf (apply #'aref (symbol-array (mget (caar form) 'array))
			  sub) u)

	     u))))


(defmspec $array (x)
  (setq x (cdr x))
  (cond ($use_fast_arrays
	 (mset (car x) (apply '$make_array '$any (mapcar #'1+ (cdr x)))))
	((symbolp (car x))
	 (funcall #'(lambda (compp)
		      (funcall #'(lambda (fun diml funp old new ncells)
				   (cond ((member '$function diml :test #'eq)
					  (setq diml (delete '$function (copy-list diml)
							     :count 1 :test #'eq) funp t)))
				   (setq diml (mapcar #'meval diml))
				   (cond ((null diml)
					  (wna-err '$array))
					 ((> (length diml) 5)
					  (merror "`array' takes at most 5 indices"))
					 ((member nil (mapcar #'(lambda (u) (eq (ml-typep u) 'fixnum))
							      diml) :test #'eq)
					  (merror "Non-integer dimension - `array'")))
				   (setq diml (mapcar #'1+ diml))
				   (setq new (apply #'*array (cons (if compp fun (gensym))
								   (cons t diml))))
				   (cond ((eq compp 'fixnum) (fillarray new '(0)))
					 ((eq compp 'flonum) (fillarray new '(0.0d0))))
				   (cond ((not (member compp '(fixnum flonum) :test #'eq))
					  (fillarray new (list munbound)))
					 ((or funp (arrfunp fun))
					  (fillarray new (list (cond ((eq compp 'fixnum) fixunbound)
								     (t flounbound))))))
				   (cond ((null (setq old (mget fun 'hashar)))
					  (mputprop fun new 'array))
					 (t (cond ((not (= (afuncall old 2) (length diml)))
						   (merror "Array ~:M already has ~:M dimension(s)"
							   fun (afuncall old 2))))
					    (setq ncells (+ 2 (afuncall old 0)))
					    (do ((n 3 (1+ n))) ((> n ncells))
					      (do ((items (afuncall old n) (cdr items))) ((null items))
						(do ((x (caar items) (cdr x)) (y diml (cdr y)))
						    ((null x)
						     (if (and (member compp '(fixnum flonum) :test #'eq)
							      (not (eq (ml-typep (cdar items)) compp)))
							 (merror "Element and array type do not match:~%~M"
								 (cdar items)))

						     (setf (apply #'aref (symbol-array new)
								  (caar items))
							   (cdar items)))
						  (if (or (not (eq (ml-typep (car x)) 'fixnum))
							  (< (car x) 0)
							  (not (< (car x) (car y))))
						      (merror "Improper index for declared array:~%~M"
							      (car x))))))
					    (mremprop fun 'hashar)
					    (mputprop fun new 'array)))
				   (add2lnc fun $arrays)
				   (if (eq compp 'fixnum) (putprop fun '$fixnum 'array-mode))
				   (if (eq compp 'flonum) (putprop fun '$float 'array-mode))
				   fun)
			       ($verbify (car x)) (cond (compp (setq compp (cdr compp)) (cddr x)) (t (cdr x)))
			       nil nil nil 0))
		  (assoc (cadr x) '(($complete . t) ($integer . fixnum) ($fixnum . fixnum)
				   ($float . flonum) ($flonum . flonum)) :test #'eq)))
	(($listp (car x))
	 (do ((u (cdar x) (cdr u))) ((null u)) (meval `(($array) ,(car u) ,@(cdr x))))
	 (car x))
	(t (merror "Improper first argument to `array':~%~M" (car x)))))


(defmfun $show_hash_array (x)
  (maphash #'(lambda (k v) (format t "~%~A-->~A" k v)) x))

;; If this is T then arrays are stored in the value cell,
;; whereas if it is false they are stored in the function cell
(defmvar $use_fast_arrays nil)

(defmfun arrstore (l r &aux tem index)
  (cond ($use_fast_arrays
	 (cond ((and (boundp (caar l)) (setq tem (symbol-value (caar l))))
		(setq index (mevalargs (cdr l)))
		(let ((the-type (ml-typep tem)))
		  (cond ((eq the-type 'array)
			 (setf (apply #'aref tem index)  r))
			((eq the-type 'hash-table)
			 (cond ((gethash 'dim1 tem)
				(if (cdr index)
				    (error "Array has dimension 1")))
			       (t (or (cdr index)
				      (error "Array has dimension > 1"))))
			 (setf (gethash
				(if (cdr index) index
				    (car index))
				tem) r))
			((eq the-type  'list)
			 (cond ((eq (caar tem) 'mlist)
				(setq index (car index))
				(setf (nth index tem) r)
				r)
			       ((eq (caar tem) '$matrix)
				(setf (nth (second index) (nth (first index) tem)) r)
				r)
			       (t
				(error "The value of ~A is not a hash-table ,an ~
					   array, Maxima list, or a matrix" (caar l)))))
			(t(cond ((eq tem (caar l))
				 (meval* `((mset) ,(caar l)
					   ,(make-equal-hash-table
					     (cdr (mevalargs (cdr l))))))
				 (arrstore l r))
				(t
				 (error "The value of ~A is not a hash-table , ~
                                         an array, a Maxima list, or a matrix" (caar l))))))))
	       (t
		(cond ((mget (caar l) 'hashar)
		       (let ($use_fast_arrays)
			 (arrstore l r)))
		      (t
		       (meval* `((mset) ,(caar l)
				 ,(make-equal-hash-table
				   (cdr (mevalargs (cdr l))))))
		       (arrstore l r))))))
	(t
	 (let ((fun ($verbify (caar l))) ary sub (lispsub 0) hashl mqapplyp)
	   (cond ((setq ary (mget fun 'array))
		  (when (mfilep ary)
		    (i-$unstore (ncons fun)) (setq ary (mget fun 'array)))
		  (dimcheck fun (setq sub (mapcar #'meval (cdr l))) t)
		  (if (and (member (setq fun (car (arraydims ary))) '(fixnum flonum) :test #'eq)
			   (not (eq (ml-typep r) fun)))
		      (merror "Improper assignment to complete array:~%~M" r))
		  (setf (apply #'aref (symbol-array ary) sub)  r))
		 ((setq ary (mget fun 'hashar))
		  (when (mfilep ary)
		    (i-$unstore (ncons fun)) (setq ary (mget fun 'hashar)))
		  (if (not (= (afuncall ary 2) (length (cdr l))))
		      (merror "Array ~:M has dimension ~:M; it was called by ~:M"
			      fun (afuncall ary 2) l))
		  (setq sub (mapcar #'meval (cdr l)))
		  (setq hashl (afuncall ary (setq lispsub
						  (+ 3 (rem (hasher sub) (afuncall ary 0))))))
		  (do ((hashl1 hashl (cdr hashl1)))
		      ((null hashl1)
		       (cond ((not (eq r munbound))
			      (setq sub (ncons (cons sub r)))
			      (cond ((null hashl) (store (afuncall ary lispsub) sub))
				    (t (nconc hashl sub)))
			      (store (afuncall ary 1) (1+ (afuncall ary 1))))))
		    (cond ((alike (caar hashl1) sub)
			   (cond ((eq r munbound) (store (afuncall ary 1)
							 (1- (afuncall ary 1))))
				 (t (nconc hashl (ncons (cons sub r)))))
			   (store (afuncall ary lispsub)
				  (delete (car hashl1) hashl :count 1 :test #'equal))
			   (return nil))))
		  (if (> (afuncall ary 1) (afuncall ary 0))
		      (arraysize fun (* 2 (afuncall ary 0))))
		  r)
		 ((and (eq fun 'mqapply) (mxorlistp (setq ary (meval (cadr l))))
		       (prog2
			   (setq mqapplyp t l (cdr l))
			   nil)))

		 ((and (not mqapplyp)
		       (or (not (boundp fun)) (not (or (mxorlistp (setq ary (symbol-value fun)))
						       (eq (ml-typep ary) 'array)))))
		  (if (member fun '(mqapply $%) :test #'eq) (merror "Illegal use of :"))
		  (add2lnc fun $arrays)
		  (mputprop fun (setq ary (gensym)) 'hashar)
		  (*array ary t 7) (store (afuncall ary 0) 4) (store (afuncall ary 1) 0)
		  (store (afuncall ary 2) (length (cdr l)))
		  (arrstore l r))

		 ((eq (ml-typep ary) 'array)
		  (arrstore-extend ary (mevalargs (cdr l)) r))
		 ((or (eq (caar ary) 'mlist) (= (length l) 2))
		  (cond ((eq (caar ary) '$matrix)
			 (cond ((or (not ($listp r)) (not (= (length (cadr ary)) (length r))))
				(merror "Attempt to assign bad matrix row:~%~M" r))))
			((not (= (length l) 2))
			 (merror "Wrong number of indices:~%~M" (cons '(mlist) (cdr l)))))
		  (let ((index (meval (cadr l))))
		    (cond ((not (eq (ml-typep index) 'fixnum))
			   (merror "Index not an integer:~%~M" index))
			  ((and (> index 0) (< index (length ary)))
			   (rplaca (nthcdr (1- index) (cdr ary)) r))
			  (t (merror "~A - index out of range" index))))
		  r)
		 (t (if (not (= (length l) 3))
			(merror "Wrong number of indices:~%~M" (cons '(mlist) (cdr l))))
		    ($setelmx r (meval (cadr l)) (meval (caddr l)) ary)
		    r))))))

(defun arrfunp (x)
  (or (and $transrun (getl x '(a-expr))) (mgetl x '(aexpr))))

(defun arrfuncall (arrfun subs form)
  (let ((aexprp t))
    (case (car arrfun)
      (aexpr (mapply1 (cadr arrfun) subs (cadr arrfun) form))
      (a-expr (apply (cadr arrfun) subs))
      (a-subr
       (comment "This is what the code used to look like:"
		(eval (nconc (list 'subrcall nil
				   (list 'quote (cadr arrfun))) subs)))
       (system-subrcall* (cadr arrfun) subs)))))

(defun hasher (l)  ; This is not the best way to write a hasher.  But,
  (if (null l)	   ; please don't change this code or you're liable to
      0					; break SAVE files.
      (logand #o77777
	      (let ((x (car l)))
		(cond (($ratp x) (merror "Subscripts may not be in CRE form."))
		      ((or (fixnump x) (floatp x))
		       (+ (if (fixnump x) x (floor (+ x 5d-4)))
			   (* 7 (hasher (cdr l)))))
		      ((atom x) (+ (sxhash x) (hasher (cdr l))))
		      (t (+ 1 (sxhash (caar x)) (hasher (cdr x))
			     (hasher (cdr l)))))))))

(defun arraysize (fun n)
  (prog (old new indx ncells cell item i y)
     (setq old (symbol-array (mget fun 'hashar)))
     (mputprop fun (setq new (gensym)) 'hashar)
     (*array new t (+ n 3))
     (setq new (symbol-array new))
     (setf (aref new 0) n)
     (setf (aref new 1) (aref old 1))
     (setf (aref new 2) (aref old 2))
     (setq indx 2 ncells (+ 2 (aref old 0)))
     a    (if (> (setq indx (1+ indx)) ncells) (return t))
     (setq cell (aref old indx))
     b    (if (null cell) (go a))
     (setq i (+ 3 (rem (hasher (car (setq item (car cell)))) n)))
     (if (setq y (aref new i))
	 (nconc y (ncons item))
	 (setf (aref new i) (ncons item)))
     (setq cell (cdr cell))
     (go b)))

(defun dimcheck (ary sub fixpp)
  (do ((x sub (cdr x))
       (ret t)
       (y (cdr (arraydims (mget ary 'array))) (cdr y)))
      ((null y)
       (if x (merror "Array ~:M has dimensions ~:M, but was called with ~:M"
		     ary `((mlist)
			   ,.(mapcar #'1-
			      (cdr (arraydims (mget ary 'array)))))
		     `((mlist) ,.sub))
	   ret))
    (cond ((or (null x) (and (eq (ml-typep (car x)) 'fixnum)
			     (or (< (car x) 0) (not (< (car x) (car y))))))
	   (setq y nil x (cons nil t)))
	  ((not (fixnump (car x)) )
	   (if fixpp (setq y nil x (cons nil t)) (setq ret nil))))))

(defun constlam (x &aux (lam x))
  (if aexprp
      `(,(car lam) ,(cadr lam) ,@(mbinding ((mparams (cadr lam)))
					   (mapcar #'meval (cddr lam))))

      lam))

(defmspec $define (l)
  (twoargcheck l)
  (setq l (cdr l))
  (meval `((mdefine)
	   ,(cond ((mquotep (car l)) (cadar l))
		  ((and (not (atom (car l)))
			(member (caaar l) '($ev $funmake $arraymake) :test #'eq))
		   (meval (car l)))
		  (t (disp2 (car l))))
	   ,(meval (cadr l)))))

(defun set-lineinfo (fnname lineinfo body)
  (cond ((and (consp lineinfo) (eq 'src (third lineinfo)))
	 (setf (cdddr lineinfo) (list fnname (first lineinfo)))
	 (setf (get fnname 'lineinfo) body))
	(t (remprop fnname 'lineinfo))))

(defmspec mdefine (l )
  (let ($use_fast_arrays) ;;for mdefine's we allow use the oldstyle hasharrays
    (twoargcheck l)
    (setq l (cdr l))
    (let ((fun (car l)) (body (cadr l)) args subs ary fnname mqdef redef)
      (cond ((or (atom fun)
		 (and (setq mqdef (eq (caar fun) 'mqapply))
		      (member 'array (cdar fun) :test #'eq)))
	     (merror "Improper function definition:~%~M" fun))
	    (mqdef (if (or (atom (cadr fun))
			   (not (setq ary (member 'array (cdaadr fun) :test #'eq))))
		       (merror "Improper function definition:~%~M" (cadr fun)))
		   (setq subs (cdadr fun) args (cddr fun) fun (cadr fun)
			 fnname ($verbify (caar fun)))
		   (if (and (not (mgetl fnname '(hashar array)))
			    (get fnname 'specsimp))
		       (mtell "Warning - you are redefining the Maxima ~
			    subscripted function ~:M.~%"
			      fnname)))
	    ((prog2 (setq fnname ($verbify (caar fun)))
		 (or (mopp fnname) (member fnname '($all $allbut $%) :test #'eq)))
	     (merror "Improper function name: ~:@M" fnname))
	    ((setq ary (member 'array (cdar fun) :test #'eq)) (setq subs (cdr fun)))
	    (t (setq args (cdr fun) redef (mredef-check fnname))))
      (if (not ary) (remove1 (ncons fnname) 'mmacro t $macros t))
      (mdefchk fnname (or args (and (not mqdef) subs)) ary mqdef)
      (if (not (eq fnname (caar fun))) (rplaca (car fun) fnname))
      (cond ((not ary) (if (and evp (member fnname (car loclist) :test #'eq))
			   (mputprop fnname t 'local-fun)
			   (remove-transl-fun-props fnname))
	     (add2lnc (cons (ncons fnname) args) $functions)
	     (set-lineinfo fnname (cadar fun) body)
	     (mputprop fnname (mdefine1 args body) 'mexpr)
	     (if $translate (translate-function fnname)))
	    ((prog2 (add2lnc fnname $arrays)
		 (setq ary (mgetl fnname '(hashar array)))
	       (remove-transl-array-fun-props fnname))
	     (when (mfilep (cadr ary))
	       (i-$unstore (ncons fnname))
	       (setq ary (mgetl fnname '(hashar array))))
	     (if (not (= (if (eq (car ary) 'hashar)
			     (aref (symbol-array (cadr ary)) 2)
			     (length (cdr (arraydims (cadr ary)))))
			 (length subs)))
		 (merror "Array ~:M already defined with different dimensions"
			 fnname))
	     (mdefarray fnname subs args body mqdef))
	    (t (mputprop fnname (setq ary (gensym)) 'hashar)
	       (*array ary t 7)
	       (store (afuncall ary 0) 4)
	       (store (afuncall ary 1) 0)
	       (store (afuncall ary 2) (length subs))
	       (mdefarray fnname subs args body mqdef)))
      (cons '(mdefine simp) (copy-list l)))))

;; Checks to see if a user is clobbering the name of a system function.
;; Prints a warning and returns T if he is, and NIL if he isn't.
(defun mredef-check (fnname)
  (cond ((and (not (mget fnname 'mexpr))
	      (or (and (or (get fnname 'autoload)
			   (getl-lm-fcn-prop fnname '(subr fsubr lsubr))
			   (get fnname 'mfexpr*s))
		       (not (get fnname 'translated)))
		  (mopp fnname)))
	 (princ "Warning - you are redefining the Maxima ")
	 (if (getl fnname '(verb operators))
	     (princ "command ") (princ "function "))
	 (princ (print-invert-case (stripdollar fnname)))
	 (terpri)
	 t)))

(defun mdefarray (fun subs args body mqdef)
  (cond ((and  (boundp fun) (hash-table-p fun))
	 (error "~a is already a hash table.  Make it a function first" fun)))
  (cond ((and (null args) (not mqdef)) (mputprop fun (mdefine1 subs body) 'aexpr))
	((null (dolist (u subs)
		 (if (not (or (consp u) ($constantp u) (char= (getcharn u 1) #\&)))
		     (return t))))
	 (arrstore (cons (ncons fun) subs) (mdefine1 args body)))
	(t (mdefchk fun subs t nil)
	   (mputprop fun (mdefine1 subs (mdefine1 args body)) 'aexpr))))

(defmfun mspecfunp (fun)
  (and (or (getl-fun fun '(fsubr fexpr macro))
	   (getl fun '(mfexpr* mfexpr*s))
	   (and $transrun (get fun 'translated-mmacro))
	   (mget fun 'mmacro))
       (not (get fun 'evok))))

(defun mdefine1 (args body)
  (if fundefsimp
      (let ((sbody (simplify body)))
	(when (and (not (atom body)) (not (atom sbody)))
	  (rplaca body (car sbody)) (rplacd body (cdr sbody)))))
  (list '(lambda) (cons '(mlist) args) body))

(defun mdefchk (fun args ary mqdef)
  (do ((l args (cdr l)) (mfex) (mlex))
      ((null l) (and mfex (not mqdef) (mputprop fun mfex 'mfexprp))
       (and mlex (not mqdef) (mputprop fun mlex 'mlexprp)))
    (if (not (or (mdefparam (car l))
		 (and (or (not ary) mqdef)
		      (or (and mfexprp (mquotep (car l))
			       (mdefparam (cadar l)) (setq mfex t))
			  (and (mdeflistp l)
			       (or (mdefparam (cadar l))
				   (and mfexprp (mquotep (cadar l))
					(mdefparam (cadr (cadar l)))
					(setq mfex t)))
			       (setq mlex t))))))
	(merror "Improper parameter in function definition for ~:M:~%~M" fun (car l)))))

(defun mdefparam (x)
  (and (atom x) (not (maxima-constantp x)) (not (char= (getcharn x 1) #\&))))

(defun mdeflistp (l)
  (and (null (cdr l)) ($listp (car l)) (cdar l) (null (cddar l))))

(defmfun mopp (fun)
  (and (not (eq fun 'mqapply))
       (or (mopp1 fun)
	   (and (get fun 'operators) (not (rulechk fun))
		(not (member fun rulefcnl :test #'eq)) (not (get fun 'opers))))))

(defmfun mopp1 (fun)
  (and (setq fun (get fun 'op)) (not (member fun (cdr $props) :test #'eq))))

;; maybe should have a separate version, or a macro..
(defun mapply (a b c)
  (mapply1 a b c nil))

(defmspec $apply (l)
  (twoargcheck l)
  (let ((fun (meval (cadr l))) (arg (meval (caddr l))))
    (if (not ($listp arg))
	(merror "Attempt to apply ~:M to ~M~
		 ~%Second argument to `apply' must be a list."
		fun arg))
    (autoldchk (setq fun (getopr fun)))
    (mapply1 fun (cdr arg) (cadr l) l)))

(defun autoldchk (fun)
  (if (and (symbolp fun)
	   (get fun 'autoload)
	   (not (or (fboundp fun) (mfboundp fun))))
      (load-function fun t)))

(defmspec $dispfun (l)
  (setq l (cdr l))
  (cond ((or (cdr l) (not (eq (car l) '$all))) (dispfun1 l nil nil))
	(t
	 `((mlist simp)
	   ,@(apply #'append
		    (list (cdr (dispfun1 (cdr $functions) t nil))
			  (cdr (dispfun1
				(mapcan #'(lambda (x) (if (mget x 'aexpr) (ncons x)))
					(cdr $arrays)) nil t))
			  (cdr (dispfun1 (cdr $macros) t nil))))))))

(defun dispfun1 (l flag maexprp)
  `((mlist simp)
    ,@(loop for fun in l collect
	    (cadr ($ldisp (consfundef (if flag (caar fun) fun) maexprp nil))))))

(defmspec $fundef (x)
  (consfundef (fexprcheck x) nil nil))

(defun consfundef (x maexprp stringp)
  (prog (arryp name fun)
     (setq arryp (and (not (atom x)) (not (eq (caar x) 'mqapply)) (member 'array (cdar x) :test #'eq)))
     (cond ((atom x) (setq name ($verbify x)
			   fun (or (and (not maexprp) (mgetl name '(mexpr mmacro)))
				   (mgetl name '(aexpr)))))
	   (arryp (setq fun (meval1 (setq name (cons (list ($verbify (caar x)) 'array) (cdr x)))))
		  (if (or (atom fun) (not (eq (caar fun) 'lambda))) (setq fun nil))))
     (cond ((not fun) (cond (stringp (return x)) ((member 'edit state-pdl :test #'eq) (terpri)))
	    (merror "~:M is not the name of a user function." x))
	   ((and (not arryp) (mfilep (cadr fun)))
	    (setq fun (list (car fun) (dskget (cadadr fun) (car (cddadr fun)) (car fun) nil)))))
     (return
       (cons (if (eq (car fun) 'mmacro) '(mdefmacro simp) '(mdefine simp))
	     (cond (arryp (cons (cons '(mqapply) (cons name (cdadr fun))) (cddr fun)))
		   (t (funcall #'(lambda (body)
				   (cond ((and (eq (car fun) 'aexpr) (not (atom body))
					       (eq (caar body) 'lambda))
					  (list (cons '(mqapply) (cons (cons (cons name '(array))
									     (cdr (cadadr fun)))
								       (cdadr body)))
						(caddr body)))
					 (t (list (cons (cons name (if (eq (car fun) 'aexpr) '(array)))
							(cdr (cadadr fun)))
						  body))))
			       (caddr (cadr fun)))))))))


(defmfun $funmake (fun args)
  (if (not (or (symbolp fun) ($subvarp fun)
	       (and (not (atom fun)) (eq (caar fun) 'lambda))))
      (merror "Bad first argument to `funmake': ~M" fun))
  (if (not ($listp args)) (merror "Bad second argument to `funmake': ~M" args))
  (mcons-op-args (getopr fun) (cdr args)))

(defmfun mcons-op-args (op args)
  (if (symbolp op) (cons (ncons op) args) (list* '(mqapply) op args)))

(defmfun optionp (x)
  (and (boundp x) (not (member x (cdr $values) :test #'eq)) (not (member x (cdr $labels) :test #'eq))))

(defmspec mcond (form)
  (setq form (cdr form))
  (do ((u form (cddr u)) (v))
      ((null u) nil)
    (cond ((eq (setq v (mevalp (car u))) t) (return (meval (cadr u))))
	  (v (return (list* '(mcond) v (mapcar #'meval-atoms (cdr u))))))))

(defun meval-atoms (form)
  (cond ((atom form) (meval1 form))
	((eq (caar form) 'mquote) (cadr form))
	((and (or (getl-fun (caar form) '(fsubr fexpr))
		  (getl (caar form) '(mfexpr* mfexpr*s)))
	      (not (member (caar form) '(mcond mand mor mnot mprogn mdo mdoin) :test #'eq)))
	 form)
	(t (recur-apply #'meval-atoms form))))

(defmspec mdo (form)
  (setq form (cdr form))
  (funcall #'(lambda (mdop var next test do)
	       (setq next (or (cadddr form) (list '(mplus) (or (caddr form) 1) var))
		     test (list '(mor)
				(cond ((null (car (cddddr form))) nil)
				      (t (list (if (mnegp ($numfactor (simplify (caddr form))))
						   '(mlessp)
						   '(mgreaterp))
					       var (car (cddddr form)))))
				(cadr (cddddr form)))
		     do (caddr (cddddr form)))
	       (mbinding ((ncons var)
			  (ncons (if (null (cadr form)) 1 (meval (cadr form)))))
			 (do ((val) (bindl bindlist))
			     ((is test) '$done)
			   (cond ((null (setq val (catch 'mprog (prog2 (meval do) nil))))
				  (mset var (meval next)))
				 ((atom val) (merror "`go' not in `block':~%~M" val))
				 ((not (eq bindl bindlist))
				  (merror "Illegal `return':~%~M" (car val)))
				 (t (return (car val)))))))
	   t (or (car form) 'mdo) nil nil nil))

(defmspec mdoin (form)
  (setq form (cdr form))
  (funcall #'(lambda  (mdop var set test action)
	       (setq set (if ($atom (setq set (format1 (meval (cadr form)))))
			     (merror "Atomic 'in' argument to `do' statement:~%~M" set)
			     (margs set))
		     test (list '(mor)
				(if (car (cddddr form))
				    (list '(mgreaterp) var (car (cddddr form))))
				(cadr (cddddr form)))
		     action (caddr (cddddr form)))
	       (cond ((atom set) '$done)
		     (t (mbinding ((ncons var) (ncons (car set)))
				  (do ((val) (bindl bindlist))
				      ((or (atom set) (is test))
				       '$done)
				    (cond ((null (setq val (catch 'mprog (prog2 (meval action) nil))))
					   (if (setq set (cdr set)) (mset var (car set))))
					  ((atom val) (merror "`go' not in `block':~%~M" val))
					  ((not (eq bindl bindlist))
					   (merror "Illegal `return':~%~M" (car val)))
					  (t (return (car val)))))))))
	   t (or (car form) 'mdo) nil nil nil))

(defmspec mprog (prog)
  (setq prog (cdr prog))
  (let (vars vals (mlocp t))
    (if ($listp (car prog)) (setq vars (cdar prog) prog (cdr prog)))
    (setq loclist (cons nil loclist))
    (do ((l vars (cdr l))) ((null l) (setq vals vars))
      (if (not (atom (car l))) (return (setq vals t))))
    (if (eq vals t)
	(setq vals (mapcar #'(lambda (v)
			       (cond ((atom v) v)
				     ((eq (caar v) 'msetq) (meval (caddr v)))
				     (t (merror
					 "Improper form in `block' variable list: ~M"
					 v))))
			   vars)
	      vars (mapcar #'(lambda (v) (if (atom v) v (cadr v))) vars)))
    (mbinding (vars vals)
	      (do ((prog prog (cdr prog)) (mprogp prog)
		   (bindl bindlist) (val '$done) (retp) (x) ($%% '$%%))
		  ((null prog) (munlocal) val)
		(cond ((atom (car prog))
		       (if (null (cdr prog))
			   (setq retp t val (meval (car prog)))))
		      ((null (setq x (catch 'mprog
				       (prog2 (setq val (setq $%% (meval (car prog))))
					   nil)))))
		      ((not (eq bindl bindlist))
		       (if (not (atom x))
			   (merror "Illegal `return':~%~M" (car x))
			   (merror "Illegal `go':~%~M" x)))
		      ((not (atom x)) (setq retp t val (car x)))
		      ((not (setq prog (member x mprogp :test #'equal)))
		       (merror "No such tag as ~:M" x)))
		(if retp (setq prog '(nil)))))))

(defmfun mreturn (x)
  (if (and (not mprogp) (not mdop))
      (merror "`return' not in `block':~%~M" x))
  (throw 'mprog (ncons x)))

(defmspec mgo (tag)
  (setq tag (fexprcheck tag))
  (cond ((not mprogp) (merror "`go' not in `block':~%~M" tag))
	((atom tag) (throw 'mprog tag))
	(t (merror "Argument to `go' not atomic:~%~M" tag))))

(defmspec $subvar (l)
  (setq l (cdr l))
  (if (null l)
      (wna-err '$subvar))
  (meval (cons '(mqapply array) l)))

(defmfun rat (x y)
  `((rat simp) ,x ,y))

(defmfun $exp (x)
  `((mexpt) $%e ,x))

(defmfun $sqrt (x)
  `((%sqrt) ,x))

(defmfun add2lnc (item llist)
  (when (not (memalike item (if ($listp llist) (cdr llist) llist)))
    (if (not (atom item))
	(setf llist (delete (assoc (car item) llist :test #'equal) llist :count 1 :test #'equal)))
    (nconc llist (ncons item))))

(defmfun bigfloatm* (bf)
  (if (not (member 'simp (cdar bf) :test #'eq))
      (setq bf (cons (list* (caar bf) 'simp (cdar bf)) (cdr bf))))
  (if $float ($float bf) bf))

(defmfun $allbut n
  (cons '($allbut) (listify n)))

(defmfun mfilep (x)
  (and (not (atom x)) (not (atom (car x))) (eq (caar x) 'mfile)))

(defquote dsksetq (&rest l)
  (let ((dsksetp t))
    (mset (car l) (eval (cadr l)))))

(defmfun dskrat (x)
  (orderpointer (caddar x))
  (mapc #'(lambda (a b) (dskrat-subst a b (cddddr (car x))) ; for TAYLOR forms
		  (dskrat-subst a b (cdr x)))
	genvar (cadddr (car x)))
  (rplaca (cdddar x) genvar)
  (if (member 'trunc (car x) :test #'eq)
      (srconvert x) x))			; temporary

(defun dskrat-subst (x y z)
  (cond ((atom z) z)
	(t (if (eq y (car z)) (rplaca z x) (dskrat-subst x y (car z)))
	   (dskrat-subst x y (cdr z))
	   z)))

(defmfun |''MAKE-FUN| (noun-name x)
  (let (($numer t) ($float t))
    (simplifya (list (ncons noun-name) (resimplify x)) t)))

(defmacro |''MAKE| (fun noun)
  `(defmfun ,fun (x) (|''MAKE-FUN| ',noun x)))

(|''MAKE| $log %log)
(|''MAKE| $sin %sin) (|''MAKE| $cos %cos) (|''MAKE| $tan %tan)
(|''MAKE| $cot %cot) (|''MAKE| $sec %sec) (|''MAKE| $csc %csc)
(|''MAKE| $sinh %sinh) (|''MAKE| $cosh %cosh) (|''MAKE| $tanh %tanh)
(|''MAKE| $coth %coth) (|''MAKE| $sech %sech) (|''MAKE| $csch %csch)
(|''MAKE| $asin %asin) (|''MAKE| $acos %acos) (|''MAKE| $atan %atan)
(|''MAKE| $acot %acot) (|''MAKE| $asec %asec) (|''MAKE| $acsc %acsc)
(|''MAKE| $asinh %asinh) (|''MAKE| $acosh %acosh) (|''MAKE| $atanh %atanh)
(|''MAKE| $acoth %acoth) (|''MAKE| $asech %asech) (|''MAKE| $acsch %acsch)
(|''MAKE| $gamma %gamma)

(defmfun $binomial (x y)
  (let (($numer t) ($float t))
    (simplify (list '(%binomial) x y))))

;; evfun properties
(mapc #'(lambda (x) (putprop x t 'evfun))
      '($radcan $factor $ratsimp $trigexpand $trigreduce $logcontract
	$rootscontract $bfloat $ratexpand $fullratsimp $rectform $polarform))

;; evflag properties
(mapc #'(lambda (x) (putprop x t 'evflag))
      '($exponentialize $%emode $demoivre $logexpand $logarc $lognumer
	$radexpand $keepfloat $listarith $float $ratsimpexpons $ratmx
	$simp $simpsum $simpproduct $algebraic $ratalgdenom $factorflag $ratfac
	$infeval $%enumer $programmode $lognegint $logabs $letrat
	$halfangles $exptisolate $isolate_wrt_times $sumexpand
	$cauchysum $numer_pbranch $m1pbranch $dotscrules $trigexpand))

(mdefprop $%e     2.71828182845904523536 $numer) ; (EXP 1) [wrong in ITS-MACLISP]
(mdefprop $%pi    3.14159265358979323846 $numer) ; (ATAN 0 -1)
(mdefprop $%phi   1.61803398874989484820 $numer) ; (1+sqrt(5))/2
(mdefprop $%gamma 0.5772156649015328606  $numer) ; Euler's constant

(mdefprop $herald_package (nil $transload t) $props)
(mdefprop $load_package (nil $transload t) $props)

(defprop bigfloat bigfloatm* mfexpr*)
(defprop lambda constlam mfexpr*)
(defprop quote cadr mfexpr*)		; Needed by MATCOM/MATRUN.

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (setq  *read-base* *old-read-base*))
