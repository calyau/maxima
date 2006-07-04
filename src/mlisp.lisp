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
 (special mspeclist mproplist bindlist loclist bvars nounsflag putl
	  noitems derivflag derivlist mprogp mdop evp aexprp mlocp $labels
	  $values $functions $arrays $rules $gradefs $dependencies $aliases
	  $myoptions $props genvar $maxposex $maxnegex $expop $expon
	  $float $numer aryp msump state-pdl evarrp $setval nounl
	  $setcheckbreak $refcheck *mdebug* refchkl baktrcl maplp
	  $norepeat $detout $doallmxops $doscmxops opers factlist opexprp
	  $translate $transrun $maperror outargs1 outargs2 fmaplvl mopl
	  $powerdisp $subscrmap $dispflag $optionset dsksetp fexprerrp
	  $features alphabet $%enumer $infeval $savedef $%% %e-val
	  $mapprint featurel outfiles fundefsimp mfexprp transp
	  sfindex mspeclist2 envlist $macros linel $ratfac $ratwtlvl
	  $operators noevalargs $piece $partswitch *gcdl*
	  scanmapp *builtin-$props*))

(declare-top (unspecial args))

;;#-cl (proclaim ' (GENPREFIX %LS))
;;#-cl(proclaim '	 (*EXPR RATF $FLOAT))
;;#-cl(proclaim '	 (*LEXPR MAP1 MMAPCAR FMAPL1 OUTERMAP1 $INPART LINEL $DIFF $INTEGRATE
;;		 $LDISP $RATVARS $RATWEIGHT))

;;(declare-top	 (FIXNUM N I J NNEED NGIVEN NCELLS NITEMS LISPSUB INDX FMAPLVL EVFLG 
;;			 LINEL SFINDEX #-cl (HASHER)))
;;  NNEED to be flushed

(setq mspeclist nil bindlist nil loclist nil mproplist nil $%enumer nil
      $float nil nounl nil $refcheck nil scanmapp nil maplp nil
      mprogp nil evp nil mdop nil mlocp nil putl nil
      $subscrmap nil $translate nil $transrun t $savedef t aexprp nil
      $maperror t fmaplvl 0 $optionset nil 
      $setcheckbreak nil dsksetp nil aryp nil msump nil evarrp nil
      $infeval nil factlist nil $mapprint t fundefsimp nil
      mfexprp t nounsflag nil opexprp nil ;$OPERATORS NIL
      sfindex 1 mspeclist2 nil envlist nil transp nil noevalargs nil
      $piece '$piece $setval '$setval fexprerrp nil rulefcnl nil
      featurel (purcopy '($integer $noninteger $even $odd
			  $rational $irrational $real $imaginary
			  $complex $analytic $increasing $decreasing
			  $oddfun $evenfun $posfun $commutative $lassociative
			  $rassociative $symmetric $antisymmetric))
      $features (cons '(mlist simp) (append featurel nil)))

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(defvar munbound '|#####|)

;; The most negative fixnum.  Sign bit is on and all other bits are zero.
;; Assumes two's complement arithmetic.
(defvar fixunbound most-negative-fixnum)

;; The PDP10 floating point representation is:
;; 1 bit sign, 8 bit exponent, 27 bit mantissa
;; If positive, exponent is excess 128.  If negative, exponent is one's
;; complement of excess 128.
;; If positive normalized, mantissa is between 2^26 and 2^27-1.  If negative,
;; two's complement.  See RAT;FLOAT for more details.

;; I think this is supposed to be the most negative flonum.  It's close,
;; but not quite.  The smallest is (FSC (ROT 3 -1) 0).

;;#+PDP10
;;(DEFVAR FLOUNBOUND (FSC (f- 2 (LSH -1 -1)) 0))

;; H6180 floating point representation is:
;; 8 bit exponent, 1 bit sign, 27 bit mantissa
;; The 8 bit exponent is viewed as two's complement, between 2^7-1 and -2^7.
;; The 28 bit mantissa is viewed as two's complement, between -1 and 1-2^-27.
;; The most negative flonum is given below.  The most positive flonum
;; is its logical complement.

;;#+H6180
;;(DEFVAR FLOUNBOUND (FSC (LOGIOR (LSH 1 35.) (LSH 1 27.)) 0))

;; Too bad there's no general way of getting the most negative flonum in
;; a relatively machine-independent manner.

;;#+LISPM
;;(DEFVAR FLOUNBOUND '*FLOUNBOUND-DOESNT-MATTER-ANYWAY*)

(defvar flounbound most-negative-double-float)

(defmvar munbindp nil
  "Used for safely `munbind'ing incorrectly-bound variables."
  no-reset)
(defmvar $setcheck nil)

(mapc #'(lambda (x) (set x (ncons '(mlist simp))))
      '($values $functions $macros $arrays $myoptions $rules $props))

(defmfun mapply1 (fn args fnname form)
  (declare (special aryp) (object fn))
  (cond		   ;((AND $OPERATORS (MNUMP FN)) (MUL2 FN (CAR ARGS)))
    ((atom fn) 
     (cond
					;#-cl				; #+(or cl nil)
       ;;	 ((and (symbolp fn) (fboundp fn)
       ;;	       (not (consp symbol-function fn)))
       ;;	  (apply  fn args))
       ((atom fn) 
	(cond
	  ((functionp fn)
	   (apply fn args))
	  
	  ;;better be a macro or an array.
	  ((fboundp fn)
	   (if (macro-function fn)
	       (progn (merror "~M is a lisp level macro and cannot be applied at maxima level" fn) (eval (cons fn  args)))
	       (mapply1 (symbol-function fn) args fn form)))
	   
	  ((symbol-array fn)
	   (mapply1 (symbol-array fn) args fn form))
	  (t
	   (setq fn (getopr fn)) (badfunchk fnname fn nil)
	   (let ((noevalargs t)) (meval (cons (ncons fn) args)))))
	)))
    ((functionp fn)
     (apply fn args))
    ((eq (car fn) 'lambda) (apply (coerce fn 'function) args))
					;#-cl
    ;;       ((EQ (CAR FN) 'LAMBDA) (APPLY FN ARGS))
    ;;       #+(and Lispm (not cl))
    ;;       ((memq (CAR FN)
    ;;	      '(NAMED-LAMBDA si:digested-lambda)) (APPLY FN ARGS))
    ;;       #-cl
    ;;       ((AND (EQ (CAAR FN) 'MFILE)
    ;;	     (SETQ FN (EVAL (DSKGET (CADR FN) (CADDR FN) 'VALUE NIL)))
    ;;	     NIL))
    ((eq (caar fn) 'lambda) (mlambda fn args fnname t form))
    ((eq (caar fn) 'mquote) (cons (cdr fn) args))
    ((and aryp (memq (caar fn) '(mlist $matrix)))
     (if (not (or (= (length args) 1)
		  (and (eq (caar fn) '$matrix) (= (length args) 2))))
	 (merror "Wrong number of indices:~%~M" (cons '(mlist) args)))
     (if (memq 0 args)
       (merror "No such ~M element: ~M~%" (if (eq (caar fn) 'mlist) "list" "matrix") `((mlist) ,@args)))
     (do ((args1 args (cdr args1)))
	 ((null args1) (let (($piece $piece) ($partswitch 'mapply))
			 (apply #'$inpart (cons fn args))))
       (unless (fixnump (car args1))
	 (if evarrp (throw 'evarrp 'notexist))
	 (merror "Subscript must be an integer:~%~M" (car args1)))))
    (aryp (cons '(mqapply array) (cons fn args)))
    ((memq 'array (cdar fn)) (cons '(mqapply) (cons fn args)))
    (t (badfunchk fnname fn t))))

;;#-NIL
;; the last argument to mapply1 for the lineinfo is not correct here..
(defmfun mcall n (mapply1 (arg 1) (listify (f- 1 n)) (arg 1) nil))

;;#+NIL
;;(DEFMFUN MCALL (FN &REST ARGS)
;;  (MAPPLY1 FN ARGS FN nil))

;;#-NIL
;;(declare-top (MAPEX T))  ; To avoid the overuse of pdls in this highly
;;                         ; recursive part of the evaluator.

(defun mevalargs (args)
  (cond (noevalargs (setq noevalargs nil) args) (t (mapcar #'meval args))))

;;Function Call stack each element is
;; (fname . bindlist) where bindlist was the value at time of entry.
;; So you can use this to compute what the bindings were at any
;; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

;;#-NIL 
;;(declare-top (MAPEX NIL))

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
    ;;    (MBINDING (PARAMS ARGS FNNAME)
    ;;	      (PROG1 (LET ((AEXPRP (AND AEXPRP (NOT (ATOM (CADDR FN)))
    ;;					(EQ (CAAR (CADDR FN)) 'LAMBDA))))
    ;;		       (COND ((NULL (CDDR FN))
    ;;			      (MERROR "No `lambda' body present"))
    ;;			     ((CDDDR FN) (MEVALN (CDDR FN)))
    ;;			     (T (MEVAL (CADDR FN)))))
    ;;		     ;; the MUNLOCAL should be unwind-protected,  I can't
    ;;		     ;; see how I can work it into the MBINDING macro
    ;;		     ;; at this time. Too bad for the losers who use it.
    ;;		     (MUNLOCAL)))
    ;; we expand the above, and also add stuff for the call stack.
    ;; we also move munlocal into the unwind protect.
    (let (finish2033 (finish2032 params) (ar *mlambda-call-stack*))
      (declare (type (vector t) ar))
      (unwind-protect
	   (progn
	     (or (f> (array-total-size ar) (f+ (fill-pointer ar) 10))
		 (adjust-array ar (f+ (array-total-size ar) 50)
			       :fill-pointer (fill-pointer ar)))
	     (vector-push bindlist ar)
	     ;; rather than pushing all on baktrcl it might be good
	     ;; to make a *last-form* global that is set in meval1
	     ;; and is pushed here.  
					;(vector-push baktrcl ar)
	     (vector-push form ar)
	     (vector-push params ar)
	     (vector-push args ar)
	     (vector-push fnname ar)
	     (mbind finish2032 args fnname)
	     (setq finish2033 t)
	     (prog1 (let ((aexprp (and aexprp (not (atom (caddr fn)))
				       (eq (caar (caddr fn)) 'lambda))))
		      (cond
			((null (cddr fn)) (merror "No `lambda' body present"))
			((cdddr fn) (mevaln (cddr fn)))
			(t (meval (caddr fn)))))
	       nil ))
	(if finish2033 (progn (incf (fill-pointer *mlambda-call-stack*) -5)
			      (munlocal)
			      (munbind finish2032)
			      ))))

    ))


(defmspec mprogn (form) (mevaln (cdr form)))

(defmfun mevaln (l) ;; called in a few places externally.
  (do ((body l (cdr body)) ($%% '$%%)) ((null (cdr body)) (meval (car body)))
    (setq $%% (meval (car body)))))

;;(DEFMSPEC DOLIST (FORM)  ; temporary
;; (SETF (CAR FORM) '(MPROGN)) (MEVAL FORM))

(defun mqapply1 (form)
  (declare (special aryp))
  (destructuring-let (((fn . argl) (cdr form)) (aexprp))
    (cond ((not (mquotep fn)) (setq fn (meval fn))))
    (cond ((atom fn) (meval (cons (cons fn aryp) argl)))
	  ((eq (caar fn) 'lambda)
	   (cond (aryp (merror "Improper array call"))
		 (t (mlambda fn argl (cadr form) noevalargs form))))
	  (t (mapply1 fn (mevalargs argl) (cadr form) form)))))

(defmfun meval (form) (simplifya (meval1 form) nil))
;;temporary hack to see what's going on:
(defmfun safe-mgetl (atom inds) (and (symbolp atom)
				     (let ((props (get atom 'mprops))) (and props (getl props inds)))))
(defmfun safe-mget (atom inds) (and (symbolp atom)
				    (let ((props (get atom 'mprops))) (and props (getf (cdr props) inds)))))

(defvar *last-meval1-form* nil)

(defmfun meval1 (form)
  (declare (special  nounl *break-points* *break-step*))
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
	    (when (and $refcheck (memq form (cdr $values))
		       (not (memq form refchkl)))
	      (setq refchkl (cons form refchkl))
	      (mtell "~:M has value.~%" form))
	    (return val)))
	((or (and (atom (car form))
		  (setq form (cons (ncons (car form)) (cdr form))))
	     (atom (caar form)))
	 (let ((baktrcl baktrcl) transp) 
	   (prog (u aryp)
	      (declare (special aryp))
	      ;;(COND ((EQ *mDEBUG* '$ALL) (SETQ BAKTRCL (CONS FORM BAKTRCL))))
	      (setq *last-meval1-form* form)
	      (setq aryp (memq 'array (cdar form))) 
	      (cond ((and (not opexprp) (not aryp) 
			  (memq (caar form) '(mplus mtimes mexpt mnctimes)))
		     (go c))
		    ;; dont bother pushing mplus and friends on baktrcl
		    ;; should maybe even go below aryp.
		    ((and *mdebug*
			  (progn
					;(SETQ BAKTRCL (CONS FORM BAKTRCL))
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
				    (not (or (getl-fun (caar form)
						       '(subr fsubr lsubr))
					     (safe-getl (caar form)
							'(mfexpr* mfexpr*s))))
				    (prog2 ($verbify (caar form))
					(safe-getl (caar form) '(noun))))
			       (and (not aryp) $transrun
				    (setq transp
					  (or (safe-mgetl (caar form) '(t-mfexpr))
					      (safe-getl (caar form)
							 '(translated-mmacro)))))
			       (and (not aryp)
				    (setq u
					  (or (safe-mget (caar form) 'trace)
					      (and $transrun
						   (safe-get (caar form) 'translated)
						   (not (safe-mget (caar form)
								   'local-fun))
						   (setq transp t) (caar form))))
				    (getl-fun u '(expr subr lsubr)))
			       (cond (aryp (safe-mgetl (caar form) '(hashar array)))
				     ((safe-mgetl (caar form) '(mexpr mmacro)))
				     ((safe-mgetl (caar form) '(t-mfexpr)))
				     (t (or (safe-getl (caar form)
						       '(mfexpr* mfexpr*s))
					    (getl-fun (caar form)
						      '(subr fsubr expr fexpr macro
							lsubr)))))))
	      (cond ((null u) (go b))
		    ((and (memq (car u) '(mexpr mmacro)) (mfilep (cadr u)))
		     (setq u (list (car u)
				   (dskget (cadadr u) (car (cddadr u))
					   (car u) nil))))
		    ((and (memq (car u) '(array hashar)) (mfilep (cadr u)))
		     (i-$unstore (ncons (caar form)))
		     (return (meval1 form))))
	      (return 
		(cond ((eq (car u) 'hashar) 
		       (harrfind (cons (car form) (mevalargs (cdr form)))))
		      ((memq (car u) '(fexpr fsubr))
		       (if fexprerrp
			   (merror "Attempt to call ~A ~A from Maxima level.~
				 ~%Send a bug note."
				   (car u) (caar form)))
		       (setq noevalargs nil) (apply (caar form) (cdr form)))
		      ((or (and (eq (car u) 'subr)
				(prog2 (margchk (caar form) (cdr form)) t))
			   (eq (car u) 'lsubr))
		       ;;		       ((MEMQ (CAR U) '(SUBR LSUBR))
		       ;;			(MARGCHK (CAAR FORM) (CDR FORM)))
		       (apply (caar form) (mevalargs (cdr form))))

		      ((eq (car u) 'noun)
		       ;;			(MARGCHK (CAAR FORM) (CDR FORM))
		       (cond ((or (memq (caar form) nounl) nounsflag)
			      (setq form (cons (cons (cadr u) (cdar form))
					       (cdr form)))
			      (go a))
			     (aryp (go b))
			     ((memq (caar form) '(%sum %product))
			      (setq u (do%sum (cdr form) (caar form))
				    noevalargs nil)
			      (cons (ncons (caar form)) u))
			     (t (meval2 (mevalargs (cdr form)) form))))
		      ((eq (car u) 'array)
		       (arrfind (cons (car form) (mevalargs (cdr form)))))
		      ((eq (car u) 'mexpr)
		       (mlambda (cadr u) (cdr form) (caar form) noevalargs form))
		      ((memq (car u) '(mmacro translated-mmacro))
		       (setq noevalargs nil)
		       (meval (mmacro-apply (cadr u) form)))
		      ((eq (car u) 'mfexpr*)
		       (setq noevalargs nil)  (apply (cadr u) (ncons form)))
		      ((eq (car u) 'macro)
		       (setq noevalargs nil)
		       (setq form (cons(caar form) (cdr form)))
		       ;;		     (setf (car form) (caar form) )
		       (eval form)
		       )
		      ;;		    #+Maclisp
		      ;;		    ((EQ (CAR U) 'MFEXPR*S)
		      ;;		     (SETQ NOEVALARGS NIL)
		      ;;		     ;; use macsyma Trace if you want to trace this call.
		      ;;		     (SUBRCALL T (CADR U) FORM))
		      ((eq (car u) 't-mfexpr) (apply (cadr u) (cdr form)))
		      (t (margchk (caar form) (cdr form))
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

;;old def. had some unsafe plist accesses.
;;(DEFMFUN MEVAL1 (FORM)
;;  (declare (special  nounl))
;;  (COND ((ATOM FORM)
;;	 (PROG (VAL)
;;	   (COND ((NOT (SYMBOLP FORM)) (RETURN FORM))
;;		 ((AND $NUMER (SETQ VAL (MGET FORM '$NUMER))
;;		       (OR (NOT (EQ FORM '$%E)) $%ENUMER))
;;		  (RETURN (MEVAL1 VAL)))
;;		 ((NOT (BOUNDP FORM))
;;		  (IF (GET FORM 'BINDTEST)
;;		      (MERROR "~:M unbound variable" FORM)
;;		      (RETURN FORM)))
;;		 ((MFILEP (SETQ VAL (SYMBOL-VALUE FORM)))
;;		  (SETQ VAL
;;			(EVAL (DSKGET (CADR VAL) (CADDR VAL) 'VALUE NIL)))))
;;	   (WHEN (AND $REFCHECK (MEMQ FORM (CDR $VALUES))
;;		      (NOT (MEMQ FORM REFCHKL)))
;;		 (SETQ REFCHKL (CONS FORM REFCHKL))
;;		 (MTELL "~:M has value.~%" FORM))
;;	   (RETURN VAL)))
;;	((OR (AND (ATOM (CAR FORM))
;;		  (SETQ FORM (CONS (NCONS (CAR FORM)) (CDR FORM))))
;;	     (ATOM (CAAR FORM)))
;;	 (LET ((BAKTRCL BAKTRCL) TRANSP) 
;;	   (PROG (U ARYP)
;;		 (declare (special aryp))
;;	     (COND ((EQ *mDEBUG* '$ALL) (SETQ BAKTRCL (CONS FORM BAKTRCL))))
;;	     (SETQ ARYP (MEMQ 'array (CDAR FORM))) 
;;	     (COND ((AND (NOT OPEXPRP) (NOT ARYP) 
;;			 (MEMQ (CAAR FORM) '(MPLUS MTIMES MEXPT MNCTIMES)))
;;		    (GO C))
;;		   ((AND $SUBSCRMAP ARYP
;;			 (DO ((X (MARGS FORM) (CDR X)))
;;			     ((OR (NULL X) (MXORLISTP (CAR X))) X)))
;;		    (SETQ NOEVALARGS NIL) (RETURN (SUBGEN FORM)))
;;		   ((EQ (CAAR FORM) 'MQAPPLY) (RETURN (MQAPPLY1 FORM))))
;;	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
;;	    A    (SETQ U (OR (GETL (CAAR FORM) '(NOUN))
;;			     (AND NOUNSFLAG (EQ (GETCHAR (CAAR FORM) 1) '%)
;;				  (NOT (OR (GETL-FUN (CAAR FORM)
;;						     '(SUBR FSUBR LSUBR))
;;					   (GETL (CAAR FORM)
;;						 '(MFEXPR* MFEXPR*S))))
;;				  (PROG2 ($VERBIFY (CAAR FORM))
;;					 (GETL (CAAR FORM) '(NOUN))))
;;			     (AND (NOT ARYP) $TRANSRUN
;;				  (SETQ TRANSP
;;					(OR (MGETL (CAAR FORM) '(T-MFEXPR))
;;					    (GETL (CAAR FORM)
;;						  '(TRANSLATED-MMACRO)))))
;;			     (AND (NOT ARYP)
;;				  (SETQ U
;;					(OR (MGET (CAAR FORM) 'TRACE)
;;					    (AND $TRANSRUN
;;						 (GET (CAAR FORM) 'TRANSLATED)
;;						 (NOT (MGET (CAAR FORM)
;;							    'LOCAL-FUN))
;;						 (SETQ TRANSP T) (CAAR FORM))))
;;				  (GETL-FUN U '(EXPR SUBR LSUBR)))
;;			     (COND (ARYP (MGETL (CAAR FORM) '(HASHAR ARRAY)))
;;				   ((MGETL (CAAR FORM) '(MEXPR MMACRO)))
;;				   ((MGETL (CAAR FORM) '(T-MFEXPR)))
;;				   (T (OR (GETL (CAAR FORM)
;;						'(MFEXPR* MFEXPR*S))
;;					  (GETL-FUN (CAAR FORM)
;;						    '(SUBR FSUBR EXPR FEXPR macro
;;							   LSUBR)))))))
;;#+cl     (cond ((eq (car u) 'macro) (show u) (setf (cadr u) (cdadr u))))
;;	     (COND ((NULL U) (GO B))
;;		   ((AND (MEMQ (CAR U) '(MEXPR MMACRO)) (MFILEP (CADR U)))
;;		    (SETQ U (LIST (CAR U)
;;				  (DSKGET (CADADR U) (CAR (CDDADR U))
;;					  (CAR U) NIL))))
;;		   ((AND (MEMQ (CAR U) '(ARRAY HASHAR)) (MFILEP (CADR U)))
;;		    (I-$UNSTORE (NCONS (CAAR FORM)))
;;		    (RETURN (MEVAL1 FORM))))
;;	     (RETURN 
;;	      (COND ((EQ (CAR U) 'HASHAR) 
;;		     (HARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
;;		    ((MEMQ (CAR U) '(FEXPR FSUBR))
;;		     (IF FEXPRERRP
;;			 (MERROR "Attempt to call ~A ~A from MACSYMA level.~
;;				 ~%Send a bug note."
;;				 (CAR U) (CAAR FORM)))
;;		     (SETQ NOEVALARGS NIL) (APPLY (CAAR FORM) (CDR FORM)))
;;		    ((OR (AND (EQ (CAR U) 'SUBR)
;;			      (PROG2 (MARGCHK (CAAR FORM) (CDR FORM)) T))
;;			 (EQ (CAR U) 'LSUBR))
;;		       ((MEMQ (CAR U) '(SUBR LSUBR))
;;			(MARGCHK (CAAR FORM) (CDR FORM)))
;;		     (APPLY (CAAR FORM) (MEVALARGS (CDR FORM))))
;;
;;		    ((EQ (CAR U) 'NOUN)
;;			(MARGCHK (CAAR FORM) (CDR FORM))
;;		     (COND ((OR (MEMQ (CAAR FORM) NOUNL) NOUNSFLAG)
;;			    (SETQ FORM (CONS (CONS (CADR U) (CDAR FORM))
;;					     (CDR FORM)))
;;			    (GO A))
;;			   (ARYP (GO B))
;;			   ((MEMQ (CAAR FORM) '(%SUM %PRODUCT))
;;			    (SETQ U (DO%SUM (CDR FORM) (CAAR FORM))
;;				  NOEVALARGS NIL)
;;			    (CONS (NCONS (CAAR FORM)) U))
;;			   (T (MEVAL2 (MEVALARGS (CDR FORM)) FORM))))
;;		    ((EQ (CAR U) 'array)
;;		     (ARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
;;		    ((EQ (CAR U) 'MEXPR)
;;		     (MLAMBDA (CADR U) (CDR FORM) (CAAR FORM) NOEVALARGS form))
;;		    ((MEMQ (CAR U) '(MMACRO TRANSLATED-MMACRO))
;;		     (SETQ NOEVALARGS NIL)
;;		     (MEVAL (MMACRO-APPLY (CADR U) FORM)))
;;		    ((EQ (CAR U) 'MFEXPR*)
;;		     (SETQ NOEVALARGS NIL)  (APPLY (CADR U) (NCONS FORM)))
;;		    #+cl
;;		    ((eq (car u) 'macro)
;;		     (setq noevalargs nil)
;;		     (setq form (cons(caar form) (cdr form)))
;;		     (setf (car form) (caar form) )
;;		      (eval form)
;;		     )
;;		    #+Maclisp
;;		    ((EQ (CAR U) 'MFEXPR*S)
;;		     (SETQ NOEVALARGS NIL)
;;		     ;; use macsyma Trace if you want to trace this call.
;;		     (SUBRCALL T (CADR U) FORM))
;;		    ((EQ (CAR U) 'T-MFEXPR) (APPLY (CADR U) (CDR FORM)))
;;		    (T (MARGCHK (CAAR FORM) (CDR FORM))
;;		       (APPLY (CADR U) (MEVALARGS (CDR FORM))))))
;;	    B   #+(OR PDP10 Multics Franz NIL cl)
;;	     (IF (AND (NOT ARYP) (LOAD-FUNCTION (CAAR FORM) T)) (GO A))
;;	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
;;	     (IF (SYMBOLP (CAAR FORM))
;;		 (SETQ U (BOUNDP (CAAR FORM)))
;;		 (RETURN (MEVAL1-EXTEND FORM)))
;;	    C   (COND ((OR (NULL U)
;;			   (AND (GET (CAAR FORM) 'OPERATORS) (NOT ARYP))
;;			   (EQ (CAAR FORM) (SETQ U (SYMBOL-VALUE (CAAR FORM)))))
;;		       (SETQ FORM (MEVAL2 (MEVALARGS (CDR FORM)) FORM))
;;		       (RETURN (OR (AND (MGET (CAAR FORM) 'ATVALUES)
;;					(AT1 FORM)) FORM)))
;;		      ((AND ARYP (GET (CAAR FORM) 'NONARRAY))
;;		       (RETURN (CONS (CONS (CAAR FORM) ARYP)
;;				     (MEVALARGS (CDR FORM)))))
;;		      ((ATOM U)
;;		       (BADFUNCHK (CAAR FORM) U NIL)
;;		       (SETQ FORM (CONS (CONS (GETOPR U) ARYP) (CDR FORM)))
;;		       (GO A))
;;		      ((EQ (CAAR U) 'LAMBDA)
;;		       (IF ARYP
;;			   (MERROR "Improper array call")
;;			   (RETURN (MLAMBDA U (CDR FORM)
;;					    (CAAR FORM) NOEVALARGS))))
;;		      (T (RETURN (MAPPLY1 U (MEVALARGS (CDR FORM))
;;					 (CAAR FORM))))))))
;;	(T (MAPPLY1 (CAAR FORM) (MEVALARGS (CDR FORM)) (CAAR FORM)))))
;;
;;; This function substitutes for the use of GETL on the
;;; EXPR, FEXPR, MACRO, SUBR, FSUBR, LSUBR, or ARRAY property.
;;; Note: This function used to be incompatible with GETL simply
;;;       to save two conses per function call in MEVAL, but considering
;;;       the amount of consing going on elsewere (e.g. the variable binding!)
;;;       and considering the #+LISPM grossness this introduced, it was
;;;       a bad idea. N.B. If you want efficiency in macsyma evaluation
;;;       use the Macsyma->lisp translator. -gjc
;;; DEFICIENCIES: Functions with some args &QUOTE and some args not
;;; will fail unless MEVAL is changed to call fexprs by (EVAL `(,FOO ,@L))
;;; instead of (APPLY FOO L). However: Officially everything uses 
;;; DEFMSPEC now, there are no fexprs.
;;;; **** This should be rewritten to use the new function FUNCTIONP. ****
;;
;;;;from the doe tape:
;;#+LISPM
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (PROG (FN RPROP ARGS-INFO)
;;    (SETQ RPROP
;;	  (AND (FBOUNDP SYM)
;;	       (SELECT (%DATA-TYPE (SETQ FN (SYMBOL-FUNCTION SYM)))
;;		 (DTP-SYMBOL (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;;		 (DTP-LIST (COND ((MEMQ (CAR FN) '(MACRO SUBST))
;;				  'MACRO FN)
;;				 ((EQ (CAR FN) 'NAMED-LAMBDA)
;;				  (IF (MEMQ '&QUOTE (CADDR FN))
;;				      'FEXPR 'EXPR))
;;				 ((EQ (CAR FN) 'LAMBDA)
;;				  (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;;				 (T (ERROR () "Unknown definition of ~S -- ~S"
;;					    SYM FN))))
;;		 (DTP-ARRAY-POINTER 'ARRAY)
;;		 ((DTP-FEF-POINTER DTP-U-ENTRY)
;;		  (SETQ ARGS-INFO (%ARGS-INFO FN))
;;		  (COND ((BIT-TEST (f+ %ARG-DESC-QUOTED-REST
;;				      %ARG-DESC-FEF-QUOTE-HAIR)
;;				   ARGS-INFO)
;;			 'FSUBR)
;;			((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;				 (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;			 'LSUBR)
;;			(T 'SUBR)))
;;		 (T (ERROR () "Unknown object in function cell of ~S -- ~S"
;;			    SYM FN)))))
;;    (RETURN (AND RPROP
;;		 (MEMQ RPROP PROPS)
;;		 (LIST RPROP FN)))))
;;
;;
;;
;;altered by wfs to fix the translated saved definitions so they work
;;#+LISPM 
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (PROG (FN RPROP ARGS-INFO)
;;	(cond  ((symbolp sym)
;;		(cond ((get sym 'translated)
;;		       (setq fn (getl sym props)))))
;;	       ;;case of compiled function.
;;	       ((functionp sym)(setq fn sym))  
;;	       (t nil))
;;	(SETQ RPROP
;;	      (AND (or fn (FBOUNDP SYM))
;;		   (let ((funct  (COND ((NULL FN)
;;					(SETQ FN (SYMBOL-FUNCTION SYM)))
;;				       (T FN))))
;;		     (COND ((ml-typep funct 'symbol)
;;			    (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;;			   ((ml-typep funct 'list)
;;			    (COND ((zl-MEMBER (CAR FN) '(MACRO SUBST special)) 'MACRO)
;;				  ((EQ (CAR FN) 'NAMED-LAMBDA)
;;				   (IF (MEMQ '&QUOTE (CADDR FN)) 'FEXPR 'EXPR))
;;				  ((eq (car fn) 'si:digested-lambda)
;;				   'subr)
;;				  ((AND (MEMQ (CAR FN) PROPS)
;;					(zl-MEMBER (CAADR FN) '(NAMED-LAMBDA ))) 
;;				   (RETURN FN))
;;				  ((EQ (CAR FN) 'LAMBDA)
;;				   (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;;				  (T (ERROR  "Unknown definition of ~S -- ~S" SYM FN))))
;;			   ((ml-typep funct 'array)
;;			    'array)
;;			   ((ml-typep funct 'COMPILED-FUNCTION)
;;			    (SETQ ARGS-INFO (%ARGS-INFO FN))
;;			    #+ti
;;			    (COND ((ldb-test %%arg-desc-quoted-rest args-info) 'fsubr)
;;				  ((ldb-test %%arg-desc-fef-quote-hair args-info) 'fsubr)
;;				  ((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;					   (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;				   'LSUBR)
;;				  (T 'SUBR))
;;			    #-ti
;;			    (COND ((BIT-TEST (DPB 1 %%ARG-DESC-QUOTED 0) ARGS-INFO) 'FSUBR)
;;				  ((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;					   (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;				   'LSUBR)
;;				  (T 'SUBR)))
;;			   ( t			;(TYPEP funct 'T)
;;			    (ERROR  "Unknown object in function cell of ~S -- ~S"
;;				    SYM FN))))))
;;	(RETURN (AND RPROP
;;		     (MEMQ RPROP PROPS)
;;		     (LIST RPROP FN)))))
;;

;;the following is fine but we don't need it.
;;(defun maxima-symbol-function (sym &aux tem fn)
;; (check-arg sym symbolp "symbol")
;; (cond ((fboundp sym)
;;	(setq fn (symbol-function sym))
;;	(cond ((functionp fn)(values fn 'subr))
;;	      ;;really just suitable for apply I think.
;;              ((macro-function sym)(values fn 'macro))
;;	      ((arrayp fn)(values fn 'array))
;;	      (t (error "unknown fn"))))
;;       ((setq tem (symbol-array sym))
;;	(values tem 'array))
;;       ((setq tem (get sym 'mfexpr*))
;;	(values tem 'mfexpr*))
;;       (t nil)))

;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (check-arg sym symbolp "symbol")
;;  (and (fboundp sym) (multiple-value-bind (fn typ) (maxima-symbol-function sym)
;;		       (cond ((memq typ props)(list typ fn))
;;			     ((eq typ 'lambda)(list 'subr fn))
;;			     (t nil)))))

;;
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS &aux fn typ)
;;  (check-arg sym symbolp "symbol")
;;  (cond ((fboundp sym)
;;	 (setq fn (symbol-function sym))
;;	 (cond
;;	   #+lucid
;;	   ((macro-function sym)
;;	    (setq typ 'macro))
;;	   ((functionp fn)
;;		;;this is what we did but do we want if 'subr not in props??
;;		 (return-from GETL-LM-FCN-PROP (list 'subr fn)))
;;		((macro-function sym)
;;		 (setq typ 'macro))
;;		#+lispm ((arrayp fn)(values fn 'array))
;;		(t (error "unknown fn"))))
;;	((setq fn (symbol-array sym))
;;	 (setq typ 'array))
;;	((setq fn (get sym 'mfexpr*))
;;	 (setq typ 'mfexpr*)))
;;  (and typ (member typ props :test 'eq) (list typ fn)))	
;;
(defun getl-lm-fcn-prop (sym props &aux fn typ)
  (check-arg sym symbolp "symbol")
  (setq fn sym)
  (cond
    ((functionp fn)
     (setq typ 'subr))
    ((macro-function sym)
     (setq typ 'macro))
    ;;    #+lispm ((arrayp fn)(values fn 'array))
    ((setq fn (symbol-array sym))
     (setq typ 'array))
    ((setq fn (get sym 'mfexpr*))
     (setq typ 'mfexpr*)))
  (and typ (member typ props :test 'eq) (list typ fn)))	


;;#+LISPM
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (PROG (FN RPROP ARGS-INFO)
;;    (SETQ RPROP
;;	  (AND (FBOUNDP SYM)
;;	       (TYPECASE (SETQ FN (SYMBOL-FUNCTION SYM))
;;		 (:SYMBOL (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;;		 (:LIST (COND ((MEMQ (CAR FN) '(MACRO SUBST)) 'MACRO)
;;			      ((EQ (CAR FN) 'NAMED-LAMBDA)
;;			       (IF (MEMQ '&QUOTE (CADDR FN))
;;				   'FEXPR 'EXPR))
;;			      ((EQ (CAR FN) 'LAMBDA)
;;			       (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;;			      (T (ERROR () "Unknown definition of ~S -- ~S"
;;					 SYM FN))))
;;		 (:ARRAY (kw array))
;;		 (:COMPILED-FUNCTION
;;		  (SETQ ARGS-INFO (%ARGS-INFO FN))
;;		  (COND ((BIT-TEST #-3600 (f+ %ARG-DESC-QUOTED-REST
;;					     %ARG-DESC-FEF-QUOTE-HAIR)
;;				   #+3600 (DPB 1 SI:%%ARG-DESC-QUOTED 0)
;;				   ARGS-INFO)
;;			 'FSUBR)
;;			((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;				 (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;			 'LSUBR)
;;			(T 'SUBR)))
;;		 (T (ERROR () "Unknown object in function cell of ~S -- ~S"
;;			    SYM FN)))))
;;    (RETURN (AND RPROP
;;		 (MEMQ RPROP PROPS)
;;		 (LIST RPROP FN)))))


;;#+NIL
;;(DEFUN GETL-NIL-FCN-PROP (SYM PROPS)
;;  (IF (FBOUNDP SYM)
;;      (LET* ((F (SYMBOL-FUNCTION SYM))
;;	     (PROP (IF (ATOM F)
;;		       (IF (EQ (TYPE-OF F) 'SUBR) 'SUBR 'EXPR)
;;		       (CAR F))))
;;	(IF (MEMQ PROP PROPS) (LIST PROP F)))))
;;    (RETURN (AND RPROP (MEMQ RPROP PROPS) (LIST RPROP FN)))))
 
;;#+NIL
;;(defun getl-nil-fcn-prop (sym props)
;;  (and (fboundp sym)
;;       (let* ((f (symbol-function sym))
;;	      (prop (if (atom f)
;;			(if (ml-typep f (kw COMPILED-FUNCTION)) 'subr 'expr)
;;			(car f))))
;;	 (when (memq prop '(defmacro subst)) (setq prop 'macro))
;;	 (if (memq prop props) (list prop f)))))

(defmfun meval2 (newargs old)
  (declare (special aryp))
  (let ((new (cons (car old) newargs)) nosimp)
    (cond ((not (memq 'simp (cdar old)))
	   (if (and (not (eq (caar new) 'mlist)) (equal new old)) old new))
	  ((prog2 (setq nosimp (not (get (caar new) 'operators))) (alike1 new old))
	   (if nosimp old (cons (delsimp (car old)) (cdr old))))
	  (nosimp (if aryp new (cons (cons (caar new) '(simp)) newargs)))
	  (t (cons (cons (caar new) aryp) newargs)))))
 
(defun mparams (vars)
  (mapcar #'(lambda (x) (cond ((atom x) x)
			      ((atom (cadr x)) (cadr x))
			      (t (cadadr x))))
	  (cdr vars)))

(defmfun mop (form) (if (eq (caar form) 'mqapply) (cadr form) (caar form)))
	
(defmfun margs (form) (if (eq (caar form) 'mqapply) (cddr form) (cdr form)))

(defun badfunchk (name val flag)
  (if (or flag (numberp val) (memq val '(t nil $%e $%pi $%i)))
      ;;    (OR FLAG (AND (NOT $OPERATORS)
      ;;		   (OR (NUMBERP VAL) (MEMQ VAL '(T NIL $%E $%PI $%I)))))
      (if (and (atom name) (not (equal val name)))
	  (merror "~:M evaluates to ~M~
		  ~%Improper name or value in functional position."
		  name val)
	  (merror "Improper name or value in functional position:~%~M"
		  val))))

;;#+MacLisp
;;(DEFUN MARGCHK (FN ARGS) 
;; (LET (EXPR)
;;      (OR (NOT (OR (SETQ EXPR (GET FN 'EXPR)) (GET FN 'SUBR)))
;;	  (NOT (ARGS FN))
;;	  (CAR (ARGS FN))
;;	  (LET ((NNEED (CDR (ARGS FN))) (NGIVEN (LENGTH ARGS)))
;;	       (WHEN (NOT (= NNEED NGIVEN))
;;		     (IF (AND EXPR (NOT (MGET FN 'TRACE))
;;			      (OR (NULL (CADR EXPR)) (NOT (ATOM (CADR EXPR)))))
;;			 (SETQ FN (CONS (NCONS FN) (CADR EXPR))))
;;		     (MERROR "Too ~M arguments supplied to ~M:~%~M"
;;			     (IF (< NNEED NGIVEN) '|&many| '|&few|)
;;			     FN
;;			     (CONS '(MLIST) ARGS)))))))

;;#+Franz
;;(defun margchk (fn args)
;;   (let (expr argdesc)
;;      (or (not (symbolp fn))
;;	  (not (getd fn))
;;	  (null (setq argdesc (car (get fn 'fcn-info))))
;;	  (let ((minimum (car argdesc))
;;		(maximum (cdr argdesc))
;;		(ngiven (length args)))
;;	     (cond ((or (and maximum (> ngiven maximum))
;;			(and minimum (< ngiven minimum)))
;;		    (merror "Too ~M arguments supplied to ~M:~%~M"
;;			    (cond ((> ngiven maximum) '|&many|)
;;				  (t '|&few|))
;;			    fn
;;			    (cons '(mlist) args))))))))



;;	  (LET ((NNEED (car argdesc)) (NGIVEN (LENGTH ARGS)))
;;	     (cond ((NOT (= NNEED NGIVEN))
;;		    (MERROR "Too ~M arguments supplied to ~M:~%~M"
;;			    (cond ((< NNEED NGIVEN) '|&many|)
;;				  (t '|&few|))
;;			    FN
;;			    (CONS '(MLIST) ARGS))))))))
;;#+LISPM
;;(DEFUN MARGCHK (FN ARGS &AUX ARG-DESC MIN-NARGS MAX-NARGS ACTUAL-NARGS)
;;  (AND (SYMBOLP FN)
;;       (FBOUNDP FN)
;;       (PROGN (SETQ ARG-DESC (ARGS-INFO FN)
;;		    MIN-NARGS (LDB %%ARG-DESC-MIN-ARGS ARG-DESC)
;;		    MAX-NARGS (LDB %%ARG-DESC-MAX-ARGS ARG-DESC)
;;		    ACTUAL-NARGS (LENGTH ARGS))
;;	      (OR (< ACTUAL-NARGS MIN-NARGS)
;;		  (AND (ZEROP (LOGAND #-3600 (f+ %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
;;;				      #+3600 (DPB 1 SI:%%ARG-DESC-REST-ARG 0)
;;				        #+(or lispm 3600)
;;					(DPB 1 SI:%%ARG-DESC-REST-ARG 0)
;;				      ARG-DESC))     ; has a rest argument means
;;		       (> ACTUAL-NARGS MAX-NARGS)))) ; don't check max args.
;;       (MERROR "Too ~M arguments supplied to ~M:~%~M"
;;	       (IF (< ACTUAL-NARGS MIN-NARGS) '|&few| '|&many|)
;;	       `((,FN) ,@(ARGLIST FN))
;;	       `((MLIST) ,@ARGS))))

(defun mbind-doit (lamvars fnargs fnname)
  "Makes a new frame where the variables in the list LAMVARS are bound
to the corresponding elements in FNARGS.  Note that these elements are
used tels quels, without calling MEVAL.
If FNNAME is non-NIL, it designates a function call frame.
This function does not handle errors properly, use the MBIND
wrapper for this."
  (declare (special bindlist mspeclist))
  (do ((vars lamvars (cdr vars)) (args fnargs (cdr args)))
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
      (let ((value (if (boundp var)
		       (symbol-value var)
		       munbound)))
	(without-tty-interrupts
	  (mset var (car args))
	  (psetq bindlist (cons var bindlist)
		 mspeclist (cons value mspeclist)))))))

(defun mbind (lamvars fnargs fnname)
  "Error-handling wrapper around MBIND-DOIT."
  (handler-case
      (let ((old-bindlist bindlist) win)
	(declare (special bindlist))
	(unwind-protect
	     (prog1
		 (with-$error
		   (mbind-doit lamvars fnargs fnname))
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

;; (defmfun mbind (lamvars fnargs fnname)
;;   (do ((vars lamvars (cdr vars)) (args fnargs (cdr args)))
;;       ((cond ((and vars args) nil)
;; 	     ((and (null vars) (null args)))
;; 	     (t (merror "Too ~M arguments supplied to ~M:~%~M"
;; 		        (if vars '|&FEW| '|&MANY|)
;; 		        (if fnname (cons (ncons fnname) lamvars)
;; 			    '|&A FUNCTION|)
;; 		        (cons '(mlist) fnargs)))))
;;     (let ((var (car vars)))
;;       (if (not (symbolp var))
;; 	  (merror "Only symbolic atoms can be bound:~%~M" var))
;;       #-franz (without-tty-interrupts
;; 	       (let ((bl (cons var bindlist))
;; 		     (ml (cons (if (boundp var) (symbol-value var) munbound)
;; 			       mspeclist)))
;; 		 (setq bindlist bl mspeclist ml)))
;;       ;;#+Franz (SETQ BINDLIST (CONS VAR BINDLIST))
;;       ;;#+Franz (SETQ MSPECLIST (CONS (IF (BOUNDP VAR) (SYMBOL-VALUE VAR) MUNBOUND)
;;       ;;			  MSPECLIST))
;;       (mset var (car args)))))

(defmfun munbind (vars)
  (dolist (var (reverse vars))
    (cond ((eq (car mspeclist) munbound)
	   (makunbound var) (delq var $values 1))
	  (t (let ((munbindp t)) (mset var (car mspeclist)))))
    (setq mspeclist (cdr mspeclist) bindlist (cdr bindlist))))

;;This takes the place of something like
;; (DELETE (ASSOC (NCONS VAR) $DEPENDENCIES) $DEPENDENCIES 1)
;;(defun mfunction-delete (var fn-a-list)
;;  (sys:delete var fn-a-list :count 1
;;	      :test #'(lambda (var elt)
;;			(declare (optimize (speed 3) (safety 0)))
;;			(and elt (consp (setq elt (car elt)))
;;			     (eq (car elt) var) (null (cdr elt))))))

(defun mfunction-delete (var fn-a-list)
  (zl-delete (zl-assoc (ncons var) fn-a-list) fn-a-list 1))



(defmspec mlocal (l)
  (setq loclist (cons nil loclist))
  (let ((mlocp t)) (meval `(($local) ,@(cdr l)))))

(defmspec $local (l) (setq l (cdr l))
	  (if (not mlocp) (merror "Improper call to `local'"))
	  (nointerrupt 'tty)
	  (dolist (var l)
	    (cond ((not (symbolp var))
		   (nointerrupt nil) (improper-arg-err var '$local))
		  ((and (mget var 'array)
					;#+MacLisp (GET VAR 'array)
			(arrayp (symbol-array var)))
		   (nointerrupt nil)
		   (merror "Attempt to bind a complete array ~M" var)))
	    (setq mproplist (cons (get var 'mprops) mproplist)
		  factlist (cons (get var 'data) factlist))
	    (dolist (fact (car factlist)) (putprop fact -1 'ulabs))
	    (progn
	      (mfunction-delete var $functions)
	      (mfunction-delete var $macros)
	      (mfunction-delete var $dependencies))
	    (delq var $arrays 1)
	    (zl-remprop var 'mprops)
	    (zl-remprop var 'data))
	  (rplaca loclist (reverse l))
	  (setq mlocp nil)
	  (nointerrupt nil)
	  '$done)

(defun munlocal nil
  (nointerrupt 'tty)
  (dolist (var (car loclist))
    (let ((mprop  (car mproplist))( y  nil)( fact  (car factlist)))
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
	    (t (delq var $arrays 1)))
      (cond ((setq y (old-get mprop 'depends))
	     (add2lnc (cons (ncons var) y) $dependencies))
	    (t (mfunction-delete var $dependencies)))
      (rempropchk var)
      (mapc #'remov (get var 'data))
      (cput var fact 'data)
      (dolist (u fact) (zl-remprop u 'ulabs))
      (setq mproplist (cdr mproplist) factlist (cdr factlist))))
  (setq loclist (cdr loclist))
  (nointerrupt nil))

;;(declare-top (MACROS T))
;;do we really need this??
;;since its incompatible with the special definition


;;(defmacro msetq (&rest l) `(mset ',(first l) ,(second l)))

(defmacro msetq (a b) `(mset ',a ,b))

;; A "run-time macro" needed by MATCOM/MATRUN.
(declare-top (macros nil))
;;works with the defms
(defmspec msetq (l)
  (twoargcheck l)
  (mset (simplifya (cadr l) nil) (meval (caddr l))))

(defun mset (x y)
  (declare (object y x))
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
		      (memq x '(t nil))
		      (mget x '$numer)
		      (char= (getcharn x 1) #\&))
	      (if munbindp (return nil))
	      (if (mget x '$numer)
		  (merror "~:M improper value assignment to a numerical quantity" x)
		  (merror "~:M improper value assignment" x)))
	    (let ((f (get x 'assign)))
	      (if (and f (or (not (eq x y))
			     (memq f '(neverset read-only-assign))))
		  (if (eq (funcall f x y) 'munbindp) (return nil))))
	    (cond ((and (not (boundp x))
			(not dsksetp))
		   (add2lnc x $values))
		  ((and (not (eq x y))
			(optionp x))
		   (if $optionset (mtell "~:M option is being set.~%" x))
		   (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	    (return (set x y)))
	   ((memq 'array (cdar x))
	    (return (arrstore x y)))
	   ((and $subscrmap (memq (caar x) '(mlist $matrix)))
	    (return (outermap1 'mset x y)))
	   (t (merror "Improper value assignment:~%~M" x)))))


(defmspec $ev (l) (setq l (cdr l))
	  (let ((evp t) (nounl nounl) ($float $float) ($numer $numer)
		($expop $expop) ($expon $expon) ($doallmxops $doallmxops)
		($doscmxops $doscmxops) (derivflag derivflag) ($detout $detout)
		(nounsflag nounsflag) (rulefcnl rulefcnl))
	    (if (and (cdr l) (null (cddr l)) (eq (car l) '$%e) (eq (cadr l) '$numer))
		(setq l (append l '($%enumer))))
	    (do ((l (cdr l) (cdr l)) (bndvars) (bndvals) (locvars) (exp (car l))
		 (subsl) (evflg 0) (ratf) (derivlist) (evfunl) (funcl) (predflg)
		 (noeval (memq '$noeval (cdr l))))
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
						  (= (setq evflg (f1- evflg)) 0))
					     (prog2 (setq exp1 (specrepcheck exp1))
						 (alike1 exp exp1)))
					 (setq exp exp1))
					(t (setq exp exp1) (go loop)))))
			   (if (and ratf (not $numer) (not $float))
			       (setq exp (let ($norepeat) (ratf exp)))))
		 (munlocal)
		 exp)
	      (if (not (or (atom (car l))
			   (memq 'array (cdaar l))
			   (memq (caaar l) '(mquote msetq mlist mequal mdefine mset
					     mdefmacro $expand $local $derivlist))))
		  (setq l (cons (meval (car l)) (cdr l))))
	      (cond ((or (atom (car l)) (memq 'array (cdaar l)) (eq (caaar l) 'mquote))
		     (or (and (symbolp (car l))
			      (cond ((eq (car l) '$eval) (setq evflg (f1+ evflg)))
				    ((memq (car l) '($noeval $rescan)))
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
				 ((memq (caar fl)
					'(msetq mequal mdefine mdefmacro mset))
				  (setq l (list* nil fl (cdr l))))
				 (t (improper-arg-err (car l) '$ev))))))
		    ((not (memq (caaar l) '(msetq mlist mequal mdefine mdefmacro
					    $expand $local $derivlist mset)))
		     (improper-arg-err (car l) '$ev))
		    ((eq (caaar l) '$expand)
		     (cond ((null (cdar l)) (setq $expop $maxposex $expon $maxnegex))
			   ((null (cddar l)) (setq $expop (cadar l) $expon $maxnegex))
			   (t (setq $expop (cadar l) $expon (caddar l)))))
		    ((memq (caaar l) '(mdefine mdefmacro))
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
		    ((memq (caaar l) '(msetq mequal))
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
	((memq 'array (cdar exp))
	 (let (exp1)
	   (let ((evarrp t)) (setq exp1 (catch 'evarrp (meval1 exp))))
	   (if (eq exp1 'notexist)
	       (cons (car exp) (mapcar #'mevalatoms (cdr exp)))
	       exp1)))
	((eq (caar exp) 'mquote) (cadr exp))
	((memq (caar exp) '(msetq $define))
	 (list (car exp) (cadr exp) (mevalatoms (caddr exp))))
	((or (and (eq (caar exp) '$ev)
		  (cdr exp)
		  (or (null (cddr exp)) (equal (cddr exp) '($eval))))
	     (eq (caar exp) 'mprogn))
	 (cons (car exp) (cons (mevalatoms (cadr exp)) (cddr exp))))
	((memq (caar exp) '($sum $product %sum %product))
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

(prog1 '(evok properties)
  (mapc #'(lambda (x) (putprop x t 'evok))
	'($map $maplist $fullmap $matrixmap $fullmapl $outermap $scanmap
	  $apply)))

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

(defmfun msetqp (x) (and (not (atom x)) (eq (caar x) 'msetq)))

(defmfun mquotep (x) (and (not (atom x)) (eq (caar x) 'mquote)))

(defmspec mquote (form) (cadr form))

(defmfun $subvarp (x) (and (not (atom x)) (memq 'array (cdar x)) t))

(defmfun mseterr (x y)
  (if munbindp
      'munbindp
      (merror "Attempt to set ~:M to ~M~%Improper value assignment" x y)))

(prog1 '(assign properties)
  (mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
	'(($linel msetchk) (*read-base* msetchk) (*print-base* msetchk) (modulus msetchk)
	  ($infolists neverset) ($trace neverset) ($ratweights msetchk)
	  ($ratvars msetchk) ($setcheck msetchk) ($gcd msetchk)
	  ($dotassoc msetchk) ($ratwtlvl msetchk) ($ratfac msetchk)
	  ($all neverset) ($numer numerset) ($fortindent msetchk)
	  ($gensumnum msetchk) ($genindex msetchk) ($fpprintprec msetchk)
	  ($floatwidth msetchk) ($parsewindow msetchk) ($optimprefix msetchk)
	  ($ttyintnum msetchk))))

(defmfun msetchk (x y)
  (cond ((memq x '(*read-base* *print-base*))
	 (cond #-nil ((eq y 'roman))
	       ((or (not (fixnump y)) (< y 2) (> y 35)) (mseterr x y))
	       ((eq x '*read-base*)
		#+maclisp (if (< y 11) (sstatus + nil) (sstatus + t)))))
	((memq x '($linel $fortindent $gensumnum $fpprintprec $floatwidth
		   $parsewindow $ttyintnum))
	 (if (not (fixnump y)) (mseterr x y))
	 ;;	#+MacLisp
	 ;;	(WHEN (EQ X '$LINEL)
	 ;;	  (LINEL T (LINEL NIL Y))
	 ;;	  (DOLIST (FILE OUTFILES) (LINEL FILE Y))
	 ;;	  (SETQ LINEL Y))
	 (if (eq x '$linel) (setq linel y))
	 (cond ((and (memq x '($fortindent $gensumnum $floatwidth $ttyintnum)) (< y 0))
		(mseterr x y))
	       ((and (eq x '$parsewindow) (< y -1)) (mseterr x y))
	       ((and (eq x '$fpprintprec) (or (< y 0) (= y 1))) (mseterr x y))))
	((memq x '($genindex $optimprefix)) (if (not (symbolp y)) (mseterr x y)))
	((eq x '$dotassoc) (cput 'mnctimes y 'associative))
	((eq x 'modulus)
	 (cond ((null y))
	       ((integerp y)
		(if (or (not (primep y)) (zl-member y '(1 0 -1)))
		    (mtell "Warning: `modulus' being set to ~:M, a non-prime.~%" y)))
	       (t (mseterr x y))))
	((eq x '$setcheck)
	 (if (not (or (memq y '($all t nil)) ($listp y))) (mseterr x y)))
	((eq x '$gcd) (if (not (or (null y) (memq y *gcdl*))) (mseterr x y)))
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

(defmspec $map (l) (apply #'map1 (mmapev l)))

(defmfun map1 n
  (do ((i n (f1- i))
       (argi (setarg n (format1 (arg n))) (format1 (arg (f1- i))))
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
      ((do ((i n (f1- i))) ((= i 1) nil)
	 (when (null (arg i))
	   (when (or (< i n) (do ((j 2 (f1+ j))) ((= j n) nil)
			       (if (arg j) (return t))))
	     (if $maperror
		 (merror "Arguments to `mapl' are not of the same length."))
	     (if $mapprint (mtell "`map' is truncating.~%")))
	   (return t))
	 (setq argl (cons (car (arg i)) argl))
	 (setarg i (cdr (arg i))))
       (nreverse ans))))

(defun mapatom (x) (or (symbolp x) (mnump x) ($subvarp x)))

(defmfun $mapatom (x) (if (mapatom (specrepcheck x)) t))

(defmspec $fullmap (l) (setq l (mmapev l)) (fmap1 (car l) (cdr l) nil))

(defun fmap1 (fn argl fmapcaarl)
  (setq argl (mapcar #'format1 argl))
  (do ((op (or (mapatom (car argl)) (mop (car argl))))
       (fmaplvl (f1- fmaplvl)) (cdr1 argl (cdr cdr1)) (argi nil nil)
       (cdrl nil (cons (margs (car cdr1)) cdrl)))
      ((null cdr1)
       (do ((ans nil (cons (if bottom (funcer fn carargl)
			       (fmap1 fn carargl fmapcaarl))
			   ans))
	    (carargl nil nil) (cdrargl nil nil)
	    (cdrl cdrl cdrargl) (bottom nil nil)
	    (done (when (memq nil cdrl)
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

(defmspec $matrixmap (l) (let ((fmaplvl 2)) (apply #'fmapl1 (mmapev l))))

(defmspec $fullmapl (l) (apply #'fmapl1 (mmapev l)))

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
	   (setq outargs1 (ncons (arg 1)) outargs2 (listify (f- 2 n)))
	   (fmapl1 'outermap2 (arg 2)))
	  (t (do ((i 3 (f1+ i)))
		 ((> i n) (funcer (arg 1) (listify (f- 1 n))))
	       (when (mxorlistp (arg i))
		 (setq outargs1 (listify (f1- i))
		       outargs2 (if (< i n) (listify (f- i n))))
		 (return (fmapl1 'outermap2 (arg i)))))))))

(defmfun outermap2 n
  (if (not (zerop n))
      (apply #'outermap1 (append outargs1 (listify 1) outargs2))))

(defmfun funcer (fn args)
  (cond ((and (not opexprp) (memq fn '(mplus mtimes mexpt mnctimes)))
	 (simplify (cons (ncons fn) args)))
	((or (memq fn '(outermap2 constfun))
	     (and $transrun (symbolp fn) (get fn 'translated)
		  (not (mget fn 'local-fun)) (fboundp fn)))
	 (apply fn (mapcar #'simplify args)))
	(t (mapply1 fn (mapcar #'simplify args) fn
		    nil	;; try to get more info to pass
		    ))))

(defmspec $qput (l) (setq l (cdr l))
	  (if (not (= (length l) 3)) (wna-err '$qput))
	  ($put (car l) (cadr l) (caddr l)))

(defmfun $rem (atom ind) (prop1 '$rem atom nil ind))

(defmfun $put (atom val ind)
  (prog1 (prop1 '$put atom val ind) (add2lnc atom $props)))

(defun prop1 (fun atom val ind)
  (nonsymchk atom fun) (nonsymchk ind fun)
  (let ((u (mget atom '$props)))
    (cond ((eq fun '$get) (and u (old-get u ind)))
	  ((eq fun '$rem) (and u (zl-remprop u ind) '$done))
	  ((not u) (mputprop atom (list nil ind val) '$props) val)
	  (t (putprop u val ind)))))

(defmspec $declare (l) (setq l (cdr l))
	  (if (oddp (length l)) (merror "`declare' takes an even number of arguments."))
	  (do ((l l (cddr l)) (vars) (flag nil nil)) ((null l) '$done)
	    (cond (($listp (cadr l))
		   (do ((l1 (cdadr l) (cdr l1))) ((if (null l1) (setq flag t)))
		     (meval `(($declare) ,(car l) ,(car l1)))))
		  ((nonsymchk (cadr l) '$declare))
		  (t (setq vars (declsetup (car l) '$declare))))
	    (cond (flag)
		  ((memq (cadr l) '($evfun $evflag $special $nonarray $bindtest))
		   (declare1 vars t (stripdollar (cadr l)) nil))
		  ((eq (cadr l) '$noun)
		   (dolist (var vars) (alias (getopr var) ($nounify var))))
		  ((memq (cadr l) '($constant $nonscalar $scalar $mainvar))
		   (declare1 vars t (cadr l) t))
		  ((memq (cadr l) opers)
		   (if (memq (cadr l) (cdr $features)) (declare1 vars t (cadr l) 'kind))
		   (declare1 (mapcar #'getopr vars) t (cadr l) 'opers))
		  ((memq (cadr l) (cdr $features)) (declare1 vars t (cadr l) 'kind))
		  ((eq (cadr l) '$feature)
		   (dolist (var vars) (nonsymchk var '$declare) (add2lnc var $features)))
		  ((eq (cadr l) '$alphabetic) (declare1 vars t t '$alphabetic))
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
	   (putprop (setq val (stripdollar var)) t 'alphabet)
	   (add2lnc (getcharn val 1) alphabet))
	  ((eq prop 'special)(proclaim (list 'special var))
	   (fluidize var))
	  (mpropp
	   (if (and (memq prop '($scalar $nonscalar))
		    (mget var (if (eq prop '$scalar) '$nonscalar '$scalar)))
	       (merror "Inconsistent Declaration: ~:M"
		       `(($declare) ,var ,prop)))
	   (mputprop var val prop))
	  (t (putprop var val prop)))
    (if (and (get var 'op) (operatorp1 var)
	     (not (memq (setq var (get var 'op)) (cdr $props))))
	(setq mopl (cons var mopl)))
    (add2lnc (getop var) $props)))

(defun linchk (var)
  (if (memq var '($sum $integrate $limit $diff $transpose)) ($nounify var) var))

(defmspec $remove (form) (i-$remove (cdr form)))

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
	   ;;*** MERGE LOSSAGE ***
	   ;;*** File R20:AUX:<ATP.SCHELTER.MACSYMA>MLISP.LISP.14 has:
	   (remove1 (mapcar #'rem-verbify vars) 'mexpr t $functions t))
	  ((eq (cadr l) '$macro)
	   (remove1 (mapcar #'rem-verbify vars) 'mmacro t $macros t))
	  ;;*** File R20:PS:<MACSYM.LSP-TEMP>MLISP.LSP.1 has:
	  ;;(REMOVE1 (MAPCAR #'$VERBIFY VARS) 'MEXPR T $FUNCTIONS T))
	  ;;((EQ (CADR L) '$MACRO)
	  ;;(REMOVE1 (MAPCAR #'$VERBIFY VARS) 'MMACRO T $MACROS T))
	   
	  ((eq (cadr l) '$array) (meval `(($remarray) ,@vars)))
	  ((memq (cadr l) '($alias $noun)) (remalias1 vars (eq (cadr l) '$alias)))
	  ((eq (cadr l) '$matchdeclare) (remove1 vars 'matchdeclare t t nil))
	  ((eq (cadr l) '$rule) (remrule vars))
	  ((memq (cadr l) '($evfun $evflag $special $nonarray $bindtest
			    $autoload $assign))
	   (remove1 vars (stripdollar (cadr l)) nil t nil))
	  ((memq (cadr l) '($mode $modedeclare)) (remove1 vars 'mode nil 'foo nil))
	  ((eq (cadr l) '$atvalue) (remove1 vars 'atvalues t t nil))
	  ((memq (cadr l) '($constant $nonscalar $scalar $mainvar $numer $atomgrad))
	   (remove1 vars (cadr l) t t nil))
	  ((memq (cadr l) opers) (remove1 (mapcar #'linchk vars) (cadr l) nil t nil))
	  ((memq (cadr l) (cdr $features)) (remove1 vars (cadr l) nil t nil))
	  ((eq (cadr l) '$feature) (dolist (var vars) (delq var $features 1)))
	  ((memq (cadr l) '($alphabetic $transfun))
	   (remove1 vars (cadr l) nil t nil))
	  ((memq (cadr l) '($gradef $grad)) (remove1 vars 'grad nil $gradefs t))
	  ((memq (cadr l) '($dependency $depend $depends))
	   (remove1 vars 'depends t $dependencies t))
	  ((memq (cadr l) '($op $operator)) (remove1 vars '$op nil 'foo nil))
	  ((memq (cadr l) '($deftaylor $taylordef)) (remove1 vars 'sp2 nil t nil))
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
  (do ((vars vars (cdr vars)) (allflg)) ((null vars))
    (nonsymchk (car vars) '$remove)
    (cond ((and (eq (car vars) '$all) (null allflg))
	   (setq vars (append vars (cond ((atom info) (cdr $props))
					 (funp (mapcar #'caar (cdr info)))
					 (t (cdr info))))
		 allflg t))
	  (t
	   (let ((var  (getopr (car vars)))( flag  nil))
	    
	     (cond (mpropp (mremprop var prop)
			   (when (memq prop '(mexpr mmacro))
			     (mremprop var 'mlexprp)
			     (mremprop var 'mfexprp)
			     (if (not (get var 'translated))
				 (args var nil))
			     (if (mget var 'trace)
				 (macsyma-untrace var))))
		   ((eq prop '$op) (kill-operator var))
		   ((eq prop '$alphabetic)
		    (zl-remprop (setq prop (stripdollar var)) 'alphabet)
		    (zl-delete (getcharn prop 1) alphabet 1))
		   ((eq prop '$transfun)
		    (remove-transl-fun-props var)
		    (remove-transl-array-fun-props var))
		   ((or (setq flag (memq prop (cdr $features))) (memq prop opers))
		    (if flag (unkind var prop))
		    (zl-remprop var prop)
		    (if (not (getl var (delq prop (copy-top-level opers) 1)))
			(zl-remprop var 'opers)))
		   (t (zl-remprop var prop)))
	     (cond ((eq info t) (rempropchk var))
		   ((eq info 'foo))
		   (funp     ;(DELETE (ASSOC (NCONS VAR) INFO) INFO 1)
		    (mfunction-delete var info))
		   (t (delq var info 1))))
	   ))))

(defun remove-transl-fun-props (fun)
  (if (mget fun 'trace) (macsyma-untrace fun))
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    ;;       #+Maclisp
    ;;       (DO ((PROPS '(EXPR SUBR LSUBR FEXPR FSUBR) (CDR PROPS)))
    ;;	   ((NULL PROPS))
    ;;	   (ZL-REMPROP FUN (CAR PROPS)))
    #-maclisp
    (fmakunbound fun)
    (zl-remprop fun 'translated-mmacro)
    (mremprop fun 't-mfexpr)
    (zl-remprop fun 'function-mode)
    #-(or cl nil)
    (if (not (mgetl fun '(mexpr mmacro))) (args fun nil))
    (if (not (getl fun '(a-expr a-subr))) (zl-remprop fun 'translated))))

(defun remove-transl-array-fun-props (fun)
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    (zl-remprop fun 'a-expr)
    (zl-remprop fun 'a-subr)
    (if (not (fboundp fun)) (zl-remprop fun 'translated))))

(defmfun rempropchk (var)
  (if (and (not (mgetl var '($constant $nonscalar $scalar $mainvar $numer
			     matchdeclare $atomgrad atvalues t-mfexpr)))
	   (not (getl var '(evfun evflag translated nonarray bindtest
			    opr sp2 operators opers special data
			    alphabet autoload mode)))
	   (not (memq var *builtin-$props*)))
      (delq var $props 1)))

(defun rem-verbify (fnname) (nonsymchk fnname '$remove) ($verbify fnname))



(defmspec $remfunction (l) (setq l (cdr l))
	  (cond ((memq '$all l)
		 (setq l (nconc (mapcar #'caar (cdr $functions))
				(mapcar #'caar (cdr $macros)))))
		(t (setq l (mapcar #'rem-verbify l))
		   (do ((l1 l (cdr l1))) ((null l1) t)
		     (if (not (or (zl-assoc (ncons (car l1)) (cdr $functions))
				  (zl-assoc (ncons (car l1)) (cdr $macros))))
			 (rplaca l1 nil)))))
	  (remove1 l 'mexpr t $functions t)
	  (remove1 l 'mmacro t $macros t)
	  (cons '(mlist) l))

;; (SETQ L (MAPCAR #'$VERBIFY L))
;; (DO L1 L (CDR L1) (NULL L1)
;;     (COND ((EQ (CAR L1) '$ALL)
;;	      (LET ((ZZ (DELQ '$ALL L1)))
;;		(RPLACA (RPLACD L1 (CDR ZZ)) (CAR ZZ)))
;;	      (NCONC L (MAPCAR #'CAAR (CDR $FUNCTIONS))
;;		     (MAPCAR #'CAAR (CDR $MACROS))))
;;	   ((NOT (OR (ASSOC (NCONS (CAR L1)) (CDR $FUNCTIONS))
;;		     (ASSOC (NCONS (CAR L1)) (CDR $MACROS))))
;;	    (RPLACA L1 NIL))))
;; (REMOVE1 L 'MEXPR T $FUNCTIONS T)
;; (REMOVE1 L 'MMACRO T $MACROS T)
;; (CONS '(MLIST) L))

(defmspec $remarray (l) (setq l (cdr l))
	  (cons '(mlist)
		(do ((l l (cdr l)) (x) (pred)) ((null l) (nreverse x))
		  (cond ((eq (car l) '$all) (setq l (append l (cdr $arrays))))
			(t (remcompary (car l)) (setq pred (mremprop (car l) 'array))
			   (setq pred (or (mremprop (car l) 'hashar) pred))
			   (setq pred (or (mremprop (car l) 'aexpr) pred))
			   (setq x (cons (and pred (prog2 (delq (car l) $arrays 1) (car l)))
					 x)))))))

(defun remcompary (x)
  (cond ((eq x (mget x 'array))
	 (zl-remprop x 'array-mode)
	 (zl-remprop x 'array))))

(defmspec $remvalue (form) (i-$remvalue (cdr form)))

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
  (if (mfilep (cadr ary)) (i-$unstore (ncons (caar form))))
  #-nil
  (let ((y (car (arraydims (cadr ary)))))
    (arrstore form (cond ((eq y 'fixnum) 0) ((eq y 'flonum) 0.0) (t munbound))))
  ;;  #+nil
  ;; (LET ((Y (ARRAY-TYPE (CADR ARY))))
  ;;   (ARRSTORE FORM (OR (CDR (ASSQ Y '((FIXNUM . 0)
  ;;				     (FLONUM . 0.0)
  ;;				#+NIL(SINGLE-FLOAT . 0.0F0)
  ;;				#+NIL(SHORT-FLOAT . 0.0S0)
  ;;				#+NIL(DOUBLE-FLOAT . 0.0D0)
  ;;				#+NIL(LONG-FLOAT . 0.0L0)
  ;;				)))
  ;;		      MUNBOUND)))

  )

(defmfun remrule (l)
  (do ((l l (cdr l)) (u)) ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $rules))))
	  ((get (car l) 'operators) ($remrule (car l) '$all))
	  ((setq u (ruleof (car l))) ($remrule u (car l)))
	  ((mget (car l) '$rule)
	   (zl-remprop (car l) 'expr) (mremprop (car l) '$rule)
	   (delq (car l) $rules 1)))))

(defmfun remalias1 (l aliasp)
  (do ((l l (cdr l)) (u)) ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $aliases))))
	  ((or aliasp (get (car l) 'noun)) (remalias (car l) t))
	  ((setq u (get (car l) 'verb))
	   (zl-remprop (car l) 'verb) (zl-remprop u 'noun)))))

;;in maxmac
;;(DEFMFUN MGET (ATOM IND)
;;  (LET ((PROPS (AND (SYMBOLP ATOM) (GET ATOM 'MPROPS))))
;;    (AND PROPS (GET PROPS IND)))
;;    (AND PROPS (GETf (cdr PROPS) IND))))
 


#-cl
(defun mdefprop fexpr (l) (mputprop (car l) (cadr l) (caddr l)) (car l))





(defmfun mremprop (atom ind)
  (let ((props (get atom 'mprops))) (and props (zl-remprop props ind))))

(defmfun mgetl (atom inds)
  (let ((props (get atom 'mprops))) (and props (getl props inds))))

(defmfun $matrix n
  (if (= n 0)
      (ncons '($matrix))
      (let ((l (listify n)))
	(dolist (row l)
	  (if (not ($listp row)) (merror "Invalid matrix row:~%~M" row)))
	(matcheck l)
	(cons '($matrix) l))))

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
     (setq iteml (aref ary
		       (setq lispsub
			     (f+ 3 (fixnum-remainder
				    (hasher sub) (aref ary 0))))))
     a    (cond ((null iteml) (go b))
		((alike (caar iteml) sub) (return (cdar iteml))))
     (setq iteml (cdr iteml))
     (go a)
     b    (cond (evarrp (throw 'evarrp 'notexist))
		((null (setq y (arrfunp (caar form)))) (return (meval2 sub form))))
     (setq y (arrfuncall y sub form))
     (setq ary (symbol-array (mget (caar form) 'hashar)))
     (setq iteml (aref ary (setq lispsub (f+ 3 (fixnum-remainder (hasher sub) (aref ary 0))))))
     (setq sub (ncons (cons sub y)))
     (cond (iteml (nconc iteml sub)) (t (store (aref ary lispsub) sub)))
     (store (aref ary 1) (setq nitems (f1+ (aref ary 1))))
     (cond ((> nitems (setq ncells (aref ary 0)))
	    (arraysize (caar form) (f+ ncells ncells))))
     (return y)))

;; Types of FIXNUM and FLONUM herein not currently compatible 
;; on LISP machine.  Don't worry about it for now.
(defun arrfind (form)
  (let ((sub (cdr form)) u v type)
    (setq v (dimcheck (caar form) sub nil))
    (cond (v (setq type (car (arraydims (mget (caar form) 'array))))))
    (cond ((and v (prog2 (setq u (apply 'aref (symbol-array
					       (mget (caar form) 'array))
					sub))
		      (cond ((eq type 'flonum) (not (= u flounbound)))
			    ((eq type 'fixnum) (not (= u fixunbound)))
			    (t (not (eq u munbound))))))
	   u)
	  (evarrp (throw 'evarrp 'notexist))
	  ((or (not v) (null (setq u (arrfunp (caar form)))))
	   (cond ((eq type 'flonum) 0.0)
		 ((eq type 'fixnum) 0)
		 (t (meval2 sub form))))
	  (t (setq u (arrfuncall u sub form))
	     (setf (apply #'aref (symbol-array (mget (caar form) 'array))
			  sub) u)
	     
	     u))))


;;#+cl
;;(defmacro $array (ar typ &rest dims)
;;   (setq ar (make-array dims :initial-element init))
;;   (cond ((


(defmspec $array (x) (setq x (cdr x))
	  (cond ($use_fast_arrays
	  
		 (mset (car x) (apply '$make_array '$any
				      (mapcar #'1+ (cdr x)))))
		((symbolp (car x))
		 (funcall #'(lambda (compp)
			      (funcall #'(lambda (fun diml funp old new ncells)
					   (cond ((memq '$function diml)
						  (setq diml (delq '$function (copy-top-level diml) 1) funp t)))
					   (setq diml (mapcar #'meval diml))
					   (cond ((null diml) (wna-err '$array))
						 ((> (length diml) 5) (merror "`array' takes at most 5 indices"))
						 ((memq nil (mapcar #'(lambda (u) (eq (ml-typep u) 'fixnum)) diml))
						  (merror "Non-integer dimension - `array'")))
					   (setq diml (mapcar #'1+ diml))
					   (setq new (apply #'*array (cons (if compp fun (gensym))
									   (cons t diml))))
					   (cond ((eq compp 'fixnum) (fillarray new '(0)))
						 ((eq compp 'flonum) (fillarray new '(0.0))))
					   (cond ((not (memq compp '(fixnum flonum))) (fillarray new (list munbound)))
						 ((or funp (arrfunp fun))
						  (fillarray new (list (cond ((eq compp 'fixnum) fixunbound)
									     (t flounbound))))))
					   (cond ((null (setq old (mget fun 'hashar)))
						  (mputprop fun new 'array))
						 (t (cond ((not (= (afuncall old 2) (length diml)))
							   (merror "Array ~:M already has ~:M dimension(s)"
								   fun (afuncall old 2))))
						    (setq ncells (f+ 2 (afuncall old 0)))
						    (do ((n 3 (f1+ n))) ((> n ncells))
						      (do ((items (afuncall old n) (cdr items))) ((null items))
							(do ((x (caar items) (cdr x)) (y diml (cdr y)))
							    ((null x)
							     (if (and (memq compp '(fixnum flonum))
								      (not (eq (ml-typep (cdar items)) compp)))
								 (merror "Element and array type do not match:~%~M"
									 (cdar items)))

							     (setf (apply #'aref
									  (symbol-array new)
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
			  (assq (cadr x) '(($complete . t) ($integer . fixnum) ($fixnum . fixnum)
					   ($float . flonum) ($flonum . flonum)))))
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
                                           array, Maxima list, or a matrix"
				       (caar l)))))
			(t(cond ((eq tem (caar l))
				 (meval* `((mset) ,(caar l)
					   ,(make-equal-hash-table
					     (cdr (mevalargs (cdr l))))))
				 (arrstore l r))
				(t
				 (error "The value of ~A is not a hash-table , an array,a Maxima list, or a matrix" (caar l))))
			  ))))
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
		  (if (and (memq (setq fun (car (arraydims ary))) '(fixnum flonum))
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
						  (f+ 3 (fixnum-remainder (hasher sub) (afuncall ary 0))))))
		  (do ((hashl1 hashl (cdr hashl1)))
		      ((null hashl1)
		       (cond ((not (eq r munbound))
			      (setq sub (ncons (cons sub r)))
			      (cond ((null hashl) (store (afuncall ary lispsub) sub))
				    (t (nconc hashl sub)))
			      (store (afuncall ary 1) (f1+ (afuncall ary 1))))))
		    (cond ((alike (caar hashl1) sub)
			   (cond ((eq r munbound) (store (afuncall ary 1)
							 (f1- (afuncall ary 1))))
				 (t (nconc hashl (ncons (cons sub r)))))
			   (store (afuncall ary lispsub) (zl-delete (car hashl1) hashl 1))
			   (return nil))))
		  (if (> (afuncall ary 1) (afuncall ary 0))
		      (arraysize fun (f* 2 (afuncall ary 0))))
		  r)
		 ((and (eq fun 'mqapply) (mxorlistp (setq ary (meval (cadr l))))
		       (prog2 (setq mqapplyp t l (cdr l)) nil)))
		 
		 ((and (not mqapplyp)
		       (or (not (boundp fun)) (not (or (mxorlistp (setq ary (symbol-value fun)))
						       (eq (ml-typep ary) 'array)))))
		  (if (memq fun '(mqapply $%)) (merror "Illegal use of :"))
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
			   (rplaca (ncdr (cdr ary) index) r))
			  (t (merror "~A - index out of range" index))))
		  r)
		 (t (if (not (= (length l) 3))
			(merror "Wrong number of indices:~%~M" (cons '(mlist) (cdr l))))
		    ($setelmx r (meval (cadr l)) (meval (caddr l)) ary)
		    r))))))
		   

#-cl
(defmfun arrstore (l r)
  (let ((fun ($verbify (caar l))) ary sub (lispsub 0) hashl mqapplyp)
    (cond ((setq ary (mget fun 'array))
	   (when (mfilep ary)
	     (i-$unstore (ncons fun)) (setq ary (mget fun 'array)))
	   (dimcheck fun (setq sub (mapcar #'meval (cdr l))) t)
	   (if (and (memq (setq fun (car (arraydims ary))) '(fixnum flonum))
		    (not (eq (ml-typep r) fun)))
	       (merror "Improper assignment to complete array:~%~M" r))
	   #-lispm(eval (list 'store (cons ary sub) (list 'quote r)))
	   )
	  ((setq ary (mget fun 'hashar))
	   (when (mfilep ary)
	     (i-$unstore (ncons fun)) (setq ary (mget fun 'hashar)))
	   (if (not (= (afuncall ary 2) (length (cdr l))))
	       (merror "Array ~:M has dimension ~:M; it was called by ~:M"
		       fun (afuncall ary 2) l))
	   (setq sub (mapcar #'meval (cdr l)))
	   (setq hashl (afuncall ary (setq lispsub (f+ 3 (fixnum-remainder (hasher sub) (afuncall ary 0))))))	  (do ((hashl1 hashl (cdr hashl1)))
														      ((null hashl1)
														       (cond ((not (eq r munbound))
															      (setq sub (ncons (cons sub r)))
															      (cond ((null hashl) (store (afuncall ary lispsub) sub))
																    (t (nconc hashl sub)))
															      (store (afuncall ary 1) (f1+ (afuncall ary 1))))))
														    (cond ((alike (caar hashl1) sub)
															   (cond ((eq r munbound) (store (afuncall ary 1)
																			 (f1- (afuncall ary 1))))
																 (t (nconc hashl (ncons (cons sub r)))))
															   (store (afuncall ary lispsub) (zl-delete (car hashl1) hashl 1))
															   (return nil))))
	   (if (> (afuncall ary 1) (afuncall ary 0))
	       (arraysize fun (f* 2 (afuncall ary 0))))
	   r)
	  ((and (eq fun 'mqapply) (mxorlistp (setq ary (meval (cadr l))))
		(prog2 (setq mqapplyp t l (cdr l)) nil)))
	 
	  ((and (not mqapplyp)
		(or (not (boundp fun)) (not (or (mxorlistp (setq ary (symbol-value fun)))
						(eq (ml-typep ary) 'array)))))
	   (if (memq fun '(mqapply $%)) (merror "Illegal use of :"))
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
		    (rplaca (ncdr (cdr ary) index) r))
		   (t (merror "~A - index out of range" index))))
	   r)
	  (t (if (not (= (length l) 3))
		 (merror "Wrong number of indices:~%~M" (cons '(mlist) (cdr l))))
	     ($setelmx r (meval (cadr l)) (meval (caddr l)) ary)
	     r)))) 

(defun arrfunp (x)
  (or (and $transrun (getl x '(a-expr))) (mgetl x '(aexpr))))

;;#-cl
;;(defmacro system-subrcall* (p argl) p argl
;;  (cond ((status feature maclisp)
;;	 `(subrcall* ,p ,argl))
;;	(t
;;	 `(MAXIMA-ERROR '|Don't think I can A-SUBR frobulate here!|))))

;;#+lispm
;;(defmacro system-subrcall* (p argl) p argl
;;  (cond
;;    #-cl
;;    ((status feature maclisp)
;;	 `(subrcall* ,p ,argl))
;;       (t
;;	 `(error "Don't think I can `a-subr' frobulate here!"))))

#-(or cl nil)
(defmacro assemble-subrcall* ()
  (cond ((status feature maclisp)
	 (cond ((status feature pdp10)
		'(progn 'compile
		  (setplist '|the subr| '(subr nil))
		  (lap-a-list
		   '((lap subrcall* subr) 
		     (args subrcall* (()  . 2)) 
		     (hrrz 3 '|the subr|)
		     (hrrz 4 0 3) 
		     (hrlm 1 0 4) 
		     (movei 1 '|the subr|) 
		     (jcall 2 '*apply) 
		     ()  ))))
	       (t
		;; the above optimizes out the JSP PDLNMK
		;; which is not needed since we know the first argument
		;; is NOT a number. We are more interested in
		;; illustrating the issue than in bumming out
		;; a couple instructions, however there it is.
		'(progn 'compile
		  (setplist '|the subr| '(subr nil))
		  (defun subrcall* (p argl)
		    (rplaca (cdr (symbol-plist '|the subr|)) p)
		    (apply #'|the subr| argl))))))
	(t nil)))
#-(or cl nil)
(assemble-subrcall*)
							
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

(defun hasher (l) ; This is not the best way to write a hasher.  But, 
  (if (null l)	  ; please don't change this code or you're liable to 
      0					; break SAVE files.
      (logand #o77777
	      (let ((x (car l)))
		(cond (($ratp x) (merror "Subscripts may not be in CRE form."))
		      (#+nil (ml-typep x '(or fixnum double-float))
			     #-nil (or (fixnump x) (floatp x))
			     (f+ (if (fixnump x) x (fix (+$ x 0.0005)))
				 (f* 7 (hasher (cdr l)))))
		      ((atom x) (f+ (sxhash x) (hasher (cdr l))))
		      (t (f+ 1 (sxhash (caar x)) (hasher (cdr x))
			     (hasher (cdr l)))))))))

(defun arraysize (fun n)
  (prog (old new indx ncells cell item i y)
     (setq old (symbol-array (mget fun 'hashar)))
     (mputprop fun (setq new (gensym)) 'hashar)
     (*array new t (f+ n 3))
     (setq new (symbol-array new))
     (store (aref new 0) n)
     (store (aref new 1) (aref old 1))
     (store (aref new 2) (aref old 2))
     (setq indx 2 ncells (f+ 2 (aref old 0)))
     a    (if (> (setq indx (f1+ indx)) ncells) (return t))
     (setq cell (aref old indx))
     b    (if (null cell) (go a))
     (setq i (f+ 3 (fixnum-remainder (hasher (car (setq item (car cell)))) n)))
     (if (setq y (aref new i))
	 (nconc y (ncons item))
	 (store (aref new i) (ncons item)))
     (setq cell (cdr cell))
     (go b)))

(defun dimcheck (ary sub fixpp)
  (do ((x sub (cdr x)) (ret t) (y (cdr (arraydims (mget ary 'array))) (cdr y)))
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
			(memq (caaar l) '($ev $funmake $arraymake)))
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
		      (memq 'array (cdar fun))))
	     (merror "Improper function definition:~%~M" fun))
	    (mqdef (if (or (atom (cadr fun))
			   (not (setq ary (memq 'array (cdaadr fun)))))
		       (merror "Improper function definition:~%~M" (cadr fun)))
		   (setq subs (cdadr fun) args (cddr fun) fun (cadr fun)
			 fnname ($verbify (caar fun)))
		   (if (and (not (mgetl fnname '(hashar array)))
			    (get fnname 'specsimp))
		       (mtell "Warning - you are redefining the Maxima ~
			    subscripted function ~:M.~%"
			      fnname)))
	    ((prog2 (setq fnname ($verbify (caar fun)))
		 (or (mopp fnname) (memq fnname '($all $allbut $%))))
	     (merror "Improper function name: ~:@M" fnname))
	    ((setq ary (memq 'array (cdar fun))) (setq subs (cdr fun)))
	    (t (setq args (cdr fun) redef (mredef-check fnname))))
      (if (not ary) (remove1 (ncons fnname) 'mmacro t $macros t))
      (mdefchk fnname (or args (and (not mqdef) subs)) ary mqdef)
      (if (not (eq fnname (caar fun))) (rplaca (car fun) fnname))
      (cond ((not ary) (if (and evp (memq fnname (car loclist)))
			   (mputprop fnname t 'local-fun)
			   (remove-transl-fun-props fnname))
	     (add2lnc (cons (ncons fnname) args) $functions)
	     (set-lineinfo fnname (cadar fun) body)
	     (mputprop fnname (mdefine1 args body) 'mexpr)
	     ;;		    #+MacLisp
	     ;;		    (IF (NOT REDEF)
	     ;;			(ARGS FNNAME (IF (NOT (MGET FNNAME 'MLEXPRP))
	     ;;					 (CONS NIL (LENGTH ARGS)))))
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
		 (if (not (or ($constantp u) (char= (getcharn u 1) #\&)))
		     (return t))))
 	 (arrstore (cons (ncons fun) subs) (mdefine1 args body)))
	(t (mdefchk fun subs t nil)
	   (mputprop fun (mdefine1 subs (mdefine1 args body)) 'aexpr))))

(defmfun mspecfunp (fun) (and (or (getl-fun fun '(fsubr fexpr macro))
				  (getl fun '(mfexpr* mfexpr*s)) 	 (and $transrun (get fun
											     'translated-mmacro)) 	 (mget fun 'mmacro)) (not (get fun 'evok))))

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
	(merror "Improper parameter in function definition for ~:M:~%~M"
		fun (car l)))))

(defun mdefparam (x)
  (and (atom x) (not (maxima-constantp x)) (not (char= (getcharn x 1) #\&))))

(defun mdeflistp (l)
  (and (null (cdr l)) ($listp (car l)) (cdar l) (null (cddar l))))

(defmfun mopp (fun)
  (and (not (eq fun 'mqapply))
       (or (mopp1 fun)
	   (and (get fun 'operators) (not (rulechk fun))
		(not (memq fun rulefcnl)) (not (get fun 'opers))))))

(defmfun mopp1 (fun)
  (declare (object fun))
  (and (setq fun (get fun 'op)) (not (memq fun (cdr $props)))))

;; maybe should have a separate version, or a macro..
(defun mapply (a b c ) (mapply1 a b c nil))

;;(DEFMFUN $CALL FEXPR (L)
;;  (IF (NULL L) (MERROR "Wrong number of args to `call'"))
;;  (MEVAL (CONS (NCONS (CAR L)) (CDR L))))

;;(DEFMFUN $ACALL FEXPR (L)
;;  (IF (NULL L) (MERROR "Wrong number of args to `acall'"))
;;  (MEVAL (CONS (CONS (CAR L) '(ARRAY)) (CDR L))))

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

(defmspec $dispfun (l) (setq l (cdr l))
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

(defmspec $fundef (x) (consfundef (fexprcheck x) nil nil))

(defun consfundef (x maexprp stringp)
  (prog (arryp name fun)
     (setq arryp (and (not (atom x)) (not (eq (caar x) 'mqapply)) (memq 'array (cdar x))))
     (cond ((atom x) (setq name ($verbify x)
			   fun (or (and (not maexprp) (mgetl name '(mexpr mmacro)))
				   (mgetl name '(aexpr)))))
	   (arryp (setq fun (meval1 (setq name (cons (list ($verbify (caar x)) 'array) (cdr x)))))
		  (if (or (atom fun) (not (eq (caar fun) 'lambda))) (setq fun nil))))
     (cond ((not fun) (cond (stringp (return x)) ((memq 'edit state-pdl) (terpri)))
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
  (and (boundp x) (not (memq x (cdr $values))) (not (memq x (cdr $labels)))))

(defmspec mcond (form) (setq form (cdr form))
	  (do ((u form (cddr u)) (v))
	      ((null u) nil)
	    (cond ((eq (setq v (mevalp (car u))) t) (return (meval (cadr u))))
		  (v (return (list* '(mcond) v (mapcar #'meval-atoms (cdr u))))))))

(defun meval-atoms (form) 
  (cond ((atom form) (meval1 form))
	((eq (caar form) 'mquote) (cadr form))
	((and (or (getl-fun (caar form) '(fsubr fexpr))
		  (getl (caar form) '(mfexpr* mfexpr*s)))
	      (not (memq (caar form) '(mcond mand mor mnot mprogn mdo mdoin))))
	 form)
	(t (recur-apply #'meval-atoms form))))

(defmspec mdo (form) (setq form (cdr form))
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

(defmspec mdoin (form) (setq form (cdr form))
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

(defmspec mprog (prog) (setq prog (cdr prog))
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
			      ((not (setq prog (zl-member x mprogp)))
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

(defmspec $subvar (l) (setq l (cdr l))
	  (if (null l) (wna-err '$subvar)) (meval (cons '(mqapply array) l)))

(defmfun rat (x y) `((rat simp) ,x ,y))

(defmfun $exp (x) `((mexpt) $%e ,x))

(defmfun $sqrt (x) `((%sqrt) ,x))


(defmfun add2lnc (item llist)
  (when (not (memalike item (if ($listp llist) (cdr llist) llist)))
    (if (not (atom item)) (zl-delete (zl-assoc (car item) llist) llist 1))
    (nconc llist (ncons item))))

(defmfun bigfloatm* (bf)
  (if (not (memq 'simp (cdar bf)))
      (setq bf (cons (list* (caar bf) 'simp (cdar bf)) (cdr bf))))
  (if $float ($float bf) bf))

(defmfun $allbut n (cons '($allbut) (listify n)))

(defmfun mfilep (x)
  (and (not (atom x)) (not (atom (car x))) (eq (caar x) 'mfile)))

;;#-(or NIL cl)
;;(DEFMFUN DSKSETQ FEXPR (L) (LET ((DSKSETP T)) (MSET (CAR L) (EVAL (CADR L)))))

(defquote dsksetq (&rest l) (let ((dsksetp t)) (mset (car l) (eval (cadr l)))))

(defmfun dskrat (x)
  (orderpointer (caddar x))
  (mapc #'(lambda (a b) (dskrat-subst a b (cddddr (car x))) ; for TAYLOR forms
		  (dskrat-subst a b (cdr x)))
	genvar (cadddr (car x)))
  (rplaca (cdddar x) genvar)
  #-(or cl nil) (gctwa) 
  (if (memq 'trunc (car x)) (srconvert x) x)) ; temporary

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
  (let (($numer t) ($float t)) (simplify (list '(%binomial) x y))))

(prog1 '(evfun properties)
  (mapc #'(lambda (x) (putprop x t 'evfun))
	'($radcan $factor $ratsimp $trigexpand $trigreduce $logcontract
	  $rootscontract $bfloat $ratexpand $fullratsimp $rectform
	  $polarform)))

(prog1 '(evflag properties)
  (mapc #'(lambda (x) (putprop x t 'evflag))
	'($exponentialize $%emode $demoivre $logexpand $logarc $lognumer
	  $radexpand $keepfloat $listarith $float $ratsimpexpons $ratmx
	  $simp $simpsum $simpproduct $algebraic $ratalgdenom $factorflag $ratfac
	  $infeval $%enumer $programmode $lognegint $logabs $letrat
	  $halfangles $exptisolate $isolate_wrt_times $sumexpand
	  $cauchysum $numer_pbranch $m1pbranch $dotscrules
	  $trigexpand)))

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

;; Undeclarations for the file:
;;(declare-top (NOTYPE N I J NNEED NGIVEN NCELLS NITEMS LISPSUB INDX EVFLG))




