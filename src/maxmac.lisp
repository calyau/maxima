;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module maxmac #-lispm macro)

;; This file contains miscellaneous macros used in Macsyma source files.
;; This file must run and compile in PDP10 Lisp, Multics Lisp, Franz Lisp,
;; and LMLisp.

;; General purpose macros which are used in Lisp code, but not widely enough
;; accepted to be a part of Lisp systems.

;; For evaluable declarations placed in macro files. This is a DWIM form
;; saying "evaluate this form if you think it matters." If we tried hard
;; we could come up with a better way to actually do it. -gjc

(defmacro for-declarations (&rest l)
  `(map-eval-for-declarations ',l))

(defun map-eval-for-declarations (l) (mapc #'eval-for-declarations l))

(defun eval-for-declarations (form)
  (if (and (not (atom form))
	   (symbolp (car form))
	   ;; we want an fboundp which gives T for special forms too.
	   (or (fboundp (car form))
	       #+nil (si:macro-definition (car form))
	       #+nil (eq (car form) 'special)))
      (eval form)))


;;(defmacro optimizing-declarations (dcls &body body) dcls
;;  #+NIL `(locally (declare (optimize ,@dcls)) ,@body)
;;  #-NIL `(progn ,@body))

;; All these updating macros should be made from the same generalized
;; push/pop scheme as I mentioned to LispForum. As they are defined now
;; they have inconsistent return-values and multiple-evaluations of
;; arguments. -gjc

(defmacro addl (item list)
  `(or (memq ,item ,list) (setq ,list (cons ,item ,list))))

#-multics (progn 'compile
		 (defmacro increment (counter &optional increment)
		   (if increment
		       `(setf ,counter (f+ ,counter ,increment))
		       `(setf ,counter (f1+ ,counter))))


		 (defmacro decrement (counter &optional decrement)
		   (if decrement
		       `(setf ,counter (f- ,counter ,decrement))
		       `(setf ,counter (f1- ,counter))))

		 (defmacro complement (switch)
		   `(setf ,switch (not ,switch)))

		 ) ;; End of Lispm conditionalization.


;; 'writefilep' and 'ttyoff' are system independent ways of expressing
;; the Maclisp ^R and ^W.
;; In Franz Lisp, we make writefilep equivalent to ptport, which isn't
;; exactly correct since ptport is not just a boolean variable.  However
;; it works in most cases.  
;;
(eval-when (compile eval load)
  (defvar writefilep #-franz '^r #+franz 'ptport)
  (defvar ttyoff    '^w))

;; (IFN A B) --> (COND ((NOT A) B))
;; (IFN A B C D) --> (COND ((NOT A) B) (T C D))
;; (IFN A B) is equivalent to (OR A B) as (IF A B) is equivalent to (AND A B).

(defmacro ifn (predicate then . else)
  (cond ((null else) `(cond ((not ,predicate) ,then)))
	(t `(cond ((not ,predicate) ,then) (t . ,else)))))

(defmacro fn (bvl &rest body)
  `(function (lambda ,bvl . ,body)))

;; Like PUSH, but works at the other end.

(defmacro tuchus (list object)
  `(setf ,list (nconc ,list (ncons ,object))))

;; Copy a single cons, the top level and all levels (repectively) of a piece of
;; list structure.  Something similar for strings, structures, etc. would be
;; useful.  These functions should all be open-coded subrs.

(defmacro copy-cons (cons)
  (if (atom cons)
      `(cons (car ,cons) (cdr ,cons))
      (let ((var (gensym)))
	`(let ((,var ,cons)) `(cons (car ,var) (cdr ,var))))))

(defmacro copy-top-level (list)
  #+(or cl nil) `(copy-list ,list)
  #-(or cl nil) `(append ,list nil))

;; (DEFMACRO COPY-ALL-LEVELS (LIST)
;;   #+(or cl NIL) `(COPY-TREE ,LIST)
;;   #-(or lispm NIL) `(SUBST NIL NIL ,LIST))

;; Old names kept around for compatibility.

(defmacro copy1* (list)
  #+(or cl nil) `(copy-list ,list)
  #-(or cl nil) `(append ,list nil))
(defmacro copy1 (list)
  #+(or cl nil) `(copy-list ,list)
  #-(or cl nil) `(append ,list nil))
#-franz
(defmacro copy (list)
  #+(or cl nil  symbolics) `(copy-tree ,list)
  #-(or cl nil symbolics) `(subst nil nil ,list))

;; Use this instead of GETL when looking for "function" properties,
;; i.e. one of EXPR, SUBR, LSUBR, FEXPR, FSUBR, MACRO.
;; Use FBOUNDP, SYMBOL-FUNCTION, or FMAKUNBOUND if possible.

(defmacro getl-fun (fun l)
  #+maclisp `(getl ,fun ,l)
  #+cl   `(getl-lm-fcn-prop ,fun ,l)
  #+franz   `(getl-franz-fcn-prop ,fun ,l)
  #+nil     `(getl-nil-fcn-prop ,fun ,l)
  )

;; Non-destructive versions of DELQ and DELETE.  Already part of NIL
;; and LMLisp.  These should be rewritten as SUBRS and placed
;; in UTILS.  The subr versions can be more memory efficient.

;;#-(OR Lispm NIL Multics Franz cl)
;;(DEFMACRO REMQ (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
;;	  (IF COUNTING? `(DELQ ,ITEM (APPEND ,LIST NIL) ,COUNT)
;;	      `(DELQ ,ITEM (APPEND ,LIST NIL))))


(defmacro remq (item list &optional (count () counting?))
  `(remove ,item ,list :test 'eq ,@ (and counting? `(:count ,count))))

;;#+cl ;in clmacs
;;(DEFMACRO ZL-REMOVE (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
;; `(remove ,item ,list :test 'equal ,@ (and counting? `(:count ,count))))	

;;#-(OR Lispm NIL Multics Franz)
;;(DEFMACRO zl-REMOVE (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
;;	  (IF COUNTING? `(zl-DELETE ,ITEM (APPEND ,LIST NIL) ,COUNT)
;;	      `(zl-DELETE ,ITEM (APPEND ,LIST NIL))))

#-lispm (defmacro catch-all (form) `(catch nil ,form))

;; (EXCH A B) exchanges the bindings of A and B
;; Maybe it should turn into (PSETF A B B A)?

(defmacro exch (x y) `(setf ,x (prog1 ,y (setf ,y ,x))))

;; These are here for old code only.
;; Use FIFTH rather than CADDDDR.  Better, use DEFSTRUCT.

#-franz (defmacro caddadr (x) `(car (cddadr ,x)))
#-franz (defmacro caddddr (x) `(car (cddddr ,x)))

;; The following is a bit cleaner than the kludgy (PROGN 'COMPILE . <FORMS>)

(defmacro compile-forms (&rest <forms>) `(progn 'compile . ,<forms>))

;; The following macros pertain only to Macsyma.

;; Widely used macro for printing error messages.  We should be able
;; to come up with something better.  On large address space systems
;; this should signal -- hack later.  Soon to be flushed in favor
;; of new Macsyma error system.  Yea!

;; Obsolete.  Use MERROR.

(defmacro erlist (message)
  (maxima-error "ERLIST is obsolete, all calls to it have been removed, so where
	 did you dig this one up loser?" message))

;; All functions are present on non-autoloading systems.  Definition
;; for autoloading systems is in SUPRV.
;; If you have dynamic linking you might as well take advantage of it.

#-(or pdp10 nil)
(defmacro find-function (function) function nil)

;; Facility for loading auxilliary macro files such as RATMAC or MHAYAT.
;; Global macro files are loaded by the prelude file.

#+lispm (defun macro-dir (x) (format nil "LMMAXQ;~A QFASL" x))
#+pdp10 (defun macro-dir (x) `((libmax) ,x))
#+franz (defun macro-dir (x)  (cond ((cdr (zl-assoc x '((rzmac  . "rz//macros")
							(mhayat . "rat//mhayat")
							(ratmac . "rat//ratmac")))))
				    (t (concat "libmax//" x))))
#+nil (defun macro-dir (x) (merge-pathname-defaults x "[VASL]"))

(comment sample definition only on
	 its   see "LIBMAX;MODULE"
	 lispm see "LMMAX;SYSDEF"
	 nil   see   "VAXMAX;VAXCL"
	 multics see "???"
	 franz see "/usr/lib/lisp/machacks.l"
	 ()
	 (defmacro macsyma-module (name &rest options)
	   (maybe-load-macros options)
	   (maybe-load-declarations options)
	   `(eval-when (compile eval load)
	     (print '(loading ,name) msgfiles)
	     (defprop ,name t loaded?)
	     ,@(maybe-have-some-runtime-options options)))
	 )

;; Except on the Lisp Machine, load the specified macro files.
;; On the Lisp Machine, the DEFSYSTEM facility is used for loading
;; macro files, so just check that the file is loaded. This is
;; a useful error check, has saved a lot of time since Defsystem
;; is far from fool-proof. See LMMAX;SYSDEF for the Lispm
;; definition of MACSYMA-MODULE.

#+cl
(defun load-macsyma-macros-at-runtime (&rest l)
  (mapcar #'(lambda (x)
	      (if (get x 'macsyma-module)
		  x 
		  (error  "Missing Maxima macro file -- ~A" x)))
	  l))
#-cl
(defun load-macsyma-macros-at-runtime (&rest l)
  (mapcar #'load-when-needed l))

(defmacro load-macsyma-macros (&rest macro-files)
  `(comment *macro*files*
    ,(apply #'load-macsyma-macros-at-runtime macro-files)))


#+multics
(defmacro find-documentation-file (x)
  (cond ((eq x 'manual)
	 `(let ((filep (probe-file (list (catenate macsyma-dir ">documentation")
					 "macsyma.manual"))))
	   (cond (filep filep)
		 (t (maxima-error "Cannot `maxima-find' the Macsyma manual")))))
	((eq x 'manual-index)
	 `(let ((filep (probe-file (list (catenate macsyma-dir ">documentation")
					 "macsyma.index.lisp"))))
	   (cond (filep filep)
		 (t (maxima-error "Cannot `maxima-find' the Macsyma manual index")))))
	(t (maxima-error "Unknown documentation: " x))))

#+multics
(defmacro load-documentation-file (x)
  `(load (find-documentation-file ,x)))

;;;Reset the stream to its starting position.
(defmacro rewind-stream (stream)
  
  #-(or lispm nil) `(filpos ,stream 0)
  ;;#+LispM          `(send ,stream ':rewind)
  #+cl `(file-position ,stream 0)
  #+nil            `(open ,stream))

;; Used to temporarily bind contexts in such a way as to not cause
;; the context garbage collector to run. Used when you don't want to
;; stash away contexts for later use, but simply want to run a piece
;; of code in a new context which will be destroyed when the code finishes.
;; Note that this code COULD use an unwind-protect to be safe but since
;; it will not cause out and out errors we leave it out.

(defmacro with-new-context (sub-context &rest forms)
  `(let ((context (context ,@sub-context)))
    (prog1 ,@forms
      (context-unwinder))))

;; For creating a macsyma evaluator variable binding context.
;; (MBINDING (VARIABLES &OPTIONAL VALUES FUNCTION-NAME)
;;    ... BODY ...)

(defmacro mbinding (variable-specification &rest body &aux (temp (gensym)))
  `(let ((,temp ,(car variable-specification)))
    ;; Don't optimize out this temporary, even if (CAR VARIABLE-SPECICIATION)
    ;; is an ATOM. We don't want to risk side-effects.
    ,(case (length variable-specification)
	   ((1)
	    `(mbinding-sub ,temp ,temp nil ,@body))
	   ((2)
	    `(mbinding-sub ,temp ,(cadr variable-specification) nil ,@body))
	   ((3)
	    `(mbinding-sub ,temp ,(cadr variable-specification)
	      ,(caddr variable-specification)
	      ,@body))
	   (t
	    (maxima-error "Bad variable specification:" variable-specification)))))

(defvar mbinding-usage
  #+(and pdp10 maclisp)    'prog1
  #+(and multics maclisp)  'unwind-protect
  #+franz                  'prog1
  #+cl                  'unwind-protect
  #+nil                    'unwind-protect
  )
  
(defmacro mbinding-sub (variables values function-name &rest body
			&aux (win (gensym)))
  (case mbinding-usage
    ((prog1)
     `(prog1 (progn (mbind ,variables ,values ,function-name) ,@body)
       (munbind ,variables)))
    ((unwind-protect)
     `(let ((,win nil))
       (unwind-protect
	    (progn (mbind ,variables ,values ,function-name)
		   (setq ,win t)
		   ,@body)
	 (if ,win (munbind ,variables)))))
    ((progv)
     `(let ((,win (mbinding-check ,variables ,values ,function-name)))
       (progv ,variables
	   ,win
	 ,@body)))
    (t
     (maxima-error "Unknown setting of `mbinding-usage'" mbinding-usage))))

#+nil
(defmacro mdefprop (a b c) `(mputprop ',a ',b ',c))

#-franz	;; Franz uses a function definition in COMM.
;; For MLISTP its arg is known not to be an atom.
;; Otherwise, just use $listp.
;; MLISTP exists just to support a Franz hack, so you can just 
;;   ignore it. - JPG
(defmacro mlistp (x) `(eq (caar ,x) 'mlist))

;; How About MTYPEP like (MTYPEP EXP 'TAN) or (MTYPEP EXP '*) - Jim.
;; Better, (EQ (MTYPEP EXP) 'TAN).

(defmacro mtanp (x) 
  `(let ((thing ,x))
    (and (not (atom thing)) (eq (caar thing) '%tan))))

(defmacro matanp (x)
  `(let ((thing ,x))
    (and (not (atom thing)) (eq (caar thing) '%atan))))

;; Macros used in LIMIT, DEFINT, RESIDU.
;; If we get a lot of these, they can be split off into a separate macro
;; package.

(defmacro real-infinityp (x) `(memq ,x real-infinities))

(defmacro infinityp (x) `(memq ,x infinities))

(defmacro real-epsilonp (x) `(memq ,x infinitesimals))

(defmacro free-epsilonp (x)
  `(do ((one-eps infinitesimals (cdr one-eps)))
    ((null one-eps) t)
    (if (not (free (car one-eps) ,x))  (return ()))))

(defmacro free-infp (x)
  `(do ((one-inf infinities (cdr one-inf)))
    ((null one-inf) t)
    (if (not (free (car one-inf) ,x))  (return ()))))

(defmacro inf-typep (x)
  `(car (amongl infinities ,x)))

(defmacro hot-coef (p)
  `(pdis (caddr (cadr (rat-no-ratfac ,p)))))

;; Special form for declaring Macsyma external variables.  It may be used for
;; User level variables, or those referenced by other Lisp programs.

;; Syntax is:
;; (DEFMVAR <name> &OPTIONAL <initial-value> <documentation> . <flags>) See
;; MC:LIBMAX;DEFINE > for complete documentation of syntax.  The code in this
;; file for DEFMVAR is for non-ITS systems only.  LIBMAX;DEFINE contains code
;; for ITS.  Other systems may process the documentation information as they
;; wish.

;; Be sure to expand into DEFVAR and not into (DECLARE (SPECIAL ...)) as
;; certain systems do other things with DEFVAR.  The Lisp Machine, for
;; instance, annotates the file name.  On Multics and the Lisp Machine, expand
;; into DEFCONST since the entire Macsyma system is present before user files
;; are loaded, so there is no need to do the BOUNDP check.
;; What about people who want to subsequently change a value on lispm?
;; Use defconst only if you want something that is hardwired into function definitions
;; as on the lispm.  Also one may want to reload a file to reset some variables,
;; so if *reset-var* is true defmvar will restore the original value on lispm--Wfs
;; definition is in commac.

#-(or franz its lispm cl)
(defmacro defmvar (variable &optional (initial-value nil iv-p) documentation
		   &rest flags &aux definer type)
  documentation flags ;; Ignored certain places.
  (setq definer #+(or multics ) 'defconst
	#-(or multics  ) 'defvar)
  #-(or lispm nil)
  (setq type (cond ((memq 'fixnum flags) 'fixnum)
		   ((memq 'flonum flags) 'flonum)
		   (t nil)))
  #+nil (macsyma-defmvar-declarations variable flags)
  `(progn 'compile
    ,(if iv-p
	 `(,definer ,variable ,initial-value
	   #+nil ,@(and documentation `(,documentation)))
	 `(,definer ,variable #+lispm () ))
    #-nil ,@(if type `((declare (,type ,variable))))))

;;see commac
#-(or cl lispm)
(defmacro defmfun (function &body  rest &aux .n.)
  #+nil (macsyma-defmfun-declarations function rest)
  `(defun ,function  ,rest))

;;#+LISPM
;;(DEFPROP DEFMSPEC "Macsyma special form" SI:DEFINITION-TYPE-NAME)

;; Special form for declaring Macsyma external procedures.  Version for ITS
;; is in LIBMAX;DEFINE.
;; Franz version is in libmax/vdefine.l



#+cl
(defmacro defmspec (function . rest)
  `(progn
    (defun-prop ( ,function mfexpr*) . ,rest)
    #+lispm (si::record-source-file-name ',function 'defmspec)))

;;#+LISPM
;;(DEFMACRO DEFMSPEC (FUNCTION . REST)
;;  `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,FUNCTION DEFMSPEC))
;;     (DEFUN (:PROPERTY ,FUNCTION MFEXPR*) . ,REST)
;;     (SI:RECORD-SOURCE-FILE-NAME ',FUNCTION 'DEFMSPEC)
;;     ))


;;How bout the following for a replacement for the defmspec type.
;;It gives the special form business and seems to work well in the
;;interpreter.  The translated and compiled forms for special functions
;;was a problem any way and we should leave the def%tr forms for
;;$sum etc. and maybe institute some new ones.  
;;Of course meval and translate have to be told what to do with
;;a macro but they needed to be told anyway.--wfs

;;see commac
;;#+lispm
;;(defmacro defmspec (fn (aa) &rest rest &aux ans help )
;;  (setq help (intern (format nil "~A-AUX" fn)))
;;  (setq ans
;;	(list    ;;copy-list aa
;;    `(defmacro ,fn (&rest ,aa &aux e)(setq ,aa (copy-list ,aa))
;;	       (setq e (cons (list ',fn) ,aa))
;;               `(meval* '(,', help  ',e)))
;;    `(defun ,help (,aa) . ,rest)))
;;  `(progn 'compile . , ans))

;;eg.
;;(defmspecial $ssum (l) (setq l (cdr l))
;;  (if (= (length l) 4)
;;      (dosum (car l) (cadr l) (meval (caddr l)) (meval (cadddr l)) t)
;;      (wna-err '$$sum)))


;;;	The following MAUTOLOAD macro makes setting up autoload props for files
;;; on "standard" Macsyma directories easy, and clean. As an example, the
;;; code in SUPRV would look as folllows:
;;;
;;; (MAUTOLOAD (PURCOPY '(FASL DSK MACSYM))
;;;  (LIMIT   $LIMIT $LDEFINT)
;;;  (IRINTE  INTE)
;;;  (MATCOM  $MATCHDECLARE $DEFMATCH $TELLSIMP $TELLSIMPAFTER $DEFRULE)
;;;  (MATRUN  $DISPRULE $REMRULE $APPLY1 $APPLYB1 $APPLY2 $APPLYB2
;;;	      FINDBE FINDFUN FINDEXPON FINDBASE PART+ PART*)
;;;   ...
;;;
;;;  ((LISPT FASL DSK LIBLSP) $TECO $TSTRING $TECMAC $EMACS $EDIT)
;;;
;;;   ... )
;;;
;;;	The reason the file-spec list evals, is so that one may do a PURCOPY as
;;; above, and also one could imagine having a status request here to obtain
;;; the canonical file spec's.
;;;	Note that the first arg must be of the form (FN2 DEV DIR) if a file
;;; mask is being used; this macro could be much more elaborate.

;;#+ITS
;;(DEFMACRO MAUTOLOAD (FN2-DEV-DIR &REST MASTER-LIST)
;;  `(DOLIST (L ',MASTER-LIST)
;;     (DO ((FILE (IF (ATOM (CAR L))
;;		    (CONS (CAR L) ,FN2-DEV-DIR)
;;		    (CAR L)))
;;	  (FUNLIST (CDR L) (CDR FUNLIST)))
;;	 ((NULL FUNLIST))
;;       (PUTPROP (CAR FUNLIST) FILE 'AUTOLOAD))))

#-multics
(defmacro sys-defaultf (x) `(defaultf ,x))
;;; For #+Multics a function definition for SYS-DEFAULTF can be found 
;;; in SUPRV.

(defmacro sys-user-id ()
  #+franz '(getenv '|USER|)
  #+lispm 'user-id
  #+multics '(status uname)
  #-(or franz multics lispm) '(status userid))

(defmacro sys-free-memory ()
  #-(or multics lispm) '(status memfree)
  #+(or multics lispm) 10000.)	    ;This should look at the pdir size
					;and mung it to give a good approximation.

;; Setf hacking.
;;
;;
;;(defsetf GET ((() sym tag) value) T 
;;   (eval-ordered* '(nsym ntag nvalue)
;;		  `(,sym ,tag ,value)
;;		  '`((PUTPROP ,nsym ,nvalue ,ntag))))

;;#+PDP10
;;(defsetf MGET ((() sym tag) value) T 
;;  (eval-ordered* '(nsym ntag nvalue)
;;		 `(,sym ,tag ,value)
;;		 '`((MPUTPROP ,nsym ,nvalue ,ntag))))

;;#+PDP10
;;(defsetf $GET ((() sym tag) value) T 
;;  (eval-ordered* '(nsym ntag nvalue)
;;		 `(,sym ,tag ,value)
;;		 '`(($PUT ,nsym ,nvalue ,ntag))))

;;#+Franz
;;(defsetf mget (expr value)
;;   `(mputprop ,(cadr expr) ,value ,(caddr expr)))

;;#+Franz
;;(defsetf $get (expr value)
;;   `($put ,(cadr expr) ,value ,(caddr expr)))

;;#+NIL
;;(DEFPROP MGET SETF-MGET SI:SETF-SUBR)
;;#+NIL
;;(DEFPROP $GET SETF-$GET SI:SETF-SUBR)

;;;DIFFERENT version of setf on Multics and LM ...Bummer... -JIM 3/4/81
;;#+MULTICS
;;(defsetf MGET (sym tag) value
;;  `(MPUTPROP ,sym ,value ,tag))

(defmfun mget (atom ind)
  (let ((props (and (symbolp atom) (get atom 'mprops))))
    (and props (getf (cdr props) ind))))

#+(or cl ti)
(defsetf mget (sym tag) (value)
  `(mputprop ,sym ,value ,tag))

(defmacro old-get (plist tag)
  `(getf (cdr ,plist) ,tag))

;;#+ MULTICS
;;(defsetf $GET (sym tag) value
;;  `($PUT ,sym ,value ,tag))

(defmfun $get (atom ind) (prop1 '$get atom nil ind))

#+(or cl ti)
(defsetf $get (sym tag) (value)
  `($put ,sym ,value ,tag))
;;
;;#+(and LISPM (not (or cl ti)))
;;(DEFUN (:PROPERTY MGET SI:SETF) (REF VAL)
;;  `(MPUTPROP ,(SECOND REF) ,VAL ,(THIRD REF)))
;;
;;#+(and LISPM (not (or cl ti)))
;;(DEFUN (:PROPERTY $GET SI:SETF) (REF VAL)
;;  `($PUT ,(SECOND REF) ,VAL ,(THIRD REF)))

(defmacro initialize-random-seed ()
  ;;  #+(or PDP10 NIL) '(sstatus random 0)
  #+cl () ;;(si:random-initialize si:random-array) obsolete. what now?
  )

;; These idiot macros are used in some places in macsyma.
;; The LISPM doesn't "go that high" with the series. DO NOT USE THESE
;; in new code. -gjc
;; NIL (common-lisp) has the nth accessors through to tenth, the rest
;; frobs through to rest5.  However i had thought that the latter were
;; obsolete, and had been going to flush them. --gsb
#-(or cl ti nil)
(defmacro eighth  (form) `(cadddr (cddddr ,form)))
#-(or cl ti nil)
(defmacro ninth   (form) `(car (cddddr (cddddr ,form))))
#-(or cl ti nil)
(defmacro tenth	  (form) `(cadr (cddddr (cddddr ,form))))
#-nil
(defmacro rest5 (form) `(cdr (cddddr ,form)))
(defmacro rest6 (form) `(cddr (cddddr ,form)))

;;; We should probably move these into the compatibility package on
;;; multics.

(defmacro *break (breakp mess)
  `(apply 'break `(,,mess ,',breakp)))

;;; To satisfy GJC's speed mainia I resisted changing these in the
;;; code. -Jim.

;;#+Multics
;;(defmacro +tyi (&rest args)
;;  `(tyi ,@args))

;;#+Multics 
;;(defmacro +tyo (&rest args)
;;  `(tyo ,@args))

;;;; Let the compiler know that x is a fixnum. I guess it will also
;;;; then optimize the call to +.
;;#+Multics
;;(defmacro fixnum-identity (x)
;;  `(f+ ,x))

;;this was not called.
;;(defmacro get-symbol-array-pointer (x)
;;  #+franz `(getd ,x)
;;  #+nil `(si:get-symbol-array-pointer ,x)
;;  #+cl `(symbol-array ,x)
;;  #+maclisp `(get ,x 'array))


(defmacro  mdefprop (sym val indicator)
  `(mputprop ',sym ',val ',indicator))


(defmfun mputprop (atom val ind)
  (let ((props (get atom 'mprops)))
    (if (null props) (putprop atom (setq props (ncons nil)) 'mprops))
    (putprop props val ind)))
