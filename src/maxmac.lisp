;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
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
	   (OR (fboundp (car form))
	       #+NIL (SI:MACRO-DEFINITION (CAR FORM))
	       #+NIL (EQ (CAR FORM) 'SPECIAL)))
      (eval form)))


(defmacro optimizing-declarations (dcls &body body) dcls
  #+NIL `(locally (declare (optimize ,@dcls)) ,@body)
  #-NIL `(progn ,@body))

;; All these updating macros should be made from the same generalized
;; push/pop scheme as I mentioned to LispForum. As they are defined now
;; they have inconsistent return-values and multiple-evaluations of
;; arguments. -gjc

(DEFMACRO ADDL (ITEM LIST)
	  `(OR (MEMQ ,ITEM ,LIST) (SETQ ,LIST (CONS ,ITEM ,LIST))))

#-Multics (PROGN 'COMPILE


(DEFMACRO INCREMENT (COUNTER &OPTIONAL INCREMENT)
  (IF INCREMENT
      `(SETF ,COUNTER (f+ ,COUNTER ,INCREMENT))
      `(SETF ,COUNTER (f1+ ,COUNTER))))


(DEFMACRO DECREMENT (COUNTER &OPTIONAL DECREMENT)
  (IF DECREMENT
      `(SETF ,COUNTER (f- ,COUNTER ,DECREMENT))
      `(SETF ,COUNTER (f1- ,COUNTER))))

(DEFMACRO COMPLEMENT (SWITCH) `(SETF ,SWITCH (NOT ,SWITCH)))

) ;; End of Lispm conditionalization.


;; 'writefilep' and 'ttyoff' are system independent ways of expressing
;; the Maclisp ^R and ^W.
;; In Franz Lisp, we make writefilep equivalent to ptport, which isn't
;; exactly correct since ptport is not just a boolean variable.  However
;; it works in most cases.  
;;
(eval-when (compile eval load)
   (defvar writefilep #-Franz '^R #+Franz 'ptport)
   (defvar ttyoff    '^W))

;; (IFN A B) --> (COND ((NOT A) B))
;; (IFN A B C D) --> (COND ((NOT A) B) (T C D))
;; (IFN A B) is equivalent to (OR A B) as (IF A B) is equivalent to (AND A B).

(DEFMACRO IFN (PREDICATE THEN . ELSE)
	  (COND ((NULL ELSE) `(COND ((NOT ,PREDICATE) ,THEN)))
		(T `(COND ((NOT ,PREDICATE) ,THEN) (T . ,ELSE)))))

(DEFMACRO FN (BVL &REST BODY)
	  `(FUNCTION (LAMBDA ,BVL . ,BODY)))

;; Like PUSH, but works at the other end.

(DEFMACRO TUCHUS (LIST OBJECT)
	  `(SETF ,LIST (NCONC ,LIST (NCONS ,OBJECT))))

;; Copy a single cons, the top level and all levels (repectively) of a piece of
;; list structure.  Something similar for strings, structures, etc. would be
;; useful.  These functions should all be open-coded subrs.

(DEFMACRO COPY-CONS (CONS)
  (IF (ATOM CONS)
      `(CONS (CAR ,CONS) (CDR ,CONS))
      (LET ((VAR (GENSYM)))
	   `(LET ((,VAR ,CONS)) `(CONS (CAR ,VAR) (CDR ,VAR))))))

(DEFMACRO COPY-TOP-LEVEL (LIST)
  #+(or cl NIL) `(COPY-LIST ,LIST)
  #-(or cl NIL) `(APPEND ,LIST NIL))

(DEFMACRO COPY-ALL-LEVELS (LIST)
  #+(or cl NIL) `(COPY-TREE ,LIST)
  #-(or lispm NIL) `(SUBST NIL NIL ,LIST))

;; Old names kept around for compatibility.

(DEFMACRO COPY1* (LIST)
  #+(or cl NIL) `(COPY-LIST ,LIST)
  #-(or cl NIL) `(APPEND ,LIST NIL))
(DEFMACRO COPY1 (LIST)
  #+(or cl NIL) `(COPY-LIST ,LIST)
  #-(or cl NIL) `(APPEND ,LIST NIL))
#-Franz
(DEFMACRO COPY (LIST)
  #+(or cl nil  symbolics) `(COPY-TREE ,LIST)
  #-(or cl nil symbolics) `(SUBST NIL NIL ,LIST))

;; Use this instead of GETL when looking for "function" properties,
;; i.e. one of EXPR, SUBR, LSUBR, FEXPR, FSUBR, MACRO.
;; Use FBOUNDP, SYMBOL-FUNCTION, or FMAKUNBOUND if possible.

(DEFMACRO GETL-FUN (FUN L)
	  #+MacLisp `(GETL ,FUN ,L)
	  #+CL   `(GETL-LM-FCN-PROP ,FUN ,L)
	  #+Franz   `(GETL-FRANZ-FCN-PROP ,FUN ,L)
	  #+NIL     `(GETL-NIL-FCN-PROP ,FUN ,L)
	  )

;; Non-destructive versions of DELQ and DELETE.  Already part of NIL
;; and LMLisp.  These should be rewritten as SUBRS and placed
;; in UTILS.  The subr versions can be more memory efficient.

;#-(OR Lispm NIL Multics Franz cl)
;(DEFMACRO REMQ (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
;	  (IF COUNTING? `(DELQ ,ITEM (APPEND ,LIST NIL) ,COUNT)
;	      `(DELQ ,ITEM (APPEND ,LIST NIL))))


(DEFMACRO REMQ (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
 `(remove ,item ,list :test 'eq ,@ (and counting? `(:count ,count))))

;#+cl ;in clmacs
;(DEFMACRO ZL-REMOVE (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
; `(remove ,item ,list :test 'equal ,@ (and counting? `(:count ,count))))	

;#-(OR Lispm NIL Multics Franz)
;(DEFMACRO zl-REMOVE (ITEM LIST &OPTIONAL (COUNT () COUNTING?))
;	  (IF COUNTING? `(zl-DELETE ,ITEM (APPEND ,LIST NIL) ,COUNT)
;	      `(zl-DELETE ,ITEM (APPEND ,LIST NIL))))

#-Lispm (DEFMACRO CATCH-ALL (FORM) `(CATCH NIL ,FORM))

;; (EXCH A B) exchanges the bindings of A and B
;; Maybe it should turn into (PSETF A B B A)?

(DEFMACRO EXCH (X Y) `(SETF ,X (PROG1 ,Y (SETF ,Y ,X))))

;; These are here for old code only.
;; Use FIFTH rather than CADDDDR.  Better, use DEFSTRUCT.

#-Franz (DEFMACRO CADDADR (X) `(CAR (CDDADR ,X)))
#-Franz (DEFMACRO CADDDDR (X) `(CAR (CDDDDR ,X)))

;; The following is a bit cleaner than the kludgy (PROGN 'COMPILE . <FORMS>)

(DEFMACRO COMPILE-FORMS (&REST <FORMS>) `(PROGN 'COMPILE . ,<FORMS>))


;; The following macros pertain only to Macsyma.

;; Widely used macro for printing error messages.  We should be able
;; to come up with something better.  On large address space systems
;; this should signal -- hack later.  Soon to be flushed in favor
;; of new Macsyma error system.  Yea!

;; Obsolete.  Use MERROR.

(DEFMACRO ERLIST (MESSAGE)
  (MAXIMA-ERROR "ERLIST is obsolete, all calls to it have been removed, so where
	 did you dig this one up loser?" message))

;; All functions are present on non-autoloading systems.  Definition
;; for autoloading systems is in SUPRV.
;; If you have dynamic linking you might as well take advantage of it.

#-(OR PDP10 NIL)
(DEFMACRO FIND-FUNCTION (FUNCTION) FUNCTION NIL)

;; Facility for loading auxilliary macro files such as RATMAC or MHAYAT.
;; Global macro files are loaded by the prelude file.

#+LISPM (DEFUN MACRO-DIR (X) (FORMAT NIL "LMMAXQ;~A QFASL" X))
#+PDP10 (DEFUN MACRO-DIR (X) `((LIBMAX) ,X))
#+Franz (defun macro-dir (x)  (cond ((cdr (zl-ASSOC x '((rzmac  . "rz//macros")
						     (mhayat . "rat//mhayat")
						     (ratmac . "rat//ratmac")))))
				    (t (concat "libmax//" x))))
#+NIL (defun macro-dir (x) (merge-pathname-defaults x "[VASL]"))

(comment Sample definition only on
	 ITS   see "LIBMAX;MODULE"
	 LISPM see "LMMAX;SYSDEF"
	 NIL   see   "VAXMAX;VAXCL"
	 Multics see "???"
	 Franz see "/usr/lib/lisp/machacks.l"
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

#+CL
(DEFUN LOAD-MACSYMA-MACROS-AT-RUNTIME (&REST L)
  (MAPCAR #'(LAMBDA (X)
	      (IF (GET X 'MACSYMA-MODULE)
		   X 
		   (ERROR  "Missing Macsyma macro file -- ~A" X)))
	  L))
#-CL
(DEFUN LOAD-MACSYMA-MACROS-AT-RUNTIME (&REST L)
  (MAPCAR #'load-when-needed L))

(DEFMACRO LOAD-MACSYMA-MACROS (&REST MACRO-FILES)
  `(COMMENT *MACRO*FILES*
	    ,(APPLY #'LOAD-MACSYMA-MACROS-AT-RUNTIME MACRO-FILES)))


#+Multics
(defmacro find-documentation-file (x)
  (cond ((eq x 'manual)
	 `(let ((filep (probe-file (list (catenate macsyma-dir ">documentation")
				     "macsyma.manual"))))
	    (cond (filep filep)
		  (t (MAXIMA-ERROR "Cannot MAXIMA-FIND the Macsyma manual")))))
	((eq x 'manual-index)
	 `(let ((filep (probe-file (list (catenate macsyma-dir ">documentation")
				     "macsyma.index.lisp"))))
	    (cond (filep filep)
		  (t (MAXIMA-ERROR "Cannot MAXIMA-FIND the Macsyma manual index")))))
	(t (MAXIMA-ERROR "Unknown documentation: " x))))

#+Multics
(defmacro load-documentation-file (x)
  `(load (find-documentation-file ,x)))

;;;Reset the stream to its starting position.
(defmacro rewind-stream (stream)
  
#-(or LispM NIL) `(filpos ,stream 0)
;#+LispM          `(send ,stream ':rewind)
#+cl `(file-position ,stream 0)
#+NIL            `(open ,stream))

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

(DEFMACRO MBINDING (VARIABLE-SPECIFICATION &REST BODY &AUX (TEMP (GENSYM)))
  `(LET ((,TEMP ,(CAR VARIABLE-SPECIFICATION)))
     ;; Don't optimize out this temporary, even if (CAR VARIABLE-SPECICIATION)
     ;; is an ATOM. We don't want to risk side-effects.
     ,(CASE (LENGTH VARIABLE-SPECIFICATION)
	((1)
	 `(MBINDING-SUB ,TEMP ,TEMP NIL ,@BODY))
	((2)
	 `(MBINDING-SUB ,TEMP ,(CADR VARIABLE-SPECIFICATION) NIL ,@BODY))
	((3)
	 `(MBINDING-SUB ,TEMP ,(CADR VARIABLE-SPECIFICATION)
			,(CADDR VARIABLE-SPECIFICATION)
			,@BODY))
	(T
	  (MAXIMA-ERROR "Bad variable specification:" variable-specification)))))

(DEFVAR MBINDING-USAGE
  #+(and PDP10 Maclisp)    'PROG1
  #+(and Multics Maclisp)  'UNWIND-PROTECT
  #+Franz                  'PROG1
  #+CL                  'UNWIND-PROTECT
  #+NIL                    'UNWIND-PROTECT
  )
  
(DEFMACRO MBINDING-SUB (VARIABLES VALUES FUNCTION-NAME &REST BODY
				  &AUX (WIN (GENSYM)))
  (CASE MBINDING-USAGE
    ((PROG1)
     `(PROG1 (PROGN (MBIND ,VARIABLES ,VALUES ,FUNCTION-NAME) ,@BODY)
	     (MUNBIND ,VARIABLES)))
    ((UNWIND-PROTECT)
     `(LET ((,WIN NIL))
	(UNWIND-PROTECT
	 (PROGN (MBIND ,VARIABLES ,VALUES ,FUNCTION-NAME)
		(SETQ ,WIN T)
		,@BODY)
	 (IF ,WIN (MUNBIND ,VARIABLES)))))
    ((PROGV)
     `(LET ((,WIN (MBINDING-CHECK ,VARIABLES ,VALUES ,FUNCTION-NAME)))
	(PROGV ,VARIABLES
	       ,WIN
	       ,@BODY)))
    (T
     (MAXIMA-ERROR "Unknown setting of MBINDING-USAGE" MBINDING-USAGE))))

#+NIL
(DEFMACRO MDEFPROP (A B C) `(MPUTPROP ',A ',B ',C))

#-Franz ;; Franz uses a function definition in COMM.
	;; For MLISTP its arg is known not to be an atom.
	;; Otherwise, just use $LISTP.
	;; MLISTP exists just to support a Franz hack, so you can just 
	;;   ignore it. - JPG
(DEFMACRO MLISTP (X) `(EQ (CAAR ,X) 'MLIST))

;; How About MTYPEP like (MTYPEP EXP 'TAN) or (MTYPEP EXP '*) - Jim.
;; Better, (EQ (MTYPEP EXP) 'TAN).

(DEFMACRO MTANP (X) 
  `(LET ((THING ,X))
     (AND (NOT (ATOM THING)) (EQ (CAAR THING) '%TAN))))

(DEFMACRO MATANP (X)
  `(LET ((THING ,X))
     (AND (NOT (ATOM THING)) (EQ (CAAR THING) '%ATAN))))

;; Macros used in LIMIT, DEFINT, RESIDU.
;; If we get a lot of these, they can be split off into a separate macro
;; package.

(DEFMACRO REAL-INFINITYP (X) `(MEMQ ,X REAL-INFINITIES))

(DEFMACRO INFINITYP (X) `(MEMQ ,X INFINITIES))

(DEFMACRO REAL-EPSILONP (X) `(MEMQ ,X INFINITESIMALS))

(DEFMACRO FREE-EPSILONP (X)
  `(DO ((ONE-EPS INFINITESIMALS (CDR ONE-EPS)))
       ((NULL ONE-EPS) T)
     (IF (NOT (FREE (CAR ONE-EPS) ,X))  (RETURN ()))))

(DEFMACRO FREE-INFP (X)
  `(DO ((ONE-INF INFINITIES (CDR ONE-INF)))
       ((NULL ONE-INF) T)
     (IF (NOT (FREE (CAR ONE-INF) ,X))  (RETURN ()))))

(DEFMACRO INF-TYPEP (X)
  `(CAR (AMONGL INFINITIES ,X)))

(DEFMACRO HOT-COEF (P)
 `(PDIS (CADDR (CADR (RAT-NO-RATFAC ,P)))))

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

#-(or Franz ITS lispm cl)
(DEFMACRO DEFMVAR (VARIABLE &OPTIONAL (INITIAL-VALUE NIL IV-P) DOCUMENTATION
                            &REST FLAGS &AUX DEFINER TYPE)
  DOCUMENTATION FLAGS ;; Ignored certain places.
  (SETQ DEFINER #+(or Multics ) 'DEFCONST
		#-(or Multics  ) 'DEFVAR)
  #-(or Lispm NIL)
  (SETQ TYPE (COND ((MEMQ 'fixnum FLAGS) 'fixnum)
		   ((MEMQ 'flonum FLAGS) 'flonum)
		   (T NIL)))
  #+NIL (macsyma-defmvar-declarations variable flags)
  `(PROGN 'COMPILE
	  ,(IF IV-P
	       `(,DEFINER ,VARIABLE ,INITIAL-VALUE
		    #+NIL ,@(AND DOCUMENTATION `(,DOCUMENTATION)))
	       `(,DEFINER ,VARIABLE #+LISPM () ))
	  #-NIL ,@(IF TYPE `((DECLARE (,TYPE ,VARIABLE))))))

;;see commac
#-(or cl lispm)
(Defmacro DEFMFUN (function &body  REST &aux .n.)
  #+NIL (macsyma-defmfun-declarations function rest)
       `(DEFUN ,FUNCTION  ,REST))

#+LISPM
(DEFPROP DEFMSPEC "Macsyma special form" SI:DEFINITION-TYPE-NAME)

;; Special form for declaring Macsyma external procedures.  Version for ITS
;; is in LIBMAX;DEFINE.
;; Franz version is in libmax/vdefine.l



#+cl
(DEFMACRO DEFMSPEC (FUNCTION . REST)
  `(progn
	  (DEFUN-prop ( ,FUNCTION MFEXPR*) . ,REST)
	  #+lispm (SI::RECORD-SOURCE-FILE-NAME ',FUNCTION 'DEFMSPEC)))

;#+LISPM
;(DEFMACRO DEFMSPEC (FUNCTION . REST)
;  `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,FUNCTION DEFMSPEC))
;     (DEFUN (:PROPERTY ,FUNCTION MFEXPR*) . ,REST)
;     (SI:RECORD-SOURCE-FILE-NAME ',FUNCTION 'DEFMSPEC)
;     ))


;;How bout the following for a replacement for the defmspec type.
;;It gives the special form business and seems to work well in the
;;interpreter.  The translated and compiled forms for special functions
;;was a problem any way and we should leave the def%tr forms for
;;$sum etc. and maybe institute some new ones.  
;;Of course meval and translate have to be told what to do with
;;a macro but they needed to be told anyway.--wfs

;;see commac
;#+lispm
;(defmacro defmspec (fn (aa) &rest rest &aux ans help )
;  (setq help (intern (format nil "~A-AUX" fn)))
;  (setq ans
;	(list    ;;copy-list aa
;    `(defmacro ,fn (&rest ,aa &aux e)(setq ,aa (copy-list ,aa))
;	       (setq e (cons (list ',fn) ,aa))
;               `(meval* '(,', help  ',e)))
;    `(defun ,help (,aa) . ,rest)))
;  `(progn 'compile . , ans))

;;eg.
;(defmspecial $ssum (l) (setq l (cdr l))
;  (if (= (length l) 4)
;      (dosum (car l) (cadr l) (meval (caddr l)) (meval (cadddr l)) t)
;      (wna-err '$$sum)))


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

#+ITS
(DEFMACRO MAUTOLOAD (FN2-DEV-DIR &REST MASTER-LIST)
  `(DOLIST (L ',MASTER-LIST)
     (DO ((FILE (IF (ATOM (CAR L))
		    (CONS (CAR L) ,FN2-DEV-DIR)
		    (CAR L)))
	  (FUNLIST (CDR L) (CDR FUNLIST)))
	 ((NULL FUNLIST))
       (PUTPROP (CAR FUNLIST) FILE 'AUTOLOAD))))

#-Multics
(DEFMACRO SYS-DEFAULTF (X) `(DEFAULTF ,X))
;;; For #+Multics a function definition for SYS-DEFAULTF can be found 
;;; in SUPRV.

(defmacro sys-user-id ()
  #+Franz '(getenv '|USER|)
  #+lispm 'user-id
  #+Multics '(status uname)
  #-(or Franz Multics lispm) '(status userid))

(defmacro sys-free-memory ()
  #-(or Multics lispm) '(status memfree)
  #+(or Multics lispm) 10000.) ;This should look at the pdir size
                               ;and mung it to give a good approximation.

;; Setf hacking.
;;
;;
;;(defsetf GET ((() sym tag) value) T 
;;   (eval-ordered* '(nsym ntag nvalue)
;;		  `(,sym ,tag ,value)
;;		  '`((PUTPROP ,nsym ,nvalue ,ntag))))

#+PDP10
(defsetf MGET ((() sym tag) value) T 
  (eval-ordered* '(nsym ntag nvalue)
		 `(,sym ,tag ,value)
		 '`((MPUTPROP ,nsym ,nvalue ,ntag))))

#+PDP10
(defsetf $GET ((() sym tag) value) T 
  (eval-ordered* '(nsym ntag nvalue)
		 `(,sym ,tag ,value)
		 '`(($PUT ,nsym ,nvalue ,ntag))))

#+Franz
(defsetf mget (expr value)
   `(mputprop ,(cadr expr) ,value ,(caddr expr)))

#+Franz
(defsetf $get (expr value)
   `($put ,(cadr expr) ,value ,(caddr expr)))

#+NIL
(DEFPROP MGET SETF-MGET SI:SETF-SUBR)
#+NIL
(DEFPROP $GET SETF-$GET SI:SETF-SUBR)

;;DIFFERENT version of setf on Multics and LM ...Bummer... -JIM 3/4/81
#+MULTICS
(defsetf MGET (sym tag) value
  `(MPUTPROP ,sym ,value ,tag))

(DEFMFUN MGET (ATOM IND)
  (LET ((PROPS (AND (SYMBOLP ATOM) (GET ATOM 'MPROPS))))
    (AND PROPS (GETf (cdr PROPS) IND))))

#+(or cl ti)
(defsetf MGET (sym tag) (value)
  `(MPUTPROP ,sym ,value ,tag))

(defmacro old-get (plist tag)
  `(getf (cdr ,plist) ,tag))

#+ MULTICS
(defsetf $GET (sym tag) value
  `($PUT ,sym ,value ,tag))

(DEFMFUN $GET (ATOM IND) (PROP1 '$GET ATOM NIL IND))

#+(or cl ti)
(defsetf $GET (sym tag) (value)
  `($PUT ,sym ,value ,tag))
;
;#+(and LISPM (not (or cl ti)))
;(DEFUN (:PROPERTY MGET SI:SETF) (REF VAL)
;  `(MPUTPROP ,(SECOND REF) ,VAL ,(THIRD REF)))
;
;#+(and LISPM (not (or cl ti)))
;(DEFUN (:PROPERTY $GET SI:SETF) (REF VAL)
;  `($PUT ,(SECOND REF) ,VAL ,(THIRD REF)))

(defmacro initialize-random-seed ()
  #+(or PDP10 NIL) '(sstatus random 0)
  #+CL () ;;(si:random-initialize si:random-array) obsolete. what now?
  )

;; These idiot macros are used in some places in macsyma.
;; The LISPM doesn't "go that high" with the series. DO NOT USE THESE
;; in new code. -gjc
;; NIL (common-lisp) has the nth accessors through to tenth, the rest
;; frobs through to rest5.  However i had thought that the latter were
;; obsolete, and had been going to flush them. --gsb
#-(or cl ti NIL)
(DEFMACRO EIGHTH  (FORM) `(CADDDR (CDDDDR ,FORM)))
#-(or cl ti NIL)
(DEFMACRO NINTH   (FORM) `(CAR (CDDDDR (CDDDDR ,FORM))))
#-(or cl ti NIL)
(DEFMACRO TENTH	  (FORM) `(CADR (CDDDDR (CDDDDR ,FORM))))
#-NIL
(DEFMACRO REST5 (FORM) `(CDR (CDDDDR ,FORM)))
(DEFMACRO REST6 (FORM) `(CDDR (CDDDDR ,FORM)))

;;; We should probably move these into the compatibility package on
;;; mulitcs.

#+Multics
(defmacro *break (breakp mess)
  `(apply 'break `(,,mess ,',breakp)))

;;; To satisfy GJC's speed mainia I resisted changing these in the
;;; code. -Jim.

#+Multics
(defmacro +tyi (&rest args)
  `(tyi ,@args))

#+Multics 
(defmacro +tyo (&rest args)
  `(tyo ,@args))

;;; Let the compiler know that x is a fixnum. I guess it will also
;;; then optimize the call to +.
#+Multics
(defmacro fixnum-identity (x)
  `(f+ ,x))

;;this was not called.
;(defmacro get-symbol-array-pointer (x)
;  #+franz `(getd ,x)
;  #+nil `(si:get-symbol-array-pointer ,x)
;  #+cl `(symbol-array ,x)
;  #+maclisp `(get ,x 'array))


(defmacro  mdefprop (sym val indicator)
  `(mputprop ',sym ',val ',indicator))


(DEFMFUN MPUTPROP (ATOM VAL IND)
  (LET ((PROPS (GET ATOM 'MPROPS)))
    (IF (NULL PROPS) (PUTPROP ATOM (SETQ PROPS (NCONS NIL)) 'MPROPS))
    (PUTPROP PROPS VAL IND)))
