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


(macsyma-module merror)

;;; Macsyma error signalling. 
;;; 2:08pm  Tuesday, 30 June 1981 George Carrette.

(defvar DEBUG T "Enter the lisp debugger on an error if this is true")

(DEFMVAR $ERROR '((MLIST SIMP) |&No error.|)
  "During an MAXIMA-ERROR break this is bound to a list
  of the arguments to the call to MAXIMA-ERROR, with the message
  text in a compact format.")

(DEFMVAR $ERRORMSG 'T
  "If FALSE then NO MAXIMA-ERROR message is printed!")

(DEFMFUN $ERROR (&REST L)
  "Signals a Macsyma user error."
  (apply #'merror (fstringc L)))

(DEFMVAR $ERROR_SIZE 10.
  "Expressions greater in SOME size measure over this value
  are replaced by symbols {ERREXP1, ERREXP2,...} in the MAXIMA-ERROR
  display, the symbols being set to the expressions, so that one can
  look at them with expression editing tools. The default value of
  this variable may be determined by factors of terminal speed and type.")

(DEFUN ERROR-SIZE (EXP)
  (IF (ATOM EXP) 0
      (DO ((L (CDR EXP) (CDR L))
	   (N 1 (f1+ (f+ N (ERROR-SIZE (CAR L))))))
	  ((OR (atom L)
	       ;; no need to go any further, and this will save us
	       ;; from circular structures. (Which the display
	       ;; package would have a hell of a time with too.)
	       (> N $ERROR_SIZE))
	   N)
	(DECLARE (FIXNUM N)))))

;;; Problem: Most macsyma users do not take advantage of break-points
;;; for debugging. Therefore they need to have the error variables
;;; SET (as the old ERREXP was), and not PROGV bound. The problem with
;;; this is that recursive errors will bash the old value of the
;;; error variables. However, since we do bind the value of the
;;; variable $ERROR, calling the function $ERRORMSG will always
;;; set things back. It would be better to bind these variables,
;;; for, amoung other things, then the values could get garbage 
;;; collected.

;Make up your mind.  The first definition here, commented out, is the
; original in the source.  I guess the binding didn't make it, because
; the second is from the 302 fix file F302. --gsb
;(DEFMFUN MERROR (STRING &REST L)
;  (SETQ STRING (CHECK-OUT-OF-CORE-STRING STRING))
;  (LET (($ERROR `((MLIST) ,STRING ,@L)))
;    (AND $ERRORMSG ($ERRORMSG))
;    (ERROR #+(OR LISPM NIL) STRING)))
;#-cl
;(DEFMFUN MERROR (STRING &REST L)
;  (SETQ STRING (CHECK-OUT-OF-CORE-STRING STRING))
;  (SETQ $ERROR `((MLIST) ,STRING ,@L))
;  (AND $ERRORMSG ($ERRORMSG))
;  (MAXIMA-ERROR #+(OR CL NIL) STRING))


(DEFUN MERROR (SSTRING &REST L)
  (declare (special errcatch debug))
  (SETQ $ERROR `((MLIST) ,SsTRING ,@ L))
  (AND $ERRORMSG ($ERRORMSG))
  (cond (debug
	 (let ((dispflag t) ret)
	   (declare (special $help dispflag))
	   (format t " -- an error.  Entering the Maxima Debugger dbm")
	   (progn
	     (setq ret (break-dbm-loop nil))
	     (cond ((eql ret :resume)
		    (break-quit)))
		    
;#+previous
;	     (cond ((and (eql ret 'exit)
;		    (member 'macsyma-break state-pdl))
; 	            (throw 'macsyma-break t))
;	           (t  (throw 'macsyma-quit t)
;	      ))
	     )))
	(errcatch  (error " -- an error: macsyma error"))
	(t
	 (fresh-line *standard-output*)
	 ($backtrace 3)
	 (format t "~& -- an error.  Quitting.  To debug this try DEBUGMODE(TRUE);~%")
	 (throw 'macsyma-quit t )
		;(if errcatch (error "macsyma error"))
	 )))

;sample:
;(defun h (he)
;  (merror "hi there ~:M and ~:M" he he))

(DEFMVAR $ERROR_SYMS '((MLIST) $ERREXP1 $ERREXP2 $ERREXP3)
  "Symbols to bind the too-large MAXIMA-ERROR expresssions to")

(DEFUN-prop ($ERROR_SYMS ASSIGN) (VAR VAL)
  (IF (NOT (AND ($LISTP VAL)
		(DO ((L (CDR VAL) (CDR L)))
		    ((NULL L) (RETURN T))
		  (IF (NOT (SYMBOLP (CAR L))) (RETURN NIL)))))
      (MERROR "The variable ~M being set to ~M which is not a list of symbols."
	      VAR VAL)))

(DEFUN PROCESS-ERROR-ARGL (L)
  ;; This returns things so that we could set or bind.
  (DO ((ERROR-SYMBOLS NIL)
       (ERROR-VALUES NIL)
       (NEW-ARGL NIL)
       (SYMBOL-NUMBER 0))
      ((NULL L)
       (LIST (NREVERSE ERROR-SYMBOLS)
	     (NREVERSE ERROR-VALUES)
	     (NREVERSE NEW-ARGL)))
    (LET ((FORM (POP L)))
      (COND ((> (ERROR-SIZE FORM) $ERROR_SIZE)
	     (SETQ SYMBOL-NUMBER (f1+ SYMBOL-NUMBER))
	     (LET ((SYM (NTHCDR SYMBOL-NUMBER $ERROR_SYMS)))
	       (COND (SYM
		      (SETQ SYM (CAR SYM)))
		     ('ELSE
		      (SETQ SYM (CONCAT '$ERREXP SYMBOL-NUMBER))
		      (SETQ $ERROR_SYMS (APPEND $ERROR_SYMS (LIST SYM)))))
	       (PUSH SYM ERROR-SYMBOLS)
	       (PUSH FORM ERROR-VALUES)
	       (PUSH SYM NEW-ARGL)))
	    ('ELSE
	     (PUSH FORM NEW-ARGL))))))

(DEFMFUN $ERRORMSG ()
  "ERRORMSG() redisplays the MAXIMA-ERROR message while in an MAXIMA-ERROR break."
  ;; Don't optimize out call to PROCESS-ERROR-ARGL in case of
  ;; multiple calls to $ERRORMSG, because the user may have changed
  ;; the values of the special variables controling its behavior.
  ;; The real expense here is when MFORMAT calls the DISPLA package.
  (LET ((THE-JIG (PROCESS-ERROR-ARGL (CDDR $ERROR))))
    (MAPC #'SET (CAR THE-JIG) (CADR THE-JIG))
    (fresh-line)
    (LET ((ERRSET NIL))
      (IF (NULL (ERRSET
		 (APPLY #'MFORMAT nil
			(CADR $ERROR) (CADDR THE-JIG))))
	  (MTELL "~%** error while printing ERROR message **~%~A~%"
		 (CADR $ERROR)
		 )))
    (fresh-line)
    )
  '$DONE)

(DEFMFUN READ-ONLY-ASSIGN (VAR VAL)
  (IF MUNBINDP
      'MUNBINDP
      (MERROR "Attempting to assign read-only variable ~:M the value:~%~M"
	      VAR VAL)))


(DEFPROP $ERROR READ-ONLY-ASSIGN  ASSIGN)

;; THIS THROWS TO  (CATCH 'RATERR ...), WHEN A PROGRAM ANTICIPATES
;; AN ERROR (E.G. ZERO-DIVIDE) BY SETTING UP A CATCH  AND SETTING
;; ERRRJFFLAG TO T.  Someday this will be replaced with SIGNAL.
;; Such skill with procedure names!  I'd love to see how he'd do with
;; city streets.

;;; N.B. I think the above comment is by CWH, this function used
;;; to be in RAT;RAT3A. Its not a bad try really, one of the better
;;; in macsyma. Once all functions of this type are rounded up
;;; I'll see about implementing signaling. -GJC

(DEFMFUN ERRRJF N
  (IF ERRRJFFLAG (THROW 'RATERR NIL) (APPLY #'MERROR (LISTIFY N))))

;;; The user-error function is called on |&foo| "strings" and expressions.
;;; Cons up a format string so that $ERROR can be bound.
;;; This might also be done at code translation time.
;;; This is a bit crude.

(defmfun fstringc (L)
  (do ((sl nil) (s) (sb)
		(se nil))
      ((null l)
       (setq sl (maknam sl))
       ;#+PDP10 (putprop sl t '+INTERNAL-STRING-MARKER)
       (cons sl (nreverse se)))
    (setq s (pop l))
    (cond ((and (symbolp s) (char= (getcharn s 1) #\&))
	   (setq sb (mapcan #'(lambda (x)
				(if (char= x #\~)
				    (list x x)
				    (list x)))
			    (cdr (exploden s)))))
	  (t
	   (push s se)
	   (setq sb (list #\~ #\M))))
    (setq sl (nconc sl sb (if (null l) nil (list #\SPACE))))))
