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
(macsyma-module inmis)

(DECLARE-TOP (SPECIAL LISTOFVARS))

(DEFMVAR $LISTCONSTVARS NIL
  "Causes LISTOFVARS to include %E, %PI, %I, and any variables declared
   constant in the list it returns if they appear in exp.  The default is
   to omit these." BOOLEAN SEE-ALSO $LISTOFVARS)

(DEFMVAR $LISTDUMMYVARS T)

(DEFMVAR $POLYFACTOR NIL)

(DEFMFUN $UNKNOWN (F) (CATCH 'unknown (UNKNOWN (SPECREPCHECK F))))

(DEFUN UNKNOWN (F)
  (AND (NOT (MAPATOM F))
       (COND ((AND (EQ (CAAR F) 'MQAPPLY)
		   (NOT (OLDGET (CAAADR F) 'SPECSIMP)))
	      (THROW 'unknown T))
	     ((NOT (OLDGET (CAAR F) 'OPERATORS)) (THROW 'unknown T))
	     (T (MAPC #'UNKNOWN (CDR F)) NIL))))

(DEFMFUN $LISTOFVARS (E) 
  (LET ((LISTOFVARS (NCONS '(MLIST))))
    (WHEN ($RATP E)
	  (AND (MEMQ 'TRUNC (CDDAR E)) (SETQ E ($TAYTORAT E)))
	  (SETQ E (CONS '(MLIST)
			(SUBLIS (MAPCAR #'CONS
					(CAR (CDDDAR E))
					;; GENSYMLIST
					(CADDAR E))
				;; VARLIST
				(UNION* (LISTOVARS (CADR E))
					(LISTOVARS (CDDR E)))))))
    (ATOMVARS E)
    (IF (NOT $LISTDUMMYVARS)
	(DOLIST (U (CDR LISTOFVARS))
	  (IF (FREEOF U E) (zl-DELETE U LISTOFVARS 1))))
    LISTOFVARS))

(DEFUN ATOMVARS (E) 
  (COND ((AND (SYMBOLP E) (OR $LISTCONSTVARS (NOT ($CONSTANTP E))))
	 (ADD2LNC E LISTOFVARS))
	((ATOM E))
	((SPECREPP E) (ATOMVARS (SPECDISREP E)))
	((MEMQ 'array (CAR E)) (MYADD2LNC E LISTOFVARS))
	(T (MAPC #'ATOMVARS (MARGS E))))) 

(DEFUN MYADD2LNC (ITEM LIST) 
  (AND (NOT (MEMALIKE ITEM LIST)) (NCONC LIST (NCONS ITEM)))) 

;; Reset the settings of all Macsyma user-level switches to their initial
;; values.

#+ITS
(DEFMFUN $RESET NIL (LOAD '((DSK MACSYM) RESET FASL)) '$DONE)

#+Multics
(DEFMFUN $RESET () (LOAD (EXECUTABLE-DIR "RESET")) '$DONE)

#+NIL
(DEFMFUN $RESET ()
  (LOAD "MAX$DISK:[MAXDMP]RESET" :set-default-pathname nil))

;; Please do not use the following version on MC without consulting with me.
;; I already fixed several bugs in it, but the +ITS version works fine on MC 
;; and takes less address space. - JPG
(DECLARE-TOP(SPECIAL MODULUS $FPPREC))
#-(or ITS Multics NIL) ;This version should be eventually used on Multics.
(DEFMFUN $RESET ()
	(SETQ *print-base* 10. *read-base* 10. ; *NOPOINT T
	      MODULUS NIL
	      ;ZUNDERFLOW T
	      )
	($DEBUGMODE NIL)
	(COND ((NOT (= $FPPREC 16.)) ($FPPREC 16.) (SETQ $FPPREC 16.))) 
   #+GC ($DSKGC NIL)
	(LOAD #+PDP10   '((ALJABR) INIT RESET)
	      #+Lispm   "MACSYMA-OBJECT:ALJABR;INIT"
	      #+Multics (executable-dir "init_reset")
	      #+Franz    (concat vaxima-main-dir "//aljabr//reset"))
	;; *** This can be flushed when all Macsyma user-switches are defined
	;; *** with DEFMVAR.  This is part of an older mechanism.
 #+PDP10 (LOAD '((MACSYM) RESET FASL))
	'$DONE)
