;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module defcal macro)
;; Compile-time support for defining things which dispatch
;; off the property list. The Macsyma parser uses this.

(DEFUN CHECK-SUBR-ARGL (L)
  (IF (OR (> (LENGTH L) 5.)
	  (MEMQ '&REST L)
	  (MEMQ '&OPTIONAL L)
	  (MEMQ '&RESTV L)
	  (MEMQ '&QUOTE L))
      (MAXIMA-ERROR "Can't DEF-PROPL-CALL with non-subr arglist" L)))

(DEFVAR USE-SUBRCALL
  #+CL NIL
  #+MACLISP T
  #+NIL NIL)

(DEFMACRO DEF-PROPL-CALL (NAME (OP . L) DEFAULT-ACTION
			       &AUX
			       (TEMP (GENSYM))
			       (SUBR? (IF USE-SUBRCALL
					  (LIST (SYMBOLCONC NAME '-SUBR))
					  ())))
  (IF SUBR? (CHECK-SUBR-ARGL L))
  `(PROGN 'COMPILE
          #+lispm   (si:record-source-file-name ',name 'def-propl-call)
	  (DEFMACRO ,(SYMBOLCONC 'DEF- NAME '-EQUIV) (OP EQUIV)
           #+lispm  (declare (si:function-parent ,name 'def-propl-call))
	    `(PUTPROP ',OP #',EQUIV ',',NAME))
	  (DEFMACRO ,(SYMBOLCONC NAME '-PROPL) ()
	    #+lispm  (declare (si:function-parent ,name 'def-propl-call))

	    ''(,NAME ,@SUBR?))
	  (DEFMACRO ,(SYMBOLCONC 'DEF- NAME '-FUN) (OP-NAME OP-L . BODY)
;	    #+lispm  (declare (si:function-parent ,name 'def-propl-call))
;	    `(DEFUN (,OP-NAME ,',NAME  ,@',SUBR?)
;		    ,OP-L . ,BODY))
	    `(DEFUN-prop (,OP-NAME ,',NAME  ,@',SUBR?)
		    ,OP-L
       		    #+lispm  (declare (si:function-parent ,op-name 'def-nud-fun))
	       ,@ BODY))
	  (DEFUN ,(SYMBOLCONC NAME '-CALL) (,OP . ,L)
	    #+lispm  (declare (si:function-parent ,name 'def-propl-call))
	    (LET ((,TEMP (AND (SYMBOLP ,OP)
			      (GETL ,OP '(,NAME ,@SUBR?)))))
	      (IF (NULL ,TEMP)
		  ,DEFAULT-ACTION
		  ,(IF SUBR?
		       `(IF (EQ (CAR ,TEMP) ',(CAR SUBR?))
			    (SUBRCALL NIL (CADR ,TEMP) ,OP ,@L)
			    (FUNCALL (CADR ,TEMP) ,OP ,@L))
		       `(FUNCALL (CADR ,TEMP) ,OP ,@L)))))))


(DEFUN MAKE-PARSER-FUN-DEF (OP P BVL BODY)
  ;; Used by the Parser at compile time.
  (IF (NOT (consp OP))
      `(,(SYMBOLCONC 'DEF- P '-FUN) ,OP ,BVL
				    ,(CAR BVL)
				    ;; so compiler won't warn about
				    ;; unused lambda variable.
				    . ,BODY)
      `(PROGN 'COMPILE
	      ,(MAKE-PARSER-FUN-DEF (CAR OP) P BVL BODY)
	      ,@(MAPCAR #'(LAMBDA (X)
			    `(INHERIT-PROPL ',X ',(CAR OP)
					    (,(SYMBOLCONC P '-PROPL))))
			(CDR OP)))))


;;; The tokenizer use the famous CSTR to represent the possible extended token
;;; symbols. The derivation of the name and implementation is obscure, but I've
;;; heard it has something to do with an early Fortran compiler written in Lisp.
;;;  -GJC

;;; (CSTRSETUP <description>)
;;;
;;;  <description> ::= (<descriptor> <descriptor> ...)
;;;  <descriptor>  ::= <name> ! (<name> <translation>)
;;;  
;;;  If no translation is supplied, $<name> is the default.
;;;  
;;;  Sets up a CSTR [Command STRucture] object which may be used
;;;  in conjunction with the CEQ predicate to determine if the
;;;  LINBUF cursor is currently pointing at any keyword in that 
;;;  structure.
;;;  
;;;  Note: Names containing shorter names as initial segments
;;;        must follow the shorter names in arg to CSTRSETUP.

(DEFVAR SYMBOLS-DEFINED () "For safe keeping.")
(DEFVAR MACSYMA-OPERATORS ())

(eval-when (eval compile load)
  (DEFUN *DEFINE-INITIAL-SYMBOLS (L)
    (SETQ SYMBOLS-DEFINED
	  (SORT (copy-list L) #'(LAMBDA (X Y) (< (FLATC X) (FLATC Y)))))
    (SETQ MACSYMA-OPERATORS (CSTRSETUP SYMBOLS-DEFINED)))
  )


(DEFMACRO DEFINE-INITIAL-SYMBOLS (&REST L)
  (LET ((SYMBOLS-DEFINED ())
	(MACSYMA-OPERATORS ()))
    (*DEFINE-INITIAL-SYMBOLS L)
    `(PROGN 'COMPILE
	    (DECLARE-TOP (SPECIAL SYMBOLS-DEFINED MACSYMA-OPERATORS))
	    (SETQ SYMBOLS-DEFINED (copy-list ',SYMBOLS-DEFINED))
	    (SETQ MACSYMA-OPERATORS (SUBST () () ',MACSYMA-OPERATORS)))))

(DEFUN UNDEFINE-SYMBOL (OP)
  (*DEFINE-INITIAL-SYMBOLS (DELQ (STRIPDOLLAR OP) SYMBOLS-DEFINED)))

(DEFUN DEFINE-SYMBOL (X)
  (SETQ X (STRIPDOLLAR X))
  (*DEFINE-INITIAL-SYMBOLS (CONS X SYMBOLS-DEFINED))
  ;(IMPLODE (CONS #/$ (EXPLODEN X)))
  (symbolconc '$ x))

(DEFUN CSTRSETUP (ARG)
  (DO ((ARG ARG (CDR ARG)) (TREE NIL))
      ((NULL ARG) (LIST* () '(ANS ()) TREE))
    (COND ((ATOM (CAR ARG))
	   (SETQ TREE 
		 (ADD2CSTR (CAR ARG) 
			   TREE 
			   ;(IMPLODE (CONS '$ (EXPLODEC (CAR ARG))))
			   (symbolconc '$ (car arg))
			   )))
	  (T
	   (SETQ TREE 
		 (ADD2CSTR (CAAR ARG) TREE (CADAR ARG)))))))
   
;;; (ADD2CSTR <name> <tree> <translation>)
;;; 
;;;  Adds the information <name> -> <translation> to a 
;;;  CSTR-style <tree>.

(DEFUN ADD2CSTR (X TREE ANS) 
  (ADD2CSTR1 (NCONC (EXPLODEN X) (NCONS (LIST 'ANS ANS)))
	     TREE))
   
;;; (ADD2CSTR1 <translation-info> <tree>)
;;;
;;;  Helping function for ADD2CSTR. Puts information about a 
;;;  keyword into the <tree>

(DEFUN ADD2CSTR1 (X TREE)
  (COND ((NULL TREE) X)
	((ATOM (CAR TREE))
	 (COND ((EQUAL (CAR TREE) (CAR X))
		(RPLACD TREE (ADD2CSTR1 (CDR X) (CDR TREE))))
	       (T (LIST TREE (COND ((ATOM (CAR X)) X)
				   ((EQUAL (CAAR X) 'ANS) (CAR X))
				   (T X))))))
	((EQUAL (CAAR TREE) (CAR X))
	 (RPLACD (CAR TREE) (ADD2CSTR1 (CDR X) (CDAR TREE)))
	 TREE)
	((NULL (CDR TREE))
	 (RPLACD TREE (LIST X))
	 TREE)
	(T (RPLACD TREE (ADD2CSTR1 X (CDR TREE)))
	   TREE)))
