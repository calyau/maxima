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
(macsyma-module mrgmac macro)

(defun zl-get (sym tag)
  (cond ((symbolp sym) (get sym tag))
	(t (getf (cdr sym) tag))))

#-LISPM
(DEFMACRO FIX-LM (&rest BODY)
  `(PROGN . ,BODY))

#+LISPM
(DEFMACRO FIX-LM (&BODY BODY)
  `(LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
     . ,BODY))


;; The GRAM and DISPLA packages manipulate lists of fixnums, representing
;; lists of characters.  This syntax facilitates typing them in.
;; {abc} reads as (#/a #/b #/c), unquoted.

(DEFUN CHAR-LIST-SYNTAX-ON ()
  (FIX-LM
    (SETSYNTAX '|{| 'MACRO
	       #'(LAMBDA () (DO ((C (TYI) (TYI)) (NL))
				((char= #\} C) (NREVERSE NL))
			      (SETQ NL (CONS C NL)))))
    T))

(DEFUN CHAR-LIST-SYNTAX-OFF ()
  (FIX-LM nil
    #+(OR MACLISP NIL) (SETSYNTAX '|{| 'MACRO NIL)
    #+Franz   (setsyntax '|{| 2)
    #+LISPM   (SET-SYNTAX-FROM-DESCRIPTION #\{ 'SI:ALPHABETIC)))

;; This sets up the syntax for a simple mode system defined later on
;; in this file.  As usual, it is poorly documented.

#-cl
(DEFUN MODE-SYNTAX-ON ()
  ;; :A:B:C --> (SEL A B C)
  ;; A component selection facility.  :A:B:C is like (C (B A)) in the
  ;; DEFSATRUCT world.
  (FIX-LM
    (SETSYNTAX '|:| 'MACRO
	       #'(LAMBDA () (DO ((L (LIST (READ)) (CONS (READ) L)))
				((NOT (char= #\: (TYIPEEK))) (CONS 'SEL (NREVERSE L)))
			      (TYI))))
    
    ;; <A B C> --> (SELECTOR A B C)  Used when defining a mode.
    (SETSYNTAX '|<| 'MACRO
	       #'(LAMBDA ()
		   (COND ((char= #\SPACE (TYIPEEK)) '|<|)
			 ((char= #\= (TYIPEEK)) (TYI) '|<=|)
			 (T (DO ((S (READ) (READ)) (NL))
				((EQ '|>| S) (CONS 'SELECTOR (NREVERSE NL)))
			      (SETQ NL (CONS S NL)))))))
    
    ;; Needed as a single character object.  Used when defining a mode.
    (SETSYNTAX '|>| 'MACRO
	       #'(LAMBDA ()
		   (COND ((NOT (char= #\= (TYIPEEK))) '|>|)
			 (T (TYI) '|>=|))))
    T))

#-cl
(DEFUN MODE-SYNTAX-OFF ()
  (FIX-LM
    #+(OR MACLISP NIL) (PROGN (SETSYNTAX '|:| 'MACRO NIL)
			      (SETSYNTAX '|<| 'MACRO NIL)
			      (SETSYNTAX '|>| 'MACRO NIL))
    #+LISPM (PROGN (SI:SET-SYNTAX-BITS #\: '(0 . 23))
		   (SET-SYNTAX-FROM-DESCRIPTION #\> 'SI:ALPHABETIC)
		   (SET-SYNTAX-FROM-DESCRIPTION #\< 'SI:ALPHABETIC))
    #+Franz (progn (setsyntax '|:| 2)
		   (setsyntax '|<| 2)
		   (setsyntax '|>| 2))))

;; Loading this file used to turn on the mode syntax.  Its been turned off
;; now and hopefully no files left rely on it.  Files which want to 
;; use that syntax should call (MODE-SYNTAX-ON) during read time.

#+MACLISP
(DEFUN DEFINE-MACRO (NAME LAMBDA-EXP)
    (PUTPROP NAME LAMBDA-EXP 'MACRO))

#+CL
(DEFUN DEFINE-MACRO (NAME LAMBDA-EXP)
  (FIX-LM
    (COND ((SYMBOLP LAMBDA-EXP) (SETQ LAMBDA-EXP (symbol-function LAMBDA-EXP))))
    #-cl(si:record-source-file-name name 'macro)
    #-cl(FSET NAME (CONS 'MACRO LAMBDA-EXP))
    #+cl ;note need two args for cl macro
    (setf (macro-function name) lambda-exp))
    )

#+Franz
(defun define-macro (name lambda-exp)
  (putd name `(macro (dummy-arg) (,lambda-exp dummy-arg))))

#+NIL
(DEFUN DEFINE-MACRO (NAME LAMBDA-EXP)
  (ADD-MACRO-DEFINITION NAME LAMBDA-EXP))

;; LAMBIND* and PROGB* are identical, similar to LET, but contain an implicit
;; PROG.  On the Lisp Machine, PROG is extended to provide this capability.

(DEFMACRO LAMBIND* (VAR-LIST . BODY) `(LET ,VAR-LIST (PROG NIL . ,BODY)))
(DEFMACRO PROGB* (VAR-LIST . BODY) `(LET ,VAR-LIST (PROG NIL . ,BODY)))

(DEFMACRO MAPAND (FUNCTION LIST) 
  `(DO ((L ,LIST (CDR L))) ((NULL L) T)
       (IFN (,FUNCTION (CAR L)) (RETURN NIL))))

(DEFMACRO MAPOR (FUNCTION LIST)
  `(DO ((L ,LIST (CDR L))) ((NULL L))
       (IF (FUNCALL ,FUNCTION (CAR L)) (RETURN T))))

;; (MAPLAC #'1+ '(1 2 3)) --> '(2 3 4), but the original list is rplaca'd
;; rather than a new list being consed up.

(DEFMACRO MAPLAC (FUNCTION LIST)
  `(DO ((L ,LIST (CDR L))) ((NULL L)) (RPLACA L (FUNCALL ,FUNCTION (CAR L)))))

(defmacro put (a b c) `(putprop ,a ,b ,c))

;(defmacro zl-REM (a b) `(remprop ,a ,b))

(DEFMACRO COPYP (L) `(CONS (CAR ,L) (CDR ,L)))
(DEFMACRO COPYL (L) #+NIL `(COPY-LIST ,L) #-NIL `(APPEND ,L NIL))

(DEFMACRO ECONS (X Y) `(APPEND ,X (LIST ,Y)))

#-Franz 
(progn 'compile
  (DEFMACRO CAAADAR (X) `(CAAADR (CAR ,X)))
  (DEFMACRO CAAADDR (X) `(CAAADR (CDR ,X)))
  (DEFMACRO CAADAAR (X) `(CAADAR (CAR ,X)))
  (DEFMACRO CAADADR (X) `(CAADAR (CDR ,X)))
  (DEFMACRO CADAAAR (X) `(CADAAR (CAR ,X)))
  (DEFMACRO CADADDR (X) `(CADADR (CDR ,X)))
  (DEFMACRO CADDAAR (X) `(CADDAR (CAR ,X)))
  (DEFMACRO CADDDAR (X) `(CADDDR (CAR ,X)))
  (DEFMACRO CDADADR (X) `(CDADAR (CDR ,X)))
  (DEFMACRO CDADDDR (X) `(CDADDR (CDR ,X)))
  (DEFMACRO CDDDDDR (X) `(CDDDDR (CDR ,X))))

(DEFMACRO TELL (&REST ARGS) `(DISPLA (LIST '(MTEXT) . ,ARGS)))



(declare-top (SPECIAL NAME BAS MOBJECTS SELECTOR) (*EXPR MODE))

(SETQ MOBJECTS NIL)

(DEFPROP MODE (C-MODE S-MODE A-MODE) MODE)

(DEFMACRO C-MODE (&REST L) `(LIST . ,L))

;(MACRO S-MODE (X)
;  (COND ((EQ 'C (CADDR X)) `(CAR ,(CADR X)))
;	((EQ 'SEL (CADDR X)) `(CADR ,(CADR X)))
;	((EQ '_ (CADDR X)) `(CADDR ,(CADR X)))))


;(MACRO A-MODE (X)
;  (COND ((EQ 'C (CADDR X)) `(RPLACA (CADR X) ,(CADDDR X)))
;	((EQ 'SEL (CADDR X)) `(RPLACA (CDR ,(CADR X)) ,(CADDDR X)))
;	((EQ '_ (CADDR X)) `(RPLACA (CDDR ,(CADR X)) ,(CADDDR X)))))

;(MACRO DEFMODE (X)
;  (LET ((SELECTOR (MEMQ 'SELECTOR (CDDDDR X))))
;    (DEFINE-MODE (CADR X) (CADDDR X))
;    (MAPC 'EVAL (CDDDDR X))
;    `',(CADR X)))

(defMACRO S-MODE (&rest X)
  (setq X (cons ' S-MODE X ))
  (COND ((EQ 'C (CADDR X)) `(CAR ,(CADR X)))
	((EQ 'SEL (CADDR X)) `(CADR ,(CADR X)))
	((EQ '_ (CADDR X)) `(CADDR ,(CADR X)))))


(defMACRO A-MODE (&rest X)
  (setq X (cons ' A-MODE X ))
  (COND ((EQ 'C (CADDR X)) `(RPLACA (CADR X) ,(CADDDR X)))
	((EQ 'SEL (CADDR X)) `(RPLACA (CDR ,(CADR X)) ,(CADDDR X)))
	((EQ '_ (CADDR X)) `(RPLACA (CDDR ,(CADR X)) ,(CADDDR X)))))

(defMACRO DEFMODE (&rest X)
  (setq X (cons ' DEFMODE X ))
  (LET ((SELECTOR (MEMQ 'SELECTOR (CDDDDR X))))
       (setq billy `(DEFINE-MODE ,(CADR X) ,(CADDDR X)))
       
    (DEFINE-MODE (CADR X) (CADDDR X))
    (MAPC 'EVAL (CDDDDR X))
    `',(CADR X)))

#+(OR NIL CL)
(DEFUN DEFINE-MODE (NAME DESC &aux #+lispm (default-cons-area working-storage-area))
  
  (PROG (C S A)
	(SETQ
	  C (INTERN (FORMAT NIL "C-~A" NAME)) 
	  S (INTERN (FORMAT NIL "S-~A" NAME)) 
	  A (INTERN (FORMAT NIL "A-~A" NAME)))
     (setq silly `    (DEFINE-MACRO ,C ,(DEFC DESC))	)
    (DEFINE-MACRO C (DEFC DESC))
    (DEFINE-MACRO S (DEFS DESC))
    (DEFINE-MACRO A (DEFA DESC))
    (PUT NAME (C-MODE C S A) 'MODE)
    (RETURN NAME)))

#-(OR NIL CL)
(DEFUN DEFINE-MODE (NAME DESC)
  (PROG (C S A DUMMY)
    (SETQ DUMMY (EXPLODEC NAME)
	  C (IMPLODE (APPEND '(C -) DUMMY))
	  S (IMPLODE (APPEND '(S -) DUMMY))
	  A (IMPLODE (APPEND '(A -) DUMMY)))
    (DEFINE-MACRO C (DEFC DESC))
    (DEFINE-MACRO S (DEFS DESC))
    (DEFINE-MACRO A (DEFA DESC))
    (PUT NAME (C-MODE C S A) 'MODE)
    (RETURN NAME)))


(DEFUN DEFC (DESC) (LET ((BAS 'X)) (coerce `(LAMBDA (X &optional env) env
					    ,(DEFC1 DESC)) 'function)))

(DEFUN DEFC1 (DESC)
  (COND ((ATOM DESC) (LIST 'QUOTE DESC))
	((EQ 'SELECTOR (CAR DESC))
	 (COND ((NOT (NULL (CDDDR DESC))) (LIST 'QUOTE (CADDDR DESC)))
	       (T (SETQ BAS (LIST 'CDR BAS))
		  (LIST 'CAR BAS))))
	((EQ (QUOTE ATOM) (CAR DESC))
	 `(LIST 'C-ATOM '',(MAPCAR 'CADR (CDR DESC)) (CONS (QUOTE LIST) (CDR X))))
	((EQ 'CONS (CAR DESC)) `(LIST 'CONS ,(DEFC1 (CADR DESC)) ,(DEFC1 (CADDR DESC))))
	((EQ (quote LIST) (CAR DESC))
	 (DO ((L (CDR DESC) (CDR L)) (NL))
	     ((NULL L) `(LIST (QUOTE LIST) . ,(NREVERSE NL)))
	     (SETQ NL (CONS (DEFC1 (CAR L)) NL))))
	((EQ 'STRUCT (CAR DESC)) (DEFC1 (CONS (QUOTE LIST) (CDR DESC))))
	(T (LIST 'QUOTE DESC))))


(DEFUN DEFS (DESC)
  (coerce `(LAMBDA (X &optional env)env
	   (COND . ,(NREVERSE (DEFS1 DESC '(CADR X) NIL))))'function )) 

(DEFUN DEFS1 (DESC BAS RESULT)
  (COND ((ATOM DESC) RESULT)
	((EQ 'SELECTOR (CAR DESC))
	 (PUT (CADR DESC)
	      (CONS (CONS NAME (CADDR DESC)) (ZL-GET (CADR DESC) 'MODES))
	      'MODES)
	 (PUT NAME
	      (CONS (CONS (CADR DESC) (CADDR DESC)) (ZL-GET NAME 'SELS))
	      'SELS)
	 (IF SELECTOR (DEFINE-MACRO (CADR DESC) 'SELECTOR))
	 (CONS `((EQ ',(CADR DESC) (CADDR X)) ,BAS) RESULT))
	((EQ (QUOTE ATOM) (CAR DESC))
	 (DO ((L (CDR DESC) (CDR L))) ((NULL L))
	     (PUT (CADAR L) (CONS (CONS NAME (CADDAR L))
				  (ZL-GET (CADAR L) 'MODES)) 'MODES)
	     (PUT NAME (CONS (CONS (CADAR L) (CADDAR L))
			     (ZL-GET NAME 'SELS)) 'SELS)
	     (IF SELECTOR (DEFINE-MACRO (CADAR L) 'SELECTOR)))
	 (CONS `((MEMQ (CADDR X) ',(MAPCAR 'CADR (CDR DESC)))
		 (LIST 'ZL-GET ,BAS (LIST 'QUOTE (CADDR X))))
	       RESULT))
	((EQ 'CONS (CAR DESC))
	 (SETQ RESULT (DEFS1 (CADR DESC) `(LIST 'CAR ,BAS) RESULT))
	 (DEFS1 (CADDR DESC) `(LIST 'CDR ,BAS) RESULT))
	((EQ (QUOTE LIST) (CAR DESC))
	 (DO ((L (CDR DESC) (CDR L))) ((NULL L))
	     (SETQ RESULT (DEFS1 (CAR L) `(LIST 'CAR ,BAS) RESULT)
		   BAS `(LIST 'CDR ,BAS)))
	 RESULT)
	((EQ 'STRUCT (CAR DESC)) (DEFS1 (CONS (QUOTE LIST) (CDR DESC)) BAS RESULT))
	(T RESULT)))

(DEFUN DEFA (DESC)
  (coerce `(LAMBDA (X &optional env) env
	   (COND . ,(NREVERSE (DEFA1 DESC '(CADR X) NIL NIL)))) 'function))

(DEFUN DEFA1 (DESC BAS CDR RESULT)
  (COND ((ATOM DESC) RESULT)
	((EQ 'SELECTOR (CAR DESC))
	 (SETQ BAS (COND ((NOT CDR) `(LIST 'CAR (LIST 'RPLACA ,(CADDR BAS) (CADDDR X))))
			 (T `(LIST 'CDR (LIST 'RPLACD ,(CADDR BAS) (CADDDR X))))))
	 (CONS `((EQ ',(CADR DESC) (CADDR X)) ,BAS) RESULT))
	((EQ  (QUOTE ATOM) (CAR DESC))
	 (LIST `(T (LIST 'A-ATOM (CADR X) (LIST 'QUOTE (CADDR X)) (CADDDR X)))))
	((EQ 'CONS (CAR DESC))
	 (SETQ RESULT (DEFA1 (CADR DESC) `(LIST 'CAR ,BAS) NIL RESULT))
	 (DEFA1 (CADDR DESC) `(LIST 'CDR ,BAS) T RESULT))
	((EQ (QUOTE LIST) (CAR DESC))
	 (DO ((L (CDR DESC) (CDR L))) ((NULL L))
	     (SETQ RESULT (DEFA1 (CAR L) `(LIST 'CAR ,BAS) NIL RESULT)
		   BAS `(LIST 'CDR ,BAS)))
	 RESULT)
	((EQ 'STRUCT (CAR DESC)) (DEFA1 (CONS (QUOTE LIST) (CDR DESC)) BAS CDR RESULT))
	(T RESULT)))


(DEFUN MODE (X) (CDR (zl-ASSOC X MOBJECTS)))

#-NIL

(defmacro modedeclare (&rest l)
  `(modeclare-internal ',l))

(defun modedeclare-internal (x)
  (MAPC #'(LAMBDA (L) (MAPC #'(LAMBDA (V) (PUSH (CONS V (CAR L)) MOBJECTS))
			   (CDR L)))
	X))

;; Do not make this (ERROR 'NDM-ERR).  It won't work on the Lisp machine.

(DEFUN NDM-ERR (X)
  (TERPRI)
  (PRINC "Cannot determine the mode of ") (PRINC X)
  (MAXIMA-ERROR "NDM-ERR"))

(DEFUN NSM-ERR (X)
  (TERPRI)
  (PRINC "No such mode as ") (PRINC X)
  (MAXIMA-ERROR "NSM-ERR"))

(DEFUN SEL-ERR (B S)
  (TERPRI)
  (TYO #\:) (PRINC B)
  (DO () ((NULL S)) (TYO #\:) (PRINC (CAR S)) (SETQ S (CDR S)))
  (PRINC "is an impossible selection")
  (MAXIMA-ERROR "SEL-ERR"))

(DEFUN IA-ERR (X)
  (TERPRI)
  (PRINC "Cannot assign ") (PRINC X)
  (MAXIMA-ERROR "IA-ERR"))

(defMACRO SEL (&rest X)
  (setq X (cons ' SEL X ))
  (LET ((S (FSEL (MODE (CADR X)) (CDDR X))))
    (COND ((NULL S) (SEL-ERR (CADR X) (CDDR X)))
	  (T (SETQ X (CADR X))
	     (DO () ((NULL (CDR S)) X)
		 (SETQ X (CONS (CADR (ZL-GET (CAR S) 'MODE)) (RPLACA S X)) S (CDDR S))
		 (RPLACD (CDDR X) NIL))))))

(DEFUN FSEL (M SELS)    ;;This has a bug in it. 
  (COND ((NULL SELS) (LIST M))
	((NULL M)
	 (DO ((L (ZL-GET (CAR SELS) 'MODES) (CDR L))) ((NULL L))
	     (IF (SETQ M (FSEL (CDAR L) (CDR SELS)))
		 (RETURN (CONS (CAAR L) (CONS (CAR SELS) M))))))
	((LET (DUM)
	   (IF (SETQ DUM (ASSQ (CAR SELS) (ZL-GET M 'SELS)))
	       (CONS M (CONS (CAR SELS) (FSEL (CDR DUM) (CDR SELS)))))))
	(T (DO ((L (ZL-GET M 'SELS) (CDR L)) (DUM)) ((NULL L))
	       (IF (SETQ DUM (FSEL (CDAR L) SELS))
		   (RETURN (CONS M (CONS (CAAR L) DUM))))))))

;(DEFUN SELECTOR (X &optional env) env
;  (IF (NULL (CDDR X)) `(SEL ,(CADR X) ,(CAR X))
;      `(_ (SEL ,(CADR X) ,(CAR X)) ,(CADDR X))))
(DEFUN SELECTOR (X #+cl env) env
  (IF (NULL (CDDR X)) `(SEL ,(CADR X) ,(CAR X))
      `(_ (SEL ,(CADR X) ,(CAR X)) ,(CADDR X))))


(defMACRO _ (&rest X)
  (setq X (cons ' _ X ))
  `(STO . ,(CDR X)))

(defMACRO STO (&rest X)
  (setq X (cons ' STO X ))
  (DO ((L (CDR X) (CDDR L)) (S) (NL))
      ((NULL L) `(PROGN . ,(NREVERSE NL)))
      (COND ((ATOM (CAR L)) (SETQ NL (CONS `(SETQ ,(CAR L) ,(CADR L)) NL)))
	    ((AND (EQ 'SEL (CAAR L)) (SETQ S (FSEL (MODE (CADAR L)) (CDDAR L))))
	     (SETQ X (CADAR L))
	     (DO ((L (CDDR S) (CDDR L))) ((NULL (CDR L)))
		 (SETQ X (CONS (CADR (ZL-GET (CAR L) 'MODE)) (RPLACA L X)))
		 (RPLACD (CDDR X) NIL))
	     (SETQ NL (CONS (LIST (CADDR (ZL-GET (CAR S) 'MODE)) X (CADR S) (CADR L)) NL)))
	    (T (IA-ERR (CAR L))))))

;; (C-ATOM '(AGE WEIGHT MARRIED) '(21 130 NIL)) creates a plist-structure
;; with slot names as properties.  This should use SETPLIST instead
;; of RPLACD.
;; None of these functions are needed at compile time.

;; (DEFUN C-ATOM (SELS ARGS)
;;   (DO ((NL)) ((NULL SELS) (RPLACD (INTERN (GENSYM)) (NREVERSE NL)))
;;       (IF (CAR ARGS) (SETQ NL (CONS (CAR ARGS) (CONS (CAR SELS) NL))))
;;       (SETQ SELS (CDR SELS) ARGS (CDR ARGS))))

;; (DEFUN A-ATOM (BAS SEL VAL)
;;   (COND ((NULL VAL) (REMPROP BAS SEL) NIL)
;; 	(T (PUTPROP BAS VAL SEL))))

;; (DEFUN DSSQ (X L)
;;   (DO () ((NULL L))
;;       (COND ((EQ X (CDAR L)) (RETURN (CAR L)))
;; 	    (T (SETQ L (CDR L))))))


(DEFMACRO CONS-EXP (OP . ARGS) `(SIMPLIFY (LIST (LIST ,OP) . ,ARGS)))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; End:



