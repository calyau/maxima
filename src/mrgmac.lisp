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
	((consp sym) (getf (cdr sym) tag))))

#-lispm
(defmacro fix-lm (&rest body)
  `(progn . ,body))

;;#+LISPM
;;(DEFMACRO FIX-LM (&BODY BODY)
;;  `(LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
;;     . ,BODY))


;; The GRAM and DISPLA packages manipulate lists of fixnums, representing
;; lists of characters.  This syntax facilitates typing them in.
;; {abc} reads as (#/a #/b #/c), unquoted.

;;(DEFUN CHAR-LIST-SYNTAX-ON ()
;;  (FIX-LM
;;    (SETSYNTAX '|{| 'MACRO
;;	       #'(LAMBDA () (DO ((C (TYI) (TYI)) (NL))
;;				((char= #\} C) (NREVERSE NL))
;;			      (SETQ NL (CONS C NL)))))
;;    T))

;;(DEFUN CHAR-LIST-SYNTAX-OFF ()
;;  (FIX-LM nil
;;    #+(OR MACLISP NIL) (SETSYNTAX '|{| 'MACRO NIL)
;;    #+Franz   (setsyntax '|{| 2)
;;    #+LISPM   (SET-SYNTAX-FROM-DESCRIPTION #\{ 'SI:ALPHABETIC)))

;; This sets up the syntax for a simple mode system defined later on
;; in this file.  As usual, it is poorly documented.

;;#-cl
;;(DEFUN MODE-SYNTAX-ON ()
;;  ;; :A:B:C --> (SEL A B C)
;;  ;; A component selection facility.  :A:B:C is like (C (B A)) in the
;;  ;; DEFSATRUCT world.
;;  (FIX-LM
;;    (SETSYNTAX '|:| 'MACRO
;;	       #'(LAMBDA () (DO ((L (LIST (READ)) (CONS (READ) L)))
;;				((NOT (char= #\: (TYIPEEK))) (CONS 'SEL (NREVERSE L)))
;;			      (TYI))))
    
;;    ;; <A B C> --> (SELECTOR A B C)  Used when defining a mode.
;;    (SETSYNTAX '|<| 'MACRO
;;	       #'(LAMBDA ()
;;		   (COND ((char= #\SPACE (TYIPEEK)) '|<|)
;;			 ((char= #\= (TYIPEEK)) (TYI) '|<=|)
;;			 (T (DO ((S (READ) (READ)) (NL))
;;				((EQ '|>| S) (CONS 'SELECTOR (NREVERSE NL)))
;;			      (SETQ NL (CONS S NL)))))))
    
;;    ;; Needed as a single character object.  Used when defining a mode.
;;    (SETSYNTAX '|>| 'MACRO
;;	       #'(LAMBDA ()
;;		   (COND ((NOT (char= #\= (TYIPEEK))) '|>|)
;;			 (T (TYI) '|>=|))))
;;    T))

;;#-cl
;;(DEFUN MODE-SYNTAX-OFF ()
;;  (FIX-LM
;;    #+(OR MACLISP NIL) (PROGN (SETSYNTAX '|:| 'MACRO NIL)
;;			      (SETSYNTAX '|<| 'MACRO NIL)
;;			      (SETSYNTAX '|>| 'MACRO NIL))
;;    #+LISPM (PROGN (SI:SET-SYNTAX-BITS #\: '(0 . 23))
;;		   (SET-SYNTAX-FROM-DESCRIPTION #\> 'SI:ALPHABETIC)
;;		   (SET-SYNTAX-FROM-DESCRIPTION #\< 'SI:ALPHABETIC))
;;    #+Franz (progn (setsyntax '|:| 2)
;;		   (setsyntax '|<| 2)
;;		   (setsyntax '|>| 2))))

;; Loading this file used to turn on the mode syntax.  Its been turned off
;; now and hopefully no files left rely on it.  Files which want to 
;; use that syntax should call (MODE-SYNTAX-ON) during read time.

;;#+MACLISP
;;(DEFUN DEFINE-MACRO (NAME LAMBDA-EXP)
;;    (PUTPROP NAME LAMBDA-EXP 'MACRO))

#+cl
(defun define-macro (name lambda-exp)
  (fix-lm
   (cond ((symbolp lambda-exp) (setq lambda-exp (symbol-function lambda-exp))))
   #-cl(si:record-source-file-name name 'macro)
   #-cl(fset name (cons 'macro lambda-exp))
   #+cl				      ;note need two args for cl macro
   (setf (macro-function name) lambda-exp))
  )

;;#+Franz
;;(defun define-macro (name lambda-exp)
;;  (putd name `(macro (dummy-arg) (,lambda-exp dummy-arg))))

;;#+NIL
;;(DEFUN DEFINE-MACRO (NAME LAMBDA-EXP)
;;  (ADD-MACRO-DEFINITION NAME LAMBDA-EXP))

;; LAMBIND* and PROGB* are identical, similar to LET, but contain an implicit
;; PROG.  On the Lisp Machine, PROG is extended to provide this capability.

(defmacro lambind* (var-list . body) `(let ,var-list (prog nil . ,body)))
(defmacro progb* (var-list . body) `(let ,var-list (prog nil . ,body)))

(defmacro mapand (function list) 
  `(do ((l ,list (cdr l))) ((null l) t)
    (ifn (,function (car l)) (return nil))))

(defmacro mapor (function list)
  `(do ((l ,list (cdr l))) ((null l))
    (if (funcall ,function (car l)) (return t))))

;; (MAPLAC #'1+ '(1 2 3)) --> '(2 3 4), but the original list is rplaca'd
;; rather than a new list being consed up.

(defmacro maplac (function list)
  `(do ((l ,list (cdr l))) ((null l)) (rplaca l (funcall ,function (car l)))))

(defmacro put (a b c) `(putprop ,a ,b ,c))

;;(defmacro zl-REM (a b) `(remprop ,a ,b))

(defmacro copyp (l) `(cons (car ,l) (cdr ,l)))
(defmacro copyl (l) #+nil `(copy-list ,l) #-nil `(append ,l nil))

(defmacro econs (x y) `(append ,x (list ,y)))

#-franz 
(progn 'compile
       (defmacro caaadar (x) `(caaadr (car ,x)))
       (defmacro caaaddr (x) `(caaadr (cdr ,x)))
       (defmacro caadaar (x) `(caadar (car ,x)))
       (defmacro caadadr (x) `(caadar (cdr ,x)))
       (defmacro cadaaar (x) `(cadaar (car ,x)))
       (defmacro cadaddr (x) `(cadadr (cdr ,x)))
       (defmacro caddaar (x) `(caddar (car ,x)))
       (defmacro cadddar (x) `(cadddr (car ,x)))
       (defmacro cdadadr (x) `(cdadar (cdr ,x)))
       (defmacro cdadddr (x) `(cdaddr (cdr ,x)))
       (defmacro cdddddr (x) `(cddddr (cdr ,x))))

(defmacro tell (&rest args) `(displa (list '(mtext) . ,args)))


(declare-top (special name bas mobjects selector) (*expr mode))

(setq mobjects nil)

(defprop mode (c-mode s-mode a-mode) mode)

(defmacro c-mode (&rest l) `(list . ,l))

;;(MACRO S-MODE (X)
;;  (COND ((EQ 'C (CADDR X)) `(CAR ,(CADR X)))
;;	((EQ 'SEL (CADDR X)) `(CADR ,(CADR X)))
;;	((EQ '_ (CADDR X)) `(CADDR ,(CADR X)))))


;;(MACRO A-MODE (X)
;;  (COND ((EQ 'C (CADDR X)) `(RPLACA (CADR X) ,(CADDDR X)))
;;	((EQ 'SEL (CADDR X)) `(RPLACA (CDR ,(CADR X)) ,(CADDDR X)))
;;	((EQ '_ (CADDR X)) `(RPLACA (CDDR ,(CADR X)) ,(CADDDR X)))))

;;(MACRO DEFMODE (X)
;;  (LET ((SELECTOR (MEMQ 'SELECTOR (CDDDDR X))))
;;    (DEFINE-MODE (CADR X) (CADDDR X))
;;    (MAPC 'EVAL (CDDDDR X))
;;    `',(CADR X)))

(defmacro s-mode (&rest x)
  (setq x (cons ' s-mode x ))
  (cond ((eq 'c (caddr x)) `(car ,(cadr x)))
	((eq 'sel (caddr x)) `(cadr ,(cadr x)))
	((eq '_ (caddr x)) `(caddr ,(cadr x)))))


(defmacro a-mode (&rest x)
  (setq x (cons ' a-mode x ))
  (cond ((eq 'c (caddr x)) `(rplaca (cadr x) ,(cadddr x)))
	((eq 'sel (caddr x)) `(rplaca (cdr ,(cadr x)) ,(cadddr x)))
	((eq '_ (caddr x)) `(rplaca (cddr ,(cadr x)) ,(cadddr x)))))

(defmacro defmode (&rest x)
  (setq x (cons ' defmode x ))
  (let ((selector (memq 'selector (cddddr x))))
					;(setq billy `(DEFINE-MODE ,(CADR X) ,(CADDDR X)))
       
    (define-mode (cadr x) (cadddr x))
    (mapc 'eval (cddddr x))
    `',(cadr x)))

#+(or nil cl)
(defun define-mode (name desc &aux #+lispm (default-cons-area working-storage-area))
  
  (prog (c s a)
     (setq
      c (intern (format nil "C-~A" name)) 
      s (intern (format nil "S-~A" name)) 
      a (intern (format nil "A-~A" name)))
					;(setq silly `    (DEFINE-MACRO ,C ,(DEFC DESC))	)
     (define-macro c (defc desc))
     (define-macro s (defs desc))
     (define-macro a (defa desc))
     (put name (c-mode c s a) 'mode)
     (return name)))

#-(or nil cl)
(defun define-mode (name desc)
  (prog (c s a dummy)
     (setq dummy (explodec name)
	   c (implode (append '(c -) dummy))
	   s (implode (append '(s -) dummy))
	   a (implode (append '(a -) dummy)))
     (define-macro c (defc desc))
     (define-macro s (defs desc))
     (define-macro a (defa desc))
     (put name (c-mode c s a) 'mode)
     (return name)))


(defun defc (desc) (let ((bas 'x)) (coerce `(lambda (x &optional env) env
					     ,(defc1 desc)) 'function)))

(defun defc1 (desc)
  (cond ((atom desc) (list 'quote desc))
	((eq 'selector (car desc))
	 (cond ((not (null (cdddr desc))) (list 'quote (cadddr desc)))
	       (t (setq bas (list 'cdr bas))
		  (list 'car bas))))
	((eq (quote atom) (car desc))
	 `(list 'c-atom '',(mapcar 'cadr (cdr desc)) (cons (quote list) (cdr x))))
	((eq 'cons (car desc)) `(list 'cons ,(defc1 (cadr desc)) ,(defc1 (caddr desc))))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l)) (nl))
	     ((null l) `(list (quote list) . ,(nreverse nl)))
	   (setq nl (cons (defc1 (car l)) nl))))
	((eq 'struct (car desc)) (defc1 (cons (quote list) (cdr desc))))
	(t (list 'quote desc))))


(defun defs (desc)
  (coerce `(lambda (x &optional env)env
	    (cond . ,(nreverse (defs1 desc '(cadr x) nil))))'function )) 

(defun defs1 (desc bas result)
  (cond ((atom desc) result)
	((eq 'selector (car desc))
	 (put (cadr desc)
	      (cons (cons name (caddr desc)) (zl-get (cadr desc) 'modes))
	      'modes)
	 (put name
	      (cons (cons (cadr desc) (caddr desc)) (zl-get name 'sels))
	      'sels)
	 (if selector (define-macro (cadr desc) 'selector))
	 (cons `((eq ',(cadr desc) (caddr x)) ,bas) result))
	((eq (quote atom) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (put (cadar l) (cons (cons name (caddar l))
				(zl-get (cadar l) 'modes)) 'modes)
	   (put name (cons (cons (cadar l) (caddar l))
			   (zl-get name 'sels)) 'sels)
	   (if selector (define-macro (cadar l) 'selector)))
	 (cons `((memq (caddr x) ',(mapcar 'cadr (cdr desc)))
		 (list 'zl-get ,bas (list 'quote (caddr x))))
	       result))
	((eq 'cons (car desc))
	 (setq result (defs1 (cadr desc) `(list 'car ,bas) result))
	 (defs1 (caddr desc) `(list 'cdr ,bas) result))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (setq result (defs1 (car l) `(list 'car ,bas) result)
		 bas `(list 'cdr ,bas)))
	 result)
	((eq 'struct (car desc)) (defs1 (cons (quote list) (cdr desc)) bas result))
	(t result)))

(defun defa (desc)
  (coerce `(lambda (x &optional env) env
	    (cond . ,(nreverse (defa1 desc '(cadr x) nil nil)))) 'function))

(defun defa1 (desc bas cdr result)
  (cond ((atom desc) result)
	((eq 'selector (car desc))
	 (setq bas (cond ((not cdr) `(list 'car (list 'rplaca ,(caddr bas) (cadddr x))))
			 (t `(list 'cdr (list 'rplacd ,(caddr bas) (cadddr x))))))
	 (cons `((eq ',(cadr desc) (caddr x)) ,bas) result))
	((eq  (quote atom) (car desc))
	 (list `(t (list 'a-atom (cadr x) (list 'quote (caddr x)) (cadddr x)))))
	((eq 'cons (car desc))
	 (setq result (defa1 (cadr desc) `(list 'car ,bas) nil result))
	 (defa1 (caddr desc) `(list 'cdr ,bas) t result))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (setq result (defa1 (car l) `(list 'car ,bas) nil result)
		 bas `(list 'cdr ,bas)))
	 result)
	((eq 'struct (car desc)) (defa1 (cons (quote list) (cdr desc)) bas cdr result))
	(t result)))


(defun mode (x) (cdr (zl-assoc x mobjects)))

#-nil

(defmacro modedeclare (&rest l)
  `(modeclare-internal ',l))

(defun modedeclare-internal (x)
  (mapc #'(lambda (l) (mapc #'(lambda (v) (push (cons v (car l)) mobjects))
			    (cdr l)))
	x))

;; Do not make this (ERROR 'NDM-ERR).  It won't work on the Lisp machine.

(defun ndm-err (x)
  (terpri)
  (princ "Cannot determine the mode of ") (princ x)
  (maxima-error "Ndm-err"))

(defun nsm-err (x)
  (terpri)
  (princ "No such mode as ") (princ x)
  (maxima-error "Nsm-err"))

(defun sel-err (b s)
  (terpri)
  (tyo #\:) (princ b)
  (do () ((null s)) (tyo #\:) (princ (car s)) (setq s (cdr s)))
  (princ "is an impossible selection")
  (maxima-error "Sel-err"))

(defun ia-err (x)
  (terpri)
  (princ "Cannot assign ") (princ x)
  (maxima-error "Ia-err"))

(defmacro sel (&rest x)
  (setq x (cons ' sel x ))
  (let ((s (fsel (mode (cadr x)) (cddr x))))
    (cond ((null s) (sel-err (cadr x) (cddr x)))
	  (t (setq x (cadr x))
	     (do () ((null (cdr s)) x)
	       (setq x (cons (cadr (zl-get (car s) 'mode)) (rplaca s x)) s (cddr s))
	       (rplacd (cddr x) nil))))))

(defun fsel (m sels) ;;This has a bug in it. 
  (cond ((null sels) (list m))
	((null m)
	 (do ((l (zl-get (car sels) 'modes) (cdr l))) ((null l))
	   (if (setq m (fsel (cdar l) (cdr sels)))
	       (return (cons (caar l) (cons (car sels) m))))))
	((let (dum)
	   (if (setq dum (assq (car sels) (zl-get m 'sels)))
	       (cons m (cons (car sels) (fsel (cdr dum) (cdr sels)))))))
	(t (do ((l (zl-get m 'sels) (cdr l)) (dum)) ((null l))
	     (if (setq dum (fsel (cdar l) sels))
		 (return (cons m (cons (caar l) dum))))))))

;;(DEFUN SELECTOR (X &optional env) env
;;  (IF (NULL (CDDR X)) `(SEL ,(CADR X) ,(CAR X))
;;      `(_ (SEL ,(CADR X) ,(CAR X)) ,(CADDR X))))
(defun selector (x #+cl env) env
       (if (null (cddr x)) `(sel ,(cadr x) ,(car x))
	   `(_ (sel ,(cadr x) ,(car x)) ,(caddr x))))


(defmacro _ (&rest x)
  (setq x (cons ' _ x ))
  `(sto . ,(cdr x)))

(defmacro sto (&rest x)
  (setq x (cons ' sto x ))
  (do ((l (cdr x) (cddr l)) (s) (nl))
      ((null l) `(progn . ,(nreverse nl)))
    (cond ((atom (car l)) (setq nl (cons `(setq ,(car l) ,(cadr l)) nl)))
	  ((and (eq 'sel (caar l)) (setq s (fsel (mode (cadar l)) (cddar l))))
	   (setq x (cadar l))
	   (do ((l (cddr s) (cddr l))) ((null (cdr l)))
	     (setq x (cons (cadr (zl-get (car l) 'mode)) (rplaca l x)))
	     (rplacd (cddr x) nil))
	   (setq nl (cons (list (caddr (zl-get (car s) 'mode)) x (cadr s) (cadr l)) nl)))
	  (t (ia-err (car l))))))

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


(defmacro cons-exp (op . args) `(simplify (list (list ,op) . ,args)))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; End:



