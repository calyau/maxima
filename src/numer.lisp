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
(macsyma-module numer)
(load-macsyma-macros numerm)

;;; Interface of lisp numerical routines to macsyma.
;;; 4:34pm  Thursday, 28 May 1981 - George Carrette.

(defun compatible-array-type? (type type-list)
  (declare (ignore type-list))
  ;;  #+MACLISP
  ;;  (MEMQ TYPE TYPE-LIST)
  ;;  #+NIL
  ;;  (memq (or (cdr (assq type '((double-float . flonum))))
  ;;	    type)
  ;;	type-list)
  (eq type t))

(defmfun get-array (x &optional (kinds nil) (/#-dims) &rest dimensions)
  ;;  "Get-Array is fairly general.
  ;;  Examples:
  ;;  (get-array ar '(flonum) 2 3 5) makes sure ar is a flonum array
  ;;  with 2 dimensions, of 3 and 5.
  ;;  (get-array ar '(fixnum) 1) gets a 1 dimensional fixnum array."
  (cond ((null kinds) (get-array-pointer x))
	((null /#-dims)
	 (let ((a  (get-array-pointer x)))
	   (if (compatible-array-type? (array-type a) kinds)
	       a
	       (merror "~:M is not an array of type: ~:M"  x
		       `((mlist) ,@kinds)))))
	((null dimensions)
	 (let ((a (get-array x kinds)))
	   (if (= (array-rank a) /#-dims)
	       a
	       (merror "~:M does not have ~:M dimensions." x /#-dims))))
	('else
	 (let ((a (get-array x kinds /#-dims)))
	   (do ((j 1 (f1+ j))
		(l dimensions (cdr l)))
	       ((null l)
		a)
	     (or (or (eq (car l) '*)
		     (= (car l) (array-dimension-n j a)))
		 (merror "~:M does not have dimension ~:M equal to ~:M"
			 x j (car l))))))))

(declare-top (special %e-val))

(defun mto-float (x)
  (float (if (numberp x)
	     x
	     (let (($numer t) ($float t))
	       (resimplify (subst %e-val '$%e x))))))

;;; Trampolines for calling with numerical efficiency.

(defvar tramp$-alist ())

(defmacro deftramp$ (nargs)
  (let ((tramp$ (symbolconc 'tramp nargs '$))
	;;	#+MACLISP
	;;	(TRAMP$-S (SYMBOLCONC 'TRAMP NARGS '$-S))
	(tramp$-f (symbolconc 'tramp nargs '$-f))
	(tramp$-m (symbolconc 'tramp nargs '$-m))
	(l (make-list nargs)))
    (let ((arg-list (mapcar #'(lambda (ign)ign (gensym)) l))
	  ;;	  #+MACLISP
	  ;;	  (ARG-TYPE-LIST (MAPCAR #'(LAMBDA (IGNORE) 'flonum) L))
	  )
      `(progn				;'COMPILE
	(push '(,nargs ,tramp$
		;;		    #+MACLISP ,TRAMP$-S
		,tramp$-f ,tramp$-m)
	 tramp$-alist)
	(defmvar ,tramp$ "Contains the object to jump to if needed")
	;;	    #+MACLISP
	;;	    (DECLARE-top (FLONUM (,TRAMP$-S ,@ARG-TYPE-LIST)
	;;			     (,TRAMP$-F ,@ARG-TYPE-LIST)
	;;			     (,TRAMP$-M ,@ARG-TYPE-LIST)))
	;;	    #+MACLISP
	;;	    (DEFUN ,TRAMP$-S ,ARG-LIST
	;;	      (FLOAT (SUBRCALL NIL ,TRAMP$ ,@ARG-LIST)))
	(defun ,tramp$-f ,arg-list
	  (float (funcall ,tramp$ ,@arg-list)))
	(defun ,tramp$-m ,arg-list
	  (float (mapply1 ,tramp$ (list ,@arg-list) ',tramp$ nil)))))))

(deftramp$ 1)
(deftramp$ 2)
(deftramp$ 3)

(defmfun make-tramp$ (f n)
  (let ((l (assoc n tramp$-alist :test #'equal)))
    (if (null l)
	(merror "BUG: No trampoline of argument length ~M" n))
    (pop l)
    (let (tramp$			;#+maclisp tramp$-s
	  tramp$-m tramp$-f)
      (declare (special tramp$ tramp$-m tramp$-f ))
      (setq tramp$ (pop l)
	    ;;	       #+maclisp TRAMP$-S #+maclisp (POP L)
	    tramp$-f (pop l)
	    tramp$-m (pop l))
      (let ((whatnot (funtypep f)))
	(case (car whatnot)
	  ((operators)
	   (set tramp$ f)
	   (getsubr! tramp$-m))
   	  ((mexpr)
	   (set tramp$ (cadr whatnot))
	   (getsubr! tramp$-m))
	  ;;	  #+MACLISP
	  ;;	  ((SUBR)
	  ;;	   (COND ((SHIT-EQ (CADR WHATNOT) (GETSUBR! TRAMP$-S))
	  ;;		  ;; This depends on the fact that the lisp compiler
	  ;;		  ;; always outputs the same first instruction for
	  ;;		  ;; "flonum compiled" subrs.
	  ;;		  (CADR WHATNOT))
	  ;;		 ('ELSE
	  ;;		  (SET TRAMP$ (CADR WHATNOT))
	  ;;		  (GETSUBR! TRAMP$-S))))
	  ((expr lsubr)
	   (set tramp$ (cadr whatnot))
	   (getsubr! tramp$-f))
	  (t
	   (merror "Undefined or inscrutable function~%~M" f)))))))


(defun getsubr! (x)
  (or					;#+MACLISP (GET X 'SUBR)
   (and (symbolp x) (fboundp x) (symbol-function x))
   (getsubr! (maxima-error "No subr property for it!" x 'wrng-type-arg))))

(defun funtypep (f)
  (cond ((symbolp f)
	 (let ((mprops (mgetl f '(mexpr)))
	       (lprops		;#+MACLISP (GETL F '(SUBR LSUBR EXPR))
		(and (fboundp f)
		     (list 'expr (symbol-function f)))))
	   (or (if $transrun
		   (or lprops mprops)
		   (or mprops lprops))
	       (getl f '(operators)))))
	((functionp f)
	 (list 'expr f))
	((consp f)			;(EQ (TYPEP F) 'LIST)
	 (list (if (memq (car f) '(function lambda named-lambda))
		   'expr
		   'mexpr)
	       f))
	('else
	 nil)))

;;#+MACLISP
;;(DEFUN SHIT-EQ (X Y) (= (EXAMINE (MAKNUM X)) (EXAMINE (MAKNUM Y))))

;; For some purposes we need a more general trampoline mechanism,
;; not limited by the need to use a special variable and a
;; BIND-TRAMP$ mechanism.

;; For now, we just need the special cases F(X), and F(X,Y) for plotting,
;; and the hackish GAPPLY$-AR$ for systems of equations.

(defun make-gtramp$ (f nargs)
  nargs
  ;; for now, ignoring the number of arguments, but we really should
  ;; do this error checking.
  (let ((k (funtypep f)))
    (case (car k)
      ((operators)
       (cons 'operators f))
      ;;      #+MACLISP
      ;;      ((SUBR)
      ;;       (IF (SHIT-EQ (CADR K) (GETSUBR! 'TRAMP1$-S))
      ;;	   (CONS 'SUBR$ (CADR K))
      ;;	   (CONS 'SUBR (CADR K))))
      ((mexpr expr lsubr)
       (cons (car k) (cadr k)))
      (t
       (merror "Undefined or inscrutable function~%~M" f)))))

(defun gcall1$ (f x)
  (case (car f)
    ;;    #+MACLISP
    ;;    ((SUBR$)
    ;;     (SUBRCALL FLONUM (CDR F) X))
    ;;    #+MACLISP
    ;;    ((SUBR)
    ;;     (FLOAT (SUBRCALL NIL (CDR F) X)))
    ;;    #+MACLISP
    ;;    ((LSUBR)
    ;;     (FLOAT (LSUBRCALL NIL (CDR F) X)))
    ((expr)
     (float (funcall (cdr f) x)))
    ((mexpr operators)
     (float (mapply1 (cdr f) (list x) nil nil)))
    (t
     (merror "BUG: GCALL1$"))))

(defun gcall2$ (f x y)
  (case (car f)
    ;;    #+MACLISP
    ;;    ((SUBR$)
    ;;     (SUBRCALL FLONUM (CDR F) X Y))
    ;;    #+MACLISP
    ;;    ((SUBR)
    ;;     (FLOAT (SUBRCALL NIL (CDR F) X Y)))
    ;;    #+MACLISP
    ;;    ((LSUBR)
    ;;     (FLOAT (LSUBRCALL NIL (CDR F) X Y)))
    ((expr)
     (float (funcall (cdr f) x y)))
    ((mexpr operators)
     (float (mapply (cdr f) (list x y) nil)))
    (t
     (merror "BUG: GCALL2$"))))

(defun ar$+ar$ (a$ b$ c$)
  (do ((n (array-dimension-n 1 a$))
       (j 0 (f1+ j)))
      ((= j n))
    (declare (fixnum n j))
    (setf (aref$ a$ j) (+$ (aref$ b$ j) (aref$ c$ j)))))

(defun ar$*s (a$ b$ s)
  (do ((n (array-dimension-n 1 a$))
       (j 0 (f1+ j)))
      ((= j n))
    (declare (fixnum n j))
    (setf (aref$ a$ j) (*$ (aref$ b$ j) s))))

(defun ar$gcall2$ (ar fl x y)
  (do ((j 0 (f1+ j))
       (l fl (cdr l)))
      ((null l))
    (setf (aref$ ar j) (gcall2$ (car l) x y))))

(defun make-gtramp (f nargs)
  nargs
  ;; for now, ignoring the number of arguments, but we really should
  ;; do this error checking.
  (let ((k (funtypep f)))
    (case (car k)
      ((operators)
       (cons 'operators f))
      ;;      #+MACLISP
      ;;      ((SUBR)
      ;;       (CONS 'SUBR (CADR K)))
      ((mexpr expr lsubr)
       (cons (car k) (cadr k)))
      (t
       (merror "Undefined or inscrutable function~%~M" f)))))

(defun gcall3 (f a1 a2 a3)
  (case (car f)
    ;;    #+MACLISP
    ;;    ((SUBR)
    ;;     (SUBRCALL T (CDR F) A1 A2 A3))
    ;;    #+MACLISP
    ;;    ((LSUBR)
    ;;     (LSUBRCALL T (CDR F) A1 A2 A3))
    ((expr)
     (funcall (cdr f)  a1 a2 a3))
    ((mexpr operators)
     (mapply (cdr f) (list a1 a2 a3) 'gcall3))
    (t
     (merror "BUG: GCALL3"))))
