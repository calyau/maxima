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

(macsyma-module maxmac macro)

;; This file contains miscellaneous macros used in Macsyma source files.

;; General purpose macros which are used in Lisp code, but not widely enough
;; accepted to be a part of Lisp systems.

;; All these updating macros should be made from the same generalized
;; push/pop scheme as I mentioned to LispForum. As they are defined now
;; they have inconsistent return-values and multiple-evaluations of
;; arguments. -gjc

(defmacro addl (item list)
  `(or (member ,item ,list :test #'eq)
    (setq ,list (cons ,item ,list))))

(defmacro increment (counter &optional increment)
  (if increment
      `(setf ,counter (+ ,counter ,increment))
      `(setf ,counter (1+ ,counter))))

(defmacro decrement (counter &optional decrement)
  (if decrement
      `(setf ,counter (- ,counter ,decrement))
      `(setf ,counter (1- ,counter))))

;; 'writefilep' and 'ttyoff' are system independent ways of expressing
;; the Maclisp ^R and ^W.

(defvar writefilep '^r)
(defvar ttyoff    '^w)

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

;; Use this instead of GETL when looking for "function" properties,
;; i.e. one of EXPR, SUBR, LSUBR, FEXPR, FSUBR, MACRO.
;; Use FBOUNDP, SYMBOL-FUNCTION, or FMAKUNBOUND if possible.

(defmacro getl-fun (fun l)
  `(getl-lm-fcn-prop ,fun ,l))

;; Non-destructive versions of DELQ and DELETE.  Already part of NIL
;; and LMLisp.  These should be rewritten as SUBRS and placed
;; in UTILS.  The subr versions can be more memory efficient.

(defmacro remq (item list &optional (count () counting?))
  `(remove ,item ,list :test 'eq ,@ (and counting? `(:count ,count))))

;; (EXCH A B) exchanges the bindings of A and B
;; Maybe it should turn into (PSETF A B B A)?

(defmacro exch (x y)
  `(setf ,x (prog1 ,y (setf ,y ,x))))

;; These are here for old code only.
;; Better, use DEFSTRUCT.

(defmacro caddadr (x)
  `(car (cddadr ,x)))

;; The following macros pertain only to Macsyma.

;; Except on the Lisp Machine, load the specified macro files.
;; On the Lisp Machine, the DEFSYSTEM facility is used for loading
;; macro files, so just check that the file is loaded. This is
;; a useful error check, has saved a lot of time since Defsystem
;; is far from fool-proof. See LMMAX;SYSDEF for the Lispm
;; definition of MACSYMA-MODULE.

(defun load-macsyma-macros-at-runtime (&rest l)
  (mapcar #'(lambda (x)
	      (if (get x 'macsyma-module)
		  x
		  (error  "Missing Maxima macro file -- ~A" x)))
	  l))

(defmacro load-macsyma-macros (&rest macro-files)
  `(comment *macro*files*
    ,(apply #'load-macsyma-macros-at-runtime macro-files)))

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
	    (maxima-error "Bad variable specification: ~a" variable-specification)))))

(defmacro mbinding-sub (variables values function-name &rest body &aux (win (gensym)))
  `(let ((,win nil))
    (unwind-protect
	 (progn
	   (mbind ,variables ,values ,function-name)
	   (setq ,win t)
	   ,@body)
      (if ,win (munbind ,variables)))))


;; For MLISTP its arg is known not to be an atom.
;; Otherwise, just use $listp.
;; MLISTP exists just to support a Franz hack, so you can just 
;;   ignore it. - JPG

(defmacro mlistp (x)
  `(eq (caar ,x) 'mlist))

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

(defmacro real-infinityp (x)
  `(member ,x real-infinities :test #'eq))

(defmacro infinityp (x)
  `(member ,x infinities :test #'eq))

(defmacro real-epsilonp (x)
  `(member ,x infinitesimals :test #'eq))

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

(defmacro defmspec (function . rest)
  `(progn
    (defun-prop (,function mfexpr*) . ,rest)))

(defmacro sys-user-id ()
  '(status userid))

;; Setf hacking.

(defmfun mget (atom ind)
  (let ((props (and (symbolp atom) (get atom 'mprops))))
    (and props (getf (cdr props) ind))))

(defsetf mget (sym tag) (value)
  `(mputprop ,sym ,value ,tag))

(defmacro old-get (plist tag)
  `(getf (cdr ,plist) ,tag))

(defmfun $get (atom ind) (prop1 '$get atom nil ind))

(defsetf $get (sym tag) (value)
  `($put ,sym ,value ,tag))

(defmacro *break (breakp mess)
  `(apply 'break `(,,mess ,',breakp)))

(defmacro  mdefprop (sym val indicator)
  `(mputprop ',sym ',val ',indicator))

(defmfun mputprop (atom val ind)
  (let ((props (get atom 'mprops)))
    (if (null props) (putprop atom (setq props (ncons nil)) 'mprops))
    (putprop props val ind)))
