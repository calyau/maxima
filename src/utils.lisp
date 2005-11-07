;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module utils)

;;; General purpose Lisp utilities.  This file contains runtime functions which
;;; are simple extensions to Lisp.  The functions here are not very general, 
;;; but generalized forms would be useful in future Lisp implementations.
;;;
;;; No knowledge of the Macsyma system is kept here.  
;;;
;;; Every function in this file is known about externally.

(defun maxima-getenv (envvar)
  #+gcl     (si::getenv envvar)
  #+allegro (system:getenv envvar)
  #+(or cmu scl) (cdr (assoc envvar ext:*environment-list* :test #'string=))
  #+sbcl    (sb-ext:posix-getenv envvar)
  #+clisp   (ext:getenv envvar)
  #+mcl     (ccl::getenv envvar)
  )

;; CMUCL needs because when maxima reaches EOF, it calls BYE, not $QUIT.

(defun bye ()
  #+(or cmu scl clisp) (ext:quit)
  #+sbcl           (sb-ext:quit)
  #+allegro        (excl:exit)
  #+mcl            (ccl:quit)
  #+gcl            (lisp:quit)
  )


;;; N.B. this function is different than the lisp machine
;;; and maclisp standard one. (for now).

;;; temporary until the new lispm make-list is installed

(defmfun *make-list (size &optional (val nil) )
  (do ((l nil (cons val l)))
      ((< (setq size (f1- size)) 0) l)))

;;; F is assumed to be a function of two arguments.  It is mapped down L
;;; and applied to consequtive pairs of elements of the list.
;;; Useful for iterating over property lists.

(defmfun map2c (f l)
  (do ((llt l (cddr llt)) (lans))
      ((null llt) lans)
    (setq lans (cons (funcall f (car llt) (cadr llt)) lans))))

;;; (ANDMAPC #'INTEGERP '(1 2 3)) --> T
;;; (ANDMAPC #'INTEGERP '(1 2 A)) --> NIL
;;; (ORMAPC  #'INTEGERP '(1 2 A)) --> T
;;; (ORMAPC  #'INTEGERP '(A B C)) --> NIL

;;; If you want the do loop generated inline rather than doing a function call,
;;; use the macros SOME and EVERY.  See LMLisp manual for more information.
;;; Note that the value returned by ORMAPC is slightly different from that
;;; returned by SOME.

(defmfun andmapc (f l)
  (do ((l l (cdr l)))
      ((null l) t)
    (if (not (funcall f (car l))) (return nil))))

(defmfun ormapc (f l &aux answer)
  (do ((l l (cdr l)))
      ((null l) nil)
    (setq answer (funcall f (car l)))
    (if answer (return answer))))

;;; Like MAPCAR, except if an application of F to any of the elements of L
;;; returns NIL, then the function returns NIL immediately.

(defmfun andmapcar (f l &aux d answer)
  (do ((l l (cdr l)))
      ((null l) (nreverse answer))
    (setq d (funcall f (car l)))
    (if d (push d answer) (return nil))))

;;; Returns T if either A or B is NIL, but not both.

(defmfun xor (a b) (or (and (not a) b) (and (not b) a)))
  
;;; A MEMQ which works at all levels of a piece of list structure.
;;;
;;; Note that (AMONG NIL '(A B C)) is T, however.  This could cause bugs.
;;; > This is false. (AMONG NIL anything) returns NIL always. -kmp

(defmfun among (x l) 
  (cond ((null l) nil)
	((atom l) (eq x l))
	(t (or (among x (car l)) (among x (cdr l)))))) 

;;; Similar to AMONG, but takes a list of objects to look for.  If any
;;; are found in L, returns T.

(defmfun amongl (x l) 
  (cond ((null l) nil)
	((atom l) (memq l x))
	(t (or (amongl x (car l)) (amongl x (cdr l)))))) 

;;; (RECONC '(A B C) '(D E F)) --> (C B A D E F)
;;; Like NRECONC, but not destructive.
;;;
;;; Is this really faster than macroing into (NCONC (REVERSE L1) L2)?
;;; > Yes, it is. -kmp

(defmfun reconc (l1 l2)
  #+nil (revappend l1 l2)
  #-nil (do () ((null l1) l2) (setq l2 (cons (car l1) l2) l1 (cdr l1))))


;;; (FIRSTN 3 '(A B C D E)) --> (A B C)
;;;
;;; *NOTE* Given a negative first arg will work fine with this definition
;;;	   but on LispM where the operation is primitive and defined 
;;;	   differently, bad things will happen. Make SURE it gets a 
;;;	   non-negative arg! -kmp

;;#+(OR PDP10 Franz)
;;(DEFMFUN FIRSTN (N L)
;;  (SLOOP FOR I FROM 1 TO N
;;	FOR X IN L
;;	COLLECT X))

;;; Reverse ASSQ -- like ASSQ but tries to find an element of the alist whose
;;; cdr (not car) is EQ to the object.  To be renamed to RASSQ in the near
;;; future.

(defmfun assqr (object alist)
  (dolist (pair alist)
    (if (eq object (cdr pair)) (return pair))))

;;; Should be open-coded at some point.  (Moved here from RAT;FACTOR)
(defmfun log2 (n) (f1- (haulong n)))

;;; Tries to emulate Lispm/NIL FSET.  Won't work for LSUBRS, FEXPRS, or
;;; FSUBRS.

;;#+PDP10
;;(DEFMFUN FSET (SYMBOL DEFINITION)
;;  (COND ((SYMBOLP DEFINITION)
;;	 (PUTPROP SYMBOL DEFINITION 'EXPR))
;;	((EQ (ml-typep DEFINITION) 'RANDOM)
;;	 (PUTPROP SYMBOL DEFINITION 'SUBR))
;;	((consp DEFINITION)
;;	 (PUTPROP SYMBOL DEFINITION 'EXPR))
;;	(T (MAXIMA-ERROR "Invalid symbol definition - `fset'"
;;		  DEFINITION 'WRNG-TYPE-ARG))))

;;; Takes a list in "alist" form and converts it to one in
;;; "property list" form, i.e. ((A . B) (C . D)) --> (A B C D).
;;; All elements of the list better be conses.

(defmfun dot2l (l)
  (cond ((null l) nil)
	(t (list* (caar l) (cdar l) (dot2l (cdr l))))))

;;; (A-ATOM sym selector value   )
;;; (C-PUT  sym value    selector)
;;;
;;;  They make a symbol's property list look like a structure.
;;;
;;;  If the value to be stored is NIL,
;;;     then flush the property.
;;;     else store the value under the appropriate property.
;;;
;;; >>> Note: Since they do essentially the same thing, one (A-ATOM)
;;; >>>       should eventually be flushed...

(defmfun a-atom (bas sel val) (cput bas val sel))

(defmfun cput (bas val sel)
  (cond ((null val) (zl-remprop bas sel) nil)
	(t (putprop bas val sel))))

;;; This is like the function SYMBOLCONC except that it binds base and *nopoint

(progn 'compile
       (defmfun concat n
	 (let ((*print-base* 10.))
	   (implode (mapcan 'exploden (listify n))))))

;;#+NIL
;;In NIL, symbolconc does indeed effectively bind the base and *nopoint.
;; This definition may not work if more generality is needed (flonums?
;; random Lisp object?)
;;(deff concat
;;    #'symbolconc)

;;#-cl
;;(progn 'compile
;;       (DECLARE (SPECIAL ALPHABET))	; This should be DEFVAR'd somewhere.
;;					; Sigh. -kmp
;;					; It is DEFVAR'd in Nparse-wfs
;;       (DEFMFUN ALPHABETP (N)
;;	 (DECLARE (FIXNUM N))
;;	 (OR (AND (>= N #\A) (<= N #\Z)) ; upper case
;;	     (AND (>= N #\a) (<= N #\z)) ; lower case
;;	     (zl-MEMBER N ALPHABET)))	; test for %, _, or other declared
;;					;    alphabetic characters.

;;       (DEFMFUN ASCII-NUMBERP (NUM)
;;	 (DECLARE (FIXNUM NUM))
;;	 (AND (<= NUM #\9) (>= NUM #\0))))

