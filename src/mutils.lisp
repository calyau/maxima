;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mutils)

;;; General purpose Macsyma utilities.  This file contains runtime functions 
;;; which perform operations on Macsyma functions or data, but which are
;;; too general for placement in a particular file.
;;;
;;; Every function in this file is known about externally.

;;; This function searches for the key in the left hand side of the input list
;;; of the form [x,y,z...] where each of the list elements is a expression of
;;; a binary operand and 2 elements.  For example x=1, 2^3, [a,b] etc.
;;; The key checked against the first operand and and returns the second
;;; operand if the key is found.
;;; If the key is not found it either returns the default value if supplied or
;;; false.
;;; Author Dan Stanger 12/1/02

(defmfun $assoc (key ielist &optional default)
  (let ((elist (if (listp ielist)
                   (margs ielist)
                   (merror 
                     (intl:gettext "assoc: second argument must be a nonatomic expression; found: ~:M") 
                     ielist))))
    (if (every #'(lambda (x) (and (listp x) (= 3 (length x)))) elist)
	(let ((found (find key elist :test #'alike1 :key #'second)))
	  (if found (third found) default))
	(merror (intl:gettext "assoc: every argument must be an expression of two parts; found: ~:M") ielist))))

;;; (ASSOL item A-list)
;;;
;;;  Like ASSOC, but uses ALIKE1 as the comparison predicate rather
;;;  than EQUAL.
;;;
;;;  Meta-Synonym:	(ASS #'ALIKE1 ITEM ALIST)

(defun assol (item alist)
  (dolist (pair alist)
    (if (alike1 item (car pair)) (return pair))))

(defun assolike (item alist) 
  (cdr (assol item alist)))

;;; (MEMALIKE X L)
;;;
;;;  Searches for X in the list L, but uses ALIKE1 as the comparison predicate
;;;  (which is similar to EQUAL, but ignores header flags other than the ARRAY
;;;  flag)
;;;
;;;  Conceptually, the function is the same as
;;;
;;;    (when (find x l :test #'alike1) l)
;;;
;;;  except that MEMALIKE requires a list rather than a general sequence, so the
;;;  host lisp can probably generate faster code.
(defun memalike (x l)
  (do ((l l (cdr l)))
      ((null l))
    (when (alike1 x (car l)) (return l))))

;;; Return the first duplicate element of the list LIST, or NIL if there
;;; are no duplicates present in LIST.  The function KEY is applied to
;;; each element of the list before comparison (or uses the element itself
;;; if KEY is NIL), and the comparison is done with the function TEST.
;;;
;;; This was written with "small" lists in mind.  The original use case
;;; was finding duplicates in parameter lists of functions, etc.
;;;    - Kris Katterjohn 06/2017
(defun find-duplicate (list &key (test #'eql) key)
  (declare (optimize (speed 3)))
  (declare (type (or function null) key)
           (type function test))
  (let ((seen nil))
    (dolist (e list)
      (let ((i (if key (funcall key e) e)))
        (when (member i seen :test test)
          (return-from find-duplicate e))
        (push i seen)))))

;;; Return a Maxima gensym.
;;;
;;; N.B. Maxima gensyms are interned, so they are not Lisp gensyms.
;;; This function can return the same symbol multiple times, it can
;;; return a symbol that was created and used elsewhere, etc.
;;;
;;; Maxima produces some expressions that contain Maxima gensyms, so
;;; the use of uninterned symbols instead can cause confusion (since
;;; these print like any other symbol).
(defmfun $gensym (&optional x)
  (typecase x
    (null
     (intern (symbol-name (gensym "$G")) :maxima))
    (string
     (intern
       (symbol-name (gensym (format nil "$~a" (maybe-invert-string-case x))))
       :maxima))
    ((integer 0)
     (let ((*gensym-counter* x))
       (intern (symbol-name (gensym "$G")) :maxima)))
    (t
     (merror
       (intl:gettext
         "gensym: Argument must be a nonnegative integer or a string. Found: ~M") x))))
