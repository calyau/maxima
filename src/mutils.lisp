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
(macsyma-module mutils)

;;; General purpose Macsyma utilities.  This file contains runtime functions 
;;; which perform operations on Macsyma functions or data, but which are
;;; too general for placement in a particular file.
;;;
;;; Every function in this file is known about externally.


;;; This function searches for the key in the left hand side of the input list
;;; of the form [x,y,z...] where each of the list elements is a expression of
;;; a binary operand and 2 elements.  For example x=1, 2^3, [a,b] etc.
;;; The key checked againts the first operand and and returns the second
;;; operand if the key is found.
;;; If the key is not found it either returns the default value if supplied or
;;; false.
;;; Author Dan Stanger 12/1/02
(defmfun $assoc (key ielist &optional default)
  (let ((elist (margs ielist)))
    (if (every #'(lambda (x) (= 3 (length x))) elist)
	(let ((found (find key elist :test #'alike1 :key #'second)))
	  (if found (third found) default))
	(merror "Improper form for list:~%~M" ielist))))

;;; (ASSOL item A-list)
;;;
;;;  Like ASSOC, but uses ALIKE1 as the comparison predicate rather
;;;  than EQUAL.
;;;
;;;  Meta-Synonym:	(ASS #'ALIKE1 ITEM ALIST)

(defmfun assol (item alist)
  (dolist (pair alist)
    (if (alike1 item (car pair)) (return pair))))
;;; 

(defmfun assolike (item alist) 
  (cdr (assol item alist)))

;; Old ASSOLIKE definition:
;;
;; (defun assolike (e l) 
;;	 (prog nil 
;;	  loop (cond ((null l) (return nil))
;;		     ((alike1 e (caar l)) (return (cdar l))))
;;	       (setq l (cdr l))
;;	       (go loop)))

;;; (MEM #'ALIKE1 X L)

(defmfun memalike (x l)
  (do ((l l (cdr l))) ((null l))
    (cond ((alike1 x (car l)) (return l)))))

;;;Do we want MACROS for these on MC and on Multics?? -Jim 1/29/81
#+multics
(progn 'compile
       (defmfun mstringp (x)
	 (and (symbolp x)
	      (equal (getcharn x 1) #\&)))

       (defmfun mstring-to-string (x)
	 (substring (string x) 1))

       (defmfun string-to-mstring (x)
	 (make-symbol (string-append "&" x)))
       )

