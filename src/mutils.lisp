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
         (MERROR "Improper form for list:~%~M" ielist))))

;;; This function works like the every function in lisp.
;;; It can take a list, or a positive number of arguments returning
;;; true if all its arguments are not false.
;;; Author Dan Stanger 12/1/02
(defmfun $every (&rest args)
  (let ((n (length args)))
     (cond ((= n 0) (merror "Every must have at least 1 argument"))
           ((= n 1)
               (let ((args (first args)))
                  (if (and ($listp args) (> ($length args) 0))
                      (notany #'not (margs args))
                      (if (and ($listp args) (= ($length args) 0)) nil args))))
           (t (notany #'not args)))))

;;; (ASSOL item A-list)
;;;
;;;  Like ASSOC, but uses ALIKE1 as the comparison predicate rather
;;;  than EQUAL.
;;;
;;;  Meta-Synonym:	(ASS #'ALIKE1 ITEM ALIST)

(DEFMFUN ASSOL (ITEM ALIST)
  (DOLIST (PAIR ALIST)
	  (IF (ALIKE1 ITEM (CAR PAIR)) (RETURN PAIR))))
;;; 

(DEFMFUN ASSOLIKE (ITEM ALIST) 
  (CDR (ASSOL ITEM ALIST)))

; Old ASSOLIKE definition:
;
; (defun assolike (e l) 
;	 (prog nil 
;	  loop (cond ((null l) (return nil))
;		     ((alike1 e (caar l)) (return (cdar l))))
;	       (setq l (cdr l))
;	       (go loop)))

;;; (MEM #'ALIKE1 X L)

(DEFMFUN MEMALIKE (X L)
  (DO ((L L (CDR L))) ((NULL L))
      (COND ((ALIKE1 X (CAR L)) (RETURN L)))))

;;;Do we want MACROS for these on MC and on Multics?? -Jim 1/29/81
#+Multics
(PROGN 'COMPILE
  (DEFMFUN MSTRINGP (X)
    (AND (SYMBOLP X)
	 (EQUAL (GETCHARN X 1) #\&)))

  (DEFMFUN MSTRING-TO-STRING (X)
    (SUBSTRING (STRING X) 1))

  (DEFMFUN STRING-TO-MSTRING (X)
    (MAKE-SYMBOL (STRING-APPEND "&" X)))
)

