;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module grind)

(declare-top (special lop rop *grind-charlist*))

(defmspec $grind (x)
  (setq x (cdr x))
  (let (($lispdisp t) y)
    (fresh-line)
    (cond
      ((null x))
      ((cdr x) (mapc #'(lambda (xx) (funcall (get '$grind 'mfexpr*) `(($grind) ,xx))) x))
      ((or
         (symbolp (setq x (strmeval (car x))))
         (and (stringp x) (symbolp (getopr x))))
       (setq x ($verbify x))
       (cond ((setq y (mget x 'mexpr))
	      (mgrind (list '(mdefine) (cons (list x) (cdadr y)) (caddr y)) nil))
	     ((setq y (mget x 'mmacro))
	      (mgrind (list '(mdefmacro) (cons (list x) (cdadr y)) (caddr y)) nil))
	     ((setq y (mget x 'aexpr))
	      (mgrind (list '(mdefine) (cons (list x 'array) (cdadr y)) (caddr y)) nil))
	     (t (mgrind x nil)))
       (write-char #\$ nil) (write-char #\Newline nil))
      (t (mgrind x nil) (write-char #\$ nil) (write-char #\Newline nil)))
    '$done))

;;Msize returns a list whose first member is the number of characters
;;in the printed representation of the rest of the list.

(defun i-$grind (x)
  (let (y)
    (fresh-line)
    (cond  ((symbolp (setq x (strmeval  x)))
	    (setq x ($verbify x))
	    (cond ((setq y (mget x 'mexpr))
		   (mgrind (list '(mdefine) (cons (list x) (cdadr y)) (caddr y)) nil))
		  ((setq y (mget x 'mmacro))
		   (mgrind (list '(mdefmacro) (cons (list x) (cdadr y)) (caddr y)) nil))
		  ((setq y (mget x 'aexpr))
		   (mgrind (list '(mdefine) (cons (list x 'array) (cdadr y)) (caddr y)) nil))
		  (t (mgrind x nil)))
	    (write-char #\$ nil))
	   (t (mgrind x nil) (write-char #\$ nil)))
    '$done))


(defun strgrind (x)
  (let (*grind-charlist* (*chrps* 0))
    (strprint (msize x nil nil 'mparen 'mparen))
    (nreverse *grind-charlist*)))

(defun strprint (x)
  (cond ((atom x) (styo x))
	((< (car x) (chrct*)) (mapc #'strprint (cdr x)))
	(t (prog (i)
	      (setq i *chrps*)
	      (strprint (cadr x))
	      (cond ((null (cddr x)) (return nil))
		    ((and (or (atom (cadr x)) (< (caadr x) (chrct*)))
			  (or (> (chrct*) (truncate $linel 2))
			      (atom (caddr x)) (< (caaddr x) (chrct*))))
		     (setq i *chrps*)
		     (strprint (caddr x)))
		    (t (setq i (1+ i)) (setq *chrps* 0) (sterpri)
		       (styotbsp i) (strprint (caddr x))))
	      (do ((l (cdddr x) (cdr l))) ((null l))
		(cond
		  ((or (atom (car l)) (< (caar l) (chrct*))) nil)
		  (t (setq *chrps* 0) (sterpri) (styotbsp i)))
		(strprint (car l)))))))

(defun styo (x) (setq *grind-charlist* (cons x *grind-charlist*) *chrps* (1+ *chrps*)))

(defun sterpri () (setq *grind-charlist* (cons #\newline *grind-charlist*) *chrps* 0))

(defun styotbsp (n) (declare (fixnum n)) (setq *chrps* n)
       (do () ((< n 1)) (setq *grind-charlist* (cons #\space *grind-charlist*) n (1- n))))

(defun mstring (x)
  (nreverse (string1 (msize x nil nil 'mparen 'mparen) nil)))

(defun string1 (x l)
  (cond
    ((atom x) (cons x l))
    (t (setq x  (cdr x))
       (do () ((null x) l) (setq l (string1 (car x) l) x (cdr x))))))

;;#-cl
;;(DEFUN ALPHANUMP (N) (DECLARE (FIXNUM N))
;;  (OR (ASCII-NUMBERP N) (ALPHABETP N)))

;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------


(defprop mfunction 190. lbp)
(defprop mfunction 190. rbp)
