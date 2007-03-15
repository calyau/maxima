;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mrgmac macro)

(defun zl-get (sym tag)
  (cond ((symbolp sym) (get sym tag))
	((consp sym) (getf (cdr sym) tag))))

(defun define-macro (name lambda-exp)
  (cond ((symbolp lambda-exp)
	 (setq lambda-exp (symbol-function lambda-exp))))
  (setf (macro-function name) lambda-exp))

(defmacro put (a b c)
  `(putprop ,a ,b ,c))

(defmacro caddaar (x) `(caddar (car ,x)))

(declare-top (special name bas selector))

(defvar *mobjects* nil)

(defprop mode (c-mode s-mode a-mode) mode)

(defmacro c-mode (&rest l)
  `(list . ,l))

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
  (let ((selector (member 'selector (cddddr x) :test #'eq)))
    (define-mode (cadr x) (cadddr x))
    (mapc 'eval (cddddr x))
    `',(cadr x)))

(defun define-mode (name desc)
  (prog (c s a)
     (setq
      c (intern (format nil "~A-~A" '#:c name))
      s (intern (format nil "~A-~A" '#:s name)) 
      a (intern (format nil "~A-~A" '#:a name)))
     (define-macro c (defc desc))
     (define-macro s (defs desc))
     (define-macro a (defa desc))
     (put name (c-mode c s a) 'mode)
     (return name)))

(defun defc (desc)
  (let ((bas 'x))
    (coerce `(lambda (x &optional env)
	      (declare (ignore env))
	      ,(defc1 desc))
	    'function)))

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
  (coerce `(lambda (x &optional env)
	    (declare (ignore env))
	    (cond . ,(nreverse (defs1 desc '(cadr x) nil))))
	  'function ))

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
	 (cons `((member (caddr x) ',(mapcar 'cadr (cdr desc)) :test #'eq)
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
  (coerce `(lambda (x &optional env)
	    (declare (ignore env))
	    (cond . ,(nreverse (defa1 desc '(cadr x) nil nil))))
	  'function))

(defun defa1 (desc bas cdr result)
  (cond ((atom desc) result)
	((eq 'selector (car desc))
	 (setq bas (cond ((not cdr) `(list 'car (list 'rplaca ,(caddr bas) (cadddr x))))
			 (t `(list 'cdr (list 'rplacd ,(caddr bas) (cadddr x))))))
	 (cons `((eq ',(cadr desc) (caddr x)) ,bas) result))
	((eq  (quote atom) (car desc))
	 (list `(t (list 'cput (cadr x) (cadddr x) (list 'quote (caddr x))))))
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


(defun mode (x)
  (cdr (assoc x *mobjects* :test #'equal)))

(defmacro modedeclare (&rest l)
  `(modeclare-internal ',l))

(defun modedeclare-internal (x)
  (mapc #'(lambda (l)
	    (mapc #'(lambda (v) (push (cons v (car l)) *mobjects*))
		  (cdr l)))
	x))

(defmacro sel (&rest x)
  (setq x (cons ' sel x ))
  (let ((s (fsel (mode (cadr x)) (cddr x))))
    (cond ((null s) (sel-err (cadr x) (cddr x)))
	  (t (setq x (cadr x))
	     (do ()
		 ((null (cdr s)) x)
	       (setq x (cons (cadr (zl-get (car s) 'mode)) (rplaca s x)) s (cddr s))
	       (rplacd (cddr x) nil))))))

(defun fsel (m sels) ;;This has a bug in it.
  (cond ((null sels) (list m))
	((null m)
	 (do ((l (zl-get (car sels) 'modes) (cdr l)))
	     ((null l))
	   (if (setq m (fsel (cdar l) (cdr sels)))
	       (return (cons (caar l) (cons (car sels) m))))))
	((let (dum)
	   (if (setq dum (assq (car sels) (zl-get m 'sels)))
	       (cons m (cons (car sels) (fsel (cdr dum) (cdr sels)))))))
	(t (do ((l (zl-get m 'sels) (cdr l)) (dum))
	       ((null l))
	     (if (setq dum (fsel (cdar l) sels))
		 (return (cons m (cons (caar l) dum))))))))

(defun selector (x env)
  (declare (ignore env))
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

(defmacro cons-exp (op . args)
  `(simplify (list (list ,op) . ,args)))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; End:



