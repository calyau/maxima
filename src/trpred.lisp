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

(macsyma-module trpred)

; $is, $maybe or mcond
(defvar wrap-a-pred '$is)

(defun wrap-pred (form &optional (evalp t))
  (let ((boole-fun (get wrap-a-pred
                        (if evalp
                            'tr-boole-eval
                            'tr-boole-verify))))
    (cons '$any `(,boole-fun ,form))))

(defun tr-is/maybe (wrap-type form)
  (let* ((wrap-a-pred wrap-type)
         (tr (translate-predicate form)))
    (destructuring-bind (mode . tr-form) tr
      (if (eq mode '$boolean)
          tr
          (cons '$any tr-form)))))

(def%tr $is (form)
  (tr-is/maybe '$is (cadr form)))

(def%tr $maybe (form)
  (tr-is/maybe '$maybe (cadr form)))

;;; these don't have an imperitive predicate semantics outside of
;;; being used in MNOT, MAND, MOR, MCOND, $IS.

(def%tr mnotequal (form)
  `($any . (simplify (list '(,(caar form)) ,@(tr-args (cdr form))))))

(def-same%tr mequal    mnotequal)
(def-same%tr $equal    mnotequal)
(def-same%tr $notequal mnotequal)
(def-same%tr mgreaterp mnotequal)
(def-same%tr mgeqp     mnotequal)
(def-same%tr mlessp    mnotequal)
(def-same%tr mleqp     mnotequal)

;;; It looks like it was copied from MRG;COMPAR > with 
;;; TRP- substituted for MEVALP. What a crockish way to dispatch,
;;; and in a system with a limited address space too!
;;; NOTE: See code for IS-BOOLE-CHECK, also duplication of MRG;COMPAR.

;;; Note: This TRANSLATE-PREDICATE and TRANSLATE should be combined
;;; to a single function which takes a second argument of the
;;; TARGET (mode). Targeting is a pretty basic concept in compilation
;;; so its surprising this was done. In order to make this change all
;;; special-forms need to do targeting.

(defun translate-predicate (form)
  (cond ((atom form) (trp-with-boolean-convert form))
	((eq 'mnot (caar form)) (trp-mnot form))
	((eq 'mand (caar form)) (trp-mand form))
	((eq 'mor (caar form)) (trp-mor form))
	((eq 'mnotequal (caar form)) (trp-mnotequal form))
	((eq 'mequal (caar form)) (trp-mequal form))
	((eq '$equal (caar form)) (trp-$equal form))
	((eq '$notequal (caar form)) (trp-$notequal form))
	((eq 'mgreaterp (caar form)) (trp-mgreaterp form))
	((eq 'mgeqp (caar form)) (trp-mgeqp form))
	((eq 'mlessp (caar form)) (trp-mlessp form))
	((eq 'mleqp (caar form)) (trp-mleqp form))
	((eq 'mprogn (caar form))
	 ;; it was a pain not to have this case working, so I just
	 ;; patched it in. Lets try not to lazily patch in every
	 ;; special form in macsyma!
	 (let ((exprs (cdr form)))
	   (destructuring-bind (mode . last)
	       (translate-predicate (car (last exprs)))
	     (cons mode
		   `(progn
		      ,@(tr-args (butlast exprs))
		      ,last)))))
	(t (trp-with-boolean-convert form))))

(defun trp-with-boolean-convert (form)
  (let ((tr (translate form)))
    (destructuring-bind (mode . exp) tr
      (if (eq mode '$boolean)
          tr
          (wrap-pred exp)))))

(defmacro mnot_tr (operand)
  `(is-mnot #'identity ,operand))

(defun trp-mnot (form) 
  (let ((exp (cadr form)))
    (cond ((not exp)
           (cons '$boolean t))
          ((eq t exp)
           (cons '$boolean nil))
          ((and (not (atom exp)) (eq (caar exp) 'mnot))
           (translate-predicate (cadr exp)))
          (t
           (destructuring-bind (mode . operand) (translate-predicate exp)
             (if (eq mode '$boolean)
                 (cons mode (list 'not operand))
                 (wrap-pred (list 'mnot_tr operand) nil)))))))

(defun mand/mor_tr (mop operands top bot)
  (let ((val (tr-gensym))
        (ext (tr-gensym)))
    `(let ((,val nil)
           (,ext '()))
       ,(reduce (lambda (x acc)
                  `(cond ((eq (setq ,val ,x) ,bot)
                          ,bot)
                         (t
                          (unless (eq ,val ,top)
                            (push ,val ,ext))
                          ,acc)))
                operands
                :from-end t
                :initial-value
                  `(cond ((null ,ext)
                          ,top)
                         ((null (cdr ,ext))
                          (car ,ext))
                         (t
                          (cons '(,mop) (nreverse ,ext))))))))

(defmacro mand_tr (&rest operands)
  (mand/mor_tr 'mand operands t nil))

(defmacro mor_tr (&rest operands)
  (mand/mor_tr 'mor operands nil t))

(defun simplify-mand/mor-operands_tr (operands top bot)
  (loop for o in operands unless (eq o top) collect o until (eq o bot)))

(defun map-trp (l)
  (reduce (lambda (x a)
            (destructuring-bind (mode . body) (translate-predicate x)
              (cons (*union-mode (car a) mode) (cons body (cdr a)))))
          l
          :from-end t
          :initial-value (cons nil '())))

(defun trp-mand/mor (operands lisp-op max-op top bot)
  (let ((operands (simplify-mand/mor-operands_tr operands top bot)))
    (cond ((null operands)
           (cons '$boolean top))
          ((null (cdr operands))
           (trp-with-boolean-convert (car operands)))
          (t
           (destructuring-bind (mode . tr-operands) (map-trp operands)
             (if (eq mode '$boolean)
                 (cons mode (cons lisp-op tr-operands))
                 (wrap-pred (cons max-op tr-operands) nil)))))))

(defun trp-mand (form)
  (trp-mand/mor (cdr form) 'and 'mand_tr t nil))

(defun trp-mor (form)
  (trp-mand/mor (cdr form) 'or 'mor_tr nil t))

(defvar *number-types* '($float $number $fixnum ))

(defun trp-inequality (args lisp-op max-op)
  (let* ((arg1 (translate (car args)))
         (arg2 (translate (cadr args)))
         (mode (*union-mode (car arg1) (car arg2))))
    (cond ((or (member mode '($fixnum $float) :test #'eq)
               (and (member (car arg1) *number-types* :test #'eq)
                    (member (car arg2) *number-types* :test #'eq)))
           `($boolean . (,lisp-op ,(dconv arg1 mode) ,(dconv arg2 mode))))
          ((eq '$number mode)
           `($boolean . (,lisp-op ,(cdr arg1) ,(cdr arg2))))
          (t
           (wrap-pred `(,max-op ,(dconvx arg1) ,(dconvx arg2)) nil)))))

(defun trp-mlessp (form)
  (trp-inequality (cdr form) '< 'mlsp))

(defun trp-mgreaterp (form)
  (trp-inequality (cdr form) '> 'mgrp))

(defun trp-mgeqp (form)
  (trp-inequality (cdr form) '>= 'mgqp))

(defun trp-mleqp (form)
  ; No mlqp in sight
  (translate-predicate `((mgeqp) ,@(reverse (cdr form)))))

(defun trp-mequal (form) 
  (destructuring-let (((mode1 . arg1) (translate (cadr form)))
                      ((mode2 . arg2) (translate (caddr form))))
    (cons '$boolean
          (if (and (covers '$number mode1) (covers '$number mode2))
              `(eql ,arg1 ,arg2)
              `(like ,arg1 ,arg2)))))

(defun trp-mnotequal (form)
  (translate-predicate `((mnot) ((mequal) ,@(cdr form)))))

(defun trp-$equality (args lisp-op max-op)
  (let* ((arg1 (translate (car args)))
         (arg2 (translate (cadr args)))
         (mode (*union-mode (car arg1) (car arg2))))
    (cond ((member mode '($fixnum $float) :test #'eq)
           `($boolean . (,lisp-op ,(dconv arg1 mode) ,(dconv arg2 mode))))
          ((eq '$number mode)
           `($any . (,max-op ,(cdr arg1) ,(cdr arg2))))
          (t
           (wrap-pred `(,max-op ,(dconvx arg1) ,(dconvx arg2)) nil)))))

(defun trp-$equal (form) 
  (trp-$equality (cdr form) '= 'meqp))

(defun trp-$notequal (form)
  (trp-$equality (cdr form) '/= 'mnqp))

;;; sigh, i have to copy a lot of the $assume function too.

(def%tr $assume (form)
  (let ((x (cdr form)))
    (do ((nl))
	((null x)
	 `($any . (simplify (list '(mlist) ,@(nreverse nl)))))
      (cond ((atom (car x))
	     (setq nl (cons `(assume ,(dtranslate (car x))) nl)))
	    ((eq 'mand (caaar x))
	     (mapc #'(lambda (l) (setq nl (cons `(assume ,(dtranslate l)) nl)))
		   (cdar x)))
	    ((eq 'mnot (caaar x))
	     (setq nl (cons `(assume ,(dtranslate (pred-reverse (cadar x)))) nl)))
	    ((eq 'mor (caaar x))
	     (merror (intl:gettext "assume: argument cannot be an 'or' expression; found ~M") (car x)))
	    ((eq (caaar x) 'mequal)
	     (merror (intl:gettext "assume: argument cannot be an '=' expression; found ~M~%assume: maybe you want 'equal'.") (car x)))
	    ((eq (caaar x) 'mnotequal)
	     (merror (intl:gettext "assume: argument cannot be a '#' expression; found ~M~%assume: maybe you want 'not equal'.") (car x)))
	    ('else
	     (setq nl (cons `(assume ,(dtranslate (car x))) nl))))
      (setq x (cdr x)))))
