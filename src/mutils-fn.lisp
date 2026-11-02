;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra project
;;; (https://sourceforge.net/projects/maxima/)
;;; SPDX-License-Identifier: GPL-2.0-or-later
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  This program is distributed WITHOUT ANY
;;; WARRANTY. See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

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
                     ielist)))
        found)
    (dolist (i elist)
      (unless (and (listp i) (= 3 (length i)))
        (merror (intl:gettext "assoc: elements of the second argument must be an expression of two parts; found: ~:M") i))
      (when (alike1 key (second i))
        (setq found i)
        (return)))
    (if found (third found) default)))

;;; Return a Maxima gensym.
;;;
;;; N.B. Maxima gensyms are interned, so they are not Lisp gensyms.
;;; This function can return the same symbol multiple times, it can
;;; return a symbol that was created and used elsewhere, etc.  I (kjak)
;;; do not think any of this is correct for a function named gensym.
;;;
;;; Maxima produces some expressions that contain Maxima gensyms, so
;;; the use of uninterned symbols instead can cause confusion since
;;; these print like any other symbol when lispdisp=false (the default).
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

(defmfun ($ratp :inline-impl t) (x)
  (and (not (atom x))
       (consp (car x))
       (eq (caar x) 'mrat)))

(defmfun $ratnump (x)
  (or (integerp x)
      (ratnump x)
      (and ($ratp x)
	   (not (member-eq 'trunc (car x)))
	   (integerp (cadr x))
	   (integerp (cddr x)))))

(defmfun $floatnump (x)
  (or (floatp x)
      (and ($ratp x) (floatp (cadr x)) (onep1 (cddr x)))))

(defmfun $numberp (x)
  "Returns true if X is a Maxima rational, a float, or a bigfloat number"
  (or ($ratnump x) ($floatnump x) ($bfloatp x)))

(defmfun $integerp (x)
  (or (integerp x)
      (and ($ratp x)
	   (not (member-eq 'trunc (car x)))
	   (integerp (cadr x))
	   (equal (cddr x) 1))))

;; The call to $INTEGERP in the following two functions checks for a CRE
;; rational number with an integral numerator and a unity denominator.

(defmfun ($oddp :inline-impl t) (x)
  (cond ((integerp x) (oddp x))
	((atom x) nil)
	(($integerp x) (oddp (cadr x)))))

(defmfun ($evenp :inline-impl t) (x)
  (cond ((integerp x) (evenp x))
	((atom x) nil)
	(($integerp x) (not (oddp (cadr x))))))

(defmfun $taylorp (x)
  (and (not (atom x))
       (eq (caar x) 'mrat)
       (member-eq 'trunc (cdar x)) t))

