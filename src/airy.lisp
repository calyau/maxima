;;; Airy functions Ai(z) and Bi(z) - A&S 10.4
;;;
;;; airy_ai(z)   - Airy function Ai(z)
;;; airy_dai(z)  - Derivative of Airy function Ai(z)
;;; airy_bi(z)   - Airy function Bi(z)
;;; airy_dbi(z)  - Derivative of Airy function Bi(z)

;;;; Copyright (C) 2005 David Billinghurst

;;;; airy.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; airy.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with command-line.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

(in-package :maxima)

(declaim (special *double-float-op*))

;; Airy Ai function 
(defmfun $airy_ai (z)
  "Airy function Ai(z)"
  (simplify (list '(%airy_ai) (resimplify z))))
(defprop %airy_ai simp-%airy_ai operators)
(defprop %airy_ai ((z) ((%airy_dai) z)) grad)

(defun airy-ai (z)
  (cond ((floatp z) (airy-ai-real z))
	((complexp z) (airy-ai-complex z))))

(setf (gethash '%airy_ai *double-float-op*) #'airy-ai)

(defmfun simp-%airy_ai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((double-float-eval (mop form) z))
	  (t (eqtest (list '(%airy_ai) z) form)))))


;; Derivative dAi/dz of Airy function Ai(z)
(defmfun $airy_dai (z)
  "Derivative dAi/dz of Airy function Ai(z)"
  (simplify (list '(%airy_dai) (resimplify z))))
(defprop %airy_dai simp-%airy_dai operators)
(defprop %airy_dai ((z) ((mtimes) z ((%airy_ai) z))) grad)

(defun airy-dai (z)
  (cond ((floatp z) (airy-dai-real z))
	((complexp z) (airy-dai-complex z))))

(setf (gethash '%airy_dai *double-float-op*) #'airy-dai)

(defmfun simp-%airy_dai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((double-float-eval (mop form) z))
	  (t (eqtest (list '(%airy_dai) z) form)))))


;; Airy Bi function 
(defmfun $airy_bi (z)
  "Airy function Bi(z)"
  (simplify (list '(%airy_bi) (resimplify z))))
(defprop %airy_bi simp-%airy_bi operators)
(defprop %airy_bi ((z) ((%airy_dbi) z)) grad)

(defun airy-bi (z)
  (cond ((floatp z) (airy-bi-real z))
	((complexp z) (airy-bi-complex z))))

(setf (gethash '%airy_bi *double-float-op*) #'airy-bi)

(defmfun simp-%airy_bi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((double-float-eval (mop form) z))
	  (t (eqtest (list '(%airy_bi) z) form)))))


;; Derivative dBi/dz of Airy function Bi(z)
(defmfun $airy_dbi (z)
  "Derivative dBi/dz of Airy function Bi(z)"
  (simplify (list '(%airy_dbi) (resimplify z))))
(defprop %airy_dbi simp-%airy_dbi operators)
(defprop %airy_dbi ((z) ((mtimes) z ((%airy_bi) z))) grad)

(defun airy-dbi (z)
  (cond ((floatp z) (airy-dbi-real z))
	((complexp z) (airy-dbi-complex z))))

(setf (gethash '%airy_dbi *double-float-op*) #'airy-dbi)

(defmfun simp-%airy_dbi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((double-float-eval (mop form) z))
	  (t (eqtest (list '(%airy_dbi) z) form)))))


;; Numerical routines using slatec functions

(defun airy-ai-real (z)
  " Airy function Ai(z) for real z"
  (declare (type double-float z))
  ;; slatec:dai issues underflow warning for z > zmax.  See dai.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 92.5747007268d0))
    (declare (type double-float zmax))
    (if (< z zmax) (slatec:dai z) 0.0d0))) 

(defun airy-ai-complex (z)
  "Airy function Ai(z) for complex z"
  (declare (type (complex double-float) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 0 1 0d0 0d0 0 0)
    (declare (type double-float air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-dai-real (z)
  "Derivative dAi/dz of Airy function Ai(z) for real z"
  (declare (type double-float z))
  (let ((rz (sqrt (abs z)))
	(c (* 2 (expt (abs z) 3/2) (/ 3.0))))
    (declare (type double-float rz c))
    (multiple-value-bind (var-0 var-1 var-2 ai dai)
	(slatec:djairy z rz c 0d0 0d0)
      (declare (ignore var-0 var-1 var-2 ai))
      dai)))

(defun airy-dai-complex (z)
  "Derivative dAi/dz of Airy function Ai(z) for complex z"
  (declare (type (complex double-float) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 1 1 0d0 0d0 0 0)
    (declare (type double-float air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-bi-real (z)
  "Airy function Bi(z) for real z"
  (declare (type double-float z))
  ;; slatec:dbi issues overflows for z > zmax.  See dbi.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.2179765192136d0))
    (declare (type double-float zmax))
    (if (< z zmax) (slatec:dbi z) nil)))

(defun airy-bi-complex (z)
  "Airy function Bi(z) for complex z"
  (declare (type (complex double-float) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 0 1 0d0 0d0 0)
    (declare (type double-float bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))

(defun airy-dbi-real (z)
  "Derivative dBi/dz of Airy function Bi(z) for real z"
  (declare (type double-float z))
  ;; Overflows for z > zmax.
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.1525d0))
    (declare (type double-float zmax))
    (if (< z zmax)
	(let ((rz (sqrt (abs z)))
	      (c (times 2.0 (expt (abs z) 3/2) (/ 3.0))))
        (declare (type double-float rz c))
        (multiple-value-bind (var-0 var-1 var-2 bi dbi)
	    (slatec:dyairy z rz c 0d0 0d0)
	  (declare (double-float bi dbi)
		   (ignore var-0 var-1 var-2 bi))
	  dbi))
      ;; Will overflow.  Return unevaluated.
      nil)))

(defun airy-dbi-complex (z)
  "Derivative dBi/dz of Airy function Bi(z) for complex z"
  (declare (type (complex double-float) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 1 1 0d0 0d0 0)
    (declare (type double-float bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))
