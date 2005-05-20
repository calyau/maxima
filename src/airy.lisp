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

(in-package "MAXIMA")

;; Airy Ai function 
(defmfun $airy_ai (z)
  "Airy function Ai(z)"
  (simplify (list '(%airy_ai) (resimplify z))))
(defprop %airy_ai simp-%airy_ai operators)
(defprop %airy_ai ((z) ((%airy_dai) z)) grad)

;; Derivative dAi/dz of Airy function Ai(z)
(defmfun $airy_dai (z)
  "Derivative dAi/dz of Airy function Ai(z)"
  (simplify (list '(%airy_dai) (resimplify z))))
(defprop %airy_dai simp-%airy_dai operators)
(defprop %airy_dai ((z) ((mtimes) z ((%airy_ai) z))) grad)

;; Airy Bi function 
(defmfun $airy_bi (z)
  "Airy function Bi(z)"
  (simplify (list '(%airy_bi) (resimplify z))))
(defprop %airy_bi simp-%airy_bi operators)
(defprop %airy_bi ((z) ((%airy_dbi) z)) grad)

;; Derivative dBi/dz of Airy function Bi(z)
(defmfun $airy_dbi (z)
  "Derivative dBi/dz of Airy function Bi(z)"
  (simplify (list '(%airy_dbi) (resimplify z))))
(defprop %airy_dbi simp-%airy_dbi operators)
(defprop %airy_dbi ((z) ((mtimes) z ((%airy_bi) z))) grad)

;; Simplification rules - only numerical evaluation

;; True if complex Airy functions should be evaluated 
;; numerically for z = zr + %i*zi
;; FIXME:  There might be an existing function for this
;; There is still a problem - float(airy_xx(%i)) isn't evaluated.
(defun evalate-complex-airy (zr zi)
  (cond
    ((not (and (numberp zr) (numberp zi)))
      nil)
    ((or $numer (floatp zr) (floatp zr))
      t)
    (t
       nil)))

(defmfun simp-%airy_ai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let* ((z (simpcheck (cadr form) x))
	 (zr ($realpart z))
	 (zi ($imagpart z)))
    (cond
      ((and (numberp zi) (zerop zi)
	    (or (floatp zr) (and $numer (integerp zr))))
        (airy-ai-real (float z 1.0d0)))
      ((evalate-complex-airy zr zi)
        (airy-ai-complex (float zr 1.0d0) (float zi 1.0d0)))
      (t 
        (eqtest (list '(%airy_ai) z) form)))))

(defmfun simp-%airy_dai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let* ((z (simpcheck (cadr form) x))
	 (zr ($realpart z))
	 (zi ($imagpart z)))
    (cond
      ((and (numberp zi) (zerop zi)
	    (or (floatp zr) (and $numer (integerp zr))))
        (airy-dai-real (float z 1.0d0)))
      ((evalate-complex-airy zr zi)
        (airy-dai-complex (float zr 1.0d0) (float zi 1.0d0)))
      (t 
        (eqtest (list '(%airy_dai) z) form)))))

(defmfun simp-%airy_bi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let* ((z (simpcheck (cadr form) x))
	 (zr ($realpart z))
	 (zi ($imagpart z)))
    (cond
      ((and (numberp zi) (zerop zi)
	    (or (floatp zr) (and $numer (integerp zr))))
        (airy-bi-real (float z 1.0d0)))
      ((evalate-complex-airy zr zi)
        (airy-bi-complex (float zr 1.0d0) (float zi 1.0d0)))
      (t 
        (eqtest (list '(%airy_bi) z) form)))))

(defmfun simp-%airy_dbi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let* ((z (simpcheck (cadr form) x))
	 (zr ($realpart z))
	 (zi ($imagpart z)))
    (cond
      ((and (numberp zi) (zerop zi)
	    (or (floatp zr) (and $numer (integerp zr))))
        (airy-dbi-real (float z 1.0d0)))
      ((evalate-complex-airy zr zi)
        (airy-dbi-complex (float zr 1.0d0) (float zi 1.0d0)))
      (t 
        (eqtest (list '(%airy_dbi) z) form)))))

;; Numerical routines using slatec functions

;; FIXME:  There must be an existing function for this
(defun m-complex (zr zi)
  "Generate maxima complex number from real and imaginary parts"
  (list '(mplus) zr (list '(mtimes) '$%i zi)))

(defun airy-ai-real (z)
  " Airy function Ai(z) for real z"
  (declare (type double-float z))
  ;; slatec:dai issues underflow warning for z > zmax.  See dai.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 92.5747007268d0))
    (declare (type double-float zmax))
    (if (< z zmax)	
      (slatec:dai z)
      0.0d0))) 

(defun airy-ai-complex (zr zi)
  "Airy function Ai(z) for complex z=zr+i*zi"
  (declare (type double-float zr zi))
  (let (air aii nz ierr var-0 var-1 var-2 var-3)
    (declare (type double-float air aii)
	     (type f2cl-lib:integer4 nz ierr))
    (multiple-value-setq 
      (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy zr zi 0 1 air aii nz ierr))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0))
      ;; No errors.  Return solution
      (m-complex air aii) 
      ;; zbiry shows errors or loss of precision.  Return unevaluated
      (list '(%airy_ai simp) (m-complex zr zi)))))

(defun airy-dai-real (z)
  "Derivative dAi/dz of Airy function Ai(z) for real z"
  (declare (type double-float z))
  (let (rz c ai dai var-0 var-1 var-2)
    (declare (type double-float rz c ai dai))
    (setq rz (sqrt (abs z)))
    (setq c (times 2.0 (expt (abs z) 3/2) (/ 3.0)))
    (multiple-value-setq 
      (var-0 var-1 var-2 ai dai)
      (slatec:djairy z rz c ai dai))
    dai))

(defun airy-dai-complex (zr zi)
  "Derivative dAi/dz of Airy function Ai(z) for complex z=zr+i*zi"
  (declare (type double-float zr zi))
  (let (air aii nz ierr var-0 var-1 var-2 var-3)
    (declare (type double-float air aii)
	     (type f2cl-lib:integer4 nz ierr))
    (multiple-value-setq 
      (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy zr zi 1 1 air aii nz ierr))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0))
      ;; No errors.  Return solution
      (m-complex air aii) 
      ;; zbiry shows errors or loss of precision.  Return unevaluated
      (list '(%airy_dai simp) (m-complex zr zi)))))

(defun airy-bi-real (z)
  "Airy function Bi(z) for real z"
  (declare (type double-float z))
  ;; slatec:dbi issues overflows for z > zmax.  See dbi.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.2179765192136d0))
    (declare (type double-float zmax))
    (if (< z zmax)
      (slatec:dbi z)
      ;; Will overflow.  Return unevaluated.
      (list '(%airy_bi simp) z))))

(defun airy-bi-complex (zr zi)
  "Airy function Bi(z) for complex z=zr+i*zi"
  (declare (type double-float zr zi))
  (let (bir bii ierr var-0 var-1 var-2 var-3)
    (declare (type double-float bir bii)
	     (type f2cl-lib:integer4 ierr))
    (multiple-value-setq 
      (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry zr zi 0 1 bir bii ierr))
    ;; Check ierr for errors
    (if (= ierr 0)
      ;; No errors.  Return solution
      (m-complex bir bii) 
      ;; zbiry shows errors or loss of precision.  Return unevaluated
      (list '(%airy_bi simp) (m-complex zr zi)))))

(defun airy-dbi-real (z)
  "Derivative dBi/dz of Airy function Bi(z) for real z"
  (declare (type double-float z))
  ;; Overflows for z > zmax.
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.1525d0))
    (declare (type double-float zmax))
    (if (< z zmax)
	(let (bi dbi var-0 var-1 var-2
	     (rz (sqrt (abs z)))
	     (c (times 2.0 (expt (abs z) 3/2) (/ 3.0))))
        (declare (type double-float rz c bi dbi))
        (multiple-value-setq 
          (var-0 var-1 var-2 bi dbi)
          (slatec:dyairy z rz c bi dbi))
	dbi)
      ;; Will overflow.  Return unevaluated.
      (list '(%airy_dbi simp) z))))

(defun airy-dbi-complex (zr zi)
  "Derivative dBi/dz of Airy function Bi(z) for complex z=zr+i*zi"
  (declare (type double-float zr zi))
  (let (bir bii ierr var-0 var-1 var-2 var-3)
    (declare (type double-float bir bii)
	     (type f2cl-lib:integer4 ierr))
    (multiple-value-setq 
      (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry zr zi 1 1 bir bii ierr))
    ;; Check ierr for errors
    (if (= ierr 0)
      ;; No errors.  Return solution
      (m-complex bir bii) 
      ;; zbiry shows errors or loss of precision.  Return unevaluated
      (list '(%airy_dbi simp) (m-complex zr zi)))))
