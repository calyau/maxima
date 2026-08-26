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
;;;; write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

(in-package :maxima)

;; Airy Ai function 

(defprop %airy_ai simplim%airy_ai simplim%function)
(defgrad %airy_ai ($z)
  #$$airy_dai(z)$)

;; airy_ai distributes over lists, matrices, and equations
(defprop %airy_ai (mlist $matrix mequal) distribute_over)

;; airy_ai has mirror symmetry
(defprop %airy_ai t commutes-with-conjugate)

;; Integral of Ai(z)
;; http://functions.wolfram.com/03.05.21.0002.01
;; (z/(3^(2/3)*gamma(2/3)))*hypergeometric([1/3],[2/3,4/3],z^3/9)
;; - (3^(1/6)/(4*%pi))*z^2*gamma(2/3)*hypergeometric([2/3],[4/3,5/3],z^3/9);
(defprop %airy_ai
  ((z)
   ((mplus)
    ((mtimes) 
     ((mexpt) 3 ((rat) -2 3))
     ((mexpt) ((%gamma) ((rat) 2 3)) -1)
     ((%hypergeometric) 
      ((mlist) ((rat) 1 3))
      ((mlist) ((rat) 2 3) ((rat) 4 3)) 
      ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
     z)
   ((mtimes) 
    ((rat) -1 4) ((mexpt) 3 ((rat) 1 6)) ((mexpt) $%pi -1) ((%gamma) ((rat) 2 3))
    ((%hypergeometric) 
     ((mlist) ((rat) 2 3)) 
     ((mlist) ((rat)  4 3) ((rat) 5 3))
     ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
    ((mexpt) z 2))))
  integral)

(defun airy-ai (z)
  (cond ((floatp z) (airy-ai-real z))
	((complexp z) (airy-ai-complex z))))

(setf (gethash '%airy_ai *flonum-op*) 'airy-ai)

(defun simplim%airy_ai (expr var val)
  ; Look for the limit of the argument
  (let ((z (limit (cadr expr) var val 'think)))
    (cond ((or (eq z '$inf)   ; A&S 10.4.59
	       (eq z '$minf)) ; A&S 10.4.60
	   0)
	  (t
	   ; Handle other cases with the function simplifier
	   (simplify (list '(%airy_ai) z))))))

(defun airy-ai-hypergeometric (z)
  "Returns the hypergeometric representation of Airy Ai"
  ;; See http://functions.wolfram.com/03.05.26.0001.01 and
  ;; https://fungrim.org/entry/01bbb6/:
  ;;
  ;;   Ai(z) = Ai(0)*hypergeometric([],[2/3],z^3/9)
  ;;     + z*Ai'(0)*hypergeometric([],[4/3],z^3/9)
  (add (mul (ftake '%airy_ai 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 2 3))
		   (div (power z 3)
			9)))
       (mul z
	    (ftake '%airy_dai 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 4 3))
		   (div (power z 3)
			9)))))

(defun airy-cancellation-digits (z)
  "Decimal digits destroyed by cancellation in the hypergeometric form of Ai and
  Ai'. Both terms there grow like Bi, that is like exp(2/3*z^(3/2)), while the
  combination decays like exp(-2/3*z^(3/2)), so the sum sheds about
  2*realpart(2/3*z^(3/2)) nepers, or that over log(10) digits. On the negative
  real axis, realpart(z^(3/2)) is zero and nothing is lost."
  (let ((w (ignore-errors
             (realpart
               (* 4/3 (expt (complex ($float ($realpart z)) ($float ($imagpart z)))
                            3/2))))))
    (if (and (realp w) (plusp w))
      (ceiling (/ w (log 10.0d0)))
      0)))

;; Evaluate BUILDER's hypergeometric form of an Airy function at enough extra
;; precision to survive the cancellation, then round back to the caller's FPPREC.
;; Without this, bfloat(airy_ai(20)) returned 0.0b0, and bfloat(airy_ai(30))
;; returned 2.097152b6 = 2^21 - pure roundoff residue, where the true values are
;; 1.69b-27 and 3.21b-49.
;;
;; HYPERGEOMETRIC-BY-SERIES has its own precision loop, but it cannot see this
;; cancellation: Each series is computed to full relative accuracy, and the
;; digits die in the outer sum. That loop doubles its working precision twice
;; before giving up at $MAX_FPPREC, so anything needing more than a quarter of
;; $MAX_FPPREC is out of reach - there we return NIL and let the caller give up,
;; leaving the expression unevaluated rather than handing back a number that is
;; wrong by hundreds of orders of magnitude.
;;
(defun airy-bfloat-with-guard-digits (z builder)
  (let ((needed (+ $fpprec (airy-cancellation-digits z) 10)))
    (declare (special $max_fpprec))
    (when (< (* 4 needed) $max_fpprec)
      ;; BUILDER is called inside the widened scope on purpose: It forms z^3/9,
      ;; and building that at the caller's FPPREC would round the argument
      ;; before the extra digits could do any good.
      (let ((hi (bind-fpprec needed ($rectform ($bfloat (funcall builder z))))))
        ($rectform ($bfloat hi))))))

(def-simplifier airy_ai (z)
  (cond ((equal z 0)	     ; A&S 10.4.4: Ai(0) = 3^(-2/3)/gamma(2/3)
	 (div (power 3 (div -2 3))
	      (take '(%gamma) (div 2 3))))
	((flonum-eval (mop form) z))
	((and (or (bigfloat-numerical-eval-p z)
		  (complex-bigfloat-numerical-eval-p z))
	      (airy-bfloat-with-guard-digits z #'airy-ai-hypergeometric)))
	($hypergeometric_representation
	 (airy-ai-hypergeometric z))
	(t (give-up))))


;; Derivative dAi/dz of Airy function Ai(z)
(defprop %airy_dai simplim%airy_dai simplim%function)
(defgrad %airy_dai ($z)
  #$$z*airy_ai(z)$)

(defprop %airy_dai ((z) ((%airy_ai) z)) integral)

;; airy_dai distributes over lists, matrices, and equations
(defprop %airy_dai (mlist $matrix mequal) distribute_over)

;; airy_dai has mirror symmetry
(defprop %airy_dai t commutes-with-conjugate)

(defun airy-dai (z)
  (cond ((floatp z) (airy-dai-real z))
	((complexp z) (airy-dai-complex z))))

(setf (gethash '%airy_dai *flonum-op*) 'airy-dai)

(defun simplim%airy_dai (expr var val)
  ; Look for the limit of the argument
  (let ((z (limit (cadr expr) var val 'think)))
    (cond ((eq z '$inf) ; A&S 10.4.61
	   0)
	  ((eq z '$minf) ; A&S 10.4.62
	   '$und)
	  (t
	   ; Handle other cases with the function simplifier
	   (simplify (list '(%airy_dai) z))))))

(defun airy-dai-hypergeometric (z)
  "Returns the hypergeometric representation of Ai'(x), the derivative
  of the Airy function Ai(x)"
  ;; See http://functions.wolfram.com/03.07.26.0001.01 and
  ;; https://fungrim.org/entry/20e530/.
  ;;
  ;;
  ;;   Ai'(z) = Ai'(0)*hypergeometric([],[1/3],z^3/9)
  ;;     + z^2/2*Ai(0)*hypergeometric([],[5/3],z^3/9)
  (add (mul (ftake '%airy_dai 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 1 3))
		   (div (power z 3)
			9)))
       (mul z z 1//2
	    (ftake '%airy_ai 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 5 3))
		   (div (power z 3)
			9)))))

(def-simplifier airy_dai (z)
  (cond ((equal z 0)	   ; A&S 10.4.5: Ai'(0) = -3^(-1/3)/gamma(1/3)
	 (div -1
	      (mul (power 3 (div 1 3))
		   (take '(%gamma) (div 1 3)))))
	((flonum-eval (mop form) z))
	((and (or (bigfloat-numerical-eval-p z)
		  (complex-bigfloat-numerical-eval-p z))
	      (airy-bfloat-with-guard-digits z #'airy-dai-hypergeometric)))
	($hypergeometric_representation
	 (airy-dai-hypergeometric z))
	(t (give-up))))

(defprop %airy_bi simplim%airy_bi simplim%function)
(defgrad %airy_bi ($z)
  #$$ airy_dbi(z)$)

;; airy_bi distributes over lists, matrices, and equations
(defprop %airy_bi (mlist $matrix mequal) distribute_over)

;; airy_bi has mirror symmetry
(defprop %airy_bi t commutes-with-conjugate)

;; Integral of Bi(z)
;; http://functions.wolfram.com/03.06.21.0002.01
;; (z/(3^(1/6)*gamma(2/3)))*hypergeometric([1/3],[2/3,4/3],z^3/9)
;; + (3^(2/3)/(4*%pi))*z^2*gamma(2/3)*hypergeometric([2/3],[4/3,5/3],z^3/9);
(defprop %airy_bi
  ((z)
   ((mplus)
    ((mtimes) 
     ((mexpt) 3 ((rat) -1 6))
     ((mexpt) ((%gamma) ((rat) 2 3)) -1)
     ((%hypergeometric) 
      ((mlist) ((rat) 1 3))
      ((mlist) ((rat) 2 3) ((rat) 4 3)) 
      ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
     z)
   ((mtimes) 
    ((rat) 1 4) ((mexpt) 3 ((rat) 2 3)) ((mexpt) $%pi -1) ((%gamma) ((rat) 2 3))
    ((%hypergeometric) 
     ((mlist) ((rat) 2 3)) 
     ((mlist) ((rat)  4 3) ((rat) 5 3))
     ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
    ((mexpt) z 2))))
  integral)

(defun airy-bi (z)
  (cond ((floatp z) (airy-bi-real z))
	((complexp z) (airy-bi-complex z))))

(setf (gethash '%airy_bi *flonum-op*) 'airy-bi)

(defun simplim%airy_bi (expr var val)
  ; Look for the limit of the argument
  (let ((z (limit (cadr expr) var val 'think)))
    (cond ((eq z '$inf) ; A&S 10.4.63
	   '$inf)
	  ((eq z '$minf) ; A&S 10.4.64
	   0)
	  (t
	   ; Handle other cases with the function simplifier
	   (simplify (list '(%airy_bi) z))))))

(defun airy-bi-hypergeometric (z)
  "Returns the hypergeometric representation of Airy Bi"
  ;; See http://functions.wolfram.com/03.06.26.0001.01 and https://fungrim.org/entry/bd319e/ 
  ;;
  ;;  Bi(z) = Bi(0)*hypergeometric([],[2/3],z^3/9)
  ;;    + z*Bi'(0)*hypergeometric([],[4/2],z^3/9)
  (add (mul (ftake '%airy_bi 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 2 3))
		   (div (power z 3)
			9)))
       (mul z
	    (ftake '%airy_dbi 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 4 3))
		   (div (power z 3)
			9)))))

(def-simplifier airy_bi (z)
  (cond ((equal z 0) ; A&S 10.4.4: Bi(0) = sqrt(3) 3^(-2/3)/gamma(2/3)
	 (div (mul (power 3 1//2)
		   (power 3 (div -2 3)))
	      (take '(%gamma) (div 2 3))))
	((flonum-eval (mop form) z))
	((or (bigfloat-numerical-eval-p z)
	     (complex-bigfloat-numerical-eval-p z))
	 ($rectform
	  ($bfloat (airy-bi-hypergeometric z))))
	($hypergeometric_representation
	 (airy-bi-hypergeometric z))
	(t (give-up))))


;; Derivative dBi/dz of Airy function Bi(z)
(defprop %airy_dbi simplim%airy_dbi simplim%function)
(defgrad %airy_dbi ($z)
  #$$z*airy_bi(z)$)
(defprop %airy_dbi ((z) ((%airy_bi) z)) integral)

;; airy_dbi distributes over lists, matrices, and equations
(defprop %airy_dbi (mlist $matrix mequal) distribute_over)

;; airy_dbi has mirror symmetry
(defprop %airy_dbi t commutes-with-conjugate)

(defun airy-dbi (z)
  (cond ((floatp z) (airy-dbi-real z))
	((complexp z) (airy-dbi-complex z))))

(setf (gethash '%airy_dbi *flonum-op*) 'airy-dbi)

(defun simplim%airy_dbi (expr var val)
  ; Look for the limit of the argument
  (let ((z (limit (cadr expr) var val 'think)))
    (cond ((eq z '$inf) ; A&S 10.4.66
	   '$inf)
	  ((eq z '$minf) ; A&S 10.4.67
	   '$und)
	  (t
	   ; Handle other cases with the function simplifier
	   (simplify (list '(%airy_dbi) z))))))

(defun airy-dbi-hypergeometric (z)
  "Returns the hypergeometric representation of Bi'(z), the derivative
  of Airy Bi"
  ;; See http://functions.wolfram.com/03.08.26.0001.01 and
  ;; https://fungrim.org/entry/4d65e5/.
  ;;
  ;;  Bi'(z) = Bi'(0)*hypergeometric([],[1/3],z^3/9)
  ;;    + z^2/2*Bi(0)*hypergeometric([],[5/3],z^3/9)
  (add (mul (ftake '%airy_dbi 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 1 3))
		   (div (power z 3)
			9)))
       (mul z z 1//2
	    (ftake '%airy_bi 0)
	    (ftake '%hypergeometric
		   (list '(mlist))
		   (list '(mlist) (div 5 3))
		   (div (power z 3)
			9)))))

(def-simplifier airy_dbi (z)
  (cond ((equal z 0) ; A&S 10.4.5: Bi'(0) = sqrt(3) 3^(-1/3)/gamma(1/3)
	 (div (mul (power 3 1//2)
		   (power 3 (div -1 3)))
	      (take '(%gamma) (div 1 3))))
	((flonum-eval (mop form) z))
	((or (bigfloat-numerical-eval-p z)
	     (complex-bigfloat-numerical-eval-p z))
	 ($rectform
	  ($bfloat (airy-dbi-hypergeometric z))))
	($hypergeometric_representation
	 (airy-dbi-hypergeometric z))
	(t (give-up))))

;; Numerical routines using slatec functions

(defun airy-ai-real (z)
  " Airy function Ai(z) for real z"
  (declare (type flonum z))
  ;; slatec:dai gives up at 92.5747007268, but Ai(z) is an ordinary double up to
  ;; about 107.3. Above dai's cutoff use the formula dai itself uses there,
  ;; see dai.f label 30. The upper test keeps z*sqrt(z) from overflowing for a
  ;; huge z.
  (cond ((< z 92.5747007268) (slatec:dai z))
	((< z 108.0) (* (slatec::daie z) (exp (/ (* -2.0 z (sqrt z)) 3.0))))
	(t 0.0)))

(defun airy-ai-complex (z)
  "Airy function Ai(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 0 1 0.0 0.0 0 0)
    (declare (type flonum air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-dai-real (z)
  "Derivative dAi/dz of Airy function Ai(z) for real z"
  (declare (type flonum z))
  (let ((rz (sqrt (abs z)))
	(c (* 2/3 (expt (abs z) 3/2))))
    (declare (type flonum rz c))
    (multiple-value-bind (var-0 var-1 var-2 ai dai)
	(slatec:djairy z rz c 0.0 0.0)
      (declare (ignore var-0 var-1 var-2 ai))
      dai)))

(defun airy-dai-complex (z)
  "Derivative dAi/dz of Airy function Ai(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 1 1 0.0 0.0 0 0)
    (declare (type flonum air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-bi-real (z)
  "Airy function Bi(z) for real z"
  (declare (type flonum z))
  ;; slatec:dbi issues overflows for z > zmax.  See dbi.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.2179765192136))
    (declare (type flonum zmax))
    (if (< z zmax) (slatec:dbi z) nil)))

(defun airy-bi-complex (z)
  "Airy function Bi(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 0 1 0.0 0.0 0)
    (declare (type flonum bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))

(defun airy-dbi-real (z)
  "Derivative dBi/dz of Airy function Bi(z) for real z"
  (declare (type flonum z))
  ;; Overflows for z > zmax.
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.1525))
    (declare (type flonum zmax))
    (if (< z zmax)
	(let ((rz (sqrt (abs z)))
	      (c (* 2/3 (expt (abs z) 3/2))))
        (declare (type flonum rz c))
        (multiple-value-bind (var-0 var-1 var-2 bi dbi)
	    (slatec:dyairy z rz c 0.0 0.0)
	  (declare (type flonum bi dbi)
		   (ignore var-0 var-1 var-2 bi))
	  dbi))
      ;; Will overflow.  Return unevaluated.
      nil)))

(defun airy-dbi-complex (z)
  "Derivative dBi/dz of Airy function Bi(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 1 1 0.0 0.0 0)
    (declare (type flonum bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))
