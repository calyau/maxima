;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Double Factorial
;;;
;;; This file will be extended with further functions related to the 
;;; Factorial and Gamma functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file containts the following Maxima User functions:
;;;
;;;   factorial_double(z) - Double factorial generalized for real and complex
;;;                         argument z in double float and bigfloat precision
;;;
;;; Maxima User variable:
;;;
;;;   $factorial_expand   - Allows argument simplificaton for expressions like
;;;                         factorial_double(n-1) and factorial_double(2*k+n)
;;;
;;; Maxima User variable (not definied in this file):
;;;
;;;   $factlim            - biggest integer for numerically evaluation
;;;                         of the Double factorial
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the 
;;; Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along 
;;; with this library; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Copyright (C) 2008 Dieter Kaiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declare-top (special $factlim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar $factorial_expand nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The changes to the parser to connect the operator !! to factorial_double(z)

;(def-mheader |$!!| (%factorial_double))

;(def-led (|$!!| 160.) (op left)
;  (list '$expr
;	(mheader '$!!)
;	(convert left '$expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the function Double factorial
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $factorial_double (z)
  (simplify (list '(%factorial_double) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $factorial_double %factorial_double alias)
(defprop $factorial_double %factorial_double verb)

(defprop %factorial_double $factorial_double reversealias)
(defprop %factorial_double $factorial_double noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial is a simplifying function

(defprop %factorial_double simp-factorial-double operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial has mirror symmetry

(defprop %factorial_double t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Double factorial

(defprop %factorial_double
  ((z)
   ((mtimes) 
      ((rat) 1 2)
      ((%double_factorial) $z)
      ((mplus) 
         ((%log) 2)
         ((mqapply) 
            (($psi array) 0)
            ((mplus) 1 ((mtimes) ((rat) 1 2) $z)))
         ((mtimes) 
            ((rat) 1 2) $%pi
            ((%log) ((mtimes) 2 ((mexpt) $%pi -1)))
            ((%sin) ((mtimes) $%pi $z))))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-factorial-double (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond    
    ((and (fixnump z) (> z -1) (or (minusp $factlim) (< z $factlim)))
     ;; Positive Integer less then $factlim or $factlim is -1. Call gfact.
     (gfact z (floor (/ z 2)) 2))

    ((and (mnump z)
          (eq ($sign z) '$neg)          
          (zerop1 (sub (simplify (list '(%truncate) (div z 2))) (div z 2))))
     ;; Even negative integer or real representation. Not defined.
     (merror "factorial_double(~:M) is undefined." z))

    ((or (integerp z)   ; at this point odd negative integer
         (floatp z)
         (complex-number-p z))
     (cond
       ((and (integerp z) (= z -1))  1)  ; Special cases -1 and -3 
       ((and (integerp z) (= z -3)) -1)
       (t
        ;; Odd negative integer, float or complex float.
        (complexify 
          (factorial-double 
            (complex (float ($realpart z)) (float ($imagpart z))))))))
  
    ((and (not (ratnump z))
          (or ($bfloatp z)
              (complex-number-p z 'bigfloat-or-number-p)))
     ;; bigfloat or complex bigfloat.
     (bfloat-factorial-double 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ((and $factorial_expand
          (mplusp z)
          (integerp (cadr z)))
     (let ((k (cadr z))
           (n (simplify (cons '(mplus) (cddr z)))))
       (cond
         ((= k -1)
          ;; Special case factorial_double(n-1)
          (div (simplify (list '(mfactorial) n)) 
               (simplify (list '(%factorial_double) n))))
         ((= k (* 2 (truncate (/ k 2))))
          ;; Special case factorial_double(2*k+n), k integer
          (setq k (/ k 2))
          ($factor   ; we get more simple expression when factoring
            (mul
              (power 2 k)
              (simplify (list '($pochhammer) (add (div n 2) 1) k))
              (simplify (list '(%factorial_double) n)))))
         (t
           (eqtest (list '(%factorial_double) z) expr)))))

    (t
      (eqtest (list '(%factorial_double) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a complex float argument. The result is a CL complex.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factorial-double (z)
  (let ((pival (float pi)))
  (*
    (expt
      (/ 2 pival)
      (/ (- 1 (cos (* pival z))) 4))
    (expt 2 (/ z 2))
    (gamma-lanczos (+ 1 (/ z 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a bigfloat or complex bigfloat argument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-factorial-double (z)
  (let* ((pival ($bfloat '$%pi))
         (bigfloat1 ($bfloat bigfloatone))
         (bigfloat2 (add bigfloat1 bigfloat1))
         (bigfloat4 (add bigfloat2 bigfloat2))
         ($ratprint nil))
    (cmul
      (cpower
        (cdiv bigfloat2 pival)
        (cdiv (sub bigfloat1 
                   (simplify (list '(%cos) (cmul pival z)))) bigfloat4))
      (cmul
        (cpower bigfloat2 (cdiv z bigfloat2))
        (simplify (list '(%gamma) (add bigfloat1 (cdiv z bigfloat2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
