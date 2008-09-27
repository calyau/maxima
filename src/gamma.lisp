;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Double Factorial, Incomplete Gamma function, ...
;;;
;;; This file will be extended with further functions related to the 
;;; Factorial and Gamma functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file containts the following Maxima User functions:
;;;
;;;   factorial_double(z)   - Double factorial generalized for real and complex
;;;                           argument z in double float and bigfloat precision
;;;
;;;   gamma_incomplete(a,z) - Incomplete Gamma function
;;;
;;;   gamma_incomplete_generalized(a,z1,z2)
;;;                         - Generalized Incomplete Gamma function
;;;
;;; Maxima User variable:
;;;
;;;   $factorial_expand    - Allows argument simplificaton for expressions like
;;;                          factorial_double(n-1) and factorial_double(2*k+n)
;;;
;;; Maxima User variable (not definied in this file):
;;;
;;;   $factlim             - biggest integer for numerically evaluation
;;;                          of the Double factorial
;;;   $gamma_expand        - Expansion of the Gamma und Incomplete Gamma
;;;                          function for some special cases
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

(declare-top (special $factlim $gamma_expand))

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
;;;
;;; The implementation of the Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-gamma* nil)

(defun $gamma_incomplete (a z)
  (simplify (list '(%gamma_incomplete) (resimplify a) (resimplify z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $gamma_incomplete %gamma_incomplete alias)
(defprop $gamma_incomplete %gamma_incomplete verb)

(defprop %gamma_incomplete $gamma_incomplete reversealias)
(defprop %gamma_incomplete $gamma_incomplete noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete simp-gamma-incomplete operators)

;;; Incomplete Gamma function has mirror symmetry

(defprop %gamma_incomplete t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Derivative of the Incomplete Gamma function

(defprop %gamma_incomplete
  ((a z)
   ;; The derivative wrt a in terms of hypergeometric_generalized 2F2 function
   ;; and the Generalized Incomplete Gamma function (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z)))
      ((mtimes) -1
         ((%gamma_incomplete_generalized) a 0 z)
         ((%log) z))
      ((mtimes)
         ((%gamma) a)
         ((mqapply) (($psi array) 0) a)))
   ;; The derivative wrt z
   ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z))
      ((mexpt) z ((mplus) -1 a))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((a (simpcheck (cadr expr) simpflag))
        (z (simpcheck (caddr expr) simpflag))
        (ratorder))
    (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE:~%")
         (format t "~&   : a = ~A~%" a)
         (format t "~&   : z = ~A~%" z))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg) (domain-error 0 'gamma_incomplete))
               ((eq sgn '$zero) (domain-error 0 'gamma_incomplete))
               ((member sgn '($pos $pz)) ($gamma a))
               (t (eqtest (list '(%gamma_incomplete) a z) expr)))))
              
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((and (numberp a)
            (numberp z)
            (or $numer (floatp a) (floatp z)))
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE: float evaluation.~%"))
       (complexify (gamma-incomplete a z)))

      ((and (complex-number-p a)
            (complex-number-p z)
            (or $numer 
                (floatp ($realpart a)) (floatp ($imagpart a))
                (floatp ($realpart z)) (floatp ($imagpart z))))
       (let ((ca (complex (float ($realpart a)) (float ($imagpart a))))
             (cz (complex (float ($realpart z)) (float ($imagpart z)))))
         (complexify (gamma-incomplete ca cz))))
           
      ((and (mnump a)
            (mnump z)
            (or $numer ($bfloatp a) ($bfloatp z)))
       (bfloat-gamma-incomplete a z))

      ((and (complex-number-p a 'bigfloat-or-number-p)
            (complex-number-p z 'bigfloat-or-number-p)
            (or $numer
                ($bfloatp ($realpart a)) ($bfloatp ($imagpart a))
                ($bfloatp ($realpart z)) ($bfloatp ($imagpart z))))
       (complex-bfloat-gamma-incomplete
         (add ($bfloat ($realpart a)) (mul '$%i ($bfloat ($imagpart a))))
         (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (integerp a))
       ;; Integer or a symbol declared to be an integer. Expand in a series.
       (let ((sgn ($sign a)))
         (cond
           ((eq sgn '$zero)
            (add
              (mul -1
                (simplify (list '(%expintegral_ei) (mul -1 z))))
              (mul
                (div 1 2)
                (sub
                  (simplify (list '(%log) (mul -1 z)))
                  (simplify (list '(%log) (div -1 z)))))
              (mul -1 (simplify (list '(%log) z)))))
           ((member sgn '($pos $pz))
            (when *debug-gamma* 
              (format t "Expand in series for a=~A and z=~A~%" a z))
            (mul
              (simplify (list '(%gamma) a))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div 
                    (power z index)
                    (list '(%gamma) (add index 1)))
                  index 0 (sub a 1) t))))
           ((member sgn '($neg $nz))
            (sub
              (mul
                (div
                  (power -1 (add (mul -1 a) -1))
                  (simplify (list '(%gamma) (add (mul -1 a) 1))))
                (add
                  (simplify (list '(%expintegral_ei) (mul -1 z)))
                  (mul
                    (div -1 2)
                    (sub
                      (simplify (list '(%log) (mul -1 z)))
                      (simplify (list '(%log) (div -1 z)))))
                  (simplify (list '(%log) z))))
              (mul
                (power '$%e (mul -1 z))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z (add index a -1))
                      (simplify (list '($pochhammer) a index)))
                    index 1 (mul -1 a) t)))))
           (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

      ((and $gamma_expand (setq ratorder (max-numeric-ratio-p a 2)))
       ;; We have a half integral order and $gamma_expand is not NIL.
       ;; We expand in a series with the Erfc function
       (setq ratorder (- ratorder (/ 1 2)))
       (cond
         ((equal ratorder 0)
          (mul 
            (power '$%pi '((rat) 1 2))
            (simplify (list '(%erfc) (power z '((rat) 1 2))))))
         ((> ratorder 0)
          (sub
            (mul
              (simplify (list '(%gamma) ratorder))
              (simplify (list '(%erfc) (power z '((rat) 1 2)))))
            (mul
              (power -1 (sub ratorder 1))
              (power '$%e (mul -1 z))
              (power z '((rat) 1 2))
              (let ((index (gensumindex)))
                (dosum
                  (mul -1                      ; we get more simple results
                    (simplify                  ; when multiplying with -1  
                      (list 
                       '($pochhammer)
                        (sub (div 1 2) ratorder)
                        (add (- ratorder 1) index)))
                    (power (mul -1 z) index))
                  index 0 (sub ratorder 1) t)))))
         ((< ratorder 0)
          (setq ratorder (- ratorder))
          (sub
            (div
              (mul
                (power -1 ratorder)
                (power '$%pi '((rat) 1 2))
                (simplify (list '(%erfc) (power z '((rat) 1 2)))))
              (simplify (list '($pochhammer) (div 1 2) ratorder)))
            (mul 
              (power z (sub (div 1 2) ratorder))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div
                    (power z index)
                    (simplify 
                      (list 
                       '($pochhammer) 
                        (sub (div 1 2) ratorder)  
                        (add index 1))))
                  index 0 (sub ratorder 1) t)))))))

      ((and (mplusp a) (integerp (cadr a)))
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE in COND (mplusp)~%"))
       (let ((n (cadr a))
             (a (cons '(mplus) (cddr a))))
         (cond
           ((> n 0)
            (add
              (mul
                (simplify (list '($pochhammer) a n))
                (simplify (list '(%gamma_incomplete) a z)))
              (mul
                (power '$%e (mul -1 z))
                (power z (add a n -1))
                (let ((index (gensumindex)))
                  (dosum
                    (mul
                      (simplify 
                        (list 
                         '($pochhammer) (add 1 (mul -1 a) (mul -1 n)) index))
                      (power (mul -1 z) (mul -1 index)))
                    index 0 (add n -1) t)))))
           ((< n 0)
            (setq n (- n))
            (sub
              (div
                (mul
                  (power -1 n)
                  (simplify (list '(%gamma_incomplete) a z)))
                (simplify (list '($pochhammer) (sub 1 a) n)))
              (mul
                (power '$%e (mul -1 z))
                (power z (sub a n))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z index)
                      (simplify (list '($pochhammer) (sub a n) (add index 1))))
                    index 0 (sub n 1) t))))))))

      (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gamma-incomplete-maxit* 10000)
(defvar *gamma-incomplete-eps* 1.0e-15)
(defvar *gamma-incomplete-min* 1.0e-30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The numerical evaluation for CL float or complex values a and x

(defun gamma-incomplete (a x)
  (let ((gm-maxit *gamma-incomplete-maxit*)
        (gm-eps   *gamma-incomplete-eps*)
        (gm-min   *gamma-incomplete-min*))
    (cond
      ((and (> (realpart x) 0) (> (abs x) (realpart (+ 1.0 a))))
       ;; Expansion in continued fractions
       (do* ((i 1 (+ i 1))
             (an (- a 1.0) (* i (- a i)))
             (b (+ 3.0 x (- a)) (+ b 2.0))
             (c (/ 1.0 gm-min))
             (d (/ 1.0 (- b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete'"))
         (when *debug-gamma* 
           (format t "~&GAMMA-INCOMPLETE in continued fractions:~%")
           (format t "~&   : i    = ~A~%" i)
           (format t "~&   : b    = ~A~%" b)
           (format t "~&   : c    = ~A~%" c)
           (format t "~&   : d    = ~A~%" d)
           (format t "~&   : del  = ~A~%" del)
           (format t "~&   : h    = ~A~%" h))
         
       (setq d (+ (* an d) b))
       (when (< (abs d) gm-min) (setq d gm-min))
       (setq c (+ b (/ an c)))
       (when (< (abs c) gm-min) (setq c gm-min))
       (setq d (/ 1.0 d))
       (setq del (* d c))
       (setq h (* h del))
       (when (< (abs (- del 1.0)) gm-eps)
         (return (* h (expt x a) (exp (- x)))))))

      (t
       ;; Expansion in a series
       (do* ((i 1 (+ i 1))
             (ap a (+ ap 1.0))
             (del (/ 1.0 a) (* del (/ x ap)))
             (sum del (+ sum del))
             (gm (gamma-lanczos (complex (realpart a) (imagpart a)))))
            ((> i gm-maxit)
             (merror "Series expansion failed in `gamma_incomplete"))
         (when *debug-gamma* 
           (format t "~&GAMMA-INCOMPLETE in series:~%")
           (format t "~&   : i    = ~A~%" i)
           (format t "~&   : ap   = ~A~%" ap)
           (format t "~&   : x/ap = ~A~%" (/ x ap))
           (format t "~&   : del  = ~A~%" del)
           (format t "~&   : sum  = ~A~%" sum))
         (when (< (abs del) (* (abs sum) gm-eps))
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return
             (- (gamma-lanczos (complex (realpart a) (imagpart a)))
                (* sum (expt x a) (exp (- x)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (pow ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps)))
    (cond
      ((and (eq ($sign ($realpart x)) '$pos) 
            (eq ($sign (sub (simplify (list '(mabs) x)) 
                            ($realpart (add 1.0 a)))) '$pos))
       ;; Expansion in continued fractions of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (an (sub a 1.0) (mul i (sub a i)))
             (b (add 3.0 x (mul -1 a)) (add b 2.0))
             (c (div 1.0 gm-min))
             (d (div 1.0 (sub b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete"))
         (when *debug-gamma* 
           (format t "~&in coninued fractions:~%")
           (format t "~&   : i = ~A~%" i)
           (format t "~&   : h = ~A~%" h))
         (setq d (add (mul an d) b))
         (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
           (setq d gm-min))
         (setq c (add b (div an c)))
         (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
           (setq c gm-min))
         (setq d (div 1.0 d))
         (setq del (mul d c))
         (setq h (mul h del))
         (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) gm-eps))
                   '$neg)
           (return 
             (mul h
                  (power x a) 
                  (power ($bfloat '$%e) (mul -1 x)))))))

      (t
       ;; Series expansion of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (div 1.0 a) (mul del (div x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror "Series expansion failed in `gamma-incomplete'"))
         (when *debug-gamma* 
           (format t "~&GAMMA-INCOMPLETE in series:~%")
           (format t "~&   : i    = ~A~%" i)
           (format t "~&   : ap   = ~A~%" ap)
           (format t "~&   : x/ap = ~A~%" (div x ap))
           (format t "~&   : del  = ~A~%" del)
           (format t "~&   : sum  = ~A~%" sum))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (cabs sum) gm-eps)))
                   '$neg)
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return 
             (sub (simplify (list '(%gamma) a))
                  (mul sum
                       (power x a)
                       (power ($bfloat '$%e) (mul -1 x)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (pow ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps)))
    (cond
      ((and (eq ($sign ($realpart x)) '$pos) 
            (eq ($sign (sub (simplify (list '(mabs) x)) 
                            ($realpart (add 1.0 a)))) '$pos))
       ;; Expansion in continued fractions of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (an (sub a 1.0) (mul i (sub a i)))
             (b (add 3.0 x (mul -1 a)) (add b 2.0))
             (c (cdiv 1.0 gm-min))
             (d (cdiv 1.0 (sub b 2.0)))
             (h d)
             (del 0.0))
            ((> i gm-maxit)
             (merror "Continued fractions failed in `gamma_incomplete"))
         (when *debug-gamma* 
           (format t "~&in coninued fractions:~%")
           (format t "~&   : i = ~A~%" i)
           (format t "~&   : h = ~A~%" h))
         (setq d (add (cmul an d) b))
         (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
           (setq d gm-min))
         (setq c (add b (cdiv an c)))
         (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
           (setq c gm-min))
         (setq d (cdiv 1.0 d))
         (setq del (cmul d c))
         (setq h (cmul h del))
         (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) 
                               gm-eps))
                   '$neg)
           (return 
             (cmul h
               (cmul
                 (cpower x a) 
                 (cpower ($bfloat '$%e) (mul -1 x))))))))

      (t
       ;; Series expansion of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (cdiv 1.0 a) (cmul del (cdiv x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror "Series expansion failed in `gamma-incomplete'"))
         (when *debug-gamma*
           (format t "~&GAMMA-INCOMPLETE in series:~%")
           (format t "~&   : i    = ~A~%" i)
           (format t "~&   : ap   = ~A~%" ap)
           (format t "~&   : x/ap = ~A~%" (div x ap))
           (format t "~&   : del  = ~A~%" del)
           (format t "~&   : sum  = ~A~%" sum))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (cabs sum) gm-eps)))
                   '$neg)
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return 
             (sub (simplify (list '(%gamma) a))
                  (cmul sum
                    (cmul
                      (cpower x a)
                      (cpower ($bfloat '$%e) (mul -1 x))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $gamma_incomplete_generalized (a z1 z2)
  (simplify (list '(%gamma_incomplete) 
                   (resimplify a) (resimplify z1) (resimplify z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set the properties alias, reversealias, noun and verb

(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized alias)
(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized verb)

(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized reversealias)
(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function has mirror symmetry

(defprop %gamma_incomplete_generalized t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete_generalized 
         simp-gamma-incomplete-generalized operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Generalized Incomplete Gamma function

(defprop %gamma_incomplete_generalized
  ((a z1 z2)
   ;; The derivative wrt a in terms of hypergeometric_generalized 2F2 function
   ;; and the Generalized Incomplete Gamma function (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z1 a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z1)))
      ((mtimes) -1
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z2 a)
         (($hypergeometric_generalized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z2)))
      ((mtimes) -1
         ((%gamma_incomplete_generalized) a 0 z1)
         ((%log) z1))
      ((mtimes)
         ((%gamma_incomplete_generalized) a 0 z2)
         ((%log) z2)))
   ;; The derivative wrt z1
   ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z1))
      ((mexpt) z1 ((mplus) -1 a)))
   ;; The derivative wrt z2
   ((mtimes)
      ((mexpt) $%e ((mtimes) -1 z2))
      ((mexpt) z2 ((mplus) -1 a))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$gamma_incomplete_generalized))
  (let ((a  (simpcheck (cadr expr)   simpflag))
        (z1 (simpcheck (caddr expr)  simpflag))
        (z2 (simpcheck (cadddr expr) simpflag)))

    (cond

      ;; Check for specific values

      ((zerop1 z2)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((eq sgn '$pos) 
            (sub
              (simplify (list '(%gamma_incomplete) a z1))
              (simplify (list '(%gamma) a))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 z1)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((eq sgn '$pos) 
            (sub
              (simplify (list '(%gamma) a))
              (simplify (list '(%gamma_incomplete) a z2))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 (sub z1 z2)) 0)

      ((eq z2 '$inf) (simplify (list '(%gamma_incomplete) a z1)))
      ((eq z1 '$inf) (mul -1 (simplify (list '(%gamma_incomplete) a z2))))

      ;; Check for numerical evaluation in Float or Bigfloat precision
      ;; Use the numerical routines of the Incomplete Gamma function

      ((and (numberp a)
            (numberp z1)
            (numberp z2)
            (or $numer (floatp a) (floatp z1) (floatp z2)))
       (complexify (- (gamma-incomplete a z1) (gamma-incomplete a z2))))

      ((and (complex-number-p a)
            (complex-number-p z1)
            (complex-number-p z2)
            (or $numer 
                (floatp ($realpart a)) (floatp ($imagpart a))
                (floatp ($realpart z1)) (floatp ($imagpart z1))
                (floatp ($realpart z2)) (floatp ($imagpart z2))))
       (let ((ca (complex ($realpart a) ($imagpart a)))
             (cz1 (complex ($realpart z1) ($imagpart z1)))
             (cz2 (complex ($realpart z2) ($imagpart z2))))
         (complexify (- (gamma-incomplete ca cz1) (gamma-incomplete ca cz2)))))
           
      ((and (mnump a)
            (mnump z1)
            (mnump z2)
            (or $numer ($bfloatp a) ($bfloatp z1) ($bfloatp z2)))
       (sub (bfloat-gamma-incomplete a z1) (bfloat-gamma-incomplete a z2)))

      ((and (complex-number-p a 'bigfloat-or-number-p)
            (complex-number-p z1 'bigfloat-or-number-p)
            (complex-number-p z2 'bigfloat-or-number-p)
            (or $numer
                ($bfloatp ($realpart a)) ($bfloatp ($imagpart a))
                ($bfloatp ($realpart z1)) ($bfloatp ($imagpart z1))
                ($bfloatp ($realpart z2)) ($bfloatp ($imagpart z2))))
       (sub
         (complex-bfloat-gamma-incomplete
           (add ($bfloat ($realpart a)) (mul '$%i ($bfloat ($imagpart a))))
           (add ($bfloat ($realpart z1)) (mul '$%i ($bfloat ($imagpart z1)))))
         (complex-bfloat-gamma-incomplete
           (add ($bfloat ($realpart a)) (mul '$%i ($bfloat ($imagpart a))))
           (add ($bfloat ($realpart z2)) (mul '$%i ($bfloat ($imagpart z2)))))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       ;; Expand gamma_incomplete_generalized(a+n,z1,z2) with n an integer
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (mul
              (simplify (list '($pochhammer) a n))
              (add
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2))
                (mul
                  (power '$%e (mul -1 z1))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z1 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t)))
                (mul -1
                  (power '$%e (mul -1 z2))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z2 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t))))))

           ((< n 0)
            (setq n (- n))
            (add
              (mul
                (div
                  (power -1 n)
                  (simplify (list '($pochhammer) (sub 1 a) n)))
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))
              (mul -1
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z1 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t)))
              (mul
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z2 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t))))))))

      (t (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
