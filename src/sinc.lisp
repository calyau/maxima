;; sinc: sinc function support for Maxima
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(in-package :maxima)

(defun pure-constant-p (e)
"Return true if `e` is either a Maxima number or a sum, product, or power whose 
 arguments are all pure constants. Symbolic constants such as %pi, %e, and %i are 
 not considered numeric and cause the predicate to return nil."
  (cond ((mnump e) t)
        ((or (mplusp e) (mtimesp e) (mexptp e)) (every #'pure-constant-p (cdr e)))))

;; For IEEE floats, sin(x)/x is numerically safe for numerical evaluation including
;; subnormals. It is also numerically safe for bigfloats. Special casing small nonzero
;; inputs is not needed.
(defun sinc-float (x)
 "Evaluate sinc(x) for float, bigfloat, or complex float or bigfloat input. When $numer 
 is true, all numeric inputs are coerced to IEEE floats. Exception: for x = 0, return an 
 exact 1 when $numer is false; otherwise return an IEEE float 1.0."
  (cond
    ;; Special case: sinc(0); return one of the proper type
    ((zerop1 x)
       (cond ((or (floatp x) $numer) 1.0) ; yes: block([numer:true], sinc(0.0b0)= 1.0 (not 1.0b0)
             (($bfloatp x) *bigfloatone*)
             (t 1)))
    (t
     (multiple-value-bind (flag re im)
         (complex-number-p x #'mnump)
       (cond
         ((not flag) nil) ;no floating evaluation possible: return nil
         (t
          ;; when $numer is true, promote re & im to IEEE floats
          (when $numer
            (setq re ($float re)
                  im ($float im)))
          ;; At this point, re and im are guaranteed to be mnump numbers (by complex-number-p).
          ;; This test only checks whether floating evaluation is possible.
          (cond
            ((not (or (float-or-bigfloat-p re) (float-or-bigfloat-p im))) nil)
            (t
             (let ((z (if (zerop1 im)
                             (bigfloat::to re) ; real case: no complex number division
                             (bigfloat::to re im))))
                 (maxima::to (bigfloat::/ (bigfloat::sin z) z)))))))))))

(def-simplifier sinc (x) 
   (cond ((zerop1 x) (sinc-float x))
         ((taylorize (mop form) (second form)))
         ((sinc-float x))
         (t               
          (let* ((z (ftake '%sin x)))
              (cond 
                 ;; When sin(x) is a pure constant and x is not zero, return sin(x)/x.
                 ;; Here we know that x is not a float or a complex float.
                 ((and z (pure-constant-p z) (eq t (mnqp 0 x))) (div z x)) 
                 ;; even reflection rule: sinc(-x) = sinc(x)
                 ((great (neg x) x) (ftake '%sinc (neg x))) 
                 ;; sinc nounform return
                 (t (give-up))))))) 
   
;; sinc commutes with the conjugate, so give '%sinc the 'commutes-with-conjugate property
(setf (get '%sinc 'commutes-with-conjugate) t)

(defun simplim%sinc (e x pt)  
 "Return limit(sinc(X),x,pt)."
  (let* ((*preserve-direction* t) 
         (lim (limit (cadr e) x pt 'think)))
     (cond ((eq lim '$ind) '$ind) ; sinc(ind) = ind
           ((or (eq lim '$minf) (eq lim '$inf)) 0) ; sinc(minf) = 0 & sinc(inf) = 0
           ((zerop2 lim) 1) ; includes zerob & zeroa
           ((or (eq lim '$und) (eq lim '$infinity)) (throw 'limit nil)) ; don't know
           (t (ftake '%sinc lim))))) ; use direct substitution
(setf (get '%sinc 'simplim%function) 'simplim%sinc)

;; Derivative of sinc: x -> (cos(x)-sinc(x))/x
(defgrad %sinc ($x)
  #$$ (cos(x)-sinc(x))/x$
  )

;; Antiderivative of sinc: x -> expintegral_si(x)
;; Tradeoff: Maxima integrates sin(x)/x in terms of the incomplete gamma function,
;; but the expression is not continuous at zero and the result is not manifestly 
;; real. Integrating sinc gives a better result. But Maxima can integrate 
;; sin(x)^2/x^2, but it fails to integrate sinc(x)^2.
(putprop '%sinc
  '((x) ((%expintegral_si) x)) 'integral)

(defun taylor-sinc (a b)
  "Return a list of dotted pairs (p . q), where p = (2k . 1) and
   q = ((-1)^k . (2k+1)!) for k from 0 to n-1, where n = 1+(floor (car a) 2).

   Each dotted pair q represents a coefficient of the Taylor polynomial
   by (/ (car q) (cdr q)), and p represents the exponent by (/ (car p) (cdr p)).

   The second argument `b` is required by a general scheme used by many
   functions, but not by sinc, so we ignore `b`."
  (declare (ignore b))
  (let ((ord (+ 1 (floor (car a) 2))) (cfs nil) (w 1) (sgn 1))
    (dotimes (k ord)
      (let* ((k2 (* 2 k))
             (p  (cons k2 1))
             (q  (cons sgn w)))
        (push (cons p q) cfs)
        ;; update (-1)^k and (2k+1)!
        (setf sgn (- sgn)
              w   (* w (+ k2 2) (+ k2 3)))))
    (nreverse cfs)))
(setf (get '%sinc 'exp-form) (list (list 'taylor-sinc (cons 1 1)) (cons (cons 0 1) nil)))

;; Power series centered at zero for sinc
(setf (get '%sinc 'sp2)
'((%sum)
 ((mtimes) ((mexpt) -1 *index) ((mexpt) ((mfactorial) ((mplus) 1 ((mtimes) 2 *index))) -1)
  ((mexpt) sp2var ((mtimes) 2 *index)))
 *index 0 $inf))

(defun risplit-sinc (x)
"Return the rectangular form of sinc(x). The argument x is a list of the form 
 ((%sinc) z).This function calls risplit on  sin(z)/z." 
    (let ((z (cadr x)))
      (risplit (div (ftake '%sin z) z))))
(putprop '%sinc 'risplit-sinc 'risplit-function)

;; When  -%pi <= x & x <= %pi, this code handles sign(sinc(x)). We don't attempt to find the sign for
;; any other cases.
(defun sign-sinc (e) ; e = sinc(x)
     (let ((x (cadr e)) (y 0))
       ;; When *complexsign* is true, find the rectangular form of 
       ;; the argument to sin.
       (when *complexsign* 
          (setq x (risplit x))
          (setq y (cdr x)
                x (car x)))
       (cond 
             ;; When y = 0 and -%pi < x < %pi, sign(sinc(x)) = $pos
             ((and (eql y 0)
                   (eq t (mgrp x (mul -1 '$%pi))) 
                   (eq t (mgrp '$%pi x)))
                (setf sign '$pos))

            ;; When y = 0 and -%pi <= x <= %pi, sign(sinc(x)) = $pz
             ((and (eql y 0)
                   (eq t (mgqp x (mul -1 '$%pi))) 
                   (eq t (mgqp '$%pi x)))
                (setf sign '$pz))
           
              ;; When *complexsign* is true & y # 0, set sign to complex.
              ;; To test y # 0, we'll use (not (eql y 0)))
              ((and *complexsign* (not (eql y 0)))
                (setf sign '$complex))
			        (t (setf sign '$pnz))))
		nil)
(putprop '%sinc 'sign-sinc 'sign-function)

;; Tex support for sinc
(defprop %sinc "\\sinc" texword)
