;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sqrtdenest - denest an expression containing square roots of square roots
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
;;;
;;; Copyright (C) 2016 David Billinghurst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SQRTDENEST simplifies square roots of square roots.
;;;
;;; This implementation uses the same algorithm as the maxima language
;;; SQRTDENEST in share package sqdnst.mac.  It only handles simple cases.
;;;
;;; Reference:
;;;
;;;  Jeffrey, David J. and Rich, Albert D. (1999)
;;;  Simplifying Square Roots of Square Roots by Denesting,
;;;  in Computer Algebra Systems (Ed. M. J. Wester)
;;;  <http://www.cybertester.com/data/denest.pdf>
;;;
;;; Further reading:
;;;
;;; A. Borodin and R. Fagin and J. Hopcroft and M. Tompa. (1985)
;;; Decreasing the Nesting Depth of expressions Involving Square Roots.
;;; J. Symbolic Computation, 1:169-188
;;; <http://www.almaden.ibm.com/cs/people/fagin/symb85.pdf>
;;;
;;; Landau, Susan (1992) A Note on Zippel Denesting.
;;; Journal of Symbolic Computation 13:41-45
;;; 
;;; Landau, Susan (1992) Simplification of Nested Radicals.
;;; SIAM Journal on Computing, 21: 85-110.
;;;
;;; Landau, Susan (1994). How to Tangle with a Nested Radical.
;;; Math. Intell. 16:49-55
;;;
;;; Zippel, Richard (1985) Simplification of Expressions involving Radicals.
;;; Journal of Symbolic Computation, 1:189-210.
;;;
;;; Python sympy sqrtdenest function
;;; http://docs.sympy.org/latest/_modules/sympy/simplify/sqrtdenest.html
;;; which references Borodin et al (1985), Jeffrey and Rich (2009)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmfun $sqrtdenest (e)
  "Denest square roots in expression e"
  (sqrtdenest1 e))

(defun sqrtdenest1 (e)
  "Denest square roots in expression e"
  (cond
   ((mapatom e) e)
   ((eq (mop e) 'mexpt) (sqrtdenest2 e))
   (t `((,(mop e)) ,@(mapcar #'sqrtdenest1 (rest e))))))

;;; Simple denesting of square roots.  Uses algorithm in maxima share package
;;; sqdnst.mac.  Should give identical results.
;;;
;;; Ref: Jeffrey and Rich (1999), Section 4.5.2
;;;
;;; Let X,Y real with X > Y > 0
;;;
;;; Consider
;;;     sqrt(X+Y) = sqrt(A) + sqrt(B)         (4.9)
;;;
;;; Squaring both sides
;;;     X + Y = A + B + 2 sqrt(AB)           (4.10)
;;;
;;; One way to satisfy this is to set X=A+B and Y^2=4AB
;;;
;;; Also have
;;;     sqrt(X-Y) = sqrt(A) - sqrt(B)          (4.11)
;;;
;;; Solve for A and B in terms of X and Y to derive:
;;;
;;; Theorem 4.12: Let X,Y real with X > Y > 0     
;;; sqrt(X +/- Y) = sqrt(X/2+sqrt(X^2-Y^2)/2) +/- sqrt(X/2-sqrt(X^2-Y^2)/2)
;;;
;;; Apply this below by testing discriminant D = sqrt(1-(Y/X)^2)
;;;   If $numberp(D) then theorem 4.12 denests the square-root as
;;;   sqrt(X +/- Y) =  sqrt(X*(1+D)/2) +/- *sqrt(X*(1-D)/2)
;;; Note: X>0 and D<1 so both sqrt terms on RHS are real.
(defun sqrtdenest2 (e)
  "Denest square roots in maxima expression e of form a^b"
  (let ((a (simplify (sqrtdenest1 (second e))))
	(b (simplify (sqrtdenest1 (third e))))
	x y D)
    (cond ((and ($evenp ($denom b))
		(not (mapatom a))
		(eq (mop a) 'mplus)
		(progn
		  (setq x ; maximum of args(a)
			(simplify `(($max) ,@(rest a))))
		  (setq y (sub a x)) ; a-x
		  (setq D ; sqrt(1-(y/x)^2)
			(root (sub 1 (power (div y x) 2)) 2))
		  ($numberp D)))
	   ;; (sqrt(x*(1+D)/2)+signum(y)*sqrt(x*(1-D)/2))^(2*b)
	   (power (add (root (mul 1//2 x (add 1 D)) 2)
		       (mul (take '(%signum) y)
			    (root (mul 1//2 x (sub 1 D)) 2)))
		  (mul 2 b)))
	  ;; Didn't denest at this level, but may have at a lower level.
	  (t (power a b)))))
