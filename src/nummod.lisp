;; Maxima functions for floor, ceiling, and friends
;; Copyright (C) 2004, 2005, Barton Willis

;; Barton Willis
;; Department of Mathematics, 
;; University of Nebraska at Kearney
;; Kearney NE 68847
;; willisb@unk.edu

;; This source code is licensed under the terms of the Lisp Lesser 
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU 
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL, 
;; the preamble takes precedence. 

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

(in-package :maxima)
(macsyma-module nummod)

;; Let's have version numbers 1,2,3,...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mfuncall '$declare '$integervalued '$feature)
  ($put '$nummod 2 '$version))
 
;; charfun(pred) evaluates to 1 when the predicate 'pred' evaluates
;; to true; it evaluates to 0 when 'pred' evaluates to false; otherwise,
;; it evaluates to a noun form.

(defprop $charfun simp-charfun operators)

(defun simp-charfun (e bool z) 
  (oneargcheck e)
  (setq e (specrepcheck e))  
  (cond ((member 'simp (car e)) e)
	(t
	 (let (($prederror nil))
	   (setq e (simplifya ($ratdisrep (nth 1 e)) z))
	   (setq bool (mevalp (mfuncall '$ev e '$nouns)))
	   (cond ((eq bool t) 1)
		 ((eq bool nil) 0)
		 (t `(($charfun simp) ,e)))))))
	     
(defun integer-part-of-sum (e)
  (let ((i-sum 0) (n-sum 0) (o-sum 0) (n))
    (setq e (margs e))
    (dolist (ei e)
      (cond ((maxima-integerp ei) 
	     (setq i-sum (add ei i-sum)))
	    ((or (ratnump ei) (floatp ei) ($bfloatp ei))
	     (setq n-sum (add ei n-sum)))
	    (t
	     (setq o-sum (add ei o-sum)))))
    (setq n (simplify `(($floor) ,n-sum)))
    (setq i-sum (add i-sum n))
    (setq o-sum (add o-sum (sub n-sum n)))
    (values i-sum o-sum)))
  		  
(defprop $floor simp-floor operators)

(defprop $floor tex-matchfix tex)
(defprop $floor (("\\left \\lfloor " ) " \\right \\rfloor") texsym)

;; When constantp(x) is true, we use bfloat evaluation to try to determine
;; the floor. If numerical evaluation of e is ill-conditioned, this function 
;; can misbehave.  I'm somewhat uncomfortable with this,  but it is no worse 
;; than some other stuff. One safeguard -- the evaluation is done with three
;; values for fpprec.  When the floating point evaluations are  
;; inconsistent, bailout and return nil.  I hope that this function is
;; much more likely to return nil than it is to return a bogus value.

(defun pretty-good-floor-or-ceiling (x fn &optional (digits 'no-value))
  (let (($fpprec) ($float2bf t))
    (cond ((eq digits 'no-value)

	   ;; When x doesn't evaluate to a bigfloat, bailout and return nil.  
	   ;; This happens when, for example, x = asin(2). For now, bfloatp
	   ;; evaluates to nil for a complex big float.  If this ever changes,
	   ;; the freeof(%i, xf) will detect the complex big float and return nil.

	   (meval  `((msetq) $fpprec 25))
	   (let (($algebraic t) (xf ($bfloat (setq x ($rectform x)))))
	     (if (and ($bfloatp xf) ($freeof '$%i xf)) 
		 (pretty-good-floor-or-ceiling x fn 
					       (max 25 (ceiling (* 0.4 (nth 2 xf))))) nil)))
	  (t
	   (let ((f1) (f2) (f3) (xf) (eps) (lb) (ub) (n))
	     (meval `((msetq) $fpprec ,digits))
	     (setq f1 (mfuncall fn ($bfloat x)))
	     
	     (incf digits 20)
	     (meval `((msetq) $fpprec ,digits))
	     (setq f2 (mfuncall fn ($bfloat x)))
	     
	     (incf digits 20)
	     (meval `((msetq) $fpprec ,digits))
	     (setq f3 (mfuncall fn (setq xf ($bfloat x))))
	    
	     ;; Let's say that the true value of x is in the interval
	     ;; [xf - |xf| * eps, xf + |xf| * eps], where eps = 10^(20 - digits).
	     ;; Define n to be the number of integers in this interval; we have
	     
	     (setq eps (power ($bfloat 10) (- 20 digits)))
	     (setq lb (sub xf (mult (simplify (list '(mabs) xf)) eps)))
	     (setq ub (add xf (mult (simplify (list '(mabs) xf)) eps)))
	     (setq n (sub (mfuncall '$ceiling ub) (mfuncall '$ceiling lb)))
	     	     
	     ;; Provided f1, f2, and f3 are the same and that n = 0, return f1.
	     
	     (if (and (= f1 f2) (= f2 f3) (= n 0)) f1 nil))))))

;; (a) The function fpentier rounds a bigfloat towards zero--we need to
;;     check for that.

;; (b) Mostly for fun, floor(minf) --> und and etc. I suppose floor 
;;     should be undefined for many other arguments---for example
;;     floor(a < b), floor(true).  

;;     I think floor(complex number) should be undefined too.  Some folks
;;     might like floor(a + %i b) --> floor(a) + %i floor(b). But
;;     this would violate the integer-valuedness of floor.

(defun simp-floor (e e1 z)
  (oneargcheck e)
  (setq e (specrepcheck e))
  (cond ((not (member 'simp (car e)))
	 (setq e (simplifya ($ratdisrep (nth 1 e)) z))
	 (if (ratnump e) (setq e (/ (cadr e) (caddr e))))
	 (cond ((numberp e) (floor e))

	       (($bfloatp e)
		(setq e1 (fpentier e))
		(if (and (minusp (cadr e)) (not (zerop1 (sub e1 e))))
		    (sub1 e1) e1))
	
	       (($orderlessp e (neg e))
		(sub* 0 (simplifya `(($ceiling) ,(neg e)) nil)))

	       ((maxima-integerp e) e)

	       ((and (setq e1 (mget e '$numer)) (floor e1)))
	      
	       ((or (member e infinities) (eq e '$und) (eq e '$ind)) '$und)
	       ((or (like e '$zerob)) -1)
	       ((or (like e '$zeroa)) 0)
	      	      
	       ((and ($constantp e) (pretty-good-floor-or-ceiling e '$floor)))

	       ((mplusp e)
		(let ((i-sum) (o-sum))
		  (multiple-value-setq (i-sum o-sum) (integer-part-of-sum e))
		  (setq o-sum (if (like i-sum 0) `(($floor simp) ,o-sum) 
				(simplifya `(($floor) ,o-sum) nil)))
		  (add i-sum o-sum)))
	       
	       ;; handle 0 < e < 1 implies floor(e) = 0 and 
               ;; -1 < e < 0 implies floor(e) = -1.

	       ((and (eq ($compare 0 e) '&<) (eq ($compare e 1) '&<)) 0)
	       ((and (eq ($compare -1 e) '&<) (eq ($compare e 0) '&<)) -1)
	       (t `(($floor simp) ,e))))
	(t e)))

(defprop $ceiling simp-ceiling operators)

(defprop $ceiling tex-matchfix tex)
(defprop $ceiling (("\\left \\lceil " ) " \\right \\rceil") texsym)

(defun simp-ceiling (e e1 z)
  (oneargcheck e)
  (setq e ($ratdisrep e))  
  (cond ((not (member 'simp (car e)))
	 (setq e (simplifya ($ratdisrep (nth 1 e)) z))
	 (if (ratnump e) (setq e (/ (cadr e) (caddr e))))
	 (cond ((numberp e) (ceiling e))
	       (($bfloatp e)
		(sub 0 (simplify `(($floor) ,(sub 0 e)))))

	       (($orderlessp e (neg e))
		(sub* 0 (simplifya `(($floor) ,(neg e)) nil)))
	       
	       ((maxima-integerp e) e)
	       ((and (setq e1 (mget e '$numer)) (ceiling e1)))
	       
	       ((or (member e infinities) (eq e '$und) (eq e '$ind)) '$und)
	       ((or (like e '$zerob)) 0)
	       ((or (like e '$zeroa)) 1)

	       ((and ($constantp e) (pretty-good-floor-or-ceiling e '$ceiling)))
	       
	       ((mplusp e)
		(let ((i-sum) (o-sum))
		  (multiple-value-setq (i-sum o-sum) (integer-part-of-sum e))
		  (setq o-sum (if (like i-sum 0) `(($ceiling simp) ,o-sum) 
				(simplifya `(($ceiling) ,o-sum) nil)))
		  (add i-sum o-sum)))
	      

	       ;; handle 0 < e < 1 implies ceiling(e) = 1 and 
               ;; -1 < e < 0 implies ceiling(e) = 0.

	       ((and (eq ($compare 0 e) '&<) (eq ($compare e 1) '&<)) 1)
	       ((and (eq ($compare -1 e) '&<) (eq ($compare e 0) '&<)) 0)
	       (t `(($ceiling simp) ,e))))		
	 (t e)))

(defprop $mod simp-nummod operators)
(defprop $mod tex-infix tex)
(defprop $mod (" \\rm{mod} ") texsym)
(defprop $mod 180. tex-rbp)
(defprop $mod 180. tex-rbp)

;; See "Concrete Mathematics," Section 3.21.

(defun simp-nummod (e e1 z)
  (twoargcheck e)
  (cond ((not (member 'simp (car e)))
	 (setq e (mapcar #'specrepcheck (margs e)))
	 (setq e (mapcar #'(lambda (s) (simplifya s z)) e))
	 (let ((x (nth 0 e)) (y (nth 1 e)))
	   (cond ((or (like 0 y) (like 0 x)) x) 
		 ((like 1 y) (sub* x `(($floor) ,x)))
		 ((and ($constantp x) ($constantp y) (not (like 0 y)))
		  (sub* x (mul* y `(($floor) ,(div x y)))))
		 ((not (like 1 (setq e1 ($gcd x y))))
		  (mul* e1 `(($mod) ,(div* x e1) ,(div* y e1))))
	
		 (t `(($mod simp) ,x ,y)))))
	(t e)))

