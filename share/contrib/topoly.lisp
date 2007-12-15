;;  Author Barton Willis
;;  University of Nebraska at Kearney
;;  Copyright (C) 2006, 2007 Barton Willis

;;  This program is free software; you can redistribute it and/or modify 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.				 
 		       								 
;;  You should have received a copy of the GNU General Public License	
;;  along with this program; if not, write to the Free Software 		 
;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

($load '$polynomialp)

(defmacro opapply (op args)
  `(simplify (cons (list ,op) ,args)))

;; The next three functions convert max and min to abs functions.

(defun max-to-abs (e)
  (reduce #'(lambda (a b) (div (add (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun min-to-abs (e)
  (reduce #'(lambda (a b) (div (sub (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun convert-from-max-min-to-abs (e)
  (cond (($mapatom e) e)
	((op-equalp e '$max) (max-to-abs (mapcar 'convert-from-max-min-to-abs (margs e))))
	((op-equalp e '$min) (min-to-abs (mapcar 'convert-from-max-min-to-abs (margs e))))
	(t (opapply (mop e) (mapcar 'convert-from-max-min-to-abs (margs e))))))

(defun maxima-variable-p (e)
  (or (symbolp e) ($subvarp e)))

(defun list-subst (l p)
  (if (null l) p (list-subst (rest l) ($substitute (first l) p))))
 
;; to_poly(p,vars) returns a polynomial in the variables 'vars' that has a zero whenever
;; p has a zero. When 1 is a member of vars, constant terms, such as sqrt(5) also get
;; converted to polynomial form. The value of vars defaults to all variables including 
;; constants.

(defun $to_poly (p &optional (vars 'convert-all-vars))
  (let (($listconstvars t) (q) (convert-cnst nil))

    (if (eq vars 'convert-all-vars) (setq vars ($cons 1 ($listofvars p))))
    
    (if (not ($listp vars))
	(merror "The second argument to 'topoly' must be a list"))
    
    (cond (($member 1 vars) 
	   (setq convert-cnst t)
	   (setq vars ($delete 1 vars))))

    (setq p (meqhk p))
    (setq q ($ratdenom p))
    (if (not ($constantp q)) (mtell "Assuming that ~:M~%" `((mnotequal) ,q 0)))
   
    ;; It's OK to send every expression through convert-from-max-min-to-abs.
    ;; I put in the conditional to skip the ratsimp for expressions that don't
    ;; involve max or min.

    (setq p (if ($freeof '$max '$min p) p (sratsimp (convert-from-max-min-to-abs p))))
    
    (setq p (to-polynomial p vars convert-cnst))
    `((mlist) ((mlist) ,(first p) ,@(second p)) ((mlist) ,@(third p)))))
   
(defun to-polynomial (p vars convert-cnst)
  (let ((n) (b) (nv) (acc nil) (subs nil) (pk) (q) (inequal) (np-subs))
    (cond ((or (maxima-variable-p p)
	       (mnump p)
	       (and ($emptyp vars) (not convert-cnst))
	       (and (not ($constantp p)) ($lfreeof vars p))
	       (and ($constantp p) (not convert-cnst)))
	   (list p nil nil nil))
	
	  ((mexptp p)
	   (setq n (nth 2 p))
	   (setq b (nth 1 p)) 
	   (cond ((and (integerp n) (> n 0))
		  (list p nil nil nil))

		 (($ratnump n)
		  (setq b (to-polynomial b vars convert-cnst))
		  (setq subs (second b))
		  (setq inequal (third b))
		  (setq np-subs (fourth b))
		  (setq b (first b))
		  (setq nv (gentemp "$%G"))
		  (cond ((or (mgrp n 0) (mnump b))
			 (push (take '(mleqp) (take '($carg) nv) (mul n '$%pi)) inequal)
			 (push (take '(mlessp) (mul -1 '$%pi n) (take '($carg) nv)) inequal)
			 (push (take '(mequal) (power b ($num n)) (power nv ($denom n))) subs)
			 (push (take '(mequal) p nv) np-subs)
			 (list nv subs inequal np-subs))

			(t
			 (setq n (neg n))
			 (push (take '(mequal) 1 (mul (power nv ($denom n)) (power b ($num n)))) subs)
			 (push (take '(mlessp) (take '($carg) nv) (mul n '$%pi)) inequal)
			 (push (take '(mleqp) (mul -1 '$%pi n) (take '($carg) nv)) inequal)
			 (push (take '(mnotequal) nv 0) inequal)
			 (list nv subs inequal np-subs))))
			 
		 (t (merror "Nonalgebraic argument given to 'topoly'"))))

	  ((op-equalp p 'mabs)
	   (setq b (to-polynomial (first (margs p)) vars convert-cnst))
	   (setq acc (second b))
	   (setq inequal (third b))
	   (setq np-subs (fourth b))
	   (setq b (first b))
	   (setq nv (gentemp "$%G"))
	   (list nv (cons (take '(mequal) (power b 2) (power nv 2)) acc)
		 (cons (take '(mequal) (take '($carg) nv) 0)  inequal) np-subs))
			 
	  ((mtimesp p)
	   (setq acc 1)
	   (setq p (margs p))
	   (while p
	     (setq pk (first p))
	     (setq p (rest p))
	     (setq q (to-polynomial pk vars convert-cnst))
	     (setq acc (mul acc (first q)))
	     (setq subs (append (second q) subs))
	     (setq inequal (append inequal (third q)))
	     (setq np-subs (append np-subs (fourth q)))
	     (setq vars ($append vars ($listofvars `((mlist) ,@subs))))
	     
	     (setq p (mapcar #'(lambda (s) (list-subst np-subs s)) p)))
	   (list acc subs inequal np-subs))

	  ((mplusp p)
	   (setq acc 0)
	   (setq p (margs p))
	   (while p
	     (setq pk (first p))
	     (setq p (rest p))
	     (setq q (to-polynomial pk vars convert-cnst))
	     (setq acc (add acc (first q)))
	     (setq subs (append (second q) subs))
	     (setq inequal (append (third q) inequal))
	     (setq np-subs (append (fourth q) np-subs))
	     (setq vars ($append vars ($listofvars `((mlist) ,@subs))))
	     (setq p (mapcar #'(lambda (s) (list-subst np-subs s)) p)))
	     
	   (list acc subs inequal np-subs))

	  (t (merror "Nonalgebraic argument given to 'topoly'")))))


#|
  Things I don't like about eliminate:

(1)  eliminate([x + x*y + z-4, x+y+z-12, x^2 + y^2 + z^2=7],[x,y,z]) -> [4]

Here Maxima solves for z. There are more than one solution for z, but Maxima returns
just one solution. A user might think that there is one solution or that
the equations are inconsistent.

(2)  eliminate([x+y-1,x+y-8],[x,y]) -> Argument to `last' is empty.

Here the equations are inconsistent, but we get an error (from solve) instead
of a clear message about what happened.

(3) eliminate([a],[x]) -> Can't eliminate from only one equation -- an error.  
but eliminate([a,a],[x]) -> [a,a]. This is silly.

(4) eliminate([x],[]) -> Can't eliminate from only one equation.
but eliminate([x,x],[]) -> [x,x]. Again, this is silly.

(5) elim([x^3-y^2,x^7 + y+z*p,q*q-23+ x+y,p-q,x+y+z-5],[x,y,z,p]) takes 0.3 second
but eliminate([x^3-y^2,x^7 + y+z*p,q*q-23+ x+y,p-q,x+y+z-5],[x,y,z,p]) takes 
a long time (I never let it run to completion). Unlike 'eliminate,' the function 'elim' 
makes some guesses about which polynomial to use as the pivot and which variable
to eliminate.
|#

(defun maxima-variable-p (e)
  (or (symbolp e) ($subvarp e)))

(defun require-maxima-variable (x context-string)
  (setq x (ratdisrep x))
  (if (maxima-variable-p x) x
    (merror "Function ~:M expects a symbol, instead found ~:M" context-string x)))

;; Simplify a polynomial equation p = 0 by 

;;   (1) nonzero constant * p --> p,
;;   (2) p^n --> p.

;; If you want to use $factor instead of $sqfr, go ahead. But if you do that, you might want to
;; locally set factorflag to false.

(defun suppress-multiple-zeros (q)
  (setq q ($sqfr q))
  (cond ((mnump q) (if (zerop1 q) 0 1))
	((mtimesp q) (muln (mapcar 'suppress-multiple-zeros (margs q)) t))
	((and (mexptp q) (integerp (third q)) (> (third q) 0)) (second q))
	(($constantp q) 1) ; takes care of things like (1 + sqrt(5)) * x --> x.
	(t q)))

;; Using eq as the "pivot," eliminate x from the list or set of equations eqs. 

(defun $eliminate_using (eqs eq x)
  (if (or ($listp eqs) ($setp eqs))
      (progn
	(setq eqs (mapcar #'(lambda (s) ($ratexpand (meqhk s))) (margs eqs)))
	(setq eqs (margs (simplify (cons '($set) eqs)))))
    (merror "The first argument to 'eliminate_using' must be a list or a set"))

  (setq x (require-maxima-variable x "$eliminate_using"))
  
  (if (not (every #'(lambda (s) ($polynomialp s `((mlist) ,x) 
					      `((lambda) ((mlist) s) (($freeof) ,x s)))) eqs))
      (merror "The first argument to 'eliminate_using' must be a set or list of polynomials"))

  (setq eq ($ratexpand (meqhk eq)))
  (if (not ($polynomialp eq `((mlist) ,x) `((lambda) ((mlist) s) (($freeof) ,x s))))
      (merror "The second argument to 'eliminate_using' must be a polynomial"))
   
  (setq eqs (mapcar #'suppress-multiple-zeros eqs))
  (setq eq (suppress-multiple-zeros eq))
  (simplify `(($set) ,@(eliminate-using eqs eq x))))
   
(defun eliminate-using (eqs eq x)
  (delete 0 (mapcar #'(lambda (s) (suppress-multiple-zeros ($resultant s eq x))) eqs)))

;; Return an upper bound for the total degree of the polynomial p in the variables vars.
;; When p is fully expanded, the bound is exact.

(defun degree-upper-bound (p vars)
  (cond ((maxima-variable-p p) (if (member p vars :test #'equal) 1 0))

	((mnump p) 0)

	((and (mexptp p) (integerp (third p)) (> (third p) 0))
	 (* (degree-upper-bound (nth 1 p) vars) (nth 2 p)))

	((mtimesp p)
	 (reduce '+ (mapcar #'(lambda (s) (degree-upper-bound s vars)) (margs p))))

	((mplusp p)
	 (simplify `(($max) ,@(mapcar #'(lambda (s) (degree-upper-bound s vars)) (margs p)))))

	((apply '$freeof (append vars (list p))) 0)
	(t (merror "Nonpolynomial argument given to degree-upper-bound"))))

(defun unk-eliminate (eqs vars &optional (pivots nil))
   (let ((ni) (n-min nil) (xeqs `(($set))) (pivot-var) (pivot-eq) (acc `(($set))) ($ratfac nil))
     (cond ((or (null vars) (null eqs)) (list eqs pivots))
	   (t

	    ;; The pivot is a nonconstant member of eqs with minimal total degree.
	    ;; The  constant members of eqs get adjoined into acc -- all other  members get
	    ;; adjoined into xeqs. Since each member of eqs has been ratexpanded,
	    ;; the degree bound is exact.
	    
	    (dolist (ei eqs)
	      (setq ei ($ratexpand (suppress-multiple-zeros ei)))
	      (setq ni (degree-upper-bound ei vars))

	      (if (and (or (null n-min) (< ni n-min)) (> ni 0))
		  (setq n-min ni pivot-eq ei))
	      
	      (if (and (> ni 0) (not (equal 0 ei))) (setq xeqs ($adjoin ei xeqs)) (setq acc ($adjoin ei acc))))
	    	    
	    (setq xeqs (margs xeqs))
	    (setq acc (margs acc))
	    ;; Now we'll decide which variable to eliminate. The pivot variable
	    ;; is the variable that has the least (but nonzero) degree in pivot-eq.
	    
	    (setq n-min nil)
	    (dolist (vi vars)
	      (setq ni (degree-upper-bound pivot-eq (list vi)))
	      (if (and (or (null n-min) (< ni n-min)) (> ni 0))
		  (setq pivot-var vi n-min ni)))
	  
	    (if (null pivot-var) (list eqs pivots)
	      (unk-eliminate (append acc (eliminate-using xeqs pivot-eq pivot-var)) (delete pivot-var vars) 
			     (cons pivot-eq pivots)))))))

(defun $elim (eqs x)
  (if (or ($listp eqs) ($setp eqs))
      (progn
	(setq eqs (mapcar #'(lambda (s) ($ratexpand (suppress-multiple-zeros (meqhk s)))) (margs eqs)))
	(setq eqs (margs (opapply '$set eqs))))
    (merror "The first argument to 'elim' must be a list or a set"))
  
  (setq x (margs (cond (($listp x) ($setify x))
		       (($setp x) x)
		       (t (merror "The second argument to 'elim' must be a list or a set")))))
  
  (setq x (mapcar #'(lambda (s) (require-maxima-variable s "$elim")) x)) 
  
  (setq x (opapply 'mlist x))
  (if (not (every #'(lambda (s) ($polynomialp s x `((lambda) ((mlist) s) (($lfreeof) ,x s)))) eqs))
      (merror "Each member of the first argument to 'elim' must be a polynomial"))

  (setq x (margs x))
  (opapply 'mlist (mapcar #'(lambda (s) (opapply 'mlist s)) (unk-eliminate eqs x))))

(defun $elim_allbut (eqs x)
  (let (($listconstvars nil) (v))
    (setq v ($listofvars eqs))
    (setq x (if ($listp x) ($setify x) (take '($set) x)))
    (setq v ($setdifference ($setify v) x))
    ($elim eqs ($listify v))))