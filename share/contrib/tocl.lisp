#| Copyright 2006 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

If you need to use a Maxima expression in a Common Lisp program,
the function 'common_lisp' might be useful to you. Basically,
'common_lisp' converts a Maxima expression into a Lisp lambda form.
It converts Maxima operators into their closest Common Lisp 
counterparts. Thus Maxima addition is converted into the Common 
Lisp '+' function. So the lambda form that common_lisp generates 
should work OK with numerical inputs, but not symbolic inputs.

A few examples might be the easiest way to explain what 
'common_lisp' does:

(%i1) common_lisp(a+b*c);
(LAMBDA (A B C) (+ (* B C) A)) 
(%o1) done
(%i2) common_lisp(cos(x+b) - f(z));
(LAMBDA (B X Z) (+ (COS (+ B X)) (- (F Z)))) 
(%o2) done

The function common_lisp doesn't work correctly for conditionals, 
Maxima blocks, assignments, and etc. It does (or at least it
is supposed to) work correctly for expressions that involve
polynomials and trig-like functions.
|#

(defun $common_lisp (e)
  (let (($listconstvars nil))
    (print `(lambda ,(sort (mapcar 'stripdollar (margs ($listofvars e))) 'string<) 
	      ,(expr-to-cl (nformat ($ratdisrep e)))))
    '$done))
  
(setf (get 'mplus 'cl-function) '+)
(setf (get 'mminus 'cl-function) '-)
(setf (get 'mtimes 'cl-function) '*)
(setf (get 'mquotient 'cl-function) '/)
(setf (get 'mexpt 'cl-function) 'expt)
(setf (get 'mlessp 'cl-function) '<)
(setf (get 'mgreaterp 'cl-function) '>)
(setf (get 'mgeqp 'cl-function) '>=)
(setf (get 'mleqp 'cl-function) '<=)

(defun mapatom-expr-to-cl (e)
  (cond ((eq e '$%i) (complex 0 1))
	((integerp e) e)
	(($ratnump e) `(/ ,($num e) ,($denom e)))
	((eq e '$%pi) pi)
	(($constantp e) ($float e)) ;; converts big floats to doubles
	(t (stripdollar e))))
	
(defun expr-to-cl (e)
  (if ($mapatom e) (mapatom-expr-to-cl e)
    `(,(or (get (mop e) 'cl-function) (stripdollar (mop e))) ,@(mapcar 'expr-to-cl (margs e))))))



