#|
  Copyright 2006 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Usage: The function 'opsubst' is similar to the function 'subst', except that
'opsubst' only makes substitutions for the operators in an expression. Specifically,

 opsubst(f,g,e) --> When 'f' is an operator in the expression e, substitute 'g' 
                    for 'f' in the expression 'e'. 
 opsubst(g=f,e) --> opsubst(f,g,e).
 opsubst([],e) --> e.
 opsubst([g1=f1, g2=f2, ..., gn=fn],e) --> opsubst([g2=f2,...,gn=fn], opsubst(f1=f1, e)).

 Examples:

(%i1) opsubst(f,g,g(g(x)));
(%o1) f(f(x))
(%i2) opsubst(f,g,g(g));
(%o2) f(g)
(%i3) opsubst(f,g[x],g[x](z));
(%o3) f(z)
(%i4) opsubst(g[x],f, f(z));
(%o4) g[x](z)
(%i5) opsubst(tan, sin, sin(sin));
(%o5) tan(SIN)
(%i6) opsubst([f=g,g=h],f(x));
(%o6) h(x)

To determine the operator, 'opsubst' sets 'inflag' to true. This means
'opsubst' substitutes for the internal, not the displayed, operator.
Since Maxima does not internally use the unary negation or division
operators, substituting for these operators will not work; examples:

(%i1) opsubst("+","-",a-b);
(%o1) a-b
(%i2) opsubst("f","-",-a);
(%o2) -a
(%i3) opsubst("^^","//",a/b);
(%o3) a/b

The internal representation of -a*b is *(-1,a,b); thus

(%i4) opsubst("[","*", -a*b);
(%o4) [-1,a,b]

If opsubst did not locally set 'inflag' to true, we'd have:

(%i1) opsubst("[","*", -a*b), listarith : true;
(%o1) [-a,-b]
(%i2) opsubst("[","*", -a*b), listarith : false;
(%o2) -[a,b]

So opsubst("*","[", opsubst("[","*", -a*b)) # -a*b. There is
nothing wrong with this; however, With 'inflag' set to true, 
we have (regardless of the value of listarith)

(%i1) opsubst("[","*", -a*b);
(%o1) [-1,a,b]
(%i2) opsubst("*","[",%);
(%o2) -a*b

To me, it seems that it is better to substitute for the internal
rather than the displayed operator. But do not be mislead by this
example, the equation
 
   opsubst(f,g,opsubst(g,f,e)) = e

is not an identity.

When either the first or second arguments of 'opsubst' are not Maxima
symbols, generally some other function will signal an error; for
example

(%i5) opsubst(a+b,f, f(x));
Improper name or value in functional position:b+a

However, the first two arguments to 'opsubst' can be 
subscripted:

(%i6) opsubst(g[5],f, f(x));
(%o6) g[5](x)

|#

(defun $opsubst (&rest q)
  (let ((e))
    (cond ((= 3 (length q)) (apply 'op-subst q))
	  ((= 2 (length q))
	   (setq e (second q))
	   (setq q (if ($listp (first q)) (margs (first q)) (list (first q))))
	   (dolist (qi q e)
	     (if (op-equalp qi 'mequal) (setq e (op-subst ($rhs qi) ($lhs qi) e))
	       (merror "Expected an expression of the form `a = b'; instead found ~:M" qi))))
	  (t (wna-err '$opsubst)))))

(defun op-subst (f g e)
  (let (($inflag t))
    (if ($mapatom e) e
      (mapply1 (if (like ($verbify g) ($verbify ($op e))) f ($op e))
	       (mapcar #'(lambda (s) (op-subst f g s)) (margs ($args e))) nil))))

;; If prd(e) evaluates to true, do the substitution opsubst(id, e). The
;; first argument should be an equation of the form symbol = symbol or lambda form.

(defun $opsubstif (id prd e)
  (if (op-equalp id 'mequal) (op-subst-if ($rhs id) ($lhs id) prd e)))
  
(defun op-subst-if (fn fo prd e)
  (let (($inflag t) ($prederror nil) (q))
    (cond (($mapatom e) e)
	  (t
	   (mapply1 (if (and (like ($verbify fo) ($verbify ($op e)))
			     (eq t (mevalp (mfuncall '$apply prd ($args e))))) fn ($op e))
		    (mapcar #'(lambda (s) (op-subst-if fn fo prd s)) (margs ($args e))) nil)))))
	  	   	  
;; Return a list of all the arguments to the operator 'op.' Each argument is
;; a list (what 'args' would return).  Examples:

;; (%i1) gatherargs(f(x) + f(y),'f);
;; (%o1) [[x],[y]]

;; In the expression 42 + f(f(x)), both x and f(x) are arguments to f; thus

;; (%i2) gatherargs(42 + f(f(x)),'f);
;; (%o2) [[f(x)],[x]]

;; (%i3) gatherargs(f^2 + %pi,'f);
;; (%o3) []

	   
(defun $gatherargs (e op)
  `((mlist) ,@(gatherargs e ($verbify op))))

(defun gatherargs (e op)
  (if ($mapatom e) nil
    (append (if (op-equalp e op) `(((mlist) ,@(margs e))))
	    (mapcan #'(lambda (s) (gatherargs s op)) (margs e)))))
	 	  
(defun $gatherops (e)
  ($setify `((mlist) ,@(gatherops e))))

(defun gatherops (e)
  (if ($mapatom e) nil (cons ($op e) (mapcan #'gatherops (margs e)))))



  