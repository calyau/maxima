#|
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
'opsubst' substitutes for the internal, not the displayed, operator
in the expression. Internally, Maxima does not use the unary negation,
division, or the subtraction operators; thus:

(%i1) opsubst("+","-",a-b);
(%o1) a-b
(%i2) opsubst("f","-",-a);
(%o2) -a
(%i3) opsubst("^^","//",a/b);
(%o3) a/b

The internal representation of -a*b is *(-1,a,b); thus

(%i4) opsubst("[","*", -a*b);
(%o4) [-1,a,b]


When either operator isn't a Maxima symbol, generally some other function
will signal an error:

(%i5) opsubst(a+b,f, f(x));
Improper name or value in functional position:b+a

However, subscripted operators are allowed:

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
      (mfuncall '$apply (if (like g ($op e)) f ($op e))
		(cons '(mlist) (mapcar #'(lambda (s) (op-subst f g s)) (margs ($args e))))))))

