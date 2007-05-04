;; A positional derivative package for Maxima.
;; Author: Barton Willis (willisb@unk.edu)
;; July 2002
;; License: GPL
       
;; The functions mapply1 and sdiffgrad are (slightly) modified versions 
;; of code that is copyright 1982 by the Massachusetts Institute of Technology.
;; Additionally, William Schelter, University of Texas,  translated these 
;; functions to Common Lisp and enhanced them.  His work is copyright 1984 and 
;; 1987.

;; Richard J. Fateman wrote the tex-mexpt function; his work is copyright 1987.
;; The version of tex-mexpt in this file differs slightly from the version 
;; distributed with Maxima.  
 
;; Barton Willis, University of Nebraska at Kearney, wrote the other functions 
;; in this file.

;; The user of this code assumes all risk for its use. It has no warranty.
;; If you don't know the meaning of "no warranty," don't use this code.

(in-package :maxima)
($put 'pdiff "1.3" 'version)

;; When use_pdiff is true, use positional derivatives for unknown
;; non-subscripted functions. By unknown  function, I  mean a function 
;; that is not bound to a formula and that has a derivative that is not 
;; known to Maxima

(defmvar $use_pdiff t)

;; If f is the name of an user-defined function, return its arity;
;; otherwise, return nil.  (The arity of a function is the number of 
;; arguments it takes.)

;; Subscripted functions aren't members of $functions; for this and 
;; other reasons, we disallow subscripted functions from being 
;; positionally differentiated.  

;; Possibly I should disallow variable arity functions such as 
;; f([x]) := apply("+",x); 

(defun user-functionp (f)
  (let ((z (cdar (member f (cdr $functions) :key #'caar))))
    (if z (length z) nil)))

;; Return true if and only if n1 and each element of
;; n is a nonnegative integer.

(defun nonnegative-intp (n1 &rest n)
  ;;(print `(n1 = ,n1 n = ,n))
  (and (integerp n1) (> n1 -1)
       (every (lambda (x) (and (integerp x) (> x -1))) (first n))))
      
 ;; Return true if and only if n1 and each element of n is zero.

(defun zero-intp (n1 &rest n)
  (and (eq n1 0) (every (lambda (x) (eq 0 x)) (first n))))


;; pderivop(f,n1,n2,...np) returns the function whose formula is
;; diff(f(x1,x2,..xn),x1,n1,x2,n2,...). The pderivop function allows 
;; use to do things like
;;  
;;  (c1) tellsimpafter(pderivop(f,1)(a),1);
;;  (c2) tellsimpafter(pderivop(f,2)(a),1);
;;  (c3) diff(f(x),x,2) + diff(f(x),x);
;;
;;  (d3) 			       f   (x) + f   (x)
;;  				        (2)	  (1)
;;  (c4) subst(a,x,%);
;;  (d4)      2 
 
;; And
;;   (c5) g(x) := x^2;
;;   (c6) pderivop(g,1);
;;   (c7) ev(%);
;;   (d7) lambda([i54924], 2 i54924)
;;   (c8) apply(%,[z]);
;;
;;   (d8)     2 z

(defun $pderivop (f n1 &rest n)
  ;(print `(f = ,f n1 = ,n1 n = ,n))
  
  (setq n1 (ratdisrep n1))
  (setq n (mapcar #'ratdisrep n))
  (cond ((not (nonnegative-intp n1 n))
	 (merror "Each derivative order must be a nonnegative integer")))

  (cond ((and (consp f) (eq (caar f) '%pderivop))
	 (let ((k1 (- (length f) 2))  ;; k1 is the arity of f
	       (k2 (if (null n) 1 (+ 1 (length n)))))
	   ;(print `(k1 = ,k1 k2 = ,k2))
	   (cond ((eq k1 k2)
		  `(,(nth 0 f) ,(nth 1 f) ,@(mapcar #'+ (cddr f) (cons n1 n))))
		 (t
		  (merror "The function ~:M expected ~:M derivative argument(s), but it received ~:M" (nth 1 f) k1 k2)))))
	
	((zero-intp n1 n)
	 f)
	(t
	 ;;(print `(f = ,f n1 = ,n1 @n = ,@n))
	 (meval `((%pderivop) ,f ,n1 ,@n)))))


;; When f is the name of a user defined function or the name of a built-in 
;; function with a grad property, first evaluate the function using 
;; gensym arguments and second evaluate the derivatives. This function 
;; checks that the number of derivative indices matches the arity of the 
;; function.  When f is an unknown function, return  
;; ((%pderivop simp) ,f ,n1 ,@n))))).

;; It is possible that f is a constant function; here is one way that
;; this can happen

;;  (c1) diff(f(x),x);
;;  (c2) %,f=1;

;; To handle this case,  this function returns the zero function when 
;; ($constantp f) is true.  Maxima, however, disallows constants to be 
;; used in a functional position; thus 

;;  (c1) f(x),f=1

;; isn't allowed in Maxima.  When f is not a Maxima atom, signal an 
;; error.
  
(defun %pderivop (f n1 &rest n)
  ;(print `(f = ,f  n1 = ,n1 n = ,n))

  (cond ((or (eq f '%sqrt) (eq f '$exp)) ;; any others?
	 (let ((x (gensym)))
	   (setq f `((lambda simp) ((mlist) ,x) ((,f) ,x))))))
  	 
  (cond ((not (or ($atom f) ($constantp f) (and (consp f) (eq (caar f) 'lambda))))
	 (merror "Non-atom ~:M used as a function" f)))

  (let ((k) (f-arity nil) (gen-args) (e) (d))

    (cond ((and (symbolp f) (get f 'grad))
	   (setq f-arity (length (first (get f 'grad)))))

	  ((and (consp f) (eq (caar f) 'lambda))
	   (setq f-arity ($length (nth 1 f))))
	  
	  (t
	   (setq f-arity (user-functionp f))))

    (cond ((null f-arity)
	   (cond (($constantp f)
		  `((lambda simp) ((mlist) ((mlist) ,(gensym))) 0))
		 (t
		  `((%pderivop simp) ,f ,n1 ,@n))))
	  (t
	   (cond ((null n)
		  (setq k 1))
		 (t
		  (setq k (+ 1 (length n)))))
	   (cond ((not (eq f-arity k))
		  (merror "The function ~:M expected ~:M derivative argument(s), but it received ~:M" f f-arity  k)))
	   
	   (dotimes (i k)
	     (setq gen-args (cons (gensym) gen-args)))
	   
	   (setq e (meval `((,f) ,@gen-args)))
	   (setq d `(,n1 ,@n))
	   
	   (dotimes (i k)
	     (setq e ($diff e (nth i gen-args) (nth i d))))
	   `((lambda simp) ((mlist) ,@gen-args) ,e)))))	 
  
;; Modified mapply1 ---  added evaluation stuff for %pderivop 

(defmfun mapply1 (fn args fnname form)
  (declare (special aryp))
  ;;(print `(fn = ,fn args = ,args fnname =,fnname form = ,form))
  
  (cond		   ;((and $operators (mnump fn)) (mul2 fn (car args)))
    ((atom fn) 
     (cond ((atom fn) 
	    (cond ((functionp fn)
		   (apply fn args))
		  ((fboundp fn)
		   (if (macro-function fn)
		       (progn (merror "~:M is a lisp level macro and cannot be applied at maxima level" fn) (eval (cons fn  args)))
		       (mapply1 (symbol-function fn) args fn form)))
       
		  ((symbol-array fn)
		   (mapply1 (symbol-array fn) args fn form))
		  (t
		   (setq fn (getopr fn)) (badfunchk fnname fn nil)
		   (let ((noevalargs t)) (meval (cons (ncons fn) args)))))
	    )))

    ;;---------start pdiff stuff  -----------------------
    ((and $use_pdiff (eq (caar fn) '%pderivop))
					;(print `(fn = ,fn args = ,args fnname = ,fnname form = ,form))
     (cond ((eq (length (cddr fn)) (length args))
	    `((mqapply simp) ,fn ,@args))
	   (t
	    (merror "The function ~:M expected ~:M argument(s), but it received ~:M" (cadr fn) (length (cddr fn)) (length args)))))
	 
    ;;-------- end pdiff stuff --------------------------

    ((functionp fn)
     (apply fn args))
    ((eq (caar fn) 'lambda) (mlambda fn args fnname t form))
    ((eq (caar fn) 'mquote) (cons (cdr fn) args))
    ((and aryp (member (caar fn) '(mlist $matrix) :test #'eq))
     (if (not (or (= (length args) 1)
		  (and (eq (caar fn) '$matrix) (= (length args) 2))))
	 (merror "wrong number of indices:~%~:M" (cons '(mlist) args)))
     (do ((args1 args (cdr args1)))
	 ((null args1) (let (($piece $piece) ($partswitch 'mapply))
			 (apply #'$inpart (cons fn args))))
       (unless (fixnump (car args1))
	 (if evarrp (throw 'evarrp 'notexist))
	 (merror "subscript must be an integer:~%~:M" (car args1)))))
    (aryp (cons '(mqapply array) (cons fn args)))
    ((member 'array (cdar fn) :test #'eq) (cons '(mqapply) (cons fn args)))
    (t (badfunchk fnname fn t))))

(defun pderivop (f x n)
  `((mqapply) ((%pderivop) ,f ,@n) ,@x))

;; Return the list (k1,k2,k3,...,kn), where ki = 1 and all other k's are zero.

(defun i-list (i n)
  (incf-ith i (make-list n :initial-element 0)))

;; Increment the ith element of the list e by one.

(defun incf-ith (i e)
  (let ((k (nth i e))
	(q (copy-list e)))
    (setf (nth i q) (add 1 k))
    q))
	     
(defun sdiffgrad (e x)
  (let ((fun (caar e)) grad args)
    (cond ((and (eq fun 'mqapply) (oldget (caaadr e) 'grad))
	   (sdiffgrad (cons (cons (caaadr e) nil) (append (cdadr e) (cddr e))) x))

	  ;; ---- start pdiff stuff-----------------------------

	  ((and $use_pdiff (eq fun 'mqapply) (eq (caaadr e) '%pderivop))
	   (setq args (cddr e))
	   (setq fun (cadadr e))
	   (let ((de 0)
		 (n (length args))
		 (d-order (cddadr e)))
	     (dotimes (i n de)
	       (setq de (add de (mul ($diff (nth i args) x)
				     (pderivop fun args (incf-ith i d-order))))))))
	  
	  ;; We disallow positional derivatives of subscripted functions and
          ;; lambda forms.

	  ((and $use_pdiff (null (oldget fun 'grad)) (not ($subvarp (cadr e)))
		(not (eq fun 'lambda)))
	   ;(print `(args = ,(cdr e) fun = ,(caar e) e = ,e)) 
	   (setq args (cdr e))
	   (setq fun (caar e))
	   (let ((de 0)
		 (n (length args)))
	     (dotimes (i n de)
	       (setq de (add de (mul ($diff (nth i args) x)
				     (pderivop fun args (i-list i n))))))))

	  ;; --- end  pdiff stuff------------------------------------

	  ((or (eq fun 'mqapply) (null (setq grad (oldget fun 'grad))))
	   (if (not (depends e x)) 0 (diff%deriv (list e x 1))))

	  ((not (= (length (cdr e)) (length (car grad))))
	   (merror "wrong number of arguments for ~:M" fun))
	  
	  (t (setq args (sdiffmap (cdr e) x))
	     (addn (mapcar
		    #'mul2
		    (cdr (substitutel
			  (cdr e) (car grad)
			  (do ((l1 (cdr grad) (cdr l1))
			       (args args (cdr args)) (l2))
			      ((null l1) (cons '(mlist) (nreverse l2)))
			    (setq l2 (cons (cond ((equal (car args) 0) 0)
						 (t (car l1)))
					   l2)))))
		    args)
		   t)))))

;; Extend dimension to display positional derivatives. The order of the 
;; derivative is indicated by a subscript surrounded by parenthesis.

(setf (get '%pderivop 'dimension) 'dimension-pderiv)

(defun dimension-pderiv (form result)
  (setq form (cdr form))
  (setq form `(( ,(car form) simp array) (("") ,@(cdr form))))
  (dimension-array form result))

;; Extend tex to  handle positional derivatives.  Depending on the values of 
;; the option variables $tex_uses_prime_for_derivatives
;; and $tex_uses_named_subscripts_for_derivatives, derivatives can
;; tex as superscripted primes, subscripted variable names, or
;; parenthesis surrounded subscripts that indicate the derivative order. 

(defmvar $tex_uses_prime_for_derivatives nil)
(defmvar $tex_prime_limit 3)
(defmvar $tex_uses_named_subscripts_for_derivatives nil)
(defmvar $tex_diff_var_names (list '(mlist) '$x '$y '$z))

(setf (get '%pderivop 'tex) 'tex-pderivop)

;; Examples 

;; 1. $tex_uses_prime_for_derivatives is true, 

;;    (c1) tex(diff(f(x),x));
;;           $$f^{\prime}(x)$$
;;    (c2) tex(diff(f(x),x,2));
;;           $$f^{\prime\prime}(x)$$
;;    (c4) tex(diff(f(x),x,4));
;;           $$f^{(4)}(x)$$

;; In the last example, the derivative order exceeds $tex_prime_limit, 
;; so the derivative is indicated as shown in (c4).

;; 2. $tex_uses_named subscripts is true 
;;  (c1) tex_uses_prime_for_derivatives : false;
;;  (c2) tex(diff(f(x),x));
;;       $$f_{x}(x)$$
;;  (c3) tex(diff(f(x),x,2));
;;       $$f_{xx}(x)$$
;;  (c4) tex(diff(f(y),y,2));
;;       $$f_{xx}(y)$$

;; Although the function argument in (c4) is y, the derivative with
;; respect to the first argument is indicated by a subscript that is
;; the first element of tex_diff_var_names. A further example

;;  (c5) tex_diff_var_names : [\a,\b,\c]$
;;  (c6) tex(diff(f(x,y,z),x,1,y,1,z,1));
;;       $$f_{abc}(x,y,z)$$ 

;; When the derivative order exceeds tex_prime_limit, we don't use named
;; subscripts for derivatives; otherwise, we could get ridiculously long
;; subscripts.

;;   (c43) tex_prime_limit : 3;
;;   (c44) tex(diff(f(x,y),x,1,y,1));
;;         $$f_{xy}(x,y)$$
;;   (c45) tex(diff(f(x,y),x,1066,y,1776));
;;         $$f_{\left(1066,1776\right)}(x,y)$$

;; Finally, setting tex_uses_named subscripts and tex_uses_prime_for_derivatives
;; to false, derivatives are indicated with parenthesis surrounded 
;; subscripts.  There is one subscript for each function argument; when
;; the derivative order is zero, the subscript is zero.

;;  (c11) tex_uses_prime_for_derivatives : false;
;;  (c12) tex_uses_named_subscripts_for_derivatives : false;
;;  (c13) tex(diff(f(a,b),a,2,b,1));
;;            $$f_{\left(2,1\right)}(a,b)$$
;;  (c14) tex(diff(f(a,b),a,0,b,1));
;;            $$f_{\left(0,1\right)}(a,b)$$
;;  (c15) tex(diff(f(x,y),x,0,y,1));
;;            $$f_{\left(0,1\right)}(x,y)$$

(defun tex-pderivop (x l r)
  ;(print `(lop = ,lop rop = ,rop x = ,x r = ,r l = ,l))
  (cond ((and $tex_uses_prime_for_derivatives (eq 3 (length x)))
	 (let* ((n (car (last x)))
		(p))
	   
	   (cond ((<= n $tex_prime_limit)
		  (setq p (make-list n :initial-element "\\prime")))
		 (t
		  (setq p (list "(" n ")"))))

	   ;; We need to avoid double tex superscripts; when rop is mexpt, 
	   ;; use parens.

	   (cond ((eq rop 'mexpt)
		  (append l (list "\\left(") (tex (cadr x) nil nil lop rop) 
			  (list "^{") p (list "}") (list "\\right)") r))
		 (t
		  (append l  (tex (cadr x) nil nil lop rop) 
			  (list "^{") p (list "}")  r)))))
		  
	((and $tex_uses_named_subscripts_for_derivatives 
	      (< (apply #'+ (cddr x)) $tex_prime_limit))
	 (let ((n (cddr x))
	       (v (mapcar #'stripdollar (cdr $tex_diff_var_names)))
	       (p))
	   
	   (cond ((> (length n) (length v))
		  (merror "Not enough elements in tex_diff_var_names to tex the expression")))
	   (dotimes (i (length n))
	     (setq p (append p (make-list (nth i n) :initial-element (nth i v)))))
	   (append l (tex (cadr x) nil nil lop rop) (list "_{") p (list "}") r)))
	   
	(t
	 (append l (tex (cadr x) nil nil lop rop)  (list "_{") 
		 (tex-matchfix (cons '(mprogn) 
				     (cddr x)) nil nil) (list "}") r))))

;; To convert positional derivatives to standard derivatives, we may 
;; need dummy variables. I could use gensyms, but they are too lengthy.
;; The itensor package has a function that generates dummy variables; instead 
;; of loading itensor to access this function, we duplicate it here.

(defmvar $dummy_char '$%x)
(defmvar $dummy_index -1)

(defun $dummy_var ( )
  (intern (format nil "~a~d" $dummy_char (incf $dummy_index))))

;; Convert all positional derivatives in the expression e to "normal" 
;; Maxima derivatives.

(defun $convert_to_diff (e)
  (setq e ($totaldisrep e))
  (cond ((and (consp e) (eq (caar e) 'mqapply) (eq (caaadr e) '%pderivop))
	 (let* 
	     ((args (copy-list (cddr e)))
	      (fun (cadadr e))
	      (n (length args))
	      (d-order (cddadr e))
	      (iarg) (id) (at-list nil)
	      (weave nil))
	   (dotimes (i n)
	     (setq id (nth i d-order))
	     (cond ((not (eq id 0))
		    (setq iarg (nth i args))
		    (cond ((and ($atom iarg) (not ($constantp iarg)))
			   (setq iarg (nth i args)))
			  (t
			   (setq iarg ($dummy_var))
			   (setq at-list (cons `((mequal) 
						 ,iarg ,(nth i args)) at-list))
			   (setf (nth i args) iarg)))
		    (setq weave (cons iarg (cons (nth i d-order) weave))))))
	   
	   (setq e `((%derivative simp) ((,fun simp) ,@args) ,@weave))
	   (cond ((not (null at-list))
		  (setq at-list ($convert_to_diff (cons '(mlist) at-list)))
		  ($at e at-list))
		 (t
		  e))))
	(($mapatom e)
	 e)
	(t
	 (cons (nth 0 e) (mapcar #'$convert_to_diff (cdr ($args e)))))))

;; This is a (very) slightly modified tex-mexpt function -- %pderivop 
;; was added to the list of disallowed % functions.

(defun tex-mexpt (x l r)
  (let ((nc (eq (caar x) 'mncexpt)))	; true if a^^b rather than a^b
    ;; here is where we have to check for f(x)^b to be displayed
    ;; as f^b(x), as is the case for sin(x)^2 .
    ;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2.
    ;; yet we must not display (a+b)^2 as +^2(a,b)...
    ;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
    (cond ;; this whole clause
      ;; should be deleted if this hack is unwanted and/or the
      ;; time it takes is of concern.
      ;; it shouldn't be too expensive.
    
      ((and  (eq (caar x) 'mexpt)     ; don't do this hack for mncexpt
	     (let*
		 ((fx (cadr x))		; this is f(x)
		  (f (and (not (atom fx)) (atom (caar fx)) (caar fx))) ; this is f [or nil]
		  (bascdr (and f (cdr fx))) ; this is (x) [maybe (x,y..), or nil]
		  (expon (caddr x))	    ;; this is the exponent
		  (doit (and
			 f		; there is such a function
			 (member (getchar f 1) '(% $) :test #'eq) ;; insist it is a % or $ function
			 (not (member f '(%sum %product %derivative %integral %at %pderivop) :test #'eq)) ;; what else? what a hack...
			 (or (and (atom expon) (not (numberp expon))) ; f(x)^y is ok
			     (and (atom expon) (numberp expon) (> expon 0))))))
					; f(x)^3 is ok, but not f(x)^-1, which could
					; inverse of f, if written f^-1 x
					; what else? f(x)^(1/2) is sqrt(f(x)), ??

	  

	       (cond (doit
		     
		      (setq l (tex `((mexpt) ,f ,expon) l nil 'mparen 'mparen))
		      (setq r (tex
			       (if (and (null (cdr bascdr)) (eq (get f 'tex) 'tex-prefix))
				   (car bascdr) (cons '(mprogn) bascdr))
			       nil r f rop)))
		     (t nil)))))	; won't doit. fall through
      (t (setq l (tex (cadr x) l nil lop (caar x))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		     ;; the change in base-line makes parens unnecessary
		     (if nc
			 (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
			 (tex (cadr x) '("^ {- ")(cons " }" r) 'mparen 'mparen))
		     (if nc
			 (tex x (list "^{\\langle ") (cons "\\rangle}" r) 'mparen 'mparen)
			 (if (and (numberp x) (< x 10)) ;; was (< x 10)..  blw
			     (tex x (list "^")(cons "" r) 'mparen 'mparen)
			     (tex x (list "^{")(cons "}" r) 'mparen 'mparen)))))))
    (append l r)))
