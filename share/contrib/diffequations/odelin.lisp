;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$odelin 19 '$version)

;; [DB 2007-04-29] I have added some comments to generic-de-solver and 
;; subsidiary routines.  They are just my interpretation of the code.

;; The functions mtimesp and mexptp are either missing from 
;; commerical macsyma, or they have different names. For commerical
;; macsyma, here are the definitions.

#+kcl
(defun mtimesp (x)
  (and (consp x) (consp (car x)) (eq (caar x) 'mtimes)))

#+kcl
(defun mexptp (x)
  (and (consp x) (consp (car x)) (equal (caar x) 'mexpt)))

(eval-when
    #+gcl (load compile eval)
    #-gcl (:load-toplevel :compile-toplevel :execute)
    ($load "nset")
    ($load "polynomialp")
    ($load "sqfr")
    ($load "spherodialwave")
    ($load "kummer")
    ($load "extrabessel")
    ($load "lazysolver")
    ($load "gauss"))

(defmvar $de_solver_is_loquacious t)

(defun ode-polynomialp (p x)
  (setq p ($ratdisrep p))
  (or
   ($freeof x p)
   (like p x)
   (and (or (mtimesp p) (mplusp p)) 
	(every #'(lambda (s) (ode-polynomialp s x)) (margs p)))
   (and (mexptp p) (ode-polynomialp (car (margs p)) x) 
	(integerp (cadr (margs p)))
	(> (cadr (margs p)) 0))))
   
(defun $okay (de y x)
  (let ((okay t))
  (setq de (require-linear-homogeneous-de de y x))
  (dolist (p de)
    (setq okay (and okay (ode-polynomialp ($ratnumer p) x) 
		    (ode-polynomialp ($ratnumer p) x))))
  okay))

(defun $rerat (dirtyrat)
  (let ((varlist nil) (genvar nil))
  ($rat ($ratdisrep dirtyrat))))

;; Map the function f onto a mbag and simplify the result.  When the
;; bag is an equality, list, or matrix, simplification isn't needed;
;; however, new types of bags (say sets) may need simplification after 
;; the mapping.

;; Maxima is hit-or-miss about mapping functions over mbags. I suggest
;; we develop that a function similar to this one and that we use it
;; everywhere a function is mapped over a mbag. 

;; If the arguments of bag are in CRE form, margs changes them to general 
;; form---and mbag-map may return an expression in general form. Maybe this 
;; behavior is impolite?

(defun mbag-map (f bag)
  (simplify `((,(mop bag)) ,@(mapcar f (margs bag)))))
    
(defun require-linear-homogeneous-de (de y x)
  (setq y (require-symbol y "$odelin"))
  (setq x (require-symbol x "$odelin"))

  (cond ((or ($listp de) ($setp de))
	 (if (= 1 ($cardinality de)) 
	     (setq de (nth 1 de))
	   (merror "'odelin' doesn't handle systems of DEs.~%"))))
  
  (setq de ($ratdisrep de))
  (setq de (meqhk de))
  (let ((cf) (acc) (f de) (n ($derivdegree de y x)))
    (if (not (and (integerp n) (> n 0)))
	(merror "'odelin' doesn't handle order ~:M DEs.~%" n))
    (setq de ($rat de))
    (while (> n -1)
      (if (= n 0)
	  (setq cf ($ratcoef de y 1))
	(setq cf ($ratcoef de (list '(%derivative) y x n))))
      (if  ($freeof y cf)
	  (push cf acc)
	(merror "DE must be linear"))
      (setq f (sub f (mul cf (list '(%derivative) y x n))))
      (decf n))
    (setq f ($ratsimp f))
    (if (not (like 0 f))
	(merror "DE must be linear and homogeneous.~%"))
    acc))
  	           
(defun $odelin (de y x)
  (let ((cfs (require-linear-homogeneous-de de y x)) (n))
    (setq n (length cfs))
    (cond ((= n 2) (odelin-order-one cfs x))
	  ((= n 3) (odelin-order-two cfs x))
	  (t (merror "'odelin' doesn't handle DEs with order ~:M" (- n 1))))))

(defun odelin-order-one (cfs x)
  (let ((p1 (nth 1 cfs))
	(p0 (nth 0 cfs)))
    (fss-cleanup `(($set) ,(power '$%e ($integrate (div p0 p1) x))) x)))

(defun expunge-const-factors (e x)
  (let ((acc 1))
    (cond ((like 0 e) 0)
	  (t
	   (setq e ($factor e))
	   (setq e (if (mtimesp e) (margs e) (list e)))
	   (dolist (ei e acc)
	     (if (not ($freeof x ei)) (setq acc (mul ei acc))))))))

;; Cleanup a fundamental solution set (FSS). 
  
(defun fss-cleanup (fss x)
  (let ((nfss) (k 0) ($radexpand nil) ($ratsimpexpons t))
    (while (and (< k 5) (not (like fss (setq nfss (mbag-map #'$radcan fss)))))
      (incf k)
      (setq fss nfss))
    (mbag-map #'(lambda (s) (expunge-const-factors s x)) fss)))

(defun post-check-cleanup (fss)
  (setq fss ($substitute '%bessel_j '$fbessel_j fss))
  ($substitute '%bessel_y '$fbessel_y fss))
        
(defun odelin-order-two (cfs x)
  (let ((p0 (nth 0 cfs))
	(p1 (nth 1 cfs))
	(p2 (nth 2 cfs))
	(p) (m) (sol)
	(ode-methods (list 
		      'ode-solve-by-factoring
		      'bessel-de-solver
		      'hypergeo01-de-solver
		      'spherodialwave-de-solver
		      'bessel-sqrt-de-solver
		      'hypergeo21-de-solver))
	($radexpand nil))
    
    (setq p1 (div p1 p2))
    (setq p0 (div p0 p2))

    (if (not (and (ode-polynomialp ($ratnumer p1) x)
		  (ode-polynomialp ($ratdenom p1) x)
		  (ode-polynomialp ($ratnumer p0) x)
		  (ode-polynomialp ($ratdenom p0) x)))
	(setq ode-methods nil))
      
    (setq p (add p0 (div ($diff p1 x) -2) (mul p1 p1 (div -1 4))))
    (setq p ($rat p))
    (setq m (power '$%e (simplify ($integrate (div p1 -2) x))))
       
    (dolist (method ode-methods)
      (setq sol (funcall method p x))
      (if ($setp sol) (setq sol (fss-cleanup 
				 (mbag-map #'(lambda (s) (mul m s)) sol) x)))
      (if (check-fss cfs sol x) (return (post-check-cleanup sol))))))
   
(defprop unk simp-unk operators)

(defun simp-unk (x y z)
  (declare (ignore y z))
  (merror "Maxima doesn't know the derivative of ~:M with respect the ~:M argument" (nth 2 x) (nth 1 x)))
	  		 
(defun check-de-sol (cfs sol x)
  (let ((zip 0) ($gcd '$spmod) ($algebraic t) ($ratsimpexpons t))
    (dolist (cf cfs)
      (setq zip (add zip (mul cf sol)))
      (setq sol ($diff sol x)))
    (setq zip ($ratsimp zip))
    (or
     (like 0 zip) (like 0 ($radcan zip)) (like 0 ($radcan ($expand zip)))
     (mtell "should vanish, but it does not ~:M~%" zip))))
    
(defun check-fss (cfs fss x)
  (let (($gcd '$spmod) ($algebraic t))
    (and 
     ($setp fss)
     (= (length cfs) (length fss))
     (every #'(lambda (s) (check-de-sol cfs s x)) (cdr fss))
     (not (like 0 ($xwronskian fss x))))))

(defun check-number-of-args (n arg f)
  (if (not (= n (length arg)))
      (merror "Function ~:M requires ~:M arguments; found ~:M arguments"
	      f n (+ n (length arg)))))
  
;; Return the wronskian of the list or set of expressions s. 

(defun $xwronskian (&rest arg)
  (check-number-of-args 2 arg "$xwronskian")
  (let ((mat) (dim)
	(s (require-list-or-set (nth 0 arg) "$xwronskian"))
	(x (require-symbol (nth 1 arg) "$xwronskian")))
    (setq s `((mlist) ,@s))
    (setq x (require-symbol (nth 1 arg) "$xwronskian"))
    (setq dim ($length s))
    (dotimes (i dim)
      (push s mat)
      (setq s ($diff s x)))
    ($radcan ($determinant `(($matrix) ,@mat)))))

;; Return a polynomial in x with degree deg and a list of its
;; coefficients. Each coefficient of the polynomial is a gensym.
   	    
(defun make-unk-poly (x deg)
  (setq x ($ratdisrep x))
  (let ((p 0) (z 1) (cf) (cfs))
    (incf deg)
    (dotimes (i deg (values p (reverse cfs)))
      (setq cf (gensym))
      (push cf cfs)
      (setq p (add p (mul cf z)))
      (setq z (mul z x)))))

(defun polycfs-to-eqs (p x)
  (let ((n) (eqs) ($gcd '$spmod) ($algebraic t) ($ratfac nil) ($ratprint nil))
    (setq p ($rat p x))
    (setq n (+ 1 ($hipow p x)))
    (dotimes (i n eqs)
      (push ($ratcoef p x i) eqs))))
    
;; Solve y'' - r y by factoring the differential operator (D^2 - r). Specifically,
;; we factor D^2 - r as (D+a)(D-a), where r and a are rational functions.
;; Expanding, we have  D^2 - r = D^2 -a' - a^2.  So -r = -a' - a^2 or
;; a' + a^2 = r. Solving a' + a^2 =  r is the task for factor-differential-op. 
;; Solutions to (D+a)(D-a) f = 0 are f = mu, where mu = exp (integrate(a,x)) 
;; and f = mu * integrate(1/mu^2,x).

(defun ode-solve-by-factoring (r x)
  (if $de_solver_is_loquacious (mtell "...trying factor method~%"))
  (let ((a) (mu) ($radexpand nil))
    (cond ((setq a (factor-differential-op r x))
	   (setq a ($radcan a)) ;; x*(x+1)*'DIFF(y,x,2)+(3*x+2)*'DIFF(y,x,1)+y
	   (setq mu ($radcan ($exp ($integrate a x))))
	   `(($set) ,mu ,(mul mu ($integrate (div 1 (mul mu mu)) x))))
	  (t nil))))
	   
;; Let r = p/q be rational. We either find a rational function a such that 
;; a' + a^2 =  p/q = p q / q^2, or we return nil.  We do this by setting
;; a = w / q.  Then q w' - q' w + w^2 = pq.

(defun factor-differential-op (r x)
  (setq r ($rat r x))
  (let*
      ((p ($ratnumer r))
       (q ($ratdenom r))
       (m ($hipow q x))
       (l ($hipow p x))
       (w) (zip) (n) (vars) (sol)
       ($gcd '$spmod) ($algebraic t) ($ratfac nil))
  
    (setq m ($totaldisrep m))
    (setq l ($totaldisrep l))
    (setq m (coerce m 'fixnum))
    (setq l (coerce l 'fixnum))
    (cond ((< (+ 2 l) m)
	   (setq n (max (+ m -1) (+ l 1))))
	  ((evenp (+ m l))
	   (setq n (/ (+ m l) 2)))
	  (t
	   (setq n -1)))
    (cond ((> n -1)
	   (multiple-value-setq  (w vars) (make-unk-poly x n))
	   (setq vars (reverse vars))
	   (push '(mlist) vars)
	   (setq zip (add
		      (mul q ($diff w x)) 
		      (mul -1 w ($diff q x)) 
		      (mul w w) 
		      (mul  p q)))
	   (setq zip ($rat zip x))
	   (setq zip (polycfs-to-eqs zip x))
	   (push '(mlist) zip)
	   (setq sol ($checkedalgsys zip vars))
	
	   (cond ((not sol)
		  (setq sol nil))
		 (t
		  (setq w (div w q))
		  ($substitute sol w))))
	  (t
	   nil))))

(defun polynomial-filter (p x f)
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil) 
	($ratprint nil) ($radexpand nil))
    (setq p ($ratsimp p x)) ;; Get rid of terms like sqr(5)^2, %i^2...
    (setq p ($mysqfr p x))
    (setq p (if (mtimesp p) (margs p) (list p)))
    (let ((q 1) (n))
      (dolist (pj p q)
	(cond ((mexptp pj)
	       (setq n (nth 2 pj))
	       (setq pj (nth 1 pj)))
	      (($freeof x pj)
	       (setq n 0))
	      (t
	       (setq n 1)))
	(setq n (funcall f n))
	(setq q (mult q (power pj n)))))))
  
(defun ratfun-degree (q x)
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil))
    (setq q ($rat q x))
    (- ($hipow ($ratnumer q) x) ($hipow ($ratdenom q) x))))

;; Called by generic-de-solver implementing method of Bronstein and Lafaille.
;; Returns the differential equation satisfied by xi(x) [Eq 7 in paper] 
;;
;;       2                       2                           4         2
;; 3*xi'' - 2*xi'*xi''' + (a1(xi) + 2*a1'(xi) - 4*a0(xi))*xi' - 4*v*xi' = 0
;;
;; In this routine
;;    vh is the expression v(x) in DE to be solved (D^2-v)y=0
;;    v  is the expression -(a1(x)^2 + 2*a1'(x) - 4*a0(x))/4
;;    We return (-1) times equation above
;;
(defun get-de-cnd (xi vh v x)
  (let ((dxi) (ddxi) (dddxi) (dxi^2) ($gcd '$spmod) ($algebraic t) 
	($ratfac nil) ($ratprint nil))
    
    (setq xi ($ratdisrep xi))
    (setq dxi ($diff xi x))
    (setq dxi^2 (mul dxi dxi))
    (setq ddxi ($diff dxi x))
    (setq dddxi ($diff ddxi x))
    
    ;; 4*dxi^4*v(xi)-4*dxi^2*vh(x)+2*dddxi*dxi-3*ddxi^2
   
    ($ratdisrep ($ratnumer
		 (add
		  (mul 4 ($substitute xi x v) dxi^2 dxi^2)
		  (mul -4 vh dxi^2)
		  (mul 2 dxi dddxi)
		  (mul -3 ddxi ddxi))))))

(defun xeasy-eqs (p s x)
  (let ((acc `(($set))))
    (setq s (polynomial-filter s x #'(lambda (n) (min 1 n))))
    (setq s (cdr ($solve s x)))
    (dolist (si s (require-set acc "easy-eqs"))
      (setq acc ($adjoin ($substitute si p) acc)))))

;; Find conditions that make the polynomial 'cnd' vanish at each zero of
;; the polynomial 's.'  When the zero of 's' isn't messy, we simply 
;; evaluate 'cnd' at the zero.  When the zero of 's' is messy, we 
;; use a different method. In the best of all possible worlds, I'd
;; treat all zeros as messy---when I do this, some DEs in my testing
;; routine don't get solved. 

(defun easy-eqs (cnd s x)
  (let ((acc) (n) ($gcd '$spmod) ($algebraic t) 
	($solve_inconsistent_error nil) ($programmode t) 
	($globalsolve nil) ($solveexplicit t) ($solveradcan nil))
    
    (setq s (polynomial-filter s x #'(lambda (n) (min 1 n))))
    (setq s ($factor s))
    (setq s (if (mtimesp s) (margs s) (list s)))
    (dolist (si s acc)
      (setq si ($expand si))
      (setq n ($hipow si x))
      ;; Check for a non-messy zero.
      (cond ((< n 3)
	     (setq si (margs ($solve si x)))
	     (dolist (sij si)
	       (push ($substitute sij cnd) acc)))
	    (t
	     (setq acc (append acc 
			       (polycfs-to-eqs 
				($second ($divide cnd si x)) x))))))))

      
;; Expunge multiple zeros.

(defun clean-equation (p)
  (let ((acc 1))
    (setq p ($sqfr p))
    (setq p (cond ((mtimesp p) (margs p))
		  ((mexptp p) (list (car (margs p))))
		  (t (list p))))
    (dolist (pj p acc)
      (setq acc (mul acc (if (mexptp pj) (car (margs pj)) pj))))))   
	           
(defun number-of-unks (p unks)
  (let (($listconstvars nil))
    ($cardinality ($intersection ($setify ($listofvars p)) unks))))

;; generic-de-solver uses a method described by Bronstein and Lafaille, 
;; to compute special function solutions of non-Liouvillian 2nd order 
;; linear homogeneous ODES.
;;
;; M Bronstein, S Lafaille, Solutions of linear ordinary differential 
;; equations in terms of special functions, Proceedings of ISSAC 2002, Lille, 
;; ACM Press, 23-28. 
;; (http://www-sop.inria.fr/cafe/Manuel.Bronstein/publications/issac2002.pdf)
;;
;; Given 
;; - an ode y'' = v y, where v is a function of x, 
;; - a target 2nd order linear ODE L y = 0, with
;;    - L = D^2 + a1 D + a0 a 2nd order linear differential operator
;;    - {F1,F2} a known fundamental solution set of L y = 0
;; we seek functions m(x) and xi(x) st { m(x) F1(xi(x)), m(x) F2{xi(x)) }
;; is a fundamental solution set of y'' = v y.
;;
;; For each target equation L = D^2 + a1 D + a0 we specialize the solver
;; by providing
;; o  params: a list of additional parameters in the operator
;; o  denom-filter
;; o  degree-bound
;; o  de-cnd: -Delta/4, where Delta = (a1)^2  + 2 a1' - 4 a0
;;
(defun generic-de-solver (v x params denom-filter degree-bound de-cnd)
  (setq v ($rat v x))
  (let ((s) (q) (p) (n) (unks) (nz) (eqs) (cnd) (sol) (xeqs)
	($gcd '$spmod) ($algebraic t) ($ratfac nil))

    (setq s ($ratdenom v))
    (setq q (polynomial-filter s x denom-filter))
    (setq n (funcall degree-bound q v x))
    (multiple-value-setq (p unks) (make-unk-poly x n))
    (setq nz `((mlist) ,p ,(sub (mul p ($diff q x)) (mul q ($diff p x)))))
    (setq unks (append params unks))
    (push `(mlist) unks)
    (setq cnd (get-de-cnd (div p q) v de-cnd x))
    (setq xeqs (easy-eqs cnd s x))
    (setq eqs (polycfs-to-eqs cnd x))
    (setq unks ($listify unks))
    (setq eqs (mapcar #'clean-equation eqs))
    (setq xeqs (mapcar #'clean-equation xeqs))
    (push `(mlist) eqs)
    (push `(mlist) xeqs)
    (setq sol ($aalgsys xeqs eqs unks nz))
    ;;(setq sol ($checkedalgsys ($append xeqs eqs) unks nz))
    
    (cond (($listp sol)
	   (setq p ($substitute sol p))
	   (setq params (mapcar #'(lambda (s) ($substitute sol s)) params))
	   `(,(div p q) ,params))
	  (t nil))))

;; The differential operator with the FSS {bessel_j(mu,x),bessel_y(mu,x)} 
;; is L = D^2 + a1*D + a0, with a1 = 1/x and a0 = (1-mu^2/x^2).
;; 
;; Delta = (a1)^2 + 2*a1' - 4*a0  = (4*mu^2 - 1)/x^2 - 4
;; de-cnd = -Delta/4 = (4*x^2+1-4*mu^2)/(4*x^2)
;;
;; Additional parameter [mu]
;;
(defun bessel-xi-denom-filter (n)
  (if (and (evenp n) (> n 3)) (/ (- n 2) 2) 0))

(defun bessel-xi-degree-bound (q v x)
  (ceiling (+ 1 ($hipow q x) (/ (max (ratfun-degree v x) -2) 2))))

(defun bessel-de-solver (v x)
  (if $de_solver_is_loquacious
      (mtell "...trying the Bessel solver~%"))

  (let* 
      ((mu (gensym)) (z) (m) ($radexpand nil)
       (xi (generic-de-solver v x 
			      (list mu)
			      'bessel-xi-denom-filter
			      'bessel-xi-degree-bound
			      (div (add (mul 4 x x) 1 (mul -4 mu mu))
				   (mul 4 x x)))))
    (cond (xi
	   (setq z (nth 0 xi))
	   (setq mu (car (nth 1 xi)))
	   (setq z ($ratsimp z))
	   (setq mu ($ratsimp mu))
	   (setq m (mul 
		    (power z (div 1 2)) 
		    (power ($diff z x) (div -1 2))))
	   
	   (mbag-map #'(lambda (s) (mul m s))
		     `(($set) 
		       (($fbessel_j) ,mu ,z)
		       (($fbessel_y) ,mu ,z)))))))

;; The differential operator with the FSS 
;;   {bessel_j(mu,sqrt(x)),bessel_y(mu,sqrt(x))} 
;; is L = D^2 + a1 D + a0, with a1 = 1/x and a0 = (1/x-mu^2/x^2)/4.
;; 
;; Delta = (a1)^2  + 2 a1' - 4 a0
;;       = (mu^2-1)/x^2 - 1/x
;;
;; de-cnd = -Delta/4 = (1 + x - mu^2)/(4*x^2)
;;
;; Additional parameter [mu]
;;       
(defun bessel-sqrt-xi-denom-filter (n)
  (if (> n 2) (- n 2) 0))

(defun bessel-sqrt-xi-degree-bound (q v x)
  (+ 2 ($hipow q x) (max -2 (ratfun-degree v x))))

(defun bessel-sqrt-de-solver (v x)
  (if $de_solver_is_loquacious
      (mtell "...trying the square root Bessel solver~%"))

  (let*  ((mu (gensym)) (z) (m) ($radexpand nil)
	  (xi (generic-de-solver v x 
				 (list mu)
				 'bessel-sqrt-xi-denom-filter
				 'bessel-sqrt-xi-degree-bound
				 (div (add 1 x (mul -1 mu mu)) (mul 4 x x)))))
    (cond (xi
	   (setq z (nth 0 xi))
	   (setq mu (car (nth 1 xi)))
	   (setq z ($ratsimp z))
	   (setq mu ($ratsimp mu))
	   (setq m (mul 
		    (power z (div 1 2)) 
		    (power ($diff z x) (div -1 2))))
	   
	   (setq z (power z (div 1 2)))
	   (mbag-map #'(lambda (s) (mul m s))
		     `(($set) 
		       (($fbessel_j) ,mu ,z)
		       (($fbessel_y) ,mu ,z)))))))

;; The differential operator with the FSS {kummer_m(a,b,x),kummer_u(a,b,x)} 
;; is L = D^2 + a1*D + a0, with a1 = b/x - 1 and a0 = -a/x.
;;  
;; Delta = (a1)^2  + 2 a1' - 4 a0  
;;       = 1 + (4 a - 2 b)/x + (b^2 - 2 b)/x^2
;;       = (x^2 + (4 a - 2 b) x + b^2 -2 b)/x^2
;;
;; de-cnd = -Delta/4 = (-x^2+(2*b-4*a)*x-b^2+2*b)/(4*x^2)
;;
;; Additional parameters to be determined are [a,b]
;;
(defun hypergeo01-xi-denom-filter (n)
  (if (and (evenp n) (> n 3)) (/ (- n 2) 2) 0))

(defun hypergeo01-xi-degree-bound (q v x)
  (ceiling (+ 1 ($hipow q x) (/ (max (ratfun-degree v x) -2) 2))))

(defun hypergeo01-de-solver (v x)
  (if $de_solver_is_loquacious (mtell "...trying the F01 solver~%"))
  (let* ((a (gensym)) (b (gensym)) (m) (z) ($radexpand nil)
	 (xi (generic-de-solver v x 
				(list a b)
				'hypergeo01-xi-denom-filter
				'hypergeo01-xi-degree-bound
				;; (-x^2+(2*b-4*a)*x-b^2+2*b)/(4*x^2)
				(div
				 (add 
				  (mul x x)
				  (mul (sub (mul 4 a) (mul 2 b)) x)
				  (mul b b)
				  (mul -2 b))
				 (mul -4 x x)))))
    (cond (xi
	   (setq z (car xi))
	   (setq a (caadr xi))
	   (setq b (cadadr xi))
	   (setq z ($ratsimp z))
	   (setq m (mul 
		    (power '$%e (div z -2)) 
		    (power z (div b 2))
		    (power ($diff z x) (div -1 2))))

	   (mbag-map #'(lambda (s) (mul s m))
		     `(($set) 
		       (($kummer_m) ,a ,b ,z)
		       (($kummer_u) ,a ,b ,z)))))))

;; The differential operator with the FSS 
;; {spherodialwave_a(b,c,q,x),spherodialwave_b(b,c,q,x)} 
;; is L = D^2 + a1*D + a0, with a1 = -2*(b+1)*x/(1-x^2) 
;; and a0 = (c-4*q*x^2)/(1-x^2)
;;  
;; Delta = (a1)^2  + 2 a1' - 4 a0
;;       = 4*(-q*x^4 + (4q+b^2+b+c)*x^2 - (b+c+1)) / (1-x^2)^2
;;
;; de-cnd = -Delta/4 = (q*x^4 - (4q+b^2+b+c)*x^2 + (b+c+1)) / (1-x^2)^2
;;
;; Additional parameters to be determined are [b,c,q]
;;
;; FIXME: [DB 2007-04-29] Need to clarify definition of the spherodial
;; wave functions as I can't find a reference that exactly matches.
;; The values of a1 and a0 above:
;; - give an ODE that is satisfied by spherodialwave_a and spherodialwave_b, 
;; - value of de-cnd derived from them matches the one in the code below.
;;
(defun spherodialwave-xi-denom-filter (n)
  (if (and (evenp n) (> n 3)) (/ (- n 2) 2) 0))

(defun spherodialwave-xi-degree-bound (q v x)
  (ceiling (+ 1 ($hipow q x) (/ (max (ratfun-degree v x) -2) 2))))

(defun spherodialwave-de-solver (v x)
  (if $de_solver_is_loquacious (mtell "...trying the spherodial wave solver~%"))
  (let* ((b (gensym)) (c (gensym)) (q (gensym)) (m) (z) (x2 (mul x x))
	 ($radexpand nil)
	 (xi (generic-de-solver v x 
				(list b c q)
				'spherodialwave-xi-denom-filter
				'spherodialwave-xi-degree-bound
				(mul
				 (power (add 1 (mul (add -2 x2) x2)) -1)
				 (add 1 b c
				      (mul x2 (add (mul 
						     (add -1 (mul -1 b)) b)
						   (mul -1 c) (mul -4 q)
						   (mul 4 q x2))))))))

    ;; (x2*(4*q*x2-4*q-c+(-b-1)*b)+c+b+1)/((x2-2)*x2+1), x2 = x^2.
    
    (cond (xi
	   (setq z (nth 0 xi))
	   (setq b (nth 0 (nth 1 xi)))
	   (setq c (nth 1 (nth 1 xi)))
	   (setq q (nth 2 (nth 1 xi)))
	   (setq z ($ratsimp z))
	   (setq m (mul 
		    (power (sub (mul z z) 1) (div (add b 1) 2))
		    (power ($diff z x) (div -1 2))))

	   (mbag-map #'(lambda (s) (mul m s))
		     `(($set) 
		       (($spherodialwave_a) ,b ,c ,q ,z)
		       (($spherodialwave_b) ,b ,c ,q ,z)))))))

;; The differential operator for the hypergeometric differential equation 
;; (A&S 15.5.1) with the FSS {gauss_a(a,b,c,x),gauss_b(a,b,c,x)} is
;; L = D^2 + a1 D + a0, with a1=[c-(a+b+1)x]/[x(1-x)] and a0=-ab/[x(1-x)].
;;  
;; Delta = (a1)^2  + 2 a1' - 4 a0  
;;       = ... 
;;
;; Additional parameters to be determined are [a,b,c]
;;

(defun hypergeo21-xi-denom-filter (n)
  (declare (ignore n))
  0)

(defun hypergeo21-xi-degree-bound (q v x)
  (declare (ignore q v x))
  1)


(defun hypergeo21-de-solver (v x)
  (if $de_solver_is_loquacious (mtell "...trying the 2F1 solver~%"))
  (let* ((a (gensym)) (b (gensym)) (c (gensym)) (m) (z) ($radexpand nil)
	 (xi (generic-de-solver v x 
				(list a b c)
				'hypergeo21-xi-denom-filter
				'hypergeo21-xi-degree-bound

				(div
				 (add
				  (mul x
					 (add
					  (mul
					   (add (mul (add (mul 2 a) 
							  (*mminus b)) b)
						(*mminus (power a 2))
						1)
					   x)
					  (mul (add (mul 2 b) (mul 2 a) -2) c)
					  (mul -4 a b)))
				  (mul (add 2 (*mminus c)) c))
				 (mul (power x 2)
				      (add (mul x (add (mul 4 x) -8)) 4))))))
			
    
    (cond (xi
	   (setq z (nth 0 xi))
	   (setq a (nth 0 (nth 1 xi)))
	   (setq b (nth 1 (nth 1 xi)))
	   (setq c (nth 2 (nth 1 xi)))
	   (setq z ($ratsimp z))
;; (xi(x))^(c/2) * (xi(x)-1)^((-c+b+a+1)/2) * f(xi(x)) / sqrt(diff(xi(x),x))
	   (setq m (mul 
		    (power z (div c 2)) (power (sub z 1) 
					       (div (add 1 a b (mul -1 c)) 2))
		    (power ($diff z x) (div -1 2))))

	   (mbag-map #'(lambda (s) (mul m s))
		     `(($set) 
		       (($gauss_a) ,a ,b ,c ,z)
		       (($gauss_b) ,a ,b ,c ,z)))))))

