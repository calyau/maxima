;;;  -*- LISP -*-
;;;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(in-package :maxima)

(macsyma-module hyp)

;;(eval-when
;;    #+gcl (compile eval)
;;    #-gcl (:compile-toplevel :execute)
;;    (declare-top (special fun w b l alglist $true $false n  c l1 l2)))

(declare-top (special $true $false))

(declare-top (special var *par*
		      checkcoefsignlist
		      $exponentialize $bestriglim $radexpand))


;; (eval-when (compile eval) (load '((dsk ell) macros >)) )

;; Why is this needed?
(setq checkcoefsignlist nil)

;; I (rtoy) don't know what the default should be. but $hgfred sets it
;; to 3.  But we also need to define it because some of the specint
;; demos need it set.
;;
(defmvar $bestriglim 3)

(defmvar $prefer_whittaker nil)

(eval-when
    #+gcl (eval compile)
    #-gcl (:execute :compile-toplevel)
    (defmacro fixp (x) `(typep ,x 'fixnum))

    (setq checkcoefsignlist '())
    ;;      $BESTRIGLIM 3. $RADEXPAND '$ALL

    (defmacro simp (x) `(simplifya ,x ()))

    (defmacro simp-list (l) `(mapcar #'(lambda (x) (simp x)) ,l))

    ;; The macro MABS has been renamed to HYP-MABS in order to
    ;; avoid conflict with the Maxima symbol MABS. The other
    ;; M* macros defined here should probably be similarly renamed
    ;; for consistency. jfa 03/27/2002

    (defmacro hyp-mabs (x) `(simp `((mabs) ,,x)))

    (defmacro msqrt (x) `(m^t ,x 1//2))

    (defmacro mexpt (x) `(m^t '$%e ,x))

    (defmacro mlog (x) `(simp `((%log) ,,x)))

    (defmacro msin (x) `(simp `((%sin) ,,x)))

    (defmacro mcos (x) `(simp `((%cos) ,,x)))

    (defmacro masin (x) `(simp `((%asin) ,,x)))

    (defmacro matan (x) `(simp `((%atan) ,,x)))

    (defmacro mgamma (x) `(simp `((%gamma) ,,x)))

    (defmacro mbinom (x y) `(simp `((%binomial) ,,x ,,y)))

    (defmacro merf (x) `(simp `((%erf) ,,x)))

    (defmacro =1//2 (x) `(alike1 ,x 1//2))

    (defmacro =3//2 (x) `(alike1 ,x 3//2))

    (defmacro =-1//2 (x) `(alike1 ,x -1//2))
    )

(defun hyp-integerp (x)
  ;; In this file, maxima-integerp was used in many places.  But it
  ;; seems that this code expects maxima-integerp to return T when it
  ;; is really an integer, not something that was declared an integer.
  ;; But I'm not really sure if this is true everywhere, but it is
  ;; true in some places.
  ;;
  ;; Thus, we replace all calls to maxima-integerp with hyp-integerp,
  ;; which, for now, returns T only when the arg is an integer.
  ;; Should we do something more?
  (and (maxima-integerp x) (integerp x)))

;; Main entry point for simplification of hypergeometric functions.
;;
;; F(a1,a2,a3,...;b1,b2,b3;z)
;;
;; L1 is a (maxima) list of an's, L2 is a (maxima) list of bn's.
(defun $hgfred (arg-l1 arg-l2 arg)
  ;; Do we really want $radexpand set to '$all?  This is probably a
  ;; bad idea in general, but we'll leave this in for now until we can
  ;; verify find all of the code that does or does not need this and
  ;; until we can verify all of the test cases are correct.
  (let (;;($radexpand '$all)
	(var arg)
	(*par* arg))
    (hgfsimp-exec (cdr arg-l1) (cdr arg-l2) arg)))


(defun hgfsimp-exec (arg-l1 arg-l2 arg)
  (let* ((l1 (copy-tree arg-l1))
	 (l2 (copy-tree arg-l2))
	 ($exponentialize nil)
	 (res (hgfsimp l1 l2 arg)))
    (if (or (numberp res) (not (atom res)))
	res
	(fpqform l1 l2 arg))))

(defun hgfsimp (arg-l1 arg-l2 var)
  (prog (resimp listcmdiff)
     (setq arg-l1 (macsimp arg-l1)
	   arg-l2 (macsimp arg-l2)
	   resimp (simpg arg-l1 arg-l2))
	    
     (cond ((not (eq (and (consp resimp) (car resimp))
		     'fail))
	    (return resimp)))
     (cond ((setq listcmdiff
		  (intdiffl1l2 (cadr resimp)
			       (caddr resimp)))
	    (return (splitpfq listcmdiff
			      (cadr resimp)
			      (caddr resimp)))))
     (return (dispatch-spec-simp (cadr resimp)
				 (caddr resimp)))))


(defun macsimp (exp)
  (mapcar #'(lambda (index)
	      (simplifya ($expand index) nil))
	  exp))

;; Simplify the parameters.  If L1 and L2 have common elements, remove
;; them from both L1 and L2.
(defun simpg (arg-l1 arg-l2)
  (let ((il (zl-intersection arg-l1 arg-l2)))
    (cond ((null il)
	   (simpg-exec arg-l1 arg-l2))
	  (t
	   (simpg-exec (del il arg-l1)
		       (del il arg-l2))))))

(defun del (a b)
  (cond ((null a) b)
	(t
	 (del (cdr a) (zl-delete (car a) b 1)))))

;; Handle the simple cases where the result is either a polynomial, or
;; is undefined because we divide by zero.
(defun simpg-exec (arg-l1 arg-l2)
  (let (n)
    (cond ((zerop-in-l arg-l1)
	   ;; A zero in the first index means the series terminates
	   ;; after the first term, so the result is always 1.
	   1)
	  ((setq n (hyp-negp-in-l arg-l1))
	   ;; A negative integer in the first series means we have a
	   ;; polynomial.
	   (create-poly arg-l1 arg-l2 n))
	  ((or (zerop-in-l arg-l2)
	       (hyp-negp-in-l arg-l2))
	   ;; A zero or negative number in the second index means we
	   ;; eventually divide by zero, so we're undefined.
	   'undef)
	  (t
	   ;; We failed so more complicated stuff needs to be done.
	   (append (list 'fail) (list arg-l1) (list arg-l2))))))

			

(defun intdiffl1l2 (arg-l1 arg-l2)
  (cond ((null arg-l1)
	 nil)
	(t
	 (intdiff arg-l1 arg-l2))))

#+nil
(defun intdiff (arg-l1 arg-l2)
  (prog(l a dif)
     (setq l arg-l2 a (car arg-l1))
     jump
     (cond ((null l)(return (intdiffl1l2 (cdr arg-l1) arg-l2))))
     (cond ((nni (setq dif (sub a (car l))))
	    (return (list a dif))))
     (setq l (cdr l))
     (go jump)))

;; For each element x in arg-l1 and y in arg-l2, compute d = x - y.
;; Find the smallest such non-negative integer d and return (list x
;; d).
(defun intdiff (arg-l1 arg-l2)
  (let ((result nil))
    ;; Compute all possible differences between elements in arg-l1 and
    ;; arg-l2.  Only save the ones where the difference is a positive
    ;; integer
    (dolist (x arg-l1)
      (dolist (y arg-l2)
	(let ((diff (sub x y)))
	  (when (nni diff)
	    (push (list x diff) result)))))
    ;; Find the smallest one and return it.
    (let ((min (first result)))
      (dolist (x (rest result))
	(when (lessp (second x) (second min))
	  (setf min x)))
      min)))
    

;; Create the appropriate polynomial for the hypergeometric function.
(defun create-poly (arg-l1 arg-l2 n)
  (let ((len1 (length arg-l1))
	(len2 (length arg-l2)))
    ;; n is the smallest (in magnitude) negative integer in L1.  To
    ;; make everything come out right, we need to make sure this value
    ;; is first in L1.  This is ok, the definition of the
    ;; hypergeometric function does not depend on the order of values
    ;; in L1.
    (setf arg-l1 (cons n (remove n arg-l1 :count 1)))
    (cond ((and (equal len1 2)
		(equal len2 1))
	   (2f1polys arg-l1 arg-l2 n))
	  ((and (equal len1 1)
		(equal len2 1))
	   (1f1polys arg-l2 n))
	  ((and (equal len1 2)
		(zerop len2))
	   (2f0polys arg-l1 n))
	  (t (create-any-poly arg-l1 arg-l2 (mul -1 n))))))


(defun 1f1polys (arg-l2 n)
  (let* ((c (car arg-l2))
	 (n (mul -1 n))
	 (fact1 (mul (power 2 n)
		     (factorial n)
		     (inv (power -1 n))))
	 ;; For all of the polynomials here, I think it's ok to
	 ;; replace sqrt(z^2) with z because when everything is
	 ;; expanded out they evaluate to exactly the same thing.  So
	 ;; $radexpand $all is ok here.
	 (fact2 (let (($radexpand '$all))
		  (mul (power 2 (inv 2))
		       (power var (inv 2))))))
    (cond ((alike1 c (div 1 2))
	   ;; A&S 22.5.56
	   ;; hermite(2*n,x) = (-1)^n*(2*n)!/n!*M(-n,1/2,x^2)
	   ;;
	   ;; So
	   ;; M(-n,1/2,x) = n!/(2*n)!*(-1)^n*hermite(2*n,sqrt(x))
	   ;;
	   ;; But hermite(m,x) = 2^(m/2)*He(sqrt(2)*sqrt(x)), so
	   ;;
	   ;; M(-n,1/2,x) = (-1)^n*n!*2^n/(2*n)!*He(2*n,sqrt(2)*sqrt(x))
	   (mul fact1
		(inv (factorial (add n n)))
		(hermpol (add n n) fact2)))
	  ((alike1 c (div 3 2))
	   ;; A&S 22.5.57
	   ;; hermite(2*n+1,x) = (-1)^n*(2*n+1)!/n!*M(-n,3/2,x^2)*2*x
	   ;;
	   ;; So
	   ;; M(-n,3/2,x) = n!/(2*n+1)!*(-1)^n*hermite(2*n+1,sqrt(x))/2/sqrt(x)
	   ;;
	   ;; and in terms of He, we get
	   ;;
	   ;; M(-n,3/2,x) = (-1)^n*n!*2^(n-1/2)/(2*n+1)!/sqrt(x)*He(2*n+1,sqrt(2)*sqrt(x))
	   (mul fact1
		(inv (power 2 (inv 2)))
		(inv (factorial (add n n 1)))
		(hermpol (add n n 1) fact2)
		;; Similarly, $radexpand here is ok to convert sqrt(z^2) to z.
		(let (($radexpand '$all))
		  (inv (power var (inv 2))))))
	  (t
	   ;; A&S 22.5.54:
	   ;;
	   ;; gen_laguerre(n,alpha,x) =
	   ;; binomial(n+alpha,n)*hgfred([-n],[alpha+1],x);
	   ;;
	   ;; Or hgfred([-n],[alpha],x) =
	   ;; gen_laguerre(n,alpha-1,x)/binomial(n+alpha-1,n)
	   (mul (factorial n)
		(gm c)
		(inv (gm (add c n)))
		(lagpol n (sub c 1) var))))))

;; Hermite polynomial.  Note: The Hermite polynomial used here is the
;; He polynomial, defined as (A&S 22.5.18, 22.5.19)
;;
;; He(n,x) = 2^(-n/2)*H(n,x/sqrt(2))
;;
;; or
;;
;; H(n,x) = 2^(n/2)*He(x*sqrt(2))
;;
;; We want to use H, as used in specfun, so we need to convert it.

(defun hermpol (n arg)
  (let ((fact (inv (power 2 (div n 2))))
	(x (mul arg (inv (power 2 (div 1 2))))))
    (mul fact `(($hermite) ,n ,x))))


;; Generalized Laguerre polynomial
(defun lagpol (n a arg)
  (if (and (numberp a) (zerop a))
      `(($laguerre) ,n ,arg)
      `(($gen_laguerre) ,n ,a, arg)))


(defun 2f0polys (arg-l1 n)
  (let ((a (car arg-l1))
	(b (cadr arg-l1)))
    (when (alike1 (sub b a) (div -1 2))
      (rotatef a b))
    (cond ((alike1 (sub b a) (div 1 2))
	   ;; 2F0(-n,-n+1/2,z) or 2F0(-n-1/2,-n,z)
	   (interhermpol n a b var))
	  (t
	   ;; 2F0(a,b;z)
	   (let ((x (mul -1 (inv var)))
		 (order (mul -1 n)))
	     (mul (factorial order)
		  (inv (power x order))
		  (inv (power -1 order))
		  (lagpol order (mul -1 (add b order)) x)))))))

;; Compute 2F0(-n,-n+1/2;z) and 2F0(-n-1/2,-n;z) in terms of Hermite
;; polynomials.
;;
;; Ok.  I couldn't find any references giving expressions for this, so
;; here's a quick derivation.
;;
;; 2F0(-n,-n+1/2;z) = sum(pochhammer(-n,k)*pochhammer(-n+1/2,k)*z^k/k!, k, 0, n)
;;
;; It's easy to show pochhammer(-n,k) = (-1)^k*n!/(n-k)!
;; Also, it's straightforward but tedious to show that
;; pochhammer(-n+1/2,k) = (-1)^k*(2*n)!*(n-k)!/2^(2*k)/n!/(2*n-2*k)!
;;
;; Thus,
;; 2F0 = (2*n)!*sum(z^k/2^(2*k)/k!/(2*n-2*k)!)
;;
;; Compare this to the expression for He(2*n,x) (A&S 22.3.11):
;;
;; He(2*n,x) = (2*n)! * x^(2*n) * sum((-1)^k*x^(-2*k)/2^k/k!/(2*n-2*k)!)
;;
;; Hence,
;;
;; 2F0(-n,-n+1/2;z) = y^n * He(2*n,y)
;;
;; where y = sqrt(-2/x)
;;
;; For 2F0(-n-1/2,-n;z) = sum(pochhammer(-n,k)*pochhammer(-n-1/2,k)*z^k/k!)
;; we find that
;;
;; pochhammer(-n-1/2,k) = pochhammer(-(n+1)+1/2,k)
;;  = 
;;
;; So 2F0 = (2*n+1)!*sum(z^k/z^(2*k)/k!/(2*n+1-2*k)!)
;;
;; and finally
;;
;; 2F0(-n-1/2,-n;z) = y^(2*n+1) * He(2*n+1,y)
;;
;; with y as above.
(defun interhermpol (n a b x)
  (let ((arg (power (div 2 (mul -1 x)) (inv 2)))
	(order (cond ((alike1 a n)
		      (mul -2 n))
		     ((alike1 b n)
		      (sub 1 (add n n))))))
    ;; 2F0(-n,-n+1/2;z) = y^(-2*n)*He(2*n,y)
    ;; 2F0(-n-1/2,-n;z) = y^(-(2*n+1))*He(2*n+1,y)
    ;;
    ;; where y = sqrt(-2/var);
    (mul (power arg (mul -1 order))
	 (hermpol order arg))))

;; F(a,b;c;z), where either a or b is a negative integer.
(defun 2f1polys (arg-l1 arg-l2 n)
  (prog (l v lgf)
     ;; Since F(a,b;c;z) = F(b,a;c;z), make sure L1 has the negative
     ;; integer first, so we have F(-n,d;c;z)
     (cond ((not (eql (car arg-l1) n))
	    (setq arg-l1 (reverse arg-l1))))
     #||
     (format t "l1 = ~A~%" arg-l1)
     (format t "vfvp arg = ~A~%" (div (add (cadr arg-l1) n) 2))
     (format t "var = ~A~%" var)
     (format t "*par* = ~A~%" *par*)
     ||#
     (setq l (vfvp (div (add (cadr arg-l1) n) 2)))

     ;;(format t "l  = ~A~%" l)
     (setq v (cdr (zl-assoc 'v l)))

     ;; Assuming we have F(-n,b;c;z), then v is (b+n)/2.
	    
     ;;(format t "v = ~A~%" v)

     ;; See if it can be a Legendre function.
     (cond ((setq lgf (legpol (car arg-l1) (cadr arg-l1) (car arg-l2)))
	    (return lgf)))

     (cond ((alike1 (sub (car arg-l2) v) '((rat simp) 1 2))
	    ;; A&S 15.4.5:
	    ;; F(-n, n + 2*a; a + 1/2; x) = n!*gegen(n, a, 1-2*x)/pochhammer(2*a,n)
	    ;;
	    ;; So v = a, and (car arg-l2) = a + 1/2.
	    (return (mul 
		     (cond ((zerp v) 1)
			   (t (mul (factorial (* -1 n))
				   (inv (factf (mul 2 v) (* -1 n))))))
		     (gegenpol (mul -1 n)
			       v
			       (sub 1 (mul 2 *par*)))))))
     ;; A&S 15.4.6 says
     ;; F(-n, n + a + 1 + b; a + 1; x) = n!*jacobi_p(n,a,b,1-2*x)/pochhammer(a+1,n);
     ;;
     (return (mul (factorial (* -1 n))
		  ;; I (rlt) don't think this is right, based on
		  ;; 15.4.6, because v doesn't have the right value.
		  #+nil(inv (factf (add 1 v) (* -1 n)))
		  ;; Based on 15.4.6, we really want the arg-l2 arg
		  (inv (factf (car arg-l2) (* -1 n)))
		  (jacobpol (mul -1 n)
			    (add (car arg-l2) -1)
			    (sub (mul 2 v) (car arg-l2))
			    (sub 1 (mul 2 *par*)))))))


#+nil
(defun jacobpol
    (n a b x)
  (list '(mqapply)(list '($%p array) n a b) x))


;; Jacobi polynomial
(defun jacobpol (n a b x)
  `(($jacobi_p) ,n ,a ,b ,x))


#+nil
(defun gegenpol(n v x)
  (cond ((equal v 0) (tchebypol n x))
	(t (list '(mqapply)(list '($%c array) n v) x))))

;; Gegenbauer (Ultraspherical) polynomial.  We use ultraspherical to
;; match specfun.
(defun gegenpol (n v x)
  (cond ((equal v 0) (tchebypol n x))
	(t `(($ultraspherical) ,n ,v ,x))))

;; Legendre polynomial
(defun legenpol (n x)
  `(($legendre_p) ,n ,x))

;; Chebyshev polynomial
(defun tchebypol (n x)
  `(($chebyshev_t) ,n ,x))

;; Expand the hypergeometric function as a polynomial.  No real checks
;; are made to ensure the hypergeometric function reduces to a
;; polynomial.
(defun $hgfpoly (arg-l1 arg-l2 arg)
  (let ((var arg)
	(*par* arg)
	(n (hyp-negp-in-l (cdr arg-l1))))
    (create-any-poly (cdr arg-l1) (cdr arg-l2) (- n))))

(defun create-any-poly (arg-l1 arg-l2 n)
  (prog (result exp prodnum proden)
     (setq result 1 prodnum 1 proden 1 exp 1)
     loop
     (cond ((zerop n) (return result)))
     (setq prodnum (mul prodnum (mull arg-l1))
	   proden (mul proden (mull arg-l2)))
     (setq result
	   (add result
		(mul prodnum
		     (power var exp)
		     (inv proden)
		     (inv (factorial exp)))))
     (setq n (sub n 1)
	   exp (add exp 1)
	   arg-l1 (incr1 arg-l1)
	   arg-l2 (incr1 arg-l2))
     (go loop)))


;; Compute the product of the elements of the list L.
(defun mull (l)
  (cond ((null l) 1)
	(t (mul (car l) (mull (cdr l))))))


;; Add 1 to each element of the list L
(defun incr1 (l)
  (cond ((null l) nil)
	(t (append (list (add (car l) 1)) (incr1 (cdr l))))))


#+nil
(defun dispatch-spec-simp (arg-l1 arg-l2)
  (prog(len1 len2)
     (setq len1 (length arg-l1) len2 (length arg-l2))
     (cond ((and (lessp len1 2)
		 (lessp len2 2))
	    (return (simp2>f<2 arg-l1 arg-l2 len1 len2))))
     (cond ((and (equal len1 2)
		 (equal len2 1))
	    (return (simp2f1 arg-l1 arg-l2))))
     (return (fpqform arg-l1 arg-l2 var))))

;; Figure out the orders of generalized hypergeometric function we
;; have and call the right simplifier.
(defun dispatch-spec-simp (arg-l1 arg-l2)
  (let  ((len1 (length arg-l1))
	 (len2 (length arg-l2)))
    (cond ((and (lessp len1 2)
		(lessp len2 2))
	   ;; pFq where p and q < 2.
	   (simp2>f<2 arg-l1 arg-l2 len1 len2))
	  ((and (equal len1 2)
		(equal len2 1))
	   ;; 2F1
	   (simp2f1 arg-l1 arg-l2))
	  (t
	   ;; We don't have simplifiers for any other hypergeometric
	   ;; function.
	   (fpqform arg-l1 arg-l2 var)))))


#+nil
(defun simp2>f<2 (l1 l2 len1 len2)
  (prog()
     (cond ((and (zerop len1) (zerop len2))
	    (return (power '$%e var))))
     (cond ((and (zerop len1) (equal len2 1))
	    (return (bestrig (car l2) var))))
     (cond ((zerop len2) (return (binom (car l1)))))
     (return (confl l1 l2 var))))

;; Handle the cases where the number of indices is less than 2.
(defun simp2>f<2 (arg-l1 arg-l2 len1 len2)
  (cond ((and (zerop len1) (zerop len2))
	 ;; hgfred([],[],z) = e^z
	 (power '$%e var))
	((and (zerop len1) (equal len2 1))
	 ;; hgfred([],[b],z)
	 ;;
	 ;; The hypergeometric series is then
	 ;;
	 ;; 1+sum(z^k/k!/[b*(b+1)*...(b+k-1)], k, 1, inf)
	 ;;
	 ;; = 1+sum(z^k/k!*gamma(b)/gamma(b+k), k, 1, inf)
	 ;; = sum(z^k/k!*gamma(b)/gamma(b+k), k, 0, inf)
	 ;; = gamma(b)*sum(z^k/k!/gamma(b+k), k, 0, inf)
	 ;;
	 ;; Note that bessel_i(b,z) has the series
	 ;;
	 ;; (z/2)^(b)*sum((z^2/4)^k/k!/gamma(b+k+1), k, 0, inf)
	 ;;
	 ;; bessel_i(b-1,2*sqrt(z))
	 ;;    = (sqrt(z))^(b-1)*sum(z^k/k!/gamma(b+k),k,0,inf)
	 ;;    = z^((b-1)/2)*hgfred([],[b],z)/gamma(b)
	 ;;
	 ;; So this hypergeometric series is a Bessel I function:
	 ;;
	 ;; hgfred([],[b],z) = bessel_i(b-1,2*sqrt(z))*z^((1-b)/2)*gamma(b)  
	 (bestrig (car arg-l2) var))
	((zerop len2)
	 ;; hgfred([a],[],z) = 1 + sum(binomial(a+k,k)*z^k)
	 ;;  = 1/(1-z)^a
	 (binom (car arg-l1)))
	(t
	 ;; The general case of 1F1, the confluent hypergeomtric function.
	 (confl arg-l1 arg-l2 var))))



	    
;; Computes 
;;
;; bessel_i(a-1,2*sqrt(x))*gamma(a)*x^((1-a)/2)
;;
;; if x > 0
;;
;; or
;;
;; bessel_j(a-1,2*sqrt(x))*gamma(a)*x^((1-a)/2)
;;
;; if x < 0.
;;
;; If a is half of an odd integer and small enough, the Bessel
;; functions are expanded in terms of trig or hyperbolic functions.

#+nil
(defun bestrig (a x)
  (prog (n res)
     ;; gamma(a)*x^((1-a)/2)
     (setq res (mul (gm a) (power x (div (sub 1 a) 2))))
     #+(or)
     (cond ((and (hyp-integerp (add a a))
		 (numberp (setq n (sub a (inv 2))))
		 (lessp n $bestriglim))
	    ;; This is totally broken.  It's got an extra (-1)^foo
	    ;; factor, so let's not use it at all for now.  Use the
	    ;; general forms below and let expand get the right
	    ;; answer.
	    (return (mul res
			 (meval (besredtrig (- n 1)
					    (mul 2
						 '$%i
						 (power
						  x
						  (inv
						   2)))))))))
     (cond ((equal (checksigntm x) '$negative)
	    ;; Not sure this is right, but the call to bes has an
	    ;; extra factor (-1)^(-(a-1)/2), so we cancel that out by
	    ;; multiplying by (-1)^((a-1)/2).
	    (return (mul res
			 (power -1 (div (sub a 1) 2))
			 (bes (sub a 1) (setq x (mul -1 x)) 'j)))))
     (return (mul res (bes (sub a 1) x 'i)))))

(defun bestrig (a x)
  ;; I think it's ok to have $radexpand $all here so that sqrt(z^2) is converted to z.
  (let* (($radexpand '$all)
	 (res (mul (gm a) (power x (div (sub 1 a) 2)))))
    ;; res = gamma(a)*x^((1-a)/2)
    (if (equal (checksigntm x) '$negative)
	;; Not sure this is right, but the call to bes has an
	;; extra factor (-1)^(-(a-1)/2), so we cancel that out by
	;; multiplying by (-1)^((a-1)/2).
	(mul res
	     (power -1 (div (sub a 1) 2))
	     (bes (sub a 1) (setq x (mul -1 x)) 'j))
	(mul res (bes (sub a 1) x 'i)))))

(defun bes (a x flg)
  (let ((fun (if (eq flg 'j) '%bessel_j '%bessel_i)))
    `((,fun) ,a ,(mul 2 (power x (inv 2))))))


;; Compute bessel_j(n+1/2,z) in terms of trig functions.
;;
;; See A&S 10.1.8 and 10.1.9.
;;
;; Note that bessel.lisp has a different implementation of this.
;; Should we use that instead?
(defun besredtrig (n z)
  (cond ((minusp n)
	 (trigredminus (mul -1 (add1 n)) z))
	(t (trigredplus n z))))

(defun trigredplus (n z)
  ((lambda(npinv2)
     (mul (ctr z)
	  (add (mul (sin% (sub z npinv2))
		    (firstsum n z))
	       (mul (cos% (sub z npinv2))
		    (secondsum n z)))))
   (mul n '$%pi (inv 2))))


(defun trigredminus
    (n z)
  ((lambda(npinv2)
     (mul (ctr z)
	  (sub (mul (cos% (add z npinv2))
		    (firstsum n z))
	       (mul (sin% (add z npinv2))
		    (secondsum n z)))))
   (mul n '$%pi (inv 2))))

(defun firstsum
    (n z)
  (prog(count result 2r n1)
     (setq n1 ($entier (div n 2)) count 0 result 1)
     loop
     (cond ((eq count n1)(return result)))
     (setq count
	   (add1 count)
	   2r
	   (add count count)
	   result
	   (add result
		(div (mul (power -1 count)
			  (factorial (add n 2r)))
		     (mul (factorial 2r)
			  (factorial (sub n 2r))
			  (power (add z z) 2r)))))
     (go loop)))

;; Compute Q(n+1/2,z) in A&S 10.1.9.
(defun secondsum (n z)
  (prog (count result 2r+1 n1)
     (setq n1
	   ($entier (div (sub1 n) 2))
	   count
	   0
	   result
	   (mul n (add 1 n) (inv (add z z))))
     (cond ((equal n1 -1)(return 0)))
     loop
     (cond ((eq count n1)(return result)))
     (setq count
	   (add1 count)
	   2r+1
	   (add count count 1)
	   result
	   (add result
		(div (mul (power -1 count)
			  (factorial (add n 2r+1)))
		     (mul (factorial 2r+1)
			  (factorial (sub n 2r+1))
			  (power (add z z) 2r+1)))))
     (go loop)))

;; sqrt(2/(pi*z))
(defun ctr (z)
  (power (div 2 (mul '$%pi z)) (inv 2)))

(defun negcoef (x)
  (prog(d)
     (cond ((null (setq d (cdr (zl-remprop 'd (d*u x)))))
	    (return t)))
     (cond ((eq (asksign (inv d)) '$positive)
	    (return nil)))
     (return t)))

;; (1-z)^(-a)
(defun binom (a)
  (power (sub 1 var) (mul -1 a)))


;; Kummer's transformation.  A&S 13.1.27
;;
;; M(a,b,z) = e^z*M(b-a,b,-z)
(defun kummer (arg-l1 arg-l2)
  (mul (list '(mexpt) '$%e var)
       (confl (list (sub (car arg-l2) (car arg-l1)))
	      arg-l2 (mul -1 var))))


;; Return non-NIL if any element of the list L is zero.
#+nil
(defun zerop-in-l (l)
  (cond ((null l) nil)
	((numberp (car l))
	 (cond ((zerop (car l)) t)(t (zerop-in-l (cdr l)))))
	(t (zerop-in-l (cdr l)))))

(defun zerop-in-l (l)
  (some #'(lambda (x)
	    (and (numberp x) (zerop x)))
	l))

#+nil
(defun hyp-negp-in-l (l)
  (cond ((null l) nil)
	((hyp-integerp (car l))
	 (cond ((minusp (car l)) (car l))
	       (t (hyp-negp-in-l (cdr l)))))
	(t (hyp-negp-in-l (cdr l)))))

;; If the list L contains a negative integer, return the most positive
;; of the negative integers.  Otherwise return NIL.
(defun hyp-negp-in-l (l)
  (let ((max-neg nil))
    (dolist (x l)
      (when (and (numberp x) (minusp x))
	(if max-neg
	    (setf max-neg (max max-neg x))
	    (setf max-neg x))))
    max-neg))

;; Compute the intersection of L1 and L2, possibly destructively
;; modifying L2.  Perserves duplications in L1.
(defun zl-intersection (arg-l1 arg-l2)
  (cond ((null arg-l1) nil)
	((zl-member (car arg-l1) arg-l2)
	 (cons (car arg-l1)
	       (zl-intersection (cdr arg-l1)
				(zl-delete (car arg-l1) arg-l2 1))))
	(t (zl-intersection (cdr arg-l1) arg-l2))))

;; Whittaker M function.  A&S 13.1.32 defines it as
;;
;; %m[k,u](z) = exp(-z/2)*z^(u+1/2)*M(1/2+u-k,1+2*u,z)
;;
;; where M is the confluent hypergeometric function.
(defun whitfun (k m var)
  (list '(mqapply) (list '($%m array) k m) var))

(defvar $trace2f1 nil
  "Enables simple tracing of simp2f1 so you can see how it decides
  what approach to use to simplify hypergeometric functions")

(defun simp2f1 (arg-l1 arg-l2)
  (prog (a b c lgf)
     (setq a (car arg-l1) b (cadr arg-l1) c (car arg-l2))
     (when $trace2f1
       (format t "Tracing SIMP2F1~%")
       (format t " Test F(1,1,2)...~%"))
     (cond ((and (alike1 a 1)
		 (alike1 b 1)
		 (alike1 c 2))
	    ;; F(1,1;2;z), A&S 15.1.3
	    (when $trace2f1
	      (format t " Yes~%"))
	    (return (mul (inv (mul -1 var))
			 (mlog (add 1 (mul -1 var)))))))
     (when $trace2f1
       (format t " Test c = 1/2 or c = 3/2...~%"))
     (cond ((or (alike1 c  (div 3 2))
		(alike1 c  (div 1 2)))
	    ;; F(a,b; 3/2; z) or F(a,b;1/2;z)
	    (cond ((setq lgf (trig-log (list a b) (list c)))
		   (when $trace2f1
		     (format t " Yes: trig-log~%"))
		   (return lgf)))))
     (when $trace2f1
       (format t " Test |a-b|=1/2...~%"))
     (cond ((or
	     (alike1 (sub a b) (div 1 2))
	     (alike1 (sub b a) (div 1 2)))
	    ;; F(a,b;c;z) where |a-b|=1/2 
	    (cond ((setq lgf (hyp-cos a b c))
		   (when $trace2f1
		     (format t " Yes: hyp-cos~%"))
		   (return lgf)))))
     (when $trace2f1
       (format t " Test a,b are integers, c is a numerical integer...~%"))
     (cond ((and (maxima-integerp a)
		 (maxima-integerp b)
		 (hyp-integerp c))
	    ;; F(a,b;c;z) when a, and b are integers (or are declared
	    ;; to be integers) and c is a integral number.
	    (setf lgf (simpr2f1 (list a b) (list c)))
	    (unless (atom lgf)
	      (when $trace2f1
		(format t " Yes: simpr2f1~%"))
	      (return lgf))))
     (when $trace2f1
       (format t " Test a+b and c+1/2 are numerical integers...~%"))
     (cond ((and (hyp-integerp (add c (inv 2)))
		 (hyp-integerp (add a b)))
	    ;; F(a,b;c;z) where a+b is an integer and c+1/2 is an
	    ;; integer.
	    (when $trace2f1
	      (format t " Yes: step4~%"))
	    (return (step4 a b c))))
     (when $trace2f1
       (format t " Test a-b+1/2 is a numerical integer...~%"))
     (cond ((hyp-integerp (add (sub a b) (inv 2)))
	    ;; F(a,b;c,z) where a-b+1/2 is an integer
	    (cond ((setq lgf (step7 a b c))
		   (unless (atom lgf)
		     (when $trace2f1
		       (format t " Yes: step7~%"))
		     (return lgf))))))
     (when $trace2f1
       (format t " Test for Legendre function...~%"))
     (cond ((setq lgf (legfun a b c))
	    (unless (atom lgf)
	      ;; LEGFUN returned something interesting, so we're done.
	      (when $trace2f1
		(format t " Yes: case 1~%"))
	      (return lgf))
	    ;; LEGFUN didn't return anything, so try it with the args
	    ;; reversed, since F(a,b;c;z) is F(b,a;c;z).
	    (setf lgf (legfun b a c))
	    (when lgf
	      (when $trace2f1
		(format t " Yes: case 2~%"))
	      (return lgf))))
     (print 'simp2f1-will-continue-in)
     (terpri)
     (return  (fpqform arg-l1 arg-l2 var))))

;; As best as I (rtoy) can tell, step7 is meant to handle an extension
;; of hyp-cos, which handles |a-b|=1/2 and either a+b-1/2 = c or
;; c=a+b+1/2.
;;
;; Based on the code, step7 wants a = s + m/n and c = 2*s+k/l.  For
;; hyp-cos, we want c-2*a to be a integer.  Or k/l-2*m/n is an
;; integer.
#+(or)
(progn
(defun step7 (a b c)
  (prog (l m n k mn kl sym sym1 r)
     ;; Write a = sym + mn, c = sym1 + kl
     (setq l (s+c a)
	   sym (cdras 'f l)
	   mn  (cdras 'c l)
	   l (s+c c)
	   syrm1 (cdras 'f l))
     ;; We only handle the case where 2*sym = sym1.
     (cond ((not (equal (mul sym 2) sym1))
	    (return nil)))
     (setq kl (cdras 'c l))
     ;; a-b+1/2 is an integer.
     (setq l (s+c b)
	   r (sub (add (inv 2) (cdras 'c l)) mn)
	   m ($num mn)
	   n ($denom mn)
	   k ($num kl)
	   l ($denom kl))
     ;; We have a = m*s+m/n, c = 2*m*s+k/l.
     (cond ((equal (* 2 l) n)
	    (cond ((hyp-integerp (/ (- k m) n))
		   (return (hyp-algv k l m n a b c))))))
     (cond ((hyp-integerp (/ k (* 2 l)))
	    (cond ((hyp-integerp (/ m n))
		   (return (hyp-algv k l m n a b c)))
		  (t (return nil))))
	   ((hyp-integerp (/ m n))
	    (return nil))
	   ((hyp-integerp (/ (- (* k n) (* 2 l m)) (* 2 l n)))
	    (return (hyp-algv k l m n a b c))))
     (return nil)))

(defun getxy
    (k l m n)
  (prog (x y)
     (setq y 0)
     loop
     (cond ((hyp-integerp (setq x (// (+ y
					 (// k l)
					 (* -2 (// m n)))
				      2)))
	    (return (list x y))))
     (setq y (+ 2 y))
     (go loop)))

(defun hyp-algv  (k l m n a b c)
  (prog (x y xy a-b w)
     (setq a-b (sub a b))
     (setq xy (getxy k l m n)
	   x (car xy)
	   y (cadr xy))
     (cond ((< x 0)(go out)))
     (cond ((< x y)
	    (cond ((< (add a-b x (inv 2)) 0)
		   (return (f88 x y a b c fun)))
		  (t (return (f87 x y a c fun)))))
	   (t
	    (cond ((< (add a-b x (inv 2)) 0)
		     (return (f90 x y a c fun)))
		    (t (return (f89 x y a c fun))))))
     out
     (setq w (* x -1))
     (cond ((< (- (add a-b (inv 2)) w) 0)
	    (return (f92 x y a c fun)))
	   (t (return (f91 x y a c fun))))))

(defun f87 (x y a c fun )
  (mul
   (inv (mul (factf c y)
	     (factf (sub (add c y) (add a x)) (- x y))
	     (factf (sub (add c y) (add a x (inv 2)))
		    (sub (add a x (inv 2)) (add a (inv 2))))))
   (power 'ell (sub 1 c))
   (power (sub 1 'ell)(sub (add y c) (add a (inv 2))))
   ($diff (mul (power 'ell (add a x))
	       (power (sub 1 'ell)(mul -1 a))
	       ($diff (mul (power 'ell (sub (add (inv 2) x) y))
			   ($diff (mul (power 'ell (sub (add c y) 1))
				       (power (sub 1 'ell)
					      (sub (add (inv 2)
							(mul 2 a)
							(* 2 x))
						   (add c y)))
				       fun)
				  'ell x))
		      'ell (- x y)))
	  'ell y)))

(defun f88 (x y a b c fun )
  (mul
   (inv (mul (factf c y)
	     (factf (sub (add c y) (add a x)) (- x y))
	     (factf (add a (inv 2) x)
		    (sub b (sub x (sub a (inv 2)))))))
   (power 'ell (sub 1 c))
   (power (sub 1 'ell)(sub (add y c) (add a (inv 2))))
   ($diff (mul (power 'ell (add a x))
	       (power (sub 1 'ell)(mul -1 a))
	       ($diff (mul (power 'ell (sub c (sub x (sub (inv 2) (mul a 2))))))
		      (power (sub 1 'ell) (sub (add a x b)(sub c y)))
		      ($diff (mul (power 'ell (sub b  1 ))
					    
				  fun)
			     'ell (sub b (sub a (sub x (inv 2)))))
		      'ell (- x y)))
	  'ell y)))
)

;; A new version of step7.
(defun step7 (a b c)
  ;; To get here, we know that a-b+1/2 is an integer.  To make further
  ;; progress, we want a+b-1/2-c to be an integer too.
  ;;
  ;; Let a-b+1/2 = p and a+b+1/2-c = q where p and q are (numerical)
  ;; integers.
  ;;
  ;; A&S 15.2.3 and 15.2.5 allows us to increase or decrease a.  Then
  ;; F(a,b;c;z) can be written in terms of F(a',b;c;z) where a' = a-p
  ;; and a'-b = a-p-b = 1/2.  Also, a'+b+1/2-c = a-p+b+1/2-c = q-p =
  ;; r, which is also an integer.
  ;;
  ;; A&S 15.2.4 and 15.2.6 allows us to increase or decrese c.  Thus,
  ;; we can write F(a',b;c;z) in terms of F(a',b;c',z) where c' =
  ;; c+r.  Now a'-b=1/2 and a'+b+1/2-c' = a-p+b+1/2-c-r =
  ;; a+b+1/2-c-p-r = q-p-(q-p)=0.
  ;;
  ;; Thus F(a',b;c';z) is exactly the form we want for hyp-cos.  In
  ;; fact, it's A&S 15.1.14: F(a,a+1/2,;1+2a;z) =
  ;; 2^(2*a)*(1+sqrt(1-z))^(-2*a).
  (declare (special var))
  (let ((q (sub (add a b (inv 2))
		c)))
    (unless (hyp-integerp q)
      ;; Wrong form, so return NIL
      (return-from step7 nil))
    ;; Since F(a,b;c;z) = F(b,a;c;z), we need to figure out which form
    ;; to use.  The criterion will be the fewest number of derivatives
    ;; needed.
    (let* ((p1 (add (sub a b) (inv 2)))
	   (r1 (sub q p1))
	   (p2 (add (sub b a) (inv 2)))
	   (r2 (sub q p2)))
      (when $trace2f1
	(format t "step 7:~%")
	(format t "  q, p1, r1 = ~A ~A ~A~%" q p1 r1)
	(format t "     p2, r2 = ~A ~A~%" p2 r2))
      (cond ((<= (+ (abs p1) (abs r1))
		 (+ (abs p2) (abs r2)))
	     (step7-core a b c))
	    (t
	     (step7-core b a c))))))

(defun step7-core (a b c)
  (let* ((p (add (sub a b) (inv 2)))
	 (q (sub (add a b (inv 2))
		 c))
	 (r (sub q p))
	 (a-prime (sub a p))
	 (c-prime (add 1 (mul 2 a-prime))))
    ;; Ok, p and q are integers.  We can compute something.  There are
    ;; four cases to handle depending on the sign of p and r.

    (let ((fun (hyp-cos a-prime (add a-prime 1//2) c-prime)))
      ;; fun is F(a',a'+1/2;2*a'+1;z)
      (when $trace2f1
	(format t "step7-core~%")
	(format t " a,b,c = ~A ~A ~A~%" a b c)
	(format t " p,q,r = ~A ~A ~A~%" p q r)
	(format t " a', c' = ~A ~A~%" a-prime c-prime)
	(format t " F(a',a'+1/2; 1+2*a';z) =~%")
	(maxima-display fun))
      (cond ((>= p 0)
	     (cond ((>= r 0)
		    (step-7-pp a-prime b c-prime p r var fun))
		   (t
		    (step-7-pm a-prime b c-prime p r var fun))))
	    (t
	     (cond ((>= r 0)
		    (step-7-mp a-prime b c-prime p r var fun))
		   (t
		    (step-7-mm a-prime b c-prime p r var fun))))))))
  
;; F(a,b;c;z) in terms of F(a',b;c';z)
;;
;; F(a'+p,b;c'-r;z) where p >= 0, r >= 0.
(defun step-7-pp (a b c p r var fun)
  ;; Apply A&S 15.2.4 and 15.2.3
  (let ((res (as-15.2.4 a b c r var fun)))
    (as-15.2.3 a b (sub c r) p var res)))

;; p >= 0, r < 0
;;
;; Let r' = -r
;; F(a'+p,b;c'-r;z) = F(a'+p,b;c'+r';z)
(defun step-7-pm (a b c p r var fun)
  ;; Apply A&S 15.2.6 and 15.2.3
  (let ((res (as-15.2.6 a b c (- r) var fun)))
    (as-15.2.3 a b (sub c r) p var res)))
;;
;; p < 0, r >= 0
;;
;; Let p' = -p
;; F(a'+p,b;c'-r;z) = F(a'-p',b;c'-r;z)
(defun step-7-mp (a b c p r var fun)
  ;; Apply A&S 15.2.4 and 15.2.5
  (let ((res (as-15.2.4 a b c r var fun)))
    (as-15.2.5 a b (sub c r) (- p) var res)))

;; p < 0 r < 0
;;
;; Let p' = - p, r' = -r
;;
;; F(a'+p,b;c'-r;z) = F(a'-p',b;c'+r';z)
(defun step-7-mm (a b c p r var fun)
  ;; Apply A&S 15.2.6 and A&S 15.2.5
  (let ((res (as-15.2.6 a b c (- r) var fun)))
    (as-15.2.5 a b (sub c r) (- p) var res)))

;; F(a,b;c;z) when a and b are integers (or declared to be integers)
;; and c is an integral number.
(defun simpr2f1 (arg-l1 arg-l2)
  (destructuring-bind (a b)
      arg-l1
    (destructuring-bind (c)
	arg-l2
      (let ((inl1p (hyp-integerp a))
	    (inl1bp (hyp-integerp b))
	    (inl2p (hyp-integerp c)))
	(cond (inl2p
	       ;; c is an integer
	       (cond ((and inl1p inl1bp)
		      ;; a, b, c are (numerical) integers
		      (derivint a b c))
		     (inl1p
		      ;; a and c are integers
		      (geredno2 b a c))
		     (inl1bp
		      ;; b and c are integers.
		      (geredno2 a b c))
		     (t 'fail1)))
	      ;; Can't really do anything else if c is not an integer.
	      (inl1p
	       (cond (inl1bp
		      'd)
		     (t
		      'c)))
	      ((eq (caaar arg-l1) 'rat)
	       ;; How do we ever get here?
	       (cond (inl1bp
		      'c)
		     (t
		      'd)))
	      (t
	       'failg))))))

(defun geredno1
    (arg-l1 arg-l2)
  (cond ((and (greaterp (car arg-l2)(car arg-l1))
	      (greaterp (car arg-l2)(cadr arg-l1)))
	 (geredf (car arg-l1)(cadr arg-l1)(car arg-l2)))
	(t (gered1 arg-l1 arg-l2 #'hgfsimp))))

(defun geredno2 (a b c)
  (cond ((greaterp c b) (geredf b a c))
	(t (gered2 a b c))))

;; Consider F(1,1;2;z).  A&S 15.1.3 says this is equal to -log(1-z)/z.
;;
;; Apply A&S 15.2.2:
;;
;; diff(F(1,1;2;z),z,ell) = poch(1,ell)*poch(1,ell)/poch(2,ell)*F(1+ell,1+ell;2+ell;z)
;;
;; A&S 15.2.7 says:
;;
;; diff((1-z)^(m+ell)*F(1+ell;1+ell;2+ell;z),z,m)
;;    = (-1)^m*poch(1+ell,m)*poch(1,m)/poch(2+ell,m)*(1-z)^ell*F(1+ell+m,1+ell;2+ell+m;z)
;;
;; A&S 15.2.6 gives
;;
;; diff((1-z)^ell*F(1+ell+m,1+ell;2+ell+m;z),z,n)
;;    = poch(1,n)*poch(1+m,n)/poch(2+ell+m,n)*(1-z)^(ell-n)*F(1+ell+m,1+ell;2+ell+m+n;z)
;;
;; The derivation above assumes that ell, m, and n are all
;; non-negative integers.  Thus, F(a,b;c;z), where a, b, and c are
;; integers and a <= b <= c, can be written in terms of F(1,1;2;z).
;; The result also holds for b <= a <= c, of course.
;;
;; Also note that the last two differentiations can be combined into
;; one differention since the result of the first is in exactly the
;; form required for the second.  The code below does one
;; differentiation.
;;
;; So if a = 1+ell, b = 1+ell+m, and c = 2+ell+m+n, we have ell = a-1,
;; m = b - a, and n = c - ell - m - 2 = c - b - 1.
;;
(defun derivint (a b c)
  (if (> a b)
      (derivint b a c)
      (let ((l (- a 1))
	    (m (- b a))
	    (n (- c b 1)))
	(subst var 'psey
	       (mul (power -1 m)
		    (factorial (+ n m l 1))
		    (inv (factorial n))
		    (inv (factorial l))
		    (inv (factorial (+ n m)))
		    (inv (factorial (+ m l)))
		    (power (sub 1 'psey) (sub n l))
		    ($diff (mul (power (sub 1 'psey) (+ m l))
				($diff (mul (power  'psey  -1)
					    -1
					    (mlog (sub 1 'psey)))
				       'psey
				       l))
			   'psey
			   (+ n m)))))))


#+nil
(defun hyp-cos (a b c)
  (prog (a2 a1 z1)
     ;; a1 = (a+b-1/2)/2
     ;; z1 = 1-var
     ;; a2 = c/2
     (setq a1 (div (sub (add a b) (div 1 2)) 2))
     (setq z1 (sub 1 var))
     (setq a2 (mul c (inv 2)))
     (cond ((equal (sub (add a b) (div 1 2)) c)
	    ;; a+b-1/2 = c
	    ;;
	    ;; 2^(2*a1 - 1)/sqrt(z1)*(1+sqrt(z1))^(1-2*a1)
	    (return (mul (power 2 (sub (mul a1 2) 1))
			 (inv (power  z1 (div 1 2)))
			 (power (add 1
				     (power z1
					    (div 1
						 2)))
				(sub 1 (mul 2 a1)))))))
     (cond ((equal (add 1 (mul 2 a1)) c)
	    ;; c = 1+2*a1 = a+b+1/2
	    ;;
	    ;; 2^(c-1)*(1+sqrt(z1))^(-(c-1))
	    (return (mul (power 2 (sub c 1))
			 (power (add 1
				     (power z1
					    (div 1
						 2)))
				(mul -1 (sub c 1)))))))
     ))

(defun hyp-cos (a b c)
  (let ((a1 (div (sub (add a b) (div 1 2)) 2))
	(z1 (sub 1 var)))
    ;; a1 = (a+b-1/2)/2
    ;; z1 = 1-var
    (cond ((alike1 (sub (add a b)
			(div 1 2))
		   c)
	   ;; a+b-1/2 = c
	   ;;
	   ;; A&S 15.1.14
	   ;;
	   ;; F(a,a+1/2;2*a;z)
	   ;;    = 2^(2*a-1)*(1-z)^(-1/2)*(1+sqrt(1-z))^(1-2*a)
	   ;;
	   ;; But if 1-2*a is a negative integer, let's rationalize the answer to give
	   ;;
	   ;; F(a,a+1/2;2*a;z)
	   ;;   = 2^(2*a-1)*(1-z)^(-1/2)*(1-sqrt(1-z))^(2*a-1)/z^(2*a-1)
	   (when $trace2f1
	     (format t "   Case a+b-1/2=c~%"))
	   (let ((2a-1 (sub (mul a1 2) 1)))
	     (cond ((and (integerp 2a-1) (plusp 2a-1))
		    ;; 2^(2*a-1)*(1-z)^(-1/2)*(1-sqrt(1-z))^(2*a-1)/z^(2*a-1)
		    (mul (power 2 2a-1)
			 (inv (power z1 1//2))
			 (power (sub 1 (power z1 1//2)) 2a-1)
			 (inv (power var 2a-1))))
		   (t
		    ;; 2^(2*a-1)*(1-z)^(-1/2)*(1+sqrt(1-z))^(1-2*a)
		    (mul (power 2 (sub (mul a1 2) 1))
			 (inv (power  z1 (div 1 2)))
			 (power (add 1
				     (power z1
					    (div 1
						 2)))
				(sub 1 (mul 2 a1))))))))
	  ((alike1 (add 1 (mul 2 a1)) c)
	   ;; c = 1+2*a1 = a+b+1/2
	   ;;
	   ;; A&S 15.1.13:
	   ;;
	   ;; F(a,1/2+a;1+2*a;z) = 2^(2*a)*(1+sqrt(1-z))^(-2*a)
	   ;;
	   ;; But if 2*a is a positive integer, let's rationalize the answer to give
	   ;;
	   ;; F(a,1/2+a;1+2*a;z) = 2^(2*a)*(1-sqrt(1-z))^(2*a)/z^(2*a)
	   (when $trace2f1
	     (format t "   Case c=1+2*a=a+b+1/2~%"))
	   (let ((2a (sub c 1)))
	     (cond ((and (integerp 2a) (plusp 2a))
		    ;; 2^(2*a)*(1-sqrt(1-z))^(2*a)/z^(2*a)
		    (mul (power 2 2a)
			 (power (sub 1 (power z1 1//2))
				2a)
			 (power var (mul -1 2a))))
		   (t
		    ;; 2^(2*a)*(1+sqrt(1-z))^(-2*a)
		    (mul (power 2 2a)
			 (power (add 1 (power z1 1//2))
				(mul -1 2a))))))))))

;; Is A a non-negative integer?
(defun nni (a)
  (cond ((hyp-integerp a)
	 (not (minusp a)))))


;;; The following code doesn't appear to be used at all.  Comment it all out for now.
#||
(defun degen2f1
    (a b c)
  (cond ((eq (quest (sub c b)) '$negative)
	 (cond ((eq (quest (sub c a)) '$negative)
		(gered1 (list a b)(list c) #'hgfsimp))
	       (t (gered2 a b c))))
	((eq (quest (sub c a)) '$negative)(gered2 b a c))
	(t (rest-degen a b c))))


(defun rest-degen
    (a b c)
  (prog(m n l)
     (cond ((nni (setq m (sub a 1)))
	    (return (rest-degen-1 a b c m))))
     (cond ((ni b)(return (rest-degen-2 a b c))))
     (cond ((and (nni (setq n (sub c 1)))
		 (nni (setq m (sub (sub a n) 1)))
		 (nni (setq l (sub b a)))
		 (eq (sub (sub c a) b)
		     (mul -1 (add m m n l 1))))
	    (return (gered1 (list a b)
			    (list c)
			    #'hgfsimp))))
     (return (hyp-deg b a c))))


(defun rest-degen-1
    (a b c m)
  (prog(n l)
     (cond ((and (ni b)
		 (ni (sub (sub c a) b))
		 (nni (sub (sub c a) 1)))
	    (return (deg299 a b c))))
     (cond ((and (nni (setq n (sub (sub c m) 2)))
		 (nni (setq l (sub b c)))
		 (equal (sub (sub c a) b)
			(mul -1 (add l m 1))))
	    (return (gered1 (list a b)
			    (list c)
			    #'hgfsimp))))
     (cond ((nni (setq l (sub (sub b m) 1)))
	    (return (rest-degen-1a a b c m l))))
     (return (hyp-deg b a c))))


(defun rest-degen-1a
    (a b c m l)
  (prog(n)
     (cond ((and (nni (setq n
			    (sub (sub (sub c m) l) 2)))
		 (equal (sub n m)(sub (sub c a) b)))
	    (return (deg2913 a b c))))
     (cond ((and (equal c (mul -1 n))
		 (equal (sub (sub c a) b)
			(mul -1 (add m m l n 2))))
	    (return (deg2918 a b c))))
     (return (hyp-deg b a c))))


(defun rest-degen-2
    (a b c)
  (prog(m l)
     (cond ((and (ni c)(ni (sub (sub c a) b)))
	    (return (rest-degen-2a a b c))))
     (cond ((and (nni (setq m (sub c 1)))
		 (nni (setq l (sub a c)))
		 (ni (sub (sub c a) b)))
	    (return (deg292 a b c))))
     (return (hyp-deg b a c))))


(defun rest-degen-2a
    (a b c)
  (prog()
     (cond ((nni (sub a c))
	    (return (gered1 (list a b)
			    (list c)
			    #'hgfsimp))))
     (cond ((nni (sub (sub c a) 1))
	    (return (deg2917 a b c))))
     (return (hyp-deg b a c))))

(defun quest
    (a)
  (cond ((numberp a)(checksigntm a))
	((equal (caar a) 'rat)(checksigntm a))
	(t nil)))

(defun ni(a)(not (hyp-integerp a)))


(defun hyp-deg
    (a b c)
  (prog()
     (cond (fldeg (setq fldeg nil)
		  (return (hgfsimp (list a b)
				   (list c)
				   var))))
     (setq fldeg t)
     (return (fpqform (list a b)(list c) var))))


(defun deg2913
    (a b c)
  (mul (power (mul -1 var)(mul -1 b))
       (hgfsimp (list (add b 1 (mul -1 c)) b)
		(list (add b 1 (mul -1 a)))
		(inv var))))


(defun deg2918
    (a b c)
  (mul (power var (sub 1 c))
       (power (sub 1 var)(add c (mul -1 a)(mul -1 b)))
       (hgfsimp (list (sub 1 a)(sub 1 b))
		(list (sub 2 c))
		var)))


(defun deg2917
    (a b c)
  (mul (power var (sub 1 c))
       (hgfsimp (list (add a 1 (mul -1 c))
		      (add b 1 (mul -1 c)))
		(list (sub 2 c))
		var)))


(defun deg299
    (a b c)
  (mul (power (mul -1 var)(mul -1 a))
       (hgfsimp (list a (add a 1 (mul -1 c)))
		(list (add a 1 (mul -1 b)))
		(inv var))))

||#


;; Is F(a, b; c; z) is Legendre function?
(defun legfun (a b c)			   
  (let ((1-c (sub 1 c))
	(a-b (sub a b))
	(c-a-b (sub (sub c a) b))
	(inv2 (inv 2)))
    (cond ((alike1 a-b inv2)
	   ;; a-b = 1/2
	   (when $trace2f1
	     (format t "Legendre a-b = 1/2~%"))
	   (gered1 (list a b) (list c) #'legf24))
	  ((alike1 a-b (mul -1 inv2))
	   ;; a-b = -1/2
	   ;;
	   ;; For example F(a,a+1/2;c;x)
	   (when $trace2f1
	     (format t "Legendre a-b = -1/2~%"))
	   (legf24 (list a b) (list c) var))
	  ((alike1 c-a-b inv2)
	   ;; c-a-b = 1/2
	   ;;
	   ;; For example F(a,b;a+b+1/2;z)
	   (when $trace2f1
	     (format t "Legendre c-a-b = 1/2~%"))
	   (legf20 (list a b) (list c) var))
	  ((alike1 c-a-b (mul -1 inv2))
	   ;; c-a-b = -1/2
	   (when $trace2f1
	     (format t "Legendre c-a-b = -1/2~%"))
	   (gered1 (list a b) (list c) #'legf20))
	  ((alike1 1-c a-b)
	   ;; 1-c = a-b
	   (when $trace2f1
	     (format t "Legendre 1-c = a-b~%"))
	   (gered1 (list a b) (list c) #'legf16))
	  ((alike1 1-c (mul -1 a-b))
	   ;; 1-c = b-a
	   (when $trace2f1
	     (format t "Legendre 1-c = b-a~%"))
	   (legf16 (list a b) (list c) var))
	  ((alike1 1-c c-a-b)
	   ;; 1-c = c-a-b
	   (when $trace2f1
	     (format t "Legendre 1-c = c-a-b~%"))
	   (gered1 (list a b) (list c) #'legf14))
	  ((alike1 1-c (mul -1 c-a-b))
	   ;; 1-c = a+b-c
	   ;;
	   ;; For example F(a,1-a;c;x)
	   (when $trace2f1
	     (format t "Legendre 1-c = a+b-c~%"))
	   (legf14 (list a b) (list c) var))
	  ((alike1 a-b (mul -1 c-a-b))
	   ;; a-b = a+b-c
	   ;;
	   ;; For example F(a,b;2*b;z)
	   (when $trace2f1
	     (format t "Legendre a-b = a+b-c~%"))
	   (legf36 (list a b) (list c) var))
	  ((or (alike1 1-c inv2)
	       (alike1 1-c (mul -1 inv2)))
	   ;; 1-c = 1/2 or 1-c = -1/2
	   ;;
	   ;; For example F(a,b;1/2;z) or F(a,b;3/2;z)
	   (when $trace2f1
	     (format t "Legendre |1-c| = 1/2~%"))
	   (legpol a b c))
	  ((alike1 a-b c-a-b)
	   ;; a-b = c-a-b
	   (when $trace2f1
	     (format t "Legendre a-b = c-a-b~%"))
	   'legendre-funct-to-be-discovered)
	  (t
	   nil))))


;;; The following legf<n> functions correspond to formulas in Higher
;;; Transcendental Functions.  See the chapter on Legendre functions,
;;; in particular the table on page 124ff,

;; Handle the case c-a-b = 1/2
;;
;; Formula 20:
;;
;; P(n,m,z) = 2^m*(z^2-1)^(-m/2)/gamma(1-m)*F(1/2+n/2-m/2, -n/2-m/2; 1-m; 1-z^2)
;;
;; See also A&S 15.4.12 and 15.4.13.
;;
;; Let a = 1/2+n/2-m/2, b = -n/2-m/2, c = 1-m.  Then, m = 1-c.  And we
;; have two equivalent expressions for n:
;;
;; n = c - 2*b - 1 or n = 2*a - c
;;
;; The code below chooses the first solution.  A&S chooses second.
;;
;; F(a,b;c;w) = 2^(c-1)*gamma(c)*(-w)^((1-c)/2)*P(c-2*b-1,1-c,sqrt(1-w))
;;
;;
#+nil
(defun legf20 (arg-l1 arg-l2 var)
  (let* ((b (cadr arg-l1))
	 (c (car arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 (add b b m))))
    ;; m = 1 - c
    ;; n = -(2*b+1-c) = c - 1 - 2*b
    (mul (gm (sub 1 m))
	 (power 2 (mul -1 m))
	 (power (mul -1 var) (div m 2))
	 (legen n
		m
		(power (sub 1 var) (inv 2))
		'$p))))

(defun legf20 (arg-l1 arg-l2 var)
  (let* (($radexpand nil)
	 (b (cadr arg-l1))
	 (c (car arg-l2))
	 (a (sub (sub c b) (inv 2)))
	 (m (sub 1 c))
	 (n (mul -1 (add b b m))))
    ;; m = 1 - c
    ;; n = -(2*b+1-c) = c - 1 - 2*b
    (cond ((and (eq (asksign var) '$positive)
		(eq (asksign (sub 1 var)) '$positive))
	   ;; A&S 15.4.13
	   ;;
	   ;; F(a,b;a+b+1/2;x) = 2^(a+b-1/2)*gamma(a+b+1/2)*x^((1/2-a-b)/2)
	   ;;                     *assoc_legendre_p(a-b-1/2,1/2-a-b,sqrt(1-x))
	   ;;
	   (mul (power 2 (add a b (inv -2)))
		(gm (add a b (inv 2)))
		(power var
		       (div (sub (inv 2) (add a b))
			    2))
		(legen n
		       m
		       (power (sub 1 var) (inv 2))
		       '$p)))
	  (t
	   (mul (power 2 (add a b (inv -2)))
		(gm (add a b (inv 2)))
		(power (mul -1 var)
		       (div (sub (inv 2) (add a b))
			    2))
		(legen n
		       m
		       (power (sub 1 var) (inv 2))
		       '$p))))))

;; Handle the case a-b = -1/2.
;;
;; Formula 24:
;;
;; P(n,m,z) = 2^m*(z^2-1)^(-m/2)*z^(n+m)/gamma(1-m)*F(-n/2-m/2,1/2-n/2-m/2;1-m;1-1/z^2)
;;
;; See also A&S 15.4.10 and 15.4.11.
;;
;; Let a = -n/2-m/2, b = 1/2-n/2-m/2, c = 1-m.  Then m = 1-c.  Again,
;; we have 2 possible (equivalent) values for n:
;;
;; n = -(2*a + 1 - c) or n = c-2*b
;;
;; The code below chooses the first solution.
;;
;; F(a,b;c;w) = 2^(c-1)*w^(1/2-c/2)*(1-w)^(c/2-a-1/2)*P(c-2*a-1,1-c,1/sqrt(1-w))
;;
;; F(a,b;c;w) = 2^(c-1)*w^(1/2-c/2)*(1-w)^(c/2-b)*P(c-2*b,1-c,sqrt(1-w))
;;
;; Is there a mistake in 15.4.10 and 15.4.11?
;;
#+nil
(defun legf24 (arg-l1 arg-l2 var)
  (let* ((a (car arg-l1))
	 (c (car arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 (add a a m)))
	 (z (inv (power (sub 1 var) (inv 2)))))
    (mul (inv (power 2 m))
	 (power (sub (power z 2) 1)
		(div m 2))
	 (power z (mul -1 (add n m)))
	 (gm (sub 1 m))
	 (legen n
		m
		z
		'$p))))

(defun legf24 (arg-l1 arg-l2 var)
  (let* (($radexpand nil)
	 (a (car arg-l1))
	 (c (car arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 (add a a m)))
	 (z (inv (power (sub 1 var) (inv 2)))))
    ;; A&S 15.4.10, 15.4.11
    (cond ((eq (asksign var) '$negative)
	   ;; A&S 15.4.11
	   ;;
	   ;; F(a,a+1/2;c;x) = 2^(c-1)*gamma(c)(-x)^(1/2-c/2)*(1-x)^(c/2-a-1/2)
	   ;;                   *assoc_legendre_p(2*a-c,1-c,1/sqrt(1-x))
	   (mul (inv (power 2 m))
		(gm (sub 1 m))
		(power (mul -1 var) (div m 2))
		(power (sub 1 var) (sub (div m -2) a))
		(legen n
		       m
		       z
		       '$p)))
	  (t
	   (mul (inv (power 2 m))
		(gm (sub 1 m))
		(power var (div m 2))
		(power (sub 1 var) (sub (div m -2) a))
		(legen n
		       m
		       z
		       '$p))))))



;; Handle 1-c = a-b
;;
;; Formula 16:
;;
;; P(n,m,z) = 2^(-n)*(z+1)^(m/2+n)*(z-1)^(-m/2)/gamma(1-m)*F(-n,-n-m;1-m;(z-1)/(z+1))
;;
;; See also A&S 15.4.14 and 15.4.15.
;;
;; Let a = -n, b = -n-m, c = 1-m.  Then m = 1-c.  We have 2 solutions
;; for n:
;;
;; n = -a or n = c-b-1.
;;
;; The code below chooses the first solution.
;;
;; F(a,b;c;w) = gamma(c)*w^(1/2-c/2)*(1-w)^(-a)*P(-a,1-c,(1+w)/(1-w));
;;
;; FIXME:  We don't correctly handle the branch cut here!
#+nil
(defun legf16 (arg-l1 arg-l2 var)
  (let* ((a (car arg-l1))
	 (c (car arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 a))
	 (z (div (add 1 var)
		 (sub 1 var))))
    (mul (power 2 n)
	 (power (sub z 1) (div m 2))
	 (gm (sub 1 m))
	 (inv (power (add z 1) (add (div m 2) n)))
	 (legen n
		m
		z
		'$p))))

(defun legf16 (arg-l1 arg-l2 var)
  (let* (($radexpand nil)
	 (a (car arg-l1))
	 (c (car arg-l2))
	 ;; m = 1-c = b-a
	 (m (sub 1 c))
	 ;; n = -b
	 ;; m = b - a, so b = a + m
	 (b (add a m))
	 (n (mul -1 b))
	 (z (div (add 1 var)
		 (sub 1 var))))
    (when $trace2f1
      (format t "a, c = ~A ~A~%" a c)
      (format t "b = ~A~%" b))
    ;; A&S 15.4.14, 15.4.15
    (cond ((eq (asksign var) '$negative)
	   ;; A&S 15.4.15
	   ;;
	   ;; F(a,b;a-b+1,x) = gamma(a-b+1)*(1-x)^(-b)*(-x)^(b/2-a/2)
	   ;;                   * assoc_legendre_p(-b,b-a,(1+x)/(1-x))
	   ;;
	   ;; for x < 0
	   (mul (gm c)
		(power (sub 1 var) (mul -1 b))
		(power (mul -1 var) (div m 2))
		(legen n
		       m
		       z
		       '$p)))
	  (t
	   (mul (gm c)
		(power (sub 1 var) (mul -1 b))
		(power var (div m 2))
		(legen n
		       m
		       z
		       '$p))))))


;; Handle the case 1-c = a+b-c.
;;
;; See, for example, A&S 8.1.2 (which
;; might have a bug?) or
;; http://functions.wolfram.com/HypergeometricFunctions/LegendreP2General/26/01/02/
;;
;; Formula 14:
;;
;; P(n,m,z) = (z+1)^(m/2)*(z-1)^(-m/2)/gamma(1-m)*F(-n,1+n;1-m;(1-z)/2)
;;
;; See also A&S 8.1.2, 15.4.16, 15.4.17
;;
;; Let a=-n, b = 1+n, c = 1-m.  Then m = 1-c and n has 2 solutions:
;;
;; n = -a or n = b - 1.
;;
;; The code belows chooses the first solution.
;;
;; F(a,b;c;w) = gamma(c)*(-w)^(1/2-c/2)*(1-w)^(c/2-1/2)*P(-a,1-c,1-2*w)
#+nil
(defun legf14 (arg-l1 arg-l2 var)
  (let* ((a (car arg-l1))
	 (c (car arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 a))
	 (z (sub 1 (mul 2 var))))
    (mul (power (add z 1) (div m -2))
	 (power (sub z 1) (div m 2))
	 (gm (sub 1 m))
	 (legen n m (sub 1 (mul 2 var)) '$p))))

(defun legf14 (arg-l1 arg-l2 var)
  ;; Set $radexpand to NIL, because we don't want (-z)^x getting
  ;; expanded to (-1)^x*z^x because that's wrong for this.
  (let* (($radexpand nil)
	 (a (first arg-l1))
	 (c (first arg-l2))
	 (m (sub 1 c))
	 (n (mul -1 a))
	 (z (sub 1 (mul 2 var))))
    ;; A&S 15.4.16, 15.4.17
    (cond ((and (eq (asksign var) '$positive)
		(eq (asksign (sub 1 var)) '$positive))
	   ;; A&S 15.4.17
	   ;;
	   ;; F(a,1-a;c;x) = gamma(c)*x^(1/2-c/2)*(1-x)^(c/2-1/2)*
	   ;;                 assoc_legendre_p(-a,1-c,1-2*x)
	   ;;
	   ;; for 0 < x < 1
	   (mul (gm c)
		(power var (div (sub 1 c) 2))
		(power (sub 1 var) (div (sub c 1) 2))
		(legen n m z '$p)))
	  (t
	   ;; A&S 15.4.16
	   ;;
	   ;; F(a,1-a;c;z) = gamma(c)*(-z)^(1/2-c/2)*(1-z)^(c/2-1/2)*
	   ;;                 assoc_legendre_p(-a,1-c,1-2*z)
	   (mul (gm c)
		(power (mul -1 var) (div (sub 1 c) 2))
		(power (sub 1 var) (div (sub c 1) 2))
		(legen n m z '$p))))))

;; Handle a-b = a+b-c
;;
;; Formula 36:
;;
;; exp(-%i*m*%pi)*Q(n,m,z) =
;;     2^n*gamma(1+n)*gamma(1+n+m)*(z+1)^(m/2-n-1)*(z-1)^(-m/2)/gamma(2+2*n)
;;     * hgfred([1+n-m,1+n],[2+2*n],2/(1+z))
;;
;; Let a = 1+n-m, b = 1+n, c = 2+2*n.  then n = b-1 and m = b - a.
;; (There are other solutions.)
;;
;; F(a,b;c;z) = 2*gamma(2*b)/gamma(b)/gamma(2*b-a)*w^(-b)*(1-w)^((b-a)/2)
;;              *Q(b-1,b-a,2/w-1)*exp(-%i*%pi*(b-a))
;;
(defun legf36 (arg-l1 arg-l2 var)
  (declare (ignore arg-l2))
  (let* ((a (car arg-l1))
	 (b (cadr arg-l1))
	 (n (sub b 1))
	 (m (sub b a))
	 ;;z (div (sub 2 var) var)
	 (z (sub (div 2 var) 1)))
    (mul (inv (power 2 n))
	 (inv (gm (add 1 n)))
	 (inv (gm (add 1 n m)))
	 (inv (power (add z 1)
		     (add (div m 2)
			  (mul -1 n)
			  -1)))
	 (inv (power (sub z 1) (div m -2)))
	 (gm (add 2 n n))
	 (power '$%e (mul -1 '$%i m '$%pi))
	 (legen n m z '$q))))

(defun legen (n m x pq)
  ;; A&S 8.2.1: P(-n-1,m,z) = P(n,m,z)
  ;;
  ;; Currently only applied if n is a number.  (Should this be
  ;; extended to any expression?  We'll have to ask the user for the
  ;; sign if we can' figure it out ourselves.  Should we?)
  (let ((n (if (and (mnump n)
		    (eq (checksigntm n) '$negative))
	       (mul -1 (add 1 n))
	       n)))
    (cond ((equal m 0)
	   `((,(if (eq pq '$q) '$legendre_q '$legendre_p)) ,n ,x))
	  (t
	   `((,(if (eq pq '$q) '$assoc_legendre_q '$assoc_legendre_p))
	     ,n ,m ,x)))))


(defun legpol (a b c)
  (prog (l v)
     (cond ((not (hyp-negp-in-l (list a)))
	    (print 'fail-1-in-c-1-case)
	    (return nil)))
     (setq l (vfvp (div (add b a) 2)))
     (setq v (cdr (zl-assoc 'v l)))
     ;; v is (a+b)/2
     (cond ((and (alike1 v '((rat simp) 1 2))
		 (alike1 c 1))
	    ;; A&S 22.5.49:
	    ;; P(n,x) = F(-n,n+1;1;(1-x)/2)
	    (return (legenpol (mul -1 a)
			      (sub 1 (mul 2 var))))))

     (cond ((and (alike1 c '((rat simp) 1 2))
		 (alike1 (add b a) '((rat simp) 1 2)))
	    ;; A&S 22.5.52
	    ;; P(2*n,x) = (-1)^n*(2*n)!/2^(2*n)/(n!)^2*F(-n,n+1/2;1/2;x^2)
	    ;;
	    ;; F(-n,n+1/2;1/2;x^2) = P(2*n,x)*(-1)^n*(n!)^2/(2*n)!*2^(2*n)
	    ;;
	    (let ((n (mul -1 a)))
	      (return (mul (power -1 n)
			   (power (factorial n) 2)
			   (inv (factorial (mul 2 n)))
			   (power 2 (mul 2 n))
			   (legenpol (mul 2 n)
				     (power var (div 1 2))))))))

     (cond ((and (alike1 c '((rat simp) 3 2))
		 (alike1 (add b a) '((rat simp) 3 2)))
	    ;; A&S 22.5.53
	    ;; P(2*n+1,x) = (-1)^n*(2*n+1)!/2^(2*n)/(n!)^2*F(-n,n+3/2;3/2;x^2)*x
	    ;;
	    ;; F(-n,n+3/2;3/2;x^2) = P(2*n+1,x)*(-1)^n*(n!)^2/(2*n+1)!*2^(2*n)/x
	    ;;
	    (let ((n (mul -1 a)))
	      (return (mul (power -1 n)
			   (power (factorial n) 2)
			   (inv (factorial (add 1 (mul 2 n))))
			   (power 2 (mul 2 n))
			   (legenpol (add 1 (mul 2 n))
				     (power var (div 1 2)))
			   (inv (power var (div 1 2))))))))
     
     (cond ((and (zerp (sub b a))
		 (zerp (sub c (add a b))))
	    ;; A&S 22.5.50
	    ;; P(n,x) = binomial(2*n,n)*((x-1)/2)^n*F(-n,-n;-2*n;2/(1-x))
	    ;;
	    ;; F(-n,-n;-2*n;x) = P(n,1-2/x)/binomial(2*n,n)(-1/x)^(-n)
	    (return (mul (power (factorial (mul -1 a)) 2)
			 (inv (factorial (mul -2 a)))
			 (power (mul -1 var) (mul -1 a))
			 (legenpol (mul -1 a)
				   (add 1 (div -2 var)))))))
     (cond ((and (alike1 (sub a b) '((rat simp) 1 2))
		 (alike1 (sub c (mul 2 b)) '((rat simp) 1 2)))
	    ;; A&S 22.5.51
	    ;; P(n,x) = binomial(2*n,n)*(x/2)^n*F(-n/2,(1-n)/2;1/2-n;1/x^2)
	    ;;
	    ;; F(-n/2,(1-n)/2;1/2-n,1/x^2) = P(n,x)/binomial(2*n,n)*(x/2)^(-n)
	    (return (mul (power (factorial (mul -2 b)) 2)
			 (inv (factorial (mul -4 b)))
			 (power (mul 2 (power var (div 1 2))) (mul -2 b))
			 (legenpol (mul -2 b)
				   (power var (div -1 2)))))))
     (cond ((and (alike1 (sub b a) '((rat simp) 1 2))
		 (alike1 (sub c (mul 2 a)) '((rat simp) 1 2)))
	    ;; A&S 22.5.51
	    ;; P(n,x) = binomial(2*n,n)*(x/2)^n*F(-n/2,(1-n)/2;1/2-n;1/x^2)
	    ;;
	    ;; F(-n/2,(1-n)/2;1/2-n,1/x^2) = P(n,x)/binomial(2*n,n)*(x/2)^(-n)
	    (return (mul (power (factorial (mul -2 a)) 2)
			 (inv (factorial (mul -4 a)))
			 (power (mul 2 (power var (div 1 2))) (mul -2 a))
			 (legenpol (mul -2 a)
				   (power var (div -1 2)))))))
     (return nil)))



;; See A&S 15.3.3:
;;
;; F(a,b;c;z) = (1-z)^(c-a-b)*F(c-a,c-b;c;z)
(defun gered1 (arg-l1 arg-l2 simpflg)
  (destructuring-bind (a b)
      arg-l1
    (destructuring-bind (c)
	arg-l2
      (mul (power (sub 1 var)
		  (add c
		       (mul -1 a)
		       (mul -1 b)))
	   (funcall simpflg
		    (list (sub c a)
			  (sub c b))
		    arg-l2
		    var)))))
;; See A&S 15.3.4
;;
;; F(a,b;c;z) = (1-z)^(-a)*F(a,c-b;c;z/(z-1))
(defun gered2 (a b c)
  (mul (power (sub 1 var) (mul -1 a))
       (hgfsimp (list a (sub c b))
		(list c)
		(div var (sub var 1)))))

;; See A&S 15.3.9:
;;
;; F(a,b;c;z) = A*z^(-a)*F(a,a-c+1;a+b-c+1;1-1/z)
;;              + B*(1-z)^(c-a-b)*z^(a-c)*F(c-a,1-a;c-a-b+1,1-1/z)
;;
;; where A = gamma(c)*gamma(c-a-b)/gamma(c-a)/gamma(c-b)
;;       B = gamma(c)*gamma(a+b-c)/gamma(a)/gamma(b)
;;
(defun geredf (a b c)
  (add (div (mul (gm c)
		 (gm (add c (mul -1 a)(mul -1 b)))
		 (power var (mul -1 a))
		 ($hgfred `((mlist) ,a ,(add a 1 (mul -1 c)))
			  `((mlist) ,(add a b (mul -1 c) 1))
			  (sub 1 (div 1 var))))
	    (mul (gm (sub c a))(gm (sub c b))))
       (div (mul (gm c)
		 (gm (add a b (mul -1 c)))
		 (power (sub 1 var)
			(add c (mul -1 a)(mul -1 b)))
		 (power var (sub a c))
		 ($hgfred `((mlist) ,(sub c a) ,(sub 1 a))
			  `((mlist) ,(add c
					  (mul -1 a)
					  (mul -1 b)
					  1))
			  (sub 1 (div 1 var))))
	    (mul (gm a)(gm b)))))



(defun trig-log (arg-l1 arg-l2)
  (cond ((equal (simplifya (car arg-l2) nil) '((rat simp) 3 2))
	 ;; c = 3/2
	 (when $trace2f1
	   (format t "  trig-log:  Test c=3/2~%"))
	 (trig-log-3 arg-l1 arg-l2))
	((equal (simplifya (car arg-l2) nil) '((rat simp) 1 2))
	 ;; c = 1/2
	 (when $trace2f1
	   (format t "  trig-log:  Test c=1/2~%"))
	 (trig-log-1 arg-l1 arg-l2))
	(t nil)))


(defun trig-log-3 (arg-l1 arg-l2)
  (cond ((and (or (equal (car arg-l1) 1) (equal (cadr arg-l1) 1))
	      (or (equal (car arg-l1) (div 1 2))
		  (equal (cadr arg-l1) (div 1 2))))
	 ;; (a = 1 or b = 1) and (a = 1/2 or b = 1/2)
	 (when $trace2f1
	   (format t "   Case a or b is 1 and the other is 1/2~%"))
	 (trig-log-3-exec arg-l1 arg-l2))
	((and (equal (car arg-l1) (cadr arg-l1))
	      (or (equal 1 (car arg-l1))
		  (equal (div 1 2) (car arg-l1))))
	 ;; a = b and (a = 1 or a = 1/2)
	 (when $trace2f1
	   (format t "   Case a=b and a is 1 or 1/2~%"))
	 (trig-log-3a-exec arg-l1 arg-l2))
	((or (equal (add (car arg-l1) (cadr arg-l1)) 1)
	     (equal (add (car arg-l1) (cadr arg-l1)) 2))
	 ;; a + b = 1 or a + b = 2
	 (when $trace2f1
	   (format t "   Case a+b is 1 or 2~%"))
	 (trig-sin arg-l1 arg-l2))
	((or (equal (sub (car arg-l1) (cadr arg-l1)) (div 1 2))
	     (equal (sub (cadr arg-l1) (car arg-l1)) (div 1 2)))
	 ;; a - b = 1/2 or b - a = 1/2
	 (when $trace2f1
	   (format t "   Case a-b=1/2 or b-a=1/2~%"))
	 (trig-3 arg-l1 arg-l2))
	(t nil)))

(defun trig-3 (arg-l1 arg-l2)
  (declare (ignore arg-l2))
  ;; A&S 15.1.10
  ;;
  ;; F(a,a+1/2,3/2,z^2) =
  ;; ((1+z)^(1-2*a) - (1-z)^(1-2*a))/2/z/(1-2*a)
  (let* (($radexpand '$all)
	 (a (sub 1
		 (sub (add (car arg-l1)
			   (cadr arg-l1))
		      (div 1 2))))
	 (z (power var (div 1 2))))
    (mul (inv z)
	 (inv 2)
	 (inv a)
	 (sub (power (add 1 z) a)
	      (power (sub 1 z) a)))))

(defun trig-sin (arg-l1 arg-l2)
  (declare (ignore arg-l2))
  ;; A&S 15.1.15, 15.1.16
  (destructuring-bind (a b)
      arg-l1
    ;; I think it's ok to convert sqrt(z^2) to z here, so $radexpand
    ;; is $all.
    (let (($radexpand '$all)
	  a1 z1)
      (cond ((equal (add a b) 1)
	     ;; A&S 15.1.15
	     ;;
	     ;; F(a,1-a;3/2;sin(z)^2) =
	     ;;
	     ;; sin((2*a-1)*z)/(2*a-1)/sin(z)
	     (mul (inv (mul (mul -1 (sub a b))
			    (msin (masin (msqrt var)))))
		  (msin (mul (mul -1
				  (sub a b))
			     (masin (msqrt var))))))
	    ((equal (add a b) 2)
	     ;; A&S 15.1.16
	     ;;
	     ;; F(a, 2-a; 3/2; sin(z)^2) =
	     ;;
	     ;; sin((2*a-2)*z)/(a-1)/sin(2*z)
	     (mul (msin (mul (setq z1
				   (masin (msqrt
					   var)))
			     (setq a1
				   (mul -1
					(sub a
					     b)))))
		  (inv (mul a1
			    (msin z1)
			    (mcos z1)))))
	    (t
	     nil)))))


;;Generates atan if arg positive else log
(defun trig-log-3-exec (arg-l1 arg-l2)
  (declare (ignore arg-l1 arg-l2))
  ;; See A&S 15.1.4 and 15.1.5
  ;;
  ;; F(a,b;3/2;z) where a = 1/2 and b = 1 (or vice versa).

  ;; I think it's ok to convert sqrt(z^2) to z here, so $radexpand is
  ;; $all.
  (let (($radexpand '$all))
    (cond ((equal (checksigntm var) '$positive)
	   ;; A&S 15.1.4
	   ;;
	   ;; F(1/2,1;3/2,z^2) =
	   ;;
	   ;; log((1+z)/(1-z))/z/2
	   (let ((z (power var (div 1 2))))
	     (mul (power z -1)
		  (inv 2)
		  (mlog (div (add 1 z)
			     (sub 1 z))))))
	  ((equal (checksigntm var) '$negative)
	   ;; A&S 15.1.5
	   ;;
	   ;; F(1/2,1;3/2,z^2) =
	   ;; atan(z)/z
	   (let ((z (power (mul -1 var)
			   (div 1 2))))
	     (mul (power z -1)
		  (matan z)))))))

(defun trig-log-3a-exec (arg-l1 arg-l2)
  ;; See A&S 15.1.6 and 15.1.7
  ;;
  ;; F(a,b;3/2,z) where a = b and a = 1/2 or a = 1.

  ;; I think it's ok to convert sqrt(z^2) to z here, so $radexpand is
  ;; $all.
  (let ((a (first arg-l1))
	($radexpand '$all))
    (cond ((equal (checksigntm var) '$positive)
	   ;; A&S 15.1.6
	   ;;
	   ;; F(1/2,1/2; 3/2; z^2) = sqrt(1-z^2)*F(1,1;3/2;z^2) =
	   ;; asin(z)/z
	   (let ((z (power var (div 1 2))))
	     (if (equal a 1)
		 (div (trig-log-3a-exec (list (div 1 2) (div 1 2)) arg-l2)
		      (power (sub 1 (power z 2)) (div 1 2)))
		 (div (masin z) z))))
	  ((equal (checksigntm var) '$negative)
	   ;; A&S 15.1.7
	   ;;
	   ;; F(1/2,1/2; 3/2; -z^2) = sqrt(1+z^2)*F(1,1,3/2; -z^2) =
	   ;;log(z + sqrt(1+z^2))/z
	   (let* ((z (power (mul -1 var)
			    (div 1 2)))
		  (1+z^2 (add 1 (power z 2))))
	     (if (equal a 1)
		 (div (trig-log-3a-exec (list (div 1 2) (div 1 2))
					arg-l2)
		      (power 1+z^2
			     (div 1 2)))
		 (div (mlog (add z (power 1+z^2
					  (div 1 2))))
		      z)))))))


(defun trig-log-1 (arg-l1 arg-l2)	;; 2F1's with C = 1/2
  (declare (ignore arg-l2))
  ;; 15.1.17, 11, 18, 12, 9, and 19

  ;; I think it's ok to convert sqrt(z^2) to z here, so $radexpand is
  ;; $all.
  (let (($radexpand '$all)
	x z $exponentialize a b)
    (setq a (car arg-l1) b (cadr arg-l1))
    (cond ((=0 (m+t a b))
	   ;; F(-a,a;1/2,z)

	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.17:
		  ;; F(-a,a;1/2;sin(z)^2) = cos(2*a*z)
		  (trig-log-1-pos a var))
		 ((equal (checksigntm var) '$negative)
		  ;; A&X 15.1.11:
		  ;; F(-a,a;1/2;-z^2) = 1/2*((sqrt(1+z^2)+z)^(2*a)
		  ;;                         +(sqrt(1+z^2)-z)^(2*a))
		  ;;
		  (trig-log-1-neg a b var))
		 (t ())))
	  ((equal (m+t a b) 1.)
	   ;; F(a,1-a;1/2,z)
	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.18:
		  ;; F(a,1-a;1/2;sin(z)^2) = cos((2*a-1)*z)/cos(z)
		  (m//t (mcos (m*t (m-t a b) (setq z (masin (msqrt var)))))
			(mcos z)))
		 ((equal (checksigntm var) '$negative)
		  ;; A&S 15.1.12
		  ;; F(a,1-a;1/2;-z^2) = 1/2*(1+z^2)^(-1/2)*
		  ;;                     {[(sqrt(1+z^2)+z]^(2*a-1)
		  ;;                       +[sqrt(1+z^2)-z]^(2*a-1)}
		  (m*t 1//2 (m//t (setq x (msqrt (m-t 1. var))))
		       (m+t (m^t (m+t x (setq z (msqrt (m-t var))))
				 (setq b (m-t a b)))
			    (m^t (m-t x z) b))))
		 (t ())))
	  ((=1//2 (hyp-mabs (m-t b a)))
	   ;; F(a, a+1/2; 1/2; z)
	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.9
		  ;; F(a,1/2+a;1/2;z^2) = ((1+z)^(-2*a)+(1-z)^(-2*a))/2
		  (m*t 1//2
		       (m+t (m^t (m1+t (setq z (msqrt var)))
				 (setq b (m-t 1//2 (m+t a b))))
			    (m^t (m-t 1. z) b))))
		 ((equal (checksigntm var) '$negative)
		  ;; A&S 15.1.19
		  ;; F(a,1/2+a;1/2;-tan(z)^2) = cos(z)^(2*a)*cos(2*a*z)
		  (m*t (m^t (mcos (setq z (matan (msqrt (m-t var)))))
			    (setq b (m+t a b -1//2)))
		       (mcos (m*t b z))))
		 (t ())))
	  (t ()))))

(defun trig-log-1-pos (a z)
  ;; I think it's ok to convert sqrt(z^2) to z here, so $radexpand is
  ;; $all.
  (let (($radexpand '$all))
    (mcos (m*t 2. a (masin (msqrt z))))))

(defun trig-log-1-neg (a b v)
  ;; Look to see a is of the form m*s+c where m and c
  ;; are numbers.  If m is positive, swap a and b.
  ;; Basically we want F(-a,a;1/2;-z^2) =
  ;; F(a,-a;1/2;-z^2), as they should be.
  (let* (($radexpand '$all)
	 (match (m*s+c a))
	 (m (cdras 'm match))
	 (s (cdras 's match))
	 (b (if s
		(if (and m (plusp m))
		    a
		    b)
		(if (eq (checksigntm a) '$negative)
		    b
		    a)))
	 (x (msqrt (m-t 1. v)))
	 (z (msqrt (m-t v))))
    (m*t 1//2
	 (m+t (m^t (m+t x z)
		   (setq b (m*t 2. b)))
	      (m^t (m-t x z) b)))))
  
;; Pattern match for m*s+c where a is a number, x is symbolic, and c
;; is a number.
(defun m*s+c (exp)
  (m2 exp
      '((mplus) ((coeffpt) (m $numberp) (s nonnump))
	        ((coeffpp) (c $numberp)))
      nil))

;; List L contains two elements first the numerator parameter that
;;exceeds the denumerator one and is called "C", second
;;the difference of the two parameters which is called "M". 

#||
(defun diffintprop-gen-exec (l l1 l2) 
  (prog (c m poly constfact ) 
     (setq c (car l) 
	   m (cadr l) 
	   l1 (zl-delete c l1 1.) 
	   c (sub c m)
	   l2 (zl-delete c l2 1.) 
	   poly ($expand (constrpoly c m 'avgoustis)) 
	   constfact (createconstfact c m))
     (return (yanmult constfact
		      (diffintprop-exec poly l1 l2))))) 

(defun constrpoly (c m k) 
  (cond ((zerop m) 1.)
	(t (mul (add c k (sub1 m)) (constrpoly c (sub1 m) k))))) 

(defun createconstfact (c m) 
  (cond ((zerop m) 1.)
	(t (mul (inv (add c (sub1 m)))
		(createconstfact c (sub1 m)))))) 

(defun diffintprop-exec (poly l1 l2) 
  (distrdiffintprop (createcoefpowlist-exec poly) l1 l2)) 

(defun distrdiffintprop (l l1 l2) 
  (cond ((null l) 0.)
	(t (add (yanmult ($factor (cadar l))
			 (diffintprop (caar l) l1 l2))
		(distrdiffintprop (cdr l) l1 l2))))) 

(defun diffintprop (pow l1 l2) 
  (cond ((zerop pow) (hgfsimp l1 l2 var))
	((equal pow 1.)
	 (yanmult (mul (div (multpl l1) (multpl l2)) var)
		  (hgfsimp (incr1 l1) (incr1 l2) var)))
	(t (searchaddserieslist pow l1 l2)))) 

(defun searchaddserieslist (pow l1 l2) 
  (prog (series res) 
     (cond ((setq series (searchserieslistp serieslist pow))
	    (return (eval series))))
     (setq 
      serieslist
      (append
       serieslist
       (list
	(list
	 pow
	 (setq res
	       '(yanmult (mul (div (multpl l1) (multpl l2))
			  var)
		 (diffintproprecurse (sub1 pow)
		  (incr1 l1)
		  (incr1 l2))))))))
     (return (eval res)))) 

(defun diffintproprecurse (pow l1 l2) 
  (prog (poly) 
     (setq poly
	   ($expand (power (add 'avgoustis 1.) pow)))
     (return (diffintprop-exec poly l1 l2)))) 

(defun multpl (l) 
  (cond ((null l) 1.) (t (mul (car l) (multpl (cdr l)))))) 

(defun createcoefpowlist-exec (poly) 
  (prog (hp conster) 
     (setq conster (consterminit poly 'avgoustis) 
	   hp ($hipow poly 'avgoustis))
     (return (append (list (list 0. conster))
		     (createcoefpowlist poly hp))))) 

(defun createcoefpowlist (poly hp) 
  (cond ((equal hp 1.)
	 (list (list 1. ($coeff poly 'avgoustis))))
	(t (append (createcoefpowlist poly (sub1 hp))
		   (list (list hp
			       ($coeff poly
				       (power 'avgoustis
					      hp)))))))) 

(defun consterminit (fun var) 
  (cond ((eq (caar fun) 'mplus) (consterm (cdr fun) var))
	(t (cond ((freevar fun) fun) (t 0.))))) 

(defun searchserieslistp (serieslist pow) 
  (cond ((null serieslist) nil)
	((equal (caar serieslist) pow) (cadar serieslist))
	(t (searchserieslistp (cdr serieslist) pow)))) 

(defun yanmult (a b) 
  (cond ((eq (caar b) 'mplus) (yanmul a (cdr b)))
	(t (mul a b)))) 

(defun yanmul (a b) 
  (cond ((null b) 0.)
	(t (add (mul a (car b)) (yanmul a (cdr b)))))) 

||#

(defun freevarpar (exp)
  (cond ((freevar exp) (freepar exp))
	(t nil)))

;; Why is this needed?
(setq *par* '$p)

;;(DEFUN FREEVAR (A) 
;;       (COND ((ATOM A) (NOT (EQ A VAR)))
;;	     ((ALIKE1 A VAR)NIL)
;;	     ((AND (NOT (ATOM (CAR A)))
;;		   (MEMQ 'ARRAY (CDAR A)))
;;	      (if (FREEVAR (CDR A))
;;		  T
;;		  (merror "`variable-of-integration-appeared-in-subscript'")))
;;	     (T (AND (FREEVAR (CAR A)) (FREEVAR (CDR A))))))

(defun freepar (exp)
  (cond ((atom exp)
	 (not (eq exp *par*)))
	(t (and (freepar (car exp))
		(freepar (cdr exp))))))

(defun haspar (exp)
  (cond ((freepar exp) nil)
	(t t)))

;; Confluent hypergeometric function.
;;
;; F(a;c;z)
(defun confl (arg-l1 arg-l2 var)
  (let* ((a (car arg-l1))
	 (c (car arg-l2))
	 (a-c (sub a c)))
    (cond ((alike1 c (add a a))
	   ;; F(a;2a;z)
	   ;; A&S 13.6.6
	   ;;
	   ;; F(n+1;2*n+1;2*z) =
	   ;; gamma(3/2+n)*exp(z)*(z/2)^(-n-1/2)*bessel_i(n+1/2,z).
	   ;;
	   ;; So
	   ;;
	   ;; F(n,2*n,z) =
	   ;; gamma(n+1/2)*exp(z/2)*(z/4)^(-n-3/2)*bessel_i(n-1/2,z/2);
	   (let ((z (div var 2)))
	     (mul (power '$%e z)
		  (bestrig (add a (inv 2))
			   (div (mul z z) 4)))))
	  ((not (hyp-integerp a-c))
	   (cond ((hyp-integerp a)
		  (kummer arg-l1 arg-l2))
		 ($prefer_whittaker
		  ;; A&S 15.1.32:
		  ;;
		  ;; %m[k,u](z) = exp(-z/2)*z^(u+1/2)*M(1/2+u-k,1+2*u,z)
		  ;;
		  ;; So
		  ;;
		  ;; M(a,c,z) = exp(z/2)*z^(-c/2)*%m[c/2-a,c/2-1/2](z)
		  ;;
		  ;; But if we apply Kummer's transformation, we can also
		  ;; derive the expression
		  ;;
		  ;; %m[k,u](z) = exp(z/2)*z^(u+1/2)*M(1/2+u+k,1+2*u,-z)
		  ;;
		  ;; or
		  ;;
		  ;; M(a,c,z) = exp(-z/2)*(-z)^(-c/2)*%m[a-c/2,c/2-1/2](-z)
		  ;;
		  ;; For Laplace transforms it might be more beneficial to
		  ;; return this second form instead.
		  (let* ((m (div (sub c 1) 2))
			 (k (add (inv 2) m (mul -1 a))))
		    (mul (power var (mul -1 (add (inv 2) m)))
			 (power '$%e (div var 2))
			 (whitfun k m var))))
			 
		 (t
		  (fpqform arg-l1 arg-l2 var))))
	  ((minusp a-c)
	   (erfgammared a c var))
	  (t
	   (kummer arg-l1 arg-l2)))))

;; A&S 13.6.19:
;; M(1/2,3/2,-z^2) =  sqrt(%pi)*erf(z)/2/sqrt(z)
;;
;; So M(1/2,3/2,z) = sqrt(%pi)*erf(sqrt(-z))/2/sqrt(-z)
;;                 = sqrt(%pi)*erf(%i*sqrt(z))/2/(%i*sqrt(z))
(defun hyprederf (x)
  (let ((x (mul '$%i (power x (inv 2)))))
    (mul (power '$%pi (inv 2))
	 (inv 2)
	 (inv x)
	 (list '(%erf) x))))

;; M(a,c,z), where a-c is a negative integer.
(defun erfgammared (a c z)
  (cond ((and (nump a) (nump c))
	 (erfgamnumred a c z))
	(t (gammareds a c z))))

;; M(a,c,z) where a-c is a negative integer, and at least one of a or
;; c is not a number.
#+nil
(defun gammareds (a c z)
  (prog (m numprod result count atemp)
     (setq m (sub c a))
     ;; m = c - a
     (cond ((eq m 1)
	    ;; We have M(a,a+1,z)
	    (return (hypredincgm a z))))
     (setq numprod (prod a m)
	   count 2
	   atemp a
	   result (sub (mul 2
			    numprod
			    (inv atemp)
			    (hypredincgm atemp z))
		       (mul 2
			    numprod
			    (inv (setq atemp (add atemp 1)))
			    (hypredincgm atemp z))))
     loop
     (cond ((eq count m)(return result)))
     (setq count (add1 count)
	   atemp (add atemp 1)
	   result (add result
		       (mul (power -1 count)
			    (inv (factorial (sub m
						 (sub1 count))))
			    numprod
			    (inv atemp)
			    (hypredincgm atemp z))))
     (go loop)))

;; I (rtoy) think this is what the function above is doing, but I'm
;; not sure.  Plus, I think it's wrong.
;;
;; For hgfred([n],[2+n],-z), the above returns
;;
;; 2*n*(n+1)*z^(-n-1)*(%gammagreek(n,z)*z-%gammagreek(n+1,z))
;;
;; But from A&S 13.4.3
;;
;; -M(n,2+n,z) - n*M(n+1,n+2,z) + (n+1)*M(n,n+1,z) = 0
;;
;; so M(n,2+n,z) = (n+1)*M(n,n+1,z)-n*M(n+1,n+2,z)
;;
;; And M(n,n+1,-z) = n*z^(-n)*%gammagreek(n,z)
;;
;; This gives
;;
;; M(n,2+n,z) = (n+1)*n*z^(-n)*%gammagreek(n,z) - n*(n+1)*z^(-n-1)*%gammagreek(n+1,z)
;;            = n*(n+1)*z^(-n-1)*(%gammagreek(n,z)*n-%gammagreek(n+1,z))
;;
;; So the version above is off by a factor of 2.  But I think it's more than that.
;; Using A&S 13.4.3 again,
;;
;; M(n,n+3,-z) = [n*M(n+1,n+3,-z) - (n+2)*M(n,n+2,-z)]/(-2);
;;
;; The version above doesn't produce anything like this equation would
;; produce, given the value of M(n,n+2,-z) derived above.
(defun gammareds (a c z)
  ;; M(a,c,z) where a-c is a negative integer.
  (let ((diff (sub c a)))
    (cond ((eql diff 1)
	   ;; We have M(a,a+1,z).
	   (hypredincgm a z))
	  ((eql a 1)
	   ;; We have M(1,a,z)
	   ;; Apply Kummer's tranformation to get the form M(a-1,a,z)
	   ;;
	   ;; (I don't think we ever get here, but just in case, we leave it.)
	   (let ((var z))
	     (kummer (list a) (list c))))
	  (t
	   ;; We have M(a, a+n, z)
	   ;;
	   ;; A&S 13.4.3 says
	   ;; (1+a-b)*M(a,b,z) - a*M(a+1,b,z)+(b-1)*M(a,b-1,z) = 0
	   ;;
	   ;; So
	   ;;
	   ;; M(a,b,z) = [a*M(a+1,b,z) - (b-1)*M(a,b-1,z)]/(1+a-b);
	   ;;
	   ;; Thus, the difference between b and a is reduced, until
	   ;; b-a=1, which we handle above.
	   (mul (sub (mul a
			  (gammareds (add 1 a) c z))
		     (mul (sub c 1)
			  (gammareds a (sub c 1) z)))
		(inv (sub (add 1 a) c)))))))

;; A&S 6.5.12: 
;; %gammagreek(a,x) = x^a/a*M(a,1+a,-x)
;;                  = x^a/a*exp(-x)*M(1,1+a,x)
;;
;; where %gammagreek(a,x) is the incomplete gamma function.
;;
;; M(a,1+a,x) = a*(-x)^(-a)*%gammagreek(a,-x)
#+nil
(defun hypredincgm
    (a z)
  (prog()
     (setq z (mul -1 z))
     (return (mul a
		  (power z (mul -1 a))
		  (list '($%gammagreek) a z)))))

(defun hypredincgm (a z)
  (let ((-z (mul -1 z)))
    (mul a (power -z (mul -1 a))
	 `(($%gammagreek) ,a ,-z))))

;; M(a,c,z), when a and c are numbers, and a-c is a negative integer
(defun erfgamnumred (a c z)
  (cond ((hyp-integerp (sub c (inv 2)))
	 (erfred a c z))
	(t (gammareds a c z))))

;; M(a,c,z) when a and c are numbers and c-1/2 is an integer and a-c
;; is a negative integer.  Thus, we have M(p+1/2, q+1/2,z)
(defun erfred (a c z)
  (prog (n m)
     (setq n (sub a (inv 2))
	   m (sub c (div 3 2)))
     ;; a = n + 1/2
     ;; c = m + 3/2
     ;; a - c < 0 so n - m - 1 < 0
     (cond ((not (or (greaterp n m) (minusp n)))
	    ;; 0 <= n <= m
	    (return (thno33 n m z))))
     (cond ((and (minusp n) (minusp m))
	    ;; n < 0 and m < 0
	    (return (thno35 (mul -1 n) (mul -1 m) z))))
     (cond ((and (minusp n) (plusp m))
	    ;; n < 0 and m > 0
	    (return (thno34 (mul -1 n) m z))))
     ;; n = 0 or m = 0
     (return (gammareds (add n (inv 2))
			(add m (div 3 2))
			z))))
;; Compute M(n+1/2, m+3/2, z) with 0 <= n <= m.
;;
;; I (rtoy) think this is what this routine is doing.  (I'm guessing
;; that thno33 means theorem number 33 from Yannis Avgoustis' thesis.)
;;
;; I don't have his thesis, but I see there are similar ways to derive
;; the result we want.
;;
;; Method 1:
;;   Use Kummer's transformation (A&S ) to get
;;
;;     M(n+1/2,m+3/2,z) = exp(z)*M(m-n+1,m+3/2,-z)
;;
;;   From A&S, we have
;;
;;     diff(M(1,n+3/2,z),z,m-n) = poch(1,m-n)/poch(n+3/2,m-n)*M(m-n+1,m+3/2,z)
;;
;;   Apply Kummer's transformation again:
;;
;;     M(1,n+3/2,z) = exp(z)*M(n+1/2,n+3/2,-z)
;;
;;   Apply the differentiation formula again:
;;
;;     diff(M(1/2,3/2,z),z,n) = poch(1/2,n)/poch(3/2,n)*M(n+1/2,n+3/2,z)
;;
;;   And we know that M(1/2,3/2,z) can be expressed in terms of erf.
;;
;; Method 2:
;;
;;   Since n <= m, apply the differentiation formula:
;;
;;     diff(M(1/2,m-n+3/2,z),z,n) = poch(1/2,n)/poch(m-n+3/2,n)*M(n+1/2,m+3/2,z)
;;
;;   Apply Kummer's transformation:
;;
;;     M(1/2,m-n+3/2,z) = exp(z)*M(m-n+1,m-n+3/2,z)
;;
;;   Apply the differentiation formula again:
;;
;;     diff(M(1,3/2,z),z,m-n) = poch(1,m-n)/poch(3/2,m-n)*M(m-n+1,m-n+3/2,z)
;;
;; I think this routine uses Method 2.
#+nil
(defun thno33
    (n m x)
  ((lambda(m-n)
     (subst x
	    'yannis
	    (mul (div (mul (power -1 m-n)
			   (fctrl (div 3 2) m-n)
			   (fctrl (add m-n
				       (div 3 2))
				  n))
		      (mul (fctrl 1 m-n)
			   (fctrl (inv 2) n)))
		 ;; diff(M(1/2,m-n+3/2,z),z,n)
		 (meval (list '($diff)
			      ;; Kummer's transformation
			      (mul (power '$%e
					  'yannis)
				   ;; diff(M(1,3/2,z),z,m-n)
				   (meval (list '($diff)
						;; M(1,3/2,-z) = e^(-z)*M(1/2,3/2,z)
						(mul
						 (power
						  '$%e
						  (mul
						   -1
						   'yannis))
						 (hyprederf
						  'yannis))
						'yannis
						m-n)))
			      'yannis
			      n)))))
   (sub m n)))

(defun thno33 (n m x)
  ;; M(n+1/2,m+3/2,z) = diff(M(1/2,m-n+3/2,z),z,n)*poch(m-n+3/2,n)/poch(1/2,n)
  ;; M(1/2,m-n+3/2,z) = exp(z)*M(m-n+1,m-n+3/2,-z)
  ;; M(m-n+1,m-n+3/2,z) = diff(M(1,3/2,z),z,m-n)*poch(3/2,m-n)/poch(1,m-n)
  ;; diff(M(1,3/2,z),z,m-n) = (-1)^(m-n)*diff(M(1,3/2,-z),z,m-n)
  ;; M(1,3/2,-z) = exp(-z)*M(1/2,3/2,z)
  (let* ((m-n (sub m n))
	 ;; poch(m-n+3/2,n)/poch(1/2,n)
	 (factor1 (div (fctrl (add m-n (div 3 2)) n)
		       (fctrl (inv 2) n)))
	 ;; poch(3/2,m-n)/poch(1,m-n)
	 (factor2 (div (fctrl (div 3 2) m-n)
		       (fctrl 1 m-n)))
	 ;; M(1,3/2,-z) = exp(-z)*M(1/2,3/2,z)
	 (hgferf (mul (power '$%e (mul -1 'yannis))
		      (hyprederf 'yannis)))
	 ;; diff(M(1,3/2,z),z,m-n)
	 (diff1 (meval `(($diff) ,hgferf 'yannis ,m-n)))
	 ;; exp(z)*M(m-n+1,m-n+3/2,-z)
	 (kummer (mul (power '$%e 'yannis)
		      diff1))
	 ;; diff(M(1/2,m-n+3/2,z),z,n)
	 (diff2 (meval `(($diff) ,kummer 'yannis ,n))))
    ;; Multiply all the terms together.
    (mul (power -1 m-n)
	 factor1
	 factor2
	 (subst x 'yannis diff2))))

;; M(n+1/2,m+3/2,z), with n < 0 and m > 0
;;
;; Let's write it more explicitly as M(-n+1/2,m+3/2,z) with n > 0 and
;; m > 0.
;;
;; First, use Kummer's transformation to get
;;
;;    M(-n+1/2,m+3/2,z) = exp(z)*M(m+n+1,m+3/2,-z)
;;
;; We also have
;;
;;    diff(z^(n+m)*M(m+1,m+3/2,z),z,n) = poch(m+1,n)*z^m*M(m+n+1,m+3/2,z)
;;
;; And finally
;;
;;    diff(M(1,3/2,z),z,m) = poch(1,m)/poch(3/2,m)*M(m+1,m+3/2,z)
;;
;; Thus, we can compute M(-n+1/2,m+3/2,z) from M(1,3/2,z).
;;
;; The second formula above can be derived easily by multiplying the
;; series for M(m+1,m+3/2,z) by z^(n+m) and differentiating n times.
;;
(defun thno34 (n m x)
  (subst x
	 'yannis
	 (mul (power -1 m)
	      (div (mul (fctrl (div 3 2) m)
			(power '$%e 'yannis))
		   (mul (fctrl 1 m)
			(fctrl (add1 m) n)
			(power 'yannis m)))
	      (meval (list '($diff)
			   (mul (power 'yannis
				       (plus m n))
				(meval (list '($diff)
					     (mul (power '$%e
							 (mul
							  -1
							  'yannis))
						  (hyprederf 'yannis))
					     'yannis
					     m)))
			   'yannis
			   n)))))

;; M(n+1/2,m+3/2,z), with n < 0 and m < 0
;;
;; Write it more explicitly as M(-n+1/2,-m+3/2,z) with n > 0 and m >
;; 0.
;;
;; We know that
;;
;;    diff(sqrt(z)*M(-n+1/2,3/2,z),z,m) = poch(3/2-m,m)*M(-n+1/2,-m+3/2,z).
;;
;; Apply Kummer's transformation:
;;
;;    M(-n+1/2,3/2,z) = exp(z) * M(n+1,3/2,-z)
;;
;; Finally
;;
;;    diff(z^n*M(1,3/2,z),z,n) = n!*M(n+1,3/2,z)
;;
;; So we can express M(-n+1/2,-m+3/2,z) in terms of M(1,3/2,z).
;;
;; The first formula above follows from the more general formula
;;
;;    diff(z^(b-1)*M(a,b,z),z,n) = poch(b-n,n)*z^(b-n-1)*M(a,b-n,z)
;;
;; The last formula follows from the general result
;;
;;    diff(z^(a+n-1)*M(a,b,z),z,n) = poch(a,n)*z^(a-1)*M(a+n,b,z)
;;
;; Both of these are easily derived by using the series for M and
;; differentiating.
(defun thno35 (n m x)
  (subst x
	 'yannis
	 (mul (div (power 'yannis (sub m (inv 2)))
		   (mul (power -1 (times -1 m))
			(fctrl 1 n)
			(fctrl (inv -2) m)))
	      (meval (list '($diff)
			   (mul (power 'yannis (inv 2))
				(power '$%e 'yannis)
				(meval (list '($diff)
					     (mul (power '$%e
							 (mul
							  -1
							  'yannis))
						  (power 'yannis
							 n)
						  (hyprederf 'yannis))
					     'yannis
					     n)))
			   'yannis
			   m)))))

;; Pochhammer symbol. fctrl(a,n) = a*(a+1)*(a+2)*...*(a+n-1).
;;
;; N must be a positive integer!
;;
;; FIXME:  This appears to be identical to factf below.
(defun fctrl (a n)
  (cond ((zerop n)
	 1)
	((equal n 1)
	 a)
	(t
	 (mul (add a (sub1 n))
	      (fctrl a (sub1 n))))))



(setq *par* '$p)                           

(defun vfvp (exp)
  (m2 exp '(v freevarpar) nil))


(defun d*u
    (exp)
  (m2 exp
      '((mtimes)((coefftt)(d freepar))((coefftt)(u haspar)))
      nil))

(defun fpqform (arg-l1 arg-l2 arg)
  (list '(mqapply)
	(list '($%f array) (length arg-l1)(length arg-l2))
	(append (list '(mlist)) arg-l1)
	(append (list '(mlist)) arg-l2)
	arg))



;; Consider pFq([a_k]; [c_j]; z).  If a_k = c_j + m for some k and j
;; and m >= 0, we can express pFq in terms of (p-1)F(q-1).
;;
;; Here is a derivation for F(a,b;c;z), but it generalizes to the
;; generalized hypergeometric very easily.
;;
;; From A&s 15.2.3:
;;
;; diff(z^(a+n-1)*F(a,b;c;z), z, n) = poch(a,n)*z^(a-1)*F(a+n,b;c;z)
;;
;; F(a+n,b;c;z) = diff(z^(a+n-1)*F(a,b;c;z), z, n)/poch(a,n)/z^(a-1)
;;
;;
;; So this expresses F(a+n,b;c;z) in terms of F(a,b;c;z).  Let a = c +
;; n.  This therefore gives F(c+n,b;c;z) in terms of F(c,b;c;z) =
;; 1F0(b;;z), which we know.
;;
;; For simplicity, we will write F(z) for F(a,b;c;z).
;;
;; Now,
;;
;;                       n
;; diff(z^x*F(z),z,n) = sum binomial(n,k)*diff(z^x,z,n-k)*diff(F(z),z,k)
;;                      k=0
;;
;; But diff(z^x,z,n-k) = x*(x-1)*...*(x-n+k+1)*z^(x-n+k)
;;                     = poch(x-n+k+1,n-k)*z^(x-n+k)
;;
;; so
;; 
;; z^(-a+1)/poch(a,n)*diff(z^(a+n-1),z,n-k)
;;    = poch(a+n-1-n+k+1,n-k)/poch(a,n)*z^(a+n-1-n+k)*z^(-a+1)
;;    = poch(a+k,n-k)/poch(a,n)*z^k
;;    = z^k/poch(a,k)
;;
;; Combining these we have
;;
;;                 n
;; F(a+n,b;c;z) = sum z^k/poch(a,k)*binomial(n,k)*diff(F(a,b;c;z),z,k)
;;                k=0
;;
;; Since a = c, we have
;;
;;                 n
;; F(a+n,b;a;z) = sum z^k/poch(a,k)*binomial(n,k)*diff(F(a,b;a;z),z,k)
;;                k=0
;;
;; But F(a,b;a;z) = F(b;;z) and it's easy to see that A&S 15.2.2 can
;; be specialized to this case to give
;;
;; diff(F(b;;z),z,k) = poch(b,k)*F(b+k;;z)
;;
;; Finally, combining all of these, we have
;;
;;                 n
;; F(a+n,b;c;z) = sum z^k/poch(a,k)*binomial(n,k)*poch(b,k)*F(b+k;;z)
;;                k=0
;;
;; Thus, F(a+n,b;c;z) is expressed in terms of 1F0(b+k;;z), as desired.
(defun splitpfq (l arg-l1 arg-l2)
  (destructuring-bind (a1 k)
      l
    (let* ((result 0)
	   (prodnum 1)
	   (proden 1)
	   (b1 (sub a1 k))
	   (prod-b 1)
	   (arg-l1 (zl-delete a1 arg-l1 1))
	   (arg-l2 (zl-delete b1 arg-l2 1)))
      (loop for count from 0 upto k
	 do
	   (when $trace2f1
	     (format t "splitpfg term:~%")
	     (maxima-display (mul (combin k count)
				  (div prodnum proden)
				  (inv prod-b)
				  (power var count)))
	     (format t "F(~A, ~A)~%" arg-l1 arg-l2))
	 (setq result (add result
			   (mul (combin k count)
				(div prodnum proden)
				(inv prod-b)
				(power var count)
				(hgfsimp arg-l1 arg-l2 var))))
	 (setq prod-b (mul prod-b (add b1 count)))
	 (setq prodnum (mul prodnum (mull arg-l1))
	       proden (mul proden (mull arg-l2)))
	 (setq arg-l1 (incr1 arg-l1))
	 (setq arg-l2 (incr1 arg-l2)))
      result)))

;; binomial(k,count)
(defun combin (k count)
  (div (factorial k)
       (mul (factorial count)
	    (factorial (sub k count)))))


;; Algor. II from thesis:minimizes differentiations
;;
;; We're looking at F(a+m,-a+n;1/2+L;z)
#+nil
(defun algii (a b c)
  (declare (ignore c))
  (prog (m n ap con sym m+n)
     ;; We know a+b is an integer.  In the most general form, we can
     ;; have a = r*s+f+m and b = -(r*s+f)+n.
     (cond ((not (setq sym (cdras 'f (s+c a))))
	    (setq sym 0)))
     (setq con (sub a sym))
     (setq ap sym)
     (setq m+n (add a b))
     (setq m ($entier con))
     (when (minusp m)
       (add1 m))
     ;; At this point sym = r*s, con is f+m, and m is m.
     (setq ap (add (sub con m) ap))
     ;; ap = r*a+f
     (setq n (add b ap))
     ;; Return r*a+f, r*a+f+p, and m+n, where p is chosen to minimize
     ;; the number of derivatives we need to take.  Basically
     ;; p=min(abs(m),abs(n)).
     ;;
     ;; So we have changed F(a+m,-a+n;c;z) to F(a',-a'+m+n;c;z).
     (cond ((and (minusp (mul n m))
		 (greaterp (abs m) (abs n)))
	    (return (list ap (sub ap n) m+n))))
     (return  (list ap (add ap m) m+n))))

;; F(a,b;c;z), where a + b is a (numerical) integer.
;;
;; If we're here, a and b are not integers.  In general, a = s+f+m,
;; where s is symbolic stuff and |f|<1 and m is an integer.  We can
;; also write b = -(s+f)+n.
;;
;; Let a' = s+f+m.  Then a=a' and b = -a'+m+n, and we have converted
;; F(a,b;c;z) to F(a',-a'+m+n;c;z), or, equivalently,
;; F(-a'+m+n,a';c;z).
#+nil
(defun algii (a b c)
  (declare (ignore c))
  (prog (m n ap con sym m+n)
     ;; We know a+b is an integer.  In the most general form, we can
     ;; have a = x+f+m and b = -(x-f)+n.
     ;;
     ;; Express a in the form sym + con.
     (cond ((not (setq sym (cdras 'f (s+c a))))
	    (setq sym 0))
	   (t
	    ;; a is of the form s+c.  Look at the coefficient of s.
	    ;; If it's negative, swap a and b.
	    (let ((res (m2 sym '((mtimes) ((coefft) (m $numberp)) ((coefft) (s nonnump)))
			   nil)))
	      (when (and res (minusp (cdras 'm res)))
		(rotatef a b)
		(setf sym (cdras 'f (s+c a)))))))
     (setq con (sub a sym))
     (setq ap sym)
     (setq m+n (add a b))
     ;; Truncate con to an integer m.
     (setq m ($entier con))
     (when (minusp m)
       (add1 m))
     ;; Make ap = s+con-m
     (setq ap (add (sub con m) ap))
     (setq n (add b ap))
     ;; Return r*a+f, r*a+f+p, and m+n, where p is chosen to minimize
     ;; the number of derivatives we need to take.  Basically
     ;; p=min(abs(m),abs(n)).
     ;;
     ;; Hmm, not sure why we do this.  In any case, we need to do m+n
     ;; differentiations.  So the only simplification we could do is
     ;; make a' simpler.
     (cond ((and (minusp (mul n m))
		 (greaterp (abs m) (abs n)))
	    (return (list ap (sub ap n) m+n m n))))
     (return  (list ap (add ap m) m+n m n))))

;; We have something like F(s+m,-s+n;c;z)
;; Rewrite it like F(a'+d,-a';c;z) where a'=s-n=-b and d=m+n.
;;
(defun algii (a b)
  (let* ((sym (cdras 'f (s+c a)))
	 (sign (cdras 'm (m2 sym '((mtimes) ((coefft) (m $numberp)) ((coefft) (s nonnump)))
			     nil))))
    (when (and sign (minusp sign))
      (rotatef a b))
    (list nil (mul -1 b) (add a b))))


;;Algor. 2F1-RL from thesis:step 4:dispatch on a+m,-a+n,1/2+l cases
(defun step4 (a b c)
  ;; F(a,b;c;z) where a+b is an integer and c+1/2 is an integer.  If a
  ;; and b are not integers themselves, we can derive the result from
  ;; F(a1,-a1;1/2;z).  However, if a and b are integers, we can't use
  ;; that because F(a1,-a1;1/2;z) is a polynomial.  We need to derive
  ;; the result from F(1,1;3/2;z).
  (if (and (hyp-integerp a)
	   (hyp-integerp b))
      (step4-int a b c)
      (step4-a a b c)))

(defun step4-a (a b c)
  (let* ((alglist (algii a b))
	 (aprime (cadr alglist))
	 (m (caddr alglist))
	 (n (sub c (inv 2)))
	 ($ratsimpexpons $true)
	 ($ratprint $false))
    ;; At this point, we have F(a'+m,-a';1/2+n;z) where m and n are
    ;; integers.
    (cond ((hyp-integerp (add aprime (inv 2)))
	   ;; Ok.  We have a problem if aprime + 1/2 is an integer.
	   ;; We can't always use the algorithm below because we have
	   ;; F(1/2,-1/2;1/2;z) which is 1F0(-1/2;;z) so the
	   ;; derivation isn't quite right.  Also, sometimes we'll end
	   ;; up with a division by zero.
	   ;;
	   ;; Thus, We need to do something else.  So, use A&S 15.3.3
	   ;; to change the problem:
	   ;;
	   ;; F(a,b;c;z) = (1-z)^(c-a-b)*F(c-a, c-b; c; z)
	   ;;
	   ;; which is
	   ;;
	   ;; F('a+m,-a';1/2+n;z) = (1-z)^(1/2+n-m)*F(1/2+n-a'-m,1/2+n+a';1/2+n;z)
	   ;;
	   ;; Recall that a' + 1/2 is an integer.  Thus we have
	   ;; F(<int>,<int>,1/2+n;z), which we know how to handle in
	   ;; step4-int.
	   (gered1 (list a b) (list c) #'hgfsimp))
	  (t
	   (let ((newf 
		  (cond ((equal (checksigntm var) '$positive)
			 (trig-log-1-pos aprime 'ell))
			((equal (checksigntm var) '$negative)
			 (trig-log-1-neg (mul -1 aprime) aprime 'ell)))))
	     ;; Ok, this uses F(a,-a;1/2;z).  Since there are 2 possible
	     ;; representations (A&S 15.1.11 and 15.1.17), we check the sign
	     ;; of the var (as done in trig-log-1) to select which form we
	     ;; want to use.  The original didn't and seemed to want to use
	     ;; the negative form.
	     ;;
	     ;; With this change, F(a,-a;3/2;z) matches what A&S 15.2.6 would
	     ;; produce starting from F(a,-a;1/2;z), assuming z < 0.
    
	     (subst var 'ell
		    (algiii newf
			    m n aprime)))))))

;; F(a,b;c;z), where a and b are (positive) integers and c = 1/2+l.
;; This can be computed from F(1,1;3/2;z).
;;
;; Assume a < b, without loss of generality.
;;
;; F(m,n;3/2+L;z), m < n.
;;
;; We start from F(1,1;3/2;z).  Use A&S 15.2.3, differentiating m
;; times to get F(m,1;3/2;z).  Swap a and b to get F(m,1;3/2;z) =
;; F(1,m;3/2;z) and use A&S 15.2.3 again to get F(n,m;3/2;z) by
;; differentiating n times.  Finally, if L < 0, use A&S 15.2.4.
;; Otherwise use A&S 15.2.6.
;;
;; I (rtoy) can't think of any way to do this with less than 3
;; differentiations.
;;
;; Note that if d = (n-m)/2 is not an integer, we can write F(m,n;c;z)
;; as F(-d+u,d+u;c;z) where u = (n+m)/2.  In this case, we could use
;; step4-a to compute the result.


;; Transform F(a,b;c;z) to F(a+n,b;c;z), given F(a,b;c;z)
(defun as-15.2.3 (a b c n arg fun)
  (declare (ignore b c))
  (assert (>= n 0))
  ;; A&S 15.2.3:
  ;; F(a+n,b;c;z) = z^(1-a)/poch(a,n)*diff(z^(a+n-1)*fun,z,n)
  (mul (inv (factf a n))
       (power arg (sub 1 a))
       ($diff (mul (power arg (sub (add a n) 1))
		   fun)
	      arg n)))

;; Transform F(a,b;c;z) to F(a,b;c-n;z), given F(a,b;c;z)
(defun as-15.2.4 (a b c n arg fun)
  (declare (ignore a b))
  (assert (>= n 0))
  ;; A&S 15.2.4
  ;; F(a,b;c-n;z) = 1/poch(c-n,n)/z^(c-n-1)*diff(z^(c-1)*fun,z,n)
  (mul (inv (factf (sub c n) n))
       (inv (power arg (sub (sub c n) 1)))
       ($diff (mul (power arg (sub c 1))
		   fun)
	      arg n)))

;; Transform F(a,b;c;z) to F(a-n,b;c;z), given F(a,b;c;z)
(defun as-15.2.5 (a b c n arg fun)
  ;; A&S 15.2.5
  ;; F(a-n,b;c;z) = 1/poch(c-a,n)*z^(1+a-c)*(1-z)^(c+n-a-b)
  ;;                 *diff(z^(c-a+n-1)*(1-z)^(a+b-c)*F(a,b;c;z),z,n)
  (assert (>= n 0))
  (mul (inv (factf (sub c a) n))
       (power arg (sub (add a 1) c))
       (power (sub 1 arg)
	      (sub (add c n) (add a b)))
       ($diff (mul (power arg (sub (add c n)
				   (add a 1)))
		   (power (sub 1 arg)
			  (sub (add a b) c))
		   fun)
	      arg n)))

;; Transform F(a,b;c;z) to F(a,b;c+n;z), given F(a,b;c;z)
(defun as-15.2.6 (a b c n arg fun)
  ;; A&S 15.2.6
  ;; F(a,b;c+n;z) = poch(c,n)/poch(c-a,n)/poch(c-b,n)*(1-z)^(c+n-a-b)
  ;;                 *diff((1-z)^(a+b-c)*fun,z,n)
  (assert (>= n 0))
  (mul (factf c n)
       (inv (factf (sub c a) n))
       (inv (factf (sub c b) n))
       (inv (power (sub 1 arg) (sub (add a b)
				    (add c n))))
       ($diff (mul (power (sub 1 arg) (sub (add a b) c))
		   fun)
	      arg n)))

(defun step4-int (a b c)
  (if (> a b)
      (step4-int b a c)
      (let* ((s (gensym "STEP4_VAR_"))
	     (m (1- a))
	     (n (1- b))
	     (ell (sub c 3//2))
	     (res (cond ((eq (checksigntm var) '$negative)
			 ;; F(1,1;3/2;z) =
			 ;; -%i*log(%i*sqrt(zn)+sqrt(1-zn))/(sqrt(1-zn)*sqrt(zn))
			 ;; for z < 0
			 (let ((root1-z (power (sub 1 s) (inv 2)))
			       (rootz (power s (inv 2))))
			   (mul -1 '$%i
				(mlog (add (mul '$%i rootz)
					   root1-z))
				(inv root1-z)
				(inv rootz))))
			(t
			 ;; F(1,1;3/2;z) = asin(sqrt(zp))/(sqrt(1-zp)*sqrt(zp))
			 ;; for z > 0
			 (let ((root1-z (power (sub 1 s) (inv 2)))
			       (rootz (power s (inv 2))))
			   (mul (masin rootz)
				(inv root1-z)
				(inv rootz)))))))
	;; Start with res = F(1,1;3/2;z).  Compute F(m,1;3/2;z)
	(setf res (as-15.2.3 1 1 3//2 m s res))
	;; We now have res = C*F(m,1;3/2;z).  Compute F(m,n;3/2;z)
	(setf res (as-15.2.3 1 a 3//2 n s res))
	;; We now have res = C*F(m,n;3/2;z).  Now compute F(m,n;3/2+ell;z):
	(subst var s
	       (cond ((minusp ell)
		      (as-15.2.4 a b 3//2 (- ell) s res))
		     (t
		      (as-15.2.6 a b 3//2 ell s res)))))))

;;Pattern match for s(ymbolic) + c(onstant) in parameter
(defun s+c (exp)
  (m2 exp
      '((mplus) ((coeffpt)(f nonnump)) ((coeffpp)(c $numberp)))
      nil))

(defun nonnump (z)
  (cond ((not ($numberp z)) t)
	(t nil)))

;;Algor. III from thesis:determines which Differ. Formula to use
(defun algiii (fun m n aprime)
  (let ((mm (abs m))
	(nn (abs n)))
    (cond ((and (nni m) (nni n))
	   (cond ((lessp m n)
		  (f81 fun m n aprime))
		 (t
		  (f85 fun mm nn aprime))))
	  ((and (hyp-negp n) (hyp-negp m))
	   (cond ((greaterp (abs m) (abs n))
		  (f86 fun mm nn aprime))
		 (t
		  (f82 fun mm nn aprime))))
	  ((and (hyp-negp m) (nni n))
	   (f83 fun mm nn aprime))
	  (t
	   (f84 fun mm nn aprime)))))

;; Factorial function:x*(x+1)*(x+2)...(x+n-1)
;;
;; FIXME:  This appears to be identical to fctrl above
(defun factf (x n)
  (cond ((zerop n) 1)
	(t (mul x (factf (add x 1) (sub n 1))))))

;;Formula  #85 from Yannis thesis:finds by differentiating F[2,1](a,b,c,z)
;; given F[2,1](a+m,b,c+n,z) where b=-a and c=1/2, n,m integers

;; Like F81, except m > n.
;;
;; F(a+m,-a;c+n;z), m > n, c = 1/2, m and n are non-negative integers
;;
;; A&S 15.2.3
;; diff(z^(a+m-n-1)*F(a,-a;1/2;z),z,m-n) = poch(a,m-n)*z^(a-1)*F(a+m-n,-a;1/2;z)
;;
;; A&S 15.2.7
;; diff((1-z)^(a+m-1)*F(a+m-n,-a;1/2;z),z,n)
;;     = (-1)^n*poch(a+m-n,n)*poch(1/2+a,n)/poch(1/2,n)*(1-z)^(a+m-n-1)
;;         * F(a+m,-a;1/2+n;z)
;;
(defun f85 (fun m n a)
  (mul (factf (inv 2) n)
       (inv (power -1 n))
       (inv (factf (sub (add a m)
			n)
		   n))
       (inv (factf (sub (inv 2)
			(mul a -1))
		   n))
       (inv (factf a (- m n)))
       (power (sub 1 'ell) (sub (sub (add 1 n) m) a))
       ($diff (mul (power (sub 1 'ell) (sub (add a m) 1))
		   (power 'ell (sub 1 a))
		   ($diff (mul (power 'ell (sub (add a m -1) n))
			       fun)
			  'ell (- m n)))
	      'ell n)))

;;Used to find negative things that are not integers,eg RAT's	
(defun hyp-negp (x)
  (cond ((equal (asksign x) '$negative)
	 t)
	(t nil)))

;; F(a,-a+m; c+n; z) where m,n are non-negative integers, m < n, c = 1/2.
;;
;; A&S 15.2.6
;; diff((1-z)^(a+b-c)*F(a,b;c;z),z,n)
;;    = poch(c-a,n)*poch(c-b,n)/poch(c,n)*(1-z)^(a+b-c-n)*F(a,b;c+n;z)
;;
;; A&S 15.2.7:
;; diff((1-z)^(a+m-1))*F(a,b;c;z),z,m)
;;    = (-1)^m*poch(a,m)*poch(c-b,m)/poch(c,m)*(1-z)^(a-1)*F(a+m,b;c+m;z)
;;
;; Rewrite F(a,-a+m; c+n;z) as F(-a+m, a; c+n; z).  Then apply 15.2.6
;; to F(-a,a;1/2;z), differentiating n-m times:
;;
;; diff((1-z)^(-1/2)*F(-a,a;1/2;z),z,n-m)
;;     = poch(1/2+a,n-m)*poch(1/2-a,n-m)/poch(1/2,n-m)*(1-z)^(-1/2-n+m)*F(-a,a;1/2+n-m;z)
;;
;; Multiply this result by (1-z)^(n-a-1/2) and apply 15.2.7, differentiating m times:
;;
;; diff((1-z)^(m-a-1)*F(-a,a;1/2+n-m;z),z,m)
;;     = (-1)^m*poch(-a,m)*poch(1/2+n-m-a,m)/poch(1/2+n-m)*(1-z)^(-a-1)*F(-a+m,a;1/2+n;z)
;;
;; Which gives F(-a+m,a;1/2+n;z), which is what we wanted.
(defun f81 (fun m n a)
  (mul (factf (add (inv 2) (- n m)) m)
       (factf (inv 2) (- n m))
       (inv (power -1 m))
       (inv (factf a m))
       (inv (factf (add (inv 2) n (sub a m)) m))
       (inv (factf (sub (inv 2) a) (- n m)))
       (inv (factf (add (inv 2) a) (- n m)))
       (power (sub 1 'ell) (sub 1 a))
       ($diff (mul (power (sub 1 'ell) (add a n (inv -2)))
		   ($diff (mul (power (sub 1 'ell) (inv -2))
			       fun)
			  'ell (- n m)))
	      'ell m)))

;; Like f86, but |n|>=|m|
;;
;; F(a-m,-a;1/2-n;z) where n >= m >0
;;
;; A&S 15.2.4
;; diff(z^(c-1)*F(a,b;c;z),z,n)
;;     = poch(c-n,n)*z^(c-n-1)*F(a;b;c-n;z)
;;
;; A&S 15.2.8:
;; diff(z^(c-1)*(1-z)^(b-c+n)*F(a,b;c;z),z,n)
;;     = poch(c-n,n)*z^(c-n-1)*(1-z)^(b-c)*F(a-n,b;c-n;z)
;;
;; For our problem:
;;
;; diff(z^(-1/2)*F(a,-a;1/2;z),z,n-m)
;;     = poch(1/2-n+m,n-m)*z^(m-n-1/2)*F(a,-a;1/2-n+m;z)
;;
;; diff(z^(m-n-1/2)*(1-z)^(n-a-1/2)*F(a,-a;1/2-n+m;z),z,m)
;;     = poch(1/2-n,m)*z^(-1/2-n)*(1-z)^(n-m-a-1/2)*F(a-m,-a;1/2-n;z)
;;
;; So
;;
;; G(z) = z^(m-n-1/2)*F(a,-a;1/2-n+m;z)
;;      = z^(n-m+1/2)/poch(1/2-n+m,n-m)*diff(z^(-1/2)*F(a,-a;1/2;z),z,n-m)
;;
;; F(a-m,-a;1/2-n;z)
;;     = z^(n+1/2)*(1-z)^(m+a-1/2-n)/poch(1/2-n,m)*diff((1-z)^(n-a-1/2)*G(z),z,m)
(defun f82 (fun m n a)
  (mul (inv (factf (sub (inv 2) n) m))
       (inv (factf (sub (add (inv 2) m) n) (- n m)))
       (power 'ell (add n (inv 2)))
       (power (sub 1 'ell) (sub (add m (inv 2) a) n))
       ($diff (mul (power (sub 1 'ell)
			  (sub (sub n a) (inv 2)))
		   ($diff (mul  (power 'ell (inv -2)) fun)
			  'ell
			  (- n m)))
	      'ell
	      m)))

;; F(a+m,-a;1/2+n;z) with m,n integers and m < 0, n >= 0
;;
;; Write this more clearly as F(a-m,-a;1/2+n;z), m > 0, n >= 0
;; or equivalently F(a-m,-a;c+n;z)
;;
;; A&S 15.2.6
;; diff((1-z)^(-1/2)*F(a,-a;1/2;z),z,n) 
;;     = poch((1/2+a,n)*poch(1/2-a,n)/poch(1/2,n)*(1-z)^(-1/2-n)
;;         * F(a,-a;1/2+n;z)
;;
;; A&S 15.2.5
;; diff(z^(n+m-a-1/2)*(1-z)^(-1/2-n)*F(a,-a;1/2+n;z),z,m)
;;     = poch(1/2+n-a,m)*z^(1/2+n-a-1)*(1-z)^(-1/2-n-m)*F(a-m,-a;1/2+n;z)
;;     = poch(1/2+n-a,m)*z^(n-a-1/2)*(1-z)^(-1/2-n-m)*F(a-m,-a;1/2+n;z)
;;
;; (1-z)^(-1/2-n)*F(a,-a;1/2+n;z)
;;     = poch(1/2,n)/poch(1/2-a,n)/poch(1/2+a,n)*diff((1-z)^(-1/2)*F(a,-a;1/2;z),z,n)
;;
;; F(a-m,-a;1/2+n;z)
;;     = (1-z)^(n+m+1/2)*z^(a-n+1/2)/poch(1/2+n-a,m)
;;        *diff(z^(n+m-a-1/2)*(1-z)^(-1/2-n)*F(a,-a;1/2+n;z),z,m)
(defun f83 (fun m n a)
  (mul (factf (inv 2) n)
       (inv (factf (sub (inv 2) a) n))
       (inv (factf (sub (add n (inv 2)) a) m))
       (inv (factf (add (inv 2) a) n))
       (power (sub 1 'ell) (add m n (inv 2)))
       (power 'ell (add (sub a n) (inv 2)))
       ($diff (mul (power 'ell (sub (sub (+ m n) a) (inv 2)))
		   ($diff (mul (power (sub 1 'ell)
				      (inv -2))
			       fun)
			  'ell
			  n))
	      'ell
	      m)))

;; The last case F(a+m,-a;c+n;z), m,n integers, m >= 0, n < 0
;;
;; F(a+m,-a;1/2-n;z)
;;
;; A&S 15.2.4:
;; diff(z^(c-1)*F(a,b;c;z),z,n) = poch(c-n,n)*z^(c-n-1)*F(a,b;c-n;z)
;;
;; A&S 15.2.3:
;; diff(z^(a+m-1)*F(a,b;c;z),z,m) = poch(a,n)*z^(a-1)*F(a+n,b;c;z)
;;
;; For our problem:
;;
;; diff(z^(-1/2)*F(a,-a;1/2;z),z,n) = poch(1/2-n,n)*z^(-n-1/2)*F(a,-a;1/2-n;z)
;;
;; diff(z^(a+m-1)*F(a,-a;1/2-n;z),z,m) = poch(a,m)*z^(a-1)*F(a+m,-a;1/2-n;z)
(defun f84 (fun m n a)
  (mul (inv (mul (factf a m)
		 (factf (sub (inv 2) n) n)))
       (power 'ell (sub 1 a))
       ($diff (mul (power 'ell (sub (add a m n) (inv 2)))
		   ($diff (mul (power 'ell (inv -2)) fun)
			  'ell
			  n))
	      'ell
	      m)))

;; Like f82, but |n|<|m|
;;
;; F(a-m,-a;1/2-n;z), 0 < n < m
;;
;; A&S 15.2.5
;; diff(z^(c-a+n-1)*(1-z)^(a+b-c)*F(a,b;c;z),z,n)
;;     = poch(c-a,n)*z^(c-a-1)*(1-z)^(a+b-c-n)*F(a-n,b;c;z)
;;
;; A&S 15.2.8:
;; diff(z^(c-1)*(1-z)^(b-c+n)*F(a,b;c;z),z,n)
;;     = poch(c-n,n)*z^(c-n-1)*(1-z)^(b-c)*F(a-n,b;c-n;z)
;;
;; For our problem:
;;
;; diff(z^(-a+m-n-1/2)*(1-z)^(-1/2)*F(a,-a;1/2;z),z,m-n)
;;     = poch(1/2-a,m-n)*z^(-a-1/2)*(1-z)^(-1/2-m+n)*F(a-m+n,-a;1/2;z)
;;
;; diff(z^(-1/2)*(1-z)^(-a-1/2+n)*F(a-m+n,-a;1/2;z),z,n)
;;     = poch(1/2-n,n)*z^(-n-1/2)*(1-z)^(-a-1/2)*F(a-m,-a;1/2-n;z)
;;
;; G(z) = z^(-a-1/2)*(1-z)^(-1/2-m+n)*F(a-m+n,-a;1/2;z)
;;      = 1/poch(1/2-a,m-n)*diff(z^(-a+m-n-1/2)*(1-z)^(-1/2)*F(a,-a;1/2;z),z,m-n)
;;
;; F(a-m,-a;1/2-n;z)
;;      = z^(n+1/2)*(1-z)^(a+1/2)/poch(1/2-n,n)
;;         *diff(z^(-1/2)*(1-z)^(-a-1/2+n)*F(a-m+n,-a;1/2;z),z,n)
;;      = z^(n+1/2)*(1-z)^(a+1/2)/poch(1/2-n,n)
;;         *diff(z^a*(1-z)^(m-a)*G(z),z,n)
;; 
(defun f86 (fun m n a)
  (mul (inv (mul (factf (sub (inv 2) n) n)
		 (factf (sub (inv 2) a) (- m n))))
       (power 'ell (add n (inv 2)))
       (power (sub 1 'ell) (add (inv 2) a))
       ($diff (mul (power 'ell a)
		   (power (sub 1 'ell) (sub m a))
		   ($diff (mul (power 'ell
				      (sub (sub (sub m n) (inv 2)) a))
			       (power (sub 1 'ell)
				      (inv -2))
			       fun)
			  'ell (- m n)))
	      'ell n)))


(eval-when
    #+gcl (compile)
    #-gcl (:compile-toplevel)
    (declare-top (unspecial var *par* checkcoefsignlist))
    )
