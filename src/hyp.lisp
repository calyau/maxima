;;;  -*- LISP -*-
;;;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(in-package "MAXIMA")

(macsyma-module hyp)

;;(eval-when
;;    #+gcl (compile eval)
;;    #-gcl (:compile-toplevel :execute)
;;    (declare-top (special fun w b l alglist $true $false n  c l1 l2)))

(declare-top (special fun w b l alglist $true $false n  c l1 l2))

(declare-top (special var par zerosigntest productcase 
		      fldeg flgkum checkcoefsignlist serieslist
		      $exponentialize $bestriglim $radexpand))


;; (eval-when (compile eval) (load '((dsk ell) macros >)) )

(declare-top (special fldeg flgkum listcmdiff checkcoefsignlist serieslist
		      fl1f1))

(setq flgkum t fldeg t fl1f1 t checkcoefsignlist nil)

(declare-top (special eps $exponentialize $bestriglim $radexpand))

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

    (setq flgkum t fldeg t fl1f1 t checkcoefsignlist '())
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
(defun $hgfred (l1 l2 arg )
  (let (($radexpand '$all)
	(var arg)
	(par arg))
    (hgfsimp-exec (cdr l1) (cdr l2) arg)))


(defun hgfsimp-exec
    (l1 l2 arg)
  (setq l1 (copy-tree l1) l2 (copy-tree l2))
  (prog (res $exponentialize)
     (setq  res
	    (hgfsimp l1 l2 arg))
     (cond ((or (numberp res)(not (atom res)))
	    (return res)))
     (return (fpqform l1 l2 arg))))


(defun hgfsimp (l1 l2 var)
  (prog (resimp)
     (setq l1 (macsimp l1)
	   l2 (macsimp l2)
	   resimp (simpg l1 l2))
	    
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


#+(or)
(defun macsimp (l)
  (cond ((null l) nil)
	(t (append (list (simplifya (car l) nil)) (cdr l)))))

(defun macsimp (l)
  (mapcar #'(lambda (index)
	      (simplifya index nil))
	  l))

(defun simpg (l1 l2)
  (prog(il)
     (cond ((null (setq il (zl-intersection l1 l2)))
	    (return (simpg-exec l1 l2))))
     (return (simpg-exec (del il l1) (del il l2)))))



(defun del (a b)
  (cond ((null a) b)(t (del (cdr a) (zl-delete (car a) b 1)))))

;; Handle the simple cases where the result is either a polynomial, or
;; is undefined because we divide by zero.
(defun simpg-exec (l1 l2)
  (prog(n)
     (cond ((zerop-in-l l1)
	    ;; A zero in the first index means the series terminates
	    ;; after the first term, so the result is always 1.
	    (return 1)))
     (cond ((setq n (hyp-negp-in-l l1))
	    ;; A negative integer in the first series means we have a
	    ;; polynomial.
	    (return (create-poly l1 l2 n))))
     (cond ((or (zerop-in-l l2)
		(hyp-negp-in-l l2))
	    ;; A zero or negative number in the second index means we
	    ;; eventually divide by zero, so we're undefined.
	    (return 'undef)))
     ;; We failed so more complicated stuff needs to be done.
     (return (append (list 'fail) (list l1) (list l2)))))

			

(defun intdiffl1l2 (l1 l2)
  (cond ((null l1)  nil)(t (intdiff l1 l2))))

(defun intdiff (l1 l2)
  (prog(l a dif)
     (setq l l2 a (car l1))
     jump
     (cond ((null l)(return (intdiffl1l2 (cdr l1) l2))))
     (cond ((nni (setq dif (sub a (car l))))
	    (return (list a dif))))
     (setq l (cdr l))
     (go jump)))		     


(defun create-poly (l1 l2 n)
  ((lambda(len1 len2)
     (cond ((and (equal len1 2)(equal len2 1))
	    (2f1polys l1 l2 n))
	   ((and (equal len1 1)(equal len2 1))
	    (1f1polys l2 n))
	   ((and (equal len1 2)(zerop len2))
	    (2f0polys l1 n))
	   (t (create-any-poly l1 l2 (mul -1 n)))))
   (length l1)
   (length l2)))


#+nil
(defun 1f1polys (l2 n)
  (prog(c fact1 fact2)
     (setq c
	   (car l2)
	   n
	   (mul -1 n)
	   fact1
	   (mul (power 2 n)
		(factorial n)
		(inv (power -1 n)))
	   fact2
	   (mul (power 2 (inv 2))(power var (inv 2))))
     (cond ((equal c (div 1 2))
	    (return (mul fact1
			 (inv (factorial (add n n)))
			 (hermpol (add n n) fact2)))))
     (cond ((equal c (div 3 2))
	    (return (mul fact1
			 (inv (factorial (add n n 1)))
			 (hermpol (add n n 1) fact2)))))
     (return (mul (factorial n)
		  (gm c)
		  (gm (add c n))
		  (lagpol n (sub c 1) var)))))

(defun 1f1polys (l2 n)
  (let* ((c (car l2))
	 (n (mul -1 n))
	 (fact1 (mul (power 2 n)
		     (factorial n)
		     (inv (power -1 n))))
	 (fact2 (mul (power 2 (inv 2))
		     (power var (inv 2)))))
    (cond ((equal c (div 1 2))
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
	  ((equal c (div 3 2))
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
		(inv (power var (inv 2)))))
	  (t
	   ;; A&S 22.5.54:
	   ;;
	   ;; gen_laguerre(n,alpha,x) =
	   ;; binomial(n+alpha,n)*hgfred([-n],[alpha+1],x);
	   ;;
	   ;; Or hgfred([-n],[alpha],x) =
	   ;; gen_laguerre(n,alpha-1,x)/binomial(n+alpha-1,n)
	   (mul (factorial n)
		(inv (mul
		      (gm c)
		      (gm (add c n))))
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


(defun 2f0polys (l1 n)
  (prog(a b temp x)
     (setq a (car l1) b (cadr l1))
     (cond ((equal (sub b a)(div -1 2))
	    (setq temp a a b b temp)))
     (cond ((equal (sub b a)(div 1 2))
	    (setq x (power (div 2 (mul -1 var))(inv 2)))
	    (return (interhermpol n a b x))))
     (setq x (mul -1 (inv var)) n (mul -1 n))
     (return (mul (factorial n)
		  (inv (power x n))
		  (inv (power -1 n))
		  (lagpol n (add b n) x)))))

(defun interhermpol
    (n a b x)
  (prog(fact)
     (setq fact (power x (mul -1 n)))
     (cond ((equal a n)
	    (setq n (mul -2 n))
	    (return (mul fact (hermpol n x)))))
     (cond ((equal b n)
	    (setq n (sub 1 (add n n)))
	    (return (mul fact (hermpol n x)))))))


;; F(a,b;c;z), where either a or b is a negative integer.
(defun 2f1polys (l1 l2 n)
  (prog (l v lgf)
     ;; Since F(a,b;c;z) = F(b,a;c;z), make sure L1 has the negative
     ;; integer first, so we have F(-n,d;c;z)
     (cond ((not (eql (car l1) n))
	    (setq l1 (reverse l1))))
     #||
     (format t "l1 = ~A~%" l1)
     (format t "vfvp arg = ~A~%" (div (add (cadr l1) n) 2))
     (format t "var = ~A~%" var)
     (format t "par = ~A~%" par)
     ||#
     (setq l (vfvp (div (add (cadr l1) n) 2)))

     ;;(format t "l  = ~A~%" l)
     (setq v (cdr (zl-assoc 'v l)))

     ;; Assuming we have F(-n,b;c;z), then v is (b+n)/2.
	    
     ;;(format t "v = ~A~%" v)

     ;; See if it can be a Legendre function.
     (cond ((setq lgf (legpol (car l1) (cadr l1) (car l2)))
	    (return lgf)))

     (cond ((equal (sub (car l2) v) '((rat simp) 1 2))
	    ;; A&S 15.4.5:
	    ;; F(-n, n + 2*a; a + 1/2; x) = n!*gegen(n, a, 1-2*x)/pochhammer(2*a,n)
	    ;;
	    ;; So v = a, and (car l2) = a + 1/2.
	    (return (mul 
		     (cond ((zerp v) 1)
			   (t (mul (factorial (* -1 n))
				   (inv (factf (mul 2 v) (* -1 n))))))
		     (gegenpol (mul -1 n)
			       v
			       (sub 1 (mul 2 par)))))))
     ;; A&S 15.4.6 says
     ;; F(-n, n + a + 1 + b; a + 1; x) = n!*jacobi_p(n,a,b,1-2*x)/pochhammer(a+1,n);
     ;;
     (return (mul (factorial (* -1 n))
		  ;; I (rlt) don't think this is right, based on
		  ;; 15.4.6, because v doesn't have the right value.
		  #+nil(inv (factf (add 1 v) (* -1 n)))
		  ;; Based on 15.4.6, we really want the l2 arg
		  (inv (factf (car l2) (* -1 n)))
		  (jacobpol (mul -1 n)
			    (add (car l2) -1)
			    (sub (mul 2 v) (car l2))
			    (sub 1 (mul 2 par)))))))


#+nil
(defun jacobpol
    (n a b x)
  (list '(mqapply)(list '($%p array) n a b) x))

(defun jacobpol (n a b x)
  `(($jacobi_p) ,n ,a ,b ,x))


#+nil
(defun gegenpol(n v x)
  (cond ((equal v 0) (tchebypol n x))
	(t (list '(mqapply)(list '($%c array) n v) x))))

(defun gegenpol(n v x)
  (cond ((equal v 0) (tchebypol n x))
	(t `(($ultraspherical) ,n ,v ,x))))

;; Legendre polynomial
(defun legenpol (n x)
  `(($legendre_p) ,n ,x))

;; Chebyshev polynomial
(defun tchebypol (n x)
  `(($chebyshev_t) ,n ,x))

(defun create-any-poly
    (l1 l2 n)
  (prog(result exp prodnum proden)
     (setq result 1 prodnum 1 proden 1 exp 1)
     loop
     (cond ((zerop n) (return result)))
     (setq prodnum
	   (mul prodnum (mull l1))
	   proden
	   (mul proden (mull l2)))
     (setq result
	   (add result
		(mul prodnum
		     (power var exp)
		     (inv proden)
		     (inv (factorial exp)))))
     (setq n
	   (sub n 1)
	   exp
	   (add exp 1)
	   l1
	   (incr1 l1)
	   l2
	   (incr1 l2))
     (go loop)))


(defun mull(l)
  (cond ((null l) 1)(t (mul (car l)(mull (cdr l))))))


(defun incr1 (l)
  (cond ((null l) nil)
	(t (append (list (add (car l) 1))(incr1 (cdr l))))))


(defun dispatch-spec-simp (l1 l2)
  (prog(len1 len2)
     (setq len1 (length l1) len2 (length l2))
     (cond ((and (lessp len1 2)(lessp len2 2))
	    (return (simp2>f<2 l1 l2 len1 len2))))
     (cond ((and (equal len1 2)(equal len2 1))
	    (return (simp2f1 l1 l2))))
     (return (fpqform l1 l2 var))))


(defun simp2>f<2 (l1 l2 len1 len2)
  (prog()
     (cond ((and (zerop len1) (zerop len2))
	    (return (power '$%e var))))
     (cond ((and (zerop len1) (equal len2 1))
	    (return (bestrig (car l2) var))))
     (cond ((zerop len2) (return (binom (car l1)))))
     (return (confl l1 l2 var))))


	    
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

(defun ctr(z)
  (power (div 2 (mul '$%pi z)) (inv 2)))

(defun negcoef (x)
  (prog(d)
     (cond ((null (setq d (cdr (zl-remprop 'd (d*u x)))))
	    (return t)))
     (cond ((eq (asksign (inv d)) '$positive)
	    (return nil)))
     (return t)))

(defun binom(a)
  (power (sub 1 var) (mul -1 a)))


(defun kummer (l1 l2)
  (mul (list '(mexpt) '$%e var)
       (confl (list (sub (car l2)(car l1))) l2 (mul -1 var))))


(defun zerop-in-l (l)
  (cond ((null l) nil)
	((numberp (car l))
	 (cond ((zerop (car l)) t)(t (zerop-in-l (cdr l)))))
	(t (zerop-in-l (cdr l)))))


(defun hyp-negp-in-l (l)
  (cond ((null l) nil)
	((hyp-integerp (car l))
	 (cond ((minusp (car l)) (car l))
	       (t (hyp-negp-in-l (cdr l)))))
	(t (hyp-negp-in-l (cdr l)))))


(defun zl-intersection (l1 l2)
  (cond ((null l1) nil)
	((zl-member (car l1) l2)
	 (cons (car l1)
	       (zl-intersection (cdr l1)
				(zl-delete (car l1) l2 1))))
	(t (zl-intersection (cdr l1) l2))))

#+(or)
(defun 2inp (l)
  (prog(count)
     (setq count 0)
     loop
     (cond ((and (null l)(greaterp count 1))(return t)))
     (cond ((null l)(return nil)))
     (cond ((hyp-integerp (car l))(setq count (add1 count))))
     (setq l (cdr l))
     (go loop)))

#+(or)
(defun 2ratp (l)
  (prog(count)
     (setq count 0)
     loop
     (cond ((and (null l)(greaterp count 1))(return t)))
     (cond ((null l)(return nil)))
     (cond ((eq (caaar l) 'rat)(setq count (add1 count))))
     (setq l (cdr l))
     (go loop)))

;;2NUMP SHOULD BE ELIMINATED. IT IS NOT EFFICIENT TO USE ANYTHING ELSE
;;BUT JUST CONVERTING TO RAT REPRESENTATION ALL 0.X ,X IN
;;N. ESPECIALLY LATER WHEN WE CONVERT TO OMONIMA FOR TESTING TO FIND
;;THE RIGHT FORMULA


#+(or)
(defun 2nump (l)
  (prog(count)
     (setq count 0)
     loop
     (cond ((and (null l)(greaterp count 1))(return t)))
     (cond ((null l)(return nil)))
     (cond ((numberp (car l))(setq count (add1 count))))
     (setq l (cdr l))
     (go loop)))


(defun whitfun(k m var)
  (list '(mqapply) (list '($%m array) k m) var))

(defun simp2f1
    (l1 l2)
  (prog(a b c lgf)
     (setq a (car l1) b (cadr l1) c (car l2))
     (cond ((and (equal a 1)
		 (equal b 1)
		 (equal c 2))
	    ;; F(1,1;2;z), A&S 15.1.3
	    (return (mul (inv (mul -1 var))
			 (mlog (add 1 (mul -1 var)))))))
     (cond ((or (equal c  (div 3 2))
		(equal c  (div 1 2)))
	    ;; F(a,b; 3/2; z) or F(a,b;1/2;z)
	    (cond ((setq lgf (trig-log (list a b) (list c)))
		   (return lgf)))))
	    
     (cond ((or
	     (equal (sub a b) (div 1 2))
	     (equal (sub b a) (div 1 2)))
	    ;; F(a,b;c;z) where |a-b|=1/2 
	    (cond ((setq lgf (hyp-cos a b c))(return lgf)))))
     (cond ((and (hyp-integerp a)
		 (hyp-integerp b) (hyp-integerp c))
	    ;; F(a,b;c;z) when a, b, c are integers.
	    (return (simpr2f1 (list a b) (list c)))))
     (cond ((and (hyp-integerp (add c (inv 2)))
		 (hyp-integerp (add a b)))
	    ;; F(a,b;c;z) where a+b is an integer and c+1/2 is an
	    ;; integer.
	    (return (step4 a b c))))
     (cond ((hyp-integerp (add (sub a b) (inv 2)))
	    ;; F(a,b;c,z) where a-b+1/2 is an integer
	    (cond ((setq lgf (step7 a b c))
		   (return lgf)))))
     (cond ((setq lgf (legfun a b c))
	    (return lgf)))
     (print 'simp2f1-will-continue-in)
     (return  (fpqform l1 l2 var))))

(defun step7 (a b c)
  (prog (l m n k mn kl sym sym1 r)
     (setq l (s+c a)
	   sym (cdras 'f l)
	   mn  (cdras 'c l)
	   l (s+c c)
	   sym1 (cdras 'f l))
     (cond ((not (equal (mul sym 2) sym1))(return nil)))
     (setq kl (cdras 'c l)
	   l  (s+c b)
	   r (sub (add (inv 2) (cdras 'c l)) mn)
	   m ($num mn)
	   n ($denom mn)
	   k ($num kl)
	   l ($denom kl))
     (cond ((equal (* 2 l) n)
	    (cond ((hyp-integerp (// (- k m) n))
		   (return (hyp-algv k l m n a b c))))))
     (cond ((hyp-integerp (// k (* 2 l)))
	    (cond ((hyp-integerp (// m n))
		   (return (hyp-algv k l m n a b c)))
		  (t (return nil))))
	   ((hyp-integerp (// m n))
	    (return nil))
	   ((hyp-integerp (/ (- (* k n) (* 2 l m)) (* 2 l n)))
	    (return (hyp-algv k l m n a b c))))
     (return nil)))

(defun getxy
    (k l m n)
  (prog (x y)
     (setq y 0)
     loop
     (cond ((hyp-integerp (setq x
				   (// (+ y
					  (// k l)
					  (* -2 (// m n)))
				       2)))
	    (return (list x y))))
     (setq y (+ 2 y))
     (go loop)))

(defun hyp-algv  (k l m n a b c)
  (prog (x y xy a-b)
     (setq a-b (- a b))
     (setq xy (getxy k l m n)
	   x (car xy)
	   y (cdr xy))
     (cond ((< x 0)(go out)))
     (cond ((< x y)(cond ((< (+ a-b x (inv 2)) 0)
			  (return (f88 x y a c fun)))
			 (t (return (f87 x y a c fun)))))
	   (t (cond ((< (+ a-b x (inv 2)) 0)
		     (return (f90 x y a c fun)))
		    (t (return (f89 x y a c fun))))))
     out
     (setq w (* x -1))
     (cond ((< (- (+ a-b (inv 2)) w) 0)
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

(defun f88 (x y a c fun )
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



(defun simpr2f1
    (l1 l2)
  ((lambda (inl1p inl1bp inl2p)
     (cond (inl2p (cond ((and inl1p inl1bp)
			 (derivint (- (car l1) 1)
				   (- (cadr l1)
				      (car l1))
				   (- (- (car l2)
					 (cadr l1))
				      1)))
			(inl1p (geredno2 (cadr l1)
					 (car l1)
					 (car l2)))
			(inl1bp (geredno2 (car l1)
					  (cadr l1)
					  (car l2)))
			(t 'fail1)))
	   (inl1p (cond (inl1bp 'd) (t 'c)))
	   ((eq (caaar l1) 'rat)
	    (cond (inl1bp 'c) (t 'd)))
	   (t 'failg)))
   (hyp-integerp (car l1))
   (hyp-integerp (cadr l1))
   (hyp-integerp (car l2))))
(defun geredno1
    (l1 l2)
  (cond ((and (greaterp (car l2)(car l1))
	      (greaterp (car l2)(cadr l1)))
	 (geredf (car l1)(cadr l1)(car l2)))
	(t (gered1 l1 l2 #'hgfsimp))))

(defun geredno2 (a b c)
  (cond ((greaterp c b) (geredf b a c))
	(t (gered2 a b c))))

(defun derivint
    (n m l)(subst var 'psey
		  (mul (power -1 m)
		       (factorial (+ n m l 1))
		       (inv (factorial n))
		       (inv (factorial l))
		       (inv (factorial (+ n m)))
		       (inv (factorial (+ m l)))
		       ($diff  (mul (power (sub 1 'psey) (+ m l))
				    ($diff (mul (power  'psey  -1)
						-1
						(mlog (sub 1 'psey)))
					   'psey
					   l))
			       'psey
			       (+ n m)))))



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
	(a2 (mul c (inv 2)))
	(z1 (sub 1 var)))
    ;; a1 = (a+b-1/2)/2
    ;; z1 = 1-var
    ;; a2 = c/2
    (cond ((equal (sub (add a b)
		       (div 1 2))
		  c)
	   ;; a+b-1/2 = c
	   ;;
	   ;; 2^(2*a1 - 1)/sqrt(z1)*(1+sqrt(z1))^(1-2*a1)
	   (mul (power 2 (sub (mul a1 2) 1))
		(inv (power  z1 (div 1 2)))
		(power (add 1
			    (power z1
				   (div 1
					2)))
		       (sub 1 (mul 2 a1)))))
	  ((equal (add 1 (mul 2 a1)) c)
	   ;; c = 1+2*a1 = a+b+1/2
	   ;;
	   ;; 2^(c-1)*(1+sqrt(z1))^(-(c-1))
	   (mul (power 2 (sub c 1))
		(power (add 1
			    (power z1
				   (div 1
					2)))
		       (mul -1 (sub c 1))))))))

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



(defun nni(a)(cond ((hyp-integerp a)(not (minusp a)))))


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


(defun legfun                      
    (a b c)			   
  (prog(1-c a-b c-a-b inv2)
     (setq 1-c
	   (sub 1 c)
	   a-b
	   (sub a b)
	   c-a-b
	   (sub (sub c a) b)
	   inv2
	   (inv 2))
     (cond ((equal a-b inv2)   
	    (return (gered1 (list a b)(list c) #'legf24))))
     (cond ((equal a-b (mul -1 inv2))
	    (return (legf24 (list a b)(list c) var))))
     (cond ((equal c-a-b inv2)
	    (return (legf20 (list a b)(list c) var))))
     (cond ((equal c-a-b (mul -1 inv2))
	    (return (gered1 (list a b)(list c) #'legf20))))
     (cond ((equal 1-c a-b)
	    (return (legf16 (list a b)(list c) var))))
     (cond ((equal 1-c (mul -1 a-b))
	    (return (gered1 (list a b)(list c) #'legf16))))
     (cond ((equal 1-c c-a-b)
	    (return (gered1 (list a b)(list c) #'legf14))))
     (cond ((equal 1-c (mul -1 c-a-b))
	    (return (legf14 (list a b)(list c) var))))
     (cond ((equal a-b (mul -1 c-a-b))
	    (return (legf36 (list a b)(list c) var))))
     (cond ((or (equal 1-c inv2)
		(equal 1-c (mul -1 inv2)))
	    (return (legpol a b c))))
     (cond ((equal a-b c-a-b)
	    (return 'legendre-funct-to-be-discovered)))
     (return nil)))



(defun legf20
    (l1 l2 var)
  (prog(m n b c)
     (setq b (cadr l1) c (car l2))
     (setq m (sub 1 c) n (mul -1 (add b b m)))
     (return (mul (lf n m)
		  (legen n
			 m
			 (power (sub 1 var) (inv 2))
			 '$p)))))


(defun legf24
    (l1 l2 var)
  (prog(m n a c)
     (setq a
	   (car l1)
	   c
	   (car l2)
	   m
	   (sub 1 c)
	   n
	   (mul -1 (add a a m)))
     (return (mul (lf n m)
		  (power var (add n m))
		  (legen n
			 m
			 (inv (power (sub 1 var)
				     (inv 2)))
			 '$p)))))


(defun legf16
    (l1 l2 var)
  (prog(m n a c)
     (setq a (car l1) c (car l2) m (sub 1 c) n (mul -1 a))
     (return (mul (power 2 (mul -1 n))
		  (power (sub var 1)(div m -2))
		  (inv (gm (sub 1 m)))
		  (power (add var 1)(add (div m 2) n))
		  (legen n
			 m
			 (div (add 1 var)(sub 1 var))
			 '$p)))))


(defun lf
    (n m)
  (mul (power 2 m)
       (inv (power (sub (power var 2) 1)(div m 2)))
       (inv (gm (sub 1 m)))))


(defun legf14
    (l1 l2 var)
  (prog(m n a c b)
     (setq l (s+c (car l1))
	   a (cond ((eq (cdras 'c l) 0) (cdras 'f l))
		   (t (mul -1 (cdras 'f l))))
	   c (car l2) m (sub 1 c)
	   n (mul -1 a))
     (return (mul (power  (add var 1)(div m 2))
		  (power (sub var 1)(div m -2))
		  (inv (gm (sub 1 m)))
		  (legen n m (sub 1 (mul 2 var)) '$p)))))


(defun legf36
    (l1 l2 var)
  (prog(n m a b)
     (setq a (car l1) b (cadr l1) n (sub b 1) m (sub b a))
     (return (mul (power 2 n)
		  (gm (add 1 n))
		  (gm (add 1 n m))
		  (power (add var 1)
			 (add (div m 2)(mul -1 n) -1))
		  (power (sub var 1)(div m -2))
		  (inv (gm (add 2 n n)))
		  (power '$%e (mul -1 '$%i m '$%pi))
		  (legen n m (div (sub 2 var) var) '$q)))))


(defun legen (n m x pq)
  (cond ((and (equal m 0)
	      (eq ($askinteger n) '$yes))
	 `((,(if (eq pq '$q) '$legendre_q '$legendre_p)) ,n ,x))
	(t
	 `((,(if (eq pq '$q) '$assoc_legendre_q '$assoc_legendre_p))
	    ,n ,m ,x))))


(defun legpol (a b c)
  (prog (l v)
     (cond ((not (hyp-negp-in-l (list a)))
	    (return 'fail-1-in-c-1-case)))
     (setq l (vfvp (div (add b a) 2)))
     (setq v (cdr (zl-assoc 'v l)))
     ;; v is (a+b)/2
     (cond ((and (equal v '((rat simp) 1 2))
		 (equal c 1))
	    ;; A&S 22.5.49:
	    ;; P(n,x) = F(-n,n+1;1;(1-x)/2)
	    (return (legenpol (mul -1 a)
			      (sub 1 (mul 2 var))))))

     (cond ((and (equal c '((rat simp) 1 2))
		 (equal (add b a) '((rat simp) 1 2)))
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

     (cond ((and (equal c '((rat simp) 3 2))
		 (equal (add b a) '((rat simp) 3 2)))
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
     (cond ((and (equal (sub a b) '((rat simp) 1 2))
		 (equal (sub c (mul 2 b)) '((rat simp) 1 2)))
	    ;; A&S 22.5.51
	    ;; P(n,x) = binomial(2*n,n)*(x/2)^n*F(-n/2,(1-n)/2;1/2-n;1/x^2)
	    ;;
	    ;; F(-n/2,(1-n)/2;1/2-n,1/x^2) = P(n,x)/binomial(2*n,n)*(x/2)^(-n)
	    (return (mul (power (factorial (mul -2 b)) 2)
			 (inv (factorial (mul -4 b)))
			 (power (mul 2 (power var (div 1 2))) (mul -2 b))
			 (legenpol (mul -2 b)
				   (power var (div -1 2)))))))
     (cond ((and (equal (sub b a) '((rat simp) 1 2))
		 (equal (sub c (mul 2 a)) '((rat simp) 1 2)))
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


       
(defun multaug
    (a n)
  (cond ((zerop n) 1)(t (mul a (multaug (add a 1)(sub1 n))))))


(defun gered1
    (l1 l2 simpflg)
  (mul (power (sub 1 var)
	      (add (car l2)
		   (mul -1 (car l1))
		   (mul -1 (cadr l1))))
       (funcall simpflg
		(list (sub (car l2) (car l1))
		      (sub (car l2) (cadr l1)))
		l2
		var)))





(defun gered2
    (a b c)
  (mul (power (sub 1 var)(mul -1 a))
       (hgfsimp (list a (sub c b))
		(list c)
		(div var (sub var 1)))))


(defun geredf
    (a b c)
  (add (div (mul (gm c)
		 (gm (add c (mul -1 a)(mul -1 b)))
		 (power var (mul -1 a))
		 (hgfsimp (list a (add a 1 (mul -1 c)))
			  (list (add a b (mul -1 c) 1))
			  (sub 1 (div 1 var))))
	    (mul (gm (sub c a))(gm (sub c b))))
       (div (mul (gm c)
		 (gm (add a b (mul -1 c)))
		 (power (sub 1 var)
			(add c (mul -1 a)(mul -1 b)))
		 (power var (sub a c))
		 (hgfsimp (list (sub c a)(sub 1 a))
			  (list (add c
				     (mul -1 a)
				     (mul -1 b)
				     1))
			  (sub 1 (div 1 var))))
	    (mul (gm a)(gm b)))))



(defun trig-log (l1 l2)
  (cond ((equal (simplifya (car l2) nil) '((rat simp) 3 2))
	 ;; c = 3/2
	 (trig-log-3 l1 l2))
	((equal (simplifya (car l2) nil) '((rat simp) 1 2))
	 ;; c = 1/2
	 (trig-log-1 l1 l2))
	(t nil)))


(defun trig-log-3 (l1 l2)
  (cond ((and (or (equal (car l1) 1) (equal (cadr l1) 1))
	      (or (equal (car l1) (div 1 2))
		  (equal (cadr l1) (div 1 2))))
	 ;; (a = 1 or b = 1) and (a = 1/2 or b = 1/2)
	 (trig-log-3-exec l1 l2))
	((and (equal (car l1) (cadr l1))
	      (or (equal 1 (car l1))
		  (equal (div 1 2) (car l1))))
	 ;; a = b and (a = 1 or a = 1/2)
	 (trig-log-3a-exec l1 l2))
	((or (equal (add (car l1) (cadr l1)) 1)
	     (equal (add (car l1) (cadr l1)) 2))
	 ;; a + b = 1 or a + b = 2
	 (trig-sin l1 l2))
	((or (equal (sub (car l1) (cadr l1)) (div 1 2))
	     (equal (sub (cadr l1) (car l1)) (div 1 2)))
	 ;; a - b = 1/2 or b - a = 1/2
	 (trig-3 l1 l2))
	(t nil)))

(defun trig-3 (l1 l2)
  ;; A&S 15.1.10
  ;;
  ;; F(a,a+1/2,3/2,z^2) =
  ;; ((1+z)^(1-2*a) - (1-z)^(1-2*a))/2/z/(1-2*a)
  
  (let ((a (sub 1
		(sub (add (car l1)
			  (cadr l1))
		     (div 1 2))))
	(z (power var (div 1 2))))
    (mul (inv z)
	 (inv 2)
	 (inv a)
	 (sub (power (add 1 z) a)
	      (power (sub 1 z) a)))))

(defun trig-sin (l1 l2)
  ;; A&S 15.1.15, 15.1.16
  (prog (a1 z1 a b c)
     (setq a (car l1) b (cadr l1) c (car l2))
     (cond ((equal (add a b) 1)
	    ;; A&S 15.1.15
	    ;;
	    ;; F(a,1-a;3/2;sin(z)^2) =
	    ;;
	    ;; sin((2*a-1)*z)/(2*a-1)/sin(z)
	    (return (mul (inv (mul (mul -1 (sub a b))
				   (msin (masin (msqrt var)))))
			 (msin (mul (mul -1
					 (sub a b))
				    (masin (msqrt var)))))))
	   ((equal (add a b) 2)
	    ;; A&S 15.1.16
	    ;;
	    ;; F(a, 2-a; 3/2; sin(z)^2) =
	    ;;
	    ;; sin((2*a-2)*z)/(a-1)/sin(2*z)
	    (return (mul (msin (mul (setq z1
					  (masin (msqrt
						  var)))
				    (setq a1
					  (mul -1
					       (sub a
						    b)))))
			 (inv (mul a1
				   (msin z1)
				   (mcos z1)))))))
     (return nil)))

;;Generates atan if arg positive else log
(defun trig-log-3-exec (l1 l2)
  ;; See A&S 15.1.4 and 15.1.5
  ;;
  ;; F(a,b;3/2;z) where a = 1/2 and b = 1 (or vice versa).
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
		(matan z))))))

(defun trig-log-3a-exec (l1 l2)
  ;; See A&S 15.1.6 and 15.1.7
  ;;
  ;; F(a,b;3/2,z) where a = b and a = 1/2 or a = 1.
  (destructuring-bind (a b)
      l1
    (cond ((equal (checksigntm var) '$positive)
	   ;; A&S 15.1.6
	   ;;
	   ;; F(1/2,1/2; 3/2; z^2) = sqrt(1-z^2)*F(1,1;3/2;z^2) =
	   ;; asin(z)/z
	   (let ((z (power var (div 1 2))))
	     (if (equal a 1)
		 (div (trig-log-3a-exec (list (div 1 2) (div 1 2)) l2)
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
					l2)
		      (power 1+z^2
			     (div 1 2)))
		 (div (mlog (add z (power 1+z^2
					  (div 1 2))))
		      z)))))))


;;(defun trig-log-1
;;       (l1 l2)
;;       (prog (a b c z1 $exponentialize)
	     
;;	     (setq a (car l1) b (cadr l1) c (car l2))
;;	     (cond ((equal (add a b) 0)
;;		    (cond ((equal (checksigntm var) '$positive)
;;			   (return ($cos (mul (mul 2 a)
;;					      ($asin (power var
;;							    (inv 2)))))))
;;			  (t (return (div (add (power (add (setq
;;							    z1
;;							    (power
;;							     (add
;;							      (mul
;;							       var
;;							       -1)
;;							      1)
;;							     (inv 2)))
;;							   var)
;;						      (mul 2 a))
;;					       (power (sub z1 var)
;;						      (mul 2 a)))
;;					  2)))
;;			  ((equal (add a b) 1)
;;			   (return (mul (inv ($cos (setq z1
;;							 ($asin
;;							  ($sqrt
;;							   var)))))
;;					($cos (mul z1 (sub a b))))))
;;			  ((or (equal (sub a b) (inv 2))
;;			       (equal (sub a b) (inv -2)))
;;			   (return (add (div (power (add 1
;;							 (setq
;;							  z1
;;							  (power
;;							   var
;;							   (inv
;;							    2))))
;;						    (mul -2 a))
;;					     2)
;;					(div (power (sub 1 z1)
;;						    (mul -2 a))
;;					     2)))))))
	     
;;	     (return nil)))



(defun trig-log-1 (a b)	;; 2F1's with C = 1/2
  (let (x z $exponentialize) ;; 15.1.17, 11, 18, 12, 9, and 19
    (setq a (car l1) b (cadr l1))
    (cond ((=0 (m+t a b))
	   ;; F(-a,a;1/2,z)
	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.17
		  (mcos (m*t 2. a (masin (msqrt var)))))
		 ((equal (checksigntm var) '$negative)
		  ;; A&X 15.1.11
		  (m*t 1//2
		       (m+t (m^t (m+t (setq x (msqrt (m-t 1. var)))
				      (setq z (msqrt (m-t var))))
				 (setq b (m*t 2. b)))
			    (m^t (m-t x z) b))))
		 (t ())))
	  ((equal (m+t a b) 1.)
	   ;; F(a,1-a;1/2,z)
	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.18
		  (m//t (mcos (m*t (m-t a b) (setq z (masin (msqrt var)))))
			(mcos z)))
		 ((equal (checksigntm var) '$negative)
		  ;; A&S 15.1.12
		  (m*t 1//2 (m//t (setq x (msqrt (m-t 1. var))))
		       (m+t (m^t (m+t x (setq z (msqrt (m-t var))))
				 (setq b (m-t a b)))
			    (m^t (m-t x z) b))))
		 (t ())))
	  ((=1//2 (hyp-mabs (m-t b a)))
	   ;; F(a, a+1/2; 1/2; z)
	   (cond ((equal (checksigntm var) '$positive)
		  ;; A&S 15.1.9
		  (m*t 1//2
		       (m+t (m^t (m1+t (setq z (msqrt var)))
				 (setq b (m-t 1//2 (m+t a b))))
			    (m^t (m-t 1. z) b))))
		 ((equal (checksigntm var) '$negative)
		  ;; A&S 15.1.19
		  (m*t (m^t (mcos (setq z (matan (msqrt (m-t var)))))
			    (setq b (m+t a b -1//2)))
		       (mcos (m*t b z))))
		 (t ())))
	  (t ()))))


;; List L contains two elements first the numerator parameter that
;;exceeds the denumerator one and is called "C", second
;;the difference of the two parameters which is called "M". 

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


(defun freevarpar(exp)
  (cond ((freevar exp) (freepar exp))
	(t nil)))

(declare-top (special serieslist var par zerosigntest productcase))

(setq par '$p)

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
	 (not (eq exp par)))
	(t (and (freepar (car exp))
		(freepar (cdr exp))))))

(defun haspar(exp)(cond ((freepar exp) nil)(t t)))

;; Confluent hypergeometric function.
;;
;; F(a;c;z)
(defun confl (l1 l2 var)
  (prog (a c a-c k m z)
     (setq a (car l1)
	   c (car l2))
     (cond ((equal c (add a a))
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
	    (return (mul (power '$%e (setq z (div var 2)))
			 (bestrig (add a (inv 2))
				  (div (mul z z) 4))))))
					 
		
     (cond ((not (hyp-integerp (setq a-c (sub a c))))
	    (go kumcheck)))
     (cond ((minusp a-c)
	    (return (erfgammared a c var))))
     (return (kummer l1 l2))
     kumcheck
     (cond ((hyp-integerp a)
	    (return (kummer l1 l2))))

     (cond ($prefer_whittaker
	    (setq m
		  (div (sub c 1) 2)
		  k
		  (add (inv 2) m (mul -1 a)))
	    (return (mul (power var (mul -1 (add (inv 2) m)))
			 (power '$%e (div var 2))
			 (whitfun k m var))))
	   (t
	    (return (fpqform l1 l2 var))))))

;; Return sqrt(%pi)*erf(%i*sqrt(x))/2/(%i*sqrt(x))
(defun hyprederf (x)
  (let ((x (mul '$%i (power x (inv 2)))))
    (mul (power '$%pi (inv 2))
	 (inv 2)
	 (inv x)
	 (list '(%erf) x))))

(defun erfgammared (a c z)
  (cond ((and (nump a)(nump c))
	 (erfgamnumred a c z))
	(t (gammareds a c z))))

(defun gammareds
    (a c z)
  (prog(m numprod result count atemp)
     (setq m (sub c a))
     (cond ((eq m 1)(return (hypredincgm a z))))
     (setq numprod
	   (prod a m)
	   count
	   2
	   atemp
	   a
	   result
	   (sub (mul 2
		     numprod
		     (inv atemp)
		     (hypredincgm atemp z))
		(mul 2
		     numprod
		     (inv (setq atemp (add atemp 1)))
		     (hypredincgm atemp z))))
     loop
     (cond ((eq count m)(return result)))
     (setq count
	   (add1 count)
	   atemp
	   (add atemp 1)
	   result
	   (add result
		(mul (power -1 count)
		     (inv (factorial (sub m
					  (sub1 count))))
		     numprod
		     (inv atemp)
		     (hypredincgm atemp z))))
     (go loop)))
(defun hypredincgm
    (a z)
  (prog()
     (setq z (mul -1 z))
     (return (mul a
		  (power z (mul -1 a))
		  (list '($%gammagreek) a z)))))
(defun prod
    (a m)
  (cond ((eq m 2) (mul a (add a 1)))
	(t (mul (add a (sub1 m))(prod a (sub1 m))))))

(defun erfgamnumred (a c z)
  (cond ((hyp-integerp (sub c (inv 2)))
	 (erfred a c z))
	(t (gammareds a c z))))

(defun erfred (a c z)
  (prog (n m)
     (setq n (sub a (inv 2))
	   m (sub c (div 3 2)))
     (cond ((not (or (greaterp n m) (minusp n)))
	    (return (thno33 n m z))))
     (cond ((and (minusp n) (minusp m))
	    (return (thno35 (mul -1 n) (mul -1 m) z))))
     (cond ((and (minusp n) (plusp m))
	    (return (thno34 (mul -1 n) m z))))
     (return (gammareds (add n (inv 2))
			(add m (div 3 2))
			z))))
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
		 (meval (list '($diff)
			      (mul (power '$%e
					  'yannis)
				   (meval (list '($diff)
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
(defun thno34
    (n m x)
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
(defun thno35
    (n m x)
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
(defun fctrl
    (a n)
  (cond ((zerop n) 1)
	((one n) a)
	(t (mul (add a (sub1 n))(fctrl a (sub1 n))))))

(defun one (x)(equal x 1))



;;(DEFUN CHECKSIGNTM			
;;       (EXPR)				
;;       (PROG (ASLIST QUEST ZEROSIGNTEST PRODUCTCASE)	
;;	     (SETQ ASLIST CHECKCOEFSIGNLIST)
;;	     (COND ((ATOM EXPR) (GO LOOP)))
;;	     (COND ((EQ (CAAR EXPR) 'MTIMES)
;;		    (SETQ PRODUCTCASE T)))
;;	     LOOP
;;	     (COND ((NULL ASLIST)
;;		    (SETQ CHECKCOEFSIGNLIST
;;			  (APPEND CHECKCOEFSIGNLIST
;;				  (LIST (CONS
;;					 EXPR
;;					 (LIST
;;					  (SETQ
;;					   QUEST
;;					   (CHECKFLAGANDACT
;;					    EXPR)))))))
;;		    (RETURN QUEST)))
;;	     (COND ((EQUAL (CAAR ASLIST) EXPR)
;;		    (RETURN (CADAR ASLIST))))
;;	     (SETQ ASLIST (CDR ASLIST))
;;	     (GO LOOP))) 

;;(DEFUN CHECKFLAGANDACT
;;       (EXPR)
;;       (COND (PRODUCTCASE (SETQ PRODUCTCASE NIL)
;;			  (FINDSIGNOFTHEIRPRODUCT (FINDSIGNOFACTORS
;;						   (CDR EXPR))))
;;	     (T (ASKSIGN ($REALPART EXPR))))) 

;;(DEFUN FINDSIGNOFACTORS
;;       (LISTOFACTORS)
;;       (COND ((NULL LISTOFACTORS) NIL)
;;	     ((EQ ZEROSIGNTEST '$ZERO) '$ZERO)
;;	     (T (APPEND (LIST (SETQ ZEROSIGNTEST
;;				    (CHECKSIGNTM (CAR
;;						  LISTOFACTORS))))
;;			(FINDSIGNOFACTORS (CDR LISTOFACTORS)))))) 

;;(DEFUN FINDSIGNOFTHEIRPRODUCT
;;       (LIST)
;;       (PROG (SIGN)
;;	     (COND ((EQ LIST '$ZERO) (RETURN '$ZERO)))
;;	     (SETQ SIGN '$POSITIVE)
;;	     LOOP
;;	     (COND ((NULL LIST) (RETURN SIGN)))
;;	     (COND ((EQ (CAR LIST) '$POSITIVE)
;;		    (SETQ LIST (CDR LIST))
;;		    (GO LOOP)))
;;	     (COND ((EQ (CAR LIST) '$NEGATIVE)
;;		    (SETQ SIGN
;;			  (CHANGESIGN SIGN)
;;			  LIST
;;			  (CDR LIST))
;;		    (GO LOOP)))
;;	     (RETURN '$ZERO))) 

;;(DEFUN CHANGESIGN
;;       (SIGN)
;;       (COND ((EQ SIGN '$POSITIVE) '$NEGATIVE) (T '$POSITIVE))) 


(setq par '$p)                           

(defun vfvp (exp)
  (m2 exp '(v freevarpar) nil))


(defun d*u
    (exp)
  (m2 exp
      '((mtimes)((coefftt)(d freepar))((coefftt)(u haspar)))
      nil))

(defun fpqform
    (l1 l2 arg)
  (list '(mqapply)
	(list '($%f array)(length l1)(length l2))
	(append (list '(mlist)) l1)
	(append (list '(mlist)) l2)
	arg))



(defun splitpfq
    (l l1 l2)
  (prog(result prodnum proden count k a1 b1)
     (setq result
	   0
	   prodnum
	   1
	   proden
	   1
	   count
	   0
	   k
	   (cadr l)
	   a1
	   (car l)
	   b1
	   (sub a1 k))
     (setq l1
	   (zl-delete a1 l1 1)
	   l2
	   (zl-delete b1 l2 1)
	   result
	   (hgfsimp l1 l2 var))
     loop
     (cond ((eq count k) (return result)))
     (setq count
	   (add1 count)
	   prodnum
	   (mul prodnum (mull l1))
	   proden
	   (mul proden (mull l2))
	   result
	   (add result
		(mul (combin k count)
		     (div prodnum proden)
		     (power var count)
		     (hgfsimp (setq l1 (incr1 l1))
			      (setq l2 (incr1 l2))
			      var))))
     (go loop)))

(defun combin
    (k count)
  (div (factorial k)
       (mul (factorial count)(factorial (sub k count)))))


;;Algor. II from thesis:minimizes differentiations
(defun algii(a b c)
  (prog (m n ap con sym m+n)
     (cond ((not (setq sym (cdras 'f (s+c a))))
	    (setq sym 0)))
     (setq  con (sub a sym))
     (setq ap sym)
     (setq m+n (add a b))
     (setq m ($entier con))
     (cond ((minusp m)(add1 m)))
     (setq ap (add (sub con m) ap))
     (setq n (add b ap))
     (cond ((and (minusp (mul n m))(greaterp (abs m) (abs n)))
	    (return (list ap (sub ap n) m+n))))
     (return  (list ap (add ap m) m+n))))
			    
			   



;;Algor. 2F1-RL from thesis:step 4:dispatch on a+m,-a+n,1/2+l cases
(defun step4
    (a b c)
  (prog (aprime m n $ratsimpexponens $ratprint newf)
     (setq alglist
	   (algii a b c)
	   aprime
	   (cadr alglist)
	   m
	   (caddr alglist)
	   n
	   (sub c (inv 2)))
     (setq $ratsimpexponens $true $ratprint $false)
     (setq newf
	   ($ratsimp (subst aprime
			    'psa
			    (power (add (inv 2)
					(mul (power (sub
						     1
						     var)
						    (inv
						     2))
					     (inv 2)))
				   (sub 1
					(mul 2 'psa))))))
     (return (subst var 'ell
		    (algiii (subst 'ell var newf)
			    m n aprime)))))

;;Pattern match for s(ymbolic) + c(onstant) in parameter
(defun s+c
    (exp)
  (m2 exp
      '((mplus)((coeffpt)(f nonnump))((coeffpp)(c $numberp)))
      nil))

(defun nonnump (z)
  (cond ((not ($numberp z)) t)
	(t nil)))

;;Algor. III from thesis:determines which Differ. Formula to use
(defun algiii (fun m n aprime)
  (prog (mm nn)
     (setq mm (abs m) nn (abs n))
     (cond ((and (nni m) (nni n))
	    (cond ((lessp m n) (return (f81 fun m n aprime)))
		  (t (return (f85 fun mm nn aprime)))))
	   ((and (hyp-negp n) (hyp-negp m))
	    (cond ((greaterp (abs n) (abs m))
		   (return (f86 fun mm nn aprime)))
		  (t (return (f82 fun mm nn aprime)))))
	   ((and (hyp-negp m) (nni n))(return (f83 fun mm nn aprime)))
	   (t (return (f84 fun mm nn aprime))))))

;;Factorial function:x*(x+1)*(x+2)...(x+n-1)
(defun factf (x n)
  (cond ((zerop n) 1)
	(t (mul x (factf (add x 1) (sub n 1))))))

;;Formula  #85 from Yannis thesis:finds by differentiating F[2,1](a,b,c,z)
;; given F[2,1](a+m,b,c+n,z) where b=-a and c=1/2, n,m integers
(defun f85 (fun m n a)
  (mul (factf (inv 2) n)
       (inv (power -1 n))
       (inv (factf (sub (add a m) n) n))
       (inv (factf (sub (inv 2) (mul a -1)) n))
       (inv (factf a (- m n)))
       (power (sub 1 'ell) (sub (sub (add 1 n) m) a))
       ($diff (mul
	       (power (sub 1 'ell) (sub (add a m) 1))
	       (power 'ell (sub 1 a))
	       ($diff (mul
		       (power 'ell (sub (add a m -1) n))
		       fun) 'ell (- m n))) 'ell n)))

;;Used to find negative things that are not integers,eg RAT's	
(defun hyp-negp(x) (cond ((equal (asksign x) '$negative) t)(t nil)))

(defun f81 (fun m n a)
  (mul (factf (add (inv 2) (- n m)) m)
       (factf (inv 2) (- n m))
       (inv (power -1 m))
       (inv (factf a m))
       (inv (factf (add (inv 2) n (sub a m)) m))
       (inv (factf (sub (inv 2) a) (- n m)))
       (inv (factf (add (inv 2) a) (- n m)))
       (power (sub 1 'ell) (sub 1 a))
       ($diff (mul 
	       (power (sub 1 'ell) (add a n (inv -2)))
	       ($diff (mul
		       (power (sub 1 'ell) (inv -2))
		       fun) 'ell (- n m))) 'ell m)))

(defun f82
    (fun m n a)
  (mul (inv (factf (sub (inv 2) n) m))
       ;; Was this both inverse?
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

(defun f83
    (fun m n a)
  (mul (factf (inv 2) n)
       (inv (factf (sub (inv 2) a) n))
       (inv (factf (add (sub (inv 2) a) n) m))
       (inv (factf (add (inv 2) a) n))
       (power (sub 1 'ell) (add m n (inv 2)))
       (power 'ell (sub (add (inv 2) a) n))
       ($diff (mul (power 'ell (sub (sub (+ m n)  a)(inv 2)))
		   ($diff (mul (power (sub 1 'ell)
				      (inv -2))
			       fun)
			  'ell
			  n))
	      'ell
	      m)))

(defun f84
    (fun m n a)
  (mul (inv (mul (factf a m) (factf (sub (inv 2) n) n)))
       (power 'ell (sub 1 a))
       ($diff (mul (power 'ell (sub (add a m n) (inv 2)))
		   ($diff (mul (power 'ell (inv -2)) fun)
			  'ell
			  n))
	      'ell
	      m)))

(defun f86
    (fun m n a)
  (mul (inv (mul (factf (sub (inv 2) n) n)
		 (factf (sub (inv 2) a) (- m n))))
       (power 'ell (add n (inv 2)))
       (power (sub 1 'ell)(add (inv 2) a))
       ($diff (mul (power 'ell a)
		   (power (sub 1 'ell)(sub m a))
		   ($diff (mul (power 'ell
				      (sub (sub (sub m n) (inv 2)) a))
			       (power (sub 1 'ell)
				      (inv -2))
			       fun) 'ell (- m n)))
	      'ell n)))


(eval-when
    #+gcl (compile)
    #-gcl (:compile-toplevel)
    (declare-top (unspecial serieslist var par zerosigntest productcase
			    fldeg flgkum listcmdiff checkcoefsignlist ))
  
    (declare-top (unspecial fun w b l alglist n c)))
