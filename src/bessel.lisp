;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

;; Temporarily we establish an array convention for conversion
;; of this file to new type arrays.

(eval-when (compile eval)

;; It is more efficient to use the value cell, and we can probably
;; do this everywhere, but for now just use it in this file.
	   
(defmacro nsymbol-array (x) `(symbol-value ,x))
;(defmacro nsymbol-array (x) `(get ,x 'array))

(defmacro narray (x typ &rest dims) typ
  `(setf (nsymbol-array ',x)
	 (make-array
	   ,(if (cdr dims) `(mapcar '1+ (list ,@ dims))
	      `(1+ ,(car dims))))))
)

(declare-top(flonum (j[0]-bessel flonum) (j[1]-bessel flonum)
		 (j[n]-bessel flonum fixnum) (i[0]-bessel flonum)
		 (i[1]-bessel flonum) (i[n]-bessel flonum fixnum)
		 (g[0]-bessel flonum) (g[1]-bessel flonum)
		 (g[n]-bessel flonum fixnum))
	 (flonum x z y xa sx0 sq co si q p)
	 (special $jarray $iarray $garray)
	 (array* (flonum j-bessel-array 1. i-bessel-array 1.
			 g-bessel-array 1.))
	 (array* (flonum $jarray 1. $iarray 1. $garray 1.))
	 (*fexpr $array)) 

#-(or cl NIL)
(and (not (get '*f 'subr)) 
     (mapc #'(lambda (x) (putprop x '(arith fasl dsk liblsp) 'autoload))
	   '(*f //f _f +f -f)))

#-NIL
(declare-top(flonum (*f flonum flonum) (//f flonum flonum) 
		 (_f flonum fixnum) (+f flonum flonum) (-f flonum flonum))
	 (*expr *f //f _f +f -f))

#+(or cl NIL)
(eval-when (eval compile)
  (defmacro *f (a b) `(*$ ,a ,b))
  (defmacro //f (a b) `(//$ ,a ,b))
  (defmacro +f (a b) `(+$ ,a ,b))
  (defmacro -f (a b) `(-$ ,a ,b))
  ;_f isn't used here.  That would be scale-float, no open-code version.
  )


;;
;; Bessel function of the first kind of order 0.
;;
;; One definition is
;;
;;         INF
;;         ====       k  2 k
;;         \     (- 1)  z
;;          >    -----------
;;         /       2 k   2
;;         ====   2    k!
;;         k = 0
;;
;; We only support computing this for real z.
;;
(defun j[0]-bessel (x) 
   (slatec:dbesj0 (float x 1d0)))

(defun $j0 ($x)
  (cond ((numberp $x)
	 (j[0]-bessel (float $x)))
	(t (list '($j0 simp) $x))))


;; Bessel function of the first kind of order 1.
;;
;; One definition is
;;
;;      INF
;;      ====   - 2 k - 1      k  2 k + 1
;;      \     2          (- 1)  z
;;       >    --------------------------
;;      /            k! (k + 1)!
;;      ====
;;      k = 0

(defun j[1]-bessel (x) 
   (slatec:dbesj1 (float x 1d0)))

(defun $j1 ($x)
  (cond ((numberp $x)
	 (j[1]-bessel (float $x)))
	(t (list '($j1 simp) $x))))

;; Bessel function of the first kind of order n
;;
;; The order n must be a non-negative real.
(defun $jn ($x $n)
  (cond ((and (numberp $x) (numberp $n) (>= $n 0))
	 (multiple-value-bind (n alpha)
	     (floor (float $n))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesj (float $x) alpha (1+ n) jvals 0)
	     (narray $jarray $float n)
	     (fillarray (nsymbol-array '$jarray) jvals)
	     (aref jvals n))))
	(t (list '($jn simp) $x $n))))


;; Bessel function of the second kind of order 0.  This is related to
;; J[0] via
;;
;; I[0](z) = J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;        INF
;;        ====         2 k
;;        \           z
;;         >    ----------------
;;        /         2 k   2 
;;        ====     2    k!
;;        k = 0

(defun i[0]-bessel (x)
   (slatec:dbesi0 (float x 1d0)))

(defun $i0 ($x)
  (cond ((numberp $x)
	 (i[0]-bessel (float $x)))
	(t (list '($i0 simp) $x))))

;; Bessel function of the second kind of order 1.  This is related to
;; J[1] via
;;
;; I[1](z) = exp(-%pi*%I/2)*J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;       INF
;;       ====         2 k
;;       \           z
;;        >    ----------------
;;       /      2 k
;;       ====  2    k! (k + 1)!
;;       k = 0

(defun i[1]-bessel (x)
  (slatec:dbesi1 (float x 1d0)))

(defun $i1 ($x)
  (cond ((numberp $x) (i[1]-bessel (float $x)))
	(t (list '($i1 simp) $x))))

;; Bessel function of the second kind of order n, where n is a
;; non-negative real.
(defun $in ($x $n)
  (cond ((and (numberp $x) (numberp $n) (>= $n 0))
	 (multiple-value-bind (n alpha)
	     (floor (float $n))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesi (float $x) alpha 1 (1+ n) jvals 0)
	     (narray $iarray $float n)
	     (fillarray (nsymbol-array '$iarray) jvals)
	     (aref jvals n))))
	(t (list '($in simp) $x $n))))

;; I think g0(x) = exp(-x)*I[0](x), g1(x) = exp(-x)*I[1](x), and
;; gn(x,n) = exp(-x)*I[n](x), based on some simple numerical
;; evaluations.

(defun $g0 ($x)
  (cond ((numberp $x)
	 (slatec:dbsi0e (float $x)))
	(t (list '($g0 simp) $x))))

(defun $g1 ($x)
  (cond ((numberp $x)
	 (slatec:dbsi1e (float $x)))
	(t (list '($g1 simp) $x))))


(declare-top (fixnum i n) (flonum x q1 q0 fn fi b1 b0 b an a1 a0 a)) 

(defun $gn ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (multiple-value-bind (n alpha)
	     (floor (float $n))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesi (float $x) alpha 2 (1+ n) jvals 0)
	     (narray $iarray $float n)
	     (fillarray (nsymbol-array '$iarray) jvals)
	     (aref jvals n))))
	(t (list '(gn simp) $x $n))))



(declare-top(flonum rz cz a y $t t0 t1 d r1 rp sqrp rnpa r2 ta rn rl rnp rr cr rs cs rlam
		 clam qlam s phi rsum csum)
	 (fixnum n k1 k m mpo ln l ind)
	 (notype ($bessel notype notype) (bessel flonum flonum flonum))
	 (array* (flonum rj-bessel-array 1. cj-bessel-array 1.)
		 (notype $besselarray 1.))
	 (*fexpr $array))

;; Bessel function of the first kind for real or complex arg and real
;; non-negative order.
(defun $bessel ($arg $order)
  (let ((a (float $order)))
    (cond ((not (and (numberp $order)
		     (not (< a 0.0))
		     (numberp ($realpart $arg))
		     (numberp ($imagpart $arg))))
	   ;; Args aren't numeric.  Return unevaluated.
	   (list '($bessel simp) $arg $order))
	  ((zerop ($imagpart $arg))
	   ;; We have numeric args and the first arg is purely
	   ;; real. Call the real-valued Bessel function.  (Should we
	   ;; try calling j0 and j1 as appropriate instead of jn?)
	   (multiple-value-bind (n alpha)
	     (floor (float $order))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesj (float $arg) alpha (1+ n) jvals 0)
	     (narray $besselarray $float n)
	     (fillarray (nsymbol-array '$besselarray) jvals)
	     (aref jvals n))))
	  (t
	   ;; The first arg is complex.  Use the complex-valued Bessel
	   ;; function
	   (multiple-value-bind (n alpha)
	       (floor (float $order))
	     (let ((cyr (make-array (1+ n) :element-type 'double-float))
		   (cyi (make-array (1+ n) :element-type 'double-float)))
	       (slatec:zbesj (float ($realpart $arg))
			     (float ($imagpart $arg))
			     alpha
			     1
			     (1+ n)
			     cyr
			     cyi
			     0
			     0)
	       (narray $besselarray $complete (1+ n))
	       (dotimes (k (1+ n)
			 (arraycall 'flonum (nsymbol-array '$besselarray) n))
		 (setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))

(declare-top(flonum rz y rs cs third sin60 term sum fi cossum sinsum sign (airy flonum)))

;here is Ai'
;airy1(z):=if z = 0. then -1/(gamma(1/3.)*3.^(1/3.))
;else block([zz],z:-z,zz:2./3.*z^(3./2.),bessel(zz,4./3.),
;j:realpart(2/(3.*zz)*besselarray[0]-besselarray[1]),
;-1/3.*z*(j-realpart(bessel(zz,2./3.))));

(defun $airy ($arg)
  (cond ((numberp $arg)
	 (slatec:dai (float $arg)))
	(t
	 (list '($airy simp) $arg))))

(declare-top (flonum im re ys xs y x c t2 t1 s2 s1 s r2 r1 lamb h2 h)
	 (fixnum np1 n nu capn)
	 (notype (z-function flonum flonum))) 

(defun z-function (x y) 
       ((lambda (xs ys capn nu np1 h h2 lamb r1 r2 s s1 s2 t1 t2 c bool re im) 
		(setq xs (cond ((> 0.0 x) -1.0) (t 1.0)))
		(setq ys (cond ((> 0.0 y) -1.0) (t 1.0)))
		(setq x (abs x) y (abs y))
		(cond ((and (> 4.29 y) (> 5.33 x))
		       (setq s (*$ (1+$ (*$ -0.23310023 y))
				   (sqrt (1+$ (*$ -0.035198873 x x)))))
		       (setq h (*$ 1.6 s) h2 (*$ 2.0 h) capn (f+ 6. (fix (*$ 23.0 s))))
		       (setq nu (f+ 9. (fix (*$ 21.0 s)))))
		      (t (setq h 0.0) (setq capn 0.) (setq nu 8.)))
		(and (> h 0.0) (setq lamb (^$ h2 capn)))
		(setq bool (or (= h 0.0) (= lamb 0.0)))
		(do ((n nu (f1- n)))
		    ((> 0. n))
		    (setq np1 (f1+ n))
		    (setq t1 (+$ h (*$ (float np1) r1) y))
		    (setq t2 (-$ x (*$ (float np1) r2)))
		    (setq c (//$ 0.5 (+$ (*$ t1 t1) (*$ t2 t2))))
		    (setq r1 (*$ c t1) r2 (*$ c t2))
		    (cond ((and (> h 0.0) (not (< capn n)))
			   (setq t1 (+$ s1 lamb) s1 (-$ (*$ r1 t1) (*$ r2 s2)))
			   (setq s2 (+$ (*$ r1 s2) (*$ r2 t1)) lamb (//$ lamb h2)))))
		(setq im (cond ((= y 0.0) (*$ 1.77245384 (exp (-$ (*$ x x)))))
			       (t (*$ 2.0 (cond (bool r1) (t s1))))))
		(setq re (*$ -2.0 (cond (bool r2) (t s2))))
		(cond ((> ys 0.0) (setq re (*$ re xs)))
		      (t (setq r1 (*$ 3.5449077 (exp (-$ (*$ y y) (*$ x x)))))
			 (setq r2 (*$ 2.0 x y))
			 (setq re (*$ (-$ re (*$ r1 (sin r2))) xs))
			 (setq im (-$ (*$ r1 (cos r2)) im))))
		(list '(mlist simp) re im))
	(cond ((> 0.0 x) -1.0) (t 1.0))
	(cond ((> 0.0 x) -1.0) (t 1.0))
	0. 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 nil 0.0 0.0)) 

(defun $nzeta ($z) 
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       (meval1 '$%i)
						       (caddr $w)))
				       (cadr $w)))))
	      (t (return (list '($nzeta simp) $z))))))


(defun $nzetar ($z)
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (cadr $w)))
	      (t (return (list '($nzetar simp) $z))))))


(defun $nzetai ($z)
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (caddr $w)))
	      (t (return (list '($nzetai simp) $z))))))


(declare-top (fixnum i) (flonum (gauss) te)) 

(defun gauss nil
  (do ((i 0. (f1+ i))
       ;;are these random numbers supposed to be negative too?
       (te 0.0 (+$ te (*$ (float (random #+cl most-positive-fixnum
					 #-cl #. (^ 2 30))) 1.45519152e-11))))
      ((= i 12.) te)))


(defun $gauss ($mean $sd)
  (cond ((and (numberp $mean) (numberp $sd))
	 (+$ (float $mean) (*$ (float $sd) (gauss))))
	(t (list '($gauss simp) $mean $sd))))


(declare-top (flonum x w y (expint flonum)))

;; I think this is the function E1(x).  At least some simple numerical
;; tests show that this expint matches the function de1 from SLATEC

;; Exponential integral E1(x).  The Cauchy principal value is used for
;; negative x.
(defun $expint (x)
  (cond ((numberp x)
	 (values (slatec:de1 (float x))))
	(t
	 (list '($expint simp) x))))
