;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

;; When non-NIL, the Bessel functions of half-integral order are
;; expanded in terms of elementary functions.
(defmvar $besselexpand nil)

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

(declare-top (flonum (j[0]-bessel flonum) (j[1]-bessel flonum)
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
  "J[0](x). This is deprecated.  Use bessel_j(0,x)"        
  (cond ((numberp $x)
	 (j[0]-bessel (float $x)))
	(t (list '(%bessel_j simp) 0 $x))))


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
  "J[1](x).  This is deprecated.  Use bessel_j(1,x)"
  (cond ((numberp $x)
	 (j[1]-bessel (float $x)))
	(t (list '(%bessel_j simp) 1 $x))))

;; Bessel function of the first kind of order n
;;
;; The order n must be a non-negative real.
(defun $jn ($x $n)
  "J[n](x).  This is deprecated.  Use bessel_j(n,x)"
  (cond ((and (numberp $x) (numberp $n) (>= $n 0))
	 (multiple-value-bind (n alpha)
	     (floor (float $n))
	   (let ((jvals (make-array (1+ n) :element-type 'double-float)))
	     (slatec:dbesj (float $x) alpha (1+ n) jvals 0)
	     (narray $jarray $float n)
	     (fillarray (nsymbol-array '$jarray) jvals)
	     (aref jvals n))))
	(t (list '(%bessel_j simp) $n $x))))


;; Modified Bessel function of the first kind of order 0.  This is
;; related to J[0] via
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
  "I[0](x).  This is deprecated.  Use bessel_i(0,x)"
  (cond ((numberp $x)
	 (i[0]-bessel (float $x)))
	(t (list '(%bessel_i simp) 0 $x))))

;; Modified Bessel function of the first kind of order 1.  This is
;; related to J[1] via
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
  "I[1](x).  This is deprecated.  Use bessel_i(1,x)"
  (cond ((numberp $x) (i[1]-bessel (float $x)))
	(t (list '(%bessel_i simp) 1 $x))))

;; Modified Bessel function of the first kind of order n, where n is a
;; non-negative real.
#+nil
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

(defun bessel-i (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  We call i0
	 ;; and i1 instead of jn, if possible.
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (slatec:dbesi0 (float arg)))
		 ((= order 1)
		  (slatec:dbesi1 (float arg)))
		 (t
		  (multiple-value-bind (n alpha)
		      (floor (float order))
		    (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		      (slatec:dbesi (float (realpart arg)) alpha 1 (1+ n) jvals 0)
		      (narray $besselarray $float n)
		      (fillarray (nsymbol-array '$besselarray) jvals)
		      (aref jvals n)))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz v-ierr)
		 (slatec::zbesi (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesi ierr = ~A~%" v-ierr))
	       (narray $besselarray $complete (1+ n))
	       (dotimes (k (1+ n)
			 (arraycall 'flonum (nsymbol-array '$besselarray) n))
		 (setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))

(defun bessel-k (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  We call i0
	 ;; and i1 instead of jn, if possible.
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (slatec:dbesk0 (float arg)))
		 ((= order 1)
		  (slatec:dbesk1 (float arg)))
		 (t
		  (multiple-value-bind (n alpha)
		      (floor (float order))
		    (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		      (slatec:dbesk (float (realpart arg)) alpha 1 (1+ n) jvals 0)
		      (narray $besselarray $float n)
		      (fillarray (nsymbol-array '$besselarray) jvals)
		      (aref jvals n)))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz v-ierr)
		 (slatec::zbesk (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesk ierr = ~A~%" v-ierr))
	       (narray $besselarray $complete (1+ n))
	       (dotimes (k (1+ n)
			 (arraycall 'flonum (nsymbol-array '$besselarray) n))
		 (setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))



;; I think g0(x) = exp(-x)*I[0](x), g1(x) = exp(-x)*I[1](x), and
;; gn(x,n) = exp(-x)*I[n](x), based on some simple numerical
;; evaluations.

(defun $g0 ($x)
  (cond ((numberp $x)
	 (slatec:dbsi0e (float $x)))
	(t
	 (mul `((mexpt) $%e ,(neg $x))
	      `((%bessel_i) 0 $x)))))

(defun $g1 ($x)
  (cond ((numberp $x)
	 (slatec:dbsi1e (float $x)))
	(t
	 (mul `((mexpt) $%e ,(neg $x))
	      `((%bessel_i) 1 $x)))))


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
	(t
	 (mul `((mexpt) $%e ,(neg $x))
	      `((%bessel_i) $n $x)))))



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
  "BESSEL(arg, order) = J[order](arg). This is deprecated.  Use bessel_j(order,arg)"
  (cond ((not (and (numberp $order)
		   (numberp $arg)
		   (not (< $arg 0.0))
		   (numberp ($realpart $arg))
		   (numberp ($imagpart $arg))))
	 ;; Args aren't numeric.  Return unevaluated.
	 (list '(%bessel_j simp) $order $arg))
	((zerop ($imagpart $arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.  (Should we
	 ;; try calling j0 and j1 as appropriate instead of jn?)
	 (cond ((= $order 0)
		(slatec:dbesj0 (float $arg)))
	       ((= $order 1)
		(slatec:dbesj1 (float $arg)))
	       (t
		(multiple-value-bind (n alpha)
		    (floor (float $order))
		  (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		    ;; Use analytic continuation formula A&S 9.1.35:
		    ;;
		    ;; %j[v](z*exp(m*%pi*%i)) = exp(m*%pi*%i*v)*%j[v](z)
		    ;;
		    ;; for an integer m.  In particular, for m = 1:
		    ;;
		    ;; %j[v](-x) = exp(v*%pi*%i)*%j[v](x)
		    (cond ((>= $arg 0)
			   (slatec:dbesj (float $arg) alpha (1+ n) jvals 0)
			   (narray $besselarray $float n)
			   (fillarray (nsymbol-array '$besselarray) jvals)
			   (aref jvals n))
			  (t
			   (slatec:dbesj (- (float $arg)) alpha (1+ n) jvals 0)
			   (narray $besselarray $complete n)
			   (let ((s (cis (* $order pi))))
			     (dotimes (k (1+ n))
			       (let ((v (* s (aref jvals k))))
				 (setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
				       (simplify `((mplus)
						   ,(realpart v)
						   ((mtimes)
						    $%i
						    ,(imagpart v)))))))
			     (arraycall 'flonum (nsymbol-array '$besselarray) n)))))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float $order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz v-ierr)
		 (slatec:zbesj (float ($realpart $arg))
			       (float ($imagpart $arg))
			       alpha
			       1
			       (1+ n)
			       cyr
			       cyi
			       0
			       0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi v-nz))

	       ;; Should check the return status in v-ierr of this
	       ;; routine.
	       (when (plusp v-ierr)
		 (format t "zbesj ierr = ~A~%" v-ierr))
	       (narray $besselarray $complete (1+ n))
	       (dotimes (k (1+ n)
			 (arraycall 'flonum (nsymbol-array '$besselarray) n))
		 (setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
		       (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       '$%i
						       (aref cyi k)))
				       (aref cyr k)))))))))))

(defmfun $bessel_j (v z)
  (simplify (list '(%bessel_j) (resimplify v) (resimplify z))))

;; Bessel function of the second kind, Y[n](z), for real or complex z
;; and non-negative real n.
(defun bessel-y (order arg)
  (cond ((zerop (imagpart arg))
	 ;; We have numeric args and the first arg is purely
	 ;; real. Call the real-valued Bessel function.
	 ;;
	 ;; For negative values, use the analytic continuation formula
	 ;; A&S 9.1.36:
	 ;;
	 ;; %y[v](z*exp(m*%pi*%i)) = exp(-v*m*%pi*%i) * %y[v](z)
	 ;;       + 2*%i*sin(m*v*%pi)*cot(v*%pi)*%j[v](z)
	 ;;
	 ;; In particular for m = 1:
	 ;;
	 ;; %y[v](-z) = exp(-v*%pi*%i) * %y[v](z) + 2*%i*cos(v*%pi)*%j[v](z)
	 ;; 
	 (let ((arg (realpart arg)))
	   (cond ((zerop order)
		  (cond ((>= arg 0)
			 (slatec:dbesy0 (float arg)))
			(t
			 ;; For v = 0, this simplifies to
			 ;;
			 ;; %y[0](-z) = %y[0](z) + 2*%i*%j[0](z)
			 (simplify `((mplus)
				     ,(slatec:dbesy0 (float (- arg)))
				     ((mtimes)
				      $%i
				      ,(* 2 (slatec:dbesj0 (float (- arg))))))))))
		 ((= order 1)
		  (cond ((>= arg 0)
			 (slatec:dbesy1 (float arg)))
			(t
			 ;; For v = 1, this simplifies to
			 ;;
			 ;; %y[1](-z) = -%y[1](z) - 2*%i*%j[1](v)
			 (simplify `((mplus)
				       ,(slatec:dbesy1 (float (- arg)))
				       ((mtimes)
					$%i
					,(* -2 (slatec:dbesj1 (float (- arg))))))))))
		 (t
		  (multiple-value-bind (n alpha)
		      (floor (float order))
		    (let ((jvals (make-array (1+ n) :element-type 'double-float)))
		      (cond ((>= arg 0)
			     (slatec:dbesy (float (realpart arg)) alpha (1+ n) jvals)
			     (narray $besselarray $float n)
			     (fillarray (nsymbol-array '$besselarray) jvals)
			     (aref jvals n))
			    (t
			     (let* ((j ($bessel (- arg) order))
				    (s1 (cis (- (* order pi))))
				    (s2 (* #c(0 2) (cos (* order pi)))))
			       (slatec:dbesy (- (float arg)) alpha (1+ n) jvals)
			       (narray $yarray $complete n)
			       (dotimes (k (1+ n))
				 (let ((v (+ (* s1 (aref jvals k))
					     (* s2 (arraycall 'flonum (nsymbol-array '$besselarray)
							      k)))))
				   (setf (arraycall 'flonum (nsymbol-array '$yarray) k)
					 (simplify `((mplus)
						     ,(realpart v)
						     ((mtimes)
						      $%i
						      ,(imagpart v)))))))
			       (arraycall 'flonum (nsymbol-array '$yarray) n))))))))))
	(t
	 ;; The first arg is complex.  Use the complex-valued Bessel
	 ;; function
	 (multiple-value-bind (n alpha)
	     (floor (float order))
	   (let ((cyr (make-array (1+ n) :element-type 'double-float))
		 (cyi (make-array (1+ n) :element-type 'double-float))
		 (cwrkr (make-array (1+ n) :element-type 'double-float))
		 (cwrki (make-array (1+ n) :element-type 'double-float)))
	     (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
					v-cyr v-cyi v-nz
					v-cwrkr v-cwrki v-ierr)
		 (slatec::zbesy (float (realpart arg))
				(float (imagpart arg))
				alpha
				1
				(1+ n)
				cyr
				cyi
				0
				cwrkr
				cwrki
				0)
	       (declare (ignore v-zr v-zi v-fnu v-kode v-n
				v-cyr v-cyi v-cwrkr v-cwrki))

	       ;; We should check for errors here based on the
	       ;; value of v-ierr.
	       (when (plusp v-ierr)
		 (format t "zbesy ierr = ~A~%" v-ierr))
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


;; Initialize tables for Marsaglia's Ziggurat method of generating
;; random numbers.  See http://www.jstatsoft.org for a reference.
;;
;; Let 0 = x[0] < x[1] < x[2] <...< x[n].  Select a set of rectangles
;; with common area v such that
;;
;; x[k]*(f(x[k-1]) - f(x[k])) = v
;;
;; and
;;
;;              inf
;; v = r*f(r) + int f(x) dx
;;               r
;;
;; where r = x[n].
;;
(defun ziggurat-init (n r v scale f finv)
  ;; n = one less than the number of elements in the tables
  ;; r = x[n]
  ;; v = common area term
  ;; scale = 2^scale is the scaling to use to make integers
  ;; f = density function
  ;; finv = inverse density function
  (let ((x (make-array (1+ n) :element-type 'double-float))
	(fx (make-array (1+ n) :element-type 'double-float))
	(k-table (make-array (1+ n) :element-type '(unsigned-byte 32)))
	(w-table (make-array (1+ n) :element-type 'double-float)))
    (setf (aref x n) r)
    (loop for k from (1- n) downto 1 do
	  (let ((prev (aref x (1+ k))))
	    (setf (aref x k) (funcall finv (+ (/ v prev)
					      (funcall f prev))))
	    (setf (aref fx k) (funcall f (aref x k)))))

    (setf (aref x 0) 0d0)
    (setf (aref fx 0) (funcall f (aref x 0)))
    (setf (aref fx n) (funcall f (aref x n)))

    (loop for k from 1 to n do
	  (setf (aref k-table k)
		(floor (scale-float (/ (aref x (1- k)) (aref x k)) scale)))
	  (setf (aref w-table k)
		(* (aref x k) (expt .5d0 scale))))

    (setf (aref k-table 0) (floor (scale-float (/ (* r (funcall f r)) v) scale)))
    (setf (aref w-table 0) (* (/ v (funcall f r)) (expt 0.5d0 scale)))
    (values k-table w-table fx)))

;; Marsaglia's Ziggurat method for Gaussians
(let ((r 3.442619855899d0))
  (flet ((density (x)
	   (declare (double-float x)
		    (optimize (speed 3) (safety 0)))
	   (exp (* -0.5d0 x x))))
    (declaim (inline density))
    (multiple-value-bind (k-table w-table f-table)
	(ziggurat-init 127 r 9.91256303526217d-3 31
		       #'density
		       #'(lambda (x)
			   (sqrt (* -2 (log x)))))
      (defun gen-gaussian-variate-ziggurat (state)
	(declare (random-state state)
		 (optimize (speed 3)))
	(loop
	    ;; We really want a signed 32-bit random number. So make a
	    ;; 32-bit unsigned number, take the low 31 bits as the
	    ;; number, and use the most significant bit as the sign.
	    ;; Doing this in other ways can cause consing.
	    (let* ((ran (random (ash 1 32) state))
		   (sign (ldb (byte 1 31) ran))
		   (j (if (plusp sign)
			  (- (ldb (byte 31 0) ran))
			  (ldb (byte 31 0) ran)))
		   (i (logand j 127))
		   (x (* j (aref w-table i))))
	      (when (< (abs j) (aref k-table i))
		(return x))
	      (when (zerop i)
		(loop
		    (let ((x (/ (- (log (random 1d0 state))) r))
			  (y (- (log (random 1d0 state)))))
		      (when (> (+ y y) (* x x))
			(return-from gen-gaussian-variate-ziggurat
			  (if (plusp j)
			      (- (+ r x))
			      (+ r x)))))))
	      (when (< (* (random 1d0 state) (- (aref f-table (1- i))
						(aref f-table i)))
		       (- (density x) (aref f-table i)))
		(return x))))))))

(defun $gauss ($mean $sd)
  (cond ((and (numberp $mean) (numberp $sd))
	 (+$ (float $mean)
	     (*$ (float $sd)
		 (gen-gaussian-variate-ziggurat *random-state*))))
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


;; Define the Bessel funtion J[n](z)

(defprop %bessel_j bessel-j-simp operators)

;; Derivatives of the Bessel function.
(defprop %bessel_j
    ((n x)
     ;; Derivative wrt to order n.  A&S 9.1.64.  Do we really want to
     ;; do this?  It's quite messy.
     ;;
     ;; J[n](x)*log(x/2) - (x/2)^n*sum((-1)^k*psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes simp)
       ((%bessel_j simp) n x)
       ((%log) ((mtimes) ((rat simp) 1 2) x)))
      ((mtimes simp) -1
       ((mexpt simp) ((mtimes simp) x ((rat simp) 1 2)) n)
       ((%sum simp)
	((mtimes simp) ((mexpt simp) -1 |$%k|)
	 ((mexpt simp) ((mfactorial simp) |$%k|) -1)
	 ((mqapply simp) (($psi simp array) 0) ((mplus simp) 1 |$%k| n))
	 ((mexpt simp) ((%gamma simp) ((mplus simp) 1 |$%k| n)) -1)
	 ((mexpt simp) ((mtimes simp) x x ((rat simp) 1 4)) |$%k|))
	|$%k| 0 $inf)))
      
     ;; Derivative wrt to arg x.  A&S 9.1.30
     ((mplus)
      ((%bessel_j) ((mplus) -1 n) x)
      ((mtimes) -1 n ((%bessel_j) n x) ((mexpt) x -1))))
  grad)

;; If E is a maxima ratio with a denominator of DEN, return the ratio
;; as a Lisp rational.  Otherwise NIL.
(defun max-numeric-ratio-p (e den)
  (if (and (listp e)
	   (eq 'rat (caar e))
	   (= den (third e))
	   (integerp (second e)))
      (/ (second e) (third e))
      nil))

;; Compute the Bessel function of half-integral order.
;;
;; From A&S 10.1.1, we have
;;
;; J[n+1/2](z) = sqrt(2*z/pi)*j[n](z)
;; Y[n+1/2](z) = sqrt(2*z/pi)*y[n](z)
;;
;; where j[n](z) is the spherical bessel function of the first kind
;; and y[n](z) is the spherical bessel function of the second kind.
;;
;; A&S 10.1.8 and 10.1.9 give
;;
;; j[n](z) = 1/z*[P(n+1/2,z)*sin(z-n*pi/2) + Q(n+1/2)*cos(z-n*pi/2)]
;;
;; y[n](z) = (-1)^(n+1)*1/z*[P(n+1/2,z)*cos(z+n*pi/2) - Q(n+1/2)*sin(z+n*pi/2)]
;;

;; A&S 10.1.10
;;
;; j[n](z) = f[n](z)*sin(z) + (-1)^n*f[-n-1](z)*cos(z)
;;
;; f[0](z) = 1/z, f[1](z) = 1/z^2
;;
;; f[n-1](z) + f[n+1](z) = (2*n+1)/z*f[n](z)
;;
(defun f-fun (n z)
  (cond ((= n 0)
	 (div 1 z))
	((= n 1)
	 (div 1 (mul z z)))
	((= n -1)
	 0)
	((>= n 2)
	 ;; f[n+1](z) = (2*n+1)/z*f[n](z) - f[n-1](z) or
	 ;; f[n](z) = (2*n-1)/z*f[n-1](z) - f[n-2](z)
	 (sub (mul (div (+ n n -1) z)
		   (f-fun (1- n) z))
	      (f-fun (- n 2) z)))
	(t
	 ;; Negative n
	 ;;
	 ;; f[n-1](z) = (2*n+1)/z*f[n](z) - f[n+1](z) or
	 ;; f[n](z) = (2*n+3)/z*f[n+1](z) - f[n+2](z)
	 (sub (mul (div (+ n n 3) z)
		   (f-fun (1+ n) z))
	      (f-fun (+ n 2) z)))))

(defun bessel-j-half-order (order arg)
  "Compute J[n+1/2](z)"
  (let* ((n (floor order))
	 (sign (if (oddp n) -1 1))
	 (jn (sub (mul ($expand (f-fun n arg))
		       `((%sin) ,arg))
		  (mul sign
		       ($expand (f-fun (- (- n) 1) arg))
		       `((%cos) ,arg)))))
    (mul `((mexpt) ,(div (mul 2 arg) '$%pi) ,(div 1 2))
	 jn)))

(defun bessel-y-half-order (order arg)
  "Compute Y[n+1/2](z)"
    ;; A&S 10.1.1:
    ;; Y[n+1/2](z) = sqrt(2*z/%pi)*y[n](z)
    ;;
    ;; A&S 10.1.15:
    ;; y[n](z) = (-1)^(n+1)*j[-n-1](z)
    ;;
    ;; So
    ;; Y[n+1/2](z) = sqrt(2*z/%pi)*(-1)^(n+1)*j[-n-1](z)
    ;;             = (-1)^(n+1)*sqrt(2*z/%pi)*j[-n-1](z)
    ;;             = (-1)^(n+1)*J[-n-1/2](z)
  (let* ((n (floor order))
	 (jn (bessel-j-half-order (- (- order) 1/2) arg)))
    (if (evenp n)
	(mul -1 jn)
	jn)))
	
	

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[n-1](z)*cosh(z)]
;;
;; g[0](z) = 1/z, g[1](z) = -1/z^2
;;
;; g[n-1](z) - g[n+1](z) = (2*n+1)/z*g[n](z)
;;
;;
(defun g-fun (n z)
  (declare (type integer n))
  (cond ((= n 0)
	 (div 1 z))
	((= n 1)
	 (div -1 (mul z z)))
	((>= n 2)
	 ;; g[n](z) = g[n-2](z) - (2*n-1)/z*g[n-1](z)
	 (sub (g-fun (- n 2) z)
	       (mul (div (+ n n -1) z)
		    (g-fun (- n 1) z))))
	(t
	 ;; n is negative
	 ;;
	 ;; g[n](z) = (2*n+3)/z*g[n+1](z) + g[n+2](z)
	 (add (mul (div (+ n n 3) z)
		   (g-fun (+ n 1) z))
	      (g-fun (+ n 2) z)))))

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[-n-1](z)*cosh(z)]

(defun bessel-i-half-order (order arg)
  (let ((order (floor order)))
    (mul `((mexpt) ,(div (mul 2 arg) '$%pi) ,(div 1 2))
	 (add (mul ($expand (g-fun order arg))
		   `((%sinh) ,arg))
	      (mul ($expand (g-fun (- (+ order 1)) arg))
		   `((%cosh) ,arg))))))

;; See A&S 10.2.15
;;
;; sqrt(%pi/2/z)*K[n+1/2](z) = (%pi/2/z)*exp(-z)*sum (n+1/2,k)/(2*z)^k
;;
;; or
;;                                n
;; K[n+1/2](z) = sqrt(%pi/(2*z)) sum (n+1/2,k)/(2*z)^k
;;                               k=0
;;
;; where (A&S 10.1.9)
;;
;; (n+1/2,k) = (n+k)!/k!/(n-k)!
;;

(defun k-fun (n z)
  (declare (type unsigned-byte n))
  ;; Computes the sum above
  (let ((sum 1)
	(term 1))
    (loop for k from 0 upto n do
	  (setf term (mul term
			  (div (div (* (- n k) (+ n k 1))
				    (+ k 1))
			       (mul 2 z))))
	  (setf sum (add sum term)))
    sum))

(defun bessel-k-half-order (order arg)
  (let ((order (truncate (abs order))))
    (mul (mul `((mexpt) ,(div '$%pi (mul 2 arg)) ,(div 1 2))
	      `((mexpt) $%e ,(neg arg)))
	 (k-fun (abs order) arg))))

(defun bessel-numerical-eval-p (order arg)
  ;; Return non-NIL if we should numerically evaluate a bessel
  ;; function.  Basically, both args have to be numbers.  If both args
  ;; are integers, we don't evaluate unless $numer is true.
  (or (and (numberp order) (complex-number-p arg)
	   (or (floatp order) (floatp ($realpart arg)) (floatp ($imagpart arg))))
      (and $numer (numberp order)
	   (complex-number-p arg))))
	 
(defun bessel-j-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (when (and (numberp arg) (zerop arg)
		 (numberp order))
	;; J[v](0) = 1 if v = 0.  Otherwise 0.
	(return-from bessel-j-simp
	  (if (zerop order)
	      1
	      0)))
      (cond ((bessel-numerical-eval-p order arg)
	     ;; We have numeric order and arg and $numer is true, or
	     ;; we have either the order or arg being floating-point,
	     ;; so let's evaluate it numerically.
	     ($bessel arg order))
	    ((and (integerp order) (minusp order))
	     ;; Some special cases when the order is an integer
	     ;;
	     ;; A&S 9.1.5
	     ;; J[-n](x) = (-1)^n*J[n](x)
	     (if (evenp order)
		 (list '(%bessel_j simp) (- order) arg)
		 `((mtimes simp) -1 ((%bessel_j simp) ,(- order) ,arg))))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     (bessel-j-half-order rat-order arg)
	     )
	    (t
	     (eqtest (list '(%bessel_j) order arg)
		     exp))))))


;; Define the Bessel funtion Y[n](z)

(defmfun $bessel_y (v z)
  (simplify (list '(%bessel_y) (resimplify v) (resimplify z))))

(defprop %bessel_y bessel-y-simp operators)

(defprop %bessel_y
    ((n x)
     ;; A&S 9.1.65
     ;;
     ;; cot(n*%pi)*[diff(bessel_j(n,x),n)-%pi*bessel_y(n,x)]
     ;;  - csc(n*%pi)*diff(bessel_j(-n,x),n)-%pi*bessel_j(n,x)
     ((mplus simp)
      ((mtimes simp) $%pi ((%bessel_j simp) n x))
      ((mtimes simp)
       -1
       ((%csc simp) ((mtimes simp) $%pi n))
       ((%derivative simp) ((%bessel_j simp) ((mtimes simp) -1 n) x) x 1))
      ((mtimes simp)
       ((%cot simp) ((mtimes simp) $%pi n))
       ((mplus simp)
	((mtimes simp) -1 $%pi ((%bessel_y simp) n x))
	((%derivative simp) ((%bessel_j simp) n x) n 1))))
     ((mplus)
      ((%bessel_y) ((mplus) -1 n) x)
      ((mtimes) -1 n ((%bessel_y) n x) ((mexpt) x -1))))
  grad)

(defun bessel-y-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((bessel-numerical-eval-p order arg)
	     ;; We have numeric order and arg and $numer is true, or
	     ;; we have either the order or arg being floating-point,
	     ;; so let's evaluate it numerically.
	     (bessel-y (float order) (complex ($realpart arg) ($imagpart arg))))
	    ((and (integerp order) (minusp order))
	     ;; Special case when the order is an integer.
	     ;;
	     ;; A&S 9.1.5
	     ;; Y[-n](x) = (-1)^n*Y[n](x)
	     (if (evenp order)
		 (list '(%bessel_y) (- order) arg)
		 `((mtimes simp) -1 ((%bessel_y simp) ,(- order) ,arg))))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; Y[1/2](z) = -J[1/2](z) is a function of sin.
	     ;; Y[-1/2](z) = -J[-1/2](z) is a function of cos.
	     #+nil
	     (simplify `((mtimes) -1 ,(bessel-jy-half-order arg rat-order '%sin '%cos)))
	     (bessel-y-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_y) order arg)
		     exp))))))

;; Define the Bessel funtion I[n](z)

(defmfun $bessel_i (v z)
  (simplify (list '(%bessel_i) (resimplify v) (resimplify z))))

(defprop %bessel_i bessel-i-simp operators)

(defprop %bessel_i
    ((n x)
     ;; A&S 9.6.42
     ;;
     ;; I[n](x)*log(x/2) - (x/2)^n*sum(psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes simp)
       ((%bessel_i simp) n x)
       ((%log) ((mtimes) ((rat simp) 1 2) x)))
      ((mtimes simp) -1
       ((mexpt simp) ((mtimes simp) x ((rat simp) 1 2)) n)
       ((%sum simp)
	((mtimes simp)
	 ((mexpt simp) ((mfactorial simp) |$%k|) -1)
	 ((mqapply simp) (($psi simp array) 0) ((mplus simp) 1 |$%k| n))
	 ((mexpt simp) ((%gamma simp) ((mplus simp) 1 |$%k| n)) -1)
	 ((mexpt simp) ((mtimes simp) x x ((rat simp) 1 4)) |$%k|))
	|$%k| 0 $inf)))
     ((mplus)
      ((%bessel_i) ((mplus) -1 n) x)
      ((mtimes) -1 n ((%bessel_i) n x) ((mexpt) x -1))))
  grad)

(defun bessel-i-simp (exp ignored z)
  (declare (ignore ignored))
  (twoargcheck exp)
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((bessel-numerical-eval-p order arg)
	     (bessel-i (float order) (complex ($realpart arg) ($imagpart arg))))
	    ((and (integerp order) (minusp order))
	     ;; Some special cases when the order is an integer
	     ;;
	     ;; A&S 9.6.6
	     ;; I[-n](x) = I[n](x)
	     (list '(%bessel_i) (- order) arg))
	    ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; I[1/2](z) = sqrt(2/%pi/z)*sinh(z)
	     ;; I[-1/2](z) = sqrt(2/%pi/z)*cosh(z)
	     (bessel-i-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_i) order arg)
		     exp))))))

;; Define the Bessel funtion K[n](z)

(defmfun $bessel_k (v z)
  (simplify (list '(%bessel_k) (resimplify v) (resimplify z))))


(defprop %bessel_k bessel-k-simp operators)

(defprop %bessel_k
    ((n x)
     ;; A&S 9.6.43
     ;;
     ;; %pi/2*csc(n*%pi)*['diff(bessel_i(-n,x),n)-'diff(bessel_i(n,x),n)]
     ;;    - %pi*cot(n*%pi)*bessel_k(n,x)
     ((mplus simp)
      ((mtimes simp) -1 $%pi
       ((%bessel_k simp) n x)
       ((%cot simp) ((mtimes simp) $%pi n)))
      ((mtimes simp)
       ((rat simp) 1 2)
       $%pi
       ((%csc simp) ((mtimes simp) $%pi n))
       ((mplus simp)
	((%derivative simp) ((%bessel_i simp) ((mtimes simp) -1 n) x) n 1)
	((mtimes simp) -1
	 ((%derivative simp) ((%bessel_i simp) n x) n 1)))))
     ((mplus simp)
      ((mtimes) -1 ((%bessel_k) ((mplus) -1 n) x))
      ((mtimes) -1 n ((mexpt) x -1)
       ((%bessel_k) n x))))
  grad)

(defun bessel-k-simp (exp ignored z)
  (declare (ignore ignored))
  (let ((order (simpcheck (cadr exp) z))
	(rat-order nil))
    (let* ((arg (simpcheck (caddr exp) z)))
      (cond ((bessel-numerical-eval-p order arg)
	     (bessel-k (float order) (complex ($realpart arg) ($imagpart arg))))
	    ((and (integerp order) (minusp order))
	     ;; Some special cases when the order is an integer
	     ;;
	     ;; A&S 9.6.6
	     ;; K[-v](x) = K[v](x)
	     (list '(%bessel_k) (- order) arg))
	    ((and $besselexpand
		  (setq rat-order (max-numeric-ratio-p order 2)))
	     ;; When order is a fraction with a denominator of 2, we
	     ;; can express the result in terms of elementary
	     ;; functions.
	     ;;
	     ;; K[1/2](z) = sqrt(2/%pi/z)*exp(-z) = K[1/2](z)
	     (bessel-k-half-order rat-order arg))
	    (t
	     (eqtest (list '(%bessel_k) order arg)
		     exp))))))
