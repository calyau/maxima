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

(defun j[0]-bessel (x) 
       ((lambda (xa p q si co sq sx0 y z) 
		(cond ((> xa 8.0)
		       (setq y (+$ -0.7853982 xa) si (sin y) co (cos y))
		       (setq y (//$ 8.0 xa) z (*$ y y) sq (sqrt y))
		       (setq p (+$ 0.2820948
				    (*$ z (+$ -3.096437e-4
					      (*$ 6.943574e-6 z)))))
		       (setq q (//$ (*$ y (+$ 7.030992 (*$ 0.7550996 z)))
				    (+$ 1595.15 (*$ z (+$ 185.9156 z)))))
		       (*$ (+$ (*$ co p) (*$ q si)) sq))
		      ((> xa 4.0)
		       (setq y (*$ 0.015625 (*$ xa xa)) z (-$ (1-$ y)))
		       (setq sx0 5.5200781 p (-$ xa sx0))
		       (//$ (*$ p
				(+$ sx0 xa)
				(+$ 0.1920038
				     (*$ z
					 (+$ 0.2025329
					      y
					      (*$ z
						  (+$ 0.2290394
						       y
						       (*$ z
							   (+$ -0.3228404
								(*$ -0.70066 z)))))))))
			    (+$ 12.18896
				 (*$ y
				     (+$ 13.64497
					  (*$ y
					      (+$ 7.894887
						   (*$ y (+$ 2.775489 y)))))))))
		      (t (setq y (*$ 0.0625 (*$ xa xa)) sx0 2.40482554)
			 (setq p (-$ xa sx0))
			 (//$ (*$ p
				  (+$ sx0 xa)
				  (+$ -6.171667
				       (*$ y
					   (+$ 5.953519
						(*$ y
						    (+$ -1.754611
							 (*$ 0.173663 y)))))))
			      (+$ 35.6919 (*$ y (+$ 9.590446 y)))))))
	(abs x) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)) 

(defun $j0 ($x)
  (cond ((numberp $x) (j[0]-bessel (float $x)))
	(t (list '($j0 simp) $x))))


(defun j[1]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa p q si co sq sx0 rj1 y z) 
     	 (declare (flonum z y xa rj1 sx0 sq co si q p))
	 (setq xa (abs x))
	 (cond ((> xa 8.0)
		(setq y (+$ -2.356194 xa) si (sin y) co (cos y))
		(setq y (//$ 8.0 xa) z (*$ y y) sq (sqrt y))
		(setq p (+$ 0.2820948 (*$ z (+$ 5.162034e-4 (*$ -9.002696e-6 z)))))
		(setq q (//$ (*$ y (+$ 50.53199 (*$ 4.999898 z)))
			     (+$ 3821.467 (*$ z (+$ 394.4419 z)))))
		(setq rj1 (*$ (-$ (*$ co p) (*$ q si)) sq)))
	       ((> xa 4.0)
		(setq y (*$ 0.015625 (*$ xa xa)) z (-$ (1-$ y)))
		(setq sx0 7.0155867 p (-$ xa sx0))
		(setq rj1
		      (//$ (*$ p
			       xa
			       (+$ sx0 xa)
			       (+$ 0.04297259
				    (*$ z
					(+$ 0.06689943
					     (*$ z
						 (+$ -0.05380065
						      (*$ z
							  (+$ -0.1045012
							       (*$ -0.04185412
								   z)))))))))
			   (+$ 8.886393
				(*$ y (+$ 8.204713 (*$ y (+$ 3.566279 y))))))))
	       (t (setq y (*$ 0.0625 (*$ xa xa)) sx0 3.83170596 p (-$ xa sx0))
		  (setq rj1
			(//$ (*$ p
				 xa
				 (+$ sx0 xa)
				 (+$ -4.665107
				      (*$ y (+$ 2.497075 (*$ -0.3222962 y)))))
			     (+$ 136.9859
				  (*$ y (+$ 51.3648 (*$ y (+$ 9.447542 y)))))))))
	 (and (< x 0.0) (setq rj1 (-$ rj1)))
	 rj1)
	(abs x) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)) 

(defun $j1 ($x)
  (cond ((numberp $x) (j[1]-bessel (float $x)))
	(t (list '($j1 simp) $x))))

(defun j[n]-bessel (x n) 
       (declare (fixnum n) (flonum x)) 
       (prog (a0 a1 ak b0 b1 bk ck den fm fi gn ns qk rj0 rjn m) 
	     (declare #-cl (fixnum ns m)
		      (flonum rjn rj0 qk gn fi fm den ck bk b1 b0 ak a1 a0))
	     (setq ns (cond ((< n 0.) -1.) (t 1.)) n (abs n))
	     (*rearray 'j-bessel-array)
	     (narray j-bessel-array flonum (f1+ n))
	     (setq fm (float (f1+ n)) fi (*$ 1.25 (abs x)))
	     (setq m (fix (cond ((> fm fi) fm) (t fi))))
	     (setq fi (float (f+ m m)) fm fi qk (//$ x fm))
	     (cond ((> (abs x) 2.0e-4)
		    (setq a0 -1.0 a1 0.0 b0 0.0 b1 1.0)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      ck (//$ fi (abs x)) 
			      ak (-$ (*$ a1 ck) a0))
			(setq bk (-$ (*$ b1 ck) b0) gn qk a0 a1 a1 ak b0 b1 b1 bk)
			(setq qk (//$ ak bk))
			(or (> (abs (//$ (-$ qk gn) qk)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq qk (-$ qk)))))
	     (do ((i m (f1- i)))
		 ((> 1. i))
		 (declare (fixnum i))
		 (setq den (-$ fm (*$ qk x)))
		 (and (= den 0.0) (setq den (*$ 1.0e-7 fm)))
		 (setq qk (//$ x den))
		 (or (< n i) (setf (arraycall 'flonum
					      (nsymbol-array 'j-bessel-array)
					      i) qk))
		 (setq fm (+$ -2.0 fm)))
	     (cond ((> 1.0 (abs qk)) (setq rj0 (j[0]-bessel x) rjn (*$ qk rj0)))
		   (t (setq rjn (j[1]-bessel x) rj0 (//$ rjn qk))))
	     (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) 0.) rj0)
	     (or (> n 0.) (return (arraycall 'flonum (nsymbol-array 'j-bessel-array) 0.)))
	     (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) 1.) rjn)
	     (or (> n 1.)
		 (return (setf (arraycall 'flonum
					  (nsymbol-array 'j-bessel-array) 1.)
				(*$ (float ns)
				    (arraycall 'flonum
					       (nsymbol-array
						 'j-bessel-array) 1.)))))
	     (and (= x 0.0) (return 0.0))
	     (do ((i 2. (f1+ i)))
		 ((> i n))
		 (declare (fixnum i))
		 (cond ((or (> (abs (arraycall 'flonum
					       (nsymbol-array
						 'j-bessel-array) i)) 1.0)
			    (> (abs rjn) (//$ #+cl *small-flonum*
					      #-cl 1.0e-38 ;won't read
					      (abs (arraycall 'flonum (nsymbol-array 'j-bessel-array) i)))))
			(setq rjn (*$ (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) rjn)))
		       (t (setq rjn 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) rjn))
	     (and (< ns 0.)
		  (do ((i 1. (f+ i 2.)))
		      ((> i n))
		      (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) (-$ (arraycall 'flonum (nsymbol-array 'j-bessel-array) i)))))
	     (return (arraycall 'flonum (nsymbol-array 'j-bessel-array) n)))) 

(defun $jn ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (j[n]-bessel (float $x) $n)
	 (narray $jarray $float (abs $n))
	 ;(apply '$array (list '$jarray '$float (abs $n)))
	 (fillarray (nsymbol-array '$jarray) (nsymbol-array 'j-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$jarray) (abs $n)))
	(t (list '($jn simp) $x $n))))



(defun i[0]-bessel (x) 
       (declare (flonum x)) 
       ((lambda (xa y z) 
		(declare (flonum z y xa)) 
		(setq xa (abs x))
		(cond ((> 4.0 xa)
		       (setq z (*$ 0.0625 (*$ xa xa)))
		       (//$ (+$ -162.6391
				 (*$ z
				     (+$ -585.5938
					  (*$ z (+$ -402.5407 (*$ -75.72017 z))))))
			    (+$ -162.6391
				 (*$ z (+$ 64.96299 (*$ z (+$ -11.84469 z)))))))
		      (t (setq y (//$ 4.0 xa) z (-$ (1-$ y)))
			 (*$ (exp xa)
			     (sqrt y)
			     (//$ (+$ 2.67093
				       (*$ z (+$ 2.470948 (*$ z (+$ 6.271432 z))))))
			     (+$ 0.5528884
				  (*$ z
				      (+$ 0.4861227
					   (*$ z
					       (+$ 1.281496 (*$ 0.1555914 z))))))))))
	(abs x) 0.0 0.0)) 

(defun $i0 ($x)
  (cond ((numberp $x) (i[0]-bessel (float $x)))
	(t (list '($i0 simp) $x))))

(defun i[1]-bessel (x) 
       (declare (flonum x)) 
       ((lambda (xa y z ri1) 
		(declare (flonum z y xa ri1)) 
		(cond ((> 4.0 xa)
		       (setq y (*$ 0.25 xa) z (*$ y y))
		       (setq ri1
			     (//$ (*$ y
				      (+$ -569.784
					   (*$ z
					       (+$ -947.9975
						    (*$ z
							(+$ -405.4861
							     (*$ -53.66977 z)))))))
				  (+$ -284.892
				       (*$ z
					   (+$ 95.78535 (*$ z (+$ -14.45951 z))))))))
		      (t (setq z (1+$ (//$ -4.0 xa)))
			 (setq ri1
			       (*$ (exp xa)
				   (//$ (sqrt xa))
				   (//$ (+$ 0.9980789
					     (*$ z
						 (+$ -0.3663376
						      (*$ z (+$ 2.818702 z))))))
				   (+$ 0.3568149
					(*$ z
					    (+$ -0.08379694
						 (*$ z
						     (+$ 0.9826178
							  (*$ z
							      (+$ 0.4946486
								   (*$ 0.0251859
								       z))))))))))))
		(and (< x 0.0) (setq ri1 (-$ ri1)))
		ri1)
	(abs x) 0.0 0.0 0.0)) 

(defun $i1 ($x)
  (cond ((numberp $x) (i[1]-bessel (float $x)))
	(t (list '($i1 simp) $x))))


(defun i[n]-bessel (x n) 
       (declare (fixnum n) (flonum x)) 
       (prog (a a0 a1 an b b0 b1 fi fn q0 q1) 
	     (declare (flonum q1 q0 fn fi b1 b0 b an a1 a0 a))
	     (setq n (abs n))
	     (*rearray 'i-bessel-array)
	     (narray i-bessel-array flonum (f1+ n))
	     (and (= n 0.) (go $l9))
	     (setq fn (float (f+ n n)))
	     (setq q1 (//$ x fn))
	     (cond ((> (abs x) 3.0e-4)
		    (setq a0 1.0 a1 0.0 b0 0.0 b1 1.0 fi fn)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      an (//$ fi (abs x)) 
			      a (+$ a0 (*$ a1 an)))
			(setq b (+$ b0 (*$ an b1)))
			(setq a0 a1 b0 b1 a1 a b1 b q0 q1 q1 (//$ a b))
			(or (> (abs (//$ (-$ q1 q0) q1)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq q1 (-$ q1)))))
	     (do ((i n (f1- i)))
		 ((> 0. i))
		 (declare (fixnum i))
		 (setq q1 (//$ x (+$ fn (*$ q1 x))))
		 (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) i) q1)
		 (setq fn (+$ -2.0 fn)))
	$l9  (setq fi (i[0]-bessel x))
	     (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) 0.) fi)
	     (and (or (= x 0.0) (= n 0.)) (return (arraycall 'flonum (nsymbol-array 'i-bessel-array) n)))
	     (do ((i 1. (f1+ i)))
		 ((> i n))
		 (declare (fixnum i))
		 (cond ((or (> (abs (arraycall 'flonum (nsymbol-array 'i-bessel-array) i)) 1.0)
			    (> (abs fi) (//$ #-cl 1.0e-38 #+cl *small-flonum*  (abs (arraycall 'flonum (nsymbol-array 'i-bessel-array) i)))))
			(setq fi (*$ fi (arraycall 'flonum (nsymbol-array 'i-bessel-array) i))))
		       (t (setq fi 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) i) fi))
	     (return (arraycall 'flonum (nsymbol-array 'i-bessel-array) n)))) 

(defun $in ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (i[n]-bessel (float $x) $n)
	 (narray $iarray $float (abs $n))
;	 (apply '$array (list '$iarray '$float (abs $n)))
	 (fillarray (nsymbol-array '$iarray) (nsymbol-array 'i-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$iarray) (abs $n)))
	(t (list '($in simp) $x $n))))

(defun g[0]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa y z) 
		(declare (flonum z y xa))
		(cond ((> 4.0 xa)
		       (setq z (*$ 0.0625 (*$ xa xa)))
		       (//$ (+$ -162.6391
				 (*$ z
				     (+$ -585.5938
					  (*$ z (+$ -402.5407 (*$ -75.72017 z))))))
			    (+$ -162.6391
				 (*$ z (+$ 64.96299 (*$ z (+$ -11.84469 z)))))
			    (exp xa)))
		      (t (setq y (//$ 4.0 xa))
			 (setq z (-$ (1-$ y)))
			 (*$ (sqrt y)
			     (//$ (+$ 2.67093
				       (*$ z (+$ 2.470948 (*$ z (+$ 6.271432 z))))))
			     (+$ 0.5528884
				  (*$ z
				      (+$ 0.4861227
					   (*$ z
					       (+$ 1.281496 (*$ 0.1555914 z))))))))))
	(abs x) 0.0 0.0)) 

(defun $g0 ($x)
  (cond ((numberp $x) (g[0]-bessel (float $x)))
	(t (list '($g0 simp) $x))))

(defun g[1]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa y z ri1) 
		(declare (flonum z y xa ri1))
		(cond ((> 4.0 xa)
		       (setq y (*$ 0.25 xa) z (*$ y y))
		       (setq ri1
			     (//$ (*$ y
				      (+$ -569.784
					   (*$ z
					       (+$ -947.9975
						    (*$ z
							(+$ -405.4861
							     (*$ -53.66977 z)))))))
				  (+$ -284.892
				       (*$ z (+$ 95.78535 (*$ z (+$ -14.45951 z)))))
				  (exp xa))))
		      (t (setq z (1+$ (//$ -4.0 xa)))
			 (setq ri1
			       (//$ (+$ 0.3568149
					 (*$ z
					     (+$ -0.08379694
						  (*$ z
						      (+$ 0.9826178
							   (*$ z
							       (+$ 0.4946486
								    (*$ 0.0251859
									z))))))))
				    (+$ 0.9980789
					 (*$ z
					     (+$ -0.3663376 (*$ z (+$ 2.818702 z)))))
				    (sqrt xa)))))
		(and (< x 0.0) (setq ri1 (-$ ri1)))
		ri1)
	(abs x) 0.0 0.0 0.0)) 

(defun $g1 ($x)
  (cond ((numberp $x) (g[1]-bessel (float $x)))
	(t (list '($g1 simp) $x))))


(declare-top (fixnum i n) (flonum x q1 q0 fn fi b1 b0 b an a1 a0 a)) 

(defun g[n]-bessel (x n) 
       (prog (a a0 a1 an b b0 b1 fi fn q0 q1) 
	     (setq n (abs n))
	     (*rearray 'g-bessel-array)
	     (narray g-bessel-array flonum (f1+ n))
	     (and (= n 0.) (go $l9))
	     (setq fn (float (f+ n n)) q1 (//$ x fn))
	     (cond ((> (abs x) 3.0e-4)
		    (setq a0 1.0 a1 0.0 b0 0.0 b1 1.0 fi fn)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      an (//$ fi (abs x)) 
			      a (+$ a0 (*$ a1 an)))
			(setq b (+$ b0 (*$ an b1)))
			(setq a0 a1 b0 b1 a1 a b1 b q0 q1 q1 (//$ a b))
			(or (> (abs (//$ (-$ q1 q0) q1)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq q1 (-$ q1)))))
	     (do ((i n (f1- i)))
		 ((> 0. i))
		 (setq q1 (//$ x (+$ fn (*$ q1 x))))
		 (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) i) q1)
		 (setq fn (+$ -2.0 fn)))
	$l9  (setq fi (g[0]-bessel x))
	     (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) 0.) fi)
	     (and (or (= x 0.0) (= n 0.)) (return (arraycall 'flonum (nsymbol-array 'g-bessel-array) n)))
	     (do ((i 1. (f1+ i)))
		 ((> i n))
		 (cond ((or (> (abs (arraycall 'flonum (nsymbol-array 'g-bessel-array) i)) 1.0)
			    (> (abs fi) (//$ #-cl 1.0e-38 #+cl *small-flonum*
					     (abs (arraycall 'flonum (nsymbol-array 'g-bessel-array) i)))))
			(setq fi (*$ fi (arraycall 'flonum (nsymbol-array 'g-bessel-array) i))))
		       (t (setq fi 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) i) fi))
	     (return (arraycall 'flonum (nsymbol-array 'g-bessel-array) n)))) 

(defun $gn ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (g[n]-bessel (float $x) $n)
	 (narray $garray $float (abs $n))
	 ;(apply '$array (list '$garray '$float (abs $n)))
	 (fillarray (nsymbol-array '$garray) (nsymbol-array 'g-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$garray) (abs $n)))
	(t (list '(gn simp) $x $n))))

(declare-top(flonum rz cz a y $t t0 t1 d r1 rp sqrp rnpa r2 ta rn rl rnp rr cr rs cs rlam
		 clam qlam s phi rsum csum)
	 (fixnum n k1 k m mpo ln l ind)
	 (notype ($bessel notype notype) (bessel flonum flonum flonum))
	 (array* (flonum rj-bessel-array 1. cj-bessel-array 1.)
		 (notype $besselarray 1.))
	 (*fexpr $array))

(defun bessel (rz cz a) 
  (prog (n y $t t0 t1 k1 d r1 rp sqrp rnpa r2 m ta rn rl mpo ln rnp rr cr rs cs
	 rlam clam qlam ind s phi rsum csum l) 
	(setq n (fix a) a (-$ a (float n)) ln (f1+ n) y (abs cz))
	(narray rj-bessel-array flonum ln)
	(narray cj-bessel-array flonum ln)
	(go l13)
l9	(cond
	 ((not (< 10.0 $t))
	  (setq $t (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ $t 5.794e-5)
						       -1.76148e-3)
						       $t)
						   0.0208641)
					      $t)
					  -0.129013)
				     $t)
				 0.85777)
			    $t)
			1.0125)))
	 (t (setq t0  (+$ -0.5 (log $t))
		  t1 (//$  (-$ 0.5 (log t0)) (1+$ t0))
		  $t (//$ $t (+$ (*$ t1 t0) t0)))))
	(cond ((> k1 0.) (setq r2 (*$ $t r2)) (go l25))
	      (t (setq r1 (*$ $t (float n))) (go l22)))
   l13  (cond ((and (= rz 0.0) (= y 0.0))
	       (or (= a 0.0) (MAXIMA-ERROR '|&bessel function evaluated at branch point|))
	       (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) 1.0)
	       (return (cond ((= n 0.) 1.0) (t 0.0)))))
	(setq d 17.5045 r1 0.0)
	(cond ((> n 0.) (setq $t (//$ d (float (f* 2 n))) k1 0.) (go l9)))
   l22	(setq rp (+$ (*f y y) (*f rz rz)) sqrp (sqrt rp) r2 (*$ sqrp 1.3591))
	(cond ((> d y)
	       (setq $t (//$ (*$ (-$ d y) 0.356) sqrp) k1 1.)
	       (go l9)))
l25	(cond ((> r2 r1) (setq r1 r2)))
	(setq m (f1+ (fix r1)) ta (*$ a 2.0) rn 1.0 rl 1.0)
	(setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) 1.0)
	(setq mpo (f1+ m))
	(do ((k 2. (f+ k 1.)))
	    ((> k mpo))
	    (setq rnp (1+$ rn) rl (//$ (*$ (+$ ta rn) rl) rnp))
	    (cond ((not (< ln k)) (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) (f1- k)) rl)))
	    (setq rn rnp))
	(setq rr 0.0 cr 0.0 rs 0.0 cs 0.0)
	(do
	 ((k 1. (f1+ k)))
	 ((> k m))
	 (setq l (f- mpo k) rn (1-$ rnp) rnpa (+$ ta (*$ rn 2.0)))
	 (setq rlam (-$ (+$ (*f y cr) rnpa) (*f rz rr)) clam (+$ (*f y rr) (*f rz cr)))
	 (setq qlam (+$ (*f rlam rlam) (*f clam clam)))
	 (cond ((= qlam 0.0) (setq qlam (*f rnpa 1.0e-17))))
	 (setq rr (//$ (-$ (*f rz rnpa) (*f rr rp)) qlam)
	       cr (//$ (+$ (*f y rnpa) (*f rp cr)) qlam))
	 (cond ((> l ln) (setq rl (*$ rnp rl (//$ (+$ ta rn)))))
	       (t (cond ((> ln l)
			 (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) l) rr)
			 (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) l) cr)))
		  (setq rl (arraycall 'flonum (nsymbol-array 'rj-bessel-array) (f1- l)))))
	 (setq qlam (*f rnpa rl) rlam 0.0 clam 0.0 ind (fixnum-remainder l 4.))
	 (cond ((= ind 0.) (setq rlam qlam))
	       ((= ind 1.) (setq clam (setq qlam (-$ qlam))))
	       ((= ind 2.) (setq rlam (setq qlam (-$ qlam))))
	       (t (setq clam qlam)))
	 (setq s (-$ (*$ (+$ rs rlam) rr) (*$ (+$ cs clam) cr)))
	 (setq cs (+$ (*$ (+$ rs rlam) cr) (*$ rr (+$ cs clam))))
	 (setq rs s rnp rn))
	(setq 
	 qlam
	 (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ a -13.812104)
							     a)
							 50.569126)
						     a)
						 122.48542)
					     a)
					 -968.33451)
				     a)
				 -203.72512)
			     a)
			 5452.1006)
		     a)
		 4630.389)
	     (//$
	      (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ a
								      0.12949573)
								  1.61000405)
							      a)
							  12.473999)
						      a)
						  78.03884)
					      a)
					  339.71559)
				      a)
				  1228.9483)
			      a)
			  2779.373)
		      a)
		  4630.3895))
	     (exp y)
	     (expt (*$ 0.5 sqrp) a)))
	(cond ((> 1.0e-36 (abs rz)) (setq phi (*$ a 1.57079632)))
	      (t (setq phi (atan y rz) phi (-$ (*$ phi a) (*$ rz)))))
	(setq rsum (*$ (cos phi) qlam) csum (*$ (sin phi) qlam))
	(setq rs (1+$ rs) s (+$ (*f rs rs) (*f cs cs)))
	(setq rr (//$ (+$ (*f rsum rs) (*f csum cs)) s))
	(setq cr (//$ (-$ (*f rs csum) (*f rsum cs)) s))
	(setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) rr)
	(setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.) cr)
	(cond ((> n 0.)
	       (do ((k 1. (f+ k 1.)))
		   ((> k n))
		   (setq rs (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k) cs (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k))
		   (setq s (-$ (*f rs rr) (*f cs cr)))
		   (setq cr (+$ (*f rs cr) (*f rr cs)))
		   (setq rr s)
		   (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k) rr)
		   (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k) cr))))
	(cond ((> 0.0 cz) 
	       (do ((k 0. (f1+ k)))
		   ((> k n))
		   (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k) (-$ (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k))))))
	(return (list '(mlist simp) (arraycall 'flonum (nsymbol-array 'rj-bessel-array) n) (arraycall 'flonum (nsymbol-array 'cj-bessel-array) n)))))

(defun $bessel ($arg $order)
  ((lambda (a)
	   (cond ((not (and (numberp $order)
			    (not (< (setq a (float $order)) 0.0))
			    (numberp ($realpart $arg))
			    (numberp ($imagpart $arg))))
		  (list '($bessel simp) $arg $order))
		 (t (bessel (float ($realpart $arg))
			    (float ($imagpart $arg))
			    a)
		    (narray $besselarray $complete (fix a))
		    ;(apply '$array (list '$besselarray '$complete (fix a)))
		    (do ((k 0. (f1+ k)) (n (fix a)))
			((> k n) (arraycall 'flonum (nsymbol-array '$besselarray) n))
			(setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
			       (simplify (list '(mplus)
					       (simplify (list '(mtimes)
							       '$%i
							       (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k)))
					       (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k))))))))
   0.0)) 

(declare-top(flonum rz y rs cs third sin60 term sum fi cossum sinsum sign (airy flonum)))

;here is Ai'
;airy1(z):=if z = 0. then -1/(gamma(1/3.)*3.^(1/3.))
;else block([zz],z:-z,zz:2./3.*z^(3./2.),bessel(zz,4./3.),
;j:realpart(2/(3.*zz)*besselarray[0]-besselarray[1]),
;-1/3.*z*(j-realpart(bessel(zz,2./3.))));

(defun airy (rz)
       ((lambda (y rs cs third sin60)
		(setq y (sqrt (abs rz)) rz (*$ 2.0 third y rz))
		       (cond ((= rz 0.0) 0.35502805) ;;;avoids branch point probs
			     ((> rz 3.3333333)
			      (do ((fi 1.0 (1+$ fi))
				   (term 1.0)
				   (sum 1.0))
				  ((> fi 7.0)
				   (setq sum (-$ sum (*$ 0.5 term)))
				   (//$ (*$ (exp (-$ rz)) sum)
					(*$ 2.0 (sqrt (*$ y (atan 0 -1))))))
				  (setq term (//$ (*$ term -0.5
						      (-$ fi 0.83333333)
						      (-$ fi 0.166666666))
						  (*$ fi rz))
					sum (+$ sum term))))
			     ((< rz -7.5)
			      (setq rz (-$ rz))
			      (do ((fi 1.0 (1+$ fi))
				   (term 1.0)
				   (cossum 0.0)
				   (sinsum 1.0)
				   (sign -1.0)
				   (even nil (not even)))
				  ((> fi 6.0)
				   (setq rz (+$ rz (atan 1. 1.)))
				   (//$ (+$ (*$ (sin rz) sinsum)
					    (*$ (cos rz) cossum))
					(sqrt (*$ y (atan 0 -1.)))))
				  (setq term (//$ (*$ term 0.5
						      (-$ fi 0.83333333)
						      (-$ fi 0.166666666))
						  (*$ fi rz)))
				  (cond (even (setq sinsum (+$ sinsum (*$ sign term))
						    sign (-$ sign)))
					(t (setq cossum (+$ cossum (*$ sign term)))))))
			     ((< rz 0.0)
			      (setq rz (-$ rz))
			      (bessel rz 0.0 (*$ 5.0 third))
			      (setq rs (-$ (//$ (*$ 4.0 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
						(*$ 3.0 rz)) (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 1.)))
			      (bessel rz 0.0 third)
			      (*$ y third (+$ rs (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))))
			     (t (bessel 0.0 rz (*$ 5.0 third))
				(setq rs (-$ (//$ (*$ 4.0 (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.))
						  (*$ 3.0 rz))
					     (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 1.))
				      cs (-$ (//$ (*$ -4.0 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
						  (*$ 3.0 rz))
					     (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 1.))
				      rs (-$ (*$ sin60 rs) (*$ 0.5 cs)))
				(bessel 0.0 rz third)
				(setq cs (+$ (*$ sin60 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
					     (*$ 0.5 (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.))))
				(*$ y third (-$ rs cs)))))
	0.0 0.0 0.0 (//$ 3.0) (sqrt 0.75)))

(defun $airy ($arg)
       (cond ((numberp $arg) (airy (float $arg)))
	     (t (list '($airy simp) $arg))))

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

(defun expint (x)
       (cond ((< x 1.0)
	      (-$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ 1.07857e-3 x)
	       -9.76004e-3) x) 0.05519968) x) -0.24991055) x) 0.99999193)
	       x) 0.57721565 (log x)))
	     (t ((lambda (w y)
			 (setq y (+$ (*$ (+$ (*$ (+$ (*$ (+$ x 8.57332873)
				  x) 18.059017) x) 8.63476085) x) 0.26777373)
			       w (+$ (*$ (+$ (*$ (+$ (*$ (+$ x 9.5733224)
				  x) 25.6329562) x) 21.099653) x) 3.95849696))
			 (*$ (//$ (exp (-$ x)) x) (//$ y w)))
		 0.0  0.0))))

(defun $expint ($x)
       (cond ((numberp $x) (expint (float $x)))
	     (t (list '($expint simp) $x))))
