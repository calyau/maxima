;;; -*- Mode: LISP; Package: Maxima; Syntax: Common-Lisp; 
;;; a package for chebyshev approximations. Comments at end
(in-package :maxima)


;; tocheb computes all the a_j, j=0 to n-1 for the Maxima function f.

(defun $tocheb (f n)
  (cons '($chebseries) (ajs f n)))

;; compute a[j]s  for Maxima this way

(defun ajs(f n)(loop for j from 0 to (1- n) 
		      collect (aj f j n)))


;; it turns out we will be re-using certain key values repeatedly,
;; so we save them in a hash table.

(defvar *cosc* (make-hash-table)) ;; store useful values of cosine here.

(defun cachecos(r)(or (gethash r *cosc*)(setf (gethash r *cosc*) (cos r))))

(defun aj (f j n)  ;; compute just one coefficient, a_j out of n for function f
  (let ((in (/ 1.0d0 n)))		; inverse of n
    (* in 2 (- 2 (if (= j n) 0 1))
    (loop for k from 0 to (1- n) sum
	  (let ((h(* pi (+ k 0.5d0) in)))
	    (* (cachecos (* h j))
	       (mapply1 f (list (cachecos h)) f f)))))))

(defun $cheb_call(a x) ;; clenshaw algorithm
  ;; evaluate p=sum'(a_nT_n(x), i=0 to N)  a is chebseries
  ;; sum' multiplies a_0 by 1/2
  ;; works.
  (declare (optimize (speed 3)(safety 0))
	   (double-float x))
  (cond ((eq (caar a) '$chebseries)
	 (let* ((bj2 0.0d0)
		(bj1 0.0d0)
		(bj0 0.0d0))
	   (declare(double-float bj2 bj1 bj0))
	   (setf a (reverse(cdr a)))  ;; could hack this away. maybe later
	   (loop while (cdr a) do
		 (setf bj0
		   (+ (* 2.0d0 x bj1)
		      (- bj2)
		      (the double-float (pop a))))
		 (setf bj2 bj1 bj1 bj0))
	   (+ (* x bj1) (* 0.5d0(pop a))(- bj2))))

	(t  (error "expecting chebseries,not ~s" a))))

(defun $chebadd(a b)
  ;; a slower version, chebplus,  is below
  (let* ((ans nil))
    (cond ((> (length a)(length b))
	   (setf a (append (cdr a) nil)) ;copy
	   (setf ans a)
	   (setf b (cdr b))
	   (while b (incf (car a)(pop b)) (pop a)))
	  (t
	   (setf b (append (cdr b) nil)) ;copy
	   (setf ans b)
	   (setf a (cdr a))
	   (while a (incf (car b)(pop a)) (pop b))))
    (cons '($chebseries) ans)))


(defun $chebmul(a b)
  ;; assumes each is a chebseries. no error checking here. 
  ;; a slower version is below.
  
  (let* ((aa (coerce (rest a) 'simple-array))
	 (bb (coerce (rest b) 'simple-array))
	 (la (length aa))
	 (lb (length bb))
	 (term 0.0d0)
	 (ans (make-array (+ la lb -1) :initial-element 0.0d0)))
    (setf (aref aa 0)(* 0.5d0 (aref aa 0)))
    (setf (aref bb 0)(* 0.5d0 (aref bb 0)))
    (loop for i from 0 to (1- la) do
	  (loop for j from 0 to (1- lb) do
		(setf term (* (aref aa i)(aref bb j)))
	 	(incf (aref ans (+ i j)) term)
		(incf (aref ans (abs(- i j))) term)
		))
    (loop for i from 1 to (+ la lb -2) do;; not ans[0]
	  (setf (aref ans i) (* (aref ans i)0.5d0))
	  )
    (cons '($chebseries)(coerce ans 'list))))
			   
		     
;; how about composition?
(defun $chebcomp(f g n)
  ;; f, g are chebseries
  ;; simple hack is to evaluate f(g(x)) as needed, e.g.
  ($tocheb #'(lambda(r)($cheb_call f ($cheb_call g r)))
	   ;; how many points are justified though??
	   n))

(defun $chebplus(f g)
  ;; f, g are chebseries
  ;; simple hack is to evaluate f and g as needed, e.g.
  ($tocheb #'(lambda(r)(+($cheb_call f r) ($cheb_call g r)))
	   ;; how many points are justified though??
	   (max (length f)(length g))))
;; this works
(defun $chebtimes(f g)
  ;; f, g are chebseries
  ;; simple hack is to evaluate f and g as needed, e.g.
  ($tocheb #'(lambda(r)(*($cheb_call f r) ($cheb_call g r)))
	   ;; how many points are justified though??
	   (+ (length f)(length g) -1)))

(defun $chebinv(f n)
  ;; f  is a chebseries. Compute 1/f
  ;; simple hack is to evaluate f and g as needed, e.g.
  ($tocheb #'(lambda(r)(/ 1.0d0 ($cheb_call f r)))
	   ;; how many points are justified though??
	   n))

(defun $chebapply(fun cs n) ;; apply any maxima function to a chebseries
  ;; e.g. chebapply(lambda([r](exp(r^2+1)),chebs);
  ;; constraint: fun must be a function of one float argument that
  ;; returns a double-float.
  ($tocheb #'(lambda(r)
	       (mapply1 fun (list r) fun fun)) cs
	   ;; how many points are justified though??
	       n))

(defun $chebint(a) ;; indefinite integration of the function given by a.
  ;;integrate(t[n](x),x) = 
  ;; if n=0 then t[1].
  ;; if n=1 then  1/2*t[0] +1/4*t[2]  else
  ;; 1/2*(t[n+1]/(n+1) - t[n-1]/(n-1))
  ;;
  ;;
   (let* ((aa (coerce (rest a) 'simple-array))
	  (la (length aa))
	  (term 0.0d0)
	  (ans (make-array  (max 2(1+ la))  :initial-element 0.0d0)))
     (setf (aref aa 0) (* 0.5d0 (aref aa 0)))
     (setf (aref ans 1) (aref aa 0))	;int(const)= const*x
     (setf (aref ans 0) (* 0.25d0 (aref aa 1))) ;int(x) = x^2/2 =1/4*T[0]+
     (setf (aref ans 2) (* 0.25d0 (aref aa 1))) ;...+ 1/4*T[2]
    (loop for i from 2 to  (1- la) do
	   (setf term (* 0.5 (aref aa i)))
	   (incf (aref ans (1+ i)) (/ term (1+ i)))
	   (decf (aref ans (1- i)) (/ term (1- i))))
    (setf (aref ans 0)(* 0.5d0 (aref ans 0))) ;so sum' works
    
    
    (cons '($chebseries)(coerce ans 'list))))
  



;; elaboration: fix up the above programs to combine non-chebseries with
;; chebseries.  Compute the appropriate number of terms.


#| This file defines some Maxima functions for playing with
representation of functions of a single "real" variable
(double-precision floats, actually) by their Chebyshev series
coefficients.  This particular version is set up for generation and
manipulation of functions defined on [-1,1].

We refer elsewhere (http://www2.maths.ox.ac.uk/chebfun) for a discussion
of the kinds of computations that can be considered, and the nature of functions
that can be appropriately modeled by a Chebyshev expansion. 

The chebfun web site discusses a Matlab project which includes
facilities for functions being translated or scaled, or pasted together
piecewise.

A key piece (not included here), is a method for truncation of these chebyshev series,
based on the relative size of the trailing coefficients. Drop em if them are too small
to matter.  Also, these series can be directly multiplied, added, composed. There
are neat ways of computing them for polynomials and rational functions, though not
as briefly as here.  And there is a faster way of computing the coefficients from
the evaluation points using an FFT (or a Discrete Cosine Transform), which we do not
include because it would require other programs, and more debugging.  These lisp
programs should be compiled for speed.

It has been said that "the Chebyshev polynomials are everywhere dense in numerical analysis." 

examples
load("thisfile")
s: tocheb(lambda([x],sin(x)),20)	;
cheb_call(s,0.1234)			;
sin(0.1234)				;
gg(x):=sin(20*x^2+exp(x^5+sqrt(x^4+1)))	;
g:tocheb(gg,60);
gg(0.4)					;
cheb_call(g,0.4)			;

plot2d('[gg(x),cheb_call(g,x)],[x,0,1.025]) ;  /* note they are the same until x>1 */

oh if you don't like
s: tocheb(lambda([x],sin(x)),20)	;

then we can do this:
afun(ex,x):=buildq([expr:ex,var:x],lambda([var],expr)) $

cheb3(expr,var,count):= tocheb(afun(expr,var),count)$
cheb3(sin(z),z,20)   then is the same as tocheb(lambda([z],sin(z)),20)  which happens also to be
the same as tocheb(sin,20).

Here are a few additional ideas.
Elaborate on chebseries so that they are callable. e.g. 
(($chebseries) a b c) becomes #'(lambda(r)(cheb_call '((chebseries) a b c) r))
in effect.  applying a chebseries to an argument becomes simpler.
include various limits (now default -1,1)
package up a piecewise function as the chebfun project does.

extract a coefficient from the series so that all coefficients are between -1 and 1.
e.g.  [1,2,3]  becomes   3* [1/3,2/3,1].
The max coefficient would then always be 1.
In fact, the coefficients could all be fixed point numbers between -.111111... 
and +.11111.  Low precision would look like .0000001.. .  Given that float
arithmetic is ubiquitous and fast, this may not be a winner.

other advantage:  a method for truncating the product of chebyshev series.

a=  A* csa,  where csa has max coefficient of +-1.  [apparently not new,]
b=  B* csb, similarly.  
the product is then A*B* (csa*csb).
without loss of generality, consider only csa*csb.

We agree that if we know that all coefficients in the answer beyond
some N have magnitude less than epsilon (say, 1e-13) then we will drop
all those terms beyond N. 

We will not necessarily know this to be the case
when we generate terms ab initio from some expression we are
evaluating at Gauss-Lobatto points, but if we assert the two cs to be
multiplied are in fact correct in all given terms, and furthermore,
all terms beyond those explicitly represented are exactly zero, then
we are doing about as well as can be expected with the information at
our disposal.

Typically the input cs will have rapidly declining coefficients,
especially just before they are cut off. 

For purposes of being explicit, let us say that the coefficients start
with magnitude 1 and decrease by a factor of 10 for each coefficient.
that would suggest coefficients of magnitude
1,0.1, 0.01, ... 1e-12, 1e-13, 0.... in each input, and csa and csb are
both of length 13.

We know that any terms in the answer will be of magnitude 1e-13 or greater,
so there is not much point in accumulating answers that are 1e-7 times
terms in the other series that are 1e-7 or less.  At worst, there will be
13 terms all of the same sign added together in a 13x13 multiply..

Consider 

csa =  csa_big +  sqrt(eps/n)*csa_small =A1+sqrt(eps/n)A2  n is length of csa
csb =  csb_big +  sqrt(eps/m)*csb_small =B1+sqrt(eps/m)B2  m is length of csb

The product contains  A1*B1+sqrt(eps/m)*A1*B2 +sqrt(eps/n)*A2*B1+ eps/sqrt(nm)*A2*B2.
This last component promises to be mostly negligible. Why multiply these?
So instead of computing 13^2 =169 terms to produce 26 coefficients, we compute about
3/4 of them, or about 127 terms. We will also produce about 13 coefficients.


maybe this cuts off too much?  what if we use eps^2?   a term of order eps^2 would not affect even the low order bits of a term of order eps, which is the smallest retained term, anyway.

how would terms of order eps^2 be generated -- hardly at all. ugh.


more thoughts, if we are going to make this into a generic system
for dealing with functions. 
integral (just done)
derivative
zeros
(eh, maybe) positive
(eh, heuristic)  test for equality
majorization?  f>g,  f<g, sometimes, always?
economization, remez?

rational or other powers. sqrt, power of 10.. maybe jcpmiller method?




another way of converting a polynomial in x of degree N, sum(a[i]*x^i,i,0,N)  to chebyshev series,
based on CACM paper by Thacher, 1964. 

(kill(c,phi), /*  buggy */
 c[q,N]:=sum(a[r]*phi[q,(r-q)/2,q],r,1,N),
 phi[q,j]:=if j=0 then 2^(1-q) else (q+2*j)*(q+2*j-1)/(4*j*(q+j)*phi[q,j-1]))


|# 

