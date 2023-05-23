;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10-*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 2001 by Raymond Toy.  Replaced everything and added ;;;;;
;;;     support for symbolic manipulation of all 12 Jacobian elliptic  ;;;;;
;;;     functions and the complete and incomplete elliptic integral    ;;;;;
;;;     of the first, second and third kinds.                          ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;;(macsyma-module ellipt)

;;;
;;; Jacobian elliptic functions and elliptic integrals.
;;;
;;; References:
;;;
;;; [1] Abramowitz and Stegun
;;; [2] Lawden, Elliptic Functions and Applications, Springer-Verlag, 1989
;;; [3] Whittaker & Watson, A Course of Modern Analysis
;;;
;;; We use the definitions from Abramowitz and Stegun where our
;;; sn(u,m) is sn(u|m).  That is, the second arg is the parameter,
;;; instead of the modulus k or modular angle alpha.
;;;
;;; Note that m = k^2 and k = sin(alpha).
;;;

;;
;; Routines for computing the basic elliptic functions sn, cn, and dn.
;;
;;
;; A&S gives several methods for computing elliptic functions
;; including the AGM method (16.4) and ascending and descending Landen
;; transformations (16.12 and 16.14).  The latter are actually quite
;; fast, only requiring simple arithmetic and square roots for the
;; transformation until the last step.  The AGM requires evaluation of
;; several trigonometric functions at each stage.
;;
;; However, the Landen transformations appear to have some round-off
;; issues.  For example, using the ascending transform to compute cn,
;; cn(100,.7) > 1e10.  This is clearly not right since |cn| <= 1.
;;

(in-package #:bigfloat)

(declaim (inline descending-transform ascending-transform))

(defun ascending-transform (u m)
  ;; A&S 16.14.1
  ;;
  ;; Take care in computing this transform.  For the case where
  ;; m is complex, we should compute sqrt(mu1) first as
  ;; (1-sqrt(m))/(1+sqrt(m)), and then square this to get mu1.
  ;; If not, we may choose the wrong branch when computing
  ;; sqrt(mu1).
  (let* ((root-m (sqrt m))
	 (mu (/ (* 4 root-m)
		(expt (1+ root-m) 2)))
	 (root-mu1 (/ (- 1 root-m) (+ 1 root-m)))
	 (v (/ u (1+ root-mu1))))
    (values v mu root-mu1)))

(defun descending-transform (u m)
  ;; Note: Don't calculate mu first, as given in 16.12.1.  We
  ;; should calculate sqrt(mu) = (1-sqrt(m1)/(1+sqrt(m1)), and
  ;; then compute mu = sqrt(mu)^2.  If we calculate mu first,
  ;; sqrt(mu) loses information when m or m1 is complex.
  (let* ((root-m1 (sqrt (- 1 m)))
	 (root-mu (/ (- 1 root-m1) (+ 1 root-m1)))
	 (mu (* root-mu root-mu))
	 (v (/ u (1+ root-mu))))
    (values v mu root-mu)))

;;
;; This appears to work quite well for both real and complex values
;; of u.
(defun elliptic-sn-descending (u m)
  (cond ((= m 1)
	 ;; A&S 16.6.1
	 (tanh u))
	((< (abs m) (epsilon u))
	 ;; A&S 16.6.1
	 (sin u))
	(t
	 (multiple-value-bind (v mu root-mu)
	     (descending-transform u m)
	   (let* ((new-sn (elliptic-sn-descending v mu)))
	     (/ (* (1+ root-mu) new-sn)
		(1+ (* root-mu new-sn new-sn))))))))

;; AGM scale.  See A&S 17.6
;;
;; The AGM scale is
;;
;; a[n] = (a[n-1]+b[n-1])/2, b[n] = sqrt(a[n-1]*b[n-1]), c[n] = (a[n-1]-b[n-1])/2.
;;
;; We stop when abs(c[n]) <= 10*eps
;;
;; A list of (n a[n] b[n] c[n]) is returned.
(defun agm-scale (a b c)
  (loop for n from 0
     while (> (abs c) (* 10 (epsilon c)))
     collect (list n a b c)
     do (psetf a (/ (+ a b) 2)
	       b (sqrt (* a b))
	       c (/ (- a b) 2))))

;; WARNING: This seems to have accuracy problems when u is complex.  I
;; (rtoy) do not know why.  For example (jacobi-agm #c(1e0 1e0) .7e0)
;; returns
;;
;; #C(1.134045970915582 0.3522523454566013)
;; #C(0.57149659007575 -0.6989899153338323)
;; #C(0.6229715431044184 -0.4488635962149656)
;;
;; But the actual value of sn(1+%i, .7) is .3522523469224946 %i +
;; 1.134045971912365.  We've lost about 7 digits of accuracy!
(defun jacobi-agm (u m)
  ;; A&S 16.4.
  ;;
  ;; Compute the AGM scale with a = 1, b = sqrt(1-m), c = sqrt(m).
  ;;
  ;; Then phi[N] = 2^N*a[N]*u and compute phi[n] from
  ;;
  ;; sin(2*phi[n-1] - phi[n]) = c[n]/a[n]*sin(phi[n])
  ;;
  ;; Finally,
  ;;
  ;; sn(u|m) = sin(phi[0]), cn(u|m) = cos(phi[0])
  ;; dn(u|m) = cos(phi[0])/cos(phi[1]-phi[0])
  ;;
  ;; Returns the three values sn, cn, dn.
  (let* ((agm-data (nreverse (rest (agm-scale 1 (sqrt (- 1 m)) (sqrt m)))))
	 (phi (destructuring-bind (n a b c)
		  (first agm-data)
		(declare (ignore b c))
		(* a u (ash 1 n))))
	 (phi1 0e0))
    (dolist (agm agm-data)
      (destructuring-bind (n a b c)
	  agm
	(declare (ignore n b))
	(setf phi1 phi
	      phi (/ (+ phi (asin (* (/ c a) (sin phi)))) 2))))
    (values (sin phi) (cos phi) (/ (cos phi) (cos (- phi1 phi))))))

(defun sn (u m)
  (cond ((zerop m)
	 ;; jacobi_sn(u,0) = sin(u).  Should we use A&S 16.13.1 if m
	 ;; is small enough?
	 ;;
	 ;; sn(u,m) = sin(u) - 1/4*m(u-sin(u)*cos(u))*cos(u)
	 (sin u))
	((= m 1)
	 ;; jacobi_sn(u,1) = tanh(u).  Should we use A&S 16.15.1 if m
	 ;; is close enough to 1?
	 ;;
	 ;; sn(u,m) = tanh(u) + 1/4*(1-m)*(sinh(u)*cosh(u)-u)*sech(u)^2
	 (tanh u))
	(t
	 ;; Use the ascending Landen transformation to compute sn.
	 (let ((s (elliptic-sn-descending u m)))
	   (if (and (realp u) (realp m))
	       (realpart s)
	       s)))))

(defun dn (u m)
  (cond ((zerop m)
	 ;; jacobi_dn(u,0) = 1.  Should we use A&S 16.13.3 for small m?
	 ;;
	 ;; dn(u,m) = 1 - 1/2*m*sin(u)^2
	 1)
	((= m 1)
	 ;; jacobi_dn(u,1) = sech(u).  Should we use A&S 16.15.3 if m
	 ;; is close enough to 1?
	 ;;
	 ;; dn(u,m) = sech(u) + 1/4*(1-m)*(sinh(u)*cosh(u)+u)*tanh(u)*sech(u)
	 (/ (cosh u)))
	(t
	 ;; Use the Gauss transformation from
	 ;; http://functions.wolfram.com/09.29.16.0013.01:
	 ;;
	 ;;
	 ;; dn((1+sqrt(m))*z, 4*sqrt(m)/(1+sqrt(m))^2)
	 ;;   =  (1-sqrt(m)*sn(z, m)^2)/(1+sqrt(m)*sn(z,m)^2)
	 ;;
	 ;; So
	 ;;
	 ;; dn(y, mu) = (1-sqrt(m)*sn(z, m)^2)/(1+sqrt(m)*sn(z,m)^2)
	 ;;
	 ;; where z = y/(1+sqrt(m)) and mu=4*sqrt(m)/(1+sqrt(m))^2.
	 ;;
	 ;; Solve for m, and we get
	 ;;
	 ;; sqrt(m) = -(mu+2*sqrt(1-mu)-2)/mu or (-mu+2*sqrt(1-mu)+2)/mu.
	 ;;
	 ;; I don't think it matters which sqrt we use, so I (rtoy)
	 ;; arbitrarily choose the first one above.
	 ;;
	 ;; Note that (1-sqrt(1-mu))/(1+sqrt(1-mu)) is the same as
	 ;; -(mu+2*sqrt(1-mu)-2)/mu.  Also, the former is more
	 ;; accurate for small mu.
	 (let* ((root (let ((root-1-m (sqrt (- 1 m))))
			(/ (- 1 root-1-m)
			   (+ 1 root-1-m))))
		(z (/ u (+ 1 root)))
		(s (elliptic-sn-descending z (* root root)))
		(p (* root s s )))
	   (/ (- 1 p)
	      (+ 1 p))))))

(defun cn (u m)
  (cond ((zerop m)
	 ;; jacobi_cn(u,0) = cos(u).  Should we use A&S 16.13.2 for
	 ;; small m?
	 ;;
	 ;; cn(u,m) = cos(u) + 1/4*m*(u-sin(u)*cos(u))*sin(u)
	 (cos u))
	((= m 1)
	 ;; jacobi_cn(u,1) = sech(u).  Should we use A&S 16.15.3 if m
	 ;; is close enough to 1?
	 ;;
	 ;; cn(u,m) = sech(u) - 1/4*(1-m)*(sinh(u)*cosh(u)-u)*tanh(u)*sech(u)
	 (/ (cosh u)))
	(t
	 ;; Use the ascending Landen transformation, A&S 16.14.3.
	 (multiple-value-bind (v mu root-mu1)
	     (ascending-transform u m)
	   (let ((d (dn v mu)))
	     (* (/ (+ 1 root-mu1) mu)
		(/ (- (* d d) root-mu1)
		   d)))))))

(in-package :maxima)

;; Tell maxima what the derivatives are.
;;
;; Lawden says the derivative wrt to k but that's not what we want.
;;
;; Here's the derivation we used, based on how Lawden get's his results.
;;
;; Let
;;
;; diff(sn(u,m),m) = s
;; diff(cn(u,m),m) = p
;; diff(dn(u,m),m) = q
;;
;; From the derivatives of sn, cn, dn wrt to u, we have
;;
;; diff(sn(u,m),u) = cn(u)*dn(u)
;; diff(cn(u,m),u) = -cn(u)*dn(u)
;; diff(dn(u,m),u) = -m*sn(u)*cn(u)
;;

;; Differentiate these wrt to m:
;;
;; diff(s,u) = p*dn + cn*q
;; diff(p,u) = -p*dn - q*dn
;; diff(q,u) = -sn*cn - m*s*cn - m*sn*q
;;
;; Also recall that
;;
;; sn(u)^2 + cn(u)^2 = 1
;; dn(u)^2 + m*sn(u)^2 = 1
;;
;; Differentiate these wrt to m:
;;
;; sn*s + cn*p = 0
;; 2*dn*q + sn^2 + 2*m*sn*s = 0
;;
;; Thus,
;;
;; p = -s*sn/cn
;; q = -m*s*sn/dn - sn^2/dn/2
;;
;; So
;; diff(s,u) = -s*sn*dn/cn - m*s*sn*cn/dn - sn^2*cn/dn/2
;;
;; or
;;
;; diff(s,u) + s*(sn*dn/cn + m*sn*cn/dn) = -1/2*sn^2*cn/dn
;;
;; diff(s,u) + s*sn/cn/dn*(dn^2 + m*cn^2) = -1/2*sn^2*cn/dn
;;
;; Multiply through by the integrating factor 1/cn/dn:
;;
;; diff(s/cn/dn, u) = -1/2*sn^2/dn^2 = -1/2*sd^2.
;;
;; Integrate this to get
;;
;; s/cn/dn = C + -1/2*int sd^2
;;
;; It can be shown that C is zero.
;;
;; We know that (by differentiating this expression)
;;
;; int dn^2 = (1-m)*u+m*sn*cd + m*(1-m)*int sd^2
;;
;; or
;;
;; int sd^2 = 1/m/(1-m)*int dn^2 - u/m - sn*cd/(1-m)
;;
;; Thus, we get
;;
;; s/cn/dn = u/(2*m) + sn*cd/(2*(1-m)) - 1/2/m/(1-m)*int dn^2
;;
;; or
;;
;; s = 1/(2*m)*u*cn*dn + 1/(2*(1-m))*sn*cn^2 - 1/2/(m*(1-m))*cn*dn*E(u)
;;
;; where E(u) = int dn^2 = elliptic_e(am(u)) = elliptic_e(asin(sn(u)))
;;
;; This is our desired result:
;;
;; s = 1/(2*m)*cn*dn*[u - elliptic_e(asin(sn(u)),m)/(1-m)] + sn*cn^2/2/(1-m)
;;
;;
;; Since diff(cn(u,m),m) = p = -s*sn/cn, we have
;;
;; p = -1/(2*m)*sn*dn[u - elliptic_e(asin(sn(u)),m)/(1-m)] - sn^2*cn/2/(1-m)
;;
;; diff(dn(u,m),m) = q = -m*s*sn/dn - sn^2/dn/2
;;
;; q = -1/2*sn*cn*[u-elliptic_e(asin(sn),m)/(1-m)] - m*sn^2*cn^2/dn/2/(1-m)
;;
;;      - sn^2/dn/2
;;
;;   = -1/2*sn*cn*[u-elliptic_e(asin(sn),m)/(1-m)] + dn*sn^2/2/(m-1)
;;
(defprop %jacobi_sn
    ((u m)
     ((mtimes) ((%jacobi_cn) u m) ((%jacobi_dn) u m))
     ((mplus simp)
      ((mtimes simp) ((rat simp) 1 2)
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
       ((mexpt simp) ((%jacobi_cn simp) u m) 2) ((%jacobi_sn simp) u m))
      ((mtimes simp) ((rat simp) 1 2) ((mexpt simp) m -1)
       ((%jacobi_cn simp) u m) ((%jacobi_dn simp) u m)
       ((mplus simp) u
	((mtimes simp) -1 ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
	 ((%elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
  grad)

(defprop %jacobi_cn
    ((u m)
     ((mtimes simp) -1 ((%jacobi_sn simp) u m) ((%jacobi_dn simp) u m))
     ((mplus simp)
      ((mtimes simp) ((rat simp) -1 2)
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
       ((%jacobi_cn simp) u m) ((mexpt simp) ((%jacobi_sn simp) u m) 2))
      ((mtimes simp) ((rat simp) -1 2) ((mexpt simp) m -1)
       ((%jacobi_dn simp) u m) ((%jacobi_sn simp) u m)
       ((mplus simp) u
	((mtimes simp) -1 ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
	 ((%elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
  grad)

(defprop %jacobi_dn
    ((u m)
     ((mtimes) -1 m ((%jacobi_sn) u m) ((%jacobi_cn) u m))
     ((mplus simp)
      ((mtimes simp) ((rat simp) -1 2)
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
       ((%jacobi_dn simp) u m) ((mexpt simp) ((%jacobi_sn simp) u m) 2))
      ((mtimes simp) ((rat simp) -1 2) ((%jacobi_cn simp) u m)
       ((%jacobi_sn simp) u m)
       ((mplus simp) u
	((mtimes simp) -1
	 ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
	 ((%elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
  grad)

;; The inverse elliptic functions.
;;
;; F(phi|m) = asn(sin(phi),m)
;; 
;; so asn(u,m) = F(asin(u)|m)
(defprop %inverse_jacobi_sn
    ((x m)
     ;; Lawden 3.1.2:
     ;; inverse_jacobi_sn(x) = integrate(1/sqrt(1-t^2)/sqrt(1-m*t^2),t,0,x)
     ;; -> 1/sqrt(1-x^2)/sqrt(1-m*x^2)
     ((mtimes simp)
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
       ((rat simp) -1 2))
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) x 2)))
       ((rat simp) -1 2)))
     ;; diff(F(asin(u)|m),m)
     ((mtimes simp) ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
      ((mplus simp)
       ((mtimes simp) -1 x
	((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
	 ((rat simp) 1 2))
	((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) x 2)))
	 ((rat simp) -1 2)))
       ((mtimes simp) ((mexpt simp) m -1)
	((mplus simp) ((%elliptic_e simp) ((%asin simp) x) m)
	 ((mtimes simp) -1 ((mplus simp) 1 ((mtimes simp) -1 m))
	  ((%elliptic_f simp) ((%asin simp) x) m)))))))
  grad)

;; Let u = inverse_jacobi_cn(x).  Then jacobi_cn(u) = x or
;; sqrt(1-jacobi_sn(u)^2) = x.  Or
;;
;; jacobi_sn(u) = sqrt(1-x^2)
;;
;; So u = inverse_jacobi_sn(sqrt(1-x^2),m) = inverse_jacob_cn(x,m)
;;
(defprop %inverse_jacobi_cn
    ((x m)
     ;; Whittaker and Watson, 22.121
     ;; inverse_jacobi_cn(u,m) = integrate(1/sqrt(1-t^2)/sqrt(1-m+m*t^2), t, u, 1)
     ;; -> -1/sqrt(1-x^2)/sqrt(1-m+m*x^2)
     ((mtimes simp) -1
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
       ((rat simp) -1 2))
      ((mexpt simp)
       ((mplus simp) 1 ((mtimes simp) -1 m)
	             ((mtimes simp) m ((mexpt simp) x 2)))
       ((rat simp) -1 2)))
     ((mtimes simp) ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
      ((mplus simp)
       ((mtimes simp) -1
	((mexpt simp)
	 ((mplus simp) 1
	  ((mtimes simp) -1 m ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))))
	 ((rat simp) -1 2))
	((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2))) ((rat simp) 1 2))
	((mabs simp) x))
       ((mtimes simp) ((mexpt simp) m -1)
	((mplus simp)
	 ((%elliptic_e simp)
	  ((%asin simp)
	   ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2))) ((rat simp) 1 2)))
	  m)
	 ((mtimes simp) -1 ((mplus simp) 1 ((mtimes simp) -1 m))
	  ((%elliptic_f simp)
	   ((%asin simp)
	    ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2))) ((rat simp) 1 2)))
	   m)))))))
  grad)

;; Let u = inverse_jacobi_dn(x).  Then
;;
;; jacobi_dn(u) = x or
;;
;; x^2 = jacobi_dn(u)^2 = 1 - m*jacobi_sn(u)^2
;;
;; so jacobi_sn(u) = sqrt(1-x^2)/sqrt(m)
;;
;; or u = inverse_jacobi_sn(sqrt(1-x^2)/sqrt(m))
(defprop %inverse_jacobi_dn
    ((x m)
     ;; Whittaker and Watson, 22.121
     ;; inverse_jacobi_dn(u,m) = integrate(1/sqrt(1-t^2)/sqrt(t^2-(1-m)), t, u, 1)
     ;; -> -1/sqrt(1-x^2)/sqrt(x^2+m-1)
     ((mtimes simp)
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
       ((rat simp) -1 2))
      ((mexpt simp) ((mplus simp) -1 m ((mexpt simp) x 2)) ((rat simp) -1 2)))
     ((mplus simp)
      ((mtimes simp) ((rat simp) -1 2) ((mexpt simp) m ((rat simp) -3 2))
       ((mexpt simp)
	((mplus simp) 1
	 ((mtimes simp) -1 ((mexpt simp) m -1)
	  ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))))
	((rat simp) -1 2))
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
	((rat simp) 1 2))
       ((mexpt simp) ((mabs simp) x) -1))
      ((mtimes simp) ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
       ((mplus simp)
	((mtimes simp) -1 ((mexpt simp) m ((rat simp) -1 2))
	 ((mexpt simp)
	  ((mplus simp) 1
	   ((mtimes simp) -1 ((mexpt simp) m -1)
	    ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))))
	  ((rat simp) 1 2))
	 ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
	  ((rat simp) 1 2))
	 ((mexpt simp) ((mabs simp) x) -1))
	((mtimes simp) ((mexpt simp) m -1)
	 ((mplus simp)
	  ((%elliptic_e simp)
	   ((%asin simp)
	    ((mtimes simp) ((mexpt simp) m ((rat simp) -1 2))
	     ((mexpt simp) ((mplus simp) 1
			    ((mtimes simp) -1 ((mexpt simp) x 2)))
	      ((rat simp) 1 2))))
	   m)
	  ((mtimes simp) -1 ((mplus simp) 1 ((mtimes simp) -1 m))
	   ((%elliptic_f simp)
	    ((%asin simp)
	     ((mtimes simp) ((mexpt simp) m ((rat simp) -1 2))
	      ((mexpt simp) ((mplus simp) 1
			     ((mtimes simp) -1 ((mexpt simp) x 2)))
	       ((rat simp) 1 2))))
	    m))))))))
  grad)


;; Possible forms of a complex number:
;;
;; 2.3
;; $%i
;; ((mplus simp) 2.3 ((mtimes simp) 2.3 $%i))
;; ((mplus simp) 2.3 $%i))
;; ((mtimes simp) 2.3 $%i)
;;


;; Is argument u a complex number with real and imagpart satisfying predicate ntypep?
(defun complex-number-p (u &optional (ntypep 'numberp))
  (let ((R 0) (I 0))
    (labels ((a1 (x) (cadr x))
             (a2 (x) (caddr x))
             (a3+ (x) (cdddr x))
             (N (x) (funcall ntypep x)) ; N
             (i (x) (and (eq x '$%i) (N 1))) ; %i
             (N+i (x) (and (null (a3+ x)) ; mplus test is precondition
                           (N (setq R (a1 x)))
                           (or (and (i (a2 x)) (setq I 1) t)
                               (and (mtimesp (a2 x)) (N*i (a2 x))))))
             (N*i (x) (and (null (a3+ x))               ; mtimes test is precondition
                           (N (setq I (a1 x)))
                           (eq (a2 x) '$%i))))
      (declare (inline a1 a2 a3+ N i N+i N*i))
      (cond ((N u) (values t u 0)) ;2.3
            ((atom u) (if (i u) (values t 0 1))) ;%i
            ((mplusp u) (if (N+i u) (values t R I))) ;N+%i, N+N*%i
            ((mtimesp u) (if (N*i u) (values t R I))) ;N*%i
            (t nil)))))

(defun complexify (x)
  ;; Convert a Lisp number to a maxima number
  (cond ((realp x) x)
	((complexp x) (add (realpart x) (mul '$%i (imagpart x))))
	(t (merror (intl:gettext "COMPLEXIFY: argument must be a Lisp real or complex number.~%COMPLEXIFY: found: ~:M") x))))
   
(defun kc-arg (exp m)
  ;; Replace elliptic_kc(m) in the expression with sym.  Check to see
  ;; if the resulting expression is linear in sym and the constant
  ;; term is zero.  If so, return the coefficient of sym, i.e, the
  ;; coefficient of elliptic_kc(m).
  (let* ((sym (gensym))
	 (arg (maxima-substitute sym `((%elliptic_kc) ,m) exp)))
    (if (and (not (equalp arg exp))
	     (linearp arg sym)
	     (zerop1 (coefficient arg sym 0)))
	(coefficient arg sym 1)
	nil)))

(defun kc-arg2 (exp m)
  ;; Replace elliptic_kc(m) in the expression with sym.  Check to see
  ;; if the resulting expression is linear in sym and the constant
  ;; term is zero.  If so, return the coefficient of sym, i.e, the
  ;; coefficient of elliptic_kc(m), and the constant term.  Otherwise,
  ;; return NIL.
  (let* ((sym (gensym))
	 (arg (maxima-substitute sym `((%elliptic_kc) ,m) exp)))
    (if (and (not (equalp arg exp))
	     (linearp arg sym))
	(list (coefficient arg sym 1)
	      (coefficient arg sym 0))
	nil)))

;; Tell maxima how to simplify the functions

(def-simplifier jacobi_sn (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (to (bigfloat::sn (bigfloat:to ($float u)) (bigfloat:to ($float m)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::sn (bigfloat:to ($float u)) (bigfloat:to ($float m))))))
      ((bigfloat-numerical-eval-p u m)
       (to (bigfloat::sn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::sn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m))))))
      ((zerop1 u)
       ;; A&S 16.5.1
       0)
      ((zerop1 m)
       ;; A&S 16.6.1
       (ftake '%sin u))
      ((onep1 m)
       ;; A&S 16.6.1
       (ftake '%tanh u))
      ((and $trigsign (mminusp* u))
       (neg (ftake* '%jacobi_sn (neg u) m)))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (let ((inv-arg (second u)))
	 (ecase (caar u)
	   (%inverse_jacobi_sn
	    ;; jacobi_sn(inverse_jacobi_sn(u,m), m) = u
	    inv-arg)
	   (%inverse_jacobi_ns
	    ;; inverse_jacobi_ns(u,m) = inverse_jacobi_sn(1/u,m)
	    (div 1 inv-arg))
	   (%inverse_jacobi_cn
	    ;; sn(x)^2 + cn(x)^2 = 1 so sn(x) = sqrt(1-cn(x)^2)
	    (power (sub 1 (mul inv-arg inv-arg)) 1//2))
	   (%inverse_jacobi_nc
	    ;; inverse_jacobi_nc(u) = inverse_jacobi_cn(1/u)
	    (ftake '%jacobi_sn (ftake '%inverse_jacobi_cn (div 1 inv-arg) m)
		   m))
	   (%inverse_jacobi_dn
	    ;; dn(x)^2 + m*sn(x)^2 = 1 so
	    ;; sn(x) = 1/sqrt(m)*sqrt(1-dn(x)^2)
	    (mul (div 1 (power m 1//2))
		 (power (sub 1 (mul inv-arg inv-arg)) 1//2)))
	   (%inverse_jacobi_nd
	    ;; inverse_jacobi_nd(u) = inverse_jacobi_dn(1/u)
	    (ftake '%jacobi_sn (ftake '%inverse_jacobi_dn (div 1 inv-arg) m)
		   m))
	   (%inverse_jacobi_sc
	    ;; See below for inverse_jacobi_sc.
	    (div inv-arg (power (add 1 (mul inv-arg inv-arg)) 1//2)))
	   (%inverse_jacobi_cs
	    ;; inverse_jacobi_cs(u) = inverse_jacobi_sc(1/u)
	    (ftake '%jacobi_sn (ftake '%inverse_jacobi_sc (div 1 inv-arg) m)
		   m))
	   (%inverse_jacobi_sd
	    ;; See below for inverse_jacobi_sd
	    (div inv-arg (power (add 1 (mul m (mul inv-arg inv-arg))) 1//2)))
	   (%inverse_jacobi_ds
	    ;; inverse_jacobi_ds(u) = inverse_jacobi_sd(1/u)
	    (ftake '%jacobi_sn (ftake '%inverse_jacobi_sd (div 1 inv-arg) m)
		   m))
	   (%inverse_jacobi_cd
	    ;; See below
	    (div (power (sub 1 (mul inv-arg inv-arg)) 1//2)
		 (power (sub 1 (mul m (mul inv-arg inv-arg))) 1//2)))
	   (%inverse_jacobi_dc
	    (ftake '%jacobi_sn (ftake '%inverse_jacobi_cd (div 1 inv-arg) m) m)))))
      ;; A&S 16.20.1 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       (mul '$%i
	    (ftake* '%jacobi_sc (coeff u '$%i 1) (add 1 (neg m)))))
      ((setq coef (kc-arg2 u m))
       ;; sn(m*K+u)
       ;;
       ;; A&S 16.8.1
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; sn(4*m*K + u) = sn(u), sn(0) = 0
		   (if (zerop1 const)
		       0
		       (ftake '%jacobi_sn const m)))
		  (1
		   ;; sn(4*m*K + K + u) = sn(K+u) = cd(u)
		   ;; sn(K) = 1
		   (if (zerop1 const)
		       1
		       (ftake '%jacobi_cd const m)))
		  (2
		   ;; sn(4*m*K+2*K + u) = sn(2*K+u) = -sn(u)
		   ;; sn(2*K) = 0
		   (if (zerop1 const)
		       0
		       (neg (ftake '%jacobi_sn const m))))
		  (3
		   ;; sn(4*m*K+3*K+u) = sn(2*K + K + u) = -sn(K+u) = -cd(u)
		   ;; sn(3*K) = -1
		   (if (zerop1 const)
		       -1
		       (neg (ftake '%jacobi_cd const m))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; A&S 16.5.2
		;;
		;; sn(1/2*K) = 1/sqrt(1+sqrt(1-m))
		(div 1
		     (power (add 1 (power (sub 1 m) 1//2))
			    1//2)))
	       ((and (alike1 lin 3//2)
		     (zerop1 const))
		;; A&S 16.5.2
		;;
		;; sn(1/2*K + K) = cd(1/2*K,m)
		(ftake '%jacobi_cd (mul 1//2
				       (ftake '%elliptic_kc m))
		       m))
	       (t
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

(def-simplifier jacobi_cn (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (to (bigfloat::cn (bigfloat:to ($float u)) (bigfloat:to ($float m)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::cn (bigfloat:to ($float u)) (bigfloat:to ($float m))))))
      ((bigfloat-numerical-eval-p u m)
       (to (bigfloat::cn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::cn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m))))))
      ((zerop1 u)
       ;; A&S 16.5.1
       1)
      ((zerop1 m)
       ;; A&S 16.6.2
       (ftake '%cos u))
      ((onep1 m)
       ;; A&S 16.6.2
       (ftake '%sech u))
      ((and $trigsign (mminusp* u))
       (ftake* '%jacobi_cn (neg u) m))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_cn)
	      (second u))
	     (t
	      ;; I'm lazy.  Use cn(x) = sqrt(1-sn(x)^2).  Hope
	      ;; this is right.
	      (power (sub 1 (power (ftake '%jacobi_sn u (third u)) 2))
		     1//2))))
      ;; A&S 16.20.2 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       (ftake* '%jacobi_nc (coeff u '$%i 1) (add 1 (neg m))))
      ((setq coef (kc-arg2 u m))
       ;; cn(m*K+u)
       ;;
       ;; A&S 16.8.2
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; cn(4*m*K + u) = cn(u),
		   ;; cn(0) = 1
		   (if (zerop1 const)
		       1
		       (ftake '%jacobi_cn const m)))
		  (1
		   ;; cn(4*m*K + K + u) = cn(K+u) = -sqrt(m1)*sd(u)
		   ;; cn(K) = 0
		   (if (zerop1 const)
		       0
		       (neg (mul (power (sub 1 m) 1//2)
				 (ftake '%jacobi_sd const m)))))
		  (2
		   ;; cn(4*m*K + 2*K + u) = cn(2*K+u) = -cn(u)
		   ;; cn(2*K) = -1
		   (if (zerop1 const)
		       -1
		       (neg (ftake '%jacobi_cn const m))))
		  (3
		   ;; cn(4*m*K + 3*K + u) = cn(2*K + K + u) =
		   ;; -cn(K+u) = sqrt(m1)*sd(u)
		   ;;
		   ;; cn(3*K) = 0
		   (if (zerop1 const)
		       0
		       (mul (power (sub 1 m) 1//2)
			    (ftake '%jacobi_sd const m))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; A&S 16.5.2
		;; cn(1/2*K) = (1-m)^(1/4)/sqrt(1+sqrt(1-m))
		(mul (power (sub 1 m) (div 1 4))
		     (power (add 1
				 (power (sub 1 m)
					1//2))
			    1//2)))
	       (t
		(give-up)))))
      (t
       (give-up)))))

(def-simplifier jacobi_dn (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (to (bigfloat::dn (bigfloat:to ($float u)) (bigfloat:to ($float m)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::dn (bigfloat:to ($float u)) (bigfloat:to ($float m))))))
      ((bigfloat-numerical-eval-p u m)
       (to (bigfloat::dn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::dn (bigfloat:to ($bfloat u)) (bigfloat:to ($bfloat m))))))
      ((zerop1 u)
       ;; A&S 16.5.1
       1)
      ((zerop1 m)
       ;; A&S 16.6.3
       1)
      ((onep1 m)
       ;; A&S 16.6.3
       (ftake '%sech u))
      ((and $trigsign (mminusp* u))
       (ftake* '%jacobi_dn (neg u) m))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_dn)
	      ;; jacobi_dn(inverse_jacobi_dn(u,m), m) = u
	      (second u))
	     (t
	      ;; Express in terms of sn:
	      ;; dn(x) = sqrt(1-m*sn(x)^2)
	      (power (sub 1 (mul m
				 (power (ftake '%jacobi_sn u m) 2)))
		     1//2))))
      ((zerop1 ($ratsimp (sub u (power (sub 1 m) 1//2))))
       ;; A&S 16.5.3
       ;; dn(sqrt(1-m),m) = K(m)
       (ftake '%elliptic_kc m))
      ;; A&S 16.20.2 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       (ftake* '%jacobi_dc (coeff u '$%i 1)
		 (add 1 (neg m))))
      ((setq coef (kc-arg2 u m))
       ;; A&S 16.8.3
       ;;
       ;; dn(m*K+u) has period 2K
       ;;
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 2)
		  (0
		   ;; dn(2*m*K + u) = dn(u)
		   ;; dn(0) = 1
		   (if (zerop1 const)
		       1
		       ;; dn(4*m*K+2*K + u) = dn(2*K+u) = dn(u)
		       (ftake '%jacobi_dn const m)))
		  (1
		   ;; dn(2*m*K + K + u) = dn(K + u) = sqrt(1-m)*nd(u)
		   ;; dn(K) = sqrt(1-m)
		   (if (zerop1 const)
		       (power (sub 1 m) 1//2)
		       (mul (power (sub 1 m) 1//2)
			    (ftake '%jacobi_nd const m))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; A&S 16.5.2
		;; dn(1/2*K) = (1-m)^(1/4)
		(power (sub 1 m)
		       (div 1 4)))
	       (t
		(give-up)))))
      (t (give-up)))))

;; Should we simplify the inverse elliptic functions into the
;; appropriate incomplete elliptic integral?  I think we should leave
;; it, but perhaps allow some way to do that transformation if
;; desired.

(def-simplifier inverse_jacobi_sn (u m)
  (let (args)
    ;; To numerically evaluate inverse_jacobi_sn (asn), use
    ;;
    ;; asn(x,m) = F(asin(x),m)
    ;;
    ;; But F(phi,m) = sin(phi)*rf(cos(phi)^2, 1-m*sin(phi)^2,1).  Thus
    ;;
    ;; asn(x,m) = F(asin(x),m)
    ;;          = x*rf(1-x^2,1-m*x^2,1)
    ;;
    ;; I (rtoy) am not 100% about the first identity above for all
    ;; complex values of x and m, but tests seem to indicate that it
    ;; produces the correct value as verified by verifying
    ;; jacobi_sn(inverse_jacobi_sn(x,m),m) = x.
    (cond ((float-numerical-eval-p u m)
	   (let ((uu (bigfloat:to ($float u)))
		 (mm (bigfloat:to ($float m))))
	     (complexify
	      (* uu
		 (bigfloat::bf-rf (bigfloat:to (- 1 (* uu uu)))
				  (bigfloat:to (- 1 (* mm uu uu)))
				  1)))))
	  ((setf args (complex-float-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to ($float u)))
		   (mm (bigfloat:to ($float m))))
	       (complexify (* uu (bigfloat::bf-rf (- 1 (* uu uu))
						  (- 1 (* mm uu uu))
						  1))))))
	  ((bigfloat-numerical-eval-p u m)
	   (let ((uu (bigfloat:to u))
		 (mm (bigfloat:to m)))
	     (to (bigfloat:* uu
			     (bigfloat::bf-rf (bigfloat:- 1 (bigfloat:* uu uu))
					      (bigfloat:- 1 (bigfloat:* mm uu uu))
					      1)))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to u))
		   (mm (bigfloat:to m)))
	     (to (bigfloat:* uu
			     (bigfloat::bf-rf (bigfloat:- 1 (bigfloat:* uu uu))
					      (bigfloat:- 1 (bigfloat:* mm uu uu))
						1))))))
	  ((zerop1 u)
	   ;; asn(0,m) = 0
	   0)
	  ((onep1 u)
	   ;; asn(1,m) = elliptic_kc(m)
	   (ftake '%elliptic_kc m))
	  ((and (numberp u) (onep1 (- u)))
	   ;; asn(-1,m) = -elliptic_kc(m)
	   (mul -1 (ftake '%elliptic_kc m)))
	  ((zerop1 m)
	   ;; asn(x,0) = F(asin(x),0) = asin(x)
	   (ftake '%asin u))
	  ((onep1 m)
	   ;; asn(x,1) = F(asin(x),1) = log(tan(pi/4+asin(x)/2))
	   (ftake '%elliptic_f (ftake '%asin u) 1))
	  ((and (eq $triginverses '$all)
		(listp u)
		(eq (caar u) '%jacobi_sn)
		(alike1 (third u) m))
	   ;; inverse_jacobi_sn(sn(u)) = u
	   (second u))
	  (t
	   ;; Nothing to do
	   (give-up)))))

(def-simplifier inverse_jacobi_cn (u m)
  (let (args)
    (cond ((float-numerical-eval-p u m)
	   ;; Numerically evaluate acn
	   ;;
	   ;; acn(x,m) = F(acos(x),m)
	   (to (elliptic-f (cl:acos ($float u)) ($float m))))
	  ((setf args (complex-float-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (to (elliptic-f (cl:acos (bigfloat:to ($float u)))
			     (bigfloat:to ($float m))))))
	  ((bigfloat-numerical-eval-p u m)
	   (to (bigfloat::bf-elliptic-f (bigfloat:acos (bigfloat:to u))
					(bigfloat:to m))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (to (bigfloat::bf-elliptic-f (bigfloat:acos (bigfloat:to u))
					  (bigfloat:to m)))))
	  ((zerop1 m)
	   ;; asn(x,0) = F(acos(x),0) = acos(x)
	   (ftake '%elliptic_f (ftake '%acos u) 0))
	  ((onep1 m)
	   ;; asn(x,1) = F(asin(x),1) = log(tan(pi/2+asin(x)/2))
	   (ftake '%elliptic_f (ftake '%acos u) 1))
	  ((zerop1 u)
	   (ftake '%elliptic_kc m))
	  ((onep1 u)
	   0)
	  ((and (eq $triginverses '$all)
		(listp u)
		(eq (caar u) '%jacobi_cn)
		(alike1 (third u) m))
	   ;; inverse_jacobi_cn(cn(u)) = u
	   (second u))
	  (t
	   ;; Nothing to do
	   (give-up)))))

(def-simplifier inverse_jacobi_dn (u m)
  (let (args)
    (cond ((float-numerical-eval-p u m)
	   (to (bigfloat::bf-inverse-jacobi-dn (bigfloat:to (float u))
					       (bigfloat:to (float m)))))
	  ((setf args (complex-float-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to ($float u)))
		   (mm (bigfloat:to ($float m))))
	       (to (bigfloat::bf-inverse-jacobi-dn uu mm)))))
	  ((bigfloat-numerical-eval-p u m)
	   (let ((uu (bigfloat:to u))
		 (mm (bigfloat:to m)))
	     (to (bigfloat::bf-inverse-jacobi-dn uu mm))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (to (bigfloat::bf-inverse-jacobi-dn (bigfloat:to u) (bigfloat:to m)))))
	  ((onep1 m)
	   ;; x = dn(u,1) = sech(u).  so u = asech(x)
	   (ftake '%asech u))
	  ((onep1 u)
	   ;; jacobi_dn(0,m) = 1
	   0)
	  ((zerop1 ($ratsimp (sub u (power (sub 1 m) 1//2))))
	   ;; jacobi_dn(K(m),m) = sqrt(1-m) so
	   ;; inverse_jacobi_dn(sqrt(1-m),m) = K(m)
	   (ftake '%elliptic_kc m))
	  ((and (eq $triginverses '$all)
		(listp u)
		(eq (caar u) '%jacobi_dn)
		(alike1 (third u) m))
	   ;; inverse_jacobi_dn(dn(u)) = u
	   (second u))
	  (t
	   ;; Nothing to do
	   (give-up)))))

;;;; Elliptic integrals

(let ((errtol (expt (* 4 +flonum-epsilon+) 1/6))
      (c1 (float 1/24))
      (c2 (float 3/44))
      (c3 (float 1/14)))
  (declare (type flonum errtol c1 c2 c3))
  (defun crf (x y z)
    "Compute Carlson's incomplete or complete elliptic integral of the
first kind:

                   INF
                  /
                  [                     1
  RF(x, y, z) =   I    ----------------------------------- dt
                  ]    SQRT(x + t) SQRT(y + t) SQRT(z + t)
                  /
                   0

  x, y, and z may be complex.
"
    (declare (number x y z))
    (let ((x (coerce x '(complex flonum)))
	  (y (coerce y '(complex flonum)))
	  (z (coerce z '(complex flonum))))
      (declare (type (complex flonum) x y z))
      (loop
	 (let* ((mu (/ (+ x y z) 3))
		(x-dev (- 2 (/ (+ mu x) mu)))
		(y-dev (- 2 (/ (+ mu y) mu)))
		(z-dev (- 2 (/ (+ mu z) mu))))
	   (when (< (max (abs x-dev) (abs y-dev) (abs z-dev)) errtol)
	     (let ((e2 (- (* x-dev y-dev) (* z-dev z-dev)))
		   (e3 (* x-dev y-dev z-dev)))
	       (return (/ (+ 1
			     (* e2 (- (* c1 e2)
				      1/10
				      (* c2 e3)))
			     (* c3 e3))
			  (sqrt mu)))))
	   (let* ((x-root (sqrt x))
		  (y-root (sqrt y))
		  (z-root (sqrt z))
		  (lam (+ (* x-root (+ y-root z-root)) (* y-root z-root))))
	     (setf x (* (+ x lam) 1/4))
	     (setf y (* (+ y lam) 1/4))
	     (setf z (* (+ z lam) 1/4))))))))

;; Elliptic integral of the first kind (Legendre's form):
;;
;;
;;      phi
;;     /
;;     [             1
;;     I    ------------------- ds
;;     ]                  2
;;     /    SQRT(1 - m SIN (s))
;;     0

(defun elliptic-f (phi-arg m-arg)
  (flet ((base (phi-arg m-arg)
	   (cond ((and (realp m-arg) (realp phi-arg))
		  (let ((phi (float phi-arg))
			(m (float m-arg)))
		    (cond ((> m 1)
			   ;; A&S 17.4.15
			   ;;
			   ;; F(phi|m) = 1/sqrt(m)*F(theta|1/m)
			   ;;
			   ;; with sin(theta) = sqrt(m)*sin(phi)
			   (/ (elliptic-f (cl:asin (* (sqrt m) (sin phi))) (/ m))
					  (sqrt m)))
			  ((< m 0)
			   ;; A&S 17.4.17
			   (let* ((m (- m))
				  (m+1 (+ 1 m))
				  (root (sqrt m+1))
				  (m/m+1 (/ m m+1)))
			     (- (/ (elliptic-f (float (/ pi 2)) m/m+1)
				   root)
				(/ (elliptic-f (- (float (/ pi 2)) phi) m/m+1)
				   root))))
			  ((= m 0)
			   ;; A&S 17.4.19
			   phi)
			  ((= m 1)
			   ;; A&S 17.4.21
			   ;;
1			   ;; F(phi,1) = log(sec(phi)+tan(phi))
			   ;;          = log(tan(pi/4+pi/2))
			   (log (cl:tan (+ (/ phi 2) (float (/ pi 4))))))
			  ((minusp phi)
			   (- (elliptic-f (- phi) m)))
			  ((> phi pi)
			   ;; A&S 17.4.3
			   (multiple-value-bind (s phi-rem)
			       (truncate phi (float pi))
			     (+ (* 2 s (elliptic-k m))
				(elliptic-f phi-rem m))))
			  ((<= phi (/ pi 2))
			   (let ((sin-phi (sin phi))
				 (cos-phi (cos phi))
				 (k (sqrt m)))
			     (* sin-phi
				(bigfloat::bf-rf (* cos-phi cos-phi)
						 (* (- 1 (* k sin-phi))
						    (+ 1 (* k sin-phi)))
						 1.0))))
			  ((< phi pi)
			   (+ (* 2 (elliptic-k m))
			      (elliptic-f (- phi (float pi)) m)))
			  (t
			   (error "Shouldn't happen! Unhandled case in elliptic-f: ~S ~S~%"
				  phi-arg m-arg)))))
		 (t
		  (let ((phi (coerce phi-arg '(complex flonum)))
			(m (coerce m-arg '(complex flonum))))
		    (let ((sin-phi (sin phi))
			  (cos-phi (cos phi))
			  (k (sqrt m)))
		      (* sin-phi
			 (crf (* cos-phi cos-phi)
			      (* (- 1 (* k sin-phi))
				 (+ 1 (* k sin-phi)))
			      1.0))))))))
    ;; Elliptic F is quasi-periodic wrt to z:
    ;;
    ;; F(z|m) = F(z - pi*round(Re(z)/pi)|m) + 2*round(Re(z)/pi)*K(m)
    (let ((period (round (realpart phi-arg) pi)))
      (+ (base (- phi-arg (* pi period)) m-arg)
	 (if (zerop period)
	     0
	     (* 2 period
		(bigfloat:to (elliptic-k m-arg))))))))

;; Complete elliptic integral of the first kind
(defun elliptic-k (m)
  (cond ((realp m)
	 (cond ((< m 0)
		;; A&S 17.4.17
		(let* ((m (- m))
		       (m+1 (+ 1 m))
		       (root (sqrt m+1))
		       (m/m+1 (/ m m+1)))
		  (- (/ (elliptic-k m/m+1)
			root)
		     (/ (elliptic-f 0.0 m/m+1)
			root))))
	       ((= m 0)
		;; A&S 17.4.19
		(float (/ pi 2)))
	       ((= m 1)
		(maxima::merror
		 (intl:gettext "elliptic_kc: elliptic_kc(1) is undefined.")))
	       (t
		(bigfloat::bf-rf 0.0 (- 1 m)
				 1.0))))
	(t
	 (bigfloat::bf-rf 0.0 (- 1 m)
			  1.0))))

;; Elliptic integral of the second kind (Legendre's form):
;;
;;
;;      phi
;;     /
;;     [                  2
;;     I    SQRT(1 - m SIN (s)) ds
;;     ]
;;     /
;;      0

(defun elliptic-e (phi m)
  (declare (type flonum phi m))
  (flet ((base (phi m)
	   (cond ((= m 0)
		  ;; A&S 17.4.23
		  phi)
		 ((= m 1)
		  ;; A&S 17.4.25
		  (sin phi))
		 (t
		  (let* ((sin-phi (sin phi))
			 (cos-phi (cos phi))
			 (k (sqrt m))
			 (y (* (- 1 (* k sin-phi))
			       (+ 1 (* k sin-phi)))))
		    (to (- (* sin-phi
			      (bigfloat::bf-rf (* cos-phi cos-phi) y 1.0))
			   (* (/ m 3)
			      (expt sin-phi 3)
			      (bigfloat::bf-rd (* cos-phi cos-phi) y 1.0)))))))))
    ;; Elliptic E is quasi-periodic wrt to phi:
    ;;
    ;; E(z|m) = E(z - %pi*round(Re(z)/%pi)|m) + 2*round(Re(z)/%pi)*E(m)
    (let ((period (round (realpart phi) pi)))
      (+ (base (- phi (* pi period)) m)
	 (* 2 period (elliptic-ec m))))))

;; Complete version
(defun elliptic-ec (m)
  (declare (type flonum m))
  (cond ((= m 0)
	 ;; A&S 17.4.23
	 (float (/ pi 2)))
	((= m 1)
	 ;; A&S 17.4.25
	 1.0)
	(t
	 (let* ((y (- 1 m)))
	   (to (- (bigfloat::bf-rf 0.0 y 1.0)
		  (* (/ m 3)
		     (bigfloat::bf-rd 0.0 y 1.0))))))))


;; Define the elliptic integrals for maxima
;;
;; We use the definitions given in A&S 17.2.6 and 17.2.8.  In particular:
;;
;;                 phi
;;                /
;;                [             1
;; F(phi|m)  =    I    ------------------- ds
;;                ]                  2
;;                /    SQRT(1 - m SIN (s))
;;                 0
;;
;; and
;;
;;              phi
;;             /
;;             [                  2
;; E(phi|m) =  I    SQRT(1 - m SIN (s)) ds
;;             ]
;;             /
;;              0
;;
;; That is, we do not use the modular angle, alpha, as the second arg;
;; the parameter m = sin(alpha)^2 is used.
;;


;; The derivative of F(phi|m) wrt to phi is easy.  The derivative wrt
;; to m is harder.  Here is a derivation.  Hope I got it right.
;;
;; diff(integrate(1/sqrt(1-m*sin(x)^2),x,0,phi), m);
;;
;; 			   PHI
;; 			  /	       2
;; 			  [	    SIN (x)
;; 			  I    ------------------ dx
;; 			  ]		 2    3/2
;; 			  /    (1 - m SIN (x))
;; 			   0
;;  			  --------------------------
;; 				      2
;;
;; 
;; Now use the following relationship that is easily verified:
;;
;;               2                 2
;;    (1 - m) SIN (x)           COS (x)                 COS(x) SIN(x)
;;  ------------------- = ------------------- - DIFF(-------------------, x)
;;                 2                     2                          2
;;  SQRT(1 - m SIN (x))   SQRT(1 - m SIN (x))         SQRT(1 - m SIN (x))
;;
;;
;; Now integrate this to get:
;;
;; 
;; 	       PHI
;; 	      /		    2
;; 	      [		 SIN (x)
;;    (1 - m) I	   ------------------- dx =
;; 	      ]			 2
;; 	      /	   SQRT(1 - m SIN (x))
;; 	       0

;;
;; 			   PHI
;; 			  /	        2
;; 			  [	     COS (x)
;;  			+ I    ------------------- dx
;; 			  ]		     2
;; 			  /    SQRT(1 - m SIN (x))
;; 			   0
;; 			       COS(PHI) SIN(PHI)
;;  			  -  ---------------------
;; 					   2
;; 			     SQRT(1 - m SIN (PHI))
;;
;; Use the fact that cos(x)^2 = 1 - sin(x)^2 to show that this
;; integral on the RHS is:
;;
;;
;;		  (1 - m) elliptic_F(PHI, m) + elliptic_E(PHI, m)
;; 		  -------------------------------------------
;;				       m
;; So, finally, we have
;;
;;
;; 
;;   d			    
;; 2 -- (elliptic_F(PHI, m)) = 
;;   dm				
;;
;;  elliptic_E(PHI, m) - (1 - m) elliptic_F(PHI, m)     COS(PHI) SIN(PHI)
;;  ---------------------------------------------- - ---------------------
;; 			   m					  2
;; 						     SQRT(1 - m SIN (PHI))
;;   ----------------------------------------------------------------------
;; 				     1 - m

(defprop %elliptic_f
    ((phi m)
     ;; diff wrt phi
     ;; 1/sqrt(1-m*sin(phi)^2)
     ((mexpt simp)
      ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) ((%sin simp) phi) 2)))
      ((rat simp) -1 2))
     ;; diff wrt m
     ((mtimes simp) ((rat simp) 1 2)
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
      ((mplus simp)
       ((mtimes simp) ((mexpt simp) m -1)
	((mplus simp) ((%elliptic_e simp) phi m)
	 ((mtimes simp) -1 ((mplus simp) 1 ((mtimes simp) -1 m))
	  ((%elliptic_f simp) phi m))))
       ((mtimes simp) -1 ((%cos simp) phi) ((%sin simp) phi)
	((mexpt simp)
	 ((mplus simp) 1
	  ((mtimes simp) -1 m ((mexpt simp) ((%sin simp) phi) 2)))
	 ((rat simp) -1 2))))))
  grad)

;;
;; The derivative of E(phi|m) wrt to m is much simpler to derive than F(phi|m).
;;
;; Take the derivative of the definition to get
;;
;; 	    PHI
;; 	   /		 2
;; 	   [	      SIN (x)
;; 	   I    ------------------- dx
;; 	   ]		      2
;; 	   /    SQRT(1 - m SIN (x))
;; 	    0
;; 	 - ---------------------------
;; 			2
;;
;; It is easy to see that
;;
;; 			    PHI
;; 			   /		 2
;; 			   [	      SIN (x)
;;  elliptic_F(PHI, m) - m I    ------------------- dx = elliptic_E(PHI, m)
;; 			   ]		      2
;; 			   /    SQRT(1 - m SIN (x))
;; 			    0
;;
;; So we finally have
;;
;;   d			       elliptic_E(PHI, m) - elliptic_F(PHI, m)
;;   -- (elliptic_E(PHI, m)) = ---------------------------------------
;;   dm					        2 m

(defprop %elliptic_e
    ((phi m)
     ;; sqrt(1-m*sin(phi)^2)
     ((mexpt simp)
      ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) ((%sin simp) phi) 2)))
      ((rat simp) 1 2))
     ;; diff wrt m
     ((mtimes simp) ((rat simp) 1 2) ((mexpt simp) m -1)
      ((mplus simp) ((%elliptic_e simp) phi m)
       ((mtimes simp) -1 ((%elliptic_f simp) phi m)))))
  grad)
		    
(def-simplifier elliptic_f (phi m)
  (let (args)
    (cond ((float-numerical-eval-p phi m)
	   ;; Numerically evaluate it
	   (to (elliptic-f ($float phi) ($float m))))
	  ((setf args (complex-float-numerical-eval-p phi m))
	   (destructuring-bind (phi m)
	       args
	     (to (elliptic-f (bigfloat:to ($float phi))
			     (bigfloat:to ($float m))))))
	  ((bigfloat-numerical-eval-p phi m)
	   (to (bigfloat::bf-elliptic-f (bigfloat:to ($bfloat phi))
					(bigfloat:to ($bfloat m)))))
	  ((setf args (complex-bigfloat-numerical-eval-p phi m))
	   (destructuring-bind (phi m)
	       args
	     (to (bigfloat::bf-elliptic-f (bigfloat:to ($bfloat phi))
					  (bigfloat:to ($bfloat m))))))
	  ((zerop1 phi)
	   0)
	  ((zerop1 m)
	   ;; A&S 17.4.19
	   phi)
	  ((onep1 m)
	   ;; A&S 17.4.21.  Let's pick the log tan form.  But this
	   ;; isn't right if we know that abs(phi) > %pi/2, where
	   ;; elliptic_f is undefined (or infinity).
	   (cond ((not (eq '$pos (csign (sub ($abs phi) (div '$%pi 2)))))
		  (ftake '%log
			 (ftake '%tan
				(add (mul '$%pi (div 1 4))
				     (mul 1//2 phi)))))
		 (t
		  (merror (intl:gettext "elliptic_f: elliptic_f(~:M, ~:M) is undefined.")
					phi m))))
	  ((alike1 phi (div '$%pi 2))
	   ;; Complete elliptic integral
	   (ftake '%elliptic_kc m))
	  (t
	   ;; Nothing to do
	   (give-up)))))

(def-simplifier elliptic_e (phi m)
  (let (args)
    (cond ((float-numerical-eval-p phi m)
	   ;; Numerically evaluate it
	   (elliptic-e ($float phi) ($float m)))
	  ((complex-float-numerical-eval-p phi m)
	   (complexify (bigfloat::bf-elliptic-e (complex ($float ($realpart phi)) ($float ($imagpart phi)))
						(complex ($float ($realpart m)) ($float ($imagpart m))))))
	  ((bigfloat-numerical-eval-p phi m)
	   (to (bigfloat::bf-elliptic-e (bigfloat:to ($bfloat phi))
					(bigfloat:to ($bfloat m)))))
	  ((setf args (complex-bigfloat-numerical-eval-p phi m))
	   (destructuring-bind (phi m)
	       args
	     (to (bigfloat::bf-elliptic-e (bigfloat:to ($bfloat phi))
					  (bigfloat:to ($bfloat m))))))
	  ((zerop1 phi)
	   0)
	  ((zerop1 m)
	   ;; A&S 17.4.23
	   phi)
	  ((onep1 m)
	   ;; A&S 17.4.25, but handle periodicity:
	   ;; elliptic_e(x,m) = elliptic_e(x-%pi*round(x/%pi), m)
	   ;;                    + 2*round(x/%pi)*elliptic_ec(m)
	   ;;
	   ;; Or
	   ;;
	   ;; elliptic_e(x,1) = sin(x-%pi*round(x/%pi)) + 2*round(x/%pi)*elliptic_ec(m)
	   ;;
	   (let ((mult-pi (ftake '%round (div phi '$%pi))))
	     (add (ftake '%sin (sub phi
				    (mul '$%pi
					 mult-pi)))
		  (mul 2
		       (mul mult-pi
			    (ftake '%elliptic_ec m))))))
	  ((alike1 phi (div '$%pi 2))
	   ;; Complete elliptic integral
	   (ftake '%elliptic_ec m))
	  ((and ($numberp phi)
		(let ((r ($round (div phi '$%pi))))
		  (and ($numberp r)
		       (not (zerop1 r)))))
	   ;; Handle the case where phi is a number where we can apply
	   ;; the periodicity property without blowing up the
	   ;; expression.
	   (add (ftake '%elliptic_e
		       (add phi
			    (mul (mul -1 '$%pi)
				 (ftake '%round (div phi '$%pi))))
		       m)
		(mul 2
		     (mul (ftake '%round (div phi '$%pi))
			  (ftake '%elliptic_ec m)))))
	  (t
	   ;; Nothing to do
	   (give-up)))))

;; Complete elliptic integrals
;;
;; elliptic_kc(m) = elliptic_f(%pi/2, m)
;;
;; elliptic_ec(m) = elliptic_e(%pi/2, m)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %elliptic_kc simplim%elliptic_kc simplim%function)

(defun simplim%elliptic_kc (expr var val)
  ;; Look for the limit of the argument
  (let ((m (limit (cadr expr) var val 'think)))
    (cond ((onep1 m)
           ;; For an argument 1 return $infinity.
           '$infinity)
          (t 
            ;; All other cases are handled by the simplifier of the function.
            (simplify (list '(%elliptic_kc) m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-simplifier elliptic_kc (m)
  (let (args)
    (cond ((onep1 m)
           ;; elliptic_kc(1) is complex infinity. Maxima can not handle
           ;; infinities correctly, throw a Maxima error.
           (merror
             (intl:gettext "elliptic_kc: elliptic_kc(~:M) is undefined.")
             m))
          ((float-numerical-eval-p m)
	   ;; Numerically evaluate it
	   (to (elliptic-k ($float m))))
	  ((complex-float-numerical-eval-p m)
	   (complexify (bigfloat::bf-elliptic-k (complex ($float ($realpart m)) ($float ($imagpart m))))))
	  ((setf args (complex-bigfloat-numerical-eval-p m))
	   (destructuring-bind (m)
	       args
	     (to (bigfloat::bf-elliptic-k (bigfloat:to ($bfloat m))))))
	  ((zerop1 m)
	   (mul 1//2 '$%pi))
	  ((alike1 m 1//2)
	   ;; http://functions.wolfram.com/EllipticIntegrals/EllipticK/03/01/
	   ;;
	   ;; elliptic_kc(1/2) = 8*%pi^(3/2)/gamma(-1/4)^2
	   (div (mul 8 (power '$%pi (div 3 2)))
		(power (gm (div -1 4)) 2)))
	  ((eql -1 m)
	   ;; elliptic_kc(-1) = gamma(1/4)^2/(4*sqrt(2*%pi))
	   (div (power (gm (div 1 4)) 2)
		(mul 4 (power (mul 2 '$%pi) 1//2))))
	  ((alike1 m (add 17 (mul -12 (power 2 1//2))))
	   ;; elliptic_kc(17-12*sqrt(2)) = 2*(2+sqrt(2))*%pi^(3/2)/gamma(-1/4)^2
	   (div (mul 2 (mul (add 2 (power 2 1//2))
			    (power '$%pi (div 3 2))))
		(power (gm (div -1 4)) 2)))
	  ((or (alike1 m (div (add 2 (power 3 1//2))
			      4))
	       (alike1 m (add (div (power 3 1//2)
				   4)
			      1//2)))
	   ;; elliptic_kc((sqrt(3)+2)/4) = sqrt(%pi)*gamma(1/3)/gamma(5/6).
	   ;;
	   ;; First evaluate this integral, where y = sqrt(1+t^3).
	   ;;
	   ;;   integrate(1/y,t,-1,inf) = integrate(1/y,t,-1,0) + integrate(1/y,t,0,inf).
	   ;;
	   ;; The second integral, maxima gives beta(1/6,1/3)/3.
	   ;;
	   ;; For the first, we can use the change of variable x=-u^(1/3) to get
	   ;;
	   ;;   integrate(1/sqrt(1-u)/u^(2/3),u,0,1)
	   ;;
	   ;; which is a beta integral that maxima can evaluate to
	   ;; beta(1/3,1/2)/3.  Then we see the value of the initial
	   ;; integral is
	   ;;
	   ;;    beta(1/6,1/3)/3 + beta(1/3,1/2)/3
	   ;;
	   ;; (Thanks to Guilherme Namen for this derivation on the mailing list, 2023-03-09.)
	   ;;
	   ;; We can simplify this expression by converting to gamma functions:
	   ;;
	   ;;   beta(1/6,1/3)/3 + beta(1/3,1/2)/3 = 
	   ;;     (gamma(1/3)*(gamma(1/6)*gamma(5/6)+%pi))/(3*sqrt(%pi)*gamma(5/6));
	   ;;
	   ;; Using the reflection formula gamma(1-z)*gamma(z) =
	   ;; %pi/sin(%pi*z), we can write gamma(1/6)*gamma(5/6) =
	   ;; %pi/sin(%pi*1/6) = 2*%pi.  Finally, we have
	   ;;
	   ;;   sqrt(%pi)*gamma(1/3)/gamma(5/6);
	   ;;
	   ;; All that remains is to show that integrate(1/y,t) can be
	   ;; written as an inverse_jacobi_cn function with modulus
	   ;; (sqrt(3)+2)/4.
	   ;;
	   ;; First apply the substitution
	   ;;
	   ;;    s = (t+sqrt(3)+1)/(t-sqrt(3)+1).  We then have the integral
	   ;;
	   ;;   C*integrate(1/sqrt(s^2-1)/sqrt(s^2+4*sqrt(3)+7),s)
	   ;;
	   ;; where C is some constant.  From A&S 14.4.49, we can see
	   ;; this integral is the inverse_jacobi_nc function with
	   ;; modulus of (4*sqrt(3)+7)/(4*sqrt(3)+7+1) =
	   ;; (sqrt(3)+2)/4.
	   (div (mul (power '$%pi 1//2)
		     (ftake '%gamma (div 1 3)))
		(ftake '%gamma (div 5 6))))
	  ($hypergeometric_representation
	   ;; See http://functions.wolfram.com/08.02.26.0001.01
	   ;;
	   ;;   elliptic_kc(z) = %pi/2*%f[2,1]([1/2,1/2],[1], z)
	   ;;
	   (mul (div '$%pi 2)
		(ftake '%hypergeometric
		       (make-mlist 1//2 1//2)
		       (make-mlist 1)
		       m)))
	  (t
	   ;; Nothing to do
	   (give-up)))))

(defprop %elliptic_kc
    ((m)
     ;; diff wrt m
     ((mtimes)
      ((rat) 1 2)
      ((mplus) ((%elliptic_ec) m)
       ((mtimes) -1
	((%elliptic_kc) m)
	((mplus) 1 ((mtimes) -1 m))))
      ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
      ((mexpt) m -1)))
  grad)

(def-simplifier elliptic_ec (m)
  (let (args)
    (cond ((float-numerical-eval-p m)
	   ;; Numerically evaluate it
	   (elliptic-ec ($float m)))
	  ((setf args (complex-float-numerical-eval-p m))
	   (destructuring-bind (m)
	       args
	     (complexify (bigfloat::bf-elliptic-ec (bigfloat:to ($float m))))))
	  ((setf args (complex-bigfloat-numerical-eval-p m))
	   (destructuring-bind (m)
	       args
	     (to (bigfloat::bf-elliptic-ec (bigfloat:to ($bfloat m))))))
	  ;; Some special cases we know about.
	  ((zerop1 m)
	   (div '$%pi 2))
	  ((onep1 m)
	   1)
	  ((alike1 m 1//2)
	   ;; elliptic_ec(1/2). Use the identity
	   ;;
	   ;;   elliptic_ec(z)*elliptic_kc(1-z) - elliptic_kc(z)*elliptic_kc(1-z)
	   ;;     + elliptic_ec(1-z)*elliptic_kc(z) = %pi/2;
	   ;;
	   ;; Let z = 1/2 to get
	   ;;
	   ;;   %pi^(3/2)*'elliptic_ec(1/2)/gamma(3/4)^2-%pi^3/(4*gamma(3/4)^4) = %pi/2
	   ;;
	   ;; since we know that elliptic_kc(1/2) = %pi^(3/2)/(2*gamma(3/4)^2).  Hence
	   ;;
	   ;;   elliptic_ec(1/2)
	   ;;      = (2*%pi*gamma(3/4)^4+%pi^3)/(4*%pi^(3/2)*gamma(3/4)^2)
	   ;;      = gamma(3/4)^2/(2*sqrt(%pi))+%pi^(3/2)/(4*gamma(3/4)^2)
	   ;;
	   (add (div (power (ftake '%gamma (div 3 4)) 2)
		     (mul 2 (power '$%pi 1//2)))
		(div (power '$%pi (div 3 2))
		     (mul 4 (power (ftake '%gamma (div 3 4)) 2)))))
	  ((zerop1 (add 1 m))
	   ;; elliptic_ec(-1). Use the identity
	   ;; http://functions.wolfram.com/08.01.17.0002.01
	   ;;
	   ;;
	   ;;   elliptic_ec(z) = sqrt(1 - z)*elliptic_ec(z/(z-1))
	   ;;
	   ;; Let z = -1 to get
	   ;;
	   ;;   elliptic_ec(-1) = sqrt(2)*elliptic_ec(1/2)
	   ;;
	   ;; Should we expand out elliptic_ec(1/2) using the above result?
	   (mul (power 2 1//2)
		(ftake '%elliptic_ec 1//2)))
	  ($hypergeometric_representation
	   ;; See http://functions.wolfram.com/08.01.26.0001.01
	   ;;
	   ;;   elliptic_ec(z) = %pi/2*%f[2,1]([-1/2,1/2],[1], z)
	   ;;
	   (mul (div '$%pi 2)
		(ftake '%hypergeometric
		       (make-mlist -1//2 1//2)
		       (make-mlist 1)
		       m)))
	  (t
	   ;; Nothing to do
	   (give-up)))))

(defprop %elliptic_ec
    ((m)
     ((mtimes) ((rat) 1 2)
      ((mplus) ((%elliptic_ec) m)
       ((mtimes) -1 ((%elliptic_kc)
		     m)))
      ((mexpt) m -1)))
  grad)

;;
;; Elliptic integral of the third kind:
;;
;; (A&S 17.2.14)
;;
;;                 phi
;;                /
;;                [                     1
;; PI(n;phi|m) =  I    ----------------------------------- ds
;;                ]                  2               2
;;                /    SQRT(1 - m SIN (s)) (1 - n SIN (s))
;;                 0
;;
;; As with E and F, we do not use the modular angle alpha but the
;; parameter m = sin(alpha)^2.
;;
(def-simplifier elliptic_pi (n phi m)
  (let (args)
    (cond
      ((float-numerical-eval-p n phi m)
       ;; Numerically evaluate it
       (elliptic-pi ($float n) ($float phi) ($float m)))
      ((setf args (complex-float-numerical-eval-p n phi m))
       (destructuring-bind (n phi m)
	   args
         (elliptic-pi (bigfloat:to ($float n))
		      (bigfloat:to ($float phi))
		      (bigfloat:to ($float m)))))
      ((bigfloat-numerical-eval-p n phi m)
       (to (bigfloat::bf-elliptic-pi (bigfloat:to n)
				     (bigfloat:to phi)
				     (bigfloat:to m))))
      ((setq args (complex-bigfloat-numerical-eval-p n phi m))
       (destructuring-bind (n phi m)
	   args
	 (to (bigfloat::bf-elliptic-pi (bigfloat:to n)
				       (bigfloat:to phi)
				       (bigfloat:to m)))))
      ((zerop1 n)
       (ftake '%elliptic_f phi m))
      ((zerop1 m)
       ;; 3 cases depending on n < 1, n > 1, or n = 1.
       (let ((s (asksign (add -1 n))))
	 (case s
	   ($positive
	    (div (ftake '%atanh (mul (power (add n -1) 1//2)
				    (ftake '%tan phi)))
		 (power (add n -1) 1//2)))
	   ($negative
	    (div (ftake '%atan (mul (power (sub 1 n) 1//2)
				   (ftake '%tan phi)))
		 (power (sub 1 n) 1//2)))
	   ($zero
	    (ftake '%tan phi)))))
	  (t
	   ;; Nothing to do
	   (give-up)))))

;; Complete elliptic-pi.  That is phi = %pi/2.  Then
;; elliptic_pi(n,m)
;;   = Rf(0, 1-m,1) + Rj(0,1-m,1-n)*n/3;
(defun elliptic-pi-complete (n m)
  (to (bigfloat:+ (bigfloat::bf-rf 0 (- 1 m) 1)
	 (bigfloat:* 1/3 n (bigfloat::bf-rj 0 (- 1 m) 1 (- 1 n))))))

;; To compute elliptic_pi for all z, we use the property
;; (http://functions.wolfram.com/08.06.16.0002.01)
;; 
;; elliptic_pi(n, z + %pi*k, m)
;;   = 2*k*elliptic_pi(n, %pi/2, m) + elliptic_pi(n, z, m)
;;
;; So we are left with computing the integral for 0 <= z < %pi.  Using
;; Carlson's formulation produces the wrong values for %pi/2 < z <
;; %pi.  How to do that?
;;
;; Let
;;
;;   I(a,b) = integrate(1/(1-n*sin(x)^2)/sqrt(1 - m*sin(x)^2), x, a, b)
;;
;; That is, I(a,b) is the integral for the elliptic_pi function but
;; with a lower limit of a and an upper limit of b.
;;
;; Then, we want to compute I(0, z), with %pi <= z < %pi.  Let w = z +
;; %pi/2, 0 <= w < %pi/2.  Then
;;
;;   I(0, w+%pi/2) = I(0, %pi/2) + I(%pi/2, w+%pi/2)
;;
;; To evaluate I(%pi/2, w+%pi/2), use a change of variables:
;;
;;   changevar('integrate(1/(1-n*sin(x)^2)/sqrt(1 - m*sin(x)^2), x, %pi/2, w + %pi/2),
;;      x-%pi+u,u,x)
;;
;;     = integrate(-1/(sqrt(1-m*sin(u)^2)*(1-n*sin(u)^2)),u,%pi/2-w,%pi/2)
;;     = I(%pi/2-w,%pi/2)
;;     = I(0,%pi/2) - I(0,%pi/2-w)
;; 
;; Thus,
;;
;;   I(0,%pi/2+w) = 2*I(0,%pi/2) - I(0,%pi/2-w)
;;
;; This allows us to compute the general result with 0 <= z < %pi
;;
;;   I(0, k*%pi + z) = 2*k*I(0,%pi/2) + I(0,z);
;;
;; If 0 <= z < %pi/2, then the we are done.  If %pi/2 <= z < %pi, let
;; z = w+%pi/2. Then
;;
;;   I(0,z) = 2*I(0,%pi/2) - I(0,%pi/2-w)
;;
;; Or, since w = z-%pi/2:
;;
;;   I(0,z) = 2*I(0,%pi/2) - I(0,%pi-z)
 
(defun elliptic-pi (n phi m)
  ;; elliptic_pi(n, -phi, m) = -elliptic_pi(n, phi, m).  That is, it
  ;; is an odd function of phi.
  (when (minusp (realpart phi))
    (return-from elliptic-pi (- (elliptic-pi n (- phi) m))))

  ;; Note: Carlson's DRJ has n defined as the negative of the n given
  ;; in A&S.
  (flet ((base (n phi m)
	   ;; elliptic_pi(n,phi,m) =
	   ;;   sin(phi)*Rf(cos(phi)^2, 1-m*sin(phi)^2, 1)
	   ;;   - (-n / 3) * sin(phi)^3
	   ;;     * Rj(cos(phi)^2, 1-m*sin(phi)^2, 1, 1-n*sin(phi)^2)
	   (let* ((nn (- n))
		  (sin-phi (sin phi))
		  (cos-phi (cos phi))
		  (k (sqrt m))
		  (k2sin (* (- 1 (* k sin-phi))
			    (+ 1 (* k sin-phi)))))
	     (- (* sin-phi (bigfloat::bf-rf (expt cos-phi 2) k2sin 1.0))
		    (* (/ nn 3) (expt sin-phi 3)
		       (bigfloat::bf-rj (expt cos-phi 2) k2sin 1.0
					(- 1 (* n (expt sin-phi 2)))))))))
    ;; FIXME: Reducing the arg by pi has significant round-off.
    ;; Consider doing something better.
    (let* ((cycles (round (realpart phi) pi))
	   (rem (- phi (* cycles pi))))
      (let ((complete (elliptic-pi-complete n m)))
	(to (+ (* 2 cycles complete)
	       (base n rem m)))))))

;;; Deriviatives from functions.wolfram.com
;;; http://functions.wolfram.com/EllipticIntegrals/EllipticPi3/20/
(defprop %elliptic_pi
  ((n z m)
   ;Derivative wrt first argument
   ((mtimes) ((rat) 1 2)
    ((mexpt) ((mplus) m ((mtimes) -1 n)) -1)
    ((mexpt) ((mplus) -1 n) -1)
    ((mplus)
     ((mtimes) ((mexpt) n -1)
      ((mplus) ((mtimes) -1 m) ((mexpt) n 2))
      ((%elliptic_pi) n z m))
     ((%elliptic_e) z m)
     ((mtimes) ((mplus) m ((mtimes) -1 n)) ((mexpt) n -1)
      ((%elliptic_f) z m))
     ((mtimes) ((rat) -1 2) n
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 m ((mexpt) ((%sin) z) 2)))
       ((rat) 1 2))
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 n ((mexpt) ((%sin) z) 2)))
       -1)
      ((%sin) ((mtimes) 2 z)))))
   ;derivative wrt second argument
   ((mtimes)
    ((mexpt)
     ((mplus) 1 ((mtimes) -1 m ((mexpt) ((%sin) z) 2)))
     ((rat) -1 2))
    ((mexpt)
     ((mplus) 1 ((mtimes) -1 n ((mexpt) ((%sin) z) 2))) -1))
   ;Derivative wrt third argument
   ((mtimes) ((rat) 1 2)
    ((mexpt) ((mplus) ((mtimes) -1 m) n) -1)
    ((mplus) ((%elliptic_pi) n z m)
     ((mtimes) ((mexpt) ((mplus) -1 m) -1)
      ((%elliptic_e) z m))
     ((mtimes) ((rat) -1 2) ((mexpt) ((mplus) -1 m) -1) m
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 m ((mexpt) ((%sin) z) 2)))
       ((rat) -1 2))
      ((%sin) ((mtimes) 2 z))))))
  grad)

(in-package #:bigfloat)
;; Translation of Jim FitzSimons' bigfloat implementation of elliptic
;; integrals from http://www.getnet.com/~cherry/elliptbf3.mac.
;;
;; The algorithms are based on B.C. Carlson's "Numerical Computation
;; of Real or Complex Elliptic Integrals".  These are updated to the
;; algorithms in Journal of Computational and Applied Mathematics 118
;; (2000) 71-85 "Reduction Theorems for Elliptic Integrands with the
;; Square Root of two quadritic factors"
;;

;; NOTE: Despite the names indicating these are for bigfloat numbers,
;; the algorithms and routines are generic and will work with floats
;; and bigfloats.

(defun bferrtol (&rest args)
  ;; Compute error tolerance as sqrt(2^(-fpprec)).  Not sure this is
  ;; quite right, but it makes the routines more accurate as fpprec
  ;; increases.
  (sqrt (reduce #'min (mapcar #'(lambda (x)
				  (if (rationalp (realpart x))
				      maxima::+flonum-epsilon+
				      (epsilon x)))
			      args))))

;; rc(x,y) = integrate(1/2*(t+x)^(-1/2)/(t+y), t, 0, inf)
;;
;; log(x)  = (x-1)*rc(((1+x)/2)^2, x), x > 0
;; asin(x) = x * rc(1-x^2, 1), |x|<= 1
;; acos(x) = sqrt(1-x^2)*rc(x^2,1), 0 <= x <=1
;; atan(x) = x * rc(1,1+x^2)
;; asinh(x) = x * rc(1+x^2,1)
;; acosh(x) = sqrt(x^2-1) * rc(x^2,1), x >= 1
;; atanh(x) = x * rc(1,1-x^2), |x|<=1

(defun bf-rc (x y)
  (let ((yn y)
	xn z w a an pwr4 n epslon lambda sn s)
    (cond ((and (zerop (imagpart yn))
		(minusp (realpart yn)))
	   (setf xn (- x y))
	   (setf yn (- yn))
	   (setf z yn)
	   (setf w (sqrt (/ x xn))))
	  (t
	   (setf xn x)
	   (setf z yn)
	   (setf w 1)))
    (setf a (/ (+ xn yn yn) 3))
    (setf epslon (/ (abs (- a xn)) (bferrtol x y)))
    (setf an a)
    (setf pwr4 1)
    (setf n 0)
    (loop while (> (* epslon pwr4) (abs an))
       do
	 (setf pwr4 (/ pwr4 4))
	 (setf lambda (+ (* 2 (sqrt xn) (sqrt yn)) yn))
	 (setf an (/ (+ an lambda) 4))
	 (setf xn (/ (+ xn lambda) 4))
	 (setf yn (/ (+ yn lambda) 4))
	 (incf n))
    ;; c2=3/10,c3=1/7,c4=3/8,c5=9/22,c6=159/208,c7=9/8
    (setf sn (/ (* pwr4 (- z a)) an))
    (setf s (* sn sn (+ 3/10
			(* sn (+ 1/7
				 (* sn (+ 3/8
					  (* sn (+ 9/22
						   (* sn (+ 159/208
							    (* sn 9/8))))))))))))
    (/ (* w (+ 1 s))
       (sqrt an))))



;; See https://dlmf.nist.gov/19.16.E5:
;; 
;; rd(x,y,z) = integrate(3/2/sqrt(t+x)/sqrt(t+y)/sqrt(t+z)/(t+z), t, 0, inf)
;;
;; rd(1,1,1) = 1
;; E(K) = rf(0, 1-K^2, 1) - (K^2/3)*rd(0,1-K^2,1)
;;
;; B = integrate(s^2/sqrt(1-s^4), s, 0 ,1)
;;   = beta(3/4,1/2)/4
;;   = sqrt(%pi)*gamma(3/4)/gamma(1/4)
;;   = 1/3*rd(0,2,1)

(defun bf-rd (x y z)
  (let* ((xn x)
	 (yn y)
	 (zn z)
	 (a (/ (+ xn yn (* 3 zn)) 5))
	 (epslon (/ (max (abs (- a xn))
			 (abs (- a yn))
			 (abs (- a zn)))
		    (bferrtol x y z)))
	 (an a)
	 (sigma 0)
	 (power4 1)
	 (n 0)
	 xnroot ynroot znroot lam)
    (loop while (> (* power4 epslon) (abs an))
       do
	 (setf xnroot (sqrt xn))
	 (setf ynroot (sqrt yn))
	 (setf znroot (sqrt zn))
	 (setf lam (+ (* xnroot ynroot)
		      (* xnroot znroot)
		      (* ynroot znroot)))
	 (setf sigma (+ sigma (/ power4
				 (* znroot (+ zn lam)))))
	 (setf power4 (* power4 1/4))
	 (setf xn (* (+ xn lam) 1/4))
	 (setf yn (* (+ yn lam) 1/4))
	 (setf zn (* (+ zn lam) 1/4))
	 (setf an (* (+ an lam) 1/4))
	 (incf n))
    ;; c1=-3/14,c2=1/6,c3=9/88,c4=9/22,c5=-3/22,c6=-9/52,c7=3/26
    (let* ((xndev (/ (* (- a x) power4) an))
	   (yndev (/ (* (- a y) power4) an))
	   (zndev (- (* (+ xndev yndev) 1/3)))
	   (ee2 (- (* xndev yndev) (* 6 zndev zndev)))
	   (ee3 (* (- (* 3 xndev yndev)
		      (* 8 zndev zndev))
		   zndev))
	   (ee4 (* 3 (- (* xndev yndev) (* zndev zndev)) zndev zndev))
	   (ee5 (* xndev yndev zndev zndev zndev))
	   (s (+ 1
		 (* -3/14 ee2)
		 (* 1/6 ee3)
		 (* 9/88 ee2 ee2)
		 (* -3/22 ee4)
		 (* -9/52 ee2 ee3)
		 (* 3/26 ee5)
		 (* -1/16 ee2 ee2 ee2)
		 (* 3/10 ee3 ee3)
		 (* 3/20 ee2 ee4)
		 (* 45/272 ee2 ee2 ee3)
		 (* -9/68 (+ (* ee2 ee5) (* ee3 ee4))))))
    (+ (* 3 sigma)
       (/ (* power4 s)
	  (expt an 3/2))))))

;; See https://dlmf.nist.gov/19.16.E1
;;
;; rf(x,y,z) = 1/2*integrate(1/(sqrt(t+x)*sqrt(t+y)*sqrt(t+z)), t, 0, inf);
;;
;; rf(1,1,1) = 1
(defun bf-rf (x y z)
  (let* ((xn x)
	 (yn y)
	 (zn z)
	 (a (/ (+ xn yn zn) 3))
	 (epslon (/ (max (abs (- a xn))
			 (abs (- a yn))
			 (abs (- a zn)))
		    (bferrtol x y z)))
	 (an a)
	 (power4 1)
	 (n 0)
	 xnroot ynroot znroot lam)
    (loop while (> (* power4 epslon) (abs an))
       do
       (setf xnroot (sqrt xn))
       (setf ynroot (sqrt yn))
       (setf znroot (sqrt zn))
       (setf lam (+ (* xnroot ynroot)
		    (* xnroot znroot)
		    (* ynroot znroot)))
       (setf power4 (* power4 1/4))
       (setf xn (* (+ xn lam) 1/4))
       (setf yn (* (+ yn lam) 1/4))
       (setf zn (* (+ zn lam) 1/4))
       (setf an (* (+ an lam) 1/4))
       (incf n))
    ;; c1=-3/14,c2=1/6,c3=9/88,c4=9/22,c5=-3/22,c6=-9/52,c7=3/26
    (let* ((xndev (/ (* (- a x) power4) an))
	   (yndev (/ (* (- a y) power4) an))
	   (zndev (- (+ xndev yndev)))
	   (ee2 (- (* xndev yndev) (* 6 zndev zndev)))
	   (ee3 (* xndev yndev zndev))
	   (s (+ 1
		 (* -1/10 ee2)
		 (* 1/14 ee3)
		 (* 1/24 ee2 ee2)
		 (* -3/44 ee2 ee3))))
      (/ s (sqrt an)))))
  
(defun bf-rj1 (x y z p)
  (let* ((xn x)
	 (yn y)
	 (zn z)
	 (pn p)
	 (en (* (- pn xn)
		(- pn yn)
		(- pn zn)))
	 (sigma 0)
	 (power4 1)
	 (k 0)
	 (a (/ (+ xn yn zn pn pn) 5))
	 (epslon (/ (max (abs (- a xn))
			 (abs (- a yn))
			 (abs (- a zn))
			 (abs (- a pn)))
		    (bferrtol x y z p)))
	 (an a)
	 xnroot ynroot znroot pnroot lam dn)
    (loop while (> (* power4 epslon) (abs an))
       do
       (setf xnroot (sqrt xn))
       (setf ynroot (sqrt yn))
       (setf znroot (sqrt zn))
       (setf pnroot (sqrt pn))
       (setf lam (+ (* xnroot ynroot)
		    (* xnroot znroot)
		    (* ynroot znroot)))
       (setf dn (* (+ pnroot xnroot)
		   (+ pnroot ynroot)
		   (+ pnroot znroot)))
       (setf sigma (+ sigma
		      (/ (* power4
			    (bf-rc 1 (+ 1 (/ en (* dn dn)))))
			 dn)))
       (setf power4 (* power4 1/4))
       (setf en (/ en 64))
       (setf xn (* (+ xn lam) 1/4))
       (setf yn (* (+ yn lam) 1/4))
       (setf zn (* (+ zn lam) 1/4))
       (setf pn (* (+ pn lam) 1/4))
       (setf an (* (+ an lam) 1/4))
       (incf k))
    (let* ((xndev (/ (* (- a x) power4) an))
	   (yndev (/ (* (- a y) power4) an))
	   (zndev (/ (* (- a z) power4) an))
	   (pndev (* -0.5 (+ xndev yndev zndev)))
	   (ee2 (+ (* xndev yndev)
		   (* xndev zndev)
		   (* yndev zndev)
		   (* -3 pndev pndev)))
	   (ee3 (+ (* xndev yndev zndev)
		   (* 2 ee2 pndev)
		   (* 4 pndev pndev pndev)))
	   (ee4 (* (+ (* 2 xndev yndev zndev)
		      (* ee2 pndev)
		      (* 3 pndev pndev pndev))
		   pndev))
	   (ee5 (* xndev yndev zndev pndev pndev))
	   (s (+ 1
		 (* -3/14 ee2)
		 (* 1/6 ee3)
		 (* 9/88 ee2 ee2)
		 (* -3/22 ee4)
		 (* -9/52 ee2 ee3)
		 (* 3/26 ee5)
		 (* -1/16 ee2 ee2 ee2)
		 (* 3/10 ee3 ee3)
		 (* 3/20 ee2 ee4)
		 (* 45/272 ee2 ee2 ee3)
		 (* -9/68 (+ (* ee2 ee5) (* ee3 ee4))))))
      (+ (* 6 sigma)
	 (/ (* power4 s)
	    (sqrt (* an an an)))))))

(defun bf-rj (x y z p)
  (let* ((xn x)
	 (yn y)
	 (zn z)
	 (qn (- p)))
    (cond ((and (and (zerop (imagpart xn)) (>= (realpart xn) 0))
		(and (zerop (imagpart yn)) (>= (realpart yn) 0))
		(and (zerop (imagpart zn)) (>= (realpart zn) 0))
		(and (zerop (imagpart qn)) (> (realpart qn) 0)))
	   (destructuring-bind (xn yn zn)
	       (sort (list xn yn zn) #'<)
	     (let* ((pn (+ yn (* (- zn yn) (/ (- yn xn) (+ yn qn)))))
		    (s (- (* (- pn yn) (bf-rj1 xn yn zn pn))
			  (* 3 (bf-rf xn yn zn)))))
	       (setf s (+ s (* 3 (sqrt (/ (* xn yn zn)
					  (+ (* xn zn) (* pn qn))))
			       (bf-rc (+ (* xn zn) (* pn qn)) (* pn qn)))))
	       (/ s (+ yn qn)))))
	  (t
	   (bf-rj1 x y z p)))))

(defun bf-rg (x y z)
  (* 0.5
     (+ (* z (bf-rf x y z))
	(* (- z x)
	   (- y z)
	   (bf-rd x y z)
	   1/3)
	(sqrt (/ (* x y) z)))))

;; elliptic_f(phi,m) = sin(phi)*rf(cos(phi)^2, 1-m*sin(phi)^2,1)
(defun bf-elliptic-f (phi m)
  (flet ((base (phi m)
	   (cond ((= m 1)
		  ;; F(z|1) = log(tan(z/2+%pi/4))
		  (log (tan (+ (/ phi 2) (/ (%pi phi) 4)))))
		 (t
		  (let ((s (sin phi))
			(c (cos phi)))
		    (* s (bf-rf (* c c) (- 1 (* m s s)) 1)))))))
    ;; Handle periodicity (see elliptic-f)
    (let* ((bfpi (%pi phi))
	   (period (round (realpart phi) bfpi)))
      (+ (base (- phi (* bfpi period)) m)
	 (if (zerop period)
	     0
	     (* 2 period (bf-elliptic-k m)))))))

;; elliptic_kc(k) = rf(0, 1-k^2,1)
;;
;; or
;; elliptic_kc(m) = rf(0, 1-m,1)

(defun bf-elliptic-k (m)
  (cond ((= m 0)
	 (if (maxima::$bfloatp m)
	     (maxima::$bfloat (maxima::div 'maxima::$%pi 2))
	     (float (/ pi 2) 1e0)))
	((= m 1)
	 (maxima::merror
	  (intl:gettext "elliptic_kc: elliptic_kc(1) is undefined.")))
	(t
	 (bf-rf 0 (- 1 m) 1))))

;; elliptic_e(phi, k) = sin(phi)*rf(cos(phi)^2,1-k^2*sin(phi)^2,1)
;;    - (k^2/3)*sin(phi)^3*rd(cos(phi)^2, 1-k^2*sin(phi)^2,1)
;;
;;
;; or 
;; elliptic_e(phi, m) = sin(phi)*rf(cos(phi)^2,1-m*sin(phi)^2,1)
;;    - (m/3)*sin(phi)^3*rd(cos(phi)^2, 1-m*sin(phi)^2,1)
;;
(defun bf-elliptic-e (phi m)
  (flet ((base (phi m)
	   (let* ((s (sin phi))
		  (c (cos phi))
		  (c2 (* c c))
		  (s2 (- 1 (* m s s))))
	     (- (* s (bf-rf c2 s2 1))
		(* (/ m 3) (* s s s) (bf-rd c2 s2 1))))))
    ;; Elliptic E is quasi-periodic wrt to phi:
    ;;
    ;; E(z|m) = E(z - %pi*round(Re(z)/%pi)|m) + 2*round(Re(z)/%pi)*E(m)
    (let* ((bfpi (%pi phi))
	   (period (round (realpart phi) bfpi)))
      (+ (base (- phi (* bfpi period)) m)
	 (* 2 period (bf-elliptic-ec m))))))
    

;; elliptic_ec(k) = rf(0,1-k^2,1) - (k^2/3)*rd(0,1-k^2,1);
;;
;; or
;; elliptic_ec(m) = rf(0,1-m,1) - (m/3)*rd(0,1-m,1);

(defun bf-elliptic-ec (m)
  (cond ((= m 0)
	 (if (typep m 'bigfloat)
	     (bigfloat (maxima::$bfloat (maxima::div 'maxima::$%pi 2)))
	     (float (/ pi 2) 1e0)))
	((= m 1)
	 (if (typep m 'bigfloat)
	     (bigfloat 1)
	     1e0))
	(t
	 (let ((m1 (- 1 m)))
	   (- (bf-rf 0 m1 1)
	      (* m 1/3 (bf-rd 0 m1 1)))))))

(defun bf-elliptic-pi-complete (n m)
  (+ (bf-rf 0 (- 1 m) 1)
     (* 1/3 n (bf-rj 0 (- 1 m) 1 (- 1 n)))))

(defun bf-elliptic-pi (n phi m)
  ;; Note: Carlson's DRJ has n defined as the negative of the n given
  ;; in A&S.
  (flet ((base (n phi m)
	   (let* ((nn (- n))
		  (sin-phi (sin phi))
		  (cos-phi (cos phi))
		  (k (sqrt m))
		  (k2sin (* (- 1 (* k sin-phi))
			    (+ 1 (* k sin-phi)))))
	     (- (* sin-phi (bf-rf (expt cos-phi 2) k2sin 1.0))
		(* (/ nn 3) (expt sin-phi 3)
		   (bf-rj (expt cos-phi 2) k2sin 1.0
			  (- 1 (* n (expt sin-phi 2)))))))))
    ;; FIXME: Reducing the arg by pi has significant round-off.
    ;; Consider doing something better.
    (let* ((bf-pi (%pi (realpart phi)))
	   (cycles (round (realpart phi) bf-pi))
	   (rem (- phi (* cycles bf-pi))))
	(let ((complete (bf-elliptic-pi-complete n m)))
	  (+ (* 2 cycles complete)
	     (base n rem m))))))

;; Compute inverse_jacobi_sn, for float or bigfloat args.
(defun bf-inverse-jacobi-sn (u m)
  (* u (bf-rf (- 1 (* u u))
	      (- 1 (* m u u))
	      1)))

;; Compute inverse_jacobi_dn.  We use the following identity
;; from Gradshteyn & Ryzhik, 8.153.6
;;
;;   w = dn(z|m) = cn(sqrt(m)*z, 1/m)
;;
;; Solve for z to get
;;
;;   z = inverse_jacobi_dn(w,m)
;;     = 1/sqrt(m) * inverse_jacobi_cn(w, 1/m)
(defun bf-inverse-jacobi-dn (w m)
  (cond ((= w 1)
	 (float 0 w))
	((= m 1)
	 ;; jacobi_dn(x,1) = sech(x) so the inverse is asech(x)
	 (maxima::take '(maxima::%asech) (maxima::to w)))
	(t
	 ;; We should do something better to make sure that things
	 ;; that should be real are real.
	 (/ (to (maxima::take '(maxima::%inverse_jacobi_cn)
			      (maxima::to w)
			      (maxima::to (/ m))))
	    (sqrt m)))))

(in-package :maxima)

;; Define Carlson's elliptic integrals.

(def-simplifier carlson_rc (x y)
  (let (args)
    (flet ((calc (x y)
	     (flet ((floatify (z)
		      ;; If z is a complex rational, convert to a
		      ;; complex double-float.  Otherwise, leave it as
		      ;; is.  If we don't do this, %i is handled as
		      ;; #c(0 1), which makes bf-rc use single-float
		      ;; arithmetic instead of the desired
		      ;; double-float.
		      (if (and (complexp z) (rationalp (realpart z)))
			  (complex (float (realpart z))
				   (float (imagpart z)))
			  z)))
	       (to (bigfloat::bf-rc (floatify (bigfloat:to x))
				    (floatify (bigfloat:to y)))))))
      ;; See comments from bf-rc
      (cond ((float-numerical-eval-p x y)
	     (calc ($float x) ($float y)))
	    ((bigfloat-numerical-eval-p x y)
	     (calc ($bfloat x) ($bfloat y)))
	    ((setf args (complex-float-numerical-eval-p x y))
	     (destructuring-bind (x y)
		 args
	       (calc ($float x) ($float y))))
	    ((setf args (complex-bigfloat-numerical-eval-p x y))
	     (destructuring-bind (x y)
		 args
	       (calc ($bfloat x) ($bfloat y))))
	    ((and (zerop1 x)
		  (onep1 y))
	     ;; rc(0, 1) = %pi/2
	     (div '$%pi 2))
	    ((and (zerop1 x)
		  (alike1 y (div 1 4)))
	     ;; rc(0,1/4) = %pi
	     '$%pi)
	    ((and (eql x 2)
		  (onep1 y))
	     ;; rc(2,1) = 1/2*integrate(1/sqrt(t+2)/(t+1), t, 0, inf)
	     ;;   = (log(sqrt(2)+1)-log(sqrt(2)-1))/2
	     ;; ratsimp(logcontract(%)),algebraic:
	     ;;   = -log(3-2^(3/2))/2
	     ;;   = -log(sqrt(3-2^(3/2)))
	     ;;   = -log(sqrt(2)-1)
	     ;;   = log(1/(sqrt(2)-1))
	     ;; ratsimp(%),algebraic;
	     ;;   = log(sqrt(2)+1)
	     (ftake '%log (add 1 (power 2 1//2))))
	    ((and (alike x '$%i)
		  (alike y (add 1 '$%i)))
	     ;; rc(%i, %i+1) = 1/2*integrate(1/sqrt(t+%i)/(t+%i+1), t, 0, inf)
	     ;;   = %pi/2-atan((-1)^(1/4))
	     ;; ratsimp(logcontract(ratsimp(rectform(%o42)))),algebraic;
	     ;;   = (%i*log(3-2^(3/2))+%pi)/4
	     ;;   = (%i*log(3-2^(3/2)))/4+%pi/4
	     ;;   = %i*log(sqrt(3-2^(3/2)))/2+%pi/4
	     ;; sqrtdenest(%);
	     ;;   = %pi/4 + %i*log(sqrt(2)-1)/2
	     (add (div '$%pi 4)
		  (mul '$%i
		       1//2
		       (ftake '%log (sub (power 2 1//2) 1)))))
	    ((and (zerop1 x)
		  (alike1 y '$%i))
	     ;; rc(0,%i) = 1/2*integrate(1/(sqrt(t)*(t+%i)), t, 0, inf)
	     ;;   = -((sqrt(2)*%i-sqrt(2))*%pi)/4
	     ;;   = ((1-%i)*%pi)/2^(3/2)
	     (div (mul (sub 1 '$%i)
		       '$%pi)
		  (power 2 3//2)))
	    ((and (alike1 x y)
		  (eq ($sign ($realpart x)) '$pos))
	     ;; carlson_rc(x,x) = 1/2*integrate(1/sqrt(t+x)/(t+x), t, 0, inf)
	     ;;    = 1/sqrt(x)
	     (power x -1//2))
	    ((and (alike1 x (power (div (add 1 y) 2) 2))
		  (eq ($sign ($realpart y)) '$pos))
	     ;; Rc(((1+x)/2)^2,x) = log(x)/(x-1) for x > 0.
	     ;;
	     ;; This is done by looking at Rc(x,y) and seeing if
	     ;; ((1+y)/2)^2 is the same as x.
	     (div (ftake '%log y)
		  (sub y 1)))
	    (t
	     (give-up))))))
  
(def-simplifier carlson_rd (x y z)
  (let (args)
    (flet ((calc (x y z)
	     (to (bigfloat::bf-rd (bigfloat:to x)
				  (bigfloat:to y)
				  (bigfloat:to z)))))
      ;; See https://dlmf.nist.gov/19.20.E18
      (cond ((and (eql x 1)
		  (eql x y)
		  (eql y z))
	     ;; Rd(1,1,1) = 1
	     1)
	    ((and (alike1 x y)
		  (alike1 y z))
	     ;; Rd(x,x,x) = x^(-3/2)
	     (power x (div -3 2)))
	    ((and (zerop1 x)
		  (alike1 y z))
	     ;; Rd(0,y,y) = 3/4*%pi*y^(-3/2)
	     (mul (div 3 4)
		  '$%pi
		  (power y (div -3 2))))
	    ((alike1 y z)
	     ;; Rd(x,y,y) = 3/(2*(y-x))*(Rc(x, y) - sqrt(x)/y)
	     (mul (div 3 (mul 2 (sub y x)))
		  (sub (ftake '%carlson_rc x y)
		       (div (power x 1//2)
			    y))))
	    ((alike1 x y)
	     ;; Rd(x,x,z) = 3/(z-x)*(Rc(z,x) - 1/sqrt(z))
	     (mul (div 3 (sub z x))
		  (sub (ftake '%carlson_rc z x)
		       (div 1 (power z 1//2)))))
	    ((and (eql z 1)
		  (or (and (eql x 0)
			   (eql y 2))
		      (and (eql x 2)
			   (eql y 0))))
	     ;; Rd(0,2,1) = 3*(gamma(3/4)^2)/sqrt(2*%pi)
	     ;; See https://dlmf.nist.gov/19.20.E22.
	     ;;
	     ;; But that's the same as
	     ;; 3*sqrt(%pi)*gamma(3/4)/gamma(1/4).  We can see this by
	     ;; taking the ratio to get
	     ;; gamma(1/4)*gamma(3/4)/sqrt(2)*%pi.  But
	     ;; gamma(1/4)*gamma(3/4) = beta(1/4,3/4) = sqrt(2)*%pi.
	     ;; Hence, the ratio is 1.
	     ;;
	     ;; Note also that Rd(x,y,z) = Rd(y,x,z)
	     (mul 3
		  (power '$%pi 1//2)
		  (div (ftake '%gamma (div 3 4))
		       (ftake '%gamma (div 1 4)))))
	    ((and (or (eql x 0) (eql y 0))
		  (eql z 1))
	     ;; 1/3*m*Rd(0,1-m,1) = K(m) - E(m).
	     ;; See https://dlmf.nist.gov/19.25.E1
	     ;;
	     ;; Thus, Rd(0,y,1) = 3/(1-y)*(K(1-y) - E(1-y))
	     ;;
	     ;; Note that Rd(x,y,z) = Rd(y,x,z).
	     (let ((m (sub 1 y)))
	       (mul (div 3 m)
		    (sub (ftake '%elliptic_kc m)
			 (ftake '%elliptic_ec m)))))
	    ((or (and (eql x 0)
		      (eql y 1))
		 (and (eql x 1)
		      (eql y 0)))
	     ;; 1/3*m*(1-m)*Rd(0,1,1-m) = E(m) - (1-m)*K(m)
	     ;; See https://dlmf.nist.gov/19.25.E1
	     ;;
	     ;; Thus
	     ;;  Rd(0,1,z) = 3/(z*(1-z))*(E(1-z) - z*K(1-z))
	     ;; Recall that Rd(x,y,z) = Rd(y,x,z).
	     (mul (div 3 (mul z (sub 1 z)))
		  (sub (ftake '%elliptic_ec (sub 1 z))
		       (mul z
			    (ftake '%elliptic_kc (sub 1 z))))))
	    ((float-numerical-eval-p x y z)
	     (calc ($float x) ($float y) ($float z)))
	    ((bigfloat-numerical-eval-p x y z)
	     (calc ($bfloat x) ($bfloat y) ($bfloat z)))
	    ((setf args (complex-float-numerical-eval-p x y z))
	     (destructuring-bind (x y z)
		 args
	       (calc ($float x) ($float y) ($float z))))
	    ((setf args (complex-bigfloat-numerical-eval-p x y z))
	     (destructuring-bind (x y z)
		 args
	       (calc ($bfloat x) ($bfloat y) ($bfloat z))))
	    (t
	     (give-up))))))

(def-simplifier carlson_rf (x y z)
  (let (args)
    (flet ((calc (x y z)
	     (to (bigfloat::bf-rf (bigfloat:to x)
				  (bigfloat:to y)
				  (bigfloat:to z)))))
      ;; See https://dlmf.nist.gov/19.20.i
      (cond ((and (alike1 x y)
		  (alike1 y z))
	     ;; Rf(x,x,x) = x^(-1/2)
	     (power x -1//2))
	    ((and (zerop1 x)
		  (alike1 y z))
	     ;; Rf(0,y,y) = 1/2*%pi*y^(-1/2)
	     (mul 1//2 '$%pi
		  (power y -1//2)))
	    ((alike1 y z)
	     (ftake '%carlson_rc x y))
	    ((some #'(lambda (args)
		       (destructuring-bind (x y z)
			   args
			 (and (zerop1 x)
			      (eql y 1)
			      (eql z 2))))
		   (list (list x y z)
			 (list x z y)
			 (list y x z)
			 (list y z x)
			 (list z x y)
			 (list z y x)))
	     ;; Rf(0,1,2) = (gamma(1/4))^2/(4*sqrt(2*%pi))
	     ;;
	     ;; And Rf is symmetric in all the args, so check every
	     ;; permutation too.  This could probably be simplified
	     ;; without consing all the lists, but I'm lazy.
	     (div (power (ftake '%gamma (div 1 4)) 2)
		  (mul 4 (power (mul 2 '$%pi) 1//2))))
	    ((some #'(lambda (args)
		       (destructuring-bind (x y z)
			   args
			 (and (alike1 x '$%i)
			      (alike1 y (mul -1 '$%i))
			      (eql z 0))))
		   (list (list x y z)
			 (list x z y)
			 (list y x z)
			 (list y z x)
			 (list z x y)
			 (list z y x)))
	     ;; rf(%i, -%i, 0)
	     ;;   = 1/2*integrate(1/sqrt(t^2+1)/sqrt(t),t,0,inf)
	     ;;   = beta(1/4,1/4)/4;
	     ;; makegamma(%)
	     ;;   = gamma(1/4)^2/(4*sqrt(%pi))
	     ;;
	     ;; Rf is symmetric, so check all the permutations too.
	     (div (power (ftake '%gamma (div 1 4)) 2)
		  (mul 4 (power '$%pi 1//2))))
	    ((setf args
		   (some #'(lambda (args)
			     (destructuring-bind (x y z)
				 args
			       ;; Check that x = 0 and z = 1, and
			       ;; return y.
			       (and (zerop1 x)
				    (eql z 1)
				    y)))
			 (list (list x y z)
			       (list x z y)
			       (list y x z)
			       (list y z x)
			       (list z x y)
			       (list z y x))))
	     ;; Rf(0,1-m,1) = elliptic_kc(m).
	     ;; See https://dlmf.nist.gov/19.25.E1
	     (ftake '%elliptic_kc (sub 1 args)))
	    ((some #'(lambda (args)
		       (destructuring-bind (x y z)
			   args
			 (and (alike1 x '$%i)
			      (alike1 y (mul -1 '$%i))
			      (eql z 0))))
		   (list (list x y z)
			 (list x z y)
			 (list y x z)
			 (list y z x)
			 (list z x y)
			 (list z y x)))
	     ;; rf(%i, -%i, 0)
	     ;;   = 1/2*integrate(1/sqrt(t^2+1)/sqrt(t),t,0,inf)
	     ;;   = beta(1/4,1/4)/4;
	     ;; makegamma(%)
	     ;;   = gamma(1/4)^2/(4*sqrt(%pi))
	     ;;
	     ;; Rf is symmetric, so check all the permutations too.
	     (div (power (ftake '%gamma (div 1 4)) 2)
		  (mul 4 (power '$%pi 1//2))))
	    ((float-numerical-eval-p x y z)
	     (calc ($float x) ($float y) ($float z)))
	    ((bigfloat-numerical-eval-p x y z)
	     (calc ($bfloat x) ($bfloat y) ($bfloat z)))
	    ((setf args (complex-float-numerical-eval-p x y z))
	     (destructuring-bind (x y z)
		 args
	       (calc ($float x) ($float y) ($float z))))
	    ((setf args (complex-bigfloat-numerical-eval-p x y z))
	     (destructuring-bind (x y z)
		 args
	       (calc ($bfloat x) ($bfloat y) ($bfloat z))))
	    (t
	     (give-up))))))

(def-simplifier carlson_rj (x y z p)
  (let (args)
    (flet ((calc (x y z p)
	     (to (bigfloat::bf-rj (bigfloat:to x)
				  (bigfloat:to y)
				  (bigfloat:to z)
				  (bigfloat:to p)))))
      ;; See https://dlmf.nist.gov/19.20.iii
      (cond ((and (alike1 x y)
		  (alike1 y z)
		  (alike1 z p))
	     ;; Rj(x,x,x,x) = x^(-3/2)
	     (power x (div -3 2)))
	    ((alike1 z p)
	     ;; Rj(x,y,z,z) = Rd(x,y,z)
	     (ftake '%carlson_rd x y z))
	    ((and (zerop1 x)
		  (alike1 y z))
	     ;; Rj(0,y,y,p) = 3*%pi/(2*(y*sqrt(p)+p*sqrt(y)))
	     (div (mul 3 '$%pi)
		  (mul 2
		       (add (mul y (power p 1//2))
			    (mul p (power y 1//2))))))
	    ((alike1 y z)
	     ;; Rj(x,y,y,p) = 3/(p-y)*(Rc(x,y) - Rc(x,p))
	     (mul (div 3 (sub p y))
		  (sub (ftake '%carlson_rc x y)
		       (ftake '%carlson_rc x p))))
	    ((and (alike1 y z)
		  (alike1 y p))
	     ;; Rj(x,y,y,y) = Rd(x,y,y)
	     (ftake '%carlson_rd x y y))
	    ((float-numerical-eval-p x y z p)
	     (calc ($float x) ($float y) ($float z) ($float p)))
	    ((bigfloat-numerical-eval-p x y z p)
	     (calc ($bfloat x) ($bfloat y) ($bfloat z) ($bfloat p)))
	    ((setf args (complex-float-numerical-eval-p x y z p))
	     (destructuring-bind (x y z p)
		 args
	       (calc ($float x) ($float y) ($float z) ($float p))))
	    ((setf args (complex-bigfloat-numerical-eval-p x y z p))
	     (destructuring-bind (x y z p)
		 args
	       (calc ($bfloat x) ($bfloat y) ($bfloat z) ($bfloat p))))
	    (t
	     (give-up))))))
		  
;;; Other Jacobian elliptic functions

;; jacobi_ns(u,m) = 1/jacobi_sn(u,m)
(defprop %jacobi_ns
    ((u m)
     ;; diff wrt u
     ((mtimes) -1 ((%jacobi_cn) u m) ((%jacobi_dn) u m)
      ((mexpt) ((%jacobi_sn) u m) -2))
     ;; diff wrt m
     ((mtimes) -1 ((mexpt) ((%jacobi_sn) u m) -2)
      ((mplus)
       ((mtimes) ((rat) 1 2)
	((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	((mexpt) ((%jacobi_cn) u m) 2)
	((%jacobi_sn) u m))
       ((mtimes) ((rat) 1 2) ((mexpt) m -1)
	((%jacobi_cn) u m) ((%jacobi_dn) u m)
	((mplus) u
	 ((mtimes) -1
	  ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	  ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	   m)))))))
  grad)

(def-simplifier jacobi_ns (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
	   (to (bigfloat:/ (bigfloat::sn (bigfloat:to ($float u))
					 (bigfloat:to ($float m))))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat:/ (bigfloat::sn (bigfloat:to ($float u))
				       (bigfloat:to ($float m)))))))
	  ((bigfloat-numerical-eval-p u m)
	   (let ((uu (bigfloat:to ($bfloat u)))
		 (mm (bigfloat:to ($bfloat m))))
	     (to (bigfloat:/ (bigfloat::sn uu mm)))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to ($bfloat u)))
		   (mm (bigfloat:to ($bfloat m))))
	       (to (bigfloat:/ (bigfloat::sn uu mm))))))
	  ((zerop1 m)
	   ;; A&S 16.6.10
	   (ftake '%csc u))
	  ((onep1 m)
	   ;; A&S 16.6.10
	   (ftake '%coth u))
	  ((zerop1 u)
	   (dbz-err1 'jacobi_ns))
	  ((and $trigsign (mminusp* u))
	   ;; ns is odd
	   (neg (ftake* '%jacobi_ns (neg u) m)))
	  ((and $triginverses
		(listp u)
		(member (caar u) '(%inverse_jacobi_sn
				   %inverse_jacobi_ns
				   %inverse_jacobi_cn
				   %inverse_jacobi_nc
				   %inverse_jacobi_dn
				   %inverse_jacobi_nd
				   %inverse_jacobi_sc
				   %inverse_jacobi_cs
				   %inverse_jacobi_sd
				   %inverse_jacobi_ds
				   %inverse_jacobi_cd
				   %inverse_jacobi_dc))
		(alike1 (third u) m))
	   (cond ((eq (caar u) '%inverse_jacobi_ns)
		  (second u))
		 (t
		  ;; Express in terms of sn:
		  ;; ns(x) = 1/sn(x)
		  (div 1 (ftake '%jacobi_sn u m)))))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; ns(i*u) = 1/sn(i*u) = -i/sc(u,m1) = -i*cs(u,m1)
	   (neg (mul '$%i
		     (ftake* '%jacobi_cs (coeff u '$%i 1) (add 1 (neg m))))))
	  ((setq coef (kc-arg2 u m))
	   ;; A&S 16.8.10
	   ;;
	   ;; ns(m*K+u) = 1/sn(m*K+u)
	   ;;
	   (destructuring-bind (lin const)
	       coef
	     (cond ((integerp lin)
		    (ecase (mod lin 4)
		      (0
		       ;; ns(4*m*K+u) = ns(u)
		       ;; ns(0) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_ns)
			   (ftake '%jacobi_ns const m)))
		      (1
		       ;; ns(4*m*K + K + u) = ns(K+u) = dc(u)
		       ;; ns(K) = 1
		       (if (zerop1 const)
			   1
			   (ftake '%jacobi_dc const m)))
		      (2
		       ;; ns(4*m*K+2*K + u) = ns(2*K+u) = -ns(u)
		       ;; ns(2*K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_ns)
			   (neg (ftake '%jacobi_ns const m))))
		      (3
		       ;; ns(4*m*K+3*K+u) = ns(2*K + K + u) = -ns(K+u) = -dc(u)
		       ;; ns(3*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg (ftake '%jacobi_dc const m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    (div 1 (ftake '%jacobi_sn u m)))
		   (t
		    (give-up)))))	  
	  (t
	   ;; Nothing to do
	   (give-up)))))

;; jacobi_nc(u,m) = 1/jacobi_cn(u,m)
(defprop %jacobi_nc
    ((u m)
     ;; wrt u
     ((mtimes) ((mexpt) ((%jacobi_cn) u m) -2)
      ((%jacobi_dn) u m) ((%jacobi_sn) u m))
     ;; wrt m
     ((mtimes) -1 ((mexpt) ((%jacobi_cn) u m) -2)
      ((mplus)
       ((mtimes) ((rat) -1 2)
	((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	((%jacobi_cn) u m) ((mexpt) ((%jacobi_sn) u m) 2))
       ((mtimes) ((rat) -1 2) ((mexpt) m -1)
	((%jacobi_dn) u m) ((%jacobi_sn) u m)
	((mplus) u
	 ((mtimes) -1 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	  ((%elliptic_e) ((%asin) ((%jacobi_sn) u m)) m)))))))
  grad)

(def-simplifier jacobi_nc (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
	   (to (bigfloat:/ (bigfloat::cn (bigfloat:to ($float u))
					 (bigfloat:to ($float m))))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat:/ (bigfloat::cn (bigfloat:to ($float u))
				       (bigfloat:to ($float m)))))))
	  ((bigfloat-numerical-eval-p u m)
	   (let ((uu (bigfloat:to ($bfloat u)))
		 (mm (bigfloat:to ($bfloat m))))
	     (to (bigfloat:/ (bigfloat::cn uu mm)))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to ($bfloat u)))
		   (mm (bigfloat:to ($bfloat m))))
	       (to (bigfloat:/ (bigfloat::cn uu mm))))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.8
	   (ftake '%sec u))
	  ((onep1 m)
	   ;; A&S 16.6.8
	   (ftake '%cosh u))
	  ((and $trigsign (mminusp* u))
	   ;; nc is even
	   (ftake* '%jacobi_nc (neg u) m))
	  ((and $triginverses
		(listp u)
		(member (caar u) '(%inverse_jacobi_sn
				   %inverse_jacobi_ns
				   %inverse_jacobi_cn
				   %inverse_jacobi_nc
				   %inverse_jacobi_dn
				   %inverse_jacobi_nd
				   %inverse_jacobi_sc
				   %inverse_jacobi_cs
				   %inverse_jacobi_sd
				   %inverse_jacobi_ds
				   %inverse_jacobi_cd
				   %inverse_jacobi_dc))
		(alike1 (third u) m))
	   (cond ((eq (caar u) '%inverse_jacobi_nc)
		  (second u))
		 (t
		  ;; Express in terms of cn:
		  ;; nc(x) = 1/cn(x)
		  (div 1 (ftake '%jacobi_cn u m)))))
	   ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; nc(i*u) = 1/cn(i*u) = 1/nc(u,1-m) = cn(u,1-m)
	   (ftake* '%jacobi_cn (coeff u '$%i 1) (add 1 (neg m))))
	  ((setq coef (kc-arg2 u m))
	   ;; A&S 16.8.8
	   ;;
	   ;; nc(u) = 1/cn(u)
	   ;;
	   (destructuring-bind (lin const)
	       coef
	     (cond ((integerp lin)
		    (ecase (mod lin 4)
		      (0
		       ;; nc(4*m*K+u) = nc(u)
		       ;; nc(0) = 1
		       (if (zerop1 const)
			   1
			   (ftake '%jacobi_nc const m)))
		      (1
		       ;; nc(4*m*K+K+u) = nc(K+u) = -ds(u)/sqrt(1-m)
		       ;; nc(K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_nc)
			   (neg (div (ftake '%jacobi_ds const m)
				     (power (sub 1 m) 1//2)))))
		      (2
		       ;; nc(4*m*K+2*K+u) = nc(2*K+u) = -nc(u)
		       ;; nc(2*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg (ftake '%jacobi_nc const m))))
		      (3
		       ;; nc(4*m*K+3*K+u) = nc(3*K+u) = nc(2*K+K+u) =
		       ;; -nc(K+u) = ds(u)/sqrt(1-m)
		       ;;
		       ;; nc(3*K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_nc)
			   (div (ftake '%jacobi_ds const m)
				(power (sub 1 m) 1//2))))))
		   ((and (alike1 1//2 lin)
			 (zerop1 const))
		    (div 1 (ftake '%jacobi_cn u m)))
		   (t
		    (give-up)))))
	  (t
	   ;; Nothing to do
	   (give-up)))))

;; jacobi_nd(u,m) = 1/jacobi_dn(u,m)
(defprop %jacobi_nd
    ((u m)
     ;; wrt u
     ((mtimes) m ((%jacobi_cn) u m)
      ((mexpt) ((%jacobi_dn) u m) -2) ((%jacobi_sn) u m))
     ;; wrt m
     ((mtimes) -1 ((mexpt) ((%jacobi_dn) u m) -2)
      ((mplus)
       ((mtimes) ((rat) -1 2)
	((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	((%jacobi_dn) u m)
	((mexpt) ((%jacobi_sn) u m) 2))
       ((mtimes) ((rat) -1 2) ((%jacobi_cn) u m)
	((%jacobi_sn) u m)
	((mplus) u
	 ((mtimes) -1
	  ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	  ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	   m)))))))
  grad)

(def-simplifier jacobi_nd (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
	   (to (bigfloat:/ (bigfloat::dn (bigfloat:to ($float u))
					 (bigfloat:to ($float m))))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat:/ (bigfloat::dn (bigfloat:to ($float u))
				       (bigfloat:to ($float m)))))))
	  ((bigfloat-numerical-eval-p u m)
	   (let ((uu (bigfloat:to ($bfloat u)))
		 (mm (bigfloat:to ($bfloat m))))
	     (to (bigfloat:/ (bigfloat::dn uu mm)))))
	  ((setf args (complex-bigfloat-numerical-eval-p u m))
	   (destructuring-bind (u m)
	       args
	     (let ((uu (bigfloat:to ($bfloat u)))
		   (mm (bigfloat:to ($bfloat m))))
	       (to (bigfloat:/ (bigfloat::dn uu mm))))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.6
	   1)
	  ((onep1 m)
	   ;; A&S 16.6.6
	   (ftake '%cosh u))
	  ((and $trigsign (mminusp* u))
	   ;; nd is even
	   (ftake* '%jacobi_nd (neg u) m))
	  ((and $triginverses
		(listp u)
		(member (caar u) '(%inverse_jacobi_sn
				   %inverse_jacobi_ns
				   %inverse_jacobi_cn
				   %inverse_jacobi_nc
				   %inverse_jacobi_dn
				   %inverse_jacobi_nd
				   %inverse_jacobi_sc
				   %inverse_jacobi_cs
				   %inverse_jacobi_sd
				   %inverse_jacobi_ds
				   %inverse_jacobi_cd
				   %inverse_jacobi_dc))
		(alike1 (third u) m))
	   (cond ((eq (caar u) '%inverse_jacobi_nd)
		  (second u))
		 (t
		  ;; Express in terms of dn:
		  ;; nd(x) = 1/dn(x)
		  (div 1 (ftake '%jacobi_dn u m)))))
	   ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; nd(i*u) = 1/dn(i*u) = 1/dc(u,1-m) = cd(u,1-m)
	   (ftake* '%jacobi_cd (coeff u '$%i 1) (add 1 (neg m))))
	  ((setq coef (kc-arg2 u m))
	   ;; A&S 16.8.6
	   ;;
	   (destructuring-bind (lin const)
	       coef
	     (cond ((integerp lin)
		    ;; nd has period 2K
		    (ecase (mod lin 2)
		      (0
		       ;; nd(2*m*K+u) = nd(u)
		       ;; nd(0) = 1
		       (if (zerop1 const)
			   1
			   (ftake '%jacobi_nd const m)))
		      (1
		       ;; nd(2*m*K+K+u) = nd(K+u) = dn(u)/sqrt(1-m)
		       ;; nd(K) = 1/sqrt(1-m)
		       (if (zerop1 const)
			   (power (sub 1 m) -1//2)
			   (div (ftake '%jacobi_nd const m)
				(power (sub 1 m) 1//2))))))
		   (t
		    (give-up)))))
	  (t
	   ;; Nothing to do
	   (give-up)))))

;; jacobi_sc(u,m) = jacobi_sn/jacobi_cn
(defprop %jacobi_sc
    ((u m)
     ;; wrt u
     ((mtimes) ((mexpt) ((%jacobi_cn) u m) -2)
      ((%jacobi_dn) u m))
     ;; wrt m
     ((mplus)
      ((mtimes) ((mexpt) ((%jacobi_cn) u m) -1)
       ((mplus)
	((mtimes) ((rat) 1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((mexpt) ((%jacobi_cn) u m) 2)
	 ((%jacobi_sn) u m))
	((mtimes) ((rat) 1 2) ((mexpt) m -1)
	 ((%jacobi_cn) u m) ((%jacobi_dn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) -1 ((mexpt) ((%jacobi_cn) u m) -2)
       ((%jacobi_sn) u m)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_cn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((mexpt) m -1)
	 ((%jacobi_dn) u m) ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_sc (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::sn fu fm) (bigfloat::cn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::sn fu fm) (bigfloat::cn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::sn uu mm)
			 (bigfloat::cn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::sn uu mm)
			   (bigfloat::cn uu mm))))))
      ((zerop1 u)
       0)
      ((zerop1 m)
       ;; A&S 16.6.9
       (ftake '%tan u))
      ((onep1 m)
       ;; A&S 16.6.9
       (ftake '%sinh u))
      ((and $trigsign (mminusp* u))
       ;; sc is odd
       (neg (ftake* '%jacobi_sc (neg u) m)))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_sc)
	      (second u))
	     (t
	      ;; Express in terms of sn and cn
	      ;; sc(x) = sn(x)/cn(x)
	      (div (ftake '%jacobi_sn u m)
		   (ftake '%jacobi_cn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; sc(i*u) = sn(i*u)/cn(i*u) = i*sc(u,m1)/nc(u,m1) = i*sn(u,m1)
       (mul '$%i
	    (ftake* '%jacobi_sn (coeff u '$%i 1) (add 1 (neg m)))))
      ((setq coef (kc-arg2 u m))
       ;; A&S 16.8.9
       ;; sc(2*m*K+u) = sc(u)
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 2)
		  (0
		   ;; sc(2*m*K+ u) = sc(u)
		   ;; sc(0) = 0
		   (if (zerop1 const)
		       1
		       (ftake '%jacobi_sc const m)))
		  (1
		   ;; sc(2*m*K + K + u) = sc(K+u)= - cs(u)/sqrt(1-m)
		   ;; sc(K) = infinity
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_sc)
		       (mul -1
			    (div (ftake* '%jacobi_cs const m)
				 (power (sub 1 m) 1//2)))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; From A&S 16.3.3 and 16.5.2:
		;; sc(1/2*K) = 1/(1-m)^(1/4)
		(power (sub 1 m) (div -1 4)))
	       (t
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

;; jacobi_sd(u,m) = jacobi_sn/jacobi_dn
(defprop %jacobi_sd
    ((u m)
     ;; wrt u
     ((mtimes) ((%jacobi_cn) u m)
      ((mexpt) ((%jacobi_dn) u m) -2))
     ;; wrt m
     ((mplus)
      ((mtimes) ((mexpt) ((%jacobi_dn) u m) -1)
       ((mplus)
	((mtimes) ((rat) 1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((mexpt) ((%jacobi_cn) u m) 2)
	 ((%jacobi_sn) u m))
	((mtimes) ((rat) 1 2) ((mexpt) m -1)
	 ((%jacobi_cn) u m) ((%jacobi_dn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) -1 ((mexpt) ((%jacobi_dn) u m) -2)
       ((%jacobi_sn) u m)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_dn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((%jacobi_cn) u m)
	 ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_sd (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::sn fu fm) (bigfloat::dn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::sn fu fm) (bigfloat::dn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::sn uu mm)
			 (bigfloat::dn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::sn uu mm)
			   (bigfloat::dn uu mm))))))
      ((zerop1 u)
       0)
      ((zerop1 m)
       ;; A&S 16.6.5
       (ftake '%sin u))
      ((onep1 m)
       ;; A&S 16.6.5
       (ftake '%sinh u))
      ((and $trigsign (mminusp* u))
       ;; sd is odd
       (neg (ftake* '%jacobi_sd (neg u) m)))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_sd)
	      (second u))
	     (t
	      ;; Express in terms of sn and dn
	      (div (ftake '%jacobi_sn u m)
		   (ftake '%jacobi_dn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; sd(i*u) = sn(i*u)/dn(i*u) = i*sc(u,m1)/dc(u,m1) = i*sd(u,m1)
       (mul '$%i
	    (ftake* '%jacobi_sd (coeff u '$%i 1) (add 1 (neg m)))))
      ((setq coef (kc-arg2 u m))
       ;; A&S 16.8.5
       ;; sd(4*m*K+u) = sd(u)
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; sd(4*m*K+u) = sd(u)
		   ;; sd(0) = 0
		   (if (zerop1 const)
		       0
		       (ftake '%jacobi_sd const m)))
		  (1
		   ;; sd(4*m*K+K+u) = sd(K+u) = cn(u)/sqrt(1-m)
		   ;; sd(K) = 1/sqrt(m1)
		   (if (zerop1 const)
		       (power (sub 1 m) 1//2)
		       (div (ftake '%jacobi_cn const m)
			    (power (sub 1 m) 1//2))))
		  (2
		   ;; sd(4*m*K+2*K+u) = sd(2*K+u) = -sd(u)
		   ;; sd(2*K) = 0
		   (if (zerop1 const)
		       0
		       (neg (ftake '%jacobi_sd const m))))
		  (3
		   ;; sd(4*m*K+3*K+u) = sd(3*K+u) = sd(2*K+K+u) =
		   ;; -sd(K+u) = -cn(u)/sqrt(1-m)
		   ;; sd(3*K) = -1/sqrt(m1)
		   (if (zerop1 const)
		       (neg (power (sub 1 m) -1//2))
		       (neg (div (ftake '%jacobi_cn const m)
				 (power (sub 1 m) 1//2)))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; jacobi_sn/jacobi_dn
		(div (ftake '%jacobi_sn
			    (mul 1//2
				 (ftake '%elliptic_kc m))
			    m)
		     (ftake '%jacobi_dn
			    (mul 1//2
				 (ftake '%elliptic_kc m))
			    m)))
	       (t
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

;; jacobi_cs(u,m) = jacobi_cn/jacobi_sn
(defprop %jacobi_cs
    ((u m)
     ;; wrt u
     ((mtimes) -1 ((%jacobi_dn) u m)
      ((mexpt) ((%jacobi_sn) u m) -2))
     ;; wrt m
     ((mplus)
      ((mtimes) -1 ((%jacobi_cn) u m)
       ((mexpt) ((%jacobi_sn) u m) -2)
       ((mplus)
	((mtimes) ((rat) 1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((mexpt) ((%jacobi_cn) u m) 2)
	 ((%jacobi_sn) u m))
	((mtimes) ((rat) 1 2) ((mexpt) m -1)
	 ((%jacobi_cn) u m) ((%jacobi_dn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) ((mexpt) ((%jacobi_sn) u m) -1)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_cn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((mexpt) m -1)
	 ((%jacobi_dn) u m) ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_cs (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::cn fu fm) (bigfloat::sn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::cn fu fm) (bigfloat::sn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::cn uu mm)
			 (bigfloat::sn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::cn uu mm)
			   (bigfloat::sn uu mm))))))
      ((zerop1 m)
       ;; A&S 16.6.12
       (ftake '%cot u))
      ((onep1 m)
       ;; A&S 16.6.12
       (ftake '%csch u))
      ((zerop1 u)
       (dbz-err1 'jacobi_cs))
      ((and $trigsign (mminusp* u))
       ;; cs is odd
       (neg (ftake* '%jacobi_cs (neg u) m)))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_cs)
	      (second u))
	     (t
	      ;; Express in terms of cn an sn
	      (div (ftake '%jacobi_cn u m)
		   (ftake '%jacobi_sn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; cs(i*u) = cn(i*u)/sn(i*u) = -i*nc(u,m1)/sc(u,m1) = -i*ns(u,m1)
       (neg (mul '$%i
		 (ftake* '%jacobi_ns (coeff u '$%i 1) (add 1 (neg m))))))
      ((setq coef (kc-arg2 u m))
       ;; A&S 16.8.12
       ;; 
       ;; cs(2*m*K + u) = cs(u)
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 2)
		  (0
		   ;; cs(2*m*K + u) = cs(u)
		   ;; cs(0) = infinity
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_cs)
		       (ftake '%jacobi_cs const m)))
		  (1
		   ;; cs(K+u) = -sqrt(1-m)*sc(u)
		   ;; cs(K) = 0
		   (if (zerop1 const)
		       0
		       (neg (mul (power (sub 1 m) 1//2)
				 (ftake '%jacobi_sc const m)))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; 1/jacobi_sc
		(div 1
		     (ftake '%jacobi_sc (mul 1//2
					    (ftake '%elliptic_kc m))
			    m)))
	       (t
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

;; jacobi_cd(u,m) = jacobi_cn/jacobi_dn
(defprop %jacobi_cd
    ((u m)
     ;; wrt u
     ((mtimes) ((mplus) -1 m)
      ((mexpt) ((%jacobi_dn) u m) -2)
      ((%jacobi_sn) u m))
     ;; wrt m
     ((mplus)
      ((mtimes) -1 ((%jacobi_cn) u m)
       ((mexpt) ((%jacobi_dn) u m) -2)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_dn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((%jacobi_cn) u m)
	 ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) ((mexpt) ((%jacobi_dn) u m) -1)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_cn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((mexpt) m -1)
	 ((%jacobi_dn) u m) ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_cd (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::cn fu fm) (bigfloat::dn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::cn fu fm) (bigfloat::dn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::cn uu mm) (bigfloat::dn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::cn uu mm) (bigfloat::dn uu mm))))))
      ((zerop1 u)
       1)
      ((zerop1 m)
       ;; A&S 16.6.4
       (ftake '%cos u))
      ((onep1 m)
       ;; A&S 16.6.4
       1)
      ((and $trigsign (mminusp* u))
       ;; cd is even
       (ftake* '%jacobi_cd (neg u) m))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_cd)
	      (second u))
	     (t
	      ;; Express in terms of cn and dn
	      (div (ftake '%jacobi_cn u m)
		   (ftake '%jacobi_dn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; cd(i*u) = cn(i*u)/dn(i*u) = nc(u,m1)/dc(u,m1) = nd(u,m1)
       (ftake* '%jacobi_nd (coeff u '$%i 1) (add 1 (neg m))))
      ((setf coef (kc-arg2 u m))
       ;; A&S 16.8.4
       ;;
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; cd(4*m*K + u) = cd(u)
		   ;; cd(0) = 1
		   (if (zerop1 const)
		       1
		       (ftake '%jacobi_cd const m)))
		  (1
		   ;; cd(4*m*K + K + u) = cd(K+u) = -sn(u)
		   ;; cd(K) = 0
		   (if (zerop1 const)
		       0
		       (neg (ftake '%jacobi_sn const m))))
		  (2
		   ;; cd(4*m*K + 2*K + u) = cd(2*K+u) = -cd(u)
		   ;; cd(2*K) = -1
		   (if (zerop1 const)
		       -1
		       (neg (ftake '%jacobi_cd const m))))
		  (3
		   ;; cd(4*m*K + 3*K + u) = cd(2*K + K + u) =
		   ;; -cd(K+u) = sn(u)
		   ;; cd(3*K) = 0
		   (if (zerop1 const)
		       0
		       (ftake '%jacobi_sn const m)))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; jacobi_cn/jacobi_dn
		(div (ftake '%jacobi_cn
			    (mul 1//2
				 (ftake '%elliptic_kc m))
			    m)
		     (ftake '%jacobi_dn
			    (mul 1//2
				 (ftake '%elliptic_kc m))
			    m)))
	       (t
		;; Nothing to do
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))  

;; jacobi_ds(u,m) = jacobi_dn/jacobi_sn
(defprop %jacobi_ds
    ((u m)
     ;; wrt u
     ((mtimes) -1 ((%jacobi_cn) u m)
      ((mexpt) ((%jacobi_sn) u m) -2))
     ;; wrt m
     ((mplus)
      ((mtimes) -1 ((%jacobi_dn) u m)
       ((mexpt) ((%jacobi_sn) u m) -2)
       ((mplus)
	((mtimes) ((rat) 1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((mexpt) ((%jacobi_cn) u m) 2)
	 ((%jacobi_sn) u m))
	((mtimes) ((rat) 1 2) ((mexpt) m -1)
	 ((%jacobi_cn) u m) ((%jacobi_dn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) ((mexpt) ((%jacobi_sn) u m) -1)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_dn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((%jacobi_cn) u m)
	 ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_ds (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::dn fu fm) (bigfloat::sn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::dn fu fm) (bigfloat::sn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::dn uu mm)
			 (bigfloat::sn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::dn uu mm)
			   (bigfloat::sn uu mm))))))
      ((zerop1 m)
       ;; A&S 16.6.11
       (ftake '%csc u))
      ((onep1 m)
       ;; A&S 16.6.11
       (ftake '%csch u))
      ((zerop1 u)
       (dbz-err1 'jacobi_ds))
      ((and $trigsign (mminusp* u))
       (neg (ftake* '%jacobi_ds (neg u) m)))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_ds)
	      (second u))
	     (t
	      ;; Express in terms of dn and sn
	      (div (ftake '%jacobi_dn u m)
		   (ftake '%jacobi_sn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; ds(i*u) = dn(i*u)/sn(i*u) = -i*dc(u,m1)/sc(u,m1) = -i*ds(u,m1)
       (neg (mul '$%i
		 (ftake* '%jacobi_ds (coeff u '$%i 1) (add 1 (neg m))))))
      ((setf coef (kc-arg2 u m))
       ;; A&S 16.8.11
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; ds(4*m*K + u) = ds(u)
		   ;; ds(0) = infinity
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_ds)
		       (ftake '%jacobi_ds const m)))
		  (1
		   ;; ds(4*m*K + K + u) = ds(K+u) = sqrt(1-m)*nc(u)
		   ;; ds(K) = sqrt(1-m)
		   (if (zerop1 const)
		       (power (sub 1 m) 1//2)
		       (mul (power (sub 1 m) 1//2)
			    (ftake '%jacobi_nc const m))))
		  (2
		   ;; ds(4*m*K + 2*K + u) = ds(2*K+u) = -ds(u)
		   ;; ds(0) = pole
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_ds)
		       (neg (ftake '%jacobi_ds const m))))
		  (3
		   ;; ds(4*m*K + 3*K + u) = ds(2*K + K + u) =
		   ;; -ds(K+u) = -sqrt(1-m)*nc(u)
		   ;; ds(3*K) = -sqrt(1-m)
		   (if (zerop1 const)
		       (neg (power (sub 1 m) 1//2))
		       (neg (mul (power (sub 1 m) 1//2)
				 (ftake '%jacobi_nc u m)))))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; jacobi_dn/jacobi_sn
		(div
		 (ftake '%jacobi_dn
			(mul 1//2 (ftake '%elliptic_kc m))
			m)
		 (ftake '%jacobi_sn
			(mul 1//2 (ftake '%elliptic_kc m))
			m)))
	       (t
		;; Nothing to do
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

;; jacobi_dc(u,m) = jacobi_dn/jacobi_cn
(defprop %jacobi_dc
    ((u m)
     ;; wrt u
     ((mtimes) ((mplus) 1 ((mtimes) -1 m))
      ((mexpt) ((%jacobi_cn) u m) -2)
      ((%jacobi_sn) u m))
     ;; wrt m
     ((mplus)
      ((mtimes) ((mexpt) ((%jacobi_cn) u m) -1)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_dn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((%jacobi_cn) u m)
	 ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))
      ((mtimes) -1 ((mexpt) ((%jacobi_cn) u m) -2)
       ((%jacobi_dn) u m)
       ((mplus)
	((mtimes) ((rat) -1 2)
	 ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	 ((%jacobi_cn) u m)
	 ((mexpt) ((%jacobi_sn) u m) 2))
	((mtimes) ((rat) -1 2) ((mexpt) m -1)
	 ((%jacobi_dn) u m) ((%jacobi_sn) u m)
	 ((mplus) u
	  ((mtimes) -1
	   ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
	   ((%elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(def-simplifier jacobi_dc (u m)
  (let (coef args)
    (cond
      ((float-numerical-eval-p u m)
       (let ((fu (bigfloat:to ($float u)))
	     (fm (bigfloat:to ($float m))))
	 (to (bigfloat:/ (bigfloat::dn fu fm) (bigfloat::cn fu fm)))))
      ((setf args (complex-float-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((fu (bigfloat:to ($float u)))
	       (fm (bigfloat:to ($float m))))
	   (to (bigfloat:/ (bigfloat::dn fu fm) (bigfloat::cn fu fm))))))
      ((bigfloat-numerical-eval-p u m)
       (let ((uu (bigfloat:to ($bfloat u)))
	     (mm (bigfloat:to ($bfloat m))))
	 (to (bigfloat:/ (bigfloat::dn uu mm)
			 (bigfloat::cn uu mm)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (let ((uu (bigfloat:to ($bfloat u)))
	       (mm (bigfloat:to ($bfloat m))))
	   (to (bigfloat:/ (bigfloat::dn uu mm)
			   (bigfloat::cn uu mm))))))
      ((zerop1 u)
       1)
      ((zerop1 m)
       ;; A&S 16.6.7
       (ftake '%sec u))
      ((onep1 m)
       ;; A&S 16.6.7
       1)
      ((and $trigsign (mminusp* u))
       (ftake* '%jacobi_dc (neg u) m))
      ((and $triginverses
	    (listp u)
	    (member (caar u) '(%inverse_jacobi_sn
			       %inverse_jacobi_ns
			       %inverse_jacobi_cn
			       %inverse_jacobi_nc
			       %inverse_jacobi_dn
			       %inverse_jacobi_nd
			       %inverse_jacobi_sc
			       %inverse_jacobi_cs
			       %inverse_jacobi_sd
			       %inverse_jacobi_ds
			       %inverse_jacobi_cd
			       %inverse_jacobi_dc))
	    (alike1 (third u) m))
       (cond ((eq (caar u) '%inverse_jacobi_dc)
	      (second u))
	     (t
	      ;; Express in terms of dn and cn
	      (div (ftake '%jacobi_dn u m)
		   (ftake '%jacobi_cn u m)))))
      ;; A&S 16.20 (Jacobi's Imaginary transformation)
      ((and $%iargs (multiplep u '$%i))
       ;; dc(i*u) = dn(i*u)/cn(i*u) = dc(u,m1)/nc(u,m1) = dn(u,m1)
       (ftake* '%jacobi_dn (coeff u '$%i 1) (add 1 (neg m))))
      ((setf coef (kc-arg2 u m))
       ;; See A&S 16.8.7
       (destructuring-bind (lin const)
	   coef
	 (cond ((integerp lin)
		(ecase (mod lin 4)
		  (0
		   ;; dc(4*m*K + u) = dc(u)
		   ;; dc(0) = 1
		   (if (zerop1 const)
		       1
		       (ftake '%jacobi_dc const m)))
		  (1
		   ;; dc(4*m*K + K + u) = dc(K+u) = -ns(u)
		   ;; dc(K) = pole
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_dc)
		       (neg (ftake '%jacobi_ns const m))))
		  (2
		   ;; dc(4*m*K + 2*K + u) = dc(2*K+u) = -dc(u)
		   ;; dc(2K) = -1
		   (if (zerop1 const)
		       -1
		       (neg (ftake '%jacobi_dc const m))))
		  (3
		   ;; dc(4*m*K + 3*K + u) = dc(2*K + K + u) =
		   ;; -dc(K+u) = ns(u)
		   ;; dc(3*K) = ns(0) = inf
		   (if (zerop1 const)
		       (dbz-err1 'jacobi_dc)
		       (ftake '%jacobi_dc const m)))))
	       ((and (alike1 lin 1//2)
		     (zerop1 const))
		;; jacobi_dn/jacobi_cn
		(div
		 (ftake '%jacobi_dn
			(mul 1//2 (ftake '%elliptic_kc m))
			m)
		 (ftake '%jacobi_cn
			(mul 1//2 (ftake '%elliptic_kc m))
			m)))
	       (t
		;; Nothing to do
		(give-up)))))
      (t
       ;; Nothing to do
       (give-up)))))

;;; Other inverse Jacobian functions

;; inverse_jacobi_ns(x)
;;
;; Let u = inverse_jacobi_ns(x).  Then jacobi_ns(u) = x or
;; 1/jacobi_sn(u) = x or
;;
;; jacobi_sn(u) = 1/x
;;
;; so u = inverse_jacobi_sn(1/x)
(defprop %inverse_jacobi_ns
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_ns(u,m) = integrate(1/sqrt(t^2-1)/sqrt(t^2-m), t, u, inf)
     ;; -> -1/sqrt(x^2-1)/sqrt(x^2-m)
     ((mtimes) -1
      ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) -1 2))
      ((mexpt)
       ((mplus) ((mtimes simp ratsimp) -1 m) ((mexpt) x 2))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_ns) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_ns (u m)
  (let (args)
    (cond
      ((float-numerical-eval-p u m)
       ;; Numerically evaluate asn
       ;;
       ;; ans(x,m) = asn(1/x,m) = F(asin(1/x),m)
       (to (elliptic-f (cl:asin (/ ($float u))) ($float m))))
      ((complex-float-numerical-eval-p u m)
       (to (elliptic-f (cl:asin (/ (complex ($realpart ($float u)) ($imagpart ($float u)))))
		       (complex ($realpart ($float m)) ($imagpart ($float m))))))
      ((bigfloat-numerical-eval-p u m)
       (to (bigfloat::bf-elliptic-f (bigfloat:asin (bigfloat:/ (bigfloat:to ($bfloat u))))
				    (bigfloat:to ($bfloat m)))))
      ((setf args (complex-bigfloat-numerical-eval-p u m))
       (destructuring-bind (u m)
	   args
	 (to (bigfloat::bf-elliptic-f (bigfloat:asin (bigfloat:/ (bigfloat:to ($bfloat u))))
				      (bigfloat:to ($bfloat m))))))
      ((zerop1 m)
       ;; ans(x,0) = F(asin(1/x),0) = asin(1/x)
       (ftake '%elliptic_f (ftake '%asin (div 1 u)) 0))
      ((onep1 m)
       ;; ans(x,1) = F(asin(1/x),1) = log(tan(pi/2+asin(1/x)/2))
       (ftake '%elliptic_f (ftake '%asin (div 1 u)) 1))
      ((onep1 u)
       (ftake '%elliptic_kc m))
      ((alike1 u -1)
       (neg (ftake '%elliptic_kc m)))
      ((and (eq $triginverses '$all)
	    (listp u)
	    (eq (caar u) '%jacobi_ns)
	    (alike1 (third u) m))
       ;; inverse_jacobi_ns(ns(u)) = u
       (second u))
      (t
       ;; Nothing to do
       (give-up)))))

;; inverse_jacobi_nc(x)
;;
;; Let u = inverse_jacobi_nc(x).  Then jacobi_nc(u) = x or
;; 1/jacobi_cn(u) = x or
;;
;; jacobi_cn(u) = 1/x
;;
;; so u = inverse_jacobi_cn(1/x)
(defprop %inverse_jacobi_nc
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_nc(u,m) = integrate(1/sqrt(t^2-1)/sqrt((1-m)*t^2+m), t, 1, u)
     ;; -> 1/sqrt(x^2-1)/sqrt((1-m)*x^2+m)
     ((mtimes)
      ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) -1 2))
      ((mexpt)
       ((mplus) m
	((mtimes) -1 ((mplus) -1 m) ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_nc) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_nc (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 ;;
	 (ftake '%inverse_jacobi_cn ($rectform (div 1 u)) m))
	((onep1 u)
	 0)
	((alike1 u -1)
	 (mul 2 (ftake '%elliptic_kc m)))
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_nc)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_nc(nc(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_nd(x)
;;
;; Let u = inverse_jacobi_nd(x).  Then jacobi_nd(u) = x or
;; 1/jacobi_dn(u) = x or
;;
;; jacobi_dn(u) = 1/x
;;
;; so u = inverse_jacobi_dn(1/x)
(defprop %inverse_jacobi_nd
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_nd(u,m) = integrate(1/sqrt(t^2-1)/sqrt(1-(1-m)*t^2), t, 1, u)
     ;; -> 1/sqrt(u^2-1)/sqrt(1-(1-m)*t^2)
     ((mtimes) 
      ((mexpt) ((mplus) -1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1
	((mtimes) ((mplus) -1 m) ((mexpt simp ratsimp) x 2)))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_nd) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_nd (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_dn ($rectform (div 1 u)) m))
	((onep1 u)
	 0)
	((onep1 ($ratsimp (mul (power (sub 1 m) 1//2) u)))
	 ;; jacobi_nd(1/sqrt(1-m),m) = K(m).  This follows from
	 ;; jacobi_dn(sqrt(1-m),m) = K(m).
	 (ftake '%elliptic_kc m))
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_nd)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_nd(nd(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_sc(x)
;;
;; Let u = inverse_jacobi_sc(x).  Then jacobi_sc(u) = x or
;; x = jacobi_sn(u)/jacobi_cn(u)
;;
;; x^2 = sn^2/cn^2
;;     = sn^2/(1-sn^2)
;;
;; so
;;
;; sn^2 = x^2/(1+x^2)
;;
;; sn(u) = x/sqrt(1+x^2)
;;
;; u = inverse_sn(x/sqrt(1+x^2))
;;
(defprop %inverse_jacobi_sc
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_sc(u,m) = integrate(1/sqrt(1+t^2)/sqrt(1+(1-m)*t^2), t, 0, u)
     ;; -> 1/sqrt(1+x^2)/sqrt(1+(1-m)*x^2)
     ((mtimes)
      ((mexpt) ((mplus) 1 ((mexpt) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1
	((mtimes) -1 ((mplus) -1 m) ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_sc) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_sc (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_sn
		($rectform (div u (power (add 1 (mul u u)) 1//2)))
		m))
	((zerop1 u)
	 ;; jacobi_sc(0,m) = 0
	 0)
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_sc)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_sc(sc(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_sd(x)
;;
;; Let u = inverse_jacobi_sd(x).  Then jacobi_sd(u) = x or
;; x = jacobi_sn(u)/jacobi_dn(u)
;;
;; x^2 = sn^2/dn^2
;;     = sn^2/(1-m*sn^2)
;;
;; so
;;
;; sn^2 = x^2/(1+m*x^2)
;;
;; sn(u) = x/sqrt(1+m*x^2)
;;
;; u = inverse_sn(x/sqrt(1+m*x^2))
;;
(defprop %inverse_jacobi_sd
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_sd(u,m) = integrate(1/sqrt(1-(1-m)*t^2)/sqrt(1+m*t^2), t, 0, u)
     ;; -> 1/sqrt(1-(1-m)*x^2)/sqrt(1+m*x^2)
     ((mtimes)
      ((mexpt)
       ((mplus) 1 ((mtimes) ((mplus) -1 m) ((mexpt) x 2)))
       ((rat) -1 2))
      ((mexpt) ((mplus) 1 ((mtimes) m ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_sd) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_sd (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_sn
		($rectform (div u (power (add 1 (mul m (mul u u))) 1//2)))
		m))
	((zerop1 u)
	 0)
	((eql 0 ($ratsimp (sub u (div 1 (power (sub 1 m) 1//2)))))
	 ;; inverse_jacobi_sd(1/sqrt(1-m), m) = elliptic_kc(m)
	 ;;
	 ;; We can see this from inverse_jacobi_sd(x,m) =
	 ;; inverse_jacobi_sn(x/sqrt(1+m*x^2), m).  So
	 ;; inverse_jacobi_sd(1/sqrt(1-m),m) = inverse_jacobi_sn(1,m)
	 (ftake '%elliptic_kc m))
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_sd)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_sd(sd(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_cs(x)
;;
;; Let u = inverse_jacobi_cs(x).  Then jacobi_cs(u) = x or
;; 1/x = 1/jacobi_cs(u) = jacobi_sc(u)
;;
;; u = inverse_sc(1/x)
;;
(defprop %inverse_jacobi_cs
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_cs(u,m) = integrate(1/sqrt(t^2+1)/sqrt(t^2+(1-m)), t, u, inf)
     ;; -> -1/sqrt(x^2+1)/sqrt(x^2+(1-m))
     ((mtimes) -1
      ((mexpt) ((mplus) 1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt) ((mplus) 1
		((mtimes simp ratsimp) -1 m)
		((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_cs) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_cs (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_sc ($rectform (div 1 u)) m))
	((zerop1 u)
	 (ftake '%elliptic_kc m))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_cd(x)
;;
;; Let u = inverse_jacobi_cd(x).  Then jacobi_cd(u) = x or
;; x = jacobi_cn(u)/jacobi_dn(u)
;;
;; x^2 = cn^2/dn^2
;;     = (1-sn^2)/(1-m*sn^2)
;;
;; or
;;
;; sn^2 = (1-x^2)/(1-m*x^2)
;;
;; sn(u) = sqrt(1-x^2)/sqrt(1-m*x^2)
;;
;; u = inverse_sn(sqrt(1-x^2)/sqrt(1-m*x^2))
;;
(defprop %inverse_jacobi_cd
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_cd(u,m) = integrate(1/sqrt(1-t^2)/sqrt(1-m*t^2), t, u, 1)
     ;; -> -1/sqrt(1-x^2)/sqrt(1-m*x^2)
     ((mtimes) -1
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 ((mexpt) x 2)))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 m ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_cd) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_cd (u m)
  (cond ((or (complex-float-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (let (($numer t))
	   (ftake '%inverse_jacobi_sn
		  ($rectform (div (power (mul (sub 1 u) (add 1 u)) 1//2)
				  (power (sub 1 (mul m (mul u u))) 1//2)))
		  m)))
	((onep1 u)
	 0)
	((zerop1 u)
	 (ftake '%elliptic_kc m))
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_cd)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_cd(cd(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; inverse_jacobi_ds(x)
;;
;; Let u = inverse_jacobi_ds(x).  Then jacobi_ds(u) = x or
;; 1/x = 1/jacobi_ds(u) = jacobi_sd(u)
;;
;; u = inverse_sd(1/x)
;;
(defprop %inverse_jacobi_ds
    ((x m)
     ;; Whittaker and Watson, example in 22.122
     ;; inverse_jacobi_ds(u,m) = integrate(1/sqrt(t^2-(1-m))/sqrt(t^2+m), t, u, inf)
     ;; -> -1/sqrt(x^2-(1-m))/sqrt(x^2+m)
     ((mtimes) -1
      ((mexpt)
       ((mplus) -1 m ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) m ((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_ds) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_ds (u m)
  (cond ((or (float-numerical-eval-p u m)
	     (complex-float-numerical-eval-p u m)
	     (bigfloat-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_sd ($rectform (div 1 u)) m))
	((and $trigsign (mminusp* u))
	 (neg (ftake* '%inverse_jacobi_ds (neg u) m)))
	((eql 0 ($ratsimp (sub u (power (sub 1 m) 1//2))))
	 ;; inverse_jacobi_ds(sqrt(1-m),m) = elliptic_kc(m)
	 ;;
	 ;; Since inverse_jacobi_ds(sqrt(1-m), m) =
	 ;; inverse_jacobi_sd(1/sqrt(1-m),m).  And we know from
	 ;; above that this is elliptic_kc(m)
	 (ftake '%elliptic_kc m))
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_ds)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_ds(ds(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))


;; inverse_jacobi_dc(x)
;;
;; Let u = inverse_jacobi_dc(x).  Then jacobi_dc(u) = x or
;; 1/x = 1/jacobi_dc(u) = jacobi_cd(u)
;;
;; u = inverse_cd(1/x)
;;
(defprop %inverse_jacobi_dc
    ((x m)
     ;; Note: Whittaker and Watson, example in 22.122 says
     ;; inverse_jacobi_dc(u,m) = integrate(1/sqrt(t^2-1)/sqrt(t^2-m),
     ;; t, u, 1) but that seems wrong.  A&S 17.4.47 says
     ;; integrate(1/sqrt(t^2-1)/sqrt(t^2-m), t, a, u) =
     ;; inverse_jacobi_cd(x,m).  Lawden 3.2.8 says the same.
     ;; functions.wolfram.com says the derivative is
     ;; 1/sqrt(t^2-1)/sqrt(t^2-m).
     ((mtimes)
      ((mexpt)
       ((mplus) -1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus)
	((mtimes simp ratsimp) -1 m)
	((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
     ;; wrt m
;     ((%derivative) ((%inverse_jacobi_dc) x m) m 1)
     nil)
  grad)

(def-simplifier inverse_jacobi_dc (u m)
  (cond ((or (complex-float-numerical-eval-p u m)
	     (complex-bigfloat-numerical-eval-p u m))
	 (ftake '%inverse_jacobi_cd ($rectform (div 1 u)) m))
	((onep1 u)
	 0)
	((and (eq $triginverses '$all)
	      (listp u)
	      (eq (caar u) '%jacobi_dc)
	      (alike1 (third u) m))
	 ;; inverse_jacobi_dc(dc(u)) = u
	 (second u))
	(t
	 ;; Nothing to do
	 (give-up))))

;; Convert an inverse Jacobian function into the equivalent elliptic
;; integral F.
;;
;; See A&S 17.4.41-17.4.52.
(defun make-elliptic-f (e)
  (cond ((atom e)
	 e)
	((member (caar e) '(%inverse_jacobi_sc %inverse_jacobi_cs
			    %inverse_jacobi_nd %inverse_jacobi_dn
			    %inverse_jacobi_sn %inverse_jacobi_cd
			    %inverse_jacobi_dc %inverse_jacobi_ns
			    %inverse_jacobi_nc %inverse_jacobi_ds
			    %inverse_jacobi_sd %inverse_jacobi_cn))
	 ;; We have some inverse Jacobi function.  Convert it to the F form.
	 (destructuring-bind ((fn &rest ops) u m)
	     e
	   (declare (ignore ops))
	   (ecase fn
	     (%inverse_jacobi_sc
	      ;; A&S 17.4.41
	      (ftake '%elliptic_f (ftake '%atan u) m))
	     (%inverse_jacobi_cs
	      ;; A&S 17.4.42
	      (ftake '%elliptic_f (ftake '%atan (div 1 u)) m))
	     (%inverse_jacobi_nd
	      ;; A&S 17.4.43
	      (ftake '%elliptic_f
		     (ftake '%asin
			    (mul (power m -1//2)
				 (div 1 u)
				 (power (add -1 (mul u u))
					1//2)))
		     m))
	     (%inverse_jacobi_dn
	      ;; A&S 17.4.44
	      (ftake '%elliptic_f
		     (ftake '%asin (mul
				   (power m  -1//2)
				   (power (sub 1 (power u 2)) 1//2)))
		     m))
	     (%inverse_jacobi_sn
	      ;; A&S 17.4.45
	      (ftake '%elliptic_f (ftake '%asin u) m))
	     (%inverse_jacobi_cd
	      ;; A&S 17.4.46
	      (ftake '%elliptic_f
		     (ftake '%asin
			    (power (mul (sub 1 (mul u u))
					(sub 1 (mul m u u)))
				   1//2))
		     m))
	     (%inverse_jacobi_dc
	      ;; A&S 17.4.47
	      (ftake '%elliptic_f
		     (ftake '%asin
			    (power (mul (sub (mul u u) 1)
					(sub (mul u u) m))
				   1//2))
		     m))
	     (%inverse_jacobi_ns
	      ;; A&S 17.4.48
	      (ftake '%elliptic_f (ftake '%asin (div 1 u)) m))
	     (%inverse_jacobi_nc
	      ;; A&S 17.4.49
	      (ftake '%elliptic_f (ftake '%acos (div 1 u)) m))
	     (%inverse_jacobi_ds
	      ;; A&S 17.4.50
	      (ftake '%elliptic_f
		     (ftake '%asin
			    (power (add m (mul u u))
				   1//2))
		     m))
	     (%inverse_jacobi_sd
	      ;; A&S 17.4.51
	      (ftake '%elliptic_f
		     (ftake '%asin
			    (div u
				 (power (add 1 (mul m u u))
					1//2)))
		     m))
	     (%inverse_jacobi_cn
	      ;; A&S 17.4.52
	      (ftake '%elliptic_f (ftake '%acos u) m)))))
	(t
	 (recur-apply #'make-elliptic-f e))))

(defmfun $make_elliptic_f (e)
  (if (atom e)
      e
      (simplify (make-elliptic-f e))))

(defun make-elliptic-e (e)
  (cond ((atom e) e)
	((eq (caar e) '$elliptic_eu)
	 (destructuring-bind ((ffun &rest ops) u m) e
	   (declare (ignore ffun ops))
	   (ftake '%elliptic_e (ftake '%asin (ftake '%jacobi_sn u m)) m)))
	(t
	 (recur-apply #'make-elliptic-e e))))

(defmfun $make_elliptic_e (e)
  (if (atom e)
      e
      (simplify (make-elliptic-e e))))
  
	 
;; Eu(u,m) = integrate(jacobi_dn(v,m)^2,v,0,u)
;;         = integrate(sqrt((1-m*t^2)/(1-t^2)),t,0,jacobi_sn(u,m))
;;
;; Eu(u,m) = E(am(u),m)
;;
;; where E(u,m) is elliptic-e above.
;;
;; Checks.
;; Lawden gives the following relationships
;;
;; E(u+v) = E(u) + E(v) - m*sn(u)*sn(v)*sn(u+v)
;; E(u,0) = u, E(u,1) = tanh u
;;
;; E(i*u,k) = i*sc(u,k')*dn(u,k') - i*E(u,k') + i*u
;;
;; E(2*i*K') = 2*i*(K'-E')
;;
;; E(u + 2*i*K') = E(u) + 2*i*(K' - E')
;;
;; E(u+K) = E(u) + E - k^2*sn(u)*cd(u)
(defun elliptic-eu (u m)
  (cond ((realp u)
	 ;; E(u + 2*n*K) = E(u) + 2*n*E
	 (let ((ell-k (to (elliptic-k m)))
	       (ell-e (elliptic-ec m)))
	   (multiple-value-bind (n u-rem)
	       (floor u (* 2 ell-k))
	     ;; 0 <= u-rem < 2*K
	     (+ (* 2 n ell-e)
		(cond ((>= u-rem ell-k)
		       ;; 0 <= u-rem < K so
		       ;; E(u + K) = E(u) + E - m*sn(u)*cd(u)
		       (let ((u-k (- u ell-k)))
			 (- (+ (elliptic-e (cl:asin (bigfloat::sn u-k m)) m)
			       ell-e)
			    (/ (* m (bigfloat::sn u-k m) (bigfloat::cn u-k m))
			       (bigfloat::dn u-k m)))))
		      (t
		       (elliptic-e (cl:asin (bigfloat::sn u m)) m)))))))
	((complexp u)
	 ;; From Lawden:
	 ;;
	 ;; E(u+i*v, m) = E(u,m) -i*E(v,m') + i*v + i*sc(v,m')*dn(v,m')
	 ;;                -i*m*sn(u,m)*sc(v,m')*sn(u+i*v,m)
	 ;;
	 (let ((u-r (realpart u))
	       (u-i (imagpart u))
	       (m1 (- 1 m)))
	   (+ (elliptic-eu u-r m)
	      (* #c(0 1)
		 (- (+ u-i
		       (/ (* (bigfloat::sn u-i m1) (bigfloat::dn u-i m1))
			  (bigfloat::cn u-i m1)))
		    (+ (elliptic-eu u-i m1)
		       (/ (* m (bigfloat::sn u-r m) (bigfloat::sn u-i m1) (bigfloat::sn u m))
			  (bigfloat::cn u-i m1))))))))))

(defprop $elliptic_eu
    ((u m)
     ((mexpt) ((%jacobi_dn) u m) 2)
     ;; wrt m
     )
  grad)

(def-simplifier elliptic_eu (u m)
  (cond
    ;; as it stands, ELLIPTIC-EU can't handle bigfloats or complex bigfloats,
    ;; so handle only floats and complex floats here.
    ((float-numerical-eval-p u m)
     (elliptic-eu ($float u) ($float m)))
    ((complex-float-numerical-eval-p u m)
     (let ((u-r ($realpart u))
	   (u-i ($imagpart u))
	   (m ($float m)))
       (complexify (elliptic-eu (complex u-r u-i) m))))
    (t
     (give-up))))

(def-simplifier jacobi_am (u m)
  (cond
    ;; as it stands, BIGFLOAT::SN can't handle bigfloats or complex bigfloats,
    ;; so handle only floats and complex floats here.
    ((float-numerical-eval-p u m)
     (cl:asin (bigfloat::sn ($float u) ($float m))))
    ((complex-float-numerical-eval-p u m)
     (let ((u-r ($realpart ($float u)))
	   (u-i ($imagpart ($float u)))
	   (m ($float m)))
       (complexify (cl:asin (bigfloat::sn (complex u-r u-i) m)))))
    (t
     ;; Nothing to do
     (give-up))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integrals.  At present with respect to first argument only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A&S 16.24.1: integrate(jacobi_sn(u,m),u)
;;              = log(jacobi_dn(u,m)-sqrt(m)*jacobi_cn(u,m))/sqrt(m)
(defprop %jacobi_sn
  ((u m)
   ((mtimes simp) ((mexpt simp) m ((rat simp) -1 2))
    ((%log simp)
     ((mplus simp)
      ((mtimes simp) -1 ((mexpt simp) m ((rat simp) 1 2))
       ((%jacobi_cn simp) u m))
      ((%jacobi_dn simp) u m))))
   nil)
  integral)

;; A&S 16.24.2: integrate(jacobi_cn(u,m),u) = acos(jacobi_dn(u,m))/sqrt(m)
(defprop %jacobi_cn
  ((u m)
   ((mtimes simp) ((mexpt simp) m ((rat simp) -1 2))
    ((%acos simp) ((%jacobi_dn simp) u m))) 
   nil)
  integral)

;; A&S 16.24.3: integrate(jacobi_dn(u,m),u) = asin(jacobi_sn(u,m))
(defprop %jacobi_dn
  ((u m)
   ((%asin simp) ((%jacobi_sn simp) u m)) 
   nil)
  integral)

;; A&S 16.24.4: integrate(jacobi_cd(u,m),u) 
;;              = log(jacobi_nd(u,m)+sqrt(m)*jacobi_sd(u,m))/sqrt(m)
(defprop %jacobi_cd
  ((u m)
   ((mtimes simp) ((mexpt simp) m ((rat simp) -1 2))
    ((%log simp)
     ((mplus simp) ((%jacobi_nd simp) u m)
      ((mtimes simp) ((mexpt simp) m ((rat simp) 1 2))
       ((%jacobi_sd simp) u m))))) 
   nil)
  integral)

;; integrate(jacobi_sd(u,m),u)
;;
;; A&S 16.24.5 gives
;;   asin(-sqrt(m)*jacobi_cd(u,m))/sqrt(m*m_1), where m + m_1 = 1
;;  but this does not pass some simple tests.
;;
;; functions.wolfram.com 09.35.21.001.01 gives
;;  -asin(sqrt(m)*jacobi_cd(u,m))*sqrt(1-m*jacobi_cd(u,m)^2)*jacobi_dn(u,m)/((1-m)*sqrt(m))
;; and this does pass.
(defprop %jacobi_sd
  ((u m)
   ((mtimes simp) -1
    ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
    ((mexpt simp) m ((rat simp) -1 2))
    ((mexpt simp)
     ((mplus simp) 1
      ((mtimes simp) -1 $m ((mexpt simp) ((%jacobi_cd simp) u m) 2)))
     ((rat simp) 1 2))
    ((%jacobi_dn simp) u m)
    ((%asin simp)
     ((mtimes simp) ((mexpt simp) m ((rat simp) 1 2))
      ((%jacobi_cd simp) u m)))) 
   nil)
  integral)

;; integrate(jacobi_nd(u,m),u)
;; 
;;  A&S 16.24.6 gives
;;    acos(jacobi_cd(u,m))/sqrt(m_1), where m + m_1 = 1
;;  but this does not pass some simple tests.
;;
;; functions.wolfram.com 09.32.21.0001.01 gives
;;  sqrt(1-jacobi_cd(u,m)^2)*acos(jacobi_cd(u,m))/((1-m)*jacobi_sd(u,m))
;; and this does pass.
(defprop %jacobi_nd
  ((u m)
   ((mtimes simp) ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m)) -1)
    ((mexpt simp)
     ((mplus simp) 1
      ((mtimes simp) -1 ((mexpt simp) ((%jacobi_cd simp) u m) 2)))
     ((rat simp) 1 2))
    ((mexpt simp) ((%jacobi_sd simp) u m) -1)
    ((%acos simp) ((%jacobi_cd simp) u m))) 
   nil)
  integral)

;; A&S 16.24.7: integrate(jacobi_dc(u,m),u) = log(jacobi_nc(u,m)+jacobi_sc(u,m))
(defprop %jacobi_dc
  ((u m)
   ((%log simp) ((mplus simp) ((%jacobi_nc simp) u m) ((%jacobi_sc simp) u m))) 
  nil)
  integral)

;; A&S 16.24.8: integrate(jacobi_nc(u,m),u) 
;;              = log(jacobi_dc(u,m)+sqrt(m_1)*jacobi_sc(u,m))/sqrt(m_1), where m + m_1 = 1
(defprop %jacobi_nc
  ((u m)
   ((mtimes simp)
    ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m))
     ((rat simp) -1 2))
    ((%log simp)
     ((mplus simp) ((%jacobi_dc simp) u m)
      ((mtimes simp)
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m))
	((rat simp) 1 2))
       ((%jacobi_sc simp) u m))))) 
   nil)
  integral)

;; A&S 16.24.9: integrate(jacobi_sc(u,m),u) 
;;              = log(jacobi_dc(u,m)+sqrt(m_1)*jacobi_nc(u,m))/sqrt(m_1), where m + m_1 = 1
(defprop %jacobi_sc
  ((u m)
   ((mtimes simp)
    ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m))
     ((rat simp) -1 2))
    ((%log simp)
     ((mplus simp) ((%jacobi_dc simp) u m)
      ((mtimes simp)
       ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m))
	((rat simp) 1 2))
       ((%jacobi_nc simp) u m))))) 
   nil)
  integral)

;; A&S 16.24.10: integrate(jacobi_ns(u,m),u) 
;;               = log(jacobi_ds(u,m)-jacobi_cs(u,m))
(defprop %jacobi_ns
  ((u m)
   ((%log simp)
    ((mplus simp) ((mtimes simp) -1 ((%jacobi_cs simp) u m))
     ((%jacobi_ds simp) u m)))
   nil)
  integral)

;; integrate(jacobi_ds(u,m),u)
;;
;;  A&S 16.24.11 gives
;;   log(jacobi_ds(u,m)-jacobi_cs(u,m))
;; but this does not pass some simple tests.
;;
;; functions.wolfram.com 09.30.21.0001.01 gives
;;   log((1-jacobi_cn(u,m))/jacobi_sn(u,m))
;; 
(defprop %jacobi_ds
  ((u m)
   ((%log simp)
    ((mtimes simp)
     ((mplus simp) 1 ((mtimes simp) -1 ((%jacobi_cn simp) u m)))
     ((mexpt simp) ((%jacobi_sn simp) u m) -1)))
   nil)
  integral)

;; A&S 16.24.12: integrate(jacobi_cs(u,m),u) = log(jacobi_ns(u,m)-jacobi_ds(u,m))
(defprop %jacobi_cs
  ((u m)
   ((%log simp)
    ((mplus simp) ((mtimes simp) -1 ((%jacobi_ds simp) u m))
     ((%jacobi_ns simp) u m)))
   nil)
  integral)

;; functions.wolfram.com 09.48.21.0001.01
;; integrate(inverse_jacobi_sn(u,m),u) =
;;   inverse_jacobi_sn(u,m)*u
;;   - log(         jacobi_dn(inverse_jacobi_sn(u,m),m) 
;;         -sqrt(m)*jacobi_cn(inverse_jacobi_sn(u,m),m)) / sqrt(m)
(defprop %inverse_jacobi_sn
  ((u m)
   ((mplus simp) ((mtimes simp) u ((%inverse_jacobi_sn simp) u m))
    ((mtimes simp) -1 ((mexpt simp) m ((rat simp) -1 2))
     ((%log simp)
      ((mplus simp)
       ((mtimes simp) -1 ((mexpt simp) m ((rat simp) 1 2))
	((%jacobi_cn simp) ((%inverse_jacobi_sn simp) u m) m))
       ((%jacobi_dn simp) ((%inverse_jacobi_sn simp) u m) m)))))
   nil)
  integral)

;; functions.wolfram.com 09.38.21.0001.01
;; integrate(inverse_jacobi_cn(u,m),u) =
;;   u*inverse_jacobi_cn(u,m)
;;    -%i*log(%i*jacobi_dn(inverse_jacobi_cn(u,m),m)/sqrt(m)
;;              -jacobi_sn(inverse_jacobi_cn(u,m),m))
;;      /sqrt(m)
(defprop %inverse_jacobi_cn
  ((u m)
   ((mplus simp) ((mtimes simp) u ((%inverse_jacobi_cn simp) u m))
    ((mtimes simp) -1 $%i ((mexpt simp) m ((rat simp) -1 2))
     ((%log simp)
      ((mplus simp)
       ((mtimes simp) $%i ((mexpt simp) m ((rat simp) -1 2))
	((%jacobi_dn simp) ((%inverse_jacobi_cn simp) u m) m))
       ((mtimes simp) -1
	((%jacobi_sn simp) ((%inverse_jacobi_cn simp) u m) m))))))
   nil)
  integral)

;; functions.wolfram.com 09.41.21.0001.01
;; integrate(inverse_jacobi_dn(u,m),u) =
;;   u*inverse_jacobi_dn(u,m)
;;   - %i*log(%i*jacobi_cn(inverse_jacobi_dn(u,m),m)
;;              +jacobi_sn(inverse_jacobi_dn(u,m),m))
(defprop %inverse_jacobi_dn
  ((u m)
   ((mplus simp) ((mtimes simp) u ((%inverse_jacobi_dn simp) u m))
    ((mtimes simp) -1 $%i
     ((%log simp)
      ((mplus simp)
       ((mtimes simp) $%i
	((%jacobi_cn simp) ((%inverse_jacobi_dn simp) u m) m))
       ((%jacobi_sn simp) ((%inverse_jacobi_dn simp) u m) m))))) 
  nil)
  integral)


;; Real and imaginary part for Jacobi elliptic functions.
(defprop %jacobi_sn risplit-sn-cn-dn risplit-function)
(defprop %jacobi_cn risplit-sn-cn-dn risplit-function)
(defprop %jacobi_dn risplit-sn-cn-dn risplit-function)

(defun risplit-sn-cn-dn (expr)
  (let* ((arg (second expr))
	 (param (third expr)))
    ;; We only split on the argument, not the order
    (destructuring-bind (arg-r . arg-i)
	(risplit arg)
      (cond ((=0 arg-i)
	     ;; Pure real
	     (cons (take (first expr) arg-r param)
		   0))
	    (t
	     (let* ((s (ftake '%jacobi_sn arg-r param))
		    (c (ftake '%jacobi_cn arg-r param))
		    (d (ftake '%jacobi_dn arg-r param))
		    (s1 (ftake '%jacobi_sn arg-i (sub 1 param)))
		    (c1 (ftake '%jacobi_cn arg-i (sub 1 param)))
		    (d1 (ftake '%jacobi_dn arg-i (sub 1 param)))
		    (den (add (mul c1 c1)
			      (mul param
				   (mul (mul s s)
					(mul s1 s1))))))
	       ;; Let s = jacobi_sn(x,m)
	       ;;     c = jacobi_cn(x,m)
	       ;;     d = jacobi_dn(x,m)
	       ;;     s1 = jacobi_sn(y,1-m)
	       ;;     c1 = jacobi_cn(y,1-m)
	       ;;     d1 = jacobi_dn(y,1-m)
	       (case (caar expr)
		 (%jacobi_sn
		  ;; A&S 16.21.1
		  ;; jacobi_sn(x+%i*y,m) =
		  ;;
		  ;;  s*d1 + %i*c*d*s1*c1
		  ;;  -------------------
		  ;;    c1^2+m*s^2*s1^2
		  ;;
		  (cons (div (mul s d1) den)
			(div (mul c (mul d (mul s1 c1)))
			     den)))
		 (%jacobi_cn
		  ;; A&S 16.21.2
		  ;;
		  ;; cn(x+%i_y, m) =
		  ;;
		  ;;  c*c1 - %i*s*d*s1*d1
		  ;;  -------------------
		  ;;    c1^2+m*s^2*s1^2
		  (cons (div (mul c c1) den)
			(div (mul -1
				  (mul s (mul d (mul s1 d1))))
			     den)))
		 (%jacobi_dn
		  ;; A&S 16.21.3
		  ;;
		  ;; dn(x+%i_y, m) =
		  ;;
		  ;;  d*c1*d1 - %i*m*s*c*s1
		  ;;  ---------------------
		  ;;    c1^2+m*s^2*s1^2
		  (cons (div (mul d (mul c1 d1))
			     den)
			(div (mul -1 (mul param (mul s (mul c s1))))
			     den))))))))))

