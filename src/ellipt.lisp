;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10-*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 2001 by Raymond Toy.  Replaced everything and added ;;;;;
;;;     support for symbolic manipulation of all 12 Jacobian elliptic  ;;;;;
;;;     functions and the complete and incomplete elliptic integral    ;;;;;
;;;     of the first, second and third kinds.                          ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;(macsyma-module ellipt)

;;;
;;; Jacobian elliptic functions and elliptic integrals.
;;;
;;; References:
;;;
;;; [1] Abramowitz and Stegun
;;; [2] Lawden, Elliptic Functions and Applications, Springer-Verlag, 1989
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
;; transformations (16.12 and 16.14).  We use these latter because
;; they are actually quite fast, only requiring simple arithmetic and
;; square roots for the transformation until the last step.  The AGM
;; requires evaluation of several trignometric functions at each
;; stage.
;;
;; In addition, the Landen transformations are valid for all u and m.
;; Thus, we can compute the elliptic functions for complex u and
;; m. (The code below supports this, but we could make it run much
;; faster if we specialized it to for double-floats.  However, if we
;; do that, we won't be able to handle the cases where m < 0 or m > 1.
;; We'll have to handle these specially via A&S 16.10 and 16.11.)
;;
;; See A&S 16.12 and 16.14.


;;; gcl (as of today, Jan 14, 2002) lacks destructuring-bind.
;;; The following version of destructuring-bind (ad supporting functions)
;;; was stolen from cmucl.
#+gcl
(eval-when (compile load eval)
(defun extract-declarations (body &optional environment)
  ;;(declare (values documentation declarations body))
  (let (documentation declarations form)
    (when (and (stringp (car body))
	       (cdr body))
      (setq documentation (pop body)))
    (block outer
      (loop
	(when (null body) (return-from outer nil))
	(setq form (car body))
	(when (block inner
		(loop (cond ((not (listp form))
			     (return-from outer nil))
			    ((eq (car form) 'declare)
			     (return-from inner 't))
			    (t
			     (multiple-value-bind (newform macrop)
				  (macroexpand-1 form environment)
			       (if (or (not (eq newform form)) macrop)
				   (setq form newform)
				 (return-from outer nil)))))))
	  (pop body)
	  (dolist (declaration (cdr form))
	    (push declaration declarations)))))
    (values documentation
	    (and declarations `((declare ,.(nreverse declarations))))
	    body)))
(defun destructure (pattern form)
  ;;(declare (values setqs binds))
  (let ((*destructure-vars* ())
	(setqs ()))
    (declare (special *destructure-vars*))
    (setq *destructure-vars* '(.destructure-form.)
	  setqs (list `(setq .destructure-form. ,form))
	  form '.destructure-form.)
    (values (nconc setqs (nreverse (destructure-internal pattern form)))
	    (delete nil *destructure-vars*))))
(defun destructure-internal (pattern form)
  ;; When we are called, pattern must be a list.  Form should be a symbol
  ;; which we are free to setq containing the value to be destructured.
  ;; Optimizations are performed for the last element of pattern cases.
  ;; we assume that the compiler is smart about gensyms which are bound
  ;; but only for a short period of time.
  (declare (special *destructure-vars*))
  (let ((gensym (gensym))
	(pending-pops 0)
	(var nil)
	(setqs ()))
    (labels
        ((make-pop (var form pop-into)
	   (prog1 
	     (cond ((zerop pending-pops)
		    `(progn ,(and var `(setq ,var (car ,form)))
			    ,(and pop-into `(setq ,pop-into (cdr ,form)))))
		   ((null pop-into)
		    (and var `(setq ,var ,(make-caxr pending-pops form))))
		   (t
		    `(progn (setq ,pop-into ,(make-cdxr pending-pops form))
			    ,(and var `(setq ,var (pop ,pop-into))))))
	     (setq pending-pops 0))))
      (do ((pat pattern (cdr pat)))
	  ((null pat) ())
	(if (symbolp (setq var (car pat)))
	    (progn
	      (unless (memq var '(nil ignore))
			 (push var *destructure-vars*))	      
	      (cond ((null (cdr pat))
		     (push (make-pop var form ()) setqs))
		    ((symbolp (cdr pat))
		     (push (make-pop var form (cdr pat)) setqs)
		     (push (cdr pat) *destructure-vars*)
		     (return ()))
		    ((memq var '(nil ignore)) (incf pending-pops))
		    ((memq (cadr pat) '(nil ignore))
		     (push (make-pop var form ()) setqs)
		     (incf pending-pops 1))
		    (t
		     (push (make-pop var form form) setqs))))
	    (progn
	      (push `(let ((,gensym ()))
		       ,(make-pop gensym
				  form
				  (if (symbolp (cdr pat)) (cdr pat) form))
		       ,@(nreverse
			   (destructure-internal (car pat) gensym)))
		    setqs)
	      (when (symbolp (cdr pat))
		(push (cdr pat) *destructure-vars*)
		(return)))))
      setqs)))
)
#+gcl
(defmacro destructuring-bind (pattern form &body body)
  (multiple-value-bind (ignore declares body)
      (extract-declarations body)
    (declare (ignore ignore))
    (multiple-value-bind (setqs binds)
	(destructure pattern form)
      `(let ,binds
	 ,@declares
	 ,@setqs
	 (progn .destructure-form.)
	 . ,body))))
;;; end of destructuring-bind code from cmucl.


(flet ((ascending-transform (u m)
	 ;; A&S 16.14.1
	 ;;
	 ;; Take care in computing this transform.  For the case where
	 ;; m is complex, we should compute sqrt(mu1) first as
	 ;; (1-sqrt(m))/(1+sqrt(m)), and then square this to get mu1.
	 ;; If not, we may choose the wrong branch when computing
	 ;; sqrt(mu1).
	 (let* ((root-m (lisp:sqrt m))
		(mu (/ (* 4 root-m)
		       (lisp:expt (1+ root-m) 2)))
		(root-mu1 (/ (- 1 root-m) (+ 1 root-m)))
		(v (/ u (1+ root-mu1))))
	   (values v mu root-mu1)))
       (descending-transform (u m)
	 ;; Note: Don't calculate mu first, as given in 16.12.1.  We
	 ;; should calculate sqrt(mu) = (1-sqrt(m1)/(1+sqrt(m1)), and
	 ;; then compute mu = sqrt(mu)^2.  If we calculate mu first,
	 ;; sqrt(mu) loses information when m or m1 is complex.
	 (let* ((root-m1 (lisp:sqrt (- 1 m)))
		(root-mu (/ (- 1 root-m1) (+ 1 root-m1)))
		(mu (* root-mu root-mu))
		(v (/ u (1+ root-mu))))
	   (values v mu root-mu))))
  (declaim (inline descending-transform ascending-transform))


  ;; Could use the descending transform, but some of my tests show
  ;; that it has problems with roundoff errors.
  (defun elliptic-dn-ascending (u m)
    (if (< (abs (- 1 m)) (* 4 double-float-epsilon))
	;; A&S 16.6.3
	(/ (lisp:cosh u))
	(multiple-value-bind (v mu root-mu1)
	    (ascending-transform u m)
	  ;; A&S 16.14.4
	  (let* ((new-dn (elliptic-dn-ascending v mu)))
	    (* (/ (- 1 root-mu1) mu)
	       (/ (+ root-mu1 (* new-dn new-dn))
		  new-dn))))))

  ;; Don't use the descending version because it requires cn, dn, and
  ;; sn.
  (defun elliptic-cn-ascending (u m)
    (if (< (abs (- 1 m)) (* 4 double-float-epsilon))
	;; A&S 16.6.2
	(/ (lisp:cosh u))
	(multiple-value-bind (v mu root-mu1)
	    (ascending-transform u m)
	  ;; A&S 16.14.3
	  (let* ((new-dn (elliptic-dn-ascending v mu)))
	    (* (/ (+ 1 root-mu1) mu)
	       (/ (- (* new-dn new-dn) root-mu1)
		  new-dn))))))

  ;; We don't use the ascending transform here because it requires
  ;; evaluating sn, cn, and dn.  The ascending transform only needs
  ;; sn.
  (defun elliptic-sn-descending (u m)
    ;; A&S 16.12.2
    (if (< (abs m) double-float-epsilon)
	(lisp:sin u)
	(multiple-value-bind (v mu root-mu)
	    (descending-transform u m)
	  (let* ((new-sn (elliptic-sn-descending v mu)))
	    (/ (* (1+ root-mu) new-sn)
	       (1+ (* root-mu new-sn new-sn)))))))
  #+nil
  (defun elliptic-sn-ascending (u m)
    (if (< (abs (- 1 m)) (* 4 double-float-epsilon))
	;; A&S 16.6.1
	(tanh u)
	(multiple-value-bind (v mu root-mu1)
	    (ascending-transform u m)
	  ;; A&S 16.14.2
	  (let* ((new-cn (elliptic-cn-ascending v mu))
		 (new-dn (elliptic-dn-ascending v mu))
		 (new-sn (elliptic-sn-ascending v mu)))
	    (/ (* (+ 1 root-mu1) new-sn new-cn)
	       new-dn)))))
)

(defun sn (u m)
  (if (and (realp u) (realp m))
      (realpart (elliptic-sn-descending u m))
      (elliptic-sn-descending u m)))

(defun cn (u m)
  (if (and (realp u) (realp m))
      (realpart (elliptic-cn-ascending u m))
      (elliptic-cn-ascending u m)))

(defun dn (u m)
  (if (and (realp u) (realp m))
      (realpart (elliptic-dn-ascending u m))
      (elliptic-dn-ascending u m)))


;;
;; How this works, I think.
;;
;; $jacobi_sn is the user visible function JACOBI_SN.  We put
;; properties on this symbol so maxima can figure out what to do with
;; it.

;; Tell maxima how to simplify the functions $jacobi_sn, etc.  This
;; borrows heavily from trigi.lisp.
(defprop %jacobi_sn simp-%jacobi_sn operators)
(defprop %jacobi_cn simp-%jacobi_cn operators)
(defprop %jacobi_dn simp-%jacobi_dn operators)
(defprop %inverse_jacobi_sn simp-%inverse_jacobi_sn operators)
(defprop %inverse_jacobi_cn simp-%inverse_jacobi_cn operators)
(defprop %inverse_jacobi_dn simp-%inverse_jacobi_dn operators)

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
;; Interate this to get
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
	 (($elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
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
	 (($elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
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
	 (($elliptic_e simp) ((%asin simp) ((%jacobi_sn simp) u m)) m))))))
  grad)

;; The inverse elliptic functions.
;;
;; F(phi|m) = asn(sin(phi),m)
;; 
;; so asn(u,m) = F(asin(u)|m)
(defprop %inverse_jacobi_sn
    ((x m)
     ;; 1/sqrt(1-x^2)/sqrt(1-m*x^2)
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
     ;; -1/sqrt(1-x^2)/sqrt(1-m*x^2)
     ((mtimes simp) -1
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 ((mexpt simp) x 2)))
       ((rat simp) -1 2))
      ((mexpt simp) ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) x 2)))
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
     ;; -1/sqrt(1-x^2)/sqrt(x^2+m-1)
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

;; Define the actual functions for the user
(defmfun $jacobi_sn (u m)
  (simplify (list '(%jacobi_sn) (resimplify u) (resimplify m))))
(defmfun $jacobi_cn (u m)
  (simplify (list '(%jacobi_cn) (resimplify u) (resimplify m))))
(defmfun $jacobi_dn (u m)
  (simplify (list '(%jacobi_dn) (resimplify u) (resimplify m))))

(defmfun $inverse_jacobi_sn (u m)
  (simplify (list '(%inverse_jacobi_sn) (resimplify u) (resimplify m))))

(defmfun $inverse_jacobi_cn (u m)
  (simplify (list '(%inverse_jacobi_cn) (resimplify u) (resimplify m))))

(defmfun $inverse_jacobi_dn (u m)
  (simplify (list '(%inverse_jacobi_dn) (resimplify u) (resimplify m))))

;; A complex number looks like
;;
;; ((MPLUS SIMP) 0.70710678118654757 ((MTIMES SIMP) 0.70710678118654757 $%I))
;;
;; or
;;
;; ((MPLUS SIMP) 1 $%I))
;;
(defun complex-number-p (u)
  ;; Return non-NIL if U is a complex number (or number)
  (or (numberp u)
      (and (consp u)
	   (numberp (second u))
	   (or (and (consp (third u))
		    (numberp (second (third u)))
		    (eq (third (third u)) '$%i))
	       (and (eq (third u) '$%i))))))

(defun complexify (x)
  ;; Convert a Lisp number to a maxima number
  (if (realp x)
      x
      (if (zerop (realpart x))
	  `((mtimes) ,(imagpart x) $%i)
	  `((mplus simp) ,(realpart x)
	    ((mtimes simp) ,(imagpart x) $%i)))))


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
;;
;; FORM is list containing the actual expression.  I don't really know
;; what Y and Z contain.  Most of this modeled after SIMP-%SIN.
(defmfun simp-%jacobi_sn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate sn
	   (sn (float u 1d0) (float m 1d0)))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   ;; For complex values.  Should we really do this?
	   (let ((result (sn (complex ($realpart u) ($imagpart u))
			     (complex ($realpart m) ($imagpart m)))))
	     (complexify result)))
	  ((zerop1 u)
	   ;; A&S 16.5.1
	   0)
	  ((zerop1 m)
	   ;; A&S 16.6.1
	   `((%sin) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.1
	   `((%tanh) ,u))
	  ((and $trigsign (mminusp* u))
	   (neg (cons-exp '%jacobi_sn (neg u) m)))
	  ;; A&S 16.20.1 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   (mul '$%i
		(cons-exp '%jacobi_sc (coeff u '$%i 1) (add 1 (neg m)))))
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
			   `((%jacobi_sn simp) ,const ,m)))
		      (1
		       ;; sn(4*m*K + K + u) = sn(K+u) = cd(u)
		       ;; sn(K) = 1
		       (if (zerop1 const)
			   1
			   `((%jacobi_cd simp) ,const ,m)))
		      (2
		       ;; sn(4*m*K+2*K + u) = sn(2*K+u) = -sn(u)
		       ;; sn(2*K) = 0
		       (if (zerop1 const)
			   0
			   (neg `((%jacobi_sn simp) ,const ,m))))
		      (3
		       ;; sn(4*m*K+3*K+u) = sn(2*K + K + u) = -sn(K+u) = -cd(u)
		       ;; sn(3*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_cd simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; A&S 16.5.2
		    ;;
		    ;; sn(1/2*K) = 1/sqrt(1+sqrt(1-m))
		    `((mexpt simp)
		      ((mplus simp) 1
		       ((mexpt simp)
			((mplus simp) 1 ((mtimes simp) -1 ,m))
			((rat simp) 1 2)))
		      ((rat) -1 2)))
		   ((and (alike1 lin 3//2)
			 (zerop1 const))
		    ;; A&S 16.5.2
		    ;;
		    ;; sn(1/2*K + K) = cd(1/2*K,m)
		    (simplifya
		     `((%jacobi_cd) ((mtimes) ((rat) 1 2) ((%elliptic_kc) ,m))
		       ,m)
		     nil))
		   (t
		    (eqtest (list '(%jacobi_sn) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_sn) u m) form)))))

(defmfun simp-%jacobi_cn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (cn (float u 1d0) (float m 1d0)))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((result (cn (complex ($realpart u) ($imagpart u))
			     (complex ($realpart m) ($imagpart m)))))
	     (complexify result)))
	  ((zerop1 u)
	   ;; A&S 16.5.1
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.2
	   `((%cos) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.2
	   `((%sech) ,u))
	  ((and $trigsign (mminusp* u))
	   (cons-exp '%jacobi_cn (neg u) m))
	  ;; A&S 16.20.2 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   (cons-exp '%jacobi_nc (coeff u '$%i 1) (add 1 (neg m))))
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
			   `((%jacobi_cn simp) ,const ,m)))
		      (1
		       ;; cn(4*m*K + K + u) = cn(K+u) = -sqrt(m1)*sd(u)
		       ;; cn(K) = 0
		       (if (zerop1 const)
			   0
			   (neg `((mtimes simp)
				  ((mexpt simp)
				   ((mplus simp) 1 ((mtimes simp) -1 ,m))
				   ((rat simp) 1 2))
				  ((%jacobi_sd simp) ,const ,m)))))
		      (2
		       ;; cn(4*m*K + 2*K + u) = cn(2*K+u) = -cn(u)
		       ;; cn(2*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_cn) ,const ,m))))
		      (3
		       ;; cn(4*m*K + 3*K + u) = cn(2*K + K + u) =
		       ;; -cn(K+u) = sqrt(m1)*sd(u)
		       ;;
		       ;; cn(3*K) = 0
		       (if (zerop1 const)
			   0
			   `((mtimes simp)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) 1 2))
			     ((%jacobi_sd simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; A&S 16.5.2
		    ;; cn(1/2*K) = (1-m)^(1/4)/sqrt(1+sqrt(1-m))
		    `((mtimes simp)
		      ((mexpt simp) ((mplus simp) 1
				     ((mtimes simp) -1 ,m))
		       ((rat simp) 1 4))
		      ((mexpt simp)
		       ((mplus simp) 1
			((mexpt simp)
			 ((mplus simp) 1
			  ((mtimes simp) -1 ,m))
			 ((rat simp) 1 2)))
		       ((rat simp) -1 2))))
		   (t
		    (eqtest (list '(%jacobi_cn) u m) form)))))
	  (t
	   (eqtest (list '(%jacobi_cn) u m) form)))))

(defmfun simp-%jacobi_dn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (dn (float u 1d0) (float m 1d0)))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((result (dn (complex ($realpart u) ($imagpart u))
			     (complex ($realpart m) ($imagpart m)))))
	     (complexify result)))
	  ((zerop1 u)
	   ;; A&S 16.5.1
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.3
	   1)
	  ((onep1 m)
	   ;; A&S 16.6.3
	   `(($sech) ,u))
	  ((and $trigsign (mminusp* u))
	   (cons-exp '%jacobi_dn (neg u) m))
	  ;; A&S 16.20.2 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   (cons-exp '%jacobi_dc (coeff u '$%i 1)
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
			   `((%jacobi_dn) ,const ,m)))
		      (1
		       ;; dn(2*m*K + K + u) = dn(K + u) = sqrt(1-m)*nd(u)
		       ;; dn(K) = sqrt(1-m)
		       (if (zerop1 const)
			   `((mexpt simp)
			     ((mplus simp) 1 ((mtimes simp) -1 ,m))
			     ((rat simp) 1 2))
			   `((mtimes simp)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) 1 2))
			     ((%jacobi_nd simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; A&S 16.5.2
		    ;; dn(1/2*K) = (1-m)^(1/4)
		    `((mexpt simp)
		      ((mplus simp) 1 ((mtimes simp) -1 ,m))
		      ((rat simp) 1 4)))
		   (t
		    (eqtest (list '(%jacobi_cn) u m) form)))))
	  (t (eqtest (list '(%jacobi_dn) u m) form)))))

;; Should we simplify the inverse elliptic functions into the
;; appropriate incomplete elliptic integral?  I think we should leave
;; it, but perhaps allow some way to do that transformation if
;; desired.

(defmfun simp-%inverse_jacobi_sn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate asn
	   ;;
	   ;; asn(x,m) = F(asin(x),m)
	   (elliptic-f (lisp:asin u) m))
	  ((zerop1 u)
	   ;; asn(0,m) = 0
	   0)
	  ((onep1 u)
	   ;; asn(1,m) = elliptic_kc(m)
	   `(($elliptic_kc) ,m))
	  ((and (numberp u) (onep1 (- u)))
	   ;; asn(-1,m) = -elliptic_kc(m)
	   `((mtimes) -1 ((%elliptic_kc) ,m)))
	  ((zerop1 m)
	   ;; asn(x,0) = F(asin(x),0) = asin(x)
	   `((%elliptic_f) ((%asin) ,u) 0))
	  ((onep1 m)
	   ;; asn(x,1) = F(asin(x),1) = log(tan(pi/2+asin(x)/2))
	   `((%elliptic_f) ((%asin) ,u) 1))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_sn) u m) form)))))

(defmfun simp-%inverse_jacobi_cn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate acn
	   ;;
	   ;; acn(x,m) = F(acos(x),m)
	   (elliptic-f (acos u) m))
	  ((zerop1 m)
	   ;; asn(x,0) = F(acos(x),0) = acos(x)
	   `((%elliptic_f) ((%acos) ,u) 0))
	  ((onep1 m)
	   ;; asn(x,1) = F(asin(x),1) = log(tan(pi/2+asin(x)/2))
	   `((%elliptic_f) ((%acos) ,u) 1))
	  ((zerop1 u)
	   `((%elliptic_kc) ,m))
	  ((onep1 u)
	   0)
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_cn) u m) form)))))

;;
;; adn(u) = x.
;; u = dn(x) = sqrt(1-m*sn(x)^2)
;; sn(x)^2 = (1-u^2)/m
;; sn(x) = sqrt((1-u^2)/m)
;; x = asn(sqrt((1-u^2)/m))
;; x = adn(u) = asn(sqrt((1-u^2)/m))

(defmfun simp-%inverse_jacobi_dn (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate adn
	   (let ((phi (/ (* (sqrt (- 1 u)) (sqrt (+ 1 u)))
			 (sqrt m))))
	     (elliptic-f (asin phi) m)))
	  ((onep1 m)
	   ;; x = dn(u,1) = sech(u).  so u = asech(x)
	   `((%asech) ,u))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_dn) u m) form)))))

;;;; Elliptic integrals

;; Carlson's elliptic integral of the first kind.
;;
;; ***PURPOSE  Compute the incomplete or complete elliptic integral of the
;;             1st kind.  For X, Y, and Z non-negative and at most one of
;;             them zero, RF(X,Y,Z) = Integral from zero to infinity of
;;                                 -1/2     -1/2     -1/2
;;                       (1/2)(t+X)    (t+Y)    (t+Z)    dt.
;;             If X, Y or Z is zero, the integral is complete.
;;
;;          Value of IER assigned by the DRF routine
;;
;;                   Value assigned         Error Message Printed
;;                   IER = 1                MIN(X,Y,Z) .LT. 0.0D0
;;                       = 2                MIN(X+Y,X+Z,Y+Z) .LT. LOLIM
;;                       = 3                MAX(X,Y,Z) .GT. UPLIM
;;
;;    Special double precision functions via DRF
;;
;;
;;
;;
;;                   Legendre form of ELLIPTIC INTEGRAL of 1st kind
;;
;;                   -----------------------------------------
;;
;;
;;
;;                                              2         2   2
;;                   F(PHI,K) = SIN(PHI) DRF(COS (PHI),1-K SIN (PHI),1)
;;
;;
;;                                   2
;;                   K(K) = DRF(0,1-K ,1)
;;
;;
;;                          PI/2     2   2      -1/2
;;                        = INT  (1-K SIN (PHI) )   D PHI
;;                           0
;;
;;
;;
;;                   Bulirsch form of ELLIPTIC INTEGRAL of 1st kind
;;
;;                   -----------------------------------------
;;
;;
;;                                           2 2    2
;;                   EL1(X,KC) = X DRF(1,1+KC X ,1+X )
;;
;;
;;                   Lemniscate constant A
;;
;;                   -----------------------------------------
;;
;;
;;                        1      4 -1/2
;;                   A = INT (1-S )    DS = DRF(0,1,2) = DRF(0,2,1)
;;                        0
;;
;;
;;
;;     -------------------------------------------------------------------
;;
;; ***REFERENCES  B. C. Carlson and E. M. Notis, Algorithms for incomplete
;;                  elliptic integrals, ACM Transactions on Mathematical
;;                  Software 7, 3 (September 1981), pp. 398-403.
;;                B. C. Carlson, Computing elliptic integrals by
;;                  duplication, Numerische Mathematik 33, (1979),
;;                  pp. 1-16.
;;                B. C. Carlson, Elliptic integrals of the first kind,
;;                  SIAM Journal of Mathematical Analysis 8, (1977),
;;                 pp. 231-242.

(let ((errtol (expt (* 4 double-float-epsilon) 1/6))
      (uplim (/ most-positive-double-float 5))
      (lolim (* #-gcl least-positive-normalized-double-float
		#+gcl least-positive-double-float
		5))
      (c1 (float 1/24 1d0))
      (c2 (float 3/44 1d0))
      (c3 (float 1/14 1d0)))
  (declare (double-float errtol c1 c2 c3))
  (defun drf (x y z)
    "Compute Carlson's incomplete or complete elliptic integral of the
first kind:

                   INF
                  /
                  [                     1
  RF(x, y, z) =   I    ----------------------------------- dt
                  ]    SQRT(x + t) SQRT(y + t) SQRT(z + t)
                  /
                   0


where x >= 0, y >= 0, z >=0, and at most one of x, y, z is zero.
"
    (declare (double-float x y z))
    ;; Check validity of input
    (assert (and (>= x 0) (>= y 0) (>= z 0)
		 (plusp (+ x y)) (plusp (+ x z)) (plusp (+ y z))))
    (assert (<= (max x y z) uplim))
    (assert (>= (min (+ x y) (+ x z) (+ y z)) lolim))
    (locally 
	(declare (type (double-float 0d0) x y)
		 (type (double-float (0d0)) z)
		 (optimize (speed 3)))
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
  (let ((phi (float phi-arg 1d0))
	(m (float m-arg 1d0)))
    (cond ((> m 1)
	   ;; A&S 17.4.15
	   (/ (elliptic-f (asin (* (sqrt m) (sin phi))) (/ m))))
	  ((< m 0)
	   ;; A&S 17.4.17
	   (let* ((m (- m))
		  (m+1 (+ 1 m))
		  (root (sqrt m+1))
		  (m/m+1 (/ m m+1)))
	     (- (/ (elliptic-f (float (/ pi 2) 1d0) m/m+1)
		   root)
		(/ (elliptic-f (- (float (/ pi 2) 1d0) phi) m/m+1)
		   root))))
	  ((= m 0)
	   ;; A&S 17.4.19
	   phi)
	  ((= m 1)
	   ;; A&S 17.4.21
	   (log (lisp:tan (+ (/ phi 2) (float (/ pi 2) 1d0)))))
	  ((minusp phi)
	   (- (elliptic-f (- phi) m)))
	  ((> phi pi)
	   ;; A&S 17.4.3
	   (multiple-value-bind (s phi-rem)
	       (truncate phi (float pi 1d0))
	     (+ (* 2 s (elliptic-k m))
		(elliptic-f phi-rem m))))
	  ((<= phi (/ pi 2))
	   (let ((sin-phi (sin phi))
		 (cos-phi (cos phi))
		 (k (sqrt m)))
	     (* sin-phi
		(drf (* cos-phi cos-phi)
		     (* (- 1 (* k sin-phi))
			(+ 1 (* k sin-phi)))
		     1d0))))
	  ((< phi pi)
	   (+ (* 2 (elliptic-k m))
	      (elliptic-f (- phi (float pi 1d0)) m))))))

;; Complete elliptic integral of the first kind
(defun elliptic-k (m)
  (declare (double-float m))
  (cond ((> m 1)
	 ;; A&S 17.4.15
	 (/ (elliptic-f (asin (sqrt m)) (/ m))))
	((< m 0)
	 ;; A&S 17.4.17
	 (let* ((m (- m))
		(m+1 (+ 1 m))
		(root (sqrt m+1))
		(m/m+1 (/ m m+1)))
	   (- (/ (elliptic-k m/m+1)
		 root)
	      (/ (elliptic-f 0d0 m/m+1)
		 root))))
	((= m 0)
	 ;; A&S 17.4.19
	 (float (/ pi 2) 1d0))
	(t
	 (let ((k (sqrt m)))
	   (drf 0d0 (* (- 1 k)
		       (+ 1 k))
		1d0)))))
;;
;; Carlsons' elliptic integral of the second kind.
;;
;;   1.     DRD
;;          Evaluate an INCOMPLETE (or COMPLETE) ELLIPTIC INTEGRAL
;;          of the second kind
;;          Standard FORTRAN function routine
;;          Double precision version
;;          The routine calculates an approximation result to
;;          DRD(X,Y,Z) = Integral from zero to infinity of
;;                              -1/2     -1/2     -3/2
;;                    (3/2)(t+X)    (t+Y)    (t+Z)    dt,
;;          where X and Y are nonnegative, X + Y is positive, and Z is
;;          positive.  If X or Y is zero, the integral is COMPLETE.

;;    -------------------------------------------------------------------
;;
;;
;;   Special double precision functions via DRD and DRF
;;
;;
;;                  Legendre form of ELLIPTIC INTEGRAL of 2nd kind
;;
;;                  -----------------------------------------
;;
;;
;;                                             2         2   2
;;                  E(PHI,K) = SIN(PHI) DRF(COS (PHI),1-K SIN (PHI),1) -
;;
;;                     2      3             2         2   2
;;                  -(K/3) SIN (PHI) DRD(COS (PHI),1-K SIN (PHI),1)
;;
;;
;;                                  2        2            2
;;                  E(K) = DRF(0,1-K ,1) - (K/3) DRD(0,1-K ,1)
;;
;;                         PI/2     2   2      1/2
;;                       = INT  (1-K SIN (PHI) )  D PHI
;;                          0
;;
;;                  Bulirsch form of ELLIPTIC INTEGRAL of 2nd kind
;;
;;                  -----------------------------------------
;;
;;                                               2 2    2
;;                  EL2(X,KC,A,B) = AX DRF(1,1+KC X ,1+X ) +
;;
;;                                              3          2 2    2
;;                                 +(1/3)(B-A) X DRD(1,1+KC X ,1+X )
;;
;;
;;
;;
;;                  Legendre form of alternative ELLIPTIC INTEGRAL
;;                  of 2nd kind
;;
;;                  -----------------------------------------
;;
;;
;;
;;                            Q     2       2   2  -1/2
;;                  D(Q,K) = INT SIN P  (1-K SIN P)     DP
;;                            0
;;
;;
;;
;;                                     3          2     2   2
;;                  D(Q,K) = (1/3) (SIN Q) DRD(COS Q,1-K SIN Q,1)
;;
;;
;;
;;
;;                  Lemniscate constant  B
;;
;;                  -----------------------------------------
;;
;;
;;
;;
;;                       1    2    4 -1/2
;;                  B = INT  S (1-S )    DS
;;                       0
;;
;;
;;                  B = (1/3) DRD (0,2,1)
;;
;;
;;                  Heuman's LAMBDA function
;;
;;                  -----------------------------------------
;;
;;
;;
;;                  (PI/2) LAMBDA0(A,B) =
;;
;;                                    2                2
;;                 = SIN(B) (DRF(0,COS (A),1)-(1/3) SIN (A) *
;;
;;                            2               2         2       2
;;                  *DRD(0,COS (A),1)) DRF(COS (B),1-COS (A) SIN (B),1)
;;
;;                            2       3             2
;;                  -(1/3) COS (A) SIN (B) DRF(0,COS (A),1) *
;;
;;                           2         2       2
;;                   *DRD(COS (B),1-COS (A) SIN (B),1)
;;
;;
;;
;;                  Jacobi ZETA function
;;
;;                  -----------------------------------------
;;
;;                             2                 2       2   2
;;                  Z(B,K) = (K/3) SIN(B) DRF(COS (B),1-K SIN (B),1)
;;
;;
;;                                       2             2
;;                             *DRD(0,1-K ,1)/DRF(0,1-K ,1)
;;
;;                               2       3           2       2   2
;;                            -(K /3) SIN (B) DRD(COS (B),1-K SIN (B),1)
;;
;;

(let ((errtol (expt (/ double-float-epsilon 3) 1/6))
      (c1 (float 3/14 1d0))
      (c2 (float 1/6 1d0))
      (c3 (float 9/22 1d0))
      (c4 (float 3/26 1d0)))
  (declare (double-float errtol c1 c2 c3 c4))
  (defun drd (x y z)
    (declare (real x y z))
    ;; Check validity of input

    (assert (and (>= x 0) (>= y 0) (>= z 0) (plusp (+ x y)))
	    (x y z))
	    
    (let ((x (float x 1d0))
	  (y (float y 1d0))
	  (z (float z 1d0))
	  (sigma 0d0)
	  (power4 1d0))
      (declare (type (double-float 0d0) x y power4 sigma)
	       (type (double-float (0d0)) z)
	       (optimize (speed 3)))
      (loop
	  (let* ((mu (* 1/5 (+ x y (* 3 z))))
		 (x-dev (/ (- mu x) mu))
		 (y-dev (/ (- mu y) mu))
		 (z-dev (/ (- mu z) mu)))
	    (when (< (max (abs x-dev) (abs y-dev) (abs z-dev)) errtol)
	      (let* ((ea (* x-dev y-dev))
		     (eb (* z-dev z-dev))
		     (ec (- ea eb))
		     (ed (- ea (* 6 eb)))
		     (ef (+ ed ec ec))
		     (s1 (* ed (+ (- c1)
				  (* 1/4 c3 ed)
				  (* -3/2 c4 z-dev ef))))
		     (s2 (* z-dev (+ (* c2 ef)
				     (* z-dev (+ (* (- c3) ec)
						 (* z-dev c4 ea)))))))
		(return (+ (* 3 sigma)
			   (/ (* power4 (+ 1 s1 s2))
			      (* mu (sqrt mu)))))))
	    (let* ((x-root (sqrt x))
		   (y-root (sqrt y))
		   (z-root (sqrt z))
		   (lam (+ (* x-root (+ y-root z-root)) (* y-root z-root))))
	      (incf sigma (/ power4 z-root (+ z lam)))
	      (setf power4 (/ power4 4))
	      (setf x (/ (+ x lam) 4))
	      (setf y (/ (+ y lam) 4))
	      (setf z (/ (+ z lam) 4))))))))

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
  (declare (double-float phi m))
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
	   (- (* sin-phi
		 (drf (* cos-phi cos-phi) y 1d0))
	      (* (/ m 3)
		 (expt sin-phi 3)
		 (drd (* cos-phi cos-phi) y 1d0)))))))

;; Complete version
(defun elliptic-ec (m)
  (declare (double-float m))
  (cond ((= m 0)
	 ;; A&S 17.4.23
	 (float (/ pi 2) 1d0))
	((= m 1)
	 ;; A&S 17.4.25
	 1d0)
	(t
	 (let* ((k (sqrt m))
		(y (* (- 1 k)
		      (+ 1 k))))
	   (- (drf 0d0 y 1d0)
	      (* (/ m 3)
		 (drd 0d0 y 1d0)))))))


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


(defprop $elliptic_f simp-$elliptic_f operators)
(defprop $elliptic_e simp-$elliptic_e operators)

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

(defprop $elliptic_f
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
	((mplus simp) (($elliptic_e simp) phi m)
	 ((mtimes simp) -1 ((mplus simp) 1 ((mtimes simp) -1 m))
	  (($elliptic_f simp) phi m))))
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

(defprop $elliptic_e
    ((phi m)
     ;; (1-m*sin(phi)^2)
     ((mplus simp) 1 ((mtimes simp) -1 m ((mexpt simp) ((%sin simp) phi) 2)))
     ;; diff wrt m
     ((mtimes simp) ((rat simp) 1 2) ((mexpt simp) m -1)
      ((mplus simp) (($elliptic_e simp) phi m)
       ((mtimes simp) -1 (($elliptic_f simp) phi m)))))
  grad)
		    
(defmfun $elliptic_f (phi m)
  (simplify (list '($elliptic_f) (resimplify phi) (resimplify m))))
(defmfun $elliptic_e (phi m)
  (simplify (list '($elliptic_e) (resimplify phi) (resimplify m))))

(defmfun simp-$elliptic_f (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((phi (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp phi) (floatp m))
	       (and $numer (numberp phi) (numberp m)))
	   ;; Numerically evaluate it
	   (elliptic-f (float phi 1d0) (float m 1d0)))
	  ((zerop1 phi)
	   0)
	  ((zerop1 m)
	   ;; A&S 17.4.19
	   phi)
	  ((onep1 m)
	   ;; A&S 17.4.21.  Let's pick the log tan form.
	   `((%log) ((%tan)
		     ((mplus) ((mtimes) $%pi ((rat) 1 4))
		      ((mtimes) ((rat) 1 2) ,phi)))))
	  ((alike1 phi '((mtimes) ((rat) 1 2) $%pi))
	   ;; Complete elliptic integral
	   `((%elliptic_kc) ,m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '($elliptic_f) phi m) form)))))

(defmfun simp-$elliptic_e (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((phi (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp phi) (floatp m))
	       (and $numer (numberp phi) (numberp m)))
	   ;; Numerically evaluate it
	   (elliptic-e (float phi 1d0) (float m 1d0)))
	  ((zerop1 phi)
	   0)
	  ((zerop1 m)
	   ;; A&S 17.4.23
	   phi)
	  ((onep1 m)
	   ;; A&S 17.4.25
	   `((%sin) ,phi))
	  ((alike1 phi '((mtimes) ((rat) 1 2) $%pi))
	   ;; Complete elliptic integral
	   `((%elliptic_ec) ,m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '($elliptic_e) phi m) form)))))
    

;; Complete elliptic integrals
;;
;; elliptic_kc(m) = elliptic_f(%pi/2, m)
;;
;; elliptic_ec(m) = elliptic_e(%pi/2, m)
;;
(defmfun $elliptic_kc (m)
  (simplify (list '(%elliptic_kc) (resimplify m))))
(defmfun $elliptic_ec (m)
  (simplify (list '(%elliptic_ec) (resimplify m))))


(defprop %elliptic_kc simp-%elliptic_kc operators)
(defprop %elliptic_ec simp-%elliptic_ec operators)

(defmfun simp-%elliptic_kc (form y z)
  (declare (ignore y))
  (oneargcheck form)
  (let ((m (simpcheck (cadr form) z)))
    (cond ((or (and (floatp m))
	       (and $numer (numberp m)))
	   ;; Numerically evaluate it
	   (elliptic-k (float m 1d0)))
	  ((zerop1 m)
	   '((mtimes) ((rat) 1 2) $%pi))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%elliptic_kc) m) form)))))

(defprop %elliptic_kc
    ((m)
     ;; diff wrt m
     ((mtimes)
      ((mplus) ((%elliptic_ec) m)
       ((mtimes) -1
	((%elliptic_kc) m)
	((mplus) 1 ((mtimes) -1 m))))
      ((mexpt) ((mplus) 1 ((mtimes) -1 m)) -1)
      ((mexpt) m -1)))
  grad)

(defmfun simp-%elliptic_ec (form y z)
  (declare (ignore y))
  (oneargcheck form)
  (let ((m (simpcheck (cadr form) z)))
    (cond ((or (and (floatp m))
	       (and $numer (numberp m)))
	   ;; Numerically evaluate it
	   (elliptic-ec (float m 1d0)))
	  ((zerop1 m)
	   '((mtimes) ((rat) 1 2) $%pi))
	  ;; Some special cases we know about.
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%elliptic_ec) m) form)))))

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
(defprop $elliptic_pi simp-$elliptic_pi operators)

(defmfun $elliptic_pi (n phi m)
  (simplify (list '($elliptic_pi)
		  (resimplify n) (resimplify phi) (resimplify m))))

(defmfun simp-$elliptic_pi (form y z)
  (declare (ignore y))
  ;;(threeargcheck form)
  (let ((n (simpcheck (cadr form) z))
	(phi (simpcheck (caddr form) z))
	(m (simpcheck (cadddr form) z)))
    (cond ((or (and (floatp n) (floatp phi) (floatp m))
	       (and $numer (numberp n) (numberp phi) (numberp m)))
	   ;; Numerically evaluate it
	   (elliptic-pi (float n 1d0) (float phi 1d0) (float m 1d0)))
	  ((zerop1 n)
	   `(($elliptic_f) ,phi ,m))
	  ((zerop1 m)
	   ;; 3 cases depending on n < 1, n > 1, or n = 1.
	   (let ((s (asksign `((mplus) -1 ,n))))
	     (case s
	       ($positive
		`((mtimes)
		  ((mexpt) ((mplus) -1 ,n) ((rat) -1 2))
		  ((%atanh)
		   ((mtimes) ((mexpt) ((mplus) -1 ,n) ((rat) 1 2))
		    ((%tan) ,phi)))))
	       ($negative
		`((mtimes)
		  ((mexpt) ((mplus) 1 ((mtimes) -1 ,n)) ((rat) -1 2))
		  ((%atan)
		   ((mtimes) ((mexpt)
			      ((mplus) 1 ((mtimes) -1 ,n))
			      ((rat) 1 2))
		    ((%tan) ,phi)))))
	       ($zero
		`((%tan) ,phi)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '($elliptic_pi) n phi m) form)))))

(defun elliptic-pi (n phi m)
  ;; Note: Carlson's DRJ has n defined as the negative of the n given
  ;; in A&S.
  (let* ((nn (- n))
	(sin-phi (sin phi))
	(cos-phi (cos phi))
	(k (sqrt m))
	(k2sin (* (- 1 (* k sin-phi))
		  (+ 1 (* k sin-phi)))))
    (- (* sin-phi (drf (expt cos-phi 2) k2sin 1d0))
       (* (/ nn 3) (expt sin-phi 3)
	  (drj (expt cos-phi 2) k2sin 1d0
	       (- 1 (* n (expt sin-phi 2))))))))
    
;;***PURPOSE  Calculate a double precision approximation to
;;             DRC(X,Y) = Integral from zero to infinity of
;;                              -1/2     -1
;;                    (1/2)(t+X)    (t+Y)  dt,
;;            where X is nonnegative and Y is positive.
;;   1.     DRC
;;          Standard FORTRAN function routine
;;          Double precision version
;;          The routine calculates an approximation result to
;;          DRC(X,Y) = integral from zero to infinity of
;;
;;                              -1/2     -1
;;                    (1/2)(t+X)    (t+Y)  dt,
;;
;;          where X is nonnegative and Y is positive.  The duplication
;;          theorem is iterated until the variables are nearly equal,
;;          and the function is then expanded in Taylor series to fifth
;;          order.  Logarithmic, inverse circular, and inverse hyper-
;;          bolic functions can be expressed in terms of DRC.
;;
;;   --------------------------------------------------------------------
;;
;;   Special functions via DRC
;;
;;
;;
;;                  LN X                X .GT. 0
;;
;;                                             2
;;                  LN(X) = (X-1) DRC(((1+X)/2)  , X )
;;
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCSIN X            -1 .LE. X .LE. 1
;;
;;                                       2
;;                  ARCSIN X = X DRC (1-X  ,1 )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCCOS X            0 .LE. X .LE. 1
;;
;;
;;                                     2       2
;;                  ARCCOS X = SQRT(1-X ) DRC(X  ,1 )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCTAN X            -INF .LT. X .LT. +INF
;;
;;                                        2
;;                  ARCTAN X = X DRC(1,1+X  )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCCOT X            0 .LE. X .LT. INF
;;
;;                                  2   2
;;                  ARCCOT X = DRC(X  ,X +1 )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCSINH X           -INF .LT. X .LT. +INF
;;
;;                                       2
;;                  ARCSINH X = X DRC(1+X  ,1 )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCCOSH X           X .GE. 1
;;
;;                                    2         2
;;                  ARCCOSH X = SQRT(X -1) DRC(X  ,1 )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCTANH X           -1 .LT. X .LT. 1
;;
;;                                         2
;;                  ARCTANH X = X DRC(1,1-X  )
;;
;;   --------------------------------------------------------------------
;;
;;                  ARCCOTH X           X .GT. 1
;;
;;                                   2   2
;;                  ARCCOTH X = DRC(X  ,X -1 )
;;
;;   --------------------------------------------------------------------
;;
;;***REFERENCES  B. C. Carlson and E. M. Notis, Algorithms for incomplete
;;                 elliptic integrals, ACM Transactions on Mathematical
;;                 Software 7, 3 (September 1981), pp. 398-403.
;;               B. C. Carlson, Computing elliptic integrals by
;;                 duplication, Numerische Mathematik 33, (1979),
;;                 pp. 1-16.
;;               B. C. Carlson, Elliptic integrals of the first kind,
;;                 SIAM Journal of Mathematical Analysis 8, (1977),
;;                 pp. 231-242.


(let ((errtol (expt (/ double-float-epsilon 3) 1/6))
      (c1 (float 1/7 1d0))
      (c2 (float 9/22 1d0)))
  (declare (double-float errtol c1 c2))
  (defun drc (x y)
    (declare (type (double-float 0d0) x)
	     (type (double-float (0d0)) y)
	     (optimize (speed 3)))
    (let ((xn x)
	  (yn y))
      (declare (type (double-float 0d0) xn)
	       (type (double-float (0d0)) yn))
      (loop
	  (let* ((mu (/ (+ xn yn yn) 3))
		 (sn (- (/ (+ yn mu) mu) 2)))
	    (declare (type double-float sn))
	    (when (< (abs sn) errtol)
	      (let ((s (* sn sn
			  (+ 0.3d0
			     (* sn (+ c1 (* sn (+ 0.375d0 (* sn c2)))))))))
		(return-from drc (/ (+ 1 s) (sqrt mu)))))
	    (let ((lam (+ (* 2 (sqrt xn) (sqrt yn)) yn)))
	      (setf xn (* (+ xn lam) 0.25d0))
	      (setf yn (* (+ yn lam) 0.25d0))))))))

;;   1.     DRJ
;;          Standard FORTRAN function routine
;;          Double precision version
;;          The routine calculates an approximation result to
;;          DRJ(X,Y,Z,P) = Integral from zero to infinity of
;;
;;                                -1/2     -1/2     -1/2     -1
;;                      (3/2)(t+X)    (t+Y)    (t+Z)    (t+P)  dt,
;;
;;          where X, Y, and Z are nonnegative, at most one of them is
;;          zero, and P is positive.  If X or Y or Z is zero, the
;;          integral is COMPLETE.  The duplication theorem is iterated
;;          until the variables are nearly equal, and the function is
;;          then expanded in Taylor series to fifth order.
;;
;;
;;    -------------------------------------------------------------------
;;
;;
;;   Special double precision functions via DRJ and DRF
;;
;;
;;                  Legendre form of ELLIPTIC INTEGRAL of 3rd kind
;;                  -----------------------------------------
;;
;;
;;                          PHI         2         -1
;;             P(PHI,K,N) = INT (1+N SIN (THETA) )   *
;;                           0
;;
;;
;;                                  2    2         -1/2
;;                             *(1-K  SIN (THETA) )     D THETA
;;
;;
;;                                           2          2   2
;;                        = SIN (PHI) DRF(COS (PHI), 1-K SIN (PHI),1)
;;
;;                                   3             2         2   2
;;                         -(N/3) SIN (PHI) DRJ(COS (PHI),1-K SIN (PHI),
;;
;;                                  2
;;                         1,1+N SIN (PHI))
;;
;;
;;
;;                  Bulirsch form of ELLIPTIC INTEGRAL of 3rd kind
;;                  -----------------------------------------
;;
;;
;;                                            2 2    2
;;                  EL3(X,KC,P) = X DRF(1,1+KC X ,1+X ) +
;;
;;                                            3           2 2    2     2
;;                               +(1/3)(1-P) X  DRJ(1,1+KC X ,1+X ,1+PX )
;;
;;
;;                                           2
;;                  CEL(KC,P,A,B) = A RF(0,KC ,1) +
;;
;;
;;                                                      2
;;                                 +(1/3)(B-PA) DRJ(0,KC ,1,P)
;;
;;
;;                  Heuman's LAMBDA function
;;                  -----------------------------------------
;;
;;
;;                                2                      2      2    1/2
;;                  L(A,B,P) =(COS (A)SIN(B)COS(B)/(1-COS (A)SIN (B))   )
;;
;;                                            2         2       2
;;                            *(SIN(P) DRF(COS (P),1-SIN (A) SIN (P),1)
;;
;;                                 2       3            2       2
;;                            +(SIN (A) SIN (P)/(3(1-COS (A) SIN (B))))
;;
;;                                    2         2       2
;;                            *DRJ(COS (P),1-SIN (A) SIN (P),1,1-
;;
;;                                2       2          2       2
;;                            -SIN (A) SIN (P)/(1-COS (A) SIN (B))))
;;
;;
;;
;;                  (PI/2) LAMBDA0(A,B) =L(A,B,PI/2) =
;;
;;                   2                         2       2    -1/2
;;              = COS (A)  SIN(B) COS(B) (1-COS (A) SIN (B))
;;
;;                           2                  2       2
;;                 *DRF(0,COS (A),1) + (1/3) SIN (A) COS (A)
;;
;;                                      2       2    -3/2
;;                 *SIN(B) COS(B) (1-COS (A) SIN (B))
;;
;;                           2         2       2          2       2
;;                 *DRJ(0,COS (A),1,COS (A) COS (B)/(1-COS (A) SIN (B)))
;;
;;
;;                  Jacobi ZETA function
;;                  -----------------------------------------
;;
;;                        2                     2   2    1/2
;;             Z(B,K) = (K/3) SIN(B) COS(B) (1-K SIN (B))
;;
;;
;;                                  2      2   2                 2
;;                        *DRJ(0,1-K ,1,1-K SIN (B)) / DRF (0,1-K ,1)
;;
;;
;;
;;***REFERENCES  B. C. Carlson and E. M. Notis, Algorithms for incomplete
;;                 elliptic integrals, ACM Transactions on Mathematical
;;                 Software 7, 3 (September 1981), pp. 398-403.
;;               B. C. Carlson, Computing elliptic integrals by
;;                 duplication, Numerische Mathematik 33, (1979),
;;                 pp. 1-16.
;;               B. C. Carlson, Elliptic integrals of the first kind,
;;                 SIAM Journal of Mathematical Analysis 8, (1977),
;;                 pp. 231-242.
(let ((errtol (expt (/ double-float-epsilon 3) 1/6))
      (c1 3/14)
      (c2 1/3)
      (c3 3/22)
      (c4 3/26))
  (defun drj (x y z p)
    (declare (type (double-float 0d0) x y z)
	     (type (double-float (0d0)) p))
    (let ((xn x)
	  (yn y)
	  (zn z)
	  (pn p)
	  (sigma 0d0)
	  (power4 1d0))
      (declare (type (double-float 0d0) xn yn zn)
	       (type (double-float (0d0)) pn)
	       (type double-float sigma power4))
      (loop
	  (let* ((mu (* 0.2d0 (+ xn yn zn pn pn)))
		 (xndev (/ (- mu xn) mu))
		 (yndev (/ (- mu yn) mu))
		 (zndev (/ (- mu zn) mu))
		 (pndev (/ (- mu pn) mu))
		 (eps (max (abs xndev) (abs yndev) (abs zndev) (abs pndev))))
	    (when (< eps errtol)
	      (let* ((ea (+ (* xndev (+ yndev zndev))
			    (* yndev zndev)))
		     (eb (* xndev yndev zndev))
		     (ec (* pndev pndev))
		     (e2 (- ea (* 3 ec)))
		     (e3 (+ eb (* 2 pndev (- ea ec))))
		     (s1 (+ 1 (* e2 (+ (- c1)
				       (* 3/4 c3 e2)
				       (* -3/2 c4 e3)))))
		     (s2 (* eb (+ (* 1/2 c2)
				  (* pndev (+ (- c3) (- c3)
					      (* pndev c4))))))
		     (s3 (- (* pndev ea (- c2 (* pndev c3)))
			    (* c2 pndev ec))))
		(return-from drj (+ (* 3 sigma)
				    (/ (* power4 (+ s1 s2 s3))
				       (* mu (sqrt mu)))))))
	    (let* ((xnroot (sqrt xn))
		   (ynroot (sqrt yn))
		   (znroot (sqrt zn))
		   (lam (+ (* xnroot (+ ynroot znroot))
			   (* ynroot znroot)))
		   (alfa (expt (+ (* pn (+ xnroot ynroot znroot))
				  (* xnroot ynroot znroot))
			       2))
		   (beta (* pn (expt (+ pn lam) 2))))
	      (incf sigma (* power4 (drc alfa beta)))
	      (setf power4 (* power4 0.25d0))
	      (setf xn (* 0.25d0 (+ xn lam)))
	      (setf yn (* 0.25d0 (+ yn lam)))
	      (setf zn (* 0.25d0 (+ zn lam)))
	      (setf pn (* 0.25d0 (+ pn lam)))))))))

(defun check-drj (x y z p)
  (let* ((w (/ (* x y) z))
	 (a (* p p (+ x y z w)))
	 (b (* p (+ p x) (+ p y)))
	 (drj-1 (drj x (+ x z) (+ x w) (+ x p)))
	 (drj-2 (drj y (+ y z) (+ y w) (+ y p)))
	 (drj-3 (drj a b b a))
	 (drj-4 (drj 0d0 z w p)))
    ;; Both values should be equal.
    (values (+ drj-1 drj-2 (* (- a b) drj-3) (/ 3 (sqrt a)))
	    drj-4)))

;;; Other Jacobian elliptic functions

;; jacobi_ns(u,m) = 1/jacobi_sn(u,m)
(defmfun  $jacobi_ns (u m)
  (simplify (list '(%jacobi_ns) (resimplify u) (resimplify m))))

(defprop %jacobi_ns simp-%jacobi_ns operators)

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
	  (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	   m)))))))
  grad)

(defmfun simp-%jacobi_ns (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate sn
	   (/ (sn (float u 1d0) (float m 1d0))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (sn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 m)
	   ;; A&S 16.6.10
	   `(($csc) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.10
	   `(($coth) ,u))
	  ((zerop1 u)
	   (dbz-err1 'jacobi_ns))
	  ((and $trigsign (mminusp* u))
	   ;; ns is odd
	   (neg (cons-exp '%jacobi_ns (neg u) m)))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; ns(i*u) = 1/sn(i*u) = -i/sc(u,m1) = -i*cs(u,m1)
	   (neg (mul '$%i
		     (cons-exp '%jacobi_cs (coeff u '$%i 1) (add 1 (neg m))))))
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
			   `((%jacobi_ns simp) ,const ,m)))
		      (1
		       ;; ns(4*m*K + K + u) = ns(K+u) = dc(u)
		       ;; ns(K) = 1
		       (if (zerop1 const)
			   1
			   `((%jacobi_dc simp) ,const ,m)))
		      (2
		       ;; ns(4*m*K+2*K + u) = ns(2*K+u) = -ns(u)
		       ;; ns(2*K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_ns)
			   (neg `((%jacobi_ns simp) ,const ,m))))
		      (3
		       ;; ns(4*m*K+3*K+u) = ns(2*K + K + u) = -ns(K+u) = -dc(u)
		       ;; ns(3*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_dc simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    `((mexpt) ((%jacobi_sn) ,u ,m) -1))
		   (t
		    (eqtest (list '(%jacobi_ns) u m) form)))))	  
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_ns) u m) form)))))

;; jacobi_nc(u,m) = 1/jacobi_cn(u,m)

(defmfun  $jacobi_nc (u m)
  (simplify (list '(%jacobi_nc) (resimplify u) (resimplify m))))

(defprop %jacobi_nc simp-%jacobi_nc operators)

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
	  (($elliptic_e) ((%asin) ((%jacobi_sn) u m)) m)))))))
  grad)

(defmfun simp-%jacobi_nc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (/ (cn (float u 1d0) (float m 1d0))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (cn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.8
	   `(($sec) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.8
	   `((%cosh) ,u))
	  ((and $trigsign (mminusp* u))
	   ;; nc is even
	   (cons-exp '%jacobi_nc (neg u) m))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; nc(i*u) = 1/cn(i*u) = 1/nc(u,1-m) = cn(u,1-m)
	   (cons-exp '%jacobi_cn (coeff u '$%i 1) (add 1 (neg m))))
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
			   `((%jacobi_nc simp) ,const ,m)))
		      (1
		       ;; nc(4*m*K+K+u) = nc(K+u) = -ds(u)/sqrt(1-m)
		       ;; nc(K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_nc)
			   (neg `((mtimes simp)
				  ((mexpt simp)
				   ((mplus simp) 1 ((mtimes simp) -1 ,m))
				   ((rat simp) -1 2))
				  ((%jacobi_ds simp) ,const ,m)))))
		      (2
		       ;; nc(4*m*K+2*K+u) = nc(2*K+u) = -nc(u)
		       ;; nc(2*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_nc) ,const ,m))))
		      (3
		       ;; nc(4*m*K+3*K+u) = nc(3*K+u) = nc(2*K+K+u) =
		       ;; -nc(K+u) = ds(u)/sqrt(1-m)
		       ;;
		       ;; nc(3*K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_nc)
			   `((mtimes simp)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) -1 2))
			     ((%jacobi_ds simp) ,const ,m))))))
		   ((and (alike1 1//2 lin)
			 (zerop1 const))
		    `((mexpt) ((%jacobi_cn) ,u ,m) -1))
		   (t
		    (eqtest (list '(%jacobi_cn) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_nc) u m) form)))))

;; jacobi_nd(u,m) = 1/jacobi_dn(u,m)
(defmfun  $jacobi_nd (u m)
  (simplify (list '(%jacobi_nd) (resimplify u) (resimplify m))))

(defprop %jacobi_nd simp-%jacobi_nd operators)

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
	  (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	   m)))))))
  grad)

(defmfun simp-%jacobi_nd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (/ (dn (float u 1d0) (float m 1d0))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (dn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.6
	   1)
	  ((onep1 m)
	   ;; A&S 16.6.6
	   `((%cosh) ,u))
	  ((and $trigsign (mminusp* u))
	   ;; nd is even
	   (cons-exp '%jacobi_nd (neg u) m))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; nd(i*u) = 1/dn(i*u) = 1/dc(u,1-m) = cd(u,1-m)
	   (cons-exp '%jacobi_cd (coeff u '$%i 1) (add 1 (neg m))))
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
			   `((%jacobi_nd) ,const ,m)))
		      (1
		       ;; nd(2*m*K+K+u) = nd(K+u) = dn(u)/sqrt(1-m)
		       ;; nd(K) = 1/sqrt(1-m)
		       (if (zerop1 const)
			   `((mexpt simp)
			     ((mplus simp) 1 ((mtimes simp) -1 ,m))
			     ((rat simp) -1 2))
			   `((mtimes simp)
			     ((%jacobi_nd simp) ,const ,m)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) -1 2)))))))
		   (t
		    (eqtest (list '(%jacobi_nd) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_nd) u m) form)))))

;; jacobi_sc(u,m) = jacobi_sn/jacobi_cn
(defmfun  $jacobi_sc (u m)
  (simplify (list '(%jacobi_sc) (resimplify u) (resimplify m))))

(defprop %jacobi_sc simp-%jacobi_sc operators)

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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(defmfun simp-%jacobi_sc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (sn fu fm) (cn fu fm))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (sn (complex u-r u-i) (complex m-r m-i))
			    (cn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 u)
	   0)
	  ((zerop1 m)
	   ;; A&S 16.6.9
	   `((%tan) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.9
	   `((%sinh) ,u))
	  ((and $trigsign (mminusp* u))
	   ;; sc is odd
	   (neg (cons-exp '%jacobi_sc (neg u) m)))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; sc(i*u) = sn(i*u)/cn(i*u) = i*sc(u,m1)/nc(u,m1) = i*sn(u,m1)
	   (mul '$%i
		(cons-exp '%jacobi_sn (coeff u '$%i 1) (add 1 (neg m)))))
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
			   `((%jacobi_sc simp) ,const ,m)))
		      (1
		       ;; sc(2*m*K + K + u) = sc(K+u)= - cs(u)/sqrt(1-m)
		       ;; sc(K) = infinity
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_sc)
			   `((mtimes simp) -1
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) -1 2))
			     ((%jacobi_cs simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; (1-m)^(1/4)
		    `((mexpt simp)
		      ((mplus simp) 1 ((mtimes simp) -1 ,m))
		      ((rat simp) 1 4)))
		   (t
		    (eqtest (list '(%jacobi_sc) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_sc) u m) form)))))

;; jacobi_sd(u,m) = jacobi_sn/jacobi_dn
(defmfun  $jacobi_sd (u m)
  (simplify (list '(%jacobi_sd) (resimplify u) (resimplify m))))

(defprop %jacobi_sd simp-%jacobi_sd operators)

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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(defmfun simp-%jacobi_sd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (sn fu fm) (dn fu fm))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (sn (complex u-r u-i) (complex m-r m-i))
			    (dn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 u)
	   0)
	  ((zerop1 m)
	   ;; A&S 16.6.5
	   `((%sin) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.5
	   `((%sinh) ,u))
	  ((and $trigsign (mminusp* u))
	   ;; sd is odd
	   (neg (cons-exp '%jacobi_sd (neg u) m)))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; sd(i*u) = sn(i*u)/dn(i*u) = i*sc(u,m1)/dc(u,m1) = i*sd(u,m1)
	   (mul '$%i
		(cons-exp '%jacobi_sd (coeff u '$%i 1) (add 1 (neg m)))))
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
			   `((%jacobi_sd simp) ,const ,m)))
		      (1
		       ;; sd(4*m*K+K+u) = sd(K+u) = cn(u)/sqrt(1-m)
		       ;; sd(K) = 1/sqrt(m1)
		       (if (zerop1 const)
			   `((mexpt) ((mplus) 1 ((mtimes) -1 ,m))
			     ((rat) -1 2))
			   `((mtimes simp)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) -1 2))
			     ((%jacobi_cn simp) ,const ,m))))
		      (2
		       ;; sd(4*m*K+2*K+u) = sd(2*K+u) = -sd(u)
		       ;; sd(2*K) = 0
		       (if (zerop1 const)
			   0
			   (neg `((%jacobi_sd) ,const ,m))))
		      (3
		       ;; sd(4*m*K+3*K+u) = sd(3*K+u) = sd(2*K+K+u) =
		       ;; -sd(K+u) = -cn(u)/sqrt(1-m)
		       ;; sd(3*K) = -1/sqrt(m1)
		       (if (zerop1 const)
			   (neg `((mexpt)
				  ((mplus simp) 1 ((mtimes simp) -1 ,m))
				  ((rat) -1 2)))
			   (neg `((mtimes simp)
				  ((mexpt simp)
				   ((mplus simp) 1 ((mtimes simp) -1 ,m))
				   ((rat simp) -1 2))
				  ((%jacobi_cn simp) ,const ,m)))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; jacobi_sn/jacobi_dn
		    `((mtimes)
		      ((%jacobi_sn) ((mtimes) ((rat) 1 2)
				     ((%elliptic_kc) ,m))
		       ,m)
		      ((mexpt)
		       ((%jacobi_dn) ((mtimes) ((rat) 1 2)
				      ((%elliptic_kc) ,m))
			,m)
		       -1)))
		   (t
		    (eqtest (list '(%jacobi_sd) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_sd) u m) form)))))

;; jacobi_cs(u,m) = jacobi_cn/jacobi_sn
(defmfun  $jacobi_cs (u m)
  (simplify (list '(%jacobi_cs) (resimplify u) (resimplify m))))

(defprop %jacobi_cs simp-%jacobi_cs operators)

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
      (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
      (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
       m))))))))
  grad)

(defmfun simp-%jacobi_cs (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (cn fu fm) (sn fu fm))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (cn (complex u-r u-i) (complex m-r m-i))
			    (sn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 m)
	   ;; A&S 16.6.12
	   `(($cot) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.12
	   `(($csch) ,u))
	  ((zerop1 u)
	   (dbz-err1 'jacobi_cs))
	  ((and $trigsign (mminusp* u))
	   ;; cs is odd
	   (neg (cons-exp '%jacobi_cs (neg u) m)))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; cs(i*u) = cn(i*u)/sn(i*u) = -i*nc(u,m1)/sc(u,m1) = -i*ns(u,m1)
	   (neg (mul '$%i
		     (cons-exp '%jacobi_ns (coeff u '$%i 1) (add 1 (neg m))))))
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
			   `((%jacobi_cs simp) ,const ,m)))
		      (1
		       ;; cs(K+u) = -sqrt(1-m)*sc(u)
		       ;; cs(K) = 0
		       (if (zerop1 const)
			   0
			   `((mtimes simp) -1
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) 1 2))
			     ((%jacobi_sc simp) ,const ,m))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; 1/jacobi_sc
		    `((mexpt)
		      ((%jacobi_sc) ((mtimes) ((rat) 1 2)
				     ((%elliptic_kc) ,m)) ,m)
		      -1))
		   (t
		    (eqtest (list '(%jacobi_cs simp) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_cs simp) u m) form)))))

;; jacobi_cd(u,m) = jacobi_cn/jacobi_dn
(defmfun  $jacobi_cd (u m)
  (simplify (list '(%jacobi_cd) (resimplify u) (resimplify m))))

(defprop %jacobi_cd simp-%jacobi_cd operators)

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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(defmfun simp-%jacobi_cd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (cn fu fm) (dn fu fm))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (cn (complex u-r u-i) (complex m-r m-i))
			    (dn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.4
	   `((%cos) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.4
	   1)
	  ((and $trigsign (mminusp* u))
	   ;; cd is even
	   (cons-exp '%jacobi_cd (neg u) m))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; cd(i*u) = cn(i*u)/dn(i*u) = nc(u,m1)/dc(u,m1) = nd(u,m1)
	   (cons-exp '%jacobi_nd (coeff u '$%i 1) (add 1 (neg m))))
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
			   `((%jacobi_cd) ,const ,m)))
		      (1
		       ;; cd(4*m*K + K + u) = cd(K+u) = -sn(u)
		       ;; cd(K) = 0
		       (if (zerop1 const)
			   0
			   (neg `((%jacobi_sn) ,const ,m))))
		      (2
		       ;; cd(4*m*K + 2*K + u) = cd(2*K+u) = -cd(u)
		       ;; cd(2*K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_cd) ,const ,m))))
		      (3
		       ;; cd(4*m*K + 3*K + u) = cd(2*K + K + u) =
		       ;; -cd(K+u) = sn(u)
		       ;; cd(3*K) = 0
		       (if (zerop1 const)
			   0
			   `((%jacobi_sn) ,const ,m)))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; jacobi_cn/jacobi_dn
		    `((mtimes)
		      ((%jacobi_cn) ((mtimes) ((rat) 1 2)
				     ((%elliptic_kc) ,m))
		       ,m)
		      ((mexpt)
		       ((%jacobi_dn) ((mtimes) ((rat) 1 2)
				      ((%elliptic_kc) ,m))
			,m)
		       -1)))
		   (t
		    ;; Nothing to do
		    (eqtest (list '(%jacobi_cd) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_cd) u m) form)))))

;; jacobi_ds(u,m) = jacobi_dn/jacobi_sn
(defmfun  $jacobi_ds (u m)
  (simplify (list '(%jacobi_ds) (resimplify u) (resimplify m))))

(defprop %jacobi_ds simp-%jacobi_ds operators)

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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(defmfun simp-%jacobi_ds (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (dn fu fm) (sn fu fm))))
	  ((and $numer (complex-number-p u)
		(complex-number-p m))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m-r ($realpart m))
		 (m-i ($imagpart m)))
	     (complexify (/ (dn (complex u-r u-i) (complex m-r m-i))
			    (sn (complex u-r u-i) (complex m-r m-i))))))
	  ((zerop1 m)
	   ;; A&S 16.6.11
	   `(($csc) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.11
	   `(($csch) ,u))
	  ((zerop1 u)
	   (dbz-err1 'jacobi_ds))
	  ((and $trigsign (mminusp* u))
	   (neg (cons-exp '%jacobi_ds (neg u) m)))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; ds(i*u) = dn(i*u)/sn(i*u) = -i*dc(u,m1)/sc(u,m1) = -i*ds(u,m1)
	   (neg (mul '$%i
		     (cons-exp '%jacobi_ds (coeff u '$%i 1) (add 1 (neg m))))))
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
			   `((%jacobi_ds) ,const ,m)))
		      (1
		       ;; ds(4*m*K + K + u) = ds(K+u) = sqrt(1-m)*nc(u)
		       ;; ds(K) = sqrt(1-m)
		       (if (zerop1 const)
			   `((mexpt simp)
			     ((mplus simp) 1 ((mtimes simp) -1 ,m))
			     ((rat simp) 1 2))
			   `((mtimes simp)
			     ((mexpt simp)
			      ((mplus simp) 1 ((mtimes simp) -1 ,m))
			      ((rat simp) 1 2))
			     ((%jacobi_nc simp) ,const ,m))))
		      (2
		       ;; ds(4*m*K + 2*K + u) = ds(2*K+u) = -ds(u)
		       ;; ds(0) = pole
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_ds)
			   (neg `((%jacobi_ds) ,const ,m))))
		      (3
		       ;; ds(4*m*K + 3*K + u) = ds(2*K + K + u) =
		       ;; -ds(K+u) = -sqrt(1-m)*nc(u)
		       ;; ds(3*K) = -sqrt(1-m)
		       (if (zerop1 const)
			   (neg `((mexpt simp)
				  ((mplus simp) 1 ((mtimes simp) -1 ,m))
				  ((rat simp) 1 2)))
			   (neg `((mtimes simp)
				  ((mexpt simp)
				   ((mplus simp) 1 ((mtimes simp) -1 ,m))
				   ((rat simp) 1 2))
				  ((%jacobi_nc simp) ,const ,m)))))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; jacobi_dn/jacobi_sn
		    `((mtimes)
		      ((%jacobi_dn) ((mtimes) ((rat) 1 2)
				     ((%elliptic_kc) ,m))
		       ,m)
		      ((mexpt)
		       ((%jacobi_sn) ((mtimes) ((rat) 1 2)
				      ((%elliptic_kc) ,m))
			,m)
		       -1)))
		   (t
		    ;; Nothing to do
		    (eqtest (list '(%jacobi_ds) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_ds) u m) form)))))

;; jacobi_dc(u,m) = jacobi_dn/jacobi_cn
(defmfun  $jacobi_dc (u m)
  (simplify (list '(%jacobi_dc) (resimplify u) (resimplify m))))

(defprop %jacobi_dc simp-%jacobi_dc operators)

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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
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
	   (($elliptic_e) ((%asin) ((%jacobi_sn) u m))
	    m))))))))
  grad)

(defmfun simp-%jacobi_dc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z))
	coef)
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((fu (float u 1d0))
		 (fm (float m 1d0)))
	     (/ (dn fu fm) (cn fu fm))))
	  ((zerop1 u)
	   1)
	  ((zerop1 m)
	   ;; A&S 16.6.7
	   `(($sec) ,u))
	  ((onep1 m)
	   ;; A&S 16.6.7
	   1)
	  ((and $trigsign (mminusp* u))
	   (cons-exp '%jacobi_dc (neg u) m))
	  ;; A&S 16.20 (Jacobi's Imaginary transformation)
	  ((and $%iargs (multiplep u '$%i))
	   ;; dc(i*u) = dn(i*u)/cn(i*u) = dc(u,m1)/nc(u,m1) = dn(u,m1)
	   (cons-exp '%jacobi_dn (coeff u '$%i 1) (add 1 (neg m))))
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
			   `((%jacobi_dc) ,const ,m)))
		      (1
		       ;; dc(4*m*K + K + u) = dc(K+u) = -ns(u)
		       ;; dc(K) = pole
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_dc)
			   (neg `((%jacobi_ns simp) ,const ,m))))
		      (2
		       ;; dc(4*m*K + 2*K + u) = dc(2*K+u) = -dc(u)
		       ;; dc(2K) = -1
		       (if (zerop1 const)
			   -1
			   (neg `((%jacobi_dc) ,const ,m))))
		      (3
		       ;; dc(4*m*K + 3*K + u) = dc(2*K + K + u) =
		       ;; -dc(K+u) = ns(u)
		       ;; dc(3*K) = ns(0) = inf
		       (if (zerop1 const)
			   (dbz-err1 'jacobi_dc)
			   `((%jacobi_dc simp) ,const ,m)))))
		   ((and (alike1 lin 1//2)
			 (zerop1 const))
		    ;; jacobi_dn/jacobi_cn
		    `((mtimes)
		      ((%jacobi_dn) ((mtimes) ((rat) 1 2)
				     ((%elliptic_kc) ,m))
		       ,m)
		      ((mexpt)
		       ((%jacobi_cn) ((mtimes) ((rat) 1 2)
				      ((%elliptic_kc) ,m))
			,m)
		       -1)))
		   (t
		    ;; Nothing to do
		    (eqtest (list '(%jacobi_dc) u m) form)))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_dc) u m) form)))))

;;; Other inverse Jacobian functions

;; inverse_jacobi_ns(x)
;;
;; Let u = inverse_jacobi_ns(x).  Then jacobi_ns(u) = x or
;; 1/jacobi_sn(u) = x or
;;
;; jacobi_sn(u) = 1/x
;;
;; so u = inverse_jacobi_sn(1/x)

(defmfun $inverse_jacobi_ns (u m)
  (simplify (list '(%inverse_jacobi_ns) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_ns
    ((x m)
     ;; -1/sqrt(1-x^2)/sqrt(x^2+m-1)
     ((mtimes) -1
      ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) -1 2))
      ((mexpt)
       ((mplus) ((mtimes simp ratsimp) -1 m) ((mexpt) x 2))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_ns) x m) m 1))
  grad)

(defprop %inverse_jacobi_ns simp-%inverse_jacobi_ns operators)

(defmfun simp-%inverse_jacobi_ns (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate asn
	   ;;
	   ;; ans(x,m) = asn(1/x,m) = F(asin(1/x),m)
	   (elliptic-f (lisp:asin (/ u)) m))
	  ((zerop1 m)
	   ;; ans(x,0) = F(asin(1/x),0) = asin(1/x)
	   `((%elliptic_f) ((%asin) ((mexpt) ,u -1)) 0))
	  ((onep1 m)
	   ;; ans(x,1) = F(asin(1/x),1) = log(tan(pi/2+asin(1/x)/2))
	   `((%elliptic_f) ((%asin) ((mexpt) ,u -1)) 1))
	  ((onep1 u)
	   `((%elliptic_kc) ,m))
	  ((alike1 u -1)
	   (neg `((%elliptic_kc) ,m)))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_ns) u m) form)))))

;; inverse_jacobi_nc(x)
;;
;; Let u = inverse_jacobi_nc(x).  Then jacobi_nc(u) = x or
;; 1/jacobi_cn(u) = x or
;;
;; jacobi_cn(u) = 1/x
;;
;; so u = inverse_jacobi_cn(1/x)

(defmfun $inverse_jacobi_nc (u m)
  (simplify (list '(%inverse_jacobi_nc) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_nc
    ((x m)
     ;; -1/sqrt(1-x^2)/sqrt(x^2+m-1)
     ((mtimes)
      ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) -1 2))
      ((mexpt)
       ((mplus) ((mtimes simp ratsimp) -1 m) ((mexpt) x 2))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_nc) x m) m 1))
  grad)

(defprop %inverse_jacobi_nc simp-%inverse_jacobi_nc operators)

(defmfun simp-%inverse_jacobi_nc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;;
	   ($inverse_jacobi_cn (/ u) m))
	  ((onep1 u)
	   0)
	  ((alike1 u -1)
	   `((mtimes) 2 ((%elliptic_kc) ,m)))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_nc) u m) form)))))

;; inverse_jacobi_nd(x)
;;
;; Let u = inverse_jacobi_nd(x).  Then jacobi_nd(u) = x or
;; 1/jacobi_dn(u) = x or
;;
;; jacobi_dn(u) = 1/x
;;
;; so u = inverse_jacobi_dn(1/x)

(defmfun $inverse_jacobi_nd (u m)
  (simplify (list '(%inverse_jacobi_nd) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_nd
    ((x m)
     ;; -1/sqrt(1-x^2)/sqrt(x^2+m-1)
     ((mtimes) -1
      ((mexpt) ((mplus) -1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1
	((mtimes) ((mplus) -1 m) ((mexpt simp ratsimp) x 2)))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_nd) x m) m 1))
  grad)

(defprop %inverse_jacobi_nd simp-%inverse_jacobi_nd operators)

(defmfun simp-%inverse_jacobi_nd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_dn (/ u) m))
	  ((onep1 u)
	   `((%elliptic_kc) ,m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_nd) u m) form)))))

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

(defmfun $inverse_jacobi_sc (u m)
  (simplify (list '(%inverse_jacobi_sc) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_sc
    ((x m)
     ((mtimes)
      ((mexpt) ((mplus) 1 ((mexpt) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1
	((mtimes) -1 ((mplus) -1 m) ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_sc) x m) m 1))
  grad)

(defprop %inverse_jacobi_sc simp-%inverse_jacobi_sc operators)

(defmfun simp-%inverse_jacobi_sc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_sn (/ u (sqrt (+ 1 (* u u)))) m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_sc) u m) form)))))

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

(defmfun $inverse_jacobi_sd (u m)
  (simplify (list '(%inverse_jacobi_sd) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_sd
    ((x m)
     ((mtimes)
      ((mexpt)
       ((mplus) 1 ((mtimes) ((mplus) -1 m) ((mexpt) x 2)))
       ((rat) -1 2))
      ((mexpt) ((mplus) 1 ((mtimes) m ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_sd) x m) m 1))
  grad)

(defprop %inverse_jacobi_sd simp-%inverse_jacobi_sd operators)

(defmfun simp-%inverse_jacobi_sd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_sn (/ u (sqrt (+ 1 (* m u u)))) m))
	  ((zerop1 u)
	   0)
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_sd) u m) form)))))

;; inverse_jacobi_cs(x)
;;
;; Let u = inverse_jacobi_cs(x).  Then jacobi_cs(u) = x or
;; 1/x = 1/jacobi_cs(u) = jacobi_sc(u)
;;
;; u = inverse_sc(1/x)
;;

(defmfun $inverse_jacobi_cs (u m)
  (simplify (list '(%inverse_jacobi_cs) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_cs
    ((x m)
     ((mtimes) -1
      ((mexpt) ((mplus) 1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt) ((mplus) 1
		     ((mtimes simp ratsimp) -1 m)
		     ((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_cs) x m) m 1))
  grad)

(defprop %inverse_jacobi_cs simp-%inverse_jacobi_cs operators)

(defmfun simp-%inverse_jacobi_cs (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_sc (/ u) m))
	  ((zerop1 u)
	   `((%elliptic_kc) ,m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_cs) u m) form)))))

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

(defmfun $inverse_jacobi_cd (u m)
  (simplify (list '(%inverse_jacobi_cd) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_cd
    ((x m)
     ((mtimes)
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 ((mexpt) x 2)))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) 1 ((mtimes) -1 m ((mexpt) x 2)))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_cd) x m) m 1))
  grad)

(defprop %inverse_jacobi_cd simp-%inverse_jacobi_cd operators)

(defmfun simp-%inverse_jacobi_cd (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_sn (/ (sqrt (* (- 1 u) (+ 1 u)))
				  (sqrt (- 1 (* m u u)))) m))
	  ((onep1 u)
	   0)
	  ((zerop1 u)
	   `((%elliptic_kc) ,m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_cd) u m) form)))))

;; inverse_jacobi_ds(x)
;;
;; Let u = inverse_jacobi_ds(x).  Then jacobi_ds(u) = x or
;; 1/x = 1/jacobi_ds(u) = jacobi_sd(u)
;;
;; u = inverse_sd(1/x)
;;

(defmfun $inverse_jacobi_ds (u m)
  (simplify (list '(%inverse_jacobi_ds) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_ds
    ((x m)
     ((mtimes) -1
      ((mexpt)
       ((mplus) -1 m ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus) m ((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
      ;; wrt m
     ((%derivative) ((%inverse_jacobi_ds) x m) m 1))
  grad)

(defprop %inverse_jacobi_ds simp-%inverse_jacobi_ds operators)

(defmfun simp-%inverse_jacobi_ds (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_sd (/ u) m))
	  ((and $trigsign (mminusp* u))
	   (neg (cons-exp '%inverse_jacobi_ds (neg u) m)))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_ds) u m) form)))))


;; inverse_jacobi_dc(x)
;;
;; Let u = inverse_jacobi_dc(x).  Then jacobi_dc(u) = x or
;; 1/x = 1/jacobi_dc(u) = jacobi_cd(u)
;;
;; u = inverse_cd(1/x)
;;

(defmfun $inverse_jacobi_dc (u m)
  (simplify (list '(%inverse_jacobi_dc) (resimplify u) (resimplify m))))

(defprop %inverse_jacobi_dc
    ((x m)
     ((mtimes) -1
      ((mexpt)
       ((mplus) -1 ((mexpt simp ratsimp) x 2))
       ((rat) -1 2))
      ((mexpt)
       ((mplus)
	((mtimes simp ratsimp) -1 m)
	((mexpt simp ratsimp) x 2))
       ((rat) -1 2)))
     ;; wrt m
     ((%derivative) ((%inverse_jacobi_dc) x m) m 1))
  grad)

(defprop %inverse_jacobi_dc simp-%inverse_jacobi_dc operators)

(defmfun simp-%inverse_jacobi_dc (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ($inverse_jacobi_cd (/ u) m))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%inverse_jacobi_dc) u m) form)))))

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
	      `(($elliptic_f) ((%atan) ,u) ,m))
	     (%inverse_jacobi_cs
	      ;; A&S 17.4.42
	      `(($elliptic_f) ((%atan) ((mexpt) ,u -1)) ,m))
	     (%inverse_jacobi_nd
	      ;; A&S 17.4.43
	      `(($elliptic_f)
		((%asin) ((mtimes)
			  ((mexpt) ,m ((rat) -1 2))
			  ((mexpt) ,u -1)
			  ((mexpt) ((mplus) -1 ((mexpt) ,u 2))
			   ((rat) 1 2))))
		,m))
	     (%inverse_jacobi_dn
	      ;; A&S 17.4.44
	      `(($elliptic_f)
		((%asin)
		 ((mtimes) ((mexpt) ,m ((rat) -1 2))
		  ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) ,u 2)))
		   ((rat) 1 2))))
		,m))
	     (%inverse_jacobi_sn
	      ;; A&S 17.4.45
	      `(($elliptic_f) ((%asin) ,u) ,m))
	     (%inverse_jacobi_cd
	      ;; A&S 17.4.46
	      `(($elliptic_f)
		((%asin)
		 ((mexpt) ((mtimes) ((mplus) 1
				     ((mtimes) -1 ((mexpt) ,u 2)))
			   ((mexpt) ((mplus) 1
				     ((mtimes) -1 ,m ((mexpt) ,u 2)))
			    -1))
		  ((rat) 1 2)))
		,m))
	     (%inverse_jacobi_dc
	      ;; A&S 17.4.47
	      `(($elliptic_f)
		((%asin)
		 ((mexpt)
		  ((mtimes) ((mplus) -1 ((mexpt) ,u 2))
		   ((mexpt) ((mplus) ((mtimes) -1 ,m) ((mexpt) ,u 2)) -1))
		  ((rat) 1 2)))
		,m))
	     (%inverse_jacobi_ns
	      ;; A&S 17.4.48
	      `(($elliptic_f) ((asin) ((mexpt) ,u -1)) ,m))
	     (%inverse_jacobi_nc
	      ;; A&S 17.4.49
	      `(($elliptic_f) ((acos) ((mexpt) ,u -1)) ,m))
	     (%inverse_jacobi_ds
	      ;; A&S 17.4.50
	      `(($elliptic_f)
		((%asin) ((mexpt) ((mplus) ,m ((mexpt) ,u 2))
			  ((rat) -1 2)))
		,m))
	     (%inverse_jacobi_sd
	      ;; A&S 17.4.51
	      `(($elliptic_f)
		((%asin)
		 ((mtimes) ,u
		  ((mexpt) ((mplus) 1 ((mtimes) ,m ((mexpt) ,u 2)))
		   ((rat) -1 2))))
		,m))
	     (%inverse_jacobi_cn
	      ;; A&S 17.4.52
	      `(($elliptic_f) ((%acos) ,u) ,m)))))
	(t
	 (recur-apply #'make-elliptic-f e))))

(defmfun $make_elliptic_f (e)
  (if (atom e)
      e
      (simplify (make-elliptic-f e))))

(defun make-elliptic-e (e)
  (cond ((atom e)
	 e)
	((eq (caar e) '$elliptic_eu)
	 (destructuring-bind ((fn &rest ops) u m)
	     e
	   (declare (ignore fn ops))
	   `(($elliptic_e) ((%asin) ((%jacobi_sn) ,u ,m)) ,m)))
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
	 (let ((ell-k (elliptic-k m))
	       (ell-e (elliptic-ec m)))
	   (multiple-value-bind (n u-rem)
	       (floor u (* 2 ell-k))
	     ;; 0 <= u-rem < 2*K
	     (+ (* 2 n ell-e)
		(cond ((>= u-rem ell-k)
		       ;; 0 <= u-rem < K so
		       ;; E(u + K) = E(u) + E - m*sn(u)*cd(u)
		       (let ((u-k (- u ell-k)))
			 (- (+ (elliptic-e (asin (sn u-k m)) m)
			       ell-e)
			    (/ (* m (sn u-k m) (cn u-k m))
			       (dn u-k m)))))
		      (t
		       (elliptic-e (asin (sn u m)) m)))))))
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
	      (* #C(0 1)
		 (- (+ u-i
		       (/ (* (sn u-i m1) (dn u-i m1))
			  (cn u-i m1)))
		    (+ (elliptic-eu u-i m1)
		       (/ (* m (sn u-r m) (sn u-i m1) (sn u m))
			  (cn u-i m1))))))))))

(defprop $elliptic_eu simp-$elliptic_eu operators)
(defprop $elliptic_eu
    ((u m)
     ((mexpt) ((%jacobi_dn) u m) 2)
     ;; wrt m
     )
  grad)

(defmfun simp-$elliptic_eu (form y z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   (let ((u-r ($realpart u))
		 (u-i ($imagpart u))
		 (m (float m 1d0)))
	     (complexify (elliptic-eu (complex u-r u-i) m))))
	  (t
	   (eqtest `(($elliptic_eu) ,u ,m) form)))))

(defmfun $elliptic_eu (u m)
  (simplify `(($elliptic_eu) ,(resimplify u) ,(resimplify m))))

(defun agm (a0 b0 phi)
  (let (c0)
    (dotimes (k 16)
      (psetq a0 (/ (+ a0 b0) 2)
	     b0 (sqrt (* a0 b0)))
      (setf c0 (/ (- a0 b0) 2))
      (setf phi (+ phi (lisp:atan (* (/ b0 a0) (lisp:tan phi)))))
      (format t "~A ~A ~A~%" a0 b0 c0 phi))))

(defprop %jacobi_am simp-%jacobi_am operators)

(defmfun $jacobi_am (u m)
  (simplify `((%jacobi_am) ,(resimplify u) ,(resimplify m))))

(defmfun simp-%jacobi_am (form yy z)
  (declare (ignore y))
  (twoargcheck form)
  (let ((u (simpcheck (cadr form) z))
	(m (simpcheck (caddr form) z)))
    (cond ((or (and (floatp u) (floatp m))
	       (and $numer (numberp u) (numberp m)))
	   ;; Numerically evaluate am
	   (asin (sn (float u 1d0) (float m 1d0))))
	  (t
	   ;; Nothing to do
	   (eqtest (list '(%jacobi_am) u m) form)))))
  
