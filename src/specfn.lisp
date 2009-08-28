;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module specfn)

;;*********************************************************************
;;****************                                   ******************
;;**************** Macsyma Special Function Routines ******************
;;****************                                   ******************
;;*********************************************************************

(load-macsyma-macros rzmac)
(load-macsyma-macros mhayat)

(defmacro mnumericalp (arg)
  `(or (floatp ,arg) (and (or $numer $float) (integerp ,arg))))

;; subtitle polylogarithm routines

(declare-top (special $zerobern ivars key-vars tlist %e-val))

(defun lisimp (exp vestigial z)
  (declare (ignore vestigial))
  (let ((s (simpcheck (car (subfunsubs exp)) z))
	($zerobern t)
	(a))
    (subargcheck exp 1 1 '$li)
    (setq a (simpcheck (car (subfunargs exp)) z))
    (or (cond ((eql a 0) 0)
	      ((not (integerp s)) ())
	      ((= s 1) (m- `((%log) ,(m- 1 a))))
	      ((and (integerp a) (> s 1)
		    (cond ((= a 1) ($zeta s))
			  ((= a -1)
			   (m*t (m1- `((rat) 1 ,(expt 2 (- s 1))))
				($zeta s))))))
	      ((= s 2) (li2simp a))
	      ((= s 3) (li3simp a)))
	(eqtest (subfunmakes '$li (ncons s) (ncons a))
		exp))))

(defun li2simp (arg)
  (cond ((mnumericalp arg) (li2numer (float arg)))
	((alike1 arg '((rat) 1 2))
	 (m+t (m// ($zeta 2) 2)
	      (m*t '((rat simp) -1 2)
		   (m^ '((%log) 2) 2))))))

(defun li3simp (arg)
  (cond ((mnumericalp arg) (li3numer (float arg)))
	((alike1 arg '((rat) 1 2))
	 (m+t (m* '((rat simp) 7 8) '(($zeta) 3))
	      (m*t (m// ($zeta 2) -2) (simplify '((%log) 2)))
	      (m*t '((rat simp) 1 6) (m^ '((%log) 2) 3))))))

;; Numerical evaluation for Chebyschev expansions of the first kind

(defun cheby (x chebarr)
  (let ((bn+2 0.0) (bn+1 0.0))
    (do ((i (floor (aref chebarr 0)) (1- i)))
	((< i 1) (- bn+1 (* bn+2 x)))
     (setq bn+2
	    (prog1 bn+1 (setq bn+1 (+ (aref chebarr i)
				      (- (* 2.0 x bn+1) bn+2))))))))

(defun cheby-prime (x chebarr)
  (- (cheby x chebarr)
      (* (aref chebarr 1) 0.5)))

;; These should really be calculated with minimax rational approximations.
;; Someone has done LI[2] already, and this should be updated; I haven't
;; seen any results for LI[3] yet.

(defun li2numer (y)
  ;; Spence's function can be used to compute li[2] for 0 <= x <= 1.
  ;; To compute the rest, we need the following identities:
  ;;
  ;; li[2](x) = -li[2](1/x)-log(-x)^2/2-%pi^2/6
  ;; li[2](x) = li[2](1/(1-x)) + log(1-x)*log((1-x)/x^2)/2 - %pi^2/6
  ;;
  ;; The first tells us how to compute li[2] for x > 1.  The result is complex.
  ;; For x < 0, the second can be used, and the result is real.
  ;;
  ;; (See http://functions.wolfram.com/ZetaFunctionsandPolylogarithms/PolyLog2/17/01/01/)
  (labels ((li2 (x)
	     (cond ((< x 0)
		    (+ (li2 (/ (- 1 x)))
		       (* 0.5 (log (- 1 x)) (log (/ (- 1 x) (* x x))))
		       (- (/ (cl:expt (float pi) 2) 6))))
		   ((< x 1)
		    (slatec:dspenc x))
		   ((= x 1)
		    (/ (cl:expt (float pi) 2) 6))
		   (t
		    ;; li[2](x) = -li[2](1/x)-log(-x)^2/2-%pi^2/6
		    (- (+ (li2 (/ x))
			  (/ (cl:expt (cl:log (- x)) 2) 2)
			  (/ (cl:expt (float pi) 2) 6)))))))
    (complexify (li2 y))))


(defun li3numer (x)
  (cond ((= x 0.0) 0.0)
	((= x 1.0) 1.20205690)
	((< x -1.0)
	 (- (chebyli3 (/ x)) (* 1.64493407 (log (- x)))
	     (/ (expt (log (- x)) 3) 6.0)))
	((not (> x 0.5)) (chebyli3 x))
	((not (> x 2.0))
	 (let ((fac (* (expt (log x) 2) 0.5)))
	   (m+t (+ 1.20205690
		    (- (* (log x)
			    (- 1.64493407 (chebyli2 (- 1.0 x))))
			(chebys12 (- 1.0 x))
			(* fac
			    (log (cond ((< x 1.0) (- 1.0 x))
				       ((1- x)))))))
		(cond ((< x 1.0) 0)
		      ((m*t (* fac -3.14159265) '$%i))))))
	(t (m+t (+ (chebyli3 (/ x)) (* 3.28986813 (log x))
		    (/ (expt (log x) 3) -6.0))
		(m*t (* -1.57079633 (expt (log x) 2)) '$%i)))))

(defvar *li2* (make-array 15. :initial-contents '(14.0 1.93506430 .166073033 2.48793229e-2
						  4.68636196e-3 1.0016275e-3 2.32002196e-4
						  5.68178227e-5 1.44963006e-5 3.81632946e-6
						  1.02990426e-6 2.83575385e-7 7.9387055e-8
						  2.2536705e-8 6.474338e-9)
			  :element-type 'flonum))


(defvar *li3* (make-array 15. :initial-contents '(14.0 1.95841721 8.51881315e-2 8.55985222e-3
						  1.21177214e-3 2.07227685e-4 3.99695869e-5
						  8.38064066e-6 1.86848945e-6 4.36660867e-7
						  1.05917334e-7 2.6478920e-8 6.787e-9
						  1.776536e-9 4.73417e-10)
			  :element-type 'flonum))

(defvar *s12* (make-array 18. :initial-contents '(17.0 1.90361778 .431311318 .100022507
						  2.44241560e-2 6.22512464e-3 1.64078831e-3
						  4.44079203e-4 1.22774942e-4 3.45398128e-5
						  9.85869565e-6 2.84856995e-6 8.31708473e-7
						  2.45039499e-7 7.2764962e-8 2.1758023e-8 6.546158e-9
						  1.980328e-9)
			  :element-type 'flonum))

(defun chebyli2 (x)
  (* x (cheby-prime (/ (1+ (* x 4)) 3) *li2*)))

(defun chebyli3 (x)
  (* x (cheby-prime (/ (1+ (* 4 x)) 3) *li3*)))

(defun chebys12 (x)
  (* (/ (expt x 2) 4)
      (cheby-prime (/ (1+ (* 4 x)) 3) *s12*)))

;; subtitle polygamma routines

;; gross efficiency hack, exp is a function of *k*, *k* should be mbind'ed

(defun msum (exp lo hi)
  (if (< hi lo)
      0
      (let ((sum 0))
	(do ((*k* lo (1+ *k*)))
	    ((> *k* hi) sum)
	  (declare (special *k*))
	  (setq sum (add2 sum (meval exp)))))))


(defun pole-err (exp)
  (declare (special errorsw))
  (cond (errorsw (throw 'errorsw t))
	(t (merror "Pole encountered in: ~M" exp))))


(declare-top (special $maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom))

(defprop $psi psisimp specsimp)

;; Integral of psi function psi[n](x)
(putprop '$psi
  `((n x)
   nil
   ,(lambda (n unused)
     (declare (ignore unused))
     (cond 
      ((and ($integerp n) (>= n 0))
       (cond 
	((= n 0) '((%log_gamma) x))
	(t '((mqapply) (($psi array) ((mplus) -1 n)) x))))
      (t nil))))
     'integral)

(mapcar #'(lambda (var val)
	    (and (not (boundp var)) (setf (symbol-value var) val)))
	'($maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom)
	'(20. -10. 6 6))

(defun psisimp (exp a z)
  (let ((s (simpcheck (car (subfunsubs exp)) z)))
    (subargcheck exp 1 1 '$psi)
    (setq a (simpcheck (car (subfunargs exp)) z))
    (and (integerp a) (< a 1) (pole-err exp))
    (eqtest (psisimp1 s a) exp)))

;; This gets pretty hairy now.

(defun psisimp1 (s a)
  (let ((*k*))
    (declare (special *k*))
    (or
     (and (integerp s) (>= s 0) (mnumericalp a)
	  (let (($float2bf t)) ($float (mfuncall '$bfpsi s a 18))))
     (and (integerp s) (>= s 0) ($bfloatp a)
	  (mfuncall '$bfpsi s a $fpprec))
     (and (not $numer) (not $float) (integerp s) (> s -1)
	  (cond
	    ((integerp a)
	     (and (not (> a $maxpsiposint)) ; integer values
		  (m*t (expt -1 s) (factorial s)
		       (m- (msum (inv (m^t '*k* (1+ s))) 1 (1- a))
			   (cond ((zerop s) '$%gamma)
				 (($zeta (1+ s))))))))
	    ((or (not (ratnump a)) (ratgreaterp a $maxpsiposint)) ())
	    ((ratgreaterp a 0)
	     (cond
	       ((ratgreaterp a 1)
		(let* ((int ($entier a)) ; reduction to fractional values
		       (frac (m-t a int)))
		  (m+t
		   (psisimp1 s frac)
		   (if (> int $maxpsiposint)
		       (subfunmakes '$psi (ncons s) (ncons int))
		       (m*t (expt -1 s) (factorial s)
			    (msum (m^t (m+t (m-t a int) '*k*)
				       (1- (- s)))
				  0 (1- int)))))))
	       ((= s 0)
		(let ((p (cadr a)) (q (caddr a)))
		  (cond
		    ((or (> p $maxpsifracnum)
			 (> q $maxpsifracdenom) (bignump p) (bignump q)) ())
		    ((and (= p 1)
			  (cond ((= q 2)
				 (m+ (m* -2 '((%log) 2)) (m- '$%gamma)))
				((= q 3)
				 (m+ (m* '((rat simp) -1 2)
					 (m^t 3 '((rat simp) -1 2)) '$%pi)
				     (m* '((rat simp) -3 2) '((%log) 3))
				     (m- '$%gamma)))
				((= q 4)
				 (m+ (m* '((rat simp) -1 2) '$%pi)
				     (m* -3 '((%log) 2)) (m- '$%gamma)))
				((= q 6)
				 (m- (m+ (m* '((rat simp) 3 2) '((%log) 3))
					 (m* 2 '((%log) 2))
					 (m* '((rat simp) 1 2) '$%pi
					     (m^t 3 '((rat simp) 1 2)))
					 '$%gamma))))))
		    ((and (= p 2) (= q 3))
		     (m+ (m* '((rat simp) 1 2)
			     (m^t 3 '((rat simp) -1 2)) '$%pi)
			 (m* '((rat simp) -3 2) '((%log) 3))
			 (m- '$%gamma)))
		    ((and (= p 3) (= q 4))
		     (m+ (m* '((rat simp) 1 2) '$%pi)
			 (m* -3 '((%log) 2)) (m- '$%gamma)))
		    ((and (= p 5) (= q 6))
		     (m- (m* '((rat simp) 1 2) '$%pi
			     (m^t 3 '((rat simp) 1 2)))
			 (m+ (m* '((rat simp) 3 2) '((%log) 3))
			     (m* 2 '((%log) 2))
			     '$%gamma)))
		    ;; Gauss's Formula
		    ((let ((f (m* `((%cos) ,(m* 2 a '$%pi '*k*))
				  `((%log) ,(m-t 2 (m* 2 `((%cos)
							   ,(m//t (m* 2 '$%pi '*k*)
								  q))))))))
		       (m+t (msum f 1 (1- (truncate q 2)))
			    (let ((*k* (truncate q 2)))
			      (declare (special *k*))
			      (m*t (meval f)
				   (cond ((oddp q) 1)
					 ('((rat simp) 1 2)))))
			    (m-t (m+ (m* '$%pi '((rat simp) 1 2)
					 `((%cot) ((mtimes simp) ,a $%pi)))
				     `((%log) ,q)
				     '$%gamma))))))))
	       ((alike1 a '((rat) 1 2))
		(m*t (expt -1 (1+ s)) (factorial s)
		     (1- (expt 2 (1+ s))) (simplify ($zeta (1+ s)))))
	       ((and (ratgreaterp a '((rat) 1 2))
		     (ratgreaterp 1 a))
		(m*t
		 (expt -1 s)
		 (m+t (psisimp1 s (m- 1 a))
		      (let ((dif (m* '$%pi
				     ($diff `((%cot) ,(m* '$%pi '$z)) '$z s)))
			    ($z (m-t a)))
			(declare (special $z))
			(meval dif)))))))
	    ((ratgreaterp a $maxpsinegint)  ;;; Reflection Formula
	     (m*t
	      (expt -1 s)
	      (m+t (m+t (psisimp1 s (m- a))
			(let ((dif (m* '$%pi
				       ($diff `((%cot) ,(m* '$%pi '$z)) '$z s)))
			      ($z (m-t a)))
			  (declare (special $z))
			  (meval dif)))
		   (m*t (factorial s) (m^t (m-t a) (1- (- s)))))))))
     (subfunmakes '$psi (ncons s) (ncons a)))))


;; subtitle polygamma tayloring routines

;; These routines are specially coded to be as fast as possible given the
;; current $TAYLOR; too bad they have to be so ugly.

(declare-top (special var subl *last* sign last-exp))

(defun expgam-fun (pw temp)
  (setq temp (get-datum (get-key-var (car var))))
  (let-pw temp pw
	  (pstimes
	   (let-pw temp (e1+ pw)
		   (psexpt-fn (getexp-fun '(($psi) -1) var (e1+ pw))))
	   (make-ps var (ncons pw) '(((-1 . 1) 1 . 1))))))

(defun expplygam-funs (pw subl l)	; l is a irrelevant here
  (setq subl (car subl))
  (if (or (not (integerp subl)) (< subl -1))
      (tay-err "Unable to expand at a subscript in")
      (prog ((e 0) (sign 0) npw)
	 (declare (fixnum e) (fixnum sign))
	 (setq npw (/ (float (car pw)) (float (cdr pw))))
	 (setq
	  l (cond ((= subl -1)
		   `(((1 . 1) . ,(prep1 '((mtimes) -1 $%gamma)))))
		  ((= subl 0)
		   (cons '((-1 . 1) -1 . 1)
			 (if (> 0.0 npw) ()
			     `(((0 . 1)
				. ,(prep1 '((mtimes) -1 $%gamma)))))))
		  (t (setq *last* (factorial subl))
		     `(((,(- (1+ subl)) . 1)
			,(* (expt -1 (1+ subl))
				(factorial subl)) . 1))))
	  e (if (< subl 1) (- subl) -1)
	  sign (if (< subl 1) -1 (expt -1 subl)))
	 a (setq e (1+ e) sign (- sign))
	 (if (> e npw) (return l)
	     (rplacd (last l)
		     `(((,e . 1)
			. ,(rctimes (rcplygam e)
				    (prep1 ($zeta (+ (1+ subl) e))))))))
	 (go a))))

(defun rcplygam (k)
  (declare (fixnum k) )
  (cond ((= subl -1) (cons sign k))
	((= subl 0) (cons sign 1))
	(t (prog1
	       (cons (* sign *last*) 1)
	     (setq *last*
		   (*quo (* *last* (+ subl (1+ k)))
			 (1+ k)))))))

(defun plygam-ord (subl)
  (if (equal (car subl) -1) (ncons (rcone))
      `((,(m- (m1+ (car subl))) . 1))))

(defun plygam-pole (a c func)
  (if (rcmintegerp c)
      (let ((ps (get-lexp (m- a (rcdisrep c)) () t)))
	(rplacd (cddr ps) (cons `((0 . 1) . ,c) (cdddr ps)))
	(if (atom func) (gam-const a ps func)
	    (plygam-const a ps func)))
      (prep1 (simplifya
	      (if (atom func) `((%gamma) ,(rcdisrep c))
		  `((mqapply) ,func ,(rcdisrep c)))
	      () ))))

(defun gam-const (a arg func)
  (let ((const (ps-lc* arg)) (arg-c))
    (cond ((not (rcintegerp const))
	   (taylor2 (diff-expand `((%gamma) ,a) tlist)))
	  (t
	   (setq const (car const))
	   (if (pscoefp arg) (setq arg-c (get-lexp (m+t a (- const)) (rcone) (signp le const))))
	   (if (and arg-c (not (psp arg-c)))
	       (taylor2 (simplify `((%gamma) ,const)))
	       (let ((datum (get-datum (get-key-var (gvar (or arg-c arg)))))
		     (ord (if arg-c (le (terms arg-c)) (le (n-term (terms arg))))))
		 (setq func (current-trunc datum))
		 (if (> const 0)
		     (pstimes (let-pw datum (e- func ord) (expand (m+t a (- const)) '%gamma))
			      (let-pw datum (e+ func ord)
				      (tsprsum (m+t a (m-t '%%taylor-index%%))
					       `(%%taylor-index%% 1 ,const) '%product)))
		     (pstimes (expand (m+t a (- const)) '%gamma)
			      (let-pw datum (e+ func ord)
				      (psexpt (tsprsum (m+t a '%%taylor-index%%)
						       `(%%taylor-index%% 0 ,(- (1+ const))) '%product)
					      (rcmone)))))))))))

(defun plygam-const (a arg func)
  (let ((const (ps-lc* arg)) (sub (cadr func)))
    (cond
      ((or (not (integerp sub)) (< sub -1))
       (tay-err "Unable to expand at a subscript in"))
      ((not (rcintegerp const))
       (taylor2 (diff-expand `((mqapply) ,func ,a) tlist)))
      (t (setq const (car const))
	 (psplus
	  (expand (m+t a (- const)) func)
	  (if (> const 0)
	      (pstimes
	       (cons (* (expt -1 sub) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a (m-t '%%taylor-index%%)) ,(- (1+ sub)))
			`(%%taylor-index%% 1 ,const) '%sum))
	      (pstimes
	       (cons (* (expt -1 (1+ sub)) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a '%%taylor-index%%) ,(- (1+ sub)))
			`(%%taylor-index%% 0 ,(- (1+ const))) '%sum))))))))

(declare-top (unspecial var subl *last* sign last-exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lambert W
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $lambert_w (z)
  (simplify (list '(%lambert_w) (resimplify z))))

;;; Set properties to give full support to the parser and display
(defprop $lambert_w %lambert_w alias)
(defprop $lambert_w %lambert_w verb)
(defprop %lambert_w $lambert_w reversealias)
(defprop %lambert_w $lambert_w noun)

;;; lambert_w is a simplifying function
(defprop %lambert_w simp-lambertw operators)

;;; Derivative of lambert_w
(defprop %lambert_w
  ((x) 
   ((mtimes)
    ((mexpt) $%e ((mtimes ) -1 ((%lambert_w) x)))
    ((mexpt) ((mplus) 1 ((%lambert_w) x)) -1)))
  grad)

;;; Integral of lambert_w
;;; integrate(W(x),x) := x*(W(x)^2-W(x)+1)/W(x)
(defprop %lambert_w
  ((x)
   ((mtimes)
    x
    ((mplus) 
     ((mexpt) ((%lambert_w) x) 2) 
     ((mtimes) -1 ((%lambert_w) x))
     1)
    ((mexpt) ((%lambert_w) x) -1)))
  integral)

(defun simp-lambertw (x y z)
  (oneargcheck x)
  (setq x (simpcheck (cadr x) z))
  (cond ((equal x 0) 0)
	((equal x 0.0) 0.0)
	((zerop1 x) ($bfloat 0))	;bfloat case
	((alike1 x '$%e)
	 ;; W(%e) = 1
	 1)
	((alike1 x '((mtimes simp) ((rat simp) -1 2) ((%log simp) 2)))
	 ;; W(-log(2)/2) = -log(2)
	 '((mtimes simp) -1 ((%log simp) 2)))
	((alike1 x '((mtimes simp) -1 ((mexpt simp) $%e -1)))
	 ;; W(-1/e) = -1
	 -1)
	((alike1 x '((mtimes) ((rat) -1 2) $%pi))
	 ;; W(-%pi/2) = %i*%pi/2
	 '((mtimes simp) ((rat simp) 1 2) $%i $%pi))
	;; W(x) is real for x real and x > -1/%e
	((and (float-numerical-eval-p x) (< (- (/ %e-val)) x))
	 (lambert-w x))
	;; Complex float x or real float x < -1/%e
	((complex-float-numerical-eval-p x)
	 (complexify (lambert-w 
		      (complex ($float ($realpart x)) ($float ($imagpart x))))))
	((complex-bigfloat-numerical-eval-p x)
	 (bfloat-lambert-w x))
	(t (list '(%lambert_w simp) x))))

;; Complex value of the principal branch of Lambert's W function in 
;; the entire complex plane with relative error less than 1%, given 
;; standard branch cuts for sqrt(z) and log(z).
;;
;;   Winitzki, S. Uniform Approximations for Transcendental Functions. 
;;   In Part 1 of Computational Science and its Applications - ICCSA 2003, 
;;   Lecture Notes in Computer Science, Vol. 2667, Springer-Verlag, 
;;   Berlin, 2003, 780-789. DOI 10.1007/3-540-44839-X_82
;;
;;   From http://homepages.physik.uni-muenchen.de/~Winitzki/papers/

(defun init-lambert-w (z)
  (let ((A 2.344d0) (B 0.8842d0) (C 0.9294d0) (D 0.5106d0) (E -1.213d0)
     (y (sqrt (+ (* 2 %e-val z ) 2)) ) )   ; y=sqrt(2*%e*z+2) 
    ; w = (2*log(1+B*y)-log(1+C*log(1+D*y))+E)/(1+1/(2*log(1+B*y)+2*A)
     (/ 
      (+ (* 2 (log (+ 1 (* b y))))
	 (* -1 (log (+ 1 (* C (log (+ 1 (* D y)))))))
	 E)
      (+ 1
	 (/ 1 (+ (* 2 (log (+ 1 (* B y)))) (* 2 A)))))))

;; Algorithm based in part on
;;
;; Corless, R. M., Gonnet, D. E. G., Jeffrey, D. J., Knuth, D. E. (1996). 
;; "On the Lambert W function". Advances in Computational Mathematics 5: 
;; pp 329-359
;; 
;;    http://www.apmaths.uwo.ca/~djeffrey/Offprints/W-adv-cm.pdf.
;; or http://www.apmaths.uwo.ca/~rcorless/frames/PAPERS/LambertW/
;;
;; See also http://en.wikipedia.org/wiki/Lambert's_W_function
;;
;; It is Halley's iteration applied to w*exp(w).
;;
;;
;;                               w[j] exp(w[j]) - z 
;; w[j+1] = w[j] - -------------------------------------------------
;;                                       (w[j]+2)(w[j] exp(w[j]) -z)
;;                  exp(w[j])(w[j]+1) -  ---------------------------
;;                                               2 w[j] + 2
;;
;; The algorithm has cubic convergence.  Once convergence begins, the 
;; number of digits correct at step k is roughly 3 times the number 
;; which were correct at step k-1.

(defun lambert-w (z &key (maxiter 100) (prec 1d-14))
  (let ((w (init-lambert-w z)))
    (dotimes (k maxiter)
      (let* ((we (* w (exp w)))
	     (w1e (* (1+ w)
		     (exp w)))
	     (delta (/ (- we z)
		       (- w1e (/ (* (+ w 2)
				    (- we z))
				 (+ 2 (* 2 w)))))))
	(when (<= (abs (/ delta w)) prec)
	  (return w))
	(decf w delta)))
    w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This routine is a translation of the float version for complex
;;; Bigfloat numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bfloat-lambert-w (z)
  (let ((prec (power ($bfloat 10.0) (- $fpprec)))
        (maxiter 500) ; arbitrarily chosen, we need a better choice
	(fpprec (add fpprec 8)) ; Increase precision slightly
        w)

  ;; Get an initial estimate.  
  ;; if abs(z) < 2^332 ~ 1.0e100 use W(z) ~ lambert_w(float(z))
  ;; For large z,                    W(z) ~ log(z)-log(log(z))
  (setq w 
	(if (eq ($sign (sub ($cabs z) ($bfloat (power 2 332)))) '$neg)
	    ($bfloat ($lambert_w ($float z)))
	    (let ((log-z ($log z))) (sub log-z ($log log-z)))))

  (dotimes (k maxiter)
    (let* ((one ($bfloat 1))
	   (two ($bfloat 2))
	   (exp-w ($bfloat ($exp w)))
	   (we (cmul w exp-w))
	   (w1e (cmul (add w one ) exp-w))
	   (delta (cdiv (sub we z)
		     (sub w1e (cdiv (cmul (add w two)
				  (sub we z))
			       (add two (cmul two w)))))))
      (when (eq ($sign (sub ($cabs (cdiv delta w)) prec)) '$neg)
	(return w))
      (setq w (sub w delta))))
  w))
