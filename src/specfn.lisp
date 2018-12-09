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

(declare-top (special $zerobern tlist %e-val))

(defun lisimp (expr vestigial z)
  (declare (ignore vestigial))
  (let ((s (simpcheck (car (subfunsubs expr)) z))
        ($zerobern t)
        (a))
    (subargcheck expr 1 1 '$li)
    (setq a (simpcheck (car (subfunargs expr)) z))
    (or (cond ((zerop1 a) a)
              ((not (integerp s)) ())
              ((= s 1)
               (if (onep1 a)
                   (simp-domain-error
                     (intl:gettext "li: li[~:M](~:M) is undefined.") s a)
                   (neg (take '(%log) (sub 1 a)))))
              ((= s 0) (div a (sub 1 a)))
              ((< s 0) (lisimp-negative-integer s a))
              ((and (integerp a) (> s 1)
                    (cond ((= a 1) (take '(%zeta) s))
                          ((= a -1)
			   ;; li[s](-1) = (2^(1-s)-1)*zeta(s)
                           (mul (add -1 (inv (expt 2 (- s 1))))
                                (take '(%zeta) s))))))
              ((= s 2) (li2simp a))
              ((= s 3) (li3simp a))
	      ((or (complex-float-numerical-eval-p a)
		   (complex-bigfloat-numerical-eval-p a))
	       (cond ((bigfloat:= 1 (bigfloat:to a))
		      ;; li[s](1) -> zeta(s)
		      (let ((result ($zeta s)))
			(if (floatp a)
			    ($float result)
			    ($bfloat result))))
		     ((bigfloat:= -1 (bigfloat:to a))
		      ;; li[s](-1) = (2^(1-s)-1)*zeta(s)
		      (let ((result (mul (add -1 (inv (expt 2 (- s 1))))
					 (take '(%zeta) s))))
			(if (floatp a)
			    ($float result)
			    ($bfloat result))))
		     ((integerp s)
		      (to (bigfloat::li-s-simp s (bigfloat:to a)))))))
        (eqtest (subfunmakes '$li (ncons s) (ncons a))
                expr))))

;; Expand the Polylogarithm li[s](z) for a negative integer parameter s.
(defun lisimp-negative-integer (s z)
  (let ((n (- s)))
    (mul (inv (power (sub 1 z) (+ n 1)))
         (let ((index1 (gensumindex))
               ($simpsum t))
           (dosum
             (mul (power z index1)
                  (let ((index2 (gensumindex)))
                    (dosum
                      (mul (power -1 (add index2 1))
                           (take '(%binomial) (+ n 1) (sub index2 1))
                           (power (add 1 (sub index1 index2)) n))
                      index2 1 index1 t)))
             index1 1 n t)))))

(defun li2simp (arg)
  (cond ((mnumericalp arg)
	 ;; When arg is a float or rational, use the original li2numer
	 ;; using Spences function.
	 (li2numer (float arg)))
	((complex-float-numerical-eval-p arg)
	 ;; For complex args that should should result in float
	 ;; answers, use bigfloat::li2numer.
	 (to (bigfloat::li2numer (bigfloat:to ($rectform ($float arg))))))
	((or (bigfloat-numerical-eval-p arg)
	     (complex-bigfloat-numerical-eval-p arg))
	 (to (bigfloat::li2numer (bigfloat:to ($rectform ($bfloat arg))))))
        ((alike1 arg '((rat) 1 2))
         (add (div (take '(%zeta) 2) 2)
              (mul '((rat simp) -1 2)
                   (power (take '(%log) 2) 2))))))

(defun li3simp (arg)
  (cond ((or (float-numerical-eval-p arg)
	     (complex-float-numerical-eval-p arg))
	 (to (bigfloat::li3numer (bigfloat:to ($rectform ($float arg))))))
	((or (bigfloat-numerical-eval-p arg)
	     (complex-bigfloat-numerical-eval-p arg))
	 (to (bigfloat::li3numer (bigfloat:to ($rectform ($bfloat arg))))))
        ((alike1 arg '((rat) 1 2))
         (add (mul '((rat simp) 7 8) (take '(%zeta) 3))
              (mul (div (take '(%zeta) 2) -2) (take '(%log) 2))
              (mul '((rat simp) 1 6) (power (take '(%log) 2) 3))))))

;; exponent in first term of taylor expansion of $li is one
(defun li-ord (subl)
  (declare (ignore subl))
  (ncons (rcone)))

;; taylor expansion of $li is its definition:
;; x + x^2/2^s + x^3/3^s + ...
(defun exp$li-fun (pw subl l)	; l is a irrelevant here
  (setq subl (car subl))	; subl is subscript of li
  (prog ((e 0) 			; e is exponent of current term
	 npw)			; npw is exponent of last term needed
	(declare (fixnum e))
	(setq npw (/ (float (car pw)) (float (cdr pw))))
	(setq
	 l (cons '((0 . 1) 0 . 1)
		 nil))
	a (setq e (1+ e))
	(if (> e npw) (return l)
	  (rplacd (last l)
		  `(((,e . 1)
		     . ,(prep1 (m^ e (m- subl)))))))
	(go a)))


;; computes first pw terms of asymptotic expansion of $li[s](z)
;;
;; pw should be < (1/2)*s or gamma term is undefined
;;
;; Wood, D.C. (June 1992). The Computation of Polylogarithms. Technical Report 15-92
;; University of Kent Computing Laboratory.
;; http://www.cs.kent.ac.uk/pubs/1992/110
;; equation 11.1
(defun li-asymptotic-expansion (pw s z)
  (m+l (loop for k from 0 to pw collect
	     (m* (m^ -1 k)
		 (m- 1 (m^ 2 (m- 1 (m* 2 k))))
		 (m^ (m* 2 '$%pi) (m* 2 k))
		 (m// ($bern (m* 2 k))
		      `((mfactorial) ,(m* 2 k)))
		 (m// (m^ `((%log) ,(m- z)) (m- 2 (m* 2 k))) 
		      ($gamma (m+ s 1 (m* -2 k))))))))

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
	(t (merror (intl:gettext "Pole encountered in: ~M") exp))))


(declare-top (special $maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom))

(defprop $psi psisimp specsimp)

;; Integral of psi function psi[n](x)
(putprop '$psi
  `((n x)
   nil
   ,(lambda (n x)
     (cond 
      ((and ($integerp n) (>= n 0))
       (cond 
	((= n 0) `((%log_gamma) ,x))
	(t `((mqapply) (($psi array) ((mplus) -1 ,n)) ,x))))
      (t nil))))
     'integral)

(mapcar #'(lambda (var val)
	    (and (not (boundp var)) (setf (symbol-value var) val)))
	'($maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom)
	'(20. -10. 6 6))

(defun psisimp (expr a z)
  (let ((s (simpcheck (car (subfunsubs expr)) z)))
    (subargcheck expr 1 1 '$psi)
    (setq a (simpcheck (car (subfunargs expr)) z))
    (and (setq z (integer-representation-p a))
         (< z 1)
         (pole-err expr))
    (eqtest (psisimp1 s a) expr)))

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
		   (quot (* *last* (+ subl (1+ k)))
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
;;; Lambert W function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; References
;;
;; Corless, R. M., Gonnet, D. E. G., Jeffrey, D. J., Knuth, D. E. (1996). 
;; "On the Lambert W function". Advances in Computational Mathematics 5: 
;; pp 329-359
;; 
;;    http://www.apmaths.uwo.ca/~djeffrey/Offprints/W-adv-cm.pdf.
;; or http://www.apmaths.uwo.ca/~rcorless/frames/PAPERS/LambertW/
;;
;; D. J. Jeffrey, D. E. G. Hare, R. M. Corless
;; Unwinding the branches of the Lambert W function
;; The Mathematical Scientist, 21, pp 1-7, (1996)
;; http://www.apmaths.uwo.ca/~djeffrey/Offprints/wbranch.pdf
;;
;; Winitzki, S. Uniform Approximations for Transcendental Functions. 
;; In Part 1 of Computational Science and its Applications - ICCSA 2003, 
;; Lecture Notes in Computer Science, Vol. 2667, Springer-Verlag, 
;; Berlin, 2003, 780-789. DOI 10.1007/3-540-44839-X_82
;; http://homepages.physik.uni-muenchen.de/~Winitzki/papers/
;;
;; Darko Verebic, 
;; Having Fun with Lambert W(x) Function
;; arXiv:1003.1628v1, March 2010, http://arxiv.org/abs/1003.1628
;;
;; See also http://en.wikipedia.org/wiki/Lambert's_W_function

(defmfun $lambert_w (z)
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

(defun simp-lambertw (x yy z)
  (declare (ignore yy))
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
        ;; numerical evaluation
	((complex-float-numerical-eval-p x)
          ;; x may be an integer.  eg "lambert_w(1),numer;"
	  (if (integerp x)
	    (to (bigfloat::lambert-w-k 0 (bigfloat:to ($float x))))
	    (to (bigfloat::lambert-w-k 0 (bigfloat:to x)))))
	((complex-bigfloat-numerical-eval-p x)
	 (to (bigfloat::lambert-w-k 0 (bigfloat:to x))))
	(t (list '(%lambert_w simp) x))))

;; An approximation of the k-branch of generalized Lambert W function
;;   k integer
;;   z real or complex lisp float
;; Used as initial guess for Halley's iteration. 
;; When W(z) is real, ensure that guess is real.
(defun init-lambert-w-k (k z)
  (let ( ; parameters for k = +/- 1 near branch pont z=-1/%e
        (branch-eps 0.2e0)
	(branch-point (/ -1 %e-val))) ; branch pont z=-1/%e
    (cond 
      ; For principal branch k=0, use expression by Winitzki
      ((= k 0) (init-lambert-w-0 z))
      ; For k=1 branch, near branch point z=-1/%e with im(z) <  0
      ((and (= k 1)
	    (< (imagpart z) 0)
	    (< (abs (- branch-point z)) branch-eps))
        (bigfloat::lambert-branch-approx z))
      ; For k=-1 branch, z real with -1/%e < z < 0
      ; W(z) is real in this range
      ((and (= k -1) (realp z) (> z branch-point) (< z 0))
        (init-lambert-w-minus1 z))
      ; For k=-1 branch, near branch point z=-1/%e with im(z) >= 0
      ((and (= k -1)
	    (>= (imagpart z) 0)
	    (< (abs (- branch-point z)) branch-eps))
        (bigfloat::lambert-branch-approx z))
      ; Default to asymptotic expansion Corless et al (4.20)
      ; W_k(z) = log(z) + 2.pi.i.k - log(log(z)+2.pi.i.k)
      (t (let ((two-pi-i-k (complex 0.0e0 (* 2 pi k))))
		 (+ (log z) 
		    two-pi-i-k 
		    (* -1 (log (+ (log z) two-pi-i-k )))))))))

;; Complex value of the principal branch of Lambert's W function in 
;; the entire complex plane with relative error less than 1%, given 
;; standard branch cuts for sqrt(z) and log(z).
;; Winitzki (2003)
(defun init-lambert-w-0 (z)
  (let ((a 2.344e0) (b 0.8842e0) (c 0.9294e0) (d 0.5106e0) (e -1.213e0)
     (y (sqrt (+ (* 2 %e-val z ) 2)) ) )   ; y=sqrt(2*%e*z+2) 
    ; w = (2*log(1+B*y)-log(1+C*log(1+D*y))+E)/(1+1/(2*log(1+B*y)+2*A)
     (/ 
      (+ (* 2 (log (+ 1 (* b y))))
	 (* -1 (log (+ 1 (* c (log (+ 1 (* d y)))))))
	 e)
      (+ 1
	 (/ 1 (+ (* 2 (log (+ 1 (* b y)))) (* 2 a)))))))

;; Approximate k=-1 branch of Lambert's W function over -1/e < z < 0. 
;; W(z) is real, so we ensure the starting guess for Halley iteration 
;; is also real.
;; Verebic (2010)
(defun init-lambert-w-minus1 (z)
  (cond 
    ((not (realp z)) 
      (merror "z not real in init-lambert-w-minus1"))
    ((or (< z (/ -1 %e-val)) (plusp z))
      (merror "z outside range of approximation in init-lambert-w-minus1"))
    ;; In the region where W(z) is real
    ;; -1/e < z < C, use power series about branch point -1/e ~ -0.36787
    ;; C = -0.3 seems a reasonable crossover point
    ((< z -0.3)
      (bigfloat::lambert-branch-approx z))
    ;; otherwise C <= z < 0, use iteration W(z) ~ ln(-z)-ln(-W(z))
    ;; nine iterations are sufficient over -0.3 <= z < 0 
    (t (let* ((ln-z (log (- z))) (maxiter 9) (w ln-z))
	 (dotimes (k maxiter w)
            (setq w (- ln-z (log (- w)))))))))

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;; Approximate Lambert W(k,z) for k=1 and k=-1 near branch point z=-1/%e
;; using power series in y=-sqrt(2*%e*z+2)
;;   for im(z) < 0,  approximates k=1 branch
;;   for im(z) >= 0, approximates k=-1  branch
;;
;; Corless et al (1996) (4.22)
;; Verebic (2010)
;;
;; z is a real or complex bigfloat: 
(defun lambert-branch-approx (z)
  (let ((y (- (sqrt (+ (* 2 (%e z) z ) 2)))) ; y=-sqrt(2*%e*z+2)
    (b0 -1) (b1 1) (b2 -1/3) (b3 11/72))
    (+ b0 (* y (+ b1 (* y (+ b2 (* b3 y))))))))

;; Algorithm based in part on Corless et al (1996).
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
;;
;; Convergence can stall using convergence test abs(w[j+1]-w[j]) < prec,
;; as happens for generalized_lambert_w(-1,z) near branch point z = -1/%e
;; Therefore also stop iterating if abs(w[j]*exp(w[j]) - z) << abs(z)
(defun lambert-w-k (k z &key (maxiter 50))
  (let ((w (init-lambert-w-k k z)) we w1e delta (prec (* 4 (epsilon z))))
    (dotimes (i maxiter (maxima::merror "lambert-w-k did not converge"))
      (setq we (* w (exp w)))
      (when (<= (abs (- z we)) (* 4 (epsilon z) (abs z))) (return))
      (setq w1e (* (1+ w) (exp w)))
      (setq delta (/ (- we z)
		     (- w1e (/ (* (+ w 2) (- we z)) (+ 2 (* 2 w))))))
      (decf w delta)
      (when (<= (abs (/ delta w)) prec) (return)))
    ;; Check iteration converged to correct branch
    (check-lambert-w-k k w z)
    w))

(defmethod init-lambert-w-k ((k integer) (z number))
  (maxima::init-lambert-w-k k z))

(defmethod init-lambert-w-k ((k integer) (z bigfloat))
  (bfloat-init-lambert-w-k k z))

(defmethod init-lambert-w-k ((k integer) (z complex-bigfloat))
  (bfloat-init-lambert-w-k k z))

(defun bfloat-init-lambert-w-k (k z)
  "Approximate generalized_lambert_w(k,z) for bigfloat: z as initial guess"
  (let ((branch-point -0.36787944117144)) ; branch point -1/%e
    (cond
       ;; if k=-1, z very close to -1/%e and imag(z)>=0, use power series
       ((and (= k -1)
	     (or (zerop (imagpart z))
		 (plusp (imagpart z)))
	     (< (abs (- z branch-point)) 1e-10))
	 (lambert-branch-approx z))
       ;; if k=1, z very close to -1/%e and imag(z)<0, use power series
       ((and (= k 1)
	     (minusp (imagpart z))
	     (< (abs (- z branch-point)) 1e-10))
	 (lambert-branch-approx z))
       ;; Initialize using float value if z is representable as a float
       ((< (abs z) 1.0e100)
	 (if (complexp z)
	     (bigfloat (lambert-w-k k (cl:complex (float (realpart z) 1.0)
						  (float (imagpart z) 1.0))))
	     (bigfloat (lambert-w-k k (float z 1.0)))))
       ;; For large z, use Corless et al (4.20)
       ;;              W_k(z) ~ log(z) + 2.pi.i.k - log(log(z)+2.pi.i.k)
       (t
	(let ((log-z (log z)))
	  (if (= k 0)
	    (- log-z (log log-z))
	    (let* ((i (make-instance 'complex-bigfloat :imag (intofp 1)))
		  (two-pi-i-k (* 2 (%pi z) i k)))
	      (- (+ log-z two-pi-i-k) 
		 (log (+ log-z two-pi-i-k))))))))))

;; Check Lambert W iteration converged to the correct branch
;; W_k(z) + ln W_k(z) = ln z, for k = -1 and z in [-1/e,0)
;;                    = ln z + 2 pi i k, otherwise
;; See Jeffrey, Hare, Corless (1996), eq (12)
;; k integer
;; z, w bigfloat: numbers
(defun check-lambert-w-k (k w z)
  (let ((tolerance #-gcl 1.0e-6
                   #+gcl (cl:float 1/1000000)))
  (if
     (cond 
       ;; k=-1 branch with z and w real.
      ((and (= k -1) (realp z) (minusp z) (>= z (/ -1 (%e z))))
       (if (and (realp w) 
		(<= w -1)
		(< (abs (+ w (log w) (- (log z)))) tolerance))
	   t
	   nil))
       (t
         ; i k =  (W_k(z) + ln W_k(z) - ln(z)) / 2 pi
        (let (ik)
	  (setq ik (/ (+ w (log w) (- (log z))) (* 2 (%pi z))))
	  (if (and (< (realpart ik) tolerance)
		   (< (abs (- k (imagpart ik))) tolerance))
	    t
	    nil))))
      t
      (maxima::merror "Lambert W iteration converged to wrong branch"))))

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generalized Lambert W function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $generalized_lambert_w (k z)
  (simplify (list '(%generalized_lambert_w) (resimplify k) (resimplify z))))

;;; Set properties to give full support to the parser and display
(defprop $generalized_lambert_w %generalized_lambert_w alias)
(defprop $generalized_lambert_w %generalized_lambert_w verb)
(defprop %generalized_lambert_w $generalized_lambert_w reversealias)
(defprop %generalized_lambert_w $generalized_lambert_w noun)

;;; lambert_w is a simplifying function
(defprop %generalized_lambert_w simp-generalized-lambertw operators)

;;; Derivative of lambert_w
(defprop %generalized_lambert_w
  ((k x)
   nil
   ((mtimes)
    ((mexpt) $%e ((mtimes ) -1 ((%generalized_lambert_w) k x)))
    ((mexpt) ((mplus) 1 ((%generalized_lambert_w) k x)) -1)))
  grad)

;;; Integral of lambert_w
;;; integrate(W(k,x),x) := x*(W(k,x)^2-W(k,x)+1)/W(k,x)
(defprop %generalized_lambert_w
  ((k x)
   nil
   ((mtimes)
    x
    ((mplus) 
     ((mexpt) ((%generalized_lambert_w) k x) 2) 
     ((mtimes) -1 ((%generalized_lambert_w) k x))
     1)
    ((mexpt) ((%generalized_lambert_w) k x) -1)))
  integral)

(defun simp-generalized-lambertw (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((k (simpcheck (cadr expr) z))
        (x (simpcheck (caddr expr) z)))
    (cond
     ;; Numerical evaluation for real or complex x
     ((and (integerp k) (complex-float-numerical-eval-p x))
       ;; x may be an integer.  eg "generalized_lambert_w(0,1),numer;"
       (if (integerp x) 
	   (to (bigfloat::lambert-w-k k (bigfloat:to ($float x))))
	   (to (bigfloat::lambert-w-k k (bigfloat:to x)))))
     ;; Numerical evaluation for real or complex bigfloat x
     ((and (integerp k) (complex-bigfloat-numerical-eval-p x))
      (to (bigfloat::lambert-w-k k (bigfloat:to x))))
     (t (list '(%generalized_lambert_w simp) k x)))))

(in-package "BIGFLOAT")

(defvar *debug-li-eval* nil)

(defun li3numer (x)
  ;; If |x| < series-threshold, the series is used.
  (let ((series-threshold 0.8))
    (cond ((zerop x)
	   0.0)
	  ((= x 1)
	   (maxima::$zeta (maxima::to (float 3 x))))
	  ((= x -1)
	   ;; li[3](-1) = -(1-2^(1-3))*li[3](1)
	   ;;           = -3/4*zeta(3)
	   ;;
	   ;; From the formula
	   ;;
	   ;;   li[s](-1) = (2^(1-s)-1)*zeta(s)
	   ;;
	   ;; (See http://functions.wolfram.com/10.08.03.0003.01)
	   (* -3/4 (to (maxima::$zeta (maxima::to (float 3 x))))))
	  ((> (abs x) 1)
	   ;; For z not in the interval (0, 1) and for integral n, we
	   ;; have the identity:
	   ;;
	   ;; li[n](z) = -log(-z)^n/n! + (-1)^(n-1)*li[n](1/z)
	   ;;               + 2 * sum(li[2*r](-1)/(n-2*r)!*log(-z)^(n-2*r), r, 1, floor(n/2))
	   ;;
	   ;; (See http://functions.wolfram.com/ZetaFunctionsandPolylogarithms/PolyLog/17/02/01/01/0008/)
	   ;;
	   ;; In particular for n = 3:
	   ;;
	   ;; li[3](z) = li[3](1/z) - log(-z)/6*(log(-z)^2+%pi^2)
	   (let* ((lg (log (- x)))
		  (dpi (%pi x))
		  (result (- (li3numer (/ x))
			     (* (/ lg 6)
				(+ (* lg lg) (* dpi dpi))))))
		 result))
	  ((> (abs x) .9)
	   ;; When x is on or near the unit circle the other
	   ;; approaches don't work.  Use the expansion in powers of
	   ;; log(z) (from cephes cpolylog)
	   ;;
	   ;;   li[s](z) = sum(Z(s-j)*(log(z))^j/j!, j = 0, inf)
	   ;;
	   ;; where Z(j) = zeta(j) for j != 1.  For j = 1:
	   ;;
	   ;;   Z(1) = -log(-log(z)) + sum(1/k, k, 1, s - 1)
	   ;;
	   ;;
	   ;; This is similar to
	   ;; http://functions.wolfram.com/10.08.06.0024.01, but that
	   ;; identity is clearly undefined if v is a positive
	   ;; integer because zeta(1) is undefined.
	   ;;
	   ;; Thus,
	   ;;
	   ;;  li[3](z) = Z(3) + Z(2)*log(z) + Z(1)*log(z)^2/2!
	   ;;    + Z(0)*log(z)^3/3! + sum(Z(-k)*log(z)^(k+4)/(k+4)!,k,1,inf);
	   ;;
	   ;; But Z(-k) = zeta(-k) is 0 if k is even.  So
	   ;;
	   ;;  li[3](z) = Z(3) + Z(2)*log(z) + Z(1)*log(z)^2/2!
	   ;;    + Z(0)*log(z)^3/3! + sum(Z(-(2*k+1))*log(z)^(2*k+4)/(2*k+4)!,k,0,inf);

	   (flet ((zfun (j)
		    (cond ((= j 1)
			   (let ((sum (- (log (- (log x))))))
			     (+ sum
				(loop for k from 1 below 3
				      sum (/ k)))))
			  (t
			   (to (maxima::$zeta (maxima::to (float j (realpart x)))))))))
	     (let* ((eps (epsilon x))
		    (logx (log x))
		    (logx^2 (* logx logx))
		    (sum (+ (zfun 3)
			    (* (zfun 2) logx)
			    (* (zfun 1) logx^2 1/2)
			    (* (zfun 0) (* logx^2 logx) 1/6))))
	       (do* ((k 0 (1+ k))
		     (top (expt logx 4) (* top logx^2))
		     (bot 24 (* bot (+ k k 3) (+ k k 4)))
		     (term (* (/ top bot) (to (maxima::$zeta (- (+ 1 (* 2 k))))))
			   (* (/ top bot) (to (maxima::$zeta (- (+ 1 (* 2 k))))))))
		    ((<= (abs term) (* (abs sum) eps)))
		 ;;(format t "~3d: ~A / ~A = ~A~%" k top bot term)
		 (incf sum term))
	       sum)))
	  ((> (abs x) series-threshold)
	   ;; The series converges too slowly so use the identity:
	   ;;
	   ;;   li[3](-x/(1-x)) + li[3](1-x) + li[3](x)
	   ;;     = li[3](1) + %pi^2/6*log(1-x) - 1/2*log(x)*(log(1-x))^2 + 1/6*(log(1-x))^3
	   ;;
	   ;; Or
	   ;;
	   ;;   li[3](x) = li[3](1) + %pi^2/6*log(1-x) - 1/2*log(x)*(log(1-x))^2 + 1/6*(log(1-x))^3
	   ;;      - li[3](-x/(1-x)) - li[3](1-x)
	   ;;
	   ;; (See http://functions.wolfram.com/10.08.17.0048.01)
	   (let* ((dpi (%pi x))
		  (u (log x))
		  (s (/ (* u u u) 6))
		  (xc (- 1 x)))
	     (decf s (* 0.5 u u (log xc)))
	     (incf s (/ (* dpi dpi u) 6))
	     (decf s (li3numer (- (/ xc x))))
	     (decf s (li3numer xc))
	     (incf s (li3numer 1))))
	  (t
	   ;; Sum the power series.  threshold determines when the
	   ;; summation has converted.
	   (let* ((threshold (epsilon x))
		  (p (* x x x))
		  (term (/ p 27)))
	     (incf term (* 0.125 x x))
	     (incf term x)
	     (do* ((k 4 (1+ k))
		   (p1 (* p x) (* p1 x))
		   (h (/ p1 (* k k k)) (/ p1 (* k k k)))
		   (s h (+ s h)))
		  ((<= (abs (/ h s)) threshold)
		   (+ s term))))))))

(defun li2numer (z)
  ;; The series threshold to above sqrt(1/2) because li[2](%i) needs
  ;; the value of li[2](1/2-%i/2), and the magnitude of the argument
  ;; is sqrt(1/2) = 0.707.  If the threshold is below this, we get
  ;; into an infinite recursion oscillating between the two args.
  (let ((series-threshold .75))
    (cond ((zerop z)
	 0)
	((= z 1)
	 ;; %pi^2/6.  This follows from the series.
	 (/ (expt (%pi z) 2) 6))
	((= z -1)
	 ;; -%pi^2/12.  From the formula
	 ;;
	 ;;   li[s](-1) = (2^(1-s)-1)*zeta(s)
	 ;;
	 ;; (See http://functions.wolfram.com/10.08.03.0003.01)
	 (/ (expt (%pi z) 2) -12))
	((> (abs z) 1)
	 ;; Use
	 ;;   li[2](z) = -li[2](1/z) - 1/2*log(-z)^2 - %pi^2/6,
	 ;;
	 ;; valid for all z not in the intervale (0, 1).
	 ;;
	 ;; (See http://functions.wolfram.com/10.08.17.0013.01)
	 (- (+ (li2numer (/ z))
	       (* 0.5 (expt (log (- z)) 2))
	       (/ (expt (%pi z) 2) 6))))
	((> (abs z) series-threshold)
	 ;; For 0.5 <= |z|, where the series would not converge very quickly, use
	 ;;
	 ;;  li[2](z) = li[2](1/(1-z)) + 1/2*log(1-z)^2 - log(-z)*log(1-z) - %pi^2/6
	 ;;
	 ;; (See http://functions.wolfram.com/10.08.17.0016.01)
	 (let* ((1-z (- 1 z))
		(ln (log 1-z)))
	   (+ (li2numer (/ 1-z))
	      (* 0.5 ln ln)
	      (- (* (log (- z))
		    ln))
	      (- (/ (expt (%pi z) 2) 6)))))
	(t
	 ;; Series evaluation:
	 ;;
	 ;; li[2](z) = sum(z^k/k^2, k, 1, inf);
	 (let ((eps (epsilon z)))
	   (do* ((k 0 (1+ k))
		 (term z (* term (/ (* z k k)
				    (expt (1+ k) 2))))
		 (sum z (+ term sum)))
		((<= (abs (/ term sum)) eps)
		 sum)))))))

(defun polylog-power-series (s z)
  ;; Series evaluation:
  ;;
  ;; li[s](z) = sum(z^k/k^s, k, 1, inf);
  (let ((eps (epsilon z)))
    (do* ((k 1 (1+ k))
	  (term z (* term z (expt (/ (- k 1) k) s)))
	  (sum z (+ term sum)))
	 ((<= (abs (/ term sum)) eps)
	  ;; Return the value and the number of terms used, for
	  ;; debugging and for helping in determining the series
	  ;; threshold.
	  (values sum k)))))

(defun polylog-log-series (s z)
  ;; When x is on or near the unit circle the other
  ;; approaches don't work.  Use the expansion in powers of
  ;; log(z) (from cephes cpolylog)
  ;;
  ;;   li[s](z) = sum(Z(s-j)*(log(z))^j/j!, j = 0, inf)
  ;;
  ;; where Z(j) = zeta(j) for j != 1.  For j = 1:
  ;;
  ;;   Z(1) = -log(-log(z)) + sum(1/k, k, 1, s - 1)
  (flet ((zfun (j)
	   ;; Compute Z(j)
	   (cond ((= j 1)
		  (let ((sum (- (log (- (log z))))))
		    (+ sum
		       (loop for k from 1 below s
			     sum (/ k)))))
		 (t
		  (to (maxima::$zeta (maxima::to (float j (realpart z)))))))))
    (let* ((eps (epsilon z))
	   (logx (log z))
	   (logx^2 (* logx logx))
	   (top logx)
	   (bot 1)
	   (sum (zfun s)))
      ;; Compute sum(Z(s-j)*log(z)^j/j!, j = 1, s)
      (do* ((k 1 (1+ k))
	    (zf (zfun (- s k)) (zfun (- s k)))
	    (term (* (/ top bot) zf)
		  (* (/ top bot) zf)))
	   ((> k s))
	(when *debug-li-eval*
	  (format t "~3d: ~A / ~A * ~A => ~A~%" k top bot zf term))
	(incf sum term)
	(setf bot (* bot (1+ k)))
	(setf top (* top logx)))

      (when *debug-li-eval*
	(format t "s = ~A, sum = ~S top, bot = ~S ~S~%"
		s sum top bot))
      ;; Compute the sum for j = s+1 and up.  But since
      ;; zeta(-k) is 0 for k even, we only every other term.
      (do* ((k (+ s 1) (+ k 2))
	    (zf (zfun (- s k)) (zfun (- s k)))
	    (term (* (/ top bot) zf)
		  (* (/ top bot) zf)))
	   ((<= (abs term) (* (abs sum) eps))
	    ;; Return the result and the number of terms used for
	    ;; helping in determining the series threshold and the
	    ;; log-series threshold.
	    (values sum k))
	(when *debug-li-eval*
	  (format t "~3d: ~A / ~A = ~A~%" k top bot term))
	(incf sum term)
	(setf bot (* bot (+ k 1) (+ k 2)))
	(setf top (* top logx^2))))))

(defun polylog-inversion-formula (s z)
  ;; For z not in the interval (0, 1) and for integral n, we
  ;; have the identity:
  ;;
  ;; li[n](z) = -log(-z)^n/n! + (-1)^(n-1)*li[n](1/z)
  ;;               + 2 * sum(li[2*r](-1)/(n-2*r)!*log(-z)^(n-2*r), r, 1, floor(n/2))
  ;;
  ;; (See http://functions.wolfram.com/ZetaFunctionsandPolylogarithms/PolyLog/17/02/01/01/0008/)
  ;;
  ;; Or
  ;;
  ;; li[n](z) = -log(-z)^n/n! + (-1)^(n-1)*li[n](1/z)
  ;;               + 2 * sum(li[2*m-2*r](-1)/(n-2*m+2*r)!*log(-z)^(n-2*m+2*r), r, 0, m - 1)
  ;;
  ;; where m = floor(n/2).  Thus, n-2*m = 0 if n is even and 1 if n is odd.
  ;;
  ;; For n = 2*m, we have
  ;;
  ;; li[2*m](z) = -log(-z)^(2*m)/(2*m)! - li[2*m](1/z)
  ;;               + 2 * sum(li[2*r](-1)/(2*m-2*r)!*log(-z)^(2*m-2*r), r, 1, m)
  ;;            = -log(-z)^(2*m)/(2*m)! - li[2*m](1/z)
  ;;               + 2 * sum((li[2*m-2*r](-1)*log(-z)^(2*r+1))/(2*r+1)!,r,0,m-1);
  ;;
  ;; For n = 2*m+1, we have
  ;;
  ;; li[2*m+1](z) = -log(-z)^(2*m+1)/(2*m+1)! + li[2*m+1](1/z)
  ;;                 + 2 * sum(li[2*r](-1)/(2*m-2*r + 1)!*log(-z)^(2*m-2*r + 1), r, 1, m)
  ;;              = -log(-z)^(2*m+1)/(2*m+1)! + li[2*m+1](1/z)
  ;;                + 2 * sum((li[2*m-2*r](-1)*log(-z)^(2*r+1))/(2*r+1)!,r,0,m-1);
  ;; Thus,
  ;;
  ;; li[n](z) = -log(-z)^n/n! + (-1)^(n-1)*li[n](1/z)
  ;;               + 2 * sum((li[2*m-2*r](-1)*log(-z)^(2*r+1))/(2*r+1)!,r,0,floor(n/2)-1);
  (let* ((lgz (log (- z)))
	 (lgz^2 (* lgz lgz))
	 (half-s (floor s 2))
	 (neg-1 (float -1 (realpart z)))
	 (sum 0))
    (if (evenp s)
	(do* ((r 0 (1+ r))
	      (top (if (oddp s) lgz 1) (* top lgz^2))
	      (bot 1 (* bot (+ r r -1) (+ r r)))
	      (term (* (li-s-simp (* 2 (- half-s r)) neg-1)
		       (/ top bot))
		    (* (li-s-simp (* 2 (- half-s r)) neg-1)
		       (/ top bot))))
	     ((>= r half-s))
	  (incf sum term)
	  (when *debug-li-eval*
	    (format t "r = ~4d:  ~A / ~A, ~A; ~A~%" r top bot term sum)))
	(do* ((r 0 (1+ r))
	      (top (if (oddp s) lgz 1) (* top lgz^2))
	      (bot 1 (* bot (+ r r) (+ r r 1)))
	      (term (* (li-s-simp (* 2 (- half-s r)) neg-1)
		       (/ top bot))
		    (* (li-s-simp (* 2 (- half-s r)) neg-1)
		       (/ top bot))))
	     ((>= r half-s))
	  (incf sum term)
	  (when *debug-li-eval*
	    (format t "r = ~4d:  ~A / ~A, ~A; ~A~%" r top bot term sum))))
    (+ (+ sum sum)
       (- (/ (expt lgz s)
	     (maxima::take '(maxima::mfactorial) s)))
       (* (expt -1 (- s 1))
	  (li-s-simp s (/ z))))))

(defun li-s-simp (s z)
  (let ((series-threshold 0.5)
	(log-series-threshold 2))
    (cond ((zerop z)
	   (maxima::to (to 0.0)))
	  ((= z 1)
	   (maxima::$zeta (maxima::to (float s z))))
	  ((= z -1)
	   (- (* (- 1 (expt 2 (- 1 s)))
		 (to (li-s-simp s (- z))))))
	  ((<= (abs z) series-threshold)
	   (values (polylog-power-series s z)))
	  ((<= (abs z) log-series-threshold)
	   (values (polylog-log-series s z)))
	  ((> (abs z) 1.5)
	   (polylog-inversion-formula s z)))))
