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

(declare-top (special $zerobern ivars key-vars tlist))

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
    (do ((i (fix (arraycall flonum chebarr 0)) (1- i)))
	((< i 1) (-$ bn+1 (*$ bn+2 x)))
     (setq bn+2
	    (prog1 bn+1 (setq bn+1
			      (+$ (arraycall flonum chebarr i)
				  (-$ (*$ 2.0 x bn+1)
				      bn+2))))))))

(defun cheby-prime (x chebarr)
  (-$ (cheby x chebarr)
      (*$ (arraycall flonum chebarr 1) .5)))

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
		       (* 0.5d0 (log (- 1 x)) (log (/ (- 1 x) (* x x))))
		       (- (/ (cl:expt (coerce pi 'double-float) 2) 6))))
		   ((< x 1)
		    (slatec:dspenc x))
           ((= x 1)
            (/ (cl:expt (coerce pi 'double-float) 2) 6))
		   (t
		    ;; li[2](x) = -li[2](1/x)-log(-x)^2/2-%pi^2/6
		    (- (+ (li2 (/ x))
			  (/ (cl:expt (cl:log (- x)) 2) 2)
			  (/ (cl:expt (coerce pi 'double-float) 2) 6)))))))
    (complexify (li2 y))))


(defun li3numer (x)
  (cond ((= x 0.0) 0.0)
	((= x 1.0) 1.20205690)
	((< x -1.0)
	 (-$ (chebyli3 (//$ x)) (*$ 1.64493407 (log (-$ x)))
	     (//$ (expt (log (-$ x)) 3) 6.0)))
	((not (> x .5)) (chebyli3 x))
	((not (> x 2.0))
	 (let ((fac (*$ (expt (log x) 2) .5))) 
	   (m+t (+$ 1.20205690 
		    (-$ (*$ (log x)
			    (-$ 1.64493407 (chebyli2 (-$ 1.0 x))))
			(chebys12 (-$ 1.0 x))
			(*$ fac
			    (log (cond ((< x 1.0) (-$ 1.0 x))
				       ((1-$ x)))))))
		(cond ((< x 1.0) 0)
		      ((m*t (*$ fac -3.14159265) '$%i))))))
	(t (m+t (+$ (chebyli3 (//$ x)) (*$ 3.28986813 (log x))
		    (//$ (expt (log x) 3) -6.0))
		(m*t (*$ -1.57079633 (expt (log x) 2)) '$%i)))))

(defvar *li2* (make-array 15. :initial-contents '(14.0d0 1.93506430d0 .166073033d0 2.48793229d-2
						  4.68636196d-3 1.0016275d-3 2.32002196d-4
						  5.68178227d-5 1.44963006d-5 3.81632946d-6
						  1.02990426d-6 2.83575385d-7 7.9387055d-8
						  2.2536705d-8 6.474338d-9)))


(defvar *li3* (make-array 15. :initial-contents '(14.0d0 1.95841721d0 8.51881315d-2 8.55985222d-3
						  1.21177214d-3 2.07227685d-4 3.99695869d-5
						  8.38064066d-6 1.86848945d-6 4.36660867d-7
						  1.05917334d-7 2.6478920d-8 6.787d-9 
						  1.776536d-9 4.73417d-10)))

(defvar *s12* (make-array 18. :initial-contents '(17.0d0 1.90361778d0 .431311318d0 .100022507d0
						  2.44241560d-2 6.22512464d-3 1.64078831d-3
						  4.44079203d-4 1.22774942d-4 3.45398128d-5
						  9.85869565d-6 2.84856995d-6 8.31708473d-7
						  2.45039499d-7 7.2764962d-8 2.1758023d-8 6.546158d-9
						  1.980328d-9)))

(defun chebyli2 (x)
  (*$ x (cheby-prime (//$ (1+$ (*$ x 4.0)) 3.0) *li2*)))

(defun chebyli3 (x)
  (*$ x (cheby-prime (//$ (1+$ (*$ 4.0 x)) 3.0) *li3*)))

(defun chebys12 (x)
  (*$ (//$ (expt x 2) 4.0)
      (cheby-prime (//$ (1+$ (*$ 4.0 x)) 3.0) *s12*)))

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
		    ((or (greaterp p $maxpsifracnum)
			 (greaterp q $maxpsifracdenom) (bigp p) (bigp q)) ())
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
		       (m+t (msum f 1 (1- (// q 2)))
			    (let ((*k* (// q 2)))
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
		 (^ -1 s)
		 (m+t (psisimp1 s (m- 1 a))
		      (let ((dif (m* '$%pi
				     ($diff `((%cot) ,(m* '$%pi '$z)) '$z s)))
			    ($z (m-t a)))
			(declare (special $z))
			(meval dif)))))))
	    ((ratgreaterp a $maxpsinegint)  ;;; Reflection Formula
	     (m*t
	      (^ -1 s)
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
  (if (or (not (integerp subl)) (lessp subl -1))
      (tay-err "Unable to expand at a subscript in")
      (prog ((e 0) (sign 0) npw)
	 (declare (fixnum e) (fixnum sign))
	 (setq npw (//$ (float (car pw)) (float (cdr pw))))
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
			,(times (^ -1 (1+ subl))
				(factorial subl)) . 1))))
	  e (if (< subl 1) (- subl) -1)
	  sign (if (< subl 1) -1 (^ -1 subl)))
	 a (setq e (1+ e) sign (- sign))
	 (if (greaterp e npw) (return l)
	     (rplacd (last l)
		     `(((,e . 1)
			. ,(rctimes (rcplygam e)
				    (prep1 ($zeta (+ (1+ subl) e))))))))
	 (go a))))

(defun rcplygam (k)
  (declare (fixnum k) )
  (cond ((= subl -1) (cons sign k))
	((= subl 0) (cons sign 1))
	(t (prog1 (cons (times sign *last*) 1)
		  
	     (setq *last*
		   (*quo (times *last* (plus subl (add1 k)))
			 (add1 k)))))))

(defun plygam-ord (subl)
  (if (equal (car subl) -1) (ncons (rcone))
      `((,(- (1+ (car subl))) . 1))))

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
    (ifn (rcintegerp const)
	 (taylor2 (diff-expand `((%gamma) ,a) tlist))
	 (setq const (car const))
	 ;; Try to get the datum
	 (if (pscoefp arg)
	     (setq arg-c (get-lexp (m+t a (minus const)) (rcone)
				   (signp le const))))
	 (if (and arg-c (not (psp arg-c))) ; must be zero
	     (taylor2 (simplify `((%gamma) ,const)))
	     (let ((datum (get-datum (get-key-var
				      (gvar (or arg-c arg)))))
		   (ord (if arg-c (le (terms arg-c))
			    (le (n-term (terms arg))))))
	       (setq func (current-trunc datum))
	       (if (greaterp const 0)
		   (pstimes 
		    (let-pw datum (e- func ord)
			    (expand (m+t a (minus const)) '%gamma))
		    (let-pw datum (e+ func ord)
			    (tsprsum (m+t a (m-t '%%taylor-index%%))
				     `(%%taylor-index%% 1 ,const)
				     '%product)))
		   (pstimes 
		    (expand (m+t a (minus const)) '%gamma)
		    (let-pw datum (e+ func ord)
			    (psexpt 
			     (tsprsum (m+t a '%%taylor-index%%)
				      `(%%taylor-index%% 0
					,(minus (add1 const))) '%product)
			     (rcmone))))))))))

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
	       (cons (times (^ -1 sub) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a (m-t '%%taylor-index%%)) ,(- (1+ sub)))
			`(%%taylor-index%% 1 ,const) '%sum))
	      (pstimes
	       (cons (times (^ -1 (1+ sub)) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a '%%taylor-index%%) ,(- (1+ sub)))
			`(%%taylor-index%% 0 ,(- (1+ const))) '%sum))))))))

(declare-top (unspecial var subl *last* sign last-exp))
