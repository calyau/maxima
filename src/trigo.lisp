;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trigo)

(load-macsyma-macros mrgmac)

(def-simplifier sinh (y)
  (cond ((flonum-eval (mop form) y))
	((big-float-eval (mop form) y))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) 0)))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (ftake* '%sin (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (let ((fcn (caar y))
		    (arg (cadr y)))
		(cond ((eq '%asinh fcn)
		       arg)
		      ((eq '%acosh fcn)
		       ;; ratsimp(logarc(exponentialize(sinh(acosh(x))))),algebraic;
		       ;; -> sqrt(x-1)*sqrt(x+1)
		       (mul (power (sub arg 1) 1//2)
			    (power (add arg 1) 1//2)))
		      ((eq '%atanh fcn)
		       ;; radcan(logarc(exponentialize(sinh(atanh(x)))));
		       ;; -> x/(sqrt(1-x)*sqrt(1+x))
		       (div arg
			    (mul (power (sub 1 arg) 1//2)
				 (power (add 1 arg) 1//2))))))))
	((and $trigexpand (trigexpand '%sinh y)))
	($exponentialize (exponentialize '%sinh y))
	((and $halfangles (halfangle '%sinh y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%sinh (neg y))))
	(t (give-up))))

(def-simplifier cosh (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) 1)))
	((and $%iargs (multiplep y '$%i)) (ftake* '%cos (coeff y '$%i 1)))
	((and $triginverses (not (atom y))
	      (let ((fcn (caar y))
		    (arg (cadr y)))
		(cond ((eq '%acosh fcn)
		       arg)
		      ((eq '%asinh fcn)
		       ;; ex: cosh(asinh(x));
		       ;; ex,exponentialize,logarc;
		       ;; ratsimp(%),algebraic
		       ;; -> sqrt(x^2+1)
		       ;; 
		       (sqrt1+x^2 arg))
		      ((eq '%atanh fcn)
		       ;; ex: cosh(atanh(x))
		       ;; radcan(logarc(exponentialize(ex)))
		       ;; -> 1/sqrt(1-x)/sqrt(1+x)
		       (div 1
			    (mul (power (sub 1 arg) 1//2)
				 (power (add 1 arg) 1//2))))))))
	((and $trigexpand (trigexpand '%cosh y)))
	($exponentialize (exponentialize '%cosh y))
	((and $halfangles (halfangle '%cosh y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (ftake* '%cosh (neg y)))
	(t (give-up))))

(def-simplifier tanh (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) 0)))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (ftake* '%tan (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (let ((fcn (caar y))
		    (arg (cadr y)))
		(cond ((eq '%atanh fcn)
		       arg)
		      ((eq '%asinh fcn)
		       ;; ratsimp(logarc(exponentialize(tanh(asinh(x))))),algebraic;
		       ;; --> x/sqrt(1+x^2)
		       (div arg (sqrt1+x^2 arg)))
		      ((eq '%acosh fcn)
		       ;; ratsimp(logarc(exponentialize(tanh(acosh(x))))),algebraic;
		       ;; sqrt(x-1)*sqrt(x+1)/x
		       (div (mul (power (sub arg 1) 1//2)
				 (power (add arg 1) 1//2))
			    arg))))))
	((and $trigexpand (trigexpand '%tanh y)))
	($exponentialize (exponentialize '%tanh y))
	((and $halfangles (halfangle '%tanh y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%tanh (neg y))))
	(t (give-up))))

(def-simplifier coth (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) (domain-error y 'coth))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%cot (coeff y '$%i 1))))
	((and $triginverses (not (atom y)) (if (eq '%acoth (caar y)) (cadr y))))
	((and $trigexpand (trigexpand '%coth y)))
	($exponentialize (exponentialize '%coth y))
	((and $halfangles (halfangle '%coth y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%coth (neg y))))
	(t (give-up))))

(def-simplifier csch (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) (domain-error y 'csch)))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%csc (coeff y '$%i 1))))
	((and $triginverses (not (atom y)) (if (eq '%acsch (caar y)) (cadr y))))
	((and $trigexpand (trigexpand '%csch y)))
	($exponentialize (exponentialize '%csch y))
	((and $halfangles (halfangle '%csch y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%csch (neg y))))
	(t (give-up))))

(def-simplifier sech (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (zerop1 y)) 1)
	((and $%iargs (multiplep y '$%i)) (ftake* '%sec (coeff y '$%i 1)))
	((and $triginverses (not (atom y)) (if (eq '%asech (caar y)) (cadr y))))
	((and $trigexpand (trigexpand '%sech y)))
	($exponentialize (exponentialize '%sech y))
	((and $halfangles (halfangle '%sech y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (ftake* '%sech (neg y)))
	(t (give-up))))

(def-simplifier asin (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs
	      ;; Recognize some special values
	      (cond ((zerop1 y)
		     0)
		    ((equal 1 y)
		     (div '$%pi 2))
		    ((equal -1 y)
		     (div '$%pi -2))
		    ((alike1 y 1//2)
		     (div '$%pi 6))
		    ((alike1 y -1//2)
		     (div '$%pi -6))
		    ;; 1/sqrt(2)
		    ((alike1 y (power* 2 -1//2))
		     (div '$%pi 4))
		    ;; sqrt(3)/2
		    ((alike1 y (div (power* 3 1//2) 2))
		     (div '$%pi 3))
		    ;; -sqrt(3)/2
		    ((alike1 y (div (power* 3 1//2) -2))
		     (div '$%pi -3)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (ftake* '%asinh (coeff y '$%i 1))))
	((and (not (atom y)) (member (caar y) '(%cos %sin))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod
					 (if (eq (caar y) '%sin) (cadr y) (m- %pi//2 (cadr y)))
					 (m* 2 '$%pi))))
		    (cond ((eq (mlsp y-val %pi//2) t) y-val)
			  ((eq (mlsp y-val (m* 3 %pi//2)) t) (m- '$%pi y-val))
			  ((eq (mlsp y-val (m* 2 '$%pi)) t) (m- y-val (m* 2 '$%pi))))))))
	((and (eq $triginverses t) (not (atom y)) (eq (caar y) '%sin)
	      (if (and (member (csign (m- (cadr y) %pi//2)) '($nz $neg) :test #'eq)
		       (member (csign (m+ (cadr y) %pi//2)) '($pz $pos) :test #'eq))
		  (cadr y))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%sin (caar y)) (cadr y))))
	($logarc (logarc '%asin y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%asin (neg y))))
	(t (give-up))))

(def-simplifier acos (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs 
	      ;; Recognize some special values
	      (cond ((zerop1 y)
		     (div '$%pi 2))
		    ((equal 1 y)
		     0)
		    ((equal -1 y)
		     '$%pi)
		    ((alike1 y 1//2)
		     (div '$%pi 3))
		    ((alike1 y -1//2)
		     (mul '$%pi (div 2 3)))
	            ;; 1/sqrt(2)
		    ((alike1 y (power* 2 -1//2))
		     (div '$%pi 4))
		    ;; sqrt(3)/2
		    ((alike1 y (div (power* 3 1//2) 2))
		     (div '$%pi 6))
		    ;; -sqrt(3)/2
		    ((alike1 y (div (power* 3 1//2) -2))
		     (mul '$%pi (div 5 6))))))
	((and (not (atom y)) (member (caar y) '(%cos %sin))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod
					 (if (eq (caar y) '%cos) (cadr y) (m- %pi//2 (cadr y)))
					 (m* 2 '$%pi))))
		    (cond ((eq (mlsp y-val '$%pi) t) y-val)
			  ((eq (mlsp y-val (m* 2 '$%pi)) t) (m- (m* 2 '$%pi) y-val)))))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%cos (caar y)) (cadr y))))
	((and (eq $triginverses t) (not (atom y)) (eq (caar y) '%cos)
	      (if (and (member (csign (m- (cadr y) '$%pi)) '($nz $neg) :test #'eq)
		       (member (csign (cadr y)) '($pz $pos) :test #'eq))
		  (cadr y))))
	($logarc (logarc '%acos y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (sub '$%pi (ftake* '%acos (neg y))))
	(t (give-up))))

(def-simplifier acot (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
        ((and $%piargs
              (cond ((zerop1 y) (div '$%pi 2))
                    ((equal 1 y) (div '$%pi 4))
                    ((equal -1 y) (div '$%pi -4))
                    ;; 1/sqrt(3)
                    ((alike1 y '((mexpt) 3 ((rat) -1 2))) (div '$%pi 3))
                    ;; sqrt(3)
                    ((alike1 y '((mexpt) 3 ((rat) 1 2))) (div '$%pi 6)))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%acoth (coeff y '$%i 1))))
	((and (not (atom y)) (member (caar y) '(%cot %tan))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod
					 (if (eq (caar y) '%cot) (cadr y) (m- %pi//2 (cadr y)))
					 '$%pi)))
		    (cond ((eq (mlsp y-val %pi//2) t) y-val)
			  ((eq (mlsp y-val '$%pi) t) (m- y-val '$%pi)))))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%cot (caar y)) (cadr y))))
	((and (eq $triginverses t) (not (atom y)) (eq (caar y) '%cot)
	      (if (and (member (csign (m- (cadr y) %pi//2)) '($nz $neg) :test #'eq)
		       (member (csign (m+ (cadr y) %pi//2)) '($pz $pos) :test #'eq))
		  (cadr y))))
	($logarc (logarc '%acot y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%acot (neg y))))
	(t (give-up))))

(def-simplifier acsc (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
        ((and $%piargs
              (cond ((equal 1 y) (div '$%pi 2))
                    ((equal -1 y) (div '$%pi -2))
                    ((equal y 2) (div '$%pi 6))
                    ;; sqrt(2)
                    ((alike1 y '((mexpt) 2 ((rat) 1 2))) (div '$%pi 4))
                    ;; 2*sqrt(3)/3
                    ((alike1 y '((mtimes) 2 ((mexpt) 3 ((rat) -1 2))))
                     (div '$%pi 3)))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%acsch (coeff y '$%i 1))))
	((and (not (atom y)) (eq '%csc (caar y))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod (cadr y) (m* 2 '$%pi))))
		    (cond ((eq (mlsp y-val %pi//2) t) y-val)
			  ((eq (mlsp y-val (m* 3 %pi//2)) t) (m- '$%pi y-val))
			  ((eq (mlsp y-val (m* 2 '$%pi)) t) (m- y-val (m* 2 '$%pi))))))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%csc (caar y)) (cadr y))))
	($logarc (logarc '%acsc y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%acsc (neg y))))
	(t (give-up))))

(def-simplifier asec (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
        ((and $%piargs 
              (cond ((equal 1 y) 0) 
                    ((equal -1 y) '$%pi)
                    ((equal 2 y) (div '$%pi 3))
                    ;; sqrt(2)
                    ((alike1 y '((mexpt) 2 ((rat) 1 2))) (div '$%pi 4))
                    ;; 2/sqrt(3)
                    ((alike1 y '((mtimes) 2 ((mexpt) 3 ((rat) -1 2))))
                     (div '$%pi 6)))))
	((and (not (atom y)) (eq '%sec (caar y))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod (cadr y) (m* 2 '$%pi))))
		    (cond ((eq (mlsp y-val '$%pi) t) y-val)
			  ((eq (mlsp y-val (m* 2 '$%pi)) t) (m- (m* 2 '$%pi) y-val)))))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%sec (caar y)) (cadr y))))
	($logarc (logarc '%asec y))
	((apply-reflection-simp (mop form) y $trigsign))
	;;((and $trigsign (mminusp* y)) (sub '$%pi (ftake* '%asec (neg y))))
	(t (give-up))))

(def-simplifier asinh (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) y)))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (ftake* '%asin (coeff y '$%i 1))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%sinh (caar y)) (cadr y))))
	($logarc (logarc '%asinh y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%asinh (neg y))))
	(t (give-up))))

(def-simplifier acosh (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (equal y 1) 0)))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%cosh (caar y)) (cadr y))))
	($logarc (logarc '%acosh y))
	(t (give-up))))

(def-simplifier atanh (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 0)
			     ((or (equal y 1) (equal y -1)) (domain-error y 'atanh)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (ftake* '%atan (coeff y '$%i 1))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%tanh (caar y)) (cadr y))))
	($logarc (logarc '%atanh y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%atanh (neg y))))
	(t (give-up))))

(def-simplifier acoth (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (or (equal y 1) (equal y -1)) (domain-error y 'acoth))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%acot (coeff y '$%i 1))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%coth (caar y)) (cadr y))))
	($logarc (logarc '%acoth y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%acoth (neg y))))
	(t (give-up))))

(def-simplifier acsch (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (if (zerop1 y) (domain-error y 'acsch))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (ftake* '%acsc (coeff y '$%i 1))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%csch (caar y)) (cadr y))))
	($logarc (logarc '%acsch y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (ftake* '%acsch (neg y))))
	(t (give-up))))

(def-simplifier asech (y)
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((equal y 1) 0)
			     ((zerop1 y) (domain-error y 'asech)))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq '%sech (caar y)) (cadr y))))
	($logarc (logarc '%asech y))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (ftake* '%asech (neg y)))
	(t (give-up))))

(declare-top (special $trigexpandplus $trigexpandtimes))

(defmfun ($trigexpand :properties ((evfun t))) (e)
  (cond ((atom e) e)
	((specrepp e) ($trigexpand (specdisrep e)))
	((trigexpand (caar e) (cadr e)))
	(t (recur-apply #'$trigexpand e))))

(defun trigexpand (op arg)
  (cond ((atom arg) nil)
	((and $trigexpandplus (eq 'mplus (caar arg)))
	 (cond ((eq '%sin op) (sin/cos-plus (cdr arg) 1 '%sin '%cos -1))
	       ((eq '%cos op) (sin/cos-plus (cdr arg) 0 '%sin '%cos -1))
	       ((eq '%tan op) (tan-plus (cdr arg) '%tan -1))
	       ((eq '%cot op) (cot-plus (cdr arg) '%cot -1))
	       ((eq '%csc op) (csc/sec-plus (cdr arg) 1 '%csc '%sec -1))
	       ((eq '%sec op) (csc/sec-plus (cdr arg) 0 '%csc '%sec -1))
	       ((eq '%sinh op) (sin/cos-plus (cdr arg) 1 '%sinh '%cosh 1))
	       ((eq '%cosh op) (sin/cos-plus (cdr arg) 0 '%sinh '%cosh 1))
	       ((eq '%tanh op) (tan-plus (cdr arg) '%tanh 1))
	       ((eq '%coth op) (cot-plus (cdr arg) '%coth 1))
	       ((eq '%csch op) (csc/sec-plus (cdr arg) 1 '%csch '%sech 1))
	       ((eq '%sech op) (csc/sec-plus (cdr arg) 0 '%csch '%sech 1))))
	((and $trigexpandtimes (eq 'mtimes (caar arg)) (fixnump (cadr arg)))
	 (cond ((eq '%sin op) (sin/cos-times (cddr arg) 1 (cadr arg) '%sin '%cos -1))
	       ((eq '%cos op) (sin/cos-times (cddr arg) 0 (cadr arg) '%sin '%cos -1))
	       ((eq '%tan op) (tan-times (cddr arg) (cadr arg) '%tan -1))
	       ((eq '%cot op) (cot-times (cddr arg) (cadr arg) '%cot -1))
	       ((eq '%csc op) (csc/sec-times (cddr arg) 1 (cadr arg) '%csc '%sec -1))
	       ((eq '%sec op) (csc/sec-times (cddr arg) 0 (cadr arg) '%csc '%sec -1))
	       ((eq '%sinh op) (sin/cos-times (cddr arg) 1 (cadr arg) '%sinh '%cosh 1))
	       ((eq '%cosh op) (sin/cos-times (cddr arg) 0 (cadr arg) '%sinh '%cosh 1))
	       ((eq '%tanh op) (tan-times (cddr arg) (cadr arg) '%tanh 1))
	       ((eq '%coth op) (cot-times (cddr arg) (cadr arg) '%coth 1))
	       ((eq '%csch op) (csc/sec-times (cddr arg) 1 (cadr arg) '%csch '%sech 1))
	       ((eq '%sech op) (csc/sec-times (cddr arg) 0 (cadr arg) '%csch '%sech 1))))))

(defun sin/cos-plus (l n f1 f2 flag)
  (do ((i n (+ 2 i))
       (len (length l))
       (sign 1 (* flag sign))
       (result))
      ((> i len) (simplify (cons '(mplus) result)))
    (setq result (mpc (cond ((minusp sign) '(-1 (mtimes)))
			    (t '((mtimes)))) l result f1 f2 len i))))

(defun tan-plus (l f flag) 
  (do ((i 1 (+ 2 i))
       (sign 1 (* flag sign))
       (len (length l))
       (num)
       (den (list 1)))
      ((> i len) (div* (cons '(mplus) num) (cons '(mplus) den)))
    (setq num (mpc1 (list sign '(mtimes)) l num f len i)
	  den (cond ((= len i) den)
		    (t (mpc1 (list (* flag sign) '(mtimes)) l den f len (1+ i)))))))

(defun cot-plus (l f flag)
  (do ((i (length l) (- i 2)) (len (length l)) (sign 1 (* flag sign)) (num) (den))
      ((< i 0) (div* (cons '(mplus) num) (cons '(mplus) den)))
    (setq num (mpc1 (list sign '(mtimes)) l num f len i)
	  den (cond ((= 0 i) den)
		    (t (mpc1 (list sign '(mtimes)) l den f len (1- i)))))))

(defun csc/sec-plus (l n f1 f2 flag)
  (div* (do ((l l (cdr l))
	     (result))
	    ((null l) (cons '(mtimes) result))
	  (setq result (cons (ftake* f1 (car l)) (cons (ftake* f2 (car l)) result))))
	(sin/cos-plus l n f2 f1 flag)))

(defun sin/cos-times (l m n f1 f2 flag)
  ;; Assume m,n < 2^17, but Binom may become big
  ;; Flag is 1 or -1
  (setq f1 (ftake* f1 (cons '(mtimes) l)) f2 (ftake* f2 (cons '(mtimes) l)))
  (do ((i m (+ 2 i))
       (end (abs n))
       (result)
       (binom (cond ((= 0 m) 1)
		    (t (abs n)))
	      (quotient (* flag (- end i 1) (- end i) binom) (* (+ 2 i) (1+ i)))))
      ((> i end) (setq result (simplify (cons '(mplus) result)))
       (cond ((and (= 1 m) (minusp n)) (neg result)) (t result)))
    (setq result (cons (mul binom (power f1 i) (power f2 (- end i))) result))))

(defun tan-times (l n f flag)
  (setq f (ftake* f (cons '(mtimes) l)))
  (do ((i 1 (+ 2 i))
       (end (abs n))
       (num)
       (den (list 1))
       (binom (abs n) (quotient (* (- end i 1) binom) (+ 2 i))))
      ((> i end) (setq num (div* (cons '(mplus) num) (cons '(mplus) den)))
       (cond ((minusp n) (neg num))
	     (t num)))
    (setq num (cons (mul binom (power f i)) num) 
	  den (cond ((= end i) den)
		    (t (cons (mul (setq binom (truncate (* flag (- end i) binom) (1+ i)))
				  (power f (1+ i)))
			     den))))))

(defun cot-times (l n f flag)
  (setq f (ftake* f (cons '(mtimes) l)))
  (do ((i (abs n) (- i 2))
       (end (abs n))
       (num)
       (den)
       (binom 1 (truncate (* flag (1- i) binom) (- end i -2))))
      ((< i 0) (setq num (div* (cons '(mplus) num) (cons '(mplus) den)))
       (if (minusp n) (neg num) num))
    (setq num (cons (mul binom (power f i)) num)
	  den (if (= 0 i)
		  den
		  (cons (mul (setq binom (truncate (* i binom) (- end i -1))) (power f (1- i))) den)))))

(defun csc/sec-times (l m n f1 f2 flag)
  (div* (mul (power (ftake* f1 (cons '(mtimes) l)) (abs n))
	     (power (ftake* f2 (cons '(mtimes) l)) (abs n)))
	(sin/cos-times l m n f2 f1 flag)))

(defun mpc (dl ul result f1 f2 di ui) 
  (cond ((= 0 ui)
	 (cons (revappend dl (mapcar #'(lambda (l) (ftake* f2 l)) ul)) result))
	((= di ui)
	 (cons (revappend dl (mapcar #'(lambda (l) (ftake* f1 l)) ul)) result))
	(t (mpc (cons (ftake* f1 (car ul)) dl) (cdr ul)
		(mpc (cons (ftake* f2 (car ul)) dl)
		     (cdr ul) result f1 f2 (1- di) ui) f1 f2
		(1- di) (1- ui)))))

(defun mpc1 (dl ul result f di ui) 
  (cond ((= 0 ui) (cons (reverse dl) result))
	((= di ui)
	 (cons (revappend dl (mapcar #'(lambda (l) (ftake* f l)) ul)) result))
	(t (mpc1 (cons (ftake* f (car ul)) dl) (cdr ul)
		 (mpc1 dl (cdr ul) result f (1- di) ui) f
		 (1- di) (1- ui)))))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; End:
