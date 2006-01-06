;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module float)

;; EXPERIMENTAL BIGFLOAT PACKAGE VERSION 2- USING BINARY MANTISSA 
;; AND POWER-OF-2 EXPONENT.
;; EXPONENTS MAY BE BIG NUMBERS NOW (AUG. 1975 --RJF)
;; Modified:	July 1979 by CWH to run on the Lisp Machine and to comment
;;              the code.
;;		August 1980 by CWH to run on Multics and to install
;;		new FIXFLOAT.
;;		December 1980 by JIM to fix BIGLSH not to pass LSH a second
;;		argument with magnitude greater than MACHINE-FIXNUM-PRECISION.

;; Number of bits of precision in a fixnum and in the fields of a flonum for
;; a particular machine.  These variables should only be around at eval
;; and compile time.  These variables should probably be set up in a prelude
;; file so they can be accessible to all Macsyma files.

;;#+nil
;;#.(SETQ MACHINE-FIXNUM-PRECISION
;;	#+(OR PDP10 H6180)   36.
;;	#+cl (integer-length most-positive-fixnum)
;;	;#+LISPM		     24.
;;	#+NIL		     30.
;;	#+Franz		     32.

;;#|
;;	MACHINE-MANTISSA-PRECISION
;;	#+(OR PDP10 H6180)   27.
;;	#+cl(integer-length (integer-decode-float most-positive-double-float))
;;	;#+LISPM		     32.
;;	#+(OR NIL Franz)     56.	;double-float.  Long would be 113.
;;|#
;;	;; Not used anymore, but keep it around anyway in case
;;	;; we need it later.

;;	MACHINE-EXPONENT-PRECISION
;;	#+(OR PDP10 H6180)    8.
;;	#+cl
;;	(integer-length
;;	 (multiple-value-bind (a b)
;;	     (integer-decode-float most-positive-double-float)
;;	   b))
;;	;#+LISPM		     11.
;;	#+(OR NIL Franz)      8.	;Double float.  Long would be 15.
;;	)

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)
    (defconstant +machine-fixnum-precision+
      (integer-length most-positive-fixnum)))

;; Hmm, this doesn't seem to be used anywhere, but we leave here anyway. 
;;(defconstant +machine-exponent-precision+
;;  (integer-length (multiple-value-bind (a b)
;;		      (integer-decode-float most-positive-double-float)
;;		    b)))
;; External variables

(defmvar $float2bf nil
  "If TRUE, no MAXIMA-ERROR message is printed when a floating point number is
converted to a bigfloat number.")

(defmvar $bftorat nil
  "Controls the conversion of bigfloat numbers to rational numbers.  If
FALSE, RATEPSILON will be used to control the conversion (this results in
relatively small rational numbers).  If TRUE, the rational number generated
will accurately represent the bigfloat.")

(defmvar $bftrunc t
  "Needs to be documented")

(defmvar $fpprintprec 0
  "Needs to be documented"
  fixnum)

(defmvar $fpprec 16.
  "Number of decimal digits of precision to use when creating new bigfloats.
One extra decimal digit in actual representation for rounding purposes.")

(defmvar bigfloatzero '((bigfloat simp 56.) 0 0)
  "Bigfloat representation of 0" in-core)
(defmvar bigfloatone  '((bigfloat simp 56.) #.(expt 2 55.) 1)
  "Bigfloat representation of 1" in-core)
(defmvar bfhalf	      '((bigfloat simp 56.) #.(expt 2 55.) 0)
  "Bigfloat representation of 1/2")
(defmvar bfmhalf      '((bigfloat simp 56.) #.(minus (expt 2 55.)) 0)
  "Bigfloat representation of -1/2")
(defmvar bigfloat%e   '((bigfloat simp 56.) 48968212118944587. 2)
  "Bigfloat representation of %E")
(defmvar bigfloat%pi  '((bigfloat simp 56.) 56593902016227522. 2)
  "Bigfloat representation of %pi")
(defmvar bigfloat%gamma '((bigfloat simp 56.) 41592772053807304. 0)
  "Bigfloat representation of %gamma")
;; Internal specials

;; Number of bits of precision in the mantissa of newly created bigfloats. 
;; FPPREC = ($FPPREC+1)*(Log base 2 of 10)

(defvar fpprec)

;;(declare-top (FIXNUM FPPREC))

;; FPROUND uses this to return a second value, i.e. it sets it before
;; returning.  This number represents the number of binary digits its input
;; bignum had to be shifted right to be aligned into the mantissa.  For
;; example, aligning 1 would mean shifting it FPPREC-1 places left, and
;; aligning 7 would mean shifting FPPREC-3 places left.

(defvar *m)

;;(declare-top (FIXNUM *M))

;; *DECFP = T if the computation is being done in decimal radix.  NIL implies
;; base 2.  Decimal radix is used only during output.

(defvar *decfp nil)

(defvar max-bfloat-%pi bigfloat%pi)
(defvar max-bfloat-%e  bigfloat%e)
(defvar max-bfloat-%gamma bigfloat%gamma)

(declare-top (special *cancelled $float $bfloat $ratprint $ratepsilon
		      $domain $m1pbranch adjust))
;; *** Local fixnum declarations ***
;; *** Be careful of this brain-damage ***
;;	 (FIXNUM I N EXTRADIGS)
;;	 (*EXPR $BFLOAT $FLOAT)
;;	 (MUZZLED T)) 

;; Representation of a Bigfloat:  ((BIGFLOAT SIMP precision) mantissa exponent)
;; precision -- number of bits of precision in the mantissa.  
;; 		precision = (haulong mantissa)
;; mantissa -- a signed integer representing a fractional portion computed by
;; 	       fraction = (// mantissa (^ 2 precision)).
;; exponent -- a signed integer representing the scale of the number.
;; 	       The actual number represented is (f* fraction (^ 2 exponent)).

(defun hipart (x nn)
  (if (bigp nn)
      (abs x)
      (haipart x nn)))

(defun fpprec1 (assign-var q) 
  assign-var				; ignored
  (if (or (not (fixnump q)) (< q 1))
      (merror "Improper value for `fpprec':~%~M" q))
  (setq fpprec (f+ 2 (haulong (expt 10. q)))
	bigfloatone ($bfloat 1) bigfloatzero ($bfloat 0)
	bfhalf (list (car bigfloatone) (cadr bigfloatone) 0)
	bfmhalf (list (car bigfloatone) (minus (cadr bigfloatone)) 0))
  q) 

;; FPSCAN is called by lexical scan when a
;; bigfloat is encountered.  For example, 12.01B-3
;; would be the result of (FPSCAN '(/1 /2) '(/0 /1) '(/- /3))
;; Arguments to FPSCAN are a list of characters to the left of the
;; decimal point, to the right of the decimal point, and in the exponent.

(defun fpscan (lft rt exp &aux (*read-base* 10.) (*m 1) (*cancelled 0))
  (setq exp (readlist exp))
  ;; Log[2](10) is 3.3219 ...
  ;; This should be computed at compile time.
  (bigfloatp
   (let ((fpprec (plus 4 fpprec (haulong exp)
		       (fix (add1 (times 3.322 (length lft))))))
	 $float temp)
     (setq temp (add (readlist lft)
		     (div (readlist rt) (expt 10. (length rt)))))
     ($bfloat (cond ((greaterp (abs exp) 1000.)
		     (cons '(mtimes) (list temp (list '(mexpt) 10. exp))))
		    (t (mul2 temp (power 10. exp))))))))

(defun dim-bigfloat (form result)
  (dimension-atom (maknam (fpformat form)) result))

(defun fpformat (l)
  (if (not (memq 'simp (cdar l)))
      (setq l (cons (cons (caar l) (cons 'simp (cdar l))) (cdr l))))
  (cond ((equal (cadr l) 0)
	 (if (not (equal (caddr l) 0))
	     (mtell "Warning - an incorrect form for 0.0b0 has been generated."))
	 (list '|0| '|.| '|0| '|b| '|0|))
	(t ;; L IS ALWAYS POSITIVE FP NUMBER
	 (let ((extradigs (fix (add1 (quotient (haulong (caddr l)) 3.32))))
	       (*m 1) (*cancelled 0)) 
	   (setq l
		 ((lambda (*decfp fpprec of l expon)
		    (setq expon (difference (cadr l) of))
		    (setq l
			  (cond ((minusp expon)
				 (fpquotient (intofp (car l))
					     (fpintexpt 2 (minus expon) of)))
				(t (fptimes* (intofp (car l))
					     (fpintexpt 2 expon of)))))
		    (setq fpprec (plus (minus extradigs) fpprec))
		    (list (fpround (car l))
			  (plus (minus extradigs) *m (cadr l))))
		  t
		  (plus extradigs (decimalsin (difference (caddar l) 2)))
		  (caddar l)
		  (cdr l)
		  nil)))
	 (let (#-cl (*print-base* 10.)   #+cl (*print-base* 10.)
		    #-cl (*nopoint t) #+cl *print-radix*
		    (l1 nil))
	   (setq l1 (cond ((not $bftrunc) (explodec (car l)))
			  (t (do ((l (nreverse (explodec (car l))) (cdr l)))
				 ((not (eq '|0| (car l))) (nreverse l))))))
	   (nconc (ncons (car l1)) (ncons '|.|)
		  (or (and (cdr l1)
			   (cond ((or (zerop $fpprintprec)
				      (not (< $fpprintprec $fpprec))
				      (null (cddr l1)))
				  (cdr l1))
				 (t (setq l1 (cdr l1))
				    (do ((i $fpprintprec (f1- i)) (l2))
					((or (< i 2) (null (cdr l1)))
					 (cond ((not $bftrunc) (nreverse l2))
					       (t (do ((l3 l2 (cdr l3)))
						      ((not (eq '|0| (car l3)))
						       (nreverse l3))))))
				      (setq l2 (cons (car l1) l2) l1 (cdr l1))))))
		      (ncons '|0|))
		  (ncons '|b|)
		  (explodec (sub1 (cadr l))))))))

(defun bigfloatp (x) 
  (prog nil
     (cond ((not ($bfloatp x)) (return nil))
	   ((= fpprec (caddar x)) (return x))
	   ((> fpprec (caddar x))
	    (setq x (bcons (list (fpshift (cadr x) (difference fpprec (caddar x)))
				 (caddr x)))))
	   (t (setq x (bcons (list (fpround (cadr x))
				   (plus (caddr x) *m fpprec (minus (caddar x))))))))
     (return (cond ((equal (cadr x) 0) (bcons (list 0 0))) (t x))))) 

(defun bigfloat2rat (x)
  (setq x (bigfloatp x))
  ((lambda ($float2bf exp y sign) 
     (setq exp (cond ((minusp (cadr x))
		      (setq sign t y (fpration1 (cons (car x) (fpabs (cdr x)))))
		      (rplaca y (times -1 (car y))))
		     (t (fpration1 x))))
     (cond ($ratprint (princ "`rat' replaced ")
		      (cond (sign (princ "-")))
		      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
		      (princ " by ") (princ (car exp)) (tyo #. forward-slash-char) (princ (cdr exp))
		      (princ " = ") (setq x ($bfloat (list '(rat simp) (car exp) (cdr exp))))
		      (cond (sign (princ "-")))
		      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
		      (terpri)))
     exp) 
   t nil nil nil))

(defun fpration1 (x)
  ((lambda (fprateps)
     (or (and (equal x bigfloatzero) (cons 0 1))
	 (prog (y a)
	    (return (do ((xx x (setq y (invertbigfloat
					(bcons (fpdifference (cdr xx) (cdr ($bfloat a)))))))
			 (num (setq a (fpentier x))
			      (plus (times (setq a (fpentier y)) num) onum))
			 (den 1 (plus (times a den) oden))
			 (onum 1 num)
			 (oden 0 den))
			((and (not (zerop den))
			      (not (fpgreaterp
				    (fpabs (fpquotient
					    (fpdifference (cdr x)
							  (fpquotient (cdr ($bfloat num))
								      (cdr ($bfloat den))))
					    (cdr x)))
				    fprateps)))
			 (cons num den)))))))
   (cdr ($bfloat (cond ($bftorat (list '(rat simp) 1 (exptrl 2 (f1- fpprec))))
		       (t $ratepsilon))))))

;; Convert a floating point number into a bigfloat.
(defun floattofp (x) 
  (unless $float2bf
    (mtell "Warning:  Float to bigfloat conversion of ~S~%" x))
  (setq x (fixfloat x))
  (fpquotient (intofp (car x)) (intofp (cdr x))))

;; Convert a bigfloat into a floating point number.
(defmfun fp2flo (l)
  (let ((precision (caddar l))
	(mantissa (cadr l))
	(exponent (caddr l))
	(fpprec #.machine-mantissa-precision)
	(*m 0))
    ;;Round the mantissa to the number of bits of precision of the machine,
    ;;and then convert it to a floating point fraction.
    (setq mantissa (quotient (fpround mantissa)
			     #.(expt 2.0 machine-mantissa-precision)))
    ;; Multiply the mantissa by the exponent portion.  I'm not sure
    ;; why the exponent computation is so complicated. Using
    ;; scale-float will prevent possible overflow unless the result
    ;; really would.
    (setq precision
	  (errset (scale-float mantissa (f+ exponent (minus precision) *m
					    #.machine-mantissa-precision))
		  nil))
    (if precision
	(car precision)
	(merror "Floating point overflow in converting ~:M to flonum" l))))

;; New machine-independent version of FIXFLOAT.  This may be buggy. - CWH
;; It is buggy!  On the PDP10 it dies on (RATIONALIZE -1.16066076E-7) 
;; which calls FLOAT on some rather big numbers.  ($RATEPSILON is approx. 
;; 7.45E-9) - JPG

(defun fixfloat (x)
  (let (($ratepsilon #.(expt 2.0 (f- machine-mantissa-precision))))
    (maxima-rationalize x)))

;; Takes a flonum arg and returns a rational number corresponding to the flonum
;; in the form of a dotted pair of two integers.  Since the denominator will
;; always be a positive power of 2, this number will not always be in lowest
;; terms.

;; PDP-10 Floating Point number format:
;; 1 bit sign -- 0 = negative 1 = positive
;; 8 bit exponent -- If positive, excess 128 encoding used, i.e.
;;   -128 exponent = 0 and +127 exponent = 255.  If number is negative,
;;   ones complement of excess 128 is used.  This is done so that the
;;   representation of the negation of a floating point number is the twos
;;   complement of the number interpreted as an integer.  If x is the number in
;;   the 8 bit field, x-128 will yield the exponent if the sign bit is off, and
;;   127-x will yield the exponent if the sign bit is on.
;; 27 bit fraction -- If the number is normalized, this fraction will be
;;   between 1/2 and 1-2^-27 inclusive, i.e. the msb of the fraction will
;;   always be 1.  The fraction is stored in two's complement so the most
;;   negative flonum is (fsc (rot 3 -1) 0) and the most positive flonum is
;;   (fsc (lsh -1 -1) 0).

;; Old definition which explicitly hacks floating point representations.
;;#+PDP10 (PROGN 'COMPILE
;;  (DECLARE (CLOSED T))
;;  (DEFUN FIXFLOAT (X) 
;; 	(PROG (NEG NUM EXPONENT DENOM) 
;; 	      (COND ((LESSP X 0.0) (SETQ NEG -1.) (SETQ X (MINUS X))))
;; 	      (SETQ X (LSH X 0))
;; 	      (SETQ EXPONENT (DIFFERENCE (LSH X -27.) 129.))
;; 	      (SETQ NUM (LSH (LSH X 9.) -9.))
;; 	      (SETQ DENOM #. (f* 1 (^ 2 26.)))		;(^ 2 26)
;; 	      (COND ((LESSP EXPONENT 0)
;; 		     (SETQ DENOM (TIMES DENOM (EXPT 2 (MINUS EXPONENT)))))
;; 		    (T (SETQ NUM (TIMES NUM (EXPT 2 EXPONENT)))))
;; 	      (IF NEG (SETQ NUM (MINUS NUM)))
;; 	      (RETURN (CONS NUM DENOM)))) 
;;  (DECLARE (CLOSED NIL))
;; )

;; Format of a floating point number on the Lisp Machine:
;; 
;; High 8 bits of mantissa (plus sign bit) ---------\
;; Exponent (excess 1024) --------------\           |
;; Type of extended number --\          |           |
;; DTP-HEADER (7) ---\       |          |           |
;; Not used --\      |       |          |           |
;;            |      |       |          |           |
;;         ------------------------------------------------
;;         |  3  |   5   |   5   |     11     |     8     |
;;         ------------------------------------------------
;;         ------------------------------------------------
;;         |  3  |   5   |             24                 |
;;         ------------------------------------------------
;;            |      |                  |
;; Not used --/      |                  |
;; DTP-FIX (5) ------/                  |
;; Low 24 bits of mantissa -------------/

;; #+LISPM
;; (DEFUN FIXFLOAT (X)
;;   (LET ((EXPONENT (f- (%P-LDB-OFFSET #O 1013 X 0) #O 2000))
;; 	(NUM (%P-LDB-OFFSET #O 0010 X 0))
;; 	(DENOM 1_31.))
;;     ;;Extract the high portion of the mantissa and left justify it within
;;     ;;a fixnum.
;;     (SETQ NUM (LSH NUM 16.))
;;     ;;Then extract the high 16 bits of the low portion of the mantissa and
;;     ;;store into the fixnum.
;;     (SETQ NUM (LOGIOR NUM (%P-LDB-OFFSET #O 1020 X 1)))
;;     ;;Finally, convert what we've got into a bignum by shifting left by
;;     ;;8 bits and add in low 8 bits of the low portion of the mantissa.
;;     (SETQ NUM (LOGIOR (f* NUM 1_8) (%P-LDB-OFFSET #O 0010 X 1)))
;;     (COND ((< EXPONENT 0)
;; 	   (SETQ DENOM (f* DENOM (^ 2 (f- EXPONENT)))))
;; 	  (T (SETQ NUM (f* NUM (^ 2 EXPONENT)))))
;;     (CONS NUM DENOM)))

;; The format of a floating point number on the H6180 is very similar
;; to that on the PDP-10.  There are 8 bits of exponent, 1 bit of sign,
;; and 27 bits of mantissa in that order.  The exponent is stored
;; in twos complement, and the low order 28 bits are the mantissa
;; in twos complement.

;; #+H6180
;; (DEFUN FIXFLOAT (X) 
;;       (PROG (NEG NUM EXPONENT DENOM) 
;; 	     (WHEN (LESSP X 0.0) (SETQ NEG -1) (SETQ X (-$ X)))
;; 	     (SETQ X (LSH X 0))
;; 	     (SETQ EXPONENT (LSH X -28.))
;; 	     (AND (> EXPONENT 177) (SETQ EXPONENT (DIFFERENCE EXPONENT 400)))
;; 	     (SETQ NUM (BOOLE  BOOLE-AND X 1777777777))	;2^29-1
;; 	     (SETQ DENOM
;; 		   (TIMES 1000000000	;2^27
;; 			  (COND ((LESSP EXPONENT 0)
;; 				 (EXPT 2 (MINUS EXPONENT)))
;; 				(T (SETQ NUM
;; 					 (TIMES NUM (EXPT 2 EXPONENT)))
;; 				   1))))
;; 	     (IF NEG (SETQ NUM (MINUS NUM)))
;; 	     (RETURN (CONS NUM DENOM)))) 

(defun bcons (s)
  `((bigfloat simp ,fpprec) . ,s)) 

(defmfun $bfloat (x) 
  (let (y)
    (cond ((bigfloatp x))
	  ((or (numberp x)
	       (memq x '($%e $%pi $%gamma)))
	   (bcons (intofp x)))
	  ((or (atom x) (memq 'array (cdar x)))
	   (if (eq x '$%phi)
	       ($bfloat '((mtimes simp)
			  ((rat simp) 1 2)
			  ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
	       x))
	  ((eq (caar x) 'mexpt)
	   (if (equal (cadr x) '$%e)
	       (*fpexp (caddr x))
	       (exptbigfloat ($bfloat (cadr x)) (caddr x))))
	  ((eq (caar x) 'mncexpt)
	   (list '(mncexpt) ($bfloat (cadr x)) (caddr x)))
	  ((setq y (safe-get (caar x) 'floatprog))
	   (funcall y (mapcar #'$bfloat (cdr x))))
	  ((or (trigp (caar x)) (arcp (caar x)) (eq (caar x) '$entier))
	   (setq y ($bfloat (cadr x)))
	   (if ($bfloatp y)
	       (cond ((eq (caar x) '$entier) ($entier y))
		     ((arcp (caar x))
		      (setq y ($bfloat (logarc (caar x) y)))
		      (if (free y '$%i)
			  y (let ($ratprint) (fparcsimp ($rectform y)))))
		     ((memq (caar x) '(%cot %sec %csc))
		      (invertbigfloat
		       ($bfloat (list (ncons (safe-get (caar x) 'recip)) y))))
		     (t ($bfloat (exponentialize (caar x) y))))
	       (subst0 (list (ncons (caar x)) y) x)))
	  (t (recur-apply #'$bfloat x))))) 

(defprop mplus addbigfloat floatprog)
(defprop mtimes timesbigfloat floatprog)
(defprop %sin sinbigfloat floatprog)
(defprop %cos cosbigfloat floatprog)
(defprop rat ratbigfloat floatprog)
(defprop %atan atanbigfloat floatprog)
(defprop %tan tanbigfloat floatprog)
(defprop %log logbigfloat floatprog)
(defprop mabs mabsbigfloat floatprog)

(defmfun addbigfloat (h)
  (prog (fans tst r nfans)
     (setq fans (setq tst bigfloatzero) nfans 0)
     (do ((l h (cdr l))) ((null l))
       (cond ((setq r (bigfloatp (car l)))
	      (setq fans (bcons (fpplus (cdr r) (cdr fans)))))
	     (t (setq nfans (list '(mplus) (car l) nfans)))))
     (return (cond ((equal nfans 0) fans)
		   ((equal fans tst) nfans)
		   (t (simplify (list '(mplus) fans nfans))))))) 

(defmfun ratbigfloat (l)
  (bcons (fpquotient (cdar l) (cdadr l)))) 

(defun decimalsin (x) 
  (do ((i (quotient (times 59. x) 196.)	;log[10](2)=.301029
	  (f1+ i))) (nil) (if (> (haulong (expt 10. i)) x) (return (f1- i))))) 

(defmfun atanbigfloat (x) (*fpatan (car x) (cdr x))) 

(defmfun *fpatan (a y) 
  (fpend (let ((fpprec (plus 8. fpprec)))
	   (if (null y)
	       (if ($bfloatp a) (fpatan (cdr ($bfloat a)))
		   (list '(%atan) a))
	       (fpatan2 (cdr ($bfloat a))
			(cdr ($bfloat (car y))))))))

;; Bigfloat atan
(defun fpatan (x)
  (prog (term x2 ans oans one two tmp)
     (setq one (intofp 1) two (intofp 2))
     (cond ((fpgreaterp (fpabs x) one)
	    ;; |x| > 1.
	    ;;
	    ;; Use A&S 4.4.5:
	    ;;    atan(x) + acot(x) = +/- pi/2 (+ for x >= 0, - for x < 0)
	    ;;
	    ;; and A&S 4.4.8
	    ;;    acot(z) = atan(1/z)
	    (setq tmp (fpquotient (fppi) two))
	    (setq ans (fpdifference tmp (fpatan (fpquotient one x))))
	    (return (cond ((fpgreaterp ans tmp)
			   (fpdifference ans (fppi)))
			  (t ans))))
	   ((fpgreaterp (fpabs x) (fpquotient one two))
	    ;; |x| > 1/2
	    ;;
	    ;; Use A&S 4.4.42, third formula:
	    ;;
	    ;; atan(z) = z/(1+z^2)*[1 + 2/3*r + (2*4)/(3*5)*r^2 + ...]
	    ;;
	    ;; r = z^2/(1+z^2)
	    (setq tmp (fpquotient x (fpplus (fptimes* x x) one)))
	    (setq x2 (fptimes* x tmp) term (setq ans one))
	    (do ((n 0 (f1+ n)))
		((equal ans oans))
	      (setq term
		    (fptimes* term (fptimes* x2 (fpquotient
						 (intofp (f+ 2 (f* 2 n)))
						 (intofp (f+ (f* 2 n) 3))))))
	      (setq oans ans ans (fpplus term ans)))
	    (setq ans (fptimes* tmp ans)))
	   (t
	    ;; |x| <= 1/2.  Use Taylor series (A&S 4.4.42, first
	    ;; formula).
	    (setq ans x x2 (fpminus (fptimes* x x)) term x)
	    (do ((n 3 (f+ n 2)))
		((equal ans oans))
	      (setq term (fptimes* term x2))
	      (setq oans ans 
		    ans (fpplus ans (fpquotient term (intofp n)))))))
     (return ans)))

(defun fpatan2 (y x)			; ATAN(Y/X) from -PI to PI
  (cond ((equal (car x) 0)		; ATAN(INF), but what sign?
	 (cond ((equal (car y) 0) (merror "atan(0//0) has been generated."))
	       ((minusp (car y))
		(fpquotient (fppi) (intofp -2)))
	       (t (fpquotient (fppi) (intofp 2)))))
	((signp g (car x))
	 (cond ((signp g (car y)) (fpatan (fpquotient y x)))
	       (t (fpminus (fpatan (fpquotient y x))))))
	((signp g (car y))
	 (fpplus (fppi) (fpatan (fpquotient y  x))))
	(t (fpdifference (fpatan (fpquotient y x)) (fppi))))) 

(defun tanbigfloat (a)
  (setq a (car a)) 
  (fpend (let ((fpprec (plus 8. fpprec)))
	   (cond (($bfloatp a)
		  (setq a (cdr ($bfloat a)))
		  (fpquotient (fpsin a t) (fpsin a nil)))
		 (t (list '(%tan) a))))))	 

;; Returns a list of a mantissa and an exponent.
(defun intofp (l) 
  (cond ((not (atom l)) ($bfloat l))
	((floatp l) (floattofp l))
	((equal 0 l) '(0 0))
	((eq l '$%pi) (fppi))
	((eq l '$%e) (fpe))
	((eq l '$%gamma) (fpgamma))
	(t (list (fpround l) (plus *m fpprec))))) 

;; It seems to me that this function gets called on an integer
;; and returns the mantissa portion of the mantissa/exponent pair.

;; "STICKY BIT" CALCULATION FIXED 10/14/75 --RJF
;; BASE must not get temporarily bound to NIL by being placed
;; in a PROG list as this will confuse stepping programs.

(defun fpround (l &aux #-cl (*print-base* 10.) #+cl (*print-base* 10.)
		#-cl (*nopoint t)#+cl *print-radix*
		)
  (prog () 
     (cond
       ((null *decfp)
	;;*M will be positive if the precision of the argument is greater than
	;;the current precision being used.
	(setq *m (f- (haulong l) fpprec))
	(cond ((= *m 0) (setq *cancelled 0) (return l)))
	;;FPSHIFT is essentially LSH.
	(setq adjust (fpshift 1 (sub1 *m)))
	(cond ((minusp l) (setq adjust (minus adjust))))
	(setq l (plus l adjust))
	(setq *m (f- (haulong l) fpprec))
	(setq *cancelled (abs *m))
	     
	(cond (#+cl
	       (zerop (hipart l (minus *m)))
	       #-cl
	       (signp e (hipart l (minus *m)))
					;ONLY ZEROES SHIFTED OFF
	       (return (fpshift (fpshift l (difference -1 *m))
				1)))	; ROUND TO MAKE EVEN
	      (t (return (fpshift l (minus *m))))))
       (t
	(setq *m (difference (flatsize (abs l)) fpprec))
	(setq adjust (fpshift 1 (sub1 *m)))
	(cond ((minusp l) (setq adjust (minus adjust))))
	(setq adjust (times 5 adjust))
	(setq *m
	      (difference (flatsize (abs (setq l (plus l adjust))))
			  fpprec))
	(return (fpshift l (minus *m)))))))

;; Compute (* L (expt d n)) where D is 2 or 10 depending on
;; *decfp. Throw away an fractional part by truncating to zero.
(defun fpshift (l n) 
  (cond ((null *decfp)
	 (cond ((and (minusp n) (minusp l))
		;; Left shift of negative number requires some
		;; care. (That is, (truncate l (expt 2 n)), but use
		;; shifts instead.)
		(- (ash (- l) n)))
	       (t
		(ash l n))))
	((greaterp n 0.)
	 (times l (expt 10. n)))
	((lessp n 0.)
	 (quotient l (expt 10. (minus n))))
	(t l)))

;; Bignum LSH -- N is assumed (and declared above) to be a fixnum.
;; This isn't really LSH, since the sign bit isn't propagated when
;; shifting to the right, i.e. (BIGLSH -100 -3) = -40, whereas
;; (LSH -100 -3) = 777777777770 (on a 36 bit machine).
;; This actually computes (TIMES X (EXPT 2 N)).  As of 12/21/80, this function
;; was only called by FPSHIFT.  I would like to hear an argument as why this
;; is more efficient than simply writing (TIMES X (EXPT 2 N)).  Is the
;; intermediate result created by (EXPT 2 N) the problem?  I assume that
;; EXPT tries to LSH when possible.

(defun biglsh (x n)
  (cond
    ;; In MacLisp, the result is undefined if the magnitude of the
    ;; second argument is greater than 36.
    ((and (not (bigp x))
	  (< n #.(f- +machine-fixnum-precision+))) 0)
    ;; Either we are shifting a fixnum to the right, or shifting
    ;; a fixnum to the left, but not far enough left for it to become
    ;; a bignum.
    ((and (not (bigp x)) 
	  (or (<= n 0)
	      (< (plus (haulong x) n) #.+machine-fixnum-precision+)))
     ;; The form which follows is nearly identical to (ASH X N), however
     ;; (ASH -100 -20) = -1, whereas (BIGLSH -100 -20) = 0.
     (if (>= x 0)
	 (lsh x n)
	 (f- (biglsh (minus x) n)))) ;(minus x) may be a bignum even is x is a fixnum.
    ;; If we get here, then either X is a bignum or our answer is
    ;; going to be a bignum.
    ((< n 0)
     (cond ((> (abs n) (haulong x)) 0)
	   ((greaterp x 0)
	    (hipart x (plus (haulong x) n)))
	   (t (minus (hipart x (plus (haulong x) n))))))
    ((= n 0) x)
    ;; Isn't this the kind of optimization that compilers are
    ;; supposed to make?
    ((< n #.(f1- +machine-fixnum-precision+)) (times x (lsh 1 n)))
    (t (times x (expt 2 n)))))


;; exp(x)
;;
;; For negative x, use exp(-x) = 1/exp(x)
;;
;; For x > 0, exp(x) = exp(r+y) = exp(r) * exp(y), where x = r + y and
;; r = floor(x).
(defun fpexp (x)       
  (prog (r s)
     (if (not (signp ge (car x)))
	 (return (fpquotient (fpone) (fpexp (fpabs x)))))
     (setq r (fpintpart x))
     (return (cond ((lessp r 2) (fpexp1 x))
		   (t (setq s (fpexp1 (fpdifference x (intofp r))))
		      (fptimes* s
				(cdr (bigfloatp
				      ((lambda (fpprec r) (bcons (fpexpt (fpe) r))) ; patch for full precision %E
				       (plus fpprec (haulong r) -1)
				       r)))))))))

;; exp(x) for small x, using Taylor series.
(defun fpexp1 (x) 
  (prog (term ans oans) 
     (setq ans (setq term (fpone)))
     (do ((n
	   1.
	   (add1 n)))
	 ((equal ans oans))
       (setq term (fpquotient (fptimes* x term) (intofp n)))
       (setq oans ans)
       (setq ans (fpplus ans term)))
     (return ans))) 

;; Does one higher precision to round correctly.
;; A and B are each a list of a mantissa and an exponent.
(defun fpquotient (a b) 
  (cond ((equal (car b) 0)
	 (merror "`pquotient' by zero"))
	((equal (car a) 0) '(0 0))
	(t (list (fpround (quotient (fpshift (car a)
					     (plus 3 fpprec))
				    (car b)))
		 (plus -3 (difference (cadr a) (cadr b)) *m))))) 

(defun fpgreaterp (a b) (fpposp (fpdifference a b))) 

(defun fplessp (a b) (fpposp (fpdifference b a))) 

(defun fpposp (x) (greaterp (car x) 0)) 

(defmfun fpmin na
  (prog (min) 
     (setq min (arg 1))
     (do ((i 2 (f1+ i))) ((> i na))
       (if (fplessp (arg i) min) (setq min (arg i))))
     (return min)))

;; (FPE) RETURN BIG FLOATING POINT %E.  IT RETURNS (CDR BIGFLOAT%E) IF RIGHT
;; PRECISION.  IT RETURNS TRUNCATED BIGFLOAT%E IF POSSIBLE, ELSE RECOMPUTES.
;; IN ANY CASE, BIGFLOAT%E IS SET TO LAST USED VALUE. 

(defun fpe nil
  (cond ((= fpprec (caddar bigfloat%e)) (cdr bigfloat%e))
	((< fpprec (caddar bigfloat%e))
	 (cdr (setq bigfloat%e (bigfloatp bigfloat%e))))
	((< fpprec (caddar max-bfloat-%e))
	 (cdr (setq bigfloat%e (bigfloatp max-bfloat-%e))))
	(t (cdr (setq max-bfloat-%e (setq bigfloat%e (*fpexp 1)))))))

(defun fppi nil
  (cond ((= fpprec (caddar bigfloat%pi)) (cdr bigfloat%pi))
	((< fpprec (caddar bigfloat%pi))
	 (cdr (setq bigfloat%pi (bigfloatp bigfloat%pi))))
	((< fpprec (caddar max-bfloat-%pi))
	 (cdr (setq bigfloat%pi (bigfloatp max-bfloat-%pi))))
	(t (cdr (setq max-bfloat-%pi (setq bigfloat%pi (fppi1)))))))

(defun fpgamma ()
  (cond ((= fpprec (caddar bigfloat%gamma))
	 (cdr bigfloat%gamma))
	((< fpprec (caddar bigfloat%gamma))
	 (cdr (setq bigfloat%gamma (bigfloatp bigfloat%gamma))))
	((< fpprec (caddar max-bfloat-%gamma))
	 (cdr (setq bigfloat%gamma (bigfloatp max-bfloat-%gamma))))
	(t
	 (cdr (setq max-bfloat-%gamma (setq bigfloat%gamma (fpgamma1)))))))

(defun fpone nil 
  (cond (*decfp (intofp 1)) ((= fpprec (caddar bigfloatone)) (cdr bigfloatone))
	(t (intofp 1)))) 

;; COMPPI computes PI to N bits.
;; That is, (COMPPI N)/(2.0^N) is an approximation to PI.

(defun comppi (n) 
  (prog (a b c) 
     (setq a (expt 2 n))
     (setq c (plus (times 3 a) (setq b (*quo a 8.))))
     (do ((i 4 (f+ i 2)))
	 ((zerop b))
       (setq b (*quo (times b (f1- i) (f1- i))
		     (times 4 i (f1+ i))))
       (setq c (plus c b)))
     (return c))) 

(defun fppi1 nil 
  (bcons (list (fpround (comppi (plus fpprec 3))) (plus -3 *m)))) 

;; Compute the main part of the Euler-Mascheroni constant using the
;; Bessel function approach.  See
;; http://numbers.computation.free.fr/Constants/Gamma/gamma.html for a
;; description of the algorithm.
;; Roughly, we have
;;
;; %gamma = A(N)/B(N) - log(N) + O(e^(-4*N))
;;
;; where
;;
;;
;;          a*N
;;   A(N) = sum (N^2/n!)^2*H(n)
;;          n=0
;;
;;          a*N
;;   B(N) = sum (N^2/n!)^2
;;          n=0
;;
;;           n
;;   H(n) = sum 1/k
;;          k=1
;;
;;   with H(0) = 0
;;
;; and a = 3.591121476668622136649223 where a*(log(a)-1) = 1.
;;
;; This formula can be easily justified by looking at the value
;; K0(2*N)/I0(2*N), where K0 and I0 are the modified Bessel functions.
;; From A&S 9.6.12 and 9.6.13, We see that
;;
;;           inf
;; I0(2*N) = sum (N^2/n!)^2
;;           n=0
;;
;;
;;                                        inf
;; K0(2*N) = -(log(N) + %gamma)*I0(2*N) + sum (N^2/n!)^2*H(n)
;;                                        n=0
;;
;; So
;;
;; K0(2*N)/I0(2*N) = -log(N) - %gamma + C
;;
;; where
;;
;; C = [sum (N^2/n!)^2*H(n)]/sum (N^2/n!)^2
;;
;; or
;;
;; For N large, A&S gives
;;
;; I0(2*N) = exp(2*N)/sqrt(4*%pi*N)
;;
;; K0(2*N) = sqrt(%pi/(4*N))*exp(-2*N)
;;
;; So K0(2*N)/I0(2*N) = %pi*exp(-4*N) and
;;
;; O(exp(-4*N)) = -log(N) - %gamma + C
;;
;; or
;;
;; %gamma = C - log(N) + O(exp(-4*N))
;;
;; And C is approximately A(N)/B(N) if we take enough terms in the
;; sum.
;;
(defun comp-bf%gamma (prec)
  ;; Prec is the number of digits we want.  We assume the remainder is
  ;; really e^(-4*N) and not O(e^(-4*N)).  So choose N such that
  ;; exp(-4*N) is less than the number of digits of precision we want.
  ;;
  ;; We also assume don't need a really precise value of beta because
  ;; our N's are not so big that we need more.
  (let* ((fpprec prec)
	 (big-n (floor (* 1/4 prec (log 2d0))))
	 (big-n-sq (intofp (* big-n big-n)))
	 (beta 3.591121476668622136649223d0)
	 (limit (floor (* beta big-n)))
	 (one (fpone))
	 (term (intofp 1))
	 (harmonic (intofp 0))
	 (a-sum (intofp 0))
	 (b-sum (intofp 1)))
    (do ((n 1 (1+ n)))
	((> n limit))
      (let ((bf-n (intofp n)))
	(setf term (fpquotient (fptimes* term big-n-sq)
			       (fptimes* bf-n bf-n)))
	(setf harmonic (fpplus harmonic
			       (fpquotient one bf-n)))
	(setf a-sum (fpplus a-sum
			    (fptimes* term harmonic)))
	(setf b-sum (fpplus b-sum term))))
    (fpplus (fpquotient a-sum b-sum)
	    (fpminus (fplog (intofp big-n))))))

(defun fpgamma1 ()
  ;; Use a few extra bits of precision
  (bcons (list (fpround (first (comp-bf%gamma (plus fpprec 8)))) 0)))

(defmfun fpmax na
  (prog (max) 
     (setq max (arg 1))
     (do ((i 2 (f1+ i))) ((> i na))
       (if (fpgreaterp (arg i) max) (setq max (arg i))))
     (return max)))

(defun fpdifference (a b) (fpplus a (fpminus b))) 

(defun fpminus (x) (if (equal (car x) 0) x (list (minus (car x)) (cadr x)))) 

(defun fpplus (a b) 
  (prog (*m exp man sticky) 
     (setq *cancelled 0)
     (cond ((equal (car a) 0) (return b))
	   ((equal (car b) 0) (return a)))
     (setq exp (difference (cadr a) (cadr b)))
     (setq man (cond ((equal exp 0)
		      (setq sticky 0)
		      (fpshift (plus (car a) (car b)) 2))
		     ((greaterp exp 0)
		      (setq sticky (hipart (car b) (difference 1 exp)))
		      (setq sticky (cond ((signp e sticky) 0)
					 ((signp l (car b)) -1)
					 (t 1)))
					; COMPUTE STICKY BIT
		      (plus (fpshift (car a) 2)
					; MAKE ROOM FOR GUARD DIGIT & STICKY BIT
			    (fpshift (car b) (difference 2 exp))))
		     (t (setq sticky (hipart (car a) (add1 exp)))
			(setq sticky (cond ((signp e sticky) 0)
					   ((signp l (car a)) -1)
					   (t 1)))
			(plus (fpshift (car b) 2)
			      (fpshift (car a) (plus 2 exp))))))
     (setq man (plus man sticky))
     (return (cond ((equal man 0) '(0 0))
		   (t (setq man (fpround man))
		      (setq exp
			    (plus -2 *m (max (cadr a) (cadr b))))
		      (list man exp)))))) 

(defun fptimes* (a b) 
  (cond ((or (equal (car a) 0) (equal (car b) 0)) '(0 0))
	(t (list (fpround (times (car a) (car b)))
		 (plus *m (cadr a) (cadr b) (minus fpprec)))))) 

;; Don't use the symbol BASE since it is SPECIAL.

(defun fpintexpt (int nn fixprec)	;INT is integer
  (setq fixprec (quotient fixprec (log2 int))) ;NN is pos
  (let ((bas (intofp (expt int (min nn fixprec))))) 
    (cond ((greaterp nn fixprec)
	   (fptimes* (intofp (expt int (remainder nn fixprec)))
		     (fpexpt bas (quotient nn fixprec))))
	  (t bas))))

;; NN is positive or negative integer

(defun fpexpt (p nn) 
  (cond ((equal nn 0.) (fpone))
	((equal nn 1.) p)
	((lessp nn 0.) (fpquotient (fpone) (fpexpt p (minus nn))))
	(t (prog (u) 
	      (cond ((oddp nn) (setq u p))
		    (t (setq u (fpone))))
	      (do ((ii (quotient nn 2.) (quotient ii 2.)))
		  ((zerop ii))
		(setq p (fptimes* p p))
		(cond ((oddp ii) (setq u (fptimes* u p)))))
	      (return u))))) 

;;(declare-top (NOTYPE N))

(defun exptbigfloat (p n) 
  (cond ((equal n 1) p)
	((equal n 0) ($bfloat 1))
	((not ($bfloatp p)) (list '(mexpt) p n))
	((equal (cadr p) 0) ($bfloat 0))
	((and (lessp (cadr p) 0) (ratnump n))
	 ($bfloat
	  ($expand (list '(mtimes)
			 ($bfloat ((lambda ($domain $m1pbranch) (power -1 n))
				   '$complex t))
			 (exptbigfloat (bcons (fpminus (cdr p))) n)))))
	((and (lessp (cadr p) 0) (not (integerp n)))
	 (cond ((or (equal n 0.5) (equal n bfhalf))
		(exptbigfloat p '((rat simp) 1 2)))
	       ((or (equal n -0.5) (equal n bfmhalf))
		(exptbigfloat p '((rat simp) -1 2)))
	       (($bfloatp (setq n ($bfloat n)))
		(cond ((equal n ($bfloat (fpentier n)))
		       (exptbigfloat p (fpentier n)))
		      (t ;; for P<0: P^N = (-P)^N*cos(pi*N) + i*(-P)^N*sin(pi*N)
		       (setq p (exptbigfloat (bcons (fpminus (cdr p))) n)
			     n ($bfloat `((mtimes) $%pi ,n)))
		       (add2 ($bfloat `((mtimes) ,p ,(*fpsin n nil)))
			     `((mtimes simp) ,($bfloat `((mtimes) ,p ,(*fpsin n t)))
			       $%i)))))
	       (t (list '(mexpt) p n))))
	((and (ratnump n) (lessp (caddr n) 10.))
	 (bcons (fpexpt (fproot p (caddr n)) (cadr n))))
	((not (integerp n))
	 (setq n ($bfloat n))
	 (cond
	   ((not ($bfloatp n)) (list '(mexpt) p n))
	   (t
	    ((lambda (extrabits) 
	       (setq 
		p
		((lambda (fpprec) 
		   (fpexp (fptimes* (cdr (bigfloatp n))
				    (fplog (cdr (bigfloatp p))))))
		 (plus extrabits fpprec)))
	       (setq p (list (fpround (car p))
			     (plus (minus extrabits) *m (cadr p))))
	       (bcons p))
	     (max 1 (plus (caddr n) (haulong (caddr p))))))))
					; The number of extra bits required 
	((lessp n 0) (invertbigfloat (exptbigfloat p (minus n))))
	(t (bcons (fpexpt (cdr p) n)))))

(defun fproot (a n)  ; computes a^(1/n)  see Fitch, SIGSAM Bull Nov 74
  (let* ((ofprec fpprec) (fpprec (f+ fpprec 2))	;assumes a>0 n>=2
	 (bk (fpexpt (intofp 2)
		     (add1 (quotient (cadr (setq a (cdr (bigfloatp a)))) n)))))
    (do ((x bk
	    (fpdifference
	     x (setq bk (fpquotient (fpdifference
				     x (fpquotient a (fpexpt x n1))) n))))
	 (n1 (sub1 n))
	 (n (intofp n)))
	((or (equal bk '(0 0))
	     (greaterp (difference (cadr x) (cadr bk)) ofprec)) (setq a x))))
  (list (fpround (car a)) (plus -2 *m (cadr a))))

(defun timesbigfloat (h) 
  (prog (fans tst r nfans) 
     (setq fans (setq tst (bcons (fpone))) nfans 1)
     (do ((l h (cdr l))) ((null l))
       (if (setq r (bigfloatp (car l)))
	   (setq fans (bcons (fptimes* (cdr r) (cdr fans))))
	   (setq nfans (list '(mtimes) (car l) nfans))))
     (return (cond ((equal nfans 1) fans)
		   ((equal fans tst) nfans)
		   (t (simplify (list '(mtimes) fans nfans))))))) 

(defun invertbigfloat (a) 
  (if (bigfloatp a) (bcons (fpquotient (fpone) (cdr a)))
      (simplify (list '(mexpt) a -1))))

(defun *fpexp (a) 
  (fpend (let ((fpprec (plus 8. fpprec)))
	   (if ($bfloatp (setq a ($bfloat a)))
	       (fpexp (cdr a))
	       (list '(mexpt) '$%e a)))))

(defun *fpsin (a fl) 
  (fpend (let ((fpprec (plus 8. fpprec)))
	   (cond (($bfloatp a) (fpsin (cdr ($bfloat a)) fl))
		 (fl (list '(%sin) a))
		 (t (list '(%cos) a))))))

(defun fpend (a)
  (cond ((equal (car a) 0) (bcons a))
	((numberp (car a))
	 (setq a (list (fpround (car a)) (plus -8. *m (cadr a))))
	 (bcons a))
	(t a))) 

(defun fparcsimp (e)  ; needed for e.g. ASIN(.123567812345678B0) with 
					; FPPREC 16, to get rid of the miniscule imaginary 
					; part of the a+bi answer.
  (if (and (mplusp e) (null (cdddr e))
	   (mtimesp (caddr e)) (null (cdddr (caddr e)))
	   ($bfloatp (cadr (caddr e)))
	   (eq (caddr (caddr e)) '$%i)
	   (< (caddr (cadr (caddr e))) (f+ (f- fpprec) 2)))
      (cadr e)
      e))

;;(declare-top (FIXNUM N))

(defun sinbigfloat (x) (*fpsin (car x) t)) 

(defun cosbigfloat (x) (*fpsin (car x) nil)) 

;; THIS VERSION OF FPSIN COMPUTES SIN OR COS TO PRECISION FPPREC,
;; BUT CHECKS FOR THE POSSIBILITY OF CATASTROPHIC CANCELLATION DURING
;; ARGUMENT REDUCTION (E.G. SIN(N*%PI+EPSILON)) 
;; *FPSINCHECK WILL CAUSE PRINTOUT OF ADDITIONAL INFO WHEN
;; EXTRA PRECISION IS NEEDED FOR SIN/COS CALCULATION.  KNOWN
;; BAD FEATURES:  IT IS NOT NECESSARY TO USE EXTRA PRECISION FOR, E.G.
;; SIN(PI/2), WHICH IS NOT NEAR ZERO, BUT  EXTRA
;; PRECISION IS USED SIN IT IS NEEDED FOR COS(PI/2).
;; PRECISION SEEMS TO BE 100% SATSIFACTORY FOR LARGE ARGUMENTS, E.G.
;; SIN(31415926.0B0), BUT LESS SO FOR SIN(3.1415926B0).  EXPLANATION
;; NOT KNOWN.  (9/12/75  RJF)

(declare-top (special *fpsincheck))

(setq *fpsincheck nil)

(defun fpsin (x fl) 
  (prog (piby2 r sign res k *cancelled) 
     (setq sign (cond (fl (signp g (car x))) (t)) x (fpabs x))
     (cond ((equal (car x) 0)
	    (return (cond (fl (intofp 0)) (t (intofp 1))))))
     (return 
       (cdr
	(bigfloatp
	 ((lambda (fpprec xt *cancelled oldprec) 
	    (prog (x) 
	     loop (setq x (cdr (bigfloatp xt)))
	     (setq piby2 (fpquotient (fppi) (intofp 2)))
	     (setq r (fpintpart (fpquotient x piby2)))
	     (setq x
		   (fpplus x
			   (fptimes* (intofp (minus r))
				     piby2)))
	     (setq k *cancelled)
	     (fpplus x (fpminus piby2))
	     (setq *cancelled (max k *cancelled))
	     (cond (*fpsincheck
		    (print `(*canc= ,*cancelled fpprec= ,fpprec
			     oldprec= ,oldprec))))

	     (cond
	       ((not (greaterp oldprec
			       (difference fpprec
					   *cancelled)))
		(setq r (remainder r 4))
		(setq res
		      (cond (fl (cond ((= r 0) (fpsin1 x))
				      ((= r 1) (fpcos1 x))
				      ((= r 2) (fpminus (fpsin1 x)))
				      ((= r 3) (fpminus (fpcos1 x)))))
			    (t (cond ((= r 0) (fpcos1 x))
				     ((= r 1) (fpminus (fpsin1 x)))
				     ((= r 2) (fpminus (fpcos1 x)))
				     ((= r 3) (fpsin1 x))))))
		(return (bcons (cond (sign res) (t (fpminus res))))))
	       (t (setq fpprec (plus fpprec *cancelled))
		  (go loop)))))
	  (max fpprec (plus fpprec (cadr x)))
	  (bcons x)
	  0
	  fpprec)))))) 

(defun fpcos1 (x) (fpsincos1 x nil))

;; Compute SIN or COS in (0,PI/2).  FL is T for SIN, NIL for COS.
;;
;; Use Taylor series
(defun fpsincos1 (x fl)
  (prog (ans term oans x2)
     (setq ans (if fl x (intofp 1))
	   x2 (fpminus(fptimes* x x)))
     (setq term ans)
     (do ((n (cond (fl 3) (t 2)) (plus n 2)))
	 ((equal ans oans))
       (setq term (fptimes* term (fpquotient x2 (intofp (f* n (sub1 n))))))
       (setq oans ans ans (fpplus ans term)))
     (return ans)))

(defun fpsin1(x) (fpsincos1 x t)) 

(defun fpabs (x) 
  (cond ((signp ge (car x)) x)
	(t (cons (minus (car x)) (cdr x))))) 

(defmfun fpentier (f) (let ((fpprec (caddar f))) (fpintpart (cdr f))))

(defun fpintpart (f) 
  (prog (m) 
     (setq m (difference fpprec (cadr f)))
     (return (cond ((greaterp m 0)
		    (quotient (car f) (expt 2 m)))
		   (t (times (car f) (expt 2 (minus m)))))))) 

(defun logbigfloat (a) 
  ((lambda (minus)
     (setq a ((lambda (fpprec) 
		(cond (($bfloatp (car a))
		       (setq a ($bfloat (car a)))
		       (cond ((zerop (cadr a)) (merror "log(0.0b0) has been generated"))
			     ((minusp (cadr a))
			      (setq minus t) (fplog (list (minus (cadr a)) (caddr a))))
			     (t (fplog (cdr a)))))
		      (t (list '(%log) (car a)))))
	      (plus 2 fpprec)))
     (cond ((numberp (car a))
	    (setq a
		  (if (zerop (car a))
		      (list 0 0)
		      (list (fpround (car a)) (plus -2 *m (cadr a)))))
	    (setq a (bcons a))))
     (cond (minus (add a (mul '$%i ($bfloat '$%pi)))) (t a)))
   nil)) 

;;; Computes the log of a bigfloat number.
;;;
;;; Uses the series
;;;
;;; log(1+x) = sum((x/(x+2))^(2*n+1)/(2*n+1),n,0,inf);
;;;
;;;
;;;                  INF      x   2 n + 1
;;;                  ====  (-----)
;;;                  \      x + 2
;;;          =  2     >    --------------
;;;                  /        2 n + 1
;;;                  ====
;;;                  n = 0
;;;
;;;
;;; which converges for x > 0.
;;;
;;; Note that FPLOG is given 1+X, not X.
;;;
;;; However, to aid convergence of the series, we scale 1+x until 1/e
;;; < 1+x <= e.
;;;
(defun fplog (x) 
  (prog (over two ans oldans term e sum) 
     (if (not (greaterp (car x) 0))
	 (merror "Non-positive argument to `fplog'"))
     (setq e (fpe)
	   over (fpquotient (fpone) e)
	   ans 0)
     ;; Scale X until 1/e < X <= E.  ANS keeps track of how
     ;; many factors of E were used.  Set X to NIL if X is E.
     (do () (nil)
       (cond ((equal x e) (setq x nil) (return nil))
	     ((and (fplessp x e) (fplessp over x))
	      (return nil))
	     ((fplessp x over)
	      (setq x (fptimes* x e))
	      (setq ans (sub1 ans)))
	     (t (setq ans (add1 ans))
		(setq x (fpquotient x e)))))
     (cond ((null x) (return (intofp (add1 ans)))))
     ;; Prepare X for the series.  The series is for 1 + x, so
     ;; get x from our X.  TERM is (x/(x+2)).  X becomes
     ;; (x/(x+2))^2.
     (setq x (fpdifference  x (fpone))
	   ans (intofp ans))
     (setq 
      x
      (fpexpt (setq term
		    (fpquotient x (fpplus x (setq two (intofp 2)))))
	      2))
     ;; Sum the series until the sum (in ANS) doesn't change
     ;; anymore.
     (setq sum (intofp 0))
     (do ((n 1 (+ n 2)))
	 ((equal sum oldans))
       (setq oldans sum)
       (setq sum
	     (fpplus
	      sum
	      (fpquotient term (intofp n))))
       (setq term (fptimes* term x)))
	     
     (return (fpplus ans (fptimes* two sum)))))

(defun mabsbigfloat (l) 
  (prog (r) 
     (setq r (bigfloatp (car l)))
     (return (cond ((null r) (list '(mabs) (car l)))
		   (t (bcons (fpabs (cdr r)))))))) 


;;;; Bigfloat implementations of special functions.
;;;;

;;; This is still a bit messy.  Some functions here take bigfloat
;;; numbers, represented by ((bigfloat) <mant> <exp>), but others want
;;; just the FP number, represented by (<mant> <exp>).  Likewise, some
;;; return a bigfloat, some return just the FP.
;;;
;;; This needs to be systemized somehow.  It isn't helped by the fact
;;; that some of the routines above also do the samething.
;;;
;;; The implementation for the special functions for a complex
;;; argument are mostly taken from W. Kahan, "Branch Cuts for Complex
;;; Elementary Functions or Much Ado About Nothing's Sign Bit", in
;;; Iserles and Powell (eds.) "The State of the Art in Numerical
;;; Analysis", pp 165-211, Clarendon Press, 1987

;; Compute exp(x) - 1, but do it carefully to preserve precision when
;; |x| is small.  X is a FP number, and a FP number is returned.  That
;; is, no bigfloat stuff.
(defun fpexpm1 (x)
  ;; What is the right breakpoint here?  Is 1 ok?  Perhaps 1/e is better?
  (cond ((fpgreaterp (fpabs x) (fpone))
	 ;; exp(x) - 1
	 (fpdifference (fpexp x) (fpone)))
	(t
	 ;; Use Taylor series for exp(x) - 1
	 (let ((ans x)
	       (oans nil)
	       (term x))
	   (do ((n 2 (1+ n)))
	       ((equal ans oans))
	     (setf term (fpquotient (fptimes* x term) (intofp n)))
	     (setf oans ans)
	     (setf ans (fpplus ans term)))
	   ans))))
  
;; log(1+x) for small x.  X is FP number, and a FP number is returned.
(defun fplog1p (x)
  ;; Use the same series as given above for fplog.  For small x we use
  ;; the series, otherwise fplog is accurate enough.
  (cond ((fpgreaterp (fpabs x) (fpone))
	 (fplog (fpplus x (fpone))))
	(t
	 (let* ((sum (intofp 0))
		(term (fpquotient x (fpplus x (intofp 2))))
		(f (fptimes* term term))
		(oldans nil))
	   (do ((n 1 (+ n 2)))
	       ((equal sum oldans))
	     (setq oldans sum)
	     (setq sum (fpplus sum
			       (fpquotient term (intofp n))))
	     (setq term (fptimes* term f)))
	   (fptimes* sum (intofp 2))))))

;; sinh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fpsinh (x)
  ;; X must be a maxima bigfloat
  
  ;; See, for example, Hart et al., Computer Approximations, 6.2.27:
  ;;
  ;; sinh(x) = 1/2*(D(x) + D(x)/(1+D(x)))
  ;;
  ;; where D(x) = exp(x) - 1.
  (let ((d (fpexpm1 (cdr (bigfloatp x)))))
    (bcons
     (fpquotient (fpplus d
			 (fpquotient d
				     (fpplus d (fpone))))
		 (intofp 2)))))

(defun big-float-sinh (x &optional y)
  ;; The rectform for sinh for complex args should be numerically
  ;; accurate, so return nil in that case.
  (unless y
    (fpsinh x)))

;; asinh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fpasinh (x)
  ;; asinh(x) = sign(x) * log(|x| + sqrt(1+x*x))
  ;;
  ;; And
  ;;
  ;; asinh(x) = x, if 1+x*x = 1
  ;;          = sign(x) * (log(2) + log(x)), large |x|
  ;;          = sign(x) * log(2*|x| + 1/(|x|+sqrt(1+x*x))), if |x| > 2
  ;;          = sign(x) * log1p(|x|+x^2/(1+sqrt(1+x*x))), otherwise.
  ;;
  ;; But I'm lazy right now and we only implement the last 2 cases.
  ;; We should implement all cases.
  (let* ((fp-x (cdr (bigfloatp x)))
	 (absx (fpabs fp-x))
	 (one (fpone))
	 (two (intofp 2))
	 (minus (minusp (car fp-x)))
	 result)
    ;; We only use two formulas here.  |x| <= 2 and |x| > 2.  Should
    ;; we add one for very big x and one for very small x, as given above.
    (cond ((fpgreaterp absx two)
	   ;; |x| > 2
	   ;;
	   ;; log(2*|x| + 1/(|x|+sqrt(1+x^2)))
	   (setf result
		 (fplog (fpplus (fptimes* absx two)
				(fpquotient one
					    (fpplus absx
						    (fproot (bcons (fpplus one
									   (fptimes* absx absx)))
							    2)))))))
	  (t
	   ;; |x| <= 2
	   ;;
	   ;; log1p(|x|+x^2/(1+sqrt(1+x^2)))
	   (let ((x*x (fptimes* absx absx)))
	     (setq result
		   (fplog1p (fpplus absx
				    (fpquotient x*x
						(fpplus one
							(fproot (bcons (fpplus one x*x))
								2)))))))))
    (if minus
	(bcons (fpminus result))
	(bcons result))))

(defun complex-asinh (x y)
  ;; asinh(z) = -%i * asin(%i*z)
  (multiple-value-bind (u v)
      (complex-asin (mul -1 y) x)
    (values v (bcons (fpminus (cdr u))))))

(defun big-float-asinh (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-asinh x y)
	(add u
	     (mul '$%i v)))
      (fpasinh x)))

(defun fpasin-core (x)
  ;; asin(x) = atan(x/(sqrt(1-x^2))
  ;;         = sgn(x)*[%pi/2 - atan(sqrt(1-x^2)/abs(x))]
  ;;
  ;; Use the first for  0 <= x < 1/2 and the latter for 1/2 < x <= 1.
  ;;
  ;; If |x| > 1, we need to do something else.
  ;;
  ;; asin(x) = -%i*log(sqrt(1-x^2)+%i*x)
  ;;         = -%i*log(%i*x + %i*sqrt(x^2-1))
  ;;         = -%i*[log(|x + sqrt(x^2-1)|) + %i*%pi/2]
  ;;         = %pi/2 - %i*log(|x+sqrt(x^2-1)|)
  
  (let ((fp-x (cdr (bigfloatp x))))
    (cond ((minusp (car fp-x))
	   ;; asin(-x) = -asin(x);
	   (mul -1 (fpasin (bcons (fpminus fp-x)))))
	  ((fplessp fp-x (cdr bfhalf))
	   ;; 0 <= x < 1/2
	   ;; asin(x) = atan(x/sqrt(1-x^2))
	   (bcons
	    (fpatan (fpquotient fp-x
				(fproot (bcons
					 (fptimes* (fpdifference (fpone) fp-x)
						   (fpplus (fpone) fp-x)))
					2)))))
	  ((fpgreaterp fp-x (fpone))
	   ;; x > 1
	   ;; asin(x) = %pi/2 - %i*log(|x+sqrt(x^2-1)|)
	   ;;
	   ;; Should we try to do something a little fancier with the
	   ;; argument to log and use log1p for better accuracy?
	   (let ((arg (fpplus fp-x
			      (fproot (bcons (fptimes* (fpdifference fp-x (fpone))
						       (fpplus fp-x (fpone))))
				      2))))
	     (add (div '$%pi 2)
		  (mul -1 '$%i
		       (bcons (fplog arg))))))
		       
	  (t
	   ;; 1/2 <= x <= 1
	   ;; asin(x) = %pi/2 - atan(sqrt(1-x^2)/x)
	   (let ((piby2 (fpquotient (fppi) (intofp 2))))
	     (add (div '$%pi 2)
		  (mul -1
		       (bcons
			(fpatan
			 (fpquotient (fproot
				      (bcons (fptimes* (fpdifference (fpone)
								     fp-x)
						       (fpplus (fpone) fp-x)))
				      2)
				     fp-x))))))))))

;; asin(x) for real x.  X is a bigfloat, and a maxima number (real or
;; complex) is returned.
(defun fpasin (x)
  ;; asin(x) = atan(x/(sqrt(1-x^2))
  ;;         = sgn(x)*[%pi/2 - atan(sqrt(1-x^2)/abs(x))]
  ;;
  ;; Use the first for  0 <= x < 1/2 and the latter for 1/2 < x <= 1.
  ;;
  ;; If |x| > 1, we need to do something else.
  ;;
  ;; asin(x) = -%i*log(sqrt(1-x^2)+%i*x)
  ;;         = -%i*log(%i*x + %i*sqrt(x^2-1))
  ;;         = -%i*[log(|x + sqrt(x^2-1)|) + %i*%pi/2]
  ;;         = %pi/2 - %i*log(|x+sqrt(x^2-1)|)
  
  ($bfloat (fpasin-core x)))

;; Square root of a complex number (xx, yy).  Both are bigfloats.  FP
;; (non-bigfloat) numbers are returned.
(defun complex-sqrt (xx yy)
  (let* ((x (cdr (bigfloatp xx)))
	 (y (cdr (bigfloatp yy)))
	 (rho (fpplus (fptimes* x x)
		      (fptimes* y y))))
    (setf rho (fpplus (fpabs x) (fproot (bcons rho) 2)))
    (setf rho (fpplus rho rho))
    (setf rho (fpquotient (fproot (bcons rho) 2) (intofp 2)))

    (let ((eta rho)
	  (nu y))
      (when (fpgreaterp rho (intofp 0))
	(setf nu (fpquotient (fpquotient nu rho) (intofp 2)))
	(when (fplessp x (intofp 0))
	  (setf eta (fpabs nu))
	  (setf nu (if (minusp (car y))
		       (fpminus rho)
		       rho))))
      (values eta nu))))

;; asin(z) for complex z = x + %i*y.  X and Y are bigfloats.  The real
;; and imaginary parts are returned as bigfloat numbers.
(defun complex-asin (x y)
  (let ((x (cdr (bigfloatp x)))
	(y (cdr (bigfloatp y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
	(complex-sqrt (bcons (fpdifference (intofp 1) x))
		      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
	  (complex-sqrt (bcons (fpplus (intofp 1) x))
			(bcons y))
	;; Realpart is atan(x/Re(sqrt(1-z)*sqrt(1+z)))
	;; Imagpart is asinh(Im(conj(sqrt(1-z))*sqrt(1+z)))
	(values (bcons
		 (fpatan (fpquotient x
				     (fpdifference (fptimes* re-sqrt-1-z
							     re-sqrt-1+z)
						   (fptimes* im-sqrt-1-z
							     im-sqrt-1+z)))))
		(fpasinh (bcons
			  (fpdifference (fptimes* re-sqrt-1-z
						  im-sqrt-1+z)
					(fptimes* im-sqrt-1-z
						  re-sqrt-1+z)))))))))
  
(defun big-float-asin (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-asin x y)
	(add u
	     (mul '$%i v)))
      (fpasin x)))
  
  
;; tanh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fptanh (x)
  ;; X is Maxima bigfloat
  ;; tanh(x) = D(2*x)/(2+D(2*x))
  (let* ((two (intofp 2))
	 (fp (cdr (bigfloatp x)))
	 (d (fpexpm1 (fptimes* fp two))))
    (bcons
     (fpquotient d (fpplus d two)))))

;; tanh(z), z = x + %i*y.  X, Y are bigfloats, and a maxima number is
;; returned.
(defun complex-tanh (x y)
  (let* ((fpx (cdr (bigfloatp x)))
	 (fpy (cdr (bigfloatp y)))
	 (tv (cdr (tanbigfloat (list y))))
	 (beta (fpplus (fpone) (fptimes* tv tv)))
	 (s (cdr (fpsinh x)))
	 (s^2 (fptimes* s s))
	 (rho (fproot (bcons (fpplus (fpone) s^2))
		      2))
	 (den (fpplus (fpone) (fptimes* beta s^2))))
    (values (bcons (fpquotient (fptimes* beta (fptimes* rho s))
			       den))
	    (bcons (fpquotient tv den)))))

(defun big-float-tanh (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-tanh x y)
	(add u (mul '$%i v)))
      (fptanh x)))

;; atanh(x) for real x, |x| <= 1.  X is a bigfloat, and a bigfloat is
;; returned.
(defun fpatanh (x)
  ;; atanh(x) = -atanh(-x)
  ;;          = 1/2*log1p(2*x/(1-x)), x >= 0.5
  ;;          = 1/2*log1p(2*x+2*x*x/(1-x)), x <= 0.5

  (let* ((fp-x (cdr (bigfloatp x))))
    (cond ((fplessp fp-x (intofp 0))
	   ;; atanh(x) = -atanh(-x)
	   (mul -1 (fpatanh (bcons (fpminus fp-x)))))
	  ((fpgreaterp fp-x (fpone))
	   ;; x > 1, so use complex version.
	   (multiple-value-bind (u v)
	       (complex-atanh x (bcons (intofp 0)))
	     (add u (mul '$%i v))))
	  ((fpgreaterp fp-x (cdr bfhalf))
	   ;; atanh(x) = 1/2*log1p(2*x/(1-x))
	   (bcons
	    (fptimes* bfhalf
		      (fplog1p (fpquotient (fptimes* (intofp 2) fp-x)
					   (fpdifference (fpone) fp-x))))))
	  (t
	   ;; atanh(x) = 1/2*log1p(2*x + 2*x*x/(1-x))
	   (let ((2x (fptimes* (intofp 2) fp-x)))
	     (bcons
	      (fptimes* (cdr bfhalf)
			(fplog1p (fpplus 2x
					 (fpquotient (fptimes* 2x fp-x)
						     (fpdifference (fpone) fp-x)))))))))))

(defun complex-atanh (x y)
  (let* ((fpx (cdr (bigfloatp x)))
	 (fpy (cdr (bigfloatp y)))
	 (beta (if (minusp (car fpx))
		   (fpminus (fpone))
		   (fpone)))
	 (rho (intofp 0))
	 (t1 (fpplus (fpabs fpy) rho))
	 (t1^2 (fptimes* t1 t1))
	 (1-x (fpdifference (fpone) fpx))
	 (eta (fpquotient
	       (fplog1p (fpquotient (fptimes* (intofp 4) fpx)
				    (fpplus (fptimes* 1-x 1-x)
					    t1^2)))
	       (intofp 4)))
	 (nu (fptimes* (cdr bfhalf)
		       (fpatan2
			(fptimes* (intofp 2) fpy)
			(fpdifference (fptimes* 1-x
						(fpplus (fpone) fpx))
				      t1^2)))))
    (values (bcons (fptimes* beta eta))
	    (bcons (fpminus (fptimes* beta nu))))))
    
  
(defun big-float-atanh (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-atanh x y)
	(add u (mul '$%i v)))
      (fpatanh x)))

;; acos(x) for real x.  X is a bigfloat, and a maxima number is returned.
(defun fpacos (x)
  ;; acos(x) = %pi/2 - asin(x)
  ($bfloat (add (div '$%pi 2)
		(mul -1 (fpasin-core x)))))

(defun complex-acos (x y)
  (let ((x (cdr (bigfloatp x)))
	(y (cdr (bigfloatp y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
	(complex-sqrt (bcons (fpdifference (intofp 1) x))
		      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
	  (complex-sqrt (bcons (fpplus (intofp 1) x))
			(bcons y))
	(values (bcons
		 (fptimes* (intofp 2)
			   (fpatan (fpquotient re-sqrt-1-z
					       re-sqrt-1+z))))
		(fpasinh (bcons
			  (fpdifference
			   (fptimes* re-sqrt-1+z im-sqrt-1-z)
			   (fptimes* im-sqrt-1+z re-sqrt-1-z)))))))))
				  

(defun big-float-acos (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-acos x y)
	(add u (mul '$%i v)))
      (fpacos x)))

(defun complex-log (x y)
  (let* ((x (cdr (bigfloatp x)))
	 (y (cdr (bigfloatp y)))
	 (t1 (let (($float2bf t))
	       ;; No warning message, please.
	       (floattofp 1.2d0)))
	 (t2 (intofp 3))
	 (rho (fpplus (fptimes* x x)
		      (fptimes* y y)))
	 (abs-x (fpabs x))
	 (abs-y (fpabs y))
	 (beta (fpmax abs-x abs-y))
	 (theta (fpmin abs-x abs-y)))
    (values (if (or (fpgreaterp t1 beta)
		    (fplessp rho t2))
		(fpquotient (fplog1p (fpplus (fptimes* (fpdifference beta (fpone))
						       (fpplus beta (fpone)))
					     (fptimes* theta theta)))
			    (intofp 2))
		(fpquotient (fplog rho) (intofp 2)))
	    (fpatan2 y x))))

(defun big-float-log (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-log x y)
	(add (bcons u) (mul '$%i (bcons v))))
      (let ((fp-x (cdr (bigfloatp x))))
	(if (fplessp fp-x (intofp 0))
	    ;; ??? Do we want to return an exact %i*%pi or a float
	    ;; approximation?
	    (add (bcons (fplog (fpminus fp-x)))
		 (mul '$%i (bcons (fppi))))
	    (bcons (fplog fp-x))))))

(defun big-float-sqrt (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-sqrt x y)
	(add (bcons u) (mul '$%i (bcons v))))
      (let ((fp-x (cdr (bigfloatp x))))
	(if (fplessp fp-x (intofp 0))
	    (mul '$%i (bcons (fproot (bcons (fpminus fp-x)) 2)))
	    (bcons (fproot x 2))))))

(eval-when
    #+gcl (load)
    #-gcl (:load-toplevel)
    (fpprec1 nil $fpprec))		; Set up user's precision

  
;; Undeclarations for the file:
;;(declare-top (NOTYPE I N EXTRADIGS))
