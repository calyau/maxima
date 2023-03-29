;;;; Float properties
(in-package "MAXIMA")

;;------------------------------------------------------------------------
;; Previously from share/contrib/floatproperties.lisp
;;
;; Expose some properties of floating-point numbers to Maxima.
(defmvar $most_positive_float +most-positive-flonum+
  "Largest positive floating-point number"
  :properties ((assign 'neverset)))

(defmvar $most_negative_float +most-negative-flonum+
  "Most negative floating-point number"
  :properties ((assign 'neverset)))
  
;; largest_float and largest_negative_float are deprecated constants.
(defmvar $largest_float $most_positive_float
  "Deprecated.  Use most_positive_float"
  :deprecated-p "Use most_positive_float.")

(defmvar $largest_negative_float $most_negative_float
  "Deprecated.  Use most_negative_float"
  :deprecated-p "Use most_negative_float.")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmvar $least_positive_float +least-positive-flonum+
    "The smallest positive floating-point number"
    :properties ((assign 'neverset)))

  (defmvar $least_positive_normalized_float +least-positive-normalized-flonum+
    "The smallest positive normalized floating-point number"
    :properties ((assign 'neverset))))

(defmvar $least_negative_float +least-negative-flonum+
  "The least negative floating-point number"
  :properties ((assign 'neverset)))

(defmvar $least_negative_normalized_float +least-negative-normalized-flonum+
  "The least negative normalized floating-point number"
  :properties ((assign 'neverset)))

(defun $float_eps ()
  "Floating-point epsilon, basically the smallest value eps such that
  1+eps is not equal to 1"
  +flonum-epsilon+)

(defun $bigfloat_eps ()
  "The bigfloat version of float_eps; the smallest bigfloat such that
  1+eps is not equal to 1."
  (let ((r ($bfloat (div 1 (expt 2 fpprec)))))
    (list (first r) (incf (second r)) (third r))))

(defmfun ($float_bits :deprecated-p $float_precision) ()
  "The number of bits in the fraction part of a floating-point number."
  (float-digits 0d0))

(defmfun ($bigfloat_bits :deprecated-p $float_precision) ()
  "The number of bits in the fraction part of a bigfloat number.  Note
    that this changes when $fpprec is changed, of course."
  fpprec)

(defmfun $float_precision (f)
  "The number of bits of precision in the floating-point number F.  This
  includes floats and bigfloats."
  (cond ((floatp f)
	 (float-precision f))
	(($bfloatp f)
	 (bigfloat-prec f))
	(t
	 (merror (intl:gettext "~M: expected a float or bigfloat number: ~M")
		 '$float_precision f))))
	 
  
;;; ULP is the Unit in the Last Place

;;; ---Definition---
;;; unit_in_last_place (ulp) is the gap between x and the nearest other number
;;; -- https://people.eecs.berkeley.edu/~wkahan/LOG10HAF.TXT
;;;
;;; For floating-point exact powers of two, the ulp is the gap to the next
;;; number down in magnitude, which is smaller than the gap to the next number up:
;;;    ulp(0.9999) = ulp(1.0) = ulp(1.0001)/2
;;; The ULP of all denormalized numbers is the same:
;;;    ulp(least_positive_float) = ulp(least_positive_float*2^53) = least_positive_float
;;;
;;; We assume that there is only one least-positive-flonum, so these functions
;;; will not work for denormalized single-precision floats for example.
(defconstant +most-negative-normalized-float-exponent+
  (let ((expo (nth-value 1 (decode-float $least_positive_normalized_float))))
    ;; Some lisps may not have support for denormals, like Clisp.  In
    ;; that case set the exponent to be the exponent of the smallest
    ;; float, add the number of fraction bits, and subtract 1.
    (if (/= $least_positive_float $least_positive_normalized_float)
	expo
	(+ expo (float-digits 1d0) -1)))
  "The smallest exponent that decode-float can return for a normalized
  number.")

(defmfun $unit_in_last_place (f)
  (cond ((integerp f) 1)
	((ratnump f) 0)
	((floatp f)
	 (cond
	   ((= f 0.0)
	    $least_positive_float)
	   (t
	    (multiple-value-bind (significand expon)
		(decode-float f)
	      ;; If the exponent is smaller than the smallest
	      ;; normalized exponent, the ULP is the smallest float.
	      ;; Otherwise, the ULP has an exponent that is
	      ;; float-digits smaller, except when the fraction is a
	      ;; power of two, where we have to increase the exponent
	      ;; by 1.
	      (if (> expon +most-negative-normalized-float-exponent+)
		  (scale-float 0.5d0
			       (- expon
				  (float-digits f)
				  (if ($is_power_of_two significand)
				      0
				      -1)))
		  $least_positive_float)))))
	(($bfloatp f)
	 (let ((significand (cadr f))
	       (expon (- (caddr f) (bigfloat-prec f))))
	   (cond ((= 0 significand)
		   ; ULP is arbitrarily small for bigfloat 0
		  bigfloatzero)
		 ;; precision of resulting bigfloat not necessarily the same as input
		 ;; but that doesn't matter, since 2^n can be represented exactly in all
		 ;; precisions
		 (t
		  (exptbigfloat ($bfloat 2)
				(if ($is_power_of_two (abs significand))
				    (+ expon -1) expon))))))
	(t
	 (merror (intl:gettext "~:@M: unit_in_last_place is not defined")
		 f))))

;;; is_power_of_two works for explicit numbers: integers, floats, bfloats, rats
;;; NOTE: a negative number is not a power of 2
;;; does NOT handle expressions (by choice)
(defmfun $is_power_of_two (n)
  (cond ((integerp n)
	 (and (> n 0)
	      (= 0 (logand (abs n) (+ (abs n) -1)))))
	((floatp n)
	 (and (> n 0.0)
	      (= 0.5 (decode-float n))))
	(($bfloatp n)
	 ($is_power_of_two (cadr n)))
	((ratnump n)
	 ;; ratnums not needed for unit_in_last_place, but let's be complete
	 (and (= (cadr n) 1) ($is_power_of_two (caddr n))))
	(t
	 (merror (intl:gettext "~:@M: is_power_of_two is only defined for numbers")
		 n))))

;; $scale_float(f, n)
;;
;;   A Maxima interface to CL:SCALE-FLOAT.  Basically compuutes f *
;; 2^n, but this should be exact.
(defmfun $scale_float (f n)
  (unless (integerp n)
    (merror (intl:gettext "scale_float: second arg must be an integer: ~M~%")
	    n))
  (cond
    ((floatp f)
     (scale-float f n))
    (($bfloatp f)
     ;; Should probably diddle the bfloat parts directly, but it's
     ;; easier to use bigfloat:scale-float.
     (to (bigfloat:scale-float (bigfloat:bigfloat f) n)))
    (t
     (merror (intl:gettext  "scale_float: first arg must be a float or bfloat: ~M~%")
	     f))))

;; $decode_float(f)
;;
;;   A Maxima interface to CL:DECODE-FLOAT which returns a list of a
;; float, exponent, and a sign such that (* sign (scale-float float
;; exponent)) is exactly the same number.
;;
;;   We differ from CL:DECODE-FLOAT in that Except we return a mantissa
;; in the range [1, 2) instead of [0.5,1) because that's what IEEE
;; mantissas are.  The exponent is adjusted appropriately.
(defmfun $decode_float (f)
  (cond
    ((floatp f)
     (multiple-value-bind (mant expo sign)
	 (cl:decode-float f)
       ;; Ecl 21.2.1 has a broken decode-float for negative numbers.
       ;; It returns 0 for the sign instead of -1.  Just unconditional
       ;; set the sign via float-sign.  Ecl supports signed zeroes, so
       ;; float-sign is easiest way to get the correct sign.
       #+ecl
       (setf sign (float-sign f))
       (list '(mlist)
	     (* 2 mant)
	     (1- expo)
	     sign)))
    (($bfloatp f)
     (multiple-value-bind (mant expo sign)
	 (bigfloat:decode-float (bigfloat:bigfloat f))
       (list '(mlist)
	     (to (bigfloat:* mant 2))
	     (1- expo)
	     (to sign))))
    (t
     (merror (intl:gettext "decode_float is only defined for floats and bfloats: ~M")
	     f))))

;; $integer_decode_float(f)
;;
;;   A Maxima interfact to CL:INTEGER-DECODE-FLOAT which returns a
;; list of an integer, an exponent and a sign such that (* sign
;; (scale-float (float integer 1d0) exponent)) returns the original
;; number.
;;
;;   However, there's quite a bit of variety in Lisps when returning a
;; value for denormals (if supported).  We don't want to expose this
;; to Maxima, so we choose to decide how to handle that.  Thus, the
;; number of bits in the integer part is the same as (float-precision
;; f).  If not, we scale the integer appropriately and the exponent as
;; well.
(defmfun $integer_decode_float (f)
  (cond
    ((floatp f)
     (multiple-value-bind (int expo sign)
	 (integer-decode-float f)
       (let ((shift (- (integer-length int) (float-precision f))))
	 ;; If shift > 0, we have a denormal and want to reduce the
	 ;; number of bits in the integer part.
	 (if (plusp shift)
	     (list '(mlist)
		   (ash int (- shift))
		   (+ expo shift)
		   sign)
	     (list '(mlist)
		   int
		   expo
		   sign)))))
    (($bfloatp f)
     (multiple-value-bind (int expo sign)
	 (bigfloat:integer-decode-float (bigfloat:bigfloat f))
       (list '(mlist)
	     int
	     expo
	     sign)))
    (t
     (merror (intl:gettext "decode_float is only defined for floats and bfloats: ~M")
	     f))))
