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
(in-package "MAXIMA")

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

