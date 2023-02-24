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

(defconstant +most-negative-float-exponent+
  #-cmucl
  (nth-value 1 (integer-decode-float +least-positive-flonum+))
  #+cmucl
  -1126)

(defun $unit_in_last_place (f)
  (cond ((integerp f) 1)
	((ratnump f) 0)
	((floatp f)
	 (cond
	  ((= f 0.0) least-positive-long-float)
	  (t (multiple-value-bind
		 (significand expon sign)
		 (integer-decode-float f)
	       (expt 2.0
		     (if (and ($is_power_of_two significand) (> expon +most-negative-float-exponent+))
			 (+ expon -1)
		       expon))))))
	(($bfloatp f)
	 (let ((significand (cadr f))
	       (expon (- (caddr f) (bigfloat-prec f))))
	   (cond ((= 0 significand) bigfloatzero) ; ULP is arbitrarily small for bigfloat 0
		 ;; precision of resulting bigfloat not necessarily the same as input
		 ;; but that doesn't matter, since 2^n can be represented exactly in all
		 ;; precisions
		 (t (exptbigfloat ($bfloat 2)
				  (if ($is_power_of_two (abs significand)) (+ expon -1) expon))))))
	(t (merror (intl:gettext "~:@M: unit_in_last_place is not defined")
		   f))))

;;; is_power_of_two works for explicit numbers: integers, floats, bfloats, rats
;;; NOTE: a negative number is not a power of 2
;;; does NOT handle expressions (by choice)
(defun $is_power_of_two (n)
  (cond ((integerp n)
	 (and (> n 0)
	      (= 0 (logand (abs n) (+ (abs n) -1)))))
	((floatp n)
	 (and (> n 0.0)
	      ($is_power_of_two (integer-decode-float n))))
	(($bfloatp n) ($is_power_of_two (cadr n)))
	;; ratnums not needed for unit_in_last_place, but let's be complete
	((ratnump n) (and (= (cadr n) 1) ($is_power_of_two (caddr n))))
	(t (merror (intl:gettext "~:@M: is_power_of_two is only defined for numbers")
		   n))))

