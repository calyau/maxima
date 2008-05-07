;; Expose some properties of double floating point numbers to Maxima.
;; Note: floatbits is one plus the number of bits in the fractional part.

(defmvar $float_eps flonum-epsilon)
(setf (get '$float_eps 'assign) 'neverset)

(defmvar $largest_float most-positive-flonum)
(setf (get '$largest_float 'assign) 'neverset)

(defmvar $least_positive_float least-positive-flonum)
(setf (get '$least_positive_float 'assign) 'neverset)

(defmvar $float_bits (float-digits 0.0))
(setf (get '$float_bits 'assign) 'neverset)
