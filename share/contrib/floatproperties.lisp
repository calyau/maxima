;; Expose some properties of double floating point numbers to Maxima.
;; Note: floatbits is one plus the number of bits in the fractional part.

(defmvar $double_float_eps double-float-epsilon)
(setf (get '$double_float_eps 'assign) 'neverset)

(defmvar $largest_float most-positive-double-float)
(setf (get '$largest_float 'assign) 'neverset)

(defmvar $least_positive_float least-positive-double-float)
(setf (get '$least_positive_float 'assign) 'neverset)

(defmvar $float_bits (float-digits 0.0))
(setf (get '$float_bits 'assign) 'neverset)
