;; Expose some properties of double floating point numbers to Maxima.
;; Note: floatbits is one plus the number of bits in the fractional part.

(defmvar $doublefloateps double-float-epsilon)
(setf (get '$doublefloateps 'assign) 'neverset)

(defmvar $largestfloat most-positive-double-float)
(setf (get '$largestfloat 'assign) 'neverset)

(defmvar $leastfloat least-negative-double-float)
(setf (get '$leastfloat 'assign) 'neverset)

(defmvar $floatbits (float-digits 0.0))
(setf (get '$floatbits 'assign) 'neverset)
