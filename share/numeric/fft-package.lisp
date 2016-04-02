;; The Maxima FFT package that holds the internal implementation
;; details for Maxima's FFT routines.
(defpackage maxima-fft
  (:use :cl :maxima)
  (:export "COMPLEX-TO-POLAR"
	   "FFT-ARG-CHECK"
	   "FFT-R2-NN"
	   "FIND-BF-FFT-CONVERTER"
	   "FIND-BF-IRFFT-CONVERTERS"
	   "FIND-BF-RFFT-CONVERTERS"
	   "FIND-COMPLEX-FFT-CONVERTERS"
	   "FIND-IRFFT-CONVERTERS"
	   "FIND-RFFT-CONVERTERS"
	   "LOG-BASE2"
	   "POLAR-TO-COMPLEX"
	   "SINCOS-TABLE")
  ;; Need these symbols from MAXIMA, mostly for convenience so we
  ;; don't have to prefix them everywhere.
  (:import-from "MAXIMA"
		"ADD"
		"MERROR"
		"MEVAL"
		"MGET"
		"MLIST"
		"MUL"
		"RISPLIT"
		"SIMP"
		"SYMBOL-ARRAY"
		"TO"
		"$%I"
		"$ARRAY"
		"$BFLOAT"
		"$FLOAT"
		"$LENGTH"
		"$LISTP"))
