;;the definitions needed for runtime.
(in-package "MAXIMA")

(defvar $ratvarswitch t)
(DEFMVAR GENVAR NIL
	 "List of gensyms used to point to kernels from within polynomials.
	 The values cell and property lists of these symbols are used to
	 store various information.")
(DEFMVAR GENPAIRS NIL)
(DEFMVAR VARLIST NIL "List of kernels")
(DEFMVAR *FNEWVARSW NIL)
(DEFMVAR *RATWEIGHTS NIL)
(DEFVAR *RATSIMP* NIL)
(DEFMVAR FACTORRESIMP NIL "If T resimplifies FACTOR(X-Y) to X-Y")

;; User level global variables.

(DEFMVAR $KEEPFLOAT NIL
	 "If t floating point coeffs are not converted to rationals")
(DEFMVAR $FACTORFLAG NIL "If t constant factor of polynomial is also factored")
(DEFMVAR $DONTFACTOR '((MLIST)))
(DEFMVAR $NOREPEAT T)
(DEFMVAR $RATWEIGHTS '((MLIST SIMP)))

(DEFMVAR $RATFAC NIL "If t cre-forms are kept factored")
(DEFMVAR $ALGEBRAIC NIL)
(DEFMVAR $RATVARS '((MLIST SIMP)))
(DEFMVAR $FACEXPAND T)

(DEFMVAR $FLOAT2BF NIL
  "If TRUE, no MAXIMA-ERROR message is printed when a floating point number is
converted to a bigfloat number.")

(DEFMVAR $BFTORAT NIL
  "Controls the conversion of bigfloat numbers to rational numbers.  If
FALSE, RATEPSILON will be used to control the conversion (this results in
relatively small rational numbers).  If TRUE, the rational number generated
will accurately represent the bigfloat.")

(DEFMVAR $BFTRUNC T
  "Needs to be documented")

(DEFMVAR $FPPRINTPREC 0
  "Needs to be documented"
  FIXNUM)

(DEFMVAR $FPPREC 16.
  "Number of decimal digits of precision to use when creating new bigfloats.
One extra decimal digit in actual representation for rounding purposes.")

(DEFMVAR BIGFLOATZERO '((BIGFLOAT SIMP 56.) 0 0)
  "Bigfloat representation of 0" IN-CORE)
(DEFMVAR BIGFLOATONE  '((BIGFLOAT SIMP 56.) #.(EXPT 2 55.) 1)
  "Bigfloat representation of 1" IN-CORE)
(DEFMVAR BFHALF	      '((BIGFLOAT SIMP 56.) #.(EXPT 2 55.) 0)
  "Bigfloat representation of 1/2")
(DEFMVAR BFMHALF      '((BIGFLOAT SIMP 56.) #.(MINUS (EXPT 2 55.)) 0)
  "Bigfloat representation of -1/2")
(DEFMVAR BIGFLOAT%E   '((BIGFLOAT SIMP 56.) 48968212118944587. 2)
  "Bigfloat representation of %E")
(DEFMVAR BIGFLOAT%PI  '((BIGFLOAT SIMP 56.) 56593902016227522. 2)
  "Bigfloat representation of %PI")
;rat3c
(DEFMVAR *GCDL* '($SPMOD $SUBRES $EZ $RED $MOD $SPHEN $EEZ $ALGEBRAIC))

(DEFMVAR $GCD (CAR *GCDL*))		     ;Sparse Modular

;rat3b
(DEFMVAR $RATWTLVL NIL) 
(DEFMVAR $RATALGDENOM T)  ;If T then denominator is rationalized.
