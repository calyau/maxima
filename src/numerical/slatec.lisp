
(defpackage "SLATEC"
  (:use "F2CL-LIB" "COMMON-LISP")
  (:export
   ;; Error functions
   "DERF" "DERFC"
   ;; Bessel functions
   "DBESJ0" "DBESJ1" "DBESJ"
   "DBESI0" "DBESI1" "DBESI"
   "DBSI0E" "DBSI1E"
   ;; Airy functions
   "DAI"
   ;; Exponential integrals
   "DE1")
  (:documentation "Package for the Fortran routines we need from SLATEC"))
