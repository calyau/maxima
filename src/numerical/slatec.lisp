
(defpackage "SLATEC"
  (:use "F2CL-LIB" #-gcl "COMMON-LISP" #+gcl "LISP")
  (:export
   ;; Error functions
   "DERF" "DERFC"

   ;; Bessel function: J
   "DBESJ0" "DBESJ1" "DBESJ" "ZBESJ"

   ;; Bessel function: Y
   "DBESY0" "DBESY1" "DBESY" "ZBESY"

   ;; Bessel function: I
   "DBESI0" "DBESI1" "DBESI" "DBSI0E" "DBSI1E" "ZBESI"

   ;; Bessel function: K
   "DBESK0" "DBESK1" "DBESK" "ZBESK"

   ;; Besseel function: H
   "ZBESH"
   
   ;; Airy functions
   "DAI"

   ;; Exponential integrals
   "DE1")
  (:documentation "Package for the Fortran routines we need from SLATEC"))
