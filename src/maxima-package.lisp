(in-package "COMMON-LISP-USER")

(defpackage "CL-INFO"
  (:use "COMMON-LISP")
  (:export "INFO" "*INFO-PATHS*"))

(defpackage "COMMAND-LINE"
  (:use "COMMON-LISP")
  (:nicknames "CMDLINE")
  (:export "CL-OPTION" "MAKE-CL-OPTION" "LIST-CL-OPTIONS" "PROCESS-ARGS"
	   "GET-APPLICATION-ARGS"))

;; GCL has SLOOP built in but it's slightly different now...
(defpackage "CL-SLOOP"
  (:use "COMMON-LISP")
  (:shadow "LOOP-FINISH")
  (:export "LOOP-RETURN" "SLOOP" "DEF-LOOP-COLLECT" "DEF-LOOP-MAP"
	   "DEF-LOOP-FOR" "DEF-LOOP-MACRO" "LOCAL-FINISH" "LOOP-FINISH"))

(defpackage "MAXIMA"
  (:use "COMMON-LISP" "COMMAND-LINE")
  (:nicknames "CL-MACSYMA" "CL-MAXIMA" "MACSYMA")
  (:shadowing-import-from "CL-SLOOP" "LOOP-FINISH")
  (:import-from "CL-SLOOP" "LOOP-RETURN" "LOCAL-FINISH" "SLOOP")
  (:shadow complement                   ;(maxmac)
	   continue		 ;(macsys): part of the top-level loop
	   //                           ;(clmacs): arithmetic operator
	   float		;(clmacs): has 1.0d0 as default format
	   functionp                    ;(commac): accepts symbols
	   array                        ;(commac)
	   exp			   ;various files declare this special
	   listen		     ;(suprv1): has trivial definition
					; (listen any) ==> 0
	   signum                       ;(commac): same except
					; (cl:signum 1.3)==1.0 not 1 but I (?)
					; think this is ok for macsyma
	   atan			;(zl:atan y x) == (cl:atan y x) + 2 pi
					; if latter is negative
	   asin acos asinh acosh atanh  ;different for complex numbers
	   tanh cosh sinh tan  ;(trigi): same, could remove from trigi
	   break		     ; special variable in displa.lisp
	   gcd				; special in rat module
	   ;; (getalias '$lambda) => cl:lambda, which implies that
	   ;; Maxima parses lambda as cl:lambda. 
	   #+(and sbcl sb-package-locks) makunbound)
  #+gcl
  (:import-from "SYSTEM"
		;; Faster modular arithmetic.
		;; Unfortunately, as S. Macrakis observed (bug #706562),
		;; SI::CMOD currently behaves in a strange way:
		;; (let ((si::modulus 4)) (si::cmod 2)) => 2
		;; (let ((si::modulus 4)) (si::cmod -2)) => -2
		"MODULUS" "CMOD" "CTIMES" "CDIFFERENCE" "CPLUS"

		"GETPID" "GET-INSTREAM"
		"SHORT-NAME" "CLEANUP" "INSTREAM-STREAM-NAME" "INSTREAM-LINE"
		"INSTREAM-NAME" "INSTREAM-STREAM" "STREAM-NAME" "COMPLETE-PROP"
		"*STREAM-ALIST*" "BREAK-CALL"))


(provide "MAXIMA")
