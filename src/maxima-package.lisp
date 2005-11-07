(in-package :common-lisp-user)

(defpackage :maxima-nregex
  (:use :common-lisp)
  (:export
   ;; vars
   #:*regex-debug* #:*regex-groups* #:*regex-groupings*
   ;; functions
   #:regex-compile
   ))

(defpackage :cl-info
  (:use :common-lisp :maxima-nregex)
  (:export #:info #:*info-paths*))

(defpackage :command-line
  (:use :common-lisp)
  (:nicknames :cmdline)
  (:export #:cl-option #:make-cl-option #:list-cl-options #:process-args
	   #:get-application-args))

;; GCL has SLOOP built in but it's slightly different now...
(defpackage :cl-sloop
  (:use :common-lisp)
  (:export #:sloop))

(defpackage :maxima
  (:use :common-lisp :command-line)
  (:nicknames :cl-macsyma :cl-maxima :macsyma)
  (:import-from :cl-sloop #:sloop)
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
  (:import-from :system
		;; Faster modular arithmetic.
		;; Unfortunately, as S. Macrakis observed (bug #706562),
		;; SI::CMOD currently behaves in a strange way:
		;; (let ((si::modulus 4)) (si::cmod 2)) => 2
		;; (let ((si::modulus 4)) (si::cmod -2)) => -2
		#:modulus #:cmod #:ctimes #:cdifference #:cplus

		#:getpid #:get-instream
		#:short-name #:cleanup #:instream-stream-name #:instream-line
		#:instream-name #:instream-stream #:stream-name #:complete-prop
		#:*stream-alist* #:break-call))


(provide :maxima)
