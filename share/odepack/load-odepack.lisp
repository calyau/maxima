(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+nil
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "lapack"))
  (format t "*load-truename* = ~A~%" *load-truename*) (format t "sys =
  ~A~%" (merge-pathnames (make-pathname :name "lapack" :type "system")
					  *load-truename*)))

#+(or ecl abcl) ($load "lisp-utils/defsystem.lisp")

(let ((path (merge-pathnames (make-pathname :name "odepack" :type
"system")
			     #-gcl *load-pathname* +gcl
			     #sys:*load-pathname*)))
  (format t "path = ~S~%" path) (load path))

;; Maxima errored out when any lapack function was used which
;; most certainly was an ECL bug: Seems like the definition of the
;; MAXIMA package shadows the array symbol from the COMMON-LISP package.
;; Bugfix by Marius Gerbershagen:
#+ecl (in-package #:common-lisp)
(mk:oos "maxima-odepack" :compile)
