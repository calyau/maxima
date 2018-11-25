(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+(or)
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "odepack"))
  (format t "*load-truename* = ~A~%" *load-truename*)
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "odepack" :type "system")
					  *load-truename*)))

#+(or ecl abcl) ($load "lisp-utils/defsystem.lisp")

(let ((path (merge-pathnames (make-pathname :name "odepack" :type "system")
			     (maxima-load-pathname-directory))))
  #+(or)
  (format t "loading = ~S~%" path)
  (load path))

;; Fix for ecl thinking that ARRAY in declarations is not from the CL
;; package.  This seems like a bug in ecl, and this works around it.
;; Same fix as used for lapack.
#+ecl
(in-package #:common-lisp)

(mk:oos "maxima-dlsode" :compile)
