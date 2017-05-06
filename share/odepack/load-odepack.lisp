(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+nil
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "lapack"))
  (format t "*load-truename* = ~A~%" *load-truename*) (format t "sys =
  ~A~%" (merge-pathnames (make-pathname :name "lapack" :type "system")
					  *load-truename*)))

#+ecl ($load "lisp-utils/defsystem.lisp")

(let ((path (merge-pathnames (make-pathname :name "odepack" :type
"system")
			     #-gcl *load-pathname* +gcl
			     #sys:*load-pathname*)))
  (format t "path = ~S~%" path) (load path))

(mk:oos "maxima-odepack" :compile)
