(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+(or)
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "odepack"))
  (format t "*load-truename* = ~A~%" *load-truename*)
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "odepack" :type "system")
					  *load-truename*)))

#+ecl ($load "lisp-utils/defsystem.lisp")

(let ((path (merge-pathnames (make-pathname :name "odepack" :type
					    "system")
			     #-gcl *load-pathname*
			     #+gcl sys:*load-pathname*)))
  #+(or)
  (format t "loading = ~S~%" path)
  (load path))

(mk:oos "maxima-dlsode" :compile)
