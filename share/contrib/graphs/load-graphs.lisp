(in-package #-gcl #:maxima #+gcl "MAXIMA")

#+ignore
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "graphs"))
  (format t "*load-truename* = ~A~%" *load-truename*)
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "graphs" :type "system")
					  *load-truename*)))

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "graphs" :type "system")
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))

(mk:oos "graphs" :compile)
