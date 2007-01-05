(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+nil
(progn
  (format t "path = ~A~%" (combine-path (list *maxima-sharedir* "lapack")))
  (format t "*load-truename* = ~A~%" *load-truename*)
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "lapack" :type "system")
					  *load-truename*)))


(load (merge-pathnames (make-pathname :name "lapack" :type "system")
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))
(mk:oos "lapack-interface" :compile)
