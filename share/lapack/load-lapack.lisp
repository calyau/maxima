(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+nil
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "lapack"))
  (format t "(maxima-load-pathname-directory) = ~A~%" (maxima-load-pathname-directory))
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "lapack" :type "system") (maxima-load-pathname-directory))))

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "lapack" :type "system") (maxima-load-pathname-directory)))

(mk:oos "lapack-interface" :compile)
