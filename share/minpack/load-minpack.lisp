(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "minpack" :type "system") (maxima-load-pathname-directory)))

(mk:oos "minpack-interface" :compile)
