(in-package #:maxima)

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "nelder_mead" :type "system")
		       *load-pathname*))

(mk:oos "nelder_mead" :compile)
