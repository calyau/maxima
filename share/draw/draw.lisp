(in-package #-gcl #:maxima #+gcl "MAXIMA")

($put '$draw 2 '$version)

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "draw" :type "system")
		       #-gcl *load-pathname*
		       #+gcl sys:*load-pathname*))

(mk:oos "draw" :compile)
