(in-package #-gcl #:maxima #+gcl "MAXIMA")

#+ecl ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "graphs" :type "system") (maxima-load-pathname-directory)))

(mk:oos "graphs" :compile)
