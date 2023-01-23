(in-package #:maxima)

($put '$draw 2 '$version)

#+(or ecl abcl) ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "draw" :type "system") (maxima-load-pathname-directory)))

(mk:oos "draw" :compile)
