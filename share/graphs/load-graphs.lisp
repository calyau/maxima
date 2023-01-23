(in-package #:maxima)

#+(or ecl abcl) ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "graphs" :type "system") (maxima-load-pathname-directory)))

(mk:oos "graphs" :compile)
