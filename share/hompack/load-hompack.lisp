(in-package #-gcl #:maxima #+GCL "MAXIMA")

#+(or ecl abcl) ($load "lisp-utils/defsystem.lisp")

(load (merge-pathnames (make-pathname :name "hompack" :type "system") (maxima-load-pathname-directory)))

(let ((*read-default-float-format* 'single-float))
  (mk:oos "hompack-interface" :compile))
