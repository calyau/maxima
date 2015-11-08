#+nil
(format t "colnew.system = ~S~%" (merge-pathnames (make-pathname :name "colnew" :type "system") (maxima-load-pathname-directory)))
(load (merge-pathnames (make-pathname :name "colnew" :type "system") (maxima-load-pathname-directory)))

(mk:oos "colnew-if" :compile)
