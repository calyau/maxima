(in-package "MAXIMA")

(load (merge-pathnames #p"affine.system" 
		       (make-pathname
			:directory (pathname-directory *load-truename*))))
(mk:load-system "affine")

;;; affine.lisp ends here
