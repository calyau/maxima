(in-package "MAXIMA")

#+clisp
(setq custom:*parse-namestring-ansi* t)

(setf (logical-pathname-translations "affine")
      `(("**;*.*.*" 
	 ,(merge-pathnames 
	   (make-pathname :directory '(:relative :wild-inferiors))
	   (make-pathname :directory (pathname-directory *load-pathname*))))))

(load "affine:affine.system")
(mk:load-system "affine")

;;; affine.lisp ends here
