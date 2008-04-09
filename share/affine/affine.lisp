(in-package :maxima)

(defvar *maxima-affinedir*  (combine-path *maxima-sharedir* "affine"))

(load (combine-path *maxima-affinedir* "affine.system"))

(mk:compile-system "affine")

;;; affine.lisp ends here
