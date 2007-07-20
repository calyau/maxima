(in-package :maxima)

(defvar *maxima-affinedir*
  (combine-path (list *maxima-sharedir* "affine")))
(load (combine-path
       (list *maxima-affinedir* "affine.system")))
(mk:compile-system "affine")

;;; affine.lisp ends here
