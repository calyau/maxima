(in-package :maxima)

(defvar *maxima-affinedir*  (combine-path *maxima-sharedir* "affine"))

(load (combine-path *maxima-affinedir* "affine.system"))

(mk:compile-system "affine" :load-source-if-no-binary t)

;;; affine.lisp ends here
