(in-package "MAXIMA")

(defvar *maxima-affinedir*
  (combine-path (list *maxima-sharedir* "affine")))
(load (combine-path
       (list *maxima-affinedir* "affine.system")))
(mk:load-system "affine")

;;; affine.lisp ends here
