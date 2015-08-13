;; This file should only be loaded on GCL and contains hacks to work around the
;; fact that GCL lisp is rather non-standard, and changes from one GCL version
;; to the next.

(in-package :maxima)

;; In GCL 2.6.10, get-setf-expansion is interned in CL, but isn't actually bound
;; to anything (?!). You're supposed to use get-setf-method instead. By 2.6.12,
;; get-setf-method has gone away and you have to use get-setf-expansion.
(unless (and (find-symbol "GET-SETF-EXPANSION" :cl)
             (fboundp 'cl:get-setf-expansion))

  (defmacro get-setf-expansion (form &optional environment)
    `(get-setf-method ,form ,environment)))
