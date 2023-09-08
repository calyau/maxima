;; Load all Lisp files within WITH-COMPILATION-UNIT macro.
;; This quiets the undefined function warnings from SBCL
;; which are otherwise very voluminous (and this construct
;; is accepted by other CL implementations).

#+(or ecl abcl)
($load "lisp-utils/defsystem.lisp")

(mk:defsystem linearalgebra
  :source-pathname (maxima::maxima-load-pathname-directory)
  :binary-pathname (maxima::maxima-objdir "share" "linearalgebra")
  :source-extension "lisp"
  :components
  ((:file "mring")
   (:file "lu")
   (:file "linalgcholesky")
   (:file "eigens-by-jacobi")
   (:file "linalg-extra")
   (:file "matrixexp")
   (:file "linalg-utilities")))

(with-compilation-unit ()
  (mk:oos "linearalgebra" :compile))
