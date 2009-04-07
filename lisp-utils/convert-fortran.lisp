;; Simple script to convert all of the Fortran sources to lisp.
;;
;; This should be run from the root of the maxima source tree.  We go
;; one by one and load the appropriate defsystem file and run mk:oos
;; to convert the Fortran sources to Lisp.  These are not done in any
;; particular order.
;;
;; Note that the current conversion was done using CMUCL.  It's
;; probably best to continue to use CMUCL to do the conversion.  Why?
;; So that the diffs between conversions are minimized.  Other than
;; this, any Lisp that can run f2cl can be used to do the conversion.
;;
;; This can be done from a running maxima-local:
;;
;; :lisp (load "lisp-utils/convert-fortran")

;; For this to work, you must have f2cl available.  If REQUIRE doesn't
;; work, you'll have to load f2cl yourself.
(require :f2cl)

(defun convert-system (system-name system-file &optional (clear-info t))
  (when clear-info
    ;; Clear the function info in case the various packages have
    ;; functions with the same name but different parameters or return
    ;; type.
    (f2cl::clear-f2cl-finfo))
  (load system-file)
  (mk:oos system-name :compile :force t))

(convert-system "slatec" "src/numerical/slatec/slatec.system")
(convert-system "lbfgs-lisp" "share/lbfgs/lbfgs-lisp.system")
(convert-system "minpack-lisp" "share/minpack/minpack-lisp.system")

;; BLAS and LAPACK should be compiled in this order.  And we probably
;; don't want to clear out the f2cl function info when compiling
;; LAPACK since LAPACK calls some BLAS routines.
(convert-system "blas-lisp" "share/lapack/blas/blas-lisp.system")
(convert-system "lapack-lisp" "share/lapack/lapack/lapack-lisp.system" nil)

(convert-system "colnew-lisp" "share/colnew/colnew-lisp.system")



