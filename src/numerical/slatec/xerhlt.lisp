;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


;; If we get a SLATEC error, print out the message and signal a Maxima
;; error.  Should we also clear out the error message summary that
;; SLATEC seems to keep?
(defun xerhlt (messg)
  (maxima::merror messg))
