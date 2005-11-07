(in-package :maxima)

(setf (symbol-function 'ferror) (symbol-function 'merror))
(setf (symbol-function 'zl-listp) (symbol-function 'listp))


;; (format t "~%The functions ~/maxima::tilde-q-fsh/ are inverses"
;;	(st-rat #$[x+1,1/(x+1)]$))
;; should print: The functions x+1 and 1/(x+1) are inverses

;; See the last part of polyb for the definition of FSH.

(defun tilde-q-fsh (stream arg colonp at-p) ;atp is special...
  (declare (ignore colonp at-p))
  (fsh arg stream))

(defun string-search (string1 string2)
  (search string1 string2 :test #'char-equal))

(shadowing-import 'cl-sloop::loop-return)

;;; compat.lisp ends here
