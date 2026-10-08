(in-package :maxima)
;; DEF-SIMPLIFIER simplifiers call DOLLARIFY on their constant lambda
;; list on every call, only to name the function in an arity error.
;; Prototype: memoize on the (constant, EQ) list.
(defvar *orig-dollarify* (fdefinition 'dollarify))
(defvar *dollarify-memo* (make-hash-table :test 'eq))
(defun dollarify (l)
  (if (eq l *features*)
      (funcall *orig-dollarify* l)
      (or (gethash l *dollarify-memo*)
          (setf (gethash l *dollarify-memo*) (funcall *orig-dollarify* l)))))
