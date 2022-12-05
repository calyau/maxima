(in-package :maxima)

(defvar errset nil)

;; Here is the desired behavior of errset:
;;
;; (let ((errset t)) (errset (+ 2 'a)))      ;==> signals error
;; (let ((errset nil)) (errset (+ 2 'a)))    ;==> nil
;; (let ((errset nil)) (errset (values)))    ;==> (nil)
;; (let ((errset nil)) (errset (+ 2 3)))     ;==> (5)
;; (let ((errset nil)) (errset (floor 4/3))) ;==> (1 1/3)
;;
;; Notice that there is one ambiguous case: a singleton list
;; containing only nil could mean either the form really did
;; return nil, or that the form returned no values at all.
;; The original "single value" errset had this ambiguous case,
;; and it was preserved explicitly in this "multiple value"
;; errset for backward-compatibility and to avoid the ambiguous
;; case where nil could be returned if an error occurred or if
;; the form returned no values.

(defmacro errset (&rest l)
  `(handler-case (or (multiple-value-list ,(car l)) (list nil))
    (error (e) (when errset (error e)))))

