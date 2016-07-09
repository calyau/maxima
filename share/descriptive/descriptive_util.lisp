;; descriptive.lisp -- additional functions needed by package descriptive

(defun $unique_in_sorted_array (a)
  (let ((prev (gensym)))
    (cons '(mlist) (loop for x across a when (not (alike1 prev (setq prev x))) collect x))))

(defun $vector_min_max (a)
  (loop for x across a maximize x into max minimize x into min finally (return (list '(mlist) min max))))

