;; descriptive.lisp -- additional functions needed by package descriptive

(defun $unique_in_sorted_array (a)
  (let ((prev (gensym)))
    (cons '(mlist) (loop for x across a when (not (alike1 prev (setq prev x))) collect x))))

(defun $vector_min_max (a)
  (if (> (length a) 0)
    (let ((min-a '$inf) (max-a '$minf) ($ratprint nil))
      (loop for x across a
            do (let ((cmp-min ($compare x min-a))
                     (cmp-max ($compare x max-a)))
                 (when (string= cmp-min "<") (setq min-a x))
                 (when (string= cmp-max ">") (setq max-a x))))
      (list '(mlist) min-a max-a))))
  ;; Otherwise return NIL if A is empty.
