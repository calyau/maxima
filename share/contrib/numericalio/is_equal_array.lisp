(defun $is_equal_array (A B)
  (cond ((mget A 'array)
      (setq A (symbol-array (mget A 'array)))))

  (cond ((mget B 'array)
      (setq B (symbol-array (mget B 'array)))))

  (cond ((and (arrayp A) (arrayp B))
      (is-equal-lisp-array A B))
    (t
      (merror "is_equal_array: expected two arrays"))))


(defun is-equal-lisp-array (A B)
  (cond ((equal (array-dimensions A) (array-dimensions B))
      (equal (listarray A) (listarray B)))
    (t nil)))
