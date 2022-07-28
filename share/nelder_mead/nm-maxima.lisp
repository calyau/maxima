(defun $nelder_mead (expr vars init)
  (let* ((fun (coerce-float-fun expr vars))
	 (fun1 (lambda (arr)
		 (mfuncall '$apply fun `((mlist simp) ,@(loop for i across arr collect i)))))
	 (init (make-array ($length init) :initial-contents (cdr ($float init)))))
    (multiple-value-bind
	  (xk fk fv) (neldermead:grnm-optimize fun1 init :verbose nil)
      (declare (ignore fk fv))
      `((mlist simp) ,@(mapcar #'(lambda (x y) `((mequal simp) ,x ,y))
			       (cdr vars)
			       (loop for i across xk collect i))))))

