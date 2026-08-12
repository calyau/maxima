(in-package :maxima)

(defun $nelder_mead (expr vars init &rest options)
  (let* ((fun (coerce-float-fun expr vars))
         (maxima-options (cons '(mlist) options))
         (verbose ($assoc '$nm_verbose maxima-options))
         (maxfuncalls ($assoc '$maxfuncalls maxima-options))
         (fun1 (lambda (arr)
                 (mfuncall '$apply fun `((mlist simp) ,@(loop for i across arr collect i)))))
         (init (make-array ($length init) :initial-contents (cdr ($float init)))))
    (multiple-value-bind
         (xk fk fv) (neldermead:grnm-optimize fun1 init :verbose verbose :max-function-calls maxfuncalls)
      (declare (ignore fk fv))
      `((mlist simp) ,@(mapcar #'(lambda (x y) `((mequal simp) ,x ,y))
                               (cdr vars)
                               (loop for i across xk collect i))))))

