(defun implicit_plot-fix-up-non-equations (expr)
  (let ((f #'(lambda (e) (if (and (consp e) (eq (caar e) 'mequal)) e (list '(mequal) e 0)))))
    (if ($listp expr)
      (cons '(mlist) (mapcar f (rest expr)))
      (funcall f expr))))

;; implicit_plot now punts to plot2d
(defmfun $implicit_plot (expr &rest optional-args)
  (let*
    ((expr-eqs (implicit_plot-fix-up-non-equations expr))
     (all-args (cons expr-eqs optional-args)))
    (mtell (intl:gettext "implicit_plot is now obsolete. Calling plot2d instead:~%"))
    (mtell "~M~%" (cons '($plot2d) all-args))
    (apply #'$plot2d all-args)))
