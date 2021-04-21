;; implicit_plot now punts to plot2d
(defmfun $implicit_plot (expr &rest optional-args)
  (let ((command "plot2d ("))
    (unless (and (listp expr) (eq 'mequal (caar expr)))
      (setq expr `((mequal) ,expr 0))) 
    (setq command ($sconcat command expr))
    (when optional-args
      (dolist (arg optional-args)
        (setq command ($sconcat command ", " arg))))
    (setq command ($sconcat command ")"))
  (mtell (intl:gettext "implicit_plot is now obsolete. Using plot2d instead:~%"))
  (mtell "~M~%" command)
  (apply #'$plot2d (cons expr optional-args))))
