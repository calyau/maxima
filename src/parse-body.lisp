#+gcl
(eval-when (compile load)
  (defun parse-body (body &optional docstring-allowed env)
    (do ((bodyr body (cdr bodyr))
	 (declarations nil)
	 (docstring nil)
	 (form nil))
	((null bodyr) (values bodyr declarations docstring))
      (cond ((and (stringp (car bodyr)) (cdr bodyr) (null docstring) docstring-allowed)
	     (setq docstring (car bodyr))
	     )
	    ((not (listp (setq form (macroexpand (car bodyr) env))))
	     (return (values bodyr declarations docstring))
	     )
	    ((eq (car form) 'declare)
	     (dolist (decl (cdr form)) (push decl declarations))
	     )
	    (t (return (values bodyr declarations docstring)))
	    ) ) )
  ;; gcl (as of today, Jan 14, 2002) lacks destructuring-bind.
  ;; The following version of destructuring-bind was stolen from
  ;; clisp 2.27.
  #+gcl
  (defmacro destructuring-bind (lambdalist form &body body &environment env)
    (multiple-value-bind (body-rest declarations) (parse-body body nil env)
      (if declarations (setq declarations `((declare ,@declarations))))
      (let ((%arg-count 0) (%min-args 0) (%restp nil)
	    (%let-list nil) (%keyword-tests nil) (%default-form nil))
	(analyze1 lambdalist '<destructuring-form> 'destructuring-bind '<destructuring-form>)
	(let ((lengthtest (make-length-test '<destructuring-form> 0))
	      (mainform `(let* ,(nreverse %let-list)
			  ,@declarations
			  ,@(nreverse %keyword-tests)
			  ,@body-rest
			  ))          )
	  (if lengthtest
	      (setq mainform
		    `(if ,lengthtest
		      (destructuring-error <destructuring-form>
		       '(,%min-args . ,(if %restp nil %arg-count))
		       )
		      ,mainform
		      ) )  )
	  `(let ((<destructuring-form> ,form)) ,mainform)
	  ) ) ) )

  )