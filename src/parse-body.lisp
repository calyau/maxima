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
	  ((eq (car form) 'DECLARE)
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
    (if declarations (setq declarations `((DECLARE ,@declarations))))
    (let ((%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form nil))
      (analyze1 lambdalist '<DESTRUCTURING-FORM> 'destructuring-bind '<DESTRUCTURING-FORM>)
      (let ((lengthtest (make-length-test '<DESTRUCTURING-FORM> 0))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (DESTRUCTURING-ERROR <DESTRUCTURING-FORM>
                                    '(,%min-args . ,(if %restp nil %arg-count))
               )
               ,mainform
        ) )  )
        `(LET ((<DESTRUCTURING-FORM> ,form)) ,mainform)
) ) ) )

)