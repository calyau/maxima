
;;collect the possible undefineds and show them at the end.
(defun compiler::PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED ()
  (SETQ FUNCTIONS-REFERENCED
	(DELETE-IF #'(LAMBDA (X) (COMPILATION-DEFINEDP (CAR X)))
		   (THE LIST FUNCTIONS-REFERENCED)) )
  (loop for (na . refs) in functions-referenced
	(cond ((not (compilation-definedp na))
	       (pushnew na *all-undefined*)))))

(defun print-undefined ()
  (dolist (v *all-undefined*)
    (cond ((not (or (fboundp v)
		    (get v (intern 'mfexpr* 'maxima)))
		(print v))))))
