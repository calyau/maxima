(in-package "MAXIMA")
#-(and gcl ansi-cl)
(use-package "SERROR")

(defvar errset nil)

#-(and gcl ansi-cl)
(defmacro errset (form &optional flag) flag
  `(cond-any-error (er)
	       (list ,form)
	       ((null errset) nil)))

#+(and gcl ansi-cl)
(defmacro errset (&rest l)
   `(cl:handler-case (list ,(car l))
     (cl:error (e) (if errset (error e)))))

