(in-package "MAXIMA")
(use-package "SERROR")

(defvar errset nil)

(defmacro errset (form &optional flag) flag
  `(cond-any-error (er)
	       (list ,form)
	       ((null errset) nil)))


