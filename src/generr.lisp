(in-package "MAXIMA")

(defvar errset nil)

#+(or lispm kcl)
(error "the errset special form is defined elsewhere for these machines")

#+excl ;for franz common lisp
(defmacro errset (&rest l)
  `(multiple-value-bind
    (noerr val)
    (excl::errorset ,@ l)
    (cond (noerr (list val))
	  (errset (error "error inside errset"))
	  (t nil))))

#+lucid
(defmacro errset (&rest l)
  `(multiple-value-bind
     (val err)
     (lucid::with-error-trapping ,(car l))
     (cond ((null err) val)
	   ((null errset) nil)
	   (t (eval val)))))
    

;;here is the  desired behavior of errset
;(let ((errset t)) (errset (+ 2 'a))) ;==> signals error
;(let ((errset nil)) (errset (+ 2 'a))) ;==> nil
;(let ((errset nil)) (errset (+ 2 3))) ;==> (5)

;;a generic one if you have no error handling 
;;at all, that caught no errors but at least
;;returned a list in the normal case would be 

#+clisp
(defmacro errset (&rest l)
   `(handler-case (list ,(car l))
     (error (e) (when errset (error e)))))

#-(or excl clisp lucid)
(defmacro errset (&rest l) `(list ,(car l)))


