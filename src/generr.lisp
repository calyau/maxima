(in-package :maxima)

(defvar errset nil)

;;here is the  desired behavior of errset
;;(let ((errset t)) (errset (+ 2 'a))) ;==> signals error
;;(let ((errset nil)) (errset (+ 2 'a))) ;==> nil
;;(let ((errset nil)) (errset (+ 2 3))) ;==> (5)

#-ecldebug
(defmacro errset (&rest l)
  `(handler-case (list ,(car l))
    (error (e) (when errset (error e)))))

#+ecldebug
(defmacro errset (&rest l)
  `(handler-case (list ,(car l))
    (error (e)
      (format *error-output* "~S~%~A~%" (type-of e) e)
      (when errset
	(let ((*debugger-hook* nil)) (si::default-debugger e))))))
