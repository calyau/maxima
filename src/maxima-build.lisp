(load "../lisp-utils/defsystem.lisp")
(defun maxima-compile ()
  (mk:oos "maxima" :compile))
(defun maxima-load ()
  (mk:oos "maxima" :load))
(defun maxima-dump ()
  #+clisp(ext:saveinitmem "binary-clisp/maxima.mem" 
		   :init-function (function cl-user::run)))
