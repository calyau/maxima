(push :main-files-loaded *features*)
(load "sysdef.lisp")
(load "make.lisp")
(defun compile-maxima ()
  (make::make :maxima :compile t))

(defun save-maxima ()
  (make::make :maxima )
  (savemem "maxima.clisp"))

;; compile maxima
(make::make :maxima :compile t)
(load "init_max1.lisp")
;; load it
;(make::make :maxima )
;(savemem "maxima.clisp")
