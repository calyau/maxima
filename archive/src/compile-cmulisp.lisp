(push :main-files-loaded *features*)
(load "sysdef.lisp")
(load "make.lisp")

(defun compile-maxima ()
  (make::make :maxima :compile t))


(defun save-maxima ()
  (make::make :maxima)
  (ext:gc)
  (ext:save-lisp "maxima-cmulisp.mem" :init-function #'user::run
               :load-init-file nil :site-init nil))

(in-package "MAXIMA")


;; define bye so that quit() will work in maxima
(defun bye () (ext:quit))





