(push :main-files-loaded *features*)
(load "sysdef.lisp")
(load "make.lisp")


(defun compile-maxima ()
  (make::make :maxima :compile t))

(defun save-maxima ()
  (make::make :maxima )
  (lisp:gc)
  (lisp:saveinitmem "maxima-clisp.mem"
                  :init-function #'user::run)
)

(in-package "MAXIMA")

(import '(system::getenv system::socket-connect) "MAXIMA")


(defun getpid ( &aux tem)

   (cond ((fboundp 'sys::program-id)
           (sys::program-id))
  ; ;under windows above does not work.
         ((consp (setq tem (errset (system::getenv "PID"))))
	  (read-from-string (car tem)))
	 (t (format t "using fake value for pid") -1))
  )


(or (fboundp 'commands1.orig)
  (setf (fdefinition 'commands1.orig) (fdefinition 'sys::commands1)))

;;add the quit maxima for the clisp debugger.
(defun sys::commands1 ()
    (append (list "
Quit to top    :q       Quit to MAXIMA top level"
                (cons ":q" #'(lambda () (throw 'macsyma-quit nil)))
          )
          (commands1.orig)
)
)

(defun $system (&rest x) (system::run-shell-command  (apply '$sconcat x)))



;; compile maxima
;(make::make :maxima :compile t)
;(load "init_max1.lisp")
;; load it
;(make::make :maxima )
;(savemem "maxima.clisp")
