;; very simple server started on port
;; (setup 

;(setq si::*notify-gbc* t)

(defun setup ( port )
  (setq me (si::socket port :host "localhost"))
  (si::set-blocking me t)
  (setq *standard-input* me)
  (setq *standard-output* me)
  (setq *error-output* me)
  (setq *terminal-io* me)
  (format t "pid=~a~%" (si::getpid))
  ;(setq *debug-io* me)
  

  )
(and (find-package "MAXIMA") (push :maxima *features*))
#+maxima
(in-package "MAXIMA")
#+maxima
(progn
(setq $in_netmath t)
(setq $show_openplot nil)



;(trace maxima::MREAD-SYNERR)
;(trace maxima::tyi-parse-int)
)
;(load "../lsp/top.lsp")
;(trace read-char)
  
;(setq *standard-output* me)
;(setq *terminal-io* me)
;(setq *debug-io* me)
;(setq *error-io* me)




