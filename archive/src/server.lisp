;; very simple server started on port
;; (setup 

;(setq si::*notify-gbc* t)


(and (find-package "MAXIMA") (push :maxima *features*))
#+maxima
(in-package "MAXIMA")
(defun user::setup ( port &optional (host "localhost"))
  (setq me (si::socket port :host host))
;  (si::set-blocking me t)
  ;; we exit if we get a sigpipe, usually caused by
  ;; the other side shutting down..
  (setq si::*sigpipe-action* 'si::bye)
  (setq *socket-connection* me)
  (setq *standard-input* me)
  (setq *standard-output* me)
  (setq *error-output* me)
  (setq *terminal-io* me)
  (format t "pid=~a~%" (si::getpid))
  (setq *debug-io* me)
  

  )

#+maxima
(progn
(setq $in_netmath t)
(setq $show_openplot nil)


;(trace dbm-read)
;(trace mread-raw)
;(trace si::dbl-read)
;(trace si::break-level)
;(trace maxima::MREAD-SYNERR)
;(trace maxima::tyi-parse-int)
)
;(load "../lsp/top.lsp")
;(trace read-char)
  
;(setq *standard-output* me)
;(setq *terminal-io* me)
;(setq *debug-io* me)
;(setq *error-io* me)








