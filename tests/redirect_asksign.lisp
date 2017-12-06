;;; asksign reads answers to questions from *query-io*, not from *standard-input*.
;;; Which on some lisps means that the answer for questions is read from the keyboard
;;; even if the standard input comes from a file. Which might be a feature.
;;; But for the automatic "make check" test script we need a way around this.
;;;
;;; Thanks to Tomio Arisaka for this file.

;; a copy of the function in the file "macdes.lisp"
;; that changes the variable *query-io* to *standard-input*
;;
(defun mread-noprompt (&rest read-args)
  (let ((*mread-prompt* "") (*prompt-on-read-hang*))
    (declare (special *mread-prompt* *prompt-on-read-hang*))
    (unless read-args (setq read-args (list *standard-input*))) ; *query-io* to *standard-input*
    (caddr (apply #'mread read-args))))

;; a copy of the function in the file "macsys.lisp"
;; that changes the variable *query-io* to *standard-input*
;;
(defun retrieve (msg flag &aux (print? nil))
  (declare (special msg flag print?))
  (or (eq flag 'noprint) (setq print? t))
  (cond ((not print?)
     (setq print? t)
         (format-prompt t ""))
    ((null msg)
         (format-prompt t ""))
    ((atom msg)
         (format-prompt t "~A" msg)
     (mterpri))
    ((eq flag t)
         (format-prompt t "~{~A~}" (cdr msg))
     (mterpri))
    (t
         (format-prompt t "~M" msg)
     (mterpri)))
  (let ((res (mread-noprompt *standard-input* nil))) ; *query-io* to *standard-input*
    (princ *general-display-prefix*)
    res))
