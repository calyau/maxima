
;; Default linel value for Maxima manual is 65

(progn (msetchk '$linel 65) (setq $linel 65))

(setf *prompt-prefix* "")
#-gcl (setf *prompt-suffix* "")
#+gcl (setf *prompt-suffix* "
")
(setf *general-display-prefix* "")
(setf *maxima-prolog* "")
(setf *maxima-epilog* "")


