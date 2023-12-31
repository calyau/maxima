
;; Default linel value for Maxima manual is 65

(mset '$linel 65)

(setf *prompt-prefix* "")
#-gcl (setf *prompt-suffix* "")
#+gcl (setf *prompt-suffix* "
")
(setf *general-display-prefix* "")
(setf *maxima-prolog* "")
(setf *maxima-epilog* "")

(setq $display2d_unicode nil)
