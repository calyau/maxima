(cond ((and (find-package "MAXIMA")
	    (fboundp (intern "$SOLVE" 'maxima))))
      (t (and    (find-package "SLOOP") (load "sysdef.lisp")))      )
(proclaim '(optimize (safety 2) (speed 2) (space 2)))