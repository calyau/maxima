(load "sysdef.lisp")
(load "serror.lisp")

(setq lisp-dir "/d2/wfs/gcl-2.3")
(load (concatenate 'string user::lisp-dir "/cmpnew/collectfn"))


;(mapcar 'load (directory (concatenate 'string user::lisp-dir "/lsp/*.fn")))
(load "/home/wfs/gcl-2.3/lsp/sys-proclaim.lisp")


(compiler::no-make-proclaims-hack)
(let (*load-verbose*)(mapcar #'(lambda (x) (load x) (print x)) (directory "*.fn")))
(in-package "MAXIMA")
;; proclaim single valued some functions it does not know about
(proclaim
 '(ftype (function (*) t) meval1
	 quot merror errrjf sort typep inf-signal mread $filename_merge
	 maclisp-typep SCAN-DIGITS dimension maset1 error cerror
	 $file_search mtell))


(with-open-file (st "SYS-PROCLAIM.lisp" :direction :output)
  (print '(in-package "SERROR") st)
  (print '(in-package "MAXIMA") st)
  (compiler::make-proclaims st))

  
  

