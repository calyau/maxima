;;load this file to dump maxima under excl
;;Doing this twice will result in less compiler garbage in the image.
;;thanks to  rimey@ucbarpa.Berkeley.EDU (Ken Rimey)
;;for many things in this file.


(setq si:*source-file-types* '("cl" "lsp" "lisp"))
(setq si:*load-search-list*
      `(,(make-pathname :type "fasl")
	,(make-pathname :type "lisp")
	excl::*library-code-fasl-pathname*))
(setq si:*require-search-list* si:*load-search-list*)

(load "sysdef.lisp")
(proclaim '(optimize (speed 3) (safety 0)))
(make::make :maxima :compile t)


(in-package "MAXIMA")
(export 'macsyma-top-level)

(defun macsyma-top-level ()
  (let ((*package* (find-package "MAXIMA")))
    (loop (format t "Top level.~%")
	  (catch 'macsyma-quit
	    (and (excl:errorset (continue) t)
		 (break))))))

(defun top-level::eof-command ()
  (throw 'macsyma-quit nil))

(defun bye ()
  (excl:exit))

(excl:gc)
(excl:dumplisp :name "maxima"
	       :restart-function #'macsyma-top-level
	       :read-init-file nil)
