;; to fix the paths edit this file and load it at the end
;; or put it in the sys-init.lisp in the directory which is given
;; as the second argument in execution of the executable image.
;; then it will be loaded..
(in-package "MAXIMA")
(setq $describe_documentation "/public/maxima/doc/macsym.doc")

(setq *describe-index-directory*  "/public/maxima/doc/")

(format t "Edit the file macsym-index.LISP setting the paths there.." )
