
(in-package 'si)

(setq *files*
 (sloop:sloop for v in 
   (sloop:sloop for v in (get :maxima :files) when (listp v)
		append (unless (keywordp (car v)) v) else collect v)
       collect (string-downcase (string v))))

#+bad
(progn
  (setq *unixport-directory* #"($PORTDIR)")
  (setq *lsp-directory* #"($LSPDIR)")
  (setq *cmpnew-directory* #"($CMPDIR)")
  (setq *h-directory* #"($HDIR)")
  (setq *o-directory* #"($ODIR)"))
(setq *kcl-home-directory* #"../xkcl/")
(setq *unixport-directory* #"../max/")
(setq *lsp-directory* #"../xkcl/lsp/")
(setq *cmpnew-directory* #"../xkcl/cmpnew/")
(setq *h-directory* #"../xkcl/h/")
(setq *o-directory* #"../xkcl/o/")
(setq *files-directory* #"../max/")

(load "/usr/local/schelter/xkcl/unixport/defsystem.lsp")

(defkcl :system-name "maxima" :makefile "ymakefile")

;(defun init-file ( library  &optional (dir  #"")
;				  (stream *standard-output*))
;  (format stream
;	  "	printf(\"Initializing ~A...  \");  fflush(stdout);~%"
;	  library)
;  (format stream
;	  "	fasl_data = read_fasl_data(\"~A\");~%"
;	  (namestring
;	   (merge-pathnames (change-file-type library
;					      "o")
;			    dir)))
;  (format stream "	init_~A(NULL, 0, fasl_data);~%" library)
;  (format stream
;	  "	printf(\"\\n\");  fflush(stdout);~%")
;  (format stream "~%"))
;
;
;(defun write-init (&optional (stream t) )
;  (format stream "#include \"include.h\"
;
;static object fasl_data;
;
;init_init()
;{
;	enter_mark_origin(&fasl_data);
;	fasl_data = Cnil; ~2%
;	load(\"../lsp/export.lsp\");")
;  
;
;  (sloop for v in *object-files* do (init-file v *o-directory* stream))
;  (format stream "	load(\"~A\");~%"
;	  (namestring (merge-pathnames "autoload.lsp" *lsp-directory*)))
;  (sloop for v in *lsp-object-files* do (init-file v *lsp-directory* stream))
;  (format stream "}~%~%")
;
;  (format stream "init_system()~%{~%")
;  (sloop for v in *all-libraries* do (init-file v *lsp-directory* stream))
;  (format stream
;	  "~%	Vpackage->s.s_dbind = user_package;~%")
;  (format stream "}~%"))



;(defun write-make (&optional (fi "ymakefile"))
;  (with-open-file
;   (st fi :direction :output)
;    (format st "MOBJS =")
;    (sloop for v in all
;	   for i from 1 
;	   when (zerop v (mod i 3)) do (format st "\\~%	")
;	   do (format st "($MDIR)/~A.o" (string-downcase (string v))))))
;
;(defun write-sys (&optional (fi "sys_max"))
; (format st "init_maxima(){~%"
; (sloop for v in all
;	do (setq v (string-downcase (string v)))	
;	(format st
;		"	fasl_data = read_fasl_data(\"~A\");~%"
;		(namestring
;		 (merge-pathnames (change-file-type st "o")
;                                *Maxima-directory*)))
;	(format stream "printf(\"Initializing ~a...  \");  fflush(stdout);~%	printf(\"\n\");  fflush(stdout);" v)
;	(format stream "	init_~A(NULL, 0, fasl_data);~%" library)
;	)
; (format st "}")
;))





