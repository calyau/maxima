(in-package "COMPILER")
(in-package "SYSTEM")
(defvar *command-args* nil)
(in-package "USER")
(in-package "LISP")

(lisp::in-package "SLOOP")

;;Appropriate for Austin
(setq SYSTEM:*DEFAULT-TIME-ZONE*  6)
(in-package "USER")
(load "../gcl-tk/tk-package.lsp")
(progn (allocate 'cons 100) (allocate 'string 40)
 (system:init-system) (gbc t)
 (si::multiply-bignum-stack 25)
 (or lisp::*link-array*
  (setq lisp::*link-array*
     (make-array (ash 1 11)  :element-type 'string-char :fill-pointer 0)))
 (si::use-fast-links t)
(setq compiler::*cmpinclude* "<cmpinclude.h>") (load #"../cmpnew/cmpmain.lsp") (gbc t)
 (load #"../cmpnew/lfun_list.lsp")
 (gbc t) (load #"../cmpnew/cmpopt.lsp") (gbc t)
(load #"../lsp/auto.lsp") (gbc t)

 (when compiler::*cmpinclude-string*
  (with-open-file (st "../h/cmpinclude.h")
    (let
	((tem (make-array (file-length st) :element-type 'standard-char
			  :static t)))
      (if (si::fread tem 0 (length tem) st)
	  (setq compiler::*cmpinclude-string* tem)))))
 ;;compile-file is in cmpmain.lsp

 (setf (symbol-function 'si:clear-compiler-properties)
       (symbol-function 'compiler::compiler-clear-compiler-properties))
; (load "../lsp/setdoc.lsp")
 (setq system::*old-top-level* (symbol-function 'system:top-level))

(defvar si::*lib-directory* (namestring "../"))

      

(defun system::gcl-top-level (&aux tem)
  (si::set-up-top-level)

  (if (si::get-command-arg "-compile")
        (let (;(system::*quit-tag* (cons nil nil))
              ;(system::*quit-tags* nil) (system::*break-level* '())
              ;(system::*break-env* nil) (system::*ihs-base* 1)
              ;(system::*ihs-top* 1) (system::*current-ihs* 1)
              (*break-enable* nil))
             (system:error-set
              '(progn
		 (compile-file (si::get-command-arg "-compile")
			       :output-file 
			       (or (si::get-command-arg "-o")
				   (si::get-command-arg "-compile"))
			       :o-file
			       (cond ((equalp
					     (si::get-command-arg "-o-file")
					       "nil") nil)
				     ((si::get-command-arg "-o-file" t))
				     (t t))
			       :c-file (si::get-command-arg "-c-file" t)
			       :h-file (si::get-command-arg "-h-file" t)
			       :data-file (si::get-command-arg "-data-file" t)
			       :system-p (si::get-command-arg "-system-p" t))))
             (bye (if compiler::*error-p* 1 0))))
  (cond ((si::get-command-arg "-batch")
         (setq si::*top-level-hook* 'bye))
        ((si::get-command-arg "-f"))
        (t  ;; if ANY header or license information is printed by the
            ;; program, then the following License and Enhancement notice
            ;; must be printed (see License).
	 (format t "GCL (GNU Common Lisp)  ~A~%~a~%~a~%" "DATE"
		 "Licensed under GNU Library General Public License"
		 "Contains Enhancements by W. Schelter")))
     (setq si::*ihs-top* 1)
  (in-package 'system::user) (incf system::*ihs-top* 2)
  (funcall system::*old-top-level*))
 (setq si::*gcl-version* 600) 
 (defun lisp-implementation-version nil (format nil "GCL-1-~a" si::*gcl-version*))
 (terpri)
 (setq si:*inhibit-macro-special* t)
 ;(setq *modules* nil)
 (gbc t) (system:reset-gbc-count)
 (allocate 'cons 200)
 (defun system:top-level nil (system::gcl-top-level))
 (unintern 'system)
 (unintern 'lisp)
 (unintern 'compiler)
 (unintern 'user)
 (si::chdir "/home/wfs/maxima-5.4/src")(si::save-system "saved_maxima")
 (if (fboundp 'user-init) (user-init))
 (si::set-up-top-level)
 (in-package "USER")
 (system:save-system "saved_gcl") (bye)
 (defun system:top-level nil (system::gcl-top-level))
 (save "saved_gcl") (bye))

