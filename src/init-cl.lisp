;********************************************************
; file:        init-cl.lisp                              
; description: Initialize Maxima                         
; date:        Wed Jan 13 1999 - 20:27                   
; author:      Liam Healy <Liam.Healy@nrl.navy.mil>      
;********************************************************

(in-package :maxima)
(use-package "COMMAND-LINE")
;;; An ANSI-CL portable initializer to replace init_max1.lisp

(defvar *maxima-prefix*)
(defvar *maxima-datadir*)
(defvar *maxima-infodir*)
(defvar *maxima-plotdir*)
(defvar *maxima-verpkglibexecdir*)
(defvar *maxima-verpkgdatadir*)
(defvar *maxima-libexecdir*)
(defvar *maxima-userdir*)

(defun maxima-path (dir file)
   (format nil "~a/~a/~a" *maxima-prefix* dir file))

(defun maxima-data-path (dir file)
   (format nil "~a/~a/~a"
	   *maxima-verpkgdatadir* dir file))

(defvar $file_search_lisp nil
  "Directories to search for Lisp source code.")

(defvar $file_search_maxima nil
  "Directories to search for Maxima source code.")

(defvar $file_search_demo nil
  "Directories to search for demos.")

(defvar $file_search_usage nil)
(defvar $chemin nil)

#+gcl
(defun maxima-getenv (envvar)
  (si::getenv envvar))

#+allegro
(defun maxima-getenv (envvar)
  (system:getenv envvar))

#+cmu
(defun maxima-getenv (envvar)
  (cdr (assoc envvar ext:*environment-list* :test #'string=)))

#+sbcl
(defun maxima-getenv (envvar)
  (sb-ext:posix-getenv envvar))

#+clisp
(defun maxima-getenv (envvar)
  (ext:getenv envvar))

#+mcl
(defun maxima-getenv (envvar)
  (ccl::getenv envvar))

(defun set-pathnames ()
  (let ((maxima-prefix-env (maxima-getenv "MAXIMA_PREFIX"))
	(maxima-datadir-env (maxima-getenv "MAXIMA_DATADIR"))
	(maxima-infodir-env (maxima-getenv "MAXIMA_INFODIR"))
	(maxima-plotdir-env (maxima-getenv "MAXIMA_PLOTDIR"))
	(maxima-userdir-env (maxima-getenv "MAXIMA_USERDIR"))
	(maxima-verpkgdatadir-env (maxima-getenv "MAXIMA_VERPKGDATADIR"))
	(home-env (maxima-getenv "HOME")))
    ;; MAXIMA_DIRECTORY is a deprecated substitute for MAXIMA_PREFIX
    (if (not maxima-prefix-env)
	(setq maxima-prefix-env (maxima-getenv "MAXIMA_DIRECTORY")))
    (if maxima-prefix-env
	(setq *maxima-prefix* maxima-prefix-env)
      (setq *maxima-prefix* *autoconf-prefix*))
    (if maxima-datadir-env
	(setq *maxima-datadir* maxima-datadir-env)
      (if maxima-prefix-env
	  (setq *maxima-datadir* (concatenate 'string *maxima-prefix*
					      "/"
					      "share"))
	(setq *maxima-datadir* *autoconf-datadir*)))
    (if maxima-verpkgdatadir-env
	(setq *maxima-verpkgdatadir* maxima-verpkgdatadir-env)
      (setq *maxima-verpkgdatadir* (concatenate 'string
						*maxima-datadir*
						"/"
						*autoconf-package*
						"/"
						*autoconf-version*)))
    (if maxima-prefix-env
	(setq *maxima-libexecdir* (concatenate 'string *maxima-prefix*
					       "/"
					       "libexec"))
      (setq *maxima-libexecdir* *autoconf-libexecdir*))
    (setq *maxima-verpkglibexecdir* (concatenate 'string
						 *maxima-libexecdir*
						 "/"
						 *autoconf-package*
						 "/"
						 *autoconf-version*))
    (if maxima-plotdir-env
	(setq *maxima-plotdir* (maxima-getenv "MAXIMA_PLOTDIR"))
      (setq *maxima-plotdir* *maxima-verpkglibexecdir*))
    (if maxima-infodir-env
	(setq *maxima-infodir* maxima-infodir-env)
      (if maxima-prefix-env
	  (setq *maxima-infodir* (concatenate 'string *maxima-prefix*
					      "/"
					      "info"))
	(setq *maxima-infodir* *autoconf-infodir*)))
    (if maxima-userdir-env
	(setq *maxima-userdir* maxima-userdir-env)
      (setq *maxima-userdir* (concatenate 'string home-env "/.maxima"))))
	 
  
  (let* ((ext #+gcl "o"
	      #+cmu (c::backend-fasl-file-type c::*target-backend*)
	      #+sbcl "fasl"
	      #+clisp "fas"
	      #+allegro "fasl"
	      #+(and openmcl darwinppc-target) "dfsl"
	      #+(and openmcl linuxppc-target) "pfsl"
	      #-(or gcl cmu sbcl clisp allegro openmcl)
	      "")
	 (lisp-patterns (concatenate 'string
				     "###.{"
				     (concatenate 'string ext ",lisp,lsp}")))
	 (share-with-subdirs "{share,share/algebra,share/calculus,share/combinatorics,share/contrib,share/contrib/nset,share/contrib/pdiff,share/diffequations,share/graphics,share/integequations,share/integration,share/macro,share/matrix,share/misc,share/numeric,share/physics,share/simplification,share/specfunctions,share/sym,share/tensor,share/trigonometry,share/utils,share/vector}"))
    (setq $file_search_lisp
	  (list '(mlist)
		;; actually, this entry is not correct.
		;; there should be a separate directory for compiled
		;; lisp code. jfa 04/11/02
		(concatenate 'string *maxima-userdir* "/" lisp-patterns)
		(maxima-data-path share-with-subdirs lisp-patterns)
		(maxima-data-path "src" lisp-patterns)))
  (setq $file_search_maxima
	(list '(mlist)
	      (concatenate 'string *maxima-userdir* "/" "###.{mac,mc}")
	      (maxima-data-path share-with-subdirs "###.{mac,mc}")))
  (setq $file_search_demo
	(list '(mlist)
	      (maxima-data-path share-with-subdirs
					 "###.{dem,dm1,dm2,dm3,dmt}")
	      (maxima-data-path "{demo}"
					 "###.{dem,dm1,dm2,dm3,dmt}")))
  (setq $file_search_usage
	(list '(mlist) (maxima-data-path share-with-subdirs
					 "###.{usg,texi}")
	      (maxima-data-path "doc" "###.{mac}")))
  (setq $chemin
	(maxima-data-path "sym" ""))
  (setq cl-info::*info-paths* (list (concatenate 'string
					    *maxima-infodir* "/")))))

(defun get-dirs (path)
  #+(or :clisp :sbcl)
  (directory (concatenate 'string (namestring path) "/*/"))
  #-(or :clisp :sbcl)
  (directory (concatenate 'string (namestring path) "/*")))
  
(defun unix-like-basename (path)
  (let* ((pathstring (namestring path))
	(len (length pathstring)))
    (if (equal (subseq pathstring (- len 1) len) "/")
	(progn (setf len (- len 1))
	 (setf pathstring (subseq pathstring 0 len))))
    (subseq pathstring (+ (position #\/ pathstring :from-end t) 1) len)))

(defun unix-like-dirname (path)
  (let* ((pathstring (namestring path))
	(len (length pathstring)))
    (if (equal (subseq pathstring (- len 1) len) "/")
	(progn (setf len (- len 1))
	 (setf pathstring (subseq pathstring 0 len))))
    (subseq pathstring 0 (position #\/ pathstring :from-end t))))

(defun list-avail-action ()
  ;; jfa *autoconf-libdir* is only correct if the environment
  ;; variable MAXIMA_PREFIX has not been set. Fixme, please. 11/21/03
  (let* ((mvpldir (concatenate 'string
			       *autoconf-libdir*
			       "/"
			       *autoconf-package*
			       "/"
			       *autoconf-version*))
	 (len (length mvpldir))
	 (base-dir nil)
	 (versions nil)
	 (version-string nil)
	 (lisps nil)
	 (lisp-string nil))
  (format t "Available versions:~%")
  (if (not (equal (subseq mvpldir (- len 1) len) "/"))
      (setf mvpldir (concatenate 'string mvpldir "/")))
  (setf base-dir (unix-like-dirname mvpldir))
  (setf versions (get-dirs base-dir))
  (dolist (version versions)
    (setf lisps (get-dirs version))
    (setf version-string (unix-like-basename version))
    (dolist (lisp lisps)
      (setf lisp-string (unix-like-basename lisp))
      (setf lisp-string (subseq lisp-string (length "binary-") 
				(length lisp-string)))
      (format t "version ~a, lisp ~a~%" version-string lisp-string)))
  (bye)))

;#+gcl (setq si::*top-level-hook* 'user::run)
(defun process-maxima-args (input-stream batch-flag)
;;   (format t "processing maxima args = ")
;;   (mapc #'(lambda (x) (format t "\"~a\"~%" x)) (get-application-args))
;;   (terpri)
  (let ((maxima-options nil))
    (setf maxima-options
	  (list 
	   (make-cl-option :names '("-h" "--help")
			:action #'(lambda () 
				    (format t "usage: maxima [options]~%")
				    (list-cl-options maxima-options)
				    (bye))
			:help-string "Display this usage message.")
	   (make-cl-option :names '("-l" "--lisp")
			:argument "<lisp>"
			:action nil
			:help-string "Use lisp implementation <lisp>.")
	   (make-cl-option :names '("-u" "--use-version")
			:argument "<version>"
			:action nil
			:help-string "Use maxima version <version>.")
	   (make-cl-option :names '("--list-avail")
			:action 'list-avail-action
			:help-string 
			"List the installed version/lisp combinations.")
	   (make-cl-option :names '("-b" "--batch")
			:argument "<file>"
			:action #'(lambda (file)
				    (setf input-stream
					  (make-string-input-stream
					   (format nil "batch(\"~a\");" file)))
				    (setf batch-flag :batch))
			:help-string
			"Process maxima file <file> in batch mode.")
	   (make-cl-option :names '("--batch-lisp")
			:argument "<file>"
			:action #'(lambda (file)
				    (setf input-stream
					  (make-string-input-stream
					   (format nil ":lisp (load \"~a\");"
						   file)))
				    (setf batch-flag :batch))
			:help-string "Process lisp file <file> in batch mode.")
	   (make-cl-option :names '("--batch-string")
			:argument "<string>"
			:action #'(lambda (string)
				    (setf input-stream 
					  (make-string-input-stream string))
				    (setf batch-flag :batch))
			:help-string 
			"Process maxima command(s) <string> in batch mode.")
	   (make-cl-option :names '("-r" "--run-string")
			:argument "<string>"
			:action #'(lambda (string)
				    (setf input-stream
					  (make-string-input-stream string))
				    (setf batch-flag nil))
			:help-string 
			"Process maxima command(s) <string> in interactive mode.")
	   (make-cl-option :names '("-p" "--preload-lisp")
			:argument "<lisp-file>"
			:action #'(lambda (file)
				    (load file))
			:help-string "Preload <lisp-file>.")
	   (make-cl-option :names '("-v" "--verbose")
			:action nil
			:help-string 
			"Display lisp invocation in maxima wrapper script.")
	   (make-cl-option :names '("--version")
			:action #'(lambda ()
				    (format t "Maxima ~a~%" *autoconf-version*)
				    ($quit))
			:help-string 
			"Display the default installed version.")))
    (process-args (get-application-args) maxima-options))
  (values input-stream batch-flag))



(defun user::run ()
  "Run Maxima in its own package."
  (in-package "MAXIMA")
  (setf *load-verbose* nil)
  (setf *debugger-hook* #'maxima-lisp-debugger)
  (let ((input-stream *standard-input*)
	(batch-flag nil))
    (setf (values input-stream batch-flag) 
	  (process-maxima-args input-stream batch-flag))
    #+allegro
    (progn
      (set-readtable-for-macsyma)
      (setf *read-default-float-format* 'lisp::double-float))
      
    (catch 'to-lisp
      (set-pathnames)
      #+(or cmu sbcl clisp allegro mcl)
      (progn
	(loop 
	  (with-simple-restart (macsyma-quit "Macsyma top-level")
			       (macsyma-top-level input-stream batch-flag))))
      #-(or cmu sbcl clisp allegro mcl)
      (catch 'macsyma-quit
	(macsyma-top-level input-stream batch-flag)))))

(import 'user::run)
  
($setup_autoload "eigen.mac" '$eigenvectors '$eigenvalues)

(defun $to_lisp ()
  (format t "~&Type (to-maxima) to restart~%")
  (let ((old-debugger-hook *debugger-hook*))
    (catch 'to-maxima
      (unwind-protect
	  (maxima-read-eval-print-loop)
	(setf *debugger-hook* old-debugger-hook)
	(format t "Returning to Maxima~%"))))
)

(defun to-maxima ()
  (throw 'to-maxima t))

(defun maxima-read-eval-print-loop ()
  (setf *debugger-hook* #'maxima-lisp-debugger-repl)
  (loop
   (catch 'to-maxima-repl
     (format t "~a~%~a> ~a" *prompt-prefix* 
	     (package-name *package*) *prompt-suffix*)
     (let ((form (read)))
       (prin1 (eval form))))))

(defun maxima-lisp-debugger-repl (condition me-or-my-encapsulation)
  (format t "~&Maxima encountered a Lisp error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%To reenable the Lisp debugger set *debugger-hook* to nil.~%")
  (throw 'to-maxima-repl t))

(defun $jfa_lisp ()
  (format t "jfa was here"))

(defvar $help "type describe(topic) or example(topic);")

(defun $help () $help)			;

;; CMUCL needs because when maxima reaches EOF, it calls BYE, not $QUIT.
#+cmu
(defun bye ()
  (ext:quit))

#+sbcl
(defun bye ()
  (sb-ext:quit))

#+allegro
(defun bye ()
  (excl:exit))

#+mcl
(defun bye ()
  (ccl::quit))

(defun $maxima_server (port)
  (load "/home/amundson/devel/maxima/archive/src/server.lisp")
  (user::setup port))
