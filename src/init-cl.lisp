;********************************************************
; file:        init-cl.lisp                              
; description: Initialize Maxima                         
; date:        Wed Jan 13 1999 - 20:27                   
; author:      Liam Healy <Liam.Healy@nrl.navy.mil>      
;********************************************************

(in-package :maxima)

;;; An ANSI-CL portable initializer to replace init_max1.lisp

(defvar *maxima-prefix*)
(defvar *maxima-datadir*)
(defvar *maxima-infodir*)
(defvar *maxima-plotdir*)
(defvar *maxima-verpkglibexecdir*)
(defvar *maxima-verpkgdatadir*)

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

#+clisp
(defun maxima-getenv (envvar)
  (ext:getenv envvar))

(defun set-pathnames ()
  (let ((maxima-prefix-env (maxima-getenv "MAXIMA_PREFIX"))
	(maxima-datadir-env (maxima-getenv "MAXIMA_DATADIR"))
	(maxima-infodir-env (maxima-getenv "MAXIMA_INFODIR"))
	(maxima-plotdir-env (maxima-getenv "MAXIMA_PLOTDIR")))
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
    (setq *maxima-verpkgdatadir* (concatenate 'string
					      *maxima-datadir*
					      "/"
					      *autoconf-package*
					      "/"
					      *autoconf-version*))
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
					      "info"))
	(setq *maxima-infodir* *autoconf-infodir*))))
	 
  (let ((ext #+gcl "o"
	     #+cmu (c::backend-fasl-file-type c::*target-backend*)
	     #+clisp "fas"
	     #+allegro "fasl"
	     #-(or gcl cmu clisp allegro)
	     ""))
    (setq $file_search_lisp
	  (list '(mlist)
		;; actually, this entry is not correct.
		;; there should be a separate directory for compiled
		;; lisp code. jfa 04/11/02
		(maxima-data-path "{src,share}"
				  (concatenate 'string "###." ext))
		(maxima-data-path "{src,share}" "###.lisp")
		(maxima-data-path "{src,share}" "###.lsp"))))
  (setq $file_search_maxima
	(list '(mlist)
	      (maxima-data-path "{share}" "###.mac")
	      (maxima-data-path "{share}" "###.mc")))
  (setq $file_search_demo
	(list '(mlist) (maxima-data-path "{demo,share}"
					 "###.{dem,dm1,dm2,dm3,dmt}")))
  (setq $file_search_usage
	(list '(mlist) (maxima-data-path "{share}"
					 "###.{usg,texi}")
	      (maxima-data-path "doc" "###.{mac}")))
  (setq $chemin
	(maxima-data-path "sym" ""))
  (setq si::*info-paths* (list (concatenate 'string
					    *maxima-infodir* "/")))
  )

;#+gcl (setq si::*top-level-hook* 'user::run)
(defun user::run ()
  "Run Maxima in its own package."
  (in-package "MAXIMA")
  ; jfa new command-line communication
  (let ((input-string *standard-input*)
	(maxima_int_lisp_preload (maxima-getenv "MAXIMA_INT_LISP_PRELOAD"))
	(maxima_int_input_string (maxima-getenv "MAXIMA_INT_INPUT_STRING"))
	(batch-flag (maxima-getenv "MAXIMA_INT_BATCH_FLAG")))
    (if maxima_int_lisp_preload
	(load maxima_int_lisp_preload))
    (if maxima_int_input_string
	(setq input-string (make-string-input-stream maxima_int_input_string)))
      
    (catch 'to-lisp
      (set-pathnames)
      #+cmu
      (progn
	(loop 
	  (with-simple-restart (macsyma-quit "Macsyma top-level")
			       (macsyma-top-level input-string batch-flag))))
      #-cmu
      (catch 'macsyma-quit
	(macsyma-top-level input-string batch-flag)))))

(import 'user::run)
  
($setup_autoload "eigen.mc" '$eigenvectors '$eigenvalues)

(defun $to_lisp ()
  (format t "~%Type (run) to restart~%")
  (throw 'to-lisp t))

(defun $jfa_lisp ()
  (format t "jfa was here"))

(defvar $help "type describe(topic) or example(topic);")

(defun $help () $help)			;

;; CMUCL needs because when maxima reaches EOF, it calls BYE, not $QUIT.
#+cmu
(defun bye ()
  (ext:quit))

;;add the quit maxima for the clisp debugger.
#+clisp
(progn
  
(or (fboundp 'commands1.orig)
  (setf (fdefinition 'commands1.orig) (fdefinition 'sys::commands1)))


(defun sys::commands1 ()
  (append (list "
Quit to top    :q       Quit to MAXIMA top level"
                (cons ":q" #'(lambda () (throw 'macsyma-quit nil))))
          (commands1.orig)))

) ; end progn for clisp

(defun $maxima_server (port)
  (load "/home/amundson/devel/maxima/archive/src/server.lisp")
  (user::setup port))