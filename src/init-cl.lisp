;********************************************************
; file:        init-cl.lisp                              
; description: Initialize Maxima                         
; date:        Wed Jan 13 1999 - 20:27                   
; author:      Liam Healy <Liam.Healy@nrl.navy.mil>      
;********************************************************

(in-package :maxima)

;;; An ANSI-CL portable initializer to replace init_max1.lisp

;;; Each installation must make sure that this logical path is defined,
;;; or each user must define the environment variable MAXIMA_DIRECTORY
#-gcl(defparameter *maxima-directory*
    (ignore-errors (translate-logical-pathname "maxima:maxima-54;")))
#+gcl(defvar *maxima-directory* "/home/amundson/notwork/src/maxima-clocc")

(defun maxima-path (dir file)
   (format nil "~a~a/~a" *maxima-directory* dir file))

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
  (car (assoc envvar ext:*environment-list* :test #'string=)))

#+clisp
(defun maxima-getenv (envvar)
  (ext:getenv envvar))

(defun set-pathnames ()
  (let* ((tem-env (maxima-getenv "MAXIMA_DIRECTORY"))
	 (tem (if tem-env tem-env *autoconf-prefix*)))
    (if (and tem (> (length tem) 0))
	(or (eql (aref tem (1- (length tem))) #\/)
	    (setq tem (format nil "~a/" tem))))
    (when tem
      (setq *maxima-directory* tem)))
  (let ((ext #+gcl "o"
	     #+cmu (c::backend-fasl-file-type c::*target-backend*)
	     #+clisp "fas"
	     #+allegro "fasl"
	     #-(or gcl cmu clisp allegro)
	     ""))
    (setq $file_search_lisp
	  (list '(mlist)
		(maxima-path "{src,share1,sym}"
			     (concatenate 'string "###." ext))
		(maxima-path "{src,share1}" "###.lisp")
		(maxima-path "{sym}" "###.lsp"))))
  (setq $file_search_maxima
    (list '(mlist)
	  (maxima-path "{mac,sym}" "###.mac")
	  (maxima-path "{share,share1,share2,tensor}" "###.mc")))
  (setq $file_search_demo
    (list '(mlist) (maxima-path "{demo,share,share1,share2}"
				"###.{dem,dm1,dm2,dm3,dmt}")))
  (setq $file_search_usage
    (list '(mlist) (maxima-path "{demo,share,share1,share2}"
				"###.{usg,texi}")
	  (maxima-path "doc" "###.{mac}")))
  (setq $chemin
    (maxima-path "sym" "")))

;#+gcl (setq si::*top-level-hook* 'user::run)
(defun user::run ()
  "Run Maxima in its own package."
  (in-package "MAXIMA")
  (catch 'to-lisp
    (set-pathnames)
    #+cmu
    (progn
      (loop 
	  (with-simple-restart (macsyma-quit "Macsyma top-level")
	    (macsyma-top-level))))
    #-cmu
    (catch 'macsyma-quit
      (macsyma-top-level))))

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
