;;;
;;; To compile Maxima with CMUCL, load this file.  Then
;;; (compile-maxima) to compile everything.  Then (save-maxima)
;;; to save a core file.
;;;
;;; If you want support for maxima describe command, you need to get a
;;; copy of GNU regex and compile and install it.  Change the path
;;; user::run in init_max1.lisp to the appropriate location of
;;; regex.o.
;;;
;;; Also, for things to work, you really need to have a dir file in
;;; the same location as the maxima.info file.
;;;
;;; Finally, you need to set MAXIMA_DIRECTORY appropriately before
;;; running maxima.  This should be the top-level directory of the
;;; maxima sources.  Either here or wherever you installed maxima.
;;; (See the maxima shell script if you installed maxima with gcl for
;;; an appropriate value.)
;;;

;; this is now in maxima package
(defpackage "SI"
    (:use "COMMON-LISP" "ALIEN" "C-CALL"))
(defvar si::*info-paths* nil)

(defpackage "REGEX"
    (:use "COMMON-LISP" "ALIEN" "C-CALL")
  (:export
   ;; Constants
   "+RE-BACKSLASH-ESCAPE-IN-LISTS+"
   "+RE-BK-PLUS-QM+"
   "+RE-CHAR-CLASSES+"
   "+RE-CONTEXT-INDEP-ANCHORS+"
   "+RE-CONTEXT-INDEP-OPS+"
   "+RE-CONTEXT-INVALID-OPS+"
   "+RE-DOT-NEWLINE+"
   "+RE-DOT-NOT-NULL+"
   "+RE-HAT-LISTS-NOT-NEWLINE+"
   "+RE-INTERVALS+"
   "+RE-LIMITED-OPS+"
   "+RE-NEWLINE-ALT+"
   "+RE-NO-BK-BRACES+"
   "+RE-NO-BK-PARENS+"
   "+RE-NO-BK-REFS+"
   "+RE-NO-BK-VBAR+"
   "+RE-NO-EMPTY-RANGES+"
   "+RE-UNMATCHED-RIGHT-PAREN-ORD+"
   ";; COMMON REGEXP SYNTAXES"
   "+RE-SYNTAX-EMACS+"
   "+RE-SYNTAX-EGREP+"
   "+RE-SYNTAX-POSIX-COMMON+"
   "+RE-SYNTAX-POSIX-BASIC+"
   "+RE-SYNTAX-POSIX-EXTENDED+"
   "+RE-SYNTAX-SPENCER+"
   ;; Variables
   "*MATCH-DATA*"
   "*CASE-FOLD-SEARCH*"
   ;; Functions
   "MATCH-DATA-START"
   "MATCH-DATA-END"
   "STRING-MATCH"
   "MATCH-BEGINNING"
   "MATCH-END"
   ))

(push :main-files-loaded *features*)
(load "sysdef.lisp")
(load "make.lisp")

(defun compile-maxima ()
  (compile-file "cmulisp-regex")
  (compile-file "cl-info")
  (make::make :maxima :compile t))


(defun save-maxima ()
  ;;(load "cmulisp-regex" :if-source-newer :compile)
  ;;(load "cl-info" :if-source-newer :compile)
  (make::make :maxima)
  (setq maxima::*maxima-directory* (namestring (truename "../")))
  (load "init_max1")
  (ext:gc)
  (ext:save-lisp "maxima.core" :init-function #'user::run
               :load-init-file nil :site-init nil))

(in-package "MAXIMA")
(shadow '(lisp::compiled-function-p) (find-package "MAXIMA"))

(defun compiled-function-p (x)
  (and (functionp x) (not (symbolp x))
    (not (eval:interpreted-function-p x))))

(defmacro clines (x) nil)
(defun getenv (x)
  (cdr (assoc (intern x (find-package "KEYWORD")) ext:*environment-list*)))

(defun $system (&rest x)
  (let ((cmdline (apply '$sconcat x)))
    (ext:run-program "/bin/sh"   (list "-c" cmdline))
))

(defvar *init-run* nil)
(defun init-maxima ()
  ;; Turn off gc messages
  (unless *init-run*
    (setq *init-run* t)
  (setf ext:*gc-verbose* nil)
  ;; Reload the documentation stuff.
  (handler-case
      (progn
	(ext:load-foreign *regex-lib*)
	(load (maxima-path "src" "cmulisp-regex") :if-source-newer :compile)
	(load (maxima-path "src" "cl-info") :if-source-newer :compile))
    (file-error ()
      (format t "~&Regex support files not found.  Skipping regexp stuff for describe~%")))
))

(defvar maxima::*maxima-directory* nil)


;; Set this to where GNU regex.o can be found.
(defvar *regex-lib*
  "/apps/src/regex-0.12/regex.o")
#+nil
(ext:defswitch "dir"
    #'(lambda (switch)
	(let ((dirpath (ext:cmd-switch-arg switch)))
	  ;; Make sure it ends with a slash
	(setf *maxima-directory*
	      (if (eql (aref dirpath (1- (length dirpath))) #\/)
		  dirpath
		  (concatenate 'string dirpath "/"))))))

;; define bye so that quit() will work in maxima
(defun bye () (ext:quit))





