;;********************************************************
;; file:        init-cl.lisp
;; description: Initialize Maxima
;; date:        Wed Jan 13 1999 - 20:27
;; author:      Liam Healy <Liam.Healy@nrl.navy.mil>
;;********************************************************

;;; An ANSI-CL portable initializer to replace init_max1.lisp

;; CL-USER:*MAXIMA-BUILD-TIME* is defined in maxima.asd and maxima.system,
;; but I guess ECL doesn't see that, so define it here.
#+ecl (progn
  (in-package :cl-user)
  (defvar *maxima-build-time* '#.(multiple-value-list (get-decoded-time)))
  (export '*maxima-build-time*))

(in-package :maxima)

;;; Locations of various types of files. These variables are discussed
;;; in more detail in the file doc/implementation/dir_vars.txt. Since
;;; these are already in the maxima package, the maxima- prefix is
;;; redundant. It is kept for consistency with the same variables in
;;; shell scripts, batch scripts and environment variables.
;;; jfa 02/07/04

(defvar *maxima-topdir*)        ;; top-level installation or build directory
(defvar *maxima-imagesdir*)
(defvar *maxima-sharedir*)
(defvar *maxima-srcdir*)
(defvar *maxima-docdir*)
(defvar *maxima-layout-autotools*)
(defvar *maxima-demodir*)
(defvar *maxima-objdir*)		;; Where to store object (fasl) files.

(defun shadow-string-assignment (var value)
  (cond
    ((stringp value)
     (setf (symbol-value (get var 'lisp-shadow)) value)
     value)
    (t
      (merror (intl:gettext "assignment: must assign a string to ~:M; found: ~M") var value))))

(defun print-directories ()
  (format t "maxima-prefix=~a~%" *maxima-prefix*)
  (format t "maxima-topdir=~a~%" *maxima-topdir*)
  (format t "maxima-imagesdir=~a~%" *maxima-imagesdir*)
  (format t "maxima-sharedir=~a~%" *maxima-sharedir*)
  (format t "maxima-srcdir=~a~%" *maxima-srcdir*)
  (format t "maxima-demodir=~a~%" *maxima-demodir*)
  (format t "maxima-testsdir=~a~%" *maxima-testsdir*)
  (format t "maxima-docdir=~a~%" *maxima-docdir*)
  (format t "maxima-infodir=~a~%" *maxima-infodir*)
  (format t "maxima-htmldir=~a~%" *maxima-htmldir*)
  (format t "maxima-plotdir=~a~%" *maxima-plotdir*)
  (format t "maxima-layout-autotools=~a~%" *maxima-layout-autotools*)
  (format t "maxima-userdir=~a~%" *maxima-userdir*)
  (format t "maxima-tempdir=~a~%" *maxima-tempdir*)
  (format t "maxima-lang-subdir=~a~%" *maxima-lang-subdir*)
  (format t "maxima-objdir=~A~%" *maxima-objdir*))

(defvar *maxima-lispname*
        #+clisp "clisp"
	#+cmu "cmucl"
	#+scl "scl"
	#+sbcl "sbcl"
	#+gcl "gcl"
	#+allegro "acl"
	#+openmcl "openmcl"
	#+abcl "abcl"
	#+lispworks "lispworks"
	#+ecl "ecl"
	#-(or clisp cmu scl sbcl gcl allegro openmcl abcl lispworks ecl) "unknownlisp")

(defun maxima-parse-dirstring (str)
  (let ((sep "/"))
    (if (position (character "\\") str)
	(setq sep "\\"))
    (setf str (concatenate 'string (string-right-trim sep str) sep))
    (concatenate 'string
		 (let ((dev (pathname-device str)))
		   (if (consp dev)
		       (setf dev (first dev)))
		   (if (and dev (not (eq dev :unspecific))
			    (not (string= dev "")))
		       (concatenate 'string (string-right-trim ":" dev) ":")
		       ""))
		 "/"
		 (apply #'combine-path (rest (pathname-directory str))))))

(defun set-pathnames-with-autoconf (maxima-prefix-env maxima-docprefix-env)
  (declare (ignore maxima-docprefix-env))
  (let (libdir libexecdir datadir infodir
	(package-version (combine-path *autoconf-package* *autoconf-version*))
	(binary-subdirectory (concatenate 'string "binary-" *maxima-lispname*)))
    (if maxima-prefix-env
	(progn
	  (setq libdir     (combine-path maxima-prefix-env "lib"))
	  (setq libexecdir (combine-path maxima-prefix-env "libexec"))
	  (setq datadir    (combine-path maxima-prefix-env "share"))
	  (setq infodir    (combine-path maxima-prefix-env #+(or cygwin windows win32 win64) "share" "info")))
	(progn
	  (setq libdir     (maxima-parse-dirstring *autoconf-libdir*))
	  (setq libexecdir (maxima-parse-dirstring *autoconf-libexecdir*))
	  (setq datadir    (maxima-parse-dirstring *autoconf-datadir*))
	  (setq infodir    (maxima-parse-dirstring *autoconf-infodir*))))
    (setq *maxima-topdir*    (combine-path datadir package-version))
    (setq *maxima-imagesdir* (combine-path libdir package-version binary-subdirectory))
    (setq *maxima-sharedir*  (combine-path datadir package-version "share"))
    (setq *maxima-srcdir*    (combine-path datadir package-version "src"))
    (setq *maxima-demodir*   (combine-path datadir package-version "demo"))
    (setq *maxima-testsdir*  (combine-path datadir package-version "tests"))
    (setq *maxima-docdir*    (combine-path datadir package-version "doc"))
    (setq *maxima-infodir*   infodir)
    (setq *maxima-htmldir*   (combine-path datadir package-version "doc" "html"))
    (setq *maxima-plotdir*   (combine-path libexecdir package-version))))

(defun set-pathnames-without-autoconf (maxima-prefix-env maxima-docprefix-env)
  (let* ((maxima-prefix (if maxima-prefix-env
			   maxima-prefix-env
			   (maxima-parse-dirstring *autoconf-prefix*)))
	(binary-subdirectory (concatenate 'string "binary-" *maxima-lispname*)))

    (setq *maxima-topdir*    maxima-prefix)
    (setq *maxima-imagesdir* (combine-path maxima-prefix "src" binary-subdirectory))
    (setq *maxima-sharedir*  (combine-path maxima-prefix "share"))
    (setq *maxima-srcdir*    (combine-path maxima-prefix "src"))
    (setq *maxima-demodir*   (combine-path maxima-prefix "demo"))
    (setq *maxima-testsdir*  (combine-path maxima-prefix "tests"))
    (let ((maxima-doc-prefix (if maxima-docprefix-env
				maxima-docprefix-env
			        maxima-prefix)))
      (setq *maxima-docdir*    (combine-path maxima-doc-prefix "doc"))
      (setq *maxima-infodir*   (combine-path maxima-doc-prefix "doc" "info"))
      (setq *maxima-htmldir*   (combine-path maxima-doc-prefix "doc" "html")))
    (setq *maxima-plotdir*   (combine-path maxima-prefix "plotting"))))

(defun default-userdir ()
  (let ((home-env (maxima-getenv "HOME"))
	(base-dir "")
	(maxima-dir (if (string= *autoconf-windows* "true")
			"maxima"
			".maxima")))
    (setf base-dir
	  (if (and home-env (string/= home-env ""))
	      ;; use home-env...
	      (if (string= home-env "c:\\")
		  ;; but not if home-env = c:\, which results in slow startups
		  ;; under windows. Ick.
		  "c:\\user\\"
		  home-env)
	      ;; we have to make a guess
	      (if (string= *autoconf-windows* "true")
		  "c:\\user\\"
		  "/tmp")))
    (combine-path (maxima-parse-dirstring base-dir) maxima-dir)))

(defun default-tempdir ()
  (maxima-parse-dirstring
    (let ((tmpdir-windows (maxima-getenv "TEMP"))
 	 (tmpdir-posix (maxima-getenv "TMPDIR"))
	 (tmpdir-nonstandard1 (maxima-getenv "TMP"))
	 (tmpdir-nonstandard2 (maxima-getenv "TEMPDIR")))

	 (cond
	   ((and tmpdir-windows (string/= tmpdir-windows "")) tmpdir-windows)
	   ((and tmpdir-posix (string/= tmpdir-windows "")) tmpdir-posix)
	   ((and tmpdir-nonstandard1 (string/= tmpdir-nonstandard1 "")) tmpdir-nonstandard1)
	   ((and tmpdir-nonstandard2 (string/= tmpdir-nonstandard2 "")) tmpdir-nonstandard2)
	   ; A fallback for windows if everything else has failed
           ((string= *autoconf-windows* "true") "C:\\Windows\\temp")
           ; A fallback for the rest of the operating systems
           (t "/tmp")))))

(defun set-locale-subdir ()
  (let (language territory #+nil codeset)
    ;; Determine *maxima-lang-subdir*
    ;;   1. from MAXIMA_LANG_SUBDIR environment variable
    ;;   2. from INTL::*LOCALE* if (1) fails
    (unless  (setq *maxima-lang-subdir* (maxima-getenv "MAXIMA_LANG_SUBDIR"))
      (cond ((or (null intl::*locale*) (equal intl::*locale* ""))
             (setq *maxima-lang-subdir* nil))
            ((member intl::*locale* '("C" "POSIX" "c" "posix") :test #'equal)
             (setq *maxima-lang-subdir* nil))
            (t
              ;; Code to parse code set in locale string, in case we figure out
              ;; something to do with it; it isn't needed for language
              ;; subdirectory any more, since all language files are UTF-8.
              ;; We might make use of code set in ADJUST-CHARACTER-ENCODING.
              #+nil (when (eql (position #\. intl::*locale*) 5)
                (setq codeset (string-downcase (subseq intl::*locale* 6))))
              (when (eql (position #\_ intl::*locale*) 2)
                (setq territory (string-downcase (subseq intl::*locale* 3 5))))
              (setq language (string-downcase (subseq intl::*locale* 0 2)))
              ;; Set *maxima-lang-subdir* only for known languages.
              ;; Extend procedure below as soon as new translation
              ;; is available.
              (cond ((equal language "en") ;; English
                     (setq *maxima-lang-subdir* nil))
                ;; Latin-1 aka iso-8859-1 languages
                ((member language '("es" "pt" "de") :test #'equal)
                 (if (and (string= language "pt") (string= territory "br"))
                   (setq *maxima-lang-subdir* (concatenate 'string language "_BR"))
                   (setq *maxima-lang-subdir* language)))
                ;; Japanese.
                ((string= language "ja")
                 (setq *maxima-lang-subdir* language))
                ;; Russian.
                ((string= language "ru")
                 (setq *maxima-lang-subdir* language))
                (t  (setq *maxima-lang-subdir* nil))))))))

(flet ((sanitize-string (s)
	 (map 'string (lambda(x) (if (alphanumericp x) x #\_))
	      (subseq s 0 (min 142 (length s))))))
  (defun lisp-implementation-version1 ()
    (sanitize-string (lisp-implementation-version)))
  (defun maxima-version1 ()
    (sanitize-string *autoconf-version*)))

(defun set-pathnames ()
  (let ((maxima-prefix-env (maxima-getenv "MAXIMA_PREFIX"))
	(maxima-layout-autotools-env (maxima-getenv "MAXIMA_LAYOUT_AUTOTOOLS"))
	(maxima-userdir-env (maxima-getenv "MAXIMA_USERDIR"))
	(maxima-docprefix-env (maxima-getenv "MAXIMA_DOC_PREFIX"))
	(maxima-tempdir-env (maxima-getenv "MAXIMA_TEMPDIR"))
	(maxima-objdir-env (maxima-getenv "MAXIMA_OBJDIR"))
	(maxima-htmldir-env (maxima-getenv "MAXIMA_HTMLDIR")))
    ;; MAXIMA_DIRECTORY is a deprecated substitute for MAXIMA_PREFIX
    (unless maxima-prefix-env
      (setq maxima-prefix-env (maxima-getenv "MAXIMA_DIRECTORY")))
    (if maxima-prefix-env
	(setq *maxima-prefix* maxima-prefix-env)
	(setq *maxima-prefix* (maxima-parse-dirstring *autoconf-prefix*)))
    (if maxima-layout-autotools-env
	(setq *maxima-layout-autotools*
	      (string-equal maxima-layout-autotools-env "true"))
	(setq *maxima-layout-autotools*
	      (string-equal *maxima-default-layout-autotools* "true")))
    (if *maxima-layout-autotools*
	(set-pathnames-with-autoconf maxima-prefix-env maxima-docprefix-env)
	(set-pathnames-without-autoconf maxima-prefix-env maxima-docprefix-env))
    (if maxima-userdir-env
	(setq *maxima-userdir* (maxima-parse-dirstring maxima-userdir-env))
	(setq *maxima-userdir* (default-userdir)))
    (if maxima-tempdir-env
	(setq *maxima-tempdir* (maxima-parse-dirstring maxima-tempdir-env))
	(setq *maxima-tempdir* (default-tempdir)))
    ;; Default *MAXIMA-OBJDIR* is <userdir>/binary/binary-<foo>lisp,
    ;; because userdir is almost surely writable, and we don't want to clutter up
    ;; random directories with Maxima stuff.
    ;; Append binary-<foo>lisp whether objdir is the default or obtained from environment.
    (setq *maxima-objdir*
          (concatenate 'string
                       (if maxima-objdir-env
                         (maxima-parse-dirstring maxima-objdir-env)
                         (concatenate 'string *maxima-userdir* "/binary"))
                       "/" (maxima-version1) "/" *maxima-lispname* "/" (lisp-implementation-version1)))

    (when maxima-htmldir-env
      (setq *maxima-htmldir* (combine-path (maxima-parse-dirstring maxima-htmldir-env) "doc" "info")))

    ;; On ECL the testbench fails mysteriously if this directory doesn't exist =>
    ;; let's create it by hand as a workaround.
    #+ecl (ensure-directories-exist (concatenate 'string *maxima-objdir* "/"))
    
    ; Assign initial values for Maxima shadow variables
    (setq $maxima_userdir *maxima-userdir*)
    (setf (gethash '$maxima_userdir *variable-initial-values*) *maxima-userdir*)
    (setq $maxima_tempdir *maxima-tempdir*)
    (setf (gethash '$maxima_tempdir *variable-initial-values*) *maxima-tempdir*)
    (setq $maxima_objdir *maxima-objdir*)
    (setf (gethash '$maxima_objdir *variable-initial-values*) *maxima-objdir*))

  (let* ((ext #+gcl "o"
	      #+(or cmu scl) (c::backend-fasl-file-type c::*target-backend*)
	      #+sbcl "fasl"
	      #+clisp "fas"
	      #+allegro "fasl"
	      #+openmcl (pathname-type ccl::*.fasl-pathname*)
	      #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
	      #+ecl "fas"
              #+abcl "abcl"
	      #-(or gcl cmu scl sbcl clisp allegro openmcl lispworks ecl abcl)
	      "")
	 (lisp-patterns (concatenate 'string "$$$.{" ext ",lisp,lsp}"))
	 (maxima-patterns "$$$.{mac,mc,wxm}")
	 (lisp+maxima-patterns (concatenate 'string "$$$.{" ext ",lisp,lsp,mac,mc,wxm}"))
	 (demo-patterns "$$$.{dem,dm1,dm2,dm3,dmt}")
	 (usage-patterns "$$.{usg,texi}")
	 (share-subdirs-list (share-subdirs-list))
	 ;; Smash the list of share subdirs into a string of the form
	 ;; "{affine,algebra,...,vector}" .
	 (share-subdirs (format nil "{~{~A~^,~}}" share-subdirs-list)))

    (setq $file_search_lisp
	  (list '(mlist)
		;; actually, this entry is not correct.
		;; there should be a separate directory for compiled
		;; lisp code. jfa 04/11/02
		(combine-path *maxima-userdir* lisp-patterns)
		(combine-path *maxima-sharedir* lisp-patterns)
		(combine-path *maxima-sharedir* share-subdirs lisp-patterns)
		(combine-path *maxima-srcdir* lisp-patterns)
        (combine-path *maxima-topdir* lisp-patterns)))
    (setq $file_search_maxima
	  (list '(mlist)
		(combine-path *maxima-userdir* maxima-patterns)
		(combine-path *maxima-sharedir* maxima-patterns)
		(combine-path *maxima-sharedir* share-subdirs maxima-patterns)
        (combine-path *maxima-topdir* maxima-patterns)))
    (setq $file_search_demo
	  (list '(mlist)
		(combine-path *maxima-sharedir* demo-patterns)
		(combine-path *maxima-sharedir* share-subdirs demo-patterns)
		(combine-path *maxima-demodir* demo-patterns)))
    (setq $file_search_usage
	  (list '(mlist)
		(combine-path *maxima-sharedir* usage-patterns)
		(combine-path *maxima-sharedir* share-subdirs usage-patterns)
		(combine-path *maxima-docdir* usage-patterns)))
    (setq $file_search_tests
	  `((mlist) ,(combine-path *maxima-testsdir* lisp+maxima-patterns)))

    ;; If *maxima-lang-subdir* is not nil test whether corresponding info directory
    ;; with some data really exists.  If not this probably means that required
    ;; language pack wasn't installed and we reset *maxima-lang-subdir* to nil.
    (when (and *maxima-lang-subdir*
	       (not (probe-file (combine-path *maxima-infodir* *maxima-lang-subdir* "maxima-index.lisp"))))
       (setq *maxima-lang-subdir* nil))))

(defun get-dirs (path &aux (ns (namestring path)))
  (directory (concatenate 'string
                          ns
                          (if (eql #\/ (char ns (1- (length ns)))) "" "/")
                          "*"
                          #+(or :clisp :sbcl :ecl :openmcl :gcl) "/")
             #+openmcl :directories #+openmcl t))

(defun unix-like-basename (path)
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (subseq pathstring (1+ (or (position #\/ pathstring :from-end t)
			       (position #\\ pathstring :from-end t))) len)))

(defun unix-like-dirname (path)
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (subseq pathstring 0 (or (position #\/ pathstring :from-end t)
			     (position #\\ pathstring :from-end t)))))

(defun list-avail-action ()
  (let* ((maxima-verpkglibdir (if (maxima-getenv "MAXIMA-VERPKGLIBDIR")
				  (maxima-getenv "MAXIMA-VERPKGLIBDIR")
				  (if (maxima-getenv "MAXIMA_PREFIX")
				      (combine-path (maxima-getenv "MAXIMA_PREFIX") "lib"
						    *autoconf-package* *autoconf-version*)
				      (combine-path (maxima-parse-dirstring *autoconf-libdir*)
						    *autoconf-package* *autoconf-version*))))
	 (len (length maxima-verpkglibdir))
	 (lisp-string nil))
    (format t "Available versions:~%")
    (unless (equal (subseq maxima-verpkglibdir (- len 1) len) "/")
      (setf maxima-verpkglibdir (concatenate 'string maxima-verpkglibdir "/")))
    (dolist (version (get-dirs (unix-like-dirname maxima-verpkglibdir)))
      (dolist (lisp (get-dirs version))
	(setf lisp-string (unix-like-basename lisp))
	(when (search "binary-" lisp-string)
	  (setf lisp-string (subseq lisp-string (length "binary-") (length lisp-string)))
	  (format t "version ~a, lisp ~a~%" (unix-like-basename version) lisp-string))))
    (bye)))

(defun process-maxima-args (input-stream batch-flag)
  ;;    (format t "processing maxima args = ")
  ;;    (mapc #'(lambda (x) (format t "\"~a\"~%" x)) (get-application-args))
  ;;    (terpri)
  ;;    (finish-output)
  (let ((maxima-options nil))
    ;; Note: The current option parsing code expects every short
    ;; option to have an equivalent long option.  No check is made for
    ;; this, so please make sure this holds.  Or change the code in
    ;; process-args in command-line.lisp.
    ;;
    ;; The help strings should not have any special manual formatting
    ;; but extraneous white space is ok.  They are automatically
    ;; printed with extraneous whitespace (including newlines) removed
    ;; and lines wrapped neatly.
    (setf maxima-options
	  (list
	   (make-cl-option :names '("-b" "--batch")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf input-stream
					     (make-string-input-stream
					      (format nil "batch(\"~a\");"
						      file)))
				       (setf batch-flag :batch))
			   :help-string
			   "Process maxima file <file> in batch mode.")
	   (make-cl-option :names '("--batch-lisp")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf input-stream
					     (make-string-input-stream
					      #-sbcl (format nil ":lisp (load \"~a\");" file)
					      #+sbcl (format nil ":lisp (with-compilation-unit nil (load \"~a\"));" file)))
				       (setf batch-flag :batch))
			   :help-string
			   "Process lisp file <file> in batch mode.")
	   (make-cl-option :names '("--batch-string")
			   :argument "<string>"
			   :action #'(lambda (string)
				       (setf input-stream
					     (make-string-input-stream string))
				       (setf batch-flag :batch))
			   :help-string
			   "Process maxima command(s) <string> in batch mode.")
	   (make-cl-option :names '("-d" "--directories")
			   :action #'(lambda () (print-directories) ($quit))
			   :help-string
			   "Display maxima internal directory information.")
	   (make-cl-option :names '("--disable-readline")
			   :action #'(lambda ()
				       #+gcl
				       (if (find :readline *features*)
					   (si::readline-off)))
			   :help-string "Disable readline support.")
	   (make-cl-option :names '("-g" "--enable-lisp-debugger")
			   :action #'(lambda ()
				       (setf *debugger-hook* nil))
			   :help-string
			   "Enable underlying lisp debugger.")
	   (make-cl-option :names '("-h" "--help")
			   :action #'(lambda ()
				       (format t "usage: maxima [options]~%")
				       (list-cl-options maxima-options)
				       (bye))
			   :help-string "Display this usage message.")
	   (make-cl-option :names '("--userdir")
			   :argument "<directory>"
			   :action nil
			   :help-string "Use  <directory> for user directory (default is %USERPROFILE%/maxima for Windows, and $HOME/.maxima for other operating systems).")
 	   (make-cl-option :names '("--init")
			   :argument "<file>"
			   :action
			   #'(lambda (file)
			       (flet
				   ((get-base-name (f)
				      ;; Strip off everything before
				      ;; the last "/" (or "\").  Then
				      ;; strip off everything after
				      ;; the last dot.
				      (let* ((dot (position #\. f :from-end t))
					     (dir (position-if
						   #'(lambda (c)
						       (member c '(#\/ #\\)))
						   f
						   :from-end t))
					     (base (subseq f (if dir (1+ dir) 0) dot)))
					(when (or dot dir)
					  (mtell (intl:gettext "Warning: Using basename ~S for init files instead of ~S" )
						 base f))
					base)))
				 (let ((base-name (get-base-name file)))
				   (setf *maxima-initmac*
					 (concatenate 'string base-name ".mac"))
				   (setf *maxima-initlisp*
					 (concatenate 'string base-name ".lisp")))))
			   :help-string (format nil "Set the base name of the Maxima & Lisp initialization files (default is ~s.)  The last extension and any directory parts are removed to form the base name.  The resulting files, <base>.mac and <base>.lisp are only searched for in userdir (see --userdir option).  This may be specified for than once, but only the last is used."
						(subseq *maxima-initmac* 0
							(- (length *maxima-initmac*) 4))))
 	   #+nil
	   (make-cl-option :names '("--init-mac")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf *maxima-initmac* file))
			   :help-string (format nil "Set the name of the Maxima initialization file (default is ~s)"
						*default-maxima-initmac*))
 	   #+nil
	   (make-cl-option :names '("--init-lisp")
			   :argument "<file>"
			   :action #'(lambda (file)
				       (setf *maxima-initlisp* file))
			   :help-string (format nil "Set the name of the Lisp initialization file (default is ~s)" *default-maxima-initlisp*))
	   (make-cl-option :names '("-l" "--lisp")
			   :argument "<lisp>"
			   :action nil
			   :help-string "Use lisp implementation <lisp>.")
	   (make-cl-option :names '("--list-avail")
			   :action 'list-avail-action
			   :help-string
			   "List the installed version/lisp combinations.")
	   ;; --preload-lisp is left for backward compatibility.  We
	   ;; no longer distinguish between mac and lisp files.  Any
	   ;; file type that $LOAD supports is acceptable.
	   ;; "--init-mac" and "--init-lisp" are now also (deprecated)
	   ;; aliases for --preload.
	   (make-cl-option :names '("-p" "--preload" "--preload-lisp" "--init-mac" "--init-lisp")
			   :argument "<file>"
			   :action #'(lambda (file)
				       ;; $loadprint T so we can see the file being loaded.
				       (let (($loadprint t))
				       ($load file)))
			   :help-string
        "Preload <file>, which may be any file time accepted by
        Maxima's LOAD function.  The <file> is loaded before any other
        system initialization is done.  This will be searched for in
        the locations given by file_search_maxima and
        file_search_lisp.  This can be specified multiple times to
        load multiple files. The equivalent options --preload-lisp,
        --init-mac, and --init-lisp are deprecated.")
	   (make-cl-option :names '("-q" "--quiet")
			   :action #'(lambda ()
				       (declare (special *maxima-quiet*))
				       (setq *maxima-quiet* t))
			   :help-string "Suppress Maxima start-up message.")
	   (make-cl-option :names '("-r" "--run-string")
			   :argument "<string>"
			   :action #'(lambda (string)
				       (declare (special *maxima-run-string*))
				       (setq *maxima-run-string* t)
				       (setf input-stream
					     (make-string-input-stream string))
				       (setf batch-flag nil))
			   :help-string
			   "Process maxima command(s) <string> in interactive mode.")
	   (make-cl-option :names '("-s" "--server")
			   :argument "<port>"
			   :action #'(lambda (port-string)
				       (start-client (parse-integer
						      port-string))
                                       (setf input-stream *standard-input*))
			   :help-string "Connect Maxima to server on <port>.")
	   (make-cl-option :names '("-u" "--use-version")
			   :argument "<version>"
			   :action nil
			   :help-string "Use maxima version <version>.")
	   (make-cl-option :names '("-v" "--verbose")
			   :action nil
			   :help-string
			   "Display lisp invocation in maxima wrapper script.")
	   (make-cl-option :names '("--version")
			   :action #'(lambda ()
				       (format t "Maxima ~a~%"
					       *autoconf-version*)
				       ($quit))
			   :help-string
			   "Display the default installed version.")
	   (make-cl-option :names '("--very-quiet")
			   :action #'(lambda ()
				       (declare (special *maxima-quiet*))
				       (setq *maxima-quiet* t *display-labels-p* nil))
			   :help-string "Suppress expression labels and Maxima start-up message.")
	   (make-cl-option :names '("-X" "--lisp-options")
			   :argument "<Lisp options>"
			   :action #'(lambda (&rest opts)
				       (declare (special *maxima-quiet*))
				       (unless *maxima-quiet*
					 (format t "Lisp options: ~A" opts)))
			   :help-string "Options to be given to the underlying Lisp")
	   (make-cl-option :names '("--no-init" "--norc")
			   :action #'(lambda ()
				       (setf *maxima-load-init-files* nil))
			   :help-string "Do not load the init file(s) on startup")
			   ))
    (process-args (get-application-args) maxima-options))
  (values input-stream batch-flag))

;; Delete all files *temp-files-list* contains.
(defun delete-temp-files ()
  (maphash #'(lambda(filename param)
	       (declare (ignore param))
	       (let ((file (ignore-errors (probe-file filename))))
		 (if file
		     (if (not (apparently-a-directory-p file))
			 (delete-file file)))))
	   *temp-files-list*))

(defun cl-user::run ()
  "Run Maxima in its own package."
  (in-package :maxima)
  (initialize-runtime-globals)
  (let ((input-stream *standard-input*)
	(batch-flag nil))
    (unwind-protect
	(catch 'to-lisp
	  (setf (values input-stream batch-flag)
		(process-maxima-args input-stream batch-flag))
	  (load-user-init-file)
	  (loop
	   (with-simple-restart (macsyma-quit "Maxima top-level")
				(macsyma-top-level input-stream batch-flag))))
      (delete-temp-files)
    )))

;; If the user specified an init file, use it.  If not, use the
;; default init file in the userdir directory, but only if it
;; exists.  A user-specified init file is searched in the search
;; paths.

(defun load-user-init-file ()
    (flet
	((maybe-load-init-file (loader default-init)
	   (let ((init-file
		   (combine-path *maxima-userdir* default-init)))
	     (when (and *maxima-load-init-files*
			(file-exists-p init-file))
	       (format t "Loading ~A~%" init-file)
	       (funcall loader init-file)))))
      ;; Catch errors from $load or $batchload which can throw to 'macsyma-quit.
      (catch 'macsyma-quit
	(maybe-load-init-file #'$load *maxima-initlisp*)
	(maybe-load-init-file #'$batchload *maxima-initmac*))))

(defun initialize-runtime-globals ()
  (setf *load-verbose* nil)

  (disable-some-lisp-warnings)

  (setf *debugger-hook* #'maxima-lisp-debugger)
  ;; See discussion on the maxima list
  ;; http://www.math.utexas.edu/pipermail/maxima/2011/024014.html.
  ;; Set *print-length* and *print-level* to some reasonable values so
  ;; that normal Lisp structure is shown, but prevent typical circular
  ;; structures from hanging Lisp.
  ;;
  ;; (We do we set these instead of binding them?)
  (setf *print-circle* nil)
  (setf *print-length* 100)
  (setf *print-level* 15)
  
  ;; GCL: print special floats, which are generated whether or not this flag is enabled
  #+gcl (setf si:*print-nans* t)
  #+ccl
  (progn
    (setf ccl::*invoke-debugger-hook-on-interrupt* t)
    ;; CCL 1.5 makes *read-default-float-format* a thread-local
    ;; variable.  Hence we need to set it here to get our desired
    ;; behavior.
    (setf *read-default-float-format* 'double-float))

  #+allegro
  (progn
    (set-readtable-for-macsyma)
    (setf *read-default-float-format* 'lisp::double-float))

  #+sbcl (setf *read-default-float-format* 'double-float)

  ;; GCL: disable readline symbol completion,
  ;; leaving other functionality (line editing, anything else?) enabled.
  ;;
  ;; This is kind of terrible. I don't see a flag to only disable completion,
  ;; or a way to set the symbol list to Maxima symbols and disable case inversion,
  ;; so set the completion prefix to a nonexistent package.
  ;; If ever package BLURFLE is actually defined, and contains external symbols,
  ;; those symbols will be completed. I can live with that.
  
  #+gcl (setq si::*readline-prefix* "BLURFLE:")

  (initialize-real-and-run-time)
  (intl::setlocale)
  (set-locale-subdir)
  (adjust-character-encoding)
  (set-pathnames)
  (catch 'return-from-debugger
    (cl-info::load-primary-index))
  (when (boundp '*maxima-prefix*)
    (push (pathname (concatenate 'string *maxima-prefix*
                                 (if *maxima-layout-autotools*
                                     "/share/locale/"
                                     "/locale/")))
          intl::*locale-directories*))
  ;; Set up $browser for displaying help in browser.
  (cond ((and (boundp '*autoconf-windows*)
	      (string-equal *autoconf-windows* "true"))
	 ;; Starts the default browser on Windows.
	 (setf $browser "start ~A"))
	((boundp '*autoconf-host*)
	 ;; Determine what kind of OS we're using from the host and
	 ;; set up the default browser appropriately.
	 (cond ((pregexp:pregexp-match-positions "(?:darwin)" *autoconf-host*)
		(setf $browser "open '~A'"))
	       ((pregexp:pregexp-match-positions "(?i:linux)" *autoconf-host*)
		(setf $browser "xdg-open '~A'")))))
  (setf %e-val (mget '$%e '$numer))

  ;; Initialize *bigprimes* here instead of globals.lisp because we
  ;; need the NEXT-PRIME function.
  (setf *bigprimes*
	(loop with p = (ash most-positive-fixnum -1)
	      repeat 20
	      do (setq p (next-prime (1- p) -1))
	      collect p))
  ;; Initialize *alpha and $pointbound.  Since both of these are
  ;; defmvars, we need to set the initial values appropriately too so
  ;; they get reset correctly.
  (setf *alpha (car *bigprimes*))
  (setf (gethash '*alpha *variable-initial-values*)
	(car *bigprimes*))
  (setf $pointbound *alpha)
  (setf (gethash '$pointbound *variable-initial-values*)
	*alpha)
  (values))

(defun adjust-character-encoding ()
  #+sbcl (setf sb-impl::*default-external-format* :utf-8)
  #+cmu
  (handler-bind ((error #'(lambda (c)
			    ;; If there's a continue restart, restart
			    ;; to set the filename encoding anyway.
			    (if (find-restart 'cl:continue c)
				(invoke-restart 'cl:continue)))))
    ;; Set both the terminal external format and filename encoding to
    ;; utf-8.  The handler-bind is needed in case the filename
    ;; encoding was already set to something else; we forcibly change
    ;; it to utf-8. (Is that right?)
    (setf stream:*default-external-format* :utf-8)
    (stream:set-system-external-format :utf-8 :utf-8))
  #+clisp
  (ignore-errors
    (progn (setf custom:*default-file-encoding*
		 (ext:make-encoding :input-error-action #\?))
	   (setf custom:*terminal-encoding*
		 custom:*default-file-encoding*))))

(import 'cl-user::run)

(defmfun $to_lisp ()
  (format t "~&Type (to-maxima) to restart, ($quit) to quit Maxima.~%")
  (let ((old-debugger-hook *debugger-hook*))
    (catch 'to-maxima
      (unwind-protect
	   (maxima-read-eval-print-loop)
	(setf *debugger-hook* old-debugger-hook)
	(format t "Returning to Maxima~%")))))

(defun to-maxima ()
  (throw 'to-maxima t))

(defun maxima-read-eval-print-loop ()
  (when *debugger-hook*
    ; Only set a new debugger hook if *DEBUGGER-HOOK* has not been set to NIL
    (setf *debugger-hook* #'maxima-lisp-debugger-repl))
  (let ((eof (gensym)))
    (loop
      (catch 'to-maxima-repl
        (format-prompt t "~%~A> " (package-name *package*))
        (finish-output)
        (let ((input (read *standard-input* nil eof)))
          ; Return to Maxima on EOF
          (when (eq input eof)
            (fresh-line)
            (to-maxima))
          (format t "~{~&~S~}" (multiple-value-list (eval input))))))))

(defun maxima-lisp-debugger-repl (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Maxima encountered a Lisp error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%To reenable the Lisp debugger set *debugger-hook* to nil.~%")
  (finish-output)
  (throw 'to-maxima-repl t))

(defvar $help "type `describe(topic);' or `example(topic);' or `? topic'")

(defmfun $help (&rest dummy)
  (declare (ignore dummy))
  $help)

(eval-when (:load-toplevel :execute)
    (let ((context '$global))
      (declare (special context))
      (dolist (x '($%pi $%i $%e $%phi %i $%gamma  ;numeric constants
                   $inf $minf $und $ind $infinity ;pseudo-constants
                   t nil))                        ;logical constants (Maxima names: true, false)
	(kind x '$constant)
	(setf (get x 'sysconst) t))))

;;; Now that all of maxima has been loaded, define the various lists
;;; and hashtables of builtin symbols and values.

;;; The assume database structures for numeric constants such as $%pi and $%e
;;; are circular.  Attempting to copy a circular structure
;;; into *builtin-symbol-props* would cause a hang.  Therefore
;;; the properties are copied into *builtin-symbol-props* before
;;; initializing the assume database.
(let ((maxima-package (find-package :maxima)))
  (do-symbols (s maxima-package)
    (when (and (eql (symbol-package s) maxima-package)
	       (not (eq s '||))
	       (member (char (symbol-name s) 0) '(#\$ #\%) :test #'char=))
      (push s *builtin-symbols*)
      (setf (gethash s *builtin-symbol-props*)
	    (copy-tree (symbol-plist s))))))

;; Also store the property lists for symbols associated with operators;
;; e.g. MPLUS, MTIMES, etc.
;; Here we find them via the MHEADER property, which is used by the parser.
;; I don't know any better way to find these properties.

(let ((maxima-package (find-package :maxima)))
  (do-symbols (s maxima-package)
    (let ((h (get s 'mheader)))
      (when h
        (let ((s1 (first h)))
          (unless (gethash s1 *builtin-symbol-props*)
            (push s1 *builtin-symbols*)
            (setf (gethash s1 *builtin-symbol-props*)
                  (copy-tree (symbol-plist s1)))))))))

;; Initialize assume database for $%pi, $%e, etc
(dolist (c *builtin-numeric-constants*)
  (initialize-numeric-constant c))

(dolist (s *builtin-symbols*)
  (when (boundp s)
    (push s *builtin-symbols-with-values*)))

(dolist (s *builtin-symbols-with-values*)
  (setf (gethash s *builtin-symbol-values*) (symbol-value s)))

(setf *builtin-$props* (copy-list $props))
(setf *builtin-$rules* (copy-list $rules))

(defun maxima-objdir (&rest subdirs)
  "Return a pathname string such that subdirs is a subdirectory of maxima_objdir"
  (apply #'combine-path *maxima-objdir* subdirs))

(defun maxima-load-pathname-directory ()
  "Return the directory part of *load-pathname*."
  (let ((path *load-pathname*))
    (make-pathname :directory (pathname-directory path)
                   :device (pathname-device path))))
