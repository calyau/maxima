;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module transs)

(transl-module transs)

(defmvar *transl-file-debug* nil
  "set this to T if you don't want to have the temporary files
	used automaticaly deleted in case of errors.")

;;; User-hacking code, file-io, translator toplevel.
;;; There are various macros to cons-up filename TEMPLATES
;;; which to mergef into. The filenames should be the only
;;; system dependant part of the code, although certain behavior
;;; of RENAMEF/MERGEF/DELETE-FILE is assumed.

(defmvar $tr_output_file_default '$trlisp
  "This is the second file name to be used for translated lisp
	 output.")

(defmvar $tr_file_tty_messagesp nil
  "It TRUE messages about translation of the file are sent
	 to the TTY also.")

(defmvar $tr_windy t
  "Generate helpful comments and programming hints.")

(deftrvar *translation-msgs-files* nil
  "Where the warning and other comments goes.")

(deftrvar $tr_version (get 'transl-autoload 'version))

(defmvar transl-file nil "output stream of $compfile and $translate_file")

(defmvar $compgrind nil "If `true' lisp output will be pretty-printed.")

(defmvar $tr_true_name_of_file_being_translated nil
  "This is set by TRANSLATE_FILE for use by user macros
	 which want to know the name of the source file.")

(defmvar $tr_state_vars
    '((mlist) $transcompile $tr_semicompile
      $translate_fast_arrays
      $tr_warn_undeclared
      $tr_warn_meval
      $tr_warn_fexpr
      $tr_warn_mode
      $tr_warn_undefined_variable
      $tr_function_call_default
      $tr_array_as_ref
      $tr_numer
      $define_variable))

(defmacro compfile-outputname-temp ()
  '`,(pathname "_cmf_"))

(defmacro compfile-outputname ()
  '`,(pathname (maybe-invert-string-case (symbol-name (stripdollar $tr_output_file_default)))))

(defmacro trlisp-inputname-d1 () ;; so hacks on DEFAULTF will not
  '`,(pathname "")) ;; stray the target.

(defmacro trlisp-outputname-d1 ()
  '`,(pathname (maybe-invert-string-case (symbol-name (stripdollar $tr_output_file_default)))))

(defmacro trlisp-outputname ()
  '`,(make-pathname :type "LISP"))

(defmacro trlisp-outputname-temp ()
  '`,(pathname "_trli_"))

(defmacro trtags-outputname ()
  '`,(pathname "tags"))

(defmacro trtags-outputname-temp ()
  '`,(pathname "_tags_"))

(defmacro trcomments-outputname ()
  '`,(pathname "unlisp"))

(defmacro trcomments-outputname-temp ()
  '`,(pathname "_unli_"))

(deftrvar declares nil)

(defun rename-tf (newname)
  (let ((in-file (truename transl-file)))
    (close transl-file)
    (rename-file in-file (maxima-string newname))))

(defmspec $compfile (forms)
  (let ((newname (second forms)))
    (setq forms (cdr forms))
    (bind-transl-state
     (setq $transcompile t
	   *in-compfile* t)
     (let ((out-file-name (cond ((mfilename-onlyp (car forms))
				 ($filename_merge (pop forms)))
				(t "")))
	   (t-error nil)
	   (*translation-msgs-files* nil))
       (setq out-file-name (mergef out-file-name (compfile-outputname)))
       (unwind-protect
	    (progn
	      (setq transl-file (open-out-dsk (mergef (compfile-outputname-temp)
						      out-file-name)))

	      (cond ((or (member '$all forms :test #'eq)
			 (member '$functions forms :test #'eq))
		     (setq forms (mapcar #'caar (cdr $functions)))))
	      (do ((l forms (cdr l))
		   (declares nil nil)
		   (tr-abort nil nil)
		   (item)
		   (lexprs nil nil)
		   (fexprs nil nil)
		   (t-item))		;
		  ((null l))
		(setq item (car l))
		(cond ((not (atom item))
		       (print* (dconvx (translate item))))
		      (t
		       (setq t-item (compile-function (setq item ($verbify item))))
		       (cond (tr-abort
			      (setq t-error (print-abort-msg item 'compfile)))
			     (t
			      (cond ($compgrind
				     (mformat transl-file "~2%;; Function ~:@M~%" item)))
			      (print* t-item))))))
	      (setq out-file-name (rename-tf newname))
	      (to-macsyma-namestring out-file-name))
	 ;; unwind-protected
	 (if transl-file (close transl-file))
	 (if t-error (delete-file transl-file)))))))

(defun compile-function (f)
  (mformat  *translation-msgs-files* "~%Translating ~:@M" f)
  (let ((fun (tr-mfun f)))
    (cond (tr-abort  nil)
	  (t fun))))

(defvar tr-defaultf nil
  "A default only for the case of no arguments to $translate_file")

;;; Temporary hack during debugging of this  code.

(defun mergef (x y)
  (merge-pathnames y x))

(defun $compile_file (input-file &optional bin-file translation-output-file &aux result)
  (setq input-file (maxima-string input-file))
  (and bin-file(setq  bin-file (maxima-string bin-file)))
  (and translation-output-file
       (setq translation-output-file (maxima-string translation-output-file)))
  (cond ((string-equal (pathname-type input-file) "LISP")
	 (setq result (list '(mlist) input-file)))
	(t (setq result (translate-file input-file translation-output-file))
	   (setq input-file (third result))))
  #+(or cmu scl sbcl clisp allegro openmcl)
  (multiple-value-bind (output-truename warnings-p failure-p)
      (if bin-file
	  (compile-file input-file :output-file bin-file)
	  (compile-file input-file))
    (declare (ignore warnings-p))
    ;; If the compiler encountered errors, don't set bin-file to
    ;; indicate that we found errors. Is this what we want?
    (unless failure-p
      (setq bin-file output-truename)))
  #-(or cmu scl sbcl clisp allegro openmcl)
  (setq bin-file (compile-file input-file :output-file bin-file))
  (append result (list bin-file)))

;; Converts a Maxima "string" (which is really a symbol that starts
;; with the character '&') to a Lisp string.
(defun maxima-string (symb)
  (string-left-trim "&" (print-invert-case symb)))

(defmfun $translate_file (input-file &optional output-file)
  (setq input-file (maxima-string input-file))
  (cond (output-file (setq output-file (maxima-string output-file))))
  (translate-file input-file output-file))

(defmvar $tr_gen_tags nil
  "If TRUE, TRANSLATE_FILE generates a TAGS file for use by the text editor")

(defvar *pretty-print-translation* t)

;; Define a pprinter for defmtrfun.

#-gcl
(defun pprint-defmtrfun (stream s)
  (pprint-logical-block (stream s :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (write-char #\space stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 4 stream)
    (pprint-newline :mandatory stream)
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 2 stream)
    (pprint-newline :mandatory stream)
    (loop
       (pprint-exit-if-list-exhausted)
       (write (pprint-pop) :stream stream)
       (write-char #\space stream)
       (pprint-newline :linear stream))))

(defun call-batch1 (in-stream out-stream &aux expr transl)
  (cleanup)
  ;; we want the thing to start with a newline..
  (newline in-stream #\n)
  (let ((*readtable* (copy-readtable nil))
	#-gcl (*print-pprint-dispatch* (copy-pprint-dispatch)))
    #-gcl
    (progn
      #-(or scl allegro)
      (setf (readtable-case *readtable*) :invert)
      #+(or scl allegro)
      (unless #+scl (eq ext:*case-mode* :lower)
	      #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
	(setf (readtable-case *readtable*) :invert))
      (set-pprint-dispatch '(cons (member maxima::defmtrfun))
			   #'pprint-defmtrfun))
    (loop while (and (setq expr (mread in-stream)) (consp expr))
	  do (setq transl (translate-macexpr-toplevel (third expr)))
	     (cond
	       (*pretty-print-translation*
		(pprint transl out-stream))
	       (t
		(format out-stream "~a" transl))))))


(defun translate-from-stream (from-stream &key to-stream eval pretty (print-function #'prin1)
			      &aux expr transl )
  (bind-transl-state
   (loop while (and (setq expr (mread from-stream)) (consp expr))
      with *in-translate-file* = t
      with *print-pretty* = pretty
      do (setq transl (translate-macexpr-toplevel (third expr)))
      (cond (eval (eval transl)))
      (cond (to-stream (funcall print-function transl to-stream)))
      (loop for v in forms-to-compile-queue
	 do (show v to-stream)
	 when to-stream
	 do (funcall print-function v to-stream)
	 when eval
	 do (eval v))
      (setq forms-to-compile-queue nil))))

(defvar trf-start-hook nil)

(defun delete-old-and-open (x)
  (open x :direction :output))

(defun alter-pathname (pathname &rest options)
  (apply 'make-pathname :defaults (pathname  pathname) options))

(defun delete-with-side-effects-if (test list)
  "Rudimentary DELETE-IF which, however, is guaranteed to call
the function TEST exactly once for each element of LIST, from
left to right."
  (loop while (and list (funcall test (car list)))
     do (pop list))
  (loop with list = list
     while (cdr list)
     if (funcall test (cadr list))
     do (pop (cdr list))
     else
     do (pop list))
  list)

(defun insert-necessary-function-declares (stream)
  "Write to STREAM two lists: The functions which are known to be
translated without actually being in the list passed to
$DECLARE_TRANSLATED, and those which are not known to be
translated."
  (let (translated hint)
    (setq *untranslated-functions-called*
	  (delete-with-side-effects-if
	   #'(lambda (v)
	       (prog1
		   (or (setq translated
			     (or (get v 'once-translated)
				 (get v 'translated)))
		       (and (fboundp v)
			    ;; might be traced
			    (not (mget v 'mexpr)))
		       (get v 'mfexpr*))
		 (when (and translated
			    (not (member v *declared-translated-functions* :test #'eq)))
		   (push v hint))))
	   *untranslated-functions-called*))
    (when hint
      (format  stream
	       "~2%/* The compiler might be able to optimize some function calls
   if you prepend the following declaration to your maxima code: */~%")
      (mgrind `(($eval_when) $translate (($declare_translated) ,@hint))
	      stream)
      (format stream "$"))
    (when *untranslated-functions-called*
      (format stream "~2%/* The following functions are not known to be translated.~%")
      (mgrind `((mlist) ,@(nreverse *untranslated-functions-called*)) stream)
      (format stream "$ */"))
    (fresh-line stream)
    (when (or hint *untranslated-functions-called*)
      (format t "~&See the `unlisp' file for possible optimizations.~%"))))

(defun translate-file (in-file-name out-file-name &optional (ttymsgsp $tr_file_tty_messagesp)
		       &aux warn-file translated-file *translation-msgs-files*
		       *untranslated-functions-called* *declared-translated-functions*)
  (bind-transl-state
   (setq *in-translate-file* t)
   (setq translated-file (alter-pathname (or out-file-name in-file-name) :type "LISP"))
   (setq warn-file (alter-pathname in-file-name :type "UNLISP"))
   (with-open-file (in-stream in-file-name)
     (with-open-file (out-stream translated-file :direction :output :if-exists :supersede)
       (with-open-file (warn-stream warn-file :direction :output :if-exists :supersede)
	 (setq *translation-msgs-files* (list warn-stream))
	 (if ttymsgsp
	     (setq *translation-msgs-files* (cons *standard-output* *translation-msgs-files*)))
	 (format out-stream ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp ;Base: 10 -*- ;;;~%")
	 (flet ((timezone-iso8601-name (dst tz)
		  ;; This function was borrowed from CMUCL.
		  (let ((tz (- tz)))
		    (if (and (not dst) (= tz 0))
			"Z"
			(multiple-value-bind (hours minutes)
			    (truncate (if dst (1+ tz) tz))
			  (format nil "~C~2,'0D:~2,'0D"
				  (if (minusp tz) #\- #\+)
				  (abs hours)
				  (abs (truncate (* minutes 60)))))))))
	   (multiple-value-bind (secs mins hours day month year dow dst tz)
	       (decode-universal-time (get-universal-time))
	     (declare (ignore dow))
	     (format out-stream ";;; Translated on: ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~A~%"
		     year month day hours mins secs (timezone-iso8601-name dst tz))))
	 (format out-stream ";;; Maxima System version: ~A~%" *autoconf-version*)
	 (format out-stream ";;; Lisp type:    ~A~%" (lisp-implementation-type))
	 (format out-stream ";;; Lisp version: ~A~%" (lisp-implementation-version))
	 (format out-stream "~%(in-package :maxima)")
	 (format warn-stream "~%This is the unlisp file for ~A "
		 (namestring (pathname in-stream)))
	 (mformat out-stream
		  "~%;;** Variable settings were **~%~%")
	 (loop for v in (cdr $tr_state_vars)
		do (mformat out-stream   ";; ~:M: ~:M;~%" v (symbol-value v)))
	 (mformat *terminal-io* "~%Translation begun on ~A.~%"
		  (pathname in-stream))
	 (call-batch1 in-stream out-stream)
	 (insert-necessary-function-declares warn-stream)
	 ;; BATCH1 calls TRANSLATE-MACEXPR-toplevel on each expression read.
	 (cons '(mlist)
	       (mapcar 'namestring
		       (mapcar 'pathname (list in-stream out-stream warn-stream)))))))))

;; Should be rewritten to use streams.  Barf -- perhaps SPRINTER
;; doesn't take a stream argument?
;; Yes Carl SPRINTER is old i/o, but KMP is writing a new one for NIL. -GJC

(defun print* (p)
  (let ((^w t)
	(outfiles (list transl-file))
	(^r t)
	($loadprint nil)) ;;; lusing old I/O !!!!!
    (declare (special outfiles))
    (sub-print* p)))

;;; i might as well be real pretty and flatten out PROGN's.

(defun sub-print* (p &aux (flag nil))
  (cond ((atom p))
	((and (eq (car p) 'progn) (cdr p) (equal (cadr p) ''compile))
	 (mapc #'sub-print* (cddr p)))
	(t
	 (setq flag (and $tr_semicompile
			 (not (eq (car p) 'eval-when))))
	 (when flag (princ* '|(|) (princ* 'progn) (terpri*))
	 (cond ($compgrind
		(sprin1 p))
	       (t
		(prin1 p transl-file)))
	 (when flag (princ* '|)|))
	 (terpri transl-file))))

(defun princ* (form)
  (princ form transl-file))

(defun nprinc* (&rest form)
  (mapc #'(lambda (x) (princ x transl-file)) form))

(defun terpri* ()
  (terpri transl-file))

(defun print-module (m)
  (nprinc* " " m " version " (get m 'version)))

(defun new-comment-line ()
  (terpri*)
  (princ* ";;;"))

(defun print-transl-modules ()
  (new-comment-line)
  (print-module 'transl-autoload)
  (do ((j 0 (1+ j))
       (s (delete 'transl-autoload (copy-list transl-modules) :test #'equal)
	  (cdr s)))
      ((null s))
    (if (= 0 (rem j 3)) (new-comment-line))
    (print-module (car s))))


(defun print-transl-header (source)
  (mformat transl-file ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp -*-~%")
  (if source
      (mformat transl-file ";;; Translated code for ~A" source)
      (mformat transl-file ";;; Translated Maxima functions generated by `compfile'."))
  (mformat transl-file "~%;;; Written on ~:M, from Maxima ~A~%;;;~%" ($timedate) $version)
  (print-transl-modules)
  (mformat transl-file
	   "~%~
	   ~%~
	   ~%(eval-when (compile eval) ~
	   ~%      (setq *infile-name-key*~
	   ~%               ((lambda (file-name)~
	   ~%                           ;; temp crock for multics.~
	   ~%                          (cond ((eq (ml-typep file-name) 'list)~
	   ~%                                 (namestring file-name))~
	   ~%                                (t file-name)))~
	   ~%                  (truename infile))))~
	   ~%~
	   ~%(eval-when (compile) ~
	   ~%   (setq $tr_semicompile '~S)~
	   ~%   (setq forms-to-compile-queue ()))~
	   ~%~%;;; ~S~%~%"
	   $tr_semicompile source)
  (cond ($transcompile
	 (update-global-declares)
	 (if $compgrind
	     (mformat
	      transl-file
	      ";;; General declarations required for translated Maxima code.~%"))
	 (print* `(declare . ,declares)))))

(defun print-abort-msg (fun from)
  (mformat *translation-msgs-files*
	   "~:@M failed to Translate.~
	    ~%~A will continue, but file output will be aborted."
	   fun from))

(defmacro extension-filename (x)
  `(caddr (namelist ,x)))

(defmspec $translate (functs)
  (setq functs (cdr functs))
  (cond ((and functs ($listp (car functs)))
	 (merror "Use the function `translate_file'"))
	(t
	 (cond ((or (member '$functions functs :test #'eq)
		    (member '$all functs :test #'eq))
		(setq functs (mapcar 'caar (cdr $functions)))))
	 (do ((l functs (cdr l))
	      (v nil))
	     ((null l) `((mlist) ,@(nreverse v)))
	   (cond ((atom (car l))
		  (let ((it (translate-function ($verbify (car l)))))
		    (if it (push it v))))
		 (t
		  (tr-tell
		   (car l)
		   " is an illegal argument to `translate'.")))))))


(declare-top (special forms-to-compile-queue))

(defmspec $compile (form)
  (let ((l (meval `(($translate),@(cdr form)))))
    (let ((forms-to-compile-queue ()))
      (mapc #'(lambda (x) (if (fboundp x) (compile x))) (cdr l))
      (do ()
	  ((null forms-to-compile-queue) l)
	(mapc #'(lambda (form)
		  (eval form)
		  (and (consp form)
		       (eq (car form) 'defun)
		       (symbolp (cadr form))
		       (compile (cadr form))))
	      (prog1 forms-to-compile-queue
		(setq forms-to-compile-queue nil)))))))
