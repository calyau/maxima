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

;;(defun set-up-translate ()
;;  (load '|<macsym>transl.autolo|)
;;  (load '|<macsym>trdata.fasl|)
;;  (load '|<maxout>dcl.fasl|)
;;  (load '|<macsym>transl.fasl|)
;;  (load '|<macsym>trans1.fasl|)
;;  (load '|<macsym>troper.fasl|)
;;  (load '|<macsym>trutil.fasl|)
;;  (load '|<macsym>trans2.fasl|))

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
  '`,(pathname (stripdollar $tr_output_file_default)))

(defmacro trlisp-inputname-d1 () ;; so hacks on DEFAULTF will not
  '`,(pathname "")) ;; stray the target.

(defmacro trlisp-outputname-d1 ()
  '`,(pathname (stripdollar $tr_output_file_default))) 

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

;;;these first five functions have been altered to run on 
;;;the 3600 we must try to fix translate-file  wfs fixed -wfs

(defun rename-tf (new-name true-in-file-name &optional newname)
  true-in-file-name  new-name
  (let ((in-file))
    (progn
      (setq in-file (truename transl-file))
      (close transl-file)
      (setq newname (maxima-string newname))
      (rename-file in-file newname))))

(defmspec $compfile (forms)
  (let (( newname (second  forms)))
    (setq forms (cdr forms))
    (bind-transl-state
     (setq $transcompile t
	   *in-compfile* t)
     (let ((out-file-name (cond ((mfilename-onlyp (car forms))
				 ($filename_merge (pop forms)))
				(t "")))
	   (t-error nil)
	   (*translation-msgs-files* nil))
       (setq out-file-name
	     (mergef out-file-name (compfile-outputname)))
       (unwind-protect
	    (progn
	      (setq transl-file (open-out-dsk (mergef (compfile-outputname-temp)
						      out-file-name)))

	      (cond ((or (memq '$all forms) (memq '$functions forms))
		     (setq forms (mapcar #'caar (cdr $functions)))))
	      (do ((l forms (cdr l)) 
		   (declares nil nil)
		   (tr-abort nil nil)
		   (item) (lexprs nil nil) (fexprs nil nil)
		   (t-item))		;
		  ((null l))
		(setq item (car l))
		(cond ((not (atom item))
		       (print* (dconvx (translate item))))
		      (t
		       (setq t-item
			     (compile-function
			      (setq item ($verbify item))))
		       (cond (tr-abort
			      (setq t-error
				    (print-abort-msg item
						     'compfile)))
			     (t
			      (cond ($compgrind
				     (mformat transl-file
					      "~2%;; Function ~:@M~%" item)))
			      (print* t-item))))))
	      (setq out-file-name (rename-tf out-file-name nil newname))
	      (to-macsyma-namestring out-file-name))
	 ;; unwind-protected
	 (if transl-file (close transl-file))
	 (if t-error (delete-file transl-file)))))))

;;#-cl
;;(DEFMSPEC $COMPFILE (FORMS) (setq forms (cdr forms))
;;  (bind-transl-state
;;   (SETQ $TRANSCOMPILE T
;;	 *IN-COMPFILE* T)
;;   (let ((OUT-FILE-NAME (COND ((MFILENAME-ONLYP (CAR FORMS))
;;			       ($FILENAME_MERGE (POP FORMS)))
;;			      (T "")))
;;	 (t-error nil)
;;	 (*TRANSLATION-MSGS-FILES* NIL))
;;     (SETQ OUT-FILE-NAME
;;	   (MERGEF OUT-FILE-NAME (COMPFILE-OUTPUTNAME)))
;;     (UNWIND-PROTECT
;;      (PROGN
;;       (SETQ TRANSL-FILE (OPEN-out-dsk (MERGEF (COMPFILE-OUTPUTNAME-TEMP)
;;					       OUT-FILE-NAME)))

;;       (COND ((OR (MEMQ '$ALL FORMS) (MEMQ '$FUNCTIONS FORMS))
;;	      (SETQ FORMS (MAPCAR #'CAAR (CDR $FUNCTIONS)))))
;;       (DO ((L FORMS (CDR L)) 
;;	    (DECLARES NIL NIL)
;;	    (TR-ABORT NIL NIL)
;;	    (ITEM) (LEXPRS NIL NIL) (FEXPRS NIL NIL)
;;	    (T-ITEM))
;;	   ((NULL L))
;;	 (SETQ ITEM (CAR L))
;;	 (COND ((NOT (ATOM ITEM))
;;		(PRINT* (DCONVX (TRANSLATE ITEM))))
;;	       (T
;;		(SETQ T-ITEM
;;		      (COMPILE-FUNCTION
;;		       (SETQ ITEM ($VERBIFY ITEM))))
;;		(COND (TR-ABORT
;;		       (SETQ T-ERROR
;;			     (PRINT-ABORT-MSG ITEM
;;					      'COMPFILE)))
;;		      (T
;;		       (COND ($COMPGRIND
;;			      (MFORMAT TRANSL-FILE
;;				       "~2%;; Function ~:@M~%" ITEM)))
;;		       (PRINT* T-ITEM))))))
;;       (RENAME-TF OUT-FILE-NAME NIL)
;;       (TO-MACSYMA-NAMESTRING OUT-FILE-NAME))
;;      ;; unwind-protected
;;      (IF TRANSL-FILE (CLOSE TRANSL-FILE))
;;      (IF T-ERROR (DELETE-FILE TRANSL-FILE))))))


(defun compile-function (f)
  (mformat  *translation-msgs-files*
	    "~%Translating ~:@M" f)
  (let ((fun (tr-mfun f)))
    (cond (tr-abort  nil)
	  (t fun))))

(defvar tr-defaultf nil
  "A default only for the case of no arguments to $translate_file")

;;; Temporary hack during debugging of this  code.

(progn 'compile
       (defun mergef (x y) (merge-pathnames y x)))

(defun $compile_file (input-file
		      &optional bin-file  translation-output-file &aux result )
  (setq input-file (maxima-string input-file))
  (and bin-file(setq  bin-file (maxima-string bin-file)))
  (and translation-output-file
       (setq  translation-output-file (maxima-string translation-output-file)))
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

;;#-cl
;;(DEFMFUN $TRANSLATE_FILE (&OPTIONAL (INPUT-FILE-NAME NIL I-P)
;;				    (OUTPUT-FILE-NAME NIL O-P))
;;	 #+cl
;;	 (progn (cond ((atom input-file-name)
;;		       (setq input-file-name
;;			     (string-trim "&" input-file-name)))))
;;	 (OR I-P TR-DEFAULTF
;;	     (MERROR "Arguments are input file and optional output file~
;;		     ~%which defaults to second name LISP, msgs are put~
;;		     ~%in file with second file name UNLISP"))
;;	 (COND (I-P
;;		#+cl(SETQ INPUT-FILE-NAME
;;			     (pathname
;;			       input-file-name))
;;		#-cl
;;		(SETQ INPUT-FILE-NAME (MERGEF ($FILENAME_MERGE INPUT-FILE-NAME)
;;					      (trlisp-inputname-d1)))
;;		(SETQ TR-DEFAULTF INPUT-FILE-NAME))
;;	       (T
;;		(SETQ TR-DEFAULTF INPUT-FILE-NAME)))
;;	 #+cl
;;	 (SETQ OUTPUT-FILE-NAME
;;	       (progn (setq output-file-name
;;			    (pathname
;;			      (if o-p output-file-name input-file-name)))
;;		      (send output-file-name :new-type :lisp)))
;;	 #-cl
;;	 (SETQ OUTPUT-FILE-NAME
;;	       (IF O-P
;;		   (MERGEF ($FILENAME_MERGE OUTPUT-FILE-NAME) INPUT-FILE-NAME)
;;		   (MERGEF (TRLISP-OUTPUTNAME-D1) INPUT-FILE-NAME)))
;;	 (TRANSLATE-FILE  INPUT-FILE-NAME
;;			  OUTPUT-FILE-NAME
;;			  $TR_FILE_TTY_MESSAGESP ))

;; Converts a Maxima "string" (which is really a symbol that starts
;; with the character '&') to a Lisp string.
(defun maxima-string (symb)
  (string-left-trim "&" (print-invert-case symb)))

(defmfun $translate_file (input-file &optional output-file)
  (setq input-file (maxima-string input-file))
  (cond (output-file (setq output-file (maxima-string output-file))))
  (translate-file input-file output-file))

(defmvar $tr_gen_tags nil
  "If TRUE, TRANSLATE_FILE generates a TAGS file for
	 use by the text editor")

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
	#-gcl
	(*print-pprint-dispatch* (copy-pprint-dispatch)))
    #-gcl
    (progn
      (setf (readtable-case *readtable*) :invert)
      (set-pprint-dispatch '(cons (member maxima::defmtrfun))
			   #'pprint-defmtrfun))
    (loop while (and (setq expr (mread in-stream)) (consp expr))
          do (setq transl (translate-macexpr-toplevel (third expr)))
             (cond
               (*pretty-print-translation*
                (pprint transl out-stream))
               (t
                (format out-stream "~a" transl))))))

 
(defun translate-from-stream (from-stream &key to-stream eval pretty (print-function #'prin1) &aux expr transl )
  (bind-transl-state			
   (loop while (and (setq expr (mread from-stream)) (consp expr))
	  with *in-translate-file* = t
	  with *print-pretty* = pretty
	  do (setq transl (translate-macexpr-toplevel (third expr)))
					;(show transl  forms-to-compile-queue)
	  (cond (eval (eval transl)))
	  (cond (to-stream (funcall print-function transl to-stream)))
	  (loop for v in forms-to-compile-queue
		 do (show v to-stream)
		 when to-stream
		 do (funcall print-function v to-stream)
		 when eval
		 do (eval v)
		 )
	  (setq forms-to-compile-queue nil))))

(defvar trf-start-hook nil)

(defun delete-old-and-open (x)
  (open x :direction :output))

;;#-cl
;;(DEFUN DELETE-OLD-AND-OPEN (X)
;;       (IF (LET ((F (PROBE-FILE X)))
;;		(AND F (NOT (MEMQ (CADDR (NAMELIST F)) '(< >)))))
;;	   (DELETE-FILE X))
;;       (OPEN-OUT-DSK X))

(defun alter-pathname (pathname &rest options)
  (apply 'make-pathname :defaults (pathname  pathname)  options))

(defun delete-with-side-effects-if (test list)
  "Rudimentary DELETE-IF which, however, is guaranteed to call
the function TEST exactly once for each element of LIST, from
left to right."
  (loop
   while (and list (funcall test (car list)))
   do (pop list))
  (loop
   with list = list
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
			    (not (memq v *declared-translated-functions*)))
		   (push v hint))))
	   *untranslated-functions-called*))
    (when hint
      (format
       stream
       "~2%/* The compiler might be able to optimize some function calls
   if you prepend the following declaration to your maxima code: */~%")
      (mgrind `(($eval_when) $translate (($declare_translated) ,@hint))
	      stream)
      (format stream "$"))
    (when *untranslated-functions-called*
      (format
       stream
       "~2%/* The following functions are not known to be translated.~%")
      (mgrind `((mlist) ,@(nreverse *untranslated-functions-called*)) stream)
      (format stream "$ */"))
    (fresh-line stream)
    (when (or hint *untranslated-functions-called*)
      (format t "~&See the `unlisp' file for possible optimizations.~%"))))


(defun translate-file (in-file-name out-file-name
		       &optional
		       (ttymsgsp  $tr_file_tty_messagesp) &aux  warn-file
		       translated-file
		       *translation-msgs-files* *untranslated-functions-called* *declared-translated-functions*)
  (bind-transl-state
   (setq *in-translate-file* t)
   (setq translated-file (alter-pathname (or out-file-name in-file-name) :type "LISP"))
   (setq warn-file (alter-pathname in-file-name :type "UNLISP"))
   (with-open-file (in-stream in-file-name)
     (with-open-file (out-stream translated-file :direction :output
				 :if-exists :supersede)
       (with-open-file (warn-stream warn-file :direction :output
				    :if-exists :supersede)
	 (setq *translation-msgs-files* (list warn-stream))
	 (if ttymsgsp
	     (setq *translation-msgs-files*
		   (cons *standard-output* *translation-msgs-files*)))
	 (format out-stream
		 ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp ;Base: 10 -*- ;;;~%")
					;#+lispm
	 ;;	  (format out-stream ";;;Translated on: ~A"
	 ;;		  (time:print-current-time nil))
	 ;;	  #+lispm
	 ;;	  (format out-stream
	 ;;		  ";;Maxima System version ~A"
	 ;;		  (or (si:get-system-version 'maxima)
	 ;;		      (si:get-system-version 'cl-maxima)))
	 (format out-stream "~%(in-package :maxima)")
	 (format warn-stream "~%This is the unlisp file for ~A "
		 (namestring (pathname in-stream)))
	 (mformat out-stream
		  "~%;;** Variable settings were **~%~%")
	 (loop for v in (cdr $tr_state_vars)
		do (mformat out-stream   ";;~:M:~:M;~%" v (symbol-value v)))
	 (mformat *terminal-io* "~%Translation begun on ~A.~%"
		  (pathname in-stream))
	 (call-batch1 in-stream out-stream)
	 (insert-necessary-function-declares warn-stream)
	 ;; BATCH1 calls TRANSLATE-MACEXPR-toplevel on each expression read.
	 (cons '(mlist) 
	       (mapcar 'namestring
		       (mapcar 'pathname
			       (list in-stream out-stream warn-stream)))))))))


;;#-cl 
;;(DEFUN TRANSLATE-FILE (IN-FILE-NAME OUT-FILE-NAME TTYMSGSP)
;;  (BIND-TRANSL-STATE
;;   (SETQ *IN-TRANSLATE-FILE* T)
;;   (LET ((IN-FILE)
;;	 (*TRANSLATION-MSGS-FILES*)
;;	 (DSK-MSGS-FILE)
;;	 (TAGS-OUTPUT-STREAM)
;;	 (TAGS-OUTPUT-STREAM-STATE)
;;	 (WINP NIL)
;;	 (TRUE-IN-FILE-NAME))
;;     (UNWIND-PROTECT
;;      (PROGN
;;       (SETQ IN-FILE  (OPEN IN-FILE-NAME)
;;	     TRUE-IN-FILE-NAME (TO-MACSYMA-NAMESTRING (TRUENAME IN-FILE))
;;	     $TR_TRUE_NAME_OF_FILE_BEING_TRANSLATED TRUE-IN-FILE-NAME
;;	     TRANSL-FILE (DELETE-OLD-AND-OPEN
;;			  (MERGEF (trlisp-outputname-temp)
;;				  OUT-FILE-NAME))
;;	     DSK-MSGS-FILE (DELETE-OLD-AND-OPEN
;;			    #+cl
;;			     (merge-pathnames out-file-name
;;					      (make-pathname :type "unlisp"))
;;			     #-cl(MERGEF (trcomments-outputname-temp)
;;				    OUT-FILE-NAME)
;;			     )
;;	     *TRANSLATION-MSGS-FILES* (LIST DSK-MSGS-FILE))
;;       (IF $TR_GEN_TAGS
;;	   (SETQ TAGS-OUTPUT-STREAM
;;		 (OPEN-out-dsk (MERGEF (trtags-outputname-temp)
;;				       IN-FILE-NAME))))
;;       (IF TTYMSGSP
;;	   (SETQ *TRANSLATION-MSGS-FILES*
;;		 (CONS #-cl TYO #+cl *standard-output* *TRANSLATION-MSGS-FILES*)))
;;        #-cl(PROGN(CLOSE IN-FILE)
;;	      ;; IN-FILE stream of no use with old-io BATCH1.
;;	       (SETQ IN-FILE NIL))
;;       (MFORMAT DSK-MSGS-FILE "~%This is the `unlisp' file for ~A.~%"
;;		TRUE-IN-FILE-NAME)
;;       (MFORMAT *terminal-io* "~%Translation begun on ~A.~%"
;;		TRUE-IN-FILE-NAME)
;;       (IF TRF-START-HOOK (FUNCALL TRF-START-HOOK TRUE-IN-FILE-NAME))
;;       #-cl
;;       (IF TAGS-OUTPUT-STREAM (TAGS-START//END IN-FILE-NAME))
;;       (CALL-BATCH1 in-file transl-file)
;;       ;; BATCH1 calls TRANSLATE-MACEXPR on each expression read.
;;       (MFORMAT DSK-MSGS-FILE
;;		"~%//* Variable settings were *//~%~%")
;;       (DO ((L (CDR $TR_STATE_VARS) (CDR L)))
;;	   ((NULL L))
;;	 (MFORMAT-OPEN DSK-MSGS-FILE
;;		       "~:M:~:M;~%"
;;		       (CAR L) (SYMBOL-VALUE (CAR L))))
;;       #-cl(RENAME-TF OUT-FILE-NAME TRUE-IN-FILE-NAME)
;;       #-cl       (WHEN TAGS-OUTPUT-STREAM
;;	     (TAGS-START//END)
;;	     ;;(CLOSE TAGS-OUTPUT-STREAM) 
;;	     (RENAMEF TAGS-OUTPUT-STREAM (trtags-outputname)))
;;       ;;(CLOSE DSK-MSGS-FILE)
;;       ;; The CLOSE before RENAMEF clobbers the old temp file.
;;       ;; nope. you get a FILE-ALREADY-EXISTS error. darn.
;;       (let ((tr-comment-file-name (mergef (trcomments-outputname)
;;				   out-file-name)))
;;	 #-cl (if (probe-file tr-comment-file-name)
;;		     (delete-file tr-comment-file-name))
;;	#-cl (RENAMEF DSK-MSGS-FILE tr-comment-file-name)
;;       (SETQ WINP T)
;;       #-cl`((MLIST) ,(TO-MACSYMA-NAMESTRING TRUE-IN-FILE-NAME)
;;		 ,(TO-MACSYMA-NAMESTRING OUT-FILE-NAME)
;;		 ,(TO-MACSYMA-NAMESTRING (TRUENAME tr-comment-file-name))
;;		 ,@(IF TAGS-OUTPUT-STREAM
;;		       (LIST (TO-MACSYMA-NAMESTRING
;;			      (TRUENAME TAGS-OUTPUT-STREAM)))
;;		       NIL))
;;      #+cl `((mlist) ,(send in-file :truename)
;;		,(send transl-file :truename)
;;		,(send dsk-msgs-file :truename))))
;;      ;; Unwind protected. 
;;      (IF DSK-MSGS-FILE (CLOSE DSK-MSGS-FILE))
;;      (IF TRANSL-FILE   (CLOSE TRANSL-FILE))
;;      (if in-file  (close in-file))
;;      (IF TAGS-OUTPUT-STREAM (CLOSE TAGS-OUTPUT-STREAM))
;;      (WHEN (AND (NOT WINP) (NOT *TRANSL-FILE-DEBUG*))
;;	    (IF TAGS-OUTPUT-STREAM (DELETE-FILE TAGS-OUTPUT-STREAM))
;;	    (IF TRANSL-FILE (DELETE-FILE TRANSL-FILE)))))))


;; Should be rewritten to use streams.  Barf -- perhaps SPRINTER
;; doesn't take a stream argument?
;; Yes Carl SPRINTER is old i/o, but KMP is writing a new one for NIL. -GJC

(defun print* (p)
  (let ((^w t)
	(outfiles (list transl-file))
	(^r t)
					;#-cl(*NOPOINT NIL)
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
			 (not (memq (car p) '(eval-when includef)))))
	 (when flag (princ* '|(PROGN|) (terpri*))
	 (cond ($compgrind
		(sprin1 p))
	       (t
		(prin1 p transl-file)))
	 (when flag (princ* '|)|))
	 (terpri transl-file))))

(defun princ* (form) (princ form transl-file))

(defun nprinc* (&rest form)
  (mapc #'(lambda (x) (princ x transl-file)) form))

(defun terpri* () (terpri transl-file))

(defun print-module (m)
  (nprinc* " " m " version " (get m 'version)))

(defun new-comment-line ()
  (terpri*)
  (princ* ";;;"))

(defun print-transl-modules ()
  (new-comment-line)
  (print-module 'transl-autoload)
  (do ((j 0 (f1+ j))
       (s (zl-delete 'transl-autoload (copy-top-level transl-modules ))
	  (cdr s)))
      ((null s))
    (if (= 0 (fixnum-remainder j 3)) (new-comment-line))
    (print-module (car s))))


(defun print-transl-header (source)
  (mformat transl-file
	   ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp -*-~%")
  (if source
      (mformat transl-file ";;; Translated code for ~A" source)
      (mformat transl-file 
	       ";;; Translated Maxima functions generated by `compfile'."))
  (mformat transl-file
	   "~%;;; Written on ~:M, from Maxima ~A~
	    ~%;;; Translated for ~A~%" 
	   ($timedate) $version (sys-user-id))
  (print-transl-modules)
  (mformat transl-file
	   ;; The INCLUDEF must be in lower case for transportation
	   ;; of translated code to Multics.
	   "~%~
	   ~%(includef (cond ((status feature ITS) '|DSK:LIBMAX;TPRELU >|)~
	   ~%                ((status feature Multics) '|translate|)~
	   ~%                ((status feature Unix) '|libmax//tprelu.l|)~
	   ~%                (t (maxima-error '|Unknown system, see GJC@MIT-MC|))))~
           ~%~
           ~%(eval-when (compile eval) ~
           ~%  (or (status feature lispm)~
	   ~%      (setq *infile-name-key*~
	   ~%               ((lambda (file-name)~
	   ~%                           ;; temp crock for multics.~
	   ~%                          (cond ((eq (ml-typep file-name) 'list)~
	   ~%                                 (namestring file-name))~
	   ~%                                (t file-name)))~
	   ~%                  (truename infile)))))~
           ~%~
           ~%(eval-when (compile) ~
           ~%   (setq $tr_semicompile '~S)~
           ~%   (setq forms-to-compile-queue ()))~
           ~%~%(comment ~S)~%~%"
	   $tr_semicompile source)
  (cond ($transcompile
	 (update-global-declares)
	 (if $compgrind
	     (mformat
	      transl-file
	      ";;; General declarations required for translated Maxima code.~%"))
	 (print* `(declare . ,declares))))

  )

(defun print-abort-msg (fun from)
  (mformat *translation-msgs-files*
	   "~:@M failed to Translate.~
            ~%~A will continue, but file output will be aborted."
	   fun from))

(defmacro extension-filename (x) `(caddr (namelist ,x)))

;;#-cl
;;(DEFUN RENAME-TF (NEW-NAME TRUE-IN-FILE-NAME)
;;  ;; copy the TRANSL-FILE to the file of the new name.
;;  (let ((IN-FILE))
;;    (UNWIND-PROTECT
;;     (PROGN
;;      (SETQ IN-FILE (OPEN-in-dsk TRANSL-FILE))
;;      (SETQ TRANSL-FILE
;;	    (OPEN-out-dsk (TRUENAME NEW-NAME)))
;;      (PRINT-TRANSL-HEADER TRUE-IN-FILE-NAME)
;;      (MAPC #'PRINT* (NREVERSE *PRE-TRANSL-FORMS*))	; clever eh?
;;      (terpri*)
;;      (PUMP-STREAM IN-FILE TRANSL-FILE)
;;      (MFORMAT TRANSL-FILE "~%(compile-forms-to-compile-queue)~%~%")
;;      (DELETE-FILE IN-FILE))
;;     ;; if something lost...
;;     (IF IN-FILE (CLOSE IN-FILE))
;;     (IF TRANSL-FILE (CLOSE TRANSL-FILE)))))

;;#-cl
;;(DEFUN PUMP-STREAM (IN OUT &optional (n #-cl (lsh -1 -1)
;;					#+cl  most-positive-fixnum))
;;  (declare (fixnum n))
;;  (DO ((C 0))
;;      ((ZEROP N))
;;    (DECLARE (FIXNUM C))
;;    (SETQ C (+TYI IN -1))
;;    (IF (= C -1) (RETURN NIL))
;;    (+TYO C OUT)
;;    (SETQ N (f1- N))))
	     

(defmspec $translate (functs) (setq functs (cdr functs))
	  (cond ((and functs ($listp (car functs)))
		 (merror "Use the function `translate_file'"))
		(t
		 (cond ((or (memq '$functions functs)
			    (memq '$all functs))
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

(progn 'compile
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
		       (setq forms-to-compile-queue nil))))))))
