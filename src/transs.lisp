;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module transs)

(defun set-up-translate ()
  (load '|<macsym>transl.autolo|)
  (load '|<macsym>trdata.fasl|)
  (load '|<maxout>dcl.fasl|)
  (load '|<macsym>transl.fasl|)
  (load '|<macsym>trans1.fasl|)
  (load '|<macsym>troper.fasl|)
  (load '|<macsym>trutil.fasl|)
  (load '|<macsym>trans2.fasl|))

(TRANSL-MODULE TRANSS)


(DEFMVAR *TRANSL-FILE-DEBUG* NIL
	"set this to T if you don't want to have the temporary files
	used automaticaly deleted in case of errors.")

;;; User-hacking code, file-io, translator toplevel.
;;; There are various macros to cons-up filename TEMPLATES
;;; which to mergef into. The filenames are should be the only
;;; system dependant part of the code, although certain behavior
;;; of RENAMEF/MERGEF/DELETEF is assumed.

(defmvar $TR_OUTPUT_FILE_DEFAULT '$TRLISP
	 "This is the second file name to be used for translated lisp
	 output.")

(DEFMVAR $TR_FILE_TTY_MESSAGESP nil
	 "It TRUE messages about translation of the file are sent
	 to the TTY also.")

(DEFMVAR $TR_WINDY T
	 "Generate helpful comments and programming hints.")

(DEFTRVAR *TRANSLATION-MSGS-FILES* NIL
	"Where the warning and other comments goes.")

(DEFTRVAR $TR_VERSION (GET 'TRANSL-AUTOLOAD 'VERSION))

(DEFMVAR TRANSL-FILE NIL "output stream of $COMPFILE and $TRANSLATE_FILE")

(DEFMVAR $COMPGRIND NIL "If TRUE lisp output will be pretty-printed.")

(DEFMVAR $TR_TRUE_NAME_OF_FILE_BEING_TRANSLATED nil
	 "This is set by TRANSLATE_FILE for use by user macros
	 which want to know the name of the source file.")

(DEFMVAR $TR_STATE_VARS
	 '((MLIST) $TRANSCOMPILE $TR_SEMICOMPILE
	   #+cl
	   $TRANSLATE_FAST_ARRAYS
	   $TR_WARN_UNDECLARED
	   $TR_WARN_MEVAL
	   $TR_WARN_FEXPR
	   $TR_WARN_MODE
	   $TR_WARN_UNDEFINED_VARIABLE
	   $TR_FUNCTION_CALL_DEFAULT 
	   $TR_ARRAY_AS_REF
	   $TR_NUMER
	   $DEFINE_VARIABLE))

(defmacro compfile-outputname-temp () 
;  #-(or Multics Cl) ''|_CMF_ OUTPUT|
  #+Multics ''(f* _cmf_ output)
  #+cl '`,(pathname "_cmf_"))

(defmacro compfile-outputname ()
  #-(or Multics Cl)'`((DSK ,(STATUS UDIR))
	      ,(STATUS USERID)
	      ,(stripdollar $TR_OUTPUT_FILE_DEFAULT))
  #+Multics '`(,(status udir) ,(stripdollar $tr_output_file_default))
  #+cl '`,(pathname (stripdollar $tr_output_file_default)))

(defmacro trlisp-inputname-d1 ()
  ;; so hacks on DEFAULTF will not stray the target.
  #-(or Multics Cl) '`((dsk ,(status udir)) * >)
  #+Multics '`(,(status udir) * *)
  #+cl '`,(pathname ""))

(defmacro trlisp-outputname-d1 ()
  #-(or Multics Cl) '`((* *)  * ,(stripdollar $TR_OUTPUT_FILE_DEFAULT))
  #+Multics '`(* * ,(stripdollar $tr_output_file_default))
  #+cl '`,(pathname (stripdollar $tr_output_file_default))) 

(defmacro trlisp-outputname () 
;  #-(or Multics Cl) ''|* TRLISP|
  #+Multics ''(* * lisp)
  #+cl '`,(make-pathname :type "LISP"))

(defmacro trlisp-outputname-temp ()
;  #-(or Multics Cl) ''|* _TRLI_|
  #+Multics ''(* * _trli_)
  #+cl '`,(pathname "_trli_"))

(defmacro trtags-outputname () 
;  #-(or Multics Cl) ''|* TAGS|
  #+Multics ''(* * tags)
  #+cl '`,(pathname "tags"))

(defmacro trtags-outputname-temp ()
;  #-(or Multics Cl) ''|* _TAGS_|
  #+Multics ''(* * _tags_)
  #+cl '`,(pathname "_tags_"))


(defmacro trcomments-outputname () 
;  #-(or Multics Cl) ''|* UNLISP|
  #+Multics ''(* * unlisp)
  #+cl '`,(pathname "unlisp"))

(defmacro trcomments-outputname-temp () 
;  #-(or Multics Cl) ''|* _UNLI_|
  #+Multics ''(* * _unli_)
  #+cl '`,(pathname "_unli_"))

(DEFTRVAR DECLARES NIL)
;;;these first five functions have been altered to run on 
;;;the 3600 we must try to fix translate-file  wfs fixed -wfs
#+cl
(defmacro mytruename (x) `(truename ,x))


#+cl	
(defun rename-tf (new-name true-in-file-name &optional newname)
   true-in-file-name  new-name
  (let ((in-file))
    (progn
      (setq in-file (truename transl-file))
      (close transl-file)
      (setq newname (sub-seq (string newname) 1))
      (rename-file in-file newname))))

	
#+CL
(DEFMSPEC $COMPFILE (FORMS)
  (let (( newname (second  forms)))
  (setq forms (cdr forms))
  (bind-transl-state
   (SETQ $TRANSCOMPILE T
	 *IN-COMPFILE* T)
   (let ((OUT-FILE-NAME (COND ((MFILENAME-ONLYP (CAR FORMS))
 			       ($FILENAME_MERGE (POP FORMS)))
			      (T "")))
	 (t-error nil)
	 (*TRANSLATION-MSGS-FILES* NIL))
     (SETQ OUT-FILE-NAME
	   (MERGEF OUT-FILE-NAME (COMPFILE-OUTPUTNAME)))
     (UNWIND-PROTECT
      (PROGN
       (SETQ TRANSL-FILE (OPEN-out-dsk (MERGEF (COMPFILE-OUTPUTNAME-TEMP)
					       OUT-FILE-NAME)))

       (COND ((OR (MEMQ '$ALL FORMS) (MEMQ '$FUNCTIONS FORMS))
	      (SETQ FORMS (MAPCAR #'CAAR (CDR $FUNCTIONS)))))
       (DO ((L FORMS (CDR L)) 
	    (DECLARES NIL NIL)
	    (TR-ABORT NIL NIL)
	    (ITEM) (LEXPRS NIL NIL) (FEXPRS NIL NIL)
	    (T-ITEM))				;
	   ((NULL L))
	 (SETQ ITEM (CAR L))
	 (COND ((NOT (ATOM ITEM))
		(PRINT* (DCONVX (TRANSLATE ITEM))))
	       (T
		(SETQ T-ITEM
		      (COMPILE-FUNCTION
		       (SETQ ITEM ($VERBIFY ITEM))))
		(COND (TR-ABORT
		       (SETQ T-ERROR
			     (PRINT-ABORT-MSG ITEM
					      'COMPFILE)))
		      (T
		       (COND ($COMPGRIND
			      (MFORMAT TRANSL-FILE
				       "~2%;; Function ~:@M~%" ITEM)))
		       (PRINT* T-ITEM))))))
       (setq out-file-name (RENAME-TF OUT-FILE-NAME NIL newname))
       (TO-MACSYMA-NAMESTRING OUT-FILE-NAME))
      ;; unwind-protected
      (IF TRANSL-FILE (CLOSE TRANSL-FILE))
      (IF T-ERROR (DELETEF TRANSL-FILE)))))))
#-cl

(DEFMSPEC $COMPFILE (FORMS) (setq forms (cdr forms))
  (bind-transl-state
   (SETQ $TRANSCOMPILE T
	 *IN-COMPFILE* T)
   (let ((OUT-FILE-NAME (COND ((MFILENAME-ONLYP (CAR FORMS))
			       ($FILENAME_MERGE (POP FORMS)))
			      (T "")))
	 (t-error nil)
	 (*TRANSLATION-MSGS-FILES* NIL))
     (SETQ OUT-FILE-NAME
	   (MERGEF OUT-FILE-NAME (COMPFILE-OUTPUTNAME)))
     (UNWIND-PROTECT
      (PROGN
       (SETQ TRANSL-FILE (OPEN-out-dsk (MERGEF (COMPFILE-OUTPUTNAME-TEMP)
					       OUT-FILE-NAME)))

       (COND ((OR (MEMQ '$ALL FORMS) (MEMQ '$FUNCTIONS FORMS))
	      (SETQ FORMS (MAPCAR #'CAAR (CDR $FUNCTIONS)))))
       (DO ((L FORMS (CDR L)) 
	    (DECLARES NIL NIL)
	    (TR-ABORT NIL NIL)
	    (ITEM) (LEXPRS NIL NIL) (FEXPRS NIL NIL)
	    (T-ITEM))
	   ((NULL L))
	 (SETQ ITEM (CAR L))
	 (COND ((NOT (ATOM ITEM))
		(PRINT* (DCONVX (TRANSLATE ITEM))))
	       (T
		(SETQ T-ITEM
		      (COMPILE-FUNCTION
		       (SETQ ITEM ($VERBIFY ITEM))))
		(COND (TR-ABORT
		       (SETQ T-ERROR
			     (PRINT-ABORT-MSG ITEM
					      'COMPFILE)))
		      (T
		       (COND ($COMPGRIND
			      (MFORMAT TRANSL-FILE
				       "~2%;; Function ~:@M~%" ITEM)))
		       (PRINT* T-ITEM))))))
       (RENAME-TF OUT-FILE-NAME NIL)
       (TO-MACSYMA-NAMESTRING OUT-FILE-NAME))
      ;; unwind-protected
      (IF TRANSL-FILE (CLOSE TRANSL-FILE))
      (IF T-ERROR (DELETEF TRANSL-FILE))))))


(DEFUN COMPILE-FUNCTION (F)
       (MFORMAT  *TRANSLATION-MSGS-FILES*
		 "~%Translating ~:@M" F)
       (LET ((FUN (TR-MFUN F)))
	    (COND (TR-ABORT  NIL)
		  (T FUN))))

(DEFVAR TR-DEFAULTF NIL
	"A default only for the case of NO arguments to $TRANSLATE_FILE")

;;; Temporary hack during debugging of this  code.
#+cl
(progn 'compile
#-cl
(defun mergef (x y) (fs:merge-pathnames y x))
#+cl
(defun mergef (x y) (merge-pathnames y x))
#-cl
(defmacro truename (x) `(send ,x ':truename)))

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
  #+(or cmu clisp)
  (multiple-value-bind (output-truename warnings-p failure-p)
      (compile-file input-file :output-file (or bin-file t))
    ;; If the compiler encountered errors, don't set bin-file to
    ;; indicate that we found errors. Is this what we want?
    (unless failure-p
      (setq bin-file output-truename)))
  #-(or cmu clisp)
  (setq bin-file (compile-file input-file :output-file bin-file))
  (append result (list bin-file)))

#-cl
(DEFMFUN $TRANSLATE_FILE (&OPTIONAL (INPUT-FILE-NAME NIL I-P)
				    (OUTPUT-FILE-NAME NIL O-P))
	 #+cl
	 (progn (cond ((atom input-file-name)
		       (setq input-file-name
			     (string-trim "&" input-file-name)))))
	 (OR I-P TR-DEFAULTF
	     (MERROR "Arguments are input file and optional output file~
		     ~%which defaults to second name LISP, msgs are put~
		     ~%in file with second file name UNLISP"))
	 (COND (I-P
		#+cl(SETQ INPUT-FILE-NAME
			     (pathname
			       input-file-name))
		#-cl
		(SETQ INPUT-FILE-NAME (MERGEF ($FILENAME_MERGE INPUT-FILE-NAME)
					      (trlisp-inputname-d1)))
		(SETQ TR-DEFAULTF INPUT-FILE-NAME))
	       (T
		(SETQ TR-DEFAULTF INPUT-FILE-NAME)))
	 #+cl
	 (SETQ OUTPUT-FILE-NAME
	       (progn (setq output-file-name
			    (pathname
			      (if o-p output-file-name input-file-name)))
		      (send output-file-name :new-type :lisp)))
	 #-cl
	 (SETQ OUTPUT-FILE-NAME
	       (IF O-P
		   (MERGEF ($FILENAME_MERGE OUTPUT-FILE-NAME) INPUT-FILE-NAME)
		   (MERGEF (TRLISP-OUTPUTNAME-D1) INPUT-FILE-NAME)))
	 (TRANSLATE-FILE  INPUT-FILE-NAME
			  OUTPUT-FILE-NAME
			  $TR_FILE_TTY_MESSAGESP ))

#+cl
(defun maxima-string (symb)
  (string-left-trim "&" (string symb)))

#+cl
(defmfun $translate_file (input-file &optional output-file)
      (setq input-file (maxima-string input-file))
      (cond (output-file (setq output-file (maxima-string output-file))))
      (translate-file input-file output-file))

(DEFMVAR $TR_GEN_TAGS NIL
	 "If TRUE, TRANSLATE_FILE generates a TAGS file for
	 use by the text editor")

(defvar *pretty-print-translation* t)
#+cl
(defun call-batch1 (in-stream out-stream &aux expr transl)
  (cleanup)
  ;; we want the thing to start with a newline..
  (newline in-stream #\n)
  (sloop while (and (setq  expr	  (mread in-stream))
		   (consp expr))
	do (setq transl (translate-macexpr-toplevel (third expr)))
	(cond (*pretty-print-translation* (pprint transl out-stream))
	      (t
	       (format out-stream  "~A" transl)))))

 
(defun translate-from-stream (from-stream &key to-stream eval pretty (print-function #'prin1) &aux expr transl )
  (bind-transl-state			
  (sloop while (and (setq expr (mread from-stream)) (consp expr))
	with *in-translate-file* = t
	with *print-pretty* = pretty
	do (setq transl (translate-macexpr-toplevel (third expr)))
	;(show transl  forms-to-compile-queue)
	(cond (eval (eval transl)))
	(cond (to-stream (funcall print-function transl to-stream)))
	(sloop for v in forms-to-compile-queue
	      do (show v to-stream)
	      when to-stream
	      do (funcall print-function v to-stream)
	      when eval
	      do (eval v)
	      )
	(setq forms-to-compile-queue nil))))

(DEFVAR TRF-START-HOOK NIL)

#+cl
(DEFUN DELETE-OLD-AND-OPEN (X)
    (open x :direction :output))
#-cl
(DEFUN DELETE-OLD-AND-OPEN (X)
       (IF (LET ((F (PROBE-FILE X)))
		(AND F (NOT (MEMQ (CADDR (NAMELIST F)) '(< >)))))
	   (DELETEF X))
       (OPEN-OUT-DSK X))

#+cl
(defun alter-pathname (pathname &rest options)
  (apply 'make-pathname :defaults (pathname  pathname)  options))
#+cl
(defun insert-necessary-function-declares (stream)
  (sloop for v in *untranslated-functions-called*
	when (get v 'once-translated)
	do (setq  *untranslated-functions-called*  (delete v *untranslated-functions-called*))
	and
	collecting v into warns
	finally (cond (warns
		       (format stream "~2%;;The following functions declaration should ~
                                          ;;go at the front of your macsyma file ~
                                          ~%;;" )
		       (mgrind `(($eval_when) $translate (($declare_translated) ,@ warns))
			         stream)
		       (format t "~%See the extra declarations at the end of the translated file.  They ~
                     should be included in you macsyma file, and you should retranslate.")))))


#+cl
(DEFUN TRANSLATE-FILE (IN-FILE-NAME OUT-FILE-NAME
				    &optional
				    (TTYMSGSP  $TR_FILE_TTY_MESSAGESP) &aux  warn-file
				    translated-file
				    *translation-msgs-files* *untranslated-functions-called*)
  (BIND-TRANSL-STATE
    (SETQ *IN-TRANSLATE-FILE* T)
    (setq translated-file (alter-pathname (or out-file-name in-file-name) :type "LISP"))
    (setq warn-file (alter-pathname in-file-name :type "UNLISP"))
    (with-open-file (in-stream in-file-name)
      (with-open-file (out-stream translated-file :direction :output)
	(with-open-file (warn-stream warn-file :direction :output)
	      (setq *translation-msgs-files* (list warn-stream))
	  (IF TTYMSGSP
	      (SETQ *TRANSLATION-MSGS-FILES*
		    (CONS *standard-output* *TRANSLATION-MSGS-FILES*)))
	  (format out-stream
  ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp ;Base: 10 -*- ;;;~%")
	  #+lispm
	  (format out-stream ";;;Translated on: ~A"
		  (time:print-current-time nil))
	  #+lispm
	  (format out-stream
		  ";;Maxima System version ~A"
		  (or (si:get-system-version 'maxima)
		      (si:get-system-version 'cl-maxima)))
	  #+cl (format out-stream "~%(in-package \"MAXIMA\")")
	  (format warn-stream "~%This is the unlisp file for ~A "
	   (namestring (pathname in-stream)))
  	  (MFORMAT out-stream
		   "~%;;** Variable settings were **~%~%")
	  (sloop for v in (cdr $tr_state_vars)
		do (mformat out-stream   ";;~:M:~:M;~%" v (symbol-value v)))
	  (MFORMAT *terminal-io* "~%Translation begun on ~A.~%"
		   (pathname in-stream))
	  (CALL-BATCH1 in-stream out-stream)
	  (insert-necessary-function-declares out-stream)
	  ;; BATCH1 calls TRANSLATE-MACEXPR-toplevel on each expression read.
	  (cons '(mlist) 
		(mapcar 'namestring
		(mapcar 'pathname
			(list in-stream out-stream warn-stream)))))))))


#-cl 
(DEFUN TRANSLATE-FILE (IN-FILE-NAME OUT-FILE-NAME TTYMSGSP)
  (BIND-TRANSL-STATE
   (SETQ *IN-TRANSLATE-FILE* T)
   (LET ((IN-FILE)
	 (*TRANSLATION-MSGS-FILES*)
	 (DSK-MSGS-FILE)
	 (TAGS-OUTPUT-STREAM)
	 (TAGS-OUTPUT-STREAM-STATE)
	 (WINP NIL)
	 (TRUE-IN-FILE-NAME))
     (UNWIND-PROTECT
      (PROGN
       (SETQ IN-FILE  (OPEN IN-FILE-NAME)
	     TRUE-IN-FILE-NAME (TO-MACSYMA-NAMESTRING (TRUENAME IN-FILE))
	     $TR_TRUE_NAME_OF_FILE_BEING_TRANSLATED TRUE-IN-FILE-NAME
	     TRANSL-FILE (DELETE-OLD-AND-OPEN
			  (MERGEF (trlisp-outputname-temp)
				  OUT-FILE-NAME))
	     DSK-MSGS-FILE (DELETE-OLD-AND-OPEN
			    #+cl
			     (merge-pathnames out-file-name
					      (make-pathname :type "unlisp"))
			     #-cl(MERGEF (trcomments-outputname-temp)
				    OUT-FILE-NAME)
			     )
	     *TRANSLATION-MSGS-FILES* (LIST DSK-MSGS-FILE))
       (IF $TR_GEN_TAGS
	   (SETQ TAGS-OUTPUT-STREAM
		 (OPEN-out-dsk (MERGEF (trtags-outputname-temp)
				       IN-FILE-NAME))))
       (IF TTYMSGSP
	   (SETQ *TRANSLATION-MSGS-FILES*
		 (CONS #-cl TYO #+cl *standard-output* *TRANSLATION-MSGS-FILES*)))
        #-cl(PROGN(CLOSE IN-FILE)
	      ;; IN-FILE stream of no use with old-io BATCH1.
	       (SETQ IN-FILE NIL))
       (MFORMAT DSK-MSGS-FILE "~%This is the UNLISP file for ~A.~%"
		TRUE-IN-FILE-NAME)
       (MFORMAT *terminal-io* "~%Translation begun on ~A.~%"
		TRUE-IN-FILE-NAME)
       (IF TRF-START-HOOK (FUNCALL TRF-START-HOOK TRUE-IN-FILE-NAME))
       #-cl
       (IF TAGS-OUTPUT-STREAM (TAGS-START//END IN-FILE-NAME))
       (CALL-BATCH1 in-file transl-file)
       ;; BATCH1 calls TRANSLATE-MACEXPR on each expression read.
       (MFORMAT DSK-MSGS-FILE
		"~%//* Variable settings were *//~%~%")
       (DO ((L (CDR $TR_STATE_VARS) (CDR L)))
	   ((NULL L))
	 (MFORMAT-OPEN DSK-MSGS-FILE
		       "~:M:~:M;~%"
		       (CAR L) (SYMBOL-VALUE (CAR L))))
       #-cl(RENAME-TF OUT-FILE-NAME TRUE-IN-FILE-NAME)
       #-cl       (WHEN TAGS-OUTPUT-STREAM
	     (TAGS-START//END)
	     ;;(CLOSE TAGS-OUTPUT-STREAM) 
	     (RENAMEF TAGS-OUTPUT-STREAM (trtags-outputname)))
       ;;(CLOSE DSK-MSGS-FILE)
       ;; The CLOSE before RENAMEF clobbers the old temp file.
       ;; nope. you get a FILE-ALREADY-EXISTS error. darn.
       (let ((tr-comment-file-name (mergef (trcomments-outputname)
				   out-file-name)))
	 #-cl (if (probe-file tr-comment-file-name)
		     (deletef tr-comment-file-name))
	#-cl (RENAMEF DSK-MSGS-FILE tr-comment-file-name)
       (SETQ WINP T)
       #-cl`((MLIST) ,(TO-MACSYMA-NAMESTRING TRUE-IN-FILE-NAME)
		 ,(TO-MACSYMA-NAMESTRING OUT-FILE-NAME)
		 ,(TO-MACSYMA-NAMESTRING (TRUENAME tr-comment-file-name))
		 ,@(IF TAGS-OUTPUT-STREAM
		       (LIST (TO-MACSYMA-NAMESTRING
			      (TRUENAME TAGS-OUTPUT-STREAM)))
		       NIL))
      #+cl `((mlist) ,(send in-file :truename)
		,(send transl-file :truename)
		,(send dsk-msgs-file :truename))))
      ;; Unwind protected. 
      (IF DSK-MSGS-FILE (CLOSE DSK-MSGS-FILE))
      (IF TRANSL-FILE   (CLOSE TRANSL-FILE))
      (if in-file  (close in-file))
      (IF TAGS-OUTPUT-STREAM (CLOSE TAGS-OUTPUT-STREAM))
      (WHEN (AND (NOT WINP) (NOT *TRANSL-FILE-DEBUG*))
	    (IF TAGS-OUTPUT-STREAM (DELETEF TAGS-OUTPUT-STREAM))
	    (IF TRANSL-FILE (DELETEF TRANSL-FILE)))))))



;; Should be rewritten to use streams.  Barf -- perhaps SPRINTER doesn't take
;; a stream argument? Yes Carl SPRINTER is old i/o, but KMP is writing
;; a new one for NIL.  -GJC

(DEFUN PRINT* (P)
  (LET ((^W T)
	(OUTFILES (LIST TRANSL-FILE))
	(^R T)
	#-cl(*NOPOINT NIL)
	($LOADPRINT NIL)) ;;; lusing old I/O !!!!!
       (declare (special OUTFILES))
    (SUB-PRINT* P)))

;;; i might as well be real pretty and flatten out PROGN's.

(DEFUN SUB-PRINT* (P &AUX (FLAG NIL))
  (COND ((ATOM P))
	((AND (EQ (CAR P) 'PROGN) (CDR P) (EQUAL (CADR P) ''COMPILE))
	 (MAPC #'SUB-PRINT* (CDDR P)))
	(T
	 (SETQ FLAG (AND $TR_SEMICOMPILE
			 (NOT (MEMQ (CAR P) '(EVAL-WHEN INCLUDEF)))))
	 (WHEN FLAG (PRINC* '|(PROGN|) (TERPRI*))
	 (COND ($COMPGRIND
		(SPRIN1 P))
	       (T
		(PRIN1 P TRANSL-FILE)))
	 (WHEN FLAG (PRINC* '|)|))
	 (TERPRI TRANSL-FILE))))

(DEFUN PRINC* (FORM) (PRINC FORM TRANSL-FILE))

(DEFUN NPRINC* (&REST FORM)
  (MAPC #'(LAMBDA (X) (PRINC X TRANSL-FILE)) FORM))

(DEFUN TERPRI* () (TERPRI TRANSL-FILE))

(DEFUN PRINT-MODULE (M)
  (NPRINC* " " M " version " (GET M 'VERSION)))

(DEFUN NEW-COMMENT-LINE ()
  (TERPRI*)
  (PRINC* ";;;"))

(defun print-TRANSL-MODULEs ()
  (NEW-COMMENT-LINE)
  (PRINT-MODULE 'TRANSL-AUTOLOAD)
  (DO ((J 0 (f1+ J))
       (S (zl-DELETE 'TRANSL-AUTOLOAD (copy-top-level TRANSL-MODULES ))
	  (CDR S)))
      ((NULL S))
    (IF (= 0 (fixnum-remainder J 3)) (NEW-COMMENT-LINE))
    (PRINT-MODULE (CAR S))))


(DEFUN PRINT-TRANSL-HEADER (SOURCE)
  (MFORMAT TRANSL-FILE
	   ";;; -*- Mode: Lisp; package:maxima; syntax:common-lisp -*-~%")
  (IF SOURCE
      (MFORMAT TRANSL-FILE ";;; Translated code for ~A" SOURCE)
      (MFORMAT TRANSL-FILE 
	       ";;; Translated MACSYMA functions generated by COMPFILE."))
  (MFORMAT TRANSL-FILE
	   "~%;;; Written on ~:M, from MACSYMA ~A~
	    ~%;;; Translated for ~A~%" 
	   ($TIMEDATE) $VERSION (sys-user-id))
  (print-TRANSL-MODULEs)
  (MFORMAT TRANSL-FILE
	   ;; The INCLUDEF must be in lower case for transportation
	   ;; of translated code to Multics.
	   "~%~
	   ~%(includef (cond ((status feature ITS) '|DSK:LIBMAX;TPRELU >|)~
	   ~%                ((status feature Multics) '|translate|)~
	   ~%                ((status feature Unix) '|libmax//tprelu.l|)~
	   ~%                (t (MAXIMA-ERROR '|Unknown system, see GJC@MIT-MC|))))~
           ~%~
           ~%(eval-when (compile eval)~
           ~%  (or (status feature lispm)~
	   ~%      (setq *infile-name-key*~
	   ~%               ((lambda (file-name)~
	   ~%                           ;; temp crock for multics.~
	   ~%                          (cond ((eq (ml-typep file-name) 'list)~
	   ~%                                 (namestring file-name))~
	   ~%                                (t file-name)))~
	   ~%                  (truename infile)))))~
           ~%~
           ~%(eval-when (compile)~
           ~%   (setq $tr_semicompile '~S)~
           ~%   (setq forms-to-compile-queue ()))~
           ~%~%(comment ~S)~%~%"
            $tr_semicompile source)
(COND ($TRANSCOMPILE
       (UPDATE-GLOBAL-DECLARES)
       (IF $COMPGRIND
	   (MFORMAT
	    TRANSL-FILE
	    ";;; General declarations required for translated MACSYMA code.~%"))
       (PRINT* `(DECLARE . ,DECLARES))))

)

(DEFUN PRINT-ABORT-MSG (FUN FROM)
  (MFORMAT *TRANSLATION-MSGS-FILES*
	   "~:@M failed to Translate.~
            ~%~A will continue, but file output will be aborted."
	   FUN FROM))

(defmacro extension-filename (x) `(caddr (namelist ,x)))
#-cl
(DEFUN RENAME-TF (NEW-NAME TRUE-IN-FILE-NAME)
  ;; copy the TRANSL-FILE to the file of the new name.
  (let ((IN-FILE))
    (UNWIND-PROTECT
     (PROGN
      (SETQ IN-FILE (OPEN-in-dsk TRANSL-FILE))
      (SETQ TRANSL-FILE
	    (OPEN-out-dsk (TRUENAME NEW-NAME)))
      (PRINT-TRANSL-HEADER TRUE-IN-FILE-NAME)
      (MAPC #'PRINT* (NREVERSE *PRE-TRANSL-FORMS*))	; clever eh?
      (terpri*)
      (PUMP-STREAM IN-FILE TRANSL-FILE)
      (MFORMAT TRANSL-FILE "~%(compile-forms-to-compile-queue)~%~%")
      (DELETEF IN-FILE))
     ;; if something lost...
     (IF IN-FILE (CLOSE IN-FILE))
     (IF TRANSL-FILE (CLOSE TRANSL-FILE)))))


(DEFUN PUMP-STREAM (IN OUT &optional (n #-cl (lsh -1 -1)
					#+cl  most-positive-fixnum))
  (declare (fixnum n))
  (DO ((C 0))
      ((ZEROP N))
    (DECLARE (FIXNUM C))
    (SETQ C (+TYI IN -1))
    (IF (= C -1) (RETURN NIL))
    (+TYO C OUT)
    (SETQ N (f1- N))))
	     


(DEFMSPEC $TRANSLATE (FUNCTS) (SETQ FUNCTS (CDR FUNCTS))
  (COND ((AND FUNCTS ($LISTP (CAR FUNCTS)))
	 (MERROR "Use the function TRANSLATE_FILE"))
	(T
	 (COND ((OR (MEMQ '$FUNCTIONS FUNCTS)
		    (MEMQ '$ALL FUNCTS))
		(SETQ FUNCTS (MAPCAR 'CAAR (CDR $FUNCTIONS)))))
	 (DO ((L FUNCTS (CDR L))
	      (V NIL))
	     ((NULL L) `((MLIST) ,@(NREVERSE V)))
	   (COND ((ATOM (CAR L))
		  (LET ((IT (TRANSLATE-FUNCTION ($VERBIFY (CAR L)))))
		    (IF IT (PUSH IT V))))
		 (T
		  (TR-TELL
		   (CAR L)
		   " is an illegal argument to TRANSLATE.")))))))

#+CL
(PROGN 'COMPILE
(DECLARE-TOP (SPECIAL forms-to-compile-queue))
(DEFMSPEC $COMPILE (FORM)
  (LET ((L (MEVAL `(($TRANSLATE),@(CDR FORM)))))
    (LET ((forms-to-compile-queue ()))
      (MAPC #'(LAMBDA (X) (IF (FBOUNDP X) (COMPILE X))) (CDR L))
      (DO ()
	  ((NULL FORMS-TO-COMPILE-QUEUE) L)
	(MAPC #'(LAMBDA (FORM)
		  (EVAL FORM)
		  (AND (consp FORM)
		       (EQ (CAR FORM) 'DEFUN)
		       (SYMBOLP (CADR FORM))
		       (COMPILE (CADR FORM))))
	      (PROG1 FORMS-TO-COMPILE-QUEUE
		     (SETQ FORMS-TO-COMPILE-QUEUE NIL)))))))
)
