;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                Macros for TRANSL source compilation.                 ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module transm macro)
(load-macsyma-macros procs)
(load-macsyma-macros-at-runtime 'procs)

(defvar transl-modules nil)

;;; Simple but effective single-level module definitions
;;; and utilities which work through property lists.
;;; Information has to be in various places:
;;; [1] Compile-time of the TRANSLATOR itself.
;;; [2] Runtime of the translator.
;;; [3] Translate-time of user-code
;;; [4] Compile-time of user-code.
;;; [5] Runtime of user-code.
;;; [6] "Utilities" or documentation-time of user-code.

;;; -GJC 

;;; Note: Much of the functionality here was in use before macsyma as
;;; a whole got such mechanisms, however we must admit that the macsyma
;;; user-level (and non-modular global only) INFOLISTS of FUNCTIONS and VALUES,
;;; inspired this, motivated by my characteristic lazyness.

(defmacro enterq (thing list)
  ;; should be a DEF-ALTERANT
  `(or (memq ,thing ,list)
    (setf ,list (cons ,thing ,list))))

(defmacro def-transl-module (name &rest properties)
  `(progn
    (enterq ',name transl-modules)
    ,@(mapcar #'(lambda (p)
		  `(defprop ,name
		    ,(if (atom p) t (cdr p))
		    ,(if (atom p) p (car p))))
	      properties)))

(def-transl-module transs ttime-auto)
(def-transl-module transl ttime-auto (first-load trdata dcl))
(def-transl-module trutil ttime-auto)
(def-transl-module trans1 ttime-auto)
(def-transl-module trans2 ttime-auto)
(def-transl-module trans3 ttime-auto)
(def-transl-module trans4 ttime-auto)
(def-transl-module trans5 ttime-auto)
(def-transl-module transf ttime-auto)
(def-transl-module troper ttime-auto)
(def-transl-module trpred ttime-auto)

(def-transl-module mtags ttime-auto)
(def-transl-module mdefun)
(def-transl-module transq)
(def-transl-module fcall  no-load-auto)
(def-transl-module acall no-load-auto)
(def-transl-module trdata   no-load-auto)
(def-transl-module mcompi ttime-auto)

(def-transl-module dcl pseudo)		; more data
(defprop dcl maxdoc fasl-dir)

(def-transl-module trmode ttime-auto
  no-load-auto
  ;; Temporary hack, TRANSL AUTOLOADs should be
  ;; in a different file from functional autoloads.
  )

(def-transl-module trhook hyper)
(def-transl-module transl-autoload pseudo)

(eval-when (eval compile load)
  (load-macsyma-macros procs))
#+its
(defun tr-fasl-file-name (foo)
  (namestring `((dsk  ,(get! foo 'fasl-dir)) ,foo fasl)))

#+multics
(defun tr-fasl-file-name (foo)
  (namestring `,(executable-dir foo)))

#+its
(defvar transl-autoload-oldio-name "DSK:MACSYM;TRANSL AUTOLO")

#+multics
(defvar transl-autoload-oldio-name (namestring (executable-dir 'transl/.autoload)))

(defvar module-stack nil)

(defmacro transl-module (name)
  (if (not (memq name transl-modules))
      (maxima-error "Not a `transl-module', see LIBMAX;TRANSM >"))
  #+pdp10
  (progn (push name module-stack)
	 (push '(eval-when (compile eval)
		 (transl-module-do-it)
		 (pop module-stack))
	       eof-compile-queue)
	 (putprop name nil 'functions)
	 (putprop name nil 'tr-props)
	 (putprop name nil 'variables)
	 (do ((l transl-modules (cdr l)))
	     ((null l))
	   (if (eq (car l) name) nil
	       (load-module-info (car l))))
	 )
  #+pdp10
  `(progn 'compile
    (defprop ,name
	,(caddr (namelist (truename infile)))
      version)
    (progn
      ,(if (not (get name 'no-load-auto))
	   `(or (get 'transl-autoload 'version)
	     ($load ',transl-autoload-oldio-name)))
      ,@(mapcar #'(lambda (u)
		    `(or (get ',u 'version)
		      ($load
		       ',(tr-fasl-file-name u))))
		(get name 'first-load))))
  #-pdp10
  '(comment there are reasonable things to do here)
  )

#+pdp10

(defun lambda-type (arglist)
  (cond ((null arglist)
	 '(*expr . (nil . 0)))
	((atom arglist)
	 '(*lexpr . nil))
	(t
	 ;; (FOO BAR &OPTIONAL ... &REST L &AUX)
	 ;; #O776 is the MAX MAX.
	 (do ((min 0)
	      (max 0)
	      (optional nil)
	      (l arglist (cdr l)))
	     ((null l)
	      (if (= min max)
		  `(*expr . (nil . ,min))
		  `(*lexpr . (,min . ,max))))
	   (case (car l)
	     ((&rest)
	      (setq max #o776)
	      (setq l nil))
	     ((&optional)
	      (setq optional t))
	     ((&aux)
	      (setq l nil))
	     (t
	      (if (and (symbolp (car l))
		       (= #\& (getcharn (car l) 1)))
		  (return
		    (lambda-type
		     (maxima-error (list "arglist has unknown &keword" (car l))
				   arglist 'wrng-type-arg))))
	      (or optional (setq min (f1+ min)))
	      (setq max (f1+ max))))))))

(def-def-property translate (form))

#+cl
(defmacro def%tr (name lambda-list &body body &aux definition)
  (setq definition
	(cond ((and (null body) (symbolp lambda-list))
	       `(def-same%tr ,name ,lambda-list))
	      (t
	       #+pdp10
	       (enterq name (get (car module-stack) 'tr-props))
	       `(defun-prop (,name translate) ,lambda-list ,@ body))))
  `(eval-when (compile eval load)
    #+lispm(record-source-file-name ',name 'def%tr)
    ,definition))


#-cl
(defmacro def%tr (name lambda-list &rest body)
  (cond ((and (null body) (symbolp lambda-list))
	 `(def-same%tr ,name ,lambda-list))
	(t
	 #+pdp10
	 (enterq name (get (car module-stack) 'tr-props))
	 `(def-translate-property ,name
	   ,lambda-list ,@body))))

(defmacro def-same%tr (name same-as)
  ;; right now MUST be used in the SAME file.
  #+pdp10
  (enterq name (get (car module-stack) 'tr-props))
  `(putprop ',name
    (or (get ',same-as 'translate)
     (maxima-error '|No TRANSLATE property to alias.| ',same-as))
    'translate))

(defmacro def%tr-inherit (from &rest others)
  #+pdp10
  (mapc #'(lambda (name)
	    (enterq name (get (car module-stack) 'tr-props)))
	others)
  `(let ((tr-prop (or (get ',from 'translate)
		      (maxima-error '|No TRANSLATE property to alias.| ',from))))
    (mapc #'(lambda (name) (putprop name tr-prop 'translate))
     ',others)))

#+pdp10
(defun put-lambda-type (name argl)
  (let ((lambda-type (lambda-type argl)))
    (putprop name t (car lambda-type))
    (args name (cdr lambda-type))))


(defmacro deftrfun (name argl &rest body)
  #+pdp10
  (progn (enterq name (get (car module-stack) 'functions))
	 (put-lambda-type name argl))
  `(defun ,name ,argl ,@body))

(defmacro deftrvar (name value &rest ignore-doc) ignore-doc
	  ;; to be used to put the simple default value in
	  ;; the autoload file. Should be generalized to include
	  ;; BINDING methods.
	  #+pdp10
	  (progn (enterq name (get (car module-stack) 'variables))
		 (putprop name (if (fboundp 'macro-expand)
				   (macro-expand value)
				   value)
			  'value))
	  `(defvar ,name ,value))

;;#+PDP10
;;(PROGN 'COMPILE

;;(defun get! (a b) (or (get a b) (get! (maxima-error (list "undefined" b "property")
;;					     a 'wrng-type-arg)
;;				      b)))

;;(defun print-defprop (symbol prop stream)
;;       (print `(defprop ,symbol ,(get symbol prop) ,prop) stream))

;;(defun save-module-info (module stream)
;;  (putprop module `(,(status uname) ,(status dow) ,(status date))
;;	   'last-compiled)
;;  (print-defprop module 'last-compiled stream)
;;  (print-defprop module 'functions stream)
;;  (print-defprop module 'variables stream)
;;  (print-defprop module 'tr-props stream)
;;  (DO ((VARIABLES (get module 'VARIABLES) (CDR VARIABLES)))
;;      ((NULL VARIABLES))
;;    (print-defprop (car variables) 'value stream)
;;    ;; *NB*
;;    ;; this depends on knowing about the internal workings
;;    ;; of the maclisp compiler!!!!
;;    (print `(defprop ,(car variables)
;;	      (special ,(car variables))
;;	      special)
;;	   stream)
;;    )
;;  (DO ((FUNCTIONS (GET MODULE 'FUNCTIONS) (CDR FUNCTIONS)))
;;      ((NULL FUNCTIONS))
;;    ;; *NB* depends on maclisp compiler.
;;    (LET ((X (GETL (CAR FUNCTIONS) '(*LEXPR *EXPR))))
;;      (IF X
;;	  (PRINT-DEFPROP (CAR FUNCTIONS) (CAR X) STREAM)))
;;    (LET ((X (ARGS (CAR FUNCTIONS))))
;;      (IF X
;;	  (PRINT `(ARGS ',(CAR FUNCTIONS) ',X) STREAM)))))

;;(defun save-enable-module-info (module stream)
;;  ;; this outputs stuff to be executed in the context
;;  ;; of RUNTIME of the modules, using information gotten
;;  ;; by the SAVE done by the above function.
;;  (print `(defprop ,module ,(tr-fasl-file-name module) fasload) stream)
;;  ;; FASLOAD property lets us share the TR-FASL-FILE-NAME
;;  ;; amoung the various autoload properties.
;;  (print `(map1-put-if-nil ',(get module 'functions)
;;			   (get ',module 'fasload)
;;			   'autoload)
;;	 stream)
;;  (print `(map1-put-if-nil ',(get module 'tr-props)
;;			   (get ',module 'fasload)
;;			   'autoload-translate)
;;	 stream)
;;  (print `(map1-put-if-nil ',(get module 'tr-props)
;;			   (or (get 'autoload-translate 'subr)
;;			       (maxima-error 'autoload-translate 'subr
;;				      'fail-act))
;;			   'translate)
;;	 stream)
;;  (do ((variables (get module 'variables) (cdr variables)))
;;      ((null variables))
;;    (print `(or (boundp ',(car variables))
;;		(setq ,(car variables) ,(get (car variables) 'value)))
;;	   stream)))

;;(eval-when (compile eval)
;;	   (or (get 'iota 'macro) (load '|liblsp;iota fasl|)))

;;(DEFUN TRANSL-MODULE-DO-IT (&AUX (*print-base* 10.) (*NOPOINT NIL))
;;       (let ((module (CAR MODULE-STACK)))
;;	    (cond ((AND (GET module 'ttime-auto)
;;			(macsyma-compilation-p))
;;		   (iota ((f `((dsk ,(get! module 'dir))
;;			       ,module _auto_) 'out))
;;			 (and ttynotes (format tyo "~&;`module' : ~A~%" `module'))
;;			 (save-module-info module f)
;;			 (renamef f "* AUTOLO"))
;;		   (INSTALL-TRANSL-AUTOLOADS)))))

;;(defun load-module-info (module)
;;       (IF (AND (GET MODULE 'TTIME-AUTO)
;;		;; Assume we are the only MCL compiling
;;		;; a transl module at this time.
;;		(NOT (GET MODULE 'LAST-COMPILED)))
;;	   (LET ((FILE `((dsk ,(get! module 'dir))
;;			 ,module autolo)))
;;		(COND ((PROBE-FILE FILE)
;;		       (AND TTYNOTES
;;			    (FORMAT TYO "~&;Loading ~A info~%"
;;				    file))
;;		       (LOAD FILE))
;;		      (T
;;		       (AND TTYNOTES
;;			    (FORMAT TYO "~&; ~A NOT FOUND~%"
;;				    file)))))))

;;(defvar autoload-install-file "dsk:macsyma;transl autoload")

;;(DEFUN UNAME-TIMEDATE (FORMAT-STREAM)
;;       (LET (((YEAR MONTH DAY) (STATUS DATE))
;;	     ((HOUR MINUTE SECOND) (STATUS DAYTIME)))
;;	    (FORMAT FORMAT-STREAM
;;		    "by ~A on ~A, ~
;;	       ~[January~;February~;March~;April~;May~;June~;July~;August~
;;	       ~;September~;October~;November~;December~] ~
;;	       ~D, 19~D, at ~D:~2,'0D:~2,'0D"
;;		    (status uname)
;;		    (status dow)
;;		    (f1- month) day year
;;		    hour minute second)))

;;(defun install-transl-autoloads ()
;;       (MAPC #'LOAD-MODULE-INFO TRANSL-MODULES)
;;       (iota ((f (mergef "* _TEMP"
;;			 autoload-install-file)
;;		 '(out ascii)))
;;	     (PRINT `(progn
;;		      (DEFPROP TRANSL-AUTOLOAD ,(Uname-timedate nil) VERSION)
;;		      (OR (GET 'TRANSL-AUTOLOAD 'SUBR)
;;			  (load '((dsk macsym)trhook fasl)))
;;		      (setq transl-modules
;;			    ',transl-modules))
;;		    F)
;;	     (DO ((MODULES TRANSL-MODULES (CDR MODULES)))
;;		 ((NULL MODULES)
;;		  (renamef f autoload-install-file))
;;		 (and (get (car modules) 'ttime-auto)
;;		      (save-enable-module-info (car modules) f)))))

;;(defun tr-tagS ()
;;  ;; trivial convenience utility.
;;  (iota ((f `((dsk ,(get 'transl 'dir)) transl ntags) 'out))
;;    (do ((l transl-modules (cdr l)))
;;	((null l)
;;	 (close f)
;;	 (valret
;;	  (symbolconc '|:TAGS | (NAMESTRING F) '|
;; |)))
;;      (or (get (car l) 'pseudo)
;;	  (format f "DSK:~A;~A >~%,LISP~%~%"
;;		  (get! (car l) 'dir) (car l))))))

;;;; end of #+PDP10 I/O code.

;;)

;;; in PDP-10 maclisp OP is a subr-pointer.
;;; system-dependance macro-fied away in PROCS.

(defmacro tprop-call (op form)
  `(subr-call ,op ,form))

(defmacro def-autoload-translate (&rest funs)
  #+pdp10
  `(let ((a-subr (or (get 'autoload-translate 'subr)
		     (maxima-error 'lose 'autoload-translate 'fail-act))))
    (mapc #'(lambda (u)
	      (or (get u 'translate)
		  (putprop u a-subr 'translate)))
     ',funs))
  #-pdp10
  `(comment *autoloading?* ,@funs))


;;; declarations for the TRANSL PACKAGE.

(declare-top 
 (special *transl-sources*)
 ;; The warning an error subsystem.
 (special tr-abort		    ; set this T if you want to abort.
	  *translation-msgs-files*) ; the stream to print messages to.
 (*lexpr warn-undedeclared
	 tr-nargs-check
	 warn-meval
	 warn-mode
	 warn-fexpr
	 tell)
  
 (*lexpr pump-stream			; file hacking
	 )
  
 ;; State variables.
  
 (special pre-transl-forms* ; push onto this, gets output first into the
					; transl file.
	  *warned-un-declared-vars*
	  *warned-fexprs* 
	  *warned-mode-vars*
	  *warned-undefined-vars*
	  warned-undefined-variables
	  tr-abort
	  transl-file
	  *in-compfile*
	  *in-translate-file*
	  *in-translate*
	  *pre-transl-forms* 
	  *new-autoload-entries*     ; new entries created by TRANSL. 
	  *untranslated-functions-called*
	  )
  
 ;; General entry points.
  
 (*expr translate
	;; Takes a macsyma form, returns a form
	;; such that the CAR is the MODE and the
	;; CDR is the equivalent lisp form.
	;; For the meaning of the second argument to TRANSLATE
	;; see the code. When calling TRANSLATE from outside of
	;; itself, the second arg is always left out.
	tr-args		  ; mapcar of translate, strips off the modes.
	dtranslate			; CDR TRANSLATE
	call-and-simp		; (MODE F ARGL) generates `(,F ,@ARGL)
	;; sticks on the mode and a SIMPLIFY if needed.
	array-mode
	function-mode
	value-mode
	tbind			      ; For META binding of variables.
	tunbind				; unbind.
	tunbinds			; a list.
	tboundp			     ; is the variable lexicaly bound?
	teval		 ; get the var replacement. Now this is always
	;; the same as the var itself. BUT it could be use
	;; to do internal-mode stuff.
	 
	push-pre-transl-form
	 
	)
 (*lexpr tr-local-exp
	 ;; conses up a lambda, calls, translate, strips...
	 tr-lambda
	 ;; translate only a standard lambda expression
	 )
  
 (*expr free-lisp-vars
	push-defvar
	tr-trace-exit
	tr-trace-entry
	side-effect-free-check
	tbound-free-vars)
  
 (*expr translate-function tr-mfun dconvx)
  
 ;; these special declarations are for before DEFMVAR
 (special $errexp $loadprint $numer $savedef $nolabels $functions $props 
	  $filename $filenum $direc $device munbound $values $transrun
	  st oldst  $version
	  rephrase $packagefile
	  dskfnp)
  
 ;; end of COMPLR declarations section.
 )

(defmacro bind-transl-state (&rest forms)
  ;; this binds all transl state variables to NIL.
  ;; and binds user-settable variables to themselves.
  ;; $TRANSCOMPILE for example can be set to TRUE while translating
  ;; a file, yet will only affect that file.
  ;; Called in 3 places, for compactness maybe this should be a PROGV
  ;; which references a list of variables?
  `(let (*warned-un-declared-vars*
	 *warned-fexprs* 
	 *warned-mode-vars*
	 *warned-undefined-vars*
	 warned-undefined-variables
	 tr-abort
	 transl-file
	 *in-compfile*
	 *in-translate-file*
	 *in-translate*
	 *pre-transl-forms* 
	 *new-autoload-entries*
	 ($tr_semicompile $tr_semicompile)
	 (arrays nil)
	 (exprs nil)
	 (lexprs nil)
	 (fexprs nil)
	 (specials nil)
	 (declares nil)
	 ($transcompile $transcompile)
	 ($tr_numer $tr_numer)
	 defined_variables)
    ,@forms))



#-(or cl multics)
(defmacro tr-format (string &rest argl)
  `(mformat *translation-msgs-files*
    ,string ,@argl))

;;; Is MFORMAT really prepared in general to handle
;;; the above form. Certainly not on Multics.
#+(and multics (not cl))
(defmacro tr-format (string &rest argl)
  `(cond ((consp *translation-msgs-files*)
	  (mapcar #'(lambda (file)
		      (mformat file ,string ,@argl))
	   *translation-msgs-files*))
    (t (mformat *translation-msgs-files* ,string ,@argl))))

#+cl
(defun tr-format (sstring &rest argl &aux strs)
  (cond ((consp *translation-msgs-files*)(setq strs *translation-msgs-files*))
	(t (setq strs (list *translation-msgs-files*))))
  (loop for v in strs
	 do (apply 'mformat v sstring argl)))

 
;;; for debugging convenience:
;;(DEFMACRO TR (EXP) `(BIND-TRANSL-STATE (TRANSLATE ,EXP)))

;; to use in mixing maxima and lisp
;; (tr #$$f(x):=x+2$)
(defmacro tr (u)
  (and (consp u) (eq (car u) 'quote)
       (bind-transl-state (translate-macexpr-toplevel  (second u)))))


;;; These are used by MDEFUN and MFUNCTION-CALL.
;;; N.B. this has arguments evaluated twice because I am too lazy to
;;; use a LET around things.

(defmacro push-info (name info stack)
  `(let ((*info* (assq ,name ,stack)))
    (cond (*info* ;;; should check for compatibility of INFO here.
	   )
	  (t
	   (push (cons ,name ,info) ,stack)))))

(defmacro get-info (name stack)
  `(cdr (assq ,name ,stack)))

(defmacro pop-info (name stack)
  `(let ((*info* (assq ,name ,stack)))
    (cond (*info*
	   (setq ,stack (zl-delete *info* ,stack))
	   (cdr *info*))
	  (t nil))))

(defmacro top-ind (stack)
  `(cond ((null ,stack) nil)
    (t
     (caar ,stack))))



#+cl
(defmacro maset ( val ar  &rest inds)
  `(progn
     (cond ((symbolp ,ar)
	    (setf ,ar (make-equal-hash-table
		       ,(if (cdr inds) t nil)))))
    (maset1 ,val  ,ar ,@  inds)))


;;#+lispm  ;;removed the apply from tr-arraycall and &rest.
;;(defun tr-maref (ar &rest inds)
;;    `(nil maref ,ar ,@ (copy-list inds)))



(defmacro maref (ar &rest inds)
  (cond ((or (eql ar 'mqapply)(and (consp ar) (memq 'mqapply ar)))
         `(marrayref ,(first inds) ,@ (cdr inds)))
	((consp ar)`(marrayref ,ar ,(first inds) ,@ (cdr inds)))
	(t
	 `(maref1  ,ar,@ inds))))



