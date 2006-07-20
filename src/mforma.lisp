;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module mforma macro)

;;#+ti
;;(eval-when (compile)
;;  (load "cl-maxima-source:maxima;mforma.lisp"))


;;; A mini version of FORMAT for macsyma error messages, and other
;;; user interaction.
;;; George J. Carrette - 10:59am  Tuesday, 21 October 1980

;;; This file is used at compile-time for macsyma system code in general,
;;; and also for MAXSRC;MFORMT > and MAXSRC;MERROR >.
;;; Open-coding of MFORMAT is supported, as are run-time MFORMAT string
;;; interpretation. In all cases syntax checking of the MFORMAT string
;;; at compile-time is done.

;;; For the prettiest output the normal mode here will be to
;;; cons up items to pass as MTEXT forms.

;;; Macro definitions for defining a format string interpreter.
;;; N.B. All of these macros expand into forms which contain free
;;; variables, i.e. they assume that they will be expanded in the
;;; proper context of an MFORMAT-LOOP definition. It's a bit
;;; ad-hoc, and not as clean as it should be.
;;; (Macrofy DEFINE-AN-MFORMAT-INTERPRETER, and give the free variables
;;; which are otherwise invisible, better names to boot.)

;;; There are 3 definitions of MFORMAT.
;;; [1] The interpreter.
;;; [2] The compile-time syntax checker.
;;; [3] The open-compiler.

;; Some commentary as to what the hell is going on here would be greatly
;; appreciated.  This is probably very elegant code, but I can't figure
;; it out. -cwh
;; This is macros defining macros defining function bodies man.
;; top-level side-effects during macroexpansion consing up shit
;; for an interpreter loop. I only do this to save address space (sort of
;; kidding.) -gjc

;;#-ti
;;(DEFMACRO DEF-MFORMAT (&OPTIONAL  #-ti (type '||)
;;		       #+ti (TYPE  (intern "")))
;;  ;; Call to this macro at head of file.
;;  (PUTPROP TYPE NIL 'MFORMAT-OPS)
;;  (PUTPROP TYPE NIL 'MFORMAT-STATE-VARS)
;;  `(PROGN 'COMPILE
;;	  (DEFMACRO ,(SYMBOLCONC 'DEF-MFORMAT-OP TYPE)
;;	    (CHAR &REST BODY)
;;	    `(+DEF-MFORMAT-OP ,',TYPE ,CHAR ,@BODY))
;;	  (DEFMACRO ,(SYMBOLCONC 'DEF-MFORMAT-VAR TYPE)
;;	    (VAR VAL INIT)
;;	    `(+DEF-MFORMAT-VAR ,',TYPE ,VAR ,VAL ,INIT))
;;	  (DEFMACRO ,(SYMBOLCONC 'MFORMAT-LOOP TYPE)
;;	    (&REST ENDCODE)
;;	    `(+MFORMAT-LOOP ,',TYPE ,@ENDCODE))))



(defmacro def-mformat (&optional  
		       (type  (intern "")))
  ;; Call to this macro at head of file.
  (putprop type nil 'mformat-ops)
  (putprop type nil 'mformat-state-vars)
  `(eval-when (compile load eval)
    (defmacro ,(symbolconc 'def-mformat-op type)
	(char &rest body)
      `(+def-mformat-op ,',type ,char ,@body))
    (defmacro ,(symbolconc 'def-mformat-var type)
	(var val init)
      `(+def-mformat-var ,',type ,var ,val ,init))
    (defmacro ,(symbolconc 'mformat-loop type)
	(&rest endcode)
      `(+mformat-loop ,',type ,@endcode))))


(defmacro +def-mformat-var (type var val init-condition)
  (let ()
    ;; How about that bullshit LISPM conditionalization put in
    ;; by BEE? It is needed of course or else conses will go away. -gjc
    (push (list var val)
	  (cdr (or (assoc init-condition
			  (get type 'mformat-state-vars) :test #'equal)
		   (car (push (ncons init-condition)
			      (get type 'mformat-state-vars)))))))
  `',var)

(defmacro +def-mformat-op (type char &rest body)
					; can also be a list of CHAR's
  (let ()
    (if (atom char) (setq char (list char)))
    (push (cons char body) (get type 'mformat-ops))
    `',(maknam (nconc (exploden (symbol-name '#:mformat-))
		      (mapcar #'ascii char)))))

(defmacro pop-mformat-arg ()
  `(cond ((= arg-index n)
	  (maxima-error "Ran out of mformat args" (listify n) 'fail-act))
    (t (progn (setq arg-index (f1+ arg-index))
	      (arg arg-index)))))

(defmacro leftover-mformat-args? ()
  ;; To be called after we are done.
  '(or (= arg-index n)
    (maxima-error "Extra mformat args" (listify n) 'fail-act)))

(defmacro bind-mformat-state-vars (type &rest body)
  `(let ,(do ((l nil)
	      (v (get type 'mformat-state-vars) (cdr v)))
	     ((null v) l)
	     (do ((conds (cdr (car v)) (cdr conds)))
		 ((null conds))
	       (push (car conds) l)))
    ,@body))

(defmacro pop-mformat-string ()
  '(if (null sstring) 
    (maxima-error "Runout of `mformat' string" nil 'fail-act)
    (pop sstring)))

(defmacro null-mformat-string () '(null sstring))
(defmacro top-mformat-string ()
  '(if (null sstring)
    (maxima-error "Runout of `mformat' string" nil 'fail-act)
    (car sstring)))

(defmacro cdr-mformat-string ()
  `(setq sstring (cdr sstring)))

(defmacro mformat-dispatch-on-char (type)
  `(progn (cond ,@(mapcar #'(lambda (pair)
			      `(,(if (atom (car pair))
				     `(char-equal char ,(car pair))
				     `(or-1 ,@(mapcar
					       #'(lambda (c)
						   `(char-equal char,c))
					       (car pair))))
				,@(cdr pair)))
			  (get type 'mformat-ops))
		;; perhaps optimize the COND to use ">" "<".
		(t
;		 (maxima-error "Unknown format op." (ascii char) 'fail-act)))
		 (maxima-error (format nil "Unknown format op. _~a_" ',type) (ascii char) 'fail-act)))
    ,@(mapcar #'(lambda (state)
		  `(if ,(car state)
		    (setq ,@(apply #'append (cdr state)))))
	      (get type 'mformat-state-vars))))

(defmacro or-1 (first &rest rest)
  ;; So the style warnings for one argument case to OR don't
  ;; confuse us.
  (if (null rest) first `(or ,first ,@rest)))

;;(DEFMACRO WHITE-SPACE-P (X)
;;  `(zl-MEMBER ,X '(#\LINEFEED #\Return #\SPACE #\TAB #-lispm #\VT #\Page)))

(defmacro white-space-p (x)
  `(member ,x '(#\linefeed #\return #\space #\tab #\page #-(or clisp gcl openmcl) #\vt #+clisp #\code11)
    :test #'char=))


(defmacro +mformat-loop (type &rest end-code)
  `(bind-mformat-state-vars
    ,type
    (do ((char))
	((null-mformat-string)
	 (leftover-mformat-args?)
	 ,@end-code)
      (setq char (pop sstring))
      (cond ((char= char #\~)
	     (do ()
		 (nil)
	       (setq char (pop-mformat-string))
	       (cond ((char= char #\@)
		      (setq |@-FLAG| t))
		     ((char= char #\:)
		      (setq |:-FLAG| t))
		     ((char= char #\~)
		      (push char text-temp)
		      (return nil))
		     ((white-space-p char)
		      (do ()
			  ((not (white-space-p (top-mformat-string))))
			(cdr-mformat-string))
		      (return nil))
		     ((or (char< char #\0) (char> char #\9))
		      (mformat-dispatch-on-char ,type)
		      (return nil))
		     (t
		      (setq parameter
			    (f+ (f- (char-code char) (char-code #\0))
				(f* 10. parameter))
			    parameter-p t)))))

	    (t
	     (push char text-temp))))))

;;; The following definitions of MFORMAT ops are for compile-time,
;;; the runtime definitions are in MFORMT.

(defvar want-open-compiled-mformat nil)
(defvar cant-open-compile-mformat nil)


(def-mformat -c)

	 
(def-mformat-var-c |:-FLAG|     nil t)
(def-mformat-var-c |@-FLAG|     nil t)
(def-mformat-var-c parameter   0  t) 
(def-mformat-var-c parameter-p nil t)
(def-mformat-var-c text-temp nil nil)
(def-mformat-var-c code nil nil)

(defmacro emitc (x)
  `(push ,x code))

(defmacro push-text-temp-c ()
  '(and text-temp
    (progn (emitc `(princ ',(maknam (nreverse text-temp)) ,stream))
	   (setq text-temp nil))))

(def-mformat-op-c (#\% #\&)
    (cond (want-open-compiled-mformat
	   (push-text-temp-c)
	   (if (char= char #\&)
	       (emitc `(cursorpos 'a ,stream))
	       (emitc `(terpri ,stream))))))

(def-mformat-op-c #\M
    (cond (want-open-compiled-mformat
	   (push-text-temp-c)
	   (emitc `(,(if |:-FLAG| 'mgrind 'displaf)
		    (,(if |@-FLAG| 'getop 'progn)
		     ,(pop-mformat-arg))
		    ,stream)))
	  (t (pop-mformat-arg))))

(def-mformat-op-c (#\A #\S)
    (cond (want-open-compiled-mformat
	   (push-text-temp-c)
	   (emitc `(,(if (char-equal char #\A) 'princ 'prin1)
		    ,(pop-mformat-arg)
		    ,stream)))
	  (t (pop-mformat-arg))))

(defun optimize-print-inst (l)
  ;; Should remove extra calls to TERPRI around DISPLA.
  ;; Mainly want to remove (PRINC FOO NIL) => (PRINC FOO)
  ;; although I'm not sure this is correct. geezz.
  (do ((new nil))
      ((null l) `(progn ,@new))
    (let ((a (pop l)))
      (cond ((eq (car a) 'terpri)
	     (cond ((eq (cadr a) nil)
		    (push '(terpri) new))
		   (t (push a new))))
	    ((and (eq (caddr a) nil)
		  (not (eq (car a) 'mgrind)))
	     (cond ((eq (car a) 'displaf)
		    (push `(displa ,(cadr a)) new))
		   (t
		    (push `(,(car a) ,(cadr a)) new))))
	    (t
	     (push a new))))))

(defmacro normalize-stream (stream)
  stream
  nil)

(defmfun mformat-translate-open n
  (let ((stream (arg 1))
	(sstring (exploden (arg 2)))
	(want-open-compiled-mformat t)
	(cant-open-compile-mformat nil)
	(arg-index 2))
    (normalize-stream stream)
    (mformat-loop-c
     (progn (push-text-temp-c)
	    (if cant-open-compile-mformat
		(maxima-error "Can't open compile `mformat' on this case."
			      (listify n)
			      'fail-act
			      ))
	    (optimize-print-inst code)))))

(defmfun mformat-syntax-check n
  (let ((arg-index 2)
	(stream nil)
	(sstring (exploden (arg 2)))
	(want-open-compiled-mformat nil))
    (mformat-loop-c nil)))


(defmacro progn-pig (&rest l) `(progn ,@l))

(defun process-message-argument (x)
  ;; Return NIL if we have already processed this
  ;; message argument, NCONS of object if not
  ;; processed.
  (if (and (not (atom x))
	   (memq (car x) '(out-of-core-string progn-pig)))
      nil
      (ncons (if (and (stringp x) (status feature its))
		 `(out-of-core-string ,x)
		 `(progn-pig ,x)))))

(defun mformat-translate (arguments compiling?)
  (destructuring-let (((stream sstring . other-shit) arguments))
    (let ((mess (process-message-argument sstring)))
      (cond ((null mess) nil)
	    ('on-the-other-hand
	     (setq mess (car mess))
	     (normalize-stream stream)
	     (if (and (stringp sstring) compiling?)
		 (apply #'mformat-syntax-check
			stream sstring other-shit))
	     `(,(or (cdr (assoc (f+ 2	; two leading args.
				    (length other-shit))
				'((2 . *mformat-2)
				  (3 . *mformat-3)
				  (4 . *mformat-4)
				  (5 . *mformat-5)) :test #'equal))
		    'mformat)
	       ,stream
	       ,mess
	       ,@other-shit))))))

(defun mtell-translate (arguments compiling?)
  (destructuring-let (((sstring . other-shit) arguments))
    (let ((mess (process-message-argument sstring)))
      (cond ((null mess) nil)
	    ('on-the-other-hand
	     (setq mess (car mess))
	     (if (and (stringp sstring) compiling?)
		 (apply #'mformat-syntax-check
			nil sstring other-shit))
	     `(,(or (cdr (assoc (1+ (length other-shit))
				'((1 . mtell1)
				  (2 . mtell2)
				  (3 . mtell3)
				  (4 . mtell4)
				  (5 . mtell5)) :test #'equal))
		    'mtell)
	       ,mess
	       ,@other-shit))))))

(defmacro mformat-open (stream sstring &rest other-shit)
  (if (not (stringp sstring))
      (maxima-error "Not a string, can't open-compile the `mformat' call"
		    sstring 'fail-act)
      (apply #'mformat-translate-open
	     stream
	     sstring
	     other-shit)))

(defmacro mtell-open (message &rest other-shit)
  `(mformat-open nil ,message . ,other-shit))

(defun merror-translate (arguments compiling?)
  (destructuring-let (((message . other-shit) arguments))
    (let ((mess (process-message-argument message)))
      (cond ((null mess) nil)
	    ('on-the-other-hand
	     (if (and (stringp message) compiling?)
		 (apply #'mformat-syntax-check
			nil
			message other-shit))
	     (setq mess (car mess))
	     `(,(or (cdr (assoc (1+ (length other-shit))
				'((1 . *merror-1)
				  (2 . *merror-2)
				  (3 . *merror-3)
				  (4 . *merror-4)
				  (5 . *merror-5)) :test #'equal))
		    'merror)
	       ,mess
	       ,@other-shit))))))

(defun errrjf-translate (arguments compiling?)
  (destructuring-let (((message . other-shit) arguments))
    (let ((mess (process-message-argument message)))
      (cond ((null mess) nil)
	    ('on-the-other-hand
	     (if (and (stringp message) compiling?)
		 (apply #'mformat-syntax-check
			nil
			message other-shit))
	     (setq mess (car mess))
	     `(,(or (cdr (assoc (1+ (length other-shit))
				'((1 . *errrjf-1)) :test #'equal))
		    'errrjf)
	       ,mess ,@other-shit))))))
;;#+PDP10
;;(PROGN 'COMPILE

;;(DEFUN GET-TRANSLATOR (OP)
;;  (OR (GET OP 'TRANSLATOR)
;;      (GET-TRANSLATOR (MAXIMA-ERROR "has no translator" OP 'wrng-type-arg))))

;;(DEFVAR SOURCE-TRANS-DRIVE NIL)
;;(DEFUN SOURCE-TRANS-DRIVE (FORM)
;;  (LET ((X (FUNCALL (GET-TRANSLATOR (CAR FORM)) (CDR FORM) T)))
;;    (WHEN (AND X SOURCE-TRANS-DRIVE)
;;	  (PRINT FORM TYO)
;;	  (PRINC "==>" TYO)
;;	  (PRINT X TYO))
;;    (IF (NULL X) (VALUES FORM NIL) (VALUES X T))))
;;(DEFUN PUT-SOURCE-TRANS-DRIVE (OP TR)
;;  (PUTPROP OP '(SOURCE-TRANS-DRIVE) 'SOURCE-TRANS)
;;  (PUTPROP OP TR 'TRANSLATOR))

;;(PUT-SOURCE-TRANS-DRIVE 'MFORMAT 'MFORMAT-TRANSLATE)
;;(PUT-SOURCE-TRANS-DRIVE 'MTELL 'MTELL-TRANSLATE)
;;(PUT-SOURCE-TRANS-DRIVE 'MERROR 'MERROR-TRANSLATE)
;;(PUT-SOURCE-TRANS-DRIVE 'ERRRJF 'ERRRJF-TRANSLATE)
;;)

;;; Other systems won't get the syntax-checking at compile-time
;;; unless we hook into their way of doing optimizers.

