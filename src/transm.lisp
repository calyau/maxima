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
  `(or (member ,thing ,list :test #'eq)
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
(def-transl-module fcall no-load-auto)
(def-transl-module acall no-load-auto)
(def-transl-module trdata no-load-auto)
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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (load-macsyma-macros procs))

(defmacro transl-module (name)
  (unless (member name transl-modules :test #'eq)
    (maxima-error "Not a `transl-module', see TRANSM")))

(def-def-property translate (form))

(defmacro def%tr (name lambda-list &body body &aux definition)
  (setq definition
	(if (and (null body) (symbolp lambda-list))
	    `(def-same%tr ,name ,lambda-list)
	    `(defun-prop (,name translate) ,lambda-list ,@body)))
  `(eval-when (:compile-toplevel :execute :load-toplevel)
    ,definition))

(defmacro def-same%tr (name same-as)
  ;; right now MUST be used in the SAME file.
  `(putprop ',name
    (or (get ',same-as 'translate)
     (maxima-error "No TRANSLATE property to alias. ~a" ',same-as))
    'translate))

(defmacro def%tr-inherit (from &rest others)
  `(let ((tr-prop (or (get ',from 'translate)
		      (maxima-error "No TRANSLATE property to alias. ~a" ',from))))
    (mapc #'(lambda (name) (putprop name tr-prop 'translate))',others)))

(defmacro deftrfun (name argl &rest body)
  `(defun ,name ,argl ,@body))

(defmacro deftrvar (name value &rest ignore-doc) ignore-doc
	  ;; to be used to put the simple default value in
	  ;; the autoload file. Should be generalized to include
	  ;; BINDING methods.
	  `(defvar ,name ,value))

(defmacro tprop-call (op form)
  `(subr-call ,op ,form))

(defmacro def-autoload-translate (&rest funs)
  `(comment *autoloading?* ,@funs))


;;; declarations for the TRANSL PACKAGE.

(declare-top (special *transl-sources*)
	     ;; The warning and error subsystem.
	     (special tr-abort	    ; set this T if you want to abort.
		      *translation-msgs-files*)	; the stream to print messages to.
	     ;; State variables.
	     (special pre-transl-forms*	; push onto this, gets output first into the transl file.
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
		      *new-autoload-entries* ; new entries created by TRANSL.
		      *untranslated-functions-called*)


	     ;; these special declarations are for before DEFMVAR
	     (special $errexp $loadprint $numer $savedef $nolabels $functions $props
		      $filename $filenum $direc $device munbound $values $transrun
		      st oldst  $version
		      rephrase $packagefile
		      dskfnp))

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

(defun tr-format (sstring &rest argl &aux strs)
  (if (consp *translation-msgs-files*)
      (setq strs *translation-msgs-files*)
      (setq strs (list *translation-msgs-files*)))
  (loop for v in strs
	do (apply #'mformat v sstring argl)))

;; to use in mixing maxima and lisp
;; (tr #$$f(x):=x+2$)
(defmacro tr (u)
  (and (consp u)
       (eq (car u) 'quote)
       (bind-transl-state (translate-macexpr-toplevel (second u)))))

;;; These are used by MDEFUN and MFUNCTION-CALL.
;;; N.B. this has arguments evaluated twice because I am too lazy to
;;; use a LET around things.

(defmacro push-info (name info stack)
  `(let ((*info* (assoc ,name ,stack :test #'eq)))
    (cond (*info* ;;; should check for compatibility of INFO here.
	   )
	  (t
	   (push (cons ,name ,info) ,stack)))))

(defmacro get-info (name stack)
  `(cdr (assoc ,name ,stack :test #'eq)))

(defmacro pop-info (name stack)
  `(let ((*info* (assoc ,name ,stack :test #'eq)))
    (cond (*info*
	   (setq ,stack (delete *info* ,stack :test #'equal))
	   (cdr *info*))
	  (t nil))))

(defmacro top-ind (stack)
  `(cond ((null ,stack) nil)
    (t (caar ,stack))))

(defmacro maset (val ar &rest inds)
  `(progn
    (when (symbolp ,ar)
      (setf ,ar (make-equal-hash-table ,(if (cdr inds) t nil))))
    (maset1 ,val ,ar ,@inds)))

(defmacro maref (ar &rest inds)
  (cond ((or (eql ar 'mqapply)(and (consp ar) (member 'mqapply ar :test #'eq)))
         `(marrayref ,(first inds) ,@(cdr inds)))
	((consp ar)`(marrayref ,ar ,(first inds) ,@(cdr inds)))
	(t
	 `(maref1 ,ar ,@inds))))

 	  	 
