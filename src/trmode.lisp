;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gjc: 6:27pm  sunday, 20 july 1980
;;;       (c) copyright 1979 massachusetts institute of technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")


(macsyma-module trmode)


(transl-module trmode)
(defmvar $mode_checkp t "if true, modedeclare checks the modes of bound variables.")
(defmvar $mode_check_warnp t "if true, mode errors are described.")
(defmvar $mode_check_errorp nil "if true, modedeclare calls error.")

(defmvar $macsyma_extend_warnp t "if true,
warning given about not-built-in modes being taken for Maxima EXTEND types.")

(defun mseemingly-unbound (x)
  (or (not (boundp x)) (eq (symbol-value x) x)))

(defmfun assign-mode-check (var value)
  (let ((mode (get var 'mode))
	(user-level ($get var '$value_check)))
    (if mode
	(let (($mode_check_warnp t)
	      ($mode_check_errorp t))
	  (chekvalue var mode value)))
    (if user-level
	(mcall user-level value)))
  value)

(deftrvar defined_variables ())

(deftrvar $define_variable ())

(def%tr $define_variable (form)	;;VAR INIT MODE.
  (cond ((> (length form) 3)
	 (destructuring-let (((var val mode) (cdr form)))
	   (let ((spec-form `(($declare) ,var $special))
		 (mode-form `(($modedeclare) ,var ,mode)))
	     (translate spec-form)
	     (translate mode-form)
	     (push-pre-transl-form
	      ;; POSSIBLE OVERKILL HERE
	      `(declare (special ,var)))
	     (push var defined_variables)
	     ;; Get rid of previous definitions put on by
	     ;; the translator.
	     (do ((l *pre-transl-forms* (cdr l)))
		 ((null l))
	       ;; REMOVE SOME OVERKILL
	       (cond ((and (eq (caar l) 'def-mtrvar)
			   (eq (cadar l) var))
		      (setq *pre-transl-forms*
			    (delq (car l) *pre-transl-forms*)))))
	     (if (not (eq mode '$any))
		 ;; so that the rest of the translation gronks this.
		 (putprop var 'assign-mode-check 'assign))
	     `($any . (eval-when (compile eval load)
			(meval* ',mode-form)
			(meval* ',spec-form)
			,(if (not (eq mode '$any))
			     `(defprop ,var
			       assign-mode-check
			       assign))
			(def-mtrvar ,(cadr form)
			    ,(dtranslate (caddr form))
			  )))
	     )))
	(t
	 (tr-tell "Wrong number of arguments" form)
	 nil)))

#-cl
;; Not needed on LISPM because the MACRO definition is in effect.
;; For NIL we must do some fexpr abstraction anyway.
(defun def-mtrvar fexpr (l)
       (let (((v a . ignore-crufty) l))
	 ;; priority of setting is obsolete, but must be around for
	 ;; old translated files. i.e. TRMODE version < 69.
	 (if (mseemingly-unbound v)
	     (set v (eval a))
	     (symbol-value v))))

;; the priority fails when a DEF-MTRVAR is done, then the user
;; sets the variable, because the set-priority stays the same.
;; This causes some Define_Variable's to over-ride the user setting,
;; but only in the case of re-loading, what we were worried about
;; is pre-setting of variables of autoloading files.

(defmspec $define_variable  (l) (setq l (cdr l))
	  (or (> (length l) 2)
	      (merror "Wrong number of arguments to `define_variable'"))
	  (or (symbolp (car l))
	      (merror "First arg to `define_variable' not a symbol."))
	  (meval `(($modedeclare) ,(car l) ,(caddr l)))
	  (meval `(($declare) ,(car l) $special))
	  (if (not (eq (caddr l) '$any))
	      (putprop (car l) 'assign-mode-check 'assign))
	  (if (mseemingly-unbound (car l))
	      (meval `((msetq) ,(car l) ,(cadr l)))
	      (meval (car l))))


(defmspec $mode_identity (l) (setq l (cdr l))
	  (or (= (length l) 2) (merror "`mode_identity' takes 2 arguments."))
	  (let* ((obj (cadr l)) (v (meval obj)))
	    (chekvalue obj (ir-or-extend (car l)) v)
	    v))


(def%tr $mode_identity (form)
  `(,(ir-or-extend (cadr form)) . ,(dtranslate (caddr form))))

(defun ir-or-extend (x)
  (let ((built-in-type (case x
			 (($float $real $floatp $flonum $floatnum) '$float)
			 (($fixp $fixnum $integer) '$fixnum)
			 (($rational $rat) '$rational)
			 (($number $bignum $big) '$number)
			 (($boolean $bool) '$boolean)
			 (($list $listp) '$list)
			 ($complex '$complex)
			 (($any $none $any_check) '$any))))
    (if built-in-type built-in-type
	(prog1 x
	  (if $macsyma_extend_warnp
	      (mtell
	       "Warning: ~M is not a built-in type; assuming it is a Maxima extend type" x))))))

(def%tr $modedeclare (form)
  (do ((l (cdr form) (cddr l))) ((null l))
    (declmode (car l) (ir-or-extend (cadr l)) t)))

(defmfun ass-eq-ref n
  (let ((val (assq (arg 2) (arg 1))))
    (if val (cdr val)
	(if (= n 3) (arg 3) nil))))

(defmfun ass-eq-set (val table key)
  (let ((cell (assq key table)))
    (if cell (setf (cdr cell) val)
	(push (cons key val) table)))
  table)


;;; Possible calls to MODEDECLARE.
;;; MODEDECLARE(<oblist>,<mode>,<oblist>,<mode>,...)
;;; where <oblist> is:
;;; an ATOM, signifying a VARIABLE.
;;; a LIST, giving a list of objects of <mode>
;;;

(defmspec $modedeclare (x) (setq x (cdr x))
	  (if (oddp (length x))
	      (merror "`mode_declare' takes an even number of arguments."))
	  (do ((l x (cddr l)) (nl))
	      ((null l) (cons '(mlist) (nreverse nl)))
	    (declmode (car l) (ir-or-extend (cadr l)) nil)
	    (setq nl (cons (car l) nl))))

(defun tr-declare-varmode (variable mode)
  (declvalue variable (ir-or-extend mode) t))

;;; If TRFLAG is TRUE, we are in the translator, if NIL, we are in the
;;; interpreter.
(declare-top (special trflag mode form))
(defun declmode (form mode trflag)
  (cond ((atom form)
	 (declvalue form mode trflag)
	 (and (not trflag) $mode_checkp (chekvalue form mode)))
	((eq 'mlist (caar form))
	 (mapc #'(lambda (l)
		   (declmode l mode trflag))
	       (cdr form)))
	((memq 'array (cdar form))
	 (declarray (caar form) mode))
	((eq '$function (caar form))
	 (mapc #'(lambda (l)
		   (declfun l mode))
	       (cdr form)))
	((memq (caar form) '($fixed_num_args_function
			     $variable_num_args_function))
	 (mapc #'(lambda (f)
		   (declfun f mode)
		   (mputprop f t (caar form)))
	       (cdr form)))
	((eq '$completearray (caar form))
	 (mapc #'(lambda (l)
		   (putprop (cond ((atom l) l)
				  (t (caar l)))
			    mode 'array-mode))
	       (cdr form)))
	((eq '$array (caar form))
	 (mapc #'(lambda (l) (mputprop l mode 'array-mode)) (cdr form)))
	((eq '$arrayfun (caar form))
	 (mapc #'(lambda (l) (mputprop l mode 'arrayfun-mode)) (cdr form)))
	(t
	 (declfun (caar form) mode))))
(declare-top (unspecial trflag mode form))

(deftrfun declvalue (v mode trflag)
  (if trflag (setq v (teval v)))
  (add2lnc v $props)
  (putprop v mode 'mode))


(defmfun chekvalue (v mode
		      &optional
		      (val (meval1 v) val-givenp))
  (cond ((or val-givenp (not (eq v val)))
					; hack because macsyma PROG binds variable
					; to itself. 
	 (let ((checker (assq mode `(($float . floatp)
				     ($fixnum . integerp)
				     ($number . numberp)
				     ($list . $listp)
				     ($boolean . ,#'(lambda (u)
 						      (memq u '(t nil)))))))
	       (nchecker (assq mode '(($float . $real)
				      ($fixnum . $integer)
				      ($complex . $complex))))
					;(extend-type nil) ;($extendp val))
	       (not-done t))
	   (if (cond			;(extend-type
		 ;;		      (cond ((eql mode '$any) nil)
		 ;;			    (t (not (eql mode extend-type)))))
		 ((and checker
		       (not (funcall (cdr checker) val))
		       (if nchecker
			   (prog1
			       (not (mfuncall '$featurep val (cdr nchecker)))
			     (setq not-done nil))
			   t)))
		 ((if not-done (and nchecker (not (mfuncall '$featurep val (cdr nchecker)))))))
	       (signal-mode-error v mode val))))))


(defun signal-mode-error (object mode value)
  (cond ((and $mode_check_warnp
	      (not $mode_check_errorp))
	 (mtell "Warning: ~:M was declared mode ~:M, has value: ~M"
		object mode value))
	($mode_check_errorp
	 (merror "Error: ~:M was declared mode ~:M, has value: ~M"
		 object mode value))))
			  
(defun put-mode (name mode type)
  (if (get name 'tbind)
      (setf (get name 'val-modes)
	    (ass-eq-set mode (get name 'val-modes) type))
      (setf (get name type) mode)))

(defun declarray (ar mode)
  (put-mode ar mode 'array-mode))

(defun declfun (f mode) (put-mode f mode 'function-mode))

;;; 1/2 is not $RATIONAL. bad name. it means CRE form.

(defun ir (x)
  (case x
    (($float $real $floatp $flonum $floatnum) '$float)
    (($fixp $fixnum) '$fixnum)
    (($rational $rat) '$rational)
    (($number $bignum $big) '$number)
    (($boolean $bool) '$boolean)
    (($list $listp) '$list)
    (($any $none $any_check) '$any)
    (t (udm-err x) x)))

(defun udm-err (mode)
  (mtell "Warning:  ~:M is not a known mode declaration ~
maybe you want ~:M mode.~%"
	 mode
	 (case mode
	   (($integer $integerp) '$fixnum)
	   (($complex) "&to ask about this")
	   (($fucked $shitty) "&to watch your language")
	   (t "&to see the documentation on"))))

(defmfun fluidize (variable)
  (mapc #'(lambda (v) (or (boundp v) (set v ())))
	;; what a sorry crock to have all these switches.
	'(*in-compile*
	  *in-compfile*
	  *in-translate*
	  *in-translate-file*))

  (putprop variable t 'special)
  (if (and $transcompile
	   (or *in-compile*
	       *in-compfile*
	       *in-translate*
	       *in-translate-file*))
      (addl variable specials)))

(defmspec $bind_during_translation (form)
  (mevaln (cddr form)))


