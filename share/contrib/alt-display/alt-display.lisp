;; -*- mode: lisp -*-
;; Copyright Leo Butler (l_butler@users.sourceforge.net) 2013
;; Released under the terms of GPLv2+
(in-package :maxima)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro massert (assertion vars &rest action)
    `(handler-bind ((error (lambda(msg) (merror "~a" msg))))
       (assert ,assertion ,vars ,@action))))

(defun $alt_display_output_type (&optional (form nil))
  (massert (and (listp form) (car form) (listp (car form)) (caar form))
	   (form)
	   ($printf nil "alt_display_output_type(form): form is ill-structured, found: ~a." form))
  (case (caar form)
    (mlabel '$label)
    (mtext  '$text)
    (t      '$unknown)))

(defvar *set-prompt-alist*
  '(($prefix	. *prompt-prefix*)
    ($suffix	. *prompt-suffix*)
    ($general	. *general-display-prefix*)
    ($prolog	. *maxima-prolog*)
    ($epilog	. *maxima-epilog*))
  "An alist of options to set_prompt, and the corresponding Lisp special variables.")

(defun $set_prompt (type value &rest args)
  (declare (special *prompt-prefix* *prompt-suffix* *general-display-prefix* *maxima-prolog* *maxima-epilog* *set-prompt-alist*))
  (massert (and (symbolp type) (member type '($prefix $suffix $general $prolog $epilog))
	       (or (stringp value) (null value)))
	   (type value)
	   ($printf nil "set_prompt(type, value): type must be one of prefix, suffix, general, prolog or epilog; value must be a string or false.~%type=~a value=~a" type value))
  (set (cdr (assoc type *set-prompt-alist*)) (or value ""))
  (cond ((null args)
	 '$done)
	((and args (listp args) (> (length args) 1))
	 (apply '$set_prompt args))
	(t
	 (merror "set_prompt(type,value): missing value."))))

(defun $reset_prompts ()
  (declare (special *set-prompt-alist*))
  (dolist (v *set-prompt-alist* '$done)
    ($set_prompt (car v) (or (gethash (cdr v) *variable-initial-values*) ""))))

(defun $set_alt_display (type &optional (f nil) (warn nil))
  (declare (special *alt-display1d* *alt-display2d*))
  (massert (and (member type '(1 2))
		(or (and (symbolp f) (or (fboundp f) (mfboundp f)))
		    (and (consp f) (member (caar f) '(lambda mfexpr)))
		    (null f)))
	   (type f)
	   "set_alt_display(type,f): type must equal 1 or 2, f must be a function, lambda expression or false.")
  (let* ((alt-display (ecase type
			(1 '*alt-display1d*)
			(2 '*alt-display2d*))))
    (cond (f
	   (if warn (warn "Setting ~dd-display to ~(~a~)." type ($printf nil "~a" f)))
	   (let ((error? t))
	     (labels ((error-handler ()
				     (merror "Error in ~a.~%Message: ~a~a reset to default." alt-display
					     (with-output-to-string (*standard-output*) ($errormsg)) alt-display))
		      (alt-display-fun (form)
				       ;; convert maxima errors to maxima-$error conditions (a type of error)
				       ;; we can't use handler case to forcibly reset *alt-display[12]d*, because of dynamic scope
				       ;; so we manually keep track of whether an error occurs.
				       (ignore-errors
					 (with-$error (mfuncall f form))
					 (setq error? nil))
				       (when error?
					 ($set_alt_display type nil)
					 (error-handler))))
	       (set alt-display #'alt-display-fun))))
	  (t
	   (if warn (warn "Resetting ~dd-display to default." type))
	   (set alt-display nil))))
  '$done)

;; end of alt-display.lisp
