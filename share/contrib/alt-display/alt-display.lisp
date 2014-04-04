;; -*- mode: lisp -*-
;; Copyright Leo Butler (l_butler@users.sourceforge.net) 2013
;; Released under the terms of GPLv2+
(in-package :maxima)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro massert (assertion vars &rest action)
    `(handler-bind ((error (lambda(msg) (merror "~a" msg))))
       (assert ,assertion ,vars ,@action))))

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
  (if args (apply '$set_prompt args))
  '$done)

(defun $reset_prompts ()
  (declare (special *set-prompt-alist*))
  (dolist (v *set-prompt-alist* '$done)
    ($set_prompt (car v) (or (gethash (cdr v) *variable-initial-values*) ""))))

(defun $set_alt_display (type &optional (f nil) (warn nil) (error-handler nil))
  (massert (and (member type '(1 2))
		(or (and (symbolp f) (or (fboundp f) (mfboundp f)))
		    (and (consp f) (member (caar f) '(lambda mfexpr)))
		    (null f)))
	   (type f)
	   "set_alt_display(type,f): type must equal 1 or 2, f must be a function, lambda expression or false.")
  (let* ((alt-display (ecase type
			(1 '*alt-display1d*)
			(2 '*alt-display2d*))))
    (labels ((error-handler (msg)
	       (if error-handler (mfuncall error-handler msg)
		   (let ((a-d alt-display)) (set alt-display nil)
			(merror "Error in ~a.~%Messge: ~a~%~a reset to ~a." a-d msg a-d nil))))
	     (alt-display-fun (form)
	       (handler-bind ((error #'error-handler))
		 (mfuncall f form))))
      (cond (f
	     (if warn (warn "Setting ~dd-display to ~(~a~)." type ($printf nil "~a" f)))
	     (set alt-display #'alt-display-fun))
	    (t
	     (if warn (warn "Resetting ~dd-display to default." type))
	     (set alt-display nil)))))
  '$done)

;; end of alt-display.lisp
