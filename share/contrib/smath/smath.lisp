;; -*- mode: lisp -*-
;; Copyright Leo Butler (leo.butler@member.fsf.org) 2013
;; Released under the terms of GPLv2

(in-package :maxima)

(defvar *displa* (symbol-function 'displa))
(defvar *maxima-outchar* $outchar)
(defvar *smath-outchar* '$%s)
(defvar $sm_rules)

(defun $smath_displa (form &optional (sm-rules (if (boundp '$sm_rules) $sm_rules)))
  "Display FORM by APPLY1-ing the rules in SM-RULES to the EXPR in FORM."
  (declare (special $sm_rules *alt-display1d*))
  (cond (sm-rules
	 (let ((rules (cdr sm-rules))
	       (*alt-display1d* nil)
	       (expr (third form)))
	   (funcall *displa*
		    (list (first form) (second form)
			  (dolist (rule rules expr)
			    (setf expr (mfuncall '$apply1 expr rule)))))))
	(t
	 ($to_mx_display)
	 (funcall *displa* form))))

(defun $to_sm_display ()
  "Set-up Maxima to use SMATH_DISPLA to display output."
  (declare (special *alt-display1d* $outchar *smath-outchar*))
  (setf *alt-display1d* (symbol-function '$smath_displa)
	$outchar *smath-outchar*))

(defun $to_mx_display ()
  "Return Maxima to use standard DISPLA for 1d output."
  (declare (special *alt-display1d* $outchar *maxima-outchar*))
  (setf *alt-display1d* nil
	$outchar *maxima-outchar*))

(defun $smkill (&optional s)
  "Execute KILL(S), then re-LOAD smath and set display."
  (mfuncall '$kill s)
  (mfuncall '$load "smath.mac")
  ($to_sm_display))

; end of smath.lisp 
