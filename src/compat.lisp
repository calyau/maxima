;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; Maclisp compatibility definitions.
;; This file is for Lisp differences only.  No knowledge of Macsyma should be
;; contained in this file.

;; Run time stuff

(defun symbolconc (&rest args)
  "make a symbol out of the printed representations of all args"
  (intern (apply #'concatenate 'string
		 (mapcar #'(lambda (s)
                             (typecase s
                               (integer (format nil "~d" s))
                               (symbol (symbol-name s))
                               (string s)
                               (t (format nil "~a" s))))
			 args))))



(defun maxima-makunbound (symbol)
  "Remove SYMBOL's value from its current dynamic binding."
  #+ecl
  (if (symbolp symbol)
      ;; ECL 21.2.1's MAKUNBOUND writes the global cell even under PROGV.
      ;; ECL_SETQ addresses the current binding, as SET does. Check the
      ;; actual constant flag: CONSTANTP can expand a symbol macro.
      (ffi:c-inline (symbol) (:object) :object
                    "((ecl_symbol_type(#0) & ecl_stp_constant) ? cl_makunbound(#0)
                      : (ECL_SETQ(ecl_process_env(), #0, OBJNULL), #0))"
                    :one-liner t)
      ;; Retain the implementation's checks and diagnostics for invalid
      ;; arguments and constants; these must never reach the raw accessor.
      (makunbound symbol))
  #-ecl (makunbound symbol))
