;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1981 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module evalw)

;;; Assuming that this will only be a top-level form, it will
;;; only be see by MEVAL when a file is batched.

;;; EVAL_WHEN(TRANSLATE,FOO(ME),BAZ())$
;;; EVAL_WHEN([LOADFILE,BATCH],INITIALIZE())$

(declare-top(special $version state-pdl batconl))

;; Gosh. Seems it was really stupid to have EVAL_WHEN for BATCH and DEMO,
;; people use it for the most random things. -gjc

(defmspec $eval_when (argl)
  (setq argl (cdr argl))
  (cond ((or (< (length argl) 2)
	     (not (or (atom (car argl))
		      ($listp (car argl)))))
	 (merror "Bad whens form to EVAL_WHEN~%~M" (car argl))))
  (let ((demop #-maxii (if (and (eq (ml-typep $version) 'fixnum)
				(> $version 296.))
			   (caddr batconl)
			   (cadddr batconl))
	       #+maxii  nil)
	(whens (cond (($listp (car argl)) (cdar argl))
		     (t (list (car argl))))))
    (cond ((cond (#-maxii (memq 'batch state-pdl)
			  #+maxii t	; foo for now!
			  (if demop (or (memq '$demo whens) (memq '$batch whens))
			      (memq '$batch whens)))
		 (t
		  ;; this is a form typed in on a c-line by
		  ;; the user. or, perhaps it is inside a
		  ;; program. Which is an error in the translator.
		  ;; What *was* I doing here? -gjc
		  (memq '$toplevel whens)))
	   `(($evaluated_when) ,@(mapcar 'meval (cdr argl))))
	  (t
	   '$not_evaluated_when))))

