;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

(defconstant *doc-start* (code-char 31))

(DEFmspec $example (l)   (setq l (cdr l))
  (block
   $example
   (let ((example (car l))
	 (file (or (cadr l)  (maxima-path "doc" "manual.demo"))))
     (or (symbolp example)
	 (merror
	  "First arg ~M to example must be a symbol, eg example(functions)"))
     (setq file ($file_search1 file '((mlist) $file_search_demo)))
     (with-open-file
      (st file)
      (let ( *mread-prompt*)
	(sloop with show with tem with this with all
	       while   (setq tem (dbm-read st nil nil))
	       do
	       (cond((setq this (second tem))
		     (setq this (nth 1 this))
		     (push this all)
		     (if show (return-from $example '$done))
		     (setq show (eql this example))
		     (cond ((and
			     (eql this '$syntax)
			     (not show)
			     )
			    ;; last one is unreadable i
			    (format t "Not Found.  You can look at:")
			    (return-from $example
					 `((mlist) ,@ (nreverse all)))))))
	       (cond (show
		      (let ($display2d) (mformat nil
						 "Input: ~M;" (nth 2 tem)))
		      (mformat nil "==> ~M" (setq $% (meval* (nth 2 tem)))))))
	)))))

(defun mread-noprompt (&rest read-args)
  (let ((*mread-prompt* ""))
    (declare (special *mread-prompt*))
    (or read-args (setq read-args (list *query-io*)))
  (caddr (apply #'mread read-args))))

;; Some list creation utilities.

(defmspec $create_list(l) (setq l (cdr l))
   (create-list2 (car l) (cdr l)))

(defun create-list2 (form l)
  (cons '(mlist) (apply 'create-list1 form l)))

(defun create-list1(form &rest l &aux lis var1 top)
  (cond ((null l)(list (meval* form)))
	(t
	 (setq var1 (car l)
	       lis (second l)
	       l (cddr l))
	 (or (symbolp var1) (error "~a not a symbol" var1))
 	 (setq lis (meval* lis))
	 (progv (list var1)
		(list nil)
		(cond ((and (numberp lis)
			    (progn
			      (setq top (car l) l (cdr l))
			      (setq top (meval* top))
			      (numberp top)))
		       (sloop for i from lis to top
			      nodeclare t
			      do (set var1 i)
			      append
			      (apply 'create-list1
				     form l)))
		      (($listp lis)
		       (sloop for v in (cdr lis)
			      do (set var1 v)
			      append
			      (apply 'create-list1
				     form l)
			      ))
		      (T (MAXIMA-ERROR "BAD ARG")))))))

#+gcl
(progn
(defvar si::*info-paths* nil)


(defun $describe(x &aux si::*info-paths*)
  (setq x (string-trim "&$" (symbol-name x)))
  (setq  SYSTEM::*INFO-PATHS*
	 (cons  (si::string-concatenate *maxima-directory*
				     "info/")
		SYSTEM::*INFO-PATHS*))
  (if (fboundp 'si::info)
      (si::info x '("maxima.info"))
    "The documentation is now in INFO format and can be printed using
tex, or viewed using info or gnu emacs.   Versions of maxima built
on GCL have a builtin info retrieval mechanism" ))
)
