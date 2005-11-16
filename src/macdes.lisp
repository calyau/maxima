;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defconstant *doc-start* (code-char 31))

(defvar $manual_demo "manual.demo")

(defvar *maxima-demodir*)

(defun combine-path (list)
  (let ((result (first list)))
    (mapc #'(lambda (x) 
	      (setf result 
		    (concatenate 'string result "/" x))) (rest list))
    result))

(defmspec $example (l)   (setq l (cdr l))
	  (block
	      $example
	    (let ((example (car l))
		  (file (or (cadr l)  (combine-path 
				       (list *maxima-demodir* $manual_demo)))))
	      (or (symbolp example)
		  (merror
		   "First arg ~M to example must be a symbol, eg example(functions)"))
	      (setq file ($file_search1 $manual_demo '((mlist) $file_search_demo)))
	      (with-open-file
		  (st file)
		(let (			;*mread-prompt*
		      )
		  (prog ( tem  all c-tag d-tag)

		   again
		   (setq tem (read-char st nil))
		   (or tem (go notfound))
		   (or (eql tem #\&) (go again))
		   (setq tem (read-char st nil))
		   (or (eql tem #\&) (go again))
		   ;; so we are just after having read &&
	  
		   (setq tem (read st nil nil))
		   (or tem (go notfound))
		   (setq tem (makealias tem))
		   (cond ((eql tem example)
			  (go doit))
			 (t (push tem all)
			    (go again)))
		   ;; at this stage we read maxima forms and print and eval
		   ;; until a peek sees '&' as the first character of next expression.
		   doit
		   (setq tem (peek-char nil st nil))
		   (cond ((or (null tem) (eql tem #\&))
			  (setf *need-prompt* t)
			  (return-from $example '$done)))
		   (setq tem (dbm-read st nil nil))
		   (setq $linenum (+ 1 $linenum))
		   (set (setq c-tag (makelabel $inchar)) (nth 2 tem))
		   (let ($display2d)
		     (displa `((mlable) ,c-tag ,(nth 2 tem)))
					;(mformat nil "Input: ~M;" (nth 2 tem))
		     )
		   (setq $% (meval* (nth 2 tem)))
		   (set (setq d-tag (makelabel $outchar)) $%)
		   (if (eq (caar tem) 'displayinput)
		       (displa `((mlable) ,d-tag ,$%)))
					;(mformat nil "==> ~M"  (setq $% (meval* (nth 2 tem))))
		   (go doit) 	

		   notfound
		   (format t "Not Found.  You can look at:")
		   (setf *need-prompt* t)
		   (return-from $example
		     `((mlist) ,@ (nreverse all)))
		   ))))))

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
	 (or (symbolp var1) (merror "~A not a symbol" var1))
 	 (setq lis (meval* lis))
	 (progv (list var1)
	     (list nil)
	   (cond ((and (numberp lis)
		       (progn
			 (setq top (car l) l (cdr l))
			 (setq top (meval* top))
			 (numberp top)))
		  (loop for i from lis to top
			 do (set var1 i)
			 append
			 (apply 'create-list1
				form l)))
		 (($listp lis)
		  (loop for v in (cdr lis)
			 do (set var1 v)
			 append
			 (apply 'create-list1
				form l)
			 ))
		 (t (merror "Bad arg")))))))

;; The documentation is now in INFO format and can be printed using
;; tex, or viewed using info or gnu emacs or using a web browser.  All
;; versions of maxima have a builtin info retrieval mechanism.

(defmspec $describe (x)
  (setq x ($sconcat (cadr x)))
  (let ((cl-info::*prompt-prefix* *prompt-prefix*)
	(cl-info::*prompt-suffix* *prompt-suffix*))
    #-gcl
    (cl-info:info x)
    ;; Optimization: GCL's built-in info is much faster than our info
    ;; implementation. However, GCL's info won't respect out *prompt-
    ;; variables. Compromise by only calling our info when the prompts
    ;; are not empty. --jfa 07/25/04
    ;; We have to use Maxima info for GCL, at least temorarily, since 
    ;; GCL's info is not quite compatible (GCL 2.6.6) with recent
    ;; texinfo releases. -- ZW 01-Apr-05
    #+gcl
    (cl-info:info x)
    #+nil
    (if (and (string= *prompt-prefix* "") (string= *prompt-suffix* ""))
	(progn
	  (setf system::*info-paths* cl-info:*info-paths*)
	  (system::info x '("maxima.info")))
	(cl-info:info x))))

(defun $apropos ( s ) 
  (cons '(mlist) (apropos-list s :maxima)))
