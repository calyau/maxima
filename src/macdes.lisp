;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

(defconstant *doc-start* (code-char 31))

(defvar $manual_demo "manual.demo")

(defmspec $example (l)   (setq l (cdr l))
  (block
   $example
   (let ((example (car l))
	 (file (or (cadr l)  (maxima-path "doc" $manual_demo))))
     (or (symbolp example)
	 (merror
	  "First arg ~M to example must be a symbol, eg example(functions)"))
     (setq file ($file_search1 $manual_demo '((mlist) $file_search_demo)))
     (with-open-file
      (st file)
      (let ( ;*mread-prompt*
	     )
	(prog ( tem  all c-tag d-tag)

	  AGAIN
	  (setq tem (read-char st nil))
	  (or tem (go NOTFOUND))
	  (or (eql tem #\&) (go AGAIN))
	  (setq tem (read-char st nil))
	  (or (eql tem #\&) (go AGAIN))
	  ;; so we are just after having read &&
	  
	  (setq tem (read st nil nil))
	  (or tem (go NOTFOUND))
	  (setq tem ($concat tem))
	  (cond ((eql tem example)
		 (go DOIT))
		(t (push tem all)
		   (go AGAIN)))
      ;; at this stage we read maxima forms and print and eval
	  ;; until a peek sees '&' as the first character of next expression.
	  DOIT
	  (setq tem (peek-char nil st nil))
	  (cond ((or (null tem) (eql tem #\&))
		 (return-from $example '$done)))
	  (setq tem (dbm-read st nil nil))
	  (setq $linenum (+ 1 $linenum))
	  (set (setq c-tag (makelabel $inchar)) (nth 2 tem))
	  (let ($display2d)
	    (displa `((mlable) ,c-tag ,(nth 2 tem)))
	    ;(mformat nil "Input: ~M;" (nth 2 tem))
	    )
	  (setq $% (meval* (nth 2 tem)))
	  (SET (setq d-tag (makelabel $outchar)) $%)
	  (if (eq (caar tem) 'displayinput)
	      (displa `((mlable) ,d-tag ,$%)))
	  ;(mformat nil "==> ~M"  (setq $% (meval* (nth 2 tem))))
	  (go DOIT) 	

	  NOTFOUND
	   (format t "Not Found.  You can look at:")
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
		      (T (merror "BAD ARG")))))))
(defvar *info-paths* nil)



(defun $describe(x &aux (*info-paths* *info-paths*) have)
  (setq x ($sconcat x))
  (if (and (find-package "SI")
           (fboundp (intern "INFO" "SI")))
      (return-from $describe (funcall (intern "INFO" "SI") x
         '("maxima.info") #-gcl *info-paths*)))

  "The documentation is now in INFO format and can be printed using
tex, or viewed using info or gnu emacs or using a web browser:
http://www.ma.utexas.edu/maxima/
   Some versions of maxima built have a builtin info retrieval mechanism."
  )

(defun $apropos ( s ) 
  (cons '(mlist) (apropos-list s "MAXIMA"))) 
