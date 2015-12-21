;; -*- mode: lisp; coding: utf-8 -*-
;; Copyright Leo Butler (leo.butler@member.fsf.org) 2015
;; Released under the terms of GPLv3+

#|

Rewrite Maxima's alphabetp function to handle wide-characters.
Add the ability for user to alter the table of alphabetic characters at runtime.

To do:
-detect if unicode characters are characters

|#

(defpackage :unicodedata
  (:use :common-lisp :maxima)
  (:import-from :maxima merror alphabetp *alphabet* $file_search1 $file_search_lisp $done $all mfuncall mlist)
  )

(in-package :unicodedata)

(let  ((unicode-data-hashtable (make-hash-table :test #'eql))
       (alphabetp-hashtable  (make-hash-table :test #'equal)))


  (labels ((lookup (i &optional (unicode-data-hashtable unicode-data-hashtable)) (symbol-name (car (gethash i unicode-data-hashtable))))
	   (description (x) (symbol-name (second x)))
	   (create-selector (regexp)
	     (cond ((stringp regexp)
		    (let ((s (string-downcase regexp)))
		      (lambda (x) (search s (string-downcase x)))))
		   ((eql regexp '$all)
		    (lambda (x) (declare (ignore x)) t))
		   ((null regexp)
		    (lambda (x) (declare (ignore x)) nil))
		   (t
		    (merror "regexp must be a string, the symbol `all' or empty."))))
	   (create-adder (lookup-char)
	     (labels ((this (regexp append)
			(let ((selector (create-selector regexp)))
			  (unless append (clrhash alphabetp-hashtable))
			  (maphash (lambda (k v)
				     (if (and k (funcall selector (funcall lookup-char v)))
					 (maxima::$set_alpha_char k)))
				   unicode-data-hashtable))
			'$done))
	       #'this))
	   #-sbcl (char-sym (x) (first x))
	   (category (x) (symbol-name (third x))))

    (let ((stack '()))
      (defun unicode-alphabetp (c)
	(cond ((< (char-code c) 128.)				;; this character is ascii and must be non-alphabetic
	       (setf stack '()))				;; there are no characters on the stack nor known to be part of a wide-character
	      ((null stack)					;; len=0 
	       (push c stack)
	       (if (gethash stack alphabetp-hashtable)
		   stack
		   (setf stack '())))
	      (t						;; 1 or more characters in stack
	       (push c stack)
	       (cond ((gethash stack alphabetp-hashtable)	;; c is part of wide-character
		      stack)
		     (t
		      (setf stack '())				;; c is not end of wide-character
		      (unicode-alphabetp c))))))		;; but it may start new one.
      ;; redefine alphabetp from src/nparse.lisp
      ;; to use unicode-alphabetp
      (defun alphabetp (n)
	(and (characterp n)
	     (cond ((or (alpha-char-p n)
			(member n *alphabet*))
		    (setf stack '())
		    t)
		   (t
		    (unicode-alphabetp n))))))
    
    (defun maxima::$set_alpha_char (char-sym)
      "A user-level function to add a wide character to the hashtable of
known alphabetical characters."
      (let ((char-sym-list (coerce (cond ((stringp char-sym) char-sym)
					 ((symbolp char-sym) (symbol-name char-sym))
					 ((and (integerp char-sym) (< 127. char-sym) (> 917999. char-sym))
					  #+(or sbcl clisp)
					  (format nil "~c" (code-char char-sym))
					  #-(or sbcl clisp)
					  (lookup char-sym unicode-data-hashtable))
					 (t (merror "first argument must be a string, symbol or integer")))
				   'list)))
	(do ((x (reverse char-sym-list) (cdr x))) ((null x) '$done)
	  (setf (gethash x alphabetp-hashtable)
		#-t (push (list char-sym-list (gethash char-sym unicode-data-hashtable)) (gethash x alphabetp-hashtable))
		#+t t))))

    (defun maxima::$unicode_init (&optional (regexp nil) file)
      (let ((data-file (mfuncall '$file_search1 (or file "unicodedata-txt.lisp") '((mlist) $file_search_lisp))))
	(loop for (n char-sym description category) in (with-open-file (instr data-file :direction :input) (read instr t nil nil))
	   do
	     (setf (gethash n unicode-data-hashtable) (list char-sym description category)))
	(if regexp (maxima::$unicode_add regexp))))

    
    (defun maxima::$unicode_add_category (category &optional append)
      (funcall (create-adder #'category) category append))
    
    (defun maxima::$unicode_add (&optional (regexp nil) (append nil))
      "Select the wide characters via a MAXIMA-NREGEX regexp. If
	   REGEXP is the symbol `all', this is equivalent to \".\"; if
	   REGEXP is NIL, then no matches are made, i.e. the hash
	   table UNICODE-DATA-HASHTABLE is emptied. Example:
	   unicode_add(\"greek .+ letter [^ ]+$\");"
      (cond ((and (listp regexp) (not (null regexp)))
	     (let ((r (loop for regex in (cdr regexp)
			 for append = append then t
			 collect (maxima::$unicode_add regex append))))
	       (cons '(mlist) r)))
	    (t
	     (let ((n (hash-table-count alphabetp-hashtable)))
	       (funcall (create-adder #'description) regexp append)
	       (list '(mlist) regexp (- (hash-table-count alphabetp-hashtable) n))))))

    (defun print-hashtable (&optional (ht unicode-data-hashtable))
      (let ((*print-base* 16.)
	    (*print-readably* t)
	    (*print-radix* t))
	(maphash (lambda(k v) (format t "(~a  ~{~s ~})~%" k (if (listp v) v (list v)))) ht)))
    (defun print-alphabetp-hashtable ()
      (print-hashtable alphabetp-hashtable))))

; end of unicodedata.lisp 
