;;;;;; -*- Mode: LISP; Package: zwei; Base: 8; Syntax: common-lisp -*-;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (compile load eval)
(defvar semi-colon-char  #-zlch 59. #+zlch #\; )
(defvar *maxima-form-delimiters* '(#\; #\$))
)
 
(DEFMAJOR COM-MACSYMA-MODE MACSYMA-MODE #+ti "MACSYMA" #-ti MACSYMA
          "Enter a mode for editing Macsyma code.
Modifies the delimiter dispatch tables appropriately for Macsyma syntax,
makes comment delimiters /* and */.  Tab is Indent-Relative." ()
  (SET-COMTAB *MODE-COMTAB* '(#\tab com-indent-nested
			      ;;this defines method :mode-forms 
			      #\c-sh-e COM-MACSYMA-EVALUATE-REGION
			      #\c-sh-t com-macsyma-translate-region
			      #\c-sh-g com-macsyma-grind-expression
			      #\m-sh-d com-macsyma-display-on-typeout
			       #\c-sh-d com-macsyma-display-into-buffer
			      #\c-sh-i com-macsyma-evaluate-into-buffer
			      #\hyper-i  com-evaluate-with-arrow
			      #\control-meta-f com-forward-macsyma-expression
			      #\control-meta-b com-backward-macsyma-expression
			      #\c-sh-c com-macsyma-compile-region)
  (make-command-alist '(com-macsyma-evaluate-into-buffer
			 com-macsyma-describe
			 com-macsyma-evaluate-buffer	;here is where to put
			 com-macsyma-compile-buffer	;any extended commands ofr
			 com-macsyma-evaluate-region
			 com-macsyma-display-into-buffer
			 com-macsyma-display-on-typeout
			 com-macsyma-grind-expression
			 com-macsyma-compile-region
			 com-macsyma-translate-buffer
			 com-macsyma-translate-region-to-buffer
			 )))
  ;; Tab hacking rubout.
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* (list #+ti #\tab #-ti (ascii #\tab) (format nil "   ")))
  (SETQ *COMMENT-COLUMN* (* 40. 6))
  (SETQ *COMMENT-START* "/*")
  (SETQ *COMMENT-BEGIN* "/* ")
  (SETQ *COMMENT-END* "*/")
  (PROGN
    (OR (BOUNDP '*MACSYMA-LIST-SYNTAX-TABLE*)
	(SETQ *MACSYMA-LIST-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *MACSYMA-LIST-SYNTAX-LIST*))))
  (PROGN
    (OR (BOUNDP '*MACSYMA-word-SYNTAX-TABLE*)
	(SETQ *MACSYMA-word-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *MACSYMA-word-SYNTAX-LIST*))))
  (SETQ *LIST-SYNTAX-TABLE* *MACSYMA-LIST-SYNTAX-TABLE*)
  (setq *atom-word-syntax-table* *macsyma-word-syntax-table*)
  (SETQ *mode-LIST-SYNTAX-TABLE* *MACSYMA-LIST-SYNTAX-TABLE*)
  (SET-CHAR-SYNTAX WORD-delimiter *MODE-WORD-SYNTAX-TABLE* #\*)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #\?)  
  )



(SETQ *MACSYMA-LIST-SYNTAX-LIST*
    '(
      (40 LIST-ALPHABETIC)
      
      LIST-DELIMITER		;040 space
      LIST-DELIMITER		;041 ! ***
      LIST-DOUBLE-QUOTE         ;042 "     "
      LIST-DELIMITER		;043 # ***
      LIST-DELIMITER		;044 $ ***
      LIST-ALPHABETIC           ;045 %
      LIST-DELIMITER		;046 & ***
      LIST-SINGLE-QUOTE         ;047 '
      LIST-OPEN                 ;050 (
      LIST-CLOSE		;051 )
      LIST-DELIMITER		;052 * ***
      LIST-DELIMITER		;053 + ***
      LIST-DELIMITER		;054 , ***
      LIST-DELIMITER		;055 - ***
      LIST-DELIMITER		;056 . ***
      LIST-DELIMITER		;057 / ***
      (10. LIST-ALPHABETIC)			;DIGITS
      LIST-DELIMITER		;072 : ***
      LIST-DELIMITER		;073 ; ***
      LIST-DELIMITER		;074 < ***
      LIST-DELIMITER		;075 = ***
      LIST-DELIMITER		;076 > ***
      LIST-ALPHABETIC           ;077 ?
      LIST-DELIMITER		;100 @ ***
      (26. LIST-ALPHABETIC)			;LETTERS
      LIST-OPEN                 ;133 [ ***
      LIST-SLASH		;134 \ ***
      LIST-CLOSE		;135 ] ***
      LIST-DELIMITER		;136 ^ ***
      LIST-alphabetic		;137 _ ***
      LIST-DELIMITER		;140 ` ***
      (26. LIST-ALPHABETIC)			;MORE LETTERS
      LIST-OPEN                 ;173 { ***
      LIST-DELIMITER		;174 | ***        |
      LIST-CLOSE		;175 } ***
      LIST-DELIMITER		;176 ~ ***
      LIST-ALPHABETIC           ;177 integral ???
      
      LIST-ALPHABETIC           ;200 null character
      LIST-DELIMITER		;201 break
      LIST-DELIMITER		;202 clear
      LIST-DELIMITER		;203 call
      LIST-DELIMITER		;204 escape (NOT altmode!)
      LIST-DELIMITER		;205 backnext
      LIST-DELIMITER		;206 help
      LIST-DELIMITER		;207 rubout
      LIST-ALPHABETIC           ;210 bs
      LIST-DELIMITER		;211 tab
      LIST-DELIMITER		;212 line
      LIST-DELIMITER		;213 vt
      LIST-DELIMITER		;214 form = newpage
      LIST-DELIMITER		;215 return = newline
      (162 LIST-ALPHABETIC)))





(SETQ *MACSYMA-WORD-SYNTAX-LIST*
      '(
	(40 LIST-ALPHABETIC)
	
	WORD-DELIMITER				;040 space
	WORD-DELIMITER				;041 ! ***
	WORD-DELIMITER				;042 "     "
	WORD-DELIMITER				;043 # ***
	WORD-DELIMITER				;044 $ ***
	WORD-ALPHABETIC				;045 %
	WORD-DELIMITER				;046 & ***
	WORD-DELIMITER				;047 '
	WORD-DELIMITER				;050 (
	WORD-DELIMITER				;051 )
	WORD-DELIMITER				;052 * ***
	WORD-DELIMITER				;053 + ***
	WORD-DELIMITER				;054 , ***
	WORD-DELIMITER				;055 - ***
	WORD-DELIMITER				;056 . ***
	WORD-DELIMITER				;057 / ***
	(10. LIST-ALPHABETIC)			;DIGITS
	WORD-DELIMITER				;072 : ***
	WORD-DELIMITER				;073 ; ***
	WORD-DELIMITER				;074 < ***
	WORD-DELIMITER				;075 = ***
	WORD-DELIMITER				;076 > ***
	WORD-ALPHABETIC				;077 ?
	WORD-DELIMITER				;100 @ ***
	(26. LIST-ALPHABETIC)			;LETTERS
	WORD-DELIMITER				;133 [ ***
	WORD-DELIMITER				;134 \ ***
	WORD-DELIMITER				;135 ] ***
	WORD-DELIMITER				;136 ^ ***
	WORD-ALPHABETIC				;137 _ ***
	WORD-DELIMITER				;140 ` ***
	(26. LIST-ALPHABETIC)			;MORE LETTERS
	WORD-DELIMITER				;173 { ***
	WORD-DELIMITER				;174 | ***        |
	WORD-DELIMITER				;175 } ***
	WORD-ALPHABETIC				;176 ~ ***
	WORD-ALPHABETIC				;177 integral ???
	
	WORD-ALPHABETIC				;200 null character
	WORD-DELIMITER				;201 break
	WORD-DELIMITER				;202 clear
	WORD-DELIMITER				;203 call
	WORD-DELIMITER				;204 escape (NOT altmode!)
	WORD-DELIMITER				;205 backnext
	WORD-DELIMITER				;206 help
	WORD-DELIMITER				;207 rubout
	WORD-DELIMITER				;210 bs
	WORD-DELIMITER				;211 tab
	WORD-DELIMITER				;212 line
	WORD-DELIMITER				;213 vt
	WORD-DELIMITER				;214 form = newpage
	WORD-DELIMITER				;215 return = newline
	(162 LIST-ALPHABETIC)))


(defun in-macsyma-comment (bp &aux tem tem2)
  (cond ((setq tem (search bp *comment-start*  t nil))
	 (cond ((setq tem2 (search tem *comment-end* nil ))
		(cond ((bp-< bp tem2) t)
		      (t nil)))
	       (t t)))))

(defun end-of-macsyma-form (bp &aux tem)
  (loop while (setq tem (search-set bp '(#\$ #\; )))
	when (not (or (in-macsyma-comment tem) (in-macsyma-string tem)))
	  do (return tem)
	else
	  do (setq bp tem)
       finally (return (zwei::interval-last-bp *interval*))))

(defun end-of-previous-macsyma-form (bp &aux tem)
  (loop while (setq tem (search-set bp '(#\$ #\;) t))
	when (not (or (in-macsyma-comment tem) (in-macsyma-string tem)))
	  do (return tem)
	else
	  do (setq bp tem)
       finally (return (zwei::interval-first-bp *interval*))))

(defun mark-current-macsyma-form ()
  (skip-back-over-whitespace)
  (let ((end-of-previous-form (end-of-previous-macsyma-form  (point)))
	(end-of-current-form ()))
     (cond ((equal end-of-previous-form (zwei::interval-first-bp zwei::*interval*))
	    (move-bp (point)  end-of-previous-form))
	   (t (zwei::move-bp (zwei::point) (forward-char end-of-previous-form))))
    (point-pdl-push (point) *window* nil nil)
    (move-bp (mark) (point))
    (setq end-of-current-form (zwei::end-of-macsyma-form (zwei::point)))	
    (cond ((null end-of-current-form)
	   (barf "Nothing to Evaluate"))
	  (t (move-bp (point) end-of-current-form)))))



(defun macsyma-skip-comment (bp &optional reversep stop-bp &aux found-comment answ answ2)
  (cond ((in-macsyma-comment bp)
	 (macsyma-skip-present-comment bp reversep)))
  (cond ((null stop-bp)
	 (cond (reversep (setq stop-bp (interval-first-bp *interval*)))
	       (t     (interval-last-bp *interval*)))))
  (setq answ  (cond (reversep
		     (setq bp (backward-over *whitespace-chars* bp)))
		    (t
		     (setq bp (forward-over *whitespace-chars* bp)))))
  (setq answ2 (cond ((bp-= bp stop-bp) nil)
		    ((cond ( (or (and (null reversep) (looking-at bp *comment-start*))
				 (and  reversep (looking-at-backward bp *comment-end*)))
			    (setq found-comment t)
			    (macsyma-skip-present-comment bp reversep))))))
  (values (or answ2 answ) found-comment))



(defun macsyma-skip-present-comment (bp &optional reversep &aux (times 1))
   (setq bp (forward-char bp times))
  (cond (reversep (setq times (- 1))))
  (let ((x (cond ((null reversep)   (search bp *comment-end*))
		 (t   (search bp *comment-start* (< times 0))))))
    (cond ((null x) (barf "Unbalanced comment."))
	  (t x))))
