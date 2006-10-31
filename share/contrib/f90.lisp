;;;; f90.lisp -- Application command line argument retrieval
;;;;                      and processing for Common Lisp.

;;;; Copyright (C) 2004 James F. Amundson

;;;; f90.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; f90.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with f90.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

;;;; Based on fortra.lisp. Copyright statements for fortra.lisp follow:
;;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas
;;;;     All rights reserved                                         
;;;;  (c) Copyright 1980 Massachusetts Institute of Technology    

(in-package :maxima)
(macsyma-module f90)

(defun f90-print (x
		  &aux 
		  ;; This is a poor way of saying that array references
		  ;; are to be printed with parens instead of brackets.
		  (lb #. left-parentheses-char ) 
		  (rb #. right-parentheses-char ))
  ;; Restructure the expression for displaying.
  (setq x (fortscan x))
  ;; Linearize the expression using MSTRING.  Some global state must be
  ;; modified for MSTRING to generate using Fortran syntax.  This must be
  ;; undone so as not to modifiy the toplevel behavior of MSTRING.
  (unwind-protect
       (defprop mexpt msize-infix grind)
    (defprop mminus 100. lbp)
    
    (defprop msetq (#\:) strsym)  
    (setq x (mstring x))
    ;; Make sure this gets done before exiting this frame.
    (defprop mexpt msz-mexpt grind)
    (remprop 'mminus 'lbp))
  (do ((char 0 (1+ char))
       (line ""))
      ((>= char (length x)))
    (setf line (concatenate 'string line (make-sequence 
					  'string 1 
					  :initial-element (nth char x))))
    (if (>= (length line) 65)
	(let ((break_point -1))
	  (mapc #'(lambda (x)
		    (let ((p (search x line :from-end t))) 
		      (if (and p (> p 0))
			  (setf break_point p))))
		'("+" "-" "*" "/"))
	  (increment break_point)
	  (if (= break_point 0)
	      (progn (princ line) (setf line "     "))
	      (progn
		(princ (subseq line 0 break_point))
		(princ " &")
		(terpri)
		(setf line (concatenate 'string "     "
					(subseq line break_point
						(length line))))))))
    (if (and (= char (1- (length x))) (not (equal line "     ")))
	(princ line)))
  (terpri)
  '$done)

(defmspec $f90 (l)
  (setq l (fexprcheck l))
  (let ((value (strmeval l)))
    (cond ((msetqp l) (setq value `((mequal) ,(cadr l) ,(meval l)))))
    (cond ((and (symbolp l) ($matrixp value))
	   ($fortmx l value))
	  ((and (not (atom value)) (eq (caar value) 'mequal)
		(symbolp (cadr value)) ($matrixp (caddr value)))
	   ($fortmx (cadr value) (caddr value)))
	  (t (f90-print value)))))
