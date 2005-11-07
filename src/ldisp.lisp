;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxiliary DISPLA package for doing 1-D display
;;;
;;; (c) 1979 Massachusetts Institute of Technology
;;;
;;; See KMP for details

(in-package :maxima)
(declare-top (*expr mstring stripdollar)
	     (special linear-display-break-table fortranp))

#+maclisp
(eval-when (eval compile)
  (sstatus macro /# '+internal-/#-macro splicing))

;;; (LINEAR-DISPLA <thing-to-display>)
;;;
;;; Display text linearly. This function should be usable in any case
;;;  DISPLA is usable and will attempt to do something reasonable with
;;;  its input.


;;;The old linear-displa used charpos, not available in common lisp.
;;;It also did a much worse job on the display, breaking inside things
;;;like x^2.  --wfs

#+cl
(defun linear-displa (x )
  (declare (special chrps))
  (fresh-line *standard-output*)
  (cond ((not (atom x))
	 (cond ((eq (caar x) 'mlable)
		(setq chrps 0)
		(cond ((cadr x)
		       (princ "(")
		       (setq chrps
			     (+  3 (length (mgrind (cadr x) nil))))
		       (princ ") ")))
		(mprint (msize (caddr x) nil nil 'mparen 'mparen)
			*standard-output*))
	       ((eq (caar x) 'mtext)
		(do ((x (cdr x) (cdr x))
		     (fortranp))	; Atoms in MTEXT
		    ((null x))		;  should omit ?'s
		  (setq fortranp (atom (car x)))
					;(LINEAR-DISPLA1 (CAR X) 0.)
		  (mgrind (car x) *standard-output*)
					;(tyo #\space )
		  ))
	       (t
		(mgrind x *standard-output*))))
	(t
	 (mgrind x *standard-output*)))
  (terpri))



;;; (LINEAR-DISPLA <thing-to-display>)
;;;
;;; Display text linearly. This function should be usable in any case
;;;  DISPLA is usable and will attempt to do something reasonable with
;;;  its input.
#-cl
(defun linear-displa (x)
  (terpri)
  (cond ((not (atom x))
	 (cond ((eq (caar x) 'mlable)
		(cond ((cadr x)
		       (prin1 (list (stripdollar (cadr x))))
		       (tyo #\space)))
		(linear-displa1 (caddr x) (charpos t)))
	       ((eq (caar x) 'mtext)
		(do ((x (cdr x) (cdr x))
		     (fortranp))	; Atoms in MTEXT
		    ((null x))		;  should omit ?'s
		  (setq fortranp (atom (car x)))
		  (linear-displa1 (car x) 0.)
					;(TYO #\space)
		  ))
	       (t
		(linear-displa1 x 0.))))
	(t
	 (linear-displa1 x 0.)))
  (terpri))


;;********** old linear-displa *************
;;; LINEAR-DISPLAY-BREAK-TABLE
;;;  Table entries have the form (<char> . <illegal-predecessors>)
;;;
;;;  The linear display thing will feel free to break BEFORE any
;;;  of these <char>'s unless they are preceded by one of the
;;;  <illegal-predecessor> characters.

#-cl
(setq linear-display-break-table
      '((#\= #\: #\=)
	(#. left-parentheses-char #. left-parentheses-char #\[)
	(#. right-parentheses-char #. right-parentheses-char #\])
	(#\[ #. left-parentheses-char #\[)
	(#\] #. right-parentheses-char #\])
	(#\: #\:)
	(#\+ #\E #\B)
	(#\- #\E #\B)
	(#\* #\*)
	(#\^)))
	
;;; (FIND-NEXT-BREAK <list-of-fixnums>)
;;;   Tells how long it will be before the next allowable
;;;   text break in a list of chars.

#-cl
(defun find-next-break (l)
  (do ((i 0. (f1+ i))
       (temp)
       (l l (cdr l)))
      ((null l) i)
    (cond ((zl-member (car l) '(#\space #\,)) (return i))
	  ((and (setq temp (assq (cadr l) linear-display-break-table))
		(not (memq (car l) (cdr temp))))
	   (return i)))))

;;; (LINEAR-DISPLA1 <object> <indent-level>)
;;;  Displays <object> as best it can on this line.
;;;  If atom is too long to go on line, types # and a carriage return.
;;;  If end of line is found and an elegant break is seen 
;;;   (see FIND-NEXT-BREAK), it will type a carriage return and indent
;;;   <indent-level> spaces.
#-cl
(defun linear-displa1 (x indent)
  (let ((chars (mstring x)))
    (do ((end-column  (f- (linel t) 3.))
	 (chars chars (cdr chars))
	 (i (charpos t) (f1+ i))
	 (j (find-next-break chars) (f1- j)))
	((null chars) t)
      (tyo (car chars))
      (cond ((< j 1)
	     (setq j (find-next-break (cdr chars)))
	     (cond ((> (f+ i j) end-column)
		    (terpri)
		    (do ((i 0. (f1+ i))) ((= i indent)) (tyo #\space))
		    (setq i indent))))
	    ((= i end-column)
	     (princ '/#)
	     (terpri)
	     (setq i -1.))))))
