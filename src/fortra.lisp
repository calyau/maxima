;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module fortra)

(declare-top (special lb rb	 ;Used for communication with MSTRING.
		      $loadprint ;If NIL, no load message gets printed.
		      1//2 -1//2)
	     (*lexpr fortran-print $fortmx))

(defmvar $fortspaces nil
  "If T, Fortran card images are filled out to 80 columns using spaces."
  boolean
  modified-commands '$fortran)

(defmvar $fortindent 0
  "The number of spaces (beyond 6) to indent Fortran statements as they
   are printed."
  fixnum
  modified-commands '$fortran)

(defmvar $fortfloat nil "Something JPG is working on.")

;; This function is called from Macsyma toplevel.  If the argument is a
;; symbol, and the symbol is bound to a matrix, then the matrix is printed
;; using an array assignment notation.

(defmspec $fortran (l)
  (setq l (fexprcheck l))
  (let ((value (strmeval l)))
    (cond ((msetqp l) (setq value `((mequal) ,(cadr l) ,(meval l)))))
    (cond ((and (symbolp l) ($matrixp value))
	   ($fortmx l value))
	  ((and (not (atom value)) (eq (caar value) 'mequal)
		(symbolp (cadr value)) ($matrixp (caddr value)))
	   ($fortmx (cadr value) (caddr value)))
	  (t (fortran-print value)))))

;; This function is called from Lisp programs.  It takes an expression and
;; a stream argument.  Default stream is NIL in MacLisp and *STANDARD-OUTPUT*
;; in LMLisp.  This should be canonicalized in Macsyma at some point.

;; TERPRI is a PDP10 MacLisp flag which, if set to T, will keep symbols and
;; bignums from being broken across page boundaries when printed.  $LOADPRINT
;; is NIL to keep a message from being printed when the file containing MSTRING
;; is loaded.  (MRG;GRIND)

(defprop mexpt (#\* #\*) dissym)

(defun fortran-print (x &optional (stream #+maclisp nil #-maclisp *standard-output*)
		      &aux #+pdp10 (terpri t) #+pdp10 ($loadprint nil)
		      ;; This is a poor way of saying that array references
		      ;; are to be printed with parens instead of brackets.
		      (lb #. left-parentheses-char ) (rb #. right-parentheses-char ))
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
    (remprop 'mminus 'lbp)
    )
  
  ;; MSTRING returns a list of characters.   Now print them.
  (do ((c #.(char-int #\0)
	  (f+ 1 (remainder (f- c #. (char-int #\0))
			   16) #. (char-int #\0)))
       (column (f+ 6 $fortindent) (f+ 9 $fortindent)))
      ((null x))
    ;; Print five spaces, a continuation character if needed, and then
    ;; more spaces.  COLUMN points to the last column printed in.  When
    ;; it equals 80, we should quit.
    (cond ((= c #. (char-int #\0))
	   (print-spaces column stream))
	  (t (print-spaces 5 stream)
	     (tyo (code-char c) stream)
	     (print-spaces (f- column 6) stream)))
    ;; Print the expression.  Remember, Fortran ignores blanks and line
    ;; terminators, so we don't care where the expression is broken.
    (do ()
	((= column 72.))
      (if (null x)
	  (if $fortspaces (tyo #\space stream) (return nil))
	  (progn (and (equal (car x) #. back-slash-char) (setq x (cdr x)))
		 (tyo (pop x) stream)))
      (increment column))
    ;; Columns 73 to 80 contain spaces
    (if $fortspaces (print-spaces 8 stream))
    (terpri stream))
  '$done)

(defun print-spaces (n stream)
  (dotimes (i n) (tyo #\space stream)))

;; This function is similar to NFORMAT.  Prepare an expression
;; for printing by converting x^(1/2) to sqrt(x), etc.  A better
;; way of doing this would be to have a programmable printer and
;; not cons any new expressions at all.  Some of this formatting, such
;; as E^X --> EXP(X) is specific to Fortran, but why isn't the standard
;; function used for the rest?

(defun fortscan (e)
  (cond ((atom e) (cond ((eq e '$%i) '((mprogn) 0.0 1.0))
			(t e)))		;%I is (0,1)
	((and (eq (caar e) 'mexpt) (eq (cadr e) '$%e))
	 (list '($exp simp) (fortscan (caddr e))))
	((and (eq (caar e) 'mexpt) (alike1 (caddr e) 1//2))
	 (list '(%sqrt simp) (fortscan (cadr e))))
	((and (eq (caar e) 'mexpt) (alike1 (caddr e) -1//2))
	 (list '(mquotient simp) 1 (list '(%sqrt simp) (fortscan (cadr e)))))
	((and (eq (caar e) 'mtimes) (ratnump (cadr e))
	      (zl-member (cadadr e) '(1 -1)))
	 (cond ((equal (cadadr e) 1) (fortscan-mtimes e))
	       (t (list '(mminus simp) (fortscan-mtimes e)))))
	((eq (caar e) 'rat)
	 (list '(mquotient simp) (float (cadr e)) (float (caddr e))))
	((eq (caar e) 'mrat) (fortscan (ratdisrep e)))
	;;  complex numbers to f77 syntax a+b%i ==> (a,b)
	((and (memq (caar e) '(mtimes mplus))
	      ((lambda (a) 
		 (and (numberp (cadr a))
		      (numberp (caddr a))
		      (not (zerop1 (cadr a)))
		      (list '(mprogn) (caddr a) (cadr a))))
	       (simplify ($bothcoef e '$%i)))))
	(t (cons (car e) (mapcar 'fortscan (cdr e))))))

(defun fortscan-mtimes (e)
  (list '(mquotient simp)
	(cond ((null (cdddr e)) (fortscan (caddr e)))
	      (t (cons (car e) (mapcar 'fortscan (cddr e)))))
	(float (caddr (cadr e)))))

;; Takes a name and a matrix and prints a sequence of Fortran assignment
;; statements of the form
;;  NAME(I,J) = <corresponding matrix element>

(defmfun $fortmx (name mat &optional (stream #-cl nil #+cl *standard-output*)
		       &aux ($loadprint nil))
  (cond ((not (symbolp name))
	 (merror "~%First argument to `fortmx' must be a symbol."))
	((not ($matrixp mat))
	 (merror "Second argument to `fortmx' not a matrix: ~M" mat)))
  (do ((mat (cdr mat) (cdr mat)) (i 1 (f1+ i))) ((null mat))
    (declare (fixnum i))
    (do ((m (cdar mat) (cdr m)) (j 1 (f1+ j))) ((null m))
      (declare (fixnum j))
      (fortran-print `((mequal) ((,name) ,i ,j) ,(car m)) stream)))
  '$done)


;; Local Modes:
;; Comment Column:26
;; End:
