;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module array)

;;; Macsyma User array utilities originally due to CFFK.

(defun $listarray (ary)
       (Cons '(mlist)
	     (cond ((mget ary 'hashar)
		    (mapcar #'(lambda (subs) ($arrayapply ary subs))
			    (cdddr (meval (list '($arrayinfo) ary)))))
		   ((mget ary 'array) (listarray (mget ary 'array)))
		   #+cl
		   ((arrayp ary) (coerce ary 'list))
		   #+cl
  		   ( 
		    (hash-table-p ary)
		    (let (vals (tab ary))
		      (declare (special vals tab))
		       (maphash #'(lambda (x &rest l)l (push (gethash x tab) vals)) ary )
		       vals))
		   (t 
		    (merror "Argument to LISTARRAY must be an array:~%~M" ary)))))

(defmfun $fillarray (ary1 ary2)
      (let ((ary
	      (or
	       (mget ary1 'array)
	       #+cl
	       (and (arrayp ary1) ary1)
	       (merror "First argument to FILLARRAY must be a declared array:~%~M" ary1))))
	    (replace
	     ary
	     (cond (($listp ary2) (cdr ary2))
		   ((mget ary2 'array))
		   #+cl
		   ((arrayp ary2) ary2)
		   (t
		    (merror
		     "Second argument to FILLARRAY must be an array or list:~%~M" ary2))))
	    ary1))
;#+cl
;(defmacro $rearray (ar &rest dims)
;  `(cond ($use_fast_arrays (setq ,ar (rearray-aux ',ar ,(safe-value ar) ,@ dims)))
;	 (t (rearray-aux ',ar (safe-value ,ar) ,@ dims))))

(defun getvalue (sym)
  (and (symbolp sym) (boundp sym) (symbol-value sym)))
(defmspec $rearray (l) (setq l (cdr l))
  (let ((ar (car l)) (dims (cdr l)))
    (cond ($use_fast_arrays (set ar (rearray-aux ar (getvalue ar) dims )))
	  (t (rearray-aux ar (getvalue ar) dims)))))

#+cl
(defun rearray-aux (ar val dims &aux marray-sym)
  (cond ((arrayp val)
	 (apply 'lispm-rearray val dims))
	((arrayp (symbol-array ar))
	 (setf (symbol-array ar)
	       (apply 'lispm-rearray (symbol-array ar ) dims)))
	((setq marray-sym (mget ar 'array))
	 (apply 'rearray-aux  marray-sym nil dims ) ar)
	(t (error "unknown array ~A " ar))))

#-cl
(defmspec $rearray (l) (setq l (cdr l))
	  (cond ((> (length l) 6)
		 (merror "Too many arguments to REARRAY:~%~M" l))
		((< (length l) 2)
		 (merror "Too few arguments to REARRAY:~%~M" l)))
	  (let ((name (car l))
		(ary (cond ($use_fast_arrays
			   (symbol-value (car l)))
		     (t
		       (cond ((mget (car l) 'array))
			     (t 
			      (merror "First argument to REARRAY must be a declared array:~%~M"
				      (car l))))))))
	    (setq l (cdr l)
		  l (mapcar #'(lambda (x)
				(setq x (meval x))
				(cond ((not (eq (ml-typep x) 'fixnum))
				       (merror
					 "Non-integer dimension to REARRAY:~%~M"
					 x)))
				#-cl
				(f1+ x)
				#+cl x
				)
			    l))
	    (show l)
	    #-lispm
	    (let ((new-array 
		    (apply '*rearray (cons ary 
					   (cons (car (arraydims ary)) l)))))
	      #+Franz(mputprop name new-array 'array)
	      )
	    #+lispm
	    (progn
	      (cond ($use_fast_arrays
		     (setq ary (apply 'lispm-rearray (cons ary l))))
		    (t (setf (symbol-function ary) (apply 'lispm-rearray (cons (symbol-function ary) l)))))
	      (cond ($use_fast_arrays (setq name ary))
		    (t (mputprop name ary 'array))))
	    name))


;(defmspec $rearray (l) (setq l (cdr l))
;	  (cond ((> (length l) 6)
;		 (merror "Too many arguments to REARRAY:~%~M" l))
;		((< (length l) 2)
;		 (merror "Too few arguments to REARRAY:~%~M" l)))
;	  (let ((name (car l))
;		(ary (cond ($use_fast_arrays
;			   (symbol-value (car l)))
;		     (t
;		       (cond ((mget (car l) 'array))
;			     (t 
;			      (merror "First argument to REARRAY must be a declared array:~%~M"
;				      (car l))))))))
;	    (setq l (cdr l)
;		  l (mapcar #'(lambda (x)
;				(setq x (meval x))
;				(cond ((not (eq (ml-typep x) 'fixnum))
;				       (merror
;					 "Non-integer dimension to REARRAY:~%~M"
;					 x)))
;				#-lispm
;				(f1+ x)
;				#+Lispm x
;				)
;			    l))
;	    (show l)
;	    #-lispm
;	    (let ((new-array 
;		    (apply '*rearray (cons ary 
;					   (cons (car (arraydims ary)) l)))))
;	      #+Franz(mputprop name new-array 'array)
;	      )
;	    #+lispm
;	    (progn
;	      (cond ($use_fast_arrays
;		     (setq ary (apply 'lispm-rearray (cons ary l))))
;		    (t (setf (symbol-function ary) (apply 'lispm-rearray (cons (symbol-function ary) l)))))
;	      (cond ($use_fast_arrays (setq name ary))
;		    (t (mputprop name ary 'array))))
;	    name))

#+cl
;(defun lispm-rearray (ar &rest dims)
; ( make-array (mapcar '1+ (copy-list dims)) :element-type (array-element-type ar) :displaced-to ar ))


(defun lispm-rearray (ar &rest dims)
  (cond ((eql (array-rank ar) (length dims))
	 (adjust-array ar (mapcar '1+ (copy-list dims)) :element-type (array-element-type ar)  ))
	(t (merror "Rearray only works for arrays with same rank ie number of subscripts"))))



