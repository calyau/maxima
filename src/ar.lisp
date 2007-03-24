;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module ar)

(declare-top (special evarrp munbound flounbound fixunbound $use_fast_arrays))

(defstruct (mgenarray (:conc-name mgenarray-) (:type vector))
  aref
  aset
  type
  null
  generator
  content)

(defun marray-type (x)
  (case (ml-typep x)
    (array (array-element-type x))
    (hash-table 'hash-table)
    (cl:array  (princ "confusion over `array' and `cl:array'")
		  (array-element-type x))
    (otherwise
     (or (cdr (assoc (array-element-type x)
		    '((flonum . $float)
		      (fixnum . $fixnum)) :test #'eq))
	 (mgenarray-type x)))))


(defmfun $make_array (type &rest diml)
  (let ((ltype (assoc type '(($float . flonum) ($flonum . flonum)
			    ($fixnum . fixnum)) :test #'eq)))
    (cond ((not ltype)
	   (cond ((eq type '$any)
		  (make-array diml :initial-element nil))
		 ((eq type '$hashed)
		  (let ((kludge (gensym)))
		    (or (integerp (car diml))
			(merror "non-integer number of dimensions: ~M"
				(car diml)))
		    (insure-array-props kludge () (car diml))
		    (make-mgenarray :type '$hashed
				    :content kludge)))
		 ((eq type '$functional)
		  ;; MAKE_ARRAY('FUNCTIONAL,LAMBDA(...),'ARRAY_TYPE,...)
		  (or (> (length diml) 1)
		      (merror "not enough arguments for functional array specification"))
		  (let ((ar (apply #'$make_array (cdr diml)))
			(the-null))
		    (case (marray-type ar)
		      (($fixnum)
		       (fillarray ar (list (setq the-null fixunbound))))
		      (($float)
		       (fillarray ar (list (setq the-null flounbound))))
		      (($any)
		       (fillarray (mgenarray-content ar) (list (setq the-null munbound))))
		      (t
		       ;; Nothing to do for hashed arrays. Is FUNCTIONAL here
		       ;; an error?
		       (setq the-null 'notexist)))
		    (make-mgenarray :type '$functional
				    :content ar
				    :generator (car diml)
				    :null the-null)))
		 ('else
		  (merror "Array type of ~M is not recognized by `make_array'" type))))
	  ('else
	   (apply '*array nil (cdr ltype) diml)))))

(defmfun maknum (x)
  (cond ($use_fast_arrays
	 (exploden (format nil "~A" x)))
	(t (format nil "~A" x))))

(defmfun dimension-array-object (form result &aux (mtype (marray-type form)))
  (cond ($use_fast_arrays (dimension-string  (maknum form) result))
	(t
	 (dimension-string
	  (nconc (exploden "{Array: ")
		 (cdr (exploden mtype))
		 (exploden " ")
		 (exploden (maknum form))
		 (if (memq mtype '($float $fixnum $any))
		     (nconc (exploden "[")
			    (do ((l (cdr (arraydims (if (memq mtype '($float $fixnum))
							form
							(mgenarray-content form))))
				    (cdr l))
				 (v nil
				    (nconc (nreverse (exploden (car l))) v)))
				((null l) (nreverse v))
			      (if v (push #\, v)))
			    (exploden "]")))
		 (exploden "}"))
	  result))))



(defun marray-check (a)
  (if (eq (ml-typep a) 'array)
      (case (marray-type a)
	((art-q ) a)
	(($fixnum $float) a)
	(($any) (mgenarray-content a))
	(($hashed $functional)

	 ;; BUG: It does have a number of dimensions! Gosh. -GJC
	 (merror "Hashed array has no dimension info: ~M" a))
	(t
	 (marray-type-unknown a)))
      (merror "Not an array: ~M" a)))

(defmfun $array_dimension_n (n a)
  (array-dimension  (marray-check a) n))

(defun marray-type-unknown (x)
  (merror "Bug: Array of unhandled type: ~S" x))

(defun marrayref-gensub (aarray ind1 inds)
  (case (marray-type aarray)
    ;; We are using a CASE on the TYPE instead of a FUNCALL, (or SUBRCALL)
    ;; because we are losers. All this stuff uses too many functions from
    ;; the "MLISP" modual, which are not really suitable for the kind of
    ;; speed and simplicity we want anyway. Ah me. Also, passing the single
    ;; unconsed index IND1 around is a dubious optimization, which causes
    ;; extra consing in the case of hashed arrays.
    ((t) (apply #'aref aarray ind1 inds))
    ((hash-table) (gethash (if inds (cons ind1 inds) ind1) aarray))
    (($hashed)
     (apply #'marrayref (mgenarray-content aarray) ind1 inds))
    (($float $fixnum)
     (apply aarray ind1 inds))
    (($any)
     (apply (mgenarray-content aarray) ind1 inds))
    (($functional)
     (let ((value (let ((evarrp t))
		    ;; special variable changes behavior of hashed-array
		    ;; referencing functions in case of not finding an element.
		    (catch 'evarrp (marrayref-gensub
				    (mgenarray-content aarray) ind1 inds)))))
       (if (equal value (mgenarray-null aarray))
	   (marrayset-gensub  (apply #'mfuncall
				     (mgenarray-generator aarray)
				     ;; the first argument we pass the
				     ;; function is a SELF variable.
				     aarray
				     ;; extra consing here! LEXPR madness.
				     ind1
				     inds)
			      (mgenarray-content aarray) ind1 inds)
	   value)))
    (t
     (marray-type-unknown aarray))))

(defmfun $make_art_q (&rest l)
  (make-array l))

(defun marrayset-gensub (val aarray ind1 inds)
  (case (marray-type aarray)
    ((t) (setf (apply #'aref aarray ind1 inds) val))
    (($hashed)
     (apply #'marrayset val (mgenarray-content aarray) ind1 inds))
    (($any)
     (setf (apply #'aref (mgenarray-content aarray) ind1 inds) val ))
    (($float $fixnum)
     (setf  (apply #'aref (mgenarray-content aarray) ind1 inds) val ))
    (($functional)
     (marrayset-gensub val (mgenarray-content aarray) ind1 inds))
    (t
     (marray-type-unknown aarray))))

;; Extensions to MEVAL.

(defmfun meval1-extend (form)
  (let ((l (mevalargs (cdr form))))
    (marrayref-gensub (caar form) (car l) (cdr l))))

(defmfun arrstore-extend (a l r)
  (marrayset-gensub r a (car l) (cdr l)))
