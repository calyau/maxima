;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module ar)

(declare-top (SPECIAL EVARRP MUNBOUND FLOUNBOUND FIXUNBOUND #+cl $use_fast_arrays))

;;; This code needs to be checked carefully for the lispm.



(defstruct #-cl (mgenarray conc-name array)
	   #+cl (mgenarray (:conc-name mgenarray-) (:type vector))
  aref
  aset
  type
  NULL
  GENERATOR
  CONTENT)

#-cl
(DEFUN MARRAY-TYPE (X)
  
  (OR (CDR (ASSQ (ARRAY-TYPE X)
		 '((FLONUM . $FLOAT)
		   (FIXNUM . $FIXNUM))))
      (MGENARRAY-TYPE X)))
#+cl
(DEFUN MARRAY-TYPE (X)
  (case (ml-typep x)
    (array (array-element-type x))
    (hash-table 'hash-table)
    #+lispm (si::equal-hash-table 'hash-table)
    (lisp::array  (princ "confusion over array and lisp::array")
	    (array-element-type x))
    (otherwise
 
  (OR (CDR (ASSQ (array-type x)
		 '((FLONUM . $FLOAT)
		   (FIXNUM . $FIXNUM))))
      (MGENARRAY-TYPE X)))))

;#+lispm
;(defmfun $Show_hash_array (table)
;  (send table :map-hash
;   `(lambda (u v) 
;    (format t "~%~A-->~A" u v)))
;  table)

(DEFMFUN $MAKE_ARRAY (TYPE &REST DIML)
  (LET ((LTYPE (ASSQ TYPE '(($FLOAT . FLONUM) ($FLONUM . FLONUM)
					      ($FIXNUM . FIXNUM)))))
    (COND ((NOT LTYPE)
	   (COND ((EQ TYPE '$ANY)
		  #+cl (make-array diml)
		  #-cl
		  (MAKE-MGENARRAY  #+cl :type #-cl type  '$ANY
				  #+cl :CONTENT #-cl CONTENT (APPLY '*ARRAY NIL T DIML)))
		 ((EQ TYPE '$HASHED)
		  (LET ((KLUDGE (GENSYM)))
		    (OR (INTEGERP (CAR DIML))
			(MERROR "non-integer number of dimensions: ~M"
				(CAR DIML)))
		    (INSURE-ARRAY-PROPS KLUDGE () (CAR DIML))
		    (MAKE-MGENARRAY #+cl :TYPE #-cl TYPE '$HASHED
				    #+cl :CONTENT #-cl CONTENT KLUDGE)))
		 ((EQ TYPE '$FUNCTIONAL)
		  ;; MAKE_ARRAY('FUNCTIONAL,LAMBDA(...),'ARRAY_TYPE,...)
		  (OR (> (LENGTH DIML) 1)
		      (MERROR "not enough arguments for functional array specification"))
		  (LET ((AR (APPLY #'$MAKE_ARRAY (CDR DIML)))
			(THE-NULL))
		    (CASE (MARRAY-TYPE AR)
		      (($FIXNUM)
		       (FILLARRAY AR (LIST (SETQ THE-NULL FIXUNBOUND))))
		      (($FLOAT)
		       (FILLARRAY AR (LIST (SETQ THE-NULL FLOUNBOUND))))
		      (($ANY)
		       (FILLARRAY (MGENARRAY-CONTENT AR) (LIST (SETQ THE-NULL MUNBOUND))))
		      (T
		       ;; Nothing to do for hashed arrays. Is FUNCTIONAL here
		       ;; an error?
		       (SETQ THE-NULL 'NOTEXIST)))
		    (MAKE-MGENARRAY #+cl :TYPE #-cl TYPE '$FUNCTIONAL
				    #+cl :CONTENT #-cl CONTENT AR
				    #+cl :GENERATOR #-cl GENERATOR (CAR DIML)
				    #+cl :NULL #-cl NULL THE-NULL)))
		 ('ELSE
		  (MERROR "Array type of ~M is not recognized by MAKE_ARRAY" TYPE))))
	  ('ELSE
	   (APPLY '*ARRAY NIL (CDR LTYPE) DIML)))))
#+cl
(defmfun maknum (x)
  (cond ($use_fast_arrays
  (exploden (format nil "~A" x)))
	(t (format nil "~A" x))))

(DEFMFUN DIMENSION-ARRAY-OBJECT (FORM RESULT &AUX (MTYPE (MARRAY-TYPE FORM)))
  (cond ($use_fast_arrays (dimension-string  (maknum form) result))
	(t
	 (DIMENSION-STRING
	   (NCONC (EXPLODEN "{Array: ")
		  (CDR (EXPLODEN MTYPE))
		  (EXPLODEN " ")
		  (EXPLODEN (MAKNUM FORM))
		  (IF (MEMQ MTYPE '($FLOAT $FIXNUM $ANY))
		      (NCONC (EXPLODEN "[")
			     (DO ((L (CDR (ARRAYDIMS (IF (MEMQ MTYPE '($FLOAT $FIXNUM))
							 FORM
							 (MGENARRAY-CONTENT FORM))))
				     (CDR L))
				  (V NIL
				     (NCONC (NREVERSE (EXPLODEN (CAR L))) V)))
				 ((NULL L) (NREVERSE V))
			       (IF V (PUSH #\, V)))
			     (EXPLODEN "]")))
		  (EXPLODEN "}"))
	   RESULT))))



(DEFUN MARRAY-CHECK (A)
  (IF (EQ (ml-typep A) 'array)
      (CASE (MARRAY-TYPE A)
	((art-q ) a)
	(($FIXNUM $FLOAT) A)
	(($ANY) (MGENARRAY-CONTENT A))
	(($HASHED $FUNCTIONAL)
	
	 ;; BUG: It does have a number of dimensions! Gosh. -GJC
	 (MERROR "Hashed array has no dimension info: ~M" A))
	(T
	 (MARRAY-TYPE-UNKNOWN A)))
      (MERROR "Not an array: ~M" A)))

(DEFMFUN $ARRAY_NUMBER_OF_DIMENSIONS (A)
  (ARRAY-/#-DIMS (MARRAY-CHECK A)))

(DEFMFUN $ARRAY_DIMENSION_N (N A)
  #-cl(ARRAY-DIMENSION-N N (MARRAY-CHECK A))
  #+cl(array-dimension  (MARRAY-CHECK A) n)
  )

(DEFUN MARRAY-TYPE-UNKNOWN (X)
  (MERROR "BUG: Array of unhandled type: ~S" X))

(DEFUN MARRAYREF-GENSUB (AARRAY IND1 INDS)  
       (CASE (MARRAY-TYPE AARRAY)
    ;; We are using a CASE on the TYPE instead of a FUNCALL, (or SUBRCALL)
    ;; because we are losers. All this stuff uses too many functions from
    ;; the "MLISP" modual, which are not really suitable for the kind of
    ;; speed and simplicity we want anyway. Ah me. Also, passing the single
    ;; unconsed index IND1 around is a dubious optimization, which causes
    ;; extra consing in the case of hashed arrays.
#+cl((t) (apply #'aref aarray ind1 inds))
#+cl((hash-table) (gethash (if inds (cons ind1 inds) ind1) aarray))
    (($HASHED)
     (APPLY #'MARRAYREF (MGENARRAY-CONTENT AARRAY) IND1 INDS))
    (($FLOAT $FIXNUM)
     (APPLY AARRAY IND1 INDS))
    (($ANY)
     (APPLY (MGENARRAY-CONTENT AARRAY) IND1 INDS))
    (($FUNCTIONAL)
     (LET ((VALUE (LET ((EVARRP T))
		    ;; special variable changes behavior of hashed-array
		    ;; referencing functions in case of not finding an element.
		    (CATCH 'EVARRP (MARRAYREF-GENSUB
				      (MGENARRAY-CONTENT AARRAY) IND1 INDS)))))
       (IF (EQUAL VALUE (MGENARRAY-NULL AARRAY))
	   (MARRAYSET-GENSUB  (APPLY #'MFUNCALL
					     (MGENARRAY-GENERATOR AARRAY)
					     ;; the first argument we pass the
					     ;; function is a SELF variable.
					     AARRAY
					     ;; extra consing here! LEXPR madness.
					     IND1
					     INDS)
			      (MGENARRAY-CONTENT AARRAY) IND1 INDS)
	   VALUE)))
    (T
     (MARRAY-TYPE-UNKNOWN AARRAY))))
	  
(defmfun $Make_art_q (&rest l)
    (make-array l))

(DEFUN MARRAYSET-GENSUB (VAL AARRAY IND1 INDS) 
  (CASE (MARRAY-TYPE AARRAY)
    #+cl
    ((t) (setf (apply #'aref aarray ind1 inds) val))
    (($HASHED)
     (APPLY #'MARRAYSET VAL (MGENARRAY-CONTENT AARRAY) IND1 INDS))
    (($ANY)
     #-cl(STORE (APPLY (MGENARRAY-CONTENT AARRAY) IND1 INDS) VAL)
     #+cl
     (setf (apply #'Aref (MGENARRAY-CONTENT AARRAY) IND1 INDS) val ))
     
    (($FLOAT $FIXNUM)
     #-cl(STORE (APPLY AARRAY IND1 INDS) VAL)
     #+cl     (setf  (apply #'Aref (MGENARRAY-CONTENT AARRAY) IND1 INDS) val ))
    (($FUNCTIONAL)
     (MARRAYSET-GENSUB VAL (MGENARRAY-CONTENT AARRAY) IND1 INDS))
    (T
      (MARRAY-TYPE-UNKNOWN AARRAY))))


;; Extensions to MEVAL.

(DEFMFUN MEVAL1-EXTEND (FORM)
  (LET ((L (MEVALARGS (CDR FORM))))
    (MARRAYREF-GENSUB (CAAR FORM) (CAR L) (CDR L))))

(DEFMFUN ARRSTORE-EXTEND (A L R)
  (MARRAYSET-GENSUB R A (CAR L) (CDR L)))


