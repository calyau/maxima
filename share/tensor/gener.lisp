;;; -*- Mode:LISP; Package:MACSYMA -*-
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments: Code to generate ctensor programs from itensor expressions
;;

;	** (c) Copyright 1979 Massachusetts Institute of Technology **
(in-package "MAXIMA")


(declare-top (special $imetric $metricconvert indlist empty))

;$METRICCONVERT if non-NIL will allow $IC_CONVERT to rename the metric tensor
;   ($IMETRIC must be bound) with 2 covariant indices to LG and with 2
;   contravariant indices to UG.

(defun $IC_CONVERT (e)
       (prog (free lhs rhs)
	     (cond ((or (atom e) (not (eq (caar e) 'MEQUAL)))
		    (merror "IC_CONVERT requires an equation as an argument"))
		   ((equal (setq free ($indices e)) empty)
		    (return (cons '(MSETQ) (cdr e))))
		   ((or (eq (ml-typep (cadr e)) 'SYMBOL)           ;If a symbol or
			(and (rpobj (cadr e))  ;an indexed object with no dummy
			     (null (cdaddr ($indices2 (cadr e))))))    ;indices
		    (setq lhs (cadr e) rhs (caddr e)))
		   ((or (eq (ml-typep (caddr e)) 'SYMBOL)
			(and (rpobj (caddr e))
			     (null (cdaddr ($indices2 (caddr e))))))
		    (setq lhs (caddr e) rhs (cadr e)))
		   (t (merror "At least one side of the equation must be a~
			      ~%symbol or a single indexed object")))
	     (cond ((and (not (eq (ml-typep lhs) 'SYMBOL))
			 (not (null (cdddr lhs))))
		    (merror "Cannot assign to indexed objects with derivative ~
			    indices:~%~M"
			    (ishow lhs))))
	     (setq free (nreverse (itensor-sort (cdadr free)))  ;Set FREE to just the
		   indlist nil)                           ;free indices
	     (and $METRICCONVERT (boundp '$IMETRIC)
		  (setq lhs (changename $IMETRIC t 0 2 '$UG
					(changename $IMETRIC t 2 0 '$LG lhs))
			rhs (changename $IMETRIC t 0 2 '$UG
					(changename $IMETRIC t 2 0 '$LG rhs))))
	     (tabulate rhs)
	     (setq indlist (unique indlist))
	     (do ((q (mapcar 'car indlist) (cdr q)))
		 ((null q))
		 (cond ((memq (car q) (cdr q))
			(merror "~
IC_CONVERT cannot currently handle indexed objects of the same name~
~%with different numbers of covariant and//or contravariant indices:~%~M"
				(car q)))))
	     (cond ((not (eq (ml-typep lhs) 'SYMBOL))
		    (do ((test) (flag) (name))
			(flag)
			(setq test (list (caar lhs) (length (cdadr lhs))
					 (length (cdaddr lhs))))
			(cond ((or (zl-member test indlist)
				   (not (memq (car test)
					      (mapcar 'car indlist))))
			       (setq flag t))
			      (t
			       (mtell "Assignment is to be made to ~M~
~%This name with a different number of covariant and//or contravariant~
~%indices appears on the other side of the equation. To avoid array name~
~%conflicts, choose a new name for this object:~%"
				      (ishow lhs))
                               (cond ((not (eq (ml-typep
						(setq name
						      (retrieve nil nil)))
					       'SYMBOL))
				      (merror "Name not an atom")))
			       (setq lhs (cons (ncons name) (cdr lhs))))))))
	     (return (do ((free free (cdr free))
			  (equ (cons '(MSETQ) (list (changeform lhs)
						    (t-convert
						     (summer1 rhs))))))
			 ((null free) equ)
			 (setq equ (append '((MDO)) (ncons (car free))
					   '(1 1 NIL $DIM NIL)
					   (ncons equ)))))))

(defun TABULATE (e)        ;For each indexed object in E, appends a list of the
       (cond ((atom e))    ;name of that object and the number of covariant and
	     ((rpobj e)    ;contravariant indices to the global list INDLIST
	      (setq indlist (cons (list (caar e) (length (cdadr e))
					(length (cdaddr e)))
				  indlist)))
	     ((or (eq (caar e) 'MPLUS) (eq (caar e) 'MTIMES))
	      (mapcar 'tabulate (cdr e)))))

(defun UNIQUE (l)                   ;Returns a list of the unique elements of L
       (do ((a l (cdr a)) (b))
	   ((null a) b)
	   (cond ((not (zl-member (car a) b))
		  (setq b (cons (car a) b))))))

(defun SUMMER1 (e)     ;Applies SUMMER to the products and indexed objects in E
       (cond ((atom e) e)
	     ((eq (caar e) 'MPLUS)
  	      (cons (car e) (mapcar 'summer1 (cdr e))))
	     ((or (eq (caar e) 'MTIMES) (rpobj e))
	      (summer e (cdaddr ($indices e))))
	     (t e)))

(defun SUMMER (p dummy) ;Makes implicit sums explicit in the product or indexed
                        ;object P where DUMMY is the list of dummy indices of P
       (prog (dummy2 scalars indexed s)                          ;at this level
	     (setq dummy2 (intersect (all ($indices2 p)) dummy))
	     (do ((p (cond ((eq (caar p) 'MTIMES) (cdr p))
			   (t (ncons p))) (cdr p))
		  (obj))
		 ((null p))
		 (setq obj (car p))
		 (cond ((atom obj)
			(setq scalars (cons obj scalars)))
		       ((rpobj obj)
			(cond ((null (intersect dummy2 (all ($indices2 obj))))
			       (setq scalars (cons obj scalars)))
			      (t (setq indexed (cons obj indexed)))))
		       ((eq (caar obj) 'MPLUS)
			(setq s t)
			(cond ((null (intersect dummy (all ($indices obj))))
			       (setq scalars
				     (cons (summer1 obj) scalars)))
			      (t (setq indexed
				       (cons (summer1 obj) indexed)))))
		       (t (setq scalars (cons obj scalars)))))
	     (cond ((and s
			 (not (samelists dummy2
					 (setq s
					       (cdaddr
						($indices
						 (append '((MTIMES))
							 scalars indexed)))))))
		    (setq dummy2 s
			  s scalars
			  scalars nil)
		    (do ((p s (cdr p)) (obj))
			((null p))
			(setq obj (car p))
			(cond ((null (intersect dummy2 (all ($indices obj))))
			       (setq scalars (cons obj scalars)))
			      (t (setq indexed (cons obj indexed)))))))
	     (return
	      (simptimes
	       (nconc (ncons '(MTIMES))
		      scalars
		      (cond ((not (null indexed))
		             (do ((indxd (simptimes (cons '(MTIMES) indexed)
						    1 nil))
			          (dummy (itensor-sort dummy2) (cdr dummy)))
			         ((null dummy) (ncons indxd))
			         (setq indxd (nconc (ncons '($SUM))
						    (ncons indxd)
						    (ncons (car dummy))
						    '(1 $DIM)))))
			    (t nil)))
	       1 nil))))

(defun ALL (l)                        ;Converts [[A, B], [C, D]] into (A B C D)
;       (append (cdadr l) (cdaddr l)))
        (append (covi l) (conti l)))

(defun T-CONVERT (e)        ;Applies CHANGEFORM to each individual object in an
       (cond ((atom e) e)   ;expression
	     ((or (eq (caar e) 'MPLUS) (eq (caar e) 'MTIMES))
	      (cons (car e) (mapcar 't-convert (cdr e))))
	     ((eq (caar e) '$SUM)
	      (append (ncons (car e)) (ncons (t-convert (cadr e))) (cddr e)))
	     (t (changeform e))))

(defun CHANGEFORM (e)           ;Converts a single object from ITENSOR format to
       (cond ((atom e) e)       ;ETENSR format
	     ((rpobj e)
	      (do ((deriv (cdddr e) (cdr deriv))
;		   (new (cond ((and (null (cdadr e)) (null (cdaddr e)))
		   (new (cond ((and (null (covi e)) (null (conti e)))
			       (caar e))     ;If no covariant and contravariant
			                     ;indices then make into an atom
			      (t (cons (cons (equiv-table (caar e)) '(ARRAY))
;				       (append (cdadr e) (cdaddr e)))))))
				       (append (covi e) (conti e)))))))
		  ((null deriv) new)
		  (setq new (append '(($DIFF)) (ncons new)
				    (ncons (cons '($CT_COORDS ARRAY)
						 (ncons (car deriv))))))))
	     (t e)))

(defun EQUIV-TABLE (a)                ;Makes appropiate name changes converting
       (cond ((memq a '($ICHR1 %ICHR1)) '$LCS)            ;from ITENSOR to ETENSR
	     ((memq a '($ICHR2 %ICHR2)) '$MCS)
	     (t a)))

(declare-top (unspecial indlist))

(declare-top (special SMLIST $FUNCS))
(setq $funcs '((MLIST)))

(DEFUN $MAKEBOX (E NAME)
       (COND ((ATOM E) E)
	     ((MTIMESP E) (MAKEBOX E NAME))
	     ((MPLUSP E)
	      (MYSUBST0 (SIMPLIFYA (CONS '(MPLUS)
					 (MAPCAR
					  (FUNCTION
					   (LAMBDA (Q) ($MAKEBOX Q NAME)))
					  (CDR E)))
				   NIL)
			E))
	     ((MEXPTP E) (LIST (CAR E) ($MAKEBOX (CADR E) NAME) (CADDR E)))
	     (T E))) 

(DEFUN MAKEBOX (E NAME)
       (PROG (L1 L2 X L3 L) 
	     (SETQ L (CDR E))
	AGAIN(SETQ X (CAR L))
	     (COND ((RPOBJ X)
		    (COND ((AND (EQ (CAAR X) NAME) (NULL (CDDDR X))
				(NULL (CDADR X)) (= (LENGTH (CDADDR X)) 2))
			   (SETQ L1 (CONS X L1)))
			  ((CDDDR X) (SETQ L2 (CONS X L2)))
			  (T (SETQ L3 (CONS X L3)))))
		   (T (SETQ L3 (CONS X L3))))
	     (AND (SETQ L (CDR L)) (GO AGAIN))
	     (COND ((OR (NULL L1) (NULL L2)) (RETURN E)))
	     (DO ((L2 L2 (CDR L2)))
		 ((NULL L2) )
		 (SETQ L L1)
;	     (DO L2 L2 (CDR L2)
;	      (NULL L2)
;	      (SETQ L L1)..)

	     (tagbody
	      LOOP
	      (SETQ X (CAR L))
	      (COND
	       ((AND (MEMQ (CAR (CDADDR X)) (CDDDAR L2))
		     (MEMQ (CADR (CDADDR X))(CDDDAR L2)))
		(SETQ 
		 L3
		 (CONS (NCONC
		  (LIST
		   (NCONS
		    (IMPLODE (APPEND '([ ])
				    (CDR (EXPLODEC (CAAAR L2))))))
		   (CADAR L2)
		   (CADDAR L2)) (SETDIFF (CDDDAR L2)(CDADDR X)))
		  L3))
		(SETQ L1 (ZL-DELETE X L1 1.)))
	       ((SETQ L (CDR L)) (GO LOOP))
	       (T (SETQ L3 (CONS (CAR L2) L3))))))
	     (RETURN (SIMPTIMES (CONS '(MTIMES) (NCONC L1 L3))
				1.
				NIL)))) 

(DECLARE-TOP (SPECIAL TENSR))

(DEFmfUN $AVERAGE N ((LAMBDA (TENSR) (SIMPLIFYA (AVERAGE (ARG 1)) NIL))
		   (AND (= N 2) (ARG 2))))

(DEFUN AVERAGE (E)
       (COND ((ATOM E ) E)
	     ((RPOBJ E) (COND ((OR (NOT TENSR) (EQ (CAAR E) TENSR))
			       (AVERAGE1 E))
			      (T E)))
	     (T (CONS (NCONS (CAAR E)) (MAPCAR (FUNCTION AVERAGE) (CDR E))))))

(DEFUN AVERAGE1 (E)
       (COND ((= (LENGTH (CDADR E)) 2)
	      (SETQ E (LIST '(MTIMES) '((RAT SIMP) 1 2)
			    (LIST '(MPLUS)
				  (CONS (CAR E)
					(CONS (AREV (CADR E)) (CDDR E))) E))))
	     ((= (LENGTH (CDADDR E)) 2)
	      (SETQ E (LIST '(MTIMES) '((RAT SMP) 1 2)
			    (LIST '(MPLUS)
				  (CONS (CAR E)
					(CONS (CADR E)
					      (CONS (AREV (CADDR E))
						    (CDDDR E)))) E)))))
       E)

(DEFUN AREV (L) (LIST (CAR L) (CADDR L) (CADR L)))

(DECLARE-TOP (UNSPECIAL TENSR))
(add2lnc '(($AVERAGE) $TENSOR) $funcs)

(defun $CONMETDERIV (e g)
       (cond ((not (eq (ml-typep g) 'SYMBOL))
	      (merror "Invalid metric name: ~M" g))
	     (t (conmetderiv e g ((lambda (l) (append (cdadr l) (cdaddr l)))
				  ($indices e))))))

(defun CONMETDERIV (e g indexl)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (eq (caar e) g) (null (cdadr e))
			  (equal (length (cdaddr e)) 2)
			  (not (null (cdddr e))))
		     (do ((e (cmdexpand (car e) (car (cdaddr e))
					(cadr (cdaddr e)) (cadddr e) indexl))
			  (deriv (cddddr e) (cdr deriv)))
			 ((null deriv) e)
			 (setq e (conmetderiv ($idiff e (car deriv))
					      g indexl))))
		    (t e)))
	     (t (mysubst0 (cons (car e)
				(mapcar
				 (function (lambda (q) 
						   (conmetderiv q g indexl)))
				 (cdr e))) e))))

(defun CMDEXPAND (g i j k indexl)
       (do ((dummy) (flag))
	   (flag (list '(MPLUS SIMP)
		       (list '(MTIMES SIMP) -1
			     (list g (ncons SMLIST) (list SMLIST dummy i))
			     (list '($ICHR2 SIMP) (list SMLIST dummy k)
				   (list SMLIST j)))
		       (list '(MTIMES SIMP) -1
			     (list g (ncons SMLIST) (list SMLIST dummy j))
			     (list '($ICHR2 SIMP) (list SMLIST dummy k)
				   (list SMLIST i)))))
	   (setq dummy ($idummy))
	   (and (not (memq dummy indexl)) (setq flag t))))

(add2lnc '(($CONMETDERIV) $EXP $NAME) $funcs)

(defun $FLUSH1DERIV (e g)
       (cond ((not (eq (ml-typep g) 'SYMBOL))
	      (merror "Invalid metric name: ~M" g))
	     (t (flush1deriv e g))))

(defun FLUSH1DERIV (e g)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (eq (caar e) g) (equal (length (cdddr e)) 1)
			  (or (and (equal (length (cdadr e)) 2)
				   (null (cdaddr e)))
			      (and (equal (length (cdaddr e)) 2)
				   (null (cdadr e)))))
		     0)
		    (t e)))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar
			       (function (lambda (q) (flush1deriv q g)))
			       (cdr e))) e))))

(add2lnc '(($FLUSH1DERIV) $EXP $NAME) $funcs)

(defun $IGEODESIC_COORDS (exp g)
       ($flush1deriv ($flush exp '$ICHR2 '%ICHR2) g))

(add2lnc '(($IGEODESIC_COORDS) $EXP $NAME) $funcs)


