;;; -*- Mode:LISP; Package:MACSYMA -*-
;; (based on itensor.116 ,117)
(in-package "MAXIMA")

(macsyma-module itensor) ;; added 9/24/82 at UCB
;	** (c) Copyright 1981 Massachusetts Institute of Technology **

;    Various functions in Itensr have been parceled out to separate files. A
;    function in one of these files will only be loaded in (automatically) if
;    explicitly used in the Macsyma. (It is necessary to have first loaded in
;    ITENSR FASL for this autoloading to take place.) The current status of
;    these separate files are:

;    Filename          Macsyma Functions
;    --------          -----------------
;    CANTEN FASL       CANTEN, CONCAN, IRPMON
;    GENER FASL        GENERATE, MAKEBOX, AVERAGE, CONMETDERIV, FLUSH1DERIV,
;                      GEODESIC
;    SYMTRY FASL       CANFORM, DECSYM, DISPSYM, REMSYM

#+maclisp(progn
(putprop '$GENERATE '((dsk tensor) gener fasl) 'autoload)
(putprop '$DECSYM '((dsk tensor) symtry fasl) 'autoload)
(putprop '$CANFORM '((dsk tensor) symtry fasl) 'autoload)
(putprop '$CANTEN '((dsk tensor) canten fasl) 'autoload)
(putprop '$MAKEBOX '((dsk tensor) gener fasl) 'autoload)
(putprop '$GEODESIC '((dsk tensor) gener fasl) 'autoload)
(putprop '$CONMETDERIV '((dsk tensor) gener fasl) 'autoload))
#+Franz (progn
(putprop '$GENERATE (concat vaxima-main-dir '|//tensor//gener|) 'autoload)
(putprop '$DECSYM (concat vaxima-main-dir  '|//tensor//symtry| )'autoload)
(putprop '$CANFORM (concat vaxima-main-dir  '|//tensor//symtry| )'autoload)
(putprop '$CANTEN (concat vaxima-main-dir  '|//tensor//canten| )'autoload)
(putprop '$MAKEBOX (concat vaxima-main-dir  '|//tensor//gener| )'autoload)
(putprop '$GEODESIC  (concat vaxima-main-dir '|//tensor//gener| )'autoload)
(putprop '$CONMETDERIV  (concat vaxima-main-dir '|//tensor//gener| )'autoload))

#+cl
(progn
(autof '$GENERATE '|gener|)
(autof '$DECSYM '|symtry|)
(autof '$CANFORM '|symtry|)
(autof '$CANTEN '|canten|)
(autof '$MAKEBOX '|gener|)
(autof '$GEODESIC '|gener|)
(autof '$CONMETDERIV '|gener|)
(autof '$NAME '|canten|)
(autof '$CONTI '|canten|)
(autof '$COVI '|canten|)
(autof '$DERI '|canten|)
)
#+cl
(eval-when (eval compile)
	   (defmacro fixp (x) `(typep ,x 'fixnum)))



#+maclisp ($UUO) 	                        ;Restore calls to SDIFF so it can be redefined	

(DECLARE-TOP (SPECIAL SMLIST $DUMMYX $COORDINATES $METRIC $COUNTER $DIM
		  $CONTRACTIONS $COORD $ALLSYM $METRICCONVERT)
	 (*LEXPR $RENAME $DIFF $COORD $REMCOORD $LORENTZ))

(SETQ $DUMMYX '$%                    ;Prefix for dummy indices
      $COUNTER 0.                    ;Dummy variable numeric indexs
      SMLIST '(MLIST SIMP)           ;Simplified MLIST header
      $COORDINATES NIL               ;Used when differentiating w.r.t. a number
      $COORD '((MLIST SIMP))         ;Objects treated liked coordinates in DIFF
      $ALLSYM T                      ;If T then all indexed objects symmetric
      $METRICCONVERT T)              ;Flag used by $GENERATE

;(DEFUN IFNOT MACRO (CLAUSE) (CONS 'OR (CDR CLAUSE)))
(DEFmacro IFNOT  (&rest CLAUSE) `(or ,@ clause))

;(DEFUN M+OR*OR^P MACRO (CL)
(defmacro M+OR*OR^P (&whole cl &rest ign) ign
       (SUBST (CADR CL)
	      'X
	      '(MEMQ (CAAR X) '(MTIMES MPLUS MEXPT))))

(DEFMFUN $DUMMY nil                              ;Sets arguments to dummy indices
       (progn (setq $COUNTER (1+ $COUNTER))
              (concat $DUMMYX $COUNTER)))

(DEFPROP $KDELTA ((/  . / )) CONTRACTIONS)

;KDELTA has special contraction property because it contracts with any indexed
;object.

(meval '(($DECLARE) %KDELTA $CONSTANT))                        ;So derivative will be zero

(SETQ $DIM 4. $CONTRACTIONS '((MLIST SIMP))) 

(DEFMFUN $DEFCON N            ;Defines contractions: A contracts with B to form C
       ((LAMBDA (A)
	 (ADD2LNC A $CONTRACTIONS)
	 (PUTPROP
	  A
	  (CONS (COND ((= N 1.) '(/  . / ))
		      ((= N 3.) (CONS (ARG 2.) (ARG 3.)))
		      (T (merror "DEFCON takes 1 or 3 arguments")))
		(ZL-GET A 'CONTRACTIONS))
	  'CONTRACTIONS)
	 '$DONE)
	(ARG 1.))) 

(DEFMSPEC $DISPCON (A) (SETQ A (CDR A))
  ;;Displays contraction definitions
       ((LAMBDA (TMP) 
	 (AND (EQ (CAR A) '$ALL) (SETQ A (CDR $CONTRACTIONS)))
	 (CONS
	  SMLIST
	  (MAPCAR 
	   #'(LAMBDA (E) 
	     (COND ((SETQ TMP (ZL-GET E 'CONTRACTIONS))
		    (CONS SMLIST
			  (MAPCAR #'(LAMBDA (Z) 
					   (COND ((EQ (CAR Z)
						      '/ )
						  (LIST SMLIST E))
						 (T (LIST SMLIST
							  E
							  (CAR Z)
							  (CDR Z)))))
				  TMP)))
		   (T '((MLIST SIMP)))))
	   A)))
	NIL)) 

(DEFMSPEC $REMCON (A) (SETQ A (CDR A))
  ;;Removes contraction definitions
       (AND (EQ (CAR A) '$ALL) (SETQ A (CDR $CONTRACTIONS)))
       (CONS SMLIST (MAPC (FUNCTION (LAMBDA (E) (ZL-REMPROP E 'CONTRACTIONS)
					    (DELQ E $CONTRACTIONS)))
			  A)))

(DEFUN GETCON (E)
  ;; Helper to obtain contractions on both the noun and verb form of E
	(COND ((AND (SYMBOLP E) (EQ (GETCHAR E 1) '%))  (ZL-GET ($VERBIFY E) 'CONTRACTIONS))
		(T (ZL-GET E 'CONTRACTIONS))
	)
)

(DEFUN RPOBJ (E)                  ;"True" if an indexed object and not a matrix
       (COND ((AND (NOT (ATOM E)) (EQ (CAAR E) 'MQAPPLY)) (RPOBJ (CDR E)))
	     (T 
       (AND (NOT (ATOM E))
	    (NOT (EQ (CAAR E) '$MATRIX))
	    ($LISTP (CADR E))
	    (COND ((CDDR E) ($LISTP (CADDR E)))
		  (T (NCONC E '(((MLIST SIMP))))))))))
                                          ;Transforms F([...]) into F([...],[])

;RPOBJ is the predicate for indexed objects. In the case of no contravariant
;components, it tacks a null list on.

(deff $tenpr #'rpobj)

(DEFMFUN $METRIC (V) (SETQ $METRIC V) ($DEFCON V) ($DEFCON V V '$KDELTA))

(DEFUN MYSUBST0 (NEW OLD)                  ;To reuse subparts of old expression
       (COND ((ALIKE1 NEW OLD) OLD) (T NEW))) 

(DEFUN COV (A B)                            ;COV gives covariant form of metric
       (COND ((BOUNDP '$METRIC)
	      (MEVAL (LIST (NCONS $METRIC)
			   (LIST SMLIST A B)
			   '((MLIST SIMP)))))
	     (T (merror "Name of metric must be specified"))))

(DEFUN CONTR (A B)                      ;CONTR gives contraviant form of metric
       (COND ((BOUNDP '$METRIC)
	      (MEVAL (LIST (NCONS $METRIC)
			   '((MLIST SIMP))
			   (LIST SMLIST A B))))
	     (T (merror "Name of metric must be specified"))))

(DEFUN DIFFCOV (A B D)
	(COND ((BOUNDP '$METRIC)
		(MEVAL (LIST (NCONS $METRIC)
			   (LIST SMLIST A B)
			   '((MLIST SIMP))
				D
			)

		))
		(T (merror "Name of metric must be specified"))))

;(DEFMFUN $CHR1 (L1)                             ;Christoffel symbol of first kind
;       (PROG (A B C)
;	     (SETQ A (CADDDR L1) B (CADR L1) C (CADDR L1))
;	     (RETURN (LIST '(MTIMES)
;			   '((RAT SIMP) 1. 2.)
;			   (LIST '(MPLUS)
;				 (SDIFF (COV B A) C)
;				 (SDIFF (COV C A) B)
;				 (LIST '(MTIMES)
;				       -1.
;				       (SDIFF (COV B C) A)))))))
(DEFMFUN $CHR1 NARGS
	(PROG (A B C)
		(COND 
;			((> NARGS 2) (RETURN (MEVAL (CONS '$COVDIFF (CONS ($CHR1 (ARG 1) (ARG 2)) (CDDR (LISTIFY NARGS)))))))
			((> NARGS 2) (RETURN (MEVAL (CONS '$DIFF (CONS ($CHR1 (ARG 1) (ARG 2)) (APPLY #'APPEND (MAPCAR #'(LAMBDA (E) (LIST E 1)) (CDDR (LISTIFY NARGS)))))))))
			((> NARGS 1) (AND (EQ 1 (LENGTH (ARG 2))) (RETURN ($CHR1 (ARG 1))))
					(merror "CHR1 cannot have contravariant indices"))
;;				(COND
;;					((NULL (ARG 2)) (merror "CHR1 has no contravariant indices"))
;;					(T (RETURN ($CHR1 (ARG 1))))
;;				)
;;			)
			(T
				(SETQ A (CADDDR (ARG 1)) B (CADR (ARG 1)) C (CADDR (ARG 1)))
				(RETURN (LIST '(MTIMES)
					'((RAT SIMP) 1. 2.)
					(LIST '(MPLUS)
;;						(SDIFF (COV B A) C)
;;						(SDIFF (COV C A) B)
						(DIFFCOV B A C)
						(DIFFCOV C A B)
						(LIST '(MTIMES) -1.
;;							(SDIFF (COV B C) A))))))
							(DIFFCOV B C A))))))
		)
	)
)

;(DEFMFUN $CHR2 (L1 L2)                         ;Christoffel symbol of second kind
;       (PROG (A B C D) 
;	     (SETQ A (CADR L1) B (CADDR L1) C (CADR L2))
;	     (return (do ((flag) (l (append (cdr l1) (cdr l2))))
;			 (flag (LIST '(MTIMES)
;				     (CONTR C D)
;				     ($CHR1 (LIST SMLIST A B D))))
;			 (setq d ($dummy))
;			 (and (not (memq d l)) (setq flag t))))))
(DEFMFUN $CHR2 NARGS
	(PROG (A B C D) 
		(COND
			((> NARGS 2) (RETURN (MEVAL (CONS '$DIFF (CONS ($CHR2 (ARG 1) (ARG 2)) (APPLY #'APPEND (MAPCAR #'(LAMBDA (E) (LIST E 1)) (CDDR (LISTIFY NARGS)))))))))
			(T
				(SETQ A (CADR (ARG 1)) B (CADDR (ARG 1)) C (CADR (ARG 2)))
				(return (do ((flag) (l (append (cdr (ARG 1)) (cdr (ARG 2)))))
					(flag (LIST '(MTIMES)
						(CONTR C D)
						($CHR1 (LIST SMLIST A B D))))
				(setq d ($dummy))
				(and (not (memq d l)) (setq flag t))))))
	)
)

(DEFMFUN $curvature (L1 L2) 
       (PROG (I J K H R) 
	     (setq r ($dummy))
	     (SETQ I (CADR L1) K (CADDR L1) H (CADDDR L1) J (CADR L2))
	     (RETURN (LIST '(MPLUS)
			   (SDIFF (LIST '($CHR2 SIMP)
					(LIST SMLIST I K)
					L2)
				  H)
			   (LIST '(MTIMES)
				 -1.
				 (SDIFF (LIST '($CHR2 SIMP)
					      (LIST SMLIST I H)
					      (LIST SMLIST J))
					K))
			   (LIST '(MTIMES)
				 (LIST '($CHR2 SIMP)
				       (LIST SMLIST I K)
				       (LIST SMLIST R))
				 (LIST '($CHR2 SIMP)
				       (LIST SMLIST R H)
				       L2))
			   (LIST '(MTIMES)
				 -1.
				 (LIST '($CHR2 SIMP)
				       (LIST SMLIST I H)
				       (LIST SMLIST R))
				 (LIST '($CHR2 SIMP)
				       (LIST SMLIST R K)
				       L2)))))) 

(DEFUN COVSUBST (X Y RP)       ;Substitutes X for Y in the covariant part of RP
       (CONS (CAR RP) (CONS (SUBST X Y (CADR RP)) (CDDR RP)))) 

(DEFUN CONSUBST (X Y RP)   ;Substitutes X for Y in the contravariant part of RP
       (CONS (CAR RP)
	     (CONS (CADR RP)
		   (CONS (SUBST X Y (CADDR RP)) (CDDDR RP))))) 

(DEFUN DERSUBST (X Y RP)   ;Substitutes X for Y in the derivative indices of RP
       (NCONC (LIST (CAR RP) (CADR RP) (CADDR RP))
	      (SUBST X Y (CDDDR RP)))) 

(DECLARE-TOP (SPECIAL X TEMP D)) 

(DEFMFUN $COVDIFF NARGS
       (PROG (X E TEMP D I)
	     (AND (< NARGS 2) (merror "COVDIFF must have at least 2 args"))
	     (SETQ I 2 E (ARG 1))
       AGAIN (SETQ X (ARG I) E (COVDIFF E) I (1+ I))
	     (AND (> I NARGS) (RETURN E))
	     (GO AGAIN)))

(DEFUN COVDIFF (E) 
       (setq d ($dummy)) 
       (COND
	((OR (ATOM E) (EQ (CAAR E) 'RAT)) (SDIFF E X))
	((RPOBJ E)
	 (SETQ TEMP (MAPCAR #'(LAMBDA (V) (LIST '(MTIMES)
					       (LIST '($CHR2 SIMP)
						     (LIST SMLIST D X)
						     (LIST SMLIST V))
					       (CONSUBST D V E)))
			    (CDADDR E)))  
	 (SIMPLUS
	  (CONS
	   '(MPLUS)
	   (CONS
	    (SDIFF E X)
	    (COND
	     ((OR (CDADR E) (CDDDR E))
	      (CONS
	       (LIST
		'(MTIMES)
		-1.
		(CONS '(MPLUS)
		      (NCONC (MAPCAR #'(LAMBDA (V) 
					      (LIST '(MTIMES)
						    (LIST '($CHR2 SIMP)
							  (LIST SMLIST
								V
								X)
							  (LIST SMLIST
								D))
						    (COVSUBST D V E)))
				     (CDADR E))
			     (MAPCAR #'(LAMBDA (V) 
					      (LIST '(MTIMES)
						    (LIST '($CHR2 SIMP)
							  (LIST SMLIST
								V
								X)
							  (LIST SMLIST
								D))
						    (DERSUBST D V E)))
				     (CDDDR E)))))
	       TEMP))
	     (T TEMP))))
	  1.
	  T))
 	((EQ (CAAR E) 'MTIMES) (SIMPLUS (COVDIFFTIMES (CDR E) X) 1 T))
	((EQ (CAAR E) 'MPLUS)
	 (SIMPLIFYA (CONS '(MPLUS)
			  (MAPCAR 'COVDIFF (CDR E)))
		    NIL))
	((EQ (CAAR E) 'MEXPT)
	 (SIMPTIMES (LIST '(MTIMES)
			  (CADDR E)
			  (LIST '(MEXPT)
				(CADR E)
				(LIST '(MPLUS) -1. (CADDR E)))
			  ($COVDIFF (CADR E) X))
		    1.
		    NIL))
	((EQ (CAAR E) 'MEQUAL)
	 (LIST (CAR E) (COVDIFF (CADR E)) (COVDIFF (CADDR E))))
	(T 
	   (MERROR "Not acceptable to COVDIFF: ~M"
		   (ISHOW E)))))

(DEFUN COVDIFFTIMES (L X) 
       (PROG (SP LEFT OUT) 
	     (SETQ OUT (NCONS '(MPLUS)))
	LOOP (SETQ SP (CAR L) L (CDR L))
	     (NCONC OUT
		    (LIST (SIMPTIMES (CONS '(MTIMES)
					   (CONS ($COVDIFF SP X)
						 (APPEND LEFT L)))
				     1.
				     T)))
	     (COND ((NULL L) (RETURN OUT)))
	     (SETQ LEFT (NCONC LEFT (NCONS SP)))
	     (GO LOOP))) 

(DECLARE-TOP (UNSPECIAL R TEMP D)) 

(DEFMFUN $LORENTZ n
       (cond ((equal n 0) (merror "LORENTZ requires at least one argument"))
	     ((equal n 1) (lorentz (arg 1) nil))
	     (t (lorentz (arg 1)
			 ((lambda (l) (cond ((sloop for v in  l
						     always (symbolp v)) l)
					    (t (merror
"Invalid tensor name(s) in argument to LORENTZ"))))
			  (listify (f- 1 n)))))))

;Lorentz contraction of E: indexed objects with a derivative index matching a
;contravariant index become 0. If L is NIL then do this for all indexed objects
;otherwise do this only for those indexed objects whose names are members of L.

(defun LORENTZ (e l)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (or (null l) (memq (caar e) l))
			  (intersect (cdaddr e) (cdddr e)))
		     0.)
		    (t e)))
	     (t (mysubst0
		 (simplifya
		  (cons (ncons (caar e))
			(mapcar (function (lambda (q) (lorentz q l)))
				(cdr e)))
		  t) e))))

(DEFUN LESS (X Y)                                         ;Alphanumeric compare
       (COND ((NUMBERP X)
	      (COND ((NUMBERP Y) (< X Y))
		    (T (ALPHALESSP (ASCII X) Y))))
	     (T (COND ((NUMBERP Y) (ALPHALESSP X (ASCII Y)))
		      (T (ALPHALESSP X Y)))))) 

(DECLARE-TOP (SPECIAL CHRISTOFFELS))

(SETQ CHRISTOFFELS '($CHR2 $CHR1 %CHR2 %CHR1))

(DEFMFUN $CONTRACT (E)                                 ;Main contraction function
       (COND ((ATOM E) E)
	     ((RPOBJ E) (CONTRACT5 E))
	     ((EQ (CAAR E) 'MTIMES)
	      (MYSUBST0 (SIMPLIFYA (CONS '(MTIMES) (CONTRACT4 E))
				   T)
			E))
	     ((EQ (CAAR E) 'MPLUS)
	      (MYSUBST0 (SIMPLUS (CONS '(MPLUS)
				       (MAPCAR '$CONTRACT
					       (CDR E)))
				 1.
				 T)
			E))
	     (T (MYSUBST0 (SIMPLIFYA (CONS (CAR E) (MAPCAR '$CONTRACT (CDR E)))
				     NIL) E))))

(DEFUN CONTRACT5 (E) 
       ((LAMBDA (K) (COND ((AND (NOT (MEMQ (CAAR E) CHRISTOFFELS)) K)
			   (NCONC (LIST (CAR E)
					(CONS SMLIST (CAR K))
					(CONS SMLIST (CDR K)))
				  (CDDDR E)))
			  (T E)))
	(CONTRACT2 (CDADR E) (CDADDR E)))) 

;L1 and L2 are lists. This function removes all like members from L1 and L2 and
;returns their cons or returns NIL if there aren't any like members.

(DEFUN CONTRACT2 (L1 L2)             
       ((LAMBDA (I) (AND I (CONS (SETDIFF L1 I) (SETDIFF L2 I))))
	(INTERSECT L1 L2))) 

(DEFUN SETDIFF (S1 S2)                             ;Set difference of S1 and S2
       (DO ((J S1 (CDR J)) (A))
	   ((NULL J) A)
	   (OR (AND (NOT (NUMBERP (CAR J))) (MEMQ (CAR J) S2)) (SETQ A (CONS (CAR J) A)))))

(DEFUN CONTRACT3 (IT LST)      ;Tries to contract IT with some element of LST.
       (PROG (FRST R REST)     ;If none occurs then return NIL otherwise return
			       ;a list whose first member is the result of
			       ;contraction and whose cdr is a top-level copy
		               ;of LST with the element which contracted
			       ;removed.
	LOOP (SETQ FRST (CAR LST) LST (CDR LST))
;;	     (AND (EQ (CAAR FRST) '%KDELTA) (GO SKIP))
	     (AND (SETQ R (CONTRACT1 IT FRST))
		  (RETURN (CONS R (NCONC (NREVERSE REST) LST))))
			       ;Try contraction in reverse order since the
			       ;operation is commutative.
;;	SKIP (AND (ZL-GET (CAAR FRST) 'CONTRACTIONS)
	SKIP (AND (GETCON (CAAR FRST))
		  (SETQ R (CONTRACT1 FRST IT))
		  (RETURN (CONS R (NCONC (NREVERSE REST) LST))))
	     (AND (NULL LST) (RETURN NIL))
	     (SETQ REST (CONS FRST REST))
	     (GO LOOP))) 

(DEFUN CONTRACT4 (L)                                        ;Contracts products
       (PROG (L1 L2 L3 F CL SF) 
	     (SETQ CL (CDR L)) ;Following loop sets up 3 lists from the factors
		               ;on L: L1 - atoms or the contraction of non
		               ;indexed objects (the contraction is to handle
			       ;sub-expressions in case E is not fully expanded
			       ;as in A*B*(C*D+E*F). ), L2 - indexed objects in
	                       ;L with contraction property, L3 - indexed
                               ;objects in L without contraction property
	AGAIN(SETQ F (CAR CL) CL (CDR CL))
	     (COND ((ATOM F) (SETQ L1 (CONS F L1)))
		   ((RPOBJ F)
		    (SETQ F (CONTRACT5 F))
;;		    (COND ((ZL-GET (CAAR F) 'CONTRACTIONS)
		    (COND ((GETCON (CAAR F))
			   (SETQ L2 (CONS F L2)))
			  (T (SETQ L3 (CONS F L3)))))
		   (T (SETQ L1 (CONS ($CONTRACT F) L1))))
	     (AND CL (GO AGAIN))
	     (AND (NULL L2) (RETURN (NCONC L1 L3)))
	     (AND (NULL (CDR L2)) (SETQ CL L2) (GO LOOP2+1))
                               ;If L2 is empty then no more contractions are
                               ;needed. If L2 has only 1 member then just
	                       ;contract it with L3 otherwise contract the
		               ;members of L2 with themselves. The following
		               ;loop goes down L2 trying to contract members
		               ;with other members according to the following
		               ;method: moving from front to end take current
	                       ;member (F) and see if it contracts with any
		               ;elements in the rest of the list (this is done
		               ;by CONTRACT3). If it doesn't then add it to CL.
		               ;If it does then take result of contraction and
			       ;add to L1, L2, or L3 as above.
	LOOP1(SETQ F (CAR L2) L2 (CDR L2))
	     (COND ((NULL (SETQ SF (CONTRACT3 F L2)))
		    (SETQ CL (CONS F CL)))
		   (T (SETQ L2 (CDR SF) SF (CAR SF))
		      (COND ((ATOM SF) (SETQ L1 (CONS SF L1)))
			    ((RPOBJ SF)
;;			     (COND ((ZL-GET (CAAR SF)
;;					 'CONTRACTIONS)
			     (COND ((GETCON (CAAR SF))
				    (SETQ L2 (CONS SF L2)))
				   (T (SETQ L3 (CONS SF L3)))))
			    (T (SETQ L1 (CONS SF L1))))))
			       ;If L2 has at least 2 elements left then
		               ;continue loop. If L2 has 1 element and CL
			       ;is not empty and there were some contractions
			       ;performed last time then add CL to L2 and try
	                       ;again. Otherwise add L2 to CL and quit.
	     (AND L2
		  (COND ((CDR L2) (GO LOOP1))
			((AND CL SF)
			 (SETQ SF NIL L2 (CONS (CAR L2) CL) CL NIL)
			 (GO LOOP1))
			(T (SETQ CL (NCONC L2 CL)))))
			       ;The following loop goes down CL trying to
	                       ;contract each member with some member in L3. If
		               ;there is not a contraction then the element
			       ;from CL is added onto L3 (this causes elements
	                       ;of CL to be contracted with each other). If
	                       ;there is a contraction then the result is added
			       ;onto L3 by setting L3 to the result of
			       ;CONTRACT3 here if CL is known not to be null.
			       ;If L3 is empty then there is nothing left to
			       ;contract.
	LOOP2(AND (NULL CL) (RETURN (NCONC L1 L3)))
	LOOP2+1
	     (AND (NULL L3) (RETURN (NCONC L1 CL)))
	     (SETQ F (CAR CL) CL (CDR CL))
	     (COND ((SETQ SF (CONTRACT3 F L3)) (SETQ L3 SF))
		   (T (SETQ L3 (CONS F L3))))
	     (GO LOOP2))) 

(DEFUN CONTRACT1 (F G)           ;This does the actual contraction of F with G.
       (PROG (A B C D E CF) 	 ;If f has any derivative indices then it can't
				 ;contract G. If F is Kronecker delta then see
                                 ;which of the covariant, contravariant, or
                                 ;derivative indices matches those in G.
	     (WHEN (CDDDR F) (RETURN NIL))
	     (SETQ A (CDADR F)
		   B (CDADDR F)
		   C (CADR G)
		   D (CADDR G)
		   E (CDDDR G))
	     (COND
	      ((OR (EQ (CAAR F) '%KDELTA) (EQ (CAAR F) '$KDELTA))
	       (AND (> (LENGTH A) 1) (RETURN NIL))
	       (SETQ A (CAR A) B (CAR B))
	       (RETURN
		(SIMPLIFYA (COND ((AND (CDR C) (AND (NOT (NUMBERP B)) (MEMQ B (CDR C))))
				  (SETQ C (SUBST A B (CDR C)))
				  (AND (NOT (MEMQ (CAAR G) CHRISTOFFELS))
				       (CDR D)
				       (SETQ A (CONTRACT2 C (CDR D)))
				       (SETQ C (CAR A) 
					     D (CONS SMLIST (CDR A))))
				  (NCONC (LIST (CAR G)
					       (CONS SMLIST C)
					       D)
					 E))
				 ((AND E (AND (NOT (NUMBERP B)) (MEMQ B E)))
				  (NCONC (LIST (CAR G) C D)
					 (itensor-SORT (SUBST A B E))))
				 ((AND (CDR D) (AND (NOT (NUMBERP A)) (MEMQ A (CDR D))))
				  (SETQ D (SUBST B A (CDR D)))
				  (AND (CDR C)
				       (SETQ A (CONTRACT2 (CDR C) D))
				       (SETQ D (CDR A) 
					     C (CONS SMLIST (CAR A))))
				  (NCONC (LIST (CAR G)
					       C
					       (CONS SMLIST D))
					 E))
				 (T NIL))
			   NIL))))
;; VTT: No tensor should be able to contract LC or KDELTA.
	     (AND (OR (EQ (CAAR G) '$KDELTA) (EQ (CAAR G) '%KDELTA) (EQ (CAAR G) '$LC) (EQ (CAAR G) '%LC)) (RETURN NIL))

				    ;If G has derivative indices then F must be
	     (and e                 ;constant in order to contract it.
		  (NOT (MGET (CAAR F) '$CONSTANT))
		  (RETURN NIL))
				;Contraction property of F is a list of (A.B)'S
;;	     (COND ((SETQ CF (ZL-GET (CAAR F) 'CONTRACTIONS)))
	     (COND ((SETQ CF (GETCON (CAAR F))))
		   (T (RETURN NIL)))
                          ;If G matches an A then use the B for name of result.
			  ;If an A is a space use name of G for result.
	MORE (COND ((EQ (CAAR CF) '/ ) (SETQ CF (CAR G)))
		   ((EQ (CAAR CF) (CAAR G))
		    (SETQ CF (NCONS (CDAR CF))))
		   (T (OR (SETQ CF (CDR CF)) (RETURN NIL)) (GO MORE)))
	     (SETQ C (CDR C) D (CDR D))
			        ;If CONTRACT2 of F's contravariant and G's
			        ;covariant or F's covariant and G's
                                ;contravariant indicies is NIL then return NIL.
	     (COND ((AND B C (SETQ F (CONTRACT2 B C)))
		    (SETQ B (CAR F) C (CDR F)))
		   ((AND A D (SETQ F (CONTRACT2 A D)))
		    (SETQ A (CAR F) D (CDR F)))
		   (T (RETURN NIL)))
					       ;Form combined indices of result
	     (AND D (SETQ B (APPEND B D)))
	     (AND C (SETQ A (APPEND A C)))
						       ;Zl-remove repeated indices
	     (AND (SETQ F (CONTRACT2 A B)) (SETQ A (CAR F) B (CDR F)))
	     (SETQ F (MEVAL (LIST CF (CONS SMLIST A) (CONS SMLIST B))))
	     (AND E
;		  (DO E E (CDR E)
;		      (NULL E)
;		      (SETQ F (SDIFF F (CAR E))))
		  (DO ((E E (CDR E)))
		      ((NULL E) )
		      (SETQ F (SDIFF F (CAR E))))

		  )
	     (RETURN F)))

(DEFMFUN $UNDIFF (X) 
       (COND ((ATOM X) X)
	     ((RPOBJ X)
	      (COND ((CDDDR X)
		     (NCONC (LIST '(%DERIVATIVE)
				  (LIST (CAR X) (CADR X) (CADDR X)))
			    (PUTINONES (CDDDR X))))
		    (T X)))
	     (T
	      (MYSUBST0 (SIMPLIFYA (CONS (NCONS (CAAR X))
					 (MAPCAR '$UNDIFF (CDR X)))
				   T) X))))

(DEFUN PUTINONES (E) 
       (COND ((CDR E) (CONS (CAR E) (CONS 1. (PUTINONES (CDR E)))))
	     (T (LIST (CAR E) 1.)))) 

(DEFMFUN $KDELTA (L1 L2)
       (COND ((NULL (AND ($LISTP L1)
			 ($LISTP L2)
			 (= (LENGTH L1) (LENGTH L2))))
	      (merror "Improper arg to DELTA: ~M"
		      (LIST '(%KDELTA) L1 L2)
		      ))
	     (T (DELTA (CDR L1) (CDR L2))))) 

;kdels defines the symmetric combination of the Kronecker symbols

(DEFMFUN $KDELS (L1 L2)
       (COND ((NULL (AND ($LISTP L1)
			 ($LISTP L2)
			 (= (LENGTH L1) (LENGTH L2))))
	      (merror "Improper arg to DELTA: ~M"
		      (LIST '(%KDELS) L1 L2)
		      ))
	     (T (DELTA (CDR L1) (CDR L2) 1)))) 
;;
;;(DECLARE-TOP (FIXNUM I)) 
;;
;;(DEFUN DELTA (LOWER UPPER &optional (eps -1))
;;       (COND ((NULL LOWER) $DIM)
;;	     ((NULL (CDR LOWER))
;;	      (COND ((EQUAL (CAR UPPER) (CAR LOWER))
;;		     (COND ((NUMBERP (CAR UPPER)) 1.) (T $DIM)))
;;		    ((AND (NUMBERP (CAR UPPER)) (NUMBERP (CAR LOWER))) 0.)
;;		    (T (LIST '(%KDELTA)
;;			     (CONS SMLIST LOWER)
;;			     (CONS SMLIST UPPER)))))
;;	     (T (DO ((I (LENGTH LOWER) (1- I))
;;		     (SL LOWER)
;;		     (TERM)
;;		     (RESULT)
;;		     (F (NCONS (CAR UPPER)))
;;		     (R (CDR UPPER))
;;		     (SIGN (ODDP (LENGTH LOWER))))
;;		    ((= I 0.)
;;		     (SIMPLUS (CONS '(MPLUS) RESULT) 1. T))
;;		    (SETQ TERM (LIST (DELTA (NCONS (CAR SL)) F eps)
;;				     (DELTA (CDR SL) R eps)))
;;		    (SETQ SL (CDR (APPEND SL (NCONS (CAR SL)))))
;;		    (SETQ RESULT
;;			  (CONS (SIMPTIMES (CONS '(MTIMES)
;;						 (COND ((OR SIGN
;;							    (ODDP I))
;;							(CONS eps
;;							      TERM))
;;						       (T TERM)))
;;					   1.
;;					   NIL)
;;				RESULT)))))) 
(DEFUN DELTA (LOWER UPPER &optional (eps -1))
  (COND ((NULL LOWER) $DIM)
        ((NULL (CDR LOWER))
         (COND ((EQUAL (CAR UPPER) (CAR LOWER))
                (COND ((NUMBERP (CAR UPPER)) 1.) (T $DIM)))
               ((AND (NUMBERP (CAR UPPER)) (NUMBERP (CAR LOWER))) 0.)
               (T (LIST '(%KDELTA) (CONS SMLIST LOWER) (CONS SMLIST UPPER)))))
        (T (DO ((LEFT NIL (APPEND LEFT (NCONS (CAR RIGHT))))
		(RIGHT LOWER (CDR RIGHT))
                (RESULT))
               ((NULL RIGHT) (SIMPLUS (CONS '(MPLUS) RESULT) 1. T))
               (SETQ RESULT (CONS (SIMPTIMES
                                   (LIST '(MTIMES) (DELTA (NCONS (CAR RIGHT)) (NCONS (CAR UPPER)) eps)
                                         (DELTA (APPEND LEFT (CDR RIGHT)) (CDR UPPER) eps)
                                         (COND ((ODDP (LENGTH LEFT)) eps) (T 1))
                                   ) 1. T
                                  ) RESULT)
              )))))

(DECLARE-TOP (NOTYPE I))

(DECLARE-TOP (SPECIAL $OUTCHAR $DISPFLAG LINELABLE FOOBAR DERIVLIST))

;Displays P([L1],[L2],I1,I2,...) by making the elements of L2 into a single
;atom which serves as the exponent and the elements of L1 and I1,I2,... into a
;single atom with a comma in between which serves as the subscript.

(DEFMFUN $SHOW (f)
       (progn (makelabel $LINECHAR)
              (cond ($DISPFLAG
                     (displa (list '(MLABLE) LINELABLE (ishow (specrepcheck f))))
;                     (setq $DISPFLAG nil)
))
              (SET LINELABLE f)))

(DEFUN ISHOW (F) 
       ((LAMBDA (FOOBAR)                              ;FOOBAR intialized to NIL
		(COND ((ATOM F) F)
		      ((RPOBJ F)                      ;If an indexed object ...
		       (SETQ FOOBAR
			     (COND ((OR (CDADR F) (CDDDR F))   ;If covariant or
				    (CONS (LIST (CAAR F)    ;derivative indices
						'ARRAY)
					  (NCONS (MAKNAM (CONS '$ (SPLICE (CDADR F)
							 (CDDDR F)))))))
				   (T (CAAR F))))
		       (COND ((CDADDR F)              ;If contravariant indices
			      (LIST '(MEXPT SIMP)
				    FOOBAR
				     (CONS '(MTIMES SIMP)  ;Make indices appear
					  (CDADDR F))))    ;as exponents for
			     (T FOOBAR)))                  ;proper display
		      (T
		       (CONS (CAR F) (MAPCAR 'ISHOW (CDR F))))))
	NIL))                                           ;Map onto subparts of F

(DEFUN SPLICE (L1 L2) 
       (COND (L2 (SETQ L2 (CONS '|,| (SPLICE1 L2)))
		 (AND L1 (SETQ L2 (NCONC (SPLICE1 L1) L2)))
		 L2)
	     (T (SPLICE1 L1)))) 

(DEFUN SPLICE1 (L)
  (COND ((NULL (CDR L))(SPLICE2 (CAR L)))
	(T (NCONC (SPLICE2 (CAR L))(CONS '| | (SPLICE1 (CDR L)))))))

(DEFUN SPLICE2 (X)
  (COND ((FIXP X)(EXPLODE X))
	(T (CDR (EXPLODEc X)))))

(DEFUN DERIV (E) 
       (PROG (EXP Z COUNT V) 
	     (COND ((NULL (CDR E)) (RETURN (STOTALDIFF (CAR E))))
		   ((NULL (CDDR E)) (NCONC E '(1.))))
	     (SETQ EXP (CAR E) Z (SETQ E (APPEND E NIL)))
	LOOP (COND ((OR (NULL DERIVLIST) (ZL-MEMBER (CADR Z) DERIVLIST))
		    (GO DOIT)))
						       ;DERIVLIST is set by $EV
	     (SETQ Z (CDR Z))
	LOOP2(COND ((CDR Z) (GO LOOP))
		   ((NULL (CDR E)) (RETURN EXP))
		   (T (GO NOUN)))
	DOIT (COND ((NULL (CDDR Z))
		    (merror "Wrong number of args to DERIVATIVE"))
		   ((NOT (FIXP (SETQ COUNT (CADDR Z)))) (GO NOUN))
		   ((< COUNT 0.)
		    (merror "Improper count to DIFF: ~M"
			    COUNT)))
	LOOP1(SETQ V (CADR Z))
	     (AND (FIXP V)
		  $COORDINATES
		  (> V 0.)
		  (NOT (> V $DIM))
		  (SETQ V
			(COND ((ATOM $COORDINATES)
			       (MEVAL1 (LIST (LIST $COORDINATES 'SIMP 'ARRAY)
					     V)))
			      ((EQ (CAAR $COORDINATES) 'MLIST)
			       (COND ((NOT (< V
					      (LENGTH $COORDINATES)))
				      (merror
"Coordinate list too short for derivative index"))
				     (T (NTH V $COORDINATES))))
			      (T V))))
	     (COND ((ZEROP COUNT) (RPLACD Z (CDDDR Z)) (GO LOOP2))
		   ((ZEROP1 (SETQ EXP (SDIFF EXP V))) (RETURN 0.)))
	     (SETQ COUNT (1- COUNT))
	     (GO LOOP1)
	NOUN (RETURN (DIFF%DERIV (CONS EXP (CDR E))))))

(DEFUN CHAINRULE1 (E X)					; --YS 15.02.02
	(PROG (Y)
		(COND ((AND (ATOM E) (EQ (SETQ Y (CAR (MGET E 'DEPENDS)))
			(CADR $COORD))) (RETURN (SUBST X Y (CHAINRULE E Y))))
		(T (RETURN (CHAINRULE E X))))))

;Redefined so that the derivative of any indexed object appends on the
;coordinate index in sorted order unless the indexed object was declared
;constant in which case 0 is returned.
#+Franz (sstatus translink nil) ; make sdiff take hold
#+Franz (sstatus translink t)
(DEFUN SDIFF (E X) 
       (COND ((MNUMP E) 0.)
	     ((ALIKE1 E X) 1.)
	     ((OR (ATOM E) (MEMQ 'ARRAY (CDAR E)))
	      (CHAINRULE1 E X))
	     ((MGET (CAAR E) '$CONSTANT) 0.)                    ;New line added
	     ((EQ (CAAR E) 'MRAT) (RATDX E X))
	     ((EQ (CAAR E) 'MPLUS)
	      (SIMPLUS (CONS '(MPLUS) (SDIFFMAP (CDR E) X))
		       1.
		       T))
	     ((EQ (CAAR E) 'MEQUAL)
	      (LIST (CAR E) (SDIFF (CADR E) X) (SDIFF (CADDR E) X)))
	     ((EQ (CAAR E) '$MATRIX)
	      (CONS (CAR E)
		    (MAPCAR 
		     (FUNCTION (LAMBDA (Y) 
				       (CONS (CAR Y)
					     (SDIFFMAP (CDR Y) X))))
		     (CDR E))))
	     ((EQ (CAAR E) 'MTIMES)
 	      (ADDN (SDIFFTIMES (CDR E) X) T))
	     ((EQ (CAAR E) 'MEXPT) (DIFFEXPT E X))
	     ((RPOBJ E) (DIFFRPOBJ E X))                        ;New line added
	     ((AND (BOUNDP '$METRIC) (EQ (CAAR E) '%DETERMINANT);New line added
		   (EQ (CADR E) $METRIC))
	      ((LAMBDA (DUMMY)
		       (setq dummy ($dummy))
		       (COND ((EQ DUMMY X) (setq dummy ($dummy))))
		       (LIST '(MTIMES SIMP) 2. E
			     (LIST '($CHR2 SIMP) (CONS SMLIST (LIST DUMMY X))
				   (CONS SMLIST (NCONS DUMMY)))))
	       NIL))
	     ((NOT (DEPENDS E X))
	      (COND ((FIXP X) (LIST '(%DERIVATIVE) E X))
		    ((ATOM X) 0.)
		    (T (LIST '(%DERIVATIVE E X)))))
							  ;This line moved down
	     ((EQ (CAAR E) 'MNCTIMES)
	      (SIMPLUS (LIST '(MPLUS)
			     (LIST '(MNCTIMES)
				   (SDIFF (CADR E) X)
				   (CADDR E))
			     (LIST '(MNCTIMES)
				   (CADR E)
				   (SDIFF (CADDR E) X)))
		       1.
		       NIL))
	     ((EQ (CAAR E) 'MNCEXPT) (DIFFNCEXPT E X))
	     ((EQ (CAAR E) '%INTEGRATE) (DIFFINT E X))
	     ((EQ (CAAR E) '%DERIVATIVE)
	      (COND ((OR (ATOM (CADR E))
			 (MEMQ 'ARRAY (CDAADR E)))
		     (CHAINRULE1 E X))
		    ((FREEL (CDR E) X) 0.)
		    (T (DIFF%DERIV (LIST E X 1.)))))
	     ((MEMQ (CAAR E) '(%SUM %PRODUCT)) (DIFFSUMPROD E X))
	     (T (SDIFFGRAD E X)))) 

(defun DIFFRPOBJ (e x)                         ;Derivative of an indexed object
       (cond ((and (memq (caar e) $COORD) (null (cdadr e))
		   (equal (length (cdaddr e)) 1) (null (cdddr e)))
	      (delta (ncons x) (cdaddr e)))
	     (t (NCONC (LIST (CAR E) (CADR E) (CADDR E))
		       (COND ((NULL (CDDDR E)) (NCONS X))
			     (T (itensor-SORT (APPEND (CDDDR E) (NCONS X)))))))))



(DEFMFUN $LC0 (L1) 
       (PROG (A B C SIGN) 
	     (SETQ A (CDR L1))
	     (IFNOT (AND A (CDR A)) (RETURN (LIST '(%LC) L1)))
	     (SETQ B A)
	LOOP1(IFNOT (FIXP (CAR A)) (RETURN (LIST '(%LC) L1)))
	     (AND (SETQ A (CDR A)) (GO LOOP1))
	LOOP3(SETQ A (CAR B) B (CDR B) C B)
	LOOP2(COND ((= (CAR C) A) (RETURN 0.))
		   ((< (CAR C) A) (SETQ SIGN (NOT SIGN))))
	     (AND (SETQ C (CDR C)) (GO LOOP2))
	     (AND (CDR B) (GO LOOP3))
	     (RETURN (COND (SIGN -1.) (T 1.))))) 
(DEFMFUN $LC (L1 &optional (L2 nil))
	(COND
		((EQ L2 nil) ($LC0 L1))
		((LIKE L1 '((MLIST)))
		(PROG (l) (SETQ l nil)
		  (DO ((I ($LENGTH L2) (1- I))) ((< I 1)) (SETQ l (CONS I l)))
		  (RETURN (LIST '($KDELTA SIMP) (CONS SMLIST l) L2))
		 ))
		((LIKE L2 '((MLIST)))
		(PROG (l) (SETQ l nil)
		  (DO ((I ($LENGTH L1) (1- I))) ((< I 1)) (SETQ l (CONS I l)))
		  (RETURN (LIST '($KDELTA SIMP) L1 (CONS SMLIST l)))
		))
		(T (MERROR "Mixed-index Levi-Civita symbols not supported"))
	)
)

;; simplification rules for the totally antisymmetric LC symbol
(DEFUN $LC_L (E)
    (PROG (L1 L2 L N)
	(CATCH 'MATCH
	  (COND ((ATOM E) (MATCHERR)))
	  (COND ((ATOM (CAR E)) (MATCHERR)))
	  (COND ((NOT (OR (EQ (CAAR E) '$LC) (EQ (CAAR E) '%LC))) (MATCHERR)))
	  (COND ((NOT ($LISTP (SETQ L1 (CADR E)))) (MATCHERR)))
	  (COND ((NOT (ALIKE1 '((MLIST SIMP)) (SETQ L2 (CADDR E)))) (MATCHERR)))
	  (COND ((CDDDR E) (MATCHERR)))
	  (SETQ N ($LENGTH L1))
	  (SETQ L NIL)
	  (DO ((I N (1- I))) ((< I 1)) (SETQ l (CONS ($DUMMY) L)))
	  (RETURN (LIST '(MTIMES SIMP) -1 ($KDELTA L1 (CONS SMLIST L))
	        (LIST (CONS (CAAR E) '(SIMP)) (CONS SMLIST L) (NCONS SMLIST))
	        (LIST '(MEXPT SIMP) (MEVAL (LIST 'MFACTORIAL N)) -1))
	  )
	)
    )
)

(DEFUN $LC_U (E)
    (PROG (L1 L2 L N)
	(CATCH 'MATCH
	  (COND ((ATOM E) (MATCHERR)))
	  (COND ((ATOM (CAR E)) (MATCHERR)))
	  (COND ((NOT (OR (EQ (CAAR E) '$LC) (EQ (CAAR E) '%LC))) (MATCHERR)))
	  (COND ((NOT (ALIKE1 '((MLIST SIMP)) (SETQ L1 (CADR E)))) (MATCHERR)))
	  (COND ((NOT ($LISTP (SETQ L2 (CADDR E)))) (MATCHERR)))
	  (COND ((CDDDR E) (MATCHERR)))
	  (SETQ N ($LENGTH L2))
	  (SETQ L NIL)
	  (DO ((I N (1- I))) ((< I 1)) (SETQ l (CONS ($DUMMY) L)))
	  (RETURN (LIST '(MTIMES SIMP) -1 ($KDELTA (CONS SMLIST L) L2)
	        (LIST (CONS (CAAR E) '(SIMP)) (NCONS SMLIST) (CONS SMLIST L))
	        (LIST '(MEXPT SIMP) (MEVAL (LIST 'MFACTORIAL N)) -1))
	  )
	)
    )
)

(ADD2LNC '$LC_L $RULES)
(ADD2LNC '$LC_U $RULES)

(DECLARE-TOP (SPECIAL E EMPTY $FLIPFLAG))

(SETQ $FLIPFLAG NIL EMPTY '((MLIST SIMP) ((MLIST SIMP)) ((MLIST SIMP)))) 

(DEFUN NONUMBER (L)
	(COND
		((NUMBERP (CAR L)) (NONUMBER (CDR L)))
		((EQ L NIL) ())
		(T (CONS (CAR L) (NONUMBER (CDR L))))
	)
)

(DEFUN REMOVEINDEX (E L)
 (COND	((NULL L) NIL)
	((ATOM E)
         (COND ((EQ E (CAR L)) (CDR L))
              (T (CONS (CAR L) (REMOVEINDEX E (CDR L))))
        ))
	(T (REMOVEINDEX (CDR E) (REMOVEINDEX (CAR E) L)))
 )
)

(DEFUN INDICES (E)
 (PROG (TOP BOTTOM X Y P Q R)
  (SETQ TOP NIL BOTTOM NIL)
  (COND ((RPOBJ E) (SETQ TOP (NONUMBER (CDADDR E)) BOTTOM (NONUMBER (APPEND (CDADR E) (CDDDR E)))))
        ((ATOM E))
        ((MEMQ (CAAR E) '(MTIMES MNCTIMES MNCEXPT))
         (DOLIST (V (CDR E))
          (SETQ X (INDICES V) BOTTOM (APPEND BOTTOM (CADR X)) TOP (APPEND TOP (CAR X)))
         )
        )
        ((MEMQ (CAAR E) '(MPLUS MEQUAL))
         (SETQ TOP (INDICES (CADR E)) BOTTOM (CADR TOP) TOP (CAR TOP))
         (SETQ P (INTERSECT TOP BOTTOM) Q (REMOVEINDEX P BOTTOM) P (REMOVEINDEX P TOP))
          (DOLIST (V (CDDR E))
           (SETQ X (INDICES V) Y (CADR X) X (CAR X))
           (SETQ R (INTERSECT X Y) X (REMOVEINDEX R X) Y (REMOVEINDEX R Y))
           (WHEN (NOT (AND (SAMELISTS X P) (SAMELISTS Y Q))) (MERROR "Improper indices in ~M" V))
           (SETQ TOP (UNION TOP R) BOTTOM (UNION BOTTOM R))
         )
        )
        ((MEMQ (CAAR E) '($SUM %SUM))
         (SETQ TOP (LIST (CADDR E)) BOTTOM (LIST (CADDR E)))
        )
        ((MEMQ (CAAR E) '(%DERIVATIVE $DIFF))
         (DO ((I 1 (1+ I))) ((> I (COND ((CADDDR E) (CADDDR E)) (T 1))))
           (SETQ BOTTOM (CONS (CADDR E) BOTTOM)))
        )
;;        (T (MERROR "Improper argument to INDICES: ~M" E))
  )
  (RETURN (LIST TOP BOTTOM))
 )
)

(DEFMFUN $INDICES (E)
 (PROG (TOP BOTTOM X)
	(SETQ TOP (INDICES E) BOTTOM (CADR TOP) TOP (CAR TOP) X (INTERSECT TOP BOTTOM))
	(SETQ TOP (REMOVEINDEX X TOP) BOTTOM (REMOVEINDEX X BOTTOM))
	(RETURN (CONS SMLIST (LIST (CONS SMLIST (APPEND TOP BOTTOM)) (CONS SMLIST X))))
 )
)

(DEFUN SAMELISTS (A B)       ;"True" if A and B have the same distinct elements
       (AND (= (LENGTH A) (LENGTH B))
	    (DO ((L
		A
		(CDR L)))
		(NIL)
		(COND ((NULL L) (RETURN T))
		      ((MEMQ (CAR L) B))
		      (T (RETURN NIL)))))) 

(DEFMFUN $FLUSH n           ;Replaces the given (as arguments to FLUSH) indexed
       (prog (l)          ;objects by zero if they have no derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (sloop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v)))
;		      (apply 'and (mapcar 'symbolp
;					    (setq l (listify (f- 1 n))) ))
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l t))))))

(DEFMFUN $FLUSHD n          ;Replaces the given (as arguments to FLUSHD) indexed
       (prog (l)          ;objects by zero if they have any derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (sloop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v))
;		      (apply 'and (mapcar 'symbolp
;					     (setq l (listify (f- 1 n)))))
		      )
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l nil))))))

(defun FLUSH (e l flag)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((not (memq (caar e) l)) e)
		    ((not (null (cdddr e)))
		     (cond (flag e)
			   (t 0)))
		    (t (cond (flag 0)
			     (t e)))))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function (lambda (q) (flush q l flag)))
				      (cdr e))) e))))

(DEFMFUN $FLUSHND (e name n)              ;Replaces by zero all indexed objects
       (cond ((atom e) e)               ;that have n or more derivative indices
	     ((rpobj e)
	      (cond ((and (equal (caar e) name)
			  (> (length (cdddr e)) (1- n)))
		     0)
		    (t e)))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function
				       (lambda (q) ($flushnd q name n)))
				      (cdr e))) e))))

(DECLARE-TOP (FIXNUM INDEX N) (SPECIAL INDEX N DUMX))

;(DEFMFUN $RENAME NARGS ((LAMBDA (INDEX) (RENAME (ARG 1)))
;       (COND ((= NARGS 1) 1) (T (ARG 2))))) ;Sets INDEX to 1 or 2nd argument of
;                                            ;$RENAME
(DEFMFUN $RENAME NARGS
 (cond ((= NARGS 1) (setq INDEX 1)) (t (setq INDEX (arg 2)))) (rename (arg 1)))

(DEFUN RENAME (E)                           ;Renames dummy indices consistently
       (COND
	((ATOM E) E)
	((OR (RPOBJ E) (EQ (CAAR E) 'MTIMES));If an indexed object or a product
	 ((LAMBDA  (L) 
	(SIMPTIMES (REORDER (COND (L (SUBLIS (itensor-CLEANUP L (SETQ N INDEX)) E))(T E))) 1 T))
	  (CDADDR ($INDICES E))                     ;Gets list of dummy indices
	  ))
	(T            ;Otherwise map $RENAME on each of the subparts e.g. a sum
	 (MYSUBST0 (SIMPLIFYA  (CONS (NCONS (CAAR E))
				  (MAPCAR 'RENAME (CDR E)))
			    T)
		   E))
	))

(DEFUN REORDER (E)       ;Reorders contravariant, covariant, derivative indices
       (MYSUBST0         ;Example: F([A,B],[C,D],E,F)
	(CONS
	 '(MTIMES)
	 (MAPCAR
	  #'(LAMBDA (X) 
	    (COND ((RPOBJ X)
		   (NCONC (LIST (CAR X)                              ;($F SIMP)
				(CONS SMLIST
				      (COND ($ALLSYM (itensor-SORT (COPY (CDADR X))))
					    (T (CDADR X))))          ;($A $B)
				(CONS SMLIST
				      (COND ($ALLSYM
					     (itensor-SORT (COPY (CDADDR X))))
					    (T (CDADDR X)))))        ;($C $D)
			  (itensor-SORT (COPY (CDDDR X)))))                ;($E $F)
		  (T X)))
	  (COND ((EQ (CAAR E) 'MTIMES) (CDR E))
		(T (NCONS E)))))
	E))

(DEFUN itensor-CLEANUP (A N)((LAMBDA (DUMX)(CLEANUP1 A)) NIL))        ;Sets DUMX to NIL
 
(DEFUN CLEANUP1 (A)
  (AND A (SETQ DUMX (IMPLODE (NCONC (EXPLODEN $DUMMYX)    ;Keep proper order of
				    (EXPLODEN N))) N (1+ N))          ;indices
	(COND ((EQ DUMX (CAR A)) (CLEANUP1 (CDR A)))
	      (T (CONS (CONS (CAR A) DUMX) (CLEANUP1 (CDR A)))))))
;Make list of dotted pairs indicating substitutions i.e. ((A . #1) (B . #2))

(DECLARE-TOP (NOTYPE N INDEX)(UNSPECIAL N DUMX INDEX))

(DEFUN itensor-SORT (L) (COND ((CDR L) (SORT L 'LESS)) (T L)))
;Sort into ascending order

(DEFMFUN $REMCOMPS (TENSOR)
       (ZL-REMPROP TENSOR 'EXPR) (ZL-REMPROP TENSOR 'CARRAYS)
       (ZL-REMPROP TENSOR 'TEXPRS) (ZL-REMPROP TENSOR 'INDEXED)
       (ZL-REMPROP TENSOR 'TSUBR) '$DONE)

(DEFMFUN $INDEXED (TENSOR)
  (LET (FP NEW)
    (AND (ZL-GET TENSOR 'EXPR) 
	 (merror "~M has expr" tensor))
    (ARGS TENSOR  NIL)
    (AND (SETQ FP (ZL-GET TENSOR 'SUBR))
	 (PROGN (SETQ NEW (GENSYM))(PUTPROP NEW FP 'SUBR)
		(ZL-REMPROP TENSOR 'SUBR)(PUTPROP TENSOR NEW 'TSUBR)))
    (PUTPROP TENSOR T 'INDEXED)
    (PUTPROP TENSOR (SUBST TENSOR 'G '(LAMBDA N (TENSOREVAL (QUOTE G)(LISTIFY N)))) 'EXPR)
		(eval (subst tensor 'g (quote (defmfun g n (tensoreval 'g (listify n))))))
    '$DONE))


(DEFUN ALLFIXED (L) 
       (AND L (FIXP (CAR L)) (OR (NULL (CDR L)) (ALLFIXED (CDR L))))) 

;;(DEFUN TENSOREVAL (TENSOR INDXS)
;;  ((LAMBDA (DER CON)
;;    (AND (CDR INDXS) (SETQ CON (CDADR INDXS) DER (CDDR INDXS)))
;;  (SETQ TENSOR (SELECT TENSOR (CDAR INDXS) CON))
;;  (COND (DER (APPLY '$DIFF (CONS TENSOR (PUTINONES DER))))
;;	(T TENSOR))) NIL NIL))
(DEFUN TENSOREVAL (TENSOR INDXS)
  ((LAMBDA (DER CON)
    (AND (CDR INDXS) (SETQ CON (CDADR INDXS) DER (CDDR INDXS)))
  (SETQ TENSOR (SELECT TENSOR (CDAR INDXS) CON DER))
  ) NIL NIL))

;;(DEFMFUN $COMPONENTS (TENSOR COMP)
;;  ((LAMBDA (LEN1 LEN2 NAME PROP)
;;    (COND ((OR (NOT (RPOBJ TENSOR))(CDDDR TENSOR))
;;	   (merror "Improper 1st arg to COMPONENTS: ~M"
;;		   TENSOR
;;		   )))
;;    (SETQ LEN1 (LENGTH (CDADR TENSOR)) LEN2 (LENGTH (CDADDR TENSOR)))
;;    (AND (NOT (ATOM COMP))(EQ (CAAR COMP) '$MATRIX)
;;	 (COND ((= (f+ LEN1 LEN2) 2)(SETQ NAME (GENSYM))
;;		(SET NAME COMP)(SETQ COMP NAME))
;;	       (T 
;;		(merror "Needs two indices for COMPONENTS from matrix:~%~M"
;;			TENSOR))))
;;    (COND ((AND (EQ (ML-TYPEP COMP) 'SYMBOL) (> (f+ LEN1 LEN2) 0))
;;	   (SETQ PROP 'CARRAYS))
;;	  ((SAMELISTS (SETQ NAME (APPEND (CDADR TENSOR) (CDADDR TENSOR)))
;;		      (CDADR ($INDICES COMP)))
;;	   (SETQ PROP 'TEXPRS COMP (CONS COMP NAME)))
;;	  (T (merror "Args to COMPONENTS do not have the same free indices")))
;;    (SETQ TENSOR (CAAR TENSOR) LEN1 (CONS LEN1 LEN2))
;;    (COND ((AND (SETQ NAME (ZL-GET TENSOR PROP))
;;		(SETQ LEN2 (ZL-ASSOC LEN1 NAME))) (RPLACD LEN2 COMP))
;;	  (T (PUTPROP TENSOR (CONS (CONS LEN1 COMP) NAME) PROP)))
;;    (OR (ZL-GET TENSOR 'INDEXED) ($INDEXED TENSOR))
;;    '$DONE) NIL NIL NIL NIL))
(DEFMFUN $COMPONENTS (TENSOR COMP)
  ((LAMBDA (LEN1 LEN2 LEN3 NAME PROP)
    (COND ((NOT (RPOBJ TENSOR))
	   (merror "Improper 1st arg to COMPONENTS: ~M"
		   TENSOR
		   )))
    (SETQ LEN1 (LENGTH (CDADR TENSOR)) LEN2 (LENGTH (CDADDR TENSOR)) LEN3 (LENGTH (CDDDR TENSOR)))
    (AND (NOT (ATOM COMP))(EQ (CAAR COMP) '$MATRIX)
	 (COND ((= (f+ (f+ LEN1 LEN2) LEN3) 2)(SETQ NAME (GENSYM))
		(SET NAME COMP)(SETQ COMP NAME))
	       (T 
		(merror "Needs two indices for COMPONENTS from matrix:~%~M"
			TENSOR))))
    (COND ((AND (EQ (ML-TYPEP COMP) 'SYMBOL) (> (f+ (f+ LEN1 LEN2) LEN3) 0))
	   (SETQ PROP 'CARRAYS))
	  ((SAMELISTS (SETQ NAME (APPEND (CDADR TENSOR) (CDADDR TENSOR) (CDDDR TENSOR)))
		      (CDADR ($INDICES COMP)))
	   (SETQ PROP 'TEXPRS COMP (CONS COMP NAME)))
	  (T (merror "Args to COMPONENTS do not have the same free indices")))
    (SETQ TENSOR (CAAR TENSOR) LEN1 (LIST LEN1 LEN2 LEN3))
    (COND ((AND (SETQ NAME (ZL-GET TENSOR PROP))
		(SETQ LEN2 (ZL-ASSOC LEN1 NAME))) (RPLACD LEN2 COMP))
	  (T (PUTPROP TENSOR (CONS (CONS LEN1 COMP) NAME) PROP)))
    (OR (ZL-GET TENSOR 'INDEXED) ($INDEXED TENSOR))
    '$DONE) NIL NIL NIL NIL NIL))

;;(DEFUN SELECT (TENSOR L1 L2)
;;  ((LAMBDA (PROP SUBS INDEX)
;;	(COND ((AND (ALLFIXED SUBS) (SETQ PROP (ZL-GET TENSOR 'CARRAYS))
;;		    (SETQ PROP (ZL-ASSOC INDEX PROP)))
;;	       (COND ((ALIKE1 (SETQ PROP (CONS (LIST (CDR PROP) 'ARRAY) SUBS))
;;			      (SETQ SUBS (MEVAL PROP))) 0)
;;		     (T SUBS)))
;;	      ((SETQ PROP (ZL-ASSOC INDEX (ZL-GET TENSOR 'TEXPRS)))
;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CADR PROP)))
;;	      ((SETQ PROP (ZL-GET TENSOR 'TSUBR))
;;	       (APPLY PROP (LIST (CONS SMLIST L1)(CONS SMLIST L2))))
;;	      (T (LIST (LIST TENSOR 'SIMP)(CONS SMLIST L1)(CONS SMLIST L2)))))
;;	NIL (APPEND L1 L2)(CONS (LENGTH L1)(LENGTH L2))))

;;vtt: inconstant was an attempt to remove constant indices, but it really doesn't work out.
;;(DEFUN INCONSTANT (L)
;;  (COND 
;;    ((EQ L NIL) NIL)
;;    (($CONSTANTP (CAR L)) (AND (NOT (EQ NIL (CDR L))) (INCONSTANT (CDR L))))
;;    (T (CONS (CAR L) (AND (NOT (EQ NIL (CDR L))) (INCONSTANT (CDR L)))))
;;  )
;;)

(DEFUN SELECT (TENSOR L1 L2 L3)
  ((LAMBDA (PROP SUBS INDEX)
	(COND ((AND (ALLFIXED SUBS) (SETQ PROP (ZL-GET TENSOR 'CARRAYS))
		    (SETQ PROP (ZL-ASSOC INDEX PROP)))
	       (COND ((ALIKE1 (SETQ PROP (CONS (LIST (CDR PROP) 'ARRAY) SUBS))
			      (SETQ SUBS (MEVAL PROP))) 0)
		     (T SUBS)))
	      ((SETQ PROP (ZL-ASSOC INDEX (ZL-GET TENSOR 'TEXPRS)))
;;;VTT	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CADR PROP)))
;;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) ($RENAME (CADR PROP) (COND ((BOUNDP 'N) N) (T 1)))))
	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CAR (CONS ($RENAME (CADR PROP) (1+ $COUNTER)) (SETQ $COUNTER (1- N))))))
	      ((SETQ PROP (ZL-GET TENSOR 'TSUBR))
;;	       (APPLY PROP (LIST (CONS SMLIST (INCONSTANT L1))(CONS SMLIST (INCONSTANT L2))(CONS SMLIST L3))))
;;	      ((NOT (EQ L3 NIL)) (APPLY '$DIFF (SELECT TENSOR (INCONSTANT L1) (INCONSTANT L2) (CDR L3)) (LIST (CAR L3))))
;;	      (T (APPEND (LIST (LIST TENSOR 'SIMP)(CONS SMLIST (INCONSTANT L1))(CONS SMLIST (INCONSTANT L2))) L3))))
;;	NIL (APPEND (INCONSTANT L1) (INCONSTANT L2) L3)(LIST (LENGTH (INCONSTANT L1))(LENGTH (INCONSTANT L2))(LENGTH L3))))
	       (APPLY PROP (LIST (CONS SMLIST L1)(CONS SMLIST L2)(CONS SMLIST L3))))
	      ((NOT (EQ L3 NIL)) (APPLY '$DIFF (SELECT TENSOR L1 L2 (CDR L3)) (LIST (CAR L3))))
	      (T (APPEND (LIST (LIST TENSOR 'SIMP)(CONS SMLIST L1)(CONS SMLIST L2)) L3))))
	NIL (APPEND L1 L2 L3)(LIST (LENGTH L1)(LENGTH L2)(LENGTH L3))))


(DEFMFUN $ENTERTENSOR nargs 
  (prog (fun contr cov deriv)
    (cond ((> nargs 1)
	   (merror "ENTERTENSOR takes 0 or 1 arguments only"))
	  ((= nargs 0)
	   (mtell "Enter tensor name: ") 
	   (setq fun (meval (retrieve nil nil))))
	  ((setq fun (arg 1))))
    (mtell "Enter a list of the covariant indices: ")
    (setq cov (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom cov) (setq cov (cons smlist (ncons cov)))))
    (mtell "Enter a list of the contravariant indices: ")
    (setq contr (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom contr) (setq contr (cons smlist (ncons contr)))))
    (mtell "Enter a list of the derivative indices: ")
    (setq deriv (checkindex (meval (retrieve nil nil)) fun))
    (setq deriv (cond ((atom deriv) (ncons deriv))
		      (t (cdr deriv))))
    (cond ((memberl (cdr cov) deriv)
	   (mtell "~%Warning - there are indices that are both covariant ~
		  and derivative%")))
    (return ($SHOW (nconc (list (list fun 'SIMP) cov contr)
				       deriv)))))

(defun CHECKINDEX (e f)
  (cond ((and (atom e) (not (eq e f))) e)
	((and (eq (caar e) 'MLIST)
	      (sloop for v in (cdr e) always (atom v))
;	      (apply 'and (mapcar 'atom (cdr e)))
	      (not (memq f e))) e)
	(t (merror "Indices must be atoms different from the tensor name"))))

(defun MEMBERL (a b)
  (do ((l a (cdr l))
       (carl))
      ((null l) nil)
    (setq carl (car l))
    (cond ((and (eq (ml-typep carl) 'SYMBOL)
		(zl-member carl b)) (return t)))))

(defun CONSMLIST (l) (cons smlist l))			;Converts from Lisp list to Macsyma list

(DEFMFUN $INDICES2 (e)
  (cond ((atom e) empty)
	((not (or (memq (caar e) '(MTIMES MNCTIMES)) (rpobj e)))
	 ($indices e))
	(t ((lambda (indices)
	      (do ((ind indices) (free) (dummy) (index))
		  ((null ind)
		   (consmlist (list (consmlist (nreverse free))
				    (consmlist (nreverse dummy)))))
		(setq index (car ind))
		(cond ((zl-member index dummy)
		       (merror "~M has improper indices"
			       (ishow e)))
		      ((zl-member index (cdr ind))
		       (setq dummy (cons index dummy)
			     ind (zl-delete index (copy (cdr ind))
					 1)))
		      (t (setq free (cons index free)
			       ind (cdr ind))))))
	    (do ((e (cond ((memq (caar e) '(MTIMES MNCTIMES)) (cdr e))
			  (t (ncons e))) (cdr e))
		 (a) (l))
		((null e) l)
	      (setq a (car e))
	      (and (rpobj a) (setq l (append l (cdadr a) (cdaddr a)
					     (cdddr a)))))))))

;$INDICES2 is similar to $INDICES except that here dummy indices are picked off
;as they first occur in going from left to right through the product or indexed
;object. Also, $INDICES2 works only on the top level of a product and will
;miss indices for products of sums (which is used to advantage by $GENERATE).

(DEFMFUN $CHANGENAME (a b e)				;Change the name of the indexed object A to B in E
  (prog (old indspec ncov ncontr)			;INDSPEC is INDex SPECification flag
    (cond ((not (or (and (eq (ml-typep a) 'SYMBOL) (setq old a))
		    (and ($listp a) (equal (length (cdr a)) 3)
			 (eq (ml-typep (setq old (cadr a))) 'SYMBOL)
			 (eq (ml-typep (setq ncov (caddr a))) 'FIXNUM)
			 (eq (ml-typep (setq ncontr (cadddr a))) 'FIXNUM)
			 (setq indspec t))))
	   (merror "Improper first argument to CHANGENAME: ~M" a))
	  ((not (eq (ml-typep b) 'SYMBOL))
	   (merror "Second argument to CHANGENAME must be a symbol"))
	  (t (return (changename old indspec ncov ncontr b e))))))

(defun CHANGENAME (a indspec ncov ncontr b e)
  (cond ((or (atom e) (eq (caar e) 'RAT)) e)
	((rpobj e)
	 (cond ((and (eq (caar e) a)
		     (cond (indspec (and (equal (length (cdadr e)) ncov)
					 (equal (length (cdaddr e))
						ncontr)))
			   (t t)))
		(cons (cons b (cdar e)) (cdr e)))
	       (t e)))
	(t (mysubst0 (cons (car e)
			   (mapcar (function
				    (lambda (q)
				      (changename a indspec ncov
						  ncontr b q)))
				   (cdr e))) e))))

(DEFMFUN $COORD n
  (do ((l (listify n) (cdr l)) (a))
      ((null l) '$DONE)
    (setq a (car l))
    (cond ((not (eq (ml-typep a) 'SYMBOL))
	   (merror "~M is not a valid name." a))
	  (t (add2lnc a $COORD)))))

(DEFMFUN $REMCOORD n
  (cond ((and (equal n 1) (eq (arg 1) '$ALL))
	 (setq $COORD '((MLIST))) '$DONE)
	(t (do ((l (listify n) (cdr l)))
	       ((null l) '$DONE)
	     (delq (car l) $COORD)))))


;; Additions on 5/19/2004 -- VTT

(DEFUN MEMBERLIST (E L)
	(COND ((NULL L) NIL)
	      ((EQUAL E (CAR L)) T)
	      (T (MEMBERLIST E (CDR L)))
	)
)

(DEFUN UNIONLIST (L1 L2)
	(COND ((NULL L1) L2)
	      ((MEMBERLIST (CAR L1) L2) (UNIONLIST (CDR L1) L2))
	      (T (CONS (CAR L1) (UNIONLIST (CDR L1) L2)))
	)
)

(DEFMFUN $LISTOFTENS (E) (itensor-sort (CONS SMLIST (LISTOFTENS E))))
(DEFUN LISTOFTENS (E)
	(COND
	  ((ATOM E) NIL)
	  ((RPOBJ E) (LIST E))
	  (T (PROG (L) (SETQ L NIL)
		(MAPCAR (LAMBDA (X) (SETQ L (UNIONLIST L (LISTOFTENS X)))) (CDR E))
		(RETURN L)
	     )
	  )
	)
)

(DEFUN NUMLIST (&optional (n '1)) (COND ((>= n $DIM) (LIST n)) (T (CONS n (NUMLIST (1+ n))))))

;;SHOWCOMPS(tensor):=BLOCK([i1,i2,ind:INDICES(tensor)[1]],
;;	IF LENGTH(ind)=0 THEN SHOW(EV(tensor))
;;	ELSE IF LENGTH(ind)=1 THEN SHOW(MAKELIST(EV(tensor,ind[1]=i1),i1,1,DIM))
;;	ELSE IF LENGTH(ind)=2 THEN SHOW(tensor=APPLY('MATRIX,MAKELIST(MAKELIST(EV(tensor,[ind[1]=i1,ind[2]=i2]),i1,1,DIM),i2,1,DIM)))
;;	ELSE FOR i1 THRU DIM DO (SHOWCOMPS(SUBST(i1,LAST(ind),tensor)),IF LENGTH(ind)=3 AND i1<DIM THEN LINENUM:LINENUM+1)
;;);
(DEFMFUN $SHOWCOMPS (E)
 (PROG (IND)
  (SETQ IND (CDADR ($INDICES E)))
  (COND ((> 1 (LENGTH IND)) ($SHOW (MEVAL (LIST '($EV) E))))
	((> 2 (LENGTH IND)) ($SHOW (CONS SMLIST (MAPCAR (LAMBDA (I) (MEVAL (LIST '($EV) E (LIST '(MEQUAL) (CAR IND) I)))) (NUMLIST)))))
	((> 3 (LENGTH IND)) ($SHOW (LIST '(MEQUAL) E (CONS '($MATRIX SIMP) (MAPCAR (LAMBDA (J) (CONS SMLIST (MAPCAR (LAMBDA (I) (MEVAL (LIST '($EV) E (LIST '(MEQUAL) (CAR IND) I) (LIST '(MEQUAL) (CADR IND) J)))) (NUMLIST)))) (NUMLIST))))))
	(T (MAPCAR (LAMBDA (I)  ($SHOWCOMPS ($SUBSTITUTE I (CAR (LAST IND)) E)) (AND (> 4 (LENGTH IND)) (< I $DIM) (SETQ $LINENUM (1+ $LINENUM)))) (NUMLIST)))
  )
 )
)
