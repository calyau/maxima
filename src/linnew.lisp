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
(macsyma-module linnew)

;; This is a matrix package which uses minors, basically.
;; TMLINSOLVE(LIST-OF-EQUAIONS,LIST-OF-VARIABLES,LIST-OF-VARIABLES-TO-BE-OBTAINED)
;; solves the linear equation. LIST-OF-VARIABLES-TO-BE-OBTAINED can be omitted,
;; in which case all variables are obtained. TMNEWDET(MATRIX,DIMENSION)
;; computes the determinant.  DIMENSION can be omitted.  The default is
;; DIMENSION=(declared dimension of MATRIX). TMINVERSE(MATRIX) computes the
;; inverse of matrix.

;; The program uses hash arrays to remember the minors if N > threshold.  If
;; $WISE is set to T, the program knocks out unnecessary elements.  But also it
;; kills necessary ones in the case of zero elements! The $WISE flag should
;; not be set to T for inverse.  The default of $WISE is NIL.

;; There seem to have been a number of bugs in this code.  I changed
;; the array referencing to value cell, and some of the stuff about
;; cre form.  It now seems tminverse  and tmlinsolve, now seem to work. --wfs.

;;these are arrays
(DECLARE-TOP(special  *TMARRAYS*  *A2*  *B*  *AA* 
			 *ROW*  *COL*  *ROWINV*  *COLINV*  *INDX* ))

(DECLARE-TOP(SPECIAL N NX IX)) 

(DECLARE-TOP(SPECIAL $LINENUM $DISPFLAG $LINECHAR $WISE $FOOL)) 

(defvar *tmarrays* nil)

;; If N < threshold declared array is used, otherwise hashed array.


(DEFMACRO THRESHOLD () 10.)

(DEFUN TMINITIALFLAG NIL 
       (COND ((NOT (BOUNDP '$WISE)) (SETQ $WISE NIL)))
       (COND ((NOT (BOUNDP '$FOOL)) (SETQ $FOOL NIL))))

;; TMDET returns the determinant of N*N matrix A2 which is in an globally
;; declared array A2.

(DEFUN TMDET (A4 N) 
       (PROG (INDEX RESULT IX) 
	     (TMINITIALFLAG)
	     (TMHEADING)
	     (SETQ IX 0. NX 0.)
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (SETQ INDEX (CONS I INDEX)))
	     (SETQ INDEX ;(REVERSE INDEX)
		   	  (nreverse index))
	     (SETQ RESULT (TMINOR A4 N 1. INDEX 0.))
	     (RETURN RESULT)))

;; TMLIN SOLVES M SETS OF LINEAR EQUATIONS WHITH N UNKNOWN VARIABLES. IT SOLVES
;; ONLY FOR THE FIRST NX UNKNOWNS OUT OF N. THE EQUATIONS ARE EXPRESSED IN
;; MATRIX FORM WHICH IS IN N*(N+M) ARRAY A2. AS USUAL , THE LEFT HAND SIDE N*N
;; OF A2 REPRESENTS THE COEFFICIENT MATRIX, AND NEXT N*M OF A2 IS THE RIGHT
;; HAND SIDE OF THE M SETS OF EQUATIONS.  SUPPOSE N=3, M=2, AND THE UNKKNOWNS
;; ARE (X1 Y1 Z1) FOR THE FIRST SET AND (X2 Y2 Z2) FOR THE SECOND. THEN THE
;; RESULT OF TMLIN IS ((DET) (U1 U2) (V1 V2) (W1 W2)) WHERE DET IS THE
;; DETERMINANT OF THE COEFFICIENT MATRIX AND X1=U1/DET, X2=U2/DET, Y1=V1/DET,
;; Y2=V2/DET ETC.

(DEFUN TMLIN (A4 N M NX) 
       (PROG (INDEX R) 
	     (TMDEFARRAY N)
	     (TMINITIALFLAG)
	     (TMHEADING)
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (SETQ INDEX (CONS I INDEX)))
	     (SETQ INDEX (REVERSE INDEX))
	     (SETQ R
		   (DO ((IX 0. (f1+ IX)) (RESULT))
		       ((> IX NX) (REVERSE RESULT))
		       (SETQ RESULT
			     (CONS (DO ((I 1. (f1+ I)) (RES))
				       ((> I
					   (COND ((= IX 0.) 1.)
						 (T M)))
					(REVERSE RES))
				       (COND ((NOT $WISE)
					      (TMKILLARRAY IX)))
				       (SETQ RES
					     (CONS (TMINOR A4
							   N
							   1.
							   INDEX
							   I)
						   RES)))
				   RESULT))
		       (COND ((AND (= IX 0.)
				   (EQUAL (CAR RESULT)
					  '(0. . 1.)))
			      (merror "COEFFICIENT MATRIX IS SINGULAR")))))
	     (TMREARRAY N)
	     (RETURN R)))

;; TMINOR ACTUALLY COMPUTES THE MINOR DETERMINANT OF A SUBMATRIX OF A2, WHICH
;; IS CONSTRUCTED BY EXTRACTING ROWS (K,K+1,K+2,...,N) AND COLUMNS SPECIFIED BY
;; INDEX. N IS THE DIMENSION OF THE ORIGINAL MATRIX A2.  WHEN TMINOR IS USED
;; FOR LINEAR EQUATION PROGRAM, JRIGHT SPECIFIES A COLUMN OF THE CONSTANT
;; MATRIX WHICH IS PLUGED INTO AN IX-TH COLUMN OF THE COEFFICIENT MATRIX FOR
;; ABTAINING IX-TH UNKNOWN. IN OTHER WORDS, JRIGHT SPECIFIES JRIGHT-TH
;; EQUATION.


(DEFUN TMINOR (A4 N K INDEX JRIGHT) 
       (PROG (SUBINDX L RESULT NAME AORB)
	     (setq a4 (get-array-pointer a4))
	     (COND
	      ((= K N)
	       (SETQ RESULT
		     (COND ((= K IX) (AREF A4 (CAR INDEX) (f+ JRIGHT N)))
			   (T (AREF A4 (CAR INDEX) K)))))
	      (T
	       (DO
		((J 1. (f1+ J)) (SUM '(0. . 1.)))
		((> J (f1+ (f- N K))) (SETQ RESULT SUM))
		(SETQ L (EXTRACT INDEX J))
		(SETQ SUBINDX (CADR L))
		(SETQ L (CAR L))
		(SETQ AORB (COND ((= K IX) (AREF A4 L (f+ JRIGHT N)))
				 (T (AREF A4 L K))))
		(COND
		 ((NOT (EQUAL AORB '(0. . 1.)))
		  (SETQ NAME (TMACCESS SUBINDX))
		  (SETQ 
		   SUM
		   (funcall (COND ((ODDP J) 'RATPLUS)
				  (T 'RATDIFFERENCE))
		    SUM
		    (RATTIMES
		     AORB
		     (COND ($FOOL (TMINOR A4 N (f1+ K) SUBINDX JRIGHT))
			   (T (COND ((NOT (NULL (TMEVAL NAME)))
				     (TMEVAL NAME))
				    ((TMNOMOREUSE J L K)
				     (TMSTORE NAME NIL)
				     (TMINOR A4
					     N
					     (f1+ K)
					     SUBINDX
					     JRIGHT))
				    (T (TMSTORE NAME
						(TMINOR A4
							N
							(f1+ K)
							SUBINDX
							JRIGHT))))))
		     T)))))
		(COND ($WISE (COND ((TMNOMOREUSE J L K)
				    (TMKILL SUBINDX K))))))))
	     (RETURN RESULT))) 

(DEFUN EXTRACT (INDEX J) 
       (DO ((IND INDEX (CDR IND)) (COUNT 1. (f1+ COUNT)) (SUBINDX))
	   ((NULL IND))
	   (COND ((= COUNT J)
		  (RETURN (LIST (CAR IND) (NCONC SUBINDX (CDR IND)))))
		 (T (SETQ SUBINDX (NCONC SUBINDX (LIST (CAR IND)))))))) 

(DECLARE-TOP(SPECIAL VLIST VARLIST GENVAR)) 


(DEFUN TMRATCONV (BBB N M) 
       (PROG (CCC)
	     (declare (special ccc))	;Tell me this worked in Maclisp.  --gsb
	     ;Actually, i suspect it didn't, at least ever since
	     ; (sstatus punt).
	     (SET 'CCC BBB)
	     (DO ((K 1. (f1+ K)))
		 ((> K N))
		 (DO ((J 1. (f1+ J)))
		     ((> J M))
		     (NEWVAR1 (STORE (aref *A2* K J)
				     (maref ccc k j)
;;				     (nth j (nth k *a2*))
;;				     (MEVAL (LIST (LIST 'CCC 'array) K J))  ;;just the
				     ))))
	     
	     (NEWVAR (CONS '(MTIMES) VLIST))
	     (DO ((K 1. (f1+ K)))
		 ((> K N))
		 (DO ((J 1. (f1+ J)))
		     ((> J M))
		     (STORE (aref *A2* K J)
			    (CDR (RATREP* (aref *A2* K J)))))))) 

(DEFMFUN $TMNEWDET N 
       (PROG (*AA* R VLIST) 
	     (COND ((= N 2.)
		    (COND ((NOT (INTEGERP (SETQ N (ARG 2.))))
			   (merror  "WRONG ARG")))
		    (SETQ *AA* (ARG 1.)))
		   ((AND (= N 1.) ($MATRIXP (SETQ *AA* (ARG 1.))))
		    (SETQ N (LENGTH (CDR (ARG 1.)))))
		   (T (merror "WRONG ARG")))
	     (setq  *A2* (*ARRAY nil 'T (f1+ N) (f1+ N)))
	     (TMDEFARRAY N)
	     (TMRATCONV *AA* N N)
	     (SETQ R (CONS (LIST 'MRAT
				 'SIMP
				 VARLIST
				  GENVAR)
			   (TMDET '*A2* N)))
	     (*TMREARRAY '*A2*)
	     (TMREARRAY N)
	     (RETURN R))) 

(DEFMFUN $TMLINSOLVE NARG (TMLINSOLVE (LISTIFY NARG))) 

(DEFUN TMLINSOLVE (ARGLIST) 
       (PROG (EQUATIONS VARS OUTVARS RESULT *AA*) 
	     (SETQ EQUATIONS (CDAR ARGLIST) 
		   VARS (CDADR ARGLIST) 
		   OUTVARS (COND ((NULL (CDDR ARGLIST)) VARS)
				 (T (CDADDR ARGLIST))) 
		   ARGLIST NIL)
	     (SETQ VARS (TMERGE VARS OUTVARS))
	     (SETQ NX (LENGTH OUTVARS))
	     (SETQ N (LENGTH VARS))
	     (COND ((NOT (= N (LENGTH EQUATIONS)))
		    (RETURN (PRINT 'TOO-FEW-OR-MUCH-EQUATIONS))))
	     (SETQ 
	      *AA*
	      (CONS
	       '($MATRIX SIMP)
	       (MAPCAR 
		#'(LAMBDA (EXP) 
		  (APPEND
		   '((MLIST))
		   (MAPCAR #'(LAMBDA (V) 
				    (PROG (R) 
					  (SETQ EXP
						($BOTHCOEF EXP V)
						R
						(CADR EXP)
						EXP
						(MEVAL (CADDR EXP)))
					  (RETURN R)))
			   VARS)
		   (LIST (LIST '(MMINUS) EXP))))
		(MAPCAR #'(LAMBDA (E) (MEVAL (LIST '(MPLUS)
						  ($LHS E)
						  (LIST '(MMINUS)
							($RHS E)))))
			EQUATIONS))))
	     (SETQ RESULT (CDR ($TMLIN *AA* N 1. NX)))
	     (RETURN
	      (DO
	       ((VARS (CONS NIL OUTVARS) (CDR VARS))
		(LABELS)
		(DLABEL)
		(NAME))
	       ((NULL VARS)
		(CONS '(MLIST) (CDR (REVERSE LABELS))))
	       (SETQ NAME (MAKELABEL $LINECHAR))
	       (SETQ $LINENUM (f1+ $LINENUM))
	       (SET NAME
		    (COND ((NULL (CAR VARS))
			   (SETQ DLABEL NAME)
			   (CADAR RESULT))
			  (T (LIST '(MEQUAL)
				   (CAR VARS)
				   (LIST '(MTIMES SIMP)
					 (CADAR RESULT)
					 (LIST '(MEXPT SIMP)
					       DLABEL
					       -1.))))))
	       (SETQ LABELS (CONS NAME LABELS))
	       (SETQ RESULT (CDR RESULT))
	       (COND
		($DISPFLAG (MTELL-OPEN "~M" (NCONC (NCONS '(MLABLE))
					  (NCONS NAME)
					  (NCONS (EVAL NAME)))))))))) 

(DEFUN TMERGE (VARS OUTVARS) 
       (APPEND OUTVARS
	       (PROG (L) 
		     (MAPCAR #'(LAMBDA (V) 
				      (COND ((zl-MEMBER V OUTVARS) NIL)
					    (T (SETQ L (CONS V L)))))
			     VARS)
		     (RETURN (REVERSE L))))) 

(DEFMFUN $TMLIN (*AA* N M NX) 
       (PROG (R VLIST) 
	     (setq  *A2* (*ARRAY nil 'T (f1+ N) (f1+ (f+ M N))))
	     (show *a2*)
	     (TMRATCONV *AA* N (f+ M N))
	     (SETQ 
	      R
	      (CONS
	       '(MLIST)
	       (MAPCAR 
		#'(LAMBDA (RES) 
		  (CONS '(MLIST)
			(MAPCAR #'(LAMBDA (RESULT) 
					 (CONS (LIST 'MRAT
						     'SIMP
						     VARLIST
						      GENVAR)
					       RESULT))
				RES)))
		(TMLIN '*A2* N M NX))))
	     (*TMREARRAY '*A2*)
	     (show *a2*)
	     (RETURN R))) 

(DEFUN TMKILL (*INDX* K) 
       (PROG (NAME SUBINDX J L) 
	     (COND ((NULL *INDX*) (RETURN NIL)))
	     (SETQ NAME (TMACCESS *INDX*))
	     (COND ((NOT (NULL (TMEVAL NAME))) (TMSTORE NAME NIL))
		   (T (DO ((IND *INDX* (CDR IND)) (COUNT 1. (f1+ COUNT)))
			  ((NULL IND))
			  (SETQ L (EXTRACT *INDX* COUNT) 
				J (CAR L) 
				SUBINDX (CADR L))
			  (COND ((= J COUNT)
				 (TMKILL SUBINDX (f1+ K))))))))) 

(DEFUN TMNOMOREUSE (J L K) 
       (COND ((AND (= J L) (OR (> K NX) (< K (f1+ IX)))) T) (T NIL))) 

(DEFUN TMDEFARRAY (N) 
       (PROG (NAME) 
	     (COND
	      (;(GET '*TMARRAYS* 'array)
	       (setq *tmarrays* (get-array-pointer *tmarrays*))
	       (TMREARRAY (f1- (COND ((CADR (ARRAYDIMS *TMARRAYS*)))
				    (T 1.))))))
	     (setq  *TMARRAYS* (*ARRAY nil 'T (f1+ N)))
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (SETQ NAME (COND ((= I 1.) (make-symbol "M"))
				  (T (GENSYM))))
		 (COND ((< N (THRESHOLD))
			;(STORE (aref *TMARRAYS* I) NAME)
			 (set name (*ARRAY nil T (f1+ (TMCOMBI N I))))
			 (STORE (aref *TMARRAYS* I) (get-array-pointer NAME))
			 )
		       
		       (T (STORE (aref *TMARRAYS* I)
				 (LIST NAME
				       'SIMP
				       'array)))))
	     (GENSYM "G")))

;; TMREARRAY kills the TMARRAYS which holds pointers to minors. If (TMARRAYS I)
;; is an atom, it is declared array.  Otherwise it is hashed array.

(DEFUN TMREARRAY (N) 
       (PROG NIL 
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (COND ((ATOM (aref *TMARRAYS* I)) (*TMREARRAY (aref *TMARRAYS* I)))
		       (T (TM$KILL (CAR (aref *TMARRAYS* I))))))
	     (*TMREARRAY '*TMARRAYS*))) 

(DEFUN TMACCESS (INDEX) 
       (PROG (L) 
	     (COND ($FOOL (RETURN NIL)))
	     (SETQ L (LENGTH INDEX))
	     (RETURN
	      (COND ((< N (THRESHOLD))
		     (LIST 'aref (aref *TMARRAYS* L)
			   (DO ((I 1. (f1+ I))
				(X 0. (CAR Y))
				(Y INDEX (CDR Y))
				(SUM 0.))
			       ((> I L) (f1+ SUM))
			       (DO ((J (f1+ X) (f1+ J)))
				   ((= J (CAR Y)))
				   (SETQ SUM (f+ SUM
						(TMCOMBI (f- N J)
							 (f- L I))))))))
		    (T (cons 'aref (CONS (aref *TMARRAYS* L) INDEX)))))) )

(DEFUN TMCOMBI (N I) 
       (COND ((> (f- N I) I)
	      (// (TMFACTORIAL N (f- N I)) (TMFACTORIAL I 0.)))
	     (T (// (TMFACTORIAL N I) (TMFACTORIAL (f- N I) 0.))))) 

(DEFUN TMFACTORIAL (I J) 
       (COND ((= I J) 1.) (T (f* I (TMFACTORIAL (f1- I) J))))) 

(DEFUN TMSTORE (NAME X) 
       (COND ((< N (THRESHOLD))
	      (EVAL (LIST 'STORE NAME (LIST 'QUOTE X))))
	     (T (MSET NAME (LIST '(MQUOTE SIMP) X)) X)))

;; TMKILLARRAY kills all (N-IX+1)*(N-IX+1) minors which are not necessary for
;; the computation of IX-TH variable in the linear equation.  Otherwise, they
;; will do harm.

(DEFUN TMKILLARRAY (IX) 
       (DO ((I (f1+ (f- N IX)) (f1+ I)))
	   ((> I N))
	   (COND ((< N (THRESHOLD))
		  (FILLARRAY (aref *TMARRAYS* I) '(NIL)))
		 (T (TM$KILL (CAR (aref *TMARRAYS* I))))))) 

(DEFUN TMHEADING NIL NIL) 

(DEFUN TMEVAL (E) 
       (PROG (RESULT) 
	     (RETURN (COND ((< N (THRESHOLD)) (EVAL E))
			   (T (SETQ RESULT (MEVAL E))
			      (COND ((EQUAL RESULT E) NIL)
				    (T (CADR RESULT)))))))) 

(DEFUN TM$KILL (E) (KILL1 E))

(DEFMFUN $TMINVERSE ( *AA*) 
       (PROG (R VLIST N M NX) 
	     (SETQ N (LENGTH (CDR *AA*)) M N NX N)
	     (setq  *A2* (*ARRAY nil 'T (f1+ N) (f1+ (f+ M N))))
	     (TMRATCONV *AA* N N)
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (DO ((J 1. (f1+ J)))
		     ((> J M))
		     (STORE (aref *A2* I (f+ N J))
			    (COND ((= I J) '(1. . 1.))
				  (T '(0. . 1.))))))
	     (SETQ 
	      R
	      (MAPCAR 
	       #'(LAMBDA (RES) 
		 (CONS
		  '(MLIST)
		  (MAPCAR 
		   #'(LAMBDA (RESULT) 
			    ($RATDISREP (CONS (LIST 'MRAT
						    'SIMP
						    VARLIST
						     GENVAR)
					      RESULT)))
		   RES)))
	       (TMLIN '*A2* N M NX)))
	     (SETQ R
		   (LIST '(MTIMES SIMP)
			 (LIST '(MEXPT SIMP) (CADAR R) -1.)
			 (CONS '($MATRIX SIMP) (CDR R))))
	     (*TMREARRAY '*A2*)
	     (RETURN R))) 

(DEFUN *TMREARRAY (X) (*REARRAY X)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			       
;;THIS IS A UTILITY PACKAGE FOR SPARSE
;;MATRIX INVERSION. A3 IS A N*N MATRIX.
;;IT RETURNS A LIST OF LISTS, SUCH AS
;;((I1 I2 ...) (J1 J2...) ...) WHERE (I1
;;I2 ..) SHOWS THE ROWS WHICH BELONGS TO
;;THE FIRST BLOCK, AND SO ON.  THE ROWS
;;SHOUD BE REORDERED IN THIS ORDER. THE
;;COLUMNS ARE NOT CHANGED. IT RETURNS NIL
;;IF A3 IS "OBVIOUSLY" SINGULAR.

;; (DEFUN TMISOLATE (A3 N)
;;        (PROG (NODELIST)
;; 	     (SETQ A3 (GET A3 'ARRAY))
;; 	     (setq  B (*ARRAY nil 'T (f1+ N) (f1+ N)))
;; 	     (setq  ROW (*ARRAY nil 'T (f1+ N)))
;; 	     (setq  COL (*ARRAY nil 'T (f1+ N)))
;; 	     (DO ((I 1. (f1+ I)))
;; 		 ((> I N))
;; 		 (STORE (ROW I) I)
;; 		 (STORE (COL I) I))
;; 	     (DO ((I 1. (f1+ I)))
;; 		 ((> I N))
;; 		 (DO ((J 1. (f1+ J)))
;; 		     ((> J N))
;; 		     (STORE (B I J)
;; 			    (NOT (EQUAL (AREF A3 I J)
;; 					'(0. . 1.))))))
;; 	     (COND ((NULL (TMPIVOT-ISOLATE 1.))
;; 		    (SETQ NODELIST NIL)
;; 		    (GO EXIT)))
;; 	     (DO ((I 1. (f1+ I)))
;; 		 ((> I N))
;; 		 (DO ((J 1. (f1+ J)))
;; 		     ((> J I))
;; 		     (STORE (B (ROW J) (COL I))
;; 			    (OR (B (ROW I) (COL J))
;; 				(B (ROW J) (COL I))))
;; 		     (STORE (B (ROW I) (COL J)) (B (ROW J) (COL I))))
;; 		 (STORE (B (ROW I) (COL I)) T))
;; 	     (DO ((I 1. (f1+ I)))
;; 		 ((> I N))
;; 		 (COND ((EQ (B (ROW I) (COL I)) T)
;; 			(SETQ NODELIST
;; 			      (CONS (TMPULL-OVER I N) NODELIST)))))
;; 	     EXIT
;; 	     (*TMREARRAY 'B)
;; 	     (*TMREARRAY 'ROW)
;; 	     (*TMREARRAY 'COL)
;; 	     (RETURN (REVERSE NODELIST))))) 

;; (DEFUN TMPULL-OVER (P N) 
;;        (PROG (Q) 
;; 	     (STORE (B (ROW P) (COL P)) NIL)
;; 	     (DO ((J 1. (f1+ J)))
;; 		 ((> J N) (SETQ Q NIL))
;; 		 (COND ((EQ (B (ROW P) (COL J)) T)
;; 			(RETURN (SETQ Q J)))))
;; 	     (COND ((NULL Q) (RETURN (LIST (ROW P))))
;; 		   (T (DO ((J 1. (f1+ J)))
;; 			  ((> J N))
;; 			  (STORE (B (ROW Q) (COL J))
;; 				 (OR (B (ROW Q) (COL J))
;; 				     (B (ROW P) (COL J))))
;; 			  (STORE (B (ROW J) (COL Q))
;; 				 (B (ROW Q) (COL J))))
;; 		      (TMCRIP P)
;; 		      (RETURN (CONS (ROW P) (TMPULL-OVER Q N))))))) 

;; (DEFUN TMCRIP (P) 
;;        (DO ((I 1. (f1+ I)))
;; 	   ((> I N))
;; 	   (STORE (B (ROW P) (COL I)) NIL)
;; 	   (STORE (B (ROW I) (COL P)) NIL)))		

;;TMPIVOT-ISOLATE CARRIES OUT PIVOTTING
;;SO THAT THE ALL DIAGONAL ELEMENTS ARE
;;NONZERO. THIS GARANTIES WE HAVE MAXIMUM
;;NUMBER OF BLOCKS ISOLATED.

(DEFUN TMPIVOT-ISOLATE (K) 
       (COND ((> K N) T)
	     (T (DO ((I K (f1+ I)))
		    ((> I N) NIL)
		    (COND ((aref *B* (aref *ROW* I) (aref *COL* K))
			   (TMEXCHANGE '*ROW* K I)
			   (COND ((TMPIVOT-ISOLATE (f1+ K)) (RETURN T))
				 (T (TMEXCHANGE '*ROW*
						K
						I))))))))) 

(DEFUN TMEXCHANGE (ROWCOL I J) 
       (PROG (DUMMY) 
	     (SETQ ROWCOL (get-array-pointer rowcol))
	     (SETQ DUMMY (AREF ROWCOL I))
	     (STORE (AREF ROWCOL I) (AREF ROWCOL J))
	     (STORE (AREF ROWCOL J) DUMMY)))	


;; PROGRAM TO PREDICT ZERO ELEMENTS IN
;; THE SOLUTION OF INVERSE OR LINEAR
;; EQUATION. A IS THE COEFFICIENT MATRIX.
;; B IS THE RIGHT HAND SIDE MATRIX FOR
;; LINEAR EQUATIONS. A3 IS N*N AND B IS
;; M*M. X IS AN N*M MATRIX WHERE T -NIL
;; PATTERN SHOWING THE ZERO ELEMENTS IN
;; THE RESULT IS RETURND. T CORRESPONDS TO
;; NON-ZERO ELEMENT. IN THE CASE OF
;; INVERSE, YOU CAN PUT ANYTHING (SAY,NIL)
;; FOR B AND 0 FOR M.  NORMALLY IT RETURNS
;; T, BUT IN CASE OF SINGULAR MATRIX, IT
;; RETURNS NIL.

;; (DEFUN TMPREDICT (A3 B X N M)
;;   (PROG (FLAGINV FLAG-NONSINGULAR)
;; 	(SETQ A3 (GET A3 'ARRAY) B (GET B 'ARRAY) X (GET X 'ARRAY))
;; 	(setq  AA (*ARRAY nil 'T (f1+ N) (f1+ N)))
;; 	(setq  ROW (*ARRAY nil 'T (f1+ N)))
;; 	(SETQ FLAGINV (= M 0.))
;; 	(COND (FLAGINV (SETQ M N)))
;; 	(DO ((I 1. (f1+ I)))
;; 	    ((> I N))
;; 	    (DO ((J 1. (f1+ J)))
;; 		((> J N))
;; 		(STORE (AA I J)
;; 		       (NOT (EQUAL (AREF A3 I J) '(0. . 1.))))))
;; 	(DO ((I 1. (f1+ I)))
;; 	    ((> I N))
;; 	    (DO ((J 1. (f1+ J)))
;; 		((> J M))
;; 		(STORE (AREF X I J)
;; 		       (COND (FLAGINV (EQ I J))
;; 			     (T (EQUAL (AREF B I J)
;; 				       '(0. . 1.)))))))
;; 	(DO ((I 1. (f1+ I))) ((> I N)) (STORE (ROW I) I))
;; 		;FORWARD ELIMINATION.
;; 	(DO ((I 1. (f1+ I)))
;; 	    ((> I N))
;; 	    (SETQ FLAG-NONSINGULAR
;; 		  (DO ((II I (f1+ II)))
;; 		      ((> II N) NIL)
;; 		      (COND ((AA (ROW II) I)
;; 			     (TMEXCHANGE 'ROW II I)
;; 			     (RETURN T)))))
;; 	    (COND ((NULL FLAG-NONSINGULAR) (RETURN NIL)))
;; 	    (DO ((II (f1+ I) (f1+ II)))
;; 		((> II N))
;; 		(COND ((AA (ROW II) I)
;; 		       (DO ((JJ (f1+ I) (f1+ JJ)))
;; 			   ((> JJ N))
;; 			   (STORE (AA (ROW II) JJ)
;; 				  (OR (AA (ROW I) JJ)
;; 				      (AA (ROW II) JJ))))
;; 		       (DO ((JJ 1. (f1+ JJ)))
;; 			   ((> JJ M))
;; 			   (STORE (AREF X (ROW II) JJ)
;; 				  (OR (AREF X (ROW I) JJ)
;; 				      (AREF X (ROW II) JJ))))))))
;; 	(COND ((NULL FLAG-NONSINGULAR) (GO EXIT)))       ;GET OUT  BACKWARD SUBSTITUTION
;; 	(DO ((I (f1- N) (f1- I)))
;; 	    ((< I 1.))
;; 	    (DO ((L 1. (f1+ L)))
;; 		((> L M))
;; 		(STORE (AREF X (ROW I) L)
;; 		       (OR (AREF X (ROW I) L)
;; 			   (DO ((J (f1+ I) (f1+ J)) (SUM))
;; 			       ((> J N) SUM)
;; 			       (SETQ SUM
;; 				     (OR SUM
;; 					 (AND (AA (ROW I) J)
;; 					      (AREF
;; 							 X
;; 							 (ROW J)
;; 							 L)))))))))
;; 	       ;RECOVER THE ORDER.
;; 	(TMPERMUTE 'X N M 0. 0. 'ROW N 'ROW)
;;    EXIT (*TMREARRAY 'ROW) (*TMREARRAY 'AA) (RETURN FLAG-NONSINGULAR)))

;TMPERMUTE PERMUTES THE ROWS OR COLUMNS
;OF THE N*M MATRIX AX ACCORDING TO THE
;SPECIFICATION OF INDEXLIST. THE FLAG
;MUST BE SET 'ROW IF ROW PERMUTATION IS
;DESIRED , OR 'COL OTHERWISE. THE RESULT
;IS IN AX. NM IS THE DIMENSION OF
;INDEXLIST.

(DEFUN TMPERMUTE (AX N M RBIAS CBIAS INDEXLIST NM FLAG) 
       (PROG (K L) 
;	     (SETQ AX (GET AX 'array) 
;		   INDEXLIST (GET INDEXLIST 'array))
	     (setq ax (get-array-pointer ax))
	     (setq indexlist (get-array-pointer indexlist))
	     (ARRAY *INDX* T (f1+ NM))
	     (DO ((I 1. (f1+ I)))
		 ((> I NM))
		 (STORE (aref *INDX* I) (AREF INDEXLIST I)))
	     (DO ((I 1. (f1+ I)))
		 ((> I NM))
		 (COND ((NOT (= (aref *INDX* I) I))
			(PROG NIL 
			      (TMMOVE AX N M RBIAS CBIAS I 0. FLAG)
			      (SETQ L I)
			 LOOP (SETQ K (aref *INDX* L))
			      (STORE (aref *INDX* L) L)
			      (COND ((= K I)
				     (TMMOVE AX
					     N
					     M
					     RBIAS
					     CBIAS
					     0.
					     L
					     FLAG))
				    (T (TMMOVE AX
					       N
					       M
					       RBIAS
					       CBIAS
					       K
					       L
					       FLAG)
				       (SETQ L K)
				       (GO LOOP)))))))
	     (*TMREARRAY '*INDX*))) 

(DEFUN TMMOVE (AX N M RBIAS CBIAS I J FLAG) 
       (PROG (LL)
	     (setq ax (get-array-pointer ax))
	     (SETQ LL (COND ((EQ FLAG '*ROW*) (f- M CBIAS))
			    (T (f- N RBIAS))))
	     (DO ((K 1. (f1+ K)))
		 ((> K LL))
		 (COND ((EQ FLAG '*ROW*)
			(STORE (AREF
					  AX
					  (f+ RBIAS J)
					  (f+ CBIAS K))
			       (AREF
					  AX
					  (f+ RBIAS I)
					  (f+ CBIAS K))))
		       (T (STORE (AREF
					    AX
					    (f+ RBIAS K)
					    (f+ CBIAS J))
				 (AREF
					    AX
					    (f+ RBIAS K)
					    (f+ CBIAS I))))))))

;TMSYMETRICP CHECKS THE SYMETRY OF THE MATRIX.

(DEFUN TMSYMETRICP        (A3 N)
       (SETQ A3 (GET-array-pointer A3))
       (DO ((I 1. (f1+ I)))
	   ((> I N) T)
	   (COND ((NULL (DO ((J (f1+ I) (f1+ J)))
			    ((> J N) T)
			    (COND ((NOT (EQUAL (AREF
							  A3
							  I
							  J)
					       (AREF
							  A3
							  J
							  I)))
				   (RETURN NIL)))))
		  (RETURN NIL)))))

;TMLATTICE CHECKS THE "LATTICE"
;STRUCTURE OF THE MATRIX A. IT RETURNS
;NIL IF THE MATRIX IS "OBVIOUSLY"
;SINGULAR. OTHERWISE IT RETURNS A LIST
;(L1 L2 ... LM) WHERE M IS THE NUMBER OF
;BLOCKS (STRONGLY CONNECTED SUBGRAPHS),
;AND L1 L2 ... ARE LIST OF ROW AND
;COLUMN NUBERS WHICH BELONG TO EACH
;BLOCKS. THE LIST LOOKS LIKE ((R1 C1)
;(R2 C2) ...) WHERE R R'S ARE ROWS AND
;C'S ARE COLUMMS.

(DEFUN TMLATTICE (A3 XROW XCOL N) 
       (PROG (RES) 
	     (setq a3 (get-array-pointer a3))
	     (setq xrow (get-array-pointer xrow))
	     (setq xcol (get-array-pointer xcol))
	     (setq *b* (*ARRAY nil T (f1+ N) (f1+ N)))
	     (setq *ROW* (*ARRAY nil T (f1+ N)))
	     (setq *col* (*ARRAY nil  T (f1+ N)))
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (DO ((J 1. (f1+ J)))
		     ((> J N))
		     (STORE (aref *B* I J)
			    (NOT (EQUAL (AREF A3 I J)
					'(0. . 1.))))))
	     (DO ((I 0. (f1+ I)))
		 ((> I N))
		 (STORE (aref *ROW* I) I)
		 (STORE (aref *COL* I) I))
	     (COND ((NULL (TMPIVOT-ISOLATE 1.))
		    (SETQ RES NIL)
		    (GO EXIT)))
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (STORE (aref *B* (aref *ROW* I) (aref *COL* I)) I)
		 (STORE (aref *B* (aref *ROW* I) (aref *COL* 0.)) T
			))
	     (TMLATTICE1 1.)
	     (SETQ RES (TMSORT-LATTICE XROW XCOL))
	EXIT (*TMREARRAY '*B*)
	     (*TMREARRAY '*ROW*)
	     (*TMREARRAY '*COL*)
	     (RETURN RES))) 

(DEFUN TMLATTICE1 (K) 
       (COND ((= K N) NIL)
	     (T (TMLATTICE1 (f1+ K))
		(DO ((LOOPPATH))
		    (NIL)
		    (COND ((SETQ LOOPPATH (TMPATHP K K))
			   (TMUNIFY-LOOP K (CDR LOOPPATH)))
			  (T (RETURN NIL))))))) 

(DEFUN TMPATHP (J K) 
       (COND ((EQUAL (aref *B* (aref *ROW* J) (aref *COL* K)) T) (LIST J K))
	     (T (DO ((JJ K (f1+ JJ)) (PATH))
		    ((> JJ N))
		    (COND ((AND (EQUAL (aref *B* (aref *ROW* J) (aref *COL* JJ)) T)
				(SETQ PATH (TMPATHP JJ K)))
			   (RETURN (CONS J PATH)))))))) 

(DEFUN TMUNIFY-LOOP (K CHAIN) 
       (PROG (L DUMMYK DUMMYL) 
	     (SETQ L (CAR CHAIN))
	     (COND ((= L K) (RETURN NIL)))
	     (SETQ DUMMYK (aref *B* (aref *ROW* K) (aref *COL* K)))
	     (SETQ DUMMYL (aref *B* (aref *ROW* L) (aref *COL* L)))
	     (STORE (aref *B* (aref *ROW* K) (aref *COL* K)) NIL)
	     (STORE (aref *B* (aref *ROW* L) (aref *COL* L)) NIL)
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (STORE (aref *B* (aref *ROW* K) (aref *COL* I))
			(OR (aref *B* (aref *ROW* K) (aref *COL* I)) (aref *B* (aref *ROW* L) (aref *COL* I))))
		 (STORE (aref *B* (aref *ROW* I) (aref *COL* K))
			(OR (aref *B* (aref *ROW* I) (aref *COL* K)) (aref *B* (aref *ROW* I) (aref *COL* L))))
		 (STORE (aref *B* (aref *ROW* L) (aref *COL* I)) NIL)
		 (STORE (aref *B* (aref *ROW* I) (aref *COL* L)) NIL))
	     (STORE (aref *B* (aref *ROW* K) (aref *COL* K)) DUMMYL)
	     (STORE (aref *B* (aref *ROW* L) (aref *COL* L)) DUMMYK)
	     (STORE (aref *B* (aref *ROW* K) (aref *COL* 0.)) T)
	     (STORE (aref *B* (aref *ROW* L) (aref *COL* 0.)) NIL)
	     (TMUNIFY-LOOP K (CDR CHAIN)))) 

(DEFUN TMSORT-LATTICE (XROW XCOL) 
       (PROG (NODELIST RESULT) 
	     (SETQ NODELIST (TMSORT1))
	     (SETQ 
	      RESULT
	      (DO ((X NODELIST (CDR X)) (RESULT))
		  ((NULL X) RESULT)
		  (SETQ RESULT
			(CONS (DO ((NEXT (aref *B* (aref *ROW* (CAR X))
					    (aref *COL* (CAR X)))
					 (aref *B* (aref *ROW* NEXT) (aref *COL* NEXT)))
				   (RES))
				  ((= NEXT (CAR X))
				   (CONS (LIST (aref *ROW* NEXT) (aref *COL* NEXT))
					 RES))
				  (SETQ RES
					(CONS (LIST (aref *ROW* NEXT)
						    (aref *COL* NEXT))
					      RES)))
			      RESULT))))
	     (DO ((LIST1 RESULT (CDR LIST1)) (I 1.))
		 ((NULL LIST1))
		 (DO ((LIST2 (CAR LIST1) (CDR LIST2)))
		     ((NULL LIST2))
		     (STORE (AREF XROW I) (CAAR LIST2))
		     (STORE (AREF XCOL I) (CADAR LIST2))
		     (SETQ I (f1+ I))))
	     (RETURN RESULT))) 

;; (DEFUN TMLESS (I J) (B (ROW I) (COL J))) 

(DEFUN TMSORT1 NIL 
       (DO ((I 1. (f1+ I)) (RESULT))
	   ((> I N) RESULT)
	   (COND ((AND (aref *B* (aref *ROW* I) (aref *COL* 0.)) (TMMAXP I))
		  (DO ((J 1. (f1+ J)))
		      ((> J N))
		      (COND ((NOT (= J I))
			     (STORE (aref *B* (aref *ROW* I) (aref *COL* J)) NIL))))
		  (STORE (aref *B* (aref *ROW* I) (aref *COL* 0.)) NIL)
		  (SETQ RESULT (CONS I RESULT))
		  (SETQ I 0.))))) 

(DEFUN TMMAXP (I) 
       (DO ((J 1. (f1+ J)))
	   ((> J N) T)
	   (COND ((AND (NOT (= I J)) (aref *B* (aref *ROW* J) (aref *COL* I)))
		  (RETURN NIL)))))

;;UNPIVOT IS USED IN PAUL WANG'S PROGRAM
;;TO RECOVER THE PIVOTTING. TO GET THE
;;INVERSE OF A, PAUL'S PROGRAM COMPUTES
;;THE INVERSE OF U*A*V BECAUSE OF
;;BLOCKING. LET THE INVERSE Y. THEN
;;A^^-1=V*Y*U. WHERE U AND V ARE
;;FUNDAMENTAL TRANSFORMATION
 ;;(PERMUTATION). UNPIVOT DOES THIS,
;;NAMELY, GIVEN A MATRIX A3, INDEX ROW
;;AND COL ,WHICH CORRESPONDS TO THE Y , U
;; AND V, RESPECTIVELY, IT COMPUTES V*Y*U
;;AND RETURNS IT TO THE SAME ARGUMENT A.

(DEFUN TMUNPIVOT (A3 *ROW* *COL* N M) 
       (PROG NIL 
	     (setq *col* (get-array-pointer *col*))
	     (setq *row* (get-array-pointer *row*))
	     (setq *rowinv* (*ARRAY nil T (f1+ N)))
	     (setq *colinv* (*ARRAY nil T (f1+ N)))
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (STORE (aref *ROWINV* (AREF *ROW* I)) I))
	     (DO ((I 1. (f1+ I)))
		 ((> I N))
		 (STORE (aref *COLINV* (AREF *COL* I)) I))
	     (TMPERMUTE A3 N M 0. N '*COLINV* N '*ROW*)
	     (TMPERMUTE A3 N M 0. N '*ROWINV* N '*COL*)
	     (*TMREARRAY '*ROWINV*)
	     (*TMREARRAY '*COLINV*))) 

#-NIL
(DECLARE-TOP(UNSPECIAL N  #-cl vlist NX IX))
