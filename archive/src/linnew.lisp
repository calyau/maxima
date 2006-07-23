;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
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
(declare-top(special  *tmarrays*  *a2*  *b*  *aa* 
		      *row*  *col*  *rowinv*  *colinv*  *indx* ))

(declare-top(special n nx ix)) 

(declare-top(special $linenum $dispflag $linechar $wise $fool)) 

(defvar *tmarrays* nil)

;; If N < threshold declared array is used, otherwise hashed array.


(defmacro threshold () 10.)

(defun tminitialflag nil 
  (cond ((not (boundp '$wise)) (setq $wise nil)))
  (cond ((not (boundp '$fool)) (setq $fool nil))))

;; TMDET returns the determinant of N*N matrix A2 which is in an globally
;; declared array A2.

(defun tmdet (a4 n) 
  (prog (index result ix) 
     (tminitialflag)
     (tmheading)
     (setq ix 0. nx 0.)
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (setq index (cons i index)))
     (setq index			;(REVERSE INDEX)
	   (nreverse index))
     (setq result (tminor a4 n 1. index 0.))
     (return result)))

;; TMLIN SOLVES M SETS OF LINEAR EQUATIONS WHITH N UNKNOWN VARIABLES. IT SOLVES
;; ONLY FOR THE FIRST NX UNKNOWNS OUT OF N. THE EQUATIONS ARE EXPRESSED IN
;; MATRIX FORM WHICH IS IN N*(N+M) ARRAY A2. AS USUAL , THE LEFT HAND SIDE N*N
;; OF A2 REPRESENTS THE COEFFICIENT MATRIX, AND NEXT N*M OF A2 IS THE RIGHT
;; HAND SIDE OF THE M SETS OF EQUATIONS.  SUPPOSE N=3, M=2, AND THE UNKKNOWNS
;; ARE (X1 Y1 Z1) FOR THE FIRST SET AND (X2 Y2 Z2) FOR THE SECOND. THEN THE
;; RESULT OF TMLIN IS ((DET) (U1 U2) (V1 V2) (W1 W2)) WHERE DET IS THE
;; DETERMINANT OF THE COEFFICIENT MATRIX AND X1=U1/DET, X2=U2/DET, Y1=V1/DET,
;; Y2=V2/DET ETC.

(defun tmlin (a4 n m nx) 
  (prog (index r) 
     (tmdefarray n)
     (tminitialflag)
     (tmheading)
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (setq index (cons i index)))
     (setq index (reverse index))
     (setq r
	   (do ((ix 0. (f1+ ix)) (result))
	       ((> ix nx) (reverse result))
	     (setq result
		   (cons (do ((i 1. (f1+ i)) (res))
			     ((> i
				 (cond ((= ix 0.) 1.)
				       (t m)))
			      (reverse res))
			   (cond ((not $wise)
				  (tmkillarray ix)))
			   (setq res
				 (cons (tminor a4
					       n
					       1.
					       index
					       i)
				       res)))
			 result))
	     (cond ((and (= ix 0.)
			 (equal (car result)
				'(0. . 1.)))
		    (merror "Coefficient matrix is singular")))))
     (tmrearray n)
     (return r)))

;; TMINOR ACTUALLY COMPUTES THE MINOR DETERMINANT OF A SUBMATRIX OF A2, WHICH
;; IS CONSTRUCTED BY EXTRACTING ROWS (K,K+1,K+2,...,N) AND COLUMNS SPECIFIED BY
;; INDEX. N IS THE DIMENSION OF THE ORIGINAL MATRIX A2.  WHEN TMINOR IS USED
;; FOR LINEAR EQUATION PROGRAM, JRIGHT SPECIFIES A COLUMN OF THE CONSTANT
;; MATRIX WHICH IS PLUGED INTO AN IX-TH COLUMN OF THE COEFFICIENT MATRIX FOR
;; ABTAINING IX-TH UNKNOWN. IN OTHER WORDS, JRIGHT SPECIFIES JRIGHT-TH
;; EQUATION.


(defun tminor (a4 n k index jright) 
  (prog (subindx l result name aorb)
     (setq a4 (get-array-pointer a4))
     (cond
       ((= k n)
	(setq result
	      (cond ((= k ix) (aref a4 (car index) (f+ jright n)))
		    (t (aref a4 (car index) k)))))
       (t
	(do
	 ((j 1. (f1+ j)) (sum '(0. . 1.)))
	 ((> j (f1+ (f- n k))) (setq result sum))
	  (setq l (extract index j))
	  (setq subindx (cadr l))
	  (setq l (car l))
	  (setq aorb (cond ((= k ix) (aref a4 l (f+ jright n)))
			   (t (aref a4 l k))))
	  (cond
	    ((not (equal aorb '(0. . 1.)))
	     (setq name (tmaccess subindx))
	     (setq 
	      sum
	      (funcall (cond ((oddp j) 'ratplus)
			     (t 'ratdifference))
		       sum
		       (rattimes
			aorb
			(cond ($fool (tminor a4 n (f1+ k) subindx jright))
			      (t (cond ((not (null (tmeval name)))
					(tmeval name))
				       ((tmnomoreuse j l k)
					(tmstore name nil)
					(tminor a4
						n
						(f1+ k)
						subindx
						jright))
				       (t (tmstore name
						   (tminor a4
							   n
							   (f1+ k)
							   subindx
							   jright))))))
			t)))))
	  (cond ($wise (cond ((tmnomoreuse j l k)
			      (tmkill subindx k))))))))
     (return result))) 

(defun extract (index j) 
  (do ((ind index (cdr ind)) (count 1. (f1+ count)) (subindx))
      ((null ind))
    (cond ((= count j)
	   (return (list (car ind) (nconc subindx (cdr ind)))))
	  (t (setq subindx (nconc subindx (list (car ind)))))))) 

(declare-top(special vlist varlist genvar)) 


(defun tmratconv (bbb n m) 
  (prog (ccc)
     (declare (special ccc))   ;Tell me this worked in Maclisp.  --gsb
					;Actually, i suspect it didn't, at least ever since
					; (sstatus punt).
     (set 'ccc bbb)
     (do ((k 1. (f1+ k)))
	 ((> k n))
       (do ((j 1. (f1+ j)))
	   ((> j m))
	 (newvar1 (store (aref *a2* k j)
			 (maref ccc k j)
			 ;;				     (nth j (nth k *a2*))
			 ;;				     (MEVAL (LIST (LIST 'CCC 'array) K J))  ;;just the
			 ))))
	     
     (newvar (cons '(mtimes) vlist))
     (do ((k 1. (f1+ k)))
	 ((> k n))
       (do ((j 1. (f1+ j)))
	   ((> j m))
	 (store (aref *a2* k j)
		(cdr (ratrep* (aref *a2* k j)))))))) 

(defmfun $tmnewdet n 
  (prog (*aa* r vlist) 
     (cond ((= n 2.)
	    (cond ((not (integerp (setq n (arg 2.))))
		   (merror  "Wrong arg")))
	    (setq *aa* (arg 1.)))
	   ((and (= n 1.) ($matrixp (setq *aa* (arg 1.))))
	    (setq n (length (cdr (arg 1.)))))
	   (t (merror "Wrong arg")))
     (setq  *a2* (*array nil 't (f1+ n) (f1+ n)))
     (tmdefarray n)
     (tmratconv *aa* n n)
     (setq r (cons (list 'mrat
			 'simp
			 varlist
			 genvar)
		   (tmdet '*a2* n)))
     (*tmrearray '*a2*)
     (tmrearray n)
     (return r))) 

(defmfun $tmlinsolve narg (tmlinsolve (listify narg))) 

(defun tmlinsolve (arglist) 
  (prog (equations vars outvars result *aa*) 
     (setq equations (cdar arglist) 
	   vars (cdadr arglist) 
	   outvars (cond ((null (cddr arglist)) vars)
			 (t (cdaddr arglist))) 
	   arglist nil)
     (setq vars (tmerge vars outvars))
     (setq nx (length outvars))
     (setq n (length vars))
     (cond ((not (= n (length equations)))
	    (return (print 'too-few-or-much-equations))))
     (setq 
      *aa*
      (cons
       '($matrix simp)
       (mapcar 
	#'(lambda (exp) 
	    (append
	     '((mlist))
	     (mapcar #'(lambda (v) 
			 (prog (r) 
			    (setq exp
				  ($bothcoef exp v)
				  r
				  (cadr exp)
				  exp
				  (meval (caddr exp)))
			    (return r)))
		     vars)
	     (list (list '(mminus) exp))))
	(mapcar #'(lambda (e) (meval (list '(mplus)
					   ($lhs e)
					   (list '(mminus)
						 ($rhs e)))))
		equations))))
     (setq result (cdr ($tmlin *aa* n 1. nx)))
     (return
       (do
	((vars (cons nil outvars) (cdr vars))
	 (labels)
	 (dlabel)
	 (name))
	((null vars)
	 (cons '(mlist) (cdr (reverse labels))))
	 (setq name (makelabel $linechar))
	 (setq $linenum (f1+ $linenum))
	 (set name
	      (cond ((null (car vars))
		     (setq dlabel name)
		     (cadar result))
		    (t (list '(mequal)
			     (car vars)
			     (list '(mtimes simp)
				   (cadar result)
				   (list '(mexpt simp)
					 dlabel
					 -1.))))))
	 (setq labels (cons name labels))
	 (setq result (cdr result))
	 (cond
	   ($dispflag (mtell-open "~M" (nconc (ncons '(mlable))
					      (ncons name)
					      (ncons (eval name)))))))))) 

(defun tmerge (vars outvars) 
  (append outvars
	  (prog (l) 
	     (mapcar #'(lambda (v) 
			 (cond ((zl-member v outvars) nil)
			       (t (setq l (cons v l)))))
		     vars)
	     (return (reverse l))))) 

(defmfun $tmlin (*aa* n m nx) 
  (prog (r vlist) 
     (setq  *a2* (*array nil 't (f1+ n) (f1+ (f+ m n))))
     (show *a2*)
     (tmratconv *aa* n (f+ m n))
     (setq 
      r
      (cons
       '(mlist)
       (mapcar 
	#'(lambda (res) 
	    (cons '(mlist)
		  (mapcar #'(lambda (result) 
			      (cons (list 'mrat
					  'simp
					  varlist
					  genvar)
				    result))
			  res)))
	(tmlin '*a2* n m nx))))
     (*tmrearray '*a2*)
     (show *a2*)
     (return r))) 

(defun tmkill (*indx* k) 
  (prog (name subindx j l) 
     (cond ((null *indx*) (return nil)))
     (setq name (tmaccess *indx*))
     (cond ((not (null (tmeval name))) (tmstore name nil))
	   (t (do ((ind *indx* (cdr ind)) (count 1. (f1+ count)))
		  ((null ind))
		(setq l (extract *indx* count) 
		      j (car l) 
		      subindx (cadr l))
		(cond ((= j count)
		       (tmkill subindx (f1+ k))))))))) 

(defun tmnomoreuse (j l k) 
  (cond ((and (= j l) (or (> k nx) (< k (f1+ ix)))) t) (t nil))) 

(defun tmdefarray (n) 
  (prog (name) 
     (cond
       (				;(GET '*TMARRAYS* 'array)
	(setq *tmarrays* (get-array-pointer *tmarrays*))
	(tmrearray (f1- (cond ((cadr (arraydims *tmarrays*)))
			      (t 1.))))))
     (setq  *tmarrays* (*array nil 't (f1+ n)))
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (setq name (cond ((= i 1.) (make-symbol "M"))
			(t (gensym))))
       (cond ((< n (threshold))
					;(STORE (aref *TMARRAYS* I) NAME)
	      (set name (*array nil t (f1+ (tmcombi n i))))
	      (store (aref *tmarrays* i) (get-array-pointer name))
	      )
		       
	     (t (store (aref *tmarrays* i)
		       (list name
			     'simp
			     'array)))))
     (gensym "G")))

;; TMREARRAY kills the TMARRAYS which holds pointers to minors. If (TMARRAYS I)
;; is an atom, it is declared array.  Otherwise it is hashed array.

(defun tmrearray (n) 
  (prog nil 
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (cond ((atom (aref *tmarrays* i)) (*tmrearray (aref *tmarrays* i)))
	     (t (tm$kill (car (aref *tmarrays* i))))))
     (*tmrearray '*tmarrays*))) 

(defun tmaccess (index) 
  (prog (l) 
     (cond ($fool (return nil)))
     (setq l (length index))
     (return
       (cond ((< n (threshold))
	      (list 'aref (aref *tmarrays* l)
		    (do ((i 1. (f1+ i))
			 (x 0. (car y))
			 (y index (cdr y))
			 (sum 0.))
			((> i l) (f1+ sum))
		      (do ((j (f1+ x) (f1+ j)))
			  ((= j (car y)))
			(setq sum (f+ sum
				      (tmcombi (f- n j)
					       (f- l i))))))))
	     (t (cons 'aref (cons (aref *tmarrays* l) index)))))) )

(defun tmcombi (n i) 
  (cond ((> (f- n i) i)
	 (// (tmfactorial n (f- n i)) (tmfactorial i 0.)))
	(t (// (tmfactorial n i) (tmfactorial (f- n i) 0.))))) 

(defun tmfactorial (i j) 
  (cond ((= i j) 1.) (t (f* i (tmfactorial (f1- i) j))))) 

(defun tmstore (name x) 
  (cond ((< n (threshold))
	 (eval (list 'store name (list 'quote x))))
	(t (mset name (list '(mquote simp) x)) x)))

;; TMKILLARRAY kills all (N-IX+1)*(N-IX+1) minors which are not necessary for
;; the computation of IX-TH variable in the linear equation.  Otherwise, they
;; will do harm.

(defun tmkillarray (ix) 
  (do ((i (f1+ (f- n ix)) (f1+ i)))
      ((> i n))
    (cond ((< n (threshold))
	   (fillarray (aref *tmarrays* i) '(nil)))
	  (t (tm$kill (car (aref *tmarrays* i))))))) 

(defun tmheading nil nil) 

(defun tmeval (e) 
  (prog (result) 
     (return (cond ((< n (threshold)) (eval e))
		   (t (setq result (meval e))
		      (cond ((equal result e) nil)
			    (t (cadr result)))))))) 

(defun tm$kill (e) (kill1 e))

(defmfun $tminverse ( *aa*) 
  (prog (r vlist n m nx) 
     (setq n (length (cdr *aa*)) m n nx n)
     (setq  *a2* (*array nil 't (f1+ n) (f1+ (f+ m n))))
     (tmratconv *aa* n n)
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (do ((j 1. (f1+ j)))
	   ((> j m))
	 (store (aref *a2* i (f+ n j))
		(cond ((= i j) '(1. . 1.))
		      (t '(0. . 1.))))))
     (setq 
      r
      (mapcar 
       #'(lambda (res) 
	   (cons
	    '(mlist)
	    (mapcar 
	     #'(lambda (result) 
		 ($ratdisrep (cons (list 'mrat
					 'simp
					 varlist
					 genvar)
				   result)))
	     res)))
       (tmlin '*a2* n m nx)))
     (setq r
	   (list '(mtimes simp)
		 (list '(mexpt simp) (cadar r) -1.)
		 (cons '($matrix simp) (cdr r))))
     (*tmrearray '*a2*)
     (return r))) 

(defun *tmrearray (x) (*rearray x)) 

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

(defun tmpivot-isolate (k) 
  (cond ((> k n) t)
	(t (do ((i k (f1+ i)))
	       ((> i n) nil)
	     (cond ((aref *b* (aref *row* i) (aref *col* k))
		    (tmexchange '*row* k i)
		    (cond ((tmpivot-isolate (f1+ k)) (return t))
			  (t (tmexchange '*row*
					 k
					 i))))))))) 

(defun tmexchange (rowcol i j) 
  (prog (dummy) 
     (setq rowcol (get-array-pointer rowcol))
     (setq dummy (aref rowcol i))
     (store (aref rowcol i) (aref rowcol j))
     (store (aref rowcol j) dummy)))	


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

;;TMPERMUTE PERMUTES THE ROWS OR COLUMNS
;;OF THE N*M MATRIX AX ACCORDING TO THE
;;SPECIFICATION OF INDEXLIST. THE FLAG
;;MUST BE SET 'ROW IF ROW PERMUTATION IS
;;DESIRED , OR 'COL OTHERWISE. THE RESULT
;;IS IN AX. NM IS THE DIMENSION OF
;;INDEXLIST.

(defun tmpermute (ax n m rbias cbias indexlist nm flag) 
  (prog (k l) 
     ;;	     (SETQ AX (GET AX 'array) 
     ;;		   INDEXLIST (GET INDEXLIST 'array))
     (setq ax (get-array-pointer ax))
     (setq indexlist (get-array-pointer indexlist))
     (array *indx* t (f1+ nm))
     (do ((i 1. (f1+ i)))
	 ((> i nm))
       (store (aref *indx* i) (aref indexlist i)))
     (do ((i 1. (f1+ i)))
	 ((> i nm))
       (cond ((not (= (aref *indx* i) i))
	      (prog nil 
		 (tmmove ax n m rbias cbias i 0. flag)
		 (setq l i)
		 loop (setq k (aref *indx* l))
		 (store (aref *indx* l) l)
		 (cond ((= k i)
			(tmmove ax
				n
				m
				rbias
				cbias
				0.
				l
				flag))
		       (t (tmmove ax
				  n
				  m
				  rbias
				  cbias
				  k
				  l
				  flag)
			  (setq l k)
			  (go loop)))))))
     (*tmrearray '*indx*))) 

(defun tmmove (ax n m rbias cbias i j flag) 
  (prog (ll)
     (setq ax (get-array-pointer ax))
     (setq ll (cond ((eq flag '*row*) (f- m cbias))
		    (t (f- n rbias))))
     (do ((k 1. (f1+ k)))
	 ((> k ll))
       (cond ((eq flag '*row*)
	      (store (aref
		      ax
		      (f+ rbias j)
		      (f+ cbias k))
		     (aref
		      ax
		      (f+ rbias i)
		      (f+ cbias k))))
	     (t (store (aref
			ax
			(f+ rbias k)
			(f+ cbias j))
		       (aref
			ax
			(f+ rbias k)
			(f+ cbias i))))))))

;;TMSYMETRICP CHECKS THE SYMETRY OF THE MATRIX.

(defun tmsymetricp        (a3 n)
  (setq a3 (get-array-pointer a3))
  (do ((i 1. (f1+ i)))
      ((> i n) t)
    (cond ((null (do ((j (f1+ i) (f1+ j)))
		     ((> j n) t)
		   (cond ((not (equal (aref
				       a3
				       i
				       j)
				      (aref
				       a3
				       j
				       i)))
			  (return nil)))))
	   (return nil)))))

;;TMLATTICE CHECKS THE "LATTICE"
;;STRUCTURE OF THE MATRIX A. IT RETURNS
;;NIL IF THE MATRIX IS "OBVIOUSLY"
;;SINGULAR. OTHERWISE IT RETURNS A LIST
;;(L1 L2 ... LM) WHERE M IS THE NUMBER OF
;;BLOCKS (STRONGLY CONNECTED SUBGRAPHS),
;;AND L1 L2 ... ARE LIST OF ROW AND
;;COLUMN NUBERS WHICH BELONG TO EACH
;;BLOCKS. THE LIST LOOKS LIKE ((R1 C1)
;;(R2 C2) ...) WHERE R R'S ARE ROWS AND
;;C'S ARE COLUMMS.

(defun tmlattice (a3 xrow xcol n) 
  (prog (res) 
     (setq a3 (get-array-pointer a3))
     (setq xrow (get-array-pointer xrow))
     (setq xcol (get-array-pointer xcol))
     (setq *b* (*array nil t (f1+ n) (f1+ n)))
     (setq *row* (*array nil t (f1+ n)))
     (setq *col* (*array nil  t (f1+ n)))
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (do ((j 1. (f1+ j)))
	   ((> j n))
	 (store (aref *b* i j)
		(not (equal (aref a3 i j)
			    '(0. . 1.))))))
     (do ((i 0. (f1+ i)))
	 ((> i n))
       (store (aref *row* i) i)
       (store (aref *col* i) i))
     (cond ((null (tmpivot-isolate 1.))
	    (setq res nil)
	    (go exit)))
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (store (aref *b* (aref *row* i) (aref *col* i)) i)
       (store (aref *b* (aref *row* i) (aref *col* 0.)) t
	      ))
     (tmlattice1 1.)
     (setq res (tmsort-lattice xrow xcol))
     exit (*tmrearray '*b*)
     (*tmrearray '*row*)
     (*tmrearray '*col*)
     (return res))) 

(defun tmlattice1 (k) 
  (cond ((= k n) nil)
	(t (tmlattice1 (f1+ k))
	   (do ((looppath))
	       (nil)
	     (cond ((setq looppath (tmpathp k k))
		    (tmunify-loop k (cdr looppath)))
		   (t (return nil))))))) 

(defun tmpathp (j k) 
  (cond ((equal (aref *b* (aref *row* j) (aref *col* k)) t) (list j k))
	(t (do ((jj k (f1+ jj)) (path))
	       ((> jj n))
	     (cond ((and (equal (aref *b* (aref *row* j) (aref *col* jj)) t)
			 (setq path (tmpathp jj k)))
		    (return (cons j path)))))))) 

(defun tmunify-loop (k chain) 
  (prog (l dummyk dummyl) 
     (setq l (car chain))
     (cond ((= l k) (return nil)))
     (setq dummyk (aref *b* (aref *row* k) (aref *col* k)))
     (setq dummyl (aref *b* (aref *row* l) (aref *col* l)))
     (store (aref *b* (aref *row* k) (aref *col* k)) nil)
     (store (aref *b* (aref *row* l) (aref *col* l)) nil)
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (store (aref *b* (aref *row* k) (aref *col* i))
	      (or (aref *b* (aref *row* k) (aref *col* i)) (aref *b* (aref *row* l) (aref *col* i))))
       (store (aref *b* (aref *row* i) (aref *col* k))
	      (or (aref *b* (aref *row* i) (aref *col* k)) (aref *b* (aref *row* i) (aref *col* l))))
       (store (aref *b* (aref *row* l) (aref *col* i)) nil)
       (store (aref *b* (aref *row* i) (aref *col* l)) nil))
     (store (aref *b* (aref *row* k) (aref *col* k)) dummyl)
     (store (aref *b* (aref *row* l) (aref *col* l)) dummyk)
     (store (aref *b* (aref *row* k) (aref *col* 0.)) t)
     (store (aref *b* (aref *row* l) (aref *col* 0.)) nil)
     (tmunify-loop k (cdr chain)))) 

(defun tmsort-lattice (xrow xcol) 
  (prog (nodelist result) 
     (setq nodelist (tmsort1))
     (setq 
      result
      (do ((x nodelist (cdr x)) (result))
	  ((null x) result)
	(setq result
	      (cons (do ((next (aref *b* (aref *row* (car x))
				     (aref *col* (car x)))
			       (aref *b* (aref *row* next) (aref *col* next)))
			 (res))
			((= next (car x))
			 (cons (list (aref *row* next) (aref *col* next))
			       res))
		      (setq res
			    (cons (list (aref *row* next)
					(aref *col* next))
				  res)))
		    result))))
     (do ((list1 result (cdr list1)) (i 1.))
	 ((null list1))
       (do ((list2 (car list1) (cdr list2)))
	   ((null list2))
	 (store (aref xrow i) (caar list2))
	 (store (aref xcol i) (cadar list2))
	 (setq i (f1+ i))))
     (return result))) 

;; (DEFUN TMLESS (I J) (B (ROW I) (COL J))) 

(defun tmsort1 nil 
  (do ((i 1. (f1+ i)) (result))
      ((> i n) result)
    (cond ((and (aref *b* (aref *row* i) (aref *col* 0.)) (tmmaxp i))
	   (do ((j 1. (f1+ j)))
	       ((> j n))
	     (cond ((not (= j i))
		    (store (aref *b* (aref *row* i) (aref *col* j)) nil))))
	   (store (aref *b* (aref *row* i) (aref *col* 0.)) nil)
	   (setq result (cons i result))
	   (setq i 0.))))) 

(defun tmmaxp (i) 
  (do ((j 1. (f1+ j)))
      ((> j n) t)
    (cond ((and (not (= i j)) (aref *b* (aref *row* j) (aref *col* i)))
	   (return nil)))))

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

(defun tmunpivot (a3 *row* *col* n m) 
  (prog nil 
     (setq *col* (get-array-pointer *col*))
     (setq *row* (get-array-pointer *row*))
     (setq *rowinv* (*array nil t (f1+ n)))
     (setq *colinv* (*array nil t (f1+ n)))
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (store (aref *rowinv* (aref *row* i)) i))
     (do ((i 1. (f1+ i)))
	 ((> i n))
       (store (aref *colinv* (aref *col* i)) i))
     (tmpermute a3 n m 0. n '*colinv* n '*row*)
     (tmpermute a3 n m 0. n '*rowinv* n '*col*)
     (*tmrearray '*rowinv*)
     (*tmrearray '*colinv*))) 

#-nil
(declare-top(unspecial n  #-cl vlist nx ix))
