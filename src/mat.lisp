;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module mat)

(comment this is the mat package)

(declare-top(special pivsign* *ech* *tri* lsolveflag $algebraic
		     $multiplicities equations
		     mul* formatform dosimp $dispflag $ratfac
		     *tb $nolabels errrjfflag *det* genvar
		     xm* xn* varlist ax linelable $linechar $linenum sol)
	    (*lexpr $solve $rat)
	    (array* (notype xa* 2))
	    (fixnum tim)
	    (genprefix mat))

;;these are arrays.
(declare-top(special *row* *col* *colinv*))
;; The array declarations of ROW, COL, and COLINV aren't having any
;; effect on the Lisp Machine.  Should be fixed somehow.

;;(DECLARE (ARRAY* (FIXNUM *ROW* 1 *COL* 1 *COLINV* 1)))

(defmvar $globalsolve nil)
(defmvar $sparse nil)
(defmvar $backsubst t)

(defmvar *rank* nil)
(defmvar *inv* nil)
(defvar solvexp nil)

(defun solcoef (m *c varl flag)
  (prog (cc answer leftover)
     (setq cc (cdr (ratrep* *c)))
     (if (or (atom (car cc))
	     (not (equal (cdar cc) '(1 1)))
	     (not (equal 1 (cdr cc))))
	 (merror "Unacceptable variable to `solve':~%~M" *c))
     (setq answer (ratreduce (prodcoef (car cc) (car m)) (cdr m)))
     (if (not flag) (return answer))
     (setq leftover
	   (rdis (ratplus m (rattimes (ratminus answer) cc t))))
     (if (or (not (freeof *c leftover))
	     (dependsall (rdis answer) varl))
	 (errrjf "`non-linear'"))
     (return answer)))

(defun formx (flag nam eql varl)
  (prog (b ax x ix j)
     (setq varlist varl)
     (mapc #'newvar eql)
     (and (not $algebraic)
	  (ormapc #'algp varlist) 
	  (setq $algebraic t))
     (set nam (*array nil t (f1+ (setq xn* (length eql)))
		      (f1+ (setq xm* (f1+ (length varl))))))
     (setq nam (get-array-pointer nam))
     (setq ix 0)
     loop1
     (cond ((null eql) (return  varlist)))
     (setq ax (car eql))
     (setq eql (cdr eql))
     (setq ix (f1+ ix))
     (store (aref nam ix xm*) (const ax varl))
     (setq j 0)
     (setq b varl) (setq ax (cdr (ratrep* ax)))
     loop2
     (setq x (car b))
     (setq b (cdr b))
     (setq j (f1+ j))
     (store (aref nam ix j) (solcoef ax x varl flag))
     (cond (b (go loop2)))
     (go loop1)))

(defun dependsall (exp l)
  (cond ((null l) nil)
	((or (not (freeof (car l) exp)) (dependsall exp (cdr l))) t)
	(t nil)))

(setq *det* nil *ech* nil *tri* nil)

(defun ptorat (ax m n)
  (prog (i j)
     (setq ax (get-array-pointer ax))
     (setq i (f1+ m) n (f1+ n)) 
     loop1
     (cond ((equal i 1) (return nil)))
     (setq i (f1- i) j n)
     loop2
     (cond ((equal j 1) (go loop1)))
     (setq j (f1- j))
     (store (aref ax i j) (cons (aref ax i j) 1))
     (go loop2)))

(defun meqhk (z)
  (cond ((and (not (atom z)) (eq (caar z) 'mequal))
	 (simplus (list '(mplus) (cadr z) (list '(mtimes) -1 (caddr z))) 1 nil))
	(t z)))

(defun const (e varl)
  (prog (zl)
     (setq varl (mapcar (function (lambda(x) (caadr (ratrep* x)))) varl))
     (setq e (cdr(ratrep* e)))
     (setq zl (nzeros (length varl) nil))
     (return (ratreduce (pctimes -1 (pcsubsty zl varl (car e)))
			(pcsubsty zl varl (cdr e))))))



(defvar *mosesflag nil)

(defmvar $%rnum 0)

(defmfun make-param ()
  (let ((param (concat '$%r (setq $%rnum (f1+ $%rnum)))))
    (tuchus $%rnum_list param)
    param))

(defmvar $linsolve_params t "`linsolve' generates %Rnums")

;;(DECLARE (FIXNUM N))

(defun ncdr (x n)
  (nthcdr (f1- n) x))

(defun ith (x n) (cond ((atom x) nil) (t (car (ncdr x n)))))

;;(DECLARE (NOTYPE N))

(defun polyize (ax r m mul)
  (declare (fixnum m))
  (do ((c 1 (f1+ c)) (d))
      ((> c m) nil)
    (declare (fixnum c))
    (setq d (aref ax r c))
    (setq d (cond ((equal mul 1) (car d))
		  (t (ptimes (car d)
			     (pquotientchk mul (cdr d))))))
    (store (aref ax r c) (if $sparse (cons d 1) d))))

;; TWO-STEP FRACTION-FREE GAUSSIAN ELIMINATION ROUTINE

(defun tfgeli (ax n m &aux ($sparse (and $sparse (or *det* *inv*))))
  ;;$sparse is also controlling whether polyize stores polys or ratforms
  (setq ax (get-array-pointer ax))
  (setq mul* 1)
  (do ((r 1 (f1+ r)))
      ((> r n) (cond ((and $sparse *det*)(sprdet ax n))
		     ((and *inv* $sparse)(newinv ax n m))
		     (t (tfgeli1 ax n m))))
    (do ((c 1 (f1+ c))
	 (d)
	 (mul 1))
	((> c m)
	 (and *det* (setq mul* (ptimes mul* mul)))
	 (polyize ax r m mul))
      (cond ((equal 1 (setq d (cdr (aref ax r c)))) nil)
	    (t (setq mul (ptimes mul (pquotient d (pgcd mul d)))))))))

(setq lsolveflag nil)

;; The author of the following programs is Tadatoshi Minamikawa (TM). 
;; This program is one-step fraction-free Gaussian elimination with
;; optimal pivotting.  DRB claims the hair in this program is not
;; necessary and that straightforward Gaussian elimination is sufficient,
;; for sake of future implementors.

;; To debug, delete the comments around PRINT and BREAK statements.

(declare-top(special permsign a rank delta nrow nvar n m variableorder
		     dependentrows inconsistentrows l k)
	    ;; We could just use fortran, you know.
	    (fixnum nrow nvar rank i j k l m n))

(defun tfgeli1 (ax n m)
  (prog (k l delta variableorder inconsistentrows
	 dependentrows nrow nvar rank permsign result)
     (#-cl *array #+cl cl-*array '*row* 'fixnum (f1+ n)) 
     (#-cl *array #+cl cl-*array '*col* 'fixnum (f1+ m))
     (#-cl *array #+cl cl-*array '*colinv* 'fixnum (f1+ m))
     ;;	#+LISPM (FILLARRAY (FUNCTION ROW) '(0)) ;implicit in *array
     ;;	#+LISPM (FILLARRAY (FUNCTION COL) '(0))
     ;;	#+LISPM (FILLARRAY (FUNCTION COLINV) '(0))
     (setq ax (get-array-pointer ax))
     (setq *col* (get-array-pointer *col*))
     (setq *row* (get-array-pointer *row*))
     (setq *colinv* (get-array-pointer *colinv*))
     ;; (PRINT 'ONESTEP-LIPSON-WITH-PIVOTTING)
     (setq nrow n)
     (setq nvar (cond (*rank* m) (*det* m) (*inv* n) (*ech* m) (*tri* m) (t (f1- m))))
     (do ((i 1 (f1+ i))) ((> i n)) (store (aref *row* i) i))
     (do ((i 1 (f1+ i))) ((> i m))
       (store (aref *col* i) i) (store (aref *colinv* i) i))
     (setq result
	   (cond 
	     (*rank* (forward t) rank)
	     (*det* (forward t)
		    (cond ((= nrow n) (cond (permsign  (pminus delta))
					    (t delta)))
			  (t 0)))
	     (*inv* (forward t) (backward) (recoverorder1))
	     (*ech* (forward nil) (recoverorder2))
	     (*tri* (forward nil) (recoverorder2))
	     (t (forward t) (cond ($backsubst (backward)))
		(recoverorder2)
		(list dependentrows  inconsistentrows variableorder))))
     (*rearray '*row*) (*rearray '*col*) (*rearray '*colinv*)
     (return result)))

;;FORWARD ELIMINATION
;;IF THE SWITCH *CPIVOT IS NIL, IT AVOIDS THE COLUMN PIVOTTING.
(defun forward (*cpivot)
  (setq delta 1)		  ;DELTA HOLDS THE CURRENT DETERMINANT
  (do ((k 1 (f1+ k))
       (nvar nvar)   ;PROTECTS AGAINST TEMPORARAY RESETS DONE IN PIVOT
       (m m))
      ((or (> k nrow) (> k nvar)))
    (cond ((pivot ax k *cpivot) (return nil)))
    ;; PIVOT IS T IF THERE IS NO MORE NON-ZERO ROW LEFT. THEN GET OUT OF THE LOOP
    (do ((i (f1+ k) (f1+ i)))
	((> i nrow))
      (do ((j (f1+ k) (f1+ j)))
	  ((> j m))
	(store ( aref ax (aref *row* i) (aref *col* j))
	       (pquotient (pdifference (ptimes ( aref ax (aref *row* k) (aref *col* k))
					       (aref ax (aref *row* i) (aref *col* j)))
				       (ptimes ( aref ax (aref *row* i) (aref *col* k))
					       ( aref ax (aref *row* k) (aref *col* j))))
			  delta))))
    (do ((i (f1+ k) (f1+ i)))
        ((> i nrow))
      (store ( aref ax (aref *row* i) (aref *col* k))
	     0))
    (setq delta ( aref ax (aref *row* k) (aref *col* k))))
  ;; UNDOES COLUMN HACK IN PIVOT.
  (or *cpivot (do ((i 1 (f1+ i))) ((> i m)) (store (aref *col* i) i)))
  (setq rank (min nrow nvar)))

;; BACKWARD SUBSTITUTION
(defun backward ()
  (do ((i (f1- rank) (f1- i)))
      ((< i 1))
    (do ((l (f1+ rank) (f1+ l)))
	((> l m))
      (store (aref ax (aref *row* i) (aref *col* l))
	     (pquotient (pdifference
			 (ptimes (aref ax (aref *row* i) (aref *col* l))
				 (aref ax (aref *row* rank) (aref *col* rank)))
			 (do ((j (f1+ i) (f1+ j)) (sum 0))
			     ((> j rank) sum)
			   (setq sum (pplus sum (ptimes (aref ax (aref *row* i) (aref *col* j))
							(aref ax (aref *row* j) (aref *col* l)))))))
			(aref ax (aref *row* i) (aref *col* i)))))
    (do ((l (f1+ i) (f1+ l)))
	((> l rank))
      (store (aref ax (aref *row* i) (aref *col* l)) 0)))
  ;; PUT DELTA INTO THE DIAGONAL MATRIX
  (setq delta (aref ax (aref *row* rank) (aref *col* rank)))
  (do ((i 1 (f1+ i)))
      ((> i rank))
    (store (aref ax (aref *row* i) (aref *col* i)) delta)))

;;RECOVER THE ORDER OF ROWS AND COLUMNS.

(defun recoverorder1 ()
  ;;(PRINT 'REARRANGE)
  (do ((i nvar (f1- i)))
      ((= i 0))
    (setq variableorder (cons i variableorder)))
  (do ((i (f1+ rank) (f1+ i)))
      ((> i n))
    (cond ((equal (aref ax (aref *row* i) (aref *col* m)) 0) 
	   (setq dependentrows (cons (aref *row* i) dependentrows)))
	  (t (setq inconsistentrows (cons (aref *row* i) inconsistentrows)))))
  (do ((i 1 (f1+ i)))
      ((> i n))
    (cond ((not (= (aref *row* (aref *colinv* i)) i))
	   (prog ()
	      (moverow ax n m i 0)
	      (setq l i)
	      loop
	      (setq k (aref *row* (aref *colinv* l)))
	      (store (aref *row* (aref *colinv* l)) l)
	      (cond ((= k i) (moverow ax n m 0 l))
		    (t (moverow ax n m k l)
		       (setq l k)
		       (go loop))))))))

(defun recoverorder2 ()
  (do ((i nvar (f1- i)))
      ((= i 0))
    (setq variableorder (cons (aref *col* i) variableorder)))
  (do ((i (f1+ rank) (f1+ i)))
      ((> i n))
    (cond ((equal (aref ax (aref *row* i) (aref *col* m)) 0)
	   (setq dependentrows (cons (aref *row* i) dependentrows)))
	  (t (setq inconsistentrows (cons (aref *row* i) inconsistentrows)))))
  (do ((i 1 (f1+ i)))
      ((> i n))
    (cond ((not (= (aref *row* i) i))
	   (prog ()
	      (moverow ax n m i 0)
	      (setq l i)
	      loop
	      (setq k (aref *row* l))
	      (store (aref *row* l) l)
	      (cond ((= k i) (moverow ax n m 0 l))
		    (t (moverow ax n m k l)
		       (setq l k)
		       (go loop)))))))
  (do ((i 1 (f1+ i)))
      ((> i nvar))
    (cond ((not (= (aref *col* i) i))
	   (prog ()
	      (movecol ax n m i 0)
	      (setq l i)
	      loop2
	      (setq k (aref *col* l))
	      (store (aref *col* l) l)
	      (cond ((= k i) (movecol ax n m 0 l))
		    (t (movecol ax n m k l)
		       (setq l k)
		       (go loop2))))))))

;;THIS PROGRAM IS USED IN REARRANGEMENT
(defun moverow (ax n m i j)
  (do ((k 1 (f1+ k))) ((> k m))
    (store (aref ax j k) (aref ax i k))))

(defun movecol (ax n m i j)
  (do ((k 1 (f1+ k))) ((> k n))
    (store (aref ax k j) (aref ax k i))))

;;COMPLEXITY IS DEFINED AS FOLLOWS
;; COMPLEXITY(0)=0
;; COMPLEXITY(CONSTANT)=1
;; COMPLEXITY(POLYNOMIAL)=1+SUM(COMPLEXITY(C(N))+COMPLEXITY(E(N)), FOR N=0,1 ...M)
;; WHERE POLYNOMIAL IS OF THE FORM
;;    SUM(C(N)*X^E(N), FOR N=0,1 ... M)     X IS THE VARIABLE

(defun complexity (exp)
  (cond ((null exp) 0)
	((equal exp 0) 0)
	((atom  exp) 1)
	(t (plus (complexity (car exp)) (complexity (cdr exp))))))

(defun complexity/row (ax i j1 j2)
  (do ((j j1 (f1+ j)) (sum 0))
      ((> j j2) sum)
    (setq sum (plus sum (complexity (aref ax (aref *row* i) (aref *col* j)))))))

(defun complexity/col (ax j i1 i2)
  (do ((i i1 (f1+ i)) (sum 0))
      ((> i i2) sum)
    (setq sum (plus sum (complexity (aref ax (aref *row* i) (aref *col* j)))))))

(defun zerop/row (ax i j1 j2)
  (do ((j j1 (f1+ j)))
      ((> j j2) t)
    (cond ((not (equal (aref ax (aref *row* i) (aref *col* j)) 0)) (return nil)))))

;;PIVOTTING ALGORITHM
(defun pivot (ax k *cpivot)
  (prog ( row/optimal col/optimal complexity/i/min complexity/j/min
	 complexity/i complexity/j complexity/det complexity/det/min dummy)
     (setq row/optimal k complexity/i/min 1000000. complexity/j/min 1000000.)
     ;;TEST THE SINGULARITY
     (cond ((do ((i k (f1+ i)) (isallzero t))
		((> i nrow) isallzero)
	     loop (cond ((zerop/row ax i k nvar)
			 (cond (*inv* (merror "Singular"))
			       (t (exchangerow i nrow)
				  (setq nrow (f1- nrow))))
			 (cond ((not (> i nrow)) (go loop))))
			(t (setq isallzero nil))))
	    (return t)))

     ;; FIND AN OPTIMAL ROW
     ;; IF *CPIVOT IS NIL, (AX I K) WHICH IS TO BE THE PIVOT MUST BE NONZERO.
     ;; BUT IF *CPIVOT IS T, IT IS UNNECESSARY BECAUSE WE CAN DO THE COLUMN PIVOT.
     findrow
     (do ((i k (f1+ i)))
	 ((> i nrow))
       (cond ((or *cpivot (not (equal (aref ax (aref *row* i) (aref *col* k)) 0)))
	      (cond ((> complexity/i/min
			(setq complexity/i (complexity/row ax i k m)))
		     (setq row/optimal i complexity/i/min complexity/i))))))
     ;; EXCHANGE THE ROWS K AND ROW/OPTIMAL
     (exchangerow k row/optimal)

     ;; IF THE FLAG *CPIVOT IS NIL, THE FOLLOWING STEPS ARE NOT EXECUTED.
     ;; THIS TREATMENT WAS DONE FOR THE LSA AND ECHELONTHINGS WHICH ARE NOT
     ;; HAPPY WITH THE COLUMN OPERATIONS.
     (cond ((null *cpivot)
	    (cond ((not (equal (aref ax (aref *row* k) (aref *col* k)) 0))
		   (return nil))
		  (t (do ((i k (f1+ i))) ((= i nvar))
		       (store (aref *col* i) (aref *col* (f1+ i))))
		     (setq nvar (f1- nvar) m (f1- m))
		     (go findrow)))))

     ;;STEP3 ... FIND THE OPTIMAL COLUMN
     (setq col/optimal 0
	   complexity/det/min 1000000.
	   complexity/j/min 1000000.)

     (do ((j k (f1+ j)))
	 ((> j nvar))
       (cond ((not (equal (aref ax (aref *row* k) (aref *col* j)) 0))
	      (cond ((> complexity/det/min
			(setq complexity/det
			      (complexity (aref ax (aref *row* k) (aref *col* j)))))
		     (setq col/optimal j
			   complexity/det/min complexity/det
			   complexity/j/min (complexity/col ax j (f1+ k) n)))
		    ((equal complexity/det/min complexity/det)
		     (cond ((> complexity/j/min
			       (setq complexity/j
				     (complexity/col ax j (f1+ k) n)))
			    (setq col/optimal j
				  complexity/det/min complexity/det
				  complexity/j/min complexity/j))))))))

					;(COND ((ZEROP COL/OPTIMAL) (COMMENT (PRINT '"SINGULAR!"))))

     ;; EXCHANGE THE COLUMNS K AND COL/OPTIMAL
     (exchangecol  k col/optimal)
     (setq dummy (aref *colinv* (aref *col* k)))
     (store (aref *colinv* (aref *col* k)) (aref *colinv* (aref *col* col/optimal)))
     (store (aref *colinv* (aref *col* col/optimal)) dummy)
     (return nil)))

(defun exchangerow (i j)
  (prog (dummy)
     (setq dummy (aref *row* i))
     (store (aref *row* i) (aref *row* j))
     (store (aref *row* j) dummy) 
     (cond ((= i j) (return nil))
	   (t (setq permsign (not permsign))))))

(defun exchangecol (i j)
  (prog (dummy)
     (setq dummy (aref *col* i))
     (store (aref *col* i) (aref *col* j))
     (store (aref *col* j) dummy)
     (cond ((= i j) (return nil))
	   (t (setq permsign (not permsign))))))

;; Displays list of solutions.

(defun solve2 (llist)
  (setq $multiplicities nil)
  (map2c #'(lambda (equatn multipl) 
	     (setq equations
		   (nconc equations (list (displine equatn))))
	     (push multipl $multiplicities)
	     (if (and (> multipl 1) $dispflag)
		 (mtell "Multiplicity ~A~%" multipl)))
	 llist)
  (setq $multiplicities (cons '(mlist simp) (nreverse $multiplicities))))

;; Displays an expression and returns its linelabel.

(defmfun displine (exp)
  (let ($nolabels (tim 0)) 
    (elabel exp)
    (cond ($dispflag (remprop linelable 'nodisp) 
		     (setq tim (runtime))
		     (mterpri)
		     (displa (list '(mlable) linelable exp))
		     (timeorg tim))
	  (t (putprop linelable t 'nodisp)))
    linelable))
