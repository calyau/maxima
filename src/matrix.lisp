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
(macsyma-module matrix)

(declare-top(special errrjfflag oneoff* ei* ej* *ech* *tri* *inv*
		     mdl dosimp $detout vlist mul* top* *det* genvar $ratfac
		     *mosesflag varlist header linind* $scalarmatrixp $sparse
		     $algebraic *rank*) 
	    (*lexpr fmapl1) (fixnum nn len)
	    (genprefix x))

(defmvar $detout nil)
(defmvar top* nil)
(defmvar $ratmx nil)
(defmvar $matrix_element_mult '|&*|)  ;;; Else, most useful when '|&.|
(defmvar $matrix_element_add '|&+|)
(defmvar $matrix_element_transpose nil)

;;provides some of the same spirit of *array but
;;in the value cell. see get-array-pointer below.
(defun cl-*array (nam maclisp-type &rest dimlist)
  (proclaim (list 'special nam))
  (set nam (apply '*array nil maclisp-type  dimlist)))

#+maclisp
(defun get-array-pointer (x)
  (cond ((eq (ml-typep x) 'array) x)
	((get x 'array))
	(t (merror "~S is not an array." x))))

#+franz
(defun get-array-pointer (x)
  (cond ((arrayp x) x)
	((and (symbolp x) (arrayp (getd x))) x)
	(t (merror "~s is not an array." x))))

#+oldlispm
(defun get-array-pointer (x)
  (cond ((arrayp x) x)
	((fboundp x) (symbol-function x))
	(t (error  "~S is not an array." x))))
#+nil ;Defined by maclisp-compatibility array stuff, for just this purpose.
(deff get-array-pointer
    #'si:get-array-pointer)

;;I believe that all the code now stores arrays in the value cell 
(defun get-array-pointer (symbol)
  "There may be nesting of functions and we may well need to apply
   this twice in a row"
  (cond ((arrayp symbol) symbol) (t (symbol-value symbol))))

(defun mxc (x) (mapcar #'(lambda (y) (cons '(mlist) y)) x))
					; Matrix to MACSYMA conversion

(defun mcx (x) (mapcar #'cdr x))	; MACSYMA to Matrix conversion

(defun transpose (m)
  (prog (b nn len)
     (setq len (length (car m)) nn 1)
     loop (cond ((> nn len) (return b))) 
     (setq b (nconc b (ncons (nthcol m nn))) nn (f1+ nn))
     (go loop)))

(defun nthcol (x nn)
  (cond ((or (null x) (> nn (length (car x)))) nil) (t (nthcol1 x nn))))

(defun nthcol1 (x nn)
  (cond ((or (null x) (= nn 0)) nil)
	(t (cons (ith (car x) nn) (nthcol1 (cdr x) nn)))))

(defun check (x) (cond ((atom x) (merror "Not matrix:~%~M" x))
		       ((eq (caar x) '$matrix) x)
		       ((eq (caar x) 'mlist) (list '($matrix) x))
		       (t (merror "Not matrix:~%~M" x)))) 

(defun check1 (x) (cond ((atom x) nil)
			((eq (caar x) '$matrix) x)
			((eq (caar x) 'mlist) (list '($matrix) x)))) 

(defmfun $matrixp (x) (and (not (atom x)) (eq (caar x) '$matrix)))

(defmfun $charpoly (mat var) 
  (setq mat (check mat))
  (if (not (= (length mat) (length (cadr mat))))
      (merror "Matrix must be square - CHARPOLY")) 
  (cond ((not $ratmx) (det1 (addmatrix1
			     (setq mat (mcx (cdr mat))) 
			     (diagmatrix (length mat) (list '(mtimes) -1 var) '$charpoly))))
	(t (newvar var) (newvarmat1 mat)
	   (setq mat (mcx (cdr mat)))
	   (determinant1 (addmatrix mat (diagmatrix (length mat) 
						    (list '(mtimes) -1 var)
						    '$charpoly))))))

(defun disreplist1 (a) (setq header (list 'mrat 'simp varlist genvar))
       (mapcar #'disreplist a))

(defun disreplist (a) (mapcar #'(lambda (e) (cons header e)) a))
 
(defun replist1 (a) (mapcar #'replist a)) 

(defun replist (a) (mapcar #'(lambda (e) (cdr (ratrep* e))) a))


(defun timex (mat1 mat2)
  (cond ((equal mat1 1) mat2)
	((and ($matrixp mat1) ($matrixp mat2) (null (cdr mat1)))
	 (ncons '($matrix simp)))
	(t (newvarmat mat1 mat2)
	   (let (($scalarmatrixp
		  (if (and ($listp mat1) ($listp mat2)) t $scalarmatrixp)))
	     (simplifya (timex0 mat1 mat2) nil)))))

(defun lnewvar (a)
  ((lambda (vlist)
     (lnewvar1 a)
     (setq varlist (nconc (sortgreat vlist) varlist)))
   nil))

(defun lnewvar1 (a)
  (cond ((atom a) (newvar1 a))
	((memq (caar a) '(mlist mequal $matrix)) (mapc #'lnewvar1 (cdr a)))
	(t (newvar1 a))))

(defun newvarmat (mat1 mat2)
  (cond ($ratmx
	 ((lambda (vlist)
	    (lnewvar1 mat1) (lnewvar1 mat2)
	    (setq varlist (nconc (sortgreat vlist) varlist))) nil))))

(defun newvarmat1 (a)
  (cond ($ratmx (lnewvar a))))

(defun addmatrix (x y) (setq x (replist1 x) y (replist1 y))
       (disreplist1 (addmatrix1 x y)))
 
(defun addmatrix1 (b c)
  (cond ((not (and (= (length b) (length c))
		   (= (length (car b)) (length (car c)))))
	 (merror "Attempt to add stuff of unequal length")))
  (mapcar #'addrows b c))
 
(defun addrows (a b)
  (cond ((not $ratmx) (mapcar #'(lambda (i j)
				  (simplus (list '(mplus) i j) 1 nil)) a b))
	(t (mapcar #'ratplus a b)))) 

(defmfun $determinant (mat)
  (cond ((atom mat) (list '(%determinant) mat))
	(t (setq mat (check mat))
	   (if (not (= (length mat) (length (cadr mat))))
	       (merror "DETERMINANT called on a non-square matrix."))
           (cond ((not $ratmx) (det1 (mcx (cdr mat))))
	         (t (newvarmat1 mat) (determinant1 (mcx (cdr mat))))))))

(defun det (m)
  (if (= (length m) 1)
      (caar m)
      (let (*det* mul*)
	(mtoa '*mat* (setq *det* (length m)) *det* m)
	(setq *det* (tfgeli0 '*mat* *det* *det*))
	(ratreduce *det* mul*)))) 
 
(defun determinant1 (x) (catch 'dz (rdis (det (replist1 x))))) 

(defun treedet (mat)
  (prog (row mdl lindex tuplel n id md lt)
     (setq mat (reverse mat))
     (setq n (length mat) md (car mat))
     (setq mat (cdr mat))(setq lindex (nreverse (index* n)) tuplel (mapcar #'list lindex))
     loop1(cond ((null mat) (return (car md))))
     (setq mdl nil)
     (mapcar #'(lambda(a b)
		 (setq mdl(nconc mdl (list a b))))
	     tuplel md)
     (setq md nil)
     (setq row (car mat)mat (cdr mat))
     (setq lt (setq tuplel (nextlevel tuplel lindex)))
     loop2(cond ((null lt) (setq md (nreverse md)) (go loop1)))
     (setq id (car lt) lt (cdr lt)) (setq md (cons (compumd id row) md)) (go loop2) ))

(defun assoo (e l) (prog()
		    loop(cond ((null l) (return nil))
			      ((equal e (car l)) (return (cadr l))))
		    (setq l (cddr l))(go loop)))

(defun compumd (id row)
  (prog(e minor i d sign ans)
     (setq ans 0 sign -1 i id)
     loop(cond ((null i)(return ans))) 
     (setq d (car i) i (cdr i) sign (times -1 sign))
     (cond ((equal (setq e(ith row d)) 0)(go loop))
	   ((equal (setq minor(assoo (zl-delete d(copy id)) mdl)) 0)(go loop)))
     (setq ans (simplus (list '(mplus) ans (simptimes (list '(mtimes) sign e minor) 1 nil)) 1 nil)) (go loop)))

;;Gag me with a vax!  --gsb
;;(DECLARE(SPECIAL LTP*))
;;
;;(DEFUN APDL (L1 L2)
;;  ((LAMBDA (LTP*)
;;     (MAPCAR #'(LAMBDA (J) (SETQ LTP* (CONS (APPEND L1 (LIST J)) LTP*))) L2)
;;     (NREVERSE LTP*))
;;   NIL))
;;
;;(DECLARE(UNSPECIAL LTP*))

(defun apdl (l1 l2)
  (mapcar #'(lambda (j) (append l1 (list j))) l2))

(defun nextlevel (tuplel lindex)
  (prog(ans l li)
   loop (cond ((null tuplel )(return ans)))
   (setq l (car tuplel) tuplel (cdr tuplel) li (cdr (ncdr lindex (car (last l)))))
   (cond ((null li) (go loop)))
   (setq ans(nconc ans (apdl l li))) (go loop)))

(defun det1 (x)
  (cond ($sparse (mtoa '*mat* (length x) (length x) 
		       (mapcar #'(lambda (x) (mapcar #'(lambda (y) (ncons y)) x))x))
		 (sprdet '*mat* (length x)))
	(t (treedet x))))

(defmfun $ident (n) (cons '($matrix) (mxc (diagmatrix n 1 '$ident))))
 
(defmfun $diagmatrix (n var)
  (cons '($matrix) (mxc (diagmatrix n var '$diagmatrix))))

(defun diagmatrix (n var fn)
  (prog (i ans)
     (if (or (not (eq (ml-typep n) 'fixnum)) (minusp n))
	 (improper-arg-err n fn))
     (setq i n)
     loop (if (zerop i) (return ans))
     (setq ans (cons (onen i n var 0) ans) i (f1- i))
     (go loop)))

;; ATOMAT GENERATES A MATRIX FROM A MXN ARRAY BY TAKING COLUMNS S TO N

(defun atomat (name m n s)
  (setq name (get-array-pointer name))
  (prog (j d row mat)
     (setq m (f1+ m) n (f1+ n)) 
     loop1(cond ((= m 1) (return mat)))
     (setq m (f1- m) j n)
     loop2(cond ((= j s) (setq mat (cons row mat) row nil) (go loop1)))
     (setq j (f1- j))
     (setq d (cond (top* (meval (list (list name 'array) m j)))
		   (t (aref name m j))))
     (setq row (cons (or d '(0 . 1)) row))
     (go loop2)))

(defmfun $invertmx (k) 
  (let ((*inv* t) *det* linind* top* mul* ($ratmx t) (ratmx $ratmx) $ratfac
	$sparse)
    (cond ((atom k) ($nounify '$inverx) (list '(%inverx) k))
	  (t (newvarmat1 (setq k (check k)))
	     (setq k (invert1 (replist1 (mcx (cdr k)))))
	     (setq k (cond ($detout `((mtimes)
				      ((mexpt) ,(rdis (or *det* '(1 . 1))) -1)
				      (($matrix) ,@(mxc (disreplist1 k)))))
			   (t (cons '($matrix) (mxc (disreplist1 k))))))
	     (cond ((and ratmx (not $detout))
		    (fmapl1 #'(lambda (x) x) k))
		   ((not ratmx) ($totaldisrep k))
		   (t k))))))

(defun diaginv (ax m)
  (setq ax (get-array-pointer ax))
  (cond ($detout (setq *det* 1)
		 (do ((i 1 (f1+ i))) ((> i m))
		   (setq *det* (plcm *det* (car (aref ax i i)))))
		 (setq *det* (cons *det* 1))))
  (do ((i 1 (f1+ i))(elm))
      ((> i m))
    (setq elm (aref ax i i))
    (store (aref ax i (f+ m i))
	   (cond ($detout (cons (ptimes (cdr elm)
					(pquotient (car *det*) (car elm))) 1))
		 (t (ratinvert elm))))))

(defun invert1 (k) 
  (prog (l r g i m n ei* ej* oneoff*) 
     (setq l (length k) i 1) 
     (cond ((= l (length (car k))) nil)
	   (t(merror "Non-square matrix in inverse")))
     loop (cond ((null k) (go l1))) 
     (setq r (car k)) 
     (setq g (nconc g (list (nconc r (onen i l '(1 . 1) '(0 . 1)))))) 
     (setq k (cdr k) i (f1+ i)) 
     (go loop) 
     l1   (setq k g)
     (mtoa '*mat* (setq m (length k)) (setq n (length (car k))) k)
     (setq k nil)
     (cond ((diagp '*mat* m) (diaginv '*mat* m)) (t (tfgeli0 '*mat* m n)))
     (setq k (atomat '*mat* m n (f1+ m)))
     (*rearray '*mat*)
     (return k)))

(defun diagp (ax m)
  (declare (fixnum m ))
  (prog ((i 0) (j 0))
     (declare (fixnum i j))
     (setq ax (get-array-pointer ax))
     loop1(setq i (f1+ i) j 0)
     (cond((> i m) (return t)))
     loop2(setq j (f1+ j))
     (cond((> j m) (go loop1))
	  ((and (not (= i j))(equal (aref ax i j) '(0 . 1))) nil)
	  ((and(= i j)(not (equal (aref ax i j) '(0 . 1)))) nil)
	  (t(return nil)))
     (go loop2)))

(defun tfgeli0 (x m n) (cond((or $sparse *det*) (tfgeli x m n))
			    (t(tfgeli x m n) (diaglize1 x m n))))

					;  TWO-STEP FRACTION-FREE GAUSSIAN ELIMINATION ROUTINE

(defun ritediv (x m n a)
  (declare(fixnum  m n))
  (setq x (get-array-pointer x))
  (prog ((j 0) (i 0) d errrjfflag)
     (declare(fixnum  i j))
     (setq errrjfflag t)
     (setq i m)
     loop1 (cond ((zerop i) (return nil)))
     (store (aref x i i) nil)
     (setq j m)
     loop (cond ((= j n) (setq i (f1- i)) (go loop1)))
     (setq j (f1+ j))
     (cond ((equal a 1)
	    (store (aref x i j) (cons (aref x i j) 1))
	    (go loop)))
     (setq d (catch 'raterr (pquotient (aref x i j) a)))
     (setq d (cond (d (cons d 1)) (t (ratreduce (aref x i j) a))))
     (store (aref x i j) d)
     (go loop)))

(defun diaglize1 (x m n)
  (setq x (get-array-pointer x))
  (prog nil
     (cond (*det* (return (ptimes *det* (aref x m m)))))
     (setq *det* (cons (aref x m m) 1))
     (cond ((not $detout) (return (ritediv x m n (aref x m m))))
	   (t (return (ritediv x m n 1))))))

;; Takes an M by N matrix and creates an array containing the elements
;; of the matrix.  The array is associated "functionally" with the
;; symbol NAME.
;; For CL we have put it in the value cell-WFS.  Things still work.

(defun mtoa (name m n mat)
  (declare (fixnum m n ))
  #+cl
  (proclaim (list 'special name))
  (set name (*array nil t (f1+ m) (f1+ n)))
  (setq name (get-array-pointer name))
  (do ((i 1 (f1+ i))
       (mat mat (cdr mat)))
      ((> i m) nil)
    (declare(fixnum  i))
    (do ((j 1 (f1+ j))
	 (row (car mat) (cdr row)))
	((> j n))
      (declare(fixnum  j))
      (store (aref name i j) (car row)))))


(defmfun $echelon (x)
  ((lambda ($ratmx) (newvarmat1 (setq x (check x)))) t)
  ((lambda (*ech*)
     (setq x (cons '($matrix) (mxc (disreplist1 (echelon1 (replist1 (mcx (cdr x)))))))))
   t)
  (cond ($ratmx x) (t ($totaldisrep x))))

(defun echelon1 (x)
  ((lambda (m n)
     (mtoa '*mat* m n x)
     (setq x (catch 'rank (tfgeli '*mat* m n)))
     (cond ((and *rank* x)(throw 'rnk x))(t (echelon2 '*mat* m n))))
   (length x) (length (car x))))

(defun echelon2 (name m n)
  (declare (fixnum m n ))
  (setq name (symbol-value name))
  (prog ((j 0) row mat a)
     (declare (fixnum j ))
     (setq m (f1+ m)) 
     loop1(cond ((= m 1) #+maclisp (*rearray name) (return mat)))
     (setq m (f1- m) j 0 a nil)
     loop2(cond ((= j n) (setq mat (cons row mat) row nil) (go loop1)))
     (setq j (f1+ j))
     (setq row (nconc
		row (ncons
		     (cond ((or(> m j)(equal (aref name m j)  0))
			    '(0 . 1))
			   (a (ratreduce (aref name m j)a))
			   (t (setq a (aref name m j)) '(1 . 1))))))
     (go loop2)))

(defun triang (x)
  ((lambda (m n *tri*)
     (mtoa '*mat* m n x) 
     (tfgeli '*mat* m n)
     (triang2 '*mat* m n))
   (length x) (length (car x)) t))

(defun triang2 (nam m n)
  (declare (fixnum m n ))
  (setq nam (get-array-pointer nam))
  (prog ((j 0) row mat)
     (declare (fixnum j))
     (store (aref  nam 0 0) 1)
     (setq m (f1+ m)) 
     loop1(cond ((= m 1) #+maclisp (*rearray nam) (return mat)))
     (setq m (f1- m) j 0)
     loop2(cond ((= j n) (setq mat (cons row mat) row nil) (go loop1)))
     (setq j (f1+ j))
     (setq row (nconc row (ncons
			   (cond ((> m j) '(0 . 1))
				 (t (cons (aref nam m j) 1))))))
     (go loop2)))

(defmfun onen (n i var fill)
  (prog (g)
   loop (cond ((= i n) (setq g (cons var g)))
	      ((zerop i) (return g)) 
	      (t (setq g (cons fill g))))
   (setq i (f1- i))
   (go loop)))

(defun timex0 (x y)
  ((lambda (u v)
     (cond ((and (null u) (null v)) (list '(mtimes) x y))
	   ((null u) (timex1 x (cons '($matrix) (mcx (cdr v)))))
	   ((null v) (timex1 y (cons '($matrix) (mcx (cdr u)))))
	   (t (cons '($matrix mult) (mxc (multiplymatrices (mcx (cdr u)) (mcx (cdr v))))))))
   (check1 x) (check1 y)))
 
(defun timex1 (x y)
  (setq y (check y))
  (cond ((not $ratmx) (setq y (cdr y)))
	(t (setq x (cdr (ratf x)) y (replist1 (cdr y)))))
  (ctimesx x y))

(defun ctimesx (x y)
  (prog (c)
   loop (cond ((null y) 
	       (return (cons '($matrix mult)
			     (mxc (cond ((not $ratmx) c) (t (disreplist1 c)))))))) 
   (setq c (nconc c (list (timesrow x (car y)))) y (cdr y))
   (go loop)))
 
(defun multiplymatrices (x y) 
  (cond ((and (null (cdr y)) (null (cdr x)))
	 (and (cdar x) (setq y (transpose y))))
	((and (null (cdar x)) (null (cdar y)))
	 (and (cdr y) (setq x (transpose x)))))
  (cond ((not (= (length (car x)) (length y)))
	 (cond ((and (null (cdr y)) (= (length (car x)) (length (car y))))
		(setq y (transpose y)))
	       (t (merror "incompatible dimensions - cannot multiply")))))
  (cond ((not $ratmx) (multmat x y))
	(t (setq x (replist1 x) y (replist1 y)) 
	   (disreplist1 (multmat x y))))) 

(defun multmat (x y)
  (prog (mat row yt rowx)
     (setq yt (transpose y))
     loop1(cond ((null x) (return mat)))
     (setq rowx (car x) y yt)
     loop2(cond ((null y)
		 (setq mat (nconc mat (ncons row)) x (cdr x) row nil)
		 (go loop1)))
     (setq row (nconc row (ncons (multl rowx (car y)))) y (cdr y))
     (go loop2)))

;;; This actually takes the inner product of the two vectors.
;;; I check for the most common cases for speed. '|&*| is a slight
;;; violation of data abstraction here. The parser should turn "*" into
;;; MTIMES, well, it may someday, which will break this code. Don't
;;; hold your breath.

(defun multl (a b)
  (cond ((eq $matrix_element_add '|&+|)
	 (do ((ans (if (not $ratmx) 0 '(0 . 1))
		   (cond ((not $ratmx)
			  (cond ((eq $matrix_element_mult '|&*|)
				 (add ans (mul (car a) (car b))))
				((eq $matrix_element_mult '|&.|)
				 (add ans (ncmul (car a) (car b))))
				(t
				 (add ans
				      (meval `((,(getopr $matrix_element_mult))
					       ((mquote simp) ,(car a))
					       ((mquote simp) ,(car b))))))))
			 (t
			  (ratplus ans (rattimes (car a) (car b) t)))))
	      (a a (cdr a))
	      (b b (cdr b)))
	     ((null a) ans)))
	(t
	 (mapply (getopr $matrix_element_add)
		 (mapcar #'(lambda (u v)
			     (meval `((,(getopr $matrix_element_mult))
				      ((mquote simp) ,u)
				      ((mquote simp) ,v))))
			 a b)
		 (getopr $matrix_element_add)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	   
;; I leave this for your historical enjoyment. har har.
;;       (PROG (ANS)
;;	     (SETQ ANS (COND ((NOT $RATMX) 0) (T '(0 . 1))))
;;	LOOP (COND ((NULL A) (RETURN ANS))) 
;;	     (SETQ ANS (COND ((NOT $RATMX)
;;			      (SIMPLUS (LIST '(MPLUS)  ANS  (SIMPTIMES
;;							     (LIST '(MTIMES)
;;								   (CAR A)(CAR B))
;;							     1 T)) 1 T)
;;			      )
;;			     (T (RATPLUS ANS (RATTIMES (CAR A) (CAR B) T)))))
;;	     (SETQ A (CDR A) B (CDR B))
;;	     (GO LOOP))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defmfun bbsort (l fn) (nreverse (sort (copy-top-level l ) fn)))

(defmfun powerx (mat x) 
  (prog (n y) 
     (cond ((not (fixnump x))
	    (return (list '(mncexpt simp) mat x)))
	   ((= x 1) (return mat))
	   ((minusp x)
	    (setq x (minus x) mat ($invertmx mat))
	    (cond ($detout
		   (return (let ((*inv* '$detout))
			     (mul2*
			      (power* (cadr mat) x)
			      (fmapl1 #'(lambda (x) x)
				      (powerx (caddr mat) x)))))))))
     (newvarmat1 (setq mat (check mat)))
     (setq n 1 mat (mcx (cdr mat)) y mat) 
     loop (if (= n x)
	      (let (($scalarmatrixp (if (eq $scalarmatrixp '$all) '$all)))
		(return (simplify (cons '($matrix mult) (mxc y))))))
     (setq y (multiplymatrices y mat) n (f1+ n)) 
     (go loop))) 

;; The following $ALGEBRAIC code is so that 
;; RANK(MATRIX([1-SQRT(5),2],[-2,1+SQRT(5)])); will give 1.
;; - JPG and BMT
 
(defmfun $rank (x)
  (let ((*rank* t) ($ratmx t) ($algebraic $algebraic))
    (newvarmat1 (setq x (check x)))
    (and (not $algebraic) (ormapc #'algp varlist) (setq $algebraic t))
    (setq x (replist1 (mcx (cdr x))))
    (mtoa '*mat* (length x) (length (car x)) x)
    (tfgeli '*mat* (length x) (length (car x)))))

(defun replacerow (i y x)
  (if (= i 1)
      (cons y (cdr x))			;(NCONC (LIST Y) (CDR X))
      (cons (car x) (replacerow (f1- i) y (cdr x)))
					;(NCONC (LIST (CAR X)) (REPLACEROW (f1- I) Y (CDR X)))
      ))
 
(defun timesrow (y row)
  (prog (ans)
     (cond ((and $ratmx (atom y) y) (setq y (cdr (ratf y)))))
     loop (cond ((null row) (return ans)))
     (setq ans (nconc ans (list (cond ((not $ratmx)
				       (simptimes
					(list '(mtimes) y (car row)) 1 nil))
				      (t (rattimes y (car row) t))))))
     (setq row (cdr row))
     (go loop)))
 
(defmfun $triangularize (x) 
  ((lambda ($ratmx) (newvarmat1 (setq x (check x)))) t)
  (setq x (cons '($matrix) (mxc (disreplist1 (triang (replist1 (mcx (cdr x)))))))) 
  (cond ($ratmx x) (t ($totaldisrep x))))

(defmfun $col (mat n)
  (cons '($matrix) (mxc (transpose (list (nthcol (mcx (cdr (check mat))) n)))))) 

(defun deletecol (n x)
  (prog (m g)
     (setq m x)
     loop (cond ((null m) (return g)))
     (setq g (nconc g (ncons (deleterow n (car m)))) m (cdr m))
     (go loop)))
 
(defun deleterow (i m) 
  (cond ((or (null m) (lessp i 0)) (merror "Incorrect index - MATRIX"))
	((= i 1) (cdr m)) 
	(t (cons (car m) (deleterow (f1- i) (cdr m)))))) 
 
(defmfun $minor (mat m n) (cons '($matrix) (mxc (minor m n (mcx (cdr (check mat)))))))
 
(defun minor (i j m) (deletecol j (deleterow i m))) 

(defmfun $row (mat m) (cons '($matrix) (mxc (list (ith (mcx (cdr (check mat))) m)))))

(defmfun $setelmx (elm m n mat) 
  (cond ((not (and (integerp m) (integerp n) ($matrixp mat)))
	 (merror "Wrong arg to SETELMX"))
	((not (and (> m 0) (> n 0) (> (length mat) m) (> (length (cadr mat)) n)))
	 (merror "No such entry - SETELMX")))
  (rplaca (ncdr (car (ncdr mat (f1+ m))) (f1+ n)) elm) mat) 
 
;;; Here the function transpose can actually do simplification of
;;; its argument. TRANSPOSE(TRANSPOSE(FOO)) => FOO.
;;; If you think this is a hack, well, realize that the hack is
;;; actually the fact that TRANSPOSE can return a noun form.

(defmfun $transpose (mat)
  (cond ((not (mxorlistp mat))
	 (cond ((and (not (atom mat))
		     (eq (caar mat) '%transpose))
		(cadr mat))
	       (($scalarp mat) mat)
	       ((mplusp mat)
		`((mplus) .,(mapcar #'$transpose (cdr mat))))
	       ((mtimesp mat)
		`((mtimes) .,(mapcar #'$transpose (cdr mat))))
	       ((mnctimesp mat)
		`((mnctimes) .,(nreverse (mapcar #'$transpose (cdr mat)))))
	       ((mncexptp mat)
		(destructuring-let (((mat pow) (cdr mat)))
		  `((mncexpt) ,($transpose mat) ,pow)))
	       
	       (t ($nounify '$transpose) (list '(%transpose) mat))))
	(t
	 (let ((ans (transpose (mcx (cdr (check mat))))))
	   (cond ($matrix_element_transpose
		  (setq ans (mapcar #'(lambda (u)
					(mapcar #'transpose-els
						u))
				    ans))))
	   `(($matrix) . ,(mxc ans))))))

;;; THIS IS FOR TRANSPOSING THE ELEMENTS OF A MATRIX
;;; A hack for Block matricies and tensors.

(defun transpose-els (elem)
  (cond ((eq $matrix_element_transpose '$transpose)
	 ($transpose elem))
	((eq $matrix_element_transpose '$nonscalars)
	 (cond (($nonscalarp elem)
		($transpose elem))
	       (t elem)))
	(t
	 (meval `((,(getopr $matrix_element_transpose)) ((mquote simp) ,elem))))))


(defmfun $submatrix nargs
  (prog (r c x)
     (setq x (listify nargs))
     l1   (cond ((numberp (car x)) (setq r (cons (car x) r) x (cdr x)) (go l1)))
     (setq c (nreverse (bbsort (cdr x) '>)) r (nreverse (bbsort r '>)))
     (setq x (mcx (cdar x)))
     l2   (cond ((null r) (go b)) (t (setq x (deleterow (car r) x))))
     (setq r (cdr r))
     (go l2)
     b    (cond ((null c) (return (cons '($matrix) (mxc x)))))
     (setq x (deletecol (car c) x) c (cdr c))
     (go b)))


(defun $list_matrix_entries (m)
  (or ($matrixp m) (error "not a matrix"))
  (cons (if (null (cdr m)) '(mlist) (caadr m))
	(loop for row in (cdr m) append (cdr row))))

;; Undeclarations for the file:
#-nil
(declare-top(notype nn len))
