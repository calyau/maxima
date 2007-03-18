;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module csimp2)

(load-macsyma-macros rzmac)

(declare-top (special var %p%i varlist plogabs half%pi nn* dn*))

(defmfun simpplog (x vestigial z)
  (declare (ignore vestigial))
  (prog (varlist dd check y)
     (oneargcheck x)
     (setq check x)
     (setq x (simpcheck (cadr x) z))
     (cond ((equal 0 x) (merror "plog(0) is undefined"))
	   ((among var x)	;This is used in DEFINT. 1/19/81. -JIM
	    (return (eqtest (list '(%plog) x) check))))
     (newvar x)
     (cond
       ((and (eq '$%i (car varlist)) (not (some #'atom (cdr varlist))))
	(setq dd (trisplit x))
	(cond ((setq z (patan (car dd) (cdr dd)))
	       (return (add2* (simpln (list '(%log) 
					    (simpexpt (list '(mexpt) ($expand (list '(mplus)
										    (list '(mexpt) (car dd) 2)
										    (list '(mexpt) (cdr dd) 2))) '((rat) 1 2)) 1 nil)) 1 t)
			      (list '(mtimes) z '$%i))))))
       ((and (free x '$%i) (eq ($sign x) '$pnz))
	(return (eqtest (list '(%plog) x) check)))
       ((and (equal ($imagpart x) 0) (setq y ($asksign x)))
	(cond ((eq y '$pos) (return (simpln (list '(%log) x) 1 t)))
	      ((and plogabs (eq y '$neg))
	       (return (simpln (list '(%log) (list '(mtimes) -1 x)) 1 nil)))
	      ((eq y '$neg)
	       (return (add2 %p%i
			     (simpln (list '(%log) (list '(mtimes) -1 x)) 1 nil))))
	      (t (merror "plog(0) is undefined"))))
       ((and (equal ($imagpart (setq z (div* x '$%i))) 0)
	     (setq y ($asksign z)))
	(cond
	  ((equal y '$zero) (merror "plog(0) is undefined"))
	  (t (cond ((eq y '$pos) (setq y 1))
		   ((eq y '$neg) (setq y -1)))
	     (return (add2* (simpln (list '(%log)
					  (list '(mtimes) y z)) 1 nil)
			    (list '(mtimes) y '((rat) 1 2) '$%i '$%pi)))))))
     (return (eqtest (list '(%plog) x) check))))

(defun patan (r i)
  (let (($numer $numer))
    (prog (a b var) 
       (setq i (simplifya i nil) r (simplifya r nil))
       (cond ((zerop1 r)
	      (if (floatp i) (setq $numer t))
	      (setq i ($asksign i))
	      (cond ((equal i '$pos) (return (simplify half%pi)))
		    ((equal i '$neg)
		     (return (mul2 -1 (simplify half%pi))))
		    (t (merror "atan(0//0) has been generated."))))
	     ((zerop1 i)
	      (cond ((floatp r) (setq $numer t)))
	      (setq r ($asksign r))
	      (cond ((equal r '$pos) (return 0))
		    ((equal r '$neg) (return (simplify '$%pi)))
		    (t (merror "atan(0//0) has been generated."))))
	     ((and (among '%cos r) (among '%sin i))
	      (setq var 'xz)
	      (numden (div* r i))
	      (cond ((and (eq (caar nn*) '%cos) (eq (caar dn*) '%sin))
		     (return (cadr nn*))))))
       (setq a ($sign r) b ($sign i))
       (cond ((eq a '$pos) (setq a 1))
	     ((eq a '$neg) (setq a -1))
	     ((eq a '$zero) (setq a 0)))
       (cond ((eq b '$pos) (setq b 1))
	     ((eq b '$neg) (setq b -1))
	     ((eq a '$zero) (setq b 0)))
       (cond ((equal i 0)
	      (return (if (equal a 1) 0 (simplify '$%pi))))
	     ((equal r 0)
	      (return (cond ((equal b 1) (simplify half%pi))
			    (t (mul2 '((rat simp) -1 2)
				     (simplify '$%pi)))))))
       (setq r (simptimes (list '(mtimes) a b (div* i r)) 1 nil))
       (return (cond ((onep1 r)
		      (archk a b (list '(mtimes) '((rat) 1 4) '$%pi)))
		     ((alike1 r '((mexpt) 3 ((rat) 1 2)))
		      (archk a b (list '(mtimes) '((rat) 1 3) '$%pi)))
		     ((alike1 r '((mexpt) 3 ((rat) -1 2)))
		      (archk a b (list '(mtimes) '((rat) 1 6) '$%pi))))))))

(defmfun simpbinocoef (x vestigial z) 
  (declare (ignore vestigial))
  (twoargcheck x)
  (let ((u (simpcheck (cadr x) z))
	(v (simpcheck (caddr x) z))
	(y))
    (cond ((integerp v)
	   (cond ((minusp v)
		  (if (and (integerp u) (minusp u) (< v u)) (bincomp u (- u v)) 0))
		 ((or (zerop v) (equal u v)) 1)
		 ((and (integerp u) (not (minusp u))) (bincomp u (min v (- u v))))
		 (t (bincomp u v))))
	  ((integerp (setq y (sub u v))) (bincomp u y))
	  ((and (floatp u) (floatp v)) ($makegamma (list '(%binomial) u v)))
	  (t (eqtest (list '(%binomial) u v) x)))))

(defun bincomp (u v) 
  (cond ((minusp v) 0)
	((zerop v) 1)
	((mnump u) (binocomp u v))
	(t (muln (bincomp1 u v) nil)))) 

(defun bincomp1 (u v) 
  (if (equal v 1)
      (ncons u)
      (list* u (list '(mexpt) v -1) (bincomp1 (add2 -1 u) (1- v)))))

(defmfun binocomp (u v) 
  (prog (ans) 
     (setq ans 1)
     loop (if (zerop v) (return ans))
     (setq ans (timesk (timesk u ans) (simplify (list '(rat) 1 v))))
     (setq u (addk -1 u) v (1- v))
     (go loop)))

(declare-top (special $numer $gammalim))

(defmvar $beta_args_sum_to_integer nil)

(defmfun simpbeta (x vestigial z &aux check)
  (declare (ignore vestigial))
  (twoargcheck x)
  (setq check x)
  (let ((u (simpcheck (cadr x) z)) (v (simpcheck (caddr x) z)))
    (cond ((or (zerop1 u) (zerop1 v))
	   (if errorsw (throw 'errorsw t) (merror "Zero argument to `beta'")))
	  ((or (and (floatp u) (floatp v))
	       (and $numer (numberp u) (numberp v)))
	   ($makegamma (list '($beta) u v)))
	  ((or (and (integerp u) (plusp u)) (and (integerp v) (plusp v)))
	   (setq x (add2 u v))
	   (power (mul2 (sub x 1)
			(simplifya (list '(%binomial)
					 (sub x 2)
					 (sub (if (and (integerp u) (plusp u)) u v) 1))
				   t))
		  -1))
	  ((and (integerp u) (integerp v))
	   (mul2* (div* (list '(mfactorial) (1- u))
			(list '(mfactorial) (+ u v -1)))
		  (list '(mfactorial) (1- v))))
	  ((or (and (ratnump u) (ratnump v) (integerp (setq x (addk u v))))
	       (and $beta_args_sum_to_integer
		    (integerp (setq x (expand1 (add2 u v) 1 1)))))
	   (let ((w (if (symbolp v) v u)))
	     (div* (mul2* '$%pi
			  (list '(%binomial)
				(add2 (1- x) (neg w))
				(1- x)))
		   `((%sin) ((mtimes) ,w $%pi)))))
	  (t (eqtest (list '($beta) u v) check)))))

(defmfun simpgamma (x vestigial z)
  (declare (ignore vestigial))
  (oneargcheck x)
  (let* ((j (simpcheck (cadr x) z))
	 (jr ($realpart j))
	 (ji ($imagpart j)))
    (cond ((floatp j) (gammafloat j))
	  ((and (numberp jr) 
		(numberp ji)
		(or $numer (floatp jr) (floatp ji)))
	   (complexify (gamma-lanczos (complex (float jr)
					       (float ji)))))
	  ((or (not (mnump j))
	       (ratgreaterp (simpabs (list '(%abs) j) 1 t) $gammalim))
	   (eqtest (list '(%gamma) j) x))
	  ((integerp j)
	   (cond ((> j 0) (simpfact (list '(mfactorial) (1- j)) 1 nil))
		 (errorsw (throw 'errorsw t))
		 (t (merror "gamma(~:M) is undefined" j))))
	  ($numer (gammafloat (fpcofrat j)))
	  ((alike1 j '((rat) 1 2))
	   (list '(mexpt simp) '$%pi j))
	  ((or (ratgreaterp j 1) (ratgreaterp 0 j)) (gammared j))
	  (t (eqtest (list '(%gamma) j) x)))))

(defun gamma (y) ;;; numerical evaluation for 0 < y < 1
  (prog (sum coefs)
     (setq coefs (list 0.035868343d0 -0.193527817d0 0.48219939d0
		       -0.75670407d0 0.91820685d0 -0.89705693d0
		       0.98820588d0 -0.57719165d0))
     (unless (atom y) (setq y (fpcofrat y)))
     (setq sum (car coefs) coefs (cdr coefs))
     loop (setq sum (+ (* sum y) (car coefs)))
     (when (setq coefs (cdr coefs)) (go loop))
     (return (+ (/ y) sum))))

(defun gammared (a)			;A is assumed to
  (prog (m q n)				;be '((RAT) M N)
     (cond ((floatp a) (return (gammafloat a))))
     (setq m (cadr a)			;Numerator
	   n (caddr a)			;denominator
	   q (abs (truncate m n)))		;integer part
     (cond ((minusp m)
	    (setq q (1+ q) m (+ m (* n q)))
	    (return
	      (simptimes (list '(mtimes)
			       (list '(mexpt) n q)
			       (simpgamma (list '(%gamma)
						(list '(rat) m n))
					  1
					  nil)
			       (list '(mexpt) (gammac m n q) -1))
			 1
			 nil))))
     (return (m* (gammac m n q)
		 (simpgamma (list '(%gamma)
				  (list '(rat) (rem m n) n))
			    1 nil)
		 (m^ n (- q))))))

(defun gammac (m n q)
  (do ((ans 1))
      ((< q 1) ans)
    (setq q (1- q) m (- m n) ans (* m ans))))
 

;; This implementation is based on Lanczos convergent formula for the
;; gamma function for Re(z) > 0.  We can use the reflection formula
;;
;;    -z*Gamma(z)*Gamma(-z) = pi/sin(pi*z)
;;
;; to handle the case of Re(z) <= 0.
;;
;; See http://winnie.fit.edu/~gabdo/gamma.m for some matlab code to
;; compute this and http://winnie.fit.edu/~gabdo/gamma.txt for a nice
;; discussion of Lanczos method and an improvement of Lanczos method.
;;
;;
;; The document says this should give about 15 digits of accuracy for
;; double-precision IEEE floats.  The document also indicates how to
;; compute a new set of coefficients if you need more range or
;; accuracy.

(defun gamma-lanczos (z)
  (declare (type (complex double-float) z)
	   (optimize (safety 3)))
  (let ((c (make-array 15 :element-type 'double-float
		       :initial-contents
		       '(0.99999999999999709182d0
			 57.156235665862923517d0
			 -59.597960355475491248d0
			 14.136097974741747174d0
			 -0.49191381609762019978d0
			 .33994649984811888699d-4
			 .46523628927048575665d-4
			 -.98374475304879564677d-4
			 .15808870322491248884d-3
			 -.21026444172410488319d-3
			 .21743961811521264320d-3
			 -.16431810653676389022d-3
			 .84418223983852743293d-4
			 -.26190838401581408670d-4
			 .36899182659531622704d-5))))
    (declare (type (simple-array double-float (15)) c))
    (if (minusp (realpart z))
	;; Use the reflection formula
	;; -z*Gamma(z)*Gamma(-z) = pi/sin(pi*z)
	;; or
	;; Gamma(z) = pi/z/sin(pi*z)/Gamma(-z)
	;;
	;; If z is a negative integer, Gamma(z) is infinity.  Should
	;; we test for this?  Throw an error?  What
	(/ (float pi 1d0)
	   (* (- z) (sin (* (float pi 1d0) z))
	      (gamma-lanczos (- z))))
	(let* ((z (- z 1))
	       (zh (+ z 1/2))
	       (zgh (+ zh 607/128))
	       (zp (expt zgh (/ zh 2)))
	       (ss 
		(do ((sum 0d0)
		     (pp (1- (length c)) (1- pp)))
		    ((< pp 1)
		     sum)
		  (incf sum (/ (aref c pp) (+ z pp))))))
	  (* (sqrt (float (* 2 pi) 1d0))
	     (+ ss (aref c 0))
	     (* zp (exp (- zgh)) zp))))))

(defun gammafloat (a)
  (realpart (gamma-lanczos (complex a 0d0))))

(declare-top (special $numer $trigsign))

(defmfun simperf (x vestigial z &aux y)
  (declare (ignore vestigial))
  (oneargcheck x)
  (setq y (simpcheck (cadr x) z))
  (cond ((zerop1 y) y)
	((or (floatp y) (and $numer (integerp y))) (erf (float y)))
	((eq y '$inf) 1)
	((eq y '$minf) -1)
	((and $trigsign (mminusp* y)) (neg (list '(%erf simp) (neg y))))
	(t (eqtest (list '(%erf) y) x))))


(defmfun erf (y)
  (slatec:derf (float y 1d0)))

(defmfun erfc (y)
  (slatec:derfc (float y 1d0)))

(defmfun $zeromatrix (m n) ($ematrix m n 0 1 1))

(defmfun $ematrix (m n var i j)
  (prog (ans row) 
     (cond ((equal m 0) (return (ncons '($matrix simp))))
	   ((and (equal n 0) (fixnump m) (> m 0))
	    (return (cons '($matrix simp) (list-of-mlists m))))
	   ((not (and (fixnump m) (fixnump n)
		      (fixnump i) (fixnump j)
		      (> m 0) (> n 0) (> i 0) (> j 0)))
	    (merror "Incorrect argument to `ematrix':~%~M"
		    (list '(mlist simp) m n i j) )))
     loop (cond ((= m i) (setq row (onen j n var 0)) (go on))
		((zerop m) (return (cons '($matrix) (mxc ans)))))
     (setq row nil)
     (do ((n n (1- n))) ((zerop n)) (setq row (cons 0 row)))
     on   (setq ans (cons row ans) m (1- m))
     (go loop)))

(defun list-of-mlists (n)
  (do ((n n (1- n))
       (l nil (cons (ncons '(mlist simp)) l)))
      ((= n 0) l)))

(declare-top (special $ratmx))

(defmfun $coefmatrix (eql varl) (coefmatrix eql varl nil))

(defmfun $augcoefmatrix (eql varl) (coefmatrix eql varl t))

(defun coefmatrix (eql varl ind)
  (prog (ans row a b elem)
     (if (not ($listp eql)) (improper-arg-err eql '$coefmatrix))
     (if (not ($listp varl)) (improper-arg-err varl '$coefmatrix))
     (dolist (v (cdr varl))
       (if (and (not (atom v)) (memq (caar v) '(mplus mtimes)))
	   (merror "Improper variable to `coefmatrix':~%~M" v)))
     (setq eql (nreverse (mapcar #'meqhk (cdr eql)))
	   varl (reverse (cdr varl)))
     loop1(if (null eql) (return (cons '($matrix) (mxc ans))))
     (setq a (car eql) eql (cdr eql) row nil)
     (if ind (setq row (cons (const1 a varl) row)))
     (setq b varl)
     loop2(setq elem (ratcoef a (car b)))
     (setq row (cons (if $ratmx elem (ratdisrep elem)) row))
     (if (setq b (cdr b)) (go loop2))
     (setq ans (cons row ans))
     (go loop1)))

(defun const1 (e varl)
  (dolist (v varl) (setq e (maxima-substitute 0 v e))) e)

(defmfun $entermatrix (rows columns)
  (prog (row column vector matrix sym symvector)
     (cond ((or (not (fixnump rows))
		(not (fixnump columns)))
	    (merror "`entermatrix' called with non-integer arguments")))
     (setq row 0)
     (cond ((not (= rows columns)) (setq sym nil) (go oloop)))
     ;; 	QUEST(PRINC "
     ;; Is the matrix  1. Diagonal  2. Symmetric  3. Antisymmetric  4. General
     ;; Answer 1, 2, 3 or 4 : ")	     (SETQ SYM (RETRIEVE NIL NIL))
     quest(princ "
Is the matrix  1. Diagonal  2. Symmetric  3. Antisymmetric  4. General
")	     (setq sym (retrieve "Answer 1, 2, 3 or 4 : " nil))
     (cond ((not (zl-member sym '(1 2 3 4))) (go quest)))
     oloop(cond ((> (incf row) rows)
		 (format t "~%Matrix entered.~%")
		 (return (cons '($matrix) (mxc matrix)))))
     (cond ((equal sym 1)
	    (setq column row)
	    (let ((prompt (format nil "Row ~a Column ~a: " row column)))
	      (setq matrix
		    (nconc matrix
			   (ncons (onen row columns 
					(meval (retrieve prompt nil)) 0)))))
	    (go oloop))
	   ((equal sym 2)
	    (setq column (1- row))
	    (cond ((equal row 1) (go iloop)))
	    (setq symvector 
		  (cons (nthcdr column vector) symvector)
		  vector (nreverse (mapcar 'car symvector))
		  symvector (mapcar 'cdr symvector))
	    (go iloop))
	   ((equal sym 3)
	    (setq column row)
	    (cond ((equal row 1) (setq vector (ncons 0)) (go iloop)))
	    (setq symvector
		  (cons (mapcar #'neg (nthcdr (1- column) vector))
			symvector)
		  vector (nreconc (mapcar 'car symvector) (ncons 0))
		  symvector (mapcar 'cdr symvector))
	    (go iloop)))	 	
     (setq column 0 vector nil)
     iloop(cond ((> (incf column) columns)
		 (setq matrix (nconc matrix (ncons vector)))
		 (go oloop)))
     (let ((prompt (format nil "Row ~a Column ~a: " row column)))
       (setq vector (nconc vector (ncons (meval (retrieve prompt nil))))))
     (go iloop)))

(declare-top (special sn* sd* rsn*))

(defmfun $xthru (e)
  (cond ((atom e) e)
	((mtimesp e) (muln (mapcar '$xthru (cdr e)) nil))
	((mplusp e) (simplify (comdenom (mapcar '$xthru (cdr e)) t)))
	((mexptp e) (power ($xthru (cadr e)) (caddr e)))
	((memq (caar e) '(mequal mlist $matrix))
	 (cons (car e) (mapcar '$xthru (cdr e))))
	(t e))) 

(defun comdenom (l ind) 
  (prog (n d) 
     (prodnumden (car l))
     (setq n (m*l sn*) sn* nil)
     (setq d (m*l sd*) sd* nil)
     loop	(setq l (cdr l))
     (cond ((null l)
	    (return (cond (ind (div* (cond (rsn* ($ratsimp n))
					   (t n))
				     d))
			  (t (list n d))))))
     (prodnumden (car l))
     (setq d (comdenom1 n d (m*l sn*) (m*l sd*)))
     (setq n (car d))
     (setq d (cadr d))
     (go loop)))

(defun prodnumden (e) 
  (cond ((atom e) (prodnd (list e)))
	((eq (caar e) 'mtimes) (prodnd (cdr e)))
	(t (prodnd (list e)))))

(defun prodnd (l) 
  (prog (e) 
     (setq l (reverse l))
     (setq sn* nil sd* nil)
     loop (cond ((null l) (return nil)))
     (setq e (car l))
     (cond ((atom e) (setq sn* (cons e sn*)))
	   ((ratnump e)
	    (cond ((not (equal 1 (cadr e)))
		   (setq sn* (cons (cadr e) sn*))))
	    (setq sd* (cons (caddr e) sd*)))
	   ((and (eq (caar e) 'mexpt)
		 (mnegp (caddr e)))
	    (setq sd* (cons (power (cadr e)
				   (timesk -1 (caddr e)))
			    sd*)))
	   (t (setq sn* (cons e sn*))))
     (setq l (cdr l))
     (go loop)))

(defun comdenom1 (a b c d) 
  (prog (b1 c1) 
     (prodnumden (div* b d))
     (setq b1 (m*l sn*) sn* nil)
     (setq c1 (m*l sd*) sd* nil)
     (return
       (list (add2 (m* a c1) (m* c b1))
	     (mul2 d b1)))))

(declare-top (special $globalsolve $backsubst $dispflag
		      $linsolve_params $%rnum_list ax linelable $linechar 
		      $linenum sol *mosesflag))

(defun xrutout (ax n m varl ind)
  (let (($linsolve_params (and $backsubst $linsolve_params)))
    (prog (ix imin j ans zz m-1 sol tim chk zzz)
       (setq ax (get-array-pointer ax) tim 0)
       (if $linsolve_params (setq $%rnum_list (list '(mlist))))
       (setq imin (min (setq m-1 (1- m)) n))
       (setq ix (max imin (length varl)))
       loop (if (zerop ix) (if ind (go out) (return (cons '(mlist) zz))))
       (when (or (> ix imin) (equal (car (aref ax ix ix)) 0))
	 (setf (aref ax 0 ix)
		(rform (if $linsolve_params (make-param) (ith varl ix))))
	 (if $linsolve_params (go saval) (go next)))
       (setq ans (aref ax ix m))
       (setf (aref ax ix m) nil)
       (do ((j (1+ ix) (1+ j))) ((> j m-1))
	 (setq ans (ratdif ans (rattimes (aref ax ix j) (aref ax 0 j) t)))
	 (setf (aref ax ix j) nil))
       (setf (aref ax 0 ix) (ratquotient ans (aref ax ix ix)))
       (setf (aref ax ix ix) nil)
       (setq ans nil)
       saval (push (cond (*mosesflag (aref ax 0 ix)) 
			 (t (list (if $globalsolve '(msetq) '(mequal))
				  (ith varl ix)
				  (simplify (rdis (aref ax 0 ix))))))
		   zz)
       (if (not $backsubst)
	   (setf (aref ax 0 ix) (rform (ith varl ix))))
       (and $globalsolve (meval (car zz)))
       next (decf ix)
       (go loop)
       out
       (cond ($dispflag (mtell "Solution~%")))
       (setq j 1 sol (list '(mlist)) chk (checklabel $linechar))
       (do ((ll zz (cdr ll)))
	   ((null ll))
	 (setq zzz (car ll))
	 (setq zzz (list '(mlable)
			 (progn
			   (if chk (setq chk nil)
			       (incf $linenum))
			   (let (($nolabels nil))
			     (makelabel $linechar))
			   linelable)
			 (set linelable zzz)))
	 (nconc sol (ncons linelable))
	 (cond ($dispflag (setq tim (get-internal-run-time))
			  (mtell-open "~%~M" zzz)
			  (timeorg tim))
	       (t (putprop linelable t 'nodisp))))
       (return sol))))
