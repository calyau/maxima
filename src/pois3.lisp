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
(macsyma-module pois3)

;; GENERAL POISSON SERIES

(declare-top (special *argc *coef poisvals poisco1 poiscom1 b* a* *a ss cc h* poishift
		      poistsm poissiz poists $wtlvl $poisz $pois1)
	     (*lexpr $print $coeff)
	     (genprefix \P)) 

(defvar trim nil)

(defun fumcheck (x)
  (not (and (atom x) (integerp x) (lessp (abs x) poistsm))))

(defun checkencode(r)
  (prog(q)
     (setq q ($coeff r '$u))
     (cond ((fumcheck q) (return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$u q)) nil))))
     (setq q ($coeff r '$v))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$v q)) nil))))
     (setq q ($coeff r '$w))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$w q)) nil))))
     (setq q ($coeff r '$x))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$x q)) nil))))
     (setq q ($coeff r '$y))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$y q)) nil))))
     (setq q ($coeff r '$z))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$z q)) nil))))
     (cond ((equal r 0)(return t))
	   (t (return nil)))))

(defmfun $poissimp (x)
  (if (mbagp x) (cons (car x) (mapcar #'$poissimp (cdr x))) ($outofpois x)))

;;;********

(declare-top(fixnum ae poishift poistsm poissiz poists))
;; ABOVE ASSUMES POISLIM(5) OR LESS ALSO REDEFINE ORDER< AND ORDER= TO BE < AND =

;;; THIS TELLS THE EVALUATOR TO KEEP OUT OF POISSON $SERIES.

(defprop mpois (lambda (x) x) mfexpr*) 

(defmfun $poisplus (a b) 
  (setq a (intopois a) b (intopois b))
  (list '(mpois simp) (poismerge22 (cadr a) (cadr b)) (poismerge22 (caddr a) (caddr b)))) 



(declare-top (special *b *fn)) 
(defmfun $poismap (p sinfn cosfn) 
  (prog (*b *fn) 
     (setq p (intopois p))
     (setq *fn (list sinfn))
     (return (list (car p)
		   (poismap (cadr p))
		   (prog2 (setq *fn (list cosfn)) (poismap (caddr p))))))) 

(defun poismap (y) 
  (cond ((null y) nil)
	(t (setq *b (meval (list *fn (poiscdecode (cadr y)) (poisdecodec (car y)))))
	   (tcons3(car y) (intopoisco  *b) (poismap (cddr y))))))

(defun poismerge22 (r s) 
  (cond ((null r) s)
	((null s) r)
	((equal (car r) (car s))
	 (prog (tt) 
	    (setq tt (poisco+ (cadr r) (cadr s)))
	    (return (cond ((poispzero tt) (poismerge22 (cddr r) (cddr s)))
			  (t (cons (car s) (cons tt (poismerge22 (cddr r) (cddr s)))))))))
	((lessp (car r) (car s)) (cons (car r) (cons (cadr r) (poismerge22 (cddr r) s))))
	(t (cons (car s) (cons (cadr s) (poismerge22 (cddr s) r)))))) 

(defun poiscosine (m) 
  (setq m (poisencode m))
  (cond ((poisnegpred m) (setq m (poischangesign m))))
  (list '(mpois simp) nil (list m poisco1))) 

(defun poissine (m) 
  (setq m (poisencode m))
  (cond ((poisnegpred m) (list '(mpois simp) (list (poischangesign m) poiscom1) nil))
	(t (list '(mpois simp) (list m poisco1) nil)))) 

(defmfun $intopois (x) (prog (*a) (return (intopois x)))) 

(defun intopois (a) 
  (cond ((atom a)
	 (cond ((equal a 0.) $poisz) (t (list '(mpois simp) nil (list poishift (intopoisco a))))))
	((eq (caar a) 'mpois) a)
	((eq (caar a) '%sin) (poissine (cadr a)))
	((eq (caar a) '%cos) (poiscosine (cadr a)))
	((and (eq (caar a) 'mexpt) (numberp (caddr a)) (greaterp (caddr a) 0.))
	 ($poisexpt (intopois (cadr a)) (caddr a)))
	((eq (caar a) 'mplus)
	 (setq *a (intopois (cadr a)))
	 (mapc (function (lambda (z) (setq *a ($poisplus *a (intopois z))))) (cddr a))
	 *a)
	((eq (caar a) 'mtimes)
	 (setq *a (intopois (cadr a)))
	 (mapc (function (lambda (z) (setq *a ($poistimes *a (intopois z))))) (cddr a))
	 *a)
	((eq (caar a) 'mrat) (intopois (ratdisrep a)))
	(t (list '(mpois simp) nil (list poishift (intopoisco a)))))) 

(defun tcons (r s) (cond ((poispzero (car s)) (cdr s)) (t (cons r s)))) 

(defun poisnegpred ($n) 
  (prog ($r) 
   $loop(cond ((equal $n 0.) (return nil)) (t nil))
   (setq $r (difference (remainder $n poists) poistsm))
   (cond ((greaterp $r 0.) (return nil))
	 ((greaterp 0. $r) (return t))
	 (t (setq $n (quotient $n poists))))
   (go $loop))) 

(defun poischangesign ($n) (difference (times poishift 2.) $n)) 

(declare-top (special $u $v $w $x $y $z)) 

(defun poisencode (h*)			;
  (cond ((not (checkencode h*))
	 (merror "Illegal arg to POISSIMP:~%~M" h*)))
  (apply #'(lambda ($z $y $x $w $v $u)
	     (declare (special $u $v $w $x $y $z)) 
	     (setq h* (meval h*))
	     (cond ((not (integerp h*)) (merror  "Illegal trig arg to POISSON form")))
	     (plus poishift  h*))
	 poisvals))

(prog (n)
   (setq n 5)
   (setq poisvals nil)
   (setq poists (expt 2. n))
   (do ((j 0. (f1+ j)))( (> j 5.)) (setq poisvals (cons (expt poists j) poisvals)))
   (setq poissiz n 
	 poistsm (expt 2. (sub1 n)) 
	 poishift (prog (sum) 
		     (setq sum 0.)
		     (do ((i 0. (f1+ i)))( (> i 5.)) (setq sum (plus sum (times poistsm (expt poists i)))))
		     (return sum))
	 $poisz '((mpois simp) nil nil) 
	 $pois1 (list '(mpois simp) nil (list poishift 1.)))
   n) 

(defun poisdecodec (m) 
  (prog (arg h) 
     (setq h m)
     (setq arg (list '(mtimes) (difference (remainder h poists) poistsm) '$u))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (difference (remainder h poists) poistsm) '$v)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (difference (remainder h poists) poistsm) '$w)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (difference (remainder h poists) poistsm) '$x)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (difference (remainder h poists) poistsm) '$y)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (difference (remainder h poists) poistsm) '$z)))
     (return (simplifya arg nil)))) 


;;; THIS PROGRAM MULTIPLIES A POISSON SERIES P BY A NON-SERIES, C,
;;; WHICH IS FREE OF SINES AND COSINES .

(defmfun $poisctimes (c p) 
  (list '(mpois simp) (poisctimes1 (setq c (intopoisco c)) (cadr p)) (poisctimes1 c (caddr p)))) 

 

(defmfun $outofpois (p) 
  (prog (ans) 
     (cond ((or (atom p) (not (eq (caar p) 'mpois))) (setq p (intopois p))))

     ;; DO SINES
     (do ((m
	   (cadr p)
	   (cddr m)))(
		      (null m))
       (setq ans (cons (list '(mtimes)
			     (poiscdecode (cadr m))
			     (list '(%sin) (poisdecodec (car m))))
		       ans)))

     ;; DO COSINES
     (do ((m
	   (caddr p)
	   (cddr m)))(
		      (null m))
       (setq ans (cons (list '(mtimes)
			     (poiscdecode (cadr m))
			     (cond ((equal (car m) poishift) 1.)
				   (t (list '(%cos) (poisdecodec (car m))))))
		       ans)))
     (return (cond ((null ans) 0.) (t (simplifya (cons '(mplus) ans) nil)))))) 

(defmfun $printpois (p) 
  (prog nil 
     (setq p (intopois p))

     ;; DO SINES
     (do ((m
	   (cadr p)
	   (cddr m)))(
		      (null m))
       (displa (simplifya (list '(mtimes)
				(poiscdecode (cadr m))
				(list '(%sin) (poisdecodec (car m))))
			  t))
       (terpri))

     ;; DO COSINES
     (do ((m
	   (caddr p)
	   (cddr m)))(
		      (null m))
       (displa (simplifya (list '(mtimes)
				(poiscdecode (cadr m))
				(cond ((equal (car m) poishift) 1.)
				      (t (list '(%cos) (poisdecodec (car m))))))
			  t))
       (terpri))
     (return '$done))) 


;;; $POISDIFF DIFFERENTIATES A POISSON SERIES WRT X, Y, Z, U, V, W, OR A COEFF VAR.


(defmfun $poisdiff (p m)
  (declare (special m))
  (cond ((memq m '($u $v $w $x $y $z))
	 (list (car p) (cosdif (caddr p) m) (sindif (cadr p) m)))
	(t (list (car p)(poisdif4(cadr p))(poisdif4 (caddr p))))))


(defun poisdif4 (y)
  (declare (special m))
  (cond ((null y) nil)
	(t (tcons3 (car y)(poiscodif (cadr y) m) (poisdif4 (cddr y))))))

 


;;; COSDIF DIFFERENTIATES COSINES TO GET SINES

(defun cosdif (h m) 
  (cond ((null h) nil)
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (minus (poisxcoef (car h) m))) (cadr h))
			(cosdif (cddr h) m)))))) 

(defun sindif (h m) 
  (cond ((null h) nil)
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (poisxcoef (car h) m)) (cadr h)) (sindif (cddr h) m)))))) 

(defun poisxcoef (h m) 
  (difference (remainder (quotient h
				   (expt poists
					 (cadr (memq m '($u 0. $v 1. $w 2. $x 3. $y 4. $z 5.)))))
			 poists)
	      poistsm)) 




;;; AVL BALANCED TREE SEARCH AND INSERTION.
;;; NODE LOOKS LIKE (KEY (LLINK .  RLKINK) BALANCEFACTOR .  RECORD)
;;; PROGRAM FOLLOWS ALGORITHM GIVEN IN KNUTH VOL. 3 455-57

(declare-top (special ans)) 


;; MACROS TO EXTRACT FIELDS FROM NODE

(defmacro key  (&rest l) (cons 'car l)) 

(defmacro llink  (&rest l) (cons 'caadr l)) 

(defmacro rlink  (&rest l) (cons 'cdadr l)) 

(defmacro bp  (&rest l) (cons 'caddr l)) 

(defmacro rec  (&rest l) (cons 'cdddr l)) 


;;  FOR ORDERING KEYS

(defmacro order<  (&rest l) (cons '<  l))
(defmacro order=  (&rest l) (cons '=  l))

;; MACROS TO SET FIELDS IN NODE

(defmacro setrlink  (&rest l) (setq l (cons nil l))
	  (list 'rplacd (list 'cadr (cadr l)) (caddr l))) 

(defmacro setllink  (&rest l) (setq l (cons nil l))
	  (list 'rplaca (list 'cadr (cadr l)) (caddr l))) 

(defmacro setbp  (&rest l) (setq l (cons nil l))
	  (list 'rplaca (list 'cddr (cadr l)) (caddr l))) 

(defmacro setrec  (&rest l)(setq l (cons nil l))
	  (list 'rplacd (list 'cddr (cadr l)) (caddr l)))
 

(defun insert-it (pp newrec) (setrec pp (poisco+ (rec pp) newrec))) 

(defun avlinsert (k newrec head) 
  (prog (qq tt ss pp rr) 
     (setq tt head)
     (setq ss (setq pp (rlink head)))
     a2   (cond ((order< k (key pp)) (go a3))
		((order< (key pp) k) (go a4))
		(t (insert-it pp newrec) (return head)))
     a3   (setq qq (llink pp))
     (cond ((null qq) (setllink pp (cons k (cons (cons nil nil) (cons 0. newrec)))) (go a6))
	   ((order= 0. (bp qq)) nil)
	   (t (setq tt pp ss qq)))
     (setq pp qq)
     (go a2)
     a4   (setq qq (rlink pp))
     (cond ((null qq) (setrlink pp (cons k (cons (cons nil nil) (cons 0. newrec)))) (go a6))
	   ((order= 0. (bp qq)) nil)
	   (t (setq tt pp ss qq)))
     (setq pp qq)
     (go a2)
     a6   (cond ((order< k (key ss)) (setq rr (setq pp (llink ss)))) (t (setq rr (setq pp (rlink ss)))))
     a6loop
     (cond ((order< k (key pp)) (setbp pp -1.) (setq pp (llink pp)))
	   ((order< (key pp) k) (setbp pp 1.) (setq pp (rlink pp)))
	   ((order= k (key pp)) (go a7)))
     (go a6loop)
     a7   (cond ((order< k (key ss)) (go a7l)) (t (go a7r)))
     a7l  (cond ((order= 0. (bp ss)) (setbp ss -1.) (setllink head (f1+ (llink head))) (return head))
		((order= (bp ss) 1.) (setbp ss 0.) (return head)))
     (cond ((order= (bp rr) -1.) nil) (t (go a9l)))
     (setq pp rr)
     (setllink ss (rlink rr))
     (setrlink rr ss)
     (setbp ss 0.)
     (setbp rr 0.)
     (go a10)
     a9l  (setq pp (rlink rr))
     (setrlink rr (llink pp))
     (setllink pp rr)
     (setllink ss (rlink pp))
     (setrlink pp ss)
     (cond ((order= (bp pp) -1.) (setbp ss 1.) (setbp rr 0.))
	   ((order= (bp pp) 0.) (setbp ss 0.) (setbp rr 0.))
	   ((order= (bp pp) 1.) (setbp ss 0.) (setbp rr -1.)))
     (setbp pp 0.)
     (go a10)
     a7r  (cond ((order= 0. (bp ss)) (setbp ss 1.) (setllink head (f1+ (llink head))) (return head))
		((order= (bp ss) -1.) (setbp ss 0.) (return head)))
     (cond ((order= (bp rr) 1.) nil) (t (go a9r)))
     (setq pp rr)
     (setrlink ss (llink rr))
     (setllink rr ss)
     (setbp ss 0.)
     (setbp rr 0.)
     (go a10)
     a9r  (setq pp (llink rr))
     (setllink rr (rlink pp))
     (setrlink pp rr)
     (setrlink ss (llink pp))
     (setllink pp ss)
     (cond ((order= (bp pp) 1.) (setbp ss -1.) (setbp rr 0.))
	   ((order= (bp pp) 0.) (setbp ss 0.) (setbp rr 0.))
	   ((order= (bp pp) -1.) (setbp ss 0.) (setbp rr 1.)))
     (setbp pp 0.)
     a10  (cond ((eq ss (rlink tt)) (setrlink tt pp)) (t (setllink tt pp)))
     (return head))) 

(defun avlinit (key rec) 
  (cons 'top (cons (cons 0. (cons key (cons (cons nil nil) (cons 0. rec)))) (cons 0. nil)))) 


;; UNTREE CONVERTS THE TREE TO A LIST WHICH LOOKS LIKE ( SmALLEST-KEY RECORD NEXT-SMALLEST-KEY RECORD ....  LARGEST-KEY
;;RECORD)

(defun untree (h) (prog (ans) (untree1 (rlink h)) (return ans))) 

(defun untree1 (h) 
  (cond ((null h) ans)
	((null (rlink h)) (setq ans (tcons3 (key h) (rec h) ans)) (untree1 (llink h)))
	(t (setq ans (tcons3 (key h) (rec h) (untree1 (rlink h)))) (untree1 (llink h))))) 

(defun tcons3 (r s tt) (cond ((poispzero s) tt) (t (cons r (cons s tt))))) 


(defun poismerges (a ae l) 
  (cond ((equal poishift ae) l)		; SINE(0) IS 0
	((poisnegpred ae) (poismerge (poisco* poiscom1 a) (poischangesign ae) l))
	(t (poismerge a ae l)))) 

(defun poismergec (a ae l) 
  (cond ((poisnegpred ae) (poismerge a (poischangesign ae) l)) (t (poismerge a ae l)))) 

(defun poismerge (a ae l) (cond ((poispzero a) nil) (t (merge11 a ae l)))) 

(defun poismerge2 (r s) 
  (cond ((null r) s)
	((null s) r)
	(t (prog (m n tt) 
	      (setq m (setq n (cons 0. r)))
	      a    (cond ((null r) (rplacd m s) (return (cdr n)))
			 ((null s) (return (cdr n)))
			 ((equal (car r) (car s))
			  (setq tt (poisco+ (cadr r) (cadr s)))
			  (cond ((poispzero tt) (rplacd m (cddr r)) (setq r (cddr r) s (cddr s)))
				(t (rplaca (cdr r) tt) (setq s (cddr s) r (cddr r) m (cddr m)))))
			 ((greaterp (car r) (car s))
			  (rplacd m s)
			  (setq s (cddr s))
			  (rplacd (cddr m) r)
			  (setq m (cddr m)))
			 (t (setq r (cddr r)) (setq m (cddr m))))
	      (go a))))) 

(defun merge11 (a ae l) (poismerge2 (list ae a) l)) 

(defun poismergesx (a ae l) 
  (cond ((equal poishift ae) l)		; SINE(0) IS 0
	((poisnegpred ae) (avlinsert (poischangesign ae) (poisco* poiscom1 a) l))
	(t (avlinsert ae a l)))) 

(defun poismergecx (a ae l) 
  (cond ((poisnegpred ae) (avlinsert (poischangesign ae) a l)) (t (avlinsert ae a l)))) 




(declare-top (special trim poiscom1 poishift)) 

(defun poisctimes1 (c h) 
  (cond ((null h) nil)
	((and trim (trimf (car h))) (poisctimes1 c (cddr h)))
	(t (tcons (car h) (cons (poisco* c (cadr h)) (poisctimes1 c (cddr h))))))) 

(defun trimf (m) 
  (meval (list '($poistrim)
	       (poisxcoef m '$u)
	       (poisxcoef m '$v)
	       (poisxcoef m '$w)
	       (poisxcoef m '$x)
	       (poisxcoef m '$y)
	       (poisxcoef m '$z)))) 

(defmfun $poistimes (a b) 
  (prog (slc clc temp ae aa zero trim t1 t2 f1 f2) 
     (setq a (intopois a) b (intopois b))
     (cond ((or (getl-fun '$poistrim '(expr subr))
		(mget '$poistrim 'mexpr))
	    (setq trim t)))
     (cond ((nonperiod a) (return ($poisctimes (cadr (caddr a)) b)))
	   ((nonperiod b) (return ($poisctimes (cadr (caddr b)) a))))
     (setq slc (avlinit poishift (setq zero (intopoisco 0.))) clc (avlinit poishift zero))
     ;; PROCEED THROUGH ALL THE SINES IN ARGUMENT A
     (do ((sla
	   (cadr a)
	   (cddr sla)))(
			(null sla))
       (setq aa (halve (cadr sla)) ae (car sla))
       ;; SINE(U)*SINE(V) ==> (-COSINE(U+V) + COSINE(U-V))/2
       (do ((slb
	     (cadr b)
	     (cddr slb)))(
			  (null slb))
	 (setq t1 (plus ae poishift (minus (car slb))) t2 (plus ae (minus poishift) (car slb)))
	 (cond(trim(setq f1(trimf t1) f2 (trimf t2)))
	      (t (setq f1 nil f2 nil)))
	 (setq temp (poisco* aa (cadr slb)))
	 (cond ((poispzero temp) nil)
	       (t (or f1 (poismergecx temp t1 clc))
		  (or f2 (poismergecx (poisco* poiscom1 temp) t2 clc)))))
       ;; SINE*COSINE ==> SINE + SINE
       (do ((clb
	     (caddr b)
	     (cddr clb)))(
			  (null clb))
	 (setq t1 (plus ae poishift (minus (car clb))) t2 (plus ae (minus poishift) (car clb)))
	 (cond(trim(setq f1(trimf t1) f2 (trimf t2)))
	      (t (setq f1 nil f2 nil)))
	 (setq temp (poisco* aa (cadr clb)))
	 (cond ((poispzero temp) nil)
	       (t (or f1 (poismergesx temp t1 slc)) (or f2 (poismergesx temp t2 slc))))))
     ;; PROCEED THROUGH ALL THE COSINES IN ARGUMENT A
     (do ((cla
	   (caddr a)
	   (cddr cla)))(
			(null cla))
       (setq aa (halve (cadr cla)) ae (car cla))
       ;; COSINE*SINE ==> SINE - SINE
       (do ((slb
	     (cadr b)
	     (cddr slb)))(
			  (null slb))
	 (setq t1 (plus ae poishift (minus (car slb))) t2 (plus ae (minus poishift) (car slb)))(cond(trim(setq f1(trimf t1) f2 (trimf t2)))
												    (t (setq f1 nil f2 nil))
												    )
	 (cond 
	   (t (setq temp (poisco* aa (cadr slb)))
	      (cond ((poispzero temp) nil)
		    (t (or f1 (poismergesx (poisco* poiscom1 temp) t1 slc))
		       (or f2 (poismergesx temp t2 slc)))))))
       ;; COSINE*COSINE ==> COSINE + COSINE
       (do ((clb
	     (caddr b)
	     (cddr clb)))(
			  (null clb)) 
	 (setq t1 (plus ae poishift (minus (car clb))) t2 (plus ae (minus poishift) (car clb)))(cond(trim(setq f1(trimf t1) f2 (trimf t2)))
												    (t (setq f1 nil f2 nil))
												    )
	 (cond 
	   (t (setq temp (poisco* aa (cadr clb)))
	      (cond ((poispzero temp) nil)
		    (t (or f1 (poismergecx temp t1 clc))
		       (or f2 (poismergecx temp t2 clc))))))))
     (return (list '(mpois simp) (untree slc) (untree clc)))))

(defmfun $poisexpt (p n) 
  (prog (u h) 
     (cond ((oddp n) (setq u p)) (t (setq u (setq h (intopois 1.)))))
     a    (setq n (lsh n -1.))
     (cond ((zerop n) (return u)))
     (setq p ($poistimes p p))
     (cond ((oddp n) (setq u (cond ((equal u h) p) (t ($poistimes u p))))))
     (go a))) 

(defmfun $poissquare (a) ($poisexpt a 2))


;;; $POISINT INTEGRATES A POISSON SERIES WRT X,Y, Z, U, V, W.  THE VARIABLE OF
;;; INTEGRATION MUST OCCUR ONLY IN THE ARGUMENTS OF SIN OR COS,
;;; OR ONLY IN THE COEFFICIENTS.  POISCOINTEG IS CALLED TO INTEGRATE COEFFS.

;;; NON-PERIODIC TERMS ARE REMOVED.

(defmfun $poisint (p m) 
  (declare (special m))
  (prog (b*)
     (setq p (intopois p))
     (cond ((memq m '($u $v $w $x $y $z))
	    (return (list (car p)
			  (cosint* (caddr p) m)
			  (sinint* (cadr p) m))))
	   (t (return (list (car p)
			    (poisint4 (cadr p))
			    (poisint4 (caddr p)))))))) 

(defun poisint4 (y)
  (declare (special m))
  (cond ((null y) nil)
	(t (tcons3 (car y)(poiscointeg (cadr y) m) (poisint4 (cddr y))))))

;;;COSINT* INTEGRATES COSINES TO GET SINES

(defun cosint* (h m) 
  (cond ((null h) nil)
	((equal 0. (setq b* (poisxcoef (car h) m))) (cosint* (cddr h) m))
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (list '(mexpt) b* -1.)) (cadr h))
			(cosint* (cddr h) m)))))) 

(defun sinint* (h m) 
  (cond ((null h) nil)
	((equal 0. (setq b* (poisxcoef (car h) m))) (sinint* (cddr h) m))
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (list '(mexpt) (minus (poisxcoef (car h) m)) -1.))
				 (cadr h))
			(sinint* (cddr h) m)))))) 


;;; $POISSUBST SUBSTITUTES AN EXPRESSION FOR A VARIABLE IN ARGUMENT OF TRIG FUNCTIONS OR
;;; COEFFICIENTS.

(defun poissubsta (a b* c) 
  (prog (ss cc) 
     (setq h* (difference (poisencode (list '(mplus) a (list '(mtimes) -1. b*)))
			  poishift))
     (poissubst1s (cadr c))
     (poissubst1c (caddr c))
     (return (list (car c) ss cc)))) 

(defun poissubst1s (c) 
  (cond ((null c) nil) (t (setq ss (poismerges (cadr c) (argsubst (car c)) ss)) (poissubst1s (cddr c))))) 

(defun poissubst1c (c) 
  (cond ((null c) nil) (t (setq cc (poismergec (cadr c) (argsubst (car c)) cc)) (poissubst1c (cddr c))))) 

(defun argsubst (c) (plus c (times h* (poisxcoef c b*)))) 

(defmfun $poissubst n 
  (cond ((not (or (equal n 3.) (equal n 5.))) (merror  "WRONG NUMBER OF ARGS TO POISSUBST"))
	((equal n 5.)
	 (fancypoissubst (arg 1.) (arg 2.) (intopois (arg 3.)) (intopois (arg 4.)) (arg 5.)))
	(t ((lambda (a* b* c) (cond ((memq b* '($u $v $w $x $y $z)) (poissubsta a* b* c))
				    (t (list (car c) (poissubstco1 (cadr c)) (poissubstco1 (caddr c))))))
	    (arg 1.)
	    (arg 2.)
	    (intopois (arg 3.)))))) 



(declare-top (unspecial $u $v $w $x $y $z)) 

(defun poissubstco1 (c) 
  (cond ((null c) nil) (t (tcons (car c) (cons (poissubstco a* b* (cadr c)) (poissubstco1 (cddr c))))))) 

(declare-top (special dc ds *ans)) 

(defun fancypoissubst (a b* c d n) 

  ;;SUBSTITUTES A+D FOR B IN C, WHERE D IS EXPANDED IN POWERSERIES TO ORDER N
  (prog (h* dc ds *ans) 
     (setq *ans (list '(mpois simp) nil nil) d (intopois d) dc (intopois 1.) ds (intopois 0.))
     (cond ((equal n 0.) (return ($poissubst a b* c))))
     (fancypois1s d 1. 1. n)
     (setq h* (difference (poisencode (list '(mplus) a (list '(mtimes) -1. b*)))
			  poishift))
     (fancypas (cadr c))
     (fancypac (caddr c))
     (return *ans))) 

(defun fancypois1s (d dp n lim)	; DP IS LAST POWER: D^(N-1), LIM IS HIGHEST TO
  (cond ((greaterp n lim) nil)		;GO
	(t (setq ds ($poisplus ds
			       ($poisctimes (list '(rat)
						  (expt -1. (quotient (sub1 n) 2.))
						  (factorial n))
					    (setq dp ($poistimes dp d)))))
	   (fancypois1c d dp (f1+ n) lim)))) 

(defun fancypois1c (d dp n lim)	; DP IS LAST POWER: D^(N-1), LIM IS HIGHEST TO
  (cond ((greaterp n lim) nil)		;GO
	(t (setq dc
		 ($poisplus dc
			    ($poisctimes (list '(rat) (expt -1. (quotient n 2.)) (factorial n))
					 (setq dp ($poistimes dp d)))))
	   (fancypois1s d dp (f1+ n) lim)))) 


;;; COS(R+K*B) ==> K*COS(R+K*A)*DC - K*SIN(R+K*A)*DS
;;; SIN(R+K*B) ==> K*COS(R+K*A)*DS + K*SIN(R+K*A)*DC 

(declare-top (special *argc *coef)) 

(defun fancypac (c) 
  (prog nil 
     (cond ((null c) (return nil)))
     (setq *coef (poisxcoef (car c) b*))
     (cond ((equal *coef 0.)
	    (setq *ans ($poisplus *ans (list '(mpois simp) nil (list (car c) (cadr c)))))
	    (go end)))
     (cond ((poispzero (setq *coef (poisco* (cadr c) (intopoisco *coef)))) (go end)))
     (setq *argc (argsubst (car c)))
     (setq *ans
	   ($poisplus *ans
		      ($poisplus ($poistimes (list '(mpois simp)
						   nil
						   (poismergec *coef *argc nil))
					     dc)
				 ($poistimes (list '(mpois simp)
						   (poismerges (poisco* poiscom1 *coef) *argc nil)
						   nil)
					     ds))))
     end  (fancypac (cddr c)))) 

(defun fancypas (c) 
  (prog nil 
     (cond ((null c) (return nil)))
     (setq *coef (poisxcoef (car c) b*))
     (cond ((equal *coef 0.)
	    (setq *ans ($poisplus *ans (list '(mpois simp) (list (car c) (cadr c)) nil)))
	    (go end)))
     (cond ((poispzero (setq *coef (poisco* (cadr c) (intopoisco *coef)))) (go end)))
     (setq *argc (argsubst (car c)))
     (setq *ans ($poisplus *ans
			   ($poisplus ($poistimes (list '(mpois simp)
							nil
							(poismergec *coef *argc nil))
						  ds)
				      ($poistimes (list '(mpois simp)
							(poismerges *coef *argc nil)
							nil)
						  dc))))
     end  (fancypas (cddr c)))) 

					;ARGUMENT  DO NOT EXCEED 15 IN ABSOLUTE VALUE


;;; THESE ARE THE ONLY COEFFICIENT DEPENDENT ROUTINES. RATIONAL FORM IS
;;; DEFINED IN FILE RATPOI >.

;;; POISCDECODE DECODES A COEFFICIENT

(defun poiscdecode (x) x) 


;;; INTOPOISCO PUTS AN EXPRESSION INTO POISSON COEFFICIENT FORM

(defun intopoisco (x) (simplifya x nil)) 


;;; POISCO+ ADDS 2 COEFFICIENTS

(defun poisco+ (r s) (simplifya (list '(mplus) r s) nil)) 


;;; POISCO* MULTIPLIES 2 COEFFICIENTS

(defun poisco* (r s) (simplifya (list '(mtimes) r s) nil)) 


;;; HALVE DIVIDES A COEFFICIENT BY 2

(defun halve (r) (simplifya (list '(mtimes) '((rat) 1. 2.) r) nil)) 


;;; POISSUBSTCO SUBSTITUTES AN EXPRESSION FOR A VARIABLE IN A COEFFICIENT.

(defun poissubstco (a b c) (maxima-substitute a b c)) 

;;; THIS DIFFERENTIATES A COEFFICIENT

(defun poiscodif (h var)($diff h var))

;;; THIS INTEGRATES A COEFFICIENT
(defun poiscointeg (h var)(intopoisco($integrate (poiscdecode h) var)))

;;; TEST FOR ZERO

(defun poispzero (x) (zerop1 x)) 


;;; THE NUMBER 1 IN COEFFICIENT ARITHMETIC, THE NUMBER -1

(setq poisco1 1. poiscom1 -1.) 

 


;; THE FOLLOWING PROGRAMS FOLLOW THE SUGGESTIONS OF W.H.JEFFERYS, FOR
;; FASTER POISSON SERIES MULTIPLICATION THAN JUST STRAIGHT INSERTION.
;; THEY ARE NOT AS FAST AS THE AVL TREE INSERTION, HOWEVER.  WE KEEP
;; THEM HERE FOR THE RECORD.

;;(COMMENT(DECLARE (SPECIAL SLCX CLCX LASTPTR TRIM POISCOM1 POISHIFT CLC SLC CLCPTR SLCPTR)) 

;;(DEFUN POISMERGE2K (S R) 
;;       (COND ((NULL R)(SETQ LASTPTR S))
;;	     ((NULL S) (SETQ LASTPTR R))
;;	     (T (PROG (M N TT) 
;;		      (SETQ M (SETQ N (CONS 0. R)))
;;		 A    (COND ((NULL R) (RPLACD M S)(SETQ LASTPTR S) (RETURN (CDR N)))
;;			    ((NULL S) (SETQ LASTPTR R) (RETURN (CDR N)))
;;			    ((EQUAL (CAR R) (CAR S))
;;			     (SETQ TT (POISCO+ (CADR R) (CADR S)))
;;			     (COND ((POISPZERO TT) (RPLACD M (CDDR R)) (SETQ R (CDDR R) S
;;									     (CDDR S)))
;;				   (T (RPLACA (CDR R) TT) (SETQ S (CDDR S) R (CDDR R) M
;;								(CDDR M)))))
;;			    ((GREATERP (CAR R) (CAR S))
;;			     (RPLACD M S)
;;			     (SETQ S (CDDR S))
;;			     (RPLACD (CDDR M) R)
;;			     (SETQ M (CDDR M)))
;;			    (T (SETQ R (CDDR R)) (SETQ M (CDDR M))))
;;		      (GO A)))))

;;(DEFUN POISMERGESQ (A AE L) 
;;       (SETQ SLCX (COND ((EQUAL POISHIFT AE) L)			       ; SINE(0) IS 0
;;		       ((POISNEGPRED AE) (POISMERGE (POISCO* POISCOM1 A)
;;						    (POISCHANGESIGN AE) L))
;;		       (T (POISMERGE A AE L))))) 

;;(DEFUN POISMERGECQ (A AE L) 
;;       (SETQ CLCX (COND ((POISNEGPRED AE) (POISMERGE A (POISCHANGESIGN AE) L))
;;			(T (POISMERGE A AE L))))) 

;;(DEFUN POISMERGESY (A AE L) 
;;       (SETQ SLC (COND ((EQUAL POISHIFT AE) L)			       ; SINE(0) IS 0
;;		       ((POISNEGPRED AE) (POISMERGESY1 (POISCO* POISCOM1 A)
;;						       (POISCHANGESIGN AE) L))
;;		       (T (POISMERGESY1 A AE L))))) 

;;(DEFUN POISMERGECY (A AE L) 
;;       (SETQ CLC (COND ((POISNEGPRED AE) (POISMERGECY1 A (POISCHANGESIGN AE) L))
;;		       (T (POISMERGECY1 A AE L))))) 

;;(DEFUN POISMERGECY1 (A AE L) 
;;       (COND ((POISPZERO A) NIL)
;;	     ((OR(NULL CLCPTR)(LESSP AE (CAR CLCPTR))) (SETQ CLC(POISMERGE2K(LIST AE A) L))
;;	      (SETQ CLCPTR LASTPTR))
;;	     (T (POISMERGE2K(LIST AE A) CLCPTR)
;;(SETQ CLCPTR LASTPTR)))
;; CLC) 

;;(DEFUN POISMERGESY1 (A AE L) 
;;       (COND ((POISPZERO A) NIL)
;;	     ((OR(NULL SLCPTR)(LESSP AE (CAR SLCPTR))) (SETQ SLC(POISMERGE2K(LIST AE A) L))(SETQ SLCPTR LASTPTR))
;;	     (T (POISMERGE2K(LIST AE A) SLCPTR)
;;(SETQ SLCPTR LASTPTR)))
;; SLC ) 

;;(DEFMFUN $POISTIMESL (A B) 
;;	 (PROG (SLC SLCPTR CLC CLCPTR TEMP AE AA  TRIM T1 T2 F1 F2 LASTPTR SLCX CLCX) 
;;	       (SETQ A (INTOPOIS A) B (INTOPOIS B))
;;	       (COND ((OR (GETL '$POISTRIM '(EXPR SUBR)) (MGET '$POISTRIM 'MEXPR))
;;		      (SETQ TRIM T)))
;;	       (COND ((NONPERIOD A) (RETURN ($POISCTIMES (CADR (CADDR A)) B)))
;;		     ((NONPERIOD B) (RETURN ($POISCTIMES (CADR (CADDR B)) A))))
;;	       ;; PROCEED THROUGH ALL THE SINES IN ARGUMENT A
;;	       (SETQ SLCPTR SLC CLCPTR CLC CLCX NIL SLCX NIL)
;;	       (DO SLA
;;		   (CADR A)
;;		 (CDDR SLA)
;;		 (NULL SLA)
;;		 (SETQ AA (HALVE (CADR SLA)) AE (CAR SLA))
;;		 ;; SINE(U)*SINE(V) ==> (-COSINE(U+V) + COSINE(U-V))/2
;;		 (DO SLB
;;		     (CADR B)
;;		   (CDDR SLB)
;;		   (NULL SLB)
;;		   (SETQ T1 (PLUS AE POISHIFT (MINUS (CAR SLB)))
;;			 T2 (PLUS AE (MINUS POISHIFT) (CAR SLB)))
;;		   (COND ((AND TRIM (SETQ F1 (TRIMF T1)) (SETQ F2 (TRIMF T2)))
;;			  (SETQ F1 NIL F2 NIL))
;;			 (T (SETQ TEMP (POISCO* AA (CADR SLB)))
;;			    (COND ((POISPZERO TEMP) NIL)
;;				  (T (OR F1 (POISMERGECQ TEMP T1 CLCX))
;;				     (OR F2 (POISMERGECY (POISCO* POISCOM1 TEMP) T2 CLC)))))))
;;		 ;; SINE*COSINE ==> SINE + SINE
;;		 (DO CLB
;;		     (CADDR B)
;;		   (CDDR CLB)
;;		   (NULL CLB)
;;		   (SETQ T1 (PLUS AE POISHIFT (MINUS (CAR CLB))) T2
;;			 (PLUS AE (MINUS POISHIFT) (CAR CLB)))
;;		   (COND ((AND TRIM (SETQ F1 (TRIMF T1)) (SETQ F2 (TRIMF T2)))
;;			  (SETQ F1 NIL F2 NIL))
;;			 (T (SETQ TEMP (POISCO* AA (CADR CLB)))
;;			    (COND ((POISPZERO TEMP) NIL)
;;				  (T (OR F1 (POISMERGESQ TEMP T1 SLCX))
;;				     (OR F2 (POISMERGESY TEMP T2 SLC))))))))
;;	       (SETQ CLC(POISMERGE2 CLC CLCX)SLC (POISMERGE2 SLC SLCX))
	       
;;	       ;; PROCEED THROUGH ALL THE COSINES IN ARGUMENT A
;;	       (SETQ SLCPTR SLC CLCPTR CLC SLCX NIL CLCX NIL)
;;	       (DO CLA
;;		   (CADDR A)
;;		 (CDDR CLA)
;;		 (NULL CLA)
;;		 (SETQ AA (HALVE (CADR CLA)) AE (CAR CLA))
;;		 ;; COSINE*SINE ==> SINE - SINE
;;		 (DO SLB
;;		     (CADR B)
;;		   (CDDR SLB)
;;		   (NULL SLB)
;;		   (SETQ T1 (PLUS AE POISHIFT (MINUS (CAR SLB))) T2
;;			 (PLUS AE (MINUS POISHIFT) (CAR SLB)))
;;		   (COND ((AND TRIM (SETQ F1 (TRIMF T1)) (SETQ F2 (TRIMF T2)))
;;			  (SETQ F1 NIL F2 NIL))
;;			 (T (SETQ TEMP (POISCO* AA (CADR SLB)))
;;			    (COND ((POISPZERO TEMP) NIL)
;;				  (T (OR F1 (POISMERGESQ (POISCO* POISCOM1 TEMP) T1 SLCX))
;;				     (OR F2 (POISMERGESY TEMP T2 SLC)))))))
;;		 ;; COSINE*COSINE ==> COSINE + COSINE
;;		 (DO CLB
;;		     (CADDR B)
;;		   (CDDR CLB)
;;		   (NULL CLB)
;;		   (SETQ T1 (PLUS AE POISHIFT (MINUS (CAR CLB))) T2
;;			 (PLUS AE (MINUS POISHIFT) (CAR CLB)))
;;		   (COND ((AND TRIM (SETQ F1 (TRIMF T1)) (SETQ F2 (TRIMF T2)))
;;			  (SETQ F1 NIL F2 NIL))
;;			 (T (SETQ TEMP (POISCO* AA (CADR CLB)))
;;			    (COND ((POISPZERO TEMP) NIL)
;;				  (T (OR F1 (POISMERGECQ TEMP T1 CLCX))
;;				     (OR F2 (POISMERGECY TEMP T2 CLC))))))))
;;	       (SETQ CLC(POISMERGE2 CLC CLCX)SLC (POISMERGE2 SLC SLCX))
;;	       (RETURN (LIST '(MPOIS SIMP) SLC CLC)))) 
;;)


