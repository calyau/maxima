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
(macsyma-module rat3c)

;;	THIS IS THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES AND THEIR SUPPORTING FUNCTIONS

;;(DECLARE-TOP (GENPREFIX A_3))

(load-macsyma-macros ratmac)

(declare-top (special $float $keepfloat $algebraic $ratfac *alpha)
	     (special genvar))


;; List of GCD algorithms.  Default one is first.
(defmvar *gcdl* '($subres $ez $red $spmod $mod $sphen $eez $algebraic))

(defmvar $gcd (car *gcdl*))		;Sparse Modular

(defun cgcd (a b) (cond (modulus 1)
			((and $keepfloat (or (floatp a) (floatp b))) 1)
			(t (gcd a b)))) 

(defmfun pquotientchk (a b) (if (eqn b 1) a (pquotient a b)))

(defun ptimeschk (a b) (cond ((eqn a 1) b) ((eqn b 1) a) (t (ptimes a b)))) 

(defun pfloatp (x)
  (catch 'float (cond ((pcoefp x) (floatp x)) (t (pfloatp1 x)))))

(defun pfloatp1 (x)
  (mapc #'(lambda (q) (cond ((pcoefp q)
			     (cond ((floatp q) (throw 'float t))))
			    ((pfloatp1 q))))
	(cdr x))
  nil)

(defmfun pgcd (x y)
  (setq x (car (pgcda x y nil)))
  (cond ((pminusp x) (pminus x))
	(modulus (monize x))
	(t x)))

(defmfun plcm (x y)
  (setq x (pgcdcofacts x y))
  (ptimes (car x) (ptimes (cadr x) (caddr x))))

(defun plcmcofacts (x y)
  (setq x (pgcdcofacts x y))
  (list (ptimes (car x) (ptimes (cadr x) (caddr x)))
	(caddr x) (cadr x)))

(defun pgcdcofacts (x y) 
  (let ((a (pgcda x y t)))
    (cond ((cdr a) a)
	  ((equal (setq a (car a)) 1) (list 1 x y))
	  ((and $algebraic (not (pcoefp a)))
	   (cons a (prog2 (setq x (rquotient x a)
				y (rquotient y a)
				a (pgcdcofacts (cdr x) (cdr y)))
		       (list (ptimes (car x) (caddr a))
			     (ptimes (car y) (cadr a))
			     (ptimes (cadr a) (cdr y))))))
	  ((eq a x) (list x 1 (pquotient y x)))
	  ((eq a y) (list a (pquotient x y) 1))
	  (t (list a (pquotient x a) (pquotient y a))))))

(defun pgcda (x y cofac? &aux a c) 
  (cond ((not $gcd) (list 1 x y))
	((and $keepfloat (or (pfloatp x) (pfloatp y)))
	 (cond ((or (pcoefp x) (pcoefp y)
		    (pcoefp (setq a (car (ptermcont x))))
		    (pcoefp (setq a (pgcd a (car (ptermcont y))))))
		(list 1 x y))
	       (t (list a))))
	((pcoefp x)
	 (cond ((pcoefp y)
		(cons (setq a (cgcd x y))
		      (and cofac?
			   (list (cquotient x a) ;(CQUOTIENT 0 0) = 0
				 (cquotient y a)))))
	       ((zerop x) (list y x 1))
	       (t (list (pcontent1 (cdr y) x)))))
	((pcoefp y) (cond ((zerop y) (list x 1 y))
			  (t (list (pcontent1 (cdr x) y)))))
	((equal x y) (list x 1 1))
	($ratfac (fpgcdco x y))
	((not (eq (p-var x) (p-var y)))
	 (list (if (pointergp (p-var x) (p-var y))
		   (oldcontent1 (p-terms x) y)
		   (oldcontent1 (p-terms y) x))))
	((progn (desetq (a x) (ptermcont x))
		(desetq (c y) (ptermcont y))
		(not (and (equal a 1) (equal c 1))))
	 (mapcar #'ptimes (monomgcdco a c cofac?) (pgcda x y cofac?)))
	((and (not $algebraic) (not modulus)
	      (desetq (a . c) (lin-var-find (nreverse (pdegreevector x))
					    (nreverse (pdegreevector y))
					    (reverse genvar))))
	 (cond ((f= a 1) (linhack x y (car c) (cadr c) cofac?))
	       (t (setq a (linhack y x a (cadr c) cofac?))
		  (if (cdr a) (rplacd a (nreverse (cdr a))))
		  a)))
	((eq $gcd '$spmod) (list (zgcd x y)))
	((eq $gcd '$subres) (list (oldgcd x y)))
	((eq $gcd '$algebraic)
	 (if (or (palgp x) (palgp y))
	     (let (($gcd '$subres)) (list (oldgcd x y)))
	     (let (($gcd '$spmod)) (list (zgcd x y)))))
	((eq $gcd '$ez) (ezgcd2 x y))
	((eq $gcd '$red) (list (oldgcd x y)))
	((eq $gcd '$mod) (newgcd x y modulus))
	((eq $gcd '$sphen) (merror "sphgcd not implemented")) ; (SPHGCD X Y))
	((eq $gcd '$eez) (merror "eezgcd not implemented")) ; (EEZGCD X Y))
	((not (memq $gcd *gcdl*))
	 (merror "GCD set incorrectly:~%~M" $gcd))
	(t (list 1 x y))))

;; (DEFUN PMINDEG (P) (IF (PCOEFP P) 0 (NXTTOLAST (CDR P))))

;; (DEFUN PDEGRED (P N) (IF (ZEROP N) P (PQUOTIENT P (MAKE-POLY (P-VAR P) N 1))))

(defun monomgcdco (p q cofac?)
  (let ((gcd (monomgcd p q)))
    (cons gcd (if cofac? (list (pquotient p gcd) (pquotient q gcd)) ()))))
  
(defun monomgcd (p q)
  (cond ((or (pcoefp p) (pcoefp q)) 1)
	((eq (p-var p) (p-var q))
	 (make-poly (p-var p) (min (p-le p) (p-le q))
		    (monomgcd (p-lc p) (p-lc q))))
	((pointergp (car p) (car q)) (monomgcd (p-lc p) q))
	(t (monomgcd p (p-lc q)))))
	
(defun linhack (pol1 pol2 nonlindeg var cofac?)
  (prog (coeff11 coeff12 gcdab rpol1 rpol2 gcdcd gcdcoef)
     (desetq (coeff11 . coeff12) (bothprodcoef (make-poly var) pol1))
     (setq gcdab (if (pzerop coeff12) coeff11
		     (pgcd coeff11 coeff12)))
     (cond ((equal gcdab 1)
	    (cond ((setq coeff11 (testdivide pol2 pol1))
		   (return (list pol1 1 coeff11)))
		  (t (return (list 1 pol1 pol2))))))
     (setq rpol1 (pquotient pol1 gcdab))
     (desetq (gcdcd rpol2) (linhackcontent var pol2 nonlindeg))
     (cond ((equal gcdcd 1)
	    (cond ((setq coeff12 (testdivide rpol2 rpol1))
		   (return (list rpol1 gcdab coeff12)))
		  (t (return (list 1 pol1 pol2))))))
     (cond (cofac? (desetq (gcdcoef coeff11 coeff12)
			   (pgcdcofacts gcdab gcdcd))
		   (cond ((setq gcdcd (testdivide rpol2 rpol1))
			  (return (list (ptimes gcdcoef rpol1)
					coeff11
					(ptimes coeff12 gcdcd))))
			 (t (return (list gcdcoef 
					  (ptimes coeff11 rpol1)
					  (ptimes coeff12 rpol2))))))
	   (t (setq gcdcoef (pgcd gcdcd gcdab))
	      (cond ((testdivide rpol2 rpol1)
		     (return (list (ptimes gcdcoef rpol1))))
		    (t (return (list gcdcoef))))))))

(defun lin-var-find (a b c)
  (do ((varl c (cdr varl))
       (degl1 a (cdr degl1))
       (degl2 b (cdr degl2)))
      ((or (null degl1) (null degl2)) nil)
    (if (equal (min (car degl1) (car degl2)) 1)
	(return (list (car degl1) (car degl2) (car varl))))))

(defun linhackcontent (var pol nonlindeg &aux (npol pol) coef gcd)
  (do ((i nonlindeg (f1- i)))
      ((f= i 0) (list (setq gcd (pgcd gcd npol)) (pquotient pol gcd)))
    (desetq (coef . npol) (bothprodcoef (make-poly var i 1) npol)) 
    (unless (pzerop coef)
      (setq gcd (if (null gcd) coef (pgcd coef gcd)))
      (if (equal gcd 1) (return (list 1 pol))))))

;;*** THIS IS THE REDUCED POLYNOMIAL REMAINDER SEQUENCE GCD (COLLINS')

(defun oldgcd (x y &aux u v s egcd)	;only called from pgcda
  (desetq (x  u) (oldcontent x))
  (desetq (y  v) (oldcontent y))
  (setq egcd (gcd (pgcdexpon u) (pgcdexpon v)))
  (if (> egcd 1)
      (setq u (pexpon*// u egcd nil)
	    v (pexpon*// v egcd nil)))
  (if (f> (p-le v) (p-le u)) (exch u v))
  (setq s (case $gcd
	    ($red (redgcd u v))
	    ($subres (subresgcd u v))
	    (t (merror "Illegal GCD algorithm"))))
  (unless (equal s 1)
    (setq s (pexpon*// (primpart
			(if $algebraic s
			    (pquotient s (pquotient (p-lc s)
						    (pgcd (p-lc u)
							  (p-lc v))))))
		       egcd t)))
  (setq s (ptimeschk s (pgcd x y)))
  (and $algebraic (not (pcoefp (setq u (leadalgcoef s))))
       (not (equal u s)) (setq s (algnormal s)))
  (cond (modulus (monize s))
	((pminusp s) (pminus s)) 
	(t s)))

(defun pgcdexpon (p)
  (if (pcoefp p) 0
      (do ((d (cadr p) (gcd d (car l)))
	   (l (cdddr p) (cddr l)))
	  ((or (null l) (f= d 1)) d))))

(defun pexpon*// (p n *?)
  (if (or (pcoefp p) (f= n 1)) p
      (do ((ans (list (car p))
		(cons (cadr l)
		      (cons (if *? (f* (car l) n)
				(// (car l) n))
			    ans)))
	   (l (cdr p) (cddr l)))
	  ((null l) (nreverse ans)))))

;;polynomial gcd using reduced prs

(defun redgcd (p q &aux (d 0))
  (loop until (zerop (pdegree q (p-var p)))
	 do (psetq p q
		   q (pquotientchk (prem p q) (pexpt (p-lc p) d))
		   d (f+ (p-le p) 1 (f- (p-le q))))
	 finally (return (if (pzerop q) p 1))))

;;computes gcd's using subresultant prs TOMS Sept. 1978

(defun subresgcd (p q)					
  (loop for g = 1 then (p-lc p)
	 for h = 1 then (pquotient (pexpt g d) h^1-d)
	 for d = (f- (p-le p) (p-le q))
	 for h^1-d = (if (equal h 1) 1 (pexpt h (f1- d)))
	 do (psetq p q
		   q (pquotientchk (prem p q) (ptimes g (ptimes h h^1-d))))
	 if (zerop (pdegree q (p-var p))) return (if (pzerop q) p 1)))

;;*** THIS COMPUTES PSEUDO REMAINDERS

;;(DECLARE (SPECIAL K LCU LCV) (FIXNUM K M I))

(defun psquorem1 (u v quop)
  (prog (k (m 0) lcu lcv quo lc)
     (declare (special k lcu lcv) (fixnum #-cl k m))
     (setq lcv (pt-lc v))
     (setq k (f- (pt-le u) (pt-le v)))
     (cond ((minusp k) (return (list 1 '(0 0) u))))
     (if quop (setq lc (pexpt (pt-lc v) (f1+ k))))
     a     (setq lcu (pminus (pt-lc u)))
     (if quop (setq quo (cons (ptimes (pt-lc u) (pexpt (pt-lc v) k))
			      (cons k quo))))
     (cond ((null (setq u (pgcd2 (pt-red u) (pt-red v))))
	    (return (list lc (nreverse quo) '(0 0))))
	   ((minusp (setq m (f- (pt-le u) (pt-le v))))
	    (setq u (cond ((zerop k) u)
			  (t (pctimes1 (pexpt lcv k) u))))
	    (return (list lc (nreverse quo) u)))
	   ((f> (f1- k) m)
	    (setq u (pctimes1 (pexpt lcv (f- (f1- k) m)) u))))
     (setq k m)
     (go a)))

(defun prem (p q)
  (cond ((pcoefp p) (if (pcoefp q) (cremainder p q) p))
	((pcoefp q) (pzero))
	(t (psimp (p-var p) (pgcd1 (p-terms p) (p-terms q))))))

(defmfun pgcd1 (u v) (caddr (psquorem1 u v nil)))

(defun pgcd2 (u v &aux (i 0))
  (declare (special k lcu lcv) (fixnum #-cl k i))
  (cond ((null u) (pcetimes1 v k lcu))
	((null v) (pctimes1 lcv u))
	((zerop (setq i (f+ (pt-le u) (f- k) (f- (car v)))))
	 (pcoefadd (pt-le u) (pplus (ptimes lcv (pt-lc u))
				    (ptimes lcu (pt-lc v)))
		   (pgcd2 (pt-red u) (pt-red v))))
	((minusp i)
	 (list* (f+ (pt-le v) k) (ptimes lcu (pt-lc v)) (pgcd2 u (pt-red v))))
	(t (list* (pt-le u) (ptimes lcv (pt-lc u)) (pgcd2 (pt-red u) v)))))

;;(DECLARE (UNSPECIAL K LCU LCV) (NOTYPE K M I))

;;;*** OLDCONTENT REMOVES ALL BUT MAIN VARIABLE AND PUTS THAT IN CONTENT
;;;***  OLDCONTENT OF 3*A*X IS 3*A (WITH MAINVAR=X)

(defun rcontent (p)			;RETURNS RAT-FORMS
  (let ((q (oldcontenta p)))
    (list (cons q 1) (cond ($algebraic (rquotient p q))
			   (t (cons (pquotient p q) 1))))))

(defun oldcontenta (x)
  (cond ((pcoefp x) x)
	(t (setq x (contsort (cdr x)))
	   (oldcontent2 (cdr x) (car x)))))

(defmfun oldcontent (x)
  (cond ((pcoefp x) (list x 1))
	((null (p-red x))
	 (list (p-lc x) (make-poly (p-var x) (p-le x) 1)))
	(t (let ((u (contsort (cdr x))) v)
	     (setq u (oldcontent2 (cdr u) (car u))
		   v (cond ($algebraic (car (rquotient x u)))
			   (t (pcquotient x u))))
	     (cond ((pminusp v) (list (pminus u) (pminus v)))
		   (t (list u v)))))))

(defun oldcontent1 (x gcd) (cond ((equal gcd 1) 1)
				 ((null x) gcd)
				 (t (oldcontent2 (contsort x) gcd))))

(defun oldcontent2 (x gcd)
  (do ((x x (cdr x))
       (gcd gcd (pgcd (car x) gcd)))
      ((or (null x) (equal gcd 1)) gcd)))

(defun contsort (x)
  (setq x (coefl x))
  (cond ((zl-member 1 x)'(1))
	((null (cdr x))x)
	(t (sort x (function contodr)))))

(defun coefl (x)
  (do ((x x (cddr x))
       (ans nil (cons (cadr x) ans)))
      ((null x) ans)))

(defun contodr (a b)
  (cond ((pcoefp a) t)
	((pcoefp b) nil)
	((eq (car a) (car b)) (not (f> (cadr a) (cadr b))))
	(t (pointergp (car b)(car a)))))

;;;*** PCONTENT COMPUTES INTEGER CONTENT
;;;*** PCONTENT OF 3*A*X IS 3 IF MODULUS = NIL  1 OTHERWISE

(defun pcontent (x)
  (cond ((pcoefp x) (list x 1))
	(t (let ((u (pcontentz x)))
	     (if (eqn u 1) (list 1 x)
		 (list u (pcquotient x u)))))))

(defun pcontent1 (x gcd)
  (do ((x x (cddr x))
       (gcd gcd (cgcd gcd (pcontentz (cadr x)))))
      ((or (null x) (equal gcd 1)) gcd)))

(defun pcontentz (p)
  (cond ((pcoefp p) p)
	(t (pcontent1 (p-red p) (pcontentz (p-lc p))))))

(defun ucontent (p)			;CONTENT OF UNIV. POLY
  (cond ((pcoefp p) (abs p))
	(t (setq p (mapcar #'abs (coefl (cdr p))))
	   (let ((m (apply #'min p)))
	     (oldcontent2 (zl-delete m p) m)))))

;;***	PGCDU CORRESPONDS TO BROWN'S ALGORITHM U

;;;PGCDU IS NOT NOW IN RAT;UFACT >

(defmfun pgcdu (p q)
  (do () ((pzerop q) (monize p))
    (psetq p q q (pmodrem p q))))

;;(DECLARE (SPECIAL K Q* QUO) (FIXNUM K))

(defun pmodrem (x y)
  (cond ((null modulus)
	 (merror "Illegal use of PMODREM"))
	((pacoefp y) (if (pzerop y) x 0))
	((pacoefp x) x)
	((eq (p-var x) (p-var y))
	 (psimp (car x) (pgcdu1 (p-terms x) (p-terms y) nil)))
	(t (merror "Illegal use of PMODREM"))))

(defun pmodquo (u v &aux quo)
  (declare (special quo))
  (cond ((null modulus)
	 (merror "Illegal use of PMODQUO"))
	((pcoefp v) (cons (ptimes (crecip v) u) 0))
	((alg v) (cons (ptimes (painvmod v) u) 0))
	((pacoefp u) (cons 0 u))
	((not (eq (p-var u) (p-var v)))
	 (merror "Illegal use of PMODQUO"))
	(t (xcons (psimp (car u) (pgcdu1 (cdr u) (cdr v) t))
		  (psimp (car u) quo)))))

(comment (defun pmodrem (x y) (cond ((null modulus)
				     (merror "Illegal use of PMODREM"))
				    ((pacoefp y) 0)
				    ((pacoefp x) x)
				    ((eq (car x) (car y))
				     (dpdisrep
				      (dpmodrem (dprep x) (dprep y))))
				    (t (merror "Illegal use of PMODREM")))))

(defun pgcdu1 (u v pquo*)
  (let ((invv (painvmod (pt-lc v))) (k 0) q*)
    (declare (special k quo q*) (fixnum k))
    (loop until (minusp (setq k (f- (pt-le u) (pt-le v))))
	   do (setq q* (ptimes invv (pt-lc u)))
	   if pquo* do (setq quo (nconc quo (list k q*)))
	   when (ptzerop (setq u (pquotient2 (pt-red u) (pt-red v))))
	   return (ptzero)
	   finally (return u))))

;;(DECLARE (UNSPECIAL K Q* QUO) (NOTYPE K))

(defun newprime (p)
  (declare (special bigprimes))		;defined later on
  (cond ((null p) (car bigprimes))
	(t (do ((pl bigprimes (cdr pl)))
	       ((null pl) (setq p (fnewprime p))
		(setq bigprimes (nconc bigprimes (list p)))
		p)
	     (if (f< (car pl) p) (return (car pl)))))))

(defun fnewprime (p)	     ; Finds biggest prime less than fixnum P.
  (do ((pp (if (oddp p) (f- p 2) (f- p 1)) (f- pp 2))) ((f< pp 0))
    (if (primep pp) (return pp))))

(defun primep (p)
  (and (or (lessp p 14.)
	   (let ((modulus p))
	     (and (equal 1 (cexpt 13. (sub1 p))) (equal 1 (cexpt 3 (sub1 p))))))
       (null (cddr (setq p (cfactorw p))))
       (= 1 (cadr p))))

;; #O <form> reads <form> in octal (base 8)


(defvar bigprimes nil)

(eval-when (load)
 
  ;; it is convenient to have the bigprimes be actually less than
  ;; half the size of the most positive fixnum, so that arithmetic is
  ;; easier
  #.(case most-positive-fixnum
      (2147483647
       '(setq bigprimes
	 '(1073741789 1073741783 1073741741 1073741723 1073741719 1073741717
	   1073741689 1073741671 1073741663 1073741651 1073741621 1073741567
	   1073741561 1073741527 1073741503 1073741477 1073741467 1073741441
	   1073741419 1073741399)
	 ))
      ;; Could always use the following, but it takes several seconds to compute
      ;; so if we want to autoload this file, it is tiresome.
      (t '(do ((i 0 (f1+ i))	   ;GENERATES 20 LARGEST PRIMES < WORD
	       (p (quotient most-positive-fixnum 2) (newprime p)))
	   ((= i 20.)))))

  (setq *alpha (car bigprimes))
  )

(defmvar *alpha (car bigprimes))

;;#+MacLisp 
;;(DEFMVAR BIGPRIMES
;;      '(#O 377777777741 #O 377777777717 #O 377777777703 #O 377777777673
;;	#O 377777777661 #O 377777777607 #O 377777777563 #O 377777777411
;;	#O 377777777313 #O 377777777273 #O 377777777233 #O 377777777075
;;	#O 377777777015 #O 377777776771 #O 377777776755 #O 377777776735
;;	#O 377777776725 #O 377777776677 #O 377777776661 #O 377777776653))


;;;; list of primes less than 2^30 -1  (limit of smallnums in Franz/vax)
;;#+Franz 
;;(defmvar bigprimes '(1073741789. 1073741783. 1073741741. 1073741723. 
;;		  1073741719. 1073741717. 1073741689. 1073741671.
;;		  1073741663. 1073741651. 1073741621. 1073741567. 
;;		  1073741561. 1073741527. 1073741503. 1073741477.
;;		  1073741467. 1073741441. 1073741419. 1073741399.))

;;#+NIL
;;(PROGN 'COMPILE
;;;; It takes a lot longer to compute the 35 bit primes for the PDP-10,
;;;; thats why they are wired-in there.
;;(DEFMVAR BIGPRIMES
;;  '(#o3777777775 #o3777777737 #o3777777725 #o3777777701 #o3777777667
;;    #o3777777665 #o3777777643 #o3777777635 #o3777777607 #o3777777573
;;    #o3777777557 #o3777777527 #o3777777511 #o3777777503 #o3777777475
;;    #o3777777455 #o3777777433 #o3777777401 #o3777777361 #o3777777343))

;;(DEFUN BIGPRIMES ()
;;  (SETQ BIGPRIMES ())
;;  (DO ((I 0 (f1+ I)) (P MOST-POSITIVE-FIXNUM (NEWPRIME P)))
;;      ((= I 20.))))
;;; wire it in for now. PRIMEP is losing. (BIGPRIMES)
;;)

;;#+CL
;;(DO ((I 0 (f1+ I))				;GENERATES 20 LARGEST
;;	     (P (LSH -1 -1) (NEWPRIME P)))		;PRIMES < WORD
;;	    ((= I 20.)))



(defmfun $primep (p)
  (if (not (integerp p))
      (merror "Argument to PRIMEP must be an integer:~%~M" p))
  (let ($intfaclim) (primep (abs p))))


(defun leadcoefficient (p) (if (pcoefp p) p (leadcoefficient (caddr p))))

(defun maxcoefficient (p) (if (pcoefp p) (abs p) (maxcoef1 (cdr p))))

(defun maxcoef1 (p)
  (if (null p) 0 (max (maxcoefficient (cadr p)) (maxcoef1 (cddr p)))))

(defun maxnorm (poly)
  (if (null poly) 0 (max (norm (cadr poly)) (maxnorm (cddr poly)))))

(defun norm (poly)
  (cond ((null poly) 0)
	((pcoefp poly) (abs poly))
	(t (plus (norm (caddr poly)) (norm1 (cdddr poly)) )) ))

(defun norm1 (poly)
  (if (null poly) 0 (plus (norm (cadr poly)) (norm1 (cddr poly)) )) )

(defmfun pdegree (p var)
  (cond ((pcoefp p) 0)
	((eq var (p-var p)) (p-le p))
	((pointergp var (p-var p)) 0)
	(t (do ((l (p-red p) (pt-red l))
		(e (pdegree (p-lc p) var) (max e (pdegree (pt-lc l) var))))
	       ((null l) e)))))

(defun poly-in-var (p v)
  (cond ((or (pcoefp p) (pointergp v (p-var p))) (list 0 p))
	((eq (p-var p) v) (p-terms p))
	((loop with ans
		for (exp coef) on (p-terms p) by #'pt-red
		do (setq ans (pplus1 ans
				     (everysubst2 (poly-in-var coef v)
						  (list (p-var p) exp 1))))
		finally (return ans)))))

(defun univar (x) (or (null x) (and (pcoefp (pt-lc x)) (univar (pt-red x)))))

;;**THE CHINESE REMAINDER ALGORITHM IS A SPECIAL CASE OF LAGRANGE INTERPOLATION

(defun lagrange3 (u uk p qk)
  (setqmodulus p)
  (setq uk (pdifference uk (pmod u)))
  (cond ((pzerop uk) (setq modulus nil) u)
	(t (setq uk (pctimes (crecip (cmod qk)) uk))
	   (setq modulus nil)
	   (pplus u (pctimes qk uk)))))


(defun lagrange33 (u uk qk xk)
  (declare (special xv))
  (setq uk (pdifference uk (pcsubst u xk xv)))
  (cond ((pzerop uk) u)
	(t (pplus u (ptimes
		     (pctimes (crecip (pcsubst qk xk xv)) uk)
		     qk)))))


;;;*************************************************************
 
;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES AND THEIR SUPPORTING FUNCTIONS
