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
(macsyma-module rat3e)

;;	This is the rational function package part 5.
;;	It includes the conversion and top-level routines used
;;	by the rest of the functions.

(declare-top(*lexpr outermap1 $divide $content $gcd $rat $ratsimp $factor factor)
	    (*expr $float)
	    (special intbs* alflag var dosimp alc $myoptions trunclist
		     vlist scanmapp radlist expsumsplit *ratsimp* mplc*
		     $ratsimpexpons $expop $expon $negdistrib $gcd))

(load-macsyma-macros rzmac ratmac)

(declare-top(genprefix a_5))

(defmvar genvar nil
  "List of gensyms used to point to kernels from within polynomials.
	 The values cell and property lists of these symbols are used to
	 store various information.")
(defmvar genpairs nil)
(defmvar varlist nil "List of kernels")
(defmvar *fnewvarsw nil)
(defmvar *ratweights nil)
(defvar *ratsimp* nil)
(defmvar factorresimp nil "If T resimplifies FACTOR(X-Y) to X-Y")

;; User level global variables.

(defmvar $keepfloat nil
  "If t floating point coeffs are not converted to rationals")
(defmvar $factorflag nil "If t constant factor of polynomial is also factored")
(defmvar $dontfactor '((mlist)))
(defmvar $norepeat t)
(defmvar $ratweights '((mlist simp)))

(defmvar $ratfac nil "If t cre-forms are kept factored")
(defmvar $algebraic nil)
(defmvar $ratvars '((mlist simp)))
(defmvar $facexpand t)

;; Constants required for Franz
#+franz
(progn 'compile
       (defvar two30f	(expt 2.0 30.))
       (defvar two30 	(expt 2. 30.))
       (defvar two53f	(expt 2.0 53.))
       (defvar two53 	(expt 2. 53.)))

(declare-top(special evp $infeval))

(defmfun mrateval (x)
  (let ((varlist (caddar x)))
    (cond ((and evp $infeval) (meval ($ratdisrep x)))
	  ((or evp
	       (and $float $keepfloat)
	       (not (alike varlist (mapcar #'meval varlist))))
	   (ratf (meval ($ratdisrep x))))
	  (t x))))

;;(DEFPROP MRAT (LAMBDA (X) (MRATEVAL X)) MFEXPR*)
(defprop mrat mrateval mfexpr*)

(defmfun $ratnumer (x)
  (setq x (taychk2rat x)) (cons (car x) (cons (cadr x) 1)))

(defmfun $ratdenom (x)
  (setq x (taychk2rat x)) (cons (car x) (cons (cddr x) 1)))

(defun taychk2rat (x)
  (cond ((and ($ratp x) (memq 'trunc (cdar x))) ($taytorat x)) (t (ratf x))))


(defmvar tellratlist nil)

(defun tellratdisp (x)
  (pdisrep+ (trdisp1 (cdr x) (car x))))

(defun trdisp1 (p var)
  (cond ((null p) nil)
	(t (cons (pdisrep* (if (mtimesp (cadr p)) (copy1 (cadr p))
			       (cadr p)) ;prevents clobbering p
			   (pdisrep! (car p) var))
		 (trdisp1 (cddr p) var)))))

(defmfun $untellrat n
  (dolist (x (listify n))
    (if (setq x (assol x tellratlist))
	(setq tellratlist (zl-remove x tellratlist))))
  (cons '(mlist) (mapcar 'tellratdisp tellratlist)))

#+cl
(defmfun $tellrat (&rest narg-rest-argument &aux
			 #+lispm (default-cons-area working-storage-area )
			 (narg (length narg-rest-argument)) n)
  (setq n narg)
  (do ((i 1 (f1+ i))) ((f> i n)) (tellrat1 (narg-arg i)))
  (or (= n 0) (add2lnc 'tellratlist $myoptions))
  (cons '(mlist) (mapcar 'tellratdisp tellratlist)))

#-cl
(defmfun $tellrat n
  (do ((i 1 (f1+ i))) ((f> i n)) (tellrat1 (arg i)))
  (or (= n 0) (add2lnc 'tellratlist $myoptions))
  (cons '(mlist) (mapcar 'tellratdisp tellratlist)))

(defun tellrat1 (x &aux varlist genvar $algebraic $ratfac algvar)
  (setq x ($totaldisrep x))
  (and (not (atom x)) (eq (caar x) 'mequal)
       (newvar (cadr x)))
  (newvar (setq x (meqhk x)))
  (or varlist (merror "Improper polynomial"))
  (setq algvar (car (last varlist)))
  (setq x (p-terms (primpart (cadr (ratrep* x)))))
  (if (not (equal (pt-lc x) 1)) (merror "Minimal polynomial must be monic"))
  (do ((p (pt-red x) (pt-red p))) ((ptzerop p)) (setf (pt-lc p) (pdis (pt-lc p))))
  (setq algvar (cons algvar x))
  (if (setq x (assol (car algvar) tellratlist))
      (setq tellratlist (zl-remove x tellratlist)))
  (push algvar tellratlist))


(defmfun $printvarlist () (cons '(mlist) (copy varlist)))

;;(DEFMFUN $SHOWRATVARS (E)
;;  (CONS '(MLIST SIMP)
;;	(IF ($RATP E) (CADDAR E)
;;	    (LET (VARLIST)
;;	      (LNEWVAR E) 
;;	      VARLIST))))
;;Update from F302 --gsb
(defmfun $showratvars (e)
  (cons '(mlist simp)
	(cond (($ratp e)
	       (if (memq 'trunc (cdar e)) (setq e ($taytorat e)))
	       (caddar (minimize-varlist e)))
	      (t (let (varlist) (lnewvar e) varlist)))))

(defmfun $ratvars n
  (add2lnc '$ratvars $myoptions)
  (setq $ratvars
	(cons '(mlist simp) (setq varlist (mapfr1 (listify n) varlist)))))

(defun mapfr1 (l varlist) (mapcar #'(lambda (z) (fr1 z varlist)) l))

(defmvar inratsimp nil)

(defmfun $fullratsimp n
  (if (= n 0) (wna-err '$fullratsimp))
  (prog (exp exp1 argl)
     (setq exp (arg 1) argl (cdr (listify n)))
     loop (setq exp1 (simplify (apply #'$ratsimp (cons exp argl))))
     (cond ((alike1 exp exp1) (return exp)))
     (setq exp exp1)
     (go loop)))

(defun fullratsimp (l)
  (let (($expop 0) ($expon 0) (inratsimp t) $ratsimpexpons)
    (setq l ($totaldisrep l)) (fr1 l varlist))) 

(defmfun $totaldisrep (l)
  (cond ((atom l) l)
	((not (among 'mrat l)) l)
	((eq (caar l) 'mrat) (ratdisrep l))
	(t (cons (delq 'ratsimp (car l)) (mapcar '$totaldisrep (cdr l))))))

;;;VARLIST HAS MAIN VARIABLE AT END

(defun joinvarlist (cdrl)
  (mapc #'(lambda (z) (if (not (memalike z varlist))
			  (setq varlist (cons z varlist))))
	(reverse (mapfr1 cdrl nil))))

(defmfun $rat n
  (if (f= n 0) (wna-err '$rat))
  (cond ((f> n 1)
	 (let (varlist) (joinvarlist (cdr (listify n)))
	      (lnewvar (arg 1))
	      (rat0 (arg 1))))
	(t (lnewvar (arg 1)) (rat0 (arg 1)))))

(defun rat0 (exp)			;SIMP FLAGS?
  (if (mbagp exp) (cons (car exp) (mapcar #'rat0 (cdr exp))) (ratf exp)))

(defmfun $ratsimp n
  (if (f= n 0) (wna-err '$ratsimp))
  (cond ((f> n 1)
	 (let (varlist) (joinvarlist (cdr (listify n)))
	      (fullratsimp (arg 1))))
	(t (fullratsimp (arg 1)))))

;; $RATSIMP, $FULLRATSIMP, and $RAT are LEXPRs to allow for optional extra 
;; arguments specifying the VARLIST.

;;;PSQFR HAS NOT BEEN CHANGED TO MAKE USE OF THE SQFR FLAGS YET

(defmfun $sqfr (x)
  (let ((varlist (cdr $ratvars)) genvar $keepfloat $ratfac)
    (sublis '((factored . sqfred) (irreducible . sqfr))
	    (ffactor x (function psqfr)))))

(declare-top(special fn cargs))

(defun whichfn (p)
  (cond ((and (mexptp p) (integerp (caddr p)))
	 (list '(mexpt) (whichfn (cadr p)) (caddr p)))
	((mtimesp p)
	 (cons '(mtimes) (mapcar (function whichfn) (cdr p))))
	(fn (ffactor p (function pfactor)))
	(t (factoralg p))))

(declare-top(special var))

(defmvar adn* 1 "common denom for algebraic coefficients")

(defun factoralg (p) 
  (prog (alc ans adn* $gcd)
     (setq $gcd '$algebraic)
     (cond((or (atom p) (numberp p))(return p)))
     (setq adn* 1)
     (cond ((and (not $nalgfac) (not intbs*))
	    (setq intbs* (findibase minpoly*))))
     (setq algfac* t)
     (setq ans (ffactor p (function pfactor)))
     (cond ((eq (caar ans) 'mplus)(return p))
	   (mplc* (setq ans (albk ans))))
     (if (and (not alc) (equal  1 adn*)) (return ans))
     (setq ans (partition ans (car (last varlist)) 1))
     (return (mul (let ((dosimp t))
		    (mul `((rat) 1 ,adn*)
			 (car ans)
			 (if alc (pdis alc) 1)))
		  (cdr ans)))))

(defun albk (p)				;to undo monicizing subst 
  (let ((alpha (pdis alpha)) ($ratfac t))
    (declare (special alpha))
    ;;      (sratsimp    ;; don't multiply them back out
    (maxima-substitute (list '(mtimes simp) mplc* alpha) ;assumes mplc* is int
		       alpha p)))


(defmfun $gfactor (p &aux (gauss t)) 
  (if ($ratp p) (setq p ($ratdisrep p)))
  (setq p ($factor (subst '%i '$%i p)
		   '((mplus) 1 ((mexpt) %i 2))))
  (setq p (sublis '((factored . gfactored)
		    (irreducible . irreducibleg))
		  p))
  (let (($expop 0) ($expon 0) $negdistrib) (maxima-substitute '$%i '%i p)))


;; (DEFMFUN $FACTOR (EXP &OPTIONAL MINIMUM-POLYNOMIAL) ...)

(defmfun $factor nargs
  (unless (or (f= nargs 1) (f= nargs 2)) (wna-err '$factor))
  (let ($intfaclim (varlist (cdr $ratvars)) genvar ans)
    (setq ans (apply #'factor (listify nargs)))
    (if (and factorresimp $negdistrib
	     (mtimesp ans) (null (cdddr ans))
	     (equal (cadr ans) -1) (mplusp (caddr ans)))
	(let (($expop 0) ($expon 0)) ($multthru ans))
	ans)))

#+cl (defvar alpha nil)

(defmfun factor nargs
  ((lambda (tellratlist varlist genvar $gcd $negdistrib)
     (prog (fn var mm* mplc* intbs* alflag minpoly* alpha p algfac* 
	    $keepfloat $algebraic cargs)
	(or (memq $gcd *gcdl*) (setq $gcd (car *gcdl*)))
	(let  ($ratfac)
	  (setq p (arg 1) mm* 1 cargs (cdr (listify nargs)))
	  (and (eq (ml-typep p)  'symbol) (go out))
	  (and ($numberp p) (go num))
	  (cond ((mbagp p)
		 (return (cons (car p)
			       (mapcar #'(lambda (x) (apply 'factor (cons x cargs)))
				       (cdr p))))))
	  (cond ((f= nargs 2)
		 (setq alpha (meqhk (arg 2)))
		 (newvar alpha)
		 (setq minpoly* (cadr (ratrep* alpha)))
		 (if (or (pcoefp minpoly*)
			 (not (univar (cdr minpoly*)))
			 (f< (cadr minpoly*) 2))
		     (merror
		      "The second argument to FACTOR must be a non-linear, univariate polynomial:~%~M"
		      alpha))
		 (setq alpha (pdis (list (car minpoly*) 1 1)) 
		       mm* (cadr minpoly*))
		 (cond ((not (equal (caddr minpoly*) 1))
			(setq mplc* (caddr minpoly*))
			(setq minpoly* (pmonz minpoly*))
			(setq p (maxima-substitute (div alpha mplc*) alpha p)) ))
		 (setq $algebraic t)
		 ($tellrat(pdis minpoly*))
		 (setq algfac* t))
		(t (setq fn t)))
	  (if (not scanmapp) (setq p (let (($ratfac t)) (sratsimp p))))
	  (newvar p)
	  (and (eq (ml-typep p)  'symbol) (go out))
	  (cond ((numberp p) (go num)))
	  (setq $negdistrib nil)
	  (setq p (let ($factorflag ($ratexpand $facexpand)) (whichfn p))))
								 
	(setq p (let (($expop 0) ($expon 0)) (simplify p)))
	(cond ((mnump p) (return (factornumber p)))
	      ((atom p) (go out)))
	(and $ratfac (not $factorflag) ($ratp (arg 1)) (return ($rat p)))
	(and $factorflag (mtimesp p) (mnump (cadr p))
	     (setq alpha (factornumber (cadr p)))
	     (cond ((alike1 alpha (cadr p)))
		   ((mtimesp alpha)
		    (setq p (cons (car p) (append (cdr alpha) (cddr p)))))
		   (t (setq p (cons (car p) (cons alpha (cddr p)))))))
	(and (null (memq 'factored (car p)))
	     (setq p (cons (append (car p) '(factored)) (cdr p))))
	out  (return p)
	num (return (let (($factorflag (not scanmapp))) (factornumber p)))))
   nil varlist nil $gcd $negdistrib))


(defun factornumber (n)
  (setq n (nretfactor1 (nratfact (cdr ($rat n)))))
  (cond ((cdr n) (cons '(mtimes simp factored)
		       (cond ((equal (car n) -1)
			      (cons (car n) (nreverse (cdr n))))
			     (t (nreverse n)))))
	((atom (car n)) (car n))
	(t (cons (cons (caaar n) '(simp factored)) (cdar n)))))

(defun nratfact (x)
  (cond ((equal (cdr x) 1) (cfactor (car x)))
	((equal (car x) 1) (revsign (cfactor (cdr x))))
	(t (nconc (cfactor (car x)) (revsign (cfactor (cdr x)))))))

;;; FOR LISTS OF JUST NUMBERS
(defun nretfactor1 (l)
  (cond ((null l) nil)
	((equal (cadr l) 1) (cons (car l) (nretfactor1 (cddr l))))
	(t (cons (cond ((equal (cadr l) -1)
			(list '(rat simp) 1 (car l)))
		       (t (list '(mexpt simp) (car l) (cadr l))))
		 (nretfactor1 (cddr l))))))

(declare-top(unspecial var))


(defmfun $mod nargs
  (if (not (or (f= nargs 1) (f= nargs 2))) (wna-err '$mod))
  (let ((modulus modulus))
    (cond ((f= nargs 2)
	   (setq modulus (arg 2))
	   (if (or (not (integerp modulus)) (zerop modulus))
	       (merror "Improper value for MODULUS:~%~M" modulus))))
    (if (minusp modulus) (setq modulus (abs modulus)))
    (mod1 (arg 1))))

(defun mod1 (e)
  (if (mbagp e) (cons (car e) (mapcar 'mod1 (cdr e)))
      (let (formflag)
	(newvar e)
	(setq formflag ($ratp e) e (ratrep* e))
	(setq e (cons (car e) (ratreduce (pmod (cadr e)) (pmod (cddr e)))))
	(cond (formflag e) (t (ratdisrep e))))))

(defmfun $divide nargs
  (prog (x y h varlist tt ty formflag $ratfac)
     (if (f< nargs 2) (merror "DIVIDE needs at least two arguments."))
     (setq x (arg 1))
     (if (and ($ratp x) (setq formflag t) (integerp (cadr x)) (equal (cddr x) 1))
	 (setq x (cadr x)))
     (setq y (arg 2))
     (if (and ($ratp y) (setq formflag t) (integerp (cadr y)) (equal (cddr y) 1))
	 (setq y (cadr y)))
     (if (and (integerp x) (integerp y))
	 (return (list '(mlist) (*quo x y) (remainder x y))))
     (setq varlist (cddr (listify nargs)))
     (mapc #'newvar (reverse (cdr $ratvars)))
     (newvar y)
     (newvar x)
     (setq x (ratrep* x))
     (setq h (car x))
     (setq x (cdr x))
     (setq y (cdr (ratrep* y)))
     (cond ((and (eqn (setq tt (cdr x)) 1) (eqn (cdr y) 1)) 
	    (setq x (pdivide (car x) (car y))))
	   (t (setq ty (cdr y))
	      (setq x (ptimes (car x) (cdr y)))
	      (setq x (pdivide x (car y))) 
	      (setq x (list
		       (ratqu (car x) tt)
		       (ratqu (cadr x) (ptimes tt ty))))))
     (setq h (list (quote (mlist)) (cons h (car x)) (cons h (cadr x))))
     (return (if formflag h ($totaldisrep h)))))

(defmfun $quotient nargs (cadr (apply '$divide (listify nargs))))

(defmfun $remainder nargs (caddr (apply '$divide (listify nargs))))


(defmfun $gcd nargs
  (prog (x y h varlist genvar $keepfloat formflag)
     (if (f< nargs 2) (merror "GCD needs 2 arguments"))
     (setq formflag ($ratp (setq x (arg 1))))
     (setq y (arg 2))
     (and ($ratp y) (setq formflag t))
     (setq varlist (cddr (listify nargs)))
     (dolist (v varlist) (if (numberp v) (improper-arg-err v '$gcd)))
     (newvar x)
     (newvar y)
     (when (and ($ratp x) ($ratp y) (equal (car x) (car y)))
       (setq genvar (car (last (car x))) h (car x) x (cdr x) y (cdr y))
       (go on))
     (setq x (ratrep* x))
     (setq h (car x))
     (setq x (cdr x))
     (setq y (cdr (ratrep* y)))
     on	(setq x (cons (pgcd (car x) (car y)) (plcm (cdr x) (cdr y))))
     (setq h (cons h x))
     (return (if formflag h (ratdisrep h)))))

(defmfun $content nargs
  (prog (x y h varlist formflag)
     (setq formflag ($ratp (setq x (arg 1))))
     (setq varlist (cdr (listify nargs)))
     (newvar x)
     (desetq (h x . y) (ratrep* x))
     (unless (atom x)
       ;; (CAR X) => gensym corresponding to apparent main var.
       ;; MAIN-GENVAR => gensym corresponding to the genuine main var.
       (let ((main-genvar (nth (1- (length varlist)) genvar)))
	 (unless (eq (car x) main-genvar)
	   (setq x `(,main-genvar 0 ,x)))))
     (setq x (rcontent x)
	   y (cons 1 y))
     (setq h (list '(mlist)
		   (cons h (rattimes (car x) y nil))
		   (cons h (cadr x))))
     (return (if formflag h ($totaldisrep h)))))

(defmfun pget (gen) (cons gen '(1 1)))

(defun m$exp? (x) (and (mexptp x) (eq (cadr x) '$%e)))

(defun algp ($x) (algpchk $x nil))

(defun algpget ($x) (algpchk $x t))

(defun algpchk ($x mpflag &aux temp)
  (cond ((eq $x '$%i) '(2 -1))
	((eq $x '$%phi) '(2 1 1 -1 0 -1))
	((radfunp $x nil)
	 (if (not mpflag) t
	     (let ((r (prep1 (cadr $x))))
	       (cond ((onep1 (cdr r))	;INTEGRAL ALG. QUANT.?
		      (list (caddr (caddr $x))
			    (car r)))
		     (*ratsimp* (setq radlist (cons $x radlist)) nil)))))
	((not $algebraic) nil)
	((and (m$exp? $x) (mtimesp (setq temp (caddr $x)))
	      (equal (cddr temp) '($%i $%pi))
	      (ratnump (setq temp (cadr temp))))
	 (if mpflag (primcyclo (f* 2 (caddr temp))) t))
	((not mpflag) (assolike $x tellratlist))
	((setq temp (copy1 (assolike $x tellratlist)))
	 (do ((p temp (cddr p))) ((null p))
	   (rplaca (cdr p) (car (prep1 (cadr p)))))
	 (setq temp
	       (cond ((ptzerop (pt-red temp)) (list (pt-le temp) (pzero)))
		     ((zerop (pt-le (pt-red temp)))
		      (list (pt-le temp) (pminus (pt-lc (pt-red temp)))))
		     (t temp)))
	 (if (and (f= (pt-le temp) 1) (setq $x (assol $x genpairs)))
	     (rplacd $x (cons (cadr temp) 1)))
	 temp)))

(defun radfunp (x funcflag) ;FUNCFLAG -> TEST FOR ALG FUNCTION NOT NUMBER
  (cond ((atom x) nil)
	((not (eq (caar x) 'mexpt)) nil)
	((not (ratnump (caddr x))) nil)
	(funcflag (not (numberp (cadr x))))
	(t t)))

(defmfun ratsetup (vl gl) (ratsetup1 vl gl) (ratsetup2 vl gl))

(defun ratsetup1 (vl gl)
  (and $ratwtlvl
       (mapc #'(lambda (v g) 
		 (setq v (assolike v *ratweights))
		 (if v (putprop g v '$ratweight) (remprop g '$ratweight)))
	     vl gl)))

(defun ratsetup2 (vl gl)
  (when $algebraic
    (mapc #'(lambda (g) (remprop g 'algord)) gl)
    (mapl #'(lambda (v lg)
	      (cond ((setq v (algpget (car v)))
		     (algordset v lg) (putprop (car lg) v 'tellrat))
		    (t (remprop (car lg) 'tellrat))))
	  vl gl))
  (and $ratfac (let ($ratfac)
		 (mapc #'(lambda (v g) 
			   (if (mplusp v)
			       (putprop g (car (prep1 v)) 'unhacked)
			       (remprop g 'unhacked)))
		       vl gl))))

(defun porder (p) (if (pcoefp p) 0 (valget (car p))))

(defun algordset (x gl)
  (do ((p x (cddr p))
       (mv 0))
      ((null p)
       (do ((l gl (cdr l))) ((or (null l) (f> (valget (car l)) mv)))
	 (putprop (car l) t 'algord)))
    (setq mv (max mv (porder (cadr p))))))

 
#+cl
(defun gensym-readable (name &aux #+lispm
			(default-cons-area working-storage-area ))
  (cond ((symbolp name)(gensym (string-trim "$" (string name))))
	(t  (setq name (aformat nil "~:M" name))
	    (cond (name (gensym name))
		  (t (gensym))))))

#+cl
(defun orderpointer (l)
  (loop for v in l
	 for i below (f- (length l) (length genvar))
	 collecting  (gensym-readable v) into tem
	 finally (setq genvar (nconc tem genvar)) (return (prenumber genvar 1))))
#-cl
(defun orderpointer (l)
  (creatsym (f- (length l) (length genvar)))
  (prenumber genvar 1))

(defun creatsym (n)
  #+lispm (let ((default-cons-area working-storage-area))
	    (cond ((f> n 0) (setq genvar (cons (gensym) genvar))
		   (creatsym (sub1 n)))))
  #-lispm   (cond ((f> n 0) (setq genvar (cons (gensym) genvar))
		   (creatsym (sub1 n)))))

(defun prenumber (v n)
  (do ((vl v (cdr vl))
       (i n (f1+ i)))
      ((null vl) nil)
    (set (car vl) i)))

(defun rget (genv)
  (cons (if (and $ratwtlvl
		 (or (fixnump $ratwtlvl) 
		     (merror "Illegal value for RATWTLVL:~%~M" $ratwtlvl))
		 (f> (or (get genv '$ratweight) -1) $ratwtlvl))
	    (pzero)
	    (pget genv))
	1))

(defmfun ratrep (x varl) (setq varlist varl) (ratrep* x))

(defmfun ratrep* (x) 
  (let (genpairs)
    (orderpointer varlist)
    (ratsetup1 varlist genvar)
    (mapc #'(lambda (y z) (push (cons y (rget z)) genpairs))
	  varlist genvar)
    (ratsetup2 varlist genvar)
    (xcons (prep1 x)			; PREP1 changes VARLIST
	   (list* 'mrat 'simp varlist genvar ;    when $RATFAC is T.
		  (if (and (not (atom x)) (memq 'irreducible (cdar x)))
		      '(irreducible))))))
 
(defvar *withinratf* nil)

(defmfun ratf (l)
  (prog (u *withinratf*)
     (setq *withinratf* t)
     (when (eq '%% (catch 'ratf (newvar l)))
       (setq *withinratf* nil) (return (srf l)))
     (setq u (catch 'ratf (ratrep* l)))	; for truncation routines
     (return (or u (prog2 (setq *withinratf* nil) (srf l))))))


(defun prep1 (x &aux temp) 
  (cond ((floatp x)
	 (cond ($keepfloat (cons x 1.0)) ((prepfloat x))))
	((integerp x) (cons (cmod x) 1))
	#+cl ((rationalp x)
	      (cond ((null modulus)(cons  (numerator x) (denominator x)))
		    (t (cquotient (numerator x) (denominator x)))))
	((atom x)(cond ((assolike x genpairs)) (t (newsym x))))
	((and $ratfac (assolike x genpairs)))
	((eq (caar x) 'mplus)
	 (cond ($ratfac
		(setq x (mapcar 'prep1 (cdr x)))
		(cond ((andmapc 'frpoly? x)
		       (cons (mfacpplus (mapl #'(lambda (x)
						  (rplaca x (caar x)))
					      x)) 
			     1))
		      (t (do ((a (car x) (facrplus a (car l)))
			      (l (cdr x) (cdr l)))
			     ((null l) a)))))
	       (t (do ((a (prep1 (cadr x)) (ratplus a (prep1 (car l))))
		       (l (cddr x) (cdr l)))
		      ((null l) a)))))
	((eq (caar x) 'mtimes)
	 (do ((a (savefactors (prep1 (cadr x)))
		 (rattimes a (savefactors (prep1 (car l))) sw))
	      (l (cddr x) (cdr l))
	      (sw (not (and $norepeat (memq 'ratsimp (cdar x))))))
	     ((null l) a)))
	((eq (caar x) 'mexpt)
	 (newvarmexpt x (caddr x) t))
	((eq (caar x) 'mquotient)
	 (ratquotient (savefactors (prep1 (cadr x)))
		      (savefactors (prep1 (caddr x)))))
	((eq (caar x) 'mminus)
	 (ratminus (prep1 (cadr x))))
	((eq (caar x) 'rat)
	 (cond (modulus (cons (cquotient (cmod (cadr x)) (cmod (caddr x))) 1))
	       (t (cons (cadr x) (caddr x)))))
	((eq (caar x) 'bigfloat)(bigfloat2rat x))
	((eq (caar x) 'mrat)
	 (cond ((and *withinratf* (memq 'trunc (car x)))
		(throw 'ratf nil))
	       ((catch 'compatvl
		  (progn (setq temp (compatvarl (caddar x)
						varlist
						(cadddr (car x))
						genvar))
			 t))
		(cond ((memq 'trunc (car x))
		       (cdr ($taytorat x)))
		      ((and (not $keepfloat)
			    (or (pfloatp (cadr x)) (pfloatp (cddr x))))
		       (cdr (ratrep* ($ratdisrep x))))
		      ((sublis temp (cdr x)))))
	       (t (cdr (ratrep* ($ratdisrep x))))))
	((assolike x genpairs))
	(t (setq x (littlefr1 x))
	   (cond ((assolike x genpairs))
		 (t (newsym x))))))


(defun putonvlist (x)
  (push x vlist)
  (and $algebraic
       (setq x (assolike x tellratlist))
       (mapc 'newvar1 x)))

(setq expsumsplit t)		    ;CONTROLS SPLITTING SUMS IN EXPONS

(defun newvarmexpt (x e flag) 

  ;; WHEN FLAG IS T, CALL RETURNS RATFORM
  (prog (topexp) 
     (cond ((and (integerp e) (not flag))
	    (return (newvar1 (cadr x))))

	   ;; THIS MAKES PROBLEMS FOR RISCH ((AND(NOT(INTEGERP
	   ;;E))(MEMQ 'RATSIMP (CDAR X))) (RETURN(SETQ VLIST
	   ;;(CONS X VLIST))))
	   )
     (setq topexp 1)
     top  (cond

	    ;; X=B^N FOR N A NUMBER
	    ((integerp e)
	     (setq topexp (times topexp e))
	     (setq x (cadr x)))
	    ((atom e) nil)

	    ;; X=B^(P/Q) FOR P AND Q INTEGERS
	    ((eq (caar e) 'rat)
	     (cond ((or (minusp (cadr e)) (greaterp (cadr e) 1))
		    (setq topexp (times topexp (cadr e)))
		    (setq x (list '(mexpt)
				  (cadr x)
				  (list '(rat) 1 (caddr e))))))
	     (cond ((or flag (numberp (cadr x)) ))
		   (*ratsimp*
		    (cond ((memalike x radlist) (return nil))
			  (t (setq radlist (cons x radlist))
			     (return (newvar1 (cadr x))))) )
		   ($algebraic (newvar1 (cadr x)))))
	    ;; X=B^(A*C)
	    ((eq (caar e) 'mtimes)
	     (cond
	       ((or 

		 ;; X=B^(N *C)
		 (and (atom (cadr e))
		      (integerp (cadr e))
		      (setq topexp (times topexp (cadr e)))
		      (setq e (cddr e)))

		 ;; X=B^(P/Q *C)
		 (and (not (atom (cadr e)))
		      (eq (caaadr e) 'rat)
		      (not (equal 1 (cadadr e)))
		      (setq topexp (times topexp (cadadr e)))
		      (setq e (cons (list '(rat)
					  1
					  (caddr (cadr e)))
				    (cddr e)))))
		(setq x
		      (list '(mexpt)
			    (cadr x)
			    (setq e (simplify (cons '(mtimes)
						    e)))))
		(go top))))

	    ;; X=B^(A+C)
	    ((and (eq (caar e) 'mplus) expsumsplit) ;SWITCH CONTROLS
	     (setq			;SPLITTING EXPONENT
	      x				;SUMS
	      (cons
	       '(mtimes)
	       (mapcar 
		(function (lambda (ll) 
		  (list '(mexpt)
			(cadr x)
			(simplify (list '(mtimes)
					topexp
					ll)))))
		(cdr e))))
	     (cond (flag (return (prep1 x)))
		   (t (return (newvar1 x))))))
     (cond (flag nil)
	   ((equal 1 topexp)
	    (cond ((or (atom x)
		       (not (eq (caar x) 'mexpt)))
		   (newvar1 x))
		  ((or (memalike x varlist) (memalike x vlist))
		   nil)
		  (t (cond ((or (atom x) (null *fnewvarsw))
			    (putonvlist x))
			   (t (setq x (littlefr1 x))
			      (mapc (function newvar1)
				    (cdr x))
			      (or (memalike x vlist)
				  (memalike x varlist)
				  (putonvlist x)))))))
	   (t (newvar1 x)))
     (return
       (cond
	 ((null flag) nil)
	 ((equal 1 topexp)
	  (cond
	    ((and (not (atom x)) (eq (caar x) 'mexpt))
	     (cond ((assolike x genpairs))
		   ;; *** SHOULD ONLY GET HERE IF CALLED FROM FR1. *FNEWVARSW=NIL
		   (t (setq x (littlefr1 x))
		      (cond ((assolike x genpairs))
			    (t (newsym x))))))
	    (t (prep1 x))))
	 (t (ratexpt (prep1 x) topexp))))))

(defun newvar1 (x) 
  (cond ((numberp x) nil)
	((memalike x varlist) nil)
	((memalike x vlist) nil)
	((atom x) (putonvlist x))
	((memq (caar x)
	       '(mplus mtimes rat mdifference
		 mquotient mminus bigfloat))
	 (mapc (function newvar1) (cdr x)))
	((eq (caar x) 'mexpt)
	 (newvarmexpt x (caddr x) nil))
	((eq (caar x) 'mrat)
	 (and *withinratf* (memq 'trunc (cdddar x)) (throw 'ratf '%%))
	 (cond ($ratfac (mapc 'newvar3 (caddar x)))
	       (t (mapc (function newvar1) (reverse (caddar x))))))
	(t (cond (*fnewvarsw (setq x (littlefr1 x))
			     (mapc (function newvar1)
				   (cdr x))
			     (or (memalike x vlist)
				 (memalike x varlist)
				 (putonvlist x)))
		 (t (putonvlist x))))))

(defun newvar3 (x)
  (or (memalike x vlist)
      (memalike x varlist)
      (putonvlist x)))
 


(defun fr1 (x varlist)		    ;put radicands on initial varlist?
  (prog (genvar $norepeat *ratsimp* radlist vlist nvarlist ovarlist genpairs)
     (newvar1 x)
     (setq nvarlist (mapcar #'fr-args vlist))
     (cond ((not *ratsimp*)	;*ratsimp* not set for initial varlist
	    (setq varlist (nconc (sortgreat vlist) varlist))
	    (return (rdis (cdr (ratrep* x))))))
     (setq ovarlist (nconc vlist varlist)
	   vlist nil)
     (mapc (function newvar1) nvarlist)	;*RATSIMP*=T PUTS RADICANDS ON VLIST
     (setq nvarlist (nconc nvarlist varlist) ; RADICALS ON RADLIST
	   varlist (nconc (sortgreat vlist) (radsort radlist) varlist))
     (orderpointer varlist)
     (setq genpairs
	   (mapcar (function (lambda (x y) (cons x (rget y))))
		   varlist genvar))
     (let (($algebraic $algebraic) ($ratalgdenom $ratalgdenom) radlist)
       (and (not $algebraic)
	    (ormapc (function algpget) varlist) ;NEEDS *RATSIMP*=T
	    (setq $algebraic t $ratalgdenom nil))
       (ratsetup varlist genvar)
       (setq genpairs
	     (mapcar (function (lambda (x y) (cons x (prep1 y))))
		     ovarlist nvarlist))
       (setq x (rdis (prep1 x)))
       (cond (radlist			;rational radicands
	      (setq *ratsimp* nil)
	      (setq x (ratsimp (simplify x) nil nil)))))
     (return x) ))

(defun ratsimp (x varlist genvar) ($ratdisrep (ratf x)))

(defun littlefr1 (x) 
  (cons (remq 'simp (car x))
	(mapfr1 (cdr x) nil)))

;;IF T RATSIMP FACTORS RADICANDS AND LOGANDS
(defmvar fr-factor nil)				       

(defun fr-args (x)			;SIMP (A/B)^N TO A^N/B^N ?
  (cond ((atom x)
	 (when (eq x '$%i) (setq *ratsimp* t)) ;indicates algebraic present
	 x)
	(t (setq *ratsimp* t)		;FLAG TO CHANGED ELMT.
	   (simplify (zp (cons (remq 'simp (car x))
			       (if (or (radfunp x nil) (eq (caar x) '%log))
				   (cons (if fr-factor (factor (cadr x))
					     (fr1 (cadr x) varlist))
					 (cddr x))
				   (let (modulus)
				     (mapfr1 (cdr x) varlist)))))))))

;;(DEFUN ZP (X)						;SIMPLIFY MEXPT'S &
;;       (COND ((ATOM X) X)				;RATEXPAND EXPONENT
;;	     ((NOT (EQ (CAAR X) 'MEXPT)) X)
;;	     ((EQUAL 0 (CADDR X)) 1)
;;	     ((EQUAL 0 (CADR X)) 0)
;;	     ((EQUAL 1 (CADR X)) 1)
;;	     ((ATOM (CADDR X)) X)
;;	     (T (LIST (CAR X) (CADR X)
;;		      ((LAMBDA (VARLIST *RATSIMP*) ($RATEXPAND (CADDR X)))
;;		       VARLIST NIL)))))

(defun zp (x)
  (if (and (mexptp x) (not (atom (caddr x))))
      (list (car x) (cadr x)
	    (let ((varlist varlist) *ratsimp*)
	      ($ratexpand (caddr x))))
      x))


(defun newsym (e)
  (prog (g p)
     (cond ((setq g (assolike e genpairs))
	    (return g)))
     #-cl
     (setq g (gensym))
     #+cl
     (setq g (gensym-readable e))
     (putprop g e 'disrep)
     (push e varlist)
     (push (cons e (rget g)) genpairs)
     (valput g (if genvar (sub1 (valget (car genvar))) 1))
     (push g genvar)
     (cond ((setq p (and $algebraic (algpget e)))
	    (algordset p genvar)
	    (putprop g p 'tellrat)))
     (return (rget g))))


;;  Any program which calls RATF on
;;  a floating point number but does not wish to see "RAT replaced ..."
;;  message, must bind $RATPRINT to NIL.

(defmvar $ratprint t)

(defmvar $ratepsilon #-franz 2.0e-8 
	 #+(and franz vax) (expt 2.0 -56.)
	 #+(and franz 68k) (expt 2.0 -52.))
;; Some 68k stuff has a shorter significand.
;; This control of conversion from float to rational appears to be explained
;; nowhere. - RJF

(defmfun maxima-rationalize (x)
  (cond ((not (floatp x)) x)
	((< x 0.0) (setq x (ration1 (*$ -1.0 x)))
	 (rplaca x (times -1 (car x))))
	(t (ration1 x))))

;; the following code patches the fact that fix(float(bignum))
;; sometimes fails in franz.
#+franz
(defun ration1 (x)
  ((lambda (rateps)
     (or (and (zerop x) (cons 0 1))
	 (prog (y a)
	    (return (do ((xx x (setq y (//$ 1.0 (-$ xx (float a)))))
			 (num (setq a (newfix x)) 
			      (plus (times (setq a (newfix y)) num) onum))
			 (den 1 (plus (times a den) oden))
			 (onum 1 num)
			 (oden 0 den))
			((and (not (zerop den))
			      (< (abs (//$ (-$ x (//$ (float num) (float den)))
					   x))
				 rateps))
			 (cons num den))  )))))
   (cond ((not (floatp $ratepsilon)) ($float $ratepsilon)) (t $ratepsilon))))

#+franz
(defun newfix (x)
  (cond ((greaterp (abs x) two30f)
	 (times two30 (newfix (quotient x two30f))))
	(t (fix x))))

#-franz
(defun ration1 (x)
  (let ((rateps
	 (cond ((not (floatp $ratepsilon))
		($float $ratepsilon)) (t $ratepsilon))))
    (or (and (zerop x) (cons 0 1))
	(prog
	    (y a)
	   (return
	     (do ((xx x (setq y (/ 1.0 (- xx (float a x)))))
		  (num (setq a (fix x)) (plus (times (setq a (fix y)) num) onum))
		  (den 1 (plus (times a den) oden))
		  (onum 1 num)
		  (oden 0 den))
		 ((and (not (zerop den))
		       (not (> (abs
				(/
				 (- x
				    (/ (float num x)
				       (float den x)))
				 x))
			       rateps)))
		  (cons num den))))))))

(defun prepfloat (f)
  (cond ((< (abs f) 1.0e-37) (setq f 0.0))) ;changed 38 to 37 --wfs
  (cond (modulus (merror "Floating point meaningless unless MODULUS = FALSE"))
	($ratprint (mtell "~&RAT replaced ~A by" f)))
  (setq f (maxima-rationalize f))
  (if $ratprint (mtell " ~A//~A = ~A~%"  (car f) (cdr f)
		       (fpcofrat1 (car f) (cdr f))))
  f)


(defun pdisrep (p)
  (cond ((atom p) p)
	(t (pdisrep+ (pdisrep2 (cdr p) (get (car p) 'disrep))))))

(defun pdisrep! (n var)
  (cond ((zerop n) 1)
	((eqn n 1) (cond ((atom var) var)
			 ((or (eq (caar var) 'mtimes)
			      (eq (caar var) 'mplus))
			  (copy1 var))
			 (t var)))
	(t (list '(mexpt ratsimp) var n))))

(defun pdisrep+ (p)
  (cond ((null (cdr p)) (car p))
	(t (let ((a (last p)))
	     (cond ((mplusp (car a))
		    (rplacd a (cddar a))
		    (rplaca a (cadar a))))
	     (cons '(mplus ratsimp) p)))))
	 
(defun pdisrep* (a b)
  (cond ((eqn a 1) b)
	((eqn b 1) a)
	(t (cons '(mtimes ratsimp)
		 (nconc (pdisrep*chk a) (pdisrep*chk b))))))

(defun pdisrep*chk (a)
  (if (mtimesp a) (cdr a) (ncons a)))

(defun pdisrep2 (p var)
  (cond ((null p) nil)
	($ratexpand (pdisrep2expand p var))
	(t (do ((l () (cons (pdisrep* (pdisrep (cadr p))
				      (pdisrep! (car p) var))
			    l))
		(p p (cddr p)))
	       ((null p)
		(nreverse l))))))

;; IF $RATEXPAND IS TRUE, (X+1)*(Y+1) WILL DISPLAY AS
;; XY + Y + X + 1  OTHERWISE, AS (X+1)Y + X + 1
(defmvar $ratexpand nil)

(defmfun $ratexpand (x)
  (cond ((mbagp x) (cons (car x) (mapcar '$ratexpand (cdr x))))
	(t ((lambda ($ratexpand $ratfac) (ratdisrep (ratf x))) t nil))))
	 
(defun pdisrep*expand (a b)
  (cond ((eqn a 1) (list b))
	((eqn b 1) (list a))
	((or (atom a) (not (eq (caar a) 'mplus)))
	 (list (cons (quote (mtimes ratsimp))
		     (nconc (pdisrep*chk a) (pdisrep*chk b)))))
	(t (mapcar #'(lambda (z) (if (eqn z 1) b
				     (cons '(mtimes ratsimp)
					   (nconc (pdisrep*chk z)
						  (pdisrep*chk b)))))
		   (cdr a)))))
	 
(defun pdisrep2expand (p var)
  (cond ((null p) nil)
	(t (nconc (pdisrep*expand (pdisrep (cadr p))
				  (pdisrep! (car p) var))
		  (pdisrep2expand (cddr p) var)))))


(defmvar $ratdenomdivide t)

(defmfun $ratdisrep (x)
  (cond ((not ($ratp x)) x)
	(t (setq x (ratdisrepd x))
	   (if (and (not (atom x)) (memq 'trunc (cdar x)))
	       (cons (delq 'trunc (copy-top-level (car x)) 1) (cdr x))
	       x))))

;; RATDISREPD is needed by DISPLA. - JPG
(defun ratdisrepd (x)
  (mapc #'(lambda (y z) (putprop y z (quote disrep)))
	(cadddr (car x))
	(caddar x))
  (let ((varlist (caddar x)))
    (if (memq 'trunc (car x)) (srdisrep x) (cdisrep (cdr x)))))

(defun cdisrep (x &aux n d sign)
  (cond ((pzerop (car x)) 0)
	((or (eqn 1 (cdr x)) (floatp (cdr x))) (pdisrep (car x)))
	(t (setq sign (cond ($ratexpand (setq n (pdisrep (car x))) 1)
			    ((pminusp (car x))
			     (setq n (pdisrep (pminus (car x)))) -1)
			    (t (setq n (pdisrep (car x))) 1)))
	   (setq d (pdisrep (cdr x)))
	   (cond ((and (numberp n) (numberp d))
		  (list '(rat) (times sign n) d))
		 ((and $ratdenomdivide $ratexpand
		       (not (atom n))
		       (eq (caar n) 'mplus))
		  (fancydis n d))
		 ((numberp d)
		  (list '(mtimes ratsimp)
			(list '(rat) sign d) n))
		 ((eqn sign -1) 
		  (cons '(mtimes ratsimp)
			(cond ((numberp n)
			       (list (times n -1)
				     (list '(mexpt ratsimp) d -1)))
			      (t (list sign n (list '(mexpt ratsimp) d -1))))))
		 ((eqn n 1)
		  (list '(mexpt ratsimp) d -1))
		 (t (list '(mtimes ratsimp) n
			  (list '(mexpt ratsimp) d -1)))))))
 

;; FANCYDIS GOES THROUGH EACH TERM AND DIVIDES IT BY THE DENOMINATOR.

(defun fancydis (n d)
  (setq d (simplify (list '(mexpt) d -1)))
  (simplify (cons '(mplus)
		  (mapcar #'(lambda (z)
			      ($ratdisrep (ratf (list '(mtimes) z d))))
			  (cdr n)))))


(defun compatvarl (a b c d)
  (cond ((null a) nil)
	((or (null b) (null c) (null d)) (throw 'compatvl nil))
	((alike1 (car a) (car b))
	 (setq a (compatvarl (cdr a) (cdr b) (cdr c) (cdr d)))
	 (cond ((eq (car c) (car d)) a)
	       (t (cons (cons (car c) (car d)) a))))
	(t (compatvarl a (cdr b) c (cdr d)))))

(defun newvar (l &aux vlist)
  (newvar1 l)
  (setq varlist (nconc (sortgreat vlist) varlist)))

(defun sortgreat (l) (and l (nreverse (sort l 'great))))

(defun fnewvar (l &aux (*fnewvarsw t)) (newvar l))

(defun nestlev (exp)
  (cond ((atom exp) 0)
	(t (do ((m (nestlev (cadr exp)) (max m (nestlev (car l))))
		(l (cddr exp) (cdr l)))
	       ((null l) (f1+ m))))))

(defun radsort (l)
  (sort l #'(lambda (a b)
	      ((lambda (na nb)
		 (cond ((< na nb) t)
		       ((> na nb) nil)
		       (t (great b a))))
	       (nestlev a) (nestlev b)))))

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 5
;;	IT INCLUDES THE CONVERSION AND TOP-LEVEL ROUTINES USED
;;	BY THE REST OF THE FUNCTIONS.

