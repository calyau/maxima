;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module ezgcd)

(declare-top (special lcprod svals svars oldsvars oldsvals
		      valflag $gcd pl0 d0 degd0 var
		      many* tempprime ovarlist valist modulus
		      zl *prime plim nn* ne nn*-1 dlp
		      ez1skip svalsl nsvals
		      lc1 oldlc limk *alpha))

(load-macsyma-macros ratmac)

(defun ezgcd2 (f g)
  (prog (allvars)
     (setq allvars (union* (listovars f) (listovars g)))
     (cond ((> 2 (length allvars))
	    (setq allvars (newgcd f g modulus))
	    (cond ((cdr allvars) (return allvars))
		  (t (return (list (setq allvars (car allvars))
				   (pquotient f allvars)
				   (pquotient g allvars)))))))
     (setq allvars (sort allvars #'pointergp))
     (return (ezgcd (list f g) allvars modulus))))

(defun newgcdcall (p q)
  (car (newgcd p q modulus)))

(defun gcdl (pl)
  (do ((d (car pl) (pgcd d (car l)))
       (l (cdr pl) (cdr l)))
      ((or (null l) (eql d 1)) d)))

(defun newgcdl (pl)
  (let (($gcd '$mod))
    (gcdl pl)))

(defun oldgcdl (elt pl)
  (let (($gcd '$red))
    (gcdl (cons elt pl))))

(defun oldgcdcall (pfl)
  (let ((a (oldgcdl (car pfl) (cdr pfl))))
    (cons a (mapcar #'(lambda (h) (pquotient h a)) pfl))))

(defun non0rand (modulus)
  (do ((r))
      ((not (zerop (setq r (cmod (random 1000))))) r)))

(defun getgoodvals (varl lcp)
  (mapcar #'(lambda (v) (do ((val 0 (non0rand tempprime)) (temp))
			    ((not (pzerop (setq temp (pcsubsty val v lcp))))
			     (setq lcp temp) val)))
	  varl))

(defun evmap (vals pl)
  (prog (pl0 d0)
     (cond ((equal nsvals (length svalsl)) (return nil)))
     (cond (valflag (go newvals)))
     (setq vals (getgoodvals svars lcprod))
     again (cond ((member vals svalsl :test #'equal)
		 (setq vals (rand (length svars) tempprime))
		 (go again)))
     (setq valflag t svalsl (cons vals svalsl))
     (go end)
     newvals
     (setq pl0 (rand (length svars) tempprime))
     (cond ((member pl0 svalsl :test #'equal) (go newvals))
	   (t (setq vals pl0 svalsl (cons vals svalsl))))
     (cond ((equal 0 (pcsub lcprod vals svars))
	    (cond ((equal nsvals (length svalsl))
		   (return nil))
		  (t (go newvals)))))
     ;;	     END  (GETD0 PL(SETQ VALS(SUBST 1. 0.  VALS))) WHAT WAS SUBST FOR?
     end  (getd0 pl vals)
     (return (list vals pl0 d0))))

(defun degodr (a b)
  (cond ((numberp a) nil)
	((numberp b) t)
	(t (> (cadr a) (cadr b)))))

(defun evtildegless (pl)
  (prog (evout npl0 nd0 ndeg)
   again (setq evout (evmap svals pl))
   (cond (evout (setq npl0 (cadr evout) nd0 (caddr evout)))
	 (t (return nil)))
   (cond ((numberp nd0) (setq ndeg 0)) (t (setq ndeg (cadr nd0))))
   (when (or (= degd0 ndeg) (> ndeg degd0)) (go again))
   (return (setq degd0 ndeg pl0 npl0 d0 nd0 svals (car evout)))))

(defun ptimesmerge (pl1 pl2)
  (cond (pl1 (cons (ptimes (car pl1) (car pl2))
		   (ptimesmerge (cdr pl1) (cdr pl2))))
	(t nil)))

(defun ez1call (builder factrs lc1 valist ovarlist)
  (prog (*prime plim nn* ne nn*-1 zl zfactr oldlc lcd0
	 dlp limk genvar mult subval subvar)
     (declare (special subval subvar))
     (setq oldlc (caddr builder))
     (cond ((not (equal 1 lc1))
	    (setq builder (ptimes builder lc1))))
     (setq genvar (append ovarlist (list (car builder))))
     (cond (modulus
	    (setq *prime modulus plim modulus limk -1)
	    (go mod))
	   (t (setq *prime (max (norm builder)
				(maxcoefficient (car factrs))
				(maxcoefficient (cadr factrs))))))
     (cond ((> *prime *alpha)
	    (prog (newmodulus)
	       (setq newmodulus (* *alpha *alpha)
		     limk 0)
	       again(cond ((> newmodulus *prime)
			   (setq *prime *alpha plim newmodulus))
			  (t (setq limk (1+ limk) newmodulus (* newmodulus newmodulus))
			     (go again)))))
	   (t (setq limk -1 *prime *alpha plim *alpha)))
     mod  (setq nn* (1+ (setq ne (setq nn*-1 (length ovarlist)))))
     (setq zl (completevector nil 1 nn* 0))
     (fixvl valist ovarlist)
     (cond ((equal 1 lc1)
	    (setq modulus plim builder (newrep builder))
	    (setq dlp  (loop for x in (cdr (oddelm builder))
			  maximize (multideg x)))
	    (setq zfactr (z1 builder (car factrs)(cadr factrs)))
	    (setq zfactr (restorelc zfactr (caddr builder)))
	    (return (oldrep(cadr zfactr)))))
     (setq modulus plim lcd0 (caddar factrs))
     (setq mult (ctimes (pcsub lc1 svals svars)
			(crecip lcd0)))
     (setq factrs (list (ptimes mult (car factrs))
			(ptimes lcd0 (cadr factrs))))
     (setq builder (newrep builder))
     (setq dlp (loop for x in (cdr (oddelm builder))
		  maximize (multideg x)))
     (setq zfactr (z1 builder (car factrs) (cadr factrs)))
     (setq zfactr (pmod (oldrep (car zfactr))))
     (return (cadr (let ((modulus nil))
		     (fastcont zfactr))))))

(defun getd0 (tpl tvals)
  (prog (c)
     (setq d0 (pcsub (car tpl) tvals svars)
	   pl0 (list d0) tpl (cdr tpl))
     loop (cond ((null tpl) (return d0)))
     (setq c (pcsub (car tpl) tvals svars)
	   d0 (newgcdcall d0 c))
     (cond ((numberp d0) (return (setq d0 1))))
     (setq pl0 (append pl0 (list c)) tpl (cdr tpl))
     (go loop)))

(defun numberinlistp (l)
  (do ((l l (cdr l))) ((null l))
    (and (numberp (car l)) (return (car l)))))

(defun ezgcd (pfl vl modulus)
  (prog (svars svals valflag tempprime pfcontl contgcd contcofactl
	 pl nsvars nsvals svalsl lcprod gcdlcs lcpl evmapout
	 pl0 d0 d degd0 degd0n d0n pl0n temp tryagain cofact0
	 pcofactl ith builder var termcont tcontl $algebraic)
     (cond ((setq temp (numberinlistp pfl))
	    (cond ((or (member 1 pfl) (member -1 pfl))
		   (return (cons 1 pfl))))
	    (setq temp (oldgcdl temp (remove temp pfl :test #'equal))
		  pl (mapcar #'(lambda(h) (pquotient h temp)) pfl))
	    (return (cons temp pl))))
     (setq svars (cdr vl) var (car vl))
     (cond (svars (setq many* t))
	   (t (return (cons (setq d (newgcdl pfl))
			    (mapcar #'(lambda(h) (pquotient h d)) pfl)))))
     (cond (modulus (setq tempprime modulus))
	   (t (setq tempprime 13.)))
     (setq tcontl (mapcar #'ptermcont pfl)
	   pfl (mapcar #'cadr tcontl)
	   tcontl (mapcar #'car tcontl))
     (setq termcont (oldgcdcall tcontl)
	   tcontl (cdr termcont)
	   termcont (car termcont))
     (cond ((setq temp (numberinlistp pfl))
	    (setq d (oldgcdl temp (remove temp pfl :test #'equal))
		  pcofactl
		  (mapcar #'(lambda(h) (pquotient h d)) pfl))
	    (setq contgcd termcont contcofactl tcontl)
	    (go out)))
     (setq pfcontl (mapcar #'(lambda(h) (if (eq var (car h))
					    (fastcont h)
					    (list h 1)))
			   pfl))
     (setq pfl (mapcar #'cadr pfcontl)
	   pfcontl (mapcar #'car pfcontl))
     (setq contgcd (ezgcd pfcontl svars modulus) pfcontl nil
	   contcofactl (ptimesmerge tcontl (cdr contgcd))
	   contgcd (ptimes termcont (car contgcd)))
     (cond ((numberinlistp pfl)
	    (setq d 1 pcofactl pfl) (go out)))
     (setq temp (listovarsl pfl))
     (cond ((setq temp (intersection svars temp :test #'equal)) nil)
	   (t (setq d (newgcdl pfl)) (go end)))
     (setq pl (bbsort pfl #'degodr))
     (setq nsvars (length svars))
     (do ((i nsvars (1- i)))
	 ((zerop i))
       (push 0 svals))
     (setq lcprod 1 svalsl (list svals)
	   nsvals (expt tempprime (length svars)))
     (do ((l (mapcar #'caddr pl) (cdr l)))
	 ((null l))
       (setq lcprod (ptimes lcprod (car l))))
     (cond ((equal 0 (pcsub lcprod svals svars))
	    (setq evmapout (evmap svals pl))
	    (cond (evmapout (setq svals (car evmapout)
				  pl0 (cadr evmapout)
				  d0 (caddr evmapout)))
		  (t (desetq (d . pcofactl) (oldgcdcall pfl))
		     (go out))))
	   (t (setq valflag t) (getd0 pl svals)))
     (cond ((numberp d0) (setq degd0 0))
	   (t (setq degd0 (cadr d0))))
     testd0
     (cond ((equal 1 d0) (setq d 1)
	    (setq d 1 pcofactl pfl) (go out)))
     (cond (degd0n (go testcofact)))
     anothersvals
     (setq evmapout (evmap svals pl))
     (cond (evmapout (setq pl0n (cadr evmapout)
			   d0n (caddr evmapout)
			   evmapout (car evmapout)))
	   (t (desetq (d . pcofactl) (oldgcdcall pfl))
	      (go out)))
     (cond ((numberp d0n) (setq degd0n 0))
	   (t (setq degd0n (cadr d0n))))
     (cond ((> degd0 degd0n)
	    (setq degd0 degd0n pl0 pl0n d0 d0n svals evmapout)
	    (go anothersvals)))
     (cond ((equal degd0 degd0n) (go testd0)) (t (go anothersvals)))
     testcofact
     (cond ((equal degd0 (cadar pl0)) nil) (t (go testgcd)))
     (setq d (car pl) temp pfl pcofactl nil)
     loop (cond (temp (setq d0n (eztestdivide (car temp) d)))
		(t (setq ez1skip t) (go out)))
     (cond (d0n (setq pcofactl (append pcofactl (list d0n)))
		(setq temp (cdr temp)) (go loop))
	   (t (cond ((evtildegless pl)
		     (setq degd0n nil) (go testd0))
		    (t (desetq (d . pcofactl) (oldgcdcall pfl))
		       (go out)))))
     testgcd
     (setq ith 1. temp pl0)
     next (cond (temp nil)
		(t (cond (tryagain  (setq d (nonsqfrcase pfl vl)
					  pcofactl (cdr d)
					  d (car d))
				    (go out))
			 (t (setq degd0 degd0n pl0 pl0n d0 d0n
				  degd0n nil pl0n nil d0n nil
				  svals evmapout tryagain t)
			    (go testgcd)))))
     (setq cofact0 (pquotient (car temp) d0))
     (cond ((numberp (newgcdcall d0 cofact0))
	    (setq builder (ith pl ith))
	    (cond ((intersection (listovars builder) svars :test #'equal)
		   (go callez1)))))
     (setq temp (cdr temp) ith (1+ ith)) (go next)
     callez1
     (setq lcpl (mapcar #'caddr pl)
	   gcdlcs (car (ezgcd lcpl svars modulus))
	   lcpl nil)
     (setq d (ez1call builder
		      (list d0 cofact0) gcdlcs
		      (reverse svals)
		      (reverse svars)))
     (setq modulus nil)
     end  (setq pcofactl nil temp pfl)
     (cond ((pminusp d) (setq d (pminus d))))
     loop1(cond (temp (setq cofact0 (eztestdivide (car temp) d)))
		(t (setq ez1skip nil) (go out)))
     (cond (cofact0 (setq pcofactl (append pcofactl (list cofact0)))
		    (setq temp (cdr temp)) (go loop1))
	   (t (cond ((evtildegless pl)
		     (setq degd0n nil) (go testd0))
		    (t (desetq (d . pcofactl) (oldgcdcall pfl))
		       (go out)))))
     out  (setq oldsvars svars oldsvals svals)
     (return (cons (ptimes contgcd d)
		   (ptimesmerge contcofactl pcofactl)))))

(defun listovarsl (plist)
  (prog (allvarsl allvars)
     (setq allvarsl (mapcar #'listovars plist))
     (setq allvars (car allvarsl))
     (do ((l (cdr allvarsl) (cdr l)))
	 ((null l))
       (setq allvars (union* allvars (car l))))
     (return allvars)))

(defmfun $ezgcd (&rest args)
  (prog (pfl allvars presult flag genvar denom pfl2)
     ;;need if genvar doesn't shrink
     (when (null args)
       (wna-err '$ezgcd))
     (when (some #'$ratp args)
       (setq flag t))
     (setq pfl (mapcar #'(lambda (h) (cdr (ratf h))) args))
     (setq pfl2 (list 1))
     (do ((lcm (cdar pfl))
	  (l (cdr pfl) (cdr l))
	  (cof1)
	  (cof2))
	 ((null l) (setq denom lcm))
       (desetq (lcm cof1 cof2) (plcmcofacts lcm (cdar l)))
       (unless (equal cof1 1)
	 (mapcar #'(lambda (x) (ptimes x cof1)) pfl2))
       (push cof2 pfl2))
     (setq pfl (mapcar #'car pfl))
     (setq allvars (sort (listovarsl pfl) #'pointergp))
     (setq presult (if $ratfac
		       (let (($gcd '$ez))
			 (facmgcd pfl))
		       (ezgcd pfl allvars modulus)))
     (setq presult (cons (cons (car presult) denom)
			 (if (equal denom 1)
			     (cdr presult)
			     (mapcar #'ptimes (cdr presult) pfl2))))
     (setq presult (cons '(mlist)
			 (cons (rdis* (car presult))
			       (mapcar #'pdis* (cdr presult)))))
     (return (if flag presult ($totaldisrep presult)))))

(defun insrt (nth elt l)
  (if (eql nth 1)
      (cons elt l)
      (cons (car l) (insrt (1- nth) elt (cdr l)))))

(defun nonsqfrcase(pl vl)
  (prog (d f ptr)
     (do ((dl pl (cdr dl))
	  (pt 1 (1+ pt)))
	 ((intersection (cdr vl) (listovars (car dl)) :test #'equal)
	  (setq f (car dl) ptr pt)))
     (setq d (ezgcd (list f (pderivative f (car f))) vl modulus)
	   pl (ezgcd (cons (cadr d) (remove f pl :test #'equal)) vl modulus)
	   pl (cons (car pl) (cons (car d) (cddr pl)))
	   d (car pl))
     loop  (setq pl (ezgcd pl vl modulus))
     (cond ((equal 1 (car pl))
	    (return (cons d (insrt ptr (pquotient f d) (cdddr pl))))))
     (setq d (ptimes (car pl) d)
	   pl (cons (car pl) (cddr pl)))
     (go loop)))

(defun eztestdivide (x y)
  (when (or (pcoefp x) (pcoefp y)
            (ignore-rat-err (pquotient (car (last x)) (car (last y)))))
    (ignore-rat-err (pquotient x y))))

(defun noterms (p)
  (cond ((pcoefp p) 1)
	(t (do ((nt (noterms (caddr p)) (+ nt (noterms (cadr p))))
		(p (cdddr p) (cddr p)))
	       ((null p) nt)))))

(defun fastcont (p)
  (prog (oldgenvar var tppl tcontl tcont coefvarl temp small1 small2 ans minus?)
     (cond ((univar (cdr p)) (return (oldcontent p)))
	   (t (setq oldgenvar genvar)
	      (setq var (car p))
	      ;; intersect must maintain order of genvar list
	      (setq genvar (remove var (intersect (cdr genvar) (listovars p))
				   :test #'equal))))
     (cond ((pminusp p) (setq p (pminus p) minus? t)))
     (setq tppl (oddelm (cddr p)))
     (cond ((null (cdr tppl))
	    (setq tcont 1)
	    (setq ans (car tppl))
	    (go out)))
     (setq tcontl (mapcar #'pmindegvec tppl))
     (setq tppl (mapcar #'(lambda(x y) (pquotient x (degvecdisrep y)))
			tppl tcontl))
     (setq tcont (car tcontl))
     (do ((l (cdr tcontl) (cdr l))) ((null l))
       (setq tcont (mapcar #'(lambda (x y) (min x y))
			   tcont (car l))))
     (setq tcontl nil)
     (setq tcont (degvecdisrep tcont))
     (setq genvar oldgenvar)
     (cond ((setq temp (numberinlistp tppl))
	    (cond ((or (member 1 tppl) (member -1 tppl))
		   (setq ans 1))
		  (t (setq ans (oldgcdl temp (delete temp tppl :test #'equal)))))
	    (go out)))
     (cond ((> 4 (length tppl))
	    (setq tppl (bbsort tppl #'(lambda(a b) (> (length a) (length b)))))
	    (go skip)))
     (setq coefvarl (mapcar #'listovars tppl))
     (setq temp (car coefvarl))
     (setq coefvarl (cdr coefvarl))
     loop (cond ((null coefvarl) nil)
		(t (cond ((null (setq temp (intersection temp (car coefvarl) :test #'equal)))
			  (setq ans 1) (go out))
			 (t (setq coefvarl (cdr coefvarl)) (go loop)))))
     (setq temp (mapcar #'noterms tppl))
     (setq tppl (mapcar #'(lambda (x y) (list x y))
			temp tppl))
     (setq tppl (bbsort tppl #'(lambda(x y) (> (car x) (car y)))))
     (setq tppl (mapcar #'cadr tppl))
     skip (setq small1 (car tppl))
     (setq small2 (cadr tppl))
     (setq ans (pgcd small1 small2))
     (cond ((eql 1 ans) (go out))
	   ((eql -1 ans) (setq ans 1) (go out)))
     (cond ((cddr tppl) (setq ans (cons ans (cddr tppl))))
	   (t (go out)))
     (setq temp (sort (listovarsl ans) #'pointergp))
     (setq ans (car (ezgcd ans temp modulus)))
     out (setq tcont (ptimes tcont ans))
     (setq p (pquotient p tcont))
     (cond (minus? (setq tcont (pminus tcont))))
     (return (list tcont p))))

(declare-top (unspecial lcprod svals svars oldsvars oldsvals
			valflag pl0 d0 degd0 var many* tempprime ovarlist valist
			zl *prime plim nn* ne nn*-1 dlp
			ez1skip svalsl nsvals lc1 oldlc limk))
