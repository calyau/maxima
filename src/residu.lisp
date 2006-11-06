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
(macsyma-module residu)

(load-macsyma-macros rzmac)

(declare-top (*lexpr $diff $substitute $taylor $expand)
	     (special $breakup $noprincipal varlist
		      leadcoef var *roots *failures wflag nn*
		      sn* sd* $tellratlist genvar semirat* dn* zn)
	     (genprefix res))

(setq semirat* nil) 

;; Compute the poles (roots) of the polynomial D and return them.
;; Divide these roots into several parts: Those in REGION, REGION1,
;; and everywhere else.  These are returned in a list.  (In a more
;; modern style, we'd probably return them in 4 different values.)
;;
;; The regions are determined by functions REGION and REGION1, which
;; should return non-NIL if the root is in the given region.
;;
;; The first part of the list of the form ((r1 (x - r1)^d1) (r2 (x -
;; r2)^d2) ...) where r1, r2 are the roots, d1, d2 are the
;; multiplicity of each root, and x is the variable.
;;
;; The second part is a list of the roots in region 1.  If the root is
;; simple, then the item is the root.  If the root is a multiple root
;; of order d, the item is (r d).
;;
;; The third part is similar, except it is the list of the roots in
;; region 2.
;;
;; Finally, the fourth part is either NIL if SEMIRAT* is NIL.  If
;; SEMIRAT* is non-NIL, it is the list of roots outside of region and
;; region1, in the same format as the second and third parts.
(defun polelist (d region region1)
  (prog (roots $breakup r rr ss r1 s pole wflag cf) 
     (setq wflag t)
     (setq leadcoef (polyinx d var 'leadcoef))
     (setq roots (solvecase d))
     (if (eq roots 'failure) (return ()))
     ;; Loop over all the roots.  SOLVECASE returns the roots in the form
     ;; ((x = r1) mult1
     ;;  (x = r2) mult2
     ;;  ...)
     
   loop1
     (cond ((null roots)
	    (cond ((and semirat*
			(> (f+ (length s) (length r))
					;(LENGTH (APPEND S R))
			   (f+ (length ss) (length rr))
					;(LENGTH (APPEND SS RR))
			   ))
		   ;; Return CF, repeated roots (semirat*), simple
		   ;; roots (semirat*), roots in region 1.
		   (return (list cf rr ss r1)))
		  (t
		   ;; Return CF, repeated roots, simple roots, roots in region 1.
		   (return (list cf r s r1)))))
	   (t
	    ;; Set POLE to the actual root and set D to the
	    ;; multiplicity of the root.
	    (setq pole (caddar roots))
	    (setq d (cadr roots))
	    (cond (leadcoef
		   ;; Is it possible for LEADCOEF to be NIL ever?
		   ;;
		   ;; Push (pole (x - pole)^d) onto the list CF.
		   (setq cf (cons pole
				  (cons 
				   (m^ (m+ var (m* -1 pole))
				       d)
				   cf)))))))
     (cond ((funcall region pole)
	    ;; The pole is in REGION
	    (cond ((equal d 1)
		   ;; A simple pole, so just push the pole onto the list S.
		   (setq s (cons pole s)))
		  (t
		   ;; A multiple pole, so push (pole d) onto the list R.
		   (setq r (cons (list pole d) r)))))
	   ((funcall region1 pole)
	    ;; The pole is in REGION1
	    (cond ((not $noprincipal)
		   ;; Put the pole onto the R1 list.  (Don't know what
		   ;; $NOPRINCIPAL is.)
		   (setq r1 (cons pole r1)))
		  (t
		   ;; Return NIL if we get here.  
		   (return nil))))
	   (semirat*
	    ;; (What does SEMIRAT* mean?)  Anyway if we're here, the
	    ;; pole is not in REGION or REGIOn1, so push the pole onto
	    ;; SS or RR depending if the pole is repeated or not.
	    (cond ((equal d 1)
		   (setq ss (cons pole ss)))
		  (t (setq rr (cons (list pole d) rr))))))
     ;; Pop this root and multiplicity and move on.
     (setq roots (cddr roots))
     (go loop1)))

(defun solvecase (e)
  (cond ((not (among var e)) nil)
	(t (let (*failures *roots) 
	     (solve e var 1)
	     (cond (*failures 'failure)
		   ((null *roots) ())
		   (t *roots))))))

;; Compute the sum of the residues of n/d.
(defun res (n d region region1)
  (let ((pl (polelist d region region1))
	dp a b c factors leadcoef)
    (cond 
      ((null pl) nil)
      (t
       (setq factors (car pl))
       (setq pl (cdr pl))
       ;; PL now contains the list of the roots in region, roots in
       ;; region1, and everything else.
       (cond ((or (cadr pl)
		  (caddr pl)) 
	      (setq dp (sdiff d var))))
       (cond ((car pl)
	      ;; Compute the sum of the residues of n/d for the roots in REGION.
	      (setq a (m+l (residue n (cond (leadcoef factors)
					    (t d))
				    (car pl)))))
	     (t (setq a 0.)))
       (cond ((cadr pl)
	      ;; Compute the sum of the residues of n/d for the roots in REGION1.
	      (setq b (m+l (mapcar #'(lambda (pole)
				       ($residue (m// n d) var pole))
				   (cadr pl)))))
	     (t (setq b 0.)))
       (cond ((caddr pl)
	      ;; Compute the sum of the residues of n/d for the roots
	      ;; not in REGION nor REGION1.
	      (setq c (m+l (mapcar #'(lambda (pole)
				       ($residue (m// n d) var pole))
				   (caddr pl)))))
	     (t (setq c ())))
       ;; Return the sum of the residues in the two regions and the
       ;; sum of the residues outside the two regions.
       (list (m+ a b) c)))))

(defun residue (zn factors pl)
  (cond (leadcoef 
	 (mapcar #'(lambda (j)
		     (destructuring-let (((factor1 factor2) (remfactor factors (car j) zn)))
		       (resm0 factor1 factor2 (car j) (cadr j))))
		 pl))
	(t (mapcar #'(lambda (j)
		       (resm1 (div* zn factors) (car j)))
		   pl))))

(defun res1 (zn zd pl1)
  (setq zd (div* zn zd))
  (mapcar #'(lambda (j) ($rectform ($expand (subin j zd)))) pl1))

(defun resprog0 (f g n n2)
  (prog (a b c r) 
     (setq a (resprog f g))
     (setq b (cadr a) c (ptimes (cddr a) n2) a (caar a))
     (setq a (ptimes n a) b (ptimes n b))
     (setq r (pdivide a g))
     (setq a (cadr r) r (car r))
     (setq b (cons (pplus (ptimes (car r) f) (ptimes (cdr r) b))
		   (cdr r)))
     (return (cons (cons (car a) (ptimes (cdr a) c))
		   (cons (car b) (ptimes (cdr b) c)))))) 


(defun resm0 (e n pole m)
  (setq e (div* n e))
  (setq e ($diff e var (sub1 m)))
  (setq e ($rectform ($expand (subin pole e))))
  (div* e (simplify `((mfactorial) ,(sub1 m)))))

(defun remfactor (l p n)
  (prog (f g) 
   loop (cond ((null l)
	       (return (list (m*l (cons leadcoef g)) n)))
	      ((equal p (car l)) (setq f (cadr l)))
	      (t (setq g (cons (cadr l) g))))
   (setq l (cddr l))
   (go loop)))

(defun resprog (p1b p2b)
  (prog (temp coef1r coef2r fac coef1s coef2s zeropolb f1 f2) 
     (setq coef2r (setq coef1s 0))
     (setq coef2s (setq coef1r 1))
     b1   (cond ((not (lessp (pdegree p1b var) (pdegree p2b var))) (go b2)))
     (setq temp p2b)
     (setq p2b p1b)
     (setq p1b temp)
     (setq temp coef2r)
     (setq coef2r coef1r)
     (setq coef1r temp)
     (setq temp coef2s)
     (setq coef2s coef1s)
     (setq coef1s temp)
     b2   (cond ((zerop (pdegree p2b var))
		 (return (cons (cons coef2r p2b) (cons coef2s p2b)))))
     (setq zeropolb (psimp var
			   (list (difference (pdegree p1b var)
					     (pdegree p2b var))
				 1)))
     (setq fac (pgcd (caddr p1b) (caddr p2b)))
     (setq f1 (pquotient (caddr p1b) fac))
     (setq f2 (pquotient (caddr p2b) fac))
     (setq p1b (pdifference (ptimes f2 (psimp (car p1b) (cdddr p1b)))
			    (ptimes f1
				    (ptimes zeropolb
					    (psimp (car p2b)
						   (cdddr p2b))))))
     (setq coef1r (pdifference (ptimes f2 coef1r)
			       (ptimes (ptimes f1 coef2r) zeropolb)))
     (setq coef1s (pdifference (ptimes f2 coef1s)
			       (ptimes (ptimes f1 coef2s) zeropolb)))
     (go b1))) 

;;;Looks for polynomials. puts polys^(pos-num) in sn* polys^(neg-num) in sd*.
(defun snumden (e)
  (cond ((or (atom e) 
	     (mnump e))
	 (setq sn* (cons e sn*)))
	((and (mexptp e) 
	      (integerp (caddr e)))
	 (cond ((polyinx (cadr e) var nil)
		(cond ((minusp (caddr e))
		       (setq sd* (cons (cond ((equal (caddr e) -1) (cadr e))
					     (t (m^ (cadr e)
						    (minus (caddr e)))))
				       sd*)))
		      (t (setq sn* (cons e sn*)))))))
	((polyinx e var nil)
	 (setq sn* (cons e sn*)))))

(setq sn* nil sd* nil) 

(defmfun $residue (e var p)
  (cond (($unknown e)
	 ($nounify '$residue)
	 (list '(%residue) e var p))
	(t
	 (let (sn* sd*)
	   (if (and (mtimesp e) (andmapcar #'snumden (cdr e)))
	       (setq nn* (m*l sn*) dn* (m*l sd*))
	       (numden e)))
	 (resm1 (div* nn* dn*) p))))

(defun resm1 (e pole)
  ;; Call $ratsimp to simplify pole.  Not sure if this is really
  ;; necessary or desired, but it fixes bug 1504505.  It would be
  ;; better to fix $taylor, but that seems much harder.
  (let ((pole ($ratsimp ($rectform pole))))
    (setq e (ratdisrep ($taylor e var pole
				0 ;; things like residue(s/(s^2-a^2),s,a) fails if use -1
				;;-1
				)))
    (coeff e (m^ (m+ (m* -1 pole) var) -1) 1)))

