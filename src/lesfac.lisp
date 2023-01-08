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

(macsyma-module lesfac)

(load-macsyma-macros rzmac ratmac)

(defun getunhack (gen)
  (or (get gen 'unhacked) (pget gen)))

(defun frpoly? (r)
  (equal 1 (cdr r)))

(defmacro setcall (fn a b &rest args)
  `(prog1
       (first (setq ,a (,fn ,a ,b ,@args)))
     (setq ,b (third ,a)
           ,a (second ,a))))

(defun pquocof (p q)
  (let ((qq (testdivide p q)))
    (if qq
        (values q qq 1)
        (values 1 p q))))

(defun polyst (a)
  (if (pcoefp a)
      (list a)
      (cons (cons (car a) (cadr a)) (polyst (caddr a)))))

(defun cdinf (a b both)
  (cond ((or (pcoefp a) (pcoefp b))
         (values 1 a b))
	(t
         (setq a (ncons (copy-tree a))
               b (ncons (if both (copy-tree b) b)))
         (values (cd1 a b both) (car a) (car b)))))

(defun cd1 (a b both)
  (cond ((or (pcoefp (car a)) (pcoefp (car b))) 1)
	((eq (caar a) (caar b))
	 (ptimes (pexpt (pget (caar a))	;CHECK FOR ALG. TRUNC.
			(prog1 (cond (both (+ (cadar a) (cadar b))) (t (cadar a)))
			  (rplaca a (caddar a))
			  (cond (both (rplaca b (caddar b)))
				(t (setq b (cddar b))))))
		 (cd1 a b both)))
	((pointergp (caar a) (caar b)) (cd1 (cddar a) b both))
	(t (cd1 a (cddar b) both))))

(defun lmake (p l)
  (cond ((pcoefp p)
         (cons p l))
	((get (car p) 'unhacked)
	 (lmake (caddr p) (cons (cons (car p) (cadr p)) l)))
	(t
         (setq l (lmake (caddr p) l))
         (rplaca l (list (car p) (cadr p) (car l))))))

(defun lmake2 (p l)
  (setq l (lmake p l))
  (mapc #'(lambda (x) (rplaca x (getunhack (car x)))) (cdr l))
  (if (equal (car l) 1)
      (cdr l)
      (rplaca l (cons (car l) 1))))

(defun pmake (l)
  (cond ((null l)
         1)
	((zerop (cdar l))
         (pmake (cdr l)))
	((numberp (caar l))	     ;CLAUSE SHOULD BE ELIMINATED ASAP
	 (ptimes (cexpt (caar l) (cdar l)) (pmake (cdr l))))
	(t
         (ptimes (list (caar l) (cdar l) 1) (pmake (cdr l))))))

(defun fpgcdco (p q)
  (let (($ratfac nil)
        (gcdl nil))                        ;FACTORED PGCDCOFACTS
    (if (or (pcoefp p) (pcoefp q))
        (values-list (pgcdcofacts p q))
        (values (ptimeschk (setcall pgcdcofacts p q)
                           (car (setq p (lmake p nil)
                                      q (lmake q nil)
                                      gcdl (mapcar #'pmake (lgcd1 (cdr p) (cdr q))))))
                (ptimeschk (car p) (cadr gcdl))
                (ptimeschk (car q) (caddr gcdl))))))

(defun facmgcd (pl)            ;GCD OF POLY LIST FOR EZGCD WITH RATFAC
  (do ((l (cdr pl) (cdr l))
       (ans nil)
       (gcd (car pl)))
      ((null l) (cons gcd (nreverse ans)))
    (multiple-value-bind (g x y) (fpgcdco gcd (car l))
      (cond ((equal g 1)
             (return (cons 1 pl)))
            ((null ans)
             (setq ans (list x)))
            ((not (equal x 1))
             (do ((l2 ans (cdr l2)))
                 ((null l2))
               (rplaca l2 (ptimes x (car l2))))))
      (push y ans)
      (setq gcd g))))

;;; NOTE: ITEMS ON VARLIST ARE POS. NORMAL
;;; INTEGER COEF GCD=1 AND LEADCOEF. IS POS.

(defun lgcd1 (a b)
  (prog (ptlist g bj c t1 d1 d2 dummy)
     (setq ptlist (mapcar #'(lambda (ig) (declare (ignore ig)) b) a))
     (do ((a a (cdr a))
	  (ptlist ptlist (cdr ptlist)))
	 ((null a))
       (do ((ai (getunhack (caar a)))
	    (b (car ptlist) (cdr b)))
	   ((null b))
	 (and (zerop (cdar b)) (go nextb))
	 (setq d1 1 d2 1)
	 (setq bj (getunhack (caar b)))
	 (setq c (cond ((pirredp (caar a))
			(if (pirredp (caar b))
                            1
                            (multiple-value-setq (dummy bj ai) (pquocof bj ai))))
		       ((pirredp (caar b))
                        (multiple-value-setq (dummy ai bj) (pquocof ai bj)))
		       (t
                        (setcall pgcdcofacts ai bj))))
	 (cond ((equal c 1) (go nextb))
	       ((equal ai 1) (go bloop)))
	aloop
	 (when (setq t1 (testdivide ai c))
           (setq ai t1)
           (incf d1)
           (go aloop))
	bloop
	 (and (= d1 1)
	      (not (equal bj 1))
	      (do ((t1
		    (testdivide bj c)
		    (testdivide bj c)))
		  ((null t1))
		(setq bj t1 d2 (1+ d2))))
	 (setq g (cons (cons (makprodg c t)
			     (min (setq d1 (* d1 (cdar a)))
				  (setq d2 (* d2 (cdar b)))))
		       g))
	 (cond ((> d1 (cdar g))
		(rplacd (last a)
			(ncons (cons (caar g) (- d1 (cdar g)))))
		(rplacd (last ptlist) (ncons (cdr b)))))
	 (cond ((> d2 (cdar g))
		(rplacd (last b)
			(ncons (cons (caar g) (- d2 (cdar g)))))))
	 (rplaca (car a) (makprodg ai t))
	 (rplaca (car b) (makprodg bj t))
	 (and (equal bj 1) (rplacd (car b) 0))
	 (and (equal ai 1) (rplacd (car a) 0) (return nil))
	nextb))
     (return (list g a b))))

(defun makprodg (p sw)
  (if (pcoefp p)
      p
      (car (makprod p sw))))

(defun dopgcdcofacts (x y)
  (let (($gcd $gcd)
        ($ratfac nil))
    (unless (member $gcd *gcdl* :test #'eq)
      (setq $gcd '$ez))
    (values-list (pgcdcofacts x y))))

(defun facrplus (x y)
  (let ((a (car x))
	(b (cdr x))
	(c (car y))
	(d (cdr y))
        dummy)
    (multiple-value-setq (x a c) (dopgcdcofacts a c))
    (multiple-value-setq (y b d) (fpgcdco b d))
    (setq a (makprod (pplus (pflatten (ptimeschk a d))
                            (pflatten (ptimeschk b c))) nil))
    (setq b (ptimeschk b d))
    (cond ($algebraic
           (setq y (ptimeschk y b))
           (multiple-value-setq (dummy y a) (fpgcdco y a)) ;for unexpected gcd
           (cons (ptimes x a) y))
	  (t
           (multiple-value-setq (c y b) (cdinf y b nil))
           (multiple-value-setq (dummy y a) (fpgcdco y a))
           (cons (ptimes x a) (ptimeschk y (ptimeschk c b)))))))

(defun mfacpplus (l)
  (let (($gcd (or $gcd '$ez))
	($ratfac nil)
	(g nil))
    (setq g (oldcontent2 (sort (copy-list l) 'contodr) 0))
    (cond ((pzerop g) g)
	  ((do ((a (pflatten (pquotient (car l) g))
		   (pplus a (pflatten (pquotient (car ll) g))))
		(ll (cdr l) (cdr ll)))
	       ((null ll) (ptimes g (makprod a nil))))))))

(defun  facrtimes (x y gcdsw)
  (if (not gcdsw)
      (cons (ptimes (car x) (car y)) (ptimeschk (cdr x) (cdr y)))
      ;; gcdsw = true
      (multiple-value-bind (g1 g2 g3) (cdinf (car x) (car y) t)
        (multiple-value-bind (h1 h2 h3) (cdinf (cdr x) (cdr y) t)
          (multiple-value-bind (x1 x2 x3) (fpgcdco g2 h3)
            (declare (ignore x1))
            (multiple-value-bind (y1 y2 y3) (fpgcdco g3 h2)
              (declare (ignore y1))
              (cons (ptimes g1 (ptimes x2 y2))
                    (ptimeschk h1 (ptimeschk x3 y3)))))))))

(defun pfacprod (poly) 			;FOR RAT3D
  (if (pcoefp poly)
      (cfactor poly)
      (nconc (pfacprod (caddr poly)) (list (pget (car poly)) (cadr poly)))))

(defun fpcontent (poly)
  (let (($ratfac nil))			;algebraic uses
    (setq poly (oldcontent poly))	;rattimes?
    (let ((a (lowdeg (cdadr poly))))	;main var. content
      (when (plusp a)
        (setq a (list (caadr poly) a 1))
        (setq poly (list (ptimes (car poly) a)
                         (pquotient (cadr poly) a)))))
    (if (pminusp (cadr poly))
	(list (pminus (car poly)) (pminus (cadr poly)))
	poly)))

;; LOWDEG written to compute the lowest degree of a polynomial. - RZ

(defun lowdeg (p)
  (do ((l p (cddr l)))
      ((null (cddr l)) (car l))))

(defun makprod (poly contswitch)
  (cond ((pureprod poly) poly)
	((null (cdddr poly))
	 (ptimes (list (car poly) (cadr poly) 1)
		 (makprod (caddr poly) contswitch)))
	(contswitch
         (makprod1 poly))
	(t (setq poly (fpcontent poly))
	   (ptimes (makprod (car poly) contswitch) (makprod1 (cadr poly))))))

(defun makprod1 (poly)
  (do ((v varlist (cdr v))
       (g genvar (cdr g))
       (p (pdis poly)))
      ((null v) (maksymp poly))
    (and (alike1 p (car v)) (return (pget (car g))))))

(defun maksym (p)
  (let ((g (gensym))
        (e (pdis p)))
    (putprop g e 'disrep)
    (valput g (1- (valget (car genvar))))
    (push g genvar)
    (push e varlist)
    (putprop g p 'unhacked)
    g))

(defun maksymp (p)
  (if (atom p)
      p
      (pget (maksym p))))

(defun pflatten (h)
  (do ((m (listovars h) (cdr m)))
      ((null m) (return-from pflatten h))
    (unless (let ((p (getunhack (car m))))
              (or (null p) (eq (car m) (car p))))
     (return-from pflatten (let (($ratfac nil)) (pflat1 h))))))

(defun pflat1 (p)
  (cond ((pcoefp p) p)
	((null (cdddr p))
	 (ptimes (pexpt (getunhack (car p)) (cadr p)) (pflat1 (caddr p))))
	(t (do ((val (getunhack (car p)))
		(ld (cadr p) (car a))
		(a (cdddr p) (cddr a))
		(ans (pflat1 (caddr p))))
	       ((null a) (ptimes ans (pexpt val ld)))
	     (setq ans (pplus (ptimes ans (pexpt val (- ld (car a))))
                              (pflat1 (cadr a))))))))

(defun pirredp (x)
  (and (setq x (get x 'disrep))
     (or (atom x) (member 'irreducible (cdar x) :test #'eq))))
