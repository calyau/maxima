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
(macsyma-module db)

(load-macsyma-macros mrgmac)

;; This file uses its own special syntax which is set up here.  The function
;; which does it is defined in LIBMAX;MRGMAC.  It sets up <, >, and : for
;; structure manipulation.  A major bug with this package is that the code is
;; almost completely uncommented.  Someone with nothing better to do should go
;; through it, figure out how it works, and write it down.
;; Note: After recompiling all of macsyma for the Lispm it was found
;;       that some files were compiled with the syntax of ":" set up
;;       incorrectly. The (MODE-SYNTAX-OFF) function, which calls
;;       undocumented system-internal routines evidently did not work anymore.
;;       Therefore I removed the need for MODE-SYNTAX-ON from this file.
;;       7:57pm  Thursday, 25 February 1982 -GJC


;; On systems which cons fixnums, a fixnum is used as a single label cell
;; and a pointer to the fixnum is passed around (i.e. the particular fixnum
;; is passed around.  On systems which have immediate fixnums, a single cons
;; cell is created and the fixnum is stored in the car of the cell.  Fixnums
;; are consed only in PDP-10 MacLisp and Franz Lisp.

#+(or pdp10 franz)
(eval-when (eval compile) (sstatus feature fixcons))
#+nil
(eval-when (eval compile) (set-nofeature 'fixcons))

(declare-top(genprefix db)
	    ;; LAB is not a special.  This declares all occurrences of LAB
	    ;; as a local or a parameter to be a fixnum.  This should really
	    ;; be done using a LOCAL-DECLARE around the entire file so as to
	    ;; make sure any global compiler state gets undone.
	    #+fixcons (fixnum lab)
	    (*lexpr context))

;; External specials
;; Please do not use DEFMVAR on these because some of them contain 
;; circular list structure, and we want to be able to load in the 
;; English version of the file at times.  (DEFMVAR tries to print 
;; out their values when the value in core is different from the 
;; value in the file.) - JPG
;; Why don't you set PRINLEVEL and PRINLENGTH in your macsyma? -GJC

(defmvar context 'global)
(defmvar contexts nil)
(defmvar current 'global)
(defmvar +labs nil)
(defmvar -labs nil)
(defmvar dbtrace nil)
(defmvar dbcheck nil)
(defmvar dobjects nil)
(defmvar nobjects nil)

;; Internal specials

(defmvar marks 0) 		
(declare-top (fixnum marks))
(defmvar +l)
(declare-top (fixnum +l))
(defmvar -l)
(declare-top (fixnum -l))
(defmvar ulabs nil)

(defmvar conindex 0)
(declare-top (fixnum conindex))
(defmvar connumber 50.)
(declare-top (fixnum connumber))

;; The most negative fixnum.  On the PDP-10, this is 1_35.

(defmvar lab-high-bit #-cl (rot 1 -1) #+cl most-negative-fixnum)
;; One less than the number of bits in a fixnum.  On the PDP-10, this is 35.
(defmvar labnumber (f1- (haulong lab-high-bit)))
;; A cell with the high bit turned on.
(defmvar lab-high-lab #+fixcons lab-high-bit #-fixcons (list lab-high-bit))

(declare-top(special +s +sm +sl -s -sm -sl labs lprs labindex lprindex world db*))

;; Macro for indirecting through the contents of a cell.

(defmacro unlab (cell) 
  #+fixcons cell #-fixcons `(car ,cell))

(defmacro setq-unlab (cell)
  #+fixcons nil
  #-fixcons `(setq ,cell (unlab ,cell)))

(defmacro setq-copyn (cell)
  #+fixcons nil
  #-fixcons `(setq ,cell (copyn ,cell)))

;; Conditionalize primitive functions used in this file.  These are in
;; LAP for Lisp implementations which cons fixnums.  This interface
;; is poorly designed since the meaning of COPYN is varies slightly
;; between systems.  In one case it means "take a cell and produce a
;; new one with the same contents".  In the other, it means "take an
;; immediate fixnum and return a cell containing it."  As a result of
;; this, #+FIXCONS conditionalizations appear in the actual source code.

#-fixcons
(progn 'compile
       (defmacro copyn (n) `(list ,n))
       (defmacro iorm (cell n)
	 `(rplaca ,cell (logior (car ,cell) (car ,n))))
       (defmacro xorm (cell n)
	 `(rplaca ,cell (logxor (car ,cell) (car ,n))))
       )

(defun xxorm (cell n)
  (xorm cell n))
;; The LAP for the PDP-10 version.

#+pdp10 (lap-a-list '(
		      (lap copyn subr)
		      (move tt 0 a)
		      (jsp t fwcons)
		      (popj p)
		      nil

		      (lap iorm subr)
		      (move b 0 b)
		      (iorm b 0 a)
		      (popj p)
		      nil

		      (lap xorm subr)
		      (move b 0 b)
		      (xorm b 0 a)
		      (popj p)
		      nil ))

#+franz
(progn 'compile
       (defmacro copyn (n) `(copyint* ,n))
       (defmacro iorm (cell n) `(replace ,cell (logior ,cell ,n)))
       (defmacro xorm (cell n) `(replace ,cell (logxor ,cell ,n))) )

(defprop global 1 cmark)
;;(eval-when ( load )
;;(ARRAY CONUNMRK NIL (f1+ CONNUMBER))
;;(ARRAY CONMARK T (f1+ CONNUMBER))
;;)

(defvar conunmrk (*array nil t (f1+ connumber)))
(defvar conmark (*array nil  t (f1+ connumber)))

(defmfun mark (x) (putprop x t 'mark))
(defmfun markp (x) (and (symbolp x) (zl-get x 'mark)))

;;
(defun zl-remprop (sym indicator)
  (cond ((symbolp sym) (remprop sym indicator))
	(t (remf (cdr sym) indicator))))

(defmfun unmrk (x) (zl-remprop x 'mark))

(defun marks (x) (cond ((numberp x)) ((atom x) (mark x)) (t (mapc #'marks x))))
(defun unmrks (x)
  (cond ((numberp x))
	((or (atom x) (numberp (car x))) (unmrk x))
	(t (mapc #'unmrks x))))

(progn 'compile
       (defmode type ()
	 (atom (selector +labs) (selector -labs) (selector data))
	 selector)
       (defmode indv ()
	 (atom (selector =labs) (selector nlabs) (selector data) (selector in))
	 selector)
       (defmode univ ()
	 (atom (selector =labs) (selector nlabs) (selector data) (selector un))
	 selector)
       (defmode datum ()
	 (atom (selector ulabs) (selector con) (selector wn))
	 selector)
       (defmode context ()
	 (atom (selector cmark fixnum 0) (selector subc) (selector data)))
       )




;; Is (COPYN 0) really needed in these next four macros instead of simply 0?
;; If the fixnum were to get clobbered, then it would seem that (LIST 0) would
;; be the correct thing to return in the #-FIXCONS case. -cwh

(defmacro +labz (x)
  `(cond ((+labs ,x))
    (t #+fixcons (copyn 0) #-fixcons '(0))))

(defmacro -labz (x)
  `(cond ((-labs ,x))
    (t #+fixcons (copyn 0) #-fixcons '(0))))

(defmacro =labz (x)
  `(cond ((=labs ,x))
    (t #+fixcons (copyn 0) #-fixcons '(0))))

(defmacro nlabz (x)
  `(cond ((nlabs ,x))
    (t #+fixcons (copyn 0) #-fixcons '(0))))

(defmacro ulabz (x)
  `(cond ((ulabs ,x))
    (t #+fixcons 0 #-fixcons '(0))))

(defmacro subp (&rest x)
  #-fixcons (setq x (mapcar #'(lambda (form) `(unlab ,form)) x))
  `(= ,(car x) (logand . ,x)))

(defun dbnode (x) (if (symbolp x) x (list x)))
(defun nodep (x) (or (atom x) (mnump (car x))))
(defun dbvarp (x) (getl x '(un ex)))

;; Is this supposed to return a fixnum or a cell?

(defun lab (n) (lsh 1 (f1- n)))

(defun lpr (m n)
  (cond ((do ((l lprs (cdr l))) ((null l))
	   (if (and (labeq m (caaar l)) (labeq n (cdaar l)))
	       (return (cdar l)))))
	((= (setq lprindex (f1- lprindex)) labindex) (break 'lpr t))
	(t (setq lprs (cons (cons (cons m n) (lsh 1 lprindex)) lprs))
	   (cdar lprs))))

(defun labeq (x y) (equal (logior x lab-high-bit) (logior y lab-high-bit)))

(defun marknd (nd)
  (cond ((+labs nd))
  	((= lprindex (setq labindex (f1+ labindex))) (break 'marknd t))
	(t (setq labs (cons (cons nd (lab labindex)) labs))
	   (beg nd (lab labindex))
	   (cdar labs))))

(defun dbv (x r)
  (declare (fixnum x r ))
  (do ((l lprs (cdr l)) (y 0)) ((null l) y)
    (declare (fixnum y))
    (if (and (not (= 0 (logand r (cdar l)))) (not (= 0 (logand x (caaar l)))))
	(setq y (logior (cdaar l) y)))))

(defun dba (r y)
  (declare (fixnum  r y ))
  (do ((l lprs (cdr l)) (x 0)) ((null l) x)
    (declare (fixnum x ))
    (if (and (not (= 0 (logand r (cdar l)))) (not (= 0 (logand (cdaar l) y))))
	(setq x (logior x (caaar l))))))
#-cl
(defun prlab (x)
  (setq-unlab x)
  (setq x (let ((*print-base* 2)) (exploden (boole boole-andc1 lab-high-bit x))))
  (do ((i (fixnum-remainder (length x) 3) 3)) ((null x))
    (do ((j i (f1- j))) ((= 0 j)) (tyo (car x)) (setq x (cdr x)))
    (tyo #\space)))

#+cl
(defun prlab (x)
  (setq-unlab x)
  (setq x (let ((*print-base* 2)(*read-base* 2))(and x (exploden (boole boole-andc1 lab-high-bit x)))))
  (do ((i (fixnum-remainder (length x) 3) 3)) ((null x))
    (do ((j i (f1- j))) ((= 0 j)) (tyo (car x)) (setq x (cdr x)))
    (tyo #\space)))

(defun onp (cl lab) (subp lab (+labz cl)))
(defun offp (cl lab) (subp lab (-labz cl)))
(defun onpu (lab fact) (subp lab (ulabz fact)))
(defmfun visiblep (dat) (and (not (ulabs dat)) (cntp dat)))

(defun cancel (lab dat)
  (cond
    ((setq db* (ulabs dat)) (iorm db* lab))
    (t (setq ulabs (cons dat ulabs))
       (setq-unlab lab)
       (putprop dat (copyn lab) 'ulabs))))

(defun beg (nd lab)
  (setq-copyn lab)
  (if (queue+p nd lab) 
      (if (null +s)
	  (setq +s (ncons nd) +sm +s +sl +s)
	  (setq +s (cons nd +s)))))

(defun beg- (nd lab)
  (setq-copyn lab)
  (if (queue-p nd lab)
      (if (null -s) (setq -s (ncons nd) -sm -s -sl -s)
	  (setq -s (cons nd -s)))))

(defun mid (nd lab)
  (if (queue+p nd lab)
      (cond
	((null +sm) (setq +s (ncons nd) +sm +s +sl +s))
	(t (rplacd +sm (cons nd (cdr +sm)))
	   (if (eq +sm +sl) (setq +sl (cdr +sl)))
	   (setq +sm (cdr +sm))))))

(defun mid- (nd lab)
  (if (queue-p nd lab)
      (cond
	((null -sm) (setq -s (ncons nd) -sm -s -sl -s))
	(t (rplacd -sm (cons nd (cdr -sm)))
	   (if (eq -sm -sl) (setq -sl (cdr -sl)))
	   (setq -sm (cdr -sm))))))

(defun end (nd lab)
  (if (queue+p nd lab)
      (cond
	((null +sl) (setq +s (ncons nd) +sm +s +sl +s))
	(t (rplacd +sl (ncons nd))
	   (setq +sl (cdr +sl))))))

(defun end- (nd lab)
  (if (queue-p nd lab) 
      (cond
	((null -sl) (setq -s (ncons nd) -sm -s -sl -s))
	(t (rplacd -sl (ncons nd))
	   (setq -sl (cdr -sl))))))

(defun queue+p (nd lab)
  (cond ((null (setq db* (+labs nd)))
	 (setq +labs (cons nd +labs))
	 (setq-unlab lab)
	 (put nd (copyn (logior lab-high-bit lab)) '+labs))
	((subp lab db*) nil)
	((subp lab-high-lab db*) (iorm db* lab) nil)
	(t (iorm db* (logior lab-high-bit (unlab lab))))))

(defun queue-p (nd lab)
  (cond ((null (setq db* (-labs nd)))
	 (setq -labs (cons nd -labs))
	 (setq-unlab lab)
	 (put nd (copyn (logior lab-high-bit lab)) '-labs))
	((subp lab db*) nil)
	((subp lab-high-lab db*) (iorm db* lab) nil)
	(t (iorm db* (logior lab-high-bit (unlab lab))))))

(defun dq+ () 
  (if +s (prog2 (xxorm (zl-get (car +s) '+labs) ;(+LABS (CAR +S))
		       lab-high-lab)
	     (car +s)
	   (cond ((not (eq +s +sm)) (setq +s (cdr +s)))
		 ((not (eq +s +sl)) (setq +s (cdr +s) +sm +s))
		 (t (setq +s nil +sm nil +sl nil))))))

(defun dq- ()
  (if -s (prog2 (xorm (-labs (car -s)) lab-high-lab)
	     (car -s)
	   (cond ((not (eq -s -sm)) (setq -s (cdr -s)))
		 ((not (eq -s -sl)) (setq -s (cdr -s) -sm -s))
		 (t (setq -s nil -sm nil -sl nil))))))

(defmfun clear ()
  (if dbtrace (mtell "~%Clearing ~A" marks))
  (mapc #'(lambda (sym) (_ (sel sym +labs) nil)) +labs)
  (mapc #'(lambda (sym) (_ (sel sym -labs) nil)) -labs)
  (mapc #'(lambda (sym) (zl-remprop sym 'ulabs)) ulabs)
  (setq +s nil +sm nil +sl nil -s nil -sm nil -sl nil 
	labs nil lprs nil labindex 0 lprindex labnumber  
	marks 0 +labs nil -labs nil ulabs nil)
  (contextmark))

(defmfun truep (pat)
  (clear)
  (cond ((atom pat) pat)
	((prog2 (setq pat (mapcar #'semant pat)) nil))
	((eq (car pat) 'kind) (beg (cadr pat) 1) (beg- (caddr pat) 1) (propg))
	(t (beg (cadr pat) 1) (beg- (caddr pat) 2) (beg (car pat) (lpr 1 2)) (propg))))

(defmfun falsep (pat)
  (clear)
  (cond ((eq (car pat) 'kind)
	 (beg (cadr pat) 1) (beg (caddr pat) 1) (propg))))

(defmfun isp (pat) (cond ((truep pat)) ((falsep pat) nil) (t 'unknown)))

(defmfun kindp (x y &aux #+lispm (default-cons-area working-storage-area ))
  (if (not (symbolp x)) (merror "KINDP called on a non-symbolic atom."))
  (clear)
  (beg x 1)
  (do ((p (dq+) (dq+))) ((null p))
    (if (eq y p) (return t) (mark+ p (+labs p)))))

(defmfun true* (pat)
  (let ((dum (semant pat))) (if dum (cntxt (ind (ncons dum)) context))))

(defmfun fact (fun arg val) (cntxt (ind (datum (list fun arg val))) context))

(defmfun kind (x y &aux #+kcl (y y))
  (setq y (datum (list 'kind x y))) (cntxt y context) (addf y x))

(defmfun par (s y)
  (setq y (datum (list 'par s y))) (cntxt y context)
  (mapc #'(lambda (lis) (addf y lis)) s))

(defmfun datum (pat) (ncons pat))

(defun ind (dat)
  (mapc #'(lambda (lis) (ind1 dat lis)) (cdar dat))
  (mapc #'ind2 (cdar dat))
  dat)

(defun ind1 (dat pat)
  (cond ((not (nodep pat)) (mapc #'(lambda (lis) (ind1 dat lis)) pat))
	((or (markp pat) (eq 'unknown pat)))
	(t (addf dat pat) (mark pat))))

(defun ind2 (nd) (if (nodep nd) (unmrk nd) (mapc #'ind2 nd)))


(defmfun addf (dat nd &aux #+lispm (default-cons-area working-storage-area ))
  (_ (sel nd data) (cons dat (sel nd data))))

(defmfun maxima-remf (dat nd) (_ (sel nd data) (fdel dat (sel nd data))))

(defun fdel (fact data)
  (cond
    ((and (eq (car fact) (caaar data))
	  (eq (cadr fact) (cadaar data))
	  (eq (caddr fact) (caddaar data)))
     (cdr data))
    (t (do ((ds data (cdr ds)) (d)) ((null (cdr ds)))
	 (setq d (caadr ds))
	 (cond ((and (eq (car fact) (car d))
		     (eq (cadr fact) (cadr d))
		     (eq (caddr fact) (caddr d)))
		(_ (sel d con data) (delq d (sel d con data)))
		(rplacd ds (cddr ds)) (return t))))
       data)))

(defun semantics (pat) (if (atom pat) pat (list (semant pat))))

(defun db-mnump (x)
  (or (numberp x)
      (and (not (atom x))
	   (not (atom (car x)))
	   (memq (caar x) '(rat bigfloat)))))       

(defun semant (pat) 
  (cond ((symbolp pat) (or (zl-get pat 'var) pat))
	((db-mnump pat) (dintnum pat))
	(t (mapcar #'semant pat))))

(defmfun dinternp (x)
  (cond ((mnump x) (dintnum x)) 
	((atom x) x) 
	((assol x dobjects))))

(defmfun dintern (x &aux #+lispm (default-cons-area working-storage-area ))
  (cond ((mnump x) (dintnum x))
	((atom x) x)
	((assol x dobjects))
	(t (setq dobjects (cons (dbnode x) dobjects))
	   (car dobjects))))

(defun dintnum (x)
  (cond ((assol x nobjects))
	((progn (setq x (dbnode x)) nil))
	((null nobjects) (setq nobjects (list x)) x)
	((eq '$pos (rgrp (car x) (caar nobjects)))
	 (let ((context 'global))
	   (fact 'mgrp x (car nobjects)))
	 (setq nobjects (cons x nobjects))  x)
	(t (do ((lis nobjects (cdr lis)) (context '$global))
	       ((null (cdr lis))
		(let ((context 'global))
		  (fact 'mgrp (car lis) x)) (rplacd lis (list x)) x)
	     (cond ((eq '$pos (rgrp (car x) (caadr lis)))
		    (let ((context 'global))
		      (fact 'mgrp (car lis) x) (fact 'mgrp x (cadr lis)))
		    (rplacd lis (cons x (cdr lis)))
		    (return x)))))))

(defmfun doutern (x) (if (atom x) x (car x)))

(defmfun untrue (pat)
  (kill (car pat) (semant (cadr pat)) (semant (caddr pat))))

(defmfun kill (fun arg val) (kill2 fun arg val arg) (kill2 fun arg val val))

(defun kill2 (fun arg val cl)
  (cond ((not (atom cl)) (mapc #'(lambda (lis) (kill2 fun arg val lis)) cl))
	((numberp cl))
	(t (_ (sel cl data) (kill3 fun arg val (sel cl data))))))

(defun kill3 (fun arg val data)
  (cond
    ((and (eq fun (caaar data))
	  (eq arg (cadaar data)) (eq val (caddaar data)))
     (cdr data))
    (t (do ((ds data (cdr ds)) (d)) ((null (cdr ds)))
	 (setq d (caadr ds))
	 (cond
	   ((not (and (eq fun (car d))
		      (eq arg (cadr d))
		      (eq val (caddr d))))
	    t)
	   (t (_ (sel d con data) (delq d (sel d con data)))
	      (rplacd ds (cddr ds)) (return t))))
       data)))

(defmfun unkind (x y)
  (setq y (car (datum (list 'kind x y))))
  (kcntxt y context)
  (maxima-remf y x))

(defmfun remov (fact)
  (remov4 fact (cadar fact))
  (remov4 fact (caddar fact)))

(defun remov4 (fact cl)	 
  (cond ((or (symbolp cl)		;if CL is a symbol or
	     (and (consp cl) ;an interned number, then we want to REMOV4 FACT
		  (numberp (car cl))))	;from its property list.
	 (_ (sel cl data) (delq fact (sel cl data))))
	((or (atom cl) (atom (car cl)))) ;if CL is an atom (not a symbol)
					;or its CAR is an atom then we don't want to do
					;anything to it.
	(t (mapc #'(lambda (lis) (remov4 fact lis))
		 (cond ((atom (caar cl)) (cdr cl)) ;if CL's CAAR is
					;an atom, then CL is an expression, and
					;we want to REMOV4 FACT from the parts
					;of the expression. 
		       ((atom (caaar cl)) (cdar cl)))))))
					;if CL's CAAAR is an atom, then CL is a
					;fact, and we want to REMOV4 FACT from
					;the parts of the fact.

(defmfun killframe (cl)
  (mapc #'remov (sel cl data))
  (zl-remprop cl '+labs) (zl-remprop cl '-labs)
  (zl-remprop cl 'obj) (zl-remprop cl 'var)
  (zl-remprop cl 'fact)
  (zl-remprop cl 'wn))

(defmfun activate n 
  (do ((i 1 (f1+ i))) ((> i n))
    (cond
      ((memq (arg i) contexts) nil)
      (t (setq contexts (cons (arg i) contexts))
	 (cmark (arg i))))))

(defmfun deactivate n 
  (do ((i 1 (f1+ i))) ((> i n))
    (cond
      ((not (memq (arg i) contexts)) nil)
      (t (cunmrk (arg i))
	 (setq contexts (delq (arg i) contexts))))))

(defmfun context n (newcon (listify n)))

(defun newcon (c)
  (if (> conindex connumber) (gccon))
  (setq c (if (null c) (list '*gc nil) (list '*gc nil 'subc c)))
  (store (aref conunmrk conindex) c)
  (store (aref conmark conindex) (cdr c))
  (setq conindex (f1+ conindex))
  c)

;; To be used with the WITH-NEW-CONTEXT macro.
(defun context-unwinder ()
  (killc (aref conmark conindex))
  (setq conindex (f1- conindex))
  (setf (aref conunmrk conindex) ())
  )

(defun gccon () 
  (gccon1)
  (when (> conindex connumber)
    #+gc (gc)
    (gccon1)
    (when (> conindex connumber)
      (merror "~%Too many contexts."))))

(defun gccon1 ()
  (setq conindex 0)
  (do ((i 0 (f1+ i))) ((> i connumber))
    (cond
      ((not (eq (aref conmark i) (cdr (aref conunmrk i))))
       (killc (aref conmark i)))
      (t (store (aref conunmrk conindex) (aref conunmrk i))
	  
	 (store (aref conmark conindex) (aref conmark i))
	  
	 (setq conindex (f1+ conindex))))))

(defmfun cntxt (dat con)
  (if (not (atom con)) (setq con (cdr con)))
  (put con (cons dat (zl-get con 'data)) 'data)
  (if (not (eq 'global con)) (put dat con 'con))
  dat)

(defmfun kcntxt (fact con)
  (if (not (atom con)) (setq con (cdr con)))
  (put con (fdel fact (zl-get con 'data)) 'data)
  (if (not (eq 'global con)) (zl-remprop fact 'con))
  fact)

(defun cntp (f)
  (cond ((not (setq f (sel f con))))
	((setq f (zl-get f 'cmark)) (> f 0))))

(defmfun contextmark (&aux #+lispm (default-cons-area working-storage-area ))
  (let ((con context))
    (unless (eq current con)
      (cunmrk current) (setq current con) (cmark con))))

(defun cmark (con)
  (if (not (atom con)) (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (putprop con (if cm (f1+ cm) 1) 'cmark)
    (mapc #'cmark (zl-get con 'subc))))

(defun cunmrk (con)
  (if (not (atom con)) (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (cond (cm (putprop con (f1- cm) 'cmark)))
    (mapc #'cunmrk (zl-get con 'subc))))

(defmfun killc (con)
  (contextmark)
  (cond ((not (null con))
	 (mapc #'remov (zl-get con 'data))
	 (zl-remprop con 'data)
	 (zl-remprop con 'cmark)
	 (zl-remprop con 'subc)))
  t)

(defun propg ()
  (do ((x) (lab)) (nil)
    (cond ((setq x (dq+))
	   (setq lab (+labs x))
	   (if (= 0 (logand (unlab lab) (unlab (-labz x))))
	       (mark+ x lab) (return t)))
	  ((setq x (dq-))
	   (setq lab (-labs x))
	   (if (= 0 (logand (unlab lab) (unlab (+labz x))))
	       (mark- x lab) (return t)))
	  (t (return nil)))))

(defun mark+ (cl lab &aux #+lispm (default-cons-area working-storage-area ))
  (cond (dbtrace (setq marks (f1+ marks))
		 (mtell "~%Marking ~A +" cl) (prlab lab)))
  (mapc #'(lambda (lis) (mark+0 cl lab lis)) (sel cl data)))

(defun mark+0 (cl lab fact)
  (cond (dbcheck (mtell "~%Checking ~A from ~A+" (car fact) cl) (prlab lab)))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	((null (sel fact wn)) (mark+1 cl lab fact))
	((onp (sel fact wn) world) (mark+1 cl lab fact))
	((offp (sel fact wn) world) nil)
	(t (mark+3 cl lab fact))))

(defun mark+1 (cl lab dat)
  (cond ((eq (caar dat) 'kind)
	 (if (eq (cadar dat) cl) (mid (caddar dat) lab))) ; E1
	((eq (caar dat) 'par)
	 (if (not (eq (caddar dat) cl))
	     (progn (cancel lab dat)	; PR1
		    (mid (caddar dat) lab)
		    (do ((lis (cadar dat) (cdr lis))) ((null lis))
		      (if (not (eq (car lis) cl)) (mid- (car lis) lab))))))
	((eq (cadar dat) cl)
	 (if (+labs (caar dat))		; V1
	     (end (caddar dat) (dbv lab (+labs (caar dat)))))
	 (if (-labs (caddar dat))	; F4
	     (end- (caar dat) (lpr lab (-labs (caddar dat))))))))

(defun mark+3 (cl lab dat) cl lab	;Ignored
       (ifn (= 0 (logand (unlab (+labz (caddar dat)))
			 (unlab (dbv (+labz (cadar dat)) (-labz (caar dat))))))
	    (beg- (sel dat wn) world)))


(defun mark- (cl lab &aux #+lispm (default-cons-area working-storage-area ))
  (when dbtrace
    (setq marks (f1+ marks)) (mtell "Marking ~A -" cl) (prlab lab))
  (mapc #'(lambda (lis) (mark-0 cl lab lis)) (sel cl data)))

(defun mark-0 (cl lab fact)
  (when dbcheck (mtell "~%Checking ~A from ~A-" (car fact) cl) (prlab lab))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	((null (sel fact wn)) (mark-1 cl lab fact))
	((onp (sel fact wn) world) (mark-1 cl lab fact))
	((offp (sel fact wn) world) nil)))

(defun mark-1 (cl lab dat  &aux #+lispm (default-cons-area working-storage-area ))
  (cond ((eq (caar dat) 'kind)
	 (if (not (eq (cadar dat) cl)) (mid- (cadar dat) lab)))	; E4
	((eq (caar dat) 'par)
	 (if (eq (caddar dat) cl)
	     (prog2 (cancel lab dat)	; S4
		 (do ((lis (cadar dat) (cdr lis))) ((null lis)) (mid- (car lis) lab)))
	     (progn (setq-unlab lab)	; ALL4
		    (do ((lis (cadar dat) (cdr lis))) ((null lis))
		      (setq lab (logand (unlab (-labz (car lis))) lab)))
		    (setq-copyn lab)
		    (cancel lab dat)
		    (mid- (caddar dat) lab))))
	((eq (caddar dat) cl)
	 (if (+labs (caar dat))		; A2
	     (end- (cadar dat) (dba (+labs (caar dat)) lab)))
	 (if (+labs (cadar dat))	; F6
	     (end- (caar dat) (lpr (+labs (cadar dat)) lab))))))

;;	     in out                    in out                  ins  in out
;;	-----------		-------------             ----------------
;;	E1 |     +		INV1 |     +              AB1 |(+)  +   +
;;	E2 |     -		INV2 |     -              AB2 |(+)  -   +
;;	E3 | +			INV3 | +                  AB3 |(+)  +   -
;;	E4 | -			INV4 | -                  AB4 |(+)  -   -
;;                                                         AB5 |(-)  +   +
;;            in out                    in out             AB6 |(-)  -   +
;;       -----------             -------------             AB7 |(-)  +   -
;;       S1 |    (+)             ALL1 |(+)  +              AB8 |(-)  -   -
;;       S2 |    (-)             ALL2 |(+)  -
;;       S3 |(+)                 ALL3 |(-)  +
;;       S4 |(-)                 ALL4 |(-)  -



;;	     in rel out	         in rel out	     in rel out
;;	---------------	    ---------------	---------------
;;	V1 |    (+)  +	    A1 | +  (+)		F1 |     +  (+)
;;	V2 |    (+)  -	    A2 | -  (+)		F2 |     +  (-)
;;	V3 |    (-)  +	    A3 | +  (-)		F3 |     -  (+)
;;	V4 |    (-)  -	    A4 | -  (-)		F4 |     -  (-)
;;						F5 |(+)  +
;;						F6 |(+)  -
;;						F7 |(-)  +
;;						F8 |(-)  -


(defun uni (p1 p2 al)
  (cond ((dbvarp p1) (dbunivar p1 p2 al))
	((nodep p1)
	 (cond ((dbvarp p2) (dbunivar p2 p1 al))
	       ((nodep p2) (if (eq p1 p2) al))))
	((dbvarp p2) (dbunivar p2 p1 al))
	((nodep p2) nil)
	((setq al (uni (car p1) (car p2) al)) (uni (cdr p1) (cdr p2) al))))

(defun dbunivar (p v al)
  (let ((dum (assq p al)))
    (cond ((null dum) (cons (cons p v) al))
	  (t (uni (cdr dum) v al)))))

;; Undeclarations for the file:

#-nil
(declare-top(notype lab))
