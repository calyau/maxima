;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module db)

(load-macsyma-macros mrgmac)

;; This file uses its own special syntax which is set up here.  The function
;; which does it is defined in LIBMAX;MRGMAC.  It sets up <, >, and : for
;; structure manipulation.  A major bug with this package is that the code is
;; almost completely uncommented.  Someone with nothing better to do should go
;; through it, figure out how it works, and write it down.

;; External specials

(defvar context 'global)
(defvar contexts nil)
(defvar current 'global)
(defvar dbtrace nil)
(defvar dobjects nil)

;; Internal specials

(defvar *nobjects* nil)
(defvar *dbcheck* nil)
(defvar +l)
(defvar -l)

(defvar *conindex* 0)
(defvar *connumber* 50)

(defconstant +lab-high-bit+ most-negative-fixnum)

;; One less than the number of bits in a fixnum.
(defconstant +labnumber+ (1- (integer-length +lab-high-bit+)))

;; A cell with the high bit turned on.
(defvar *lab-high-lab* (list +lab-high-bit+))

;; Variables that are set by (clear)
(defvar +s)
(defvar +sm)
(defvar +sl)
(defvar -s)
(defvar -sm)
(defvar -sl)
(defvar *labs*)
(defvar *lprs*)
(defvar *labindex*)
(defvar *lprindex*)
(defvar *marks* 0)
(defvar +labs nil)
(defvar -labs nil)
(defvar ulabs nil)


(defvar *db*)

;; Macro for indirecting through the contents of a cell.

(defmacro unlab (cell)
  `(car ,cell))

(defmacro copyn (n)
  `(list ,n))

(defmacro iorm (cell n)
  `(rplaca ,cell (logior (car ,cell) (car ,n))))

(defmacro xorm (cell n)
  `(rplaca ,cell (logxor (car ,cell) (car ,n))))

(defprop global 1 cmark)

(defvar conunmrk (make-array (1+ *connumber*) :initial-element nil))
(defvar conmark  (make-array (1+ *connumber*) :initial-element nil))

(defun mark (x)
  (putprop x t 'mark))

(defun markp (x)
  (and (symbolp x) (get x 'mark)))

(defun unmrk (x)
  (zl-remprop x 'mark))

(defun marks (x)
  (cond ((numberp x))
	((atom x) (mark x))
	(t (mapc #'marks x))))

(defun unmrks (x)
  (cond ((numberp x))
	((or (atom x) (numberp (car x))) (unmrk x))
	(t (mapc #'unmrks x))))

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
  (atom (selector ulabs) (selector con))
  selector)

(defmode context ()
  (atom (selector cmark fixnum 0) (selector subc) (selector data)))

(defmacro +labz (x)
  `(cond ((+labs ,x))
    (t '(0))))

(defmacro -labz (x)
  `(cond ((-labs ,x))
    (t '(0))))

(defmacro =labz (x)
  `(cond ((=labs ,x))
    (t '(0))))

(defmacro nlabz (x)
  `(cond ((nlabs ,x))
    (t '(0))))

(defmacro ulabz (x)
  `(cond ((ulabs ,x))
    (t '(0))))

(defmacro subp (&rest x)
  (setq x (mapcar #'(lambda (form) `(unlab ,form)) x))
  `(= ,(car x) (logand ,@x)))

(defun dbnode (x)
  (if (symbolp x) x (list x)))

(defun nodep (x)
  (or (atom x) (mnump (car x))))

(defun dbvarp (x)
  (getl x '(un ex)))

(declaim (inline dnump))
(defun dnump (nd)
  "Is ND a node cell standing for a number?"
  ;; Symbols are atoms, and expression nodes have a non-MNUMP CAR,
  ;; so this separates the three kinds cleanly:
  (and (consp nd) (mnump (car nd))))

(defun lab (n)
  (ash 1 (1- n)))

(defun lpr (m n)
  (cond ((do ((l *lprs* (cdr l)))
	     ((null l))
	   (if (and (labeq m (caaar l)) (labeq n (cdaar l)))
	       (return (cdar l)))))
	((= (decf *lprindex*) *labindex*)
	 (break))
	(t
	 (push (cons (cons m n) (ash 1 *lprindex*)) *lprs*)
	 (cdar *lprs*))))

(defun labeq (x y)
  (= (logior x +lab-high-bit+) (logior y +lab-high-bit+)))

(defun marknd (nd)
  (cond ((+labs nd))
	((= *lprindex* (incf *labindex*))
	 (break))
	(t (push (cons nd (lab *labindex*)) *labs*)
	   (beg nd (lab *labindex*))
	   (cdar *labs*))))

(defun dbv (x r)
  (do ((l *lprs* (cdr l))
       (y 0))
      ((null l) y)
    (unless (or (zerop (logand r (cdar l))) (zerop (logand x (caaar l))))
      (setq y (logior (cdaar l) y)))))

(defun dba (r y)
  (do ((l *lprs* (cdr l))
       (x 0))
      ((null l) x)
    (unless (or (zerop (logand r (cdar l))) (zerop (logand (cdaar l) y)))
      (setq x (logior x (caaar l))))))

(defun prlab (x)
  (setq x (unlab x))
  (when x
    (format t " ~,,' ,3:B" (logandc1 +lab-high-bit+ x))))

(defun onp (cl lab)
  (subp lab (+labz cl)))

(defun offp (cl lab)
  (subp lab (-labz cl)))

(defun onpu (lab fact)
  (subp lab (ulabz fact)))

(defun visiblep (dat)
  (and (not (ulabs dat)) (cntp dat)))

(defun cancel (lab dat)
  (cond ((setq *db* (ulabs dat))
	 (iorm *db* lab))
	(t
	 (push dat ulabs)
	 (setq lab (unlab lab))
	 (putprop dat (copyn lab) 'ulabs))))

(defun queue+p (nd lab)
  (cond ((atom (setq *db* (+labs nd)))
	 ;; No label, or a stale sign symbol left by DMARK: Start fresh.
	 (push nd +labs)
	 (setq lab (unlab lab))
	 (putprop nd (copyn (logior +lab-high-bit+ lab)) '+labs))
	((subp lab *db*)
	 nil)
	((subp *lab-high-lab* *db*)
	 (iorm *db* lab)
	 nil)
	(t
	 (iorm *db* (copyn (logior +lab-high-bit+ (unlab lab)))))))

(defun beg (nd lab)
  (setq lab (copyn lab))
  (if (queue+p nd lab)
      (if (null +s)
	  (setq +s (ncons nd)
		+sm +s
		+sl +s)
	  (push nd +s))))

(defun queue-p (nd lab)
  (cond ((null (setq *db* (-labs nd)))
	 (push nd -labs)
	 (setq lab (unlab lab))
	 (putprop nd (copyn (logior +lab-high-bit+ lab)) '-labs))
	((subp lab *db*)
	 nil)
	((subp *lab-high-lab* *db*)
	 (iorm *db* lab)
	 nil)
	(t
	 (iorm *db* (copyn (logior +lab-high-bit+ (unlab lab)))))))

(defun beg- (nd lab)
  (setq lab (copyn lab))
  (if (queue-p nd lab)
      (if (null -s)
	  (setq -s (ncons nd)
		-sm -s
		-sl -s)
	  (setq -s (cons nd -s)))))

(defun mid (nd lab)
  (if (queue+p nd lab)
      (cond ((null +sm)
	     (setq +s (ncons nd)
		   +sm +s
		   +sl +s))
	    (t
	     (rplacd +sm (cons nd (cdr +sm)))
	     (if (eq +sm +sl)
		 (setq +sl (cdr +sl)))
	     (setq +sm (cdr +sm))))))

(defun mid- (nd lab)
  (if (queue-p nd lab)
      (cond ((null -sm)
	     (setq -s (ncons nd)
		   -sm -s
		   -sl -s))
	    (t
	     (rplacd -sm (cons nd (cdr -sm)))
	     (when (eq -sm -sl)
	       (setq -sl (cdr -sl)))
	     (setq -sm (cdr -sm))))))

(defun end (nd lab)
  (if (queue+p nd lab)
      (cond ((null +sl)
	     (setq +s (ncons nd)
		   +sm +s
		   +sl +s))
	    (t
	     (rplacd +sl (ncons nd))
	     (setq +sl (cdr +sl))))))

(defun end- (nd lab)
  (if (queue-p nd lab)
      (cond ((null -sl)
	     (setq -s (ncons nd)
		   -sm -s
		   -sl -s))
	    (t
	     (rplacd -sl (ncons nd))
	     (setq -sl (cdr -sl))))))

(defun dq+ ()
  (if +s
      (prog2
	  (xorm (zl-get (car +s) '+labs) *lab-high-lab*)
	  (car +s)
	(cond ((not (eq +s +sm))
	       (setq +s (cdr +s)))
	      ((not (eq +s +sl))
	       (setq +s (cdr +s)
		     +sm +s))
	      (t
	       (setq +s nil
		     +sm nil
		     +sl nil))))))

(defun dq- ()
  (if -s
      (prog2
	  (xorm (-labs (car -s)) *lab-high-lab*)
	  (car -s)
	(cond ((not (eq -s -sm))
	       (setq -s (cdr -s)))
	      ((not (eq -s -sl))
	       (setq -s (cdr -s)
		     -sm -s))
	      (t
	       (setq -s nil
		     -sm nil
		     -sl nil))))))

(defun clear ()
  (when dbtrace
    (format *trace-output* "~%CLEAR: clearing ~A" *marks*))
  (mapc #'(lambda (sym) (push+sto (sel sym +labs) nil)) +labs)
  (mapc #'(lambda (sym) (push+sto (sel sym -labs) nil)) -labs)
  (mapc #'(lambda (sym) (zl-remprop sym 'ulabs)) ulabs)
  (setq +s nil
	+sm nil
	+sl nil
	-s nil
	-sm nil
	-sl nil
	*labs* nil
	*lprs* nil
	*labindex* 0
	*lprindex* +labnumber+
	*marks* 0
	+labs nil
	-labs nil
	ulabs nil)
  (contextmark))

(defun truep (pat)
  (clear)
  (cond ((atom pat) pat)
	((prog2 (setq pat (mapcar #'query-semant pat)) nil))
	((eq (car pat) 'kind)
	 (beg (cadr pat) 1)
	 (beg- (caddr pat) 1)
	 (propg))
	(t
	 (beg (cadr pat) 1)
	 (beg- (caddr pat) 2)
	 (beg (car pat) (lpr 1 2))
	 (propg))))

(defun falsep (pat)
  (clear)
  (cond ((eq (car pat) 'kind)
	 (beg (cadr pat) 1)
	 (beg (caddr pat) 1)
	 (propg))))

(defun isp (pat)
  (let ((isp 'unknown) #+ccl (err t))
    (ignore-errors
      (setq isp
	    (cond ((truep pat))
		  ((falsep pat) nil)
		  (t 'unknown)))
      #+ccl (setq err nil))
    #+ccl
    (when err
      (setq +labs nil))
    isp))

;; Return NIL for all non-symbols.
(defun kindp (x y)
  (when (and (symbolp x) (get x 'data))
    (clear)
    (beg x 1)
    (do () ((null +s))
      (let ((p (dq+)))
        (if (eq y p)
          (return t)
          (mark+ p (+labs p)))))))

(defun kind-any-of (x kinds)
  "Looks up the kind information on symbol X and returns the first kind that is
  encountered that is a member of KINDS. The order of symbols in KINDS doesn't
  affect the result. This function should only be used for mutually exclusive
  kinds, e.g. '$EVEN and '$ODD. Returns NIL if no matching kind is found.
  This is faster than (OR (KINDP X K1) (KINDP X K2) ...), since it only requires
  a single database query."
  (when (and (symbolp x) (get x 'data))
    (clear)
    (beg x 1)
    (do ((p (dq+) (dq+)))
        ((null p))
        (let ((k (member p kinds :test #'eq)))
        (if k
          (return (car k))
          (mark+ p (+labs p)))))))

(defun kind-all-of-p (x kinds)
  "Returns T iff (KINDP X K) would return T for all K in KINDS. This is faster
  than (AND (KINDP X K1) (KINDP X K2) ...), since it only requires a single
  database query. The implementation relies on counting matching kinds, therefore
  KINDS should not contain repeated items."
  (let ((remaining (length kinds)))
    (when (and (symbolp x) (get x 'data))
      (clear)
      (beg x 1)
      (do ((p (dq+) (dq+)))
          ((null p))
          (when (and (member p kinds :test #'eq)
                     (zerop (decf remaining)))
            (return))
          (mark+ p (+labs p))))
    (zerop remaining)))

(defun decl-complex-kind (x)
  "Returns '$IMAGINARY if the symbol X is declared imaginary, '$COMPLEX if it is
  declared complex but not imaginary, else NIL. This is faster than two checks."
  (when (and (symbolp x) (get x 'data))
    (clear)
    (beg x 1)
    (let (complexp)
      (do ((p (dq+) (dq+)))
          ((null p) (and complexp '$complex))
        (if (eq p '$imaginary)
          (return '$imaginary)
          (progn
            (when (eq p '$complex)
              (setq complexp t))
            (mark+ p (+labs p))))))))

(defun true* (pat)
  (let ((dum (semant pat)))
    (if dum
	(cntxt (ind (ncons dum)) context))))

(defun fact (fun arg val)
  (cntxt (ind (datum (list fun arg val))) context))

(defun kind (x y)
  (setq y (datum (list 'kind x y)))
  (cntxt y context)
  (addf y x))

(defun par (s y)
  (setq y (datum (list 'par s y)))
  (cntxt y context)
  (mapc #'(lambda (lis) (addf y lis)) s))

(defun datum (pat)
  (ncons pat))

(defun ind (dat)
  (mapc #'(lambda (lis) (ind1 dat lis)) (cdar dat))
  (mapc #'ind2 (cdar dat))
  dat)

(defun ind1 (dat pat)
  (cond ((not (nodep pat))
	 (mapc #'(lambda (lis) (ind1 dat lis)) pat))
	((or (markp pat) (eq 'unknown pat)))
	(t
	 (addf dat pat) (mark pat))))

(defun ind2 (nd)
  (if (nodep nd)
      (unmrk nd)
      (mapc #'ind2 nd)))

(defun addf (dat nd)
  (push+sto (sel nd data) (cons dat (sel nd data))))

(defun maxima-remf (dat nd)
  (push+sto (sel nd data) (fdel dat (sel nd data))))

(defun uncntxt (dat)
  (let* ((ctxt (or (zl-get dat 'con) 'global))
         (l (zl-get ctxt 'data)))
    (when l
      (putprop ctxt (delete dat l :test #'eq :count 1) 'data)))
  dat)

(defun fdel (fact data)
  (cond ((and (zl-get (car data) 'con)
	      (eq (car fact) (caaar data))
	      (eq (cadr fact) (cadaar data))
	      (eq (caddr fact) (caddar (car data))))
	 (let ((rest (cdr data)))
	   (uncntxt (car data))
	   rest))
	(t
	 (do ((ds data (cdr ds))
	      (dat))
	     ((null (cdr ds)))
	   (setq dat (cadr ds))
	   (cond ((and (zl-get dat 'con)
		       (eq (car fact) (caar dat))
		       (eq (cadr fact) (cadar dat))
		       (eq (caddr fact) (caddar dat)))
		  (rplacd ds (cddr ds))
		  (uncntxt dat) (return t))))
	 data)))

(defun semantics (pat)
  (if (atom pat)
      pat
      (list (semant pat))))

(defun db-mnump (x)
  (or (numberp x)
      (and (not (atom x))
	   (not (atom (car x)))
	   (member (caar x) '(rat bigfloat) :test #'eq))))

(defun semant (pat)
  (cond ((symbolp pat) (or (get pat 'var) pat))
	((db-mnump pat) (dintnum pat))
	(t (mapcar #'semant pat))))

(defun query-semant (pat)
  (cond ((symbolp pat) (or (get pat 'var) pat))
	((db-mnump pat) (or (dinternp pat) (dbnode pat)))
	(t (mapcar #'query-semant pat))))

(defun dinternp (x)
  "The database node for X, or NIL when the database has none."
  (cond ((mnump x) (assol x *nobjects*))
	((atom x) x)
	((assol x dobjects))))

(defun dintern (x)
  (cond ((mnump x) (dintnum x))
	((atom x) x)
	((assol x dobjects))
	(t (setq dobjects (cons (dbnode x) dobjects))
	   (car dobjects))))

(defun dnum-neighbors (x)
  "Where the number X belongs in the *NOBJECTS* chain, without putting it there.
   Returns three values: the node X is numerically equal to, if there is one;
   otherwise the nearest node above X and the nearest node below X, either of
   which is NIL when that side is empty. These are precisely the nodes DINTNUM
   links a new number to, located by the same scan - the two must stay in step.
   *NOBJECTS* is sorted descending, so the scan stops as soon as it has passed
   X: at once for a number above everything on record."
  (do ((lis *nobjects* (cdr lis))
       (above nil)
       (r))
      ((null lis) (values nil above nil))
    (setq r (rgrp x (caar lis)))
    (cond
      ((eq '$zero r) (return (values (car lis) nil nil)))
      ((eq '$pos r) (return (values nil above (car lis))))
      (t (setq above (car lis))))))

(defun dintnum (x &aux foo)
 (flet ((unlink-edge-below (node)
          (dolist (d (sel node data))
              (let ((p (car d)))
                (when (and (eq 'mgrp (car p))
                           (eq node (cadr p))
                           (null (zl-get d 'con)))
                  (remov d)
                  (putprop 'global
                           (delete d (get 'global 'data) :test #'eq :count 1)
                           'data)
                  (return))))))
  (cond ((assol x *nobjects*))
	((progn (setq x (dbnode x)) nil))
	((null *nobjects*)
	 (setq *nobjects* (list x))
	 x)
	((eq '$zero (setq foo (rgrp (car x) (caar *nobjects*))))
	 (let ((context 'global))
	   (fact 'meqp x (car *nobjects*)))
	 (push x *nobjects*)
	 x)
	((eq '$pos foo)
	 (let ((context 'global))
	   (fact 'mgrp x (car *nobjects*)))
	 (push x *nobjects*)
	 x)
	(t
	 (do ((lis *nobjects* (cdr lis))
	      (context '$global))
	     ((null (cdr lis))
	      (let ((context 'global))
		(fact 'mgrp (car lis) x))
	      (rplacd lis (list x)) x)
	   (cond ((eq '$zero (setq foo (rgrp (car x) (caadr lis))))
              (let ((context 'global))
                (fact 'meqp (cadr lis) x))
              (rplacd lis (cons x (cdr lis)))
              (return x))
         ((eq '$pos foo)
		  ;; X goes strictly between (CAR LIS) and (CADR LIS). Drop the edge
		  ;; leaving (CAR LIS) so that the number nodes stay a chain and don't
		  ;; become a DAG.
		  (unlink-edge-below (car lis))
		  ;; Insert the new edge.
		  (let ((context 'global))
		    (fact 'mgrp (car lis) x)
		    (fact 'mgrp x (cadr lis)))
		  (rplacd lis (cons x (cdr lis)))
		  (return x))))))))

(defun doutern (x)
  (if (atom x) x (car x)))

(defun untrue (pat)
  (kill (car pat) (semant (cadr pat)) (semant (caddr pat))))

(defun kill (fun arg val)
  (kill2 fun arg val arg)
  (kill2 fun arg val val))

(defun kill2 (fun arg val cl)
  (cond ((numberp cl))			;a bare number is not a node
	((atom cl)			;a symbol is its own node
	 (push+sto (sel cl data) (kill3 fun arg val (sel cl data))))
	((mnump (car cl))		;the node standing for a number
	 (push+sto (sel cl data) (kill3 fun arg val (sel cl data))))
	((or (atom (car cl))		;an operator list such as (%SIN), or
	     (atom (caar cl)))		;a Maxima expression: Do its parts
	 (mapc #'(lambda (lis) (kill2 fun arg val lis)) cl))
	((atom (caaar cl))		;the node standing for a compound
					;expression: the node itself, and then the
					;parts of the expression, where the facts
					;built by IND/IND1 sit
	 (push+sto (sel cl data) (kill3 fun arg val (sel cl data)))
	 (mapc #'(lambda (lis) (kill2 fun arg val lis)) (car cl)))))

(defun kill3 (fun arg val data)
  (cond ((and (eq fun (caaar data))
	      (eq arg (cadaar data))
	      (eq val (caddar (car data))))
	 (let ((rest (cdr data)))
	   (uncntxt (car data))
	   rest))
	(t
	 (do ((ds data (cdr ds))
	      (dat))
	     ((null (cdr ds)))
	 (setq dat (cadr ds))
	 (cond ((not (and (eq fun (caar dat))
			  (eq arg (cadar dat))
			  (eq val (caddar dat))))
		t)
	       (t (rplacd ds (cddr ds))
		  (uncntxt dat) (return t))))
	 data)))

(defun unkind (x y)
  (setq y (car (datum (list 'kind x y))))
  (kcntxt y context)
  (maxima-remf y x))

(defun remov (fact)
  (mapc #'(lambda (arg) (remov4 fact arg)) (cdar fact)))

(defun remov4 (fact cl)
  (cond ((or (symbolp cl)		;if CL is a symbol or
	     (and (consp cl)            ;an interned number, then we want to REMOV4 FACT
		  (mnump (car cl))))	;from its property list.
	 (push+sto (sel cl data) (delete fact (sel cl data) :test #'eq)))
	((or (atom cl) (atom (car cl)))) ;if CL is an atom (not a symbol)
					;or its CAR is an atom then we don't want to do
					;anything to it.
	((atom (caar cl))		;if CL's CAAR is an atom, then CL is an
					;expression, and we want to REMOV4 FACT
					;from the parts of the expression.
	 (mapc #'(lambda (lis) (remov4 fact lis)) (cdr cl)))
	((atom (caaar cl))		;if CL's CAAAR is an atom, then CL is the
					;node that DINTERN made for a compound
					;expression. MFACT hangs the fact on that
					;node itself, so take it off there; the
					;recursion below reaches only the symbols
					;inside the expression, which is where
					;IND/IND1 put the facts they build.
	 (push+sto (sel cl data) (delete fact (sel cl data) :test #'eq))
	 (mapc #'(lambda (lis) (remov4 fact lis)) (cdar cl)))))

(defun killframe (cl)
  (mapc #'(lambda (dat) (uncntxt dat) (remov dat)) (sel cl data))
  (zl-remprop cl '+labs)
  (zl-remprop cl '-labs)
  (zl-remprop cl 'obj)
  (zl-remprop cl 'var)
  (zl-remprop cl 'fact))

(defun activate (&rest l)
  (dolist (e l)
    (cond ((member e contexts :test #'eq) nil)
	  (t (push e contexts)
	     (cmark e)))))

(defun deactivate (&rest l)
  (dolist (e l)
    (cond ((not (member e contexts :test #'eq))
	   nil)
	  (t
	   (cunmrk e)
	   (setq contexts (delete e contexts :test #'eq))))))

(defun gccon ()
  (gccon1)
  (when (> *conindex* *connumber*)
    #+gc (gc)
    (gccon1)
    (when (> *conindex* *connumber*)
      (merror (intl:gettext "context: too many contexts.")))))

(defun gccon1 ()
  (setq *conindex* 0)
  (do ((i 0 (1+ i)))
      ((> i *connumber*))
    (cond ((not (eq (aref conmark i) (cdr (aref conunmrk i))))
	   (killc (aref conmark i)))
	  (t
	   (setf (aref conunmrk *conindex*) (aref conunmrk i))
	   (setf (aref conmark *conindex*) (aref conmark i))
	   (incf *conindex*)))))

(defun cntxt (dat con)
  (unless (atom con)
    (setq con (cdr con)))
  (putprop con (cons dat (zl-get con 'data)) 'data)
  (unless (eq 'global con)
    (putprop dat con 'con))
  dat)

(defun kcntxt (fact con)
  (unless (atom con)
    (setq con (cdr con)))
  (putprop con (fdel fact (zl-get con 'data)) 'data)
  (unless (eq 'global con)
    (zl-remprop fact 'con))
  fact)

(defun cntp (f)
  (cond ((not (setq f (sel f con))))
	((setq f (zl-get f 'cmark))
	 (> f 0))))

(defun contextmark ()
  (let ((con context))
    (unless (eq current con)
      (cunmrk current)
      (setq current con)
      (cmark con))))

(defun cmark (con)
  (unless (atom con)
    (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (putprop con (if cm (1+ cm) 1) 'cmark)
    (mapc #'cmark (zl-get con 'subc))))

(defun cunmrk (con)
  (if (not (atom con))
      (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (cond (cm (putprop con (1- cm) 'cmark)))
    (mapc #'cunmrk (zl-get con 'subc))))

;;; Garbage collection of the interning tables.
;;;
;;; DOBJECTS and *NOBJECTS* hold the nodes that give a Maxima object its place
;;; in the database. A node is worth keeping exactly as long as some fact names
;;; it, and the node itself answers that: ADDF puts every fact naming a node on
;;; that node's own DATA property, and MAXIMA-REMF, KILL3 and REMOV4 take it off
;;; again.
;;;
;;; Number nodes carry the MGRP/MEQP edges that order them against their
;;; neighbors on top of that. Those are bookkeeping, not information: DINTNUM
;;; makes them in the pseudo-context GLOBAL, the one context CNTXT stores no
;;; back pointer for, so they are exactly the datums with no CON property. A
;;; number node whose DATA holds nothing else is a waypoint on the chain and
;;; can go, once the chain is closed up behind it.

(defun dnode-live-p (nd)
  "Does some fact still name the node ND?"
  (and (zl-get nd 'data) t))

(defun dnum-live-p (nd)
  "Does some fact other than its own chain edges name the number node ND?"
  (dolist (dat (zl-get nd 'data))
    (when (zl-get dat 'con) (return t))))

(defun db-gc ()
  "Drop the database nodes that no longer carry a fact."
  (db-gc-dobjects)
  (db-gc-nobjects))

(defun db-gc-dobjects ()
  (unless (every #'dnode-live-p dobjects)
    (setq dobjects (remove-if-not #'dnode-live-p dobjects))))

(defun db-gc-nobjects ()
  (unless (every #'dnum-live-p *nobjects*)
    ;; Take the chain down ...
    (dolist (dat (get 'global 'data)) (remov dat))
    (remprop 'global 'data)
    (setq *nobjects* (remove-if-not #'dnum-live-p *nobjects*))
    ;; ... and lay it again over what is left. *NOBJECTS* is sorted
    ;; descending, so linking each node to its successor reproduces both what
    ;; DINTNUM builds and the order it builds it in: The edge to the smaller
    ;; neighbor is pushed onto a node's DATA last and so is tried first.
    ;; Reachability among the surviving nodes is unchanged, since a node that
    ;; goes carried no fact and was only ever a waypoint between the two nodes
    ;; that are now joined directly.
    (let ((context 'global))
      (do ((l *nobjects* (cdr l)))
	  ((null (cdr l)))
	(fact (if (eq '$zero (rgrp (caar l) (caadr l))) 'meqp 'mgrp)
	      (car l) (cadr l))))))

(defun killc (con)
  (contextmark)
  (unless (null con)
    (mapc #'remov (zl-get con 'data))
    (zl-remprop con 'data)
    (zl-remprop con 'cmark)
    (zl-remprop con 'subc))
  t)

(defun propg ()
  (do ((x)
       (lab))
      (nil)
    (cond
	  (+s
	   (setq x (dq+))
	   (setq lab (+labs x))
	   (if (zerop (logand (unlab lab) (unlab (-labz x))))
	       (mark+ x lab)
	       (return t)))
	  (-s
	   (setq x (dq-))
	   (setq lab (-labs x))
	   (if (zerop (logand (unlab lab) (unlab (+labz x))))
	       (mark- x lab)
	       (return t)))
	  (t (return nil)))))

(defun mark+ (cl lab)
  (when dbtrace
    (incf *marks*)
    (format *trace-output* "~%MARK+: marking ~A +" cl)
    (prlab lab))
  (mapc #'(lambda (lis) (mark+0 cl lab lis)) (sel cl data)))

(defun mark+0 (cl lab fact)
  (when *dbcheck*
    (format *trace-output* "~%MARK+0: checking ~a from ~A+" (car fact) cl)
    (prlab lab))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	(t (mark+1 cl lab fact))))

(defun mark+1 (cl lab dat)
  (cond ((eq (caar dat) 'kind)
	 (if (eq (cadar dat) cl) (mid (caddar dat) lab))) ; E1
	((eq (caar dat) 'par)
	 (if (not (eq (caddar dat) cl))
	     (progn
	       (cancel lab dat)		; PR1
	       (mid (caddar dat) lab)
	       (do ((lis (cadar dat) (cdr lis)))
		   ((null lis))
		 (if (not (eq (car lis) cl))
		     (mid- (car lis) lab))))))
	((eq (cadar dat) cl)
	 (if (+labs (caar dat))		; V1
	     (end (caddar dat) (dbv lab (+labs (caar dat)))))
	 (if (-labs (caddar dat))	; F4
	     (end- (caar dat) (lpr lab (-labs (caddar dat))))))))

(defun mark- (cl lab)
  (when dbtrace
    (incf *marks*)
    (format *trace-output* "~%MARK-: marking ~A -" cl)
    (prlab lab))
  (mapc #'(lambda (lis) (mark-0 cl lab lis)) (sel cl data)))

(defun mark-0 (cl lab fact)
  (when *dbcheck*
    (format *trace-output* "~%MARK-0: checking ~A from ~A-" (car fact) cl)
    (prlab lab))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	(t (mark-1 cl lab fact))))

(defun mark-1 (cl lab dat)
  (cond ((eq (caar dat) 'kind)
	 (if (not (eq (cadar dat) cl)) (mid- (cadar dat) lab)))	; E4
	((eq (caar dat) 'par)
	 (if (eq (caddar dat) cl)
	     (prog2
		 (cancel lab dat)	; S4
		 (do ((lis (cadar dat) (cdr lis)))
		     ((null lis))
		   (mid- (car lis) lab)))
	     (progn
	       (setq lab (unlab lab))	; ALL4
	       (do ((lis (cadar dat) (cdr lis)))
		   ((null lis))
		 (setq lab (logand (unlab (-labz (car lis))) lab)))
	       (setq lab (copyn lab))
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
	((setq al (uni (car p1) (car p2) al))
	 (uni (cdr p1) (cdr p2) al))))

(defun dbunivar (p v al)
  (let ((dum (assoc p al :test #'eq)))
    (if (null dum)
	(cons (cons p v) al)
	(uni (cdr dum) v al))))
