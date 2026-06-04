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

(macsyma-module simp)

(defvar *rulesw*)

;; Switches dealing with matrices and non-commutative multiplication.

(defmvar $doscmxplus nil
  "Causes SCALAR + MATRIX to return a matrix answer.  This switch
	 is not subsumed under DOALLMXOPS.")

(defmvar $domxexpt t
  "Causes SCALAR^MATRIX([1,2],[3,4]) to return
	 MATRIX([SCALAR,SCALAR^2],[SCALAR^3,SCALAR^4]).  In general, this
         transformation affects exponentiations where the *print-base* is a
         scalar and the power is a matrix or list.")

(defmvar $domxplus nil)

(defmvar $domxtimes nil)

(defmvar $mx0simp t)

;; Lisp level variables
(defmvar derivsimp t "Hack in `simpderiv' for `rwg'")

(defmvar $grindswitch nil)
(defmvar $on t)
(defmvar $off nil)
(defmvar $limitdomain '$complex)
(defmvar $lognegint nil
  nil
  :properties ((evflag t)))
(defmvar $distribute_over t
  "If T, functions are distributed over bags.")

(defvar *expandp* nil)
(defvar *timesinp* nil)
(defvar *exptrlsw* nil)
(defvar *expandflag* nil)

(defmacro while (cond &rest body)
  `(do ()
       ((not ,cond))
     ,@body))

(declaim (inline member-eq))
(defun member-eq (m l)
  "This function behaves like (MEMBER M L :TEST #'EQ).
  When inlined, this function is so small that there is almost no code size
  overhead compared to a MEMBER call, but it is faster because no CALL
  instruction is required."
  (while l
    (when (eq m (car l))
      (return l))
    (setq l (cdr l))))

(defprop mnctimes t associative)
(defprop lambda t lisp-no-simp)

;; Local functions should not be simplified. Various lisps 
;; use various names for the list structure defining these:
(eval-when
    (:load-toplevel)
  (eval '(let* ((x 1)
		(z #'(lambda () 3)))
	  (dolist (y (list x z))
	    (and (consp y)
		 (symbolp (car y))
		 (setf (get (car y) 'lisp-no-simp) t))))))

(dolist (x '(mplus mtimes mnctimes mexpt mncexpt %sum))
  (setf (get x 'msimpind) (cons x '(simp))))

;; operators properties

(mapc #'(lambda (x) (setf (get (first x) 'operators) (second x)))
      '((mplus simplus) (mtimes simptimes) (mncexpt simpncexpt)
	(mminus simpmin)
	(mfactorial simpfact)
	(mnctimes simpnct) (mquotient simpquot) (mexpt simpexpt)
        (%derivative simpderiv)
        (%signum simpsignum)
	(%integrate simpinteg) (%limit simp-limit) 
	(bigfloat simpbigfloat) (lambda simplambda) (mdefine simpmdef)
	(mqapply simpmqapply)
	(%sum simpsum)
	(%product simpprod)
	($matrix simpmatrix) (%matrix simpmatrix)))

(defprop $equal t binary)
(defprop $notequal t binary)

(defun sratsimp (e) (simplifya ($ratsimp e) nil))

(defun simpcheck (e flag)
  (cond ((specrepp e) (specdisrep e))
        (flag e)
        (t (let (($%enumer $numer))
             ;; Switch $%enumer on, when $numer is TRUE to allow
             ;; simplification of $%e to its numerical value.
             (simplifya e nil)))))

(defun mratcheck (e) (if ($ratp e) (ratdisrep e) e))

;; Note that the following two functions are carefully coupled.

(defun specrepp (e)
  (and (not (atom e))
       (member (caar e) '(mrat mpois)) t))

(defun constant (x)
  (cond ((symbolp x) (kindp x '$constant))
	(($subvarp x)
	 (and (kindp (caar x) '$constant)
	      (do ((x (cdr x) (cdr x))) ((null x) t)
		(if (not ($constantp (car x))) (return nil)))))))

(defun maxima-constantp (x)
  (or (numberp x)
      (and (symbolp x) (kindp x '$constant))))

(defun scalar-or-constant-p (x flag)
  (if flag (not ($nonscalarp x)) ($scalarp x)))


(defun consttermp (x) (and ($constantp x) (not ($nonscalarp x))))

(defun scalarclass (exp) ;  Returns $SCALAR, $NONSCALAR, or NIL (unknown).
  (cond ((mnump exp)
         ;; Maxima numbers are scalar.
         '$scalar)
        ((atom exp)
	 (cond ((or (mget exp '$nonscalar)
	            (and (not (mget exp '$scalar))
	                 ;; Arrays are nonscalar, but not if declared scalar.
	                 (or (arrayp exp)
	                     ($member exp $arrays))))
	        '$nonscalar)
	       ((or (mget exp '$scalar)
	            ;; Include constant atoms which are not declared nonscalar.
	            ($constantp exp))
	        '$scalar)))
        ((member 'array (car exp))
         (cond ((mget (caar exp) '$scalar) '$scalar)
               ((mget (caar exp) '$nonscalar) '$nonscalar)
               (t nil)))
	((specrepp exp) (scalarclass (specdisrep exp)))
	;; If the function is declared scalar or nonscalar, then return. If it
        ;; isn't explicitly declared, then try to be intelligent by looking at 
        ;; the arguments to the function.
	((scalarclass (caar exp)))
	;; <number> + <scalar> is SCALARP because that seems to be useful. 
        ;; This should probably only be true if <number> is a member of the 
        ;; field of scalars. <number> * <scalar> is SCALARP since 
        ;; <scalar> + <scalar> is SCALARP. Also, this has to be done to make 
        ;; <scalar> - <scalar> SCALARP.
	((member (caar exp) '(mplus mtimes))
	 (do ((l (cdr exp) (cdr l))) ((null l) '$scalar)
	   (if (not (consttermp (car l)))
	       (return (scalarclass-list l)))))
	((and (eq (caar exp) 'mqapply) (scalarclass (cadr exp))))
	((mxorlistp exp) '$nonscalar)
	;; If we can't find out anything about the operator, then look at the
        ;; arguments to the operator.  I think NIL should be returned at this 
        ;; point.  -cwh
	(t
	 (do ((exp (cdr exp) (cdr exp)) (l '(1)))
	      ((null exp) (scalarclass-list l))
	    (if (not (consttermp (car exp)))
	        (setq l (cons (car exp) l)))))))

;;  Could also do <scalar> +|-|*|/ |^ <declared constant>, but this is not
;;  always correct and could screw somebody.

;;  SCALARCLASS-LIST takes a list of expressions as its argument.  If their
;;  scalarclasses all agree, then that scalarclass is returned.

(defun scalarclass-list (llist)
  (cond ((null llist) nil)
	((null (cdr llist)) (scalarclass (car llist)))
	(t (let ((sc-car (scalarclass (car llist)))
		 (sc-cdr (scalarclass-list (cdr llist))))
	     (cond ((or (eq sc-car '$nonscalar)
			(eq sc-cdr '$nonscalar))
		    '$nonscalar)
		   ((and (eq sc-car '$scalar) (eq sc-cdr '$scalar))
		    '$scalar))))))

(defun constmx (const x)
  (simplifya (fmapl1 #'(lambda (ign)
			 (declare (ignore ign))
			 const)
		     x)
	     t))

;;; ISINOP returns the complete subexpression with the operator OP, when the 
;;; operator OP is found in EXPR.

(defun isinop (expr op)    ; OP is assumed to be an atom
  (cond ((atom expr) nil)
        ((and (eq (caar expr) op)
              (not (member 'array (cdar expr))))
         expr)
        (t 
         (do ((expr (cdr expr) (cdr expr))
              (res nil))
             ((null expr))
           (when (setq res (isinop (car expr) op)) 
             (return res))))))

(defun free (exp free-var)
  (cond ((alike1 exp free-var) nil)
	((atom exp) t)
	(t
	 (and (listp (car exp))
	      (free (caar exp) free-var)
	      (freel (cdr exp) free-var)))))

(defun freel (l free-var)
  (do ((l l (cdr l))) ((null l) t)
    (cond
     ((atom l) (return (free l free-var)))	;; second element of a pair
     ((not (free (car l) free-var)) (return nil)))))

(defun simplifya (x y)
 (let (op)
  (cond ((not $simp) x)
        ((atom x)
         (cond ((and (eq x '$%e) $%enumer $numer)
                ;; Replace $%e with its numerical value,
                ;; when %enumer and $numer TRUE
                (setq x %e-val))
               (t x)))
	((atom (car x))
	 (cond ((and (cdr x) (atom (cdr x)))
		(merror (intl:gettext "simplifya: malformed expression (atomic cdr).")))
	       ((get (car x) 'lisp-no-simp)
		;; this feature is to be used with care. it is meant to be
		;; used to implement data objects with minimum of consing.
		;; forms must not bash the DISPLA package. Only new forms
		;; with carefully chosen names should use this feature.
		x)
	       (t (cons (car x)
			(mapcar #'(lambda (x) (simplifya x y)) (cdr x))))))
	((eq (setq op (caar x)) 'rat) (*red1 x))
	;; Enforced resimplification: Reset dosimp and strip 'simp tags from x.
	(dosimp (let ((dosimp nil)) (simplifya (unsimplify x) nil)))
	((member-eq 'simp (cdar x)) x)
	((eq op 'mrat) x)
	((stringp op)
	 (simplifya (cons (cons ($verbify op) (rest (car x))) (rest x)) y))
	((not (atom op))
	 (cond ((or (eq (caaar x) 'lambda)
		    (and (not (atom (caaar x))) (eq (caaaar x) 'lambda)))
		(mapply1 op (cdr x) op x))
	       (t (merror (intl:gettext "simplifya: operator is neither an atom nor a lambda expression: ~S") x))))
        ((and $distribute_over
              (get op 'distribute_over)
              ;; A function with the property 'distribute_over.
              (distribute-over x y)))
	((get op 'opers)
	 (let ((opers-list *opers-list)) (oper-apply x y)))
	((and (eq op 'mqapply)
	      (or (atom (cadr x))
		  (and (eq substp 'mqapply)
		       (or (eq (car (cadr x)) 'lambda)
			   (eq (caar (cadr x)) 'lambda)))))
	 (cond ((or (symbolp (cadr x)) (not (atom (cadr x))))
		(simplifya (cons (cons (cadr x) (cdar x)) (cddr x)) y))
	       ((or (not (member-eq 'array (cdar x))) (not $subnumsimp))
		(merror (intl:gettext "simplifya: I don't know how to simplify this operator: ~M") x))
	       (t (cadr x))))
	(t (let ((w (get op 'operators)))
	     (cond ((and w
	                 (or (not (member-eq 'array (cdar x)))
	                     (rulechk op)))
		    (funcall w x 1 y))
		   (t (simpargs x y))))))))

;; EQTEST returns an expression which is the same as X
;; except that it is marked with SIMP and maybe other flags from CHECK.
;;
;; Following description is inferred from the code. Dunno why the function is named "EQTEST".
;;
;; (1) if X is already marked with SIMP flag or doesn't need it: return X.
;; (2) if X is pretty much the same as CHECK (same operator and same arguments),
;; then return CHECK after marking it with SIMP flag.
;; (3) if operator of X has the MSIMPIND property, replace it
;; with value of MSIMPIND (something like '(MPLUS SIMP)) and return X.
;; (4) if X or CHECK is an array expression, return X after marking it with SIMP and ARRAY flags.
;; (5) otherwise, return X after marking it with SIMP flag.

(defun eqtest (x check)
  (let ((y nil))
    (cond ((or (atom x)
	       (eq (caar x) 'rat)
	       (eq (caar x) 'mrat)
	       (member-eq 'simp (cdar x)))
	   x)
	  ((and (eq (caar x) (caar check))
		(equal (cdr x) (cdr check)))
	   (cond ((and (null (cdar check))
		       (setq y (get (caar check) 'msimpind)))
		  (cons y (cdr check)))
		 ((member-eq 'simp (cdar check))
		  check)
		 (t
		  (cons (cons (caar check)
			      (if (cdar check)
				  (cons 'simp (cdar check))
				  '(simp)))
			(cdr check)))))
	  ((setq y (get (caar x) 'msimpind))
	   (rplaca x y))
	  ((or (member-eq 'array (cdar x))
	       (and (eq (caar x) (caar check))
		    (member-eq 'array (cdar check))))
	   (rplaca x (cons (caar x) '(simp array))))
	  (t
	   (rplaca x (cons (caar x) '(simp)))))))

;; A function which distributes over bags like a list, matrix, or equation,
;; or any other operator.
(defun distribute-over (expr args-simped)
  (cond ((= 1 (length (cdr expr)))
         ;; Distribute over for a function with one argument.
         (cond ((and (not (atom (cadr expr)))
                     (member (caaadr expr) (get (caar expr) 'distribute_over) :test #'eq))
                (simplifya
                  (cons (caadr expr)
                        (mapcar #'(lambda (u) (simplifya (list (car expr) u) args-simped))
                                (cdadr expr)))
                  t))
                (t nil)))
        (t
         ;; A function with more than one argument.
         (do ((args (cdr expr) (cdr args))
              (first-args nil))
             ((null args) nil)
           (when (and (not (atom (car args)))
                      (member (caar (car args))
                              (get (caar expr) 'distribute_over) :test #'eq))
             ;; Distribute the function over the arguments and simplify again.
             (return 
               (simplifya
                 (cons (ncons (caar (car args)))
                       (mapcar #'(lambda (u) 
                                   (simplifya
                                     (append 
                                       (append 
                                         (cons (ncons (caar expr))
                                               (reverse first-args))
                                         (ncons u))
                                       (rest args)) args-simped))
                               (cdr (car args))))
                 t)))
           (setq first-args (cons (car args) first-args))))))

(defun rulechk (x) (or (mget x 'oldrules) (get x 'rules)))

(defun resimplify (x) (let ((dosimp t)) (simplifya x nil)))

(defun unsimplify (x)
  (if (or (atom x) (specrepp x))
      x
      (cons (remove 'simp (car x) :count 1) (mapcar #'unsimplify (cdr x)))))

(defun simpargs (x y)
  (if (or (and (eq (get (caar x) 'dimension) 'dimension-infix)
	       (not (getl (caar x) '($lassociative $rassociative))))
	  (get (caar x) 'binary))
      (twoargcheck x))
  (if (and (member 'array (cdar x)) (null (margs x)))
      (merror (intl:gettext "SIMPARGS: subscripted variable found with no subscripts.")))
  (eqtest (if (member (caar x) '(mlist mequal))
		    ;; MLIST or MEQUAL: Arguments will not be converted from special
		    ;; representations to the general form.
		    (if y
		      ;; Arguments are already simplified - nothing to do here.
		      x
		      ;; Arguments are not simplified. Strip any flags from the header,
		      ;; and simplify arguments.
		      (cons (ncons (caar x)) (mapcar #'simplify (cdr x))))
		    ;; Not MLIST or MEQUAL. Strip any flags from the header, and call
		    ;; SIMPMAP, which will convert special representations to general
		    ;; form and simplify, if necessary (depending on flag Y).
		    (cons (ncons (caar x)) (simpmap (cdr x) y)))
	  x))

;;;-----------------------------------------------------------------------------
;;; ADDK (X Y)                                                   27.09.2010/DK
;;;
;;; Arguments and values:
;;;   X      - a Maxima number
;;;   Y      - a Maxima number
;;;   result - a simplified Maxima number
;;;
;;; Description:
;;;   ADDK adds two Maxima numbers and returns a simplified Maxima number.
;;;   ADDK can be called in Lisp code, whenever the arguments are valid
;;;   Maxima numbers, these are integer, float, Maxima rational, or
;;;   Maxima bigfloat numbers. The arguments must not be simplified. The
;;;   precision of a bigfloat result depends on the setting of the
;;;   global variable $FPPREC. If the option variable $FLOAT is T, a
;;;   Maxima rational number as a result is converted to a float number.
;;;
;;; Examples:
;;;   (addk 2 3) -> 5
;;;   (addk 2.0 3) -> 5.0
;;;   (addk ($bfloat 2) 3)-> ((BIGFLOAT SIMP 56) 45035996273704960 3)
;;;   (addk 2 '((rat) 1 2)) -> ((RAT SIMP) 5 2)
;;;   (let (($float t)) (addk 2 '((rat) 1 2))) -> 2.5
;;;
;;; Affected by:
;;;   The option variables $FLOAT and $FPPREC.
;;;
;;; See also:
;;;   TIMESK to multiply and EXPTRL to exponentiate two Maxima numbers.
;;;
;;; Notes:
;;;   The routine works for Lisp rational and Lisp complex numbers too.
;;;   This feature is not used in Maxima code. If Lisp complex and
;;;   rational numbers are mixed with Maxima rational or bigfloat
;;;   numbers the result is wrong or a Lisp error is generated.
;;;-----------------------------------------------------------------------------

(defun addk (x y)
  (cond ((eql x 0) y)
	((eql y 0) x)
	((and (numberp x) (numberp y)) (+ x y))
	((or ($bfloatp x) ($bfloatp y)) ($bfloat (list '(mplus) x y)))
	(t (prog (g a b)
	      (cond ((numberp x)
		     (cond ((floatp x) (return (+ x (fpcofrat y))))
			   (t (setq x (list '(rat) x 1)))))
		    ((numberp y)
		     (cond ((floatp y) (return (+ y (fpcofrat x))))
			   (t (setq y (list '(rat) y 1))))))
	      (setq g (gcd (caddr x) (caddr y)))
	      (setq a (truncate (caddr x) g)
	            b (truncate (caddr y) g))
	      (return (timeskl (list '(rat) 1 g)
			       (list '(rat)
				     (+ (* (cadr x) b)
					   (* (cadr y) a))
				     (* a b))))))))

;;;-----------------------------------------------------------------------------
;;; *RED1 (X)                                                      27.09.2010/DK
;;; *RED (N D)
;;;
;;; Arguments and values:
;;;   X      - a Maxima rational number (for *RED1)
;;;   N      - an integer number representing the numerator of a rational
;;;   D      - an integer number representing the denominator of a rational
;;;   result - a simplified Maxima rational number
;;;
;;; Description:
;;;   *RED1 is called from SIMPLIFYA to reduce and simplify a Maxima rational
;;;   number. *RED1 checks if the rational number is already simplified. If
;;;   the option variable $FLOAT is T, the rational number is converted to a
;;;   float number. If the number is not simplified, *RED is called.
;;;
;;;   *RED reduces the numerator N and the demoniator D and returns a 
;;;   simplified Maxima rational number. The result is converted to a float
;;;   number, if the option variable $FLOAT is T.
;;;
;;; Affected by:
;;;   The option variable $FLOAT.
;;;-----------------------------------------------------------------------------

(defun *red1 (x)
  (cond ((member 'simp (cdar x))
	 (if $float (fpcofrat x) x))
	(t (*red (cadr x) (caddr x)))))

(defun *red (n d)
  (cond ((zerop n) 0)
	((equal d 1) n)
	(t (let ((u (gcd n d)))
	     (setq n (truncate n u)
	           d (truncate d u))
	     (if (minusp d) (setq n (- n) d (- d)))
	     (cond ((equal d 1) n)
		   ($float (fpcofrat1 n d))
		   (t (list '(rat simp) n d)))))))

;;;-----------------------------------------------------------------------------
;;; TIMESK (X Y)                                                   27.09.2010/DK
;;;
;;; Arguments and values:
;;;   X      - a Maxima number
;;;   Y      - a Maxima number
;;;   result - a simplified Maxima number
;;;
;;; Description:
;;;   TIMESK Multiplies two Maxima numbers and returns a simplified Maxima
;;;   number. TIMESK can be called in Lisp code, whenever the arguments are
;;;   valid Maxima numbers, these are integer, float, Maxima rational, or
;;;   Maxima bigfloat numbers. The arguments must not be simplified. The
;;;   precision of a bigfloat result depends on the setting of the
;;;   global variable $FPPREC. If the option variable $FLOAT is T, a
;;;   Maxima rational number as a result is converted to a float number.
;;;
;;;   TIMESKL is called from TIMESK to multiply two Maxima rational numbers or
;;;   a rational number with an integer number.
;;;
;;; Examples:
;;;   (timesk 2 3) -> 6
;;;   (timesk 2.0 3) -> 6.0
;;;   (timesk ($bfloat 2) 3)-> ((BIGFLOAT SIMP 56) 54043195528445952 3)
;;;   (timesk 3 '((rat) 1 2)) -> ((RAT SIMP) 3 2)
;;;   (let (($float t)) (timesk 3 '((rat) 1 2))) -> 1.5
;;;
;;; Affected by:
;;;   The option variables $FLOAT and $FPPREC.
;;;
;;; See also:
;;;   ADDK to add and EXPTRL to exponentiate two Maxima numbers.
;;;
;;; Notes:
;;;   The routine works for Lisp rational and Lisp complex numbers too.
;;;   This feature is not used in Maxima code. If Lisp complex and
;;;   rational numbers are mixed with Maxima rational or bigfloat
;;;   numbers the result is wrong or a Lisp error is generated.
;;;-----------------------------------------------------------------------------

;; NUM1 and DENOM1 are helper functions for TIMESKL to get the numerator and the
;; denominator of an integer or Maxima rational number. For an integer the
;; denominator is 1. Both functions are used at other places in Maxima code too.

(defun num1 (a)
  (if (numberp a) a (cadr a)))

(defun denom1 (a)
  (if (numberp a) 1 (caddr a)))

(defun timesk (x y)     ; X and Y are assumed to be already reduced
  (cond ((equal x 1) y)
	((equal y 1) x)
	((and (numberp x) (numberp y)) (* x y))
	((or ($bfloatp x) ($bfloatp y)) ($bfloat (list '(mtimes) x y)))
	((floatp x) (* x (fpcofrat y)))
	((floatp y) (* y (fpcofrat x)))
	(t (timeskl x y))))

;; TIMESKL takes one or two Maxima rational numbers, one argument can be an
;; integer number. The result is a Maxima rational or an integer number. 
;; If the option variable $FLOAT is T, a Maxima rational number in converted
;; to a float value.

(defun timeskl (x y)
  (prog (u v g)
     (setq u (*red (num1 x) (denom1 y)))
     (setq v (*red (num1 y) (denom1 x)))
     (setq g (cond ((or (equal u 0) (equal v 0)) 0)
		   ((equal v 1) u)
		   ((and (numberp u) (numberp v)) (* u v))
		   (t (list '(rat simp)
			    (* (num1 u) (num1 v))
			    (* (denom1 u) (denom1 v))))))
     (return (cond ((numberp g) g)
		   ((equal (caddr g) 1) (cadr g))
		   ($float (fpcofrat g))
		   (t g)))))

;;;-----------------------------------------------------------------------------
;;; FPCOFRAT (RATNO)                                               27.09.2010/DK
;;; FPCOFRT1 (NU D)
;;;
;;; Arguments and values:
;;;   RATNO  - a Maxima rational number (for FPCOFRAT)
;;;   NU     - an integer number which represents the numerator of a rational
;;;   D      - an integer number which represents the denominator of a rational
;;;   result - floating point approximation of a rational number
;;;
;;; Description:
;;;   Floating Point Conversion OF RATional number routine.
;;;   Finds floating point approximation to rational number.
;;;
;;;   FPCOFRAT1 computes the quotient of NU/D.
;;;
;;; Exceptional situations:
;;;   A Lisp error is generated, if the rational number does not fit into a
;;;   float number.
;;;-----------------------------------------------------------------------------

;; This constant is only needed in the file float.lisp.
(eval-when
    (:compile-toplevel :load-toplevel :execute)
    (defconstant machine-mantissa-precision (float-digits 1.0)))

(defun fpcofrat (ratno)
  (fpcofrat1 (cadr ratno) (caddr ratno)))

(defun fpcofrat1 (nu d)
  (float (/ nu d)))

;;;-----------------------------------------------------------------------------
;;; EXPTA (X Y)                                                    27.09.2010/DK
;;; 
;;; Arguments and values:
;;;   X      - a Maxima number
;;;   Y      - an integer number
;;;   result - a simplified Maxima number
;;;
;;; Description:
;;;   Computes X^Y, where X is Maxima number and Y an integer. The result is 
;;;   a simplified Maxima number. Y can be a rational Maxima number. For this
;;;   case the numerator is taken as the power.
;;;
;;; Affected by:
;;;   The option variables $FLOAT and $FPPREC.
;;;
;;; Notes:
;;;   This routine is not used within the simplifier. There is only one 
;;;   call from the file hayat.lisp. This call can be replaced with a
;;;   call of the function power.
;;;-----------------------------------------------------------------------------

(defun expta (x y)
  (cond ((equal y 1)
	 x)
	((numberp x)
	 (exptb x (num1 y)))
	(($bfloatp x)
	 ($bfloat (list '(mexpt) x y)))
	((minusp (num1 y))
	 (*red (exptb (caddr x) (- (num1 y)))
	       (exptb (cadr x) (- (num1 y)))))
	(t
	 (*red (exptb (cadr x) (num1 y))
	       (exptb (caddr x) (num1 y))))))

;;;-----------------------------------------------------------------------------
;;; EXPTB (A B)                                                    27.09.2010/DK
;;;
;;; Arguments and values:
;;;   A      - a float or integer number
;;;   B      - an integer number
;;;   result - a simplified Maxima number
;;;
;;; Description:
;;;   Computes A^B, where A is a float or an integer number and B is an 
;;;   integer number. The result is an integer, float, or Maxima
;;;   rational number.
;;;
;;; Examples:
;;;   (exptb 3 2)   -> 9
;;;   (exptb 3.0 2) -> 9.0
;;;   (exptb 3 -2)  -> ((RAT SiMP) 1 9)
;;;   (let (($float t)) (exptb 3 -2)) -> 0.1111111111111111
;;;
;;; Affected by:
;;;   The option variable $FLOAT.
;;;
;;; Notes:
;;;   EXPTB calls the Lisp functions EXP or EXPT to compute the result.
;;;-----------------------------------------------------------------------------

(defun exptb (a b)
  (let ((result
	 (cond ((equal a %e-val)
		;; Make B a float so we'll get double-precision result.
		(exp (float b)))
	       ((or (floatp a) (not (minusp b)))
		(expt a b))
	       (t
		(setq b (expt a (- b)))
		(*red 1 b)))))
    (when (float-inf-p result)	;; needed for gcl and sbcl - (sometimes) no trap of overflow
      (error 'floating-point-overflow))
    result))
    

;;;-----------------------------------------------------------------------------
;;; SIMPLUS (X W Z)                                                27.09.2010/DK
;;;
;;; Arguments and values:
;;;   X      - a Maxima expression of the form ((mplus) term1 term2 ...)
;;;   W      - an arbitrary value, the value is ignored
;;;   Z      - T or NIL, if T the arguments are assumed to be simplified
;;;   result - a simplified mplus-expression or an atom
;;;
;;; Description:
;;;  Implementation of the simplifier for the "+" operator.
;;;  A general description of SIMPLUS can be found in the paper:
;;;    http://www.cs.berkeley.edu/~fateman/papers/simplifier.txt
;;;
;;; Affected by:
;;;   The addition of matrices and lists is affected by the following option
;;;   variables:
;;;   $DOALLMXOPS, $DOMXMXOPS, $DOMXPLUS, $DOSCMXOPPS, $DOSCMXPLUS, $LISTARITH
;;;
;;; Notes:
;;;   This routine should not be called directly. It is called by SIMPLIFYA.
;;;   A save access is to call the function ADD.
;;;-----------------------------------------------------------------------------

(defun simplus (x w z)
  (prog (res check eqnflag matrixflag sumflag)
     (if (null (cdr x)) (return 0))
     (setq check x)
  start
     (setq x (cdr x))
     (if (null x) (go end))
     (setq w (if z (car x) (simplifya (car x) nil)))
  st1
     (cond ((atom w) nil)
           ((eq (caar w) 'mrat)
            (cond ((or eqnflag
                       matrixflag
                       (and sumflag
                            (not (member 'trunc (cdar w))))
                       (spsimpcases (cdr x) w))
                   (setq w (ratdisrep w))
                   (go st1))
                  (t
                   (return
                     (ratf (cons '(mplus)
                                 (nconc (mapcar #'simplify (cons w (cdr x)))
                                        (cdr res))))))))
           ((eq (caar w) 'mequal)
            (setq eqnflag
                  (if (not eqnflag)
                      w
                      (list (car eqnflag)
                            (add2 (cadr eqnflag) (cadr w))
                            (add2 (caddr eqnflag) (caddr w)))))
            (go start))
           ((member (caar w) '(mlist $matrix))
            (setq matrixflag
                  (cond ((not matrixflag) w)
                        ((and (or $doallmxops $domxmxops $domxplus
                                  (and (eq (caar w) 'mlist)
                                       ($listp matrixflag)))
                              (or (not (eq (caar w) 'mlist)) $listarith))
                         (addmx matrixflag w))
                        (t (setq res (pls w res)) matrixflag)))
            (go start))
           ((eq (caar w) '%sum)
            (setq sumflag t res (sumpls w res))
            (setq w (car res) res (cdr res))))
     (setq res (pls w res))
     (go start)
  end
     (setq res (testp res))
     (if matrixflag
         (setq res 
               (cond ((and (or ($listp matrixflag)
                               $doallmxops $doscmxplus $doscmxops)
                           (or (not ($listp matrixflag)) $listarith))
                      (mxplusc res matrixflag))
                     (t (testp (pls matrixflag (pls res nil)))))))
     (setq res (eqtest res check))
     (return (if eqnflag
                 (list (car eqnflag)
                       (add2 (cadr eqnflag) res)
                       (add2 (caddr eqnflag) res))
                 res))))

;;;-----------------------------------------------------------------------------
;;; PLS (X OUT)                                                    27.09.2010/DK
;;;
;;; Arguments and values:
;;;   X      - a Maxima expression or an atom
;;;   OUT    - a form ((mplus) <number> term1 term2 ...) or NIL
;;;   result - a form ((mplus) <number> term1 ...), where x is added in.
;;;
;;; Description:
;;;   Adds the argument X into the form OUT. If OUT is NIL a form
;;;   ((mplus) 0 X) is initialized, if X is an expression or a symbol,
;;;   or ((mplus) X), if X is a number. Numbers are added to the first
;;;   term <number> of the form. Any other symbol or expression is added
;;;   into the canonical ordered list of arguments. The result is in a
;;;   canonical order, but it is not a valid Maxima expression. To get a
;;;   valid Maxima expression the result has to be checked with the
;;;   function TESTP. This is done by the calling routine SIMPLUS.
;;;
;;;   PLS checks the global flag *PLUSFLAG*, which is set in PLUSIN to T,
;;;   if a mplus-expression is part of the result.
;;;
;;; Examples:
;;;   (pls 2 nil) -> ((MPLUS) 2)
;;;   (pls '$A nil) -> ((MPLUS) 0 $A)
;;;   (pls '$B '((mplus) 0 $A)) -> ((MPLUS) 0 $A $B)
;;;   (pls '$A '((mplus) 0 $A)) -> ((MPLUS) 0 ((MTIMES SIMP) 2 $A))
;;;
;;; Examples with the option variables $NUMER and $NEGDISTRIB:
;;;   (let (($numer t)) (pls '$%e nil)) -> ((MPLUS) 2.718281828459045)
;;;   (let (($negdistrib t)) (pls '((mtimes) -1 ((mplus) $A $B)) nil))
;;;           -> ((MPLUS) 0 ((MTIMES SIMP) -1 $A) ((MTIMES SIMP) -1 $B))
;;;   (let (($negdistrib nil)) (pls '((mtimes) -1 ((mplus) $A $B)) nil))
;;;           -> ((MPLUS) 0 ((MTIMES) -1 ((MPLUS) $A $B)))
;;;
;;; Affected by:
;;;   The option variables $NUMER and $NEGDISTRIB and the global flag
;;;   *PLUSFLAG*, which is set in the routine PLUSIN.
;;;
;;; See also:
;;;   PLUSIN and ADDK which are called from PLS and SIMPLUS.
;;;
;;; Notes:
;;;   To add an expression into the list (CDR OUT), the list is passed
;;;   to the routine PLUSIN as an argument. PLUSIN adds the argument to
;;;   the list of terms by modifying the list (CDR OUT) destructively.
;;;   The new value of OUT is returned as a result by PLS.
;;;-----------------------------------------------------------------------------

;; Set in PLUSIN to T to indicate a nested mplus expression.
(defvar *plusflag* nil)

;; TESTP checks the result of PLS to get a valid Maxima mplus-expression.

(defun testp (x)
  (cond ((atom x) 0)
        ((null (cddr x)) (cadr x))
        ((zerop1 (cadr x))
         (cond ((null (cdddr x)) (caddr x)) (t (rplacd x (cddr x)))))
        (t x)))

(defun pls (x out)
  (prog (fm *plusflag*)
     (if (mtimesp x) (setq x (testtneg x)))
     (when (and (eq x '$%e) $numer)
       ;; Replace $%e with its numerical value, when $numer is TRUE
       (setq x %e-val))
     (cond ((null out)
            ;; Initialize a form like ((mplus) <number> expr)
            (return
              (cons '(mplus)
                    (cond ((mnump x) (ncons x))
                          ((not (mplusp x))
                           (list 0 (cond ((atom x) x) (t (copy-list x)))))
                          ((mnump (cadr x)) (copy-list (cdr x) ))
                          (t (cons 0 (copy-list (cdr x) )))))))
           ((mnump x)
            ;; Add a number into the first term of the list out.
            (return (cons '(mplus)
                          (if (mnump (cadr out))
                              (cons (addk (cadr out) x) (cddr out))
                              (cons x (cdr out))))))
           ((not (mplusp x)) (plusin x (cdr out)) (go end)))
     ;; At this point we have a mplus expression as argument x. The following
     ;; code assumes that the argument x is already simplified and the terms
     ;; are in a canonical order.
     ;; First we add the number to the first term of the list out.
     (rplaca (cdr out)
             (addk (if (mnump (cadr out)) (cadr out) 0)
                   (cond ((mnump (cadr x)) (setq x (cdr x)) (car x)) (t 0))))
     ;; Initialize fm with the list of terms and start the loop to add the
     ;; terms of an mplus expression into the list out.
     (setq fm (cdr out))
  start
     (if (null (setq x (cdr x))) (go end))
     ;; The return value of PLUSIN is a list, where the first element is the
     ;; added argument and the rest are the terms which follow the added
     ;; argument.
     (setq fm (plusin (car x) fm))
     (go start)
  end
     (if (not *plusflag*) (return out))
     (setq *plusflag* nil)   ; *PLUSFLAG* T handles e.g. a+b+3*(a+b)-2*(a+b)
  a  
     ;; *PLUSFLAG* is set by PLUSIN to indicate that a mplus expression is
     ;; part of the result. For this case go again through the terms of the
     ;; result and add any term of the mplus expression into the list out.
     (setq fm (cdr out))
  loop
     (when (mplusp (cadr fm))
       (setq x (cadr fm))
       (rplacd fm (cddr fm))
       (pls x out)
       (go a))
     (setq fm (cdr fm))
     (if (null (cdr fm)) (return out))
     (go loop)))

;;;-----------------------------------------------------------------------------
;;; PLUSIN (X FM)                                                  27.09.2010/DK
;;;
;;; Arguments and values:
;;;   X      - a Maxima expression or atom
;;;   FM     - a list with the terms of an addition
;;;   result - part of the list FM (see below)
;;;
;;; Description:
;;;   Adds X into running list of additive terms FM. The routine modifies
;;;   the argument FM destructively, but does not return the modified list as
;;;   a result. The return value is a part of the list FM, which starts at the
;;;   place where the calling routine (PLS) can safely resume scanning to insert
;;;   subsequent, canonically sorted terms. PLUSIN is called only from the
;;;   routine PLS.
;;;
;;; Examples:
;;;   (setq fm '(0))
;;;   (plusin '$a fm) -> ($A)
;;;   fm -> (0 $A)
;;;   (plusin '$b fm) -> ($B)
;;;   fm -> (0 $A $B)
;;;   (plusin '$a fm) -> (((MTIMES SIMP) 2 $A) $B)
;;;   fm -> (0 ((MTIMES SIMP) 2 $A) $B)
;;;
;;; Side effects:
;;;   Modifies destructively the argument FM, which contains the result of the
;;;   addition of the argument X into the list FM.
;;;
;;; Affected by;
;;;   The option variables $doallmxops and $listarith.
;;;
;;; Notes:
;;;   The return value is used in PLS to go in parallel through the list of
;;;   terms, when adding a complete mplus-expression into the list of terms.
;;;   This is triggered by the flag *PLUSFLAG*, which is set in PLUSIN, if
;;;   a mplus-expression is added to the result list.
;;;-----------------------------------------------------------------------------

(defun plusin (x fm)
  (prog (x1 x2 flag check v w xnew a n m c int-base-expt-p base-match-p
         canonical-insert-fm cluster-start-fm)
     (setq w 1)
     (setq v 1)
     (cond ((mtimesp x)
            (setq check x)
            (if (mnump (cadr x)) (setq w (cadr x) x (cddr x))
                (setq x (cdr x))))
           (t (setq x (ncons x))))
     (setq x1 (if (null (cdr x)) (car x) (cons '(mtimes) x))
           xnew (list* '(mtimes) w x))
     ;; Remember whether X1 is an MEXPT with an integer base.
     ;; In that case, it can potentially merge with an already existing
     ;; term in such a way that the resulting term needs to be moved to a
     ;; different place to maintain canonical ordering.
     ;; Example: X = 7*2^(1/4) gets added into FM = 2^(1/4) + 2^(3/8)
     ;; 7*2^(1/4) merges with 2^(1/4) to 8*2^(1/4), but this term is fed into
     ;; SIMPTIMES, which recognizes that 8 = 2^3, pushes the 3 into the
     ;; exponent and returns 2^(13/4). But 2^(13/4) now must come after the
     ;; already existing term 2^(3/8) in FM to maintain canonical ordering.
     (setq int-base-expt-p (and (mexptp x1) (integerp (cadr x1))))
  start
     (setq base-match-p nil)
     (cond ((null (cdr fm)))
           ((and (null (cdr x)) (alike1 x1 (cadr fm)))
            (setq cluster-start-fm (or cluster-start-fm fm))
            (go equ))
           ;; Implement the simplification of
           ;;   v*a^(c+n)+w*a^(c+m) -> (v*a^n+w*a^m)*a^c
           ;; where a, v, w, and (n-m) are integers.
           ((and int-base-expt-p
                 (or (and (mexptp (setq x2 (cadr fm)))
                          (setq v 1))
                     (and (mtimesp x2)
                          (null (cadddr x2))
                          (integerp (setq v (cadr x2)))
                          (mexptp (setq x2 (caddr x2)))))
                 (equal (setq a (cadr x2)) (cadr x1))
                 (setq base-match-p t
                       cluster-start-fm (or cluster-start-fm fm))
                 (integerp (sub (caddr x2) (caddr x1))))
            (setq n (if (and (mplusp (caddr x2))
                             (mnump (cadr (caddr x2))))
                        (cadr (caddr x2))
                        (if (mnump (caddr x2))
                            (caddr x2)
                            0)))
            (setq m (if (and (mplusp (caddr x1))
                             (mnump (cadr (caddr x1))))
                        (cadr (caddr x1))
                        (if (mnump (caddr x1))
                            (caddr x1)
                            0)))
            (setq c (sub (caddr x2) n))
            (cond ((integerp n)
                   ;; The simple case:
                   ;; n and m are integers and the result is (v*a^n+w*a^m)*a^c.
                   (setq x1 (mul (addk (timesk v (exptb a n))
                                       (timesk w (exptb a m)))
                                 (power a c)))
                   (go equt2))
                  (t
                   ;; n and m are rational numbers: The difference n-m is an
                   ;; integer. The rational numbers might be improper fractions.
                   ;; The mixed numbers are: n = n1 + d1/r and m = n2 + d2/r,
                   ;; where r is the common denominator. We have two cases:
                   ;; I)  d1 = d2: e.g. 2^(1/3+c)+2^(4/3+c)
                   ;;     The result is (v*a^n1+w*a^n2)*a^(c+d1/r)
                   ;; II) d1 # d2: e.g. 2^(1/2+c)+2^(-1/2+c)
                   ;;     In this case one of the exponents d1 or d2 must
                   ;;     be negative. The negative exponent is factored out.
                   ;;     This guarantees that the factor (v*a^n1+w*a^n2)
                   ;;     is an integer. But the positive exponent has to be
                   ;;     adjusted accordingly. E.g. when we factor out
                   ;;     a^(d2/r) because d2 is negative, then we have to
                   ;;     adjust the positive exponent to n1 -> n1+(d1-d2)/r.
                   ;; Remark:
                   ;; Part of the simplification is done in simptimes. E.g.
                   ;; this algorithm simplifies the sum sqrt(2)+3*sqrt(2)
                   ;; to 4*sqrt(2). In simptimes this is further simplified
                   ;; to 2^(5/2).
                   (multiple-value-bind (n1 d1)
                       (truncate (num1 n) (denom1 n))
                     (multiple-value-bind (n2 d2)
                         (truncate (num1 m) (denom1 m))
                       (cond ((equal d1 d2)
                              ;; Case I: -> (v*a^n1+w*a^n2)*a^(c+d1/r)
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a
                                                (add c
                                                     (div d1 (denom1 n))))))
                              (go equt2))
                             ((minusp d2)
                              ;; Case II:: d2 is negative, adjust n1.
                              (setq n1 (add n1 (div (sub d1 d2) (denom1 n))))
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a
                                                (add c
                                                     (div d2 (denom1 n))))))
                              (go equt2))
                             ((minusp d1)
                              ;; Case II: d1 is negative, adjust n2.
                              (setq n2 (add n2 (div (sub d2 d1) (denom1 n))))
                              (setq x1
                                    (mul (addk (timesk v (exptb a n1))
                                               (timesk w (exptb a n2)))
                                         (power a 
                                                (add c
                                                     (div d1 (denom1 n))))))
                              (go equt2))
                             ;; This clause should never be reached.
                             (t (merror "Internal error in simplus."))))))))
           ((mtimesp (cadr fm))
            (cond ((and (cdr x) (alike1 x1 (cadr fm)))
                   (setq cluster-start-fm (or cluster-start-fm fm))
                   (go equt))
                  ((and (mnump (cadadr fm)) (alike x (cddadr fm)))
                   (setq flag t) ; found common factor
                   (setq cluster-start-fm (or cluster-start-fm fm))
                   (go equt))
                  ((great xnew (cadr fm)) (go gr))))
           ((great x1 (cadr fm)) (go gr)))
     (when base-match-p
       (when (null canonical-insert-fm)
         (setq canonical-insert-fm fm))
       (setq fm (cdr fm))
       (go start))
     (setq xnew (eqtest (testt xnew) (or check '((foo)))))
     (when canonical-insert-fm
       (setq fm canonical-insert-fm))
     (return (cdr (rplacd fm (cons xnew (cdr fm)))))
  gr 
     (setq fm (cdr fm))
     (go start)
  equ
     (setq x1
             (if (equal w -1)
                 (list* '(mtimes simp) 0 x)
                 ;; Call muln to get a simplified product.
                 (if (mtimesp (setq x1 (muln (cons (addk 1 w) x) t)))
                     (testtneg x1)
                     x1)))
     (go equt2)
  del
     (cond ((not (mtimesp (cadr fm)))
            (go check))
           ((onep (cadadr fm))
            ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
            (rplacd (cadr fm) (cddadr fm))
            (return (if int-base-expt-p cluster-start-fm (cdr fm))))
           ((not (zerop1 (cadadr fm)))
            (return (if int-base-expt-p cluster-start-fm (cdr fm))))
           ;; Handle the multiplication with a zero.
           ((and (or (not $listarith) (not $doallmxops))
                 (mxorlistp (caddr (cadr fm))))
            (rplacd fm 
                    (cons (constmx 0 (caddr (cadr fm))) (cddr fm)))
            (return (if int-base-expt-p cluster-start-fm fm))))
     ;; (cadadr fm) is zero. If the first term of fm is a number,
     ;;  add it to preserve the type.
     (when (mnump (car fm))
       (rplaca fm (addk (car fm) (cadadr fm))))
     (rplacd fm (cddr fm))
     (return (if int-base-expt-p cluster-start-fm fm))
  equt
     ;; Call muln to get a simplified product.
     (setq x1 (muln (cons (addk w (if flag (cadadr fm) 1)) x) t))
     ;; Make a mplus expression to guarantee that x1 is added again into the sum
     (setq x1 (list '(mplus) x1))
  equt2
     (setq x1 (if (zerop1 x1)
                (list* '(mtimes) x1 x)
                (if (mtimesp x1) (testtneg x1) x1)))
     (cond
       (int-base-expt-p
         ;; Potentially unstable order: The new term, resulting from merging X
         ;; with an already existing term, may have structurally changed and
         ;; may need to go to a different place in the output sum.
         ;; Remove the matched term, rewind the sum pointer back to the cluster
         ;; start, and scan forward to find the correct insertion place.
         (rplacd fm (cddr fm))
         (setq fm cluster-start-fm)
         (while (and (cdr fm) (great x1 (cadr fm)))
           (setq fm (cdr fm)))
         (rplacd fm (cons x1 (cdr fm))))
       (t
         ;; Safe case, merged term has same structure. Inline replacement.
         (rplaca (cdr fm) x1)))
     (if (not (mtimesp (cadr fm))) (go check))
     (when (and (onep (cadadr fm)) flag (null (cdddr (cadr fm))))
       ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
       (rplaca (cdr fm) (caddr (cadr fm))) (go check))
     (go del)
  check
     (if (mplusp (cadr fm)) (setq *plusflag* t)) ; A nested mplus expression
     (return (if int-base-expt-p cluster-start-fm (cdr fm)))))

;;;-----------------------------------------------------------------------------

;; Routines to add matrices

(defun mxplusc (sc mx)
  (cond ((mplusp sc)
	 (setq sc (partition-ns (cdr sc)))
	 (cond ((null (car sc)) (cons '(mplus) (cons mx (cadr sc))))
	       ((not (null (cadr sc)))
		(cons '(mplus)
		      (cons (simplify
			     (outermap1 'mplus (cons '(mplus) (car sc)) mx))
			    (cadr sc))))
	       (t (simplify (outermap1 'mplus (cons '(mplus) (car sc)) mx)))))
	((not (scalar-or-constant-p sc $assumescalar))
	 (testp (pls mx (pls sc nil))))
	(t (simplify (outermap1 'mplus sc mx)))))

(defun partition-ns (x)
  (let (sp nsp)		      ; SP = scalar part, NSP = nonscalar part
    (mapc #'(lambda (z) (if (scalar-or-constant-p z $assumescalar)
			    (setq sp (cons z sp))
			    (setq nsp (cons z nsp))))
	  x)
    (list (nreverse sp) (nreverse nsp))))

(defun conformable-matrices-p (x1 x2)
  (and (= (length x1) (length x2)) (or (and (null (rest x1)) (null (rest x2))) (= (length (second x1)) (length (second x2))))))

(defun addmx (x1 x2)

  ;; When maperror = true, ensure that arguments are conformable, in the sense of not causing an error in fullmap.
  ;; In practice this means the following:
  ;; (1) both arguments are lists, and of the same length.
  ;; (2) both arguments are matrices, and of the same number of rows and columns.
  ;; (3) one argument is a list and the other is a matrix.
  ;;     The matrix must have a number of rows equal to the length of the list.
  ;; Otherwise, maperror = false; fullmap will smash the arguments together somehow
  ;; ("truncating one or more arguments" as it says), so,
  ;; in the interest of preserving existing behavior, just let it.

  (when $maperror
    (unless
      (if (and ($listp x1) ($listp x2))
        (= (length x1) (length x2))
        (if (and ($matrixp x1) ($matrixp x2))
          (conformable-matrices-p x1 x2)
          ;; One argument is a list and the other is a matrix.
          ;; Test for (length of list) = (number of rows of matrix).
          ;; The following works whether X1 is the matrix and X2 is the list or vice versa.
          (= (length x1) (length x2))))
      (merror (intl:gettext "addition: non-conformable list or matrix arguments."))))

  (let (($doscmxops t) ($domxmxops t) ($listarith t))
    (simplify (fmapl1 'mplus x1 x2))))

;;; ----------------------------------------------------------------------------

;;; Simplification of the Log function

;; The log function distributes over lists, matrices, and equations
(defprop %log (mlist $matrix mequal) distribute_over)

(def-simplifier log (y)
  (cond ((onep1 y) (addk -1 y))
        ((zerop1 y)
         (cond (radcanp (list '(%log simp) 0))
               ((not errorsw)
                (merror (intl:gettext "log: encountered log(0).")))
               (t (throw 'errorsw t))))
        ;; Check evaluation in floating point precision.
        ((flonum-eval (mop form) y))
        ;; Check evaluation in bigfloag precision.
        ((and (not (member 'simp (car form)))
              (big-float-eval (mop form) y)))
        ((eq y '$%e) 1)
        ((mexptp y)
         (cond ((or (and $logexpand (eq $domain '$real))
                    (member $logexpand '($all $super))
                    (and (eq ($csign (cadr y)) '$pos)
                         (not (member ($csign (caddr y))
                                      '($complex $imaginary)))))
                ;; Simplify log(x^a) -> a*log(x), where x > 0 and a is real
                (mul (caddr y) (take '(%log) (cadr y))))
               ((or (and (ratnump (caddr y))
                         (or (eql 1 (cadr (caddr y)))
                             (eql -1 (cadr (caddr y)))))
                    (maxima-integerp (inv (caddr y))))
                ;; Simplify log(z^(1/n)) -> log(z)/n, where n is an integer
                (mul (caddr y)
                     (take '(%log) (cadr y))))
               ((and (eq (cadr y) '$%e)
                     (or (not (member ($csign (caddr y))
                                      '($complex $imaginary)))
                         (not (member ($csign (mul '$%i (caddr y)))
                                      '($complex $imaginary)))))
                ;; Simplify log(exp(x)) and log(exp(%i*x)), where x is a real
                (caddr y))
               (t (give-up))))
        ((ratnump y)
         ;; Simplify log(n/d)
         (cond ((eql (cadr y) 1)
                (mul -1 (take '(%log) (caddr y))))
               ((eq $logexpand '$super)
                (sub (take '(%log) (cadr y)) (take '(%log) (caddr y))))
               (t (give-up))))
        ((and (member $logexpand '($all $super))
              (mtimesp y))
         (do ((y (cdr y) (cdr y))
              (b nil))
             ((null y) (return (addn b t)))
           (setq b (cons (take '(%log) (car y)) b))))
        ((and (member $logexpand '($all $super))
              (consp y)
              (member (caar y) '(%product $product)))
         (let ((new-op (if (char= (get-first-char (caar y)) #\%) '%sum '$sum)))
           (simplifya `((,new-op) ((%log) ,(cadr y)) ,@(cddr y)) nil)))
        ((and $lognegint
              (maxima-integerp y)
              (eq ($sign y) '$neg))
         (add (mul '$%i '$%pi) (take '(%log) (neg y))))
        ((taylorize (mop form) (second form)))
        (t (give-up))))

;;; SIMPLN1 computes LOG(W), where W is an MEXPT expression.
;;; It's called by ORDMEXPT in cases where (TAKE '(%LOG) ...) could result
;;; in infinite recursion.
;;;
(defun simpln1 (w)
  (let ((simped (and (member 'simp (cdar w)) t)))
    (simplifya (list '(mtimes) (caddr w) ; Don't lie to SIMPLIFYA (2nd arg)!
      (simplifya (list '(%log) (cadr w)) simped)) simped)))

;;; ----------------------------------------------------------------------------

;;; Implementation of the Square root function

(defprop $sqrt %sqrt verb)
(defprop $sqrt %sqrt alias)

(defprop %sqrt $sqrt noun)
(defprop %sqrt $sqrt reversealias)

(defprop %sqrt simp-sqrt operators)

(defun simp-sqrt (x y z)
  (declare (ignore y))
  (oneargcheck x)
  (let ((arg (if z (cadr x) (simplifya (cadr x) nil))))
    (simplifya (list '(mexpt) arg '((rat simp) 1 2)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simplification of the "/" operator.

(defun simpquot (x y z)
  (declare (ignore y))
  (twoargcheck x)
  (let ((num (if z (cadr x) (simplifya (cadr x) nil)))
        (den (if z (caddr x) (simplifya (caddr x) nil))))
  (cond ((and (integerp num) (integerp den) (not (zerop den)))
	 (*red num den))
	((and (numberp num) (numberp den) (not (zerop den)))
	 (/ num den))
	((and (floatp num) (floatp den) #-ieee-floating-point (not (zerop den)))
	 (/ num den))
	((and ($bfloatp num) ($bfloatp den) (not (zerop (cadr den))))
	 ;; Call BIGFLOATP to ensure that arguments have same precision.
	 ;; Otherwise FPQUOTIENT could return a spurious value.
	 (bcons (fpquotient (cdr (bigfloatp num)) (cdr (bigfloatp den)))))
	(t
      (let ((inv-den (simplifya (list '(mexpt) den -1) t)))
        (if (equal num 1)
          inv-den
          (simplifya (list '(mtimes) num inv-den) t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the abs function.

;; Put the properties alias, reversealiases, noun and verb on the property list.
(defprop $abs mabs alias)
(defprop $abs mabs verb)
(defprop mabs $abs reversealias)
(defprop mabs $abs noun)

;; The abs function distributes over bags.
(defprop mabs (mlist $matrix mequal) distribute_over)

;; The abs function is a simplifying function.
(defprop mabs simpabs operators)

(defun simpabs (e y z)
  (declare (ignore y))
  (oneargcheck e)
  (let ((sgn)
	(x (simpcheck (second e) z)))
    
    (cond ((complex-number-p x #'(lambda (s) (or (floatp s) ($bfloatp s)))) 
	   (maxima::to (bigfloat::abs (bigfloat:to x))))
     		  		   
	  ((complex-number-p x #'mnump)
	   ($cabs x))
		 
	  ;; nounform for arrays...
	  ((or (arrayp x) ($member x $arrays)) `((mabs simp) ,x))
		   
	  ;; taylor polynomials
	  ((taylorize 'mabs x))
		   
	  ;; values for extended real arguments:
	  ((member x '($inf $infinity $minf)) '$inf)
	  ((member x '($ind $und)) x)

	  ;; abs(abs(expr)) --> abs(expr). Since x is simplified, it's OK to return x.
	  ((and (consp x) (consp (car x)) (eq (caar x) 'mabs))
	   x)
		    
	  ;; abs(conjugate(expr)) = abs(expr).
	  ((and (consp x) (consp (car x)) (eq (caar x) '$conjugate))
	   (take '(mabs) (cadr x)))

	  (t
	   (setq sgn ($csign x))
	   (cond ((member sgn '($neg $nz)) (mul -1 x))
		 ((eq '$zero sgn) (mul 0 x))
		 ((member sgn '($pos $pz)) x)
		 		  
		 ;; for complex constant expressions, use $cabs
		 ((and (eq sgn '$complex) ($constantp x))
		  ($cabs x))
		 		   
		 ;; abs(pos^complex) --> pos^(realpart(complex)).
		 ((and (eq sgn '$complex) (mexptp x) (eq '$pos ($csign (second x))))
		  (power (second x) ($realpart (third x))))

		 ;; for abs(neg^z), use cabs.
		 ((and (mexptp x) (eq '$neg ($csign (second x))))
		  ($cabs x))

		 ;; When x # 0, we have abs(signum(x)) = 1.
		 ((and (eq '$pn sgn) (consp x) (consp (car x)) (eq (caar x) '%signum)) 1)
		 		  		 		 
		 ;; multiplicative property: abs(x*y) = abs(x) * abs(y). We would like
		 ;; assume(a*b > 0), abs(a*b) --> a*b. Thus the multiplicative property
		 ;; is applied after the sign test.
		 ((mtimesp x)
		  (muln (mapcar #'(lambda (u) (take '(mabs) u)) (margs x)) t))
		   
		 ;; abs(x^n) = abs(x)^n for integer n. Is the featurep check worthwhile?
		 ;; Again the sign check is done first because we'd like abs(x^2) --> x^2.
		 ((and (mexptp x) ($featurep (caddr x) '$integer))
		  (power (take '(mabs) (cadr x)) (caddr x)))
		 		  
		 ;; Reflection rule: abs(-x) --> abs(x)
		 ((great (neg x) x) (take '(mabs) (neg x)))
	
		 ;; nounform return
		 (t (eqtest (list '(mabs) x) e)))))))

(defun abs-integral (x)
  (mul (div 1 2) x (take '(mabs) x)))

(putprop 'mabs `((x) ,'abs-integral) 'integral)

;; I (rtoy) think this does some simple optimizations of x * y.
(defun testt (x)
  (cond ((mnump x)
	 x)
	((null (cddr x))
	 ;; We have something like ((mtimes) foo).  This is the same as foo.
	 (cadr x))
	((eql 1 (cadr x))
	 ;; We have 1*foo.  Which is the same as foo.  This should not
	 ;; be applied to 1.0 or 1b0!
	 (cond ((null (cdddr x))
		(caddr x))
	       (t (rplacd x (cddr x)))))
	(t
	 (testtneg x))))

;; This basically converts -(a+b) to -a-b.
(defun testtneg (x)
  (cond ((and (equal (cadr x) -1)
	      (null (cdddr x))
	      (mplusp (caddr x))
	      $negdistrib)
	 ;; If x is exactly of the form -1*(sum), and $negdistrib is
	 ;; true, we distribute the -1 across the sum.
	 (addn (mapcar #'(lambda (z)
			   (mul2 -1 z))
		       (cdaddr x))
	       t))
	(t x)))

;; Simplification of the "-" operator
(defun simpmin (x vestigial z)
  (declare (ignore vestigial))
  (cond ((null (cdr x)) 0)
        ((null (cddr x))
         (mul -1 (if z (cadr x) (simplifya (cadr x) nil))))
        (t
         ;; ((mminus) a b ...) -> ((mplus) a ((mtimes) -1 b) ...)
         (sub (if z (cadr x) (simplifya (cadr x) nil)) (addn (cddr x) z)))))

(defun simptimes (x w z)		; W must be 1
  (prog (res check eqnflag matrixflag sumflag)
     (if (null (cdr x)) (return 1))
     (setq check x)
  start
     (setq x (cdr x))
     (cond ((zerop1 res)
	    (cond ($mx0simp
		   (cond ((and matrixflag (mxorlistp1 matrixflag))
			  (return (constmx res matrixflag)))
			 (eqnflag (return (list '(mequal simp)
						(mul2 res (cadr eqnflag))
						(mul2 res (caddr eqnflag)))))
		         (t
		          (dolist (u x)
			    (cond ((mxorlistp u)
				   (return (setq res (constmx res u))))
				  ((and (mexptp u)
					(mxorlistp1 (cadr u))
					($numberp (caddr u)))
				   (return (setq res (constmx res (cadr u)))))
				  ((mequalp u)
				   (return
				     (setq res 
				           (list '(mequal simp)
						 (mul2 res (cadr u))
						 (mul2 res (caddr u))))))))))))
	    (return res))
	   ((null x) (go end)))
     (setq w (if z (car x) (simplifya (car x) nil)))
  st1
     (cond ((atom w) nil)
	   ((eq (caar w) 'mrat)
	    (cond ((or eqnflag matrixflag
	               (and sumflag
	                    (not (member 'trunc (cdar w))))
		       (spsimpcases (cdr x) w))
	           (setq w (ratdisrep w))
	           (go st1))
	          (t
	           (return 
	             (ratf (cons '(mtimes)
			         (nconc (mapcar #'simplify (cons w (cdr x)))
					(cdr res))))))))
	   ((eq (caar w) 'mequal)
	    (setq eqnflag
		  (if (not eqnflag)
		      w
		      (list (car eqnflag)
			    (mul2 (cadr eqnflag) (cadr w))
			    (mul2 (caddr eqnflag) (caddr w)))))
	    (go start))
	   ((member (caar w) '(mlist $matrix))
	    (setq matrixflag
		  (cond ((not matrixflag) w)
			((and (or $doallmxops $domxmxops $domxtimes)
			      (or (not (eq (caar w) 'mlist)) $listarith)
			      (not (eq *inv* '$detout)))
			 (stimex matrixflag w))
			(t (setq res (tms w 1 res)) matrixflag)))
	    (go start))
	   ((and (eq (caar w) '%sum) $sumexpand)
	    (setq sumflag (sumtimes sumflag w))
	    (go start)))
     (setq res (tms w 1 res))
     (go start)
  end
     (cond ((mtimesp res) (setq res (testt res))))
     (cond (sumflag (setq res (cond ((or (null res) (equal res 1)) sumflag)
				    ((not (mtimesp res))
				     (list '(mtimes) res sumflag))
				    (t (nconc res (list sumflag)))))))
     (cond ((or (atom res)
		(not (member (caar res) '(mexpt mtimes)))
		(and (zerop $expop) (zerop $expon))
		*expandflag*))
	   ((eq (caar res) 'mtimes) (setq res (expandtimes res)))
	   ((and (mplusp (cadr res))
		 (fixnump (caddr res))
		 (not (or (> (caddr res) $expop)
			  (> (- (caddr res)) $expon))))
	    (setq res (expandexpt (cadr res) (caddr res)))))
     (cond (matrixflag
            (setq res
                  (cond ((null res) matrixflag)
                        ((and (or ($listp matrixflag)
                                  $doallmxops
			          (and $doscmxops
			               (not (member res '(-1 -1.0))))
			          ;; RES should only be -1 here (not = 1)
                                  (and $domxmxops
                                       (member res '(-1 -1.0))))
                              (or (not ($listp matrixflag)) $listarith))
                         (mxtimesc res matrixflag))
			(t (testt (tms matrixflag 1 (tms res 1 nil))))))))
     (if res (setq res (eqtest res check)))
     (return (cond (eqnflag
		    (if (null res) (setq res 1))
		    (list (car eqnflag)
			  (mul2 (cadr eqnflag) res)
			  (mul2 (caddr eqnflag) res)))
		   (t res)))))

(defun spsimpcases (l e)
  (dolist (u l)
    (if (or (mbagp u) (and (not (atom u))
			   (eq (caar u) '%sum)
			   (not (member 'trunc (cdar e)))))
	(return t))))

(defun mxtimesc (sc mx)
  (let (sign out)
    (and (mtimesp sc) (member (cadr sc) '(-1 -1.0))
	 $doscmxops (not (or $doallmxops $domxmxops $domxtimes))
	 (setq sign (cadr sc)) (rplaca (cdr sc) nil))
    (setq out (let ((scp* (cond ((mtimesp sc) (partition-ns (cdr sc)))
                                ((not (scalar-or-constant-p sc $assumescalar))
                                 nil)
				(t sc))))
		(cond  ((null scp*) (list '(mtimes simp) sc mx))
		       ((and (not (atom scp*)) (null (car scp*)))
			(append '((mtimes)) (cadr scp*) (list mx)))
		       ((or (atom scp*) (and (null (cdr scp*))
					     (not (null (cdr sc)))
					     (setq scp* (cons '(mtimes) (car scp*))))
			    (not (mtimesp sc)))
			(simplifya (outermap1 'mtimes scp* mx) nil))
		       (t (append '((mtimes))
				  (list (simplifya
					 (outermap1 'mtimes
						    (cons '(mtimes) (car scp*)) mx)
					 t))
				  (cadr scp*))))))
    (cond (sign (if (mtimesp out)
		    (rplacd out (cons sign (cdr out)))
		    (list '(mtimes) sign out)))
	  ((mtimesp out) (testt out))
	  (t out))))

(defun stimex (x y)
  (let (($doscmxops t) ($domxmxops t) ($listarith t))
    (simplify (fmapl1 'mtimes x y))))

;;  TMS takes a simplified expression FACTOR and a cumulative
;;  PRODUCT as arguments and modifies the cumulative product so
;;  that the expression is now one of its factors.  The
;;  exception to this occurs when a tellsimp rule is triggered.
;;  The second argument is the POWER to which the expression is
;;  to be raised within the product.

(defun tms (factor power product &aux tem)
  (let ((*rulesw* nil)
	(z nil))
    (when (mplusp product) (setq product (list '(mtimes simp) product)))
    (cond ((zerop1 factor)
	   (cond ((mnegp power)
		  (if errorsw
		      (throw 'errorsw t)
		      (merror (intl:gettext "Division by 0"))))
		 (t factor)))
	  ((and (null product)
		(or (and (mtimesp factor) (equal power 1))
		    (and (setq product (list '(mtimes) 1)) nil)))
	   (setq tem (append '((mtimes)) (if (mnump (cadr factor)) nil '(1))
			     (cdr factor) nil))
	   (if (= (length tem) 1)
	       (setq tem (copy-list tem))
	       tem))
	  ((mtimesp factor)
	   (do ((factor-list (cdr factor) (cdr factor-list)))
	       ((or (null factor-list) (zerop1 product))  product)
	     (setq z (timesin (car factor-list) (cdr product) power))
	     (when *rulesw*
	       (setq *rulesw* nil)
	       (setq product (tms-format-product z)))))
	  (t
	   (setq z (timesin factor (cdr product) power))
	   (if *rulesw*
	       (tms-format-product z)
	       product)))))

(defun tms-format-product (x)
  (cond ((zerop1 x) x)
	((mnump x) (list '(mtimes) x))
	((not (mtimesp x)) (list '(mtimes) 1 x))
	((not (mnump (cadr x))) (cons '(mtimes) (cons 1 (cdr x))))
	(t x)))

(defun plsk (x y)
  (cond ($ratsimpexpons (sratsimp (list '(mplus) x y)))
	((and (mnump x) (mnump y)) (addk x y))
	(t (add2 x y))))

(defun mult (x y)
  (if (and (mnump x) (mnump y))
      (timesk x y)
      (mul2 x y)))

(defun simp-limit (x vestigial z)
  (declare (ignore vestigial))
  (let ((l1 (length x))
	y)
    (unless (or (= l1 2) (= l1 4) (= l1 5))
      (merror (intl:gettext "limit: wrong number of arguments.")))
    (setq y (simpmap (cdr x) z))
    (cond ((and (= l1 5) (not (member (cadddr y) '($plus $minus))))
           (merror (intl:gettext "limit: direction must be either 'plus' or 'minus': ~M") (cadddr y)))
	  ((mnump (cadr y))
	   (merror (intl:gettext "limit: variable must not be a number; found: ~M") (cadr y)))
	  ((equal (car y) 1)
	   1)
	  (t
	   (eqtest (cons '(%limit) y) x)))))

(defun simpinteg (x vestigial z)
  (declare (ignore vestigial))
  (let ((l1 (length x))
	y)
    (unless (or (= l1 3) (= l1 5))
      (merror (intl:gettext "integrate: wrong number of arguments.")))
    (setq y (simpmap (cdr x) z))
    (cond ((mnump (cadr y))
	   (merror (intl:gettext "integrate: variable must not be a number; found: ~M") (cadr y)))
	  ((and (= l1 5) (alike1 (caddr y) (cadddr y)))
	   0)
          ((and (= l1 5)
                (free (setq z (sub (cadddr y) (caddr y))) '$%i)
                (eq ($sign z) '$neg))
	   (neg (simplifya (list '(%integrate) (car y) (cadr y) (cadddr y) (caddr y)) t)))
	  ((equal (car y) 1)
	   (if (= l1 3)
	       (cadr y)
	       (if (or (among '$inf z) (among '$minf z))
		   (infsimp z)
		   z)))
	  (t
	   (eqtest (cons '(%integrate) y) x)))))

(defun simpbigfloat (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  (bigfloatm* x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the Exp function.

(defprop $exp %exp verb)
(defprop $exp %exp alias)

(defprop %exp $exp noun)
(defprop %exp $exp reversealias)

(defprop %exp simp-exp operators)

(defun simp-exp (x y z)
  (oneargcheck x)
  (setq y (list '(mexpt) '$%e (cadr x)))
  (if z y (simplifya y nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simplambda (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  ; Check for malformed lambda expressions.
  ; We verify that we have a valid list of parameters and a non-empty body.
  (let ((params (cadr x)))
    (unless ($listp params)
      (merror (intl:gettext "lambda: first argument must be a list; found: ~M") params))
    (do ((params (cdr params) (cdr params))
         (seen-params nil))
        ((null params))
      (when (mdeflistp params)
        (setq params (cdar params)))
      (let ((p (car params)))
        (unless (or (mdefparam p)
                    (and (op-equalp p 'mquote)
                         (mdefparam (cadr p))))
          (merror (intl:gettext "lambda: parameter must be a symbol and must not be a system constant; found: ~M") p))
        (setq p (mparam p))
        (when (member p seen-params :test #'eq)
          (merror (intl:gettext "lambda: ~M occurs more than once in the parameter list") p))
        (push p seen-params))))
  (when (null (cddr x))
    (merror (intl:gettext "lambda: no body present")))
  (cons '(lambda simp) (cdr x)))

(defun simpmdef (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  (twoargcheck x)
  (cons '(mdefine simp) (cdr x)))

(defun simpmap (e z)
  (mapcar #'(lambda (u) (simpcheck u z)) e))

(defun infsimp (e)
  (let ((x ($expand e 1 1)))
    (cond ((or (not (free x '$ind)) (not (free x '$und))
	       (not (free x '$zeroa)) (not (free x '$zerob))
	       (not (free x '$infinity))
	       (mbagp x))
	   (infsimp2 x e))
	  ((and (free x '$inf) (free x '$minf)) x)
	  (t (infsimp1 x e)))))

(defun infsimp1 (x e)
  (let ((minf-coef (coeff x '$minf 1))
	(inf-coef (coeff x '$inf 1)))
    (cond ((or (and (equal minf-coef 0)
		    (equal inf-coef 0))
	       (and (not (free minf-coef '$inf))
		    (not (free inf-coef '$minf)))
	       (let ((new-exp (sub (add2 (mul2 minf-coef '$minf)
					 (mul2 inf-coef '$inf))
				   x)))
		 (and (not (free new-exp '$inf))
		      (not (free new-exp '$minf)))))
	   (infsimp2 x e))
	  (t (let ((sign-minf-coef ($asksign minf-coef))
		   (sign-inf-coef ($asksign inf-coef)))
	       (cond ((or (and (eq sign-inf-coef '$zero)
			       (eq sign-minf-coef '$neg))
			  (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$zero))
			  (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$neg)))  '$inf)
		     ((or (and (eq sign-inf-coef '$zero)
			       (eq sign-minf-coef '$pos))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$zero))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$pos)))  '$minf)
		     ((or (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$pos))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$neg)))  '$und)))))))

(defun infsimp2 (x e)
  (setq x ($limit x))
  (if (isinop x '%limit) e x))

(defun simpderiv (x y z)
  (prog (flag w u)
     (cond ((not (even (length x)))
	    (cond ((and (cdr x) (null (cdddr x))) (nconc x '(1)))
		  (t (wna-err '%derivative)))))
     (setq w (cons '(%derivative) (simpmap (cdr x) z)))
     (setq y (cadr w))
     (do ((u (cddr w) (cddr u))) ((null u))
       (cond ((mnump (car u))
	      (merror (intl:gettext "diff: variable must not be a number; found: ~M") (car u)))))
     (cond ((or (zerop1 y)
		(and (or (mnump y) (and (atom y) (constant y)))
		     (or (null (cddr w))
			 (and (not (alike1 y (caddr w)))
			      (do ((u (cddr w) (cddr u))) ((null u))
			        (cond ((and (numberp (cadr u))
			                    (not (zerop (cadr u))))
				       (return t))))))))
	    (return 0))
	   ((and (not (atom y)) (eq (caar y) '%derivative) derivsimp)
	    (rplacd w (append (cdr y) (cddr w)))))
     (if (null (cddr w))
	 (return (if (null derivflag) (list '(%del simp) y) (deriv (cdr w)))))
     (setq u (cdr w))
  ztest
     (cond ((null u) (go next))
           ((zerop1 (caddr u)) (rplacd u (cdddr u)))
           (t (setq u (cddr u))))
     (go ztest)
  next
     (cond ((null (cddr w)) (return y))
           ((and (null (cddddr w))
                 (onep (cadddr w))
                 (alike1 (cadr w) (caddr w)))
            (return 1)))
  again
     (setq z (cddr w))
  sort
     (cond ((null (cddr z)) (go loop))
           ((alike1 (car z) (caddr z))
            (rplaca (cdddr z) (add2 (cadr z) (cadddr z)))
            (rplacd z (cdddr z)))
           ((great (car z) (caddr z))
            (let ((u1 (car z)) (u2 (cadr z)) (v1 (caddr z)) (v2 (cadddr z)))
              (setq flag t)
              (rplaca z v1)
              (rplacd z (cons v2 (cons u1 (cons u2 (cddddr z))))))))
     (cond ((setq z (cddr z)) (go sort)))
  loop
     (cond ((null flag) (return (cond ((null derivflag) (eqtest w x))
                                      (t (deriv (cdr w)))))))
     (setq flag nil)
     (go again)))

(defun signum1 (x)
  (cond ((mnump x)
	 (setq x (num1 x)) (cond ((plusp x) 1) ((minusp x) -1) (t 0)))
	((atom x) 1)
	((mplusp x) (if *expandp* 1 (signum1 (car (last x)))))
	((mtimesp x) (if (mplusp (cadr x)) 1 (signum1 (cadr x))))
	(t 1)))

(defprop %signum (mlist $matrix mequal) distribute_over)

#+nil
(defun simpsignum (e y z)
  (declare (ignore y))
  (oneargcheck e)
  (let ((x (simpcheck (second e) z)) (sgn))
    
    (cond ((complex-number-p x #'mnump)
		    (if (complex-number-p x #'$ratnump) ;; nonfloat complex
		        (if (zerop1 x) 0 ($rectform (div x ($cabs x))))
		      (maxima::to (bigfloat::signum (bigfloat::to x)))))
		   
	  ;; idempotent: signum(signum(z)) = signum(z).
	  ((and (consp x) (consp (car x)) (eq '%signum (mop x))) x)
		   
	  (t
	   (setq sgn ($csign x))
	   (cond ((eq sgn '$neg) -1)
		 ((eq sgn '$zero) 0)
		 ((eq sgn '$pos) 1)

		 ;; multiplicative: signum(ab) = signum(a) * signum(b).
		 ((mtimesp x)
		  (muln (mapcar #'(lambda (s) (take '(%signum) s)) (margs x)) t))

		 ;; Reflection rule: signum(-x) --> -signum(x).
		 ((great (neg x) x) (neg (take '(%signum) (neg x))))
	
		 ;; nounform return
		 (t (eqtest (list '(%signum) x) e)))))))

(def-simplifier signum (x)
  (let (sgn)
    (cond ((complex-number-p x #'mnump)
	   (if (complex-number-p x #'$ratnump) ;; nonfloat complex
	       (if (zerop1 x)
		   0
		   ($rectform (div x ($cabs x))))
	       (maxima::to (bigfloat::signum (bigfloat::to x)))))
		   
	  ;; idempotent: signum(signum(z)) = signum(z).
	  ((and (consp x)
		(consp (car x))
		(eq '%signum (mop x)))
	   x)
		   
	  (t
     ;; Let's catch errors from $csign. Doing so allows signum(ind) to return a signum nounform, for example.
     (setq sgn (car (let (($errormsg nil) (errcatch t) (errset nil)) (errset ($csign x)))))

	   (cond ((eq sgn '$neg) -1)
		 ((eq sgn '$zero) 0)
		 ((eq sgn '$pos) 1)

		 ;; multiplicative: signum(ab) = signum(a) * signum(b).
		 ((mtimesp x)
		  (muln (mapcar #'(lambda (s) (take '(%signum) s)) (margs x))
			t))

		 ;; Reflection rule: signum(-x) --> -signum(x).
		 ((great (neg x) x)
		  (neg (take '(%signum) (neg x))))
	
		 ;; nounform return
		 (t (give-up)))))))

(defun exptrl (r1 r2)
  (cond ((equal r2 1) r1)
        ((equal r2 1.0) 
         (cond ((mnump r1) (addk 0.0 r1))
               ;; Do not simplify the type of the number away.
               (t (list '(mexpt simp) r1 1.0))))
        ((equal r2 *bigfloatone*)
         (cond ((mnump r1) ($bfloat r1))
               ;; Do not simplify the type of the number away.
               (t (list '(mexpt simp) r1 *bigfloatone*))))
	((zerop1 r1)
	 (cond ((or (zerop1 r2) (mnegp r2))
		(if (not errorsw)
		    (merror (intl:gettext "expt: undefined: ~M") (list '(mexpt) r1 r2))
		    (throw 'errorsw t)))
	       (t (zerores r1 r2))))
	((or (zerop1 r2) (onep1 r1))
	 (cond ((or ($bfloatp r1) ($bfloatp r2)) *bigfloatone*)
	       ((or (floatp r1) (floatp r2)) 1.0)
	       (t 1)))
	((or ($bfloatp r1) ($bfloatp r2)) ($bfloat (list '(mexpt) r1 r2)))
	((and (numberp r1) (integerp r2)) (exptb r1 r2))
	((and (numberp r1) (floatp r2) (equal r2 (float (floor r2))))
	 (exptb (float r1) (floor r2)))
	((or $numer (and (floatp r2) (or (plusp (num1 r1)) $numer_pbranch)))
	 (let (y)
	   (cond ((minusp (setq r1 (addk 0.0 r1)))
		  (cond ((or $numer_pbranch (eq $domain '$complex))
		         ;; for R1<0:
		         ;; R1^R2 = (-R1)^R2*cos(pi*R2) + i*(-R1)^R2*sin(pi*R2)
			 (setq r2 (addk 0.0 r2))
			 (setq y (exptrl (- r1) r2) r2 (* (mget '$%pi '$numer) r2))
			 (add2 (* y (cos r2))
			       (list '(mtimes simp) (* y (sin r2)) '$%i)))
			(t (setq y (let ($numer $float $keepfloat $ratprint)
				     (power -1 r2)))
			   (mul2 y (exptrl (- r1) r2)))))
	         ((equal (setq r2 (addk 0.0 r2)) (float (floor r2)))
	          (exptb r1 (floor r2)))
	         ((and (equal (setq y (* 2.0 r2)) (float (floor y)))
	               (not (equal r1 %e-val)))
		  (exptb (sqrt r1) (floor y)))
		 (t (exp (* r2 (log r1)))))))
	((floatp r2) (list '(mexpt simp) r1 r2))
	((integerp r2)
	 (cond ((minusp r2)
	        (exptrl (cond ((equal (abs (cadr r1)) 1)
	                       (* (cadr r1) (caddr r1)))
	                       ;; We set the simp flag at this place. This
	                       ;; changes nothing for an exponent r2 # -1.
	                       ;; exptrl is called again and does not look at
	                       ;; the simp flag. For the case r2 = -1 exptrl
	                       ;; is called with an exponent 1. For this case
	                       ;; the base is immediately returned. Now the
	                       ;; base has the correct simp flag. (DK 02/2010)
			      ((minusp (cadr r1))
			       (list '(rat simp) (- (caddr r1)) (- (cadr r1))))
			      (t (list '(rat simp) (caddr r1) (cadr r1))))
			(- r2)))
	       (t (list '(rat simp) (exptb (cadr r1) r2) (exptb (caddr r1) r2)))))
	((and (floatp r1) (alike1 r2 '((rat) 1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (sqrt (- r1)) '$%i)
	     (sqrt r1)))
	((and (floatp r1) (alike1 r2 '((rat) -1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (/ -1.0 (sqrt (- r1))) '$%i)
	     (/ (sqrt r1))))
	((floatp r1)
	 (if (plusp r1)
	     (exptrl r1 (fpcofrat r2))
	     (mul2 (exptrl -1 r2) ;; (-4.5)^(1/4) -> (4.5)^(1/4) * (-1)^(1/4)
		   (exptrl (- r1) r2))))
	(*exptrlsw* (list '(mexpt simp) r1 r2))
	(t
	 (let ((*exptrlsw* t))
	   (simptimes (list '(mtimes)
			    (exptrl r1 (truncate (cadr r2) (caddr r2)))
			    (let ((y (let ($keepfloat $ratprint)
				       (simpnrt r1 (caddr r2))))
				  (z (rem (cadr r2) (caddr r2))))
			      (if (mexptp y)
				  (list (car y) (cadr y) (mul2 (caddr y) z))
				  (power y z))))
		      1 t)))))

(defun simpexpt (x y z)
  (prog (gr pot check res *rulesw* w mlpgr mlppot)
     (setq check x)
     (twoargcheck x)
     (cond (z (setq gr (cadr x) pot (caddr x)) (go cont)))
     (setq gr (simplifya (cadr x) nil))
     (setq pot
           (let (($%enumer $numer))
             ;; Switch $%enumer on, when $numer is TRUE to allow 
             ;; simplification of $%e to its numerical value.
             (simplifya (if $ratsimpexpons ($ratsimp (caddr x)) (caddr x))
                        nil)))
  cont  
     (cond (($ratp pot)
            (setq pot (ratdisrep pot))
            (go cont))
           (($ratp gr)
            (cond ((member 'trunc (car gr))
                   (return (srf (list '(mexpt) gr pot))))
                  ((integerp pot)
                   (let ((varlist (caddar gr)) (genvar (cadddr (car gr))))
                     (return (ratrep* (list '(mexpt) gr pot)))))
                  (t
                   (setq gr (ratdisrep gr))
                   (go cont))))
           ((or (setq mlpgr (mxorlistp gr))
                (setq mlppot (mxorlistp pot)))
            (go matrix))
           ((onep1 pot) (go atgr))
           ((or (zerop1 pot) (onep1 gr)) (go retno))
           
           ;; This code tries to handle 0^a more complete.
           ;; If the sign of realpart(a) is not known return an unsimplified
           ;; expression. The handling of the flag *zexptsimp? is not changed.
           ;; Reverting the return of an unsimplified 0^a, because timesin
           ;; can not handle such expressions. (DK 02/2010)
           ((zerop1 gr)
            (cond ((or (member (setq z ($csign pot)) '($neg $nz))
                       (and *zexptsimp? (eq ($asksign pot) '$neg)))
                   ;; A negative exponent. Maxima error.
                   (cond ((not errorsw) (merror (intl:gettext "expt: undefined: 0 to a negative exponent.")))
                         (t (throw 'errorsw t))))
                  ((and (member z '($complex $imaginary))
                        ;; A complex exponent. Look at the sign of the realpart.
                        (member (setq z ($sign ($realpart pot))) 
                                '($neg $nz $zero)))
                   (cond ((not errorsw)
                          (merror (intl:gettext "expt: undefined: 0 to a complex exponent.")))
                         (t (throw 'errorsw t))))
                  ((and *zexptsimp? (eq ($asksign pot) '$zero))
                   (cond ((not errorsw)
                          (merror (intl:gettext "expt: undefined: 0^0")))
                         (t (throw 'errorsw t))))
                  ((not (member z '($pos $pz)))
                   ;; The sign of realpart(pot) is not known. We can not return
                   ;; an unsimplified 0^a expression, because timesin can not
                   ;; handle it. We return ZERO. That is the old behavior.
                   ;; Look for the imaginary symbol to be consistent with 
                   ;; old code.
                   (cond ((not (free pot '$%i))
                          (cond ((not errorsw)
                                 (merror (intl:gettext "expt: undefined: 0 to a complex exponent.")))
                                (t (throw 'errorsw t))))
                         (t
                          ;; Return ZERO and not an unsimplified expression.
                          (return (zerores gr pot)))))
                  (t (return (zerores gr pot)))))
           
           ((and (mnump gr)
                 (mnump pot)
                 (or (not (ratnump gr)) (not (ratnump pot))))
            (return (eqtest (exptrl gr pot) check)))
           ;; Check for numerical evaluation of the sqrt.
           ((and (alike1 pot '((rat) 1 2))
                 (or (setq res (flonum-eval '%sqrt gr))
                     (and (not (member 'simp (car x)))
                          (setq res (big-float-eval '%sqrt gr)))))
            (return res))
           ((eq gr '$%i)
            (return (%itopot pot)))
           ((and (realp gr) (minusp gr) (mevenp pot))
            (setq gr (- gr))
            (go cont))
           ((and (realp gr) (minusp gr) (moddp pot))
            (return (mul2 -1 (power (- gr) pot))))
           ((and (equal gr -1) (maxima-integerp pot) (mminusp pot))
            (setq pot (neg pot))
            (go cont))
           ((and (equal gr -1)
                 (maxima-integerp pot)
                 (mtimesp pot)
                 (= (length pot) 3)
                 (integerp (cadr pot))
                 (oddp (cadr pot))
                 (maxima-integerp (caddr pot)))
            (setq pot (caddr pot))
            (go cont))
           ((atom gr) (go atgr))
           ((and (eq (caar gr) 'mabs)
                 (or (evnump pot)
                     (mevenp pot))
                 (or (and (eq $domain '$real) (not (apparently-complex-to-judge-by-$csign-p (cadr gr))))
                     (and (eq $domain '$complex) (apparently-real-to-judge-by-$csign-p (cadr gr)))))
            (return (power (cadr gr) pot)))
           ((and (eq (caar gr) 'mabs)
                 (integerp pot)
                 (oddp pot)
                 (not (equal pot -1))
                 (or (and (eq $domain '$real) (not (apparently-complex-to-judge-by-$csign-p (cadr gr))))
                     (and (eq $domain '$complex) (apparently-real-to-judge-by-$csign-p (cadr gr)))))
            ;; abs(x)^(2*n+1) -> abs(x)*x^(2*n), n an integer number
            (if (plusp pot)
                (return (mul (power (cadr gr) (add pot -1))
                             gr))
                (return (mul (power (cadr gr) (add pot 1))
                             (inv gr)))))
           ((eq (caar gr) 'mequal)
            (return (eqtest (list (ncons (caar gr))
                                  (power (cadr gr) pot)
                                  (power (caddr gr) pot))
                            gr)))
           ((symbolp pot) (go opp))
           ((eq (caar gr) 'mexpt) (go e1))
           ((and (eq (caar gr) '%sum)
                 $sumexpand
                 (integerp pot)
                 (signp g pot)
                 (< pot $maxposex))
            (return (do ((i (1- pot) (1- i))
                         (an gr (simptimes (list '(mtimes) an gr) 1 t)))
                        ((signp e i) an))))
           ((equal pot -1) 
            (return (eqtest (testt (tms gr pot nil)) check)))
           ((fixnump pot)
            (return (eqtest (cond ((and (mplusp gr)
                                        (not (or (> pot $expop)
                                                 (> (- pot) $expon))))
                                   (expandexpt gr pot))
                                  (t (simplifya (tms gr pot nil) t)))
                            check))))
     
  opp
     (cond ((eq (caar gr) 'mexpt) (go e1))
           ((eq (caar gr) 'rat)
            (return (mul2 (power (cadr gr) pot)
                          (power (caddr gr) (mul2 -1 pot)))))
           ((not (eq (caar gr) 'mtimes)) (go up))
           ((or (eq $radexpand '$all) (and $radexpand (simplexpon pot)))
            (setq res (list 1))
            (go start))
           ((and (or (not (numberp (cadr gr)))
                     (equal (cadr gr) -1))
                 (equal -1 ($num gr)) ; only for -1
                 ;; Do not simplify for a complex base.
                 (not (member ($csign gr) '($complex $imaginary)))
                 (and (eq $domain '$real) $radexpand))
            ;; (-1/x)^a -> 1/(-x)^a for x negative
            ;; For all other cases (-1)^a/x^a
            (if (eq ($csign (setq w ($denom gr))) '$neg)
                (return (inv (power (neg w) pot)))
                (return (div (power -1 pot)
                             (power w pot)))))
           ((or (eq $domain '$complex) (not $radexpand)) (go up)))
     (return (do ((l (cdr gr) (cdr l)) (res (ncons 1)) (rad))
                 ((null l)
                  (cond ((equal res '(1))
                         (eqtest (list '(mexpt) gr pot) check))
                        ((null rad) 
                         (testt (cons '(mtimes simp) res)))
                        (t
                         (setq rad (power* ; RADEXPAND=()?
                                     (cons '(mtimes) (nreverse rad)) pot))
                         (cond ((not (onep1 rad))
                                (setq rad
                                      (testt (tms rad 1 (cons '(mtimes) res))))
                                (cond (*rulesw*
                                       (setq *rulesw* nil res (cdr rad))))))
                         (eqtest (testt (cons '(mtimes) res)) check))))
               ;; Check with $csign to be more complete. This prevents wrong 
               ;; simplifications like sqrt(-z^2)->%i*sqrt(z^2) for z complex.
               (setq z ($csign (car l)))
               (if (member z '($complex $imaginary))
                   (setq z '$pnz)) ; if appears complex, unknown sign
               (setq w (cond ((member z '($neg $nz))
                              (setq rad (cons -1 rad))
                              (mult -1 (car l)))
                             (t (car l))))
               (cond ((onep1 w))
                     ((alike1 w gr) (return (list '(mexpt simp) gr pot)))
                     ((member z '($pn $pnz))
                      (setq rad (cons w rad)))
                     (t
                      (setq w (testt (tms (simplifya (list '(mexpt) w pot) t)
                                          1 (cons '(mtimes) res))))))
               (cond (*rulesw* (setq *rulesw* nil res (cdr w))))))
     
  start
     (cond ((and (cdr res) (onep1 (car res)) (ratnump (cadr res)))
            (setq res (cdr res))))
     (cond ((null (setq gr (cdr gr)))
            (return (eqtest (testt (cons '(mtimes) res)) check)))
           ((mexptp (car gr))
            (setq y (power (cadar gr) (mult (caddar gr) pot))))
           ((eq (car gr) '$%i)
            (setq y (%itopot pot)))
           ((mnump (car gr))
            (setq y (list '(mexpt) (car gr) pot)))
           (t (setq y (list '(mexpt simp) (car gr) pot))))
     (setq w (testt (tms (simplifya y t) 1 (cons '(mtimes) res))))
     (cond (*rulesw* (setq *rulesw* nil res (cdr w))))
     (go start)
     
  retno
     (return (exptrl gr pot))
     
  atgr
     (cond ((zerop1 pot) (go retno))
           ((onep1 pot)
            (let ((y (mget gr '$numer)))
              (if (and y (floatp y) (or $numer (not (equal pot 1))))
                  ;; A numeric constant like %e, %pi, ... and 
                  ;; exponent is a float or bigfloat value.
                  (return (if (and (member gr *builtin-numeric-constants*)
                                   (equal pot *bigfloatone*))
                              ;; Return a bigfloat value.
                              ($bfloat gr)
                              ;; Return a float value.
                              y))
                  ;; In all other cases exptrl simplifies accordingly.
                  (return (exptrl gr pot)))))
           ((eq gr '$%e)
            ;; Numerically evaluate if the power is a flonum.
            (when $%emode
              (let ((val (flonum-eval '%exp pot)))
		(when (float-inf-p val)
		    ;; needed for gcl and sbcl - (sometimes) no trap of overflow
		    (error 'floating-point-overflow))
                (when val
                  (return val)))
              ;; Numerically evaluate if the power is a (complex)
              ;; big-float.  (This is basically the guts of
              ;; big-float-eval, but we can't use big-float-eval.)
              (when (and (not (member 'simp (car x)))
                         (complex-number-p pot 'bigfloat-or-number-p))
                (destructuring-bind (x . y) (trisplit pot)
                  (cond ((and ($bfloatp x) (eql 0 y))
                         (return ($bfloat `((mexpt simp) $%e ,pot))))
                        ((or ($bfloatp x) ($bfloatp y))
                         (let ((z (add ($bfloat x) (mul '$%i ($bfloat y)))))
                           (setq z ($rectform `((mexpt simp) $%e ,z)))
                           (return ($bfloat z))))))))
            (cond ((and $logsimp (among '%log pot)) (return (%etolog pot)))
                  ((and $demoivre (setq z (demoivre pot))) (return z))
                  ((and $%emode
                        (among '$%i pot)
                        (among '$%pi pot)
                        ;; Exponent contains %i and %pi and %emode is TRUE:
                        ;; Check simplification of exp(%i*%pi*p/q*x)
                        (setq z (%especial pot)))
                   (return z))
                  (($taylorp (third x))
                   ;; taylorize %e^taylor(...)
                   (return ($taylor x)))))
           (t
            (let ((y (mget gr '$numer)))
              ;; Check for a numeric constant.
              (and y
                   (floatp y)
                   (or (floatp pot)
                       ;; The exponent is a bigfloat. Convert base to bigfloat.
                       (and ($bfloatp pot)
                            (member gr *builtin-numeric-constants*)
                            (setq y ($bfloat gr)))
                       (and $numer (integerp pot)))
                   (return (exptrl y pot))))))

  up 
     (return (eqtest (list '(mexpt) gr pot) check))

  matrix
     (cond ((zerop1 pot)
            (cond ((mxorlistp1 gr) (return (constmx (addk 1 pot) gr)))
                  (t (go retno))))
           ((onep1 pot) (return gr))
           ((or $doallmxops $doscmxops $domxexpt)
            (cond ((or (and mlpgr
                            (or (not ($listp gr)) $listarith)
                            (scalar-or-constant-p pot $assumescalar))
                       (and $domxexpt
                            mlppot
                            (or (not ($listp pot)) $listarith)
                            (scalar-or-constant-p gr $assumescalar)))
                   (return (simplifya (outermap1 'mexpt gr pot) t)))
                  (t (go up))))
           ((and $domxmxops (member pot '(-1 -1.0)))
            (return (simplifya (outermap1 'mexpt gr pot) t)))
           (t (go up)))
  e1 
     ;; At this point we have an expression: (z^a)^b with gr = z^a and pot = b
     (cond ((or (eq $radexpand '$all)
                ;; b is an integer or an odd rational
                (simplexpon pot)
                (and (eq $domain '$complex)
                     (not (member ($csign (caddr gr)) '($complex $imaginary)))
                         ;; z >= 0 and a not a complex
                     (or (member ($csign (cadr gr)) '($pos $pz $zero))
                         ;; -1 < a <= 1
                         (and (mnump (caddr gr))
                              (eq ($sign (sub 1 (take '(mabs) (caddr gr))))
                                  '$pos))))
                (and (eq $domain '$real)
                     (member ($csign (cadr gr)) '($pos $pz $zero)))
                ;; (1/z)^a -> 1/z^a when z a constant complex
                (and (eql (caddr gr) -1)
                     (or (and $radexpand
                              (eq $domain '$real))
                         (and (eq ($csign (cadr gr)) '$complex)
                              ($constantp (cadr gr)))))
                ;; This does (1/z)^a -> 1/z^a. This is in general wrong.
                ;; We switch this type of simplification on, when
                ;; $ratsimpexpons is T. E.g. radcan sets this flag to T.
                ;; radcan hangs for expressions like sqrt(1/(1+x)) without
                ;; this simplification.
                (and $ratsimpexpons
                     (equal (caddr gr) -1))
                (and $radexpand
                     (eq $domain '$real)
                     (odnump (caddr gr))))
            ;; Simplify (z^a)^b -> z^(a*b)
            (setq pot (mul pot (caddr gr))
                  gr (cadr gr)))
           ((and (eq $domain '$real)
                 (free gr '$%i)
                 $radexpand
                 (not (apparently-complex-to-judge-by-$csign-p (cadr gr)))
                 (evnump (caddr gr)))
            ;; Simplify (x^a)^b -> abs(x)^(a*b)
            (setq pot (mul pot (caddr gr))
                  gr (radmabs (cadr gr))))
           ((and $radexpand
                 (eq $domain '$real)
                 (mminusp (caddr gr)))
            ;; Simplify (1/z^a)^b -> 1/(z^a)^b
            (setq pot (neg pot)
                  gr (power (cadr gr) (neg (caddr gr)))))
           (t (go up)))
     (go cont)))

(defun apparently-complex-to-judge-by-$csign-p (e)
  (let ((s ($csign e)))
    (and (member s '($complex $imaginary)) t)))

(defun apparently-real-to-judge-by-$csign-p (e)
  (let ((s ($csign e)))
    (and (member s '($pos $neg $zero $pn $pnz $pz $nz)) t)))

;; Basically computes log of m base b.  Except if m is not a power
;; of b, we return nil.  m is a positive integer and base an integer
;; not equal to +/-1.
(defun exponent-of (m base)
  ;; Just compute base^k until base^k >= m.  Then check if they're equal.
  ;; If so, we have the exponent.  Otherwise, give up.
  (let ((expo 0))
    (loop
      (multiple-value-bind (q r)
          (floor m base)
        (cond ((zerop r)
               (setf m q)
               (incf expo))
              (t (return nil)))))
    (if (zerop expo) nil expo)))

(defun timesin (x y w)                  ; Multiply X^W into Y
  (prog (fm temp z check u expo)
     (if (mexptp x) (setq check x))
  top
     ;; Prepare the factor x^w and initialize the work of timesin
     (cond ((equal w 1)
            (setq temp x))
           (t
            (setq temp (cons '(mexpt) (if check 
                                          (list (cadr x) (mult (caddr x) w))
                                          (list x w))))
            (if (and (not *timesinp*) (not (eq x '$%i)))
                (let ((*timesinp* t))
                  (setq temp (simplifya temp t))))))
     (setq x (if (mexptp temp)
                 (cdr temp)
                 (list temp 1)))
     (setq w (cadr x)
           fm y)
  start
     ;; Go through the list of terms in fm and look what is to do.
     (cond ((null (cdr fm))
            ;; The list of terms is empty. The loop is finished.
            (go less))
           ((or (and (mnump temp)
                     (not (or (integerp temp)
                              (ratnump temp))))
                (and (integerp temp)
                     (equal temp -1)))
            ;; Stop the loop for a float or bigfloat number, or number -1.
            (go less))
           ((mexptp (cadr fm))
            (cond ((alike1 (car x) (cadadr fm))
                   (cond ((zerop1 (setq w (plsk (caddr (cadr fm)) w)))
                          (go del))
                         ((and (mnump w)
                               (or (mnump (car x))
                                   (eq (car x) '$%i)))
                          (rplacd fm (cddr fm))
                          (cond ((mnump (setq x (if (mnump (car x))
                                                    (exptrl (car x) w)
                                                    (power (car x) w))))
                                 (return (rplaca y (timesk (car y) x))))
                                ((mtimesp x)
                                 (go times))
                                (t
                                 (setq temp x
                                       x (if (mexptp x) (cdr x) (list x 1)))
                                 (setq w (cadr x)
                                       fm y)
                                 (go start))))
                         ((maxima-constantp (car x))
                          (go const))
                         ((onep1 w)
                          (cond ((mtimesp (car x))
                                 ;; A base which is a mtimes expression. Remove
                                 ;; the factor from the lists of products.
                                 (rplacd fm (cddr fm))
                                 ;; Multiply the factors of the base with
                                 ;; the list of all remaining products.
                                 (setq *rulesw* t)
                                 (return (muln (concatenate 'list y (cdar x)) t)))
                                (t (return (rplaca (cdr fm) (car x))))))
                         (t
                          (go spcheck))))
                  ;; At this place we have to add code for a rational number
                  ;; as a factor to the list of products.
                  ((and (onep1 w)
                        (or (ratnump (car x))
                            (and (integerp (car x))
                                 (not (onep (car x))))))
                   ;; Multiplying base^k * num/den
                   (let ((num (num1 (car x)))
                         (den (denom1 (car x)))
                         (base (second (cadr fm))))
                     (cond ((and (integerp base)
                                 (not (eql 1 (abs base)))
                                 (setq expo (exponent-of (abs num) base)))
                            ;; We have base^m*base^k = base^(k+m).
                            (setq temp (power base
                                              (add (third (cadr fm)) expo)))
                            ;; Set fm to have 1/denom term.
                            (setq x (mul (car y)
                                         (div (div num
                                                   (exptrl base expo))
                                              den))))
                           ((and (integerp base)
                                 (not (eql 1 (abs base)))
                                 (setq expo (exponent-of den base)))
                            (setq expo (- expo))
                            ;; We have base^(-m)*base^k = base^(k-m).
                            (setq temp (power base
                                              (add (third (cadr fm)) expo)))
                            ;; Set fm to have the numerator term.
                            (setq x (mul (car y)
                                         (div num
                                              (div den
                                                   (exptrl base (- expo)))))))
                           (t
                            ;; Next term in list of products.
                            (setq fm (cdr fm))
                            (go start)))
                     ;; Add in the base^(k+m) term or base^(k-m)
                     (setf y (rplaca y 1))
                     (rplacd fm (cddr fm))
                     (rplacd fm (cons temp (cdr fm)))
                     (setq temp x
                           x (list x 1)
                           w 1
                           fm y)
                     (go start)))
                  ((and (not (atom (car x)))
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) 1)
                        (integerp (caddr (cadr fm)))
                        (< (caddr (cadr fm)) -1)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex imaginary))))
                   ;; 1/x^n*abs(x) -> 1/(x^(n-2)*abs(x)), where n an integer
                   ;; Replace 1/x^n -> 1/x^(n-2)
                   (setq temp (power (cadr (cadr fm))
                                     (add (caddr (cadr fm)) 2)))
                   (rplacd fm (cddr fm))
                   (if (not (equal temp 1))
                       (rplacd fm (cons temp (cdr fm))))
                   ;; Multiply factor 1/abs(x) into list of products.
                   (setq x (list (car x) -1))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((and (not (atom (car x)))
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) -1)
                        (integerp (caddr (cadr fm)))
                        (> (caddr (cadr fm)) 1)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex imaginary))))
                   ;; x^n/abs(x) -> x^(n-2)*abs(x), where n an integer.
                   ;; Replace x^n -> x^(n-2)
                   (setq temp (power (cadr (cadr fm)) 
                                     (add (caddr (cadr fm)) -2)))
                   (rplacd fm (cddr fm))
                   (if (not (equal temp 1))
                       (rplacd fm (cons temp (cdr fm))))
                   ;; Multiply factor abs(x) into list of products.
                   (setq x (list (car x) 1))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((and (not (atom (cadr fm)))
                        (not (atom (cadr (cadr fm))))
                        (eq (caaadr (cadr fm)) 'mabs)
                        (equal (caddr (cadr fm)) -1)
                        (integerp (cadr x))
                        (> (cadr x) 1)
                        (alike1 (cadadr (cadr fm)) (car x))
                        (not (member ($csign (cadadr (cadr fm)))
                                     '($complex imaginary))))
                   ;; 1/abs(x)*x^n -> x^(n-2)*abs(x), where n an integer.
                   ;; Replace 1/abs(x) -> abs(x)
                   (setq temp (cadr (cadr fm)))
                   (rplacd fm (cddr fm))
                   (rplacd fm (cons temp (cdr fm)))
                   ;; Multiply factor x^(n-2) into list of products.
                   (setq x (list (car x) (add (cadr x) -2)))
                   (setq temp (power (car x) (cadr x)))
                   (setq w (cadr x))
                   (go start))
                  
                  ((or (maxima-constantp (car x))
                       (maxima-constantp (cadadr fm)))
                   (if (great temp (cadr fm))
                       (go gr)))
                  ((great (car x) (cadadr fm))
                   (go gr)))
            (go less))
           ((alike1 (car x) (cadr fm))
            (go equ))
          ((mnump temp)
           ;; When a number goto start and look in the next term.
           (setq fm (cdr fm))
           (go start))
           
           ((and (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (integerp (cadr x))
                 (< (cadr x) -1)
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                                     '($complex imaginary))))
            ;; abs(x)/x^n -> 1/(x^(n-2)*abs(x)), where n an integer.
            ;; Replace abs(x) -> 1/abs(x).
            (setq temp (power (cadr fm) -1))
            (rplacd fm (cddr fm))
            (rplacd fm (cons temp (cdr fm)))
            ;; Multiply factor x^(-n+2) into list of products.
            (setq x (list (car x) (add (cadr x) 2)))
            (setq temp (power (car x) (cadr x)))
            (setq w (cadr x))
            (go start))
           
           ((maxima-constantp (car x))
            (when (great temp (cadr fm))
              (go gr)))
           ((great (car x) (cadr fm))
            (go gr)))
  less
     (cond ((mnump temp)
           ;; Multiply a number into the list of products.
           (return (rplaca y (timesk (car y) temp))))
           ((and (eq (car x) '$%i)
                 (fixnump w))
            (go %i))
           ((and (eq (car x) '$%e)
                 $numer
                 (integerp w))
            (return (rplaca y (timesk (car y) (exp (float w))))))
           ((and (onep1 w)
                 (not (constant (car x))))
            (go less1))                  
           ;; At this point we will insert a mexpt expression,
           ;; but first we look at the car of the list of products and
           ;; modify the expression if we found a rational number.
           ((and (mexptp temp)
                 (not (onep1 (car y)))
                 (or (integerp (car y))
                     (ratnump (car y))))
            ;; Multiplying base^k * num/den.
            (let ((num (num1 (car y)))
                  (den (denom1 (car y)))
                  (base (car x)))
              (cond ((and (integerp base)
                          (not (eql 1 (abs base)))
                          (setq expo (exponent-of (abs num) base)))
                     ;; We have base^m*base^k.
                     (setq temp (power base (add (cadr x) expo)))
                     ;; Set fm to have 1/denom term.
                     (setq x (div (div num (exptrl base expo)) den)))
                    ((and (integerp base)
                          (not (eql 1 (abs base)))
                          (setq expo (exponent-of den base)))
                     (setq expo (- expo))
                     ;; We have base^(-m)*base^k.
                     (setq temp (power base (add (cadr x) expo)))
                     ;; Set fm to have the numerator term.
                     (setq x (div num (div den (exptrl base (- expo))))))
                    (t
                     ;; The rational doesn't contain any (simple) powers of
                     ;; the exponential term.  We're done.
                     (return (cdr (rplacd fm (cons temp (cdr fm)))))))
              ;; Add in the a^(m+k) or a^(k-m) term.
              (setf y (rplaca y 1))
              (rplacd fm (cons temp (cdr fm)))
              (setq temp x
                    x (list x 1)
                    w 1
                    fm y)
              (go start)))
           ((and (maxima-constantp (car x))
                 (do ((l (cdr fm) (cdr l)))
                     ((null (cdr l)))
                   (when (and (mexptp (cadr l))
                              (alike1 (car x) (cadadr l)))
                     (setq fm l)
                     (return t))))
            (go start))
           ((or (and (mnump (car x))
                     (mnump w))
                (and (eq (car x) '$%e)
                     $%emode
                     (among '$%i w)
                     (among '$%pi w)
                     (setq u (%especial w))))
            (setq x (cond (u)
                          ((alike (cdr check) x)
                           check)
                          (t
                           (exptrl (car x) w))))
            (cond ((mnump x)
                   (return (rplaca y (timesk (car y) x))))
                  ((mtimesp x)
                   (go times))
                  ((mexptp x)
                   (return (cdr (rplacd fm (cons x (cdr fm))))))
                  (t
                   (setq temp x
                         x (list x 1)
                         w 1
                         fm y)
                   (go start))))
           ((onep1 w)
            (go less1))
           (t
            (setq temp (list '(mexpt) (car x) w))
            (setq temp (eqtest temp (or check '((foo)))))
            (return (cdr (rplacd fm (cons temp (cdr fm)))))))
  less1
     (return (cdr (rplacd fm (cons (car x) (cdr fm)))))
  gr
     (setq fm (cdr fm))
     (go start)
  equ
     (cond ((and (eq (car x) '$%i) (equal w 1))
            (rplacd fm (cddr fm))
            (return (rplaca y (timesk -1 (car y)))))
           ((zerop1 (setq w (plsk 1 w)))
            (go del))
           ((and (mnump (car x)) (mnump w))
            (return (rplaca (cdr fm) (exptrl (car x) w))))
           ((maxima-constantp (car x))
            (go const)))
  spcheck
     (setq z (list '(mexpt) (car x) w))
     (cond ((alike1 (setq x (simplifya z t)) z)
            (return (rplaca (cdr fm) x)))
           (t
            (rplacd fm (cddr fm))
            (setq *rulesw* t)
            (return (muln (cons x y) t))))
  const
     (rplacd fm (cddr fm))
     (setq x (car x) check nil)
     (go top)
  times
     (setq z (tms x 1 (setq temp (cons '(mtimes) y))))
     (return (cond ((eq z temp)
                    (cdr z))
                   (t
                    (setq *rulesw* t) z)))
  del
     (return (rplacd fm (cddr fm)))
  %i
     (if (minusp (setq w (rem w 4)))
         (incf w 4))
     (return (cond ((zerop w)
                    fm)
                   ((= w 2)
                    (rplaca y (timesk -1 (car y))))
                   ((= w 3)
                    (rplaca y (timesk -1 (car y)))
                    (rplacd fm (cons '$%i (cdr fm))))
                   (t
                    (rplacd fm (cons '$%i (cdr fm))))))))

(defun simpmatrix (x vestigial z)
  (declare (ignore vestigial))
  (if (and (null (cddr x))
	   $scalarmatrixp
	   (or (eq $scalarmatrixp '$all) (member 'mult (cdar x)))
	   ($listp (cadr x)) (cdadr x) (null (cddadr x)))
      (if z (cadadr x) (simplifya (cadadr x) nil))
      (let ((badp (dolist (row (cdr x)) (if (not ($listp row)) (return t))))
	    (args (simpmap (cdr x) z)))
	(if (and args (not badp)) (matcheck args))
	(cons (if badp '(%matrix simp) '($matrix simp)) args))))

(defun %itopot (pot)
  (if (fixnump pot)
      (let ((i (boole  boole-and pot 3)))
	(cond ((= i 0) 1)
	      ((= i 1) '$%i)
	      ((= i 2) -1)
	      (t (list '(mtimes simp) -1 '$%i))))
    (power -1 (mul2 pot '((rat simp) 1 2)))))

(defun mnlogp (pot)
  (cond ((eq (caar pot) '%log) (simplifya (cadr pot) nil))
	((and (eq (caar pot) 'mtimes)
	      (or (maxima-integerp (cadr pot))
	          (and $%e_to_numlog ($numberp (cadr pot))))
	      (not (atom (caddr pot))) (eq (caar (caddr pot)) '%log)
	      (null (cdddr pot)))
	 (power (cadr (caddr pot)) (cadr pot)))))

(defun mnlog (pot)
  (prog (a b c)
   loop (cond ((null pot)
	       (cond (a (setq a (cons '(mtimes) a))))
	       (cond (c (setq c (list '(mexpt simp) '$%e (addn c nil)))))
	       (return (cond ((null c) (simptimes a 1 nil))
			     ((null a) c)
			     (t (simptimes (append a (list c)) 1 nil)))))
	      ((and (among '%log (car pot)) (setq b (mnlogp (car pot))))
	       (setq a (cons b a)))
	      (t (setq c (cons (car pot) c))))
   (setq pot (cdr pot))
   (go loop)))

(defun %etolog (pot) (cond ((mnlogp pot))
			   ((eq (caar pot) 'mplus) (mnlog (cdr pot)))
			   (t (list '(mexpt simp) '$%e pot))))

(defun zerores (r1 r2)
  (cond ((or ($bfloatp r1) ($bfloatp r2)) *bigfloatzero*)
	((or (floatp r1) (floatp r2)) 0.0)
	(t 0)))

(defun evnump (n) (or (even n) (and (ratnump n) (even (cadr n)))))
(defun odnump (n) (or (and (integerp n) (oddp n))
		      (and (ratnump n) (oddp (cadr n)))))

(defun simplexpon (e)
  (or (maxima-integerp e)
      (and (eq $domain '$real) (ratnump e) (oddp (caddr e)))))

;; This function is not called in Maxima core or share code
;; and can be cut out.
(defun noneg (p)
  (and (free p '$%i) (member ($sign p) '($pos $pz $zero)) t))

(defun radmabs (e)
  (if (and limitp (free e '$%i)) (asksign-p-or-n e))
  (simplifya (list '(mabs) e) t))

(defun simpmqapply (exp y z)
  (let ((simpfun (and (not (atom (cadr exp))) (safe-get (caaadr exp) 'specsimp))) u)
    (if simpfun
	(funcall simpfun exp y z)
	(progn (setq u (simpargs exp z))
	       (if (symbolp (cadr u))
		   (simplifya (cons (cons (cadr u) (cdar u)) (cddr u)) t)
		   u)))))

;; TRUE, if the symbol e is declared to be $complex or $imaginary.
(defun decl-complexp (e)
  (kindp e '$complex))

;; TRUE, if the symbol e is declared to be $real, $rational, $irrational,
;; $integer, $even or $odd.
(defun decl-realp (e)
  (kindp e '$real))

;; TRUE, if the symbol e is declared to be $imaginary.
(defun decl-imaginaryp (e)
  (kindp e '$imaginary))

;; WARNING:  Exercise extreme caution when modifying this function!
;;
;; Richard Fateman and Stavros Macrakis both say that changing the
;; actual ordering relations (as opposed to making them faster to
;; determine) could have very subtle and wide-ranging effects.  Also,
;; the simplifier spends the vast majority of its time here, so be
;; very careful about changes that may drastically slow down the
;; simplifier.
(defun great (x y)
  (cond ((atom x)
	 (cond ((atom y)
		(cond ((numberp x)
		       (cond ((numberp y)
			      ;; If x - y > 0, then x is "greater" than y.
				  ;; If x - y = 0 and x is a float and y is not, then x is "greater" than y.
			      (let ((diff (- x y)))
			        (cond ((zerop diff) (and (floatp x) (not (floatp y)))) (t (plusp diff)))))))
		      ((constant x)
		       (cond ((constant y) (alphalessp y x)) (t (numberp y))))
		      ((mget x '$scalar)
		       (cond ((mget y '$scalar) (alphalessp y x))
		             (t (maxima-constantp y))))
		      ((mget x '$mainvar)
		       (cond ((mget y '$mainvar) (alphalessp y x)) (t t)))
		      (t (or (maxima-constantp y) (mget y '$scalar)
			     (and (not (mget y '$mainvar)) (not (null (alphalessp y x))))))))
	       (t (not (ordfna y x)))))
	((atom y) (ordfna x y))
	((eq (caar x) 'rat)
	 (cond ((eq (caar y) 'rat)
		(> (* (caddr y) (cadr x)) (* (caddr x) (cadr y))))))
	((eq (caar y) 'rat))
	((or (member (caar x) '(mtimes mplus mexpt %del))
	     (member (caar y) '(mtimes mplus mexpt %del)))
	 (ordfn x y))
	((and (eq (caar x) 'bigfloat) (eq (caar y) 'bigfloat))
      (ordbfloat x y))
	((or (eq (caar x) 'mrat) (eq (caar y) 'mrat))
	 (error "GREAT: internal error: unexpected MRAT argument"))
	(t (do ((x1 (margs x) (cdr x1)) (y1 (margs y) (cdr y1))) (())
	     (cond ((null x1)
		    (return (cond (y1 nil)
				  ((not (alike1 (mop x) (mop y)))
				   (great (mop x) (mop y)))
				  ((member 'array (cdar x)) t))))
		   ((null y1) (return t))
		   ((not (alike1 (car x1) (car y1)))
		    (return (great (car x1) (car y1)))))))))

;; Not sure if we want to enable comparison of maxima arrays.
;; Aside from that, add2lnc calls alike1 (via memalike) and that causes trouble.
;; Possible code for ALIKE1 to enable such comparisons should we choose to do so:
;;
;; ((maxima-declared-arrayp x)
;;  (and (maxima-declared-arrayp y) (maxima-declared-array-alike1 x y)))
;; ((maxima-undeclared-arrayp x)
;;  (and (maxima-undeclared-arrayp y) (maxima-undeclared-array-alike1 x y)))

(defun maxima-declared-array-alike1 (x y)
  (lisp-array-alike1 (get (mget x 'array) 'array) (get (mget y 'array) 'array)))

(defun maxima-undeclared-array-alike1 (x y)
  (and
    (alike1 (mfuncall '$arrayinfo x) (mfuncall '$arrayinfo y))
    (alike1 ($listarray x) ($listarray y))))

(defun ordfna (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
        ((and (not (member (caar e) '(mplus mtimes mexpt)))
              (constant a))
	 (not (member (caar e) '(rat bigfloat))))
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)
	((eq (caar e) 'mexpt) (ordmexpt e a))
	((member (caar e) '(mplus mtimes))
	 (let ((u (car (last e))))
	   (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
	((eq (caar e) '%del))
	((prog2 (setq e (car (margs e)))	; use first arg of e
	     (and (not (atom e)) (member (caar e) '(mplus mtimes))))
	 (let ((u (car (last e))))		; and compare using 
	   (cond ((eq u a) (not (ordhack e)))	; same procedure as above
		 (t (great u a)))))
	((eq e a))
	(t (great e a))))

;; compare lists a and b elementwise from back to front
(defun ordlist (a b cx cy)
  (prog (l1 l2 c d)
     (setq l1 (length a) l2 (length b))
     loop (cond ((= l1 0)
		 (return (cond ((= l2 0) (eq cx 'mplus))
			       ((and (eq cx cy) (= l2 1))
				(great (cond ((eq cx 'mplus) 0) (t 1)) (car b))))))
		((= l2 0)
		  (return (not (if (and (eq cx cy) (= l1 1))
				(great (cond ((eq cy 'mplus) 0) (t 1)) (car a)))))))
     (setq c (nthelem l1 a) d (nthelem l2 b))
     (cond ((not (alike1 c d)) (return (great c d))))
     (setq l1 (1- l1) l2 (1- l2))
     (go loop)))

(defun term-list (x)
  (if (mplusp x)
      (cdr x)
    (list x)))

(defun factor-list (x)
  (if (mtimesp x)
      (cdr x)
    (list x)))

;; one of the exprs x or y should be one of:
;; %del, mexpt, mplus, mtimes
(defun ordfn (x y)
  (let ((cx (caar x)) (cy (caar y)))
    (cond ((eq cx '%del) (if (eq cy '%del) (great (cadr x) (cadr y)) t))
	  ((eq cy '%del) nil)
	  ((or (eq cx 'mtimes) (eq cy 'mtimes))
	   (ordlist (factor-list x) (factor-list y) 'mtimes 'mtimes))
	  ((or (eq cx 'mplus) (eq cy 'mplus))
	   (ordlist (term-list x) (term-list y) 'mplus 'mplus))
	  ((eq cx 'mexpt) (ordmexpt x y))
	  ((eq cy 'mexpt) (not (ordmexpt y x))))))

(defun ordhack (x)
  (if (and (cddr x) (null (cdddr x)))
      (great (if (eq (caar x) 'mplus) 0 1) (cadr x))))

;; Return great(x,y), where x is an `mexpt` expression and y is any Maxima expression. 
(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be a `mexpt` expression and `y` may 
  or may not be a `mexpt` expression."
  ;(assert (mexptp x) (x) "ordmexpt: first argument must be a mexpt expression")
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be a mexpt expression, but y need not be a mexpt expression.
  (let ((base-x (second x)) (exp-x (third x)))
     (multiple-value-bind (base-y exp-y)
        (if (mexptp y)
            (values (second y) (third y))
            (values y 1))
    (cond 
      ;; bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; default: comparison between bases
      (t (great base-x base-y))))))

(defun ordbfloat (x y)
  "'Greater than' predicate (in the expression ordering, not numerical sense)
  for two bigfloats, which can differ in presence of a SIMP flag, precision and
  presence of a DECIMAL flag (radix).
  Bigfloats of the same precision and radix are sorted numerically among each other,
  but only based on their internal representation, without subtracting them.
  For bigfloats with different precision and/or radix, the sorting order is well
  defined, but not related to their numerical value."
  (let* ((mant-x (cadr x))
         (mant-y (cadr y))
         (sgn-mant-x (signum mant-x))
         (sgn-mant-y (signum mant-y)))
    (cond
      ((= sgn-mant-x sgn-mant-y)
        ;; Equal mantissa signs, compare exponents next.
        (let ((exp-x (caddr x))
              (exp-y (caddr y)))
          (cond
            ((= exp-x exp-y)
              ;; Equal exponents, compare mantissas next.
              (cond
                ((= mant-x mant-y)
                  ;; Equal mantissas, compare precisions next.
                  ;; An optional SIMP flag must be ignored.
                  (let* ((rest-x (if (eq 'simp (cadar x)) (cddar x) (cdar x)))
                         (rest-y (if (eq 'simp (cadar y)) (cddar y) (cdar y)))
                         (prec-x (car rest-x))
                         (prec-y (car rest-y)))
                    (cond
                      ((= prec-x prec-y)
                        ;; Equal precisions.
                        ;; Finally, let decimal bigfloats "win" over binary bigfloats.
                        (and (eq 'decimal (cadr rest-x))
                             (not (eq 'decimal (cadr rest-y)))))
                      (t
                        ;; Different precisions, directly compare (greater "wins").
                        (> prec-x prec-y)))))
                (t
                  ;; Different mantissas, directly compare (greater "wins").
                  (> mant-x mant-y))))
            (t
              ;; Different exponents, directly compare.
              ;; For positive mantissas, the greater exponent "wins",
              ;; for negative mantissas, the smaller exponent "wins".
              ;; The case of a zero mantissa doesn't need to be handled here,
              ;; because the bigfloat package makes sure to also set the exponent
              ;; to zero if the mantissa is zero.
              (if (plusp sgn-mant-x)
                (> exp-x exp-y)
                (< exp-x exp-y))))))
      (t
        ;; Different mantissa signs, directly compare.
        (> sgn-mant-x sgn-mant-y)))))

;;  EXPANDEXPT computes the expansion of (x1 + x2 + ... + xm)^n
;;  taking a sum and integer power as arguments.
;;  Its theory is to recurse down the binomial expansion of
;;  (x1 + (x2 + x3 + ... + xm))^n using the Binomial Expansion
;;  Thus it does a sigma:
;;
;;                n
;;             -------
;;              \         / n \    k                     (n - k)
;;               >        |   |  x1  (x2 + x3 + ... + xm)
;;              /         \ k /
;;             -------
;;               k=0
;;
;;   The function EXPONENTIATE-SUM does this and recurses through the second
;;   sum raised to a power.  It takes a list of terms and a positive integer
;;   power as arguments.


(defun expandexpt (sum power)
  (declare (fixnum power))
  (let ((expansion (exponentiate-sum (cdr sum) (abs power))))
    (cond ((plusp power) expansion)
	  (t (inv expansion)))))

(defun exponentiate-sum (terms rpower)
  (declare (fixnum rpower))
  (cond ((= rpower 0) 1)
	((null (cdr terms)) (power (car terms) rpower))
	((= rpower 1) (cons '(mplus simp) terms))
	(t (do ((i 0 (1+ i))
		(result 0 (add2 result
				(muln (list (combination rpower i)
					    (exponentiate-sum (cdr terms)
							      (- rpower i))
					    (power (car terms) i)) t))))
	       ((> i rpower) result)
	     (declare (fixnum i))))))

;;  Computes the combination of n elements taken m at a time by the formula
;;
;;     (n * (n-1) * ... * (n - m + 1)) / m! =
;;	(n / 1) * ((n - 1) / 2) * ... * ((n - m + 1) / m)
;;
;;  Checks for the case when m is greater than n/2 and translates
;;  to an equivalent expression.

(defun combination (n m)
  (declare (fixnum n m))
  (cond ((> m (truncate n 2))
	 (combination n (- n m)))
	(t
	 (do ((result 1 (truncate (* result n1) m1))
	      (n1 n (1- n1))
	      (m1 1 (1+ m1)))
	     ((> m1 m) result)
	   (declare (fixnum  n1 m1))))))

(defun expandsums (a b)
  (addn (prog (c)
	   (setq a (fixexpand a) b (cdr b))
	   loop
	   (when (null a) (return c))
	   (setq c (cons (expandterms (car a) b) c))
	   (setq a (cdr a))
	   (go loop))
	t))

(defun expandterms (a b)
  (addn (prog (c)
	 loop
	 (when (null b) (return c))
	 (setq c (cons (mul2 a (car b)) c))
	 (setq b (cdr b))
	 (go loop))
	t))

(defun expandtimes (a)
  (let (simp-prods simp-negprods simp-sums simp-negsums expsums expnegsums)
    (labels
	((genexpands (l)
	   (prog ()
	    loop
	      (setq l (cdr l))
	      (cond ((null l)
		     (setq simp-prods (nreverse simp-prods)
			   simp-negprods (nreverse simp-negprods)
			   simp-sums (nreverse simp-sums)
			   simp-negsums (nreverse simp-negsums))
		     (return nil))
		    ((atom (car l))
		     (push (car l) simp-prods))
		    ((eq (caaar l) 'rat)
		     (unless (equal (cadar l) 1)
		       (push (cadar l) simp-prods))
		     (push (caddar l) simp-negprods))
		    ((eq (caaar l) 'mplus)
		     (push (car l) simp-sums))
		    ((and (eq (caaar l) 'mexpt)
			  (equal (caddar l) -1) (mplusp (cadar l)))
		     (push (cadar l) simp-negsums))
		    ((and (eq (caaar l) 'mexpt)
			  (let ((*expandp* t))
			    (mminusp (caddar l))))
		     (push (if (equal (caddar l) -1)
			       (cadar l)
			       (list (caar l) (cadar l) (neg (caddar l))))
			   simp-negprods))
		    (t
		     (push (car l) simp-prods)))
	      (go loop))))
      (genexpands a)
      (setq simp-prods (cond ((null simp-prods) 1)
			  ((null (cdr simp-prods)) (car simp-prods))
			  (t (cons '(mtimes simp) simp-prods))))
      (setq simp-negprods (cond ((null simp-negprods) 1)
			     ((null (cdr simp-negprods)) (car simp-negprods))
			     (t (cons '(mtimes simp) simp-negprods))))
      (block nil
	(tagbody
	   (cond ((null simp-sums) (go down))
		 (t (setq expsums (car simp-sums))
		    (mapc #'(lambda (c)
			      (setq expsums (expandsums expsums c)))
			  (cdr simp-sums))))
	   (setq simp-prods (cond ((equal simp-prods 1) expsums)
			       (t (expandterms simp-prods (fixexpand expsums)))))
	 down (cond ((null simp-negsums)
		     (cond ((equal 1 simp-negprods) (return simp-prods))
			   ((mplusp simp-prods)
		            (return (expandterms (power simp-negprods -1) (cdr simp-prods))))
			   (t (return (let ((*expandflag* t))
					(mul2 simp-prods (power simp-negprods -1)))))))
		    (t
		     (setq expnegsums (car simp-negsums))
		     (mapc #'(lambda (c)
			       (setq expnegsums (expandsums expnegsums c)))
			   (cdr simp-negsums))))
	   (setq expnegsums (expandterms simp-negprods (fixexpand expnegsums)))
	   (return (if (mplusp simp-prods)
		       (expandterms (inv expnegsums) (cdr simp-prods))
		       (let ((*expandflag* t))
			 (mul2 simp-prods (inv expnegsums))))))))))

(defun expand1 (exp $expop $expon)
  (unless (and (integerp $expop) (> $expop -1))
    (merror (intl:gettext "expand: expop must be a nonnegative integer; found: ~M") $expop))
  (unless (and (integerp $expon) (> $expon -1))
    (merror (intl:gettext "expand: expon must be a nonnegative integer; found: ~M") $expon))
  (resimplify (specrepcheck exp)))

(defun fixexpand (a)
  (if (not (mplusp a))
      (ncons a)
      (cdr a)))

(defun nrthk (in nth-root)
  (labels
      ((nrthk1 (in nth-root)
	 ;; Computes the NTH-ROOT of -IN
	 (if $radexpand
	     (mul2 (nrthk2 in nth-root) (nrthk -1 nth-root))
	     (nrthk2 (mul2* -1 in) nth-root)))

       (nrthk2 (in nth-root)
	 ;; Computes the NTH-ROOT of IN
	 (power* in (list '(rat) 1 nth-root))))
    (cond ((equal in 1)
	   1)
	  ((equal in -1)
	   (cond ((equal nth-root 2)
		  '$%i)
		 ((eq $domain '$real)
		  (if (even nth-root)
		      (nrthk2 -1 nth-root)
		      -1))
		 ($m1pbranch
		  (let (($%emode t))
		    (power* '$%e (list '(mtimes) (list '(rat) 1 nth-root) '$%pi '$%i))))
		 (t
		  (nrthk2 -1 nth-root))))
	  ((or (and wflag (eq ($asksign in) '$neg))
	       (and (mnump in) (equal ($sign in) '$neg)))
	   (nrthk1 (mul2* -1 in) nth-root))
	  (t
	   (nrthk2 in nth-root)))))

(defun simpnrt (x nth-root)
  ;; Computes the NTH-ROOT of X
  (prog (simp-in simp-out varlist genvar $factorflag $dontfactor)
     (labels
	 ((simpnrt1 (x)
	    (do ((x x (cddr x)) (y))
		((null x))
	      (cond ((not (equal 1 (setq y (gcd (cadr x) nth-root))))
		     (push (simpnrt (list '(mexpt) (car x) (quotient (cadr x) y))
				    (quotient nth-root y))
			   simp-out))
		    ((and (equal (cadr x) 1) (integerp (car x)) (plusp (car x))
			  (setq y (pnthrootp (car x) nth-root)))
		     (push y simp-out))
		    (t
		     (unless (> nth-root (abs (cadr x)))
		       (push (list '(mexpt) (car x) (quotient (cadr x) nth-root)) simp-out))
		     (push (list '(mexpt) (car x) (rem (cadr x) nth-root)) simp-in))))))
       (setq $factorflag t)
       (newvar x)
       (setq x (ratrep* x))
       (when (equal (cadr x) 0) (return 0))
       (setq x (ratfact (cdr x) 'psqfr))
       (simpnrt1 (mapcar #'pdis x))
       (setq simp-out (if simp-out (muln simp-out nil) 1))
       (setq simp-in (cond (simp-in
			    (setq simp-in (muln simp-in nil))
			    (nrthk simp-in nth-root))
			   (t 1)))
       (return (let (($%emode t))
		 (simplifya (list '(mtimes) simp-in simp-out)
			    (not (or (atom simp-in)
				     (atom (cadr simp-in))
				     (member (caaadr simp-in) '(mplus mtimes rat))))))))))

(defun ratp (a ratp-var)
  (cond ((atom a) t)
	((member (caar a) '(mplus mtimes))
	 (do ((l (cdr a) (cdr l))) ((null l) t)
	   (or (ratp (car l) ratp-var) (return nil))))
	((eq (caar a) 'mexpt)
	 (if (free (cadr a) ratp-var)
	     (free (caddr a) ratp-var)
	     (and (integerp (caddr a)) (ratp (cadr a) ratp-var))))
	(t (free a ratp-var))))

(defun ratnumerator (r)
  (cond ((atom r) r)
	((atom (cdr r)) (car r))
	((numberp (cadr r)) r)
	(t (car r))))

(defun ratdenominator (r)
  (cond ((atom r) 1)
	((atom (cdr r)) (cdr r))
	((numberp (cadr r)) 1)
	(t (cdr r))))

;; (BPROG U V) appears to return A and B (as ((A1 . A2) B1 . B2) with A = A1/A2, B = B1/B2)
;; such that B/U + A/V = 1/(U*V), where U, V are polynomials represented as a list of
;; exponents and coefficients, (<gensym> E1 C1 E2 C2 ...) = C1*<gensym>^E1 + C2*<gensym>^E2 + ....
;; Example:
;; (%i73) partfrac ((2*x^2-3)/(x^4-3*x^2+2), x);
;; 1. Trace: (PARTFRAC '((#:X16910 2 2 0 -3) #:X16910 4 1 2 -3 0 2) '#:X16910)
;; 2. Trace: (BPROG '(#:X16910 2 1 0 -2) '(#:X16910 2 1 0 -1))
;; 2. Trace: BPROG ==> ((-1 . 1) 1 . 1)
;; 2. Trace: (BPROG '(#:X16910 1 1 0 1) '(#:X16910 1 1 0 -1))
;; 2. Trace: BPROG ==> ((1 . 2) -1 . 2)
;; 2. Trace: (BPROG '(#:X16910 1 1 0 -1) '1)
;; 2. Trace: BPROG ==> ((0 . 1) 1 . 1)
;; 1. Trace: PARTFRAC ==> ((0 . 1) ((1 . 2) (#:X16910 1 1 0 -1) 1) ((-1 . 2) (#:X16910 1 1 0 1) 1) ((1 . 1) (#:X16910 2 1 0 -2) 1))
;; (%o73) 1/(x^2-2)-1/(2*(x+1))+1/(2*(x-1))

(defun bprog (r s bprog-var)
  (prog (p1b p2b coef1r coef2r coef1s coef2s f1 f2 a egcd oldalg)
     (setq oldalg $algebraic)
     (setq r (ratfix r))
     (setq s (ratfix s))
     (setq coef2r (setq coef1s 0))
     (setq coef2s (setq coef1r 1))
     (setq a 1 egcd 1)
     (setq p1b (car r))
     (unless (zerop (pdegree p1b bprog-var)) (setq egcd (pgcdexpon p1b)))
     (setq p2b (car s))
     (unless (or (zerop (pdegree p2b bprog-var)) (= egcd 1))
       (setq egcd (gcd egcd (pgcdexpon p2b)))
       (setq p1b (pexpon*// p1b egcd nil)
	     p2b (pexpon*// p2b egcd nil)))
     b1   (cond ((< (pdegree p1b bprog-var) (pdegree p2b bprog-var))
		 (rotatef p1b p2b)
		 (rotatef coef1r coef2r)
		 (rotatef coef1s coef2s)))
     (when (zerop (pdegree p2b bprog-var))
       (unless (zerop (pdegree coef2r bprog-var))
	 (setq coef2r (pexpon*// coef2r egcd t)))
       (unless (zerop (pdegree coef2s bprog-var))
	 (setq coef2s (pexpon*// coef2s egcd t)))
       (return (cons (ratreduce (ptimes (cdr r) coef2r) p2b)
		     (ratreduce (ptimes (cdr s) coef2s) p2b))))
     (setq f1 (psquorem1 (cdr p1b) (cdr p2b) t))
     (setq $algebraic $false)
     (setq f2 (psimp bprog-var (cadr f1)))
     (setq p1b (pquotientchk (psimp bprog-var (caddr f1)) a))
     (setq $algebraic oldalg)
     (setq f1 (car f1))
     (setq coef1r (pquotientchk (pdifference (ptimes f1 coef1r)
					     (ptimes f2 coef2r))
				a))
     (setq coef1s (pquotientchk (pdifference (ptimes f1 coef1s)
					     (ptimes f2 coef2s))
				a))
     (setq a f1)
     (go b1)))

(defun ratdifference (a b) (ratplus a (ratminus b)))

(defun ratpl (a b) (ratplus (ratfix a) (ratfix b)))

(defun ratti (a b c) (rattimes (ratfix a) (ratfix b) c))

(defun ratqu (a b) (ratquotient (ratfix a) (ratfix b)))

(defun ratfix (a) (cond ((equal a (ratnumerator a)) (cons a 1)) (t a)))

(defun ratdivide (f g)
  (destructuring-let* (((fnum . fden) (ratfix f))
		       ((gnum . gden) (ratfix g))
		       ((q r) (pdivide fnum gnum)))
    (cons (ratqu (ratti q gden t) fden)
	  (ratqu r fden))))

(defun polcoef (l n polcoef-var)
  (cond ((or (atom l) (pointergp polcoef-var (car l)))
	 (cond ((equal n 0) l)
	       (t 0)))
	(t
	 (ptterm (cdr l) n))))

(defun disrep (l disrep-ratform)
  (cond ((equal (ratnumerator l) l)
	 ($ratdisrep (cons disrep-ratform (cons l 1))))
	(t ($ratdisrep (cons disrep-ratform l)))))

(defun simpargs1 (a vestigial c)
  (declare (ignore vestigial))
  (simpargs a c))

(defquote retlist (&rest l)
  (cons '(mlist simp)
	(mapcar #'(lambda (z) (list '(mequal simp) z (meval z))) l)))

(defun coeff (e var pow)
  (simplify
   (cond ((alike1 e var) (if (equal pow 1) 1 0))
	 ((atom e) (if (equal pow 0) e 0))
	 ((eq (caar e) 'mexpt)
	  (cond ((alike1 (cadr e) var)
		 (if (or (equal pow 0) (not (alike1 (caddr e) pow))) 0 1))
		((equal pow 0) e)
		(t 0)))
	 ((or (eq (caar e) 'mplus) (mbagp e))
	  (cons (if (eq (caar e) 'mplus) '(mplus) (car e))
		(mapcar #'(lambda (e) (coeff e var pow)) (cdr e))))
	 ((eq (caar e) 'mrat) (ratcoeff e var pow))
         ((equal pow 0) (if (coeff-contains-powers e var) 0 e))
	 ((eq (caar e) 'mtimes)
	  (let ((term (if (equal pow 1) var (power var pow))))
	    (if (memalike term (cdr e)) ($delete term e 1) 0)))
	 (t 0))))
