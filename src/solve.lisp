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

(macsyma-module solve)

(load-macsyma-macros ratmac strmac)

(declare-top (special expsumsplit *g
		      equations ;List of E-labels
		      *power *varb *flg
		      broken-not-freeof
		      mult    ;Some crock which tracks multiplicities.
		      *roots ;alternating list of solutions and multiplicities
		      *failures	;alternating list of equations and multiplicities
		      *myvar
		      *has*var *var
		      xm* xn* mul*))

(defmvar $linsolvewarn t
  "Needs to be documented.")

(defmvar $solvedecomposes t
  "Causes `solve' to use `polydecomp' in attempting to solve polynomials.")

(defmvar $solveexplicit nil
  "Causes `solve' to return implicit solutions i.e. of the form F(x)=0.")

(defmvar $solvenullwarn t
  "Causes the user will be warned if SOLVE is called with either a
	 null equation list or a null variable list.  For example,
	 SOLVE([],[]); would print two warning messages and return [].")

(defmvar $solve_reject_undefined t
  "Whether SOLVE rejects a solution at which an equation is undefined.
TRUE substitutes the solution and rejects it where an equation cannot be
evaluated, FALSE returns whatever was found, and LIMIT keeps a solution
at which an equation merely has a removable singularity, at the cost of
a limit computation that may ask questions.  Whether the equation comes
out zero is not asked: SOLVE has already made a numerator vanish."
  :setting-predicate #'(lambda (x)
			 (values (member x '(t nil $limit))
				 "must be true, false or limit")))

;; Utility macros

;; This macro returns the number of trivial equations.  It counts up the
;; number of zeros in a list.

;(defmacro nzlist (llist)
;  `(do ((l ,llist (cdr l))
;	(zcount 0))
;       ((null l) zcount)
;     (if (and (integerp (car l)) (zerop (car l)))
;	 (incf zcount))))

;; This is only called on a variable.

(defmacro allroot (exp)
  `(setq *failures (list* (make-mequal-simp ,exp ,exp) 1 *failures)))

;; Finds variables, changes equations into expressions without MEQUAL.
;; Checks for consistency between the number of unknowns and equations.
;; Calls SOLVEX for simultaneous equations and SSOLVE for a single equation.

(defmfun $solve (*eql &optional (varl nil varl-p))
  (setq $multiplicities (make-mlist))
  (prog (eql                            ; Equations to solve
         $keepfloat $ratfac             ; In case user has set these
         *roots                         ; *roots gets solutions,
         *failures                      ; *failures "roots of"
         broken-not-freeof) ;Has something to do with splitting up roots
     
     ;; Create the equation list (this is a lisp list, not 'MLIST)
     (setq eql
           (cond
             ;; If an atom, cons it.
             ((atom *eql) (ncons *eql))
             ;; If we have a list of equations, move everything over
             ;; to one side, so x=5 -> x-5=0.
             ((eq (g-rep-operator *eql) 'mlist)
              (mapcar 'meqhk (cdr *eql)))
             ;; We can't solve inequalities
             ((member (g-rep-operator *eql)
                      '(mnotequal mgreaterp mlessp mgeqp mleqp) :test #'eq)
              (merror (intl:gettext "solve: cannot solve inequalities.")))
             ;; Finally, assume we have just one equation, and put it
             ;; on one side again.
             (t (ncons (meqhk *eql)))))

     (cond
       ;; If the variable list wasn't supplied we have to supply it
       ;; ourselves. Also remove constants like $%pi from the list.
       ((null varl-p)
        (setq varl
              (let (($listconstvars nil))
                (cdr ($listofvars *eql))))
        (if varl (setq varl (remc varl)))) ; Remove all constants

       ;; If VARL is a list of variables, filter out duplicates.
       ;; Otherwise, just cons it.
       (t
        (setq varl
              (cond (($listp varl) (remove-duplicates
                                    (cdr varl) :test #'equal))
                    (t (list varl))))))

     ;; Some sanity checks and warning messages.
     (when (and (null varl) $solvenullwarn)
       (mtell (intl:gettext "~&solve: variable list is empty, continuing anyway.~%")))

     (when (and (null eql) $solvenullwarn)
       (mtell (intl:gettext "~&solve: equation list is empty, continuing anyway.~%")))

     (when (some #'mnump varl)
       (merror (intl:gettext "solve: all variables must not be numbers.")))
     
     ;; Deal with special cases.
     (cond
       ;; Trivially true equations for any set of variables.
       ((equal eql '(0))
        (return '$all))

       ;; Trivially false equations: return []
       ((or (null varl) (null eql))
        (return (make-mlist-simp)))

       ;; One equation in one variable: SSOLVE
       ((and (null (cdr varl)) (null (cdr eql)))
        (return (ssolve (car eql) (car varl))))

       ;; We were given a variable list, or there are same # of eqns
       ;; as unknowns: SOLVEX.
       ((or varl-p
            (= (length varl) (length eql)))
        (let ((eqns eql))
          (setq eql (solvex eql varl (not $programmode) t))
          (return
            (solve-drop-bad-solutions
             (cond ((and (cdr eql)
                         (not ($listp (cadr eql))))
                    (make-mlist eql))
                   (t eql))
             eqns varl)))))

     ;; We don't know what to do, so complain. The let sets u to varl
     ;; but as an MLIST list and e to the original eqns coerced to a
     ;; list.
     (let ((u (make-mlist-l varl))
           (e (cond (($listp *eql) *eql)
                    (t (make-mlist *eql)))))
       ;; MFORMAT doesn't have ~:[~] yet, so I just change this to
       ;; make one of two possible calls to MERROR. Smaller codesize
       ;; then what was here before anyway.
       (if (> (length varl) (length eql))
	   (merror
      (intl:gettext "solve: more unknowns than equations.~
		  ~%Unknowns given :  ~%~M~
		  ~%Equations given:  ~%~M")
      u e)
	   (merror
      (intl:gettext "solve: more equations than unknowns.~
		  ~%Unknowns given :  ~%~M~
		  ~%Equations given:  ~%~M")
      u e)))))


;; Removes anything from its list arg which solve considers not to be a
;; variable, i.e.  constants, functions or subscripted variables without
;; numeric args.

(defun remc (lst)
  (do ((l lst (cdr l)) (fl) (vl)) ((null l) vl)
    (cond ((atom (setq fl (car l)))
	   (unless (maxima-constantp fl) (push fl vl)))
	  ((every #'$constantp (cdr fl)) (push fl vl)))))

;;; Verifying a solution.  SOLVE works on numerators, so a denominator
;;; cleared along the way leaves its poles among the roots: the zeros
;;; of 1-COS(X) include the zero of the SIN(X) divided out of
;;; CSC(X)-COT(X), and the rational package cannot cancel the two,
;;; knowing nothing of SIN^2+COS^2 = 1.  $SOLVE_REJECT_UNDEFINED says
;;; what to do about that.

;; SOL, one solution as a list of equations, as the (VAR . VALUE) pairs
;; to substitute, or NIL when it is not of a shape we can substitute:
;; every equation has to give one of VARS a value free of all of them.
(defun solve-solution-pairs (sol vars)
  (let ((pairs nil))
    (dolist (e sol pairs)
      (unless (and (not (atom e))
		   (not (atom (car e)))
		   (eq (caar e) 'mequal)
		   (member (cadr e) vars :test #'alike1)
		   (every #'(lambda (v) (free (caddr e) v)) vars))
	(return nil))
      (push (cons (cadr e) (caddr e)) pairs))))

;; E with every (VAR . VALUE) of PAIRS substituted, or T when a
;; substitution signals, which is what NO-ERR-SUB-VAR answers.
(defun solve-subst-pairs (e pairs)
  (dolist (p pairs e)
    (setq e (no-err-sub-var (cdr p) e (car p)))
    (when (eq e t) (return t))))

;; True when the limit L came back as a definite finite value, rather
;; than as UND, IND or an infinity, so that the singularity it was
;; taken at is removable.
(defun solve-finite-limit-p (l)
  (and (not (member l '($und $ind $inf $minf $infinity)))
       (free l '$und) (free l '$ind)
       (free l '$inf) (free l '$minf) (free l '$infinity)))

;; True when the equation E is defined at PAIRS.  It is when it can be
;; evaluated there at all: SOLVE has already made a numerator vanish,
;; so an equation that survives substitution holds.
;;
;; Under LIMIT a removable singularity counts as well, and there the
;; limit has to be zero, not merely finite.  That is not a different
;; standard, it is the same one: substitution answers "is it defined"
;; and "does it hold" together, while a limit answers only the first.
;; SIN(X)/X extends continuously to 1 at 0, so the singularity is
;; removable and the extension is still not a root; asking only that
;; the limit exist puts X = 0 back into solve(sin(x)/x = 0, x).
;;
;; A limit needs E to depend on one unknown only, Maxima having none
;; in several variables and an iterated one not being the same thing.
;; That is asked of the equation rather than of the solution, so the
;; equations of an independent system are each taken on their own; the
;; unknowns E does not contain are substituted first.  An equation in
;; two of them falls back to what TRUE does.
(defun solve-defined-p (e pairs)
  (or (not (eq t (solve-subst-pairs e pairs)))
      (and (eq $solve_reject_undefined '$limit)
	   (let ((in (remove-if #'(lambda (p) (free e (car p))) pairs)))
	     (and in (null (cdr in))
		  (let ((e1 (solve-subst-pairs e (remove (car in) pairs))))
		    (and (not (eq e1 t))
			 (let ((l (errcatch (mfuncall '$limit e1 (caar in) (cdar in)))))
			   (and l (solve-finite-limit-p (car l))
				(zerop1 (car l)))))))))))

(defun solve-solution-defined-p (pairs eqns)
  (dolist (e eqns t)
    (unless (solve-defined-p e pairs)
      (return nil))))

;; Drop from *ROOTS the roots of EXP that do not verify, keeping each
;; root with its multiplicity.
(defun solve-drop-bad-roots (exp var)
  (when $solve_reject_undefined
    (setq *roots
	  (loop for (r m) on *roots by #'cddr
		for pairs = (solve-solution-pairs (list r) (list var))
		when (or (null pairs)
			 (solve-solution-defined-p pairs (list exp)))
		  append (list r m)))))

;; The same for the solution sets $SOLVE returns for a list of
;; equations.  This filters what $SOLVE returns rather than what SOLVEX
;; does, DEFINT calling SOLVEX directly and wanting every candidate it
;; can get.
(defun solve-drop-bad-solutions (sols eqns vars)
  (if (and $solve_reject_undefined ($listp sols))
      (make-mlist-l
       (remove-if #'(lambda (sol)
		      (and ($listp sol)
			   (let ((pairs (solve-solution-pairs (cdr sol) vars)))
			     (and pairs
				  (not (solve-solution-defined-p pairs eqns))))))
		  (cdr sols)))
      sols))

;; Solve a single equation for a single unknown.
;; Obtains roots via solve and prints them.

(defun ssolve (exp *var)
  (let (($solvetrigwarn $solvetrigwarn)
	equations multi)
    (cond ((null *var) '$all)
	  (t (solve exp *var 1)
	     (solve-drop-bad-roots exp *var)
	     (cond ((not (or *roots *failures)) (make-mlist))
		   ($programmode
		    (prog1
			(make-mlist-l (nreverse (map2c #'(lambda (eqn mult) (push mult multi) eqn)
						       (if $solveexplicit
							   *roots
							   (nconc *roots *failures)))))
		      (setq $multiplicities (make-mlist-l (nreverse multi)))))
		   (t
		      (when (and *failures (not $solveexplicit))
			(when $dispflag (mtell (intl:gettext "solve: the roots of:~%")))
			(setq equations (nth-value 1
			  (solve2 *failures equations))))
		      (when *roots
			(when $dispflag (mtell (intl:gettext "solve: solution:~%")))
			(setq equations (nth-value 1
			  (solve2 *roots equations))))
		      (make-mlist-l equations)))))))

;; Solve takes three arguments, the expression to solve for zero, the variable
;; to solve for, and what multiplicity this solution is assumed to have (from
;; higher-level Solve's).  Solve returns NIL.  Isn't that useful?  The lists
;; *roots and *failures are special variables to which Solve prepends solutions
;; and their multiplicities in that order: *roots contains explicit solutions
;; of the form <var>=<function of independent variables>, and *failures
;; contains equations which if solved would yield additional solutions.

;; Factors expression and reduces exponents by their gcd (via solventhp)

(defun solve (*exp *var mult &aux (genvar nil) ($derivsubst nil)
		     (exp (float2rat (mratcheck *exp)))
		     (*myvar *var) ($savefactors t))
  (prog (factors *has*var genpairs $dontfactor temp symbol *g *checkfactors* 
	 varlist expsumsplit)
     (let (($ratfac t))
       (setq exp (ratdisrep (ratf exp))))
     ;; Cancel out any simple 
     ;; (non-algebraic) common factors in numerator and 
     ;; denominator without altering the structure of the 
     ;; expression too much.
     ;; Also, RJFPROB in TEST;SOLVE TEST is now solved.
     ;; - JPG
     a (cond ((atom exp)
	      (cond ((eq exp *var)
		     (solve3 0 mult))
		    ((equal exp 0) (allroot *var))
		    (t nil)))
	     (t (setq exp (meqhk exp))
		(cond ((equal exp '(0))
		       (return (allroot *var)))
		      ((free exp *var)
		       (return nil)))
		(cond ((not (atom *var))
		       (setq symbol (gensym))
		       (setq exp (maxima-substitute symbol *var exp))
		       (setq temp *var)
		       (setq *var symbol)
		       (setq *myvar *var))) ;keep *MYVAR up-to-date
	      
		(cond ($solveradcan (setq exp (radcan1 exp *var))
				    (if (atom exp) (go a))))
	      
		(cond ((easy-cases exp *var mult)
		       (cond (symbol (setq *roots (subst temp *var *roots))
				     (setq *failures (subst temp *var *failures))))
		       (rootsort *roots)
		       (rootsort *failures)
		       (return nil)))
	      
		(cond ((setq factors (first-order-p exp *var))
		       (solve3 (ratdisrep
				(ratf (make-mtimes -1 (div* (cdr factors)
							    (car factors)))))
			       mult))
		    
		      (t (setq varlist (list *var))
			 (fnewvar exp)
			 (setq varlist (varsort varlist))
			 (let ((vartemp)
			       (ratnumer (mrat-numer (ratrep* exp)))
			       (numer-varlist varlist)
			       (subst-list (trig-subst-p varlist)))
			   (setq varlist (ncons *var))
			   (cond (subst-list
				  (setq exp (trig-subst exp subst-list))
				  (fnewvar exp)
				  (setq varlist (varsort varlist))
				  (setq exp (mrat-numer (ratrep* exp)))
				  (setq vartemp varlist))
				 (t (setq vartemp numer-varlist)
				    (setq exp ratnumer)))
			   (setq varlist vartemp))
		       
			 (cond ((atom exp) (go a))
			       ((of-form-A*F<X>^N+B exp) (solve1a exp mult))
			       ((and (not (pcoefp exp))
				     (cddr exp)
				     (not (equal 1 (setq *g (solventhp (cdddr exp) (cadr exp))))))
				(solventh exp *g))
			       (t (cond ($solvefactors 
					 (map2c (lambda (x y) (solve1a x (m* mult y)))
						(pfactor exp)))
					(t (solve1a exp mult)))))))))

     (cond (symbol (setq *roots (subst temp *var *roots))
		   (setq *failures (subst temp *var *failures))))
     (rootsort *roots)
     (rootsort *failures)
     (return nil)))

(defun float2rat (exp)
  (cond ((floatp exp) (setq exp (prep1 exp)) (make-rat-simp (car exp) (cdr exp)))
	((or (atom exp) (specrepp exp)) exp)
	(t (recur-apply #'float2rat exp))))

;;; The following takes care of cases where the expression is already in 
;;; factored form. This can introduce spurious roots if one of the factors
;;; is an expression that can be undefined or infinity for certain values of
;;; the variable in question. But soon this will be no worry because I will
;;; add a list of  "possible bad roots" to what $SOLVE returns.
;;; Passes multiplicity to recursive calls to solve.

(defun easy-cases (*exp *var mult)
  (cond ((or (atom *exp) (atom (car *exp))) nil)
	((eq (caar *exp) 'mtimes)
	 (do ((terms (cdr *exp) (cdr terms)))
	     ((null terms))
	   (solve (car terms) *var mult))
	 'mtimes)

	((member (caar *exp) '(mabs %signum))		;; abs(x) = 0, signum(x) = 0  <=>  x = 0
	 (solve (cadr *exp) *var mult)
	 (caar *exp))

	((eq (caar *exp) 'mexpt)
	 (cond ((and (freeof *var (cadr *exp))
		     (not (zerop1 (cadr *exp))))
		;; no solutions: c^x is never zero
		'mexpt)

	       ((and (integerp  (caddr *exp))
		     (plusp (caddr *exp)))
		(solve (cadr *exp) *var (m* mult (caddr *exp)))
		'mexprat)))))

;;; Predicate to test for presence of troublesome trig functions to be
;;; canonicalized.  A  table of when to make substitutions should
;;; be used here. 
;;;  trig kind                     => SIN | COS | TAN ...   subst to make
;;; number around in expression ->     1     1     0         ......
;;; what you want to be able to do for example is to see if SIN and COS^2 
;;; are around and then make a reasonable substitution.

(defun trig-subst-p (vlist)
  ;; Only the kernels containing *VAR decide whether to canonicalize:
  ;; a trigonometric function of a parameter is not a second kind to be
  ;; reconciled with, and counting it turned TAN(*VAR) into
  ;; SIN(*VAR)/COS(*VAR), which USOLVE can no longer invert.
  (and (not (trig-not-subst-p
	     (remove-if #'(lambda (v) (free v *var)) vlist)))
       (do ((all vlist)
	    (var (car vlist) (car vlist))
	    (vlist (cdr vlist) (cdr vlist))
	    (subst-list))
	   ((null var) subst-list)
	 (cond ((and (not (atom var))
		     (trig-cannon (g-rep-operator var))
		     (not (free var *var))
		     (not (half-angle-kernel-p var all)))
		(push var subst-list))))))

;; Predicate to see when obviously not to substitute for trigs.
;; A hack in the direction of expression properties-table driven
;; substitution. The "measure" of the expression is the total number
;; of different kinds of trig functions in the expression.

(defun trig-not-subst-p (vlist)
  (let ((trigs '(%sin %cos %tan %cot %csc %sec)))
    (< (measure #'sign-gjc (operator-frequency-table vlist trigs) trigs)
       2)))

;; To get the total "value" of things in a table, this case an assoc list.
;; (MEASURE FUNCTION ASSOCIATION-LIST SET) where FUNCTION is a function mapping
;; the range of the ASSOCIATION-LIST viewed as a function on the SET, to the
;; integers.

(defun measure (f alist set &aux (sum 0))
  (dolist (element set)
    (incf sum (funcall f (cdr (assoc element alist :test #'eq)))))
  sum)

;; Named for uniqueness only

(defun sign-gjc (x)
  (cond ((or (null x) (= x 0)) 0)
	((< 0 x) 1)
	(t -1)))

;; A function that can EXTEND a function
;; over two association lists. Note that I have been using association lists
;; as mere functions (that is, as sets of ordered pairs).
;; (EXTEND '+ L1 L2 S) could also be to take the union of two multi-sets in the
;; sample space S. (what the '&%%#?& has this got to do with SOLVE?) 

(defun extend (f l1 l2 s)
  (do ((j 0 (1+ j))
       (value nil))
      ((= j (length s)) value)
    (setq value (cons (cons (nth j s)
			    (funcall f (cdr (assoc (nth j s) l1 :test #'equal))
				     (cdr (assoc (nth j s) l2 :test #'equal))))
		      value))))

;; For the case where the value of assoc is NIL, we will need a special "+"

(defun +mset (a b)
  (+ (or a 0) (or b 0)))

;; To recursively looks through a list
;; structure (the VLIST) for members of the SET appearing in the MACSYMA 
;; functional position (caar list). Returning an assoc. list of appearance
;; frequencies. Notice the use of EXTEND.

(defun operator-frequency-table (vlist set)
  (do ((j 0 (1+ j))
       (it)
       (assl (do ((k 0 (1+ k))
		  (made nil))
		 ((= k (length set)) made)
	       (setq made (cons (cons (nth k set) 0)
				made)))))
      ((= j (length vlist)) assl)
    (setq it (nth j vlist))
    (cond ((atom it))
	  (t (setq assl (extend #'+mset (cons (cons (caar it) 1) nil)
				assl set))
	     (setq assl (extend #'+mset assl
				(operator-frequency-table (cdr it) set)
				set))))))

(defun trig-subst (exp sub-list)
  (do ((exp exp)
       (sub-list (cdr sub-list) (cdr sub-list))
       (var (car sub-list) (car sub-list)))
      ((null var) exp)
    (setq exp
	  (maxima-substitute (funcall (trig-cannon (g-rep-operator var))
				      (make-mlist-l (g-rep-operands var)))
			     var exp))))

;; Here are the canonical trig substitutions.

(defun-prop (%sec trig-cannon) (x)
  (inv* (make-g-rep '%cos (g-rep-first-operand x))))

(defun-prop (%csc trig-cannon) (x)
  (inv* (make-g-rep '%sin (g-rep-first-operand x))))

(defun-prop (%tan trig-cannon) (x)
  (div* (make-g-rep '%sin (g-rep-first-operand x))
	(make-g-rep '%cos (g-rep-first-operand x))))

(defun-prop (%cot trig-cannon) (x)
  (div* (make-g-rep '%cos (g-rep-first-operand x))
	(make-g-rep '%sin (g-rep-first-operand x))))

(defun-prop (%sech trig-cannon) (x)
  (inv* (make-g-rep '%cosh (g-rep-first-operand x))))

(defun-prop (%csch trig-cannon) (x)
  (inv* (make-g-rep '%sinh (g-rep-first-operand x))))

(defun-prop (%tanh trig-cannon) (x)
  (div* (make-g-rep '%sinh (g-rep-first-operand x))
	(make-g-rep '%cosh (g-rep-first-operand x))))

(defun-prop (%coth trig-cannon) (x)
  (div* (make-g-rep '%cosh (g-rep-first-operand x))
	(make-g-rep '%sinh (g-rep-first-operand x))))

;;; Equations in several trigonometric or hyperbolic functions of one
;;; argument U.  No single inverse function solves those, and SOLVE
;;; gives up on them with one kernel isolated in terms of another.  In
;;; TAN(U/2), or TANH(U/2), they are rational in one kernel, which the
;;; ordinary machinery does solve.

;; The functions TAN(U/2) makes rational, and after them those TANH(U/2)
;; does.
(defvar *half-angle-ops* '(%sin %cos %tan %cot %sec %csc
			  %sinh %cosh %tanh %coth %sech %csch))

;; 1 for a circular kernel and -1 for a hyperbolic one.  The images of
;; the two differ only in the sign of TH^2, so one set of formulas
;; serves both; a mixture of them is rational in neither TAN(U/2) nor
;; TANH(U/2) and is left alone.
(defun half-angle-sign (k)
  (if (member (caar k) '(%sinh %cosh %tanh %coth %sech %csch)) -1 1))

;; True while SOLVE-HALF-ANGLE is solving what it rewrote.  The rewrite
;; leaves one kernel, so it never needs itself, and a second round would
;; halve the argument and double the degree again.
(defvar *half-angle-p* nil)

;; True when VAR is TAN or COT of half the argument of another kernel
;; in VLIST.  SOLVE-HALF-ANGLE wants such a kernel exactly as it
;; stands, so TRIG-SUBST-P leaves it alone rather than canonicalizing
;; it into SIN and COS of the half argument.
(defun half-angle-kernel-p (var vlist)
  (and (member (g-rep-operator var) '(%tan %cot))
       (let ((u (mul 2 (g-rep-first-operand var))))
	 (dolist (v vlist)
	   (when (and (not (atom v))
		      (member (g-rep-operator v) *half-angle-ops*)
		      (alike1 u (g-rep-first-operand v)))
	     (return t))))))

(defun half-angle-half-p (k)
  (member (caar k) '(%tan %cot %tanh %coth)))

(defun half-angle-fits-p (k u)
  (or (alike1 (cadr k) u)
      (and (half-angle-half-p k) (alike1 (mul 2 (cadr k)) u))))

;; The argument U that the kernels of *VAR in EXP share, and those
;; kernels, or NIL.  TAN and COT may have U/2 instead of U, *VAR may
;; occur nowhere but in them, and there must be two of them or more:
;; one alone is what USOLVE's inverse functions already solve, and in a
;; better form.  Only a rational function of the kernels becomes one of
;; TH, so nothing but sums, products and integer powers of them may
;; stand in between: SQRT(COS(U)) and LOG(SIN(U)) do not, and rewriting
;; those yields an equation with TH on both sides.
(defun half-angle-arg (exp)
  (let ((kernels nil) (ok t))
    (labels ((walk (e)
	       (cond ((free e *var))
		     ((or (atom e) (atom (car e))) (setq ok nil))
		     ((member (caar e) *half-angle-ops*)
		      (pushnew e kernels :test #'alike1))
		     ((member (caar e) '(mplus mtimes)) (mapc #'walk (cdr e)))
		     ((and (eq (caar e) 'mexpt) (integerp (caddr e)))
		      (walk (cadr e)))
		     (t (setq ok nil)))))
      (walk exp))
    (when (and ok (cdr kernels)
	       (let ((e (half-angle-sign (car kernels))))
	         (every #'(lambda (k) (eql e (half-angle-sign k)))
	                kernels)))
      ;; A TAN or COT kernel admits its own argument and twice it; the
      ;; plain arguments come first, for the finer period.
      (dolist (u (append (mapcar #'cadr kernels)
			 (mapcar #'(lambda (k) (mul 2 (cadr k)))
				 (remove-if-not #'half-angle-half-p kernels))))
	(when (every #'(lambda (k) (half-angle-fits-p k u)) kernels)
	  (return (values u kernels)))))))

;; The kernel K, whose argument is U or, for the tangents and
;; cotangents, U/2, written in TH = TAN(U/2) or TANH(U/2).
(defun half-angle-image (k u th)
  (let* ((e (half-angle-sign k))
	 (q (add 1 (mul e (power th 2))))
	 (sn (div (mul 2 th) q))
	 (cs (div (sub 1 (mul e (power th 2))) q)))
    (if (alike1 (cadr k) u)
	(case (caar k)
	  ((%sin %sinh) sn)
	  ((%cos %cosh) cs)
	  ((%tan %tanh) (div sn cs))
	  ((%cot %coth) (div cs sn))
	  ((%sec %sech) (inv cs))
	  ((%csc %csch) (inv sn)))
	(if (member (caar k) '(%tan %tanh)) th (inv th)))))

;; MAXIMA-SUBSTITUTE is no use here: it matches by object identity, and
;; rewriting one kernel rebuilds the expression around the others.
(defun half-angle-subst (exp u kernels th)
  (let ((k (find-if #'(lambda (k) (alike1 k exp)) kernels)))
    (cond (k (half-angle-image k u th))
	  ((atom exp) exp)
	  (t (recur-apply #'(lambda (e) (half-angle-subst e u kernels th))
				  exp)))))

;; Solve EXP = 0 for *VAR, EXP being built from the functions of U in
;; KERNELS.  In TH = TAN(U/2), or TANH(U/2), it is a rational function
;; of the one kernel TH, and SOLVE inverts that with ATAN or ATANH, so
;; the solutions lost to two kernels come back -- one per period, as
;; ever.  U = %pi, or %i*%pi, is not among them, TH being infinite
;; there: it is a root exactly when the numerator's degree in TH falls
;; short of the denominator's, and by as much.  Returns NIL when
;; nothing comes of this method, leaving the caller its own.
(defun solve-half-angle (exp u kernels mult)
  ;; TH stands in for TAN(U/2) while the degrees are taken, $HIPOW
  ;; wanting a symbol rather than a kernel, and goes back into the
  ;; numerator before SOLVE sees it.  Q is what the images have
  ;; underneath, and TH never takes a value where it vanishes -- U
  ;; would have to be infinite -- so a factor of it in the numerator is
  ;; no root of EXP.  The degrees are taken before it goes.
  ;;
  ;; $ALGEBRAIC because a rewritten equation whose coefficients are
  ;; algebraic numbers keeps a factor of Q in both parts otherwise,
  ;; which is a wrong degree gap and roots at an infinite U.
  (let* (($algebraic t)
	 (e (half-angle-sign (car kernels)))
	 (th (gensym "HALF-ANGLE-"))
	 (rat (sratsimp (half-angle-subst exp u kernels th)))
	 (num (ratdisrep ($ratnumer rat)))
	 (k (- ($hipow (ratdisrep ($ratdenom rat)) th) ($hipow num th)))
	 (q (add 1 (mul e (power th 2))))
	 (n (maxima-substitute
	     (ftake (if (minusp e) '%tanh '%tan) (div u 2)) th
	     (sratsimp (div num ($gcd num q))))))
    (multiple-value-bind (roots failures)
	(let ((*roots nil) (*failures nil) (*half-angle-p* t))
	  (solve n *var mult)
	  (when (plusp k)
	    (solve (sub u (if (minusp e) (mul '$%i '$%pi) '$%pi))
		   *var (mul k mult)))
	  (values *roots *failures))
      (when (or roots failures)
	(setq *roots (append roots *roots))
	(setq *failures (append failures *failures))
	t))))

;; Predicate to replace ISLINEAR....Returns NIL if not of for A*X+B, A and B
;; freeof X, else returns (A . B)

(defun first-order-p (exp var &aux temp)
  ;; Expand the expression at one level, i.e. distribute products
  ;; over sums, but leave exponentiations alone.
  ;; (X+1)^2*(X+Y) --> X*(X+1)^2 + Y*(X+1)^2
  (setq exp (expand1 exp 1 1))
  (cond ((atom exp) nil)
	(t (case (g-rep-operator exp)
	     (mtimes
	      (cond ((setq temp (linear-term-p exp var))
		     (make-lineq temp 0))
		    (t nil)))
	     (mplus
	      (do ((arg  (car (g-rep-operands exp)) (car rest))
		   (rest (cdr (g-rep-operands exp)) (cdr rest))
		   (linear-term-list)
		   (constant-term-list)
		   (temp))
		  ((null arg)
		   (if linear-term-list
		       (make-lineq (make-mplus-l linear-term-list)
				   (if constant-term-list
				       (make-mplus-l constant-term-list)
				       0))))
		(cond ((setq temp (linear-term-p arg var))
		       (push temp linear-term-list))
		      ((broken-freeof var arg)
		       (push arg constant-term-list))
		      (t (return nil)))))
	     (t nil)))))

;; Function to test if a term from an expanded expression is a linear term
;; check and see that exactly one item in the product is the main var and
;; all others are free of the main var.  Returns NIL or a G-REP expression.

(defun linear-term-p (exp var)
  (cond ((atom exp)
	 (cond ((eq exp var) 1)
	       (t nil)))
	(t (case (g-rep-operator exp)
	     (mtimes
	      (do ((factor (car (g-rep-operands exp)) ;individual factors
			   (car rest))
		   (rest (cdr (g-rep-operands exp)) ;factors yet to be done
			 (cdr rest))
		   (main-var-p)	     ;nt -> main-var seen at top level
		   (list-of-factors))	;accumulate our factors
		  ((null factor)	;for all factors
		   (and main-var-p
					;no-main-var at top level -=> not linear
			(make-mtimes-l list-of-factors)))
		(cond ((eq factor var)  ;if it's our main var
					;note it...it has to be there to be a linear term
		       (setq main-var-p t))
		      ((broken-freeof var factor) ;if 
		       (push factor list-of-factors))
		      (t (return nil)))))
	     (t nil)))))


;;; DISPATCHING FUNCTION ON DEGREE OF EXPRESSION
;;; This is a crock of shit, it should be data driven and be able to
;;; dispatch to all manner of special cases that are in a table.
;;; EXP here is a polynomial in MRAT form.  All of this well-structured,
;;; intelligently-designed code works by side effect.  SOLVECUBIC
;;; takes something that looks like (G0003 3 4 1 1 0 10) as an argument
;;; and returns something like ((MEQUAL) $X ((MTIMES) ...)).  You figure
;;; out where the $X comes from.

;;; It comes from GENVARS/VARLIST, of course.  Isn't this wonderful rational
;;; function package irrational?  If you don't know about GENVARS and
;;; VARLIST, you'd better bite the bullet and learn...everything depends
;;; on them.  The canonical example of mis-use of special variables!
;;; --RWK

(defun solve1a (exp mult) 
  (let ((*myvar *myvar)
	(*g nil)
	(gexp nil) (u nil) (kernels nil)) 
    (cond ((atom exp) nil)
          ((not (memalike (setq *myvar (simplify (pdis (list (car exp) 1 1))))
                          *has*var))
           nil)
	  ;; Several trigonometric or hyperbolic kernels of one argument:
	  ;; rewriting them in the tangent of the half argument leaves one.
	  ((and (not *half-angle-p*)
		(cdr *has*var)
		(setq gexp (pdis exp))
		(multiple-value-setq (u kernels) (half-angle-arg gexp))
		(solve-half-angle gexp u kernels mult)))
	  ((equal (cadr exp) 1) (solvelin exp))
	  ((of-form-A*F<X>^N+B exp) (solve-A*F<X>^N+B exp t))
	  ((equal (cadr exp) 2) (solvequad exp))
	  ((not (equal 1 (setq *g (solventhp (cdddr exp) (cadr exp)))))
	   (solventh exp *g))
	  ((equal (cadr exp) 3) (solvecubic exp))
	  ((equal (cadr exp) 4) (solvequartic exp))
	  (t (let ((tt (solve-by-decomposition exp *myvar)))
	       (setq *failures (append (solution-losses tt) *failures))
	       (setq *roots    (append (solution-wins tt) *roots)))))))

(defun solve-simplist (list-of-things)
  (g-rep-operands (simplifya (make-mlist-l list-of-things) nil)))

;; The Solve-by-decomposition program returns the cons of (ROOTS . FAILURES).
;; It returns a "Solution" object, that is, a CONS with the CAR being the
;; failures and the CDR being the successes.
;; It takes a POLY as an argument and returns a SOLUTION.

(defun solve-by-decomposition (poly *$var)
  (let ((decomp))
    (cond ((or (not $solvedecomposes)
	       (= (length (setq decomp (polydecomp poly (poly-var poly)))) 1))
	   (make-solution nil `(,(make-mequal 0 (pdis poly)) 1)))
	  (t (decomp-trace (make-mequal 0 (rdis (car decomp)))
			   decomp
			   (poly-var poly) *$var 1)))))

;; DECOMP-TRACE is the recursive function which maps itself down the
;; intermediate solutions until the end is reached.  If it encounters
;; non-solvable equations it stops.  It returns a SOLUTION object, that is, a
;; CONS with the CAR being the failures and the CDR being the successes.

(defun decomp-trace (eqn decomp var *$var mult &aux sol chain-sol wins losses)
  (setq sol (if decomp
		(re-solve eqn *$var mult)
		(make-solution `(,eqn 1) nil)))
  (cond ((solution-losses sol) sol)
	;; End test
	((null decomp) sol)
	(t (do ((l (solution-wins sol) (cddr l)))
	       ((null l))
	     (setq chain-sol
		   (decomp-chain (car l) (cdr decomp) var *$var (cadr l)))
	     (setq wins (nconc wins (copy-list (solution-wins chain-sol))))
	     (setq losses (nconc losses (copy-list (solution-losses chain-sol)))))
	   (make-solution wins losses))))

;; Decomp-chain is the function which formats the mess for the recursive call.
;; It returns a "Solution" object, that is, a CONS with the CAR being the
;; failures and the CDR being the successes.

(defun decomp-chain (rsol decomp var *$var mult)
  (let ((sol (simplify (make-mequal (rdis (if decomp (car decomp)
					      ;; Include the var itself in the decomposition
					      (make-mrat-body (make-mrat-poly var '(1 1)) 1)))
				    (mequal-rhs rsol)))))
    (decomp-trace sol decomp var *$var mult)))

;; RE-SOLVE calls SOLVE recursively, returning a SOLUTION object.
;; Will not decompose or factor.

(defun re-solve (eqn var mult)
  (let ((*roots nil)
	(*failures nil)
	;; We've already decomposed and factored
	($solvedecomposes)
	($solvefactors))
    (solve eqn var mult)
    (make-solution *roots *failures)))

;; SOLVENTH programs test to see if the variable of interest appears 
;; to some power in all terms.  If so, a new variable is substituted for it
;; and the simpler expression solved with the multiplicity
;; adjusted accordingly.
;; SOLVENTHP returns gcd of exponents.

(defun solventhp (l gcd) 
  (cond ((null l) gcd)
	((equal gcd 1) 1)
	(t (solventhp (cddr l)
		      (gcd (car l) gcd)))))

;; Reduces exponents by their gcd.

(defun solventh (exp *g) 
  (let ((*varb (pdis (make-mrat-poly (poly-var exp) '(1 1))))
	(exp   (make-mrat-poly (poly-var exp) (solventh1 (poly-terms exp)))))
    (let* ((rts (re-solve-full (pdis exp) *varb))
	   (fails (solution-losses rts))
	   (wins (solution-wins rts))
	   (*power (make-mexpt *varb *g)))
      (map2c #'(lambda (w z)
		 (cond ((atom *varb)
			(solve (make-mequal *power (mequal-rhs w)) *varb z))
		       (t (let ((rts (re-solve-full
				      (make-mequal *power (mequal-rhs w))
				      *varb)))
			    (map2c #'(lambda (root mult)
				       (solve (make-mequal (mequal-rhs root) 0)
				      *myvar mult))
				   (solution-wins rts))))))
	     wins)
      (map2c #'(lambda (w z)
		 (push z *failures)
		 (push (solventh3 w *power *varb) *failures))
	     fails)
      *roots)))

(defun solventh3 (w *power *varb &aux varlist genvar *flg w1 w2)
  (cond ((broken-freeof *varb w) w)
	(t (setq w1 (ratf (cadr w)))
	   (setq w2 (ratf (caddr w)))
	   (setq varlist
		 (mapcar #'(lambda (h) 
			     (cond (*flg h)
				   ((alike1 h *varb)
				    (setq *flg t)
				    *power)
				   (t h)))
			 varlist))
	   (list (car w) (rdis (cdr w1)) (rdis (cdr w2))))))

(defun solventh1 (l) 
  (cond ((null l) nil)
	(t (cons (quotient (car l) *g)
		 (cons (cadr l) (solventh1 (cddr l)))))))

;; Will decompose or factor

(defun re-solve-full (x var &aux *roots *failures)
  (solve x var mult)
  (make-solution *roots *failures))

;; Sees if expression is of the form A*F<X>^N+B.

(defun of-form-A*F<X>^N+B (e)
  (and (memalike (simplify (pdis (list (car e) 1 1))) *has*var)
       (or (atom (caddr e))
           (not (memalike (simplify (pdis (list (caaddr e) 1 1)))
                          *has*var)))
       (or (null (cdddr e)) (equal (cadddr e) 0))))

;; Solves the special case A*F<X>^N+B.

(defun solve-A*F<X>^N+B (exp $%emode) 
  (prog (a b c) 
     (setq a (pdis (caddr exp)))
     (setq c (pdis (list (car exp) 1 1)))
     (cond ((null (cdddr exp))
	    (return (solve c *var (* (cadr exp) mult)))))
     (setq b (pdis (pminus (cadddr (cdr exp)))))
     (return (solve-A*F<X>^N+B1 c
			 (simpnrt (div* b a) (cadr exp))
			 (make-rat 1 (cadr exp))
			 (cadr exp)))))

(defun solve-A*F<X>^N+B1 (var root n thisn) 
  (do ((thisn thisn (1- thisn))) ((zerop thisn))
    (solve (add* var (mul* -1 root (power* '$%e (mul* 2 '$%pi '$%i thisn n))))
	   *var mult)))


;; ADISPLINE displays a line like DISPLINE, and in addition, notes that it is
;; not free of *VAR if it isn't.

(defun adispline (line)
  ;; This may be redundant, but nice if ADISPLINE gets used where not needed.
  (cond ((and $breakup (not $programmode))
	 (let ((linelabel (displine line)))
	   (cond ((broken-freeof *var line))
		 (t (setq broken-not-freeof
			  (cons linelabel broken-not-freeof))))
	   linelabel))
	(t (displine line))))

;; Predicate to check if an expression which may be broken up
;; is freeof

(setq broken-not-freeof nil)

;; For consistency, use backwards args.
;; == (freeof var exp) but works even if solution is broken up ($breakup=t)
(defun broken-freeof (var exp)
  (cond ($breakup
	 (do ((b-n-fo var (car b-n-fo-l))
	      (b-n-fo-l broken-not-freeof (cdr b-n-fo-l)))
	     ((null b-n-fo) t)
	   (and (not (argsfreeof b-n-fo exp))
		(return nil))))
	(t (argsfreeof var exp))))

;; Adds solutions to roots list.
;; Solves for inverse of functions (via USOLVE)

(defun solve3 (exp mult) 
  (setq exp (simplify exp))
  (cond ((not (broken-freeof *var exp))
	 (push mult *failures)
	 (push (make-mequal-simp (simplify *myvar) exp) *failures))
	(t (cond ((eq *myvar *var)
		  (push mult *roots)
		  (push (make-mequal-simp *var exp) *roots))
		 ((atom *myvar)
		  (push mult *failures)
		  (push (make-mequal-simp *myvar exp) *failures))
		 (t (usolve exp (g-rep-operator *myvar)))))))


;; Solve a linear equation.  Argument is a polynomial in pseudo-cre form.
;; This function is called for side-effect only.

(defun solvelin (exp) 
  (cond ((equal 0 (ptterm (cdr exp) 0))
	 (solve1a (caddr exp) mult)))
  (solve3 (rdis (ratreduce (pminus (ptterm (cdr exp) 0))
			   (caddr exp)))
	  mult))

;; Solve a quadratic equation.  Argument is a polynomial in pseudo-cre form.
;; This function is called for side-effect only.
;; The code for handling the case where the discriminant = 0 seems to never
;; be run.  Presumably, the expression is factored higher up.

(defun solvequad (exp &aux discrim a b c)
  (setq a (caddr exp))
  (setq b (ptterm (cdr exp) 1.))
  (setq c (ptterm (cdr exp) 0.))
  (setq discrim (simplify (pdis (pplus (pexpt b 2.)
				       (pminus (ptimes 4. (ptimes a c)))))))
  (setq b (pdis (pminus b)))
  (setq a (pdis (ptimes 2. a)))
  ;; At this point, everything is back in general representation.
  (let ((varlist nil)) ;;2/6/2002 RJF
    (cond ((equal 0 discrim)
	   (solve3 (fullratsimp `((mquotient) ,b ,a))
		   (* 2 mult)))
	  (t (setq discrim (simpnrt discrim 2))
	     (solve3 (fullratsimp `((mquotient) ((mplus) ,b ,discrim) ,a))
		     mult)
	     (solve3 (fullratsimp `((mquotient) ((mplus) ,b ((mminus) ,discrim)) ,a))
		     mult)))))

;; Reorders V so that members which contain the variable of
;; interest come first.

(defun varsort (v)
  (let ((*u nil)
	(*v (copy-list v)))
    (mapc #'(lambda (z) 
	      (cond ((broken-freeof *var z)
		     (setq *u (cons z *u))
		     (setq *v (delete z *v :count 1 :test #'equal)))))
	  v)
    (setq $dontfactor *u)
    (setq *has*var (mapcar #'resimplify *v))
    (append *u *v)))

;; Solves for variable when it occurs within a function by taking the inverse.
;; When this code is fixed, the `((mplus) ,x ,y) forms should be rewritten as
;; (MAKE-MPLUS X Y).  I didn't do this because the code was buggy and it should
;; be fixed first.  - cwh
;; You mean you didn't do it because you were buggy.  Hope you're fixed soon!
;; --RWK

;; Solve <exp> = <*myvar> for <*var>, where <*myvar>=<op>(...)
(defun usolve (exp op) 
  (prog (inverse) 
     (setq inverse
	   (cond
	     ((eq op 'mexpt)
	      (cond ((broken-freeof *var
				    (cadr *myvar))
		     (cond ((equal exp 0)
			    (go fail)))
		     `((mplus) ((mminus) ,(caddr *myvar))
		       ,(div* `((%log) ,exp)
			      `((%log) ,(cadr *myvar)))))
		    ((broken-freeof *var
				    (caddr *myvar))
		     (cond ((equal exp 0)
			    (cond ((mnegp (caddr *myvar))
				   (go fail))
				  (t (cadr *myvar))))
			   ;; There is a bug right here.
			   ;; SOLVE(SQRT(U)+1) should return U=1
			   ;; This code is entered with EXP = -1, OP = MEXPT
			   ;; *VAR = U, and *MYVAR = ((MEXPT) U ((RAT) 1 2))
			   ;; BULLSHIT -- RWK.  That is precisely the bug
			   ;; this code was added to fix!
			   ((and (not (eq (ask-integer (caddr *myvar)
						       '$integer)
					  '$yes))
				 (free exp '$%i)
				 (eq ($asksign exp) '$neg))
			    (go fail))				    
			   (t `((mplus) ,(cadr *myvar)
				((mminus)
				 ((mexpt) ,exp
				  ,(div* 1 (caddr *myvar))))))))
		    (t (go fail))))
	     ((setq inverse (get op '$inverse))
	      (when (and $solvetrigwarn
			 (member op '(%sin %cos %tan %sec %csc %cot %cosh %sech) :test #'eq))
		(mtell (intl:gettext "~&solve: using arc-trig functions to get a solution.~%Some solutions will be lost.~%"))
		(setq $solvetrigwarn nil))
          ;; Sanity check: return NIL (no solutions) if either
          ;; - simplifying INVERSE(EXP) causes an error
          ;;   (e.g., tan(%pi/2) when solving atan(x) = %pi/2)
          ;; - simplifying OP(INVERSE(EXP)) causes an error
          ;; - MEQP says that OP(INVERSE(EXP)) is definitely not equal to EXP
          ;;   (e.g., tan(2) as a solution to atan(x) = 2)
          (let* ((inverse-exp-l (errcatch (ftake* inverse exp)))
                 (check-l (and inverse-exp-l (errcatch (ftake op (car inverse-exp-l))))))
            (if (or (null inverse-exp-l)
                    (null check-l)
                    (not (meqp (car check-l) exp)))
              (return nil)
	      `((mplus) ((mminus) ,(cadr *myvar))
		,(car inverse-exp-l)))))
	     ((eq op '%log)
	      `((mplus) ((mminus) ,(cadr *myvar))
		((mexpt) $%e ,exp)))
         ((eq op 'mabs)
           (let ((exp-sign ($csign exp)))
             (cond
               ((eq exp-sign '$zero)
                 ;; abs(z) = 0 <=> z = 0
                 (solve (cadr *myvar) *var mult)
                 (return nil))
               ((or (member exp-sign '($neg $imaginary))
                    (eq t (mnqp 0 ($imagpart exp))))
                 ;; abs(z) can't be negative or complex; no solution.
                 (return nil))
               ((not (member ($csign (cadr *myvar)) '($complex $imaginary)))
                 ;; abs(real) = EXP <=> real +- EXP = 0.
                 (solve (add (cadr *myvar) exp) *var mult)
                 (solve (sub (cadr *myvar) exp) *var mult)
                 (return nil))
               (t
                 (go fail)))))
         ((eq op '%signum)
           (cond
             ((eq ($csign exp) '$zero)
               ;; signum(z) = 0 <=> z = 0
               (solve (cadr *myvar) *var mult)
               (return nil))
             ((and (not (member ($csign (cadr *myvar)) '($complex $imaginary)))
                   (not (meqp exp -1))
                   (not (meqp exp 0))
                   (not (meqp exp 1)))
               ;; signum(real) can only be -1, 0, 1; no solution.
               (return nil))
             (t
               (go fail))))
	     (t (go fail))))

     ;; Return NIL (no solutions) if simplification causes an error.
     ;; Otherwise, keep solving.
     (setq inverse (errcatch (simplify inverse)))
     (return (if inverse
               (solve (car inverse) *var mult)))

     fail (return (setq *failures
			(cons (simplify `((mequal) ,*myvar ,exp))
			      (cons mult *failures))))))

;; Predicate for determining if an expression is messy enough to 
;; generate a new linelabel for it.
;; Expression must be in general form.

(defun complicated (exp)
  (and $breakup
       (not $programmode)
       (not (free exp 'mplus))))

(defun rootsort (l) 
  (prog (a fm fm1) 
   g1   (cond ((null l) (return nil)))
   (setq a (car (setq fm l)))
   (setq fm1 (cdr fm))
   loop (cond ((null (cddr fm)) (setq l (cddr l)) (go g1))
	      ((alike1 (caddr fm) a)
	       (rplaca fm1 (+ (car fm1) (cadddr fm)))
	       (rplacd (cdr fm) (cddddr fm))
	       (go loop)))
   (setq fm (cddr fm))
   (go loop)))

(defmfun $linsolve (eql varl)
  (let (($ratfac))
    (setq eql (if ($listp eql) (cdr eql) (ncons eql)))
    (setq varl (if ($listp varl)
		   (delete-duplicates (cdr varl) :test #'equal :from-end t)
		   (ncons varl)))
    (do ((varl varl (cdr varl)))
	((null varl))
      (when (mnump (car varl))
	(merror (intl:gettext "solve: variable must not be a number; found: ~M") (car varl))))
    (if (null varl)
	(make-mlist-simp)
	(solvex (mapcar 'meqhk eql) varl (not $programmode) nil))))

(defun solvex (eql varl ind flag &aux ($algebraic $algebraic))
  (declare (special xa*))
  (prog (*varl ans varlist genvar xm* xn* mul*)
     (setq *varl varl)
     (setq eql (mapcar #'(lambda (x) ($ratdisrep ($ratnumer x))) eql))
     (cond ((atom (ignore-rat-err (formx flag 'xa* eql varl)))
	    ;; This flag is T if called from SOLVE
	    ;; and NIL if called from LINSOLVE.
	    (cond (flag (return ($algsys (make-mlist-l eql)
					 (make-mlist-l varl))))
		  (t (merror (intl:gettext "linsolve: cannot solve a nonlinear equation."))))))
     (setq ans (tfgeli 'xa* xn* xm*))
     (if (and $linsolvewarn (car ans))
	 (mtell (intl:gettext "~&solve: dependent equations eliminated: ~A~%") (car ans)))
     (if (cadr ans)
	 (return '((mlist simp))))
     (do ((j 0 (1+ j)))
	 ((> j xm*))
       ;;I put this in the value cell--wfs 
       (setf (aref xa* 0 j) nil))
     (ptorat 'xa* xn* xm*)
     (setq varl
	   (xrutout 'xa* xn* xm* 
		    (mapcar #'(lambda (x) (ith varl x))
			    (caddr ans))
		    ind))
     (if $programmode
	 (setq varl (make-mlist-l (linsort (cdr varl) *varl))))
     (return varl)))

;; (LINSORT '(((MEQUAL) A2 FOO) ((MEQUAL) A3 BAR)) '(A3 A2))
;; returns (((MEQUAL) A3 BAR) ((MEQUAL) A2 FOO)) .

(defun linsort (meq-list var-list)
  (mapcar #'(lambda (x) (cons (caar meq-list) x))
	  (sort (mapcar #'cdr meq-list)
		   #'(lambda (x y) (member y (member x var-list :test #'equal) :test #'equal)) :key #'car)))
