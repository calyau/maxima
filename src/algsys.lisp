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

(macsyma-module algsys)

(load-macsyma-macros ratmac)

;;This is the algsys package.

;;It solves systems of polynomial equations by straight-forward
;;resultant hackery.  Other possible methods seem worse:
;;the Buchberger-Spear canonical ideal basis algorithm is slow,
;;and the "resolvent" method (see van der Waerden, section 79)
;;blows up in time and space.  The "resultant"
;;method (see the following sections of van der Waerden and
;;Macaulay's book - Algebraic Theory of Modular Systems) looks
;;good, but it requires the evaluation of large determinants.
;;Unless some hack (such as prs's for evaluating resultants of
;;two polynomials) is developed for multi-polynomial resultants,
;;this method will remain impractical.

;;Some other possible ideas:  Keeping the total number of equations constant,
;;in an effort to reduce extraneous solutions, or Reducing to a linear
;;equation before taking resultants.

(declare-top (special $algdelta
		     *roots *failures $ratprint
		     *tvarxlist* *ivar* errset))

;;note if $algepsilon is too large you may lose some roots.

(defmvar $algdelta 1e-5 )

(defmvar $realonly nil "If t only real solutions are returned.")

(defmvar realonlyratnum nil
  "A REALROOTS hack for RWG.  Causes ALGSYS to retain rational numbers
  returned by REALROOTS when REALONLY is TRUE."
  in-core)

(defmvar $algexact nil "If t ALGSYS always calls SOLVE to try to MAXIMA-FIND exact
			solutions.")

(defmvar algnotexact nil
  "A hack for RWG for univariate polys.  Causes SOLVE not to get called
  so that sqrts and cube roots will not be generated."
  in-core)

(defmacro merrset (l)
  `(let ((errset t) (unbind (cons bindlist loclist)) val)
     (setq val (errset ,l))
     (when (null val) (errlfun1 unbind))
     val))

(defmfun $algsys (lhslist varxlist &aux varlist genvar)
  ;;  (declare (special varxlist)) ;;??
  (setq $%rnum_list (list '(mlist)))
  (cond ((not ($listp lhslist))
	 (merror (intl:gettext "algsys: first argument must be a list; found ~M") lhslist))
	((not ($listp varxlist))
	 (merror (intl:gettext "algsys: second argument must be a list; found ~M") varxlist)))
  (let ((tlhslist nil) (*tvarxlist* nil) (solnlist nil) ($ratprint nil)
	($ratepsilon 1e-7)
	($keepfloat nil)
	(varlist (reverse (cdr varxlist)))
	(genvar nil) ($ratfac nil) ($breakup nil)
	($solvefactors nil) (*roots nil) (*failures nil)
	(*ivar* nil) ($polyfactor nil) (varxl nil)
	($infeval nil) ($numer nil) ($float nil)
	(numerflg $numer))
    (dolist (var (cdr ($listofvars (list '(mlist simp) lhslist varxlist))))
      (if (and (symbolp var) (not (constant var)))
	  (setq varxl (cons var varxl))))
    (orderpointer varlist)
    (setq tlhslist
	  (mapcar #'(lambda (q) (cadr (ratf (meqhk q))))
		  (cdr lhslist)))
    (setq *ivar* (caadr (ratf '$%i)))
    (setq *tvarxlist*
	  (mapcar #'(lambda (q)
		      (if (mnump q)
			  (merror (intl:gettext "algsys: variable cannot be a number; found ~M") q)
			  (caadr (ratf q))))
		  (cdr varxlist)))
    (putorder *tvarxlist*)
    (mbinding (varxl varxl)
	      (setq solnlist
		    (mapcar #'(lambda (q)
				(addmlist
				 (bbsorteqns
				  (addparam (roundroots1 q) varxlist))))
			    (algsys tlhslist))))
    (remorder *tvarxlist*)
    (setq solnlist (addmlist solnlist))
    (if numerflg
	(let (($numer t) ($float t))
	  (resimplify solnlist))
	solnlist)))

;;; (CONDENSESOLNL TEMPSOLNL)
;;;
;;; Condense a solution list, discarding any solution that is a special case of
;;; another one. (For example, if the list contained [x=1, y=1] as a solution,
;;; but also just [x=1], then the first solution would be discarded)
;;;
;;; Destructively modifies TEMPSOLNL
(defun condensesolnl (tempsolnl)
  (let (solnl)
    (mapl (lambda (q)
            (unless (subsetl (cdr q) (car q))
              (push (car q) solnl)))
	  (stable-sort tempsolnl #'(lambda (a b) (> (length a) (length b)))));FIXME consider a total order function with #'sort
    solnl))

;;; (SUBSETL L1 S2)
;;;
;;; Check whether some element of L1 is a subset of S2 (comparing elements with
;;; ALIKE1). As a special case, if S2 is '(NIL) then return true.
(defun subsetl (l1 s2)
  (or (equal s2 '(nil))
      (member-if (lambda (x)
                   (subsetp x s2 :test #'alike1))
                 l1)))

(defun algsys (tlhslist)
  (condensesolnl
   (mapcan #'algsys0
           (distrep (mapcar #'lofactors tlhslist)))))

(defun algsys0 (tlhslist)
  (cond ((null tlhslist) (list nil))
	((equal tlhslist (list nil)) nil)
	(t (algsys1 tlhslist))))

(defun algsys1 (tlhslist)
  (destructuring-bind (resulteq . vartorid) (findleastvar tlhslist)
    (bakalevel (algsys
                (mapcar #'(lambda (q)
                            (if (among vartorid q)
                                (presultant q resulteq vartorid)
                                q))
                        (remove resulteq tlhslist :test #'equal)))
               tlhslist vartorid)))

(defun addmlist (l)
  (cons '(mlist) l))

(defmacro what-the-$ev (&rest l)
  ;; macro for calling $EV when you are not really
  ;; sure why you are calling it, but you want the
  ;; features of multiple evaluations and unpredictabiltiy
  ;; anyway.
  `(meval (list '($ev) ,@l)))

(defun rootsp (asolnset eqn)		;eqn is ((MLIST) eq deriv)
  (let (rr ($keepfloat t) ($numer t) ($float t))
    (setq rr (what-the-$ev eqn asolnset)) ; ratsimp?
    (cond ((and (complexnump (cadr rr)) (complexnump (caddr rr)))
	   (< (cabs (cadr rr))
		  (* $algdelta (max 1 (cabs (caddr rr))))))
	  (t nil))))

(defun round1 (a)
  (cond ((floatp a)
	 (setq a (maxima-rationalize a))
	 (fpcofrat1 (car a) (cdr a)))
	(t a)))

(defun roundrhs (eqn)
  (list (car eqn) (cadr eqn) (round1 (caddr eqn))))

(defun roundroots1 (lsoln)
  (mapcar #'roundrhs lsoln))

(defun bbsorteqns (l)
  (sort (copy-list l) #'orderlessp))

(defun putorder (tempvarl)
  (do ((n 1 (1+ n))
       (tempvarl tempvarl (cdr tempvarl)))
      ((null tempvarl) nil)
    (putprop (car tempvarl) n 'varorder)))

(defun remorder (gvarl)
  (mapc #'(lambda (x) (remprop x 'varorder)) gvarl))


(defun orderlessp (eqn1 eqn2)
  (< (get (caadr (ratf (cadr eqn1))) 'varorder)
     (get (caadr (ratf (cadr eqn2))) 'varorder)))

(defun addparam (asolnsetl varxlist)
  (cond ((= (length asolnsetl) (length *tvarxlist*))
	 asolnsetl)
	(t
	 (do ((tvarxl (cdr varxlist) (cdr tvarxl))
	      (defvar (mapcar #'cadr asolnsetl))
	      (var) (param))
	     ((null tvarxl) asolnsetl)
	   (setq var (car tvarxl))
	   (cond ((memalike var defvar) nil)
		 (t (setq param (make-param)
			  asolnsetl (cons (list '(mequal) var param)
					  (cdr (maxima-substitute
						param var
						(addmlist asolnsetl)))))))))))

;; Do not remove this (and the unspecial below).  While the functions
;; that reference *vardegs* have special declarations for it,
;; something is missing.  If this is removed, then the testsuite fails
;; where algsys produces the same solutions but in a different order.
;; More troubling is that tests from rtest_odelin produces incorrect
;; results and appears to hang somewhere after problem 57.
(declare-top (special *vardegs*))

;;; (FINDLEASTVAR LHSL)
;;;
;;; Iterate over the polynomials in LHSL, trying to find a "least var", which is
;;; a variable that will hopefully be easiest to solve for. Variables from
;;; *TVARXLIST* and their products are considered.
;;;
;;; For example, if *TVARXLIST* contains x, y and we only considered the
;;; polynomial x^3 + y^2 + x then we'd have a least var of y with degree 2. If c
;;; is not in *TVARXLIST* then we'd get the same answer from x^3 + c*y^2 + x
;;; because such variables are just ignored. However, x^3 + x^2*y^2 would yield
;;; x with degree 3 because the mixed term x^2*y^2 has higher total degree.
;;;
;;; The function returns the polynomial with the variable with minimal maximum
;;; degree (as described above), together with that variable.
;;;
;;; Mixed terms are mostly ignored, but consider this pair of polynomials:
;;; [x*y+1, x^3+1]. In the first polynomial, the only non-constant term is
;;; mixed. Its degree in the first polynomial is 2 which is less than 3, so that
;;; first polynomial is returned along with its leading variable.
(defun findleastvar (lhsl)
  (let ((*vardegs*)
        (leasteq) (leastvar)
        ;; most-positive-fixnum is larger than any polynomial degree, so we can
        ;; initialise with this and be certain to replace it on the first
        ;; iteration.
        (leastdeg most-positive-fixnum))
    (declare (special *vardegs*))
    (loop
       for teq in lhsl
       for *vardegs* = (getvardegs teq)
       for tdeg = (killvardegsc teq)
       do (loop
             for q in *vardegs*
             if (<= (cdr q) leastdeg)
             do (setq leastdeg (cdr q)
                      leasteq teq
                      leastvar (car q)))
       if (< tdeg leastdeg)
       do (setq leastdeg tdeg
                leasteq teq
                leastvar (car teq)))
    (cons leasteq leastvar)))

;;; DO-POLY-TERMS
;;;
;;; Iterate over the terms in a polynomial, POLY, executing BODY with LE and LC
;;; bound to the exponent and coefficient respectively of each term. If RESULT
;;; is non-NIL, it is evaluated to give a result when the iteration finishes.
(defmacro do-poly-terms ((le lc poly &optional result) &body body)
  (let ((pt (gensym)))
    `(do ((,pt (p-terms ,poly) (pt-red ,pt)))
         ((null ,pt) ,result)
       (let ((,le (pt-le ,pt))
             (,lc (pt-lc ,pt)))
         ,@body))))

;;; (KILLVARDEGSC POLY)
;;;
;;; For each monomial in POLY that is mixed in the variables in *VARDEGS*
;;; (i.e. has more than one variable from *VARDEGS* with positive exponent),
;;; iterate over all but the first variable, checking to see whether its degree
;;; in the monomial is at least as high as that in *VARDEGS*. If so, delete that
;;; variable and its degree from *VARDEGS*.
;;;
;;; Returns the maximum total degree of any term in the polynomial, summing
;;; degrees over the variables in *VARDEGS*.
(defun killvardegsc (poly)
  (if (pconstp poly)
      0
      (let ((tdeg 0))
        (do-poly-terms (le lc poly tdeg)
          (setf tdeg (max tdeg (+ le
                                  (if (= le 0)
                                      (killvardegsc lc)
                                      (killvardegsn lc)))))))))

;;; (KILLVARDEGSN POLY)
;;;
;;; For each monomial in POLY, look at its degree in each variable in
;;; *TVARXLIST*. If the degree is at least as high as that recorded in
;;; *VARDEGS*, delete that variable and its degree from *VARDEGS*.
;;;
;;; Returns the maximum total degree of any term in the polynomial, summing
;;; degrees over the variables in *VARDEGS*.
(defun killvardegsn (poly)
  (declare (special *vardegs*))
  (cond
    ((pconstp poly) 0)
    (t
     (let ((x (assoc (p-var poly) *vardegs* :test #'eq)))
       (when (and x (<= (cdr x) (p-le poly)))
         (setq *vardegs* (delete x *vardegs* :test #'equal))))
     (let ((tdeg 0))
       (do-poly-terms (le lc poly tdeg)
         (setf tdeg (max tdeg (+ le (killvardegsn lc)))))))))

;;; (GETVARDEGS POLY)
;;;
;;; Return degrees of POLY's monomials in the variables for which we're
;;; solving. Ignores mixed terms (like x*y). Results are returned as an alist
;;; with elements (VAR . DEGREE).
;;;
;;; For example, if *TVARXLIST* is '(x y) and we are looking at the polynomial
;;; x^2 + y^2, we have
;;;
;;;   (GETVARDEGS '(X 2 1 0 (Y 2 1))) => ((X . 2) (Y . 2))
;;;
;;; Variables that aren't in *TVARXLIST* are assumed to come after those that
;;; are. For example c*x^2 would look like
;;;
;;;   (GETVARDEGS '(X 2 (C 1 1))) => ((X . 2))
;;;
;;; Mixed powers are ignored, so x*y + y looks like:
;;;
;;;   (GETVARDEGS '(X 1 (Y 1 1) 0 (Y 1 1))) => ((Y . 1))

(defun getvardegs (poly)
  (cond ((pconstp poly) nil)
	((pconstp (caddr poly))
	 (cons (cons (car poly) (cadr poly))
	       (getvardegs (ptterm (cdr poly) 0))))
	(t (getvardegs (ptterm (cdr poly) 0)))))

(declare-top (unspecial *vardegs*))

(defun pconstp (poly)
  (or (atom poly) (not (member (car poly) *tvarxlist* :test #'eq))))

;;; (PFREEOFMAINVARSP POLY)
;;;
;;; If POLY isn't a polynomial in the variables for which we're solving,
;;; disrep it and simplify appropriately.
(defun pfreeofmainvarsp (poly)
  (if (or (atom poly)
          (member (car poly) *tvarxlist* :test #'eq))
      poly
     (simplify-after-subst (pdis poly))))

;;; (LOFACTORS POLY)
;;;
;;; If POLY is a polynomial in one of the variables for which we're solving,
;;; then factor it into a list of factors (where the result returns factors
;;; alternating with their multiplicity in the same way as PFACTOR).
;;;
;;; If POLY is not a polynomial in one of the solution variables, return NIL.
(defun lofactors (poly)
  (let ((main-var-poly (pfreeofmainvarsp poly)))
    (cond
      ((pzerop main-var-poly) '(0))

      ;; If POLY isn't a polynomial in our chosen variables, RADCAN will return
      ;; something whose CAR is a cons. In that case, or if the polynomial is
      ;; something like a number, there are no factors to extract.
      ((or (atom main-var-poly)
           (not (atom (car main-var-poly))))
       nil)

      (t
       (do ((tfactors (pfactor main-var-poly) (cddr tfactors))
            (lfactors))
           ((null tfactors) lfactors)
         (let ((main-var-factor (pfreeofmainvarsp (car tfactors))))
           (cond
             ((pzerop main-var-factor)
              (return (list 0)))
             ((and (not (atom main-var-factor))
                   (atom (car main-var-factor)))
              (push (pabs main-var-factor) lfactors)))))))))

;;; (COMBINEY LISTOFL)
;;;
;;; Combine "independent" lists in LISTOFL. If all the lists have empty pairwise
;;; intersections, this returns all selections of items, one from each
;;; list. Destructively modifies LISTOFL.
;;;
;;; Selections are built up starting at the last list. When building, if there
;;; would be a repeated element because the list we're about to select from has
;;; nonempty intersection with an existing partial selections then elements from
;;; the current list aren't added to this selection.
;;;
;;; COMBINEY guarantees that no list in the result has two elements that are
;;; ALIKE1 each other.
;;;
;;; This is used to enumerate combinations of solutions from multiple
;;; equations. Each entry in LISTOFL is a list of possible solutions for an
;;; equation. A solution for the set of equations is found by looking at
;;; (compatible) combinations of solutions.
;;;
;;; (I don't know why the non-disjoint behaviour works like this. RJS 1/2015)
(defun combiney (listofl)
  (unless (member nil listofl)
    (combiney1 (delete '(0) listofl :test #'equal))))

;;; DB (2016-09-13) Commit a158b1547 introduced a regression (SF bug 3210)
;;; It: - restructured combiney
;;;     - used ":test #'alike1" in place of "test #'equal" in combiney1
;;; Reverting the change to combiney1 restores previous behaviour.
;;; I don't understand algsys internals and haven't analysed this further.
(defun combiney1 (listofl)
  (cond ((null listofl) (list nil))
	(t (mapcan #'(lambda (r)
		       (if (intersection (car listofl) r :test #'equal)
			   (list r)
			   (mapcar #'(lambda (q) (cons q r)) (car listofl))))
		   (combiney1 (cdr listofl))))))

(defun midpnt (l)
  (rhalf (rplus* (car l) (cadr l))))

(defun rflot (l)
  (let ((rr (midpnt l)))
    (if realonlyratnum (list '(rat) (car rr) (cdr rr))
	(/ (+ 0.0 (car rr)) (cdr rr)))))

(defun memberroot (a x eps)
  (cond ((null x) nil)
	((< (abs (- a (car x)))
		(/ (+ 0.0 (car eps)) (cdr eps)))
	 t)
	(t (memberroot a (cdr x) eps))))

(defun commonroots (eps solnl1 solnl2)
  (cond ((null solnl1) nil)
	((memberroot (car solnl1) solnl2 eps)
	 (cons (car solnl1) (commonroots eps (cdr solnl1) solnl2)))
	(t (commonroots eps (cdr solnl1) solnl2))))

;; (REMOVE-MULT L)
;;
;; Return a copy of L with all elements in odd positions removed. This is so
;; named because some code returns roots and multiplicities in the format
;;
;;   (ROOT0 MULT0 ROOT1 MULT1 ... ROOTN MULTN)
;;
;; Calling REMOVE-MULT on such a list removes the multiplicities.
(defun remove-mult (l)
  (and l (cons (car l) (remove-mult (cddr l)))))

(defun punivarp (poly)
  ;; Check if called with the number zero, return nil. 
  ;; Related bugs: SF[609466], SF[1430379], SF[1663399]
  (when (and (numberp poly) (= poly 0)) (return-from punivarp nil))
  (do ((l (cdr poly) (cddr l)))
      ((null l) t)
    (or (numberp (cadr l))
	(and (eq (caadr l) *ivar*)
	     (punivarp (cadr l)))
	(return nil))))

;; (REALONLY ROOTSL)
;;
;; Return only the elements of ROOTSL whose $IMAGPART simplifies to zero with
;; SRATSIMP. (Note that this a subset of "the real roots", because SRATSIMP may
;; not be able to check that a given expression is zero)
(defun realonly (rootsl)
  (remove-if-not (lambda (root)
                   (equal 0 (sratsimp ($imagpart (caddr root)))))
                 rootsl))


(defun presultant (p1 p2 var)
  (cadr (ratf (simplify ($resultant (pdis p1) (pdis p2) (pdis (list var 1 1)))))))

(defun ptimeftrs (l)
  (prog (ll)
     (setq ll (cddr l))
     (cond ((null ll) (return (car l)))
	   (t (return (ptimes (car l) (ptimeftrs ll)))))))

;; (EBAKSUBST SOLNL LHSL)
;;
;; Substitute a solution for one variable back into the "left hand side
;; list". If the equation had to be solved for multiple variables, this allows
;; us to use the solution for a first variable to feed in to the equation for
;; the next one along.
;;
;; As well as doing the obvious substitution, EBAKSUBST also simplifies with
;; $RADCAN (presumably, E stands for Exponential)
(defun ebaksubst (solnl lhsl)
  (mapcar #'(lambda (q) (ebaksubst1 solnl q)) lhsl))

(defun ebaksubst1 (solnl q)
  (let ((e ($substitute `((mlist) ,@solnl) (pdis q))))
    (setq e (simplify-after-subst e))
    (cadr (ratf e))))

(defun baksubst (solnl lhsl)
  (setq lhsl (delete 't (mapcar #'(lambda (q) (car (merrset (baksubst1 solnl q))))
				lhsl)
		     :test #'eq))	;catches arith. ovfl
  (if (member nil lhsl :test #'eq)
      (list nil)
      lhsl))

(defun baksubst1 (solnl poly)
  (let* (($keepfloat (not $realonly))	;sturm1 needs poly with
	 (poly1				;integer coefs
	  (cdr (ratf (what-the-$ev (pdis poly)
				   (cons '(mlist) solnl)
				   '$numer)))))
    (cond ((and (complexnump (pdis (car poly1)))
		(numberp (cdr poly1)))
	   (rootsp (cons '(mlist) solnl)
		   (list '(mlist) (pdis poly) (tayapprox poly))))
	  (t (car poly1)))))

(defun complexnump (p)
  (let ((p (cadr (ratf ($ratsimp p)))))
    (or (numberp p)
	(eq (pdis (pget (car p))) '$%i))))

;; (SIMPLIFY-AFTER-SUBST EXPR)
;;
;; Simplify EXPR after substitution of a partial solution.
;;
;; Focus is on constant expressions:
;; o failure to reduce a constant expression that is equivalent
;;   to zero causes solutions to be falsely rejected
;; o some operations, such as the reduction of nested square roots,
;;   requires known sign and ordering of all terms
;; o inappropriate simplification by $RADCAN introduced errors
;;   $radcan(sqrt(-1/(1+%i)))     => exhausts heap
;;   $radcan(sqrt(6-3^(3/2))) > 0 => sqrt(sqrt(3)-2)*sqrt(3)*%i < 0
;;
;; Problems from bug reports showed that further simplification of
;; non-constant terms, with incomplete information, could lead to
;; missed roots or unwanted complexity.
;;
;; $ratsimp with algebraic:true can transform
;;     sqrt(2)*sqrt(-1/(sqrt(3)*%i+1)) => (sqrt(3)*%i)/2+1/2
;; but $rectform is required for
;;     sqrt(sqrt(3)*%i-1)) => (sqrt(3)*%i)/sqrt(2)+1/sqrt(2)
;; and $rootscontract is required for
;;     sqrt(34)-sqrt(2)*sqrt(17) => 0
(defun simplify-after-subst (expr)
  "Simplify expression after substitution"
  (let (($keepfloat t) ($algebraic t) (e expr)
	e1 e2 tmp (growth-factor 1.2)
	(genvar nil) (varlist nil)
	($rootsconmode t) ($radexpand t))
    ;; Try two approaches
    ;; 1) ratsimp
    ;; 2) if $constantp(e) sqrtdenest + rectform + rootscontract + ratsimp
    ;; take smallest expression
    (setq e1 (sratsimp e))
    (if ($constantp e)
      (progn
	(setq e ($sqrtdenest e))
	;; Rectform does more than is wanted.  A function that denests and
	;; rationalizes nested complex radicals would be better.
	;; Limit expression growth.  The factor is based on trials.
	(setq tmp ($rectform e))
	(when (< (conssize tmp) (* growth-factor (conssize e)))
	  (setq e tmp))
	(setq e ($rootscontract e))
	(setq e2 (sratsimp e))
	(if (< (conssize e1) (conssize e2)) e1 e2))
      e1)))

;; (BAKALEVEL SOLNL LHSL VAR)
;;
;;; Recursively try to find a solution to the list of polynomials in LHSL. SOLNL
;;; should be a non-empty list of partial solutions (for example, these might be
;;; solutions we've already found for x when we're solving for x and y).
;;;
;;; BAKALEVEL works over each partial solution. This should itself be a list. If
;;; it is non-nil, it is a list of equations for the variables we're trying to
;;; solve for ("x = 3 + y" etc.). In this case, BAKALEVEL substitutes these
;;; solutions into the system of equations and then tries to solve the
;;; result. On success, it merges the partial solutions in SOLNL with those it
;;; gets recursively.
;;;
;;; If a partial solution is nil, we don't yet have any partial information. If
;;; there is only a single polynomial to solve in LHSL, we try to solve it in
;;; the given variable, VAR. Otherwise we choose a variable of lowest degree
;;; (with FINDLEASTVAR), solve for that (with CALLSOLVE) and then recurse.
(defun bakalevel (solnl lhsl var)
  (loop for q in solnl nconcing (bakalevel1 q lhsl var)))

(defun bakalevel1 (solnl lhsl var)
  (cond
    ((not (exactonly solnl))
     (mergesoln solnl (apprsys (baksubst solnl lhsl))))
    (solnl
     (mergesoln solnl (algsys (ebaksubst solnl lhsl))))
    ((cdr lhsl)
     (let ((poly-and-var (findleastvar lhsl)))
       (bakalevel (callsolve poly-and-var)
                  (remove (car poly-and-var) lhsl :test #'equal)
                  var)))
    ;; LHSL contains one polynomial and we try to solve in one variable VAR.
    ;; CALLSOLVE can miss solutions when the coefficient of the highest order
    ;; term in VAR contains other variables in *TVARXLIST**.
    ;; BAKALEVELSOLVE looks for these missed solutions.
    (t (nconc (callsolve (cons (car lhsl) var))
	      (bakalevelsolve (car lhsl) var)))))


;; (EVERY-ATOM PRED X)
;;
;; Evaluates to true if (PRED Y) is true for every atom Y in the cons tree X.
(defun every-atom (pred x)
  (if (atom x)
      (funcall pred x)
      (and (every-atom pred (car x))
           (every-atom pred (cdr x)))))

;; (EXACTONLY SOLNL)
;;
;; True if the list of solutions doesn't contain any terms that look inexact
;; (just floating point numbers, unless realonlyratnum is true)
(defun exactonly (solnl)
  (every-atom (lambda (x)
                (and (not (floatp x))
                     (or (null realonlyratnum)
                         (not (eq x 'rat)))))
              solnl))

;; (MERGESOLN ASOLN SOLNL)
;;
;; For each solution S in SOLNL, evaluate each element of ASOLN in light of S
;; and, collecting up the results and prepending them to S. If evaluating an
;; element in light of S caused an error, ignore the combination of ASOLN and S.
(defun mergesoln (asoln solnl)
  (let ((unbind (cons bindlist loclist))
        (errorsw t))
    (macrolet ((catch-error-t (&body body)
                 `(let ((result (catch 'errorsw ,@body)))
                    (when (eq result t)
                      (errlfun1 unbind))
                    result)))
      (loop
         for q in solnl
         for result =
           (catch-error-t
            (append (mapcar (lambda (r)
                              (what-the-$ev r (cons '(mlist) q)))
                            asoln)
                   q))
         if (not (eq result t)) collect result))))

;; (CALLSOLVE PV)
;;
;; Try to solve a polynomial with respect to the given variable. PV is a cons
;; pair (POLY . VAR). On success, return a list of solutions. Each solution is
;; itself a list, whose elements are equalities (one for each variable in the
;; equation). If we determine that there aren't any solutions, return '(NIL).
;;
;; If POLY is in more than one variable or if it can clearly be solved by the
;; quadratic formula (BIQUADRATICP), we always call SOLVE to try to get an exact
;; solution. Similarly if the user has set the $ALGEXACT variable to true.
;;
;; Otherwise, or if SOLVE fails, we try to find an approximate solution with a
;; call to CALLAPPRS.
;;
;; SOLVE introduces solutions with nested radicals, which causes problems
;; in EBAKSUBST1.  Try to clean up the solutions now.
(defun callsolve (pv)
  (mapcar  #'callsolve2 (callsolve1 pv)))

(defun callsolve1 (pv)
  (let ((poly (car pv))
	(var (cdr pv))
	(varlist varlist)
	(genvar genvar)
	(*roots nil)
	(*failures nil)
	($programmode t))
    (cond ((or $algexact
               (not (punivarp poly))
	       (biquadraticp poly))
           ;; Call SOLVE to try to solve POLY. When it returns, the solutions it
           ;; found end up in *ROOTS. *FAILURES contains expressions that, if
           ;; solved, would lead to further solutions.
	   (solve (pdis poly) (pdis (list var 1 1)) 1)
           (if (null (or *roots *failures))
               ;; We're certain there are no solutions
               (list nil)
               ;; Try to find approximate solutions to the terms that SOLVE gave
               ;; up on (in *FAILURES) and remove any roots from SOLVE that
               ;; aren't known to be real if $REALONLY is true.
               (append (mapcan (lambda (q)
                                 (callapprs (cadr (ratf (meqhk q)))))
                               (remove-mult *failures))
                       (mapcar #'list
                               (if $realonly
                                   (realonly (remove-mult *roots))
                                   (remove-mult *roots))))))
	  (t (callapprs poly)))))

(defun callsolve2 (l)
  "Simplify solution returned by callsolve1"
  ;; l is a single element list '((mequal simp) var expr)
  (let ((e (first l)))
    `(((,(mop e)) ,(second e) ,(simplify-after-subst (third e))))))

;;; (BIQUADRATICP POLY)
;;;
;;; Check whether POLY is biquadratic in its main variable: either of degree at
;;; most two or of degree four and with only even powers.
(defun biquadraticp (poly)
  (or (atom poly)
      (if algnotexact
	  (< (p-le poly) 2)
	  (or (< (p-le poly) 3)
	      (and (= (p-le poly) 4) (biquadp1 (p-red poly)))))))

(defun biquadp1 (terms)
  (or (null terms)
      (and (or (= (pt-le terms) 2) (= (pt-le terms) 0))
	   (biquadp1 (pt-red terms)))))

;;; (CALLAPPRS POLY)
;;;
;;; Try to find approximate solutions to POLY, which should be a polynomial in a
;;; single variable. Uses STURM1 if we're only after real roots (because
;;; $REALONLY is set). Otherwise, calls $ALLROOTS.
(defun callapprs (poly)
  (unless (punivarp poly)
    (merror (intl:gettext "algsys: Couldn't reduce system to a polynomial in one variable.")))
  (let ($dispflag)
    (if $realonly
        (let ((dis-var (pdis (list (car poly) 1 1))))
          (mapcar #'(lambda (q)
                      (list (list '(mequal) dis-var (rflot q))))
                  (sturm1 poly (cons 1 $algepsilon))))
        (mapcar #'list
                (let* (($programmode t)
                       (roots (cdr ($allroots (pdis poly)))))
                  (if (eq (caaar roots) 'mequal)
                      roots
                      (cdr roots)))))))

(defun apprsys (lhsl)
  (cond ((null lhsl) (list nil))
	(t
	 (do ((tlhsl lhsl (cdr tlhsl))) (nil)
	   (cond ((null tlhsl)
          ;; SHOULD TRY TO BE MORE SPECIFIC: "TOO COMPLICATED" IN WHAT SENSE??
		  (merror (intl:gettext "algsys: system too complicated; give up.")))
		 ((pconstp (car tlhsl)) (return nil))
		 ((punivarp (car tlhsl))
		  (return (bakalevel (callapprs (car tlhsl))
				     lhsl nil))))))))

(defun tayapprox (p)
  (cons '(mplus)
	(mapcar #'(lambda (x)
		    (list '(mycabs) (pdis (ptimes (list x 1 1)
						  (pderivative p x)))))
		(listovars p))))

(defun mycabs (x)
  (and (complexnump x) (cabs x)))

;;; (DISTREP LOL)
;;;
;;; Take selections from LOL, a list of lists, using COMBINEY. When used by
;;; ALGSYS, the elements of the lists are per-equation solutions for some system
;;; of equations. COMBINEY combines the per-equation solutions into prospective
;;; solutions for the entire system.
;;;
;;; These prospective solutions are then filtered with CONDENSESOLNL, which
;;; discards special cases of more general solutions.
;;;
;;; (I don't understand why this reversal has to be here, but we get properly
;;;  wrong solutions to some of the testsuite functions without it. Come back to
;;;  this... RJS 1/2015)
(defun distrep (lol)
  (condensesolnl (mapcar #'reverse (combiney lol))))

(defun exclude (l1 l2)
  (cond ((null l2)
	 nil)
	((member (car l2) l1 :test #'equal)
	 (exclude l1 (cdr l2)))
	(t
	 (cons (car l2) (exclude l1 (cdr l2))))))

;;; (BAKALEVELSOLVE P VAR)
;;;
;;; P is a polynomial in VAR.  VAR may not be the main variable.
;;; We are trying to solve P in terms of the single variable VAR, so
;;; the other solution variables (in *TVARXLIST*) are treated as constants.
;;;
;;; CALLSOLVE can miss solutions when the leading (highest order)
;;; coefficient C of VAR contains other variables in *TVARXLIST*.
;;; Apparently C is implicitly assumed to be non-zero.
;;;
;;; Seek these missing solutions by:
;;; - finding N, the highest power of VAR in P
;;; - separating P into C*VAR^N + D (VAR may not be the main variable in P)
;;; - look for solutions of form (C=0,D=0)
;;;
;;; An example is solving a*b-c*d in terms of [a,b,c,d]
;;;
(defun bakalevelsolve (p var)
  (let (n c d)
    (cond
     ;; a bare coefficient isn't an equation
     ((pcoefp p) nil)
     ;; N is highest power of VAR in polynomial P.
     ;; No equation in VAR if N=0.  No solution.
     ((= (setq n (p-hipow-var p var)) 0) nil)
    (t
      ;; express P as C*VAR^N + D
      (setf (values c d) (p-coef-x p var n))
      (if (p-allvars c)   ; Does C contain any variables from *TVARXLIST*
        (algsys `(,c ,d)) ; Try and solve C=0 and D=0 using algsys
	 nil)))))        ; C is freeof of *TVARXLIST*.  No solution.

;;; De-duplicated list of variables in polynomial P
(defun p-allvars (p)
  (unless (pcoefp p) (remove-duplicates (p-allvars1 p))))

;;; List of variables in polynomial P.  May contain duplicates.
(defun p-allvars1 (p)
  (unless (pcoefp p)
    (cons
     (p-var p)       ; the main variable of poly
     (loop           ; Recusively extract variables from coefficients.
      for (nil ci) on (p-terms p) by #'cddr
      if (not (pcoefp ci)) append (p-allvars1 ci)))))

;;; Maximum degree of variable V in polynomial P
;;; V may not be the main variable in P
(defun p-hipow-var (p v)
  (cond
    ((pcoefp p) 0)              ; bare coefficient has degree 0
    ((eq (p-var p) v) (p-le p)) ; V is main variable - return leading exponent
    (t
      (loop              ; Recusively search through coefficients of p
        for (nil c) on (p-terms p) by #'cddr
        maximize (p-hipow-var c v)))))

;;; (P-COEF-X P X N)
;;;
;;; Separate multivariable polynomial P with main variable V into C*x^n + D
;;; where X may be different to V.  Return (values C D)
;;; Assumes terms of P are in decending order
(defun p-coef-x (p x n)
  (cond
   ((pcoefp p)                  ; p is a bare coefficient
     (if (= n 0)
       (values p (pzero))       ; c = bare coef, d = 0
       (values (pzero) p)))     ; c = 0,         d = bare coef
   ((eq (p-var p) x)            ; main variable V in P is X
     (p-coef-main p n))
   (t
     ;; P is polynomial with main variable V other than X
     ;; iterate over coefficients ai
     ;; find (ci di) such that ai = ci*x^n + di
     ;; C = ck*v^k + ... + c0*v^0, a polynomial in v
     ;; D = dk*v^k + ... + d0*v^0, a polynomial in v
     (loop
       with terms = (p-terms p) with v = (p-var p) with ci with di
       for (i ai) on terms by #'cddr
       do (setf (values ci di) (p-coef-x ai x n))
       unless (pzerop ci) append `(,i ,ci) into cterms
       unless (pzerop di) append `(,i ,di) into dterms
       finally (return (values (psimp v cterms) (psimp v dterms)))))))


;;; (P-COEF-MAIN P N)
;;;
;;; Separate multivariable polynomial P with main variable X into C*x^n + D
;;; Return (values C D)
;;; Assumes terms of P are in decending order
(defun p-coef-main (p n)
  (if (pcoefp p)              ; bare coefficient
    (if (= n 0)
      (values p 0)             ; c = bare coef & d = 0
      (values 0 p))            ; c = 0         & d = bare coef
    (loop                      ; proper polynomial
      with L = (p-terms p)     ; list of terms in polynomial
      with var = (p-var p)     ; main variable
      for i  = (pop L)         ; power of term
      for ai = (pop L)         ; coefficient of term x^i
      while (>= i n)           ; exit via finally if n-th power not present
      if (> i n)
        append `(,i ,ai) into terms       ; accumulate higher order terms
      else                ; have i==n so C=ai.  Construct D and return
        do (return (values ai (psimp var (append terms L))))
      while L          ; exit via finally if all coefficients examined
      finally (return (values 0 p)))))
