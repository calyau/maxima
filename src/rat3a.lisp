;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module rat3a)

;; This is the new Rational Function Package Part 1.
;; It includes most of the coefficient and polynomial routines required
;; by the rest of the functions.  Certain specialized functions are found
;; elsewhere.  Most low-level utility programs are also in this file.

(declare-top (unspecial coef var exp p y))

;;These do not need to be special for this file and they
;;slow it down on lispm. We also eliminated the special
;;from ptimes2--wfs

;; CQUOTIENT
;;
;; Calculate the quotient of two coefficients, which should be numbers. If
;; MODULUS is non-nil, we try to take the reciprocal of A with respect to the
;; modulus (using CRECIP) and then multiply by B. Note that this fails if B
;; divides A as an integer, but B is not a unit in the ring of integers modulo
;; MODULUS. For example,
;;
;;   (let ((modulus 20)) (cquotient 10 5)) => ERROR
;;
;; If MODULUS is nil, then we work over the ring of integers when A and B are
;; integers, and raise a RAT-ERROR if A is not divisible by B. If either A or B
;; is a float then the division is done in floating point. Floats can get as far
;; as the rational function code if $KEEPFLOAT is true.

;; Before May 2023, this code used (rem a b) along with (/ a b) instead of
;; just (floor a b). For Clozure CL the floor method is faster.
(defun cquotient (a b)
  (cond ((eql a 0) 0) ;not sure this is OK--what if b=0 too?
        ((null modulus)
           (cond ((or (floatp a) (floatp b)) (/ a b)) ;not sure about floats here!
                 (t
                  (multiple-value-bind (q r) (floor a b)
                   ;; when the remainder vanishes, return the quotient; else rat-error.
                   (if (eql 0 r) q (rat-error "CQUOTIENT: quotient is not exact"))))))
        (t
           (ctimes a (crecip b)))))
;; ALG
;;
;; Get any value stored on the tellrat property of (car l). Returns NIL if L
;; turns out not to be a list or if $ALGEBRAIC is false.
(defun alg (l)
  (unless (atom l) (algv (car l))))

;; PACOEFP
;;
;; Return T if either X is a bare coefficient or X is a polynomial whose main
;; variable has a declared value as an algebraic integer. Otherwise return NIL.
(defun pacoefp (x)
  (and (or (pcoefp x) (alg x))
       T))

;; LEADTERM
;;
;; Return the leading term of POLY as a polynomial itself.
(defun leadterm (poly)
  (if (pcoefp poly)
      poly
      (make-poly (p-var poly) (p-le poly) (p-lc poly))))

;; CRECIP
;;
;; Takes the inverse of an integer N mod MODULUS. If there is a modulus then the
;; result is constrained to lie in (-modulus/2, modulus/2]
;;
;; This just uses the extended Euclidean algorithm: once you have found a,b such
;; that a*n + b*modulus = 1 then a must be the reciprocal you're after.
;;
;; When MODULUS is greater than 2^15, we use exactly the same algorithm in
;; CRECIP-GENERAL, but it can't use fixnum arithmetic. Note: There's no
;; particular reason to target 32 bits except that trying to work out the right
;; types on the fly looks complicated and this lisp compiler, at least, uses 32
;; bit words. Since we have to take a product, we constrain the types to 16 bit
;; numbers.
(defun crecip (n)
  ;; Punt on anything complicated
  (unless (and modulus (typep modulus '(unsigned-byte 15)))
    (return-from crecip (crecip-general n)))

  ;; And make sure that -MODULUS < N < MODULUS
  (unless (<= (- modulus) n modulus)
    (merror "N is out of range [-MODULUS, MODULUS] in crecip."))

  ;; N in [0, MODULUS]
  (when (minusp n) (setf n (+ n modulus)))

  ;; The mod-copy parameter stores a copy of MODULUS on the stack, which is
  ;; useful because the lisp implementation doesn't know that the special
  ;; variable MODULUS is still an (unsigned-byte 15) when we get to the end
  ;; (since it can't tell that our function calls don't change it behind our
  ;; backs, I guess)
  (let ((mod modulus) (remainder n) (a 1) (b 0)
        (mod-copy modulus))
    ;; On SBCL in 2013 at least, the compiler doesn't spot that MOD and
    ;; REMAINDER are unsigned and bounded above by MODULUS, a 16-bit integer. So
    ;; we have to tell it. Also, the lisp implementation can't really be
    ;; expected to know that Bezout coefficients are bounded by the modulus and
    ;; remainder, so we have to tell it that too.
    (declare (type (unsigned-byte 15) mod mod-copy remainder)
             (type (signed-byte 16) a b))

    (loop
       until (= remainder 1)

       when (zerop remainder) do
         (merror (intl:gettext "CRECIP: ~M does not have an inverse with modulus=~M")
                 n modulus)
       doing
         (multiple-value-bind (quot rem)
             (truncate mod remainder)
           (setf mod remainder
                 remainder rem)
           (psetf a (- b (* a quot))
                  b a))

       finally
         ;; Since this isn't some general purpose Euclidean algorithm, but
         ;; instead is trying to find a modulo inverse, we need to ensure that
         ;; the Bezout coefficient we found (called A) is actually in [0,
         ;; MODULUS).
         ;;
         ;; The general code calls CMOD here, but that doesn't know about the
         ;; types of A and MODULUS, so we do it by hand, special-casing the easy
         ;; case of modulus=2.
         (return
           (if (= mod-copy 2)
               (logand a 1)
               (let ((nn (mod a mod-copy)))
                 ;; nn here is in [0, modulus)
                 (if (<= (* 2 nn) mod-copy)
                     nn
                     (- nn mod-copy))))))))

;; CRECIP-GENERAL
;;
;; The general algorithm for CRECIP, valid when the modulus is any integer. See
;; CRECIP for more details.
(defun crecip-general (n)
  ;; We assume that |n| < modulus, so n+modulus is always positive
  (let ((mod modulus)
        (remainder (if (minusp n) (+ n modulus) n))
        (a 1) (b 0))
    (loop
       until (= remainder 1)

       when (zerop remainder) do
         (merror (intl:gettext "CRECIP: ~M does not have an inverse with modulus=~M")
                 n modulus)
       doing
         (let ((quotient (truncate mod remainder)))
           (psetf mod remainder
                  remainder (- mod (* quotient remainder)))
           (psetf a (- b (* a quotient))
                  b a))

       finally (return (cmod a)))))

;; CEXPT
;;
;; Raise an coefficient to a positive integral power. BASE should be a
;; number. POW should be a non-negative integer.
(defun cexpt (base pow)
  (unless (typep pow '(integer 0))
    (error "CEXPT only defined for non-negative integral exponents."))
  (if (not modulus)
      (expt base pow)
      (do ((pow (ash pow -1) (ash pow -1))
           (s (if (oddp pow) base 1)))
          ((zerop pow) s)
        (setq base (rem (* base base) modulus))
        (when (oddp pow) (setq s (rem (* s base) modulus))))))

;; CMOD
;;
;; When MODULUS is null, this is the identity. Otherwise, it normalises N, which
;; should be a number, to lie in the range (-modulus/2, modulus/2].
(defun cmod (n)
  (declare (type number n))
  (if (not modulus)
      n
      (let ((rem (mod n modulus)))
        (if (<= (* 2 rem) modulus)
            rem
            (- rem modulus)))))

(defun cplus       (a b) (cmod (+ a b)))
(defun ctimes      (a b) (cmod (* a b)))
(defun cdifference (a b) (cmod (- a b)))

;; SET-MODULUS
;;
;; Set the base in which the rational function package works. This does
;; sanity-checking on the value chosen and is probably the way you should set
;; the global value.
;;
;; Valid values for M are either a positive integer or NULL.
(defun set-modulus (m)
  (if (or (null m) (typep m '(integer 1)))
      (setq modulus m)
      (error "modulus must be a positive integer or nil"))
  (values))

;; PCOEFADD
;;
;; Prepend a term to an existing polynomial. EXPONENT should be the exponent of
;; the term to add; COEFF should be its coefficient; REMAINDER is a list of
;; polynomial terms. The function returns polynomial terms that correspond to
;; adding the given term.
;;
;; The function doesn't check that EXPONENT is higher than the highest exponent
;; in REMAINDER, so you have to do this yourself.
(defun pcoefadd (exponent coeff remainder)
  (if (pzerop coeff)
      remainder
      (cons exponent (cons coeff remainder))))

;; PPLUS
;;
;; Add together two polynomials.
(defun pplus (x y)
  (cond ((pcoefp x) (pcplus x y))
	((pcoefp y) (pcplus y x))
	((eq (p-var x) (p-var y))
	 (psimp (p-var x) (ptptplus (p-terms y) (p-terms x))))
	((pointergp (p-var x) (p-var y))
	 (psimp (p-var x) (ptcplus y (p-terms x))))
	(t (psimp (p-var y) (ptcplus x (p-terms y))))))

;; PTPTPLUS
;;
;; Add together two lists of polynomial terms.
(defun ptptplus (x y)
  (cond ((ptzerop x) y)
	((ptzerop y) x)
	((= (pt-le x) (pt-le y))
	 (pcoefadd (pt-le x)
		   (pplus (pt-lc x) (pt-lc y))
		   (ptptplus (pt-red x) (pt-red y))))
	((> (pt-le x) (pt-le y))
	 (cons (pt-le x) (cons (pt-lc x) (ptptplus (pt-red x) y))))
	(t (cons (pt-le y) (cons (pt-lc y) (ptptplus x (pt-red y)))))))

;; PCPLUS
;;
;; Add a coefficient to a polynomial
(defun pcplus (c p)
  (if (pcoefp p)
      (cplus p c)
      (psimp (p-var p)
             (ptcplus c (p-terms p)))))

;; PTCPLUS
;;
;; Add a coefficient to a list of terms. C should be a used as a coefficient;
;; TERMS is a list of a polynomial's terms. Note that we don't assume that C is
;; a number: it might be a polynomial in a variable that isn't the main variable
;; of the polynomial.
(defun ptcplus (c terms)
  (cond
    ;; Adding zero doesn't do anything.
    ((pzerop c) terms)
    ;; Adding to zero, you just get the coefficient.
    ((null terms) (list 0 c))
    ;; If terms are from a constant polynomial, we can just add C to its leading
    ;; coefficient (which might not be a number in the multivariate case, so you
    ;; have to use PPLUS)
    ((zerop (pt-le terms))
     (pcoefadd 0 (pplus c (pt-lc terms)) nil))
    ;; If TERMS is a polynomial with degree > 0, recurse.
    (t
     (cons (pt-le terms) (cons (pt-lc terms) (ptcplus c (pt-red terms)))))))

;; PDIFFERENCE
;;
;; Compute the difference of two polynomials
(defun pdifference (x y)
  (cond
    ;; If Y is a coefficient, it's a number, so we can just add -Y to X using
    ;; pcplus. If, however, X is the coefficient, we have to negate all the
    ;; coefficients in Y, so defer to a utility function.
    ((pcoefp x) (pcdiffer x y))
    ((pcoefp y) (pcplus (cminus y) x))
    ;; If X and Y have the same variable, work down their lists of terms.
    ((eq (p-var x) (p-var y))
     (psimp (p-var x) (ptptdiffer (p-terms x) (p-terms y))))
    ;; Treat Y as a coefficient in the main variable of X.
    ((pointergp (p-var x) (p-var y))
     (psimp (p-var x) (ptcdiffer-minus (p-terms x) y)))
    ;; Treat X as a coefficient in the main variable of Y.
    (t (psimp (p-var y) (ptcdiffer x (p-terms y))))))

;; PTPTDIFFER
;;
;; Compute the difference of two lists of polynomial terms (assumed to represent
;; two polynomials in the same variable).
(defun ptptdiffer (x y)
  (cond
    ((ptzerop x) (ptminus y))
    ((ptzerop y) x)
    ((= (pt-le x) (pt-le y))
     (pcoefadd (pt-le x)
               (pdifference (pt-lc x) (pt-lc y))
               (ptptdiffer (pt-red x) (pt-red y))))
    ((> (pt-le x) (pt-le y))
     (cons (pt-le x) (cons (pt-lc x) (ptptdiffer (pt-red x) y))))
    (t (cons (pt-le y) (cons (pminus (pt-lc y))
                             (ptptdiffer x (pt-red y)))))))
;; PCDIFFER
;;
;; Subtract the polynomial P from the coefficient C to form c - p.
(defun pcdiffer (c p)
  (if (pcoefp p)
      (cdifference c p)
      (psimp (p-var p) (ptcdiffer c (p-terms p)))))

;; PTCDIFFER
;;
;; Subtract a polynomial represented by the list of terms, TERMS, from the
;; coefficient C.
(defun ptcdiffer (c terms)
  (cond
    ;; Unlike in the plus case or in PTCDIFFER-MINUS, we don't have a shortcut
    ;; if C=0. However, if TERMS is null then we are calculating C-0, which is
    ;; easy:
    ((null terms)
     (if (pzerop c) nil (list 0 c)))
    ;; If the leading exponent is zero (in the main variable), then we can
    ;; subtract the coefficients. Of course, these might actually be polynomials
    ;; in other variables, so do this using pdifference.
    ((zerop (pt-le terms))
     (pcoefadd 0 (pdifference c (pt-lc terms)) nil))
    ;; Otherwise we have to negate the leading coefficient (using pminus of
    ;; course, because it might be a polynomial in other variables) and recurse.
    (t
     (cons (pt-le terms)
           (cons (pminus (pt-lc terms)) (ptcdiffer c (pt-red terms)))))))

;; PTCDIFFER-MINUS
;;
;; Subtract a coefficient, C, from a polynomial represented by a list of terms,
;; TERMS, to form "p-c". This is the same as PTCDIFFER but the opposite sign (we
;; don't implement it by (pminus (ptcdiffer c terms)) because that would require
;; walking the polynomial twice)
(defun ptcdiffer-minus (terms c)
  (cond
    ;; We're subtracting zero from a polynomial, which is easy!
    ((pzerop c) terms)
    ;; We're subtracting a coefficient from zero, which just comes out as the
    ;; negation of the coefficient (compute it using pminus)
    ((null terms) (list 0 (pminus c)))
    ;; If the leading exponent is zero, subtract the coefficients just like in
    ;; PTCDIFFER.
    ((zerop (pt-le terms))
     (pcoefadd 0 (pdifference (pt-lc terms) c) nil))
    ;; Otherwise recurse.
    (t
     (cons (pt-le terms)
           (cons (pt-lc terms) (ptcdiffer-minus (pt-red terms) c))))))

;; PCSUB
;;
;; Substitute values for variables in the polynomial P. VARS and VALS should be
;; list of variables to substitute for and values to substitute, respectively.
;;
;; The code assumes that if VAR1 precedes VAR2 in the list then (POINTERGP VAR1
;; VAR2). As such, VAR1 won't appear in the coefficients of a polynomial whose
;; main variable is VAR2.
(defun pcsub (p vals vars)
  (cond
    ;; Nothing to substitute, or P has no variables in it.
    ((or (null vals) (pcoefp p)) p)
    ;; The first variable in our list is the main variable of P.
    ((eq (p-var p) (first vars))
     (ptcsub (p-terms p) (first vals)
             (cdr vals) (cdr vars)))
    ;; If the first var should appear before the main variable of P, we know it
    ;; doesn't appear in any of the coefficients, so can (tail-)recurse on vals
    ;; + vars.
    ((pointergp (car vars) (p-var p))
     (pcsub p (cdr vals) (cdr vars)))
    ;; Else, the main variable shouldn't get clobbered, but maybe we should
    ;; replace variables in the coefficients.
    (t (psimp (p-var p) (ptcsub-args (p-terms p) vals vars)))))

;; PCSUBST
;;
;; Substitute VAL for VAR in a polynomial. Like PCSUB, but with only a single
;; var to be substituted.
;;
;; (The logic of this function is exactly the same as PCSUB, but is marginally
;;  simpler because there are no more vars afterwards. Presumably, it was
;;  thought worth separating this case out from PCSUB to avoid spurious
;;  consing. I'm not convinced. RJS)
(defun pcsubst (p val var)
  (cond ((pcoefp p) p)
	((eq (p-var p) var) (ptcsub (cdr p) val nil nil))
	((pointergp var (p-var p)) p)
	(t (psimp (car p) (ptcsub-args (cdr p) (list val) (list var))))))

;; PTCSUB
;;
;; Substitute a constant, VAL, for the main variable in TERMS, which represent
;; the terms of a polynomial. The coefficients might themselves be polynomials
;; and, if so, we might substitute values for them too. To do so, pass VALS and
;; VARS, with the same ordering requirements as in PCSUB.
(defun ptcsub (terms val vals vars)
  (if (eql val 0)
      ;; If we're substituting 0 for the value, then we just extract the
      ;; constant term.
      (pcsub (ptterm terms 0) vals vars)
      ;; Otherwise, walk through the polynomial using Horner's scheme to
      ;; evaluate it. Because the polynomial is sparse, you can't just multiply
      ;; by VAL every step, and instead have to keep track of the jump in
      ;; exponents, which is what the LAST-LE variable does.
      (do ((terms (pt-red terms) (pt-red terms))
	   (ans (pcsub (pt-lc terms) vals vars)
		(pplus (ptimes ans (pexpt val (- last-le (pt-le terms))))
		       (pcsub (pt-lc terms) vals vars)))
	   (last-le (pt-le terms) (pt-le terms)))
	  ((null terms)
           (ptimes ans (pexpt val last-le))))))

;; PTCSUB-ARGS
;;
;; Substitute values for vars in TERMS, which should be the terms of some
;; polynomial. Unlike PTCSUB, we assume that the main variable of the polynomial
;; isn't being substituted. VARS and VALS should be ordered as in PCSUB.
(defun ptcsub-args (terms vals vars)
  (loop
     for (exp coef) on terms by #'cddr
     unless (pzerop (setq coef (pcsub coef vals vars)))
     nconc (list exp coef)))

;; PCSUBSTY
;;
;; Like PCSUB, but with arguments in a different order and with a special case
;; that you can pass atoms for VALS and VARS, in which case they will be treated
;; as one-element lists. The big difference with PCSUB is that we don't assume
;; that VARS and VALS come pre-sorted, and sort them here.
(defun pcsubsty (vals vars p)
  (cond
    ;; If there is nothing to do, the answer is just P.
    ((null vars) p)
    ;; When there's only one variable, we don't need to do any sorting, so skip
    ;; it and call PCSUB directly.
    ((atom vars) (pcsub p (list vals) (list vars)))
    ;; Otherwise, call PCSUB with a sorted list of VARS and VALS.
    (t
     (let ((pairs (sort (mapcar #'cons vars vals) #'pointergp :key #'car)))
       (pcsub p (mapcar #'cdr pairs) (mapcar #'car pairs))))))

;; PDERIVATIVE
;;
;; Compute the derivative of the polynomial P with respect to the variable VARI.
(defun pderivative (p vari)
  (cond
    ;; The derivative of a constant is zero.
    ((pcoefp p) 0)
    ;; If we have the same variable, do the differentiation term-by-term.
    ((eq vari (p-var p))
     (psimp (p-var p) (ptderivative (p-terms p))))
    ;; If VARI > (P-VAR P) then we know it doesn't occur in any of the
    ;; coefficients either, so return zero. This test comes after the one above
    ;; because we expect more univariate polynomials and eq is cheaper than
    ;; pointergp.
    ((pointergp vari (p-var p)) 0)
    ;; The other possibility is that (P-VAR P) > VARI, so the coefficients might
    ;; need differentiating.
    (t
     (psimp (p-var p) (ptderivative-coeffs (p-terms p) vari)))))

;; PTDERIVATIVE
;;
;; Formally differentiate TERMS, which is a list of the terms of some
;; polynomial, with respect to that polynomial's main variable.
(defun ptderivative (terms)
  (if (or (null terms) (zerop (pt-le terms)))
      ;; Zero or constant polynomials -> 0
      nil
      ;; Recurse, adding up "k . x^(k-1)" each time.
      (pcoefadd (1- (pt-le terms))
                (pctimes (cmod (pt-le terms)) (pt-lc terms))
                (ptderivative (pt-red terms)))))

;; PTDERIVATIVE-COEFFS
;;
;; Differentiate TERMS, which is a list of the terms of some polynomial, with
;; respect to the variable VARI. We assume that VARI is not the main variable of
;; the polynomial, but it might crop up in the coefficients.
(defun ptderivative-coeffs (terms vari)
  (and terms
       ;; Recurse down the list of terms, calling PDERIVATIVE to actually
       ;; differentiate each coefficient, then PTDERIVATIVE-COEFFS to do the rest.
       (pcoefadd (pt-le terms)
                 (pderivative (pt-lc terms) vari)
                 (ptderivative-coeffs (pt-red terms) vari))))

;; PDIVIDE
;;
;; Polynomial division with remainder. X and Y should be polynomials. If V
;; denotes the main variable of X, then we are carrying out the division in a
;; ring of polynomials over Q where all variables that occur after V have been
;; formally inverted. This is a Euclidean ring, and PDIVIDE implements division
;; with remainder in this ring.
;;
;; The result is a list of two elements (Q R). Each is a rational function (a
;; cons pair of polynomials), representing an element of F[V].
(defun pdivide (x y)
  (cond
    ((pzerop y) (rat-error "PDIVIDE: Quotient by zero"))
    ;; If Y is a coefficient, it doesn't matter what X is: we can always do the
    ;; division.
    ((pacoefp y) (list (ratreduce x y) (rzero)))
    ;; If X is a coefficient but Y isn't then the quotient must be zero
    ((pacoefp x) (list (rzero) (cons x 1)))
    ;; If neither is a coefficient then compare the variables. If V is greater
    ;; than the main variable of Y, then Y is invertible in F[V].
    ((pointergp (p-var x) (p-var y)) (list (ratreduce x y) (rzero)))
    ;; If we've got to here, V might occur in the coefficients of Y, but it
    ;; needn't be the main variable.
    (t
     (do* ((lcy (cons (p-lc y) 1))
           (q (rzero))
           (r (cons x 1))
           (k (- (pdegree x (p-var y)) (p-le y))
              (- (pdegree (car r) (p-var y)) (p-le y))))

          ;; k is the degree of the numerator of the remainder minus the degree
          ;; of y, both in the leading variable of y. For there to be further
          ;; factors of y to subtract from q, this must be non-negative.
          ((minusp k) (list q r))

       ;; Divide the leading coefficient of r (which means the leading term of
       ;; the numerator, divided by the denominator) by the leading coefficient
       ;; of y.
       ;;
       ;; The quotient gets added to q and gets multiplied back up by y and the
       ;; result is subtracted from r.
       (let* ((lcr (cons (p-lc (car r)) (cdr r)))
              (quot (ratquotient lcr lcy))
              (quot-simp (cons (psimp (p-var y) (list k (car quot)))
                               (cdr quot))))
         (setf q (ratplus q quot-simp)
               r (ratplus r (rattimes (cons (pminus y) 1) quot-simp t))))))))

;; PEXPT
;;
;; Polynomial exponentiation. Raise the polynomial P to the power N (which
;; should be an integer)
(defun pexpt (p n)
  (cond
    ;; p^0 = 1; p^1 = p
    ((= n 0) 1)
    ((= n 1) p)
    ;; p^(-n) = 1/p^n
    ((minusp n) (pquotient 1 (pexpt p (- n))))
    ;; When p is a coefficient, we can the simpler cexpt (which expects n >= 0,
    ;; guaranteed by the previous clause)
    ((pcoefp p) (cexpt p n))
    ;; If the main variable of P is an algebraic integer, calculate the power by
    ;; repeated squaring (which will correctly take the remainder wrt the
    ;; minimal polynomial for the variable)
    ((alg p) (pexptsq p n))
    ;; If p is a monomial in the main variable, we're doing something like
    ;; (x^2(y+1))^n, which is x^2n (y+1)^n, exponentiate the coefficient by
    ;; recursion and just multiply the exponent. The call to PCOEFADD is just to
    ;; ensure that we get zero if the coefficient raises to the power
    ;; zero. (Possible when the coefficient is an algebraic integer)
    ((null (p-red p))
     (psimp (p-var p)
            (pcoefadd (* n (p-le p)) (pexpt (p-lc p) n) nil)))
    ;; In the general case, expand using the binomial theorem. Write the
    ;; calculation as
    ;;
    ;;    (b + rest)^n  = sum (binomial (n,k) * rest^k * b^(n-k), k, 0, n)
    ;;
    ;; We pre-compute a list of descending powers of B and use the formula
    ;;
    ;;    binomial(n,k)/binomial(n,k-1) = (n+1-k) / k
    ;;
    ;; to keep track of the binomial coefficient.
    (t
     (let ((descending-powers (p-descending-powers
                               (make-poly (p-var p) (p-le p) (p-lc p)) n))
           (rest (psimp (p-var p) (p-red p))))
       (do* ((b-list descending-powers (rest b-list))
             (k 0 (1+ k))
             (n-choose-k 1 (truncate (* n-choose-k (- (1+ n) k)) k))
             (rest-pow 1 (case k
                           (1 rest)
                           (2 (pexpt rest 2))
                           (t (ptimes rest rest-pow))))
             (sum (first descending-powers)
                  (pplus sum
                         (if b-list
                             (ptimes (pctimes (cmod n-choose-k) rest-pow)
                                     (first b-list))
                             (pctimes (cmod n-choose-k) rest-pow)))))
            ((> k n) sum))))))

;; P-DESCENDING-POWERS
;;
;; Return a list of the powers of the polynomial P in descending order, starting
;; with P^N and ending with P.
(defun p-descending-powers (p n)
  (let ((lst (list p)))
    (dotimes (i (1- n)) (push (ptimes p (car lst)) lst))
    lst))

;; PMINUSP
;;
;; Returns true if the coefficient of the leading monomial of the polynomial is
;; negative. Note that this depends on the variable ordering (for example,
;; consider x-y).
;;
;;   (pminusp '(y 1 -1 0 (x 1 1))) => T     but
;;   (pminusp '(x 1 1 0 (y 1 -1))) => NIL
(defun pminusp (p)
  (if (realp p) (minusp p)
      (pminusp (p-lc p))))

;; PMINUS
;;
;; Unary negation for polynomials.
(defun pminus (p)
  (if (pcoefp p) (cminus p)
      (cons (p-var p) (ptminus (p-terms p)))))

;; PTMINUS
;;
;; Negate a list of polynomial terms.
(defun ptminus (x)
  (loop for (exp coef) on x by #'cddr
	 nconc (list exp (pminus coef))))

;; PMOD
;;
;; Reduce a polynomial modulo the current value of MODULUS.
(defun pmod (p)
  (if (pcoefp p) (cmod p)
      (psimp (car p)
	     (loop for (exp coef) on (p-terms p) by #'cddr
		    unless (pzerop (setq coef (pmod coef)))
		    nconc (list exp coef)))))

;; PQUOTIENT
;;
;; Calculate x/y in the polynomial ring over the integers. Y should divide X
;; without remainder.
(defun pquotient (x y)
  (cond ((pcoefp x)
	 (cond ((pzerop x) (pzero))
	       ((pcoefp y) (cquotient x y))
	       ((alg y) (paquo x y))
	       (t (rat-error "PQUOTIENT: Quotient by a polynomial of higher degree (case 1)"))))

	((pcoefp y)
         (cond ((pzerop y) (rat-error "PQUOTIENT: Quotient by zero"))
               (modulus (pctimes (crecip y) x))
               (t (pcquotient x y))))

        ;; If (alg y) is true, then y is a polynomial in some variable that
        ;; itself has a minimum polynomial. Moreover, the $algebraic flag must
        ;; be true. We first try to compute an exact quotient ignoring that
        ;; minimal polynomial, by binding $algebraic to nil. If that fails, we
        ;; try to invert y and then multiply the results together.
	((alg y) (or (let ($algebraic)
                       (ignore-rat-err (pquotient x y)))
		     (patimes x (rainv y))))

        ;; If the main variable of Y comes after the main variable of X, Y must
        ;; be free of that variable, so must divide each coefficient in X. Thus
        ;; we can use PCQUOTIENT.
	((pointergp (p-var x) (p-var y)) (pcquotient x y))

        ;; Either Y contains a variable that is not in X, or they have the same
        ;; main variable and Y has a higher degree. There can't possibly be an
        ;; exact quotient.
	((pointergp (p-var y) (p-var x))
     (rat-error "PQUOTIENT: Quotient by a polynomial of higher degree (case 2a)"))
	((> (p-le y) (p-le x))
     (rat-error "PQUOTIENT: Quotient by a polynomial of higher degree (case 2b)"))

        ;; If we got to here then X and Y have the same main variable and Y has
        ;; a degree less than or equal to that of X. We can now forget about the
        ;; main variable and work on the terms, with PTPTQUOTIENT.
	(t
         (psimp (p-var x) (ptptquotient (p-terms x) (p-terms y))))))

;; PCQUOTIENT
;;
;; Divide the polynomial P by Q. Q should be either a coefficient (so that
;; (pcoefp q) => T), or should be a polynomial in a later variable than the main
;; variable of P. Either way, Q is free of the main variable of P. The division
;; is done at each coefficient.
(defun pcquotient (p q)
  (psimp (p-var p)
         (loop
            for (exp coef) on (p-terms p) by #'cddr
            nconc (list exp (pquotient coef q)))))

;; PTPTQUOTIENT
;;
;; Exactly divide two polynomials in the same variable, represented here by the
;; list of their terms.
(defun ptptquotient (u v)
  ;; The algorithm is classic long division. You notice that if X/Y = Q then X =
  ;; QY, so lc(X) = lc(Q)lc(Y) (where lc(Q)=Q when Q is a bare coefficient). Now
  ;; divide again in the ring of coefficients to see that lc(X)/lc(Y) =
  ;; lc(Q). Of course, you also know that le(Q) = le(X) - le(Y).
  ;;
  ;; Once you know lc(Q), you can subtract Y * lc(Q)*(var^le(Q)) from X and
  ;; repeat. You know that you'll remove the leading term, so the algorithm will
  ;; always terminate. To do the subtraction, use PTPT-SUBTRACT-POWERED-PRODUCT.
  (do ((q-terms nil)
       (u u (ptpt-subtract-powered-product (pt-red u) (pt-red v)
                                           (first q-terms) (second q-terms))))
      ((ptzerop u)
       (nreverse q-terms))
    ;; If B didn't divide A after all, then eventually we'll end up with the
    ;; remainder in u, which has lower degree than that of B.
    (when (< (pt-le u) (pt-le v))
      (rat-error "PTPTQUOTIENT: Polynomial quotient is not exact"))
    (let ((le-q (- (pt-le u) (pt-le v)))
          (lc-q (pquotient (pt-lc u) (pt-lc v))))
      ;; We've calculated the leading exponent and coefficient of q. Push them
      ;; backwards onto q-terms (which holds the terms in reverse order).
      (setf q-terms (cons lc-q (cons le-q q-terms))))))

;; PTPT-SUBTRACT-POWERED-PRODUCT
;;
;; U and V are the terms of two polynomials, A and B, in the same variable, x. Q
;; is free of x. This function computes the terms of A - x^k * B * Q. This
;; rather specialised function is used to update a numerator when doing
;; polynomial long division.
(defun ptpt-subtract-powered-product (u v q k)
  (cond
    ;; A - x^k * 0 * Q = A
    ((null v) u)
    ;; 0 - x^k * B * Q = x^k * B * (- Q)
    ((null u) (pcetimes1 v k (pminus q)))
    (t
     ;; hipow is the highest exponent in x^k*B*Q.
     (let ((hipow (+ (pt-le v) k)))
       (cond
         ;; If hipow is greater than the highest exponent in A, we have to
         ;; prepend the first coefficient, which will be Q * lc(B). We can then
         ;; recurse to this function to sort out the rest of the sum.
         ((> hipow (pt-le u))
          (pcoefadd hipow
                    (ptimes q (pminus (pt-lc v)))
                    (ptpt-subtract-powered-product u (pt-red v) q k)))
         ;; If hipow is equal to the highest exponent in A, we can just subtract
         ;; the two leading coefficients and recurse to sort out the rest.
         ((= hipow (pt-le u))
          (pcoefadd hipow
                    (pdifference (pt-lc u) (ptimes q (pt-lc v)))
                    (ptpt-subtract-powered-product (pt-red u) (pt-red v) q k)))
         ;; If hipow is lower than the highest exponent in A then keep the first
         ;; term of A and recurse.
         (t
          (list* (pt-le u) (pt-lc u)
                 (ptpt-subtract-powered-product (pt-red u) v q k))))))))

(defun algord (var)
  (and $algebraic (get var 'algord)))

;; PSIMP
;;
;; Return a "simplified" polynomial whose main variable is VAR and whose terms
;; are given by X.
;;
;; If the polynomial is free of X, the result is the zero'th order coefficient:
;; either a polynomial in later variables or a number. PSIMP also deals with
;; reordering variables when $ALGEBRAIC is true, behaviour which is triggered by
;; the ALGORD property on the main variable.
(defun psimp (var x)
  (cond ((ptzerop x) 0)
	((atom x) x)
	((zerop (pt-le x)) (pt-lc x))
	((algord var)
         ;; Fix wrong alg ordering: We deal with the case that the main variable
         ;; of a coefficient should precede VAR.
         (do ((p x (cddr p)) (sum 0))
             ((null p)
              (if (pzerop sum)
                  (cons var x)
                  (pplus sum (p-delete-zeros var x))))
           ;; We only need to worry about the wrong ordering if a coefficient is
           ;; a polynomial in another variable, and that variable should precede
           ;; VAR.
           (unless (or (pcoefp (pt-lc p))
                       (pointergp var (p-var (pt-lc p))))
             (setq sum (pplus sum
                              (if (zerop (pt-le p)) (pt-lc p)
                                  (ptimes (make-poly var (pt-le p) 1)
                                          (pt-lc p)))))
             ;; When we finish, we'll call PPLUS to add SUM and the remainder of
             ;; X, and this line zeroes out this term in X (through P) to avoid
             ;; double counting. The term will be deleted by the call to
             ;; P-DELETE-ZEROS later.
             (setf (pt-lc p) 0))))

        (t
         (cons var x))))

;; P-DELETE-ZEROS
;;
;; Destructively operate on X, deleting any terms that have a zero coefficient.
(defun p-delete-zeros (var x)
  ;; The idea is that P always points one before the term in which we're
  ;; interested. When that term has zero coefficient, it is trimmed from P by
  ;; replacing the cdr. Consing NIL to the front of X allows us to throw away
  ;; the first term if necessary.
  (do ((p (setq x (cons nil x))))
      ((null (cdr p))
       ;; Switch off $algebraic so that we can recurse to PSIMP without any fear
       ;; of an infinite recursion - PSIMP only calls this function when (ALGORD
       ;; VAR) is true, and that only happens when $algebraic is true.
       (let (($algebraic)) (psimp var (cdr x))))
    (if (pzerop (pt-lc (cdr p)))
        (setf (cdr p) (pt-red (cdr p)))
        (setq p (cddr p)))))

;; PTTERM
;;
;; Given X representing the terms of a polynomial in a variable z, return the
;; coefficient of z^n.
(defun ptterm (x n)
  (do ((x x (pt-red x)))
      ((ptzerop x) (pzero))
    (cond ((< (pt-le x) n) (return (pzero)))
	  ((= (pt-le x) n) (return (pt-lc x))))))

(defun ptimes (x y)
  (cond ((pcoefp x) (if (pzerop x) (pzero) (pctimes x y)))
	((pcoefp y) (if (pzerop y) (pzero) (pctimes y x)))
	((eq (p-var x) (p-var y))
	 (palgsimp (p-var x) (ptimes1 (p-terms x) (p-terms y)) (alg x)))
	((pointergp (p-var x) (p-var y))
	 (psimp (p-var x) (pctimes1 y (p-terms x))))
	(t (psimp (p-var y) (pctimes1 x (p-terms y))))))

(defun   ptimes1 (x y-orig &aux uuu  )
  (do ((vvv (setq uuu (pcetimes1 y-orig (pt-le x) (pt-lc x))))
       (x (pt-red x) (pt-red x)))
      ((ptzerop x) uuu)
    (let ((y y-orig) (xe (pt-le x)) (xc (pt-lc x)))
      (prog (e u c) 
       a1 (cond ((null y) (return nil)))
       (setq e (+ xe (car y)))
       (setq c (ptimes (cadr y) xc))
       (cond ((pzerop c) (setq y (cddr y)) (go a1))
	     ((or (null vvv) (> e (car vvv)))
	      (setq uuu (setq vvv (ptptplus uuu (list e c))))
	      (setq y (cddr y)) (go a1))
	     ((= e (car vvv))
	      (setq c (pplus c (cadr vvv)))
	      (cond ((pzerop c)
		     (setq uuu (setq vvv (ptptdiffer uuu (list (car vvv) (cadr vvv))))))
		    (t (rplaca (cdr vvv) c)))
	      (setq y (cddr y))
	      (go a1)))
       a  
       (cond ((and (cddr vvv) (> (caddr vvv) e))
	      (setq vvv (cddr vvv)) (go a)))
       (setq u (cdr vvv ))
       b  (cond ((or (null (cdr u)) (< (cadr u) e))
		 (rplacd u (cons e (cons c (cdr u)))) (go e)))
       (cond ((pzerop (setq c (pplus (caddr u) c)))
	      (rplacd u (cdddr u)) (go d))
	     (t (rplaca (cddr u) c)))
       e  (setq u (cddr u))
       d  (setq y (cddr y))
       (cond ((null y) (return nil)))
       (setq e (+ xe (car y)))
       (setq c (ptimes (cadr y) xc))
       c  (cond ((and (cdr u) (> (cadr u) e)) (setq u (cddr u)) (go c)))
       (go b))))
  uuu)

(defun pcetimes1 (y e c)		;C*V^E*Y
  (loop for (exp coef) on y by #'cddr
	 unless (pzerop (setq coef (ptimes c coef)))
	 nconc (list (+ e exp) coef)))

(defun pctimes (c p)
  (if (pcoefp p) (ctimes c p)
      (psimp (p-var p) (pctimes1 c (p-terms p)))))

(defun pctimes1 (c terms)
  (loop for (exp coef) on terms by #'cddr
	 unless (pzerop (setq coef (ptimes c coef)))
	 nconc (list exp coef)))

(defun leadalgcoef (p)
  (cond ((pacoefp p) p)
	(t (leadalgcoef (p-lc p))) ))

(defun painvmod (q)
  (cond ((pcoefp q) (crecip q))
	(t (paquo (list (car q) 0 1) q ))))

(defun palgsimp (var p tell)		;TELL=(N X) -> X^(1/N)
  (psimp var (cond ((or (null tell) (null p)
			(< (car p) (car tell))) p)
		   ((null (cddr tell)) (pasimp1 p (car tell) (cadr tell)))
		   (t (pgcd1 p tell)) )))

(defun pasimp1 (p deg kernel)		;assumes deg>=(car p)
  (do ((a p (pt-red a))
       (b p a))
      ((or (null a) (< (pt-le a) deg))
       (rplacd (cdr b) nil)
       (ptptplus (pctimes1 kernel p) a))
    (rplaca a (- (pt-le a) deg))))

(defun monize (p) 
  (cond ((pcoefp p) (if (pzerop p) p 1))
	(t (cons (p-var p) (pmonicize (copy-list (p-terms p)))))))

(defun pmonicize (p)			;CLOBBERS POLY
  (cond ((equal (pt-lc p) 1) p)
	(t (pmon1 (painvmod (leadalgcoef (pt-lc p))) p) p)))

(defun pmon1 (mult l)
  (cond (l (pmon1 mult (pt-red l))
	   (setf (pt-lc l) (ptimes mult (pt-lc l))))))

(defun pmonz (poly &aux lc)		;A^(N-1)*P(X/A)
  (setq poly (pabs poly))       
  (cond ((equal (setq lc (p-lc poly)) 1) poly)
	(t (do ((p (p-red poly) (pt-red p))
		(p1 (make-poly (p-var poly) (p-le poly) 1))
		(mult 1)
		(deg (1- (p-le poly)) (pt-le p)))
	       ((null p) p1)
	     (setq mult (ptimes mult (pexpt lc (- deg (pt-le p)))))
	     (nconc p1 (list (pt-le p) (ptimes mult (pt-lc p))))))))

;;	THESE ARE ROUTINES FOR MANIPULATING ALGEBRAIC NUMBERS

(defun algnormal (p) (car (rquotient p (leadalgcoef p))))

(defun algcontent (p)
  (destructuring-let* ((lcf (leadalgcoef p))
		       ((prim . denom) (rquotient p lcf)))
    (list (ratreduce lcf denom) prim)))

(defun rquotient (p q &aux algfac* a e)	;FINDS PSEUDO QUOTIENT IF PSEUDOREM=0
  (cond ((equal p q) (cons 1 1))
	((pcoefp q) (ratreduce p q))
	((setq a (testdivide p q)) (cons a 1))
	((alg q) (rattimes (cons p 1) (rainv q) t))
	(t (cond ((alg (setq a (leadalgcoef q)))
		  (setq a (rainv a))
		  (setq p (ptimes p (car a)))
		  (setq q (ptimes q (car a)))
		  (setq a (cdr a)) ))
	   (cond ((minusp (setq e (+ 1 (- (cadr q)) (pdegree p (car q)))))
		  (rat-error "RQUOTIENT: Quotient by a polynomial of higher degree")))
	   (setq a (pexpt a e))
	   (ratreduce (or (testdivide (ptimes a p) q)
			  (prog2 (setq a (pexpt (p-lc q) e))
			      (pquotient (ptimes a p) q)))
		      a)) ))

(defun patimes (x r) (pquotientchk (ptimes x (car r)) (cdr r)))

(defun paquo (x y) (patimes x (rainv y)))

(defun mpget (var)
  (cond ((null (setq var (alg var))) nil)
	((cddr var) var)
	(t (list (car var) 1 0 (pminus (cadr var))))))


(defun rainv (q)
  (cond ((pcoefp q)
	 (cond (modulus (cons (crecip q) 1))
	       (t (cons 1 q))))
	(t (let ((var (car q)) (p (mpget q)))
	     (declare (special var))	;who uses this? --gsb
	     (cond ((null p) (cons 1 q))
		   (t (setq p (car (let ($ratalgdenom)
				     (bprog q (cons var p) var))))
		      (rattimes (cons (car p) 1) (rainv (cdr p)) t)))))))

(defun pexptsq (p n)
  (do ((n (ash n -1) (ash n -1))
       (s (if (oddp n) p 1)))
      ((zerop n) s)
    (setq p (ptimes p p))
    (and (oddp n) (setq s (ptimes s p))) ))

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 1.
