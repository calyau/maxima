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

(macsyma-module rpart)

;;;	Complex variable utilities
;;;
;;; Macsyma functions: $realpart $imagpart $rectform $polarform
;;;		       $cabs $carg
;;; Utility functions: trisplit risplit absarg cabs andmapc andmapcar

(load-macsyma-macros rzmac)

(declare-top (special $radexpand
		      $keepfloat))

;; $domain as it was when RISPLIT was entered.  RISPLIT binds $domain to
;; $complex while it works, so its helpers read this to learn whether the
;; user works in the real domain.
(defvar *risplit-domain* nil)

;; True when POW is a root with an integer numerator and an odd denominator,
;; as 1/3, 2/3 or 1/n with n declared odd.  With domain : real the
;; simplifier takes the real root of a real quantity raised to such a
;; power: (-8)^(1/3) simplifies to -2 and (x^3)^(1/3) to x.  An integer
;; power, with the denominator 1, is not one.
(defun odd-root-p (pow)
  (let ((den ($denom pow)))
    (and (not (eql den 1))
         (maxima-integerp ($num pow))
         (eq (evod den) '$odd))))

;; ODD-ROOT-P with the user's $domain real: a real quantity raised to the
;; power is then real, and csign, abs and signum agree with it.  With
;; domain : complex, or an even denominator, the power is on the principal
;; branch and may be complex.
(defun real-odd-root-p (pow)
  (and (eq (or *risplit-domain* $domain) '$real) (odd-root-p pow)))

;; The functions below write z^s on the principal branch for a z = k*w^n
;; with a constant k, a real w and a rational n, where with domain : real
;; the simplifier takes the real root instead, as (-x^3)^(-1/3) to -1/x:
;; the power of z that comes with gamma_incomplete(a, z), in the
;; antiderivatives of INTEGRATE-EXP-SPECIAL, in the derivative and in the
;; recurrences of gamma_expand, is on the principal branch as the function
;; is.  The argument of z is that of k for a positive w^n and that of -k
;; for a negative one, so
;;
;;   z^s = abs(k)^s * abs(w)^(n*s) * (alpha + beta*w/abs(w))
;;
;; with alpha + beta the phase %e^(%i*s*carg(k)) and alpha - beta the
;; phase %e^(%i*s*carg(-k)).  The sign w/abs(w) and the powers of abs(w)
;; are rational in w and abs(w), abs(w)^2 simplifies to w^2 and the sign
;; has that one form, so that the product of two such powers, as of an
;; antiderivative and of its derivative, reduces under expand or ratsimp,
;; where an atan2(0, w) in an exponent would not.

;; %e^(%i*s*arg(z)) as alpha + beta*SIGMA for SIGMA the sign w/abs(w) of w
;; in z = k*w^n, with RE and IM the real and imaginary parts of k.  The
;; constants are expanded to give them one form wherever they come from,
;; as one instance has to cancel another.
(defun principal-phase (s re im sigma)
  (let ((plus (power '$%e (mul '$%i s (take '(%atan2) im re))))
        (minus (power '$%e (mul '$%i s (take '(%atan2) (neg im) (neg re))))))
    (add ($expand (div (add plus minus) 2))
         (mul ($expand (div (sub plus minus) 2)) sigma))))

;; The integrator substitutes an internal variable for an even root and
;; declares it complex, as the root is complex for a negative radicand;
;; where the integrand is real, the variable is a nonnegative real.
(defun nonneg-internal-p (w)
  (and (symbolp w) (get w 'internal) (kindp w '$complex)))

;; Z as k*w^n, for a constant k and a real w that is not constant, with n
;; an integer or a rational number: (values k w n nonneg), NONNEG saying
;; that w is known to be nonnegative; nil when z is not of this form, or
;; when n is 1 and k positive, where the simplifier leaves z^s as k^s*w^s
;; and commits to no branch.
(defun real-power-factors (z)
  (let ((k 1) (w z) (n 1))
    (when (mtimesp z)
      (let ((consts (remove-if-not #'$constantp (cdr z))))
        (when consts
          (setq k (muln consts t)
                w (muln (remove-if #'$constantp (cdr z)) t)))))
    (when (and (mexptp w) (or (integerp (caddr w)) (ratnump (caddr w))))
      (setq n (caddr w) w (cadr w)))
    (cond (($constantp w) nil)
          ((and (eql n 1) (member ($csign k) '($pos $pz))) nil)
          ((nonneg-internal-p w) (values k w n t))
          ((member ($csign w) '($complex $imaginary)) nil)
          (t (values k w n nil)))))

;; True when the sign of w^n, for a real w, is that of w: n is an odd
;; integer, or with domain : real an odd numerator over an odd denominator,
;; where the root is the real one.  Otherwise w^n is nonnegative wherever
;; it is real.
(defun sign-varies-p (n)
  (and (oddp ($num n)) (oddp ($denom n))))

;; z^s on the principal branch, as above, or Z^S itself where the
;; treatment does not apply.
(defun principal-power (z s)
  (multiple-value-bind (k w n nonneg) (real-power-factors z)
    (if (and k (odd-root-p s))
        (destructuring-bind (re . im) (trisplit k)
          (let ((qodd (and (not nonneg) (oddp ($denom n)))))
            (mul (power (cabs k) s)
                 (if qodd
                     (power (take '(mabs) w) (mul n s))
                     (power (power w n) s))
                 (principal-phase s re im
                                  (if (and qodd (sign-varies-p n))
                                      (div w (take '(mabs) w))
                                      1)))))
        (power z s))))

;; w^m * z^s for the integrator, multiplied through the phase for an
;; integer m, so that the sign of w^m folds into it: the sign of a real w
;; has the one form w/abs(w), whose square is 1.
(defun principal-power-times (w m z s)
  (let ((p (mul (power w m) (principal-power z s))))
    (if (integerp m) ($multthru p) p)))

;; z^s on the principal branch as a template in the symbol Z-NAME for the
;; derivative of gamma_incomplete, which SDIFFGRAD fills in by substituting
;; the argument for the symbol; unsimplified, as (-z^2)^(s/2) for an
;; imaginary z would not survive the simplifier with the symbol in it.
;; The modulus is abs(k)^s * (z^2/k^2)^(s/2), where z^2/k^2 is w^(2*n):
;; the constant stays outside the root, in the form the integrator gives
;; it, so that the two cancel.  The sign of w is that of z times that of
;; k.  Nil where the treatment does not apply, or where k has the symbol a
;; or Z-NAME in it, which the substitution would replace.
(defun principal-power-template (z z-name s)
  (multiple-value-bind (k w n nonneg) (real-power-factors z)
    (declare (ignore w))
    (when (and k (odd-root-p s) ($freeof '$a k) ($freeof z-name k))
      (destructuring-bind (re . im) (trisplit k)
        (list '(mtimes)
              (power (cabs k) s)
              (list '(mexpt)
                    (list '(mtimes) (inv (power k 2))
                          (list '(mexpt) z-name 2))
                    (div s 2))
              (principal-phase s re im
                               (if (or nonneg (not (sign-varies-p n)))
                                   1
                                   (mul (div (cabs k) k)
                                        (div z-name
                                             (take '(mabs) z-name))))))))))

;;; Realpart gives the real part of an expr.

(defun risplit-signum (x) ;rectangular form for a signum expression
  (let*  ((z (risplit (cadr x))) (a (car z)) (b (cdr z)) (r)) ;signum(a+%i b), where a and b are real
    (cond ((eq t (meqp b 0)) ;signum(a) -> signum(a) + 0 %i
	   (cons (take '(%signum) a) 0))
	  ((or (eq t (mnqp a 0)) (eq t (mnqp b 0))) ;signum(a + %i b) --> a/sqrt(a^2+b^2) + %i b/sqrt(a^2+b^2)
	   (setq r (take '(%sqrt) (add (power a 2) (power b 2))))
	   (cons (div a r) (div b r)))
	  (t (cons (take '(%realpart) x) (take '(%imagpart) x)))))) ;nothing known

(setf (get '%signum 'risplit-function) 'risplit-signum)

(defmfun $realpart (xx) (car (trisplit xx)))

(def-simplifier (realpart :custom-defmfun t
                          ;; DO NOT set the ALIAS and REVERSEALIAS
                          ;; properties for this simplifier.  It
                          ;; causes failures in the testsuite.
                          :skip-properties (alias reversealias))
    (z)
  (let ((sgn nil))
    (cond ((mnump z) z)
          ((eq (setq sgn ($csign z)) '$imaginary)
           0)
          ((eq sgn '$complex)
           (let ((z-expanded ($expand z)))
           (cond ((complex-number-p z-expanded 'bigfloat-or-number-p)
                  ($realpart z-expanded))
                 (t
                  (give-up)))))
          (t
           (give-up)))))

;;; Imagpart gives the imaginary part of an expr.

(defmfun $imagpart (xx) (cdr (trisplit xx)))

(def-simplifier (imagpart :custom-defmfun t
                          ;; DO NOT set the ALIAS and REVERSEALIAS
                          ;; properties for this simplifier.  It
                          ;; causes failures in the testsuite.
                          :skip-properties (alias reversealias))
    (z)
  (let ((sgn nil))
    (cond ((mnump z) 0)
          ((eq (setq sgn ($csign z)) '$imaginary)
           (mul -1 '$%i z))
          ((eq sgn '$complex)
           (let ((z-expanded ($expand z)))
           (cond ((complex-number-p z-expanded 'bigfloat-or-number-p)
                  ($imagpart z-expanded))
                 (t
                  (give-up)))))
          (t
           (give-up)))))

;;; Rectform gives a result of the form a+b*%i.

(defmfun ($rectform :properties ((evfun t))) (xx)
  (let ((ris (trisplit xx)))
    (add (car ris) (mul (cdr ris) '$%i))))

;;; Polarform gives a result of the form a*%e^(%i*b).

(defmfun ($polarform :properties ((evfun t))) (xx)
  (cond ((mbagp xx)
	 (cons (car xx) (mapcar #'$polarform (cdr xx))))
	(t
	 (let ((aas (absarg xx)) ($%emode nil))
	   (mul (car aas) (powers '$%e (mul '$%i (cdr aas))))))))

;;; Cabs gives the complex absolute value.  Nota bene: an expression may
;;; be syntactically real without being real (e.g. sqrt(x), x<0).  Thus
;;; Cabs must lead an independent existence from Abs.

;; The internal cabs, used by other Macsyma programs.
(defun cabs (xx)
  (car (absarg xx t)))

(def-simplifier cabs (z)
  (if (and (consp z) (eq (caar z) 'mabs))
  (ftake '%cabs (cadr z)) ; cabs(abs(x)) = cabs(x)
  (let ((sgn ($csign z)))
    (cond ((member sgn '($complex $imaginary))
           (cabs z))
          ((eq sgn '$zero)
           0)
          ((member sgn '($pos $pz))
           z)
          ((eq sgn '$neg)
           (mul -1 z))
          (t
           (cabs z))))))

;;; Carg gives the complex argument.

(def-simplifier carg (z)
  (let ((sgn nil))
    (labels
        ((carg (xx)
           (cond ((mbagp xx)
	          (cons (car xx) (mapcar #'carg (cdr xx))))
	         (t (cdr (absarg xx))))))
      (cond ((eq z '$%i)
             (div '$%pi 2))
            ((member (setq sgn ($csign z)) '($complex $imaginary))
             (carg z))
            ((member sgn '($pos $pz $zero))
             0)
            ((eq sgn '$neg)
             '$%pi)
            ((eq sgn '$pnz)
             (carg z))
            (t
             (give-up))))))

;; Some objects can only appear at the top level of a legal simplified
;; expression: CRE forms and equations in particular.

(defun trisplit (el) ; Top level of risplit
  (cond ((atom el) (risplit el))
	((specrepp el) (trisplit (specdisrep el)))
	((eq (caar el) 'mequal) (dot-sp-ri (cdr el) '(mequal simp)))
	(t (risplit el))))

;;; Auxiliaries

;; These are Macsyma equivalents to (mapcar 'trisplit ...).  They must
;; differ from other maps for two reasons: the lists are Macsyma lists,
;; and therefore prefixed with list indicators; and the results must
;; be separated: ((a . b) (c . d)) becomes something like ([a,c].[b,d]).

(defun dsrl (el) (dot-sp-ri (cdr el) '(mlist simp)))

(defun dot-sp-ri (el ind)
  (dot--ri (mapcar #'trisplit el) ind))

;; Dot--ri does the ((a.b)(c.d))->([a,c].[b,d]) transformation with
;; minimal Cons'ing.

(defun dot--ri (el ind)
  (do ((i el (cdr i)) (k))
      ((null i) (cons (cons ind (nreverse k)) (cons ind el)))
    (let ((cdari (cdar i)))
      (setq k (rplacd (car i) k))
      (rplaca i cdari))))

(defun risplit-mplus (l)
  (do ((rpart) (ipart) (m (cdr l) (cdr m)))
      ((null m) (cons (addn rpart t) (addn ipart t)))
    (let ((sp (risplit (car m))))
      (cond ((=0 (car sp)))
	    (t (setq rpart (cons (car sp) rpart))))
      (cond ((=0 (cdr sp)))
	    (t (setq ipart (cons (cdr sp) ipart)))))))

(defun risplit-times (l)
  (let ((risl (do ((purerl nil)
		   (compl nil)
		   (l (cdr l) (cdr l)))
		  ((null l) (cons purerl compl))
		(let ((sp (risplit (car l))))
		  (cond ((=0 (cdr sp))
			 (setq purerl (rplacd sp purerl)))
			((or (atom (car sp)) (atom (cdr sp)))
			 (setq compl (cons sp compl)))
			((and (eq (caaar sp) 'mtimes)
;;;Try risplit z/w and notice denominator.  If this check were not made,
;;; the real and imaginary parts would not each be over a common denominator.
			      (eq (caadr sp) 'mtimes)
			      (let ((nr (nreverse (cdar sp)))
				    (ni (nreverse (cddr sp))))
				(cond ((equal (car nr) (car ni))
				       (push (car nr) purerl)
				       (push (cons (muln (nreverse (cdr nr)) t)
						   (muln (nreverse (cdr ni)) t))
					     compl))
				      (t
				       (setq nr (nreverse nr))
				       (setq ni (nreverse ni))
				       nil)))))
			(t
			 (push sp compl)))))))
    (cond ((null (cdr risl))
	   (cons (muln (car risl) t) 0))
	  (t
	   (do ((rpart 1) (ipart 0) (m (cdr risl) (cdr m)))
	       ((null m)
		(cons (if (=0 rpart) 0 (muln (cons rpart (car risl)) t))
		      (if (=0 ipart) 0 (muln (cons ipart (car risl)) t))))
	     (psetq rpart (sub (mul rpart (caar m)) (mul ipart (cdar m)))
		    ipart (add (mul ipart (caar m)) (mul rpart (cdar m)))))))))

;; Split L = ((mexpt) BASE POW) into real and imaginary parts.
(defun risplit-expt (l)
  (let* ((base (cadr l)) (pow (caddr l))
         ;; Disable 'simplifications' like sqrt(-x) -> %i*sqrt(x)
         ($radexpand nil)
         (sp (risplit base)))
    (cond
      ((fixnump pow)
       (risplit-expt-fixnum-pow sp pow))

      ((and (=0 (cdr sp)) (real-odd-root-p pow))
       ;; A real base to a rational power with an odd denominator is real
       ;; with domain : real.  Simplify the power in that domain, since
       ;; RISPLIT binds $domain to $complex.
       (cons (let (($domain '$real)) (power (car sp) pow)) 0))

      ((and (ratnump pow)
            (fixnump (cadr pow))
            (not (< (cadr pow) (- $maxnegex)))
            (not (> (cadr pow) $maxposex))
            (or (= (caddr pow) 2) (=0 (cdr sp))))
       (if (=0 (cdr sp))
           (risplit-expt-real^rat base pow)
           (risplit-expt-sqrt-pow base sp pow)))

      ((and (or (floatp pow) ($bfloatp pow)) (spcomplexnump sp))
       (risplit-expt-inexact-pow sp pow))

      ((and (floatp base) (floatp pow))
       (risplit (let (($numer t)) (exptrl base pow))))

      (t
       (destructuring-bind (alpha . beta) (risplit pow)
         (destructuring-bind (r . theta) (absarg1 base)
           (risplit-expt-general-form r theta alpha beta)))))))

;; Split BASE^POWER into real and imaginary parts. POWER is assumed to be a
;; fixnum. SP is (RISPLIT BASE)
(defun risplit-expt-fixnum-pow (sp power)
  ;; We use the squared absolute value of BASE several times
  ;; below. Unfortunately, we can't calculate it at the start, since that causes
  ;; floating point under/overflows in the case mentioned in the comment
  ;; below. Instead, we calculate it when it's needed (a maximum of once).
  (destructuring-bind (real . imag) sp
    (cond
      ((= power -1)
       ;; Handle the case of 1/(x+%i*y) carefully.  This
       ;; is needed if x and y are (Lisp) numbers to
       ;; prevent spurious underflows/overflows. See bug 1908.
       (if (and (or (numberp real) (ratnump real))
                (or (numberp imag) (ratnump imag)))
           (sprecip sp)
           (let ((abs2 (spabs sp)))
             (cons (div real abs2) (mul -1 (div imag abs2))))))

      ((and (> (abs power) $maxposex) (not (spinexactp sp)))
       (if (=0 imag)
           (cons (powers real power) 0)
           (let ((abs^n (powers (spabs sp) (*red power 2)))
                 (natan (mul power (genatan imag real))))
             (cons (mul abs^n (take '(%cos) natan))
                   (mul abs^n (take '(%sin) natan))))))

      ((> power 0)
       (spintexpt sp power))

      (t
       (let ((abbas (powers (spabs sp) (- power)))
             (basspli (spintexpt sp (- power))))
         (cons (div (car basspli) abbas)
               (neg (div (cdr basspli) abbas))))))))

;; Return the "general form" for RISPLIT applied to
;; (r*exp(%i*theta))^(alpha+%i*beta), whose rectform is
;;
;;   pre * cos(post) + %i * pre * sin(post)
;;
;; where pre  = exp(-theta*beta) * r^alpha
;; and   post = beta*log(r) + alpha*theta
(defun risplit-expt-general-form (r theta alpha beta)
  (let ((pre (mul (powers '$%e (mul -1 theta beta))
                  (powers r alpha)))
        (post (add (mul beta (take '(%log) r))
                   (mul alpha theta))))
    (cons (mul pre (take '(%cos) post))
          (mul pre (take '(%sin) post)))))

;; Split BASE^POW into real and imaginary parts. SP is (RISPLIT BASE) and is
;; assumed to be a complex number, POW is assumed to be a float or a bigfloat.
(defun risplit-expt-inexact-pow (sp pow)
  (let ((sp (if (or ($bfloatp (car sp)) ($bfloatp (cdr sp)) ($bfloatp pow))
                (cons ($bfloat (car sp)) ($bfloat (cdr sp)))
                (cons ($float (car sp)) ($float (cdr sp)))))
        (n (integer-representation-p pow)))
    (if n
      ;; BASE^POW is the integer power BASE^N, whose argument is N times the
      ;; argument of BASE. Multiplying that out spends the leading digits of the
      ;; product on a multiple of 2*%pi, so raise BASE to the N instead.
      (risplit-expt-fixnum-pow sp n)
      (destructuring-bind (r . theta) (absarg1 (add (car sp) (mul '$%i (cdr sp))))
        (risplit-expt-general-form r theta pow 0)))))

;; Split BASE^POWER into real and imaginary parts. We assume that BASE is real
;; and that POWER is a rational number.
(defun risplit-expt-real^rat (base power)
  (case (cond ((mnegp base) '$neg)
              (implicit-real '$pos)
              (t ($sign base)))    ; Use $sign not asksign
    ($neg (risplit-expt-general-form (neg base) '$%pi power 0))
    ($zero (cons (power 0 power) 0))
    ($pos (cons (power base power) 0))
    (t
     (destructuring-bind (r . theta) (absarg1 base)
       (risplit-expt-general-form r theta power 0)))))

;; Split BASE^POWER into real and imaginary parts. SP is (RISPLIT BASE). We
;; assume that POWER is a rational number. Moreover, we assume that the
;; denominator of POWER is 2.
(defun risplit-expt-sqrt-pow (base sp power)
  ;; n = abs(2*power) is a non-negative integer
  (destructuring-bind (real . imag) sp
    (let* ((abs2 (spabs sp)) (abs (power abs2 1//2))
           (n (abs (cadr power)))
           (pos? (> (cadr power) -1))
           (imag-sign ($sign imag)))
      (cond
        ((member imag-sign '($neg $pos))
         ;; Here, we use the half-angle formulas for cos and sin. Assuming we
         ;; are always taking the "principal square root" (that with argument
         ;; less than equal to the argument of base), these come out as
         ;;
         ;;   cos(arg/2) = +- sqrt((1+real/abs)/2)
         ;;   sin(arg/2) = +- sqrt((1-real/abs)/2)
         ;;
         ;; We know that real+%i*imag = abs*exp(%i*arg). Taking square roots,
         ;; you get that
         ;;
         ;;   sqrt(real+%i*imag) = sqrt(abs)*exp(%i*arg/2).
         ;;                      = sqrt(abs)*cos(arg/2) +
         ;;                           %i * sqrt(abs)*sin(arg/2)
         ;;                      = (sqrt(abs+real) + %i*sqrt(abs-real))/sqrt(2)
         ;;
         ;; but possibly with signs on the square roots. This function always
         ;; chooses the square root with the non-negative real part. As such, we
         ;; have to switch the sign of the sine term when we are raising to a
         ;; positive power and imag < 0 or if raising to a negative power and
         ;; imag > 0. To see that the first argument of the PORM call below is
         ;; correct, write out the 2x2 truth table...
         (divcarcdr
          (spintexpt
           (cons (power (add abs real) 1//2)
                 (porm (eq (eq imag-sign '$pos) pos?)
                       (power (sub abs real) 1//2)))
           n)
          (if pos?
              (power 2 (div n 2))
              (power (mul 2 abs2) (div n 2)))))

        (t
         (destructuring-bind (alpha . beta) (risplit power)
           (destructuring-bind (r . theta) (absarg1 base)
             (risplit-expt-general-form r theta alpha beta))))))))

(defun risplit-noun (l)
  (cons (simplify (list '(%realpart) l)) (simplify (list '(%imagpart) l))))


(defun absarg1 (arg)
  (let ((arg1 arg) ($keepfloat t))
    (cond ((and (or (free arg '$%i)
		    (free (setq arg1 (sratsimp arg)) '$%i))
		(not (eq (csign arg1) t)))
	   (setq arg arg1)
	   (if implicit-real
	       (cons arg 0)
	       (let ((fact (assume `(($notequal) ,arg 0))))
	       (unwind-protect
			(absarg arg)
			(forget fact)))))
	  (t (absarg arg)))))

;;;	Main function
;;; Takes an expression and returns the dotted pair
;;; (<Real part> . <imaginary part>).

(defun risplit (l)
  (let* ((*risplit-domain* (or *risplit-domain* $domain))
         ($domain '$complex) ($m1pbranch t) $logarc op)
    (cond ((atom l)
           ;; Symbols are assumed to represent real values, unless they have
           ;; been declared to be complex. If they have been declared to be both
           ;; real and complex, they are taken to be real.
	   (cond ((eq l '$%i) (cons 0 1))
		 ((eq l '$infinity) (cons '$und '$und))
		 ((and (decl-complexp l) (not (decl-realp l))) (risplit-noun l))
		 (t (cons l 0))))
	  ((eq (caar l) 'rat) (cons l 0))
	  ((eq (caar l) 'mplus) (risplit-mplus l))
	  ((eq (caar l) 'mtimes) (risplit-times l))
	  ((eq (caar l) 'mexpt) (risplit-expt l))
	  ((eq (caar l) '%log)
	   (let ((aa (absarg1 (cadr l))))
	     (rplaca aa (take '(%log) (car aa)))))
	  ((eq (caar l) 'bigfloat) (cons l 0)) ;All numbers are real.
	  ((and (member (caar l) '(%integrate %derivative %laplace %sum) :test #'eq)
		(freel (cddr l) '$%i))
	   (let ((ris (risplit (cadr l))))
	     (cons (simplify (list* (ncons (caar l)) (car ris) (cddr l)))
		   (simplify (list* (ncons (caar l)) (cdr ris) (cddr l))))))
          ((eq (caar l) '$conjugate)
           (cons (simplify (list '(%realpart) (cadr l)))
                 (mul -1 (simplify (list '(%imagpart) (cadr l))))))
	  ((let ((ass (assoc (caar l)
			     '((%sin %cosh %cos . %sinh)
			       (%cos %cosh %sin . %sinh)
			       (%sinh %cos %cosh . %sin)
			       (%cosh %cos %sinh . %sin)) :test #'eq)))
;;;This clause handles the very similar trigonometric and hyperbolic functions.
;;; It is driven by the table at the end of the lambda.
	     (and ass
		  (let ((ri (risplit (cadr l))))
		    (cond ((=0 (cdr ri)) ;Pure real case.
			   (cons (take (list (car ass)) (car ri)) 0))
			  (t
			   (cons (mul (take (list (car ass)) (car ri))
				      (take (list (cadr ass)) (cdr ri)))
				 (negate-if (eq (caar l) '%cos)
					    (mul (take (list (caddr ass)) (car ri))
						 (take (list (cdddr ass)) (cdr ri)))))))))))
	  ((member (caar l) '(%tan %tanh) :test #'eq)
	   (let ((sp (risplit (cadr l))))
;;;The similar tan and tanh cases.
	     (cond ((=0 (cdr sp))
		    (cons l 0))
		   (t
		    (let* ((2rl (mul (car sp) 2))
			   (2im (mul (cdr sp) 2))
			   (denom (inv (if (eq (caar l) '%tan)
					   (add (take '(%cosh) 2im) (take '(%cos) 2rl))
					   (add (take '(%cos) 2im) (take '(%cosh) 2rl))))))
		      (if (eq (caar l) '%tan)
			  (cons (mul (take '(%sin) 2rl) denom)
				(mul (take '(%sinh) 2im) denom))
			  (cons (mul (take '(%sinh) 2rl) denom)
				(mul (take '(%sin) 2im) denom))))))))
	  ((and (member (caar l) '(%atan %csc %sec %cot %csch %sech %coth) :test #'eq)
		(=0 (cdr (risplit (cadr l)))))
	   (cons l 0))
          ((and (eq (caar l) '%atan2)
                (not (zerop1 (caddr l)))
                (=0 (cdr (risplit (div (cadr l) (caddr l))))))
           ;; Case atan2(y,x) and y/x a real expression.
           (cons l 0))
	  ((or (arcp (caar l)) (eq (caar l) '%atan2))
	   (let ((ans (risplit (logarc (caar l)
				       ;; atan2 has 2 args, unlike all
				       ;; the other inverse trig
				       ;; functions.
				       (if (eq (caar l) '%atan2)
					   (rest l)
					   (cadr l))))))
	     (when (eq (caar l) '%atan2)
	       (setq ans (cons (sratsimp (car ans)) (sratsimp (cdr ans)))))
	     (if (and (free l '$%i) (=0 (cdr ans)))
		 (cons l 0)
		 ans)))
	  ((eq (caar l) '%plog)
	   ;;  (princ '|Warning: Principal value not guaranteed for Plog in Rectform/|)
	   (risplit (cons '(%log) (cdr l))))
	  ;; Look for a risplit-function on the property list to handle the
	  ;; realpart and imagpart for this function.
          ((setq op (safe-get (mop l) 'risplit-function))
	   (funcall op l))
;;; ^ All the above are guaranteed pure real.
;;; The handling of lists and matrices below has to be thought through.
	  ((eq (caar l) 'mlist) (dsrl l))
	  ((eq (caar l) '$matrix)
	   (dot--ri (mapcar #'dsrl (cdr l)) '($matrix simp)))
;;;The Coversinemyfoot clause covers functions which can be converted
;;; to functions known by risplit, such as the more useless trigonometrics.
	  ((let ((foot (coversinemyfoot l)))
	     (and foot (risplit foot))))
          ((or (safe-get (mop l) 'real-valued)
               (decl-realp (mop l)))
           ;; Simplification for a real-valued function
           (cons l 0))
      ((and (or (safe-get (mop l) 'commutes-with-conjugate)
                (safe-get (mop l) 'conjugate-function))
       ;; An operator that $conjugate may be able to simplify.
       ;; If $conjugate simplifies l to something not involving '$conjugate,
       ;; use Re(z) = (conjugate(z) + z) / 2, Im(z) = %i * (conjugate(z) - z) / 2.
       ;; Possible improvement: Ignore instances of '$conjugate already occurring
       ;; in the original expression l.
       (let* ((conjugate (ftake '$conjugate l)))
         (when (freeof '$conjugate conjugate)
           (cons (mul (div 1 2) (add conjugate l))
                 (mul (div 1 2) '$%i (sub conjugate l)))))))
;;; A MAJOR ASSUMPTION:
;;;  All random functions are pure real, regardless of argument.
;;;  This is evidently assumed by some of the integration functions.
;;;  Perhaps the best compromise is to return 'realpart/'imagpart
;;;   under the control of a switch set by the integrators.  First
;;;   all such dependencies must be found in the integ
	  ((and rp-polylogp (mqapplyp l) (eq (subfunname l) '$li)) (cons l 0))
	  ((prog2 (setq op (if (eq (caar l) 'mqapply) (caaadr l) (caar l)))
	       (decl-complexp op))
	   (risplit-noun l))
	  ((and (eq (caar l) '%product) (not (free (cadr l) '$%i)))
	   (risplit-noun l))
          (($subvarp l)
           ;; return a real answer for subscripted variable
           (cons l 0))
          (t
           (cons (list '(%realpart simp) l)
                 (list '(%imagpart simp) l))))))

(defun coversinemyfoot (l)
  (prog (recip)
     (cond ((not (member (caar l) '(%csc %sec %cot %csch %sech %coth) :test #'eq)))
	   ((null (setq recip (get (caar l) 'recip))))
	   (t (return (div 1 (cons (list recip) (cdr l))))))))

(defun powers (c d)
  (cond ((=1 d) c)
	((equal d 0) 1)		      ;equal to preclude 0^(pdl 0)->0:
	((=0 c) 0)			; see comment before =0.
	((=1 c) 1)
	(t (power c d))))

(defun spabs (sp)
  ;; SP is a cons of the real part and imaginary part of a complex
  ;; number.  SPABS computes the sum of squares of the real and
  ;; imaginary parts.
  (add (powers (car sp) 2)
       (powers (cdr sp) 2)))

;; Compute 1/(x+%i*y) when both x and y are Lisp numbers or Maxima
;; rationals.  Return a cons of the real and imaginary part of the
;; result.  We count on the underlying Lisp to be able to compute (/
;; (complex x y)) accurately and without unnecessary overflow or
;; underflow..  If not, complain to your Lisp vendor.  Well, it seems
;; that Clisp, CMUCL, and SBCL do a nice job.  But others apparently
;; do not.  (I tested ecl 9.12.3 and ccl 1.4, which both fail.)
;; Workaround those deficiencies.
(defun sprecip (sp)
  (destructuring-bind (x . y)
      sp
    #+(or cmu sbcl)
    (let* ((x (bigfloat:to x))
	   (y (bigfloat:to y))
	   (q (bigfloat:/ (bigfloat:complex x y))))
      (cons (to (bigfloat:realpart q))
	    (to (bigfloat:imagpart q))))
    #-(or cmu sbcl)
    (let ((x (bigfloat:to x))
	  (y (bigfloat:to y)))
      ;; 1/(x+%i*y).
      ;;
      ;; Assume abs(x) > abs(y).  Let r = y/x.  Then
      ;; 1/(x+%i*y) = 1/x/(1+%i*r)
      ;;            = (1-%i*r)/(x*(1+r*r))
      ;;
      ;; The case for abs(x) <= abs(y) is similar with r = x/y:
      ;; 1/(x+%i*y) = 1/y/(r+%i)
      ;;            = (r-%i)/(y*(1+r^2))
      (if (> (bigfloat:abs x) (bigfloat:abs y))
	  (let* ((r (bigfloat:/ y x))
		 (dn (bigfloat:* x (bigfloat:+ 1 (bigfloat:* r r)))))
	    (cons (to (bigfloat:/ dn))
		  (to (bigfloat:/ (bigfloat:- r) dn))))
	  (let* ((r (bigfloat:/ x y))
		 (dn (bigfloat:* y (bigfloat:+ 1 (bigfloat:* r r)))))
	    (cons (to (bigfloat:/ r dn))
		  (to (bigfloat:/ (bigfloat:- dn)))))))))
      
  


(defvar negp* (let ((l (list nil nil t t))) (nconc l l)))

(defun divcarcdr (a b)
  (cons (div (car a) b) (div (cdr a) b)))


(defun spcomplexnump (sp)
  "Is SP, a (<real part> . <imaginary part>) pair, a complex number?"
  (and (mnump (car sp))
       (mnump (cdr sp))
       (not (zerop1 (cdr sp)))))

(defun spinexactp (sp)
  "Is SP a complex number with a float or a bigfloat part?"
  (and (spcomplexnump sp)
       (or (floatp (car sp))
           (floatp (cdr sp))
           ($bfloatp (car sp))
           ($bfloatp (cdr sp)))))

(defun spmult (sp1 sp2)
  "Multiply SP1 and SP2, both (<real part> . <imaginary part>) pairs"
  (cons (sub (mul (car sp1) (car sp2)) (mul (cdr sp1) (cdr sp2)))
        (add (mul (car sp1) (cdr sp2)) (mul (cdr sp1) (car sp2)))))

(defun spintexpt (base n)
  "Compute base^N, where base is (<real part> . <imaginary part>), and N is a
  positive integer"
  (if (and (spinexactp base) (> n 0))
    ;; The binomial expansion in EXPANINTEXPT is exact as long as BASE is,
    ;; but its terms are larger than their sum by a factor of about
    ;; binomial(n, n/2), so for an inexact BASEBASE the cancellation eats every
    ;; significant digit. Use exponentiation by squaring instead, which costs
    ;; only the rounding error of 2*log2(n) multiplications.
    (do ((acc base)
         (i (- (integer-length n) 2) (1- i)))
        ((< i 0) acc)
      (setq acc (spmult acc acc))
      (when (logbitp i n) (setq acc (spmult acc base))))
    (expanintexpt base n)))

;;Expand bas^n, where bas is (<real part> . <imaginary part>)

(defun expanintexpt (bas n)
  (cond ((= n 1) bas)
	(t (do ((rp (car bas))
		(ip (cdr bas))
		(c 1 (quotient (* c ex) i))
		(ex n (1- ex)) (i 1 (1+ i))
		(rori t (not rori)) (negp negp* (cdr negp))
		(rpt nil) (ipt nil))
	       ((< ex 0) (cons (addn rpt t) (addn ipt t)))
	     (declare (fixnum ex i))
	     (set-either rpt ipt
			 rori
			 (cons (negate-if (car negp)
					  (mul c
					       (powers rp ex)
					       (powers ip (1- i))))
			       (cond (rori rpt) (t ipt))))))))



;;;   Subtract out multiples of 2*%pi with a minimum of consing.
;;;   Attempts to reduce to interval (-pi,pi].

(defun 2pistrip (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mtimes)
	 (cond ((and (mnump (cadr exp))
		     (eq (caddr exp) '$%pi)
		     (null (cdddr exp)))
		(cond ((integerp (cadr exp))	; 5*%pi
		       (mul (mod (cadr exp) 2) '$%pi))
		      ((floatp (cadr exp))	; 1.5*%pi
		       (mul (1- (mod (1+ (cadr exp)) 2))
			    '$%pi))
		      ;; Neither 0 nor 1 appears as a coef
		      ((and (listp (cadr exp))
			    (eq 'rat (caaadr exp))) ;5/2*%pi
		       (mul (list* '(rat simp)
				   (- (mod (+ (cadadr exp) (car (cddadr exp)))
					   (* 2 (car (cddadr exp))))
				      (car (cddadr exp)))
				   (cddadr exp))
			    '$%pi))
		      (t exp)))
	       (t exp)))
	((eq (caar exp) 'mplus)
	 (let ((res (2pirec (cdr exp))))
	   (if (eq res (cdr exp))
	       exp
	       (addn res t))))
	(t exp)))

(defun 2pirec (fm)			;Takes a list of exprs
  (cond ((null (cdr fm))		;If monad, just return.
	 (let ((2pf (2pistrip (car fm))))
	   (cond ((eq 2pf (car fm)) fm)
		 ((=0 2pf) nil)
		 (t (list 2pf)))))
	(t
	 (let ((2pfma (2pistrip (car fm)))
	       (2pfmd (2pirec (cdr fm))))
	   (cond ((or (null 2pfmd) (=0 2pfmd)) (list 2pfma))
		 ((and (eq 2pfmd (cdr fm)) (eq 2pfma (car fm))) fm)
		 (t (cons 2pfma 2pfmd)))))))

;;;	Rectify into polar form; Arguments similar to risplit

(defun argnum (n)
  (if (minusp n)
      (simplify '$%pi)
      0))


;; absarg
;; returns pair (abs . arg)
;; if absflag is true, arg result is not guaranteed to be correct

;; The function of Absflag is to communicate that only the absolute
;; value part of the result is wanted.  This allows Absarg to avoid asking
;; questions irrelevant to the absolute value.  For instance, Cabs(x) is
;; invariably Abs(x), while the complex phase may be 0 or %pi.  Note also
;; the steps taken in Absarg to assure that Asksign's will happen before Sign's
;; as often as possible, so that, for instance, Abs(x) can be simplified to
;; x or -x if the sign of x must be known for some other reason.  These
;; techniques, however, are not perfect.

(defmacro half () ''((rat simp) 1 2))  ;1/2

(defun absarg (l &optional (absflag nil))
;; Commenting out the the expansion of the expression l. It seems to be not
;; necessary, but can cause expression swelling (DK 01/2010).
;  (setq l ($expand l))
  (cond ((atom l)
	 (cond ((eq l '$%i)
		(cons 1 (simplify '((mtimes) ((rat simp) 1 2) $%pi))))
	       ((numberp l)
		(cons (abs l) (argnum l)))
	       ((member l '($%e $%pi) :test #'eq) (cons l 0))
	       ((eq l '$infinity) (cons '$inf '$ind))
               ((decl-complexp l)
                (cons (list '(mabs simp) l) ; noun form with mabs
                      (list '(%carg simp) l)))
	       (absflag (cons (take '(mabs) l) 0))
	       (t
                ;; At this point l is representing a real value. Try to 
                ;; determine the sign and return a general form when the sign is
                ;; unknown.
		(let ((gs (if (eq rischp l) '$pos ($sign l))))
		  (cond ((member gs '($pos $pz)) (cons l 0))
			((eq gs '$zero) (cons 0 0))
			((eq gs '$neg)
			 (cons (neg l) (simplify '$%pi)))
			(t (cons (take '(mabs) l) (genatan 0 l))))))))
	((eq '$zero (let ((sign-imag-errp nil)) (catch 'sign-imag-err ($sign l))))
	 (cond ((some-bfloatp l)
		(cons *bigfloatzero* *bigfloatzero*))	; contagious
	       ((some-floatp l)
		(cons 0.0 0.0))
	       (t (cons 0 0))))
	((member (caar l) '(rat bigfloat) :test #'eq)
	 (cons (list (car l) (abs (cadr l)) (caddr l))
	       (argnum (cadr l))))
	((eq (caar l) 'mtimes)
	 (do ((n (cdr l) (cdr n))
	      (abars)
	      (argl () (cons (cdr abars) argl))
	      (absl () (rplacd abars absl)))
	     (())
	   (unless n
	     (return (cons (muln absl t) (2pistrip (addn argl t)))))
	   (setq abars (absarg (car n) absflag))))
        ((and (eq (caar l) 'mexpt)
              (real-odd-root-p (caddr l))
              (=0 (cdr (risplit (cadr l)))))
         ;; With domain : real an odd root of a real quantity is real: its
         ;; modulus is the root of the modulus of the base, and its
         ;; argument is 0 or %pi, by its sign.  The last clause must not
         ;; handle it: its ABSARG-MABS would call SIMPABS, which sends a
         ;; power of a negative base back to CABS.
         (cons (power (car (absarg (cadr l) absflag)) (caddr l))
               (if absflag 0 (genatan 0 l))))
        ((eq (caar l) 'mexpt)
         ;; An expression z^a
         (let ((aa (absarg (cadr l) nil)) ; (abs(z) . arg(z))
               (sp (risplit (caddr l)))   ; (realpart(a) . imagpart(a))
               ($radexpand nil))
           (cond ((and (zerop1 (cdr sp))
                       (eq ($sign (sub 1 (take '(mabs) (car sp)))) '$pos))
                  ;; Special case: a is real and abs(a) < 1.
                  ;; This simplifies e.g. carg(sqrt(z)) -> carg(z)/2
                  (cons (mul (power (car aa) (car sp))
                             (power '$%e (neg (mul (cdr aa) (cdr sp)))))
                        (mul (caddr l) (cdr aa))))
                 (t
                  ;; General case for z and a
                  (let ((arg (add (mul (cdr sp) (take '(%log) (car aa)))
                                  (mul (cdr aa) (car sp)))))
                    (cons (mul (power (car aa) (car sp))
                               (power '$%e (neg (mul (cdr aa) (cdr sp)))))
                          (if generate-atan2
			      (take '(%atan2)
				    (take '(%sin) arg)
				    (take '(%cos) arg))
			    (take '(%atan) (take '(%tan) arg)))))))))
	((and (member (caar l) '(%tan %tanh) :test #'eq)
	      (not (=0 (cdr (risplit (cadr l))))))
	 (let* ((sp (risplit (cadr l)))
		(2frst (mul (cdr sp) 2))
		(2scnd (mul (car sp) 2)))
	   (when (eq (caar l) '%tanh)
	     (psetq 2frst 2scnd 2scnd 2frst))
	   (cons (let ((cosh (take '(%cosh) 2frst))
		       (cos (take '(%cos) 2scnd)))
		   (root (div (add cosh (neg cos))
			      (add cosh cos))
			 2))
		 (take '(%atan)
		       (if (eq (caar l) '%tan)
			   (div (take '(%sinh) 2frst) (take '(%sin) 2scnd))
			   (div (take '(%sin) 2scnd) (take '(%sinh) 2frst)))))))
	((specrepp l) (absarg (specdisrep l) absflag))
	((let ((foot (coversinemyfoot l)))
	   (and foot (not (=0 (cdr (risplit (cadr l))))) (absarg foot absflag))))
	(t
	 (let ((ris (trisplit l)))
	   (xcons
;;; Arguments must be in this order so that the side-effect of the Atan2,
;;; that is, determining the Asksign of the argument, can happen before
;;; Take Mabs does its Sign.  Blame JPG for noticing this lossage.
	    (if absflag 0 (genatan (cdr ris) (car ris)))
	    (cond ((equal (car ris) 0) (absarg-mabs (cdr ris)))
		  ((equal (cdr ris) 0) (absarg-mabs (car ris)))
		  (t (hypotenuse (car ris) (cdr ris)))))))))

(defun hypotenuse-numerical (re im)
 "Dispatch the CL abs function to return |re + %i im|. The inputs re and im should be floating point numbers.
  We trust the compiler to work correctly for all double floats, including denormalized floats, and not needlessly
  over or underflow."
  (cond ((zerop im) (abs re))
        ((zerop re) (abs im))
        (t (abs (complex re im)))))

(defun hypotenuse (re im)
 (flet ((hypotenuse-default (re im) ;ok to use when no worries about floating point over/underflow
          ;; For mixed binary64 and symbolic cases, computing re^2 or im^2 can cause a 
          ;; floating point overflow. When an error happens, we'll punt to an abs nounform.

          ;; I'd prefer to eliminate the following calls to expand, but doing so causes
          ;; some testsuite failures.
          (setq re ($expand re 1 0)
                im ($expand im 1 0))
          (or (ignore-errors (ftake 'mexpt (add (mul re re) (mul im im)) 1//2))
              (ftake 'mabs (add re (mul '$%i im))))))

    (cond ((or ($bfloatp re) ($bfloatp im)) ; at least one bigfloat
            (hypotenuse-default ($bfloat re) ($bfloat im)))

          ((or (and (floatp re) (mnump im)) (and (mnump re) (floatp im))) ;at least one float part
             (hypotenuse-numerical ($float re) ($float im)))

          (t (hypotenuse-default re im))))) ;fall back

(defun genatan (num den)
  (let ((arg (take '(%atan2) num den)))
    (if (or generate-atan2
            (zerop1 den)
            (free arg '%atan2))
        arg
        (take '(%atan) (div num den)))))

(defun absarg-mabs (l)
  (cond ((eq (csign l) t)
         (if (member (caar l) '(mabs %cabs) :test #'eq) 
             l 
             (list '(mabs simp) l))) ; mabs and not %cabs as noun form
        ((member ($csign l) '($complex $imaginary))
         ;; Do not try to simplify a complex expression at this point,
         ;; this would cause an endless loop. Return a noun form.
         (list '(mabs simp) l))
        (t 
         (take '(mabs) l))))
