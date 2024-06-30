;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) copyright 1982 massachusetts institute of technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module defint)

;;;          this is the definite integration package.
;;	defint does definite integration by trying to find an
;;appropriate method for the integral in question.  the first thing that
;;is looked at is the endpoints of the problem.
;;
;;	i(grand,var,a,b) will be used for integrate(grand,var,a,b)

;; References are to "Evaluation of Definite Integrals by Symbolic
;; Manipulation", by Paul S. Wang,
;; (http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-092.pdf;
;; a better copy might be: https://maxima.sourceforge.io/misc/Paul_Wang_dissertation.pdf)
;;
;;	nointegrate is a macsyma level flag which inhibits indefinite
;;integration.
;;	abconv is a macsyma level flag which inhibits the absolute
;;convergence test.
;;
;;	$defint is the top level function that takes the user input
;;and does minor changes to make the integrand ready for the package.
;;
;;	next comes defint, which is the function that does the
;;integration.  it is often called recursively from the bowels of the
;;package.  defint does some of the easy cases and dispatches to:
;;
;;	dintegrate.  this program first sees if the limits of
;;integration are 0,inf or minf,inf.  if so it sends the problem to
;;ztoinf or mtoinf, respectively.
;;	else, dintegrate tries:
;;
;;	intsc1 - does integrals of sin's or cos's or exp(%i var)'s
;;		 when the interval is 0,2 %pi or 0,%pi.
;;		 method is conversion to rational function and find
;;		 residues in the unit circle. [wang, pp 107-109]
;;
;;	ratfnt - does rational functions over finite interval by
;;		 doing polynomial part directly, and converting
;;		 the rational part to an integral on 0,inf and finding
;;		 the answer by residues.
;;
;;	zto1   - i(x^(k-1)*(1-x)^(l-1),x,0,1) = beta(k,l)  or
;;		 i(log(x)*x^(x-1)*(1-x)^(l-1),x,0,1) = psi...
;;		 [wang, pp 116,117]
;;
;;	dintrad- i(x^m/(a*x^2+b*x+c)^(n+3/2),x,0,inf) [wang, p 74]
;;
;;	dintlog- i(log(g(x))*f(x),x,0,inf) = 0 (by symmetry) or
;;		 tries an integration by parts.  (only routine to
;;		 try integration by parts) [wang, pp 93-95]
;;
;;	dintexp- i(f(exp(k*x)),x,a,inf) = i(f(x+1)/(x+1),x,0,inf)
;;               or i(f(x)/x,x,0,inf)/k. First case hold for a=0;
;;               the second for a=minf. [wang 96-97]
;;
;;dintegrate also tries indefinite integration based on certain
;;predicates (such as abconv) and tries breaking up the integrand
;;over a sum or tries a change of variable.
;;
;;	ztoinf is the routine for doing integrals over the range 0,inf.
;;          it goes over a series of routines and sees if any will work:
;;
;;	   scaxn  - sc(b*x^n) (sc stands for sin or cos) [wang, pp 81-83]
;;
;;	   ssp    - a*sc^n(r*x)/x^m  [wang, pp 86,87]
;;
;;	   zmtorat- rational function. done by multiplication by plog(-x)
;;		    and finding the residues over the keyhole contour
;;		    [wang, pp 59-61]
;;
;;	   log*rat- r(x)*log^n(x) [wang, pp 89-92]
;;
;;	   logquad0 log(x)/(a*x^2+b*x+c) uses formula
;;		    i(log(x)/(x^2+2*x*a*cos(t)+a^2),x,0,inf) =
;;		    t*log(a)/sin(t).  a better formula might be
;;		    i(log(x)/(x+b)/(x+c),x,0,inf) =
;;		    (log^2(b)-log^2(c))/(2*(b-c))
;;
;;	   batapp - x^(p-1)/(b*x^n+a)^m uses formula related to the beta
;;		    function [wang, p 71]
;;		    there is also a special case when m=1 and a*b<0
;;		    see [wang, p 65]
;;
;;          sinnu  - x^-a*n(x)/d(x) [wang, pp 69-70]
;;
;;	   ggr    - x^r*exp(a*x^n+b)
;;
;;	   dintexp- see dintegrate
;;
;;     ztoinf also tries 1/2*mtoinf if the integrand is an even function
;;
;; mtoinf is the routine for doing integrals on minf,inf.
;;        it too tries a series of routines and sees if any succeed.
;;
;;	 scaxn  - when the integrand is an even function, see ztoinf
;;
;;	 mtosc  - exp(%i*m*x)*r(x) by residues on either the upper half
;;		  plane or the lower half plane, depending on whether
;;		  m is positive or negative.
;;
;;	 zmtorat- does rational function by finding residues in upper
;;	          half plane
;;
;;	 dintexp- see dintegrate
;;
;;	 rectzto%pi2 - poly(x)*rat(exp(x)) by finding residues in
;;		       rectangle [wang, pp98-100]
;;
;;	 ggrm   - x^r*exp((x+a)^n+b)
;;
;;   mtoinf also tries 2*ztoinf if the integrand is an even function.

(load-macsyma-macros rzmac)

(declare-top (special *mtoinf*
                      exp
		      *defint-assumptions*
		      *current-assumptions*
		      *global-defint-assumptions*)
;;;rsn* is in comdenom. does a ratsimp of numerator.
					;expvar
	     (special $noprincipal)
					;impvar
	     (special *roots *failures
		      context
		      ;;LIMITP T Causes $ASKSIGN to do special things
		      ;;For DEFINT like eliminate epsilon look for prin-inf
		      ;;take realpart and imagpart.
		      ))

(defvar *loopstop* 0)

(defmvar $intanalysis t
  "When @code{true}, definite integration tries to find poles in the integrand 
in the interval of integration.")

;; Currently, if true, $solvetrigwarn is set to true.  No additional
;; debugging information is displayed.
(defvar *defintdebug* ()
  "If true Defint prints out some debugging information.")

(defvar *pcprntd*
  nil
  "When NIL, print a message that the principal value of the integral has
  been computed.")

(defvar *nodiverg*
  nil
  "When non-NIL, a divergent integral will throw to `divergent.
  Otherwise, an error is signaled that the integral is divergent.")

(defvar *dflag* nil)

(defvar *bptu* nil)
(defvar *bptd* nil)

;; Set to true when OSCIP-VAR returns true in DINTEGRATE.
(defvar *scflag* nil)

(defvar *sin-cos-recur* nil
  "Prevents recursion of integrals of sin and cos in intsc1.")

(defvar *rad-poly-recur* nil
  "Prevents recursion in method-radical-poly.")

(defvar *dintlog-recur* nil
  "Prevents recursion in dintlog.")

(defvar *dintexp-recur* nil
  "Prevents recursion in dintexp.")


(defmfun $defint (exp ivar ll ul)

  ;; Distribute $defint over equations, lists, and matrices.
  (cond ((mbagp exp)
         (return-from $defint
           (simplify
             (cons (car exp)
                   (mapcar #'(lambda (e)
                               (simplify ($defint e ivar ll ul)))
                           (cdr exp)))))))

  (let ((*global-defint-assumptions* ())
	(*integer-info* ()) (integerl integerl) (nonintegerl nonintegerl))
    (with-new-context (context)
      (unwind-protect
	   (let ((*defint-assumptions* ()) (*rad-poly-recur* ())
		 (*sin-cos-recur* ())  (*dintexp-recur* ())  (*dintlog-recur* 0.)
		 (ans nil)  (orig-exp exp)  (orig-var ivar)
		 (orig-ll ll)  (orig-ul ul)
		 (*pcprntd* nil)  (*nodiverg* nil)  ($logabs t)  ; (limitp t)
		 (rp-polylogp ())
                 ($%edispflag nil) ; to get internal representation
		 ($m1pbranch ())) ;Try this out.

	     (make-global-assumptions) ;sets *global-defint-assumptions*
	     (setq exp (ratdisrep exp))
	     (setq ivar (ratdisrep ivar))
	     (setq ll (ratdisrep ll))
	     (setq ul (ratdisrep ul))
	     (cond (($constantp ivar)
		    (merror (intl:gettext "defint: variable of integration cannot be a constant; found ~M") ivar))
		   (($subvarp ivar)  (setq ivar (gensym))
		    (setq exp ($substitute ivar orig-var exp))))
	     (cond ((not (atom ivar))
		    (merror (intl:gettext "defint: variable of integration must be a simple or subscripted variable.~%defint: found ~M") ivar))
		   ((or (among ivar ul)
			(among ivar ll))
		    (setq ivar (gensym))
		    (setq exp ($substitute ivar orig-var exp))))
             (unless (lenient-extended-realp ll)
               (merror (intl:gettext "defint: lower limit of integration must be real; found ~M") ll))
             (unless (lenient-extended-realp ul)
               (merror (intl:gettext "defint: upper limit of integration must be real; found ~M") ul))

	     (cond ((setq ans (defint exp ivar ll ul))
		    (setq ans (subst orig-var ivar ans))
		    (cond ((atom ans)  ans)
			  ((and (free ans '%limit)
				(free ans '%integrate)
				(or (not (free ans '$inf))
				    (not (free ans '$minf))
				    (not (free ans '$infinity))))
			   (diverg))
			  ((not (free ans '$und))
			   `((%integrate) ,orig-exp ,orig-var ,orig-ll ,orig-ul))
			  (t ans)))
		   (t `((%integrate) ,orig-exp ,orig-var ,orig-ll ,orig-ul))))
	(forget-global-assumptions)))))

(defun eezz (exp ll ul ivar)
  (cond ((or (polyinx exp ivar nil)
	     (catch 'pin%ex (pin%ex exp ivar)))
	 (setq exp (antideriv exp ivar))
	 ;; If antideriv can't do it, returns nil
	 ;; use limit to evaluate every answer returned by antideriv.
	 (cond ((null exp) nil)
	       (t (intsubs exp ll ul ivar))))))

;;;Hack the expression up for exponentials.

(defun sinintp (expr ivar)
  ;; Is this expr a candidate for SININT ?
  (let ((expr (factor expr))
	(numer nil)
	(denom nil))
    (setq numer ($num expr))
    (setq denom ($denom expr))
    (cond ((polyinx numer ivar nil)
	   (cond ((and (polyinx denom ivar nil)
		       (deg-lessp denom ivar 2))
		  t)))
	  ;;ERF type things go here.
	  ((let ((exponent (%einvolve-var numer ivar)))
	     (and (polyinx exponent ivar nil)
		  (deg-lessp exponent ivar 2)))
	   (cond ((free denom ivar)
		  t))))))

(defun deg-lessp (expr ivar power)
  (cond  ((or (atom expr)
	      (mnump expr)) t)
	 ((or (mtimesp expr)
	      (mplusp expr))
	  (do ((ops (cdr expr) (cdr ops)))
	      ((null ops) t)
	    (cond ((not (deg-lessp (car ops) ivar power))
		   (return ())))))
	 ((mexptp expr)
	  (and (or (not (alike1 (cadr expr) ivar))
		   (and (numberp (caddr expr))
			(not (eq (asksign (m+ power (m- (caddr expr))))
				 '$negative))))
	       (deg-lessp (cadr expr) ivar power)))
	 ((and (consp expr)
	       (member 'array (car expr))
	       (not (eq ivar (caar expr))))
	  ;; We have some subscripted variable that's not our variable
	  ;; (I think), so it's deg-lessp.
	  ;;
	  ;; FIXME: Is this the best way to handle this?  Are there
	  ;; other cases we're mising here?
	  t)))

(defun antideriv (a ivar)
  (let ((limitp ())
	(ans ())
	(generate-atan2 ()))
    (setq ans (sinint a ivar))
    (cond ((among '%integrate ans)  nil)
	  (t (simplify ans)))))

;; This routine tries to take a limit a couple of ways.
(defun get-limit (exp ivar val &optional (dir '$plus dir?))
  (let ((ans (if dir?
		 (funcall #'limit-no-err exp ivar val dir)
		 (funcall #'limit-no-err exp ivar val))))
    (if (and ans (not (among '%limit ans)))
	ans
	(when (member val '($inf $minf) :test #'eq)
	  (setq ans (limit-no-err (maxima-substitute (m^t ivar -1) ivar exp)
				  ivar
				  0
				  (if (eq val '$inf) '$plus '$minus)))
	  (if (among '%limit ans) nil ans)))))

(defun limit-no-err (&rest argvec)
  (let ((errorsw t) (ans nil))
    (setq ans (catch 'errorsw (apply #'$limit argvec)))
    (if (eq ans t) nil ans)))

;; Test whether fun2 is inverse of fun1 at val.
(defun test-inverse (fun1 var1 fun2 var2 val)
  (let* ((out1 (no-err-sub-var val fun1 var1))
	 (out2 (no-err-sub-var out1 fun2 var2)))
    (alike1 val out2)))

;; integration change of variable
(defun intcv (nv flag ivar ll ul)
  (let ((d (bx**n+a nv ivar))
	(*roots ())  (*failures ())  ($breakup ()))
    (cond ((and (eq ul '$inf)
		(equal ll 0)
		(equal (cadr d) 1)) ())
	  ((eq ivar 'yx)		; new ivar cannot be same as old ivar
	   ())
	  (t
	   ;; This is a hack!  If nv is of the form b*x^n+a, we can
	   ;; solve the equation manually instead of using solve.
	   ;; Why?  Because solve asks us for the sign of yx and
	   ;; that's bogus.
	   (cond (d
		  ;; Solve yx = b*x^n+a, for x.  Any root will do.  So we
		  ;; have x = ((yx-a)/b)^(1/n).
		  (destructuring-bind (a n b)
		      d
		    (let ((root (power* (div (sub 'yx a) b) (inv n))))
		      (cond (t
			     (setq d root)
			     (cond (flag (intcv2 d nv ivar ll ul))
				   (t (intcv1 d nv ivar ll ul))))
			    ))))
		 (t
		  (putprop 'yx t 'internal);; keep ivar from appearing in questions to user
		  (solve (m+t 'yx (m*t -1 nv)) ivar 1.)
		  (cond ((setq d	;; look for root that is inverse of nv
			       (do* ((roots *roots (cddr roots))
				     (root (caddar roots) (caddar roots)))
				    ((null root) nil)
				    (if (and (or (real-infinityp ll)
						 (test-inverse nv ivar root 'yx ll))
					     (or (real-infinityp ul)
						 (test-inverse nv ivar root 'yx ul)))
					(return root))))
			 (cond (flag (intcv2 d nv ivar ll ul))
			       (t (intcv1 d nv ivar ll ul))))
			(t ()))))))))

;; d: original variable (ivar) as a function of 'yx
;; ind: boolean flag
;; nv: new variable ('yx) as a function of original variable (ivar)
(defun intcv1 (d nv ivar ll ul)
  (multiple-value-bind (exp-yx ll1 ul1)
      (intcv2 d nv ivar ll ul)
    (cond ((and (equal ($imagpart ll1) 0)
	        (equal ($imagpart ul1) 0)
	        (not (alike1 ll1 ul1)))
	   (defint exp-yx 'yx ll1 ul1)))))

;; converts limits of integration to values for new variable 'yx
(defun intcv2 (d nv ivar ll ul)
  (flet ((intcv3 (d nv ivar)
           ;; rewrites exp, the integrand in terms of ivar, the
           ;; integrand in terms of 'yx, and returns the new
           ;; integrand.
           (let ((exp-yx (m* (sdiff d 'yx)
		             (subst d ivar (subst 'yx nv exp)))))
             (sratsimp exp-yx))))
    (let ((exp-yx (intcv3 d nv ivar))
          ll1 ul1)
      (and (cond ((and (zerop1 (m+ ll ul))
		       (evenfn nv ivar))
	          (setq exp-yx (m* 2 exp-yx)
		        ll1 (limcp nv ivar 0 '$plus)))
	         (t (setq ll1 (limcp nv ivar ll '$plus))))
           (setq ul1 (limcp nv ivar ul '$minus))
           (values exp-yx ll1 ul1)))))

;; wrapper around limit, returns nil if 
;; limit not found (nounform returned), or undefined ($und or $ind)
(defun limcp (a b c d)
  (let ((ans ($limit a b c d)))
    (cond ((not (or (null ans)
		    (among '%limit ans)
		    (among '$ind ans)
		    (among '$und ans)))
	   ans))))

(defun integrand-changevar (d newvar exp ivar)
  (m* (sdiff d newvar)
      (subst d ivar exp)))
  
(defun defint (exp ivar ll ul)
  (let ((old-assumptions *defint-assumptions*)  
        (*current-assumptions* ())
        (limitp t))
    (unwind-protect
	 (prog ()
            (multiple-value-setq (*current-assumptions* ll ul)
	      (make-defint-assumptions 'noask ivar ll ul))
	    (let ((exp (resimplify exp))
		  (ivar (resimplify ivar))
		  ($exptsubst t)
		  (*loopstop* 0)
		  ;; D (not used? -- cwh)
		  ans nn* dn* $noprincipal)
	      (cond ((setq ans (defint-list exp ivar ll ul))
		     (return ans))
		    ((or (zerop1 exp)
			 (alike1 ul ll))
		     (return 0.))
		    ((not (among ivar exp))
		     (cond ((or (member ul '($inf $minf) :test #'eq)
				(member ll '($inf $minf) :test #'eq))
			    (diverg))
			   (t (setq ans (m* exp (m+ ul (m- ll))))
			      (return ans))))
                    ;; Look for integrals which involve log and exp functions.
                    ;; Maxima has a special algorithm to get general results.
                    ((and (setq ans (defint-log-exp exp ivar ll ul)))
                     (return ans)))
	      (let* ((exp (rmconst1 exp ivar))
		     (c (car exp))
		     (exp (%i-out-of-denom (cdr exp))))
		(cond ((and (not $nointegrate)
			    (not (atom exp))
			    (or (among 'mqapply exp)
				(not (member (caar exp)
					   '(mexpt mplus mtimes %sin %cos
					     %tan %sinh %cosh %tanh
					     %log %asin %acos %atan
					     %cot %acot %sec
					     %asec %csc %acsc
					     %derivative) :test #'eq))))
		       ;; Call ANTIDERIV with logabs disabled,
		       ;; because the Risch algorithm assumes
		       ;; the integral of 1/x is log(x), not log(abs(x)).
		       ;; Why not just assume logabs = false within RISCHINT itself?
		       ;; Well, there's at least one existing result which requires
		       ;; logabs = true in RISCHINT, so try to make a minimal change here instead.
		       (cond ((setq ans (let ($logabs) (antideriv exp ivar)))
			      (setq ans (intsubs ans ll ul ivar))
			      (return (cond (ans (m* c ans)) (t nil))))
			     (t (return nil)))))
		(setq exp (tansc-var exp ivar))
		(cond ((setq  ans (initial-analysis exp ivar ll ul))
		       (return (m* c ans))))
		(return nil))))
      (restore-defint-assumptions old-assumptions *current-assumptions*))))

(defun defint-list (exp ivar ll ul)
  (cond ((mbagp exp)
	 (let ((ans (cons (car exp)
			  (mapcar
			   #'(lambda (sub-exp)
			       (defint sub-exp ivar ll ul))
			   (cdr exp)))))
	   (cond (ans (simplify ans))
		 (t nil))))
	(t nil)))

(defun initial-analysis (exp ivar ll ul)
  (let ((pole (cond ((not $intanalysis)
		     '$no)		;don't do any checking.
		    (t (poles-in-interval exp ivar ll ul)))))
    (cond ((eq pole '$no)
	   (cond ((and (oddfn exp ivar)
		       (or (and (eq ll '$minf)
				(eq ul '$inf))
			   (eq ($sign (m+ ll ul))
			       '$zero)))  0)
		 (t (parse-integrand exp ivar ll ul))))
	  ((eq pole '$unknown)  ())
	  (t (principal-value-integral exp ivar ll ul pole)))))

(defun parse-integrand (exp ivar ll ul)
  (let (ans)
    (cond ((setq ans (eezz exp ll ul ivar))  ans)
	  ((and (ratp exp ivar)
		(setq ans (method-by-limits exp ivar ll ul)))
           ans)
	  ((and (mplusp exp)
		(setq ans (intbyterm exp t ivar ll ul)))
           ans)
	  ((setq ans (method-by-limits exp ivar ll ul))  ans)
	  (t ()))))

(defun rmconst1 (e ivar)
  (cond ((not (freeof ivar e))
	 (partition e ivar 1))
	(t (cons e 1))))


(defun method-by-limits (exp ivar ll ul)
  (let ((old-assumptions *defint-assumptions*))
    (multiple-value-bind (*current-assumptions* ll ul)
        (make-defint-assumptions 'noask ivar ll ul))

    ;;Should be a PROG inside of unwind-protect, but Multics has a compiler
    ;;bug wrt. and I want to test this code now.
    (unwind-protect
	 (cond ((and (and (eq ul '$inf)
			  (eq ll '$minf))
		     (mtoinf exp ivar ll ul)))
	       ((and (and (eq ul '$inf)
			  (equal ll 0.))
		     (ztoinf exp ivar ll ul)))
;;;This seems((and (and (eq ul '$inf)
;;;fairly losing	(setq exp (subin (m+ ll ivar) exp))
;;;			(setq ll 0.))
;;;		   (ztoinf exp ivar)))
	       ((and (equal ll 0.)
		     (freeof ivar ul)
		     (eq ($asksign ul) '$pos)
		     (zto1 exp ivar ul)))
	       ;;	     ((and (and (equal ul 1.)
	       ;;			(equal ll 0.))  (zto1 exp)))
	       (t (dintegrate exp ivar ll ul)))
      (restore-defint-assumptions old-assumptions *defint-assumptions*))))


(defun dintegrate (exp ivar ll ul)
  (let ((ans nil) (arg nil) (*scflag* nil)
	(*dflag* nil) ($%emode t))
;;;NOT COMPLETE for sin's and cos's.
    (cond ((and (not *sin-cos-recur*)
		(oscip-var exp ivar)
		(setq *scflag* t)
		(intsc1 ll ul exp ivar)))
	  ((and (not *rad-poly-recur*)
		(notinvolve-var exp ivar '(%log))
		(not (%einvolve-var exp ivar))
		(method-radical-poly exp ivar ll ul)))
	  ((and (not (equal *dintlog-recur* 2.))
		(setq arg (involve-var exp ivar '(%log)))
		(dintlog exp arg ivar ll ul)))
	  ((and (not *dintexp-recur*)
		(setq arg (%einvolve-var exp ivar))
		(dintexp exp ivar ll ul)))
	  ((and (not (ratp exp ivar))
		(setq ans (let (($trigexpandtimes nil)
				($trigexpandplus t))
			    ($trigexpand exp)))
		(setq ans ($expand ans))
		(not (alike1 ans exp))
		(intbyterm ans t ivar ll ul)))
	  ;; Call ANTIDERIV with logabs disabled,
	  ;; because the Risch algorithm assumes
	  ;; the integral of 1/x is log(x), not log(abs(x)).
	  ;; Why not just assume logabs = false within RISCHINT itself?
	  ;; Well, there's at least one existing result which requires
	  ;; logabs = true in RISCHINT, so try to make a minimal change here instead.
	  ((setq ans (let ($logabs) (antideriv exp ivar)))
	   (intsubs ans ll ul ivar))
	  (t nil))))

(defun method-radical-poly (exp ivar ll ul)
;;;Recursion stopper
  (let ((*rad-poly-recur* t)		;recursion stopper
	(result ()))
    (cond ((and (sinintp exp ivar)
		(setq result (antideriv exp ivar))
		(intsubs result ll ul ivar)))
	  ((and (ratp exp ivar)
		(setq result (ratfnt exp ivar ll ul))))
	  ((and (not *scflag*)
		(not (eq ul '$inf))
		(radicalp exp ivar)
		(kindp34 ivar ll ul)
		(setq result (cv exp ivar ll ul))))
	  (t ()))))

(defun principal-value-integral (exp ivar ll ul poles)
  (let ((anti-deriv ()))
    (cond ((not (null (setq anti-deriv (antideriv exp ivar))))
	   (cond ((not (null poles))
		  (multiple-value-bind (ignore new-ll new-ul)
                      (order-limits 'ask ivar ll ul)
                    (declare (ignore ignore))
		    (cond ((take-principal anti-deriv new-ll new-ul ivar poles))
			  (t ())))))))))

;; adds up integrals of ranges between each pair of poles.
;; checks if whole thing is divergent as limits of integration approach poles.
(defun take-principal (anti-deriv ll ul ivar poles &aux ans merged-list)
  ;;; calling $logcontract causes antiderivative of 1/(1-x^5) to blow up
  ;;  (setq anti-deriv (cond ((involve anti-deriv '(%log))
  ;;			  ($logcontract anti-deriv))
  ;;			 (t anti-deriv)))
  (setq ans 0.)
  (multiple-value-setq (merged-list ll ul)
    (interval-list poles ll ul))
  (do ((current-pole (cdr merged-list) (cdr current-pole))
       (previous-pole merged-list (cdr previous-pole)))
      ((null current-pole)  t)
    (setq ans (m+ ans
		  (intsubs anti-deriv (m+ (caar previous-pole) 'epsilon)
			   (m+ (caar current-pole) (m- 'epsilon))
                           ivar))))

  (setq ans (get-limit (get-limit ans 'epsilon 0 '$plus) 'prin-inf '$inf))
  ;;Return section.
  (cond ((or (null ans)
	     (not (free ans '$infinity))
	     (not (free ans '$ind)))  ())
	((or (among '$minf ans)
	     (among '$inf ans)
	     (among '$und ans))
	 (diverg))
	(t (principal) ans)))

;; I think this takes the pole-list and replaces $MINF with -PRIN-INF
;; and $INF with PRIN-INF.  The lower and upper integration limits
;; (ll, ul) can also be modified to be -PRIN-INF and PRIN-INF.  These
;; special values are used in TAKE-PRINCIPAL.
(defun interval-list (pole-list ll ul)
  (let ((first (car (first pole-list)))
	(last (caar (last pole-list))))
    (cond ((eq ul last)
	   (if (eq ul '$inf)
	       (setq pole-list (subst 'prin-inf '$inf pole-list))))
	  (t (if (eq ul '$inf)
		 (setq ul 'prin-inf))
	     (setq pole-list (append pole-list (list (cons ul 'ignored))))))
    (cond ((eq ll first)
	   (if (eq ll '$minf)
	       (setq pole-list (subst (m- 'prin-inf) '$minf pole-list))))
	  (t (if (eq ll '$minf)
		 (setq ll (m- 'prin-inf)))
	     (setq pole-list (append (list (cons ll 'ignored)) pole-list)))))
  (values pole-list ll ul))

;; Assumes EXP is a rational expression with no polynomial part and
;; converts the finite integration to integration over a half-infinite
;; interval.  The substitution y = (x-a)/(b-x) is used.  Equivalently,
;; x = (b*y+a)/(y+1).
;;
;; (I'm guessing CV means Change Variable here.)
(defun cv (exp ivar ll ul)
  (if (not (or (real-infinityp ll) (real-infinityp ul)))
      ;; FIXME!  This is a hack.  We apply the transformation with
      ;; symbolic limits and then substitute the actual limits later.
      ;; That way method-by-limits (usually?) sees a simpler
      ;; integrand.
      ;;
      ;; See Bugs 938235 and 941457.  These fail because $FACTOR is
      ;; unable to factor the transformed result.  This needs more
      ;; work (in other places).
      (let ((trans (integrand-changevar (m// (m+t 'll (m*t 'ul 'yx))
					     (m+t 1. 'yx))
					'yx exp ivar)))
	;; If the limit is a number, use $substitute so we simplify
	;; the result.  Do we really want to do this?
	(setf trans (if (mnump ll)
			($substitute ll 'll trans)
			(subst ll 'll trans)))
	(setf trans (if (mnump ul)
			($substitute ul 'ul trans)
			(subst ul 'ul trans)))
	(method-by-limits trans 'yx 0. '$inf))
      ()))

;; Integrate rational functions over a finite interval by doing the
;; polynomial part directly, and converting the rational part to an
;; integral from 0 to inf.  This is evaluated via residues.
(defun ratfnt (exp ivar ll ul)
  (let ((e (pqr exp ivar)))
    ;; PQR divides the rational expression and returns the quotient
    ;; and remainder
    (flet ((try-antideriv (e lo hi)
	     (let ((ans (antideriv e ivar)))
	       (when ans
		 (intsubs ans lo hi ivar)))))

      (cond ((equal 0. (car e))
	     ;; No polynomial part
	     (let ((ans (try-antideriv exp ll ul)))
	       (if ans
		   ans
		   (cv exp ivar ll ul))))
	    ((equal 0. (cdr e))
	     ;; Only polynomial part
	     (eezz (car e) ll ul ivar))
	    (t
	     ;; A non-zero quotient and remainder.  Combine the results
	     ;; together.
	     (let ((ans (try-antideriv (m// (cdr e) dn*) ll ul)))
	       (cond (ans
		      (m+t (eezz (car e) ll ul ivar)
			   ans))
		     (t
		      (m+t (eezz (car e) ll ul ivar)
			   (cv (m// (cdr e) dn*) ivar ll ul))))))))))

;; I think this takes a rational expression E, and finds the
;; polynomial part.  A cons is returned.  The car is the quotient and
;; the cdr is the remainder.
(defun pqr (e ivar)
  (let ((varlist (list ivar)))
    (newvar e)
    (setq e (cdr (ratrep* e)))
    (setq dn* (pdis (ratdenominator e)))
    (setq e (pdivide (ratnumerator e) (ratdenominator e)))
    (cons (simplify (rdis (car e))) (simplify (rdis (cadr e))))))


(defun intbyterm (exp *nodiverg* ivar ll ul)
  (let ((saved-exp exp))
    (cond ((mplusp exp)
	   (let ((ans (catch 'divergent
			(andmapcar #'(lambda (new-exp)
				       (defint new-exp ivar ll ul))
				   (cdr exp)))))
	     (cond ((null ans) nil)
		   ((eq ans 'divergent)
		    (let ((*nodiverg* nil))
		      (cond ((setq ans (antideriv saved-exp ivar))
			     (intsubs ans ll ul ivar))
			    (t nil))))
		   (t (sratsimp (m+l ans))))))
;;;If leadop isn't plus don't do anything.
	  (t nil))))

(defun kindp34 (ivar ll ul)
  (let* ((d (nth-value 1 (numden-var exp ivar)))
	 (a (cond ((and (zerop1 ($limit d ivar ll '$plus))
			(eq (limit-pole (m+ exp (m+ (m- ll) ivar))
					ivar ll '$plus)
			    '$yes))
		   t)
		  (t nil)))
	 (b (cond ((and (zerop1 ($limit d ivar ul '$minus))
			(eq (limit-pole (m+ exp (m+ ul (m- ivar)))
					ivar ul '$minus)
			    '$yes))
		   t)
		  (t nil))))
    (or a b)))

(defun diverg nil
  (cond (*nodiverg* (throw 'divergent 'divergent))
	(t (merror (intl:gettext "defint: integral is divergent.")))))

;; May reorder the limits LL and UL so that LL <= UL.  (See
;; ORDER-LIMITS.)  Hence, this function also returns the possibly
;; updated values of LL and UL as additional values.
(defun make-defint-assumptions (ask-or-not ivar ll ul)
  (values
   (cond ((null
           (multiple-value-setq (result ll ul)
             (order-limits ask-or-not ivar ll ul)))
          ())
	 (t (mapc 'forget *defint-assumptions*)
	    (setq *defint-assumptions* ())
	    (let ((sign-ll (cond ((eq ll '$inf)  '$pos)
			         ((eq ll '$minf) '$neg)
			         (t ($sign ($limit ll)))))
	          (sign-ul (cond ((eq ul '$inf)  '$pos)
			         ((eq ul '$minf)  '$neg)
			         (t ($sign ($limit ul)))))
	          (sign-ul-ll (cond ((and (eq ul '$inf)
				          (not (eq ll '$inf)))  '$pos)
				    ((and (eq ul '$minf)
				          (not (eq ll '$minf)))  '$neg)
				    (t ($sign ($limit (m+ ul (m- ll))))))))
	      (cond ((eq sign-ul-ll '$pos)
		     (setq *defint-assumptions*
			   `(,(assume `((mgreaterp) ,ivar ,ll))
			      ,(assume `((mgreaterp) ,ul ,ivar)))))
		    ((eq sign-ul-ll '$neg)
		     (setq *defint-assumptions*
			   `(,(assume `((mgreaterp) ,ivar ,ul))
			      ,(assume `((mgreaterp) ,ll ,ivar))))))
	      (cond ((and (eq sign-ll '$pos)
		          (eq sign-ul '$pos))
		     (setq *defint-assumptions*
			   `(,(assume `((mgreaterp) ,ivar 0))
			      ,@*defint-assumptions*)))
		    ((and (eq sign-ll '$neg)
		          (eq sign-ul '$neg))
		     (setq *defint-assumptions*
			   `(,(assume `((mgreaterp) 0 ,ivar))
			      ,@*defint-assumptions*)))
		    (t *defint-assumptions*)))))
   ll ul))

(defun restore-defint-assumptions (old-assumptions assumptions)
  (do ((llist assumptions (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (do ((llist old-assumptions (cdr llist)))
      ((null llist) t)
    (assume (car llist))))

(defun make-global-assumptions ()
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) *z* 0.))
	      *global-defint-assumptions*))
  ;; *Z* is a "zero parameter" for this package.
  ;; Its also used to transform.
  ;;  limit(exp,var,val,dir) -- limit(exp,tvar,0,dir)
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) epsilon 0.))
	      *global-defint-assumptions*))
  (setq *global-defint-assumptions*
	(cons (assume '((mlessp) epsilon 1.0e-8))
	      *global-defint-assumptions*))
  ;; EPSILON is used in principal value code to denote the familiar
  ;; mathematical entity.
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) prin-inf 1.0e+8))
	      *global-defint-assumptions*)))

;;; PRIN-INF Is a special symbol in the principal value code used to
;;; denote an end-point which is proceeding to infinity.

(defun forget-global-assumptions ()
  (do ((llist *global-defint-assumptions* (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (cond ((not (null *integer-info*))
	 (do ((llist *integer-info* (cdr llist)))
	     ((null llist) t)
	   (i-$remove `(,(cadar llist) ,(caddar llist)))))))

;; Order the limits LL and UL so that LL <= UL, as expected.  Of
;; course, this changes the sign of the integrand (in EXP), so that's
;; also updated as well.  Since the order can be changed, the possibly
;; updated values of LL and UL are returned as additional values of
;; this function.
(defun order-limits (ask-or-not ivar ll ul)
  (values
   (cond ((or (not (equal ($imagpart ll) 0))
	      (not (equal ($imagpart ul) 0)))  ())
	 (t (cond ((alike1 ll (m*t -1 '$inf))
		   (setq ll '$minf)))
	    (cond ((alike1 ul (m*t -1 '$inf))
		   (setq ul '$minf)))
	    (cond ((alike1 ll (m*t -1 '$minf))
		   (setq ll '$inf)))
	    (cond ((alike1 ul (m*t -1 '$minf))
		   (setq ul '$inf)))
	    (cond ((eq ll ul)
                   ;; We have minf <= ll = ul <= inf
		   )
		  ((eq ul '$inf)
                   ;; We have minf <= ll < ul = inf
		   )
		  ((eq ll '$minf)
                   ;; We have minf = ll < ul < inf
                   ;;
                   ;; Now substitute
                   ;;
                   ;;   ivar -> -ivar
                   ;;   ll  -> -ul
                   ;;   ul  -> inf
                   ;;
                   ;; so that minf < ll < ul = inf
		   (setq exp (subin-var (m- ivar) exp ivar))
		   (setq ll (m- ul))
		   (setq ul '$inf))
		  ((or (eq ll '$inf)
		       (equal (complm ask-or-not ll ul) -1))
                   ;; We have minf <= ul < ll
                   ;;
                   ;; Now substitute
                   ;;
                   ;;   exp  -> -exp
                   ;;   ll  <-> ul
                   ;;
                   ;; so that minf <= ll < ul
		   (setq exp (m- exp))
		   (rotatef ll ul)))
	    t))
   ll ul))

(defun complm (ask-or-not ll ul)
  (let ((askflag (cond ((eq ask-or-not 'ask)  t)
		       (t nil)))
	(a ()))
    (cond ((alike1 ul ll)  0.)
	  ((eq (setq a (cond (askflag ($asksign ($limit (m+t ul (m- ll)))))
			     (t ($sign ($limit (m+t ul (m- ll)))))))
	       '$pos)
	   1.)
	  ((eq a '$neg)  -1)
	  (t 1.))))

;; Substitute a and b into integral e
;;
;; Looks for discontinuties in integral, and works around them.
;; For example, in  
;;
;; integrate(x^(2*n)*exp(-(x)^2),x)    ==>
;; -gamma_incomplete((2*n+1)/2,x^2)*x^(2*n+1)*abs(x)^(-2*n-1)/2
;; 
;; the integral has a discontinuity at x=0.
;;
(defun intsubs (e a b ivar)
  (let ((edges (cond ((not $intanalysis)
		      '$no)		;don't do any checking.
		    (t (discontinuities-in-interval 
			(let (($algebraic t)) 
			  (sratsimp e))
			ivar a b)))))

    (cond ((or (eq edges '$no)
	       (eq edges '$unknown))
	   (whole-intsubs e a b ivar))
	  (t
	   (do* ((l edges (cdr l))
		 (total nil)
		 (a1 (car l) (car l))
		 (b1 (cadr l) (cadr l)))
		((null (cdr l)) (if (every (lambda (x) x) total)
				    (m+l total)))
		(push
		 (whole-intsubs e a1 b1 ivar)
		 total))))))

;; look for terms with a negative exponent
;;
;; recursively traverses exp in order to find discontinuities such as
;;  erfc(1/x-x) at x=0
(defun discontinuities-denom (exp ivar)
  (cond ((atom exp) 1)
	((and (eq (caar exp) 'mexpt)
	      (not (freeof ivar (cadr exp)))
	      (not (member ($sign (caddr exp)) '($pos $pz))))
	 (m^ (cadr exp) (m- (caddr exp))))
	(t 
	 (m*l (mapcar #'(lambda (e)
                          (discontinuities-denom e ivar))
                      (cdr exp))))))

;; returns list of places where exp might be discontinuous in ivar.
;; list begins with ll and ends with ul, and include any values between
;; ll and ul.
;; return '$no or '$unknown if no discontinuities found.
(defun discontinuities-in-interval (exp ivar ll ul)
  (let* ((denom (discontinuities-denom exp ivar))
	 (roots (real-roots denom ivar)))
    (cond ((eq roots '$failure)
	   '$unknown)
	  ((eq roots '$no)
	   '$no)
	  (t (do ((dummy roots (cdr dummy))
		  (pole-list nil))
		 ((null dummy)
		  (cond (pole-list
			 (append (list ll)
				 (sortgreat pole-list)
				 (list ul)))
			(t '$no)))
		 (let ((soltn (caar dummy)))
		   ;; (multiplicity (cdar dummy)) ;; not used
		   (if (strictly-in-interval soltn ll ul)
		       (push soltn pole-list))))))))


;; Carefully substitute the integration limits A and B into the
;; expression E.
(defun whole-intsubs (e a b ivar)
  (cond ((easy-subs e a b ivar))
	(t
         (let (new-ll new-ul)
           ;; Note: MAKE-DEFINT-ASSUMPTIONS may reorder the limits A
           ;; and B, but I (rtoy) don't think that's should ever
           ;; happen because the limits should already be in the
           ;; correct order when this function is called.  We don't
           ;; check for that, though.
           (multiple-value-setq (*current-assumptions* new-ll new-ul)
	       (make-defint-assumptions 'ask ivar a b)) ;get forceful!
         
	   (let (($algebraic t))
	     (setq e (sratsimp e))
	     (cond ((limit-subs e a b ivar))
		   (t (same-sheet-subs e a b ivar))))))))

;; Try easy substitutions.  Return NIL if we can't.
(defun easy-subs (e ll ul ivar)
  (cond ((or (infinityp ll) (infinityp ul))
	 ;; Infinite limits aren't easy
	 nil)
	(t
	 (cond ((or (polyinx e ivar ())
		    (and (not (involve-var e ivar '(%log %asin %acos %atan %asinh %acosh %atanh %atan2
						%gamma_incomplete %expintegral_ei)))
			 (free ($denom e) ivar)))
		;; It's easy if we have a polynomial.  I (rtoy) think
		;; it's also easy if the denominator is free of the
		;; integration variable and also if the expression
		;; doesn't involve inverse functions.
		;;
		;; gamma_incomplete and expintegral_ie
		;; included because of discontinuity in
		;; gamma_incomplete(0, exp(%i*x)) and 
		;; expintegral_ei(exp(%i*x))
		;;
		;; XXX:  Are there other cases we've forgotten about?
		;;
		;; So just try to substitute the limits into the
		;; expression.  If no errors are produced, we're done.
		(let ((ll-val (no-err-sub-var ll e ivar))
		      (ul-val (no-err-sub-var ul e ivar)))
		  (cond ((or (eq ll-val t)
                             (eq ul-val t))
                         ;; no-err-sub has returned T. An error was catched.
                         nil)
                        ((and ll-val ul-val)
			 (m- ul-val ll-val))
			(t nil))))
	       (t nil)))))

(defun limit-subs (e ll ul ivar)
  (cond ((involve-var e ivar '(%atan %gamma_incomplete %expintegral_ei))
	 ())	; functions with discontinuities
	(t (setq e ($multthru e))
	   (let ((a1 ($limit e ivar ll '$plus))
		 (a2 ($limit e ivar ul '$minus)))
	     (combine-ll-ans-ul-ans a1 a2)))))

;; check for divergent integral
(defun combine-ll-ans-ul-ans (a1 a2)
  (cond ((member a1 '($inf $minf $infinity ) :test #'eq)
	 (cond ((member a2 '($inf $minf $infinity) :test #'eq)
		(cond ((eq a2 a1)  ())
		      (t (diverg))))
	       (t (diverg))))
	((member a2 '($inf $minf $infinity) :test #'eq)  (diverg))
	((or (member a1 '($und $ind) :test #'eq)
	     (member a2 '($und $ind) :test #'eq))  ())
	(t (m- a2 a1))))

;;;This function works only on things with ATAN's in them now.
(defun same-sheet-subs (exp ll ul ivar &aux ll-ans ul-ans)
  ;; POLES-IN-INTERVAL doesn't know about the poles of tan(x).  Call
  ;; trigsimp to convert tan into sin/cos, which POLES-IN-INTERVAL
  ;; knows how to handle.
  ;;
  ;; XXX Should we fix POLES-IN-INTERVAL instead?
  ;;
  ;; XXX Is calling trigsimp too much?  Should we just only try to
  ;; substitute sin/cos for tan?
  ;;
  ;; XXX Should the result try to convert sin/cos back into tan?  (A
  ;; call to trigreduce would do it, among other things.)
  (let* ((exp (mfuncall '$trigsimp exp))
	 (poles (atan-poles exp ll ul ivar)))
    ;;POLES -> ((mlist) ((mequal) ((%atan) foo) replacement) ......)
    ;;We can then use $SUBSTITUTE
    (setq ll-ans (limcp exp ivar ll '$plus))
    (setq exp (sratsimp ($substitute poles exp)))
    (setq ul-ans (limcp exp ivar ul '$minus))
    (if (and ll-ans 
	     ul-ans)
	(combine-ll-ans-ul-ans ll-ans ul-ans)
      nil)))

(defun atan-poles (exp ll ul ivar)
  `((mlist) ,@(atan-pole1 exp ll ul ivar)))

(defun atan-pole1 (exp ll ul ivar &aux ipart)
  (cond
    ((mapatom exp)  ())
    ((matanp exp)	 ;neglect multiplicity and '$unknowns for now.
     (desetq (exp . ipart) (trisplit exp))
     (cond
       ((not (equal (sratsimp ipart) 0))  ())
       (t (let ((pole (poles-in-interval (let (($algebraic t))
					   (sratsimp (cadr exp)))
					 ivar ll ul)))
	    (cond ((and pole (not (or (eq pole '$unknown)
				      (eq pole '$no))))
		   (do ((l pole (cdr l)) (llist ()))
		       ((null l)  llist)
		     (cond
		       ((zerop1 (m- (caar l) ll)) t)  ; don't worry about discontinuity
 		       ((zerop1 (m- (caar l) ul)) t)  ;  at boundary of integration
		       (t (let ((low-lim ($limit (cadr exp) ivar (caar l) '$minus))
				(up-lim ($limit (cadr exp) ivar (caar l) '$plus)))
			    (cond ((and (not (eq low-lim up-lim))
					(real-infinityp low-lim)
					(real-infinityp up-lim))
				   (let ((change (if (eq low-lim '$minf)
						     (m- '$%pi)
						     '$%pi)))
				     (setq llist (cons `((mequal simp) ,exp  ,(m+ exp change))
						       llist)))))))))))))))
    (t (do ((l (cdr exp) (cdr l))
	    (llist ()))
	   ((null l)  llist)
	 (setq llist (append llist (atan-pole1 (car l) ll ul ivar)))))))

(defun difapply (ivar n d s fn1)
  (prog (k m r $noprincipal)
     (cond ((eq ($asksign (m+ (deg-var d ivar) (m- s) (m- 2.)))  '$neg)
	    (return nil)))
     (setq $noprincipal t)
     (cond ((or (not (mexptp d))
		(not (numberp (setq r (caddr d)))))
	    (return nil))
	   ((and (equal n 1.)
                 ;; There are no calls where fn1 is ever equal to
                 ;; 'mtorat.  Hence this case is never true.  What is
                 ;; this testing for?
		 (eq fn1 'mtorat)
		 (equal 1. (deg-var (cadr d) ivar)))
	    (return 0.)))
     (setq m (deg-var (setq d (cadr d)) ivar))
     (setq k (m// (m+ s 2.) m))
     (cond ((eq (ask-integer (m// (m+ s 2.) m) '$any)  '$yes)
	    nil)
	   (t (setq k (m+ 1 k))))
     (cond ((eq ($sign (m+ r (m- k))) '$pos)
	    (return (diffhk fn1 n d k (m+ r (m- k)) ivar))))))

(defun diffhk (fn1 n d r m ivar)
  (prog (d1 *dflag*)
     (setq *dflag* t)
     (setq d1 (funcall fn1 n
		       (m^ (m+t '*z* d) r)
		       (m* r (deg-var d ivar))))
     (cond (d1 (return (difap1 d1 r '*z* m 0.))))))

(defun principal nil
  (cond ($noprincipal (diverg))
	((not *pcprntd*)
	 (format t "Principal Value~%")
	 (setq *pcprntd* t))))

;; e is of form poly(x)*exp(m*%i*x)
;; s is degree of denominator
;; adds e to *bptu* or *bptd* according to sign of m
(defun rib (e s ivar)
  (cond ((or (mnump e) (constant e))
	 (setq *bptu* (cons e *bptu*)))
	(t
         (let (updn c nd nn)
           (setq e (rmconst1 e ivar))
	   (setq c (car e))
	   (setq nn (cdr e))
	   (setq nd s)
	   (multiple-value-setq (e updn)
             (catch 'ptimes%e (ptimes%e nn nd ivar)))
	   (cond ((null e) nil)
		 (t (setq e (m* c e))
		    (cond (updn (setq *bptu* (cons e *bptu*)))
			  (t (setq *bptd* (cons e *bptd*))))))))))

;; Check term is of form poly(x)*exp(m*%i*x)
;; n is degree of denominator.
(defun ptimes%e (term n ivar &aux updn)
  (cond ((and (mexptp term)
	      (eq (cadr term) '$%e)
	      (polyinx (caddr term) ivar nil)
	      (eq ($sign (m+ (deg-var ($realpart (caddr term)) ivar) -1))
		  '$neg)
	      (eq ($sign (m+ (deg-var (setq nn* ($imagpart (caddr term))) ivar)
			     -2.))
		  '$neg))
         ;; Set updn to T if the coefficient of IVAR in the
         ;; polynomial is known to be positive.  Otherwise set to NIL.
         ;; (What does updn really mean?)
         (setq updn (eq ($asksign (ratdisrep (ratcoef nn* ivar))) '$pos))
	 (values term updn))
	((and (mtimesp term)
	      (setq nn* (polfactors term ivar))
	      (or (null (car nn*))
		  (eq ($sign (m+ n (m- (deg-var (car nn*) ivar))))
		      '$pos))
              (not (alike1 (cadr nn*) term))
	      (multiple-value-setq (term updn)
                (ptimes%e (cadr nn*) n ivar))
	      term)
         (values term updn))
	(t (throw 'ptimes%e nil))))

(defun csemidown (n d ivar)
  (let ((*pcprntd* t)) ;Not sure what to do about PRINCIPAL values here.
    (princip
       (res-var ivar n d #'lowerhalf #'(lambda (x)
				         (cond ((equal ($imagpart x) 0)  t)
				               (t ())))))))

(defun lowerhalf (j)
  (eq ($asksign ($imagpart j)) '$neg))

(defun upperhalf (j)
  (eq ($asksign ($imagpart j)) '$pos))


(defun csemiup (n d ivar)
  (let ((*pcprntd* t)) ;I'm not sure what to do about PRINCIPAL values here.
    (princip
     (res-var ivar n d #'upperhalf #'(lambda (x)
				        (cond ((equal ($imagpart x) 0)  t)
				              (t ())))))))

(defun princip (n)
  (cond ((null n) nil)
	(t (m*t '$%i ($rectform (m+ (cond ((car n)
					   (m*t 2. (car n)))
					  (t 0.))
				    (cond ((cadr n)
					   (principal)
					   (cadr n))
					  (t 0.))))))))

;; exponentialize sin and cos
(defun sconvert (e ivar)
  (cond ((atom e) e)
	((polyinx e ivar nil) e)
	((eq (caar e) '%sin)
	 (m* '((rat) -1 2)
	     '$%i
	     (m+t (m^t '$%e (m*t '$%i (cadr e)))
		  (m- (m^t '$%e (m*t (m- '$%i) (cadr e)))))))
	((eq (caar e) '%cos)
	 (mul* '((rat) 1. 2.)
	       (m+t (m^t '$%e (m*t '$%i (cadr e)))
		    (m^t '$%e (m*t (m- '$%i) (cadr e))))))
	(t (simplify
	    (cons (list (caar e)) (mapcar #'(lambda (ee)
                                              (sconvert ee ivar))
                                          (cdr e)))))))

(defun polfactors (exp ivar)
  (let (poly rest)
    (cond ((mplusp exp)  nil)
	  (t (cond ((mtimesp exp)
		    (setq exp (reverse (cdr exp))))
		   (t (setq exp (list exp))))
	     (mapc #'(lambda (term)
		       (cond ((polyinx term ivar nil)
			      (push term poly))
			     (t (push term rest))))
		   exp)
	     (list (m*l poly) (m*l rest))))))

(defun esap (e)
  (prog (d)
     (cond ((atom e) (return e))
	   ((not (among '$%e e)) (return e))
	   ((and (mexptp e)
		 (eq (cadr e) '$%e))
	    (setq d ($imagpart (caddr e)))
	    (return (m* (m^t '$%e ($realpart (caddr e)))
			(m+ `((%cos) ,d)
			    (m*t '$%i `((%sin) ,d))))))
	   (t (return (simplify (cons (list (caar e))
				      (mapcar #'esap (cdr e)))))))))

;; computes integral from minf to inf for expressions of the form
;; exp(%i*m*x)*r(x) by residues on either the upper half
;;		  plane or the lower half plane, depending on whether
;;		  m is positive or negative.  [wang p. 77]
;;
;; exponentializes sin and cos before applying residue method.
;; can handle some expressions with poles on real line, such as
;; sin(x)*cos(x)/x.
(defun mtosc (grand ivar)
  (multiple-value-bind (n d)
      (numden-var grand ivar)
    (let (ratterms ratans
	  plf *bptu* *bptd* s upans downans)
      (cond ((not (or (polyinx d ivar nil)
		      (and (setq grand (%einvolve-var d ivar))
			   (among '$%i grand)
			   (polyinx (setq d (sratsimp (m// d (m^t '$%e grand))))
				    ivar
				    nil)
			   (setq n (m// n (m^t '$%e grand))))))  nil)
	    ((equal (setq s (deg-var d ivar)) 0)  nil)
;;;Above tests for applicability of this method.
	    ((and (or (setq plf (polfactors n ivar))  t)
		  (setq n ($expand (cond ((car plf)
					  (m*t 'x* (sconvert (cadr plf) ivar)))
				         (t (sconvert n ivar)))))
		  (cond ((mplusp n)  (setq n (cdr n)))
		        (t (setq n (list n))))
		  (dolist (term n t)
		    (cond ((polyinx term ivar nil)
			   ;; call to $expand can create rational terms
			   ;; with no exp(m*%i*x)
			   (setq ratterms (cons term ratterms)))
			  ((rib term s ivar))
			  (t (return nil))))
;;;Function RIB sets up the values of BPTU and BPTD
		  (cond ((car plf)
		         (setq *bptu* (subst (car plf) 'x* *bptu*))
		         (setq *bptd* (subst (car plf) 'x* *bptd*))
		         (setq ratterms (subst (car plf) 'x* ratterms))
		         t)	 ;CROCK, CROCK. This is TERRIBLE code.
		        (t t))
;;;If there is BPTU then CSEMIUP must succeed.
;;;Likewise for BPTD.
		  (setq ratans
		        (if ratterms
			    (let (($intanalysis nil))
			      ;; The original integrand was already
			      ;; determined to have no poles by initial-analysis.
			      ;; If individual terms of the expansion have poles, the poles 
			      ;; must cancel each other out, so we can ignore them.
			      (try-defint (m// (m+l ratterms) d) ivar '$minf '$inf))
			    0))
		  ;; if integral of ratterms is divergent, ratans is nil, 
		  ;; and mtosc returns nil

		  (cond (*bptu* (setq upans (csemiup (m+l *bptu*) d ivar)))
		        (t (setq upans 0)))
		  (cond (*bptd* (setq downans (csemidown (m+l *bptd*) d ivar)))
		        (t (setq downans 0))))
	   
	     (sratsimp (m+ ratans
			   (m* '$%pi (m+ upans (m- downans))))))))))


(defun evenfn (e ivar)
  (let ((temp (m+ (m- e)
		  (cond ((atom ivar)
			 ($substitute (m- ivar) ivar e))
			(t ($ratsubst (m- ivar) ivar e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 (sratsimp temp))
	   t)
	  (t nil))))

(defun oddfn (e ivar)
  (let ((temp (m+ e (cond ((atom ivar)
			   ($substitute (m- ivar) ivar e))
			  (t ($ratsubst (m- ivar) ivar e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 (sratsimp temp))
	   t)
	  (t nil))))

(defun ztoinf (grand ivar ll ul)
  (prog (n d sn sd varlist
	 s nc dc
	 ans r $savefactors *checkfactors* temp test-var
         nn-var dn-var)
     (setq $savefactors t sn (setq sd (list 1.)))
     (cond ((eq ($sign (m+ *loopstop* -1))
		'$pos)
	    (return nil))
	   ((setq temp (or (scaxn grand ivar)
			   (ssp grand ivar ll ul)))
	    (return temp))
	   ((involve-var grand ivar '(%sin %cos %tan))
	    (setq grand (sconvert grand ivar))
	    (go on)))

     (cond ((polyinx grand ivar nil)
	    (diverg))
	   ((and (ratp grand ivar)
		 (mtimesp grand)
		 (andmapcar #'(lambda (e)
                                (multiple-value-bind (result new-sn new-sd)
                                    (snumden-var e ivar sn sd)
                                  (when result
                                    (setf sn new-sn
                                          sd new-sd))
                                  result))
                            (cdr grand)))
	    (setq nn-var (m*l sn)
		  sn nil)
	    (setq dn-var (m*l sd)
		  sd nil))
	   (t (multiple-value-setq (nn-var dn-var)
                (numden-var grand ivar))))
;;;
;;;New section.
     (setq n (rmconst1 nn-var ivar))
     (setq d (rmconst1 dn-var ivar))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d ivar nil)
	    (setq s (deg-var d ivar)))
	   (t (go findout)))
     (cond ((and (setq r (findp n ivar))
		 (eq (ask-integer r '$integer) '$yes)
		 (setq test-var (bxm d s ivar))
		 (setq ans (apply 'fan (cons (m+ 1. r) test-var))))
	    (return (m* (m// nc dc) (sratsimp ans))))
	   ((and (ratp grand ivar)
		 (setq ans (zmtorat n (cond ((mtimesp d) d)
					    (t ($sqfr d)))
				    s
                                    #'(lambda (n d s)
                                        (ztorat n d s ivar))
                                    ivar)))
		   (return (m* (m// nc dc) ans)))
	   ((and (evenfn d ivar)
		 (setq nn-var (p*lognxp n s ivar)))
	    (setq ans (log*rat (car nn-var) d (cadr nn-var) ivar))
	    (return (m* (m// nc dc) ans)))
	   ((involve-var grand ivar '(%log))
	    (cond ((setq ans (logquad0 grand ivar))
		   (return (m* (m// nc dc) ans)))
		  (t (return nil)))))
     findout
     (cond ((setq temp (batapp grand ivar ll ul))
	    (return temp))
	   (t nil))
     on
     (cond ((let ((*mtoinf* nil))
	      (setq temp (ggr grand t ivar)))
	    (return temp))
	   ((mplusp grand)
	    (cond ((let ((*nodiverg* t))
		     (setq ans (catch 'divergent
				 (andmapcar #'(lambda (g)
						(ztoinf g ivar ll ul))
					    (cdr grand)))))
		   (cond ((eq ans 'divergent) nil)
			 (t (return (sratsimp (m+l ans)))))))))

     (cond ((and (evenfn grand ivar)
		 (setq *loopstop* (m+ 1 *loopstop*))
		 (setq ans (method-by-limits grand ivar '$minf '$inf)))
	    (return (m*t '((rat) 1. 2.) ans)))
	   (t (return nil)))))

(defun ztorat (n d s ivar)
  (cond ((and (null *dflag*)
	      (setq s (difapply ivar n d s #'(lambda (n d s)
                                          (ztorat n d s ivar)))))
	 s)
	((setq n (let ((plogabs ()))
		   (keyhole (let ((var ivar))
                              (declare (special var))
                              ;; It's very important here to bind VAR
                              ;; because the PLOG simplifier checks
                              ;; for VAR.  Without this, the
                              ;; simplifier converts plog(-x) to
                              ;; log(x)+%i*%pi, which messes up the
                              ;; keyhole routine.
                              (m* `((%plog) ,(m- ivar)) n))
                            d
                            ivar)))
	 (m- n))
	(t
	 ;; Let's not signal an error here.  Return nil so that we
	 ;; eventually return a noun form if no other algorithm gives
	 ;; a result.
	 #+(or)
	 (merror (intl:gettext "defint: keyhole integration failed.~%"))
	 nil)))

;;(setq *dflag* nil)

(defun logquad0 (exp ivar)
  (let ((a ()) (b ())  (c ()))
    (cond ((setq exp (logquad exp ivar))
	   (setq a (car exp) b (cadr exp) c (caddr exp))
	   ($asksign b)	  ;let the data base know about the sign of B.
	   (cond ((eq ($asksign c) '$pos)
		  (setq c (m^ (m// c a) '((rat) 1. 2.)))
		  (setq b (simplify
			   `((%acos) ,(add* 'epsilon (m// b (mul* 2. a c))))))
		  (setq a (m// (m* b `((%log) ,c))
			       (mul* a (simplify `((%sin) ,b)) c)))
		  (get-limit a 'epsilon 0 '$plus))))
	  (t ()))))

(defun logquad (exp ivar)
  (let ((varlist (list ivar)))
    (newvar exp)
    (setq exp (cdr (ratrep* exp)))
    (cond ((and (alike1 (pdis (car exp))
			`((%log) ,ivar))
		(not (atom (cdr exp)))
		(equal (cadr (cdr exp)) 2.)
		(not (equal (ptterm (cddr exp) 0.) 0.)))
	   (setq exp (mapcar 'pdis (cdr (oddelm (cdr exp)))))))))

(defun mtoinf (grand ivar ll ul)
  (prog (ans ans1 sd sn pp pe n d s nc dc $savefactors *checkfactors* temp
         nn-var dn-var)
     (setq $savefactors t)
     (setq sn (setq sd (list 1.)))
     (cond ((eq ($sign (m+ *loopstop* -1)) '$pos)
	    (return nil))
	   ((involve-var grand ivar '(%sin %cos))
	    (cond ((and (evenfn grand ivar)
			(or (setq temp (scaxn grand ivar))
			    (setq temp (ssp grand ivar ll ul))))
		   (return (m*t 2. temp)))
		  ((setq temp (mtosc grand ivar))
		   (return temp))
		  (t (go en))))
	   ((among '$%i (%einvolve-var grand ivar))
	    (cond ((setq temp (mtosc grand ivar))
		   (return temp))
		  (t (go en)))))
     (setq grand ($exponentialize grand))	; exponentializing before numden 
     (cond ((polyinx grand ivar nil)		;  avoids losing multiplicities [ 1309432 ]
	    (diverg))
	   ((and (ratp grand ivar)
		 (mtimesp grand)
		 (andmapcar #'(lambda (e)
                                (multiple-value-bind (result new-sn new-sd)
                                    (snumden-var e ivar sn sd)
                                  (when result
                                    (setf sn new-sn
                                          sd new-sd))
                                  result))
                            (cdr grand)))
	    (setq nn-var (m*l sn) sn nil)
	    (setq dn-var (m*l sd) sd nil))
	   (t (multiple-value-setq (nn-var dn-var)
                (numden-var grand ivar))))
     (setq n (rmconst1 nn-var ivar))
     (setq d (rmconst1 dn-var ivar))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d ivar nil)
	    (setq s (deg-var d ivar))))
     (cond ((and (not (%einvolve-var grand ivar))
		 (notinvolve-var exp ivar '(%sinh %cosh %tanh))
		 (setq pp (findp n ivar))
		 (eq (ask-integer pp '$integer) '$yes)
		 (setq pe (bxm d s ivar)))
	    (cond ((and (eq (ask-integer (caddr pe) '$even) '$yes)
			(eq (ask-integer pp '$even) '$yes))
		   (cond ((setq ans (apply 'fan (cons (m+ 1. pp) pe)))
			  (setq ans (m*t 2. ans))
			  (return (m* (m// nc dc) ans)))))
		  ((equal (car pe) 1.)
		   (cond ((and (setq ans (apply 'fan (cons (m+ 1. pp) pe)))
			       (setq nn-var (fan (m+ 1. pp)
					      (car pe)
					      (m* -1 (cadr pe))
					      (caddr pe)
					      (cadddr pe))))
			  (setq ans (m+ ans (m*t (m^ -1 pp) nn-var)))
			  (return (m* (m// nc dc) ans))))))))

     (labels
         ((pppin%ex (nd ivar)
            ;; Test to see if exp is of the form p(x)*f(exp(x)).  If so, set pp to
            ;; be p(x) and set pe to f(exp(x)).
            (setq nd ($factor nd))
            (cond ((polyinx nd ivar nil)
	           (setq pp (cons nd pp)) t)
	          ((catch 'pin%ex (pin%ex nd ivar))
	           (setq pe (cons nd pe)) t)
	          ((mtimesp nd)
	           (andmapcar #'(lambda (ex)
                                  (pppin%ex ex ivar))
                              (cdr nd))))))
       (cond ((and (ratp grand ivar)
	           (setq ans1 (zmtorat n
                                       (cond ((mtimesp d) d) (t ($sqfr d)))
                                       s
                                       #'(lambda (n d s)
                                           (mtorat n d s ivar))
                                       ivar)))
	      (setq ans (m*t '$%pi ans1))
	      (return (m* (m// nc dc) ans)))
	     ((and (or (%einvolve-var grand ivar)
		       (involve-var grand ivar '(%sinh %cosh %tanh)))
		   (pppin%ex n ivar)  ;setq's P* and PE*...Barf again.
		   (setq ans (catch 'pin%ex (pin%ex d ivar))))
	      ;; We have an integral of the form p(x)*F(exp(x)), where
	      ;; p(x) is a polynomial.
	      (cond ((null pp)
		     ;; No polynomial
		     (return (dintexp grand ivar ll ul)))
		    ((not (and (zerop1 (get-limit grand ivar '$inf))
			       (zerop1 (get-limit grand ivar '$minf))))
		     ;; These limits must exist for the integral to converge.
		     (diverg))
		    ((setq ans (rectzto%pi2 (m*l pp) (m*l pe) d ivar))
		     ;; This only handles the case when the F(z) is a
		     ;; rational function.
		     (return (m* (m// nc dc) ans)))
		    ((setq ans (log-transform (m*l pp) (m*l pe) d ivar ul))
		     ;; If we get here, F(z) is not a rational function.
		     ;; We transform it using the substitution x=log(y)
		     ;; which gives us an integral of the form
		     ;; p(log(y))*F(y)/y, which maxima should be able to
		     ;; handle.
		     (return (m* (m// nc dc) ans)))
		    (t
		     ;; Give up.  We don't know how to handle this.
		     (return nil))))))
     en
     (cond ((setq ans (ggrm grand ivar))
	    (return ans))
	   ((and (evenfn grand ivar)
		 (setq *loopstop* (m+ 1 *loopstop*))
		 (setq ans (method-by-limits grand ivar 0 '$inf)))
	    (return (m*t 2. ans)))
	   (t (return nil)))))

(defun linpower0 (exp ivar)
  (cond ((and (setq exp (linpower exp ivar))
	      (eq (ask-integer (caddr exp) '$even)
		  '$yes)
	      (ratgreaterp 0. (car exp)))
	 exp)))

;;; given (b*x+a)^n+c returns  (a b n c)
(defun linpower (exp ivar)
  (let (linpart deg lc c varlist)
    (cond ((not (polyp-var exp ivar))   nil)
	  (t (let ((varlist (list ivar)))
	       (newvar exp)
	       (setq linpart (cadr (ratrep* exp)))
	       (cond ((atom linpart)
		      nil)
		     (t (setq deg (cadr linpart))
;;;get high degree of poly
			(setq linpart ($diff exp ivar (m+ deg -1)))
;;;diff down to linear.
			(setq lc (sdiff linpart ivar))
;;;all the way to constant.
			(setq linpart (sratsimp (m// linpart lc)))
			(setq lc (sratsimp (m// lc `((mfactorial) ,deg))))
;;;get rid of factorial from differentiation.
			(setq c (sratsimp (m+ exp (m* (m- lc)
						      (m^ linpart deg)))))))
;;;Sees if can be expressed as (a*x+b)^n + part freeof x.
	       (cond ((not (among ivar c))
		      `(,lc ,linpart ,deg ,c))
		     (t nil)))))))

(defun mtorat (n d s ivar)
  (let ((*semirat* t))
    (cond ((and (null *dflag*)
		(setq s (difapply ivar n d s #'(lambda (n d s)
                                            (mtorat n d s ivar)))))
	   s)
	  (t (csemiup n d ivar)))))

(defun zmtorat (n d s fn1 ivar)
  (prog (c)
     (cond ((eq ($sign (m+ s (m+ 1 (setq nn* (deg-var n ivar)))))
		'$neg)
	    (diverg))
	   ((eq ($sign (m+ s -4))
		'$neg)
	    (go on)))
     (setq d ($factor d))
     (setq c (rmconst1 d ivar))
     (setq d (cdr c))
     (setq c (car c))
     (cond
       ((mtimesp d)
	(setq d (cdr d))
	(setq n (partnum n d ivar))
	(let ((rsn* t))
	  (setq n ($xthru (m+l
			   (mapcar #'(lambda (a b)
				       (let ((foo (funcall fn1 (car a) b (deg-var b ivar))))
					 (if foo (m// foo (cadr a))
						 (return-from zmtorat nil))))
				   n
				   d)))))
	(return (cond (c (m// n c))
		      (t n)))))
     on

     (setq n (funcall fn1 n d s))
     (return (when n (sratsimp (cond (c  (m// n c))
				     (t n)))))))

(defun pfrnum (f g n n2 ivar)
  (let ((varlist (list ivar))  genvar)
    (setq f (polyform f)
	  g (polyform g)
	  n (polyform n)
	  n2 (polyform n2))
    (setq ivar (caadr (ratrep* ivar)))
    (setq f (resprog0-var ivar f g n n2))
    (list (list (pdis (cadr f)) (pdis (cddr f)))
	  (list (pdis (caar f)) (pdis (cdar f))))))

(defun polyform (e)
  (prog (f d)
     (newvar e)
     (setq f (ratrep* e))
     (and (equal (cddr f) 1)
	  (return (cadr f)))
     (and (equal (length (setq d (cddr f))) 3)
	  (not (among (car d)
		      (cadr f)))
	  (return (list (car d)
			(- (cadr d))
			(ptimes (cadr f) (caddr d)))))
     (merror "defint: bug from PFRNUM in RESIDU.")))

(defun partnum (n dl ivar)
  (let ((n2 1)  ans nl)
    (do ((dl dl (cdr dl)))
	((null (cdr dl))
	 (nconc ans (ncons (list n n2))))
      (setq nl (pfrnum (car dl) (m*l (cdr dl)) n n2 ivar))
      (setq ans (nconc ans (ncons (car nl))))
      (setq n2 (cadadr nl) n (caadr nl) nl nil))))

(defun ggrm (e ivar)
  (prog (poly expo *mtoinf* mb  varlist  genvar l c gvar)
     (setq varlist (list ivar))
     (setq *mtoinf* t)
     (cond ((and (setq expo (%einvolve-var e ivar))
		 (polyp-var (setq poly (sratsimp (m// e (m^t '$%e expo)))) ivar)
		 (setq l (catch 'ggrm (ggr (m^t '$%e expo) nil ivar))))
	    (setq *mtoinf* nil)
	    (setq mb (m- (subin-var 0. (cadr l) ivar)))
	    (setq poly (m+ (subin-var (m+t mb ivar) poly ivar)
			   (subin-var (m+t mb (m*t -1 ivar)) poly ivar))))
	   (t (return nil)))
     (setq expo (caddr l)
	   c (cadddr l)
	   l (m* -1 (car l))
	   e nil)
     (newvar poly)
     (setq poly (cdr (ratrep* poly)))
     (setq mb (m^ (pdis (cdr poly)) -1)
	   poly (car poly))
     (setq gvar (caadr (ratrep* ivar)))
     (cond ((or (atom poly)
		(pointergp gvar (car poly)))
	    (setq poly (list 0. poly)))
	   (t (setq poly (cdr poly))))
     (return (do ((poly poly (cddr poly)))
		 ((null poly)
		  (mul* (m^t '$%e c) (m^t expo -1) mb (m+l e)))
	       (setq e (cons (ggrm1 (car poly) (pdis (cadr poly)) l expo)
			     e))))))

(defun ggrm1 (d k a b)
  (setq b (m// (m+t 1. d) b))
  (m* k `((%gamma) ,b) (m^ a (m- b))))

;; Compute the integral(n/d,x,0,inf) by computing the negative of the
;; sum of residues of log(-x)*n/d over the poles of n/d inside the
;; keyhole contour.  This contour is basically an disk with a slit
;; along the positive real axis.  n/d must be a rational function.
(defun keyhole (n d ivar)
  (let* ((*semirat* ())
	 (res (res-var ivar n d
		       #'(lambda (j)
		           ;; Ok if not on the positive real axis.
		           (or (not (equal ($imagpart j) 0))
			       (eq ($asksign j) '$neg)))
		       #'(lambda (j)
		           (cond ((eq ($asksign j) '$pos)
			          t)
			         (t (diverg)))))))
    (when res
      (let ((rsn* t))
	($rectform ($multthru (m+ (cond ((car res)
					 (car res))
					(t 0.))
				  (cond ((cadr res)
					 (cadr res))
					(t 0.)))))))))

;; Look at an expression e of the form sin(r*x)^k, where k is an
;; integer.  Return the list (1 r k).  (Not sure if the first element
;; of the list is always 1 because I'm not sure what partition is
;; trying to do here.)
(defun skr (e ivar)
  (prog (m r k)
     (cond ((atom e) (return nil)))
     (setq e (partition e ivar 1))
     (setq m (car e))
     (setq e (cdr e))
     (cond ((setq r (sinrx e ivar))
	    (return (list m r 1)))
	   ((and (mexptp e)
		 (eq (ask-integer (setq k (caddr e)) '$integer) '$yes)
		 (setq r (sinrx (cadr e) ivar)))
	    (return (list m r k))))))

;; Look at an expression e of the form sin(r*x) and return r.
(defun sinrx (e ivar)
  (cond ((and (consp e) (eq (caar e) '%sin))
	 (cond ((eq (cadr e) ivar)
		1.)
	       ((and (setq e (partition (cadr e) ivar 1))
		     (eq (cdr e) ivar))
		(car e))))))



;; integrate(a*sc(r*x)^k/x^n,x,0,inf).
(defun ssp (exp ivar ll ul)
  (prog (u n c arg)
     ;; Get the argument of the involved trig function.
     (when (null (setq arg (involve-var exp ivar '(%sin %cos))))
       (return nil))
     ;; I don't think this needs to be special.
     #+nil
     (declare (special n))
     ;; Replace (1-cos(arg)^2) with sin(arg)^2.
     (setq exp ($substitute ;(m^t `((%sin) ,ivar) 2.)
                            ;(m+t 1. (m- (m^t `((%cos) ,ivar) 2.)))
                            ;; The code from above generates expressions with
                            ;; a missing simp flag. Furthermore, the 
                            ;; substitution has to be done for the complete
                            ;; argument of the trig function. (DK 02/2010)
                            `((mexpt simp) ((%sin simp) ,arg) 2)
                            `((mplus) 1 ((mtimes) -1 ((mexpt) ((%cos) ,arg) 2)))
                            exp))
     (multiple-value-bind (u dn)
         (numden-var exp ivar)
       (cond ((and (setq n (findp dn ivar))
		   (eq (ask-integer n '$integer) '$yes))
	      ;; n is the power of the denominator.
	      (cond ((setq c (skr u ivar))
		     ;; The simple case.
		     (return (scmp c n ivar ll ul)))
		    ((and (mplusp u)
			  (setq c (andmapcar #'(lambda (uu)
                                                 (skr uu ivar))
                                             (cdr u))))
		     ;; Do this for a sum of such terms.
		     (return (m+l (mapcar #'(lambda (j) (scmp j n ivar ll ul))
					  c))))))))))

;; We have an integral of the form sin(r*x)^k/x^n.  C is the list (1 r k).
;;
;; The substitution y=r*x converts this integral to
;;
;;   r^(n-1)*integral(sin(y)^k/y^n,y,0,inf)
;;
;; (If r is negative, we need to negate the result.)
;;
;; The recursion Wang gives on p. 87 has a typo.  The second term
;; should be subtracted from the first.  This formula is given in G&R,
;; 3.82, formula 12.
;;
;; integrate(sin(x)^r/x^s,x) =
;;    r*(r-1)/(s-1)/(s-2)*integrate(sin(x)^(r-2)/x^(s-2),x)
;;    - r^2/(s-1)/(s-2)*integrate(sin(x)^r/x^(s-2),x)
;;
;; (Limits are assumed to be 0 to inf.)
;;
;; This recursion ends up with integrals with s = 1 or 2 and
;;
;; integrate(sin(x)^p/x,x,0,inf) = integrate(sin(x)^(p-1),x,0,%pi/2)
;;
;; with p > 0 and odd.  This latter integral is known to maxima, and
;; it's value is beta(p/2,1/2)/2.
;;
;; integrate(sin(x)^2/x^2,x,0,inf) = %pi/2*binomial(q-3/2,q-1)
;;
;; where q >= 2.
;;
(defun scmp (c n ivar ll ul)
  ;; Compute sign(r)*r^(n-1)*integrate(sin(y)^k/y^n,y,0,inf)
  (destructuring-bind (mult r k)
      c
    (let ((recursion (sinsp k n)))
      (if recursion
	  (m* mult
	      (m^ r (m+ n -1))
	      `((%signum) ,r)
	      recursion)
          ;; Recursion failed.  Return the integrand
          ;; The following code generates expressions with a missing simp flag 
          ;; for the sin function. Use better simplifying code. (DK 02/2010)
;	  (let ((integrand (div (pow `((%sin) ,(m* r ivar))
;				     k)
;				(pow ivar n))))
          (let ((integrand (div (power (take '(%sin) (mul r ivar))
                                       k)
                                (power ivar n))))
	    (m* mult
		`((%integrate) ,integrand ,ivar ,ll ,ul)))))))

;; integrate(sin(x)^n/x^2,x,0,inf) = pi/2*binomial(n-3/2,n-1).
;; Express in terms of Gamma functions, though.
(defun sevn (n)
  (m* half%pi ($makegamma `((%binomial) ,(m+t (m+ n -1) '((rat) -1 2))
			    ,(m+ n -1)))))


;; integrate(sin(x)^n/x,x,0,inf) = beta((n+1)/2,1/2)/2, for n odd and
;; n > 0.
(defun sforx (n)
  (cond ((equal n 1.)
	 half%pi)
	(t (bygamma (m+ n -1) 0.))))

;; This implements the recursion for computing
;; integrate(sin(y)^l/y^k,y,0,inf).  (Note the change in notation from
;; the above!)
(defun sinsp (l k)
  (let ((i ())
	(j ()))
    (cond ((eq ($sign (m+ l (m- (m+ k -1))))
	       '$neg)
	   ;; Integral diverges if l-(k-1) < 0.
	   (diverg))
	  ((not (even1 (m+ l k)))
	   ;; If l + k is not even, return NIL.  (Is this the right
	   ;; thing to do?)
	   nil)
	  ((equal k 2.)
	   ;; We have integrate(sin(y)^l/y^2).  Use sevn to evaluate.
	   (sevn (m// l 2.)))
	  ((equal k 1.)
	   ;; We have integrate(sin(y)^l/y,y)
	   (sforx l))
	  ((eq ($sign  (m+ k -2.))
	       '$pos)
	   (setq i (m* (m+ k -1)
		       (setq j (m+ k -2.))))
	   ;; j = k-2, i = (k-1)*(k-2)
	   ;;
	   ;;
	   ;; The main recursion:
	   ;;
	   ;; i(sin(y)^l/y^k)
	   ;;    = l*(l-1)/(k-1)/(k-2)*i(sin(y)^(l-2)/y^k)
	   ;;      - l^2/(k-1)/(k-1)*i(sin(y)^l/y^(k-2))
	   (m+ (m* l (m+ l -1)
		   (m^t i -1)
		   (sinsp (m+ l -2.) j))
	       (m* (m- (m^ l 2))
		   (m^t i -1)
		   (sinsp l j)))))))

;; Returns the fractional part of a?
(defun fpart (a)
  (cond ((null a) 0.)
	((numberp a)
	 ;; Why do we return 0 if a is a number?  Perhaps we really
	 ;; mean integer?
	 0.)
	((mnump a)
	 ;; If we're here, this basically assumes a is a rational.
	 ;; Compute the remainder and return the result.
	 (list (car a) (rem (cadr a) (caddr a)) (caddr a)))
	((and (atom a) (abless1 a)) a)
	((and (mplusp a)
	      (null (cdddr a))
	      (abless1 (caddr a)))
	 (caddr a))))

;; Doesn't appear to be used anywhere in Maxima.  Not sure what this
;; was intended to do.
#+nil
(defun thrad (e)
  (cond ((polyinx e var nil) 0.)
	((and (mexptp e)
	      (eq (cadr e) var)
	      (mnump (caddr e)))
	 (fpart (caddr e)))
	((mtimesp e)
	 (m+l (mapcar #'thrad e)))))


;;; THE FOLLOWING FUNCTION IS FOR TRIG FUNCTIONS OF THE FOLLOWING TYPE:
;;; LOWER LIMIT=0 B A MULTIPLE OF %PI SCA FUNCTION OF SIN (X) COS (X)
;;; B<=%PI2

(defun period (p e ivar)
  (and (alike1 (no-err-sub-var ivar e ivar)
               (setq e (no-err-sub-var (m+ p ivar) e ivar)))
       ;; means there was no error
       (not (eq e t))))

; returns cons of (integer_part . fractional_part) of a
(defun infr (a)
  ;; I think we really want to compute how many full periods are in a
  ;; and the remainder.
  (let* ((q (igprt (div a (mul 2 '$%pi))))
	 (r (add a (mul -1 (mul q 2 '$%pi)))))
    (cons q r)))

; returns cons of (integer_part . fractional_part) of a
(defun lower-infr (a)
  ;; I think we really want to compute how many full periods are in a
  ;; and the remainder.
  (let* (;(q (igprt (div a (mul 2 '$%pi))))
	 (q (mfuncall '$ceiling (div a (mul 2 '$%pi))))
	 (r (add a (mul -1 (mul q 2 '$%pi)))))
    (cons q r)))


;; Return the integer part of r.
(defun igprt (r)
  ;; r - fpart(r)
  (mfuncall '$floor r))


;;;Try making exp(%i*ivar) --> yy, if result is rational then do integral
;;;around unit circle. Make corrections for limits of integration if possible.
(defun scrat (sc b ivar)
  (let* ((exp-form (sconvert sc ivar))	;Exponentialize
	 (rat-form (maxima-substitute 'yy (m^t '$%e (m*t '$%i ivar))
				      exp-form))) ;Try to make Rational fun.
    (cond ((and (ratp rat-form 'yy)
		(not (among ivar rat-form)))
	   (cond ((alike1 b %pi2)
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans ans)
			  (t nil))))
		 ((and (eq b '$%pi)
		       (evenfn exp-form ivar))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 2.) ans))
			  (t nil))))
		 ((and (alike1 b half%pi)
		       (evenfn exp-form ivar)
		       (alike1 rat-form
			       (no-err-sub-var (m+t '$%pi (m*t -1 ivar))
					       rat-form
                                               ivar)))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 4.) ans))
			  (t nil)))))))))

;;; Do integrals of sin and cos. this routine makes sure lower limit
;;; is zero.
(defun intsc1 (a b e ivar)
  ;; integrate(e,var,a,b)
  (let ((trigarg (find-first-trigarg e))
	($%emode t)
	($trigsign t)
	(*sin-cos-recur* t))		;recursion stopper
    (prog (ans d nzp2 l int-zero-to-d int-nzp2 int-zero-to-c limit-diff)
       (let* ((arg (simple-trig-arg trigarg ivar))	;; pattern match sin(cc*x + bb)
	      (cc (cdras 'c arg))
	      (bb (cdras 'b arg))
	      (new-var (gensym "NEW-VAR-")))
	(putprop new-var t 'internal)	  
	 (when (or (not arg)
		   (not (every-trigarg-alike e trigarg)))
	   (return nil))
	 (when (not (and (equal cc 1) (equal bb 0)))
	   (setq e (div (maxima-substitute (div (sub new-var bb) cc)
					   ivar e)
			cc))
	   (setq ivar new-var)	;; change of variables to get sin(new-var)
	   (setq a (add bb (mul a cc)))
	   (setq b (add bb (mul b cc)))))
       (setq limit-diff (m+ b (m* -1 a)))
       (when (or (not (period %pi2 e ivar))
		 (member a *infinities*)
		 (member b *infinities*)
		 (not (and ($constantp a)
			   ($constantp b))))
	 ;; Exit if b or a is not a constant or if the integrand
	 ;; doesn't appear to have a period of 2 pi.
	 (return nil))
       
       ;; Multiples of 2*%pi in limits.
       (cond ((integerp (setq d (let (($float nil))
				 (m// limit-diff %pi2))))
	      (cond ((setq ans (intsc e %pi2 ivar))
		     (return (m* d ans)))
		    (t (return nil)))))
       
       ;; The integral is not over a full period (2*%pi) or multiple
       ;; of a full period.  

       ;; Wang p. 111: The integral integrate(f(x),x,a,b) can be
       ;; written as:
       ;;
       ;;   n * integrate(f,x,0,2*%pi) + integrate(f,x,0,c)
       ;;     - integrate(f,x,0,d)
       ;;
       ;; for some integer n and d >= 0, c < 2*%pi because there exist
       ;; integers p and q such that a = 2 * p *%pi + d and b = 2 * q
       ;; * %pi + c.  Then n = q - p.

       ;; Compute q and c for the upper limit b.
       (setq b (infr b))
       (setq l a)
       (cond ((null l)
	      (setq nzp2 (car b))
	      (setq int-zero-to-d 0.)
	      (go out)))
       ;; Compute p and d for the lower limit a.
       (setq l (infr l))
       ;; avoid an extra trip around the circle - helps skip principal values
       (if (ratgreaterp (car b) (car l))		; if q > p
	   (setq l (cons (add 1 (car l))		;   p += 1
			 (add (mul -1 %pi2) (cdr l))))) ;   d -= 2*%pi
       
       ;; Compute -integrate(f,x,0,d)
       (setq int-zero-to-d
	     (cond ((setq ans (try-intsc e (cdr l) ivar))
		    (m*t -1 ans))
		   (t  nil)))
       ;; Compute n = q - p (stored in nzp2)
       (setq nzp2 (m+ (car b) (m- (car l))))
       out
       ;; Compute n*integrate(f,x,0,2*%pi)
       (setq int-nzp2 (cond ((zerop1 nzp2)
			      ;; n = 0
			      0.)
			     ((setq ans (try-intsc e %pi2 ivar))
			      ;; n is not zero, so compute
			      ;; integrate(f,x,0,2*%pi)
			      (m*t nzp2 ans))
			     ;; Unable to compute integrate(f,x,0,2*%pi)
			     (t nil)))
       ;; Compute integrate(f,x,0,c)
       (setq int-zero-to-c (try-intsc e (cdr b) ivar))

       (return (cond ((and int-zero-to-d int-nzp2 int-zero-to-c)
		      ;; All three pieces succeeded.
		      (add* int-zero-to-d int-nzp2 int-zero-to-c))
		     ((ratgreaterp %pi2 limit-diff)
		      ;; Less than 1 full period, so intsc can integrate it.
		      ;; Apply the substitution to make the lower limit 0.
		      ;; This is last resort because substitution often causes intsc to fail.
		      (intsc (maxima-substitute (m+ a ivar) ivar e)
			     limit-diff ivar))
		     ;; nothing worked
		     (t nil))))))

;; integrate(sc, var, 0, b), where sc is f(sin(x), cos(x)).
;; calls intsc with a wrapper to just return nil if integral is divergent,
;;  rather than generating an error.
(defun try-intsc (sc b ivar)
  (let* ((*nodiverg* t)
	 (ans (catch 'divergent (intsc sc b ivar))))
    (if (eq ans 'divergent)
	nil
      ans)))

;; integrate(sc, ivar, 0, b), where sc is f(sin(x), cos(x)).  I (rtoy)
;; think this expects b to be less than 2*%pi.
(defun intsc (sc b ivar)
  (if (zerop1 b)
      0
      (multiple-value-bind (b sc)
	  (cond ((eq ($sign b) '$neg)
		 (values (m*t -1 b)
			 (m* -1 (subin-var (m*t -1 ivar) sc ivar))))
		(t
		 (values b sc)))
	;; Partition the integrand SC into the factors that do not
	;; contain VAR (the car part) and the parts that do (the cdr
	;; part).
	(setq sc (partition sc ivar 1))
	(cond ((setq b (intsc0 (cdr sc) b ivar))
	       (m* (resimplify (car sc)) b))))))

;; integrate(sc, ivar, 0, b), where sc is f(sin(x), cos(x)).
(defun intsc0 (sc b ivar)
  ;; Determine if sc is a product of sin's and cos's.
  (let ((nn* (scprod sc ivar))
	(dn* ()))
    (cond (nn*
	   ;; We have a product of sin's and cos's.  We handle some
	   ;; special cases here.
	   (cond ((alike1 b half%pi)
		  ;; Wang p. 110, formula (1):
		  ;; integrate(sin(x)^m*cos(x)^n, x, 0, %pi/2) =
		  ;;   gamma((m+1)/2)*gamma((n+1)/2)/2/gamma((n+m+2)/2)
		  (bygamma (car nn*) (cadr nn*)))
		 ((eq b '$%pi)
		  ;; Wang p. 110, near the bottom, says
		  ;;
		  ;; int(f(sin(x),cos(x)), x, 0, %pi) =
		  ;;   int(f(sin(x),cos(x)) + f(sin(x),-cos(x)),x,0,%pi/2)
		  (cond ((eq (real-branch (cadr nn*) -1) '$yes)
			 (m* (m+ 1. (m^ -1 (cadr nn*)))
			     (bygamma (car nn*) (cadr nn*))))))
		 ((alike1 b %pi2)
		  (cond ((or (and (eq (ask-integer (car nn*) '$even)
				      '$yes)
				  (eq (ask-integer (cadr nn*) '$even)
				      '$yes))
			     (and (ratnump (car nn*))
				  (eq (real-branch (car nn*) -1)
				      '$yes)
				  (ratnump (cadr nn*))
				  (eq (real-branch (cadr nn*) -1)
				      '$yes)))
			 (m* 4.	(bygamma (car nn*) (cadr nn*))))
			((or (eq (ask-integer (car nn*) '$odd) '$yes)
			     (eq (ask-integer (cadr nn*) '$odd) '$yes))
			 0.)
			(t nil)))
		 ((alike1 b half%pi3)
		  ;; Wang, p. 111 says
		  ;;
		  ;; int(f(sin(x),cos(x)),x,0,3*%pi/2) =
		  ;;   int(f(sin(x),cos(x)),x,0,%pi)
		  ;;   + int(f(-sin(x),-cos(x)),x,0,%pi/2)
		  (m* (m+ 1. (m^ -1 (cadr nn*)) (m^ -1 (m+l nn*)))
		      (bygamma (car nn*) (cadr nn*))))))
	  (t
	   ;; We don't have a product of sin's and cos's.
	   (cond ((and (or (eq b '$%pi)
			   (alike1 b %pi2)
			   (alike1 b half%pi))
		       (setq dn* (scrat sc b ivar)))
		  dn*)
		 ((setq nn* (antideriv sc ivar))
		  (sin-cos-intsubs nn* ivar 0. b))
		 (t ()))))))

;;;Is careful about substitution of limits where the denominator may be zero
;;;because of various assumptions made.
(defun sin-cos-intsubs (exp ivar ll ul)
  (cond ((mplusp exp)
	 (let ((l (mapcar #'(lambda (e)
                              (sin-cos-intsubs1 e ivar ll ul))
                          (cdr exp))))
	   (if (not (some #'null l))
	       (m+l l))))
	(t (sin-cos-intsubs1 exp ivar ll ul))))

(defun sin-cos-intsubs1 (exp ivar ll ul)
  (let* ((rat-exp ($rat exp))
	 (denom (pdis (cddr rat-exp))))
    (cond ((equal ($csign denom) '$zero)
	   '$und)
	  (t (try-intsubs exp ll ul ivar)))))

(defun try-intsubs (exp ll ul ivar)
  (let* ((*nodiverg* t)
	 (ans (catch 'divergent (intsubs exp ll ul ivar))))
    (if (eq ans 'divergent)
	nil
      ans)))

(defun try-defint (exp ivar ll ul)
  (let* ((*nodiverg* t)
	 (ans (catch 'divergent (defint exp ivar ll ul))))
    (if (eq ans 'divergent)
	nil
      ans)))

;; Determine whether E is of the form sin(x)^m*cos(x)^n and return the
;; list (m n).
(defun scprod (e ivar)
  (let ((great-minus-1 #'(lambda (temp)
			   (ratgreaterp temp -1)))
	m n)
    (cond
      ((setq m (powerofx e `((%sin) ,ivar) great-minus-1 ivar))
       (list m 0.))
      ((setq n (powerofx e `((%cos) ,ivar) great-minus-1 ivar))
       (setq m 0.)
       (list 0. n))
      ((and (mtimesp e)
	    (or (setq m (powerofx (cadr e) `((%sin) ,ivar) great-minus-1 ivar))
		(setq n (powerofx (cadr e) `((%cos) ,ivar) great-minus-1 ivar)))
	    (cond
	      ((null m)
	       (setq m (powerofx (caddr e) `((%sin) ,ivar) great-minus-1 ivar)))
	      (t (setq n (powerofx (caddr e) `((%cos) ,ivar) great-minus-1 ivar))))
	    (null (cdddr e)))
       (list m n))
      (t ()))))

(defun real-branch (exponent value)
  ;; Says whether (m^t value exponent) has at least one real branch.
  ;; Only works for values of 1 and -1 now.  Returns $yes $no
  ;; $unknown.
  (cond ((equal value 1.)
	 '$yes)
	((eq (ask-integer exponent '$integer) '$yes)
	 '$yes)
	((ratnump exponent)
	 (cond ((eq ($oddp (caddr exponent)) t)
		'$yes)
	       (t '$no)))
	(t '$unknown)))

;; Compute beta((m+1)/2,(n+1)/2)/2.
(defun bygamma (m n)
  (let ((one-half (m//t 1. 2.)))
    (m* one-half `((%beta) ,(m* one-half (m+t 1. m))
		   ,(m* one-half (m+t 1. n))))))

;;Seems like Guys who call this don't agree on what it should return.
(defun powerofx (e x p ivar)
  (setq e (cond ((not (among ivar e)) nil)
		((alike1 e x) 1.)
		((atom e) nil)
		((and (mexptp e)
		      (alike1 (cadr e) x)
		      (not (among ivar (caddr e))))
		 (caddr e))))
  (cond ((null e) nil)
	((funcall p e) e)))


;; Check e for an expression of the form x^kk*(b*x^n+a)^l.  If it
;; matches, Return the two values kk and (list l a n b).
(defun bata0 (e ivar)
  (let (k c)
    (cond ((atom e) nil)
	  ((mexptp e)
	   ;; We have f(x)^y.  Look to see if f(x) has the desired
	   ;; form.  Then f(x)^y has the desired form too, with
	   ;; suitably modified values.
	   ;;
	   ;; XXX: Should we ask for the sign of f(x) if y is not an
	   ;; integer?  This transformation we're going to do requires
	   ;; that f(x)^y be real.
	   (destructuring-bind (mexp base power)
	       e
	     (declare (ignore mexp))
	     (multiple-value-bind (kk cc)
		 (bata0 base ivar)
	       (when kk
		 ;; Got a match.  Adjust kk and cc appropriately.
		 (destructuring-bind (l a n b)
		     cc
		   (values (mul kk power)
			   (list (mul l power) a n b)))))))
	  ((and (mtimesp e)
		(null (cdddr e))
		(or (and (setq k (findp (cadr e) ivar))
			 (setq c (bxm (caddr e) (polyinx (caddr e) ivar nil) ivar)))
		    (and (setq k (findp (caddr e) ivar))
			 (setq c (bxm (cadr e) (polyinx (cadr e) ivar nil) ivar)))))
	   (values k c))
	  ((setq c (bxm e (polyinx e ivar nil) ivar))
	   (setq k 0.)
	   (values k c)))))


;;(DEFUN BATAP (E)
;;  (PROG (K C L)
;;    (COND ((NOT (BATA0 E)) (RETURN NIL))
;;	  ((AND (EQUAL -1. (CADDDR C))
;;		(EQ ($askSIGN (SETQ K (m+ 1. K)))
;;		    '$pos)
;;		(EQ ($askSIGN (SETQ L (m+ 1. (CAR C))))
;;		    '$pos)
;;		(ALIKE1 (CADR C)
;;			(m^ UL (CADDR C)))
;;		(SETQ E (CADR C))
;;		(EQ ($askSIGN (SETQ C (CADDR C))) '$pos))
;;	   (RETURN (M// (m* (m^ UL (m+t K (m* C (m+t -1. L))))
;;			    `(($BETA) ,(SETQ NN* (M// K C))
;;				      ,(SETQ DN* L)))
;;			C))))))


;; Integrals of the form i(log(x)^m*x^k*(a+b*x^n)^l,x,0,ul).  There
;; are two cases to consider: One case has ul>0, b<0, a=-b*ul^n, k>-1,
;; l>-1, n>0, m a nonnegative integer.  The second case has ul=inf, l < 0.
;;
;; These integrals are essentially partial derivatives of the Beta
;; function (i.e. the Eulerian integral of the first kind).  Note
;; that, currently, with the default setting intanalysis:true, this
;; function might not even be called for some of these integrals.
;; However, this can be palliated by setting intanalysis:false.

(defun zto1 (e ivar ul)
  (when (or (mtimesp e) (mexptp e))
    (let ((m 0)
	  (log (list '(%log) ivar)))
      (flet ((set-m (p)
	       (setq m p)))
	(find-if #'(lambda (fac)
		     (powerofx fac log #'set-m ivar))
		 (cdr e)))
      (when (and (freeof ivar m)
		 (eq (ask-integer m '$integer) '$yes)
		 (not (eq ($asksign m) '$neg)))
	(setq e (m//t e (list '(mexpt) log m)))
	(cond
	  ((eq ul '$inf)
	   (multiple-value-bind (kk s d r cc)
	       (batap-inf e ivar)
	     ;; We have i(x^kk/(d+cc*x^r)^s,x,0,inf) =
	     ;; beta(aa,bb)/(cc^aa*d^bb*r).  Compute this, and then
	     ;; differentiate it m times to get the log term
	     ;; incorporated.
	     (when kk
	       (let* ((aa (div (add 1 ivar) r))
		      (bb (sub s aa))
		      (m (if (eq ($asksign m) '$zero)
			     0
			     m)))
	       (let ((res (div `((%beta) ,aa ,bb)
			       (mul (m^t cc aa)
				    (m^t d bb)
				    r))))
		 ($at ($diff res ivar m)
		      (list '(mequal) ivar kk)))))))
	  (t
	   (multiple-value-bind
		 (k/n l n b) (batap-new e ivar ul)
	     (when k/n
	       (let ((beta (ftake* '%beta k/n l))
		     (m (if (eq ($asksign m) '$zero) 0 m)))
		 ;; The result looks like B(k/n,l) ( ... ).
		 ;; Perhaps, we should just $factor, instead of
		 ;; pulling out beta like this.
		 (m*t
		  beta
		  ($fullratsimp
		   (m//t
		    (m*t
		     (m^t (m-t b) (m1-t l))
		     (m^t ul (m*t n (m1-t l)))
		     (m^t n (m-t (m1+t m)))
		     ($at ($diff (m*t (m^t ul (m*t n ivar))
				      (list '(%beta) ivar l))
				 ivar m)
			  (list '(mequal) ivar k/n)))
		    beta))))))))))))


;;; If e is of the form given below, make the obvious change
;;; of variables (substituting ul*x^(1/n) for x) in order to reduce
;;; e to the usual form of the integrand in the Eulerian
;;; integral of the first kind.
;;; N. B: The old version of ZTO1 completely ignored this
;;; substitution; the log(x)s were just thrown in, which,
;;; of course would give wrong results.

(defun batap-new (e ivar ul)
  ;; Parse e
  (multiple-value-bind (k c)
      (bata0 e ivar)
    (when k
      ;; e=x^k*(a+b*x^n)^l
      (destructuring-bind (l a n b)
	  c
	(when (and (freeof ivar k)
		   (freeof ivar n)
		   (freeof ivar l)
		   (alike1 a (m-t (m*t b (m^t ul n))))
		   (eq ($asksign b) '$neg)
		   (eq ($asksign (setq k (m1+t k))) '$pos)
		   (eq ($asksign (setq l (m1+t l))) '$pos)
		   (eq ($asksign n) '$pos))
	  (values (m//t k n) l n b))))))


;; Wang p. 71 gives the following formula for a beta function:
;;
;; integrate(x^(k-1)/(c*x^r+d)^s,x,0,inf)
;;   = beta(a,b)/(c^a*d^b*r)
;;
;; where a = k/r > 0, b = s - a > 0, s > k > 0, r > 0, c*d > 0.
;;
;; This function matches this and returns k-1, d, r, c, a, b.  And
;; also checks that all the conditions hold.  If not, NIL is returned.
;;
(defun batap-inf (e ivar)
  (multiple-value-bind (k c)
      (bata0 e ivar)
    (when k
      (destructuring-bind (l d r cc)
	  c
	(let* ((s (mul -1 l))
	       (kk (add k 1))
	       (a (div kk r))
	       (b (sub s a)))
	  (when (and (freeof ivar k)
		     (freeof ivar r)
		     (freeof ivar l)
		     (eq ($asksign kk) '$pos)
		     (eq ($asksign a) '$pos)
		     (eq ($asksign b) '$pos)
		     (eq ($asksign (sub s k)) '$pos)
		     (eq ($asksign r) '$pos)
		     (eq ($asksign (mul cc d)) '$pos))
	    (values k s d r cc)))))))


;; Handles beta integrals.
(defun batapp (e ivar ll ul)
  (cond ((not (or (equal ll 0)
		  (eq ll '$minf)))
	 (setq e (subin-var (m+ ll ivar) e ivar))))
  (multiple-value-bind (k c)
      (bata0 e ivar)
    (cond ((null k)
	   nil)
	  (t
	   (destructuring-bind (l d al c)
	       c
	     ;; e = x^k*(d+c*x^al)^l.
	     (let ((new-k (m// (m+ 1 k) al)))
	       (when (and (ratgreaterp al 0.)
			  (eq ($asksign new-k) '$pos)
			  (ratgreaterp (setq l (m* -1 l))
				       new-k)
			  (eq ($asksign (m* d c))
			      '$pos))
		 (setq l (m+ l (m*t -1 new-k)))
		 (m// `((%beta) ,new-k ,l)
		      (mul* al (m^ c new-k) (m^ d l))))))))))


;; Compute exp(d)*gamma((c+1)/b)/b/a^((c+1)/b).  In essence, this is
;; the value of integrate(x^c*exp(d-a*x^b),x,0,inf).
(defun gamma1 (c a b d)
  (m* (m^t '$%e d)
      (m^ (m* b (m^ a (setq c (m// (m+t c 1) b)))) -1)
      `((%gamma) ,c)))

(defun zto%pi2 (grand ivar)
  (let ((result (unitcir (sratsimp (m// grand ivar)) ivar)))
    (cond (result (sratsimp (m* (m- '$%i) result)))
	  (t nil))))

;; Evaluates the contour integral of GRAND around the unit circle
;; using residues.
(defun unitcir (grand ivar)
  (multiple-value-bind (nn dn)
      (numden-var grand ivar)
    (let* ((sgn nil)
	   (result (princip (res-var ivar nn dn 
			             #'(lambda (pt)
				         ;; Is pt stricly inside the unit circle?
				         (setq sgn (let ((limitp nil))
					             ($asksign (m+ -1 (cabs pt)))))
				         (eq sgn '$neg))
			             #'(lambda (pt)
				         (declare (ignore pt))
				         ;; Is pt on the unit circle?  (Use
				         ;; the cached value computed
				         ;; above.)
				         (prog1
				             (eq sgn '$zero)
				           (setq sgn nil)))))))
      (when result
        (m* '$%pi result)))))


(defun logx1 (exp ll ul ivar)
  (let ((arg nil))
    (cond
      ((and (notinvolve-var exp ivar '(%sin %cos %tan %atan %asin %acos))
	    (setq arg (involve-var exp ivar '(%log))))
       (cond ((eq arg ivar)
	      (cond ((ratgreaterp 1. ll)
		     (cond ((not (eq ul '$inf))
			    (intcv1 (m^t '$%e (m- 'yx)) (m- `((%log) ,ivar)) ivar ll ul))
			   (t (intcv1 (m^t '$%e 'yx) `((%log) ,ivar) ivar ll ul))))))
	     (t (intcv arg nil ivar ll ul)))))))


;; Wang 81-83.  Unfortunately, the pdf version has page 82 as all
;; black, so here is, as best as I can tell, what Wang is doing.
;; Fortunately, p. 81 has the necessary hints.
;;
;; First consider integrate(exp(%i*k*x^n),x) around the closed contour
;; consisting of the real axis from 0 to R, the arc from the angle 0
;; to %pi/(2*n) and the ray from the arc back to the origin.
;;
;; There are no poles in this region, so the integral must be zero.
;; But consider the integral on the three parts.  The real axis is the
;; integral we want.  The return ray is
;;
;;   exp(%i*%pi/2/n) * integrate(exp(%i*k*(t*exp(%i*%pi/2/n))^n),t,R,0)
;;     = exp(%i*%pi/2/n) * integrate(exp(%i*k*t^n*exp(%i*%pi/2)),t,R,0)
;;     = -exp(%i*%pi/2/n) * integrate(exp(-k*t^n),t,0,R)
;;
;; As R -> infinity, this last integral is gamma(1/n)/k^(1/n)/n.
;;
;; We assume the integral on the circular arc approaches 0 as R ->
;; infinity.  (Need to prove this.)
;;
;; Thus, we have
;;
;;   integrate(exp(%i*k*t^n),t,0,inf)
;;     = exp(%i*%pi/2/n) * gamma(1/n)/k^(1/n)/n.
;;
;; Equating real and imaginary parts gives us the desired results:
;;
;; integrate(cos(k*t^n),t,0,inf) = G * cos(%pi/2/n)
;; integrate(sin(k*t^n),t,0,inf) = G * sin(%pi/2/n)
;;
;; where G = gamma(1/n)/k^(1/n)/n.
;;
(defun scaxn (e ivar)
  (let (ind s g)
    (cond ((atom e)  nil)
	  ((and (or (eq (caar e) '%sin)
		    (eq (caar e) '%cos))
		(setq ind (caar e))
		(setq e (bx**n (cadr e) ivar)))
	   ;; Ok, we have cos(b*x^n) or sin(b*x^n), and we set e = (n
	   ;; b)
	   (cond ((equal (car e) 1.)
		  ;; n = 1.  Give up.  (Why not divergent?)
		  nil)
		 ((zerop (setq s (let ((sign ($asksign (cadr e))))
				   (cond ((eq sign '$pos) 1)
					 ((eq sign '$neg) -1)
					 ((eq sign '$zero) 0)))))
		  ;; s is the sign of b.  Give up if it's zero.
		  nil)
		 ((not (eq ($asksign (m+ -1 (car e)))  '$pos))
		  ;; Give up if n-1 <= 0.  (Why give up?  Isn't the
		  ;; integral divergent?)
		  nil)
		 (t
		  ;; We can apply our formula now.  g = gamma(1/n)/n/b^(1/n)
		  (setq g (gamma1 0. (m* s (cadr e)) (car e) 0.))
		  (setq e (m* g `((,ind) ,(m// half%pi (car e)))))
		  (m* (cond ((and (eq ind '%sin)
				  (equal s -1))
			     -1)
			    (t 1))
		      e)))))))


;; this is the second part of the definite integral package

(defun p*lognxp (a s ivar)
  (let (b)
    (cond ((not (among '%log a))
	   ())
	  ((and (polyinx (setq b (maxima-substitute 1. `((%log) ,ivar) a))
			 ivar t)
		(eq ($sign (m+ s (m+ 1 (deg-var b ivar))))
		    '$pos)
		(evenfn b ivar)
		(setq a (lognxp (sratsimp (m// a b)) ivar)))
	   (list b a)))))

(defun lognxp (a ivar)
  (cond ((atom a) nil)
	((and (eq (caar a) '%log)
	      (eq (cadr a) ivar))
         1.)
	((and (mexptp a)
	      (numberp (caddr a))
	      (lognxp (cadr a) ivar))
	 (caddr a))))

(defun logcpi0 (n d ivar)
  (prog (polelist dp plm rlm factors pl rl pl1 rl1)
     (setq polelist
           (polelist-var ivar d #'upperhalf #'(lambda (j)
					        (cond ((zerop1 j)
                                                       nil)
						      ((equal ($imagpart j) 0)
						       t)))))
     (cond ((null polelist)
	    (return nil)))
     (setq factors (car polelist)
	   polelist (cdr polelist))
     (cond ((or (cadr polelist)
		(caddr polelist))
	    (setq dp (sdiff d ivar))))
     (cond ((setq plm (car polelist))
	    (setq rlm (residue-var ivar
                                   n
                                   (cond (*leadcoef* factors)
					 (t d))
				   plm))))
     (cond ((setq pl (cadr polelist))
	    (setq rl (res1-var ivar n dp pl))))
     (cond ((setq pl1 (caddr polelist))
	    (setq rl1 (res1-var ivar n dp pl1))))
     (return (values
              (m*t (m//t 1. 2.)
		   (m*t '$%pi
		        (princip
			 (list (cond ((setq nn* (append rl rlm))
				      (m+l nn*)))
			       (cond (rl1 (m+l rl1)))))))
              plm
              factors
              pl
              rl
              pl1
              rl1))))

(defun lognx2 (nn dn pl rl)
  (do ((pl pl (cdr pl))
       (rl rl (cdr rl))
       (ans ()))
      ((or (null pl)
	   (null rl))
       ans)
    (setq ans (cons (m* dn (car rl)
                        ;; AFAICT, this call to PLOG doesn't need
                        ;; to bind VAR.
                        (m^ `((%plog) ,(car pl)) nn))
		    ans))))

(defun logcpj (n d i ivar plm pl rl pl1 rl1)
  (setq n (append
	   (if plm
	       (list (mul* (m*t '$%i %pi2)
			   (m+l
                            ;; AFAICT, this call to PLOG doesn't need
                            ;; to bind VAR.  An example where this is
                            ;; used is
                            ;; integrate(log(x)^2/(1+x^2),x,0,1) =
                            ;; %pi^3/16.
			    (residue-var ivar
                                         (m* (m^ `((%plog) ,ivar) i)
                                             n)
				         d
				         plm)))))
	   (lognx2 i (m*t '$%i %pi2) pl rl)
	   (lognx2 i %p%i pl1 rl1)))
  (if (null n)
      0
      (simplify (m+l n))))

;; Handle integral(n(x)/d(x)*log(x)^m,x,0,inf).  n and d are
;; polynomials.
(defun log*rat (n d m ivar)
  (let ((i-vals (make-array (1+ m)))
        (j-vals (make-array (1+ m))))
    (labels
        ((logcpi (n d c ivar)
           (if (zerop c)
               (logcpi0 n d ivar)
               (m* '((rat) 1 2) (m+ (aref j-vals c) (m* -1 (sumi c))))))
         (sumi (c)
           (do ((k 1 (1+ k))
                (ans ()))
               ((= k c)
                (m+l ans))
             (push (mul* ($makegamma `((%binomial) ,c ,k))
		         (m^t '$%pi k)
		         (m^t '$%i k)
		         (aref i-vals (- c k)))
	           ans))))
      (setf (aref j-vals 0) 0)
      (prog (*leadcoef* res)
         (dotimes (c m (return (logcpi n d m ivar)))
           (multiple-value-bind (res plm factors pl rl pl1 rl1)
               (logcpi n d c ivar)
             (setf (aref i-vals c) res)
             (setf (aref j-vals c) (logcpj n factors c ivar plm pl rl pl1 rl1))))))))

(defun fan (p m a n b)
  (let ((povern (m// p n))
	(ab (m// a b)))
    (cond
      ((or (eq (ask-integer povern '$integer) '$yes)
	   (not (equal ($imagpart ab) 0)))  ())
      (t (let ((ind ($asksign ab)))
	   (cond ((eq ind '$zero) nil)
		 ((eq ind '$neg) nil)
		 ((not (ratgreaterp m povern)) nil)
		 (t (m// (m* '$%pi
			     ($makegamma `((%binomial) ,(m+ -1 m (m- povern))
					   ,(m+t -1 m)))
			     `((mabs) ,(m^ a (m+ povern (m- m)))))
			 (m* (m^ b povern)
			     n
			     `((%sin) ,(m*t '$%pi povern)))))))))))


;;Makes a new poly such that np(x)-np(x+2*%i*%pi)=p(x).
;;Constructs general POLY of degree one higher than P with
;;arbitrary coeff. and then solves for coeffs by equating like powers
;;of the varibale of integration.
;;Can probably be made simpler now.

(defun makpoly (p ivar)
  (let ((n (deg-var p ivar))  (ans ())  (varlist ())  (gp ())  (cl ())  (zz ()))
    (setq ans (genpoly (m+ 1 n) ivar)) ;Make poly with gensyms of 1 higher deg.
    (setq cl (cdr ans))			;Coefficient list
    (setq varlist (append cl (list ivar))) ;Make VAR most important.
    (setq gp (car ans))		 ;This is the poly with gensym coeffs.
;;;Now, poly(x)-poly(x+2*%i*%pi)=p(x), P is the original poly.
    (setq ans (m+ gp (subin-var (m+t (m*t '$%i %pi2) ivar) (m- gp) ivar) (m- p)))
    (newvar ans)
    (setq ans (ratrep* ans))	       ;Rational rep with VAR leading.
    (setq zz (coefsolve n cl (cond ((not (eq (caadr ans) ;What is Lead Var.
					     (genfind (car ans) ivar)))
				    (list 0 (cadr ans))) ;No VAR in ans.
				   ((cdadr ans))))) ;The real Poly.
    (if (or (null zz) (null gp))
	-1
	($substitute zz gp))))	       ;Substitute Values for gensyms.

(defun coefsolve (n cl e)
  (do (($breakup)
       (eql (ncons (pdis (ptterm e n))) (cons (pdis (ptterm e m)) eql))
       (m (m+ n -1) (m+ m -1)))
      ((signp l m) (solvex eql cl nil nil))))

;; Integrate(p(x)*f(exp(x))/g(exp(x)),x,minf,inf) by applying the
;; transformation y = exp(x) to get
;; integrate(p(log(y))*f(y)/g(y)/y,y,0,inf).  This should be handled
;; by dintlog.
(defun log-transform (p pe d ivar ul)
  (let ((new-p (subst (list '(%log) ivar) ivar p))
	(new-pe (subst ivar 'z* (catch 'pin%ex (pin%ex pe ivar))))
	(new-d (subst ivar 'z* (catch 'pin%ex (pin%ex d ivar)))))
    (defint (div (div (mul new-p new-pe) new-d) ivar) ivar 0 ul)))

;; This implements Wang's algorithm in Chapter 5.2, pp. 98-100.
;;
;; This is a very brief description of the algorithm.  Basically, we
;; have integrate(R(exp(x))*p(x),x,minf,inf), where R(x) is a rational
;; function and p(x) is a polynomial.
;;
;; We find a polynomial q(x) such that q(x) - q(x+2*%i*%pi) = p(x).
;; Then consider a contour integral of R(exp(z))*q(z) over a
;; rectangular contour.  Opposite corners of the rectangle are (-R,
;; 2*%i*%pi) and (R, 0).
;;
;; Wang shows that this contour integral, in the limit, is the
;; integral of R(exp(x))*q(x)-R(exp(x))*q(x+2*%i*%pi), which is
;; exactly the integral we're looking for.
;;
;; Thus, to find the value of the contour integral, we just need the
;; residues of R(exp(z))*q(z).  The only tricky part is that we want
;; the log function to have an imaginary part between 0 and 2*%pi
;; instead of -%pi to %pi.
(defun rectzto%pi2 (p pe d ivar)
  ;; We have R(exp(x))*p(x) represented as p(x)*pe(exp(x))/d(exp(x)).
  (prog (dp n pl a b c denom-exponential)
     (if (not (and (setq denom-exponential (catch 'pin%ex (pin%ex d ivar)))
		   (%e-integer-coeff pe ivar)
		   (%e-integer-coeff d ivar)))
	 (return ()))
     ;; At this point denom-exponential has converted d(exp(x)) to the
     ;; polynomial d(z), where z = exp(x).
     (setq n (m* (cond ((null p) -1)
		       (t ($expand (m*t '$%i %pi2 (makpoly p ivar)))))
		 pe))
     (let ((*leadcoef* ()))
       ;; Find the poles of the denominator.  denom-exponential is the
       ;; denominator of R(x).
       ;;
       ;; It seems as if polelist returns a list of several items.
       ;; The first element is a list consisting of the pole and (z -
       ;; pole).  We don't care about this, so we take the rest of the
       ;; result.
       (setq pl (cdr (polelist-var 'z* denom-exponential
			           #'(lambda (j)
				       ;; The imaginary part is nonzero,
				       ;; or the realpart is negative.
				       (or (not (equal ($imagpart j) 0))
				           (eq ($asksign ($realpart j)) '$neg)))
			           #'(lambda (j)
				       ;; The realpart is not zero.
				       (not (eq ($asksign ($realpart j)) '$zero)))))))
     ;; Not sure what this does.
     (cond ((null pl)
	    ;; No roots at all, so return
	    (return nil))
	   ((or (cadr pl)
		(caddr pl))
	    ;; We have simple roots or roots in REGION1
	    (setq dp (sdiff d ivar))))
     (cond ((cadr pl)
	    ;; The cadr of pl is the list of the simple poles of
	    ;; denom-exponential.  Take the log of them to find the
	    ;; poles of the original expression.  Then compute the
	    ;; residues at each of these poles and sum them up and put
	    ;; the result in B.  (If no simple poles set B to 0.)
	    (setq b (mapcar #'log-imag-0-2%pi (cadr pl)))
	    (setq b (res1-var ivar n dp b))
	    (setq b (m+l b)))
	   (t (setq b 0.)))
     (cond ((caddr pl)
	    ;; I think this handles the case of poles outside the
	    ;; regions.  The sum of these residues are placed in C.
	    (let ((temp (mapcar #'log-imag-0-2%pi (caddr pl))))
	      (setq c (append temp (mapcar #'(lambda (j)
					       (m+ (m*t '$%i %pi2) j))
					   temp)))
	      (setq c (res1-var ivar n dp c))
	      (setq c (m+l c))))
	   (t (setq c 0.)))
     (cond ((car pl)
	    ;; We have the repeated poles of deonom-exponential, so we
	    ;; need to convert them to the actual pole values for
	    ;; R(exp(x)), by taking the log of the value of poles.
	    (let ((poles (mapcar #'(lambda (p)
				     (log-imag-0-2%pi (car p)))
				 (car pl)))
		  (exp (m// n (subst (m^t '$%e ivar) 'z* denom-exponential))))
	      ;; Compute the residues at all of these poles and sum
	      ;; them up.
	      (setq a (mapcar #'(lambda (j)
				  ($residue exp ivar j))
			      poles))
	      (setq a (m+l a))))
	   (t (setq a 0.)))
     (return (sratsimp (m+ a b (m* '((rat) 1. 2.) c))))))

(defun genpoly (i ivar)
  (do ((i i (m+ i -1))
       (c (gensym) (gensym))
       (cl ())
       (ans ()))
      ((zerop i)
       (cons (m+l ans) cl))
    (setq ans (cons (m* c (m^t ivar i)) ans))
    (setq cl (cons c cl))))

;; Check to see if each term in exp that is of the form exp(k*x) has
;; an integer value for k.
(defun %e-integer-coeff (exp ivar)
  (cond ((mapatom exp) t)
	((and (mexptp exp)
	      (eq (cadr exp) '$%e))
	 (eq (ask-integer ($coeff (caddr exp) ivar) '$integer)
	     '$yes))
	(t (every #'(lambda (e)
                      (%e-integer-coeff e ivar))
                  (cdr exp)))))

(defun wlinearpoly (e ivar)
  (cond ((and (setq e (polyinx e ivar t))
	      (equal (deg-var e ivar) 1))
	 (subin-var 1 e ivar))))

;; Test to see if exp is of the form f(exp(x)), and if so, replace
;; exp(x) with 'z*.  That is, basically return f(z*).
(defun pin%ex (exp ivar)
  (pin%ex0 (cond ((notinvolve-var exp ivar '(%sinh %cosh %tanh))
		  exp)
		 (t
		  (let (($exponentialize t))
		    (setq exp ($expand exp)))))
           ivar))

(defun pin%ex0 (e ivar)
  ;; Does e really need to be special here?  Seems to be ok without
  ;; it; testsuite works.
  #+nil
  (declare (special e))
  (cond ((not (among ivar e))
	 e)
	((atom e)
	 (throw 'pin%ex nil))
	((and (mexptp e)
	      (eq (cadr e)  '$%e))
	 (cond ((eq (caddr e) ivar)
		'z*)
	       ((let ((linterm (wlinearpoly (caddr e) ivar)))
		  (and linterm
		       (m* (subin-var 0 e ivar) (m^t 'z* linterm)))))
	       (t
		(throw 'pin%ex nil))))
	((mtimesp e)
	 (m*l (mapcar #'(lambda (ee)
                          (pin%ex0 ee ivar))
                      (cdr e))))
	((mplusp e)
	 (m+l (mapcar #'(lambda (ee)
                          (pin%ex0 ee ivar))
                      (cdr e))))
	(t
	 (throw 'pin%ex nil))))

(defun findsub (p ivar)
  (let (nd)
    (cond ((findp p ivar) nil)
	  ((setq nd (bx**n p ivar))
	   (m^t ivar (car nd)))
	  ((setq p (bx**n+a p ivar))
	   (m* (caddr p) (m^t ivar (cadr p)))))))

;; I think this is looking at f(exp(x)) and tries to find some
;; rational function R and some number k such that f(exp(x)) =
;; R(exp(k*x)).
(defun funclogor%e (e ivar)
  (prog (ans arg nvar r)
     (cond ((or (ratp e ivar)
		(involve-var e ivar '(%sin %cos %tan))
		(not (setq arg (xor (and (setq arg (involve-var e ivar '(%log)))
					 (setq r '%log))
				    (%einvolve-var e ivar)))))
	    (return nil)))
     ag (setq nvar (cond ((eq r '%log) `((%log) ,arg))
			 (t (m^t '$%e arg))))
     (setq ans (maxima-substitute (m^t 'yx -1) (m^t nvar -1) (maxima-substitute 'yx nvar e)))
     (cond ((not (among ivar ans))  (return (list (subst ivar 'yx ans) nvar)))
	   ((and (null r)
		 (setq arg (findsub arg ivar)))
	    (go ag)))))

;; Integration by parts.
;;
;; integrate(u(x)*diff(v(x),x),x,a,b)
;;              |b
;;   = u(x)*v(x)| - integrate(v(x)*diff(u(x),x))
;;              |a
;;
(defun dintbypart (u v a b ivar)
;;;SINCE ONLY CALLED FROM DINTLOG TO get RID OF LOGS - IF LOG REMAINS, QUIT
  (let ((ad (antideriv v ivar)))
    (cond ((or (null ad)
	       (involve-var ad ivar '(%log)))
	   nil)
	  (t (let ((p1 (m* u ad))
		   (p2 (m* ad (sdiff u ivar))))
	       (let ((p1-part1 (get-limit p1 ivar b '$minus))
		     (p1-part2 (get-limit p1 ivar a '$plus)))
		 (cond ((or (null p1-part1)
			    (null p1-part2))
			nil)
		       (t (let ((p2 (defint p2 ivar a b)))
			    (cond (p2 (add* p1-part1
					    (m- p1-part2)
					    (m- p2)))
				  (t nil)))))))))))

;; integrate(f(exp(k*x)),x,a,b), where f(z) is rational.
;;
;; See Wang p. 96-97.
;;
;; If the limits are minf to inf, we use the substitution y=exp(k*x)
;; to get integrate(f(y)/y,y,0,inf)/k.  If the limits are 0 to inf,
;; use the substitution s+1=exp(k*x) to get
;; integrate(f(s+1)/(s+1),s,0,inf).
(defun dintexp (exp ivar ll ul &aux ans)
  (let ((*dintexp-recur* t))		;recursion stopper
    (cond ((and (sinintp exp ivar)     ;To be moved higher in the code.
		(setq ans (antideriv exp ivar))
		(setq ans (intsubs ans ll ul ivar)))
	   ;; If we can integrate it directly, do so and take the
	   ;; appropriate limits.
	   )
	  ((setq ans (funclogor%e exp ivar))
	   ;; ans is the list (f(x) exp(k*x)).
	   (cond ((and (equal ll 0.)
		       (eq ul '$inf))
		  ;; Use the substitution s + 1 = exp(k*x).  The
		  ;; integral becomes integrate(f(s+1)/(s+1),s,0,inf)
		  (setq ans (m+t -1 (cadr ans))))
		 (t
		  ;; Use the substitution y=exp(k*x) because the
		  ;; limits are minf to inf.
		  (setq ans (cadr ans))))
	   ;; Apply the substitution and integrate it.
	   (intcv ans nil ivar ll ul)))))

;; integrate(log(g(x))*f(x),x,0,inf)
(defun dintlog (exp arg ivar ll ul)
  (let ((*dintlog-recur* (1+ *dintlog-recur*))) ;recursion stopper
    (prog (ans d)
       (cond ((and (eq ul '$inf)
		   (equal ll 0.)
		   (eq arg ivar)
		   (equal 1 (sratsimp (m// exp (m* (m- (subin-var (m^t ivar -1)
							          exp
                                                                  ivar))
						    (m^t ivar -2))))))
	      ;; Make the substitution y=1/x.  If the integrand has
	      ;; exactly the same form, the answer has to be 0.
	      (return 0.))
             ((and (setq ans (let (($gamma_expand t)) (logx1 exp ll ul ivar)))
		   (free ans '%limit))
	      (return ans))
	     ((setq ans (antideriv exp ivar))
	      ;; It's easy if we have the antiderivative.
	      ;; but intsubs sometimes gives results containing %limit
	      (return (intsubs ans ll ul ivar))))
       ;; Ok, the easy cases didn't work.  We now try integration by
       ;; parts.  Set ANS to f(x).
       (setq ans (m// exp `((%log) ,arg)))
       (cond ((involve-var ans ivar '(%log))
	      ;; Bad. f(x) contains a log term, so we give up.
	      (return nil))
	     ((and (eq arg ivar)
		   (equal 0. (no-err-sub-var 0. ans ivar))
		   (setq d (defint (m* ans (m^t ivar '*z*))
				 ivar ll ul)))
	      ;; The arg of the log function is the same as the
	      ;; integration variable.  We can do something a little
	      ;; simpler than integration by parts.  We have something
	      ;; like f(x)*log(x).  Consider f(x)*x^z.  If we
	      ;; differentiate this wrt to z, the integrand becomes
	      ;; f(x)*log(x)*x^z.  When we evaluate this at z = 0, we
	      ;; get the desired integrand.
	      ;;
	      ;; So we need f(0) to be 0 at 0.  If we can integrate
	      ;; f(x)*x^z, then we differentiate the result and
	      ;; evaluate it at z = 0.
	      (return (derivat '*z* 1. d 0.)))
	     ((setq ans (dintbypart `((%log) ,arg) ans ll ul ivar))
	      ;; Try integration by parts.
	      (return ans))))))

;; Compute diff(e,ivar,n) at the point pt.
(defun derivat (ivar n e pt)
  (subin-var pt (apply '$diff (list e ivar n)) ivar))

;;; GGR and friends

;; MAYBPC returns (COEF EXPO CONST)
;;
;; This basically picks off b*x^n+a and returns the list
;; (b n a).
(defun maybpc (e ivar nd-var)
  (let (zd zn)
    (cond (*mtoinf* (throw 'ggrm (linpower0 e ivar)))
	  ((and (not *mtoinf*)
	        (null (setq e (bx**n+a e ivar)))) ;bx**n+a --> (a n b) or nil.
	   nil)                                   ;with ivar being x.
	  ;; At this point, e is of the form (a n b)
	  ((and (among '$%i (caddr e))
	        (zerop1 ($realpart (caddr e)))
	        (setq zn ($imagpart (caddr e)))
	        (eq ($asksign (cadr e)) '$pos))
	   ;; If we're here, b is complex, and n > 0.  zn = imagpart(b).
	   ;;
	   ;; Set ivar to the same sign as zn.
	   (cond ((eq ($asksign zn) '$neg)
		  (setq ivar -1)
		  (setq zn (m- zn)))
	         (t (setq ivar 1)))
	   ;; zd = exp(ivar*%i*%pi*(1+nd)/(2*n). (ZD is special!)
	   (setq zd (m^t '$%e (m// (mul* ivar '$%i '$%pi (m+t 1 nd-var))
				  (m*t 2 (cadr e)))))
	   ;; Return zn, n, a, zd.
	   (values `(,(caddr e) ,(cadr e) ,(car e)) zd))
	  ((and (or (eq (setq ivar ($asksign ($realpart (caddr e)))) '$neg)
		    (equal ivar '$zero))
	        (equal ($imagpart (cadr e)) 0)
	        (ratgreaterp (cadr e) 0.))
	   ;; We're here if realpart(b) <= 0, and n >= 0.  Then return -b, n, a.
	   `(,(caddr e) ,(cadr e) ,(car e))))))

;; Integrate x^m*exp(b*x^n+a), with realpart(m) > -1.
;;
;; See Wang, pp. 84-85.
;;
;; I believe the formula Wang gives is incorrect.  The derivation is
;; correct except for the last step.
;;
;; Let J = integrate(x^m*exp(%i*k*x^n),x,0,inf), with real k.
;;
;; Consider the case for k < 0.  Take a sector of a circle bounded by
;; the real line and the angle -%pi/(2*n), and by the radii, r and R.
;; Since there are no poles inside this contour, the integral
;;
;; integrate(z^m*exp(%i*k*z^n),z) = 0
;;
;; Then J = exp(-%pi*%i*(m+1)/(2*n))*integrate(R^m*exp(k*R^n),R,0,inf)
;;
;; because the integral along the boundary is zero except for the part
;; on the real axis.  (Proof?)
;;
;; Wang seems to say this last integral is gamma(s/n/(-k)^s) where s =
;; (m+1)/n.  But that seems wrong.  If we use the substitution R =
;; (y/(-k))^(1/n), we end up with the result:
;;
;;   integrate(y^((m+1)/n-1)*exp(-y),y,0,inf)/(n*k^((m+1)/n).
;;
;; or gamma((m+1)/n)/k^((m+1)/n)/n.
;;
;; Note that this also handles the case of
;;
;;   integrate(x^m*exp(-k*x^n),x,0,inf);
;;
;; where k is positive real number.  A simple change of variables,
;; y=k*x^n, gives
;;
;;   integrate(y^((m+1)/n-1)*exp(-y),y,0,inf)/(n*k^((m+1)/n))
;;
;; which is the same form above.
(defun ggr (e ind ivar)
  (prog (c zd nn* dn* nd-var dosimp $%emode)
     (setq nd-var 0.)
     (cond (ind (setq e ($expand e))
		(cond ((and (mplusp e)
			    (let ((*nodiverg* t))
			      (setq e (catch 'divergent
					(andmapcar
					 #'(lambda (j)
					     (ggr j nil ivar))
					 (cdr e))))))
		       (cond ((eq e 'divergent) nil)
			     (t (return (sratsimp (cons '(mplus) e)))))))))
     (setq e (rmconst1 e ivar))
     (setq c (car e))
     (setq e (cdr e))
     (cond ((multiple-value-setq (e zd)
              (ggr1 e ivar nd-var))
	    ;; e = (m b n a).  That is, the integral is of the form
	    ;; x^m*exp(b*x^n+a).  I think we want to compute
	    ;; gamma((m+1)/n)/b^((m+1)/n)/n.
	    ;;
	    ;; FIXME: If n > m + 1, the integral converges.  We need
	    ;; to check for this.
	    (destructuring-bind (m b n a)
		e
	      (when (and (not (zerop1 ($realpart b)))
			 (not (zerop1 ($imagpart b))))
		;; The derivation only holds if b is purely real or
		;; purely imaginary.  Give up if it's not.
		(return nil))
	      ;; Check for convergence.  If b is complex, we need n -
	      ;; m > 1.  If b is real, we need b < 0.
	      (when (and (zerop1 ($imagpart b))
			 (not (eq ($asksign b) '$neg)))
		(diverg))
	      (when (and (not (zerop1 ($imagpart b)))
			 (not (eq ($asksign (sub n (add m 1))) '$pos)))
		(diverg))

	      (setq e (gamma1 m (cond ((zerop1 ($imagpart b))
				       ;; If we're here, b must be negative.
				       (neg b))
				      (t
				       ;; Complex b.  Take the imaginary part
				       `((mabs) ,($imagpart b))))
			      n a))
	      (when zd
		;; FIXME: Why do we set %emode here?  Shouldn't we just
		;; bind it?  And why do we want it bound to T anyway?
		;; Shouldn't the user control that?  The same goes for
		;; dosimp.
		;;(setq $%emode t)
		(setq dosimp t)
		(setq e (m* zd e))))))
     (cond (e (return (m* c e))))))


;; Match x^m*exp(b*x^n+a).  If it does, return (list m b n a).
(defun ggr1 (e ivar nd-var)
  (let (zd)
    (cond ((atom e) nil)
	  ((and (mexptp e)
	        (eq (cadr e) '$%e))
	   ;; We're looking at something like exp(f(ivar)).  See if it's
	   ;; of the form b*x^n+a, and return (list 0 b n a).  (The 0 is
	   ;; so we can graft something onto it if needed.)
	   (cond ((multiple-value-setq (e zd)
                    (maybpc (caddr e) ivar nd-var))
		  (values (cons 0. e) zd))))
	  ((and (mtimesp e)
	        ;; E should be the product of exactly 2 terms
	        (null (cdddr e))
	        ;; Check to see if one of the terms is of the form
	        ;; ivar^p.  If so, make sure the realpart of p > -1.  If
	        ;; so, check the other term has the right form via
	        ;; another call to ggr1.
	        (or (and (setq dn* (xtorterm (cadr e) ivar))
		         (ratgreaterp (setq nd-var ($realpart dn*))
				      -1.)
		         (multiple-value-setq (nn* zd)
                           (ggr1 (caddr e) ivar nd-var)))
		    (and (setq dn* (xtorterm (caddr e) ivar))
		         (ratgreaterp (setq nd-var ($realpart dn*))
				      -1.)
		         (multiple-value-setq (nn* zd)
                           (ggr1 (cadr e) ivar nd-var)))))
	   ;; Both terms have the right form and nn* contains the ivar of
	   ;; the exponential term.  Put dn* as the car of nn*.  The
	   ;; result is something like (m b n a) when we have the
	   ;; expression x^m*exp(b*x^n+a).
	   (values (rplaca nn* dn*) zd)))))


;; Match b*x^n+a.  If a match is found, return the list (a n b).
;; Otherwise, return NIL
(defun bx**n+a (e ivar)
  (cond ((eq e ivar)
	 (list 0 1 1))
	((or (atom e)
	     (mnump e)) ())
	(t (let ((a (no-err-sub-var 0. e ivar)))
	     (cond ((null a)  ())
		   (t (setq e (m+ e (m*t -1 a)))
		      (cond ((setq e (bx**n e ivar))
			     (cons a e))
			    (t ()))))))))

;; Match b*x^n.  Return the list (n b) if found or NIL if not.
(defun bx**n (e ivar)
  (let ((n ()))
    (and (setq n (xexponget e ivar))
	 (not (among ivar
		     (setq e (let (($maxposex 1)
				   ($maxnegex 1))
			       ($expand (m// e (m^t ivar n)))))))
	 (list n e))))

;; nn* should be the value of var.  This is only called by bx**n with
;; the second arg of var.
(defun xexponget (e nn*)
  (cond ((atom e) (cond ((eq e nn*) 1.)))
	((mnump e) nil)
	((and (mexptp e)
	      (eq (cadr e) nn*)
	      (not (among nn* (caddr e))))
	 (caddr e))
	(t (some #'(lambda (j) (xexponget j nn*)) (cdr e)))))


;;; given (b*x^n+a)^m returns (m a n b)
(defun bxm (e ind ivar)
  (let (m r)
    (cond ((or (atom e)
	       (mnump e)
	       (involve-var e ivar '(%log %sin %cos %tan))
	       (%einvolve-var e ivar))
           nil)
	  ((mtimesp e)  nil)
	  ((mexptp e)  (cond ((among ivar (caddr e))  nil)
			     ((setq r (bx**n+a (cadr e) ivar))
			      (cons (caddr e) r))))
	  ((setq r (bx**n+a e ivar))  (cons 1. r))
	  ((not (null ind))
;;;Catches Unfactored forms.
           (multiple-value-bind (m r)
               (numden-var (m// (sdiff e ivar) e)
                           ivar)
	     (cond
	       ((and (setq r (bx**n+a (sratsimp r) ivar))
		     (not (among ivar (setq m (m// m (m* (cadr r) (caddr r)
						         (m^t ivar (m+t -1 (cadr r))))))))
		     (setq e (m// (subin-var 0. e ivar) (m^t (car r) m))))
	        (cond ((equal e 1.)
		       (cons m r))
		      (t (setq e (m^ e (m// 1. m)))
		         (list m (m* e (car r)) (cadr r)
			       (m* e (caddr r)))))))))
	  (t ()))))

;;;Is E = VAR raised to some power? If so return power or 0.
(defun findp (e ivar)
  (cond ((not (among ivar e)) 0.)
	(t (xtorterm e ivar))))

(defun xtorterm (e ivar)
;;;Is E = VAR1 raised to some power? If so return power.
  (cond ((alike1 e ivar) 1.)
	((atom e) nil)
	((and (mexptp e)
	      (alike1 (cadr e) ivar)
	      (not (among ivar (caddr e))))
	 (caddr e))))

(defun tbf (l)
  (m^ (m* (m^ (caddr l) '((rat) 1 2))
	  (m+ (cadr l) (m^ (m* (car l) (caddr l))
			   '((rat) 1 2))))
      -1))

(defun radbyterm (d l ivar)
  (do ((l l (cdr l))
       (ans ()))
      ((null l)
       (m+l ans))
    (destructuring-let (((const . integrand) (rmconst1 (car l) ivar)))
      (setq ans (cons (m* const (dintrad0 integrand d ivar))
		      ans)))))

(defun sqdtc (e ind ivar)
  (prog (a b c varlist)
     (setq varlist (list ivar))
     (newvar e)
     (setq e (cdadr (ratrep* e)))
     (setq c (pdis (ptterm e 0)))
     (setq b (m*t (m//t 1 2) (pdis (ptterm e 1))))
     (setq a (pdis (ptterm e 2)))
     (cond ((and (eq ($asksign (m+ b (m^ (m* a c)
					 '((rat) 1 2))))
		     '$pos)
		 (or (and ind
			  (not (eq ($asksign a) '$neg))
			  (eq ($asksign c) '$pos))
		     (and (eq ($asksign a) '$pos)
			  (not (eq ($asksign c) '$neg)))))
	    (return (list a b c))))))

(defun difap1 (e pwr ivar m pt)
  (m// (mul* (cond ((eq (ask-integer m '$even) '$yes)
		    1)
		   (t -1))
	     `((%gamma) ,pwr)
	     (derivat ivar m e pt))
       `((%gamma) ,(m+ pwr m))))

;; Note:  This doesn't seem be called from anywhere.
(defun sqrtinvolve (e ivar)
  (cond ((atom e) nil)
	((mnump e) nil)
	((and (mexptp e)
	      (and (mnump (caddr e))
		   (not (numberp (caddr e)))
		   (equal (caddr (caddr e)) 2.))
	      (among ivar (cadr e)))
	 (cadr e))
	(t (some #'(lambda (a)
                     (sqrtinvolve a ivar))
                 (cdr e)))))

(defun bydif (r s d ivar)
  (let ((b 1)  p)
    (setq d (m+ (m*t '*z* ivar) d))
    (cond ((or (zerop1 (setq p (m+ s (m*t -1 r))))
	       (and (zerop1 (m+ 1 p))
		    (setq b ivar)))
	   (difap1 (dintrad0 b (m^ d '((rat) 3 2)) ivar)
		   '((rat) 3 2) '*z* r 0))
	  ((eq ($asksign p) '$pos)
	   (difap1 (difap1 (dintrad0 1 (m^ (m+t 'z** d)
					   '((rat) 3 2))
                                     ivar)
			   '((rat) 3 2) '*z* r 0)
		   '((rat) 3 2) 'z** p 0)))))

(defun dintrad0 (n d ivar)
  (let (l r s)
    (cond ((and (mexptp d)
		(equal (deg-var (cadr d) ivar) 2.))
	   (cond ((alike1 (caddr d) '((rat) 3. 2.))
		  (cond ((and (equal n 1.)
			      (setq l (sqdtc (cadr d) t ivar)))
			 (tbf l))
			((and (eq n ivar)
			      (setq l (sqdtc (cadr d) nil ivar)))
			 (tbf (reverse l)))))
		 ((and (setq r (findp n ivar))
		       (or (eq ($asksign (m+ -1. (m-  r) (m*t 2.
							      (caddr d))))
			       '$pos)
			   (diverg))
		       (setq s (m+ '((rat) -3. 2.) (caddr d)))
		       (eq ($asksign s) '$pos)
		       (eq (ask-integer s '$integer) '$yes))
		  (bydif r s (cadr d) ivar))
		 ((polyinx n ivar nil)
		  (radbyterm d (cdr n) ivar)))))))


;;;Looks at the IMAGINARY part of a log and puts it in the interval 0 2*%pi.
(defun log-imag-0-2%pi (x)
  (let ((plog (simplify ($rectform `((%plog) ,x)))))
    ;; We take the $rectform above to make sure that the log is
    ;; expanded out for the situations where simplifying plog itself
    ;; doesn't do it.  This should probably be considered a bug in the
    ;; plog simplifier and should be fixed there.
    (cond ((not (free plog '%plog))
	   (subst '%log '%plog plog))
	  (t
	   (destructuring-let (((real . imag) (trisplit plog)))
	     (cond ((eq ($asksign imag) '$neg)
		    (setq imag (m+ imag %pi2)))
		   ((eq ($asksign (m- imag %pi2)) '$pos)
		    (setq imag (m- imag %pi2)))
		   (t t))
	     (m+ real (m* '$%i imag)))))))


;;; Temporary fix for a lacking in taylor, which loses with %i in denom.
;;; Besides doesn't seem like a bad thing to do in general.
(defun %i-out-of-denom (exp)
  (let ((denom ($denom exp)))
    (cond ((among '$%i denom)
	   ;; Multiply the denominator by it's conjugate to get rid of
	   ;; %i.
	   (let* ((den-conj (maxima-substitute (m- '$%i) '$%i denom))
		  (num ($num exp))
		  (new-denom (sratsimp (m* denom den-conj)))
		  (new-exp (sratsimp (m// (m* num den-conj) new-denom))))
	     ;; If the new denominator still contains %i, just give up.
	     (if (among '$%i ($denom new-exp))
		 exp
		 new-exp)))
	  (t exp))))

;;; LL and UL must be real otherwise this routine return $UNKNOWN.
;;; Returns $no $unknown or a list of poles in the interval (ll ul)
;;; for exp w.r.t. ivar.
;;; Form of list ((pole . multiplicity) (pole1 . multiplicity) ....)
(defun poles-in-interval (exp ivar ll ul)
  (let* ((denom (cond ((mplusp exp)
		       ($denom (sratsimp exp)))
		      ((and (mexptp exp)
			    (free (caddr exp) ivar)
			    (eq ($asksign (caddr exp)) '$neg))
		       (m^ (cadr exp) (m- (caddr exp))))
		      (t ($denom exp))))
	 (roots (real-roots denom ivar))
	 (ll-pole (limit-pole exp ivar ll '$plus))
	 (ul-pole (limit-pole exp ivar ul '$minus)))
    (cond ((or (eq roots '$failure)
	       (null ll-pole)
	       (null ul-pole))   '$unknown)
	  ((and (or (eq roots '$no)
		    (member ($csign denom) '($pos $neg $pn)))
		    ;; this clause handles cases where we can't find the exact roots,
		    ;; but we know that they occur outside the interval of integration.
		    ;;  example: integrate ((1+exp(t))/sqrt(t+exp(t)), t, 0, 1);
		(eq ll-pole '$no)
		(eq ul-pole '$no))  '$no)
	  (t (cond ((equal roots '$no)
		    (setq roots ())))
	     (do ((dummy roots (cdr dummy))
		  (pole-list (cond ((not (eq ll-pole '$no))
				    `((,ll . 1)))
				   (t nil))))
		 ((null dummy)
		  (cond ((not (eq ul-pole '$no))
			 (sort-poles (push `(,ul . 1) pole-list)))
			((not (null pole-list))
			 (sort-poles pole-list))
			(t '$no)))
	       (let* ((soltn (caar dummy))
		      ;; (multiplicity (cdar dummy)) (not used? -- cwh)
		      (root-in-ll-ul (in-interval soltn ll ul)))
		 (cond ((eq root-in-ll-ul '$no) '$no)
		       ((eq root-in-ll-ul '$yes)
			(let ((lim-ans (is-a-pole exp soltn ivar)))
			  (cond ((null lim-ans)
				 (return '$unknown))
				((equal lim-ans 0)
				 '$no)
				(t (push (car dummy)
					 pole-list))))))))))))


;;;Returns $YES if there is no pole and $NO if there is one.
(defun limit-pole (exp ivar limit direction)
  (let ((ans (cond ((member limit '($minf $inf) :test #'eq)
		    (cond ((eq (special-convergent-formp exp limit ivar) '$yes)
			   '$no)
			  (t (get-limit (m* exp ivar) ivar limit direction))))
		   (t '$no))))
    (cond ((eq ans '$no)   '$no)
	  ((null ans)   nil)
	  ((eq ans '$und) '$no)
	  ((equal ans 0.)   '$no)
	  (t '$yes))))

;;;Takes care of forms that the ratio test fails on.
(defun special-convergent-formp (exp limit ivar)
  (cond ((not (oscip-var exp ivar))  '$no)
	((or (eq (sc-converg-form exp limit ivar) '$yes)
	     (eq (exp-converg-form exp limit ivar) '$yes))
	 '$yes)
	(t  '$no)))

(defun exp-converg-form (exp limit ivar)
  (let (exparg)
    (setq exparg (%einvolve-var exp ivar))
    (cond ((or (null exparg)
	       (freeof '$%i exparg))
	   '$no)
	  (t (cond
	       ((and (freeof '$%i
			     (%einvolve-var
			      (setq exp
				    (sratsimp (m// exp (m^t '$%e exparg))))
                              ivar))
		     (equal (get-limit exp ivar limit)  0))
		'$yes)
	       (t '$no))))))

(defun sc-converg-form (exp limit ivar)
  (prog (scarg trigpow)
     (setq exp ($expand exp))
     (setq scarg (involve-var (sin-sq-cos-sq-sub exp) ivar '(%sin %cos)))
     (cond ((null scarg) (return '$no))
	   ((and (polyinx scarg ivar ())
		 (eq ($asksign (m- ($hipow scarg ivar) 1)) '$pos))
            (return '$yes))
	   ((not (freeof ivar (sdiff scarg ivar)))
	    (return '$no))
	   ((and (setq trigpow ($hipow exp `((%sin) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%sin) ,scarg)) ivar limit)
			0))
	    (return '$yes))
	   ((and (setq trigpow ($hipow exp `((%cos) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%cos) ,scarg)) ivar limit)
			0))
	    (return '$yes))
	   (t (return '$no)))))

(defun is-a-pole (exp soltn ivar)
  (get-limit ($radcan
	      (m* (maxima-substitute (m+ 'epsilon soltn) ivar exp)
		  'epsilon))
	     'epsilon 0 '$plus))

(defun in-interval (place ll ul)
  ;; real values for ll and ul; place can be imaginary.
  (let ((order (ask-greateq ul ll)))
    (cond ((eq order '$yes))
	  ((eq order '$no) (let ((temp ul)) (setq ul ll ll temp)))
	  (t (merror (intl:gettext "defint: failed to order limits of integration:~%~M")
		     (list '(mlist simp) ll ul)))))
  (if (not (equal ($imagpart place) 0))
      '$no
      (let ((lesseq-ul (ask-greateq ul place))
	    (greateq-ll (ask-greateq place ll)))
	(if (and (eq lesseq-ul '$yes) (eq greateq-ll '$yes)) '$yes '$no))))

;; returns true or nil
(defun strictly-in-interval (place ll ul)
  ;; real values for ll and ul; place can be imaginary.
  (and (equal ($imagpart place) 0)
       (or (eq ul '$inf) 
	   (eq ($asksign (m+ ul (m- place))) '$pos))
       (or (eq ll '$minf) 
	   (eq ($asksign (m+ place (m- ll))) '$pos))))

(defun real-roots (exp ivar)
  (let (($solvetrigwarn (cond (*defintdebug* t) ;Rest of the code for
			      (t ())))	;TRIGS in denom needed.
	($solveradcan (cond ((or (among '$%i exp)
				 (among '$%e exp)) t)
			    (t nil)))
	*roots *failures)		;special vars for solve.
    (cond ((not (among ivar exp))   '$no)
	  (t (solve exp ivar 1)
	     ;; If *failures is set, we may have missed some roots.
	     ;; We still return the roots that we have found.
	     (do ((dummy *roots (cddr dummy))
		  (rootlist))
		 ((null dummy)
		  (cond ((not (null rootlist))
			 rootlist)
			(t '$no)))
	       (cond ((equal ($imagpart (caddar dummy)) 0)
		      (setq rootlist
			    (cons (cons
				   ($rectform (caddar dummy))
				   (cadr dummy))
				  rootlist)))))))))

(defun ask-greateq (x y)
;;; Is x > y. X or Y can be $MINF or $INF, zeroA or zeroB.
  (let ((x (cond ((among 'zeroa x)
		  (subst 0 'zeroa x))
		 ((among 'zerob x)
		  (subst 0 'zerob x))
		 ((among 'epsilon x)
		  (subst 0 'epsilon x))
		 ((or (among '$inf x)
		      (among '$minf x))
		  ($limit x))
		 (t x)))
	(y (cond ((among 'zeroa y)
		  (subst 0 'zeroa y))
		 ((among 'zerob y)
		  (subst 0 'zerob y))
		 ((among 'epsilon y)
		  (subst 0 'epsilon y))
		 ((or (among '$inf y)
		      (among '$minf y))
		  ($limit y))
		 (t y))))
    (cond ((eq x '$inf)
	   '$yes)
	  ((eq x '$minf)
	   '$no)
	  ((eq y '$inf)
	   '$no)
	  ((eq y '$minf)
	   '$yes)
	  (t (let ((ans ($asksign (m+ x (m- y)))))
	       (cond ((member ans '($zero $pos) :test #'eq)
		      '$yes)
		     ((eq ans '$neg)
		      '$no)
		     (t '$unknown)))))))

(defun sort-poles (pole-list)
  (sort pole-list #'(lambda (x y)
		      (cond ((eq (ask-greateq (car x) (car y))
				 '$yes)
			     nil)
			    (t t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Integrate Definite Integrals involving log and exp functions. The algorithm
;;;  are taken from the paper "Evaluation of CLasses of Definite Integrals ..."
;;;  by K.O.Geddes et. al.
;;;
;;;  1. CASE: Integrals generated by the Gamma function.
;;;
;;;    inf
;;;   /
;;;   [     w    m            s        - m - 1
;;;   I    t  log (t) expt(- t ) dt = s        signum(s)
;;;   ]
;;;   /
;;;    0
;;;                                                                 !
;;;                                                    m            !
;;;                                                   d             !
;;;                                                  (--- (gamma(z))!         )
;;;                                                     m           !
;;;                                                   dz            !    w + 1
;;;                                                                 !z = -----
;;;                                                                        s
;;;
;;;  The integral converges for: 
;;;  s # 0, m = 0, 1, 2, ... and realpart((w+1)/s) > 0.
;;;  
;;;  2. CASE: Integrals generated by the Incomplete Gamma function.
;;;
;;;    inf                                                         !
;;;   /                                m                           !
;;;   [     w    m           s        d                         s  !
;;;   I    t  log (t) exp(- t ) dt = (--- (gamma_incomplete(a, x ))!         )
;;;   ]                                 m                          !
;;;   /                               da                           !    w + 1
;;;    x                                                           !z = -----
;;;                                                                       s
;;;                                                           - m - 1
;;;                                                          s        signum(s)
;;;
;;;  The integral converges for:
;;;  s # 0, m = 0, 1, 2, ... and realpart((w+1)/s) > 0.
;;;  The shown solution is valid for s>0. For s<0 gamma_incomplete has to be 
;;;  replaced by gamma(a) - gamma_incomplete(a,x^s).
;;;
;;;  3. CASE: Integrals generated by the beta function.
;;;
;;;    1
;;;   /
;;;   [     m               s  r    n
;;;   I  log (1 - t) (1 - t)  t  log (t) dt = 
;;;   ]
;;;   /
;;;    0
;;;                                                                  !
;;;                                                       !          !
;;;                                   n    m              !          !
;;;                                  d    d               !          !
;;;                                  --- (--- (beta(z, w))!         )!
;;;                                    n    m             !          !
;;;                                  dz   dw              !          !
;;;                                                       !w = s + 1 !
;;;                                                                  !z = r + 1
;;;
;;;  The integral converges for:
;;;  n, m = 0, 1, 2, ..., s > -1 and r > -1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-defint-log* nil)

;;; Recognize c*z^w*log(z)^m*exp(-t^s)

(defun m2-log-exp-1 (expr ivar)
  (when *debug-defint-log*
    (format t "~&M2-LOG-EXP-1 with ~A~%" expr))
  (m2 expr
    `((mtimes)
        (c freevar2 ,ivar)
        ((mexpt) (z varp2 ,ivar) (w freevar2 ,ivar))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) (z varp2 ,ivar) (s freevar02 ,ivar))))
        ((mexpt) ((%log) (z varp2 ,ivar)) (m freevar2 ,ivar)))))

;;; Recognize c*z^r*log(z)^n*(1-z)^s*log(1-z)^m

(defun m2-log-exp-2 (expr ivar)
  (when *debug-defint-log*
    (format t "~&M2-LOG-EXP-2 with ~A~%" expr))
  (m2 expr
    `((mtimes)
        (c freevar2 ,ivar)
        ((mexpt) (z varp2 ,ivar) (r freevar2 ,ivar))
        ((mexpt) ((%log) (z varp2 ,ivar)) (n freevar2 ,ivar))
        ((mexpt) ((mplus) 1 ((mtimes) -1 (z varp2 ,ivar))) (s freevar2 ,ivar))
        ((mexpt) ((%log) ((mplus) 1 ((mtimes)-1 (z varp2 ,ivar)))) (m freevar2 ,ivar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defint-log-exp (expr ivar ll ul)
  (let ((x nil)
        (result nil)
        (var1 (gensym)))
    
    ;; var1 is used as a parameter for differentiation. Add var1>0 to the 
    ;; database, to get the desired simplification of the differentiation of 
    ;; the gamma_incomplete function.
    (setq *global-defint-assumptions*
          (cons (assume `((mgreaterp) ,var1 0))
                *global-defint-assumptions*))

    (cond
      ((and (eq ul '$inf)
            (setq x (m2-log-exp-1 expr ivar)))
       ;; The integrand matches the cases 1 and 2.
       (let ((c (cdras 'c x))
             (w (cdras 'w x))
             (m (cdras 'm x))
             (s (cdras 's x))
             ($gamma_expand nil)) ; No expansion of Gamma functions.

         (when *debug-defint-log*
           (format t "~&DEFINT-LOG-EXP-1:~%")
           (format t "~&   : c = ~A~%" c)
           (format t "~&   : w = ~A~%" w)
           (format t "~&   : m = ~A~%" m)
           (format t "~&   : s = ~A~%" s))

         (cond ((and (zerop1 ll)
                     (integerp m)
                     (>= m 0)
                     (not (eq ($sign s) '$zero))
                     (eq ($sign (div (add w 1) s)) '$pos))
                ;; Case 1: Generated by the Gamma function.
                (setq result 
                     (mul c
                          (simplify (list '(%signum) s))
                          (power s (mul -1 (add m 1)))
                          ($at ($diff (list '(%gamma) var1) var1 m)
                               (list '(mequal)
                                     var1
                                     (div (add w 1) s))))))
             ((and (member ($sign ll) '($pos $pz))
                   (integerp m)
                   (or (= m 0) (= m 1))	; Exclude m>1, because Maxima can not
                                        ; derivate the involved hypergeometric
                                        ; functions.
                   (or (and (eq ($sign s) '$neg)
                            (eq ($sign (div (add 1 w) s)) '$pos))
                       (and (eq ($sign s) '$pos)
                            (eq ($sign (div (add 1 w) s)) '$pos))))
              ;; Case 2: Generated by the Incomplete Gamma function.
	      (let ((f (if (eq ($sign s) '$pos)
			   (list '(%gamma_incomplete) var1 (power ll s))
			   (sub (list '(%gamma) var1)
				(list '(%gamma_incomplete) var1 (power ll s))))))
		(setq result 
		      (mul c
			   (simplify (list '(%signum) s))
			   (power s (mul -1 (add m 1)))
			   ($at ($diff f var1 m)
				(list '(mequal) var1 (div (add 1 w) s)))))))
               (t 
                (setq result nil)))))
      ((and (zerop1 ll)
            (onep1 ul)
            (setq x (m2-log-exp-2 expr ivar)))
       ;; Case 3: Generated by the Beta function.
       (let ((c (cdras 'c x))
             (r (cdras 'r x))
             (n (cdras 'n x))
             (s (cdras 's x))
             (m (cdras 'm x))
             (var1 (gensym))
             (var2 (gensym)))

         (when *debug-defint-log*
           (format t "~&DEFINT-LOG-EXP-2:~%")
           (format t "~&   : c = ~A~%" c)
           (format t "~&   : r = ~A~%" r)
           (format t "~&   : n = ~A~%" n)
           (format t "~&   : s = ~A~%" s)
           (format t "~&   : m = ~A~%" m))

         (cond ((and (integerp m)
                     (>= m 0)
                     (integerp n)
                     (>= n 0)
                     (eq ($sign (add 1 r)) '$pos)
                     (eq ($sign (add 1 s)) '$pos))
                (setq result 
                      (mul c
                           ($at ($diff ($at ($diff (list '(%beta) var1 var2) 
                                                   var2 m)
                                            (list '(mequal) var2 (add 1 s)))
                                       var1 n)
                                (list '(mequal) var1 (add 1 r))))))
              (t 
               (setq result nil)))))
      (t 
       (setq result nil)))
    ;; Simplify result and set $gamma_expand to global value
    (let (($gamma_expand $gamma_expand)) (sratsimp result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
