;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
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
;; (http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-092.pdf)
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
;;ztoinf or mtoinf, respectivly.
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
;;	dintlog- i(log(g(x))*f(x),x,0,inf) = 0 (by symetry) or
;;		 tries an integration by parts.  (only routine to
;;		 try integration by parts) [wang, pp 93-95]
;;
;;	dintexp- i(f(exp(x)),x,a,b) = i(f(x+1),x,a',b')
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
;;	   ssp    - a*sc^n(r*x)/x^m  [wang, pp 83,84]
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

(declare-top(*lexpr $diff $limit $substitute $ezgcd $ratsimp context)
	    (*expr subfunmake $coeff $logcontract $radcan $makegamma
		   $constantp $subvarp maxima-substitute freeof ith
		   $oddp $hipow $multthru $xthru $num $denom 
		   stripdollar maxima-find sdiff partition
		   constant free mapatom

		   $ratdisrep ratdisrep $ratp ratp ratnumerator 
		   sratsimp ratdenominator $ratsubst ratnump ratcoef
		   pterm rdis pdis ratrep newvar pdivide pointergp
		      
		   $factor factor $sqfr oddelm zerop1

		   $asksign asksign $sign ask-integer assume forget
		      
		   $residue residue res res1 polelist partnum

		   solve solvex sinint
		      
		   $rectform $realpart $imagpart trisplit cabs
		      
		   among involve notinvolve  
		   numden* ratgreaterp
		   subin polyinx genfind xor fmt polyp numden andmapcar
		   abless1 even1 rddeg tansc radicalp deg simplerd
		   no-err-sub oscip %einvolve sin-sq-cos-sq-sub)
		      
;;;rsn* is in comdenom. does a ratsimp of numerator.
	    (special *def2* pcprntd mtoinf* rsn* semirat*
		     sn* sd* leadcoef checkfactors 
		     *nodiverg rd* exp1
		     ul1 ll1 *dflag bptu bptd plm* zn
		     #+nil zd
		     *updn ul ll exp pe* pl* rl* pl*1 rl*1
		     loopstop* var nn* nd* dn* p*
		     ind* factors rlm*
		     plogabs *zexptsimp? scflag
		     sin-cos-recur rad-poly-recur dintlog-recur
		     dintexp-recur defintdebug defint-assumptions
		     current-assumptions
		     global-defint-assumptions)
	 
	    (array* (notype *i* 1 *j* 1))
	    (genprefix def)
	    (muzzled t)
					;expvar
	    (special $intanalysis $abconvtest $noprincipal $nointegrate)
					;impvar
	    (special $solveradcan $solvetrigwarn *roots *failures 
		     $logabs $tlimswitch $maxposex $maxnegex
		     $trigsign $savefactors $radexpand $breakup $%emode
		     $float $exptsubst dosimp context rp-polylogp
		     %p%i half%pi %pi2 half%pi3 varlist genvar
		     $domain $m1pbranch errorsw errrjfflag raterr
		     limitp $algebraic
		     ;;LIMITP T Causes $ASKSIGN to do special things
		     ;;For DEFINT like eliminate epsilon look for prin-inf
		     ;;take realpart and imagpart.
		     integer-info
		     ;;If LIMITP is non-null ask-integer conses 
		     ;;its assumptions onto this list.
		     generate-atan2))
					;If this switch is () then RPART returns ATAN's
					;instead of ATAN2's

(declare-top(special infinities real-infinities infinitesimals))
(cond	 ;These are really defined in LIMIT but DEFINT uses them also.
  ((not (boundp 'infinities))
   (setq infinities '($inf $minf $infinity))
   (setq real-infinities '($inf $minf))
   (setq infinitesimals '($zeroa $zerob))))

(defmvar defintdebug () "If true Defint prints out debugging information")

(defmvar integerl nil
  "An integer-list for non-atoms found out to be `integer's")

(defmvar nonintegerl nil
  "A non-integer-list for non-atoms found out to be `noninteger's")

(defun $defint (exp var ll ul)
  (let ((global-defint-assumptions ())
	(integer-info ()) (integerl integerl) (nonintegerl nonintegerl))
    (with-new-context (context)
      (unwind-protect
	   (let ((defint-assumptions ())  (*def2* ())  (rad-poly-recur ())
		 (sin-cos-recur ())  (dintexp-recur ())  (dintlog-recur 0.)
		 (ans nil)  (orig-exp exp)  (orig-var var)
		 (orig-ll ll)  (orig-ul ul) 
		 (pcprntd nil)  (*nodiverg nil)  ($logabs t)  (limitp t)
		 (rp-polylogp ())
		 ($domain '$real) ($m1pbranch ())) ;Try this out.

	     (find-function '$limit)
	     (make-global-assumptions) ;sets global-defint-assumptions
	     (find-function '$residue)
	     (setq exp (ratdisrep exp))
	     (setq var (ratdisrep var))
	     (setq ll (ratdisrep ll))
	     (setq ul (ratdisrep ul))
	     (cond (($constantp var)
		    (merror "Variable of integration not a variable: ~M"
			    var))
		   (($subvarp var)  (setq var (stripdollar (caar var)))
		    (setq exp ($substitute var orig-var exp))))
	     (cond ((not (atom var))
		    (merror "Improper variable of integration: ~M" var))
		   ((or (among var ul)
			(among var ll)) 
		    (setq var (stripdollar var))
		    (setq exp ($substitute var orig-var exp))))
	     (cond ((not (equal ($ratsimp ($imagpart ll)) 0))
		    (merror "Defint: Lower limit of integration must be real."))
		   ((not (equal ($ratsimp ($imagpart ul)) 0))
		    (merror
		     "Defint: Upper limit of integration must be real.")))

	     (cond ((setq ans (defint exp var ll ul))
		    (setq ans (subst orig-var var ans))
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

(defun eezz (exp ll ul)
  (cond ((or (polyinx exp var nil)
	     (catch 'pin%ex (pin%ex exp)))
	 (setq exp (antideriv exp))
	 ;; If antideriv can't do it, returns nil
	 ;; use limit to evaluate every answer returned by antideriv.
	 (cond ((null exp) nil)
	       (t (intsubs exp ll ul))))))
;;;Hack the expression up for exponentials.

(defun sinintp (expr var)
  ;; Is this expr a candidate for SININT ?
  (let ((expr (factor expr))
	(numer nil)
	(denom nil))
    (setq numer ($num expr))
    (setq denom ($denom expr))
    (cond ((polyinx numer var nil)
	   (cond ((and (polyinx denom var nil)
		       (deg-lessp denom var 2))
		  t)))
	  ;;ERF type things go here.
	  ((let ((exponent (%einvolve numer)))
	     (and (polyinx exponent var nil)
		  (deg-lessp exponent var 2)))
	   (cond ((free denom var)
		  t))))))

(defun deg-lessp (expr var power)
  (cond  ((or (atom expr) 
	      (mnump expr)) t)
	 ((or (mtimesp expr) 
	      (mplusp expr))
	  (do ((ops (cdr expr) (cdr ops)))
	      ((null ops) t)
	    (cond ((not (deg-lessp (car ops) var power))
		   (return ())))))
	 ((mexptp expr)
	  (and (or (not (alike1 (cadr expr) var))
		   (and (numberp (caddr expr))
			(not (eq (asksign (m+ power (m- (caddr expr))))
				 '$negative))))
	       (deg-lessp (cadr expr) var power)))))

(defun antideriv (a)
  (let ((limitp ())
	(ans ())
	(generate-atan2 ()))
    (setq ans (sinint a var))
    (cond ((among '%integrate ans)  nil) 
	  (t (simplify ans)))))

;;;This routine tries to take a limit a couple of ways.
(defmfun get-limit nargs
  (let ((ans (apply 'limit-no-err (listify nargs)))
	(val ()) (var ()) (exp ()) (dir ()))
    (cond ((and ans (not (among '%limit ans)))  ans)
	  (t (cond ((and (or (equal nargs 3) (equal nargs 4))
			 (memq (setq val (arg 3)) '($inf $minf)))
		    (setq var (arg 2))
		    (setq exp (maxima-substitute (m^t var -1) var (arg 1)))
		    (cond ((eq val '$inf)  (setq dir '$plus))
			  (t (setq dir '$minus)))
					;(setq ans (apply 'limit-no-err `(,exp ,var 0 ,dir)))
		    (setq ans (limit-no-err exp var 0 dir))
		    (cond ((not (among '%limit ans))  ans)
			  (t ()))))))))

;;(defun limit-no-err nargs
;;  (let ((errorsw t)  (ans ()))
;;    (setq ans (catch 'errorsw (apply '$limit (listify nargs))))
;;    (cond ((not (eq ans t))  ans)
;;	  (t nil))))
(defun limit-no-err (&rest argvec)
  (declare (special errorsw))
  (let ((errorsw t) (ans nil))
    (setq ans (catch 'errorsw (apply '$limit argvec)))
    (if (eq ans t) nil ans)))

(defun intcv (nv ind flag)
  (let ((d (bx**n+a nv))
	(*roots ())  (*failures ())  ($breakup ()))
    (cond ((and (eq ul '$inf)
		(equal ll 0)
		(equal (cadr d) 1)) ())
	  (t (solve (m+t 'yx (m*t -1. nv)) var 1.)
	     (cond (*roots (setq d (subst var 'yx (caddar *roots)))
			   (cond (flag (intcv2 d ind nv))
				 (t (intcv1 d ind nv))))
		   (t ()))))))

(defun intcv1 (d ind nv) 
  (cond ((and (intcv2 d ind nv)
	      (not (alike1 ll1 ul1)))
	 (let ((*def2* t))
	   (defint exp1 var ll1 ul1)))))

(defun intcv2 (d ind nv)
  (intcv3 d ind nv)
  (and (cond ((and (zerop1 (m+ ll ul))
		   (evenfn nv var))
	      (setq exp1 (m* 2 exp1)
		    ll1 (limcp nv var 0 '$plus)))
	     (t (setq ll1 (limcp nv var ll '$plus))))
       (setq ul1 (limcp nv var ul '$minus))))

(defun limcp (a b c d) 
  (let ((ans ($limit a b c d)))
    (cond ((not (or (null ans)
		    (among '%limit ans)
		    (among '$ind ans)
		    (among '$und ans)))
	   ans))))

(defun intcv3 (d ind nv)
  (setq nn* ($ratsimp (sdiff d var)))
  (setq exp1 (subst 'yx nv exp))
  (setq exp1 (m* nn* (cond (ind exp)
			   (t (subst d var exp1)))))
  (setq exp1 (sratsimp (subst var 'yx exp1))))

(defun defint (exp var ll ul)
  (let ((old-assumptions defint-assumptions)  (current-assumptions ()))
    (unwind-protect
	 (prog ()
	    (setq current-assumptions (make-defint-assumptions 'noask))
	    (let ((exp (resimplify exp))            
		  (var (resimplify var))
		  ($exptsubst t)
		  (loopstop* 0)
		  ;; D (not used? -- cwh)
		  ans nn* dn* nd* $noprincipal)
	      (cond ((setq ans (defint-list exp var ll ul))
		     (return ans))
		    ((or (zerop1 exp)
			 (alike1 ul ll))
		     (return 0.))
		    ((not (among var exp))
		     (cond ((or (memq ul '($inf $minf))
				(memq ll '($inf $minf)))
			    (diverg))
			   (t (setq ans (m* exp (m+ ul (m- ll))))
			      (return ans)))))
	      (let* ((exp (rmconst1 exp))
		     (c (car exp))
		     (exp (%i-out-of-denom (cdr exp))))
		(cond ((and (not $nointegrate)
			    (not (atom exp))
			    (or (among 'mqapply exp)
				(not (memq (caar exp)
					   '(mexpt mplus mtimes %sin %cos
					     %tan %sinh %cosh %tanh
					     %log %asin %acos %atan
					     %cot %acot %sec 
					     %asec %csc %acsc 
					     %derivative)))))
		       (cond ((setq ans (antideriv exp))
			      (setq ans (intsubs ans ll ul))
			      (return (m* c ans)))
			     (t (return nil)))))
		(setq exp (tansc exp))
		(cond ((setq  ans (initial-analysis exp var ll ul))
		       (return (m* c ans))))
		(return nil))))
      (restore-defint-assumptions old-assumptions current-assumptions))))

(defun defint-list (exp var ll ul)
  (cond ((and (not (atom exp)) 
	      (memq (caar exp)
		    '(mequal mlist $matrix)))
	 (let ((ans (cons (car exp)
			  (mapcar
			   #'(lambda (sub-exp)
			       (defint sub-exp var ll ul))
			   (cdr exp)))))
	   (cond (ans (simplify ans))
		 (t nil))))
	(t nil)))

(defun initial-analysis (exp var ll ul)
  (let ((pole (cond ((not $intanalysis)
		     '$no)		;don't do any checking.
		    (t (poles-in-interval exp var ll ul)))))
    (cond ((eq pole '$no)
	   (cond ((and (oddfn exp var)
		       (or (and (eq ll '$minf)
				(eq ul '$inf))
			   (eq ($sign (m+ ll ul))
			       '$zero)))  0)
		 (t (parse-integrand exp var ll ul))))
	  ((eq pole '$unknown)  ())
	  (t (principal-value-integral exp var ll ul pole)))))

(defun parse-integrand (exp var ll ul)
  (let (ans)
    (cond ((setq ans (eezz exp ll ul))  ans)
	  ((and (ratp exp var)
		(setq ans (method-by-limits exp var ll ul)))  ans)
	  ((and (mplusp exp)
		(setq ans (intbyterm exp t)))  ans)
	  ((setq ans (method-by-limits exp var ll ul))  ans)
	  (t ()))))

(defun rmconst1 (e)
  (cond ((among var e) 
	 (partition e var 1))
	(t (cons e 1))))


(defun method-by-limits (exp var ll ul)
  (let ((old-assumptions defint-assumptions))
    (setq current-assumptions (make-defint-assumptions 'noask))
    ;;Should be a PROG inside of unwind-protect, but Multics has a compiler
    ;;bug wrt. and I want to test this code now.
    (unwind-protect
	 (cond ((and (and (eq ul '$inf)
			  (eq ll '$minf))
		     (mtoinf exp var)))
	       ((and (and (eq ul '$inf)
			  (equal ll 0.))
		     (ztoinf exp var)))
;;;This seems((and (and (eq ul '$inf)
;;;fairly losing	(setq exp (subin (m+ ll var) exp))
;;;			(setq ll 0.))
;;;		   (ztoinf exp var)))
	       ((and (equal ll 0.)
		     (freeof var ul)
		     (eq ($asksign ul) '$pos)
		     (zto1 exp)))
	       ;;	     ((and (and (equal ul 1.)
	       ;;			(equal ll 0.))  (zto1 exp)))
	       (t (dintegrate exp var ll ul)))
      (restore-defint-assumptions old-assumptions defint-assumptions))))
       

(defun dintegrate (exp var ll ul)
  (let ((ans nil) (arg nil) (scflag nil) 
	(*dflag nil) ($%emode t))
;;;NOT COMPLETE for sin's and cos's.
    (cond ((and (not sin-cos-recur)
		(oscip exp)
		(setq scflag t)
		(intsc1 ll ul exp)))
	  ((and (not rad-poly-recur)
		(notinvolve exp '(%log))
		(not (%einvolve exp))
		(method-radical-poly exp var ll ul)))
	  ((and (not (equal dintlog-recur 2.))
		(setq arg (involve exp '(%log)))
		(dintlog exp arg)))
	  ((and (not dintexp-recur)
		(setq arg (%einvolve exp))
		(dintexp exp var)))
	  ((and (not (ratp exp var)) 
		(setq ans ($expand exp))
		(not (alike1 ans exp))
		(intbyterm ans t)))
	  ((setq ans (antideriv exp))
	   (intsubs ans ll ul))
	  (t nil))))

(defun method-radical-poly (exp var ll ul)
;;;Recursion stopper
  (let ((rad-poly-recur t)		;recursion stopper
	(result ()))
    (cond ((and (sinintp exp var) 
		(setq result (antideriv exp))
		(intsubs result ll ul)))
	  ((and (ratp exp var)
		(setq result (ratfnt exp))))
	  ((and (setq result (antideriv exp))
		(intsubs result ll ul)))
	  ((and (not scflag)
		(not (eq ul '$inf))
		(radic exp var)
		(kindp34)
		(setq result (cv exp))))
	  (t ()))))

;;; LIMIT loss can't set logabs to true for these cases.
(defun principal-value-integral (exp var ll ul poles)
  (let (($logabs ())  (anti-deriv ()))
    (cond ((not (null (setq anti-deriv (antideriv exp))))
	   (cond ((not (null poles))
		  (order-limits 'ask)
		  (cond ((take-principal anti-deriv ll ul poles))
			(t ()))))))))

(defun take-principal (anti-deriv ll ul poles &aux ans merged-list)
  (setq anti-deriv (cond ((involve anti-deriv '(%log))
			  ($logcontract anti-deriv))
			 (t anti-deriv)))
  (setq ans 0.)
  (setq merged-list (interval-list poles ll ul))
  (do ((current-pole (cdr merged-list) (cdr current-pole))
       (previous-pole merged-list (cdr previous-pole)))
      ((null current-pole)  t)
    (setq ans (m+ ans	    
		  (intsubs anti-deriv (m+ (caar previous-pole) 'epsilon)
			   (m+ (caar current-pole) (m- 'epsilon))))))
			   
  ;;Hack answer to simplify "Correctly".
  (cond ((not (freeof '%log ans)) 
	 (setq ans ($logcontract ans))))
  (setq ans (get-limit (get-limit ans 'epsilon 0 '$plus) 'prin-inf '$inf))
  ;;Return setion.
  (cond ((or (null ans)
	     (not (free ans '$infinity)) 
	     (not (free ans '$ind)))  ())
	((or (among '$minf ans)
	     (among '$inf ans)
	     (among '$und ans))
	 (diverg))
	(t (principal) ans)))

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
  pole-list)

;; Assumes EXP is a rational expression with no polynomial part and
;; converts the finite integration to integration over a half-infinite
;; interval.  The substitution y = (x-a)/(b-x) is used.  Equivalently,
;; x = (b*y+a)/(y+1).
;;
;; (I'm guessing CV means Change Variable here.)
#+nil
(defun cv (exp)
  (if (not (or (real-infinityp ll) (real-infinityp ul)))
      (method-by-limits (intcv3 (m// (m+t ll (m*t ul var))
				     (m+t 1. var)) nil 'yx)
			var 0. '$inf)
      ()))

(defun cv (exp)
  (if (not (or (real-infinityp ll) (real-infinityp ul)))
      ;; FIXME!  This is a hack.  We apply the transformation with
      ;; symbolic limits and then substitute the actual limits later.
      ;; That way method-by-limits (usually?) sees a simpler
      ;; integrand.
      ;;
      ;; See Bugs 938235 and 941457.  These fail because $FACTOR is
      ;; unable to factor the transformed result.  This needs more
      ;; work (in other places).
      (let ((trans (intcv3 (m// (m+t 'll (m*t 'ul var))
				(m+t 1. var))
			   nil 'yx)))
	(setf trans (subst ll 'll trans))
	(setf trans (subst ul 'ul trans))
	(method-by-limits trans var 0. '$inf))
      ()))

(defun ratfnt (exp)
  (let ((e (pqr exp)))
    (cond ((equal 0. (car e))  (cv exp))
	  ((equal 0. (cdr e))  (eezz (car e) ll ul))
	  (t (m+t (eezz (car e) ll ul)
		  (cv (m// (cdr e) dn*)))))))

(defun pqr (e)
  (let ((varlist (list var)))
    (newvar e)
    (setq e (cdr (ratrep* e)))
    (setq dn* (pdis (ratdenominator e)))
    (setq e (pdivide (ratnumerator e) (ratdenominator e)))
    (cons (simplify (rdis (car e))) (simplify (rdis (cadr e))))))


(defun intbyterm (exp *nodiverg)
  (let ((saved-exp exp))
    (cond ((mplusp exp)
	   (let ((ans (catch 'divergent 
			(andmapcar #'(lambda (new-exp) 
				       (let ((*def2* t))
					 (defint new-exp var ll ul)))
				   (cdr exp)))))
	     (cond ((null ans) nil)
		   ((eq ans 'divergent)
		    (let ((*nodiverg nil))
		      (cond ((setq ans (antideriv saved-exp))
			     (intsubs ans ll ul))
			    (t nil))))
		   (t (sratsimp (m+l ans))))))
;;;If leadop isn't plus don't do anything.
	  (t nil))))

(defun kindp34 nil
  (numden exp)
  (let* ((d dn*)
	 (a (cond ((and (zerop1 ($limit d var ll '$plus))
			(eq (limit-pole (m+ exp (m+ (m- ll) var))
					var ll '$plus)
			    '$yes))
		   t)
		  (t nil)))
	 (b (cond ((and (zerop1 ($limit d var ul '$minus))
			(eq (limit-pole (m+ exp (m+ ul (m- var)))
					var ul '$minus)
			    '$yes))
		   t)
		  (t nil))))
    (or a b)))

(defun diverg nil
  (cond (*nodiverg (throw 'divergent 'divergent))
	(t (merror "Integral is divergent"))))

(defun make-defint-assumptions (ask-or-not)
  (cond ((null (order-limits ask-or-not))  ())
	(t (mapc 'forget defint-assumptions)
	   (setq defint-assumptions ())
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
		    (setq defint-assumptions
			  `(,(assume `((mgreaterp) ,var ,ll))
			    ,(assume `((mgreaterp) ,ul ,var)))))
		   ((eq sign-ul-ll '$neg)
		    (setq defint-assumptions
			  `(,(assume `((mgreaterp) ,var ,ul))
			    ,(assume `((mgreaterp) ,ll ,var))))))
	     (cond ((and (eq sign-ll '$pos)
			 (eq sign-ul '$pos))
		    (setq defint-assumptions
			  `(,(assume `((mgreaterp) ,var 0))
			    ,@defint-assumptions)))
		   ((and (eq sign-ll '$neg)
			 (eq sign-ul '$neg))
		    (setq defint-assumptions
			  `(,(assume `((mgreaterp) 0 ,var))
			    ,@defint-assumptions)))
		   (t defint-assumptions))))))

(defun restore-defint-assumptions (old-assumptions assumptions)
  (do ((llist assumptions (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (do ((llist old-assumptions (cdr llist)))
      ((null llist) t)
    (assume (car llist))))

(defun make-global-assumptions ()
  (setq global-defint-assumptions
	(cons (assume '((mgreaterp) *z* 0.))
	      global-defint-assumptions))
  ;; *Z* is a "zero parameter" for this package.
  ;; Its also used to transform.
  ;;  limit(exp,var,val,dir) -- limit(exp,tvar,0,dir)
  (setq global-defint-assumptions
	(cons (assume '((mgreaterp) epsilon 0.))
	      global-defint-assumptions))	   
  (setq global-defint-assumptions
	(cons (assume '((mlessp) epsilon 1.0e-8))
	      global-defint-assumptions)) 
  ;; EPSILON is used in principal vaule code to denote the familiar
  ;; mathematical entity.
  (setq global-defint-assumptions
	(cons (assume '((mgreaterp) prin-inf 1.0e+8))
	      global-defint-assumptions)))

;;; PRIN-INF Is a special symbol in the principal value code used to
;;; denote an end-point which is proceeding to infinity.

(defun forget-global-assumptions ()
  (do ((llist global-defint-assumptions (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (cond ((not (null integer-info))
	 (do ((llist integer-info (cdr llist)))
	     ((null llist) t)
	   (i-$remove `(,(cadar llist) ,(caddar llist)))))))

(defun order-limits (ask-or-not)
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
	   (cond ((eq ul '$inf) nil)
		 ((eq ll '$minf)
		  (setq exp (subin (m- var) exp))
		  (setq ll (m- ul))
		  (setq ul '$inf))
		 ((eq ll '$inf)
		  (setq ll ul)
		  (setq ul '$inf)
		  (setq exp (m- exp))))
	   ;;Fix limits so that ll < ul. 
	   (let ((d (complm ask-or-not)))
	     (cond ((equal d -1.)
		    (setq exp (m- exp))
		    (setq d ll)
		    (setq ll ul)
		    (setq ul d))
		   (t t))))))

(defun complm (ask-or-not)
  (let ((askflag (cond ((eq ask-or-not 'ask)  t)
		       (t nil)))
	(a ()))
    (cond ((alike1 ul ll)  0.)
	  ((eq (setq a (cond (askflag ($asksign ($limit (m+t ul (m- ll)))))
			     (t ($sign ($limit (m+t ul (m- ll)))))))
	       '$pos)
	   1.)
	  ((eq a '$neg)  -1.)
	  (t 1.))))


(defun intsubs (e a b)
  (cond ((easy-subs e a b))
	(t (setq current-assumptions
		 (make-defint-assumptions 'ask)) ;get forceful!
	   (let ((generate-atan2 ())  ($algebraic t)
		 (rpart ())  (ipart ()))
	     (desetq (rpart . ipart)
		     (cond ((not (free e '$%i))
			    (trisplit e))
			   (t (cons e 0))))
	     (cond ((not (equal (sratsimp ipart) 0))  
		    (let ((rans (cond ((limit-subs rpart a b))
				      (t (m-
					  `((%limit) ,rpart ,var ,b $minus)
					  `((%limit) ,rpart ,var ,a $plus)))))
			  (ians (cond ((limit-subs ipart a b))
				      (t (m-
					  `((%limit) ,ipart ,var ,b $minus)
					  `((%limit) ,ipart ,var ,a $plus))))))
		      (m+ rans (m* '$%i ians))))
		   (t (setq rpart (sratsimp rpart))
		      (cond ((limit-subs rpart a b))
			    (t (same-sheet-subs rpart a b)))))))))

(defun easy-subs (e ll ul)
  (cond ((or (infinityp ll) (infinityp ul)) ())
	(t (cond ((polyinx e var ())
		  (let ((ll-val (no-err-sub ll e))
			(ul-val (no-err-sub ul e)))
		    (cond ((and ll-val ul-val)  (m- ul-val ll-val))
			  (t ()))))
		 (t ())))))

(defun limit-subs (e ll ul)
  (cond ((not (free e '%atan))  ())
	(t (setq e ($multthru e))
	   (let ((a1 ($limit e var ll '$plus))	
		 (a2 ($limit e var ul '$minus)))
	     (cond ((memq a1 '($inf $minf $infinity ))
		    (cond ((memq a2 '($inf $minf $infinity))
			   (cond ((eq a2 a1)  (diverg))
				 (t ())))
			  (t (diverg))))
		   ((memq a2 '($inf $minf $infinity))  (diverg))
		   ((or (memq a1 '($und $ind))
			(memq a2 '($und $ind)))  ())
		   (t (m- a2 a1)))))))

;;;This function works only on things with ATAN's in them now.
(defun same-sheet-subs (exp ll ul &aux ans)
  (let ((poles (atan-poles exp ll ul)))
    ;;POLES -> ((mlist) ((mequal) ((%atan) foo) replacement) ......) 
    ;;We can then use $SUBSTITUTE
    (setq ans ($limit exp var ll '$plus))
    (setq exp (sratsimp ($substitute poles exp)))
    (m- ($limit exp var ul '$minus) ans)))

(defun atan-poles (exp ll ul)
  `((mlist) ,@(atan-pole1 exp ll ul)))

(defun atan-pole1 (exp ll ul &aux ipart)
  (cond 
    ((mapatom exp)  ())
    ((matanp exp)	 ;neglect multiplicity and '$unknowns for now.
     (desetq (exp . ipart) (trisplit exp))
     (cond 
       ((not (equal (sratsimp ipart) 0))  ())
       (t (let ((pole (poles-in-interval (let (($algebraic t))
					   (sratsimp (cadr exp)))
					 var ll ul)))
	    (cond ((and pole (not (or (eq pole '$unknown)
				      (eq pole '$no))))
		   (do ((l pole (cdr l)) (llist ()))
		       ((null l)  llist)
		     (cond 
		       ((eq (caar l) ll)  t) ;Skip this one by definition.
		       (t (let ((low-lim ($limit (cadr exp) var (caar l) '$minus))
				(up-lim ($limit (cadr exp) var (caar l) '$plus)))
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
	 (setq llist (append llist (atan-pole1 (car l) ll ul)))))))

(defun difapply (n d s fn1)
  (prog (k m r $noprincipal) 
     (cond ((eq ($asksign (m+ (deg d) (m- s) (m- 2.)))  '$neg)
	    (return nil)))
     (setq $noprincipal t)
     (cond ((or (not (mexptp d))
		(not (numberp (setq r (caddr d)))))
	    (return nil))
	   ((and (equal n 1.)
		 (eq fn1 'mtorat)
		 (equal 1. (deg (cadr d))))
	    (return 0.)))
     (setq m (deg (setq d (cadr d))))
     (setq k (m// (m+ s 2.) m))
     (cond ((eq (ask-integer (m// (m+ s 2.) m) '$any)  '$yes)
	    nil)
	   (t (setq k (m+ 1 k))))
     (cond ((eq ($sign (m+ r (m- k))) '$pos)
	    (return (diffhk fn1 n d k (m+ r (m- k)))))))) 

(defun diffhk (fn1 n d r m)
  (prog (d1 *dflag) 
     (setq *dflag t)
     (setq d1 (funcall fn1 n
		       (m^ (m+t '*z* d) r)
		       (m* r (deg d))))
     (cond (d1 (return (difap1 d1 r '*z* m 0.)))))) 

(defun principal nil
  (cond ($noprincipal (diverg))
	((not pcprntd)
	 (princ "Principal Value")
	 (setq pcprntd t))))

(defun rib (e s)
  (let (*updn c) 
    (cond ((or (mnump e) (constant e))
	   (setq bptu (cons e bptu)))
	  (t (setq e (rmconst1 e))
	     (setq c (car e))
	     (setq nn* (cdr e))
	     (setq nd* s)
	     (setq e (catch 'ptimes%e (ptimes%e nn* nd*)))
	     (cond ((null e) nil)
		   (t (setq e (m* c e))
		      (cond (*updn (setq bptu (cons e bptu)))
			    (t (setq bptd (cons e bptd))))))))))

(defun ptimes%e (term n)
  (cond ((polyinx term var nil) term)
	((and (mexptp term)
	      (eq (cadr term) '$%e)
	      (polyinx (caddr term) var nil)
	      (eq ($sign (m+ (deg ($realpart (caddr term))) -1.))
		  '$neg)
	      (eq ($sign (m+ (deg (setq nn* ($imagpart (caddr term)))) 
			     -2.))
		  '$neg))
	 (cond ((eq ($asksign (ratcoef nn* var)) '$pos) 
		(setq *updn t))
	       (t (setq *updn nil)))
	 term)
	((and (mtimesp term)
	      (setq nn* (polfactors term))
	      (or (null (car nn*))
		  (eq ($sign (m+ n (m- (deg (car nn*)))))
		      '$pos))
	      (ptimes%e (cadr nn*) n)
	      term))
	(t (throw 'ptimes%e nil))))

(defun csemidown (n d var)
  (let ((pcprntd t)) ;Not sure what to do about PRINCIPAL values here.
    (princip (res n d #'lowerhalf #'(lambda (x)
	        		      (cond ((equal ($imagpart x) 0)  t)
					    (t ())))))))

(defun lowerhalf (j)
  (eq ($asksign ($imagpart j)) '$neg))

(defun upperhalf (j)
  (eq ($asksign ($imagpart j)) '$pos))


(defun csemiup (n d var)
  (let ((pcprntd t)) ;I'm not sure what to do about PRINCIPAL values here.
    (princip (res n d #'upperhalf #'(lambda (x)
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


(defun sconvert (e)
  (cond ((atom e) e)
	((polyinx e var nil) e)
	((eq (caar e) '%sin)
	 (m* '((rat) -1. 2.)
	     '$%i
	     (m+t (m^t '$%e (m*t '$%i (cadr e)))
		  (m- (m^t '$%e (m*t (m- '$%i) (cadr e)))))))
	((eq (caar e) '%cos)
	 (mul* '((rat) 1. 2.)
	       (m+t (m^t '$%e (m*t '$%i (cadr e)))
		    (m^t '$%e (m*t (m- '$%i) (cadr e))))))
	(t (simplify
	    (cons (list (caar e)) (mapcar #'sconvert (cdr e)))))))

(defun polfactors (exp)
  (let (poly rest)
    (cond ((mplusp exp)  nil)
	  (t (cond ((mtimesp exp)
		    (setq exp (reverse (cdr exp))))
		   (t (setq exp (list exp))))
	     (mapc #'(lambda (term)
		       (cond ((polyinx term var nil)
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

(defun mtosc (grand)
  (numden grand)
  (let ((n nn*)
	(d dn*)
	plf bptu bptd s upans downans)
    (cond ((not (or (polyinx d var nil)
		    (and (setq grand (%einvolve d))
			 (among '$%i grand)
			 (polyinx (setq d ($ratsimp (m// d (m^t '$%e grand))))
				  var
				  nil)
			 (setq n (m// n (m^t '$%e grand))))))  nil)
	  ((equal (setq s (deg d)) 0)  nil)
;;;Above tests for applicability of this method.
	  ((and (or (setq plf (polfactors n))  t)
		(setq n ($expand (cond ((car plf)
					(m*t 'x* (sconvert (cadr plf))))
				       (t (sconvert n)))))
		(cond ((mplusp n)  (setq n (cdr n)))
		      (t (setq n (list n))))
		(do ((do-var n (cdr do-var)))
		    ((null do-var) t)
		  (cond ((rib (car do-var) s))
			(t (return nil))))
;;;Function RIB sets up the values of BPTU and BPTD
		(cond ((car plf)
		       (setq bptu (subst (car plf) 'x* bptu))
		       (setq bptd (subst (car plf) 'x* bptd))
		       t)	 ;CROCK, CROCK. This is TERRIBLE code.
		      (t t))
;;;If there is BPTU then CSEMIUP must succeed.
;;;Likewise for BPTD.
		(cond (bptu (cond ((setq upans (csemiup (m+l bptu) d var)))
				  (t nil)))
		      (t (setq upans 0)))
		(cond (bptd (cond ((setq downans (csemidown (m+l bptd) d var)))
				  (t nil)))
		      (t (setq downans 0))))
	   (sratsimp (m* '$%pi (m+ upans (m- downans))))))))


(defun evenfn (e var)
  (let ((temp (m+ (m- e)
		  (cond ((atom var)
			 ($substitute (m- var) var e))
			(t ($ratsubst (m- var) var e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 ($ratsimp temp))
	   t)
	  (t nil))))
		
(defun oddfn (e var)       
  (let ((temp (m+ e (cond ((atom var)
			   ($substitute (m- var) var e))
			  (t ($ratsubst (m- var) var e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 ($ratsimp temp))
	   t)
	  (t nil))))

(defun ztoinf (grand var)
  (prog (n d sn* sd* varlist
	 s nc dc
	 ans r $savefactors checkfactors temp test-var)
     (setq $savefactors t sn* (setq sd* (list 1.)))
     (cond ((eq ($sign (m+ loopstop* -1))
		'$pos)
	    (return nil))
	   ((setq temp (or (scaxn grand)
			   (ssp grand))) 
	    (return temp))
	   ((involve grand '(%sin %cos %tan))
	    (setq grand (sconvert grand))
	    (go on)))

     (cond ((polyinx grand var nil)
	    (diverg))
	   ((and (ratp grand var)
		 (mtimesp grand)
		 (andmapcar #'snumden (cdr grand)))
	    (setq nn* (m*l sn*)
		  sn* nil)
	    (setq dn* (m*l sd*)
		  sd* nil))
	   (t (numden grand)))
;;;
;;;New section.
     (setq n (rmconst1 nn*))
     (setq d (rmconst1 dn*))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d var nil) 
	    (setq s (deg d)))
	   (t (go findout)))
     (cond ((and (setq r (findp n))
		 (eq (ask-integer r '$integer) '$yes)
		 (setq test-var (bxm d s))
		 (setq ans (apply 'fan (cons (m+ 1. r) test-var))))
	    (return (m* (m// nc dc) ($ratsimp ans))))
	   ((and (ratp grand var)
		 (setq ans (zmtorat n (cond ((mtimesp d) d)
					    (t ($sqfr d)))
				    s #'ztorat)))
	    	   (return (m* (m// nc dc) ans)))
	   ((and (evenfn d var) 
		 (setq nn* (p*lognxp n s)))
	    (setq ans (log*rat (car nn*) d (cadr nn*)))
	    (return (m* (m// nc dc) ans)))
	   ((involve grand '(%log))
	    (cond ((setq ans (logquad0 grand))
		   (return (m* (m// nc dc) ans)))
		  (t (return nil)))))
     findout
     (cond ((setq temp (batapp grand)) 
	    (return temp))
	   (t nil))
     on
     (cond ((let ((mtoinf* nil))
	      (setq temp (ggr grand t)))
	    (return temp))
	   ((mplusp grand)
	    (cond ((let ((*nodiverg t))
		     (setq ans (catch 'divergent
				 (andmapcar #'(lambda (g)
						(ztoinf g var))
					    (cdr grand)))))
		   (cond ((eq ans 'divergent) nil)
			 (t (return (sratsimp (m+l ans)))))))))

     (cond ((and (evenfn grand var)
		 (setq loopstop* (m+ 1 loopstop*))
		 (setq ans (method-by-limits grand var '$minf '$inf)))
	    (return (m*t '((rat) 1. 2.) ans)))
	   (t (return nil)))))
   
(defun ztorat (n d s)
  (cond ((and (null *dflag)
	      (setq s (difapply n d nn* #'ztorat)))
	 s)
	((setq n (let ((plogabs ()))
		   (keyhole (m* `((%plog) ,(m- var)) n) d var)))
	 (m- n))
	(t (merror "Keyhole failed"))))

(setq *dflag nil) 

(defun logquad0 (exp)
  (let ((a ()) (b ())  (c ()))
    (cond ((setq exp (logquad exp))
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
	
(defun logquad (exp)
  (let ((varlist (list var)))
    (newvar exp)
    (setq exp (cdr (ratrep* exp)))
    (cond ((and (alike1 (pdis (car exp))
			`((%log) ,var))
		(not (atom (cdr exp)))
		(equal (cadr (cdr exp)) 2.)
		(not (equal (pterm (cddr exp) 0.) 0.)))
	   (setq exp (mapcar 'pdis (cdr (oddelm (cdr exp)))))))))

(defun mtoinf (grand var) 
  (prog (ans sd* sn* p* pe* n d s nc dc $savefactors checkfactors temp)
     (setq $savefactors t)
     (setq sn* (setq sd* (list 1.)))
     (cond ((eq ($sign (m+ loopstop* -1)) '$pos)
	    (return nil))
	   ((involve grand '(%sin %cos))
	    (cond ((and (evenfn grand var)
			(or (setq temp (scaxn grand))
			    (setq temp (ssp grand))))
		   (return (m*t 2. temp)))
		  ((setq temp (mtosc grand))
		   (return temp))
		  (t (go en))))
	   ((among '$%i (%einvolve grand))
	    (cond ((setq temp (mtosc grand))
		   (return temp))
		  (t (go en)))))
     (cond ((polyinx grand var nil)
	    (diverg))
	   ((and (ratp grand var)
		 (mtimesp grand)
		 (andmapcar #'snumden (cdr grand)))
	    (setq nn* (m*l sn*) sn* nil)
	    (setq dn* (m*l sd*) sd* nil))
	   (t (numden grand)))
     (setq n (rmconst1 nn*))
     (setq d (rmconst1 dn*))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d var nil)
	    (setq s (deg d))))
     (cond ((and (not (%einvolve grand))
		 (notinvolve exp '(%sinh %cosh %tanh))
		 (setq p* (findp n))
		 (eq (ask-integer p* '$integer) '$yes)
		 (setq pe* (bxm d s)))
	    (cond ((and (eq (ask-integer (caddr pe*) '$even) '$yes)
			(eq (ask-integer p* '$even) '$yes))
		   (cond ((setq ans (apply 'fan (cons (m+ 1. p*) pe*)))
			  (setq ans (m*t 2. ans))
			  (return (m* (m// nc dc) ans)))))
		  ((equal (car pe*) 1.)
		   (cond ((and (setq ans (apply 'fan (cons (m+ 1. p*) pe*)))
			       (setq nn* (fan (m+ 1. p*)
					      (car pe*)
					      (m* -1.(cadr pe*))
					      (caddr pe*)
					      (cadddr pe*))))
			  (setq ans (m+ ans (m*t (m^ -1. p*) nn*)))
			  (return (m* (m// nc dc) ans))))))))
     (cond ((ratp grand var)
	    (setq ans (m*t '$%pi (zmtorat n (cond ((mtimesp d) d)
						  (t ($sqfr d)))
					  s
					  #'mtorat)))
	    (return (m* (m// nc dc) ans)))
	   ((and (or (%einvolve grand)
		     (involve grand '(%sinh %cosh %tanh)))
		 (p*pin%ex n)	      ;setq's P* and PE*...Barf again.
		 (setq ans (catch 'pin%ex (pin%ex d))))
	    (cond ((null p*)
		   (return (dintexp grand var)))
		  ((and (zerop1 (get-limit grand var '$inf))
			(zerop1 (get-limit grand var '$minf))
			(setq ans (rectzto%pi2 (m*l p*) (m*l pe*) d)))
		   (return (m* (m// nc dc) ans)))
		  (t (diverg)))))
     en
     (cond ((setq ans (ggrm grand)) 
	    (return ans))
	   ((and (evenfn grand var)
		 (setq loopstop* (m+ 1 loopstop*))
		 (setq ans (method-by-limits grand var 0 '$inf)))
	    (return (m*t 2. ans)))
	   (t (return nil)))))

(defun linpower0 (exp var)
  (cond ((and (setq exp (linpower exp var))
	      (eq (ask-integer (caddr exp) '$even)
		  '$yes)
	      (ratgreaterp 0. (car exp)))
	 exp)))

;;; given (b*x+a)^n+c returns  (a b n c)
(defun linpower (exp var)
  (let (linpart deg lc c varlist) 
    (cond ((not (polyp exp))   nil)
	  (t (let ((varlist (list var)))
	       (newvar exp)
	       (setq linpart (cadr (ratrep* exp)))
	       (cond ((atom linpart)
		      nil)
		     (t (setq deg (cadr linpart)) 
;;;get high degree of poly
			(setq linpart ($diff exp var (m+ deg -1))) 
;;;diff down to linear.
			(setq lc (sdiff linpart var))	
;;;all the way to constant.
			(setq linpart ($ratsimp (m// linpart lc))) 
			(setq lc ($ratsimp (m// lc `((mfactorial) ,deg))))
;;;get rid of factorial from differentiation.
			(setq c ($ratsimp (m+ exp (m* (m- lc)
						      (m^ linpart deg)))))))
;;;Sees if can be expressed as (a*x+b)^n + part freeof x.
	       (cond ((not (among var c))
		      `(,lc ,linpart ,deg ,c))
		     (t nil)))))))

(defun mtorat (n d s)
  (let ((semirat* t)) 
    (cond ((and (null *dflag)
		(setq s (difapply n d s #'mtorat)))
	   s)
	  (t (csemiup n d var)))))

(defun zmtorat (n d s fn1)
  (prog (c) 
     (cond ((eq ($sign (m+ s (m+ 1 (setq nn* (deg n))))) 
		'$neg)
	    (diverg))
	   ((eq ($sign (m+ s -4))
		'$neg)
	    (go on)))
     (setq d ($factor d))
     (setq c (rmconst1 d))
     (setq d (cdr c))
     (setq c (car c))
     (cond
       ((mtimesp d)
	(setq d (cdr d))
	(setq n (partnum n d))
	(let ((rsn* t))
	  (setq n ($xthru (m+l
			   (mapcar #'(lambda (a b) 
				       (m// (funcall fn1 (car a) b (deg b)) 
					    (cadr a)))
				   n
				   d)))))
	(return (cond (c (m// n c)) 
		      (t n)))))
     on

     (setq n (funcall fn1 n d s))
     (return  (sratsimp (cond (c  (m// n c))
			      (t n))))))

(defun pfrnum (f g n n2 var)
  (let ((varlist (list var))  genvar)
    (setq f (polyform f)
	  g (polyform g)
	  n (polyform n)
	  n2 (polyform n2))
    (setq var (caadr (ratrep* var)))
    (setq f (resprog0 f g n n2))
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
			(f* -1 (cadr d))
			(ptimes (cadr f) (caddr d)))))
     (merror "Bug from `pfrnum' in `residu'")))

(defun partnum (n dl)
  (let ((n2 1)  ans nl)
    (do ((dl dl (cdr dl)))
	((null (cdr dl))
	 (nconc ans (ncons (list n n2))))
      (setq nl (pfrnum (car dl) (m*l (cdr dl)) n n2 var))
      (setq ans (nconc ans (ncons (car nl))))
      (setq n2 (cadadr nl) n (caadr nl) nl nil))))

(defun ggrm (e)
  (prog (poly expo mtoinf* mb  varlist  genvar l c gvar) 
     (setq varlist (list var))
     (setq mtoinf* t)
     (cond ((and (setq expo (%einvolve e))
		 (polyp (setq poly ($ratsimp (m// e (m^t '$%e expo)))))
		 (setq l (catch 'ggrm (ggr (m^t '$%e expo) nil))))
	    (setq mtoinf* nil)
	    (setq mb (m- (subin 0. (cadr l))))
	    (setq poly (m+ (subin (m+t mb var) poly)
			   (subin (m+t mb (m*t -1. var)) poly))))
	   (t (return nil)))
     (setq expo (caddr l)
	   c (cadddr l)
	   l (m* -1. (car l))
	   e nil)
     (newvar poly)
     (setq poly (cdr (ratrep* poly)))
     (setq mb (m^ (pdis (cdr poly)) -1.) 
	   poly (car poly))
     (setq gvar (caadr (ratrep* var)))
     (cond ((or (atom poly)
		(pointergp gvar (car poly))) 
	    (setq poly (list 0. poly)))
	   (t (setq poly (cdr poly))))
     (return (do ((poly poly (cddr poly)))
		 ((null poly)
		  (mul* (m^t '$%e c) (m^t expo -1.) mb (m+l e)))
	       (setq e (cons (ggrm1 (car poly) (pdis (cadr poly)) l expo)
			     e))))))

(defun ggrm1 (d k a b)
  (setq b (m// (m+t 1. d) b))
  (m* k `((%gamma) ,b) (m^ a (m- b)))) 

(defun radic (e v) 
  ;;If rd* is t the m^ts must just be free of var.
  ;;If rd* is () the m^ts must be mnump's.
  (let ((rd* ())) 
    (radicalp e v)))

(defun keyhole (n d var)
  (let ((semirat* ()))
    (setq n (res n d #'(lambda (j) 
			 (or (not (equal ($imagpart j) 0))
			     (eq ($asksign j) '$neg)))
		 #'(lambda (j)
		     (cond ((eq ($asksign j) '$pos)
			    t)
			   (t (diverg))))))
    (let ((rsn* t))
      ($rectform ($multthru (m+ (cond ((car n) (car n))
				      (t 0.))
				(cond ((cadr n) (cadr n))
				      (t 0.))))))))

(defun skr (e)
  (prog (m r k) 
     (cond ((atom e) (return nil)))
     (setq e (partition e var 1))
     (setq m (car e))
     (setq e (cdr e))
     (cond ((setq r (sinrx e)) (return (list m r 1)))
	   ((and (mexptp e)
		 (eq (ask-integer (setq k (caddr e)) '$integer) '$yes)
		 (setq r (sinrx (cadr e))))
	    (return (list m r k)))))) 

(defun sinrx (e)
  (cond ((and (consp e) (eq (caar e) '%sin))
	 (cond ((eq (cadr e) var)
		1.)
	       ((and (setq e (partition (cadr e) var 1))
		     (eq (cdr e) var))
		(car e))))))

(declare-top(special n)) 


(defun ssp (exp)
  (prog (u n c) 
     (setq exp ($substitute (m^t `((%sin) ,var) 2.)
			    (m+t 1. (m- (m^t `((%cos) ,var) 2.)))
			    exp))
     (numden exp)
     (setq u nn*)
     (cond ((and (setq n (findp dn*))
		 (eq (ask-integer n '$integer) '$yes))
	    (cond ((setq c (skr u)) 
		   (return (scmp c n)))
		  ((and (mplusp u)
			(setq c (andmapcar #'skr (cdr u))))
		   (return (m+l (mapcar #'(lambda (j) (scmp j n))
					c)))))))))

(declare-top(unspecial n)) 

(defun scmp (c n)
  (m* (car c) (m^ (cadr c) (m+ n -1)) `((%signum) ,(cadr c))
      (sinsp (caddr c) n)))

(defun sevn (n)
  (m* half%pi ($makegamma `((%binomial) ,(m+t (m+ n -1) '((rat) -1. 2.))
			    ,(m+ n -1)))))


(defun sforx (n)
  (cond ((equal n 1.) 
	 half%pi) 
	(t (bygamma (m+ n -1) 0.)))) 

(defun sinsp (l k)
  (let ((i ())
	(j ()))
    (cond ((eq ($sign (m+ l (m- (m+ k -1))))
	       '$neg)
	   (diverg))
	  ((not (even1 (m+ l k)))
	   nil)
	  ((equal k 2.)
	   (sevn (m// l 2.)))
	  ((equal k 1.) 
	   (sforx l))
	  ((eq ($sign  (m+ k -2.))
	       '$pos)
	   (setq i (m* (m+ k -1) (setq j (m+ k -2.))))
	   (m+ (m* l (m+ l -1) (m^t i -1.) (sinsp (m+ l -2.) j))
	       (m* (m- (m^ l 2)) (m^t i -1.)
		   (sinsp l j)))))))

(defun fpart (a)
  (cond ((null a) 0.)
	((numberp a) 0.)
	((mnump a)
	 (list (car a) (remainder (cadr a) (caddr a)) (caddr a)))
	((and (atom a) (abless1 a)) a)
	((and (mplusp a)
	      (null (cdddr a))
	      (abless1 (caddr a)))
	 (caddr a)))) 

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

(defun period (p e var)
  (and (alike1 (no-err-sub var e) (setq e (no-err-sub (m+ p var) e)))
       ;; means there was no error
       (not (eq e t))))

(defun infr (a)
  (let ((var '$%i)
	(r (subin 0. a))
	c)
    (setq c (subin 1. (m+ a (m*t -1. r))))
    (setq a (igprt (m* '((rat) 1. 2.) c)))
    (cons a (m+ r (m*t (m+ c (m* -2. a)) '$%pi)))))

(defun igprt (r) 
  (m+ r (m* -1. (fpart r)))) 


;;;Try making exp(%i*var) --> yy, if result is rational then do integral
;;;around unit circle. Make corrections for limits of integration if possible.
(defun scrat (sc b)
  (let* ((exp-form (sconvert sc))	;Exponentialize
	 (rat-form (maxima-substitute 'yy (m^t '$%e (m*t '$%i var))
				      exp-form))) ;Try to make Rational fun.
    (cond ((and (ratp rat-form 'yy)
		(not (among var rat-form)))
	   (cond ((alike1 b %pi2) 
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans ans)
			  (t nil))))
		 ((and (eq b '$%pi)
		       (evenfn exp-form var))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 2.) ans))
			  (t nil))))
		 ((and (alike1 b half%pi)
		       (evenfn exp-form var)
		       (alike1 rat-form 
			       (no-err-sub (m+t '$%pi (m*t -1. var))
					   rat-form)))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 4.) ans))
			  (t nil)))))))))

;;; Do integrals of sin and cos. this routine makes sure lower limit
;;; is zero.
(defun intsc1 (a b e)
  (let ((limit-diff (m+ b (m* -1 a)))
	($%emode t)
	($trigsign t)
	(sin-cos-recur t))		;recursion stopper
    (prog (ans d nzp2 l) 
       (cond ((or (not (mnump (m// limit-diff '$%pi)))
		  (not (period %pi2 e var)))
	      (return nil))
	     ((not (equal a 0.))
	      (setq e (maxima-substitute (m+ a var) var e))
	      (setq a 0.)
	      (setq b limit-diff)))
;;;Multiples of 2*%pi in limits.
       (cond ((eq (ask-integer (setq d (let (($float nil))
					 (m// limit-diff %pi2))) '$integer)
		  '$yes)
	      (setq ans (m* d (cond ((setq ans (intsc e %pi2 var))
				     (return ans))
				    (t (return nil)))))))
       (cond ((ratgreaterp %pi2 b)
	      (return (intsc e b var)))
	     (t (setq l a) 
		(setq a 0.)))
       (setq b (infr b))
       (cond ((null l) 
	      (setq nzp2 (car b))
	      (setq limit-diff 0.)
	      (go out)))
       (setq l (infr l))
       (setq limit-diff
	     (m*t -1. (cond ((setq ans (intsc e (cdr l) var)) 
			     ans)
			    (t (return nil)))))
       (setq nzp2 (m+ (car b) (m- (car l))))
       out  (setq ans (add* (cond ((zerop1 nzp2) 0.)
				  ((setq ans (intsc e %pi2 var))
				   (m*t nzp2 ans))
				  (t (return nil)))
			    (cond ((zerop1 (cdr b)) 0.)
				  ((setq ans (intsc e (cdr b) var))
				   ans)
				  (t (return nil)))
			    limit-diff))
       (return ans))))

(defun intsc (sc b var)
  (cond ((eq ($sign b) '$neg)
	 (setq b (m*t -1. b))
	 (setq sc (m* -1. (subin (m*t -1. var) sc)))))
  (setq sc (partition sc var 1))
  (cond ((setq b (intsc0 (cdr sc) b var))
	 (m* (resimplify (car sc)) b))))

(defun intsc0 (sc b var)
  (let ((nn* (scprod sc))
	(dn* ()))
    (cond (nn* (cond ((alike1 b half%pi)
		      (bygamma (car nn*) (cadr nn*)))
		     ((eq b '$%pi)
		      (cond ((eq (real-branch (cadr nn*) -1.) '$yes)
			     (m* (m+ 1. (m^ -1 (cadr nn*)))
				 (bygamma (car nn*) (cadr nn*))))))
		     ((alike1 b %pi2)
		      (cond ((or (and (eq (ask-integer (car nn*) '$even)
					  '$yes)
				      (eq (ask-integer (cadr nn*) '$even)
					  '$yes))
				 (and (ratnump (car nn*))
				      (eq (real-branch (car nn*) -1.)
					  '$yes)
				      (ratnump (cadr nn*))
				      (eq (real-branch (cadr nn*) -1.)
					  '$yes)))
			     (m* 4.	(bygamma (car nn*) (cadr nn*))))
			    ((or (eq (ask-integer (car nn*) '$odd) '$yes)
				 (eq (ask-integer (cadr nn*) '$odd) '$yes))
			     0.)
			    (t nil)))
		     ((alike1 b half%pi3)
		      (m* (m+ 1. (m^ -1 (cadr nn*)) (m^ -1 (m+l nn*)))
			  (bygamma (car nn*) (cadr nn*))))))
	  (t (cond ((and (or (eq b '$%pi)
			     (alike1 b %pi2)
			     (alike1 b half%pi))
			 (setq dn* (scrat sc b)))
		    dn*)
		   ((setq nn* (antideriv sc))
		    (sin-cos-intsubs nn* var 0. b))
		   (t ()))))))

;;;Is careful about substitution of limits where the denominator may be zero
;;;because of various assumptions made.
(defun sin-cos-intsubs (exp var ll ul)
  (cond ((mplusp exp)
	 (m+l (mapcar #'sin-cos-intsubs1 (cdr exp))))
	(t (sin-cos-intsubs1 exp))))

(defun sin-cos-intsubs1 (exp)	 
  (let* ((rat-exp ($rat exp))
	 (num (pdis (cadr rat-exp)))
	 (denom (pdis (cddr rat-exp))))
    (cond ((not (equal (intsubs num ll ul) 0.))
	   (intsubs exp ll ul))
	  ;; Why do we want to return zero when the denom is not zero?
	  ;; That doesn't seem to make sense to me (rtoy).  Checking
	  ;; for a zero denominator makes sense, but what we should
	  ;; return in that case?  0 seems like a bad choice.  $inf or
	  ;; $undefined seem like better choices.  Or maybe just
	  ;; signaling an error?
	  #+nil  
	  ((not (equal ($asksign denom) '$zero))
	   0.)
	  ((equal ($asksign denom) '$zero)
	   '$undefined)
	  (t (let (($%piargs ()))
	       (intsubs exp ll ul))))))

(defun scprod (e)
  (let ((great-minus-1 #'(lambda (temp)
			   (ratgreaterp temp -1)))
	m n)
    (cond
      ((setq m (powerofx e `((%sin) ,var) great-minus-1 var))
       (list m 0.))
      ((setq n (powerofx e `((%cos) ,var) great-minus-1 var))
       (setq m 0.)
       (list 0. n))
      ((and (mtimesp e)
	    (or (setq m (powerofx (cadr e) `((%sin) ,var) great-minus-1 var))
		(setq n (powerofx (cadr e) `((%cos) ,var) great-minus-1 var)))
	    (cond
	      ((null m)
	       (setq m (powerofx (caddr e) `((%sin) ,var) great-minus-1 var)))
	      (t (setq n (powerofx (caddr e) `((%cos) ,var) great-minus-1 var))))
	    (null (cdddr e)))
       (list m n))
      (t ()))))

(defun real-branch (exponent value)
  ;; Says wether (m^t value exponent) has at least one real branch.
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

(defun bygamma (m n)
  (let ((one-half (m//t 1. 2.)))
    (m* one-half `(($beta) ,(m* one-half (m+t 1. m))
		   ,(m* one-half (m+t 1. n))))))

;;Seems like Guys who call this don't agree on what it should return.
(defun powerofx (e x p var)
  (setq e (cond ((not (among var e)) nil)
		((alike1 e x) 1.)
		((atom e) nil)
		((and (mexptp e)
		      (alike1 (cadr e) x)
		      (not (among var (caddr e))))
		 (caddr e))))
  (cond ((null e) nil)
	((funcall p e) e))) 

(declare-top(special l c k)) 

(comment (the following func is not complete)) 

;;(DEFUN ZTO1 (E)
;;  (prog (ans k l)
;;    (COND ((NOTINVOLVE E '(%SIN %COS %TAN %LOG))
;;	   (cond ((SETQ ANS (BATAP E))
;;		  (return ans)))))
;;    (cond ((AND (NOTINVOLVE E '(%SIN %COS %TAN))
;;		(AMONG '%LOG E))
;;	   (COND ((SETQ ANS (BATAP (M// E `((%LOG) ,VAR))))
;;		  (SETQ K NN* L DN*)
;;		  (SETQ ANS (m* ANS
;;				(m+ (subfunmake '$PSI '(0) (list K))
;;				    (m* -1. (subfunmake '$PSI
;;							'(0)
;;							(ncons (m+ K
;;								   L)))))))
;;		  (return ans)))))))



(defun bata0 (e)
  (cond ((atom e) nil)
	((and (mtimesp e)
	      (null (cdddr e))
	      (or (and (setq k (findp (cadr e)))
		       (setq c (bxm (caddr e) (polyinx (caddr e) var nil))))
		  (and (setq k (findp (caddr e)))
		       (setq c (bxm (cadr e) (polyinx (cadr e) var nil))))))
	 t)
	((setq c (bxm e (polyinx e var nil)))
	 (setq k 0.)))) 

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


;;; Integrals of the form i(log(x)^m*x^k*(a+b*x^n)^l,x,0,ul) with
;;; ul>0, b<0, a=-b*ul^n, k>-1, l>-1, n>0, m a nonnegative integer. 
;;; These integrals are essentially partial derivatives of the 
;;; Beta function (i.e. the Eulerian integral of the first kind).
;;; Note that, currently, with the default setting intanalysis:true,
;;; this function might not even be called for some of these integrals.
;;; However, this can be palliated by setting intanalysis:false. 

(defun zto1 (e)				
  (when (or (mtimesp e) (mexptp e))
    (let ((m 0) (log (list '(%log) var)))
      (flet ((set-m (p) (setq m p)))
	(find-if #'(lambda (fac) (powerofx fac log #'set-m var)) (cdr e)))
      (when (and (freeof var m) 
		 (eq (ask-integer m '$integer) '$yes)
		 (not (eq ($asksign m) '$neg))) 
	(setq e (m//t e (list '(mexpt) log m)))
	(multiple-value-bind
	      (k/n l n b) (batap-new e)
	  (when k/n
	    (let ((beta (simplify (list '($beta) k/n l)))
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
		  ($at ($diff (m*t (m^t ul (m*t n var)) (list '($beta) var l))
			      var m) (list '(mequal) var k/n))) 
		 beta))))))))))


;;; If e is of the form given below, make the obvious change
;;; of variables (substituting ul*x^(1/n) for x) in order to reduce
;;; e to the usual form of the integrand in the Eulerian
;;; integral of the first kind.
;;; N. B: The old version of ZTO1 completely ignored this  
;;; substitution; the log(x)s were just thrown in, which,
;;; of course would give wrong results.

(defun batap-new (e) 
  (let (k c) 
    (declare (special k c))
    ;; Parse e
    (when (bata0 e)
      (multiple-value-bind
	    ;; e=x^k*(a+b*x^n)^l
	    (l a n b)  (values-list c)
	(when (and (freeof var k) (freeof var n) (freeof var l)
		   (alike1 a (m-t (m*t b (m^t ul n))))
		   (eq ($asksign b) '$neg)
		   (eq ($asksign (setq k (m1+t k))) '$pos)
		   (eq ($asksign (setq l (m1+t l))) '$pos)
		   (eq ($asksign n) '$pos))
	  (values (m//t k n) l n b))))))



(defun batapp (e)
  (prog (k c d l al) 
     (cond ((not (or (equal ll 0) (eq ll '$minf)))
	    (setq e (subin (m+ ll var) e))))
     (cond ((not (bata0 e)) (return nil))
	   ((and (ratgreaterp (setq al (caddr c)) 0.)
		 (eq ($asksign (setq k (m// (m+ 1. k)
					    al)))
		     '$pos)
		 (ratgreaterp (setq l (m* -1. (car c)))
			      k)
		 (eq ($asksign (m* (setq d (cadr c))
				   (setq c (cadddr c))))
		     '$pos))
	    (setq l (m+ l (m*t -1. k)))
	    (return (m// `(($beta) ,k ,l)
			 (mul* al (m^ c k) (m^ d l)))))))) 

(declare-top(unspecial l c k)) 

;; Compute exp(d)*gamma((c+1)/b)/b/a^((c+1)/b).  In essence, this is
;; the value of integrate(x^c*exp(d-a*x^b),x,0,inf).
(defun gamma1 (c a b d)
  (m* (m^t '$%e d)
      (m^ (m* b (m^ a (setq c (m// (m+t c 1.) b))))
	  -1.)
      `((%gamma) ,c)))
       
(defun zto%pi2 (grand var)
  (let ((result (unitcir ($ratsimp (m// grand var)) var)))
    (cond (result (sratsimp (m* (m- '$%i) result)))
	  (t nil))))

(defun unitcir (grand var)
  (numden grand)
  (let ((result (princip (res nn* dn* #'(lambda (pt)
					  (ratgreaterp 1 (cabs pt)))
			      #'(lambda (pt)
				  (alike1 1 (cabs pt)))))))
    (cond (result (m* '$%pi result))
	  (t nil))))


(defun logx1 (exp ll ul)
  (let ((arg nil))
    (cond
      ((and (notinvolve exp '(%sin %cos %tan %atan %asin %acos))
	    (setq arg (involve exp '(%log))))
       (cond ((eq arg var)
	      (cond ((ratgreaterp 1. ll)
		     (cond ((not (eq ul '$inf))
			    (intcv1 (m^t '$%e (m- var)) () (m- `((%log) ,var))))
			   (t (intcv1 (m^t '$%e var) () `((%log) ,var)))))))
	     (t (intcv arg nil nil)))))))


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
(defun scaxn (e)
  (let (ind s g) 
    (cond ((atom e)  nil)
	  ((and (or (eq (caar e) '%sin)
		    (eq (caar e) '%cos))
		(setq ind (caar e))
		(setq e (bx**n (cadr e))))
	   ;; Ok, we have cos(b*x^n) or sin(b*x^n), and we set e = (a
	   ;; b n) where the arg of the trig function is b*x^n+a.
	   (cond ((equal (car e) 1.)
		  ;; a = 1.  Give up.
		  '$ind)
		 ((zerop (setq s (let ((sign ($asksign (cadr e))))
				   (cond ((eq sign '$pos) 1)
					 ((eq sign '$neg) -1)
					 ((eq sign '$zero) 0)))))
		  ;; s is the sign of b.  Give up if it's zero.
		  nil)
		 ((not (eq ($asksign (m+ -1 (car e)))  '$pos))
		  ;; Give up if a-1 <= 0
		  nil)
		 (t
		  ;; We can apply our formula now.  g = gamma(1/n)/n/b^(1/n)
		  (setq g (gamma1 0. (m* s (cadr e)) (car e) 0.))
		  (setq e (m* g `((,ind) ,(m// half%pi (car e))))) 
		  (m* (cond ((and (eq ind '%sin)
				  (equal s -1.))
			     -1.)
			    (t 1.))
		      e)))))))
		      

(comment this is the second part of the definite integral package) 

(declare-top(special var plm* pl* rl* pl*1 rl*1)) 

(defun p*lognxp (a s)
  (let (b) 
    (cond ((not (among '%log a)) 
	   ())
	  ((and (polyinx (setq b (maxima-substitute 1. `((%log) ,var) a))
			 var t)
		(eq ($sign (m+ s (m+ 1 (deg b))))
		    '$pos)
		(evenfn b var)
		(setq a (lognxp ($ratsimp (m// a b)))))
	   (list b a)))))

(defun lognxp (a)
  (cond ((atom a) nil)
	((and (eq (caar a) '%log) 
	      (eq (cadr a) var)) 1.)
	((and (mexptp a)
	      (numberp (caddr a))
	      (lognxp (cadr a)))
	 (caddr a)))) 

(comment check the following function for unused prog var a) 

(defun logcpi0 (n d)
  (prog (pl dp) 
     (setq pl (polelist d #'upperhalf #'(lambda (j)
					  (cond ((zerop1 j) nil)
						((equal ($imagpart j) 0)
						 t)))))
     (cond ((null pl)
	    (return nil)))
     (setq factors (car pl) 
	   pl (cdr pl))
     (cond ((or (cadr pl)
		(caddr pl))
	    (setq dp (sdiff d var))))
     (cond ((setq plm* (car pl))
	    (setq rlm* (residue n (cond (leadcoef factors)
					(t d))
				plm*))))
     (cond ((setq pl* (cadr pl)) 
	    (setq rl* (res1 n dp pl*))))
     (cond ((setq pl*1 (caddr pl))
	    (setq rl*1 (res1 n dp pl*1))))
     (return (m*t (m//t 1. 2.)
		  (m*t '$%pi 
		       (princip 
			(list (cond ((setq nn* (append rl* rlm*))
				     (m+l nn*)))
			      (cond (rl*1 (m+l rl*1))))))))))

(defun lognx2 (nn dn pl rl)
  (do ((pl pl (cdr pl))
       (rl rl (cdr rl))
       (ans ()))
      ((or (null pl)
	   (null rl))  ans)
    (setq ans (cons (m* dn (car rl) (m^ `((%plog) ,(car pl)) nn))
		    ans))))

(defun logcpj (n d i)
  (setq n (append
	   (cond (plm* (list (mul* (m*t '$%i %pi2)
				   (m+l
				    (residue (m* (m^ `((%plog) ,var) i)
						 n)
					     d
					     plm*))))))
	   (lognx2 i (m*t '$%i %pi2) pl* rl*)
	   (lognx2 i %p%i pl*1 rl*1)))
  (cond ((null n) 0)
	(t (simplify (m+l n)))))


;;should replace these references to *i* and *j* to symbol-value arrays.
;;here and in SUMI, and LOGCPI.  These are the only references in this file.
;;I did change I to *I* 

#-cl	      ;in case other lisps don't understand internal declares.
(declare-top(special *i* *j*))

(defun log*rat (n d m)
  (prog (leadcoef factors c plm* pl* rl* pl*1 rl*1 rlm*)
     (declare (special *i* *j*))
     ;;	(ARRAY *I* T (M+ 1 M))
     ;;	(ARRAY *J* T (M+ 1 M))
     (setq *i* (*array nil t (m+ 1 m)))
     (setq *j* (*array nil t (m+ 1 m)))
     (setq c 0.)
     (store (aref *j* c) 0.)
     (do ((c 0. (m+ 1 c)))
	 ((equal c m)
	  (return (logcpi n d m)))
       (store (aref *i* c) (logcpi n d c))
       (store (aref *j* c) (logcpj n factors c)))))

(defun logcpi (n d c)
  (declare (special *j*))
  (cond ((equal c 0.)
	 (logcpi0 n d))
	(t (m* '((rat) 1. 2.)
	       (m+ (aref *j* c) (m* -1. (sumi c)))))))

(defun sumi (c)
  (declare (special *i*))
  (do ((k 1. (m+ 1 k))
       (ans ()))
      ((equal k c)
       (m+l ans))
    (setq ans (cons (mul* ($makegamma `((%binomial) ,c ,k))
			  (m^t '$%pi k)
			  (m^t '$%i k)
			  (aref *i* (m+ c (m- k))))
		    ans))))

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
			     ($makegamma `((%binomial) ,(m+ -1. m (m- povern))
					   ,(m+t -1. m)))
			     `((mabs) ,(m^ a (m+ povern (m- m)))))
			 (m* (m^ b povern)
			     n
			     `((%sin) ,(m*t '$%pi povern)))))))))))


;;Makes a new poly such that np(x)-np(x+2*%i*%pi)=p(x).
;;Constructs general POLY of degree one higher than P with
;;arbitrary coeff. and then solves for coeffs by equating like powers
;;of the varibale of integration.
;;Can probably be made simpler now.

(defun makpoly (p)
  (let ((n (deg p))  (ans ())  (varlist ())  (gp ())  (cl ())  (zz ()))
    (setq ans (genpoly (m+ 1 n))) ;Make poly with gensyms of 1 higher deg.
    (setq cl (cdr ans))			;Coefficient list
    (setq varlist (append cl (list var))) ;Make VAR most important.
    (setq gp (car ans))		 ;This is the poly with gensym coeffs.
;;;Now, poly(x)-poly(x+2*%i*%pi)=p(x), P is the original poly.
    (setq ans (m+ gp (subin (m+t (m*t '$%i %pi2) var) (m- gp)) (m- p)))
    (newvar ans)
    (setq ans (ratrep* ans))	       ;Rational rep with VAR leading.
    (setq zz (coefsolve n cl (cond ((not (eq (caadr ans) ;What is Lead Var.
					     (genfind (car ans) var)))
				    (list 0 (cadr ans))) ;No VAR in ans.
				   ((cdadr ans))))) ;The real Poly.
    (if (or (null zz) (null gp)) 
	-1
	($substitute zz gp))))	       ;Substitute Values for gensyms.

(defun coefsolve (n cl e)    
  (do (($breakup)
       (eql (ncons (pdis (pterm e n))) (cons (pdis (pterm e m)) eql))
       (m (m+ n -1) (m+ m -1)))
      ((signp l m) (solvex eql cl nil nil))))

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
(defun rectzto%pi2 (p pe d)
  (prog (dp n pl a b c denom-exponential)
     (if (not (and (setq denom-exponential (catch 'pin%ex (pin%ex d)))
		   (%e-integer-coeff pe)
		   (%e-integer-coeff d)))
	 (return ()))
     (setq n (m* (cond ((null p) -1.)
		       (t ($expand (m*t '$%i %pi2 (makpoly p)))))
		 pe))
     (let ((var 'z*)
	   (leadcoef ()))
       ;; Find the poles of the denominator.  denom-exponential is the
       ;; denominator of R(x).
       ;;
       ;; It seems as if polelist returns a list of several items.
       ;; The first element is a list consisting of the pole and (z -
       ;; pole).  We don't care about this, so we take the rest of the
       ;; result.  I think the second element of the list is an alist
       ;; consisting of the pole and it's multiplicity.  I don't know
       ;; what the rest of the list is.
       (setq pl (cdr (polelist denom-exponential
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
	    (return nil))
	   ((or (cadr pl)
		(caddr pl))
	    (setq dp (sdiff d var))))
     ;; Not sure what this does.
     (cond ((cadr pl)
	    (setq b (mapcar #'log-imag-0-2%pi (cadr pl)))
	    (setq b (res1 n dp b))
	    (setq b (m+l b)))
	   (t (setq b 0.)))
     ;; Not sure what this does either.
     (cond ((caddr pl)
	    (let ((temp (mapcar #'log-imag-0-2%pi (caddr pl))))
	      (setq c (append temp (mapcar #'(lambda (j) 
					       (m+ (m*t '$%i %pi2) j))
					   temp)))
	      (setq c (res1 n dp c))
	      (setq c (m+l c))))
	   (t (setq c 0.)))
     (cond ((car pl)
	    ;; We have the poles of deonom-exponential, so we need to
	    ;; convert them to the actual pole values for R(exp(x)),
	    ;; by taking the log of the value of poles.
	    (let ((poles (mapcar #'(lambda (p)
				     (log-imag-0-2%pi (car p)))
				 (car pl)))
		  (exp (m// n (subst (m^t '$%e var) 'z* denom-exponential))))
	      ;; Compute the residues at all of these poles and sum
	      ;; them up.
	      (setq a (mapcar #'(lambda (j) 
				  ($residue exp var j))
			      poles))
	      (setq a (m+l a))))
	   (t (setq a 0.)))
     (return (sratsimp (m+ a b (m* '((rat) 1. 2.) c))))))

(defun genpoly (i)
  (do ((i i (m+ i -1))
       (c (gensym) (gensym))
       (cl ())
       (ans ()))
      ((zerop i)
       (cons (m+l ans) cl))
    (setq ans (cons (m* c (m^t var i)) ans))
    (setq cl (cons c cl))))

(declare-top(special *failflag *lhflag lhv *indicator cnt *disconflag)) 

(defun %e-integer-coeff (exp)
  (cond ((mapatom exp) t)
	((and (mexptp exp)
	      (eq (cadr exp) '$%e)
	      (eq (ask-integer ($coeff (caddr exp) var) '$integer)
		  '$yes))  t)
	(t (andmapc '%e-integer-coeff (cdr exp)))))

(defun wlinearpoly (e var)
  (cond ((and (setq e (polyinx e var t))
	      (equal (deg e) 1.))
	 (subin 1. e)))) 

(declare-top(special e $exponentialize))

(defun pin%ex (exp)
  (pin%ex0 (cond ((notinvolve exp '(%sinh %cosh %tanh)) exp)
		 (t (setq exp (let (($exponentialize t))
				($expand exp)))))))

(defun pin%ex0 (e)
  (cond ((not (among var e))  e)
	((atom e)  (throw 'pin%ex nil))
	((and (mexptp e)
	      (eq (cadr e)  '$%e))
	 (cond ((eq (caddr e) var)  'z*)
	       ((let ((linterm (wlinearpoly (caddr e) var)))
		  (and linterm
		       (m* (subin 0 e) (m^t 'z* linterm)))))
	       (t (throw 'pin%ex nil))))
	((mtimesp e)  (m*l (mapcar #'pin%ex0 (cdr e))))
	((mplusp e)  (m+l (mapcar #'pin%ex0 (cdr e))))
	(t (throw 'pin%ex nil))))

(declare-top (unspecial e)) 

(defun p*pin%ex (nd*)
  (setq nd* ($factor nd*))
  (cond ((polyinx nd* var nil) (setq p* (cons nd* p*)) t)
	((catch 'pin%ex (pin%ex nd*)) (setq pe* (cons nd* pe*)) t)
	((mtimesp nd*)
	 (andmapcar #'p*pin%ex (cdr nd*)))))

(defun findsub (p)
  (cond ((findp p) nil)
	((setq nd* (bx**n p)) 
	 (m^t var (car nd*)))
	((setq p (bx**n+a p))
	 (m* (caddr p) (m^t var (cadr p)))))) 

(defun funclogor%e (e)
  (prog (ans arg nvar r) 
     (cond ((or (ratp e var)
		(involve e '(%sin %cos %tan))
		(not (setq arg (xor (and (setq arg (involve e '(%log)))
					 (setq r '%log))
				    (%einvolve e)))))
	    (return nil)))
     ag (setq nvar (cond ((eq r '%log) `((%log) ,arg))
			 (t (m^t '$%e arg))))
     (setq ans (maxima-substitute (m^t 'yx -1.) (m^t nvar -1.) (maxima-substitute 'yx nvar e)))
     (cond ((not (among var ans))  (return (list (subst var 'yx ans) nvar)))
	   ((and (null r) 
		 (setq arg (findsub arg)))
	    (go ag)))))

(defun dintbypart (u v a b)
;;;SINCE ONLY CALLED FROM DINTLOG TO get RID OF LOGS - IF LOG REMAINS, QUIT
  (let ((ad (antideriv v)))
    (cond ((or (null ad)
	       (involve ad '(%log)))
	   nil)
	  (t (let ((p1 (m* u ad))
		   (p2 (m* ad (sdiff u var))))
	       (let ((p1-part1 (get-limit p1 var b '$minus))
		     (p1-part2 (get-limit p1 var a '$plus)))
		 (cond ((or (null p1-part1)
			    (null p1-part2))
			nil)
		       (t (let ((p2 (let ((*def2* t))
				      (defint p2 var a b))))
			    (cond (p2 (add* p1-part1 
					    (m- p1-part2)
					    (m- p2)))
				  (t nil)))))))))))

(defun dintexp (exp arg &aux ans)
  (let ((dintexp-recur t))		;recursion stopper
    (cond ((and (sinintp exp var)     ;To be moved higher in the code.
		(setq ans (antideriv exp))
		(setq ans (intsubs ans ll ul))))
	  ((setq ans (funclogor%e exp))
	   (cond ((and (equal ll 0.) (eq ul '$inf))
		  (setq exp (subin (m+t 1. arg) (car ans)))
		  (setq ans (m+t -1. (cadr ans))))
		 (t (setq exp (car ans))
		    (setq ans (cadr ans))))
	   (intcv ans t nil)))))

(defun dintlog (exp arg)
  (let ((dintlog-recur (f1+ dintlog-recur))) ;recursion stopper
    (prog (ans d) 
       (cond ((and (eq ul '$inf)
		   (equal ll 0.)
		   (eq arg var)
		   (equal 1. ($ratsimp (m// exp (m* (m- (subin (m^t var -1.)
							       exp))
						    (m^t var -2.))))))
	      (return 0.))
	     ((setq ans (antideriv exp))
	      (return (intsubs ans ll ul)))
	     ((setq ans (logx1 exp ll ul))
	      (return ans)))
       (setq ans (m// exp `((%log) ,arg)))
       (cond ((involve ans '(%log)) (return nil))
	     ((and (eq arg var)
		   (equal 0. (no-err-sub 0. ans))
		   (setq d (let ((*def2* t))
			     (defint (m* ans (m^t var '*z*))
				 var ll ul))))
	      (return (derivat '*z* 1. d 0.)))
	     ((setq ans (dintbypart `((%log) ,arg) ans ll ul))
	      (return ans))))))

(defun derivat (var n e pt) (subin pt (apply '$diff (list e var n)))) 

;;; GGR and friends

;; MAYBPC returns (COEF EXPO CONST)
;;
;; This basically picks off b*x^n+a and returns the list
;; (b n a).  It may also set the global *zd*.
(defun maybpc (e var)
  (declare (special *zd*))
  (cond (mtoinf* (throw 'ggrm (linpower0 e var)))
	((and (not mtoinf*)
	      (null (setq e (bx**n+a e)))) ;bx**n+a --> (a n b) or nil.
	 nil)				;with var being x.
	;; At this point, e is of the form (a n b)
	((and (among '$%i (caddr e))
	      (zerop1 ($realpart (caddr e)))
	      (setq zn ($imagpart (caddr e)))
	      (eq ($asksign (cadr e)) '$pos))
	 ;; If we're here, b is complex, and n > 0.  zn = imagpart(b).
	 ;;
	 ;; Set var to the same sign as zn.
	 (cond ((eq ($asksign zn) '$neg)
		(setq var -1.)
		(setq zn (m- zn)))
	       (t (setq var 1.)))
	 ;; zd = exp(var*%i*%pi*(1+nd)/(2*n). (ZD is special!)
	 (setq *zd* (m^t '$%e (m// (mul* var '$%i '$%pi (m+t 1. nd*))
				   (m*t 2. (cadr e)))))
	 ;; Return zn, n, a.
	 `(,(caddr e) ,(cadr e) ,(car e)))
	((and (or (eq (setq var ($asksign ($realpart (caddr e)))) '$neg)
		  (equal var '$zero))
	      (equal ($imagpart (cadr e)) 0)
	      (ratgreaterp (cadr e) 0.))
	 ;; We're here if realpart(b) <= 0, and n >= 0.  Then return -b, n, a.
	 `(,(caddr e) ,(cadr e) ,(car e)))))

;; Integrate x^m*exp(b*x^n+a), with realpart(m) > -1.
;;
;; See Wang, pp. 84-85.
;;
;; I believe the formula Wang gives is incorrect.  The derivation is
;; correct except for the last step.
;;
;; Let J = integrate(x^m*exp(%i*k*x^n),x,0,inf), with k < 0.
;;
;; Then J = exp(-%pi*%i*(m+1)/(2*n))*integrate(R^m*exp(k*R^n),R,0,inf)
;;
;; Wang seems to say this last integral is gamma(s/n/(-k)^s) where s =
;; (m+1)/n.  But that seems wrong.  If we use the substitution x =
;; (y/k)^(1/n), we end up with the result:
;;
;;   integrate(y^((m+1)/n-1)*exp(-y),y,0,inf)/k^((m+1)/n)/n.
;;
;; or gamma((m+1)/n)/k^((m+1)/n)/n.
;;
#+nil
(defun ggr (e ind)
  (prog (c *zd* zn nn* dn* nd* dosimp $%emode)
     (declare (special *zd*))
     (setq nd* 0.)
     (cond (ind (setq e ($expand e))
		(cond ((and (mplusp e)
			    (let ((*nodiverg t))
			      (setq e (catch 'divergent
					(andmapcar
					 #'(lambda (j) 
					     (ggr j nil))
					 (cdr e))))))
		       (cond ((eq e 'divergent) nil)
			     (t (return (sratsimp (cons '(mplus) e)))))))))
     (setq e (rmconst1 e))
     (setq c (car e))
     (setq e (cdr e))
     (cond ((setq e (ggr1 e var))
	    ;; e = (m b n a).  I think we want to compute
	    ;; gamma((m+1)/n)/k^((m+1)/n)/n.
	    ;;
	    ;; FIXME: If n > m + 1, the integral converges.  We need
	    ;; to check for this.
	    (progn
	      (format t "e = ~A~%" e)
	      (format t "asksign ~A = ~A~%"
		      (sub (third e) (add ($realpart (first e)) 1))
		      ($asksign (sub (third e) (add ($realpart (first e)) 1)))))
	    
	    (setq e (apply #'gamma1 e))
	    ;; NOTE: *zd* (Ick!) is special and might be set by maybpc.
	    (when *zd*
	      ;; FIXME: Why do we set %emode here?  Shouldn't we just
	      ;; bind it?  And why do we want it bound to T anyway?
	      ;; Shouldn't the user control that?  The same goes for
	      ;; dosimp.
	      ;;(setq $%emode t)
	      (setq dosimp t)
	      (setq e (m* *zd* e)))))
     (cond (e (return (m* c e))))))

(defun ggr (e ind)
  (prog (c *zd* zn nn* dn* nd* dosimp $%emode)
     (declare (special *zd*))
     (setq nd* 0.)
     (cond (ind (setq e ($expand e))
		(cond ((and (mplusp e)
			    (let ((*nodiverg t))
			      (setq e (catch 'divergent
					(andmapcar
					 #'(lambda (j) 
					     (ggr j nil))
					 (cdr e))))))
		       (cond ((eq e 'divergent) nil)
			     (t (return (sratsimp (cons '(mplus) e)))))))))
     (setq e (rmconst1 e))
     (setq c (car e))
     (setq e (cdr e))
     (cond ((setq e (ggr1 e var))
	    ;; e = (m b n a).  I think we want to compute
	    ;; gamma((m+1)/n)/k^((m+1)/n)/n.
	    ;;
	    ;; FIXME: If n > m + 1, the integral converges.  We need
	    ;; to check for this.
	    (destructuring-bind (m b n a)
		e
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
	      ;; NOTE: *zd* (Ick!) is special and might be set by maybpc.
	      (when *zd*
		;; FIXME: Why do we set %emode here?  Shouldn't we just
		;; bind it?  And why do we want it bound to T anyway?
		;; Shouldn't the user control that?  The same goes for
		;; dosimp.
		;;(setq $%emode t)
		(setq dosimp t)
		(setq e (m* *zd* e))))))
     (cond (e (return (m* c e))))))


;; Match x^m*exp(b*x^n+a).  If it does, return (list m b n a).
(defun ggr1 (e var) 
  (cond ((atom e) nil)
	((and (mexptp e)
	      (eq (cadr e) '$%e))
	 ;; We're looking at something like exp(f(var)).  See if it's
	 ;; of the form b*x^n+a, and return (list 0 b n a).  (The 0 is
	 ;; so we can graft something onto it if needed.)
	 (cond ((setq e (maybpc (caddr e) var))
		(cons 0. e))))
	((and (mtimesp e)
	      ;; E should be the product of exactly 2 terms
	      (null (cdddr e))
	      ;; Check to see if one of the terms is of the form
	      ;; var^p.  If so, make sure the realpart of p > -1.  If
	      ;; so, check the other term has the right form via
	      ;; another call to ggr1.
	      (or (and (setq dn* (xtorterm (cadr e) var))
		       (ratgreaterp (setq nd* ($realpart dn*))
				    -1.)
		       (setq nn* (ggr1 (caddr e) var)))
		  (and (setq dn* (xtorterm (caddr e) var))
		       (ratgreaterp (setq nd* ($realpart dn*))
				    -1.)
		       (setq nn* (ggr1 (cadr e) var)))))
	 ;; Both terms have the right form and nn* contains the arg of
	 ;; the exponential term.  Put dn* as the car of nn*.  The
	 ;; result is something like (m b n a) when we have the
	 ;; expression x^m*exp(b*x^n+a).
	 (rplaca nn* dn*))))


;; Match b*x^n+a.  If a match is found, return the list (a n b).
;; Otherwise, return NIL
(defun bx**n+a (e)
  (cond ((eq e var) 
	 (list 0. 1. 1.))
	((or (atom e) 
	     (mnump e)) ())
	(t (let ((a (no-err-sub 0. e)))
	     (cond ((null a)  ())
		   (t (setq e (m+ e (m*t -1. a)))
		      (cond ((setq e (bx**n e))
			     (cons a e))
			    (t ()))))))))

;; Match b*x^n.  Return the list (n b) if found or NIL if not.
(defun bx**n (e)
  (let ((n ()))
    (and (setq n (xexponget e var))
	 (not (among var
		     (setq e (let (($maxposex 1)
				   ($maxnegex 1))
			       ($expand (m// e (m^t var n)))))))
	 (list n e))))

(defun xexponget (e nn*)
  (cond ((atom e) (cond ((eq e var) 1.)))
	((mnump e) nil)
	((and (mexptp e)
	      (eq (cadr e) nn*)
	      (not (among nn* (caddr e))))
	 (caddr e))
	(t (ormapc #'(lambda (j)
		       (xexponget j nn*))
		   (cdr e)))))


;;; given (b*x^n+a)^m returns (m a n b)
(defun bxm (e ind)
  (let (m r)
    (cond ((or (atom e)
	       (mnump e)
	       (involve e '(%log %sin %cos %tan))
	       (%einvolve e))  nil)
	  ((mtimesp e)  nil)
	  ((mexptp e)  (cond ((among var (caddr e))  nil)
			     ((setq r (bx**n+a (cadr e))) 
			      (cons (caddr e) r))))
	  ((setq r (bx**n+a e))  (cons 1. r))
	  ((not (null ind))
;;;Catches Unfactored forms.
	   (setq m (m// (sdiff e var) e))
	   (numden m)
	   (setq m nn*)
	   (setq r dn*)
	   (cond 
	     ((and (setq r (bx**n+a ($ratsimp r)))
		   (not (among var (setq m (m// m (m* (cadr r) (caddr r)
						      (m^t var (m+t -1.
								    (cadr r))))))))
		   (setq e (m// (subin 0. e) (m^t (car r) m))))
	      (cond ((equal e 1.)
		     (cons m r))
		    (t (setq e (m^ e (m// 1. m)))
		       (list m (m* e (car r)) (cadr r) 
			     (m* e (caddr r))))))))
	  (t ()))))

;;;Is E = VAR raised to some power? If so return power or 0.
(defun findp (e) 
  (cond ((not (among var e)) 0.)
	(t (xtorterm e var))))

(defun xtorterm (e var1)
;;;Is E = VAR1 raised to some power? If so return power.
  (cond ((alike1 e var1) 1.)
	((atom e) nil)
	((and (mexptp e)
	      (alike1 (cadr e) var1)
	      (not (among var (caddr e))))
	 (caddr e)))) 

(defun tbf (l)
  (m^ (m* (m^ (caddr l) '((rat) 1. 2.))
	  (m+ (cadr l) (m^ (m* (car l) (caddr l))
			   '((rat) 1. 2.))))
      -1.))

(defun radbyterm (d l)
  (do ((l l (cdr l))
       (ans ()))
      ((null l)
       (m+l ans))
    (destructuring-let (((const . integrand) (rmconst1 (car l))))
      (setq ans (cons (m* const (dintrad0 integrand d))
		      ans)))))

(defun sqdtc (e ind)
  (prog (a b c varlist) 
     (setq varlist (list var))
     (newvar e)
     (setq e (cdadr (ratrep* e)))
     (setq c (pdis (pterm e 0.)))
     (setq b (m*t (m//t 1. 2.) (pdis (pterm e 1.))))
     (setq a (pdis (pterm e 2.)))
     (cond ((and (eq ($asksign (m+ b (m^ (m* a c)
					 '((rat) 1. 2.))))
		     '$pos)
		 (or (and ind
			  (not (eq ($asksign a) '$neg))
			  (eq ($asksign c) '$pos))
		     (and (eq ($asksign a) '$pos)
			  (not (eq ($asksign c) '$neg)))))
	    (return (list a b c)))))) 

(defun difap1 (e pwr var m pt)
  (m// (mul* (cond ((eq (ask-integer m '$even) '$yes)
		    1.)
		   (t -1.))
	     `((%gamma) ,pwr)
	     (derivat var m e pt))
       `((%gamma) ,(m+ pwr m)))) 

(defun sqrtinvolve (e)
  (cond ((atom e) nil)
	((mnump e) nil)
	((and (mexptp e) 
	      (and (mnump (caddr e))
		   (not (numberp (caddr e)))
		   (equal (caddr (caddr e)) 2.))
	      (among var (cadr e)))
	 (cadr e))
	(t (ormapc #'sqrtinvolve (cdr e)))))

(defun bydif (r s d)
  (let ((b 1)  p)
    (setq d (m+ (m*t '*z* var) d))
    (cond ((or (zerop1 (setq p (m+ s (m*t -1. r))))
	       (and (zerop1 (m+ 1. p))
		    (setq b var)))
	   (difap1 (dintrad0 b (m^ d '((rat) 3. 2.)))
		   '((rat) 3. 2.) '*z* r 0.))
	  ((eq ($asksign p) '$pos)
	   (difap1 (difap1 (dintrad0 1. (m^ (m+t 'z** d)
					    '((rat) 3. 2.)))
			   '((rat) 3. 2.) '*z* r 0.)
		   '((rat) 3. 2.) 'z** p 0.)))))

(defun dintrad0 (n d)
  (let (l r s) 
    (cond ((and (mexptp d) 
		(equal (deg (cadr d)) 2.))
	   (cond ((alike1 (caddr d) '((rat) 3. 2.))
		  (cond ((and (equal n 1.)
			      (setq l (sqdtc (cadr d) t)))
			 (tbf l))
			((and (eq n var)
			      (setq l (sqdtc (cadr d) nil)))
			 (tbf (reverse l)))))
		 ((and (setq r (findp n))
		       (or (eq ($asksign (m+ -1. (m-  r) (m*t 2.
							      (caddr d))))
			       '$pos)
			   (diverg))
		       (setq s (m+ '((rat) -3. 2.) (caddr d)))
		       (eq ($asksign s) '$pos)
		       (eq (ask-integer s '$integer) '$yes))
		  (bydif r s (cadr d)))
		 ((polyinx n var nil)
		  (radbyterm d (cdr n))))))))


;;;Looks at the IMAGINARY part of a log and puts it in the interval 0 2*%pi.
(defun log-imag-0-2%pi (x)
  (let ((plog (simplify `((%plog) ,x))))
    (cond ((not (free plog '%plog))
	   (subst '%log '%plog plog))
	  (t (destructuring-let (((real . imag) (trisplit plog)))
	       (cond ((eq ($asksign imag) '$neg)
		      (setq imag (m+ imag %pi2)))
		     ((eq ($asksign (m- imag %pi2)) '$pos)
		      (setq imag (m- imag %pi2)))
		     (t t))
	       (m+ real (m* '$%i imag)))))))

	    
;;; Temporary fix for a lacking in taylor, which loses with %i in denom.
;;; Besides doesn't seem like a bad thing to do in general.
#+nil
(defun %i-out-of-denom (exp)
  (let ((denom ($denom exp))
	(den-conj nil))
    (cond ((among '$%i denom)
	   (setq den-conj (maxima-substitute (m- '$%i) '$%i denom))
	   (setq exp (m* den-conj ($ratsimp (m// exp den-conj))))
	   (setq exp (simplify ($multthru  (sratsimp exp)))))
	  (t exp))))

(defun %i-out-of-denom (exp)
  (let ((denom ($denom exp)))
    (cond ((among '$%i denom)
	   ;; Multiply the denominator by it's conjugate to get rid of
	   ;; %i.
	   (let* ((den-conj (maxima-substitute (m- '$%i) '$%i denom))
		  (num ($num exp))
		  (new-denom ($ratsimp (m* denom den-conj))))
	     ;; If the new denominator still contains %i, just give
	     ;; up.  Otherwise, multiply the numerator by the
	     ;; conjugate and divide by the new denominator.
	     (if (among '$%i new-denom)
		 exp
		 (setq exp (m// (m* num den-conj) new-denom)))))
	  (t exp))))

;;; LL and UL must be real otherwise this routine return $UNKNOWN.
;;; Returns $no $unknown or a list of poles in the interval (ll ul)
;;; for exp w.r.t. var.
;;; Form of list ((pole . multiplicity) (pole1 . multiplicity) ....)
(defun poles-in-interval (exp var ll ul)
  (let* ((denom (cond ((mplusp exp)
		       ($denom (sratsimp exp)))
		      ((and (mexptp exp)
			    (free (caddr exp) var)
			    (eq ($asksign (caddr exp)) '$neg))
		       (m^ (cadr exp) (m- (caddr exp))))
		      (t ($denom exp))))
	 (roots (real-roots denom var))
	 (ll-pole (limit-pole exp var ll '$plus))
	 (ul-pole (limit-pole exp var ul '$minus)))
    (cond ((or (eq roots '$failure)
	       (null ll-pole)
	       (null ul-pole))   '$unknown)
	  ((and (eq roots '$no)
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
			(let ((lim-ans (is-a-pole exp soltn)))
			  (cond ((null lim-ans)
				 (return '$unknown))
				((equal lim-ans 0)
				 '$no)
				(t (push (car dummy)
					 pole-list))))))))))))


;;;Returns $YES if there is no pole and $NO if there is one.
(defun limit-pole (exp var limit direction)
  (let ((ans (cond ((memq limit '($minf $inf))
		    (cond ((eq (special-convergent-formp exp limit) '$yes)
			   '$no)
			  (t (get-limit (m* exp var) var limit direction))))
		   (t '$no))))
    (cond ((eq ans '$no)   '$no)
	  ((null ans)   nil)
	  ((eq ans '$und) '$no)
	  ((equal ans 0.)   '$no)
	  (t '$yes))))

;;;Takes care of forms that the ratio test fails on.
(defun special-convergent-formp (exp limit)
  (cond ((not (oscip exp))  '$no)
	((or (eq (sc-converg-form exp limit) '$yes)
	     (eq (exp-converg-form exp limit) '$yes))
	 '$yes)
	(t  '$no)))

(defun exp-converg-form (exp limit)
  (let (exparg)
    (setq exparg (%einvolve exp))
    (cond ((or (null exparg)
	       (freeof '$%i exparg))
	   '$no)
	  (t (cond
	       ((and (freeof '$%i 
			     (%einvolve 
			      (setq exp 
				    (sratsimp (m// exp (m^t '$%e exparg))))))
		     (equal (get-limit exp var limit)  0))
	        '$yes)
	       (t '$no))))))

(defun sc-converg-form (exp limit)       
  (prog (scarg trigpow)
     (setq exp ($expand exp))
     (setq scarg (involve (sin-sq-cos-sq-sub exp) '(%sin %cos)))
     (cond ((null scarg) (return '$no))
	   ((and (polyinx scarg var ())
		 (eq ($asksign (m- ($hipow scarg var) 1)) '$pos)) (return '$yes))
	   ((not (freeof var (sdiff scarg var)))
	    (return '$no))
	   ((and (setq trigpow ($hipow exp `((%sin) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%sin) ,scarg)) var limit)
			0))
	    (return '$yes))
	   ((and (setq trigpow ($hipow exp `((%cos) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%cos) ,scarg)) var limit)
			0))
	    (return '$yes))
	   (t (return '$no)))))

(defun is-a-pole (exp soltn)
  (get-limit ($radcan 
	      (m* (maxima-substitute (m+ 'epsilon soltn) var exp)
		  'epsilon))
	     'epsilon 0 '$plus))

(defun in-interval (place ll ul)
  ;; real values for ll and ul; place can be imaginary.
  (let ((order (ask-greateq ul ll)))
    (cond ((eq order '$yes))
	  ((eq order '$no) (let ((temp ul)) (setq ul ll ll temp)))
	  (t (merror "Incorrect limits given to `defint':~%~M"
		     (list '(mlist simp) ll ul)))))
  (if (not (equal ($imagpart place) 0))
      '$no
      (let ((lesseq-ul (ask-greateq ul place))
	    (greateq-ll (ask-greateq place ll)))
	(if (and (eq lesseq-ul '$yes) (eq greateq-ll '$yes)) '$yes '$no))))

(defun real-roots (exp var)
  (let (($solvetrigwarn (cond (defintdebug t) ;Rest of the code for
			      (t ())))	;TRIGS in denom needed.
	($solveradcan (cond ((or (among '$%i exp)
				 (among '$%e exp)) t)
			    (t nil)))
	*roots *failures)		;special vars for solve.
    (cond ((not (among var exp))   '$no)
	  (t (solve exp var 1)
	     (cond (*failures '$failure)
		   (t (do ((dummy *roots (cddr dummy))
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
					   rootlist)))))))))))

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
	       (cond ((memq ans '($zero $pos))
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
