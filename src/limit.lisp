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

(macsyma-module limit)


;;;	**************************************************************
;;;	**							    **
;;;	**			   LIMIT PACKAGE		    **
;;;	**							    **
;;;	**************************************************************

;;; I believe a large portion of this file is described in Paul
;;; Wang's thesis, "Evaluation of Definite Integrals by Symbolic
;;; Manipulation," MIT/LCS/TR-92, Oct. 1971.  This can be found at
;;; https://web.archive.org/web/20191019131847/https://apps.dtic.mil/dtic/tr/fulltext/u2/732005.pdf


;;; TOP LEVEL FUNCTION(S): $LIMIT $LDEFINT

(declare-top (special origval
		      *indicator numer denom exp var val
		      taylored logcombed
		      $exponentialize lhp? lhcount
		      loginprod? a context limit-assumptions
		      limit-top integer-info old-integer-info))

(defconstant +behavior-count+ 4)
(defvar *behavior-count-now*)
(defvar *getsignl-asksign-ok* nil)

(load-macsyma-macros rzmac)

(defmvar simplimplus-problems ()
  "A list of all problems in the stack of recursive calls to simplimplus.")

(defmvar limit-answers ()
  "An association list for storing limit answers.")

(defmvar limit-using-taylor ()
  "Is the current limit computation using taylor expansion?")

(defmvar preserve-direction () "Makes `limit' return Direction info.")

(unless (boundp 'integer-info) (setq integer-info ()))

;; For limits toward infinity for the gruntz code, we assume that the limit
;; variable exceeds *large-positive-number*. This value matches the value
;; that the limit code uses for the same purpose.
(defmvar *large-positive-number* (expt 10 8))

;; Don't ask sign questions about $ind.
(putprop '$ind t 'internal)

;; This should be made to give more information about the error.
;;(DEFun DISCONT ()
;;       (cond (errorsw (throw 'errorsw t))
;;	     (t (merror "Discontinuity Encountered"))))

;;(DEFUN PUTLIMVAL (E V)
;;  (let ((exp (cons '(%limit) (list e var val))))
;;    (cond ((not (assolike exp limit-answers))
;;	   (setq limit-answers (cons (cons exp v) limit-answers))
;;	   v)
;;	  (t ()))))

(defun putlimval (e v &aux exp)
  (setq exp `((%limit) ,e ,var ,val))
  (unless (assolike exp limit-answers)
    (push (cons exp v) limit-answers))
  v)

(defun getlimval (e)
  (let ((exp (cons '(%limit) (list e var val))))
    (assolike exp limit-answers)))

(defmacro limit-catch (exp var val)
  `(let ((errorsw t))
    (let ((ans (catch 'errorsw
		 (catch 'limit (limit ,exp ,var ,val 'think)))))
      (if (or (null ans) (eq ans t))
	  ()
	  ans))))

(defmfun $limit (&rest args)
  (let ((first-try (apply #'toplevel-$limit args)))
    (if (and (consp first-try) (eq (caar first-try) '%limit))
      (let ((*getsignl-asksign-ok* t))
        (apply #'toplevel-$limit args))

      ;; The function toplevel-$limit sets $numer, $%enumer, %emode, and 
	  ;; $%e_to_numlog to false. When any of these option variables are true,
	  ;; we resimplify the limit in the current context. 
      (progn
	 	 (when (or $numer $%enumer $%emode $%e_to_numlog)
	  		(setq first-try (resimplify first-try)))
      	 first-try))))

(defun toplevel-$limit (&rest args)
  (let ((limit-assumptions ())
	(old-integer-info ())
	($keepfloat t)
	($numer nil)
	($%enumer nil)
	($%emode t) 
	($%e_to_numlog nil)
	(limit-top t))
    (declare (special limit-assumptions old-integer-info
		      limit-top))
    (unless limitp
      (setq old-integer-info integer-info)
      (setq integer-info ()))

    (unwind-protect
	 (let ((exp1 ()) (lhcount $lhospitallim) (*behavior-count-now* 0)
	       (exp ()) (var ()) (val ()) (dr ())
	       (*indicator ()) (taylored ()) (origval ())
	       (logcombed ()) (lhp? ())
	       (varlist ()) (ans ()) (genvar ()) (loginprod? ())
	       (limit-answers ()) (limitp t) (simplimplus-problems ())
	       (lenargs (length args))
	       (genfoo ()))
	   (declare (special lhcount *behavior-count-now* exp var val *indicator
			     taylored origval logcombed lhp?
			     varlist genvar loginprod? limitp))
	   (prog ()
	      (unless (or (= lenargs 3) (= lenargs 4) (= lenargs 1))
		(wna-err '$limit))
	      ;; Is it a LIST of Things?
	      (when (setq ans (apply #'limit-list args))
		(return ans))
	      (setq exp1 (specrepcheck (first args)))
              (when (and (atom exp1)
                         (member exp1 '(nil t)))
                ;; The expression is 'T or 'NIL. Return immediately.
                (return exp1))
	      (cond ((= lenargs 1)
		     (setq var (setq genfoo (gensym)) ; Use a gensym. Not foo.
		           val 0))
		    (t
		     (setq var (second args))
		       (when ($constantp var)
			 (merror (intl:gettext "limit: second argument must be a variable, not a constant; found: ~M") var))
		       (unless (or ($subvarp var) (atom var))
			 (merror (intl:gettext "limit: variable must be a symbol or subscripted symbol; found: ~M") var))
		       (setq val (infsimp (third args)))
		       ;; infsimp converts -inf to minf.  it also converts -infinity to
		       ;; infinity, although perhaps this should generate the error below.
		       (when (and (not (atom val))
				  (some #'(lambda (x) (not (freeof x val)))
					*infinities*))
			 (merror (intl:gettext "limit: third argument must be a finite value or one of: inf, minf, infinity; found: ~M") val))
		       (when (eq val '$zeroa) (setq dr '$plus))
		       (when (eq val '$zerob) (setq dr '$minus))))
	      (cond ((= lenargs 4)
		     (unless (member (fourth args) '($plus $minus) :test #'eq)
		       (merror (intl:gettext "limit: direction must be either 'plus' or 'minus'; found: ~M") (fourth args)))
		     (setq dr (fourth args))))
	      (if (and (atom var) (not (among var val)))
		  (setq exp exp1)
		  (let ((realvar var)) ;; Var is funny so make it a gensym.
		    (setq var (gensym))
		    (setq exp (maxima-substitute var realvar exp1))
		    (putprop var realvar 'limitsub)))
	      (unless (or $limsubst (eq var genfoo))
		(when (limunknown exp)
		  (return `((%limit) ,@(cons exp1 (cdr args))))))
	      (setq varlist (ncons var) genvar nil origval val)
	      ;; Transform limits to minf to limits to inf by
	      ;; replacing var with -var everywhere.
	      (when (eq val '$minf)
		(setq val '$inf
		      origval '$inf
		      exp (subin (m* -1 var) exp)))
              
	      ;; Transform the limit value.
	      (unless (infinityp val)
		(unless (zerop2 val)
		  (let ((*atp* t) (realvar var))
		    ;; *atp* prevents substitution from applying to vars 
		    ;; bound by %sum, %product, %integrate, %limit
		    (setq var (gensym))
		    (putprop var t 'internal)
		    (setq exp (derivative-subst exp val var realvar))
		    (setq exp (maxima-substitute (m+ val var) realvar exp))))
		(setq val (cond ((eq dr '$plus) '$zeroa)
				((eq dr '$minus) '$zerob)
				(t 0)))
		(setq origval 0))
              
	      ;; Make assumptions about limit var being very small or very large.
	      ;; Assumptions are forgotten upon exit.
	      (unless (= lenargs 1)
		(limit-context var val dr))

              ;; Resimplify in light of new assumptions.
              (setq exp (resimplify
			             (extra-simp
			              (factosimp
                            (tansc
                              (lfibtophi
                                (limitsimp ($expand exp 1 0) var)))))))

	      (if (not (or (real-epsilonp val)		;; if direction of limit not specified
			   (infinityp val)))
		  (setq ans (both-side exp var val))	;; compute from both sides
		(let ((d (catch 'mabs (mabs-subst exp var val))))
		  (cond 				;; otherwise try to remove absolute value
		   ((eq d '$und) (return '$und))
		   ((eq d 'retn) )
		   (t (setq exp d)))
		  (setq ans (limit-catch exp var val));; and find limit from one side

		  ;; try gruntz
		  (if (not ans)
		      (setq ans (catch 'taylor-catch
				  (let ((silent-taylor-flag t))
				    (declare (special silent-taylor-flag))       
				    (gruntz1 exp var val)))))

		  ;; try taylor series expansion if simple limit didn't work
		  (if (and (null ans)		;; if no limit found and
			   $tlimswitch		;; user says ok to use taylor and
			   (not limit-using-taylor));; not already doing taylor
		      (let ((limit-using-taylor t))
			(declare (special limit-using-taylor))
			(setq ans (limit-catch exp var val))))))

	      (if ans
		  (return (clean-limit-exp ans))
                  (return (cons '(%limit) args)))))	;; failure: return nounform
      (restore-assumptions))))

(defun clean-limit-exp (exp)
  (setq exp (restorelim exp))
  (if preserve-direction exp (ridofab exp)))

;; Users who want limit to map over equality (mequal) will need to do that 
;; manually.
(defun limit-list (exp1 &rest rest)
  (if (and (mbagp exp1) (not (mequalp exp1)))
      `(,(car exp1) ,@(mapcar #'(lambda (x) (apply #'toplevel-$limit `(,x ,@rest))) (cdr exp1)))
      ()))

(defun limit-context (var val direction) ;Only works on entry!
  (cond (limit-top
	 (assume '((mgreaterp) lim-epsilon 0))
	 (assume '((mgreaterp) prin-inf 100000000))
	 (setq limit-assumptions (make-limit-assumptions var val direction))
	 (setq limit-top ()))
	(t ()))
  limit-assumptions)

(defun make-limit-assumptions (var val direction)
  (let ((new-assumptions))
    (cond ((or (null var) (null val))
	   ())
	  ((and (not (infinityp val)) (null direction))
	   ())
	  ((eq val '$inf)
	   `(,(assume `((mgreaterp) ,var 100000000)) ,@new-assumptions))
	  ((eq val '$minf)
	   `(,(assume `((mgreaterp) -100000000 ,var)) ,@new-assumptions))
	  ((eq direction '$plus)
	   `(,(assume `((mgreaterp) ,var 0)) ,@new-assumptions)) ;All limits around 0
	  ((eq direction '$minus)
	   `(,(assume `((mgreaterp) 0 ,var)) ,@new-assumptions))
	  (t
	   ()))))

(defun restore-assumptions ()
;;;Hackery until assume and forget take reliable args. Nov. 9 1979.
;;;JIM.
  (do ((assumption-list limit-assumptions (cdr assumption-list)))
      ((null assumption-list) t)
    (forget (car assumption-list)))
  (forget '((mgreaterp) lim-epsilon 0))
  (forget '((mgreaterp) prin-inf 100000000))
  (cond ((and (not (null integer-info))
	      (not limitp))
	 (do ((list integer-info (cdr list)))
	     ((null list) t)
	   (i-$remove `(,(cadar list) ,(caddar list))))
	 (setq integer-info old-integer-info))))

;; The optional arg allows the caller to decide on the value of
;; preserve-direction.  Default is nil, since we immediately ridofab.
(defun both-side (exp var val &optional (preserve nil))
  (let* ((preserve-direction preserve)
         (la (toplevel-$limit exp var val '$plus)) lb)
    ; Immediately propagate an und without trying the
    ; other direction
    (when (eq la '$und) (return-from both-side '$und))
    (setf lb (toplevel-$limit exp var val '$minus))
    ; Immediately propagate an und
    (when (eq lb '$und) (return-from both-side '$und))
    (let ((ra (ridofab la))
          (rb (ridofab lb)))
      (cond ((eq t (meqp ra rb))
             ra)
            ((and (eq ra '$ind)
                  (eq rb '$ind))
             ; Maxima does not consider equal(ind,ind) to be true, but
             ; if both one-sided limits are ind then we want to call
             ; the two-sided limit ind (e.g., limit(sin(1/x),x,0)).
             '$ind)
            ((or (not (free la '%limit))
                 (not (free lb '%limit)))
             ())
            (t
             (let ((infa (infinityp la))
                   (infb (infinityp lb)))
               (cond ((and infa infb)
                      ; inf + minf => infinity
                      '$infinity)
                     ((or infa infb)
                      '$und)
                     (t
                      '$ind))))))))

(defun limunknown (f)
  (catch 'limunknown (limunknown1 (specrepcheck f))))

(defun limunknown1 (f)
  (cond ((mapatom f) nil)
     ;; special pass for fib(X). What else?
    ((and (consp f) (eq '$fib (caar f))) nil)
	((or (not (safe-get (caar f) 'operators))
	     (member (caar f) '(%sum %product mncexpt) :test #'eq)
	     ;;Special function code here i.e. for li[2](x).
	     (and (eq (caar f) 'mqapply)
		  (not (get (subfunname f) 'specsimp))))
	 (if (not (free f var)) (throw 'limunknown t)))
	(t (mapc #'limunknown1 (cdr f)) nil)))

(defun factosimp(e)
  (if (involve e '(%gamma)) (setq e ($makefact e)))
  (cond ((involve e '(mfactorial))
	 (setq e (simplify ($minfactorial e))))
	(t e)))

;; returns 1, 0, -1
;; or nil if sign unknown or complex
(defun getsignl (z)
  (let ((z (ridofab z)))
    (if (not (free z var)) (setq z (toplevel-$limit z var val)))
    (let ((*complexsign* t))
      (let ((sign (if *getsignl-asksign-ok* ($asksign z) ($sign z))))
        (cond ((eq sign '$pos) 1)
              ((eq sign '$neg) -1)
              ((eq sign '$zero) 0))))))

(defun restorelim (exp)
  (cond ((null exp) nil)
	((atom exp) (or (and (symbolp exp) (get exp 'limitsub)) exp))
	((and (consp (car exp)) (eq (caar exp) 'mrat))
	 (cons (car exp)
	       (cons (restorelim (cadr exp))
		     (restorelim (cddr exp)))))
	(t (cons (car exp) (mapcar #'restorelim (cdr exp))))))


(defun mabs-subst (exp var val)	; RETURNS EXP WITH MABS REMOVED, OR THROWS.
  (let ((d (involve exp '(mabs)))
        arglim)
    (cond ((null d) exp)
	  (t (cond
	       ((not (and (equal ($imagpart (let ((v (limit-catch d var val)))
					      ;; The above call might
					      ;; throw 'limit, so we
					      ;; need to catch it.  If
					      ;; we can't find the
					      ;; limit without ABS, we
					      ;; assume the limit is
					      ;; undefined.  Is this
					      ;; right?  Anyway, this
					      ;; fixes Bug 1548643.
					      (unless v
						(throw 'mabs '$und))
					      (setq arglim v)))
				 0)
			  (equal ($imagpart var) 0)))
                (cond ((eq arglim '$infinity)
                       ;; Check for $infinity as limit of argument.
                       '$inf)
                      (t
                       (throw 'mabs 'retn))))
	       (t (do ((ans d (involve exp '(mabs))) (a () ()))
		      ((null ans) exp)
		    (setq a (mabs-subst ans var val))
		    (setq d (limit a var val t))
		    (cond
		      ((and a d)
		       (cond ((zerop1 d)
			      (setq d (behavior a var val))
			      (if (zerop1 d) (throw 'mabs 'retn))))
		       (if (eq d '$und)
			   (throw 'mabs d))
		       (cond ((or (eq d '$zeroa) (eq d '$inf)
				  (eq d '$ind)
				  ;; fails on limit(abs(sin(x))/sin(x), x, inf)
				  (eq ($sign d) '$pos))
			      (setq exp (maxima-substitute a `((mabs) ,ans) exp)))
			     ((or (eq d '$zerob) (eq d '$minf)
				  (eq ($sign d) '$neg))
			      (setq exp (maxima-substitute (m* -1 a) `((mabs) ,ans) exp)))
			     (t (throw 'mabs 'retn))))
		      (t (throw 'mabs 'retn))))))))))

;; Called on an expression that might contain $INF, $MINF, $ZEROA, $ZEROB. Tries
;; to simplify it to sort out things like inf^inf or inf+1.
(defun simpinf (exp)
  (simpinf-ic exp (count-general-inf exp)))

(defun count-general-inf (expr)
  (count-atoms-matching
   (lambda (x) (or (infinityp x) (real-epsilonp x))) expr))

(defun count-atoms-matching (predicate expr)
  "Count the number of atoms in the Maxima expression EXPR matching PREDICATE,
ignoring dummy variables and array indices."
  (cond
    ((atom expr) (if (funcall predicate expr) 1 0))
    ;; Don't count atoms that occur as a limit of %integrate, %sum, %product,
    ;; %limit etc.
    ((member (caar expr) dummy-variable-operators)
     (count-atoms-matching predicate (cadr expr)))
    ;; Ignore array indices
    ((member 'array (car expr)) 0)
    (t (loop
          for arg in (cdr expr)
          summing (count-atoms-matching predicate arg)))))

(defun simpinf-ic (exp &optional infinity-count)
  (case infinity-count
    ;; A very slow identity transformation...
    (0 exp)

    ;; If there's only one infinity, we replace it by a variable and take the
    ;; limit as that variable goes to infinity. Use $gensym in case we can't
    ;; compute the answer and the limit leaks out.
    (1 (let* ((val (or (inf-typep exp) (epsilon-typep exp)))
              (var ($gensym))
              (expr (subst var val exp))
              (limit (toplevel-$limit expr var val)))
         (cond
           ;; Now we look to see whether the computed limit is any simpler than
           ;; what we shoved in (which we'll define as "doesn't contain EXPR as a
           ;; subtree"). If so, return it.
           ((not (subtree-p expr limit :test #'equal))
            limit)

           ;; Otherwise, return the original form: apparently, we can't compute
           ;; the limit we needed, and it's uglier than what we started with.
           (t exp))))

    ;; If more than one infinity, we have to be a bit more careful.
    (otherwise
     (let* ((arguments (mapcar 'simpinf (cdr exp)))
            (new-expression (cons (list (caar exp)) arguments))
            infinities-left)
       (cond
         ;; If any of the arguments are undefined, we are too.
         ((among '$und arguments) '$und)
         ;; If we ended up with something indeterminate, we punt and just return
         ;; the input.
         ((amongl '(%limit $ind) arguments) exp)

         ;; Exponentiation & multiplication
         ((mexptp exp) (simpinf-expt (first arguments) (second arguments)))
         ((mtimesp exp) (simpinf-times arguments))

         ;; Down to at most one infinity? We do this after exponentiation to
         ;; avoid zeroa^zeroa => 0^0, which will raise an error rather than just
         ;; returning und. We do it after multiplication to avoid zeroa * inf =>
         ;; 0 * inf => 0.
         ((<= (setf infinities-left (count-general-inf new-expression)) 1)
          (simpinf-ic new-expression infinities-left))

         ;; Addition
	 ((mplusp exp) (simpinf-plus arguments))

         ;; Give up!
	 (t new-expression))))))

(defun simpinf-times (arguments)
  (declare (special exp var val))
  ;; When we have a product, we need to spot that zeroa * zerob = zerob, zeroa *
  ;; inf = und etc. Note that (SIMPINF '$ZEROA) => 0, so a nonzero atom is not
  ;; an infinitesimal. Moreover, we can assume that each of ARGUMENTS is either
  ;; a number, computed successfully by the recursive SIMPINF call, or maybe a
  ;; %LIMIT noun-form (in which case, we aren't going to be able to tell the
  ;; answer).
  (cond
    ((member 0 arguments)
     (cond
       ((find-if #'infinityp arguments) '$und)
       ((every #'atom arguments) 0)
       (t exp)))

    ((member '$infinity arguments)
     (if (every #'atom arguments)
         '$infinity
         exp))

    (t (simplimit (cons '(mtimes) arguments) var val))))

(defun simpinf-expt (base exponent)
  ;; In the comments below, zero* represents one of 0, zeroa, zerob.
  ;;
  ;; TODO: In some cases we give up too early. E.g. inf^(2 + 1/inf) => inf^2
  ;;       (which should simplify to inf)
  (case base
    ;; inf^inf = inf
    ;; inf^minf = 0
    ;; inf^zero* = und
    ;; inf^foo = inf^foo
    ($inf
     (case exponent
       ($inf '$inf)
       ($minf 0)
       ((0 $zeroa $zerob) '$und)
       (t (list '(mexpt) base exponent))))
    ;; minf^inf = infinity  <== Or should it be und?
    ;; minf^minf = 0
    ;; minf^zero* = und
    ;; minf^foo = minf^foo
    ($minf
     (case exponent
       ($inf '$infinity)
       ($minf 0)
       ((0 $zeroa $zerob) '$und)
       (t (list '(mexpt) base exponent))))
    ;; zero*^inf = 0
    ;; zero*^minf = und
    ;; zero*^zero* = und
    ;; zero*^foo = zero*^foo
    ((0 $zeroa $zerob)
     (case exponent
       ($inf 0)
       ($minf '$und)
       ((0 $zeroa $zerob) '$und)
       (t (list '(mexpt) base exponent))))
    ;; a^b where a is pretty much anything except for a naked
    ;; inf,minf,zeroa,zerob or 0.
    (t
     (cond
       ;; When a isn't crazy, try a^b = e^(b log(a))
       ((not (amongl (append *infinitesimals* *infinities*) base))
        (simpinf (m^ '$%e (m* exponent `((%log) ,base)))))

       ;; No idea. Just return what we've found so far.
       (t (list '(mexpt) base exponent))))))

(defun simpinf-plus (arguments)
  ;; We know that none of the arguments are infinitesimals, since SIMPINF never
  ;; returns one of them. As such, we partition our arguments into infinities
  ;; and everything else. The latter won't have any "hidden" infinities like
  ;; lim(x,x,inf), since SIMPINF gave up on anything containing a %lim already.
  (let ((bigs) (others))
    (dolist (arg arguments)
      (cond ((infinityp arg) (push arg bigs))
            (t (push arg others))))
    (cond
      ;; inf + minf or the like
      ((cdr (setf bigs (delete-duplicates bigs))) '$und)
      ;; inf + smaller + stuff
      (bigs (car bigs))
      ;; I don't think this can happen, since SIMPINF goes back to the start if
      ;; there are fewer than two infinities in the arguments, but let's be
      ;; careful.
      (t (cons '(mplus) others)))))

;; Simplify expression with zeroa or zerob.
(defun simpab (small)
  (cond ((null small) ())
	((member small '($zeroa $zerob $inf $minf $infinity) :test #'eq) small)
	((not (free small '$ind)) '$ind) ;Not exactly right but not
	((not (free small '$und)) '$und) ;causing trouble now.
	((mapatom small)  small)
	(t (let ((preserve-direction t)
		  (new-small (subst (m^ '$inf -1) '$zeroa
				       (subst (m^ '$minf -1) '$zerob small))))
	          (simpinf new-small)))))
			
    
;;;*I* INDICATES: T => USE LIMIT1,THINK, NIL => USE SIMPLIMIT.
(defun limit (exp var val *i*)
  (cond
    ((among '$und exp)  '$und)
    ((eq var exp)  val)
    ((atom exp)  exp)
    ((not (among var exp))
     (cond ((amongl '($inf $minf $infinity $ind) exp)
	    (simpinf exp))
           ((amongl '($zeroa $zerob) exp)
            ;; Simplify expression with zeroa or zerob.
            (simpab exp))
	   (t exp)))
    ((getlimval exp))
    (t (putlimval exp (cond ((and limit-using-taylor
				  (null taylored)
				  (tlimp exp))
			     (taylim exp var val *i*))
			    ((ratp exp var) (ratlim exp))
			    ((or (eq *i* t) (radicalp exp var))
			     (limit1 exp var val))
			    ((eq *i* 'think)
			     (cond ((or (mtimesp exp) (mexptp exp))
				    (limit1 exp var val))
				   (t (simplimit exp var val))))
			    (t (simplimit exp var val)))))))

(defun limitsimp (exp var)
  (limitsimp-expt (sin-sq-cos-sq-sub exp) var))
;;Hack for sin(x)^2+cos(x)^2.

;; if var appears in base and power of expt,
;; push var into power of of expt
(defun limitsimp-expt (exp var)
  (cond ((or (atom exp)
	     (mnump exp)
	     (freeof var exp))   exp)
	((and (mexptp exp)
	      (not (freeof var (cadr exp)))
	      (not (freeof var (caddr exp))))
	 (m^ '$%e (simplify `((%log) ,exp))))
	(t (subst0 (cons (cons (caar exp) ())
			 (mapcar #'(lambda (x)
				     (limitsimp-expt x var))
				 (cdr exp)))
		   exp))))

(defun gather-args-of (e fn x) 
   (cond (($mapatom e) nil)        
         ((and (eq fn (caar e)) (not (freeof x e))) (cdr e))
          (t 
            (reduce #'append (mapcar #'(lambda (q) (gather-args-of q fn x)) (cdr e))))))

;; When X depends on x, replace cos(X)^2 + sin(X)^2 by 1 
 (defun sin-sq-cos-sq-sub (e &optional (x var))
    (let ((ccc nil) (z) (ee))
      (cond (($mapatom e) e)
            ((mplusp e)
                (setq ccc (gather-args-of e '%cos x))
                  (dolist (g ccc)
                    (setq z (gensym))
                    (setq ee (maxima-substitute z (power (ftake '%cos g) 2) e))
                    (setq ee (maxima-substitute (sub 1 z) (power (ftake '%sin g) 2) ee))
                    (when (freeof z (sratsimp ee))
                        (setq e ee)))
                e)
            ;; maybe this isn't needed, but I think it's not wrong.
            ((eq 'mqapply (caar e))
              (subftake (caar (second e))
                 (mapcar #'(lambda (q) (sin-sq-cos-sq-sub q x)) (subfunsubs e))
                 (mapcar #'(lambda (q) (sin-sq-cos-sq-sub q x)) (subfunargs e)))) 
            ;; map sin-sq-cos-sq-sub over the args
            (t 
             (fapply (caar e) (mapcar #'(lambda (q) (sin-sq-cos-sq-sub q x)) (cdr e)))))))

(defun expand-trigs (x var)
  (cond ((atom x) x)
	((mnump x) x)
	((and (or (eq (caar x) '%sin)
		  (eq (caar x) '%cos))
	      (not (free (cadr x) var)))
	 ($trigexpand x))
	((member 'array (car x))
	 ;; Some kind of array reference.  Return it.
	 x)
	(t (simplify (cons (ncons (caar x))
			   (mapcar #'(lambda (x)
				       (expand-trigs x var))
				   (cdr x)))))))


(defun tansc (e)
  (cond ((not (involve e
		       '(%cot %csc %binomial
			 %sec %coth %sech %csch
			 %acot %acsc %asec %acoth
			 %asech %acsch
			 %jacobi_ns %jacobi_nc %jacobi_cs
			 %jacobi_ds %jacobi_dc)))
	 e)
	(t ($ratsimp (tansc1 e)))))

(defun tansc1 (e &aux tem)
  (cond ((atom e) e)
	((and (setq e (cons (car e) (mapcar 'tansc1 (cdr e))))  ()))
	((setq tem (assoc (caar e) '((%cot . %tan) (%coth . %tanh)
				    (%sec . %cos) (%sech . %cosh)
				    (%csc . %sin) (%csch . %sinh)) :test #'eq))
	 (tansc1 (m^ (list (ncons (cdr tem)) (cadr e)) -1.)))
	((setq tem (assoc (caar e) '((%jacobi_nc . %jacobi_cn)
				    (%jacobi_ns . %jacobi_sn)
				    (%jacobi_cs . %jacobi_sc)
				    (%jacobi_ds . %jacobi_sd)
				    (%jacobi_dc . %jacobi_cd)) :test #'eq))
	 ;; Converts Jacobi elliptic function to its reciprocal
	 ;; function.
	 (tansc1 (m^ (list (ncons (cdr tem)) (cadr e) (third e)) -1.)))
	((setq tem (member (caar e) '(%sinh %cosh %tanh) :test #'eq))
	 (let (($exponentialize t))
	   (resimplify e)))
	((setq tem (assoc (caar e) '((%acsc . %asin) (%asec . %acos)
				    (%acot . %atan) (%acsch . %asinh)
				    (%asech . %acosh) (%acoth . %atanh)) :test #'eq))
	 (list (ncons (cdr tem)) (m^t (cadr e) -1.)))
	((and (eq (caar e) '%binomial) (among var (cdr e)))
	 (m//  `((mfactorial) ,(cadr e))
	       (m* `((mfactorial) ,(m+t (cadr e) (m- (caddr e))))
		   `((mfactorial) ,(caddr e)))))
	(t e)))

(defun hyperex (ex)
  (cond ((not (involve ex '(%sin %cos %tan %asin %acos %atan
			    %sinh %cosh %tanh %asinh %acosh %atanh)))
	 ex)
	(t (hyperex0 ex))))

(defun hyperex0 (ex)
  (cond ((atom ex) ex)
	((eq (caar ex) '%sinh)
	 (m// (m+ (m^ '$%e (cadr ex)) (m- (m^ '$%e (m- (cadr ex)))))
	      2))
	((eq (caar ex) '%cosh)
	 (m// (m+ (m^ '$%e (cadr ex)) (m^ '$%e (m- (cadr ex))))
	      2))
	((and (member (caar ex)
		    '(%sin %cos %tan %asin %acos %atan %sinh
		      %cosh %tanh %asinh %acosh %atanh) :test #'eq)
	      (among var ex))
	 (hyperex1 ex))
	(t (cons (car ex) (mapcar #'hyperex0 (cdr ex))))))

(defun hyperex1 (ex)
  (resimplify ex))

;;Used by tlimit also.
(defun limit1 (exp var val)
  (prog ()
     (let ((lhprogress? lhp?)
           (lhp? ())
           (ans ()))
       (cond ((setq ans (and (not (atom exp)) (getlimval exp)))
	      (return ans))
	     ((and (not (infinityp val)) (setq ans (simplimsubst val exp)))
	      (return ans))
	     (t nil))
;;;NUMDEN* => (values numerator denominator)
       (multiple-value-bind (n dn)
           (numden* exp)
         (cond ((not (among var dn))
                (return (simplimit (m// (simplimit n var val) dn) var val)))
               ((not (among var n))
                (return (simplimit (m* n (simplimexpt dn -1 (simplimit dn var val) -1)) var val)))
               ((and lhprogress?
                   (/#alike n (car lhprogress?))
                   (/#alike dn (cdr lhprogress?)))
                (throw 'lhospital nil)))
         (return (limit2 n dn var val))))))

(defun /#alike (e f)
  (if (alike1 e f)
      t
      (let ((deriv (sdiff (m// e f) var)))
        (cond ((=0 deriv) t)
              ((=0 ($ratsimp deriv)) t)
              (t nil)))))

;; The standard /#alike function is possibly somewhat inefficient. Here is
;; a possible replacement:

;; If e/f is free of var (a special), return true. This code 
;; assumes that f is not zero. First, we test if e & f
;; are alike--this check is relatively fast; second, we check
;; if e/f is free of var.

;;(defun /#alike (e f) 
;;	(or (alike1 e f) (freeof var (sratsimp (div e f)))))

(defun limit2 (n dn var val)
  (prog (n1 d1 lim-sign gcp sheur-ans)
     (setq n (hyperex n) dn (hyperex dn))
;;;Change to uniform limit call.
     (cond ((infinityp val)
	    (setq d1 (limit dn var val nil))
	    (setq n1 (limit n var val nil)))
	   (t (cond ((setq n1 (simplimsubst val n)) nil)
		    (t (setq n1 (limit n var val nil))))
	      (cond ((setq d1 (simplimsubst val dn)) nil)
		    (t (setq d1 (limit dn var val nil))))))
     (cond ((or (null n1) (null d1)) (return nil))
	   (t (setq n1 (sratsimp n1) d1 (sratsimp d1))))
     (cond ((or (involve n '(mfactorial)) (involve dn '(mfactorial)))
	    (let ((ans (limfact2 n dn var val)))
	      (cond (ans (return ans))))))
     (cond ((and (zerop2 n1) (zerop2 d1))
	    (cond  ((not (equal (setq gcp (gcpower n dn)) 1))
		    (return (colexpt n dn gcp)))
		   ((and (real-epsilonp val)
			 (not (free n '%log))
			 (not (free dn '%log)))
		    (return (liminv (m// n dn))))
		   ((setq n1 (try-lhospital-quit n dn nil))
		    (return n1))))
	   ((and (zerop2 n1) (not (member d1 '($ind $und) :test #'eq))) (return 0))
	   ((zerop2 d1)
	    (setq n1 (ridofab n1))
	    (return (simplimtimes `(,n1 ,(simplimexpt dn -1 d1 -1))))))
     (setq n1 (ridofab n1))
     (setq d1 (ridofab d1))
     (cond ((or (eq d1 '$und)
		(and (eq n1 '$und) (not (real-infinityp d1))))
	    (return '$und))
           ((eq d1 '$ind)
	    ;; At this point we have n1/$ind. Look if n1 is one of the
	    ;; infinities or zero.
	    (cond ((and (infinityp n1) (eq ($sign dn) '$pos))
		   (return n1))
		  ((and (infinityp n1) (eq ($sign dn) '$neg))
		   (return (simpinf (m* -1 n1))))
		  ((and (zerop1 n1)
			(or (eq ($sign dn) '$pos)
			    (eq ($sign dn) '$neg)))
		   (return 0))
		  (t (return '$und))))
	   ((eq n1 '$ind) (return (cond ((infinityp d1) 0)
					((equal d1 0) '$und)
					(t '$ind)))) ;SET LB
	   ((and (real-infinityp d1) (member n1 '($inf $und $minf) :test #'eq))
	    (cond ((and (not (atom dn)) (not (atom n))
			(cond ((not (equal (setq gcp (gcpower n dn)) 1))
			       (return (colexpt n dn gcp)))
			      ((and (eq '$inf val)
				    (or (involve dn '(mfactorial %gamma))
					(involve n '(mfactorial %gamma))))
			       (return (limfact n dn))))))
		  ((eq n1 d1) (setq lim-sign 1) (go cp))
		  (t (setq lim-sign -1) (go cp))))
	   ((and (infinityp d1) (infinityp n1))
	    (setq lim-sign (if (or (eq d1 '$minf) (eq n1 '$minf)) -1 1))
	    (go cp))
	   (t (return (simplimtimes `(,n1 ,(m^ d1 -1))))))
     cp   (setq n ($expand n) dn ($expand dn))
     (cond ((mplusp n)
	    (let ((new-n (m+l (maxi (cdr n)))))
	      (cond ((not (alike1 new-n n))
		     (return (limit (m// new-n dn) var val 'think))))
	      (setq n1 new-n)))
	   (t (setq n1 n)))
     (cond ((mplusp dn)
	    (let ((new-dn (m+l (maxi (cdr dn)))))
	      (cond ((not (alike1 new-dn dn))
		     (return (limit (m// n new-dn) var val 'think))))
	      (setq d1 new-dn)))
	   (t (setq d1 dn)))
     (setq sheur-ans (sheur0 n1 d1))
     (cond ((or (member sheur-ans '($inf $zeroa) :test #'eq)
		(free sheur-ans var))
	    (return (simplimtimes `(,lim-sign ,sheur-ans))))
	   ((and (alike1 sheur-ans dn)
		 (not (mplusp n))))
	   ((member (setq n1 (cond ((expfactorp n1 d1)  (expfactor n1 d1 var))
				 (t ())))
		  '($inf $zeroa) :test #'eq)
	    (return n1))
	   ((not (null (setq n1 (cond ((expfactorp n dn)  (expfactor n dn var))
				      (t ())))))
	    (return n1))
	   ((and (alike1 sheur-ans dn) (not (mplusp n))))
	   ((not (alike1 sheur-ans (m// n dn)))
	    (return (simplimit (m// ($expand (m// n sheur-ans))
				    ($expand (m// dn sheur-ans)))
			       var
			       val))))
     (cond ((and (not (and (eq val '$inf) (expp n) (expp dn)))
		 (setq n1 (try-lhospital-quit n dn nil))
		 (not (eq n1 '$und)))
	    (return n1)))
     (throw 'limit t)))

;; Test whether both n and dn have form
;; product of poly^poly
(defun expfactorp (n dn)
  (do ((llist (append (cond ((mtimesp n) (cdr n))
			    (t (ncons n)))
		      (cond ((mtimesp dn) (cdr dn))
			    (t (ncons dn))))
	      (cdr llist))
       (exp? t)		  ;IS EVERY ELEMENT SO FAR
       (factor nil))			;A POLY^POLY?
      ((or (null llist)
	   (not exp?))
       exp?)
    (setq factor (car llist))
    (setq exp? (or (polyinx factor var ())
		   (and (mexptp factor)
			(polyinx (cadr factor) var ())
			(polyinx (caddr factor) var ()))))))

(defun expfactor (n dn var)	;Attempts to evaluate limit by grouping
  (prog (highest-deg)		       ; terms with similar exponents.
     (let ((new-exp (exppoly n)))	;exppoly unrats expon
       (setq n (car new-exp)		;and rtns deg of expons
	     highest-deg (cdr new-exp)))
     (cond ((null n) (return nil)))	;nil means expon is not
     (let ((new-exp (exppoly dn)))	;a rat func.
       (setq dn (car new-exp)
	     highest-deg (max highest-deg (cdr new-exp))))
     (cond ((or (null dn)
		(= highest-deg 0))	; prevent infinite recursion
	    (return nil)))
     (return
       (do ((answer 1)
	    (degree highest-deg (1- degree))
	    (numerator n)
	    (denominator dn)
	    (numfactors nil)
	    (denfactors nil))
	   ((= degree -1)
	    (m* answer
		(limit (m// numerator denominator)
		       var
		       val
		       'think)))
	 (let ((newnumer-factor (get-newexp&factors
				 numerator
				 degree
				 var)))
	   (setq numerator (car newnumer-factor)
		 numfactors (cdr newnumer-factor)))
	 (let ((newdenom-factor (get-newexp&factors
				 denominator
				 degree
				 var)))
	   (setq denominator (car newdenom-factor)
		 denfactors (cdr newdenom-factor)))
	 (setq answer (simplimit (list '(mexpt)
				       (m* answer
					   (m// numfactors denfactors))
				       (cond ((> degree 0) var)
					     (t 1)))
				 var
				 val))
	 (cond ((member answer '($ind $und) :test #'equal)
		;; cannot handle limit(exp(x*%i)*x, x, inf);
		(return nil))
	       ((member answer '($inf $minf) :test #'equal)
		;; 0, zeroa, zerob are passed through to next iteration 
		(return (simplimtimes (list (m// numerator denominator) answer)))))))))

(defun exppoly (exp)	   ;RETURNS EXPRESSION WITH UNRATTED EXPONENTS
  (do ((factor nil)
       (highest-deg 0)
       (new-exp 1)
       (exp (cond ((mtimesp exp)
		   (cdr exp))
		  (t (ncons exp)))
	    (cdr exp)))
      ((null exp) (cons new-exp highest-deg))
    (setq factor (car exp))
    (setq new-exp
	  (m* (cond ((or (not (mexptp factor))
			 (not (ratp (caddr factor) var)))
		     factor)
		    (t (setq highest-deg
			     (max highest-deg
				  (ratdegree (caddr factor))))
		       (m^ (cadr factor) (unrat (caddr factor)))))
	      new-exp))))

(defun unrat (exp)			;RETURNS UNRATTED EXPRESION
  (multiple-value-bind (n d)
      (numden* exp)
    (let ((tem ($divide n d)))
      (m+ (cadr tem)
	  (m// (caddr tem) d)))))

(defun get-newexp&factors (exp degree var) ;RETURNS (CONS NEWEXP FACTORS)
  (do ((terms (cond ((mtimesp exp)(cdr exp)) ; SUCH THAT
		    (t (ncons exp)))	; NEWEXP*FACTORS^(VAR^DEGREE)
	      (cdr terms))		; IS EQUAL TO EXP.
       (factors 1)
       (newexp 1)
       (factor nil))
      ((null terms)
       (cons newexp
	     factors))
    (setq factor (car terms))
    (cond ((not (mexptp factor))
	   (cond ((= degree 0)
		  (setq factors (m* factor factors)))
		 (t (setq newexp (m* factor newexp)))))
	  ((or (= degree -1)
	       (= (ratdegree (caddr factor))
		  degree))
	   (setq factors (m* (m^ (cadr factor)
				 (leading-coef (caddr factor)))
			     factors)
		 newexp (m* (m^ (cadr factor)
				(m- (caddr factor)
				    (m* (leading-coef (caddr factor))
					(m^ var degree))))
			    newexp)))
	  (t (setq newexp (m* factor newexp))))))

(defun leading-coef (rat)
  (ratlim (m// rat (m^ var (ratdegree rat)))))

(defun ratdegree (rat)
  (multiple-value-bind (n d)
      (numden* rat)
    (- (deg n) (deg d))))

(defun limfact2 (n d var val)
  (let ((n1 (reflect0 n var val))
	(d1 (reflect0 d var val)))
    (cond ((and (alike1 n n1)
		(alike1 d d1))
	   nil)
	  (t (limit (m// n1 d1) var val 'think)))))

;; takes expression and returns operator at front with all flags removed
;; except array flag.
;; array flag must match for alike1 to consider two things to be the same.
;;   ((MTIMES SIMP) ... ) => (MTIMES)
;;   ((PSI SIMP ARRAY) 0) => (PSI ARRAY)
(defun operator-with-array-flag (exp)
  (cond ((member 'array (car exp) :test #'eq)
	 (list (caar exp) 'array))
	(t (list (caar exp)))))

(defun reflect0 (exp var val)
  (cond ((atom exp) exp)
	((and (eq (caar exp) 'mfactorial)
	      (let ((argval (limit (cadr exp) var val 'think)))
		(or (eq argval '$minf)
		    (and (numberp argval)
			 (> 0 argval)))))
	 (reflect (cadr exp)))
	(t (cons (operator-with-array-flag exp)
		 (mapcar (function
			  (lambda (term)
			   (reflect0 term var val)))
			 (cdr exp))))))

(defun reflect (arg)
  (m* -1
      '$%pi
      (m^ (list (ncons 'mfactorial)
		(m+ -1
		    (m* -1 arg)))
	  -1)
      (m^ (list (ncons '%sin)
		(m* '$%pi arg))
	  -1)))

(defun limfact (n d)
  (let ((ans ()))
    (setq n (stirling0 n)
	  d (stirling0 d))
    (setq ans (toplevel-$limit (m// n d) var '$inf))
    (cond ((and (atom ans)
		(not (member ans '(und ind ) :test #'eq)))  ans)
	  ((eq (caar ans) '%limit)  ())
	  (t ans))))

;; substitute asymptotic approximations for gamma, factorial, and
;; polylogarithm
(defun stirling0 (e)
   (cond ((atom e) e)
	((and (setq e (cons (car e) (mapcar 'stirling0 (cdr e))))
	      nil))
	((and (eq (caar e) '%gamma)
	      (eq (limit (cadr e) var val 'think) '$inf))
	 (stirling (cadr e)))
	((eq (caar e) 'mfactorial)
		(let ((n (limit (cadr e) var val 'think)))
	 	  (cond ((eq n '$inf)
		          (m* (cadr e) (stirling (cadr e))))
		        ((and (integerp n) (< n 0))
		   		  (setq n (mul -1 n))
		          (div (power -1 n) (mul (ftake 'mfactorial n) (add var n))))
				(t e))))
	((and (eq (caar e) 'mqapply)		;; polylogarithm
	      (eq (subfunname e) '$li)
	      (let ((arglim (limit (car (subfunargs e)) var val 'think)))
		(or (eq arglim '$inf) (eq arglim '$minf)))
	      (integerp (car (subfunsubs e))))
	 (li-asymptotic-expansion (m- (car (subfunsubs e)) 1) 
				   (car (subfunsubs e))
				   (car (subfunargs e))))
	(t e)))

(defun stirling (x)
  (maxima-substitute x '$z
		     '((mtimes simp)
		       ((mexpt simp) 2 ((rat simp) 1 2))
		       ((mexpt simp) $%pi ((rat simp) 1 2))
		       ((mexpt simp) $z ((mplus simp) ((rat simp) -1 2) $z))
		       ((mexpt simp) $%e ((mtimes simp) -1 $z)))))

(defun no-err-sub (v e &aux ans)
  (let ((errorsw t) (*zexptsimp? t)
	(errcatch t)
	;; Don't print any error messages
	($errormsg nil))
    ;; Should we just use IGNORE-ERRORS instead HANDLER-CASE here?  I
    ;; (rtoy) am choosing the latter so that unexpected errors will
    ;; actually show up instead of being silently discarded.
    (handler-case
	(setq ans (catch 'errorsw
                    (ignore-rat-err
                      (sratsimp (subin v e)))))
      (maxima-$error ()
	(setq ans nil)))
    (cond ((null ans) t)     ; Ratfun package returns NIL for failure.
	  (t ans))))

(defun extended-real-p (e)
	(member e (list '$minf '$zerob '$zeroa '$ind '$und '$inf '$infinity)))

;; Evaluate the limit of e as var (special) approaches v using direct substitution.
;; This function is the last of the chain of methods tried by limit. As such it
;; shouldn't call higher level functions such as simplimit or limit--doing so 
;; would risk creating an infinite loop.

;; This function special cases sums, products, and powers. It declines to use 
;; direct substitution on any expression whose main operator has a simplim%function 
;; function. Generally, limits of functions that have a simplim%function should be 
;; handled by those specialized functions, not by simplimsubst. Having said this, 
;; it's OK for simplimsubst to special case an operator and do direct substitution 
;; when its OK. The cases of log, cosine, and sine happen often enough that these
;; functions are special cased.

;; Possibly  simplimsubst should decline to do direct substitution on functions 
;; that have a limit function, for example inverse_jacobi_ns.

;; For all other kinds of expressions, this function assumes that if substitution 
;; doesn't result in an error (division by zero, for example), the function
;; is continuous at the limit point. That's dodgy. This dodginess underscores the
;; need to define a simplim%function functions that are not continuous on their 
;; domains. 

;; This function sometimes receives an un-simplified expression. Maybe they should be
;; simplified, maybe not. 

;; Locally setting $numer and $%enumer to nil keeps some limits, for example
;; limit(sin(x)/x,x,0) from returning 1.0 when numer is true. (Barton Willis)

(defun simplimsubst (v e)
  (let ((ans nil) ($numer nil) ($%enumer nil) (ee))
   	(cond 
	    ;; When e is a number, return it.
	    (($numberp e) e)
		;; When e is a mapatom, substitute v for var and return.
		(($mapatom e) ($substitute v var e))
	    ;; Special case mexpt expressions. Decline direct substitution for 
		;; extended reals. 
		((and (mexptp e) (not (extended-real-p v)))
		   (let ((x (simplimsubst v (second e)))
		   		 (n (simplimsubst v (third e))))
		   ;; Decline direct substitution (DS) for 0^negative. Also decline
		   ;; DS when x is on the negative real axis and n isn't an integer.
		   ;; Additionally, we require that DS is OK for both x & n.
		   (if (and x n (not (and (zerop2 x) (eq t (mgqp 0 n)))) ; not 0^negative
		            (or (off-negative-real-axisp x) (integerp n)))
		   	     (ftake 'mexpt x n) nil)))
        ;; Special case product and sum expressions. Again, we decline direct 
		;; substitution for extended reals. 
        ((and (or (mplusp e) (mtimesp e)) (not (extended-real-p v)))
		  (setq ee (mapcar #'(lambda(q) (simplimsubst v q)) (cdr e)))
		  (if (some #'(lambda (q) (eq q nil)) ee) nil
				  (simplifya (cons (list (caar e)) ee) t)))
        ;; Decline direct substitution for sums, products, and powers for
		;; the extended real case.
        ((and (or (mexptp e) (mplusp e) (mtimesp e)) (extended-real-p v))
	  	 nil)
	    ;; Special case log expressions--possibly the log case happens
		;; often enough to make this worthwhile.
        ((and (consp e) (consp (car e)) (eq '%log (caar e)))
		  (let ((w (simplimsubst v (cadr e))))
		    (if (and w (off-negative-real-axisp w)) (ftake '%log w) nil))) 
		;; Special case %cos and %sin expressions--we could special case others.
		;; The lenient-realp check declines direct substitution for complex arguments--
		;; this check could be relaxed.
		((and (consp e) (consp (car e)) (or (member (caar e) (list '%cos '%sin)))) 
		  (let ((op (caar e)))
			 (setq e (simplimsubst v (second e)))
		 (if (and e (lenient-realp e) (not (extended-real-p e))) (ftake op e) nil)))
		;; Don't use direct substitution on expressions whose main operator has
		;; a simplim%function. 
	    ((and (consp e) (consp (car e)) (get (caar e) 'simplim%function)) nil)
		;; The function no-err-sub returns true when there is an error
		((not (eq t (setq ans (no-err-sub (ridofab v) e))))
	        ;; Previously the condition (zerop2 ans) was (=0 ($radcan ans)). In 
			;; December 2021, the testsuite + the share testsuite only gives ans = 0,
			;; making the radcan unneeded. 
 	        (cond ((and (member v '($zeroa $zerob) :test #'eq) (zerop2 ans))
	     	  	    (setq ans (behavior e var v))
	                (cond ((eql ans 1) '$zeroa)
		                  ((eql ans -1) '$zerob)
		                  (t nil)))  ;behavior can't find direction
	              (t ans)))
		;; direct substitution fails, so return nil.		  
		(t nil))))		  

;;;returns (cons numerator denominator)
(defun numden* (e)
  (let ((e (factor (simplify e)))
	(numer ())
        (denom ()))
    (cond ((atom e)
	   (push e numer))
	  ((mtimesp e)
	   (mapc #'forq (cdr e)))
	  (t
           (forq e)))
    (cond ((null numer)
	   (setq numer 1))
	  ((null (cdr numer))
	   (setq numer (car numer)))
	  (t
           (setq numer (m*l numer))))
    (cond ((null denom)
	   (setq denom 1))
	  ((null (cdr denom))
	   (setq denom (car denom)))
	  (t
           (setq denom (m*l denom))))
    (values (factor numer) (factor denom))))

;;;FACTOR OR QUOTIENT
;;;Setq's the special vars numer and denom from numden*
(defun forq (e)
  (cond ((and (mexptp e)
	      (not (freeof var e))
	      (null (pos-neg-p (caddr e))))
	 (push (m^ (cadr e) (m* -1. (caddr e))) denom))
	(t (push e numer))))

;;;Predicate to tell whether an expression is pos,zero or neg as var -> val.
;;;returns T if pos,zero. () if negative or don't know.
(defun pos-neg-p (exp)
  (let ((ans (limit exp var val 'think)))
    (cond ((and (not (member ans '($und $ind $infinity) :test #'eq))
		(equal ($imagpart ans) 0))
	   (let ((sign (getsignl ans)))
	     (cond ((or (equal sign 1)
			(equal sign 0))
		    t)
		   ((equal sign -1)  nil))))
	  (t 'unknown))))

(declare-top (unspecial n dn))

(defun expp (e)
  (cond ((radicalp e var) nil)
	((member (caar e) '(%log %sin %cos %tan %sinh %cosh %tanh mfactorial
			    %asin %acos %atan %asinh %acosh %atanh) :test #'eq) nil)
	((simplexp e) t)
	((do ((e (cdr e) (cdr e)))
	     ((null e) nil)
	   (and (expp (car e)) (return t))))))

(defun simplexp (e)
  (and (mexptp e)
       (radicalp (cadr e) var)
       (among var (caddr e))
       (radicalp (caddr e) var)))


(defun gcpower (a b)
  ($gcd (getexp a) (getexp b)))

(defun getexp (exp)
  (cond ((and (mexptp exp)
	      (free (caddr exp) var)
	      (eq (ask-integer (caddr exp) '$integer) '$yes))
	 (caddr exp))
	((mtimesp exp) (getexplist (cdr exp)))
	(t 1)))

(defun getexplist (list)
  (cond ((null (cdr list))
	 (getexp (car list)))
	(t ($gcd (getexp (car list))
		 (getexplist (cdr list))))))

(defun limroot (exp power)
  (cond ((or (atom exp) (not (member (caar exp) '(mtimes mexpt) :test #'eq)))
	 (limroot (list '(mexpt) exp 1) power)) ;This is strange-JIM.
	((mexptp exp)  (m^ (cadr exp)
			   (sratsimp (m* (caddr exp) (m^ power -1.)))))
	(t (m*l (mapcar #'(lambda (x)
			    (limroot x power))
			(cdr exp))))))

;;NUMERATOR AND DENOMINATOR HAVE EXPONENTS WITH GCD OF GCP.
;;; Used to call simplimit but some of the transformations used here
;;; were not stable w.r.t. the simplifier, so try keeping exponent separate
;;; from bas.

(defun colexpt (n dn gcp)
  (let ((bas (m* (limroot n gcp) (limroot dn (m* -1 gcp))))
	(expo gcp)
	baslim expolim)
    (setq baslim (limit bas var val 'think))
    (setq expolim (limit expo var val 'think))
    (simplimexpt bas expo baslim expolim)))

;; this function is responsible for the following bug:
;; limit(x^2 + %i*x, x, inf)  -> inf	(should be infinity)
(defun ratlim (e)
  (setq e (sratsimp ($trigreduce e)))
  (cond ((member val '($inf $infinity) :test #'eq)
	 (setq e (maxima-substitute (m^t 'x -1) var e)))
	((eq val '$minf)
	 (setq e (maxima-substitute (m^t -1 (m^t 'x -1)) var e)))
	((eq val '$zerob)
	 (setq e (maxima-substitute (m- 'x) var e)))
	((eq val '$zeroa)
	 (setq e (maxima-substitute 'x var e)))
	((setq e (maxima-substitute (m+t 'x val) var e))))
  (destructuring-let* ((e (let (($ratfac ()))
			    ($rat (sratsimp e) 'x)))
		       ((h n . d) e)
		       (g (genfind h 'x))
		       (nd (lodeg n g))
		       (dd (lodeg d g)))
		      (cond ((and (setq e
					(subst var
					       'x
					       (sratsimp (m// ($ratdisrep `(,h ,(locoef n g) . 1))
							      ($ratdisrep `(,h ,(locoef d g) . 1))))))
				  (> nd dd))
			     (cond ((not (member val '($zerob $zeroa $inf $minf) :test #'eq))
				    0)
				   ((not (equal ($imagpart e) 0))
				    0)
				   ((null (setq e (getsignl ($realpart e))))
				    0)
				   ((equal e 1) '$zeroa)
				   ((equal e -1) '$zerob)
				   (t 0)))
			    ((equal nd dd) e)
			    ((not (member val '($zerob $zeroa $infinity $inf $minf) :test #'eq))
			     (throw 'limit t))
			    ((eq val '$infinity)  '$infinity)
			    ((not (equal ($imagpart e) 0)) '$infinity)
			    ((null (setq e (getsignl ($realpart e))))
			     (throw 'limit t))
			    ((equal e 1) '$inf)
			    ((equal e -1) '$minf)
			    (t 0))))

(defun lodeg (n x)
  (if (or (atom n) (not (eq (car n) x)))
      0
      (lowdeg (cdr n))))

(defun locoef (n x)
  (if (or (atom n) (not (eq (car n) x)))
      n
      (car (last n))))

;; This function tries to determine the increasing/decreasing
;; behavior of an expression exp with respect to some variable var.
;; val determines the mode:
;; - $zeroa: From "positive zero" towards the right
;; - $zerob: From "negative zero" towards the left
;; - $inf:   From "positive infinity" towards the left
;; - $minf:  From "negative infinity" towards the right
;; Return values are -1 (decreasing), +1 (increasing) or 0 (don't know).
(defun behavior (exp var val)
  ;; $inf/$minf is handled by substituting 1/var for var
  ;; and setting val to $zeroa/$zerob.
  (cond ((real-infinityp val)
		 (setq val (cond
					 ((eq val '$inf) '$zeroa)
					 ((eq val '$minf) '$zerob))
			   exp (sratsimp (subin (m^ var -1) exp)))))
  (cond
	((not (member val '($zeroa $zerob $inf $minf)))
	  0)
	;; Shortcut for constants.
	((freeof var exp)
	  0)
	;; Shortcut for the variable itself.
	((eq exp var)
	  (if (member val '($zeroa $minf) :test #'eq) 1 -1))
	;; This limits the recursion depth of the function.
	;; Before giving up, always try behavior-by-diff, which doesn't recurse.
	((= *behavior-count-now* +behavior-count+)
      (behavior-by-diff exp var val))
	(t
      (let ((*behavior-count-now* (1+ *behavior-count-now*)) pair sign ans)
	(cond
	  ;; Pull out constant factors with known signs from a product:
	  ;; behavior(c * f(x)) = signum(c) * behavior(f(x))
	  ((and (mtimesp exp)
		(prog2 (setq pair (partition exp var 1))
		(not (equal (car pair) 1))))
	   (setq sign (getsignl (car pair)))
	   (if (not (fixnump sign))
	   0
	   (mul sign (behavior (cdr pair) var val))))
	  ;; Pull out constant terms from a sum:
	  ;; behavior(c + f(x)) = behavior(f(x))
	  ((and (mplusp exp)
		(prog2 (setq pair (partition exp var 0))
		(not (equal (car pair) 0))))
	   (behavior (cdr pair) var val))
	  ;; Handle f(x)^c for the case f(0) = 0 and c > 0
	  ;; (probably other cases could be handled, too)
	  ((and (mexptp exp)
		(free (caddr exp) var)
		(=0 (no-err-sub 0 exp))
		(equal (getsignl (caddr exp)) 1)
		(not (equal 0 (setq ans (behavior-expt (cadr exp) (caddr exp))))))
	   ans)
	  ;; Handle 1 / f(x):
	  ;; behavior(1 / f(x)) = -behavior(f(x))
	  ((equal ($num exp) 1)
		(- (behavior ($denom exp) var val)))
	  ;; Handle c^f(x) for c > 1:
	  ;; behavior(c^f(x)) = behavior(f(x))
	  ((and (mexptp exp)
		(free (cadr exp) var)
		(equal 1 (getsignl (m- (cadr exp) 1)))
		(not (equal 0 (setq ans (behavior (caddr exp) var val)))))
	   ans)
	  ;; Handle c^f(x) for 0 < c < 1:
	  ;; behavior(c^f(x)) = -behavior(f(x))
	  ((and (mexptp exp)
		(free (cadr exp) var)
		(equal 1 (getsignl (cadr exp)))
		(equal -1 (getsignl (m- (cadr exp) 1)))
		(not (equal 0 (setq ans (behavior (caddr exp) var val)))))
	   (- ans))
	  ;; atan, erf, sinh and tanh are monotonically increasing,
	  ;; so their behavior is equal to the behavior of their arguments.
	  ;; behavior(monotonically_increasing(f(x))) = behavior(f(x))
	  ((member (caar exp) '(%atan %erf %sinh %tanh) :test #'eq)
		(behavior (cadr exp) var val))
	  ;; Similarly, acot is monotonically decreasing left and right of x = 0.
	  ;; The singularity doesn't matter for our purposes.
	  ;; behavior(monotonically_decreasing(f(x))) = -behavior(f(x))
	  ((eq (caar exp) '%acot)
		(- (behavior (cadr exp) var val)))
	  ;; Note: More functions could be added here.
	  ;; Ideally, use properties defined on the functions.
	  ;; Handle log(f(x)) for f(0) = 0:
	  ;; If behavior(f(x)) = 1, then behavior(log(f(x))) = 1
	  ((and (mlogp exp)
		(=0 (no-err-sub 0 (cadr exp)))
		(equal 1 (behavior (cadr exp) var val)))
	   1)
	  ;; Try to determine the behavior by taking the derivative.
	  ((not (equal 0 (setq ans (behavior-by-diff exp var val))))
	   ans)
	  ;; If exp is a sum and all terms have the same behavior, return that.
	  ;; The sum of increasing functions is increasing.
	  ;; The sum of decreasing functions is decreasing.
	  ((and (mplusp exp)
		(not (equal 0 (setq ans (behavior-all-same exp var val)))))
	   ans)
	  (t 0))))))

(defun behavior-all-same (exp var val)
  (let* ((exps (cdr exp)) (first-behavior (behavior (first exps) var val)))
	(if (or (equal first-behavior 0)
			(not (every #'(lambda (exp) (equal (behavior exp var val) first-behavior))
						(rest exps))))
	  0
	  first-behavior)))

(defun behavior-expt (bas expo)
  (let ((behavior (behavior bas var val)))
    (cond ((= behavior 1) 1)
	  ((= behavior 0) 0)
	  ((eq (ask-integer expo '$integer) '$yes)
	   (cond ((eq (ask-integer expo '$even) '$yes) 1)
		 (t behavior)))
	  ((ratnump expo)
	   (cond ((evenp (cadr expo)) 1)
		 ((oddp (caddr expo)) behavior)
		 (t 0)))
	  (t 0))))

(defun behavior-by-diff (exp var val)
 (do ((ct 0 (1+ ct))
  (exp (sratsimp (sdiff exp var)) (sratsimp (sdiff exp var)))
  (n () (not n))
  (ans ()))	; This do wins by a return.
 ((> ct 1) 0)	; This loop used to run up to 5 times,
 ;; but the size of some expressions would blow up.
   (setq ans (no-err-sub 0 exp)) ;Why not do an EVENFN and ODDFN
			;test here.
   (cond ((eq ans t)
	  (return 0))
	 ((=0 ans) ())	;Do it again.
	 (t (setq ans (getsignl ans))
	(cond (n (return ans))
		  ((equal ans 1)
		   (return (if (eq val '$zeroa) 1 -1)))
		  ((equal ans -1)
		   (return (if (eq val '$zeroa) -1 1)))
		  (t (return 0)))))))

(defun try-lhospital (n d ind)
  ;;Make one catch for the whole bunch of lhospital trials.
  (let ((ans (lhospital-catch n d ind)))
    (cond ((null ans) ())
	  ((not (free-infp ans)) (simpinf ans))
	  ((not (free-epsilonp ans)) (simpab ans))
	  (t ans))))

(defun try-lhospital-quit (n d ind)
  (let ((ans (or (lhospital-catch n d ind)
		 (lhospital-catch (m^ d -1) (m^ n -1) ind))))
    (cond ((null ans) (throw 'limit t))
	  ((not (free-infp ans)) (simpinf ans))
	  ((not (free-epsilonp ans)) (simpab ans))
	  (t ans))))

(defun lhospital-catch (n d ind)
  (cond ((> 0 lhcount)
	 (setq lhcount $lhospitallim)
	 (throw 'lhospital nil))
	((equal lhcount $lhospitallim)
	 (let ((lhcount (m+ lhcount -1)))
	   (catch 'lhospital (lhospital n d ind))))
	(t (setq lhcount (m+ lhcount -1))
	   (prog1 (lhospital n d ind)
	     (setq lhcount (m+ lhcount 1))))))
;;If this succeeds then raise LHCOUNT.


(defun lhospital (n d ind)
  (declare (special val lhp?))
  (when (mtimesp n)
    (setq n (m*l (mapcar #'(lambda (term) (lhsimp term var val)) (cdr n)))))
  (when (mtimesp d)
    (setq d (m*l (mapcar #'(lambda (term) (lhsimp term var val)) (cdr d)))))
  (multiple-value-bind (n d)
      (lhop-numden n d)
    (let (const nconst dconst)
      (setq lhp? (and (null ind) (cons n d)))
      (multiple-value-setq (nconst n) (var-or-const n))
      (multiple-value-setq (dconst d) (var-or-const d))

      (setq n (stirling0 n))	;; replace factorial and %gamma
      (setq d (stirling0 d))  	;;  with approximations

      (setq n (sdiff n var)	;; take derivatives for l'hospital
            d (sdiff d var))

      (if (or (not (free n '%derivative)) (not (free d '%derivative)))
          (throw 'lhospital ()))
      (setq n (expand-trigs (tansc n) var))
      (setq d (expand-trigs (tansc d) var))

      (multiple-value-setq (const n d) (remove-singularities n d))
      (setq const (m* const (m// nconst dconst)))
      (simpinf (let ((ans (if ind
                              (limit2 n d var val)
                              (limit-numden n d val))))
                 ;; When the limit function returns, it's possible that it will return NIL
                 ;; (gave up without finding a limit). It's also possible that it will
                 ;; return something containing UND. We treat that as a failure too.
                 (when (and ans (freeof '$und ans))
                   (sratsimp (m* const ans))))))))

;; Try to compute the limit of a quotient NUM/DEN, trying to massage the input
;; into a convenient form for LIMIT on the way.
(defun limit-numden (n d val)
  (let ((expr (cond
                ;; For general arguments, the best approach seems to be to use
                ;; sratsimp to simplify the quotient as much as we can, then
                ;; $multthru, which splits it up into a sum (presumably useful
                ;; because limit(a+b) = limit(a) + limit(b) if the limits exist, and
                ;; the right hand side might be easier to calculate)
                ((not (mplusp n))
                 ($multthru (sratsimp (m// n d))))

                ;; If we've already got a sum in the numerator, it seems to be
                ;; better not to recombine it. Call LIMIT on the whole lot, though,
                ;; because terms with infinite limits might cancel to give a finite
                ;; result.
                (t
                 (m+l (mapcar #'(lambda (x)
                                  (sratsimp (m// x d)))
                              (cdr n)))))))

    (limit expr var val 'think)))

;; Heuristics for picking the right way to express a LHOSPITAL problem.
(defun lhop-numden (num denom)
  (declare (special var))
  (cond ((let ((log-num (involve num '(%log))))
	   (cond ((null log-num) ())
		 ((lessthan (num-of-logs (factor (sratsimp (sdiff (m^ num -1) var))))
			    (num-of-logs (factor (sratsimp (sdiff num var)))))
		  (psetq num (m^ denom -1) denom (m^ num -1))
		  t)
		 (t t))))
	((let ((log-denom (involve denom '(%log))))
	   (cond ((null log-denom) ())
		 ((lessthan (num-of-logs (sratsimp (sdiff (m^ denom -1) var)))
			    (num-of-logs (sratsimp (sdiff denom var))))
		  (psetq denom (m^ num -1) num (m^ denom -1))
		  t)
		 (t t))))
	((let ((exp-num (%einvolve num)))
	   (cond (exp-num
		  (cond ((%e-right-placep exp-num)
			 t)
			(t (psetq num (m^ denom -1)
				  denom (m^ num -1)) t)))
		 (t ()))))
	((let ((exp-den (%einvolve denom)))
	   (cond (exp-den
		  (cond ((%e-right-placep exp-den)
			 t)
			(t (psetq num (m^ denom -1)
				  denom (m^ num -1)) t)))
		 (t ()))))
	((let ((scnum (involve num '(%sin))))
	   (cond (scnum (cond ((trig-right-placep '%sin scnum) t)
			      (t (psetq num (m^ denom -1)
					denom (m^ num -1)) t)))
		 (t ()))))
	((let ((scden (involve denom '(%sin))))
	   (cond (scden (cond ((trig-right-placep '%sin scden) t)
			      (t (psetq num (m^ denom -1)
					denom (m^ num -1)) t)))
		 (t ()))))
	((let ((scnum (involve num '(%asin %acos %atan))))
	   ;; If the numerator contains an inverse trig and the
	   ;; denominator or reciprocal of denominator is polynomial,
	   ;; leave everything as is.  If the inverse trig is moved to
	   ;; the denominator, things get messy, even if the numerator
	   ;; becomes a polynomial.  This is not perfect.
	   (cond ((and scnum (or (polyinx denom var ())
				 (polyinx (m^ denom -1) var ())))
		  t)
		 (t nil))))
	((or (oscip num) (oscip denom)))
	((frac num)
	 (psetq num (m^ denom -1) denom (m^ num -1))))
  (values num denom))

;;i don't know what to do here for some cases, may have to be refined.
(defun num-of-logs (exp)
  (cond ((mapatom exp) 0)
	((equal (caar exp) '%log)
	 (m+ 1 (num-of-log-l (cdr exp))))
	((and (mexptp exp) (mnump (caddr exp)))
	 (m* (simplify `((mabs) ,(caddr exp)))
	     (num-of-logs (cadr exp))))
	(t (num-of-log-l (cdr exp)))))

(defun num-of-log-l (llist)
  (do ((temp llist (cdr temp)) (ans 0))
      ((null temp) ans)
    (setq ans (m+ ans (num-of-logs (car temp))))))

(defun %e-right-placep (%e-arg)
  (let ((%e-arg-diff (sdiff %e-arg var)))
    (cond
      ((free %e-arg-diff var))		;simple cases
      ((or (and (mexptp denom)
		(equal (cadr denom) -1))
	   (polyinx (m^ denom -1) var ()))  ())
      ((let ((%e-arg-diff-lim (ridofab (limit %e-arg-diff var val 'think)))
	     (%e-arg-exp-lim (ridofab (limit (m^ '$%e %e-arg) var val 'think))))
	 #+nil
	 (progn
	   (format t "%e-arg-dif-lim = ~A~%" %e-arg-diff-lim)
	   (format t "%e-arg-exp-lim = ~A~%" %e-arg-exp-lim))
	 (cond ((equal %e-arg-diff-lim %e-arg-exp-lim)
		t)
	       ((and (mnump %e-arg-diff-lim) (mnump %e-arg-exp-lim))
		t)
	       ((and (mnump %e-arg-diff-lim) (infinityp %e-arg-exp-lim))
		;; This is meant to make maxima handle bug 1469411
		;; correctly.  Undoubtedly, this needs work.
		t)
	       (t ())))))))

(defun trig-right-placep (trig-type arg)
  (let ((arglim (ridofab (limit arg var val 'think)))
	(triglim (ridofab (limit `((,trig-type) ,arg) var val 'think))))
    (cond ((and (equal arglim 0) (equal triglim 0))  t)
	  ((and (infinityp arglim)  (infinityp triglim))  t)
	  (t ()))))

;;Takes a numerator and a denominator. If they tries all combinations of
;;products to try and make a simpler set of subproblems for LHOSPITAL.
(defun remove-singularities (numer denom)
  (cond ((or (null numer) (null denom)
            (atom numer) (atom denom)
            (not (mtimesp numer))		;Leave this here for a while.
            (not (mtimesp denom)))
         (values 1 numer denom))
        (t
         (let ((const 1))
           (multiple-value-bind (num-consts num-vars)
               (var-or-const numer)
             (multiple-value-bind (denom-consts denom-vars)
                 (var-or-const denom)
               (if (not (mtimesp num-vars))
                   (setq num-vars (list num-vars))
                   (setq num-vars (cdr num-vars)))
               (if (not (mtimesp denom-vars))
                   (setq denom-vars (list denom-vars))
                   (setq denom-vars (cdr denom-vars)))
               (do ((nl num-vars (cdr nl))
                    (num-list (copy-list num-vars ))
                    (den-list denom-vars den-list-temp)
                    (den-list-temp (copy-list denom-vars)))
                   ((null nl) (values (m* const (m// num-consts denom-consts))
                                      (m*l num-list)
                                      (m*l den-list-temp)))
                 (do ((dl den-list (cdr dl)))
                     ((null dl) t)
                   (if (or (%einvolve (car nl)) (%einvolve (car nl)))
                       t
                       (let ((lim (catch 'limit (simpinf (simpab (limit (m// (car nl) (car dl))
                                                                        var val 'think))))))
                         (cond ((or (eq lim t)
                                   (eq lim ())
                                   (equal (ridofab lim) 0)
                                   (infinityp lim)
                                   (not (free lim '$inf))
                                   (not (free lim '$minf))
                                   (not (free lim '$infinity))
                                   (not (free lim '$ind))
                                   (not (free lim '$und)))
                                ())
                               (t
                                (setq const (m* lim const))
                                (setq num-list (delete (car nl) num-list :count 1 :test #'equal))
                                (setq den-list-temp (delete (car dl) den-list-temp :count 1 :test #'equal))
                                (return t)))))))))))))

;; separate terms that contain var from constant terms 
;; returns (const-terms . var-terms)
(defun var-or-const (expr)
  (setq expr ($factor expr))
  (cond ((atom expr)
	 (if (eq expr var)
             (values 1 expr)
             (values expr 1)))
	((free expr var)
         (values expr 1))
	((mtimesp expr)
	 (do ((l (cdr expr) (cdr l))
	      (const 1)
              (varl 1))
	     ((null l) (values const varl))
	   (if (free (car l) var)
               (setq const (m* (car l) const))
               (setq varl (m* (car l) varl)))))
	(t
         (values 1 expr))))

;; if term goes to non-zero constant, replace with constant
(defun lhsimp (term var val)
  (cond (($mapatom term) term)
	(t
	 (let ((term-value (ridofab (limit term var val 'think))))
	   (if (and (not (member term-value '($inf $minf $und $ind $infinity)))
	            (eq t (mnqp term-value 0))) term-value term)))))

(defun bylog (expo bas)
  (simplimexpt '$%e
	       (setq bas
		     (try-lhospital-quit (simplify `((%log) ,(tansc bas)))
					 (m^ expo -1)
					 nil))
	       '$%e bas))

(defun simplimexpt (bas expo bl el)
  (cond ((or (eq bl '$und) (eq el '$und)) '$und)
        ((zerop2 bl)
         (cond ((eq el '$inf) (if (eq bl '$zeroa) bl 0))
               ((eq el '$minf) (if (eq bl '$zeroa) '$inf '$infinity))
               ((eq el '$ind) '$ind)
               ((eq el '$infinity) '$und)
               ((zerop2 el)  (bylog expo bas))
               (t (cond ((equal (getsignl el) -1)
                         (cond ((eq bl '$zeroa) '$inf)
                               ((eq bl '$zerob)
                                (cond ((even1 el) '$inf)
                                      ((eq (ask-integer el '$integer) '$yes)
                                       (if (eq (ask-integer el '$even) '$yes)
                                           '$inf
                                           '$minf)))) ;Gotta be ODD.
                               (t (setq bas (behavior bas var val))
                                  (cond ((equal bas 1) '$inf)
                                        ((equal bas -1) '$minf)
                                        (t (throw 'limit t))))))
                        ((and (mnump el)
                            (member bl '($zeroa $zerob) :test #'eq))
                         (cond ((even1 el) '$zeroa)
                               ((and (eq bl '$zerob)
                                   (ratnump el)
                                   (evenp (caddr el))) 0)
                               (t bl)))
                        ((equal (getsignl el) 1)
                            (if (eq bl '$zeroa) bl 0))
                        ((equal (getsignl el) 0)
                         1)
                        (t (throw 'limit t))))))
        ((eq bl '$infinity)
         (cond ((zerop2 el) (bylog expo bas))
               ((eq el '$minf) 0)
               ((eq el '$inf) '$infinity)
               ((member el '($infinity $ind) :test #'eq) '$und)
               ((equal (setq el (getsignl el)) 1) '$infinity)
               ((equal el 0) 1)
	       ((equal el -1) 0)
	       (t (throw 'limit t))))
	((eq bl '$inf)
         (cond ((eq el '$inf) '$inf)
               ((equal el '$minf) 0)
               ((zerop2 el) (bylog expo bas))
               ((member el '($infinity $ind) :test #'eq) '$und)
               (t (cond ((eql 0 (getsignl el)) 1)
                        ((ratgreaterp 0 el) '$zeroa)
                        ((ratgreaterp el 0) '$inf)
			(t (throw 'limit t))))))
        ((eq bl '$minf)
         (cond ((zerop2 el) (bylog expo bas))
               ((eq el '$inf) '$und)
               ((equal el '$minf) 0)
;;;Why not generalize this. We can ask about the number. -Jim 2/23/81
               ((mnump el)  (cond ((mnegp el)
                                   (if (even1 el)
                                       '$zeroa
                                       (if (eq (ask-integer el '$integer) '$yes)
                                           (if (eq (ask-integer el '$even) '$yes)
                                               '$zeroa
                                               '$zerob)
                                           0)))
                                  (t (cond ((even1 el) '$inf)
                                           ((eq (ask-integer el '$integer) '$yes)
                                            (if (eq (ask-integer el '$even) '$yes)
                                                '$inf
                                                '$minf))
                                           (t '$infinity)))))
               (loginprod? (throw 'lip? 'lip!))
               (t '$und)))
        ((equal (simplify (ratdisrep (ridofab bl))) 1)
         (if (infinityp el) (bylog expo bas) 1))
        ((and (equal (ridofab bl) -1)
            (infinityp el))  '$ind)	;LB
        ((eq bl '$ind)  (cond ((or (zerop2 el) (infinityp el)) '$und)
                              ((not (equal (getsignl el) -1)) '$ind)
                              (t '$und)))
        ((eq el '$inf)  (cond ((abeq1 bl)
                               (if (equal (getsignl bl) 1) 1 '$ind))
                              ((abless1 bl)
                               (if (equal (getsignl bl) 1) '$zeroa 0))
                              ((equal (getsignl (m1- bl)) 1) '$inf)
                              ((equal (getsignl (m1- `((mabs) ,bl))) 1) '$infinity)
                              (t (throw 'limit t))))
        ((eq el '$minf)  (cond ((abeq1 bl)
                                (if (equal (getsignl bl) 1) 1 '$ind))
                               ((not (abless1 bl))
                                (if (equal (getsignl bl) 1) '$zeroa 0))
                               ((ratgreaterp 0 bl)  '$infinity)
                               (t '$inf)))
        ((eq el '$infinity)
         (if (equal val '$infinity)
             '$und			      ;Not enough info to do anything.
             (destructuring-bind (real-el . imag-el)
                 (trisplit expo)
               (setq real-el (limit real-el var origval nil))
               (cond ((eq real-el '$minf)
                      0)
                     ((and (eq real-el '$inf)
                         (not (equal (ridofab (limit imag-el var origval nil)) 0)))
                      '$infinity)
                     ((eq real-el '$infinity)
                      (throw 'limit t))	;; don't really know real component
                     (t
                      '$ind)))))

        ((eq el '$ind)  '$ind)
        ((zerop2 el) 1)
        (t (m^ bl el))))

(defun even1 (x)
  (cond ((numberp x) (and (integerp x) (evenp x)))
	((and (mnump x) (evenp (cadr x))))))

;; is absolute value less than one?
(defun abless1 (bl)
  (setq bl (nmr bl))
  (cond ((mnump bl)
	 (and (ratgreaterp 1. bl) (ratgreaterp bl -1.)))
	(t (equal (getsignl (m1- `((mabs) ,bl))) -1.))))

;; is absolute value equal to one?
(defun abeq1 (bl)
  (setq bl (nmr bl))
  (cond ((mnump bl)
	 (or (equal 1. bl) (equal bl -1.)))
	(t (equal (getsignl (m1- `((mabs) ,bl))) 0))))

(defun simplimit (exp var val &aux op)
  (cond
    ((eq var exp) val)
    ((or (atom exp) (mnump exp)) exp)
    ;; Lookup and dispatch a simplim%function from the property list  
    ((setq op (safe-get (mop exp) 'simplim%function))
     (funcall op exp var val))

    ;; And do the same for subscripted:
	((and (consp exp) (consp (car exp)) (eq (caar exp) 'mqapply)
		  (setq op (safe-get (subfunname exp) 'simplim%function)))
	  (funcall op exp var val))	  

    ;; Without the call to rootscontract, 
	;;   limit(((-4)*x^2-10*x+24)/((4*x+8)^(1/3)+2),x,-4)
	;; returns zero (should be 66). This bug is due to the fact that
	;; 4^(1/3)*2^(1/3)-2 does not simplify to zero. Although the call
	;; to rootscontract takes care of this case, almost surely there are
	;; many other limit problems that need more than rootscontract.
    ((mplusp exp) (let (($rootsconmode nil)) ($rootscontract (simplimplus exp))))
    ((mtimesp exp)  (simplimtimes (cdr exp)))
    ((mexptp exp)  (simplimexpt (cadr exp) (caddr exp)
				(limit (cadr exp) var val 'think)
				(limit (caddr exp) var val 'think)))
    ((member (caar exp) '(%sin %cos) :test #'eq)
     (simplimsc exp (caar exp) (limit (cadr exp) var val 'think)))
    ((eq (caar exp) '%tan) (simplim%tan (cadr exp)))
    ((member (caar exp) '(%sinh %cosh) :test #'eq)
     (simplimsch (caar exp) (limit (cadr exp) var val 'think)))
    ((member (caar exp) '(%erf %tanh) :test #'eq)
     (simplim%erf-%tanh (caar exp) (cadr exp)))
    ((eq (caar exp) '%atanh)
     (simplim%atanh (limit (cadr exp) var val 'think) val))
    ((eq (caar exp) '%acosh)
     (simplim%acosh (limit (cadr exp) var val 'think)))
    ((eq (caar exp) '%asinh)
     (simplim%asinh (limit (cadr exp) var val 'think)))
    ((eq (caar exp) '%inverse_jacobi_ns)
     (simplim%inverse_jacobi_ns (limit (cadr exp) var val 'think) (third exp)))
    ((eq (caar exp) '%inverse_jacobi_nc)
     (simplim%inverse_jacobi_nc (limit (cadr exp) var val 'think) (third exp)))
    ((eq (caar exp) '%inverse_jacobi_sc)
     (simplim%inverse_jacobi_sc (limit (cadr exp) var val 'think) (third exp)))
    ((eq (caar exp) '%inverse_jacobi_cs)
     (simplim%inverse_jacobi_cs (limit (cadr exp) var val 'think) (third exp)))
    ((eq (caar exp) '%inverse_jacobi_dc)
     (simplim%inverse_jacobi_dc (limit (cadr exp) var val 'think) (third exp)))
    ((eq (caar exp) '%inverse_jacobi_ds)
     (simplim%inverse_jacobi_ds (limit (cadr exp) var val 'think) (third exp)))
    ((and (eq (caar exp) 'mqapply)
	  (eq (subfunname exp) '$psi))
     (simplim$psi (subfunsubs exp) (subfunargs exp) val))
    ((and (eq (caar exp) var)
	  (member 'array (car exp) :test #'eq)
	  (every #'(lambda (sub-exp)
		       (free sub-exp var))
		   (cdr exp)))
     exp)				;LIMIT(B[I],B,INF); -> B[I]
	;; When limsubst is true, limit(f(n+1)/f(n),n,inf) = 1. The user documentation
	;; warns against setting limsubst to true. 
	(t (if $limsubst
	   (simplify (cons (operator-with-array-flag exp)
			   (mapcar #'(lambda (a)
				       (limit a var val 'think))
				   (cdr exp))))
	   (throw 'limit t))))) 
  
(defun liminv (e)
  (setq e (resimplify (subst (m// 1 var) var e)))
  (let ((new-val (cond ((eq val '$zeroa)  '$inf)
		       ((eq val '$zerob)  '$minf))))
    (if new-val (let ((preserve-direction t))
		  (toplevel-$limit e var new-val)) (throw 'limit t))))

(defun simplimtimes (exp)
  ;; The following test
  ;; handles         (-1)^x * 2^x => (-2)^x => $infinity
  ;; wants to avoid  (-1)^x * 2^x => $ind * $inf => $und
  (let ((try
         (and (expfactorp (cons '(mtimes) exp) 1)
              (expfactor (cons '(mtimes) exp) 1 var))))
    (when try (return-from simplimtimes try)))

  (let ((prod 1) (num 1) (denom 1)
        (zf nil) (ind-flag nil) (inf-type nil)
        (constant-zero nil) (constant-infty nil))
    (dolist (term exp)
      (let* ((loginprod? (involve term '(%log)))
             (y (catch 'lip? (limit term var val 'think))))
        (cond
          ;; limit failed due to log in product
          ((eq y 'lip!)
           (return-from simplimtimes (liminv (cons '(mtimes simp) exp))))

          ;; If the limit is infinitesimal or zero
          ((zerop2 y)
           (setf num (m* num term)
                 constant-zero (or constant-zero (not (among var term))))
           (case y
             ($zeroa
              (unless zf (setf zf 1)))
             ($zerob
              (setf zf (* -1 (or zf 1))))))

          ;; If the limit is not some form of infinity or
          ;; undefined/indeterminate.
          ((not (member y '($inf $minf $infinity $ind $und) :test #'eq))
           (setq prod (m* prod y)))

          ((eq y '$und) (return-from simplimtimes '$und))
          ((eq y '$ind) (setq ind-flag t))

          ;; Some form of infinity
          (t
           (setf denom (m* denom term)
                 constant-infty (or constant-infty (not (among var term))))
           (unless (eq inf-type '$infinity)
             (cond
               ((eq y '$infinity) (setq inf-type '$infinity))
               ((null inf-type) (setf inf-type y))
               ;; minf * minf or inf * inf
               ((eq y inf-type) (setf inf-type '$inf))
               ;; minf * inf
               (t (setf inf-type '$minf))))))))

    (cond
      ;; If there are zeros and infinities among the terms that are free of
      ;; VAR, then we have an expression like "inf * zeroa * f(x)" or
      ;; similar. That gives an undefined result. Note that we don't
      ;; necessarily have something undefined if only the zeros have a term
      ;; free of VAR. For example "zeroa * exp(-1/x) * 1/x" as x -> 0. And
      ;; similarly for the infinities.
      ((and constant-zero constant-infty) '$und)

      ;; If num=denom=1, we didn't find any explicit infinities or zeros, so we
      ;; either return the simplified product or ind
      ((and (eql num 1) (eql denom 1))
       (if ind-flag '$ind prod))
      ;; If denom=1 (and so num != 1), we have some form of zero
      ((equal denom 1)
       (if (null zf)
           0
           (let ((sign (getsignl prod)))
             (if (or (not sign) (eq sign 'complex))
                 0
                 (ecase (* zf sign)
                   (0 0)
                   (1  '$zeroa)
                   (-1 '$zerob))))))
      ;; If num=1 (and so denom != 1), we have some form of infinity
      ((equal num 1)
       (let ((sign ($csign prod)))
         (cond
           (ind-flag '$und)
           ((eq sign '$pos) inf-type)
           ((eq sign '$neg) (case inf-type
                              ($inf '$minf)
                              ($minf '$inf)
                              (t '$infinity)))
           ((member sign '($complex $imaginary)) '$infinity)
           ; sign is '$zero, $pnz, $pz, etc
           (t (throw 'limit t)))))
      ;; Both zeros and infinities
      (t
       ;; All bets off if there are some infinities or some zeros, but it
       ;; needn't be undefined (see above)
       (when (or constant-zero constant-infty) (throw 'limit t))

       (let ((ans (limit2 num (m^ denom -1) var val)))
         (if ans
             (simplimtimes (list prod ans))
             (throw 'limit t)))))))

;;;PUT CODE HERE TO ELIMINATE FAKE SINGULARITIES??

(defun simplimplus (exp)
  (cond ((memalike exp simplimplus-problems)
	 (throw 'limit t))
	(t (unwind-protect
		(progn (push exp simplimplus-problems)
		       (let ((ans (catch 'limit (simplimplus1 exp))))
			 (cond ((or (eq ans ())
				    (eq ans t)
				    (among '%limit ans))
				(let ((new-exp (sratsimp exp)))
				  (cond ((not (alike1 exp new-exp))
					 (setq ans
					       (limit new-exp var val 'think))))
				  (cond ((or (eq ans ())
					     (eq ans t))
					 (throw 'limit t))
					(t ans))))
			       (t ans))))
	     (pop simplimplus-problems)))))

(defun simplimplus1 (exp)
  (prog (sum y infl infinityl minfl indl undl)
     (setq sum 0.)
     (do ((exp (cdr exp) (cdr exp)) (f))
	 ((or y (null exp)) nil)
       (setq f (limit (car exp) var val 'think))
       (cond ((null f)
	      (throw 'limit t))
	     ((eq f '$und) (push (car exp) undl))
	     ((not (member f '($inf $minf $infinity $ind) :test #'eq))
	      (setq sum (m+ sum f)))
	     ((eq f '$ind)  (push (car exp) indl))
	     (infinityl (throw 'limit t))
;;;Don't know what to do with an '$infinity and an $inf or $minf
	     ((eq f '$inf)  (push (car exp) infl))
	     ((eq f '$minf)  (push (car exp) minfl))
	     ((eq f '$infinity)
	      (cond ((or infl minfl)
		     (throw 'limit t))
		    (t (push (car exp) infinityl))))))
     (cond (undl
	    (if (or infl minfl indl infinityl)
		(setq infinityl (append undl infinityl)); x^2 + x*sin(x)
		(return '$und)))			; 1 + x*sin(x)
	   ((not (or infl minfl indl infinityl))
	    (return (cond ((atom sum)  sum)
			  ((or (not (free sum '$zeroa))
			       (not (free sum '$zerob)))
			   (simpab sum))
			  (t sum))))
	   (t (cond ((null infinityl)
		     (cond (infl (cond ((null minfl) (return '$inf))
				       (t (go oon))))
			   (minfl (return '$minf))
                           ((> (length indl) 1)
                            ;; At this point we have a sum of '$ind. We factor 
                            ;; the sum and try again. This way we get the limit 
                            ;; of expressions like (a-b)*ind, where (a-b)--> 0.
                            (cond ((not (alike1 (setq y ($factorsum exp)) exp))
                                   (return (limit y var val 'think)))
                                  (t
                                   (return '$ind))))		       
			   (t (return '$ind))))
		    (t (setq infl (append infl infinityl))))))

     oon  (setq y (m+l (append minfl infl)))
     (cond ((alike1 exp (setq y (sratsimp (hyperex y))))
	    (cond ((not (infinityp val))
		   (setq infl (cnv infl val)) ;THIS IS HORRIBLE!!!!
		   (setq minfl (cnv minfl val))))
	    (let ((val '$inf))
	      (cond ((every #'(lambda (j) (radicalp j var))
			    (append infl minfl))
		     (setq y (rheur infl minfl)))
		    (t (setq y (sheur infl minfl))))))
	   (t (setq y (limit y var val 'think))))
     (cond ((or (eq y ())
		(eq y t))  (return ()))
	   ((infinityp y)  (return y))
	   (t (return (m+ sum y))))))

;; Limit n/d, using heuristics on the order of growth.
(defun sheur0 (n d)
  (let ((orig-n n))
    (cond ((and (free n var)
		(free d var))
	   (m// n d))
	  (t (setq n (cpa n d nil))
	     (cond ((equal n 1)
		    (cond ((oscip orig-n)  '$und)
			  (t '$inf)))
		   ((equal n -1)  '$zeroa)
		   ((equal n 0)  (m// orig-n d)))))))


;;;L1 is a list of INF's and L2 is a list of MINF's. Added together
;;;it is indeterminate.
(defun sheur (l1 l2)
  (let ((term (sheur1 l1 l2)))
    (cond ((equal term '$inf)  '$inf)
	  ((equal term '$minf)   '$minf)
	  (t (let ((new-num (m+l (mapcar #'(lambda (num-term)
					     (m// num-term (car l1)))
					 (append l1 l2)))))
	       (cond ((limit2 new-num (m// 1 (car l1)) var val))))))))

(defun frac (exp)
  (cond ((atom exp) nil)
	(t (setq exp (nformat exp))
	   (cond ((and (eq (caar exp) 'mquotient)
		       (among var (caddr exp)))
		  t)))))

(defun zerop2 (z) (=0 (ridofab z)))

(defun raise (a) (m+ a '$zeroa))

(defun lower (a) (m+ a '$zerob))

(defun sincoshk (exp1 l sc)
  (cond ((equal l 1) (lower l))
	((equal l -1) (raise l))
	((among sc l) l)
	((member val '($zeroa $zerob) :test #'eq) (spangside exp1 l))
	(t l)))

(defun spangside (e l)
  (setq e (behavior e var val))
  (cond ((equal e 1) (raise l))
	((equal e -1) (lower l))
	(t l)))

;; get rid of zeroa and zerob
(defun ridofab (e)
   (if (among '$zeroa e) (setq e (maxima-substitute 0 '$zeroa e)))
   (if (among '$zerob e) (setq e (maxima-substitute 0 '$zerob e)))
   e)

;; simple radical
;; returns true if exp is a polynomial raised to a numeric power
(defun simplerd (exp)
  (and (mexptp exp)
       (mnump (caddr exp))	;; exponent must be a number - no variables
       (polyp (cadr exp))))

(defun branch1 (exp val)
  (cond ((polyp exp) nil)
	((simplerd exp) (zerop2 (subin val (cadr exp))))
	(t
	 (loop for v on (cdr exp)
		when (branch1 (car v) val)
		do (return v)))))

(defun branch (exp val)
  (cond ((polyp exp) nil)
	((or (simplerd exp) (mtimesp exp))
	 (branch1 exp val))
	((mplusp exp)
	 (every #'(lambda (j) (branch j val)) (the list (cdr exp))))))

(defun ser0 (e n d val)
  (cond ((and (branch n val) (branch d val))
	 (setq nn* nil)
	 (setq n (ser1 n))
	 (setq d (ser1 d))
;;;NN* gets set by POFX, called by SER1, to get a list of exponents.
	 (setq nn* (ratmin nn*))
	 (setq n (sratsimp (m^ n (m^ var nn*))))
	 (setq d (sratsimp (m^ d (m^ var nn*))))
	 (cond ((member val '($zeroa $zerob) :test #'eq) nil)
	       (t (setq val 0.)))
	 (radlim e n d))
	(t (try-lhospital-quit n d nil))))

(defun rheur (l1 l2)
  (prog (ans m1 m2)
     (setq m1 (mapcar (function asymredu) l1))
     (setq m2 (mapcar (function asymredu) l2))
     (setq ans (m+l (append m1 m2)))
     (cond ((rptrouble (m+l (append l1 l2)))
	    (return (limit (simplify (rdsget (m+l (append l1 l2))))
			   var
			   val
			   nil)))
	   ((mplusp ans)  (return (sheur m1 m2)))
	   (t (return (limit ans var val t))))))

(defun rptrouble (rp)
  (not (equal (rddeg rp nil) (rddeg (asymredu rp) nil))))

(defun radicalp (exp var)
  (cond ((polyinx exp var ()))
	((mexptp exp)  (cond ((equal (caddr exp) -1.)
			      (radicalp (cadr exp) var))
			     ((simplerd exp))))
	((member (caar exp) '(mplus mtimes) :test #'eq)
	 (every #'(lambda (j) (radicalp j var))
		(cdr exp)))))

(defun involve (e nn*)
  (declare (special var))
  (cond ((atom e) nil)
	((mnump e) nil)
	((and (member (caar e) nn* :test #'eq) (among var (cdr e))) (cadr e))
	(t (some #'(lambda (j) (involve j nn*)) (cdr e)))))

(defun notinvolve (exp nn*)
  (cond ((atom exp) t)
	((mnump exp) t)
	((member (caar exp) nn* :test #'eq) (not (among var (cdr exp))))
	((every #'(lambda (j) (notinvolve j nn*))
		(cdr exp)))))

(defun sheur1 (l1 l2)
  (prog (ans)
     (setq l1 (mapcar #'stirling0 l1))
     (setq l2 (mapcar #'stirling0 l2))
     (setq l1 (m+l (maxi l1)))
     (setq l2 (m+l (maxi l2)))
     (setq ans (cpa l1 l2 t))
     (return (cond ((=0 ans)  (m+  l1 l2))
		   ((equal ans 1.) '$inf)
		   (t '$minf)))))
			  
(defun zero-lim (cpa-list)
  (do ((l cpa-list (cdr l)))
      ((null l) ())
    (and (eq (caar l) 'gen)
	 (zerop2 (limit (cadar l) var val 'think))
	 (return t))))

;; Compare order of growth for R1 and R2.  The result is 0, -1, +1
;; depending on the relative order of growth.  0 is returned if R1 and
;; R2 have the same growth; -1 if R1 grows much more slowly than R2;
;; +1 if R1 grows much more quickly than R2.
(defun cpa (r1 r2 flag)
  (let ((t1 r1)
	(t2 r2))
    (cond ((alike1 t1 t2)   0.)
	  ((free t1 var)
	   (cond ((free t2 var)  0.)
		 (t (let ((lim-ans (limit1 t2 var val)))
		      (cond ((not (member lim-ans '($inf $minf $und $ind) :test #'eq))  0.)
			    (t  -1.))))))
	  ((free t2 var)
	   (let ((lim-ans (limit1 t1 var val)))
	     (cond ((not (member lim-ans '($inf $minf $und $ind) :test #'eq))  0.)
		   (t  1.))))
	  (t
	   ;; Make T1 and T2 be a list of terms that are multiplied
	   ;; together.
	   (cond ((mtimesp t1)  (setq t1 (cdr t1)))
		 (t (setq t1 (list t1))))
	   (cond ((mtimesp t2)  (setq t2 (cdr t2)))
		 (t (setq t2 (list t2))))
	   ;; Find the strengths of each term of T1 and T2
	   (setq t1 (mapcar (function istrength) t1))
	   (setq t2 (mapcar (function istrength) t2))
	   ;; Compute the max of the strengths of the terms.
	   (let ((ans (ismax t1))
		 (d (ismax t2)))
	     (cond ((or (null ans) (null d))
		    ;;(eq (car ans) 'gen) (eq (car d) 'gen))
		    ;; ismax couldn't find highest term; give up
		    0.)
		   (t
		    (if (eq (car ans) 'var)  (setq ans (add-up-deg t1)))
		    (if (eq (car d) 'var)  (setq d (add-up-deg t2)))
		    ;; Can't just just compare dominating terms if there are
		    ;; indeterm-inates present; e.g. X-X^2*LOG(1+1/X). So
		    ;; check for this.
		    (cond ((or (zero-lim t1)
			       (zero-lim t2))
			   (cpa-indeterm ans d t1 t2 flag))
			  ((isgreaterp ans d)  1.)
			  ((isgreaterp d ans)  -1.)
			  (t  0)))))))))

(defun cpa-indeterm (ans d t1 t2 flag)
  (cond ((not (eq (car ans) 'var))
	 (setq ans (gather ans t1)  d (gather d t2))))
  (let ((*indicator (and (eq (car ans) 'exp)
			 flag))
	(test ()))
    (setq test (cpa1 ans d))
    (cond ((and (zerop1 test)
		(or (equal ($radcan (m// (cadr ans) (cadr d))) 1.)
		    (and (polyp (cadr ans))
			 (polyp (cadr d))
			 (equal (limit (m// (cadr ans) (cadr d)) var val 'think)
				1.))))
	   (let ((new-term1 (m// t1 (cadr ans)))
		 (new-term2 (m// t2 (cadr d))))
	     (cpa new-term1 new-term2 flag)))
	  (t 0))))

(defun add-up-deg (strengthl)
  (do ((stl strengthl (cdr stl))
       (poxl)
       (degl))
      ((null stl) (list 'var (m*l poxl) (m+l degl)))
    (cond ((eq (caar stl) 'var)
	   (push (cadar stl) poxl)
	   (push (caddar stl) degl)))))

(defun cpa1 (p1 p2)
  (prog (flag s1 s2)
     (cond ((eq (car p1) 'gen) (return 0.)))
     (setq flag (car p1))
     (setq p1 (cadr p1))
     (setq p2 (cadr p2))
     (cond
       ((eq flag 'var)
	(setq s1 (istrength p1))
	(setq s2 (istrength p2))
	(return
	  (cond
	    ((isgreaterp s1 s2) 1.)
	    ((isgreaterp s2 s1) -1.)
	    (*indicator
	     (setq *indicator nil)
	     (cond
	       ((and (poly? p1 var) (poly? p2 var))
		(setq p1 (m- p1 p2))
		(cond ((zerop1 p1) 0.)
		      (t (getsignl (hot-coef p1)))))
	       (t
		(setq s1
		      (rheur (list p1)
			     (list (m*t -1 p2))))
		(cond ((zerop2 s1) 0.)
		      ((ratgreaterp s1 0.) 1.)
		      (t -1.)))))
	    (t 0.))))
       ((eq flag 'exp)
	(setq p1 (caddr p1))
	(setq p2 (caddr p2))
	(cond ((and (poly? p1 var) (poly? p2 var))
	       (setq p1 (m- p1 p2))
	       (return (cond ((or (zerop1 p1)
				  (not (among var p1)))
			      0.)
			     (t (getsignl (hot-coef p1))))))
	      ((and (radicalp p1 var) (radicalp p2 var))
	       (setq s1
		     (rheur (list p1)
			    (list (m*t -1 p2))))
	       (return (cond ((eq s1 '$inf) 1.)
			     ((eq s1 '$minf) -1.)
			     ((mnump s1)
			      (cond ((ratgreaterp s1 0.) 1.)
				    ((ratgreaterp 0. s1) -1.)
				    (t 0.)))
			     (t 0.))))
	      (t (return (cpa p1 p2 t)))))
       ((eq flag 'log)
	(setq p1 (try-lhospital (asymredu p1) (asymredu p2) nil))
	(return (cond ((zerop2 p1) -1.)
		      ((real-infinityp p1) 1.)
		      (t 0.)))))))

;;;EXPRESSIONS TO ISGREATERP ARE OF THE FOLLOWING FORMS
;;;	("VAR" POLY DEG)
;;;	("EXP" %E^EXP)
;;;	("LOG" LOG(EXP))
;;;	("FACT"	<A FACTORIAL EXPRESSION>)
;;;	("GEN" <ANY OTHER TYPE OF EXPRESSION>)

(defun isgreaterp (a b)
  (let ((ta (car a))
	(tb (car b)))
    (cond ((or (eq ta 'gen)
	       (eq tb 'gen))  ())
	  ((and (eq ta tb) (eq ta 'var))
	   (ratgreaterp (caddr a) (caddr b)))
	  ((and (eq ta tb) (eq ta 'exp))
	   ;; Both are exponential order of infinity.  Check the
	   ;; exponents to determine which exponent is bigger.
	   (eq (limit (m- `((%log) ,(second a)) `((%log) ,(second b)))
		      var val 'think)
	       '$inf))
	  ((member ta (cdr (member tb '(num log var exp fact gen) :test #'eq)) :test #'eq)))))

(defun ismax (l)
  ;; Preprocess the list of products.  Separate the terms that
  ;; exponentials and those that don't.  Actually multiply the
  ;; exponential terms together to form a single term.  Pass this and
  ;; the rest to ismax-core to find the max.
  (let (exp-terms non-exp-terms)
    (dolist (term l)
      (if (eq 'exp (car term))
	  (push term exp-terms)
	  (push term non-exp-terms)))
    ;; Multiply the exp-terms together
    (if exp-terms
	(let ((product 1))
	  ;;(format t "exp-terms = ~A~%" exp-terms)
	  (dolist (term exp-terms)
	    (setf product (simplify (mul product (second term)))))
	  ;;(format t "product = ~A~%" product)
	  (setf product `(exp ,($logcontract product)))
	  ;;(format t "product = ~A~%" product)
	  (ismax-core (cons product non-exp-terms)))
	(ismax-core l))))

(defun ismax-core (l)
  (cond ((null l)  ())
	((atom l)   ())
	((= (length l) 1)  (car l)) ;If there is only 1 thing give it back.
	((every #'(lambda (x)
		      (not (eq (car x) 'gen))) l)

	 (do ((l1 (cdr l) (cdr l1))
	      (temp-ans (car l))
	      (ans ()))
	     ((null l1) ans)
	   (cond ((isgreaterp temp-ans (car l1))
		  (setq ans temp-ans))
		 ((isgreaterp (car l1) temp-ans)
		  (setq temp-ans (car l1))
		  (setq ans temp-ans))
		 (t (setq ans ())))))
	(t ())))

;RETURNS LIST OF HIGH TERMS
(defun maxi (all)
  (cond ((atom all)  nil)
	(t (do ((l (cdr all) (cdr l))
		(hi-term (car all))
		(total 1)		; running total constant factor of hi-term
		(hi-terms (ncons (car all)))
		(compare nil))
	       ((null l) (if (zerop2 total)  ; if high-order terms cancel each other
			     all	     ; keep everything
			   hi-terms))        ; otherwise return list of high terms
	     (setq compare (limit (m// (car l) hi-term) var val 'think))
	     (cond
	       ((or (infinityp compare)
		    (and (eq compare '$und)
			 (zerop2 (limit (m// hi-term (car l)) var val 'think))))
		(setq total 1)	; have found new high term
		(setq hi-terms (ncons (setq hi-term (car l)))))
	       ((zerop2 compare)  nil)
	       ;; COMPARE IS IND, FINITE-VALUED, or und in both directions
	       (t		; add to list of high terms
		(setq total (m+ total compare))
		(setq hi-terms (append hi-terms (ncons (car l))))))))))

(defun ratmax (l)
  (prog (ans)
     (cond ((atom l) (return nil)))
     l1   (setq ans (car l))
     l2   (setq l (cdr l))
     (cond ((null l) (return ans))
	   ((ratgreaterp ans (car l)) (go l2))
	   (t (go l1)))))

(defun ratmin (l)
  (prog (ans)
     (cond ((atom l) (return nil)))
     l1   (setq ans (car l))
     l2   (setq l (cdr l))
     (cond ((null l) (return ans))
	   ((ratgreaterp (car l) ans) (go l2))
	   (t (go l1)))))

(defun pofx (e)
  (cond ((atom e)
	 (cond ((eq e var)
		(push 1 nn*))
	       (t ())))
	((or (mnump e) (not (among var e))) nil)
	((and (mexptp e) (eq (cadr e) var))
	 (push (caddr e) nn*))
	((simplerd e) nil)
	(t (mapc #'pofx (cdr e)))))

(defun ser1 (e)
  (cond ((member val '($zeroa $zerob) :test #'eq) nil)
	(t (setq e (subin (m+ var val) e))))
  (setq e (rdfact e))
  (cond ((pofx e) e)))

(defun gather (ind l)
  (prog (ans)
     (setq ind (car ind))
     loop (cond ((null l)
		 (return (list ind (m*l ans))))
		((equal (caar l) ind)
		 (push (cadar l) ans)))
     (setq l (cdr l))
     (go loop)))

; returns rough class-of-growth of term
(defun istrength (term)
  (cond ((mnump term)  (list 'num term))
	((atom term)   (cond ((eq term var)
			      (list 'var var 1.))
			     (t (list 'num term))))
	((not (among var term))  (list 'num term))
	((mplusp term)
	 (let ((temp (ismax-core (mapcar #'istrength (cdr term)))))
	   (cond ((not (null temp))  temp)
		 (t `(gen ,term)))))
	((mtimesp term)
	 (let ((temp (mapcar #'istrength (cdr term)))
	       (temp1 ()))
	   (setq temp1 (ismax temp))
	   (cond ((null temp1)  `(gen ,term))
		 ((eq (car temp1) 'log)  `(log ,temp))
		 ((eq (car temp1) 'var)  (add-up-deg temp))
		 (t `(gen ,temp)))))
	((and (mexptp term)
	      (real-infinityp (limit term var val t)))
	 (let ((logterm (logred term)))
	   (cond ((and (among var (caddr term))
		       (member (car (istrength logterm))
			       '(var exp fact) :test #'eq)
		       (real-infinityp (limit logterm var val t)))
		  (list 'exp (m^ '$%e logterm)))
		 ((not (among var (caddr term)))
		  (let ((temp (istrength (cadr term))))
		    (cond ((not (alike1 temp term))
			   (rplaca (cdr temp) term)
			   (and (eq (car temp) 'var)
				(rplaca (cddr temp)
					(m* (caddr temp) (caddr term))))
			   temp)
			  (t `(gen ,term)))))
		 (t `(gen ,term)))))
	((and (eq (caar term) '%log)
	      (real-infinityp (limit term var val t)))
	 (let ((stren (istrength (cadr term))))
	   (cond ((member (car stren) '(log var) :test #'eq)
		  `(log ,term))
		 ((and (eq (car stren) 'exp)
		       (eq (caar (second stren)) 'mexpt))
		  (istrength (logred (second stren))))
		 (t `(gen ,term)))))
	((eq (caar term) 'mfactorial)
	 (list 'fact term))
	((let ((temp (hyperex term)))
	   (and (not (alike1 term temp))
		(istrength temp))))
	(t (list 'gen term))))

;; log reduce - returns log of s1
(defun logred (s1)
  (or (and (eq (cadr s1) '$%e) (caddr s1))
      (m* (caddr s1) `((%log) ,(cadr s1)))))

(defun asymredu (rd)
  (cond ((atom rd) rd)
	((mnump rd) rd)
	((not (among var rd)) rd)
	((polyinx rd var t))
	((simplerd rd)
	 (cond ((eq (cadr rd) var) rd)
	       (t (mabs-subst
		   (factor ($expand (m^ (polyinx (cadr rd) var t)
					(caddr rd))))
		   var
		   val))))
	(t (simplify (cons (list (caar rd))
			   (mapcar #'asymredu (cdr rd)))))))

(defun rdfact (rd)
  (let ((dn** ())  (nn** ()))
    (cond ((atom rd) rd)
	  ((mnump rd) rd)
	  ((not (among var rd)) rd)
	  ((polyp rd)
	   (factor rd))
	  ((simplerd rd)
	   (cond ((eq (cadr rd) var) rd)
		 (t (setq dn** (caddr rd))
		    (setq nn** (factor (cadr rd)))
		    (cond ((mtimesp nn**)
			   (m*l (mapcar #'(lambda (j) (m^ j dn**))
					(cdr nn**))))
			  (t rd)))))
	  (t (simplify (cons (ncons (caar rd))
			     (mapcar #'rdfact (cdr rd))))))))

(defun cnv (expl val)
  (mapcar #'(lambda (e)
	      (maxima-substitute (cond ((eq val '$zerob)
					(m* -1 (m^ var -1)))
				       ((eq val '$zeroa)
					(m^ var -1))
				       ((eq val '$minf)
					(m* -1 var))
				       (t (m^ (m+ var (m* -1 val)) -1.)))
				 var
				 e))
	  expl))

(defun pwtaylor (exp var l terms)
  (prog (coef ans c mc)
     (cond ((=0 terms) (return nil)) ((=0 l) (setq mc t)))
     (setq c 0.)
     (go tag1)
     loop (setq c (1+ c))
     (cond ((or (> c 10.) (equal c terms))
	    (return (m+l ans)))
	   (t (setq exp (sdiff exp var))))
     tag1 (setq coef ($radcan (subin l exp)))
     (cond ((=0 coef) (setq terms (1+ terms)) (go loop)))
     (setq
      ans
      (append
       ans
       (list
	(m* coef
	    (m^ `((mfactorial) ,c) -1)
	    (m^ (if mc var (m+t (m*t -1 l) var)) c)))))
     (go loop)))

(defun rdsget (e)
  (cond ((polyp e) e)
	((simplerd e) (rdtay e))
	(t (cons (list (caar e))
		 (mapcar #'rdsget (cdr e))))))

(defun rdtay (rd)
  (cond (limit-using-taylor ($ratdisrep ($taylor rd var val 1.)))
	(t (lrdtay rd))))

(defun lrdtay (rd)
  (prog (varlist p c e d $ratfac)
     (setq varlist (ncons var))
     (setq p (ratnumerator (cdr (ratrep* (cadr rd)))))
     (cond ((< (length p) 3.) (return rd)))
     (setq e (caddr rd))
     (setq d (pdegr p))
     (setq c (m^ var (m* d e)))
     (setq d ($ratsimp (varinvert (m* (pdis p) (m^ var (m- d)))
				  var)))
     (setq d (pwtaylor (m^ d e) var 0. 3.))
     (return (m* c (varinvert d var)))))

(defun varinvert (e var) (subin (m^t var -1.) e))

(defun deg (p)
  (prog ((varlist (list var)))
     (return (let (($ratfac nil))
	       (newvar p)
	       (pdegr (cadr (ratrep* p)))))))

(defun rat-no-ratfac (e)
  (let (($ratfac nil))
    (newvar e)
    (ratrep* e)))
(setq low* nil)

(defun rddeg (rd low*)
  (cond ((or (mnump rd)
	     (not (among var rd)))
	 0)
	((polyp rd)
	 (deg rd))
	((simplerd rd)
	 (m* (deg (cadr rd)) (caddr rd)))
	((mtimesp rd)
	 (addn (mapcar #'(lambda (j)
			   (rddeg j low*))
		       (cdr rd)) nil))
	((and (mplusp rd)
	      (setq rd (andmapcar #'(lambda (j) (rddeg j low*))
				  (cdr rd))))
	 (cond (low* (ratmin rd))
	       (t (ratmax rd))))))

(defun pdegr (pf)
  (cond ((or (atom pf) (not (eq (caadr (ratf var)) (car pf))))
	 0)
	(low* (cadr (reverse pf)))
	(t (cadr pf))))
;;There is some confusion here. We need to be aware of Branch cuts etc....
;;when doing this section of code. It is not very carefully done so there
;;are bugs still lurking. Another misfortune is that LIMIT or its inferiors
;;sometimes decides to change the limit VAL in midstream. This must be corrected
;;since LIMIT's interaction with the data base environment must be maintained.
;;I'm not sure that this code can ever be called with VAL other than $INF but
;;there is a hook in the first important cond clause to cathc them anyway.

(defun asy (n d)
  (let ((num-power (rddeg n nil))
	(den-power (rddeg d nil))
	(coef ())  (coef-sign ())  (power ()))
    (setq coef (m// ($ratcoef ($expand n) var num-power)
		    ($ratcoef ($expand d) var den-power)))
    (setq coef-sign (getsignl coef))
    (setq power (m// num-power den-power))
    (cond ((eq (ask-integer power '$integer) '$integer)
	   (cond ((eq (ask-integer power '$even) '$even)  '$even)
		 (t   '$odd))))		;Can be extended from here.
    (cond ((or (eq val '$minf)
	       (eq val '$zerob)
	       (eq val '$zeroa)
	       (equal val 0))  ()) ;Can be extended to cover some these.
	  ((ratgreaterp den-power num-power)
	   (cond ((equal coef-sign 1.)  '$zeroa)
		 ((equal coef-sign -1)  '$zerob)
		 ((equal coef-sign 0)   0)
		 (t 0)))
	  ((ratgreaterp num-power den-power)
	   (cond ((equal coef-sign 1.)  '$inf)
		 ((equal coef-sign -1)  '$minf)
		 ((equal coef-sign 0)   nil) ; should never be zero
		 ((null coef-sign)   '$infinity)))
	  (t coef))))

(defun radlim (e n d)
  (prog (nl dl)
     (cond ((eq val '$infinity) (throw 'limit nil))
	   ((eq val '$minf)
	    (setq nl (m* var -1))
	    (setq n (subin nl n))
	    (setq d (subin nl d))
	    (setq val '$inf))) ;This is the Culprit. Doesn't tell the DATABASE.
     (cond ((eq val '$inf)
	    (setq nl (asymredu n))
	    (setq dl (asymredu d))
	    (cond
	      ((or (rptrouble n) (rptrouble d))
	       (return (limit (m* (rdsget n) (m^ (rdsget d) -1.)) var val t)))
	      (t (return (asy nl dl))))))
     (setq nl (limit n var val t))
     (setq dl (limit d var val t))
     (cond ((and (zerop2 nl) (zerop2 dl))
	    (cond ((or (polyp n) (polyp d))
		   (return (try-lhospital-quit n d t)))
		  (t (return (ser0 e n d val)))))
	   (t (return ($radcan (ratrad (m// n d) n d nl dl)))))))

(defun ratrad (e n d nl dl)
  (prog (n1 d1)
     (cond ((equal nl 0) (return 0))
	   ((zerop2 dl)
	    (setq n1 nl)
	    (cond ((equal dl 0) (setq d1 '$infinity)) ;No direction Info.
		  ((eq dl '$zeroa)
		   (setq d1 '$inf))
		  ((equal (setq d (behavior d var val)) 1)
		   (setq d1 '$inf))
		  ((equal d -1) (setq d1 '$minf))
		  (t (throw 'limit nil))))
	   ((zerop2 nl)
	    (setq d1 dl)
	    (cond ((equal (setq n (behavior n var val)) 1)
		   (setq n1 '$zeroa))
		  ((equal n -1) (setq n1 '$zerob))
		  (t (setq n1 0))))
	   (t (return ($radcan (ridofab (subin val e))))))
     (return (simplimtimes (list n1 d1)))))

;;; Limit(log(XXX), var, 0, val), where val is either zerob (limit from below)
;;; or zeroa (limit from above).
(defun simplimln (expr var val)
  (let ((arglim (limit (cadr expr) var val 'think)) (dir)) 
    (cond ((eq arglim '$inf) '$inf) ;log(inf) = inf
          ;;log(minf,infinity,zerob) = infinity & log(0) = infinity
		  ((or (member arglim '($minf $infinity $zerob)))
		   '$infinity)
		  ((eq arglim '$zeroa) '$minf) ;log(zeroa) = minf
          ;; log(ind)=und & log(und)=und
		  ((member arglim '($ind $und)) '$und)
          ;; log(1^(-)) = zerob, log(1^(+)) = zeroa & log(1)=0
		  ((eql arglim 1)
		      (if (or (eq val '$zerob) (eq var '$zeroa)) val 0))
		  ;; Special case of arglim = 0
		  ((eql arglim 0)
		    (setq dir (behavior (cadr expr) var val))
			(cond ((eql dir -1) '$infinity)
				  ((eql dir 0) '$infinity)
				  ((eql dir 1) '$minf)))
          ;; When arglim is off the negative real axis, use direct substitution
		  ((off-negative-real-axisp arglim) 
            (ftake '%log arglim))
	      (t
		     ;; We know that arglim is a negative real number, say xx.
			 ;; When the imaginary part of (cadr expr) near var is negative,
			 ;; return log(-x) - %i*pi; when the imaginary part of (cadr expr) 
			 ;; near var is positive return log(-x) + %i*pi; and when
			 ;; we cannot determine the behavior of the imaginary part,
			 ;; return a nounform. The value of val (either zeroa or zerob)
			 ;; determines what is meant by "near" (smaller than var when 
			 ;; val is zerob and larger than var when val is zeroa).
	         (setq dir (behavior ($imagpart (cadr expr)) var val))
             (cond  ((or (eql dir 1) (eql dir -1))
	                  (add (ftake '%log (mul -1 arglim)) (mul dir '$%i '$%pi)))
	                (t (throw 'limit nil))))))) ;do a nounform return
(setf (get '%log 'simplim%function) 'simplimln)
(setf (get '%plog 'simplim%function) 'simplimln)

(defun simplim%limit (e x pt)
	(declare (ignore e x pt))
	(throw 'limit t))
(setf (get '%limit 'simplim%function) 'simplim%limit)

(defun simplim%unit_step (e var val)
	(let ((lim (limit (cadr e) var val 'think)))
		(cond ((eq lim '$und) '$und)
			  ((eq lim '$ind) '$ind)
			  ((eq lim '$zerob) 0)
			  ((eq lim '$zeroa) 1)
			  ((not (lenient-realp lim)) (throw 'limit nil)) ;catches infinity too
			  ;; catches minf & inf cases
			  ((eq t (mgrp 0 lim)) 0)
			  ((eq t (mgrp lim 0)) 1)
			  (t '$ind))))
(setf (get '$unit_step 'simplim%function) 'simplim%unit_step)

(defun simplim%conjugate (e var val)
	(let ((lim (limit (cadr e) var val 'think)))
		(cond ((off-negative-real-axisp lim)
				(ftake '$conjugate lim))
              (t (throw 'limit nil)))))
(setf (get '$conjugate 'simplim%function)  'simplim%conjugate)

(defun simplim%imagpart (e var val)
	(let ((lim (limit (cadr e) var val 'think)))
	   (cond ((eq lim '$und) '$und)
	         ((eq lim '$ind) 0)
			 ((eq lim '$infinity) (throw 'limit nil))
			 (t (mfuncall '$imagpart lim)))))
(setf (get '$imagpart 'simplim%function) 'simplim%imagpart)
(setf (get '%imagpart 'simplim%function) 'simplim%imagpart)

(defun simplim%realpart (e var val)
	(let ((lim (limit (cadr e) var val 'think)))
	   (cond ((eq lim '$und) '$und)
	         ((eq lim '$ind) '$ind)
			 ((eq lim '$infinity) (throw 'limit nil))
			 (t  (mfuncall '$realpart lim)))))
(setf (get '$realpart 'simplim%function) 'simplim%realpart)
(setf (get '%realpart 'simplim%function) 'simplim%realpart)
;;; Limit of the Factorial function

(defun simplimfact (expr var val)
  (let* ((arglim (limit (cadr expr) var val 'think)) ; Limit of the argument.
         (arg2 arglim))
    (cond ((eq arglim '$inf) '$inf)
          ((member arglim '($minf $infinity $und $ind) :test #'eq) '$und)
          ((zerop2 arglim) 1)
          ((and (or (maxima-integerp arglim)
                    (setq arg2 (integer-representation-p arglim)))
                (eq ($sign arg2) '$neg))
           ;; A negative integer or float or bigfloat representation.
           (let ((dir (limit (add (cadr expr) (mul -1 arg2)) var val 'think))
                 (even (mevenp arg2)))
             (cond ((or (and even
                             (eq dir '$zeroa))
                        (and (not even)
                             (eq dir '$zerob)))
                    '$minf)
                   ((or (and even
                             (eq dir '$zerob))
                        (and (not even)
                             (eq dir '$zeroa)))
                    '$inf)
                   (t (throw 'limit nil)))))
          (t
           ;; Call simplifier to get value at the limit of the argument.
           (simplify (list '(mfactorial) arglim))))))
(setf (get 'mfactorial 'simplim%function) 'simplimfact)

(defun simplim%erf-%tanh (fn arg)
  (let ((arglim (limit arg var val 'think))
        (ans ())
        (rlim ()))
    (cond ((eq arglim '$inf) 1)
	  ((eq arglim '$minf) -1)
	  ((eq arglim '$infinity)
	   (destructuring-bind (rpart . ipart)
               (trisplit arg)
	     (setq rlim (limit rpart var origval 'think))
	     (cond ((eq fn '%tanh)
		    (cond ((equal rlim '$inf) 1)
			  ((equal rlim '$minf) -1)))
		   ((eq fn '%erf)
		    (setq ans (limit (m* rpart (m^t ipart -1)) var origval 'think))
		    (setq ans ($asksign (m+ `((mabs) ,ans) -1)))
		    (cond ((or (eq ans '$pos) (eq ans '$zero))
			   (cond ((eq rlim '$inf) 1)
				 ((eq rlim '$minf) -1)
				 (t '$und)))
			  (t '$und))))))
	  ((eq arglim '$und) '$und)
	  ((member arglim '($zeroa $zerob $ind) :test #'eq) arglim)
;;;Ignore tanh(%pi/2*%I) and multiples of the argument.
	  (t
	   ;; erf (or tanh) of a known value is just erf(arglim).
	   (simplify (list (ncons fn) arglim))))))

(defun in-domain-of-atan (z)
  (setq z (trisplit z)) ; split z into real and imaginary parts
  (let ((x (car z)) (y (cdr z))) ;z = x+%i*y
      (not 
       (and
          (eq t (meqp x 0)) ;Re(z) = 0
          (or (eq t (mgqp -1 y)) ;-1 >= Im(z)
              (eq t (mgqp y 1))))))) ; Im(z) >= 1

(defun simplim%atan (e x pt)
  (let ((lim (limit (cadr e) x pt 'think)))
	(cond ((or (eq lim '$zeroa) (eq lim '$zerob) (eq lim 0) (eq lim '$ind)) lim)
		  ((or (eq lim '$und) (eq lim '$infinity)) (throw 'limit nil))
   	   	  ((eq lim '$inf) #$%pi/2$)    ;atan(inf) -> %pi/2
	      ((eq lim '$minf) #$-%pi/2$)  ;atan(-inf) -> -%pi/2
		  ((in-domain-of-atan (ridofab lim)) ; direct substitution
			  (ftake '%atan (ridofab lim)))
	    (t (limit ($logarc e) x pt 'think)))))
(setf (get '%atan 'simplim%function) 'simplim%atan)

(defmvar extended-reals 
	(append *infinitesimals* *infinities* (list '$und '$ind)))

;; Most instances of atan2 are simplified to atan expressions, but this routine 
;; handles tricky cases such as limit(atan2((x^2-2), x^3-2*x), x, sqrt(2), minus).
;; Taylor and Gruntz cannot handle the discontinuity at atan(0, -1)

;; When possible, we want to evaluate the limit of an atan2 expression using 
;; direct substitution--that produces, I think, the least surprising values. 

;; The general simplifier catches atan2(0,0) and it transforms atan2(minf or inf,X)
;; and atan2(X, minf or inf) into an atan expression, but it doesn't catch 
;; atan2(X, zerob or zeroa) or atan2(zerob or zeroa, X). For the other extended 
;; real (ind,und, or infinity) arguments, the general simplifier gives sign errors.

(defun simplim%atan2 (e v pt)
     (let ((y (second e)) (x (third e)) (xlim) (ylim) (xlim-z) (ylim-z) (q))
		(setq xlim (limit x v pt 'think))
		(setq ylim (limit y v pt 'think))
	  	(setq xlim-z (ridofab xlim)
			  ylim-z (ridofab ylim))
		;; For cases for which direct substitution fails, normalize 
		;; x & y and try again.
		(setq q (cond ((eq xlim '$inf) x)
			((eq xlim '$minf) (mul -1 x))
			((eq ylim '$inf) y)
			((eq ylim '$minf) (mul -1 y))
            ((and (eq xlim '$zerob) (zerop2 ylim)) (mul -1 x))
			((and (eq xlim '$zeroa) (zerop2 ylim)) x)
			((and (eq ylim '$zerob) (zerop2 xlim)) (mul -1 y))
			((and (eq ylim '$zeroa) (zerop2 xlim)) y) 
			(t 1)))

			(when (not (eql q 1))
					(setq x (div x q))
					(setq y (div y q))
					(setq xlim (limit x v pt 'think))
		  		    (setq ylim (limit y v pt 'think))
	  	            (setq xlim-z (ridofab xlim)
			              ylim-z (ridofab ylim)))		  	  
				
	   	(cond
			((and (eq '$zerob ylim) (eq t (mgrp 0 xlim))) (mul -1 '$%pi))
			((and (eq '$zerob ylim) (eq t (mgrp xlim 0))) 0)
			((and (eq '$zeroa ylim) (eq t (mgrp 0 xlim))) '$%pi)
			((and (eq '$zeroa ylim) (eq t (mgrp xlim 0))) 0)
			((and (eql xlim 1) (eql ylim '$inf)) (div '$%pi 2))
			((and (eql xlim -1) (eql ylim 0)) '$ind)
					 
		    ;; Use direct substitution when ylim-z # 0 or xlim-z > 0. We need
		    ;; to check that xlim-z & ylim-z are real too.
		    ((and (lenient-realp xlim-z) (lenient-realp ylim-z)
				  (or (eq t (mnqp ylim-z 0)) (eq t (mgrp xlim-z 0))))
			 	        (ftake '$atan2 ylim-z xlim-z))
			(t
				(throw 'limit nil)))))
(setf (get '$atan2 'simplim%function) 'simplim%atan2)

(defun simplimsch (sch arg)
  (cond ((real-infinityp arg)
	 (cond ((eq sch '%sinh) arg) (t '$inf)))
	((eq arg '$infinity) '$infinity)
	((eq arg '$ind) '$ind)
	((eq arg '$und) '$und)
	((and (eq sch '%sinh)
	      (or (eq arg '$zerob) (eq arg '$zeroa)))
         arg)
	(t (let (($exponentialize t))
	     (resimplify (list (ncons sch) (ridofab arg)))))))

;; simple limit of sin and cos
(defun simplimsc (exp fn arg)
  (cond ((member arg '($inf $minf $ind) :test #'eq) '$ind)
	((member arg '($und $infinity) :test #'eq)
	 (throw 'limit ()))
	((member arg '($zeroa $zerob) :test #'eq)
	 (cond ((eq fn '%sin) arg)
	       (t (m+ 1 '$zerob))))
	((sincoshk exp
		   (simplify (list (ncons fn) (ridofab arg)))
		   fn))))

(defun simplim%tan (arg)
  (let ((arglim (limit arg var val 'think)))
	(cond
	  ((member arglim '($inf $minf $ind) :test #'eq)
		'$ind)
	  ((member arglim '($und $infinity) :test #'eq)
		(throw 'limit nil))
	  (t
		;; Write the limit of the argument as c*%pi + rest.
		(let*
		  ((c (or (pip arglim) 0))
		   (rest (sratsimp (m- arglim (m* '$%pi c))))
		   (hit-zero))
		  ;; Check if tan(x) has a zero or pole at x=arglim.
		  ;; zero: tan(n*%pi + 0*)
		  ;; pole: tan((2*n+1)*%pi/2 + 0*)
		  ;; 0* can be $zeroa, $zerob or 0.
		  (if (and (member rest '(0 $zeroa $zerob) :test #'equal)
				   (or (setq hit-zero (integerp c))
					   (and (ratnump c) (equal (caddr c) 2))))
			;; This is a zero or a pole.
			;; Determine on which side of the zero/pole we are.
			;; If rest is $zeroa or $zerob, use that.
			;; Otherwise (rest = 0), try to determine the side
			;; using the behavior of the argument.
			(let
			  ((side (cond ((eq rest '$zeroa) 1)
						   ((eq rest '$zerob) -1)
						   (t (behavior arg var val)))))
			  (if hit-zero
				;; For a zero, if we don't know the side, just return 0.
				(cond
				  ((equal side 1) '$zeroa)
				  ((equal side -1) '$zerob)
				  (t 0))
				;; For a pole, we need to know the side.
				;; Otherwise, we can't determine the limit.
				(cond
				  ((equal side 1) '$minf)
				  ((equal side -1) '$inf)
				  (t (throw 'limit t)))))
			;; No zero or pole - substitute in the limit of the argument.
			(take '(%tan) (ridofab arglim))))))))

(defun simplim%asinh (arg)
  (cond ((member arg '($inf $minf $zeroa $zerob $ind $und) :test #'eq)
	 arg)
	((eq arg '$infinity) '$und)
	(t (simplify (list '(%asinh) (ridofab arg))))))

(defun simplim%acosh (arg)
  (cond ((equal (ridofab arg) 1) '$zeroa)
	((eq arg '$inf) arg)
	((eq arg '$minf) '$infinity)
	((member arg '($und $ind $infinity) :test #'eq) '$und)
	(t (simplify (list '(%acosh) (ridofab arg))))))

(defun simplim%atanh (arg dir)
  ;; Compute limit(atanh(x),x,arg).  If ARG is +/-1, we need to take
  ;; into account which direction we're approaching ARG.
  (cond ((zerop2 arg) arg)
	((member arg '($ind $und $infinity $minf $inf) :test #'eq)
	 '$und)
	((equal (setq arg (ridofab arg)) 1.)
	 ;; The limit at 1 should be complex infinity because atanh(x)
	 ;; is complex for x > 1, but inf if we're approaching 1 from
	 ;; below.
	 (if (eq dir '$zerob)
	     '$inf
	     '$infinity))
	((equal arg -1.)
	 ;; Same as above, except for the limit is at -1.
	 (if (eq dir '$zeroa)
	     '$minf
	     '$infinity))
	(t (simplify (list '(%atanh) arg)))))

(defun simplim%asin (e x pt)
  (let ((lim (limit (cadr e) x pt 'think)) (dir) (lim-sgn))
 	  (cond ((member lim '($zeroa $zerob)) lim) ;asin(zeoroa/b) = zeroa/b
	        ((member lim '($minf '$inf '$infinity)) '$infinity)
			((eq lim '$ind) '$ind) ;asin(ind)=ind 
			((eq lim '$und) '$und) ;asin(und)=und
			((in-domain-of-asin lim) ;direct substitution
				(ftake '%asin lim))
			(t 
			  (setq e (trisplit (cadr e))) ;overwrite e!
			  (setq dir (behavior (cdr e) x pt))
			  (setq lim-sgn ($csign (car e))) ;lim-sgn = sign limit(Re(e))
			  (cond 			  
			  	((eql dir 0)
				  	(throw 'limit t)) ;unable to find behavior of imaginary part

		        ;; For the values of asin on the branch cuts, see DLMF 4.23.20 & 4.23.21
				;; Diagram of the values of asin just above and below the branch cuts
				;; 
				;;  asin(x)                           pi - asin(x) 
				;;................ -1 ....0.... 1  ...............
                ;; -pi - asin(x)                        asin(x)
				;;
				;; Let's start in northwest and rotate counterclockwise:
				((and (eq '$neg lim-sgn) (eq dir 1))
					(ftake '%asin lim))
				((and (eq '$pos lim-sgn) (eq dir 1))
					(sub '$%pi (ftake '%asin lim)))
				((and (eq '$pos lim-sgn) (eq dir -1))
					(ftake '%asin lim))
				((and (eq '$neg lim-sgn) (eq dir -1))
				    (sub (mul -1 '$%pi) (ftake '%asin lim)))
				(t  (throw 'limit t))))))) ; unable to find sign of real part of lim.
(setf (get '%asin 'simplim%function) 'simplim%asin)

(defun simplim%acos (e x pt)
  (let ((lim (limit (cadr e) x pt 'think)) (dir) (lim-sgn))
 	  (cond ((in-domain-of-asin lim) ;direct substitution
				(ftake '%acos lim))
			((member lim '($und $ind $inf $minf $infinity)) ;boundary cases
			 '$und)	
			(t 
			  (setq e (trisplit (cadr e))) ;overwrite e!
			  (setq dir (behavior (cdr e) x pt))
			  (setq lim-sgn ($csign lim))
			  (cond 			  
			  	((eql dir 0)
				  	(throw 'limit t)) ;unable to find behavior of imaginary part
		        ;; for the values of acos on the branch cuts, see DLMF 4.23.24 & 4.23.25
				;; http://dlmf.nist.gov/4.23.E24
			    ((or (eq '$pos lim-sgn) (eq '$neg lim-sgn))
					;; continuous from above
					(if (eql dir 1) (ftake '%acos lim) (sub (mul 2 '$%pi) (ftake '%acos lim))))
				(t  (throw 'limit t))))))) ; unable to find sign of real part of lim.
(setf (get '%acos 'simplim%function) 'simplim%acos)

;; Limit of an %integrate expression. For a definite integral
;; integrate(ee,var,a,b), when ee is free of the limit variable
(defun simplim%integrate (e x pt)
	(let* ((ee (second e)) ;ee = integrand
		   (var (third e)) ;integration variable
		   (a (fourth e))  ;lower limit or nil if indefinite
           (b (fifth e))   ;lower limit or nil if indefinite
		   (alim) (blim))
	  (cond ((and a b ($freeof x ee) ($freeof x var))
	    	  (setq alim (limit a x pt 'think))
		      (setq blim (limit b x pt 'think))
		      (if (and (lenient-extended-realp alim) 
			           (lenient-extended-realp blim)
					   (not (eq alim '$infinity))
					   (not (eq blim '$infinity)))
		    	(ftake '%integrate ee var alim blim)
			    (throw 'limit t)))
            (t
                (throw 'limit t)))))
(setf (get '%integrate 'simplim%function) 'simplim%integrate)

(defun subftake (op subarg arg)
  (simplifya (subfunmake op subarg arg) t))
  
(defun off-one-to-inf (z)
  (setq z (trisplit z)) ; split z into x+%i*y
  (or
    (eq t (mnqp (cdr z) 0))    ; y # 0
    (eq t (mgrp 1 (car z)))))    ; x < 1

(defun simplim%li (expr x pt)
  (let ((n (car (subfunsubs expr))) (e (car (subfunargs expr))))
    (cond ((freeof x n)
	   (setq e (limit e x pt 'think))
	   (cond ((and (eq e '$minf) (integerp n) (>= n 2))
		  '$minf)
		 ((and (eq e '$inf) (integerp n) (>= n 2))
		  '$infinity)
		 ((or (eql (ridofab e) 1) (and (not (extended-real-p e)) (off-one-to-inf e)))
		  ;; Limit of li[s](1) can be evaluated by just
		  ;; substituting in 1.
		  ;; Same for li[s](x) when x is < 1.
		  (subftake '$li (list n) (list e)))
		 (t (throw 'limit nil))))
	  ;; Claim ignorance when order depends on limit variable.	
	  (t (throw 'limit nil)))))

(setf (get '$li 'simplim%function) 'simplim%li)
(setf (get '%li 'simplim%function) 'simplim%li)

(defun simplim$psi (order arg val)
  (if (and (not (equal (length order) 1))
         (not (equal (length arg) 1)))
      (throw 'limit ())
      (setq order (car order)
            arg (car arg)))
  (cond ((equal order 0) 
	 (destructuring-bind (rpart . ipart)
             (trisplit arg)
           (cond ((not (equal ipart 0))  (throw 'limit ()))
                 (t (setq rpart (limit rpart var val 'think))
                    (cond ((eq rpart '$zeroa)  '$minf)
                          ((eq rpart '$zerob)  '$inf)
                          ((eq rpart '$inf)  '$inf)
                          ((eq rpart '$minf)   '$und)
                          ((equal (getsignl rpart) -1)  (throw 'limit ()))
                          (t (simplify (subfunmake '$psi (list order)
                                                   (list rpart)))))))))
	((and (integerp order) (> order 0) 
            (equal (limit arg var val 'think) '$inf))
	 (cond ((mevenp order) '$zerob)
	       ((moddp order) '$zeroa)
	       (t (throw 'limit ()))))
	(t (throw 'limit ()))))

(defun simplim%inverse_jacobi_ns (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      0
      `((%inverse_jacobi_ns) ,arg ,m)))

(defun simplim%inverse_jacobi_nc (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      `((%elliptic_kc) ,m)
      `((%inverse_jacobi_nc) ,arg ,m)))

(defun simplim%inverse_jacobi_sc (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      `((%elliptic_kc) ,m)
      `((%inverse_jacobi_sc) ,arg ,m)))

(defun simplim%inverse_jacobi_dc (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      `((%elliptic_kc) ,m)
      `((%inverse_jacobi_dc) ,arg ,m)))

(defun simplim%inverse_jacobi_cs (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      0
      `((%inverse_jacobi_cs) ,arg ,m)))

(defun simplim%inverse_jacobi_ds (arg m)
  (if (or (eq arg '$inf) (eq arg '$minf))
      0
      `((%inverse_jacobi_ds) ,arg ,m)))

(defun simplim%signum (e x pt)
  (let ((e (limit (cadr e) x pt 'think)) (sgn))
    (cond ((eq '$minf e) -1)
		  ((eq '$inf e) 1)
		  ((eq '$infinity e) '$und)
		  ((eq '$ind e) '$ind)
		  ((eq '$und e) e)
		  ((eq '$zerob e) -1)
		  ((eq '$zeroa e) 1)
		  (t  
		     (setq sgn (mnqp e 0))
			 (cond ((eq t sgn) (ftake '%signum e))
			 	   (t (throw 'limit nil))))))) ; don't know
(setf (get '%signum 'simplim%function) 'simplim%signum)

;; more functions for limit to handle

(defun lfibtophi (e)
  (cond ((not (involve e '($fib))) e)
	((eq (caar e) '$fib)
	 (let ((lnorecurse t))
	   ($fibtophi (list '($fib) (lfibtophi (cadr e))) lnorecurse)))
	(t (cons (car e)
		 (mapcar #'lfibtophi (cdr e))))))

;;;     FOLLOWING CODE MAKES $LDEFINT WORK

(defmfun $ldefint (exp var ll ul &aux $logabs ans a1 a2)
  (setq $logabs t ans (sinint exp var)
	a1 (toplevel-$limit ans var ul '$minus)
	a2 (toplevel-$limit ans var ll '$plus))
  (and (member a1 '($inf $minf $infinity $und $ind) :test #'eq)
       (setq a1 (nounlimit ans var ul)))
  (and (member a2 '($inf $minf $infinity $und $ind) :test #'eq)
       (setq a2 (nounlimit ans var ll)))
  ($expand (m- a1 a2)))

(defun nounlimit (exp var val)
  (setq exp (restorelim exp))
  (nconc (list '(%limit) exp var (ridofab val))
	 (cond ((eq val '$zeroa) '($plus))
	       ((eq val '$zerob) '($minus)))))

;; substitute inside noun form of %derivative
;; for cases such as     limit('diff(x+2,x), x, 1)
;;                 ->    limit('diff(xx+3), xx, 0)
;;
;; maxima-substitute with *atp* skips over %derivative
;;
;; substitutes   diff(f(realvar), realvar, n)
;;           ->  diff(f(var+val), var, n)
(defun derivative-subst (exp val var realvar)
  (cond ((atom exp) exp)
	((eq '%derivative (caar exp))
	 (cons
	  (car exp)
	  (cons		;; the function being differentiated
	   (maxima-substitute (m+ val var) realvar (cadr exp))
	   (cons	;; the var of differentiation
	    (maxima-substitute var realvar (caddr exp))
	    (cdddr exp))))) ;; the order of the derivative
	(t (cons (car exp)
		 (mapcar (lambda (x) (derivative-subst x val var realvar))
			 (cdr exp))))))

;;;Used by Defint also.
(defun oscip (e)
  (or (involve e '(%sin %cos %tan))
      (among '$%i (%einvolve e))))

(defun %einvolve (e)
  (when (among '$%e e) (%einvolve01 e)))

(defun %einvolve01 (e)
  (cond ((atom e) nil)
	((mnump e) nil)
	((and (mexptp e)
	      (eq (cadr e) '$%e)
	      (among var (caddr e)))
	 (caddr e))
	(t (some #'%einvolve (cdr e)))))

(declare-top (unspecial *indicator exp var val origval taylored
			$tlimswitch logcombed lhp? lhcount))


;; GRUNTZ ALGORITHM

;; Dominik Gruntz
;; "On Computing Limits in a Symbolic Manipulation System"
;; PhD Dissertation ETH Zurich 1996

;; The algorithm identifies the most rapidly varying (MRV) subexpression,
;; replaces it with a new variable w, rewrites the expression in terms
;; of the new variable, and then repeats.

;; The algorithm doesn't handle oscillating functions, so it can't do things like
;; limit(sin(x)/x, x, inf).

;; To handle limits involving functions like gamma(x) and erf(x), the
;; gruntz algorithm requires them to be written in terms of asymptotic
;; expansions, which maxima cannot currently do.

;; The algorithm assumes that everything is real, so it can't
;; currently handle limit((-2)^x, x, inf).

;; This is one of the methods used by maxima's $limit.
;; It is also directly available to the user as $gruntz.


;; most rapidly varying subexpression of expression exp with respect to limit variable var.
;; returns a list of subexpressions which are in the same MRV equivalence class.
(defun mrv (exp var)
  (cond ((freeof var exp)
	 nil)
	((eq var exp)
	 (list var))
	((mtimesp exp)
	 (mrv-max (mrv (cadr exp) var)
		  (mrv (m*l (cddr exp)) var)
		  var))
	((mplusp exp)
	 (mrv-max (mrv (cadr exp) var)
		  (mrv (m+l (cddr exp)) var)
		  var))
	((mexptp exp)
	 (cond ((freeof var (caddr exp))
		(mrv (cadr exp) var))
	       ((member (limitinf (logred exp) var) '($inf $minf) :test #'eq)
		(mrv-max (list exp) (mrv (caddr exp) var) var))
	       (t (mrv-max (mrv (cadr exp) var) (mrv (caddr exp) var) var))))
	((mlogp exp)
	 (mrv (cadr exp) var))
	((equal (length (cdr exp)) 1)
	 (mrv (cadr exp) var))
	((equal (length (cdr exp)) 2)
	 (mrv-max (mrv (cadr exp) var)
		  (mrv (caddr exp) var)
		  var))
	(t (tay-error "mrv not implemented" exp))))

;; takes two lists of expressions, f and g, and limit variable var.
;; members in each list are assumed to be in same MRV equivalence
;; class.  returns MRV set of the union of the inputs - either f or g
;; or the union of f and g.
(defun mrv-max (f g var)
  (prog ()
	(cond ((not f)
	       (return g))
	      ((not g)
	       (return f))
	      ((intersection f g)
	       (return (union f g))))
	(let ((c (mrv-compare (car f) (car g) var)))
	  (cond ((eq c '>)
		 (return f))
		((eq c '<)
		 (return g))
		((eq c '=)
		 (return (union f g)))
		(t (merror "MRV-MAX: expected '>' '<' or '='; found: ~M" c))))))

(defun mrv-compare (a b var)
  (let ((c (limitinf (m// `((%log) ,a) `((%log) ,b)) var)))
    (cond ((equal c 0)
	   '<)
	  ((member c '($inf $minf) :test #'eq)
	   '>)
	  (t '=))))

;; rewrite expression exp by replacing members of MRV set omega with
;; expressions in terms of new variable wsym.  return cons pair of new
;; version of exp and the log of the new variable wsym.
(defun mrv-rewrite (exp omega var wsym)
  (setq omega (stable-sort omega (lambda (x y) (> (length (mrv x var))
					   (length (mrv y var))))));FIXME consider a total order function with #'sort
  (let* ((g (car (last omega)))
	 (logg (logred g))
	 (sig (equal (mrv-sign logg var) 1))
	 (w (if sig (m// 1 wsym) wsym))
	 (logw (if sig (m* -1 logg) logg)))
    (mapcar (lambda (x y)
	      ;;(mtell "y:~M x:~M exp:~M~%" y x exp)
	      (setq exp (syntactic-substitute y x exp)))
	    omega
	    (mapcar (lambda (f)		;; rewrite each element of omega
		      (let* ((logf (logred f))
			     (c (mrv-leadterm (m// logf logg) var nil)))
			(cond ((not (equal (cadr c) 0))
			       (merror "MRV-REWRITE: expected leading term to be constant in ~M" c)))
			;;(mtell "logg: ~M  logf: ~M~%" logg logf)
			(m* (m^ w (car c))
			    (m^ '$%e (m- logf
					 (m* (car c) logg))))))
		    omega))
    (cons exp logw)))

;;; if log w(x) = h(x), rewrite all subexpressions of the form
;;; log(f(x)) as log(w^-c f(x)) + c h(x) with c the unique constant
;;; such that w^-c f(x) is strictly less rapidly varying than w.
(defun mrv-rewrite-logs (exp wsym logw)
  (cond ((atom exp) exp)
	((and (mlogp exp)
	      (not (freeof wsym exp)))
	 (let* ((f (cadr exp))
		(c ($lopow (calculate-series f wsym)
			   wsym)))
	   (m+ (list (car exp)
		     (m* (m^ wsym (m- c))
			 (mrv-rewrite-logs f wsym logw)))
	       (m* c logw))))
	(t
	 (cons (car exp)
	       (mapcar (lambda (e)
			 (mrv-rewrite-logs e wsym logw))
		       (cdr exp))))))

;; returns list of two elements: coeff and exponent of leading term of exp,
;; after rewriting exp in term of its MRV set omega.
(defun mrv-leadterm (exp var omega)
  (prog ((new-omega ()))
	(cond ((freeof var exp)
	       (return (list exp 0))))
	(dolist (term omega)
	  (cond ((subexp exp term)
		 (push term new-omega))))
	(setq omega new-omega)
	(cond ((not omega)
	       (setq omega (mrv exp var))))
	(cond ((member var omega :test #'eq)
	       (let* ((omega-up (mrv-moveup omega var))
		      (e-up (car (mrv-moveup (list exp) var)))
		      (mrv-leadterm-up (mrv-leadterm e-up var omega-up)))
		 (return (mrv-movedown mrv-leadterm-up var)))))
	(destructuring-let* ((wsym (gensym "w"))
			     lo
			     coef
			     ((f . logw) (mrv-rewrite exp omega var wsym))
			     (series (calculate-series (mrv-rewrite-logs f wsym logw)
						       wsym)))
			    (setq series (maxima-substitute logw `((%log) ,wsym) series))
			    (setq lo ($lopow series wsym))
			    (when (or (not ($constantp lo))
				      (not (free series '%derivative)))
				      ;; (mtell "series: ~M lo: ~M~%" series lo)
			      (tay-error "error in series expansion" f))
			    (setq coef ($coeff series wsym lo))
                           (when (not (free coef wsym))
                             (tay-error "MRV-LEADTERM: failed to extract leading coefficient; obtained" coef))
			    ;;(mtell "exp: ~M f: ~M~%" exp f)
			    ;;(mtell "series: ~M~%coeff: ~M~%pow: ~M~%" series coef lo)
			    (return (list coef lo)))))

(defun mrv-moveup (l var)
  (mapcar (lambda (exp)
	    (simplify-log-of-exp
	     (syntactic-substitute `((mexpt) $%e ,var) var exp)))
	  l))

(defun mrv-movedown (l var)
  (mapcar (lambda (exp) (syntactic-substitute `((%log simp) ,var) var exp))
	  l))

;; test whether sub is a subexpression of exp
(defun subexp (exp sub)
  (let ((dummy (gensym)))
	(putprop dummy t 'internal)
 	(not (alike1 (maxima-substitute dummy
				 sub
				 exp)
	      exp))))

;; Generate $lhospitallim terms of taylor expansion.
;; Ideally we would use a lazy series representation that generates
;; more terms as higher order terms cancel.
(defun calculate-series (exp var)
  (let ((cntx ($supcontext)))
		 ($activate cntx)
		 (unwind-protect 
		 	 (progn
				 (mfuncall '$assume (ftake 'mgreaterp var 0))
				 (putprop var t 'internal); keep var from appearing in questions to user
 			     ($taylor exp var 0 $lhospitallim))
			  (remprop var 'internal)	  
              ($killcontext cntx))))

(defun mrv-sign (exp var)
  (cond ((freeof var exp)
	 (let ((sign ($sign ($radcan exp))))
	   (cond ((eq sign '$zero)
		  0)
		 ((eq sign '$pos)
		  1)
		 ((eq sign '$neg)
		  -1)
		 (t (tay-error " cannot determine mrv-sign" exp)))))
	((eq exp var)
	 1)
	((mtimesp exp)
	 (* (mrv-sign (cadr exp) var)
	    (mrv-sign (m*l (cddr exp)) var)))
	((and (mexptp exp)
	      (equal (mrv-sign (cadr exp) var) 1))
	 1)
	((mlogp exp)
	 (cond ((equal (mrv-sign (cadr exp) var) -1)
		(tay-error " complex expression in gruntz limit" exp)))
	 (mrv-sign (m+ -1 (cadr exp)) var))
	((mplusp exp)
	 (mrv-sign (limitinf exp var) var))
	(t (tay-error " cannot determine mrv-sign" exp))))

;; gruntz algorithm for limit of exp as var goes to positive infinity
(defun limitinf (exp var)
  (prog (($exptsubst nil))
	(cond ((freeof var exp)
	       (return exp)))
	(destructuring-let* ((c0-e0 (mrv-leadterm exp var nil))
			     (c0 (car c0-e0))
			     (e0 (cadr c0-e0))
			     (sig (mrv-sign e0 var)))
			    (cond ((equal sig 1)
				   (return 0))
				  ((equal sig -1)
				   (cond ((equal (mrv-sign c0 var) 1)
					  (return '$inf))
					 ((equal (mrv-sign c0 var) -1)
					  (return '$minf))))
				  ((equal sig 0)
				   (if (equal exp c0)
				       ;; example: gruntz(n^n/(n^n+(n-1)^n), n, inf);
				       (tay-error " infinite recursion in limitinf" exp))
				   (return (limitinf c0 var)))))))

;; user-level function equivalent to $limit.
;; direction must be specified if limit point is not infinite
;; The arguments are checked and a failure of taylor is catched.

(defmfun $gruntz (expr var val &rest rest)
  (let (ans dir)
    (when (> (length rest) 1)
      (merror
        (intl:gettext "gruntz: too many arguments; expected just 3 or 4")))
    (setq dir (car rest))
    (when (and (not (member val '($inf $minf $zeroa $zerob)))
               (not (member dir '($plus $minus))))
      (merror
        (intl:gettext "gruntz: direction must be 'plus' or 'minus'")))
    (setq ans
          (catch 'taylor-catch
            (let ((silent-taylor-flag t))
              (gruntz1 expr var val dir))))
     (if (or (null ans) (eq ans t))
         (if dir
             `(($gruntz simp) ,expr ,var, val ,dir)
             `(($gruntz simp) ,expr ,var ,val))
         ans)))

;; Additional simplifications for the limit function. Specifically:
;;   (a) replace every mapatom that is declared to be zero by zero 
;;   (b) dispatch radcan on expressions of the form (positive integer)^XXX
;; The mechanism (a) isn't perfect--if a+b is declared to zero, it doesn't
;; simplify a+b+c to c, for example.

;; For efficiency, the functionality of factosimp, tansc, lfibtophi, 
;; and limitsimp should be incorporated into extra-simp. 
(defun extra-simp (e)
   (cond (($subvarp e) e) ;return e
	     ((extended-real-p e) e) ;we don't want to call sign on ind, so catch this
		 (($mapatom e) ;if e is declared zero, return 0; otherwise e
		     (if (eq '$zero ($csign e)) 0 e))
         ;; dispatch radcan on (positive integer)^Y
         ((and (mexptp e) (integerp (cadr e)) (> (cadr e) 0))
		     ($radcan (ftake 'mexpt (cadr e) (extra-simp (caddr e)))))
	     (($subvarp (mop e)) ;subscripted function
		     (subfunmake 
		      (subfunname e) 
			  (mapcar #'extra-simp (subfunsubs e)) 
			  (mapcar #'extra-simp (subfunargs e))))
		 (t
		     (simplifya (cons (list (caar e)) (mapcar #'extra-simp (cdr e))) t))))

;; This function is for internal use in $limit.

;; The function gruntz1 standardizes the limit point to inf and the limit variable
;; to a gensym. Since the limit point is possibly altered by this function, we
;; need to make the appropriate assumptions on the limit variable. This is done
;; in a supcontext.
(defun gruntz1 (exp var val &rest rest)
  (cond ((> (length rest) 1)
	 (merror (intl:gettext "gruntz: too many arguments; expected just 3 or 4"))))
  (let (($logexpand t) ; gruntz needs $logexpand T
        (newvar (gensym "w"))
	(dir (car rest)))
	(putprop newvar t 'internal); keep var from appearing in questions to user	
    (cond ((eq val '$inf)
	   (setq exp (maxima-substitute newvar var exp)))
	  ((eq val '$minf)
	   (setq exp (maxima-substitute (m* -1 newvar) var exp)))
	  ((eq val '$zeroa)
	   (setq exp (maxima-substitute (m// 1 newvar) var exp)))
	  ((eq val '$zerob)
	   (setq exp (maxima-substitute (m// -1 newvar) var exp)))
	  ((eq dir '$plus)
	   (setq exp (maxima-substitute (m+ val (m// 1 newvar)) var exp)))
	  ((eq dir '$minus)
	   (setq exp (maxima-substitute (m+ val (m// -1 newvar)) var exp)))
	  (t (merror (intl:gettext "gruntz: direction must be 'plus' or 'minus'; found: ~M") dir)))
	  (let ((cx ($supcontext)))
	   	    (unwind-protect
 	         (progn
				  (mfuncall '$assume (ftake 'mlessp *large-positive-number* newvar)) ; *large-positive-number* < newvar
                  (mfuncall '$assume (ftake 'mlessp 0 'lim-epsilon)) ; 0 < lim-epsilon
				  (mfuncall '$assume (ftake 'mlessp *large-positive-number* 'prin-inf)) ; *large-positive-number* < prin-inf
				  (mfuncall '$activate cx) ;not sure this is needed, but OK	
				  (setq exp (resimplify exp)) ;simplify in new context
				  (setq exp (extra-simp (sratsimp exp))) ;additional simplifications
				  (limitinf exp newvar)) ;compute & return limit
			($killcontext cx))))) ;kill context & forget all new facts.	 			

;; substitute y for x in exp
;; similar to maxima-substitute but does not simplify result
(defun syntactic-substitute (y x exp)
  (cond ((alike1 x exp) y)
	((atom exp) exp)
	(t (cons (car exp)
		 (mapcar (lambda (exp)
			   (syntactic-substitute y x exp))
			 (cdr exp))))))

;; log(exp(subexpr)) -> subexpr
;; without simplifying entire exp
(defun simplify-log-of-exp (exp)
  (cond ((atom exp) exp)
	((and (mlogp exp)
	      (mexptp (cadr exp))
	      (eq '$%e (cadadr exp)))
	 (caddr (cadr exp)))
	(t (cons (car exp)
		 (mapcar #'simplify-log-of-exp
			 (cdr exp))))))
