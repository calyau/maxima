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

(macsyma-module trgred)

(declare-top (special var *schatc-ans* *trigred *noexpand *lin))

(load-macsyma-macros rzmac)

(defvar *trans-list-plus*
  '((((mplus) ((coeffpt) (c true) ((mexpt) ((%tan) (x true)) 2))
       (var* (uvar) c))
      ((mtimes) c ((mexpt) ((%sec) x) 2)))
    (((mplus) ((coeffpt) (c true) ((mexpt) ((%cot) (x true)) 2))
      (var* (uvar) c))
     ((mtimes) c ((mexpt) ((%csc) x) 2)))
    (((mplus) ((coeffpt) (c true) ((mexpt) ((%tanh) (x true)) 2))
      ((mtimes) -1 (var* (uvar) c)))
     ((mtimes) -1 c ((mexpt) ((%sech) x) 2)))
    (((mplus) ((coeffpt) (c true) ((mexpt) ((%coth) (x true)) 2))
      ((mtimes) -1 (var* (uvar) c)))
     ((mtimes) c ((mexpt) ((%csch) x) 2)))))

(defvar *triglaws*
      '(* %sin (* %cot %cos %sec %tan) %cos (* %tan %sin %csc %cot)
	  %tan (* %cos %sin %csc %sec) %cot (* %sin %cos %sec %csc)
	  %sec (* %sin %tan %cot %csc) %csc (* %cos %cot %tan %sec)))

(defvar *hyperlaws*
      '(* %sinh (* %coth %cosh %sech %tanh) %cosh (* %tanh %sinh %csch %coth)
	  %tanh (* %cosh %sinh %csch %sech) %coth (* %sinh %cosh %sech %csch)
	  %sech (* %sinh %tanh %coth %csch) %csch (* %cosh %coth %tanh %sech)))

(defvar *sc^ndisp* '((%sin . sin^n) (%cos . cos^n) (%sinh . sinh^n) (%cosh . cosh^n)))

(defvar *laws*)
(defvar *trigbuckets*)
(defvar *hyperbuckets*)

;;The Trigreduce file contains a group of routines which can be used to
;;make trigonometric simplifications of expressions.  The bulk of the
;;routines here involve the reductions of products of sin's and cos's.
;;
;;	*TRIGRED	indicates that the special simplifications for
;;			$TRIGREDUCE are to be used.
;;	*NOEXPAND	indicates that trig functions of sums of
;;			angles are not to be used.

;; Without the check for the value of $simp, trigreduce can enter an
;; endless loop--for example trigreduce((-1)*x). There is at least one
;; test in the testsuite that sets simp to false and then needs to compute
;; a limit. But the limit code calls trigreduce and the test fails. The
;; top level check for simp seems like the best way to prevent infinite
;; loops when simp is false.
(defmfun ($trigreduce :properties ((evfun t))) (exp &optional (var '*novar))
  (let ((*trigred t)
        (*noexpand t)
        $trigexpand $verbose $ratprint)
    (declare (special $trigexpand *trigred))
    (if $simp (gcdred (sp1 exp)) exp)))

;; The first pass in power series expansion (used by $powerseries in
;; series.lisp), but also used by $trigreduce. Expands / reduces the expression
;; E as a function VAR, controlled by the *TRIGRED and *NOEXPAND flags.
(defun sp1 (e)
  (cond
    ((atom e) e)

    ;; We recognise some special patterns that are expressed as sums, such as
    ;; 1+tan(x)^2 => sec(x)^2. Rewrite using the first matching pattern (and
    ;; recurse to try to simplify further). If no pattern matches, expand each
    ;; term in the sum.
    ((eq (caar e) 'mplus)
     (or (dolist (pair *trans-list-plus*)
           (let ((match (m2 e (first pair))))
             (when match
               (return (sp1 (sch-replace match (second pair)))))))
         (m+l (mapcar #'sp1 (cdr e)))))

    ((eq (caar e) 'mtimes)
     (sp1times e))

    ((eq (caar e) 'mexpt)
     (sp1expt (sp1 (cadr e)) (sp1 (caddr e))))

    ((eq (caar e) '%log)
     (sp1log (sp1 (cadr e))))

    ((member (caar e) '(%cos %sin %tan %cot %sec %csc
                        %cosh %sinh %tanh %coth %sech %csch) :test #'eq)
     (sp1trig (list (car e) (let* ((*noexpand t)) (sp1 (cadr e))))))

    ((member (caar e) '(%asin %acos %atan %acot %asec %acsc
                        %asinh %acosh %atanh %acoth %asech %acsch) :test #'eq)
     (sp1atrig (caar e) (let* ((*noexpand t)) (sp1 (cadr e)))))

    ((eq (caar e) 'mrat) (sp1 (ratdisrep e)))

    ((mbagp e)
     (cons (list (caar e)) (mapcar #'(lambda (u)
                                       (gcdred (sp1 u)))
                                   (cdr e))))
    ((eq (caar e) '%integrate)
     (list* '(%integrate) (sp1 (cadr e)) (cddr e)))

    (t (recur-apply #'sp1 e))))

(defun trigfp (e)
  (or (and (not (atom e)) (trigp (caar e))) (equal e 1)))

(defun gcdred (e)
  (cond ((atom e) e)
	((eq (caar e) 'mplus) (m+l (mapcar #'gcdred (cdr e))))
	((eq (caar e) 'mtimes)
	 (let* ((nn '(1))
                (nd '(1))
                (gcd nil))
	   (do ((e (cdr e) (cdr e)))
	       ((null e)
		(setq nn (m*l nn) nd (m*l nd)))
	     (cond ((and (mexptp (car e))
			 (or (signp l (caddar e))
			     (and (mtimesp (caddar e))
				  (signp l (cadr (caddar e))))))
		    (setq nd (cons (m^ (cadar e) (m- (caddar e))) nd)))
		   ((ratnump (car e))
		    (setq nn (cons (cadar e) nn)
			  nd (cons (caddar e) nd)))
		   ((setq nn (cons (car e) nn)))))
	   (cond ((equal nd 1) nn)
		 ((equal (setq gcd ($gcd nn nd)) 1) e)
		 ((div* (cadr ($divide nn gcd))
			(cadr ($divide nd gcd)))))))
        (t (recur-apply #'gcdred e))))

(defun sp1times (e)
  (let* ((fr nil)
         (g '(1))
         (*trigbuckets* nil)
         (*hyperbuckets* nil)
         (tr nil)
         (hyp nil)
         (*lin '(0)))
    (dolist (factor (cdr e))
      (cond ((or (mnump factor)
		 (and (not (eq var '*novar)) (free factor var)))
	     (push factor fr))
	    ((atom factor) (push factor g))
	    ((or (trigfp factor)
		 (and (eq (caar factor) 'mexpt)
                      (trigfp (cadr factor))))
	     (sp1add factor))
	    (t
             (push factor g))))
    (setq g (mapcar #'sp1 g))

    (mapc #'(lambda (q)  (sp1sincos q t)) *trigbuckets*)
    (mapc #'(lambda (q) (sp1sincos q nil)) *hyperbuckets*)
    (setq fr (cons (m^ 1//2 (m+l *lin)) fr)
	  *lin nil)
    (setq tr (cons '* (mapcan #'sp1untrep *trigbuckets*)))
    (setq g (nconc (sp1tlin tr t) (sp1tplus *lin t) g)
	  *lin nil)
    (setq hyp (cons '* (mapcan #'sp1untrep *hyperbuckets*)))
    (setq g (nconc (sp1tlin hyp nil) (sp1tplus *lin nil) g))
    (setq g ($expand (let* (($keepfloat t))
                       (declare (special $keepfloat))
                       ($ratsimp (cons '(mtimes) g)))))
    (if (mtimesp g)
        (setq g (mapcar #'sp1 (cdr g)))
        (setq g (list (sp1 g))))
    (m*l (cons 1 (nconc g fr (cdr tr) (cdr hyp))))))

(defun sp1tlin (l trig)
  (cond ((null (cdr l)) nil)
	((and (eq (caaadr l) 'mexpt)
	      (integerp (caddr (cadr l)))
	      (member (caaadr (cadr l))
		    (if trig '(%sin %cos) '(%sinh %cosh)) :test #'eq))
	 (cons (funcall (cdr (assoc (caaadr (cadr l)) *sc^ndisp* :test #'eq))
			(caddr (cadr l)) (cadadr (cadr l)))
	       (sp1tlin (rplacd l (cddr l)) trig)))
	((member (caaadr l) (if trig '(%sin %cos) '(%sinh %cosh)) :test #'eq)
	 (push (cadr l) *lin)
	 (sp1tlin (rplacd l (cddr l)) trig))
	((sp1tlin (cdr l) trig))))

;; Rewrite a product of sines and cosines as a sum
;;
;; L is a list of sines and cosines. For example, consider the list
;;
;;  sin(x), sin(2*x), sin(3*x)
;;
;; This represents the product sin(x)*sin(2*x)*sin(3*x).
;;
;; ANS starts as sin(x). Then for each term in the rest of the list, we multiply
;; the answer that we have found so far by that term. The result will be a sum
;; of sines. In this example, sin(x)*sin(2*x) gives us
;;
;;  1/2 * (cos(x) - cos(3*x))
;;
;; In fact we don't calculate the 1/2 coefficient in sp1sintcos: you always get
;; a factor of 2^(k-1), where k is the length of the list, so this is calculated
;; at the bottom of sp1tplus. Anyway, next we calculate cos(x)*sin(3*x) and
;; -cos(3*x)*sin(3*x) and sum the answers. Note that -cos(3*x) will crop up
;; represented as ((mtimes) -1 ((%cos) ((mtimes) 3 $x))). See note in the let
;; form for info on what form ANS must take.
(defun sp1tplus (l trig)
  (if (or (null l) (null (cdr l)))
      l
      ;; ANS is a list containing the terms in a sum for the expanded
      ;; expression. Each element in this list is either of the form sc(x),
      ;; where sc is sin or cos, or of the form ((mtimes) coeff ((sc) $x)),
      ;; where coeff is some coefficient.
      ;;
      ;; multiply-sc-terms rewrites a*sc as a sum of sines and cosines. The
      ;; result is a list containing the terms in a sum which is
      ;; mathematically equal to a*sc. Assuming that term is of one of the
      ;; forms described for ANS below and that SC is of the form sc(x), the
      ;; elements of the resulting list will all be of suitable form for
      ;; inclusion into ANS.
      (flet ((multiply-sc-terms (term sc)
               (let* ((coefficient (when (mtimesp term) (cadr term)))
                      (term-sc (if (mtimesp term) (caddr term) term))
                      (expanded (sp1sintcos term-sc sc trig)))
                 ;; expanded will now either be sin(foo) or cos(foo) OR it
                 ;; will be a sum of such terms.
                 (cond
                   ((not coefficient) (list expanded))
                   ((or (atom expanded)
                        (member (caar expanded) '(%sin %cos %sinh %cosh)
                                :test 'eq))
                    (list (m* coefficient expanded)))
                   ((mplusp expanded)
                    (mapcar (lambda (summand) (m* coefficient summand))
                            (cdr expanded)))
                   (t
                    ;; SP1SINTCOS can also return numbers and constant expressions.
                    ;; Assume that's the case here.
                    (list (m* coefficient expanded))))))
             ;; Treat EXPR as a sum and return a list of its terms
             (terms-of-sum (expr)
               (if (mplusp expr) (cdr expr) (ncons expr))))

        (let ((ans (list (first l))))
          (dolist (sc (rest l))
            (setq ans (terms-of-sum
                       (m+l (mapcan (lambda (q)
                                      (multiply-sc-terms q sc)) ans)))))
          (list (list '(rat) 1 (expt 2 (1- (length l))))
                (m+l ans))))))

;; The core of trigreduce. Performs transformations like sin(x)*cos(x) =>
;; sin(2*x)
;;
;; This function only does something non-trivial if both a and b have one of
;; sin, cos, sinh and cosh as top-level operators. (Note the first term in the
;; cond: we assume that if a,b are non-atomic and not both of them are
;; hyperbolic/trigonometric then we can just multiply the two terms)
(defun sp1sintcos (a b trig)
  (let* ((x nil)
         (y nil))
    (cond ((or (atom a) (atom b)
	       (not (member (caar a) '(%sin %cos %sinh %cosh) :test #'eq))
	       (not (member (caar b) '(%sin %cos %sinh %cosh) :test #'eq)))
	   (mul3 2 a b))
	  ((prog2 (setq x (m+ (cadr a) (cadr b)) y (m- (cadr a) (cadr b)))
	       (null (eq (caar a) (caar b))))
	   (setq b (if trig '(%sin) '(%sinh)))
	   (or (eq (caar a) '%sin) (eq (caar a) '%sinh)
	       (setq y (m- y)))
	   (m+ (list b x) (list b y)))
	  ((member (caar a) '(%cos %cosh) :test #'eq)
	   (m+ (list (list (caar a)) x)
	       (list (list (caar a)) y)))
	  (trig
	   (m- (list '(%cos) y) (list '(%cos) x)))
	  ((m- (list '(%cosh) x) (list '(%cosh) y))))))

;; For COS(X)^2, TRIGBUCKET is (X (1 (COS . 2))) or, more generally,
;; (arg (numfactor-of-arg (operator . exponent)))

(defun sp1add (e)
  (let* ((n (cond ((eq (caar e) 'mexpt)
                   (cond ((= (signum1 (caddr e)) -1)
                          (prog1 (m- (caddr e))
                            (setq e (cons (list (zl-get (caaadr e) 'recip)) (cdadr e)))))
                         ((prog1 (caddr e) (setq e (cadr e))))))
                  ( 1 )))
         (arg (sp1kget (cadr e)))
         (buc nil)
         (*laws* *hyperlaws*))
    (cond ((member (caar e) '(%sin %cos %tan %cot %sec %csc) :test #'eq)
	   (cond ((setq buc (assoc (cdr arg) *trigbuckets* :test #'equal))
		  (setq *laws* *triglaws*)
		  (sp1addbuc (caar e) (car arg) n buc))
		 ((setq *trigbuckets*
			(cons (list (cdr arg) (list (car arg) (cons (caar e) n)))
			      *trigbuckets*)))))
	  ((setq buc (assoc (cdr arg) *hyperbuckets* :test #'equal))
	   (sp1addbuc (caar e) (car arg) n buc))
	  ((setq *hyperbuckets*
		 (cons (list (cdr arg) (list (car arg) (cons (caar e) n)))
		       *hyperbuckets*))))))

(defun sp1addbuc (f arg n b) ;FUNCTION, ARGUMENT, EXPONENT, BUCKET LIST
  (cond ((and (cdr b) (alike1 arg (caadr b))) ;GOES IN THIS BUCKET
	 (sp1putbuc f n (cadr b)))
	((or (null (cdr b)) (great (caadr b) arg))
	 (rplacd b (cons (list arg (cons f n)) (cdr b))))
	((sp1addbuc f arg n (cdr b)))))

(defun sp1putbuc (f n *buc)		;PUT IT IN THERE
  (do ((buc *buc (cdr buc)))
      ((null (cdr buc))
       (rplacd buc (list (cons f n))))
    (cond ((eq f (caadr buc))		;SAME FUNCTION
	   (return
	     (rplacd (cadr buc) (m+ n (cdadr buc))))) ;SO BOOST EXPONENT
	  ((eq (caadr buc) (zl-get f 'recip)) ;RECIPROCAL FUNCTIONS
	   (setq n (m- (cdadr buc) n))
	   (return
	     (cond ((signp e n) (rplacd buc (cddr buc)))
		   ((= (signum1 n) -1)
		    (rplaca (cadr buc) f)
		    (rplacd (cadr buc) (neg n)))
		   (t (rplacd (cadr buc) n)))))
	  (t (let* ((nf (zl-get (zl-get *laws* (caadr buc)) f))
                    (m nil))
	       (cond ((null nf))	;NO SIMPLIFICATIONS HERE
		     ((equal n (cdadr buc)) ;EXPONENTS MATCH
		      (rplacd buc (cddr buc))
		      (return
			(sp1putbuc1 nf n *buc))) ;TO MAKE SURE IT DOESN'T OCCUR TWICE
		     ((eq (setq m (sp1great n (cdadr buc))) 'nomatch))
		     (m (setq m (cdadr buc))
			(rplacd buc (cddr buc))
			(sp1putbuc1 nf m *buc)
			(sp1putbuc1 f (m- n m) *buc)
			(return t))
		     (t (rplacd (cadr buc) (m- (cdadr buc) n))
			(return (sp1putbuc1 nf n *buc)))))))))

(defun sp1putbuc1 (f n buc)
  (cond ((null (cdr buc))
	 (rplacd buc (list (cons f n))))
	((eq f (caadr buc))
	 (rplacd (cadr buc) (m+ n (cdadr buc))))
	((sp1putbuc1 f n (cdr buc)))))

(defun sp1great (x y)
  (let* ((a nil)
         (b nil))
    (cond ((mnump x)
	   (cond ((mnump y) (great x y)) (t 'nomatch)))
	  ((or (atom x) (atom y)) 'nomatch)
	  ((and (eq (caar x) (caar y))
		(alike (cond ((mnump (cadr x))
			      (setq a (cadr x)) (cddr x))
			     (t (setq a 1) (cdr x)))
		       (cond ((mnump (cadr y))
			      (setq b (cadr y)) (cddr y))
			     (t (setq b 1) (cdr y)))))
	   (great a b))
	  (t 'nomatch))))

(defun sp1untrep (b)
  (mapcan
   #'(lambda (buc)
       (mapcar #'(lambda (term)
		   (let* ((bas (simplifya (list (list (car term))
                                                (m* (car b) (car buc)))
                                          t)))
		     (cond ((equal (cdr term) 1) bas)
			   ((m^ bas (cdr term))))))
	       (cdr buc)))
   (cdr b)))

(defun sp1kget (e)			;FINDS NUMERIC COEFFICIENTS
  (or (and (mtimesp e) (numberp (cadr e))
	   (cons (cadr e) (m*l (cddr e))))
      (cons 1 e)))

(defun sp1sincos (l trig)
  (mapcar #'(lambda (q) (sp1sincos2 (m* (car l) (car q)) q trig)) (cdr l)))

(defun sp1sincos2 (arg l trig)
  (let* ((a nil))
    (cond ((null (cdr l)))
	  ((and
	    (setq a (member (caadr l)
                            (if (null trig)
                                '(%sinh %cosh %sinh %csch %sech %csch)
				'(%sin %cos %sin %csc %sec %csc)) :test #'eq))
	    (cddr l))		 ;THERE MUST BE SOMETHING TO MATCH TO.
	   (sp1sincos1 (cadr a) l arg trig))
	  ((sp1sincos2 arg (cdr l) trig)))))

(defun sp1sincos1 (s l arg trig)
  (let* ((g nil)
         (e 1))
    (do ((ll (cdr l) (cdr ll)))
	((null (cdr ll)) t)
      (cond ((eq s (caadr ll))
	     (setq arg (m* 2 arg))
	     (cond (trig
		    (cond ((member s '(%sin %cos) :test #'eq)
			   (setq s '%sin))
			  ((setq s '%csc e -1))))
		   (t
		    (cond ((member s '(%sinh %cosh) :test #'eq)
			   (setq s '%sinh))
			  ((setq s '%csch e -1)))))
	     (cond ((alike1 (cdadr ll) (cdadr l))
		    (sp1addto s arg (cdadr l))
		    (setq *lin (cons (m* e (cdadr l)) *lin))
		    (rplacd ll (cddr ll))   ;;;MUST BE IN THIS ORDER!!
		    (rplacd l (cddr l))
		    (return t))
		   ((eq (setq g (sp1great (cdadr l) (cdadr ll))) 'nomatch))
		   ((null g)
		    (rplacd (cadr ll) (m- (cdadr ll) (cdadr l)))
		    (sp1addto s arg (cdadr l))
		    (setq *lin (cons (m* e (cdadr l)) *lin))
		    (rplacd l (cddr l))
		    (return t))
		   (t
		    (rplacd (cadr l) (m- (cdadr l) (cdadr ll)))
		    (sp1addto s arg (cdadr ll))
		    (push (m* e (cdadr ll)) *lin)
		    (rplacd ll (cddr ll))
		    (return t))))))))

(defun sp1addto (fn arg exp)
  (setq arg (list (list fn) arg))
  (sp1add (if (equal exp 1) arg (m^ arg exp))))

(defun sp1expt (b e)
  (cond ((mexptp b)
         (power (sp1 b) (sp1 e)))
	((and (null (trigfp b)) (free e var))
	 (m^ b e))
	((equal b '$%e)
	 (sp1expt2 e))
	((and (null (eq var '*novar)) (free b var))
	 (sp1expt2 (m* (list '(%log) b) e)))
	((and (consp b) (consp (car b)) (member (caar b) '(%sin %cos %tan %cot %sec %csc
								%sinh %cosh %tanh %coth %sech %csch) :test #'eq))
	 (cond ((= (signum1 e) -1)
		(sp1expt (list (list (zl-get (caar b) 'recip)) (cadr b))
			 (neg e)))
	       ((and (signp g e)
		     (member (caar b) '(%sin %cos %sinh %cosh) :test #'eq))
		(funcall (cdr (assoc (caar b) *sc^ndisp* :test #'eq)) e (cadr b)))
	       ((m^ b e))))
	((m^ b e))))

(defun sp1expt2 (e)
  (let* ((*schatc-ans* (m2 e '((mplus) ((coeffpp) (fr freevar)) ((coeffpp) (exp true)))))
         (fr (cdr (assoc 'fr *schatc-ans* :test #'eq)))
         (exp (cdr (assoc 'exp *schatc-ans* :test #'eq))))
    (cond ((equal fr 0)
	   (m^ '$%e exp))
	  ((m* (m^ '$%e fr) (m^ '$%e exp))))))

;; Split TERMS into (VALUES NON-NEG OTHER) where NON-NEG and OTHER are a
;; partition of the elements of TERMS. Expressions that are known not to be
;; negative are placed in NON-NEG and all others end up in OTHER.
;;
;; This function is used to safely split products when expanding logarithms to
;; avoid accidentally ending up with something like
;;
;;   log(1 - x) => log(-1) + log(x-1).
;;
;; Note that we don't check a term is strictly positive: if it was actually
;; zero, the logarithm was bogus in the first place.
(defun non-negative-split (terms)
  (let ((non-neg) (other))
    (dolist (term terms)
      (if (memq ($sign term) '($pos $pz $zero))
          (push term non-neg)
          (push term other)))
    (values non-neg other)))

;; Try to expand a logarithm for use in a power series in VAR by splitting up
;; products.
(defun sp1log (e &optional no-recurse)
  (cond
    ;; If E is free of VAR, is an atom, or we're supposed to be reducing rather
    ;; than expanding, then just return E.
    ((or *trigred (atom e) (free e var))
     (list '(%log) e))

    ;; The logarithm of a sum doesn't simplify very nicely, but call $factor to
    ;; see if we can pull out one or more terms and then recurse (setting
    ;; NO-RECURSE to make sure we don't end up in a loop)
    ((eq (caar e) 'mplus)
     (let* ((exp (m1- e)) *a *n)
       (declare (special *n *a))
       (cond
         ((smono exp var)
          (list '(%log) e))
         ((not no-recurse)
          (sp1log ($factor e) t))
         (t (sp1log2 e)))))

    ;; A product is much more promising. Do the transformation log(ab) =>
    ;; log(a)+log(b) and pass it to SP1 for further simplification.
    ;;
    ;; We need to be a little careful here because eg. factor(1-x) gives
    ;; -(x-1). We don't want to end up with a log(-1) term! So check the sign of
    ;; terms and only pull out the terms we know to be non-negative. If the
    ;; argument was a negative real in the first place then we'd already got
    ;; rubbish, but otherwise we won't pull out anything we don't want.
    ((eq (caar e) 'mtimes)
     (multiple-value-bind (non-neg other) (non-negative-split (cdr e))
       (cond
         ((null non-neg) (sp1log2 e))
         (t
          (sp1 (m+l (mapcar #'sp1log (append other non-neg))))))))

    ;; Similarly, transform log(a^b) => b log(a) and pass back to SP1.
    ((eq (caar e) 'mexpt)
     (sp1 (m* (caddr e) (list '(%log) (cadr e)))))

    ;; If we can't find any other expansions, pass the result to SP1LOG2, which
    ;; tries again after expressing E as integrate(diff(e)/e).
    ((sp1log2 e))))

;; We didn't manage to expand the expression, so make use of the fact that
;; diff(log(f(x)), x) = f'(x)/f(x) and return integrate(f'(x)/f(x), x), hoping
;; that a later stage will be able to do something useful with it.
;;
;; We have to be a little bit careful because an indefinite integral might have
;; the wrong constant term. Instead, rewrite as
;;
;;  log(f(x0+h)) = log(f(x0+h)) - log(f(x0)) + log(f(x0))
;;               = integrate(diff(log(f(x0+k)), k), k, 0, h) + log(f(x0))
;;               = integrate(diff(f(x0+k))/f(x0+k), k, 0, h) + log(f(x0))
;;
;; The "x0" about which we expand is always zero (see the code in $powerseries)
(defun sp1log2 (e)
  (when $verbose
    (mtell (intl:gettext "trigreduce: failed to expand.~%~%"))
    (show-exp (list '(%log) e))
    (mtell (intl:gettext "trigreduce: try again after applying rule:~2%~M~%~%")
           (list '(mlabel) nil
                 (out-of
                  `((mequal)
                    ((%log) ,e)
                    ((%integrate)
                     ((mquotient) ((%derivative) ,e ,var 1) ,e) ,var))))))
  (let* ((dummy-sym ($gensym)))
    (m+ (list '(%log) ($limit e var 0))
        (list '(%integrate)
              (maxima-substitute dummy-sym var
                                 (sp1 (m// (sdiff e var) e)))
              dummy-sym 0 var))))

(defun sp1trig (e)
  (cond ((atom (cadr e)) (simplify e))
	((eq (caaadr e) (zl-get (caar e) '$inverse)) (sp1 (cadadr e)))
	((eq (caaadr e) (zl-get (zl-get (caar e) 'recip) '$inverse))
	 (sp1 (m// (cadadr e))))
	((and (null *trigred) (null *noexpand) (eq (caaadr e) 'mplus))
	 (sp1trigex e))
	( e )))

;; Return the expansion of ((trigfun) ((mplus) a b)). For example sin(a+b) =
;; sin(a)cos(b) + cos(a)sin(b).
(defun expand-trig-of-sum (trigfun a b)
  (flet ((expand-it (op f1 f2 f3 f4)
           (funcall op
                    (m* (sp1trig (list f1 a)) (sp1trig (list f2 b)))
                    (m* (sp1trig (list f3 a)) (sp1trig (list f4 b))))))
    (ecase trigfun
      (%sin  (expand-it #'add2* '(%sin)  '(%cos)  '(%cos)  '(%sin)))
      (%cos  (expand-it #'sub*  '(%cos)  '(%cos)  '(%sin)  '(%sin)))
      (%sinh (expand-it #'add2* '(%sinh) '(%cosh) '(%cosh) '(%sinh)))
      (%cosh (expand-it #'sub*  '(%cosh) '(%cosh) '(%sinh) '(%sinh))))))

;; Try to expand f(a+b) where f is sin, cos, sinh or cosh.
(defun sp1trigex (e)
  (schatchen-cond w
    ;; Ideally, we'd like to split the argument of the trig function into terms
    ;; that involve VAR and those that are free of it.
    ((m2 (cadr e) '((mplus) ((coeffpp) (a freevar)) ((coeffpp) (b true))))
     (a b)

     ;; Make sure that if B is zero then so is A (to simplify the cond)
     (when (signp e b) (rotatef a b))

     ;; Assuming we didn't just swap them, A will be free of VAR and B will
     ;; contain any other terms. If A is zero (because the argument of trig
     ;; function is a sum of terms, all of which involve VAR), then fall back on
     ;; a different splitting, by terms of taking the first term of B.
     (cond
       ((and (signp e a)
             (not (atom b))
             (eq (caar b) 'mplus))
        (expand-trig-of-sum (caar e)
                            (cadr b)
                            (if (cdddr b)
                                (cons (car b) (cddr b))
                                (caddr b))))

       ;; For some weird reason, B isn't a sum. Give up.
       ((signp e a) e)

       ;; Do the splitting we intended in the first place.
       (t
        (expand-trig-of-sum (caar e) a b))))

    ;; E doesn't match f(a+b). Return it unmodified.
    (t nil e)))

(defun sp1atrig (fn exp)
  (cond ((atom exp)
	 (sp1atrig2 fn exp))
	((eq fn (zl-get (caar exp) '$inverse))
	 (sp1 (cadr exp)))
	(t (sp1atrig2 fn exp))))

(defun sp1atrig2 (fn exp)
  (cond ((member fn '(%cot %sec %csc %coth %sech %csch) :test #'eq)
	 (setq exp (sp1 (m// exp))
	       fn (cdr (assoc fn '((%acot . %atan) (%asec . %acos) (%acsc . %asin)
				  (%acoth . %atanh) (%asech . %acosh) (%acsch . %asinh)) :test #'eq)))))
  (cond ((and (null *trigred)
	      (member fn '(%acos %acosh) :test #'eq))
	 (m+ half%pi (list
		      (list (cdr (assoc fn '((%acos . %asin) (%acosh . %asinh)) :test #'eq)))
		      exp)))
	((list (list fn) exp))))

(defun sin^n (%n v)
  (sc^n %n v (if (oddp %n) '(%sin) '(%cos)) (not (oddp %n))
	(m^ -1 (m+ (ash %n -1) 'k))))

(defun sinh^n (%n v)
  (if (oddp %n)
      (sc^n %n v '(%sinh) nil (m^ -1 'k))
      (if (zerop (mod %n 4))
	  (sc^n %n v '(%cosh) t (m^ -1 'k))
	  (m- (sc^n %n v '(%cosh) t (m- (m^ -1 'k)))))))

(defun cos^n (%n v)
  (sc^n %n v '(%cos) (not (oddp %n)) 1))

(defun cosh^n (%n v)
  (sc^n %n v '(%cosh) (not (oddp %n)) 1))

(defun sc^n (%n v fn fl coef)
  (when (minusp %n)
    (merror "trigreduce: internal error; %N must be nonnegative, found: ~M") %n)
  (m* (list '(rat) 1 (expt 2 %n))
      (m+ (if fl
              (list '(%binomial) %n (ash %n -1))
              0)
	  (maxima-substitute v 'trig-var
			     (dosum (m+ (m* 2
					    (list '(%binomial) %n 'k)
					    coef
					    (list fn (m* 'trig-var
							 (m+ %n (m* -2 'k))))))
				    'k 0 (ash (1- %n) -1) t)))))
