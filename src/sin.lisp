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

(macsyma-module sin)

;;; Reference:  J. Moses, Symbolic Integration, MIT-LCS-TR-047, 12-1-1967.
;;; http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-047.pdf.
;;;;
;;;; Unfortunately, some important pages in the scan are all black.
;;;;
;;;; A version with the missing pages is available (2008-12-14) from
;;;; http://www.softwarepreservation.org/projects/LISP/MIT

(defvar *debug-integrate* nil
  "Enable debugging for the integrator routines.")

;; When T do not call the risch integrator. This flag can be set by the risch 
;; integrator to avoid endless loops when calling the integrator from risch.
(defvar *in-risch-p* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicate functions

;; Note: varp and freevarp are not used in this file anymore.  But
;; they are used in other files.  Someday, varp and freevarp should be
;; moved elsewhere.
(declaim (inline varp))
(defun varp (x)
  (declare (special var))
  (alike1 x var))

(defun freevar (a)
  (declare (special var))
  (cond ((atom a) (not (eq a var)))
	((varp a) nil)
	((and (not (atom (car a)))
	      (member 'array (cdar a) :test #'eq))
	 (cond ((freevar (cdr a)) t)
	       (t (merror "~&FREEVAR: variable of integration appeared in subscript."))))
	(t (and (freevar (car a)) (freevar (cdr a))))))

;; Same as varp, but the second arg specifiies the variable to be
;; tested instead of using the special variable VAR.
(defun varp2 (x var2)
  (alike1 x var2))

;; Like freevar bug the second arg specifies the variable to be tested
;; instead of using the special variable VAR.
(defun freevar2 (a var2)
  (cond ((atom a) (not (eq a var2)))
	((varp2 a var2) nil)
	((and (not (atom (car a)))
	      (member 'array (cdar a) :test #'eq))
	 (cond ((freevar2 (cdr a) var2) t)
	       (t (merror "~&FREEVAR: variable of integration appeared in subscript."))))
	(t (and (freevar2 (car a) var2) (freevar2 (cdr a) var2)))))

(defun integerp1 (x)
  "Returns 2*x if 2*x is an integer, else nil"
  (integerp2 (mul2* 2 x)))

(defun integerp2 (x)
  "Returns x if x is an integer, else false"
  (let (u)
    (cond ((not (numberp x)) nil)
	  ((not (floatp x)) x)
	  ((prog2 (setq u (maxima-rationalize x))
	       (equal (cdr u) 1))
           (car u)))))

;; This predicate is used with m2 pattern matcher.
;; A rational expression in var2.
(defun rat8 (ex var2)
  (cond ((or (varp2 ex var2) (freevar2 ex var2))
	 t)
	((member (caar ex) '(mplus mtimes) :test #'eq)
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat8 (car u) var2))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 nil)
	((integerp (caddr ex))
	 (rat8 (cadr ex) var2))))

;; Predicate for m2 pattern matcher
(defun rat8prime (c var2)
  (and (rat8 c var2)
       (or (not (mnump c))
           (not (zerop1 c)))))

;; Predicate for m2 patter matcher
(defun elem (a expres var2)
  (cond ((freevar2 a var2) t)
	((atom a) nil)
	((m2 a expres) t)
	(t (every #'(lambda (f)
                      (elem f expres var2))
                  (cdr a)))))

;; Like freevar0 (in hypgeo.lisp), but we take a second arg to specify
;; the variable instead of implicitly using VAR.
(defun freevar02 (m var2)
  (cond ((equal m 0) nil)
        (t (freevar2 m var2))))

;; Like free1 in schatc, but takes an extra arg for the variable.
(defun free12 (a var2)
  (and (null (pzerop a))
       (free a var2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rationalizer (x)
  (let ((ex (simplify ($factor x))))
    (if (not (alike1 ex x)) ex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II of the Integrator
;;;
;;; Check if the problem can be transformed or solved by special methods.
;;; 11 Methods are implemented by Moses, some more have been added.

(let (powerl)
  ;; POWERL is initialized in INTEGRATOR to NIL and can be modified in
  ;; INTFORM in certain cases and is read by INTEGRATOR in some cases.
  ;; Instead of a global special variable, use a closure.

  ;; It would be really good to get rid of the special variable *EXP*
  ;; used only in INTFORM and INTEGRATOR.  I (rtoy) haven't been able
  ;; to figure out exactly how to do that.

  (defun intform (expres var2 &aux w arg)
    (declare (special *exp*))
    (cond ((freevar2 expres var2) nil)
          ((atom expres) nil)

          ;; Map the function intform over the arguments of a sum or a product
	  ((member (caar expres) '(mplus mtimes) :test #'eq)
           (some #'(lambda (ex)
                     (intform ex var2))
                 (cdr expres)))

          ((or (eq (caar expres) '%log)
               (arcp (caar expres)))
           (cond
             ;; Method 9: Rational function times a log or arctric function
	     ((setq arg (m2 *exp*
			    `((mtimes) ((,(caar expres)) (b rat8 ,var2))
			      ((coefftt) (c rat8prime ,var2)))))
	      ;; Integrand is of the form R(x)*F(S(x)) where F is a log, or 
	      ;; arctric function and R(x) and S(x) are rational functions.
	      (ratlog var2 (cons (cons 'a expres) arg)))
	     (t
	      (prog (y z)
	         (cond
	           ((setq y (intform (cadr expres) var2)) (return y))
	         
	           ;; Method 10: Rational function times log(b*x+a)
		   ((and (eq (caar expres) '%log)
                         (setq z (m2-b*x+a (cadr expres) var2))
                         (setq y (m2 *exp*
                                     `((mtimes)
                                       ((coefftt) (c rat8 ,var2))
                                       ((coefftt) (d elem ,expres ,var2))))))
		    (return
		      (let ((a (cdr (assoc 'a z :test #'eq)))
			    (b (cdr (assoc 'b z :test #'eq)))
			    (c (cdr (assoc 'c y :test #'eq)))
			    (d (cdr (assoc 'd y :test #'eq)))
		            (newvar (gensym "intform")))
		        ;; keep var from appearing in questions to user
		        (putprop newvar t 'internal)
		        ;; Substitute y = log(b*x+a) and integrate again
		        (substint
		         expres
		         newvar
		         (integrator
			  (muln
			   (list (maxima-substitute
				  `((mquotient) ((mplus) ((mexpt) $%e ,newvar)
					                 ((mtimes) -1 ,a))
				                ,b)
				  var2
				  c)
			         `((mquotient) ((mexpt) $%e ,newvar) ,b)
			         (maxima-substitute newvar expres d))
			   nil)
			  newvar)
                         var2
                         *exp*))))
		   (t (return nil)))))))
        
          ;; We have a special function with an integral on the property list.
          ;; After the integral property was defined for the trig functions,
          ;; in rev 1.52, need to exclude trig functions here.
          ((and (not (atom (car expres)))
                (not (optrig (caar expres)))
	        (not (eq (caar expres) 'mexpt))
	        (get (caar expres) 'integral))
           (when *debug-integrate*
             (format t "~&INTFORM: found 'INTEGRAL on property list~%"))
           (cond
             ((setq arg
                    (m2 *exp* `((mtimes) ((,(caar expres)) (b rat8 ,var2)) ((coefftt) (c rat8prime ,var2)))))
              ;; A rational function times the special function.
              ;; Integrate with the method integration-by-parts.
              (partial-integration (cons (cons 'a expres) arg) var2))
             ;; The method of integration-by-parts can not be applied.
             ;; Maxima tries to get a clue for the argument of the function which
             ;; allows a substitution for the argument.
             ((intform (cadr expres) var2))
             (t nil)))
        
          ;; Method 6: Elementary function of trigonometric functions
	  ((optrig (caar expres))
	   (cond ((not (setq w (m2-b*x+a (cadr expres) var2)))
		  (intform (cadr expres) var2))
	         (t
		  (prog2
                      (setq powerl t)
                      (monstertrig *exp* var2 (cadr expres))))))
        
	  ((and (eq (caar expres) '%derivative)
                (eq (caar *exp*) (caar expres))
                (checkderiv *exp* var2)))
        
          ;; Stop intform if we have not a power function.
          ((not (eq (caar expres) 'mexpt)) nil)
        
          ;; Method 2: Substitution for an integral power
          ((integerp (caddr expres)) (intform (cadr expres) var2))
        
          ;; Method 1: Elementary function of exponentials
          ((freevar2 (cadr expres) var2)
           (cond ((setq w (m2-b*x+a (caddr expres) var2))
                  (superexpt *exp* var2 (cadr expres) w))
                 ((intform (caddr expres) var2))
                 ((and (eq '$%e (cadr expres))
                       (isinop (caddr expres) '%log))
                  ;; Found something like exp(r*log(x))
                  (let* (($%e_to_numlog t)
                         ($radexpand nil) ; do not simplify sqrt(x^2) -> abs(x)
                         (nexp (resimplify *exp*)))
                    (cond ((alike1 *exp* nexp) nil)
                          (t (integrator (setq *exp* nexp) var2)))))
                 (t nil)))
        
          ;; The base is not a rational function. Try to get a clue for the base.
	  ((not (rat8 (cadr expres) var2))
	   (intform (cadr expres) var2))
        
          ;; Method 3: Substitution for a rational root
	  ((and (setq w (m2-ratrootform (cadr expres) var2)) ; e*(a*x+b) / (c*x+d)
                (denomfind (caddr expres))) ; expon is ratnum
           (or (progn
                 (setq powerl t)
                 (ratroot *exp* var2 (cadr expres) w))
               (inte *exp* var2)))
        
          ;; Method 4: Binomial - Chebyschev
	  ((not (integerp1 (caddr expres))) ; 2*exponent not integer
	   (cond ((m2-chebyform *exp* var2)
		  (chebyf *exp* var2))
	         (t (intform (cadr expres) var2))))
        
          ;; Method 5: Arctrigonometric substitution
	  ((setq w (m2-c*x^2+b*x+a (cadr expres) var2)) ; sqrt(c*x^2+b*x+a)
	   #+nil
	   (format t "expres = sqrt(c*x^2+b*x+a)~%")
	   ;; I think this is method 5, arctrigonometric substitutions.
	   ;; (Moses, pg 80.)  The integrand is of the form
	   ;; R(x,sqrt(c*x^2+b*x+a)).  This method first eliminates the b
	   ;; term of the quadratic, and then uses an arctrig substitution.
	   (inte *exp* var2))
        
          ;; Method 4: Binomial - Chebyschev
	  ((m2-chebyform *exp* var2)
	   (chebyf *exp* var2))
        
          ;; Expand expres.
          ;; Substitute the expanded factor into the integrand and try again.
	  ((not (m2 (setq w ($expand (cadr expres)))
                    (cadr expres)))
	   (prog2
               (setq *exp* (maxima-substitute w (cadr expres) *exp*))
               (intform (simplify (list '(mexpt) w (caddr expres)))
                        var2)))
        
          ;; Factor expres.
          ;; Substitute the factored factor into the integrand and try again.
	  ((setq w (rationalizer (cadr expres)))
	   ;; The forms below used to have $radexpand set to $all.  But I
	   ;; don't think we really want to do that here because that makes
	   ;; sqrt(x^2) become x, which might be totally wrong.  This is one
	   ;; reason why we returned -4/3 for the
	   ;; integrate(sqrt(x+1/x-2),x,0,1).  We were replacing
	   ;; sqrt((x-1)^2) with x - 1, which is totally wrong since 0 <= x
	   ;; <= 1.
	   (setq *exp* (let (($radexpand $radexpand))
		         (maxima-substitute w (cadr expres) *exp*)))
	   (intform (let (($radexpand '$all))
		      (simplify (list '(mexpt) w (caddr expres))))
                    var2))))
  ;;------------------------------------------------------------------------------

  ;; This is the main integration routine.  It is called from sinint.

  (defun integrator (*exp* var2 &optional stack)
    (declare (special *exp*))
    (prog (y const w arcpart coef integrand result)
       (declare (special *integrator-level*))
       (setq powerl nil)
       ;; Increment recursion counter
       (incf *integrator-level*)
     
       ;; Trivial case. exp is not a function of var2.
       (if (freevar2 *exp* var2) (return (mul2* *exp* var2)))
     
       ;; Remove constant factors
       (setq w (partition *exp* var2 1))
       (setq const (car w))
       (setq *exp* (cdr w))
       #+nil
       (progn
         (format t "w = ~A~%" w)
         (format t "const = ~A~%" const)
         (format t "exp = ~A~%" *exp*))
     
       (cond ;; First stage, Method I: Integrate a sum.
         ((mplusp *exp*)
          (return (mul2* const (integrate1 (cdr *exp*) var2))))
           
         ;; Convert atan2(a,b) to atan(a/b) and try again.
         ((setq w (isinop *exp* '%atan2))
          (setq *exp*
                (maxima-substitute (take '(%atan) (div (cadr w) (caddr w)))
                                   w
                                   *exp*))
          (return (mul* const
                        (integrator *exp* var2 stack))))
           
         ;; First stage, Method II: Integrate sums.
	 ((and (not (atom *exp*))
	       (eq (caar *exp*) '%sum))
	  (return (mul2* const (intsum *exp* var2))))
           
         ;; First stage, Method III: Try derivative-divides method.
         ;; This is the workhorse that solves many integrals.
         ((setq y (diffdiv *exp* var2))
	  (return (mul2* const y))))
     
       ;; At this point, we have EXP as a product of terms.  Make Y a
       ;; list of the terms of the product.
       (setq y (cond ((mtimesp *exp*)
		      (cdr *exp*))
		     (t
		      (list *exp*))))
     
       ;; Second stage:
       ;; We're looking at each term of the product and check if we can
       ;; apply one of the special methods.
     loop
       #+nil
       (progn
         (format t "car y =~%")
         (maxima-display (car y)))
       (cond ((rat8 (car y) var2)
	      #+nil
	      (format t "In loop, go skip~%")
	      (go skip))
	     ((and (setq w (intform (car y) var2))
		   ;; Do not return a noun form as result at this point, because
		   ;; we would like to check for further special integrals.
		   ;; We store the result for later use.
		   (setq result w)
		   (not (isinop w '%integrate)))
	      #+nil
	      (format t "In loop, case intform~%")
	      (return (mul2* const w)))
	     (t
	      #+nil
	      (format t "In loop, go special~%")
	      ;; Store a possible partial result
	      (setq result w)
	      (go special)))
     skip
       (setq y (cdr y))
       (cond ((null y)
              ;; Method 8: Rational functions
	      (return (mul2* const (cond ((setq y (powerlist *exp* var2)) y)
				         (t (ratint *exp* var2)))))))
       (go loop)
        
     special
       ;; Third stage: Try more general methods
     
       ;; SEPARC SETQS ARCPART AND COEF SUCH THAT
       ;; COEF*ARCEXP=EXP WHERE ARCEXP IS OF THE FORM
       ;; ARCFUNC^N AND COEF IS ITS ALGEBRAIC COEFFICIENT
       (multiple-value-setq
           (arcpart coef)
         (separc *exp*))
     
       #+nil
       (progn
         (format t "arcpart = ~A~%" arcpart)
         (format t "coef =~%")
         (maxima-display coef))
       (cond ((and (not (null arcpart))
		   (do  ((stacklist stack (cdr stacklist)))
		        ((null stacklist) t)
		     (cond ((alike1 (car stacklist) coef)
			    (return nil))))
		   (not (isinop (setq w (let ((stack (cons coef stack)))
					  (integrator coef var2 stack)))
			        '%integrate))
		   (setq integrand (mul2 w (sdiff arcpart var2)))
		   (do ((stacklist stack (cdr stacklist)))
		       ((null stacklist) t)
		     (cond ((alike1 (car stacklist) integrand)
			    (return nil))))
		   (not (isinop
		         (setq y (let ((stack (cons integrand stack))
				       (integ integrand))
				   (integrator integ var2 stack)))
		         '%integrate)))
	      (return (add* (list '(mtimes) const w arcpart)
			    (list '(mtimes) -1 const y))))
	     (t
	      (return
		(mul* const
		      (cond ((setq y (scep *exp* var2))
			     (cond ((cddr y)
				    #+nil
				    (progn
				      (format t "cddr y =~%")
				      (maxima-display (cddr y)))
				    (integrator ($trigreduce *exp*) var2 stack))
				   (t (sce-int (car y) (cadr y) var2))))
			    ;; I don't understand why we do this. This
			    ;; causes the stack overflow in Bug
			    ;; 1487703, because we keep expanding *exp*
			    ;; into a form that matches the original
			    ;; and therefore we loop forever.  To
			    ;; break this we keep track how how many
			    ;; times we've tried this and give up
			    ;; after 4 (arbitrarily selected) times.
			    ((and (< *integrator-level* 4)
				  (not (alike1 *exp* (setq y ($expand *exp*)))))
			     #+nil
			     (progn
			       (format t "*exp* = ~A~%" *exp*)
			       (maxima-display *exp*)
			       (format t "y   = ~A~%" y)
			       (maxima-display y)
			       (break))
			     (integrator y var2 stack))
			    ((and (not powerl)
				  (setq y (powerlist *exp* var2)))
			     y)
			    ((and (not *in-risch-p*) ; Not called from rischint
			          (setq y (rischint *exp* var2))
				  ;; rischint has not found an integral but
				  ;; returns a noun form. Do not return that
				  ;; noun form as result at this point, but
				  ;; store it for later use.
				  (setq result y)
				  (not (isinop y '%integrate)))
			     y)
			    ((setq y (integrate-exp-special *exp* var2))
			     ;; Maxima found an integral for a power function
			     y)
			    (t
			     ;; Integrate-exp-special has not found an integral
			     ;; We look for a previous result obtained by
			     ;; intform or rischint.
			     (if result
				 result
				 (list '(%integrate) *exp* var2)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun separc (ex)
  (let (arcpart coef)
    (labels
        ((arcfuncp (ex)
           (and (not (atom ex))
                (or (arcp (caar ex))
	            (eq (caar ex) '%log) ; Experimentally treat logs also.
	            (and (eq (caar ex) 'mexpt)
		         (integerp2 (caddr ex))
		         (> (integerp2 (caddr ex)) 0)
		         (arcfuncp (cadr ex))))))
         (arclist (list)
           (cond ((null list)
                  nil)
	         ((and (arcfuncp (car list))
                       (null arcpart))
	          (setq arcpart (car list))
                  (arclist (cdr list)))
	         (t
                  (setq coef (cons (car list) coef))
	          (arclist (cdr list))))))
      (cond ((arcfuncp ex)
             (setq arcpart ex
                   coef 1))
	    ((and (consp ex)
                  (eq (caar ex) 'mtimes))
	     (arclist (cdr ex))
	     (setq coef (cond ((null (cdr coef))
                               (car coef))
			      (t
                               (setq coef (cons (car ex) coef)))))))
      (values arcpart coef))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Five pattern for the Integrator and other routines.

;; This is matching the pattern e*(a*x+b)/(c*x+d), where
;; a, b, c, d, and e are free of x, and x is the variable of integration.
(defun m2-ratrootform (expr var2)
  (m2 expr
      `((mtimes)
        ((coefftt) (e freevar2 ,var2))
        ((mplus)
         ((coeffpt) (a freevar2 ,var2) (var varp2 ,var2))
         ((coeffpt) (b freevar2 ,var2)))
        ((mexpt)
         ((mplus)
          ((coeffpt) (c freevar2 ,var2) (var varp2 ,var2))
          ((coeffpt) (d freevar2 ,var2)))
         -1))))

;; This is for matching the pattern a*x^r1*(c1+c2*x^q)^r2.
(defun m2-chebyform (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (var varp2 ,var2) (r1 numberp))
        ((mexpt)
         ((mplus)
          ((mtimes)
           ((coefftt) (c2 freevar2 ,var2))
           ((mexpt) (var varp2 ,var2) (q free12 ,var2)))
          ((coeffpp) (c1 freevar2 ,var2)))
         (r2 numberp))
        ((coefftt) (a freevar2 ,var2)))))

;; Pattern to match b*x + a
(defun m2-b*x+a (expr var2)
  (m2 expr
      `((mplus)
        ((coeffpt) (b freevar2 ,var2) (x varp2 ,var2))
        ((coeffpt) (a freevar2 ,var2)))))

;; This is the pattern c*x^2 + b * x + a.
(defun m2-c*x^2+b*x+a (expr var2)
  (m2 expr
      `((mplus)
        ((coeffpt) (c freevar2 ,var2) ((mexpt) (x varp2 ,var2) 2))
        ((coeffpt) (b freevar2 ,var2) (x varp2 ,var2))
        ((coeffpt) (a freevar2 ,var2)))))

;; This is the pattern (a*x+b)*(c*x+d)
;; NOTE:  This doesn't seem to be used anywhere in Maxima.
(defun m2-a*x+b/c*x+d (expr)
  (m2 expr
      `((mtimes)
        ((mplus)
         ((coeffpt) (a freevar) (var varp))
         ((coeffpt) (b freevar)))
        ((mplus)
         ((coeffpt) (c freevar) (var varp))
         ((coeffpt) (d freevar))))))

(defun optrig (x)
  (member x '(%sin %cos %sec %tan %csc %cot) :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 1: Integrate a sum

;;after finding a non-integrable summand usually better to pass rest to risch
(defun integrate1 (expr var2)
  (do ((terms expr (cdr terms)) (ans))
      ((null terms) (addn ans nil))
    (let ($liflag)					; don't gen li's for
      (push (integrator (car terms) var2) ans))		; parts of integrand
    (when (and (not *in-risch-p*)                     ; Not called from rischint
               (not (free (car ans) '%integrate))
               (cdr terms))
	  (return (addn (cons (rischint (cons '(mplus) terms) var2) (cdr ans))
			nil)))))

(defun scep (expr var2 &aux trigl ex)	; Product of SIN, COS, EXP
  (and (mtimesp expr)			;	of linear args.
       (loop for fac in (cdr expr) do
	     (cond ((atom fac) (return nil))
		   ((trig1 (car fac))
		    (if (linearp (cadr fac) var2) (push fac trigl)
			(return nil)))
		   ((and (mexptp fac)
			 (eq (cadr fac) '$%e)
			 (linearp (caddr fac) var2))
		    ;; should be only one exponential factor
		    (setq ex fac))
		   (t (return nil)))
	     finally (return (cons ex trigl)))))

;; Integrates exponential * sin or cos, all with linear args.

(defun sce-int (expr s-c var2)		; EXP is non-trivial
  (let* ((e-coef (car (islinear (caddr expr) var2)))
         (sc-coef (car (islinear (cadr s-c) var2)))
         (sc-arg (cadr s-c))
         (abs-val (add (power e-coef 2) (power sc-coef 2))))
    (if (zerop1 abs-val)
        ;; The numerator is zero. Exponentialize the integrand and try again.
        (integrator ($exponentialize (mul expr s-c)) var2)
        (mul (div expr abs-val)
             (add (mul e-coef s-c)
                  (if (eq (caar s-c) '%sin)
                      (mul* (neg sc-coef) `((%cos) ,sc-arg))
                      (mul* sc-coef `((%sin) ,sc-arg))))))))

(defun checkderiv (expr var2)
  (checkderiv1 (cadr expr) (cddr expr) () var2))

;; CHECKDERIV1 gets called on the expression being differentiated,
;; an alternating list of variables being differentiated with
;; respect to and powers thereof, and a reversed list of the latter
;; that have already been examined.  It returns either the antiderivative
;; or (), saying this derivative isn't wrt the variable of integration.

(defun checkderiv1 (expr wrt old-wrt var2)
  (cond ((varp2 (car wrt) var2)
	 (if (equal (cadr wrt) 1)	;Power = 1?
	     (if (null (cddr wrt))	;single or partial
		 (if (null old-wrt)
		     expr		;single
		     `((%derivative), expr ;partial in old-wrt
		       ,.(nreverse old-wrt)))
		 `((%derivative) ,expr	;Partial, return rest
		   ,.(nreverse old-wrt)
		   ,@(cddr wrt)))
	     `((%derivative) ,expr	;Higher order, reduce order
	       ,.(nreverse old-wrt)
	       ,(car wrt) ,(add* (cadr wrt) -1)
	       ,@ (cddr wrt))))
	((null (cddr wrt)) () )		;Say it doesn't apply here
	(t (checkderiv1 expr (cddr wrt)	;Else we check later terms
			(list* (cadr wrt) (car wrt) old-wrt)
                        var2))))

(defun integrallookups (expr var2)
  (let (form dummy-args real-args)
  (cond
	((eq (caar expr) 'mqapply)
	 ;; Transform to functional form and try again.
	 ;; For example:
	 ;; ((MQAPPLY SIMP) (($PSI SIMP ARRAY) 1) $X)
	 ;; => (($PSI) 1 $X)
	 (integrallookups `((,(caaadr expr)) ,@(cdadr expr) ,@(cddr expr))
                          var2))

	;; Lookup algorithm for integral of a special function. 
	;; The integral form is put on the property list, and can be a 
	;; lisp function of the args.  If the form is nil, or evaluates 
        ;; to nil, then return noun form unevaluated.
	((and (not (atom (car expr)))
	    (setq form (get (caar expr) 'integral))
	    (setq dummy-args (car form))
	    (setq real-args (cdr expr))
	    ;; search through the args of expr and find the arg containing var2
	    ;; look up the integral wrt this arg from form
	    (setq form
	      (do ((x real-args (cdr x))
		   (y (cdr form) (cdr y)))
		  ((or (null x) (null y)) nil)
		  (if (not (freevar2 (car x) var2)) (return (car y)))))
	    ;; If form is a function then evaluate it with actual args
	    (or (not (functionp form))
		(setq form (apply form real-args))))
	 (when *debug-integrate*
	   (format t "~&INTEGRALLOOKUPS: Found integral ~A.~%" (caar expr)))
	 (substitutel real-args dummy-args form))

	((eq (caar expr) 'mplus)
	 (muln (list '((rat simp) 1 2) expr expr) nil))

	(t nil))))

;; Define the antiderivatives of some elementary special functions.
;; This may not be the best place for this definition, but it is close
;; to the original code.
;; Antiderivatives that depend on the logabs flag are defined further below.
(defprop %log  ((x) ((mplus) ((mtimes) x ((%log) x)) ((mtimes) -1 x))) integral)
(defprop %sin  ((x) ((mtimes) -1 ((%cos) x))) integral)
(defprop %cos  ((x) ((%sin) x)) integral)
(defprop %sinh ((x) ((%cosh) x)) integral)
(defprop %cosh ((x) ((%sinh) x)) integral)
;; No need to take logabs into account for tanh(x), because cosh(x) is positive.
(defprop %tanh ((x) ((%log) ((%cosh) x))) integral)
(defprop %sech ((x) ((%atan) ((%sinh) x))) integral)
(defprop %asin ((x) ((mplus) ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) ((rat) 1 2)) ((mtimes) x ((%asin) x)))) integral)
(defprop %acos ((x) ((mplus) ((mtimes) -1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) ((rat) 1 2))) ((mtimes) x ((%acos) x)))) integral)
(defprop %atan ((x) ((mplus) ((mtimes) x ((%atan) x)) ((mtimes) ((rat) -1 2) ((%log) ((mplus) 1 ((mexpt) x 2)))))) integral)
(defprop %acsc ((x) ((mplus) ((mtimes) ((rat) -1 2) ((%log) ((mplus) 1 ((mtimes) -1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2))) ((rat) 1 2)))))) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2))) ((rat) 1 2))))) ((mtimes) x ((%acsc) x)))) integral)
(defprop %asec ((x) ((mplus) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mtimes) -1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2))) ((rat) 1 2)))))) ((mtimes) ((rat) -1 2) ((%log) ((mplus) 1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2))) ((rat) 1 2))))) ((mtimes) x ((%asec) x)))) integral)
(defprop %acot ((x) ((mplus) ((mtimes) x ((%acot) x)) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mexpt) x 2)))))) integral)
(defprop %asinh ((x) ((mplus) ((mtimes) -1 ((mexpt) ((mplus) 1 ((mexpt) x 2)) ((rat) 1 2))) ((mtimes) x ((%asinh) x)))) integral)
(defprop %acosh ((x) ((mplus) ((mtimes) -1 ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) 1 2))) ((mtimes) x ((%acosh) x)))) integral)
(defprop %atanh ((x) ((mplus) ((mtimes) x ((%atanh) x)) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))))))) integral)
(defprop %acsch ((x) ((mplus) ((mtimes) ((rat) -1 2) ((%log) ((mplus) -1 ((mexpt) ((mplus) 1 ((mexpt) x -2)) ((rat) 1 2))))) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mexpt) ((mplus) 1 ((mexpt) x -2)) ((rat) 1 2))))) ((mtimes) x ((%acsch) x)))) integral)
(defprop %asech ((x) ((mplus) ((mtimes) -1 ((%atan) ((mexpt) ((mplus) -1 ((mexpt) x -2)) ((rat) 1 2)))) ((mtimes) x ((%asech) x)))) integral)
(defprop %acoth ((x) ((mplus) ((mtimes) x ((%acoth) x)) ((mtimes) ((rat) 1 2) ((%log) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))))))) integral)

;; Define a little helper function to be used in antiderivatives.
;; Depending on the logabs flag, it either returns log(x) or log(abs(x)).
(defun log-or-logabs (x)
  (take '(%log) (if $logabs (take '(mabs) x) x)))

;; Define the antiderivative of tan(x), taking logabs into account.
(defun integrate-tan (x)
  (log-or-logabs (take '(%sec) x)))
(putprop '%tan `((x) ,'integrate-tan) 'integral)

;; ... the same for csc(x) ...
(defun integrate-csc (x)
  (mul -1 (log-or-logabs (add (take '(%csc) x) (take '(%cot) x)))))
(putprop '%csc `((x) ,'integrate-csc) 'integral)

;; ... the same for sec(x) ...
(defun integrate-sec (x)
  (log-or-logabs (add (take '(%sec) x) (take '(%tan) x))))
(putprop '%sec `((x) ,'integrate-sec) 'integral)

;; ... the same for cot(x) ...
(defun integrate-cot (x)
  (log-or-logabs (take '(%sin) x)))
(putprop '%cot `((x) ,'integrate-cot) 'integral)

;; ... the same for coth(x) ...
(defun integrate-coth (x)
  (log-or-logabs (take '(%sinh) x)))
(putprop '%coth `((x) ,'integrate-coth) 'integral)

;; ... the same for csch(x) ...
(defun integrate-csch (x)
  (log-or-logabs (take '(%tanh) (mul '((rat simp) 1 2) x))))
(putprop '%csch `((x) ,'integrate-csch) 'integral)

;; integrate(x^n,x) = if n # -1 then x^(n+1)/(n+1) else log-or-logabs(x).
(defun integrate-mexpt-1 (x n)
  (let ((n-is-minus-one ($askequal n -1)))
    (cond ((eq '$yes n-is-minus-one)
	   (log-or-logabs x))
	  (t
	   (setq n (add n 1))
	   (div (take '(mexpt) x n) n)))))

;; integrate(a^x,x) = a^x/log(a).
(defun integrate-mexpt-2 (a x)
  (div (take '(mexpt) a x) (take '(%log) a)))

(putprop 'mexpt `((x n) ,'integrate-mexpt-1 ,'integrate-mexpt-2) 'integral)

(defun integrate5 (ex var2)
  (if (rat8 ex var2)
      (ratint ex var2)
      (integrator ex var2)))

(defun denomfind (x)
  (cond ((ratnump x) (caddr x))
	((not (numberp x)) nil)
	((not (floatp x)) 1)
	(t (cdr (maxima-rationalize x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stage II
;;; Implementation of Method 1: Elementary function of exponentials
;;;
;;; The following examples are integrated with this method:
;;;
;;;   integrate(exp(x)/(2+3*exp(2*x)),x)
;;;   integrate(exp(x+1)/(1+exp(x)),x)
;;;   integrate(10^x*exp(x),x)

(let ((base nil)       ; The common base.
      (pow nil)       ; The common power of the form b*x+a. The values are
                      ; stored in a list which is returned from m2.
      (exptflag nil)) ; When T, the substitution is not possible.
  
  (defun superexpt (expr var2 bas1 pow1)
    (prog (y ($logabs nil) (new-var (gensym "NEW-VAR-")))
      (putprop new-var t 'internal)
      (setq base bas1
            pow pow1
            exptflag nil)
      ;; Transform the integrand. At this point resimplify, because it is not
      ;; guaranteed, that a correct simplified expression is returned.
      ;; Use a new variable to prevent facts on the old variable to be wrongly used.
      (setq y (resimplify (maxima-substitute new-var var2 (elemxpt expr var2))))
      (when exptflag (return nil))
      ;; Integrate the transformed integrand and substitute back.
      (return
        ($multthru
          (substint (list '(mexpt) base
                          (list '(mplus) (cdras 'a pow)
                                (list '(mtimes) (cdras 'b pow) var2)))
                    new-var
                    (integrator (div y
                                     (mul new-var
                                          (cdras 'b pow)
                                          (take '(%log) base)))
                                new-var)
                    var2
                    expr)))))
  
  ;; Transform expressions like g^(b*x+a) to the common base base and
  ;; do the substitution y = base^(b*x+a) in the expr.
  (defun elemxpt (expr var2 &aux w)
    (cond ((freevar2 expr var2) expr)
          ;; var2 is the base of a subexpression. The transformation fails.
          ((atom expr) (setq exptflag t))
          ((not (eq (caar expr) 'mexpt))
           (cons (car expr)
                 (mapcar #'(lambda (c)
                             (elemxpt c var2))
                         (cdr expr))))
          ((not (freevar2 (cadr expr) var2))
           (list '(mexpt)
                 (elemxpt (cadr expr) var2)
                 (elemxpt (caddr expr) var2)))
          ;; Transform the expression to the common base.
          ((not (eq (cadr expr) base))
           (elemxpt (list '(mexpt)
                          base
                          (mul (power (take '(%log) base) -1)
                               (take '(%log) (cadr expr))
                               (caddr expr)))
                    var2))
          ;; The exponent must be linear in the variable of integration.
          ((not (setq w (m2-b*x+a (caddr expr) var2)))
           (list (car expr) base (elemxpt (caddr expr) var2)))
          ;; Do the substitution y = g^(b*x+a).
          (t
           (setq w (cons (cons 'bb (cdras 'b pow)) w))
           (setq w (cons (cons 'aa (cdras 'a pow)) w))
           (setq w (cons (cons 'base base) w))
           (subliss w '((mtimes)
                        ((mexpt) base a)
                        ((mexpt)
                         base
                         ((mquotient)
                          ((mtimes) -1 aa b) bb))
                        ((mexpt)
                         x
                         ((mquotient) b bb)))))))
) ; End of let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stage II
;;; Implementation of Method 3:
;;; Substitution for a rational root of a linear fraction of x
;;;
;;; This method is applicable when the integrand is of the form:
;;;
;;;   /
;;;   [       a x + b n1/m1   a x + b n1/m2
;;;   I R(x, (-------)   ,   (-------)     , ...) dx
;;;   ]       c x + d         c x + d
;;;   /
;;;
;;; Substitute 
;;;
;;;    (1) t = ((a*x+b)/(c*x+d))^(1/k), or
;;;
;;;    (2) x = (b-d*t^k)/(c*t^k-a)
;;;
;;; where k is the least common multiplier of m1, m2, ... and
;;;
;;;    (3) dx = k*(a*d-b*c)*t^(k-1)/(a-c*t^k)^2 * dt
;;;
;;; First, the algorithm calls the routine RAT3 to collect the roots of the
;;; form ((a*x+b)/(c*x+d))^(n/m) in the list ROOTLIST.
;;; search for the least common multiplier of m1, m2, ... then the
;;; substitutions (2) and (3) are done and the new problem is integrated.
;;; As always, W is an alist which associates to the coefficients
;;; a, b... (and to VAR) their values.

;; ratroot2 is an expression of the form (a*x+b)/(c*x+d)
(defun ratroot (expr var2 ratroot2 w)
  (prog (rootlist k y w1)
     ;; List of powers of the expression ratroot2.
     ;;
     ;; Check if the integrand has a chebyform, if so return the result.
     (when (setq y (chebyf expr var2)) (return y))
     ;; Check if the integrand has a suitably form and collect the roots
     ;; in the global special variable *ROOTLIST*.
     (labels
         ((rat3 (ex ind var2 ratroot2)
            (cond ((freevar2 ex var2) t)
	          ((atom ex) ind)
	          ((member (caar ex) '(mtimes mplus) :test #'eq)
	           (do ((u (cdr ex) (cdr u)))
	               ((null u) t)
	             (if (not (rat3 (car u) ind var2 ratroot2))
	                 (return nil))))
	          ((not (eq (caar ex) 'mexpt))
	           (rat3 (car (margs ex)) t var2 ratroot2))
	          ((freevar2 (cadr ex) var2)
	           (rat3 (caddr ex) t var2 ratroot2))
	          ((integerp (caddr ex))
	           (rat3 (cadr ex) ind var2 ratroot2))
                  ((and (m2 (cadr ex) ratroot2)
	                (denomfind (caddr ex)))
                   (setq rootlist (cons (denomfind (caddr ex)) rootlist)))
                  (t (rat3 (cadr ex) nil var2 ratroot2)))))
       (unless (rat3 expr t var2 ratroot2) (return nil)))
     ;; Get the least common multiplier of m1, m2, ...
     (setq k (apply #'lcm rootlist))
     (setq w1 (cons (cons 'k k) w))
     ;; Substitute for the roots.
     (setq y
           (subst41 expr
                    (subliss w1
                             `((mquotient)
                               ((mplus) ((mtimes) b e)
                                ((mtimes) -1 d ((mexpt) ,var2 k)))
                               ((mplus) ((mtimes) c ((mexpt) ,var2 k))
                                ((mtimes) -1 e a))))
                    var2
                    k
                    ratroot2))
     ;; Integrate the new problem.
     (setq y
           (integrator
            (mul y
                 (subliss w1
                          `((mquotient)
                            ((mtimes) e
                             ((mplus)
                              ((mtimes) a d k
                               ((mexpt) ,var2 ((mplus) -1 k)))
                              ((mtimes) -1
                               ((mtimes) b c k
                                ((mexpt) ,var2 ((mplus) -1 k))))))
                            ((mexpt) ((mplus)
                                      ((mtimes) c ((mexpt) ,var2 k))
                                      ((mtimes) -1 a e))
                             2))))
            var2))
     ;; Substitute back and return the result.
     (return (substint (power ratroot2 (power k -1)) var2 y var2 expr))))

(let ((rootform nil) ; Expression of the form x = (b*e-d*t^k)/(c*t^k-e*a).
      (rootvar nil)) ; The variable we substitute for the root.
  
  (defun subst4 (ex k ratroot2)
    (cond ((freevar2 ex rootvar)
           ;; SUBST4 is called from SUBST41 with ROOTVAR equal to VAR
           ;; (from RATROOT).  Hence we can use FREEVAR2 to see if EX
           ;; is free of ROOTVAR instead of using FREEVAR.
           ex)
          ((atom ex) rootform)
          ((not (eq (caar ex) 'mexpt))
           (mapcar #'(lambda (u) (subst4 u k ratroot2)) ex))
          ((m2 (cadr ex) ratroot2)
           (list (car ex) rootvar (integerp2 (timesk k (caddr ex)))))
          (t (list (car ex) (subst4 (cadr ex) k ratroot2) (subst4 (caddr ex) k ratroot2)))))
  
  (defun subst41 (expr a b k ratroot2)
    ;; Note:  SUBST41 is only called from RATROOT, and the arg B is VAR.
    (setq rootform a
          rootvar b)
    ;; At this point resimplify, because it is not guaranteed, that a correct 
    ;; simplified expression is returned.
    (resimplify (subst4 expr k ratroot2)))
) ; End of let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 4: Binomial Chebyschev

;; exp = a*t^r1*(c1+c2*t^q)^r2, where var2 = t.
;;
;; G&S 2.202 has says this integral can be expressed by elementary
;; functions ii:
;;
;; 1. q is an integer
;; 2. (r1+1)/q is an integer
;; 3. (r1+1)/q+r2 is an integer.
;;
;; I (rtoy) think that for this code to work, r1, r2, and q must be numbers.
(defun chebyf (expr var2)
  (prog (r1 r2 d1 d2 n1 n2 w q)
     ;; Return NIL if the expression doesn't match.
     (when (not (setq w (m2-chebyform expr var2)))
       (return nil))
     #+nil
     (format t "w = ~A~%" w)
     (when (zerop1 (cdr (assoc 'c1 w :test #'eq)))
       ;; rtoy: Is it really possible to be in this routine with c1 =
       ;; 0?
       (return
	 (mul*
	  ;; This factor is locally constant as long as t and
	  ;; c2*t^q avoid log's branch cut.
	  (subliss w `((mtimes) a ((mexpt) ,var2 ((mtimes) -1 q r2))
		       ((mexpt) ((mtimes) c2 ((mexpt) ,var2 q)) r2)))
	  (integrator
	   (subliss w `((mexpt) ,var2 ((mplus) r1 ((mtimes) q r2))))
           var2))))
     (setq q (cdr (assoc 'q w :test #'eq)))
     ;; Reset parameters.  a = a/q, r1 = (1 - q + r1)/q
     (setq w
	   (list* (cons 'a (div* (cdr (assoc 'a w :test #'eq)) q))
		  (cons
		   'r1
		   (div* (addn (list 1 (neg (simplify q)) (cdr (assoc 'r1 w :test #'eq))) nil) q))
		  w))
     #+nil
     (format t "new w = ~A~%" w)
     (setq r1 (cdr (assoc 'r1 w :test #'eq))
	   r2 (cdr (assoc 'r2 w :test #'eq)))
     #+nil
     (progn
       (format t "new r1 = ~A~%" r1)
       (format t "r2     = ~A~%" r2))
     ;; Write r1 = d1/n1, r2 = d2/n2, if possible.  Update w with
     ;; these values, if so.  If we can't, give up.  I (rtoy) think
     ;; this only happens if r1 or r2 can't be expressed as rational
     ;; numbers.  Hence, r1 and r2 have to be numbers, not variables.
     (cond
       ((not (and (setq d1 (denomfind r1))
		  (setq d2 (denomfind r2))
		  (setq n1 (integerp2 (timesk r1 d1)))
		  (setq n2 (integerp2 (timesk r2 d2)))
		  (setq w (list* (cons 'd1 d1) (cons 'd2 d2)
				 (cons 'n1 n1) (cons 'n2 n2)
				 w))))
	#+nil
	(progn
	  (format t "cheby can't find one of d1,d2,n1,n2:~%")
	  (format t "  d1 = ~A~%" d1)
	  (format t "  d2 = ~A~%" d2)
	  (format t "  n1 = ~A~%" n1)
	  (format t "  n2 = ~A~%" n2))
	(return nil))
       ((and (integerp2 r1) (> r1 0))
	#+nil (format t "integer r1 > 0~%")
	;; (r1+q-1)/q is positive integer.
	;;
	;; I (rtoy) think we are using the substitution z=(c1+c2*t^q).
	;; Maxima thinks the resulting integral should then be
	;;
	;; a/q*c2^(-r1/q-1/q)*integrate(z^r2*(z-c1)^(r1/q+1/q-1),z)
	;;
	(return
	  (substint
	   (subliss w `((mplus) c1 ((mtimes) c2 ((mexpt) ,var2 q))))
	   var2
	   (integrator
	    (expands (list (subliss w
				    ;; a*t^r2*c2^(-r1-1)
				    `((mtimes)
				      a
				      ((mexpt) ,var2 r2)
				      ((mexpt)
				       c2
				       ((mtimes)
					-1
					((mplus) r1 1))))))
		     (cdr
		      ;; (t-c1)^r1
		      (expandexpt (subliss w
					   `((mplus)
					     ,var2
					     ((mtimes) -1 c1)))
				  r1)))
	    var2)
           var2
           expr)))
       ((integerp2 r2)
	#+nil (format t "integer r2~%")
	;; I (rtoy) think this is using the substitution z = t^(q/d1).
	;;
	;; The integral (as maxima will tell us) becomes
	;;
	;; a*d1/q*integrate(z^(n1/q+d1/q-1)*(c1+c2*z^d1)^r2,z)
	;;
	;; But be careful because the variable A in the code is
	;; actually a/q.
	(return
	  (substint (subliss w `((mexpt) ,var2 ((mquotient) q d1)))
		    var2
		    (ratint (simplify (subliss w
					       `((mtimes)
						 d1 a
						 ((mexpt)
						  ,var2
						  ((mplus)
						   n1 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mtimes)
						    c2
						    ((mexpt)
						     ,var2 d1))
						   c1)
						  r2))))
			    var2)
                    var2
                    expr)))
       ((and (integerp2 r1) (< r1 0))
	#+nil (format t "integer r1 < 0~%")
	;; I (rtoy) think this is using the substitution
	;;
	;; z = (c1+c2*t^q)^(1/d2)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;;  a/q*c2^(-r1/q-1/q)*d2*
	;;    integrate(z^(n2+d2-1)*(z^d2-c1)^(r1/q+1/q-1),z)
	(return
	  (substint (subliss w
			     ;; (c1+c2*t^q)^(1/d2)
			     `((mexpt)
			       ((mplus)
				c1
				((mtimes) c2 ((mexpt) ,var2 q)))
			       ((mquotient) 1 d2)))
		    var2
		    (ratint (simplify (subliss w
					       ;; This is essentially
					       ;; the integrand above,
					       ;; except A and R1 here
					       ;; are not the same as
					       ;; derived above.
					       `((mtimes)
						 a d2
						 ((mexpt)
						  c2
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 1)))
						 ((mexpt)
						  ,var2
						  ((mplus)
						   n2 d2 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    ,var2 d2)
						   ((mtimes) -1 c1))
						  r1))))
			    var2)
                    var2
                    expr)))
       ((integerp2 (add* r1 r2))
	#+nil (format t "integer r1+r2~%")
	;; If we're here,  (r1-q+1)/q+r2 is an integer.
	;;
	;; I (rtoy) think this is using the substitution
	;;
	;; z = ((c1+c2*t^q)/t^q)^(1/d1)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;; a*d2/q*c1^(r2+r1/q+1/q)*
	;;   integrate(z^(d2*r2+d2-1)*(z^d2-c2)^(-r2-r1/q-1/q-1),z)
	(return
	  (substint (let (($radexpand '$all))
		      ;; Setting $radexpand to $all here gets rid of
		      ;; ABS in the substitution.  I think that's ok in
		      ;; this case.  See Bug 1654183.
		      (subliss w
			       `((mexpt)
				 ((mquotient)
				  ((mplus)
				   c1
				   ((mtimes) c2 ((mexpt) ,var2 q)))
				  ((mexpt) ,var2 q))
				 ((mquotient) 1 d1))))
		    var2
		    (ratint (simplify (subliss w
					       `((mtimes)
						 -1 a d1
						 ((mexpt)
						  c1
						  ((mplus)
						   r1 r2 1))
						 ((mexpt)
						  ,var2
						  ((mplus)
						   n2 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    ,var2 d1)
						   ((mtimes)
						    -1 c2))
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 r2
						    2))))))
			    var2)
                    var2
                    expr)))
       (t (return (list '(%integrate) expr var2))))))

(defun greaterratp (x1 x2)
  (cond ((and (numberp x1) (numberp x2))
	 (> x1 x2))
	((ratnump x1)
	 (greaterratp (quotient (float (cadr x1))
				(caddr x1))
		      x2))
	((ratnump x2)
	 (greaterratp x1
		      (quotient (float (cadr x2))
				(caddr x2))))))

(defun trig1 (x)
  (member (car x) '(%sin %cos) :test #'eq))

(defun supertrig (expr var2 trigarg)
  (declare (special *notsame*))
  (cond ((freevar2 expr var2) t)
	((atom expr) nil)
	((member (caar expr) '(mplus mtimes) :test #'eq)
	 (and (supertrig (cadr expr) var2 trigarg)
	      (or (null (cddr expr))
		  (supertrig (cons (car expr)
				   (cddr expr))
                             var2 trigarg))))
	((eq (caar expr) 'mexpt)
	 (and (supertrig (cadr expr) var2 trigarg)
	      (supertrig (caddr expr) var2 trigarg)))
	((eq (caar expr) '%log)
	 (supertrig (cadr expr) var2 trigarg))
	((member (caar expr)
	       '(%sin %cos %tan %sec %cot %csc) :test #'eq)
	 (cond ((m2 (cadr expr) trigarg) t)
               ((m2-b*x+a (cadr expr) var2)
                (and (setq *notsame* t) nil))
	       (t (supertrig (cadr expr) var2 trigarg))))
	(t (supertrig (cadr expr) var2 trigarg))))

(defun subst2s (ex pat var2)
  (cond ((null ex) nil)
	((m2 ex pat) var2)
	((atom ex) ex)
	(t (cons (subst2s (car ex) pat var2)
		 (subst2s (cdr ex) pat var2)))))

;; Match (c*x+b), where c and b are free of x
(defun simple-trig-arg (expr var2)
  (m2 expr `((mplus) ((mtimes)
		       ((coefftt) (c freevar2 ,var2))
		       ((coefftt) (v varp2 ,var2)))
	      ((coeffpp) (b freevar2 ,var2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 6: Elementary function of trigonometric functions

(defun monstertrig (expr var2 trigarg)
  (prog (*notsame* w a b y d)
     (declare (special *notsame*))
     (cond
       ((supertrig expr var2 trigarg)
        (go a))
       ((null *notsame*) (return nil))
       ;; Check for an expression like a*trig1(m*x)*trig2(n*x),
       ;; where trig1 and trig2 are sin or cos.
       ((not (setq y (m2 expr
                         `((mtimes)
                           ((coefftt) (a freevar2 ,var2))
                           (((b trig1))
                            ((mtimes)
                             (x varp2 ,var2)
                             ((coefftt) (m freevar2 ,var2))))
                           (((d trig1))
                            ((mtimes)
                             (x varp2 ,var2)
                             ((coefftt) (n freevar2 ,var2))))))))
        (go b))
; This check has been done with the pattern match.
;       ((not (and (member (car (setq b (cdr (assoc 'b y :test #'eq)))) '(%sin %cos) :test #'eq)
;                  (member (car (setq d (cdr (assoc 'd y :test #'eq)))) '(%sin %cos) :test #'eq)))
;        (return nil))
       ((progn
	  ;; The tests after this depend on values of b and d being
	  ;; set.  Set them here unconditionally, before doing the
	  ;; tests.
	  (setq b (cdras 'b y))
	  (setq d (cdras 'd y))
	  (and (eq (car b) '%sin)
	       (eq (car d) '%sin)))
        ;; We have a*sin(m*x)*sin(n*x).
        ;;
        ;; The integral is:
        ;; a*(sin((m-n)*x)/(2*(m-n))-sin((m+n)*x)/(2*(m+n)).  But if n
        ;; = m, the integral is x/2-sin(2*n*x)/(4*n).
        (let ((n (cdras 'n y))
              (m (cdras 'm y)))
          (cond
            ((eq ($askequal n m) '$yes)
             ;; n=m, so we have the integral of a*sin(n*x)^2 which is
             (return (subliss y
                              `((mtimes) a
                                ((mplus)
                                 ((mquotient) x 2)
                                 ((mtimes) -1
                                  ((mquotient)
                                   ((%sin) ((mtimes) 2 n x))
                                   ((mtimes) 4 n))))))))
            (t
             (return (subliss y
                              '((mtimes) a
                                ((mplus)
                                 ((mquotient)
                                  ((%sin) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                                  ((mtimes) 2 ((mplus) m ((mtimes) -1 n))))
                                 ((mtimes) -1
                                  ((mquotient)
                                   ((%sin) ((mtimes) ((mplus) m n) x))
                                   ((mtimes) 2 ((mplus) m n))))))))))))
       ((and (eq (car b) '%cos) (eq (car d) '%cos))
        ;; We have a*cos(m*x)*cos(n*x).
        ;; 
        ;; The integral is:
        ;; a*(sin((m-n)*x)/(2*(m-n))+sin((m+n)*x)/(2*(m+n)).  But when
        ;; n = m, the integral is sin(2*m*x)/(4*m)+x/2.
        (let ((n (cdras 'n y))
              (m (cdras 'm y)))
          (cond
            ((eq ($askequal n m) '$yes)
             (return (subliss y
                              '((mtimes) a
                                ((mplus)
                                 ((mquotient)
                                  ((%sin) ((mtimes) 2 n x))
                                  ((mtimes) 4 n))
                                 ((mquotient)
                                  x 2))))))
            (t
             (return (subliss y
                              '((mtimes) a
                                ((mplus)
                                 ((mquotient)
                                  ((%sin) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                                  ((mtimes) 2
                                   ((mplus) m ((mtimes) -1 n))))
                                 ((mquotient)
                                  ((%sin) ((mtimes) ((mplus) m n) x))
                                  ((mtimes) 2 ((mplus) m n)))))))))))
       ((or (and (eq (car b) '%cos)
		 ;; The following (destructively!) swaps the values of
		 ;; m and n if first trig term is sin.  I (rtoy) don't
		 ;; understand why this is needed.  The formula
		 ;; doesn't depend on that.
                 (setq w (cdras 'm y ))
                 (rplacd (assoc 'm y) (cdras 'n y))
                 (rplacd (assoc 'n y) w))
            t)
        ;; We have a*cos(n*x)*sin(m*x).
        ;;
        ;; The integral is:
        ;; -a*(cos((m-n)*x)/(2*(m-n))+cos((m+n)*x)/(2*(m+n)).  But
        ;; if n = m, the integral is -cos(n*x)^2/(2*n).
        (let ((n (cdras 'n y))
              (m (cdras 'm y)))
          (cond
            ((eq ($askequal n m) '$yes)
             ;; This needs work.  For example
             ;; integrate(cos(m*x)*sin(2*m*x),x).  We ask if 2*m = m.
             ;; If the answer is yes, we return -cos(m*x)^2/(2*m).
             ;; But if 2*m=m, then m=0 and the integral must be 0.
             (return (subliss y
                              '((mquotient)
                                ((mtimes) -1 a
                                 ((mexpt)
                                  ((%cos) ((mtimes) n x))
                                  2))
                                ((mtimes) 2 n)))))
            (t
             (return (subliss y
                              '((mtimes) -1 a
                                ((mplus)
                                 ((mquotient)
                                  ((%cos) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                                  ((mtimes) 2 ((mplus) m ((mtimes) -1 n))))
                                 ((mquotient)
                                  ((%cos) ((mtimes) ((mplus) m n) x))
                                  ((mtimes) 2 ((mplus) m n))))))))))))
  b  ;; At this point we have trig functions with different arguments,
     ;; but not a product of sin and cos.
     (cond ((not (setq y (prog2 
                           (setq trigarg var2)
                           (m2 expr
                               `((mtimes)
                                 ((coefftt) (a freevar2 ,var2))
                                 (((b trig1))
                                  ((mtimes) 
                                   (x varp2 ,var2)
                                   ((coefftt) (n integerp2))))
                                 ((coefftt) (c supertrig ,var2 ,trigarg)))))))
            (return nil)))
     ;; We have a product of trig functions: trig1(n*x)*trig2(y).
     ;; trig1 is sin or cos, where n is a numerical integer. trig2 is not a sin
     ;; or cos. The cos or sin function is expanded.
     (return
       (integrator
         ($expand
           (list '(mtimes)
                 (cdras 'a y)                             ; constant factor
                 (cdras 'c y)                             ; trig functions
                 (cond ((eq (car (cdras 'b y)) '%cos)     ; expand cos(n*x)
                        (maxima-substitute var2
                                           'x
                                           (supercosnx (cdras 'n y))))
                       (t                                 ; expand sin(x*x)
                        (maxima-substitute var2
                                           'x
                                           (supersinx (cdras 'n y)))))))
         var2))
  a  ;; A product of trig functions and all trig functions have the same
     ;; argument trigarg. Maxima substitutes trigarg with the variable var2
     ;; of integration and calls trigint to integrate the new problem.
     (setq w (subst2s expr trigarg var2))
     (setq b (cdras 'b (m2-b*x+a trigarg var2)))
     (setq a (substint trigarg var2 (trigint (div* w b) var2) var2 expr))
     (return (if (isinop a '%integrate)
                 (list '(%integrate) expr var2)
                 a))))

(defun trig2 (x)
  (member (car x) '(%sin %cos %tan %cot %sec %csc) :test #'eq))

;; sin(n*x) for integer n /= 0.  Result not simplified.
(defun supersinx (n)
  (let ((i (if (< n 0) -1 1)))
    ($expand (list '(mtimes) i (sinnx (timesk i n))))))

;; cos(n*x) for integer n /= 0.  Result not simplified.
(defun supercosnx (n)
  ($expand (cosnx (timesk (if (< n 0) -1 1) n))))

;; sin(n*x) for integer n >= 1.  Result is not simplified.
(defun sinnx (n)
  (if (equal n 1)
      '((%sin) x)
      (list '(mplus)
	    (list '(mtimes) '((%sin) x) (cosnx (1- n)))
	    (list '(mtimes) '((%cos) x) (sinnx (1- n))))))

;; cos(n*x) for integer n >= 1.  Result is not simplified.
(defun cosnx (n)
  (if (equal n 1)
      '((%cos) x)
      (list '(mplus)
	    (list '(mtimes) '((%cos) x) (cosnx (1- n)))
	    (list '(mtimes) -1 '((%sin) x) (sinnx (1- n))))))

(defun poseven (x)
  (and (even x) (> x -1)))

(defun trigfree (x)
  (if (atom x)
      (not (member x '(sin* cos* sec* tan*) :test #'eq))
      (and (trigfree (car x)) (trigfree (cdr x)))))

(defun rat1 (expr aa bb cc)
  (prog (b1 *notsame*)
     (declare (special *yy* *notsame*))
     (when (and (numberp expr) (zerop expr))
       (return nil))
     (setq b1 (subst bb 'b '((mexpt) b (n even))))
     (return (prog2
		 (setq *yy* (rats expr aa b1 cc))
		 (cond ((not *notsame*) *yy*))))))

(defun rats (expr aa b1 cc)
  (prog (y)
     (declare (special *notsame*))
     (return
       (cond ((eq expr aa) 'x)
	     ((atom expr)
	      (cond ((member expr '(sin* cos* sec* tan*) :test #'eq)
		     (setq *notsame* t))
		    (t expr)))
	     ((setq y (m2 expr b1))
	      (f3 y cc))
	     (t (cons (car expr) (mapcar #'(lambda (g) (rats g aa b1 cc))
                                         (cdr expr))))))))

(defun f3 (y cc)
  (maxima-substitute cc
		     'c
		     (maxima-substitute (quotient (cdr (assoc 'n y :test #'eq)) 2)
					'n
					'((mexpt)
					  ((mplus)
					   1
					   ((mtimes)
					    c
					    ((mexpt) x 2)))
					  n))))

(defun odd1 (n cc)
  (declare (special *yz*))
  (cond ((not (numberp n)) nil)
	((not (equal (rem n 2) 0))
	 (setq *yz*
	       (maxima-substitute cc
				  'c
				  (list '(mexpt)
					'((mplus) 1 ((mtimes) c ((mexpt) x 2)))
					(quotient (1- n) 2)))))
	(t nil)))

(defun subvar (x var2)
  (maxima-substitute var2 'x x))

(defun subvardlg (x var2)
  (mapcar #'(lambda (m)
	      (cons (maxima-substitute var2 'x (car m)) (cdr m)))
	  x))

;; This appears to be the implementation of Method 6, pp.82 in Moses' thesis.

(defun trigint (expr var2)
  (prog (y repl y1 y2 *yy* z m n *yz*)
     (declare (special *yy* *yz*))
     ;; Transform trig(x) into trig* (for simplicity?)  Convert cot to
     ;; tan and csc to sin.
     (setq y2
	   (subliss (subvardlg '((((%sin) x) . sin*)
				 (((%cos) x) . cos*)
				 (((%tan) x) . tan*)
				 (((%cot) x) . ((mexpt) tan* -1))
				 (((%sec) x) . sec*)
				 (((%csc) x) . ((mexpt) sin* -1)))
                               var2)
		    expr))
     
     (when *debug-integrate*
       (format t "~& in TRIGINT:~%")
       (format t "~&   : y2 = ~A~%" y2))
     
     ;; Now transform tan to sin/cos and sec to 1/cos.
     (setq y1 (setq y (subliss '((tan* . ((mtimes) sin*
                                          ((mexpt) cos* -1)))
                                 (sec* . ((mexpt) cos* -1)))
                               y2)))
     
     (when *debug-integrate* (format t "~&   : y  = ~A~%" y))
     
     (when (null (setq z
                       (m2 y
                           '((mtimes)
                             ((coefftt) (b trigfree))
                             ((mexpt) sin* (m poseven))
                             ((mexpt) cos* (n poseven))))))
       ;; Go if y is not of the form sin^m*cos^n for positive even m and n.
       (go l1))
     
     ;; Case III:
     ;; Handle the case of sin^m*cos^n, m, n both non-negative and even.
     
     (setq m (cdras 'm z))
     (setq n (cdras 'n z))
     (let ((aa (integerp2 (* 0.5 (if (< m n) 1 -1) (+ n (* -1 m))))))
       (setq z (cons (cons 'a aa) z))
       (setq z (cons (cons 'x var2) z))
     
       (when *debug-integrate*
         (format t "~& CASE III:~%")
         (format t "~&   : m, n = ~A ~A~%" m n)
         (format t "~&   : a    = ~A~%" aa)
         (format t "~&   : z    = ~A~%" z)))
     
     ;; integrate(sin(y)^m*cos(y)^n,y) is transformed to the following form:
     ;;
     ;; m < n:  integrate((sin(2*y)/2)^n*(1/2+1/2*cos(2*y)^((n-m)/2),y)
     ;; m >= n: integrate((sin(2*y)/2)^n*(1/2-1/2*cos(2*y)^((m-n)/2),y)
     (return
       (mul (cdras 'b z)
            (div 1 2)
            (substint 
              (mul 2 var2)
              var2
              (integrator 
                (cond ((< m n)
                       (subliss z
                                '((mtimes)
                                  ((mexpt)
                                   ((mtimes) ((rat simp) 1 2) ((%sin) x))
                                   m)
                                  ((mexpt)
                                   ((mplus)
                                    ((rat simp) 1 2)
                                    ((mtimes)
                                     ((rat simp) 1 2) ((%cos) x))) a))))
                      (t
                       (subliss z
                                '((mtimes)
                                  ((mexpt)
                                   ((mtimes) ((rat simp) 1 2) ((%sin) x))
                                   n)
                                  ((mexpt)
                                   ((mplus)
                                    ((rat simp) 1 2)
                                    ((mtimes)
                                     ((rat simp) -1 2) 
                                     ((%cos) x))) a)))))
                var2)
              var2
              expr)))
  l1 
     ;; Case IV:
     ;; I think this is case IV, working on the expression in terms of
     ;; sin and cos.
     ;;
     ;; Elem(x) means constants, x, trig functions of x, log and
     ;; inverse trig functions of x, and which are closed under
     ;; addition, multiplication, exponentiation, and substitution.
     ;;
     ;; Elem(f(x)) is the same as Elem(x), but f(x) replaces x in the
     ;; definition.
     
     (when *debug-integrate* (format t "~& Case IV:~%"))
     
     (when (and (m2 y '((coeffpt) (c rat1 sin* cos* -1) ((mexpt) cos* (n odd1 -1))))
                (setq repl (list '(%sin) var2)))
       ;; The case cos^(2*n+1)*Elem(cos^2,sin).  Use the substitution z = sin.
       (go getout))

     (when (and (m2 y '((coeffpt) (c rat1 cos* sin* -1) ((mexpt) sin* (n odd1 -1))))
                (setq repl (list '(%cos) var2)))
       ;; The case sin^(2*n+1)*Elem(sin^2,cos).  Use the substitution z = cos.
       (go get3))
     
     ;; Case V:
     ;; Transform sin and cos to tan and sec to see if the integral is
     ;; of the form Elem(tan, sec^2).  If so, use the substitution z = tan.
     
     (when *debug-integrate* (format t "~& Case V:~%"))
     
     (setq y (subliss '((sin* (mtimes) tan* ((mexpt) sec* -1))
                        (cos* (mexpt) sec* -1))
                      y2))
     (when (and (rat1 y 'tan* 'sec* 1) (setq repl (list '(%tan) var2)))
       (go get1))

     (when (and (m2 y '((coeffpt) (c rat1 sec* tan* 1) ((mexpt) tan* (n odd1 1))))
           (setq repl (list '(%sec) var2)))
       (go getout))
     (when (not (alike1 (setq repl ($expand expr)) expr))
       (return (integrator repl var2)))
     (setq y (subliss '((sin* (mtimes) 2 x
                              ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))
                        (cos* (mtimes)
                              ((mplus) 1 ((mtimes) -1 ((mexpt) x 2)))
                              ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1)))
                      y1))
     (setq y (list '(mtimes) 
                   y 
                   '((mtimes) 2 ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))))
     (setq repl (subvar '((mquotient) ((%sin) x) ((mplus) 1 ((%cos) x)))
                        var2))
     (go get2)
  get3
     (setq y (list '(mtimes) -1 *yy* *yz*))
     (go get2)
  get1
     (setq y (list '(mtimes) '((mexpt) ((mplus) 1 ((mexpt) x 2)) -1) *yy*))
     (go get2)
  getout
     (setq y (list '(mtimes) *yy* *yz*))
  get2
     (when *debug-integrate*
       (format t "~& Call the INTEGRATOR with:~%")
       (format t "~&   : y    = ~A~%" y)
       (format t "~&   : repl = ~A~%" repl))
     ;; See Bug 2880797.  We want atan(tan(x)) to simplify to x, so
     ;; set $triginverses to '$all.
     (return
       ;; Do not integrate for the global variable VAR, but substitute it.
       ;; This way possible assumptions on VAR are no longer present. The
       ;; algorithm of DEFINT depends on this behavior. See Bug 3085498.
       (let (($triginverses '$all) (newvar (gensym)))
         (substint repl
                   newvar
                   (integrator (maxima-substitute newvar 'x y) newvar)
                   var2
                   expr)))))

(defmvar $integration_constant_counter 0)
(defmvar $integration_constant '$%c)

;; This is the top level of the integrator
(defun sinint (expr var2)
  ;; *integrator-level* is a recursion counter for INTEGRATOR.  See
  ;; INTEGRATOR for more details.  Initialize it here.
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))

    ;; Sanity checks for variables
    (when (mnump var2)
      (merror (intl:gettext "integrate: variable must not be a number; found: ~:M") var2))
    (when ($ratp var2) (setf var2 (ratdisrep var2)))
    (when ($ratp expr) (setf expr (ratdisrep expr)))

    (cond
      ;; Distribute over lists and matrices
      ((mxorlistp expr)
       (cons (car expr)
             (mapcar #'(lambda (y) (sinint y var2)) (cdr expr))))

      ;; The symbolic integration code doesn't really deal very well with
      ;; subscripted variables, so if we have one then replace occurrences of var2
      ;; with an atomic gensym and recurse.
      ((and (not (atom var2))
            (member 'array (cdar var2)))
       (let ((dummy-var2 (gensym)))
         (maxima-substitute var2 dummy-var2
                            (sinint (maxima-substitute dummy-var2 var2 expr) dummy-var2))))

      ;; If expr is an equality, integrate both sides and add an integration
      ;; constant
      ((mequalp expr)
       (list (car expr) (sinint (cadr expr) var2)
             (add (sinint (caddr expr) var2)
                  ($concat $integration_constant (incf $integration_constant_counter)))))

      ;; If var2 is an atom which occurs as an operator in expr, then return a noun form.
      ((and (atom var2)
            (isinop expr var2))
       (list '(%integrate) expr var2))

      ((zerop1 expr)	;; special case because 0 will not pass sum-of-intsp test
       0)
      
      ((let ((ans (simplify
                     (let ($opsubst varlist genvar)
		       (integrator expr var2 nil)))))
	     (if (sum-of-intsp ans var2)
		 (list '(%integrate) expr var2)
		 ans))))))

;; SUM-OF-INTSP
;;
;; This is a heuristic that SININT uses to work out whether the result from
;; INTEGRATOR is worth returning or whether just to return a noun form. If this
;; function returns T, then SININT will return a noun form.
;;
;; The logic, as I understand it (Rupert 01/2014):
;;
;;   (1) If I integrate f(x) wrt x and get an atom other than x or 0, either
;;       something's gone horribly wrong, or this is part of a larger
;;       expression. In the latter case, we can return T here because hopefully
;;       something else interesting will make the top-level return NIL.
;;
;;   (2) If I get a sum, return a noun form if every one of the summands is no
;;       better than what I started with. (Presumably this is where the name
;;       came from)
;;
;;   (3) If this is a noun form, it doesn't convey much information on its own,
;;       so return T.
;;
;;   (4) If this is a product, something interesting has probably happened. But
;;       I still want a noun form if the result is like 2*'integrate(f(x),x), so
;;       I'm only interested in terms in the product that are not free of
;;       VAR. If one of those terms is worthy of report, that's great: return
;;       NIL. Otherwise, return T if we saw at least two things (eg splitting an
;;       integral into a product of two integrals)
;;
;;   (5) If the result is free of VAR, we're in a similar position to (1).
;;
;;   (6) Otherwise something interesting (and hopefully useful) has
;;       happened. Return NIL to tell SININT to report it.
(defun sum-of-intsp (ans var2)
  (cond ((atom ans)
	 ;; Result of integration should never be a constant other than zero.
	 ;; If the result of integration is zero, it is either because:
	 ;; 1) a subroutine inside integration failed and returned nil,
	 ;;    and (mul 0 nil) yielded 0, meaning that the result is wrong, or
	 ;; 2) the original integrand was actually zero - this is handled
	 ;;    with a separate special case in sinint
	 (not (eq ans var2)))
	((mplusp ans) (every #'(lambda (e)
                                 (sum-of-intsp e var2))
                             (cdr ans)))
	((eq (caar ans) '%integrate) t)
	((mtimesp ans)
         (let ((int-factors 0))
           (not (or (dolist (factor (cdr ans))
                      (unless (freeof var2 factor)
                        (if (sum-of-intsp factor var2)
                            (incf int-factors)
                            (return t))))
                    (<= 2 int-factors)))))
	((freeof var2 ans) t)
	(t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 2: Integrate a summation

(defun intsum (form var2)
  (prog (expr idx ll ul pair val)
     (setq expr (cadr form)
	   idx (caddr form)
	   ll (cadddr form)
	   ul (car (cddddr form)))
     (if (or (not (atom var2))
	     (not (free idx var2))
	     (not (free ll var2))
	     (not (free ul var2)))
	 (return (list '(%integrate) form var2)))
     (setq pair (partition expr var2 1))
     (when (and (mexptp (cdr pair))
		(eq (caddr pair) var2))
       (setq val (maxima-substitute ll idx (cadddr pair)))
       (cond ((equal val -1)
	      (return (add (integrator (maxima-substitute ll idx expr) var2)
			    (intsum1 expr idx (add 1 ll) ul var2))))
	     ((mlsp val -1)
	      (return (list '(%integrate) form var2)))))
     (return (intsum1 expr idx ll ul var2))))

(defun intsum1 (expr idx ll ul var2)
  (assume (list '(mgeqp) idx ll))
  (if (not (eq ul '$inf))
      (assume (list '(mgeqp) ul idx)))
  (simplifya (list '(%sum) (integrator expr var2) idx ll ul) t))

(defun finds (x)
  (if (atom x)
      (member x '(%log %integrate %atan) :test #'eq)
      (or (finds (car x)) (finds (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 9:
;;; Rational function times a log or arctric function

;;; ratlog is called for an expression containing a log or arctrig function
;;; The integrand is like log(x)*f'(x). To obtain the result the technique of
;;; partial integration is applied: log(x)*f(x)-integrate(1/x*f(x),x)

;;; Only called by intform.
(defun ratlog (var2 form)
  (prog (b c d y z)
     (setq y form)
     (setq b (cdr (assoc 'b y :test #'eq)))
     (setq c (cdr (assoc 'c y :test #'eq)))
     (setq y (integrator c var2))
     (when (finds y) (return nil))
     (setq d (sdiff (cdr (assoc 'a form :test #'eq)) var2))
     
     (setq z (integrator (mul2* y d) var2))
     (setq d (cdr (assoc 'a form :test #'eq)))
     (return (simplify (list '(mplus)
			     (list '(mtimes) y d)
			     (list '(mtimes) -1 z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; partial-integration is an extension of the algorithm of ratlog to support
;;; the technique of partial integration for more cases. The integrand
;;; is like g(x)*f'(x) and the result is g(x)*f(x)-integrate(g'(x)*f(x),x).
;;;
;;; Adding integrals properties for elementary functions led to infinite recursion 
;;; with integrate(z*expintegral_shi(z),z). This was resolved by limiting the 
;;; recursion depth. *integrator-level* needs to be at least 3 to solve 
;;;  o  integrate(expintegral_ei(1/sqrt(x)),x)
;;;  o  integrate(sqrt(z)*expintegral_li(z),z)
;;; while a value of 4 causes testsuite regressions with 
;;;  o  integrate(z*expintegral_shi(z),z)
(defun partial-integration (form var2)
  (declare (special *integrator-level*))
  (let ((g  (cdr (assoc 'a form)))   ; part g(x)
	(df (cdr (assoc 'c form)))   ; part f'(x)
	(f  nil))
    (setq f (integrator df var2))     ; integrate f'(x) wrt var2
    (cond
      ((or (isinop f '%integrate)    ; no result or
	   (isinop f (caar g))       ; g in result
	   (> *integrator-level* 3))
       nil)                          ; we return nil
      (t
       ;; Build the result: g(x)*f(x)-integrate(g'(x)*f(x))
       (add (mul f g)
	    (mul -1 (integrator (mul f (sdiff g var2)) var2)))))))

;; returns t if argument of every trig operation in y matches arg
(defun every-trigarg-alike (y arg)
  (cond ((atom y) t)
	((optrig (caar y)) (alike1 arg (cadr y)))
	(t (every (lambda (expr)
		    (every-trigarg-alike expr arg))
		  (cdr y)))))

;; return argument of first trig operation encountered in y
(defun find-first-trigarg (y)
  (cond ((atom y) nil)
	((optrig (caar y)) (cadr y))
	(t (some (lambda (expr)
		   (find-first-trigarg expr))
		 (cdr y)))))

;; return constant factor that makes elements of alist match elements of blist
;; or nil if no match found
;; (we could replace this using rat package to divide alist and blist)
(defun matchsum (alist blist var2)
  (prog (r s cc dd)
     (setq s (m2 (car alist)	;; find coeff for first term of alist
		 `((mtimes)
		   ((coefftt) (a freevar2 ,var2))
		   ((coefftt) (c true)))))
     (setq cc (cdr (assoc 'c s :test #'eq)))
     (cond ((not (setq r	;; find coeff for first term of blist
		       (m2 (car blist)
                           (cons '(mtimes)
                                 (cons `((coefftt) (b free12 ,var2))
                                       (cond ((mtimesp cc)
                                              (cdr cc))
                                             (t (list cc))))))))
	    (return nil)))
     (setq dd (simplify (list '(mtimes)
			     (subliss s 'a)
			     (list '(mexpt)
				   (subliss r 'b)
				   -1))))
     (cond ((m2 (cons '(mplus) alist)	;; check that all terms match
		(timesloop dd blist))
	    (return dd))
	   (t (return nil)))))

(defun timesloop (a b)
  (cons '(mplus) (mapcar #'(lambda (c) (mul2* a c)) b)))

(defun expands (arg1 arg2)
  (addn (mapcar #'(lambda (c) (timesloop c arg1)) arg2) nil))

;; possibly a bug: For var2 = x and dd =3, we have expand(?subst10(x^9 * (x+x^6))) --> x^5+x^4, but
;; ?subst10(expand(x^9 * (x+x^6))) --> x^5+x^3. (Barton Willis)

(defun subst10 (ex var2 dd)
  (cond ((atom ex) ex)
	((and (eq (caar ex) 'mexpt) (eq (cadr ex) var2))
	 (list '(mexpt) var2 (integerp2 (quotient (caddr ex) dd))))
	(t (cons (remove 'simp (car ex))
		 (mapcar #'(lambda (c)
                             (subst10 c var2 dd))
                         (cdr ex))))))

(defun powerlist (expr var2)
  (prog (y cc dd power-list bb)
     (setq y (m2 expr
		 `((mtimes)
		   ((mexpt) (var varp2 ,var2) (c integerp2))
		   ((coefftt) (a freevar2 ,var2))
		   ((coefftt) (b true)))))
     (setq bb (cdr (assoc 'b y :test #'eq)))
     (setq cc (cdr (assoc 'c y :test #'eq)))
     (labels
         ((rat10 (ex)
            (cond ((freevar2 ex var2)
                   t)
	          ((varp2 ex var2)
                   nil)
	          ((eq (caar ex) 'mexpt)
	           (if (varp2 (cadr ex) var2)
	               (if (integerp2 (caddr ex))
		           (setq power-list (cons (caddr ex) power-list)))
	               (and (rat10 (cadr ex))
                            (rat10 (caddr ex)))))
	          ((member (caar ex) '(mplus mtimes) :test #'eq)
	           (do ((u (cdr ex) (cdr u)))
                       ((null u) t)
	             (if (not (rat10 (car u)))
                         (return nil)))))))
       (unless  (rat10 bb) (return nil))
       (setq dd (apply #'gcd (cons (1+ cc) power-list))))
     (when (or (eql 1 dd) (zerop dd)) (return nil))
     (return
       (substint
	(list '(mexpt) var2 dd)
	var2
	(integrate5 (simplify (list '(mtimes)
				    (power* dd -1)
				    (cdr (assoc 'a y :test #'eq))
				    (list '(mexpt) var2 (1- (quotient (1+ cc) dd)))
				    (subst10 bb var2 dd)))
		    var2)
        var2
        expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 3: Derivative-divides algorithm

;; This is the derivative-divides algorithm of Moses.
;;
;;                /
;;                [
;; Look for form  I  c * op(u(x)) * u'(x) dx
;;                ]
;;                /
;;
;;  where:  c     is a constant
;;          u(x)  is an elementary expression in x
;;          u'(x) is its derivative
;;          op    is an elementary operator:
;;                - the identity, or
;;                - any function that can be integrated by INTEGRALLOOKUPS
;;
;; The method of solution, once the problem has been determined to
;; posses the form above, is to look up OP in a table and substitute
;; u(x) for each occurrence of x in the expression given in the table.
;; In other words, the method performs an implicit substitution y = u(x),
;; and obtains the integral of op(y)dy by a table look up.
;;
(defun diffdiv (expr var2)
  (prog (y x v dd z w r)
     (cond ((and (mexptp expr)
		 (mplusp (cadr expr))
		 (integerp (caddr expr))
		 (< (caddr expr) 6)
		 (> (caddr expr) 0))
	    (return (integrator (expandexpt (cadr expr) (caddr expr)) var2))))

     ;; If not a product, transform to a product with one term
     (setq expr (cond ((mtimesp expr) expr) (t (list '(mtimes) expr))))

     ;; Loop over the terms in expr
     (setq z (cdr expr))
     a    (setq y (car z))

     ;; This m2 pattern matches const*(exp/y)
     (setq r (list '(mplus)
		   (cons '(coeffpt)
			 (cons `(c free12 ,var2)
			       (remove y (cdr expr) :count 1)))))
     (cond
      ;; Case u(var2) is the identity function. y is a term in exp.
      ;; Match if diff(y,var2) == c*(exp/y).
      ;; This even works when y is a function with multiple args.
       ((setq w (m2 (sdiff y var2) r))
	(return (muln (list y y (power* (mul2* 2 (cdr (assoc 'c w :test #'eq))) -1)) nil))))

     ;; w is the arg in y.
     (let ((arg-freevar))
       (setq w
	 (cond
	  ((or (atom y) (member (caar y) '(mplus mtimes) :test #'eq)) y)
	  ;; Take the argument of a function with one value.
	  ((= (length (cdr y)) 1) (cadr y))
	  ;; A function has multiple args, and exactly one arg depends on var2
	  ((= (count-if #'null (setq arg-freevar (mapcar #'(lambda (v)
                                                             (freevar2 v var2))
                                                         (cdr y))))
              1)
	   (do ((args (cdr y) (cdr args))
		(argf arg-freevar (cdr argf)))
	       ((if (not (car argf)) (return (car args))))))
	  (t 0))))

     (cond
       ((setq w (cond ((and (setq x (sdiff w var2))
			    (mplusp x)
			    (setq dd (remove y (cdr expr) :count 1))
			    (setq v (car dd))
			    (mplusp v)
			    (not (cdr dd)))
		       (cond ((setq dd (matchsum (cdr x) (cdr v) var2))
			      (list (cons 'c dd)))
			     (t nil)))
		      (t (m2 x r))))
	(return (cond ((null (setq x (integrallookups y var2))) nil)
		      ((eq w t) x)
		      (t (mul2* x (power* (cdr (assoc 'c w :test #'eq)) -1)))))))
     (setq z (cdr z))
     (when (null z) (return nil))
     (go a)))

(defun subliss (alist expr)
  "Alist is an alist consisting of a variable (symbol) and its value.  expr is
  an expression.  For each entry in alist, substitute the corresponding
  value into expr."
  (let ((x expr))
    (dolist (a alist x)
      (setq x (maxima-substitute (cdr a) (car a) x)))))

(defun substint (x y expres var2 expr)
  (if (and (not (atom expres)) (eq (caar expres) '%integrate))
      (list (car expres) expr var2)
      (substint1 (maxima-substitute x y expres) var2)))

(defun substint1 (expr var2)
  (cond ((atom expr) expr)
	((and (eq (caar expr) '%integrate)
	      (null (cdddr expr))
	      (not (symbolp (caddr expr)))
	      (not (free (caddr expr) var2)))
	 (simplify (list '(%integrate)
			 (mul2 (cadr expr) (sdiff (caddr expr) var2))
			 var2)))
	(t (recur-apply #'(lambda (e)
                            (substint1 e var2))
                        expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;:; Extension of the integrator for more integrals with power functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Recognize (a^(c*(z^r)^p+d)^v

(defun m2-exp-type-1a (expr var2)
  (m2 expr
      `((mexpt)
        ((mexpt)
         (a freevar02 ,var2)
         ((mplus)
          ;; The order of the pattern is critical. If we change it,
          ;; we do not get the expected match.
          ((coeffpp) (d freevar2 ,var2))
          ((coefft) (c freevar02 ,var2)
           ((mexpt)
            ((mexpt) (z varp2 ,var2) (r freevar02 ,var2))
            (p freevar2 ,var2)))))
        (v freevar2 ,var2))))

;;; Recognize z^v*a^(b*z^r+d)

(defun m2-exp-type-2 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (v freevar02 ,var2))
        ((mexpt)
         (a freevar02 ,var2)
         ((mplus)
          ((coeffpp) (d freevar2 ,var2))
          ((coefft) (b freevar02 ,var2) ((mexpt) (z varp2 ,var2) (r freevar02 ,var2))))))))

;;; Recognize z^v*%e^(a*z^r+b)^u

(defun m2-exp-type-2-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (v freevar02 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (b freevar2 ,var2))
           ((coefft) (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) (r freevar02 ,var2)))))
         (u freevar2 ,var2)))))

;;; Recognize (a*z+b)^p*%e^(c*z+d)

(defun m2-exp-type-3 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt)
	   ((mplus)
	      ((coefft) (a freevar02 ,var2) (z varp2 ,var2))
	      ((coeffpp) (b freevar2 ,var2)))
	   (p freevar02 ,var2))
      ((mexpt)
	 $%e
	 ((mplus)
	    ((coefft) (c freevar02 ,var2) (z varp2 ,var2))
	    ((coeffpp) (d freevar2 ,var2)))))))

;;; Recognize d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4 (expr var2)
  (m2 expr
    `((mexpt)
	(d freevar02 ,var2)
	((mplus)
	   ((coefft) (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) 2))
	   ((coefft) (b freevar02 ,var2) ((mexpt) (z varp2 ,var2) -2))
	   ((coeffpp) (c freevar2 ,var2))))))

;;; Recognize z^(2*n)*d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4-1 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt) (z varp2 ,var2) (n freevar02 ,var2))
	((mexpt)
	   (d freevar02 ,var2)
	   ((mplus)
	      ((coefft)  (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) 2))
	      ((coefft)  (b freevar02 ,var2) ((mexpt) (z varp2 ,var2) -2))
	      ((coeffpp) (c freevar2 ,var2)))))))

;;; Recognize z^n*d^(a*z^2+b*z+c)

(defun m2-exp-type-5 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
        ((mexpt)
         (d freevar02 ,var2)
         ((mplus)
          ((coeffpt) (a freevar2 ,var2) ((mexpt) (z varp2 ,var2) 2))
          ((coeffpt) (b freevar2 ,var2) (z varp2 ,var2))
          ((coeffpp) (c freevar2 ,var2)))))))

;;; Recognize z^n*(%e^(a*z^2+b*z+c))^u

(defun m2-exp-type-5-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (n freevar02 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (c freevar2 ,var2))
           ((coefft) (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) 2))
           ((coefft) (b freevar02 ,var2) (z varp2 ,var2))))
         (u freevar2 ,var2)))))

;;; Recognize z^n*d^(a*sqrt(z)+b*z+c)

(defun m2-exp-type-6 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt) (z varp2 ,var2) (n freevar02 ,var2))
	((mexpt)
	   (d freevar02 ,var2)
	   ((mplus)
	      ((coefft) (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
	      ((coefft) (b freevar02 ,var2) (z varp2 ,var2))
	      ((coeffpp) (c freevar2 ,var2)))))))

;;; Recognize z^n*(%e^(a*sqrt(z)+b*z+c))^u

(defun m2-exp-type-6-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (n freevar02 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (c freevar2 ,var2))
           ((coefft) (a freevar02 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
           ((coefft) (b freevar02 ,var2) (z varp2 ,var2))))
         (u freevar2 ,var2)))))

;;; Recognize z^n*a^(b*z^r+e)*h^(c*z^r+g)

(defun m2-exp-type-7 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
	((mexpt)
	   (a freevar02 ,var2)
	   ((mplus)
	      ((coefft)
		 (b freevar02 ,var2)
		 ((mexpt) (z varp2 ,var2) (r freevar02 ,var2)))
	      ((coeffpp) (e freevar2 ,var2))))
	((mexpt)
	   (h freevar02 ,var2)
	   ((mplus)
	      ((coefft)
		 (c freevar02 ,var2)
		 ((mexpt) (z varp2 ,var2) (r1 freevar02 ,var2)))
	      ((coeffpp) (g freevar2 ,var2)))))))

;;; Recognize z^v*(%e^(b*z^r+e))^q*(%e^(c*z^r+g))^u

(defun m2-exp-type-7-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (v freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar2 ,var2))
           ((coefft) (b freevar02 ,var2) ((mexpt) (z varp2 ,var2) (r freevar02 ,var2)))))
         (q freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar2 ,var2))
           ((coefft) (c freevar02 ,var2) ((mexpt) (z varp2 ,var2) (r1 freevar02 ,var2)))))
         (u freevar2 ,var2)))))

;;; Recognize a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)

(defun m2-exp-type-8 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt)
	   (a freevar02 ,var2)
	   ((mplus)
	      ((coeffpt) (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
	      ((coeffpt) (d freevar2 ,var2) (z varp2 ,var2))
	      ((coeffpp) (e freevar2 ,var2))))
	((mexpt)
	   (h freevar02 ,var2)
	   ((mplus)
	      ((coeffpt) (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
	      ((coeffpt) (f freevar2 ,var2) (z varp2 ,var2))
	      ((coeffpp) (g freevar2 ,var2)))))))

;;; Recognize (%e^(b*sqrt(z)+d*z+e))^u*(%e^(c*sqrt(z)+f*z+g))^v

(defun m2-exp-type-8-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar2 ,var2))
           ((coeffpt) (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
           ((coeffpt) (d freevar2 ,var2) (z varp2 ,var2))))
         (u freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar2 ,var2))
           ((coeffpt) (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
           ((coeffpt) (f freevar2 ,var2) (z varp2 ,var2))))
         (v freevar2 ,var2)))))

;;; Recognize (%e^(b*z^r+e))^u*(%e^(c*z^r+g))^v

(defun m2-exp-type-8-2 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar2 ,var2))
           ((coefft) (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) (r freevar02 ,var2)))))
         (u freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar2 ,var2))
           ((coefft) (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) (r1 freevar02 ,var2)))))
         (v freevar2 ,var2)))))

;;; Recognize z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)

(defun m2-exp-type-9 (expr var2)
  (m2 expr
    `((mtimes)
      ((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
      ((mexpt)
	 (a freevar02 ,var2)
	 ((mplus)
	    ((coeffpt)  (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) 2))
	    ((coeffpt)  (d freevar2 ,var2) (z varp2 ,var2))
	    ((coeffpp) (e freevar2 ,var2))))
      ((mexpt)
	 (h freevar02 ,var2)
	 ((mplus)
	    ((coeffpt)  (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) 2))
	    ((coeffpt)  (f freevar2 ,var2) (z varp2 ,var2))
	    ((coeffpp) (g freevar2 ,var2)))))))

;;; Recognize z^n*(%e^(b*z^2+d*z+e))^q*(%e^(c*z^2+f*z+g))^u

(defun m2-exp-type-9-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar2 ,var2))
           ((coeffpt) (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) 2))
           ((coeffpt) (d freevar2 ,var2) (z varp2 ,var2))))
         (q freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar2 ,var2))
           ((coeffpt) (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) 2))
           ((coeffpt) (f freevar2 ,var2) (z varp2 ,var2))))
         (u freevar2 ,var2)))))

;;; Recognize z^n*a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z+)f*z+g)

(defun m2-exp-type-10 (expr var2)
  (m2 expr
    `((mtimes)
	((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
	((mexpt)
	   (a freevar02 ,var2)
	   ((mplus)
	      ((coeffpt)  (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
	      ((coeffpt)  (d freevar2 ,var2) (z varp2 ,var2))
	      ((coeffpp) (e freevar2 ,var2))))
	((mexpt)
	   (h freevar02 ,var2)
	   ((mplus)
	      ((coeffpt)  (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
	      ((coeffpt)  (f freevar2 ,var2) (z varp2 ,var2))
	      ((coeffpp) (g freevar2 ,var2)))))))

;;; Recognize z^n*(%e^(b*sqrt(z)+d*z+e))^q*(%e^(c*sqrt(z)+f*z+g))^u

(defun m2-exp-type-10-1 (expr var2)
  (m2 expr
      `((mtimes)
        ((mexpt) (z varp2 ,var2) (n freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar2 ,var2))
           ((coeffpt) (b freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
           ((coeffpt) (d freevar2 ,var2) (z varp2 ,var2))))
         (q freevar2 ,var2))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar2 ,var2))
           ((coeffpt) (c freevar2 ,var2) ((mexpt) (z varp2 ,var2) ((rat) 1 2)))
           ((coeffpt) (f freevar2 ,var2) (z varp2 ,var2))))
         (u freevar2 ,var2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integrate-exp-special (expr var2 &aux w const)

  ;; First factor the expression.
  (setq expr ($factor expr))

  ;; Remove constant factors.
  (setq w (partition expr var2 1))
  (setq const (car w))
  (setq expr (cdr w))

  (schatchen-cond w
    ((m2-exp-type-1a (facsum-exponent expr var2) var2)
     (a c d r p v)
     (when *debug-integrate*
       (format t "~&Type 1a: (a^(c*(z^r)^p+d)^v : w = ~A~%" w))

     (mul -1
	  const
	  ;; 1/(p*r*(a^(c*v*(var2^r)^p)))
	  (inv (mul p r (power a (mul c v (power (power var2 r) p)))))
	  var2
	  ;; (a^(d+c*(var2^r)^p))^v
	  (power (power a (add d (mul c (power (power var2 r) p)))) v)
	  ;; gamma_incomplete(1/(p*r), -c*v*(var2^r)^p*log(a))
	  (take '(%gamma_incomplete)
		(inv (mul p r))
		(mul -1 c v (power (power var2 r) p) (take '(%log) a)))
	  ;; (-c*v*(var2^r)^p*log(a))^(-1/(p*r))
	  (power (mul -1 c v (power (power var2 r) p) (take '(%log) a))
		 (div -1 (mul p r)))))

    ((m2-exp-type-2 (facsum-exponent expr var2) var2)
     (a b d v r)

     (when *debug-integrate*
       (format t "~&Type 2: z^v*a^(b*z^r+d) : w = ~A~%" w))

     (mul
      const
      (div -1 r)
      (power a d)
      (power var2 (add v 1))
      ($gamma_incomplete
       (div (add v 1) r)
       (mul -1 b (power var2 r) ($log a)))
      (power
       (mul -1 b (power var2 r) ($log a))
       (mul -1 (div (add v 1) r)))))

    ((m2-exp-type-2-1 (facsum-exponent expr var2) var2)
     (a b v r u)
     (when *debug-integrate*
       (format t "~&Type 2-1: z^v*(%e^(a*z^r+b))^u : w = ~A~%" w))

     (mul const
          -1
          (inv r)
          (power '$%e (mul -1 a u (power var2 r)))
          (power (power '$%e (add (mul a (power var2 r)) b)) u)
          (power var2 (add v 1))
          (power (mul -1 a u (power var2 r)) (div (mul -1 (add v 1)) r))
          (take '(%gamma_incomplete)
                (div (add v 1) r)
                (mul -1 a u (power var2 r)))))

    ((m2-exp-type-3 (facsum-exponent expr var2) var2)
     (a b c d p)
     (when *debug-integrate*
       (format t "~&Type 3: (a*z+b)^p*%e^(c*z+d) : w = ~A~%" w))
     (mul
      const
      (div -1 a)
      (power '$%e (sub d (div (mul b c) a)))
      (power (add b (mul a var2)) (add p 1))
      (ftake '%expintegral_e (mul -1 p) (mul (div -1 a) c (add b (mul a var2))))))

    ((m2-exp-type-4 expr var2)
     (a b c d)
     (let (($trigsign nil))             ; Do not simplify erfc(-x) !
       (when *debug-integrate*
	 (format t "~&Type 4: d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (mul
        const
        (div 1 (mul 4 (power (mul -1 a ($log d)) (div 1 2))))
        (mul
         (power d c)
         (power '$%pi (div 1 2))
         (power '$%e
                (mul -2
                     (power (mul -1 a ($log d)) (div 1 2))
                     (power (mul -1 b ($log d)) (div 1 2))))
         (add
          ($erfc
           (add
            (div (power (mul -1 b ($log d)) (div 1 2)) var2)
            (mul -1 var2 (power (mul -1 a ($log d)) (div 1 2)))))
          (mul -1
	       (power '$%e
                      (mul 4
                           (power (mul -1 a ($log d)) (div 1 2))
                           (power (mul -1 b ($log d)) (div 1 2))))
	       ($erfc
                (add
                 (mul var2 (power (mul -1 a ($log d)) (div 1 2)))
                 (div (power (mul -1 b ($log d)) (div 1 2)) var2)))))))))

    ((and (m2-exp-type-4-1 expr var2)
	  (poseven (cdras 'n w))  ; only for n a positive, even integer
	  (symbolp (cdras 'a w))) ; a has to be a symbol
     (a b c d n)
     (let (($trigsign nil)) ; Do not simplify erfc(-x) !

       (when *debug-integrate*
	 (format t "~&Type 4-1: z^(2*n)*d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (setq n (div n 2))

       (mul const
            (div 1 4)
	    (power d c)
	    (power '$%pi (div 1 2))
	    (simplify (list '(%derivative)
	     (div
	       (sub
		 (mul
		   (power ($log d) (mul -1 n))
		   (add
		     (mul
		       (power
			 '$%e
			 (mul -2
			   (power (mul -1 a ($log d)) (div 1 2))
			   (power (mul -1 b ($log d)) (div 1 2))))
		     ($erfc
		       (sub
			 (div
			   (power (mul -1 b ($log d)) (div 1 2))
			   var2)
			 (mul var2 (power (mul -1 ($log d)) (div 1 2))))))))
		 (mul
		   (power
		     '$%e
		     (mul 2
		       (power (mul -1 a ($log d)) (div 1 2))
		       (power (mul -1 b ($log d)) (div 1 2))))
		   ($erfc
		     (add
		       (power (mul -1 a ($log d)) (div 1 2))
		       (div (power (mul -1 b ($log d)) (div 1 2)) var2)))))
	       (power (mul -1 a ($log d)) (div 1 2)))
	     a n)))))

    ((and (m2-exp-type-5 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (a b c d n)

     (when *debug-integrate*
       (format t "~&Type 5: z^n*d^(a*z^2+b*z+c) : w = ~A~%" w))

     (mul
      const
      (div -1 (mul 2 (power (mul a ($log d)) (div 1 2))))
      (mul
       (power d (sub c (div (mul b b) (mul 4 a))))
       (let ((index (gensumindex))
             ($simpsum t))
         (mfuncall '$sum
                   (mul
                    (power 2 (sub index n))
                    (ftake '%binomial n index)
                    ($gamma_incomplete
                     (div (add index 1) 2)
                     (mul
                      (div -1 (mul 4 a))
                      (power (add b (mul 2 a var2)) 2)
                      ($log d)))
                    (power (mul a ($log d)) (mul -1 (add n (div 1 2))))
                    (power (mul -1 b ($log d)) (sub n index))
                    (power (mul (add b (mul 2 a var2)) ($log d)) (add index 1))
                    (power
                     (mul (div -1 a) (power (add b (mul 2 a var2)) 2) ($log d))
                     (mul (div -1 2) (add index 1))))
                   index 0 n)))))

    ((and (m2-exp-type-5-1 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos))
     (a b c u n)
     (when *debug-integrate*
       (format t "~&Type 5-1: z^n*(%e^(a*z^2+b*z+c))^u : w = ~A~%" w))

     (mul const
          (div -1 2)
          (power '$%e
                 (add (mul -1 (div (mul b b u) (mul 4 a)))
                      (mul -1 u (add (mul a var2 var2) (mul b var2)))))
          (power a (mul -1 (add n 1)))
          (power (power '$%e
                        (add (mul a var2 var2) (mul b var2) c))
                 u)
          (let ((index (gensumindex))
                ($simpsum t))
            (dosum
             (mul (power 2 (sub index n))
                  (power (mul -1 b) (sub n index))
                  (power (add b (mul 2 a var2)) (add index 1))
                  (power (div (mul -1 u (power (add b (mul 2 a var2)) 2)) a)
                         (mul (div -1 2) (add index 1)))
                  (take '(%binomial) n index)
                  (take '(%gamma_incomplete)
                        (div (add index 1) 2)
                        (div (mul -1 u (power (add b (mul 2 a var2)) 2))
                             (mul 4 a))))
             index 0 n t))))

    ((and (m2-exp-type-6 (facsum-exponent expr var2) var2)
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (a b c d n)
     (when *debug-integrate*
       (format t "~&Type 6: z^n*d^(a*sqrt(z)+b*z+c) : w = ~A~%" w))

     (mul
      const
      (power 2 (mul -1 (add n 1)))
      (power d (sub c (div (mul a a) (mul 4 b))))
      (power (mul b ($log d)) (mul -2 (add n 1)))
      (let ((index1 (gensumindex))
            (index2 (gensumindex))
            ($simpsum t))
        (mfuncall '$sum
                  (mfuncall '$sum
                            (mul
                             (power -1 (sub index1 index2))
                             (power 4 index1)
                             (ftake '%binomial index1 index2)
                             (ftake '%binomial n index1)
                             ($log d)
                             (power (mul a ($log d)) (sub (mul 2 n) (add index1 index2)))
                             (power
                              (mul (add a (mul 2 b (power var2 (div 1 2)))) ($log d))
                              (add index1 index2))
                             (power
                              (mul
                               (div -1 b)
                               (power (add a (mul 2 b (power var2 (div 1 2)))) 2)
                               ($log d))
                              (mul (div -1 2) (add index1 index2 1)))
                             (add
                              (mul 2 b
                                   (power
                                    (mul
                                     (div -1 b)
                                     (power (add a (mul 2 b (power var2 (div 1 2)))) 2)
                                     ($log d))
                                    (div 1 2))
                                   ($gamma_incomplete
                                    (div (add index1 index2 2) 2)
                                    (mul
                                     (div -1 (mul 4 b))
                                     (power (add a (mul 2 b (power var2 (div 1 2)))) 2)
                                     ($log d))))
                              (mul a
                                   (add a (mul 2 b (power var2 (div 1 2))))
                                   ($log d)
                                   ($gamma_incomplete
                                    (div (add index1 index2 1) 2)
                                    (mul
                                     (div -1 (mul 4 b))
                                     (power (add a (mul 2 b (power var2 (div 1 2)))) 2)
                                     ($log d))))))
                            index2 0 index1)
                  index1 0 n))))

    ((and (m2-exp-type-6-1 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos))
     (a b c u n)
     (when *debug-integrate*
       (format t "~&Type 6-1: z^n*(%e^(a*sqrt(z)+b*z+c))^u : w = ~A~%" w))

     (mul const
          (power 2 (mul -1 (add (mul 2 n) 1)))
          (power '$%e
                 (add (div (mul -1 u a a) (mul 4 b))
                      (mul u (add (mul a (power var2 (div 1 2)))
                                  (mul b var2)
                                  c))))
          (power b (mul -2 (add n 1)))
          (power (power '$%e
                        (add (mul a (power var2 (div 1 2)))
                             (mul b var2)))
                 u)
          (let ((index1 (gensumindex))
                (index2 (gensumindex))
                ($simpsum t))
            (dosum
             (dosum
              (mul (power -1 (sub index1 index2))
                   (power 4 index1)
                   (power a (add (neg index2) (neg index1) (mul 2 n)))
                   (power (add a (mul 2 b (power var2 (div 1 2))))
                          (add index1 index2))
                   (power (div (mul -1 u
                                    (power (add a
                                                (mul 2
                                                     b
                                                     (power var2 (div 1 2))))
                                           2))
                               b)
                          (mul (div -1 2) (add index1 index2 1)))
                   (take '(%binomial) index1 index2)
                   (take '(%binomial) n index1)
                   (add (mul a
                             (add a (mul 2 b (power var2 (div 1 2))))
                             (take '(%gamma_incomplete)
                                   (div (add index1 index2 1) 2)
                                   (div (mul -1 u
                                             (power (add a
                                                         (mul 2 b
                                                              (power var2
                                                                     (div 1 2))))
                                                    2))
                                        (mul 4 b))))
                        (mul (inv u)
                             (power (div (mul -1 u
                                              (power (add a
                                                          (mul 2 b
                                                               (power var2
                                                                      (div 1 2))))
                                                     2))
                                         b)
                                    (div 1 2))
                             (mul 2 b)
                             (take '(%gamma_incomplete)
                                   (div (add index1 index2 2) 2)
                                   (div (mul -1 u
                                             (power (add a
                                                         (mul 2 b
                                                              (power var2 (div 1 2))))
                                                    2))
                                        (mul 4 b))))))
              index2 0 index1 t)
             index1 0 n t))))

    ((and (m2-exp-type-7 (facsum-exponent expr var2) var2)
	  (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (a b c e g h r n)
     (when *debug-integrate*
       (format t "~&Type 7: z^n*a^(b*z^r+e)*h^(c*z^r+g) : w = ~A~%" w))

     (setq n (add n 1))

     (mul
      const
      (power var2 n)
      (div -1 r)
      (power a e)
      (power h g)
      (power
       (mul -1
            (power var2 r)
            (add (mul b ($log a)) (mul c ($log h))))
       (div (mul -1 n) r))
      ($gamma_incomplete
       (div n r)
       (mul -1 (power var2 r) (add (mul b ($log a)) (mul c ($log h)))))))

    ((and (m2-exp-type-7-1 (facsum-exponent expr var2) var2)
          (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (b c e g r v q u)
     (when *debug-integrate*
       (format t "~&Type 7-1: z^v*(%e^(b*z^r+e))^q*(%e^(c*z^r+g))^u : w = ~A~%" w))

     (mul const
          (div -1 r)
          (power '$%e (mul -1 (power var2 r) (add (mul b q) (mul c u))))
          (power (power '$%e (add e (mul b (power var2 r)))) q)
          (power (power '$%e (add g (mul c (power var2 r)))) u)
          (power var2 (add v 1))
          (power (mul -1 (power var2 r) (add (mul b q) (mul c u)))
                 (div (mul -1 (add v 1)) r))
          (take '(%gamma_incomplete)
                (div (add v 1) r)
                (mul -1 (power var2 r) (add (mul b q) (mul c u))))))

    ((m2-exp-type-8 (facsum-exponent expr var2) var2)
     (a b c d e f g h)
     (when *debug-integrate*
       (format t "~&Type 8: a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
       (format t "~&   : w = ~A~%" w))

     (mul
      const
      (div 1 2)
      (power a e)
      (power h g)
      (add
       (mul 2
            (power a (add (mul b (power var2 (div 1 2))) (mul d var2)))
            (power h (add (mul c (power var2 (div 1 2))) (mul f var2)))
            (div 1 (add (mul d ($log a)) (mul f ($log h)))))
       (mul -1
            (power '$%pi (div 1 2))
            (power '$%e
                   (mul -1
                        (div
                         (power (add (mul b ($log a)) (mul c ($log h))) 2)
                         (mul 4 (add (mul d ($log a)) (mul f ($log h)))))))
            ($erfi
             (div
              (add
               (mul b ($log a))
               (mul c ($log h))
               (mul 2
                    (power var2 (div 1 2))
                    (add (mul d ($log a)) (mul f ($log h)))))
              (mul 2
                   (power (add (mul d ($log a)) (mul f ($log h))) (div 1 2)))))
            (add (mul b ($log a)) (mul c ($log h)))
            (power (add (mul d ($log a)) (mul f ($log h))) (div -3 2))))))

    ((m2-exp-type-8-1 (facsum-exponent expr var2) var2)
     (b c d e f g u v)
     (when *debug-integrate*
       (format t "~&Type 8-1: (%e^(b*sqrt(z)+d*z+e))^u*(%e^(c*sqrt(z)+f*z+g))^v")
       (format t "~&   : w = ~A~%" w))

     (mul const
          (div 1 2)
          (power (add (mul d u) (mul f v)) (div -3 2))
          (mul (power '$%e
                      (mul -1
                           (power (add (mul b u)
                                       (mul 2 d u (power var2 (div 1 2)))
                                       (mul v (add c (mul 2 f (power var2 (div 1 2))))))
                                  2)
                           (inv (mul 4 (add (mul d u) (mul f v))))))
               (power (power '$%e
                             (add (mul b (power var2 (div 1 2)))
                                  e
                                  (mul d var2)))
                      u)
               (power (power '$%e
                             (add (mul c (power var2 (div 1 2)))
                                  g
                                  (mul f var2)))
                      v)
               (add (mul 2
                         (power '$%e
                                (mul (power (add (mul b u)
                                                 (mul 2 d u (power var2 (div 1 2)))
                                                 (mul v (add c (mul 2 f (power var2 (div 1 2))))))
                                            2)
                                     (inv (mul 4 (add (mul d u) (mul f v))))))
                         (power (add (mul d u) (mul f v)) (div 1 2)))
                    (mul -1
                         (power '$%pi (div 1 2))
                         (add (mul b u) (mul c v))
                         (take '(%erfi)
                               (div (add (mul b u)
                                         (mul 2 d u (power var2 (div 1 2)))
                                         (mul c v)
                                         (mul 2 f v (power var2 (div 1 2))))
                                    (mul 2
                                         (power (add (mul d u) (mul f v))
                                                (div 1 2))))))))))

    ((and (m2-exp-type-8-2 (facsum-exponent expr var2) var2)
          (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (b c e g r u v)
     (when *debug-integrate*
       (format t "~&Type 8-2: (%e^(b*z^r+e))^u*(%e^(c*z^r+g))^v")
       (format t "~&   : w = ~A~%" w))

     (mul const
          -1
          (inv r)
          (power '$%e
                 (mul -1
                      (power var2 r)
                      (add (mul b u) (mul c v))))
          (power (power '$%e
                        (add (power var2 r) e))
                 u)
          (power (power '$%e
                        (add (power var2 r) g))
                 v)
          var2
          (power (mul -1
                      (power var2 r)
                      (add (mul b u) (mul c v)))
                 (div -1 r))
          (take '(%gamma_incomplete)
                (div 1 r)
                (mul -1 (power var2 r) (add (mul b u) (mul c v))))))

    ((and (m2-exp-type-9 (facsum-exponent expr var2) var2)
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos)
	  (or (not (eq ($sign (cdras 'b w)) '$zero))
	      (not (eq ($sign (cdras 'c w)) '$zero))))
     (a b c d e f g h n)
     (when *debug-integrate*
       (format t "~&Type 9: z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)")
       (format t "~&   : w = ~A~%" w))

     (mul
      const
      (div -1 2)
      (power a e)
      (power h g)
      (power '$%e
             (div
              (power (add (mul d ($log a)) (mul f ($log h))) 2)
              (mul -4 (add (mul b ($log a)) (mul c ($log h))))))
      (power (add (mul b ($log a)) (mul c ($log h))) (mul -1 (add n 1)))
      (let ((index (gensumindex))
            ($simpsum t))
        (mfuncall '$sum
                  (mul
                   (power 2 (sub index n))
                   (ftake '%binomial n index)
                   (power
                    (add (mul -1 d ($log a)) (mul -1 f ($log h)))
                    (sub n index))
                   (power
                    (add
                     (mul (add d (mul 2 b var2)) ($log a))
                     (mul (add f (mul 2 c var2)) ($log h)))
                    (add index 1))
                   (power
                    (mul -1
                         (div
                          (power
                           (add
                            (mul (add d (mul 2 b var2)) ($log a))
                            (mul (add f (mul 2 c var2)) ($log h)))
                           2)
                          (add (mul b ($log a)) (mul c ($log h)))))
                    (div (add index 1) -2))
                   ($gamma_incomplete
                    (div (add index 1) 2)
                    (mul -1
                         (div
                          (power
                           (add
                            (mul (add d (mul 2 b var2)) ($log a))
                            (mul (add f (mul 2 c var2)) ($log h)))
                           2)
                          (mul 4 (add (mul b ($log a)) (mul c ($log h))))))))
                  index 0 n))))

    ((and (m2-exp-type-9-1 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (b c d e f g q u n)
     (when *debug-integrate*
       (format t "~&Type 9-1: z^n*(%e^(b*z^2+d*z+e))^q*(%e^(c*z^2+f*z+g))^u")
       (format t "~&   : w = ~A~%" w))

     (mul const
          (div -1 2)
          (power (add (mul b q) (mul c u)) (div -1 2))
          (power '$%e
                 (add (div (power (add (mul d q) (mul f u)) 2)
                           (mul -4 (add (mul b q) (mul c u))))
                      (mul -1 var2
                           (add (mul d q)
                                (mul b q var2)
                                (mul f u)
                                (mul c u var2)))))
          (power (power '$%e
                        (add e
                             (mul var2 (add d (mul b var2)))))
                 q)
          (power (power '$%e
                        (add g
                             (mul var2 (add f (mul c var2)))))
                 u)
          (let ((index (gensumindex))
                ($simpsum t))
            (dosum
             (mul (power 2 (sub index n))
                  (power (add (mul b q) (mul c u)) (neg (add n (div 1 2))))
                  (power (add (neg (mul d q)) (neg (mul f u)))
                         (sub n index))
                  (power (add (mul d q)
                              (mul f u)
                              (mul 2 var2 (add (mul b q) (mul c u))))
                         (add index 1))
                  (power (div (power (add (mul d q)
                                          (mul f u)
                                          (mul 2
                                               (add (mul b q)
                                                    (mul c u))
                                               var2))
                                     2)
                              (neg (add (mul b q) (mul c u))))
                         (mul (div -1 2) (add index 1)))
                  (take '(%binomial) n index)
                  (take '(%gamma_incomplete)
                        (div (add index 1) 2)
                        (div (power (add (mul d q)
                                         (mul f u)
                                         (mul 2
                                              (add (mul b q)
                                                   (mul c u))
                                              var2))
                                    2)
                             (mul -4 (add (mul b q) (mul c u))))))
             index 0 n t))))

    ((and (m2-exp-type-10 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (a b c d e f g h n)
     (when *debug-integrate*
       (format t "~&Type 10: z^n*a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
       (format t "~&   : w = ~A~%" w))

     (mul const
          (power 2 (add (mul -2 n) -1))
          (power a e)
          (power h g)
          (power '$%e
                 (div (power (add (mul b ($log a)) (mul c ($log h))) 2)
                      (mul -4 (add (mul d ($log a)) (mul f ($log h))))))
          (power (add (mul d ($log a)) (mul f ($log h))) (mul -2 (add n 1)))
          (let ((index1 (gensumindex))
                (index2 (gensumindex))
                ($simpsum t))
            (dosum
             (dosum
              (mul (power -1 (sub index1 index2))
                   (power 4 index1)
                   (ftake '%binomial index1 index2)
                   (ftake '%binomial n index1)
                   (power (add (mul b ($log a)) (mul c ($log h)))
                          (sub (mul 2 n) (add index1 index2)))
                   (power (add (mul b ($log a))
                               (mul c ($log h))
                               (mul 2
                                    (power var2 (div 1 2))
                                    (add (mul d ($log a)) (mul f ($log h)))))
                          (add index1 index2))
                   (power (mul -1
                               (div (power (add (mul b ($log a))
                                                (mul c ($log h))
                                                (mul 2
                                                     (power var2 (div 1 2))
                                                     (add (mul d ($log a))
                                                          (mul f ($log h)))))
                                           2)
                                    (add (mul d ($log a)) (mul f ($log h)))))
                          (mul (div -1 2) (add index1 index2 1)))
                   (add (mul ($gamma_incomplete (mul (div 1 2)
                                                     (add index1 index2 1))
                                                (mul (div -1 4)
                                                     (div (power (add (mul b ($log a))
                                                                      (mul c ($log h))
                                                                      (mul 2
                                                                           (power var2 (div 1 2))
                                                                           (add (mul d ($log a)) (mul f ($log h)))))
                                                                 2)
                                                          (add (mul d ($log a)) (mul f ($log h))))))
                             (add (mul b ($log a)) (mul c ($log h)))
                             (add (mul b ($log a))
                                  (mul c ($log h))
                                  (mul 2
                                       (power var2 (div 1 2))
                                       (add (mul d ($log a)) (mul f ($log h))))))
                        (mul 2
                             ($gamma_incomplete (mul (div 1 2)
                                                     (add index1 index2 2))
                                                (mul (div -1 4)
                                                     (div (power (add (mul b ($log a))
                                                                      (mul c ($log h))
                                                                      (mul 2
                                                                           (power var2 (div 1 2))
                                                                           (add (mul d ($log a))
                                                                                (mul f ($log h)))))
                                                                 2)
                                                          (add (mul d ($log a))
                                                               (mul f ($log h))))))
                             (add (mul d ($log a)) (mul f ($log h)))
                             (power (mul -1
                                         (div (power (add (mul b ($log a))
                                                          (mul c ($log h))
                                                          (mul 2
                                                               (power var2 (div 1 2))
                                                               (add (mul d ($log a))
                                                                    (mul f ($log h)))))
                                                     2)
                                              (add (mul d ($log a))
                                                   (mul f ($log h)))))
                                    (div 1 2)))))
              index2 0 index1 t)
             index1 0 n t))))

    ((and (m2-exp-type-10-1 (facsum-exponent expr var2) var2)
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (b c d e f g q u n)
     (let ((bq+cu (add (mul b q) (mul c u)))
           (dq+fu (add (mul d q) (mul f u))))
       (when *debug-integrate*
         (format t "~&Type 10-1: z^n*(%e^(b*sqrt(z)+d*z+e))^q*(%e^(c*sqrt(z)+f*z+g))^u")
         (format t "~&   : w = ~A~%" w))

       (mul const
            (power 2 (mul -1 (add (mul 2 n) 1)))
            (power '$%e
                   (add (div (mul -1 (power bq+cu 2)) (mul 4 dq+fu))
                        (mul -1 d var2 q)
                        (mul -1 b (power var2 (div 1 2)) q)
                        (mul -1 f var2 u)
                        (mul -1 c (power var2 (div 1 2)) u)))
            (power (power '$%e
                          (add (mul b (power var2 (div 1 2)))
                               (mul d var2)
                               e))
                   q)
            (power (power '$%e
                          (add (mul c (power var2 (div 1 2)))
                               (mul f var2)
                               g))
                   u)
            (power dq+fu (mul -2 (add n 1)))
            (let ((index1 (gensumindex))
                  (index2 (gensumindex))
                  ($simpsum t))
              (dosum
               (dosum
                (mul (power -1 (sub index1 index2))
                     (power 4 index1)
                     (power bq+cu
                            (add (neg index1) (neg index2) (mul 2 n)))
                     (power (add bq+cu
                                 (mul 2 (power var2 (div 1 2)) dq+fu))
                            (add index1 index2))
                     (power (div (power (add bq+cu
                                             (mul 2
                                                  (power var2 (div 1 2))
                                                  dq+fu))
                                        2)
                                 (mul -1 dq+fu))
                            (mul (div -1 2)
                                 (add index1 index2 1)))
                     (take '(%binomial) index1 index2)
                     (take '(%binomial) n index1)
                     (add (mul bq+cu
                               (add bq+cu
                                    (mul 2
                                         (power var2 (div 1 2))
                                         dq+fu))
                               (take '(%gamma_incomplete)
                                     (mul (div 1 2)
                                          (add index1 index2 1))
                                     (div (power (add (mul b q)
                                                      (mul c u)
                                                      (mul 2
                                                           (power var2 (div 1 2))
                                                           dq+fu))
                                                 2)
                                          (mul -4
                                               dq+fu))))
                          (mul 2
                               (power (div (power (add bq+cu
                                                       (mul 2
                                                            (power var2 (div 1 2))
                                                            dq+fu))
                                                  2)
                                           (mul 1 dq+fu))
                                      (div 1 2))
                               dq+fu
                               (take '(%gamma_incomplete)
                                     (mul (div 1 2)
                                          (add index1 index2 2))
                                     (div (power (add bq+cu
                                                      (mul 2
                                                           (power var2 (div 1 2))
                                                           dq+fu))
                                                 2)
                                          (mul -4
                                               dq+fu))))))
                index2 0 index1 t)
               index1 0 n t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Do a facsum for the exponent of power functions.
;;; This is necessary to integrate all general forms. The pattern matcher is
;;; not powerful enough to do the job.

(defun facsum-exponent (expr var2)
  ;; Make sure that expr has the form ((mtimes) factor1 factor2 ...)
  (when (not (mtimesp expr)) (setq expr (list '(mtimes) expr)))
  (do ((result nil)
       (l (cdr expr) (cdr l)))
      ((null l) (cons (list 'mtimes) result))
    (cond
      ((mexptp (car l))
       ;; Found an power function. Factor the exponent with facsum.
       (let* ((fac (mfuncall '$facsum (caddr (car l)) var2))
              (num ($num fac))
              (den ($denom fac)))
         (setq result
               (cons (cons (list 'mexpt) 
                           (cons (cadr (car l))
                                 (if (equal 1 den)
                                     (list num)
                                     (list ($multthru (inv den) num)))))
                     result))))
      (t
       ;; Nothing to do.
       (setq result (cons (car l) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

