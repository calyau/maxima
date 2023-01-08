;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;       Maintained by GJC                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trans3)

;;; The translation of macsyma LAMBDA into lexicaly scoped closures.
;;; Two cases [1] the downward transmission of variable binding environment,
;;; e.g. MAP(LAMBDA([U],F(U,X)),EXP)
;;; [2] downward and upward, requiring a full closure, e.g.
;;; MAP(LAMBDA([U],SUM:SUM+U),EXP);

;;; LAMBDA([U],F(U,X)) =>
;;; (DOWN-CLOSE (LAMBDA (U) (F U X)) (X))

;;; TBIND, TBOUNDP, and TUNBIND and TUNBINDS hack lexical scoping.

;;; A function to determine free vars from a lisp expression.
;;; It returns a <var-set> which is a list of pairs
;;; (<var> . <side-effectp>)

;;; N.B. This code does a veritable storm of consing, it need not
;;; do any if it used the lambda-bound plist scheme of GJC;UTRANS >
;;; a compiler is allowed to cons though, isn't it?

(defun free-lisp-vars (exp &aux prop)
  (cond ((atom exp)
	 (cond ((or (null exp)(eq t exp)) nil)
	       ((symbolp exp) `((,exp . nil)))
	       (t nil)))
	((atom (car exp))
	 (cond ((setq prop (get (car exp) 'free-lisp-vars))
		(funcall prop exp))
	       ((setq prop (get (car exp) 'macro))
		(free-lisp-vars (funcall prop exp)))
	       ((getl (car exp) '(fsubr fexpr))
		(warn-fexpr (car exp)
			    "environment may fail to be correct.")
		(free-lisp-vars-of-argl (cdr exp)))
	       (t
		(free-lisp-vars-of-argl (cdr exp)))))
	((eq (caar exp) 'lambda)
	 (sum-var-sets (free-lisp-vars (car exp))
		       (free-lisp-vars-of-argl (cdr exp))))
	(t
	 (barfo (intl:gettext "encountered an unrecognized Lisp expression in FREE-LISP-VARS.")))))


(defun free-lisp-vars-of-argl (argl)
  (union-var-set (mapcar #'free-lisp-vars argl)))

;;; (REDUCE-VAR-SET '((A . NIL) NIL (B . T) (B . NIL))) => ((A . NIL) (B . T))
;;;  mult-set reduction.

(defun reduce-var-set&op (var-set op)
  (do ((var-set var-set (cdr var-set))
       (reduced-var-set nil)
       (var1)
       (var2))
      ((null var-set) reduced-var-set)
    (setq var1 (car var-set))
    (cond ((null var1))
	  ((setq var2 (assoc (car var1) reduced-var-set :test #'eq))
	   (rplacd var2 (funcall op (cdr var1) (cdr var2))))
	  (t
	   (push var1 reduced-var-set)))))

(defun reduce-var-set (var-set)
  (reduce-var-set&op var-set #'(lambda (p1 p2)(or p1 p2))))

;;; S1 - S2. S1 reduced, minus any vars that are in S2.

(defun difference-var-sets (s1 s2)
  (setq s1 (reduce-var-set s1))
  (do ((s nil))
      ((null s1) s)
    (cond ((assoc (caar s1) s2 :test #'eq))	;;; is the first elem of S1 a member of S2?
	  (t
	   (push (car s1) s)))  ;;; yes. shove it in.
    (pop s1)))

;;; N.B. union of var sets is defined classicaly ala G.F.

(defun union-var-set (set-of-var-sets)
  (reduce-var-set (apply #'append set-of-var-sets)))

;;; SUM-VAR-SETS is the usual convention.

(defun sum-var-sets (&rest l)
  (reduce-var-set (apply #'append l))) ; consing up a storm aren't we?

(defun make-var-set (vars)
  (loop for v in vars collect (ncons v)))

(macrolet ((empty-free-lisp-vars (name)
             (let ((form (gensym)))
               `(defun-prop (,name free-lisp-vars) (,form)
                  (declare (ignore ,form))
                  '()))))
  (empty-free-lisp-vars declare)
  (empty-free-lisp-vars function)
  (empty-free-lisp-vars go)
  (empty-free-lisp-vars quote))

;;; (LAMBDA <BVL> . <BODY>)

(defun-prop (lambda free-lisp-vars) (form)
  (difference-var-sets
    ; get free lisp vars from body forms
    (free-lisp-vars-of-argl (cddr form))
    ; get vars bound by LAMBDA
    (make-var-set (cadr form))))

;;; (PROG <BVLSPEC> . <BODY>)

(defun-prop (prog free-lisp-vars) (form)
  (sum-var-sets
    ; get free lisp vars from init forms
    (union-var-set
      (mapcar (lambda (e) (when (consp e) (free-lisp-vars (cadr e))))
              (cadr form)))
    (difference-var-sets
      ; get free lisp vars from body forms
      (union-var-set (mapcar (lambda (e)
                               ; skip go tags
                               (if (go-tag-p e) '() (free-lisp-vars e)))
                             (cddr form)))
      ; get vars bound by PROG
      (make-var-set (mapcar (lambda (e) (if (consp e) (car e) e))
                            (cadr form))))))

;;; (LET <BVLSPEC> . <BODY>)

(defun-prop (let free-lisp-vars) (form)
  (sum-var-sets
    ; get free lisp vars from init forms
    (union-var-set
      (mapcar (lambda (e) (when (consp e) (free-lisp-vars (cadr e))))
              (cadr form)))
    (difference-var-sets
      ; get free lisp vars from body forms
      (free-lisp-vars-of-argl (cddr form))
      ; get vars bound by LET
      (make-var-set (mapcar (lambda (e) (if (atom e) e (car e)))
                            (cadr form))))))

;;; (DO (<VARSPEC> ...) (<END-TEST-FORM> . <RESULT-FORMS>) . <BODY>)

(defun-prop (do free-lisp-vars) (form)
  (sum-var-sets
    ; get free lisp vars from init forms
    (union-var-set (mapcar (lambda (e)
                             (when (consp e)
                               (free-lisp-vars (cadr e))))
                           (cadr form)))
    (difference-var-sets
      (sum-var-sets
        ; get free lisp vars from body forms
        (union-var-set (mapcar (lambda (e)
                                 ; skip go tags
                                 (if (go-tag-p e) '() (free-lisp-vars e)))
                               (cdddr form)))
        ; get free lisp vars from the end test form and result forms
        (free-lisp-vars-of-argl (caddr form))
        ; get free lisp vars from step forms
        (union-var-set (mapcar (lambda (e)
                                 (when (consp e)
                                   (free-lisp-vars (caddr e))))
                               (cadr form))))
      ; get vars bound by DO
      (make-var-set (mapcar (lambda (e) (if (atom e) e (car e)))
                            (cadr form))))))

;;; (COND (<I> ..) (<J> ..) ...)

(defun-prop (cond free-lisp-vars) (form)
  (union-var-set (mapcar #'free-lisp-vars-of-argl (cdr form))))

;;; (SETQ ... ODD AND EVENS...)

(defun-prop (setq free-lisp-vars) (form)
  (do ((free-vars nil (sum-var-sets `((,(car form) . t))
				    (free-lisp-vars (cadr form))
				    free-vars))
       (form (cdr form) (cddr form)))
      ((null form) free-vars)))

;;; uhm. LAMBDA, PROG, GO, DO, COND, QUOTE, SETQ.

(defun-prop (and free-lisp-vars)(form)(free-lisp-vars-of-argl (cdr form)))
(defun-prop (or free-lisp-vars)(form)(free-lisp-vars-of-argl (cdr form)))

;;; these next forms are generated by TRANSLATE.

(defprop $piece t sort-of-lexical)

(defun-prop (trd-msymeval free-lisp-vars) (form)
  (if (get (cadr form) 'sort-of-lexical)
      ;; acts like a lexical variable because of the $SUBSTPART translator.
      (list (list (cadr form)))
      ()))

(defun-prop (mfunction-call free-lisp-vars) (form)
  ;; it is not strictly known if the name of the function being called
  ;; is a variable or not. lets say its not.
  (free-lisp-vars-of-argl (cddr form)))

;;; (FUNGEN&ENV-FOR-MEVAL () () EXP)
(defun-prop (fungen&env-for-meval free-lisp-vars) (form)
  (free-lisp-vars (car (cdddr form))))

;;; the various augmented lambda forms.

(defun free-lisp-vars-m-tlambda (form)
  (difference-var-sets (free-lisp-vars-of-argl (cddr form))
		       (free-lisp-vars-of-argl (cadr form))))

(mapc #'(lambda (u) (putprop u 'free-lisp-vars-m-tlambda 'free-lisp-vars))
      '(m-tlambda m-tlambda&))

(defun free-lisp-vars-m-tlambda&env (form)
  (difference-var-sets (free-lisp-vars-of-argl (cddr form))
		       (free-lisp-vars-of-argl (car (cadr form)))))

(defprop m-tlambda&env free-lisp-vars-m-tlambda&env free-lisp-vars)
(defprop m-tlambda&env& free-lisp-vars-m-tlambda&env free-lisp-vars)

;;; Other entry points:

(defun tbound-free-vars (free-varl)
  ;; Takes a FREE-VAR list and returns a list of two lists.
  ;; the tbound free vars and the tbound free vars that are
  ;; side effected also.
  (do ((free nil)
       (free&s nil))
      ((null free-varl) (list free free&s))
    (let ((v (pop free-varl)))
      (cond ((and (tboundp (car v))
		  (not (tr-get-special (car v))))
	     (push (car v) free)
	     (cond ((cdr v)
		    (push (car v) free&s))))))))

(defun side-effect-free-check (varl form)
  (cond ((null varl) t)
	(t
	 (tr-format (intl:gettext "error: unsupported side effects on ~:M in expression ~M~%") `((mlist) ,@varl) form)
	 nil)))


;;; O.K. here is the translate property for LAMBDA.
;;; given catch and throw we don't know where a funarg lambda
;;; may end up.

;;; Cases:
;;; I. No side effects on free variables.
;;;    A. one funarg only, not reconsed. e.g.
;;;       F(N,L):=MAP(LAMBDA([U],Q(N,U)),L)$
;;;       (PROGN (SET-ENV <*LINK*> N)
;;;              (FUNCTION (LAMBDA (U) (LET ((N (GET-ENV *LINK*))) (f* U N)))))
;;;    B. need new instance of the environment each time,
;;;       F(N):=LAMBDA([U],N*U);
;;;       `(LAMBDA (U) (gen-func U 'N)) without extend loaded.
;;; II. side effects.
;;;    A. Those since effects need to be propogated to the environment
;;;       where the LAMBDA was made. This is difficult to do in the
;;;       present translator. e.g.
;;;       F(L):=BLOCK([SUM:0],FULLMAP(LAMBDA([U],SUM:SUM+U),L),SUM);
;;;       every function which guarantees the order of argument evalation
;;;       (MPROG and MPROGN), must translate and expression and get information
;;;       about environment propagation.
;;;       (PROGN (FULLMAP (PROGN (SET-ENV) '(LAMBDA ...)) L)
;;;              (GET-ENV)), uhm. this is pretty tricky anyway.
;;;    B. side effects only have to be maintained inside the LAMBDA.
;;;       this is easier, and if you have it, you really don't need II.A.
;;;       since you can always ask the LAMBDA for its environment by
;;;       calling it on the proper message {If the LAMBDA is written that way}.


;;; ((LAMBDA) ((MLIST) X Y ((MLIST Z))) . <BODY>)
;;; must also handle the &REST arguments. N.B. MAPPLY correctly handles
;;; the application of a lisp lambda form.


;;; Some forms know that the lambda is not going to
;;; be an upward funarg, that it is not possible (wanted)
;;; have two different lambda's generated from the same
;;; place. e.g. INTERPOLATE(SIN(X^2)=A,X,0,N) (implied lambda
;;; which is contructed by the translation property for
;;; interpolate. MAP(LAMBDA([U],...),L) is another example)
;;; these forms will be called I-LAMBDA's, and will be generated
;;; from LAMBDA's by the functions that want to. All this
;;; is meaningless in the present macsyma evaluator of course, since
;;; it uses dynamic binding and just hopes for the best.

(def%tr lambda (form)
  (gen-tr-lambda form))

;;; we keep a pointer to the original FORM so that we can
;;; generate messages with it if need be.

(defun gen-tr-lambda (form &aux arg-info frees t-form dup)
  (unless ($listp (cadr form))
    (tr-format (intl:gettext "error: first argument of lambda expression must be a list; found ~M") (cadr form))
    (tr-abort)
    (return-from gen-tr-lambda nil))
  (when (null (cddr form))
    (tr-format (intl:gettext "error: empty body in lambda expression.~%"))
    (tr-abort)
    (return-from gen-tr-lambda nil))
  (setq arg-info (mapcar #'(lambda (v)
			     (cond ((mdefparam v) nil)
				   ((and (op-equalp v 'mlist)
					 (mdefparam (cadr v))
					 (null (cddr v)))
				    t)
				   (t '*bad*)))
			 (cdr (cadr form))))
  (cond ((or (member '*bad* arg-info :test #'eq)
	     (and (member t arg-info :test #'eq)
		  (cdr (member t arg-info :test #'eq)))) ;;; the &REST is not the last one.
	 (tr-format (intl:gettext "error: unsupported argument list ~:M in lambda expression.~%") (cadr form))
	 (tr-abort)
	 nil)
	((setq dup (find-duplicate (cdadr form) :test #'eq :key #'mparam))
	 (tr-format (intl:gettext "error: ~M occurs more than once in lambda expression parameter list") (mparam dup))
	 (tr-abort)
	 nil)
	(t
	 (setq arg-info (member t arg-info :test #'eq) ;; &RESTP
	       t-form
	       (tr-lambda `((lambda)
			    ((mlist) ,@(mapcar #'(lambda (v)
						   (cond ((atom v) v)
							 (t (cadr v))))
					       (cdr (cadr form))))
			    ,@(cddr form)))
	       t-form (cdr t-form)
	       frees (tbound-free-vars (free-lisp-vars t-form)))))
  (cond ((null (car frees))
         (let ((tlambda (if arg-info 'm-tlambda& 'm-tlambda)))
           `($any . (,tlambda ,(cadr t-form) ,@(cddr t-form)))))
        ((null (cadr frees))
         (let* ((tlambda (if arg-info 'm-tlambda&env& 'm-tlambda&env))
                (fvl (car frees))
                (cfvl (intersection fvl *tr-free-vars-to-capture*)))
           `($any . (,tlambda (,(cadr t-form) ,fvl ,cfvl) ,@(cddr t-form)))))
        (t
         (warn-meval form)
         (side-effect-free-check (cadr frees) form)
         (punt-to-meval form))))
