;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module compar)

(load-macsyma-macros mrgmac)

(declare-top (special $float2bf $radexpand $ratprint $ratsimpexpons $listconstvars
		     success $props *x* $%enumer)
	    ;; Variables defined in DB
	    (special context current dobjects dbtrace +labs))

(defvar %initiallearnflag)

(defmvar $context '$initial
  "Whenever a user assumes a new fact, it is placed in the context
named as the current value of the variable CONTEXT.  Similarly, FORGET
references the current value of CONTEXT.  To add or DELETE a fact from a
different context, one must bind CONTEXT to the intended context and then
perform the desired additions or deletions.  The context specified by the
value of CONTEXT is automatically activated.  All of MACSYMA's built-in
relational knowledge is contained in the default context GLOBAL."
  no-reset)

(defmvar $contexts '((mlist) $initial $global)
  "A list of the currently active contexts."
  no-reset)

(defmvar $activecontexts '((mlist))
  "A list of the currently activated contexts"
  no-reset)

(defmvar sign-imag-errp t
  "If T errors out in case COMPAR meets up with an imaginary quantity.
	  If NIL THROWs in that case."
  no-reset)

(defmvar complexsign nil
  "If T, COMPAR attempts to work in a complex mode.
	  This scheme is only very partially developed at this time."
  no-reset)

(defmvar $prederror nil)
(defmvar $signbfloat t)
(defmvar $askexp)
(defmvar limitp)
(defmvar $assume_pos nil)
(defmvar $assume_pos_pred nil)

(defmvar factored nil)
(defmvar locals nil)
(defmvar sign nil)
(defmvar minus nil)
(defmvar odds nil)
(defmvar evens nil)
(defmvar lhs nil)
(defmvar rhs nil)

;; This variable is also initialized in DB for its own purposes.
;; COMPAR is loaded after DB.
(setq context '$global)

;; Load-time environment for COMPAR.  $CONTEXT and $CONTEXTS will be
;; reset at the end of the file via a call to ($newcontext '$initial).
(setq $context '$global $contexts '((mlist) $global))

(defmacro ask (&rest x)
 `(retrieve (list '(mtext) ,@x) nil))

(defmacro pow (&rest x)
  `(power ,@x))

(defun lmul (l)
  (simplify (cons '(mtimes) l)))

(defun conssize (x)
  (if (atom x)
      0
      (do ((x (cdr x) (cdr x))
	   (sz 1))
	  ((null x) sz)
	(incf sz (1+ (conssize (car x)))))))

;;;  Functions for creating, activating, manipulating, and killing contexts

;;; This "turns on" a context, making its facts visible.

(defmfun $activate n
  (do ((i 1 (1+ i)))
      ((> i n))
    (cond ((not (symbolp (arg i))) (nc-err))
	  ((member (arg i) (cdr $activecontexts) :test #'eq))
	  ((member (arg i) (cdr $contexts) :test #'eq)
	   (setq $activecontexts (mcons (arg i) $activecontexts))
	   (activate (arg i)))
	  (t (merror "There is no context with the name ~:M" (arg i)))))
  '$done)

;;; This "turns off" a context, keeping the facts, but making them
;;; invisible

(defmfun $deactivate n
  (do ((i 1 (1+ i))) ((> i n))
    (cond ((not (symbolp (arg i))) (nc-err))
	  ((member (arg i) (cdr $contexts) :test #'eq)
	   (setq $activecontexts ($delete (arg i) $activecontexts))
	   (deactivate (arg i)))
	  (t (merror "There is no context with the name ~:M" (arg i)))))
  '$done)

;;; This function of 0 or 1 argument prints out a list of the facts
;;; in the specified context.  No argument implies the current context.

(defmfun $facts n
  (cond ((= n 0) (facts1 $context))
	((= n 1) (facts1 (arg n)))
	(t (merror "`facts' takes zero or one argument only."))))

(defun facts1 (con)
  (contextmark)
  (do ((l (zl-get con 'data) (cdr l)) (nl) (u))
      ((null l) (cons '(mlist) nl))
    (when (visiblep (car l))
      (setq u (intext (caaar l) (cdaar l)))
      (unless (memalike u nl)
	(setq nl (cons u nl))))))

(defun intext (rel body)
  (setq body (mapcar #'doutern body))
  (cond ((eq 'kind rel) (cons '($kind) body))
	((eq 'par rel) (cons '($par) body))
	((eq 'mgrp rel) (cons '(mgreaterp) body))
	((eq 'mgqp rel) (cons '(mgeqp) body))
	((eq 'meqp rel) (cons '($equal) body))
	((eq 'mnqp rel) (list '(mnot) (cons '($equal) body)))))

(defprop $context asscontext assign)

;;; This function switches contexts, creating one if necessary.

(defun asscontext (xx y)
  (declare (ignore xx))
  (cond ((not (symbolp y)) (nc-err))
	((member y $contexts :test #'eq) (setq context y $context y))
	(t ($newcontext y))))

;;; This function actually creates a context whose subcontext is $GLOBAL.
;;; It also switches contexts to the newly created one.

(defmfun $newcontext (x)
  (cond ((not (symbolp x)) (nc-err))
	((member x $contexts :test #'eq)
	 (mtell "Context ~M already exists." x) nil)
	(t (setq $contexts (mcons x $contexts))
	   (putprop x '($global) 'subc)
	   (setq context x $context x))))

;;; This function creates a supercontext.  If given one argument, it
;;; makes the current context be the subcontext of the argument.  If
;;; given more than one argument, the first is assumed the name of the
;;; supercontext and the rest are the subcontexts.

(defmspec $supcontext (x)
  (setq x (cdr x))
  (cond ((null x) (merror "You must supply a name for the context."))
	((caddr x) (merror "`supcontext' takes either one or two arguments."))
	((not (symbolp (car x))) (nc-err))
	((member (car x) $contexts :test #'eq)
	 (merror "Context ~M already exists." (car x)))
	((and (cadr x) (not (member (cadr x) $contexts :test #'eq)))
	 (merror "Nonexistent context ~M." (cadr x)))
	(t (setq $contexts (mcons (car x) $contexts))
	   (putprop (car x) (ncons (or (cadr x) $context)) 'subc)
	   (setq context (car x) $context (car x)))))

;;; This function kills a context or a list of contexts

(defmfun $killcontext n
  (do ((i 1 (1+ i)))
      ((> i n))
    (if (symbolp (arg i))
	(killcontext (arg i))
	(nc-err)))
  (if (and (= n 1) (eq (arg 1) '$global))
      '$not_done
      '$done))

(defun killallcontexts ()
  (mapcar #'killcontext (cdr $contexts))
  (setq $context '$initial context '$initial current '$initial
	$contexts '((mlist) $initial $global) dobjects ())
  ;;The DB variables
  ;;conmark, conunmrk, conindex, connumber, and contexts
  ;;concern garbage-collectible contexts, and so we're
  ;;better off not resetting them.
  (defprop $global 1 cmark) (defprop $initial 1 cmark)
  (defprop $initial ($global) subc))

(defun killcontext (x)
  (cond ((not (member x $contexts :test #'eq))
	 (mtell "The context ~M doesn't exist." x))
	((eq x '$global) '$global)
	((eq x '$initial)
	 (mapc #'remov (zl-get '$initial 'data))
	 (remprop '$initial 'data)
	 '$initial)
	((and (not (eq $context x)) (contextmark) (< 0 (zl-get x 'cmark)))
	 (mtell "The context ~M is currently active." x))
	(t (setq $contexts ($delete x $contexts))
	   (cond ((and (eq x $context)
		       (equal ;;replace eq ?? wfs
			(zl-get x 'subc) '($global)))
		  (setq $context '$initial)
		  (setq context '$initial))
		 ((eq x $context)
		  (setq $context (car (zl-get x 'subc)))
		  (setq context (car (zl-get x 'subc)))))
	   (killc x)
	   x)))

(defun nc-err ()
  (merror "Contexts must be symbolic atoms."))

;; Simplification and evaluation of boolean expressions
;;
;; Simplification of boolean expressions:
;;
;; and and or are declared nary. The sole effect of this is to allow Maxima to
;; flatten nested expressions, e.g., a and (b and c) => a and b and c
;; (The nary declaration does not make and and or commutative, and and and or
;; are not otherwise declared commutative.)
;;
;; and: if any argument simplifies to false, return false
;;  otherwise omit arguments which simplify to true and simplify others
;;  if only one argument remains, return it
;;  if none remain, return true
;;
;; or: if any argument simplifies to true, return true
;;  otherwise omit arguments which simplify to false and simplify others
;;  if only one argument remains, return it
;;  if none remain, return false
;;
;; not: if argument simplifies to true / false, return false / true
;;  otherwise reverse sense of comparisons (if argument is a comparison)
;;  otherwise return not <simplified argument>
;;
;; Evaluation (MEVAL) of boolean expressions:
;; same as simplification except evaluating (MEVALP) arguments instead of simplifying
;; When prederror = true, complain if expression evaluates to something other than T / NIL
;; (otherwise return unevaluated boolean expression)
;;
;; Evaluation (MEVALP) of boolean expressions:
;; same as simplification except evaluating (MEVALP) arguments instead of simplifying
;; When prederror = true, complain if expression evaluates to something other than T / NIL
;; (otherwise return unevaluated boolean expression)
;;
;; Simplification of "is" expressions:
;; if argument simplifies to true/false, return true/false
;; otherwise return is (<simplified argument>)
;;
;; Evaluation of "is" expressions:
;; if argument evaluates to true/false, return true/false
;; otherwise return unknown if prederror = false, else trigger an error
;;
;; Simplification of "maybe" expressions:
;; if argument simplifies to true/false, return true/false
;; otherwise return maybe (<simplified expression>)
;;
;; Evaluation of "maybe" expressions:
;; if argument evaluates to true/false, return true/false
;; otherwise return unknown

(defprop $is simp-$is operators)
(defprop %is simp-$is operators)
(defprop $maybe simp-$is operators)
(defprop %maybe simp-$is operators)

; I'VE ASSUMED (NULL Z) => SIMPLIFIY ARGUMENTS
; SAME WITH SIMPCHECK (SRC/SIMP.LISP)
; SAME WITH TELLSIMP-GENERATED SIMPLIFICATION FUNCTIONS
; SAME WITH SIMPLIFICATION OF %SIN
; PRETTY SURE I'VE SEEN OTHER EXAMPLES AS WELL
; Z SEEMS TO SIGNIFY "ARE THE ARGUMENTS SIMPLIFIED YET"

(defun maybe-simplifya (x z) (if z x (simplifya x z)))

(defun maybe-simplifya-protected (x z)
  (let ((errcatch t) ($errormsg nil))
    (declare (special errcatch $errormsg))
    (ignore-errors (maybe-simplifya x z) x)))

(defun simp-$is (x y z)
  (declare (ignore y))
  (let ((a (maybe-simplifya (cadr x) z)))
    (if (or (eq a t) (eq a nil))
      a
      `((,(caar x) simp) ,a))))

(defmfun $is (pat)
  (let*
    ((x (mevalp1 pat))
     (ans (car x))
     (patevalled (cadr x)))
    (cond
      ((member ans '(t nil) :test #'eq) ans)
      ; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
      ($prederror (pre-err patevalled))
      (t '$unknown))))

(defmfun $maybe (pat)
  (let*
    ((x (let (($prederror nil)) (mevalp1 pat)))
     (ans (car x))
     (patevalled (cadr x)))
    (cond
      ((member ans '(t nil) :test #'eq) ans)
      (t '$unknown))))

(defmfun is (pred)
  (let (($prederror t))
    (mevalp pred)))

; The presence of OPERS tells SIMPLIFYA to call OPER-APPLY,
; which calls NARY1 to flatten nested "and" and "or" expressions
; (due to $NARY property of MAND and MOR, declared elsewhere).

(put 'mand t 'opers)
(put 'mor t 'opers)

(putprop 'mnot 'simp-mnot 'operators)
(putprop 'mand 'simp-mand 'operators)
(putprop 'mor 'simp-mor 'operators)

(defun simp-mand (x y z)
  (declare (ignore y))
  (do ((l (cdr x) (cdr l)) (a) (simplified))
    ((null l)
    (cond
      ((= (length simplified) 0) t)
      ((= (length simplified) 1) (car simplified))
      (t (cons '(mand simp) (reverse simplified)))))
  (setq a (maybe-simplifya (car l) z))
  (cond
    ((null a) (return nil))
    ((eq a '$unknown) (if (not (member '$unknown simplified :test #'eq)) (push a simplified)))
    ((not (member a '(t nil) :test #'eq)) (push a simplified)))))

(defun simp-mor (x y z)
  (declare (ignore y))
  (do ((l (cdr x) (cdr l)) (a) (simplified))
    ((null l)
    (cond
      ((= (length simplified) 0) nil)
      ((= (length simplified) 1) (car simplified))
      (t (cons '(mor simp) (reverse simplified)))))
  (setq a (maybe-simplifya (car l) z))
  (cond
    ((eq a t) (return t))
    ((eq a '$unknown) (if (not (member '$unknown simplified :test #'eq)) (push a simplified)))
    ((not (member a '(t nil) :test #'eq)) (push a simplified)))))

; ALSO CUT STUFF ABOUT NOT EQUAL => NOTEQUAL AT TOP OF ASSUME

(defun simp-mnot (x y z)
  (declare (ignore y))
  (let ((arg (maybe-simplifya (cadr x) z)))
    (if (atom arg)
      (cond
        ((or (eq arg t) (eq arg '$true))
         nil)
        ((or (eq arg nil) (eq arg '$false))
         t)
        ((eq arg '$unknown)
         '$unknown)
        (t `((mnot simp) ,arg)))
      (let ((arg-op (caar arg)) (arg-arg (cdr arg)))
        (setq arg-arg (mapcar #'(lambda (a) (maybe-simplifya a z)) arg-arg))
        (cond
          ((eq arg-op 'mlessp)
           `((mgeqp simp) ,@arg-arg))
          ((eq arg-op 'mleqp)
           `((mgreaterp simp) ,@arg-arg))
          ((eq arg-op 'mequal)
           `((mnotequal simp) ,@arg-arg))
          ((eq arg-op '$equal)
           `(($notequal simp) ,@arg-arg))
          ((eq arg-op 'mnotequal)
           `((mequal simp) ,@arg-arg))
          ((eq arg-op '$notequal)
           `(($equal simp) ,@arg-arg))
          ((eq arg-op 'mgeqp)
           `((mlessp simp) ,@arg-arg))
          ((eq arg-op 'mgreaterp)
           `((mleqp simp) ,@arg-arg))
          ((eq arg-op 'mnot)
           (car arg-arg))

          ; Distribute negation over conjunction and disjunction;
          ; analogous to '(- (a + b)) --> - a - b.

          ((eq arg-op 'mand)
           (let ((L (mapcar #'(lambda (e) `((mnot) ,e)) arg-arg)))
             (simplifya `((mor) ,@L) nil)))

          ((eq arg-op 'mor)
           (let ((L (mapcar #'(lambda (e) `((mnot) ,e)) arg-arg)))
             (simplifya `((mand) ,@L) nil)))

          (t `((mnot simp) ,arg)))))))

;; =>* N.B. *<=
;; The function IS-BOOLE-CHECK, used by the translator, depends
;; on some stuff in here.  Check it out in the transl module
;; ACALL before proceeding.

(defmfun mevalp (pat)
  (let*
    ((x (mevalp1 pat))
     (ans (car x))
     (patevalled (cadr x)))
    (cond ((member ans '(#.(not ()) ()) :test #'eq)
       ans)
      ; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
      ($prederror (pre-err patevalled))
      (t (or patevalled ans)))))

(defun mevalp1 (pat)
  (let (patevalled ans)
    (setq ans 
      (cond ((and (not (atom pat)) (member (caar pat) '(mnot mand mor) :test #'eq))
	   (cond ((eq 'mnot (caar pat)) (is-mnot (cadr pat)))
	         ((eq 'mand (caar pat)) (is-mand (cdr pat)))
	         (t (is-mor (cdr pat)))))
	  ((atom (setq patevalled (meval pat))) patevalled)
	  ((member (caar patevalled) '(mnot mand mor) :test #'eq) (mevalp1 patevalled))
	  (t (mevalp2 patevalled (caar patevalled) (cadr patevalled) (caddr patevalled)))))
    (list ans patevalled)))

(defmfun mevalp2 (patevalled pred arg1 arg2)
  (cond ((eq 'mequal pred) (like arg1 arg2))
	((eq '$equal pred) (meqp arg1 arg2))
	((eq 'mnotequal pred) (not (like arg1 arg2)))
	((eq '$notequal pred) (mnqp arg1 arg2))
	((eq 'mgreaterp pred) (mgrp arg1 arg2))
	((eq 'mlessp pred) (mgrp arg2 arg1))
	((eq 'mgeqp pred) (mgqp arg1 arg2))
	((eq 'mleqp pred) (mgqp arg2 arg1))
	(t (isp (munformat patevalled)))))

(defmfun pre-err (pat)
  (merror "Maxima was unable to evaluate the predicate:~%~M" pat))

(defun is-mnot (pred)
  (setq pred (mevalp pred))
  (cond ((eq t pred) nil)
	((not pred))
	(t (pred-reverse pred))))

(defmfun pred-reverse (pred)
  (cond ((atom pred) (list '(mnot) pred))
	((eq 'mnot (caar pred)) (cadr pred))
	((eq 'mgreaterp (caar pred)) (cons '(mleqp) (cdr pred)))
	((eq 'mgeqp (caar pred)) (cons '(mlessp) (cdr pred)))
	((eq 'mequal (caar pred)) (cons '(mnotequal) (cdr pred)))
	((eq '$equal (caar pred)) (cons '($notequal) (cdr pred)))
	((eq '$notequal (caar pred)) (cons '($equal) (cdr pred)))
	((eq 'mnotequal (caar pred)) (cons '(mequal) (cdr pred)))
	((eq 'mleqp (caar pred)) (cons '(mgreaterp) (cdr pred)))
	((eq 'mlessp (caar pred)) (cons '(mgeqp) (cdr pred)))
	(t (list '(mnot) pred))))

(defun is-mand (pl)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl))
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mand) (nreverse npl)))))
    (setq dummy (mevalp (car pl)) pl (cdr pl))
    (cond ((eq t dummy))
	  ((null dummy) (return nil))
	  (t (setq npl (cons dummy npl))))))

(defun is-mor (pl)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl) nil)
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mor) (nreverse npl)))))
    (setq dummy (mevalp (car pl)) pl (cdr pl))
    (cond ((eq t dummy) (return t))
	  ((null dummy))
	  (t (setq npl (cons dummy npl))))))

(defmspec $assume (x)
  (setq x (cdr x))
  (do ((nl)) ((null x) (cons '(mlist) (nreverse nl)))
    (cond ((atom (car x)) (setq nl (cons (assume (meval (car x))) nl)))
	  ((eq 'mand (caaar x))
	   (mapc #'(lambda (l) (setq nl (cons (assume (meval l)) nl)))
		 (cdar x)))
	  ((eq 'mnot (caaar x))
	   (setq nl (cons (assume (meval (pred-reverse (cadar x)))) nl)))
	  ((eq 'mor (caaar x))
	   (merror "`assume': Maxima is unable to handle assertions involving `or'."))
	  ((eq (caaar x) 'mequal)
	   (merror "`assume': `=' means syntactic equality in Maxima. Maybe you want to use `equal'."))
	  ((eq (caaar x) 'mnotequal)
	   (merror "`assume': `#' means syntactic nonequality in Maxima. Maybe you want to use `not equal'."))
	  (t (setq nl (cons (assume (meval (car x))) nl))))
    (setq x (cdr x))))

(defmfun assume (pat)
  (if (and (not (atom pat))
	   (eq (caar pat) 'mnot)
	   (eq (caaadr pat) '$equal))
      (setq pat `(($notequal) ,@(cdadr pat))))
  (let ((dummy (let ($assume_pos) (car (mevalp1 pat)))))
    (cond ((eq dummy t) '$redundant)
	  ((null dummy) '$inconsistent)
	  ((atom dummy) '$meaningless)
	  (t (learn pat t)))))

(defmfun learn (pat flag)
  (cond ((atom pat))
	((zl-get (caar pat) (if flag 'learn 'unlearn))
	 (funcall (zl-get (caar pat) (if flag 'learn 'unlearn)) pat))
	((eq (caar pat) 'mgreaterp) (daddgr flag (sub (cadr pat) (caddr pat))))
	((eq (caar pat) 'mgeqp) (daddgq flag (sub (cadr pat) (caddr pat))))
	((member (caar pat) '(mequal $equal) :test #'eq)
	 (daddeq flag (sub (cadr pat) (caddr pat))))
	((member (caar pat) '(mnotequal $notequal) :test #'eq)
	 (daddnq flag (sub (cadr pat) (caddr pat))))
	((eq (caar pat) 'mleqp) (daddgq flag (sub (caddr pat) (cadr pat))))
	((eq (caar pat) 'mlessp) (daddgr flag (sub (caddr pat) (cadr pat))))
	(flag (true* (munformat pat)))
	(t (untrue (munformat pat)))))

(defmacro def-learn (nname pat flag)
  (declare (ignore nname))
  `(learn ,pat ,flag))

(defmspec $forget (x)
  (setq x (cdr x))
  (do ((nl))
      ((null x) (cons '(mlist) (nreverse nl)))
    (cond ((atom (car x)) (setq nl (cons (forget (meval (car x))) nl)))
	  ((eq 'mand (caaar x))
	   (mapc #'(lambda (l) (setq nl (cons (forget (meval l)) nl))) (cdar x)))
	  ((eq 'mnot (caaar x))
	   (setq nl (cons (forget (meval (pred-reverse (cadar x)))) nl)))
	  ((eq 'mor (caaar x))
	   (merror "Maxima is unable to handle assertions involving `or'."))
	  (t (setq nl (cons (forget (meval (car x))) nl))))
    (setq x (cdr x))))

(defmfun forget (pat)
  (cond (($listp pat)
	 (cons '(mlist simp) (mapcar #'forget1 (cdr pat))))
	(t (forget1 pat))))

(defun forget1 (pat)
  (cond ((and (not (atom pat))
	      (eq (caar pat) 'mnot)
	      (eq (caaadr pat) '$equal))
	 (setq pat `(($notequal) ,@(cdadr pat)))))
  (learn pat nil))

(defmfun restore-facts (factl)		; used by SAVE
  (dolist (fact factl)
    (cond ((eq (caar fact) '$kind)
	   (declarekind (cadr fact) (caddr fact))
	   (add2lnc (getop (cadr fact)) $props))
	  ((eq (caar fact) '$par))
	  (t (assume fact)))))


(defmacro compare (a b)
  `(sign1 (sub* ,a ,b)))

(defmfun maximum (l)
  (maximin l '$max))

(defmfun minimum (l)
  (maximin l '$min))

(defmspec mand (form) (setq form (cdr form))
  (do ((l form (cdr l)) (x) (unevaluated))
    ((null l)
    (cond
      ((= (length unevaluated) 0) t)
      ((= (length unevaluated) 1) (car unevaluated))
      (t (cons '(mand) (reverse unevaluated)))))
  (setq x (mevalp (car l)))
  (cond
    ((null x) (return nil))
    ((not (member x '(t nil) :test #'eq)) (push x unevaluated)))))

(defmspec mor (form) (setq form (cdr form))
  (do ((l form (cdr l)) (x) (unevaluated))
  ((null l)
    (cond
      ((= (length unevaluated) 0) nil)
      ((= (length unevaluated) 1) (car unevaluated))
      (t (cons '(mor) (reverse unevaluated)))))
  (setq x (mevalp (car l)))
  (cond
    ((eq x t) (return t))
    ((not (member x '(t nil) :test #'eq)) (push x unevaluated)))))

(defmspec mnot (form) (setq form (cdr form))
  (let ((x (mevalp (car form))))
    (cond
      ((member x '(t nil) :test #'eq) (not x))
      (t `((mnot) ,x)))))

;;;Toplevel functions- $ASKSIGN, $SIGN.
;;;Switches- LIMITP If TRUE $ASKSIGN and $SIGN will look for special
;;;		     symbols such as EPSILON, $INF, $MINF and attempt
;;;		     to do the correct thing. In addition calls to
;;;		     $REALPART and $IMAGPART are made to assure that
;;;		     the expression is real.
;;;
;;;		  if NIL $ASKSIGN and $SIGN assume the expression
;;;		     given is real unless it contains an $%I, in which
;;;		     case they call $RECTFORM.

(setq limitp nil)

(defmfun $asksign (exp)
  (let (sign minus odds evens factored)
    (asksign01 (cond (limitp (restorelim exp))
		     ((among '$%i exp) ($rectform exp))
		     (t exp)))))

(defmfun asksign-p-or-n (e)
  (unwind-protect (prog2
		      (assume `(($notequal) ,e 0))
		      ($asksign e))
    (forget `(($notequal) ,e 0))))

(defun asksign01 (a)
  (let ((e (sign-prep a)))
    (cond ((eq e '$pnz) '$pnz)
	  ((member (setq e (asksign1 e)) '($pos $neg) :test #'eq) e)
	  (limitp (eps-sign a))
	  (t '$zero))))

;; csign returns t if x appears to be complex.
;; Else, it returns the sign.
(defmfun csign (x) 
  (or (not (free x '$%i))
      (let (sign-imag-errp limitp) (catch 'sign-imag-err ($sign x)))))

(defmfun $sign (x)
  (let ((x (specrepcheck x))
	sign minus odds evens factored)
    (sign01 (cond (limitp (restorelim x))
		  ((not (free x '$%i)) ($rectform x))
		  (t x)))))

(defun sign01 (a)
  (let ((e (sign-prep a)))
    (cond ((eq e '$pnz) '$pnz)
	  (t (setq e (sign1 e))
	     (if (and limitp (eq e '$zero)) (eps-sign a) e)))))

;;; Preparation for asking questions from DEFINT or LIMIT.
(defun sign-prep (x)
  (if limitp
      (destructuring-let (((rpart . ipart) (trisplit x)))
	(cond ((and (equal (sratsimp ipart) 0)
		    (free rpart '$infinity))
	       (setq x (nmr (sratsimp rpart)))
	       (if (free x 'prin-inf)
		   x
		   ($limit x 'prin-inf '$inf '$minus)))
	      (t '$pnz)))	       ; Confess ignorance if COMPLEX.
      x))

;;; Do substitutions for special symbols.
(defmfun nmr (a)
  (if (not (free a '$zeroa)) (setq a ($limit a '$zeroa 0 '$plus)))
  (if (not (free a '$zerob)) (setq a ($limit a '$zerob 0 '$minus)))
  (if (not (free a 'z**)) (setq a ($limit a 'z** 0 '$plus)))
  (if (not (free a '*z*)) (setq a ($limit a '*z* 0 '$plus)))
  (if (not (free a 'epsilon)) (setq a ($limit a 'epsilon 0 '$plus)))
  a)  ;;; Give A back.

;;; Get the sign of EPSILON-like terms.  Could be made MUCH hairier.
(defun eps-sign (b)
  (let (temp1 temp2 temp3 free1 free2 free3)
    (cond ((not (free b '$zeroa))
	   (setq temp1 (eps-coef-sign b '$zeroa)))
	  (t (setq free1 t)))
    (cond ((not (free b '$zerob))
	   (setq temp2 (eps-coef-sign b '$zerob)))
	  (t (setq free2 t)))
    (cond ((not (free b 'epsilon))
	   (setq temp3 (eps-coef-sign b 'epsilon)))
	  (t (setq free3 t)))
    (cond ((and free1 free2 free3) '$zero)
	  ((or (not (null temp1)) (not (null temp2)) (not (null temp3)))
	   (cond ((and (null temp1) (null temp2)) temp3)
		 ((and (null temp2) (null temp3)) temp1)
		 ((and (null temp1) (null temp3)) temp2)
		 (t (merror
		     "~%`asksign': Internal error. See Maintainers.")))))))

(defun eps-coef-sign (exp epskind)
  (let ((eps-power ($lopow exp epskind)) eps-coef)
    (cond ((and (not (equal eps-power 0))
		(not (equal (setq eps-coef (ratcoeff exp epskind eps-power))
			    0))
		(eq (ask-integer eps-power '$integer) '$yes))
	   (cond ((eq (ask-integer eps-power '$even) '$yes)
		  ($asksign eps-coef))
		 ((eq (ask-integer eps-power '$odd) '$yes)
		  (setq eps-coef ($asksign eps-coef))
		  (cond ((or (and (eq eps-coef '$pos)
				  (or (eq epskind 'epsilon)
				      (eq epskind '$zeroa)))
			     (and (eq eps-coef '$neg)
				  (or (alike epskind (mul2* -1 'epsilon))
				      (eq epskind '$zerob))))
			 '$pos)
			(t '$neg)))
		 (t (merror "~%`asksign' or `sign': Insufficient information.~%"))))
	  (t (let ((deriv (sdiff exp epskind)) deriv-sign)
	       (cond ((not (eq (setq deriv-sign ($asksign deriv)) '$zero))
		      (total-sign epskind deriv-sign))
		     ((not
		       (eq (let ((deriv (sdiff deriv epskind)))
			     (setq deriv-sign ($asksign deriv)))
			   '$zero))
		      deriv-sign)
		     (t (merror "~%`asksign' or `sign': Insufficient data.~%"))))))))

;;; The above code does a partial Taylor series analysis of something
;;; that isn't a polynomial.

(defun total-sign (epskind factor-sign)
  (cond ((or (eq epskind '$zeroa) (eq epskind 'epsilon))
	 (cond ((eq factor-sign '$pos) '$pos)
	       ((eq factor-sign '$neg) '$neg)
	       ((eq factor-sign '$zero) '$zero)))
	((eq epskind '$zerob)
	 (cond ((eq factor-sign '$pos) '$neg)
	       ((eq factor-sign '$neg) '$pos)
	       ((eq factor-sign '$zero) '$zero)))))

(defun asksign (x)
  (setq x ($asksign x))
  (cond ((eq '$pos x) '$positive)
	((eq '$neg x) '$negative)
	((eq '$pnz x) '$pnz)	 ;COMPLEX expression encountered here.
	(t '$zero)))

(defun asksign1 ($askexp)
  (let ($radexpand) (sign1 $askexp))
  (cond ((member sign '($pos $neg $zero) :test #'eq) sign)
	((null odds)
	 (setq $askexp (lmul evens)
	       sign (cdr (assol $askexp locals)))
	 (do ()
	     (nil)
	   (cond ((member sign '($zero |$Z| |$z| 0 0.0) :test #'equal)
		  (tdzero $askexp) (setq sign '$zero) (return t))
		 ((member sign '($pn $nonzero |$N| |$n| $nz $nonz $non0) :test #'eq)
		  (tdpn $askexp) (setq sign '$pos) (return t))
		 ((member sign '($pos |$P| |$p| $positive) :test #'eq)
		  (tdpos $askexp) (setq sign '$pos) (return t))
		 ((member sign '($neg |$N| |$n| $negative) :test #'eq)
		  (tdneg $askexp) (setq sign '$pos) (return t)))
	   (setq sign (ask "Is  " $askexp "  zero or nonzero?")))
	 (if minus (flip sign) sign))
	(t (if minus (setq sign (flip sign)))
	   (setq $askexp (lmul (nconc odds (mapcar #'(lambda (l) (pow l 2)) evens))))
	   (do ((dom (cond ((eq '$pz sign) "  positive or zero?")
			   ((eq '$nz sign) "  negative or zero?")
			   ((eq '$pn sign) "  positive or negative?")
			   (t "  positive, negative, or zero?")))
		(ans (cdr (assol $askexp locals))))
	       (nil)
	     (cond ((and (member ans '($pos |$P| |$p| $positive) :test #'eq)
			 (member sign '($pz $pn $pnz) :test #'eq))
		    (tdpos $askexp) (setq sign '$pos) (return t))
		   ((and (member ans '($neg |$N| |$n| $negative) :test #'eq)
			 (member sign '($nz $pn $pnz) :test #'eq))
		    (tdneg $askexp) (setq sign '$neg) (return t))
		   ((and (member ans '($zero |$Z| |$z| 0 0.0) :test #'equal)
			 (member sign '($pz $nz $pnz) :test #'eq))
		    (tdzero $askexp) (setq sign '$zero) (return t)))
	     (setq ans (ask "Is  " $askexp dom)))
	   (if minus (flip sign) sign))))

(defun clearsign ()
  (do ()
      ((null locals))
    (cond ((eq '$pos (cdar locals)) (daddgr nil (caar locals)))
	  ((eq '$neg (cdar locals)) (daddgr nil (neg (caar locals))))
	  ((eq '$zero (cdar locals)) (daddeq nil (caar locals)))
	  ((eq '$pn (cdar locals)) (daddnq nil (caar locals)))
	  ((eq '$pz (cdar locals)) (daddgq nil (caar locals)))
	  ((eq '$nz (cdar locals)) (daddgq nil (neg (caar locals)))))
    (setq locals (cdr locals))))

(defmfun like (x y)
  (alike1 (specrepcheck x) (specrepcheck y)))

(setf (get '$und 'sysconst) t)
(setf (get '$ind 'sysconst) t)
(setf (get '$zeroa 'sysconst) t)
(setf (get '$zerob 'sysconst) t)

;; There have been some conversations about NaN on the list, but
;; the issue hasn't been settled.

(defvar indefinites `($und $ind))

;; Other than sums, products, and lambda forms, meqp knows nothing
;; about dummy variables. Because of the way niceindices chooses names
;; for the sum indicies, it's necessary to locally assign a new value to
;; niceindicespref.

(defun meqp-by-csign (z a b)
  (let ((sgn) ($niceindicespref `((mlist) ,(gensym) ,(gensym) ,(gensym))))
    (setq z ($niceindices z))
    (setq sgn (csign z))
    (cond ((eq '$zero sgn) t)
	  ((eq sgn t)
	   (setq z ($rectform z))
	   (if (or (eq nil (meqp ($realpart z) 0)) (eq nil (meqp ($imagpart z) 0)))
	       nil
	       `(($equal) ,a ,b)))
	  ((member sgn '($pos $neg $pn)) nil)
	  (t `(($equal) ,a ,b)))))

;; For each fact of the form equal(a,b) in the active context, do e : ratsubst(b,a,e).

(defun equal-facts-simp (e)
  (let ((f (margs ($facts))))
    (dolist (fi f e)
      (if (op-equalp fi '$equal)
	  (setq e ($ratsubst (nth 2 fi) (nth 1 fi) e))))))

(defun meqp (a b)
  (let ((z))
    (setq a (specrepcheck a))
    (setq b (specrepcheck b))
    (cond ((or (like a b)) (not (member a indefinites)))

	  ((or (member a indefinites) (member b indefinites)
	       (member a infinities) (member b infinities)) nil)

	  ((and (symbolp a) (or (eq t a) (eq nil a) (get a 'sysconst))
		(symbolp b) (or (eq t b) (eq nil b) (get b 'sysconst))) nil)

	  ((or (mbagp a) (mrelationp a) (mbagp b) (mrelationp b))
	   (cond ((and (or (and (mbagp a) (mbagp b)) (and (mrelationp a) (mrelationp b)))
		       (eq (mop a) (mop b)) (= (length (margs a)) (length (margs b))))
		  (setq z (list-meqp (margs a) (margs b)))
		  (if (or (eq z t) (eq z nil)) z `(($equal) ,a ,b)))
		 (t nil)))

	  ((and (op-equalp a 'lambda) (op-equalp b 'lambda)) (lambda-meqp a b))
	  (($setp a) (set-meqp a b))
	  (t (meqp-by-csign (equal-facts-simp ($ratsimp (sub a b))) a b)))))

(defun list-meqp (p q)
  (let ((z))
    (cond ((or (null p) (null q)) (and (null p) (null q)))
	  (t
	   (setq z (meqp (car p) (car q)))
	   (cond ((eq z nil) nil)
		 ((or (eq z '$unknown) (op-equalp z '$equal)) z)
		 (t (list-meqp (cdr p) (cdr q))))))))

(defun lambda-meqp (a b)
  (let ((z))
    (cond ((= (length (second a)) (length (second b)))
	   (let ((x) (n ($length (second a))))
	     (dotimes (i n (push '(mlist) x)) (push (gensym) x))
	     (setq z (meqp (mfuncall '$apply a x) (mfuncall '$apply b x)))
	     (if (or (eq t z) (eq nil z)) z `(($equal) ,a ,b))))
	  (t nil))))

(defun set-meqp (a b)
  (let ((aa) (bb))
    (setq aa (equal-facts-simp a))
    (setq bb (equal-facts-simp b))

    (cond ((or (not ($setp bb))
	       (and ($emptyp aa) (not ($emptyp bb)))
	       (and ($emptyp bb) (not ($emptyp aa))))
	   nil)

	  ((and (= (length aa) (length bb))
		(every #'(lambda (p q) (eq t (meqp p q))) (margs aa) (margs bb))) t)

	  ((set-not-eqp (margs aa) (margs bb)) nil)

	  (t `(($equal ,a ,b))))))

(defun set-not-eqp (a b)
  (catch 'done
    (dolist (ak a)
      (if (every #'(lambda (s) (eq nil (meqp ak s))) b) (throw 'done t)))
    (dolist (bk b)
      (if (every #'(lambda (s) (eq nil (meqp bk s))) a) (throw 'done t)))
    (throw 'done nil)))

(defun mgrp (a b)
  (setq a (sub a b))
  (let ((sgn (csign a)))
    (cond ((eq sgn '$pos) t)
	  ((eq sgn t) nil) ;; csign thinks a - b isn't real
	  ((memq sgn '($neg $zero $nz)) nil)
	  (t `((mgreaterp) ,a 0)))))

(defun mlsp (x y)
  (mgrp y x))

(defun mgqp (a b)
  (setq a (sub a b))
  (let ((sgn (csign a)))
    (cond ((memq sgn '($pos $zero $pz)) t)
	  ((eq sgn t) nil) ;; csign thinks a - b isn't real 
	  ((eq sgn '$neg) nil)
	  (t `((mgeqp) ,a 0)))))

(defun mnqp (x y)
  (let ((b (meqp x y)))
    (cond ((eq b '$unknown) b)
	  ((or (eq b t) (eq b nil)) (not b))
	  (t `(($notequal) ,x ,y)))))

(defun c-$pn (o e)
  (list '(mnot) (c-$zero o e)))

(defun c-$zero (o e)
  (list '($equal) (lmul (nconc o e)) 0))

(defun c-$pos (o e)
  (cond ((null o) (list '(mnot) (list '($equal) (lmul e) 0)))
	((null e) (list '(mgreaterp) (lmul o) 0))
	(t (setq e (mapcar #'(lambda (l) (pow l 2)) e))
	   (list '(mgreaterp) (lmul (nconc o e)) 0))))

(defun c-$pz (o e)
  (cond ((null o) (list '(mnot) (list '($equal) (lmul e) 0)))
	((null e) (list '(mgeqp) (lmul o) 0))
	(t (setq e (mapcar #'(lambda (l) (pow l 2)) e))
	   (list '(mgeqp) (lmul (nconc o e)) 0))))

(defun sign* (x)
  (let (sign minus odds evens)
    (sign1 x)))

(defmfun infsimp* (e)
  (if (or (atom e) (and (free e '$inf) (free e '$minf)))
      e
      (infsimp e)))

(defun sign1 (x)
  (setq x (specrepcheck x))
  (setq x (infsimp* x))
  (if (member x '($und $ind $infinity) :test #'eq)
      (if limitp '$pnz (merror "The sign of ~:M is undefined" x)))
  (prog (dum exp)
     (setq dum (constp x) exp x)
     (cond ((or (numberp x) (ratnump x)))
	   ((eq dum 'bigfloat)
	    (if (prog2 (setq dum ($bfloat x)) ($bfloatp dum))
		(setq exp dum)))
	   ((eq dum 'float)
	    (if (and (setq dum (numer x)) (numberp dum)) (setq exp dum)))
	   ((and (member dum '(numer symbol) :test #'eq)
		 (prog2 (setq dum (numer x))
		     (or (null dum)
			 (and (numberp dum)
			      (prog2 (setq exp dum)
				  (< (abs dum) 1.0d-6))))))
	    (cond ($signbfloat
		   (and (setq dum ($bfloat x)) ($bfloatp dum) (setq exp dum)))
		  (t (setq sign '$pnz evens nil odds (ncons x) minus nil)
		     (return sign)))))
     (or (and (not (atom x)) (not (mnump x)) (equal x exp)
	      (let (s o e m lhs rhs)
		(compsplt x)
		(dcompare lhs rhs)
		(cond ((member sign '($pos $neg $zero) :test #'eq))
		      ((eq sign '$pnz) nil)
		      (t (setq s sign o odds e evens m minus)
			 (sign x)
			 (if (not (strongp sign s))
			     (if (and (eq sign '$pnz) (eq s '$pn))
				 (setq sign s)
				 (setq sign s odds o evens e minus m)))
			 t))))
	 (sign exp))
     (return sign)))

(defun numer (x)
  (let ($ratsimpexpons)
    (car (errset (meval `(($ev) ,x $numer $%enumer)) nil))))

(defun constp (x)
  (cond ((floatp x) 'float)
	((numberp x) 'numer)
	((symbolp x) (if (member x '($%pi $%e $%phi $%gamma) :test #'eq) 'symbol))
	((eq (caar x) 'rat) 'numer)
	((eq (caar x) 'bigfloat) 'bigfloat)
	((specrepp x) (constp (specdisrep x)))
	(t (do ((l (cdr x) (cdr l)) (dum) (ans 'numer))
	       ((null l) ans)
	     (setq dum (constp (car l)))
	     (cond ((eq dum 'float) (return 'float))
		   ((eq dum 'numer))
		   ((eq dum 'bigfloat) (setq ans 'bigfloat))
		   ((eq dum 'symbol)
		    (if (eq ans 'numer) (setq ans 'symbol)))
		   (t (return nil)))))))

(defmfun sign (x)
  (cond ((mnump x) (setq sign (rgrp x 0) minus nil odds nil evens nil))
	((atom x) (if (eq x '$%i) (imag-err x)) (sign-any x))
	((eq (caar x) 'mtimes) (sign-mtimes x))
	((eq (caar x) 'mplus) (sign-mplus x))
	((eq (caar x) 'mexpt) (sign-mexpt x))
	((eq (caar x) '%log) (compare (cadr x) 1))
	((eq (caar x) 'mabs) (sign-mabs x))
	((member (caar x) '(%csc %csch) :test #'eq)
	 (sign (inv* (cons (ncons (zl-get (caar x) 'recip)) (cdr x)))))
	((specrepp x) (sign (specdisrep x)))
	((kindp (caar x) '$posfun) (sign-posfun x))
	((or (member (caar x) '(%signum %erf) :test #'eq)
	     (and (kindp (caar x) '$oddfun) (kindp (caar x) '$increasing)))
	 (sign-oddinc x))
	(t (sign-any x))))

(defun sign-any (x)
  (dcompare x 0)
  (if (and $assume_pos
	   (member sign '($pnz $pz $pn) :test #'eq)
	   (if $assume_pos_pred (let ((*x* x)) (is '(($assume_pos_pred) *x*)))
	       (mapatom x)))
      (setq sign '$pos))
  (setq minus nil evens nil
	odds (if (not (member sign '($pos $neg $zero) :test #'eq)) (ncons x))))

(defun sign-mtimes (x)
  (setq x (cdr x))
  (do ((s '$pos) (m) (o) (e)) ((null x) (setq sign s minus m odds o evens e))
    (sign1 (car x))
    (cond ((eq sign '$zero) (return t))
	  ((eq sign '$pos))
	  ((eq sign '$neg) (setq s (flip s) m (not m)))
	  ((prog2 (setq m (not (eq m minus)) o (nconc odds o) e (nconc evens e))
	       nil))
	  ((eq s sign))
	  ((eq s '$pos) (setq s sign))
	  ((eq s '$neg) (setq s (flip sign)))
	  ((or (and (eq s '$pz) (eq sign '$nz))
	       (and (eq s '$nz) (eq sign '$pz)))
	   (setq s '$nz))
	  (t (setq s '$pnz)))
    (setq x (cdr x))))

(defun sign-mplus (x &aux s o e m)
  (cond ((signdiff x))
	((prog2 (setq s sign e evens o odds m minus) nil))
	((signsum x))
	((prog2 (cond ((strongp s sign))
		      (t (setq s sign e evens o odds m minus)))
	     nil))
	((and (not factored) (signfactor x)))
	((strongp sign s))
	(t (setq sign s evens e odds o minus m))))

(defun signdiff (x)
  (setq sign '$pnz)
  (compsplt x)
  (if (and (mplusp lhs) (equal rhs 0)
	   (null (cdddr lhs))
	   (negp (cadr lhs)) (not (negp (caddr lhs))))
      (setq rhs (neg (cadr lhs)) lhs (caddr lhs)))
  (let (dum)
    (cond ((or (equal rhs 0) (mplusp lhs)) nil)
	  ((and (member (constp rhs) '(numer symbol) :test #'eq)
		(numberp (setq dum (numer rhs)))
		(prog2 (setq rhs dum) nil)))
	  ((mplusp rhs) nil)
	  ((and (dcompare lhs rhs) (member sign '($pos $neg $zero) :test #'eq)))
	  ((and (not (atom lhs)) (not (atom rhs))
		(eq (caar lhs) (caar rhs))
		(kindp (caar lhs) '$increasing))
	   (sign (sub (cadr lhs) (cadr rhs)))
	   t)
	  ((and (not (atom lhs)) (eq (caar lhs) 'mabs)
		(alike1 (cadr lhs) rhs))
	   (setq sign '$pz minus nil odds nil evens nil) t)
	  ((signdiff-special lhs rhs)))))

(defun signdiff-special (xlhs xrhs)
  (when (or (and (numberp xrhs) (minusp xrhs)
		 (not (atom xlhs)) (eq (sign* xlhs) '$pos))
					; e.g. sign(a^3+%pi-1) where a>0
	    (and (mexptp xlhs)		; e.g. sign(%e^x-1) where x>0
		 (member (sign* (sub 1 xrhs)) '($pos $zero $pz) :test #'eq)
		 (eq (sign* (caddr xlhs)) '$pos)
		 (eq (sign* (sub (cadr xlhs) 1)) '$pos))
	    (and (mexptp xlhs) (mexptp xrhs) ; e.g. sign(2^x-2^y) where x>y
		 (alike1 (cadr xlhs) (cadr xrhs))
		 (eq (sign* (sub (cadr xlhs) 1)) '$pos)
		 (eq (sign* (sub (caddr xlhs) (caddr xrhs))) '$pos)))
    (setq sign '$pos minus nil odds nil evens nil) t))

(defun signsum (x)
  (do ((l (cdr x) (cdr l)) (s '$zero))
      ((null l) (setq sign s minus nil odds (list x) evens nil) t)
    (sign (car l))
    (cond ((or (and (eq sign '$zero)
		    (setq x (sub x (car l))))
	       (and (eq s sign) (not (eq s '$pn))) ; $PN + $PN = $PNZ
	       (and (eq s '$pos) (eq sign '$pz))
	       (and (eq s '$neg) (eq sign '$nz))))
	  ((or (and (member sign '($pz $pos) :test #'eq) (member s '($zero $pz) :test #'eq))
	       (and (member sign '($nz $neg) :test #'eq) (member s '($zero $nz) :test #'eq))
	       (and (eq sign '$pn) (eq s '$zero)))
	   (setq s sign))
	  (t (setq sign '$pnz odds (list x) evens nil minus nil)
	     (return nil)))))

(defun signfactor (x)
  (let (y (factored t))
    (setq y (factor-if-small x))
    (cond ((or (mplusp y) (> (conssize y) 50.))
	   (prog2
	       (setq sign '$pnz)
	       nil))
	  (t (sign y)))))

(defun factor-if-small (x)
  (if (< (conssize x) 51.)
      (let ($ratprint)
	(factor x)) x))

(defmvar complexsign nil)

(defun sign-mexpt (x)
  (let* ((expt (caddr x)) (base1 (cadr x))
	 (sign-expt (sign1 expt)) (sign-base (sign1 base1))
	 (evod (evod expt)))
    (cond ((and (eq sign-base '$zero)
		(member sign-expt '($zero $neg) :test #'eq))
	   (dbzs-err x))
	  ((eq sign-expt '$zero) (setq sign '$pos) (tdzero (sub x 1)))
	  ((eq sign-base '$pos))
	  ((eq sign-base '$zero) (tdpos expt))
	  ((eq evod '$even)
	   (cond ((eq sign-expt '$neg)
		  (setq sign '$pos minus nil evens (ncons base1) odds nil)
		  (tdpn base1))
		 ((member sign-base '($pn $neg) :test #'eq)
		  (setq sign '$pos minus nil
			evens (nconc odds evens)
			odds nil))
		 (t (setq sign '$pz minus nil
			  evens (nconc odds evens)
			  odds nil))))
	  ((and (member sign-expt '($neg $nz) :test #'eq)
		(member sign-base '($nz $pz $pnz) :test #'eq))
	   (tdpn base1)
	   (setq sign (cond ((eq sign-base '$pnz) '$pn)
			    ((eq sign-base '$pz) '$pos)
			    ((eq sign-expt '$neg) '$neg)
			    (t '$pn))))
	  ((member sign-expt '($pz $nz $pnz) :test #'eq)
	   (cond ((eq sign-base '$neg)
		  (setq odds (ncons x) sign '$pn))))
	  ((eq sign-expt '$pn))
	  (t (cond ((ratnump expt)
		    (cond ((mevenp (cadr expt))
			   (cond ((member sign-base '($pn $neg) :test #'eq)
				  (setq sign-base '$pos))
				 ((member sign-base '($pnz $nz) :test #'eq)
				  (setq sign-base '$pz)))
			   (setq evens (nconc odds evens)
				 odds nil minus nil))
			  ((mevenp (caddr expt))
			   (cond (complexsign
				  (setq sign-base (setq sign-expt '$pnz)))
				 ((eq sign-base '$neg) (imag-err x))
				 ((eq sign-base '$pn)
				  (setq sign-base '$pos)
				  (tdpos base1))
				 ((eq sign-base '$nz)
				  (setq sign-base '$zero)
				  (tdzero base1))
				 (t (setq sign-base '$pz)
				    (tdpz base1)))))))
	     (cond ((eq sign-expt '$neg)
		    (cond ((eq sign-base '$zero) (dbzs-err x))
			  ((eq sign-base '$pz)
			   (setq sign-base '$pos)
			   (tdpos base1))
			  ((eq sign-base '$nz)
			   (setq sign-base '$neg)
			   (tdneg base1))
			  ((eq sign-base '$pnz)
			   (setq sign-base '$pn)
			   (tdpn base1)))))
	     (setq sign sign-base)))))

(defun sign-mabs (x)
  (sign (cadr x))
  (cond ((member sign '($pos $zero) :test #'eq))
	((member sign '($neg $pn) :test #'eq) (setq sign '$pos))
	(t (setq sign '$pz minus nil evens (nconc odds evens) odds nil))))

(defun sign-posfun (xx)
  (declare (ignore xx))
  (setq sign '$pos minus nil odds nil evens nil))

(defun sign-oddinc (x)
  (sign (cadr x)))

(defun imag-err (x)
  (if sign-imag-errp
      (merror "`sign' called on an imaginary argument:~%~M" x)
      (throw 'sign-imag-err t)))

(defun dbzs-err (x)
  (merror "Division by zero detected in `sign':~%~M" x))

;; Return true iff e is an expression with operator op1, op2,...,or opn.

(defun op-equalp (e &rest op)
  (and (consp e) (consp (car e)) (some #'(lambda (s) (equal (caar e) s)) op)))

;; Return true iff the operator of e is a Maxima relation operator.

(defun mrelationp (a)
  (op-equalp a 'mlessp 'mleqp 'mequal 'mgeqp 'mgreaterp))

;; This version of featurep applies ratdisrep to the first argument.  This
;; change allows things like featurep(rat(n),integer) --> true when n has
;; been declared an integer.

(defmfun $featurep (e ind)
  (setq e ($ratdisrep e))
  (cond ((not (symbolp ind)) (merror "The second argument to 'featurep' must be a symbol"))
	((eq ind '$integer) (maxima-integerp e))
	((eq ind '$noninteger) (nonintegerp e))
	((eq ind '$even) (mevenp e))
	((eq ind '$odd) (moddp e))
	((eq ind '$real)
	 (if (atom e)
	     (or (numberp e) (kindp e '$real) (numberp (numer e)))
	     (free ($rectform e) '$%i)))
	((eq ind '$complex) t)
	((symbolp e) (kindp e ind))))

;; Give a function the maps-integers-to-integers property when it is integer
;; valued on the integers; give it the integer-valued property when its
;; range is a subset of the integers. What have I missed?

(setf (get 'mplus 'maps-integers-to-integers) t)
(setf (get 'mtimes 'maps-integers-to-integers) t)
(setf (get 'mabs 'maps-integers-to-integers) t)
(setf (get '$max 'maps-integers-to-integers) t)
(setf (get '$min 'maps-integers-to-integers) t)

(setf (get '$floor 'integer-valued) t)
(setf (get '$ceiling 'integer-valued) t)
(setf (get '%signum  'integer-valued) t)
(setf (get '$signum 'integer-valued) t)
(setf (get '$charfun 'integer-valued) t)

(defun maxima-integerp (x)
  (cond ((integerp x))
	((mnump x) nil)
	((and (symbolp x) (or (kindp x '$integer) (kindp x '$even) (kindp x '$odd))))
	(t (let ((x-op (and (consp x) (consp (car x)) (caar x))) ($prederror nil))
	     (cond ((null x-op) nil)
		   ((not (symbolp x-op)) nil) ; fix for mqapply at some point?
		   ((eq x-op 'mrat) (and (integerp (cadr x)) (equal (cddr x) 1)))
		   ;; mtimes and mplus are generally handled by this clause
		   ((and (get x-op 'maps-integers-to-integers) (every #'maxima-integerp (margs x))))
		   ;; Special case for 1/2*...*even
		   ((eq x-op 'mtimes)
		    (and (mnump (cadr x))
			 (integerp (mul 2 (cadr x)))
			 (every 'maxima-integerp (cddr x))
			 (some #'(lambda (s) ($featurep s '$even)) (rest (margs x)))))
		   ((eq x-op 'mexpt)
		    (and (every #'maxima-integerp (margs x))
			 (null (mevalp (mlsp (caddr x) 0)))))
		   ;; ! in Maxima allows real arguments
		   ((eq x-op 'mfactorial)
		    (and (maxima-integerp (cadr x))
			 (not (mevalp (mlsp (cadr x) 0)))))
		   ((eq x-op '%gamma)
		    (and (maxima-integerp (cadr x))
			 (not (mevalp (mlsp (cadr x) 1)))))
		   ;; other x-ops
		   ((or ($featurep ($verbify x-op) '$integervalued)
			(get x-op 'integer-valued))))))))

(defmfun nonintegerp (e)
  (let (num)
    (cond ((integerp e) nil)
	  ((mnump e) t)
	  ((atom e) (kindp e '$noninteger))
	  ((specrepp e) (nonintegerp (specdisrep e)))
	  ((and (eq (caar e) 'mplus) (ratnump (cadr e)) (intp (cdr e))) t)
	  ((and (integerp (setq num ($num e)))
		(prog2 (setq e ($denom e))
		    (or (eq (csign (sub e num)) '$pos)
			(eq (csign (add2 e num)) '$neg))))
	   t))))

(defun intp (l)
  (setq l (cdr l))
  (do () ((null l) t)
    (if (maxima-integerp (car l))
	(setq l (cdr l))
	(return nil))))

(defun intp-mexpt (e)
  (and (integerp (caddr e)) (not (minusp (caddr e))) (maxima-integerp (cadr e))))


(defmfun mevenp (e)
  (cond ((integerp e) (not (oddp e)))
	((mnump e) nil)
	(t (eq '$even (evod e)))))

(defmfun moddp (e)
  (cond ((integerp e) (oddp e))
	((mnump e) nil)
	(t (eq '$odd (evod e)))))

;; An extended evod that recognizes that abs(even) is even and
;; abs(odd) is odd.

(defmfun evod (e)
  (cond ((integerp e) (if (oddp e) '$odd '$even))
	((mnump e) nil)
	((atom e) (cond ((kindp e '$odd) '$odd) ((kindp e '$even) '$even)))
	((eq 'mtimes (caar e)) (evod-mtimes e))
	((eq 'mplus (caar e)) (evod-mplus e))
	((eq 'mabs (caar e)) (evod (cadr e))) ;; extra code
	((eq 'mexpt (caar e)) (evod-mexpt e))))

(defun evod-mtimes (x)
  (do ((l (cdr x) (cdr l)) (flag '$odd))
      ((null l) flag)
    (setq x (evod (car l)))
    (cond ((eq '$odd x))
	  ((eq '$even x) (setq flag '$even))
	  ((maxima-integerp (car l)) (cond ((eq '$odd flag) (setq flag nil))))
	  (t (return nil)))))

(defun evod-mplus (x)
  (do ((l (cdr x) (cdr l)) (flag))
      ((null l) (cond (flag '$odd) (t '$even)))
    (setq x (evod (car l)))
    (cond ((eq '$odd x) (setq flag (not flag)))
	  ((eq '$even x))
	  (t (return nil)))))

(defun evod-mexpt (x)
  (when (and (integerp (caddr x)) (not (minusp (caddr x))))
    (evod (cadr x))))


(declare-top (special mgqp mlqp))

(defmode cl ()
  (atom (selector +labs) (selector -labs) (selector data)))

(defmacro c-dobj (&rest x)
  `(list ,@x))

(defun dcompare (x y)
  (setq odds (list (sub x y)) evens nil minus nil
	sign (cond ((eq x y) '$zero)
		   ((or (eq '$inf x) (eq '$minf y)) '$pos)
		   ((or (eq '$minf x) (eq '$inf y)) '$neg)
		   (t (dcomp x y)))))

(defun dcomp (x y)
  (let (mgqp mlqp)
    (setq x (dinternp x) y (dinternp y))
    (cond ((or (null x) (null y)) '$pnz)
	  ((progn (clear) (deq x y) (sel y +labs)))
	  (t '$pnz))))


(defun deq (x y)
  (cond ((dmark x '$zero) nil)
	((eq x y))
	(t (do ((l (sel x data) (cdr l))) ((null l))
	     (if (and (visiblep (car l)) (deqf x y (car l))) (return t))))))

(defun deqf (x y f)
  (cond ((eq 'meqp (caar f))
	 (if (eq x (cadar f)) (deq (caddar f) y) (deq (cadar f) y)))
	((eq 'mgrp (caar f))
	 (if (eq x (cadar f)) (dgr (caddar f) y) (dls (cadar f) y)))
	((eq 'mgqp (caar f))
	 (if (eq x (cadar f)) (dgq (caddar f) y) (dlq (cadar f) y)))
	((eq 'mnqp (caar f))
	 (if (eq x (cadar f)) (dnq (caddar f) y) (dnq (cadar f) y)))))

(defun dgr (x y)
  (cond ((dmark x '$pos) nil)
	((eq x y))
	(t (do ((l (sel x data) (cdr l)))
	       ((null l))
	     (when (or mlqp (and (visiblep (car l)) (dgrf x y (car l))))
	       (return t))))))

(defun dgrf (x y f)
  (cond ((eq 'mgrp (caar f)) (if (eq x (cadar f)) (dgr (caddar f) y)))
	((eq 'mgqp (caar f)) (if (eq x (cadar f)) (dgr (caddar f) y)))
	((eq 'meqp (caar f))
	 (if (eq x (cadar f))
	     (dgr (caddar f) y)
	     (dgr (cadar f) y)))))

(defun dls (x y)
  (cond ((dmark x '$neg) nil)
	((eq x y))
	(t (do ((l (sel x data) (cdr l)))
	       ((null l))
	     (when (or mgqp (and (visiblep (car l)) (dlsf x y (car l))))
	       (return t))))))

(defun dlsf (x y f)
  (cond ((eq 'mgrp (caar f)) (if (eq x (caddar f)) (dls (cadar f) y)))
	((eq 'mgqp (caar f)) (if (eq x (caddar f)) (dls (cadar f) y)))
	((eq 'meqp (caar f))
	 (if (eq x (cadar f)) (dls (caddar f) y) (dls (cadar f) y)))))

(defun dgq (x y)
  (cond ((member (sel x +labs) '($pos $zero) :test #'eq) nil)
	((eq '$nz (sel x +labs)) (deq x y))
	((eq '$pn (sel x +labs)) (dgr x y))
	((dmark x '$pz) nil)
	((eq x y) (setq mgqp t) nil)
	(t (do ((l (sel x data) (cdr l))) ((null l))
	     (if (and (visiblep (car l)) (dgqf x y (car l))) (return t))))))

(defun dgqf (x y f)
  (cond ((eq 'mgrp (caar f)) (if (eq x (cadar f)) (dgr (caddar f) y)))
	((eq 'mgqp (caar f)) (if (eq x (cadar f)) (dgq (caddar f) y)))
	((eq 'meqp (caar f))
	 (if (eq x (cadar f)) (dgq (caddar f) y) (dgq (cadar f) y)))))

(defun dlq (x y)
  (cond ((member (sel x +labs) '($neg $zero) :test #'eq) nil)
	((eq '$pz (sel x +labs)) (deq x y))
	((eq '$pn (sel x +labs)) (dgr x y))
	((dmark x '$nz) nil)
	((eq x y) (setq mlqp t) nil)
	(t (do ((l (sel x data) (cdr l))) ((null l))
	     (if (and (visiblep (car l)) (dlqf x y (car l))) (return t))))))

(defun dlqf (x y f)
  (cond ((eq 'mgrp (caar f)) (if (eq x (caddar f)) (dls (cadar f) y)))
	((eq 'mgqp (caar f)) (if (eq x (caddar f)) (dlq (cadar f) y)))
	((eq 'meqp (caar f))
	 (if (eq x (cadar f)) (dlq (caddar f) y) (dlq (cadar f) y)))))

(defun dnq (x y)
  (cond ((member (sel x +labs) '($pos $neg) :test #'eq) nil)
	((eq '$pz (sel x +labs)) (dgr x y))
	((eq '$nz (sel x +labs)) (dls x y))
	((dmark x '$pn) nil)
	((eq x y) nil)
	(t (do ((l (sel x data) (cdr l))) ((null l))
	     (if (and (visiblep (car l)) (dnqf x y (car l))) (return t))))))

(defun dnqf (x y f)
  (cond ((eq 'meqp (caar f))
	 (if (eq x (cadar f)) (dnq (caddar f) y) (dnq (cadar f) y)))))

(defun dmark (x m)
  (cond ((eq m (sel x +labs)))
	((and dbtrace (prog1
			  t
			(mtell "marking ~M ~M" (if (atom x) x (car x)) m))
	      nil))
	(t
	 (setq +labs (cons x +labs))
	 (_ (sel x +labs) m)
	 nil)))

(defun daddgr (flag x)
  (let (lhs rhs)
    (compsplt x)
    (mdata flag 'mgrp (dintern lhs) (dintern rhs))
    (if (or (mnump lhs) (constant lhs))
	(list '(mlessp) rhs lhs)
	(list '(mgreaterp) lhs rhs))))

(defun daddgq (flag x)
  (let (lhs rhs)
    (compsplt x)
    (mdata flag 'mgqp (dintern lhs) (dintern rhs))
    (if (or (mnump lhs) (constant lhs))
	(list '(mleqp) rhs lhs)
	(list '(mgeqp) lhs rhs))))

(defun daddeq (flag x)
  (let (lhs rhs)
    (compsplt-eq x)
    (mdata flag 'meqp (dintern lhs) (dintern rhs))
    (list '($equal) lhs rhs)))

(defun daddnq (flag x)
  (let (lhs rhs)
    (compsplt-eq x)
    (cond ((and (mtimesp lhs) (equal rhs 0))
	   (dolist (term (cdr lhs)) (daddnq flag term)))
	  ((and (mexptp lhs) (mexptp rhs)
		(integerp (caddr lhs)) (integerp (caddr rhs))
		(equal (caddr lhs) (caddr rhs)))
	   (mdata flag 'mnqp (dintern (cadr lhs)) (dintern (cadr rhs)))
	   (cond ((not (oddp (caddr lhs)))
		  (mdata flag 'mnqp (dintern (cadr lhs))
			 (dintern (neg (cadr rhs)))))))
	  (t (mdata flag 'mnqp (dintern lhs) (dintern rhs))))
    (list '(mnot) (list '($equal) lhs rhs))))

(defun tdpos (x)
  (daddgr t x)
  (setq locals (cons (cons x '$pos) locals)))

(defun tdneg (x)
  (daddgr t (neg x))
  (setq locals (cons (cons x '$neg) locals)))

(defun tdzero (x)
  (daddeq t x)
  (setq locals (cons (cons x '$zero) locals)))

(defun tdpn (x)
  (daddnq t x)
  (setq locals (cons (cons x '$pn) locals)))

(defun tdpz (x)
  (daddgq t x)
  (setq locals (cons (cons x '$pz) locals)))

(defun compsplt-eq (x)
  (compsplt x)
  (if (equal lhs 0)
      (setq lhs rhs rhs 0))
  (if (and (equal rhs 0)
	   (or (mexptp lhs)
	       (and (not (atom lhs))
		    (kindp (caar lhs) '$oddfun)
		    (kindp (caar lhs) '$increasing))))
      (setq lhs (cadr lhs))))

(defun mdata (flag r x y)
  (if flag
      (mfact r x y)
      (mkill r x y)))

(defun mfact (r x y)
  (let ((f (datum (list r x y))))
    (cntxt f context)
    (addf f x)
    (addf f y)))

(defun mkill (r x y)
  (let ((f (car (datum (list r x y)))))
    (kcntxt f context)
    (maxima-remf f x)
    (maxima-remf f y)))

(defun mkind (x y)
  (kind (dintern x) (dintern y)))

(defmfun rgrp (x y)
  (cond ((or ($bfloatp x) ($bfloatp y))
	 (setq x (let (($float2bf t)) (cadr ($bfloat (sub x y)))) y 0))
	((numberp x)
	 (cond ((numberp y))
	       (t (setq x (* x (caddr y)) y (cadr y)))))
	((numberp y) (setq y (* (caddr x) y) x (cadr x)))
	(t (let ((dummy x))
	     (setq x (* (cadr x) (caddr y)))
	     (setq y (* (caddr dummy) (cadr y))))))
  (cond ((> x y) '$pos)
	((> y x) '$neg)
	(t '$zero)))

(defun mcons (x l)
  (cons (car l) (cons x (cdr l))))

(defun flip (s)
  (cond ((eq '$pos s) '$neg)
	((eq '$neg s) '$pos)
	((eq '$pz s) '$nz)
	((eq '$nz s) '$pz)
	(t s)))

(defun strongp (x y)
  (cond ((eq '$pnz y))
	((eq '$pnz x) nil)
	((member y '($pz $nz $pn) :test #'eq))))

(defun munformat (form)
  (if (atom form)
      form
      (cons (caar form) (mapcar #'munformat (cdr form)))))

(defmfun declarekind (var prop)	; This function is for $DECLARE to use.
  (let (prop2)
    (cond ((truep (list 'kind var prop)) t)
	  ((or (falsep (list 'kind var prop))
	       (and (setq prop2 (assoc prop '(($integer . $noninteger)
					     ($noninteger . $integer)
					     ($increasing . $decreasing)
					     ($decreasing . $increasing)
					     ($symmetric . $antisymmetric)
					     ($antisymmetric . $symmetric)
					     ($oddfun . $evenfun)
					     ($evenfun . $oddfun)) :test #'eq))
		    (truep (list 'kind var (cdr prop2)))))
	   (merror "Inconsistent Declaration: ~:M" `(($declare) ,var ,prop)))
	  (t (mkind var prop) t))))

;;;  These functions reformat expressions to be stored in the data base.

(defun compsplt (x)
  (cond ((atom x) (setq lhs x rhs 0))
	((atom (car x)) (setq lhs x rhs 0))
	((not (null (cdr (symbols x)))) (compsplt2 x))
	(t (compsplt1 x))))

(defun compsplt1 (x)
  (do ((exp (list x 0)) (success nil))
      ((or success (symbols (cadr exp))) (setq lhs (car exp) rhs (cadr exp)))
    (cond ((atom (car exp)) (setq success t))
	  ((eq (caaar exp) 'mplus)  (setq exp (splitsum exp)))
	  ((eq (caaar exp) 'mtimes) (setq exp (splitprod exp)))
	  (t (setq success t)))))

(defun compsplt2 (x)
  (cond ((or (atom x) (atom (car x))) ; If x is an atom or a single level
	 (setq lhs x rhs 0))	;    list then we won't change it any.
	((negp x)	    ; If x is a negative expression but not a
	 (setq lhs 0 rhs (neg x))) ; sum, then get rid of the negative sign.
	((or (cdddr x)		      ; If x is not a sum, or is a sum
	     (not (eq (caar x) 'mplus))	; with more than 2 terms, or has
	     (intersect* (symbols (cadr x)) (symbols (caddr x))))
					; some symbols common to both summands, then do nothing.
	 (setq lhs x rhs 0))
	((and (or (negp (cadr x)) (mnump (cadr x)))
	      (not (negp (caddr x))))
	 (setq lhs (caddr x) rhs (neg (cadr x))))
	((and (not (negp (cadr x)))
	      (or (negp (caddr x)) (mnump (caddr x))))
	 (setq lhs (cadr x) rhs (neg (caddr x))))
	((and (negp (cadr x)) (negp (caddr x)))
	 (setq lhs 0 rhs (neg x)))
	(t (setq lhs x rhs 0))))

(defun negp (x)
  (and (mtimesp x) (mnegp (cadr x))))

(defun splitsum (exp)
  (do ((llist (cdar exp) (cdr llist))
       (lhs (car exp))
       (rhs (cadr exp)))
      ((null llist)
       (if (mplusp lhs) (setq success t))
       (list lhs rhs))
    (cond ((member '$inf llist :test #'eq)
	   (setq rhs (add2 '$inf (sub* rhs (addn llist t)))
		 lhs (add2 '$inf (sub* lhs (addn llist t)))
		 llist nil))
	  ((member '$minf llist :test #'eq)
	   (setq rhs (add2 '$minf (sub* rhs (addn llist t)))
		 lhs (add2 '$minf (sub* lhs (addn llist t)))
		 llist nil))
	  ((null (symbols (car llist)))
	   (setq lhs (sub lhs (car llist))
		 rhs (sub rhs (car llist)))))))

(defun splitprod (exp)
  (do ((flipsign)
       (lhs (car exp))
       (rhs (cadr exp))
       (llist (cdar exp) (cdr llist))
       (sign)
       (minus)
       (evens)
       (odds))
      ((null llist)
       (if (mtimesp lhs) (setq success t))
       (cond (flipsign (compsplt (sub lhs rhs))
		       (setq success t)
		       (list rhs lhs))
	     (t (list lhs rhs))))
    (when (null (symbols (car llist)))
      (sign (car llist))
      (if (eq sign '$neg) (setq flipsign (not flipsign)))
      (if (member sign '($pos $neg) :test #'eq)
	  (setq lhs (div lhs (car llist)) rhs (div rhs (car llist)))))))

(defun symbols (x)
  (let (($listconstvars %initiallearnflag))
    (cdr ($listofvars x))))

;; %initiallearnflag is only necessary so that %PI, %E, etc. can be LEARNed.

(eval-when
    #+gcl (load)
    #-gcl (:load-toplevel)

  (setq %initiallearnflag t)

  (def-learn $%e `((mequal) $%e ,(mget '$%e '$numer)) t)
  (def-learn $%pi `((mequal) $%pi ,(mget '$%pi '$numer)) t)
  (def-learn $%phi `((mequal) $%phi ,(mget '$%phi '$numer)) t)
  (def-learn $%gamma `((mequal) $%gamma ,(mget '$%gamma '$numer)) t)

  (setq %initiallearnflag nil)

  (mapc #'true*
	'((par ($even $odd) $integer)

	  (kind $integer $rational)

	  (par ($rational $irrational) $real)
	  (par ($real $imaginary) $complex)

	  (kind %log $increasing)
	  (kind %atan $increasing) (kind %atan $oddfun)
	  (kind $delta $evenfun)
	  (kind %sinh $increasing) (kind %sinh $oddfun)
	  (kind %cosh $posfun)
	  (kind %tanh $increasing) (kind %tanh $oddfun)
	  (kind %coth $oddfun)
	  (kind %csch $oddfun)
	  (kind %sech $posfun)
	  (kind $li $complex)
	  (kind %cabs $complex)))

  ;; Create an initial context for the user which is a subcontext of $global.
  ($newcontext '$initial))
