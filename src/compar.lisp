;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module compar)

(load-macsyma-macros mrgmac)

(declare-top (special success))

(defvar *debug-compar* nil
  "Enables debugging code for this file.")

(defvar %initiallearnflag)

(defmvar complexsign nil
  "If T, COMPAR attempts to work in a complex mode.
	  This scheme is only very partially developed at this time."
  no-reset)

(defmvar $signbfloat t)
(defmvar $askexp)
(defmvar $assume_pos nil)
(defmvar $assume_pos_pred nil)

(defmvar factored nil)

;; The *LOCAL-SIGNS* variable contains a list of facts that are local to the
;; current evaluation. These are stored in the assume database (in the global
;; context) by asksign1 when the user answers questions. A "top-level"
;; evaluation is run by MEVAL* and that function calls CLEARSIGN when it
;; finishes to discard them.
(defmvar *local-signs* nil)

(defmvar sign nil)
(defmvar minus nil)
(defmvar odds nil)
(defmvar evens nil)

(defvar $useminmax t)

;; Remove this (nil'ed out) function after a while.  We should be
;; using POWER instead of POW.
#+nil
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

(defmfun $activate (&rest args)
  (dolist (c args)
    (cond ((not (symbolp c)) (nc-err '$activate c))
	  ((member c (cdr $activecontexts) :test #'eq))
	  ((member c (cdr $contexts) :test #'eq)
	   (setq $activecontexts (mcons c $activecontexts))
	   (activate c))
	  (t (merror (intl:gettext "activate: no such context ~:M") c))))
  '$done)

;;; This "turns off" a context, keeping the facts, but making them invisible

(defmfun $deactivate (&rest args)
  (dolist (c args)
    (cond ((not (symbolp c)) (nc-err '$deactivate c))
	  ((member c (cdr $contexts) :test #'eq)
	   (setq $activecontexts ($delete c $activecontexts))
	   (deactivate c))
	  (t (merror (intl:gettext "deactivate: no such context ~:M") c))))
  '$done)

;;; This function prints out a list of the facts in the specified context.
;;; No argument implies the current context.

(defmfun $facts (&optional (ctxt $context))
  (if (member ctxt (cdr $contexts))
      (facts1 ctxt)
      (facts2 ctxt)))

(defun facts1 (con)
  (contextmark)
  (do ((l (zl-get con 'data) (cdr l))
       (nl)
       (u))
      ((null l) (cons '(mlist) nl))
    (when (visiblep (car l))
      (setq u (intext (caaar l) (cdaar l)))
      (unless (memalike u nl)
	(push u nl)))))

;; Look up facts from the database which contain expr. expr can be a symbol or 
;; a more general expression.
(defun facts2 (expr)
  (labels ((among (x l)
             (cond ((null l) nil)
                   ((atom l) (eq x l))
                   ((alike1 x l) t)
                   (t
                    (do ((ll (cdr l) (cdr ll)))
                        ((null ll) nil)
                      (if (among x (car ll)) (return t)))))))
  (do ((facts (cdr ($facts $context)) (cdr facts))
       (ans))
      ((null facts) (return (cons '(mlist) (reverse ans))))
    (when (or (among expr (cadar facts))
              (among expr (caddar facts)))
      (push (car facts) ans)))))

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
  (cond ((not (symbolp y)) (nc-err "context assignment" y))
	((member y $contexts :test #'eq) (setq context y $context y))
	(t ($newcontext y))))

;;; This function actually creates a context whose subcontext is $GLOBAL.
;;; It also switches contexts to the newly created one.
;;; If no argument supplied, then invent a name via gensym and use that.

(defmfun $newcontext (&rest args)
  (if (null args)
    ($newcontext ($gensym "context")) ;; make up a name and try again
    (if (> (length args) 1)
      (merror "newcontext: found more than one argument.")
      (let ((x (first args)))
        (cond
          ((not (symbolp x)) (nc-err '$newcontext x))
          ((member x $contexts :test #'eq)
           (mtell (intl:gettext "newcontext: context ~M already exists.") x) nil)
          (t
            (setq $contexts (mcons x $contexts))
            (putprop x '($global) 'subc)
            (setq context x $context x)))))))

;;; This function creates a supercontext.  If given one argument, it
;;; makes the current context be the subcontext of the argument.  If
;;; given more than one argument, the first is assumed the name of the
;;; supercontext and the rest are the subcontexts.
;;; If no arguments supplied, then invent a name via gensym and use that.

(defmfun $supcontext (&rest x)
  (cond ((null x) ($supcontext ($gensym "context"))) ;; make up a name and try again
	((caddr x) (merror (intl:gettext "supcontext: found more than two arguments.")))
	((not (symbolp (car x))) (nc-err '$supcontext (car x)))
	((member (car x) $contexts :test #'eq)
	 (merror (intl:gettext "supcontext: context ~M already exists.") (car x)))
	((and (cadr x) (not (member (cadr x) $contexts :test #'eq)))
	 (merror (intl:gettext "supcontext: no such context ~M") (cadr x)))
	(t (setq $contexts (mcons (car x) $contexts))
	   (putprop (car x) (ncons (or (cadr x) $context)) 'subc)
	   (setq context (car x) $context (car x)))))

;;; This function kills a context or a list of contexts

(defmfun $killcontext (&rest args)
  (dolist (c args)
    (if (symbolp c)
	(killcontext c)
	(nc-err '$killcontext c)))
  (if (and (= (length args) 1) (eq (car args) '$global))
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
	 (mtell (intl:gettext "killcontext: no such context ~M.") x))
	((eq x '$global) '$global)
	((eq x '$initial)
	 (mapc #'remov (zl-get '$initial 'data))
	 (remprop '$initial 'data)
	 '$initial)
	((and (not (eq $context x)) (contextmark) (< 0 (zl-get x 'cmark)))
	 (mtell (intl:gettext "killcontext: context ~M is currently active.") x))
        (t (if (member x $activecontexts)
               ;; Context is on the list of active contexts. The test above 
               ;; checks for active contexts, but it seems not to work in all
               ;; cases. So deactivate the context at this place to remove it 
               ;; from the list of active contexts before it is deleted.
               ($deactivate x))
	   (setq $contexts ($delete x $contexts))
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

(defun nc-err (fn x)
  (merror (intl:gettext "~M: context name must be a symbol; found ~M") fn x))

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

(defun maybe-simplifya (x z)
  (if z x (simplifya x z)))

(defun maybe-simplifya-protected (x z)
  (let ((errcatch t) ($errormsg nil))
    (ignore-errors (maybe-simplifya x z) x)))

(defun simp-$is (x yy z)
  (declare (ignore yy))
  (let ((a (maybe-simplifya (cadr x) z)))
    (if (or (eq a t) (eq a nil))
	a
	`((,(caar x) simp) ,a))))

(defmspec $is (form)
  (unless (= 1 (length (rest form)))
    (merror (intl:gettext "is() expects a single argument. Found ~A")
            (length (rest form))))
  (destructuring-bind (answer patevalled)
      (mevalp1 (cadr form))
    (cond ((member answer '(t nil) :test #'eq) answer)
	  ;; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
	  ($prederror (pre-err patevalled))
	  (t '$unknown))))

(defmspec $maybe (form)
  (let* ((pat (cadr form))
	 (x (let (($prederror nil)) (mevalp1 pat)))
	 (ans (car x)))
    (if (member ans '(t nil) :test #'eq)
	ans
	'$unknown)))

(defun is (pred)
  (let (($prederror t))
    (mevalp pred)))

					; The presence of OPERS tells SIMPLIFYA to call OPER-APPLY,
					; which calls NARY1 to flatten nested "and" and "or" expressions
					; (due to $NARY property of MAND and MOR, declared elsewhere).

(putprop 'mand t 'opers)
(putprop 'mor t 'opers)

(putprop 'mnot 'simp-mnot 'operators)
(putprop 'mand 'simp-mand 'operators)
(putprop 'mor 'simp-mor 'operators)

(defun simp-mand (x yy z)
  (declare (ignore yy))
  (do ((l (cdr x) (cdr l))
       (a)
       (simplified))
      ((null l)
       (cond ((= (length simplified) 0) t)
	     ((= (length simplified) 1) (car simplified))
	     (t (cons '(mand simp) (reverse simplified)))))
    (setq a (maybe-simplifya (car l) z))
    (cond ((null a) (return nil))
	  ((eq a '$unknown) (unless (member '$unknown simplified :test #'eq) (push a simplified)))
	  ((not (member a '(t nil) :test #'eq)) (push a simplified)))))

(defun simp-mor (x yy z)
  (declare (ignore yy))
  (do ((l (cdr x) (cdr l))
       (a)
       (simplified))
      ((null l)
       (cond ((= (length simplified) 0) nil)
	     ((= (length simplified) 1) (car simplified))
	     (t (cons '(mor simp) (reverse simplified)))))
    (setq a (maybe-simplifya (car l) z))
    (cond ((eq a t) (return t))
	  ((eq a '$unknown) (unless (member '$unknown simplified :test #'eq) (push a simplified)))
	  ((not (member a '(t nil) :test #'eq)) (push a simplified)))))

					; ALSO CUT STUFF ABOUT NOT EQUAL => NOTEQUAL AT TOP OF ASSUME

(defun simp-mnot (x yy z)
  (declare (ignore yy))
  (let ((arg (maybe-simplifya (cadr x) z)))
    (if (atom arg)
	(cond ((or (eq arg t) (eq arg '$true))
	       nil)
	      ((or (eq arg nil) (eq arg '$false))
	       t)
	      ((eq arg '$unknown)
	       '$unknown)
	      (t `((mnot simp) ,arg)))
	(let ((arg-op (caar arg)) (arg-arg (cdr arg)))
	  ;;(setq arg-arg (mapcar #'(lambda (a) (maybe-simplifya a z)) arg-arg))
          (cond ((eq arg-op 'mlessp)
                 (simplify `((mgeqp) ,@arg-arg)))
                ((eq arg-op 'mleqp)
                 (simplify `((mgreaterp) ,@arg-arg)))
                ((eq arg-op 'mequal)
                 (simplify `((mnotequal) ,@arg-arg)))
                ((eq arg-op '$equal)
                 (simplify `(($notequal) ,@arg-arg)))
                ((eq arg-op 'mnotequal)
                 (simplify `((mequal) ,@arg-arg)))
                ((eq arg-op '$notequal)
                 (simplify `(($equal) ,@arg-arg)))
                ((eq arg-op 'mgeqp)
                 (simplify `((mlessp) ,@arg-arg)))
                ((eq arg-op 'mgreaterp)
                 (simplify `((mleqp) ,@arg-arg)))
		((eq arg-op 'mnot)
		 (car arg-arg))
		;; Distribute negation over conjunction and disjunction;
		;; analogous to '(- (a + b)) --> - a - b.
		((eq arg-op 'mand)
		 (let ((L (mapcar #'(lambda (e) `((mnot) ,e)) arg-arg)))
		   (simplifya `((mor) ,@L) nil)))
		((eq arg-op 'mor)
		 (let ((L (mapcar #'(lambda (e) `((mnot) ,e)) arg-arg)))
		   (simplifya `((mand) ,@L) nil)))
		(t `((mnot simp) ,arg)))))))

;; =>* N.B. *<=
;; The translator depends on some stuff in here.
;; Check it out in the transl module ACALL before proceeding.

(defun mevalp (pat)
  (let* ((x (mevalp1 pat))
	 (ans (car x))
	 (patevalled (cadr x)))
    (cond ((member ans '(t ()) :test #'eq) ans)
	  ;; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
	  ($prederror (pre-err patevalled))
	  (t (or patevalled ans)))))

(defun mevalp1 (pat)
  (let (patevalled ans)
    (setq ans
          (cond ((and (not (atom pat))
                      (member (caar pat) '(mnot mand mor) :test #'eq))
                 (cond ((eq 'mnot (caar pat)) (is-mnot #'mevalp (cadr pat)))
                       ((eq 'mand (caar pat)) (is-mand #'mevalp (cdr pat)))
                       (t (is-mor #'mevalp (cdr pat)))))
                ((atom (setq patevalled (specrepcheck (meval pat))))
                 patevalled)
                ((member (caar patevalled) '(mnot mand mor) :test #'eq)
                 (return-from mevalp1 (mevalp1 patevalled)))
                (t
                 (mevalp2 patevalled
                          (caar patevalled)
                          (cadr patevalled)
                          (caddr patevalled)))))
    (list ans patevalled)))

(defun mevalp2 (patevalled pred arg1 arg2)
  (cond ((eq 'mequal pred) (like arg1 arg2))
	((eq '$equal pred) (meqp arg1 arg2))
	((eq 'mnotequal pred) (not (like arg1 arg2)))
	((eq '$notequal pred) (mnqp arg1 arg2))
	((eq 'mgreaterp pred) (mgrp arg1 arg2))
	((eq 'mlessp pred) (mgrp arg2 arg1))
	((eq 'mgeqp pred) (mgqp arg1 arg2))
	((eq 'mleqp pred) (mgqp arg2 arg1))
	(t (isp (munformat patevalled)))))

(defun pre-err (pat)
  (merror (intl:gettext "Unable to evaluate predicate ~M") pat))

(defun is-mnot (pred-eval pred)
  (setq pred (funcall pred-eval pred))
  (cond ((eq t pred) nil)
	((not pred))
	(t (pred-reverse pred))))

(defun pred-reverse (pred)
  (take '(mnot) pred))
 
(defun is-mand (pred-eval pl)
  (do ((dummy)
       (npl))
      ((null pl) (cond ((null npl))
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mand) (nreverse npl)))))
    (setq dummy (funcall pred-eval (car pl)) pl (cdr pl))
    (cond ((eq t dummy))
	  ((null dummy) (return nil))
	  (t (push dummy npl)))))

(defun is-mor (pred-eval pl)
  (do ((dummy)
       (npl))
      ((null pl) (cond ((null npl) nil)
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mor) (nreverse npl)))))
    (setq dummy (funcall pred-eval (car pl)) pl (cdr pl))
    (cond ((eq t dummy) (return t))
	  ((null dummy))
	  (t (push dummy npl)))))

(defmspec $assume (x)
  (setq x (cdr x))
  (do ((nl)) ((null x) (cons '(mlist) (nreverse nl)))
    (cond ((atom (car x)) (push (assume (meval (car x))) nl))
	  ((eq 'mand (caaar x))
	   (mapc #'(lambda (l) (push (assume (meval l)) nl))
		 (cdar x)))
	  ((eq 'mnot (caaar x))
	   (push (assume (meval (pred-reverse (cadar x)))) nl))
	  ((eq 'mor (caaar x))
	   (merror (intl:gettext "assume: argument cannot be an 'or' expression; found ~M") (car x)))
	  ((eq (caaar x) 'mequal)
	   (merror (intl:gettext "assume: argument cannot be an '=' expression; found ~M~%assume: maybe you want 'equal'.") (car x)))
	  ((eq (caaar x) 'mnotequal)
	   (merror (intl:gettext "assume: argument cannot be a '#' expression; found ~M~%assume: maybe you want 'not equal'.") (car x)))
	  (t (push (assume (meval (car x))) nl)))
    (setq x (cdr x))))

(defun assume (pat)
  (if (and (not (atom pat))
	   (eq (caar pat) 'mnot)
	   (eq (caaadr pat) '$equal))
      (setq pat `(($notequal) ,@(cdadr pat))))
  (let ((dummy (let ($assume_pos) (car (mevalp1 pat)))))
    (cond ((eq dummy t) '$redundant)
	  ((null dummy) '$inconsistent)
	  ((atom dummy) '$meaningless)
	  (t (learn pat t)))))

(defun learn (pat flag)
  (cond ((atom pat))
        ;; Check for abs function in pattern.
        ((and (not limitp)
              (learn-abs pat flag)))
        ;; Check for constant expression in pattern.
        ((and (not limitp)
              (learn-numer pat flag)))
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
	(t 
      (cond
        ((eq (caar pat) '$kind)
         (unkind (second pat) (third pat)))
        (t (untrue (munformat pat))))
      pat)))

;;; When abs(x)<a is in the pattern, where a is a positive expression,
;;; then learn x<a and -x<a too. The additional facts are put into the context
;;; '$learndata, if the current context is user context 'initial

(defun learn-abs (pat flag)
  (let (tmp)
    (when (and (setq tmp (isinop pat 'mabs))
               (or (and (member (caar pat) '(mlessp mleqp))
                        (isinop (cadr pat) 'mabs)
                        (member ($sign (caddr pat)) '($pos $pz)))
                   (and (member (caar pat) '(mgreaterp mgeqp))
                        (member ($sign (cadr pat)) '($pos $pz))
                        (isinop (caddr pat) 'mabs))))
      (let ((oldcontext context))
        (if (eq oldcontext '$initial)
            (asscontext nil '$learndata)) ; switch to context '$learndata
        ; learn additional facts
        (learn ($substitute (cadr tmp) tmp pat) flag)
        (learn ($substitute (mul -1 (cadr tmp)) tmp pat) flag)
        (when (eq oldcontext '$initial)
          (asscontext nil oldcontext)     ; switch back to context on entry
          ($activate '$learndata))))      ; context '$learndata is active
    nil))

;;; The value of a constant expression which can be numerically evaluated is
;;; put into the context '$learndata.

(defun learn-numer (pat flag)
  (let (dum expr patnew)
    (do ((x (cdr pat) (cdr x)))
        ((null x) (setq patnew (reverse patnew)))
      (setq dum (constp (car x))
            expr (car x))
      (cond ((or (numberp (car x))
                 (ratnump (car x))))
            ((eq dum 'bigfloat)
             (if (prog2
                    (setq dum ($bfloat (car x)))
                    ($bfloatp dum))
                 (setq expr dum)))
            ((eq dum 'float)
             (if (and (setq dum (numer (car x)))
                      (numberp dum))
                 (setq expr dum)))
            ((and (member dum '(numer symbol) :test #'eq)
                  (prog2 
                     (setq dum (numer (car x)))
                     (or (null dum)
                         (and (numberp dum)
                              (prog2 
                                  (setq expr dum)
                                  (< (abs dum) 1.0e-6))))))
             (cond ($signbfloat
                    (and (setq dum ($bfloat (car x)))
                         ($bfloatp dum)
                         (setq expr dum))))))
      (setq patnew (cons expr patnew)))
    (setq patnew (cons (car pat) patnew))
    (when (and (not (alike (cdr pat) (cdr patnew)))
               (or (not (mnump (cadr patnew)))    ; not both sides of the
                   (not (mnump (caddr patnew))))) ; relation can be number
      (let ((oldcontext $context))
        (if (eq oldcontext '$initial)
          (asscontext nil '$learndata)) ; switch to context '$learndata
        (learn patnew flag)             ; learn additional fact
        (when (eq oldcontext '$initial) 
          (asscontext nil oldcontext)   ; switch back to context on entry
          ($activate '$learndata))))    ; context '$learndata is active
    nil))

(defmspec $forget (x)
  (setq x (cdr x))
  (do ((nl))
      ((null x) (cons '(mlist) (nreverse nl)))
    (cond ((atom (car x)) (push (forget (meval (car x))) nl))
	  ((eq 'mand (caaar x))
	   (mapc #'(lambda (l) (push (forget (meval l)) nl)) (cdar x)))
	  ((eq 'mnot (caaar x))
	   (push (forget (meval (pred-reverse (cadar x)))) nl))
	  ((eq 'mor (caaar x))
	   (merror (intl:gettext "forget: argument cannot be an 'or' expression; found ~M") (car x)))
	  (t (push (forget (meval (car x))) nl)))
    (setq x (cdr x))))

(defun forget (pat)
  (cond (($listp pat)
	 (cons '(mlist simp) (mapcar #'forget1 (cdr pat))))
	(t (forget1 pat))))

(defun forget1 (pat)
  (cond ((and (not (atom pat))
	      (eq (caar pat) 'mnot)
	      (eq (caaadr pat) '$equal))
	 (setq pat `(($notequal) ,@(cdadr pat)))))
  (learn pat nil))

(defun restore-facts (factl)		; used by SAVE
  (dolist (fact factl)
    (cond ((eq (caar fact) '$kind)
	   (declarekind (cadr fact) (caddr fact))
	   (add2lnc (getop (cadr fact)) $props))
	  ((eq (caar fact) '$par))
	  (t (assume fact)))))

(defmacro compare (a b)
  `(sign1 (sub* ,a ,b)))

(defun maximum (l)
  (maximin l '$max))

(defun minimum (l)
  (maximin l '$min))

(defmspec mand (form)
  (setq form (cdr form))
  (do ((l form (cdr l))
       (x)
       (unevaluated))
      ((null l)
       (cond ((= (length unevaluated) 0) t)
	     ((= (length unevaluated) 1) (car unevaluated))
	     (t (cons '(mand) (reverse unevaluated)))))
    (setq x (mevalp (car l)))
    (cond ((null x) (return nil))
	  ((not (member x '(t nil) :test #'eq)) (push x unevaluated)))))

(defmspec mor (form)
  (setq form (cdr form))
  (do ((l form (cdr l))
       (x)
       (unevaluated))
      ((null l)
       (cond ((= (length unevaluated) 0) nil)
	     ((= (length unevaluated) 1) (car unevaluated))
	     (t (cons '(mor) (reverse unevaluated)))))
    (setq x (mevalp (car l)))
    (cond ((eq x t) (return t))
	  ((not (member x '(t nil) :test #'eq)) (push x unevaluated)))))

(defmspec mnot (form)
  (setq form (cdr form))
  (let ((x (mevalp (car form))))
    (if (member x '(t nil) :test #'eq)
	(not x)
	`((mnot) ,x))))

;;;Toplevel functions- $askequal, $asksign, and $sign.
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

(defmfun $askequal (a b)
  (let ((answer (meqp (sratsimp a) (sratsimp b)))) ; presumably handles mbags and extended reals.
    (cond ((eq answer t) '$yes)
	  ((eq answer nil) '$no)
	  (t
	   (setq answer (retrieve `((mtext) ,(intl:gettext "Is ") ,a ,(intl:gettext " equal to ") ,b ,(intl:gettext "?")) nil))
	   (cond ((member answer '($no |$n| |$N|) :test #'eq)
		  (tdpn (sub b a))
		  '$no)
		 ((member answer '($yes |$y| |$Y|) :test #'eq)
		  (tdzero (sub a b))
		  '$yes)
		 (t  
		  (mtell (intl:gettext "Acceptable answers are yes, y, Y, no, n, N. ~%"))
		  ($askequal a b)))))))
	   
(defmfun $asksign (exp)
  (let (sign minus odds evens factored)
    (asksign01 (cond (limitp (restorelim exp))
		     ((among '$%i exp) ($rectform exp))
		     (t exp)))))

(defun asksign-p-or-n (e)
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
(defun csign (x)
  (or (not (free x '$%i))
      (let (sign-imag-errp limitp) (catch 'sign-imag-err ($sign x)))))

;;; $csign works like $sign but switches the sign-functions into a complex
;;; mode. In complex mode complex and imaginary expressions give the results
;;; imagarinary or complex.

(defmfun $csign (z)
  (let ((*complexsign* t)
        (limitp nil))
    ($sign z)))

(defmfun $sign (x)
  (let ((x (specrepcheck x))
	sign minus odds evens factored)
    (sign01 (cond (limitp (restorelim x))
		  (*complexsign*
		   ;; No rectform in Complex mode. Rectform ask unnecessary
		   ;; questions about complex expressions and can not handle
		   ;; imaginary expressions completely. Thus $csign can not
		   ;; handle something like (1+%i)*(1-%i) which is real.
		   ;; After improving rectform, we can change this. (12/2008)
		   (when *debug-compar*
		     (format t "~&$SIGN with ~A~%" x))
		   x)
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

;; don't ask about internal variables created by gruntz
(defun has-int-symbols (e)
  (cond ((and (symbolp e) (get e 'internal))
	 t)
	((atom e) nil)
	(t (or (has-int-symbols (car e))
	       (has-int-symbols (cdr e))))))

;;; Do substitutions for special symbols.
(defun nmr (a)
  (unless (free a '$zeroa) (setq a ($limit a '$zeroa 0 '$plus)))
  (unless (free a '$zerob) (setq a ($limit a '$zerob 0 '$minus)))
  (unless (free a 'z**) (setq a ($limit a 'z** 0 '$plus)))
  (unless (free a '*z*) (setq a ($limit a '*z* 0 '$plus)))
  (unless (free a 'epsilon) (setq a ($limit a 'epsilon 0 '$plus)))
  a)  ;;; Give A back.

;;; Get the sign of EPSILON-like terms.  Could be made MUCH hairier.
(defun eps-sign (b)
  (let (temp1 temp2 temp3 free1 free2 free3 limitp)
    ;; unset limitp to prevent infinite recursion
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
		 (t (merror (intl:gettext "asksign: internal error."))))))))

(defun eps-coef-sign (exp epskind)
  (let ((eps-power ($lopow exp epskind)) eps-coef)
    (cond ((and (not (equal eps-power 0))
		(not (equal (setq eps-coef (ratcoeff exp epskind eps-power))
			    0))
		(eq (ask-integer eps-power '$integer) '$yes))
	   (cond ((eq (ask-integer eps-power '$even) '$yes)
		  ($sign eps-coef))
		 ((eq (ask-integer eps-power '$odd) '$yes)
		  (setq eps-coef ($sign eps-coef))
		  (cond ((or (and (eq eps-coef '$pos)
				  (or (eq epskind 'epsilon)
				      (eq epskind '$zeroa)))
			     (and (eq eps-coef '$neg)
				  (or (alike epskind (mul2* -1 'epsilon))
				      (eq epskind '$zerob))))
			 '$pos)
			(t '$neg)))
		 (t (merror (intl:gettext "sign or asksign: insufficient information.")))))
	  (t (let ((deriv (sdiff exp epskind)) deriv-sign)
	       (cond ((not (eq (setq deriv-sign ($sign deriv)) '$zero))
		      (total-sign epskind deriv-sign))
		     ((not
		       (eq (let ((deriv (sdiff deriv epskind)))
			     (setq deriv-sign ($sign deriv)))
			   '$zero))
		      deriv-sign)
		     (t (merror (intl:gettext "sign or asksign: insufficient data.")))))))))

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
  (let ($radexpand)
    (sign1 $askexp))
  (cond
    ((has-int-symbols $askexp) '$pnz)
    ((member sign '($pos $neg $zero $imaginary) :test #'eq) sign)
    (t
     (let ((domain sign) (squared nil))
       (cond
         ((null odds)
          (setq $askexp (lmul evens)
                domain '$znz
                squared t))
         (t
          (if minus (setq sign (flip sign)))
          (setq $askexp
                (lmul (nconc odds (mapcar #'(lambda (l) (power l 2)) evens))))))
       (setq sign (cdr (assol $askexp *local-signs*)))
       (ensure-sign $askexp domain squared)))))

(defun match-sign (sgn domain expression squared)
  "If SGN makes sense for DOMAIN store the result (see ENSURE-SIGN) and return
it. Otherwise, return NIL. If SQUARED is true, we are actually looking for the
sign of the square, so any negative results are converted to positive."
  ;; The entries in BEHAVIOUR are of the form
  ;;   (MATCH DOMAINS REGISTRAR SIGN SIGN-SQ)
  ;;
  ;; The algorithm goes as follows:
  ;;
  ;; Look for SGN in MATCH. If found, use REGISTRAR to store SIGN for the
  ;; expression and then return SIGN if SQUARED is false or SIGN-SQ if it is
  ;; true.
  (let* ((behaviour
          '((($pos |$P| |$p| $positive) (nil $znz $pz $pn $pnz) tdpos $pos $pos)
            (($neg |$N| |$n| $negative) (nil $znz $nz $pn $pnz) tdneg $neg $pos)
            (($zero |$Z| |$z| 0 0.0) (nil $znz $pz $nz $pnz) tdzero $zero $zero)
            (($pn $nonzero $nz $nonz $non0) ($znz) tdpn $pn $pos)))
         (hit (find-if (lambda (bh)
                         (and (member sgn (first bh) :test #'equal)
                              (member domain (second bh) :test #'eq)))
                       behaviour)))
    (when hit
      (let ((registrar (third hit))
            (found-sign (if squared (fifth hit) (fourth hit))))
        (funcall registrar expression)
        (setq sign
              (if minus (flip found-sign) found-sign))))))

(defun ensure-sign (expr &optional domain squared)
  "Try to determine the sign of EXPR. If DOMAIN is not one of the special values
described below, we try to tell whether EXPR is positive, negative or zero. It
can be more specialised ($pz => positive or zero; $nz => negative or zero; $pn
=> positive or negative; $znz => zero or nonzero).

If SQUARED is true, then we're actually interested in the sign of EXPR^2. As
such, a nonzero sign should be regarded as positive.

When calling ENSURE-SIGN, set the special variable SIGN to the best current
guess for the sign of EXPR. The function returns the sign, calls one of (TDPOS
TDNEG TDZERO TDPN) to store it, and also sets SIGN."
  (loop
     (let ((new-sign (match-sign sign domain expr squared)))
       (when new-sign (return new-sign)))
     (setf sign (retrieve
                 (list '(mtext)
                       "Is " expr
                       (or (second
                            (assoc domain
                                   '(($znz " zero or nonzero?")
                                     ($pz  " positive or zero?")
                                     ($nz  " negative or zero?")
                                     ($pn  " positive or negative?"))))
                           " positive, negative or zero?"))
                 nil))))

;; During one evaluation phase asksign writes answers from the user into the
;; global context '$initial. These facts are removed by clearsign after
;; finishing the evaluation phase. clearsign is called from the top-level
;; evaluation function meval*. The facts which have to be removed are stored
;; in the global variable *local-signs*.

(defun clearsign ()
  (let ((context '$initial))
    (dolist (cons-pair *local-signs*)
      (destructuring-bind (x . sgn) cons-pair
        (cond
          ((eq '$pos sgn) (daddgr nil x))
          ((eq '$neg sgn) (daddgr nil (neg x)))
          ((eq '$zero sgn) (daddeq nil x))
          ((eq '$pn sgn) (daddnq nil x))
          ((eq '$pz sgn) (daddgq nil x))
          ((eq '$nz sgn) (daddgq nil (neg x))))))
    (setf *local-signs* nil)))

(defun like (x y)
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
;; for the sum indices, it's necessary to locally assign a new value to
;; niceindicespref.

(defun meqp-by-csign (z a b)
  (let (($niceindicespref `((mlist) ,(gensym) ,(gensym) ,(gensym))))
    (setq z ($niceindices z))
    (setq z (if ($constantp z) ($rectform z) (sratsimp z)))
    (let ((sgn ($csign z))
          (dunno `(($equal) ,a ,b)))
      (cond ((eq '$zero sgn) t)
            ((memq sgn '($pos $neg $pn)) nil)

            ;; previously checked also for (linearp z '$%i))
            ((memq sgn '($complex $imaginary))
             ;; We call trisplit here, which goes back to general evaluation and
             ;; could cause an infinite recursion. To make sure that doesn't
             ;; happen, use the with-safe-recursion macro.
             (handler-case
                 (with-safe-recursion meqp-by-csign z
                   (let* ((ri-parts (trisplit z))
                          (rsgn ($csign (car ri-parts)))
                          (isgn ($csign (cdr ri-parts))))
                     (cond ((and (eq '$zero rsgn)
                                 (eq '$zero isgn)) t)

                           ((or (memq rsgn '($neg $pos $pn))
                                (memq isgn '($neg $pos $pn))) nil)

                           (t dunno))))
               (unsafe-recursion () dunno)))

            (t dunno)))))

;; For each fact of the form equal(a,b) in the active context, do e : ratsubst(b,a,e).

(defun equal-facts-simp (e)
  (let ((f (margs ($facts))))
    (dolist (fi f e)
      (if (op-equalp fi '$equal)
	  (setq e ($ratsubst (nth 2 fi) (nth 1 fi) e))))))

(defun maxima-declared-arrayp (x)
  (and
   (symbolp x)
   (mget x 'array)
   (get (mget x 'array) 'array)))

(defun maxima-undeclared-arrayp (x)
  (and
   (symbolp x)
   (mget x 'hashar)
   (get (mget x 'hashar) 'array)))

(defun meqp (a b)
  ;; Check for some particular types before falling into the general case.
  (cond ((stringp a)
	 (and (stringp b) (equal a b)))
	((stringp b) nil)
	((arrayp a)
	 (and (arrayp b) (array-meqp a b)))
	((arrayp b) nil)
	((maxima-declared-arrayp a)
	 (and (maxima-declared-arrayp b) (maxima-declared-array-meqp a b)))
	((maxima-declared-arrayp b) nil)
	((maxima-undeclared-arrayp a)
	 (and (maxima-undeclared-arrayp b) (maxima-undeclared-array-meqp a b)))
	((maxima-undeclared-arrayp b) nil)
	(t
	 (let ((z) (sign))
	   (setq a (specrepcheck a))
	   (setq b (specrepcheck b))
	   (cond ((or (like a b)) (not (member a indefinites)))
		 ((or (member a indefinites) (member b indefinites)
		      (member a *infinities*) (member b *infinities*)) nil)
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
		 ;; 0 isn't in the range of an exponential function.
		 ((or (and (mexptp a) (not (eq '$minf (third a))) (zerop1 b) (eq t (mnqp (second a) 0)))
		      (and (mexptp b) (not (eq '$minf (third b))) (zerop1 a) (eq t (mnqp (second b) 0))))
		  nil)

		 ;; DCOMPARE emits new stuff (via DINTERNP) into the assume database.
		 ;; Let's avoid littering the database with numbers.
		 ((and (mnump a) (mnump b)) (zerop1 (sub a b)))

		 ;; lookup in assumption database
		 ((and (dcompare a b) (eq '$zero sign)))	; dcompare sets sign
		 ((memq sign '($pos $neg $pn)) nil)

		 ;; if database lookup failed, apply all equality facts
		 (t (meqp-by-csign (equal-facts-simp (sratsimp (sub a b))) a b)))))))

;; Two arrays are equal (according to MEQP)
;; if (1) they have the same dimensions,
;; and (2) their elements are MEQP.

(defun array-meqp (p q)
  (and
   (equal (array-dimensions p) (array-dimensions q))
   (progn
     (dotimes (i (array-total-size p))
       (let ((z (let ($ratprint)
		  (declare (special $ratprint))
		  (meqp (row-major-aref p i) (row-major-aref q i)))))
	 (cond ((eq z nil) (return-from array-meqp nil))
	       ((eq z t))
	       (t (return-from array-meqp `(($equal) ,p ,q))))))
     t)))

(defun maxima-declared-array-meqp (p q)
  (array-meqp (get (mget p 'array) 'array) (get (mget q 'array) 'array)))

(defun maxima-undeclared-array-meqp (p q)
  (and
   (alike1 (mfuncall '$arrayinfo p) (mfuncall '$arrayinfo q))
   (let ($ratprint)
     (declare (special $ratprint))
     (meqp ($listarray p) ($listarray q)))))

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
  (let ((aa (equal-facts-simp a))
	(bb (equal-facts-simp b)))
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

;; MGRP-GENERAL applies when sign of A - B (possibly complex) makes sense.

(defun mgrp-general (a b)
  (let ((*complexsign* t))
    (setq a (sub a b))
    (let ((sgn (csign a)))
      (cond ((eq sgn '$pos) t)
	    ((eq sgn t) nil) ;; csign thinks a - b isn't real
	    ((member sgn '($neg $zero $nz) :test #'eq) nil)
	    (t `((mgreaterp) ,a 0))))))

(defun mgrp (a b)
  (cond
    ((or (stringp a) (stringp b))
     (if (and (stringp a) (stringp b)) (not (null (string< b a)))
       (when $prederror
         (merror (intl:gettext "greater than: arguments are incomparable; found: ~:M, ~:M") a b))))
     (t (mgrp-general a b))))

(defun mlsp (x y)
  (mgrp y x))

;; MGQP-GENERAL applies when sign of A - B (possibly complex) makes sense.

(defun mgqp-general (a b)
  (let ((*complexsign* t))
    (setq a (sub a b))
    (let ((sgn (csign a)))
      (cond ((member sgn '($pos $zero $pz) :test #'eq) t)
	    ((eq sgn t) nil) ;; csign thinks a - b isn't real
	    ((eq sgn '$neg) nil)
	    (t `((mgeqp) ,a 0))))))

(defun mgqp (a b)
  (cond
    ((or (stringp a) (stringp b))
     (if (and (stringp a) (stringp b)) (not (null (string<= b a)))
       (when $prederror
         (merror (intl:gettext "greater than or equal: arguments are incomparable; found: ~:M, ~:M") a b))))
     (t (mgqp-general a b))))

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
	(t (setq e (mapcar #'(lambda (l) (power l 2)) e))
	   (list '(mgreaterp) (lmul (nconc o e)) 0))))

(defun c-$pz (o e)
  (cond ((null o) (list '(mnot) (list '($equal) (lmul e) 0)))
	((null e) (list '(mgeqp) (lmul o) 0))
	(t (setq e (mapcar #'(lambda (l) (power l 2)) e))
	   (list '(mgeqp) (lmul (nconc o e)) 0))))

(defun sign* (x)
  (let (sign minus odds evens)
    (sign1 x)))

(defun infsimp* (e)
  (if (or (atom e) (and (free e '$inf) (free e '$minf)))
      e
      (infsimp e)))

;; Like WITH-COMPSPLT, but runs COMPSPLT-EQ instead
(defmacro with-compsplt-eq ((lhs rhs x) &body forms)
  `(multiple-value-bind (,lhs ,rhs) (compsplt-eq ,x)
     ,@forms))

;; Call FORMS with LHS and RHS bound to the splitting of EXPR by COMPSPLT.
(defmacro with-compsplt ((lhs rhs expr) &body forms)
  `(multiple-value-bind (,lhs ,rhs) (compsplt ,expr)
     ,@forms))

(defun sign1 (x)
  (setq x (specrepcheck x))
  (setq x (infsimp* x))
  (when (and *complexsign* (atom x) (eq x '$infinity))
    ;; In Complex Mode the sign of infinity is complex.
    (when *debug-compar* (format t "~& in sign1 detect $infinity.~%"))
    (return-from sign1 '$complex))
  (if (member x '($und $ind $infinity) :test #'eq)
      (if limitp '$pnz (merror (intl:gettext "sign: sign of ~:M is undefined.") x)))
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
				  (< (abs dum) 1.0e-6))))))
	    (cond ($signbfloat
		   (and (setq dum ($bfloat x)) ($bfloatp dum) (setq exp dum)))
		  (t (setq sign '$pnz evens nil odds (ncons x) minus nil)
		     (return sign)))))
     (or (and (not (atom x)) (not (mnump x)) (equal x exp)
	      (let (s o e m)
                (with-compsplt (lhs rhs x)
                  (dcompare lhs rhs)
                  (cond ((member sign '($pos $neg) :test #'eq))
                        ((eq sign '$pnz) nil)
                        (t (setq s sign o odds e evens m minus)
                           (sign x)
                           (if (not (strongp sign s))
                               (if (and (eq sign '$pnz) (eq s '$pn))
                                   (setq sign s)
                                   (setq sign s odds o evens e minus m)))
                           t)))))
	 (sign exp))
     (return sign)))

(defun numer (x)
  (let (($numer t) ; currently, no effect on $float, but proposed to
        ($ratprint nil)
        result)
    ;; Catch a Lisp error, if a floating point overflow occurs.
    (setq result (let ((errset nil)) (errset ($float x))))
    (if result (car result) nil)))

(defun constp (x)
  (cond ((floatp x) 'float)
	((numberp x) 'numer)
	((symbolp x) (if (member x '($%pi $%e $%phi $%gamma) :test #'eq) 'symbol))
    ((atom x) nil)
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

(mapcar #'(lambda (s) (putprop (first s) (second s) 'sign-function))
	(list
	 (list 'mtimes 'sign-mtimes)
	 (list 'mplus 'sign-mplus)
	 (list 'mexpt 'sign-mexpt)
	 (list '%log 'sign-log)
	 (list 'mabs 'sign-mabs)
	 (list '$min #'(lambda (x) (sign-minmax (caar x) (cdr x))))
	 (list '$max #'(lambda (x) (sign-minmax (caar x) (cdr x))))
	 (list '%csc #'(lambda (x) (sign (inv* (cons (ncons (zl-get (caar x) 'recip)) (cdr x))))))
	 (list '%csch #'(lambda (x) (sign (inv* (cons (ncons (zl-get (caar x) 'recip)) (cdr x))))))

	 (list '%signum #'(lambda (x) (sign (cadr x))))
	 (list '%erf #'(lambda (x) (sign (cadr x))))
	 (list '$li #'(lambda (x) 
			(let ((z (first (margs x))) (n (cadadr x)))
			  (if (and (mnump n) (eq t (mgrp z 0)) (eq t (mgrp 1 z))) (sign z) (sign-any x)))))))
(defun sign (x)
  (cond ((mnump x) (setq sign (rgrp x 0) minus nil odds nil evens nil))
	((and *complexsign* (symbolp x) (eq x '$%i))
	 ;; In Complex Mode the sign of %i is $imaginary.
	 (setq sign '$imaginary))
	((symbolp x) (if (eq x '$%i) (imag-err x)) (sign-any x))
	((and (consp x) (symbolp (caar x)) (not (specrepp x)) (get (caar x) 'sign-function))
	 (funcall (get (caar x) 'sign-function) x))
	((and (consp x) (not (specrepp x)) ($subvarp (mop x)) (get (mop (mop x)) 'sign-function))
	 (funcall (get (mop (mop x)) 'sign-function) x))
	((specrepp x) (sign (specdisrep x)))
	((kindp (caar x) '$posfun) (sign-posfun x))
	((kindp (caar x) '$oddfun) (sign-oddfun x))
	(t (sign-any x))))

(defun sign-any (x)
  (cond ((and *complexsign*
              (symbolp x)
              (decl-complexp x))
         ;; In Complex Mode look for symbols declared to be complex.
         (if ($featurep x '$imaginary)
             (setq sign '$imaginary)
             (setq sign '$complex)))
        ((and *complexsign*
              (not (atom x))
              (decl-complexp (caar x)))
         ;; A function f(x), where f is declared to be imaginary or complex.
         (if ($featurep (caar x) '$imaginary)
             (setq sign '$imaginary)
             (setq sign '$complex)))
	(t
	 (dcompare x 0)
	 (if (and $assume_pos
		  (member sign '($pnz $pz $pn) :test #'eq)
		  (if $assume_pos_pred
		      (let ((*x* x))
			(declare (special *x*))
			(is '(($assume_pos_pred) *x*)))
		      (mapatom x)))
	     (setq sign '$pos))
	 (setq minus nil evens nil
	       odds (if (not (member sign '($pos $neg $zero) :test #'eq))
			(ncons x))))))

(defun sign-mtimes (x)
  (setq x (cdr x))
  (do ((s '$pos) (m) (o) (e)) ((null x) (setq sign s minus m odds o evens e))
    (sign1 (car x))
    (cond ((eq sign '$zero) (return t))
	  ((and *complexsign* (eq sign '$complex))
	   ;; Found a complex factor.  We don't return immediately
	   ;; because another factor could be zero.
	   (setq s '$complex))
	  ((and *complexsign* (eq s '$complex))) ; continue the loop
	  ((and *complexsign* (eq sign '$imaginary))
	   ;; Found an imaginary factor. Look if we have already one.
	   (cond ((eq s '$imaginary)
		  ;; imaginary*imaginary is real. But remember the sign in m.
		  (setq s (if m '$pos '$neg) m (not m)))
		 (t (setq s sign))))
	  ((and *complexsign* (eq s '$imaginary))) ; continue the loop
	  ((eq sign '$pos))
	  ((eq sign '$neg) (setq s (flip s) m (not m)))
	  ((prog2 (setq m (not (eq m minus)) o (nconc odds o) e (nconc evens e))
	       nil))
	  ((eq s sign) (when (eq s '$nz) (setq s '$pz)))
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
  (let ((swapped nil) (retval))
    (with-compsplt (lhs rhs x)
      (if (and (mplusp lhs) (equal rhs 0) (null (cdddr lhs)))
        (cond ((and (negp (cadr lhs)) (not (negp (caddr lhs))))
                (setq rhs (neg (cadr lhs)) lhs (caddr lhs)))
              ;; The following fixes SourceForge bug #3148
              ;; where sign(a-b) returned pnz and sign(b-a) returned pos.
			  ;; Previously, only the case (-a)+b was handled.
			  ;; Now we also handle a+(-b) by swapping lhs and rhs,
			  ;; setting a flag "swapped", running through the same code and
			  ;; then flipping the answer.
              ((and (negp (caddr lhs)) (not (negp (cadr lhs))))
                (setq rhs (cadr lhs) lhs (neg (caddr lhs)) swapped t))))
      (let (dum)
        (setq retval
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
                ((and (not (atom lhs)) (not (atom rhs))
                      (eq (caar lhs) (caar rhs))
                      (kindp (caar lhs) '$decreasing))
                 (sign (sub (cadr rhs) (cadr lhs)))
                 t)
                ((and (not (atom lhs)) (eq (caar lhs) 'mabs)
                      (alike1 (cadr lhs) rhs))
                 (setq sign '$pz minus nil odds nil evens nil) t)
                ((signdiff-special lhs rhs))))))
    (if swapped
      (setq sign (flip sign)))
    retval))

(defun signdiff-special (xlhs xrhs)
  ;; xlhs may be a constant
  (let ((sgn nil))
    (when (or (and (realp xrhs) (minusp xrhs)
		   (not (atom xlhs)) (eq (sign* xlhs) '$pos))
					; e.g. sign(a^3+%pi-1) where a>0
	      (and (mexptp xlhs)
		   ;; e.g. sign(%e^x-1) where x>0
		   (eq (sign* (caddr xlhs)) '$pos)
		   (or (and
			;; Q^Rpos - S, S<=1, Q>1
			(member (sign* (sub 1 xrhs)) '($pos $zero $pz) :test #'eq)
			(eq (sign* (sub (cadr xlhs) 1)) '$pos))
		       (and (not (eq $domain '$complex))
			;; Qpos ^ Rpos - Spos => Qpos - Spos^(1/Rpos)
			(eq (sign* (cadr xlhs)) '$pos)
			(eq (sign* xrhs) '$pos)
			(eq (sign* (sub (cadr xlhs)
					(power xrhs (div 1 (caddr xlhs)))))
			    '$pos))))
	      (and (mexptp xlhs) (mexptp xrhs)
		   ;; Q^R - Q^T, Q>1, (R-T) > 0
		   ;; e.g. sign(2^x-2^y) where x>y
		   (alike1 (cadr xlhs) (cadr xrhs))
		   (eq (sign* (sub (cadr xlhs) 1)) '$pos)
		   (eq (sign* (sub (caddr xlhs) (caddr xrhs))) '$pos)))
      (setq sgn '$pos))

    ;; sign(sin(x)+c)
    (when (and (not (atom xlhs))
	       (member (caar xlhs) '(%sin %cos))
	       (zerop1 ($imagpart (cadr xlhs))))
      (cond ((eq (sign* (add xrhs 1)) '$neg)	;; c > 1
	     (setq sgn '$pos))
	    ((eq (sign* (add xrhs -1)) '$pos)	;; c < -1
	     (setq sgn '$neg))
		((zerop1 (add xrhs 1))				;; c = 1
	     (setq sgn '$pz))
		((zerop1 (add xrhs -1))				;; c = -1
		 (setq sgn '$nz))))
	   
    (when (and $useminmax (or (minmaxp xlhs) (minmaxp xrhs)))
      (setq sgn (signdiff-minmax xlhs xrhs)))
    (when sgn (setq sign sgn minus nil odds nil evens nil)
	  t)))

;;; Look for symbols with an assumption a > n or a < -n, where n is a number.
;;; For this case shift the symbol a -> a+n in a summation and multiplication.
;;; This handles cases like a>1 and b>1 gives sign(a+b-2) -> pos.

(defun sign-shift (expr)
  (do ((l (cdr expr) (cdr l))
       (fl nil)
       (acc nil))
      ((null l) (addn acc nil))
    (cond ((symbolp (car l))
           ;; Get the facts related to the symbol (car l)
           ;; Reverse the order to test the newest facts first.
           (setq fl (reverse (cdr (facts1 (car l)))))
           (push (car l) acc)
           (dolist (f fl)
             (cond ((and (eq (caar f) 'mgreaterp)
                         (mnump (caddr f))
                         (eq ($sign (caddr f)) '$pos))
                    ;; The case a > n, where a is a symbol and n a number.
                    ;; Add the number to the list of terms.
                    (return (push (caddr f) acc)))
                   ((and (eq (caar f) 'mgreaterp)
                         (mnump (cadr f))
                         (eq ($sign (cadr f)) '$neg))
                    ;; The case a < -n, where a is a symbol and n a number.
                    ;; Add the number to the list of terms.
                    (return (push (cadr f) acc))))))
          ((mtimesp (car l))
           (let ((acctimes) (flag))
             ;; Go through the factors of the multiplication.
             (dolist (ll (cdar l))
               (cond ((symbolp ll)
                      ;; Get the facts related to the symbol (car l)
                      ;; Reverse the order to test the newest facts first.
                      (setq fl (reverse (cdr (facts1 ll))))
                      (dolist (f fl)
                        (cond ((and (eq (caar f) 'mgreaterp)
                                    (mnump (caddr f))
                                    (eq ($sign (caddr f)) '$pos))
                               ;; The case a > n, where a is a symbol and n a
                               ;; number. Add the number to the list of terms.
                               (setq flag t)
                               (return (push (add ll (caddr f)) acctimes)))
                              ((and (eq (caar f) 'mgreaterp)
                                    (mnump (cadr f))
                                    (eq ($sign (cadr f)) '$neg))
                               ;; The case a < -n, where a is a symbol and n a
                               ;; number. Add the number to the list of terms.
                               (setq flag t)
                               (return (push (add ll (cadr f)) acctimes)))))
                        (when (not flag) (push ll acctimes)))
                     (t
                      (push ll acctimes))))
             (if flag
                 ;; If a shift has been done expand the factors.
                 (push ($multthru (muln acctimes nil)) acc)
                 (push (muln acctimes nil) acc))))
          (t
           (push (car l) acc)))))

(defun signsum (x)
  (setq x (sign-shift x))
  ;; x might be simplified to an atom in sign-shift.
  (when (atom x) (setq x (cons '(mplus) (list x))))
  (do ((l (cdr x) (cdr l)) (s '$zero))
      ((null l) (setq sign s minus nil odds (list x) evens nil)
       (cond (*complexsign*
	      ;; Because we have continued the loop in Complex Mode
	      ;; we have to look for the sign '$pnz and return nil.
	      (if (eq s '$pnz) nil t))
	     (t t))) ; in Real Mode return T
    ;; Call sign1 and not sign, because sign1 handles constant expressions.
    (sign1 (car l))
    (cond ((and *complexsign*
		(or (eq sign '$complex) (eq sign '$imaginary)))
	   ;; Found a complex or imaginary expression. The sign is $complex.
	   (setq sign '$complex odds nil evens nil minus nil)
	   (return t))
	  ((or (and (eq sign '$zero)
		    (setq x (sub x (car l))))
	       (and (eq s sign) (not (eq s '$pn))) ; $PN + $PN = $PNZ
	       (and (eq s '$pos) (eq sign '$pz))
	       (and (eq s '$neg) (eq sign '$nz))))
	  ((or (and (member sign '($pz $pos) :test #'eq) (member s '($zero $pz) :test #'eq))
	       (and (member sign '($nz $neg) :test #'eq) (member s '($zero $nz) :test #'eq))
	       (and (eq sign '$pn) (eq s '$zero)))
	   (setq s sign))
	  (t
	   (cond (*complexsign*
		  ;; In Complex Mode we have to continue the loop to look further
		  ;; for a complex or imaginay expression.
		  (setq s '$pnz))
		 (t
		  ;; In Real mode the loop stops when the sign is 'pnz.
		  (setq sign '$pnz odds (list x) evens nil minus nil)
		  (return nil)))))))

(defun signfactor (x)
  (let (y (factored t))
    (setq y (factor-if-small x))
    (cond ((or (mplusp y) (> (conssize y) 50.))
	   (setq sign '$pnz)
	   nil)
	  (t (sign y)))))

(defun factor-if-small (x)
  (if (< (conssize x) 51.)
      (let ($ratprint)
	(declare (special $ratprint))
	(factor x)) x))

(defun sign-mexpt (x)
  (let* ((expt (caddr x)) (base1 (cadr x))
	 (sign-expt (sign1 expt)) (sign-base (sign1 base1))
	 (evod (evod expt)))
    (cond ((and *complexsign* (or (eq sign-expt '$complex)
				  (eq sign-expt '$imaginary)
				  (eq sign-base '$complex)))
	   ;; Base is complex or exponent is complex or imaginary.
	   ;; The sign is $complex.
	   (when *debug-compar*
	     (format t "~&in SIGN-MEXPT for ~A, sign is complex.~%" x))
	   (setq sign '$complex))

	  ((and *complexsign*
		(eq sign-base '$neg)
		(eq (evod ($expand (mul 2 expt))) '$odd))
	   ;; Base is negative and the double of the exponent is odd.
	   ;; Result is imaginary.
	   (when *debug-compar*
	     (format t "~&in SIGN-MEXPT for ~A, sign is $imaginary.~%" x))
	   (setq sign '$imaginary))

	  ((and *complexsign*
		(eq sign-base '$imaginary))
	   ;; An imaginary base. Look for even or odd exponent.
	   (when *debug-compar*
	     (format t "~&in SIGN-MEXPT for ~A, base is $imaginary.~%" x))
	   (cond
	     ((and (integerp expt) (eq evod '$even))
	      (setq sign (if (eql (mod expt 4) 0) '$pz '$nz)))
	     ((and (integerp expt) (eq evod '$odd))
	      (setq sign '$imaginary
		    minus (if (eql (mod (- expt 1) 4) 0) t nil)))
	     (t (setq sign '$complex))))

	  ((and (eq sign-base '$zero)
		(member sign-expt '($zero $neg) :test #'eq))
	   (dbzs-err x))
	  ((eq sign-expt '$zero) (setq sign '$pos))
	  ((eq sign-base '$pos))
	  ((eq sign-base '$zero))
	  ((eq evod '$even)
	   (cond ((eq sign-expt '$neg)
		  (setq sign '$pos minus nil evens (ncons base1) odds nil))
		 ((member sign-base '($pn $neg) :test #'eq)
		  (setq sign '$pos minus nil
			evens (nconc odds evens)
			odds nil))
		 (t (setq sign '$pz minus nil
			  evens (nconc odds evens)
			  odds nil))))
	  ((and (member sign-expt '($neg $nz) :test #'eq)
		(member sign-base '($nz $pz $pnz) :test #'eq))
	   (setq sign (cond ((eq sign-base '$pnz) '$pn)
			    ((eq sign-base '$pz) '$pos)
			    ((eq sign-expt '$neg) '$neg)
			    (t '$pn))))
	  ((member sign-expt '($pz $nz $pnz) :test #'eq)
	   (cond ((eq sign-base '$neg)
		  (setq odds (ncons x) sign '$pn))))
	  ((eq sign-expt '$pn))
	  ((ratnump expt)
	   (cond ((mevenp (cadr expt))
		  (cond ((member sign-base '($pn $neg) :test #'eq)
			 (setq sign-base '$pos))
			((member sign-base '($pnz $nz) :test #'eq)
			 (setq sign-base '$pz)))
		  (setq evens (nconc odds evens)
			odds nil minus nil))
		 ((mevenp (caddr expt))
		  (cond ((and *complexsign* (eq sign-base '$neg))
			 ;; In Complex Mode the sign is $complex.
			 (setq sign-base (setq sign-expt '$complex)))
			(complexsign
			 ;; The only place the variable complexsign
			 ;; is used. Unfortunately, one routine in
			 ;; to_poly.lisp in /share/to_poly_solve depends on
			 ;; this piece of code. Perhaps we can remove
			 ;; the dependency. (12/2008)
			 (setq sign-base (setq sign-expt '$pnz)))
			((eq sign-base '$neg) (imag-err x))
			((eq sign-base '$pn)
			 (setq sign-base '$pos))
			((eq sign-base '$nz)
			 (setq sign-base '$zero))
			(t (setq sign-base '$pz)))))
	   (cond ((eq sign-expt '$neg)
		  (cond ((eq sign-base '$zero) (dbzs-err x))
			((eq sign-base '$pz)
			 (setq sign-base '$pos))
			((eq sign-base '$nz)
			 (setq sign-base '$neg))
			((eq sign-base '$pnz)
			 (setq sign-base '$pn)))))
	   (setq sign sign-base))
	  ((eq sign-base '$pos)
	   (setq sign '$pos))
	  ((eq sign-base '$neg)
	   (if (eq evod '$odd)
	       (setq sign '$neg)
	     (setq sign (if *complexsign* '$complex '$pn)))))))

;;; Determine the sign of log(expr). This function changes the special variable sign.

(defun sign-log (x)
  (setq x (cadr x))
  (setq sign
	(cond ((eq t (mgrp x 0))
	       (cond ((eq t (mgrp 1 x)) '$neg)
		     ((eq t (meqp x 1)) '$zero);; log(1) = 0.
		     ((eq t (mgqp 1 x)) '$nz)
		     ((eq t (mgrp x 1)) '$pos)
		     ((eq t (mgqp x 1)) '$pz)
		     ((eq t (mnqp x 1)) '$pn)
		     (t '$pnz)))
	      ((and  *complexsign* (eql 1 (cabs x))) '$imaginary)
	      (*complexsign* '$complex)
	      (t '$pnz))))

(defun sign-mabs (x)
  (let ((*complexsign* t))
    (sign (cadr x))
    (cond ((member sign '($pos $zero) :test #'eq))
	  ((member sign '($neg $pn) :test #'eq) (setq sign '$pos))
	  (t (setq sign '$pz minus nil evens (nconc odds evens) odds nil)))))

;;; Compare min/max

;;; Macros used in simp min/max
;;; If op is min, use body; if not, negate sign constants in body
;;; Used to avoid writing min and max code separately: just write the min code
;;; in such a way that its dual works for max
(defmacro minmaxforms (op &rest body)
  `(if (eq ,op '$min)
       ,@body
       ,@(sublis '(($neg . $pos)
		   ($nz . $pz)
		   ($pz . $nz)
		   ($pos . $neg)
		   ;;($zero . $zero)
		   ;;($pn . $pn)
		   ;;($pnz . $pnz)
		   ;;
		   ($max . $min)
		   ($min . $max)
		   ;;
		   ($inf . $minf)
		   ($minf . $inf))
		 body)))

(defun sign-minmax (op args)
  (do ((sgn (minmaxforms op '$pos)	;identity element for min
	    (sminmax op sgn (sign* (car l))))
       (end (minmaxforms op '$neg))
       (l args (cdr l)))
      ((or (null l) (eq sgn end))
       (setq minus nil
	     odds (if (not (member sgn '($pos $neg $zero) :test #'eq))
		      (ncons (cons (list op) args)))
	     evens nil
	     sign sgn))))

;; sign(op(a,b)) = sminmax(sign(a),sign(b))
;; op is $min/$max; s1/s2 in neg, nz, zero, pz, pos, pn, pnz
(defun sminmax (op s1 s2)
  (minmaxforms
   op
   ;; Many of these cases don't come up in simplified expressions,
   ;; since e.g. sign(a)=neg and sign(b)=pos implies min(a,b)=a
   ;; the order of these clauses is important
   (cond ((eq s1 '$pos) s2)
	 ((eq s2 '$pos) s1)
	 ((eq s1 s2) s1)
	 ((or (eq s1 '$neg) (eq s2 '$neg)) '$neg)
	 ((or (eq s1 '$nz) (eq s2 '$nz)) '$nz)
	 ((eq s1 '$zero) (if (eq s2 '$pz) '$zero '$nz))
	 ((eq s2 '$zero) (if (eq s1 '$pz) '$zero '$nz))
	 (t '$pnz))))

(defun minmaxp (ex)
  (cond ((atom ex) nil)
	((member (caar ex) '($min $max) :test #'eq) (caar ex))
	(t nil)))

(defun signdiff-minmax (l r)
  ;; sign of l-r; nil if unknown (not PNZ)
  (let* ((lm (minmaxp l))
	 (rm (minmaxp r))
	 (ll (if lm (cdr l)))
	 (rr (if rm (cdr r)))) ;distinguish between < and <= argument lists of min/max
    (minmaxforms
     (or rm lm)
     (cond ((eq lm rm)			; min(a,...) - min(b,...)
	    (multiple-value-bind (both onlyl onlyr) (intersect-info ll rr)
	      (declare (ignore both))
	      (cond ((null onlyl) '$pz)	; min(a,b) - min(a,b,c)
		    ((null onlyr) '$nz)	; min(a,b,c) - min(a,b)
		    ;; TBD: add processing for full onlyl/onlyr case
		    (t nil))))
	   ;; TBD: memalike and set-disjointp are crude approx.
	   ((null lm) (if (memalike l rr) '$pz)) ; a - min(a,b)
	   ((null rm) (if (memalike r ll) '$nz)) ; min(a,b) - a
	   (t				; min/max or max/min
	    (if (not (set-disjointp ll rr)) '$pz)))))) ; max(a,q,r) - min(a,s,t)

(defun intersect-info (a b)
  (let ((both nil)
	(onlya nil)
	(onlyb nil))
    (do-merge-asym
	a b
	#'like
	#'$orderlessp
	#'(lambda (x) (push x both))
	#'(lambda (x) (push x onlya))
	#'(lambda (x) (push x onlyb)))
    (values
     (reverse both)
     (reverse onlya)
     (reverse onlyb))))

;;; end compare min/max

(defun sign-posfun (xx)
  (declare (ignore xx))
  (setq sign '$pos
	minus nil
	odds nil
	evens nil))

(defun sign-oddfun (x)
  (cond ((kindp (caar x) '$increasing)
         ; Take the sign of the argument
         (sign (cadr x)))
        ((kindp (caar x) '$decreasing)
         ; Take the sign of negative of the argument
         (sign (neg (cadr x))))
        (t
         ; If the sign of the argument is zero, then we're done (the sign of
         ; the function value is the same).  Otherwise, punt to SIGN-ANY.
         (sign (cadr x))
         (unless (eq sign '$zero)
           (sign-any x)))))

(defun imag-err (x)
  (if sign-imag-errp
      (merror (intl:gettext "sign: argument cannot be imaginary; found ~M") x)
      (throw 'sign-imag-err t)))

(defun dbzs-err (x)
  (merror (intl:gettext "sign: division by zero in ~M") x))

;; Return true iff e is an expression with operator op1, op2,...,or opn.

(defun op-equalp (e &rest op)
  (and (consp e) (consp (car e)) (some #'(lambda (s) (equal (caar e) s)) op)))

;; Return true iff the operator of a is a Maxima relation operator.

(defun mrelationp (a)
  (op-equalp a 'mlessp 'mleqp 'mequal 'mnotequal 'mgeqp 'mgreaterp))

;; This version of featurep applies ratdisrep to the first argument.  This
;; change allows things like featurep(rat(n),integer) --> true when n has
;; been declared an integer.

(defmfun $featurep (e ind)
  (setq e ($ratdisrep e))
  (cond ((not (symbolp ind))
         (merror 
           (intl:gettext "featurep: second argument must be a symbol; found ~M")
          ind))
        ;; Properties not related to the assume database.
        ((and (member ind opers) (safe-get e ind)))
        ((and (member ind '($evfun $evflag $bindtest $nonarray))
              (safe-get e (stripdollar ind))))
        ((and (eq ind '$noun)
              (safe-get e (stripdollar ind))
              t))
        ((and (member ind '($scalar $nonscalar $mainvar))
              (mget e ind)))
        ((and (eq ind '$feature)
              (member e $features)
              t))
        ((eq ind '$alphabetic)
         (dolist (l (coerce e 'list) t)
           (when (not (member l *alphabet*)) (return nil))))
        ;; Properties related to the assume database.
        ((eq ind '$integer) (maxima-integerp e))
        ((eq ind '$noninteger) (nonintegerp e))
        ((eq ind '$even) (mevenp e))
        ((eq ind '$odd) (moddp e))
        ((eq ind '$real)
         (if (atom e)
             (or (numberp e) (kindp e '$real) (numberp (numer e)))
             (free ($rectform e) '$%i)))
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
(setf (get '$charfun 'integer-valued) t)

(defun maxima-integerp (x)
  (cond ((integerp x))
	((mnump x) nil)
        ((and (symbolp x)
              (or (kindp x '$integer)
                  (kindp x '$even)
                  (kindp x '$odd)
                  (check-integer-facts x))))
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

;; When called with mode 'integer look into the database for symbols which are 
;; declared to be equal to an integer or an expression which is an integer.
;; In mode 'evod look for odd and even expressions.
(defun check-integer-facts (x &optional (mode 'integer))
  (do ((factsl (cdr (facts1 x)) (cdr factsl))
       fact)
      ((null factsl) nil)
    (setq fact (car factsl))
    (cond ((and (not (atom fact))
                (eq (caar fact) '$equal))
           (cond ((and (symbolp (cadr fact))
                       (eq (cadr fact) x))
                  ;; Case equal(x,expr): Test expr to be an integer.
                  (cond ((symbolp (caddr fact))
                         (cond ((and (eq mode 'integer)
                                     (or (kindp (caddr fact) '$integer)
                                         (kindp (caddr fact) '$odd)
                                         (kindp (caddr fact) '$even)))
                                (return t))
                               ((eq mode 'evod)
                                (cond ((kindp (caddr fact) '$odd)
                                       (return '$odd))
                                      ((kindp (caddr fact) '$even)
                                       (return '$even))
                                      (t (return nil))))
                               (t (return nil))))
                        (t
                         (cond ((eq mode 'integer)
                                (return (maxima-integerp (caddr fact))))
                               ((eq mode 'evod)
                                (return (evod (caddr fact))))
                               (t (return nil))))))
                 ((and (symbolp (caddr fact))
                       (eq (caddr fact) x))
                  ;; Case equal(expr,x): Test expr to be an integer.
                  (cond ((symbolp (caddr fact))
                         (cond ((and (eq mode 'integer)
                                     (or (kindp (cadr fact) '$integer)
                                         (kindp (cadr fact) '$odd)
                                         (kindp (cadr fact) '$even)))
                                (return t))
                               ((eq mode 'evod)
                                (cond ((kindp (cadr fact) '$odd)
                                       (return '$odd))
                                      ((kindp (cadr fact) '$even)
                                       (return '$even))
                                      (t (return nil))))
                               (t (return nil))))
                        (t
                         (cond ((eq mode 'integer)
                                (return (maxima-integerp (cadr fact))))
                               ((eq mode 'evod)
                                (return (evod (cadr fact))))
                               (t (return nil)))))))))))

(defun nonintegerp (e)
  (cond ((and (symbolp e) (or (kindp e '$noninteger) (check-noninteger-facts e) (kindp e '$irrational)))) ;declared noninteger
    ((mnump e)
     (if (integerp e) nil t)) ;all floats are noninteger and integers are not nonintegers
    (($ratp e)
     (nonintegerp ($ratdisrep e)))
    (t (eq t (mgrp e (take '($floor) e))))))


;; Look into the database for symbols which are declared to be equal 
;; to a noninteger or an expression which is a noninteger.
(defun check-noninteger-facts (x)
  (do ((factsl (cdr (facts1 x)) (cdr factsl)))
      ((null factsl) nil)
    (cond ((and (not (atom (car factsl)))
                (eq (caar (car factsl)) '$equal))
           (cond ((and (symbolp (cadr (car factsl)))
                       (eq (cadr (car factsl)) x))
                  ;; Case equal(x,expr): Test expr to be a noninteger.
                  (cond ((symbolp  (caddr (car factsl)))
                         (if (kindp (caddr (car factsl)) '$noninteger)
                             (return t)))
                        (t
                         (return (nonintegerp (caddr (car factsl)))))))
                 ((and (symbolp (caddr (car factsl)))
                       (eq (caddr (car factsl)) x))
                  ;; Case equal(expr,x): Test expr to be a noninteger.
                  (cond ((symbolp  (cadr (car factsl)))
                         (if (kindp (cadr (car factsl)) '$noninteger)
                             (return t)))
                        (t
                         (return (nonintegerp (cadr (car factsl))))))))))))

(defun intp (l)
  (every #'maxima-integerp (cdr l)))

(defun mevenp (e)
  (cond ((integerp e) (not (oddp e)))
	((mnump e) nil)
	(t (eq '$even (evod e)))))

(defun moddp (e)
  (cond ((integerp e) (oddp e))
	((mnump e) nil)
	(t (eq '$odd (evod e)))))

;; An extended evod that recognizes that abs(even) is even and
;; abs(odd) is odd.

(defun evod (e)
  (cond ((integerp e) (if (oddp e) '$odd '$even))
	((mnump e) nil)
        ((atom e)
         (cond ((kindp e '$odd) '$odd)
	       ((kindp e '$even) '$even)
	       ;; Check the database for facts.
	       ((symbolp e) (check-integer-facts e 'evod))))
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
	((eq '$pn (sel x +labs)) (dls x y))
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

;; mark sign of x to be m, relative to current comparison point for dcomp.
;; returns true if this fact is already known, nil otherwise.
(defun dmark (x m)
  (cond ((eq m (sel x +labs)))
	((and dbtrace (prog1
			  t
			(mtell (intl:gettext "DMARK: marking ~M ~M") (if (atom x) x (car x)) m))
	      nil))
	(t
	 (push x +labs)
	 (push+sto (sel x +labs) m)
	 nil)))

(defun daddgr (flag x)
  (with-compsplt (lhs rhs x)
    (mdata flag 'mgrp (dintern lhs) (dintern rhs))
    (if (or (mnump lhs) (constant lhs))
        (list '(mlessp) rhs lhs)
        (list '(mgreaterp) lhs rhs))))

(defun daddgq (flag x)
  (with-compsplt (lhs rhs x)
    (mdata flag 'mgqp (dintern lhs) (dintern rhs))
    (if (or (mnump lhs) (constant lhs))
        (list '(mleqp) rhs lhs)
        (list '(mgeqp) lhs rhs))))

(defun daddeq (flag x)
  (with-compsplt-eq (lhs rhs x)
    (mdata flag 'meqp (dintern lhs) (dintern rhs))
    (list '($equal) lhs rhs)))

(defun daddnq (flag x)
  (with-compsplt-eq (lhs rhs x)
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

;; The following functions are used by asksign to write answers into the 
;; database. We make sure that these answers are written into the global 
;; context '$initial and not in a local context which might be generated during
;; the evaluation phase and which will be destroyed before the evaluation has 
;; finshed.
;; The additional facts are removed from the global context '$initial after 
;; finishing the evaluation phase of meval with a call to clearsign.

(defun tdpos (x)
  (let ((context '$initial))
    (daddgr t x)
    (push (cons x '$pos) *local-signs*)))

(defun tdneg (x)
  (let ((context '$initial))
    (daddgr t (neg x))
    (push (cons x '$neg) *local-signs*)))

(defun tdzero (x)
  (let ((context '$initial))
    (daddeq t x)
    (push (cons x '$zero) *local-signs*)))

(defun tdpn (x)
  (let ((context '$initial))
    (daddnq t x)
    (push (cons x '$pn) *local-signs*)))

(defun tdpz (x)
  (let ((context '$initial))
    (daddgq t x)
    (push (cons x '$pz) *local-signs*)))

(defun compsplt-eq (x)
  (with-compsplt (lhs rhs x)
    (when (equal lhs 0)
      (setq lhs rhs rhs 0))
    (if (and (equal rhs 0)
             (or (mexptp lhs)
                 (and (not (atom lhs))
                      (kindp (caar lhs) '$oddfun)
                      (kindp (caar lhs) '$increasing))))
        (setq lhs (cadr lhs)))
    (values lhs rhs)))

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

;; To guess from the previous incarnation of this code,
;; each argument is assumed to be a float, bigfloat, integer, or Maxima rational.
;; Convert Maxima rationals to Lisp rationals to make them comparable to others.

(defun rgrp (x y)
  (when (or ($bfloatp x) ($bfloatp y))
    (setq
          x (let (($float2bf t))
              (declare (special $float2bf))
              (cadr ($bfloat (sub x y))))
          y 0))
  (if (not (numberp x))
    (setq x (/ (cadr x) (caddr x))))
  (if (not (numberp y))
    (setq y (/ (cadr y) (caddr y))))
  (cond
    (#-ecl (> x y) #+ecl (> (- x y) 0) '$pos)
    (#-ecl (> y x) #+ecl (> (- y x) 0) '$neg)
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

(defun declarekind (var prop)	; This function is for $DECLARE to use.
  (let (prop2)
    (cond ((truep (list 'kind var prop)) t)
	  ((or (falsep (list 'kind var prop))
	       (and (setq prop2 (assoc prop '(($integer . $noninteger)
					      ($noninteger . $integer)
					      ($increasing . $decreasing)
					      ($decreasing . $increasing)
					      ($symmetric . $antisymmetric)
					      ($antisymmetric . $symmetric)
					      ($rational . $irrational)
					      ($irrational . $rational)
					      ($oddfun . $evenfun)
					      ($evenfun . $oddfun)) :test #'eq))
		    (truep (list 'kind var (cdr prop2)))))
	   (merror (intl:gettext "declare: inconsistent declaration ~:M") `(($declare) ,var ,prop)))
	  (t (mkind var prop) t))))

;;;  These functions reformat expressions to be stored in the data base.

;; Return a list of all the atoms in X that aren't either numbers or constants
;; whose numerical value we know.
(defun unknown-atoms (x)
  (let (($listconstvars t))
    (remove-if (lambda (sym) (mget sym '$numer))
               (cdr ($listofvars x)))))

;; Rewrite a^b to a simpler expression that has the same sign:
;; If b is odd or 1/b is odd, remove the exponent, e.g. x^3 becomes x.
;; If b has a negative sign, return a^-b, e.g. 1/x^a becomes x^a.
;; Otherwise, do nothing.
(defun rewrite-mexpt-retaining-sign (x)
  (if (mexptp x)
    (let ((base (cadr x)) (exponent (caddr x)))
      (cond ((or (eq (evod exponent) '$odd) (eq (evod (inv exponent)) '$odd)) base)
	    ((negp exponent) (inv x))
	    (t x)))
    x))

;; COMPSPLT
;;
;; Split X into (values LHS RHS) so that X>0 <=> LHS > RHS. This is supposed to
;; be a canonical form for X that can be stored in the database and then looked
;; up in future.
;;
;; This uses two worker routines: COMPSPLT-SINGLE and COMPSPLT-GENERAL. The
;; former assumes that X only contains one symbol whose value is not known (eg not %e,
;; %pi etc.). The latter tries to deal with arbitrary collections of variables.
(defun compsplt (x)
  (multiple-value-bind (lhs rhs) 
    (cond
      ((or (atom x) (atom (car x))) (values x 0))
      ((null (cdr (unknown-atoms x))) (compsplt-single x))
      (t (compsplt-general x)))
	(let ((swapped nil))
	  ;; If lhs is zero, swap lhs and rhs to make the following code simpler.
	  ;; Remember that they were swapped to swap them back afterwards.
      (when (equal lhs 0)
        (setq lhs rhs rhs 0 swapped t))
      (when (equal rhs 0)
	    ;; Rewrite mexpts in factors so that e.g. x^3/y>0 becomes x*y>0. */
	    (setq lhs
		  (if (mtimesp lhs)
		    (cons (car lhs) (mapcar #'rewrite-mexpt-retaining-sign (cdr lhs)))
			(rewrite-mexpt-retaining-sign lhs))))
	  ;; Undo swapping lhs and rhs.
	  (if swapped
	    (values rhs lhs)
        (values lhs rhs)))))

(defun compsplt-single (x)
  (do ((exp (list x 0)) (success nil))
      ((or success (symbols (cadr exp))) (values (car exp) (cadr exp)))
    (cond ((atom (car exp)) (setq success t))
	  ((eq (caaar exp) 'mplus)  (setq exp (splitsum exp)))
	  ((eq (caaar exp) 'mtimes) (setq exp (splitprod exp)))
	  (t (setq success t)))))

(defun compsplt-general (x)
  ;; Let compsplt-single work on it first to treat constant factors/terms.
  (multiple-value-bind (lhs rhs) (compsplt-single x) (setq x (sub lhs rhs)))
  (cond
    ;; If x is an atom or a single level list then we won't change it any.
    ((or (atom x) (atom (car x)))
     (values x 0))
    ;; If x is a negative expression but not a sum, then get rid of the
    ;; negative sign.
    ((negp x) (values 0 (neg x)))
    ;; If x is not a sum, or is a sum with more than 2 terms, or has some
    ;; symbols common to both summands, then do nothing.
    ((or (cdddr x)
         (not (eq (caar x) 'mplus))
         (intersect* (symbols (cadr x)) (symbols (caddr x))))
     (values x 0))
    ;; -x + y gives (y, x)
    ((and (or (negp (cadr x)) (mnump (cadr x)))
          (not (negp (caddr x))))
     (values (caddr x) (neg (cadr x))))
    ;; x - y gives (x, y)
    ((and (not (negp (cadr x)))
          (or (negp (caddr x)) (mnump (caddr x))))
     (values (cadr x) (neg (caddr x))))
    ;; - x - y gives (0, x+y)
    ((and (negp (cadr x)) (negp (caddr x)))
     (values 0 (neg x)))
    ;; Give up! (x, 0)
    (t
     (values x 0))))

(defun negp (x)
  (and (mtimesp x) (mnegp (cadr x))))

(defun splitsum (exp)
  (do ((llist (cdar exp) (cdr llist))
       (lhs1 (car exp))
       (rhs1 (cadr exp)))
      ((null llist)
       (if (mplusp lhs1) (setq success t))
       (list lhs1 rhs1))
    (cond ((member '$inf llist :test #'eq)
	   (setq rhs1 (add2 '$inf (sub* rhs1 (addn llist t)))
		 lhs1 (add2 '$inf (sub* lhs1 (addn llist t)))
		 llist nil))
	  ((member '$minf llist :test #'eq)
	   (setq rhs1 (add2 '$minf (sub* rhs1 (addn llist t)))
		 lhs1 (add2 '$minf (sub* lhs1 (addn llist t)))
		 llist nil))
	  ((null (symbols (car llist)))
	   (setq lhs1 (sub lhs1 (car llist))
		 rhs1 (sub rhs1 (car llist)))))))

(defun splitprod (exp)
  (do ((flipsign)
       (lhs1 (car exp))
       (rhs1 (cadr exp))
       (llist (cdar exp) (cdr llist))
       (sign)
       (minus)
       (evens)
       (odds))
      ((null llist)
       (if (mtimesp lhs1) (setq success t))
       (cond (flipsign
              (setq success t)
              (list rhs1 lhs1))
	     (t (list lhs1 rhs1))))
    (when (null (symbols (car llist)))
      (sign (car llist))
      (if (eq sign '$neg) (setq flipsign (not flipsign)))
      (if (member sign '($pos $neg) :test #'eq)
	  (setq lhs1 (div lhs1 (car llist)) 
	        rhs1 (div rhs1 (car llist)))))))

(defun symbols (x)
  (let (($listconstvars %initiallearnflag))
    (cdr ($listofvars x))))

;; %initiallearnflag is only necessary so that %PI, %E, etc. can be LEARNed.

(defun initialize-numeric-constant (c)
  (setq %initiallearnflag t)
  (let ((context '$global))
    (learn `((mequal) ,c ,(mget c '$numer)) t))
  (setq %initiallearnflag nil))

(eval-when (:load-toplevel :execute)
  (mapc #'true*
	'(;; even and odd are integer
	  (par ($even $odd) $integer)

; Cutting out inferences for integer, rational, real, complex (DK 10/2009).
;	  (kind $integer $rational)
;	  (par ($rational $irrational) $real)
;	  (par ($real $imaginary) $complex)
	  
	  ;; imaginary is complex
	  (kind $imaginary $complex)
	  
          ;; Declarations for constants
          (kind $%i     $noninteger)
          (kind $%i     $imaginary)
          (kind $%e     $noninteger)
          (kind $%e     $real)
          (kind $%pi    $noninteger)
          (kind $%pi    $real)
          (kind $%gamma $noninteger)
          (kind $%gamma $real)
          (kind $%phi   $noninteger)
          (kind $%phi   $real)
          (kind $%pi $irrational)
	  (kind $%e $irrational)
	  (kind $%phi $irrational)
	  
          ;; Declarations for functions
	  (kind %log $increasing)
	  (kind %atan $increasing) (kind %atan $oddfun)
	  (kind $delta $evenfun)
	  (kind %sinh $increasing) (kind %sinh $oddfun)
	  (kind %cosh $posfun)
	  (kind %tanh $increasing) (kind %tanh $oddfun)
	  (kind %coth $oddfun)
	  (kind %csch $oddfun)
	  (kind %sech $posfun)
	  (kind %asinh $increasing) (kind %asinh $oddfun)
	  ;; It would be nice to say %acosh is $posfun, but then
	  ;; assume(xn<0); abs(acosh(xn)) -> acosh(xn), which is wrong
	  ;; since acosh(xn) is complex.
	  (kind %acosh $increasing)
	  (kind %atanh $increasing) (kind %atanh $oddfun)
	  (kind $li $complex)
	  (kind $lambert_w $complex)
	  (kind %cabs $complex)))

  ;; Create an initial context for the user which is a subcontext of $global.
  ($newcontext '$initial))
