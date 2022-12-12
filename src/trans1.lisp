;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       1001 TRANSLATE properties for everyone.                        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;       Maintained by GJC                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;; This file handles System FSUBR translation properties that were not handled in TRANSL. 

(macsyma-module trans1)

;;;;;;;; THE FOLLOWING ARE MOSTLY FROM JPG MLISP ;;;;;;;;;;;;;;;;;;;;;

;;; APPLY(F,[X]) is an idiom for funcall.

(defun quoted-symbolp (form)
  (and (consp form)
     (eq 'quote (car form))
     (symbolp (cadr form))))

(def%tr $apply (form)
  (let* ((fun (dtranslate (cadr form)))
	 (mode (cond ((symbolp fun)
		      (function-mode-@ fun))
		     ((quoted-symbolp fun)
		      (function-mode (cadr fun)))
		     ('else
		      '$any))))
    (cond (($listp (caddr form))
	   (let ((args (tr-args (cdr (caddr form)))))
	     (call-and-simp mode
			    'mfuncall
			    `(,fun ,@args))))
	  (t
	   (let ((arg (dtranslate (caddr form))))
	     (call-and-simp mode 'mapply-tr
			    `(,fun ,arg)))))))

(def%tr $map (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'map1 `((getopr ,fun) . ,args))))

(def%tr $maplist (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    `($any . (maplist_tr ,fun ,@args))))

(def%tr $fullmap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'fmap1 `((getopr ,fun) (list . ,args) nil))))

(def%tr $matrixmap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any `(lambda (fmaplvl)
			   (fmapl1 (getopr ,fun) . ,args))
		   '(2))))
		       
(def%tr $fullmapl (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'fmapl1 `((getopr ,fun) . ,args))))

(def%tr $outermap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any (cond ((= (length args) 1) 'fmapl1)
			       (t 'outermap1))
		   `((getopr ,fun)  ,@args))))


(def%tr $scanmap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'scanmap1 `((getopr ,fun) ,@args))))

(def%tr $qput (form)
  `($any $put ',(cadr form) ',(caddr form) ',(cadddr form)))

(def%tr $subvar (form)
  (translate (cons '(mqapply array) (cdr form))))

;;; If the evaluation of the first argument does not depend on the
;;; setting of the special variable PIECE, then it need not be 
;;; evaluated inside of PART1. If the PIECE feature is used, then
;;; we must send down an expression to PART1 which when evaluated has
;;; the proper environment for the compiled-away variable names in the
;;; environment of the calling function. 
;;; It is possible to get unbelivebly strange results from the order of
;;; evaluation of the arguments to $SUBSTPART, these crocks shall not
;;; be supported.
;;; The PIECE feature is not as often used as say,
;;; SUBSTPART("*",EXP,0) is.

(def%tr $substpart (form)
  (substpart-translation form t nil '$inflag '$substpart))

(def%tr $substinpart (form)
  (substpart-translation form t nil t '$substinpart))

(defun for-eval-then-mquote-simp-argl (l)
  ;;       (MAPCAR #'(LAMBDA (U) ;;; consing not important here.
  ;;			 `(LIST '(MQUOTE SIMP) ,U))
  ;;	       L)
  ;; JONL broke the fucking compiler. So I re-write this as=>
  (prog (v)
   loop
   (if (null l) (return (nreverse v)))
   (push `(list '(mquote simp) ,(pop l)) v)
   (go loop)))

(defun substpart-translation (form flag1 flag2 flag3 fn)
  (let* ((subst-item (dtranslate (cadr form)))
	 (freevars (free-lisp-vars subst-item))
	 (argl (tr-args (cddr form))))
    (cond ((null (assoc '$piece freevars :test #'eq))
					; this code is just to screw the people who
					; would use $PIECE non lexicaly. Not really, the
					; closure hacking is a lot slower at run time than
					; this easy case, so no sense screwing the people who
					; don't use $PIECE in foolish ways.
	   `($any . (simplify
		     (part1
		      (list  ,@(for-eval-then-mquote-simp-argl
				(cons subst-item argl)))

		      ,flag1 ,flag2 ,flag3 ',fn))))
	  (t
	   (setq freevars (tbound-free-vars freevars))
	   (side-effect-free-check (cadr freevars) (cadr form))
	   `($any . (simplify
		     (part1 (list (fungen&env-for-meval
				   ,(delete '$piece (car freevars) :test #'equal)
				   ($piece) ,subst-item)
				  ,@(for-eval-then-mquote-simp-argl argl))
		      ,flag1 ,flag2 ,flag3 ',fn)))))))

(def%tr $errcatch (form)
  (destructuring-bind (mode . body) (translate `((mprogn) ,@(cdr form)))
    (declare (ignore mode))
    (cons '$any `(cons '(mlist) (errcatch ,body)))))

;;; The MODE of a CATCH could either be the MODE of the last of the PROGN
;;; or the mode of the THROW. The THROW may be hard to find, so this goes
;;; on the assumption that the mode of the PROGN is enough to tell.

(def%tr $catch (form)
  (destructuring-bind (mode . body) (translate `((mprogn) . ,(cdr form)))
    (cons mode `(mcatch ,body))))

(def%tr $throw (form)
  (destructuring-bind (mode . body) (translate (cadr form))
    (cons mode `($throw ,body))))

;;; Makelist is a very sorry FSUBR. All these FSUBRS are just to avoid
;;; writing LAMBDA. But lots of users use MAKELIST now. 
;;; MAKELIST(EXP,X,0,N) with 4 args it is an iteration, with three it
;;; is a mapping over a list (the third argument).

(def%tr $makelist (form)
  (setq form (cdr form))
  (cond 
    ((= (length form) 0) '($any . '((mlist))))
    ((= (length form) 1)
     (destructuring-let
      (((exp) form))
      `($any . (list '(mlist) ,(cdr (tr-local-exp exp))))))
    ((= (length form) 2)
     (destructuring-let
      (((exp n) form) (sum (tr-gensym)) (nn (tr-gensym)) (|0| (tr-gensym)))
      (setq n (dtranslate n))
      `($any .
             ((lambda (,nn)
                (setq ,nn ($float ,nn))
                (if (numberp ,nn)
                    (do ((,|0| 1 (add 1 ,|0|)) (,sum nil))
                        ((> ,|0| ,nn) (cons '(mlist) ,sum))
                      (setq ,sum
                            (cons ,(cdr (tr-local-exp exp)) ,sum)))
                    (merror
                     (intl:gettext "makelist: second argument must evaluate to a number; found: ~M") ,nn)))
              ,n))))
    ((= (length form) 3)
     (destructuring-let
      (((exp x n) form) (sum (tr-gensym)) (nn (tr-gensym)) (lil (tr-gensym)))
      (setq n (dtranslate n))
      `($any .
             ((lambda (,nn)
                (if ($listp ,nn)
                   (do ((,lil (cdr ,nn) (cdr ,lil))
                        (,sum nil) (,x))
                       ((null ,lil) (cons '(mlist) (nreverse ,sum)))
                     (setq
                      ,x (car ,lil)
                      ,sum 
                      (cons ,(cdr (tr-local-exp exp x (value-mode x))) ,sum)))
                   (progn
                     (setq ,nn ($float ,nn))
                     (if (numberp ,nn)
                         (do ((,x 1 (add 1 ,x))
                              (,sum nil
                                    (cons
                                     ,(cdr (tr-local-exp exp x (value-mode x)))
                                     ,sum)))
                             ((> ,x ,nn)
                              (cons '(mlist) (nreverse ,sum)))
                           (declare (special ,x)))
                         (merror
                          (intl:gettext "makelist: third argument must be a number or a list; found: ~M") ,nn)))))
              ,n))))
    ((= (length form) 4)
     (destructuring-let
      (((exp x |0| n) form) (|00| (tr-gensym)) (nn (tr-gensym))
       (sum (tr-gensym)) (ii (tr-gensym)))
      (setq |0| (dtranslate |0|) n (dtranslate n))
      `($any .
             ((lambda (,|00| ,nn)
                (setq ,nn ($float (sub ,nn ,|00|)))
                (if (numberp ,nn)
                    (do ((,x ,|00| (add 1 ,x)) (,ii 0 (add 1 ,ii))
                         (,sum nil
                               (cons
                                ,(cdr (tr-local-exp exp x (value-mode x)))
                                ,sum)))
                        ((> ,ii ,nn) (cons '(mlist) (nreverse ,sum)))
                      (declare (special ,x)))
                    (merror
                     (intl:gettext "makelist: the fourth argument minus the third one must evaluate to a number; found: ~M")
                     ,nn)))
              ,|0| ,n))))
    ((= (length form) 5)
     (destructuring-let
      (((exp x |0| n s) form) (|00| (tr-gensym)) (nn (tr-gensym))
       (ss (tr-gensym)) (sum (tr-gensym)) (ii (tr-gensym)))
      (setq |0| (dtranslate |0|) n (dtranslate n) s (dtranslate s))
      `($any .
             ((lambda (,|00| ,nn ,ss)
                (setq ,nn ($float (div (sub ,nn ,|00|) ,ss)))
                (if (numberp ,nn)
                    (do ((,x ,|00| (add ,ss ,x)) (,ii 0 (add 1 ,ii))
                         (,sum nil
                               (cons
                                ,(cdr (tr-local-exp exp x (value-mode x)))
                                ,sum)))
                        ((> ,ii ,nn) (cons '(mlist) (nreverse ,sum)))
                      (declare (special ,x)))
                    (merror
                     (intl:gettext "makelist: the fourth argument minus the third one, divided by the fifth one must evaluate to a number; found: ~M")
                     ,nn)))
              ,|0| ,n ,s))))
    (t
     (tr-format (intl:gettext "makelist: maximum 5 arguments allowed; found: ~M.~%makelist: to create a list with sublists, use nested makelist commands.~%")
                (length form))
     (tr-abort)
     '($any . '$**error**))))

(def%tr $kill (form)
  `($any . (mapply '$kill ',(cdr form) nil)))

;;; Macsyma arrays are the biggest crock since STATUS PUNT NIL days.
;;; The basic idea of ARRAY(<frob>,type,dims...) is that
;;; if type is of
;;; (ASSoc (CADR X) '(($COMPLETE . T) ($INTEGER . FIXNUM) ($FIXNUM . FIXNUM)
;;;			  ($FLOAT . FLONUM) ($FLONUM . FLONUM)))
;;; then the dims are evaluated. But, if type is not one of those,
;;; it "must" be a dim spec! Of course, I must make this "analysis"
;;; at translate time, in order to preserve referential transparency
;;; in compiled code.

(def%tr $array (form)
  (setq form (cdr form))
  (let ((name (car form))
	(specp (assoc (cadr form)
		     '(($complete . t) ($integer . fixnum) ($fixnum . fixnum)
		       ($float . flonum) ($flonum . flonum)) :test #'eq)))
    (cond
      (specp
        `($any . (mapply
                   '$array
                   (list ',name ',(cadr form) ,@(tr-args (cddr form)))
                   '$array)))
	  (t
        `($any . (mapply
                   '$array
                   (list ',name ,@(tr-args (cdr form)))
                   '$array))))))


;;; it seems TRANSL has all sorts of code for hacking some kind of
;;; $CRE mode. somehow there is no translate property for MRAT. who
;;; knows. anyway here is something in the mean time before this
;;; I have time to do up TRANSL correctly.
;;;(DEFUN MRATEVAL (X)
;;; ((LAMBDA (VARLIST)
;;;   (COND (EVP (MEVAL ($RATDISREP X)))
;;;	 ((OR (AND $FLOAT $KEEPFLOAT) (NOT (ALIKE VARLIST (MAPCAR 'MEVAL VARLIST))))
;;;	  (RATF (MEVAL ($RATDISREP X))))
;;;	 (T X)))
;;;  (CADDAR X)))
;;; EVP is a hack for $EV I think. The MEVAL down the varlist is to see if the
;;; variables have any values, if not, then the result of (ratf (meval ($ratdisrep)))
;;; will be alike to what you started with, so it is an efficiency hack! What a
;;; joke!
;;;(DEFPROP MRAT (LAMBDA (X) (MRATEVAL X)) MFEXPR*)

(def%tr mrat (form)
  (let ((t-form (translate ($ratdisrep form))))
    (cond ((member (car t-form) '($float $fixnum $number) :test #'eq) t-form)
	  (t `($any . (ratf ,(cdr t-form)))))))


;;; The following special forms do not call the evaluator.

(def%tr $alias (form)
  (punt-to-meval form))

;;most of these will lose in common since a local variable will not
;;have its value accessible to the mfexpr*.  They should
;;be redone as macros with any necessary info passed along.

(def%tr $batch $alias)
(def%tr $batchload $alias)
(def%tr $closefile $alias)
(def%tr $compfile $alias)
(def%tr $declare $alias)
(def%tr $defstruct $alias)
(def%tr $demo $alias)
(def%tr $dependencies $alias)
(def%tr $describe $alias)
(def%tr $dispfun $alias)
(def%tr $disprule $alias)
(def%tr $fundef $alias)
(def%tr $gradef $alias)
(def%tr $labels $alias)
(def%tr $loadarrays $alias)
(def%tr $loadfile $alias)
(def%tr $new $alias)
(def%tr $numerval $alias)
(def%tr $options $alias)
(def%tr $ordergreat $alias)
(def%tr $orderless $alias)
(def%tr $printfile $alias)
(def%tr $printprops $alias)
(def%tr $product $alias)
(def%tr %product $alias)
(def%tr $properties $alias)
(def%tr $propvars $alias)
(def%tr $rearray $alias)
(def%tr $remarray $alias)
(def%tr $remfunction $alias)
(def%tr $remove $alias)
(def%tr $remvalue $alias)
(def%tr $setup_autoload $alias)
(def%tr $sum $alias)
(def%tr %sum $alias)
(def%tr $translate $alias)
(def%tr $writefile $alias)

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; END:

