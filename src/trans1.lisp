;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
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
;;; This file handles System FSUBR translation properties that
;;; were not handled in TRANSL. 

(macsyma-module trans1)


(transl-module trans1)

;; Also defined in TRANSL;TRANSS
#-cl (defvar $tr_windy t)


;;;;;;;; THE FOLLOWING ARE MOSTLY FROM JPG MLISP ;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MMAPEV DOES error checking and a macar of MEVAL down the arguments.
;;; The second arg to MMAPEV is purely for printing of error messages
;;; except for SCANMAP, which is obscure.

;;(comment

;;(DEFMFUN MMAPEV (MAPFUN L) 
;;	 (IF (< (LENGTH L) 2)
;;	     (MERROR "~:M called with fewer than 2 args" MAPFUN))
;;	 (LET ((U (GETOPR (MEVAL (CAR L)))))
;;	      (AUTOLDCHK U)
;;	      (BADFUNCHK (CAR L) U NIL)
;;	      (IF (ATOM U)
;;		  ;; number of argument checking before mapping,
;;		  ;; some efficiency gain, really, how minor.
;;		  ;; he should instead do some trampolining and
;;		  ;; get some real efficiency gains.
;;		  (MARGCHK U (COND ((EQ MAPFUN '$SCANMAP)
;;				    (NCONS (CADR L)))
;;				   (T (CDR L)))))
;;	      (CONS U (MAPCAR 'MEVAL (CDR L)))))
;;)

;;(comment
;; (DEFMFUN $APPLY FEXPR (L)
;; (TWO-ARG-CHECK L)
;; ((LAMBDA (FUN ARG)
;;   (COND ((NOT ($LISTP ARG))
;;	  (DISPLA FUN) (DISPLA ARG) (MERROR "Second arg to `apply' must be a list")))
;;   (AUTOLDCHK (SETQ FUN (GETOPR FUN)))
;;   (COND ((EQ (GET FUN 'DIMENSION) 'DIMENSION-INFIX) (TWOARGCHK ARG FUN)))
;;   (MAPPLY FUN (CDR ARG) (CAR L)))
;;  (MEVAL (CAR L)) (MEVAL (CADR L))))
;;)

;;; APPLY(F,[X]) is an idiom for funcall.

(defun quoted-symbolp (form)
  (and (eq (ml-typep form) 'list)
       (eq 'quote (car form))
       (symbolp (cadr form))))

(def%tr $apply (form)
  (let* ((fun (dtranslate (cadr form)))
	 (mode (cond ((atom fun)
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

;;; (DEFMFUN $MAP FEXPR (L) (APPLY 'MAP1 (MMAPEV 'MAP L)))

(def%tr $map (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'map1 `((getopr ,fun) . ,args))))

;;; (DEFMFUN $MAPLIST FEXPR (L) 
;;;  ((LAMBDA (MAPLP RES)
;;;   (SETQ RES (APPLY 'MAP1 (MMAPEV 'MAPLIST L)))
;;;   (COND ((ATOM RES) (LIST '(MLIST) RES))
;;;	 ((EQ (CAAR RES) 'MLIST) RES)
;;;	 (T (CONS '(MLIST) (CDR RES)))))
;;;    T NIL))

(def%tr $maplist (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    ;; this statement saves the trouble of adding autoload definitions
    ;; for runtime translator support.
    (push-autoload-def 'marrayref '(maplist_tr))
    `($any . (maplist_tr ,fun ,@args))))

;;; (DEFMFUN $FULLMAP FEXPR (L)
;;;        (SETQ L (MMAPEV 'FULLMAP L)) (FMAP1 (CAR L) (CDR L) NIL))

(def%tr $fullmap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'fmap1 `((getopr ,fun) (list . ,args) nil))))

;;; (DEFMFUN $MATRIXMAP FEXPR (L)
;;;        ((LAMBDA (FMAPLVL) (APPLY 'FMAPL1 (MMAPEV 'MATRIXMAP L))) 2))

(def%tr $matrixmap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any `(lambda (fmaplvl)
			   (fmapl1 (getopr ,fun) . ,args))
		   '(2))))
		       
;;; (DEFMFUN $FULLMAPL FEXPR (L) (APPLY 'FMAPL1 (MMAPEV 'FULLMAPL L)))

(def%tr $fullmapl (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'fmapl1 `((getopr ,fun) . ,args))))

;;;(DEFMFUN $OUTERMAP FEXPR (L)
;;; (APPLY (COND ((= (LENGTH L) 2) 'FMAPL1) (T 'OUTERMAP1)) (MMAPEV 'OUTERMAP L)))

(def%tr $outermap (form)
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any (cond ((= (length args) 1) 'fmapl1)
			       (t 'outermap1))
		   `((getopr ,fun)  ,@args))))


;;;(DEFMFUN $SCANMAP FEXPR (L)
;;; (LET ((SCANMAPP T)) (SSIMPLIFYA (APPLY 'SCANMAP1 (MMAPEV '$SCANMAP L)))))

(def%tr $scanmap (form)
  (push-autoload-def '$scanmap '(scanmap1))
  ;; there's something more fundamental about the above than
  ;; just autoload definitions.
  (destructuring-let (((fun . args) (tr-args (cdr form))))
    (call-and-simp '$any 'scanmap1 `((getopr ,fun) ,@args))))

;;;(DEFMFUN $QPUT FEXPR (L)
;;; (COND ((NOT (= (LENGTH L) 3)) (ERLIST '|Wrong number of args to QPUT|)))
;;; ($PUT (CAR L) (CADR L) (CADDR L)))

(def%tr $qput (form)
  `($any $put ',(cadr form) ',(caddr form) ',(cadddr form)))

;;;(DEFMFUN $SUBVAR FEXPR (L)
;;; (COND ((NULL L) (ERLIST "Wrong number of args to `subvar'")))
;;; (MEVAL (CONS '(MQAPPLY ARRAY) L)))

(def%tr $subvar (form)
  (translate (cons '(mqapply array) (cdr form))))

;;; From JPG;COMM >
;;;(DEFMFUN $PART N (PART1 (LISTIFY N) NIL NIL $INFLAG))
;;;
;;;(DEFMFUN $INPART N (PART1 (LISTIFY N) NIL NIL T))
;;;
;;;(DEFMFUN $SUBSTPART FEXPR (L) (PART1 L T NIL $INFLAG))
;;;
;;;(DEFMFUN $SUBSTINPART FEXPR (L) (PART1 L T NIL T))
;;;
;;;(DEFUN PART1 (ARGLIST SUBSTFLAG DISPFLAG INFLAG) ....)

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
  (substpart-translation form t nil '$inflag))

(def%tr $substinpart (form)
  (substpart-translation form t nil t))

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

(defun  substpart-translation (form flag1 flag2 flag3)
  (let* ((subst-item (dtranslate (cadr form)))
	 (freevars (free-lisp-vars subst-item))
	 (argl (tr-args (cddr form))))
    (cond ((null (assq '$piece freevars))
					; this code is just to screw the people who
					; would use $PIECE non lexicaly. Not really, the
					; closure hacking is a lot slower at run time than
					; this easy case, so no sense screwing the people who
					; don't use $PIECE in foolish ways.
	   `($any . (simplify
		     (part1
		      (list  ,@(for-eval-then-mquote-simp-argl
				(cons subst-item argl)))

		      ,flag1 ,flag2 ,flag3))))
	  (t
	   (setq freevars (tbound-free-vars freevars))
	   (side-effect-free-check (cadr freevars) (cadr form))
	   `($any . (simplify
		     (part1 (list (fungen&env-for-meval
				   ,(zl-delete '$piece (car freevars))
				   ($piece) ,subst-item)
				  ,@(for-eval-then-mquote-simp-argl argl))
		      ,flag1 ,flag2 ,flag3)))))))



;;; From JPG;SUPRV >
;;(comment
;;(DEFMFUN $ERRCATCH FEXPR (X)
;;       ((LAMBDA (ERRCATCH RET)
;;		(COND ((NULL (SETQ RET
;;				   (ERRSET (APPLY 'MPROGN X)
;;					   LISPERRPRINT)))
;;		       (ERRLFUN1 ERRCATCH)))
;;		(CONS '(MLIST) RET))
;;	(CONS BINDLIST LOCLIST) NIL)))

;;; This is could be done better on the LISPM

(def%tr $errcatch (form)
  (setq form (translate `((mprogn) ,@(cdr form))))
  `(,(car form) . ((lambda (errcatch ret)
		     (declare (special errcatch))
		     ;; Very important to declare errcatch special
		     ;; here because merror uses it to figure out if
		     ;; someone is catching an error so it can be
		     ;; signaled in a way that we can catch.
		     (cond ((null (setq ret
					(errset ,(cdr form)
						lisperrprint)))
			    (errlfun1 errcatch)))
		     (cons '(mlist) ret))
		   (cons bindlist loclist) nil)))


;;(COMMENT 
;; (DEFMFUN $CATCH FEXPR (X)
;;	((LAMBDA (MCATCH)
;;		 (PROG2 NIL (CATCH 'MCATCH (APPLY 'MPROGN X))
;;			(ERRLFUN1 MCATCH)))
;;  (CONS BINDLIST LOCLIST))))

;;; The MODE of a CATCH could either be the MODE of the last of the PROGN
;;; or the mode of the THROW. The THROW may be hard to find, so this goes
;;; on the assumption that the mode of the PROGN is enough to tell.

(def%tr $catch (form)
  (destructuring-let (((mode . body) (translate `((mprogn) . ,(cdr form)))))
    `(,mode . ((lambda ()
		 ((lambda (mcatch)
		    (prog2 nil
			(catch
			    'mcatch ,body)
		      (errlfun1 mcatch)))
		  (cons bindlist loclist)))))))
;;(COMMENT
;; (DEFMFUN $THROW (X)
;; (COND ((NULL MCATCH) (DISPLA X) (ERLIST '|THROW not within CATCH|)))
;; (THROW 'MCATCH X)))

(def%tr $throw (form)
  (destructuring-let (((mode . exp) (translate (cadr form))))
    `(,mode . ((lambda (x)
		 (cond ((null mcatch)
			(displa x)
			(merror "`throw' not within `catch'")))
		 (throw 'mcatch x))
	       ,exp))))

;;; Makelist is a very sorry FSUBR. All these FSUBRS are just to avoid
;;; writing LAMBDA. But lots of users use MAKELIST now. 
;;; MAKELIST(EXP,X,0,N) with 4 args it is an iteration, with three it
;;; is a mapping over a list (the third argument).

(def%tr $makelist (form)
  (setq form (cdr form))
  (cond ((= (length form) 3)
	 (destructuring-let (((exp x llist) form)
			      (sum (tr-gensym))
			      (lil (tr-gensym)))
	   `($any . (do ((,lil (cdr ,(dtranslate llist)) (cdr ,lil))
			 (,sum nil)
			 (,x))
			((null ,lil)
			 `((mlist) ,@(nreverse ,sum)))
		      (setq ,x (car ,lil)
			    ,sum (cons ,(cdr (tr-local-exp exp
							   x
							   (value-mode x)))
				       ,sum))))))
	((= (length form) 4)
	 (destructuring-let (((exp x |0| n) form)
			     (|00| (tr-gensym))
			     (nn (tr-gensym))
			     (sum (tr-gensym)))
	   (setq |0| (dtranslate |0|)	; I had forgotten this before!
		 n (dtranslate n))	; never noticed.
	   `($any . ((lambda (,|00| ,nn)
					; bogus -gjc
					;(DECLARE (FIXNUM ,|00| ,NN))
		       (cond ((not (< ,nn ,|00|))
			      (do ((,x ,|00| (f1+ ,x))
				   (,sum
				    nil
				    (cons
				     ,(cdr (tr-local-exp exp
							 x
							 '$fixnum))

				     ,sum)))
				  ((> ,x ,nn)
				   `((mlist) ,@(nreverse ,sum)))
				(declare (fixnum ,x))))
			     (t
			      (interval-error
			       '$makelist ,|00| ,nn))))
		     ,|0| ,n))))
	(t
	 (mformat *translation-msgs-files*
		  "Wrong number of args to `makelist'")
	 (setq tr-abort t)
	 '($any . '$**error**))))

;;(comment
;; |jpg;suprv >|
;; (DEFMFUN $KILL FEXPR (L) (MAPC 'KILL1 L) #+GC (GCTWA) '$DONE))

(def%tr $kill (form)
  (cond ($tr_windy
	 (tr-tell "
Warning:" form
"Use of KILL in translated program is not recommended. See GJC for
a replacement form. Translating anyway though.")))
  `($any . (apply '$kill ',(cdr form))))

;;; Macsyma arrays are the biggest crock since STATUS PUNT NIL days.
;;; The basic idea of ARRAY(<frob>,type,dims...) is that
;;; if type is of
;;;(ASSQ (CADR X) '(($COMPLETE . T) ($INTEGER . FIXNUM) ($FIXNUM . FIXNUM)
;;;			  ($FLOAT . FLONUM) ($FLONUM . FLONUM)))
;;; then the dims are evaluated. But, if type is not one of those,
;;; it "must" be a dim spec! Of course, I must make this "analysis"
;;; at translate time, in order to preserve referential transparency
;;; in compiled code.

(def%tr $array (form)
  (setq form (cdr form))
  (let ((name (car form))
	(specp (assq (cadr form)
		     '(($complete . t) ($integer . fixnum) ($fixnum . fixnum)
		       ($float . flonum) ($flonum . flonum)))))
    (cond (specp
	   `($any . (apply '$array (list ',name
				    ',(cadr form)
				    ,@(tr-args (cddr form))))))
	  (t
	   `($any . (apply '$array (list ',name
				    ,@(tr-args (cdr form)))))))))


;;(comment
;;(DEFMFUN $DEFINE FEXPR (L)
;; (COND ((OR (NULL L) (NULL (CDR L)) (CDDR L))
;;	(ERLIST '|Wrong number of args to DEFINE|)))
;; (APPLY 'MDEFINE
;;	(LIST (COND ((MQUOTEP (CAR L)) (CADAR L)) (T (DISP2 (CAR L)))) (MEVAL (CADR L))))))

;;; MDEFINE is an FSUBR also.

(def%tr $define (form)
  (destructuring-let (((header body) (cdr form)))
    `($any . (apply 'mdefine
	      (list ',(cond ((mquotep header) (cadr header))
			    (t (disp2 header)))
	       ,(dtranslate body))))))


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
    (cond ((memq (car t-form) '($float $fixnum $number)) t-form)
	  (t `($any . (ratf ,(cdr t-form)))))))


;;; The following special forms do not call the evaluator.







(def%tr $batcon (form)
  `($any . (meval ',form)))  
;;most of these will lose in common since a local variable will not
;;have its value accessible to the mfexpr*.  They should
;;be redone as macros with any necessary info passed along.

(def%tr $remarray           $batcon)
(def%tr $rearray $batcon)
(def%tr $alias $batcon)
(def%tr $alloc $batcon)
(def%tr $batch $batcon)
(def%tr $batchload          $batcon)
;;(DEF%TR $BATCON $batcon)
(def%tr $closefile $batcon)
(def%tr $compfile           $batcon)
(def%tr $delfile $batcon)
(def%tr $demo $batcon)
(def%tr $dependencies $batcon)
(def%tr $describe           $batcon)
(def%tr $diskfree $batcon)
(def%tr $diskuse $batcon)
(def%tr $dispfun $batcon)
(def%tr $disprule $batcon)
(def%tr $filelength $batcon)
(def%tr $filelist $batcon)
(def%tr $fundef $batcon)
(def%tr $fulldiskuse $batcon)
(def%tr $gradef $batcon)
(def%tr $listfiles $batcon)
(def%tr $loadfile $batcon)
(def%tr $loadarrays         $batcon)
(def%tr $loadplots $batcon)
(def%tr $makeatomic $batcon)
(def%tr $namefile $batcon)
(def%tr $numerval           $batcon)
(def%tr $options $batcon)
(def%tr $ordergreat $batcon)
(def%tr $orderless $batcon)
(def%tr $plotmode $batcon)
(def%tr $primer $batcon)
(def%tr $printdiskuse $batcon)
(def%tr $printfile $batcon)
(def%tr $printprops $batcon)
(def%tr $properties $batcon)
(def%tr $propvars $batcon)
(def%tr $qlistfiles $batcon)
(def%tr $remfile            $batcon)
(def%tr $remfunction $batcon)
(def%tr $remove $batcon)
(def%tr $remvalue           $batcon)
(def%tr $renamefile $batcon)
(def%tr $restore $batcon)
(def%tr $translate          $batcon)
(def%tr $writefile $batcon)
(def%tr $hardcopy $batcon)
(def%tr $labels $batcon)
(def%tr $setup_autoload $batcon)
(def%tr $tobreak $batcon  )

;; Kill off the special code for translating sum and product.
(def%tr $sum $batcon)
(def%tr $product $batcon)

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; END:

