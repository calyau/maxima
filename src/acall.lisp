;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;;; Run-time support for translated code.
;;; GJC: Experimental macsyma array lisp level support for translated
;;; code.
;;; To quickly handle the array reference and setting syntax in macsyma,

;;; In macsyma arrays go by an atomic name. Lists and matricies 
;;; may be hacked with the array syntax, which is convient.

;;;additions for handling arrays in value cell on cl --wfs

(macsyma-module acall)

#+pdp10
(eval-when (eval compile) (sstatus feature jpg))

(transl-module acall)

(defmfun interval-error (fun low high)
  (merror "Lower bound to ~:@M : ~M, not less than upper bound: ~M"
	  fun low high))

(defmfun mfuncall (f &rest l)
  (cond
    #+cl ((functionp f)(apply f l))
    #-cl ((and (symbolp f)
	       (fboundp f))
	  ;; This is unfortunately NOT correct.
	  ;; A complicated interplay of the setting of $TRANSRUN,
	  ;; and MGET '$TRACE, GET 'TRANSLATED and MGET 'MEXPR
	  ;; interacts to determine if a function can be called
	  ;; via APPLY.
	  (apply f l))
    #+cl
    ((and (symbolp f)(or (macro-function f)
			 (special-operator-p f)))
     (eval (cons f (copy-rest-arg l))))
    (t
     (mapply f (copy-rest-arg  l) nil))))

(declare-top(*lexpr list-ref))
 
;;; ((MQAPPLY ARRAY) X Y) is a strange form, meaning (X)[Y].

(defmfun marrayref (aarray ind1 &rest inds &aux ap tem)
  (declare (special fixunbound flounbound))
  (case
      (ml-typep aarray)
    ((array)
     (case (array-type aarray)
       ((flonum fixnum #+lispm art-q #+cl t)
	(apply 'aref aarray ind1 inds))
       #-cl
       ((t)
	(marrayref-gensub aarray ind1 inds))
       (t
	(merror "Bug: Non-handled array created. ~M" aarray))))
    #-cl
    ((si:equal-hash-table)
     (gethash (if inds (cons ind1 inds) inds) aarray))
    #+cl
    ((hash-table)
     (gethash (if inds (cons ind1 inds) inds) aarray))
    ((symbol)
     (cond
       #+cl
       ($use_fast_arrays
	(setq tem (and (boundp aarray) (symbol-value aarray)))
	(simplify (cond ((arrayp tem) (apply 'aref tem ind1 inds))
			((hash-table-p tem)
			 (gethash (if inds (cons ind1 inds) inds)
				  tem))
			((eq aarray 'mqapply) 
			 (apply #'marrayref ind1 inds))
			((mget aarray 'hashar)
			 (harrfind `((,aarray array) ,ind1 ,@ (copy-rest-arg inds))))
			((symbolp tem)
			 `((,tem array) ,ind1 ,@ (copy-rest-arg inds)))
			(t (error "unknown type of array for use_fast_arrays. ~
                               the value cell should have the array or hash table")))))
       (t
	(simplify (cond 
		    ((setq ap (get aarray 'array))
		     (let ((val (cond ((null inds)
				       (funcall ap ind1))
				      (t
				       (apply ap ind1 inds)))))
		       ;; Check for KLUDGING array function implementation.
		       (if (case (array-type ap)
			     ((flonum) (= val flounbound))
			     ((fixnum) (= val fixunbound))
			     ((t) (eq val munbound))
			     (t (merror "Bug: Array pointer of unknown type: ~S"
					ap)))
			   (arrfind `((,aarray ,aarray) ,ind1 ,@ (copy-rest-arg inds)))
			   val)))
		    ((setq ap (mget aarray 'array))
		     #+jpg
		     (and (mfilep ap) (i-$unstore (list aarray)))
		     (arrfind `((,aarray array) ,ind1 ,@ (copy-rest-arg inds))))
		    ((setq ap (mget aarray 'hashar))
		     #+jpg
		     (and (mfilep ap) (i-$unstore (list aarray)))
		     (harrfind `((,aarray array) ,ind1  ,@ (copy-rest-arg inds))))
		    ((eq aarray 'mqapply)
		     (apply #'marrayref ind1 inds))
		    (t
		     `((,aarray  array) ,ind1  ,@ (copy-rest-arg inds))))))))
    ((list)
     (simplify (cond ((memq (caar aarray) '(mlist $matrix))
		      (list-ref aarray (cons ind1 (copy-rest-arg inds))))
		     (t
		      `((mqapply aarray) ,aarray ,ind1 ,@ (copy-rest-arg inds))))))
    (t
     (merror "Bad object to reference as an array: ~M" aarray))))

(defmfun $arrayapply (ar inds)
  (or ($listp inds)
      (merror "The second arg to `arrayapply' must be a list."))
  (apply #'marrayref ar (cdr inds)))

(defmfun $arraysetapply (ar inds val)
  (or ($listp inds)
      (merror "The second arg to `arrayapply' must be a list."))
  (apply #'marrayset val ar (cdr inds)))


(defmfun marrayset (val aarray &rest all-inds &aux ap (ind1 (first all-inds))
			(inds (cdr all-inds)))
  (case (ml-typep aarray)
    ((array)
     (case (array-type aarray)
       ((fixnum flonum #+lispm art-q #+cl t)
	#-cl (store (apply aarray ind1 inds) val)
	#+cl (setf (apply #'aref aarray ind1 inds) val)
	)
       #-cl
       ((t)
	(marrayset-gensub val aarray ind1 inds))
       (t
	(merror "Bug: unhandled array type. ~M" aarray))))
    #+cl
    ((hash-table #+lispm si:equal-hash-table)
     (setf (gethash (if (cdr all-inds)
			(copy-rest all-inds)
			(car all-inds))
		    aarray) val))
    ((symbol)
     (cond ((setq ap (get aarray 'array))
	    (cond ((null inds)
		   (store (funcall ap ind1) val))
		  (t
		   #-cl (store (apply ap ind1 inds) val)
		   #+cl (setf (apply #'aref ap all-inds) val)
		   )))
	   ((setq ap (mget aarray 'array))
	    #+jpg
	    (and (mfilep ap) (i-$unstore (list aarray)))
	    ;; the macsyma ARRAY frob is NOT an array pointer, it
	    ;; is a GENSYM with a lisp array property, don't
	    ;; ask me why.
	    (cond ((null inds)
		   (store (funcall ap ind1) val))
		  (t
		   #-cl (store (apply ap all-inds) val)
		   #+cl (setf (apply #'aref ap all-inds) val)
		   )))
	   ((setq ap (mget aarray 'hashar))
	    #+jpg
	    (and (mfilep ap) (i-$unstore (list aarray)))
	    (arrstore `((,aarray ,'array)
			,@(mapcar #'(lambda (u)
				      `((mquote simp) ,u))
				  all-inds
				  ))
		      val))
	   ((eq aarray 'mqapply)
	    #-cl
	    (apply #'marrayset `(,val ,ind1 ,@inds))
            #+cl (apply #'marrayset val ind1 inds)
	    )
	   (t
	    (arrstore `((,aarray ,'array) ,@(mapcar #'(lambda (u)
							`((mquote simp) ,u))
						    all-inds
						    ))
		      val))))
    (list
     (cond ((memq (caar aarray) '(mlist $matrix))
	    (list-ref aarray (copy-rest-arg all-inds) t val))
	   ('else
	    (merror "Bad use of `:' on~%~M" aarray))))
    (t
     (merror "Bad argument to set as an array.~%~M" aarray)))
  val)



;;; Note that all these have HEADERS on the list. The CAR of a list I
;;; will call element 0. So [1,2][1] => 1

(defun list-ref (l indexl &optional set-flag val)
  (cond ((atom l)
	 (merror "Error-> tried to take part of an atom."))
	((null (cdr indexl))
	 (let ((n (car indexl)))
	   (cond ((and (integerp n) (plusp n)
		       (or (eq (caar l) 'mlist)
			   (eq (caar l) '$matrix)))
		  (let ((ret (do ((j 1 (f1+ j))
				  (n (fixnum-identity n))
				  (l (cdr l) (cdr l)))
				 ((or (null l) (= j n))
				  (cond ((null l)
					 (merror "Improper index to list or matrix: ~M" n))
					(set-flag
					 (rplaca l val))
					(t
					 (car l))))
			       (declare (fixnum j n)))))
		    (cond (set-flag l)
			  (t ret))))
		 (t
		  (merror "Error-> ~M  bad part subscript." n)))))
	(set-flag
	 (list-ref (list-ref l `(,(car indexl)))
		   (cdr indexl)
		   set-flag
		   val)
	 l)
	(t
	 (list-ref (list-ref l `(,(car indexl))) (cdr indexl)))))

;;; 3 guesses where this code is from.
;;;(DEFUN DISP1 (LL LABLIST EQNSP)
;;; (COND (LABLIST (SETQ LABLIST (cons '(MLIST SIMP) nil))))
;;; (DO ((LL LL (CDR LL)) (L) (ANS) ($DISPFLAG T) (TIM 0))
;;;     ((NULL LL) (OR LABLIST '$DONE))
;;;     (SETQ L (CAR LL) ANS (MEVAL L))
;;;     (COND ((AND EQNSP (OR (ATOM ANS) (NOT (EQ (CAAR ANS) 'MEQUAL))))
;;;	    (SETQ ANS (LIST '(MEQUAL) (DISP2 L) ANS))))
;;;     (COND (LABLIST (COND ((NOT (CHECKLABEL $LINECHAR))
;;;                           (SETQ $LINENUM (f1+ $LINENUM))))
;;;		    (MAKELABEL $LINECHAR) (NCONC LABLIST (cons LINELABLE nil))
;;;		    (COND ((NOT $NOLABELS) (SET LINELABLE ANS)))))
;;;     (SETQ TIM (RUNTIME))
;;;     (DISPLA (LIST '(MLABLE) (COND (LABLIST LINELABLE)) ANS))
;;;     (MTERPRI)
;;;     (TIMEORG TIM)))

(declare-top(special $dispflag))
(defmfun display-for-tr (labelsp equationsp &rest argl)
  (declare (special linelable))
  (do ((argl argl (cdr argl))
       (lablist nil)
       (tim 0))
      ((null argl)
       (cond (labelsp
	      `((mlist) ,@lablist))
	     (t '$done)))
    (let ((ans (car argl)))
      (cond ((and equationsp
		  ;; ((MEQUAL) FOO BAR)
		  (not (atom (caddr ans)))
		  (eq (caar (caddr ans)) 'mequal))
	     ;; if the ANS evaluats to something with an "="
	     ;; allready then of course he really meant to use
	     ;; DISP, but we might as well do what he means right?
	     (setq ans (caddr ans))))
      (cond (labelsp
	     (or (checklabel $linechar)
		 (setq $linenum (f1+ $linenum)))
	     (makelabel $linechar)
	     ;; setqs the free variable LINELABLE, what a win,
	     ;; how convenient, now I don't need to use LET !
	     (push linelable ;; note the spelling
		   lablist)
	     (or  $nolabels
		  (set linelable ;; SET !!!!
		       ans))))
      (setq tim (runtime))
      (displa `((mlable) ,(cond (labelsp linelable)) ,ans))
      (mterpri)
      (timeorg tim))))


(defmfun insure-array-props (fnname ignore-mode number-of-args &aux ary)
  ignore-mode
  ;; called during load or eval time by the defining forms
  ;; for translated array-functions.
  ;; this duplicates code in JPG;MLISP (however, the code in MLISP
  ;; is not callable because it is in a big piece of so-called
  ;; multi-purpose code).

  ;; This code is incredibly kludgy. For example, what if
  ;; the function FOO[J] had a lisp array property gotten
  ;; by ARRAY(FOO,FIXNUM,33), how is *THAT* detected by this code?
  ;; Well, it is because that will also put an MPROP ARRAY of $FOO,
  ;; and (ARRAYDIMS '$FOO) works! (Also checks the array property).
  ;; Isn't that something. Shit, I never knew that ARRAYDIMS worked
  ;; on symbols. What a crock.
  (cond ((prog2 (add2lnc fnname $arrays)
	     (setq ary (mgetl fnname '(hashar array))))
	 #+jpg
	 (cond ((mfilep (cadr ary))
		(i-$unstore (cons fnname nil))
		(setq ary (mgetl fnname '(hashar array)))))
	 (cond ((not (= (cond ((eq (car ary) 'hashar) (funcall (cadr ary) 2))
			      (t (length (cdr (arraydims (cadr ary))))))
			number-of-args))
		(merror
		 "~:@M Array already defined with different dimensions"
		 fnname))))
	(t (mputprop fnname (setq ary (gensym)) 'hashar)
	   (*array ary t 7)
	   (store (funcall ary 0) 4)
	   (store (funcall ary 1) 0)
	   (store (funcall ary 2) number-of-args))))

;;; An entry point to $APPLY for translated code.

(defmfun mapply-tr (fun list)
  (or ($listp list)
      (merror "Second arg to `apply' was not a list:~%~M" list))
  (mapply1 fun (cdr list) '|the first arg to a translated `apply'| list))


(defmfun assign-check (var val)
  (let ((a (get var 'assign)))
    (if a (funcall a var val))))


(declare-top (special maplp))

;;(format t "~%Change maplist_tr for the explorer rest arg bug")
#+cl
(defmfun maplist_tr (fun  l1 &rest l)
  (setq l (cons l1 (copy-list l)))
  (simplify (let ((maplp t) res)
	      (setq res (apply #'map1 (getopr fun) l))
	      (cond ((atom res) (list '(mlist) res))
		    ((eq (caar res) 'mlist) res)
		    (t (cons '(mlist) (margs res))))))) 
#-cl
(defmfun maplist_tr (fun &rest l)
  (simplify (let ((maplp t) res)
	      (setq res (apply #'map1 (getopr fun) l))
	      (cond ((atom res) (list '(mlist) res))
		    ((eq (caar res) 'mlist) res)
		    (t (cons '(mlist) (margs res)))))))


;;; Entry point into DB for translated code. The main point here
;;; is that evaluation of a form takes place first, (using the lisp
;;; evaluator), and then the trueness is checked. It is not correct
;;; to call the function IS because double-evaluation will then
;;; result, which is wrong, not to mention being incompatible with
;;; the interpreter. 
;;;
;;; This code is take from the COMPAR module, and altered such that calls to
;;; the macsyma evaluator do not take place. It would be a lot
;;; better to simply modify the code in COMPAR! However, mumble...
;;; Anyway, be carefull of changes to COMPAR that break this code.

(defmfun is-boole-check (form)
  (cond ((null form) nil)
	((eq form t) t)
	('else
	 ;; We check for T and NIL quickly, otherwise go for the database.
	 (mevalp_tr form $prederror nil))))

(defmfun maybe-boole-check (form)
  (mevalp_tr form nil nil))

;; The following entry point is for querying the database without
;; the dubious side effects of using PREDERROR:FALSE.

(defmspec $maybe (form) (mevalp_tr (fexprcheck form) nil t))

(declare-top(special patevalled))

(defun mevalp_tr (pat error? meval?)
  (let (patevalled ans)
    (setq ans (mevalp1_tr pat error? meval?))
    (cond ((memq ans '(t nil)) ans)
	  (error?
	   (pre-err patevalled))
	  ('else '$unknown))))

(defun mevalp1_tr (pat error? meval?)
  (cond ((and (not (atom pat)) (memq (caar pat) '(mnot mand mor)))
	 (cond ((eq 'mnot (caar pat)) (is-mnot_tr (cadr pat) error? meval?))
	       ((eq 'mand (caar pat)) (is-mand_tr (cdr pat) error? meval?))
	       (t (is-mor_tr (cdr pat) error? meval?))))
	((atom (setq patevalled (if meval? (meval pat) pat))) patevalled)
	((memq (caar patevalled) '(mnot mand mor)) (mevalp1_tr patevalled
							       error?
							       meval?))
	(t (mevalp2 (caar patevalled) (cadr patevalled) (caddr patevalled)))))

(defun is-mnot_tr (pred error? meval?)
  (setq pred (mevalp_tr pred error? meval?))
  (cond ((eq t pred) nil)
	((not pred))
	(t (pred-reverse pred))))

(defun is-mand_tr (pl error? meval?)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl))
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mand) (nreverse npl)))))
    (setq dummy (mevalp_tr (car pl) error? meval?)
	  pl (cdr pl))
    (cond ((eq t dummy))
	  ((null dummy) (return nil))
	  (t (setq npl (cons dummy npl))))))

(defun is-mor_tr (pl error? meval?)
  (do ((dummy) (npl))
      ((null pl) (cond ((null npl) nil)
		       ((null (cdr npl)) (car npl))
		       (t (cons '(mor) (nreverse npl)))))
    (setq dummy (mevalp_tr (car pl) error? meval?)
	  pl (cdr pl))
    (cond ((eq t dummy) (return t))
	  ((null dummy))
	  (t (setq npl (cons dummy npl))))))


;; Some functions for even faster calling of arrays.
(declare-top(flonum (marrayref1$ nil nil)
		    (marrayset1$ flonum nil nil)))

(defun marrayref1$ (aarray index)
  (case (ml-typep aarray)
    ((aarray)
     (case (array-type aarray)
       ((flonum) (arraycall flonum aarray index))
       (t (merror "Bad type of array to call for `float' value: ~M" aarray))))
    (t
     (float (marrayref aarray index)))))

(defun marrayset1$ (value aarray index)
  (case (ml-typep aarray)
    ((aarray)
     (case (array-type aarray)
       ((flonum) (store (arraycall flonum aarray index) value))
       (t (merror "Bad type of array to set `float' into: ~M" aarray))))
    (t
     (float (marrayset value aarray index)))))


(defmfun application-operator (form &rest ign) ign 
	 (apply (caar form) (cdr form)))

;;; Multics trys to optimize EVAL calls into APPLY's 
;;; On Multics DEFUN is a MACRO so we indirect to fool the complier
;;; by letting the form be a variable.
(defmfun make-alambda (formals body)
  (let* ((name (gensym))
	 (form-to-eval `(defun ,name ,formals ,body)))
    ;; on LISPM we can use closures after we fix up MEVAL and MAPPLY.
    ;; This isn't much more expensive, GENSYMs get garbage collected
    ;; just like any other object.
    (putprop name 'application-operator 'operators)
    (eval `(defun ,name ,formals ,body))
    (eval form-to-eval)
    name))

;; more efficient operators calls.

(defun *mminus (x)
  (if (numberp x)
      (minus x)
      (simplify (list '(mminus) x))))

(defmfun retlist_tr n
  (do ((j (f1- n) (f- j 2))
       (l () (cons (list '(mequal simp) (arg j) (arg (f1+ j))) l)))
      ((< j 0) (cons '(mlist simp) l))))
