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

(transl-module acall)

(defmfun interval-error (fun low high)
  (merror "Lower bound to ~:@M : ~M, not less than upper bound: ~M" fun low high))

(defmfun mfuncall (f &rest l)
  (cond ((functionp f)
	 (apply f l))
	((and (symbolp f) (or (macro-function f) (special-operator-p f)))
	 (eval (cons f l)))
	(t
	 (mapply f l nil))))

;;; ((MQAPPLY ARRAY) X Y) is a strange form, meaning (X)[Y].

(defmfun marrayref (aarray ind1 &rest inds &aux ap tem)
  (declare (special fixunbound flounbound))
  (case (ml-typep aarray)
    ((array)
     (case (array-element-type aarray)
       ((flonum fixnum t)
	(apply #'aref aarray ind1 inds))
       (t
	(merror "Bug: Non-handled array created. ~M" aarray))))
    ((hash-table)
     (gethash (if inds (cons ind1 inds) inds) aarray))
    ((symbol)
     (cond ($use_fast_arrays
	    (setq tem (and (boundp aarray) (symbol-value aarray)))
	    (simplify (cond ((arrayp tem) (apply 'aref tem ind1 inds))
			    ((hash-table-p tem)
			     (gethash (if inds (cons ind1 inds) inds)
				      tem))
			    ((eq aarray 'mqapply)
			     (apply #'marrayref ind1 inds))
			    ((mget aarray 'hashar)
			     (harrfind `((,aarray array) ,ind1 ,@inds)))
			    ((symbolp tem)
			     `((,tem array) ,ind1 ,@inds))
			    (t (error "unknown type of array for use_fast_arrays. ~
			       the value cell should have the array or hash table")))))
	   (t
	    (simplify (cond ((setq ap (get aarray 'array))
			     (let ((val (if (null inds)
					    (funcall ap ind1)
					    (apply ap ind1 inds))))
			       ;; Check for KLUDGING array function implementation.
			       (if (case (array-element-type ap)
				     ((flonum) (= val flounbound))
				     ((fixnum) (= val fixunbound))
				     ((t) (eq val munbound))
				     (t (merror "Bug: Array pointer of unknown type: ~S" ap)))
				   (arrfind `((,aarray ,aarray) ,ind1 ,@inds))
				   val)))
			    ((setq ap (mget aarray 'array))
			     (arrfind `((,aarray array) ,ind1 ,@inds)))
			    ((setq ap (mget aarray 'hashar))
			     (harrfind `((,aarray array) ,ind1  ,@inds)))
			    ((eq aarray 'mqapply)
			     (apply #'marrayref ind1 inds))
			    (t
			     `((,aarray  array) ,ind1  ,@inds)))))))
    ((list)
     (simplify (if (member (caar aarray) '(mlist $matrix) :test #'eq)
		   (list-ref aarray (cons ind1 inds))
		   `((mqapply aarray) ,aarray ,ind1 ,@inds))))
    (t
     (merror "Bad object to reference as an array: ~M" aarray))))

(defmfun $arrayapply (ar inds)
  (unless ($listp inds)
    (merror "The second arg to `arrayapply' must be a list."))
  (apply #'marrayref ar (cdr inds)))

(defmfun $arraysetapply (ar inds val)
  (unless ($listp inds)
    (merror "The second arg to `arrayapply' must be a list."))
  (apply #'marrayset val ar (cdr inds)))

(defmfun marrayset (val aarray &rest all-inds &aux ap (ind1 (first all-inds)) (inds (cdr all-inds)))
  (case (ml-typep aarray)
    ((array)
     (case (array-element-type aarray)
       ((fixnum flonum t)
	(setf (apply #'aref aarray ind1 inds) val))
       (t
	(merror "Bug: unhandled array type. ~M" aarray))))
    ((hash-table)
     (setf (gethash (if (cdr all-inds)
			(copy-list all-inds)
			(car all-inds))
		    aarray) val))
    ((symbol)
     (cond ((setq ap (get aarray 'array))
	    (cond ((null inds)
		   (store (funcall ap ind1) val))
		  (t
		   (setf (apply #'aref ap all-inds) val))))
	   ((setq ap (mget aarray 'array))
	    ;; the macsyma ARRAY frob is NOT an array pointer, it
	    ;; is a GENSYM with a lisp array property, don't
	    ;; ask me why.
	    (cond ((null inds)
		   (store (funcall ap ind1) val))
		  (t
		   (setf (apply #'aref ap all-inds) val))))
	   ((setq ap (mget aarray 'hashar))
	    (arrstore `((,aarray ,'array)
			,@(mapcar #'(lambda (u) `((mquote simp) ,u)) all-inds))
		      val))
	   ((eq aarray 'mqapply)
	    (apply #'marrayset val ind1 inds))
	   (t
	    (arrstore `((,aarray ,'array)
			,@(mapcar #'(lambda (u) `((mquote simp) ,u)) all-inds))
		      val))))
    (list (if (member (caar aarray) '(mlist $matrix) :test #'eq)
	      (list-ref aarray all-inds t val)
	      (merror "Bad use of `:' on~%~M" aarray)))
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
		  (let ((ret (do ((j 1 (1+ j))
				  (l (cdr l) (cdr l)))
				 ((or (null l) (= j n))
				  (cond ((null l)
					 (merror "Improper index to list or matrix: ~M" n))
					(set-flag
					 (rplaca l val))
					(t
					 (car l)))))))
		    (if set-flag l ret)))
		 (t
		  (merror "Error-> ~M  bad part subscript." n)))))
	(set-flag
	 (list-ref (list-ref l `(,(car indexl))) (cdr indexl) set-flag val)
	 l)
	(t
	 (list-ref (list-ref l `(,(car indexl))) (cdr indexl)))))

(declare-top (special $dispflag))

(defmfun display-for-tr (labelsp equationsp &rest argl)
  (declare (special linelable))
  (do ((argl argl (cdr argl))
       (lablist nil)
       (tim 0))
      ((null argl) (if labelsp `((mlist) ,@lablist) '$done))
    (let ((ans (car argl)))
      (cond ((and equationsp
		  ;; ((MEQUAL) FOO BAR)
		  (not (atom (caddr ans)))
		  (eq (caar (caddr ans)) 'mequal))
	     ;; if the ANS evaluats to something with an "="
	     ;; allready then of course he really meant to use
	     ;; DISP, but we might as well do what he means right?
	     (setq ans (caddr ans))))
      (when labelsp
	(unless (checklabel $linechar)
	  (incf $linenum))
	(makelabel $linechar)
	;; setqs the free variable LINELABLE, what a win,
	;; how convenient, now I don't need to use LET !
	(push linelable lablist) ;; note the spelling
	(unless $nolabels
	  (setf (symbol-value linelable) ans)))
      (setq tim (get-internal-run-time))
      (displa `((mlable) ,(cond (labelsp linelable)) ,ans))
      (mterpri)
      (timeorg tim))))


(defmfun insure-array-props (fnname ignore-mode number-of-args &aux ary)
  (declare (ignore ignore-mode))
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
  (cond ((prog2
	     (add2lnc fnname $arrays)
	     (setq ary (mgetl fnname '(hashar array))))
	 (unless (= (if (eq (car ary) 'hashar)
			(funcall (cadr ary) 2)
			(length (cdr (arraydims (cadr ary)))))
		    number-of-args)
	   (merror "~:@M Array already defined with different dimensions" fnname)))
	(t (mputprop fnname (setq ary (gensym)) 'hashar)
	   (*array ary t 7)
	   (store (funcall ary 0) 4)
	   (store (funcall ary 1) 0)
	   (store (funcall ary 2) number-of-args))))

;;; An entry point to $APPLY for translated code.

(defmfun mapply-tr (fun list)
  (unless ($listp list)
    (merror "Second arg to `apply' was not a list:~%~M" list))
  (mapply1 fun (cdr list) '|the first arg to a translated `apply'| list))

(defmfun assign-check (var val)
  (let ((a (get var 'assign)))
    (if a (funcall a var val))))


(declare-top (special maplp))

(defmfun maplist_tr (fun  l1 &rest l)
  (setq l (cons l1 (copy-list l)))
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
;;; This code is taken from the COMPAR module, and altered such that calls to
;;; the macsyma evaluator do not take place. It would be a lot
;;; better to simply modify the code in COMPAR! However, mumble...
;;; Anyway, be careful of changes to COMPAR that break this code.

(defmfun is-boole-check (form)
  (cond ((null form) nil)
	((eq form t) t)
	(t
	 ;; We check for T and NIL quickly, otherwise go for the database.
	 (mevalp_tr form $prederror nil))))

(defmfun maybe-boole-check (form)
  (mevalp_tr form nil nil))

;; The following entry point is for querying the database without
;; the dubious side effects of using PREDERROR:FALSE.

(defmspec $maybe (form)
  (mevalp_tr (fexprcheck form) nil t))

(declare-top (special patevalled))

(defun mevalp_tr (pat error? meval?)
  (let (patevalled ans)
    (setq ans (mevalp1_tr pat error? meval?))
    (cond ((member ans '(t nil) :test #'eq) ans)
	  (error? (pre-err patevalled))
	  (t '$unknown))))

(defun mevalp1_tr (pat error? meval?)
  (cond ((and (not (atom pat)) (member (caar pat) '(mnot mand mor) :test #'eq))
	 (cond ((eq 'mnot (caar pat)) (is-mnot_tr (cadr pat) error? meval?))
	       ((eq 'mand (caar pat)) (is-mand_tr (cdr pat) error? meval?))
	       (t (is-mor_tr (cdr pat) error? meval?))))
	((atom (setq patevalled (if meval? (meval pat) pat)))
	 patevalled)
	((member (caar patevalled) '(mnot mand mor) :test #'eq)
	 (mevalp1_tr patevalled error? meval?))
	(t
	 (mevalp2 (caar patevalled) (cadr patevalled) (caddr patevalled)))))

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

(defun marrayref1$ (aarray index)
  (case (ml-typep aarray)
    ((aarray)
     (case (array-element-type aarray)
       ((flonum) (aref aarray index))
       (t (merror "Bad type of array to call for `float' value: ~M" aarray))))
    (t
     (float (marrayref aarray index)))))

(defun marrayset1$ (value aarray index)
  (case (ml-typep aarray)
    ((aarray)
     (case (array-element-type aarray)
       ((flonum) (setf (aref aarray index) value))
       (t (merror "Bad type of array to set `float' into: ~M" aarray))))
    (t
     (float (marrayset value aarray index)))))


(defmfun application-operator (form &rest ign)
  (declare (ignore ign))
  (apply (caar form) (cdr form)))

;; more efficient operators calls.

(defun *mminus (x)
  (if (numberp x)
      (- x)
      (simplify (list '(mminus) x))))

(defmfun retlist_tr n
  (do ((j (1- n) (- j 2))
       (l () (cons (list '(mequal simp) (arg j) (arg (1+ j))) l)))
      ((< j 0) (cons '(mlist simp) l))))
