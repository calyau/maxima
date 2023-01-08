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

;;; Run-time support for translated code.

;;; GJC: Experimental macsyma array lisp level support for translated code.
;;; To quickly handle the array reference and setting syntax in macsyma,

;;; In macsyma arrays go by an atomic name. Lists and matrices
;;; may be hacked with the array syntax, which is convient.

;;; additions for handling arrays in value cell on cl --wfs

(macsyma-module acall)

(defun interval-error (fun low high)
  (merror (intl:gettext "~@:M: lower bound ~M is greater than upper bound ~M") fun low high))

(defun mfuncall (f &rest l)
  (cond ((functionp f)
	 (apply f l))
	((and (symbolp f) (or (macro-function f) (special-operator-p f)))
	 (eval (cons f l)))
	(t
	 (mapply f l f))))

;;; ((MQAPPLY ARRAY) X Y) is a strange form, meaning (X)[Y].

(defun marrayref (aarray ind1 &rest inds)
  (typecase aarray
    (cl:array
     (case (array-element-type aarray)
       ((flonum fixnum t)
	(apply #'aref aarray ind1 inds))
       (t
	(merror (intl:gettext "MARRAYREF: encountered array ~M of unknown type.") aarray))))
    (cl:hash-table
     (gethash (if inds (cons ind1 inds) ind1) aarray))
    (cl:symbol
     (if $use_fast_arrays
         (let ((tem (and (boundp aarray) (symbol-value aarray))))
           (simplify (cond ((arrayp tem)
                            (apply #'aref tem ind1 inds))
                           ((hash-table-p tem)
                            (gethash (if inds (cons ind1 inds) ind1) tem))
                           ((eq aarray 'mqapply)
                            (apply #'marrayref ind1 inds))
                           ((mget aarray 'hashar)
                            (harrfind `((,aarray array) ,ind1 ,@inds)))
                           ((symbolp tem)
                            `((,tem array) ,ind1 ,@inds))
                           (t
                            (error "unknown type of array for use_fast_arrays. ~
			       the value cell should have the array or hash table")))))
         (let (ap)                      ; no fast arrays
           (simplify (cond ((setq ap (get aarray 'array))
                            (let ((val (if (null inds)
                                           (aref ap ind1)
                                           (apply #'aref (append (list ap ind1) inds)))))
                              ;; Check for KLUDGING array function implementation.
                              (if (case (array-element-type ap)
                                    ((flonum) (= val flounbound))
                                    ((fixnum) (= val fixunbound))
                                    ((t) (eq val munbound))
                                    (t (merror (intl:gettext "MARRAYREF: encountered array pointer ~S of unknown type.") ap)))
                                  (arrfind `((,aarray array) ,ind1 ,@inds))
                                  val)))
                           ((setq ap (mget aarray 'array))
                            (arrfind `((,aarray array) ,ind1 ,@inds)))
                           ((setq ap (mget aarray 'hashar))
                            (harrfind `((,aarray array) ,ind1  ,@inds)))
                           ((eq aarray 'mqapply)
                            (apply #'marrayref ind1 inds))
                           (t
                            `((,aarray  array) ,ind1  ,@inds)))))))
    (cl:list
     (simplify (if (member (caar aarray) '(mlist $matrix) :test #'eq)
		   (list-ref aarray (cons ind1 inds))
		   `((mqapply array) ,aarray ,ind1 ,@inds))))
    (t
     (merror (intl:gettext "MARRAYREF: cannot retrieve an element of ~M") aarray))))

(defmfun $arrayapply (ar inds)
  (unless ($listp inds)
    (merror (intl:gettext "arrayapply: second argument must be a list; found ~M") inds))
  (apply #'marrayref ar (cdr inds)))

(defmfun $arraysetapply (ar inds val)
  (unless ($listp inds)
    (merror (intl:gettext "arraysetapply: second argument must be a list; found ~M") inds))
  (apply #'marrayset val ar (cdr inds)))

(defun marrayset (val aarray &rest all-inds)
  (let ((ind1 (first all-inds))
        (inds (rest all-inds)))
    (typecase aarray
      (cl:array
       (case (array-element-type aarray)
         ((fixnum flonum t)
          (setf (apply #'aref aarray ind1 inds) val))
         (t
          (merror (intl:gettext "MARRAYSET: encountered array ~M of unknown type.") aarray))))
      (cl:hash-table
       (setf (gethash (if (cdr all-inds)
                          (copy-list all-inds)
                          (car all-inds))
                      aarray) val))
      (cl:symbol
       (let (ap)
         (cond ((setq ap (get aarray 'array))
                (if (null inds)
                    (setf (aref ap ind1) val)
                    (setf (apply #'aref ap all-inds) val)))
               ((setq ap (mget aarray 'array))
                ;; the macsyma ARRAY frob is NOT an array pointer, it
                ;; is a GENSYM with a lisp array property, don't
                ;; ask me why.
                (if (null inds)
                    (setf (aref (symbol-array ap) ind1) val)
                    (setf (apply #'aref (symbol-array ap) all-inds) val)))
               ((setq ap (mget aarray 'hashar))
                (arrstore `((,aarray ,'array)
                            ,@(mapcar #'(lambda (u) `((mquote simp) ,u)) all-inds))
                          val))
               ((eq aarray 'mqapply)
                (apply #'marrayset val ind1 inds))
               (t
                (arrstore `((,aarray ,'array)
                            ,@(mapcar #'(lambda (u) `((mquote simp) ,u)) all-inds))
                          val)))))
      (cl:list (if (member (caar aarray) '(mlist $matrix) :test #'eq)
                   (list-ref aarray all-inds t val)
                   (merror (intl:gettext "MARRAYSET: cannot assign to an element of ~M") aarray)))
      (t
       (merror (intl:gettext "MARRAYSET: ~M is not an array.") aarray)))      )
  val)

;;; Note that all these have HEADERS on the list. The CAR of a list I
;;; will call element 0. So [1,2][1] => 1

(defun list-ref (l indexl &optional set-flag val)
  (cond ((atom l)
	 (merror (intl:gettext "LIST-REF: argument must be a list; found ~M") l))
	((null (cdr indexl))
	 (let ((n (car indexl)))
	   (cond ((and (integerp n) (plusp n)
		       (or (eq (caar l) 'mlist)
			   (eq (caar l) '$matrix)))
		  (let ((ret (do ((j 1 (1+ j))
				  (l (cdr l) (cdr l)))
				 ((or (null l) (= j n))
				  (cond ((null l)
					 (merror (intl:gettext "LIST-REF: invalid subscript: ~M") n))
					(set-flag
					 (rplaca l val))
					(t
					 (car l)))))))
		    (if set-flag l ret)))
		 (t
		  (merror (intl:gettext "LIST-REF: invalid subscript: ~M") n)))))
	(set-flag
	 (list-ref (list-ref l `(,(car indexl))) (cdr indexl) set-flag val)
	 l)
	(t
	 (list-ref (list-ref l `(,(car indexl))) (cdr indexl)))))

(defun display-for-tr (labelsp equationsp &rest argl)
  (do ((argl argl (cdr argl))
       (lablist nil)
       (tim 0))
      ((null argl) (if labelsp `((mlist) ,@(nreverse lablist)) '$done))
    (let ((ans (car argl)))
      (cond ((and equationsp
		  ;; ((MEQUAL) FOO BAR)
		  (not (atom (caddr ans)))
		  (eq (caar (caddr ans)) 'mequal))
	     ;; if the ANS evaluats to something with an "="
	     ;; already then of course he really meant to use
	     ;; DISP, but we might as well do what he means right?
	     (setq ans (caddr ans))))
      (when labelsp
	(unless (checklabel $linechar)
	  (incf $linenum))
	(makelabel $linechar)
	;; setqs the free variable *LINELABEL*, what a win,
	;; how convenient, now I don't need to use LET !
	(push *linelabel* lablist)
	(unless $nolabels
	  (setf (symbol-value *linelabel*) ans)))
      (setq tim (get-internal-run-time))
      (let ((*display-labels-p* (not (null lablist))))
	(displa `((mlabel) ,(cond (labelsp *linelabel*)) ,ans)))
      (mterpri)
      (timeorg tim))))


(defun insure-array-props (fnname ignore-mode number-of-args &aux ary)
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
			(aref (symbol-array (cadr ary)) 2)
			(length (cdr (arraydims (cadr ary)))))
		    number-of-args)
	   (merror (intl:gettext "INSURE-ARRAY-PROPS: array ~:@M already defined with different dimensions.") fnname)))
	(t
	 (setq ary (gensym))
	 (mputprop fnname ary 'hashar)
	 (setf (symbol-array ary) (make-array 7 :initial-element nil))
	 (setf (aref (symbol-array ary) 0) 4)
	 (setf (aref (symbol-array ary) 1) 0)
	 (setf (aref (symbol-array ary) 2) number-of-args))))

;;; An entry point to $APPLY for translated code.

(defun mapply-tr (fun list)
  (unless ($listp list)
    (merror (intl:gettext "apply: second argument must be a list; found ~M") list))
  (mapply1 fun (cdr list) '|the first arg to a translated `apply'| list))

(defun assign-check (var val)
  (let ((a (get var 'assign)))
    (if a (funcall a var val))))

(defun maplist_tr (fun  l1 &rest l)
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

(defun boole-verify (form error? $unknown?)
  (cond ((typep form 'boolean)
         form)
        (error?
         (pre-err form))
        ($unknown?
         '$unknown)
        (t
         form)))

(defun boole-eval (form error? $unknown?)
  (if (typep form 'boolean)
      form
      (let ((ans (mevalp_tr form error?)))
        (if (or (typep ans 'boolean)
                (not $unknown?))
            ans
            '$unknown))))

(defun $is-boole-verify (form)
  (boole-verify form $prederror t))

(defun $is-boole-eval (form)
  (boole-eval form $prederror t))

(setf (get '$is 'tr-boole-verify) '$is-boole-verify)
(setf (get '$is 'tr-boole-eval) '$is-boole-eval)

(defun $maybe-boole-verify (form)
  (boole-verify form nil t))

(defun $maybe-boole-eval (form)
  (boole-eval form nil t))

(setf (get '$maybe 'tr-boole-verify) '$maybe-boole-verify)
(setf (get '$maybe 'tr-boole-eval) '$maybe-boole-eval)

(defun mcond-boole-verify (form)
  (boole-verify form $prederror nil))

(defun mcond-boole-eval (form)
  (boole-eval form $prederror nil))

(setf (get 'mcond 'tr-boole-verify) 'mcond-boole-verify)
(setf (get 'mcond 'tr-boole-eval) 'mcond-boole-eval)

(defun mevalp_tr (pat error?)
  (boole-verify (mevalp1_tr pat error?) error? nil))

(defun mevalp1_tr (pat error?)
  (cond ((atom pat) pat)
	((member (caar pat) '(mnot mand mor) :test #'eq)
	 (flet ((pred-eval (o) (mevalp_tr o error?)))
	   (cond ((eq 'mnot (caar pat)) (is-mnot #'pred-eval (cadr pat)))
	         ((eq 'mand (caar pat)) (is-mand #'pred-eval (cdr pat)))
	         (t (is-mor #'pred-eval (cdr pat))))))
	(t
	 (let ((ans (mevalp2 pat (caar pat) (cadr pat) (caddr pat))))
	   (if (typep ans 'boolean)
	       ans
	       pat)))))

;; Some functions for even faster calling of arrays.

(defun marrayref1$ (aarray index)
  (typecase aarray
    (cl:array
     (case (array-element-type aarray)
       ((flonum) (aref aarray index))
       (t (merror (intl:gettext "MARRAYREF1$: array must be an array of floats; found ~M") aarray))))
    (t
     (marrayref aarray index))))

(defun marrayset1$ (value aarray index)
  (typecase aarray
    (cl:array
     (case (array-element-type aarray)
       ((flonum) (setf (aref aarray index) value))
       (t (merror (intl:gettext "MARRAYSET1$: array must be an array of floats; found ~M") aarray))))
    (t
     (marrayset value aarray index))))


(defun application-operator (form &rest ign)
  (declare (ignore ign))
  (apply (caar form) (cdr form)))

;; more efficient operators calls.

(defun *mminus (x)
  (if (numberp x)
      (- x)
      (simplify (list '(mminus) x))))

(defun retlist_tr (&rest args)
  (do ((j (- (length args) 2) (- j 2))
       (l () (cons (list '(mequal simp) (nth j args) (nth (1+ j) args)) l)))
      ((< j 0) (cons '(mlist simp) l))))
