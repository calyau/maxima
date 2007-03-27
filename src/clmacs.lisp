;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(eval-when
    #+gcl (compile load)
    #-gcl (:compile-toplevel :load-toplevel)

    ;;this will make operators which
    ;;declare the type and result of numerical operations

    (defmacro def-op (name type op &optional return-type)
      `(setf (macro-function ',name)
	     (make-operation ',type ',op ',return-type)))

    ;;make very sure .type .op and .return are not special!!
    (defun make-operation (.type .op .return)
      (or .return (setf .return .type))
      #'(lambda (bod env)
	  (declare (ignore env))
	  (loop for v in (cdr bod)
	     when (eq t .type) collect v into body
	     else
	     collect `(the , .type ,v) into body
	     finally (setq body `(, .op ,@body))
	     (return
	       (if (eq t .return)
		   body
		   `(the , .return ,body))))))

    #+fix-debug
    (progn
      ;; these allow running of code and they print out where the error occurred

      (defvar *dbreak* t)

      (defun chk-type (lis na typ sho)
	(unless (every #'(lambda (v) (typep v typ)) lis)
	  (format t "~%Bad call ~a types:~a" (cons na sho)
		  (mapcar #'type-of lis))
	  (when *dbreak*
	    (break "hi"))))

      (defmacro def-op (name type old)
	`(defmacro ,name (&rest l)
	   `(progn (chk-type (list ,@l) ',',name ',',type ',l)
		   (,',old ,@l)))))

    (def-op f+ fixnum +)
    (def-op f* fixnum *)
    (def-op f- fixnum -)
    (def-op +$ double-float +)
    (def-op *$ double-float *)
    (def-op -$ double-float -)
    (def-op f1- fixnum 1-)
    (def-op f1+ fixnum 1+)
    (def-op quotient t quot)
    (def-op // t quot))

;;this is essentially what the quotient is supposed to do.

(defun quot (a &rest b)
  (cond ((null b)
	 (quot 1 a))
	((null (cdr b))
	 (setq b (car b))
	 (cond ((and (integerp a) (integerp b))
		(values (truncate a b)))
	       (t
		(/ a b))))
	(t (apply #'quot (quot a (car b)) (cdr b)))))

(defmacro status (option &optional item)
  (cond ((equal (symbol-name option) (symbol-name '#:feature))
	 `(member ,(intern (string item) (find-package 'keyword)) *features*))
	((equal option 'gctime) 0)))

;;numbers<strings<symbols<lists<?
(defun alphalessp (x y)
  (cond ((numberp x)
	 (if (numberp y) (< x y) t))
	((stringp x)
	 (cond ((numberp y) nil)
	       ((stringp y)
		(string< x y))
	       (t t)))
	((symbolp x)
	 (cond ((or (numberp y) (stringp y)) nil)
	       ((symbolp y)
		(let ((nx (symbol-name x))
		      (ny (symbol-name y)))
		  (declare (string nx ny))
		  (cond ((string< nx ny)
			 t)
			((string= nx ny)
			 (cond ((eq nx ny) nil)
			       ((null (symbol-package x)) nil)
			       ((null (symbol-package y)) nil)
			       (t (string<
				   (package-name (symbol-package x))
				   (package-name (symbol-package y))))))
			(t nil))))
	       ((consp y) t)))
	((listp x)
	 (cond ((or (numberp y) (stringp y)(symbolp y )) nil)
	       ((listp y)
		(or (alphalessp (car x) (car y))
		    (and (equal (car x) (car y))
			 (alphalessp (cdr x) (cdr y)))))
	       (t nil)))
	((or (numberp y) (stringp y) (symbolp y)(consp y))
	 nil)
	(t				;neither is of known type:
	 (alphalessp (format nil "~s" x)(format nil "~s" y)))))

(defmacro symbol-array (sym)
  `(get ,sym 'array))

(defun arraydims (ar)
  (when (symbolp ar)
    (setq ar (symbol-array ar)))
  (cons (array-element-type ar) (array-dimensions ar)))

(defun fixnump (n)
  (typep n 'fixnum))

(defun  bignump (x)
  (typep x 'bignum))

;;actually this was for lists too.

(defun putprop (sym val  indic)
  (if (consp sym)
      (setf (getf (cdr sym) indic) val)
      (setf (get sym indic) val)))

(defmacro defprop (sym val indic)
  (if (eq indic 'expr)
      `(setf (symbol-function ',sym) #',val)
      `(setf (get ',sym ',indic) ',val)))

(defun sassq (item alist fun)
  (or (assoc item alist :test #'eq)
      (funcall fun)))

(defun memq (x lis)
  (member x lis :test #'eq))

;; Find the N most significant or least significant bits of the
;; absolute value of X.  If N is positive, take the most significant;
;; otherwise, the least significant.
(defun haipart (x n)
  (let ((x (abs x)))
    (if (< n 0)
	(logand x (1- (ash 1 (- n))))
	(ash x (min (- n (integer-length x)) 0)))))

;; also correct but slower.
;;(defun haipart (integer count)
;;  (let ((x (abs integer)))
;;    (if (minusp count)
;;      (ldb (byte (- count) 0) x)
;;      (ldb (byte count (max 0 (- (integer-length x) count))) x))))

;;used in translation
(defun fset (sym val)
  (setf (symbol-function sym) val))

(defun oldget (plist indic)
  (cond ((symbolp plist)
	 (setq plist (symbol-plist plist)))
	((consp plist) (setq plist (cdr plist)))
	(t (return-from oldget nil)))
  (loop for tail on plist by #'cddr
	 when (eq (car tail) indic)
	 do (return (second tail))))

(defun safe-get (sym prop)
  (and (symbolp sym) (get sym prop)))

(defmacro safe-getl (sym prop)
  `(and (symbolp ,sym) (getl ,sym ,prop)))

(defun getl (plist indicator-list )
  (cond ((symbolp plist)
	 (setq plist (symbol-plist plist)))
	((consp plist) (setq plist (cdr plist)))
	(t (return-from getl nil)))
  (loop for tail on plist by #'cddr
	 when (member (car tail) indicator-list :test #'eq)
	 do (return tail)))

(defmacro ncons (x)
  `(cons ,x nil)) ;;can one optimize this??

(defvar *acursor* (make-array 11 :element-type 'fixnum :initial-element 0))

;; Format of *acursor*.
;; 0                 1  2  3  4  5    6  7  8  9  10
;; dim               i1 i2 i3 i4 i5   d1 d2 d3 d4 d5
;; array dimension   current index    maximal index

(defun set-up-cursor (ar)
  (let ((lis (array-dimensions ar)))
    (setf (aref *acursor* 0) (length lis))
    (loop for v in lis for i from 6 do (setf (aref *acursor* i) (1- v)))
    (loop for i from 1 to (length lis) do (setf (aref *acursor* i) 0))))

(defun aset-by-cursor (ar  val)
  (let ((curs  *acursor*))
    (declare (type (simple-array fixnum (11)) curs))
    (ecase (aref curs 0)
      (1 (setf (aref ar (aref curs 1)) val))
      (2 (setf (aref ar (aref curs 1) (aref curs 2)) val))
      (3 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)) val))
      (4 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)
		     (aref curs 4)) val))
      (5 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)
		     (aref curs 4) (aref curs 5)) val)))
    ;; set the index (`cursor') for the next call to ASET-BY-CURSOR
    (loop for j downfrom (aref curs 0)
	   do (cond ((< (aref curs j) (aref curs (f+ 5 j)))
		     (setf (aref curs j) (f+  (aref curs j) 1))
		     (return-from aset-by-cursor t))
		    (t (setf (aref curs j) 0)))
	   (cond ((eql j 0) (return-from aset-by-cursor nil))))))

(defun fillarray (ar x)
  (when (symbolp ar)
    (setq ar (get ar 'array)))
  (when (/= (array-rank ar) 1)
    (setq ar (make-array (array-total-size ar) :displaced-to ar)))
  (setq x (cond ((null x)
		 (ecase (array-element-type ar)
		   (fixnum '(0))
		   (float '(0d0))
		   ((t) '(nil))))
		((arrayp x)(listarray x))
		((atom x) (list x))
		(t x)))
  (when (> (length ar) 0)
    (set-up-cursor ar)
    (loop while (aset-by-cursor ar (car x))
       do (and (cdr x) (setq x (cdr x))))))

(defun listarray (x)
  (when (symbolp x)
    (setq x (get x 'array)))
  (if (eql (array-rank x) 1)
      (coerce x 'list)
      (coerce (make-array (apply '* (array-dimensions x)) :displaced-to x
			  :element-type (array-element-type x))
	      'list)))

(defmacro check-arg (place pred &rest res)
  (when (atom pred)
    (setq pred (list pred place)))
  `(assert ,pred (,place) ,@res))

(defmacro deff (fun val)
  `(setf (symbol-function ',fun) ,val))

(defmacro xcons (x y)
  (cond ((atom x) `(cons ,y,x))
	(t (let ((g (gensym)))
	     `(let ((,g ,x))
	       (cons ,y ,g))))))

(defun make-equal-hash-table (not-dim1)
  (let ((table (make-hash-table :test 'equal)))
    (or not-dim1 (setf (gethash 'dim1 table) t))
    table))

;;range of atan should be [0,2*pi]
(defun atan (y x)
  (let ((tem (cl:atan y x)))
    (if (>= tem 0)
	tem
	(+ tem (* 2 pi)))))

;;range of atan2 should be (-pi,pi]
;;CL manual says that's what lisp::atan is supposed to have.
(deff atan2 #'cl:atan)

;;exp is shadowed to save trouble for other packages--its declared special
(deff exp #'cl:exp)

(setq *read-default-float-format* 'double-float)

#+clisp
(progn
  ;; This used to be enabled, but
  ;; http://clisp.cons.org/impnotes/num-dict.html seems to indicate
  ;; that the result of float, coerce, sqrt, etc., on a rational will
  ;; return a float of the specified type.  But ANSI CL says we must
  ;; return a single-float.  I (rtoy) am commenting this out for now.

  ;; (setq custom:*default-float-format* 'double-float)

  ;; We currently don't want any warnings about floating-point contagion.
  (setq custom::*warn-on-floating-point-contagion* nil)

  ;; We definitely want ANSI-style floating-point contagion.
  (setq custom:*floating-point-contagion-ansi* t)

  ;; Set custom:*floating-point-rational-contagion-ansi* so that
  ;; contagion is done as per the ANSI CL standard. Has an effect only
  ;; in those few cases when the mathematical result is exact although
  ;; one of the arguments is a floating-point number, such as (* 0
  ;; 1.618), (/ 0 1.618), (atan 0 1.0), (expt 2.0 0)
  (setq custom:*floating-point-rational-contagion-ansi* t))

(defmacro float (x &optional (y 1d0))
  `(cl:float ,x ,y))
