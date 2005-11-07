;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)

    ;;  (defmacro if (test &rest args)
    ;;    (cond ((> (length args) 2)
    ;;	(format t "~%Warning: Too many args for if:~% ~a"
    ;;		(cons 'if (cons test args)))
    ;;	   `(lisp::if ,test ,(car args) (progn ,@(cdr args))))
    ;;	  (t `(lisp:if ,test ,@args))))



    ;;this will make operators which 
    ;;declare the type and result of numerical operations


    (defmacro def-op (name type op &optional return-type)
      `(setf (macro-function ',name)
	(make-operation ',type ',op ',return-type)))
  
    ;;make very sure .type .op and .return are not special!!
    (defun make-operation (.type .op .return)
      (or .return (setf .return .type))
      #'(lambda (bod env) env
		(loop for v in (cdr bod)
		       when (eq t .type) collect v into body
		       else
		       collect `(the , .type ,v) into body
		       finally (setq body `(, .op ,@ body))
		       (return
			 (if (eq t .return) body
			     `(the , .return ,body))))))

    #+fix-debug
    (progn ;; these allow running of code and they print out where the error
      ;; occurred
    
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

    ;;note 1+ and 1- in the main macsyma code were for fixnum 1+, 
    ;;so we should replace them by f1+ and f1- and then add the appropriate
    ;;definitions here.

    (def-op f+ fixnum +)
    (def-op f* fixnum *)
    (def-op f- fixnum -)
    (def-op +$ double-float +)
    (def-op *$ double-float *)
    (def-op -$ double-float -)
    (def-op 1-$ double-float 1-)
    (def-op 1+$ double-float 1+)
    (def-op f1- fixnum 1-)
    (def-op f1+ fixnum 1+)
    (def-op sub1 t 1-)
    (def-op add1 t 1+)
    (def-op plus t +)
    (def-op times t *)
    (def-op difference t -)
    (def-op quotient t quot)
    (def-op // t quot)			;(def-op // fixnum quot) ??
    (def-op //$ double-float quot)
    (def-op ^ fixnum expt)
    (def-op ^$ double-float expt)
    (def-op greaterp t > )
    (def-op f> fixnum > t)
    (def-op f< fixnum <  t)
    (def-op f= fixnum = t)
    (def-op lessp t < t)
    (def-op remainder t rem)
    #-mcl
    (def-op lsh fixnum ash)
    (def-op fixnum-remainder fixnum rem)
    (def-op minus t -)
    ;;(def-op \\ fixnum rem) ;no calls any more

    ;;exp is shadowed to save trouble for other packages--its declared special
    (setf (symbol-function 'exp) (symbol-function 'cl:exp))

    ) ;;end eval-when (symbolics needed this).

;;this is essentially what the quotient is supposed to do.

(defun quot (a &rest b)
  (cond ((null b)
	 (quot 1 a))
	
	((null (cdr b))
	 (setq b (car b))
	 (cond ((and (integerp a) (integerp b))
		(values (truncate a b)))
	       (t 
		( / a b))))
	(t (apply 'quot (quot a (car b)) (cdr b)))))


(defmacro status (option &optional item)
  (let ((it (intern (string item) (find-package 'keyword))))
    (cond ((equal (symbol-name option) (symbol-name '#:feature))
	   `(member ,it *features*))
	  ((equal option 'gctime) 0))))

(defmacro sstatus (option item )
  (let ((it (intern (string item) (find-package 'keyword))))
    (if (equal (symbol-name option) (symbol-name '#:feature))
	`(pushnew ,it *features*)
	(error "unknown sstatus ~a" option))))

(defun setplist (sym val)
  (setf (symbol-plist sym) val))

(defun sortcar (lis &optional (test 'alphalessp))
  (sort lis test :key 'car))

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


	       
;;(defmacro array-active-length (ar)
;;  `(length (the vector ,ar)))

(defmacro symbol-array (sym) `(get ,sym 'array))

(defmacro afuncall (sym &rest ind)
  `(aref (symbol-array ,sym) ,@ind))

(defun arraydims (ar)
  (when (symbolp ar) (setq ar (symbol-array ar)))
  (cons (array-type ar) (array-dimensions ar)))

(defun array-dimension-n (n ar)
  (declare (fixnum n))
  (array-dimension ar (the fixnum (- n 1))))

(defun array-type (ar)
  (array-element-type ar))
 
(defun firstn (n lis)
  (subseq lis 0 n))
  
(defun fixnump (n)
  (typep n 'fixnum))

(defun fix (n) (values (floor n)))


;;did result of fix have to  be fixnum in maclisp??
;;so could this be more efficient??
(setf (symbol-function 'fixr) #'round) 

(defun mapatoms (func &optional (pack *package*))
  (do-symbols (x pack)
    (funcall func x)))

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

(defun zl-member (x lis)
  (member x lis :test #'equal))

(defun assq (x alist)
  (assoc x alist :test #'eq))

(defun zl-assoc (x alist)
  (assoc x alist :test #'equal))
  
(defun delq (x lis &optional (count (1- most-positive-fixnum)))
  (delete x lis :test #'eq :count count))

(defun zl-delete (x lis &optional count)
  (delete x lis :test #'equal :count count))

(setf (symbol-function 'lsh) #'ash)

(defun haulong (x)
  (integer-length x))

(defun bigp (x)
  (typep x 'bignum))

(defun  bignump (x)
  (typep x 'bignum))

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

(defmacro aset (val ar &rest inds)
  `(setf (aref ,ar ,@inds) ,val))


;;used in translation
(defun fset (sym val)
  (setf (symbol-function sym) val))

(defun oldget (plist indic)
  (declare (object plist))
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
  (declare (object plist))
  (cond ((symbolp plist)
	 (setq plist (symbol-plist plist)))
	((consp plist) (setq plist (cdr plist)))
	(t (return-from getl nil)))
  (loop for tail on plist by #'cddr
	 when (memq (car tail) indicator-list)
	 do (return tail)))

;;this is the get of maclisp
;; works on symbols and plists
;;(defun maclisp-get (sym-or-plist prop)
;;  (cond ((symbolp sym-or-plist)
;;	 (get sym-or-plist prop))
;;	((consp sym-or-plist)
;;	 (getf (cdr sym-or-plist) prop))
;;	(t nil)))

(defmacro ncons (x)
  `(cons ,x nil)) ;;can one optimize this??

(defun zl-remove (item list &optional n)
  (remove item list :count n :test #'equal))

(defvar *acursor* (make-array 11 :element-type 'fixnum
			      :initial-element 0))

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
  (setq x
	(cond ((null x)
	       (ecase (array-element-type ar)
		 (fixnum '(0))
		 (float '(0.0))
		 ((t) '(nil))))
	      ((arrayp x)(listarray x))
	      ((atom x) (list x))
	      (t x)))
  (when (> (length ar) 0)  
    (set-up-cursor ar)
    (loop while (aset-by-cursor ar (car x))
	   do (and (cdr x) (setq x (cdr x))))))

;;(defun fillarray (ar x)
;;  (when (symbolp ar)
;;    (setq ar (get ar 'ARRAY)))
;;  (let ((leng (length (the (lisp:array  t ) ar))))
;;    (declare (fixnum leng))
;;  (cond ((null x)
;;	 (setq x (ecase (array-element-type ar)
;;			     (fixnum 0)
;;			     (float 0.0)
;;			     ((t) nil)))
;;	 (loop for i below leng
;;		do (setf (aref ar i) x)))
;;	((consp x)
;;	 (loop for i below leng
;;		for u in x
;;		do (setf (aref ar i) u)
;;		finally
;;		(loop for j from i below leng
;;		       do (setf (aref ar j) u))))
;;	((arrayp x)
;;	 (loop for i below (min leng (length x))
;;		do (setf (aref ar i) (aref x i))
;;		finally (loop for j from i below leng
;;			       with u = (aref x (f- i 1))
;;			       do (setf (aref ar j ) u))))
;;	(t (error "bad second arg to fillarray")))))

 
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

(setf (symbol-function 'atan2) (symbol-function 'cl:atan))

(setq *read-default-float-format* 'double-float)

#+clisp (setq custom:*default-float-format* 'double-float)
;;don't care about float contagion for now
#+clisp (setq custom::*warn-on-floating-point-contagion* nil)

(defmacro float (x &optional (y 1.0d0)) `(cl:float ,x ,y))
