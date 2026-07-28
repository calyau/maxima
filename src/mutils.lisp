;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mutils)

;;; General purpose Macsyma utilities.  This file contains runtime functions 
;;; which perform operations on Macsyma functions or data, but which are
;;; too general for placement in a particular file.
;;;
;;; Every function in this file is known about externally.

;;; A MEMQ which works at all levels of a piece of list structure.
;;;
;;; Note that (AMONG NIL '(A B C)) is T, however.  This could cause bugs.
;;; > This is false. (AMONG NIL anything) returns NIL always. -kmp

(defun among (x l) 
  (cond ((null l) nil)
	((atom l) (eq x l))
	(t (or (among x (car l)) (among x (cdr l)))))) 


;;; (ASSOL item A-list)
;;;
;;;  Like ASSOC, but uses ALIKE1 as the comparison predicate rather
;;;  than EQUAL.
;;;
;;;  Meta-Synonym:	(ASS #'ALIKE1 ITEM ALIST)

(defun assol (item alist)
 (cond
   ((symbolp item)
     (and alist (assoc item alist :test #'eq)))
   ((integerp item)
     (and alist (assoc item alist :test #'eql)))
   (t
  (dolist (pair alist)
    (if (alike1 item (car pair)) (return pair))))))

(declaim (inline assolike))
(defun assolike (item alist) 
  (cdr (assol item alist)))

;;; (MEMALIKE X L)
;;;
;;;  Searches for X in the list L, but uses ALIKE1 as the comparison predicate
;;;  (which is similar to EQUAL, but ignores header flags other than the ARRAY
;;;  flag)
;;;
;;;  Conceptually, the function is the same as
;;;
;;;    (when (find x l :test #'alike1) l)
;;;
;;;  except that MEMALIKE requires a list rather than a general sequence, so the
;;;  host lisp can probably generate faster code.
(defun memalike (x l)
 (if (symbolp x)
  (and l (member x l :test #'eq))
  (do ((l l (cdr l)))
      ((null l))
    (when (alike1 x (car l)) (return l)))))

;;; Return the first duplicate element of the list LIST, or NIL if there
;;; are no duplicates present in LIST.  The function KEY is applied to
;;; each element of the list before comparison (or uses the element itself
;;; if KEY is NIL), and the comparison is done with the function TEST.
;;;
;;; This was written with "small" lists in mind.  The original use case
;;; was finding duplicates in parameter lists of functions, etc.
(defun find-duplicate (list &key (test #'eql) key)
  (declare (optimize (speed 3)))
  (declare (type (or function null) key)
           (type function test))
  (let ((seen nil))
    (dolist (e list)
      (let ((i (if key (funcall key e) e)))
        (when (member i seen :test test)
          (return-from find-duplicate e))
        (push i seen)))))

;; Does X or a subexpression match PREDICATE?
;;
;; If X is a tree then we recurse depth-first down its arguments. The specrep
;; check is because rat forms are built rather differently from normal Maxima
;; expressions so we need to unpack them for the recursion to work properly.
(defun subexpression-matches-p (predicate x)
  (or (funcall predicate x)
      (and (consp x)
           (if (specrepp x)
               (subexpression-matches-p predicate (specdisrep x))
               (some (lambda (arg) (subexpression-matches-p predicate arg))
                     (cdr x))))))

;;; ----------------------------------------------------------------------------
;;; Subscripted function utilities
;;; ----------------------------------------------------------------------------

;; Constructor and extractor primitives for subscripted functions, e.g.
;; F[1,2](X,Y).  SUBL is (1 2) and ARGL is (X Y).

(declaim (inline subfunmakes))
(defun subfunmakes (fun subl argl)
  `((mqapply simp) ((,fun simp array) . ,subl) . ,argl))

(declaim (inline subfunmake))
(defun subfunmake (fun subl argl)
  `((mqapply) ((,fun simp array) . ,subl) . ,argl))

(declaim (inline subfunname))
(defun subfunname (expr) (caaadr expr))

(declaim (inline subfunsubs))
(defun subfunsubs (expr) (cdadr expr))

(declaim (inline subfunargs))
(defun subfunargs (expr) (cddr expr))

;;; ----------------------------------------------------------------------------
;;; Modular arithmetic
;;; ----------------------------------------------------------------------------

(defun normalized-modulus (n)
  "Normalizes the number N with respect to MODULUS,
  returning a number in (-MODULUS/2, MODULUS/2]."
  (let* ((m modulus) (rem (mod n m)))
    (if (<= (* 2 rem) m)
      rem
      (- rem m))))

;; CMOD
;;
;; When MODULUS is null, this is the identity. Otherwise, it normalises N, which
;; should be a number, to lie in the range (-modulus/2, modulus/2].
(declaim (inline cmod))
(defun cmod (n)
  (declare (type number n))
  (if modulus (normalized-modulus n) n))

(declaim (inline cplus ctimes cdifference))
(defun cplus       (a b) (cmod (+ a b)))
(defun ctimes      (a b) (cmod (* a b)))
(defun cdifference (a b) (cmod (- a b)))

;;; ----------------------------------------------------------------------------
;;; Matcher utilities
;;; ----------------------------------------------------------------------------

(declaim (inline matcherr))
(defun matcherr ()
  (throw 'match nil))

(declaim (inline kar))
(defun kar (x)
  (if (atom x) (matcherr) (car x)))

(declaim (inline *kar))
(defun *kar (x)
  (if (not (atom x)) (car x)))

(declaim (inline kaar))
(defun kaar (x)
  (kar (kar x)))

(declaim (inline kdr))
(defun kdr (x)
  (if (atom x) (matcherr) (cdr x)))

(defun nthkdr (x c)
  (if (zerop c) x (nthkdr (kdr x) (1- c))))

;;; ----------------------------------------------------------------------------
;;; Widely used trivial predicates
;;; ----------------------------------------------------------------------------

(declaim (inline ratnump))
(defun ratnump (x)
  "Determines if X is a Maxima rational form:  ((rat ...) a b)"
  (and (not (atom x)) (eq (caar x) 'rat)))

(declaim (inline mnump))
(defun mnump (x)
  "Returns T if X is a Lisp number or if it is a Maxima rational
  form or a bigfloat form, NIL otherwise"
  (or (numberp x)
      (and (not (atom x)) (not (atom (car x)))
	   (member (caar x) '(rat bigfloat)) t)))

(defun bigfloat-one-p (x)
  "Returns T if X, assumed to be a bigfloat, represents the value 1."
  ;; Binary bigfloat ones are of the form '((BIGFLOAT [SIMP] <P>) 2^(<P>-1) 1).
  ;; Decimal bigfloat ones are of the form '((BIGFLOAT [SIMP] <P> DECIMAL) 1 0).
  ;; The SIMP flag is optional.
  (if (eq 'decimal (car (last (car x))))
    (and (= 1 (cadr x)) (zerop (caddr x)))
    (and (= 1 (caddr x)) (= (ash 1 (1- (car (last (car x))))) (cadr x)))))

(declaim (inline onep1))
(defun onep1 (x)
  "Returns non-NIL if X is Lisp number or bfloat that is equal
  to 1"
  (cond
    ((numberp x) (= 1 x))
    (($bfloatp x) (bigfloat-one-p x))))

;; EVEN works for any arbitrary lisp object since it does an integer
;; check first.  In other cases, you may want the Lisp EVENP function
;; which only works for integers.

(declaim (inline even))
(defun even (x)
  (and (integerp x) (not (oddp x))))

;; Is there a bfloat anywhere in X?
(defun some-bfloatp (x)
  (subexpression-matches-p '$bfloatp x))

;; Is there a float anywhere in X?
(defun some-floatp (x)
  (subexpression-matches-p 'floatp x))

(declaim (inline mplusp))
(defun mplusp (x)
  "Determines if X is a Maxima sum form: ((mplus ...) ...)"
  (and (not (atom x)) (eq (caar x) 'mplus)))

(declaim (inline mtimesp))
(defun mtimesp (x)
  "Determines if X is a Maxima product form: ((mtimes ...) ...)"
  (and (not (atom x)) (eq (caar x) 'mtimes)))

(declaim (inline mexptp))
(defun mexptp (x)
  "Determines if X is a Maxima exponential form: ((mexpt ...) ...)"
  (and (not (atom x)) (eq (caar x) 'mexpt)))

(declaim (inline mnctimesp))
(defun mnctimesp (x)
  (and (not (atom x)) (eq (caar x) 'mnctimes)))

(declaim (inline mncexptp))
(defun mncexptp (x)
  (and (not (atom x)) (eq (caar x) 'mncexpt)))

(declaim (inline mlogp))
(defun mlogp (x)
  "Determines if X is a Maxima log form: ((%log ...) ...)"
  (and (not (atom x)) (eq (caar x) '%log)))

(declaim (inline mmminusp))
(defun mmminusp (x)
  "Determines if X is a Maxima negative form: ((mminus ...) ...)
   This generally only happens on input forms like a - b:
     ((mplus) $a ((mminus) $b)).  
   After simplification a - b becomes 
     ((mplus) $a ((mtimes) -1 $b))"
  (and (not (atom x)) (eq (caar x) 'mminus)))

(defun mnegp (x)
  "Determines if X is negative if X is a Lisp number or a Maxima rat
  form or bigfloat form"
  (cond ((numberp x) (minusp x))
        ((or (ratnump x) ($bfloatp x)) (minusp (cadr x)))))

(declaim (inline mqapplyp))
(defun mqapplyp (x)
  (and (not (atom x)) (eq (caar x) 'mqapply)))

(declaim (inline mbagp))
(defun mbagp (x)
  (and (not (atom x))
       (member (caar x) '(mequal mlist $matrix)) t))

(declaim (inline mequalp))
(defun mequalp (x)
  (and (not (atom x)) (eq (caar x) 'mequal)))

(declaim (inline mxorlistp))
(defun mxorlistp (x)
  (and (not (atom x))
       (member (caar x) '(mlist $matrix)) t))

(declaim (inline mxorlistp1))
(defun mxorlistp1 (x)
  (and (not (atom x))
       (or (eq (caar x) '$matrix)
	   (and $listarith (eq (caar x) 'mlist)))))

(defun sinp (e)
  (and (consp e) (eq '%sin (caar e))))

(defun cosp (e)
  "Return true iff the expression `e` is in general form and its operator is `%cos`."
  (and (consp e) (eq '%cos (caar e))))


