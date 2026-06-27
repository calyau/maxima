;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra project
;;; (https://sourceforge.net/projects/maxima/) 
;;; SPDX-License-Identifier: GPL-2.0-or-later 
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  This program is distributed WITHOUT ANY
;;; WARRANTY. See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defmfun $polysign (x)
  (setq x (cadr (ratf x)))
  (cond ((equal x 0) 0) ((pminusp x) -1) (t 1)))

(defmfun $numfactor (x)
  (setq x (specrepcheck x))
  (cond ((mnump x) x)
	((atom x) 1)
	((not (eq (caar x) 'mtimes)) 1)
	((mnump (cadr x)) (cadr x))
	(t 1)))

(defmfun $constantp (x)
  (cond ((atom x) (or ($numberp x) (kindp x '$constant)))
	((member (caar x) '(rat bigfloat)) t)
	((specrepp x) ($constantp (specdisrep x)))
	((or (mopp (caar x)) (kindp (caar x) '$constant))
	 (do ((x (cdr x) (cdr x))) ((null x) t)
	   (if (not ($constantp (car x))) (return nil))))))


(defmfun $scalarp (x) (or (consttermp x) (eq (scalarclass x) '$scalar)))

(defmfun $nonscalarp (x) (eq (scalarclass x) '$nonscalar))

(defmfun ($resimplify :inline-impl t) (expr)
  "Resimplifies the expression EXPR based on the current environment.
  This function is useful when the fact database, option variables,
  or tellsimp rules have changed since the expression was last simplified."
  (resimplify expr))

(defmfun $sqrt (z)
  (simplify (list '(%sqrt) z)))

;; Define a verb function $abs
(defmfun $abs (x)
  (simplify (list '(mabs) x)))

(defmfun $exp (z)
  (simplify (list '(%exp) z)))

;; Support a function for code,
;; which depends on an unsimplified noun form. 
(defmfun $exp-form (z)
  (list '(mexpt) '$%e z))

(defmfun $orderlessp (a b)
  (setq a ($totaldisrep (specrepcheck a))
        b ($totaldisrep (specrepcheck b)))
  (and (not (alike1 a b)) (great b a) t))

(defmfun $ordergreatp (a b)
  (setq a ($totaldisrep (specrepcheck a))
        b ($totaldisrep (specrepcheck b)))
  (and (not (alike1 a b)) (great a b) t))

;; Test function to order a and b by magnitude. If it is not possible to
;; order a and b by magnitude they are ordered by great. This function
;; can be used by sort, e.g. sort([3,1,7,x,sin(1),minf],ordermagnitudep)
(defmfun $ordermagnitudep (a b)
  (let (sgn)
    (setq a ($totaldisrep (specrepcheck a))
          b ($totaldisrep (specrepcheck b)))
    (cond ((and (or (constp a) (member a '($inf $minf)))
                (or (constp b) (member b '($inf $minf)))
                (member (setq sgn ($csign (sub b a))) '($pos $neg $zero)))
           (cond ((eq sgn '$pos) t)
                 ((eq sgn '$zero) (and (not (alike1 a b)) (great b a)))
                 (t nil)))
          ((or (constp a) (member a '($inf $minf))) t)
          ((or (constp b) (member b '($inf $minf))) nil)
          (t (and (not (alike1 a b)) (great b a))))))

(defmfun $multthru (e1 &optional e2)
  (let (arg1 arg2)
    (cond (e2				;called with two args
	   (setq arg1 (specrepcheck e1)
		 arg2 (specrepcheck e2))
           (cond ((or (atom arg2)
                      (not (member (caar arg2) '(mplus mequal))))
		  (mul2 arg1 arg2))
		 ((eq (caar arg2) 'mequal)
		  (list (car arg2) ($multthru arg1 (cadr arg2))
			($multthru arg1 (caddr arg2))))
		 (t (expandterms arg1 (cdr arg2)))))
	  (t 				;called with only one arg
	   (prog (l1)
	      (setq arg1 (setq arg2 (specrepcheck e1)))
	      (cond ((atom arg1) (return arg1))
		    ((eq (caar arg1) 'mnctimes)
		     (setq arg1 (cdr arg1)) (go nct))
		    ((not (eq (caar arg1) 'mtimes)) (return arg1)))
	      (setq arg1 (reverse (cdr arg1)))
	      times (when (mplusp (car arg1))
		      (setq l1 (nconc l1 (cdr arg1)))
		      (return (expandterms (muln l1 t) (cdar arg1))))
	      (setq l1 (cons (car arg1) l1))
	      (setq arg1 (cdr arg1))
	      (if (null arg1) (return arg2))
	      (go times)
	      nct  (when (mplusp (car arg1))
		     (setq l1 (nreverse l1))
		     (return (addn (mapcar
				    #'(lambda (u)
					(simplifya
					 (cons '(mnctimes) 
					       (append l1 (ncons u) (cdr arg1)))
					 t))
				    (cdar arg1))
				   t)))
	      (setq l1 (cons (car arg1) l1))
	      (setq arg1 (cdr arg1))
	      (if (null arg1) (return arg2))
	      (go nct))))))

(defmfun $expand (exp &optional (expop $maxposex) (expon $maxnegex))
  (expand1 exp expop expon))

;; The following was formerly in SININT.  This code was placed here because
;; SININT is now an out-of-core file on MC, and this code is needed in-core
;; because of the various calls to it. - BMT & JPG

(defmfun $integrate (expr x &optional lo hi)
  (declare (special *in-risch-p* context))
  (if (and lo (null hi))
    (wna-err '$integrate))
  (let ($ratfac)
    (if (not hi)
	(with-new-context (context)
	  (if (member '%risch *nounl*)
	      (if *in-risch-p*
            ;; Give up; we're being called from RISCHINT by some path.
            (list '(%integrate) expr x)
            (rischint expr x))
	      (sinint expr x)))
	($defint expr x lo hi))))

