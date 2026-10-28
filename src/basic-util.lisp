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

;;; Basic utilities used by many routines.  These functions were moved
;;; from various files to here to reduce or fix some circular
;;; dependencies.


(defun recur-apply (fun e)
  (cond ((eq (caar e) 'bigfloat) e)
	((specrepp e) (funcall fun (specdisrep e)))
	(t (let ((newargs (mapcar fun (cdr e))))
	     (if (alike newargs (cdr e))
		 e
		 (simplifya (cons (cons (caar e) (member 'array (cdar e) :test #'eq)) newargs)
			    nil))))))

;; replace y with x in z, but leave z's second arg unchanged.
;; This is for cases like at(integrate(x, x, a, b), [x=3])
;; where second arg of integrate binds a new variable x,
;; and we do not wish to subst 3 for x inside integrand.
(defun subst-except-second-arg (x y z)
  (cond 
    ((member (caar z) '(%integrate %sum %product %limit %laplace))
     (append 
       (list (remove 'simp (car z))   ; ensure resimplification after substitution
             (if (eq y (third z))     ; if (third z) is new var that shadows y
                 (second z)           ; leave (second z) unchanged
                 (subst1 x y (second z))) ; otherwise replace y with x in (second z)
             (third z))               ; never change integration var
       (mapcar (lambda (z) (subst1 x y z)); do subst in limits of integral
               (cdddr z))))
    ((eq (caar z) '%at)
     ;; similar considerations here, but different structure of expression.
     (let*
       ((at-eqn-or-eqns (third z))
        (at-eqns-list (if (eq (caar at-eqn-or-eqns) 'mlist) (rest at-eqn-or-eqns) (list at-eqn-or-eqns))))
       (list
         (remove 'simp (car z)) ;; ensure resimplification after substitution
         (if (member y (mapcar #'(lambda (e) (second e)) at-eqns-list))
           (second z)
           (subst1 x y (second z)))
         `((mlist) ,@(mapcar #'(lambda (e) (list (first e) (second e) (subst1 x y (third e)))) at-eqns-list)))))
    ((eq (caar z) '%derivative)
     ;; and again, with yet a different structure.
     (let*
       ((vars-and-degrees (rest (rest z)))
        (diff-vars (loop for v in vars-and-degrees by #'cddr collect v))
        (diff-degrees (loop for n in (rest vars-and-degrees) by #'cddr collect n)))
       (append
         (list
           (remove 'simp (car z)) ;; ensure resimplification after substitution
           (if (member y diff-vars)
             (second z)
             (subst1 x y (second z))))
         (apply #'append (loop for v in diff-vars for n in diff-degrees collect (list v (subst1 x y n)))))))
    (t z)))

(defun subst0 (new old)
  (cond ((atom new) new)
	((alike (cdr new) (cdr old))
	 (cond ((eq (caar new) (caar old)) old)
	       (t (simplifya (cons (cons (caar new) (member 'array (cdar old) :test #'eq)) (cdr old))
			     nil))))
	((member 'array (cdar old) :test #'eq)
	 (simplifya (cons (cons (caar new) '(array)) (cdr new)) nil))
	(t (simplifya new nil))))

;;Remainder of page is update from F302 --gsb

(defun subst1 (x y z)			; Y is an atom
  (cond ((atom z) (if (equal y z) x z))
	((specrepp z) (subst1 x y (specdisrep z)))
	((eq (caar z) 'bigfloat) z)
	((and (eq (caar z) 'rat) (or (equal y (cadr z)) (equal y (caddr z))))
	 (div (subst1 x y (cadr z)) (subst1 x y (caddr z))))
	((at-substp z) (subst-except-second-arg x y z))
	((and (eq y t) (eq (caar z) 'mcond))
	 ;; Substitute into everything except the T of the final else clause.
	 (cons (cons (caar z) nil)
	       (maplist #'(lambda (l)
			    (if (and (eq (car l) t) (cdr l) (null (cddr l)))
				(car l)
				(subst1 x y (car l))))
			(cdr z))))
	(t (let ((margs (mapcar #'(lambda (z1) (subst1 x y z1)) (cdr z)))
                 (oprx (getopr x)) (opry (getopr y)))
	     (if (and $opsubst
		      (or (eq opry (caar z))
			  (and (eq (caar z) 'rat) (eq opry 'mquotient))))
		 (if (or (numberp x)
			 (member x '(t nil $%e $%pi $%i) :test #'eq)
			 (and (not (atom x))
			      (not (or (eq (car x) 'lambda)
				       (eq (caar x) 'lambda)))))
		     (if (or (and (member 'array (cdar z) :test #'eq)
				  (or (and (mnump x) $subnumsimp)
				      (and (not (mnump x)) (not (atom x)))))
			     ($subvarp x))
			 (let ((substp 'mqapply))
			   (subst0 (list* '(mqapply) x margs) z))
			 (merror (intl:gettext "subst: cannot substitute ~M for operator ~M in expression ~M") x y z))
		     (subst0 (cons (cons oprx nil) margs) z))
		 (subst0 (cons (cons (caar z) nil) margs) z))))))

(defun subst2 (x y z negxpty timesp)
  (let (newexpt)
    (cond ((atom z) z)
	  ((specrepp z) (subst2 x y (specdisrep z) negxpty timesp))
	  ((at-substp z) z) ;; IS SUBST-EXCEPT-SECOND-ARG APPROPRIATE HERE ?? !!
	  ((alike1 y z) x)
	  ((and timesp (eq (caar z) 'mtimes) (alike1 y (setq z (nformat z)))) x)
	  ((and (eq (caar y) 'mexpt) (eq (caar z) 'mexpt) (alike1 (cadr y) (cadr z))
		(setq newexpt (cond ((alike1 negxpty (caddr z)) -1)
				    ($exptsubst (expthack (caddr y) (caddr z))))))
	   (list '(mexpt) x newexpt))
	  ((and $derivsubst (eq (caar y) '%derivative) (eq (caar z) '%derivative)
		(alike1 (cadr y) (cadr z)))
	   (let ((tail (subst-diff-match (cddr y) (cdr z))))
	     (cond ((null tail) z)
		   (t (cons (cons (caar z) nil) (cons x (cdr tail)))))))
	  (t (recur-apply #'(lambda (z1) (subst2 x y z1 negxpty timesp)) z)))))

(defun maxima-substitute (x y z) ; The args to SUBSTITUTE are assumed to be simplified.
  ;; Prevent replacing dependent variable with constant in derivative
  (cond ((and (not (atom z))
              (eq (caar z) '%derivative)
              (eq (cadr z) y)
              (typep x 'number))
         z)
        (t
         (let ((in-p t) (substp t))
           (if (and (mnump y) (= (signum1 y) 1))
	       (let ($sqrtdispflag ($pfeformat t)) (setq z (nformat-all z))))
           (simplifya
            (if (atom y)
	        (cond ((equal y -1)
		       (setq y '((mminus) 1))
                       (subst2 x y (nformat-all z) nil nil)) ;; negxpty and timesp don't matter in this call since (caar y) != 'mexpt
	              (t
		       (cond ((and (not (symbolp x))
			           (functionp x))
		              (let ((tem (gensym)))
			        (setf (get  tem  'operators) 'application-operator)
			        (setf (symbol-function tem) x)
			        (setq x tem))))
		       (subst1 x y z)))
	        (let ((negxpty (if (and (eq (caar y) 'mexpt)
				        (= (signum1 (caddr y)) 1))
			           (mul2 -1 (caddr y))))
	              (timesp (if (eq (caar y) 'mtimes)
                                  (setq y (nformat y)))))
	          (subst2 x y z negxpty timesp)))
            nil)))))

(defun substitutel (l1 l2 e)
  "l1 is a list of expressions.  l2 is a list of variables. For each 
   element in list l2, substitute corresponding element of l1 into e"
  (do ((l1 l1 (cdr l1))
       (l2 l2 (cdr l2)))
      ((null l1) e)
    (setq e (maxima-substitute (car l1) (car l2) e))))

(defun union* (a b)
  (do ((a a (cdr a))
       (x b))
      ((null a) x)
    (if (not (memalike (car a) b)) (setq x (cons (car a) x)))))

(defun intersect* (a b)
  (do ((a a (cdr a))
       (x))
      ((null a) x)
    (if (memalike (car a) b) (setq x (cons (car a) x)))))

(defun nthelem (n e)
  (car (nthcdr (1- n) e)))

(defmfun $listp (x)
  (and (not (atom x))
       (not (atom (car x)))
       (eq (caar x) 'mlist)))

(defun depends (e x &aux l)
  (setq e (specrepcheck e))
  (cond ((alike1 e x) t)
        ((mnump e) nil)
        ((and (symbolp e) (setq l (mget e 'depends)))
         ;; Go recursively through the list of dependencies.
         ;; This code detects indirect dependencies like a(x) and x(t).
         (dependsl l x))
        ((atom e) nil)
        (t (or (depends (caar e) x)
               (dependsl (cdr e) x)))))

(defun dependsl (l x)
  (dolist (u l)
    (if (depends u x) (return t))))

