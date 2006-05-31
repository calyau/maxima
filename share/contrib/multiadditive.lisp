#|
  Copyright 2006 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Examples:

Declaring a function to be multiadditive makes it additive 
in all of its arguments; declaring a funtion to be additive
makes it additive in just its first argument.  Examples:

(%i1) load("multiadditive")$
(%i2) declare(f,multiadditive);
(%o2) done
(%i3) f(x+y,a+b);
(%o3) f(y,b)+f(y,a)+f(x,b)+f(x,a)
(%i4) f(x+y+z,a+b);
(%o4) f(z,b)+f(z,a)+f(y,b)+f(y,a)+f(x,b)+f(x,a)
(%i5) declare(g,additive)$
(%i6) g(x+y,a+b);
(%o6) g(y,b+a)+g(x,b+a)
|#

;; When e is a mapatom, the function call (oper-apply e z) gives an
;; error. I think oper-apply should be changed so that its first
;; argument can be a mapatom. Till then:

(defun protected-oper-apply (e z)
  (if ($mapatom e) e (oper-apply e z)))

(setq opers (cons '$multiadditive opers)
      *opers-list (cons '($multiadditive . multiadditive) *opers-list))

(setq $opproperties ($cons '$multiadditive $opproperties))

(defun multiadditive (e z)
  (cond ((some #'(lambda (s) (op-equalp s 'mplus)) (margs e))
	 (let ((op (mop e)) (args (margs e)))
	   (setq args (mapcar #'(lambda (s) (if (op-equalp s 'mplus) ($args s) `((mlist) ,s))) args))
	   (setq args (mfuncall '$apply '$outermap ($cons op (cons '(mlist) args))))
	   (reduce 'add (mapcar #'(lambda (s) (protected-oper-apply s z)) (margs ($flatten args))))))
	(t (protected-oper-apply e z))))

(setq opers (cons '$threadable opers)
      *opers-list (cons '($threadable . threadable) *opers-list))

(setq $opproperties ($cons '$threadable $opproperties))

;; ((op) bag) --> map(op bag).

(defun threadable (e z)
  (let ((arg (margs e)) (fop) (bop)) ;; fop = function operator and bop = bag operator.
    (cond ((and (= 1 (length arg)) (or (mbagp (first arg)) (op-equalp (first arg) '$set)))
	   (setq arg (first arg))
	   (setq fop (mop e))
	   (setq bop (mop arg))
	   (simplify `((,bop) ,@(mapcar #'(lambda (s) (protected-oper-apply (mfuncall fop s) z)) 
					(margs arg)))))
	  (t (protected-oper-apply e z)))))

(setq opers (cons '$idempotent opers)
      *opers-list (cons '($idempotent . idempotent) *opers-list))

(setq $opproperties ($cons '$idempotent $opproperties))

;; ((op) ((op) x)) --> ((op) x)

(defun idempotent (e z)
  (protected-oper-apply (if (and (= 1 (length (margs e))) (eq (mop e) (mop (first (margs e)))))
		  (first (margs e)) e) z))

(setq opers (cons '$involution opers)
      *opers-list (cons '($involution . involution) *opers-list))

(setq $opproperties ($cons '$involution $opproperties))

;; ((op) ((op) x)) --> x.

(defun involution (e z)
  (protected-oper-apply (if (and (= 1 (length (margs e))) (eq (mop e) (mop (first (margs e))))
		       (= 1 (length (margs (first (margs e))))))
		  (first (margs (first (margs e)))) e) z))