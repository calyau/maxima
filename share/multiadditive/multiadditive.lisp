#|
  Copyright 2006 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Examples:

Declaring a function to be multiadditive makes it additive 
in all of its arguments; declaring a function to be additive
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

The order of *opers-list matters. For example, if f is threadable and
an involution, then f(f([1,2])) would simplify to f([f(1),f(2)]) if
the threadable rule was used first, or it would simplify to [1,2] if
the the involution rule was used first. A user doesn't have any control
over the order the rules are applied. There is a user-level list $opproperties, 
but re-ordering $opproperties doesn't change the order the rules are applied.
|#

;; As of 2 June 2006, simplifya doesn't check for a subscripted function
;; before sending it to oper-apply. I don't think this is what we want:
;; declare(f,multiadditive), f[x+y] --> f[x] + f[y]. And f[x+y](a+b) --> error.
;; For now, these functions check for subscripted arguments.
;; TODO: MOVE THIS BUG FIX TO SIMPLIFYA OR OPER-APPLY !!

;; When e is a mapatom, the function call (oper-apply e z) gives an
;; error. I think oper-apply should be changed so that its first
;; argument can be a mapatom. Till then:
;; TODO: MOVE THIS BUG FIX TO SIMPLIFYA OR OPER-APPLY !!

(defun protected-oper-apply (e z)
  (if ($mapatom e) e (oper-apply e z)))
	
(defun multiadditive (e)
  (cond ((and (not ($subvarp e)) (some #'(lambda (s) (op-equalp s 'mplus)) (margs e)))
	 (let ((op (mop e)) (args (margs e)))
	   (setq args (mapcar #'(lambda (s) (if (op-equalp s 'mplus) (margs s) (list s))) args))
	   (setq args (apply 'cartesian-product args))
	   (setq args (mapcar #'(lambda (s) (simplify `((,op) ,@s))) args))
	   (reduce 'add args)))
	(t e)))

;; ((op) bag) --> map(op bag).

(defun threadable (e)
  (let ((arg (margs e)) (fop) (bop)) ;; fop = function operator and bop = bag operator.
    (cond ((and (= 1 (length arg)) (not ($subvarp e))
		(or (mbagp (first arg)) (op-equalp (first arg) '$set)))
	   (setq arg (first arg))
	   (setq fop (mop e))
	   (setq bop (mop arg))
	   `((,bop) ,@(mapcar #'(lambda (s) (mfuncall fop s)) (margs arg))))
	  (t e))))

;; ((op) ((op) x)) --> ((op) x).
;; Good test: declare(f,idempotent), f[5](x).

(defun idempotent (e)
  (if (and (not ($subvarp e)) 
  	 (= 1 (length (margs e))) 
  	 (not ($mapatom (first (margs e))))
  	 (eq (mop e) (mop (first (margs e)))))
      (first (margs e)) e))

;; ((op) ((op) x)) --> x. 
;; Good test: declare(f,involution), f[5](x).

(defun involution (e)
  (if (and (not ($subvarp e)) 
  	 (= 1 (length (margs e)))
  	 (not ($mapatom (first (margs e))))
  	 (eq (mop e) (mop (first (margs e))))
  	 (= 1 (length (margs (first (margs e))))))
      (first (margs (first (margs e)))) e))


