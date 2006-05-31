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

(setq opers (cons '$multiadditive opers)
      *opers-list (cons '($multiadditive . multiadditive) *opers-list))

(setq $opproperties ($cons '$multiadditive $opproperties))

(defun multiadditive (e z)
  (cond ((some #'(lambda (s) (op-equalp s 'mplus)) (margs e))
	 (let ((op (mop e)) (args (margs e)))
	   (setq args (mapcar #'(lambda (s) (if (op-equalp s 'mplus) ($args s) `((mlist) ,s))) args))
	   (setq args (mfuncall '$apply '$outermap ($cons op (cons '(mlist) args))))
	   (reduce 'add (mapcar #'(lambda (s) (oper-apply s z)) (margs ($flatten args))))))
	(t (oper-apply e z))))
