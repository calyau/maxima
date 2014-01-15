;; -*- mode: lisp -*-
;; Copyright Leo Butler (l.butler@cmich.edu) 2014
;; Released under the terms of GPLv3+



(defun get-tex-environment* (x)
  (cond
    ((stringp x)
     '("" . ""))
    (t
     (get-tex-environment x))))

(defun $tex_displa (x)
  (let ((s ($tex1 (list (car x) (cadr x))))
	(e (mfuncall '$tex1 (caddr x)))
	(env (get-tex-environment* (caddr x))))
    (concatenate 'string (subseq s 0 (- (length s) 14)) (car env) e (cdr env))))
	    

;; end of tex-display.lisp 
