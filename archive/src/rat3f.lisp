;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(import '(compiler::inline-unsafe compiler::inline-always compiler::boolean
    compiler::definline ) 'cl-maxima)
(macsyma-module rat3f)

(clines "#include \"rat3f-hc.c\"")


;;plan make file crat.c to include in the macsyma build
;;it will have the necessary primitives, and we will then
;;put inline things for ctimes,...
;;the symbol-value cell of modulus will be snarfed, and 
;;consulted by the ctimes and friends.
;; make #ifdef MC68020 for the ftimes and dblrem stuff,
;; but add defs that will work on the vax.
;;kclrat.lisp file to be loaded before compiling rat3a
;;cplus,etc commented out for kcl


(eval-when (compile)
  (setf (get 'cload-time 'compiler::t1) #'(lambda (&rest l)
					   (push (list 'load-time (car l))
						 compiler::*top-level-forms*)))
  (setf (get 'cload-time 'compiler::t2) #'(lambda (&rest l)
					    (apply 'compiler::wt-nl (car l)))))




(defmacro definline
    (property return-type side-effect-p new-object-p name arg-types
            body)
 `(push 
	 '(,arg-types ,return-type ,side-effect-p ,new-object-p ,body)
	(get ',name ',property)))



;;the bignum mod does not work.
;(defentry fplus  (object object) (object fplus))
(defentry fplus  (int int) (object fplus))
(defentry my-mcmod  (object object) (object mcmod))
(defentry myctimes  (object object object) (object ctimes))
(defentry mycplus  (object object object) (object cplus))
(defentry mycdifference  (object object object) (object cdifference))
(defentry my-doublerem (int int int) (int dblrem))
(defentry plusrem (int int int) (int plusrem))
(defentry subrem (int int int) (int subrem))

(defun new-ctimes (x y)
   (myctimes x y modulus))

(defun new-cplus (x y)
   (mycplus x y modulus))

(defun new-cdifference (x y)
   (mycdifference x y modulus))

(defun cdiff (x y)
  (let ((res (mod (- x y) modulus)))
    (cond ((> res (floor modulus 2))
	   (- (mod res modulus) modulus))
	  (t res))))


#+debug
(defun comp (modulus &aux (bi most-positive-fixnum) (li most-negative-fixnum)
	       ( lis (list bi bi li (+ bi 1) (- li 3) )))
  (sloop for u in lis
	 do (sloop for w in lis
		   when (not (equal (new-cplus u w) (cplus u w)))
		   do (print (list 'bad (list u w (fixnump u)(fixnump w)))))))

#+debug
(defun comp (modulus &aux (bi most-positive-fixnum) (li most-negative-fixnum)
	       ( lis (list 7 8 bi bi li (+ bi 1) (- li 3) )))
  (sloop for u in lis
	 do (sloop for w in lis with nans and ans
		   when (not (equal (setq nans (new-cdifference u w))
				    (setq ans(cdiff u w))))
		   do (print (list 'bad nans ans (list u w (fixnump u)(fixnump w)))))))
;(BAD (2147483647 -2147483648 T T))

#+debug
(defun te (x n)
  (let ((a (my-mcmod x n))
	(b (let ((modulus n)) (mcmod x n))))
    (list (- a b) a b)))
    

#+debug
(defun te (x n)
  (let ((a (+ x n))
	(b (fplus x n)))
    (list (- a b) a b)))
    
#+debug
(progn
 (setf (symbol-function 'cplus) (symbol-function 'new-cplus))
 (setf (symbol-function 'ctimes) (symbol-function 'new-ctimes))
 (setf (symbol-function 'cdifference) (symbol-function 'new-cdifference)))


(progn
(proclaim '(function ptimes (t t) t))
(proclaim '(function ptimes1 (t t) t))
(proclaim '(function pctimes (t t) t))
(proclaim '(function pctimes1 (t t) t))

(proclaim '(function pplus (t t) t))
(proclaim '(function pplus1 (t t) t))
(proclaim '(function pcplus (t t) t))
(proclaim '(function pcplus1 (t t) t))

(proclaim '(function pdifference (t t) t))
(proclaim '(function pdiffer1 (t t) t))
(proclaim '(function pcdiffer (t t) t))
(proclaim '(function pcdiffer1 (t t) t))


(proclaim '(function psimp1 (t t) t))
(proclaim '(function palgsimp (t t t) t))
(proclaim '(function alg (t) t))

(definline inline-always boolean nil nil pointergp (t t)
           "((fix((#0)->s.s_dbind)) > fix(((#1)->s.s_dbind)))")


(definline inline-always boolean nil nil pzerop (t )
  "(type_of(#0)==t_fixnum ?  (fix(#0)==0)
       :type_of(#0)==t_shortfloat ? (sf(#0)==0.0)
       :(type_of(#0)==t_longfloat && (lf(#0)==0.0)))")


)

;;fix pzerop to maybe assume fixnum or bignum entry??
;;thus just == will work!!




#+debug
(defun comp (
  &aux (bi most-positive-fixnum) (li most-negative-fixnum)
	       ( lis (list bi bi li (+ bi 1) (- li 3) ))
	       (moduli (list nil 23 47 bi (+ bi 10)))
	       (funs '(new-cdifference cdifference new-ctimes
				       ctimes new-cplus cplus)))
  (sloop for (fun1 fun2) on funs by 'cddr
	 do
  (sloop for m in moduli
	  do
	  (let ((modulus m))
  (sloop for u in lis
	 do (sloop for w in lis
		   do (setq u (cmod u) w (cmod w))
		   when (not (equal (setq ans1(funcall fun1 u w))
				    (setq ans2(funcall fun2 u w))))
		   do (print
		       (list 'bad ans1 ans2
			     (list fun1 u w modulus
				   (fixnump u)(fixnump w)(fixnump modulus)))
		       )))))))
