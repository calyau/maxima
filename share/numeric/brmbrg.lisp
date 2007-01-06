; -*- Lisp -*-
(declare-top (special $brombergit $brombergmin $brombergtol $brombergabs
		  $bfloat $float2bf)
	 (fixnum m l i k $brombergit $brombergmin))

(defun fpscale (x m)
       (cond ((equal (car x) 0) x)
	     (t (list (car x) (plus (cadr x) m)))))

(defun bfmeval3 (x1) 
       (cond (($bfloatp (setq x1 ($bfloat (meval x1)))) (cdr x1))
	     (t (displa x1) (merror "bromberg: encountered a non-bigfloat."))))

(defun bqeval3 (y1 x1 z)
       (setq z (bcons z))
       (cond (x1 (bfmeval3 (list '($ev) y1 (list '(mequal) x1 z) '$bfloat)))
	     (t (cdr (funcall y1 z)))))

(or (boundp '$brombergit) (setq $brombergit 11.)) 
(or (boundp '$brombergmin) (setq $brombergmin 0.))
(or (boundp '$brombergtol)
    (setq $brombergtol '((bigfloat simp 56.) 59029581035870565. -13.))) ;1.b-4
(or (boundp '$brombergabs)
    (setq $brombergabs '((bigfloat simp 56.) 0. 0.))) ; 0.0b0

(defun $bromberg (&rest l1) 
       (or (= (length l1) 4.) (= (length l1) 3.)
	   (merror "bromberg: wrong number of arguments."))
       ((lambda (fun var a b x $bfloat $float2bf lim limabs tt rr zero one three)
		(setq var (= (length l1) 4.)) ;var=nil ==> first arg is function name 
		(cond (var (setq fun (cond ((atom (car l1)) (meval (car l1)))
					   (t (car l1))) 
				 var (cadr l1)
				 l1 (cdr l1)))
		      (t (setq fun (car l1))
			 (or (get fun 'expr) (get fun 'subr) 
			     (get fun 'translated)
			     (displa fun)
			     (merror "bromberg: first argument doesn't appear to be a function."))))
		(setq a (bfmeval3 (cadr l1)) 
		      b (bfmeval3 (caddr l1))
		      x (fpdifference b a))
		(store (arraycall t tt 0.)
		       (fpscale (fptimes* x (fpplus (bqeval3 fun var b)
						    (bqeval3 fun var a)))
				-1))
		(store (arraycall t rr 0.)
		       (fptimes* x (bqeval3 fun var (fpscale (fpplus b a) -1))))
		(do ((l 1. (1+ l)) (m 4. (* m 2.)) (y) (z) (cerr nil))
		    ((= l $brombergit) (merror "bromberg: failed to converge."))
		    (setq y (intofp m) z (fpquotient x y))
		    (store (arraycall t tt l)
			   (fpscale (fpplus (arraycall t tt (1- l))
					    (arraycall t rr (1- l))) -1))
		    (store (arraycall t rr l) zero)
		    (do ((i 1. (+ i 2.)))
			((> i m))
			(store (arraycall t rr l)
			       (fpplus (bqeval3 fun var (fpplus (fptimes* z (intofp i))
								a))
				       (arraycall t rr l))))
		    (store (arraycall t rr l) (fpscale (fptimes* z (arraycall t rr l))
						       1))
		    (setq y zero)
		    (do ((k l (1- k))) ((= k 0.))
			(setq y (fpplus (fpscale y 2) three))
			(store (arraycall t tt (1- k))
			       (fpplus (fpquotient
					(fpdifference (arraycall t tt k)
						      (arraycall t tt (1- k)))
					y)
				       (arraycall t tt k)))
			(store (arraycall t rr (1- k))
			       (fpplus (fpquotient
					(fpdifference (arraycall t rr k)
						      (arraycall t rr (1- k)))
					y)
				       (arraycall t rr k))))
		    (setq y (fpscale (fpplus (arraycall t tt 0.)
					     (arraycall t rr 0.)) -1))
		    (cond ((and
			    (or (not
				 (fplessp
				  limabs
				  (setq cerr
					(fpabs (fpdifference (arraycall t tt 0.)
							     (arraycall t rr 0.))))))
				(not (fplessp lim
					      (fpquotient
					 cerr
					 (cond ((equal y '(0 0)) one)
					       (t (fpabs y)))))))
			    (> l $brombergmin))
			   (return (bcons y))))))
	nil nil nil nil nil t t (cdr ($bfloat $brombergtol))
	(cdr ($bfloat $brombergabs))
	(*array nil t $brombergit) (*array nil t $brombergit)
	(intofp 0) (intofp 1) (intofp 3))) 
