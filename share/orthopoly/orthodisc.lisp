(eval-when (compile load eval) ($load "orthopoly"))

(defmvar $hyper_f_tol 1.0e-18)
(defmvar max-fpprec 1000)

(defun float-or-bfloatp (s)
  (or (floatp s) ($bfloatp s)))

(defun $hyper_f (a b x)
 
  (if (not ($listp a))
      (merror "The first argument to hyper_f must be a list, instead
found ~:M" a))
  
  (if (not ($listp a))
      (merror "The second argument to hyper_f must be a list, instead
found ~:M" b))

  (setq a (cdr a))
  (setq b (cdr b))
  (setq b (cons 1 b))
  (let ((n -1)
	(use-float nil))
    (dolist (ai a)
      (if (and (integerp ai) (< ai 0))
	  (setq n (max n (- ai)))))

    (setq use-float (and 
		     (or (some 'float-or-bfloatp a) 
			 (some 'float-or-bfloatp b)
			 (float-or-bfloatp x))
		     (every '$numberp a) (every '$numberp b) ($numberp x)))
    
    (cond ((= n -1)
	   (setq a (mapcar #'(lambda (s) `(($pochhammer simp) ,s ,$genindex)) a))
	   (setq b  (mapcar #'(lambda (s) `(($pochhammer simp) ,s ,$genindex)) b))
	   `((%sum)
	     ((mtimes) ,@a ((mexpt) ,x ,$genindex)
	      ((mexpt) ((mtimes) ,@b) -1))
	     ,$genindex 0 $inf))
	  (use-float
	   (hyper-f-float a b x))
	  (t
	   (let ((sum 1) (cf 1))
	     (dotimes (i n sum)
	       (setq cf (div 
			 (mul x cf (apply 'mul 
					  (mapcar #'(lambda (s) (add i s)) a)))
			 (apply 'mul (mapcar #'(lambda (s) (add i s)) b))))
	       (setq sum (add sum cf))))))))

(defun hyper-f-float (aa bb xx &optional (tol $hyper_f_tol))
  (let ((n -1)
	(continue t) (cf) (sum) (err) (save-fpprec $fpprec)
	($float2bf t) (new-digits) (bf-digits) (a) (b) (x) (i) (m))
    (dolist (ai aa)
      (if (and (integerp ai) (< ai 0))
	  (setq n (max n (- ai)))))
    (setq bf-digits (* 2 (max 19 (- (ceiling (/ (log tol) (log 10.0)))))))
    (fpprec1 nil bf-digits)
    (setq $fpprec bf-digits)
    (setq m (* 2 (+ (length aa) (length bb) 1)))
    
    (while continue
      (if (> $fpprec max-fpprec)
	  (merror "Unable to obtain requested precision"))
      (print `(fpprec = ,$fpprec))
      (setq a (mapcar '$bfloat aa))
      (setq b (mapcar '$bfloat bb))

      (setq x ($bfloat xx))
      (setq sum bigfloatone)
      (setq err bigfloatzero) 
      (setq i (- n 1))
      
      (while (>= i 0)
	(setq cf (div (mul x (apply 'mul (mapcar #'(lambda (s) (add i s)) a)))
		      (apply 'mul (mapcar #'(lambda (s) (add i s)) b))))
	(setq err (add bigfloatone (mul m (simplify `((mabs) ,cf)) err)))
	(setq cf (mul sum cf))
	(setq sum (add bigfloatone cf))
	(setq err (add err (mul (+ 2 m) (simplify `((mabs) ,cf)))))
	(decf i))
     
      (setq new-digits (- (/ (log (/ ($float tol) ($float err))) (log 10.0))))
      (setq err (mul err (bfloat-epsilon)))
      (setq new-digits (+ 10 (max $fpprec (ceiling new-digits))))
      (fpprec1 nil new-digits)
      (setq $fpprec new-digits)
      (setq continue (mgrp err tol)))

    (displa sum)

    (cond ((or (some '$bfloatp aa) (some '$bfloatp bb) ($bfloatp xx))
	   (fpprec1 nil bf-digits)
	   (setq $fpprec bf-digits)
	   (setq sum ($bfloat sum)))
	  (t
	   (setq sum ($float sum))))
    (fpprec1 nil save-fpprec)
    (setq $fpprec save-fpprec)
    sum))
 
(defun bfloat-epsilon ()
  (power ($bfloat 2) (- fpprec)))
    
(defun $krawtchouk (n a b x)
  (cond ((use-hypergeo n x)
	 (let ((f) (e))
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) (mul -1 x) (mul -1 a) (div 1 b) n))
	   (orthopoly-return-handler 1 f e)))
	(t `(($krawtchouk simp) ,n ,a ,x))))

(defun $meixner_m (n g mu x)
  (cond ((use-hypergeo n x)
	 (let ((d) (f) (e))
	   (setq d ($pochhammer g n))
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) (mul -1 x) g (sub 1 (div 1 mu)) n))
	     (orthopoly-return-handler d f e)))
	(t `(($meixner_m simp) ,n ,g ,mu ,x))))

(defun $charlier_c (n mu x)
  (cond ((use-hypergeo n x)
	 (let ((f) (e))
	   (multiple-value-setq (f e)
	     ($hyper_f `((mlist) ,(mul -1 n) ,(mul -1 x)) `((mlist)) (div -1 mu)))
	   (orthopoly-return-handler 1 f e)))
	(t `(($charlier_c simp) ,n ,mu ,x))))

(defun $hahn_h (n m a b x)
  (cond ((use-hypergeo n x)
	 (let ((f) (d) (e))
	   (setq d (mul ($pochhammer (sub m n) n)
			($pochhammer (add 1 b) n)))
	   (if (oddp n) (setq d (mul -1 d)))
	   (setq d (div d ($pochhammer 1 n)))
	   (multiple-value-setq (f e)
	     ($hyper_f `((mlist) ,(mul -1 n) ,(mul -1 x) ,(add a b n 1)) 
		       `((mlist) ,(add 1 b) ,(sub 1 m)) 1))
	   (orthopoly-return-handler d f e)))
	(t `(($hahn_h simp) ,n ,m ,a ,b ,x))))

(defun $hahn_q (n m a b x)
  (cond ((use-hypergeo n x)
	 (let ((f) (e))
	   (multiple-value-setq (f e)
	     ($hyper_f `((mlist) ,(mul -1 n) ,(mul -1 x) ,(add a b n 1)) 
		       `((mlist) ,(add 1 a) ,(mul -1 m)) 1))
	     (orthopoly-return-handler 1 f e)))
	(t `(($hahn_q simp) ,n ,m ,a ,b ,x))))

(defun $discrete_chebyshev_h (n m x)
  ($hahn_h n m 0 0 x))

(defun $discrete_jacobi_p (n m a b x)
  ($hahn_h n m a b (div (mul (add x 1) (sub m 1)) 2)))

(defun $discrete_legendre_p (n m x)
  ($discrete_jacobi_p n m 0 0 x))

(defun $orthodisc_weight (fn arg)
  
  (if (not ($listp arg)) 
      (merror "The second argument to orthodisc_weight must be a list"))
  
  (if (not (or ($symbolp (car (last arg))) ($subvarp (car (last arg)))))
      (merror "The last element of the second argument to orthopoly_disc must
be a symbol or a subscripted symbol, instead found ~:M" (car (last arg))))

  (if (not (every #'(lambda (s) 
		      ($freeof (car (last arg)) s)) (butlast (cdr arg))))
      (merror "Only the last element of ~:M may depend on the summation
variable ~:M" arg (car (last arg))))

  (cond ((eq fn '$discrete_chebyshev_h)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((m (nth 2 arg)))
	   `((mlist) 1  ((lambda) ((mlist) k) k) 0 ,($entier m))))
	(t (merror "The weight for ~:M isn't known to maxima" fn))))

	 

