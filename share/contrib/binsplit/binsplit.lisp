
;; exp, log, sin, asin and friends for bigfloats in Maxima
;;
;; An implementation of the binary splitting algorithms
;;
;;    described in http://www.ginac.de/CLN/binsplit.pdf .
;;
;; 
;; Copyright (C) 2014 by Volker van Nek
;; 
;; Released under the terms of the GNU General Public License
;; 

;;
;; Functions at Maxima level     
;;                               
;; bs_asin(x), bs_acos(x), where 0 <= x <= 1 
;; bs_atan(x), bs_acot(x)        0 <= x
;; bs_sin(x),  bs_cos(x)         0 <= x <= pi/2
;; bs_tan(x),  bs_cot(x)         0 <= x < pi/2
;; bs_exp(x)                     0 <= x < 1
;; bs_log(x)                     x > 0
;; bs_expm1(x)                   0 <= x < log(2)
;; bs_log1p(eps)                 eps > -1
;;                               x and eps are real valued bigfloats
;;

;;----------------------------------------------------------------------------;;
;; Compute phi = asin(x) or acos(x), where 0 <= x <= 1 
;;
;;      or phi = atan(x) or acot(x), where 0 <= x. 
;;
(defun $bs_asin (x) (bs-asincos x 'asin)) ;; top level test functions
(defun $bs_acos (x) (bs-asincos x 'acos))
(defun $bs_atan (x) (bs-asincos x 'atan))
(defun $bs_acot (x) (bs-asincos x 'acot))

(defun bs-asincos (x fn)
  (let ((phi ;; phi = carg(z), where ..
         (let* ((fpprec (+ fpprec 12)) ;; needs testing
                (x (cdr (bigfloatp x)))
                (xx (fptimes* x x))
                (one (fpone))
                 y re-z im-z )
           (cond
             ((or (eq fn 'asin) (eq fn 'acos))
               (setq y (fproot (bcons (fpdifference one xx)) 2))
               (if (eq fn 'asin)
                 ;; asin: .. z = sqrt(1-x^2) + %i * x
                 (setq re-z y im-z x)
                 ;; acos: .. z = x + %i * sqrt(1-x^2)
                 (setq re-z x im-z y) ))
             (t
               (setq y (fpquotient one (fproot (bcons (fpplus one xx)) 2)))
               (if (eq fn 'atan)
                 ;; atan: .. z = 1/sqrt(x^2+1) + %i * x/sqrt(x^2+1)
                 (setq re-z y im-z (fptimes* x y))
                 ;; acot: .. z = x/sqrt(x^2+1) + %i * 1/sqrt(x^2+1)
                 (setq re-z (fptimes* x y) im-z y) )))
           (bcons (bs-carg1 re-z im-z fpprec)) )))
    (bigfloatp phi) ))

;;----------------------------------------------------------------------------;;
;;
;;----------------------------------------------------------------------------;;
;; Compute phi = carg(z) in quadrant I, where |z| = 1. 
;;
;; im(z) < 1/2: Algorithm bs-carg11 moves z along the unit circle down to the real axis 
;;
;;    while counting the angular movement (Haible & Papanikolaou, 2.2.8).
;;
;; im(z) < 1/sqrt(2): phi/2 = carg(sqrt(z)).
;;
;; im(z) = 1/sqrt(2): phi = pi/4.
;;
;; im(z) > 1/sqrt(2): pi/2 - phi = carg(imagpart(z) + %i*realpart(z)).
;; 
(defun bs-carg1 (re-z im-z prec) ;; assumes |z| = 1, quadrant 1
  (cond 
    ((zerop (car (fpdifference re-z im-z)))
      (let ((%pi (fppi))) (list (car %pi) (- (cadr %pi) 2))) ) ;; pi/4
    ((fpgreaterp im-z re-z) ;; im-z > 1/sqrt(2) (i.e. phi > pi/4)
      (let* ((%pi (fppi)) 
             (%pi/2 (list (car %pi) (1- (cadr %pi)))) ) 
        (fpdifference %pi/2 (bs-carg1 im-z re-z prec)) ))
    (t
      (let ((e 0) phi) 
        ;; bs-carg11 needs im-z < 1/2  (i.e. phi < pi/6)
        ;; when im-z >= 1/2 bisect phi by taking the complex sqrt
        (when (not (fplessp im-z (cdr bfhalf)))
          (multiple-value-setq (re-z im-z) (complex-sqrt (bcons re-z) (bcons im-z)))
          (setq e 1) )
        (setq phi (bs-carg11 (list re-z im-z) prec))
        (list (car phi) (+ (cadr phi) e)) ))))
;;
(defun bs-carg11 (z prec) ;; assumes |z| = 1, 0 < Re(z) <= 1, 0 <= Im(z) < 1/2
  (let ((one (fpone)) 
         k u e alpha )
    (do ((phi (list 0 0)) 
         (i 1 (1+ i))
         (im-z (cadr z) (cadr z)) ) 
        ((not (fpgreaterp (fpplus one im-z) one)) phi)  ;; stop when im-z = 0 within current prec
      (setq k (- (cadr im-z)) ;; im-z < 2^-k
            u (ldb (byte k (- prec k)) (car im-z)) ;; first k significant mantissa bit
            e (complex-exp-%i*u/2^2k (- u) k prec)
            z (complex-fptimes* z e)
            alpha (intofp u)
            alpha (list (car alpha) (- (cadr alpha) (* 2 k)))
            phi (fpplus phi alpha) ))))
;;
(defun complex-exp-%i*u/2^2k (u k prec) ;; ;; compute exp(%i*u/2^2k) where u < 2^k, integer u,k
  (if (= 0 u) 
    (list (fpone) (list 0 0))
    (let ((z (* 2 k))
          (kk k) ) ;; see (**) below why k is param for bs-exp-u/2^z-series-size
      (complex-exp-%i*u/2^z u z kk prec) )))
;;
(defun split-bs-exp-u/2^z (i j u z) ;; u is integer or complex with integer parts
  (let (pp qq zz tt)
    (if (= (- j i) 1) 
      (if (= i 0) 
        (setq  pp 1  qq 1  zz 0  tt 1)
        (setq  pp u  qq i  zz z  tt pp) )
      (let ((m (ash (+ i j) -1))) 
        (multiple-value-bind (tl zl ql pl) (split-bs-exp-u/2^z i m u z)
          (multiple-value-bind (tr zr qr pr) (split-bs-exp-u/2^z m j u z)
            (setq pp (* pl pr)
                  qq (* ql qr)
                  zz (+ zl zr)
                  tt (* qr tl)
                  tt (if (complexp u)
                       (apply #'complex 
                              (mapcar #'(lambda (n) (ash n zr)) 
                                      (list (realpart tt) (imagpart tt)) ))
                       (ash tt zr) )
                  tt (+ tt (* pl tr)) )))))
    (values tt zz qq pp) ))
;;
(defun complex-fptimes* (a b)
  (let ((x1 (car a)) (y1 (cadr a))
        (x2 (car b)) (y2 (cadr b)) )
    (list 
      (fpdifference (fptimes* x1 x2) (fptimes* y1 y2))
      (fpplus (fptimes* x1 y2) (fptimes* x2 y1)) )))
;;
(defun complex-fpsquare (a)
  (let* ((x (car a)) (y (cadr a))
         (xy (fptimes* x y)) )
    (list 
      (fpdifference (fptimes* x x) (fptimes* y y))
      (list (car xy) (1+ (cadr xy))) )))
;;
;;----------------------------------------------------------------------------;;


;;----------------------------------------------------------------------------;;
;; Compute sin(x), cos(x), tan(x), cot(x), where 0 <= x <= %pi/2 
;;
(defun $bs_sin (x) (bs-sincos x 'sin)) ;; top level test functions
(defun $bs_cos (x) (bs-sincos x 'cos))
(defun $bs_tan (x) (bs-sincos x 'tan))
(defun $bs_cot (x) (bs-sincos x 'cot))
;;
(defun bs-sincos (x fn) ;; 0 <= x <= pi/2
  (let ((xx (cdr x)) pi/1 pi/2 dx)
    (cond
      ((or (= 0 (car xx)) (< (cadr xx) (- fpprec))) ;; x is zero within current prec
        (cond ((eql fn 'sin) xx)
              ((eql fn 'cos) (intofp 1))
              ((eql fn 'tan) xx)
              ((eql fn 'cot) (merror (intl:gettext "attempted quotient by zero."))) ))
      ((and (setq pi/1 (fppi)
                  pi/2 (list (car pi/1) (1- (cadr pi/1)))
                  dx (fpdifference pi/2 xx) )
            (or (= 0 (car dx)) (< (cadr dx) (- fpprec))) ) ;; x is pi/2 within current prec
        (cond ((eql fn 'sin) (intofp 1))
              ((eql fn 'cos) dx)
              ((eql fn 'tan) (merror (intl:gettext "attempted quotient by zero.")))
              ((eql fn 'cot) dx) ))
      (t
        (let ((res
               (let* ((fpprec (+ fpprec 12)) ;; needs testing
                      (x (cdr (bigfloatp x))) )
                 (bcons 
                   (multiple-value-bind (re im) (bs-exp-%i*x x fpprec)   
                     (cond ((eq fn 'sin) im)
                           ((eq fn 'cos) re)
                           ((eq fn 'tan) (fpquotient im re))
                           ((eq fn 'cot) (fpquotient re im)) ))))))
          (bigfloatp res) )))))
;;
(defun bs-exp-%i*x (x prec) ;; assume 0 <= x <= pi/2
  (let ((mant (car x)) (e (cadr x))
        (acc (list (intofp 1) (list 0 0))) ;; complex-(1,0)
         x/2? ) 
    (when (fpgreaterp x (floattofp (/ pi 4))) ;; is x > pi/4 ?
      (setq e (1- e) ;; x <-- x/2
            x/2? t ))
    ;; now x <= pi/4 (the alogrithm needs x < 1)
    (do ((k 0 (1+ k))
         (nr 1)  
         (pos1 1 (ash pos1 1))
         (pos2 1) u tmp )
        ((= 0 pos2))
      (setq pos2 (- prec (+ pos1 e)))
      (when (< pos2 0) 
        (setq mant (ash mant (- pos2)) 
              pos2 0 ))
      (setq u (ldb (byte nr pos2) mant) 
            tmp (complex-exp-%i*u/2^2^k u k prec)
            acc (complex-fptimes* acc tmp)
            nr pos1 ))
    (when x/2? (setq acc (complex-fpsquare acc))) ;; exp(%i*x) = exp(%i*x/2)^2
    (apply #'values acc) ))
;;
(defun complex-exp-%i*u/2^2^k (u k prec) ;; compute exp(%i*u/2^2^k) where u < 2^2^(k-1) resp. 2^-1 for k=0, integer u,k
  (if (= 0 u) 
    (list (fpone) (list 0 0))
    (let ((z (ash 1 k))
          (kk (if (= k 0) 1/2 (ash 1 (1- k)))) ) ;; param for bs-exp-u/2^z-series-size, see (*)
      (complex-exp-%i*u/2^z u z kk prec) )))
;;
(defun complex-exp-%i*u/2^z (u z kk prec)
  (let ((nr (bs-exp-u/2^z-series-size kk prec))
         x y )
    (do ((i 0)) 
        ((logbitp i u)
          (setq u (ash u (- i)) z (- z i)) )
      (incf i) )
    (multiple-value-bind (tt zz qq) (split-bs-exp-u/2^z 0 (1+ nr) (complex 0 u) z)
      (setq qq (intofp qq)
            x (fpquotient (intofp (realpart tt)) qq)
            y (fpquotient (intofp (imagpart tt)) qq)
            x (list (car x) (- (cadr x) zz))
            y (list (car y) (- (cadr y) zz)) )
      (list x y) )))
;;
;;----------------------------------------------------------------------------;;


;;----------------------------------------------------------------------------;;
;; exp(x)-1 for 0 <= x < log(2)
;;
;; x = mantissa*2^e = 2^(e-1) + ... where e <= 0
;;
;; exp(x) = 1 + x + x^2/2! + ... = 1 + 2^(e-1) + ... 
;;
;; exp(x)-1 looses |e-1| bits precision
;;
(defun $bs_expm1 (x) (bs-expm1 x)) ;; top level test function

(defun bs-expm1 (x) ;; 0 <= x < log(2)
  (setq x (bigfloatp x))
  (let* ((extra (1+ (- (caddr x))))
         (res 
           (let ((fpprec (+ fpprec extra)))
             (bcons (fpdifference (cdr (bs-exp x)) (fpone))) )))
    (bigfloatp res) ))
;;
;;----------------------------------------------------------------------------;;
;;
;; exp(x), real 0 <= x < 1
;;
(defun $bs_exp (x) (bs-exp x)) ;; top level test function

(defun bs-exp (x) ;; assume 0 <= x < 1
  (let ((exp1 
         (let* ((fpprec (+ fpprec 12)) ;; needs testing
                (x (cdr (bigfloatp x)))
                (mant (car x)) (e (cadr x)) 
                (res (fpone))
                (nr 1) ;; nr of extracted bits
                 u )
            (do ((k 0 (1+ k))  
                 (pos1 1 (ash pos1 1))
                 (pos2 1) )
                ((= 0 pos2) (bcons res))
              (setq pos2 (- fpprec (+ pos1 e)))
              (when (< pos2 0) 
                (setq mant (ash mant (- pos2)) 
                      pos2 0 ))
              (setq u (ldb (byte nr pos2) mant)
                    res (fptimes* res (bs-exp-u/2^2^k u k fpprec))
                    nr pos1) ))))
    (bigfloatp exp1) ))
;;
(defun bs-exp-u/2^2^k (u k prec) ;; compute exp(u/2^2^k) where u < 2^2^(k-1), integer u,k (k >= 0)
  (if (= 0 u) 
    (fpone)
    (let ((z (ash 1 k))
          (kk (if (= k 0) 1/2 (ash 1 (1- k)))) ) ;; param for bs-exp-u/2^z-series-size, see (*)
      (bs-exp-u/2^z u z kk prec) )))
;;
;; (*) End the Taylor-series when (u/2^2^k)^i/i! < 1/2^fpprec.
;;
;; u might not fit into a float. Use bound u < 2^2^(k-1), i.e. u/2^2^k < 1/2^2^(k-1).
;;
;; So end when i!*(2^2^(k-1))^i > 2^fpprec or 
;;
;; log(i!) + i * 2^(k-1)*log(2) = sum(log(n) + 2^(k-1)*log(2), n,1,i) > fpprec * log(2).
;;
(defun bs-exp-u/2^z-series-size (kk prec)
  (setq kk (* kk (log 2)))
  (do ((n 1 (1+ n))
       (acc 0)
       (lim (* prec (log 2))) )
      ((> acc lim) n)
    (incf acc (+ (log n) kk)) ))
;;
(defun bs-exp-u/2^z (u z kk prec) ;; compute exp(u/2^z), integer u,z
  (let (nr tt/qq)
    (do ((i 0)) 
        ((logbitp i u)
          (setq u (ash u (- i)) 
                z (- z i) ))
      (incf i) )
    (setq nr (bs-exp-u/2^z-series-size kk prec)) 
    (multiple-value-bind (tt zz qq) (split-bs-exp-u/2^z 0 (1+ nr) u z)
      (setq tt/qq (fpquotient (intofp tt) (intofp qq)))
      (list (car tt/qq) (- (cadr tt/qq) zz)) )))
;;----------------------------------------------------------------------------;;


;;----------------------------------------------------------------------------;;
;; 
;; Compute log(1 + eps), real eps > -1
;;
(defun $bs_log1p (eps) (bs-log1p eps)) ;; top level test function

(defun bs-log1p (eps) ;; eps = x - 1
  (setq eps (bigfloatp eps))
  (let ((res 
         (let* ((extra (1+ (- (caddr eps)))) ;; give |e-1| extra bits
               (fpprec (+ fpprec extra))
               ;; x = 1 + eps = m*2^e = 2^(e-1) + ...
               (x (bcons (fpplus (cdr (bigfloatp eps)) (fpone)))) ) 
            (bs-log x) )))
    (bigfloatp res) ))
;;
;;----------------------------------------------------------------------------;;
;; 
;; Compute log(x), real x > 0
;;
;; Use log(x) = log(x/2^k) + k*log(2), where x/2^k < 1.
;;
(defun $bs_log (x) (bs-log x)) ;; top level test function

(defun bs-log (x) ;; assume x > 0
  (let ((res 
         (let* ((fpprec (+ fpprec 12)) ;; needs more testing
                (x (cdr (bigfloatp x)))
                (m (car x)) (k (cadr x)) ;; mantissa, exponent
                ($float2bf t)
                 lgx )
           (setq x (list m 0)) ;; now x = 0 or 1/2 <= x < 1
           ;; choose k so that |x - 1| is as small as possible:
           (when (fplessp x (floattofp (coerce 2/3 'flonum)))
             (setq x (list m 1))
             (decf k) )
             ;; now |x - 1| <= 1/3 < 1/2
           (setq lgx (bs-log1 x fpprec))
           (bcons 
             (if (= k 0) 
               lgx 
               (fpplus lgx (fptimes* (intofp k) (comp-log2))) )))))
    (bigfloatp res) ))
;;
(defun bs-log1 (x prec) ;; assume |x - 1| < 1/2
  (let ((one (fpone))
         dx posp u e z )
    (do ((y (list 0 0)) (k 0) m-len (pos 1))
        ((= 0 pos) y)
      (setq dx (fpdifference x one)
            posp (fpposp dx)
            dx (fpabs dx) )
      (unless (fpposp dx) (return y))
      (setq k (- (cadr dx))) ;; |x - 1| < 2^-k
      (setq m-len (integer-length (car dx)) ;; m-len isn't always prec
            pos (- m-len k)
            u (ldb (byte k pos) (car dx)) ;; first k significant mantissa bits
            u (if posp u (- u))
            e (bs-exp-u/2^2k (- u) k prec)
            x (fptimes* x e)
            z (intofp u)
            z (list (car z) (- (cadr z) (* 2 k)))
            y (fpplus y z) ))))
;;
(defun bs-exp-u/2^2k (u k prec) ;; compute exp(u/2^2k) where u < 2^k, integer u,k (k >= 0)
  (if (= 0 u) 
    (fpone)
    (bs-exp-u/2^z u (* 2 k) k prec) )) ;; k is param for bs-exp-u/2^z-series-size, see (**)
;;
;; (**) End the Taylor-series when (u/2^(2*k))^i/i! < 1/2^fpprec.
;;
;; u might not fit into a float. Use bound u < 2^k, i.e. u/2^(2*k) < 1/2^k.
;;
;; So end when i!*(2^k)^i > 2^fpprec or 
;;
;; log(i!) + i * k*log(2) = sum(log(n) + k*log(2), n,1,i) > fpprec * log(2).
;;
;;----------------------------------------------------------------------------;;

