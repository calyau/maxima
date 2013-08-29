;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;;   *****************************************************************
;;;   ***** NUMTH ******* VARIOUS NUMBER THEORY FUNCTIONS *************
;;;   *****************************************************************

(macsyma-module numth)

(declare-top (special $intfaclim))

(load-macsyma-macros rzmac)

;;; Sum of divisors and Totient functions

(defmfun $divsum (n &optional (k 1))
  (let (($intfaclim nil))
    (if (and (integerp k) (integerp n))
      (let ((n (abs n)))
        (cond 
          ((= n 1) 1)
          ((zerop n) 1)
          ((zerop k)
            (do ((l (cfactorw n) (cddr l))
                 (a 1 (* a (1+ (cadr l)))))
                ((null l) a)))
          ((minusp k)
            `((rat) ,(divsum (cfactorw n) (- k)) ,(expt n (- k))))
          (t
            (divsum (cfactorw n) k))))
      (list '($divsum) n k))))

(defun divsum (l k)
  (do ((l l (cddr l))
       (ans 1))
      ((null l) ans)
    (unless (eql (car l) 1)
      (let ((temp (expt (car l) k)))
    (setq ans (* ans
      (truncate (1- (expt temp (1+ (cadr l))))
                (1- temp))))))))

(defmfun $totient (n)
  (cond 
    ((integerp n)
      (setq n (abs n))
      (cond 
        ((< n 1) 0)
        ((equal n 1) 1)
        (t (do ((factors (let ($intfaclim) (cfactorw n))
                  (cddr factors))
                (total 1 (* total (1- (car factors))
                  (expt (car factors) (1- (cadr factors))))))
               ((null factors) total)))))
    (t (list '($totient) n))))


;;; JACOBI symbol and Gaussian factoring

(declare-top (special modulus $intfaclim))

(defvar *incl* (let ((l (list 2 4))) (nconc l l)))

(defun imodp (p)
  (cond 
    ((not (= (rem p 4) 1)) nil)
    ((= (rem p 8) 5) (imodp1 2 p))
    ((= (rem p 24) 17) (imodp1 3 p)) ;p=2(mod 3)
    (t (do ((i 5 (+ i (car j)))      ;p=1(mod 24)
            (j *incl* (cdr j)))
           ((= (jacobi i p) -1) (imodp1 i p))))))

(defun imodp1 (i modulus)
  (abs (cexpt i (ash (1- modulus) -2) )))

(defun psumsq (p)
  (let ((x (imodp p)))
    (cond 
      ((equal p 2) (list 1 1))
      ((null x) nil)
      (t (psumsq1 p x)))))

(defun psumsq1 (p x)
  (do ((sp ($isqrt p))
       (r1 p r2)
       (r2 x (rem r1 r2)))
      ((not (> r1 sp)) (list r1 r2))))

(defun gctimes (a b c d)
  (list (- (* a c) (* b d))
        (+ (* a d) (* b c))))

(defmfun $gcfactor (n)
  (let ((n (cdr ($totaldisrep ($bothcoef ($rat n '$%i) '$%i)))))
    (if (not (and (integerp (car n)) (integerp (cadr n))))
      (gcdisp (nreverse n))
      (do ((factors (gcfactor (cadr n) (car n)) (cddr factors))
           (res nil))
          ((null factors)
            (cond 
              ((null res) 1)
              ((null (cdr res)) (car res))
              (t (cons '(mtimes simp) (nreverse res)))))
        (let ((term (car factors))
              (exp (cadr factors)))
          (push (if (= exp 1)
                  (gcdisp term)
                  (pow (gcdisp term) exp))
            res))))))

(defun gcdisp (term)
  (cond 
    ((atom term) term)
    ((let ((rp (car term))
           (ip (cadr term)))
      (setq ip (if (equal ip 1) '$%i (list '(mtimes) ip '$%i)))
      (if (equal rp 0)
         ip
         (list '(mplus) rp ip))))))

(defun gcfactor (a b &aux tem)
  (prog (gl cd dc econt p e1 e2 ans plis nl $intfaclim )
    (setq e1 0
          e2 0
          econt 0
          gl (gcd a b)
          a (quotient a gl)
          b (quotient b gl)
          nl (cfactorw (+ (* a a) (* b b)))
          gl (cfactorw gl))
    (and (equal 1 (car gl)) (setq gl nil))
    (and (equal 1 (car nl)) (setq nl nil))
    loop
    (cond ((null gl)
            (cond ((null nl) (go ret))
                  ((setq p (car nl)))))
          ((null nl) (setq p (car gl)))
          (t (setq p (max (car gl) (car nl)))))
    (setq cd (psumsq p))
    (cond ((null cd)
            (setq plis (cons p (cons (cadr gl) plis)))
            (setq gl (cddr gl)) 
            (go loop))
          ((equal p (car nl))
            (cond ((zerop (rem (setq tem (+ (* a (car cd)) ;gcremainder
                                            (* b (cadr cd))))
                               p))     ;remainder(real((a+bi)cd~),p)
                                       ;z~ is complex conjugate
                    (setq e1 (cadr nl)) (setq dc cd))
                  (t (setq e2 (cadr nl))
                     (setq dc (reverse cd))))
            (setq dc (gcexpt dc (cadr nl)) ;
                  dc (gctimes a b (car dc) (- (cadr dc)))
                  a (quotient (car dc) p)
                  b (quotient (cadr dc) p)
                  nl (cddr nl))))
    (cond ((equal p (car gl))
            (setq econt (+ econt (cadr gl)))
            (cond ((equal p 2)
                    (setq e1 (+ e1 (* 2 (cadr gl)))))
                  (t (setq e1 (+ e1 (cadr gl))
                           e2 (+ e2 (cadr gl)))))
            (setq gl (cddr gl))))
    (and (not (zerop e1))
         (setq ans (cons cd (cons e1 ans)))
         (setq e1 0))
    (and (not (zerop e2))
         (setq ans (cons (reverse cd) (cons e2 ans)))
         (setq e2 0))
    (go loop)
    ret    
    (setq cd (gcexpt (list 0 -1)
                     (rem econt 4)))
    (setq a (gctimes a b (car cd) (cadr cd)))
    ;;a hasn't been divided by p yet..
    (setq a (mapcar 'signum a))
    #+cl (assert (or (zerop (car a))(zerop (second a))))
    (cond ((or (equal (car a) -1) (equal (cadr a) -1))
            (setq plis (cons -1 (cons 1 plis)))))
    (cond ((equal (car a) 0)
            (setq ans (cons '(0 1) (cons 1 ans)))))
    (setq ans (nconc plis ans))
    (return ans)))

(defun multiply-gcfactors (lis)
  (loop for (term exp) on (cddr lis) by #'cddr
    with answ = (cond ((numberp (car lis))(list (pexpt (car lis) (second lis)) 0))
                      (t(gcexpt (car lis) (second lis))))
    when (numberp term)
      do (setq answ (list (* (first answ) term) (* (second answ) term)))
      (show answ)
    else
      do (setq answ (apply 'gctimes (append answ (gcexpt term exp))))
    finally (return answ)))

(defun gcexpt (a n)
  (cond ((zerop n) '(1 0))
        ((equal n 1) a)
        ((evenp n) (gcexpt (gctime1 a a) (truncate n 2)))
        (t (gctime1 a (gcexpt (gctime1 a a) (truncate n 2))))))

(defun gctime1 (a b)
  (gctimes (car a) (cadr a) (car b) (cadr b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maxima functions in (Z/nZ)*
;; 
;; zn_order, zn_primroot_p, zn_primroot, zn_log, 
;; chinese,
;; zn_add_table, zn_mult_table, zn_power_table 
;;
;; 2012 - 2013, Volker van Nek  
;;

;; Maxima option variables:
(defmvar $zn_primroot_limit 1000 "Upper bound for `zn_primroot'." fixnum)
(defmvar $zn_primroot_verbose nil "Print message when `zn_primroot_limit' is reached." boolean)
(defmvar $zn_primroot_pretest nil "`zn_primroot' pretests whether (Z/nZ)* is cyclic." boolean)


;; compute the order of x in (Z/nZ)*
;;
;; optional argument: ifactors of totient(n) as returned in Maxima by 
;;    block([factors_only:false], ifactors(totient(n)))
;;    e.g. [[2, 3], [3, 1], ... ]
;;
(defmfun $zn_order (x n &optional fs-phi) 
  (unless (and (integerp x) (integerp n))
    (return-from $zn_order 
      (if fs-phi 
        (list '($zn_order) x n fs-phi)
        (list '($zn_order) x n) )))
  (when (minusp x) (setq x (mod x n)))
  (cond 
    ((= 0 x) nil)
    ((= 1 x) (if (= n 1) nil 1))
    ((/= 1 (gcd x n)) nil)
    (t 
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (gf-merror (intl:gettext 
             "Third argument to `zn_order' must be of the form [[p1, e1], ..., [pk, ek]]." )))
         (setq fs-phi (totient-with-factors n)) )
      (zn_order x 
                n
                (car fs-phi) ;; phi
                (cdr fs-phi)) ))) ;; factors of phi with multiplicity
;;
(defun zn_order (x n phi fs-phi)
  (let ((s phi) p e)
    (dolist (f fs-phi s)
      (setq p (car f) e (cadr f))
      (setq s (truncate s (expt p e)))
      (do ((z (power-mod x s n)))
          ((= z 1))
        (setq z (power-mod z p n))
        (setq s (* s p)) )) ))


;; compute totient (euler-phi) of n and its factors in one function
;;
;; returns a list of the form (phi ((p1 e1) ... (pk ek)))
;;
(defun totient-with-factors (n)
  (let (($intfaclim) (phi 1) fs-n (fs) p e (fs-phi) g)
    (setq fs-n (get-factor-list n))
    (dolist (f fs-n fs)
      (setq p (car f) e (cadr f))
      (setq phi (* phi (1- p) (expt p (1- e))))
      (when (> e 1) (setq fs (cons `(,p ,(1- e)) fs)))
      (setq fs (append (get-factor-list (1- p)) fs)) )
    (setq fs (copy-tree fs)) ;; this deep copy is a workaround to avoid references 
                             ;; to the list returned by ifactor.lisp/get-factor-list.
                             ;; see bug 3510983
    (setq fs (sort fs #'(lambda (a b) (< (car a) (car b)))))
    (setq g (car fs))
    (dolist (f (cdr fs) (cons phi (reverse (cons g fs-phi))))
      (if (= (car f) (car g)) 
        (incf (cadr g) (cadr f)) ;; assignment
        (progn 
          (setq fs-phi (cons g fs-phi))
          (setq g f) ))) ))

;; recompute totient from given factors
;;
;; fs-phi: factors of totient with multiplicity: ((p1 e1) ... (pk ek))
;;
(defun totient-from-factors (fs-phi) 
  (let ((phi 1) p e)
    (dolist (f fs-phi phi)
      (setq p (car f) e (cadr f))
      (setq phi (* phi (expt p e))) )))


;; for n > 2 is x a primitive root modulo n 
;;   when n does not divide x
;;   and for all prime factors p of phi = totient(n)
;;   x^(phi/p) mod n # 1
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_primroot_p (x n &optional fs-phi)
  (unless (and (integerp x) (integerp n))
    (return-from $zn_primroot_p 
      (if fs-phi 
        (list '($zn_primroot_p) x n fs-phi)
        (list '($zn_primroot_p) x n) )))
  (when (minusp x) (setq x (mod x n)))
  (cond 
    ((= 0 x) nil)
    ((= 1 x) (if (= n 2) t nil))
    ((<= n 2) nil)
    ((= 0 (mod x n)) nil)
    (t 
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (gf-merror (intl:gettext 
             "Third argument to `zn_primroot_p' must be of the form [[p1, e1], ..., [pk, ek]]." )))
         (setq fs-phi (totient-with-factors n)) )
      (zn-primroot-p x n
                     (car fs-phi) ;; phi
                     (mapcar #'car (cdr fs-phi))) ))) ;; factors only (omitting multiplicity)
;;
(defun zn-primroot-p (x n phi fs-phi)
  (unless (= 1 (gcd x n))
    (return-from zn-primroot-p nil) )  
  (dolist (p fs-phi t)
    (when (= 1 (power-mod x (truncate phi p) n))
      (return-from zn-primroot-p nil) )))

;;
;; find the smallest primitive root modulo n
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_primroot (n &optional fs-phi)
  (unless (integerp n)
    (return-from $zn_primroot 
      (if fs-phi 
        (list '($zn_primroot) n fs-phi)
        (list '($zn_primroot) n) )))
  (cond 
    ((<= n 1) nil)
    ((= n 2) 1)
    (t 
      (when $zn_primroot_pretest
        (unless (cyclic-p n)
          (return-from $zn_primroot nil) ))
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (gf-merror (intl:gettext
             "Second argument to `zn_primroot' must be of the form [[p1, e1], ..., [pk, ek]]." )))
         (setq fs-phi (totient-with-factors n)) )
      (zn-primroot n 
                   (car fs-phi) ;; phi
                   (mapcar #'car (cdr fs-phi))) ))) ;; factors only (omitting multiplicity)
;;
;; (Z/nZ)* is cyclic if n = 2, 4, p^k or 2*p^k where p prime > 2
(defun cyclic-p (n) 
  (cond
    ((< n 2) nil)
    ((< n 8) t)
    (t 
      (when (evenp n) 
        (setq n (ash n -1))
        (when (evenp n) (return-from cyclic-p nil)) )
      (let (($intfaclim) (fs (get-small-factors n)) (len 0) p q)
        (setq n (car fs))
        (when (cadr fs) (setq len (length (cadr fs))))
        (if (= 1 n) 
          (return-from cyclic-p (= 1 len))
          (when (> len 0) (return-from cyclic-p nil)) )
        (when (primep n) (return-from cyclic-p t))
        (setq q (setq p (get-one-factor n)))
        (do () (())
          (setq n (truncate n q))
          (when (primep n) (return (= n p)))
          (setq q (get-one-factor n))
          (when (/= p q) (return nil)) )))))
;;
(defun zn-primroot (n phi fs-phi) 
  (do ((i 2 (1+ i)))
       ((= i n) nil)
    (when (zn-primroot-p i n phi fs-phi)
      (return i) )
    (when (= i $zn_primroot_limit)
      (when $zn_primroot_verbose
        (format t "`zn_primroot' stopped at zn_primroot_limit = ~A~%" $zn_primroot_limit) )
      (return nil) )))

;;
;; Chinese Remainder Theorem
;;
(defmfun $chinese (rems mods) 
  (cond 
    ((not (and ($listp rems) ($listp mods)))
      (list '($chinese) rems mods) )
    ((or (= 0 ($length rems)) (= 0 ($length mods)))
      (gf-merror (intl:gettext
        "At least one argument to `chinese' was an empty list." )))
    ((notevery #'integerp (setq rems (cdr rems)))
      (list '($chinese) (cons '(mlist simp) rems) mods) )
    ((notevery #'integerp (setq mods (cdr mods)))
      (list '($chinese) (cons '(mlist simp) rems) (cons '(mlist simp) mods)) )
    (t
      (car (chinese rems mods)) )))
;;
(defun chinese (rems mods)
  (if (onep (length mods)) 
    (list (car rems) (car mods))
    (let* ((rp (car rems))
           (p  (car mods))
           (rq-q (chinese (cdr rems) (cdr mods)))
           (rq (car rq-q))
           (q (cadr rq-q))
           (q-inv (inv-mod q p))
           (h (mod (* (- rp rq) q-inv) p))
           (x (+ (* h q) rq)) )
      (list x (* p q)) )))

;;
;; discrete logarithm:
;; solve g^x = a mod n, where g is a generator of (Z/nZ)* 
;;
;; see: lecture notes 'Grundbegriffe der Kryptographie' - Eike Best
;; http://theoretica.informatik.uni-oldenburg.de/~best/publications/kry-Mai2005.pdf
;;
;; optional argument: ifactors of totient(n)
;;
(defmfun $zn_log (a g n &optional fs-phi)
  (unless (and (integerp a) (integerp g) (integerp n))
    (return-from $zn_log 
      (if fs-phi 
        (list '($zn_log) a g n fs-phi)
        (list '($zn_log) a g n) )))
  (when (minusp a) (setq a (mod a n)))
  (cond 
    ((or (= 0 a) (>= a n)) nil)
    ((= 1 a) 0)
    ((= g a) 1)
    ((> (gcd a n) 1) nil)
    (t 
      (if fs-phi
        (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
          (progn 
            (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
            (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
          (gf-merror (intl:gettext
             "Fourth argument to `zn_log' must be of the form [[p1, e1], ..., [pk, ek]]." )))
        (setq fs-phi (totient-with-factors n)) )
      (cond
        ((not (zn-primroot-p g n 
                             (car fs-phi)                   ;; phi
                             (mapcar #'car (cdr fs-phi)) )) ;; factors without multiplicity
          (gf-merror (intl:gettext 
            "Second argument to `zn_log' must be a generator of (Z/~MZ)*." ) n ))
        ((= 0 (mod (- a (* g g)) n)) 
          2 )
        ((= 1 (mod (* a g) n))
          (mod -1 (car fs-phi)) )
        (t 
          (zn-dlog a g n 
                   (car fs-phi)         ;; phi
                   (cdr fs-phi) ) ))))) ;; factors with multiplicity

;; Pohlig and Hellman reduction:

(defun zn-dlog (a g n ord fs-ord) ;; g is generator of order ord mod n
  (let (p e ord/p gp x dlog (dlogs nil))
    (dolist (f fs-ord)
      (setq p (car f) e (cadr f))
      (setq ord/p (truncate ord p))
      (setq gp (power-mod g ord/p n)) ;; gp is generator of prime order p mod n
      (cond 
        ((= 1 e) 
          (setq x (dlog-rho (power-mod a ord/p n) gp p n)) )
        (t 
          (setq x 0)
          (do ((aa a) (k 1) (pk 1)) (())
            (setq dlog (dlog-rho (power-mod aa (truncate ord/p pk) n) gp p n))
            (setq x (+ x (* dlog pk)))
            (if (= k e) 
              (return)
              (setq k (1+ k) pk (* pk p)) )
            (setq aa (mod (* a ($power_mod g (- x) n)) n)) )))
      (setq dlogs (cons x dlogs)) )
    (car (chinese (nreverse dlogs) (mapcar #'(lambda (z) (apply #'expt z)) fs-ord))) ))

;; baby-steps-giant-steps:

(defun dlog-baby-giant (a g p n) ;; g is generator of order p mod n
  (let* ((m (1+ (isqrt p)))
         (s (floor (* 1.3 m)))
         (gi (inv-mod g n)) d babies )
    (setf babies 
      (make-hash-table :size s :test #'eql :rehash-threshold 0.9) )
    (do ((r 0 (1+ r)) b)
        ((= r m))
      (setq b (mod (* a (power-mod gi r n)) n))
      (when (= 1 b)
        (clrhash babies)
        (return-from dlog-baby-giant r) )
      (setf (gethash b babies) r) )
    (setq d (power-mod g m n))
    (do ((rr 0 (1+ rr)) bb r) (())
      (setq bb (power-mod d rr n))
      (when (setq r (gethash bb babies))
        (clrhash babies)
        (return-from dlog-baby-giant (mod (+ (* rr m) r) n)) )) ))

;; brute-force:

(defun dlog-naive (a g n)
  (do ((i 0 (1+ i)) (gi 1 (mod (* gi g) n)))
      ((= gi a) i) ))

;; Pollard rho for dlog computation (Brents variant of collision detection)

(defun dlog-rho (a g p n) ;; g is generator of prime order p mod n
  (cond
    ((= 1 a) 0)
    ((= g a) 1)
    ((= 0 (mod (- a (* g g)) n)) 2)
    ((= 1 (mod (* a g) n)) (1- p))
    ((< p 512) (dlog-naive a g n))
    ((< p 65536) (dlog-baby-giant a g p n))
    (t
      (prog ((b 1) (y 0) (z 0)    ;; b = g^y * a^z
             (bb 1) (yy 0) (zz 0) ;; bb = g^yy * a^zz
             dy dz )
        rho
        (do ((i 0)(j 1)) (()) (declare (fixnum i j))
          (multiple-value-setq (b y z) (dlog-f b y z a g p n))
          (when (equal b bb) (return))                 ;; g^y * a^z = g^yy * a^zz
          (incf i)
          (when (= i j)
            (setq j (1+ (ash j 1)))
            (setq bb b yy y zz z) ))
        (setq dy (mod (- y yy) p) dz (mod (- zz z) p)) ;; g^dy = a^dz = g^(x*dz)
        (when (= 1 (gcd dz p))
          (return (mod (* dy (inv-mod dz p)) p)) )     ;; x = dy/dz mod p (since g is generator of order p)
        (setq y 0
              z 0
              b 1
              yy (1+ (random (1- p)))
              zz (1+ (random (1- p)))
              bb (mod (* (power-mod g yy n) (power-mod a zz n)) n) )
        (go rho) ))))

;; iteration for Pollard rho:  b = g^y * a^z in each step

(defun dlog-f (b y z a g ord n)
  (let ((m (mod b 3)))
    (cond 
      ((= 0 m)
        (values (mod (* b b) n) (mod (ash y 1) ord) (mod (ash z 1) ord)) ) 
      ((= 1 m) ;; avoid stationary case b=1 => b*b=1  
        (values (mod (* a b) n) y                   (mod (+ z 1) ord)  ) )
      (t
        (values (mod (* g b) n) (mod (+ y 1) ord)   z                ) ) )))


;; for educational puposes: tables of small residue class rings

(defun zn-table-errchk (n fun)
  (unless (and (fixnump n) (< 1 n))
    (gf-merror (intl:gettext 
      "Argument to `~m' must be a small fixnum greater than 1." ) fun )))

(defmfun $zn_add_table (n)
  (zn-table-errchk n "zn_add_table")
  (do ((i 0 (1+ i)) res)
      ((= i n) 
        (cons '($matrix simp) (nreverse res)) )
    (push (mfuncall '$makelist `(mod (+ ,i $j) ,n) '$j 0 (1- n)) res) ))

(defmfun $zn_mult_table (n &optional all?)
  (zn-table-errchk n "zn_mult_table")
  (cond
    ((or (primep n) ;; field
         (equal all? '$all) )
      (do ((i 1 (1+ i)) res)
          ((= i n) (cons '($matrix simp) (nreverse res)))
        (push 
           (mfuncall '$makelist `(mod (* ,i $j) ,n) '$j 1 (1- n))
           res )))
    (t ;; units only
      (let (units res)
        (do ((i 1 (1+ i)))
            ((= i n) 
              (setq units (cons '(mlist simp) (nreverse units))) ) 
          (when (= 1 (gcd i n)) (push i units)) )
        (dolist (i (cdr units) (cons '($matrix simp) (nreverse res)))
          (push 
            (mfuncall '$makelist `(mod (* ,i $j) ,n) '$j units)
            res ))))))

(defmfun $zn_power_table (n &optional all?)
  (zn-table-errchk n "zn_power_table")
  (let ((tn ($totient n)))
    (when (equal all? '$all) (incf tn))
    (do ((i 1 (1+ i)) res)
        ((= i n) 
          (cons '($matrix simp) (nreverse res)) )
      (when (or (equal all? '$all) (= 1 (gcd i n))) 
        (push (mfuncall '$makelist `(power-mod ,i $j ,n) '$j 1 tn) res) ))))


;; $zn_invert_by_lu (m p) 
;; $zn_determinant (m p) 
;; see below: --> galois fields--> interfaces to linearalgebra/lu.lisp

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ----------------------------------------------------------------------------- 
;; *** GALOIS FIELDS *** 
                                                                        
;; The following is a revision and improvement of the first part of share/
;; contrib/gf/gf.mac by Alasdair McAndrew, Fabrizio Caruso and Jacopo D'Aurizio 
;; released under terms of the GPLv2 in 2007.          

;; I would like to thank the original authors for their contribution to Maxima 
;; which allowed me to study, improve and extend the source code.

;; I would also like to thank Camm Maguire who helped me coding compiler macros
;; for GCL.                                                      

;; 2012 - 2013, Volker van Nek

(declare-top (special *gf-char* *gf-exp* *ef-arith?*)) ;; modulus $intfaclim see above

(defvar *gf-rat-header* nil "header of internal CRE representation") 

(defvar *ef-arith?* nil "Should extension field arithmetic be used?")

;; base field:
(defvar *gf-char* 0 "characteristic p") 
(defvar *gf-exp* 0 "exponent n, degree of the reduction polynomial") 
(defvar *gf-ord* 0 "group order, number of units") 
(defvar *gf-card* 0 "cardinality, ring order") 
(defvar *gf-red* nil "reduction polynomial") 
(defvar *gf-prim* nil "primitive element")
(defvar *gf-fs-ord* nil "ifactors of *gf-ord*") 
(defvar *gf-fsx* nil "extended factors of *gf-ord*") 
(defvar *gf-fsx-base-p* nil "*gf-fsx* in  base p") 
(defvar *gf-x^p-powers* nil "x^p^i, i=0,..,n-1") 

(declaim (fixnum *gf-exp* *ef-exp*))

;; extension:
(defvar *ef-exp* 0 "exponent m, degree of the reduction polynomial") 
(defvar *ef-ord* 0 "group order, number of units") 
(defvar *ef-card* 0 "cardinality, ring order") 
(defvar *ef-red* nil "reduction polynomial") 
(defvar *ef-prim* nil "primitive element")
(defvar *ef-fs-ord* nil "ifactors of *ef-ord*") 
(defvar *ef-fsx* nil "extended factors of *ef-ord*") 
(defvar *ef-fsx-base-q* nil "*ef-fsx* in  base q = p^n") 
(defvar *ef-x^q-powers* nil "x^q^i, i=0,..,m-1")

(defvar *gf-char?* nil "Was the characteristic defined?")
(defvar *gf-red?* nil "Was the reduction polynomial defined?")
(defvar *gf-irred?* nil "Is the reduction polynomial irreducible?")
(defvar *gf-data?* nil "gf_set_data called?")

(defvar *ef-red?* nil "Was the reduction polynomial defined?")
(defvar *ef-irred?* nil "Is the reduction polynomial irreducible?")
(defvar *ef-data?* nil "ef_set_data called?")

(defmvar $gf_rat nil "Return values are rational expressions?" boolean)

(defmvar $gf_symmetric nil "A symmetric modulus should be used?" boolean)

(defmvar $gf_coeff_limit 256 
  "`gf_coeff_limit' limits the coeffs when searching for irreducible and primitive polynomials." fixnum)

(putprop '$gf_coeff_limit 'gf-coeff-check 'assign)

(defun gf-coeff-check (var arg) 
  (declare (ignore var))
  (unless (and (integerp arg) (> arg 1))
    (gf-merror (intl:gettext 
      "`gf_coeff_limit': Assignment ignored. Value must be an integer greater than 1.~%" ))))

(defmvar $gf_cantor_zassenhaus t "Should the Cantor-Zassenhaus algorithm be used?" boolean)

(defmvar $ef_coeff_mult nil)
(defmvar $ef_coeff_add nil)
(defmvar $ef_coeff_inv nil) 
(defmvar $ef_coeff_exp nil)

(defmvar $gf_powers nil) 
(defmvar $gf_logs nil) 
(defmvar $gf_sums nil)
(defvar *gf-logs?* nil "Were the power and log tables calculated?")
(defvar *gf-sums?* nil "Was the sum table calculated?")


;; contains parts of merror.lisp/merror but avoids "To debug this ...".

(defun gf-merror (sstring &rest l)
  (setq $error `((mlist) ,sstring ,@ l))
  (and $errormsg ($errormsg))
  (fresh-line *standard-output*)
  (format t (intl:gettext "~& -- an error.~%"))
  (throw 'macsyma-quit 'maxima-error) )


(defun gf-char? (fun)
  (if *gf-char?* t
    (gf-merror (intl:gettext "`~m': The characteristic is not defined yet.") fun) ))

(defun gf-red? (fun)
  (if *gf-red?* t
    (gf-merror (intl:gettext "`~m': The reduction polynomial is not defined yet.") fun) ))

(defun gf-data? (fun)
  (if *gf-data?* t
    (gf-merror (intl:gettext "`~m': gf_set_data called?") fun) ))  

(defun gf-field? (fun)
  (if (and (gf-data? fun) *gf-irred?*) t
    (gf-merror (intl:gettext "`~m': The reduction polynomial is not irreducible.") fun) ))


(defun ef-gf-field? (fun)
  (if (and *gf-data?* *gf-irred?*) t
    (gf-merror (intl:gettext "`~m': The base field is not defined yet.") fun) ))

(defun ef-red? (fun)
  (if (and (ef-gf-field? fun) *ef-red?*) t
    (gf-merror (intl:gettext "`~m': The reduction polynomial is not defined yet.") fun) ))

(defun ef-data? (fun)
  (if (and (ef-gf-field? fun) *ef-data?*) t
    (gf-merror (intl:gettext "`~m': ef_set_data called?") fun) ))  

(defun ef-field? (fun)
  (if (and (ef-data? fun) *ef-irred?*) t
    (gf-merror (intl:gettext "`~m': The extension is no field.") fun) ))
;;
;; ----------------------------------------------------------------------------- 


;; basic coefficient arithmetic ------------------------------------------------
;;

;; optimize the fixnum cases

(defmacro maybe-char-is-fixnum-let (binds &body body)
  `(if (or (and (not *ef-arith?*) (typep *gf-char* 'fixnum))
           (and *ef-arith?* (typep *gf-card* 'fixnum)) )
    (let ,binds
         (declare (fixnum ,@(mapcar #'(lambda (x) (car x)) binds)))
         ,@body)
    (let ,binds 
         (declare (integer ,@(mapcar #'(lambda (x) (car x)) binds)))
         ,@body )))

;; basic coefficient functions and compiler macros

;; gf coefficient arith :

;; *ef-arith?* controls coefficient arithmetic. If *ef-arith?* is false, 
;; coeffs are elements of Zp, where p is the defined characteristic *gf-char*.
;; If *ef-arith?* is true, coeffs are interpreted as the integer representation 
;; of a polynomial over Zp[x] reduced by the irreducible polynomial *gf-red*. 

(defun gf-cinv (c)
  (if *ef-arith?*
    (ef-cinv c)
    (maybe-char-is-fixnum-let ((c c))
      (cond
        ((= 0 c) (gf-merror (intl:gettext "gf coefficient inversion: Quotient by zero")))
        (t (inv-mod c *gf-char*)) )))) ; *gf-char* is prime

(defun gf-cpow (c n)
  (if *ef-arith?*
    (ef-cpow c n)
    (maybe-char-is-fixnum-let ((c c))
      (power-mod c n *gf-char*) )))

(defun gf-cmod (c) 
  (if *ef-arith?*
    (ef-cmod c)
    (maybe-char-is-fixnum-let ((c c))
      (mod c *gf-char*) )))

(defun gf-ctimes (a b)
  (if *ef-arith?*
    (ef-ctimes a b)
    (maybe-char-is-fixnum-let ((a a)(b b))
      (mod (* a b) *gf-char*) )))

(defun gf-cplus-b (a b) ;; assumes that both 0 <= a,b < *gf-char* 
  (if *ef-arith?*
    (ef-cplus-b a b)
    (maybe-char-is-fixnum-let ((a a)(b b)) 
      (let ((s (+ a b)))
        (if (< (the integer s) *gf-char*) 
          s 
          (- (the integer s) *gf-char*) )))))

(defun gf-cminus-b (c) ;; assumes that 0 <= c < *gf-char* 
  (if *ef-arith?*
    (ef-cminus-b c)
    (maybe-char-is-fixnum-let ((c c))
      (- *gf-char* c) )))

;; ef coefficient arith :

(defun ef-cinv (c)
  (declare (integer c))
  (cond 
    ((= 0 c) (gf-merror (intl:gettext "ef coefficient inversion: Quotient by zero")))
    (*gf-logs?* (ef-cinv-by-table c))
    ($ef_coeff_inv (mfuncall '$ef_coeff_inv c))
    (t (let ((*ef-arith?*))
         (gf-x2n (gf-inv (gf-n2x c) *gf-red*)) ))))

(defun ef-cpow (c n)
  (cond 
    (*gf-logs?* (ef-cpow-by-table c n))
    ($ef_coeff_exp (mfuncall '$ef_coeff_exp c n))
    (t (let ((*ef-arith?*)) 
         (gf-x2n (gf-pow (gf-n2x c) n *gf-red*)) ))))

(defun ef-cmod (c) 
  (declare (integer c))
  (cond 
    ((plusp c)
      (cond 
        ((< c *gf-ord*) c) 
        (t (let ((*ef-arith?*))
             (gf-x2n (gf-nred (gf-n2x c) *gf-red*)) ))))
    (t 
      (setq c (ef-cmod (abs c)))
      (let ((*ef-arith?* t)) (gf-ctimes (1- *gf-char*) c)) ))) 

(defun ef-ctimes (a b) 
  (cond 
    (*gf-logs?* (ef-ctimes-by-table a b))
    ($ef_coeff_mult (mfuncall '$ef_coeff_mult a b))
    (t (let ((*ef-arith?*)) 
         (gf-x2n (gf-times (gf-n2x a) (gf-n2x b) *gf-red*)) ))))

(defun ef-cplus-b (a b)
  (cond 
    (*gf-sums?* (ef-cplus-by-table a b))
    ($ef_coeff_add (mfuncall '$ef_coeff_add a b))
    (t (let ((*ef-arith?*)) 
         (gf-x2n (gf-nplus (gf-n2x a) (gf-n2x b))) ))))
 
(defun ef-cminus-b (a)
  (cond 
    (*gf-logs?* (ef-ctimes-by-table (1- *gf-char*) a))
    ($ef_coeff_mult (mfuncall '$ef_coeff_mult (1- *gf-char*) a))
    (t (let ((*ef-arith?*))
         (gf-x2n (gf-nminus (gf-n2x a))) ))))

;; ef coefficient arith by lookup:

(defun ef-ctimes-by-table (c d)
  (declare (integer c d))
  (cond
    ((or (= 0 c) (= 0 d)) 0)
    (t (svref $gf_powers 
         (mod (+ (the integer (svref $gf_logs c)) 
                 (the integer (svref $gf_logs d)) ) 
              *gf-ord* ))) ))

(defun ef-cinv-by-table (c)
  (declare (integer c))
  (cond
    ((= 0 c) (gf-merror (intl:gettext "ef coefficient inversion: Quotient by zero")))
    (t (svref $gf_powers (- *gf-ord* (the integer (svref $gf_logs c))))) ))

(defun ef-cplus-by-table (c d)
  (aref $gf_sums c d) )

(defun ef-cpow-by-table (c n)
  (declare (integer c n))
  (cond
    ((= 0 n) 1)
    ((= 0 c) 0)
    (t (svref $gf_powers 
         (mod (* n (the integer (svref $gf_logs c))) *gf-ord*) )) ))


#-gcl (eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-compiler-macro gf-cmod (a)
      `(cond 
        (*ef-arith?*
          (ef-cmod ,a) )
        ((and (typep *gf-char* 'fixnum) (typep ,a 'fixnum)) ;; maybe a > *gf-char* 
          (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
            (the fixnum (mod x z)) ))
        (t
          (mod (the integer ,a) *gf-char*) )))

    (define-compiler-macro gf-ctimes (a b) 
      `(cond 
        (*ef-arith?*
          (ef-ctimes ,a ,b) )
        ((typep *gf-char* 'fixnum)                                               
          (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
            (the fixnum (mod (* x y) z)) ))                                               
        (t
          (mod (* (the integer ,a) (the integer ,b)) *gf-char*) )))

    (define-compiler-macro gf-cplus-b (a b) ;; assumes that both 0 <= a,b < *gf-char* 
      `(cond 
        (*ef-arith?*
          (ef-cplus-b ,a ,b) )
        ((typep *gf-char* 'fixnum)                                               
          (let ((x ,a) (y ,b) (z *gf-char*) (s 0)) (declare (fixnum x y z) (integer s))                               
            (setq s (the integer (+ x y)))
            (if (< s z) s (- s z)) ))                                               
        (t
          (let ((x (+ (the integer ,a) (the integer ,b)))) (declare (integer x))
            (if (< x *gf-char*) x (- x *gf-char*)) ))))  
 
    (define-compiler-macro gf-cminus-b (a) ;; assumes that 0 <= a < *gf-char* 
      `(cond 
        (*ef-arith?*
          (ef-cminus-b ,a) )
        ((typep *gf-char* 'fixnum)
          (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
            (the fixnum (- z x)) ))
        (t
          (- *gf-char* (the integer ,a)) )))
))

#+gcl (eval-when (compile load eval) 
  (progn
    (push '((fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)(((long long)(#0))%((long long)(#1)))" ) 
          (get 'i% 'compiler::inline-always) )
    (push '((fixnum fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)((((long long)(#0))*((long long)(#1)))%((long long)(#2)))" ) 
          (get '*% 'compiler::inline-always) )
    (push '((fixnum fixnum fixnum) fixnum #.(compiler::flags compiler::rfa compiler::set)
            "@02;({long long _t=((long long)(#0))+((long long)(#1)),_w=((long long)(#2));_t<_w ? (fixnum)_t : (fixnum)(_t - _w);})" )          
          (get '+%b 'compiler::inline-always) )
    (push '((fixnum fixnum) fixnum #.(compiler::flags compiler::rfa)
            "(fixnum)(((long long)(#1))-((long long)(#0)))" ) 
          (get 'neg%b 'compiler::inline-always) )
    
    (setf (get 'i% 'compiler::return-type) t)
    (setf (get '*% 'compiler::return-type) t)
    (setf (get '+%b 'compiler::return-type) t)
    (setf (get 'neg%b 'compiler::return-type) t) 

    (si::define-compiler-macro gf-cmod (a) 
      `(cond 
        (*ef-arith?*
          (ef-cmod ,a) )
        ((and (typep *gf-char* 'fixnum) (typep ,a 'fixnum) (plusp ,a)) ;; maybe a > *gf-char* 
          (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
            (i% x z) ))
        (t
          (mod (the integer ,a) *gf-char*) )))
 
    (si::define-compiler-macro gf-ctimes (a b) ;; assume that 0 <= a,b :
      `(cond 
        (*ef-arith?*
          (ef-ctimes ,a ,b) )
        ((typep *gf-char* 
            ',(if (< (integer-length most-positive-fixnum) 32) `fixnum `(signed-byte 32)) )                                               
          (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
            (*% x y z) ))                                               
        (t
          (mod (* (the integer ,a) (the integer ,b)) *gf-char*) )))

    (si::define-compiler-macro gf-cplus-b (a b) ;; assume that both 0 <= a,b < *gf-char* :
      `(cond 
        (*ef-arith?*
          (ef-cplus-b ,a ,b) )
        ((typep *gf-char* 
            ',(if (< (integer-length most-positive-fixnum) 63) `fixnum `(signed-byte 63)) )                                               
          (let ((x ,a) (y ,b) (z *gf-char*)) (declare (fixnum x y z))                               
            (+%b x y z) ))                                               
        (t
          (let ((x (+ (the integer ,a) (the integer ,b)))) (declare (integer x))
            (if (< x *gf-char*) x (- x *gf-char*)) ))))  
 
    (si::define-compiler-macro gf-cminus-b (a) ;; assume that 0 <= a < *gf-char* :
      `(cond 
        (*ef-arith?*
          (ef-cminus-b ,a) )
        ((typep *gf-char* 'fixnum)
          (let ((x ,a) (z *gf-char*)) (declare (fixnum x z))
            (neg%b x z) ))
        (t
          (- *gf-char* (the integer ,a)) )))
))
;;
;; -----------------------------------------------------------------------------


;; setting the finite field and retrieving basic informations ------------------
;;

(defmfun $gf_set (p &optional a1 a2 a3) ;; deprecated
  (format t "`gf_set' is deprecated. ~%~\
             The user is asked to use `gf_set_data' instead.~%" )
  (when a2
    (format t "In future versions `gf_set_data' will only accept two arguments.~%") )
  ($gf_set_data p a1 a2 a3) )


(defmfun $gf_set_data (p &optional a1 a2 a3) ;; opt: *gf-exp*, *gf-red*, *gf-fs-ord*
  (declare (ignore a2 a3)) ;; remove a2 a3 in next versions
  (let ((*ef-arith?*))
    (unless (and (integerp p) (primep p))
      (gf-merror (intl:gettext "`gf_set_data': Field characteristic must be a prime number.")) )
    ($gf_unset)
    (setq *gf-char* p)
    
    (when a1 ;; exponent or reduction poly
      (cond 
        ((integerp a1) 
          (unless (and (fixnump a1) (plusp a1))
            (gf-merror (intl:gettext "`gf_set_data': The exponent must be a positive fixnum.")) )
          (setq *gf-exp* a1) )
        (t
          (setq *gf-red* (gf-p2x-red a1 "gf_set_data") 
                *gf-exp* (car *gf-red*)
                *gf-irred?* (gf-irr-p *gf-red* *gf-char* *gf-exp*) )) ))
    
    (gf-set-rat-header) ;; CRE-headers

    (unless *gf-red* ;; find irreducible reduction poly:
      (setq *gf-red* (if (= 1 *gf-exp*) (list 1 1) (gf-irr p *gf-exp*))
            *gf-irred?* t ))
    
    (setq *gf-card* (expt p *gf-exp*)) ;; cardinality #(F)

    (setq *gf-ord* ;; group order #(F*)
      (cond 
        ((= 1 *gf-exp*) (1- p))
        ((not *gf-irred?*) (gf-group-order *gf-char* *gf-red*))
        (t (1- (expt p *gf-exp*))) ))
    (let* (($intfaclim)
           (fs (get-factor-list *gf-ord*)) ) 
      (setq *gf-fs-ord* (sort fs #'(lambda (a b) (< (car a) (car b))))) )   ;; .. [pi, ei] .. 

    (setq *gf-prim* ;; primitive element
      (cond 
        ((= 1 *gf-exp*)
          (if (= 2 *gf-char*) (list 0 1)
            (list 0 (zn-primroot p *gf-ord* (mapcar #'car *gf-fs-ord*))) )) ;; .. pi ..  (factors_only:true)
        (t
          (gf-precomp)
          (if *gf-irred?*
            (gf-prim)
            '$unknown ))))

    (setq *gf-char?* t *gf-red?* t *gf-data?* t) ;; global flags
    ($gf_get_data) )) ;; data structure


(defun gf-set-rat-header ()
  (let ((modulus))
    (setq *gf-rat-header* (car ($rat '$x))) ))

(defun gf-p2x-red (p fun)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let* ((modulus) (x (car (prep1 p))))
    (unless (and (listp x) 
                 (every #'numberp (setq x (cdr x))) )
      (gf-merror (intl:gettext "`~m': Not suitable as reduction polynomial: ~m") fun p) )
    (setq x (gf-mod x))
    (unless (and (typep (car x) 'fixnum) (plusp (car x)))
      (gf-merror (intl:gettext "`~m': The exponent must be a positive fixnum.") fun) )
    (unless (eql 1 (cadr x))
      (gf-merror (intl:gettext "`~m': A monic reduction polynomial is assumed.") fun) )
    x ))


(defmfun $ef_set_data (red) 
  (ef-gf-field? "ef_set_data")
  ($ef_unset)
  (let ((*ef-arith?* t)) 
    (setq *ef-red* (gf-p2x-red red "ef_set_data")
          *ef-exp* (car *ef-red*)
          *ef-card* (expt *gf-card* *ef-exp*)
          *ef-irred?* (gf-irr-p *ef-red* *gf-card* *ef-exp*)
          *ef-ord* (if *ef-irred?*
                     (1- *ef-card*) 
                     (gf-group-order *gf-card* *ef-red*) ))
    (let* (($intfaclim)
           (fs (get-factor-list *ef-ord*)) ) 
      (setq *ef-fs-ord* (sort fs #'(lambda (a b) (< (car a) (car b))))) ) 
    (ef-precomp)
    (setq *ef-data?* t
          *ef-red?* t
          *ef-prim* (if (= 1 *ef-exp*) 
                      (list 0 (let ((*ef-arith?*)) (gf-x2n *gf-prim*))) 
                      (if *ef-irred?* (ef-prim) '$unknown) )))
  ($ef_get_data) ) 


(defstruct (gf-data (:print-function gf-data-short-print))
  char exp red prim card
  ord fs-ord fsx fsx-base-p x^p-powers )

(defun gf-data-short-print (struct stream i) 
  (declare (ignore struct i))
  (format stream "Structure [GF-DATA]") ) ;; wxMaxima returns this
                                          ;; terminal should return this too

;; returns a struct containing all data necessary to use gf_set_again (see below)
(defmfun $gf_get_data () 
  (gf-data? "gf_get_data")
  (make-gf-data
    :char *gf-char*                ; characteristic 
    :exp *gf-exp*                  ; exponent 
    :red *gf-red*                  ; reduction 
    :prim *gf-prim*                ; primitive 
    :card *gf-card*                ; cardinality 
    :ord *gf-ord*                  ; order 
    :fs-ord *gf-fs-ord*            ; factors of order
    :fsx *gf-fsx*                  ; extended factors of order
    :fsx-base-p *gf-fsx-base-p*    ; extended factors in base p
    :x^p-powers *gf-x^p-powers* )) ; pre-calculated powers

(defstruct (ef-data (:print-function ef-data-short-print))
  exp red prim card
  ord fs-ord fsx fsx-base-q x^q-powers )

(defun ef-data-short-print (struct stream i) 
  (declare (ignore struct i))
  (format stream "Structure [EF-DATA]") ) 

(defstruct1 '(($ef_data) 
  $exponent $reduction $primitive $cardinality $order $factors_of_order ))

(defmfun $ef_get_data () 
  (ef-data? "ef_get_data")
  (make-ef-data
    :exp *ef-exp*                  ; exponent 
    :red *ef-red*                  ; reduction 
    :prim *ef-prim*                ; primitive 
    :card *ef-card*                ; cardinality 
    :ord *ef-ord*                  ; order 
    :fs-ord *ef-fs-ord*            ; factors of order
    :fsx *ef-fsx*                  ; extended factors of order
    :fsx-base-q *ef-fsx-base-q*    ; extended factors in base q
    :x^q-powers *ef-x^q-powers* )) ; pre-calculated powers

(defmfun $gf_info (&optional (t? t))
  (gf-data? "gf_info")
  (let ((no-prim (or (null *gf-prim*) (equal *gf-prim* '$unknown)))
        (*ef-arith?*) ) 
    (format t? 
      "characteristic = ~a~:[, ~;~%~]~\
       reduction polynomial = ~a~:[, ~;~%~]~\
       primitive element = ~a~:[, ~;~%~]~\
       nr of elements = ~a~:[, ~;~%~]~\
       nr of units = ~a~:[, ~;~]~\
       ~:[~;~%nr of primitive elements = ~a~] ~%"
      *gf-char* t?
      (mfuncall '$string (gf-x2p *gf-red*)) t?
      (mfuncall '$string 
        (if no-prim
          *gf-prim*
          (gf-x2p *gf-prim*) )) t?
      *gf-card* t?
      *gf-ord* (or t? no-prim) (not no-prim)
      (totient-by-fs-n *gf-fs-ord*) )))

(defun totient-by-fs-n (fs-n)
  (let ((phi 1) p e)
    (dolist (f fs-n phi)
      (setq p (car f) e (cadr f))
      (setq phi (* phi (1- p) (expt p (1- e)))) )))

(defmfun $gf_infolist () ;; enables testing gf_set_data in rtest
  (gf-data? "gf_infolist")
  (let ((*ef-arith?*)) 
    `((mlist simp)
      ,*gf-char*
      ,(gf-x2p *gf-red*)
      ,(if (or (null *gf-prim*) (equal *gf-prim* '$unknown))
        *gf-prim*
        (gf-x2p *gf-prim*) )
      ,*gf-card*
      ,*gf-ord* )))

(defmfun $ef_info (&optional (t? t))
  (ef-data? "ef_info")
  (let ((no-prim (or (null *ef-prim*) (equal *ef-prim* '$unknown)))
        (*ef-arith?* t) )
    (format t? 
      "reduction polynomial = ~a~:[, ~;~%~]~\
       primitive element = ~a~:[, ~;~%~]~\
       nr of elements = ~a~:[, ~;~%~]~\
       nr of units = ~a~:[, ~;~]~\
       ~:[~;~%nr of primitive elements = ~a~] ~%"
      (mfuncall '$string (gf-x2p *ef-red*)) t?
      (mfuncall '$string 
        (if no-prim
          *ef-prim*
          (gf-x2p *ef-prim*) )) t?
      *ef-card* t?
      *ef-ord* (or t? no-prim) (not no-prim)
      (totient-by-fs-n *ef-fs-ord*) )))

(defmfun $ef_infolist () ;; enables testing ef_set_data in rtest
  (ef-data? "ef_infolist")
  (let ((*ef-arith?* t)) 
    `((mlist simp)
      ,(gf-x2p *ef-red*)
      ,(if (or (null *ef-prim*) (equal *ef-prim* '$unknown))
        *ef-prim*
        (gf-x2p *ef-prim*) )
      ,*ef-card*
      ,*ef-ord* )))


(defmfun $gf_unset ()
  (setq $gf_powers nil $gf_logs nil $gf_sums nil
        $gf_rat nil
        $ef_coeff_mult nil $ef_coeff_add nil $ef_coeff_inv nil $ef_coeff_exp nil
        *gf-rat-header* nil *gf-char* 0 
        *gf-exp* 1 *gf-ord* 0 *gf-card* 0 ;; *gf-exp* = 1 when gf_set_data has no optional arg
        *gf-red* nil *gf-prim* nil 
        *gf-fs-ord* nil *gf-fsx* nil *gf-fsx-base-p* nil *gf-x^p-powers* nil 
        *gf-char?* nil *gf-red?* nil *gf-irred?* nil *gf-data?* nil  
        *gf-logs?* nil *gf-sums?* nil ) 
  t )

(defmfun $ef_unset ()
  (setq *ef-exp* 0 *ef-ord* 0 *ef-card* 0 
        *ef-red* nil *ef-prim* nil 
        *ef-fs-ord* nil *ef-fsx* nil *ef-fsx-base-q* nil *ef-x^q-powers* nil
        *ef-red?* nil *ef-irred?* nil *ef-data?* nil ) 
  t )


;; Minimal set
;; Just set characteristic and reduction poly to allow basic arithmetics on the fly.
(defmfun $gf_minimal_set (p &optional (red))
  (unless (and (integerp p) (primep p))
    (gf-merror (intl:gettext "First argument to `gf_minimal_set' must be a prime number.")) )
  ($gf_unset)
  (setq *gf-char* p
        *gf-char?* t )
  (gf-set-rat-header)
  (let ((*ef-arith?*)) 
    (when red 
      (setq *gf-red* (gf-p2x-red red "gf_minimal_set")
            *gf-red?* t
            *gf-exp* (car *gf-red*) ))
    (format nil "characteristic = ~a, reduction polynomial = ~a"
      *gf-char*
      (if red (mfuncall '$string (gf-x2p *gf-red*)) "false") )))


(defmfun $ef_minimal_set (red) 
  (ef-gf-field? "ef_minimal_set")
  ($ef_unset)
  (let ((*ef-arith?* t)) 
    (when red 
      (setq *ef-red* (gf-p2x-red red "ef_minimal_set")
            *ef-exp* (car *ef-red*)
            *ef-red?* t ))
    (format nil "reduction polynomial = ~a" 
      (if red (mfuncall '$string (gf-x2p *ef-red*)) "false") )))


(defmfun $gf_characteristic () 
  (gf-char? "gf_characteristic") 
  *gf-char* )

(defmfun $gf_exponent () 
  (gf-red? "gf_exponent") 
  *gf-exp* )

(defmfun $gf_reduction () 
  (gf-red? "gf_reduction") 
  (when *gf-red* (let ((*ef-arith?*)) (gf-x2p *gf-red*))) )

(defmfun $gf_cardinality () 
  (gf-data? "gf_cardinality") 
  *gf-card* )


(defmfun $ef_exponent () 
  (ef-red? "ef_exponent") 
  *ef-exp* )

(defmfun $ef_reduction () 
  (ef-red? "ef_reduction") 
  (when *ef-red* (let ((*ef-arith?* t)) (gf-x2p *ef-red*))) )

(defmfun $ef_cardinality () 
  (ef-data? "ef_cardinality") 
  *ef-card* )


;; Reuse data and results from a previous gf_set_data
(defmfun $gf_set_again (data) 
  (unless (gf-data-p data)
    (gf-merror (intl:gettext 
      "Argument to `gf_set_again' must be a return value of `gf_set_data'." )))
  ($gf_unset) 
  (gf-set-rat-header)
  (setq *gf-char* (gf-data-char data)
        *gf-exp* (gf-data-exp data)
        *gf-red* (gf-data-red data)
        *gf-prim* (gf-data-prim data)
        *gf-card* (gf-data-card data)
        *gf-ord* (gf-data-ord data)
        *gf-fs-ord* (gf-data-fs-ord data)        
        *gf-fsx* (gf-data-fsx data)
        *gf-fsx-base-p* (gf-data-fsx-base-p data)
        *gf-x^p-powers* (gf-data-x^p-powers data)
        *gf-irred?* (= *gf-ord* (1- *gf-card*))
        *gf-char?* t
        *gf-red?* t
        *gf-data?* t ))
 
(defmfun $ef_set_again (data) 
  (ef-gf-field? "ef_set_again")
  (unless (ef-data-p data)
    (gf-merror (intl:gettext 
      "Argument to `ef_set_again' must be a return value of `ef_set_data'." )))
  ($ef_unset) 
  (setq *ef-exp* (ef-data-exp data)
        *ef-red* (ef-data-red data)
        *ef-prim* (ef-data-prim data)
        *ef-card* (ef-data-card data)
        *ef-ord* (ef-data-ord data)
        *ef-fs-ord* (ef-data-fs-ord data)        
        *ef-fsx* (ef-data-fsx data)
        *ef-fsx-base-q* (ef-data-fsx-base-q data)
        *ef-x^q-powers* (ef-data-x^q-powers data)
        *ef-irred?* (= *ef-ord* (1- *ef-card*))
        *ef-red?* t
        *ef-data?* t ))
;;
;; -----------------------------------------------------------------------------


;; lookup tables ---------------------------------------------------------------
;;

(defmfun $gf_make_arrays () 
  (format t "`gf_make_arrays' is deprecated. ~%~\
             The user is asked to use `gf_make_logs' instead.~%" )
  ($gf_make_logs) )

(defmfun $gf_make_logs () ;; and antilogs
  (gf-field? "gf_make_logs")
  (let ((*ef-arith?*)) (gf-make-logs)) )

(defun gf-make-logs () 
  (unless (typep *gf-ord* 'fixnum)
    (gf-merror (intl:gettext "`gf_make_logs': group order must be a fixnum.")) )
  (let ((x (list 0 1)) (ord *gf-ord*) (primx *gf-prim*) (red *gf-red*)) 
       (declare (fixnum ord))
;;
;; power table of the field, where the i-th element is the numerical
;; equivalent of the field element e^i, where e is a primitive element 
;;
    (setq $gf_powers (make-array (1+ ord) :element-type 'integer))
    (setf (svref $gf_powers 0) 1)
    (do ((i 1 (1+ i)))
        ((> i ord))
        (declare (fixnum i))
      (setq x (gf-times x primx red))
      (setf (svref $gf_powers i) (gf-x2n x)) )
;;
;; log table: the inverse lookup of the power table 
;;
    (setq $gf_logs (make-array (1+ ord) :initial-element nil))
    (do ((i 0 (1+ i)))
        ((= i ord))
        (declare (fixnum i))
      (setf (svref $gf_logs (svref $gf_powers i)) i) )
    (setq *gf-logs?* t)
    `((mlist simp) ,$gf_powers ,$gf_logs) ))

(defmfun $gf_make_sums () 
  (gf-data? "gf_make_sums")
  (let ((*ef-arith?*)) (gf-make-sums)) )

(defun gf-make-sums () 
  (let ((n *gf-card*)) (declare (fixnum n))
    (setq $gf_sums (make-array `(,n ,n) :element-type 'integer))
    (do ((i 0 (1+ i)))
        ((= i n))
        (declare (fixnum i))
      (do ((j 0 (1+ j)))
          ((= j n))
          (declare (fixnum j))
        (setf (aref $gf_sums i j) 
          (gf-x2n (gf-nplus (gf-n2x i) (gf-n2x j))) )))
    (setq *gf-sums?* t)
    $gf_sums ))

(defun gf-clear-tables () 
  (setq $gf_powers nil
        $gf_logs nil
        $gf_sums nil
        *gf-sums?* nil
        *gf-logs?* nil ))
;;
;; -----------------------------------------------------------------------------


;; converting to/from internal representation ----------------------------------
;;
;; user level      <---> internal
;; 0                     nil
;; integer # 0           (0 integer') where integer' = mod(integer, *gf-char*) 
;; x                     (1 1)
;; x^4 + 3*x^2 + 4       (4 1 2 3 0 4) 
;;
;; This representation uses the term part of the internal CRE representation.
;; The coeffcients are exclusively positive: 1, 2, ..., (*gf-char* -1)
;; Header informations are stored in *gf-rat-header*.
;;
;; gf_set_data(5, 4)$
;; :lisp `(,*gf-char* ,*gf-exp*)
;; (5 4)
;; p : x^4 + 3*x^2 - 1$
;; :lisp ($rat $p)
;; ((MRAT SIMP ($X) (X33303)) (X33303 4 1 2 3 0 -1) . 1)
;; :lisp (gf-p2x $p)
;; (4 1 2 3 0 4)
;; :lisp *gf-rat-header*
;; (MRAT SIMP ($X) (X33303))
;;
;; Remark: I compared the timing results of the arithmetic functions using this 
;; data structure to arithmetics using an array implementation and in case of 
;; modulus 2 to an implementation using bit-arithmetics over integers. 
;; It turns out that in all cases the timing advantages of other data structures 
;; were consumed by conversions from/to the top-level.
;; So for sparse polynomials the CRE representation seems to fit best.


(defun gf-p2x (p) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq p (car (let ((modulus)) (prep1 p))))
  (cond 
    ((integerp p) 
      (cond 
        ((= p 0) nil)
        (t (setq p (gf-cmod p))
           (if (= p 0) nil (list 0 p)) ))) 
    (t 
      (setq p (gf-mod (cdr p)))
      (if (typep (car p) 'fixnum) 
        p
        (gf-merror (intl:gettext "Exponents are limited to fixnums.")) ))))


;; version of gf-p2x that doesn't apply mod reduction

(defun gf-p2x-raw (p) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq p (car (let ((modulus)) (prep1 p))))
  (cond 
    ((integerp p) (if (= 0 p) nil (list 0 p))) 
    (t (setq p (cdr p))
       (unless (every #'numberp p)
         (gf-merror (intl:gettext "gf: polynomials must be univariate.")) )
       p )))


(defun gf-x2p (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq x 
    (cond
      ((null x) 0)
      ((= 0 (the fixnum (car x))) (gf-cp2smod (cadr x)))
      (t (gf-np2smod x)) ))
  (if (eql $gf_rat t)
    (gf-x2cre x)
    (gf-disrep x) ))
;;
;; depending on $gf_rat gf-x2p returns a CRE or a ratdisrepped expression
;;
(defun gf-x2cre (x)
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (if (integerp x) 
    `(,*gf-rat-header* ,x . 1)
    `(,*gf-rat-header* ,(cons (caar (cdddr *gf-rat-header*)) x) . 1) ))

(defun gf-disrep (x &optional (var '$x)) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (integerp x) x
    (maybe-char-is-fixnum-let ((c 0))
      (do ((not-plus? (null (cddr x))) p (e 0)) 
          ((null x) (if not-plus? (car p) (cons '(mplus simp) p)))
          (declare (fixnum e))
        (setq e (car x) c (cadr x) x (cddr x) 
              p (cond 
                  ((= 0 e) 
                    (cons c p) )
                  ((= 1 e) 
                    (if (= 1 c) 
                      (cons var p)
                      (cons `((mtimes simp) ,c ,var) p) ))
                  ((= 1 c)
                    (cons `((mexpt simp) ,var ,e) p) )
                  (t
                    (cons `((mtimes simp) ,c ((mexpt simp) ,var ,e)) p) )))))))
;;
;; -----------------------------------------------------------------------------


;; evaluation and adjustment ---------------------------------------------------
;;

;; an arbitrary polynomial is evaluated in a given field

(defmfun $gf_eval (a) 
  (gf-char? "gf_eval") 
  (let ((*ef-arith?*)) (gf-eval a *gf-red* "gf_eval")) )

(defmfun $ef_eval (a) 
  (ef-gf-field? "ef_eval")
  (let ((*ef-arith?*)) (gf-eval a *ef-red* "ef_eval")) )

(defun gf-eval (a red fun)
  (setq a (let ((modulus)) (car (prep1 a)))) 
  (cond
    ((integerp a) (gf-cmod a))
    (t 
      (setq a (gf-mod (cdr a)))
      (unless (typep (car a) 'fixnum)
        (gf-merror (intl:gettext "`~m': The exponent is expected to be a fixnum.") fun) )
      (gf-x2p (gf-nred a red)) )))


;; gf-mod adjusts arbitrary integer coefficients (pos, neg or unbounded)

(defun gf-mod (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (null x) nil
    (maybe-char-is-fixnum-let ((c 0))
      (do ((r x (cddr r)) res) 
          ((null r) (nreverse res))
        (unless (numberp (cadr r))
          (gf-merror (intl:gettext "gf: polynomials must be univariate.")) )
        (setq c (gf-cmod (cadr r))) 
        (unless (= c 0) (setq res (cons c (cons (car r) res)))) ))))

;; positive 2 symmetric mod:

(defun gf-np2smod (x) ;; modifies x
  (cond 
    ((null x) nil)
    ((not $gf_symmetric) x)
    (*ef-arith?*
      (*f-np2smod x *gf-card* #'(lambda (c) (neg (gf-ctimes (1- *gf-char*) c)))) )
    (t 
      (*f-np2smod x *gf-char* #'(lambda (c) (- (the integer c) *gf-char*))) )))

(defun *f-np2smod (x p cp2smod-fn)
  (if (null x) x
    (maybe-char-is-fixnum-let ((p2 (ash p -1)))
      (do ((r (cdr x) (cddr r))) (())
        (when (> (the integer (car r)) p2) 
          (rplaca r (funcall cp2smod-fn (car r))) ) 
        (when (null (cdr r)) (return x)) ))))

;; adjust a coefficient to a symmetric modulus:
(defun gf-cp2smod (c)
  (cond 
    ((not $gf_symmetric) c)
    (*ef-arith?*
      (if (> c (ash *gf-card* -1)) (neg (gf-ctimes c (1- *gf-char*))) c) )
    (t 
      (if (> c (ash *gf-char* -1)) (- (the integer c) *gf-char*) c) )))
;;
;; -----------------------------------------------------------------------------


;; arithmetic in Galois Fields - Maxima level functions ------------------------
;;

;; gf:

(defmfun $gf_neg (a) 
  (gf-char? "gf_neg")
  (let ((*ef-arith?*))
    (gf-x2p (gf-nminus (gf-p2x a))) ))

(defmfun $gf_add (&rest args) 
  (gf-char? "gf_add")
  (let ((*ef-arith?*))
    (setq args (mapcar #'gf-p2x args))
    (gf-x2p (reduce #'gf-plus args)) ))

(defmfun $gf_sub (&rest args) 
  (gf-char? "gf_sub")
  (let ((*ef-arith?*))
    (setq args (mapcar #'gf-p2x args))
    (gf-x2p (gf-plus (car args) (gf-minus (reduce #'gf-plus (cdr args))))) ))

(defmfun $gf_mult (&rest args) 
  (gf-char? "gf_mult")
  (let ((*ef-arith?*))
    (setq args (mapcar #'gf-p2x args))
    (and (not *gf-red*) 
         (not (some #'null args))
         (not (typep (apply #'+ (mapcar #'car args)) 'fixnum))
         (gf-merror (intl:gettext "`gf_mult': Resulting exponent won't be a fixnum.")) )
    (gf-x2p (reduce #'(lambda (x y) (gf-times x y *gf-red*)) args)) )) 

(defmfun $gf_reduce (a b) 
  (gf-char? "gf_reduce")
  (let ((*ef-arith?*))
    (gf-x2p (gf-nrem (gf-p2x a) (gf-p2x b))) ))

(defmfun $gf_inv (a) 
  (gf-red? "gf_inv") 
  (let ((*ef-arith?*))
    (setq a (gf-inv (gf-p2x a) *gf-red*))
    (when a (gf-x2p a)) )) ;; a is nil in case the inverse does not exist
      
(defmfun $gf_div (&rest args) 
  (gf-red? "gf_div")
  (unless (cadr args)
    (gf-merror (intl:gettext "`gf_div' needs at least two arguments." )) )
  (let* ((*ef-arith?*) 
         (a2 (mapcar #'gf-p2x args))
         (a2 (cons (car a2) (mapcar #'(lambda (x) (gf-inv x *gf-red*)) (cdr a2)))) )
    (cond
      ((some #'null (cdr a2)) ;; but check if exact division is possible ..
        (let ((q (gf-p2x (car args))) r)
          (setq args (cdr args))
          (do ((d (car args) (car args))) 
              ((null d) (gf-x2p q))
            (multiple-value-setq (q r) (gf-divide q (gf-p2x d)))
            (when r (return)) ;; .. in case it is not return false 
            (setq args (cdr args)) )))
      (t ;; a / b = a * b^-1 :
        (gf-x2p (reduce #'(lambda (x y) (gf-times x y *gf-red*)) a2)) )))) 

(defmfun $gf_exp (a n)
  (gf-char? "gf_exp") 
  (let ((*ef-arith?*))
    (cond 
      ((not n) 
        (gf-merror (intl:gettext "`gf_exp' needs two arguments.")) )
      ((not (integerp n))
        (gf-merror (intl:gettext "Second argument to `gf_exp' must be an integer.")) )
      ((< (the integer n) 0)
        (unless *gf-red*
          (gf-merror (intl:gettext "`gf_exp': Unknown reduction polynomial.")) )
        (setq a (gf-inv (gf-p2x a) *gf-red*))
        (when a ($gf_exp (gf-x2p a) (neg n))) ) ;; a is nil in case the inverse does not exist
      (*gf-x^p-powers*
        (gf-x2p (gf-pow$ (gf-p2x a) n *gf-red*)) )
      (t 
        (setq a (gf-p2x a))
        (and (not *gf-red*) 
             (not (null a))
             (not (typep (* n (car a)) 'fixnum))
             (gf-merror (intl:gettext "`gf_exp': Resulting exponent won't be a fixnum.")) )
        (gf-x2p (gf-pow a n *gf-red*)) ))))

;; ef:

(defmfun $ef_neg (a) 
  (ef-gf-field? "ef_neg")
  (let ((*ef-arith?* t))
    (gf-x2p (gf-nminus (gf-p2x a))) ))

(defmfun $ef_add (&rest args) 
  (ef-gf-field? "ef_add")
  (let ((*ef-arith?* t))
    (setq args (mapcar #'gf-p2x args))
    (gf-x2p (reduce #'gf-plus args)) ))

(defmfun $ef_sub (&rest args) 
  (ef-gf-field? "ef_sub")
  (let ((*ef-arith?* t))
    (setq args (mapcar #'gf-p2x args))
    (gf-x2p (gf-plus (car args) (gf-minus (reduce #'gf-plus (cdr args))))) ))

(defmfun $ef_mult (&rest args) 
  (ef-gf-field? "ef_mult")
  (let ((*ef-arith?* t) 
        (red *ef-red*) )
    (setq args (mapcar #'gf-p2x args))
    (and (not red) 
         (not (some #'null args))
         (not (typep (apply #'+ (mapcar #'car args)) 'fixnum))
         (gf-merror (intl:gettext "`ef_mult': Resulting exponent won't be a fixnum.")) )
    (gf-x2p (reduce #'(lambda (x y) (gf-times x y red)) args)) )) 

(defmfun $ef_reduce (a b) 
  (ef-gf-field? "ef_reduce")
  (let ((*ef-arith?* t))
    (gf-x2p (gf-nrem (gf-p2x a) (gf-p2x b))) ))

(defmfun $ef_inv (a) 
  (ef-red? "ef_inv")
  (let ((*ef-arith?* t))
    (setq a (gf-inv (gf-p2x a) *ef-red*))
    (when a (gf-x2p a)) ))

(defmfun $ef_div (&rest args) 
  (ef-red? "ef_div")
  (unless (cadr args)
    (gf-merror (intl:gettext "`ef_div' needs at least two arguments." )) )
  (let ((*ef-arith?* t) 
        (red *ef-red*) )
    (setq args (mapcar #'gf-p2x args))
    (setq args 
      (cons (car args) (mapcar #'(lambda (x) (gf-inv x red)) (cdr args))) )
    (cond
      ((null (car args)) 0)
      ((some #'null (cdr args)) nil)
      (t (gf-x2p (reduce #'(lambda (x y) (gf-times x y red)) args))) ))) 

(defmfun $ef_exp (a n) 
  (ef-gf-field? "ef_exp")
  (let ((*ef-arith?* t))
    (cond 
      ((< (the integer n) 0)
        (unless *ef-red*
          (gf-merror (intl:gettext "`ef_exp': Unknown reduction polynomial.")) )
        (setq a (gf-inv (gf-p2x a) *ef-red*))
        (when a ($ef_exp (gf-x2p a) (neg n))) ) 
      (*ef-x^q-powers*  
        (gf-x2p (gf-pow$ (gf-p2x a) n *ef-red*)) )
      (t  
        (setq a (gf-p2x a))
        (and (not *ef-red*) 
             (not (null a))
             (not (typep (* n (car a)) 'fixnum))
             (gf-merror (intl:gettext "`ef_exp': Resulting exponent won't be a fixnum.")) )
        (gf-x2p (gf-pow a n *ef-red*)) ))))
;;
;; -----------------------------------------------------------------------------


;; arithmetic in Galois Fields - Lisp level functions --------------------------
;;

;; Both gf (base field) and ef (extension field) Maxima level functions use 
;; this Lisp level functions. The switch *ef-arith?* controls how the coefficients 
;; were treated. The coefficient functions gf-ctimes and friends behave 
;; differently depending on *ef-arith?*. See above definitions.

;; Remark: A prefixed character 'n' indicates a destructive function.

;; c * x

(defun gf-xctimes (x c)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-char-is-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
      (do* ((res (list (car x) (gf-ctimes c (cadr x))))
            (r (cdr res) (cddr r)) 
            (rx (cddr x) (cddr rx)) )
           ((null rx) res)
        (rplacd r (list (car rx) (gf-ctimes c (cadr rx)))) ))))

(defun gf-nxctimes (x c) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-char-is-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
    (do ((r (cdr x) (cddr r)))
        ((null r) x)
      (rplaca r (gf-ctimes c (car r))) ))))

;; c*v^e * x

(defun gf-xectimes (x e c)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum e))
  (maybe-char-is-fixnum-let ((c c))
    (if (or (= 0 c) (null x)) nil
      (do* ((res (list (+ e (the fixnum (car x))) (gf-ctimes c (cadr x))))
            (r (cdr res) (cddr r)) 
            (rx (cddr x) (cddr rx)) )
           ((null rx) res)
        (rplacd r (list (+ e (the fixnum (car rx))) (gf-ctimes c (cadr rx)))) ))))

;; - x

(defun gf-minus (x) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (or (null x) (= 2 *gf-char*)) x
    (do* ((res (list (car x) (gf-cminus-b (cadr x))))
          (r (cdr res) (cddr r)) 
          (rx (cddr x) (cddr rx)) )
         ((null rx) res)
      (rplacd r (list (car rx) (gf-cminus-b (cadr rx)))) )))

(defun gf-nminus (x) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (or (null x) (= 2 *gf-char*)) x
    (do ((r (cdr x) (cddr r))) (())
      (rplaca r (gf-cminus-b (car r)))
      (when (null (cdr r)) (return x)) )))

;; x + c, 0 < c < *gf-char*

(defun gf-nxcplus (x c) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-char-is-fixnum-let ((c c))
    (cond 
      ((null x) (list 0 c))
      (t (setq x (nreverse x))
         (cond
           ((= 0 (the fixnum (cadr x)))
             (setq c (gf-cplus-b c (car x)))
             (if (= 0 c)
               (setq x (cddr x))
               (rplaca x c) ))
           (t (setq x (cons c (cons 0 x)))) )
         (nreverse x) ))))

;; x + y

(defun gf-plus (x y) 
  (cond 
    ((null x) y)
    ((null y) x)
    (t (gf-nplus (copy-list x) y)) )) 

;; merge y into x

(defun gf-nplus (x y) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null x) y)
    ((null y) x)
    (t
      (maybe-char-is-fixnum-let ((cy 0)(c 0))
        (prog ((ex 0)(ey 0) r) (declare (fixnum ex ey))
          a1
          (setq ex (car x) ey (car y) cy (cadr y))
          (cond 
            ((> ey ex)
              (setq x (cons ey (cons cy x)) y (cddr y)) ) 
            ((= ey ex)
              (setq c (gf-cplus-b (cadr x) cy) y (cddr y))
              (cond  
                ((= 0 c)
                  (when (null (setq x (cddr x))) (return y)) 
                  (when (null y) (return x))
                  (go a1) )
                (t (rplaca (cdr x) c)) ))
            (t (setq r (cdr x)) (go b)) )
          (setq r (cdr x))
          a
          (when (null y) (return x))
          (setq ey (car y) cy (cadr y))
          b
          (while (and (cdr r) (> (the fixnum (cadr r)) ey))
            (setq r (cddr r)) )
          (cond 
            ((null (cdr r)) (rplacd r y) (return x))
            ((> ey (the fixnum (cadr r)))
              (rplacd r (cons ey (cons cy (cdr r))))
              (setq r (cddr r) y (cddr y)) )
            (t
              (setq c (gf-cplus-b (caddr r) cy) y (cddr y))
              (if (= 0 c)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) c) )) ) 
          (go a) )))))

;; x + c*v^e*y

(defun gf-xyecplus (x y e c) 
  (cond 
    ((null y) x)
    ((null x) (gf-xectimes y e c))
    (t (gf-nxyecplus (copy-list x) y e c) )))

;; merge c*v^e*y into x

(defun gf-nxyecplus (x y e c) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null y) x)
    ((null x) (gf-xectimes y e c))
    (t 
      (maybe-char-is-fixnum-let ((cy 0) (cc 0))
        (prog ((e e) (ex 0) (ey 0) r) (declare (fixnum e ex ey))
          a1
          (setq ey (+ (the fixnum (car y)) e) 
                cy (gf-ctimes c (cadr y)) 
                ex (car x) )
          (cond 
            ((> ey ex)
              (setq x (cons ey (cons cy x)) y (cddr y)) ) 
            ((= ey ex)
              (setq cc (gf-cplus-b (cadr x) cy) y (cddr y)) 
              (cond  
                ((= 0 cc)
                  (when (null (setq x (cddr x))) (return (gf-xectimes y e c))) 
                  (when (null y) (return x))
                  (go a1) )
                (t (rplaca (cdr x) cc)) ))
            (t (setq r (cdr x)) (go b)) )
          (setq r (cdr x))
          a
          (when (null y) (return x))
          (setq ey (+ (the fixnum (car y)) e) 
                cy (gf-ctimes c (cadr y)) )
          b
          (when (null (cdr r)) (go d))
          (setq ex (cadr r))
          (cond 
            ((> ey ex)
              (rplacd r (cons ey (cons cy (cdr r))))
              (setq r (cddr r) y (cddr y))
              (go a) )
            ((= ey ex)
              (setq cc (gf-cplus-b (caddr r) cy)) 
              (if (= 0 cc)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) cc) )
              (setq y (cddr y))
              (go a) ) 
            (t (setq r (cddr r)) (go b)) ) 
          d
          (do () ((null y))
            (setq x (nconc x (list (+ (the fixnum (car y)) e) (gf-ctimes c (cadr y))))
                  y (cddr y) ))
          (return x) ) ))))

;; x * y 
;;
;; For sparse polynomials (in Galois Fields) with not too high degrees 
;; simple school multiplication is faster than Karatsuba.
;; 
;; x * y = (x1 + x2 + ... + xk) * (y1 + y2 + ... + yn)
;;       =  x1 * (y1 + y2 + ... + yn) + x2 * (y1 + y2 + ... + yn) + ...
;;
;;         where e.g. xi = ci*v^ei
;;
(defun gf-times (x y red)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (or (null x) (null y)) nil
    (maybe-char-is-fixnum-let ((c 0) (cx 0))
      (do* ((res (gf-xectimes y (car x) (cadr x))) ;; x1 * (y1 + y2 + ... + yn). summands in res are sorted. res is a new list.
            (r1 (cdr res))                         ;; r1 marks the place of xi*y1 in res. x[i+1]*y1 will be smaller.
            ry                                     ;; ry iterates over y
            (x (cddr x) (cddr x))                  ;; each loop: res += xi * (y1 + y2 + ... + yn)
            (e 0)(ex 0) )
           ((or (null x)(null y)) (gf-nred res red))
           (declare (fixnum e ex))
        (setq ry y                                 ;; start with y1 again
              ex (car x) cx (cadr x)               ;; xi = ci*v^ei
              e (+ ex (the fixnum (car ry)))       ;; c*v^e = xi*y1
              c (gf-ctimes (cadr ry) cx) )         ;; zero divisor free mult in Fp^n
      
        (while (and (cdr r1) (< e (the fixnum (cadr r1))))
          (setq r1 (cddr r1)) )                    ;; mark the position of xi*y1
      
        (do ((r r1)) (())                          ;; merge xi*y1 into res and then xi*y2, etc...
          (cond  
            ((or (null (cdr r)) (> e (the fixnum (cadr r))))
              (rplacd r (cons e (cons c (cdr r)))) 
              (setq r (cddr r)) )
            ((= 0 (setq c (gf-cplus-b (caddr r) c)))
              (rplacd r (cdddr r)) ) 
            (t (rplaca (setq r (cddr r)) c)) )
          
          (when (null (setq ry (cddr ry))) (return))
          (setq e (+ ex (the fixnum (car ry))) 
                c (gf-ctimes (cadr ry) cx) )
        
          (while (and (cdr r) (< e (the fixnum (cadr r)))) 
            (setq r (cddr r)) ) )) )))

;; x^2
;;
;; x * x = (x_1 + x_2 + ... + x_k) * (x_1 + x_2 + ... + x_k)
;;
;;       = x_1^2 + 2*x_1*x_2 + 2*x_1*x_3 + ... + x_2^2 + 2*x_2*x_3 + 2*x_2*x_4 + ...
;;
;;       = x_k^2 + x_{k-1}^2 + 2*x_{k-1}*xk + x_{k-2}^2 + 2*x_{k-2}*x_{k-1} + 2*x_{k-2}*xk + ...
;;
;; The reverse needs some additional consing but is slightly faster.
;;
(defun gf-sq (x red) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null x) x) 
    ((and (not *ef-arith?*) (eql *gf-char* 2)) ;; the mod 2 case degrades to x_1^2 + x_2^2 + ... + x_k^2
      (do (res)
          ((null x) (gf-nred (nreverse res) red))
        (setq res (cons 1 (cons (ash (car x) 1) res))
              x (cddr x) ) ))
    (t 
      (maybe-char-is-fixnum-let ((ci 0)(*2ci 0)(c 0))
        (setq x (reverse x)) ;; start with x_k
        (prog (res           ;; result
               r             ;; insertion marker in res
               acc           ;; acc accumulates previous x_i
               r1            ;; r1 iterates in each loop over acc
               (e 0) (ei 0) ) (declare (fixnum e ei))
          a1
          (setq ci (car x) ei (cadr x)                            ;; x_i = ci*v^ei
                *2ci (gf-cplus-b ci ci)                           ;; 2*ci (2*ci # 0 when *gf-char* # 2) 
                res (cons (+ ei ei) (cons (gf-ctimes ci ci) res)) ;; res += x_i^2 (ci^2 # 0, no zero divisors) 
                r (cdr res)                                       ;; place insertion marker behind x_i^2
                r1 acc ) 
          a
          (when (or (null r1) (= 0 *2ci)) ;; in ef *2ci might be 0 !
            (when (null (setq x (cddr x))) (return (gf-nred res red))) 
            (setq acc (cons ei (cons ci acc)))           ;; cons previous x_i to acc ..
            (go a1) )                                    ;; .. and start next loop
        
          (setq e (+ ei (the fixnum (car r1))) 
                c (gf-ctimes *2ci (cadr r1))
                r1 (cddr r1) )
        
          (while (< e (the fixnum (cadr r)))
            (setq r (cddr r)) )
          (cond 
            ((> e (the fixnum (cadr r)))
              (rplacd r (cons e (cons c (cdr r))))
              (setq r (cddr r)) )
            (t
              (setq c (gf-cplus-b c (caddr r)))
              (if (= 0 c)
                (rplacd r (cdddr r))
                (rplaca (setq r (cddr r)) c) ) )) 
          (go a) ))) ))

;; x^n mod y

(defun gf-pow (x n red) ;; assume 0 <= n
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer n))
  (cond 
    ((= 0 n) (list 0 1))
    ((null x) nil) 
    (t (do (res)(())
         (when (oddp n)
           (setq res (if res (gf-times x res red) x)) 
           (when (= 1 n)
             (return-from gf-pow res) ))
         (setq n (ash n -1) 
               x (gf-sq x red)) ))))

;; use precomputed *gf-x^p-powers* resp. *ef-x^q-powers*

(defun gf-pow$ (x n red) 
  (if *ef-arith?* 
    (*f-pow$ x n red *gf-card* *ef-card* *ef-x^q-powers*)
    (*f-pow$ x n red *gf-char* *gf-card* *gf-x^p-powers*) ))

(defun *f-pow$ (x n red p card x^p-powers) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer n p card))
  (cond 
    ((= 0 n) (list 0 1))
    ((null x) nil) 
    ((>= n card) (gf-pow x n red))
    (t 
      (let ((prod (list 0 1)) 
            (j 0) n-base-p y )
        (do (quo r) ((= 0 n))
          (multiple-value-setq (quo r) (truncate n p))
          (push r n-base-p)
          (setq n quo) )
        (dolist (ni (nreverse n-base-p))
          (setq y (gf-compose (svref x^p-powers j) x red)
                y (gf-pow y ni red)
                prod (gf-times prod y red)  
                j (1+ j) ))
        prod ))))

;; remainder:
;; x - quotient(x, y) * y 

(defun gf-rem (x y)
  (when (null y) 
    (gf-merror (intl:gettext "~m arithmetic: Quotient by zero") (if *ef-arith?* "ef" "gf")) )
  (if (null x) x 
    (gf-nrem (copy-list x) y) ))

(defun gf-nrem (x y) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (when (null y) 
    (gf-merror (intl:gettext "~m arithmetic: Quotient by zero") (if *ef-arith?* "ef" "gf")) )
  (if (null x) x 
    (maybe-char-is-fixnum-let ((c 0) (lcx 0) (lcy-inv (gf-cinv (cadr y))))
      (let ((e 0) (ley (car y)))
           (declare (fixnum e ley))
        (setq lcy-inv (gf-cminus-b lcy-inv))
        (do ((y (cddr y)))
            ((null x) x)   
          (setq e (- (the fixnum (car x)) ley))
          (when (< e 0) (return x)) 
          (setq lcx (cadr x) 
                c (gf-ctimes lcx lcy-inv)
                x (gf-nxyecplus (cddr x) y e c)) )))))

;; reduce x by red
;;
;; assume lc(red) = 1, reduction poly is monic

(defun gf-nred (x red) ;; modifies x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (or (null x) (null red)) x
    (let ((e 0) (le-red (car red)))
         (declare (fixnum e le-red))
      (setq red (cddr red))
      (do () ((null x) x)
        (setq e (- (the fixnum (car x)) le-red))
        (when (< e 0) (return x))
        (setq x (gf-nxyecplus (cddr x) red e (gf-cminus-b (cadr x)))) ))))

;; (monic) gcd

(defun gf-gcd (x y)
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null x) y)
    ((null y) x)
    (t (let ((r nil))
         (do ()((null y) 
                 (if (eql 0 (car x)) (list 0 1) 
                   (gf-xctimes x (gf-cinv (cadr x))) )) 
           (setq r (gf-rem x y)) 
           (psetf x y y r) )))))

;; (monic) extended gcd

(defun gf-gcdex (x y red) 
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (let ((x1 (list 0 1)) x2 y1 (y2 (list 0 1)) q r) 
    (do ()((null y) 
            (let ((inv (gf-cinv (cadr x)))) 
              (mapcar #'(lambda (a) (gf-xctimes a inv)) (list x1 x2 x)) )) 
      (multiple-value-setq (q r) (gf-divide x y))
      (psetf x y y r)
      (psetf 
        y1 (gf-nplus (gf-nminus (gf-times q y1 red)) x1) 
        x1 y1 ) 
      (psetf 
        y2 (gf-nplus (gf-nminus (gf-times q y2 red)) x2) 
        x2 y2 ) )))

;; inversion: y^-1
;;
;; in case the inverse does not exist it returns nil 
;; (might happen when reduction poly isn't irreducible)

(defun gf-inv (y red) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (when (null y) 
    (gf-merror (intl:gettext "~m arithmetic: Quotient by zero") (if *ef-arith?* "ef" "gf")) )
  (let ((y1 (list 0 1)) (x red) x1 q r)
    (setq y (copy-list y))
    (do ()((null y) 
            (when (= 0 (car x)) ;; gcd = 1 (const)
              (gf-nxctimes x1 (gf-cinv (cadr x))) )) 
      (multiple-value-setq (q r) (gf-divide x y)) 
      (psetf x y y r) 
      (psetf 
        x1 y1
        y1 (gf-nplus (gf-nminus (gf-times q y1 red)) x1) )) )) 

;; quotient and remainder

(defun gf-divide (x y)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond 
    ((null y)
      (gf-merror (intl:gettext "~m arithmetic: Quotient by zero") (if *ef-arith?* "ef" "gf")) )
    ((null x) (values nil nil))
    (t 
      (maybe-char-is-fixnum-let ((c 0) (lcx 0) (lcyi (gf-cinv (cadr y))))
        (let ((e 0) (ley (car y)))
             (declare (fixnum e ley))
          (setq x (copy-list x)) 
          (do (q (y (cddr y)))
              ((null x) (values (nreverse q) x))
            (setq e (- (the fixnum (car x)) ley))
            (when (< e 0) 
              (return (values (nreverse q) x)) )
            (setq lcx (cadr x) 
                  x (cddr x)
                  c (gf-ctimes lcx lcyi) )
            (unless (null y) (setq x (gf-nxyecplus x y e (gf-cminus-b c)))) 
            (setq q (cons c (cons e q))) ))))))
;;
;; -----------------------------------------------------------------------------


;; polynomial/number/list - conversions ----------------------------------------
;;

;; poly 2 number:

(defmfun $ef_p2n (p)
  (gf-data? "ef_p2n")
  (let ((*ef-arith?* t)) (gf-x2n (gf-p2x p))) )

(defmfun $gf_p2n (p &optional gf-char) 
  (let ((*ef-arith?*)) 
    (cond 
      (gf-char
        (let ((*gf-char* gf-char)) (gf-x2n (gf-p2x p))) )
      (*gf-char?*
        (gf-x2n (gf-p2x p)) )
      (t
        (gf-merror (intl:gettext "`gf_p2n': missing modulus.")) ))))

(defun gf-x2n (x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (null x) 0
    (maybe-char-is-fixnum-let ((m *gf-char*))
      (when *ef-arith?* (setq m *gf-card*))
      (do ((n 0))(())
        (incf n (cadr x))
        (if (null (cddr x)) 
          (return (* n (expt m (the fixnum (car x)))))
          (setq n (* n (expt m (- (the fixnum (car x)) (the fixnum (caddr x)))))) )
        (setq x (cddr x)) ))))

;; number 2 poly:

(defun gf-n2p-errchk (fun n)
  (unless (integerp n)
    (gf-merror (intl:gettext "`~m': Not an integer: ~m") fun n) ))

(defmfun $gf_n2p (n &optional gf-char) 
  (gf-n2p-errchk "gf_n2p" n)
  (let ((*ef-arith?*)) 
    (cond 
      (gf-char
        (gf-set-rat-header)
        (let ((*gf-char* gf-char)) (gf-x2p (gf-n2x n))) )
      (*gf-char?*
        (gf-x2p (gf-n2x n)) )
      (t
        (gf-merror (intl:gettext "`gf_n2p': missing modulus.")) ))))

(defmfun $ef_n2p (n)
  (gf-data? "ef_n2p")
  (gf-n2p-errchk "ef_n2p" n)
  (let ((*ef-arith?* t)) (gf-x2p (gf-n2x n))) )

(defun gf-n2x (n)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer n))
  (maybe-char-is-fixnum-let ((r 0) (m *gf-char*)) 
    (let ((e 0)) (declare (fixnum e))
      (when *ef-arith?* (setq m *gf-card*))
      (do (x) 
          ((= 0 n) x)
        (multiple-value-setq (n r) (truncate n m))
        (unless (= 0 r)
          (setq x (cons e (cons r x))) )
        (incf e) ))))

;; poly 2 list:

(defmfun $ef_p2l (p &optional (len 0))
  (declare (fixnum len))
  (let ((*ef-arith?* t))
    (cons '(mlist simp) (gf-x2l (gf-p2x-raw p) len)) )) ;; more flexibility ...

(defmfun $gf_p2l (p &optional (len 0)) ;; len = 0 : no padding
  (declare (fixnum len))
  (let ((*ef-arith?*))
    (cons '(mlist simp) (gf-x2l (gf-p2x-raw p) len)) )) ;; ... by omitting mod reduction

(defun gf-x2l (x len)
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum len))
  (do* ((e (if x (the fixnum (car x)) 0)) 
        (k (max e (1- len)) (1- k))
        l ) 
       ((< k 0) (nreverse l))
       (declare (fixnum e k))
    (cond
      ((or (null x) (> k e))
        (push 0 l) )
      ((= k e) 
        (push (cadr x) l)
        (setq x (cddr x))
        (unless (null x) (setq e (the fixnum (car x)))) ))))

;; list 2 poly:

(defmfun $ef_l2p (l)
  (gf-l2p-errchk l "ef_l2p")
  (let ((*ef-arith?* t)) (gf-x2p (gf-l2x (cdr l)))) )

(defmfun $gf_l2p (l)
  (gf-l2p-errchk l "gf_l2p")
  (let ((*ef-arith?*)) (gf-x2p (gf-l2x (cdr l)))) )

(defun gf-l2p-errchk (l fun)
  (unless ($listp l)
    (gf-merror (intl:gettext "`~m': Argument must be a list of integers.") fun) ))

(defun gf-l2x (l)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq l (reverse l))
  (maybe-char-is-fixnum-let ((c 0)) 
    (let ((e 0)) (declare (fixnum e))
      (do (x)
          ((null l) x)
        (unless (= 0 (setq c (car l)))
          (setq x (cons e (cons c x))) )
        (setq l (cdr l))
        (incf e) ))))

;; list 2 number:

(defmfun $gf_l2n (l) 
  (gf-char? "gf_l2n")
  (gf-l2p-errchk l "gf_l2n")
  (let ((*ef-arith?*)) (gf-l2n (cdr l))) )

(defmfun $ef_l2n (l) 
  (gf-data? "ef_l2n")
  (gf-l2p-errchk l "ef_l2n")
  (let ((*ef-arith?* t)) (gf-l2n (cdr l))) )

(defun gf-l2n (l) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-char-is-fixnum-let ((m *gf-char*) (c1 (car l)) (c 0))
    (when *ef-arith?* (setq m *gf-card*))
    (setq l (reverse (cdr l)))
    (do ((n 0) (b 1)) 
        ((null l) (+ (* c1 b) n))
        (declare (integer n b))
      (unless (= 0 (setq c (car l))) (incf n (* c b))) 
      (setq b (* b m) l (cdr l)) )))

;; number 2 list:

(defmfun $gf_n2l (n &optional (len 0)) ;; in case of len = 0 the list isn't padded or truncated
  (declare (integer n) (fixnum len))
  (gf-char? "gf_n2l")
  (gf-n2p-errchk "gf_n2l" n)
  (cons '(mlist simp) 
    (let ((*ef-arith?*)) 
      (if (= 0 len) (gf-n2l n) (gf-n2l-twoargs n len)) )))

(defmfun $ef_n2l (n &optional (len 0)) ;; in case of len = 0 the list isn't padded or truncated
  (declare (integer n) (fixnum len))
  (gf-data? "ef_n2l")
  (gf-n2p-errchk "ef_n2l" n)
  (cons '(mlist simp) 
    (let ((*ef-arith?* t)) 
      (if (= 0 len) (gf-n2l n) (gf-n2l-twoargs n len)) )))

(defun gf-n2l (n) ;; this version is frequently called by gf-precomp, keep it simple
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer n))
  (maybe-char-is-fixnum-let ((m *gf-char*) (r 0))
    (when *ef-arith?* (setq m *gf-card*))
    (do (l) ((= 0 n) l)
      (multiple-value-setq (n r) (truncate n m))
      (setq l (cons r l)) )))

(defun gf-n2l-twoargs (n len)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer n) (fixnum len))
  (maybe-char-is-fixnum-let ((m *gf-char*) (r 0))
    (when *ef-arith?* (setq m *gf-card*))
    (do (l) ((= 0 len) l) 
      (multiple-value-setq (n r) (truncate n m))
      (setq l (cons r l))
      (decf len) )))
;;
;; -----------------------------------------------------------------------------


;; irreducibility (Ben-Or algorithm) -------------------------------------------
;;

(defmfun $gf_irreducible_p (a &optional p) 
  (cond
    (p (unless (and (integerp p) (primep p))
         (gf-merror (intl:gettext 
           "`gf_irreducible_p': Second argument must be a prime number." )) ))
    (t (gf-char? "gf_irreducible_p") 
       (setq p *gf-char*) ))
  (let* ((*ef-arith?*)
         (*gf-char* p)                
         (x (gf-p2x a)) n) ;; gf-p2x depends on *gf-char*
    (cond
      ((null x) nil)
      ((= 0 (setq n (car x))) nil)
      ((= 1 n) t)
      (t (gf-irr-p x p (car x))) )))

(defmfun $ef_irreducible_p (a) 
  (ef-gf-field? "ef_irreducible_p")
  (let ((*ef-arith?* t))
    (setq a (gf-p2x a)) 
    (gf-irr-p a *gf-card* (car a)) ))

;; is y irreducible of degree n over Fq[x] ?
;;
;;                 q,n > 1 ! 
(defun gf-irr-p (y q n) ;; gf-irr-p is independent from any settings
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer q) (fixnum n))
  (when (= 1 (the integer (cadr y)))
    (let* ((*gf-char* (car (cfactorw q)))
           (x (list 1 1)) 
           (mx (gf-minus x)) ) ;; gf-minus needs *gf-char*
      (do ((i 1 (1+ i)) (xq x) (n2 (ash n -1))) 
          ((> i n2) t)
          (declare (fixnum i n2))
        (setq xq (gf-pow xq q y))
        (unless (= 0 (car (gf-gcd y (gf-plus xq mx))))
          (return) ) ))))

;; find an irreducible element
;;
;; gf_irreducible is independent from any settings
;;
(defmfun $gf_irreducible (p n)  ;; p,n : desired characteristic and degree
  (unless (and (integerp p) (primep p) (integerp n))
    (gf-merror (intl:gettext "`gf_irreducible' needs a prime number and an integer.")) )
  (gf-set-rat-header)
  (let* ((*gf-char* p) 
         (*ef-arith?*)
         (irr (gf-irr p n)) )
    (when irr (gf-x2p irr)) ))

(defmfun $ef_irreducible (n) ;; n : desired degree
  (ef-gf-field? "ef_irreducible")
  (unless (integerp n)
    (gf-merror (intl:gettext "`ef_irreducible' needs an integer.")) )
  (let* ((*ef-arith?* t)
         (irr (ef-irr n)) ) 
    (when irr (gf-x2p irr)) ))


(defun gf-irr (p n) 
  (*f-irr p n) )

(defun ef-irr (n) 
  (*f-irr *gf-card* n) )

(defun *f-irr (q n) 
  #+ (or ccl ecl gcl)  (declare (optimize (speed 3) (safety 0)))
  (when (= 1 n)
    (return-from *f-irr (list 1 1)) )
  (let* ((inc (min $gf_coeff_limit q))
         (i-lim (expt inc n))
         x )
    (do ((i 1 (1+ i))) 
        ((>= i i-lim) 
          (gf-merror (intl:gettext "No irreducible polynomial found.~%~\
                                    `gf_coeff_limit' might be too small.~%" )))
      (setq x (let ((*gf-char* inc)) (gf-n2x i)))
      (when (= 0 (car (last x 2)))
        (setq x (cons n (cons 1 x)))
        (when (gf-irr-p x q n) (return-from *f-irr x)) ))))
;;
;; -----------------------------------------------------------------------------


;; Primitive elements ----------------------------------------------------------
;;

;; Tests if an element is primitive in the field 

(defmfun $gf_primitive_p (a)
  (gf-data? "gf_primitive_p") ;; --> precomputations are performed
  (let* ((*ef-arith?*)
         (x (gf-p2x a)) 
         (n (gf-x2n x)) )
    (cond 
      ((or (= 0 n) (>= n *gf-card*)) nil)
      ((= 1 *gf-exp*) 
        (zn-primroot-p n *gf-char* *gf-ord* (mapcar #'car *gf-fs-ord*)) )
      (t 
        (gf-prim-p x) ))))

(defmfun $ef_primitive_p (a) 
  (ef-data? "ef_primitive_p") ;; --> precomputations are performed
  (let ((*ef-arith?* t)) 
    (setq a (gf-p2x a))
    (cond 
      ((null a) nil)
      ((>= (car a) *ef-exp*) nil) 
      ((= (car a) 0)
        (if (= 1 *ef-exp*) 
          (let ((*ef-arith?*)) (gf-prim-p (gf-n2x (cadr a))))
          nil ))
      (t (ef-prim-p a)) )))


;; Testing primitivity in (Fq^n)*:

;; We check f(x)^ei # 1 (ei = ord/pi) for all prime factors pi of ord.
;; 
;; With ei = sum(aij*q^j, j,0,m) in base q and using f(x)^q = f(x^q) we get
;; 
;; f(x)^ei = f(x)^sum(aij*q^j, j,0,m) = prod(f(x^q^j)^aij, j,0,m).


;; Special case: red is irreducible, f(x) = x+c and pi|ord and pi|q-1.
;; 
;; Then ei = (q^n-1)/(q-1) * (q-1)/pi = sum(q^j, j,0,n-1) * (q-1)/pi.
;; 
;; With ai = (q-1)/pi and using red(z) = prod(z - x^q^j, j,0,n-1) we get
;; 
;; f(x)^ei = f(x)^sum(ai*q^j, j,0,n-1) = (prod(f(x)^q^j, j,0,n-1))^ai 
;; 
;;         = (prod(x^q^j + c, j,0,n-1))^ai = ((-1)^n * prod(-c - x^q^j, j,0,n-1))^ai
;; 
;;         = ((-1)^n * red(-c))^ai
;;

(defun gf-prim-p (x) 
  (*f-prim-p x *gf-irred?* *gf-red* *gf-fsx* *gf-fsx-base-p* *gf-x^p-powers*) )

(defun ef-prim-p (x) 
  (*f-prim-p x *ef-irred?* *ef-red* *ef-fsx* *ef-fsx-base-q* *ef-x^q-powers*) )
;; 
;; *f-prim-p uses precomputations
;;
(defun *f-prim-p (x irr? red fs fs-base-q x^q-powers) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (unless (or irr? (gf-unit-p x red))
    (return-from *f-prim-p) )
  (let ((exponent (car red))
        (x+c? (and (= (car x) 1) (= (cadr x) 1)))
        y prod -c z )
    (do ((i 0 (1+ i)) (j 0 0) (lf (array-dimension fs 0)))
        ((= i lf) t)
        (declare (fixnum i j lf))
      (cond 
        ((and irr? x+c? (cadr (svref fs i)))                ;; linear and pi|ord and pi|p-1
          (setq -c (if (= 2 (length x)) 0 (gf-cminus-b (car (last x)))) 
                z (list 0 (gf-at red -c)) )
          (when (oddp exponent) (setq z (gf-minus z)))      ;;  (-1)^n * red(-c)
          (setq z (gf-pow z (caddr (svref fs i)) red))      ;; ((-1)^n * red(-c))^ai
          (when (or (null z) (equal z '(0 1))) 
            (return nil) ))
        (t
          (setq prod (list 0 1))
          (dolist (aij (svref fs-base-q i))
            (setq y (gf-compose (svref x^q-powers j) x red) ;;      f(x^q^j)
                  y (gf-pow y aij red)                      ;;      f(x^q^j)^aij
                  prod (gf-times prod y red)  
                  j (1+ j) ))
          (when (or (null prod) (equal prod '(0 1)))        ;; prod(f(x^q^j)^aij, j,0,m)
            (return nil) )) )))) 

;; modular composition (uses Horner and square and multiply)
;; y(x) mod red

(defmfun $gf_compose (a b)
  (gf-red? "gf_compose")
  (let ((*ef-arith?*)) 
    (gf-x2p (gf-compose (gf-p2x a) (gf-p2x b) *gf-red*)) ))

(defmfun $ef_compose (a b)
  (ef-red? "ef_compose")
  (let ((*ef-arith?* t)) 
    (gf-x2p (gf-compose (gf-p2x a) (gf-p2x b) *ef-red*)) ))

(defun gf-compose (x y red) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond
    ((or (null x) (null y)) nil)
    ((= 0 (car y)) y)
    ((= 0 (car x)) 
      (let ((n (gf-at y (cadr x))))
        (if (= 0 n) nil (list 0 n)) ))
    (t
      (do (res) (())
        (setq res (gf-nxcplus res (cadr y))) 
        (when (null (cddr y)) 
          (return (gf-times res (gf-pow x (car y) red) red)) ) 
        (setq res (gf-times res (gf-pow x (- (car y) (caddr y)) red) red)
              y (cddr y) ) ))))

;; a(n) with poly a and integer n

(defun gf-at-errchk (n fun)
  (unless (integerp n)
    (gf-merror (intl:gettext "`~m': Second argument must be an integer.") fun) ))

(defmfun $gf_at (a n) ;; integer n
  (gf-char? "gf_at")
  (gf-at-errchk n "gf_at")
  (let ((*ef-arith?*)) 
    (gf-at (gf-p2x a) n) ))

(defmfun $ef_at (a n) ;; poly a, integer n
  (ef-gf-field? "ef_at")
  (gf-at-errchk n "ef_at")
  (let ((*ef-arith?* t)) 
    (gf-at (gf-p2x a) n) ))

(defun gf-at (x n) ;; Horner and square and multiply
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (or (null x) (integerp x)) x
    (maybe-char-is-fixnum-let ((n n))
      (do ((i 0) i1) (())
        (setq i (gf-cplus-b i (cadr x)))
        (when (null (cddr x)) 
          (setq i1 (gf-cpow n (the fixnum (car x))))
          (return (gf-ctimes i i1)) )
        (setq i1 (gf-cpow n (- (the fixnum (car x)) (the fixnum (caddr x))))
              i (gf-ctimes i i1)
              x (cddr x) )))))

;; find a primitive element:
;;
(defmfun $gf_primitive () 
  (gf-data? "gf_primitive")
  (let ((*ef-arith?*)) 
    (cond
      ((null *gf-prim*) nil)
      ((equal *gf-prim* '$unknown)
        (setq *gf-prim* (gf-prim))
        (unless (null *gf-prim*) (gf-x2p *gf-prim*)) )
      (t (gf-x2p *gf-prim*)) )))

(defmfun $ef_primitive () 
  (ef-data? "ef_primitive")
  (let ((*ef-arith?* t)) 
    (cond
      ((null *ef-prim*) nil)
      ((equal *ef-prim* '$unknown)
        (cond
          ((= 1 *ef-exp*) 
            (setq *ef-prim* (let ((*ef-arith?*)) (gf-x2n *gf-prim*))) )
          (t
            (setq *ef-prim* (ef-prim)) 
            (unless (null *ef-prim*) (gf-x2p *ef-prim*)) )))
      (t (gf-x2p *ef-prim*)) )))


(defun gf-prim ()
  (*f-prim *gf-char* *gf-exp* #'gf-prim-p) )

(defun ef-prim ()
  (*f-prim *gf-card* *ef-exp* #'ef-prim-p) )

(defun *f-prim (inc e prim-p-fn)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (setq inc (min $gf_coeff_limit inc)) 
  (do ((n inc (1+ n))
       (n-lim (expt inc e))
       x )
      ((>= n n-lim) 
        (when (= $gf_coeff_limit inc) '$unknown) )
    (setq x (let ((*gf-char* inc)) (gf-n2x n)))
    (cond 
      ((= 2 (cadr x))
        (setq n (1- (* (ash n -1) inc))) ) ;; go to next monic poly
      ((funcall prim-p-fn x)
        (return x) ) )))
         

;; precomputation for *f-prim-p:
;;
(defun gf-precomp () 
  (*f-precomp (1- *gf-char*) *gf-ord* *gf-fs-ord*) ) 

(defun ef-precomp () 
  (*f-precomp (1- *gf-card*) *ef-ord* *ef-fs-ord*) ) 

(defun *f-precomp (q-1 ord fs-ord) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let (fs-q-1 
        fs-list
        ($intfaclim) )
    (setq fs-q-1 
      (sort (get-factor-list q-1) #'(lambda (a b) (< (car a) (car b)))) )        ;; .. [pi, ei] ..
    (dolist (fj fs-q-1) 
      (setq fs-ord (remove-if #'(lambda (sj) (= (car fj) (car sj))) fs-ord :count 1)) )
    (setq fs-q-1 
      (mapcar #'(lambda (pe) (list (car pe) t (truncate q-1 (car pe)))) fs-q-1) );; .. [pi, true, (p-1)/pi] ..
    (setq fs-ord 
      (mapcar #'(lambda (pe) (list (car pe) nil)) fs-ord) )                      ;; .. [pi, false] ..
    (setq fs-list 
      (merge 'list fs-q-1 fs-ord #'(lambda (a b) (< (car a) (car b)))) )
    (cond 
      (*ef-arith?*
        (setq *ef-fsx* (apply #'vector fs-list))
        (setq *ef-fsx-base-q* 
          (apply #'vector 
            (mapcar #'(lambda (pe) (nreverse (gf-n2l (truncate ord (car pe)))))  ;; qi = ord/pi = sum(aij*p^j, j,0,m)
                    fs-list) ))  
        (setq *ef-x^q-powers* (gf-x^p-powers *gf-card* *ef-exp* *ef-red*)) )     ;; x^p^j
      (t
        (setq *gf-fsx* (apply #'vector fs-list))
        (setq *gf-fsx-base-p* 
          (apply #'vector 
            (mapcar #'(lambda (pe) (nreverse (gf-n2l (truncate ord (car pe)))))  ;; qi = ord/pi = sum(aij*p^j, j,0,m)
                    fs-list) ))  
        (setq *gf-x^p-powers* (gf-x^p-powers *gf-char* *gf-exp* *gf-red*)) ))))  ;; x^p^j

;; returns an array of polynomials x^p^j, j = 0, 1, .. , (n-1), where n = *gf-exp*
;;
(defun gf-x^p-powers (q n red) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (declare (integer q) (fixnum n))
  (let ((a (make-array n :element-type 'list :initial-element nil)) )
    (setf (svref a 0) (list 1 1)) ;; x
    (do ((j 1 (1+ j)))
        ((= j n) a)
        (declare (fixnum j))
      (setf (svref a j) (gf-pow (svref a (1- j)) q red)) )))
;;
;; -----------------------------------------------------------------------------


;; Primitive polynomials -------------------------------------------------------
;;

;; test if a is a primitive polynomial over Fq

(defmfun $gf_primitive_poly_p (a &optional p) 
  (cond
    (p (unless (and (integerp p) (primep p))
         (gf-merror (intl:gettext "`gf_primitive_poly_p': ~m is not a prime number.") p) ))
    (t (gf-char? "gf_primitive_poly_p") 
       (setq p *gf-char*) ))
  (let* ((*ef-arith?*)
         (*gf-char* p) 
         (y (gf-p2x a)) ;; gf-p2x depends on *gf-char*
         (n (car y)) )
    (gf-primpoly-p y p n) ))

(defmfun $ef_primitive_poly_p (y) 
  (ef-gf-field? "ef_primitive_poly_p")
  (let ((*ef-arith?* t))
    (setq y (gf-p2x y))
    (gf-primpoly-p y *gf-card* (car y)) ))

;; based on
;; TOM HANSEN AND GARY L. MULLEN
;; PRIMITIVE POLYNOMIALS OVER FINITE FIELDS
;;
(defun gf-primpoly-p (y q n) 
  (let* ((fs-q (cfactorw q)) 
         (*gf-char* (car fs-q)) 
         (*gf-exp* (if *ef-arith?* (cadr fs-q) n)) 
         (q-1 (1- q)) 
         ($intfaclim)
         const fs-q-1 r fs-r x^r x^r/fi )
    (unless (= 1 (cadr y)) ;; monic poly assumed
      (return-from gf-primpoly-p) )
    ;; the constant part ...
    (setq const (last y 2))
    (unless (= 0 (car const)) 
      (return-from gf-primpoly-p) )
    (setq const (cadr const))
    (when (oddp n) 
      (setq const (gf-cminus-b const)) ) ;; (-1)^n*const
    ;; ... must be primitive in Fq ...
    (unless (cond 
              ((and *ef-arith?* (> *gf-exp* 1))
                (let ((*ef-arith?*)) 
                  (gf-prim-p (gf-n2x const)) ))
              (t 
                (setq fs-q-1 (sort (mapcar #'car (get-factor-list q-1)) #'<))
                (zn-primroot-p const q q-1 fs-q-1) ))
      (return-from gf-primpoly-p) )
    ;; ...  and y must be irreducible:
    (unless (gf-irr-p y q n) 
      (return-from gf-primpoly-p) )
    (when (= n 1) 
      (return-from gf-primpoly-p t))
    ;; r = (q^n-1)/(q-1), check if x^r = const:
    (setq r (truncate (1- (expt q n)) q-1)
          x^r (gf-pow (list 1 1) r y) )
    (unless (equal `(0 ,const) x^r)
      (return-from gf-primpoly-p) )
    ;; check if x^r/fi # integer for all prime factors fi of r wich do not divide q-1:
    (setq fs-r (sort (mapcar #'car (get-factor-list r)) #'<))
    (dolist (fi fs-r t)
      (when (and (if fs-q-1 
                   (not (member fi fs-q-1))
                   (/= 0 (mod q-1 fi)) )
                 (setq x^r/fi (gf-pow '(1 1) (truncate r fi) y))
                 (= 0 (car x^r/fi)) )
        (return-from gf-primpoly-p) )) ))


;; find a primitive polynomial
;;
(defmfun $gf_primitive_poly (p n) 
  (unless (and (integerp p) (primep p) (integerp n))
    (gf-merror (intl:gettext "`gf_primitive_poly' needs a prime number and an integer.")) )
  (gf-set-rat-header)
  (let ((*ef-arith?*) (*gf-char* p)) ;; gf-x2p needs *gf-char*
    (gf-x2p (gf-primpoly p n)) ))

(defmfun $ef_primitive_poly (n) 
  (ef-gf-field? "ef_primitive_poly")
  (let ((*ef-arith?* t))
    (gf-x2p (gf-primpoly *gf-card* n)) ))

(defun gf-primpoly (q n) 
  #+ (or ccl ecl gcl)  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  (let* ((fs-q (cfactorw q)) 
         (*gf-char* (car fs-q)) 
         (*gf-exp* (if *ef-arith?* (cadr fs-q) n)) 
         (q-1 (1- q)) 
         ($intfaclim)
         (fs-q-1 (sort (mapcar #'car (get-factor-list q-1)) #'<)) 
         r r-base-q fs-r fs-r-base-q )
    (when (= 1 n)
      (let ((prt (if (= q 2) 1 (zn-primroot q q-1 fs-q-1))))
        (return-from gf-primpoly 
          (list 1 1 0 (gf-cminus-b prt)) )))
    ;; pre-computation part 1:
    (setq r (truncate (1- (expt q n)) q-1) 
          r-base-q (nreverse (let ((*gf-char* q)) (gf-n2l r)))
          fs-r (sort (mapcar #'car (get-factor-list r)) #'<) )
    (dolist (fj fs-q-1) 
      (setq fs-r (delete-if #'(lambda (sj) (= fj sj)) fs-r :count 1)) )
    (setq fs-r-base-q 
      (let ((*gf-char* q)) 
        (apply #'vector 
          (mapcar #'(lambda (f) (nreverse (gf-n2l (truncate r f)))) fs-r ) )))
    ;; search:
    (let* ((inc (min $gf_coeff_limit q))
           (i-lim (expt inc n))
           x )
      (do ((i (1+ inc) (1+ i))) 
          ((>= i i-lim) 
            (gf-merror (intl:gettext "No primitive polynomial found.~%~\
                                      `gf_coeff_limit' might be too small.~%" )) )
        (setq x (let ((*gf-char* inc)) (gf-n2x i))
              x (cons n (cons 1 x)) )
        (when (gf-primpoly-p$ x *gf-char* *gf-exp* q n fs-q-1 r-base-q fs-r-base-q) 
          (return-from gf-primpoly x) )))))


;; version of gf-primpoly-p
;; that uses exponentiation by pre-computation
;;
(defun gf-primpoly-p$ (y p e q n fs-q-1 r-base-q fs-r-base-q) 
  #+ (or ccl ecl gcl)  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum e n))
  (let ((*gf-char* p)
        (*gf-exp* e) 
        (q-1 (1- q))
        const x^q-powers prod z 
        (j 0) ) (declare (fixnum j))
    (unless (= 1 (cadr y)) ;; monic poly assumed
      (return-from gf-primpoly-p$) )
    ;; the constant part ...
    (setq const (last y 2))
    (unless (= 0 (car const)) 
      (return-from gf-primpoly-p$) )
    (setq const (cadr const))
    (when (oddp n) 
      (setq const (gf-cminus-b const)) ) ;; (-1)^n*const
    ;; ... must be primitive in Fq:
    (unless (cond 
              ((and *ef-arith?* (> *gf-exp* 1))
                (let ((*ef-arith?*)) 
                  (gf-prim-p (gf-n2x const)) ))
              (t 
                (setq fs-q-1 (sort (mapcar #'car (get-factor-list q-1)) #'<))
                (zn-primroot-p const q q-1 fs-q-1) ))
      (return-from gf-primpoly-p$) )
    ;; ...  and y must be irreducible:
    (unless (gf-irr-p y q n) 
      (return-from gf-primpoly-p$) )
    (when (= n 1) 
      (return-from gf-primpoly-p$ t))
    ;; r = (q^n-1)/(q-1), check if x^r = const:
    (setq x^q-powers (gf-x^p-powers q n y)       ;; pre-computation, y dependend
          prod (list 0 1) 
          j 0 )
    (dolist (aj r-base-q)                        ;; r = sum(aj*q^j, j,0,n-1)
      (setq z (gf-pow (svref x^q-powers j) aj y) ;; (x^q^j)^aj
            prod (gf-times prod z y)  
            j (1+ j) ))
    (unless (= const (cadr prod))                ;; x^r = prod((x^q^j)^aj, j,0,n-1)
      (return-from gf-primpoly-p$) )
    ;; check if x^r/ri # integer for all prime factors ri of r wich do not divide q-1:
    (do ((i 0 (1+ i)) (dim (array-dimension fs-r-base-q 0)))
        ((= i dim) t)
        (declare (fixnum i dim))
      (setq prod (list 0 1) 
            j 0 )
      (dolist (aij (svref fs-r-base-q i))      ;; ri = sum(aij*q^j, j,0,n-1)
        (setq z (gf-pow (svref x^q-powers j) aij y) 
              prod (gf-times prod z y)  
              j (1+ j) ))
      (when (= 0 (car prod)) 
        (return-from gf-primpoly-p$) )) ))
;;
;; -----------------------------------------------------------------------------


;; random elements -------------------------------------------------------------
;;

;; Produces a random element within the given environment 

(defmfun $gf_random (&optional p n) 
  (let ((*ef-arith?* t)) 
    (cond
      (n (let ((*gf-char* p))
           (unless *gf-red?* (gf-set-rat-header))
           (gf-x2p (gf-random p n)) ))
      (t (gf-data? "gf_random") 
         (gf-x2p (gf-random *gf-char* *gf-exp*)) ))))

(defmfun $ef_random (&optional q n) 
  (let ((*ef-arith?* t)) 
    (cond
      (n (let ((*gf-char* q)) (gf-x2p (gf-random q n))))
      (t (ef-data? "ef_random") 
         (gf-x2p (gf-random *gf-card* *ef-exp*)) ))))

(defun gf-random (q n) 
  (do ((e 0 (1+ e)) c x)
      ((= e n) x)
    (setq c (random q))
    (when (/= 0 c)
      (setq x (cons e (cons c x))) )))
;;
;; -----------------------------------------------------------------------------


;; factoring -------------------------------------------------------------------
;;

(defmfun $gf_factor (a &optional p) ;; set p to switch to another modulus
  (cond
    (p (unless (and (integerp p) (primep p))
         (gf-merror (intl:gettext "`gf_factor': Second argument must be a prime number.")) )
       (gf-set-rat-header) )
    (t (gf-char? "gf_factor")
       (setq p *gf-char*) ))
  (let* ((*gf-char* p) 
         (modulus p) (a ($rat a))
         (*ef-arith?*) )
    (when (> (length (caddar a)) 1) 
      (gf-merror (intl:gettext "`gf_factor': Polynomial must be univariate.")) )
    (setq a (cadr a)) 
    (cond 
      ((integerp a) (mod a *gf-char*))
      (t (setq a 
           (if $gf_cantor_zassenhaus 
             (gf-factor (gf-mod (cdr a)) p)
             (gf-ns2pmod-factors (pfactor a)) ))
         (setq a (gf-disrep-factors a))
         (and (consp a) (consp (car a)) (equal (caar a) 'mtimes)
           (setq a (simplifya (cons '(mtimes) (cdr a)) nil)) )
         a ))))

;; adjust results from rat3d/pfactor to a positive modulus if $gf_symmetric = false 
(defun gf-ns2pmod-factors (fs) ;; modifies fs 
  (if $gf_symmetric fs
    (maybe-char-is-fixnum-let ((m *gf-char*))
      (do ((r fs (cddr r)))
          ((null r) fs)
        (if (integerp (car r))
          (when (< (the integer (car r)) 0) 
            (incf (car r) m) ) ;; only in the case *ef-arith?* = false 
          (rplaca r (gf-ns2pmod-factor (cdar r) m)) )))))

(defun gf-ns2pmod-factor (fac m)
  (do ((r (cdr fac) (cddr r))) (())
    (when (< (the integer (car r)) 0) 
      (incf (car r) m) ) 
    (when (null (cdr r)) (return fac)) ))

(defun gf-disrep-factors (fs) 
  (cond 
    ((integerp fs) (gf-cp2smod fs))
    (t 
      (setq fs (nreverse fs))
      (do ((e 0) fac p)
          ((null fs) (cons '(mtimes simp factored) p))
          (declare (fixnum e))
        (setq e (the fixnum (car fs)) 
              fac (cadr fs)
              fs (cddr fs)
              p (cond 
                  ((integerp fac) (cons (gf-cp2smod fac) p))
                  ((= 1 e) (cons (gf-disrep (gf-np2smod fac)) p))
                  (t (cons `((mexpt simp) ,(gf-disrep (gf-np2smod fac)) ,e) p)) ))))))

(defmfun $ef_factor (a)
  (ef-gf-field? "ef_factor")
  (let ((*ef-arith?* t))
    (setq a (let ((modulus)) ($rat a)))
    (when (> (length (caddar a)) 1) 
      (gf-merror (intl:gettext "`ef_factor': Polynomial must be univariate.")) )
    (setq a (cadr a)) 
    (cond 
      ((integerp a) (ef-cmod a))
      (t (setq a 
           (gf-disrep-factors 
             (gf-factor (gf-mod (cdr a)) *gf-card*) ))
         (and (consp a) (consp (car a)) (equal (caar a) 'mtimes)
           (setq a (simplifya (cons '(mtimes) (cdr a)) nil)) )
         a ))))

(defun gf-factor (x q) ;; non-integer x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((lc (cadr x)) z)
    (unless (= 1 lc) 
      (setq x (gf-xctimes x (gf-cinv lc))) ) ;; monicize x
    (if (gf-irr-p x q (car x))
      (setq z (list x 1))
      (let ((sqfr (gf-square-free x)) e y)
        (dolist (v sqfr) 
          (setq e (car v) 
                y (cadr v)
                y (gf-distinct-degree-factors y q) )
          (dolist (w y)
            (setq z (nconc (gf-equal-degree-factors w q e) z)) ))))
    (if (= 1 lc) z (cons lc (cons 1 z))) )) 

(defun gf-diff (x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (if (null x) nil
    (maybe-char-is-fixnum-let ((m *gf-char*))
      (do ((rx x (cddr rx)) res c)
          ((or (null rx) (= 0 (car rx))) (nreverse res))
        (setq c (gf-ctimes (mod (the fixnum (car rx)) m) (cadr rx))) 
        (when (/= 0 c)
          (push (1- (car rx)) res)
          (push c res) ))))  )

;; c -> c^p^(n-1) 
(defun ef-pth-croot (c)
  (let ((p *gf-char*) (*ef-arith?* t))
    (dotimes (i (1- *gf-exp*) c) 
      (setq c (gf-cpow c p)) ))) 

(defun gf-pth-root (x)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (maybe-char-is-fixnum-let ((p *gf-char*))
    (if (null x) nil
      (do ((rx x (cddr rx)) res c) 
          ((null rx) (nreverse res))
        (push (truncate (the fixnum (car rx)) p) res) 
        (setq c (cadr rx))
        (when *ef-arith?*  ;; p # q
          (setq c (ef-pth-croot c)) )
        (push c res) ))))

(defun gf-gcd-cofactors (x dx)
  (let ((g (gf-gcd x dx)))
    (values g (gf-divide x g) (gf-divide dx g)) ))

(defun gf-square-free (x) ;; monic x
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let (f fs (r (gf-diff x)) g)
    (cond 
      ((equal '(0 1) (setq g (gf-gcd x r))) `((1 ,x)))
      (t 
        (when r ;; # 0
          (setq r (gf-divide x g)
                x g ) ;; d.h. x # 1
          (do ((m 1 (1+ m)))
              ((equal '(0 1) r))
              (declare (fixnum m))
            (multiple-value-setq (r f x) (gf-gcd-cofactors r x))
            (unless (equal '(0 1) f)
              (push (list m f) fs) )))
        (unless (equal '(0 1) x)
          (setq fs 
            (append (mapcar #'(lambda (v) (rplaca v (* (car v) *gf-char*))) 
                            (gf-square-free (gf-pth-root x)) ) 
                    fs )))
        (nreverse fs) ))))

(defun gf-distinct-degree-factors (x q)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((w '(1 1)) f fs (*gf-char* (car (cfactorw q))) )
    (do ((n 1 (1+ n)))
        ((equal '(0 1) x) fs)
        (declare (fixnum n))
      (when (> (ash n 1) (car x))
        (setq fs (cons (list x (car x)) fs))
        (return) )
      (setq w (gf-nred w x)
            w (gf-pow w q x)
            f (gf-gcd (gf-plus w (gf-nminus (list 1 1))) x) )
      (unless (equal '(0 1) f)
        (setq fs (cons (list f n) fs)
              x (gf-divide x f) )))
    (nreverse fs) ))

(defun gf-nonconst-random (q q^n)
  (do (r) (())
    (setq r (random q^n))
    (when (>= r q) (return (let ((*gf-char* q)) (gf-n2x r)))) ))

;; computes Tm(x) = x^2^(m-1) + x^2^(m-2) + .. + x^4 + x^2 + x in F2[x]
;;
(defun gf-trace-poly-f2 (x m red) ;; m > 0
  (let ((tm (gf-nred x red))) 
    (do ((i 1 (1+ i)))
        ((= i m) tm)
        (declare (fixnum i))
      (setq x (gf-sq x red)
            tm (gf-plus tm x) ))))

;; Cantor and Zassenhaus' algorithm
;;
(defun gf-equal-degree-factors (x-and-d q mult)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let* ((x (car x-and-d)) (d (cadr x-and-d))
         (n (car x)) )
        (declare (fixnum d n))
    (cond
      ((= n d) (list x mult))
      (t 
        (let* ((p^k (cfactorw q)) (p (car p^k)) (k (cadr p^k)) (*gf-char* p) 
               (f '(0 1)) (q^n (expt q n)) m e r r^e )
          (if (= 2 p) 
            (setq m (* k d)) ;; q = 2^k 
            (setq e (ash (1- (expt q d)) -1)) ) 
          
          (do () ((and (not (equal '(0 1) f)) (not (equal x f))))
            (setq r (gf-nonconst-random q q^n)
                  f (gf-gcd x r) )
            (when (equal '(0 1) f)
              (setq r^e 
                (if (= 2 p) (gf-trace-poly-f2 r m x) ;; q = 2^k 
                            (gf-pow r e x) ))        ;; q is odd prime power
              (setq f (gf-gcd x (gf-nplus r^e (gf-nminus (list 0 1))))) ))
              
          (append (gf-equal-degree-factors (list (gf-divide x f) d) q mult)
                  (gf-equal-degree-factors (list f d) q mult) ))))))
;;
;; -----------------------------------------------------------------------------


;; gcd, gcdex and test of invertibility ----------------------------------------
;;

(defmfun $ef_gcd (a b) 
  (ef-gf-field? "ef_gcd")
  (let ((*ef-arith?* t))
    (gf-x2p (gf-gcd (gf-p2x a) (gf-p2x b))) ))

(defmfun $gf_gcd (a b &optional p) 
  (let ((*ef-arith?*))
    (cond
      (p (unless (and (integerp p) (primep p))
           (gf-merror (intl:gettext "`gf_gcd': ~m is not a prime number.") p) )
        (gf-set-rat-header)
        (let* ((*gf-char* p) 
               (modulus p)
               (vars (caddar ($rat a))) )
          (when (> (length vars) 1) 
            (gf-merror (intl:gettext "`gf_gcd': Polynomials must be univariate.")) )
          (gf-x2p (gf-gcd (gf-p2x a) (gf-p2x b))) ))
      (t (gf-char? "gf_gcd")
         (gf-x2p (gf-gcd (gf-p2x a) (gf-p2x b))) ))))


(defmfun $gf_gcdex (a b) 
  (gf-red? "gf_gcdex")
  (let ((*ef-arith?*))
    (cons '(mlist simp) 
      (mapcar #'gf-x2p (gf-gcdex (gf-p2x a) (gf-p2x b) *gf-red*)) )))

(defmfun $ef_gcdex (a b) 
  (ef-red? "ef_gcdex")
  (let ((*ef-arith?* t))
    (cons '(mlist simp) 
      (mapcar #'gf-x2p (gf-gcdex (gf-p2x a) (gf-p2x b) *gf-red*)) )))


(defmfun $gf_unit_p (a) 
  (gf-red? "gf_unit_p")
  (let ((*ef-arith?*))
    (gf-unit-p (gf-p2x a) *gf-red*) ))

(defmfun $ef_unit_p (a) 
  (ef-red? "ef_unit_p")
  (let ((*ef-arith?* t))
    (gf-unit-p (gf-p2x a) *ef-red*) ))

(defun gf-unit-p (x red) 
  (= 0 (car (gf-gcd x red))) )
;;
;; -----------------------------------------------------------------------------
       

;; order, degree and minimal polynomial ----------------------------------------
;;

;; group/element order

(defmfun $gf_order (&optional a) 
  (gf-data? "gf_order") 
  (cond 
    (a (let ((*ef-arith?*))
         (setq a (gf-p2x a))
         (when (and a (or *gf-irred?* (gf-unit-p a *gf-red*)))
           (gf-ord a *gf-ord* *gf-fs-ord* *gf-red*) ))) 
    (t *gf-ord*) ))

(defmfun $ef_order (&optional a) 
  (ef-data? "ef_order")
  (cond 
    (a (let ((*ef-arith?* t))
         (setq a (gf-p2x a))
         (when (and a (or *ef-irred?* (gf-unit-p a *ef-red*)))
           (gf-ord a *ef-ord* *ef-fs-ord* *ef-red*) ))) 
    (t *ef-ord*) ))

;; find the lowest value k for which a^k = 1

(defun gf-ord (x ord fs-ord red) ;; assume x # 0 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let (p (e 0)) 
       (declare (fixnum e))
    (dolist (pe fs-ord ord)
      (setq p (car pe) 
            e (the fixnum (cadr pe))
            ord (truncate ord (expt p e)) )
      (do ((z (gf-pow$ x ord red))) ;; use exponentiation by precomputation
          ((equal z '(0 1)))
        (setq z (gf-pow$ z p red) 
              ord (* ord p) ) ))))

(defun gf-ord-by-table (x) 
  (let ((index (svref $gf_logs (gf-x2n x))))
    (truncate *gf-ord* (gcd *gf-ord* index)) ))


;; Fq^n = F[x]/(f) is no field <=> f splits into factors
;;
;;   f = f1^e1 * ... * fk^ek where fi are irreducible of degree ni.
;;
;; We compute the order of the group (F[x]/(fi^ei))* by 
;;
;;   ((q^ni)^ei - (q^ni)^(ei-1)) = ((q^ni) - 1) * (q^ni)^(ei-1)
;;
;; and ord((Fq^n)*) with help of the Chinese Remainder Theorem.
;;
(defun gf-group-order (q red) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let (ne-list q^n (e 0) (ord 1)) 
       (declare (fixnum e))
    (do ((x (gf-factor red q))) ;; red is assumed to be a monic poly
        ((null x))
      (push (list (caar x) (cadr x)) ne-list) ;; (list ni ei), f = prod(fi^ei) with fi of degree ni
      (setq x (cddr x)) )
    (dolist (a ne-list)
      (setq q^n (expt q (the fixnum (car a))) 
            e   (the fixnum (cadr a))
            ord (* ord (1- q^n) (expt q^n (the fixnum (1- e)))) ))
    ord ))


;; Finds the lowest value d for which x^(q^d) = x

(defun gf-degree-errchk (a n fun)
  (when (and (not (null a)) (>= (car a) n))
    (gf-merror (intl:gettext "`~m': Leading exponent must be smaller than ~m.") fun n) ))

(defmfun $gf_degree (a) 
  (gf-field? "gf_degree")
  (let ((*ef-arith?*)) 
    (setq a (gf-p2x a))
    (gf-degree-errchk a *gf-exp* "gf_degree")
    (gf-deg a) ))

(defmfun $ef_degree (a) 
  (ef-field? "ef_degree") 
  (let ((*ef-arith?* t)) 
    (setq a (gf-p2x a))
    (gf-degree-errchk a *ef-exp* "ef_degree")
    (ef-deg a) ))

(defun gf-deg (x) ;; gf_minimal_poly also needs it
  (*f-deg x *gf-exp* *gf-red* *gf-x^p-powers*) )

(defun ef-deg (x)
  (*f-deg x *ef-exp* *ef-red* *ef-x^q-powers*) )

(defun *f-deg (x n red x^q-powers) 
  (do ((d 1 (1+ d))) 
      ((= d n) d) 
      (declare (fixnum d))
    (when (equal x (gf-compose (svref x^q-powers d) x red)) ;; f(x)^q = f(x^q)
      (return d) ) ))

;; produce the minimal polynomial 

(defmfun $gf_minimal_poly (a)
  (gf-field? "gf_minimal_poly")
  (let ((*ef-arith?*))
    (setq a (gf-p2x a))
    (gf-degree-errchk a *gf-exp* "gf_minimal_poly")
    (gf-minpoly a (gf-deg a) *gf-red* *gf-x^p-powers*) ))

(defmfun $ef_minimal_poly (a) 
  (ef-field? "ef_minimal_poly")
  (let ((*ef-arith?* t))
    (setq a (gf-p2x a))
    (gf-degree-errchk a *ef-exp* "ef_minimal_poly")
    (gf-minpoly a (ef-deg a) *ef-red* *ef-x^q-powers*) ))
;;
;;                                  2             (d-1)
;;                        q        q             q
;;   f(z) = (z - x) (z - x ) (z - x  ) ... (z - x  )   , where d = degree(x)
;;
(defun gf-minpoly (x deg red x^q-powers)
  (declare (fixnum deg))
  (if (null x) '$z
    (let ((powers (list (gf-minus x))) 
          (prod (list 0 (list 0 1)))
           zx cx )
      (do ((i 1 (1+ i)))
          ((= i deg)) (declare (fixnum i))
        (push 
          (gf-minus (gf-compose (svref x^q-powers i) x red)) 
          powers ))
      (dolist (pow powers)
        (setq zx (gf-zx prod) 
              cx (gf-ncx pow prod red) 
              prod (gf-nzx+cx zx cx)) )
      ($substitute '$z '$x (gf-x2p (gf-nxx2x prod))) )))
;;
(defun gf-zx (x) ;; (3 (5 1 3 1) 2 (4 1)) -> (4 (5 1 3 1) 3 (4 1))
                 ;;  3   5   3     2  4       4   5   3     3  4
                 ;; z  (x + x ) + z  x    -> z  (x + x ) + z  x 
  (do* ((res (list (1+ (car x)) (cadr x)))
        (r (cdr res) (cddr r)) 
        (rx (cddr x) (cddr rx)) )
       ((null rx) res)
    (rplacd r (list (1+ (car rx)) (cadr rx))) ))
;;
(defun gf-ncx (c x red) ;; modifies x
                        ;; (1 1) (3 (4 1 3 1) 2 (2 1)) (6 1)
                        ;;    -> (3 (5 1 4 1) 2 (3 1))
  (if (null c) c
    (do ((r (cdr x) (cddr r)))
        ((null r) x)
      (rplaca r (gf-times c (car r) red)) )))
;;
(defun gf-nzx+cx (zx cx) ;; modifies zx
  (do ((r (cdr zx)) 
       (s (cdr cx) (cddr s)) r+s ) 
      ((null (cdr r)) (nconc zx (list 0 (car s))))
    (setq r+s (gf-plus (caddr r) (car s)))
    (if r+s 
      (rplaca (setq r (cddr r)) r+s)
      (rplacd r (cdddr r)) ))) 
;;
(defun gf-nxx2x (xx) ;; modifies xx
                     ;; (4 (0 3) 2 (0 1)) -> (4 3 2 1)
  (do ((r (cdr xx) (cddr r)))
      ((null r) xx)
    (rplaca r (cadar r)) ))
;;
;; -----------------------------------------------------------------------------


;; trace and norm --------------------------------------------------------------
;;

;;                      2         (n-1)
;;                 q   q         q
;; trace(a) = a + a + a  + .. + a  

(defun $gf_trace (a)
  (gf-field? "gf_trace")
  (let ((*ef-arith?*))
    (gf-trace (gf-p2x a) *gf-red* *gf-x^p-powers*) ))

(defun $ef_trace (a)
  (ef-field? "ef_trace")
  (let ((*ef-arith?* t)) 
    (gf-trace (gf-p2x a) *ef-red* *ef-x^q-powers*) ))

(defun gf-trace (x red x^q-powers)
  (let ((n (car red))
        (su x) )
    (do ((i 1 (1+ i)))
        ((= i n) (gf-x2p su)) (declare (fixnum i))
      (setq su (gf-plus su (gf-compose (svref x^q-powers i) x red))) ))) 


;;                     2        (n-1)      n                         2         (n-1)
;;            1 + q + q + .. + q         (q - 1)/(q - 1)        q   q         q
;; norm(a) = a                        = a                = a * a * a  * .. * a  

(defmfun $gf_norm (a)
  (gf-field? "gf_norm")
  (let ((*ef-arith?*))
    (gf-norm (gf-p2x a) *gf-red* *gf-x^p-powers*) ))

(defmfun $ef_norm (a)
  (ef-field? "ef_norm")
  (let ((*ef-arith?* t)) 
    (gf-norm (gf-p2x a) *ef-red* *ef-x^q-powers*) ))

(defun gf-norm (x red x^q-powers)
  (let ((n (car red))
        (prod x) )
    (do ((i 1 (1+ i)))
        ((= i n) (gf-x2p prod)) (declare (fixnum i))
      (setq prod (gf-times prod (gf-compose (svref x^q-powers i) x red) red)) ))) 
;;
;; -----------------------------------------------------------------------------


;; normal elements and normal basis --------------------------------------------
;;

;; Tests if an element is normal 

(defmfun $gf_normal_p (a) 
  (gf-field? "gf_normal_p")
  (let ((*ef-arith?*)) (gf-normal-p (gf-p2x a))) )

(defun gf-normal-p (x) 
  (unless (null x) 
    (let ((modulus *gf-char*) 
          (mat (gf-maybe-normal-basis x)) )
      (equal ($rank mat) *gf-exp*) )))

(defmfun $ef_normal_p (a) 
  (ef-field? "ef_normal_p")
  (let ((*ef-arith?* t)) (ef-normal-p (gf-p2x a))) )

(defun ef-normal-p (x) 
  (unless (null x) 
    (let ((mat (gf-maybe-normal-basis x)) )
      (/= 0 ($ef_determinant mat)) )))


;; Finds a normal element e in the field; that is, 
;; an element for which the list [e, e^q, e^(q^2), ... , e^(q^(n-1))] is a basis 

(defmfun $gf_normal () 
  (gf-field? "gf_normal")
  (let ((*ef-arith?*)) 
    (gf-x2p (gf-normal *gf-char* *gf-exp* #'gf-normal-p)) ))

(defmfun $ef_normal () 
  (ef-field? "gf_normal")
  (let ((*ef-arith?* t)) 
    (gf-x2p (gf-normal *gf-card* *ef-exp* #'ef-normal-p)) ))

(defun gf-normal (q n normal-p-fn) 
  (let* ((inc (min $gf_coeff_limit q))
         (i-lim (expt inc n))
         x )
    (do ((i inc (1+ i))) 
        ((>= i i-lim) 
          (gf-merror (intl:gettext "No normal element found.~%~\
                                    `gf_coeff_limit' might be too small.~%" )) )
      (setq x (let ((*gf-char* inc)) (gf-n2x i)))
      (when (funcall normal-p-fn x ) (return-from gf-normal x)) )))


;; Finds a normal element in the field by producing random elements and checking 
;; if each one is normal 

(defmfun $gf_random_normal ()
  (gf-field? "gf_random_normal")
  (let ((*ef-arith?*)) (gf-x2p (gf-random-normal))) )
  
(defun gf-random-normal ()
  (do ((x (gf-random *gf-char* *gf-exp*) (gf-random *gf-char* *gf-exp*))) 
      ((gf-normal-p x) x) ))

(defmfun $ef_random_normal ()
  (ef-field? "ef_random_normal")
  (let ((*ef-arith?* t)) (gf-x2p (ef-random-normal))) )
  
(defun ef-random-normal ()
  (do ((x (gf-random *gf-card* *ef-exp*) (gf-random *gf-card* *ef-exp*))) 
      ((ef-normal-p x) x) ))

;; Produces a normal basis as a matrix; 
;; the columns are the coefficients of the powers e^(q^i) of the normal element 

(defmfun $gf_normal_basis (a)
  (gf-field? "gf_normal_basis")
  (let* ((*ef-arith?*) 
         (x (gf-p2x a))
         (modulus *gf-char*) 
         (mat (gf-maybe-normal-basis x)) )
    (unless (equal ($rank mat) *gf-exp*)
      (gf-merror (intl:gettext "Argument to `gf_normal_basis' must be a normal element.")) )
    mat ))

(defmfun $ef_normal_basis (a)
  (ef-field? "ef_normal_basis")
  (let* ((*ef-arith?* t)
         (mat (ef-maybe-normal-basis (gf-p2x a))) )
    (unless (/= 0 ($ef_determinant mat))
      (merror (intl:gettext "Argument to `ef_normal_basis' must be a normal element." )) )
    mat ))

(defun gf-maybe-normal-basis (x)
  (*f-maybe-normal-basis x *gf-x^p-powers* *gf-exp* *gf-red*) )

(defun ef-maybe-normal-basis (x)
  (*f-maybe-normal-basis x *ef-x^q-powers* *ef-exp* *ef-red*) )

(defun *f-maybe-normal-basis (x x^q-powers e red)
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum e))
  (let ((e-1 (- e 1))) (declare (fixnum e-1))
    ($transpose
      ($genmatrix 
        #'(lambda (i j) (declare (fixnum i j))
          (svref (gf-x2array 
                   (gf-compose (svref x^q-powers (1- i)) x red) e-1 ) (1- j)))
        e e ))))

;; coefficients as an array of designated length

(defun gf-x2array (x len) 
  #+ (or ccl ecl gcl sbcl) (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum len))
  (let ((cs (make-array (1+ len) :initial-element 0)))
    (do ((k len)) ((null x) cs) (declare (fixnum k))
      (cond 
        ((> k (the fixnum (car x))) 
          (decf k) )
        ((= k (the fixnum (car x))) 
          (setf (svref cs (- len k)) (cadr x)) 
          (setq x (cddr x)) 
          (decf k) )
        (t 
          (setq x (cddr x)) ) ))))

;; Produces the normal representation of an element as a list of coefficients 
;; Needs the inverted normal basis as the second argument 
;; (the inversion might be time consuming) 

(defmfun $gf_normal_basis_rep (a m-inv)
  (gf-field? "gf_normal_basis_rep")
  (let ((*ef-arith?*))
    (gf-normal-basis-rep (gf-p2x a) m-inv *gf-exp* '$gf_matmult) ))

(defmfun $ef_normal_basis_rep (a m-inv)
  (ef-field? "ef_normal_basis_rep")
  (let ((*ef-arith?* t))
    (gf-normal-basis-rep (gf-p2x a) m-inv *ef-exp* '$ef_matmult) ))

(defun gf-normal-basis-rep (x m-inv e matmult-fn)
  (let* ((cs (cons '(mlist simp) (gf-x2l x e))) 
         (nbrep (mfuncall matmult-fn m-inv cs)) )
    (cons '(mlist simp) (mapcar #'cadr (cdr nbrep))) ))
;;
;; -----------------------------------------------------------------------------


;; functions for matrices ------------------------------------------------------
;;

(defmfun $gf_matneg (m) 
  (gf-char? "gf_matneg")
  (mfuncall '$matrixmap '$gf_neg m) )

(defmfun $ef_matneg (m) 
  (ef-gf-field? "ef_matneg")
  (mfuncall '$matrixmap '$ef_neg m) )


;; matrix addition (convenience: mat, list or poly possible as argument)

(defmfun $gf_matadd (&rest args) 
  (let ((*ef-arith?*)) 
    (reduce #'(lambda (m1 m2) (gf-matadd m1 m2 '$gf_add)) args) ))

(defmfun $ef_matadd (&rest args) 
  (gf-data? "ef_matadd")
  (let ((*ef-arith?* t)) 
    (reduce #'(lambda (m1 m2) (gf-matadd m1 m2 '$ef_add)) args) ))

(defun gf-matadd (m1 m2 add-fn) 
  (when ($listp m1) (setq m1 ($transpose m1)))
  (when ($listp m2) (setq m2 ($transpose m2)))
  (cond 
    ((and ($matrixp m1) ($matrixp m2))
      (gf-matadd2 m1 m2 add-fn) )
    (($matrixp m1)
      (gf-matadd1 m1 m2 add-fn) ) ;; assumed without checking: m2 is poly  
    (($matrixp m2)
      (gf-matadd1 m2 m1 add-fn) )
    (t 
      (mfuncall add-fn m1 m2) ) ))

(defmfun gf-matadd1 (m poly add-fn) 
  (do ((r (cdr m) (cdr r)) new)
      ((null r) (cons '($matrix simp) (nreverse new)))
    (push (cons '(mlist simp) 
                (mapcar #'(lambda (p) (mfuncall add-fn p poly)) (cdar r)) ) 
          new )))

(defun gf-matadd2-error ()
  (gf-merror 
    (intl:gettext "Arguments to `~m' must have same formal structure.")
    (if *ef-arith?* "ef_matadd" "gf_matadd") ))

(defmfun gf-matadd2 (m1 m2 add-fn) 
  (setq m1 (cdr m1) m2 (cdr m2))
  (unless (= (length (car m1)) (length (car m2)))
    (gf-matadd2-error) )
  (do ((r1 m1 (cdr r1)) (r2 m2 (cdr r2)) new)
      ((or (null r1) (null r2)) 
        (unless (and (null r1) (null r2))
          (gf-matadd2-error) )
        (cons '($matrix simp) (nreverse new)) )
    (push (cons '(mlist simp) (mapcar add-fn (cdar r1) (cdar r2))) new) ))


;; matrix multiplication (convenience: mat, list or poly possible as argument)

(defmfun $gf_matmult (&rest args) 
  (gf-red? "gf_matmult")
  (let ((*ef-arith?*)) 
    (apply #'*f-matmult `($gf_mult ,@args)) )) 

(defmfun $ef_matmult (&rest args) 
  (ef-red? "gf_matmult")
  (let ((*ef-arith?* t)) 
    (apply #'*f-matmult `($ef_mult ,@args)) )) 

(defun *f-matmult (mult-fn &rest args) 
  ($rreduce 
    #'(lambda (m1 m2) (gf-matmult m1 m2 mult-fn)) 
    (cons '(mlist simp) args) ))

(defun gf-matmult (m1 m2 mult-fn) 
  (when ($listp m1) (setq m1 (list '($matrix simp) m1)))
  (when ($listp m2) (setq m2 ($transpose m2)))
  (cond 
    ((and ($matrixp m1) ($matrixp m2))
      (gf-matmult2 m1 m2) )
    (($matrixp m1)
      (gf-matmult1 m1 m2 mult-fn) ) ;; assumed without checking: m2 is poly 
    (($matrixp m2)
      (gf-matmult1 m2 m1 mult-fn) )
    (t 
      (mfuncall mult-fn m1 m2) ) ))

(defmfun gf-matmult1 (m poly mult-fn) 
  (do ((r (cdr m) (cdr r)) new)
      ((null r) (cons '($matrix simp) (nreverse new)))
    (push (cons '(mlist simp) 
                (mapcar #'(lambda (p) (mfuncall mult-fn p poly)) (cdar r)) ) 
          new )))

(defmfun gf-matmult2 (m1 m2) 
  (setq m1 (cdr m1) m2 (cdr ($transpose m2)))
  (unless (= (length (car m1)) (length (car m2)))
    (gf-merror 
      (intl:gettext "`~m': attempt to multiply non conformable matrices.")
      (if *ef-arith?* "ef_matmult" "gf_matmult") ))
  (let ((red (if *ef-arith?* *ef-red* *gf-red*)) )
    (do ((r1 m1 (cdr r1)) new-mat)
        ((null r1) 
          (if (and (not (eq nil $scalarmatrixp))
                   (= 1 (length new-mat)) (= 1 (length (cdar new-mat))) )
            (cadar new-mat)
            (cons '($matrix simp) (nreverse new-mat)) )) 
      (do ((r2 m2 (cdr r2)) new-row)
          ((null r2) 
            (push (cons '(mlist simp) (nreverse new-row)) new-mat) ) 
        (push (gf-x2p 
                (reduce #'gf-nplus 
                  (mapcar #'(lambda (a b) (gf-times (gf-p2x a) (gf-p2x b) red)) 
                    (cdar r1) (cdar r2) )))
              new-row ) ))))
;;
;; -----------------------------------------------------------------------------


;; interface to share/linearalgebra; some temporarily workarounds --------------
;;

;; copied from mring.lisp:

(defstruct mring
  name
  coerce-to-lisp-float
  abs
  great
  add
  div
  rdiv
  reciprocal
  mult
  sub
  negate
  psqrt
  add-id
  mult-id
  fzerop
  adjoint
  maxima-to-mring
  mring-to-maxima)

;; these ring definitions will move to mring.lisp:

(defparameter *gf-coeff-ring*
  (make-mring
   :name 'gf-coeff-ring
   :coerce-to-lisp-float nil
   :abs #'cl:identity ;; #'gf-mod
   :great #'(lambda (a b) (declare (ignore a)) (null b)) ;; #'> gives wrong results
   :add #'gf-cplus-b
   :div #'(lambda (a b) (gf-ctimes a (gf-cinv b)))
   :rdiv #'(lambda (a b) (gf-ctimes a (gf-cinv b)))
   :reciprocal #'gf-cinv
   :mult #'gf-ctimes
   :sub #'(lambda (a b) (gf-cplus-b a (gf-cminus-b b)))
   :negate #'gf-cminus-b
   :psqrt nil
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (= 0 s))
   :adjoint nil
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'cl:identity ))

(setf (get 'gf-coeff-ring 'ring) *gf-coeff-ring*)

(defparameter *gf-ring*
  (make-mring
   :name 'gf-ring
   :coerce-to-lisp-float nil
   :abs #'cl:identity ;; #'gf-mod
   ;; :great #'(lambda (a b) (> (gf-x2n a) (gf-x2n b)))
   :great #'(lambda (a b) (declare (ignore a)) (null b)) ;; ??
   :add #'gf-plus
   :div #'(lambda (a b) (gf-times a (gf-inv b *gf-red*) *gf-red*))
   :rdiv #'(lambda (a b) (gf-times a (gf-inv b *gf-red*) *gf-red*))
   :reciprocal #'(lambda (a) (gf-inv a *gf-red*))
   :mult #'(lambda (a b) (gf-times a b *gf-red*))
   :sub #'(lambda (a b) (gf-plus a (gf-minus b)))
   :negate #'gf-minus
   :psqrt nil
   :add-id #'(lambda () nil)
   :mult-id #'(lambda () '(0 1))
   :fzerop #'(lambda (s) (null s))
   :adjoint nil
   :mring-to-maxima #'gf-x2p
   :maxima-to-mring #'gf-p2x ))

(setf (get 'gf-ring 'ring) *gf-ring*)

(defparameter *ef-ring*
  (make-mring
   :name 'ef-ring
   :coerce-to-lisp-float nil
   :abs #'cl:identity ;; #'ef-mod
   ;; :great #'(lambda (a b) (> (gf-x2n a) (gf-x2n b)))
   :great #'(lambda (a b) (declare (ignore a)) (null b)) ;; ??
   :add #'gf-plus
   :div #'(lambda (a b) (gf-times a (gf-inv b *ef-red*) *ef-red*))
   :rdiv #'(lambda (a b) (gf-times a (gf-inv b *ef-red*) *ef-red*))
   :reciprocal #'(lambda (a) (gf-inv a *ef-red*))
   :mult #'(lambda (a b) (gf-times a b *ef-red*))
   :sub #'(lambda (a b) (gf-plus a (gf-minus b)))
   :negate #'gf-minus
   :psqrt nil
   :add-id #'(lambda () nil)
   :mult-id #'(lambda () '(0 1))
   :fzerop #'(lambda (s) (null s))
   :adjoint nil
   :mring-to-maxima #'gf-x2p
   :maxima-to-mring #'gf-p2x ))

(setf (get 'ef-ring 'ring) *ef-ring*)

;; workarounds: calling Lisp functions in lu.lisp:

(defun *f-invert-by-lu (m field)
  (let ((id (mfuncall '$identfor m)) ;; autoloads linearalgebra
        (fs (*f-lu-factor m field)) )
    (when fs (mfuncall '$lu_backsub fs id)) ))

(defun *f-determinant-by-lu (m field)
  (mfuncall '$zeromatrixp nil) ;; autoloads linearalgebra
  (let ((fs (*f-lu-factor m field)))
    (when fs (mfuncall 'determinant-by-lu-factors fs field)) ))

;; returns nil if lu-factor fails
(defun *f-lu-factor (m field)
  (let (c perm ma fs)
    (setq c ($length m))
    (setq perm (make-array c))
    (decf c)
    (loop for i from 0 to c do (setf (aref perm i) i)) 
    (setq ma (mfuncall 'maxima-to-array m (mring-maxima-to-mring field)))
    (unwind-protect                      ;; protect against lu-factor failure
      (let* ((old-out *standard-output*) ;; discard error messages from lu-factor
             (redirect (make-string-output-stream))
             (*standard-output* redirect) )
        (setq fs (mfuncall 'lu-factor ma perm c field 0.0))
        (setf *standard-output* old-out)
        (close redirect)
        fs )
      (unless fs (return-from *f-lu-factor nil)) )))
;;
;; -----------------------------------------------------------------------------


;; invert and determinant by lu ------------------------------------------------
;;

(defun zn-p-errchk (p fun pos)
  (unless (and p (integerp p) (primep p))
    (gf-merror (intl:gettext "`~m': ~m argument must be prime number.") fun pos) ))

;; invert by lu

(defmfun $zn_invert_by_lu (m p) 
  (zn-p-errchk p "zn_invert_by_lu" "Second")
  (let ((*ef-arith?*) (*gf-char* p)) (*f-invert-by-lu m *gf-coeff-ring*)) )

(defmfun $gf_matinv (m) 
  (format t "`gf_matinv' is deprecated. ~%~\
             The user is asked to use `gf_invert_by_lu' instead.~%" )
  ($gf_invert_by_lu m) )

(defmfun $gf_invert_by_lu (m) 
  (gf-field? "gf_invert_by_lu")
  (let ((*ef-arith?*)) (*f-invert-by-lu m *gf-ring*)) )

(defmfun $ef_invert_by_lu (m) 
  (ef-field? "ef_invert_by_lu")
  (let ((*ef-arith?* t)) (*f-invert-by-lu m *ef-ring*)) )

;; determinant

(defmfun $zn_determinant (m p) 
  (zn-p-errchk p "zn_determinant" "Second")
  (let* ((*ef-arith?*) 
         (*gf-char* p)
         (det (*f-determinant-by-lu m *gf-coeff-ring*)) ) ;; try LU-decomposition first
    (if det det
      (mod (mfuncall '$determinant m) p) )))
 
(defmfun $gf_determinant (m) 
  (gf-field? "gf_determinant")
  (let* ((*ef-arith?*)
         (det (*f-determinant-by-lu m *gf-ring*)) )
    (if det det
      (let (($matrix_element_mult '$gf_mult) ($matrix_element_add '$gf_add))
        (mfuncall '$determinant m) ))))

(defmfun $ef_determinant (m) 
  (ef-field? "ef_determinant")
  (let* ((*ef-arith?* t)
         (det (*f-determinant-by-lu m *ef-ring*)) )
    (if det det
      (let (($matrix_element_mult '$ef_mult) ($matrix_element_add '$ef_add))
        (mfuncall '$determinant m) ))))
;;
;; -----------------------------------------------------------------------------


;; discrete logarithm ----------------------------------------------------------
;;

;; solve g^x = a in Fq^n, where g a generator of (Fq^n)*

(defmfun $gf_index (a) 
  (gf-data? "gf_index")
  (gf-log-errchk1 *gf-prim* "gf_index")
  (let ((*ef-arith?*)) 
    (if (= 1 *gf-exp*)
      ($zn_log a (gf-x2n *gf-prim*) *gf-char*)
      (gf-dlog (gf-p2x a)) )))

(defmfun $ef_index (a) 
  (ef-data? "ef_index")
  (gf-log-errchk1 *ef-prim* "ef_index")
  (let ((*ef-arith?* t)) 
    (setq a (gf-p2x a))
    (if (= 1 *ef-exp*)
      (let ((*ef-arith?*)) (gf-dlog (gf-n2x (cadr a))))
      (ef-dlog a) )))

(defmfun $gf_log (a &optional b) 
  (gf-data? "gf_log")
  (gf-log-errchk1 *gf-prim* "gf_log")
  (let ((*ef-arith?*))
    (cond 
      ((= 1 *gf-exp*)
        ($zn_log a (if b b (gf-x2n *gf-prim*)) *gf-char*) ) ;; $zn_log checks if b is primitive
      (t
        (setq a (gf-p2x a))
        (and b (setq b (gf-p2x b)) (gf-log-errchk2 b #'gf-prim-p "gf_log"))
        (if b
          (gf-dlogb a b)
          (gf-dlog a) )))))

(defun gf-log-errchk1 (prim fun)
  (when (null prim)
    (gf-merror (intl:gettext "`~m': there is no primitive element.") fun) )
  (when (equal prim '$unknown)
    (gf-merror (intl:gettext "`~m': a primitive element is not known.") fun) ))

(defun gf-log-errchk2 (x prim-p-fn fun)
  (unless (funcall prim-p-fn x)
    (gf-merror (intl:gettext 
      "Second argument to `~m' must be a primitive element." ) fun )))

(defmfun $ef_log (a &optional b) 
  (ef-data? "ef_log")
  (gf-log-errchk1 *ef-prim* "ef_log")
  (let ((*ef-arith?* t))
    (setq a (gf-p2x a))
    (and b (setq b (gf-p2x b)) (gf-log-errchk2 b #'ef-prim-p "ef_log")) )
  (cond 
    ((= 1 *ef-exp*)
      (let ((*ef-arith?*)) 
        (setq a (gf-n2x (cadr a)))
        (if b   
          (gf-dlogb a (gf-n2x (cadr b)))
          (gf-dlog a) )))
    (t
      (let ((*ef-arith?* t))
        (if b
          (ef-dlogb a b)
          (ef-dlog a) )))))

(defun gf-dlogb (a b) 
  (*f-dlogb a b #'gf-dlog *gf-ord*) )

(defun ef-dlogb (a b) 
  (*f-dlogb a b #'ef-dlog *ef-ord*) )

(defun *f-dlogb (a b dlog-fn ord) 
  (let* ((a-ind (funcall dlog-fn a)) (b-ind (funcall dlog-fn b))
         (d (gcd (gcd a-ind b-ind) ord))
         (m (truncate ord d)) )
    (mod (* (inv-mod (truncate b-ind d) m) (truncate a-ind d)) m) ))

;; Pohlig and Hellman reduction

(defun gf-dlog (a)
  (*f-dlog a *gf-prim* *gf-red* *gf-ord* *gf-fs-ord*) )

(defun ef-dlog (a)
  (*f-dlog a *ef-prim* *ef-red* *ef-ord* *ef-fs-ord*) )

(defun *f-dlog (a g red ord fs-ord)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond
    ((or (null a) (null g)) nil) 
    ((>= (car a) (car red)) nil)
    ((equal '(0 1) a) 0)
    ((equal g a) 1)
    ((not (gf-unit-p a red)) nil)
    (t 
      (let (p (e 0) ord/p gg x dlog dlogs tmp) 
           (declare (fixnum e))
        (dolist (f fs-ord)
          (setq p (car f) e (cadr f)
                ord/p (truncate ord p)
                gg (gf-pow g ord/p red) ) ;; gg is generator of order p
          (cond 
            ((= 1 e) 
              (setq x (gf-dlog-rho-brent (gf-pow a ord/p red) gg p red)) )
            (t
              (setq x 0)
              (do ((aa a) (k 1) (pk 1)) (()) (declare (fixnum k))
                (setq tmp (gf-pow aa (truncate ord/p pk) red) 
                      dlog (gf-dlog-rho-brent tmp gg p red) 
                      x (+ x (* dlog pk)) )
                (if (= k e) 
                  (return)
                  (setq k (1+ k) pk (* pk p)) )
                (setq tmp (gf-inv (gf-pow g x red) red)) 
                (setq aa (gf-times a tmp red)) ))) 
          (setq dlogs (cons x dlogs)) )
        (car (chinese (nreverse dlogs) 
                    (mapcar #'(lambda (z) (apply #'expt z)) fs-ord) )) ))))

;; iteration for Pollard rho:  b = g^y * a^z in each step

(defun gf-dlog-f (b y z a g p red)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((m (mod (cadr b) 3))) (declare (fixnum m))
    (cond 
      ((= 0 m) 
        (values (gf-sq b red)      (mod (ash y 1) p) (mod (ash z 1) p)) )
      ((= 1 m) ;; avoid stationary case b=1 => b^2=1 
        (values (gf-times g b red) (mod (+ y 1) p)   z                ) )
      (t
        (values (gf-times a b red) y                 (mod (+ z 1) p)  ) ) )))

;; brute-force:

(defun gf-dlog-naive (a g red)
  (do ((i 0 (1+ i)) 
       (gi '(0 1) (gf-times gi g red)) )
      ((equal gi a) i) ))

;; baby-steps-giant-steps:

(defun gf-dlog-baby-giant (a g p red) ;; g is generator of order prime p
  (let* ((m (1+ (isqrt p)))
         (s (floor (* 1.3 m)))
         (gi (gf-inv g red)) d babies )
    (setf babies 
      (make-hash-table :size s :test #'equal :rehash-threshold 0.9) )
    (do ((r 0 (1+ r)) b)
        ((= r m))
      (setq b (gf-times a (gf-pow gi r red) red))
      (when (equal '(0 1) b)
        (clrhash babies)
        (return-from gf-dlog-baby-giant r) )
      (setf (gethash b babies) r) )
    (setq d (gf-pow g m red))
    (do ((rr 0 (1+ rr)) bb r) 
        ((= rr m))
      (setq bb (gf-pow d rr red))
      (when (setq r (gethash bb babies))
        (clrhash babies)
        (return-from gf-dlog-baby-giant (+ (* rr m) r)) )) ))

;; Pollard rho for dlog computation (Brents variant of collision detection)

(defun gf-dlog-rho-brent (a g p red) ;; g is generator of order p
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (cond
    ((equal '(0 1) a) 0)
    ((equal g a) 1)
    ((equal a (gf-sq g red)) 2) 
    ((equal '(0 1) (gf-times a g red)) (1- p)) 
    ((< p 32) (gf-dlog-naive a g red))
    ((< p 1024) (gf-dlog-baby-giant a g p red))
    (t
      (prog ((b (list 0 1)) (y 0) (z 0)    ;; b = g^y * a^z
             (bb (list 0 1)) (yy 0) (zz 0) ;; bb = g^yy * a^zz
             dy dz )
        rho
        (do ((i 0)(j 1)) (())
            (declare (fixnum i j)) 
          (multiple-value-setq (b y z) (gf-dlog-f b y z a g p red))
          (when (equal b bb) (return))     ;; g^y * a^z = g^yy * a^zz
          (incf i)
          (when (= i j)
            (setq j (1+ (ash j 1)))
            (setq bb b yy y zz z) ))
        (setq dy (mod (- yy y) p) dz (mod (- z zz) p)) ;; g^dy = a^dz = g^(x*dz)
        (when (= 1 (gcd dz p))
          (return (mod (* dy (inv-mod dz p)) p)) )     ;; x = dy/dz mod p (since g is generator of order p)
        (setq y 0
              z 0
              b (list 0 1)
              yy (1+ (random (1- p)))
              zz (1+ (random (1- p)))
              bb (gf-times (gf-pow g yy red) (gf-pow a zz red) red) )
        (go rho) ))))
;;
;; -----------------------------------------------------------------------------


;; tables of small fields ------------------------------------------------------
;;

(defmfun $gf_add_table ()
  (gf-data? "gf_add_table")
  (let ((*ef-arith?*)) (gf-add-table *gf-card*)) )

(defmfun $ef_add_table ()
  (ef-data? "ef_add_table")
  (let ((*ef-arith?* t)) (gf-add-table *ef-card*)) )

(defun gf-add-table (card)
  ($genmatrix  
    #'(lambda (i j) 
      (gf-x2n (gf-plus (gf-n2x (1- i)) (gf-n2x (1- j)))) ) 
    card 
    card ))

(defmfun $gf_mult_table (&optional all?)
  (gf-data? "gf_mult_table")
  (let ((*ef-arith?*))
    (gf-mult-table *gf-red* *gf-irred?* *gf-card* all?) ))

(defmfun $ef_mult_table (&optional all?)
  (ef-data? "ef_mult_table")
  (let ((*ef-arith?* t))
    (gf-mult-table *ef-red* *ef-irred?* *ef-card* all?) ))

(defun gf-mult-table (red irred? card all?)
  (let (units res)
    (cond
      ((or irred? ;; field
            (equal all? '$all) )
        ($genmatrix  
           #'(lambda (i j) 
             (gf-x2n (gf-times (gf-n2x i) (gf-n2x j) red))) 
           (1- card) 
           (1- card) ))
      (t ;; units only
        (do ((i 1 (1+ i)) x )
            ((= i card) )
          (setq x (gf-n2x i))
          (when (gf-unit-p x red) (push x units)) )
        (dolist (x units (cons '($matrix simp) (nreverse res)))
          (push 
            (cons '(mlist simp) 
              (mapcar #'(lambda (y) (gf-x2n (gf-times x y red))) units) )
            res ) )) )))

(defmfun $gf_power_table (&optional all?)
  (gf-data? "gf_power_table")
  (let ((*ef-arith?*))
    (gf-power-table *gf-red* *gf-irred?* *gf-card* *gf-ord* all? ) ))

(defmfun $ef_power_table (&optional all?)
  (ef-data? "ef_power_table")
  (let ((*ef-arith?* t))
    (gf-power-table *ef-red* *ef-irred?* *ef-card* *ef-ord* all? ) ))

(defun gf-power-table (red irred? card ord all?)
  (cond
    ((or irred? ;; field
         (equal all? '$all) )
      (when (equal all? '$all) (incf ord))
      ($genmatrix  
         #'(lambda (i j) 
           (gf-x2n (gf-pow (gf-n2x i) j red) ))
         (1- card) 
         ord ))
    (t ;; units only
      (do ((i 1 (1+ i)) x res) 
          ((= i card) (cons '($matrix simp) (nreverse res)))
        (setq x (gf-n2x i)) 
        (when (gf-unit-p x red)
          (push 
            (cons '(mlist simp) 
              (mapcar #'(lambda (j) (gf-x2n (gf-pow x j red)))
                (cdr (mfuncall '$makelist '$j '$j 1 ord)) ))
            res ) )) )))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
