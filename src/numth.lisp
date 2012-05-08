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
	  (cond ((= n 1) 1)
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
  (cond ((integerp n)
	 (setq n (abs n))
	 (cond ((< n 1) 0)
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
  (cond ((not (= (rem p 4) 1)) nil)
	((= (rem p 8) 5) (imodp1 2 p))
	((= (rem p 24) 17) (imodp1 3 p)) ;p=2(mod 3)
	(t (do ((i 5 (+ i (car j)))	;p=1(mod 24)
		(j *incl* (cdr j)))
	       ((= (jacobi i p) -1) (imodp1 i p))))))

(defun imodp1 (i modulus)
  (abs (cexpt i (ash (1- modulus) -2) )))

(defun psumsq (p)
  (let ((x (imodp p)))
    (cond ((equal p 2) (list 1 1))
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
	     (cond ((null res) 1)
		   ((null (cdr res)) (car res))
		   (t (cons '(mtimes simp) (nreverse res)))))
	  (let ((term (car factors))
		(exp (cadr factors)))
	    (push (if (= exp 1)
		      (gcdisp term)
		      (pow (gcdisp term) exp))
		  res))))))

(defun gcdisp (term)
  (cond ((atom term) term)
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
	    (setq gl (cddr gl)) (go loop))
	   ((equal p (car nl))
	    (cond ((zerop (rem (setq tem (+ (* a (car cd)) ;gcremainder
					    (* b (cadr cd))))
			       p))		;remainder(real((a+bi)cd~),p)
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
     ret    (setq cd (gcexpt (list 0 -1)
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
;; Maxima functions: 
;; zn_order, zn_primroot_p, zn_primroot, zn_log, chinese
;;
;; 2012, Volker van Nek  
;;

;; Maxima option variables:
(defmvar $zn_primroot_limit 1000 "Upper bound for `zn_primroot'." fixnum)
(defmvar $zn_primroot_verbose nil "Print message when `zn_primroot_limit' is reached." boolean)
(defmvar $zn_primroot_pretest nil "`zn_primroot' performs pretest if (Z/nZ)* is cyclic." boolean)


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
           (merror (intl:gettext 
             "Third argument to `zn_order' must be of the form [[p1, e1], ..., [pk, ek]].")) )
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
      (setq s (/ s (expt p e)))
      (do ((z (power-mod x s n)))
          ((= z 1))
        (setq z (power-mod z p n))
        (setq s (* s p)) )) ))


;; compute totient (euler-phi) of n and its factors in one function
;;
;; returns a list of the form (phi ((p1 e1) ... (pk ek)))
;;
(defun totient-with-factors (n)
  (let (($factors_only) ($intfaclim) (phi 1) fs-n (fs) p e (fs-phi) g)
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
    ((= 1 x) (if (= n 2) 1 nil))
    ((<= n 2) nil)
    ((= 0 (mod x n)) nil)
    (t 
      (if fs-phi
         (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
           (progn 
             (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
             (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
           (merror (intl:gettext 
             "Third argument to `zn_primroot_p' must be of the form [[p1, e1], ..., [pk, ek]].")) )
         (setq fs-phi (totient-with-factors n)) )
      (zn-primroot-p x 
                     n
                     (car fs-phi) ;; phi
                     (mapcar #'car (cdr fs-phi))) ))) ;; factors only (omitting multiplicity)
;;
(defun zn-primroot-p (x n phi fs-phi)
  (unless (= 1 (gcd x n))
    (return-from zn-primroot-p nil) )  
  (dolist (p fs-phi t)
    (when (= 1 (power-mod x (/ phi p) n))
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
           (merror (intl:gettext
             "Second argument to `zn_primroot' must be of the form [[p1, e1], ..., [pk, ek]].")) )
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
          (setq n (/ n q))
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
      (merror (intl:gettext
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
    (t 
      (if fs-phi
        (if (and ($listp fs-phi) ($listp (cadr fs-phi)))
          (progn 
            (setq fs-phi (mapcar #'cdr (cdr fs-phi))) ; Lispify fs-phi
            (setq fs-phi (cons (totient-from-factors fs-phi) fs-phi)) )
          (merror (intl:gettext
             "Fourth argument to `zn_log' must be of the form [[p1, e1], ..., [pk, ek]].")) )
        (setq fs-phi (totient-with-factors n)) )
      (unless (zn-primroot-p g n (car fs-phi) (mapcar #'car (cdr fs-phi)))
        (merror (intl:gettext "Second argument to `zn_log' must be a generator of (Z/~MZ)*.") n) )
      (when (= 0 (mod (- a (* g g)) n)) 
        (return-from $zn_log 2) )
      (when (= 1 (mod (* a g) n))
        (return-from $zn_log (mod -1 (car fs-phi))) )
      (zn-dlog a 
               g 
               n 
               (car fs-phi) ;; phi
               (cdr fs-phi)) ))) ;; factors with multiplicity
;;
;; Pohlig and Hellman reduction:
(defun zn-dlog (a g n phi fs-phi)
  (let (p e phip gp x dlog (dlogs nil))
    (dolist (f fs-phi)
      (setq p (car f) e (cadr f))
      (setq phip (/ phi p))
      (setq gp (power-mod g phip n))
      (if (= 1 e) 
        (setq x (dlog-rho (power-mod a phip n) gp p n))
        (progn 
          (setq x 0)
          (do ((agx a) (k 1) (pk 1)) (())
            (setq dlog (dlog-rho (power-mod agx (/ phip pk) n) gp p n))
            (setq x (+ x (* dlog pk)))
            (if (= k e) 
              (return)
              (setq k (1+ k) pk (* pk p)) )
            (setq agx (mod (* a ($power_mod g (- x) n)) n)) )))
      (setq dlogs (cons x dlogs)) )
    (car (chinese (reverse dlogs) (mapcar #'(lambda (z) (apply #'expt z)) fs-phi))) ))
;;
;; brute-force:
(defun dlog-naive (a g q n)
  (decf q)
  (do ((i 0 (1+ i)) (gi 1 (mod (* gi g) n)))
      ((= gi a) i) ))
;;
;; Pollard rho for dlog computation:
(defun dlog-rho (a g q n)  
  (cond
    ((= 1 a) 0)
    ((= g a) 1)
    ((= 0 (mod (- a (* g g)) n)) 2)
    ((= 1 (mod (* a g) n)) (1- q))
    ((< q 512) (dlog-naive a g q n))
    (t
      (let (rnd (b 1) (y 0) (z 0) (bb 1) (yy 0) (zz 0) dy dz)
        (dotimes (i 32 (progn (print "pollard-rho failed.") nil))
          (do () (())
            (multiple-value-setq (b y z) (dlog-f b y z a g q n))
            (multiple-value-setq (bb yy zz) (dlog-f bb yy zz a g q n))
            (multiple-value-setq (bb yy zz) (dlog-f bb yy zz a g q n))
            (when (= b bb) (return)) )
          (setq dy (mod (- y yy) q) dz (mod (- zz z) q))
          (when (= 1 (gcd dz q))
            (return (mod (* dy (inv-mod dz q)) q)) )
          (setq rnd (1+ (random (1- q))))
          (multiple-value-setq (b y z) 
            (values (mod (* a (power-mod g rnd n)) n) rnd 1) )
          (multiple-value-setq (bb yy zz) (values b y z)) )))))
;;
;; iteration for Pollard rho:
(defun dlog-f (b y z a g q n)
  (let ((s (mod b 3)))
    (cond 
      ((= 0 s)
        (values (mod (* b b) n) (mod (ash y 1) q) (mod (ash z 1) q)) )
      ((= 1 s)
        (values (mod (* a b) n) y                 (mod (+ z 1) q)) )
      (t
        (values (mod (* g b) n) (mod (+ y 1) q)   z) ))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
