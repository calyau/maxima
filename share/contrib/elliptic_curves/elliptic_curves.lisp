#|
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.
   

   *** Elliptic Curves *********************************************************
   
   Copyright Volker van Nek, 2015
   
   This file contains some algorithms for elliptic curves over prime fields.

   Maxima functions:
   ec_set_curve, ec_add, ec_mult, ec_random, ec_point_p, ec_point_order, 
   ec_log, ec_twist_curve, ec_trace
   
   Option variables:
   ec_balanced, ec_sea_verbose
   
   (The SEA-implementation also contains test functions at Maxima level.)
   
   The computation of the trace takes some time for characteristics larger than 
   112 bit and the user should consider to compile this file. The database of 
   modular polynomials is limited and for parameters larger than 256 bit it is 
   recommended to use pari/gp.

   Example session:

   (%i1) load("elliptic_curves")$
   
   (%i2) p : 4451685225093714772084598273548427$
   (%i3) a : -3$
   (%i4) b : 2061118396808653202902996166388514$
   (%i5) ec_set_curve(p,a,b)$
   
   This is curve secp112r1. 
   
   (%i6) ord : p + 1 - ec_trace();
   (%o6)                 4451685225093714776491891542548933
   (%i7) primep(ord);
   (%o7)                                true
   
   The order is a prime number and every random curve point is a generator.

   (%i8) pt : ec_random()$
   (%i9) is(ec_point_order(pt, ord) = ord);
   (%o9)                                true
   
   The point at infinity is ec_inf.

   (%i10) ec_mult(ord, pt);
   (%o10)                              ec_inf
|#


(declare-top (special 
  *ec-a* *ec-b* *ec-p* *ec-inv2* *ec-set?* 
  *ec-g* *ec-g1* *ec-sea-data?*
  *ec-psi* *ec-psi-i* *ec-psi-red* *ec-psi2* *ec-psi3* 
  *ec-poly* *ec-poly2* 
  *gf-char* *ef-arith?* ))

(defmvar $ec_balanced nil "point coordinates are balanced?")
(defmvar $ec_sea_verbose nil "print splitting type and traces")

;;
;;
;; define a curve over a prime field (as global variables)
;;
(defmfun $ec_set_curve (p a b)
  (let ((fn "ec_set_curve"))
    (unless (and (integerp p) (integerp a) (integerp b))
      (gf-merror (intl:gettext "`~m': Arguments must be a integers.") fn) )
    (unless (and (primep p) (> p 3))
      (gf-merror (intl:gettext
        "`~m': First argument must be a prime number greater than 3. Found ~m." ) fn p) )
    (if (= 0 (mod (+ (* 4 a a a) (* 27 b b)) p))
      (setq *ec-set?* nil)
      (setq *ec-p* p
            *ec-a* (mod a p)
            *ec-b* (mod b p)
            *ec-inv2* (inv-mod 2 p)
            *ec-set?* t ))))
;;
(defun ec-set? (fun)
  (if *ec-set?* t
    (gf-merror (intl:gettext "`~m': The curve is not defined yet.") fun) ))
;;
;;
;; convert between Maxima and Lisp level
;;
(defun ec-m2l (pt_m fn)
  (let (pt)
    (cond
      ((equal pt_m '$ec_inf) nil) ;; neutral element, point of infinity (nil at Lisp level)
      ((not (and ($listp pt_m)
                 (setq pt (cdr pt_m))
                 (every #'integerp pt) ))
        (gf-merror (intl:gettext "`~m': Unsuitable argument: ~m") fn pt_m) )
      (t 
        (mapcar #'(lambda (n) (mod n *ec-p*)) pt) ))))
;;
(defun ec-l2m (pt)
  (if pt
    (cons '(mlist simp) (ec-balance pt))
    '$ec_inf ))
;;
(defun ec-balance (pt)
  (if $ec_balanced
    (mapcar #'(lambda (n) (if (< (ash *ec-p* -1) n) (- n *ec-p*) n)) pt)
    pt ))
;;
;;
;; point addition (use affine coords at Maxima level, projective at Lisp level)
;;
(defmfun $ec_add (pt qt)
  (let ((fn "ec_add"))
    (ec-set? fn)
    (setq pt (ec-projectify (ec-m2l pt fn))
          qt (ec-projectify (ec-m2l qt fn)) )
    (ec-l2m (ec-affinify (ec-padd pt qt))) )) 
;;
;;
;; point multiplication
;;
(defmfun $ec_mult (n pt)
  (let ((fn "ec_mult"))
    (ec-set? fn)
    (unless (integerp n) 
      (gf-merror (intl:gettext "`~m': First argument must be an integer.") fn) )
    (setq pt (ec-projectify (ec-m2l pt fn)))
    (ec-l2m (ec-affinify (ec-pmult n pt))) ))
;;

;; -------------------------------------------------------------------------- ;;
;;
;; arithmetic in affine coordinates
;;
(defun ec-add (pt qt)
  (cond 
    ((null pt) qt)
    ((null qt) pt)
    ((equal pt qt) (ec-double pt))
    ((eql (car pt) (car qt)) nil) ;; erroneous if pt or qt is not on the curve
    (t (let* ((p *ec-p*)
              (xp (car pt)) (yp (cadr pt))
              (xq (car qt)) (yq (cadr qt))
              (m (zn-quo (- yq yp) (- xq xp) p))              
              (xx (mod (- (* m m) xq xp) p))
              (yy (mod (- (* m (- xp xx)) yp) p)) )
        (list xx yy) ))))
;;
(defun ec-double (pt)
  (cond 
    ((null pt) nil)
    ((= 0 (cadr pt)) nil)
    (t (let* ((p *ec-p*)
              (x (car pt)) (y (cadr pt))
              (m (zn-quo (+ (* 3 x x) *ec-a*) (ash y 1) p))              
              (xx (mod (- (* m m) (ash x 1)) p))
              (yy (mod (- (* m (- x xx)) y) p)) )
        (list xx yy) ))))
;;
(defun ec-mult (n pt)
  (cond 
    ((or (= 0 n) (null pt)) nil)
    ((and (< n 0) 
          (setq n (- n) pt (ec-neg pt)) 
          nil ))
    (t
      (do ((res)) (())
        (when (oddp n)
          (setq res (ec-add pt res))
          (when (= 1 n) (return res)) )
        (setq n (ash n -1)
              pt (ec-double pt) )) )))
;;
(defun ec-neg (pt) 
  (when pt (list (car pt) (- *ec-p* (cadr pt)))) )
;;
;;
;; arithmetic in projective coordinates ------------------------------------- ;;
;;
(defun ec-projectify (pt) 
  (if pt
    (nconc pt (list 1))
    (list 1 1 0) ))
;;
(defun ec-affinify (pt)  
  (unless (= (caddr pt) 0)
    (let* ((p *ec-p*)
           (i (inv-mod (caddr pt) p))
           (ii (* i i))
           (x (mod (* ii (car pt)) p))
           (y (mod (* ii i (cadr pt)) p)) )
      (list x y) )))
;;
(defun ec-pdouble (pt)  
  (cond 
    ((= (caddr pt) 0) pt)
    (t (let* ((p *ec-p*)
              (a *ec-a*)
              (x1 (car pt)) (y1 (cadr pt)) (z1 (caddr pt))
              (y11 (* y1 y1))
              (z11 (* z1 z1))
              (l1 (if (= a -3) ;; NIST curves have a = -3
                    (* 3 (- x1 z11) (+ x1 z11))
                    (+ (* 3 x1 x1) (* a z11 z11)) ))
              (z3 (ash (* y1 z1) 1))
              (l2 (ash (* x1 y11) 2))
              (x3 (- (* l1 l1) (ash l2 1)))
              (l3 (ash (* y11 y11) 3))
              (y3 (- (* l1 (- l2 x3)) l3)) )
      (list (mod x3 p) (mod y3 p) (mod z3 p)) ))))
;;
(defun ec-padd (pt qt)  
  (cond 
    ((= (caddr pt) 0) qt)
    ((= (caddr qt) 0) pt)
    (t (let* ((p *ec-p*)
              (x1 (car pt)) (y1 (cadr pt)) (z1 (caddr pt))
              (x2 (car qt)) (y2 (cadr qt)) (z2 (caddr qt))
              (z11 (* z1 z1))
              (z22 (* z2 z2))
              (l1 (* x1 z22))
              (l2 (* x2 z11))
              (l3 (mod (- l1 l2) p))
              (l4 (* y1 z22 z2))
              (l5 (* y2 z11 z1))
              (l6 (mod (- l4 l5) p)) )
         (if (= l3 0)
           (if (= l6 0)
             (ec-pdouble pt) 
             (list 1 1 0) )
           (let* ((l7 (+ l1 l2))
                  (l8 (+ l4 l5))
                  (z3 (mod (* z1 z2 l3) p))
                  (l33 (* l3 l3))
                  (l733 (* l7 l33))
                  (x3 (mod (- (* l6 l6) l733) p))
                  (l9 (- l733 (ash x3 1)))
                  (y3 (mod (* *ec-inv2* (- (* l9 l6) (* l8 l33 l3))) p)) )
             (list x3 y3 z3) ))))))
;;
(defun ec-pmult (n pt)
  (cond 
    ((or (= 0 n) (= (caddr pt) 0)) (list 1 1 0))
    ((and (< n 0) 
          (setq n (- n) pt (ec-pneg pt)) 
          nil ))
    ((typep n 'fixnum)
      (do ((res (list 1 1 0))) (())
        (when (oddp n)
          (setq res (ec-padd pt res)) 
          (when (= 1 n) (return res)) )
        (setq n (ash n -1)
              pt (ec-pdouble pt) )) )
    (t 
      (ec-pmult-sliding-window n pt) )))
;;
(defun ec-pneg (pt) 
  (when pt (list (car pt) (- *ec-p* (cadr pt)) (caddr pt))) )
;;
(defun ec-pmult-sliding-window (n pt)
  (let* ((l (integer-length n))
         (k (cond ((<= l  64) 3)
                  ((<= l 160) 4)
                  ((<= l 384) 5)
                  ((<= l 896) 6)
                  (t          7) ))
         (tab (ec-pmult-table pt k))
         (i (1- l)) (s 0) (h 0)
         (res (list 1 1 0))
          u )
    (do () ((< i 0) res)
      (cond
        ((logbitp i n)
          (setq s (max (1+ (- i k)) 0))
          (do () ((logbitp s n)) (incf s))
          (setq h (1+ (- i s)))
          (dotimes (j h) (setq res (ec-pdouble res)))
          (setq u (ldb (byte h s) n))
          (unless (= u 0) (setq res (ec-padd res (svref tab (ash u -1)))))
          (setq i (1- s)) )
        (t
          (setq res (ec-pdouble res))
          (decf i) )))))
;;
(defun ec-pmult-table (pt k)
  (let* ((l (ash 1 (1- k)))
         (tab (make-array l :element-type 'list :initial-element nil))
         (pti pt)
         (pt2 (ec-pdouble pt))
         (i 1) )
    (setf (svref tab 0) pt)
    (do () ((= i l) tab)
      (setq pti (ec-padd pti pt2))
      (setf (svref tab i) pti)
      (incf i) )))
;;
;; -------------------------------------------------------------------------- ;;

;;
;; find random point on given curve
;;
(defmfun $ec_random ()
  (ec-set? "ec_random")
  (ec-l2m (ec-random)) ) ;; does not return $ec_inf
;;
(defun ec-random ()      ;; does not return null
  (do ((p *ec-p*) (a *ec-a*) (b *ec-b*)
        x y rts ) 
      (())
    (setq x ($random p) ;; $random is set by set_random_state
          y (mod (+ (power-mod x 3 p) (* a x) b) p) )
    (when (or (= y 0) 
              (and (= 1 ($jacobi y p))
                   (setq rts (zn-nrt y 2 p `((,p 1)))
                         y (if (= 0 ($random 2)) (car rts) (cadr rts)) ))) ;; y#0, p>3: 2 solutions
      (return (list x y)) )))
;;
;;
;; check if point is on the current curve
;;
(defun $ec_point_p (pt) 
  (let ((fn "ec_point_p"))
    (ec-set? fn)
    (cond 
      ((equal pt '$ec_inf) t)
      (t (ec-point-p (ec-m2l pt fn))) )))
;;
(defun ec-point-p (pt) ;; check only non-infinite points
  (let ((p *ec-p*) 
        (x (car pt)) (y (cadr pt)) )
    (= (power-mod y 2 p) 
       (mod (+ (power-mod x 3 p) (* *ec-a* x) *ec-b*) p) )))
;;
;;
;; order of a point (as a factor of a given group order)
;;
(defmfun $ec_point_order (pt ord &optional fs-ord)
  (let ((fn "ec_point_order"))
    (ec-set? fn)
    (setq pt (ec-m2l pt fn))
    (unless (ec-point-p pt)
      (gf-merror (intl:gettext 
        "`~m': [~m,~m] is no curve point." ) fn (car pt) (cadr pt)) )
    (unless (and (integerp ord) (> ord 0))
      (gf-merror (intl:gettext 
        "`~m': Second argument must be a positive integer. Found ~m." ) fn ord) )
    (if fs-ord
      (if (and ($listp fs-ord) (setq fs-ord (cdr fs-ord))
               (every #'$listp fs-ord) )
        (setq fs-ord (mapcar #'cdr fs-ord))
        (gf-merror (intl:gettext 
          "Third argument to `~m' must be of the form [[p1, e1], ..., [pk, ek]]." ) fn))
      (setq fs-ord (let (($intfaclim)) (get-factor-list ord))) )
    (ec-point-order pt ord fs-ord) ))
;;
(defun ec-point-order (pt ord fs-ord)
  (setq pt (ec-projectify pt)) 
  (let ((s ord) qt fp fe)
    (dolist (f fs-ord s)
      (setq fp (car f)
            fe (cadr f)
            s (truncate s (expt fp fe)) )
      (do () (())
        (setq qt (ec-pmult s pt))
        (when (= (caddr qt) 0) (return))
        (setq qt (ec-pmult fp qt) 
              s (* fp s) )))))
;;
;;
;; discrete logarithm (simple baby-giant-variant, p < 2^36)
;;
(defmfun $ec_log (pt gen)
  (let ((fn "ec_log"))
    (ec-set? fn)
    (when (> (integer-length *ec-p*) 36)
      (gf-merror (intl:gettext 
        "`~m': The characteristic should not exceed a length of 36 bit." ) fn) )
    (setq pt (ec-m2l pt fn)
          gen (ec-m2l gen fn) )
    (ec-dlog-baby-giant pt gen *ec-p*) ))
;;
(defun ec-dlog-baby-giant (pt gen p)
  (let* ((lim (+ p 2 (* 2 (isqrt p))))
         (m (1+ (isqrt lim)))
         (s (floor (* 1.3 m)))
         (gi (ec-neg gen)) ;; additive inverse
          babies mg )
    (setf babies 
      (make-hash-table :size s :test #'equal :rehash-threshold 0.9) )
    (do ((r 0) b acc) 
        (())
      (setq b (ec-add pt acc))
      (when (null b) ;; point of infinity
        (clrhash babies)
        (return-from ec-dlog-baby-giant r) )
      (setf (gethash b babies) r)
      (incf r)
      (when (= r m) (return))      
      (setq acc (ec-add gi acc)) )
    (setq mg (ec-mult m gen))
    (do ((rr 1) (bb mg) r) 
      ((> rr m) nil)
      (when (setq r (gethash bb babies)) ;; points don't match if we would use projective coords
        (clrhash babies)
        (return-from ec-dlog-baby-giant (+ (* rr m) r)) )
      (incf rr)
      (setq bb (ec-add mg bb)) )))
;;
;;
;; curve twist
;;
(defmfun $ec_twist_curve () 
  (ec-set? "ec_twist_curve")
  (cons '(mlist simp) (ec-twist-curve)) )
;;
(defun ec-twist-curve ()
  (let ((p *ec-p*)
        (n 2) nn a b )
    (do () ((= ($jacobi n p) -1))
      (incf n) )
    (setq nn (* n n) 
          a (mod (* nn *ec-a*) p)
          b (mod (* n nn *ec-b*) p) )
    (list n a b) ))
;;
;;
;; trace of Frobenius (ord = #E = p+1 - trace)
;;
(defmfun $ec_trace () 
  (ec-set? "ec_trace")
  (let ((*gf-char* *ec-p*) 
        (*ef-arith?*) )
    (ec-mueller-10-5 *ec-a* *ec-b* *ec-p*) )) ;; naive, Shanks-Mestre or SEA
;;
;; *** SEA ****************************************************************** ;;
   
;; This Lisp-implementation of the Schoof-Elkies-Atkin algorithm is based on 
;;        Ein Algorithmus zur Bestimmung der Punktanzahl elliptischer           
;;     Kurven ueber endlichen Koerpern der Charakteristic groesser drei         
;; by Volker Mueller. 
;; http://lecturer.ukdw.ac.id/vmueller/publications.php#thesis

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 2.22
;;
(defmfun $ec_j_invariant () ;; test function
  (ec-set? "ec_j_invariant")
  (ec-j-invariant *ec-a* *ec-b* *ec-p*) )
;;
(defun ec-j-invariant (a b p)
  (let ((a4aa (* 4 a a a)))
    (zn-quo (* 1728 a4aa) (+ a4aa (* 27 b b)) p) ))
;;
;; -------------------------------------------------------------------------- ;;

;; modular polynomials ------------------------------------------------------ ;;
;;
;; We use precomputed Mueller and Atkin polynomials.
;;
(defun ec-sea-data? ()
  (unless (boundp '*ec-sea-data?*) 
    ($load "elliptic_curves/modular_polynomials.lisp") ))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 7.3 [Computation of at,bt] (find isogenous curve)
;;
(defmfun $ec_mueller_7_3 (l g) ;; test function
  (ec-set? "ec_mueller_7_3")
  (ec-sea-data?)
  (let* ((p *ec-p*)
         (*gf-char* p) 
         (*ef-arith?*)
         (j (ec-j-invariant *ec-a* *ec-b* p))
          gl f )
    (if (svref *ec-g* l)
      (setq gl (svref *ec-g* l)  f 'ec-mueller-7-3)          ;; Mueller modular poly 
      (setq gl (svref *ec-g1* l) f 'ec-isogenous-from-atkin) ) ;; Atkin modular poly 
    (cons '(mlist simp) 
      (multiple-value-list
        (apply f (list l (ec-gl-mod-p gl p) g j *ec-a* *ec-b* p)) ))))
;;
(defun ec-mueller-7-3 (l gl g j a b p) ;; g is a root of gl(x,j). (-> ec-glj-roots)
  (let* ((glx (ec-dx gl))
         (gly (ec-dy gl))
         (glxgj (ec-at-xy glx g j))
         (glygj (ec-at-xy gly g j))
         (glxxgj (ec-at-xy (ec-dx glx) g j))
         (glxygj (ec-at-xy (ec-dy glx) g j))
         (glyygj (ec-at-xy (ec-dy gly) g j))
         ;; choose s minimal, so that s*(l-1)/12 is an integer (page 58):
         (ggt (gcd (1- l) 12)) ;; 12/s
         (s (truncate 12 ggt))
         (s/12 (inv-mod ggt p))
         ;; step 1:
         (e4 (zn-quo a -3 p))
         (e6 (zn-quo b -2 p))
         (delta (zn-quo (* e4 e4 e4) j p)) ;; = (e4^3 - e6^2)/1728
         (deltal (zn-quo (* delta (power-mod g ggt p)) (power-mod l 12 p) p))
         ;; step 2:
         (dg (mod (* g glxgj) p)) ;; DF in Mueller 7.3
         (dj (mod (* j glygj) p)) )
    (cond
      ((= 0 dj) 
        ;; remark 7.2, page 110: 
        ;; p = 65537, a = 1, b = 33965, l = 19 => j = 19797, roots = [24273,1616]
        ;; with g = 24273 we have dj = 0
        ;; steps 3-6:
        (let* ((e4l (zn-quo e4 (* l l) p))
               (at (mod (* -3 e4l (power-mod l 4 p)) p))
               (jl (zn-quo (* e4l e4l e4l) deltal p))
               (e6l (car (zn-nrt (* (- jl 1728) deltal) 2 p))) 
               ;; Mueller suggests to return the neg. and pos. root (step 6).
               ;; Do we need two solutions?
               ;; The above example results the same trace = 3 mod 19 for both roots.
               (bt (mod (* 2 e6l (power-mod l 6 p)) p))
               (p1 0) )
          (values at bt p1) ))
      (t ;; dj # 0:
               ;; steps 7-9:
        (let* ((e2* (zn-quo (* -12 e6 dj) (* s e4 dg) p))
               (e0 (zn-quo e6 (* e4 e2*) p))
               (gq (mod (* -1 s/12 e2* g) p))
               (jq (zn-quo (* -1 e4 e4 e6) delta p))
               ;; step 10:
               (dgq (mod (+ (* gq glxgj) (* g (+ (* gq glxxgj) (* jq glxygj)))) p))
               (djq (mod (+ (* jq glygj) (* j (+ (* jq glyygj) (* gq glxygj)))) p))
               ;; steps 11-14:
               (e0q (zn-quo (- (* -1 s/12 dgq) (* e0 djq)) dj p))
               (tmp (+ (* 12 e0q (inv-mod e0 p)) (- (* 6 e4 e4 (inv-mod e6 p)) (* 4 e6 (inv-mod e4 p)))))
               (e4l (zn-quo (+ e4 (* e2* (- e2* tmp))) (* l l) p))
               (jl (zn-quo (* e4l e4l e4l) deltal p))
               (ff (zn-quo (power-mod l s p) g p))
               (fq (mod (* s/12 e2* ff) p))
               ;; step 15:
               (dg2 (ec-at-xy glx ff jl))
               (dj2 (ec-at-xy gly ff jl))
               ;; steps 16-17:
               (jlq (zn-quo (* -1 fq dg2) (* l dj2) p))
               (e6l (if (= e4l 0) 0 (zn-quo (* -1 e4l jlq) jl p))) 
               ;; TODO: case e4l = 0: e6l = ?
               ;; e.g. [a,b,p,l]:[625,41,907,7] (one-root-case)
               ;; step 18:
               (at (mod (* -3 e4l (power-mod l 4 p)) p))
               (bt (mod (* -2 e6l (power-mod l 6 p)) p))
               (p1 (zn-quo (* -1 l e2*) 2 p)) )
          (values at bt p1) )))))
;;
;; f = f(x,y) mod p
;;
;; x? and y? indicate whether this variable is present, 
;; e.g. if y? is nil then f is effectively a univariate poly in x.
;;
;; The global *gf-char* is used for reduction.
;;
(defun ec-at-y (f x? y)
  (cond
    ((null f) 0)
    (x? (do ((itr f (cddr itr)) res cy)
            ((null itr) res)
         (setq cy (cadr itr))
         (when (consp cy) (setq cy (gf-at cy y)))
         (setq res (nconc res (list (car itr) cy))) ))
    (t (gf-at f y)) ))
;;
(defun ec-at-x (f x y?)
  (cond
    ((null f) 0)
    (y? (do ((res nil) cy xx) (())
          (setq cy (cadr f))
          (unless (consp cy) (setq cy (list 0 cy)))
          (setq res (gf-nplus res cy))
          (when (null (cddr f)) 
            (setq xx (gf-cpow x (car f)))
            (return (gf-xctimes res xx)) )
          (setq xx (gf-cpow x (- (car f) (caddr f)))
                res (gf-xctimes res xx)
                f (cddr f) )))
    (t (gf-at f x)) ))
;;
(defun ec-at-xy (f x y)
  (if (null f) 0
    (ec-at-x (ec-at-y f t y) x nil) ))
;;
;; partial derivatives:
;;
(defun ec-dx (f)
  (do ((itr f (cddr itr)) res e cy)
      ((null itr) res)
    (setq e (car itr))
    (when (= e 0) (return res))
    (setq cy (cadr itr) 
          cy (if (consp cy) (gf-xctimes cy e) (gf-ctimes e cy)) )
    (setq res (nconc res (list (1- e) cy))) ))
;;
(defun ec-dy (f)
  (do ((itr f (cddr itr)) res cy)
      ((null itr) res)
    (setq cy (cadr itr))
    (when (consp cy)
      (setq cy (gf-diff cy))
      (when (= 0 (car cy)) (setq cy (cadr cy)))
      (setq res (nconc res (list (car itr) cy)) ))))
;;
;; For l > 43 a lot of mueller polynomials have sizes of more than 100 kB. 
;; We use Atkin polynimials instead. Mueller 7.3 needs some modifications. 
;; These are copied from pari-gp/src/modules/ellsea.c/find_isogenous_from_Atkin
;; (license GPL).
;;
(defun ec-isogenous-from-atkin (l gl g j a b p) ;; g is a root of gl(x,j)
  (let* ((glx (ec-dx gl))
         (gly (ec-dy gl))
         (glxgj (ec-at-xy glx g j))
         (glygj (ec-at-xy gly g j))
         (glxxg (ec-at-x (ec-dx glx) g t))
         (glxyg (ec-at-x (ec-dy glx) g t))
         (glyyg (ec-at-x (ec-dy gly) g t))
         (e4 (zn-quo a -3 p))
         (e6 (zn-quo b -2 p))
         (dg (mod (* g glxgj) p)) ;; dx in pari-gp
         (dj (mod (* j glygj) p)) ;; dJ
         (aa (mod (* dj g e6) p))
         (bb (mod (* dg e4) p))
          gp u1 pg* dg* pj* dj* u v e4t e6t u2 p1 at bt fc ck wp )
    (when (or (= aa 0) (= bb 0)) (return-from ec-isogenous-from-atkin (values)))
    (setq gp (zn-quo aa bb p) ;; gprime
          u1 (ec-compute-u gp glxxg glxyg glyyg j glygj glxgj 1 e4 e6 p) )
    (dolist (jt (ec-glg-roots gl g p))
      (setq pg* (ec-at-xy glx g jt)
            dg* (mod (* g pg*) p)
            pj* (ec-at-xy gly g jt)
            dj* (mod (* jt l pj*) p)
            u (mod (* dg* dj e6) p)
            v (mod (* dj* dg e4) p)
            e4t (zn-quo (* u u jt) (* v v (- jt 1728)) p)
            e6t (zn-quo (* u e4t) v  p)
            u2 (ec-compute-u gp glxxg glxyg glyyg jt pj* pg* l e4t e6t p)
            p1 (mod (* 3 l (- u1 u2)) p)
            at (mod (* -3 (power-mod l 4 p) e4t) p)
            bt (mod (* -2 (power-mod l 6 p) e6t) p) )
      (multiple-value-setq (fc ck wp) (ec-mueller-7-8 l at bt p1 ck wp a b p))
      (ec-set-div-poly-mod l l fc a b p)
      (when (null (svref *ec-psi* l))
        (return (values at bt fc)) ))))
;;
(defun ec-compute-u (gp glxxg glxyg glyyg j glygj glxgj q e4 e6 p)
  (let* ((glxxgj (ec-at-y glxxg nil j))
         (glxygj (ec-at-y glxyg nil j))
         (glyygj (ec-at-y glyyg nil j))
         (e6/e4 (zn-quo e6 e4 p))
         (aa (mod (* gp glxxgj) p))
         (bb (mod (* 2 j q glxygj e6/e4) p))
         (cc (mod (* (zn-quo (* e6/e4 e6/e4) gp p) j) p))
         (dd (mod (* cc q q (+ glygj (* j glyygj))) p))
         (ee (mod (- (zn-quo e6/e4 3 p) (zn-quo (* e4 e4) (* 2 e6) p)) p))
         (ff (mod (- bb aa dd) p)) )
    (mod (+ (zn-quo ff glxgj p) (* ee q)) p) ))
;;
(defun ec-glg-roots (gl g p) 
  (let ((glg (ec-at-x gl g t))
         x^p ggt )
    (setq glg (gf-xctimes glg (inv-mod (cadr glg) p)) ;; monicize poly
          x^p (gf-pow-sliding-window (list 1 1) p glg)
          ggt (gf-gcd (gf-plus x^p (list 1 (1- p))) glg) )
    (ec-extract-roots (gf-equal-degree-factors (list ggt 1) p 1) p) ))
;;
;; -------------------------------------------------------------------------- ;;

;; division polynomials ----------------------------------------------------- ;;
;;
;; Mueller, definition 4.11, page 39
;;
(defun ec-set-div-poly-mod (cap n red a b p) 
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (when (< n 4) (setq n 4))
  (let ((aa (* a a))
        (cub (cond ((= 0 a) (list 3 1 0 b))
                   ((= 0 b) (list 3 1 1 a))
                   (t       (list 3 1 1 a 0 b)) ))
        (psi (setf *ec-psi* (make-array (+ cap 3) :element-type 'list :initial-element nil)))
        (psi2 (setf *ec-psi2* (make-array (+ cap 3) :element-type 'list :initial-element nil)))
        (psi3 (setf *ec-psi3* (make-array (+ cap 3) :element-type 'list :initial-element nil))) ) 
    (setf *ec-poly* (gf-nred cub red)
          *ec-poly2* (gf-sq *ec-poly* red)
          *ec-psi-red* red
          ;; (svref psi 0) nil     ;; 0
          (svref psi 1) (list 0 1) ;; 1
          (svref psi 2) (list 0 2) ;; 2
          (svref psi 3)            ;; 3*x^4+6*a*x^2+12*b*x-a^2
            (gf-nred (gf-mod (list 4 3 2 (* 6 a) 1 (* 12 b) 0 (- aa))) red)
          (svref psi2 1) (list 0 1)
          (svref psi2 2) (list 0 (mod 4 p))
          (svref psi3 1) (list 0 1)
          (svref psi3 2) (list 0 (mod 8 p))
          (svref psi 4) ;; 4*x^6+20*a*x^4+80*b*x^3-20*a^2*x^2-16*a*b*x-4*a^3-32*b^2
            (gf-nred (gf-mod (list 6 4 4 (* 20 a) 3 (* 80 b) 2 (- (* 20 aa)) 1 (- (* 16 a b)) 0 (- (+ (* 32 b b) (* 4 a aa))))) red)
          (svref psi2 3) (gf-sq (svref psi 3) red)
          (svref psi3 3) (gf-times (svref psi 3) (svref psi2 3) red) )
    (setq *ec-psi-i* 4)
    (ec-set-div-poly-mod1 n cap) ))
;;
(defun ec-set-div-poly-mod1 (n cap)
  (do ((i (1+ *ec-psi-i*) (1+ i)))
      ((> i n) (when (> n *ec-psi-i*) (setq *ec-psi-i* n)))
    (ec-set-psi i cap) ))
;;
(defun ec-set-psi (i cap)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((j (ash i -1))
        (cub2 *ec-poly2*)
        (psi *ec-psi*)
        (psi2 *ec-psi2*)
        (psi3 *ec-psi3*)
        (red *ec-psi-red*) )
    (setf (svref psi i)
      (cond
        ((evenp i)
          (unless (= i cap)
            (setf (svref psi3 (1+ j)) (gf-ktimes (svref psi (1+ j)) (svref psi2 (1+ j)) red)) )
          ;; f[j] * (f[j+2]*f[j-1]^2 - f[j-2]*f[j+1]^2) / 2
          (gf-xctimes
            (gf-ktimes 
              (svref psi j) 
              (gf-plus (gf-ktimes (svref psi (+ j 2)) (svref psi2 (1- j)) red)
                       (gf-nminus (gf-ktimes (svref psi (- j 2)) (svref psi2 (1+ j)) red)) ) red)
            *ec-inv2* ))
        (t
          (unless (= i cap)
            (setf (svref psi2 (+ j 2)) (gf-ksq (svref psi (+ j 2)) red)) )
          (if (evenp j)
            ;; cub^2*f[j+2]*f[j]^3 - f[j-1]*f[j+1]^3
            (gf-plus (gf-ktimes 
                       (gf-ktimes (svref psi (+ j 2)) (svref psi3 j) red)
                       cub2 red )
                     (gf-nminus (gf-ktimes (svref psi (1- j)) (svref psi3 (1+ j)) red)) )
            ;; f[j+2]*f[j]^3 - cub^2*f[j-1]*f[j+1]^3
            (gf-plus (gf-ktimes (svref psi (+ j 2)) (svref psi3 j) red)
                     (gf-nminus (gf-ktimes 
                                  (gf-ktimes (svref psi (1- j)) (svref psi3 (1+ j)) red)
                                  cub2 red )))))))))
;;
;; Karatsuba splitting:
;;
;; x*y = (x1*b^(n/2) + x0) * (y1*b^(n/2) + y0) =
;;
;;    x1*y1*(b^n - b^(n/2)) + (x0+x1)*(y0+y1)*b^(n/2) + x0*y0*(1 - b^(n/2))
;;
(defun gf-ktimes (x y red) 
  (declare (optimize (speed 3) (safety 0)))
  (unless (or (null x) (null y))
    (let ((n (max (car x) (car y))))
         (declare (fixnum n))
      (cond 
        ((< n 24) (gf-times x y red))
        (t
          (when (logbitp 0 n) (incf n))
          (gf-nktimes (copy-list x) (copy-list y) n red) )))))
;;
(defun gf-nktimes (x y n red) ;; modify x
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  (cond 
    ((< n 24) (gf-times x y red))
    (t
      (when (logbitp 0 n) (incf n))
      (let (x1 x0 y1 y0 z1 z0 zz (n/2 (ash n -1)))
        (multiple-value-setq (x1 x0) (gf-nsplit x n/2))
          (multiple-value-setq (y1 y0) (gf-nsplit y n/2))
            (setq z1 (gf-times x1 y1 red) 
                  z1 (gf-nplus (gf-nxetimes (copy-list z1) n) (gf-nminus (gf-nxetimes z1 n/2))) 
                  z0 (gf-times x0 y0 red) 
                  z0 (gf-nplus (copy-list z0) (gf-nminus (gf-nxetimes z0 n/2))) 
                  zz (gf-nxetimes (gf-times (gf-nplus x0 x1) (gf-nplus y0 y1) red) n/2) )
            (gf-nred (gf-nplus (gf-nplus z0 z1) zz) red) ))))
;;
;; x^2 = (z1*b^(n/2) + z0)^2 = b^n*z1^2 + 2*b^(n/2)*z0*z1 + z0^2
;;
;;   where the characteristic is assumed to be odd
;;
(defun gf-ksq (x red) 
  (declare (optimize (speed 3) (safety 0)))
  (unless (null x)
    (let ((n (car x)))
         (declare (fixnum n))
      (cond 
        ((< n 16) (gf-sq x red))
        (t
          (when (logbitp 0 n) (incf n))
          (gf-nksq (copy-list x) n red) )))))
;;
(defun gf-nksq (x n red) ;; modify x
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  (cond 
    ((< n 16) (gf-sq x red))
    (t
      (when (logbitp 0 n) (incf n))
      (let (z1 z0 zz (n/2 (ash n -1)))
        (multiple-value-setq (z1 z0) (gf-nsplit x n/2))
          (cond
            ((null z1) 
              (gf-nred (gf-nksq z0 n/2 red) red) )
            ((null z0) 
              (gf-nred (gf-nxetimes (gf-nksq z1 n/2 red) n) red) )
            (t 
              (setq zz (gf-xectimes (gf-times z1 z0 red) n/2 2)    
                    z0 (gf-nksq z0 n/2 red) 
                    z1 (gf-nxetimes (gf-nksq z1 n/2 red) n))
              (gf-nred (gf-nplus (gf-nplus z0 z1) zz) red) ))))))
;;
(defun gf-nsplit (x n) ;; modify x
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum n))
  (if (null x) 
    (values nil nil)
    (prog (r r0)
      a1
      (when (< (the fixnum (car x)) n) (return (values nil x)))
      (rplaca x (- (the fixnum (car x)) n))
      (setq r (cdr x))
      a 
      (when (null (cdr r)) (return (values x nil)))
      (when (< (the fixnum (cadr r)) n) 
        (setq r0 (cdr r))
        (rplacd r nil)
        (return (values x r0)) )
      (setq r (cdr r))
      (rplaca r (- (the fixnum (car r)) n))
      (setq r (cdr r))
      (go a) )))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 7.8 [Computation of f_C] 
;;
(defmfun $ec_mueller_7_8 (l at bt p1) ;; test function
  (ec-set? "ec_mueller_7_8")
  (let ((*gf-char* *ec-p*) 
        (*ef-arith?*) )
    (gf-x2p (ec-mueller-7-8 l at bt p1 nil nil *ec-a* *ec-b* *ec-p*)) ))
;;
(defun ec-mueller-7-8 (l at bt p1 ck wpv a b p)
  (let* ((d (ash (- l 1) -1))
         (d1 (1+ d))                    ;; step 1
         (l3 (1+ (ash (- l 3) -1))) 
          ctk hlp h fc )
    (unless ck
      ;; ck,wp do not depend on at,bt,p1 and can be reused in a second run
      (setq ck (ec-ck l3 a b p)         ;; 2,3
            wpv (ec-wpv d1 l3 ck) ))    ;; 4-6
    (setq ctk (ec-ck l3 at bt p)        ;; 8,9
          hlp (ec-hlp l l3 ck ctk p1 p) ;; 10, second sum
          h (ec-h-init d1 hlp p)        ;; 10
          fc (ec-fc d h wpv p) )        ;; 11-14
    (values fc ck wpv) ))
;;
;; Lemma 6.2, Mueller, page 90:
;;
(defun ec-ck (ll a b p) 
  (let ((ck (make-array (1+ ll) :element-type 'integer)))
    (when (> ll 0) (setf (svref ck 1) (zn-quo (- p a) 5 p)))
    (when (> ll 1) (setf (svref ck 2) (zn-quo (- p b) 7 p)))
    (when (> ll 2) 
      (do ((k 3 (1+ k)))
          ((= k ll))
        (setf (svref ck k) 0)
        (do ((i 1 (1+ i)) (k1 (1- k)))
            ((= i k1))
          (setf (svref ck k)
            (+ (svref ck k) (* (svref ck i) (svref ck (- k 1 i)))) ))
        (setf (svref ck k)
          (zn-quo (* 3 (svref ck k)) (* (- k 2) (+ (* 2 k) 3)) p) )))
    ck ))
;;
;; steps 4-6:
;;
(defun ec-wpv (d1 l3 ck) 
  (let ((wp (make-array d1 :element-type 'list :initial-element nil))
        (red (list d1 1))  ;; reduce by x^(d+1), remainder has necessary precision
        (wp1 (ec-weierstrass l3 ck)) ) ;; wp1(z), where z^2 is replaced by x 
    (setq wp1 (gf-nxetimes wp1 1))     ;; adjust exponents by z^2
    (setf (svref wp 1) wp1)
    ;; store wpv(z) (with exponents adjusted)
    (do ((v 2 (1+ v)))
        ((= v d1) wp)
      (setf (svref wp v) 
        (gf-times (svref wp (1- v)) wp1 red) )) ))
;;
(defun ec-weierstrass (l ck)
  (let ((wp (list -1 1))) ;; contains 1/x
    (do ((k 1 (1+ k)))
        ((= k l) wp)
      (push (svref ck k) wp)
      (push k wp) )
    wp ))
;;
;; step 10: return sum(coeffs*x^k , k,1,(l-3)/2) - p1, where x = z^2 
;;
(defun ec-hlp (l l3 ck ctk p1 p)
  (let ((hlp (if (= p1 0) nil (list 1 (- p p1)))))
    (do ((k 1) twok tmp)
        ((= k l3) hlp)
      (setq twok (* 2 k))
      (setq tmp (zn-quo (- (* l (svref ck k)) (svref ctk k)) 
                        (* (+ twok 1) (+ twok 2)) p) )
      (when (/= tmp 0) (push tmp hlp))
      (incf k)
      (when (/= tmp 0) (push k hlp)) )))
;;
;; step 10: return h * x^d
;;
(defun ec-h-init (d1 hlp p)
  (let ((red (list d1 1)) ;; reduce by x^(d+1)
        (h (list 0 1))
        (r! 1)
        (hlp^r (list 0 1)) )
    (do ((r 1 (1+ r)))
        ((= r d1) h)
      (setq r! (* r! r)
            hlp^r (gf-times hlp^r hlp red)
            h (gf-nplus h (gf-xctimes hlp^r (inv-mod r! p))) ))))
;;
;; steps 11-14:
;;
(defun ec-fc (d h wpv p)
  (let ((av 1)
        (fc (list d 1)) )
    (do ((v (1- d) (1- v)))
        ((< v 0) fc)
      (setq h (gf-nplus h (gf-xctimes (svref wpv (1+ v)) (- p av)))
            h (gf-nxetimes h -1) ;; h/x
            av (car (last h))
            fc (nconc fc (list v av)) ))))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 7.9 [Computation of c mod l] 
;;
(defmfun $ec_mueller_7_9 (l fc) ;; test function
  (ec-set? "ec_mueller_7_9")
  (let* ((p *ec-p*)
         (*gf-char* p) 
         (*ef-arith?*) )
    (ec-set-div-poly-mod l l fc *ec-a* *ec-b* p)
    (multiple-value-bind (c alpha) (ec-mueller-7-9 l (gf-p2x fc) p)
      (list '(mlist simp) c alpha) )))
;;
(defun ec-mueller-7-9 (l fc p) 
  (let* (;; steps 1,2: (ec-set-div-poly-mod called within ec-elkies)
         (psi2 *ec-psi2*)
         (psi3 *ec-psi3*)
         ;; step 3:
         (lx  (gf-pow-sliding-window (list 1 1) p fc))
         ;; step 4:
         (cub *ec-poly*)
         (ly  (gf-times (gf-xctimes cub 4) 
                        (gf-pow-sliding-window cub (ash (1- p) -1) fc) 
                        fc ))
          ry c )
    ;; step 5:
    (do ((alpha 1 (1+ alpha))
         (l1  (1+ (ash (- l 1) -1)))
          lhs )
        ((= alpha l1) (values))
      ;; step 7:
      (setq lhs (gf-ktimes lx (svref psi2 alpha) fc))
      (when (evenp alpha) (setq lhs (gf-times cub lhs fc))) 
      (when (equal lhs (ec-rx alpha cub fc))
        ;; steps 9-12:
        (setq lhs (gf-ktimes ly (svref psi3 alpha) fc))
        (when (evenp alpha) (setq lhs (gf-times cub lhs fc)))
        (setq ry (ec-ry alpha cub fc))
        (when (or (equal lhs ry)
                  (and (equal lhs (gf-minus ry)) (setq alpha (- l alpha))) )
          (setq c (+ alpha (zn-quo p alpha l)))
          (return (values (mod c l) alpha)) )))))
;; 
;; step 6:
;;
(defun ec-rx (n cub fc) ;; n >= 1
  (let* ((psi *ec-psi*)
         (r1 (gf-times (list 1 1) (svref *ec-psi2* n) fc))
         (r2 (gf-nminus (gf-ktimes (svref psi (1- n)) (svref psi (1+ n)) fc))) )
    (if (evenp n) 
      (setq r1 (gf-times cub r1 fc))
      (setq r2 (gf-times cub r2 fc)) )
    (gf-nplus r1 r2) ))
;;        
;; step 8:
;;
(defun ec-ry (n cub fc) ;; n >= 1
  (let* ((psi *ec-psi*)
         (psi2 *ec-psi2*)
         (r1 (gf-ktimes (svref psi (+ n 2)) (svref psi2 (1- n)) fc))
         (r2 (svref psi2 (1+ n))) 
          ry )
    (when (> n 1)
      (setq r2 (gf-nminus (gf-ktimes (svref psi (- n 2)) r2 fc))) )
    (setq ry (gf-nplus r1 r2))
    (when (oddp n) (setq ry (gf-times cub ry fc)))
    ry ))
;;
;; sliding-window polynomial exponentiation using Karatsuba multiplication
;;
(defun gf-pow-sliding-window (x e red)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let* ((l (integer-length e))
         (k (cond ((<= l  64) 3)
                  ((<= l 160) 4)
                  ((<= l 384) 5)
                  ((<= l 896) 6)
                  (t          7) ))
         (tab (gf-pow-sliding-window-table x k red))
         (res (list 0 1)) s u tmp )
    (do ((i (1- l)))
        ((< i 0) res)
      (cond
        ((logbitp i e)
          (setq s (max (1+ (- i k)) 0))
          (do () ((logbitp s e)) (incf s))
          (setq tmp (1+ (- i s)))
          (dotimes (h tmp) (setq res (gf-sq res red)))
          (setq u (ldb (byte tmp s) e))
          (unless (= u 0) (setq res (gf-ktimes res (svref tab (ash u -1)) red)))
          (setq i (1- s)) )
        (t
          (setq res (gf-sq res red))
          (decf i) )))))
;;
(defun gf-pow-sliding-window-table (x k red)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let* ((l (ash 1 (1- k)))
         (tab (make-array l :element-type 'list :initial-element nil))
         (xi x)
         (x2 (gf-sq x red)) )
    (setf (svref tab 0) x)
    (do ((i 1 (1+ i)))
        ((= i l) tab)
      (setq xi (gf-ktimes xi x2 red))
      (setf (svref tab i) xi) )))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 8.4 [Computation of c mod l^i in the Elkies case]
;;
(defun ec-elkies-l^i (i n l l1 gl tau1 i1_m iso1 a0 b0 a1 b1 a b p)
  ;; E = [a0,b0] (passed through to ec-set-div-poly-mod),
  ;; the postfix 1 indicates the corresponding value of the last round.

  ;; Different from the steps 1-3 in Mueller 8.4 the polynomial gt is not 
  ;; computed by Lemma 8.1. These steps are replaced by an algorithm from
  ;; 
  ;; Couveignes, Dewaghe, Morain - Isogeny cycles and the Schoof-Elkies-Atkin algorithm
  ;;
  ;; 4.1 procedure ComputeTModLn
  ;;
  ;; steps 1-2b done in ec-elkies
  ;; step 2c:
  
  ;; i) find next curve E11:
  (let* ((j (ec-j-invariant a b p))
         (rts (ec-glj-roots l j p))
          at bt p1 h11 ck wpv h11_m num iso11 h c tau11 l^i res )
    (dolist (g rts) 
      (multiple-value-setq (at bt p1) (ec-mueller-7-3 l gl g j a b p))
      (unless (or (= at 0) (= bt 0))
      
        ;; ii) compute the isogeny, the factor h11 of psi_l(E11) and
        ;; iii) the new factor h of psi_l^(i+1)(E):
        (multiple-value-setq (h11 ck wpv) (ec-mueller-7-8 l at bt p1 ck wpv a b p))
        (setq h11_m (gf-x2p h11)
              num (gf-p2x (meval `(($num) (($substitute) ,i1_m '$x ,h11_m)))))
        (ec-set-div-poly-mod l l num a1 b1 p)
        (unless (null (svref *ec-psi* l))
          (if (= i 2) 
            (setq iso11 iso1
                  h num )
            (setq iso11 (meval `(($substitute) ,iso1 '$x ,i1_m))
                  h (gf-p2x (meval `(($num) (($substitute) ,iso11 '$x ,h11_m)))) ))
          
          ;; iv) find eigenvalue mod l^(i+1) by Mueller 8.4, steps 4-10:
          (multiple-value-setq (c tau11) (ec-c-mod-l^i l1 l tau1 h a0 b0 p))
          (setq l^i (* l1 l))
          (cond 
            ((null tau11) nil)
            ((and (setq res `(,l^i (,c))) 
                  (= i n) ) 
              (return) )
            (t 
              (let* ((k11_m (gf-x2p (ec-k1 l a b at bt h11 p)))
                     (h11^2_m (gf-x2p (gf-sq h11 nil)))
                     (i11_m (meval `((mquotient) ,k11_m ,h11^2_m)))
                     (lc (ec-elkies-l^i (1+ i) n l l^i gl tau11 i11_m iso11 a0 b0 a b at bt p)) )
                (when lc (setq res lc) (return)) ))))))
    res ))
;;
;; 4.2. computing h1 and i1
;;
;; pre-process: k1
;;
(defun ec-k1 (l a b at bt h1 p)
  (let* ((ck (ec-ck l a b p))
         (w (ec-weierstrass l ck))
         (ckt (ec-ck l at bt p))
         (w1 (ec-weierstrass l ckt))
         (red (list l 1)) 
         (h1w^2 (gf-sq (gf-compose w h1 red) red)) ;; reduce by x^l (x = z^2)
         (k1w (gf-times w1 h1w^2 (list 1 1)))      ;; reduce by x^1
         ;; now we know k1(w) = w1*h1(w)^2, but we want to know k1(x)
         ;;
         ;; adjusting exponents:
         (val (gf-nxetimes k1w l)) ;; x^l*k1w
         (arg (gf-nxetimes w 1))   ;; x*w
          c y k1x )
   ;; given two power series arg = w and val = k1(w) 
   ;; we want to find a poly k1(x) such that val = k1(arg):
   (do ((i l (1- i))) 
       (())       
     (setq c (car (last val))                                    ;; c = coeff(val,x,0)
           k1x (nconc k1x (list i c)) )                          ;; k1x = k1x + c*x^i
     (when (= i 0) (return k1x))
     (setq y (gf-xctimes (gf-pow arg i (list (1+ i) 1)) (- p c)) ;; y = - c*arg^i mod x^(i+1)
           val (gf-nxetimes (gf-nplus val y) -1) ))))            ;; val = (val + y)/x
;;
;;
;; Mueller 8.4, steps 4-10:
;; 
(defun ec-c-mod-l^i (l^i l alpha gt a0 b0 p)
  ;; compute *ec-poly* and the division polynomials psi_0 to psi_l ...
  (ec-set-div-poly-mod (* l^i l) l gt a0 b0 p)
  (let ((psi *ec-psi*)
        (cub *ec-poly*)
        ;; step 4:
        (x^p (gf-pow-sliding-window (list 1 1) p gt))
         tmp rhs1 rhs2 lhs )
    ;; steps 5,6:
    (do* ((k 0 (1+ k)) 
          (alphat alpha (+ alphat l^i)) )
        ((= k l) (values))
      ;; ... and up to psi_{alphat+1} if necessary:
      (ec-set-div-poly-mod1 (1+ alphat) (* l^i l))
      ;; steps 7,8:
      (setq tmp (gf-ksq (svref psi alphat) gt)
            rhs1 (gf-times (list 1 1) tmp gt)
            rhs2 (gf-ktimes (svref psi (1- alphat)) (svref psi (1+ alphat)) gt)
            lhs (gf-ktimes x^p tmp gt) )
      (if (evenp alphat) 
        (setq lhs (gf-times cub lhs gt)
              rhs1 (gf-times cub rhs1 gt) )
        (setq rhs2 (gf-times cub rhs2 gt)) )
      (setq lhs (gf-nplus lhs rhs2)
            lhs (gf-nplus lhs (gf-nminus rhs1)) )
      ;; steps 9,10:
      (when (> (car (gf-gcd lhs gt)) 0)
        (setq l^i (* l l^i))
        (return (values (mod (+ alphat (zn-quo p alphat l^i)) l^i) alphat)) ))))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 9.2 [supersingularity] 
;;
(defun ec-mueller-9-2 (p) ;; assume p > 3
  (ec-check-trace 0 p) )  ;; prime fields: d = 1, m = p+1
;;
;; Mueller 9.5 [j-invariants 0 and 1728] 
;;
(defun ec-mueller-9-5 (b p) ;; use only: d = 1
  (let ((trs
          (if (= b 0) ;; j = 1728 (mod p)
            (let ((x (* 2 (car (cornacchia 4 p)))) ;; cornacchia replaces Mueller 9.4
                  (i2 (cons 1 (cdr (zn-nrt -1 2 p)))) ;; 1,i
                  (modulus p) )
              (mapcar #'(lambda (n) (cmod (* x n))) i2) ) ;; symmetric mod
          ;; j = 0:
            (let ((x (* 2 (car (cornacchia 3 p))))
                  (om3 (zn-nrt 1 3 p)) ;; 1,om,om^2
                  (modulus p) )
              (mapcar #'(lambda (n) (cmod (* x n))) om3) ))))
    (dolist (tr (nconc trs (mapcar #'neg trs)))
      ;; steps 8,15: check trace for "some" random points:
      (when (ec-check-trace tr p) (return tr)) )))
;;
;; Blake,Seroussi,Smart - Elliptic Curves in Cryptography, Algorithm 8.1
;;
;; compute a solution to x^2+d*y^2 mod p
;;
(defmfun $cornacchia (d p) ;; test function
  (let ((xy (cornacchia d p)))
    (when xy `((mlist simp) ,@xy)) ))
;; 
(defun cornacchia (d p)
  (let ((rts (zn-nrt (- p d) 2 p)))
    (when rts 
      (let* ((rt (car rts))
             (x (if (> (* 2 rt) p) (- p rt) rt))
             (h p)
              y )
        (do ((bound (isqrt p))) 
            ((<= x bound))
          (psetq h x x (mod h x)) )
        (setq h (- p (* x x)))
        (multiple-value-bind (q r) (truncate h d)
          (when (or (/= r 0) (and (setq y (isqrt q)) (/= (* y y) q)))
            (return-from cornacchia nil) ))
        (list x y) ))))
;;
;; check trace for "some" random points:
;; 
(defun ec-check-trace (tr p)
  (let ((ord (- (1+ p) tr)))
    (dotimes (i 3 t) ;; "some" = 3 (?)
      (unless (= (caddr (ec-pmult ord (ec-projectify (ec-random)))) 0)
        (return) ))))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 10.1 [information about c mod l] 
;;
;;
;; Compute the splitting type of gl(x,j) and call the appropriate subroutines.
;;
;; Assume j # 0, j # 1728 and l < odd prime p.
;; 
(defmfun $ec_mueller_10_1 (l) ;; l < p, test function
  (ec-set? "ec_mueller_10_1")
  (ec-sea-data?)
  (let* ((p *ec-p*) (a *ec-a*) (b *ec-b*)
         (*gf-char* p) 
         (*ef-arith?*)
         (j (ec-j-invariant a b p))
         (res (ec-mueller-10-1 l j a b p)) )
    `((mlist simp) ,(car res) ((mlist simp) ,@(cadr res))) ))
;;
(defun ec-mueller-10-1 (l j a b p) ;; l < p
  (let (nr one-rt elkies len k)
    ;; steps 1-7:
    (multiple-value-bind (rts gl glj x^p) (ec-glj-roots l j p) 
      (cond
        ;; step 13:
        ((null rts)
          (when $ec_sea_verbose (format t "atkin : "))
          (ec-atkin l glj x^p p) ) 
        ((= (setq nr (length rts)) 1)
          (when $ec_sea_verbose (format t "1 root: "))
          (setq one-rt (ec-one-root l p)
                elkies (ec-elkies 1 l gl rts j a b p) ) ;; ec-elkies might fail 
          (if elkies elkies one-rt) )
        ((= nr (1+ l))
          (when $ec_sea_verbose (format t "~d roots: " (1+ l)))
          (ec-lp1-roots l p) )
        ;; steps 8-11:
        (t ;; nr = 2
          (when $ec_sea_verbose (format t "elkies: "))
          (setq len (integer-length p)
                k (cond 
                    ((< len 52) 1)
                    ((= l 3) (if (< len 128) 3 4))
                    ((= l 5) (if (< len 192) 2 3))
                    ((= l 7) 2)
                    ((= l 11) (if (< len 160) 1 2))
                    ((= l 13) (if (< len 192) 1 2))
                    (t 1) ))
          (ec-elkies k l gl rts j a b p) )))))
;;
;;
(defmfun $ec_glj_roots (l) ;; test function
  (ec-set? "ec_glj_roots")
  (ec-sea-data?)
  (let* ((p *ec-p*)
         (*gf-char* p) 
         (*ef-arith?*)
         (j (ec-j-invariant *ec-a* *ec-b* p))
         (rts (ec-glj-roots l j p)) )
    (when rts (cons '(mlist simp) rts)) ))
;;
;; 10.1, steps 1-7:
;; Compute the splitting type of gl(x,j) and roots if there are any.
;;
(defun ec-glj-roots (l j p) 
  (let* (;; steps 1-3: use pre-computed polynomials instead
         (gl (ec-gl-mod-p (if (svref *ec-g* l) 
                           (svref *ec-g* l)    ;; Mueller modular poly 
                           (svref *ec-g1* l) ) ;; Atkin modular poly 
                         p ))
         ;; step 4: 
         (glj (ec-at-y gl t j))
         (x^p (gf-pow-sliding-window (list 1 1) p glj))
         ;; step 5:
         (ggt (gf-gcd (gf-plus x^p (list 1 (1- p))) glj))
          rts )
    ;; step 7:
    (unless (= 0 (car ggt))
      (setq rts (ec-extract-roots (gf-equal-degree-factors (list ggt 1) p 1) p)) )
    (values rts gl glj x^p) ))
;;
(defun ec-gl-mod-p (gl p) ;; gl = gl(x,y)
  (do ((itr gl (cddr itr)) res cy)
      ((null itr) res)
    (setq cy (cadr itr)
          cy (if (consp cy) (gf-mod cy) (mod cy p)) )
    (setq res (nconc res (list (car itr) cy))) ))
;;
(defun ec-extract-roots (factors p)
  (do ((fs factors (cddr fs)) fs1 len roots)
      ((null fs) (sort roots #'<))
    (setq fs1 (car fs) 
          len (length fs1) )
    (cond
      ((= 4 len) (push (- p (car (last fs1))) roots))
      ((= 2 len) (push 0 roots)) )))
;;
;; 10.1, steps 8-11 (the Elkies case)
;;
(defun ec-elkies (k l gl rts j a b p)
  (let (at bt p1 fc ck wpv found res )
    (dolist (g rts) 
      (cond 
        ((null (svref *ec-g* l)) ;; using Atkin modular poly:
          ;; steps 8,9:
          (multiple-value-setq (at bt fc) (ec-isogenous-from-atkin l gl g j a b p)) 
          (when at (setq found t)) )
        (t ;; using Mueller modular poly:
          ;; step 8:
          (multiple-value-setq (at bt p1) (ec-mueller-7-3 l gl g j a b p)) 
          (unless (or (= at 0) (= bt 0)) ;; see example in ec-mueller-7-3
            ;; step 9:
            (multiple-value-setq (fc ck wpv) (ec-mueller-7-8 l at bt p1 ck wpv a b p)) 
            (ec-set-div-poly-mod l l fc a b p)
            (when (null (svref *ec-psi* l)) (setq found t)) )))
      (when found
        ;; steps 10,11:
        (multiple-value-bind (c alpha) (ec-mueller-7-9 l fc p)
          (cond 
            ((null alpha) nil)
            ((and (setq res `(,l (,c))) 
                  (= k 1) )
              (return) )
            (t 
              (let* ((k1_m (gf-x2p (ec-k1 l a b at bt fc p)))
                     (h1^2_m (gf-x2p (gf-sq fc nil)))
                     (i1_m (meval `((mquotient) ,k1_m ,h1^2_m)))
                     (iso (let ((modulus p)) ($rat i1_m)))
                     (lc (ec-elkies-l^i 2 k l l gl alpha i1_m iso a b a b at bt p)) )
                (when lc (setq res lc) (return)) ))))))
    res ))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 10.2 [partial information] (the non-Elkies-case)
;;
;; 10.2, steps 1,2:
;; 
(defun ec-one-root (l p) 
  (let ((rt (car (zn-nrt p 2 l `((,l 1)))))
         h )
    (when rt ;; e.g. [a,b,p,l]:[233,38,9743,11] fails
      (setq h (mod (* 2 rt) l))
      `(,l (,h ,(- l h))) )))
;;
;; 10.2, steps 3-5:
;; 
(defun ec-lp1-roots (l p) 
  (let ((ll (* l l)) 
        alpha h )
    (setq ;; p (mod p ll)
          alpha (car (zn-nrt p 2 ll `((,l 2)))) )
    (when alpha
      (setq h (mod (+ alpha (zn-quo p alpha ll)) ll))
      `(,ll (,h ,(- ll h))) )))
;;
;; 10.2, steps 6-19 (the Atkin case) (l < p only):
;; 
(defun ec-atkin (l glj x^p p)
  (let* (;; step 6:
         (d-rest (car glj))
         ;; step 7:
         (odd-inc (if (= ($jacobi p l) 1) 0 1))
         ;; prepare computation in F_l^2:
         (n (do ((i 2 (1+ i))) ((= ($jacobi i l) -1) i))) ;; non-square mod l
         (ord (1- (* l l))) ;; order of F_l^2
         ($intfaclim)
         (fs (get-factor-list ord))
         (fs-ord (sort fs #'< :key #'car))
          ds d cs )
    ;; steps 8,14:
    (dolist (d (nreverse (cddr (meval `(($divisors) ,d-rest)))))
      (when (evenp (+ (* (1- d) (truncate d-rest d)) odd-inc)) 
          ;; evenp is effectively oddp when odd-inc is 1 resp. jacobi(p,l) # 1
        (push d ds) ))
    (setq d 
      (if (= (length ds) 1) ;; shortcut: just one possible d
        (car ds) 
        ;; here we compute d different from steps 9,15 in Mueller and use the 
        ;; approach of pari-gp in src/modules/ellsea.c/study_modular_eqn (license GPL): 
        (ec-comp-d l glj x^p p) ))
    
    ;; steps 12,13, 18,19:
    (if (= d 2) 
      (push 0 cs)
      (let ((*gf-char* l)              ;; locally compute in F_l^2
            (red (list 2 1 0 (- l n))) ;; reduction poly x^2 - n, n is non-square mod l
            (phi ($totient d))         ;; number of possible traces
             gen gam gam^i g1 z rts )
        (do ((i 1 (1+ i))) (())        
          (setq gen (list 1 1 0 i))    
          (when (*f-prim-p-1 gen red ord fs-ord) (return)) ) ;; gen is primitive element
        (setq gam (gf-pow gen (truncate ord d) red)          ;; gam is d-th root of unity
              gam^i gam )
        ;; compute the set of possible traces:
        (do ((i 1 (1+ i)) (old-i 1) (nr 0)) 
            ((= nr phi))
          (when (= (gcd i d) 1) ;; ord(gam^i) = d
            (setq gam^i (gf-times gam^i (gf-pow gam (- i old-i) red) red)
                  old-i i
                  g1 (if (= (length gam^i) 4) (car (last gam^i)) 0)
                  z (zn-quo (* p (1+ g1)) 2 l) )
            (when (= ($jacobi z l) 1)
              (setq rts (zn-nrt z 2 l `((,l 1))))
              (dolist (rt rts) 
                (setq rt (mod (* 2 rt) l))
                (unless (member rt cs) (incf nr) (push rt cs)) ))))))
    (list l (sort cs #'<)) ))
;;
(defun ec-comp-d (l glj x^p p)
  (let* ((lp1 (1+ l))
         (cs (nreverse (gf-x2l (copy-list x^p) lp1)))
          c x^p^i r )
    (setq c (nth 1 cs))
    (setf (nth 1 cs) (if (= c 0) (1- p) (1- c)))
    (setq x^p^i (mapcar #'list cs))
    (do ((i 2 (1+ i)) (acc x^p))
        ((= i lp1))
      (setq acc (gf-times acc x^p glj)
            cs (nreverse (gf-x2l acc lp1))
            c (nth i cs) )
      (setf (nth i cs) (if (= c 0) (1- p) (1- c)))
      (mapcar #'nconc x^p^i (mapcar #'list cs)) )
    ;; now x^p^i is a coefficient-matrix of powers of x^p as columns, where the 
    ;; (l+1)-identity-matrix is already subtracted and the first (zero) column omitted:
    (setq r (zn-nrank x^p^i p))
    (truncate lp1 (- lp1 r)) ))
;;
(defun zn-nrank (m p) ;; nulls m
  (do ((r 0) m1 row piv? piv inv c1 )
      ((null m) r)
    (setq m1 nil piv? nil)
    (do () ((null m))
      (setq row (car m) m (cdr m)
            c1 (car row) 
            row (cdr row) )
      (cond 
        ((= c1 0)
          (unless (every #'zerop row) (push row m1)) )
        (piv?
          (zn-naddrows row (- p c1) piv p) ;; modify row
          (unless (null row) (push row m1)) )        
        (t 
          (incf r)
          (setq piv row piv? t
                inv (inv-mod c1 p) )
          (zn-ncrow piv inv p) ))) ;; modify piv
    (setq m m1) ))
;;
(defun zn-ncrow (row c p) ;; c*row mod p, modifies row
  (do ((itr row (cdr itr)))
      ((null itr))
    (rplaca itr (mod (* c (car itr)) p)) ))
;;
(defun zn-naddrows (row c piv p) ;; row + c*piv mod p, modifies row, assumes len(row) = len(piv)
  (do ((itr row (cdr itr)))
      ((null itr))
    (rplaca itr (mod (+ (car itr) (* c (car piv))) p))
    (setq piv (cdr piv)) ))
;;  
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 10.4 [combination of the partial information]
;;

;; Preprocessing: Computation of the two sets C_i with possible traces c mod m_i:
;;

;; Algorithme de Nicolas pour le calcul de champions
;;
;; This algorithm reduces the work of the baby-step-giant-step-phase to a minimum 
;; by computing the optimal combination of given results from ec-atkin. 
;; The return value is a list containing the number of remaining possible traces 
;; and a binary pattern from which the combination can be recovered.
;;
;; (see Reynald Lercier, 
;;      ALGORITHMIQUE DES COURBES ELLIPTIQUES DANS LES CORPS FINIS, 11.2.1)
;;
(defun ec-champions (cost profit atkin-bound)
  (cond 
    ((= atkin-bound 0) nil)
    ((= (length profit) 1)
      (when (> (car profit) atkin-bound) (list 1 (car profit))) )
    (t
      (setq cost (coerce cost 'vector)
            profit (coerce profit 'vector) )
      (let* ((k (array-dimension cost 0))
             (s (max 256 (ash 1 k)))
             (a (make-array s :element-type 'integer :initial-element 0))
             (aq (make-array s :element-type 'integer :initial-element 0))
             (n 2) pro cst )
        (setf (svref aq 2) 1)
        (do ((j 2 (1+ j)) 
             (i 1 1) (i1 2 2) (i2 1 1)
              b b1 b2 )
            ((> j k))
          (do () ((> i1 n))
            (setq b1 (svref aq i1)
                  b2 (logior (svref aq i2) (ash 1 (1- j))) )
            (cond 
              ((< (ec-pattern2val b1 profit k) (ec-pattern2val b2 profit k))
                (setq b b1)
                (incf i1) )
              (t
                (setq b b2)
                (incf i2) ))
            (setq cst (ec-pattern2val b cost k))
            (while (< cst (ec-pattern2val (svref a i) cost k)) (decf i))
            (incf i)
            (setf (svref a i) b) )
          (do () ((> i2 n))
            (setq b (logior (svref aq i2) (ash 1 (1- j)))
                  cst (ec-pattern2val b cost k) )
            (while (< cst (ec-pattern2val (svref a i) cost k)) (decf i))
            (incf i)
            (setf (svref a i) b)
            (incf i2) )
          (setq n i)
          (do ((c 1 (1+ c))) 
              ((> c n)) 
            (setf (svref aq c) (svref a c)) ))
        (do ((i 1 (1+ i)) res) 
            ((= i s) res) 
          (when (/= 0 (svref a i))
            (setq pro (ec-pattern2val (svref a i) profit k))
            (when (>= pro atkin-bound)
              (setq cst (ec-pattern2val (svref a i) cost k))
              (when (or (null res) (< cst (cadr res)))
                (setq res (list (svref a i) cst)) )))) ))))
;;
(defun ec-pattern2val (pattern vec k)
  (do ((i 0 (1+ i)) (val 1))
      ((= i k) val)
    (when (logbitp i pattern)
      (setq val (* val (svref vec i))) )))
;;
;; Lercier notes that the size of the two sets C_i should have a quotient of 
;; about #C_babies = 3/4 #C_giants. Here 0.4 seems to be a better value.
;;
;; ec-cm12 tries to split atkin accordingly.
;; 
(defun ec-cm12 (atkin)
  (let* ((cost (mapcar #'length (mapcar #'cadr atkin)))
         (prod (apply #'* cost)) ;; nb*ng = prod
         (p4 (* 0.45 prod))      ;; nb = 0.45*ng   => nb^2 = p4 
         (delta prod)            ;; start value
         (clen (length cost))
         (pattern 0) ;; baby set pattern
          ilen nb n1 n2 d
          h1 h2 l1 l2 m1 m2 c1 c2 )
    (setq cost (coerce cost 'vector))
    ;; find the pattern:
    (do ((i 1 (1+ i)) (imax (ash 1 clen)))
        ((= i imax))
      (setq ilen (integer-length i)
            nb 1 )
      (do ((k 0 (1+ k)))
          ((= k ilen))
        (when (logbitp k i) 
          (setq nb (* nb (svref cost k))
                d (abs (- (* nb nb) p4)) ) ;; search for min(|nb^2 - p4|)
          (when (< d delta) 
            (setq delta d
                  n1 nb 
                  pattern (ldb (byte (1+ k) 0) i) )))))
    (setq n2 (truncate prod n1))
    ;; recover the combination from the pattern:
    (do ((k 0 (1+ k)) (itr atkin (cdr itr)))
        ((= k clen))
      (if (logbitp k pattern) (push (car itr) h1) (push (car itr) h2)) )
    ;; compute possible traces using CRT:
    (setq l1 (ec-combine-init (car h1))
          h1 (rest h1)
          l2 (ec-combine-init (car h2))
          h2 (rest h2) )
    (dolist (h h1) (setq l1 (ec-combine h l1)))
    (dolist (h h2) (setq l2 (ec-combine h l2)))
    (setq m1 (apply #'* (car l1))
          m2 (apply #'* (car l2))
          c1 (mapcar #'(lambda (l) (car (chinese l (car l1)))) (cadr l1))
          c2 (mapcar #'(lambda (l) (car (chinese l (car l2)))) (cadr l2)) )
    (values c1 n1 m1 c2 n2 m2) ))
;;
(defun ec-combine-init (l)
  `((,(car l)) ,(mapcar #'list (cadr l))))
;;
(defun ec-combine (l1 l2)
  (let ((mods (cons (car l1) (car l2))) res)
    (dolist (r (cadr l1) (list mods res))
      (dolist (rs (cadr l2))
        (push (cons r rs) res) ))))
;;
;; Mueller 10.4
;;
(defun ec-mueller-10-4 (elkies atkin p) 
  (let* ((cm3 (if elkies ;; elkies should always be non-null: (trace mod 2) is an elkies
                (chinese (mapcar #'caadr elkies) (mapcar #'car elkies))
                (list 0 1) ))
         (c3 (car cm3))
         (m3 (cadr cm3))
         (la (length atkin))
         (bound (floor (* 4 (sqrt (coerce p 'double-float)))))
         (run1 t)
          h mc1 c1 m1 inv-m3 c2 m2 c1-len c2-len inv-m2m3 inv-m1m3 m1/2 )
    (dotimes (i 3 nil) ;; step 19: if we fail after 3 tries we exit and change c3,m3
      (setq h 0) ;; step 2
      (cond
        ((= la 0) 
          (setq h (- c3 (* i m3))) )
        ((= la 1) 
          (when run1
            (setq mc1 (car atkin)
                  m1 (car mc1) 
                  c1 (cadr mc1) 
                  inv-m3 (inv-mod m3 m1) 
                  run1 nil ))
          (let* ((pt (ec-random))
                 (ptp (ec-projectify pt))
                 (q0 (ec-affinify (ec-pmult (- (1+ p) c3) ptp)))
                 (q1p (ec-pmult m3 ptp))
                 (q1 (ec-affinify q1p))
                 (q3 (ec-affinify (ec-pmult m1 q1p)))
                  r1q pt1 q1*2i )
            (cond 
              ((null q0) (setq h c3))
              (t 
                (setq q1*2i (ec-mult-table q1 m1))
                (dolist (x c1)
                  (setq r1q (- (mod (* (- x c3) inv-m3) m1) m1))
                  (when (and (= x 0) ;; r1q = - m1
                             (equal q0 (ec-neg q3)) )
                    (setq h (- c3 (* m1 m3)))
                    (return) )
                  (setq pt1 (ec-mult-by-table r1q q1 q1*2i))
                  (when (equal q0 pt1)
                    (setq h (- (+ c3 (* (+ r1q m1) m3)) (* m1 m3)))
                    (return h) )
                  (setq pt1 (ec-add pt1 q3))
                  (when (equal q0 pt1)
                    (setq h (+ c3 (* (+ r1q m1) m3)))
                    (return) )) ))))
        (t
          (when run1
            (multiple-value-setq (c1 c1-len m1 c2 c2-len m2) (ec-cm12 atkin))
            (setq inv-m2m3 (inv-mod (* m2 m3) m1) 
                  inv-m1m3 (inv-mod (* m1 m3) m2)
                  m1/2 (ash m1 -1)
                  run1 nil )
            (let* (;; step 1:
                   (pt (ec-random))
                   (ptp (ec-projectify pt)) 
                   ;; step 2:
                   (q0 (ec-affinify (ec-pmult (- (1+ p) c3) ptp)))
                   (qhp (ec-pmult m3 ptp))
                   (q1p (ec-pmult m2 qhp))
                   (q1 (ec-affinify q1p))
                   (q2 (ec-affinify (ec-pmult m1 qhp)))
                   (q3 (ec-affinify (ec-pmult m1 q1p)))
                   (k 0)
                    done babies tmp
                    r1q r1q-tab r1q0 dr1q pt1 q1*2i 
                    r2q r2q-tab r2q0 dr2q pt2 q2*2i )
              (when (null q0) (setq h c3 done t)) 
              ;; steps 3-5:
              (unless done
                (setf r1q-tab (make-array c1-len :element-type 'integer :initial-element 0) 
                      babies (make-array c1-len :element-type 'list :initial-element nil) )
                (dolist (x c1)
                  (setq r1q (mod (* (- x c3) inv-m2m3) m1))
                  (setf (svref r1q-tab k) (if (> r1q m1/2) (- r1q m1) r1q))
                  (incf k) )
                (sort r1q-tab #'<)
                (setq q1*2i (ec-mult-table q1 m1)
                      r1q0 (svref r1q-tab 0)
                      pt1 (ec-add q0 (ec-neg (ec-mult-by-table r1q0 q1 q1*2i)))
                      k 1 )
                (when (null pt1) (setq h (+ c3 (* r1q0 m2 m3)) done t))
                (unless done
                  (setf (svref babies 0) (nconc pt1 (list r1q0)))
                  (do () ((= k c1-len))
                    (setq r1q (svref r1q-tab k)
                          dr1q (- r1q r1q0)
                          r1q0 r1q )
                    (setq pt1 (ec-add pt1 (ec-neg (ec-mult-by-table dr1q q1 q1*2i))))
                    (when (null pt1) (setq h (+ c3 (* r1q0 m2 m3)) done t) (return))
                    (setf (svref babies k) (nconc pt1 (list r1q0)))
                    (incf k) )))
              (unless done
                (sort babies 
                  #'(lambda (a b) (or (< (car a)(car b)) 
                                      (and (= (car a)(car b)) (< (cadr a)(cadr b))) ))))
              ;; steps 6-14:
              (unless done
                (setf r2q-tab (make-array c2-len :element-type 'integer :initial-element 0))
                (setq k 0)
                (dolist (y c2)
                  ;; steps 7-9:
                  (setq r2q (- (mod (* (- y c3) inv-m1m3) m2) m2))
                  (when (= r2q (- m2)) 
                    (when (and q3 (setq r1q (ec-binary-pt-search q3 babies)))
                      (setq h (- (+ c3 (* r1q m2 m3)) (* m1 m2 m3))
                            done t )
                      (return) ))
                  (setf (svref r2q-tab k) r2q)
                  (incf k) ))
              (unless done
                (sort r2q-tab #'<)
                ;; steps 10-14:
                (setq q2*2i (ec-mult-table q2 m2)
                      r2q0 (svref r2q-tab 0)
                      pt2 (ec-mult-by-table r2q0 q2 q2*2i) )
                (cond 
                  ((and pt2 (setq r1q (ec-binary-pt-search pt2 babies)))
                    (setq h (+ c3 (* r1q m2 m3) (* r2q0 m1 m3))) )
                  ((and (setq tmp (ec-add pt2 q3))
                        (setq r1q (ec-binary-pt-search tmp babies)) )
                    (setq h (+ c3 (* r1q m2 m3) (* (+ r2q0 m2) m1 m3))) )
                  (t
                    (do ((k 1 (1+ k))) 
                        ((= k c2-len))
                      (setq r2q (svref r2q-tab k)
                            dr2q (- r2q r2q0)
                            r2q0 r2q
                            pt2 (ec-add pt2 (ec-mult-by-table dr2q q2 q2*2i)) )
                      (when (and pt2 (setq r1q (ec-binary-pt-search pt2 babies)))
                        (setq h (+ c3 (* r1q m2 m3) (* r2q m1 m3)))
                        (return) )
                      (when (and (setq tmp (ec-add pt2 q3)) 
                                 (setq r1q (ec-binary-pt-search tmp babies)) )
                        (setq h (+ c3 (* r1q m2 m3) (* (+ r2q m2) m1 m3)))
                        (return) ))) ))))))
      (setq h
        (cond 
          ((> (abs h) (ash bound -1)) nil)
          ;; steps 15,16:
          ((ec-check-trace h p) h) 
          ;; steps 17,18:
          (t (let ((divs (cdr (meval `(($divisors) ,h)))))
               (setq divs (delete h (nconc divs (mapcar #'neg divs))))
               (dolist (d divs nil)
                 (when (ec-check-trace d p) (return d)) )))))
      (when h (return h)) ))) 
      ;; step 19: else return nil and change c3,m3
;;
;;
(defun ec-binary-pt-search (pt a)
  #+ (or ccl ecl gcl) (declare (optimize (speed 3) (safety 0)))
  (let ((px (car pt)) (py (cadr pt))
        x y (m 0) am )
        (declare (fixnum m))
    (do ((lo 0) (hi (1- (array-dimension a 0))))
        ((< hi lo))
        (declare (fixnum lo hi))
      (setq m (ash (+ lo hi) -1)
            am (svref a m)
            x (car am) )
      (cond 
        ((< px x) (setq hi (1- m)))
        ((> px x) (setq lo (1+ m)))
        (t 
          (setq y (cadr am))
          (cond 
            ((< py y) (setq hi (1- m)))
            ((> py y) (setq lo (1+ m)))
            (t 
              (return (caddr am)) )))))))
;;
;;
(defun ec-mult-by-table (n pt pt*2i) 
  (cond
    ((or (= 0 n) (null pt)) nil)
    ((< n 0) (ec-neg (ec-mult-by-table (- n) pt pt*2i)))
    (t
      (do ((i 1 (1+ i)) res) 
          (())
        (when (oddp n)
          (setq res (ec-add pt res))
          (when (= 1 n) (return res)) )
        (setq n (ash n -1)
              pt (svref pt*2i i) )))))
;;
(defun ec-mult-table (pt range) 
  (let* ((ilen (integer-length range))
         (pt*2i (make-array ilen :element-type 'list :initial-element nil)) )
    (do ((i 1 (1+ i)))
        ((= i ilen) pt*2i)
      (setq pt (ec-add pt pt))
      (setf (svref pt*2i i) pt) )))
;;
;; -------------------------------------------------------------------------- ;;

;; -------------------------------------------------------------------------- ;;
;;
;; Mueller 10.5 [complete algorithm] 
;;
;; assume p prime 
;;
(defmfun $ec_mueller_10_5 () ;; test function
  (ec-set? "ec_mueller_10_5")
  (let ((*gf-char* *ec-p*) 
        (*ef-arith?*) )
    (ec-mueller-10-5 *ec-a* *ec-b* *ec-p*) ))
;;
(defun ec-mueller-10-5 (a b p)
  (let (tr)
    (cond
      ;; steps 1,2:
      ((or (and (= a 0) (= (mod p 3) 2)) ;; see 9.2.1, case 1
           (and (= b 0) (= (mod p 4) 3)) ;; see 9.2.2, case 1
           (ec-mueller-9-2 p) )
        (when $ec_sea_verbose (format t "supersingular~%"))
        0 )
      ;; steps 3,4:
      ((and (or (= a 0) (= b 0)) ;; j = 0 or 1728 (mod p)
            (setq tr (ec-mueller-9-5 b p)))
        (when $ec_sea_verbose (format t "j = 0 or 1728~%"))
        tr )
      ;; use naive algorithm or Shanks-Mestre for small primes:
      ((<= p 457) 
        (ec-trace-naive a b p) ) 
      ((< p 4503599627370496) ;; 2^52
        (ec-shanks-mestre a b p) )
      (t ;; SEA:
        (ec-sea-data?)
        (let ((bound (floor (* 4 (sqrt (coerce p 'double-float)))))
              (ilen (integer-length p))
              (m3 2) 
              (lmax 151)
              (m-atkin 1)
               bound-bsgs ls j tr2 lc l^i cs nrcs cost profit 
               elkies elkies1 atkin atkin1 champ pattern )
          (setq bound-bsgs ;; max. number of possible traces in 10.4
            (cond 
              ((>= ilen 256) 400000000)
              ((>= ilen 160) 300000000)
              (t             100000000) ))
          (setf elkies (make-array (1+ lmax) :element-type 'list :initial-element nil))
          ;; include trace mod 2:
          (setq tr2 (ec-trace-mod-2 a b p))
          (setf (svref elkies 2) tr2) ;; process l = 2 as Elkies
          (when $ec_sea_verbose (format t "mod 2 : [2, [~d]]~%" (caadr tr2)))
          ;; steps 6-9:
          (setq ls (cdr ($primes 3 lmax))
                j (ec-j-invariant a b p) ) ;; Mueller 10.1, step 4
          (dolist (l ls (gf-merror (intl:gettext "`ec_trace': Not enough modular polynomials.")))
            (setq lc (ec-mueller-10-1 l j a b p))
            (unless lc (when $ec_sea_verbose (format t "failure~%")))
            (when lc
              (setq l^i (car lc)
                    cs (cadr lc)
                    nrcs (length cs) )
              (when $ec_sea_verbose (format t "[~d, [~{~d~^,~}]]~%" l^i cs))
              (cond 
                ((= 1 nrcs) 
                  (setf (svref elkies l) lc) ;; process Atkin with c = 0 as Elkies
                  (setq m3 (* l^i m3)) )
                (t 
                  (push nrcs cost)
                  (push l^i profit)
                  (push lc atkin) ;; process Elkies with nrcs > 1 as Atkin
                  (setq m-atkin (* l^i m-atkin)) ))
              ;; steps 10-13:
              (when (>= (* m3 m-atkin) bound)
                (setq elkies1 (list tr2))
                (dolist (l ls) 
                  (when (setq lc (svref elkies l)) (push lc elkies1)) )
                (cond
                  ((> m3 bound)
                    (setq tr (ec-mueller-10-4 elkies1 nil p))
                    (if tr 
                      (return tr)
                      (ec-elkies-fallback elkies j a b p) ))
                  (t 
                    (setq champ (ec-champions cost profit (floor bound m3))
                          pattern (if champ (car champ) 0)
                          nrcs (if champ (cadr champ) 0) )
                    (when (< nrcs bound-bsgs)
                      (setq atkin1 nil)
                      (dolist (lc atkin)
                        (when (oddp pattern) (push lc atkin1))
                        (setq pattern (ash pattern -1)) )
                      (when atkin1 
                        (setq atkin1 
                          (sort atkin1 #'(lambda (a b) (< (length (cadr a)) (length (cadr b))))) ))
                      (setq tr (ec-mueller-10-4 elkies1 atkin1 p))
                      (if tr 
                        (return tr) 
                        (ec-elkies-fallback elkies j a b p) ))))))))))))
;;
(defun ec-elkies-fallback (elkies j a b p)
  (let (lc)
    (dolist (l '(3 5 7 11 13))
      (when (and (setq lc (svref elkies l))
                 (/= (car lc) l) )
        (multiple-value-bind (rts gl) (ec-glj-roots l j p)
          (setq lc (ec-elkies 1 l gl rts j a b p))
          (when $ec_sea_verbose (format t "fallback: [~d, [~{~d~^,~}]]~%" (car lc) (cadr lc)))
          (setf (svref elkies l) lc) )))))
;;
;; tr = 1 mod 2 if and only if x^3+a*x+b is irreducible over Fp
;;
(defun ec-trace-mod-2 (a b p)               
  (let* ((cub (cond
                ((= 0 a) (list 3 1 0 b))
                ((= 0 b) (list 3 1 1 a))
                (t (list 3 1 1 a 0 b)) ))
         (x^p (gf-pow-sliding-window (list 1 1) p cub))
         (x^p-x (gf-nplus x^p (list 1 (1- p)))) )
    `(2 (,(if (= 0 (car (gf-gcd cub x^p-x))) 1 0))) ))
;;
(defun ec-trace-naive (a b p)
  (do ((s 0) (x 0 (1+ x)) y)
      ((= x p) s)
    (setq y (mod (+ (power-mod x 3 p) (* a x) b) p)
          s (- s ($jacobi y p)) )))
;;
;; ************************************************************************** ;;

;; *** SHANKS-MESTRE ******************************************************** ;;
;;
;; This version of the Shanks-Mestre-Algorithm follows 
;;
;; Algorithm 7.4.12, Cohen - A Course in Computational Algebraic Number Theory
;;
(defmfun $ec_shanks_mestre () ;; test function
  (ec-set? "ec_shanks_mestre")
  (let ((*gf-char* *ec-p*) 
        (*ef-arith?*) )
    (ec-shanks-mestre *ec-a* *ec-b* *ec-p*) ))
;;
(defun ec-shanks-mestre (a b p)
  (let* ((delta (floor (* 2 (sqrt (coerce p 'double-float)))))
         (p+1 (1+ p))
         (lo (- p+1 delta))
         (hi (+ p+1 delta))
         (aa 0) aa1 (bb 1) ;; big-a, big-a1, big-b in Cohen
          k (k-old 0)
          d pt r m n fs-n h h1
         (x 0) )
    (do ((i 0 (1+ i))) 
        ((= i p) 
          (gf-merror (intl:gettext "`ec_trace': unexpected failure")) )
      (do () (())
        (setq d (mod (+ (power-mod x 3 p) (* a x) b) p)
              k ($jacobi d p) )
        (when (and (/= k 0) (/= k k-old)) (return))
        (incf x) )
      (setq k-old k
            aa1 (if (= k 1) aa (mod (- (* 2 p+1) aa) bb)) )
      (setq r (mod (- lo aa1) bb)
            m (if (= r 0) lo (+ lo (- bb r))) )
      (let* ((xd (mod (* x d) p))
             (dd (mod (* d d) p))
             (*ec-a* (mod (* dd *ec-a*) p)) ) ;; twist curve locally
        (setq pt (list xd dd) ;; point on twisted curve
              n (ec-baby-giant pt m bb p)
              fs-n (let (($intfaclim)) (get-factor-list n))
              h (ec-point-order pt n fs-n)
              h1 h ))
      ;; assume aa1 = 0 mod gcd(bb,h)
      (let* ((gc (zn-gcdex2 h bb)) ;; third and first entry of $gcdex
             (g (car gc))
             (c (cadr gc))
             (h/g (truncate h g))
             (lcm-hb (* h/g bb)) )
        (setq h1 (if (= 0 aa1) lcm-hb (mod (* c h/g aa1) lcm-hb))) )
      (when (and (< (- n h) lo) (> (+ n h) hi)) (return))
      (setq bb (lcm bb h) 
            aa (if (= 1 k) (mod h1 bb) (mod (- (* 2 p+1) h1) bb)) )) 
    (* k (- p+1 n)) ))
;;
(defun ec-baby-giant (pt m bb p) 
  (let* ((w (ceiling (* 2 (expt (/ (coerce p 'double-float) (* bb bb)) 0.25))))
         (s (ceiling (* 1.3 w))) 
         (ptp (ec-projectify pt))
         (m*pt (ec-affinify (ec-pmult m ptp)))
         (bb*pt (ec-affinify (ec-pmult bb ptp)))
          babies w*bb*pt )
    (setf babies 
      (make-hash-table :size s :test #'equal :rehash-threshold 0.9) )
    (do ((r 0) (qt m*pt)) (()) 
      (when (null qt)
        (clrhash babies)
        (return-from ec-baby-giant (+ m (* bb r))) )
      (setf (gethash qt babies) r)
      (incf r)
      (when (= r w) (return))      
      (setq qt (ec-add bb*pt qt)) )
    (setq w*bb*pt (ec-affinify (ec-pmult w (ec-projectify (ec-neg bb*pt)))))
    (do ((rr 1) r (qt w*bb*pt)) (())
      (when (setq r (gethash qt babies))
        (clrhash babies)
        (return-from ec-baby-giant (+ m (* bb r) (* w bb rr))) )
      (incf rr)
      (when (> rr w) 
        (gf-merror (intl:gettext "`ec_trace': unexpected failure")) )    
      (setq qt (ec-add w*bb*pt qt)) )))
;;
;; ************************************************************************** ;;
