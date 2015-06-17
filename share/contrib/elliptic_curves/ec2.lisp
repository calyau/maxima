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
   

   *** Elliptic Curves 2 *******************************************************
   
   Copyright Volker van Nek, 2015
   
   This file contains some algorithms for elliptic curves over binary fields.

   Maxima functions:
   ec2_set_curve, ec2_reduction, ec2_add, ec2_mult, ec2_random, 
   ec2_solve_quadratic, ec2_point_p, ec2_point_order
   
   
   Example session:

   (%i1) load("elliptic_curves")$
   
   ec2 ist part of the elliptic_curve package.
   
   (%i3) a: 1$
   (%i4) b: 1$
   (%i6) E: y^2 + x*y = x^3 + a*x^2 + b;
   (%i6)                     y^2+x*y=x^3+x^2+1
   
   (%i2) poly: x^163+x^7+x^6+x^3+1$
   (%i5) ec2_set_curve(poly, a,b)$
   
   This is the Koblitz curve sect163k1.
   
   ec2_set_curve sets the curve parameters as global variables. The defining 
   polynomial can be passed in as a number or as a polynomial.
   
   (%i7) obase: 16.$
   (%i8) ec2_reduction('number); 
   (%o8)              800000000000000000000000000000000000000c9
   
   ec2_reduction() shows the polynomial. With 'number as the optional argument, 
   the corresponding number is returned.
   
   (%i7) set_random_state(make_random_state(4711.))$
   (%i7) pt: ec2_random();
   (%i7) [34e19c14195489d593d08af3e19f19b371ab0d224, 1d668e7edbf0007e5ffc9620433c7c45a8e6ba7c3]
   
   In case the group order is known, the point order can be computed.
   
   (%i7) ord: 2 * 4000000000000000000020108a2e0cc0d99f8a5ef$
   (%i7) ec2_point_order(pt, ord);
   (%i7)              800000000000000000004021145c1981b33f14bde
   
   The point at infinity is ec2_inf.

   (%i10) ec_mult(ord, pt);
   (%o10)                             ec2_inf
|#


(declare-top (special 
  *ec2-a* *ec2-b* *ec2-exp* *ec2-red* *f2-red* 
  *ec2-delta* *ec2-tab* *ec2-set?* 
  *gf-char* *ef-arith?* ))


;;
;;
;; define a curve over a binary field (as global variables)
;;
(defmfun $ec2_set_curve (red a b)
  (setq *ec2-red* 
    (if (integerp red) 
      red 
      (let ((*gf-char* 2) (*ef-arith?*))
        (gf-x2n (gf-p2x-red red "ec2_set_curve")) )))
  (unless (and (integerp a) (integerp b))
    (let ((fn "ec2_set_curve"))
      (gf-merror (intl:gettext "Second and third argument to `~m' must be an integer.") fn) ))
  (let ((*f2-red* *ec2-red*))
    (setq *ec2-exp* (1- (integer-length *f2-red*))
          *ec2-a* (if (< a *f2-red*) a (f2-red a))
          *ec2-b* (if (< b *f2-red*) b (f2-red b))
          *ec2-delta* nil
          *ec2-tab* nil
          *ec2-set?* t )))
;;
(defun ec2-set? (fun)
  (if *ec2-set?* t
    (gf-merror (intl:gettext "`~m': The curve is not defined yet.") fun) ))
;;
;; the reduction polynomial
;; 
(defmfun $ec2_reduction (&optional rtype) ;; optional: return poly as number
  (ec2-set? "ec2_reduction") 
  (let ((*gf-char* 2) (*ef-arith?*)) 
    (if (equal rtype '$number) *ec2-red* (gf-x2p (gf-n2x *ec2-red*))) ))
;;
;;
;; convert between Maxima and Lisp level
;;
(defun ec2-m2l (pt_m fn)
  (ec2-set? fn) 
  (let (pt)
    (cond
      ((equal pt_m '$ec2_inf) nil)
      ((not (and ($listp pt_m)
                 (setq pt (cdr pt_m))
                 (every #'integerp pt) ))
        (gf-merror (intl:gettext "`~m': Unsuitable argument: ~m") fn pt_m) )
      (t 
        (let ((*f2-red* *ec2-red*))
          (mapcar #'(lambda (n) (f2-red n)) pt) )))))
;;
(defun ec2-l2m (pt)
  (if pt
    (cons '(mlist simp) pt)
    '$ec2_inf ))
;;
;;
;; point addition (in affine coords)
;;
(defmfun $ec2_add (pt qt) 
  (ec2-l2m (ec2-add (ec2-m2l pt "ec2_add") (ec2-m2l qt "ec2_add"))) )
;;
;;
;; point multiplication
;;
(defmfun $ec2_mult (n pt)
  (ec2-l2m (ec2-mult n (ec2-m2l pt "ec2_mult"))) )
;;

;; -------------------------------------------------------------------------- ;;
;;
;; arithmetic in affine coordinates
;;
(defun ec2-add (pt qt)
  (cond 
    ((null pt) qt)
    ((null qt) pt)
    ((equal pt qt) (ec2-double pt))
    ((equal pt (ec2-neg qt)) nil)
    (t (let* ((*f2-red* *ec2-red*)
              (xp (car pt)) 
              (yp (cadr pt))
              (xq (car qt)) 
              (yq (cadr qt))
              (xpq (logxor xp xq))
              (m (f2-times (logxor yp yq) (f2-inv xpq)))
              (xx (logxor (f2-times m m) m xpq *ec2-a*))
              (yy (logxor (f2-times m (logxor xp xx)) xx yp)) )
        (list xx yy) ))))
;;
(defun ec2-double (pt)
  (cond 
    ((null pt) nil)
    ((= 0 (car pt)) nil)
    (t (let* ((*f2-red* *ec2-red*)
              (x (car pt)) 
              (y (cadr pt))
              (x2 (f2-times x x))
              (xi (f2-inv x))
              (xx (logxor x2 (f2-times *ec2-b* (f2-times xi xi))))
              (tt (logxor x (f2-times y xi)))
              (yy (logxor x2 (f2-times tt xx) xx)) )
        (list xx yy) ))))
;;
(defun ec2-mult (n pt)
  (cond 
    ((or (= 0 n) (null pt)) nil)
    ((and (< n 0) 
          (setq n (- n) pt (ec2-neg pt)) 
          nil ))
    (t 
      (do ((res)) (())
        (when (oddp n)
          (setq res (ec2-add pt res))
          (when (= 1 n) (return res)) )
        (setq n (ash n -1)
              pt (ec2-double pt) )) )))
;;
(defun ec2-neg (pt) 
  (when pt 
    (let ((xp (car pt)))
      (list xp (logxor xp (cadr pt))) )))
;;
;; -------------------------------------------------------------------------- ;;

;;
;; find random point on given curve
;;
(defmfun $ec2_random ()
  (ec2-set? "ec2_random")
  (ec2-l2m (ec2-random)) ) ;; does not return $ec2_inf
;;
(defun ec2-random ()       ;; does not return null
  (do* (x y 
        (a *ec2-a*)
        (b *ec2-b*)
        (n *ec2-exp*)
        (q (ash 1 n)) ) 
       (())
    (setq x ($random q) ;; $random is set by set_random_state
          y (ec2-comp-y x a b n) )
    (when y
      (setq y (if (= ($random 2) 0) (car y) (cadr y)))
      (return (list x y)) )))
;;
;; y^2 + x*y = x^3 + a*x^2 + b
;; --> y = x*z, where z^2 + z = x + a + b/x^2
;;
(defun ec2-comp-y (x a b n) ;; q = 2^n
  (let* ((*f2-red* *ec2-red*)
         (xi (f2-inv x))
         (xi2 (f2-times xi xi))
         (tmp (f2-times b xi2))
         (beta (logxor x a tmp))
         (zz (cdr ($f2_solve_quadratic beta n))) )
    (when zz
      (mapcar #'(lambda (z) (f2-times x z)) zz) )))
;;

;; -------------------------------------------------------------------------- ;;
;;
;; Solving quadratic equations in F2n
;;
;;    (Blake, Seroussi, Smart - Elliptic Curves in Cryptography, II.2.4) 
;;
(defmfun $f2_solve_quadratic (beta n) ;; x^2 + x + beta = 0 in Fq, q = 2^n
  (let ((ht (f2-trace beta n))
         x )
    (when (= (car ht) 0)
      (setq x 
        (cond 
          ((oddp n) (cadr ht)) ;; cadr is halftrace
          (t
            (unless *ec2-delta*
              (do ((q (ash 1 n))) (())
                (setq *ec2-delta* (random q))
                (when (= (car (f2-trace *ec2-delta* n)) 1) ;; car is trace
                  (return) )))
            (f2-solve-even *ec2-delta* beta n) )))
      `((mlist simp) ,x ,(logxor x 1)) )))
;;
(defun f2-trace (b n) ;; compute trace and half-trace in parallel
  (let ((tr b) 
        (ht b)
        (*f2-red* *ec2-red*) ) 
    (do ((i 1 (1+ i)))
        ((= i n) (list tr ht))
        (declare (fixnum i))
      (setq b (f2-times b b)
            tr (logxor tr b) )
      (unless (logbitp 0 i) (setq ht (logxor ht b))) )))
;;
;; Blake, Seroussi, Smart - equation (II.8) - n is even
;;
(defun f2-solve-even (d b n) 
  (unless *ec2-tab* 
    (setq *ec2-tab* (f2-solve-even-table d n)) )
  (let* ((*f2-red* *ec2-red*)
         (s-tab *ec2-tab*)
         (s (car s-tab))
         (tab (cadr s-tab))
         (x (f2-times s b)) ) 
    (do ((i 1 (1+ i)))
        ((= i (1- n)) x)
        (declare (fixnum i))
      (setq b (f2-times b b)
            s (logxor (svref tab i) s)
            x (logxor (f2-times s b) x) ))))
;;
(defun f2-solve-even-table (d n)
  (let ((tab (make-array n :element-type 'integer :initial-element 0))
        (s 0)
        (*f2-red* *ec2-red*) )
    (do ((j 1 (1+ j)))
        ((= j n) (list s tab))
        (declare (fixnum j))
      (setq d (f2-times d d)
            s (logxor d s) )
      (setf (svref tab j) d) )))
;;
;; -------------------------------------------------------------------------- ;;

;;
;; check if point is on the current curve
;;
(defun $ec2_point_p (pt) 
  (let ((fn "ec2_point_p"))
    (ec2-set? fn)
    (cond 
      ((equal pt '$ec2_inf) t)
      (t (ec2-point-p (ec2-m2l pt fn))) )))
;;
(defun ec2-point-p (pt) ;; check only non-infinite points
  (let* ((*f2-red* *ec2-red*)
         (x (car pt)) 
         (y (cadr pt))
         (l (logxor (f2-times y y) (f2-times x y)))
         (r (logxor (f2-pow x 3) (f2-times *ec2-a* (f2-times x x)) *ec2-b*)) )
    (= l r) ))

;;
;; order of a point (as a factor of a given group order)
;;
(defmfun $ec2_point_order (pt ord &optional fs-ord)
  (let ((fn "ec2_point_order"))
    (ec2-set? fn)
    (setq pt (ec2-m2l pt fn))
    (unless (ec2-point-p pt)
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
    (ec2-point-order pt ord fs-ord) ))
;;
(defun ec2-point-order (pt ord fs-ord)
  (let ((s ord) qt fp fe)
    (dolist (f fs-ord s)
      (setq fp (car f)
            fe (cadr f)
            s (truncate s (expt fp fe)) )
      (do () (())
        (setq qt (ec2-mult s pt))
        (when (null qt) (return))
        (setq qt (ec2-mult fp qt) 
              s (* fp s) )))))
;;
;; -------------------------------------------------------------------------- ;;


