;;; Compiled by f2cl version 2.0 beta Date: 2006/01/31 15:11:05 
;;; Using Lisp CMU Common Lisp Snapshot 2006-01 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((nulim
       (make-array 2
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(35 70))))
  (declare (type (simple-array f2cl-lib:integer4 (2)) nulim))
  (defun dbesk (x fnu kode n y nz)
    (declare (type (simple-array double-float (*)) y)
             (type f2cl-lib:integer4 nz n kode)
             (type double-float fnu x))
    (prog ((w (make-array 2 :element-type 'double-float)) (cn 0.0) (dnu 0.0)
           (elim 0.0) (etx 0.0) (flgik 0.0) (fn 0.0) (fnn 0.0) (gln 0.0)
           (gnu 0.0) (rtz 0.0) (s 0.0) (s1 0.0) (s2 0.0) (t$ 0.0) (tm 0.0)
           (trx 0.0) (xlim 0.0) (zn 0.0) (i 0) (j 0) (k 0) (mz 0) (nb 0) (nd 0)
           (nn 0) (nud 0) (log$ 0))
      (declare (type f2cl-lib:integer4 log$ nud nn nd nb mz k j i)
               (type (simple-array double-float (2)) w)
               (type double-float zn xlim trx tm t$ s2 s1 s rtz gnu gln fnn fn
                                  flgik etx elim dnu cn))
      (setf nn (f2cl-lib:int-sub (f2cl-lib:i1mach 15)))
      (setf elim (* 2.303 (- (* nn (f2cl-lib:d1mach 5)) 3.0)))
      (setf xlim (* (f2cl-lib:d1mach 1) 1000.0))
      (if (or (< kode 1) (> kode 2)) (go label280))
      (if (< fnu 0.0) (go label290))
      (if (<= x 0.0) (go label300))
      (if (< x xlim) (go label320))
      (if (< n 1) (go label310))
      (setf etx
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-sub kode 1))
                      'double-float))
      (setf nd n)
      (setf nz 0)
      (setf nud (f2cl-lib:int fnu))
      (setf dnu (- fnu nud))
      (setf gnu fnu)
      (setf nn (min (the f2cl-lib:integer4 2) (the f2cl-lib:integer4 nd)))
      (setf fn (- (+ fnu n) 1))
      (setf fnn fn)
      (if (< fn 2.0) (go label150))
      (setf zn (/ x fn))
      (if (= zn 0.0) (go label320))
      (setf rtz (f2cl-lib:fsqrt (+ 1.0 (* zn zn))))
      (setf gln (f2cl-lib:flog (/ (+ 1.0 rtz) zn)))
      (setf t$ (+ (* rtz (- 1.0 etx)) (/ etx (+ zn rtz))))
      (setf cn (* (- fn) (- t$ gln)))
      (if (> cn elim) (go label320))
      (if (< nud (f2cl-lib:fref nulim (nn) ((1 2)))) (go label30))
      (if (= nn 1) (go label20))
     label10
      (setf fn gnu)
      (setf zn (/ x fn))
      (setf rtz (f2cl-lib:fsqrt (+ 1.0 (* zn zn))))
      (setf gln (f2cl-lib:flog (/ (+ 1.0 rtz) zn)))
      (setf t$ (+ (* rtz (- 1.0 etx)) (/ etx (+ zn rtz))))
      (setf cn (* (- fn) (- t$ gln)))
     label20
      (if (< cn (- elim)) (go label230))
      (setf flgik -1.0)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dasyik x gnu kode flgik rtz cn nn y)
        (declare (ignore var-0 var-1 var-2 var-3 var-6 var-7))
        (setf rtz var-4)
        (setf cn var-5))
      (if (= nn 1) (go label240))
      (setf trx (/ 2.0 x))
      (setf tm (/ (+ gnu gnu 2.0) x))
      (go label130)
     label30
      (if (= kode 2) (go label40))
      (if (> x elim) (go label230))
     label40
      (if (/= dnu 0.0) (go label80))
      (if (= kode 2) (go label50))
      (setf s1 (dbesk0 x))
      (go label60)
     label50
      (setf s1 (dbsk0e x))
     label60
      (if (and (= nud 0) (= nd 1)) (go label120))
      (if (= kode 2) (go label70))
      (setf s2 (dbesk1 x))
      (go label90)
     label70
      (setf s2 (dbsk1e x))
      (go label90)
     label80
      (setf nb 2)
      (if (and (= nud 0) (= nd 1)) (setf nb 1))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (dbsknu x dnu kode nb w nz)
        (declare (ignore var-0 var-1 var-2 var-3 var-4))
        (setf nz var-5))
      (setf s1 (f2cl-lib:fref w (1) ((1 2))))
      (if (= nb 1) (go label120))
      (setf s2 (f2cl-lib:fref w (2) ((1 2))))
     label90
      (setf trx (/ 2.0 x))
      (setf tm (/ (+ dnu dnu 2.0) x))
      (if (= nd 1) (setf nud (f2cl-lib:int-sub nud 1)))
      (if (> nud 0) (go label100))
      (if (> nd 1) (go label120))
      (setf s1 s2)
      (go label120)
     label100
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nud) nil)
        (tagbody
          (setf s s2)
          (setf s2 (+ (* tm s2) s1))
          (setf s1 s)
          (setf tm (+ tm trx))
         label110))
      (if (= nd 1) (setf s1 s2))
     label120
      (f2cl-lib:fset (f2cl-lib:fref y (1) ((1 *))) s1)
      (if (= nd 1) (go label240))
      (f2cl-lib:fset (f2cl-lib:fref y (2) ((1 *))) s2)
     label130
      (if (= nd 2) (go label240))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i nd) nil)
        (tagbody
          (f2cl-lib:fset (f2cl-lib:fref y (i) ((1 *)))
                         (+
                          (* tm
                             (f2cl-lib:fref y
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 *))))
                          (f2cl-lib:fref y ((f2cl-lib:int-sub i 2)) ((1 *)))))
          (setf tm (+ tm trx))
         label140))
      (go label240)
     label150
      (if (= kode 2) (go label160))
      (if (> x elim) (go label230))
     label160
      (if (<= fn 1.0) (go label170))
      (if (> (* (- fn) (- (f2cl-lib:flog x) 0.693)) elim) (go label320))
     label170
      (if (= dnu 0.0) (go label180))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (dbsknu x fnu kode nd y mz)
        (declare (ignore var-0 var-1 var-2 var-3 var-4))
        (setf mz var-5))
      (go label240)
     label180
      (setf j nud)
      (if (= j 1) (go label210))
      (setf j (f2cl-lib:int-add j 1))
      (if (= kode 2) (go label190))
      (f2cl-lib:fset (f2cl-lib:fref y (j) ((1 *))) (dbesk0 x))
      (go label200)
     label190
      (f2cl-lib:fset (f2cl-lib:fref y (j) ((1 *))) (dbsk0e x))
     label200
      (if (= nd 1) (go label240))
      (setf j (f2cl-lib:int-add j 1))
     label210
      (if (= kode 2) (go label220))
      (f2cl-lib:fset (f2cl-lib:fref y (j) ((1 *))) (dbesk1 x))
      (go label240)
     label220
      (f2cl-lib:fset (f2cl-lib:fref y (j) ((1 *))) (dbsk1e x))
      (go label240)
     label230
      (setf nud (f2cl-lib:int-add nud 1))
      (setf nd (f2cl-lib:int-sub nd 1))
      (if (= nd 0) (go label240))
      (setf nn (min (the f2cl-lib:integer4 2) (the f2cl-lib:integer4 nd)))
      (setf gnu (+ gnu 1.0))
      (if (< fnn 2.0) (go label230))
      (if (< nud (f2cl-lib:fref nulim (nn) ((1 2)))) (go label230))
      (go label10)
     label240
      (setf nz (f2cl-lib:int-sub n nd))
      (if (= nz 0) (go end_label))
      (if (= nd 0) (go label260))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nd) nil)
        (tagbody
          (setf j (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1))
          (setf k (f2cl-lib:int-add (f2cl-lib:int-sub nd i) 1))
          (f2cl-lib:fset (f2cl-lib:fref y (j) ((1 *)))
                         (f2cl-lib:fref y (k) ((1 *))))
         label250))
     label260
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nz) nil)
        (tagbody (f2cl-lib:fset (f2cl-lib:fref y (i) ((1 *))) 0.0) label270))
      (go end_label)
     label280
      (xermsg "SLATEC" "DBESK" "SCALING OPTION, KODE, NOT 1 OR 2" 2 1)
      (go end_label)
     label290
      (xermsg "SLATEC" "DBESK" "ORDER, FNU, LESS THAN ZERO" 2 1)
      (go end_label)
     label300
      (xermsg "SLATEC" "DBESK" "X LESS THAN OR EQUAL TO ZERO" 2 1)
      (go end_label)
     label310
      (xermsg "SLATEC" "DBESK" "N LESS THAN ONE" 2 1)
      (go end_label)
     label320
      (xermsg "SLATEC" "DBESK" "OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL" 6
       1)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nz)))))

