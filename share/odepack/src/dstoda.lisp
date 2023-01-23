;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(let ((sm1
       (make-array 12
                   :element-type 'double-float
                   :initial-contents '(0.5d0 0.575d0 0.55d0 0.45d0 0.35d0
                                       0.25d0 0.2d0 0.15d0 0.1d0 0.075d0 0.05d0
                                       0.025d0))))
  (declare (type (array double-float (12)) sm1))
  (defun dstoda (neq y yh nyh yh1 ewt savf acor wm iwm f jac pjac slvs)
    (declare (type (f2cl-lib:integer4) nyh)
             (type (array double-float (*)) wm acor savf ewt yh1 yh y)
             (type (array f2cl-lib:integer4 (*)) iwm neq))
    (let ((dls001-el
           (make-array 13
                       :element-type 'double-float
                       :displaced-to (dls001-part-0 *dls001-common-block*)
                       :displaced-index-offset 2))
          (dls001-elco
           (make-array 156
                       :element-type 'double-float
                       :displaced-to (dls001-part-0 *dls001-common-block*)
                       :displaced-index-offset 15))
          (dls001-tesco
           (make-array 36
                       :element-type 'double-float
                       :displaced-to (dls001-part-0 *dls001-common-block*)
                       :displaced-index-offset 173))
          (dlsa01-cm1
           (make-array 12
                       :element-type 'double-float
                       :displaced-to (dlsa01-part-0 *dlsa01-common-block*)
                       :displaced-index-offset 1))
          (dlsa01-cm2
           (make-array 5
                       :element-type 'double-float
                       :displaced-to (dlsa01-part-0 *dlsa01-common-block*)
                       :displaced-index-offset 13)))
      (symbol-macrolet ((conit (aref (dls001-part-0 *dls001-common-block*) 0))
                        (crate (aref (dls001-part-0 *dls001-common-block*) 1))
                        (el dls001-el)
                        (elco dls001-elco)
                        (hold (aref (dls001-part-0 *dls001-common-block*) 171))
                        (rmax (aref (dls001-part-0 *dls001-common-block*) 172))
                        (tesco dls001-tesco)
                        (ccmax
                         (aref (dls001-part-0 *dls001-common-block*) 209))
                        (el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                        (h (aref (dls001-part-0 *dls001-common-block*) 211))
                        (hmin (aref (dls001-part-0 *dls001-common-block*) 212))
                        (hmxi (aref (dls001-part-0 *dls001-common-block*) 213))
                        (hu (aref (dls001-part-0 *dls001-common-block*) 214))
                        (rc (aref (dls001-part-0 *dls001-common-block*) 215))
                        (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                        (uround
                         (aref (dls001-part-0 *dls001-common-block*) 217))
                        (ialth (aref (dls001-part-1 *dls001-common-block*) 6))
                        (ipup (aref (dls001-part-1 *dls001-common-block*) 7))
                        (lmax (aref (dls001-part-1 *dls001-common-block*) 8))
                        (nqnyh (aref (dls001-part-1 *dls001-common-block*) 10))
                        (nslp (aref (dls001-part-1 *dls001-common-block*) 11))
                        (icf (aref (dls001-part-1 *dls001-common-block*) 12))
                        (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                        (iersl (aref (dls001-part-1 *dls001-common-block*) 14))
                        (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                        (jstart
                         (aref (dls001-part-1 *dls001-common-block*) 16))
                        (kflag (aref (dls001-part-1 *dls001-common-block*) 17))
                        (l (aref (dls001-part-1 *dls001-common-block*) 18))
                        (meth (aref (dls001-part-1 *dls001-common-block*) 25))
                        (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                        (maxord
                         (aref (dls001-part-1 *dls001-common-block*) 27))
                        (maxcor
                         (aref (dls001-part-1 *dls001-common-block*) 28))
                        (msbp (aref (dls001-part-1 *dls001-common-block*) 29))
                        (mxncf (aref (dls001-part-1 *dls001-common-block*) 30))
                        (n (aref (dls001-part-1 *dls001-common-block*) 31))
                        (nq (aref (dls001-part-1 *dls001-common-block*) 32))
                        (nst (aref (dls001-part-1 *dls001-common-block*) 33))
                        (nfe (aref (dls001-part-1 *dls001-common-block*) 34))
                        (nqu (aref (dls001-part-1 *dls001-common-block*) 36))
                        (cm1 dlsa01-cm1)
                        (cm2 dlsa01-cm2)
                        (pdest (aref (dlsa01-part-0 *dlsa01-common-block*) 18))
                        (pdlast
                         (aref (dlsa01-part-0 *dlsa01-common-block*) 19))
                        (ratio (aref (dlsa01-part-0 *dlsa01-common-block*) 20))
                        (pdnorm
                         (aref (dlsa01-part-0 *dlsa01-common-block*) 21))
                        (icount (aref (dlsa01-part-1 *dlsa01-common-block*) 3))
                        (irflag (aref (dlsa01-part-1 *dlsa01-common-block*) 4))
                        (jtyp (aref (dlsa01-part-1 *dlsa01-common-block*) 5))
                        (mused (aref (dlsa01-part-1 *dlsa01-common-block*) 6))
                        (mxordn (aref (dlsa01-part-1 *dlsa01-common-block*) 7))
                        (mxords (aref (dlsa01-part-1 *dlsa01-common-block*) 8)))
        (f2cl-lib:with-multi-array-data
            ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
             (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%)
             (y double-float y-%data% y-%offset%)
             (yh double-float yh-%data% yh-%offset%)
             (yh1 double-float yh1-%data% yh1-%offset%)
             (ewt double-float ewt-%data% ewt-%offset%)
             (savf double-float savf-%data% savf-%offset%)
             (acor double-float acor-%data% acor-%offset%)
             (wm double-float wm-%data% wm-%offset%))
          (prog ((newq 0) (ncf 0) (m 0) (jb 0) (j 0) (iret 0) (iredo 0) (i1 0)
                 (i 0) (nqm2 0) (nqm1 0) (lm2p1 0) (lm2 0) (lm1p1 0) (lm1 0)
                 (told 0.0d0) (rhup 0.0d0) (rhsm 0.0d0) (rhdn 0.0d0) (rh 0.0d0)
                 (r 0.0d0) (exup 0.0d0) (exsm 0.0d0) (exdn 0.0d0) (dup 0.0d0)
                 (dsm 0.0d0) (delp 0.0d0) (del 0.0d0) (ddn 0.0d0) (dcon 0.0d0)
                 (rm 0.0d0) (rh2 0.0d0) (rh1it 0.0d0) (rh1 0.0d0) (rate 0.0d0)
                 (pnorm 0.0d0) (pdh 0.0d0) (exm2 0.0d0) (exm1 0.0d0)
                 (dm2 0.0d0) (dm1 0.0d0) (alpha 0.0d0))
            (declare (type (double-float) alpha dm1 dm2 exm1 exm2 pdh pnorm
                                          rate rh1 rh1it rh2 rm dcon ddn del
                                          delp dsm dup exdn exsm exup r rh rhdn
                                          rhsm rhup told)
                     (type (f2cl-lib:integer4) lm1 lm1p1 lm2 lm2p1 nqm1 nqm2 i
                                               i1 iredo iret j jb m ncf newq))
            (setf kflag 0)
            (setf told tn)
            (setf ncf 0)
            (setf ierpj 0)
            (setf iersl 0)
            (setf jcur 0)
            (setf icf 0)
            (setf delp 0.0d0)
            (if (> jstart 0) (go label200))
            (if (= jstart -1) (go label100))
            (if (= jstart -2) (go label160))
            (setf lmax (f2cl-lib:int-add maxord 1))
            (setf nq 1)
            (setf l 2)
            (setf ialth 2)
            (setf rmax 10000.0d0)
            (setf rc 0.0d0)
            (setf el0 1.0d0)
            (setf crate 0.7d0)
            (setf hold h)
            (setf nslp 0)
            (setf ipup miter)
            (setf iret 3)
            (setf icount 20)
            (setf irflag 0)
            (setf pdest 0.0d0)
            (setf pdlast 0.0d0)
            (setf ratio 5.0d0)
            (dcfode 2 elco tesco)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 5) nil)
              (tagbody
               label10
                (setf (f2cl-lib:fref cm2 (i) ((1 5)))
                        (* (f2cl-lib:fref tesco (2 i) ((1 3) (1 12)))
                           (f2cl-lib:fref elco
                                          ((f2cl-lib:int-add i 1) i)
                                          ((1 13) (1 12)))))))
            (dcfode 1 elco tesco)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 12) nil)
              (tagbody
               label20
                (setf (f2cl-lib:fref cm1 (i) ((1 12)))
                        (* (f2cl-lib:fref tesco (2 i) ((1 3) (1 12)))
                           (f2cl-lib:fref elco
                                          ((f2cl-lib:int-add i 1) i)
                                          ((1 13) (1 12)))))))
            (go label150)
           label100
            (setf ipup miter)
            (setf lmax (f2cl-lib:int-add maxord 1))
            (if (= ialth 1) (setf ialth 2))
            (if (= meth mused) (go label160))
            (dcfode meth elco tesco)
            (setf ialth l)
            (setf iret 1)
           label150
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i l) nil)
              (tagbody
               label155
                (setf (f2cl-lib:fref el (i) ((1 13)))
                        (f2cl-lib:fref elco (i nq) ((1 13) (1 12))))))
            (setf nqnyh (f2cl-lib:int-mul nq nyh))
            (setf rc (/ (* rc (f2cl-lib:fref el (1) ((1 13)))) el0))
            (setf el0 (f2cl-lib:fref el (1) ((1 13))))
            (setf conit (/ 0.5d0 (f2cl-lib:int-add nq 2)))
            (f2cl-lib:computed-goto (label160 label170 label200) iret)
           label160
            (if (= h hold) (go label200))
            (setf rh (/ h hold))
            (setf h hold)
            (setf iredo 3)
            (go label175)
           label170
            (setf rh (max rh (/ hmin (abs h))))
           label175
            (setf rh (min rh rmax))
            (setf rh (/ rh (max 1.0d0 (* (abs h) hmxi rh))))
            (if (= meth 2) (go label178))
            (setf irflag 0)
            (setf pdh (max (* (abs h) pdlast) 1.0d-6))
            (if (< (* rh pdh 1.00001d0) (f2cl-lib:fref sm1 (nq) ((1 12))))
                (go label178))
            (setf rh (/ (f2cl-lib:fref sm1 (nq) ((1 12))) pdh))
            (setf irflag 1)
           label178
            (setf r 1.0d0)
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j l) nil)
              (tagbody
                (setf r (* r rh))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref yh-%data%
                                         (i j)
                                         ((1 nyh) (1 *))
                                         yh-%offset%)
                            (*
                             (f2cl-lib:fref yh-%data%
                                            (i j)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)
                             r))))))
           label180
            (setf h (* h rh))
            (setf rc (* rc rh))
            (setf ialth l)
            (if (= iredo 0) (go label690))
           label200
            (if (> (abs (- rc 1.0d0)) ccmax) (setf ipup miter))
            (if (>= nst (f2cl-lib:int-add nslp msbp)) (setf ipup miter))
            (setf tn (+ tn h))
            (setf i1 (f2cl-lib:int-add nqnyh 1))
            (f2cl-lib:fdo (jb 1 (f2cl-lib:int-add jb 1))
                          ((> jb nq) nil)
              (tagbody
                (setf i1 (f2cl-lib:int-sub i1 nyh))
                (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                              ((> i nqnyh) nil)
                  (tagbody
                   label210
                    (setf (f2cl-lib:fref yh1-%data% (i) ((1 *)) yh1-%offset%)
                            (+
                             (f2cl-lib:fref yh1-%data%
                                            (i)
                                            ((1 *))
                                            yh1-%offset%)
                             (f2cl-lib:fref yh1-%data%
                                            ((f2cl-lib:int-add i nyh))
                                            ((1 *))
                                            yh1-%offset%)))))
               label215))
            (setf pnorm (dmnorm n yh1 ewt))
           label220
            (setf m 0)
            (setf rate 0.0d0)
            (setf del 0.0d0)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label230
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (f2cl-lib:fref yh-%data%
                                       (i 1)
                                       ((1 nyh) (1 *))
                                       yh-%offset%))))
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall f neq tn y savf)
              (declare (ignore var-0 var-2 var-3))
              (when var-1
                (setf tn var-1)))
            (setf nfe (f2cl-lib:int-add nfe 1))
            (if (<= ipup 0) (go label250))
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10)
                (funcall pjac neq y yh nyh ewt acor savf wm iwm f jac)
              (declare (ignore var-0 var-1 var-2 var-4 var-5 var-6 var-7 var-8
                               var-9 var-10))
              (when var-3
                (setf nyh var-3)))
            (setf ipup 0)
            (setf rc 1.0d0)
            (setf nslp nst)
            (setf crate 0.7d0)
            (if (/= ierpj 0) (go label430))
           label250
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label260
                (setf (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                        0.0d0)))
           label270
            (if (/= miter 0) (go label350))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref savf-%data% (i) ((1 *)) savf-%offset%)
                        (-
                         (* h
                            (f2cl-lib:fref savf-%data%
                                           (i)
                                           ((1 *))
                                           savf-%offset%))
                         (f2cl-lib:fref yh-%data%
                                        (i 2)
                                        ((1 nyh) (1 *))
                                        yh-%offset%)))
               label290
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (-
                         (f2cl-lib:fref savf-%data% (i) ((1 *)) savf-%offset%)
                         (f2cl-lib:fref acor-%data%
                                        (i)
                                        ((1 *))
                                        acor-%offset%)))))
            (setf del (dmnorm n y ewt))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (+
                         (f2cl-lib:fref yh-%data%
                                        (i 1)
                                        ((1 nyh) (1 *))
                                        yh-%offset%)
                         (* (f2cl-lib:fref el (1) ((1 13)))
                            (f2cl-lib:fref savf-%data%
                                           (i)
                                           ((1 *))
                                           savf-%offset%))))
               label300
                (setf (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                        (f2cl-lib:fref savf-%data%
                                       (i)
                                       ((1 *))
                                       savf-%offset%))))
            (go label400)
           label350
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label360
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (-
                         (* h
                            (f2cl-lib:fref savf-%data%
                                           (i)
                                           ((1 *))
                                           savf-%offset%))
                         (+
                          (f2cl-lib:fref yh-%data%
                                         (i 2)
                                         ((1 nyh) (1 *))
                                         yh-%offset%)
                          (f2cl-lib:fref acor-%data%
                                         (i)
                                         ((1 *))
                                         acor-%offset%))))))
            (funcall slvs wm iwm y savf)
            (if (< iersl 0) (go label430))
            (if (> iersl 0) (go label410))
            (setf del (dmnorm n y ewt))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                        (+
                         (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                         (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)))
               label380
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (+
                         (f2cl-lib:fref yh-%data%
                                        (i 1)
                                        ((1 nyh) (1 *))
                                        yh-%offset%)
                         (* (f2cl-lib:fref el (1) ((1 13)))
                            (f2cl-lib:fref acor-%data%
                                           (i)
                                           ((1 *))
                                           acor-%offset%))))))
           label400
            (if (<= del (* 100.0d0 pnorm uround)) (go label450))
            (if (and (= m 0) (= meth 1)) (go label405))
            (if (= m 0) (go label402))
            (setf rm 1024.0d0)
            (if (<= del (* 1024.0d0 delp)) (setf rm (/ del delp)))
            (setf rate (max rate rm))
            (setf crate (max (* 0.2d0 crate) rm))
           label402
            (setf dcon
                    (/ (* del (min 1.0d0 (* 1.5d0 crate)))
                       (* (f2cl-lib:fref tesco (2 nq) ((1 3) (1 12))) conit)))
            (if (> dcon 1.0d0) (go label405))
            (setf pdest
                    (max pdest
                         (/ rate (abs (* h (f2cl-lib:fref el (1) ((1 13))))))))
            (if (/= pdest 0.0d0) (setf pdlast pdest))
            (go label450)
           label405
            (setf m (f2cl-lib:int-add m 1))
            (if (= m maxcor) (go label410))
            (if (and (>= m 2) (> del (* 2.0d0 delp))) (go label410))
            (setf delp del)
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall f neq tn y savf)
              (declare (ignore var-0 var-2 var-3))
              (when var-1
                (setf tn var-1)))
            (setf nfe (f2cl-lib:int-add nfe 1))
            (go label270)
           label410
            (if (or (= miter 0) (= jcur 1)) (go label430))
            (setf icf 1)
            (setf ipup miter)
            (go label220)
           label430
            (setf icf 2)
            (setf ncf (f2cl-lib:int-add ncf 1))
            (setf rmax 2.0d0)
            (setf tn told)
            (setf i1 (f2cl-lib:int-add nqnyh 1))
            (f2cl-lib:fdo (jb 1 (f2cl-lib:int-add jb 1))
                          ((> jb nq) nil)
              (tagbody
                (setf i1 (f2cl-lib:int-sub i1 nyh))
                (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                              ((> i nqnyh) nil)
                  (tagbody
                   label440
                    (setf (f2cl-lib:fref yh1-%data% (i) ((1 *)) yh1-%offset%)
                            (-
                             (f2cl-lib:fref yh1-%data%
                                            (i)
                                            ((1 *))
                                            yh1-%offset%)
                             (f2cl-lib:fref yh1-%data%
                                            ((f2cl-lib:int-add i nyh))
                                            ((1 *))
                                            yh1-%offset%)))))
               label445))
            (if (or (< ierpj 0) (< iersl 0)) (go label680))
            (if (<= (abs h) (* hmin 1.00001d0)) (go label670))
            (if (= ncf mxncf) (go label670))
            (setf rh 0.25d0)
            (setf ipup miter)
            (setf iredo 1)
            (go label170)
           label450
            (setf jcur 0)
            (if (= m 0)
                (setf dsm (/ del (f2cl-lib:fref tesco (2 nq) ((1 3) (1 12))))))
            (if (> m 0)
                (setf dsm
                        (/ (dmnorm n acor ewt)
                           (f2cl-lib:fref tesco (2 nq) ((1 3) (1 12))))))
            (if (> dsm 1.0d0) (go label500))
            (setf kflag 0)
            (setf iredo 0)
            (setf nst (f2cl-lib:int-add nst 1))
            (setf hu h)
            (setf nqu nq)
            (setf mused meth)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j l) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref yh-%data%
                                         (i j)
                                         ((1 nyh) (1 *))
                                         yh-%offset%)
                            (+
                             (f2cl-lib:fref yh-%data%
                                            (i j)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)
                             (* (f2cl-lib:fref el (j) ((1 13)))
                                (f2cl-lib:fref acor-%data%
                                               (i)
                                               ((1 *))
                                               acor-%offset%))))))))
           label460
            (setf icount (f2cl-lib:int-sub icount 1))
            (if (>= icount 0) (go label488))
            (if (= meth 2) (go label480))
            (if (> nq 5) (go label488))
            (if (and (> dsm (* 100.0d0 pnorm uround)) (/= pdest 0.0d0))
                (go label470))
            (if (= irflag 0) (go label488))
            (setf rh2 2.0d0)
            (setf nqm2
                    (min (the f2cl-lib:integer4 nq)
                         (the f2cl-lib:integer4 mxords)))
            (go label478)
           label470
            (setf exsm (/ 1.0d0 l))
            (setf rh1 (/ 1.0d0 (+ (* 1.2d0 (expt dsm exsm)) 1.2d-6)))
            (setf rh1it (* 2.0d0 rh1))
            (setf pdh (* pdlast (abs h)))
            (if (> (* pdh rh1) 1.0d-5)
                (setf rh1it (/ (f2cl-lib:fref sm1 (nq) ((1 12))) pdh)))
            (setf rh1 (min rh1 rh1it))
            (if (<= nq mxords) (go label474))
            (setf nqm2 mxords)
            (setf lm2 (f2cl-lib:int-add mxords 1))
            (setf exm2 (/ 1.0d0 lm2))
            (setf lm2p1 (f2cl-lib:int-add lm2 1))
            (setf dm2
                    (/
                     (dmnorm n
                      (f2cl-lib:array-slice yh-%data%
                                            double-float
                                            (1 lm2p1)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)
                      ewt)
                     (f2cl-lib:fref cm2 (mxords) ((1 5)))))
            (setf rh2 (/ 1.0d0 (+ (* 1.2d0 (expt dm2 exm2)) 1.2d-6)))
            (go label476)
           label474
            (setf dm2
                    (* dsm
                       (/ (f2cl-lib:fref cm1 (nq) ((1 12)))
                          (f2cl-lib:fref cm2 (nq) ((1 5))))))
            (setf rh2 (/ 1.0d0 (+ (* 1.2d0 (expt dm2 exsm)) 1.2d-6)))
            (setf nqm2 nq)
           label476
            (if (< rh2 (* ratio rh1)) (go label488))
           label478
            (setf rh rh2)
            (setf icount 20)
            (setf meth 2)
            (setf miter jtyp)
            (setf pdlast 0.0d0)
            (setf nq nqm2)
            (setf l (f2cl-lib:int-add nq 1))
            (go label170)
           label480
            (setf exsm (/ 1.0d0 l))
            (if (>= mxordn nq) (go label484))
            (setf nqm1 mxordn)
            (setf lm1 (f2cl-lib:int-add mxordn 1))
            (setf exm1 (/ 1.0d0 lm1))
            (setf lm1p1 (f2cl-lib:int-add lm1 1))
            (setf dm1
                    (/
                     (dmnorm n
                      (f2cl-lib:array-slice yh-%data%
                                            double-float
                                            (1 lm1p1)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)
                      ewt)
                     (f2cl-lib:fref cm1 (mxordn) ((1 12)))))
            (setf rh1 (/ 1.0d0 (+ (* 1.2d0 (expt dm1 exm1)) 1.2d-6)))
            (go label486)
           label484
            (setf dm1
                    (* dsm
                       (/ (f2cl-lib:fref cm2 (nq) ((1 5)))
                          (f2cl-lib:fref cm1 (nq) ((1 12))))))
            (setf rh1 (/ 1.0d0 (+ (* 1.2d0 (expt dm1 exsm)) 1.2d-6)))
            (setf nqm1 nq)
            (setf exm1 exsm)
           label486
            (setf rh1it (* 2.0d0 rh1))
            (setf pdh (* pdnorm (abs h)))
            (if (> (* pdh rh1) 1.0d-5)
                (setf rh1it (/ (f2cl-lib:fref sm1 (nqm1) ((1 12))) pdh)))
            (setf rh1 (min rh1 rh1it))
            (setf rh2 (/ 1.0d0 (+ (* 1.2d0 (expt dsm exsm)) 1.2d-6)))
            (if (< (* rh1 ratio) (* 5.0d0 rh2)) (go label488))
            (setf alpha (max 0.001d0 rh1))
            (setf dm1 (* (expt alpha exm1) dm1))
            (if (<= dm1 (* 1000.0d0 uround pnorm)) (go label488))
            (setf rh rh1)
            (setf icount 20)
            (setf meth 1)
            (setf miter 0)
            (setf pdlast 0.0d0)
            (setf nq nqm1)
            (setf l (f2cl-lib:int-add nq 1))
            (go label170)
           label488
            (setf ialth (f2cl-lib:int-sub ialth 1))
            (if (= ialth 0) (go label520))
            (if (> ialth 1) (go label700))
            (if (= l lmax) (go label700))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label490
                (setf (f2cl-lib:fref yh-%data%
                                     (i lmax)
                                     ((1 nyh) (1 *))
                                     yh-%offset%)
                        (f2cl-lib:fref acor-%data%
                                       (i)
                                       ((1 *))
                                       acor-%offset%))))
            (go label700)
           label500
            (setf kflag (f2cl-lib:int-sub kflag 1))
            (setf tn told)
            (setf i1 (f2cl-lib:int-add nqnyh 1))
            (f2cl-lib:fdo (jb 1 (f2cl-lib:int-add jb 1))
                          ((> jb nq) nil)
              (tagbody
                (setf i1 (f2cl-lib:int-sub i1 nyh))
                (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                              ((> i nqnyh) nil)
                  (tagbody
                   label510
                    (setf (f2cl-lib:fref yh1-%data% (i) ((1 *)) yh1-%offset%)
                            (-
                             (f2cl-lib:fref yh1-%data%
                                            (i)
                                            ((1 *))
                                            yh1-%offset%)
                             (f2cl-lib:fref yh1-%data%
                                            ((f2cl-lib:int-add i nyh))
                                            ((1 *))
                                            yh1-%offset%)))))
               label515))
            (setf rmax 2.0d0)
            (if (<= (abs h) (* hmin 1.00001d0)) (go label660))
            (if (<= kflag -3) (go label640))
            (setf iredo 2)
            (setf rhup 0.0d0)
            (go label540)
           label520
            (setf rhup 0.0d0)
            (if (= l lmax) (go label540))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label530
                (setf (f2cl-lib:fref savf-%data% (i) ((1 *)) savf-%offset%)
                        (-
                         (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                         (f2cl-lib:fref yh-%data%
                                        (i lmax)
                                        ((1 nyh) (1 *))
                                        yh-%offset%)))))
            (setf dup
                    (/ (dmnorm n savf ewt)
                       (f2cl-lib:fref tesco (3 nq) ((1 3) (1 12)))))
            (setf exup (/ 1.0d0 (f2cl-lib:int-add l 1)))
            (setf rhup (/ 1.0d0 (+ (* 1.4d0 (expt dup exup)) 1.4d-6)))
           label540
            (setf exsm (/ 1.0d0 l))
            (setf rhsm (/ 1.0d0 (+ (* 1.2d0 (expt dsm exsm)) 1.2d-6)))
            (setf rhdn 0.0d0)
            (if (= nq 1) (go label550))
            (setf ddn
                    (/
                     (dmnorm n
                      (f2cl-lib:array-slice yh-%data%
                                            double-float
                                            (1 l)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)
                      ewt)
                     (f2cl-lib:fref tesco (1 nq) ((1 3) (1 12)))))
            (setf exdn (/ 1.0d0 nq))
            (setf rhdn (/ 1.0d0 (+ (* 1.3d0 (expt ddn exdn)) 1.3d-6)))
           label550
            (if (= meth 2) (go label560))
            (setf pdh (max (* (abs h) pdlast) 1.0d-6))
            (if (< l lmax)
                (setf rhup
                        (min rhup (/ (f2cl-lib:fref sm1 (l) ((1 12))) pdh))))
            (setf rhsm (min rhsm (/ (f2cl-lib:fref sm1 (nq) ((1 12))) pdh)))
            (if (> nq 1)
                (setf rhdn
                        (min rhdn
                             (/
                              (f2cl-lib:fref sm1
                                             ((f2cl-lib:int-sub nq 1))
                                             ((1 12)))
                              pdh))))
            (setf pdest 0.0d0)
           label560
            (if (>= rhsm rhup) (go label570))
            (if (> rhup rhdn) (go label590))
            (go label580)
           label570
            (if (< rhsm rhdn) (go label580))
            (setf newq nq)
            (setf rh rhsm)
            (go label620)
           label580
            (setf newq (f2cl-lib:int-sub nq 1))
            (setf rh rhdn)
            (if (and (< kflag 0) (> rh 1.0d0)) (setf rh 1.0d0))
            (go label620)
           label590
            (setf newq l)
            (setf rh rhup)
            (if (< rh 1.1d0) (go label610))
            (setf r (/ (f2cl-lib:fref el (l) ((1 13))) l))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label600
                (setf (f2cl-lib:fref yh-%data%
                                     (i (f2cl-lib:int-add newq 1))
                                     ((1 nyh) (1 *))
                                     yh-%offset%)
                        (*
                         (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                         r))))
            (go label630)
           label610
            (setf ialth 3)
            (go label700)
           label620
            (if (= meth 2) (go label622))
            (if (>= (* rh pdh 1.00001d0) (f2cl-lib:fref sm1 (newq) ((1 12))))
                (go label625))
           label622
            (if (and (= kflag 0) (< rh 1.1d0)) (go label610))
           label625
            (if (<= kflag -2) (setf rh (min rh 0.2d0)))
            (if (= newq nq) (go label170))
           label630
            (setf nq newq)
            (setf l (f2cl-lib:int-add nq 1))
            (setf iret 2)
            (go label150)
           label640
            (if (= kflag -10) (go label660))
            (setf rh 0.1d0)
            (setf rh (max (/ hmin (abs h)) rh))
            (setf h (* h rh))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label645
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (f2cl-lib:fref yh-%data%
                                       (i 1)
                                       ((1 nyh) (1 *))
                                       yh-%offset%))))
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall f neq tn y savf)
              (declare (ignore var-0 var-2 var-3))
              (when var-1
                (setf tn var-1)))
            (setf nfe (f2cl-lib:int-add nfe 1))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label650
                (setf (f2cl-lib:fref yh-%data%
                                     (i 2)
                                     ((1 nyh) (1 *))
                                     yh-%offset%)
                        (* h
                           (f2cl-lib:fref savf-%data%
                                          (i)
                                          ((1 *))
                                          savf-%offset%)))))
            (setf ipup miter)
            (setf ialth 5)
            (if (= nq 1) (go label200))
            (setf nq 1)
            (setf l 2)
            (setf iret 3)
            (go label150)
           label660
            (setf kflag -1)
            (go label720)
           label670
            (setf kflag -2)
            (go label720)
           label680
            (setf kflag -3)
            (go label720)
           label690
            (setf rmax 10.0d0)
           label700
            (setf r (/ 1.0d0 (f2cl-lib:fref tesco (2 nqu) ((1 3) (1 12)))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label710
                (setf (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                        (*
                         (f2cl-lib:fref acor-%data% (i) ((1 *)) acor-%offset%)
                         r))))
           label720
            (setf hold h)
            (setf jstart 1)
            (go end_label)
           end_label
            (return
             (values nil
                     nil
                     nil
                     nyh
                     nil
                     nil
                     nil
                     nil
                     nil
                     nil
                     nil
                     nil
                     nil
                     nil))))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dstoda
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t t t)
           :return-values '(nil nil nil fortran-to-lisp::nyh nil nil nil nil
                            nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dmnorm fortran-to-lisp::dcfode))))

