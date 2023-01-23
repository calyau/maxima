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


(let ((mord
       (make-array 2
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(12 5)))
      (mxstp0 500)
      (mxhnl0 10)
      (lenrat 2))
  (declare (type (array f2cl-lib:integer4 (2)) mord)
           (type (f2cl-lib:integer4) mxstp0 mxhnl0 lenrat))
  (defun dlsodes
         (f neq y t$ tout itol rtol atol itask istate iopt rwork lrw iwork liw
          jac mf)
    (declare (type (f2cl-lib:integer4) mf liw lrw iopt istate itask itol)
             (type (double-float) tout t$)
             (type (array double-float (*)) rwork atol rtol y)
             (type (array f2cl-lib:integer4 (*)) iwork neq))
    (let ()
      (symbol-macrolet ((ccmax
                         (aref (dls001-part-0 *dls001-common-block*) 209))
                        (h (aref (dls001-part-0 *dls001-common-block*) 211))
                        (hmin (aref (dls001-part-0 *dls001-common-block*) 212))
                        (hmxi (aref (dls001-part-0 *dls001-common-block*) 213))
                        (hu (aref (dls001-part-0 *dls001-common-block*) 214))
                        (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                        (uround
                         (aref (dls001-part-0 *dls001-common-block*) 217))
                        (init (aref (dls001-part-1 *dls001-common-block*) 0))
                        (mxstep (aref (dls001-part-1 *dls001-common-block*) 1))
                        (mxhnil (aref (dls001-part-1 *dls001-common-block*) 2))
                        (nhnil (aref (dls001-part-1 *dls001-common-block*) 3))
                        (nslast (aref (dls001-part-1 *dls001-common-block*) 4))
                        (nyh (aref (dls001-part-1 *dls001-common-block*) 5))
                        (jstart
                         (aref (dls001-part-1 *dls001-common-block*) 16))
                        (kflag (aref (dls001-part-1 *dls001-common-block*) 17))
                        (l (aref (dls001-part-1 *dls001-common-block*) 18))
                        (lyh (aref (dls001-part-1 *dls001-common-block*) 19))
                        (lewt (aref (dls001-part-1 *dls001-common-block*) 20))
                        (lacor (aref (dls001-part-1 *dls001-common-block*) 21))
                        (lsavf (aref (dls001-part-1 *dls001-common-block*) 22))
                        (lwm (aref (dls001-part-1 *dls001-common-block*) 23))
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
                        (nje (aref (dls001-part-1 *dls001-common-block*) 35))
                        (nqu (aref (dls001-part-1 *dls001-common-block*) 36))
                        (ccmxj (aref (dlss01-part-0 *dlss01-common-block*) 2))
                        (psmall (aref (dlss01-part-0 *dlss01-common-block*) 3))
                        (rbig (aref (dlss01-part-0 *dlss01-common-block*) 4))
                        (seth (aref (dlss01-part-0 *dlss01-common-block*) 5))
                        (istatc (aref (dlss01-part-1 *dlss01-common-block*) 2))
                        (iys (aref (dlss01-part-1 *dlss01-common-block*) 3))
                        (ipian (aref (dlss01-part-1 *dlss01-common-block*) 8))
                        (ipjan (aref (dlss01-part-1 *dlss01-common-block*) 9))
                        (lenyh (aref (dlss01-part-1 *dlss01-common-block*) 18))
                        (lenyhm
                         (aref (dlss01-part-1 *dlss01-common-block*) 19))
                        (lenwk (aref (dlss01-part-1 *dlss01-common-block*) 20))
                        (lrat (aref (dlss01-part-1 *dlss01-common-block*) 22))
                        (lrest (aref (dlss01-part-1 *dlss01-common-block*) 23))
                        (lwmin (aref (dlss01-part-1 *dlss01-common-block*) 24))
                        (moss (aref (dlss01-part-1 *dlss01-common-block*) 25))
                        (msbj (aref (dlss01-part-1 *dlss01-common-block*) 26))
                        (nslj (aref (dlss01-part-1 *dlss01-common-block*) 27))
                        (ngp (aref (dlss01-part-1 *dlss01-common-block*) 28))
                        (nlu (aref (dlss01-part-1 *dlss01-common-block*) 29))
                        (nnz (aref (dlss01-part-1 *dlss01-common-block*) 30))
                        (nzl (aref (dlss01-part-1 *dlss01-common-block*) 32))
                        (nzu (aref (dlss01-part-1 *dlss01-common-block*) 33)))
        (f2cl-lib:with-multi-array-data
            ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
             (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%)
             (y double-float y-%data% y-%offset%)
             (rtol double-float rtol-%data% rtol-%offset%)
             (atol double-float atol-%data% atol-%offset%)
             (rwork double-float rwork-%data% rwork-%offset%))
          (prog ((ncolm 0) (mf1 0) (lyhn 0) (lyhd 0) (lwtem 0) (lrtem 0)
                 (lja 0) (lia 0) (lf0 0) (lenrw 0) (leniw 0) (lenyht 0) (kgo 0)
                 (j 0) (irem 0) (ipgo 0) (ipflag 0) (imxer 0) (imul 0) (imax 0)
                 (iflag 0) (i2 0) (i1 0) (i 0) (w0 0.0d0) (sum 0.0d0)
                 (size 0.0d0) (tp 0.0d0) (tolsf 0.0d0) (tol 0.0d0)
                 (tnext 0.0d0) (tdist 0.0d0) (tcrit 0.0d0) (rtoli 0.0d0)
                 (rh 0.0d0) (hmx 0.0d0) (hmax 0.0d0) (h0 0.0d0) (ewti 0.0d0)
                 (big 0.0d0) (ayi 0.0d0) (atoli 0.0d0) (ihit nil)
                 (msg
                  (make-array '(60)
                              :element-type 'character
                              :initial-element #\ )))
            (declare (type (string 60) msg)
                     (type f2cl-lib:logical ihit)
                     (type (double-float) atoli ayi big ewti h0 hmax hmx rh
                                          rtoli tcrit tdist tnext tol tolsf tp
                                          size sum w0)
                     (type (f2cl-lib:integer4) i i1 i2 iflag imax imul imxer
                                               ipflag ipgo irem j kgo lenyht
                                               leniw lenrw lf0 lia lja lrtem
                                               lwtem lyhd lyhn mf1 ncolm))
            (if (or (< istate 1) (> istate 3)) (go label601))
            (if (or (< itask 1) (> itask 5)) (go label602))
            (if (= istate 1) (go label10))
            (if (= init 0) (go label603))
            (if (= istate 2) (go label200))
            (go label20)
           label10
            (setf init 0)
            (if (= tout t$) (go end_label))
           label20
            (if (<= (f2cl-lib:fref neq-%data% (1) ((1 *)) neq-%offset%) 0)
                (go label604))
            (if (= istate 1) (go label25))
            (if (> (f2cl-lib:fref neq-%data% (1) ((1 *)) neq-%offset%) n)
                (go label605))
           label25
            (setf n (f2cl-lib:fref neq-%data% (1) ((1 *)) neq-%offset%))
            (if (or (< itol 1) (> itol 4)) (go label606))
            (if (or (< iopt 0) (> iopt 1)) (go label607))
            (setf moss (the f2cl-lib:integer4 (truncate mf 100)))
            (setf mf1 (f2cl-lib:int-sub mf (f2cl-lib:int-mul 100 moss)))
            (setf meth (the f2cl-lib:integer4 (truncate mf1 10)))
            (setf miter (f2cl-lib:int-sub mf1 (f2cl-lib:int-mul 10 meth)))
            (if (or (< moss 0) (> moss 2)) (go label608))
            (if (or (< meth 1) (> meth 2)) (go label608))
            (if (or (< miter 0) (> miter 3)) (go label608))
            (if (or (= miter 0) (= miter 3)) (setf moss 0))
            (if (= iopt 1) (go label40))
            (setf maxord (f2cl-lib:fref mord (meth) ((1 2))))
            (setf mxstep mxstp0)
            (setf mxhnil mxhnl0)
            (if (= istate 1) (setf h0 0.0d0))
            (setf hmxi 0.0d0)
            (setf hmin 0.0d0)
            (setf seth 0.0d0)
            (go label60)
           label40
            (setf maxord
                    (f2cl-lib:fref iwork-%data% (5) ((1 liw)) iwork-%offset%))
            (if (< maxord 0) (go label611))
            (if (= maxord 0) (setf maxord 100))
            (setf maxord
                    (min (the f2cl-lib:integer4 maxord)
                         (the f2cl-lib:integer4
                              (f2cl-lib:fref mord (meth) ((1 2))))))
            (setf mxstep
                    (f2cl-lib:fref iwork-%data% (6) ((1 liw)) iwork-%offset%))
            (if (< mxstep 0) (go label612))
            (if (= mxstep 0) (setf mxstep mxstp0))
            (setf mxhnil
                    (f2cl-lib:fref iwork-%data% (7) ((1 liw)) iwork-%offset%))
            (if (< mxhnil 0) (go label613))
            (if (= mxhnil 0) (setf mxhnil mxhnl0))
            (if (/= istate 1) (go label50))
            (setf h0 (f2cl-lib:fref rwork-%data% (5) ((1 lrw)) rwork-%offset%))
            (if (< (* (- tout t$) h0) 0.0d0) (go label614))
           label50
            (setf hmax
                    (f2cl-lib:fref rwork-%data% (6) ((1 lrw)) rwork-%offset%))
            (if (< hmax 0.0d0) (go label615))
            (setf hmxi 0.0d0)
            (if (> hmax 0.0d0) (setf hmxi (/ 1.0d0 hmax)))
            (setf hmin
                    (f2cl-lib:fref rwork-%data% (7) ((1 lrw)) rwork-%offset%))
            (if (< hmin 0.0d0) (go label616))
            (setf seth
                    (f2cl-lib:fref rwork-%data% (8) ((1 lrw)) rwork-%offset%))
            (if (< seth 0.0d0) (go label609))
           label60
            (setf rtoli (f2cl-lib:fref rtol-%data% (1) ((1 *)) rtol-%offset%))
            (setf atoli (f2cl-lib:fref atol-%data% (1) ((1 *)) atol-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (if (>= itol 3)
                    (setf rtoli
                            (f2cl-lib:fref rtol-%data%
                                           (i)
                                           ((1 *))
                                           rtol-%offset%)))
                (if (or (= itol 2) (= itol 4))
                    (setf atoli
                            (f2cl-lib:fref atol-%data%
                                           (i)
                                           ((1 *))
                                           atol-%offset%)))
                (if (< rtoli 0.0d0) (go label619))
                (if (< atoli 0.0d0) (go label620))
               label65))
            (setf lrat lenrat)
            (if (= istate 1) (setf nyh n))
            (setf lwmin 0)
            (if (= miter 1)
                (setf lwmin
                        (+ (f2cl-lib:int-mul 4 n)
                           (the f2cl-lib:integer4 (truncate (* 10 n) lrat)))))
            (if (= miter 2)
                (setf lwmin
                        (+ (f2cl-lib:int-mul 4 n)
                           (the f2cl-lib:integer4 (truncate (* 11 n) lrat)))))
            (if (= miter 3) (setf lwmin (f2cl-lib:int-add n 2)))
            (setf lenyh (f2cl-lib:int-mul (f2cl-lib:int-add maxord 1) nyh))
            (setf lrest (f2cl-lib:int-add lenyh (f2cl-lib:int-mul 3 n)))
            (setf lenrw (f2cl-lib:int-add 20 lwmin lrest))
            (setf (f2cl-lib:fref iwork-%data% (17) ((1 liw)) iwork-%offset%)
                    lenrw)
            (setf leniw 30)
            (if (and (= moss 0) (/= miter 0) (/= miter 3))
                (setf leniw (f2cl-lib:int-add leniw n 1)))
            (setf (f2cl-lib:fref iwork-%data% (18) ((1 liw)) iwork-%offset%)
                    leniw)
            (if (> lenrw lrw) (go label617))
            (if (> leniw liw) (go label618))
            (setf lia 31)
            (if (and (= moss 0) (/= miter 0) (/= miter 3))
                (setf leniw
                        (f2cl-lib:int-sub
                         (f2cl-lib:int-add leniw
                                           (f2cl-lib:fref iwork-%data%
                                                          ((f2cl-lib:int-add
                                                            lia
                                                            n))
                                                          ((1 liw))
                                                          iwork-%offset%))
                         1)))
            (setf (f2cl-lib:fref iwork-%data% (18) ((1 liw)) iwork-%offset%)
                    leniw)
            (if (> leniw liw) (go label618))
            (setf lja (f2cl-lib:int-add lia n 1))
            (setf lia
                    (min (the f2cl-lib:integer4 lia)
                         (the f2cl-lib:integer4 liw)))
            (setf lja
                    (min (the f2cl-lib:integer4 lja)
                         (the f2cl-lib:integer4 liw)))
            (setf lwm 21)
            (if (= istate 1) (setf nq 1))
            (setf ncolm
                    (min (the f2cl-lib:integer4 (f2cl-lib:int-add nq 1))
                         (the f2cl-lib:integer4 (f2cl-lib:int-add maxord 2))))
            (setf lenyhm (f2cl-lib:int-mul ncolm nyh))
            (setf lenyht lenyh)
            (if (or (= miter 1) (= miter 2)) (setf lenyht lenyhm))
            (setf imul 2)
            (if (= istate 3) (setf imul moss))
            (if (= moss 2) (setf imul 3))
            (setf lrtem (f2cl-lib:int-add lenyht (f2cl-lib:int-mul imul n)))
            (setf lwtem lwmin)
            (if (or (= miter 1) (= miter 2))
                (setf lwtem (f2cl-lib:int-sub lrw 20 lrtem)))
            (setf lenwk lwtem)
            (setf lyhn (f2cl-lib:int-add lwm lwtem))
            (setf lsavf (f2cl-lib:int-add lyhn lenyht))
            (setf lewt (f2cl-lib:int-add lsavf n))
            (setf lacor (f2cl-lib:int-add lewt n))
            (setf istatc istate)
            (if (= istate 1) (go label100))
            (setf lyhd (f2cl-lib:int-sub lyh lyhn))
            (setf imax (f2cl-lib:int-add (f2cl-lib:int-sub lyhn 1) lenyhm))
            (cond
              ((< lyhd 0)
               (f2cl-lib:fdo (i lyhn (f2cl-lib:int-add i 1))
                             ((> i imax) nil)
                 (tagbody
                   (setf j (f2cl-lib:int-sub (f2cl-lib:int-add imax lyhn) i))
                  label72
                   (setf (f2cl-lib:fref rwork-%data%
                                        (j)
                                        ((1 lrw))
                                        rwork-%offset%)
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-add j lyhd))
                                          ((1 lrw))
                                          rwork-%offset%))))))
            (cond
              ((> lyhd 0)
               (f2cl-lib:fdo (i lyhn (f2cl-lib:int-add i 1))
                             ((> i imax) nil)
                 (tagbody
                  label76
                   (setf (f2cl-lib:fref rwork-%data%
                                        (i)
                                        ((1 lrw))
                                        rwork-%offset%)
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-add i lyhd))
                                          ((1 lrw))
                                          rwork-%offset%))))))
           label80
            (setf lyh lyhn)
            (setf (f2cl-lib:fref iwork-%data% (22) ((1 liw)) iwork-%offset%)
                    lyh)
            (if (or (= miter 0) (= miter 3)) (go label92))
            (if (/= moss 2) (go label85))
            (dewset n itol rtol atol
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lyh)
                                   ((1 lrw))
                                   rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lewt)
                                   ((1 lrw))
                                   rwork-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (if
                 (<=
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt)
                                                    1))
                                 ((1 lrw))
                                 rwork-%offset%)
                  0.0d0)
                 (go label621))
               label82
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lewt)
                                       1))
                                     ((1 lrw))
                                     rwork-%offset%)
                        (/ 1.0d0
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add i lewt)
                                            1))
                                          ((1 lrw))
                                          rwork-%offset%)))))
           label85
            (setf lsavf
                    (min (the f2cl-lib:integer4 lsavf)
                         (the f2cl-lib:integer4 lrw)))
            (setf lewt
                    (min (the f2cl-lib:integer4 lewt)
                         (the f2cl-lib:integer4 lrw)))
            (setf lacor
                    (min (the f2cl-lib:integer4 lacor)
                         (the f2cl-lib:integer4 lrw)))
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                (diprep neq y rwork
                 (f2cl-lib:array-slice iwork-%data%
                                       f2cl-lib:integer4
                                       (lia)
                                       ((1 liw))
                                       iwork-%offset%)
                 (f2cl-lib:array-slice iwork-%data%
                                       f2cl-lib:integer4
                                       (lja)
                                       ((1 liw))
                                       iwork-%offset%)
                 ipflag f jac)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-6 var-7))
              (setf ipflag var-5))
            (setf lenrw
                    (f2cl-lib:int-add (f2cl-lib:int-sub lwm 1) lenwk lrest))
            (setf (f2cl-lib:fref iwork-%data% (17) ((1 liw)) iwork-%offset%)
                    lenrw)
            (if (/= ipflag -1)
                (setf (f2cl-lib:fref iwork-%data%
                                     (23)
                                     ((1 liw))
                                     iwork-%offset%)
                        ipian))
            (if (/= ipflag -1)
                (setf (f2cl-lib:fref iwork-%data%
                                     (24)
                                     ((1 liw))
                                     iwork-%offset%)
                        ipjan))
            (setf ipgo (f2cl-lib:int-sub 1 ipflag))
            (f2cl-lib:computed-goto
             (label90 label628 label629 label630 label631 label632 label633)
             ipgo)
           label90
            (setf (f2cl-lib:fref iwork-%data% (22) ((1 liw)) iwork-%offset%)
                    lyh)
            (if (> lenrw lrw) (go label617))
           label92
            (setf jstart -1)
            (if (= n nyh) (go label200))
            (setf i1 (f2cl-lib:int-add lyh (f2cl-lib:int-mul l nyh)))
            (setf i2
                    (f2cl-lib:int-sub
                     (f2cl-lib:int-add lyh
                                       (f2cl-lib:int-mul
                                        (f2cl-lib:int-add maxord 1)
                                        nyh))
                     1))
            (if (> i1 i2) (go label200))
            (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                          ((> i i2) nil)
              (tagbody
               label95
                (setf (f2cl-lib:fref rwork-%data% (i) ((1 lrw)) rwork-%offset%)
                        0.0d0)))
            (go label200)
           label100
            (setf lyh lyhn)
            (setf (f2cl-lib:fref iwork-%data% (22) ((1 liw)) iwork-%offset%)
                    lyh)
            (setf tn t$)
            (setf nst 0)
            (setf h 1.0d0)
            (setf nnz 0)
            (setf ngp 0)
            (setf nzl 0)
            (setf nzu 0)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label105
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lyh)
                                       1))
                                     ((1 lrw))
                                     rwork-%offset%)
                        (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%))))
            (setf lf0 (f2cl-lib:int-add lyh nyh))
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall f
                         neq
                         t$
                         y
                         (f2cl-lib:array-slice rwork-%data%
                                               double-float
                                               (lf0)
                                               ((1 lrw))
                                               rwork-%offset%))
              (declare (ignore var-0 var-2 var-3))
              (when var-1
                (setf t$ var-1)))
            (setf nfe 1)
            (dewset n itol rtol atol
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lyh)
                                   ((1 lrw))
                                   rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lewt)
                                   ((1 lrw))
                                   rwork-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (if
                 (<=
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt)
                                                    1))
                                 ((1 lrw))
                                 rwork-%offset%)
                  0.0d0)
                 (go label621))
               label110
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lewt)
                                       1))
                                     ((1 lrw))
                                     rwork-%offset%)
                        (/ 1.0d0
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add i lewt)
                                            1))
                                          ((1 lrw))
                                          rwork-%offset%)))))
            (if (or (= miter 0) (= miter 3)) (go label120))
            (setf lacor
                    (min (the f2cl-lib:integer4 lacor)
                         (the f2cl-lib:integer4 lrw)))
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                (diprep neq y rwork
                 (f2cl-lib:array-slice iwork-%data%
                                       f2cl-lib:integer4
                                       (lia)
                                       ((1 liw))
                                       iwork-%offset%)
                 (f2cl-lib:array-slice iwork-%data%
                                       f2cl-lib:integer4
                                       (lja)
                                       ((1 liw))
                                       iwork-%offset%)
                 ipflag f jac)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-6 var-7))
              (setf ipflag var-5))
            (setf lenrw
                    (f2cl-lib:int-add (f2cl-lib:int-sub lwm 1) lenwk lrest))
            (setf (f2cl-lib:fref iwork-%data% (17) ((1 liw)) iwork-%offset%)
                    lenrw)
            (if (/= ipflag -1)
                (setf (f2cl-lib:fref iwork-%data%
                                     (23)
                                     ((1 liw))
                                     iwork-%offset%)
                        ipian))
            (if (/= ipflag -1)
                (setf (f2cl-lib:fref iwork-%data%
                                     (24)
                                     ((1 liw))
                                     iwork-%offset%)
                        ipjan))
            (setf ipgo (f2cl-lib:int-sub 1 ipflag))
            (f2cl-lib:computed-goto
             (label115 label628 label629 label630 label631 label632 label633)
             ipgo)
           label115
            (setf (f2cl-lib:fref iwork-%data% (22) ((1 liw)) iwork-%offset%)
                    lyh)
            (if (> lenrw lrw) (go label617))
           label120
            (if (and (/= itask 4) (/= itask 5)) (go label125))
            (setf tcrit
                    (f2cl-lib:fref rwork-%data% (1) ((1 lrw)) rwork-%offset%))
            (if (< (* (- tcrit tout) (- tout t$)) 0.0d0) (go label625))
            (if (and (/= h0 0.0d0) (> (* (- (+ t$ h0) tcrit) h0) 0.0d0))
                (setf h0 (- tcrit t$)))
           label125
            (setf uround (dumach))
            (setf jstart 0)
            (if (/= miter 0)
                (setf (f2cl-lib:fref rwork-%data%
                                     (lwm)
                                     ((1 lrw))
                                     rwork-%offset%)
                        (f2cl-lib:fsqrt uround)))
            (setf msbj 50)
            (setf nslj 0)
            (setf ccmxj 0.2d0)
            (setf psmall (* 1000.0d0 uround))
            (setf rbig (/ 0.01d0 psmall))
            (setf nhnil 0)
            (setf nje 0)
            (setf nlu 0)
            (setf nslast 0)
            (setf hu 0.0d0)
            (setf nqu 0)
            (setf ccmax 0.3d0)
            (setf maxcor 3)
            (setf msbp 20)
            (setf mxncf 10)
            (setf lf0 (f2cl-lib:int-add lyh nyh))
            (if (/= h0 0.0d0) (go label180))
            (setf tdist (abs (- tout t$)))
            (setf w0 (max (abs t$) (abs tout)))
            (if (< tdist (* 2.0d0 uround w0)) (go label622))
            (setf tol (f2cl-lib:fref rtol-%data% (1) ((1 *)) rtol-%offset%))
            (if (<= itol 2) (go label140))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label130
                (setf tol
                        (max tol
                             (f2cl-lib:fref rtol-%data%
                                            (i)
                                            ((1 *))
                                            rtol-%offset%)))))
           label140
            (if (> tol 0.0d0) (go label160))
            (setf atoli (f2cl-lib:fref atol-%data% (1) ((1 *)) atol-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (if (or (= itol 2) (= itol 4))
                    (setf atoli
                            (f2cl-lib:fref atol-%data%
                                           (i)
                                           ((1 *))
                                           atol-%offset%)))
                (setf ayi
                        (abs (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)))
                (if (/= ayi 0.0d0) (setf tol (max tol (/ atoli ayi))))
               label150))
           label160
            (setf tol (max tol (* 100.0d0 uround)))
            (setf tol (min tol 0.001d0))
            (setf sum
                    (dvnorm n
                     (f2cl-lib:array-slice rwork-%data%
                                           double-float
                                           (lf0)
                                           ((1 lrw))
                                           rwork-%offset%)
                     (f2cl-lib:array-slice rwork-%data%
                                           double-float
                                           (lewt)
                                           ((1 lrw))
                                           rwork-%offset%)))
            (setf sum (+ (/ 1.0d0 (* tol w0 w0)) (* tol (expt sum 2))))
            (setf h0 (/ 1.0d0 (f2cl-lib:fsqrt sum)))
            (setf h0 (min h0 tdist))
            (setf h0 (f2cl-lib:sign h0 (- tout t$)))
           label180
            (setf rh (* (abs h0) hmxi))
            (if (> rh 1.0d0) (setf h0 (/ h0 rh)))
            (setf h h0)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label190
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lf0)
                                       1))
                                     ((1 lrw))
                                     rwork-%offset%)
                        (* h0
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add i lf0)
                                            1))
                                          ((1 lrw))
                                          rwork-%offset%)))))
            (go label270)
           label200
            (setf nslast nst)
            (f2cl-lib:computed-goto
             (label210 label250 label220 label230 label240)
             itask)
           label210
            (if (< (* (- tn tout) h) 0.0d0) (go label250))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dintdy tout 0
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 nyh y iflag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4))
              (setf iflag var-5))
            (if (/= iflag 0) (go label627))
            (setf t$ tout)
            (go label420)
           label220
            (setf tp (- tn (* hu (+ 1.0d0 (* 100.0d0 uround)))))
            (if (> (* (- tp tout) h) 0.0d0) (go label623))
            (if (< (* (- tn tout) h) 0.0d0) (go label250))
            (go label400)
           label230
            (setf tcrit
                    (f2cl-lib:fref rwork-%data% (1) ((1 lrw)) rwork-%offset%))
            (if (> (* (- tn tcrit) h) 0.0d0) (go label624))
            (if (< (* (- tcrit tout) h) 0.0d0) (go label625))
            (if (< (* (- tn tout) h) 0.0d0) (go label245))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dintdy tout 0
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 nyh y iflag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4))
              (setf iflag var-5))
            (if (/= iflag 0) (go label627))
            (setf t$ tout)
            (go label420)
           label240
            (setf tcrit
                    (f2cl-lib:fref rwork-%data% (1) ((1 lrw)) rwork-%offset%))
            (if (> (* (- tn tcrit) h) 0.0d0) (go label624))
           label245
            (setf hmx (+ (abs tn) (abs h)))
            (setf ihit (<= (abs (- tn tcrit)) (* 100.0d0 uround hmx)))
            (if ihit (go label400))
            (setf tnext (+ tn (* h (+ 1.0d0 (* 4.0d0 uround)))))
            (if (<= (* (- tnext tcrit) h) 0.0d0) (go label250))
            (setf h (* (- tcrit tn) (- 1.0d0 (* 4.0d0 uround))))
            (if (= istate 2) (setf jstart -2))
           label250
            (if (>= (f2cl-lib:int-sub nst nslast) mxstep) (go label500))
            (dewset n itol rtol atol
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lyh)
                                   ((1 lrw))
                                   rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (lewt)
                                   ((1 lrw))
                                   rwork-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (if
                 (<=
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt)
                                                    1))
                                 ((1 lrw))
                                 rwork-%offset%)
                  0.0d0)
                 (go label510))
               label260
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lewt)
                                       1))
                                     ((1 lrw))
                                     rwork-%offset%)
                        (/ 1.0d0
                           (f2cl-lib:fref rwork-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add i lewt)
                                            1))
                                          ((1 lrw))
                                          rwork-%offset%)))))
           label270
            (setf tolsf
                    (* uround
                       (dvnorm n
                        (f2cl-lib:array-slice rwork-%data%
                                              double-float
                                              (lyh)
                                              ((1 lrw))
                                              rwork-%offset%)
                        (f2cl-lib:array-slice rwork-%data%
                                              double-float
                                              (lewt)
                                              ((1 lrw))
                                              rwork-%offset%))))
            (if (<= tolsf 1.0d0) (go label280))
            (setf tolsf (* tolsf 2.0d0))
            (if (= nst 0) (go label626))
            (go label520)
           label280
            (if (/= (+ tn h) tn) (go label290))
            (setf nhnil (f2cl-lib:int-add nhnil 1))
            (if (> nhnil mxhnil) (go label290))
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Warning..Internal T (=R1) and H (=R2) are"
                                      (string 60))
            (xerrwd msg 50 101 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      such that in the machine, T + H = T on the next step  "
                                      (string 60))
            (xerrwd msg 60 101 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "     (H = step size). Solver will continue anyway."
                                      (string 60))
            (xerrwd msg 50 101 0 0 0 0 2 tn h)
            (if (< nhnil mxhnil) (go label290))
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Above warning has been issued I1 times.  "
                                      (string 60))
            (xerrwd msg 50 102 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "     It will not be issued again for this problem."
                                      (string 60))
            (xerrwd msg 50 102 0 1 mxhnil 0 0 0.0d0 0.0d0)
           label290
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13)
                (dstode neq y
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 nyh
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lewt)
                                       ((1 lrw))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lsavf)
                                       ((1 lrw))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lacor)
                                       ((1 lrw))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lwm)
                                       ((1 lrw))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lwm)
                                       ((1 lrw))
                                       rwork-%offset%)
                 f jac #'dprjs #'dsolss)
              (declare (ignore var-0 var-1 var-2 var-4 var-5 var-6 var-7 var-8
                               var-9 var-10 var-11 var-12 var-13))
              (setf nyh var-3))
            (setf kgo (f2cl-lib:int-sub 1 kflag))
            (f2cl-lib:computed-goto (label300 label530 label540 label550) kgo)
           label300
            (setf init 1)
            (f2cl-lib:computed-goto
             (label310 label400 label330 label340 label350)
             itask)
           label310
            (if (< (* (- tn tout) h) 0.0d0) (go label250))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dintdy tout 0
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 nyh y iflag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4))
              (setf iflag var-5))
            (setf t$ tout)
            (go label420)
           label330
            (if (>= (* (- tn tout) h) 0.0d0) (go label400))
            (go label250)
           label340
            (if (< (* (- tn tout) h) 0.0d0) (go label345))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dintdy tout 0
                 (f2cl-lib:array-slice rwork-%data%
                                       double-float
                                       (lyh)
                                       ((1 lrw))
                                       rwork-%offset%)
                 nyh y iflag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4))
              (setf iflag var-5))
            (setf t$ tout)
            (go label420)
           label345
            (setf hmx (+ (abs tn) (abs h)))
            (setf ihit (<= (abs (- tn tcrit)) (* 100.0d0 uround hmx)))
            (if ihit (go label400))
            (setf tnext (+ tn (* h (+ 1.0d0 (* 4.0d0 uround)))))
            (if (<= (* (- tnext tcrit) h) 0.0d0) (go label250))
            (setf h (* (- tcrit tn) (- 1.0d0 (* 4.0d0 uround))))
            (setf jstart -2)
            (go label250)
           label350
            (setf hmx (+ (abs tn) (abs h)))
            (setf ihit (<= (abs (- tn tcrit)) (* 100.0d0 uround hmx)))
           label400
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label410
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (f2cl-lib:fref rwork-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add i lyh)
                                         1))
                                       ((1 lrw))
                                       rwork-%offset%))))
            (setf t$ tn)
            (if (and (/= itask 4) (/= itask 5)) (go label420))
            (if ihit (setf t$ tcrit))
           label420
            (setf istate 2)
            (setf (f2cl-lib:fref rwork-%data% (11) ((1 lrw)) rwork-%offset%)
                    hu)
            (setf (f2cl-lib:fref rwork-%data% (12) ((1 lrw)) rwork-%offset%) h)
            (setf (f2cl-lib:fref rwork-%data% (13) ((1 lrw)) rwork-%offset%)
                    tn)
            (setf (f2cl-lib:fref iwork-%data% (11) ((1 liw)) iwork-%offset%)
                    nst)
            (setf (f2cl-lib:fref iwork-%data% (12) ((1 liw)) iwork-%offset%)
                    nfe)
            (setf (f2cl-lib:fref iwork-%data% (13) ((1 liw)) iwork-%offset%)
                    nje)
            (setf (f2cl-lib:fref iwork-%data% (14) ((1 liw)) iwork-%offset%)
                    nqu)
            (setf (f2cl-lib:fref iwork-%data% (15) ((1 liw)) iwork-%offset%)
                    nq)
            (setf (f2cl-lib:fref iwork-%data% (19) ((1 liw)) iwork-%offset%)
                    nnz)
            (setf (f2cl-lib:fref iwork-%data% (20) ((1 liw)) iwork-%offset%)
                    ngp)
            (setf (f2cl-lib:fref iwork-%data% (21) ((1 liw)) iwork-%offset%)
                    nlu)
            (setf (f2cl-lib:fref iwork-%data% (25) ((1 liw)) iwork-%offset%)
                    nzl)
            (setf (f2cl-lib:fref iwork-%data% (26) ((1 liw)) iwork-%offset%)
                    nzu)
            (go end_label)
           label500
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At current T (=R1), MXSTEP (=I1) steps   "
                                      (string 60))
            (xerrwd msg 50 201 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      taken on this call before reaching TOUT     "
                                      (string 60))
            (xerrwd msg 50 201 0 1 mxstep 0 1 tn 0.0d0)
            (setf istate -1)
            (go label580)
           label510
            (setf ewti
                    (f2cl-lib:fref rwork-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add lewt i)
                                                      1))
                                   ((1 lrw))
                                   rwork-%offset%))
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At T (=R1), EWT(I1) has become R2  <=  0."
                                      (string 60))
            (xerrwd msg 50 202 0 1 i 0 2 tn ewti)
            (setf istate -6)
            (go label580)
           label520
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At T (=R1), too much accuracy requested  "
                                      (string 60))
            (xerrwd msg 50 203 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      for precision of machine..  See TOLSF (=R2) "
                                      (string 60))
            (xerrwd msg 50 203 0 0 0 0 2 tn tolsf)
            (setf (f2cl-lib:fref rwork-%data% (14) ((1 lrw)) rwork-%offset%)
                    tolsf)
            (setf istate -2)
            (go label580)
           label530
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At T(=R1) and step size H(=R2), the error"
                                      (string 60))
            (xerrwd msg 50 204 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      test failed repeatedly or with ABS(H) = HMIN"
                                      (string 60))
            (xerrwd msg 50 204 0 0 0 0 2 tn h)
            (setf istate -4)
            (go label560)
           label540
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At T (=R1) and step size H (=R2), the    "
                                      (string 60))
            (xerrwd msg 50 205 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      corrector convergence failed repeatedly     "
                                      (string 60))
            (xerrwd msg 50 205 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      or with ABS(H) = HMIN   "
                                      (string 60))
            (xerrwd msg 30 205 0 0 0 0 2 tn h)
            (setf istate -5)
            (go label560)
           label550
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At T (=R1) and step size H (=R2), a fatal"
                                      (string 60))
            (xerrwd msg 50 207 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      error flag was returned by CDRV (by way of  "
                                      (string 60))
            (xerrwd msg 50 207 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      Subroutine DPRJS or DSOLSS)       "
                                      (string 60))
            (xerrwd msg 40 207 0 0 0 0 2 tn h)
            (setf istate -7)
            (go label580)
           label560
            (setf big 0.0d0)
            (setf imxer 1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf size
                        (abs
                         (*
                          (f2cl-lib:fref rwork-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add i lacor)
                                           1))
                                         ((1 lrw))
                                         rwork-%offset%)
                          (f2cl-lib:fref rwork-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add i lewt)
                                           1))
                                         ((1 lrw))
                                         rwork-%offset%))))
                (if (>= big size) (go label570))
                (setf big size)
                (setf imxer i)
               label570))
            (setf (f2cl-lib:fref iwork-%data% (16) ((1 liw)) iwork-%offset%)
                    imxer)
           label580
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label590
                (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                        (f2cl-lib:fref rwork-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add i lyh)
                                         1))
                                       ((1 lrw))
                                       rwork-%offset%))))
            (setf t$ tn)
            (setf (f2cl-lib:fref rwork-%data% (11) ((1 lrw)) rwork-%offset%)
                    hu)
            (setf (f2cl-lib:fref rwork-%data% (12) ((1 lrw)) rwork-%offset%) h)
            (setf (f2cl-lib:fref rwork-%data% (13) ((1 lrw)) rwork-%offset%)
                    tn)
            (setf (f2cl-lib:fref iwork-%data% (11) ((1 liw)) iwork-%offset%)
                    nst)
            (setf (f2cl-lib:fref iwork-%data% (12) ((1 liw)) iwork-%offset%)
                    nfe)
            (setf (f2cl-lib:fref iwork-%data% (13) ((1 liw)) iwork-%offset%)
                    nje)
            (setf (f2cl-lib:fref iwork-%data% (14) ((1 liw)) iwork-%offset%)
                    nqu)
            (setf (f2cl-lib:fref iwork-%data% (15) ((1 liw)) iwork-%offset%)
                    nq)
            (setf (f2cl-lib:fref iwork-%data% (19) ((1 liw)) iwork-%offset%)
                    nnz)
            (setf (f2cl-lib:fref iwork-%data% (20) ((1 liw)) iwork-%offset%)
                    ngp)
            (setf (f2cl-lib:fref iwork-%data% (21) ((1 liw)) iwork-%offset%)
                    nlu)
            (setf (f2cl-lib:fref iwork-%data% (25) ((1 liw)) iwork-%offset%)
                    nzl)
            (setf (f2cl-lib:fref iwork-%data% (26) ((1 liw)) iwork-%offset%)
                    nzu)
            (go end_label)
           label601
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ISTATE (=I1) illegal."
                                      (string 60))
            (xerrwd msg 30 1 0 1 istate 0 0 0.0d0 0.0d0)
            (if (< istate 0) (go label800))
            (go label700)
           label602
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ITASK (=I1) illegal. "
                                      (string 60))
            (xerrwd msg 30 2 0 1 itask 0 0 0.0d0 0.0d0)
            (go label700)
           label603
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ISTATE > 1 but DLSODES not initialized. "
                                      (string 60))
            (xerrwd msg 50 3 0 0 0 0 0 0.0d0 0.0d0)
            (go label700)
           label604
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- NEQ (=I1)  <  1     "
                                      (string 60))
            (xerrwd msg 30 4 0 1
             (f2cl-lib:fref neq-%data% (1) ((1 *)) neq-%offset%) 0 0 0.0d0
             0.0d0)
            (go label700)
           label605
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ISTATE = 3 and NEQ increased (I1 to I2). "
                                      (string 60))
            (xerrwd msg 50 5 0 2 n
             (f2cl-lib:fref neq-%data% (1) ((1 *)) neq-%offset%) 0 0.0d0 0.0d0)
            (go label700)
           label606
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ITOL (=I1) illegal.  "
                                      (string 60))
            (xerrwd msg 30 6 0 1 itol 0 0 0.0d0 0.0d0)
            (go label700)
           label607
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- IOPT (=I1) illegal.  "
                                      (string 60))
            (xerrwd msg 30 7 0 1 iopt 0 0 0.0d0 0.0d0)
            (go label700)
           label608
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- MF (=I1) illegal.    "
                                      (string 60))
            (xerrwd msg 30 8 0 1 mf 0 0 0.0d0 0.0d0)
            (go label700)
           label609
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- SETH (=R1)  <  0.0  "
                                      (string 60))
            (xerrwd msg 30 9 0 0 0 0 1 seth 0.0d0)
            (go label700)
           label611
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- MAXORD (=I1)  <  0  "
                                      (string 60))
            (xerrwd msg 30 11 0 1 maxord 0 0 0.0d0 0.0d0)
            (go label700)
           label612
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- MXSTEP (=I1)  <  0  "
                                      (string 60))
            (xerrwd msg 30 12 0 1 mxstep 0 0 0.0d0 0.0d0)
            (go label700)
           label613
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- MXHNIL (=I1)  <  0  "
                                      (string 60))
            (xerrwd msg 30 13 0 1 mxhnil 0 0 0.0d0 0.0d0)
            (go label700)
           label614
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- TOUT (=R1) behind T (=R2)      "
                                      (string 60))
            (xerrwd msg 40 14 0 0 0 0 2 tout t$)
            (f2cl-lib:f2cl-set-string msg
                                      "      Integration direction is given by H0 (=R1)  "
                                      (string 60))
            (xerrwd msg 50 14 0 0 0 0 1 h0 0.0d0)
            (go label700)
           label615
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- HMAX (=R1)  <  0.0  "
                                      (string 60))
            (xerrwd msg 30 15 0 0 0 0 1 hmax 0.0d0)
            (go label700)
           label616
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- HMIN (=R1)  <  0.0  "
                                      (string 60))
            (xerrwd msg 30 16 0 0 0 0 1 hmin 0.0d0)
            (go label700)
           label617
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RWORK length is insufficient to proceed. "
                                      (string 60))
            (xerrwd msg 50 17 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENRW (=I1), exceeds LRW (=I2)"
                                      (string 60))
            (xerrwd msg 60 17 0 2 lenrw lrw 0 0.0d0 0.0d0)
            (go label700)
           label618
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- IWORK length is insufficient to proceed. "
                                      (string 60))
            (xerrwd msg 50 18 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENIW (=I1), exceeds LIW (=I2)"
                                      (string 60))
            (xerrwd msg 60 18 0 2 leniw liw 0 0.0d0 0.0d0)
            (go label700)
           label619
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RTOL(I1) is R1  <  0.0        "
                                      (string 60))
            (xerrwd msg 40 19 0 1 i 0 1 rtoli 0.0d0)
            (go label700)
           label620
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ATOL(I1) is R1  <  0.0        "
                                      (string 60))
            (xerrwd msg 40 20 0 1 i 0 1 atoli 0.0d0)
            (go label700)
           label621
            (setf ewti
                    (f2cl-lib:fref rwork-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add lewt i)
                                                      1))
                                   ((1 lrw))
                                   rwork-%offset%))
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- EWT(I1) is R1  <=  0.0         "
                                      (string 60))
            (xerrwd msg 40 21 0 1 i 0 1 ewti 0.0d0)
            (go label700)
           label622
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- TOUT(=R1) too close to T(=R2) to start integration."
                                      (string 60))
            (xerrwd msg 60 22 0 0 0 0 2 tout t$)
            (go label700)
           label623
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  "
                                      (string 60))
            (xerrwd msg 60 23 0 1 itask 0 2 tout tp)
            (go label700)
           label624
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   "
                                      (string 60))
            (xerrwd msg 60 24 0 0 0 0 2 tcrit tn)
            (go label700)
           label625
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   "
                                      (string 60))
            (xerrwd msg 60 25 0 0 0 0 2 tcrit tout)
            (go label700)
           label626
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- At start of problem, too much accuracy   "
                                      (string 60))
            (xerrwd msg 50 26 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "      requested for precision of machine..  See TOLSF (=R1) "
                                      (string 60))
            (xerrwd msg 60 26 0 0 0 0 1 tolsf 0.0d0)
            (setf (f2cl-lib:fref rwork-%data% (14) ((1 lrw)) rwork-%offset%)
                    tolsf)
            (go label700)
           label627
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Trouble in DINTDY.  ITASK = I1, TOUT = R1"
                                      (string 60))
            (xerrwd msg 50 27 0 1 itask 0 1 tout 0.0d0)
            (go label700)
           label628
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RWORK length insufficient (for Subroutine DPREP).  "
                                      (string 60))
            (xerrwd msg 60 28 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENRW (=I1), exceeds LRW (=I2)"
                                      (string 60))
            (xerrwd msg 60 28 0 2 lenrw lrw 0 0.0d0 0.0d0)
            (go label700)
           label629
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RWORK length insufficient (for Subroutine JGROUP). "
                                      (string 60))
            (xerrwd msg 60 29 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENRW (=I1), exceeds LRW (=I2)"
                                      (string 60))
            (xerrwd msg 60 29 0 2 lenrw lrw 0 0.0d0 0.0d0)
            (go label700)
           label630
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RWORK length insufficient (for Subroutine ODRV).   "
                                      (string 60))
            (xerrwd msg 60 30 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENRW (=I1), exceeds LRW (=I2)"
                                      (string 60))
            (xerrwd msg 60 30 0 2 lenrw lrw 0 0.0d0 0.0d0)
            (go label700)
           label631
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Error from ODRV in Yale Sparse Matrix Package.     "
                                      (string 60))
            (xerrwd msg 60 31 0 0 0 0 0 0.0d0 0.0d0)
            (setf imul (the f2cl-lib:integer4 (truncate (- iys 1) n)))
            (setf irem (f2cl-lib:int-sub iys (f2cl-lib:int-mul imul n)))
            (f2cl-lib:f2cl-set-string msg
                                      "      At T (=R1), ODRV returned error flag = I1*NEQ + I2.   "
                                      (string 60))
            (xerrwd msg 60 31 0 2 imul irem 1 tn 0.0d0)
            (go label700)
           label632
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- RWORK length insufficient (for Subroutine CDRV).   "
                                      (string 60))
            (xerrwd msg 60 32 0 0 0 0 0 0.0d0 0.0d0)
            (f2cl-lib:f2cl-set-string msg
                                      "        Length needed is  >=  LENRW (=I1), exceeds LRW (=I2)"
                                      (string 60))
            (xerrwd msg 60 32 0 2 lenrw lrw 0 0.0d0 0.0d0)
            (go label700)
           label633
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Error from CDRV in Yale Sparse Matrix Package.     "
                                      (string 60))
            (xerrwd msg 60 33 0 0 0 0 0 0.0d0 0.0d0)
            (setf imul (the f2cl-lib:integer4 (truncate (- iys 1) n)))
            (setf irem (f2cl-lib:int-sub iys (f2cl-lib:int-mul imul n)))
            (f2cl-lib:f2cl-set-string msg
                                      "      At T (=R1), CDRV returned error flag = I1*NEQ + I2.   "
                                      (string 60))
            (xerrwd msg 60 33 0 2 imul irem 1 tn 0.0d0)
            (cond
              ((= imul 2)
               (f2cl-lib:f2cl-set-string msg
                                         "        Duplicate entry in sparsity structure descriptors.  "
                                         (string 60))
               (xerrwd msg 60 33 0 0 0 0 0 0.0d0 0.0d0)))
            (cond
              ((or (= imul 3) (= imul 6))
               (f2cl-lib:f2cl-set-string msg
                                         "        Insufficient storage for NSFC (called by CDRV).     "
                                         (string 60))
               (xerrwd msg 60 33 0 0 0 0 0 0.0d0 0.0d0)))
           label700
            (setf istate -3)
            (go end_label)
           label800
            (f2cl-lib:f2cl-set-string msg
                                      "DLSODES- Run aborted.. apparent infinite loop.    "
                                      (string 60))
            (xerrwd msg 50 303 2 0 0 0 0 0.0d0 0.0d0)
            (go end_label)
           end_label
            (return
             (values nil
                     nil
                     nil
                     t$
                     nil
                     nil
                     nil
                     nil
                     nil
                     istate
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
  (setf (gethash 'fortran-to-lisp::dlsodes
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) t
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::t$ nil nil nil nil nil
                            fortran-to-lisp::istate nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::dstode fortran-to-lisp::xerrwd
                    fortran-to-lisp::dintdy fortran-to-lisp::dvnorm
                    fortran-to-lisp::diprep fortran-to-lisp::dewset))))

