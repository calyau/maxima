;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2017-01 (21B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format double-float))

(in-package "ODEPACK")


(let ((mord
       (make-array 2
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(12 5)))
      (mxstp0 500)
      (mxhnl0 10))
  (declare (type (simple-array f2cl-lib:integer4 (2)) mord)
           (type (f2cl-lib:integer4) mxstp0 mxhnl0))
  (defun dlsode
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
                        (liwm (aref (dls001-part-1 *dls001-common-block*) 24))
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
                        (nqu (aref (dls001-part-1 *dls001-common-block*) 36)))
        (prog ((mu 0) (ml 0) (lenwm 0) (lenrw 0) (leniw 0) (lf0 0) (kgo 0)
               (imxer 0) (iflag 0) (i2 0) (i1 0) (i 0) (w0 0.0) (sum 0.0)
               (size 0.0) (tp 0.0) (tolsf 0.0) (tol 0.0) (tnext 0.0)
               (tdist 0.0) (tcrit 0.0) (rtoli 0.0) (rh 0.0) (hmx 0.0)
               (hmax 0.0) (h0 0.0) (ewti 0.0) (big 0.0) (ayi 0.0) (atoli 0.0)
               (ihit nil)
               (msg
                (make-array '(80)
                            :element-type 'character
                            :initial-element #\ )))
          (declare (type (string 80) msg)
                   (type f2cl-lib:logical ihit)
                   (type (double-float) atoli ayi big ewti h0 hmax hmx rh rtoli
                                        tcrit tdist tnext tol tolsf tp size sum
                                        w0)
                   (type (f2cl-lib:integer4) i i1 i2 iflag imxer kgo lf0 leniw
                                             lenrw lenwm ml mu))
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
          (if (<= (f2cl-lib:fref neq (1) ((1 *))) 0) (go label604))
          (if (= istate 1) (go label25))
          (if (> (f2cl-lib:fref neq (1) ((1 *))) n) (go label605))
         label25
          (setf n (f2cl-lib:fref neq (1) ((1 *))))
          (if (or (< itol 1) (> itol 4)) (go label606))
          (if (or (< iopt 0) (> iopt 1)) (go label607))
          (setf meth (the f2cl-lib:integer4 (truncate mf 10)))
          (setf miter (f2cl-lib:int-sub mf (f2cl-lib:int-mul 10 meth)))
          (if (or (< meth 1) (> meth 2)) (go label608))
          (if (or (< miter 0) (> miter 5)) (go label608))
          (if (<= miter 3) (go label30))
          (setf ml (f2cl-lib:fref iwork (1) ((1 liw))))
          (setf mu (f2cl-lib:fref iwork (2) ((1 liw))))
          (if (or (< ml 0) (>= ml n)) (go label609))
          (if (or (< mu 0) (>= mu n)) (go label610))
         label30
          (if (= iopt 1) (go label40))
          (setf maxord (f2cl-lib:fref mord (meth) ((1 2))))
          (setf mxstep mxstp0)
          (setf mxhnil mxhnl0)
          (if (= istate 1) (setf h0 0.0))
          (setf hmxi 0.0)
          (setf hmin 0.0)
          (go label60)
         label40
          (setf maxord (f2cl-lib:fref iwork (5) ((1 liw))))
          (if (< maxord 0) (go label611))
          (if (= maxord 0) (setf maxord 100))
          (setf maxord
                  (min (the f2cl-lib:integer4 maxord)
                       (the f2cl-lib:integer4
                            (f2cl-lib:fref mord (meth) ((1 2))))))
          (setf mxstep (f2cl-lib:fref iwork (6) ((1 liw))))
          (if (< mxstep 0) (go label612))
          (if (= mxstep 0) (setf mxstep mxstp0))
          (setf mxhnil (f2cl-lib:fref iwork (7) ((1 liw))))
          (if (< mxhnil 0) (go label613))
          (if (= mxhnil 0) (setf mxhnil mxhnl0))
          (if (/= istate 1) (go label50))
          (setf h0 (f2cl-lib:fref rwork (5) ((1 lrw))))
          (if (< (* (- tout t$) h0) 0.0) (go label614))
         label50
          (setf hmax (f2cl-lib:fref rwork (6) ((1 lrw))))
          (if (< hmax 0.0) (go label615))
          (setf hmxi 0.0)
          (if (> hmax 0.0) (setf hmxi (/ 1.0 hmax)))
          (setf hmin (f2cl-lib:fref rwork (7) ((1 lrw))))
          (if (< hmin 0.0) (go label616))
         label60
          (setf lyh 21)
          (if (= istate 1) (setf nyh n))
          (setf lwm
                  (f2cl-lib:int-add lyh
                                    (f2cl-lib:int-mul
                                     (f2cl-lib:int-add maxord 1)
                                     nyh)))
          (if (= miter 0) (setf lenwm 0))
          (if (or (= miter 1) (= miter 2))
              (setf lenwm (f2cl-lib:int-add (f2cl-lib:int-mul n n) 2)))
          (if (= miter 3) (setf lenwm (f2cl-lib:int-add n 2)))
          (if (>= miter 4)
              (setf lenwm
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul
                        (f2cl-lib:int-add (f2cl-lib:int-mul 2 ml) mu 1)
                        n)
                       2)))
          (setf lewt (f2cl-lib:int-add lwm lenwm))
          (setf lsavf (f2cl-lib:int-add lewt n))
          (setf lacor (f2cl-lib:int-add lsavf n))
          (setf lenrw (f2cl-lib:int-sub (f2cl-lib:int-add lacor n) 1))
          (setf (f2cl-lib:fref iwork (17) ((1 liw))) lenrw)
          (setf liwm 1)
          (setf leniw (f2cl-lib:int-add 20 n))
          (if (or (= miter 0) (= miter 3)) (setf leniw 20))
          (setf (f2cl-lib:fref iwork (18) ((1 liw))) leniw)
          (if (> lenrw lrw) (go label617))
          (if (> leniw liw) (go label618))
          (setf rtoli (f2cl-lib:fref rtol (1) ((1 *))))
          (setf atoli (f2cl-lib:fref atol (1) ((1 *))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (if (>= itol 3) (setf rtoli (f2cl-lib:fref rtol (i) ((1 *)))))
              (if (or (= itol 2) (= itol 4))
                  (setf atoli (f2cl-lib:fref atol (i) ((1 *)))))
              (if (< rtoli 0.0) (go label619))
              (if (< atoli 0.0) (go label620))
             label70))
          (if (= istate 1) (go label100))
          (setf jstart -1)
          (if (<= nq maxord) (go label90))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label80
              (setf (f2cl-lib:fref rwork
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add i lsavf)
                                     1))
                                   ((1 lrw)))
                      (f2cl-lib:fref rwork
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lwm)
                                       1))
                                     ((1 lrw))))))
         label90
          (if (> miter 0)
              (setf (f2cl-lib:fref rwork (lwm) ((1 lrw)))
                      (f2cl-lib:fsqrt uround)))
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
            (tagbody label95 (setf (f2cl-lib:fref rwork (i) ((1 lrw))) 0.0)))
          (go label200)
         label100
          (setf uround (dumach))
          (setf tn t$)
          (if (and (/= itask 4) (/= itask 5)) (go label110))
          (setf tcrit (f2cl-lib:fref rwork (1) ((1 lrw))))
          (if (< (* (- tcrit tout) (- tout t$)) 0.0) (go label625))
          (if (and (/= h0 0.0) (> (* (- (+ t$ h0) tcrit) h0) 0.0))
              (setf h0 (- tcrit t$)))
         label110
          (setf jstart 0)
          (if (> miter 0)
              (setf (f2cl-lib:fref rwork (lwm) ((1 lrw)))
                      (f2cl-lib:fsqrt uround)))
          (setf nhnil 0)
          (setf nst 0)
          (setf nje 0)
          (setf nslast 0)
          (setf hu 0.0)
          (setf nqu 0)
          (setf ccmax 0.3)
          (setf maxcor 3)
          (setf msbp 20)
          (setf mxncf 10)
          (setf lf0 (f2cl-lib:int-add lyh nyh))
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f
                       neq
                       t$
                       y
                       (f2cl-lib:array-slice rwork
                                             double-float
                                             (lf0)
                                             ((1 lrw))))
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf t$ var-1)))
          (setf nfe 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label115
              (setf (f2cl-lib:fref rwork
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add i lyh)
                                                      1))
                                   ((1 lrw)))
                      (f2cl-lib:fref y (i) ((1 *))))))
          (setf nq 1)
          (setf h 1.0)
          (dewset n itol rtol atol
           (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw)))
           (f2cl-lib:array-slice rwork double-float (lewt) ((1 lrw))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (if
               (<=
                (f2cl-lib:fref rwork
                               ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt) 1))
                               ((1 lrw)))
                0.0)
               (go label621))
             label120
              (setf (f2cl-lib:fref rwork
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt)
                                                      1))
                                   ((1 lrw)))
                      (/ 1.0
                         (f2cl-lib:fref rwork
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add i lewt)
                                          1))
                                        ((1 lrw)))))))
          (if (/= h0 0.0) (go label180))
          (setf tdist (abs (- tout t$)))
          (setf w0 (max (abs t$) (abs tout)))
          (if (< tdist (* 2.0 uround w0)) (go label622))
          (setf tol (f2cl-lib:fref rtol (1) ((1 *))))
          (if (<= itol 2) (go label140))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label130
              (setf tol (max tol (f2cl-lib:fref rtol (i) ((1 *)))))))
         label140
          (if (> tol 0.0) (go label160))
          (setf atoli (f2cl-lib:fref atol (1) ((1 *))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (if (or (= itol 2) (= itol 4))
                  (setf atoli (f2cl-lib:fref atol (i) ((1 *)))))
              (setf ayi (abs (f2cl-lib:fref y (i) ((1 *)))))
              (if (/= ayi 0.0) (setf tol (max tol (/ atoli ayi))))
             label150))
         label160
          (setf tol (max tol (* 100.0 uround)))
          (setf tol (min tol 0.001))
          (setf sum
                  (dvnorm n
                   (f2cl-lib:array-slice rwork double-float (lf0) ((1 lrw)))
                   (f2cl-lib:array-slice rwork double-float (lewt) ((1 lrw)))))
          (setf sum (+ (/ 1.0 (* tol w0 w0)) (* tol (expt sum 2))))
          (setf h0 (/ 1.0 (f2cl-lib:fsqrt sum)))
          (setf h0 (min h0 tdist))
          (setf h0 (f2cl-lib:sign h0 (- tout t$)))
         label180
          (setf rh (* (abs h0) hmxi))
          (if (> rh 1.0) (setf h0 (/ h0 rh)))
          (setf h h0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label190
              (setf (f2cl-lib:fref rwork
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add i lf0)
                                                      1))
                                   ((1 lrw)))
                      (* h0
                         (f2cl-lib:fref rwork
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add i lf0)
                                          1))
                                        ((1 lrw)))))))
          (go label270)
         label200
          (setf nslast nst)
          (f2cl-lib:computed-goto
           (label210 label250 label220 label230 label240)
           itask)
         label210
          (if (< (* (- tn tout) h) 0.0) (go label250))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy tout 0
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw))) nyh y
               iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (if (/= iflag 0) (go label627))
          (setf t$ tout)
          (go label420)
         label220
          (setf tp (- tn (* hu (+ 1.0 (* 100.0 uround)))))
          (if (> (* (- tp tout) h) 0.0) (go label623))
          (if (< (* (- tn tout) h) 0.0) (go label250))
          (go label400)
         label230
          (setf tcrit (f2cl-lib:fref rwork (1) ((1 lrw))))
          (if (> (* (- tn tcrit) h) 0.0) (go label624))
          (if (< (* (- tcrit tout) h) 0.0) (go label625))
          (if (< (* (- tn tout) h) 0.0) (go label245))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy tout 0
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw))) nyh y
               iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (if (/= iflag 0) (go label627))
          (setf t$ tout)
          (go label420)
         label240
          (setf tcrit (f2cl-lib:fref rwork (1) ((1 lrw))))
          (if (> (* (- tn tcrit) h) 0.0) (go label624))
         label245
          (setf hmx (+ (abs tn) (abs h)))
          (setf ihit (<= (abs (- tn tcrit)) (* 100.0 uround hmx)))
          (if ihit (go label400))
          (setf tnext (+ tn (* h (+ 1.0 (* 4.0 uround)))))
          (if (<= (* (- tnext tcrit) h) 0.0) (go label250))
          (setf h (* (- tcrit tn) (- 1.0 (* 4.0 uround))))
          (if (= istate 2) (setf jstart -2))
         label250
          (if (>= (f2cl-lib:int-sub nst nslast) mxstep) (go label500))
          (dewset n itol rtol atol
           (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw)))
           (f2cl-lib:array-slice rwork double-float (lewt) ((1 lrw))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (if
               (<=
                (f2cl-lib:fref rwork
                               ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt) 1))
                               ((1 lrw)))
                0.0)
               (go label510))
             label260
              (setf (f2cl-lib:fref rwork
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add i lewt)
                                                      1))
                                   ((1 lrw)))
                      (/ 1.0
                         (f2cl-lib:fref rwork
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add i lewt)
                                          1))
                                        ((1 lrw)))))))
         label270
          (setf tolsf
                  (* uround
                     (dvnorm n
                      (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw)))
                      (f2cl-lib:array-slice rwork
                                            double-float
                                            (lewt)
                                            ((1 lrw))))))
          (if (<= tolsf 1.0) (go label280))
          (setf tolsf (* tolsf 2.0))
          (if (= nst 0) (go label626))
          (go label520)
         label280
          (if (/= (+ tn h) tn) (go label290))
          (setf nhnil (f2cl-lib:int-add nhnil 1))
          (if (> nhnil mxhnil) (go label290))
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  Warning..internal T (=R1) and H (=R2) are"
                                    (string 80))
          (xerrwd msg 50 101 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      such that in the machine, T + H = T on the next step  "
                                    (string 80))
          (xerrwd msg 60 101 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      (H = step size). Solver will continue anyway"
                                    (string 80))
          (xerrwd msg 50 101 0 0 0 0 2 tn h)
          (if (< nhnil mxhnil) (go label290))
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  Above warning has been issued I1 times.  "
                                    (string 80))
          (xerrwd msg 50 102 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      It will not be issued again for this problem"
                                    (string 80))
          (xerrwd msg 50 102 0 1 mxhnil 0 0 0.0 0.0)
         label290
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13)
              (dstode neq y
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw))) nyh
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw)))
               (f2cl-lib:array-slice rwork double-float (lewt) ((1 lrw)))
               (f2cl-lib:array-slice rwork double-float (lsavf) ((1 lrw)))
               (f2cl-lib:array-slice rwork double-float (lacor) ((1 lrw)))
               (f2cl-lib:array-slice rwork double-float (lwm) ((1 lrw)))
               (f2cl-lib:array-slice iwork f2cl-lib:integer4 (liwm) ((1 liw)))
               f jac #'dprepj #'dsolsy)
            (declare (ignore var-0 var-1 var-2 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13))
            (setf nyh var-3))
          (setf kgo (f2cl-lib:int-sub 1 kflag))
          (f2cl-lib:computed-goto (label300 label530 label540) kgo)
         label300
          (setf init 1)
          (f2cl-lib:computed-goto
           (label310 label400 label330 label340 label350)
           itask)
         label310
          (if (< (* (- tn tout) h) 0.0) (go label250))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy tout 0
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw))) nyh y
               iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (setf t$ tout)
          (go label420)
         label330
          (if (>= (* (- tn tout) h) 0.0) (go label400))
          (go label250)
         label340
          (if (< (* (- tn tout) h) 0.0) (go label345))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy tout 0
               (f2cl-lib:array-slice rwork double-float (lyh) ((1 lrw))) nyh y
               iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (setf t$ tout)
          (go label420)
         label345
          (setf hmx (+ (abs tn) (abs h)))
          (setf ihit (<= (abs (- tn tcrit)) (* 100.0 uround hmx)))
          (if ihit (go label400))
          (setf tnext (+ tn (* h (+ 1.0 (* 4.0 uround)))))
          (if (<= (* (- tnext tcrit) h) 0.0) (go label250))
          (setf h (* (- tcrit tn) (- 1.0 (* 4.0 uround))))
          (setf jstart -2)
          (go label250)
         label350
          (setf hmx (+ (abs tn) (abs h)))
          (setf ihit (<= (abs (- tn tcrit)) (* 100.0 uround hmx)))
         label400
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label410
              (setf (f2cl-lib:fref y (i) ((1 *)))
                      (f2cl-lib:fref rwork
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lyh)
                                       1))
                                     ((1 lrw))))))
          (setf t$ tn)
          (if (and (/= itask 4) (/= itask 5)) (go label420))
          (if ihit (setf t$ tcrit))
         label420
          (setf istate 2)
          (setf (f2cl-lib:fref rwork (11) ((1 lrw))) hu)
          (setf (f2cl-lib:fref rwork (12) ((1 lrw))) h)
          (setf (f2cl-lib:fref rwork (13) ((1 lrw))) tn)
          (setf (f2cl-lib:fref iwork (11) ((1 liw))) nst)
          (setf (f2cl-lib:fref iwork (12) ((1 liw))) nfe)
          (setf (f2cl-lib:fref iwork (13) ((1 liw))) nje)
          (setf (f2cl-lib:fref iwork (14) ((1 liw))) nqu)
          (setf (f2cl-lib:fref iwork (15) ((1 liw))) nq)
          (go end_label)
         label500
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At current T (=R1), MXSTEP (=I1) steps   "
                                    (string 80))
          (xerrwd msg 50 201 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      taken on this call before reaching TOUT     "
                                    (string 80))
          (xerrwd msg 50 201 0 1 mxstep 0 1 tn 0.0)
          (setf istate -1)
          (go label580)
         label510
          (setf ewti
                  (f2cl-lib:fref rwork
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add lewt i)
                                                    1))
                                 ((1 lrw))))
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At T (=R1), EWT(I1) has become R2  <=  0."
                                    (string 80))
          (xerrwd msg 50 202 0 1 i 0 2 tn ewti)
          (setf istate -6)
          (go label580)
         label520
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At T (=R1), too much accuracy requested  "
                                    (string 80))
          (xerrwd msg 50 203 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      for precision of machine..  see TOLSF (=R2) "
                                    (string 80))
          (xerrwd msg 50 203 0 0 0 0 2 tn tolsf)
          (setf (f2cl-lib:fref rwork (14) ((1 lrw))) tolsf)
          (setf istate -2)
          (go label580)
         label530
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At T(=R1) and step size H(=R2), the error"
                                    (string 80))
          (xerrwd msg 50 204 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      test failed repeatedly or with ABS(H) = HMIN"
                                    (string 80))
          (xerrwd msg 50 204 0 0 0 0 2 tn h)
          (setf istate -4)
          (go label560)
         label540
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At T (=R1) and step size H (=R2), the    "
                                    (string 80))
          (xerrwd msg 50 205 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      corrector convergence failed repeatedly     "
                                    (string 80))
          (xerrwd msg 50 205 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      or with ABS(H) = HMIN   "
                                    (string 80))
          (xerrwd msg 30 205 0 0 0 0 2 tn h)
          (setf istate -5)
         label560
          (setf big 0.0)
          (setf imxer 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf size
                      (abs
                       (*
                        (f2cl-lib:fref rwork
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add i lacor)
                                         1))
                                       ((1 lrw)))
                        (f2cl-lib:fref rwork
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add i lewt)
                                         1))
                                       ((1 lrw))))))
              (if (>= big size) (go label570))
              (setf big size)
              (setf imxer i)
             label570))
          (setf (f2cl-lib:fref iwork (16) ((1 liw))) imxer)
         label580
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label590
              (setf (f2cl-lib:fref y (i) ((1 *)))
                      (f2cl-lib:fref rwork
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lyh)
                                       1))
                                     ((1 lrw))))))
          (setf t$ tn)
          (setf (f2cl-lib:fref rwork (11) ((1 lrw))) hu)
          (setf (f2cl-lib:fref rwork (12) ((1 lrw))) h)
          (setf (f2cl-lib:fref rwork (13) ((1 lrw))) tn)
          (setf (f2cl-lib:fref iwork (11) ((1 liw))) nst)
          (setf (f2cl-lib:fref iwork (12) ((1 liw))) nfe)
          (setf (f2cl-lib:fref iwork (13) ((1 liw))) nje)
          (setf (f2cl-lib:fref iwork (14) ((1 liw))) nqu)
          (setf (f2cl-lib:fref iwork (15) ((1 liw))) nq)
          (go end_label)
         label601
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ISTATE (=I1) illegal "
                                    (string 80))
          (xerrwd msg 30 1 0 1 istate 0 0 0.0 0.0)
          (if (< istate 0) (go label800))
          (go label700)
         label602
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ITASK (=I1) illegal  "
                                    (string 80))
          (xerrwd msg 30 2 0 1 itask 0 0 0.0 0.0)
          (go label700)
         label603
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ISTATE  >  1 but DLSODE not initialized "
                                    (string 80))
          (xerrwd msg 50 3 0 0 0 0 0 0.0 0.0)
          (go label700)
         label604
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  NEQ (=I1)  <  1     "
                                    (string 80))
          (xerrwd msg 30 4 0 1 (f2cl-lib:fref neq (1) ((1 *))) 0 0 0.0 0.0)
          (go label700)
         label605
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ISTATE = 3 and NEQ increased (I1 to I2)  "
                                    (string 80))
          (xerrwd msg 50 5 0 2 n (f2cl-lib:fref neq (1) ((1 *))) 0 0.0 0.0)
          (go label700)
         label606
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ITOL (=I1) illegal   "
                                    (string 80))
          (xerrwd msg 30 6 0 1 itol 0 0 0.0 0.0)
          (go label700)
         label607
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  IOPT (=I1) illegal   "
                                    (string 80))
          (xerrwd msg 30 7 0 1 iopt 0 0 0.0 0.0)
          (go label700)
         label608
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  MF (=I1) illegal     "
                                    (string 80))
          (xerrwd msg 30 8 0 1 mf 0 0 0.0 0.0)
          (go label700)
         label609
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ML (=I1) illegal..  < 0 or  >= NEQ (=I2)"
                                    (string 80))
          (xerrwd msg 50 9 0 2 ml (f2cl-lib:fref neq (1) ((1 *))) 0 0.0 0.0)
          (go label700)
         label610
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  MU (=I1) illegal..  < 0 or  >= NEQ (=I2)"
                                    (string 80))
          (xerrwd msg 50 10 0 2 mu (f2cl-lib:fref neq (1) ((1 *))) 0 0.0 0.0)
          (go label700)
         label611
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  MAXORD (=I1)  <  0  "
                                    (string 80))
          (xerrwd msg 30 11 0 1 maxord 0 0 0.0 0.0)
          (go label700)
         label612
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  MXSTEP (=I1)  <  0  "
                                    (string 80))
          (xerrwd msg 30 12 0 1 mxstep 0 0 0.0 0.0)
          (go label700)
         label613
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  MXHNIL (=I1)  <  0  "
                                    (string 80))
          (xerrwd msg 30 13 0 1 mxhnil 0 0 0.0 0.0)
          (go label700)
         label614
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  TOUT (=R1) behind T (=R2)      "
                                    (string 80))
          (xerrwd msg 40 14 0 0 0 0 2 tout t$)
          (f2cl-lib:f2cl-set-string msg
                                    "      Integration direction is given by H0 (=R1)  "
                                    (string 80))
          (xerrwd msg 50 14 0 0 0 0 1 h0 0.0)
          (go label700)
         label615
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  HMAX (=R1)  <  0.0  "
                                    (string 80))
          (xerrwd msg 30 15 0 0 0 0 1 hmax 0.0)
          (go label700)
         label616
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  HMIN (=R1)  <  0.0  "
                                    (string 80))
          (xerrwd msg 30 16 0 0 0 0 1 hmin 0.0)
          (go label700)
         label617
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)"
                                    (string 80))
          (xerrwd msg 60 17 0 2 lenrw lrw 0 0.0 0.0)
          (go label700)
         label618
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)"
                                    (string 80))
          (xerrwd msg 60 18 0 2 leniw liw 0 0.0 0.0)
          (go label700)
         label619
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  RTOL(I1) is R1  <  0.0        "
                                    (string 80))
          (xerrwd msg 40 19 0 1 i 0 1 rtoli 0.0)
          (go label700)
         label620
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ATOL(I1) is R1  <  0.0        "
                                    (string 80))
          (xerrwd msg 40 20 0 1 i 0 1 atoli 0.0)
          (go label700)
         label621
          (setf ewti
                  (f2cl-lib:fref rwork
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add lewt i)
                                                    1))
                                 ((1 lrw))))
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  EWT(I1) is R1  <=  0.0         "
                                    (string 80))
          (xerrwd msg 40 21 0 1 i 0 1 ewti 0.0)
          (go label700)
         label622
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  TOUT (=R1) too close to T(=R2) to start integration"
                                    (string 80))
          (xerrwd msg 60 22 0 0 0 0 2 tout t$)
          (go label700)
         label623
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  "
                                    (string 80))
          (xerrwd msg 60 23 0 1 itask 0 2 tout tp)
          (go label700)
         label624
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ITASK = 4 OR 5 and TCRIT (=R1) behind TCUR (=R2)   "
                                    (string 80))
          (xerrwd msg 60 24 0 0 0 0 2 tcrit tn)
          (go label700)
         label625
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   "
                                    (string 80))
          (xerrwd msg 60 25 0 0 0 0 2 tcrit tout)
          (go label700)
         label626
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  At start of problem, too much accuracy   "
                                    (string 80))
          (xerrwd msg 50 26 0 0 0 0 0 0.0 0.0)
          (f2cl-lib:f2cl-set-string msg
                                    "      requested for precision of machine..  See TOLSF (=R1) "
                                    (string 80))
          (xerrwd msg 60 26 0 0 0 0 1 tolsf 0.0)
          (setf (f2cl-lib:fref rwork (14) ((1 lrw))) tolsf)
          (go label700)
         label627
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  Trouble in DINTDY.  ITASK = I1, TOUT = R1"
                                    (string 80))
          (xerrwd msg 50 27 0 1 itask 0 1 tout 0.0)
         label700
          (setf istate -3)
          (go end_label)
         label800
          (f2cl-lib:f2cl-set-string msg
                                    "DLSODE-  Run aborted.. apparent infinite loop     "
                                    (string 80))
          (xerrwd msg 50 303 2 0 0 0 0 0.0 0.0)
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
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlsode
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
                    fortran-to-lisp::dewset))))

