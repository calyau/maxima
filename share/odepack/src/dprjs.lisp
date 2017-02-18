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


(defun dprjs (neq y yh nyh ewt ftem savf wk iwk f jac)
  (declare (type (f2cl-lib:integer4) nyh)
           (type (array double-float (*)) wk savf ftem ewt yh y)
           (type (array f2cl-lib:integer4 (*)) iwk neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (rc (aref (dls001-part-0 *dls001-common-block*) 215))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (uround (aref (dls001-part-0 *dls001-common-block*) 217))
                      (icf (aref (dls001-part-1 *dls001-common-block*) 12))
                      (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                      (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nst (aref (dls001-part-1 *dls001-common-block*) 33))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34))
                      (nje (aref (dls001-part-1 *dls001-common-block*) 35))
                      (con0 (aref (dlss01-part-0 *dlss01-common-block*) 0))
                      (conmin (aref (dlss01-part-0 *dlss01-common-block*) 1))
                      (ccmxj (aref (dlss01-part-0 *dlss01-common-block*) 2))
                      (psmall (aref (dlss01-part-0 *dlss01-common-block*) 3))
                      (rbig (aref (dlss01-part-0 *dlss01-common-block*) 4))
                      (iplost (aref (dlss01-part-1 *dlss01-common-block*) 0))
                      (iesp (aref (dlss01-part-1 *dlss01-common-block*) 1))
                      (iys (aref (dlss01-part-1 *dlss01-common-block*) 3))
                      (iba (aref (dlss01-part-1 *dlss01-common-block*) 4))
                      (ibian (aref (dlss01-part-1 *dlss01-common-block*) 5))
                      (ibjan (aref (dlss01-part-1 *dlss01-common-block*) 6))
                      (ibjgp (aref (dlss01-part-1 *dlss01-common-block*) 7))
                      (ipian (aref (dlss01-part-1 *dlss01-common-block*) 8))
                      (ipjan (aref (dlss01-part-1 *dlss01-common-block*) 9))
                      (ipigp (aref (dlss01-part-1 *dlss01-common-block*) 11))
                      (ipr (aref (dlss01-part-1 *dlss01-common-block*) 12))
                      (ipc (aref (dlss01-part-1 *dlss01-common-block*) 13))
                      (ipic (aref (dlss01-part-1 *dlss01-common-block*) 14))
                      (ipisp (aref (dlss01-part-1 *dlss01-common-block*) 15))
                      (iprsp (aref (dlss01-part-1 *dlss01-common-block*) 16))
                      (ipa (aref (dlss01-part-1 *dlss01-common-block*) 17))
                      (msbj (aref (dlss01-part-1 *dlss01-common-block*) 26))
                      (nslj (aref (dlss01-part-1 *dlss01-common-block*) 27))
                      (ngp (aref (dlss01-part-1 *dlss01-common-block*) 28))
                      (nlu (aref (dlss01-part-1 *dlss01-common-block*) 29))
                      (nsp (aref (dlss01-part-1 *dlss01-common-block*) 31)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (ftem double-float ftem-%data% ftem-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (wk double-float wk-%data% wk-%offset%))
        (prog ((ng 0) (kmin 0) (kmax 0) (k 0) (jmin 0) (jmax 0) (jok 0) (jj 0)
               (j 0) (imul 0) (i 0) (srur 0.0d0) (rcont 0.0d0) (rcon 0.0d0)
               (r0 0.0d0) (r 0.0d0) (pij 0.0d0) (hl0 0.0d0) (fac 0.0d0)
               (di 0.0d0) (con 0.0d0))
          (declare (type (double-float) con di fac hl0 pij r r0 rcon rcont
                                        srur)
                   (type (f2cl-lib:integer4) i imul j jj jok jmax jmin k kmax
                                             kmin ng))
          (setf hl0 (* h el0))
          (setf con (- hl0))
          (if (= miter 3) (go label300))
          (setf jok 1)
          (if (or (= nst 0) (>= nst (f2cl-lib:int-add nslj msbj)))
              (setf jok 0))
          (if (and (= icf 1) (< (abs (- rc 1.0d0)) ccmxj)) (setf jok 0))
          (if (= icf 2) (setf jok 0))
          (if (= jok 1) (go label250))
         label20
          (setf jcur 1)
          (setf nje (f2cl-lib:int-add nje 1))
          (setf nslj nst)
          (setf iplost 0)
          (setf conmin (abs con))
          (f2cl-lib:computed-goto (label100 label200) miter)
         label100
          (setf kmin (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref iwk-%data%
                                      ((f2cl-lib:int-add ipian j))
                                      ((1 *))
                                      iwk-%offset%)
                       1))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label110
                  (setf (f2cl-lib:fref ftem-%data% (i) ((1 *)) ftem-%offset%)
                          0.0d0)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (funcall jac
                           neq
                           tn
                           y
                           j
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipian)
                                                 ((1 *))
                                                 iwk-%offset%)
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipjan)
                                                 ((1 *))
                                                 iwk-%offset%)
                           ftem)
                (declare (ignore var-0 var-2 var-4 var-5 var-6))
                (when var-1
                  (setf tn var-1))
                (when var-3
                  (setf j var-3)))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjan k))
                                         ((1 *))
                                         iwk-%offset%))
                  (setf (f2cl-lib:fref wk-%data%
                                       ((f2cl-lib:int-add iba k))
                                       ((1 *))
                                       wk-%offset%)
                          (*
                           (f2cl-lib:fref ftem-%data%
                                          (i)
                                          ((1 *))
                                          ftem-%offset%)
                           con))
                  (if (= i j)
                      (setf (f2cl-lib:fref wk-%data%
                                           ((f2cl-lib:int-add iba k))
                                           ((1 *))
                                           wk-%offset%)
                              (+
                               (f2cl-lib:fref wk-%data%
                                              ((f2cl-lib:int-add iba k))
                                              ((1 *))
                                              wk-%offset%)
                               1.0d0)))
                 label120))
              (setf kmin (f2cl-lib:int-add kmax 1))
             label130))
          (go label290)
         label200
          (setf fac (dvnorm n savf ewt))
          (setf r0 (* 1000.0d0 (abs h) uround n fac))
          (if (= r0 0.0d0) (setf r0 1.0d0))
          (setf srur (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
          (setf jmin (f2cl-lib:fref iwk-%data% (ipigp) ((1 *)) iwk-%offset%))
          (f2cl-lib:fdo (ng 1 (f2cl-lib:int-add ng 1))
                        ((> ng ngp) nil)
            (tagbody
              (setf jmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref iwk-%data%
                                      ((f2cl-lib:int-add ipigp ng))
                                      ((1 *))
                                      iwk-%offset%)
                       1))
              (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                            ((> j jmax) nil)
                (tagbody
                  (setf jj
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjgp j))
                                         ((1 *))
                                         iwk-%offset%))
                  (setf r
                          (max
                           (* srur
                              (abs
                               (f2cl-lib:fref y-%data%
                                              (jj)
                                              ((1 *))
                                              y-%offset%)))
                           (/ r0
                              (f2cl-lib:fref ewt-%data%
                                             (jj)
                                             ((1 *))
                                             ewt-%offset%))))
                 label210
                  (setf (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                             r))))
              (multiple-value-bind (var-0 var-1 var-2 var-3)
                  (funcall f neq tn y ftem)
                (declare (ignore var-0 var-2 var-3))
                (when var-1
                  (setf tn var-1)))
              (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                            ((> j jmax) nil)
                (tagbody
                  (setf jj
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjgp j))
                                         ((1 *))
                                         iwk-%offset%))
                  (setf (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                          (f2cl-lib:fref yh-%data%
                                         (jj 1)
                                         ((1 nyh) (1 *))
                                         yh-%offset%))
                  (setf r
                          (max
                           (* srur
                              (abs
                               (f2cl-lib:fref y-%data%
                                              (jj)
                                              ((1 *))
                                              y-%offset%)))
                           (/ r0
                              (f2cl-lib:fref ewt-%data%
                                             (jj)
                                             ((1 *))
                                             ewt-%offset%))))
                  (setf fac (/ (- hl0) r))
                  (setf kmin
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibian jj))
                                         ((1 *))
                                         iwk-%offset%))
                  (setf kmax
                          (f2cl-lib:int-sub
                           (f2cl-lib:fref iwk-%data%
                                          ((f2cl-lib:int-add ibian jj 1))
                                          ((1 *))
                                          iwk-%offset%)
                           1))
                  (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                                ((> k kmax) nil)
                    (tagbody
                      (setf i
                              (f2cl-lib:fref iwk-%data%
                                             ((f2cl-lib:int-add ibjan k))
                                             ((1 *))
                                             iwk-%offset%))
                      (setf (f2cl-lib:fref wk-%data%
                                           ((f2cl-lib:int-add iba k))
                                           ((1 *))
                                           wk-%offset%)
                              (*
                               (-
                                (f2cl-lib:fref ftem-%data%
                                               (i)
                                               ((1 *))
                                               ftem-%offset%)
                                (f2cl-lib:fref savf-%data%
                                               (i)
                                               ((1 *))
                                               savf-%offset%))
                               fac))
                      (if (= i jj)
                          (setf (f2cl-lib:fref wk-%data%
                                               ((f2cl-lib:int-add iba k))
                                               ((1 *))
                                               wk-%offset%)
                                  (+
                                   (f2cl-lib:fref wk-%data%
                                                  ((f2cl-lib:int-add iba k))
                                                  ((1 *))
                                                  wk-%offset%)
                                   1.0d0)))
                     label220))
                 label230))
              (setf jmin (f2cl-lib:int-add jmax 1))
             label240))
          (setf nfe (f2cl-lib:int-add nfe ngp))
          (go label290)
         label250
          (setf jcur 0)
          (setf rcon (/ con con0))
          (setf rcont (/ (abs con) conmin))
          (if (and (> rcont rbig) (= iplost 1)) (go label20))
          (setf kmin (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref iwk-%data%
                                      ((f2cl-lib:int-add ipian j))
                                      ((1 *))
                                      iwk-%offset%)
                       1))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjan k))
                                         ((1 *))
                                         iwk-%offset%))
                  (setf pij
                          (f2cl-lib:fref wk-%data%
                                         ((f2cl-lib:int-add iba k))
                                         ((1 *))
                                         wk-%offset%))
                  (if (/= i j) (go label260))
                  (setf pij (- pij 1.0d0))
                  (if (>= (abs pij) psmall) (go label260))
                  (setf iplost 1)
                  (setf conmin (min (abs con0) conmin))
                 label260
                  (setf pij (* pij rcon))
                  (if (= i j) (setf pij (+ pij 1.0d0)))
                  (setf (f2cl-lib:fref wk-%data%
                                       ((f2cl-lib:int-add iba k))
                                       ((1 *))
                                       wk-%offset%)
                          pij)
                 label270))
              (setf kmin (f2cl-lib:int-add kmax 1))
             label275))
         label290
          (setf nlu (f2cl-lib:int-add nlu 1))
          (setf con0 con)
          (setf ierpj 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label295
              (setf (f2cl-lib:fref ftem-%data% (i) ((1 *)) ftem-%offset%)
                      0.0d0)))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (cdrv n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipr)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipc)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipic)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               ftem ftem nsp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (iprsp)
                                     ((1 *))
                                     wk-%offset%)
               iesp 2 iys)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf iesp var-12)
            (setf iys var-14))
          (if (= iys 0) (go end_label))
          (setf imul (the f2cl-lib:integer4 (truncate (- iys 1) n)))
          (setf ierpj -2)
          (if (= imul 8) (setf ierpj 1))
          (if (= imul 10) (setf ierpj -1))
          (go end_label)
         label300
          (setf jcur 1)
          (setf nje (f2cl-lib:int-add nje 1))
          (setf (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%) hl0)
          (setf ierpj 0)
          (setf r (* el0 0.1d0))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label310
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                         (* r
                            (-
                             (* h
                                (f2cl-lib:fref savf-%data%
                                               (i)
                                               ((1 *))
                                               savf-%offset%))
                             (f2cl-lib:fref yh-%data%
                                            (i 2)
                                            ((1 nyh) (1 *))
                                            yh-%offset%)))))))
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f
                       neq
                       tn
                       y
                       (f2cl-lib:array-slice wk-%data%
                                             double-float
                                             (3)
                                             ((1 *))
                                             wk-%offset%))
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf tn var-1)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf r0
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
              (setf di
                      (- (* 0.1d0 r0)
                         (* h
                            (-
                             (f2cl-lib:fref wk-%data%
                                            ((f2cl-lib:int-add i 2))
                                            ((1 *))
                                            wk-%offset%)
                             (f2cl-lib:fref savf-%data%
                                            (i)
                                            ((1 *))
                                            savf-%offset%)))))
              (setf (f2cl-lib:fref wk-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wk-%offset%)
                      1.0d0)
              (if
               (< (abs r0)
                  (/ uround
                     (f2cl-lib:fref ewt-%data% (i) ((1 *)) ewt-%offset%)))
               (go label320))
              (if (= (abs di) 0.0d0) (go label330))
              (setf (f2cl-lib:fref wk-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wk-%offset%)
                      (/ (* 0.1d0 r0) di))
             label320))
          (go end_label)
         label330
          (setf ierpj 2)
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dprjs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::cdrv fortran-to-lisp::dvnorm))))

