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


(defun dprjis (neq y yh nyh ewt rtem savr s wk iwk res jac adda)
  (declare (type (f2cl-lib:integer4) nyh)
           (type (array double-float (*)) wk s savr rtem ewt yh y)
           (type (array f2cl-lib:integer4 (*)) iwk neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                      (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34))
                      (nje (aref (dls001-part-1 *dls001-common-block*) 35))
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
                      (ngp (aref (dlss01-part-1 *dlss01-common-block*) 28))
                      (nlu (aref (dlss01-part-1 *dlss01-common-block*) 29))
                      (nsp (aref (dlss01-part-1 *dlss01-common-block*) 31)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (rtem double-float rtem-%data% rtem-%offset%)
           (savr double-float savr-%data% savr-%offset%)
           (s double-float s-%data% s-%offset%)
           (wk double-float wk-%data% wk-%offset%))
        (prog ((ng 0) (kmin 0) (kmax 0) (k 0) (jmin 0) (jmax 0) (jj 0) (j 0)
               (ires 0) (imul 0) (i 0) (srur 0.0d0) (r 0.0d0) (hl0 0.0d0)
               (fac 0.0d0) (con 0.0d0))
          (declare (type (double-float) con fac hl0 r srur)
                   (type (f2cl-lib:integer4) i imul ires j jj jmax jmin k kmax
                                             kmin ng))
          (setf hl0 (* h el0))
          (setf con (- hl0))
          (setf jcur 1)
          (setf nje (f2cl-lib:int-add nje 1))
          (f2cl-lib:computed-goto (label100 label200) miter)
         label100
          (setf ires 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
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
                  (setf (f2cl-lib:fref rtem-%data% (i) ((1 *)) rtem-%offset%)
                          0.0d0)))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                  (funcall jac
                           neq
                           tn
                           y
                           s
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
                           rtem)
                (declare (ignore var-0 var-2 var-3 var-5 var-6 var-7))
                (when var-1
                  (setf tn var-1))
                (when var-4
                  (setf j var-4)))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label120
                  (setf (f2cl-lib:fref rtem-%data% (i) ((1 *)) rtem-%offset%)
                          (*
                           (f2cl-lib:fref rtem-%data%
                                          (i)
                                          ((1 *))
                                          rtem-%offset%)
                           con))))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (funcall adda
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
                           rtem)
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
                          (f2cl-lib:fref rtem-%data%
                                         (i)
                                         ((1 *))
                                         rtem-%offset%))
                 label125))
              (setf kmin (f2cl-lib:int-add kmax 1))
             label130))
          (go label290)
         label200
          (setf ires -1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
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
                           (/ 0.01d0
                              (f2cl-lib:fref ewt-%data%
                                             (jj)
                                             ((1 *))
                                             ewt-%offset%))))
                 label210
                  (setf (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                             r))))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                  (funcall res neq tn y s rtem ires)
                (declare (ignore var-0 var-2 var-3 var-4))
                (when var-1
                  (setf tn var-1))
                (when var-5
                  (setf ires var-5)))
              (setf nfe (f2cl-lib:int-add nfe 1))
              (if (> ires 1) (go label600))
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
                           (/ 0.01d0
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
                      (setf (f2cl-lib:fref rtem-%data%
                                           (i)
                                           ((1 *))
                                           rtem-%offset%)
                              (*
                               (-
                                (f2cl-lib:fref rtem-%data%
                                               (i)
                                               ((1 *))
                                               rtem-%offset%)
                                (f2cl-lib:fref savr-%data%
                                               (i)
                                               ((1 *))
                                               savr-%offset%))
                               fac))
                     label220))
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                      (funcall adda
                               neq
                               tn
                               y
                               jj
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
                               rtem)
                    (declare (ignore var-0 var-2 var-4 var-5 var-6))
                    (when var-1
                      (setf tn var-1))
                    (when var-3
                      (setf jj var-3)))
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
                              (f2cl-lib:fref rtem-%data%
                                             (i)
                                             ((1 *))
                                             rtem-%offset%))
                     label225))
                 label230))
              (setf jmin (f2cl-lib:int-add jmax 1))
             label240))
          (setf ires 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
         label290
          (setf nlu (f2cl-lib:int-add nlu 1))
          (setf ierpj 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label295
              (setf (f2cl-lib:fref rtem-%data% (i) ((1 *)) rtem-%offset%)
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
               rtem rtem nsp
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
         label600
          (setf ierpj ires)
          (go end_label)
         end_label
          (return
           (values nil nil nil nil nil nil nil nil nil nil nil nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dprjis
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::cdrv))))

