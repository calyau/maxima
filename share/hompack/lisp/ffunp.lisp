;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2020-04 (21D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "HOMPACK")


(defun ffunp (n numt mmaxt kdeg coef cl x xx trm dtrm clx dxnp1 f df)
  (declare (type (array double-float (*)) df f dxnp1 clx dtrm trm xx x cl coef)
           (type (array f2cl-lib:integer4 (*)) kdeg numt)
           (type (f2cl-lib:integer4) mmaxt n))
  (f2cl-lib:with-multi-array-data
      ((numt f2cl-lib:integer4 numt-%data% numt-%offset%)
       (kdeg f2cl-lib:integer4 kdeg-%data% kdeg-%offset%)
       (coef double-float coef-%data% coef-%offset%)
       (cl double-float cl-%data% cl-%offset%)
       (x double-float x-%data% x-%offset%)
       (xx double-float xx-%data% xx-%offset%)
       (trm double-float trm-%data% trm-%offset%)
       (dtrm double-float dtrm-%data% dtrm-%offset%)
       (clx double-float clx-%data% clx-%offset%)
       (dxnp1 double-float dxnp1-%data% dxnp1-%offset%)
       (f double-float f-%data% f-%offset%)
       (df double-float df-%data% df-%offset%))
    (prog ((temp1 (make-array 2 :element-type 'double-float))
           (temp2 (make-array 2 :element-type 'double-float))
           (xnp1 (make-array 2 :element-type 'double-float)) (i 0) (ierr 0)
           (j 0) (k 0) (l 0) (m 0) (nnnn 0) (np1 0))
      (declare (type (f2cl-lib:integer4) np1 nnnn m l k j ierr i)
               (type (array double-float (2)) xnp1 temp2 temp1))
      (setf np1 (f2cl-lib:int-add n 1))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (mulp
           (f2cl-lib:array-slice cl-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 (f2cl-lib:int-add n 1)))
                                 cl-%offset%)
           (f2cl-lib:array-slice x-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 x-%offset%)
           (f2cl-lib:array-slice clx-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 clx-%offset%))
         label40))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 2) nil)
        (tagbody
          (setf (f2cl-lib:fref xnp1 (i) ((1 2)))
                  (f2cl-lib:fref cl-%data%
                                 (i np1)
                                 ((1 2) (1 (f2cl-lib:int-add n 1)))
                                 cl-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf (f2cl-lib:fref xnp1 (i) ((1 2)))
                      (+ (f2cl-lib:fref xnp1 (i) ((1 2)))
                         (f2cl-lib:fref clx-%data%
                                        (i j)
                                        ((1 2) (1 n))
                                        clx-%offset%)))
              (setf (f2cl-lib:fref dxnp1-%data%
                                   (i j)
                                   ((1 2) (1 n))
                                   dxnp1-%offset%)
                      (f2cl-lib:fref cl-%data%
                                     (i j)
                                     ((1 2) (1 (f2cl-lib:int-add n 1)))
                                     cl-%offset%))
             label50))
         label60))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 n)))) nil)
            (tagbody
              (powp
               (f2cl-lib:fref kdeg-%data%
                              (j np1 k)
                              ((1 n) (1 (f2cl-lib:int-add n 1)) (1 mmaxt))
                              kdeg-%offset%)
               xnp1
               (f2cl-lib:array-slice xx-%data%
                                     double-float
                                     (1 j np1 k)
                                     ((1 2) (1 n) (1 (f2cl-lib:int-add n 1))
                                      (1 mmaxt))
                                     xx-%offset%))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l n) nil)
                (tagbody
                  (powp
                   (f2cl-lib:fref kdeg-%data%
                                  (j l k)
                                  ((1 n) (1 (f2cl-lib:int-add n 1)) (1 mmaxt))
                                  kdeg-%offset%)
                   (f2cl-lib:array-slice x-%data%
                                         double-float
                                         (1 l)
                                         ((1 2) (1 n))
                                         x-%offset%)
                   (f2cl-lib:array-slice xx-%data%
                                         double-float
                                         (1 j l k)
                                         ((1 2) (1 n)
                                          (1 (f2cl-lib:int-add n 1)) (1 mmaxt))
                                         xx-%offset%))
                 label100))))))
     label100
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 n)))) nil)
            (tagbody
              (setf (f2cl-lib:fref trm-%data%
                                   (1 j k)
                                   ((1 2) (1 n) (1 mmaxt))
                                   trm-%offset%)
                      (f2cl-lib:fref coef-%data%
                                     (j k)
                                     ((1 n) (1 mmaxt))
                                     coef-%offset%))
              (setf (f2cl-lib:fref trm-%data%
                                   (2 j k)
                                   ((1 2) (1 n) (1 mmaxt))
                                   trm-%offset%)
                      (coerce 0.0f0 'double-float))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l np1) nil)
                (tagbody
                  (mulp
                   (f2cl-lib:array-slice xx-%data%
                                         double-float
                                         (1 j l k)
                                         ((1 2) (1 n)
                                          (1 (f2cl-lib:int-add n 1)) (1 mmaxt))
                                         xx-%offset%)
                   (f2cl-lib:array-slice trm-%data%
                                         double-float
                                         (1 j k)
                                         ((1 2) (1 n) (1 mmaxt))
                                         trm-%offset%)
                   temp1)
                  (setf (f2cl-lib:fref trm-%data%
                                       (1 j k)
                                       ((1 2) (1 n) (1 mmaxt))
                                       trm-%offset%)
                          (f2cl-lib:fref temp1 (1) ((1 2))))
                  (setf (f2cl-lib:fref trm-%data%
                                       (2 j k)
                                       ((1 2) (1 n) (1 mmaxt))
                                       trm-%offset%)
                          (f2cl-lib:fref temp1 (2) ((1 2))))
                 label120))
             label200))))
     label200
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 2) (1 n)) f-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref f-%data% (2 j) ((1 2) (1 n)) f-%offset%)
                  (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 2) nil)
            (tagbody
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k (f2cl-lib:fref numt (j) ((1 n)))) nil)
                (tagbody
                  (setf (f2cl-lib:fref f-%data% (i j) ((1 2) (1 n)) f-%offset%)
                          (+
                           (f2cl-lib:fref f-%data%
                                          (i j)
                                          ((1 2) (1 n))
                                          f-%offset%)
                           (f2cl-lib:fref trm-%data%
                                          (i j k)
                                          ((1 2) (1 n) (1 mmaxt))
                                          trm-%offset%)))
                 label220))))
         label220
         label300))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 n)))) nil)
            (tagbody
              (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                            ((> m np1) nil)
                (tagbody
                  (cond
                    ((=
                      (f2cl-lib:fref kdeg
                                     (j m k)
                                     ((1 n) (1 (f2cl-lib:int-add n 1))
                                      (1 mmaxt)))
                      0)
                     (setf (f2cl-lib:fref dtrm-%data%
                                          (1 j m k)
                                          ((1 2) (1 n)
                                           (1 (f2cl-lib:int-add n 1))
                                           (1 mmaxt))
                                          dtrm-%offset%)
                             (coerce 0.0f0 'double-float))
                     (setf (f2cl-lib:fref dtrm-%data%
                                          (2 j m k)
                                          ((1 2) (1 n)
                                           (1 (f2cl-lib:int-add n 1))
                                           (1 mmaxt))
                                          dtrm-%offset%)
                             (coerce 0.0f0 'double-float)))
                    (t
                     (if (<= m n)
                         (multiple-value-bind (var-0 var-1 var-2 var-3)
                             (divp
                              (f2cl-lib:array-slice trm-%data%
                                                    double-float
                                                    (1 j k)
                                                    ((1 2) (1 n) (1 mmaxt))
                                                    trm-%offset%)
                              (f2cl-lib:array-slice x-%data%
                                                    double-float
                                                    (1 m)
                                                    ((1 2) (1 n))
                                                    x-%offset%)
                              (f2cl-lib:array-slice dtrm-%data%
                                                    double-float
                                                    (1 j m k)
                                                    ((1 2) (1 n)
                                                     (1 (f2cl-lib:int-add n 1))
                                                     (1 mmaxt))
                                                    dtrm-%offset%)
                              ierr)
                           (declare (ignore var-0 var-1 var-2))
                           (setf ierr var-3)))
                     (if (= m np1)
                         (multiple-value-bind (var-0 var-1 var-2 var-3)
                             (divp
                              (f2cl-lib:array-slice trm-%data%
                                                    double-float
                                                    (1 j k)
                                                    ((1 2) (1 n) (1 mmaxt))
                                                    trm-%offset%)
                              xnp1
                              (f2cl-lib:array-slice dtrm-%data%
                                                    double-float
                                                    (1 j m k)
                                                    ((1 2) (1 n)
                                                     (1 (f2cl-lib:int-add n 1))
                                                     (1 mmaxt))
                                                    dtrm-%offset%)
                              ierr)
                           (declare (ignore var-0 var-1 var-2))
                           (setf ierr var-3)))
                     (cond
                       ((= ierr 0)
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (1 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (*
                                 (f2cl-lib:fref kdeg-%data%
                                                (j m k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                kdeg-%offset%)
                                 (f2cl-lib:fref dtrm-%data%
                                                (1 j m k)
                                                ((1 2) (1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                dtrm-%offset%)))
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (2 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (*
                                 (f2cl-lib:fref kdeg-%data%
                                                (j m k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                kdeg-%offset%)
                                 (f2cl-lib:fref dtrm-%data%
                                                (2 j m k)
                                                ((1 2) (1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                dtrm-%offset%))))
                       (t
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (1 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (f2cl-lib:fref coef-%data%
                                               (j k)
                                               ((1 n) (1 mmaxt))
                                               coef-%offset%))
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (2 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (coerce 0.0f0 'double-float))
                        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                                      ((> l np1) nil)
                          (tagbody
                            (if (= l m) (go label320))
                            (mulp
                             (f2cl-lib:array-slice xx-%data%
                                                   double-float
                                                   (1 j l k)
                                                   ((1 2) (1 n)
                                                    (1 (f2cl-lib:int-add n 1))
                                                    (1 mmaxt))
                                                   xx-%offset%)
                             (f2cl-lib:array-slice dtrm-%data%
                                                   double-float
                                                   (1 j m k)
                                                   ((1 2) (1 n)
                                                    (1 (f2cl-lib:int-add n 1))
                                                    (1 mmaxt))
                                                   dtrm-%offset%)
                             temp1)
                            (setf (f2cl-lib:fref dtrm-%data%
                                                 (1 j m k)
                                                 ((1 2) (1 n)
                                                  (1 (f2cl-lib:int-add n 1))
                                                  (1 mmaxt))
                                                 dtrm-%offset%)
                                    (f2cl-lib:fref temp1 (1) ((1 2))))
                            (setf (f2cl-lib:fref dtrm-%data%
                                                 (2 j m k)
                                                 ((1 2) (1 n)
                                                  (1 (f2cl-lib:int-add n 1))
                                                  (1 mmaxt))
                                                 dtrm-%offset%)
                                    (f2cl-lib:fref temp1 (2) ((1 2))))
                           label320))
                        (setf nnnn
                                (f2cl-lib:int-sub
                                 (f2cl-lib:fref kdeg-%data%
                                                (j m k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                kdeg-%offset%)
                                 1))
                        (if (<= m n)
                            (powp nnnn
                             (f2cl-lib:array-slice x-%data%
                                                   double-float
                                                   (1 m)
                                                   ((1 2) (1 n))
                                                   x-%offset%)
                             temp2))
                        (if (= m np1) (powp nnnn xnp1 temp2))
                        (mulp temp2 temp1
                         (f2cl-lib:array-slice dtrm-%data%
                                               double-float
                                               (1 j m k)
                                               ((1 2) (1 n)
                                                (1 (f2cl-lib:int-add n 1))
                                                (1 mmaxt))
                                               dtrm-%offset%))
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (1 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (*
                                 (f2cl-lib:fref kdeg-%data%
                                                (j m k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                kdeg-%offset%)
                                 (f2cl-lib:fref dtrm-%data%
                                                (1 j m k)
                                                ((1 2) (1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                dtrm-%offset%)))
                        (setf (f2cl-lib:fref dtrm-%data%
                                             (2 j m k)
                                             ((1 2) (1 n)
                                              (1 (f2cl-lib:int-add n 1))
                                              (1 mmaxt))
                                             dtrm-%offset%)
                                (*
                                 (f2cl-lib:fref kdeg-%data%
                                                (j m k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                kdeg-%offset%)
                                 (f2cl-lib:fref dtrm-%data%
                                                (2 j m k)
                                                ((1 2) (1 n)
                                                 (1 (f2cl-lib:int-add n 1))
                                                 (1 mmaxt))
                                                dtrm-%offset%)))))))
                 label400))))))
     label400
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                        ((> m np1) nil)
            (tagbody
              (setf (f2cl-lib:fref df-%data%
                                   (1 j m)
                                   ((1 2) (1 n) (1 (f2cl-lib:int-add n 1)))
                                   df-%offset%)
                      (coerce 0.0f0 'double-float))
              (setf (f2cl-lib:fref df-%data%
                                   (2 j m)
                                   ((1 2) (1 n) (1 (f2cl-lib:int-add n 1)))
                                   df-%offset%)
                      (coerce 0.0f0 'double-float))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i 2) nil)
                (tagbody
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k (f2cl-lib:fref numt (j) ((1 n)))) nil)
                    (tagbody
                      (setf (f2cl-lib:fref df-%data%
                                           (i j m)
                                           ((1 2) (1 n)
                                            (1 (f2cl-lib:int-add n 1)))
                                           df-%offset%)
                              (+
                               (f2cl-lib:fref df-%data%
                                              (i j m)
                                              ((1 2) (1 n)
                                               (1 (f2cl-lib:int-add n 1)))
                                              df-%offset%)
                               (f2cl-lib:fref dtrm-%data%
                                              (i j m k)
                                              ((1 2) (1 n)
                                               (1 (f2cl-lib:int-add n 1))
                                               (1 mmaxt))
                                              dtrm-%offset%)))
                     label420))))
             label420
             label600))))
     label600
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k n) nil)
            (tagbody
              (mulp
               (f2cl-lib:array-slice df-%data%
                                     double-float
                                     (1 j np1)
                                     ((1 2) (1 n) (1 (f2cl-lib:int-add n 1)))
                                     df-%offset%)
               (f2cl-lib:array-slice dxnp1-%data%
                                     double-float
                                     (1 k)
                                     ((1 2) (1 n))
                                     dxnp1-%offset%)
               temp1)
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i 2) nil)
                (tagbody
                  (setf (f2cl-lib:fref df-%data%
                                       (i j k)
                                       ((1 2) (1 n) (1 (f2cl-lib:int-add n 1)))
                                       df-%offset%)
                          (+
                           (f2cl-lib:fref df-%data%
                                          (i j k)
                                          ((1 2) (1 n)
                                           (1 (f2cl-lib:int-add n 1)))
                                          df-%offset%)
                           (f2cl-lib:fref temp1 (i) ((1 2)))))
                 label700))))))
     label700
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ffunp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::divp fortran-to-lisp::powp
                    fortran-to-lisp::mulp))))

