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


(defun hfun1p
       (qdg lambda$ x pdg cl coef rho drhox drhol xdgm1 xdg g dg pxdgm1 pxdg f
        df xx trm dtrm clx dxnp1 n mmaxt ideg numt kdeg)
  (declare (type (array f2cl-lib:integer4 (*)) kdeg numt ideg)
           (type (f2cl-lib:integer4) mmaxt n)
           (type (double-float) lambda$)
           (type (array double-float (*)) dxnp1 clx dtrm trm xx df f pxdg
                                          pxdgm1 dg g xdg xdgm1 drhol drhox rho
                                          coef cl pdg x qdg))
  (f2cl-lib:with-multi-array-data
      ((qdg double-float qdg-%data% qdg-%offset%)
       (x double-float x-%data% x-%offset%)
       (pdg double-float pdg-%data% pdg-%offset%)
       (cl double-float cl-%data% cl-%offset%)
       (coef double-float coef-%data% coef-%offset%)
       (rho double-float rho-%data% rho-%offset%)
       (drhox double-float drhox-%data% drhox-%offset%)
       (drhol double-float drhol-%data% drhol-%offset%)
       (xdgm1 double-float xdgm1-%data% xdgm1-%offset%)
       (xdg double-float xdg-%data% xdg-%offset%)
       (g double-float g-%data% g-%offset%)
       (dg double-float dg-%data% dg-%offset%)
       (pxdgm1 double-float pxdgm1-%data% pxdgm1-%offset%)
       (pxdg double-float pxdg-%data% pxdg-%offset%)
       (f double-float f-%data% f-%offset%)
       (df double-float df-%data% df-%offset%)
       (xx double-float xx-%data% xx-%offset%)
       (trm double-float trm-%data% trm-%offset%)
       (dtrm double-float dtrm-%data% dtrm-%offset%)
       (clx double-float clx-%data% clx-%offset%)
       (dxnp1 double-float dxnp1-%data% dxnp1-%offset%)
       (ideg f2cl-lib:integer4 ideg-%data% ideg-%offset%)
       (numt f2cl-lib:integer4 numt-%data% numt-%offset%)
       (kdeg f2cl-lib:integer4 kdeg-%data% kdeg-%offset%))
    (prog ((oneml 0.0) (j 0) (j2 0) (j2m1 0) (k 0) (k2 0) (k2m1 0))
      (declare (type (f2cl-lib:integer4) k2m1 k2 k j2m1 j2 j)
               (type (double-float) oneml))
      (gfunp n ideg pdg qdg x xdgm1 xdg pxdgm1 pxdg g dg)
      (ffunp n numt mmaxt kdeg coef cl x xx trm dtrm clx dxnp1 f df)
      (setf oneml (- 1.0f0 lambda$))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf j2 (f2cl-lib:int-mul 2 j))
          (setf j2m1 (f2cl-lib:int-sub j2 1))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k n) nil)
            (tagbody
              (setf k2 (f2cl-lib:int-mul 2 k))
              (setf k2m1 (f2cl-lib:int-sub k2 1))
              (setf (f2cl-lib:fref drhox-%data%
                                   (j2m1 k2m1)
                                   ((1 (f2cl-lib:int-mul 2 n))
                                    (1 (f2cl-lib:int-mul 2 n)))
                                   drhox-%offset%)
                      (* lambda$
                         (f2cl-lib:fref df-%data%
                                        (1 j k)
                                        ((1 2) (1 n)
                                         (1 (f2cl-lib:int-add n 1)))
                                        df-%offset%)))
              (setf (f2cl-lib:fref drhox-%data%
                                   (j2 k2)
                                   ((1 (f2cl-lib:int-mul 2 n))
                                    (1 (f2cl-lib:int-mul 2 n)))
                                   drhox-%offset%)
                      (f2cl-lib:fref drhox-%data%
                                     (j2m1 k2m1)
                                     ((1 (f2cl-lib:int-mul 2 n))
                                      (1 (f2cl-lib:int-mul 2 n)))
                                     drhox-%offset%))
              (setf (f2cl-lib:fref drhox-%data%
                                   (j2 k2m1)
                                   ((1 (f2cl-lib:int-mul 2 n))
                                    (1 (f2cl-lib:int-mul 2 n)))
                                   drhox-%offset%)
                      (* lambda$
                         (f2cl-lib:fref df-%data%
                                        (2 j k)
                                        ((1 2) (1 n)
                                         (1 (f2cl-lib:int-add n 1)))
                                        df-%offset%)))
              (setf (f2cl-lib:fref drhox-%data%
                                   (j2m1 k2)
                                   ((1 (f2cl-lib:int-mul 2 n))
                                    (1 (f2cl-lib:int-mul 2 n)))
                                   drhox-%offset%)
                      (-
                       (f2cl-lib:fref drhox-%data%
                                      (j2 k2m1)
                                      ((1 (f2cl-lib:int-mul 2 n))
                                       (1 (f2cl-lib:int-mul 2 n)))
                                      drhox-%offset%)))
             label20))
          (setf (f2cl-lib:fref drhox-%data%
                               (j2m1 j2m1)
                               ((1 (f2cl-lib:int-mul 2 n))
                                (1 (f2cl-lib:int-mul 2 n)))
                               drhox-%offset%)
                  (+
                   (f2cl-lib:fref drhox-%data%
                                  (j2m1 j2m1)
                                  ((1 (f2cl-lib:int-mul 2 n))
                                   (1 (f2cl-lib:int-mul 2 n)))
                                  drhox-%offset%)
                   (* oneml
                      (f2cl-lib:fref dg-%data%
                                     (1 j)
                                     ((1 2) (1 n))
                                     dg-%offset%))))
          (setf (f2cl-lib:fref drhox-%data%
                               (j2 j2)
                               ((1 (f2cl-lib:int-mul 2 n))
                                (1 (f2cl-lib:int-mul 2 n)))
                               drhox-%offset%)
                  (f2cl-lib:fref drhox-%data%
                                 (j2m1 j2m1)
                                 ((1 (f2cl-lib:int-mul 2 n))
                                  (1 (f2cl-lib:int-mul 2 n)))
                                 drhox-%offset%))
          (setf (f2cl-lib:fref drhox-%data%
                               (j2 j2m1)
                               ((1 (f2cl-lib:int-mul 2 n))
                                (1 (f2cl-lib:int-mul 2 n)))
                               drhox-%offset%)
                  (+
                   (f2cl-lib:fref drhox-%data%
                                  (j2 j2m1)
                                  ((1 (f2cl-lib:int-mul 2 n))
                                   (1 (f2cl-lib:int-mul 2 n)))
                                  drhox-%offset%)
                   (* oneml
                      (f2cl-lib:fref dg-%data%
                                     (2 j)
                                     ((1 2) (1 n))
                                     dg-%offset%))))
          (setf (f2cl-lib:fref drhox-%data%
                               (j2m1 j2)
                               ((1 (f2cl-lib:int-mul 2 n))
                                (1 (f2cl-lib:int-mul 2 n)))
                               drhox-%offset%)
                  (-
                   (f2cl-lib:fref drhox-%data%
                                  (j2 j2m1)
                                  ((1 (f2cl-lib:int-mul 2 n))
                                   (1 (f2cl-lib:int-mul 2 n)))
                                  drhox-%offset%)))
          (setf (f2cl-lib:fref drhol-%data%
                               (j2m1)
                               ((1 (f2cl-lib:int-mul 2 n)))
                               drhol-%offset%)
                  (- (f2cl-lib:fref f-%data% (1 j) ((1 2) (1 n)) f-%offset%)
                     (f2cl-lib:fref g-%data% (1 j) ((1 2) (1 n)) g-%offset%)))
          (setf (f2cl-lib:fref drhol-%data%
                               (j2)
                               ((1 (f2cl-lib:int-mul 2 n)))
                               drhol-%offset%)
                  (- (f2cl-lib:fref f-%data% (2 j) ((1 2) (1 n)) f-%offset%)
                     (f2cl-lib:fref g-%data% (2 j) ((1 2) (1 n)) g-%offset%)))
          (setf (f2cl-lib:fref rho-%data%
                               (j2m1)
                               ((1 (f2cl-lib:int-mul 2 n)))
                               rho-%offset%)
                  (+
                   (* lambda$
                      (f2cl-lib:fref f-%data% (1 j) ((1 2) (1 n)) f-%offset%))
                   (* oneml
                      (f2cl-lib:fref g-%data%
                                     (1 j)
                                     ((1 2) (1 n))
                                     g-%offset%))))
          (setf (f2cl-lib:fref rho-%data%
                               (j2)
                               ((1 (f2cl-lib:int-mul 2 n)))
                               rho-%offset%)
                  (+
                   (* lambda$
                      (f2cl-lib:fref f-%data% (2 j) ((1 2) (1 n)) f-%offset%))
                   (* oneml
                      (f2cl-lib:fref g-%data%
                                     (2 j)
                                     ((1 2) (1 n))
                                     g-%offset%))))
         label30))
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hfun1p
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::ffunp fortran-to-lisp::gfunp))))

