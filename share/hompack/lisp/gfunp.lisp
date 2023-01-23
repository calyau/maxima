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


(defun gfunp (n ideg pdg qdg x xdgm1 xdg pxdgm1 pxdg g dg)
  (declare (type (array double-float (*)) dg g pxdg pxdgm1 xdg xdgm1 x qdg pdg)
           (type (array f2cl-lib:integer4 (*)) ideg)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((ideg f2cl-lib:integer4 ideg-%data% ideg-%offset%)
       (pdg double-float pdg-%data% pdg-%offset%)
       (qdg double-float qdg-%data% qdg-%offset%)
       (x double-float x-%data% x-%offset%)
       (xdgm1 double-float xdgm1-%data% xdgm1-%offset%)
       (xdg double-float xdg-%data% xdg-%offset%)
       (pxdgm1 double-float pxdgm1-%data% pxdgm1-%offset%)
       (pxdg double-float pxdg-%data% pxdg-%offset%)
       (g double-float g-%data% g-%offset%)
       (dg double-float dg-%data% dg-%offset%))
    (prog ((i 0) (j 0))
      (declare (type (f2cl-lib:integer4) j i))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (powp
           (f2cl-lib:int-sub
            (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
            1)
           (f2cl-lib:array-slice x-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 x-%offset%)
           (f2cl-lib:array-slice xdgm1-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 xdgm1-%offset%))
          (mulp
           (f2cl-lib:array-slice x-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 x-%offset%)
           (f2cl-lib:array-slice xdgm1-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 xdgm1-%offset%)
           (f2cl-lib:array-slice xdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 xdg-%offset%))
         label5))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (mulp
           (f2cl-lib:array-slice pdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 pdg-%offset%)
           (f2cl-lib:array-slice xdgm1-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 xdgm1-%offset%)
           (f2cl-lib:array-slice pxdgm1-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 pxdgm1-%offset%))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (mulp
           (f2cl-lib:array-slice pdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 pdg-%offset%)
           (f2cl-lib:array-slice xdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 xdg-%offset%)
           (f2cl-lib:array-slice pxdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 pxdg-%offset%))
         label20))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 2) nil)
            (tagbody
              (setf (f2cl-lib:fref g-%data% (i j) ((1 2) (1 n)) g-%offset%)
                      (-
                       (f2cl-lib:fref pxdg-%data%
                                      (i j)
                                      ((1 2) (1 n))
                                      pxdg-%offset%)
                       (f2cl-lib:fref qdg-%data%
                                      (i j)
                                      ((1 2) (1 n))
                                      qdg-%offset%)))
              (setf (f2cl-lib:fref dg-%data% (i j) ((1 2) (1 n)) dg-%offset%)
                      (* (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
                         (f2cl-lib:fref pxdgm1-%data%
                                        (i j)
                                        ((1 2) (1 n))
                                        pxdgm1-%offset%)))
             label30))))
     label30
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::gfunp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::mulp fortran-to-lisp::powp))))

