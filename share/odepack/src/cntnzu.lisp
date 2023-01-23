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


(defun cntnzu (n ia ja nzsut)
  (declare (type (array f2cl-lib:integer4 (*)) ja ia)
           (type (f2cl-lib:integer4) nzsut n))
  (f2cl-lib:with-multi-array-data
      ((ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%))
    (prog ((ii 0) (jj 0) (j 0) (jmin 0) (jmax 0) (k 0) (kmin 0) (kmax 0)
           (num 0))
      (declare (type (f2cl-lib:integer4) num kmax kmin k jmax jmin j jj ii))
      (setf num 0)
      (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                    ((> ii n) nil)
        (tagbody
          (setf jmin (f2cl-lib:fref ia-%data% (ii) ((1 *)) ia-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add ii 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (if (> jmin jmax) (go label50))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (f2cl-lib:arithmetic-if
               (f2cl-lib:int-sub
                (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%)
                ii)
               (go label10)
               (go label40)
               (go label30))
             label10
              (setf jj (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%))
              (setf kmin (f2cl-lib:fref ia-%data% (jj) ((1 *)) ia-%offset%))
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref ia-%data%
                                      ((f2cl-lib:int-add jj 1))
                                      ((1 *))
                                      ia-%offset%)
                       1))
              (if (> kmin kmax) (go label30))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (if (= (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%) ii)
                      (go label40))
                 label20))
             label30
              (setf num (f2cl-lib:int-add num 1))
             label40))
         label50))
      (setf nzsut num)
      (go end_label)
     end_label
      (return (values nil nil nil nzsut)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cntnzu
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::nzsut)
           :calls 'nil)))

