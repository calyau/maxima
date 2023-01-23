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


(defun sro (n ip ia ja a q r dflag)
  (declare (type f2cl-lib:logical dflag)
           (type (array double-float (*)) a)
           (type (array f2cl-lib:integer4 (*)) r q ja ia ip)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((ip f2cl-lib:integer4 ip-%data% ip-%offset%)
       (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (q f2cl-lib:integer4 q-%data% q-%offset%)
       (r f2cl-lib:integer4 r-%data% r-%offset%)
       (a double-float a-%data% a-%offset%))
    (prog ((ak 0.0d0) (jak 0) (jdummy 0) (ilast 0) (k 0) (j 0) (jmax 0)
           (jmin 0) (i 0))
      (declare (type (f2cl-lib:integer4) i jmin jmax j k ilast jdummy jak)
               (type (double-float) ak))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label1
          (setf (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%) 0)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf jmin (f2cl-lib:fref ia-%data% (i) ((1 *)) ia-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (if (> jmin jmax) (go label3))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf k (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%))
              (if
               (< (f2cl-lib:fref ip-%data% (k) ((1 *)) ip-%offset%)
                  (f2cl-lib:fref ip-%data% (i) ((1 *)) ip-%offset%))
               (setf (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%) i))
              (if
               (>= (f2cl-lib:fref ip-%data% (k) ((1 *)) ip-%offset%)
                   (f2cl-lib:fref ip-%data% (i) ((1 *)) ip-%offset%))
               (setf k i))
              (setf (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%) k)
             label2
              (setf (f2cl-lib:fref q-%data% (k) ((1 *)) q-%offset%)
                      (f2cl-lib:int-add
                       (f2cl-lib:fref q-%data% (k) ((1 *)) q-%offset%)
                       1))))
         label3))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref ia-%data%
                               ((f2cl-lib:int-add i 1))
                               ((1 *))
                               ia-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref ia-%data% (i) ((1 *)) ia-%offset%)
                   (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%)))
         label4
          (setf (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%)
                  (f2cl-lib:fref ia-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 ia-%offset%))))
      (setf ilast 0)
      (setf jmin (f2cl-lib:fref ia-%data% (1) ((1 *)) ia-%offset%))
      (setf jmax
              (f2cl-lib:int-sub
               (f2cl-lib:fref ia-%data%
                              ((f2cl-lib:int-add n 1))
                              ((1 *))
                              ia-%offset%)
               1))
      (setf j jmax)
      (f2cl-lib:fdo (jdummy jmin (f2cl-lib:int-add jdummy 1))
                    ((> jdummy jmax) nil)
        (tagbody
          (setf i (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%))
          (if
           (or (not dflag)
               (/= (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%) i)
               (= i ilast))
           (go label5))
          (setf (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%)
                  (f2cl-lib:fref ia-%data% (i) ((1 *)) ia-%offset%))
          (setf ilast i)
          (go label6)
         label5
          (setf (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%)
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%)
                   1))
          (setf (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%)
                  (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%))
         label6
          (setf j (f2cl-lib:int-sub j 1))))
      (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                    ((> j jmax) nil)
        (tagbody
         label7
          (if (= (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%) j)
              (go label8))
          (setf k (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%))
          (setf (f2cl-lib:fref r-%data% (j) ((1 *)) r-%offset%)
                  (f2cl-lib:fref r-%data% (k) ((1 *)) r-%offset%))
          (setf (f2cl-lib:fref r-%data% (k) ((1 *)) r-%offset%) k)
          (setf jak (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%))
          (setf (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%)
                  (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%))
          (setf (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%) jak)
          (setf ak (f2cl-lib:fref a-%data% (k) ((1 *)) a-%offset%))
          (setf (f2cl-lib:fref a-%data% (k) ((1 *)) a-%offset%)
                  (f2cl-lib:fref a-%data% (j) ((1 *)) a-%offset%))
          (setf (f2cl-lib:fref a-%data% (j) ((1 *)) a-%offset%) ak)
          (go label7)
         label8))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::sro fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        fortran-to-lisp::logical)
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls 'nil)))

