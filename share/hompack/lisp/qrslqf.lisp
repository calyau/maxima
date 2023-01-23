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


(defun qrslqf (qt r b x n)
  (declare (type (f2cl-lib:integer4) n)
           (type (array double-float (*)) x b r qt))
  (f2cl-lib:with-multi-array-data
      ((qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%)
       (b double-float b-%data% b-%offset%)
       (x double-float x-%data% x-%offset%))
    (prog ((indexr 0) (i 0) (j 0) (tau 0.0))
      (declare (type (double-float) tau) (type (f2cl-lib:integer4) j i indexr))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%)
                  (ddot n
                   (f2cl-lib:array-slice qt-%data%
                                         double-float
                                         (i 1)
                                         ((1 n) (1 n))
                                         qt-%offset%)
                   n b 1))
         label10))
      (setf indexr (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2)))
      (setf (f2cl-lib:fref b-%data% (n) ((1 n)) b-%offset%)
              (/ (f2cl-lib:fref x-%data% (n) ((1 n)) x-%offset%)
                 (f2cl-lib:fref r-%data%
                                (indexr)
                                ((1
                                  (f2cl-lib:f2cl/
                                   (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                   2)))
                                r-%offset%)))
      (setf indexr (f2cl-lib:int-sub indexr 1))
      (f2cl-lib:fdo (i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                     (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                    ((> i 1) nil)
        (tagbody
          (setf tau (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%))
          (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                        ((> j (f2cl-lib:int-add i 1)) nil)
            (tagbody
              (setf tau
                      (- tau
                         (*
                          (f2cl-lib:fref r-%data%
                                         (indexr)
                                         ((1
                                           (f2cl-lib:f2cl/
                                            (f2cl-lib:int-mul n
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            2)))
                                         r-%offset%)
                          (f2cl-lib:fref b-%data% (j) ((1 n)) b-%offset%))))
              (setf indexr (f2cl-lib:int-sub indexr 1))
             label20))
          (setf (f2cl-lib:fref b-%data% (i) ((1 n)) b-%offset%)
                  (/ tau
                     (f2cl-lib:fref r-%data%
                                    (indexr)
                                    ((1
                                      (f2cl-lib:f2cl/
                                       (f2cl-lib:int-mul n
                                                         (f2cl-lib:int-add n
                                                                           1))
                                       2)))
                                    r-%offset%)))
          (setf indexr (f2cl-lib:int-sub indexr 1))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::qrslqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil)
           :calls '(fortran-to-lisp::ddot))))

