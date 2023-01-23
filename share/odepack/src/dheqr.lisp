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


(defun dheqr (a lda n q info ijob)
  (declare (type (f2cl-lib:integer4) ijob info n lda)
           (type (array double-float (*)) q a))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (q double-float q-%data% q-%offset%))
    (prog ((c 0.0d0) (s 0.0d0) (t$ 0.0d0) (t1 0.0d0) (t2 0.0d0) (i 0) (iq 0)
           (j 0) (k 0) (km1 0) (kp1 0) (nm1 0))
      (declare (type (f2cl-lib:integer4) nm1 kp1 km1 k j iq i)
               (type (double-float) t2 t1 t$ s c))
      (if (> ijob 1) (go label70))
      (setf info 0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf km1 (f2cl-lib:int-sub k 1))
          (setf kp1 (f2cl-lib:int-add k 1))
          (if (< km1 1) (go label20))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j km1) nil)
            (tagbody
              (setf i
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul 2 (f2cl-lib:int-sub j 1))
                       1))
              (setf t1
                      (f2cl-lib:fref a-%data%
                                     (j k)
                                     ((1 lda) (1 *))
                                     a-%offset%))
              (setf t2
                      (f2cl-lib:fref a-%data%
                                     ((f2cl-lib:int-add j 1) k)
                                     ((1 lda) (1 *))
                                     a-%offset%))
              (setf c (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%))
              (setf s
                      (f2cl-lib:fref q-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 *))
                                     q-%offset%))
              (setf (f2cl-lib:fref a-%data% (j k) ((1 lda) (1 *)) a-%offset%)
                      (- (* c t1) (* s t2)))
              (setf (f2cl-lib:fref a-%data%
                                   ((f2cl-lib:int-add j 1) k)
                                   ((1 lda) (1 *))
                                   a-%offset%)
                      (+ (* s t1) (* c t2)))
             label10))
         label20
          (setf iq (f2cl-lib:int-add (f2cl-lib:int-mul 2 km1) 1))
          (setf t1 (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 *)) a-%offset%))
          (setf t2 (f2cl-lib:fref a-%data% (kp1 k) ((1 lda) (1 *)) a-%offset%))
          (if (/= t2 0.0d0) (go label30))
          (setf c 1.0d0)
          (setf s 0.0d0)
          (go label50)
         label30
          (if (< (abs t2) (abs t1)) (go label40))
          (setf t$ (/ t1 t2))
          (setf s (/ -1.0d0 (f2cl-lib:fsqrt (+ 1.0d0 (* t$ t$)))))
          (setf c (* (- s) t$))
          (go label50)
         label40
          (setf t$ (/ t2 t1))
          (setf c (/ 1.0d0 (f2cl-lib:fsqrt (+ 1.0d0 (* t$ t$)))))
          (setf s (* (- c) t$))
         label50
          (setf (f2cl-lib:fref q-%data% (iq) ((1 *)) q-%offset%) c)
          (setf (f2cl-lib:fref q-%data%
                               ((f2cl-lib:int-add iq 1))
                               ((1 *))
                               q-%offset%)
                  s)
          (setf (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 *)) a-%offset%)
                  (- (* c t1) (* s t2)))
          (if
           (= (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 *)) a-%offset%) 0.0d0)
           (setf info k))
         label60))
      (go end_label)
     label70
      (setf nm1 (f2cl-lib:int-sub n 1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf i
                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 (f2cl-lib:int-sub k 1))
                                    1))
          (setf t1 (f2cl-lib:fref a-%data% (k n) ((1 lda) (1 *)) a-%offset%))
          (setf t2
                  (f2cl-lib:fref a-%data%
                                 ((f2cl-lib:int-add k 1) n)
                                 ((1 lda) (1 *))
                                 a-%offset%))
          (setf c (f2cl-lib:fref q-%data% (i) ((1 *)) q-%offset%))
          (setf s
                  (f2cl-lib:fref q-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 q-%offset%))
          (setf (f2cl-lib:fref a-%data% (k n) ((1 lda) (1 *)) a-%offset%)
                  (- (* c t1) (* s t2)))
          (setf (f2cl-lib:fref a-%data%
                               ((f2cl-lib:int-add k 1) n)
                               ((1 lda) (1 *))
                               a-%offset%)
                  (+ (* s t1) (* c t2)))
         label100))
      (setf info 0)
      (setf t1 (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%))
      (setf t2
              (f2cl-lib:fref a-%data%
                             ((f2cl-lib:int-add n 1) n)
                             ((1 lda) (1 *))
                             a-%offset%))
      (if (/= t2 0.0d0) (go label110))
      (setf c 1.0d0)
      (setf s 0.0d0)
      (go label130)
     label110
      (if (< (abs t2) (abs t1)) (go label120))
      (setf t$ (/ t1 t2))
      (setf s (/ -1.0d0 (f2cl-lib:fsqrt (+ 1.0d0 (* t$ t$)))))
      (setf c (* (- s) t$))
      (go label130)
     label120
      (setf t$ (/ t2 t1))
      (setf c (/ 1.0d0 (f2cl-lib:fsqrt (+ 1.0d0 (* t$ t$)))))
      (setf s (* (- c) t$))
     label130
      (setf iq (f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))
      (setf (f2cl-lib:fref q-%data% (iq) ((1 *)) q-%offset%) c)
      (setf (f2cl-lib:fref q-%data%
                           ((f2cl-lib:int-add iq 1))
                           ((1 *))
                           q-%offset%)
              s)
      (setf (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%)
              (- (* c t1) (* s t2)))
      (if (= (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%) 0.0d0)
          (setf info n))
      (go end_label)
     end_label
      (return (values nil nil nil nil info nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dheqr fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::info nil)
           :calls 'nil)))

