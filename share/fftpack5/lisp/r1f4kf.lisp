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
;;;           (:float-format single-float))

(in-package "FFTPACK5")


(defun r1f4kf (ido l1 cc in1 ch in2 wa1 wa2 wa3)
  (declare (type (array double-float (*)) wa3 wa2 wa1 ch cc)
           (type (f2cl-lib:integer4) in2 in1 l1 ido))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (wa1 double-float wa1-%data% wa1-%offset%)
       (wa2 double-float wa2-%data% wa2-%offset%)
       (wa3 double-float wa3-%data% wa3-%offset%))
    (prog ((ic 0) (i 0) (idp2 0) (k 0) (hsqt2 0.0d0))
      (declare (type (double-float) hsqt2)
               (type (f2cl-lib:integer4) k idp2 i ic))
      (setf hsqt2 (/ (f2cl-lib:fsqrt 2.0d0) 2.0d0))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (setf (f2cl-lib:fref ch-%data%
                               (1 1 1 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (+
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 2)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 4)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)
                   (+
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 1)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%)
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 3)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%))))
          (setf (f2cl-lib:fref ch-%data%
                               (1 ido 4 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (-
                   (+
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 1)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%)
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 3)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%))
                   (+
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 2)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%)
                    (f2cl-lib:fref cc-%data%
                                   (1 1 k 4)
                                   ((1 in1) (1 ido) (1 l1) (1 4))
                                   cc-%offset%))))
          (setf (f2cl-lib:fref ch-%data%
                               (1 ido 2 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 1)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 3)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (1 1 3 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 4)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 1 k 2)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)))
         label101))
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub ido 2)
                              (go label107)
                              (go label105)
                              (go label102))
     label102
      (setf idp2 (f2cl-lib:int-add ido 2))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                        ((> i ido) nil)
            (tagbody
              (setf ic (f2cl-lib:int-sub idp2 i))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 (f2cl-lib:int-sub i 1) 1 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (+
                       (*
                        (f2cl-lib:fref wa1-%data%
                                       ((f2cl-lib:int-sub i 2))
                                       ((1 ido))
                                       wa1-%offset%)
                        (f2cl-lib:fref cc-%data%
                                       (1 (f2cl-lib:int-sub i 1) k 2)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%))
                       (*
                        (f2cl-lib:fref wa1-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 ido))
                                       wa1-%offset%)
                        (f2cl-lib:fref cc-%data%
                                       (1 i k 2)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%))
                       (+
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (+
                        (f2cl-lib:fref cc-%data%
                                       (1 (f2cl-lib:int-sub i 1) k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 (f2cl-lib:int-sub ic 1) 4 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (-
                       (+
                        (f2cl-lib:fref cc-%data%
                                       (1 (f2cl-lib:int-sub i 1) k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))
                       (+
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (+
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 i 1 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (+
                       (-
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (-
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (+
                        (f2cl-lib:fref cc-%data%
                                       (1 i k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 ic 4 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (-
                       (+
                        (-
                         (*
                          (f2cl-lib:fref wa1-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa1-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 2)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa1-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa1-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 2)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%)))
                        (-
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))
                       (+
                        (f2cl-lib:fref cc-%data%
                                       (1 i k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 (f2cl-lib:int-sub i 1) 3 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (+
                       (-
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (-
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))
                       (-
                        (f2cl-lib:fref cc-%data%
                                       (1 (f2cl-lib:int-sub i 1) k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 (f2cl-lib:int-sub ic 1) 2 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 (f2cl-lib:int-sub i 1) k 1)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)
                       (+
                        (*
                         (f2cl-lib:fref wa2-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa2-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 3)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa2-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa2-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 3)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (-
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (-
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 i 3 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (+
                       (-
                        (+
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa3-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa3-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 4)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%)))
                        (+
                         (*
                          (f2cl-lib:fref wa1-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa1-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 2)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa1-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa1-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 2)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))
                       (-
                        (f2cl-lib:fref cc-%data%
                                       (1 i k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 ic 2 k)
                                   ((1 in2) (1 ido) (1 4) (1 l1))
                                   ch-%offset%)
                      (-
                       (+
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa3-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa3-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 4)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (+
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 2))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 (f2cl-lib:int-sub i 1) k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%))
                        (*
                         (f2cl-lib:fref wa1-%data%
                                        ((f2cl-lib:int-sub i 1))
                                        ((1 ido))
                                        wa1-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 i k 2)
                                        ((1 in1) (1 ido) (1 l1) (1 4))
                                        cc-%offset%)))
                       (-
                        (f2cl-lib:fref cc-%data%
                                       (1 i k 1)
                                       ((1 in1) (1 ido) (1 l1) (1 4))
                                       cc-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 2))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 i k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))
                         (*
                          (f2cl-lib:fref wa2-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 ido))
                                         wa2-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (1 (f2cl-lib:int-sub i 1) k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 4))
                                         cc-%offset%))))))
             label103))
         label104))
      (if (= (mod ido 2) 1) (go end_label))
     label105
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (setf (f2cl-lib:fref ch-%data%
                               (1 ido 1 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (+
                   (* hsqt2
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 2)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 4)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)))
                   (f2cl-lib:fref cc-%data%
                                  (1 ido k 1)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (1 ido 3 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (1 ido k 1)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)
                   (* hsqt2
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 2)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 4)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)))))
          (setf (f2cl-lib:fref ch-%data%
                               (1 1 2 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (-
                   (* (- hsqt2)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 2)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 4)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)))
                   (f2cl-lib:fref cc-%data%
                                  (1 ido k 3)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (1 1 4 k)
                               ((1 in2) (1 ido) (1 4) (1 l1))
                               ch-%offset%)
                  (+
                   (* (- hsqt2)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 2)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 ido k 4)
                                      ((1 in1) (1 ido) (1 l1) (1 4))
                                      cc-%offset%)))
                   (f2cl-lib:fref cc-%data%
                                  (1 ido k 3)
                                  ((1 in1) (1 ido) (1 l1) (1 4))
                                  cc-%offset%)))
         label106))
     label107
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::r1f4kf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls 'nil)))

