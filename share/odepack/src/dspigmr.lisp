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


(defun dspigmr
       (neq tn y savf b wght n maxl maxlp1 kmp delta hl0 jpre mnewt f psol npsl
        x v hes q lgmr wp iwp wk dl iflag)
  (declare (type (f2cl-lib:integer4) iflag lgmr npsl mnewt jpre kmp maxlp1 maxl
                                     n)
           (type (array double-float (*)) dl wk wp q hes v x wght b savf y)
           (type (double-float) hl0 delta tn)
           (type (array f2cl-lib:integer4 (*)) iwp neq))
  (f2cl-lib:with-multi-array-data
      ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
       (iwp f2cl-lib:integer4 iwp-%data% iwp-%offset%)
       (y double-float y-%data% y-%offset%)
       (savf double-float savf-%data% savf-%offset%)
       (b double-float b-%data% b-%offset%)
       (wght double-float wght-%data% wght-%offset%)
       (x double-float x-%data% x-%offset%)
       (v double-float v-%data% v-%offset%)
       (hes double-float hes-%data% hes-%offset%)
       (q double-float q-%data% q-%offset%)
       (wp double-float wp-%data% wp-%offset%)
       (wk double-float wk-%data% wk-%offset%)
       (dl double-float dl-%data% dl-%offset%))
    (prog ((bnrm 0.0d0) (bnrm0 0.0d0) (c 0.0d0) (dlnrm 0.0d0) (prod 0.0d0)
           (rho 0.0d0) (s 0.0d0) (snormw 0.0d0) (tem 0.0d0) (i 0) (ier 0)
           (info 0) (ip1 0) (i2 0) (j 0) (k 0) (ll 0) (llp1 0))
      (declare (type (f2cl-lib:integer4) llp1 ll k j i2 ip1 info ier i)
               (type (double-float) tem snormw s rho prod dlnrm c bnrm0 bnrm))
      (setf iflag 0)
      (setf lgmr 0)
      (setf npsl 0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref v-%data% (i 1) ((1 n) (1 *)) v-%offset%)
                  (* (f2cl-lib:fref b-%data% (i) ((1 *)) b-%offset%)
                     (f2cl-lib:fref wght-%data% (i) ((1 *)) wght-%offset%)))))
      (setf bnrm0 (dnrm2 n v 1))
      (setf bnrm bnrm0)
      (if (> bnrm0 delta) (go label30))
      (if (> mnewt 0) (go label20))
      (dcopy n b 1 x 1)
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label25
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%) 0.0d0)))
      (go end_label)
     label30
      (setf ier 0)
      (if (or (= jpre 0) (= jpre 2)) (go label55))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (funcall psol neq tn y savf wk hl0 wp iwp b 1 ier)
        (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
        (when var-1
          (setf tn var-1))
        (when var-5
          (setf hl0 var-5))
        (when var-10
          (setf ier var-10)))
      (setf npsl 1)
      (if (/= ier 0) (go label300))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label50
          (setf (f2cl-lib:fref v-%data% (i 1) ((1 n) (1 *)) v-%offset%)
                  (* (f2cl-lib:fref b-%data% (i) ((1 *)) b-%offset%)
                     (f2cl-lib:fref wght-%data% (i) ((1 *)) wght-%offset%)))))
      (setf bnrm (dnrm2 n v 1))
      (setf delta (* delta (/ bnrm bnrm0)))
     label55
      (setf tem (/ 1.0d0 bnrm))
      (dscal n tem
       (f2cl-lib:array-slice v-%data%
                             double-float
                             (1 1)
                             ((1 n) (1 *))
                             v-%offset%)
       1)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j maxl) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i maxlp1) nil)
            (tagbody
             label60
              (setf (f2cl-lib:fref hes-%data%
                                   (i j)
                                   ((1 maxlp1) (1 *))
                                   hes-%offset%)
                      0.0d0)))
         label65))
      (setf prod 1.0d0)
      (f2cl-lib:fdo (ll 1 (f2cl-lib:int-add ll 1))
                    ((> ll maxl) nil)
        (tagbody
          (setf lgmr ll)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15)
              (datv neq y savf
               (f2cl-lib:array-slice v-%data%
                                     double-float
                                     (1 ll)
                                     ((1 n) (1 *))
                                     v-%offset%)
               wght x f psol
               (f2cl-lib:array-slice v-%data%
                                     double-float
                                     (1 (f2cl-lib:int-add ll 1))
                                     ((1 n) (1 *))
                                     v-%offset%)
               wk wp iwp hl0 jpre ier npsl)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf hl0 var-12)
            (setf ier var-14)
            (setf npsl var-15))
          (if (/= ier 0) (go label300))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (dorthog
               (f2cl-lib:array-slice v-%data%
                                     double-float
                                     (1 (f2cl-lib:int-add ll 1))
                                     ((1 n) (1 *))
                                     v-%offset%)
               v hes n ll maxlp1 kmp snormw)
            (declare (ignore var-0 var-1 var-2))
            (when var-3
              (setf n var-3))
            (when var-4
              (setf ll var-4))
            (when var-5
              (setf maxlp1 var-5))
            (when var-6
              (setf kmp var-6))
            (when var-7
              (setf snormw var-7)))
          (setf (f2cl-lib:fref hes-%data%
                               ((f2cl-lib:int-add ll 1) ll)
                               ((1 maxlp1) (1 *))
                               hes-%offset%)
                  snormw)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dheqr hes maxlp1 ll q info ll)
            (declare (ignore var-0 var-1 var-2 var-3 var-5))
            (setf info var-4))
          (if (= info ll) (go label120))
          (setf prod
                  (* prod
                     (f2cl-lib:fref q-%data%
                                    ((f2cl-lib:int-mul 2 ll))
                                    ((1 *))
                                    q-%offset%)))
          (setf rho (abs (* prod bnrm)))
          (cond
            ((and (> ll kmp) (< kmp maxl))
             (cond
               ((= ll (f2cl-lib:int-add kmp 1))
                (dcopy n
                 (f2cl-lib:array-slice v-%data%
                                       double-float
                                       (1 1)
                                       ((1 n) (1 *))
                                       v-%offset%)
                 1 dl 1)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i kmp) nil)
                  (tagbody
                    (setf ip1 (f2cl-lib:int-add i 1))
                    (setf i2 (f2cl-lib:int-mul i 2))
                    (setf s (f2cl-lib:fref q-%data% (i2) ((1 *)) q-%offset%))
                    (setf c
                            (f2cl-lib:fref q-%data%
                                           ((f2cl-lib:int-sub i2 1))
                                           ((1 *))
                                           q-%offset%))
                    (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                  ((> k n) nil)
                      (tagbody
                       label70
                        (setf (f2cl-lib:fref dl-%data% (k) ((1 *)) dl-%offset%)
                                (+
                                 (* s
                                    (f2cl-lib:fref dl-%data%
                                                   (k)
                                                   ((1 *))
                                                   dl-%offset%))
                                 (* c
                                    (f2cl-lib:fref v-%data%
                                                   (k ip1)
                                                   ((1 n) (1 *))
                                                   v-%offset%))))))
                   label75))))
             (setf s
                     (f2cl-lib:fref q-%data%
                                    ((f2cl-lib:int-mul 2 ll))
                                    ((1 *))
                                    q-%offset%))
             (setf c
                     (/
                      (f2cl-lib:fref q-%data%
                                     ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 ll)
                                                        1))
                                     ((1 *))
                                     q-%offset%)
                      snormw))
             (setf llp1 (f2cl-lib:int-add ll 1))
             (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                           ((> k n) nil)
               (tagbody
                label80
                 (setf (f2cl-lib:fref dl-%data% (k) ((1 *)) dl-%offset%)
                         (+
                          (* s
                             (f2cl-lib:fref dl-%data% (k) ((1 *)) dl-%offset%))
                          (* c
                             (f2cl-lib:fref v-%data%
                                            (k llp1)
                                            ((1 n) (1 *))
                                            v-%offset%))))))
             (setf dlnrm (dnrm2 n dl 1))
             (setf rho (* rho dlnrm))))
          (if (<= rho delta) (go label200))
          (if (= ll maxl) (go label100))
          (setf tem (/ 1.0d0 snormw))
          (dscal n tem
           (f2cl-lib:array-slice v-%data%
                                 double-float
                                 (1 (f2cl-lib:int-add ll 1))
                                 ((1 n) (1 *))
                                 v-%offset%)
           1)
         label90))
     label100
      (if (<= rho 1.0d0) (go label150))
      (if (and (<= rho bnrm) (= mnewt 0)) (go label150))
     label120
      (setf iflag 2)
      (go end_label)
     label150
      (setf iflag 1)
     label200
      (setf ll lgmr)
      (setf llp1 (f2cl-lib:int-add ll 1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k llp1) nil)
        (tagbody
         label210
          (setf (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%) 0.0d0)))
      (setf (f2cl-lib:fref b-%data% (1) ((1 *)) b-%offset%) bnrm)
      (dhels hes maxlp1 ll q b)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
         label220
          (setf (f2cl-lib:fref x-%data% (k) ((1 *)) x-%offset%) 0.0d0)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i ll) nil)
        (tagbody
          (daxpy n (f2cl-lib:fref b-%data% (i) ((1 *)) b-%offset%)
           (f2cl-lib:array-slice v-%data%
                                 double-float
                                 (1 i)
                                 ((1 n) (1 *))
                                 v-%offset%)
           1 x 1)
         label230))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label240
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                  (/ (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                     (f2cl-lib:fref wght-%data% (i) ((1 *)) wght-%offset%)))))
      (if (<= jpre 1) (go end_label))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (funcall psol neq tn y savf wk hl0 wp iwp x 2 ier)
        (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
        (when var-1
          (setf tn var-1))
        (when var-5
          (setf hl0 var-5))
        (when var-10
          (setf ier var-10)))
      (setf npsl (f2cl-lib:int-add npsl 1))
      (if (/= ier 0) (go label300))
      (go end_label)
     label300
      (if (< ier 0) (setf iflag -1))
      (if (> ier 0) (setf iflag 3))
      (go end_label)
     end_label
      (return
       (values nil
               tn
               nil
               nil
               nil
               nil
               n
               nil
               maxlp1
               kmp
               delta
               hl0
               nil
               nil
               nil
               nil
               npsl
               nil
               nil
               nil
               nil
               lgmr
               nil
               nil
               nil
               nil
               iflag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dspigmr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        t t (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::tn nil nil nil nil
                            fortran-to-lisp::n nil fortran-to-lisp::maxlp1
                            fortran-to-lisp::kmp fortran-to-lisp::delta
                            fortran-to-lisp::hl0 nil nil nil nil
                            fortran-to-lisp::npsl nil nil nil nil
                            fortran-to-lisp::lgmr nil nil nil nil
                            fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::dhels
                    fortran-to-lisp::dheqr fortran-to-lisp::datv
                    fortran-to-lisp::dscal fortran-to-lisp::dcopy
                    fortran-to-lisp::dnrm2))))

