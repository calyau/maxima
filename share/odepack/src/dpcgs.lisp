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


(defun dpcgs
       (neq tn y savf r wght n maxl delta hl0 jpre mnewt f psol npsl x p w z
        lpcg wp iwp wk iflag)
  (declare (type (f2cl-lib:integer4) iflag lpcg npsl mnewt jpre maxl n)
           (type (array double-float (*)) wk wp z w p x wght r savf y)
           (type (double-float) hl0 delta tn)
           (type (array f2cl-lib:integer4 (*)) iwp neq))
  (f2cl-lib:with-multi-array-data
      ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
       (iwp f2cl-lib:integer4 iwp-%data% iwp-%offset%)
       (y double-float y-%data% y-%offset%)
       (savf double-float savf-%data% savf-%offset%)
       (r double-float r-%data% r-%offset%)
       (wght double-float wght-%data% wght-%offset%)
       (x double-float x-%data% x-%offset%)
       (p double-float p-%data% p-%offset%)
       (w double-float w-%data% w-%offset%)
       (z double-float z-%data% z-%offset%)
       (wp double-float wp-%data% wp-%offset%)
       (wk double-float wk-%data% wk-%offset%))
    (prog ((alpha 0.0d0) (beta 0.0d0) (bnrm 0.0d0) (ptw 0.0d0) (rnrm 0.0d0)
           (ztr 0.0d0) (ztr0 0.0d0) (i 0) (ier 0))
      (declare (type (f2cl-lib:integer4) ier i)
               (type (double-float) ztr0 ztr rnrm ptw bnrm beta alpha))
      (setf iflag 0)
      (setf npsl 0)
      (setf lpcg 0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%) 0.0d0)))
      (setf bnrm (dvnorm n r wght))
      (if (> bnrm delta) (go label20))
      (if (> mnewt 0) (go end_label))
      (dcopy n r 1 x 1)
      (go end_label)
     label20
      (setf ztr 0.0d0)
     label30
      (setf lpcg (f2cl-lib:int-add lpcg 1))
      (dcopy n r 1 z 1)
      (setf ier 0)
      (if (= jpre 0) (go label40))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (funcall psol neq tn y savf wk hl0 wp iwp z 3 ier)
        (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
        (when var-1
          (setf tn var-1))
        (when var-5
          (setf hl0 var-5))
        (when var-10
          (setf ier var-10)))
      (setf npsl (f2cl-lib:int-add npsl 1))
      (if (/= ier 0) (go label100))
     label40
      (setf ztr0 ztr)
      (setf ztr 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label45
          (setf ztr
                  (+ ztr
                     (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                        (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%)
                        (expt
                         (f2cl-lib:fref wght-%data% (i) ((1 *)) wght-%offset%)
                         2))))))
      (if (/= lpcg 1) (go label50))
      (dcopy n z 1 p 1)
      (go label70)
     label50
      (if (= ztr0 0.0d0) (go label200))
      (setf beta (/ ztr ztr0))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label60
          (setf (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%)
                  (+ (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                     (* beta
                        (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%))))))
     label70
      (datp neq y savf p wght hl0 wk f w)
      (setf ptw 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label80
          (setf ptw
                  (+ ptw
                     (* (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%)
                        (f2cl-lib:fref w-%data% (i) ((1 *)) w-%offset%)
                        (expt
                         (f2cl-lib:fref wght-%data% (i) ((1 *)) wght-%offset%)
                         2))))))
      (if (= ptw 0.0d0) (go label200))
      (setf alpha (/ ztr ptw))
      (daxpy n alpha p 1 x 1)
      (setf alpha (- alpha))
      (daxpy n alpha w 1 r 1)
      (setf rnrm (dvnorm n r wght))
      (if (<= rnrm delta) (go end_label))
      (if (< lpcg maxl) (go label30))
      (setf iflag 2)
      (if (<= rnrm 1.0d0) (setf iflag 1))
      (if (and (<= rnrm bnrm) (= mnewt 0)) (setf iflag 1))
      (go end_label)
     label100
      (if (< ier 0) (setf iflag -1))
      (if (> ier 0) (setf iflag 3))
      (go end_label)
     label200
      (setf iflag 4)
      (go end_label)
     end_label
      (return
       (values nil
               tn
               nil
               nil
               nil
               nil
               nil
               nil
               nil
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
               lpcg
               nil
               nil
               nil
               iflag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dpcgs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        t t (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::tn nil nil nil nil nil nil nil
                            fortran-to-lisp::hl0 nil nil nil nil
                            fortran-to-lisp::npsl nil nil nil nil
                            fortran-to-lisp::lpcg nil nil nil
                            fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::datp
                    fortran-to-lisp::dcopy fortran-to-lisp::dvnorm))))

