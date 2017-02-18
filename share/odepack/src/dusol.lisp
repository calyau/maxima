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


(defun dusol
       (neq tn y savf b wght n delta hl0 mnewt psol npsl x wp iwp wk iflag)
  (declare (type (f2cl-lib:integer4) iflag npsl mnewt n)
           (type (array double-float (*)) wk wp x wght b savf y)
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
       (wp double-float wp-%data% wp-%offset%)
       (wk double-float wk-%data% wk-%offset%))
    (prog ((bnrm 0.0d0) (i 0) (ier 0))
      (declare (type (f2cl-lib:integer4) ier i) (type (double-float) bnrm))
      (setf iflag 0)
      (setf npsl 0)
      (setf bnrm (dvnorm n b wght))
      (if (> bnrm delta) (go label30))
      (if (> mnewt 0) (go label10))
      (dcopy n b 1 x 1)
      (go end_label)
     label10
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label20
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%) 0.0d0)))
      (go end_label)
     label30
      (setf ier 0)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (funcall psol neq tn y savf wk hl0 wp iwp b 0 ier)
        (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
        (when var-1
          (setf tn var-1))
        (when var-5
          (setf hl0 var-5))
        (when var-10
          (setf ier var-10)))
      (setf npsl 1)
      (if (/= ier 0) (go label100))
      (dcopy n b 1 x 1)
      (go end_label)
     label100
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
               nil
               nil
               hl0
               nil
               nil
               npsl
               nil
               nil
               nil
               nil
               iflag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dusol fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4) t
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::tn nil nil nil nil nil nil
                            fortran-to-lisp::hl0 nil nil fortran-to-lisp::npsl
                            nil nil nil nil fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::dvnorm))))

