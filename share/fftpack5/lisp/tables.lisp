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


(defun tables (ido ip wa)
  (declare (type (array double-float (*)) wa)
           (type (f2cl-lib:integer4) ip ido))
  (f2cl-lib:with-multi-array-data
      ((wa double-float wa-%data% wa-%offset%))
    (prog ((arg4 0.0d0) (arg3 0.0d0) (i 0) (arg2 0.0d0) (j 0) (arg1 0.0d0)
           (argz 0.0d0) (tpi 0.0d0))
      (declare (type (f2cl-lib:integer4) j i)
               (type (double-float) tpi argz arg1 arg2 arg3 arg4))
      (setf tpi (* 8.0d0 (atan 1.0d0)))
      (setf argz (/ tpi (f2cl-lib:freal ip)))
      (setf arg1 (/ tpi (f2cl-lib:freal (f2cl-lib:int-mul ido ip))))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ip) nil)
        (tagbody
          (setf arg2 (* (f2cl-lib:freal (f2cl-lib:int-sub j 1)) arg1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ido) nil)
            (tagbody
              (setf arg3 (* (f2cl-lib:freal (f2cl-lib:int-sub i 1)) arg2))
              (setf (f2cl-lib:fref wa-%data%
                                   (i (f2cl-lib:int-sub j 1) 1)
                                   ((1 ido)
                                    (1
                                     (f2cl-lib:int-add ip
                                                       (f2cl-lib:int-sub 1)))
                                    (1 2))
                                   wa-%offset%)
                      (cos arg3))
              (setf (f2cl-lib:fref wa-%data%
                                   (i (f2cl-lib:int-sub j 1) 2)
                                   ((1 ido)
                                    (1
                                     (f2cl-lib:int-add ip
                                                       (f2cl-lib:int-sub 1)))
                                    (1 2))
                                   wa-%offset%)
                      (sin arg3))
             label100))
          (if (<= ip 5) (go label110))
          (setf arg4 (* (f2cl-lib:freal (f2cl-lib:int-sub j 1)) argz))
          (setf (f2cl-lib:fref wa-%data%
                               (1 (f2cl-lib:int-sub j 1) 1)
                               ((1 ido)
                                (1 (f2cl-lib:int-add ip (f2cl-lib:int-sub 1)))
                                (1 2))
                               wa-%offset%)
                  (cos arg4))
          (setf (f2cl-lib:fref wa-%data%
                               (1 (f2cl-lib:int-sub j 1) 2)
                               ((1 ido)
                                (1 (f2cl-lib:int-add ip (f2cl-lib:int-sub 1)))
                                (1 2))
                               wa-%offset%)
                  (sin arg4))
         label110))
      (go end_label)
     end_label
      (return (values nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tables
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil)
           :calls 'nil)))

