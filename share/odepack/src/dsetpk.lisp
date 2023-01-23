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


(defun dsetpk (neq y ysv ewt ftem savf jok wm iwm f jac)
  (declare (type (f2cl-lib:integer4) jok)
           (type (array double-float (*)) wm savf ftem ewt ysv y)
           (type (array f2cl-lib:integer4 (*)) iwm neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                      (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                      (nje (aref (dls001-part-1 *dls001-common-block*) 35))
                      (locwp (aref (dlpk01-part-1 *dlpk01-common-block*) 2))
                      (lociwp (aref (dlpk01-part-1 *dlpk01-common-block*) 3)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%)
           (y double-float y-%data% y-%offset%)
           (ysv double-float ysv-%data% ysv-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (ftem double-float ftem-%data% ftem-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (wm double-float wm-%data% wm-%offset%))
        (prog ((ier 0) (hl0 0.0d0))
          (declare (type (double-float) hl0) (type (f2cl-lib:integer4) ier))
          (setf ierpj 0)
          (setf jcur 0)
          (if (= jok -1) (setf jcur 1))
          (setf hl0 (* el0 h))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12)
              (funcall jac
                       f
                       neq
                       tn
                       y
                       ysv
                       ewt
                       savf
                       ftem
                       hl0
                       jok
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (locwp)
                                             ((1 *))
                                             wm-%offset%)
                       (f2cl-lib:array-slice iwm-%data%
                                             f2cl-lib:integer4
                                             (lociwp)
                                             ((1 *))
                                             iwm-%offset%)
                       ier)
            (declare (ignore var-0 var-1 var-3 var-4 var-5 var-6 var-7 var-10
                             var-11))
            (when var-2
              (setf tn var-2))
            (when var-8
              (setf hl0 var-8))
            (when var-9
              (setf jok var-9))
            (when var-12
              (setf ier var-12)))
          (setf nje (f2cl-lib:int-add nje 1))
          (if (= ier 0) (go end_label))
          (setf ierpj 1)
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil jok nil nil nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsetpk
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t)
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::jok nil
                            nil nil nil)
           :calls 'nil)))

