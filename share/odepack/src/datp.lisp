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


(defun datp (neq y savf p wght hl0 wk f w)
  (declare (type (double-float) hl0)
           (type (array double-float (*)) w wk wght p savf y)
           (type (array f2cl-lib:integer4 (*)) neq))
  (let ()
    (symbol-macrolet ((tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (y double-float y-%data% y-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (p double-float p-%data% p-%offset%)
           (wght double-float wght-%data% wght-%offset%)
           (wk double-float wk-%data% wk-%offset%)
           (w double-float w-%data% w-%offset%))
        (prog ((i 0) (rpnrm 0.0d0) (pnrm 0.0d0) (fac 0.0d0))
          (declare (type (double-float) fac pnrm rpnrm)
                   (type (f2cl-lib:integer4) i))
          (setf pnrm (dvnorm n p wght))
          (setf rpnrm (/ 1.0d0 pnrm))
          (dcopy n y 1 w 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label20
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref w-%data% (i) ((1 *)) w-%offset%)
                         (* (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%)
                            rpnrm)))))
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f neq tn y wk)
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf tn var-1)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (dcopy n w 1 y 1)
          (setf fac (* hl0 pnrm))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label40
              (setf (f2cl-lib:fref w-%data% (i) ((1 *)) w-%offset%)
                      (- (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%)
                         (* fac
                            (-
                             (f2cl-lib:fref wk-%data% (i) ((1 *)) wk-%offset%)
                             (f2cl-lib:fref savf-%data%
                                            (i)
                                            ((1 *))
                                            savf-%offset%)))))))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::datp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (array double-float (*)) t
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::dvnorm))))

