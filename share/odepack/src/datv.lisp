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


(defun datv (neq y savf v wght ftem f psol z vtem wp iwp hl0 jpre ier npsl)
  (declare (type (f2cl-lib:integer4) npsl ier jpre)
           (type (double-float) hl0)
           (type (array double-float (*)) wp vtem z ftem wght v savf y)
           (type (array f2cl-lib:integer4 (*)) iwp neq))
  (let ()
    (symbol-macrolet ((tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwp f2cl-lib:integer4 iwp-%data% iwp-%offset%)
           (y double-float y-%data% y-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (v double-float v-%data% v-%offset%)
           (wght double-float wght-%data% wght-%offset%)
           (ftem double-float ftem-%data% ftem-%offset%)
           (z double-float z-%data% z-%offset%)
           (vtem double-float vtem-%data% vtem-%offset%)
           (wp double-float wp-%data% wp-%offset%))
        (prog ((i 0) (tempn 0.0d0) (rnorm 0.0d0) (fac 0.0d0))
          (declare (type (double-float) fac rnorm tempn)
                   (type (f2cl-lib:integer4) i))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref vtem-%data% (i) ((1 *)) vtem-%offset%)
                      (/ (f2cl-lib:fref v-%data% (i) ((1 *)) v-%offset%)
                         (f2cl-lib:fref wght-%data%
                                        (i)
                                        ((1 *))
                                        wght-%offset%)))))
          (setf ier 0)
          (if (>= jpre 2) (go label30))
          (dcopy n y 1 z 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label20
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                         (f2cl-lib:fref vtem-%data%
                                        (i)
                                        ((1 *))
                                        vtem-%offset%)))))
          (setf fac hl0)
          (go label60)
         label30
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
              (funcall psol neq tn y savf ftem hl0 wp iwp vtem 2 ier)
            (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf hl0 var-5))
            (when var-10
              (setf ier var-10)))
          (setf npsl (f2cl-lib:int-add npsl 1))
          (if (/= ier 0) (go end_label))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label40
              (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref vtem-%data% (i) ((1 *)) vtem-%offset%)
                         (f2cl-lib:fref wght-%data%
                                        (i)
                                        ((1 *))
                                        wght-%offset%)))))
          (setf tempn (dnrm2 n z 1))
          (setf rnorm (/ 1.0d0 tempn))
          (dcopy n y 1 z 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label50
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                         (*
                          (f2cl-lib:fref vtem-%data% (i) ((1 *)) vtem-%offset%)
                          rnorm)))))
          (setf fac (* hl0 tempn))
         label60
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f neq tn y ftem)
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf tn var-1)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (dcopy n z 1 y 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label70
              (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                      (- (f2cl-lib:fref ftem-%data% (i) ((1 *)) ftem-%offset%)
                         (f2cl-lib:fref savf-%data%
                                        (i)
                                        ((1 *))
                                        savf-%offset%)))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label80
              (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                      (- (f2cl-lib:fref vtem-%data% (i) ((1 *)) vtem-%offset%)
                         (* fac
                            (f2cl-lib:fref z-%data%
                                           (i)
                                           ((1 *))
                                           z-%offset%))))))
          (if (or (= jpre 0) (= jpre 2)) (go label85))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
              (funcall psol neq tn y savf ftem hl0 wp iwp z 1 ier)
            (declare (ignore var-0 var-2 var-3 var-4 var-6 var-7 var-8 var-9))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf hl0 var-5))
            (when var-10
              (setf ier var-10)))
          (setf npsl (f2cl-lib:int-add npsl 1))
          (if (/= ier 0) (go end_label))
         label85
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label90
              (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                         (f2cl-lib:fref wght-%data%
                                        (i)
                                        ((1 *))
                                        wght-%offset%)))))
          (go end_label)
         end_label
          (return
           (values nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   hl0
                   nil
                   ier
                   npsl)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::datv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) t t (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::hl0 nil fortran-to-lisp::ier
                            fortran-to-lisp::npsl)
           :calls '(fortran-to-lisp::dnrm2 fortran-to-lisp::dcopy))))

