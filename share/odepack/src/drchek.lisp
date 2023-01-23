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


(defun drchek (job g neq y yh nyh g0 g1 gx jroot irt)
  (declare (type (array double-float (*)) gx g1 g0 yh y)
           (type (array f2cl-lib:integer4 (*)) jroot neq)
           (type (f2cl-lib:integer4) irt nyh job))
  (let ()
    (symbol-macrolet ((h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (uround (aref (dls001-part-0 *dls001-common-block*) 217))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (t0 (aref (dlsr01-part-0 *dlsr01-common-block*) 2))
                      (tlast (aref (dlsr01-part-0 *dlsr01-common-block*) 3))
                      (toutc (aref (dlsr01-part-0 *dlsr01-common-block*) 4))
                      (irfnd (aref (dlsr01-part-1 *dlsr01-common-block*) 5))
                      (itaskc (aref (dlsr01-part-1 *dlsr01-common-block*) 6))
                      (ngc (aref (dlsr01-part-1 *dlsr01-common-block*) 7))
                      (nge (aref (dlsr01-part-1 *dlsr01-common-block*) 8)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (jroot f2cl-lib:integer4 jroot-%data% jroot-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (g0 double-float g0-%data% g0-%offset%)
           (g1 double-float g1-%data% g1-%offset%)
           (gx double-float gx-%data% gx-%offset%))
        (prog ((jflag 0) (iflag 0) (i 0) (x 0.0d0) (temp2 0.0d0) (temp1 0.0d0)
               (t1 0.0d0) (hming 0.0d0) (zroot nil))
          (declare (type f2cl-lib:logical zroot)
                   (type (double-float) hming t1 temp1 temp2 x)
                   (type (f2cl-lib:integer4) i iflag jflag))
          (setf irt 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ngc) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref jroot-%data% (i) ((1 *)) jroot-%offset%)
                      0)))
          (setf hming (* (+ (abs tn) (abs h)) uround 100.0d0))
          (f2cl-lib:computed-goto (label100 label200 label300) job)
         label100
          (setf t0 tn)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq t0 y ngc g0)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf t0 var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge 1)
          (setf zroot f2cl-lib:%false%)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ngc) nil)
            (tagbody
             label110
              (if
               (<= (abs (f2cl-lib:fref g0-%data% (i) ((1 *)) g0-%offset%))
                   0.0d0)
               (setf zroot f2cl-lib:%true%))))
          (if (not zroot) (go label190))
          (setf temp1 (f2cl-lib:sign hming h))
          (setf t0 (+ t0 temp1))
          (setf temp2 (/ temp1 h))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label120
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                         (* temp2
                            (f2cl-lib:fref yh-%data%
                                           (i 2)
                                           ((1 nyh) (1 *))
                                           yh-%offset%))))))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq t0 y ngc g0)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf t0 var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge (f2cl-lib:int-add nge 1))
          (setf zroot f2cl-lib:%false%)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ngc) nil)
            (tagbody
             label130
              (if
               (<= (abs (f2cl-lib:fref g0-%data% (i) ((1 *)) g0-%offset%))
                   0.0d0)
               (setf zroot f2cl-lib:%true%))))
          (if (not zroot) (go label190))
          (setf irt -1)
          (go end_label)
         label190
          (go end_label)
         label200
          (if (= irfnd 0) (go label260))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy t0 0 yh nyh y iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq t0 y ngc g0)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf t0 var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge (f2cl-lib:int-add nge 1))
          (setf zroot f2cl-lib:%false%)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ngc) nil)
            (tagbody
             label210
              (if
               (<= (abs (f2cl-lib:fref g0-%data% (i) ((1 *)) g0-%offset%))
                   0.0d0)
               (setf zroot f2cl-lib:%true%))))
          (if (not zroot) (go label260))
          (setf temp1 (f2cl-lib:sign hming h))
          (setf t0 (+ t0 temp1))
          (if (< (* (- t0 tn) h) 0.0d0) (go label230))
          (setf temp2 (/ temp1 h))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label220
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                         (* temp2
                            (f2cl-lib:fref yh-%data%
                                           (i 2)
                                           ((1 nyh) (1 *))
                                           yh-%offset%))))))
          (go label240)
         label230
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy t0 0 yh nyh y iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
         label240
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq t0 y ngc g0)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf t0 var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge (f2cl-lib:int-add nge 1))
          (setf zroot f2cl-lib:%false%)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ngc) nil)
            (tagbody
              (if
               (> (abs (f2cl-lib:fref g0-%data% (i) ((1 *)) g0-%offset%))
                  0.0d0)
               (go label250))
              (setf (f2cl-lib:fref jroot-%data% (i) ((1 *)) jroot-%offset%) 1)
              (setf zroot f2cl-lib:%true%)
             label250))
          (if (not zroot) (go label260))
          (setf irt 1)
          (go end_label)
         label260
          (if (= tn tlast) (go label390))
         label300
          (if (or (= itaskc 2) (= itaskc 3) (= itaskc 5)) (go label310))
          (if (>= (* (- toutc tn) h) 0.0d0) (go label310))
          (setf t1 toutc)
          (if (<= (* (- t1 t0) h) 0.0d0) (go label390))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy t1 0 yh nyh y iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (go label330)
         label310
          (setf t1 tn)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label320
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (f2cl-lib:fref yh-%data%
                                     (i 1)
                                     ((1 nyh) (1 *))
                                     yh-%offset%))))
         label330
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq t1 y ngc g1)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf t1 var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge (f2cl-lib:int-add nge 1))
          (setf jflag 0)
         label350
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (droots ngc hming jflag t0 t1 g0 g1 gx x jroot)
            (declare (ignore var-0 var-1 var-5 var-6 var-7 var-9))
            (setf jflag var-2)
            (setf t0 var-3)
            (setf t1 var-4)
            (setf x var-8))
          (if (> jflag 1) (go label360))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy x 0 yh nyh y iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (funcall g neq x y ngc gx)
            (declare (ignore var-0 var-2 var-4))
            (when var-1
              (setf x var-1))
            (when var-3
              (setf ngc var-3)))
          (setf nge (f2cl-lib:int-add nge 1))
          (go label350)
         label360
          (setf t0 x)
          (dcopy ngc gx 1 g0 1)
          (if (= jflag 4) (go label390))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dintdy x 0 yh nyh y iflag)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf iflag var-5))
          (setf irt 1)
          (go end_label)
         label390
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil nil irt)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drchek
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) t
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::irt)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::droots
                    fortran-to-lisp::dintdy))))

