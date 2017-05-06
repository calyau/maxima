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


(let ((zero 0.0d0))
  (declare (type (double-float) zero))
  (defun droots (ng hmin jflag x0 x1 g0 g1 gx x jroot)
    (declare (type (array f2cl-lib:integer4 (*)) jroot)
             (type (array double-float (*)) gx g1 g0)
             (type (double-float) x x1 x0 hmin)
             (type (f2cl-lib:integer4) jflag ng))
    (let ()
      (symbol-macrolet ((alpha (aref (dlsr01-part-0 *dlsr01-common-block*) 0))
                        (x2 (aref (dlsr01-part-0 *dlsr01-common-block*) 1))
                        (imax (aref (dlsr01-part-1 *dlsr01-common-block*) 3))
                        (last$ (aref (dlsr01-part-1 *dlsr01-common-block*) 4)))
        (f2cl-lib:with-multi-array-data
            ((g0 double-float g0-%data% g0-%offset%)
             (g1 double-float g1-%data% g1-%offset%)
             (gx double-float gx-%data% gx-%offset%)
             (jroot f2cl-lib:integer4 jroot-%data% jroot-%offset%))
          (prog ((nxlast 0) (imxold 0) (i 0) (tmax 0.0d0) (t2 0.0d0)
                 (xroot nil) (sgnchg nil) (zroot nil))
            (declare (type f2cl-lib:logical zroot sgnchg xroot)
                     (type (double-float) t2 tmax)
                     (type (f2cl-lib:integer4) i imxold nxlast))
            (if (= jflag 1) (go label200))
            (setf imax 0)
            (setf tmax zero)
            (setf zroot f2cl-lib:%false%)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i ng) nil)
              (tagbody
                (if
                 (> (abs (f2cl-lib:fref g1-%data% (i) ((1 ng)) g1-%offset%))
                    zero)
                 (go label110))
                (setf zroot f2cl-lib:%true%)
                (go label120)
               label110
                (if
                 (=
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref g0-%data%
                                                (i)
                                                ((1 ng))
                                                g0-%offset%))
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref g1-%data%
                                                (i)
                                                ((1 ng))
                                                g1-%offset%)))
                 (go label120))
                (setf t2
                        (abs
                         (/ (f2cl-lib:fref g1-%data% (i) ((1 ng)) g1-%offset%)
                            (-
                             (f2cl-lib:fref g1-%data% (i) ((1 ng)) g1-%offset%)
                             (f2cl-lib:fref g0-%data%
                                            (i)
                                            ((1 ng))
                                            g0-%offset%)))))
                (if (<= t2 tmax) (go label120))
                (setf tmax t2)
                (setf imax i)
               label120))
            (if (> imax 0) (go label130))
            (setf sgnchg f2cl-lib:%false%)
            (go label140)
           label130
            (setf sgnchg f2cl-lib:%true%)
           label140
            (if (not sgnchg) (go label400))
            (setf xroot f2cl-lib:%false%)
            (setf nxlast 0)
            (setf last$ 1)
           label150
            (if xroot (go label300))
            (if (= nxlast last$) (go label160))
            (setf alpha 1.0d0)
            (go label180)
           label160
            (if (= last$ 0) (go label170))
            (setf alpha (* 0.5d0 alpha))
            (go label180)
           label170
            (setf alpha (* 2.0d0 alpha))
           label180
            (setf x2
                    (+ x1
                       (/
                        (* (- (- x1 x0))
                           (f2cl-lib:fref g1-%data%
                                          (imax)
                                          ((1 ng))
                                          g1-%offset%))
                        (-
                         (f2cl-lib:fref g1-%data% (imax) ((1 ng)) g1-%offset%)
                         (* alpha
                            (f2cl-lib:fref g0-%data%
                                           (imax)
                                           ((1 ng))
                                           g0-%offset%))))))
            (if
             (and (< (abs (- x2 x0)) hmin) (> (abs (- x1 x0)) (* 10.0d0 hmin)))
             (setf x2 (+ x0 (* 0.1d0 (- x1 x0)))))
            (setf jflag 1)
            (setf x x2)
            (go end_label)
           label200
            (setf imxold imax)
            (setf imax 0)
            (setf tmax zero)
            (setf zroot f2cl-lib:%false%)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i ng) nil)
              (tagbody
                (if
                 (> (abs (f2cl-lib:fref gx-%data% (i) ((1 ng)) gx-%offset%))
                    zero)
                 (go label210))
                (setf zroot f2cl-lib:%true%)
                (go label220)
               label210
                (if
                 (=
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref g0-%data%
                                                (i)
                                                ((1 ng))
                                                g0-%offset%))
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref gx-%data%
                                                (i)
                                                ((1 ng))
                                                gx-%offset%)))
                 (go label220))
                (setf t2
                        (abs
                         (/ (f2cl-lib:fref gx-%data% (i) ((1 ng)) gx-%offset%)
                            (-
                             (f2cl-lib:fref gx-%data% (i) ((1 ng)) gx-%offset%)
                             (f2cl-lib:fref g0-%data%
                                            (i)
                                            ((1 ng))
                                            g0-%offset%)))))
                (if (<= t2 tmax) (go label220))
                (setf tmax t2)
                (setf imax i)
               label220))
            (if (> imax 0) (go label230))
            (setf sgnchg f2cl-lib:%false%)
            (setf imax imxold)
            (go label240)
           label230
            (setf sgnchg f2cl-lib:%true%)
           label240
            (setf nxlast last$)
            (if (not sgnchg) (go label250))
            (setf x1 x2)
            (dcopy ng gx 1 g1 1)
            (setf last$ 1)
            (setf xroot f2cl-lib:%false%)
            (go label270)
           label250
            (if (not zroot) (go label260))
            (setf x1 x2)
            (dcopy ng gx 1 g1 1)
            (setf xroot f2cl-lib:%true%)
            (go label270)
           label260
            (dcopy ng gx 1 g0 1)
            (setf x0 x2)
            (setf last$ 0)
            (setf xroot f2cl-lib:%false%)
           label270
            (if (<= (abs (- x1 x0)) hmin) (setf xroot f2cl-lib:%true%))
            (go label150)
           label300
            (setf jflag 2)
            (setf x x1)
            (dcopy ng g1 1 gx 1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i ng) nil)
              (tagbody
                (setf (f2cl-lib:fref jroot-%data% (i) ((1 ng)) jroot-%offset%)
                        0)
                (if
                 (> (abs (f2cl-lib:fref g1-%data% (i) ((1 ng)) g1-%offset%))
                    zero)
                 (go label310))
                (setf (f2cl-lib:fref jroot-%data% (i) ((1 ng)) jroot-%offset%)
                        1)
                (go label320)
               label310
                (if
                 (/=
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref g0-%data%
                                                (i)
                                                ((1 ng))
                                                g0-%offset%))
                  (f2cl-lib:sign 1.0d0
                                 (f2cl-lib:fref g1-%data%
                                                (i)
                                                ((1 ng))
                                                g1-%offset%)))
                 (setf (f2cl-lib:fref jroot-%data% (i) ((1 ng)) jroot-%offset%)
                         1))
               label320))
            (go end_label)
           label400
            (if (not zroot) (go label420))
            (setf x x1)
            (dcopy ng g1 1 gx 1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i ng) nil)
              (tagbody
                (setf (f2cl-lib:fref jroot-%data% (i) ((1 ng)) jroot-%offset%)
                        0)
                (if
                 (<= (abs (f2cl-lib:fref g1-%data% (i) ((1 ng)) g1-%offset%))
                     zero)
                 (setf (f2cl-lib:fref jroot-%data% (i) ((1 ng)) jroot-%offset%)
                         1))
               label410))
            (setf jflag 3)
            (go end_label)
           label420
            (dcopy ng g1 1 gx 1)
            (setf x x1)
            (setf jflag 4)
            (go end_label)
           end_label
            (return (values nil nil jflag x0 x1 nil nil nil x nil))))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::droots
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil fortran-to-lisp::jflag fortran-to-lisp::x0
                            fortran-to-lisp::x1 nil nil nil fortran-to-lisp::x
                            nil)
           :calls '(fortran-to-lisp::dcopy))))

