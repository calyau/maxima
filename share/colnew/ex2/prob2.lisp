;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format double-float))

(in-package :colnew)


(defstruct (%blank%
             (:predicate is-%blank%-p))
  (eps 0.0 :type double-float)
  (dmu 0.0 :type double-float)
  (eps4mu 0.0 :type double-float)
  (gamma 0.0 :type double-float)
  (xt 0.0 :type double-float))


(defparameter *%blank%-common-block*
  (let* ()
    (declare (ignorable))
    (make-%blank%)))


(defun *main* ()
  (let ()
    (symbol-macrolet ((xt (%blank%-xt *%blank%-common-block*))
                      (gamma (%blank%-gamma *%blank%-common-block*))
                      (eps4mu (%blank%-eps4mu *%blank%-common-block*))
                      (dmu (%blank%-dmu *%blank%-common-block*))
                      (eps (%blank%-eps *%blank%-common-block*)))
      (prog ((iii 0) (np1 0) (x 0.0) (iflag 0) (fixpnt 0.0) (i 0) (aright 0.0)
             (aleft 0.0) (ncomp 0)
             (z (make-array 4 :element-type 'double-float))
             (tol (make-array 4 :element-type 'double-float))
             (fspace (make-array 40000 :element-type 'double-float))
             (zeta (make-array 4 :element-type 'double-float))
             (ltol (make-array 4 :element-type 'f2cl-lib:integer4))
             (ispace (make-array 2500 :element-type 'f2cl-lib:integer4))
             (ipar (make-array 11 :element-type 'f2cl-lib:integer4))
             (m (make-array 2 :element-type 'f2cl-lib:integer4)))
        (declare (type (array f2cl-lib:integer4 (2)) m)
                 (type (array f2cl-lib:integer4 (11)) ipar)
                 (type (array f2cl-lib:integer4 (2500)) ispace)
                 (type (array f2cl-lib:integer4 (4)) ltol)
                 (type (array double-float (40000)) fspace)
                 (type (array double-float (4)) zeta tol z)
                 (type double-float aleft aright fixpnt x)
                 (type (f2cl-lib:integer4) ncomp i iflag np1 iii))
        (setf gamma 1.1)
        (setf eps 0.01)
        (setf dmu eps)
        (setf eps4mu (/ (expt eps 4) dmu))
        (setf xt (f2cl-lib:fsqrt (/ (* 2.0f0 (- gamma 1.0f0)) gamma)))
        (f2cl-lib:fformat 6
                          ("1" "DIMPLING OF SPHERICAL CAPS." "~%" " GAMMA =" 1
                           (("~7,2,0,'*,F")) "~%" "  XT =" 1
                           (("~12,5,2,0,'*,,'EE")) "~%" " EPS =" 1
                           (("~12,5,2,0,'*,,'EE")) "~%" "  MU =" 1
                           (("~12,5,2,0,'*,,'EE")) "~%" " EPS**4/M" "U =" 1
                           (("~12,5,2,0,'*,,'EE")) "~%")
                          gamma
                          xt
                          eps
                          dmu
                          eps4mu)
        (setf ncomp 2)
        (setf (f2cl-lib:fref m (1) ((1 2))) 2)
        (setf (f2cl-lib:fref m (2) ((1 2))) 2)
        (setf aleft (coerce 0.0f0 'double-float))
        (setf aright (coerce 1.0f0 'double-float))
        (setf (f2cl-lib:fref zeta (1) ((1 4))) (coerce 0.0f0 'double-float))
        (setf (f2cl-lib:fref zeta (2) ((1 4))) (coerce 0.0f0 'double-float))
        (setf (f2cl-lib:fref zeta (3) ((1 4))) (coerce 1.0f0 'double-float))
        (setf (f2cl-lib:fref zeta (4) ((1 4))) (coerce 1.0f0 'double-float))
        (setf (f2cl-lib:fref ipar (1) ((1 11))) 1)
        (setf (f2cl-lib:fref ipar (2) ((1 11))) 4)
        (setf (f2cl-lib:fref ipar (3) ((1 11))) 10)
        (setf (f2cl-lib:fref ipar (8) ((1 11))) 0)
        (setf (f2cl-lib:fref ipar (5) ((1 11))) 40000)
        (setf (f2cl-lib:fref ipar (6) ((1 11))) 2500)
        (setf (f2cl-lib:fref ipar (7) ((1 11))) -1)
        (setf (f2cl-lib:fref ipar (9) ((1 11))) 1)
        (setf (f2cl-lib:fref ipar (10) ((1 11))) 0)
        (setf (f2cl-lib:fref ipar (11) ((1 11))) 0)
        (setf (f2cl-lib:fref ipar (4) ((1 11))) 4)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 4) nil)
          (tagbody
            (setf (f2cl-lib:fref ltol (i) ((1 4))) i)
            (setf (f2cl-lib:fref tol (i) ((1 4))) 1.0e-5)
           label10))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16)
            (colsys ncomp m aleft aright zeta ipar ltol tol
             (make-array 1
                         :element-type (type-of fixpnt)
                         :initial-element fixpnt)
             ispace fspace iflag #'fsub #'dfsub #'gsub #'dgsub #'solutn)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-12 var-13 var-14 var-15
                           var-16))
          (setf iflag var-11))
        (setf x (coerce 0.0f0 'double-float))
        (f2cl-lib:fformat 6
                          ("1" "       X             PHI           DPHI     "
                           "      PSI          DPSI" "~%" "~%"))
        (setf np1 21)
        (f2cl-lib:fdo (iii 1 (f2cl-lib:int-add iii 1))
                      ((> iii np1) nil)
          (tagbody
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (appsln x z fspace ispace)
              (declare (ignore var-1 var-2 var-3))
              (setf x var-0))
            (f2cl-lib:fformat 6
                              ("~6@T" 1 (("~5,2,0,'*,F")) "~4@T" 6
                               (("~15,5,2,0,'*,,'EE")) "~%")
                              x
                              z)
            (setf x (+ x 0.05))
           label20))
        (f2cl-lib::stop)
       end_label
        (return nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::*main*
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

