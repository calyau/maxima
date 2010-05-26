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
  (en 0.0 :type double-float)
  (s 0.0 :type double-float)
  (el 0.0 :type double-float)
  (cons$ 0.0 :type double-float))


(defparameter *%blank%-common-block*
  (let* ()
    (declare (ignorable))
    (make-%blank%)))


(let ((sval
       (make-array 3
                   :element-type 'double-float
                   :initial-contents '(0.2 0.1 0.05)))
      (elval
       (make-array 3
                   :element-type 'double-float
                   :initial-contents '(60.0 120.0 200.0))))
  (declare (type (array double-float (3)) sval elval))
  (defun *main* ()
    (let ()
      (symbol-macrolet ((cons$ (%blank%-cons$ *%blank%-common-block*))
                        (el (%blank%-el *%blank%-common-block*))
                        (s (%blank%-s *%blank%-common-block*))
                        (en (%blank%-en *%blank%-common-block*)))
        (prog ((xl 0.0) (dm 0.0) (iii 0) (np1 0) (x 0.0) (is4 0) (is5 0)
               (is6 0) (iflag 0) (fixpnt 0.0) (ijk 0) (aright 0.0) (aleft 0.0)
               (ncomp 0) (tol (make-array 2 :element-type 'double-float))
               (fspace (make-array 40000 :element-type 'double-float))
               (zeta (make-array 5 :element-type 'double-float))
               (ipar (make-array 11 :element-type 'f2cl-lib:integer4))
               (ltol (make-array 2 :element-type 'f2cl-lib:integer4))
               (ispace (make-array 2500 :element-type 'f2cl-lib:integer4))
               (m (make-array 2 :element-type 'f2cl-lib:integer4))
               (z (make-array 5 :element-type 'double-float))
               (a (make-array 28 :element-type 'double-float)))
          (declare (type (array double-float (28)) a)
                   (type (array f2cl-lib:integer4 (2500)) ispace)
                   (type (array f2cl-lib:integer4 (2)) m ltol)
                   (type (array f2cl-lib:integer4 (11)) ipar)
                   (type (array double-float (5)) z zeta)
                   (type (array double-float (40000)) fspace)
                   (type (array double-float (2)) tol)
                   (type (f2cl-lib:integer4) ncomp ijk iflag is6 is5 is4 np1
                                             iii)
                   (type double-float aleft aright fixpnt x dm xl))
          (setf en (coerce 0.2f0 'double-float))
          (setf cons$ (* 0.5f0 (- 3.0f0 en)))
          (setf ncomp 2)
          (setf (f2cl-lib:fref m (1) ((1 2))) 2)
          (setf (f2cl-lib:fref m (2) ((1 2))) 3)
          (setf aleft (coerce 0.0f0 'double-float))
          (setf aright (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref zeta (1) ((1 5))) (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref zeta (2) ((1 5))) (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref zeta (3) ((1 5))) (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref zeta (4) ((1 5))) (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref zeta (5) ((1 5))) (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref ipar (1) ((1 11))) 1)
          (setf (f2cl-lib:fref ipar (2) ((1 11))) 4)
          (setf (f2cl-lib:fref ipar (3) ((1 11))) 10)
          (setf (f2cl-lib:fref ipar (4) ((1 11))) 2)
          (setf (f2cl-lib:fref ipar (5) ((1 11))) 40000)
          (setf (f2cl-lib:fref ipar (6) ((1 11))) 2500)
          (setf (f2cl-lib:fref ipar (7) ((1 11))) 0)
          (setf (f2cl-lib:fref ipar (8) ((1 11))) 0)
          (setf (f2cl-lib:fref ipar (9) ((1 11))) 1)
          (setf (f2cl-lib:fref ipar (10) ((1 11))) 0)
          (setf (f2cl-lib:fref ipar (11) ((1 11))) 0)
          (setf (f2cl-lib:fref ltol (1) ((1 2))) 1)
          (setf (f2cl-lib:fref ltol (2) ((1 2))) 3)
          (setf (f2cl-lib:fref tol (1) ((1 2))) (coerce 1.0f-5 'double-float))
          (setf (f2cl-lib:fref tol (2) ((1 2))) (coerce 1.0f-5 'double-float))
          (f2cl-lib:fdo (ijk 1 (f2cl-lib:int-add ijk 1))
                        ((> ijk 3) nil)
            (tagbody
              (setf s (f2cl-lib:fref sval (ijk) ((1 3))))
              (setf el (f2cl-lib:fref elval (ijk) ((1 3))))
              (if (= ijk 1) (go label10))
              (setf (f2cl-lib:fref ipar (9) ((1 11))) 3)
              (setf (f2cl-lib:fref ipar (3) ((1 11)))
                      (f2cl-lib:fref ispace (1) ((1 2500))))
             label10
              (f2cl-lib:fformat 6
                                ("1" " ROTATING FLOW OVER A STATIONARY DISK."
                                 "~%" "  PARAME" "TERS -  N =" 1
                                 (("~5,2,0,'*,F")) "   S =" 1 (("~5,2,0,'*,F"))
                                 "   L =" 1 (("~6,1,0,'*,F")) "~%" "~%")
                                en
                                s
                                el)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (colsys ncomp m aleft aright zeta ipar ltol tol
                   (make-array 1
                               :element-type (type-of fixpnt)
                               :initial-element fixpnt)
                   ispace fspace iflag #'fsub #'dfsub #'gsub #'dgsub #'solutn)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-12 var-13 var-14
                                 var-15 var-16))
                (setf iflag var-11))
              (if (/= iflag 1) (f2cl-lib::stop))
              (setf is6 (f2cl-lib:fref ispace (6) ((1 2500))))
              (setf is5
                      (f2cl-lib:int-add (f2cl-lib:fref ispace (1) ((1 2500)))
                                        2))
              (setf is4
                      (f2cl-lib:int-add is5
                                        (f2cl-lib:int-mul
                                         (f2cl-lib:fref ispace (4) ((1 2500)))
                                         (f2cl-lib:int-add
                                          (f2cl-lib:fref ispace (1) ((1 2500)))
                                          1))))
              (setf x (coerce 0.0f0 'double-float))
              (f2cl-lib:fformat 6
                                ("1"
                                 "       X              G              DG     "
                                 "       H             DH            D2H" "~%"
                                 "~%"))
              (setf np1 (f2cl-lib:int (+ el 1.5f0)))
              (f2cl-lib:fdo (iii 1 (f2cl-lib:int-add iii 1))
                            ((> iii np1) nil)
                (tagbody
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13 var-14 var-15
                         var-16)
                      (approx iii x z a
                       (f2cl-lib:array-slice fspace
                                             double-float
                                             (is6)
                                             ((1 40000)))
                       (f2cl-lib:array-slice fspace
                                             double-float
                                             (1)
                                             ((1 40000)))
                       (f2cl-lib:fref ispace (1) ((1 2500)))
                       (f2cl-lib:array-slice fspace
                                             double-float
                                             (is5)
                                             ((1 40000)))
                       (f2cl-lib:array-slice fspace
                                             double-float
                                             (is4)
                                             ((1 40000)))
                       (f2cl-lib:fref ispace (2) ((1 2500)))
                       (f2cl-lib:fref ispace (3) ((1 2500)))
                       (f2cl-lib:fref ispace (5) ((1 2500)))
                       (f2cl-lib:array-slice ispace
                                             f2cl-lib:integer4
                                             (8)
                                             ((1 2500)))
                       (f2cl-lib:fref ispace (4) ((1 2500))) 1
                       (make-array 1
                                   :element-type (type-of dm)
                                   :initial-element dm)
                       0)
                    (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                     var-9 var-10 var-11 var-12 var-13 var-14
                                     var-15 var-16))
                    (setf iii var-0)
                    (setf x var-1))
                  (setf xl (* x el))
                  (setf (f2cl-lib:fref z (2) ((1 5)))
                          (/ (f2cl-lib:fref z (2) ((1 5))) el))
                  (setf (f2cl-lib:fref z (4) ((1 5)))
                          (/ (f2cl-lib:fref z (4) ((1 5))) el))
                  (setf (f2cl-lib:fref z (5) ((1 5)))
                          (/ (/ (f2cl-lib:fref z (5) ((1 5))) el) el))
                  (f2cl-lib:fformat 6 (6 (("~15,5,2,0,'*,,'EE")) "~%") xl z)
                  (setf x (+ x (/ 1.0f0 el)))
                 label20))
             label30))
          (f2cl-lib::stop)
         end_label
          (return nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::*main*
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

