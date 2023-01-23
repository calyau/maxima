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
;;;           (:float-format double-float))

(in-package "HOMPACK")


(defun otputp (n numpat cl facv clx x xnp1)
  (declare (type (array double-float (*)) xnp1)
           (type (array double-float (*)) x clx facv cl)
           (type (f2cl-lib:integer4) numpat n))
  (f2cl-lib:with-multi-array-data
      ((cl double-float cl-%data% cl-%offset%)
       (facv double-float facv-%data% facv-%offset%)
       (clx double-float clx-%data% clx-%offset%)
       (x double-float x-%data% x-%offset%)
       (xnp1 double-float xnp1-%data% xnp1-%offset%))
    (prog ((temp (make-array 2 :element-type 'double-float)) (fac 0.0) (i 0)
           (ierr 0) (j 0) (np1 0))
      (declare (type (f2cl-lib:integer4) np1 j ierr i)
               (type (array double-float (2)) temp)
               (type (double-float) fac))
      (setf np1 (f2cl-lib:int-add n 1))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (mulp
           (f2cl-lib:array-slice cl-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 (f2cl-lib:int-add n 1)))
                                 cl-%offset%)
           (f2cl-lib:array-slice x-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 x-%offset%)
           (f2cl-lib:array-slice clx-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 clx-%offset%))
         label1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 2) nil)
        (tagbody
          (setf (f2cl-lib:fref xnp1-%data% (i) ((1 2)) xnp1-%offset%)
                  (f2cl-lib:fref cl-%data%
                                 (i np1)
                                 ((1 2) (1 (f2cl-lib:int-add n 1)))
                                 cl-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf (f2cl-lib:fref xnp1-%data% (i) ((1 2)) xnp1-%offset%)
                      (+ (f2cl-lib:fref xnp1-%data% (i) ((1 2)) xnp1-%offset%)
                         (f2cl-lib:fref clx-%data%
                                        (i j)
                                        ((1 2) (1 n))
                                        clx-%offset%)))
             label2))))
     label2
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (divp
               (f2cl-lib:array-slice x-%data%
                                     double-float
                                     (1 j)
                                     ((1 2) (1 n))
                                     x-%offset%)
               xnp1 temp ierr)
            (declare (ignore var-0 var-1 var-2))
            (setf ierr var-3))
          (setf (f2cl-lib:fref x-%data% (1 j) ((1 2) (1 n)) x-%offset%)
                  (f2cl-lib:fref temp (1) ((1 2))))
          (setf (f2cl-lib:fref x-%data% (2 j) ((1 2) (1 n)) x-%offset%)
                  (f2cl-lib:fref temp (2) ((1 2))))
         label10))
      (setf (f2cl-lib:fref temp (1) ((1 2))) (f2cl-lib:d1mach 2))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf fac
                  (expt 10.0f0
                        (f2cl-lib:fref facv-%data% (j) ((1 n)) facv-%offset%)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 2) nil)
            (tagbody
              (if
               (<
                (*
                 (/
                  (abs (f2cl-lib:fref x-%data% (i j) ((1 2) (1 n)) x-%offset%))
                  (f2cl-lib:fref temp (1) ((1 2))))
                 fac)
                1.0f0)
               (setf (f2cl-lib:fref x-%data% (i j) ((1 2) (1 n)) x-%offset%)
                       (* fac
                          (f2cl-lib:fref x-%data%
                                         (i j)
                                         ((1 2) (1 n))
                                         x-%offset%))))
             label30))))
     label30
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::otputp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::d1mach fortran-to-lisp::divp
                    fortran-to-lisp::mulp))))

