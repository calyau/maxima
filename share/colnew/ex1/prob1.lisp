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
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun *main* ()
  (prog ((ispace (make-array 200 :element-type 'f2cl-lib:integer4))
         (m (make-array 1 :element-type 'f2cl-lib:integer4))
         (ipar (make-array 11 :element-type 'f2cl-lib:integer4))
         (ltol (make-array 2 :element-type 'f2cl-lib:integer4))
         (fspace (make-array 2000 :element-type 'double-float))
         (zeta (make-array 4 :element-type 'double-float))
         (tol (make-array 2 :element-type 'double-float))
         (z (make-array 4 :element-type 'double-float))
         (u (make-array 4 :element-type 'double-float))
         (err (make-array 4 :element-type 'double-float)) (j 0) (x 0.0)
         (iflag 0) (dummy 0.0) (i 0))
    (declare (type double-float dummy x)
             (type (f2cl-lib:integer4) i iflag j)
             (type (array double-float (2)) tol)
             (type (array double-float (4)) err u z zeta)
             (type (array double-float (2000)) fspace)
             (type (array f2cl-lib:integer4 (2)) ltol)
             (type (array f2cl-lib:integer4 (11)) ipar)
             (type (array f2cl-lib:integer4 (1)) m)
             (type (array f2cl-lib:integer4 (200)) ispace))
    (f2cl-lib:fformat 6
                      ("1" " EXAMPLE OF A SIMPLE PROBLEM SETUP." "~%"
                       "  UNIFORML" "Y LOADED BEAM OF VARIABLE STIFFNESS," "~%"
                       "  SIMPLY SUPPORTED AT" " BOTH ENDS." "~%" "~%"))
    (setf (f2cl-lib:fref m (1) ((1 1))) 4)
    (setf (f2cl-lib:fref zeta (1) ((1 4))) (coerce 1.0f0 'double-float))
    (setf (f2cl-lib:fref zeta (2) ((1 4))) (coerce 1.0f0 'double-float))
    (setf (f2cl-lib:fref zeta (3) ((1 4))) (coerce 2.0f0 'double-float))
    (setf (f2cl-lib:fref zeta (4) ((1 4))) (coerce 2.0f0 'double-float))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 11) nil)
      (tagbody (setf (f2cl-lib:fref ipar (i) ((1 11))) 0) label10))
    (setf (f2cl-lib:fref ipar (3) ((1 11))) 1)
    (setf (f2cl-lib:fref ipar (4) ((1 11))) 2)
    (setf (f2cl-lib:fref ipar (5) ((1 11))) 2000)
    (setf (f2cl-lib:fref ipar (6) ((1 11))) 200)
    (setf (f2cl-lib:fref ltol (1) ((1 2))) 1)
    (setf (f2cl-lib:fref ltol (2) ((1 2))) 3)
    (setf (f2cl-lib:fref tol (1) ((1 2))) (coerce 1.0f-7 'double-float))
    (setf (f2cl-lib:fref tol (2) ((1 2))) (coerce 1.0f-7 'double-float))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16)
        (colsys 1 m 1.0 2.0 zeta ipar ltol tol
         (make-array 1 :element-type (type-of dummy) :initial-element dummy)
         ispace fspace iflag #'fsub #'dfsub #'gsub #'dgsub dummy)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-12 var-13 var-14 var-15 var-16))
      (setf iflag var-11))
    (if (/= iflag 1) (f2cl-lib::stop))
    (setf x (coerce 1.0f0 'double-float))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 4) nil)
      (tagbody
        (setf (f2cl-lib:fref err (i) ((1 4))) (coerce 0.0f0 'double-float))
       label20))
    (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                  ((> j 101) nil)
      (tagbody
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (appsln x z fspace ispace)
          (declare (ignore var-1 var-2 var-3))
          (setf x var-0))
        (exact x u)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 4) nil)
          (tagbody
            (setf (f2cl-lib:fref err (i) ((1 4)))
                    (f2cl-lib:amax1 (f2cl-lib:fref err (i) ((1 4)))
                                    (abs
                                     (- (f2cl-lib:fref u (i) ((1 4)))
                                        (f2cl-lib:fref z (i) ((1 4)))))))
           label30))
        (setf x (+ x 0.01f0))
       label40))
    (f2cl-lib:fformat 6
                      ("~%" " ERROR TOLERANCES SATISFIED" "~%" "~%"
                       " THE EXACT ERRORS ARE," "~%" "~7@T" 4
                       (("~12,4,2,0,'*,,'EE")) "~%")
                      (do ((i 1 (f2cl-lib:int-add i 1))
                           (%ret nil))
                          ((> i 4) (nreverse %ret))
                        (declare (type f2cl-lib:integer4 i))
                        (push (f2cl-lib:fref err (i) ((1 4))) %ret)))
    (f2cl-lib::stop)
   end_label
    (return nil)))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::*main*
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

