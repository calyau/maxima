;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:18:39 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun xgetua (iunita n)
  (declare (type (integer) n)
           (type (simple-array f2cl-lib:integer4 (*)) iunita))
  (prog ((f2cl-lib:index 0) (i 0))
    (declare (type (integer) i f2cl-lib:index))
    (setf n (j4save 5 0 f2cl-lib:%false%))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
        (setf f2cl-lib:index (f2cl-lib:int-add i 4))
        (if (= i 1) (setf f2cl-lib:index 3))
        (setf (f2cl-lib:fref iunita (i) ((1 5)))
                (j4save f2cl-lib:index 0 f2cl-lib:%false%))
       label30))
    (go end_label)
   end_label
    (return (values nil n))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xgetua
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array fortran-to-lisp::integer4 (5)) (integer))
           :return-values '(nil fortran-to-lisp::n)
           :calls '(fortran-to-lisp::j4save))))

