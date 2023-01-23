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


(defun divp (xxxx yyyy zzzz ierr)
  (declare (type (f2cl-lib:integer4) ierr)
           (type (array double-float (*)) zzzz yyyy xxxx))
  (f2cl-lib:with-multi-array-data
      ((xxxx double-float xxxx-%data% xxxx-%offset%)
       (yyyy double-float yyyy-%data% yyyy-%offset%)
       (zzzz double-float zzzz-%data% zzzz-%offset%))
    (prog ((denom 0.0) (xnum 0.0))
      (declare (type (double-float) xnum denom))
      (setf ierr 0)
      (setf denom
              (+
               (* (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%)
                  (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%))
               (* (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%)
                  (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%))))
      (setf xnum
              (+
               (* (f2cl-lib:fref xxxx-%data% (1) ((1 2)) xxxx-%offset%)
                  (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%))
               (* (f2cl-lib:fref xxxx-%data% (2) ((1 2)) xxxx-%offset%)
                  (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%))))
      (cond
        ((or (>= (abs denom) 1.0f0)
             (and (< (abs denom) 1.0f0)
                  (< (f2cl-lib:f2cl/ (abs xnum) (f2cl-lib:d1mach 2))
                     (abs denom))))
         (setf (f2cl-lib:fref zzzz-%data% (1) ((1 2)) zzzz-%offset%)
                 (/ xnum denom)))
        (t
         (setf (f2cl-lib:fref zzzz-%data% (1) ((1 2)) zzzz-%offset%)
                 (f2cl-lib:d1mach 2))
         (setf ierr 1)))
      (setf xnum
              (-
               (* (f2cl-lib:fref xxxx-%data% (2) ((1 2)) xxxx-%offset%)
                  (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%))
               (* (f2cl-lib:fref xxxx-%data% (1) ((1 2)) xxxx-%offset%)
                  (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%))))
      (cond
        ((or (>= (abs denom) 1.0f0)
             (and (< (abs denom) 1.0f0)
                  (< (f2cl-lib:f2cl/ (abs xnum) (f2cl-lib:d1mach 2))
                     (abs denom))))
         (setf (f2cl-lib:fref zzzz-%data% (2) ((1 2)) zzzz-%offset%)
                 (/ xnum denom)))
        (t
         (setf (f2cl-lib:fref zzzz-%data% (2) ((1 2)) zzzz-%offset%)
                 (f2cl-lib:d1mach 2))
         (setf ierr 1)))
      (go end_label)
     end_label
      (return (values nil nil nil ierr)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::divp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::d1mach))))

