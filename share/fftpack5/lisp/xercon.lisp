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
;;;           (:float-format single-float))

(in-package "FFTPACK5")


(defun xercon (inc jump n lot)
  (declare (type (f2cl-lib:integer4) lot n jump inc))
  (prog ((i 0) (j 0) (jnew 0) (lcm$ 0) (xercon nil))
    (declare (type f2cl-lib:logical xercon)
             (type (f2cl-lib:integer4) lcm$ jnew j i))
    (setf i inc)
    (setf j jump)
   label10
    (cond
      ((/= j 0)
       (setf jnew (mod i j))
       (setf i j)
       (setf j jnew)
       (go label10)))
    (setf lcm$ (the f2cl-lib:integer4 (truncate (* inc jump) i)))
    (cond
      ((and
        (<= lcm$
            (f2cl-lib:int-mul (f2cl-lib:int-add n (f2cl-lib:int-sub 1)) inc))
        (<= lcm$
            (f2cl-lib:int-mul (f2cl-lib:int-add lot (f2cl-lib:int-sub 1))
                              jump)))
       (setf xercon f2cl-lib:%false%))
      (t
       (setf xercon f2cl-lib:%true%)))
    (go end_label)
   end_label
    (return (values xercon nil nil nil nil))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xercon
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls 'nil)))

