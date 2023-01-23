;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2017-01 (21B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "ODEPACK")


(defun dewset (n itol rtol atol ycur ewt)
  (declare (type (array double-float (*)) ewt ycur atol rtol)
           (type (f2cl-lib:integer4) itol n))
  (prog ((i 0))
    (declare (type (f2cl-lib:integer4) i))
    (f2cl-lib:computed-goto (label10 label20 label30 label40) itol)
   label10
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
       label15
        (setf (f2cl-lib:fref ewt (i) ((1 n)))
                (+
                 (* (f2cl-lib:fref rtol (1) ((1 *)))
                    (abs (f2cl-lib:fref ycur (i) ((1 n)))))
                 (f2cl-lib:fref atol (1) ((1 *)))))))
    (go end_label)
   label20
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
       label25
        (setf (f2cl-lib:fref ewt (i) ((1 n)))
                (+
                 (* (f2cl-lib:fref rtol (1) ((1 *)))
                    (abs (f2cl-lib:fref ycur (i) ((1 n)))))
                 (f2cl-lib:fref atol (i) ((1 *)))))))
    (go end_label)
   label30
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
       label35
        (setf (f2cl-lib:fref ewt (i) ((1 n)))
                (+
                 (* (f2cl-lib:fref rtol (i) ((1 *)))
                    (abs (f2cl-lib:fref ycur (i) ((1 n)))))
                 (f2cl-lib:fref atol (1) ((1 *)))))))
    (go end_label)
   label40
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
       label45
        (setf (f2cl-lib:fref ewt (i) ((1 n)))
                (+
                 (* (f2cl-lib:fref rtol (i) ((1 *)))
                    (abs (f2cl-lib:fref ycur (i) ((1 n)))))
                 (f2cl-lib:fref atol (i) ((1 *)))))))
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dewset
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

