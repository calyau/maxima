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


(defun dslsbt (wm iwm x tem)
  (declare (type (array f2cl-lib:integer4 (*)) iwm)
           (type (array double-float (*)) tem x wm))
  (f2cl-lib:with-multi-array-data
      ((wm double-float wm-%data% wm-%offset%)
       (x double-float x-%data% x-%offset%)
       (tem double-float tem-%data% tem-%offset%)
       (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%))
    (prog ((lblox 0) (lpb 0) (lpc 0) (mb 0) (nb 0))
      (declare (type (f2cl-lib:integer4) nb mb lpc lpb lblox))
      (setf mb (f2cl-lib:fref iwm-%data% (1) ((1 *)) iwm-%offset%))
      (setf nb (f2cl-lib:fref iwm-%data% (2) ((1 *)) iwm-%offset%))
      (setf lblox (f2cl-lib:int-mul mb mb nb))
      (setf lpb (f2cl-lib:int-add 3 lblox))
      (setf lpc (f2cl-lib:int-add lpb lblox))
      (dsolbt mb nb
       (f2cl-lib:array-slice wm-%data% double-float (3) ((1 *)) wm-%offset%)
       (f2cl-lib:array-slice wm-%data% double-float (lpb) ((1 *)) wm-%offset%)
       (f2cl-lib:array-slice wm-%data% double-float (lpc) ((1 *)) wm-%offset%)
       x
       (f2cl-lib:array-slice iwm-%data%
                             f2cl-lib:integer4
                             (21)
                             ((1 *))
                             iwm-%offset%))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dslsbt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::dsolbt))))

