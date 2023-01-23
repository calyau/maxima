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


(defun xerfft (srname info)
  (declare (type (f2cl-lib:integer4) info) (type (string *) srname))
  (f2cl-lib:with-multi-array-data
      ((srname character srname-%data% srname-%offset%))
    (prog ()
      (declare)
      (cond
        ((>= info 1)
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") 1 (("~3D")) ("~A")) "~%")
                           " ** On entry to "
                           srname
                           " parameter number "
                           info
                           " had an illegal value"))
        ((= info (f2cl-lib:int-sub 1))
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") ("~A")) "~%")
                           " ** On entry to "
                           srname
                           " parameters LOT, JUMP, N and INC are inconsistent"))
        ((= info (f2cl-lib:int-sub 2))
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") ("~A")) "~%")
                           " ** On entry to "
                           srname
                           " parameter L is greater than LDIM"))
        ((= info (f2cl-lib:int-sub 3))
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") ("~A")) "~%")
                           " ** On entry to "
                           srname
                           " parameter M is greater than MDIM"))
        ((= info (f2cl-lib:int-sub 5))
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") ("~A")) "~%")
                           " ** Within "
                           srname
                           " input error returned by lower level routine"))
        ((= info (f2cl-lib:int-sub 6))
         (f2cl-lib:fformat t
                           (t (("~A") ("~A") ("~A") ("~A")) "~%")
                           " ** On entry to "
                           srname
                           " parameter LDIM is less than 2*(L/2+1)")))
      (f2cl-lib::stop)
     end_label
      (return (values nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerfft
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((string) (fortran-to-lisp::integer4))
           :return-values '(nil nil)
           :calls 'nil)))

