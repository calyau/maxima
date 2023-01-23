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


(defun cfftmf (lot jump n inc c lenc wsave lensav work lenwrk ier)
  (declare (type (array double-float (*)) work wsave)
           (type (array f2cl-lib:complex16 (*)) c)
           (type (f2cl-lib:integer4) ier lenwrk lensav lenc inc n jump lot))
  (f2cl-lib:with-multi-array-data
      ((c f2cl-lib:complex16 c-%data% c-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((iw1 0))
      (declare (type (f2cl-lib:integer4) iw1))
      (setf ier 0)
      (cond
        ((< lenc
            (f2cl-lib:int-add
             (f2cl-lib:int-mul (f2cl-lib:int-add lot (f2cl-lib:int-sub 1))
                               jump)
             (f2cl-lib:int-mul inc (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
             1))
         (setf ier 1)
         (xerfft "CFFTMF " 6))
        ((< lensav
            (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                              (f2cl-lib:int
                               (f2cl-lib:f2cl/
                                (f2cl-lib:flog (f2cl-lib:freal n))
                                (f2cl-lib:flog 2.0d0)))
                              4))
         (setf ier 2)
         (xerfft "CFFTMF " 8))
        ((< lenwrk (f2cl-lib:int-mul 2 lot n))
         (setf ier 3)
         (xerfft "CFFTMF " 10))
        ((not (xercon inc jump n lot))
         (setf ier 4)
         (xerfft "CFFTMF " -1)))
      (if (= n 1) (go end_label))
      (setf iw1 (f2cl-lib:int-add n n 1))
      (cmfm1f lot jump n inc c work wsave
       (f2cl-lib:fref wsave-%data% (iw1) ((1 lensav)) wsave-%offset%)
       (f2cl-lib:array-slice wsave-%data%
                             double-float
                             ((+ iw1 1))
                             ((1 lensav))
                             wsave-%offset%))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cfftmf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::cmfm1f fortran-to-lisp::xercon
                    fortran-to-lisp::xerfft))))

