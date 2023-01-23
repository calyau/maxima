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


(defun cfft2f (ldim l m c wsave lensav work lenwrk ier)
  (declare (type (array double-float (*)) work wsave)
           (type (array f2cl-lib:complex16 (*)) c)
           (type (f2cl-lib:integer4) ier lenwrk lensav m l ldim))
  (f2cl-lib:with-multi-array-data
      ((c f2cl-lib:complex16 c-%data% c-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((ier1 0) (log$ 0) (iw 0))
      (declare (type (f2cl-lib:integer4) iw log$ ier1))
      (setf ier 0)
      (cond
        ((> l ldim)
         (setf ier 5)
         (xerfft "CFFT2F" -2)
         (go label100))
        ((< lensav
            (f2cl-lib:int-add (f2cl-lib:int-mul 2 l)
                              (f2cl-lib:int
                               (f2cl-lib:f2cl/
                                (f2cl-lib:flog (f2cl-lib:freal l))
                                (f2cl-lib:flog 2.0d0)))
                              (f2cl-lib:int-mul 2 m)
                              (f2cl-lib:int
                               (f2cl-lib:f2cl/
                                (f2cl-lib:flog (f2cl-lib:freal m))
                                (f2cl-lib:flog 2.0d0)))
                              8))
         (setf ier 2)
         (xerfft "CFFT2F" 6)
         (go label100))
        ((< lenwrk (f2cl-lib:int-mul 2 l m))
         (setf ier 3)
         (xerfft "CFFT2F" 8)
         (go label100)))
      (setf iw
              (f2cl-lib:int-add (f2cl-lib:int-mul 2 l)
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal l))
                                    (f2cl-lib:flog 2.0d0)))
                                3))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (cfftmf l 1 m ldim c
           (f2cl-lib:int-add (f2cl-lib:int-sub l 1)
                             (f2cl-lib:int-mul ldim (f2cl-lib:int-sub m 1))
                             1)
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 (iw)
                                 ((1 lensav))
                                 wsave-%offset%)
           (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                             (f2cl-lib:int
                              (/ (f2cl-lib:flog (f2cl-lib:freal m))
                                 (f2cl-lib:flog 2.0d0)))
                             4)
           work (f2cl-lib:int-mul 2 l m) ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf ier1 var-10))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "CFFT2F" -5)
         (go label100)))
      (setf iw 1)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (cfftmf m ldim l 1 c
           (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub m 1) ldim) l)
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 (iw)
                                 ((1 lensav))
                                 wsave-%offset%)
           (f2cl-lib:int-add (f2cl-lib:int-mul 2 l)
                             (f2cl-lib:int
                              (/ (f2cl-lib:flog (f2cl-lib:freal l))
                                 (f2cl-lib:flog 2.0d0)))
                             4)
           work (f2cl-lib:int-mul 2 m l) ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf ier1 var-10))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "CFFT2F" -5)))
     label100
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cfft2f
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::cfftmf fortran-to-lisp::xerfft))))

