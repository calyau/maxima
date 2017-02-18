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


(defun odrv (n ia ja a p ip nsp isp path flag)
  (declare (type (array double-float (*)) a)
           (type (array f2cl-lib:integer4 (*)) isp ip p ja ia)
           (type (f2cl-lib:integer4) flag path nsp n))
  (f2cl-lib:with-multi-array-data
      ((ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (p f2cl-lib:integer4 p-%data% p-%offset%)
       (ip f2cl-lib:integer4 ip-%data% ip-%offset%)
       (isp f2cl-lib:integer4 isp-%data% isp-%offset%)
       (a double-float a-%data% a-%offset%))
    (prog ((dflag nil) (v 0) (l 0) (head 0) (tmp 0) (q 0) (next 0)
           (max (the f2cl-lib:integer4 0)))
      (declare (type (f2cl-lib:integer4) max next q tmp head l v)
               (type f2cl-lib:logical dflag))
      (setf flag 0)
      (if (or (< path 1) (< 5 path)) (go label111))
      (if
       (/=
        (f2cl-lib:int-mul (f2cl-lib:int-sub path 1)
                          (f2cl-lib:int-sub path 2)
                          (f2cl-lib:int-sub path 4))
        0)
       (go label1))
      (setf max (the f2cl-lib:integer4 (truncate (- nsp n) 2)))
      (setf v 1)
      (setf l (f2cl-lib:int-add v max))
      (setf head (f2cl-lib:int-add l max))
      (setf next (f2cl-lib:int-add head n))
      (if (< max n) (go label110))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (md n ia ja max
           (f2cl-lib:array-slice isp-%data%
                                 f2cl-lib:integer4
                                 (v)
                                 ((1 *))
                                 isp-%offset%)
           (f2cl-lib:array-slice isp-%data%
                                 f2cl-lib:integer4
                                 (l)
                                 ((1 *))
                                 isp-%offset%)
           (f2cl-lib:array-slice isp-%data%
                                 f2cl-lib:integer4
                                 (head)
                                 ((1 *))
                                 isp-%offset%)
           p ip
           (f2cl-lib:array-slice isp-%data%
                                 f2cl-lib:integer4
                                 (v)
                                 ((1 *))
                                 isp-%offset%)
           flag)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf flag var-10))
      (if (/= flag 0) (go label100))
     label1
      (if
       (/=
        (f2cl-lib:int-mul (f2cl-lib:int-sub path 2)
                          (f2cl-lib:int-sub path 3)
                          (f2cl-lib:int-sub path 4)
                          (f2cl-lib:int-sub path 5))
        0)
       (go label2))
      (setf tmp (f2cl-lib:int-sub (f2cl-lib:int-add nsp 1) n))
      (setf q
              (f2cl-lib:int-sub tmp
                                (f2cl-lib:int-sub
                                 (f2cl-lib:fref ia-%data%
                                                ((f2cl-lib:int-add n 1))
                                                ((1 *))
                                                ia-%offset%)
                                 1)))
      (if (< q 1) (go label110))
      (setf dflag (or (= path 4) (= path 5)))
      (sro n ip ia ja a
       (f2cl-lib:array-slice isp-%data%
                             f2cl-lib:integer4
                             (tmp)
                             ((1 *))
                             isp-%offset%)
       (f2cl-lib:array-slice isp-%data%
                             f2cl-lib:integer4
                             (q)
                             ((1 *))
                             isp-%offset%)
       dflag)
     label2
      (go end_label)
     label100
      (go end_label)
     label110
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 10 n) 1))
      (go end_label)
     label111
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 11 n) 1))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil flag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::odrv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls '(fortran-to-lisp::sro fortran-to-lisp::md))))

