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


(let ((lratio 2))
  (declare (type (f2cl-lib:integer4) lratio))
  (defun cdrv (n r c ic ia ja a b z nsp isp rsp esp path flag)
    (declare (type (array double-float (*)) rsp z b a)
             (type (array f2cl-lib:integer4 (*)) isp ja ia ic c r)
             (type (f2cl-lib:integer4) flag path esp nsp n))
    (f2cl-lib:with-multi-array-data
        ((r f2cl-lib:integer4 r-%data% r-%offset%)
         (c f2cl-lib:integer4 c-%data% c-%offset%)
         (ic f2cl-lib:integer4 ic-%data% ic-%offset%)
         (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
         (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
         (isp f2cl-lib:integer4 isp-%data% isp-%offset%)
         (a double-float a-%data% a-%offset%)
         (b double-float b-%data% b-%offset%)
         (z double-float z-%data% z-%offset%)
         (rsp double-float rsp-%data% rsp-%offset%))
      (prog ((d 0) (u 0) (q 0) (row 0) (tmp 0) (ar 0) (umax 0) (lmax 0) (l 0)
             (j 0) (ju 0) (i 0) (jumax 0) (jutmp 0) (jru 0) (iru 0) (irac 0)
             (jra 0) (ira 0) (jlmax 0) (max (the f2cl-lib:integer4 0)) (jl 0)
             (jrl 0) (irl 0) (iju 0) (iu 0) (ijl 0) (il 0))
        (declare (type (f2cl-lib:integer4) il ijl iu iju irl jrl jl max jlmax
                                           ira jra irac iru jru jutmp jumax i
                                           ju j l lmax umax ar tmp row q u d))
        (if (or (< path 1) (< 5 path)) (go label111))
        (setf il 1)
        (setf ijl (f2cl-lib:int-add il (f2cl-lib:int-add n 1)))
        (setf iu (f2cl-lib:int-add ijl n))
        (setf iju (f2cl-lib:int-add iu (f2cl-lib:int-add n 1)))
        (setf irl (f2cl-lib:int-add iju n))
        (setf jrl (f2cl-lib:int-add irl n))
        (setf jl (f2cl-lib:int-add jrl n))
        (if
         (/=
          (f2cl-lib:int-mul (f2cl-lib:int-sub path 1)
                            (f2cl-lib:int-sub path 5))
          0)
         (go label5))
        (setf max
                (f2cl-lib:int-sub
                 (f2cl-lib:int-add (f2cl-lib:int-mul lratio nsp) 1)
                 jl
                 (f2cl-lib:int-add n 1)
                 (f2cl-lib:int-mul 5 n)))
        (setf jlmax (the f2cl-lib:integer4 (truncate max 2)))
        (setf q (f2cl-lib:int-add jl jlmax))
        (setf ira (f2cl-lib:int-add q (f2cl-lib:int-add n 1)))
        (setf jra (f2cl-lib:int-add ira n))
        (setf irac (f2cl-lib:int-add jra n))
        (setf iru (f2cl-lib:int-add irac n))
        (setf jru (f2cl-lib:int-add iru n))
        (setf jutmp (f2cl-lib:int-add jru n))
        (setf jumax
                (f2cl-lib:int-sub
                 (f2cl-lib:int-add (f2cl-lib:int-mul lratio nsp) 1)
                 jutmp))
        (setf esp (the f2cl-lib:integer4 (truncate max lratio)))
        (if (or (<= jlmax 0) (<= jumax 0)) (go label110))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if (/= (f2cl-lib:fref c-%data% (i) ((1 *)) c-%offset%) i)
                (go label2))
           label1))
        (go label3)
       label2
        (setf ar (f2cl-lib:int-sub (f2cl-lib:int-add nsp 1) n))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
            (nroc n ic ia ja a
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (il)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (ar)
                                   ((1 *))
                                   rsp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iu)
                                   ((1 *))
                                   isp-%offset%)
             flag)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
          (setf flag var-8))
        (if (/= flag 0) (go label100))
       label3
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
               var-19 var-20 var-21)
            (nsfc n r ic ia ja jlmax
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (il)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (ijl)
                                   ((1 *))
                                   isp-%offset%)
             jumax
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iu)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jutmp)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iju)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (q)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (ira)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jra)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (irac)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (irl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jrl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iru)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jru)
                                   ((1 *))
                                   isp-%offset%)
             flag)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14
                           var-15 var-16 var-17 var-18 var-19 var-20))
          (setf flag var-21))
        (if (/= flag 0) (go label100))
        (setf jlmax
                (f2cl-lib:fref isp-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add ijl n) 1))
                               ((1 *))
                               isp-%offset%))
        (setf ju (f2cl-lib:int-add jl jlmax))
        (setf jumax
                (f2cl-lib:fref isp-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add iju n) 1))
                               ((1 *))
                               isp-%offset%))
        (if (<= jumax 0) (go label5))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j jumax) nil)
          (tagbody
           label4
            (setf (f2cl-lib:fref isp-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add ju j) 1))
                                 ((1 *))
                                 isp-%offset%)
                    (f2cl-lib:fref isp-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add jutmp j)
                                     1))
                                   ((1 *))
                                   isp-%offset%))))
       label5
        (setf jlmax
                (f2cl-lib:fref isp-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add ijl n) 1))
                               ((1 *))
                               isp-%offset%))
        (setf ju (f2cl-lib:int-add jl jlmax))
        (setf jumax
                (f2cl-lib:fref isp-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add iju n) 1))
                               ((1 *))
                               isp-%offset%))
        (setf l
                (+
                 (the f2cl-lib:integer4
                      (truncate (+ (- (+ ju jumax) 2) lratio) lratio))
                 1))
        (setf lmax
                (f2cl-lib:int-sub
                 (f2cl-lib:fref isp-%data%
                                ((f2cl-lib:int-add il n))
                                ((1 *))
                                isp-%offset%)
                 1))
        (setf d (f2cl-lib:int-add l lmax))
        (setf u (f2cl-lib:int-add d n))
        (setf row (f2cl-lib:int-sub (f2cl-lib:int-add nsp 1) n))
        (setf tmp (f2cl-lib:int-sub row n))
        (setf umax (f2cl-lib:int-sub tmp u))
        (setf esp
                (f2cl-lib:int-sub umax
                                  (f2cl-lib:int-sub
                                   (f2cl-lib:fref isp-%data%
                                                  ((f2cl-lib:int-add iu n))
                                                  ((1 *))
                                                  isp-%offset%)
                                   1)))
        (if
         (/=
          (f2cl-lib:int-mul (f2cl-lib:int-sub path 1)
                            (f2cl-lib:int-sub path 2))
          0)
         (go label6))
        (if (< umax 0) (go label110))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
               var-19 var-20 var-21 var-22 var-23 var-24)
            (nnfc n r c ic ia ja a z b lmax
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (il)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (ijl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (l)
                                   ((1 *))
                                   rsp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (d)
                                   ((1 *))
                                   rsp-%offset%)
             umax
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iu)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (ju)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (iju)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (u)
                                   ((1 *))
                                   rsp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (row)
                                   ((1 *))
                                   rsp-%offset%)
             (f2cl-lib:array-slice rsp-%data%
                                   double-float
                                   (tmp)
                                   ((1 *))
                                   rsp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (irl)
                                   ((1 *))
                                   isp-%offset%)
             (f2cl-lib:array-slice isp-%data%
                                   f2cl-lib:integer4
                                   (jrl)
                                   ((1 *))
                                   isp-%offset%)
             flag)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14
                           var-15 var-16 var-17 var-18 var-19 var-20 var-21
                           var-22 var-23))
          (setf flag var-24))
        (if (/= flag 0) (go label100))
       label6
        (if (/= (f2cl-lib:int-sub path 3) 0) (go label7))
        (nnsc n r c
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (il)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (jl)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (ijl)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (l)
                               ((1 *))
                               rsp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (d)
                               ((1 *))
                               rsp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (iu)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (ju)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (iju)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (u)
                               ((1 *))
                               rsp-%offset%)
         z b
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (tmp)
                               ((1 *))
                               rsp-%offset%))
       label7
        (if (/= (f2cl-lib:int-sub path 4) 0) (go label8))
        (nntc n r c
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (il)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (jl)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (ijl)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (l)
                               ((1 *))
                               rsp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (d)
                               ((1 *))
                               rsp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (iu)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (ju)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice isp-%data%
                               f2cl-lib:integer4
                               (iju)
                               ((1 *))
                               isp-%offset%)
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (u)
                               ((1 *))
                               rsp-%offset%)
         z b
         (f2cl-lib:array-slice rsp-%data%
                               double-float
                               (tmp)
                               ((1 *))
                               rsp-%offset%))
       label8
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
        (return
         (values nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 esp
                 nil
                 flag))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cdrv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::esp nil fortran-to-lisp::flag)
           :calls '(fortran-to-lisp::nntc fortran-to-lisp::nnsc
                    fortran-to-lisp::nnfc fortran-to-lisp::nsfc
                    fortran-to-lisp::nroc))))

