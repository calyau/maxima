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


(let* ((limitd 1000))
  (declare (type (f2cl-lib:integer4 1000 1000) limitd) (ignorable limitd))
  (let ((wk 0.0)
        (s 0.0)
        (relerr 0.0)
        (hold 0.0)
        (h 0.0)
        (abserr 0.0)
        (np1 0)
        (limit 0)
        (jw 0)
        (iter 0)
        (iflagc 0)
        (start nil)
        (crash nil))
    (declare (type (double-float) wk s relerr hold h abserr)
             (type (f2cl-lib:integer4) np1 limit jw iter iflagc)
             (type f2cl-lib:logical start crash))
    (defun fixpqf
           (n y iflag arcre arcae ansre ansae trace$ a nfe arclen yp yold ypold
            qt r f0 f1 z0 dz w t$ ysav sspar par ipar)
      (declare (type (array f2cl-lib:integer4 (*)) ipar)
               (type (array double-float (*)) par)
               (type (array double-float (*)) sspar)
               (type (double-float) arclen ansae ansre arcae arcre)
               (type (array double-float (*)) ysav t$ w dz z0 f1 f0 r qt ypold
                                              yold yp a y)
               (type (f2cl-lib:integer4) nfe trace$ iflag n))
      (f2cl-lib:with-multi-array-data
          ((y double-float y-%data% y-%offset%)
           (a double-float a-%data% a-%offset%)
           (yp double-float yp-%data% yp-%offset%)
           (yold double-float yold-%data% yold-%offset%)
           (ypold double-float ypold-%data% ypold-%offset%)
           (qt double-float qt-%data% qt-%offset%)
           (r double-float r-%data% r-%offset%)
           (f0 double-float f0-%data% f0-%offset%)
           (f1 double-float f1-%data% f1-%offset%)
           (z0 double-float z0-%data% z0-%offset%)
           (dz double-float dz-%data% dz-%offset%)
           (w double-float w-%data% w-%offset%)
           (t$ double-float t$-%data% t$-%offset%)
           (ysav double-float ysav-%data% ysav-%offset%)
           (sspar double-float sspar-%data% sspar-%offset%)
           (par double-float par-%data% par-%offset%)
           (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
        (prog ()
          (declare)
          (if (or (<= n 0) (<= ansre 0.0f0) (< ansae 0.0f0)) (setf iflag 7))
          (if (and (>= iflag -2) (<= iflag 0)) (go label10))
          (if (= iflag 2) (go label50))
          (if (= iflag 3) (go label40))
          (setf iflag 7)
          (go end_label)
         label10
          (setf arclen (coerce 0.0f0 'double-float))
          (if (<= arcre 0.0f0) (setf arcre (* 0.5f0 (f2cl-lib:fsqrt ansre))))
          (if (<= arcae 0.0f0) (setf arcae (* 0.5f0 (f2cl-lib:fsqrt ansae))))
          (setf nfe 0)
          (setf iflagc iflag)
          (setf np1 (f2cl-lib:int-add n 1))
          (setf start f2cl-lib:%true%)
          (setf crash f2cl-lib:%false%)
          (setf relerr arcre)
          (setf abserr arcae)
          (setf hold (coerce 1.0f0 'double-float))
          (setf h (coerce 0.1f0 'double-float))
          (setf s (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref ypold-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               ypold-%offset%)
                  (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref y-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               y-%offset%)
                  (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (jw 2 (f2cl-lib:int-add jw 1))
                        ((> jw np1) nil)
            (tagbody
              (setf (f2cl-lib:fref ypold-%data%
                                   (jw)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   ypold-%offset%)
                      (coerce 0.0f0 'double-float))
             label20))
          (if
           (<= (f2cl-lib:fref sspar-%data% (1) ((1 4)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (1) ((1 4)) sspar-%offset%)
                   (* (+ (f2cl-lib:fsqrt (+ n 1.0f0)) 4.0f0)
                      (f2cl-lib:d1mach 4))))
          (if
           (<= (f2cl-lib:fref sspar-%data% (2) ((1 4)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (2) ((1 4)) sspar-%offset%)
                   (coerce 1.0f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (3) ((1 4)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (3) ((1 4)) sspar-%offset%)
                   (coerce 0.1f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (4) ((1 4)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (4) ((1 4)) sspar-%offset%)
                   (coerce 7.0f0 'double-float)))
          (cond
            ((>= iflagc (f2cl-lib:int-sub 1))
             (dcopy n
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              1 a 1)))
         label40
          (setf limit limitd)
         label50
          (f2cl-lib:fdo (iter 1 (f2cl-lib:int-add iter 1))
                        ((> iter limit) nil)
            (tagbody
              (cond
                ((< (f2cl-lib:fref y (1) ((1 (f2cl-lib:int-add n 1)))) 0.0f0)
                 (setf arclen s)
                 (setf iflag 5)
                 (go end_label)))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                     var-17 var-18 var-19 var-20 var-21 var-22 var-23 var-24
                     var-25 var-26)
                  (stepqf n nfe iflagc start crash hold h wk relerr abserr s y
                   yp yold ypold a qt r f0 f1 z0 dz w t$ sspar par ipar)
                (declare (ignore var-0 var-11 var-12 var-13 var-14 var-15
                                 var-16 var-17 var-18 var-19 var-20 var-21
                                 var-22 var-23 var-24 var-25 var-26))
                (setf nfe var-1)
                (setf iflagc var-2)
                (setf start var-3)
                (setf crash var-4)
                (setf hold var-5)
                (setf h var-6)
                (setf wk var-7)
                (setf relerr var-8)
                (setf abserr var-9)
                (setf s var-10))
              (cond
                ((> trace$ 0)
                 (f2cl-lib:fformat trace
                                   ("~%" " STEP" 1 (("~5D")) "~3@T" "NFE =" 1
                                    (("~5D")) "~3@T" "ARC LENGTH =" 1
                                    (("~9,4,0,'*,F")) "~3@T" "LAMBDA =" 1
                                    (("~7,4,0,'*,F")) "~5@T" "X vector:" "~%" t
                                    ("~1@T" 6 (("~12,4,2,0,'*,,'EE"))) "~%")
                                   iter
                                   nfe
                                   s
                                   (f2cl-lib:fref y-%data%
                                                  (1)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  y-%offset%)
                                   (do ((jw 2 (f2cl-lib:int-add jw 1))
                                        (%ret nil))
                                       ((> jw np1) (nreverse %ret))
                                     (declare (type f2cl-lib:integer4 jw))
                                     (push
                                      (f2cl-lib:fref y-%data%
                                                     (jw)
                                                     ((1
                                                       (f2cl-lib:int-add n 1)))
                                                     y-%offset%)
                                      %ret)))))
              (cond
                ((> iflagc 0)
                 (setf arclen s)
                 (setf iflag iflagc)
                 (go end_label)))
              (cond
                (crash
                 (setf iflag 2)
                 (cond
                   ((< arcre relerr)
                    (setf arcre relerr)
                    (setf ansre relerr)))
                 (if (< arcae abserr) (setf arcae abserr))
                 (setf limit (f2cl-lib:int-sub limit iter))
                 (go end_label)))
              (if
               (>=
                (f2cl-lib:fref y-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               y-%offset%)
                1.0f0)
               (go label500))
             label400))
          (setf arclen s)
          (setf iflag 3)
          (go end_label)
         label500
          (dcopy np1 yold 1 ysav 1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19)
              (rootqf n nfe iflagc ansre ansae y yp yold ypold a qt r dz z0 w
               t$ f0 f1 par ipar)
            (declare (ignore var-0 var-1 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-15
                             var-16 var-17 var-18 var-19))
            (setf iflagc var-2))
          (setf iflag 1)
          (if (> iflagc 0) (setf iflag iflagc))
          (dcopy np1 y 1 dz 1)
          (setf wk (coerce -1.0f0 'double-float))
          (daxpy np1 wk ysav 1 dz 1)
          (setf arclen (+ (- s hold) (dnrm2 np1 dz 1)))
          (go end_label)
         end_label
          (return
           (values nil
                   nil
                   iflag
                   arcre
                   arcae
                   ansre
                   nil
                   nil
                   nil
                   nfe
                   arclen
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
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fixpqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil fortran-to-lisp::iflag
                            fortran-to-lisp::arcre fortran-to-lisp::arcae
                            fortran-to-lisp::ansre nil nil nil
                            fortran-to-lisp::nfe fortran-to-lisp::arclen nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::dnrm2 fortran-to-lisp::daxpy
                    fortran-to-lisp::dcopy fortran-to-lisp::rootqf
                    fortran-to-lisp::stepqf fortran-to-lisp::d1mach))))

