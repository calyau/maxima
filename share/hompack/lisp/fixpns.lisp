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


(let* ((limitd 1000) (cursw (coerce 10.0f0 'double-float)))
  (declare (type (f2cl-lib:integer4 1000 1000) limitd)
           (type (double-float) cursw)
           (ignorable limitd cursw))
  (let ((s 0.0)
        (relerr 0.0)
        (hold 0.0)
        (h 0.0)
        (curtol 0.0)
        (abserr 0.0)
        (np1 0)
        (nfec 0)
        (nc 0)
        (limit 0)
        (jw 0)
        (iz1 0)
        (iz0 0)
        (iwp 0)
        (iw 0)
        (itz 0)
        (iter 0)
        (itangw 0)
        (irho 0)
        (ipp 0)
        (iflagc 0)
        (crash nil)
        (start nil))
    (declare (type (double-float) s relerr hold h curtol abserr)
             (type (f2cl-lib:integer4) np1 nfec nc limit jw iz1 iz0 iwp iw itz
                                       iter itangw irho ipp iflagc)
             (type f2cl-lib:logical crash start))
    (defun fixpns
           (n y iflag arcre arcae ansre ansae trace$ a nfe arclen yp yold ypold
            qr lenqr pivot work sspar par ipar)
      (declare (type (array f2cl-lib:integer4 (*)) ipar)
               (type (array double-float (*)) par)
               (type (array double-float (*)) sspar)
               (type (array f2cl-lib:integer4 (*)) pivot)
               (type (double-float) arclen ansae ansre arcae arcre)
               (type (array double-float (*)) work qr ypold yold yp a y)
               (type (f2cl-lib:integer4) lenqr nfe trace$ iflag n))
      (f2cl-lib:with-multi-array-data
          ((y double-float y-%data% y-%offset%)
           (a double-float a-%data% a-%offset%)
           (yp double-float yp-%data% yp-%offset%)
           (yold double-float yold-%data% yold-%offset%)
           (ypold double-float ypold-%data% ypold-%offset%)
           (qr double-float qr-%data% qr-%offset%)
           (work double-float work-%data% work-%offset%)
           (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
           (sspar double-float sspar-%data% sspar-%offset%)
           (par double-float par-%data% par-%offset%)
           (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
        (prog ()
          (declare)
          (if (or (<= n 0) (<= ansre 0.0f0) (< ansae 0.0f0)) (setf iflag 7))
          (if (and (>= iflag -2) (<= iflag 0)) (go label20))
          (if (= iflag 2) (go label120))
          (if (= iflag 3) (go label90))
          (setf iflag 7)
          (go end_label)
         label20
          (setf arclen (coerce 0.0f0 'double-float))
          (if (<= arcre 0.0f0) (setf arcre (* 0.5f0 (f2cl-lib:fsqrt ansre))))
          (if (<= arcae 0.0f0) (setf arcae (* 0.5f0 (f2cl-lib:fsqrt ansae))))
          (setf nc n)
          (setf nfec 0)
          (setf iflagc iflag)
          (setf np1 (f2cl-lib:int-add n 1))
          (setf ipp 1)
          (setf irho (f2cl-lib:int-add n 1))
          (setf iw (f2cl-lib:int-add irho n))
          (setf iwp (f2cl-lib:int-add iw np1))
          (setf itz (f2cl-lib:int-add iwp np1))
          (setf iz0 (f2cl-lib:int-add itz np1))
          (setf iz1 (f2cl-lib:int-add iz0 np1))
          (setf itangw (f2cl-lib:int-add iz1 np1))
          (setf start f2cl-lib:%true%)
          (setf crash f2cl-lib:%false%)
          (setf hold (coerce 1.0f0 'double-float))
          (setf h (coerce 0.1f0 'double-float))
          (setf s (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref ypold-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               ypold-%offset%)
                  (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref yp-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref y-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               y-%offset%)
                  (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                        ((> jw n) nil)
            (tagbody
              (setf (f2cl-lib:fref ypold-%data%
                                   (jw)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   ypold-%offset%)
                      (coerce 0.0f0 'double-float))
              (setf (f2cl-lib:fref yp-%data%
                                   (jw)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   yp-%offset%)
                      (coerce 0.0f0 'double-float))
             label40))
          (f2cl-lib:fdo (jw itangw (f2cl-lib:int-add jw 1))
                        ((> jw (f2cl-lib:int-add itangw np1 n)) nil)
            (tagbody
              (setf (f2cl-lib:fref work-%data%
                                   (jw)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 13
                                                        (f2cl-lib:int-add n 1))
                                      (f2cl-lib:int-mul 2 n)
                                      lenqr)))
                                   work-%offset%)
                      (coerce 0.0f0 'double-float))
             label50))
          (if
           (<= (f2cl-lib:fref sspar-%data% (1) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (1) ((1 8)) sspar-%offset%)
                   (coerce 0.5f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (2) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (2) ((1 8)) sspar-%offset%)
                   (coerce 0.01f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (3) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (3) ((1 8)) sspar-%offset%)
                   (coerce 0.5f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (4) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (4) ((1 8)) sspar-%offset%)
                   (* (+ (f2cl-lib:fsqrt (+ n 1.0f0)) 4.0f0)
                      (f2cl-lib:d1mach 4))))
          (if
           (<= (f2cl-lib:fref sspar-%data% (5) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (5) ((1 8)) sspar-%offset%)
                   (coerce 1.0f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (6) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (6) ((1 8)) sspar-%offset%)
                   (coerce 0.1f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (7) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (7) ((1 8)) sspar-%offset%)
                   (coerce 3.0f0 'double-float)))
          (if
           (<= (f2cl-lib:fref sspar-%data% (8) ((1 8)) sspar-%offset%) 0.0f0)
           (setf (f2cl-lib:fref sspar-%data% (8) ((1 8)) sspar-%offset%)
                   (coerce 2.0f0 'double-float)))
          (cond
            ((>= iflagc (f2cl-lib:int-sub 1))
             (dcopy n y 1 a 1)))
         label90
          (setf limit limitd)
         label120
          (f2cl-lib:fdo (iter 1 (f2cl-lib:int-add iter 1))
                        ((> iter limit) nil)
            (tagbody
              (cond
                ((< (f2cl-lib:fref y (np1) ((1 (f2cl-lib:int-add n 1)))) 0.0f0)
                 (setf arclen s)
                 (setf iflag 5)
                 (go end_label)))
             label140
              (setf curtol (* cursw hold))
              (setf relerr arcre)
              (setf abserr arcae)
              (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                            ((> jw np1) nil)
                (tagbody
                  (cond
                    ((>
                      (abs
                       (+ (f2cl-lib:fref yp (jw) ((1 (f2cl-lib:int-add n 1))))
                          (-
                           (f2cl-lib:fref ypold
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))))))
                      curtol)
                     (setf relerr ansre)
                     (setf abserr ansae)
                     (go label200)))
                 label160))
             label200
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                     var-17 var-18 var-19 var-20 var-21)
                  (stepns nc nfec iflagc start crash hold h relerr abserr s y
                   yp yold ypold a qr lenqr pivot work sspar par ipar)
                (declare (ignore var-0 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16 var-17 var-18 var-19 var-20
                                 var-21))
                (setf nfec var-1)
                (setf iflagc var-2)
                (setf start var-3)
                (setf crash var-4)
                (setf hold var-5)
                (setf h var-6)
                (setf relerr var-7)
                (setf abserr var-8)
                (setf s var-9))
              (cond
                ((> trace$ 0)
                 (f2cl-lib:fformat trace
                                   ("~%" " STEP" 1 (("~5D")) "~3@T" "NFE =" 1
                                    (("~5D")) "~3@T" "ARC LENGTH =" 1
                                    (("~9,4,0,'*,F")) "~3@T" "LAMBDA =" 1
                                    (("~7,4,0,'*,F")) "~5@T" "X vector:" "~%" t
                                    ("~1@T" 6 (("~12,4,2,0,'*,,'EE"))) "~%")
                                   iter
                                   nfec
                                   s
                                   (f2cl-lib:fref y-%data%
                                                  (np1)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  y-%offset%)
                                   (do ((jw 1 (f2cl-lib:int-add jw 1))
                                        (%ret nil))
                                       ((> jw nc) (nreverse %ret))
                                     (declare (type f2cl-lib:integer4 jw))
                                     (push
                                      (f2cl-lib:fref y-%data%
                                                     (jw)
                                                     ((1
                                                       (f2cl-lib:int-add n 1)))
                                                     y-%offset%)
                                      %ret)))))
              (setf nfe nfec)
              (cond
                ((> iflagc 0)
                 (setf arclen s)
                 (setf iflag iflagc)
                 (go end_label)))
              (cond
                (crash
                 (setf iflag 2)
                 (if (< arcre relerr) (setf arcre relerr))
                 (if (< ansre relerr) (setf ansre relerr))
                 (if (< arcae abserr) (setf arcae abserr))
                 (if (< ansae abserr) (setf ansae abserr))
                 (setf limit (f2cl-lib:int-sub limit iter))
                 (go end_label)))
              (cond
                ((>= (f2cl-lib:fref y (np1) ((1 (f2cl-lib:int-add n 1)))) 1.0f0)
                 (dcopy np1 yold 1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iz0)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                     (rootns nc nfec iflagc ansre ansae y yp yold ypold a qr
                      lenqr pivot work par ipar)
                   (declare (ignore var-0 var-3 var-4 var-5 var-6 var-7 var-8
                                    var-9 var-10 var-11 var-12 var-13 var-14
                                    var-15))
                   (setf nfec var-1)
                   (setf iflagc var-2))
                 (setf nfe nfec)
                 (setf iflag 1)
                 (if (> iflagc 0) (setf iflag iflagc))
                 (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                               ((> jw np1) nil)
                   (tagbody
                     (setf (f2cl-lib:fref work-%data%
                                          (jw)
                                          ((1
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-mul 13
                                                               (f2cl-lib:int-add
                                                                n
                                                                1))
                                             (f2cl-lib:int-mul 2 n)
                                             lenqr)))
                                          work-%offset%)
                             (-
                              (f2cl-lib:fref y-%data%
                                             (jw)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%)
                              (f2cl-lib:fref work-%data%
                                             ((f2cl-lib:int-sub
                                               (f2cl-lib:int-add iz0 jw)
                                               1))
                                             ((1
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-mul 13
                                                                  (f2cl-lib:int-add
                                                                   n
                                                                   1))
                                                (f2cl-lib:int-mul 2 n)
                                                lenqr)))
                                             work-%offset%)))
                    label290))
                 (setf arclen (+ (- s hold) (dnrm2 np1 work 1)))
                 (go end_label)))
             label400))
          (setf iflag 3)
          (setf arclen s)
          (go end_label)
         end_label
          (return
           (values nil
                   nil
                   iflag
                   arcre
                   arcae
                   ansre
                   ansae
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
                   nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fixpns
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil fortran-to-lisp::iflag
                            fortran-to-lisp::arcre fortran-to-lisp::arcae
                            fortran-to-lisp::ansre fortran-to-lisp::ansae nil
                            nil fortran-to-lisp::nfe fortran-to-lisp::arclen
                            nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dnrm2 fortran-to-lisp::dcopy
                    fortran-to-lisp::rootns fortran-to-lisp::stepns
                    fortran-to-lisp::d1mach))))

