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


(let ((dum 0.0)
      (lmfpn 0.0)
      (ntur 0.0)
      (rtol 0.0)
      (sum 0.0)
      (i 0)
      (iflag 0)
      (f2cl-lib:index 0)
      (irmax 0)
      (j 0)
      (jj 0)
      (k 0)
      (lenr 0)
      (n2 0)
      (s 0))
  (declare (type (double-float) dum lmfpn ntur rtol sum)
           (type (f2cl-lib:integer4) i iflag f2cl-lib:index irmax j jj k lenr
                                     n2 s))
  (defun sclgnp
         (n nn mmaxt numt deg mode eps0 coef nnumt ddeg ccoef alpha beta rwork
          xwork facv face coescl ierr)
    (declare (type (array double-float (*)) coescl face facv xwork rwork beta
                                            alpha ccoef coef)
             (type (double-float) eps0)
             (type (array f2cl-lib:integer4 (*)) ddeg nnumt deg numt)
             (type (f2cl-lib:integer4) ierr mode mmaxt nn n))
    (f2cl-lib:with-multi-array-data
        ((numt f2cl-lib:integer4 numt-%data% numt-%offset%)
         (deg f2cl-lib:integer4 deg-%data% deg-%offset%)
         (nnumt f2cl-lib:integer4 nnumt-%data% nnumt-%offset%)
         (ddeg f2cl-lib:integer4 ddeg-%data% ddeg-%offset%)
         (coef double-float coef-%data% coef-%offset%)
         (ccoef double-float ccoef-%data% ccoef-%offset%)
         (alpha double-float alpha-%data% alpha-%offset%)
         (beta double-float beta-%data% beta-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%)
         (xwork double-float xwork-%data% xwork-%offset%)
         (facv double-float facv-%data% facv-%offset%)
         (face double-float face-%data% face-%offset%)
         (coescl double-float coescl-%data% coescl-%offset%))
      (prog ()
        (declare)
        (setf ierr 0)
        (setf n2 (f2cl-lib:int-mul 2 n))
        (setf lmfpn (f2cl-lib:d1mach 2))
        (setf ntur (* (f2cl-lib:d1mach 4) n))
        (setf lenr (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf jj 0)
            (setf (f2cl-lib:fref nnumt-%data% (i) ((1 n)) nnumt-%offset%) 0)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:fref numt (i) ((1 nn)))) nil)
              (tagbody
                (cond
                  ((> (abs (f2cl-lib:fref coef (i j) ((1 nn) (1 mmaxt)))) eps0)
                   (setf jj (f2cl-lib:int-add jj 1))
                   (setf (f2cl-lib:fref nnumt-%data%
                                        (i)
                                        ((1 n))
                                        nnumt-%offset%)
                           (f2cl-lib:int-add
                            (f2cl-lib:fref nnumt-%data%
                                           (i)
                                           ((1 n))
                                           nnumt-%offset%)
                            1))
                   (setf (f2cl-lib:fref ccoef-%data%
                                        (i jj)
                                        ((1 n) (1 mmaxt))
                                        ccoef-%offset%)
                           (f2cl-lib:fref coef-%data%
                                          (i j)
                                          ((1 nn) (1 mmaxt))
                                          coef-%offset%))
                   (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                 ((> k n) nil)
                     (tagbody
                       (setf (f2cl-lib:fref ddeg-%data%
                                            (i k jj)
                                            ((1 n) (1 (f2cl-lib:int-add n 1))
                                             (1 mmaxt))
                                            ddeg-%offset%)
                               (f2cl-lib:fref deg-%data%
                                              (i k j)
                                              ((1 nn)
                                               (1 (f2cl-lib:int-add nn 1))
                                               (1 mmaxt))
                                              deg-%offset%))
                      label20))))
               label40))
           label60))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:fref nnumt (i) ((1 n)))) nil)
              (tagbody
                (setf (f2cl-lib:fref coescl-%data%
                                     (i j)
                                     ((1 n) (1 mmaxt))
                                     coescl-%offset%)
                        (f2cl-lib:log10
                         (abs
                          (f2cl-lib:fref ccoef-%data%
                                         (i j)
                                         ((1 n) (1 mmaxt))
                                         ccoef-%offset%))))
               label80))
           label90))
        (cond
          ((= mode 0)
           (tagbody
             (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                           ((> s n) nil)
               (tagbody
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref alpha-%data%
                                          (s k)
                                          ((1 (f2cl-lib:int-mul 2 n))
                                           (1 (f2cl-lib:int-mul 2 n)))
                                          alpha-%offset%)
                             (coerce (the f2cl-lib:integer4 0) 'double-float))
                    label110))))
            label110
             (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                           ((> s n) nil)
               (tagbody
                 (setf (f2cl-lib:fref alpha-%data%
                                      (s s)
                                      ((1 (f2cl-lib:int-mul 2 n))
                                       (1 (f2cl-lib:int-mul 2 n)))
                                      alpha-%offset%)
                         (coerce
                          (the f2cl-lib:integer4
                               (f2cl-lib:fref nnumt-%data%
                                              (s)
                                              ((1 n))
                                              nnumt-%offset%))
                          'double-float))
                label200))
             (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                           ((> s n) nil)
               (tagbody
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf sum
                             (coerce (the f2cl-lib:integer4 0) 'double-float))
                     (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                   ((> j (f2cl-lib:fref nnumt (i) ((1 n)))) nil)
                       (tagbody
                         (setf sum
                                 (+ sum
                                    (f2cl-lib:fref ddeg-%data%
                                                   (i s j)
                                                   ((1 n)
                                                    (1 (f2cl-lib:int-add n 1))
                                                    (1 mmaxt))
                                                   ddeg-%offset%)))
                        label220))
                     (setf (f2cl-lib:fref alpha-%data%
                                          ((f2cl-lib:int-add n s) i)
                                          ((1 (f2cl-lib:int-mul 2 n))
                                           (1 (f2cl-lib:int-mul 2 n)))
                                          alpha-%offset%)
                             sum)
                    label300))))
            label300
             (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                           ((> s n) nil)
               (tagbody
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf sum
                             (coerce (the f2cl-lib:integer4 0) 'double-float))
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                       ((> j (f2cl-lib:fref nnumt (i) ((1 n))))
                                        nil)
                           (tagbody
                             (setf sum
                                     (+ sum
                                        (f2cl-lib:int-mul
                                         (f2cl-lib:fref ddeg-%data%
                                                        (i s j)
                                                        ((1 n)
                                                         (1
                                                          (f2cl-lib:int-add n
                                                                            1))
                                                         (1 mmaxt))
                                                        ddeg-%offset%)
                                         (f2cl-lib:fref ddeg-%data%
                                                        (i k j)
                                                        ((1 n)
                                                         (1
                                                          (f2cl-lib:int-add n
                                                                            1))
                                                         (1 mmaxt))
                                                        ddeg-%offset%))))
                            label310))
                        label320))
                     (setf (f2cl-lib:fref alpha-%data%
                                          ((f2cl-lib:int-add n s)
                                           (f2cl-lib:int-add n k))
                                          ((1 (f2cl-lib:int-mul 2 n))
                                           (1 (f2cl-lib:int-mul 2 n)))
                                          alpha-%offset%)
                             sum)
                    label330))
                label400))
             (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                           ((> s n) nil)
               (tagbody
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf sum
                             (coerce (the f2cl-lib:integer4 0) 'double-float))
                     (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                   ((> j (f2cl-lib:fref nnumt (s) ((1 n)))) nil)
                       (tagbody
                         (setf sum
                                 (+ sum
                                    (f2cl-lib:fref ddeg-%data%
                                                   (s k j)
                                                   ((1 n)
                                                    (1 (f2cl-lib:int-add n 1))
                                                    (1 mmaxt))
                                                   ddeg-%offset%)))
                        label420))
                     (setf (f2cl-lib:fref alpha-%data%
                                          (s (f2cl-lib:int-add n k))
                                          ((1 (f2cl-lib:int-mul 2 n))
                                           (1 (f2cl-lib:int-mul 2 n)))
                                          alpha-%offset%)
                             sum)
                    label500))))
            label500
             (multiple-value-bind (var-0 var-1 var-2 var-3)
                 (qrfaqf alpha rwork (f2cl-lib:int-mul 2 n) iflag)
               (declare (ignore var-0 var-1 var-2))
               (setf iflag var-3))
             (setf irmax (idamax lenr rwork 1))
             (setf rtol
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     (irmax)
                                     ((1
                                       (f2cl-lib:int-mul n
                                                         (f2cl-lib:int-add
                                                          (f2cl-lib:int-mul 2
                                                                            n)
                                                          1))))
                                     rwork-%offset%)
                      ntur))
             (setf f2cl-lib:index 1)
             (f2cl-lib:fdo (i n (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                           ((> i 2) nil)
               (tagbody
                 (cond
                   ((<
                     (abs
                      (f2cl-lib:fref rwork
                                     (f2cl-lib:index)
                                     ((1
                                       (f2cl-lib:int-mul n
                                                         (f2cl-lib:int-add
                                                          (f2cl-lib:int-mul 2
                                                                            n)
                                                          1))))))
                     rtol)
                    (setf (f2cl-lib:fref rwork-%data%
                                         (f2cl-lib:index)
                                         ((1
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               n)
                                                              1))))
                                         rwork-%offset%)
                            lmfpn)
                    (setf ierr 1)))
                 (setf f2cl-lib:index (f2cl-lib:int-add f2cl-lib:index i))
                label510))
             (cond
               ((<
                 (abs
                  (f2cl-lib:fref rwork
                                 (f2cl-lib:index)
                                 ((1
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 n)
                                                      1))))))
                 rtol)
                (setf (f2cl-lib:fref rwork-%data%
                                     (f2cl-lib:index)
                                     ((1
                                       (f2cl-lib:int-mul n
                                                         (f2cl-lib:int-add
                                                          (f2cl-lib:int-mul 2
                                                                            n)
                                                          1))))
                                     rwork-%offset%)
                        lmfpn)
                (setf ierr 1))))))
        (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                      ((> s n) nil)
          (tagbody
            (setf sum (coerce (the f2cl-lib:integer4 0) 'double-float))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:fref nnumt (s) ((1 n)))) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (f2cl-lib:fref coescl-%data%
                                          (s j)
                                          ((1 n) (1 mmaxt))
                                          coescl-%offset%)))
               label550))
            (setf (f2cl-lib:fref beta-%data%
                                 (s)
                                 ((1 (f2cl-lib:int-mul 2 n)))
                                 beta-%offset%)
                    (- sum))
           label600))
        (f2cl-lib:fdo (s 1 (f2cl-lib:int-add s 1))
                      ((> s n) nil)
          (tagbody
            (setf sum (coerce (the f2cl-lib:integer4 0) 'double-float))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j (f2cl-lib:fref nnumt (i) ((1 n)))) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (*
                                (f2cl-lib:fref coescl-%data%
                                               (i j)
                                               ((1 n) (1 mmaxt))
                                               coescl-%offset%)
                                (f2cl-lib:fref ddeg-%data%
                                               (i s j)
                                               ((1 n)
                                                (1 (f2cl-lib:int-add n 1))
                                                (1 mmaxt))
                                               ddeg-%offset%))))
                   label610))
               label620))
            (setf (f2cl-lib:fref beta-%data%
                                 ((f2cl-lib:int-add n s))
                                 ((1 (f2cl-lib:int-mul 2 n)))
                                 beta-%offset%)
                    (- sum))
           label700))
        (qrslqf alpha rwork beta xwork (f2cl-lib:int-mul 2 n))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref face-%data% (i) ((1 n)) face-%offset%)
                    (f2cl-lib:fref beta-%data%
                                   (i)
                                   ((1 (f2cl-lib:int-mul 2 n)))
                                   beta-%offset%))
            (setf (f2cl-lib:fref facv-%data% (i) ((1 n)) facv-%offset%)
                    (f2cl-lib:fref beta-%data%
                                   ((f2cl-lib:int-add n i))
                                   ((1 (f2cl-lib:int-mul 2 n)))
                                   beta-%offset%))
           label800))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:fref numt (i) ((1 nn)))) nil)
              (tagbody
                (setf dum
                        (abs
                         (f2cl-lib:fref coef-%data%
                                        (i j)
                                        ((1 nn) (1 mmaxt))
                                        coef-%offset%)))
                (cond
                  ((= dum 0.0f0)
                   (setf (f2cl-lib:fref coescl-%data%
                                        (i j)
                                        ((1 n) (1 mmaxt))
                                        coescl-%offset%)
                           (coerce 0.0f0 'double-float)))
                  (t
                   (setf sum
                           (+
                            (f2cl-lib:fref face-%data%
                                           (i)
                                           ((1 n))
                                           face-%offset%)
                            (f2cl-lib:log10 dum)))
                   (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                 ((> k n) nil)
                     (tagbody
                       (setf sum
                               (+ sum
                                  (*
                                   (f2cl-lib:fref facv-%data%
                                                  (k)
                                                  ((1 n))
                                                  facv-%offset%)
                                   (f2cl-lib:fref deg-%data%
                                                  (i k j)
                                                  ((1 nn)
                                                   (1 (f2cl-lib:int-add nn 1))
                                                   (1 mmaxt))
                                                  deg-%offset%))))
                      label810))
                   (setf (f2cl-lib:fref coescl-%data%
                                        (i j)
                                        ((1 n) (1 mmaxt))
                                        coescl-%offset%)
                           (f2cl-lib:sign (expt 10.0f0 sum)
                                          (f2cl-lib:fref coef-%data%
                                                         (i j)
                                                         ((1 nn) (1 mmaxt))
                                                         coef-%offset%)))))
               label820))
           label900))
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
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 ierr))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::sclgnp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::idamax fortran-to-lisp::qrslqf
                    fortran-to-lisp::qrfaqf fortran-to-lisp::d1mach))))

