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


(defun dsolpk (neq y savf x ewt wm iwm f psol)
  (declare (type (array double-float (*)) wm ewt x savf y)
           (type (array f2cl-lib:integer4 (*)) iwm neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (iersl (aref (dls001-part-1 *dls001-common-block*) 14))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (delt (aref (dlpk01-part-0 *dlpk01-common-block*) 0))
                      (epcon (aref (dlpk01-part-0 *dlpk01-common-block*) 1))
                      (sqrtn (aref (dlpk01-part-0 *dlpk01-common-block*) 2))
                      (rsqrtn (aref (dlpk01-part-0 *dlpk01-common-block*) 3))
                      (jpre (aref (dlpk01-part-1 *dlpk01-common-block*) 0))
                      (locwp (aref (dlpk01-part-1 *dlpk01-common-block*) 2))
                      (lociwp (aref (dlpk01-part-1 *dlpk01-common-block*) 3))
                      (kmp (aref (dlpk01-part-1 *dlpk01-common-block*) 5))
                      (maxl (aref (dlpk01-part-1 *dlpk01-common-block*) 6))
                      (mnewt (aref (dlpk01-part-1 *dlpk01-common-block*) 7))
                      (nni (aref (dlpk01-part-1 *dlpk01-common-block*) 8))
                      (nli (aref (dlpk01-part-1 *dlpk01-common-block*) 9))
                      (nps (aref (dlpk01-part-1 *dlpk01-common-block*) 10))
                      (ncfl (aref (dlpk01-part-1 *dlpk01-common-block*) 12)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%)
           (y double-float y-%data% y-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (x double-float x-%data% x-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (wm double-float wm-%data% wm-%offset%))
        (prog ((npsl 0) (maxlp1 0) (lz 0) (lwk 0) (lw 0) (lv 0) (lr 0) (lq 0)
               (lp 0) (lpcg 0) (lgmr 0) (liom 0) (lhes 0) (ldl 0) (lb 0)
               (iflag 0) (hl0 0.0d0) (delta 0.0d0))
          (declare (type (double-float) delta hl0)
                   (type (f2cl-lib:integer4) iflag lb ldl lhes liom lgmr lpcg
                                             lp lq lr lv lw lwk lz maxlp1
                                             npsl))
          (setf iersl 0)
          (setf hl0 (* h el0))
          (setf delta (* delt epcon))
          (f2cl-lib:computed-goto
           (label100 label200 label300 label400 label900 label900 label900
            label900 label900)
           miter)
         label100
          (setf lv 1)
          (setf lb (f2cl-lib:int-add lv (f2cl-lib:int-mul n maxl)))
          (setf lhes (f2cl-lib:int-add lb n))
          (setf lwk (f2cl-lib:int-add lhes (f2cl-lib:int-mul maxl maxl)))
          (dcopy n x 1
           (f2cl-lib:array-slice wm-%data%
                                 double-float
                                 (lb)
                                 ((1 *))
                                 wm-%offset%)
           1)
          (dscal n rsqrtn ewt 1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21 var-22 var-23 var-24)
              (dspiom neq tn y savf
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lb)
                                     ((1 *))
                                     wm-%offset%)
               ewt n maxl kmp delta hl0 jpre mnewt f psol npsl x
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lv)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lhes)
                                     ((1 *))
                                     wm-%offset%)
               iwm liom
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (locwp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (lociwp)
                                     ((1 *))
                                     iwm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lwk)
                                     ((1 *))
                                     wm-%offset%)
               iflag)
            (declare (ignore var-0 var-2 var-3 var-4 var-5 var-11 var-12 var-13
                             var-14 var-16 var-17 var-18 var-19 var-21 var-22
                             var-23))
            (setf tn var-1)
            (setf n var-6)
            (setf maxl var-7)
            (setf kmp var-8)
            (setf delta var-9)
            (setf hl0 var-10)
            (setf npsl var-15)
            (setf liom var-20)
            (setf iflag var-24))
          (setf nni (f2cl-lib:int-add nni 1))
          (setf nli (f2cl-lib:int-add nli liom))
          (setf nps (f2cl-lib:int-add nps npsl))
          (dscal n sqrtn ewt 1)
          (if (/= iflag 0) (setf ncfl (f2cl-lib:int-add ncfl 1)))
          (if (>= iflag 2) (setf iersl 1))
          (if (< iflag 0) (setf iersl -1))
          (go end_label)
         label200
          (setf maxlp1 (f2cl-lib:int-add maxl 1))
          (setf lv 1)
          (setf lb (f2cl-lib:int-add lv (f2cl-lib:int-mul n maxl)))
          (setf lhes (f2cl-lib:int-add lb n 1))
          (setf lq (f2cl-lib:int-add lhes (f2cl-lib:int-mul maxl maxlp1)))
          (setf lwk (f2cl-lib:int-add lq (f2cl-lib:int-mul 2 maxl)))
          (setf ldl
                  (f2cl-lib:int-add lwk
                                    (f2cl-lib:int-mul
                                     (min (the f2cl-lib:integer4 1)
                                          (the f2cl-lib:integer4
                                               (f2cl-lib:int-sub maxl kmp)))
                                     n)))
          (dcopy n x 1
           (f2cl-lib:array-slice wm-%data%
                                 double-float
                                 (lb)
                                 ((1 *))
                                 wm-%offset%)
           1)
          (dscal n rsqrtn ewt 1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21 var-22 var-23 var-24 var-25 var-26)
              (dspigmr neq tn y savf
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lb)
                                     ((1 *))
                                     wm-%offset%)
               ewt n maxl maxlp1 kmp delta hl0 jpre mnewt f psol npsl x
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lv)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lhes)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lq)
                                     ((1 *))
                                     wm-%offset%)
               lgmr
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (locwp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (lociwp)
                                     ((1 *))
                                     iwm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lwk)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (ldl)
                                     ((1 *))
                                     wm-%offset%)
               iflag)
            (declare (ignore var-0 var-2 var-3 var-4 var-5 var-7 var-12 var-13
                             var-14 var-15 var-17 var-18 var-19 var-20 var-22
                             var-23 var-24 var-25))
            (setf tn var-1)
            (setf n var-6)
            (setf maxlp1 var-8)
            (setf kmp var-9)
            (setf delta var-10)
            (setf hl0 var-11)
            (setf npsl var-16)
            (setf lgmr var-21)
            (setf iflag var-26))
          (setf nni (f2cl-lib:int-add nni 1))
          (setf nli (f2cl-lib:int-add nli lgmr))
          (setf nps (f2cl-lib:int-add nps npsl))
          (dscal n sqrtn ewt 1)
          (if (/= iflag 0) (setf ncfl (f2cl-lib:int-add ncfl 1)))
          (if (>= iflag 2) (setf iersl 1))
          (if (< iflag 0) (setf iersl -1))
          (go end_label)
         label300
          (setf lr 1)
          (setf lp (f2cl-lib:int-add lr n))
          (setf lw (f2cl-lib:int-add lp n))
          (setf lz (f2cl-lib:int-add lw n))
          (setf lwk (f2cl-lib:int-add lz n))
          (dcopy n x 1
           (f2cl-lib:array-slice wm-%data%
                                 double-float
                                 (lr)
                                 ((1 *))
                                 wm-%offset%)
           1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21 var-22 var-23)
              (dpcg neq tn y savf
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lr)
                                     ((1 *))
                                     wm-%offset%)
               ewt n maxl delta hl0 jpre mnewt f psol npsl x
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lw)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lz)
                                     ((1 *))
                                     wm-%offset%)
               lpcg
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (locwp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (lociwp)
                                     ((1 *))
                                     iwm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lwk)
                                     ((1 *))
                                     wm-%offset%)
               iflag)
            (declare (ignore var-0 var-2 var-3 var-4 var-5 var-12 var-13 var-15
                             var-16 var-17 var-18 var-20 var-21 var-22))
            (when var-1
              (setf tn var-1))
            (when var-6
              (setf n var-6))
            (when var-7
              (setf maxl var-7))
            (when var-8
              (setf delta var-8))
            (when var-9
              (setf hl0 var-9))
            (when var-10
              (setf jpre var-10))
            (when var-11
              (setf mnewt var-11))
            (when var-14
              (setf npsl var-14))
            (when var-19
              (setf lpcg var-19))
            (when var-23
              (setf iflag var-23)))
          (setf nni (f2cl-lib:int-add nni 1))
          (setf nli (f2cl-lib:int-add nli lpcg))
          (setf nps (f2cl-lib:int-add nps npsl))
          (if (/= iflag 0) (setf ncfl (f2cl-lib:int-add ncfl 1)))
          (if (>= iflag 2) (setf iersl 1))
          (if (< iflag 0) (setf iersl -1))
          (go end_label)
         label400
          (setf lr 1)
          (setf lp (f2cl-lib:int-add lr n))
          (setf lw (f2cl-lib:int-add lp n))
          (setf lz (f2cl-lib:int-add lw n))
          (setf lwk (f2cl-lib:int-add lz n))
          (dcopy n x 1
           (f2cl-lib:array-slice wm-%data%
                                 double-float
                                 (lr)
                                 ((1 *))
                                 wm-%offset%)
           1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21 var-22 var-23)
              (dpcgs neq tn y savf
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lr)
                                     ((1 *))
                                     wm-%offset%)
               ewt n maxl delta hl0 jpre mnewt f psol npsl x
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lw)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lz)
                                     ((1 *))
                                     wm-%offset%)
               lpcg
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (locwp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (lociwp)
                                     ((1 *))
                                     iwm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lwk)
                                     ((1 *))
                                     wm-%offset%)
               iflag)
            (declare (ignore var-0 var-2 var-3 var-4 var-5 var-12 var-13 var-15
                             var-16 var-17 var-18 var-20 var-21 var-22))
            (when var-1
              (setf tn var-1))
            (when var-6
              (setf n var-6))
            (when var-7
              (setf maxl var-7))
            (when var-8
              (setf delta var-8))
            (when var-9
              (setf hl0 var-9))
            (when var-10
              (setf jpre var-10))
            (when var-11
              (setf mnewt var-11))
            (when var-14
              (setf npsl var-14))
            (when var-19
              (setf lpcg var-19))
            (when var-23
              (setf iflag var-23)))
          (setf nni (f2cl-lib:int-add nni 1))
          (setf nli (f2cl-lib:int-add nli lpcg))
          (setf nps (f2cl-lib:int-add nps npsl))
          (if (/= iflag 0) (setf ncfl (f2cl-lib:int-add ncfl 1)))
          (if (>= iflag 2) (setf iersl 1))
          (if (< iflag 0) (setf iersl -1))
          (go end_label)
         label900
          (setf lb 1)
          (setf lwk (f2cl-lib:int-add lb n))
          (dcopy n x 1
           (f2cl-lib:array-slice wm-%data%
                                 double-float
                                 (lb)
                                 ((1 *))
                                 wm-%offset%)
           1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
              (dusol neq tn y savf
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lb)
                                     ((1 *))
                                     wm-%offset%)
               ewt n delta hl0 mnewt psol npsl x
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (locwp)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (lociwp)
                                     ((1 *))
                                     iwm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lwk)
                                     ((1 *))
                                     wm-%offset%)
               iflag)
            (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                             var-10 var-12 var-13 var-14 var-15))
            (setf tn var-1)
            (setf hl0 var-8)
            (setf npsl var-11)
            (setf iflag var-16))
          (setf nni (f2cl-lib:int-add nni 1))
          (setf nps (f2cl-lib:int-add nps npsl))
          (if (/= iflag 0) (setf ncfl (f2cl-lib:int-add ncfl 1)))
          (if (= iflag 3) (setf iersl 1))
          (if (< iflag 0) (setf iersl -1))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsolpk
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t)
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dusol fortran-to-lisp::dspigmr
                    fortran-to-lisp::dspiom fortran-to-lisp::dscal
                    fortran-to-lisp::dcopy))))

