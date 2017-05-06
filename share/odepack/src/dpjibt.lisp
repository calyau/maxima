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


(defun dpjibt (neq y yh nyh ewt rtem savr s wm iwm res jac adda)
  (declare (type (f2cl-lib:integer4) nyh)
           (type (array double-float (*)) wm s savr rtem ewt yh y)
           (type (array f2cl-lib:integer4 (*)) iwm neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                      (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34))
                      (nje (aref (dls001-part-1 *dls001-common-block*) 35)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (rtem double-float rtem-%data% rtem-%offset%)
           (savr double-float savr-%data% savr-%offset%)
           (s double-float s-%data% s-%offset%)
           (wm double-float wm-%data% wm-%offset%))
        (prog ((nb 0) (mwid 0) (mbsq 0) (mb 0) (lpc 0) (lpb 0) (lblox 0)
               (lenp 0) (k1 0) (k 0) (j2 0) (j1 0) (j 0) (ires 0) (ipc 0)
               (ipb 0) (ipa 0) (iic 0) (iib 0) (iia 0) (ier 0) (i 0)
               (srur 0.0d0) (r 0.0d0) (hl0 0.0d0) (fac 0.0d0) (con 0.0d0))
          (declare (type (double-float) con fac hl0 r srur)
                   (type (f2cl-lib:integer4) i ier iia iib iic ipa ipb ipc ires
                                             j j1 j2 k k1 lenp lblox lpb lpc mb
                                             mbsq mwid nb))
          (setf nje (f2cl-lib:int-add nje 1))
          (setf hl0 (* h el0))
          (setf ierpj 0)
          (setf jcur 1)
          (setf mb (f2cl-lib:fref iwm-%data% (1) ((1 *)) iwm-%offset%))
          (setf nb (f2cl-lib:fref iwm-%data% (2) ((1 *)) iwm-%offset%))
          (setf mbsq (f2cl-lib:int-mul mb mb))
          (setf lblox (f2cl-lib:int-mul mbsq nb))
          (setf lpb (f2cl-lib:int-add 3 lblox))
          (setf lpc (f2cl-lib:int-add lpb lblox))
          (setf lenp (f2cl-lib:int-mul 3 lblox))
          (f2cl-lib:computed-goto (label100 label200) miter)
         label100
          (setf ires 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label110
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      0.0d0)))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
              (funcall jac
                       neq
                       tn
                       y
                       s
                       mb
                       nb
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (3)
                                             ((1 *))
                                             wm-%offset%)
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (lpb)
                                             ((1 *))
                                             wm-%offset%)
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (lpc)
                                             ((1 *))
                                             wm-%offset%))
            (declare (ignore var-0 var-2 var-3 var-6 var-7 var-8))
            (when var-1
              (setf tn var-1))
            (when var-4
              (setf mb var-4))
            (when var-5
              (setf nb var-5)))
          (setf con (- hl0))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label120
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      (*
                       (f2cl-lib:fref wm-%data%
                                      ((f2cl-lib:int-add i 2))
                                      ((1 *))
                                      wm-%offset%)
                       con))))
          (go label260)
         label200
          (setf ires -1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
          (setf mwid (f2cl-lib:int-mul 3 mb))
          (setf srur (f2cl-lib:fref wm-%data% (1) ((1 *)) wm-%offset%))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label205
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add 2 i))
                                   ((1 *))
                                   wm-%offset%)
                      0.0d0)))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 3) nil)
            (tagbody
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j mb) nil)
                (tagbody
                  (setf j1
                          (f2cl-lib:int-add j
                                            (f2cl-lib:int-mul
                                             (f2cl-lib:int-sub k 1)
                                             mb)))
                  (f2cl-lib:fdo (i j1 (f2cl-lib:int-add i mwid))
                                ((> i n) nil)
                    (tagbody
                      (setf r
                              (max
                               (* srur
                                  (abs
                                   (f2cl-lib:fref y-%data%
                                                  (i)
                                                  ((1 *))
                                                  y-%offset%)))
                               (/ 0.01d0
                                  (f2cl-lib:fref ewt-%data%
                                                 (i)
                                                 ((1 *))
                                                 ewt-%offset%))))
                      (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                              (+
                               (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                               r))
                     label210))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                      (funcall res neq tn y s rtem ires)
                    (declare (ignore var-0 var-2 var-3 var-4))
                    (when var-1
                      (setf tn var-1))
                    (when var-5
                      (setf ires var-5)))
                  (setf nfe (f2cl-lib:int-add nfe 1))
                  (if (> ires 1) (go label600))
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                     label215
                      (setf (f2cl-lib:fref rtem-%data%
                                           (i)
                                           ((1 *))
                                           rtem-%offset%)
                              (-
                               (f2cl-lib:fref rtem-%data%
                                              (i)
                                              ((1 *))
                                              rtem-%offset%)
                               (f2cl-lib:fref savr-%data%
                                              (i)
                                              ((1 *))
                                              savr-%offset%)))))
                  (setf k1 k)
                  (f2cl-lib:fdo (i j1 (f2cl-lib:int-add i mwid))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                              (f2cl-lib:fref yh-%data%
                                             (i 1)
                                             ((1 nyh) (1 *))
                                             yh-%offset%))
                      (setf r
                              (max
                               (* srur
                                  (abs
                                   (f2cl-lib:fref y-%data%
                                                  (i)
                                                  ((1 *))
                                                  y-%offset%)))
                               (/ 0.01d0
                                  (f2cl-lib:fref ewt-%data%
                                                 (i)
                                                 ((1 *))
                                                 ewt-%offset%))))
                      (setf fac (/ (- hl0) r))
                      (setf iia (f2cl-lib:int-sub i j))
                      (setf ipa
                              (f2cl-lib:int-add 2
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-sub j 1)
                                                 mb)
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-sub k1 1)
                                                 mbsq)))
                      (f2cl-lib:fdo (j2 1 (f2cl-lib:int-add j2 1))
                                    ((> j2 mb) nil)
                        (tagbody
                         label221
                          (setf (f2cl-lib:fref wm-%data%
                                               ((f2cl-lib:int-add ipa j2))
                                               ((1 *))
                                               wm-%offset%)
                                  (*
                                   (f2cl-lib:fref rtem-%data%
                                                  ((f2cl-lib:int-add iia j2))
                                                  ((1 *))
                                                  rtem-%offset%)
                                   fac))))
                      (if (<= k1 1) (go label223))
                      (setf iib (f2cl-lib:int-sub iia mb))
                      (setf ipb
                              (f2cl-lib:int-sub (f2cl-lib:int-add ipa lblox)
                                                mbsq))
                      (f2cl-lib:fdo (j2 1 (f2cl-lib:int-add j2 1))
                                    ((> j2 mb) nil)
                        (tagbody
                         label222
                          (setf (f2cl-lib:fref wm-%data%
                                               ((f2cl-lib:int-add ipb j2))
                                               ((1 *))
                                               wm-%offset%)
                                  (*
                                   (f2cl-lib:fref rtem-%data%
                                                  ((f2cl-lib:int-add iib j2))
                                                  ((1 *))
                                                  rtem-%offset%)
                                   fac))))
                     label223
                      (if (>= k1 nb) (go label225))
                      (setf iic (f2cl-lib:int-add iia mb))
                      (setf ipc
                              (f2cl-lib:int-add ipa
                                                (f2cl-lib:int-mul 2 lblox)
                                                mbsq))
                      (f2cl-lib:fdo (j2 1 (f2cl-lib:int-add j2 1))
                                    ((> j2 mb) nil)
                        (tagbody
                         label224
                          (setf (f2cl-lib:fref wm-%data%
                                               ((f2cl-lib:int-add ipc j2))
                                               ((1 *))
                                               wm-%offset%)
                                  (*
                                   (f2cl-lib:fref rtem-%data%
                                                  ((f2cl-lib:int-add iic j2))
                                                  ((1 *))
                                                  rtem-%offset%)
                                   fac))))
                     label225
                      (if (/= k1 3) (go label227))
                      (setf ipc
                              (f2cl-lib:int-add
                               (f2cl-lib:int-sub ipa (f2cl-lib:int-mul 2 mbsq))
                               (f2cl-lib:int-mul 2 lblox)))
                      (f2cl-lib:fdo (j2 1 (f2cl-lib:int-add j2 1))
                                    ((> j2 mb) nil)
                        (tagbody
                         label226
                          (setf (f2cl-lib:fref wm-%data%
                                               ((f2cl-lib:int-add ipc j2))
                                               ((1 *))
                                               wm-%offset%)
                                  (*
                                   (f2cl-lib:fref rtem-%data%
                                                  (j2)
                                                  ((1 *))
                                                  rtem-%offset%)
                                   fac))))
                     label227
                      (if (/= k1 (f2cl-lib:int-sub nb 2)) (go label229))
                      (setf iib (f2cl-lib:int-sub n mb))
                      (setf ipb
                              (f2cl-lib:int-add ipa
                                                (f2cl-lib:int-mul 2 mbsq)
                                                lblox))
                      (f2cl-lib:fdo (j2 1 (f2cl-lib:int-add j2 1))
                                    ((> j2 mb) nil)
                        (tagbody
                         label228
                          (setf (f2cl-lib:fref wm-%data%
                                               ((f2cl-lib:int-add ipb j2))
                                               ((1 *))
                                               wm-%offset%)
                                  (*
                                   (f2cl-lib:fref rtem-%data%
                                                  ((f2cl-lib:int-add iib j2))
                                                  ((1 *))
                                                  rtem-%offset%)
                                   fac))))
                     label229
                      (setf k1 (f2cl-lib:int-add k1 3))
                     label230))
                 label240))
             label250))
          (setf ires 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res neq tn y s savr ires)
            (declare (ignore var-0 var-2 var-3 var-4))
            (when var-1
              (setf tn var-1))
            (when var-5
              (setf ires var-5)))
          (setf nfe (f2cl-lib:int-add nfe 1))
          (if (> ires 1) (go label600))
         label260
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (funcall adda
                       neq
                       tn
                       y
                       mb
                       nb
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (3)
                                             ((1 *))
                                             wm-%offset%)
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (lpb)
                                             ((1 *))
                                             wm-%offset%)
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (lpc)
                                             ((1 *))
                                             wm-%offset%))
            (declare (ignore var-0 var-2 var-5 var-6 var-7))
            (when var-1
              (setf tn var-1))
            (when var-3
              (setf mb var-3))
            (when var-4
              (setf nb var-4)))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (ddecbt mb nb
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (3)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lpb)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (lpc)
                                     ((1 *))
                                     wm-%offset%)
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (21)
                                     ((1 *))
                                     iwm-%offset%)
               ier)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
            (setf ier var-6))
          (if (/= ier 0) (setf ierpj 1))
          (go end_label)
         label600
          (setf ierpj ires)
          (go end_label)
         end_label
          (return
           (values nil nil nil nil nil nil nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dpjibt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::ddecbt))))

