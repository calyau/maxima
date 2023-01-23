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


(defun polsys
       (n numt coef kdeg iflg1 iflg2 epsbig epssml sspar numrr nn mmaxt ttotdg
        lenwk leniwk lambda$ roots arclen nfe wk iwk)
  (declare (type (array double-float (*)) sspar)
           (type (double-float) epssml epsbig)
           (type (array double-float (*)) wk arclen roots lambda$ coef)
           (type (array f2cl-lib:integer4 (*)) iwk nfe iflg2 kdeg numt)
           (type (f2cl-lib:integer4) leniwk lenwk ttotdg mmaxt nn numrr iflg1
                                     n))
  (f2cl-lib:with-multi-array-data
      ((numt f2cl-lib:integer4 numt-%data% numt-%offset%)
       (kdeg f2cl-lib:integer4 kdeg-%data% kdeg-%offset%)
       (iflg2 f2cl-lib:integer4 iflg2-%data% iflg2-%offset%)
       (nfe f2cl-lib:integer4 nfe-%data% nfe-%offset%)
       (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%)
       (coef double-float coef-%data% coef-%offset%)
       (lambda$ double-float lambda$-%data% lambda$-%offset%)
       (roots double-float roots-%data% roots-%offset%)
       (arclen double-float arclen-%data% arclen-%offset%)
       (wk double-float wk-%data% wk-%offset%)
       (sspar double-float sspar-%data% sspar-%offset%))
    (prog ((lwk (make-array 19 :element-type 'f2cl-lib:integer4))
           (liwk (make-array 4 :element-type 'f2cl-lib:integer4))
           (wkoff (make-array 19 :element-type 'f2cl-lib:integer4))
           (iwkoff (make-array 4 :element-type 'f2cl-lib:integer4)) (i 0)
           (ideg 0) (iideg 0) (j 0) (k 0) (l 0) (leniww 0) (lenwkk 0) (maxt 0)
           (n2 0) (totdg 0))
      (declare (type (array f2cl-lib:integer4 (19)) wkoff lwk)
               (type (array f2cl-lib:integer4 (4)) liwk iwkoff)
               (type (f2cl-lib:integer4) totdg n2 maxt lenwkk leniww l k j
                                         iideg ideg i))
      (cond
        ((< nn n)
         (setf iflg1 -1)
         (go end_label)))
      (setf maxt 0)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (if (< maxt (f2cl-lib:fref numt-%data% (j) ((1 nn)) numt-%offset%))
              (setf maxt
                      (f2cl-lib:fref numt-%data% (j) ((1 nn)) numt-%offset%)))
         label50))
      (cond
        ((< mmaxt maxt)
         (setf iflg1 -2)
         (go end_label)))
      (setf totdg 1)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf ideg 0)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 nn)))) nil)
            (tagbody
              (setf iideg 0)
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l n) nil)
                (tagbody
                  (setf iideg
                          (f2cl-lib:int-add iideg
                                            (f2cl-lib:fref kdeg-%data%
                                                           (j l k)
                                                           ((1 nn)
                                                            (1
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                                            (1 mmaxt))
                                                           kdeg-%offset%)))
                 label60))
              (if (> iideg ideg) (setf ideg iideg))
             label70))
          (setf totdg (f2cl-lib:int-mul totdg ideg))
         label80))
      (cond
        ((< ttotdg totdg)
         (setf iflg1 -3)
         (go end_label)))
      (setf lenwkk
              (f2cl-lib:int-add 21
                                (f2cl-lib:int-mul 61 n)
                                (f2cl-lib:int-mul 10 (expt n 2))
                                (f2cl-lib:int-mul 7 n mmaxt)
                                (f2cl-lib:int-mul 4 (expt n 2) mmaxt)))
      (cond
        ((< lenwk lenwkk)
         (setf iflg1 -4)
         (go end_label)))
      (setf leniww
              (f2cl-lib:int-add 43
                                (f2cl-lib:int-mul 7 n)
                                (f2cl-lib:int-mul n
                                                  (f2cl-lib:int-add n 1)
                                                  mmaxt)))
      (cond
        ((< leniwk leniww)
         (setf iflg1 -5)
         (go end_label)))
      (cond
        ((and (/= iflg1 0) (/= iflg1 1) (/= iflg1 10) (/= iflg1 11))
         (setf iflg1 -6)
         (go end_label)))
      (setf n2 (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lwk (1) ((1 19))) n2)
      (setf (f2cl-lib:fref lwk (2) ((1 19))) n2)
      (setf (f2cl-lib:fref lwk (3) ((1 19))) n2)
      (setf (f2cl-lib:fref lwk (4) ((1 19))) n)
      (setf (f2cl-lib:fref lwk (5) ((1 19)))
              (f2cl-lib:int-mul 2 (f2cl-lib:int-add n 1)))
      (setf (f2cl-lib:fref lwk (6) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (7) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (8) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (9) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (10) ((1 19)))
              (f2cl-lib:int-mul n2 (f2cl-lib:int-add n2 2)))
      (setf (f2cl-lib:fref lwk (11) ((1 19))) n2)
      (setf (f2cl-lib:fref lwk (12) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (13) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (14) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (15) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (16) ((1 19))) (f2cl-lib:int-add n2 1))
      (setf (f2cl-lib:fref lwk (17) ((1 19))) 8)
      (setf (f2cl-lib:fref lwk (18) ((1 19)))
              (f2cl-lib:int-add 2
                                (f2cl-lib:int-mul 28 n)
                                (f2cl-lib:int-mul 6 (expt n 2))
                                (f2cl-lib:int-mul 7 n mmaxt)
                                (f2cl-lib:int-mul 4 (expt n 2) mmaxt)))
      (setf (f2cl-lib:fref liwk (1) ((1 4))) n)
      (setf (f2cl-lib:fref liwk (2) ((1 4))) n)
      (setf (f2cl-lib:fref liwk (3) ((1 4)))
              (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 1))
      (setf (f2cl-lib:fref liwk (4) ((1 4)))
              (f2cl-lib:int-add 42
                                (f2cl-lib:int-mul 2 n)
                                (f2cl-lib:int-mul n
                                                  (f2cl-lib:int-add n 1)
                                                  mmaxt)))
      (setf (f2cl-lib:fref wkoff (1) ((1 19))) 1)
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 18) nil)
        (tagbody
          (setf (f2cl-lib:fref wkoff (i) ((1 19)))
                  (f2cl-lib:int-add
                   (f2cl-lib:fref wkoff ((f2cl-lib:int-sub i 1)) ((1 19)))
                   (f2cl-lib:fref lwk ((f2cl-lib:int-sub i 1)) ((1 19)))))
         label100))
      (setf (f2cl-lib:fref iwkoff (1) ((1 4))) 1)
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 4) nil)
        (tagbody
          (setf (f2cl-lib:fref iwkoff (i) ((1 4)))
                  (f2cl-lib:int-add
                   (f2cl-lib:fref iwkoff ((f2cl-lib:int-sub i 1)) ((1 4)))
                   (f2cl-lib:fref liwk ((f2cl-lib:int-sub i 1)) ((1 4)))))
         label200))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 8) nil)
        (tagbody
          (setf (f2cl-lib:fref wk-%data%
                               ((f2cl-lib:int-add
                                 (f2cl-lib:fref wkoff (17) ((1 19)))
                                 (f2cl-lib:int-sub j 1)))
                               ((1 lenwk))
                               wk-%offset%)
                  (f2cl-lib:fref sspar-%data% (j) ((1 8)) sspar-%offset%))
         label300))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19
             var-20 var-21 var-22 var-23 var-24 var-25 var-26 var-27 var-28
             var-29 var-30 var-31 var-32 var-33 var-34 var-35 var-36 var-37
             var-38)
          (polyp n numt coef kdeg iflg1 iflg2 epsbig epssml numrr nn mmaxt
           ttotdg lambda$ roots arclen nfe totdg
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (1) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (2) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (3) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (4) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (5) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (6) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (7) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (8) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (9) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (10) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (11) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (12) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (13) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (14) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (15) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (16) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (17) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice wk-%data%
                                 double-float
                                 ((f2cl-lib:fref wkoff (18) ((1 19))))
                                 ((1 lenwk))
                                 wk-%offset%)
           (f2cl-lib:array-slice iwk-%data%
                                 f2cl-lib:integer4
                                 ((f2cl-lib:fref iwkoff (1) ((1 4))))
                                 ((1 leniwk))
                                 iwk-%offset%)
           (f2cl-lib:array-slice iwk-%data%
                                 f2cl-lib:integer4
                                 ((f2cl-lib:fref iwkoff (2) ((1 4))))
                                 ((1 leniwk))
                                 iwk-%offset%)
           (f2cl-lib:array-slice iwk-%data%
                                 f2cl-lib:integer4
                                 ((f2cl-lib:fref iwkoff (3) ((1 4))))
                                 ((1 leniwk))
                                 iwk-%offset%)
           (f2cl-lib:array-slice iwk-%data%
                                 f2cl-lib:integer4
                                 ((f2cl-lib:fref iwkoff (4) ((1 4))))
                                 ((1 leniwk))
                                 iwk-%offset%))
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                         var-10 var-11 var-12 var-13 var-14 var-15 var-16
                         var-17 var-18 var-19 var-20 var-21 var-22 var-23
                         var-24 var-25 var-26 var-27 var-28 var-29 var-30
                         var-31 var-32 var-33 var-34 var-35 var-36 var-37
                         var-38))
        (setf numrr var-8))
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               iflg1
               nil
               nil
               nil
               nil
               numrr
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
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::polsys
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*)) (double-float)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil fortran-to-lisp::iflg1 nil nil nil
                            nil fortran-to-lisp::numrr nil nil nil nil nil nil
                            nil nil nil nil nil)
           :calls '(fortran-to-lisp::polyp))))

