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


(defun rfft2b (ldim l m r wsave lensav work lenwrk ier)
  (declare (type (array double-float (*)) work wsave r)
           (type (f2cl-lib:integer4) ier lenwrk lensav m l ldim))
  (f2cl-lib:with-multi-array-data
      ((r double-float r-%data% r-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((i 0) (j 0) (ier1 0) (mwsav 0) (lwsav 0))
      (declare (type (f2cl-lib:integer4) lwsav mwsav ier1 j i))
      (setf ier 0)
      (setf lwsav
              (f2cl-lib:int-add l
                                (f2cl-lib:int
                                 (f2cl-lib:flog (f2cl-lib:freal l)))
                                4))
      (setf mwsav
              (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                (f2cl-lib:int
                                 (f2cl-lib:flog (f2cl-lib:freal m)))
                                4))
      (cond
        ((< lensav (f2cl-lib:int-add lwsav mwsav))
         (setf ier 2)
         (xerfft "RFFT2B" 6)
         (go label100)))
      (cond
        ((< lenwrk
            (f2cl-lib:int-mul 2 (f2cl-lib:int-add (f2cl-lib:f2cl/ l 2) 1) m))
         (setf ier 3)
         (xerfft "RFFT2B" 8)
         (go label100)))
      (cond
        ((< ldim (f2cl-lib:int-mul 2 (f2cl-lib:int-add (f2cl-lib:f2cl/ l 2) 1)))
         (setf ier 5)
         (xerfft "RFFT2B" -6)
         (go label100)))
      (let ((%copy-r
             (f2cl-lib:make-compatible-seq (array f2cl-lib:complex16 (*))
                                           r
                                           (array double-float (*)))))
        (f2cl-lib:f2cl-copy-seq %copy-r r f2cl-lib:complex16 double-float)
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10)
            (cfftmb (+ (the f2cl-lib:integer4 (truncate l 2)) 1) 1 m
             (the f2cl-lib:integer4 (truncate ldim 2)) %copy-r
             (the f2cl-lib:integer4 (truncate (* m ldim) 2))
             (f2cl-lib:array-slice wsave-%data%
                                   double-float
                                   ((+ l
                                       (f2cl-lib:int
                                        (f2cl-lib:flog (f2cl-lib:freal l)))
                                       5))
                                   ((1 lensav))
                                   wsave-%offset%)
             (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                               (f2cl-lib:int
                                (f2cl-lib:flog (f2cl-lib:freal m)))
                               4)
             work (* 2 (+ (the f2cl-lib:integer4 (truncate l 2)) 1) m) ier1)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9))
          (setf ier1 var-10))
        (f2cl-lib:f2cl-copy-seq r %copy-r double-float f2cl-lib:complex16))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "RFFT2B" -5)
         (go label100)))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j m) nil)
        (tagbody
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                        ((> i l) nil)
            (tagbody
              (setf (f2cl-lib:fref r-%data% (i j) ((1 ldim) (1 m)) r-%offset%)
                      (f2cl-lib:fref r-%data%
                                     ((f2cl-lib:int-add i 1) j)
                                     ((1 ldim) (1 m))
                                     r-%offset%))
             label100001))
         label100000))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (rfftmb m ldim l 1 r (f2cl-lib:int-mul m ldim)
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 (1)
                                 ((1 lensav))
                                 wsave-%offset%)
           (f2cl-lib:int-add l
                             (f2cl-lib:int (f2cl-lib:flog (f2cl-lib:freal l)))
                             4)
           work (* 2 (+ (the f2cl-lib:integer4 (truncate l 2)) 1) m) ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf ier1 var-10))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "RFFT2F" -5)
         (go label100)))
     label100
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rfft2b
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::rfftmb fortran-to-lisp::cfftmb
                    fortran-to-lisp::xerfft))))

