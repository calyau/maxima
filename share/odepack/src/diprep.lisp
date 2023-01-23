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
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defstruct (dls001
             (:predicate is-dls001-p))
  (part-0 (make-array 218 :element-type 'double-float)
          :type (simple-array double-float (218)))
  (part-1 (make-array 37 :element-type 'f2cl-lib:integer4)
          :type (simple-array f2cl-lib:integer4 (37))))


(defparameter *dls001-common-block*
  (let* ()
    (declare (ignorable))
    (make-dls001)))


(defstruct (dlss01
             (:predicate is-dlss01-p))
  (part-0 (make-array 6 :element-type 'double-float)
          :type (simple-array double-float (6)))
  (part-1 (make-array 34 :element-type '(f2cl-lib:integer4))
          :type (simple-array (f2cl-lib:integer4) (34))))


(defparameter *dlss01-common-block*
  (let* ()
    (declare (ignorable))
    (make-dlss01)))


(defun diprep (neq y rwork ia ja ipflag f jac)
  (declare (type (f2cl-lib:integer4) ipflag)
           (type (array double-float (*)) rwork y)
           (type (array f2cl-lib:integer4 (*)) ja ia neq))
  (let ()
    (symbol-macrolet ((lyh (aref (dls001-part-1 *dls001-common-block*) 19))
                      (lewt (aref (dls001-part-1 *dls001-common-block*) 20))
                      (lacor (aref (dls001-part-1 *dls001-common-block*) 21))
                      (lsavf (aref (dls001-part-1 *dls001-common-block*) 22))
                      (lwm (aref (dls001-part-1 *dls001-common-block*) 23))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (istatc (aref (dlss01-part-1 *dlss01-common-block*) 2))
                      (lenyh (aref (dlss01-part-1 *dlss01-common-block*) 18))
                      (lenyhm (aref (dlss01-part-1 *dlss01-common-block*) 19))
                      (lenwk (aref (dlss01-part-1 *dlss01-common-block*) 20))
                      (lreq (aref (dlss01-part-1 *dlss01-common-block*) 21))
                      (lwmin (aref (dlss01-part-1 *dlss01-common-block*) 24)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
           (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
           (y double-float y-%data% y-%offset%)
           (rwork double-float rwork-%data% rwork-%offset%))
        (prog ((lyhn 0) (lyhd 0) (lewtn 0) (imax 0) (i 0))
          (declare (type (f2cl-lib:integer4) i imax lewtn lyhd lyhn))
          (setf ipflag 0)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12)
              (dprep neq y
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lyh)
                                     ((1 *))
                                     rwork-%offset%)
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lsavf)
                                     ((1 *))
                                     rwork-%offset%)
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lewt)
                                     ((1 *))
                                     rwork-%offset%)
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lacor)
                                     ((1 *))
                                     rwork-%offset%)
               ia ja
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lwm)
                                     ((1 *))
                                     rwork-%offset%)
               (f2cl-lib:array-slice rwork-%data%
                                     double-float
                                     (lwm)
                                     ((1 *))
                                     rwork-%offset%)
               ipflag f jac)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-11 var-12))
            (setf ipflag var-10))
          (setf lenwk
                  (max (the f2cl-lib:integer4 lreq)
                       (the f2cl-lib:integer4 lwmin)))
          (if (< ipflag 0) (go end_label))
          (setf lyhn (f2cl-lib:int-add lwm lenwk))
          (if (> lyhn lyh) (go end_label))
          (setf lyhd (f2cl-lib:int-sub lyh lyhn))
          (if (= lyhd 0) (go label20))
          (setf imax (f2cl-lib:int-add (f2cl-lib:int-sub lyhn 1) lenyhm))
          (f2cl-lib:fdo (i lyhn (f2cl-lib:int-add i 1))
                        ((> i imax) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref rwork-%data% (i) ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-add i lyhd))
                                     ((1 *))
                                     rwork-%offset%))))
          (setf lyh lyhn)
         label20
          (setf lsavf (f2cl-lib:int-add lyh lenyh))
          (setf lewtn (f2cl-lib:int-add lsavf n))
          (setf lacor (f2cl-lib:int-add lewtn n))
          (if (= istatc 3) (go label40))
          (if (> lewtn lewt) (go end_label))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label30
              (setf (f2cl-lib:fref rwork-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add i lewtn)
                                     1))
                                   ((1 *))
                                   rwork-%offset%)
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i lewt)
                                       1))
                                     ((1 *))
                                     rwork-%offset%))))
         label40
          (setf lewt lewtn)
          (go end_label)
         end_label
          (return (values nil nil nil nil nil ipflag nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::diprep
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) t t)
           :return-values '(nil nil nil nil nil fortran-to-lisp::ipflag nil
                            nil)
           :calls '(fortran-to-lisp::dprep))))

