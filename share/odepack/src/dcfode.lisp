;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2017-01 (21B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "ODEPACK")


(defun dcfode (meth elco tesco)
  (declare (type (array double-float (*)) tesco)
           (type (array double-float (*)) elco)
           (type (f2cl-lib:integer4) meth))
  (prog ((pc (make-array 12 :element-type 'double-float)) (agamq 0.0) (fnq 0.0)
         (fnqm1 0.0) (pint 0.0) (ragq 0.0) (rqfac 0.0) (rq1fac 0.0) (tsign 0.0)
         (xpin 0.0) (i 0) (ib 0) (nq 0) (nqm1 0) (nqp1 0))
    (declare (type (f2cl-lib:integer4) nqp1 nqm1 nq ib i)
             (type (simple-array double-float (12)) pc)
             (type (double-float) xpin tsign rq1fac rqfac ragq pint fnqm1 fnq
                                  agamq))
    (f2cl-lib:computed-goto (label100 label200) meth)
   label100
    (setf (f2cl-lib:fref elco (1 1) ((1 13) (1 12))) 1.0)
    (setf (f2cl-lib:fref elco (2 1) ((1 13) (1 12))) 1.0)
    (setf (f2cl-lib:fref tesco (1 1) ((1 3) (1 12))) 0.0)
    (setf (f2cl-lib:fref tesco (2 1) ((1 3) (1 12))) 2.0)
    (setf (f2cl-lib:fref tesco (1 2) ((1 3) (1 12))) 1.0)
    (setf (f2cl-lib:fref tesco (3 12) ((1 3) (1 12))) 0.0)
    (setf (f2cl-lib:fref pc (1) ((1 12))) 1.0)
    (setf rqfac 1.0)
    (f2cl-lib:fdo (nq 2 (f2cl-lib:int-add nq 1))
                  ((> nq 12) nil)
      (tagbody
        (setf rq1fac rqfac)
        (setf rqfac (/ rqfac nq))
        (setf nqm1 (f2cl-lib:int-sub nq 1))
        (setf fnqm1 (coerce (the f2cl-lib:integer4 nqm1) 'double-float))
        (setf nqp1 (f2cl-lib:int-add nq 1))
        (setf (f2cl-lib:fref pc (nq) ((1 12))) 0.0)
        (f2cl-lib:fdo (ib 1 (f2cl-lib:int-add ib 1))
                      ((> ib nqm1) nil)
          (tagbody
            (setf i (f2cl-lib:int-sub nqp1 ib))
           label110
            (setf (f2cl-lib:fref pc (i) ((1 12)))
                    (+ (f2cl-lib:fref pc ((f2cl-lib:int-sub i 1)) ((1 12)))
                       (* fnqm1 (f2cl-lib:fref pc (i) ((1 12))))))))
        (setf (f2cl-lib:fref pc (1) ((1 12)))
                (* fnqm1 (f2cl-lib:fref pc (1) ((1 12)))))
        (setf pint (f2cl-lib:fref pc (1) ((1 12))))
        (setf xpin (/ (f2cl-lib:fref pc (1) ((1 12))) 2.0))
        (setf tsign 1.0)
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i nq) nil)
          (tagbody
            (setf tsign (- tsign))
            (setf pint
                    (+ pint (/ (* tsign (f2cl-lib:fref pc (i) ((1 12)))) i)))
           label120
            (setf xpin
                    (+ xpin
                       (/ (* tsign (f2cl-lib:fref pc (i) ((1 12))))
                          (f2cl-lib:int-add i 1))))))
        (setf (f2cl-lib:fref elco (1 nq) ((1 13) (1 12))) (* pint rq1fac))
        (setf (f2cl-lib:fref elco (2 nq) ((1 13) (1 12))) 1.0)
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i nq) nil)
          (tagbody
           label130
            (setf (f2cl-lib:fref elco
                                 ((f2cl-lib:int-add i 1) nq)
                                 ((1 13) (1 12)))
                    (/ (* rq1fac (f2cl-lib:fref pc (i) ((1 12)))) i))))
        (setf agamq (* rqfac xpin))
        (setf ragq (/ 1.0 agamq))
        (setf (f2cl-lib:fref tesco (2 nq) ((1 3) (1 12))) ragq)
        (if (< nq 12)
            (setf (f2cl-lib:fref tesco (1 nqp1) ((1 3) (1 12)))
                    (/ (* ragq rqfac) nqp1)))
        (setf (f2cl-lib:fref tesco (3 nqm1) ((1 3) (1 12))) ragq)
       label140))
    (go end_label)
   label200
    (setf (f2cl-lib:fref pc (1) ((1 12))) 1.0)
    (setf rq1fac 1.0)
    (f2cl-lib:fdo (nq 1 (f2cl-lib:int-add nq 1))
                  ((> nq 5) nil)
      (tagbody
        (setf fnq (coerce (the f2cl-lib:integer4 nq) 'double-float))
        (setf nqp1 (f2cl-lib:int-add nq 1))
        (setf (f2cl-lib:fref pc (nqp1) ((1 12))) 0.0)
        (f2cl-lib:fdo (ib 1 (f2cl-lib:int-add ib 1))
                      ((> ib nq) nil)
          (tagbody
            (setf i (f2cl-lib:int-sub (f2cl-lib:int-add nq 2) ib))
           label210
            (setf (f2cl-lib:fref pc (i) ((1 12)))
                    (+ (f2cl-lib:fref pc ((f2cl-lib:int-sub i 1)) ((1 12)))
                       (* fnq (f2cl-lib:fref pc (i) ((1 12))))))))
        (setf (f2cl-lib:fref pc (1) ((1 12)))
                (* fnq (f2cl-lib:fref pc (1) ((1 12)))))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i nqp1) nil)
          (tagbody
           label220
            (setf (f2cl-lib:fref elco (i nq) ((1 13) (1 12)))
                    (/ (f2cl-lib:fref pc (i) ((1 12)))
                       (f2cl-lib:fref pc (2) ((1 12)))))))
        (setf (f2cl-lib:fref elco (2 nq) ((1 13) (1 12))) 1.0)
        (setf (f2cl-lib:fref tesco (1 nq) ((1 3) (1 12))) rq1fac)
        (setf (f2cl-lib:fref tesco (2 nq) ((1 3) (1 12)))
                (/ nqp1 (f2cl-lib:fref elco (1 nq) ((1 13) (1 12)))))
        (setf (f2cl-lib:fref tesco (3 nq) ((1 3) (1 12)))
                (/ (f2cl-lib:int-add nq 2)
                   (f2cl-lib:fref elco (1 nq) ((1 13) (1 12)))))
        (setf rq1fac (/ rq1fac fnq))
       label230))
    (go end_label)
   end_label
    (return (values nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dcfode
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil)
           :calls 'nil)))

