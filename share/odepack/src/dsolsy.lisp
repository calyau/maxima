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


(defun dsolsy (wm iwm x tem)
  (declare (type (array f2cl-lib:integer4 (*)) iwm)
           (type (array double-float (*)) tem x wm))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (iersl (aref (dls001-part-1 *dls001-common-block*) 14))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31)))
      (prog ((mu 0) (ml 0) (meband 0) (i 0) (r 0.0) (phl0 0.0) (hl0 0.0)
             (di 0.0))
        (declare (type (double-float) di hl0 phl0 r)
                 (type (f2cl-lib:integer4) i meband ml mu))
        (setf iersl 0)
        (f2cl-lib:computed-goto (label100 label100 label300 label400 label400)
                                miter)
       label100
        (dgesl (f2cl-lib:array-slice wm double-float (3) ((1 *))) n n
         (f2cl-lib:array-slice iwm f2cl-lib:integer4 (21) ((1 *))) x 0)
        (go end_label)
       label300
        (setf phl0 (f2cl-lib:fref wm (2) ((1 *))))
        (setf hl0 (* h el0))
        (setf (f2cl-lib:fref wm (2) ((1 *))) hl0)
        (if (= hl0 phl0) (go label330))
        (setf r (/ hl0 phl0))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf di
                    (- 1.0
                       (* r
                          (+ 1.0
                             (/ -1.0
                                (f2cl-lib:fref wm
                                               ((f2cl-lib:int-add i 2))
                                               ((1 *))))))))
            (if (= (abs di) 0.0) (go label390))
           label320
            (setf (f2cl-lib:fref wm ((f2cl-lib:int-add i 2)) ((1 *)))
                    (/ 1.0 di))))
       label330
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
           label340
            (setf (f2cl-lib:fref x (i) ((1 *)))
                    (* (f2cl-lib:fref wm ((f2cl-lib:int-add i 2)) ((1 *)))
                       (f2cl-lib:fref x (i) ((1 *)))))))
        (go end_label)
       label390
        (setf iersl 1)
        (go end_label)
       label400
        (setf ml (f2cl-lib:fref iwm (1) ((1 *))))
        (setf mu (f2cl-lib:fref iwm (2) ((1 *))))
        (setf meband (f2cl-lib:int-add (f2cl-lib:int-mul 2 ml) mu 1))
        (dgbsl (f2cl-lib:array-slice wm double-float (3) ((1 *))) meband n ml
         mu (f2cl-lib:array-slice iwm f2cl-lib:integer4 (21) ((1 *))) x 0)
        (go end_label)
       end_label
        (return (values nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsolsy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::dgbsl fortran-to-lisp::dgesl))))

