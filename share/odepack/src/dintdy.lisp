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


(defun dintdy (t$ k yh nyh dky iflag)
  (declare (type (array double-float (*)) dky yh)
           (type (f2cl-lib:integer4) iflag nyh k)
           (type (double-float) t$))
  (let ()
    (symbol-macrolet ((h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (hu (aref (dls001-part-0 *dls001-common-block*) 214))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (uround (aref (dls001-part-0 *dls001-common-block*) 217))
                      (l (aref (dls001-part-1 *dls001-common-block*) 18))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nq (aref (dls001-part-1 *dls001-common-block*) 32)))
      (prog ((jp1 0) (jj1 0) (jj 0) (jb2 0) (jb 0) (j 0) (ic 0) (i 0) (tp 0.0)
             (s 0.0) (r 0.0) (c 0.0)
             (msg
              (make-array '(80)
                          :element-type 'character
                          :initial-element #\ )))
        (declare (type (string 80) msg)
                 (type (double-float) c r s tp)
                 (type (f2cl-lib:integer4) i ic j jb jb2 jj jj1 jp1))
        (setf iflag 0)
        (if (or (< k 0) (> k nq)) (go label80))
        (setf tp (+ (- tn hu) (* -100.0 uround (+ tn hu))))
        (if (> (* (- t$ tp) (- t$ tn)) 0.0) (go label90))
        (setf s (/ (- t$ tn) h))
        (setf ic 1)
        (if (= k 0) (go label15))
        (setf jj1 (f2cl-lib:int-sub l k))
        (f2cl-lib:fdo (jj jj1 (f2cl-lib:int-add jj 1))
                      ((> jj nq) nil)
          (tagbody label10 (setf ic (f2cl-lib:int-mul ic jj))))
       label15
        (setf c (coerce (the f2cl-lib:integer4 ic) 'double-float))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
           label20
            (setf (f2cl-lib:fref dky (i) ((1 *)))
                    (* c (f2cl-lib:fref yh (i l) ((1 nyh) (1 *)))))))
        (if (= k nq) (go label55))
        (setf jb2 (f2cl-lib:int-sub nq k))
        (f2cl-lib:fdo (jb 1 (f2cl-lib:int-add jb 1))
                      ((> jb jb2) nil)
          (tagbody
            (setf j (f2cl-lib:int-sub nq jb))
            (setf jp1 (f2cl-lib:int-add j 1))
            (setf ic 1)
            (if (= k 0) (go label35))
            (setf jj1 (f2cl-lib:int-sub jp1 k))
            (f2cl-lib:fdo (jj jj1 (f2cl-lib:int-add jj 1))
                          ((> jj j) nil)
              (tagbody label30 (setf ic (f2cl-lib:int-mul ic jj))))
           label35
            (setf c (coerce (the f2cl-lib:integer4 ic) 'double-float))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label40
                (setf (f2cl-lib:fref dky (i) ((1 *)))
                        (+ (* c (f2cl-lib:fref yh (i jp1) ((1 nyh) (1 *))))
                           (* s (f2cl-lib:fref dky (i) ((1 *))))))))
           label50))
        (if (= k 0) (go end_label))
       label55
        (setf r (expt h (f2cl-lib:int-sub k)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
           label60
            (setf (f2cl-lib:fref dky (i) ((1 *)))
                    (* r (f2cl-lib:fref dky (i) ((1 *)))))))
        (go end_label)
       label80
        (f2cl-lib:f2cl-set-string msg
                                  "DINTDY-  K (=I1) illegal      "
                                  (string 80))
        (xerrwd msg 30 51 0 1 k 0 0 0.0 0.0)
        (setf iflag -1)
        (go end_label)
       label90
        (f2cl-lib:f2cl-set-string msg
                                  "DINTDY-  T (=R1) illegal      "
                                  (string 80))
        (xerrwd msg 30 52 0 0 0 0 1 t$ 0.0)
        (f2cl-lib:f2cl-set-string msg
                                  "      T not in interval TCUR - HU (= R1) to TCUR (=R2)      "
                                  (string 80))
        (xerrwd msg 60 52 0 0 0 0 2 tp tn)
        (setf iflag -2)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil iflag))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dintdy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::xerrwd))))

