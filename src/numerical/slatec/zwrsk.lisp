;;; Compiled by f2cl version 2.0 beta Date: 2006/11/28 21:41:12 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zwrsk (zrr zri fnu kode n yr yi nz cwr cwi tol elim alim)
  (declare (type (simple-array double-float (*)) cwi cwr)
           (type (simple-array double-float (*)) yi yr)
           (type (f2cl-lib:integer4) nz n kode)
           (type (double-float) alim elim tol fnu zri zrr))
  (prog ((i 0) (nw 0) (act 0.0) (acw 0.0) (ascle 0.0) (cinui 0.0) (cinur 0.0)
         (csclr 0.0) (cti 0.0) (ctr 0.0) (c1i 0.0) (c1r 0.0) (c2i 0.0)
         (c2r 0.0) (pti 0.0) (ptr 0.0) (ract 0.0) (sti 0.0) (str 0.0))
    (declare (type (double-float) str sti ract ptr pti c2r c2i c1r c1i ctr cti
                                  csclr cinur cinui ascle acw act)
             (type (f2cl-lib:integer4) nw i))
    (setf nz 0)
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10)
        (zbknu zrr zri fnu kode 2 cwr cwi nw tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                       var-10))
      (setf nw var-7))
    (if (/= nw 0) (go label50))
    (zrati zrr zri fnu n yr yi tol)
    (setf cinur 1.0)
    (setf cinui 0.0)
    (if (= kode 1) (go label10))
    (setf cinur (cos zri))
    (setf cinui (sin zri))
   label10
    (setf acw
            (coerce
             (realpart
              (zabs (f2cl-lib:fref cwr (2) ((1 2)))
               (f2cl-lib:fref cwi (2) ((1 2)))))
             'double-float))
    (setf ascle (/ (* 1000.0 (f2cl-lib:d1mach 1)) tol))
    (setf csclr 1.0)
    (if (> acw ascle) (go label20))
    (setf csclr (/ 1.0 tol))
    (go label30)
   label20
    (setf ascle (/ 1.0 ascle))
    (if (< acw ascle) (go label30))
    (setf csclr tol)
   label30
    (setf c1r (* (f2cl-lib:fref cwr (1) ((1 2))) csclr))
    (setf c1i (* (f2cl-lib:fref cwi (1) ((1 2))) csclr))
    (setf c2r (* (f2cl-lib:fref cwr (2) ((1 2))) csclr))
    (setf c2i (* (f2cl-lib:fref cwi (2) ((1 2))) csclr))
    (setf str (f2cl-lib:fref yr (1) ((1 n))))
    (setf sti (f2cl-lib:fref yi (1) ((1 n))))
    (setf ptr (- (* str c1r) (* sti c1i)))
    (setf pti (+ (* str c1i) (* sti c1r)))
    (setf ptr (+ ptr c2r))
    (setf pti (+ pti c2i))
    (setf ctr (- (* zrr ptr) (* zri pti)))
    (setf cti (+ (* zrr pti) (* zri ptr)))
    (setf act (coerce (realpart (zabs ctr cti)) 'double-float))
    (setf ract (/ 1.0 act))
    (setf ctr (* ctr ract))
    (setf cti (* (- cti) ract))
    (setf ptr (* cinur ract))
    (setf pti (* cinui ract))
    (setf cinur (- (* ptr ctr) (* pti cti)))
    (setf cinui (+ (* ptr cti) (* pti ctr)))
    (setf (f2cl-lib:fref yr (1) ((1 n))) (* cinur csclr))
    (setf (f2cl-lib:fref yi (1) ((1 n))) (* cinui csclr))
    (if (= n 1) (go end_label))
    (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
        (setf ptr (- (* str cinur) (* sti cinui)))
        (setf cinui (+ (* str cinui) (* sti cinur)))
        (setf cinur ptr)
        (setf str (f2cl-lib:fref yr (i) ((1 n))))
        (setf sti (f2cl-lib:fref yi (i) ((1 n))))
        (setf (f2cl-lib:fref yr (i) ((1 n))) (* cinur csclr))
        (setf (f2cl-lib:fref yi (i) ((1 n))) (* cinui csclr))
       label40))
    (go end_label)
   label50
    (setf nz -1)
    (if (= nw -2) (setf nz -2))
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil nz nil nil nil nil nil))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(:and) '(:or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zwrsk fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (2))
                        (simple-array double-float (2)) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz nil
                            nil nil nil nil)
           :calls '(fortran-to-lisp::d1mach fortran-to-lisp::zabs
                    fortran-to-lisp::zrati fortran-to-lisp::zbknu))))

