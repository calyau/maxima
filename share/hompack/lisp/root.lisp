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


(let ((ic 0)
      (kount 0)
      (a 0.0)
      (acbs 0.0)
      (acmb 0.0)
      (ae 0.0)
      (cmb 0.0)
      (fa 0.0)
      (fb 0.0)
      (fc 0.0)
      (fx 0.0)
      (p 0.0)
      (q 0.0)
      (re 0.0)
      (tol 0.0)
      (u 0.0))
  (declare (type (f2cl-lib:integer4) ic kount)
           (type (double-float) a acbs acmb ae cmb fa fb fc fx p q re tol u))
  (defun root (t$ ft b c relerr abserr iflag)
    (declare (type (f2cl-lib:integer4) iflag)
             (type (double-float) abserr relerr c b ft t$))
    (prog ()
      (declare)
      (if (>= iflag 0) (go label100))
      (setf iflag (abs iflag))
      (f2cl-lib:computed-goto (label200 label300 label400) iflag)
     label100
      (setf u (f2cl-lib:d1mach 4))
      (setf re (max relerr u))
      (setf ae (max abserr 0.0))
      (setf ic 0)
      (setf acbs (abs (- b c)))
      (setf a c)
      (setf t$ a)
      (setf iflag -1)
      (go end_label)
     label200
      (setf fa ft)
      (setf t$ b)
      (setf iflag -2)
      (go end_label)
     label300
      (setf fb ft)
      (setf fc fa)
      (setf kount 2)
      (setf fx (max (abs fb) (abs fc)))
     label1
      (if (>= (abs fc) (abs fb)) (go label2))
      (setf a b)
      (setf fa fb)
      (setf b c)
      (setf fb fc)
      (setf c a)
      (setf fc fa)
     label2
      (setf cmb (* 0.5f0 (- c b)))
      (setf acmb (abs cmb))
      (setf tol (+ (* re (abs b)) ae))
      (if (<= acmb tol) (go label8))
      (if (>= kount 500) (go label12))
      (setf p (* (- b a) fb))
      (setf q (- fa fb))
      (if (>= p 0.0f0) (go label3))
      (setf p (- p))
      (setf q (- q))
     label3
      (setf a b)
      (setf fa fb)
      (setf ic (f2cl-lib:int-add ic 1))
      (if (< ic 4) (go label4))
      (if (>= (* 8.0f0 acmb) acbs) (go label6))
      (setf ic 0)
      (setf acbs acmb)
     label4
      (if (> p (* (abs q) tol)) (go label5))
      (setf b (+ b (f2cl-lib:sign tol cmb)))
      (go label7)
     label5
      (if (>= p (* cmb q)) (go label6))
      (setf b (+ b (/ p q)))
      (go label7)
     label6
      (setf b (* 0.5f0 (+ c b)))
     label7
      (setf t$ b)
      (setf iflag -3)
      (go end_label)
     label400
      (setf fb ft)
      (if (= fb 0.0f0) (go label9))
      (setf kount (f2cl-lib:int-add kount 1))
      (if (/= (f2cl-lib:sign 1.0 fb) (f2cl-lib:sign 1.0 fc)) (go label1))
      (setf c a)
      (setf fc fa)
      (go label1)
     label8
      (if (= (f2cl-lib:sign 1.0 fb) (f2cl-lib:sign 1.0 fc)) (go label11))
      (if (> (abs fb) fx) (go label10))
      (setf iflag 1)
      (go end_label)
     label9
      (setf iflag 2)
      (go end_label)
     label10
      (setf iflag 3)
      (go end_label)
     label11
      (setf iflag 4)
      (go end_label)
     label12
      (setf iflag 5)
      (go end_label)
     end_label
      (return (values t$ nil b c nil nil iflag)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::root fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(fortran-to-lisp::t$ nil fortran-to-lisp::b
                            fortran-to-lisp::c nil nil fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::d1mach))))

