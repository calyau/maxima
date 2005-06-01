;;; Compiled by f2cl version 2.0 beta Date: 2005/06/01 15:29:41 
;;; Using Lisp CMU Common Lisp Snapshot 2005-06 (19B)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun zbunk (zr zi fnu kode mr n yr yi nz tol elim alim)
  (declare (type (simple-array double-float (*)) yi yr)
           (type f2cl-lib:integer4 nz n mr kode)
           (type double-float alim elim tol fnu zi zr))
  (prog ((ax 0.0) (ay 0.0) (abs$ 0.0f0))
    (declare (type single-float abs$) (type double-float ay ax))
    (setf nz 0)
    (setf ax (* (abs zr) 1.7321))
    (setf ay (coerce (abs zi) 'double-float))
    (if (> ay ax) (go label10))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11)
        (zunk1 zr zi fnu kode mr n yr yi nz tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                       var-10 var-11))
      (setf nz var-8))
    (go label20)
   label10
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11)
        (zunk2 zr zi fnu kode mr n yr yi nz tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                       var-10 var-11))
      (setf nz var-8))
   label20
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil nil nz nil nil nil))))

