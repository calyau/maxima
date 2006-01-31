;;; Compiled by f2cl version 2.0 beta Date: 2006/01/31 15:11:05 
;;; Using Lisp CMU Common Lisp Snapshot 2006-01 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dqelg (n epstab result abserr res3la nres)
  (declare (type (array double-float (*)) res3la)
           (type double-float abserr result)
           (type (array double-float (*)) epstab)
           (type f2cl-lib:integer4 nres n))
  (f2cl-lib:with-multi-array-data
      ((epstab double-float epstab-%data% epstab-%offset%)
       (res3la double-float res3la-%data% res3la-%offset%))
    (prog ((i 0) (ib 0) (ib2 0) (ie 0) (indx 0) (k1 0) (k2 0) (k3 0) (limexp 0)
           (newelm 0) (num 0) (delta1 0.0) (delta2 0.0) (delta3 0.0)
           (epmach 0.0) (epsinf 0.0) (err1 0.0) (err2 0.0) (err3 0.0) (e0 0.0)
           (e1 0.0) (e1abs 0.0) (e2 0.0) (e3 0.0) (oflow 0.0) (res 0.0)
           (ss 0.0) (tol1 0.0) (tol2 0.0) (tol3 0.0) (error$ 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$)
               (type double-float error$ tol3 tol2 tol1 ss res oflow e3 e2
                                  e1abs e1 e0 err3 err2 err1 epsinf epmach
                                  delta3 delta2 delta1)
               (type f2cl-lib:integer4 num newelm limexp k3 k2 k1 indx ie ib2
                                       ib i))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf oflow (f2cl-lib:d1mach 2))
      (setf nres (f2cl-lib:int-add nres 1))
      (setf abserr oflow)
      (setf result (f2cl-lib:fref epstab-%data% (n) ((1 52)) epstab-%offset%))
      (if (< n 3) (go label100))
      (setf limexp 50)
      (f2cl-lib:fset
       (f2cl-lib:fref epstab-%data%
                      ((f2cl-lib:int-add n 2))
                      ((1 52))
                      epstab-%offset%)
       (f2cl-lib:fref epstab-%data% (n) ((1 52)) epstab-%offset%))
      (setf newelm (the f2cl-lib:integer4 (truncate (- n 1) 2)))
      (f2cl-lib:fset (f2cl-lib:fref epstab-%data% (n) ((1 52)) epstab-%offset%)
                     oflow)
      (setf num n)
      (setf k1 n)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i newelm) nil)
        (tagbody
          (setf k2 (f2cl-lib:int-sub k1 1))
          (setf k3 (f2cl-lib:int-sub k1 2))
          (setf res
                  (f2cl-lib:fref epstab-%data%
                                 ((f2cl-lib:int-add k1 2))
                                 ((1 52))
                                 epstab-%offset%))
          (setf e0 (f2cl-lib:fref epstab-%data% (k3) ((1 52)) epstab-%offset%))
          (setf e1 (f2cl-lib:fref epstab-%data% (k2) ((1 52)) epstab-%offset%))
          (setf e2 res)
          (setf e1abs (coerce (abs e1) 'double-float))
          (setf delta2 (- e2 e1))
          (setf err2 (coerce (abs delta2) 'double-float))
          (setf tol2 (* (max (abs e2) e1abs) epmach))
          (setf delta3 (- e1 e0))
          (setf err3 (coerce (abs delta3) 'double-float))
          (setf tol3 (* (max e1abs (abs e0)) epmach))
          (if (or (> err2 tol2) (> err3 tol3)) (go label10))
          (setf result res)
          (setf abserr (+ err2 err3))
          (go label100)
         label10
          (setf e3 (f2cl-lib:fref epstab-%data% (k1) ((1 52)) epstab-%offset%))
          (f2cl-lib:fset
           (f2cl-lib:fref epstab-%data% (k1) ((1 52)) epstab-%offset%)
           e1)
          (setf delta1 (- e1 e3))
          (setf err1 (coerce (abs delta1) 'double-float))
          (setf tol1 (* (max e1abs (abs e3)) epmach))
          (if (or (<= err1 tol1) (<= err2 tol2) (<= err3 tol3)) (go label20))
          (setf ss (+ (/ 1.0 delta1) (/ 1.0 delta2) (/ -1.0 delta3)))
          (setf epsinf (coerce (abs (* ss e1)) 'double-float))
          (if (> epsinf 1.e-4) (go label30))
         label20
          (setf n (f2cl-lib:int-sub (f2cl-lib:int-add i i) 1))
          (go label50)
         label30
          (setf res (+ e1 (/ 1.0 ss)))
          (f2cl-lib:fset
           (f2cl-lib:fref epstab-%data% (k1) ((1 52)) epstab-%offset%)
           res)
          (setf k1 (f2cl-lib:int-sub k1 2))
          (setf error$ (+ err2 (abs (- res e2)) err3))
          (if (> error$ abserr) (go label40))
          (setf abserr error$)
          (setf result res)
         label40))
     label50
      (if (= n limexp)
          (setf n (- (* 2 (the f2cl-lib:integer4 (truncate limexp 2))) 1)))
      (setf ib 1)
      (if (= (* (the f2cl-lib:integer4 (truncate num 2)) 2) num) (setf ib 2))
      (setf ie (f2cl-lib:int-add newelm 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i ie) nil)
        (tagbody
          (setf ib2 (f2cl-lib:int-add ib 2))
          (f2cl-lib:fset
           (f2cl-lib:fref epstab-%data% (ib) ((1 52)) epstab-%offset%)
           (f2cl-lib:fref epstab-%data% (ib2) ((1 52)) epstab-%offset%))
          (setf ib ib2)
         label60))
      (if (= num n) (go label80))
      (setf indx (f2cl-lib:int-add (f2cl-lib:int-sub num n) 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fset
           (f2cl-lib:fref epstab-%data% (i) ((1 52)) epstab-%offset%)
           (f2cl-lib:fref epstab-%data% (indx) ((1 52)) epstab-%offset%))
          (setf indx (f2cl-lib:int-add indx 1))
         label70))
     label80
      (if (>= nres 4) (go label90))
      (f2cl-lib:fset
       (f2cl-lib:fref res3la-%data% (nres) ((1 3)) res3la-%offset%)
       result)
      (setf abserr oflow)
      (go label100)
     label90
      (setf abserr
              (coerce
               (+
                (abs
                 (- result
                    (f2cl-lib:fref res3la-%data% (3) ((1 3)) res3la-%offset%)))
                (abs
                 (- result
                    (f2cl-lib:fref res3la-%data% (2) ((1 3)) res3la-%offset%)))
                (abs
                 (- result
                    (f2cl-lib:fref res3la-%data%
                                   (1)
                                   ((1 3))
                                   res3la-%offset%))))
               'double-float))
      (f2cl-lib:fset (f2cl-lib:fref res3la-%data% (1) ((1 3)) res3la-%offset%)
                     (f2cl-lib:fref res3la-%data% (2) ((1 3)) res3la-%offset%))
      (f2cl-lib:fset (f2cl-lib:fref res3la-%data% (2) ((1 3)) res3la-%offset%)
                     (f2cl-lib:fref res3la-%data% (3) ((1 3)) res3la-%offset%))
      (f2cl-lib:fset (f2cl-lib:fref res3la-%data% (3) ((1 3)) res3la-%offset%)
                     result)
     label100
      (setf abserr (max abserr (* 5.0 epmach (abs result))))
      (go end_label)
     end_label
      (return (values n nil result abserr nil nres)))))

