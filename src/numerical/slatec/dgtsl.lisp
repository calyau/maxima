;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dgtsl (n c d e b info)
  (declare (type (array double-float (*)) b e d c)
   (type f2cl-lib:integer4 info n))
  (f2cl-lib:with-multi-array-data
      ((c double-float c-%data% c-%offset%)
       (d double-float d-%data% d-%offset%)
       (e double-float e-%data% e-%offset%)
       (b double-float b-%data% b-%offset%))
    (prog ((t$ 0.0) (k 0) (kb 0) (kp1 0) (nm1 0) (nm2 0) (abs$ 0.0f0))
      (declare (type single-float abs$)
       (type f2cl-lib:integer4 nm2 nm1 kp1 kb k) (type double-float t$))
      (setf info 0)
      (f2cl-lib:fset (f2cl-lib:fref c-%data% (1) ((1 *)) c-%offset%)
                     (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (if (< nm1 1) (go label40))
      (f2cl-lib:fset (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                     (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%))
      (f2cl-lib:fset (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%) 0.0)
      (f2cl-lib:fset (f2cl-lib:fref e-%data% (n) ((1 *)) e-%offset%) 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf kp1 (f2cl-lib:int-add k 1))
          (if
           (< (abs (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
              (abs (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
           (go label10))
          (setf t$ (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%)
                         (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%) t$)
          (setf t$ (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                         (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%) t$)
          (setf t$ (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%)
                         (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%) t$)
          (setf t$ (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                         (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%))
          (f2cl-lib:fset (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%) t$)
         label10
          (if (/= (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%) 0.0)
              (go label20))
          (setf info k)
          (go label100)
         label20
          (setf t$
                  (/ (- (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
                     (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
          (f2cl-lib:fset (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%)
                         (+ (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                            (* t$
                               (f2cl-lib:fref d-%data%
                                              (k)
                                              ((1 *))
                                              d-%offset%))))
          (f2cl-lib:fset (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                         (+ (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%)
                            (* t$
                               (f2cl-lib:fref e-%data%
                                              (k)
                                              ((1 *))
                                              e-%offset%))))
          (f2cl-lib:fset (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%) 0.0)
          (f2cl-lib:fset (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                         (+ (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                            (* t$
                               (f2cl-lib:fref b-%data%
                                              (k)
                                              ((1 *))
                                              b-%offset%))))
         label30))
     label40
      (if (/= (f2cl-lib:fref c-%data% (n) ((1 *)) c-%offset%) 0.0)
          (go label50))
      (setf info n)
      (go label90)
     label50
      (setf nm2 (f2cl-lib:int-sub n 2))
      (f2cl-lib:fset (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)
                     (/ (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)
                        (f2cl-lib:fref c-%data% (n) ((1 *)) c-%offset%)))
      (if (= n 1) (go label80))
      (f2cl-lib:fset (f2cl-lib:fref b-%data% (nm1) ((1 *)) b-%offset%)
                     (/
                      (- (f2cl-lib:fref b-%data% (nm1) ((1 *)) b-%offset%)
                         (* (f2cl-lib:fref d-%data% (nm1) ((1 *)) d-%offset%)
                            (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)))
                      (f2cl-lib:fref c-%data% (nm1) ((1 *)) c-%offset%)))
      (if (< nm2 1) (go label70))
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb nm2) nil)
        (tagbody
          (setf k (f2cl-lib:int-add (f2cl-lib:int-sub nm2 kb) 1))
          (f2cl-lib:fset (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%)
                         (/
                          (- (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%)
                             (* (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%)
                                (f2cl-lib:fref b-%data%
                                               ((f2cl-lib:int-add k 1))
                                               ((1 *))
                                               b-%offset%))
                             (* (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%)
                                (f2cl-lib:fref b-%data%
                                               ((f2cl-lib:int-add k 2))
                                               ((1 *))
                                               b-%offset%)))
                          (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
         label60))
     label70
     label80
     label90
     label100
      (go end_label)
     end_label
      (return (values nil nil nil nil nil info)))))

