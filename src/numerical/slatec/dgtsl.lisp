;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dgtsl (n c d e b info)
  (declare (type (simple-array double-float (*)) b e d c)
   (type f2cl-lib:integer4 info n))
  (prog ((t$ 0.0) (k 0) (kb 0) (kp1 0) (nm1 0) (nm2 0) (abs$ 0.0f0))
    (declare (type single-float abs$) (type f2cl-lib:integer4 nm2 nm1 kp1 kb k)
     (type double-float t$))
    (setf info 0)
    (f2cl-lib:fset (f2cl-lib:fref c (1) ((1 *))) (f2cl-lib:fref d (1) ((1 *))))
    (setf nm1 (f2cl-lib:int-sub n 1))
    (if (< nm1 1) (go label40))
    (f2cl-lib:fset (f2cl-lib:fref d (1) ((1 *))) (f2cl-lib:fref e (1) ((1 *))))
    (f2cl-lib:fset (f2cl-lib:fref e (1) ((1 *))) 0.0)
    (f2cl-lib:fset (f2cl-lib:fref e (n) ((1 *))) 0.0)
    (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                  ((> k nm1) nil)
      (tagbody
        (setf kp1 (f2cl-lib:int-add k 1))
        (if
         (< (abs (f2cl-lib:fref c (kp1) ((1 *))))
            (abs (f2cl-lib:fref c (k) ((1 *)))))
         (go label10))
        (setf t$ (f2cl-lib:fref c (kp1) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref c (kp1) ((1 *)))
                       (f2cl-lib:fref c (k) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref c (k) ((1 *))) t$)
        (setf t$ (f2cl-lib:fref d (kp1) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref d (kp1) ((1 *)))
                       (f2cl-lib:fref d (k) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref d (k) ((1 *))) t$)
        (setf t$ (f2cl-lib:fref e (kp1) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref e (kp1) ((1 *)))
                       (f2cl-lib:fref e (k) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref e (k) ((1 *))) t$)
        (setf t$ (f2cl-lib:fref b (kp1) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref b (kp1) ((1 *)))
                       (f2cl-lib:fref b (k) ((1 *))))
        (f2cl-lib:fset (f2cl-lib:fref b (k) ((1 *))) t$)
       label10
        (if (/= (f2cl-lib:fref c (k) ((1 *))) 0.0) (go label20))
        (setf info k)
        (go label100)
       label20
        (setf t$
                (/ (- (f2cl-lib:fref c (kp1) ((1 *))))
                   (f2cl-lib:fref c (k) ((1 *)))))
        (f2cl-lib:fset (f2cl-lib:fref c (kp1) ((1 *)))
                       (+ (f2cl-lib:fref d (kp1) ((1 *)))
                          (* t$ (f2cl-lib:fref d (k) ((1 *))))))
        (f2cl-lib:fset (f2cl-lib:fref d (kp1) ((1 *)))
                       (+ (f2cl-lib:fref e (kp1) ((1 *)))
                          (* t$ (f2cl-lib:fref e (k) ((1 *))))))
        (f2cl-lib:fset (f2cl-lib:fref e (kp1) ((1 *))) 0.0)
        (f2cl-lib:fset (f2cl-lib:fref b (kp1) ((1 *)))
                       (+ (f2cl-lib:fref b (kp1) ((1 *)))
                          (* t$ (f2cl-lib:fref b (k) ((1 *))))))
       label30))
   label40
    (if (/= (f2cl-lib:fref c (n) ((1 *))) 0.0) (go label50))
    (setf info n)
    (go label90)
   label50
    (setf nm2 (f2cl-lib:int-sub n 2))
    (f2cl-lib:fset (f2cl-lib:fref b (n) ((1 *)))
                   (/ (f2cl-lib:fref b (n) ((1 *)))
                      (f2cl-lib:fref c (n) ((1 *)))))
    (if (= n 1) (go label80))
    (f2cl-lib:fset (f2cl-lib:fref b (nm1) ((1 *)))
                   (/
                    (- (f2cl-lib:fref b (nm1) ((1 *)))
                       (* (f2cl-lib:fref d (nm1) ((1 *)))
                          (f2cl-lib:fref b (n) ((1 *)))))
                    (f2cl-lib:fref c (nm1) ((1 *)))))
    (if (< nm2 1) (go label70))
    (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                  ((> kb nm2) nil)
      (tagbody
        (setf k (f2cl-lib:int-add (f2cl-lib:int-sub nm2 kb) 1))
        (f2cl-lib:fset (f2cl-lib:fref b (k) ((1 *)))
                       (/
                        (- (f2cl-lib:fref b (k) ((1 *)))
                           (* (f2cl-lib:fref d (k) ((1 *)))
                              (f2cl-lib:fref b
                                             ((f2cl-lib:int-add k 1))
                                             ((1 *))))
                           (* (f2cl-lib:fref e (k) ((1 *)))
                              (f2cl-lib:fref b
                                             ((f2cl-lib:int-add k 2))
                                             ((1 *)))))
                        (f2cl-lib:fref c (k) ((1 *)))))
       label60))
   label70
   label80
   label90
   label100
    (go end_label)
   end_label
    (return (values nil nil nil nil nil info))))

