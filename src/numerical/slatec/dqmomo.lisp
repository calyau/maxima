;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dqmomo (alfa beta ri rj rg rh integr)
  (declare (type f2cl-lib:integer4 integr)
   (type (array double-float (*)) rh rg rj ri) (type double-float beta alfa))
  (prog ((i 0) (im1 0) (alfp1 0.0) (alfp2 0.0) (an 0.0) (anm1 0.0) (betp1 0.0)
         (betp2 0.0) (ralf 0.0) (rbet 0.0))
    (declare (type double-float rbet ralf betp2 betp1 anm1 an alfp2 alfp1)
     (type f2cl-lib:integer4 im1 i))
    (setf alfp1 (+ alfa 1.0))
    (setf betp1 (+ beta 1.0))
    (setf alfp2 (+ alfa 2.0))
    (setf betp2 (+ beta 2.0))
    (setf ralf (expt 2.0 alfp1))
    (setf rbet (expt 2.0 betp1))
    (f2cl-lib:fset (f2cl-lib:fref ri (1) ((1 25))) (/ ralf alfp1))
    (f2cl-lib:fset (f2cl-lib:fref rj (1) ((1 25))) (/ rbet betp1))
    (f2cl-lib:fset (f2cl-lib:fref ri (2) ((1 25)))
                   (/ (* (f2cl-lib:fref ri (1) ((1 25))) alfa) alfp2))
    (f2cl-lib:fset (f2cl-lib:fref rj (2) ((1 25)))
                   (/ (* (f2cl-lib:fref rj (1) ((1 25))) beta) betp2))
    (setf an 2.0)
    (setf anm1 1.0)
    (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                  ((> i 25) nil)
      (tagbody
        (f2cl-lib:fset (f2cl-lib:fref ri (i) ((1 25)))
                       (/
                        (-
                         (+ ralf
                            (* an
                               (- an alfp2)
                               (f2cl-lib:fref ri
                                              ((f2cl-lib:int-sub i 1))
                                              ((1 25))))))
                        (* anm1 (+ an alfp1))))
        (f2cl-lib:fset (f2cl-lib:fref rj (i) ((1 25)))
                       (/
                        (-
                         (+ rbet
                            (* an
                               (- an betp2)
                               (f2cl-lib:fref rj
                                              ((f2cl-lib:int-sub i 1))
                                              ((1 25))))))
                        (* anm1 (+ an betp1))))
        (setf anm1 an)
        (setf an (+ an 1.0))
       label20))
    (if (= integr 1) (go label70))
    (if (= integr 3) (go label40))
    (f2cl-lib:fset (f2cl-lib:fref rg (1) ((1 25)))
                   (/ (- (f2cl-lib:fref ri (1) ((1 25)))) alfp1))
    (f2cl-lib:fset (f2cl-lib:fref rg (2) ((1 25)))
                   (- (/ (- (+ ralf ralf)) (* alfp2 alfp2))
                      (f2cl-lib:fref rg (1) ((1 25)))))
    (setf an 2.0)
    (setf anm1 1.0)
    (setf im1 2)
    (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                  ((> i 25) nil)
      (tagbody
        (f2cl-lib:fset (f2cl-lib:fref rg (i) ((1 25)))
                       (/
                        (-
                         (+
                          (* an (- an alfp2) (f2cl-lib:fref rg (im1) ((1 25))))
                          (* -1 an (f2cl-lib:fref ri (im1) ((1 25))))
                          (* anm1 (f2cl-lib:fref ri (i) ((1 25))))))
                        (* anm1 (+ an alfp1))))
        (setf anm1 an)
        (setf an (+ an 1.0))
        (setf im1 i)
       label30))
    (if (= integr 2) (go label70))
   label40
    (f2cl-lib:fset (f2cl-lib:fref rh (1) ((1 25)))
                   (/ (- (f2cl-lib:fref rj (1) ((1 25)))) betp1))
    (f2cl-lib:fset (f2cl-lib:fref rh (2) ((1 25)))
                   (- (/ (- (+ rbet rbet)) (* betp2 betp2))
                      (f2cl-lib:fref rh (1) ((1 25)))))
    (setf an 2.0)
    (setf anm1 1.0)
    (setf im1 2)
    (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                  ((> i 25) nil)
      (tagbody
        (f2cl-lib:fset (f2cl-lib:fref rh (i) ((1 25)))
                       (/
                        (-
                         (+
                          (* an (- an betp2) (f2cl-lib:fref rh (im1) ((1 25))))
                          (* -1 an (f2cl-lib:fref rj (im1) ((1 25))))
                          (* anm1 (f2cl-lib:fref rj (i) ((1 25))))))
                        (* anm1 (+ an betp1))))
        (setf anm1 an)
        (setf an (+ an 1.0))
        (setf im1 i)
       label50))
    (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 2))
                  ((> i 25) nil)
      (tagbody
        (f2cl-lib:fset (f2cl-lib:fref rh (i) ((1 25)))
                       (- (f2cl-lib:fref rh (i) ((1 25)))))
       label60))
   label70
    (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 2))
                  ((> i 25) nil)
      (tagbody
        (f2cl-lib:fset (f2cl-lib:fref rj (i) ((1 25)))
                       (- (f2cl-lib:fref rj (i) ((1 25)))))
       label80))
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil))))

