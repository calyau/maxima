;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dgamlm (xmin xmax)
  (declare (type double-float xmax xmin))
  (prog ((alnbig 0.0) (alnsml 0.0) (xln 0.0) (xold 0.0) (i 0))
    (declare (type f2cl-lib:integer4 i)
             (type double-float xold xln alnsml alnbig))
    (setf alnsml (f2cl-lib:flog (f2cl-lib:d1mach 1)))
    (setf xmin (- alnsml))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 10) nil)
      (tagbody
        (setf xold xmin)
        (setf xln (f2cl-lib:flog xmin))
        (setf xmin
                (+ xmin
                   (/
                    (* (- xmin)
                       (+ (- (* (+ xmin 0.5) xln) xmin 0.2258) alnsml))
                    (+ (* xmin xln) 0.5))))
        (if (< (abs (- xmin xold)) 0.005) (go label20))
       label10))
    (xermsg "SLATEC" "DGAMLM" "UNABLE TO FIND XMIN" 1 2)
   label20
    (setf xmin (- 0.01 xmin))
    (setf alnbig (f2cl-lib:flog (f2cl-lib:d1mach 2)))
    (setf xmax alnbig)
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 10) nil)
      (tagbody
        (setf xold xmax)
        (setf xln (f2cl-lib:flog xmax))
        (setf xmax
                (+ xmax
                   (/
                    (* (- xmax)
                       (- (+ (- (* (- xmax 0.5) xln) xmax) 0.9189) alnbig))
                    (- (* xmax xln) 0.5))))
        (if (< (abs (- xmax xold)) 0.005) (go label40))
       label30))
    (xermsg "SLATEC" "DGAMLM" "UNABLE TO FIND XMAX" 2 2)
   label40
    (setf xmax (- xmax 0.01))
    (setf xmin (max xmin (- 1.0 xmax)))
    (go end_label)
   end_label
    (return (values xmin xmax))))

