;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun xgetua (iunita n)
  (declare (type f2cl-lib:integer4 n)
           (type (simple-array f2cl-lib:integer4 (*)) iunita))
  (prog ((f2cl-lib:index 0) (i 0))
    (declare (type f2cl-lib:integer4 i f2cl-lib:index))
    (setf n (j4save 5 0 f2cl-lib:%false%))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
        (setf f2cl-lib:index (f2cl-lib:int-add i 4))
        (if (= i 1) (setf f2cl-lib:index 3))
        (f2cl-lib:fset (f2cl-lib:fref iunita (i) ((1 5)))
                       (j4save f2cl-lib:index 0 f2cl-lib:%false%))
       label30))
    (go end_label)
   end_label
    (return (values nil n))))

