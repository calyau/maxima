;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:11
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun xgetua (iunita n)
  (declare (type f2cl-lib:integer4 n)
           (type (simple-array f2cl-lib:integer4 (*)) iunita))
  (f2cl-lib:with-array-data (iunita-%data% iunita-%offset% iunita)
    (declare (type f2cl-lib:integer4 iunita-%offset%)
             (type (simple-array f2cl-lib:integer4 (*)) iunita-%data%)
             (ignorable iunita-%offset% iunita-%data%))
    (prog ((f2cl-lib:index 0) (i 0))
      (declare (type f2cl-lib:integer4 i f2cl-lib:index))
      (setf n (j4save 5 0 f2cl-lib:%false%))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf f2cl-lib:index (f2cl-lib:int-add i 4))
          (if (= i 1) (setf f2cl-lib:index 3))
          (f2cl-lib:fset
           (f2cl-lib:fref iunita-%data% (i) ((1 5)) iunita-%offset%)
           (j4save f2cl-lib:index 0 f2cl-lib:%false%))
         label30))
      (go end_label)
     end_label
      (return (values nil n)))))

