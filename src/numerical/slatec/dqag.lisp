;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dqag
       (f a b epsabs epsrel key result abserr neval ier limit lenw last$ iwork
        work)
  (declare (type (array double-float (*)) work)
   (type (array f2cl-lib:integer4 (*)) iwork)
   (type f2cl-lib:integer4 last$ lenw limit ier neval key)
   (type double-float abserr result epsrel epsabs b a))
  (prog ((lvl 0) (l1 0) (l2 0) (l3 0))
    (declare (type f2cl-lib:integer4 l3 l2 l1 lvl))
    (setf ier 6)
    (setf neval 0)
    (setf last$ 0)
    (setf result 0.0)
    (setf abserr 0.0)
    (cond
     ((and (>= limit 1) (>= lenw (f2cl-lib:int-mul limit 4)))
      (setf l1 (f2cl-lib:int-add limit 1))
      (setf l2 (f2cl-lib:int-add limit l1))
      (setf l3 (f2cl-lib:int-add limit l2))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16)
          (dqage f a b epsabs epsrel key limit result abserr neval ier
           (f2cl-lib:array-slice work double-float (1) ((1 *)))
           (f2cl-lib:array-slice work double-float (l1) ((1 *)))
           (f2cl-lib:array-slice work double-float (l2) ((1 *)))
           (f2cl-lib:array-slice work double-float (l3) ((1 *))) iwork last$)
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-11 var-12 var-13
          var-14 var-15))
        (setf result var-7)
        (setf abserr var-8)
        (setf neval var-9)
        (setf ier var-10)
        (setf last$ var-16))
      (setf lvl 0)))
    (if (= ier 6) (setf lvl 1))
    (if (/= ier 0)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4)
            (xermsg "SLATEC" "DQAG" "ABNORMAL RETURN" ier lvl)
          (declare (ignore var-0 var-1 var-2))
          (when var-3 (setf ier var-3))
          (when var-4 (setf lvl var-4))))
    (go end_label)
   end_label
    (return
     (values nil
             nil
             nil
             nil
             nil
             nil
             result
             abserr
             neval
             ier
             nil
             nil
             last$
             nil
             nil))))

