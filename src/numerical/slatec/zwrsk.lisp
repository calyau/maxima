;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:47
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun zwrsk (zrr zri fnu kode n yr yi nz cwr cwi tol elim alim)
  (declare (type (simple-array double-float (*)) yr yi)
           (type f2cl-lib:integer4 kode n nz)
           (type (simple-array double-float (*)) cwr cwi)
           (type double-float zrr zri fnu tol elim alim))
  (f2cl-lib:with-array-data (cwi-%data% cwi-%offset% cwi)
    (declare (type f2cl-lib:integer4 cwi-%offset%)
             (type (simple-array double-float (*)) cwi-%data%)
             (ignorable cwi-%offset% cwi-%data%))
    (f2cl-lib:with-array-data (cwr-%data% cwr-%offset% cwr)
      (declare (type f2cl-lib:integer4 cwr-%offset%)
               (type (simple-array double-float (*)) cwr-%data%)
               (ignorable cwr-%offset% cwr-%data%))
      (f2cl-lib:with-array-data (yi-%data% yi-%offset% yi)
        (declare (type f2cl-lib:integer4 yi-%offset%)
                 (type (simple-array double-float (*)) yi-%data%)
                 (ignorable yi-%offset% yi-%data%))
        (f2cl-lib:with-array-data (yr-%data% yr-%offset% yr)
          (declare (type f2cl-lib:integer4 yr-%offset%)
                   (type (simple-array double-float (*)) yr-%data%)
                   (ignorable yr-%offset% yr-%data%))
          (prog ((i 0) (nw 0) (act 0.0) (acw 0.0) (ascle 0.0) (cinui 0.0)
                 (cinur 0.0) (csclr 0.0) (cti 0.0) (ctr 0.0) (c1i 0.0)
                 (c1r 0.0) (c2i 0.0) (c2r 0.0) (pti 0.0) (ptr 0.0) (ract 0.0)
                 (sti 0.0) (str 0.0))
            (declare
             (type double-float str sti ract ptr pti c2r c2i c1r c1i ctr cti
              csclr cinur cinui ascle acw act)
             (type f2cl-lib:integer4 nw i))
            (setf nz 0)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
                (zbknu zrr zri fnu kode 2 cwr cwi nw tol elim alim)
              (declare (ignore var-4 var-5 var-6))
              (when var-0 (setf zrr var-0))
              (when var-1 (setf zri var-1))
              (when var-2 (setf fnu var-2))
              (when var-3 (setf kode var-3))
              (when var-7 (setf nw var-7))
              (when var-8 (setf tol var-8))
              (when var-9 (setf elim var-9))
              (when var-10 (setf alim var-10)))
            (if (/= nw 0) (go label50))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                (zrati zrr zri fnu n yr yi tol)
              (declare (ignore var-4 var-5))
              (when var-0 (setf zrr var-0))
              (when var-1 (setf zri var-1))
              (when var-2 (setf fnu var-2))
              (when var-3 (setf n var-3))
              (when var-6 (setf tol var-6)))
            (setf cinur 1.0)
            (setf cinui 0.0)
            (if (= kode 1) (go label10))
            (setf cinur (cos zri))
            (setf cinui (sin zri))
           label10
            (setf acw
                    (multiple-value-bind
                        (ret-val var-0 var-1)
                        (zabs
                         (f2cl-lib:fref cwr-%data% (2) ((1 2)) cwr-%offset%)
                         (f2cl-lib:fref cwi-%data% (2) ((1 2)) cwi-%offset%))
                      (declare (ignore))
                      (when var-0
                        (f2cl-lib:fset
                         (f2cl-lib:fref cwr-%data% (2) ((1 2)) cwr-%offset%)
                         var-0))
                      (when var-1
                        (f2cl-lib:fset
                         (f2cl-lib:fref cwi-%data% (2) ((1 2)) cwi-%offset%)
                         var-1))
                      ret-val))
            (setf ascle (/ (* 1000.0 (f2cl-lib:d1mach 1)) tol))
            (setf csclr 1.0)
            (if (> acw ascle) (go label20))
            (setf csclr (/ 1.0 tol))
            (go label30)
           label20
            (setf ascle (/ 1.0 ascle))
            (if (< acw ascle) (go label30))
            (setf csclr tol)
           label30
            (setf c1r
                    (* (f2cl-lib:fref cwr-%data% (1) ((1 2)) cwr-%offset%)
                       csclr))
            (setf c1i
                    (* (f2cl-lib:fref cwi-%data% (1) ((1 2)) cwi-%offset%)
                       csclr))
            (setf c2r
                    (* (f2cl-lib:fref cwr-%data% (2) ((1 2)) cwr-%offset%)
                       csclr))
            (setf c2i
                    (* (f2cl-lib:fref cwi-%data% (2) ((1 2)) cwi-%offset%)
                       csclr))
            (setf str (f2cl-lib:fref yr-%data% (1) ((1 n)) yr-%offset%))
            (setf sti (f2cl-lib:fref yi-%data% (1) ((1 n)) yi-%offset%))
            (setf ptr (- (* str c1r) (* sti c1i)))
            (setf pti (+ (* str c1i) (* sti c1r)))
            (setf ptr (+ ptr c2r))
            (setf pti (+ pti c2i))
            (setf ctr (- (* zrr ptr) (* zri pti)))
            (setf cti (+ (* zrr pti) (* zri ptr)))
            (setf act
                    (multiple-value-bind
                        (ret-val var-0 var-1)
                        (zabs ctr cti)
                      (declare (ignore))
                      (when var-0 (setf ctr var-0))
                      (when var-1 (setf cti var-1))
                      ret-val))
            (setf ract (/ 1.0 act))
            (setf ctr (* ctr ract))
            (setf cti (* (- cti) ract))
            (setf ptr (* cinur ract))
            (setf pti (* cinui ract))
            (setf cinur (- (* ptr ctr) (* pti cti)))
            (setf cinui (+ (* ptr cti) (* pti ctr)))
            (f2cl-lib:fset (f2cl-lib:fref yr-%data% (1) ((1 n)) yr-%offset%)
                           (* cinur csclr))
            (f2cl-lib:fset (f2cl-lib:fref yi-%data% (1) ((1 n)) yi-%offset%)
                           (* cinui csclr))
            (if (= n 1) (go end_label))
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf ptr (- (* str cinur) (* sti cinui)))
                (setf cinui (+ (* str cinui) (* sti cinur)))
                (setf cinur ptr)
                (setf str (f2cl-lib:fref yr-%data% (i) ((1 n)) yr-%offset%))
                (setf sti (f2cl-lib:fref yi-%data% (i) ((1 n)) yi-%offset%))
                (f2cl-lib:fset
                 (f2cl-lib:fref yr-%data% (i) ((1 n)) yr-%offset%)
                 (* cinur csclr))
                (f2cl-lib:fset
                 (f2cl-lib:fref yi-%data% (i) ((1 n)) yi-%offset%)
                 (* cinui csclr))
               label40))
            (go end_label)
           label50
            (setf nz -1)
            (if (= nw -2) (setf nz -2))
            (go end_label)
           end_label
            (return
             (values zrr zri fnu kode n nil nil nz nil nil tol elim alim))))))))

