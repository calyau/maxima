;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2020-04 (21D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "HOMPACK")


(defun sintrp
       (x y xout yout ypout neqn kold phi ivc iv kgi gi alpha g w xold p)
  (declare (type (array double-float (*)) g)
           (type (array double-float (*)) w alpha)
           (type (array double-float (*)) gi)
           (type (array f2cl-lib:integer4 (*)) iv)
           (type (f2cl-lib:integer4) kgi ivc kold neqn)
           (type (array double-float (*)) p phi ypout yout y)
           (type (double-float) xold xout x))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yout double-float yout-%data% yout-%offset%)
       (ypout double-float ypout-%data% ypout-%offset%)
       (phi double-float phi-%data% phi-%offset%)
       (p double-float p-%data% p-%offset%)
       (iv f2cl-lib:integer4 iv-%data% iv-%offset%)
       (gi double-float gi-%data% gi-%offset%)
       (alpha double-float alpha-%data% alpha-%offset%)
       (w double-float w-%data% w-%offset%)
       (g double-float g-%data% g-%offset%))
    (prog ((gtemp (make-array 13 :element-type 'double-float))
           (c (make-array 13 :element-type 'double-float))
           (wtemp (make-array 13 :element-type 'double-float)) (i 0) (iq 0)
           (iw 0) (j 0) (jq 0) (kp1 0) (kp2 0) (l 0) (m 0) (alp 0.0)
           (gamma 0.0) (gdi 0.0) (gdif 0.0) (h 0.0) (hi 0.0) (hmu 0.0)
           (rmu 0.0) (sigma 0.0) (temp1 0.0) (temp2 0.0) (temp3 0.0) (xi 0.0)
           (xim1 0.0) (xiq 0.0))
      (declare (type (array double-float (13)) wtemp gtemp c)
               (type (double-float) xiq xim1 xi temp3 temp2 temp1 sigma rmu hmu
                                    hi h gdif gdi gamma alp)
               (type (f2cl-lib:integer4) m l kp2 kp1 jq j iw iq i))
      (setf kp1 (f2cl-lib:int-add kold 1))
      (setf kp2 (f2cl-lib:int-add kold 2))
      (setf hi (- xout xold))
      (setf h (- x xold))
      (setf xi (/ hi h))
      (setf xim1 (- xi 1.0f0))
      (setf xiq xi)
      (f2cl-lib:fdo (iq 1 (f2cl-lib:int-add iq 1))
                    ((> iq kp1) nil)
        (tagbody
          (setf xiq (* xi xiq))
          (setf temp1
                  (coerce
                   (the f2cl-lib:integer4
                        (f2cl-lib:int-mul iq (f2cl-lib:int-add iq 1)))
                   'double-float))
         label10
          (setf (f2cl-lib:fref wtemp (iq) ((1 13))) (/ xiq temp1))))
      (if (<= kold kgi) (go label50))
      (if (> ivc 0) (go label20))
      (setf gdi (/ 1.0f0 temp1))
      (setf m 2)
      (go label30)
     label20
      (setf iw (f2cl-lib:fref iv-%data% (ivc) ((1 10)) iv-%offset%))
      (setf gdi (f2cl-lib:fref w-%data% (iw) ((1 12)) w-%offset%))
      (setf m (f2cl-lib:int-add (f2cl-lib:int-sub kold iw) 3))
     label30
      (if (> m kold) (go label60))
      (f2cl-lib:fdo (i m (f2cl-lib:int-add i 1))
                    ((> i kold) nil)
        (tagbody
         label40
          (setf gdi
                  (-
                   (f2cl-lib:fref w-%data%
                                  ((f2cl-lib:int-sub kp2 i))
                                  ((1 12))
                                  w-%offset%)
                   (* (f2cl-lib:fref alpha-%data% (i) ((1 12)) alpha-%offset%)
                      gdi)))))
      (go label60)
     label50
      (setf gdi (f2cl-lib:fref gi-%data% (kold) ((1 11)) gi-%offset%))
     label60
      (setf (f2cl-lib:fref gtemp (1) ((1 13))) xi)
      (setf (f2cl-lib:fref gtemp (2) ((1 13))) (* 0.5f0 xi xi))
      (setf (f2cl-lib:fref c (1) ((1 13))) (coerce 1.0f0 'double-float))
      (setf (f2cl-lib:fref c (2) ((1 13))) xi)
      (if (< kold 2) (go label90))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i kold) nil)
        (tagbody
          (setf alp (f2cl-lib:fref alpha-%data% (i) ((1 12)) alpha-%offset%))
          (setf gamma (+ 1.0f0 (* xim1 alp)))
          (setf l (f2cl-lib:int-sub kp2 i))
          (f2cl-lib:fdo (jq 1 (f2cl-lib:int-add jq 1))
                        ((> jq l) nil)
            (tagbody
             label70
              (setf (f2cl-lib:fref wtemp (jq) ((1 13)))
                      (- (* gamma (f2cl-lib:fref wtemp (jq) ((1 13))))
                         (* alp
                            (f2cl-lib:fref wtemp
                                           ((f2cl-lib:int-add jq 1))
                                           ((1 13))))))))
          (setf (f2cl-lib:fref gtemp ((f2cl-lib:int-add i 1)) ((1 13)))
                  (f2cl-lib:fref wtemp (1) ((1 13))))
         label80
          (setf (f2cl-lib:fref c ((f2cl-lib:int-add i 1)) ((1 13)))
                  (* gamma (f2cl-lib:fref c (i) ((1 13)))))))
     label90
      (setf sigma
              (/
               (- (f2cl-lib:fref wtemp (2) ((1 13)))
                  (* xim1 (f2cl-lib:fref wtemp (1) ((1 13)))))
               gdi))
      (setf rmu (/ (* xim1 (f2cl-lib:fref c (kp1) ((1 13)))) gdi))
      (setf hmu (/ rmu h))
      (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                    ((> l neqn) nil)
        (tagbody
          (setf (f2cl-lib:fref yout-%data% (l) ((1 neqn)) yout-%offset%)
                  (coerce 0.0f0 'double-float))
         label100
          (setf (f2cl-lib:fref ypout-%data% (l) ((1 neqn)) ypout-%offset%)
                  (coerce 0.0f0 'double-float))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j kold) nil)
        (tagbody
          (setf i (f2cl-lib:int-sub kp2 j))
          (setf gdif
                  (- (f2cl-lib:fref g-%data% (i) ((1 13)) g-%offset%)
                     (f2cl-lib:fref g-%data%
                                    ((f2cl-lib:int-sub i 1))
                                    ((1 13))
                                    g-%offset%)))
          (setf temp2
                  (- (f2cl-lib:fref gtemp (i) ((1 13)))
                     (f2cl-lib:fref gtemp ((f2cl-lib:int-sub i 1)) ((1 13)))
                     (* sigma gdif)))
          (setf temp3
                  (+
                   (- (f2cl-lib:fref c (i) ((1 13)))
                      (f2cl-lib:fref c ((f2cl-lib:int-sub i 1)) ((1 13))))
                   (* rmu gdif)))
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l neqn) nil)
            (tagbody
              (setf (f2cl-lib:fref yout-%data% (l) ((1 neqn)) yout-%offset%)
                      (+
                       (f2cl-lib:fref yout-%data% (l) ((1 neqn)) yout-%offset%)
                       (* temp2
                          (f2cl-lib:fref phi-%data%
                                         (l i)
                                         ((1 neqn) (1 16))
                                         phi-%offset%))))
             label110
              (setf (f2cl-lib:fref ypout-%data% (l) ((1 neqn)) ypout-%offset%)
                      (+
                       (f2cl-lib:fref ypout-%data%
                                      (l)
                                      ((1 neqn))
                                      ypout-%offset%)
                       (* temp3
                          (f2cl-lib:fref phi-%data%
                                         (l i)
                                         ((1 neqn) (1 16))
                                         phi-%offset%))))))
         label120))
      (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                    ((> l neqn) nil)
        (tagbody
          (setf (f2cl-lib:fref yout-%data% (l) ((1 neqn)) yout-%offset%)
                  (+
                   (* (- 1.0f0 sigma)
                      (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%))
                   (* sigma (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%))
                   (* h
                      (+
                       (f2cl-lib:fref yout-%data% (l) ((1 neqn)) yout-%offset%)
                       (*
                        (- (f2cl-lib:fref gtemp (1) ((1 13)))
                           (* sigma
                              (f2cl-lib:fref g-%data%
                                             (1)
                                             ((1 13))
                                             g-%offset%)))
                        (f2cl-lib:fref phi-%data%
                                       (l 1)
                                       ((1 neqn) (1 16))
                                       phi-%offset%))))))
         label130
          (setf (f2cl-lib:fref ypout-%data% (l) ((1 neqn)) ypout-%offset%)
                  (+
                   (* hmu
                      (- (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                         (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)))
                   (+
                    (f2cl-lib:fref ypout-%data% (l) ((1 neqn)) ypout-%offset%)
                    (*
                     (+ (f2cl-lib:fref c (1) ((1 13)))
                        (* rmu
                           (f2cl-lib:fref g-%data% (1) ((1 13)) g-%offset%)))
                     (f2cl-lib:fref phi-%data%
                                    (l 1)
                                    ((1 neqn) (1 16))
                                    phi-%offset%)))))))
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::sintrp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (array double-float (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (double-float)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil)
           :calls 'nil)))

