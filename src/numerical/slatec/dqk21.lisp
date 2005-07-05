;;; Compiled by f2cl version 2.0 beta Date: 2005/06/20 01:53:39 
;;; Using Lisp CMU Common Lisp Snapshot 2005-06 (19B)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((wg
       (make-array 5
                   :element-type 'double-float
                   :initial-contents '(0.06667134430868814 0.1494513491505806
                                       0.21908636251598204 0.26926671930999635
                                       0.29552422471475287)))
      (xgk
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.9956571630258081 0.9739065285171717
                                       0.9301574913557082 0.8650633666889845
                                       0.7808177265864169 0.6794095682990244
                                       0.5627571346686047 0.4333953941292472
                                       0.2943928627014602 0.14887433898163122
                                       0.0)))
      (wgk
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.011694638867371874
                                       0.032558162307964725
                                       0.054755896574351995 0.07503967481091996
                                       0.0931254545836976 0.10938715880229764
                                       0.12349197626206584 0.13470921731147334
                                       0.14277593857706009 0.14773910490133849
                                       0.1494455540029169))))
  (declare (type (array double-float (11)) wgk xgk)
           (type (array double-float (5)) wg))
  (defun dqk21 (f a b result abserr resabs resasc)
    (declare (type double-float resasc resabs abserr result b a)
             (type (function (double-float) (values double-float &rest t)) f))
    (f2cl-lib:with-multi-array-data
        nil
      (prog ((fv1 (make-array 10 :element-type 'double-float))
             (fv2 (make-array 10 :element-type 'double-float)) (j 0) (jtw 0)
             (jtwm1 0) (absc 0.0) (centr 0.0) (dhlgth 0.0) (epmach 0.0)
             (fc 0.0) (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0)
             (resk 0.0) (reskh 0.0) (uflow 0.0) (abs$ 0.0f0))
        (declare (type single-float abs$)
                 (type (array double-float (10)) fv2 fv1)
                 (type double-float uflow reskh resk resg hlgth fval2 fval1
                                    fsum fc epmach dhlgth centr absc)
                 (type f2cl-lib:integer4 jtwm1 jtw j))
        (setf epmach (f2cl-lib:d1mach 4))
        (setf uflow (f2cl-lib:d1mach 1))
        (setf centr (* 0.5 (+ a b)))
        (setf hlgth (* 0.5 (- b a)))
        (setf dhlgth (coerce (abs hlgth) 'double-float))
        (setf resg 0.0)
        (setf fc
                (multiple-value-bind (ret-val var-0)
                    (funcall f centr)
                  (declare (ignore))
                  (when var-0
                    (setf centr var-0))
                  ret-val))
        (setf resk (* (f2cl-lib:fref wgk (11) ((1 11))) fc))
        (setf resabs (coerce (abs resk) 'double-float))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 5) nil)
          (tagbody
            (setf jtw (f2cl-lib:int-mul 2 j))
            (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 11)))))
            (setf fval1 (funcall f (- centr absc)))
            (setf fval2 (funcall f (+ centr absc)))
            (f2cl-lib:fset (f2cl-lib:fref fv1 (jtw) ((1 10))) fval1)
            (f2cl-lib:fset (f2cl-lib:fref fv2 (jtw) ((1 10))) fval2)
            (setf fsum (+ fval1 fval2))
            (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 5))) fsum)))
            (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 11))) fsum)))
            (setf resabs
                    (+ resabs
                       (* (f2cl-lib:fref wgk (jtw) ((1 11)))
                          (+ (abs fval1) (abs fval2)))))
           label10))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 5) nil)
          (tagbody
            (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul 2 j) 1))
            (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 11)))))
            (setf fval1 (funcall f (- centr absc)))
            (setf fval2 (funcall f (+ centr absc)))
            (f2cl-lib:fset (f2cl-lib:fref fv1 (jtwm1) ((1 10))) fval1)
            (f2cl-lib:fset (f2cl-lib:fref fv2 (jtwm1) ((1 10))) fval2)
            (setf fsum (+ fval1 fval2))
            (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 11))) fsum)))
            (setf resabs
                    (+ resabs
                       (* (f2cl-lib:fref wgk (jtwm1) ((1 11)))
                          (+ (abs fval1) (abs fval2)))))
           label15))
        (setf reskh (* resk 0.5))
        (setf resasc (* (f2cl-lib:fref wgk (11) ((1 11))) (abs (- fc reskh))))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 10) nil)
          (tagbody
            (setf resasc
                    (+ resasc
                       (* (f2cl-lib:fref wgk (j) ((1 11)))
                          (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 10))) reskh))
                             (abs
                              (- (f2cl-lib:fref fv2 (j) ((1 10))) reskh))))))
           label20))
        (setf result (* resk hlgth))
        (setf resabs (* resabs dhlgth))
        (setf resasc (* resasc dhlgth))
        (setf abserr (coerce (abs (* (- resk resg) hlgth)) 'double-float))
        (if (and (/= resasc 0.0) (/= abserr 0.0))
            (setf abserr
                    (* resasc
                       (min 1.0 (expt (/ (* 200.0 abserr) resasc) 1.5)))))
        (if (> resabs (/ uflow (* 50.0 epmach)))
            (setf abserr (max (* epmach 50.0 resabs) abserr)))
        (go end_label)
       end_label
        (return (values nil nil nil result abserr resabs resasc))))))

