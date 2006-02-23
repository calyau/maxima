;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defstruct (lb3 (:predicate is-lb3-p))
  (mp 0 :type f2cl-lib:integer4)
  (lp 0 :type f2cl-lib:integer4)
  (gtol 0.0d0 :type double-float)
  (stpmin 0.0d0 :type double-float)
  (stpmax 0.0d0 :type double-float))


(let* ()
  (defparameter *lb3-common-block* (make-lb3)))



(let ((finish nil)
      (mp 0)
      (lp 0)
      (iter 0)
      (nfun 0)
      (point 0)
      (ispt 0)
      (iypt 0)
      (maxfev 0)
      (info 0)
      (bound 0)
      (npt 0)
      (cp 0)
      (i 0)
      (nfev 0)
      (inmc 0)
      (iycn 0)
      (iscn 0)
      (gtol 0.0d0)
      (one 1.0d0)
      (zero 0.0d0)
      (gnorm 0.0d0)
      (stp1 0.0d0)
      (ftol 0.0d0)
      (stpmin 0.0d0)
      (stpmax 0.0d0)
      (stp 0.0d0)
      (ys 0.0d0)
      (yy 0.0d0)
      (sq 0.0d0)
      (yr 0.0d0)
      (beta 0.0d0)
      (xnorm 0.0d0))
  (declare
   (type double-float
         xnorm
         beta
         yr
         sq
         yy
         ys
         stp
         stpmax
         stpmin
         ftol
         stp1
         gnorm
         zero
         one
         gtol)
   (type f2cl-lib:integer4
         iscn
         iycn
         inmc
         nfev
         i
         cp
         npt
         bound
         info
         maxfev
         iypt
         ispt
         point
         nfun
         iter
         lp
         mp)
   (type f2cl-lib:logical finish))
  (defun lbfgs (n m x f g diagco diag iprint eps xtol w iflag scache)
    (declare (type (array f2cl-lib:integer4 (*)) iprint)
             (type f2cl-lib:logical diagco)
             (type double-float xtol eps f)
             (type (array double-float (*)) scache w diag g x)
             (type f2cl-lib:integer4 iflag m n))
    (let ()
      (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                        (stpmin (lb3-stpmin *lb3-common-block*))
                        (gtol (lb3-gtol *lb3-common-block*))
                        (lp (lb3-lp *lb3-common-block*))
                        (mp (lb3-mp *lb3-common-block*)))
        (f2cl-lib:with-multi-array-data
            ((x double-float x-%data% x-%offset%)
             (g double-float g-%data% g-%offset%)
             (diag double-float diag-%data% diag-%offset%)
             (w double-float w-%data% w-%offset%)
             (scache double-float scache-%data% scache-%offset%)
             (iprint f2cl-lib:integer4 iprint-%data% iprint-%offset%))
          (prog ()
            (declare)
            (if (= iflag 0) (go label10))
            (f2cl-lib:computed-goto (label172 label100) iflag)
           label10
            (setf iter 0)
            (if (or (<= n 0) (<= m 0)) (go label196))
            (cond
             ((<= gtol 1.d-4)
              (if (> lp 0)
                  (f2cl-lib:fformat lp
                                    ("~%"
                                     "  GTOL IS LESS THAN OR EQUAL TO 1.D-04"
                                     "~%" " IT HAS BEEN RESET TO 9.D-01" "~%")
                                    nil))
              (setf gtol 0.9d0)))
            (setf nfun 1)
            (setf point 0)
            (setf finish f2cl-lib:%false%)
            (cond
             (diagco
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label30
                  (if
                   (<= (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                       zero)
                   (go label195)))))
             (t
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label40
                  (f2cl-lib:fset
                   (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                   1.0d0)))))
            (setf ispt (f2cl-lib:int-add n (f2cl-lib:int-mul 2 m)))
            (setf iypt (f2cl-lib:int-add ispt (f2cl-lib:int-mul n m)))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label50
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                ((f2cl-lib:int-add ispt i))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (* (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%))
                    (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)))))
            (setf gnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n g 1 g 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0 (setf n var-0))
                       ret-val)))
            (setf stp1 (/ one gnorm))
            (setf ftol 1.d-4)
            (setf maxfev 20)
            (if
             (>= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
             (multiple-value-bind
                 (var-0 var-1
                        var-2
                        var-3
                        var-4
                        var-5
                        var-6
                        var-7
                        var-8
                        var-9
                        var-10)
                 (lb1 iprint iter nfun gnorm n m x f g stp finish)
               (declare (ignore var-0 var-6 var-8))
               (when var-1 (setf iter var-1))
               (when var-2 (setf nfun var-2))
               (when var-3 (setf gnorm var-3))
               (when var-4 (setf n var-4))
               (when var-5 (setf m var-5))
               (when var-7 (setf f var-7))
               (when var-9 (setf stp var-9))
               (when var-10 (setf finish var-10))))
           label80
            (setf iter (f2cl-lib:int-add iter 1))
            (setf info 0)
            (setf bound (f2cl-lib:int-sub iter 1))
            (if (= iter 1) (go label165))
            (if (> iter m) (setf bound m))
            (setf ys
                    (multiple-value-bind
                        (ret-val var-0 var-1 var-2 var-3 var-4)
                        (ddot n
                              (f2cl-lib:array-slice w
                                                    double-float
                                                    ((+ iypt npt 1))
                                                    ((1
                                                      (f2cl-lib:int-add
                                                       (f2cl-lib:int-mul n
                                                                         (f2cl-lib:int-add
                                                                          (f2cl-lib:int-mul
                                                                           2
                                                                           m)
                                                                          1))
                                                       (f2cl-lib:int-mul 2
                                                                         m)))))
                              1
                              (f2cl-lib:array-slice w
                                                    double-float
                                                    ((+ ispt npt 1))
                                                    ((1
                                                      (f2cl-lib:int-add
                                                       (f2cl-lib:int-mul n
                                                                         (f2cl-lib:int-add
                                                                          (f2cl-lib:int-mul
                                                                           2
                                                                           m)
                                                                          1))
                                                       (f2cl-lib:int-mul 2
                                                                         m)))))
                              1)
                      (declare (ignore var-1 var-2 var-3 var-4))
                      (when var-0 (setf n var-0))
                      ret-val))
            (cond
             ((not diagco)
              (setf yy
                      (multiple-value-bind
                          (ret-val var-0 var-1 var-2 var-3 var-4)
                          (ddot n
                                (f2cl-lib:array-slice w
                                                      double-float
                                                      ((+ iypt npt 1))
                                                      ((1
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul n
                                                                           (f2cl-lib:int-add
                                                                            (f2cl-lib:int-mul
                                                                             2
                                                                             m)
                                                                            1))
                                                         (f2cl-lib:int-mul 2
                                                                           m)))))
                                1
                                (f2cl-lib:array-slice w
                                                      double-float
                                                      ((+ iypt npt 1))
                                                      ((1
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul n
                                                                           (f2cl-lib:int-add
                                                                            (f2cl-lib:int-mul
                                                                             2
                                                                             m)
                                                                            1))
                                                         (f2cl-lib:int-mul 2
                                                                           m)))))
                                1)
                        (declare (ignore var-1 var-2 var-3 var-4))
                        (when var-0 (setf n var-0))
                        ret-val))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label90
                  (f2cl-lib:fset
                   (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                   (/ ys yy)))))
             (t (setf iflag 2) (go end_label)))
           label100
            (cond
             (diagco
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label110
                  (if
                   (<= (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                       zero)
                   (go label195))))))
            (setf cp point)
            (if (= point 0) (setf cp m))
            (f2cl-lib:fset
             (f2cl-lib:fref w-%data%
                            ((f2cl-lib:int-add n cp))
                            ((1
                              (f2cl-lib:int-add
                               (f2cl-lib:int-mul n
                                                 (f2cl-lib:int-add
                                                  (f2cl-lib:int-mul 2 m)
                                                  1))
                               (f2cl-lib:int-mul 2 m))))
                            w-%offset%)
             (/ one ys))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label112
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                (i)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%)))))
            (setf cp point)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i bound) nil)
              (tagbody
                (setf cp (f2cl-lib:int-sub cp 1))
                (if (= cp -1) (setf cp (f2cl-lib:int-sub m 1)))
                (setf sq
                        (multiple-value-bind
                            (ret-val var-0 var-1 var-2 var-3 var-4)
                            (ddot n
                                  (f2cl-lib:array-slice w
                                                        double-float
                                                        ((+ ispt
                                                            (f2cl-lib:int-mul
                                                             cp
                                                             n)
                                                            1))
                                                        ((1
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul n
                                                                             (f2cl-lib:int-add
                                                                              (f2cl-lib:int-mul
                                                                               2
                                                                               m)
                                                                              1))
                                                           (f2cl-lib:int-mul 2
                                                                             m)))))
                                  1
                                  w
                                  1)
                          (declare (ignore var-1 var-2 var-3 var-4))
                          (when var-0 (setf n var-0))
                          ret-val))
                (setf inmc (f2cl-lib:int-add n m cp 1))
                (setf iycn (f2cl-lib:int-add iypt (f2cl-lib:int-mul cp n)))
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                (inmc)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (*
                  (f2cl-lib:fref w-%data%
                                 ((f2cl-lib:int-add n cp 1))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul n
                                                      (f2cl-lib:int-add
                                                       (f2cl-lib:int-mul 2 m)
                                                       1))
                                    (f2cl-lib:int-mul 2 m))))
                                 w-%offset%)
                  sq))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5)
                    (daxpy n
                           (-
                            (f2cl-lib:fref w-%data%
                                           (inmc)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul n
                                                                (f2cl-lib:int-add
                                                                 (f2cl-lib:int-mul
                                                                  2
                                                                  m)
                                                                 1))
                                              (f2cl-lib:int-mul 2 m))))
                                           w-%offset%))
                           (f2cl-lib:array-slice w
                                                 double-float
                                                 ((+ iycn 1))
                                                 ((1
                                                   (f2cl-lib:int-add
                                                    (f2cl-lib:int-mul n
                                                                      (f2cl-lib:int-add
                                                                       (f2cl-lib:int-mul
                                                                        2
                                                                        m)
                                                                       1))
                                                    (f2cl-lib:int-mul 2 m)))))
                           1
                           w
                           1)
                  (declare (ignore var-1 var-2 var-3 var-4 var-5))
                  (when var-0 (setf n var-0)))
               label125))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label130
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                (i)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (* (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                    (f2cl-lib:fref w-%data%
                                   (i)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul 2 m)
                                                         1))
                                      (f2cl-lib:int-mul 2 m))))
                                   w-%offset%)))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i bound) nil)
              (tagbody
                (setf yr
                        (multiple-value-bind
                            (ret-val var-0 var-1 var-2 var-3 var-4)
                            (ddot n
                                  (f2cl-lib:array-slice w
                                                        double-float
                                                        ((+ iypt
                                                            (f2cl-lib:int-mul
                                                             cp
                                                             n)
                                                            1))
                                                        ((1
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul n
                                                                             (f2cl-lib:int-add
                                                                              (f2cl-lib:int-mul
                                                                               2
                                                                               m)
                                                                              1))
                                                           (f2cl-lib:int-mul 2
                                                                             m)))))
                                  1
                                  w
                                  1)
                          (declare (ignore var-1 var-2 var-3 var-4))
                          (when var-0 (setf n var-0))
                          ret-val))
                (setf beta
                        (*
                         (f2cl-lib:fref w-%data%
                                        ((f2cl-lib:int-add n cp 1))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)
                         yr))
                (setf inmc (f2cl-lib:int-add n m cp 1))
                (setf beta
                        (-
                         (f2cl-lib:fref w-%data%
                                        (inmc)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)
                         beta))
                (setf iscn (f2cl-lib:int-add ispt (f2cl-lib:int-mul cp n)))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5)
                    (daxpy n
                           beta
                           (f2cl-lib:array-slice w
                                                 double-float
                                                 ((+ iscn 1))
                                                 ((1
                                                   (f2cl-lib:int-add
                                                    (f2cl-lib:int-mul n
                                                                      (f2cl-lib:int-add
                                                                       (f2cl-lib:int-mul
                                                                        2
                                                                        m)
                                                                       1))
                                                    (f2cl-lib:int-mul 2 m)))))
                           1
                           w
                           1)
                  (declare (ignore var-2 var-3 var-4 var-5))
                  (when var-0 (setf n var-0))
                  (when var-1 (setf beta var-1)))
                (setf cp (f2cl-lib:int-add cp 1))
                (if (= cp m) (setf cp 0))
               label145))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label160
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                ((f2cl-lib:int-add ispt
                                                   (f2cl-lib:int-mul point n)
                                                   i))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (f2cl-lib:fref w-%data%
                                (i)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%))))
           label165
            (setf nfev 0)
            (setf stp one)
            (if (= iter 1) (setf stp stp1))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label170
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                (i)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%))))
           label172
            (multiple-value-bind
                (var-0 var-1
                       var-2
                       var-3
                       var-4
                       var-5
                       var-6
                       var-7
                       var-8
                       var-9
                       var-10
                       var-11)
                (mcsrch n
                        x
                        f
                        g
                        (f2cl-lib:array-slice w
                                              double-float
                                              ((+ ispt
                                                  (f2cl-lib:int-mul point n)
                                                  1))
                                              ((1
                                                (f2cl-lib:int-add
                                                 (f2cl-lib:int-mul n
                                                                   (f2cl-lib:int-add
                                                                    (f2cl-lib:int-mul
                                                                     2
                                                                     m)
                                                                    1))
                                                 (f2cl-lib:int-mul 2 m)))))
                        stp
                        ftol
                        xtol
                        maxfev
                        info
                        nfev
                        diag)
              (declare (ignore var-1 var-3 var-4 var-11))
              (when var-0 (setf n var-0))
              (when var-2 (setf f var-2))
              (when var-5 (setf stp var-5))
              (when var-6 (setf ftol var-6))
              (when var-7 (setf xtol var-7))
              (when var-8 (setf maxfev var-8))
              (when var-9 (setf info var-9))
              (when var-10 (setf nfev var-10)))
            (cond
             ((= info (f2cl-lib:int-sub 1)) (setf iflag 1) (go end_label)))
            (if (/= info 1) (go label190))
            (setf nfun (f2cl-lib:int-add nfun nfev))
            (setf npt (f2cl-lib:int-mul point n))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                ((f2cl-lib:int-add ispt npt i))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (* stp
                    (f2cl-lib:fref w-%data%
                                   ((f2cl-lib:int-add ispt npt i))
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul 2 m)
                                                         1))
                                      (f2cl-lib:int-mul 2 m))))
                                   w-%offset%)))
               label175
                (f2cl-lib:fset
                 (f2cl-lib:fref w-%data%
                                ((f2cl-lib:int-add iypt npt i))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul n
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul 2 m)
                                                      1))
                                   (f2cl-lib:int-mul 2 m))))
                                w-%offset%)
                 (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%)
                    (f2cl-lib:fref w-%data%
                                   (i)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul 2 m)
                                                         1))
                                      (f2cl-lib:int-mul 2 m))))
                                   w-%offset%)))))
            (setf point (f2cl-lib:int-add point 1))
            (if (= point m) (setf point 0))
            (setf gnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n g 1 g 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0 (setf n var-0))
                       ret-val)))
            (setf xnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n x 1 x 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0 (setf n var-0))
                       ret-val)))
            (setf xnorm (f2cl-lib:dmax1 1.0d0 xnorm))
            (if (<= (/ gnorm xnorm) eps) (setf finish f2cl-lib:%true%))
            (if
             (>= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
             (multiple-value-bind
                 (var-0 var-1
                        var-2
                        var-3
                        var-4
                        var-5
                        var-6
                        var-7
                        var-8
                        var-9
                        var-10)
                 (lb1 iprint iter nfun gnorm n m x f g stp finish)
               (declare (ignore var-0 var-6 var-8))
               (when var-1 (setf iter var-1))
               (when var-2 (setf nfun var-2))
               (when var-3 (setf gnorm var-3))
               (when var-4 (setf n var-4))
               (when var-5 (setf m var-5))
               (when var-7 (setf f var-7))
               (when var-9 (setf stp var-9))
               (when var-10 (setf finish var-10))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref scache-%data% (i) ((1 n)) scache-%offset%)
                 (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%))
               label177))
            (cond (finish (setf iflag 0) (go end_label)))
            (go label80)
           label190
            (setf iflag -1)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -1 " "~%"
                                   " LINE SEARCH FAILED. SEE"
                                   " DOCUMENTATION OF ROUTINE MCSRCH" "~%"
                                   " ERROR RETURN" " OF LINE SEARCH: INFO= " 1
                                   (("~2D")) "~%"
                                   " POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT"
                                   "~%" " OR INCORRECT TOLERANCES" "~%")
                                  info))
            (go end_label)
           label195
            (setf iflag -2)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -2" "~%" " THE" 1 (("~5D"))
                                   "-TH DIAGONAL ELEMENT OF THE" "~%"
                                   " INVERSE HESSIAN APPROXIMATION IS NOT POSITIVE"
                                   "~%")
                                  i))
            (go end_label)
           label196
            (setf iflag -3)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -3" "~%"
                                   " IMPROPER INPUT PARAMETERS (N OR M"
                                   " ARE NOT POSITIVE)" "~%")
                                  nil))
            (go end_label)
           end_label
            (return
             (values n m nil f nil nil nil nil nil xtol nil iflag nil))))))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)



(defun lb1 (iprint iter nfun gnorm n m x f g stp finish)
  (declare (type f2cl-lib:logical finish)
           (type (array double-float (*)) g x)
           (type double-float stp f gnorm)
           (type f2cl-lib:integer4 m n nfun iter)
           (type (array f2cl-lib:integer4 (*)) iprint))
  (let ()
    (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                      (stpmin (lb3-stpmin *lb3-common-block*))
                      (gtol (lb3-gtol *lb3-common-block*))
                      (lp (lb3-lp *lb3-common-block*))
                      (mp (lb3-mp *lb3-common-block*)))
      (f2cl-lib:with-multi-array-data
          ((iprint f2cl-lib:integer4 iprint-%data% iprint-%offset%)
           (x double-float x-%data% x-%offset%)
           (g double-float g-%data% g-%offset%))
        (prog ()
          (declare)
          (cond
           ((= iter 0)
            (f2cl-lib:fformat mp
                              ("*************************************************"
                               "~%")
                              nil)
            (f2cl-lib:fformat mp
                              ("  N=" 1 (("~5D")) "   NUMBER OF CORRECTIONS=" 1
                               (("~2D")) "~%" "       INITIAL VALUES" "~%")
                              n
                              m)
            (f2cl-lib:fformat mp
                              (" F= " 1 (("~22,15,2,1,'*,,'DE")) "   GNORM= " 1
                               (("~22,15,2,1,'*,,'DE")) "~%")
                              f
                              gnorm)
            (cond
             ((>= (f2cl-lib:fref iprint (2) ((1 2))) 1)
              (f2cl-lib:fformat mp (" VECTOR X= " "~%") nil)
              (f2cl-lib:fformat mp
                                (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (ret nil
                                          (append ret
                                                  (list
                                                   (f2cl-lib:fref x-%data%
                                                                  (i)
                                                                  ((1 n))
                                                                  x-%offset%)))))
                                    ((> i n) ret)
                                  (declare (type f2cl-lib:integer4 i))))
              (f2cl-lib:fformat mp (" GRADIENT VECTOR G= " "~%") nil)
              (f2cl-lib:fformat mp
                                (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (ret nil
                                          (append ret
                                                  (list
                                                   (f2cl-lib:fref g-%data%
                                                                  (i)
                                                                  ((1 n))
                                                                  g-%offset%)))))
                                    ((> i n) ret)
                                  (declare (type f2cl-lib:integer4 i))))))
            (f2cl-lib:fformat mp
                              ("*************************************************"
                               "~%")
                              nil)
            (f2cl-lib:fformat mp
                              ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T" "GNORM"
                               "~19@T" "STEPLENGTH" "~%" "~%")
                              nil))
           (t
            (if
             (and
              (= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
              (and (/= iter 1) (not finish)))
             (go end_label))
            (cond
             ((/= (f2cl-lib:fref iprint (1) ((1 2))) 0)
              (cond
               ((or
                 (=
                  (mod (f2cl-lib:int-add iter (f2cl-lib:int-sub 1))
                       (f2cl-lib:fref iprint (1) ((1 2))))
                  0)
                 finish)
                (if
                 (and
                  (> (f2cl-lib:fref iprint-%data% (2) ((1 2)) iprint-%offset%)
                     1)
                  (> iter 1))
                 (f2cl-lib:fformat mp
                                   ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T"
                                    "GNORM" "~19@T" "STEPLENGTH" "~%" "~%")
                                   nil))
                (f2cl-lib:fformat mp
                                  (2 (1 (("~4D")) "~1@T") "~3@T" 3
                                   (1 (("~22,15,2,1,'*,,'DE")) "~2@T") "~%")
                                  iter
                                  nfun
                                  f
                                  gnorm
                                  stp))
               (t (go end_label))))
             (t
              (if
               (and
                (> (f2cl-lib:fref iprint-%data% (2) ((1 2)) iprint-%offset%) 1)
                finish)
               (f2cl-lib:fformat mp
                                 ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T"
                                  "GNORM" "~19@T" "STEPLENGTH" "~%" "~%")
                                 nil))
              (f2cl-lib:fformat mp
                                (2 (1 (("~4D")) "~1@T") "~3@T" 3
                                 (1 (("~22,15,2,1,'*,,'DE")) "~2@T") "~%")
                                iter
                                nfun
                                f
                                gnorm
                                stp)))
            (cond
             ((or (= (f2cl-lib:fref iprint (2) ((1 2))) 2)
                  (= (f2cl-lib:fref iprint (2) ((1 2))) 3))
              (cond
               (finish (f2cl-lib:fformat mp (" FINAL POINT X= " "~%") nil))
               (t (f2cl-lib:fformat mp (" VECTOR X= " "~%") nil)))
              (f2cl-lib:fformat mp
                                (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (ret nil
                                          (append ret
                                                  (list
                                                   (f2cl-lib:fref x-%data%
                                                                  (i)
                                                                  ((1 n))
                                                                  x-%offset%)))))
                                    ((> i n) ret)
                                  (declare (type f2cl-lib:integer4 i))))
              (cond
               ((= (f2cl-lib:fref iprint (2) ((1 2))) 3)
                (f2cl-lib:fformat mp (" GRADIENT VECTOR G= " "~%") nil)
                (f2cl-lib:fformat mp
                                  (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                                  (do ((i 1 (f2cl-lib:int-add i 1))
                                       (ret nil
                                            (append ret
                                                    (list
                                                     (f2cl-lib:fref g-%data%
                                                                    (i)
                                                                    ((1 n))
                                                                    g-%offset%)))))
                                      ((> i n) ret)
                                    (declare (type f2cl-lib:integer4 i))))))))
            (if finish
                (f2cl-lib:fformat mp
                                  ("~%"
                                   " THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS."
                                   "~%" " IFLAG = 0" "~%")
                                  nil))))
          (go end_label)
         end_label
          (return (values nil iter nfun gnorm n m nil f nil stp finish)))))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)



(defun /blockdata-lb2/ ()
  (let ()
    (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                      (stpmin (lb3-stpmin *lb3-common-block*))
                      (gtol (lb3-gtol *lb3-common-block*))
                      (lp (lb3-lp *lb3-common-block*))
                      (mp (lb3-mp *lb3-common-block*)))
      (setf mp 6)
      (setf lp 6)
      (setf gtol 0.9d0)
      (setf stpmin 1.d-20)
      (setf stpmax 1.d20))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defun daxpy (n da dx incx dy incy)
  (declare (type (array double-float (*)) dy dx)
           (type double-float da)
           (type f2cl-lib:integer4 incy incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%)
       (dy double-float dy-%data% dy-%offset%))
    (prog ((i 0) (ix 0) (iy 0) (m 0) (mp1 0))
      (declare (type f2cl-lib:integer4 mp1 m iy ix i))
      (if (<= n 0) (go end_label))
      (if (= da 0.0d0) (go end_label))
      (if (and (= incx 1) (= incy 1)) (go label20))
      (setf ix 1)
      (setf iy 1)
      (if (< incx 0)
          (setf ix
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incx)
                   1)))
      (if (< incy 0)
          (setf iy
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incy)
                   1)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fset (f2cl-lib:fref dy-%data% (iy) ((1 1)) dy-%offset%)
                         (+ (f2cl-lib:fref dy-%data% (iy) ((1 1)) dy-%offset%)
                            (* da
                               (f2cl-lib:fref dx-%data%
                                              (ix)
                                              ((1 1))
                                              dx-%offset%))))
          (setf ix (f2cl-lib:int-add ix incx))
          (setf iy (f2cl-lib:int-add iy incy))
         label10))
      (go end_label)
     label20
      (setf m (mod n 4))
      (if (= m 0) (go label40))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (f2cl-lib:fset (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%)
                         (+ (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%)
                            (* da
                               (f2cl-lib:fref dx-%data%
                                              (i)
                                              ((1 1))
                                              dx-%offset%))))
         label30))
      (if (< n 4) (go end_label))
     label40
      (setf mp1 (f2cl-lib:int-add m 1))
      (f2cl-lib:fdo (i mp1 (f2cl-lib:int-add i 4))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fset (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%)
                         (+ (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%)
                            (* da
                               (f2cl-lib:fref dx-%data%
                                              (i)
                                              ((1 1))
                                              dx-%offset%))))
          (f2cl-lib:fset
           (f2cl-lib:fref dy-%data%
                          ((f2cl-lib:int-add i 1))
                          ((1 1))
                          dy-%offset%)
           (+
            (f2cl-lib:fref dy-%data%
                           ((f2cl-lib:int-add i 1))
                           ((1 1))
                           dy-%offset%)
            (* da
               (f2cl-lib:fref dx-%data%
                              ((f2cl-lib:int-add i 1))
                              ((1 1))
                              dx-%offset%))))
          (f2cl-lib:fset
           (f2cl-lib:fref dy-%data%
                          ((f2cl-lib:int-add i 2))
                          ((1 1))
                          dy-%offset%)
           (+
            (f2cl-lib:fref dy-%data%
                           ((f2cl-lib:int-add i 2))
                           ((1 1))
                           dy-%offset%)
            (* da
               (f2cl-lib:fref dx-%data%
                              ((f2cl-lib:int-add i 2))
                              ((1 1))
                              dx-%offset%))))
          (f2cl-lib:fset
           (f2cl-lib:fref dy-%data%
                          ((f2cl-lib:int-add i 3))
                          ((1 1))
                          dy-%offset%)
           (+
            (f2cl-lib:fref dy-%data%
                           ((f2cl-lib:int-add i 3))
                           ((1 1))
                           dy-%offset%)
            (* da
               (f2cl-lib:fref dx-%data%
                              ((f2cl-lib:int-add i 3))
                              ((1 1))
                              dx-%offset%))))
         label50))
      (go end_label)
     end_label
      (return (values n nil nil nil nil nil)))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defun ddot (n dx incx dy incy)
  (declare (type (array double-float (*)) dy dx)
           (type f2cl-lib:integer4 incy incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%)
       (dy double-float dy-%data% dy-%offset%))
    (prog ((i 0) (ix 0) (iy 0) (m 0) (mp1 0) (dtemp 0.0d0) (ddot 0.0d0))
      (declare (type double-float ddot dtemp)
               (type f2cl-lib:integer4 mp1 m iy ix i))
      (setf ddot 0.0d0)
      (setf dtemp 0.0d0)
      (if (<= n 0) (go end_label))
      (if (and (= incx 1) (= incy 1)) (go label20))
      (setf ix 1)
      (setf iy 1)
      (if (< incx 0)
          (setf ix
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incx)
                   1)))
      (if (< incy 0)
          (setf iy
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incy)
                   1)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (* (f2cl-lib:fref dx-%data% (ix) ((1 1)) dx-%offset%)
                        (f2cl-lib:fref dy-%data% (iy) ((1 1)) dy-%offset%))))
          (setf ix (f2cl-lib:int-add ix incx))
          (setf iy (f2cl-lib:int-add iy incy))
         label10))
      (setf ddot dtemp)
      (go end_label)
     label20
      (setf m (mod n 5))
      (if (= m 0) (go label40))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (* (f2cl-lib:fref dx-%data% (i) ((1 1)) dx-%offset%)
                        (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%))))
         label30))
      (if (< n 5) (go label60))
     label40
      (setf mp1 (f2cl-lib:int-add m 1))
      (f2cl-lib:fdo (i mp1 (f2cl-lib:int-add i 5))
                    ((> i n) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (* (f2cl-lib:fref dx-%data% (i) ((1 1)) dx-%offset%)
                        (f2cl-lib:fref dy-%data% (i) ((1 1)) dy-%offset%))
                     (*
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 1))
                                     dx-%offset%)
                      (f2cl-lib:fref dy-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 1))
                                     dy-%offset%))
                     (*
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 2))
                                     ((1 1))
                                     dx-%offset%)
                      (f2cl-lib:fref dy-%data%
                                     ((f2cl-lib:int-add i 2))
                                     ((1 1))
                                     dy-%offset%))
                     (*
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 3))
                                     ((1 1))
                                     dx-%offset%)
                      (f2cl-lib:fref dy-%data%
                                     ((f2cl-lib:int-add i 3))
                                     ((1 1))
                                     dy-%offset%))
                     (*
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 4))
                                     ((1 1))
                                     dx-%offset%)
                      (f2cl-lib:fref dy-%data%
                                     ((f2cl-lib:int-add i 4))
                                     ((1 1))
                                     dy-%offset%))))
         label50))
     label60
      (setf ddot dtemp)
      (go end_label)
     end_label
      (return (values ddot n nil nil nil nil)))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)



(let ((dg 0.0d0)
      (dgm 0.0d0)
      (dginit 0.0d0)
      (dgtest 0.0d0)
      (dgx 0.0d0)
      (dgxm 0.0d0)
      (dgy 0.0d0)
      (dgym 0.0d0)
      (finit 0.0d0)
      (ftest1 0.0d0)
      (fm 0.0d0)
      (fx 0.0d0)
      (fxm 0.0d0)
      (fy 0.0d0)
      (fym 0.0d0)
      (p5 0.5d0)
      (p66 0.66d0)
      (stx 0.0d0)
      (sty 0.0d0)
      (stmin 0.0d0)
      (stmax 0.0d0)
      (width 0.0d0)
      (width1 0.0d0)
      (xtrapf 4.0d0)
      (zero 0.0d0)
      (brackt nil)
      (stage1 nil)
      (infoc 0)
      (j 0)
      (gtol 0.0d0)
      (stpmin 0.0d0)
      (stpmax 0.0d0))
  (declare (type f2cl-lib:integer4 j infoc)
           (type f2cl-lib:logical stage1 brackt)
           (type double-float
                 stpmax
                 stpmin
                 gtol
                 zero
                 xtrapf
                 width1
                 width
                 stmax
                 stmin
                 sty
                 stx
                 p66
                 p5
                 fym
                 fy
                 fxm
                 fx
                 fm
                 ftest1
                 finit
                 dgym
                 dgy
                 dgxm
                 dgx
                 dgtest
                 dginit
                 dgm
                 dg))
  (defun mcsrch (n x f g s stp ftol xtol maxfev info nfev wa)
    (declare (type double-float xtol ftol stp f)
             (type (array double-float (*)) wa s g x)
             (type f2cl-lib:integer4 nfev info maxfev n))
    (let ()
      (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                        (stpmin (lb3-stpmin *lb3-common-block*))
                        (gtol (lb3-gtol *lb3-common-block*))
                        (lp (lb3-lp *lb3-common-block*))
                        (mp (lb3-mp *lb3-common-block*)))
        (f2cl-lib:with-multi-array-data
            ((x double-float x-%data% x-%offset%)
             (g double-float g-%data% g-%offset%)
             (s double-float s-%data% s-%offset%)
             (wa double-float wa-%data% wa-%offset%))
          (prog ()
            (declare)
            (if (= info -1) (go label45))
            (setf infoc 1)
            (if
             (or (<= n 0)
                 (<= stp zero)
                 (< ftol zero)
                 (< gtol zero)
                 (< xtol zero)
                 (< stpmin zero)
                 (< stpmax stpmin)
                 (<= maxfev 0))
             (go end_label))
            (setf dginit zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf dginit
                        (+ dginit
                           (* (f2cl-lib:fref g-%data% (j) ((1 n)) g-%offset%)
                              (f2cl-lib:fref s-%data%
                                             (j)
                                             ((1 n))
                                             s-%offset%))))
               label10))
            (cond
             ((>= dginit zero)
              (f2cl-lib:fformat lp
                                ("~%"
                                 "  THE SEARCH DIRECTION IS NOT A DESCENT DIRECTION"
                                 "~%")
                                nil)
              (go end_label)))
            (setf brackt f2cl-lib:%false%)
            (setf stage1 f2cl-lib:%true%)
            (setf nfev 0)
            (setf finit f)
            (setf dgtest (* ftol dginit))
            (setf width (- stpmax stpmin))
            (setf width1 (/ width p5))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                 (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
               label20))
            (setf stx zero)
            (setf fx finit)
            (setf dgx dginit)
            (setf sty zero)
            (setf fy finit)
            (setf dgy dginit)
           label30
            (cond
             (brackt (setf stmin (min stx sty)) (setf stmax (max stx sty)))
             (t (setf stmin stx) (setf stmax (+ stp (* xtrapf (- stp stx))))))
            (setf stp (max stp stpmin))
            (setf stp (min stp stpmax))
            (if
             (or (and brackt (or (<= stp stmin) (>= stp stmax)))
                 (>= nfev (f2cl-lib:int-sub maxfev 1))
                 (= infoc 0)
                 (and brackt (<= (- stmax stmin) (* xtol stmax))))
             (setf stp stx))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                               (+
                                (f2cl-lib:fref wa-%data%
                                               (j)
                                               ((1 n))
                                               wa-%offset%)
                                (* stp
                                   (f2cl-lib:fref s-%data%
                                                  (j)
                                                  ((1 n))
                                                  s-%offset%))))
               label40))
            (setf info -1)
            (go end_label)
           label45
            (setf info 0)
            (setf nfev (f2cl-lib:int-add nfev 1))
            (setf dg zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf dg
                        (+ dg
                           (* (f2cl-lib:fref g-%data% (j) ((1 n)) g-%offset%)
                              (f2cl-lib:fref s-%data%
                                             (j)
                                             ((1 n))
                                             s-%offset%))))
               label50))
            (setf ftest1 (+ finit (* stp dgtest)))
            (if
             (or (and brackt (or (<= stp stmin) (>= stp stmax))) (= infoc 0))
             (setf info 6))
            (if (and (= stp stpmax) (<= f ftest1) (<= dg dgtest))
                (setf info 5))
            (if (and (= stp stpmin) (or (> f ftest1) (>= dg dgtest)))
                (setf info 4))
            (if (>= nfev maxfev) (setf info 3))
            (if (and brackt (<= (- stmax stmin) (* xtol stmax))) (setf info 2))
            (if (and (<= f ftest1) (<= (f2cl-lib:dabs dg) (* gtol (- dginit))))
                (setf info 1))
            (if (/= info 0) (go end_label))
            (if (and stage1 (<= f ftest1) (>= dg (* (min ftol gtol) dginit)))
                (setf stage1 f2cl-lib:%false%))
            (cond
             ((and stage1 (<= f fx) (> f ftest1))
              (setf fm (- f (* stp dgtest))) (setf fxm (- fx (* stx dgtest)))
              (setf fym (- fy (* sty dgtest))) (setf dgm (- dg dgtest))
              (setf dgxm (- dgx dgtest)) (setf dgym (- dgy dgtest))
              (multiple-value-bind
                  (var-0 var-1
                         var-2
                         var-3
                         var-4
                         var-5
                         var-6
                         var-7
                         var-8
                         var-9
                         var-10
                         var-11
                         var-12)
                  (mcstep stx
                          fxm
                          dgxm
                          sty
                          fym
                          dgym
                          stp
                          fm
                          dgm
                          brackt
                          stmin
                          stmax
                          infoc)
                (declare (ignore))
                (when var-0 (setf stx var-0))
                (when var-1 (setf fxm var-1))
                (when var-2 (setf dgxm var-2))
                (when var-3 (setf sty var-3))
                (when var-4 (setf fym var-4))
                (when var-5 (setf dgym var-5))
                (when var-6 (setf stp var-6))
                (when var-7 (setf fm var-7))
                (when var-8 (setf dgm var-8))
                (when var-9 (setf brackt var-9))
                (when var-10 (setf stmin var-10))
                (when var-11 (setf stmax var-11))
                (when var-12 (setf infoc var-12)))
              (setf fx (+ fxm (* stx dgtest))) (setf fy (+ fym (* sty dgtest)))
              (setf dgx (+ dgxm dgtest)) (setf dgy (+ dgym dgtest)))
             (t
              (multiple-value-bind
                  (var-0 var-1
                         var-2
                         var-3
                         var-4
                         var-5
                         var-6
                         var-7
                         var-8
                         var-9
                         var-10
                         var-11
                         var-12)
                  (mcstep stx
                          fx
                          dgx
                          sty
                          fy
                          dgy
                          stp
                          f
                          dg
                          brackt
                          stmin
                          stmax
                          infoc)
                (declare (ignore))
                (when var-0 (setf stx var-0))
                (when var-1 (setf fx var-1))
                (when var-2 (setf dgx var-2))
                (when var-3 (setf sty var-3))
                (when var-4 (setf fy var-4))
                (when var-5 (setf dgy var-5))
                (when var-6 (setf stp var-6))
                (when var-7 (setf f var-7))
                (when var-8 (setf dg var-8))
                (when var-9 (setf brackt var-9))
                (when var-10 (setf stmin var-10))
                (when var-11 (setf stmax var-11))
                (when var-12 (setf infoc var-12)))))
            (cond
             (brackt
              (if (>= (f2cl-lib:dabs (- sty stx)) (* p66 width1))
                  (setf stp (+ stx (* p5 (- sty stx)))))
              (setf width1 width)
              (setf width (f2cl-lib:dabs (- sty stx)))))
            (go label30)
           end_label
            (return
             (values n nil f nil nil stp ftol xtol maxfev info nfev nil))))))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defun mcstep (stx fx dx sty fy dy stp fp dp brackt stpmin stpmax info)
  (declare (type f2cl-lib:integer4 info)
           (type f2cl-lib:logical brackt)
           (type double-float stpmax stpmin dp fp stp dy fy sty dx fx stx))
  (f2cl-lib:with-multi-array-data
      nil
    (prog ((gamma 0.0d0) (p 0.0d0) (q 0.0d0) (r 0.0d0) (s 0.0d0) (sgnd 0.0d0)
           (stpc 0.0d0) (stpf 0.0d0) (stpq 0.0d0) (theta 0.0d0) (bound nil))
      (declare (type f2cl-lib:logical bound)
               (type double-float theta stpq stpf stpc sgnd s r q p gamma))
      (setf info 0)
      (if
       (or (and brackt (or (<= stp (min stx sty)) (>= stp (max stx sty))))
           (>= (* dx (- stp stx)) 0.0d0)
           (< stpmax stpmin))
       (go end_label))
      (setf sgnd (* dp (/ dx (f2cl-lib:dabs dx))))
      (cond
       ((> fp fx) (setf info 1) (setf bound f2cl-lib:%true%)
        (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
        (setf s
                (max (f2cl-lib:dabs theta)
                     (f2cl-lib:dabs dx)
                     (f2cl-lib:dabs dp)))
        (setf gamma
                (* s
                   (f2cl-lib:dsqrt
                    (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s))))))
        (if (< stp stx) (setf gamma (- gamma))) (setf p (+ (- gamma dx) theta))
        (setf q (+ (- gamma dx) gamma dp)) (setf r (/ p q))
        (setf stpc (+ stx (* r (- stp stx))))
        (setf stpq
                (+ stx
                   (* (/ (/ dx (+ (/ (- fx fp) (- stp stx)) dx)) 2)
                      (- stp stx))))
        (cond
         ((< (f2cl-lib:dabs (+ stpc (- stx))) (f2cl-lib:dabs (+ stpq (- stx))))
          (setf stpf stpc))
         (t (setf stpf (+ stpc (/ (- stpq stpc) 2)))))
        (setf brackt f2cl-lib:%true%))
       ((< sgnd 0.0d0) (setf info 2) (setf bound f2cl-lib:%false%)
        (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
        (setf s
                (max (f2cl-lib:dabs theta)
                     (f2cl-lib:dabs dx)
                     (f2cl-lib:dabs dp)))
        (setf gamma
                (* s
                   (f2cl-lib:dsqrt
                    (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s))))))
        (if (> stp stx) (setf gamma (- gamma))) (setf p (+ (- gamma dp) theta))
        (setf q (+ (- gamma dp) gamma dx)) (setf r (/ p q))
        (setf stpc (+ stp (* r (- stx stp))))
        (setf stpq (+ stp (* (/ dp (- dp dx)) (- stx stp))))
        (cond
         ((> (f2cl-lib:dabs (+ stpc (- stp))) (f2cl-lib:dabs (+ stpq (- stp))))
          (setf stpf stpc))
         (t (setf stpf stpq)))
        (setf brackt f2cl-lib:%true%))
       ((< (f2cl-lib:dabs dp) (f2cl-lib:dabs dx)) (setf info 3)
        (setf bound f2cl-lib:%true%)
        (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
        (setf s
                (max (f2cl-lib:dabs theta)
                     (f2cl-lib:dabs dx)
                     (f2cl-lib:dabs dp)))
        (setf gamma
                (* s
                   (f2cl-lib:dsqrt
                    (max 0.0d0
                         (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s)))))))
        (if (> stp stx) (setf gamma (- gamma))) (setf p (+ (- gamma dp) theta))
        (setf q (+ gamma (- dx dp) gamma)) (setf r (/ p q))
        (cond
         ((and (< r 0.0d0) (/= gamma 0.0d0))
          (setf stpc (+ stp (* r (- stx stp)))))
         ((> stp stx) (setf stpc stpmax))
         (t (setf stpc stpmin)))
        (setf stpq (+ stp (* (/ dp (- dp dx)) (- stx stp))))
        (cond
         (brackt
          (cond
           ((< (f2cl-lib:dabs (+ stp (- stpc)))
               (f2cl-lib:dabs (+ stp (- stpq))))
            (setf stpf stpc))
           (t (setf stpf stpq))))
         (t
          (cond
           ((> (f2cl-lib:dabs (+ stp (- stpc)))
               (f2cl-lib:dabs (+ stp (- stpq))))
            (setf stpf stpc))
           (t (setf stpf stpq))))))
       (t (setf info 4)
          (setf bound f2cl-lib:%false%)
          (cond
           (brackt (setf theta (+ (/ (* 3 (- fp fy)) (- sty stp)) dy dp))
                   (setf s
                           (max (f2cl-lib:dabs theta)
                                (f2cl-lib:dabs dy)
                                (f2cl-lib:dabs dp)))
                   (setf gamma
                           (* s
                              (f2cl-lib:dsqrt
                               (- (expt (/ theta s) 2)
                                  (* (/ dy s) (/ dp s))))))
                   (if (> stp sty) (setf gamma (- gamma)))
                   (setf p (+ (- gamma dp) theta))
                   (setf q (+ (- gamma dp) gamma dy))
                   (setf r (/ p q))
                   (setf stpc (+ stp (* r (- sty stp))))
                   (setf stpf stpc))
           ((> stp stx) (setf stpf stpmax))
           (t (setf stpf stpmin)))))
      (cond ((> fp fx) (setf sty stp) (setf fy fp) (setf dy dp))
            (t (cond ((< sgnd 0.0d0) (setf sty stx) (setf fy fx) (setf dy dx)))
               (setf stx stp)
               (setf fx fp)
               (setf dx dp)))
      (setf stpf (min stpmax stpf))
      (setf stpf (max stpmin stpf))
      (setf stp stpf)
      (cond
       ((and brackt bound)
        (cond ((> sty stx) (setf stp (min (+ stx (* 0.66d0 (- sty stx))) stp)))
              (t (setf stp (max (+ stx (* 0.66d0 (- sty stx))) stp))))))
      (go end_label)
     end_label
      (return (values stx fx dx sty fy dy stp nil nil brackt nil nil info)))))

