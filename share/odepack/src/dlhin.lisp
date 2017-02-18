;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(let ((half 0.5d0) (hun 100.0d0) (pt1 0.1d0) (two 2.0d0))
  (declare (type (double-float) half hun pt1 two))
  (defun dlhin
         (neq n t0 y0 ydot f tout uround ewt itol atol y temp h0 niter ier)
    (declare (type (array double-float (*)) temp y atol ewt ydot y0)
             (type (double-float) h0 uround tout t0)
             (type (f2cl-lib:integer4) ier niter itol n)
             (type (array f2cl-lib:integer4 (*)) neq))
    (f2cl-lib:with-multi-array-data
        ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
         (y0 double-float y0-%data% y0-%offset%)
         (ydot double-float ydot-%data% ydot-%offset%)
         (ewt double-float ewt-%data% ewt-%offset%)
         (atol double-float atol-%data% atol-%offset%)
         (y double-float y-%data% y-%offset%)
         (temp double-float temp-%data% temp-%offset%))
      (prog ((i 0) (iter 0) (afi 0.0d0) (atoli 0.0d0) (delyi 0.0d0) (hg 0.0d0)
             (hlb 0.0d0) (hnew 0.0d0) (hrat 0.0d0) (hub 0.0d0) (t1 0.0d0)
             (tdist 0.0d0) (tround 0.0d0) (yddnrm 0.0d0))
        (declare (type (double-float) yddnrm tround tdist t1 hub hrat hnew hlb
                                      hg delyi atoli afi)
                 (type (f2cl-lib:integer4) iter i))
        (setf niter 0)
        (setf tdist (abs (- tout t0)))
        (setf tround (* uround (max (abs t0) (abs tout))))
        (if (< tdist (* two tround)) (go label100))
        (setf hlb (* hun tround))
        (setf hub (* pt1 tdist))
        (setf atoli (f2cl-lib:fref atol-%data% (1) ((1 *)) atol-%offset%))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if (or (= itol 2) (= itol 4))
                (setf atoli
                        (f2cl-lib:fref atol-%data% (i) ((1 *)) atol-%offset%)))
            (setf delyi
                    (+
                     (* pt1
                        (abs
                         (f2cl-lib:fref y0-%data% (i) ((1 *)) y0-%offset%)))
                     atoli))
            (setf afi
                    (abs
                     (f2cl-lib:fref ydot-%data% (i) ((1 *)) ydot-%offset%)))
            (if (> (* afi hub) delyi) (setf hub (/ delyi afi)))
           label10))
        (setf iter 0)
        (setf hg (f2cl-lib:fsqrt (* hlb hub)))
        (cond
          ((< hub hlb)
           (setf h0 hg)
           (go label90)))
       label50
        (setf t1 (+ t0 hg))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
           label60
            (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                    (+ (f2cl-lib:fref y0-%data% (i) ((1 *)) y0-%offset%)
                       (* hg
                          (f2cl-lib:fref ydot-%data%
                                         (i)
                                         ((1 *))
                                         ydot-%offset%))))))
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (funcall f neq t1 y temp)
          (declare (ignore var-0 var-2 var-3))
          (when var-1
            (setf t1 var-1)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
           label70
            (setf (f2cl-lib:fref temp-%data% (i) ((1 *)) temp-%offset%)
                    (/
                     (- (f2cl-lib:fref temp-%data% (i) ((1 *)) temp-%offset%)
                        (f2cl-lib:fref ydot-%data% (i) ((1 *)) ydot-%offset%))
                     hg))))
        (setf yddnrm (dvnorm n temp ewt))
        (cond
          ((> (* yddnrm hub hub) two)
           (setf hnew (f2cl-lib:fsqrt (/ two yddnrm))))
          (t
           (setf hnew (f2cl-lib:fsqrt (* hg hub)))))
        (setf iter (f2cl-lib:int-add iter 1))
        (if (>= iter 4) (go label80))
        (setf hrat (/ hnew hg))
        (if (and (> hrat half) (< hrat two)) (go label80))
        (cond
          ((and (>= iter 2) (> hnew (* two hg)))
           (setf hnew hg)
           (go label80)))
        (setf hg hnew)
        (go label50)
       label80
        (setf h0 (* hnew half))
        (if (< h0 hlb) (setf h0 hlb))
        (if (> h0 hub) (setf h0 hub))
       label90
        (setf h0 (f2cl-lib:sign h0 (- tout t0)))
        (dcopy n y0 1 y 1)
        (setf niter iter)
        (setf ier 0)
        (go end_label)
       label100
        (setf ier -1)
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
                 h0
                 niter
                 ier))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlhin fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*)) t
                        (double-float) (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::h0 fortran-to-lisp::niter
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::dvnorm))))

