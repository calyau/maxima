;;; Compiled by f2cl version 2.0 beta Date: 2007/05/04 17:29:50 
;;; Using Lisp CMU Common Lisp Snapshot 2007-05 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((nalgm 0)
      (xbig 0.0)
      (xmax 0.0)
      (algmcs
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.16663894804518634
                                       -1.384948176067564e-5
                                       9.81082564692473e-9
                                       -1.809129475572494e-11
                                       6.221098041892606e-14
                                       -3.399615005417722e-16
                                       2.683181998482699e-18
                                       -2.868042435334643e-20
                                       3.9628370610464347e-22
                                       -6.831888753985767e-24
                                       1.4292273559424982e-25
                                       -3.5475981581010704e-27
                                       1.025680058010471e-28
                                       -3.401102254316749e-30
                                       1.276642195630063e-31)))
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (15)) algmcs)
           (type (double-float) xmax xbig)
           (type (integer) nalgm))
  (setq first$ f2cl-lib:%true%)
  (defun d9lgmc (x)
    (declare (type (double-float) x))
    (prog ((d9lgmc 0.0))
      (declare (type (double-float) d9lgmc))
      (cond
        (first$
         (setf nalgm (initds algmcs 15 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf xbig (/ 1.0 (f2cl-lib:fsqrt (f2cl-lib:d1mach 3))))
         (setf xmax
                 (exp
                  (min (f2cl-lib:flog (/ (f2cl-lib:d1mach 2) 12.0))
                       (- (f2cl-lib:flog (* 12.0 (f2cl-lib:d1mach 1)))))))))
      (setf first$ f2cl-lib:%false%)
      (if (< x 10.0) (xermsg "SLATEC" "D9LGMC" "X MUST BE GE 10" 1 2))
      (if (>= x xmax) (go label20))
      (setf d9lgmc (/ 1.0 (* 12.0 x)))
      (if (< x xbig)
          (setf d9lgmc
                  (/ (dcsevl (- (* 2.0 (expt (/ 10.0 x) 2)) 1.0) algmcs nalgm)
                     x)))
      (go end_label)
     label20
      (setf d9lgmc 0.0)
      (xermsg "SLATEC" "D9LGMC" "X SO BIG D9LGMC UNDERFLOWS" 2 1)
      (go end_label)
     end_label
      (return (values d9lgmc nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::d9lgmc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

