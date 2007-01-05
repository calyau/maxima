;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "LAPACK")


(let* ((zero 0.0) (one 1.0) (two 2.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two))
  (defun dlas2 (f g h ssmin ssmax)
    (declare (type (double-float) ssmax ssmin h g f))
    (prog ((as 0.0) (at 0.0) (au 0.0) (c 0.0) (fa 0.0) (fhmn 0.0) (fhmx 0.0)
           (ga 0.0) (ha 0.0))
      (declare (type (double-float) as at au c fa fhmn fhmx ga ha))
      (setf fa (abs f))
      (setf ga (abs g))
      (setf ha (abs h))
      (setf fhmn (min fa ha))
      (setf fhmx (max fa ha))
      (cond
        ((= fhmn zero)
         (setf ssmin zero)
         (cond
           ((= fhmx zero)
            (setf ssmax ga))
           (t
            (setf ssmax
                    (* (max fhmx ga)
                       (f2cl-lib:fsqrt
                        (+ one (expt (/ (min fhmx ga) (max fhmx ga)) 2))))))))
        (t
         (cond
           ((< ga fhmx)
            (setf as (+ one (/ fhmn fhmx)))
            (setf at (/ (- fhmx fhmn) fhmx))
            (setf au (expt (/ ga fhmx) 2))
            (setf c
                    (/ two
                       (+ (f2cl-lib:fsqrt (+ (* as as) au))
                          (f2cl-lib:fsqrt (+ (* at at) au)))))
            (setf ssmin (* fhmn c))
            (setf ssmax (/ fhmx c)))
           (t
            (setf au (/ fhmx ga))
            (cond
              ((= au zero)
               (setf ssmin (/ (* fhmn fhmx) ga))
               (setf ssmax ga))
              (t
               (setf as (+ one (/ fhmn fhmx)))
               (setf at (/ (- fhmx fhmn) fhmx))
               (setf c
                       (/ one
                          (+ (f2cl-lib:fsqrt (+ one (expt (* as au) 2)))
                             (f2cl-lib:fsqrt (+ one (expt (* at au) 2))))))
               (setf ssmin (* fhmn c au))
               (setf ssmin (+ ssmin ssmin))
               (setf ssmax (/ ga (+ c c)))))))))
      (go end_label)
     end_label
      (return (values nil nil nil ssmin ssmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlas2 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::ssmin
                            fortran-to-lisp::ssmax)
           :calls 'nil)))

