;;; Runs the old SIGNFACTOR and prototypes/signfactor.lisp on every call
;;; and prints a DIFF line to *DEBUG-IO* when the resulting sign differs.
;;; The old result is kept, so the test results are those of the old code.
(in-package :maxima)
(defvar *sfd-orig* (fdefinition 'signfactor))
(load (merge-pathnames "../prototypes/signfactor.lisp" *load-truename*))
(defvar *sfd-new* (fdefinition 'signfactor))
(setf (fdefinition 'signfactor)
      (lambda (x)
        (let* ((s0 sign) (e0 evens) (o0 odds) (m0 minus)
               (r1 (funcall *sfd-orig* x)) (s1 sign))
          (setq sign s0 evens e0 odds o0 minus m0)
          (let* ((r2 (funcall *sfd-new* x)) (s2 sign))
            (unless (eq s1 s2)
              (format *debug-io* "~&DIFF x=~A old=~A new=~A factor=~A~%" (mstring x) s1 s2
                      (mstring (let (($ratprint nil)) (factor x)))))
            (setq sign s1)
            r1))))
