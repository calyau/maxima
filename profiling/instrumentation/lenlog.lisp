;;; Prints the lengths of $props and $values at the start and end of
;;; each test file to *DEBUG-IO* (lines starting with LENLOG).
(in-package :maxima)
(defvar *ll-orig* (fdefinition 'test-batch))
(setf (fdefinition 'test-batch)
      (lambda (filename &rest args)
        (format *debug-io* "~&LENLOG ~A props=~D values=~D facts-contexts=~D~%"
                (pathname-name (pathname filename)) (length (cdr $props)) (length (cdr $values)) (length (cdr $contexts)))
        (multiple-value-prog1 (apply *ll-orig* filename args)
          (format *debug-io* "~&LENLOG-END ~A props=~D values=~D~%"
                  (pathname-name (pathname filename)) (length (cdr $props)) (length (cdr $values))))))
