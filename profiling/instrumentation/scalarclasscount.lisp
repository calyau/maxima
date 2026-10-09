;;; Round 2. Counts calls of SCALARCLASS, $CONSTANTP, MOPP and friends.
;;; Load after scalarclass-setup.mac, report with cntreport().
(in-package :maxima)
(defvar *cnt* (make-hash-table))
(defmacro defcount (name)
  `(let ((orig (fdefinition ',name)))
     (setf (fdefinition ',name)
           (lambda (&rest args) (incf (gethash ',name *cnt* 0)) (apply orig args)))))
(defcount scalarclass) (defcount constantp-impl) (defcount mopp) (defcount mopp1)
(defcount consttermp) (defcount nonscalarp-impl) (defcount scalarp-impl)
(defcount simpnct) (defcount simplifya)
(defun $cntreport ()
  (maphash (lambda (k v) (format t "~&~20a ~12d~%" k v)) *cnt*) (clrhash *cnt*))
