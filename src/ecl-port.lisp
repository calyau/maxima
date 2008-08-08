(in-package :maxima)

(eval-when (:execute)
  (compile 'maxima::make-unspecial
	   '(lambda (s)
	     (when (symbolp s)
	       (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
	       (ffi::c-inline (s) (:object) :object
		     "(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
	       s))))

(eval-when (:load-toplevel)
  (defun maxima::make-unspecial (s)
    (when (symbolp s)
      (format t "~%;;; Declaring ~A as NOT SPECIAL" s)
      (ffi::c-inline (s) (:object) :object
		     "(#0)->symbol.stype &= ~stp_special;"
		     :one-liner nil)
      s)))

(si::trap-fpe 'floating-point-underflow nil)

;;;
;;; The following optimizers are here because we sometimes run maxima
;;; interpreted. They will change when some of these optimizations get
;;; in ECL's compiler -- in that case we will simply reuse its code.
;;;

(defmacro maxima::typep (object type &environment env)
  (let* ((whole (list 'cl:typep object type))
	 (fd (compiler-macro-function 'cl:typep)))
    (let* ((output (funcall fd whole env)))
      (subst 'cl:typep 'typep output))))

(defmacro coerce (&whole whole object type &environment env)
  (let* ((whole (list* 'cl:coerce (rest whole)))
	 (fd (compiler-macro-function 'cl:coerce)))
    (let* ((output (funcall fd whole env)))
      (subst 'cl:coerce 'coerce output))))