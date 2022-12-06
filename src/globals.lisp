;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

;;;; This file contains global vars (defvars/defmvars) that are used in
;;;; multiple files.  We gather them all here so that they are
;;;; consistently defined across the build and to make the dependencies
;;;; easier to track.

(in-package "MAXIMA")

(defvar *variable-initial-values* (make-hash-table)
  "Hash table containing all Maxima defmvar variables and their
  initial values")

(defmacro defmvar (var &optional (val nil valp) (doc nil docp) &rest options)
  "Define a Maxima variable VAR that is user-visible.  It is
  initialized to the value VAL.  An associated documentation string
  can be supplied in DOC.  OPTIONS contains a list of options that can
  be applied to the variable.

  The valid options are:

    NO-RESET        - If given, the variable will not be reset.
    FIXNUM, BOOLEAN - The variable is declared to have this type.
    :PROPERTIES     - A list of properties to be applied for this variable.

  The list of properties has the form ((ind1 val1) (ind2 val2) ...)
  where IND1 is the name of the property and VAL1 is the value
  associated with the property.

  Other options that are recognized but ignored: IN-CORE, SEE-ALSO,
  MODIFIED-COMMANDS, SETTING-PREDICATE, SETTING-LIST.  For any other
  options, a warning is produced.
"
  (let ((maybe-reset
          ;; Default is to reset the variable to it's initial value.
          `((unless (gethash ',var *variable-initial-values*)
              (setf (gethash ',var *variable-initial-values*)
                    ,val))))
        maybe-declare-type
        maybe-set-props)

    (do ((opts options (rest opts)))
        ((null opts))
      #+nil
      (format t "opts = ~S~%" opts)
      (case (car opts)
        (no-reset
         ;; Don't reset the value
         (setf maybe-reset nil))
        ((fixnum boolean string flonum)
         (setf maybe-declare-type
               `((declaim (type ,(car opts) ,var)))))
        (in-core
         ;; Ignore this
         )
       (:properties
         (setf maybe-set-props
               (mapcar #'(lambda (o)
                           (destructuring-bind (ind val)
                               o
                             `(putprop ',var ',val ',ind)))
                       (second opts)))
         (setf opts (rest opts)))
        ((see-also modified-commands setting-predicate setting-list)
         ;; Not yet supported, but we need to skip over the following
         ;; item too which is the parameter for this option.
         (setf opts (rest opts)))
        (t
         (warn "Ignoring unknown defmvar option for ~S: ~S"
               var (car opts)))))
    `(progn
       ,@maybe-reset
       ,@maybe-declare-type
       (defvar ,var ,val ,doc)
       ,@maybe-set-props)))

(defun putprop (sym val  indic)
  (if (consp sym)
      (setf (getf (cdr sym) indic) val)
      (setf (get sym indic) val)))

;;; Declare user-visible special variables and other global special variables.

(defvar infinities '($inf $minf $infinity)
  "The types of infinities recognized by Maxima.
   INFINITY is complex infinity")

(defvar real-infinities '($inf $minf)
  "The real infinities, `inf' is positive infinity, `minf' negative infinity")

(defvar infinitesimals '($zeroa $zerob)
  "The infinitesimals recognized by Maxima. ZEROA zero from above,
   ZEROB zero from below")

;;------------------------------------------------------------------------
;; From clmacs.lisp
;;
;; Define useful floating-point constants
#+clisp
(progn
  ;; This used to be enabled, but
  ;; http://clisp.cons.org/impnotes/num-dict.html seems to indicate
  ;; that the result of float, coerce, sqrt, etc., on a rational will
  ;; return a float of the specified type.  But ANSI CL says we must
  ;; return a single-float.  I (rtoy) am commenting this out for now.

  ;; (setq custom:*default-float-format* 'double-float)

  ;; We currently don't want any warnings about floating-point contagion.
  (setq custom::*warn-on-floating-point-contagion* nil)

  ;; We definitely want ANSI-style floating-point contagion.
  (setq custom:*floating-point-contagion-ansi* t)

  ;; Set custom:*floating-point-rational-contagion-ansi* so that
  ;; contagion is done as per the ANSI CL standard. Has an effect only
  ;; in those few cases when the mathematical result is exact although
  ;; one of the arguments is a floating-point number, such as (* 0
  ;; 1.618), (/ 0 1.618), (atan 0 1.0), (expt 2.0 0)
  (setq custom:*floating-point-rational-contagion-ansi* t)

  ;; When building maxima using with 'flonum being a 'long-float it may be
  ;; useful to adjust the number of bits of precision that CLISP uses for
  ;; long-floats.
  #+nil
  (setf (ext:long-float-digits) 128)

  ;; We want underflows not to signal errors.
  (ext:without-package-lock ()
    (setq sys::*inhibit-floating-point-underflow* t))
  )

#+abcl
(progn
  ;; We want underflows not to signal errors
  (when (fboundp (find-symbol "FLOAT-UNDERFLOW-MODE" "SYS"))
    (funcall (find-symbol "FLOAT-UNDERFLOW-MODE" "SYS") nil))
  )

;; Make the maximum exponent larger for CMUCL.  Without this, cmucl
;; will generate a continuable error when raising an integer to a
;; power greater than this.
#+cmu
(setf ext::*intexp-maximum-exponent* 100000)
;;;; Setup the mapping from the Maxima 'flonum float type to a CL float type.
;;;;
;;;; Add :flonum-long to *features* if you want flonum to be a
;;;; long-float.  Or add :flonum-double-double if you want flonum to
;;;; be a double-double (currently only for CMUCL).  Otherwise, you
;;;; get double-float as the flonum type.
;;;;
;;;; Default double-float flonum.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))

#-(or flonum-long flonum-double-double)
(progn
;; Tell Lisp the float type for a 'flonum.
#-(or clisp abcl)
(deftype flonum (&optional low high)
  (cond (high
	 `(double-float ,low ,high))
	(low
	 `(double-float ,low))
	(t
	 'double-float)))

;; Some versions of clisp and ABCL appear to be buggy: (coerce 1 'flonum)
;; signals an error.  So does (coerce 1 '(double-float 0d0)).  But
;; (coerce 1 'double-float) returns 1d0 as expected.  So for now, make
;; flonum be exactly the same as double-float, without bounds.
#+(or clisp abcl)
(deftype flonum (&optional low high)
  (declare (ignorable low high))
  'double-float)

(defconstant most-positive-flonum most-positive-double-float)
(defconstant most-negative-flonum most-negative-double-float)
(defconstant least-positive-flonum least-positive-double-float)
(defconstant least-negative-flonum least-negative-double-float)
(defconstant flonum-epsilon double-float-epsilon)
(defconstant least-positive-normalized-flonum least-positive-normalized-double-float)
(defconstant least-negative-normalized-flonum least-negative-normalized-double-float)

(defconstant flonum-exponent-marker #\D)
)

#+flonum-long
(progn
;;;; The Maxima 'flonum can be a CL 'long-float on the Scieneer CL or CLISP,
;;;; but should be the same as 'double-float on other CL implementations.

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *read-default-float-format* 'long-float))

;; Tell Lisp the float type for a 'flonum.
(deftype flonum (&optional low high)
  (cond (high
	 `(long-float ,low ,high))
	(low
	 `(long-float ,low))
	(t
	 'long-float)))

(defconstant most-positive-flonum most-positive-long-float)
(defconstant most-negative-flonum most-negative-long-float)
(defconstant least-positive-flonum least-positive-long-float)
(defconstant least-negative-flonum least-negative-long-float)
(defconstant flonum-epsilon long-float-epsilon)
(defconstant least-positive-normalized-flonum least-positive-normalized-long-float)

(defconstant flonum-exponent-marker #\L)

)

#+flonum-double-double
(progn

;;;; The Maxima 'flonum can be a 'kernel:double-double-float on the CMU CL.

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *read-default-float-format* 'kernel:double-double-float))

;; Tell Lisp the float type for a 'flonum.
(deftype flonum (&optional low high)
  (cond (high
	 `(kernel:double-double-float ,low ,high))
	(low
	 `(kernel:double-double-float ,low))
	(t
	 'kernel:double-double-float)))

;; While double-double can represent number as up to
;; most-positive-double-float, it can't really do operations on them
;; due to the way multiplication and division are implemented.  (I
;; don't think there's any workaround for that.)
;;
;; So, the largest number that can be used is the float just less than
;; 2^1024/(1+2^27).  This is the number given here.
(defconstant most-positive-double-double-hi
  (scale-float (cl:float (1- 9007199187632128) 1d0) 944))

(defconstant most-positive-flonum (cl:float most-positive-double-double-hi 1w0))
(defconstant most-negative-flonum (cl:float (- most-positive-double-double-hi 1w0)))
(defconstant least-positive-flonum (cl:float least-positive-double-float 1w0))
(defconstant least-negative-flonum (cl:float least-negative-double-float 1w0))
;; This is an approximation to a double-double epsilon.  Due to the
;; way double-doubles are represented, epsilon is actually zero
;; because 1+x = 1 only when x is zero.  But double-doubles only have
;; 106 bits of precision, so we use that as epsilon.
(defconstant flonum-epsilon (scale-float 1w0 -106))
(defconstant least-positive-normalized-flonum (cl:float least-positive-normalized-double-float 1w0))

(defconstant flonum-exponent-marker #\W)

)



;;------------------------------------------------------------------------
;; From algsys.lisp
(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS
	 which where introduced into the expression. Useful for mapping
	 over and using as an argument to SUBST.")

;;------------------------------------------------------------------------
;; From asum.lisp
(defmvar $zeta%pi t)

;; factorial stuff

(defmvar $factlim 100000) ; set to a big integer which will work (not -1)
(defvar makef nil)

(defmvar $cauchysum nil
  "When multiplying together sums with INF as their upper limit, 
causes the Cauchy product to be used rather than the usual product.
In the Cauchy product the index of the inner summation is a function of 
the index of the outer one rather than varying independently."
  modified-commands '$sum)

;; sum begins
(defmvar $gensumnum 0
  "The numeric suffix used to generate the next variable of
summation.  If it is set to FALSE then the index will consist only of
GENINDEX with no numeric suffix."
  modified-commands '$sum
  setting-predicate #'(lambda (x) (or (null x) (integerp x))))

(defmvar $genindex '$i
  "The alphabetic prefix used to generate the next variable of
summation when necessary."
  modified-commands '$sum
  setting-predicate #'symbolp)

(defmvar $zerobern t)
(defmvar $simpsum nil)
(defmvar $simpproduct nil)

(defvar *infsumsimp t)

(defmvar $cflength 1)
(defmvar $taylordepth 3)
(defmvar $verbose nil)

(defvar ps-bmt-disrep t)
(defvar silent-taylor-flag nil
  ;; From comment in hayat.lisp
  "If true indicates that errors will be returned via a throw to
  TAY-ERR")

;;------------------------------------------------------------------------
;; From comm2.lisp
(defmvar $rootsconmode t)

;;------------------------------------------------------------------------
;; From comm.lisp
(defmvar $exptsubst nil)
(defmvar $partswitch nil)
(defmvar $inflag nil)
(defmvar $derivsubst nil)
(defmvar $opsubst t)

(defvar *islinp* nil
  "When T, sdiff is called from the function islinear")
(defvar *atp* nil
  "When T, prevents substitution from applying to vars ; bound by %sum,
  %product, %integrate, %limit")

;; Store built-in operators, which get additional properties.
;; These operators aren't killed by the function kill-operator.
(defvar *mopl* nil)

(defvar $gradefs '((mlist simp)))
(defvar $dependencies '((mlist simp)))

(defvar atvars '($@1 $@2 $@3 $@4))
(defvar in-p nil)
(defvar substp nil)

(defvar dummy-variable-operators '(%product %sum %laplace %integrate %limit %at))

;;------------------------------------------------------------------------
;; From compar.lisp
(defmvar $prederror nil)
(defmvar limitp)

;;------------------------------------------------------------------------

