;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

;;;; This file contains global vars (defvars/defmvars) that are used in
;;;; multiple files.  We gather them all here so that they are
;;;; consistently defined across the build and to make the dependencies
;;;; easier to track.

(in-package "MAXIMA")

(defvar *variable-initial-values* (make-hash-table)
  "Hash table containing all Maxima defmvar variables and their
  initial values")

(defmacro defmvar (var &optional val doc &rest options)
  "Define a Maxima variable VAR that is user-visible.  It is
  initialized to the value VAL.  An associated documentation string
  can be supplied in DOC.  OPTIONS contains a list of options that can
  be applied to the variable.

  The valid options are:

    NO-RESET
        - If given, the variable will not be reset.
    FIXNUM, BOOLEAN, STRING, FLONUM
        - The type of variable.  Currently ignored.
    :PROPERTIES
        - A list of properties to be applied for this variable.  It is
          a list of lists.  Each sublist is a list of the property and
          the value to be assigned to the property.
    :SETTING-PREDICATE
        - A function of one argument that returns NIL if the given
          value is not a valid value for the variable.
    :SETTING-LIST
        - A list of values that can be assigned to the variable.  An
          error is signaled if an attempt to assign a different value
          is done.
    :DEPRECATED-P
        - The variable is marked as deprecated.  The option is a
          string to be printed when this deprecated variable is used.
          A warning is printed once when first used. This option
          overrides any other options.  When the variable is used, a
          message is printed of the form: \"Deprecated variable
          `<var>': <string>\" where <var> is the name of this
          variable, and <string> is the value given to :deprecated-p.

          An example of usage:

            (defmvar $foo foo-value
              \"Docstring for deprecated foo.\"
              :deprecated-p \"Use bar instead\")

  The list of properties has the form ((ind1 val1) (ind2 val2) ...)
  where IND1 is the name of the property and VAL1 is the value
  associated with the property.

  Other options that are recognized but ignored: IN-CORE, SEE-ALSO,
  MODIFIED-COMMANDS.  For any other options, a warning is produced.

  Do not use both :SETTING-PREDICATE and :SETTING-LIST.  Also do not
  use a :PROPERTIES with an 'ASSIGN property.  :SETTING-PREDICATE and
  :SETTING-LIST sets the 'ASSIGN property to implement the
  functionality.

  
"
  (let ((maybe-reset
          ;; Default is to reset the variable to it's initial value.
          `((unless (gethash ',var *variable-initial-values*)
              (setf (gethash ',var *variable-initial-values*)
                    ,val))))
        maybe-declare-type
        maybe-set-props
	maybe-predicate
	setting-predicate-p
	setting-list-p
	assign-property-p
	deprecated-p)

    (do ((opts options (rest opts)))
        ((null opts))
      #+nil
      (format t "opts = ~S~%" opts)
      (case (car opts)
        (no-reset
	 (unless deprecated-p
           ;; Don't reset the value
           (setf maybe-reset nil)))
        ((fixnum boolean string flonum)
	 ;; Don't declare the types yet.  There are testsuite failures
	 ;; with sbcl that some things declared fixnum aren't assigned
	 ;; fixnum values.  Some are clearly bugs in the code where we
	 ;; do things like
	 ;;
	 ;;  (let ($expop ...)
	 ;;    (setq $expop 0))
	 ;;
	 ;; Some known such variables: $expop, $fpprintprec (in ev
	 ;; statements), $factors_only, $expon,
	 ;;
	 ;; We should also note that when this is fixed, we should
	 ;; also add an 'assign property to verify that only fixnums,
	 ;; etc., are allowed.
         #+nil
	 (setf maybe-declare-type
               `((declaim (type ,(car opts) ,var)))))
        (in-core
         ;; Ignore this
         )
	(:properties
	 (unless deprecated-p
           (setf maybe-set-props
		 (mapcar #'(lambda (o)
                             (destructuring-bind (ind val)
				 o
			       (when (eql ind 'assign)
				 (setf assign-property-p t))
                               `(putprop ',var ,val ',ind)))
			 (second opts)))
	   (when assign-property-p
	     (when setting-predicate-p
	       (error "Cannot use 'ASSIGN property in :PROPERTIES if :SETTING-PREDICATE already specified."))
	     (when setting-list-p
	       (error "Cannot use 'ASSIGN property in :PROPERTIES if :SETTING-LIST already specified."))))
           (setf opts (rest opts)))
	(:setting-predicate
	 (unless deprecated-p
	   (when setting-list-p
	     (error "Cannot use :SETTING-PREDICATE when :SETTING-LIST already specified."))
	   (when assign-property-p
	     (error "Cannot use :SETTING-PREDICATE when :PROPERTIES uses the 'ASSIGN property."))
	   (setf setting-predicate-p t)
	   ;; A :SETTING-PREDICATE is a function (symbol or lambda) of
	   ;; one arg specifying the value that variable is to be set
	   ;; to.  It should return non-NIL if the value is valid.  An
	   ;; optional second value may be returned.  This is a string
	   ;; that can be used as the reason arg for MSETERR to explain
	   ;; why the setting failed.
	   ;;
	   ;; WARNING: Do not also have a :properties item with an
	   ;; 'assign property.  Currently this takes precedence.
	   (let ((assign-func
		  `#'(lambda (var val)
		       (multiple-value-bind (ok reason)
			   (funcall ,(second opts) val)
			 (unless ok
			   (mseterr var val reason))))))
	     (setf maybe-predicate
		   `((putprop ',var ,assign-func 'assign)))))
	 
	 ;; Skip over the predicate function.
	 (setf opts (rest opts)))
	(:setting-list
	 (unless deprecated-p
	   (when setting-predicate-p
	     (error "Cannot use :SETTING-LIST when :SETTING-PREDICATE already specified."))
	   (when assign-property-p
	     (error "Cannot use :SETTING-LIST when :PROPERTIES uses the 'ASSIGN property."))
	   (setf setting-list-p t)
	   ;; A :SETTING-LIST is a list of possible values that can be
	   ;; assigned to the variable.  An error is signaled if the
	   ;; variable is not assigned to one of these values.  This
	   ;; could be handled with :SETTING-PREDICATE, of course.  This
	   ;; is a convenience feature but it also makes it explicit
	   ;; that we only allow the possible values.
	   (let ((assign-func
		  `#'(lambda (var val)
		       (let ((possible-values ',(second opts)))
			 (unless (member val possible-values)
			   (mseterr var val
				    (let ((*print-case* :downcase))
				      (format nil "must be one of: ~{~A~^, ~}"
					      (mapcar #'stripdollar possible-values)))))))))
	     (setf maybe-predicate
		   `((putprop ',var ,assign-func 'assign)))))
	 ;; Skip over the values.
	 (setf opts (rest opts)))
        ((see-also modified-commands setting-list)
         ;; Not yet supported, but we need to skip over the following
         ;; item too which is the parameter for this option.
         (setf opts (rest opts)))
	(:deprecated-p
	 ;; This overrides everything and makes the variable
	 ;; deprecated.  This means it's unbound, and the 'bindtest
	 ;; property is set and the 'assign property is set to
	 ;; 'neverset.  The option is a string for the bindtest
	 ;; message.
	 (setf deprecated-p t
	       maybe-reset nil
	       maybe-declare-type nil
	       maybe-predicate nil
	       setting-predicate-p nil
	       setting-list-p nil
	       assign-property-p nil)
	 (setf maybe-set-props
	       `((cl:makunbound ',var)
		 (putprop ',var :deprecated 'bindtest)
		 (putprop ',var 'neverset 'assign)
		 (setf *bindtest-deprecation-messages*
		       (acons ',var
			      (cons 
			       (concatenate 'string
					    "Deprecated variable `~M': "
					    ,(second opts))
			       ,val)
			      *bindtest-deprecation-messages*))))
	 (setf opts (rest opts)))
        (t
         (warn "Ignoring unknown defmvar option for ~S: ~S"
               var (car opts)))))
    `(progn
       ,@maybe-reset
       ,@maybe-declare-type
       (defvar ,var ,val ,doc)
       ,@maybe-set-props
       ,@maybe-predicate)))

;; For the symbol SYM, add to the plist the property INDIC with a
;; value of VAL.
;;
;; Some known INDIC properties
;;
;;   ASSIGN
;;       When a variable is assigned (in maxima), the 'assign property
;;       is used to check whether the value to be assigned is valid.
;;       The value can be anything that can be funcall'ed.  If it
;;       returns, then the assignment proceeds.  Thus, if the value is
;;       invalid, an error must be signaled.
;;   EVFLAG
;;       When a symbol <x> has this property, expressions 'ev(<expr>,
;;       <x>)' and '<expr>, <x>' are equivalent to 'ev(<expr>,
;;       <x>=true)'.  See the user manual for more details.
;;   SYSCONST
;;       When true, it denotes that the symbol is a constant and
;;       cannot be changed by the user.  This includes things like
;;       $%pi, $%e, $inf, $minf, $true, and $false.
(defun putprop (sym val  indic)
  (if (consp sym)
      (setf (getf (cdr sym) indic) val)
      (setf (get sym indic) val)))

;;------------------------------------------------------------------------
;; From limit.lisp
;;
;; Declare user-visible special variables and other global special variables.
;;
;; Should these be defconstants?  I (rtoy) don't think they should be
;; allowed to be changed.
(defmvar *infinities* '($inf $minf $infinity)
  "The types of infinities recognized by Maxima.
   INFINITY is complex infinity")

(defmvar *real-infinities* '($inf $minf)
  "The real infinities, `inf' is positive infinity, `minf' negative infinity")

(defmvar *infinitesimals* '($zeroa $zerob)
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

(defconstant +most-positive-flonum+ most-positive-double-float)
(defconstant +most-negative-flonum+ most-negative-double-float)
(defconstant +least-positive-flonum+ least-positive-double-float)
(defconstant +least-negative-flonum+ least-negative-double-float)
(defconstant +flonum-epsilon+ double-float-epsilon)
(defconstant +least-positive-normalized-flonum+ least-positive-normalized-double-float)
(defconstant +least-negative-normalized-flonum+ least-negative-normalized-double-float)

(defconstant +flonum-exponent-marker+ #\D)
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

(defconstant +most-positive-flonum+ most-positive-long-float)
(defconstant +most-negative-flonum+ most-negative-long-float)
(defconstant +least-positive-flonum+ least-positive-long-float)
(defconstant +least-negative-flonum+ least-negative-long-float)
(defconstant +flonum-epsilon+ long-float-epsilon)
(defconstant +least-positive-normalized-flonum+ least-positive-normalized-long-float)
(defconstant +least-negative-normalized-flonum+ least-negative-normalized-long-float)

(defconstant +flonum-exponent-marker+ #\L)

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

(defconstant +most-positive-flonum+ (cl:float most-positive-double-double-hi 1w0))
(defconstant +most-negative-flonum+ (cl:float (- most-positive-double-double-hi 1w0)))
(defconstant +least-positive-flonum+ (cl:float least-positive-double-float 1w0))
(defconstant +least-negative-flonum+ (cl:float least-negative-double-float 1w0))
;; This is an approximation to a double-double epsilon.  Due to the
;; way double-doubles are represented, epsilon is actually zero
;; because 1+x = 1 only when x is zero.  But double-doubles only have
;; 106 bits of precision, so we use that as epsilon.
(defconstant +flonum-epsilon+ (scale-float 1w0 -106))
(defconstant +least-positive-normalized-flonum+ (cl:float least-positive-normalized-double-float 1w0))
(defconstant +least-negative-normalized-flonum+ (cl:float least-negative-normalized-double-float 1w0))

(defconstant +flonum-exponent-marker+ #\W)

)

;;------------------------------------------------------------------------
;; From algsys.lisp
(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS which
  where introduced into the expression. Useful for mapping over and
  using as an argument to SUBST.") 
;;------------------------------------------------------------------------
;; From asum.lisp
(defmvar $zeta%pi t
  "When true, 'zeta' returns an expression proportional to '%pi^n' for
  even integer 'n'.")

;; factorial stuff

(defmvar $factlim 100000 ; set to a big integer which will work (not -1)
  "specifies the highest factorial which is automatically expanded.  If
  it is -1 then all integers are expanded.") 
(defvar makef nil)

(defmvar $cauchysum nil
  "When multiplying together sums with INF as their upper limit, causes
  the Cauchy product to be used rather than the usual product. In the
  Cauchy product the index of the inner summation is a function of the
  index of the outer one rather than varying independently."
  modified-commands '$sum
  :properties ((evflag t)))

;; sum begins
(defmvar $gensumnum 0
  "The numeric suffix used to generate the next variable of summation.
  If it is set to FALSE then the index will consist only of GENINDEX
  with no numeric suffix."
  modified-commands '$sum
  :setting-predicate #'(lambda (x)
			 (values (or (null x)
				     (and (integerp x)
					  (>= x 0)))
				 "must be false or a non-negative integer")))

(defmvar $genindex '$i
  "The alphabetic prefix used to generate the next variable of summation
  when necessary."
  modified-commands '$sum
  :setting-predicate #'symbolp)

(defmvar $zerobern t
  "when false, 'bern' excludes the Bernoulli numbers and 'euler'
  excludes the Euler numbers which are equal to zero.")
(defmvar $simpsum nil
  "When true, the result of a 'sum' is simplified. This simplification
  may sometimes be able to produce a closed form."
  :properties ((evflag t)))
(defmvar $simpproduct nil
  "When true, the result of a 'product' is simplified.  This
  simplification may sometimes be able to produce a closed form."
  :properties ((evflag t)))

(defvar *infsumsimp t)

(defmvar $cflength 1
  "Controls the number of terms of the continued fraction the function
  'cf' will give, as the value 'cflength' times the period.")
(defmvar $taylordepth 3
  "If there are still no nonzero terms, 'taylor' doubles the degree of
  the expansion of '<g>(<x>)' so long as the degree of the expansion
  is less than or equal to '<n> 2^taylordepth'.")
(defmvar $verbose nil
  "When true, 'powerseries' prints progress messages.")

(defvar silent-taylor-flag nil
  ;; From comment in hayat.lisp
  "If true indicates that errors will be returned via a throw to
  TAY-ERR")

;; linear operator stuff
(defparameter *opers-list '(($linear . linearize1)))

(defparameter  opers (list '$linear))

;;------------------------------------------------------------------------
;; From comm2.lisp
(defmvar $rootsconmode t
  "Governs the behavior of the 'rootscontract' command. See
  'rootscontract' for details.")

;;------------------------------------------------------------------------
;; From comm.lisp
(defmvar $exptsubst nil
  "When 'true', permits substitutions such as 'y' for '%e^x' in
  '%e^(a*x)'.")
(defmvar $partswitch nil
  "When true, 'end' is returned when a selected part of an expression
  doesn't exist, otherwise an error message is given.")
(defmvar $inflag nil
  "When true, functions for part extraction inspect the internal form of
  'expr'.")
(defmvar $derivsubst nil
  "When true, a non-syntactic substitution such as 'subst (x, 'diff (y,
  t), 'diff (y, t, 2))' yields ''diff (x, t)'.")
(defmvar $opsubst t
  "When false, 'subst' does not attempt to substitute into the operator
  of an expression.")

(defvar *islinp* nil
  "When T, sdiff is called from the function islinear")
(defvar *atp* nil
  "When T, prevents substitution from applying to vars ; bound by %sum,
  %product, %integrate, %limit")

;; Store built-in operators, which get additional properties.
;; These operators aren't killed by the function kill-operator.
(defvar *mopl* nil)

(defvar atvars '($@1 $@2 $@3 $@4))
(defvar in-p nil)
(defvar substp nil)

(defvar dummy-variable-operators '(%product %sum %laplace %integrate %limit %at))

;;------------------------------------------------------------------------
;; From compar.lisp
(defvar $context '$global
  "Whenever a user assumes a new fact, it is placed in the context named
  as the current value of the variable CONTEXT.  Similarly, FORGET
  references the current value of CONTEXT.  To add or DELETE a fact
  from a different context, one must bind CONTEXT to the intended
  context and then perform the desired additions or deletions.  The
  context specified by the value of CONTEXT is automatically
  activated.  All of MACSYMA's built-in relational knowledge is
  contained in the default context GLOBAL.")

(defmvar $contexts '((mlist) $global)
  "A list of the currently active contexts."
  no-reset
  :properties ((assign 'neverset)))

(defvar $activecontexts '((mlist))
  "A list of the currently activated contexts")

(defvar *complexsign* nil
  "If T, COMPAR works in a complex mode.")

(defmvar $prederror nil
  "When true, an error message is displayed whenever the predicate of an
  'if' statement or an 'is' function fails to evaluate to either
  'true' or 'false'.")
(defmvar limitp)

(defmvar sign-imag-errp t
  "If T errors out in case COMPAR meets up with an imaginary
  quantity. If NIL THROWs in that case."
  no-reset)

;;------------------------------------------------------------------------
;; From cpoly.lisp
(defmvar $polyfactor nil
  "When T factor the polynomial over the real or complex numbers.")

;;------------------------------------------------------------------------
;; From csimp2.lisp
(defmvar $gamma_expand nil
  "Expand gamma(z+n) for n an integer when T.") 

;;------------------------------------------------------------------------
;; From csimp.lisp
(defmvar $demoivre nil
  "When true, complex exponentials are converted into equivalent
  expressions in terms of circular functions."
  :properties ((evflag t)))
(defmvar $nointegrate nil)
(defmvar $lhospitallim 4
  "The maximum number of times L'Hospital's rule is used in 'limit'.")
(defmvar $tlimswitch t
  "When true, the 'limit' command will use a Taylor series expansion if
  the limit of the input expression cannot be computed directly.")
(defmvar $limsubst nil
  "When false, prevents 'limit' from attempting substitutions on unknown
  forms.")

(defvar rsn* nil)
(defvar plogabs nil)

;; Simplified shortcuts of constant expressions involving %pi.
(defvar %p%i '((mtimes) $%i $%pi)
  "%pi*%i")
(defvar fourth%pi '((mtimes) ((rat simp) 1 4) $%pi)
  "%pi/4")
(defvar half%pi '((mtimes) ((rat simp) 1 2) $%pi)
  "%pi/2")
(defvar %pi2 '((mtimes) 2 $%pi)
  "2*%pi")
(defvar half%pi3 '((mtimes) ((rat simp) 3 2) $%pi)
  "3/2*%pi")

(defmvar $sumsplitfact t
  "When false, 'minfactorial' is applied after a 'factcomb'.")

;;------------------------------------------------------------------------
;; From defint.lisp
(defmvar integerl nil
  "An integer-list for non-atoms found out to be `integer's")

(defmvar nonintegerl nil
  "A non-integer-list for non-atoms found out to be `noninteger's")

;; Not really sure what this is meant to do, but it's used by MTORAT,
;; KEYHOLE, and POLELIST.
(defvar *semirat* nil)

;;------------------------------------------------------------------------
;; From displa.lisp
(defmvar $ttyoff nil
  "When true, output expressions are not displayed.")

(defmvar $display2d t
  "Causes equations to be drawn in two dimensions.  Otherwise, drawn
  linearly.")

(defmvar $lispdisp nil
  "Causes symbols not having $ as the first character in their pnames to
  be preceded with a ? when displayed.")

(defmvar $derivabbrev nil
  "When true, symbolic derivatives (that is, 'diff' nouns) are displayed
  as subscripts.  Otherwise, derivatives are displayed in the Leibniz
  notation 'dy/dx'.")

(defmvar $stringdisp nil
  "Causes strings to be bracketed in double quotes when displayed.
  Normally this is off, but is turned on when a procedure definition
  is being displayed.")

;; These three variables are bound within Macsyma Listeners since they are different
;; for each window.  Set them here, anyway, so that RETRIEVE can be called from
;; top level.  The size of TOP-WINDOW is wired in here.

(defmvar $linel 79.
  "The assumed width (in characters) of the console display for the
  purpose of displaying expressions."
  :setting-predicate #'(lambda (val)
			 ;; The value must be fixnum within range.
			 ;; The upper limit was arbitrarily chosen.
			 (values (and (fixnump val)
				      (< 0 val 1000001))
				 "must be an integer between 0 and 1000001, exclusive")))
(defvar ttyheight 24.)

(defmvar $known_index_properties '((mlist) $presubscript $presuperscript $postsubscript $postsuperscript))

(defvar *display-labels-p* t)

;;------------------------------------------------------------------------
;; From dskfn.lisp
(defmvar $packagefile nil
  "When true, prevent information from being added to Maxima's
  information-lists (e.g.  'values', 'functions') except where
  necessary when the file is loaded in.  Useful for package designers
  who use 'save' or 'translate' to create packages (files).")

;;------------------------------------------------------------------------
;; From float.lisp
(defmvar $float2bf t
  "If TRUE, no MAXIMA-ERROR message is printed when a floating point
  number is converted to a bigfloat number.")

(defmvar $bftorat nil
  "Controls the conversion of bigfloat numbers to rational numbers.  If
  FALSE, RATEPSILON will be used to control the conversion (this
  results in relatively small rational numbers).  If TRUE, the
  rational number generated will accurately represent the bigfloat.")

(defmvar $bftrunc t
  "If TRUE, printing of bigfloat numbers will truncate trailing zeroes.
  Otherwise, all trailing zeroes are printed.")

(defmvar $fpprintprec 0
  "Controls the number of significant digits printed for floats.  If
  0, then full precision is used."
  fixnum
  :setting-predicate #'(lambda (val)
			 ;; $fpprintprec must be a non-negative fixnum
			 ;; and also cannot be equal to 1.
			 (values (and (fixnump val)
				      (>= val 0)
				      (/= val 1))
				 "must be a non-negative integer and not equal to -1")))

(defmvar $maxfpprintprec (ceiling (log (expt 2 (float-digits 1.0)) 10.0))
  "The maximum number of significant digits printed for floats.")

(defmvar $fpprec $maxfpprintprec
  "Number of decimal digits of precision to use when creating new
  bigfloats. One extra decimal digit in actual representation for
  rounding purposes."
  :properties ((assign 'fpprec1)))

(defmvar bigfloatzero '((bigfloat simp 56.) 0 0)
  "Bigfloat representation of 0"
  in-core)

(defmvar bigfloatone  '((bigfloat simp 56.) #.(expt 2 55.) 1)
  "Bigfloat representation of 1"
  in-core)

(defmvar bfhalf	      '((bigfloat simp 56.) #.(expt 2 55.) 0)
  "Bigfloat representation of 1/2")

(defmvar bfmhalf      '((bigfloat simp 56.) #.(- (expt 2 55.)) 0)
  "Bigfloat representation of -1/2")

(defmvar bigfloat%e   '((bigfloat simp 56.) 48968212118944587. 2)
  "Bigfloat representation of %E")

(defmvar bigfloat%pi  '((bigfloat simp 56.) 56593902016227522. 2)
  "Bigfloat representation of %pi")

(defmvar bigfloat%gamma '((bigfloat simp 56.) 41592772053807304. 0)
  "Bigfloat representation of %gamma")

(defmvar bigfloat_log2 '((bigfloat simp 56.) 49946518145322874. 0)
  "Bigfloat representation of log(2)")

;; Number of bits of precision in the mantissa of newly created bigfloats.
;; FPPREC = ($FPPREC+1)*(Log base 2 of 10)

(defvar fpprec)

;;------------------------------------------------------------------------
;; From gamma.lisp
(defmvar $factorial_expand nil
  "Controls the simplification of expressions like '(n+1)!', where 'n'
  is an integer.  See 'factorial'.")
(defmvar $beta_expand nil
  "When true, 'beta(a,b)' and related functions are expanded for
  arguments like a+n or a-n, where n is an integer.")

(defmvar $hypergeometric_representation nil
  "When T a transformation to a hypergeometric representation is done.")

;;------------------------------------------------------------------------
;; From inmis.lisp
(defmvar $listconstvars nil
  "Causes LISTOFVARS to include %E, %PI, %I, and any variables declared
  constant in the list it returns if they appear in exp.  The default
  is to omit these." boolean see-also $listofvars)

;;------------------------------------------------------------------------
;; From macsys.lisp
(defmvar $showtime nil
  "When T, the computation time is printed with each output expression.")

(defmvar $_ '$_
  "last thing read in, corresponds to lisp +")
(defmvar $__ '$__
  "thing read in which will be evaluated, corresponds to -")

;;------------------------------------------------------------------------
;; From mat.lisp
(defmvar $globalsolve nil
  "When true, solved-for variables are assigned the solution values
  found by 'linsolve', and by 'solve' when solving two or more linear
  equations.")
(defmvar $sparse nil
  "When true and if 'ratmx' is 'true', then 'determinant' will use
  special routines for computing sparse determinants.")
(defmvar $backsubst t
  "When false, prevents back substitution in 'linsolve' after the
  equations have been triangularized.")

(defmvar $%rnum 0
  "The counter for the '%r' variables introduced in solutions by 'solve'
  and 'algsys'.")

(defmvar $linsolve_params t
  "When true, 'linsolve' also generates the '%r' symbols used to
  represent arbitrary parameters described in the manual under
  'algsys'.")

(defmvar *rank* nil)
(defmvar *inv* nil)

;;------------------------------------------------------------------------
;; From matrix.lisp
(defmvar $detout nil
  "When true, the determinant of a matrix whose inverse is computed is
  factored out of the inverse.")
(defmvar $ratmx nil
  "When 'ratmx' is 'false', determinant and matrix addition,
  subtraction, and multiplication are performed in the representation
  of the matrix elements and cause the result of matrix inversion to
  be left in general representation.

  When 'ratmx' is 'true', the 4 operations mentioned above are
  performed in CRE form and the result of matrix inverse is in CRE
  form."
  :properties ((evflag t)))
(defmvar $matrix_element_mult "*"
  "The operation invoked in place of multiplication in a matrix
  multiplication.  'matrix_element_mult' can be assigned any binary
  operator.")
(defmvar $matrix_element_add "+"
  "the operation invoked in place of addition in a matrix
  multiplication.  'matrix_element_add' can be assigned any n-ary
  operator.")

;;------------------------------------------------------------------------
;; From mdot.lisp
(defmvar $dotscrules nil
  "Causes a non-commutative product of a scalar and another term to be
  simplified to a commutative product.  Scalars and constants are
  carried to the front of the expression."
  :properties ((evflag t)))

(defmvar $dotdistrib nil
  "Causes every non-commutative product to be expanded each time it is
  simplified, i.e.  A . (B + C) will simplify to A . B + A . C.")

(defmvar $dotexptsimp t "Causes A . A to be simplified to A ^^ 2.")

(defmvar $dotassoc t
  "Causes a non-commutative product to be considered associative, so
  that A . (B . C) is simplified to A . B . C.  If this flag is off,
  dot is taken to be right associative, i.e.  A . B . C is simplified
  to A . (B . C)."
  :properties ((assign #'(lambda (name val)
			   (declare (ignore name))
			   (cput 'mnctimes val 'associative)))))

(defmvar $doallmxops t
  "Causes all operations relating to matrices (and lists) to be carried
  out.  For example, the product of two matrices will actually be
  computed rather than simply being returned.  Turning on this switch
  effectively turns on the following three.")

(defmvar $domxmxops t "Causes matrix-matrix operations to be carried out.")

(defmvar $doscmxops nil "Causes scalar-matrix operations to be carried out.")

(defmvar $scalarmatrixp t
  "Causes a square matrix of dimension one to be converted to a scalar,
  i.e. its only element.")

(defmvar $assumescalar t
  "This governs whether unknown expressions 'exp' are assumed to
  behave like scalars for combinations of the form 'exp op matrix'
  where op is one of {+, *, ^, .}.  It has three settings:

  FALSE -- such expressions behave like non-scalars.
  TRUE  -- such expressions behave like scalars only for the commutative
	   operators but not for non-commutative multiplication.
  ALL   -- such expressions will behave like scalars for all operators
	   listed above.

  Note: This switch is primarily for the benefit of old code.  If
  possible, you should declare your variables to be SCALAR or
  NONSCALAR so that there is no need to rely on the setting of this
  switch.")

;;------------------------------------------------------------------------
;; From merror.lisp
(defmvar $error `((mlist simp) "No error.")
  "During an MAXIMA-ERROR break this is bound to a list of the
  arguments to the call to MAXIMA-ERROR, with the message text in a
  compact format."
  :properties ((assign 'neverset)))

(defmvar $errormsg 't
  "If `false' then no maxima-error message is printed!")

;;------------------------------------------------------------------------
;; From mlisp.lisp
(defvar featurel
  '($integer $noninteger $even $odd $rational $irrational $real $imaginary $complex
    $analytic $increasing $decreasing $oddfun $evenfun $posfun $constant
    $commutative $lassociative $rassociative $symmetric $antisymmetric
    $integervalued))

(defmvar $features (cons '(mlist simp) (append featurel nil))
  "A list of mathematical features which are mathematical preoperties of
  functions and variables.")
(defmvar $%enumer nil
  "When true, '%e' is replaced by its numeric value 2.718... whenever
  'numer' is 'true'."
  :properties ((evflag t)))
(defmvar $float nil
  "Causes non-integral rational numbers and bigfloat numbers to be
  converted to floating point."
  :properties ((evflag t)))
(defmvar $translate nil
  "Causes automatic translation of a user's function to Lisp.")
(defmvar $transrun t
  "When false, the interpreted version of all functions to be
  run (provided they are still around) rather than the translated
  version.")
(defmvar $savedef t
  "When true, the Maxima version of a user function is preserved when
  the function is translated.  This permits the definition to be
  displayed by 'dispfun' and allows the function to be edited.")
(defmvar $infeval nil
  "When true, Enables \"infinite evaluation\" mode.  'ev' repeatedly
  evaluates an expression until it stops changing."
  :properties ((evflag t)))
(defmvar $piece '$piece
  "Holds the last expression selected when using the 'part' functions.")

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(defvar munbound '|#####|)
(defvar fixunbound most-negative-fixnum)
(defvar flounbound +most-negative-flonum+)

(defmvar munbindp nil
  "Used for safely `munbind'ing incorrectly-bound variables."
  no-reset)

(defmvar $setcheck nil
  "If 'setcheck' is set to a list of variables (which can be
  subscripted), Maxima prints a message whenever the variables, or
  subscripted occurrences of them, are bound with the ordinary
  assignment operator ':', the '::' assignment operator, or function
  argument binding, but not the function assignment ':=' nor the macro
  assignment '::=' operators.  The message comprises the name of the
  variable and the value it is bound to.

  'setcheck' may be set to 'all' or 'true' thereby including all
  variables."
  :setting-predicate #'(lambda (val)
			 (values (or ($listp val)
				     (member val '($all t nil)))
				 "must be a list or one of all, true, or false")))


;;Function Call stack each element is
;; (fname . bindlist) where bindlist was the value at time of entry.
;; So you can use this to compute what the bindings were at any
;; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

;; If this is T then arrays are stored in the value cell,
;; whereas if it is false they are stored in the function cell
(defmvar $use_fast_arrays nil
  "When true, arrays declared by 'array' are values instead of
  properties, and undeclared arrays ('hashed arrays') are implemented
  as Lisp hashed arrays.")

(defvar bindlist nil)
(defvar loclist nil)
(defvar *nounl* nil)
(defvar scanmapp nil)
(defvar maplp nil)
(defvar evp nil)
(defvar mlocp nil)
(defvar fmaplvl 0)
(defvar aryp nil)
(defvar msump nil)
(defvar evarrp nil)
(defvar factlist nil)
(defvar *nounsflag* nil)
(defvar transp nil)
(defvar noevalargs nil)

(defmvar $structures '((mlist))
  nil
  no-reset
  :properties ((assign 'neverset)))

(defvar mspeclist nil)
(defvar mfexprp t)
;;------------------------------------------------------------------------
;; From mload.lisp
(defmvar $load_pathname nil
  "The full pathname of the file being loaded")

(defvar *maxima-testsdir*)

;;------------------------------------------------------------------------
;; From nforma.lisp
(defmvar $powerdisp nil
  "When true, a sum is displayed with its terms in order of increasing
  power.")
(defmvar $pfeformat nil
  "When true, a ratio of integers is displayed with the solidus (forward
  slash) character, and an integer denominator 'n' is displayed as a
  leading multiplicative term '1/n'.")
(defmvar $%edispflag nil
  "When true, Maxima displays '%e' to a negative exponent as a
  quotient.")
(defmvar $sqrtdispflag t
  "When false, causes 'sqrt' to display with exponent 1/2.")

;;------------------------------------------------------------------------
;; From rat3b.lisp
(defmvar $ratwtlvl nil
  "'ratwtlvl' is used in combination with the 'ratweight' function to
  control the truncation of canonical rational expressions (CRE). For
  the default value of 'false', no truncation occurs."
  :properties ((assign #'(lambda (name val)
			   ;; Forward declaration
			   (declare (special $ratfac))
			   (when (and val (not (fixnump val)))
			     (mseterr name val "must be an integer"))
			   (when (and val $ratfac)
			     (merror (intl:gettext "assignment: 'ratfac' and 'ratwtlvl' may not both be used at the same time.")))))))

(defmvar $ratalgdenom t        ;If T then denominator is rationalized.
  "When true, allows rationalization of denominators with respect to
  radicals to take effect."
  :properties ((evflag t)))

;;------------------------------------------------------------------------
;; From rat3c.lisp
;; List of GCD algorithms.  Default one is first.
(defmvar *gcdl* '($spmod $subres $ez $red $mod $algebraic))

(defmvar $gcd (car *gcdl*)		;Sparse Modular
  "The default GCD algorithm.  If false, the GCD is prevented from being
  taken when expressions are converted to canonical rational
  expression (CRE) form."
  :setting-predicate #'(lambda (val)
			 (or (null val)
			     (member val *gcdl*))))


;; It is convenient to have the *bigprimes* be actually less than half
;; the size of the most positive fixnum, so that arithmetic is easier.
;;
;; *bigprimes* and *alpha are initialized in
;; initialize-runtime-globals instead of here because *bigprimes*
;; needs the NEXT-PRIME function to generate the list of primes, and
;; NEXT-PRIME isn't available yet.  Likewise, *alpha is initialized to
;; the first element of *bigprimes*.
(defvar *bigprimes*)
(defmvar *alpha)

;;------------------------------------------------------------------------
;; From rat3d.lisp
(defmvar algfac* nil)
(defmvar low* nil)
(defmvar $intfaclim t
  "If 'true', maxima will give up factorization of integers if no factor
  is found after trial divisions and Pollard's rho method and
  factorization will not be complete.

  When 'intfaclim' is 'false' (this is the case when the user calls
  'factor' explicitly), complete factorization will be attempted.")
(defmvar $factor_max_degree 1000
  "If set to an integer n, some potentially large (many factors)
  polynomials of degree > n won't be factored, preventing huge memory
  allocations and stack overflows. Set to zero to deactivate."
  fixnum
  :properties ((assign 'posintegerset)))

(defmvar $savefactors nil "If t factors of ratreped forms will be saved")

(defvar *checkfactors* () "List of saved factors")

;;------------------------------------------------------------------------
;; From rat3e.lisp

;; User level global variables.
(defmvar $keepfloat nil
  "If `t' floating point coeffs are not converted to rationals"
  :properties ((evflag t)))
(defmvar $factorflag nil
  "If `t' constant factor of polynomial is also factored"
  :properties ((evflag t)))
(defmvar $dontfactor '((mlist))
  "A list of variables with respect to which factoring is not to occur.")
(defmvar $norepeat t)
(defmvar $ratweights '((mlist simp))
  "The list of weights assigned by 'ratweight'."
  :properties ((assign
		#'(lambda (name val)
		    (cond ((not ($listp val))
			   (mseterr name val "must be a list"))
			  ((null (cdr val))
			   (kill1 '$ratweights))
			  (t
			   (apply #'$ratweight (cdr val))))))))

(defmvar $algebraic nil
  "Set to 'true' in order for the simplification of algebraic integers
  to take effect."
  :properties ((evflag t)))

(defmvar $ratfac nil
  "If `t' cre-forms are kept factored"
  :properties ((evflag t)
	       (assign #'(lambda (name val)
			   (declare (ignore name))
			   (when (and val $ratwtlvl)
			       (merror (intl:gettext "assignment: 'ratfac' and 'ratwtlvl' may not both be used at the same time.")))))))

(defmvar $ratvars '((mlist simp))
  "A list of the arguments of the function 'ratvars' when it was called
  most recently.  Each call to the function 'ratvars' resets the
  list."
  :properties ((assign #'(lambda (name val)
			   (if ($listp val)
			       (apply #'$ratvars (cdr val))
			       (mseterr name val "must be a list"))))))

(defmvar $facexpand t
  "Controls whether the irreducible factors returned by 'factor' are in
  expanded (the default) or recursive (normal CRE) form.")

(defmvar genvar nil
  "List of gensyms used to point to kernels from within polynomials.
  The values cell and property lists of these symbols are used to
  store various information.")

(defmvar genpairs nil)

(defmvar *fnewvarsw nil)
(defmvar *ratweights nil)

(defmvar tellratlist nil)

(defmvar adn* 1
  "common denom for algebraic coefficients")

;;  Any program which calls RATF on
;;  a floating point number but does not wish to see "RAT replaced ..."
;;  message, must bind $RATPRINT to NIL.

(defmvar $ratprint t
  "When true, a message informing the user of the conversion of floating
  point numbers to rational numbers is displayed.")

(defmvar $ratepsilon 2d-15
  "The tolerance used in the conversion of floating point numbers to
  rational numbers, when the option variable 'bftorat' has the value
  'false'.")

;; IF $RATEXPAND IS TRUE, (X+1)*(Y+1) WILL DISPLAY AS
;; XY + Y + X + 1  OTHERWISE, AS (X+1)Y + X + 1
(defmvar $ratexpand nil
  "Controls some simplifications of radicals.  See user manual for
  complicated rules.")

(defmvar varlist nil
  "List of kernels")

;;------------------------------------------------------------------------
;; From result.lisp
(defmvar $resultant '$subres
  "Designates which resultant algorithm")

;;------------------------------------------------------------------------
;; From risch.lisp
(defmvar $liflag t
  "Controls whether `risch' generates polylogs")

;;------------------------------------------------------------------------
;; From simp.lisp
;; General purpose simplification and conversion switches.

(defmvar $negdistrib t
  "Causes negations to be distributed over sums, e.g. -(A+B) is
  simplified to -A-B.")

(defmvar $numer nil
  "Causes SOME mathematical functions (including exponentiation) with
  numerical arguments to be evaluated in floating point.  It causes
  variables in an expression which have been given NUMERVALs to be
  replaced by their values.  It also turns on the FLOAT switch."
  see-also ($numerval $float)
  :properties ((assign 'numerset)))

(defmvar $simp t
  "Enables simplification."
  :properties ((evflag t)))

(defmvar $sumexpand nil
  "If TRUE, products of sums and exponentiated sums go into nested
  sums."
  :properties ((evflag t)))

(defmvar $numer_pbranch nil
  "When true and the exponent is a floating point number or the option
  variable 'numer' is 'true' too, Maxima evaluates the numerical
  result using the principal branch.  Otherwise a simplified, but not
  an evaluated result is returned."
  :properties ((evflag t)))

;; Switches dealing with expansion.
(defmvar $expop 0
  "The largest positive exponent which will be automatically
  expanded.  (X+1)^3 will be automatically expanded if EXPOP is
  greater than or equal to 3."
  fixnum
  see-also ($expon $maxposex $expand))

(defmvar $expon 0
  "The largest negative exponent which will be automatically
  expanded.  (X+1)^(-3) will be automatically expanded if EXPON is
  greater than or equal to 3."
  fixnum
  see-also ($expop $maxnegex $expand))

(defmvar $maxposex 1000.
  "The largest positive exponent which will be expanded by the EXPAND
  command."
  fixnum
  see-also ($maxnegex $expop $expand)
  ;; Check assignment to be a positive integer
  :properties ((assign 'posintegerset)))

(defmvar $maxnegex 1000.
  "The largest negative exponent which will be expanded by the EXPAND
  command."
  fixnum
  see-also ($maxposex $expon $expand)
  ;; Check assignment to be a positive integer
  :properties ((assign 'posintegerset)))

;; Lisp level variables
(defmvar dosimp nil
  "Causes SIMP flags to be ignored.  $EXPAND works by binding $EXPOP to
  $MAXPOSEX, $EXPON to $MAXNEGEX, and DOSIMP to T.")


(defmvar errorsw nil
  "Causes a throw to the tag ERRORSW when certain errors occur rather
  than the printing of a message.  Kludgy MAXIMA-SUBSTITUTE for
  MAXIMA-ERROR signalling.")

(defmvar $rootsepsilon 1d-7
  "The tolerance which establishes the confidence interval for the
  roots found by the 'realroots' function.")
(defmvar $algepsilon 100000000)
(defmvar $true t)
(defmvar $false nil)
(defmvar $logabs nil
  "When true, indefinite integration where logs are generated,
  e.g. 'integrate(1/x,x) produces answers in terms of log(...) instead
  of log(abs(...))." 
  :properties ((evflag t)))
(defmvar $listarith t
  "If 'false' causes any arithmetic operations with lists to be
  suppressed; when 'true', list-matrix operations are contagious
  causing lists to be converted to matrices yielding a result which is
  always a matrix."
  :properties ((evflag t)))
(defmvar $domain '$real)
(defmvar $m1pbranch nil
  "When true, the principal branch for -1 to a power is returned,
  depending on whether 'domain' is real or complex."
  :properties ((evflag t)))
(defmvar $%e_to_numlog nil
  "When 'true', 'r' some rational number, and 'x' some
  expression,'%e^(r*log(x))' will be simplified into 'x^r'.")
(defmvar $%emode t
  "When '%emode' is 'true', '%e^(%pi %i x)' is simplified. See the user
  manual for more details."
  :properties ((evflag t)))
(defmvar $ratsimpexpons nil
  "When true, 'ratsimp' is applied to the exponents of expressions
  during simplification."
  :properties ((evflag t)))
(defmvar $logexpand t ; Possible values are T, $ALL and $SUPER
  "Controls how logs are expanded.  See the user manual."
  :properties ((evflag t))) 
(defmvar $radexpand t
  "Controls some simplification of radicals.  See the user manual."
  :properties ((evflag t))
  :setting-predicate #'(lambda (val)
			 ;; $radexpand can only be set to $all, $true,
			 ;; or $false.
			 (values (member val '($all t nil))
				 "must be one of all, true, or false")))
(defmvar $subnumsimp nil
  "When true, the functions 'subst' and 'psubst' can substitute a
  subscripted variable 'f[x]' with a number, when only the symbol 'f'
  is given.")
(defmvar $logsimp t
  "If 'false' then no simplification of '%e' to a power containing
  'log''s is done.")

(defvar rischp nil)
(defvar rp-polylogp nil)
(defvar wflag nil)
(defvar derivflag nil)
(defvar *zexptsimp? nil)

;; This is initialized in initialize-runtime-globals
(defvar %e-val)
;;------------------------------------------------------------------------
;; From solve.lisp
(defmvar $breakup t
  "Causes solutions to cubic and quartic equations to be expressed in
  terms of common subexpressions.")

(defmvar $multiplicities '$not_set_yet
  "Set to a list of the multiplicities of the individual solutions
  returned by SOLVE, REALROOTS, or ALLROOTS.")

(defmvar $programmode t
  "Causes SOLVE to return its answers explicitly as elements in a list
  rather than printing E-labels."
  :properties ((evflag t)))

(defmvar $solvefactors t
  "If T, then SOLVE will try to factor the expression.  The FALSE
  setting may be desired in zl-SOME cases where factoring is not
  necessary.")

(defmvar $solvetrigwarn t
  "Causes SOLVE to print a warning message when it is uses inverse
  trigonometric functions to solve an equation, thereby losing
  solutions.")

(defmvar $solveradcan nil
  "SOLVE will use RADCAN which will make SOLVE slower but will allow
  certain problems containing exponentials and logs to be solved.")


;;------------------------------------------------------------------------
;; From sumcon.lisp
(defmvar $niceindicespref '((mlist simp) $i $j $k $l $m $n)
  "The list from which 'niceindices' takes the names of indices for sums
  and properties."
  :properties ((assign #'(lambda (name val)
			   ;; The value must be a nonempty list
			   ;; consisting of symbols.  While
			   ;; niceindices can handle subscripted
			   ;; variables, sum and product cannot, so
			   ;; for now we'll restrict the possible
			   ;; values to be simple symbols.
			   (unless (and ($listp val)
					(not ($emptyp val))
					(every '$symbolp (cdr val)))
			     (merror
			      (intl:gettext "~M: value must be a nonempty list of symbols; found: ~:M")
			      name val)))
		       )))

;;------------------------------------------------------------------------
;; From suprv1.lisp
(defmvar $loadprint nil
  "Controls whether to print a message when a file is loaded.")
(defmvar $nolabels nil
  "When 'true', input and output result labels ('%i' and '%o',
  respectively) are displayed, but the labels are not bound to
  results, and the labels are not appended to the 'labels' list.
  Since labels are not bound to results, garbage collection can
  recover the memory taken up by the results.")

;; For each of the variables below ($aliases to $dependencies), create
;; a new list as the initial value instead of '((mlist simp)).  Ecl
;; can and will coalesce these to be the same list, and this causes
;; problems if one of the variables is destructively modified in
;; place.
(defmvar $aliases (list '(mlist simp))
  "The list of atoms which have a user defined alias (set up by the
  'alias', 'ordergreat', 'orderless' functions or by declaring the
  atom a 'noun' with 'declare'."
  no-reset
  :properties ((assign 'neverset)))

;; Define $infolist variables here and set up the initial value and
;; properties.
(defmvar $labels (list '(mlist simp))
  "The list of input, output, and intermediate expression labels,
  including all previous labels if 'inchar', 'outchar', or 'linechar'
  were redefined."
  :properties ((assign 'neverset)))

(defmvar $values (list '(mlist simp))
  "The list of all bound user variables (not Maxima options or
  switches).  The list comprises symbols bound by ':', or '::'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $functions (list '(mlist simp))
  "The list of ordinary Maxima functions in the current session.  An
  ordinary function is a function constructed by 'define' or ':=' and
  called with parentheses '()'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $macros (list '(mlist simp))
  "The list of user-defined macro functions.  The macro function
  definition operator '::=' puts a new macro function onto this list."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $arrays (list '(mlist simp))
  "The list of arrays that have been allocated.  These comprise arrays
  declared by 'array', 'hashed arrays' that can be constructed by
  implicit definition (assigning something to an element that isn't
  yet declared as a list or an array), and 'memoizing functions'
  defined by ':=' and 'define'.  Arrays defined by 'make_array' are
  not included."
  no-reset
  :properties ((assign 'neverset)))


(defmvar $myoptions (list '(mlist simp))
  "The list of all options ever reset by the user, whether or not they
  get reset to their default value."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $props (list '(mlist simp))
  "The list of atoms which have any property other than those explicitly
  mentioned in 'infolists', such as specified by 'atvalue',
  'matchdeclare', etc., as well as properties specified in the
  'declare' function."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $rules (list '(mlist simp))
  nil
  no-reset
  :properties ((assign 'neverset)))

(defmvar $gradefs (list '(mlist simp))
  "The list of the functions for which partial derivatives have been
  defined by 'gradef'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $dependencies (list '(mlist simp))
  "The list of atoms which have functional dependencies, assigned by
  'depends', the function 'dependencies', or 'gradef'.  The
  'dependencies' list is cumulative: each call to 'depends',
  'dependencies', or 'gradef' appends additional items."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $let_rule_packages '((mlist) $default_let_rule_package)
  "The names of the various let rule simplification packages"
  :properties ((assign 'let-rule-setter)))

(defmvar $infolists
  '((mlist simp) $labels $values $functions $macros $arrays
                 $myoptions $props $aliases $rules $gradefs
                 $dependencies $let_rule_packages $structures)
  "The list of the names of all of the information lists in Maxima."
  :properties ((assign 'neverset)))

(defmvar $dispflag t
  "If set to 'false' within a 'block' will inhibit the display of output
  generated by the solve functions called from within the 'block'.")

(defmvar $% '$%
  "The last out-line computed, corresponds to lisp *"
  no-reset)

(defmvar $%% '$%%
  "In compound statements, namely 'block', 'lambda', or '(<s_1>,
  ...,<s_n>)', '%%' is the value of the previous statement."
  no-reset)

(defmvar $inchar '$%i
  "The alphabetic prefix of the names of expressions typed by the user.")

(defmvar $outchar '$%o
  "The alphabetic prefix of the names of expressions returned by the
  system.")

(defmvar $linechar '$%t
  "The alphabetic prefix of the names of intermediate displayed
  expressions.")

(defmvar $linenum 1
  "The line number of the last expression."
  fixnum no-reset)

(defmvar $file_output_append nil
  "Flag to tell file-writing functions whether to append or clobber the
  output file.")

(defvar *refchkl* nil)
(defvar *mdebug* nil)
(defvar errcatch nil)
(defvar lessorder nil)
(defvar greatorder nil)
(defvar *in-translate-file* nil)
(defvar *linelabel* nil)

(defvar *builtin-numeric-constants* '($%e $%pi $%phi $%gamma))
;;------------------------------------------------------------------------
;; From trigi.lisp
(defmvar $%piargs t
  "When true, trigonometric functions are simplified to algebraic
  constants when the argument is an integer multiple of %pi, %pi/2,
  %pi/3, %pi/4, or %pi/6.")
(defmvar $%iargs t
  "When true, trigonometric functions are simplified to hyperbolic
  functions when the argument is apparently a multiple of the
  imaginary unit %i.")
(defmvar $triginverses t
  "Controls the simplification of the composition of trigonometric and
  hyperbolic functions with their inverse functions.")
(defmvar $trigexpand nil
  "If 'true' causes expansion of all expressions containing sin's and
  cos's occurring subsequently."
  :properties ((evflag t)))
(defmvar $trigexpandplus t
  "Controls the \"sum\" rule for 'trigexpand', expansion of sums (e.g.
  'sin(x + y)') will take place only if 'trigexpandplus' is 'true'.")
(defmvar $trigexpandtimes t
  "Controls the \"product\" rule for 'trigexpand', expansion of
  products (e.g.  'sin(2 x)') will take place only if
  'trigexpandtimes' is 'true'.")
(defmvar $trigsign t
  "When true, permits simplification of negative arguments to
  trigonometric functions.")
(defmvar $exponentialize nil
  "When true, all circular and hyperbolic functions are converted to
  exponential form."
  :properties ((evflag t)))
(defmvar $logarc nil
  "When true, inverse circular and hyperbolic functions are replaced by
  equivalent logarithmic functions."
  :properties ((evflag t)))
(defmvar $halfangles nil
  "When true, trigonometric functions of arguments '<expr>/2' are
  simplified to functions of <expr>."
  :properties ((evflag t)))

;; Simplified shortcuts for constant expressions.
(defvar %pi//4 '((mtimes simp) ((rat simp) 1 4.) $%pi))
(defvar %pi//2 '((mtimes simp) ((rat simp) 1 2) $%pi))
(defvar sqrt3//2 '((mtimes simp)
                   ((rat simp) 1 2) 
                   ((mexpt simp) 3 ((rat simp) 1 2))))
(defvar -sqrt3//2 '((mtimes simp)
                    ((rat simp) -1 2)
                    ((mexpt simp) 3 ((rat simp) 1 2))))

;; Build hash tables '*flonum-op*' and '*big-float-op*' that map Maxima
;; function names to their corresponding Lisp functions.

(defvar *flonum-op* (make-hash-table :size 64)
  "Hash table mapping a maxima function to a corresponding Lisp function
  to evaluate the maxima function numerically with flonum precision.")

(defvar *big-float-op* (make-hash-table)
  "Hash table mapping a maxima function to a corresponding Lisp function
  to evaluate the maxima function numerically with big-float
  precision.")
  
;;------------------------------------------------------------------------
;; From init-cl.lisp
(defvar $file_search_lisp nil
  "Directories to search for Lisp source code.")

(defvar $file_search_maxima nil
  "Directories to search for Maxima source code.")

(defvar $file_search_demo nil
  "Directories to search for demos.")

(defvar $file_search_usage nil)

(defvar $file_search_tests nil
  "Directories to search for maxima test suite")

(defvar *maxima-prefix*)
(defvar *maxima-infodir*)
(defvar *maxima-htmldir*)
(defvar *maxima-userdir*)
(defvar *maxima-initmac* "maxima-init.mac"
  "Default maxima mac init file if none specified by the user.  This
  file is looked for only in the maxima userdir.")
(defvar *maxima-initlisp* "maxima-init.lisp"
  "Default maxima lisp init file if none specified by the user.  This
  file is looked for only in the maxima userdir")
(defvar *maxima-load-init-files* t
  "When non-NIL, the init files are not loaded.")
(defvar *maxima-tempdir*)
(defvar *maxima-lang-subdir* nil)
(defvar $maxima_frontend nil
  "The frontend maxima is used with.")
(defvar $maxima_frontend_version nil
  "The version of the maxima frontend.")
(defvar $maxima_frontend_bugreportinfo nil
  "The bug report info the maxima frontend comes with.")

;; A list of temporary files that can be deleted on leaving maxima
(defvar *temp-files-list* (make-hash-table :test 'equal))

;;------------------------------------------------------------------------
;; From macdes.lisp
(defmvar $browser "firefox '~a'"
  "Browser to use for displaying the documentation.  This may be
  initialized on startup to an OS-specific value.  It must contain
  exactly one ~a which will be replaced by the url.")

(defmvar $url_base "localhost:8080"
  "Base URL where the HTML doc may be found.  This can be a file path
  like \"file:///<path>\" or a web server like \"localhost:8080\" or
  some other web server.

  This may be initialized on startup to a file path where the html
  files can be found.")

(defmvar $output_format_for_help '$text
  "The output format for help.  It should be one of the values '$text,
  '$html, '$frontend.  The default is '$text which causes the help to
  be sent to the terminal as plain text.  '$html opens a browser to
  the page for the help.  '$frontend assumes that some frontend will
  provide the necessary function to display help appropriately for the
  frontend."
  :properties ((assign 'set-output-format-for-help)))

(defvar *help-display-function*
  'display-text-topics
  "A symbol naming the function used to display help, as determined
  from $output_format_for_help.")

;;------------------------------------------------------------------------
(defmvar $maxima_userdir nil
  "Names a directory which Maxima searches to find Maxima and Lisp
  files."
  ;; $maxima_userdir is aliased to *maxima-userdir* so that when one
  ;; changes, the other does too.
  :properties ((assign 'shadow-string-assignment)
	       (lisp-shadow '*maxima-userdir*)))

(defmvar $maxima_tempdir nil
  "Names the directory in which Maxima creates some temporary files."
  :properties ((assign 'shadow-string-assignment)
	       (lisp-shadow '*maxima-tempdir*)))

(defmvar $maxima_objdir nil
  "Names the directory where fasl files are written to."
  :properties ((assign 'shadow-string-assignment)
	       (lisp-shadow '*maxima-objdir*)))
;;------------------------------------------------------------------------
;; From nisimp
(defmvar $default_let_rule_package '$default_let_rule_package
  "The name of the default rule package used by `let' and `letsimp'"
  :properties ((assign 'let-rule-setter)))

(defmvar $current_let_rule_package '$default_let_rule_package
  "The name of the current rule package used by `let' and `letsimp'"
  :properties ((assign 'let-rule-setter)))

;;------------------------------------------------------------------------
;; From factor.lisp
(defmvar $nalgfac t
  "If t use bmt's algebraic factoring algorithm")

(defmvar gauss nil)
(defmvar *min* nil)
(defmvar *mx* nil)
(defmvar minpoly* nil)
(defmvar mplc* nil)
(defmvar mm* 1)

;;------------------------------------------------------------------------
;; From nparse.lisp
(defvar *alphabet* (list #\_ #\%))

;;------------------------------------------------------------------------
;; From ellipt.lisp
(defvar 3//2 '((rat simp) 3 2))
(defvar 1//2 '((rat simp) 1 2))
(defvar -1//2 '((rat simp) -1 2))

;;------------------------------------------------------------------------
;; From rpart.lisp

;; generate-atan2 is set to nil when doing integration to avoid
;; generating discontinuities that defint can't handle.
(defmvar generate-atan2 t
  "Controls whether RPART will generate ATAN's or ATAN2's, default is to
  make ATAN2's")

(defmvar implicit-real nil
  "If t RPART assumes radicals and logs of real quantities are real and
  doesn't ask sign questions")

;;------------------------------------------------------------------------
;; From ifactor.lisp
(defmvar *alpha* nil)
(defvar *small-primes*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
    101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181
    191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277
    281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383
    389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487
    491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601
    607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709
    719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827
    829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947
    953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049
    1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151
    1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249
    1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361
    1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459
    1471 1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559
    1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637 1657
    1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759
    1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877
    1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993 1997
    1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089
    2099 2111 2113 2129 2131 2137 2141 2143 2153 2161 2179 2203 2207 2213
    2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311
    2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411
    2417 2423 2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543
    2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663
    2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741
    2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851
    2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969
    2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083 3089
    3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217 3221
    3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329 3331
    3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433 3449 3457 3461
    3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557
    3559 3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671
    3673 3677 3691 3697 3701 3709 3719 3727 3733 3739 3761 3767 3769 3779
    3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907
    3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013
    4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099 4111 4127 4129
    4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243
    4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363
    4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493
    4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621
    4637 4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721 4723 4729
    4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871
    4877 4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973
    4987 4993 4999 5003 5009 5011 5021 5023 5039 5051 5059 5077 5081 5087
    5099 5101 5107 5113 5119 5147 5153 5167 5171 5179 5189 5197 5209 5227
    5231 5233 5237 5261 5273 5279 5281 5297 5303 5309 5323 5333 5347 5351
    5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441 5443 5449 5471
    5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573
    5581 5591 5623 5639 5641 5647 5651 5653 5657 5659 5669 5683 5689 5693
    5701 5711 5717 5737 5741 5743 5749 5779 5783 5791 5801 5807 5813 5821
    5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923
    5927 5939 5953 5981 5987 6007 6011 6029 6037 6043 6047 6053 6067 6073
    6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197 6199
    6203 6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301
    6311 6317 6323 6329 6337 6343 6353 6359 6361 6367 6373 6379 6389 6397
    6421 6427 6449 6451 6469 6473 6481 6491 6521 6529 6547 6551 6553 6563
    6569 6571 6577 6581 6599 6607 6619 6637 6653 6659 6661 6673 6679 6689
    6691 6701 6703 6709 6719 6733 6737 6761 6763 6779 6781 6791 6793 6803
    6823 6827 6829 6833 6841 6857 6863 6869 6871 6883 6899 6907 6911 6917
    6947 6949 6959 6961 6967 6971 6977 6983 6991 6997 7001 7013 7019 7027
    7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177 7187
    7193 7207 7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309
    7321 7331 7333 7349 7351 7369 7393 7411 7417 7433 7451 7457 7459 7477
    7481 7487 7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561
    7573 7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681
    7687 7691 7699 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793 7817
    7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933
    7937 7949 7951 7963 7993 8009 8011 8017 8039 8053 8059 8069 8081 8087
    8089 8093 8101 8111 8117 8123 8147 8161 8167 8171 8179 8191 8209 8219
    8221 8231 8233 8237 8243 8263 8269 8273 8287 8291 8293 8297 8311 8317
    8329 8353 8363 8369 8377 8387 8389 8419 8423 8429 8431 8443 8447 8461
    8467 8501 8513 8521 8527 8537 8539 8543 8563 8573 8581 8597 8599 8609
    8623 8627 8629 8641 8647 8663 8669 8677 8681 8689 8693 8699 8707 8713
    8719 8731 8737 8741 8747 8753 8761 8779 8783 8803 8807 8819 8821 8831
    8837 8839 8849 8861 8863 8867 8887 8893 8923 8929 8933 8941 8951 8963
    8969 8971 8999 9001 9007 9011 9013 9029 9041 9043 9049 9059 9067 9091
    9103 9109 9127 9133 9137 9151 9157 9161 9173 9181 9187 9199 9203 9209
    9221 9227 9239 9241 9257 9277 9281 9283 9293 9311 9319 9323 9337 9341
    9343 9349 9371 9377 9391 9397 9403 9413 9419 9421 9431 9433 9437 9439
    9461 9463 9467 9473 9479 9491 9497 9511 9521 9533 9539 9547 9551 9587
    9601 9613 9619 9623 9629 9631 9643 9649 9661 9677 9679 9689 9697 9719
    9721 9733 9739 9743 9749 9767 9769 9781 9787 9791 9803 9811 9817 9829
    9833 9839 9851 9857 9859 9871 9883 9887 9901 9907 9923 9929 9931 9941
    9949 9967 9973)
  "List of small primes")

;;------------------------------------------------------------------------
;; From rat3a.lisp
;; Global variables referenced throughout the rational function package.

(defmvar modulus nil
  "Global switch for doing modular arithmetic"
  :setting-predicate
  #'(lambda (val)
      ;; The modulus must be $false, or a positive integer.  If the
      ;; value is a positive integer, print a warning if it is not
      ;; prime.
      (or (null val)
	  (and (integerp val) (plusp val)
	       (prog1 t
		 (unless (primep val)
		   (mtell
		    (intl:gettext "Warning: assigning ~:M, a non-prime, to 'modulus'~&")
		    val)))))))

;; $modulus is aliased to modulus and vice-versa.  Setting one, sets
;; the other to the corresponding value.  MODULUS is used in Lisp
;; code; $MODULUS isn't and appears to be the Maxima interface to
;; MODULUS.
;;
;; FIXME: We should probably replace MODULUS with $MODULUS in the lisp
;; code and remove these aliases.
(putprop '$modulus 'modulus 'alias)
(putprop 'modulus '$modulus 'reversealias)

;;------------------------------------------------------------------------
;; From nrat4.lisp
(defvar radcanp nil)

;;------------------------------------------------------------------------
;; From specfn.lisp
;;
;; For these variables, specfn.lisp doesn't explicitly make them
;; defvars, but does set the symbol value for these and declares them
;; special.  Since these are user-visible vars defined in the manual,
;; let's make it explicit.
(defmvar $maxpsiposint 20
  "The largest positive value for which 'psi[n](x)'will try to compute
  an exact value.")

(defmvar $maxpsinegint -10
  "The most negative value for which 'psi[n](x)' will try to compute an
  exact value.  That is if <x> is less than 'maxnegint', 'psi[n](<x>)'
  will not return simplified answer, even if it could.")

(defmvar $maxpsifracnum 6
  "Let <x> be a rational number less than one of the form 'p/q'.  If 'p'
  is greater than 'maxpsifracnum', then 'psi[<n>](<x>)' will not try
  to return a simplified value.")

(defmvar $maxpsifracdenom 6
  "Let <x> be a rational number less than one of the form 'p/q'.  If 'q'
  is greater than 'maxpsifracdenom', then 'psi[<n>](<x>)' will not try
  to return a simplified value.")

;;------------------------------------------------------------------------
;; From hypgeo.lisp
(defvar *par* nil
  "Parameter of Laplace transform.")

(defvar *checkcoefsignlist*)
;;------------------------------------------------------------------------
;; Miscellaneous vars.
(defvar nn*)
(defvar dn*)
(defvar *n)
(defvar derivlist)
(defvar opers-list)

;;------------------------------------------------------------------------
(defvar *bindtest-deprecation-messages* '()
  "An alist whose key is a symbol and datum is a cons of a string to be
  used with bindtest and the value of the variable.  The string should
  contain exactly ~M which will be replaced by the variable in the
  error message.  This is useful for printing a deprecation message
  for any symbol.")
