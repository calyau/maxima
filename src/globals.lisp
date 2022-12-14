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
        maybe-set-props
	maybe-predicate)

    (do ((opts options (rest opts)))
        ((null opts))
      #+nil
      (format t "opts = ~S~%" opts)
      (case (car opts)
        (no-reset
         ;; Don't reset the value
         (setf maybe-reset nil))
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
         (setf maybe-set-props
               (mapcar #'(lambda (o)
                           (destructuring-bind (ind val)
                               o
                             `(putprop ',var ,val ',ind)))
                       (second opts)))
         (setf opts (rest opts)))
	(:setting-predicate
	 ;; A :SETTING-PREDICATE is a function (symbol or lambda) of
	 ;; one arg specifying the value that variable is to be set
	 ;; to.  It should return non-NIL if the value is valid.
	 ;;
	 ;; WARNING: Do not also have a :properties item with an
	 ;; 'assign property.  Currently this takes precedence.
	 (let ((assign-func
		 `#'(lambda (var val)
		     (unless (funcall ,(second opts) val)
			(mseterr var val)))))
	   (setf maybe-predicate
		 `((putprop ',var ,assign-func 'assign))))
	 ;; Skip over the predicate function.
	 (setf opts (rest opts)))
        ((see-also modified-commands setting-list)
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
       ,@maybe-set-props
       ,@maybe-predicate)))

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
  :setting-predicate #'(lambda (x) (or (null x) (and (integerp x) (>= x 0)))))

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

(defvar ps-bmt-disrep t)
(defvar silent-taylor-flag nil
  ;; From comment in hayat.lisp
  "If true indicates that errors will be returned via a throw to
  TAY-ERR")

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

(defvar $gradefs '((mlist simp)))
(defvar $dependencies '((mlist simp)))

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

(defvar $contexts '((mlist) $global)
  "A list of the currently active contexts.")

(defvar $activecontexts '((mlist))
  "A list of the currently activated contexts")

(defvar *complexsign* nil
  "If T, COMPAR works in a complex mode.")

(defmvar $prederror nil
  "When true, an error message is displayed whenever the predicate of an
  'if' statement or an 'is' function fails to evaluate to either
  'true' or 'false'.")
(defmvar limitp)

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
			 (and (fixnump val)
			      (< 0 val 1000001))))
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
			 (and (fixnump val)
			      (>= val 0)
			      (/= val 1))))

(defmvar $maxfpprintprec (ceiling (log (expt 2 (float-digits 1.0)) 10.0))
  "The maximum number of significant digits printed for floats.")

(defmvar $fpprec $maxfpprintprec
  "Number of decimal digits of precision to use when creating new
  bigfloats. One extra decimal digit in actual representation for
  rounding purposes.")

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
  compact format.")

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
(defvar flounbound most-negative-flonum)

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
			 (or ($listp val)
			     (member val '($all t nil))))
  )

;;Function Call stack each element is
;; (fname . bindlist) where bindlist was the value at time of entry.
;; So you can use this to compute what the bindings were at any
;; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

(defmvar $structures '((mlist simp))
  "The list of user-defined structures defined by 'defstruct'."
  no-reset
  :properties ((assign 'neverset)))

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
			   (when (and val (not (fixnump val)))
			     (mseterr name val))
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

(defvar checkfactors () "List of saved factors")

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
			   (mseterr name val))
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
			   (when (and val $ratwtlvl)
			       (merror (intl:gettext "assignment: 'ratfac' and 'ratwtlvl' may not both be used at the same time.")))))))

(defmvar $ratvars '((mlist simp))
  "A list of the arguments of the function 'ratvars' when it was called
  most recently.  Each call to the function 'ratvars' resets the
  list."
  :properties ((assign #'(lambda (name val)
			   (if ($listp val)
			       (apply #'$ratvars (cdr val))
			       (mseterr name val))))))

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

(defmvar $rootsepsilon #+gcl (float 1/10000000) #-gcl 1d-7
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
  :properties ((evflag t)))
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
  ::properties ((assign #'(lambda (name val)
			    ;; The value must be a nonempty list
			    (unless (and ($listp val)
					 (not ($emptyp val)))
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

;; Define $infolists variables here and set up the initial value and
;; properties.
(defmvar $labels (list '(mlist simp))
  "The list of input, output, and intermediate expression labels,
  including all previous labels if 'inchar', 'outchar', or 'linechar'
  were redefined."
  :properties ((assign 'neverset)))

(defmvar $values '((mlist simp))
  "The list of all bound user variables (not Maxima options or
  switches).  The list comprises symbols bound by ':', or '::'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $functions '((mlist simp))
  "The list of ordinary Maxima functions in the current session.  An
  ordinary function is a function constructed by 'define' or ':=' and
  called with parentheses '()'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $macros '((mlist simp))
  "The list of user-defined macro functions.  The macro function
  definition operator '::=' puts a new macro function onto this list."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $arrays '((mlist simp))
  "The list of arrays that have been allocated.  These comprise arrays
  declared by 'array', 'hashed arrays' that can be constructed by
  implicit definition (assigning something to an element that isn't
  yet declared as a list or an array), and 'memoizing functions'
  defined by ':=' and 'define'.  Arrays defined by 'make_array' are
  not included."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $myoptions '((mlist simp))
  "The list of all options ever reset by the user, whether or not they
  get reset to their default value."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $props '((mlist simp))
  "The list of atoms which have any property other than those explicitly
  mentioned in 'infolists', such as specified by 'atvalue',
  'matchdeclare', etc., as well as properties specified in the
  'declare' function."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $aliases '((mlist simp))
  "The list of atoms which have a user defined alias (set up by the
  'alias', 'ordergreat', 'orderless' functions or by declaring the
  atom a 'noun' with 'declare'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $rules '((mlist simp))
  nil
  no-reset
  :properties ((assign 'neverset)))

(defmvar $gradefs '((mlist simp))
  "The list of the functions for which partial derivatives have been
  defined by 'gradef'."
  no-reset
  :properties ((assign 'neverset)))

(defmvar $dependencies '((mlist simp))
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
(defmvar $dispflag t)

(defmvar $% '$%
  "The last out-line computed, corresponds to lisp *"
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
  "the line number of the last expression."
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

;;------------------------------------------------------------------------
;; From trigi.lisp
(defmvar $%piargs t)
(defmvar $%iargs t)
(defmvar $triginverses t)
(defmvar $trigexpand nil
  nil
  :properties ((evflag t)))
(defmvar $trigexpandplus t)
(defmvar $trigexpandtimes t)
(defmvar $trigsign t)
(defmvar $exponentialize nil
  nil
  :properties ((evflag t)))
(defmvar $logarc nil
  nil
  :properties ((evflag t)))
(defmvar $halfangles nil
  nil
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

;; A list of temporary files that can be deleted on leaving maxima
(defvar *temp-files-list* (make-hash-table :test 'equal))

;;------------------------------------------------------------------------
