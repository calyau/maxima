;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

;;; This file contains global vars (defvars/defmvars) that are used in
;;; multiple files.  We gather them all here so that they are
;;; consistently defined across the build and to make the dependencies
;;; easier to track.

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
	((fixnum boolean string)
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

;;; Declare user-visible special variables.

;; From clmacs.lisp for floating-point constants

;; Setup the mapping from the Maxima 'flonum float type to a CL float type.
;;
;; Add :flonum-long to *features* if you want flonum to be a
;; long-float.  Or add :flonum-double-double if you want flonum to
;; be a double-double (currently only for CMUCL).  Otherwise, you
;; get double-float as the flonum type.
;;
;; Default double-float flonum.
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
;; From limit.lisp
(defvar infinities '($inf $minf $infinity)
  "The types of infinities recognized by Maxima. INFINITY is complex
  infinity")

(defvar real-infinities '($inf $minf)
  "The real infinities, `inf' is positive infinity, `minf' negative
  infinity")

(defvar infinitesimals '($zeroa $zerob)
  "The infinitesimals recognized by Maxima. ZEROA zero from above,ZEROB
  zero from below")


;;------------------------------------------------------------------------
;; From trigi.lisp
(defmvar $%iargs t)
(defmvar $%piargs t)
(defmvar $triginverses t)
(defmvar $trigexpand nil)
(defmvar $trigexpandplus t)
(defmvar $trigexpandtimes t)
(defmvar $trigsign t)
(defmvar $exponentialize nil)
(defmvar $logarc nil)
(defmvar $halfangles nil)

;;------------------------------------------------------------------------
;; From suprv1.lisp
(defmvar $disptime nil)
(defmvar $strdisp t)
(defmvar $grind nil)
(defmvar $backtrace '$backtrace)
(defmvar $debugmode nil
  "When 'true', Maxima will start the Maxima debugger when a Maxima
  error occurs"
  :properties ((assign debugmode1)))
(defmvar $poislim 5
  "Determines the domain of the coefficients in the arguments of the
  trig functions."
  :properties ((assign poislim1)))
(defmvar $loadprint nil)
(defmvar $nolabels nil)
(defmvar $aliases '((mlist simp)))
(defmvar $% '$% "The last out-line computed, corresponds to lisp *"
	 no-reset)

;;------------------------------------------------------------------------
;; From mdot.lisp
(defmvar $dotconstrules t
  "Causes a non-commutative product of a constant and another term to
  be simplified to a commutative product.  Turning on this flag
  effectively turns on DOT0SIMP, DOT0NSCSIMP, and DOT1SIMP as well.")

(defmvar $dot0simp t
  "Causes a non-commutative product of zero and a scalar term to be
  simplified to a commutative product.")

(defmvar $dot0nscsimp t
  "Causes a non-commutative product of zero and a nonscalar term to be
  simplified to a commutative product.")

(defmvar $dot1simp t
  "Causes a non-commutative product of one and another term to be
  simplified to a commutative product.")

(defmvar $dotscrules nil
  "Causes a non-commutative product of a scalar and another term to be
  simplified to a commutative product.  Scalars and constants are
  carried to the front of the expression.")

(defmvar $dotdistrib nil
  "Causes every non-commutative product to be expanded each time it is
  simplified, i.e.  A . (B + C) will simplify to A . B + A . C.")

(defmvar $dotexptsimp t
  "Causes A . A to be simplified to A ^^ 2.")

(defmvar $dotassoc t
  "Causes a non-commutative product to be considered associative, so
  that A . (B . C) is simplified to A . B . C.  If this flag is off,
  dot is taken to be right associative, i.e.  A . B . C is simplified
  to A . (B . C)."
  :properties ((assign msetchk)))

(defmvar $doallmxops t
  "Causes all operations relating to matrices (and lists) to be
  carried out.  For example, the product of two matrices will actually
  be computed rather than simply being returned.  Turning on this
  switch effectively turns on the following three.")

(defmvar $domxmxops t
  "Causes matrix-matrix operations to be carried out.")

(defmvar $doscmxops nil
  "Causes scalar-matrix operations to be carried out.")

(defmvar $domxnctimes nil
  "Causes non-commutative products of matrices to be carried out.")

(defmvar $scalarmatrixp t
  "Causes a square matrix of dimension one to be converted to a scalar,
  i.e. its only element.")

(defmvar $dotident 1 "The value to be returned by X^^0.")

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

(defmvar $%rnum 0)

(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS which
  where introduced into the expression. Useful for mapping over and
  using as an argument to SUBST.")

;;------------------------------------------------------------------------
;; From mat.lisp
(defmvar $globalsolve nil)
(defmvar $sparse nil)
(defmvar $backsubst t)
(defmvar $linsolve_params t "`linsolve' generates %Rnums")

;; Probably should be defvar and not defmvar.
(defmvar *rank* nil)
(defmvar *inv* nil)

;;------------------------------------------------------------------------
;; From float.lisp
(defmvar $float2bf t
  "If TRUE, no MAXIMA-ERROR message is printed when a floating point
  number is converted to a bigfloat number.")


(defmvar $bftorat nil
  "Controls the conversion of bigfloat numbers to rational numbers.
  If FALSE, RATEPSILON will be used to control the conversion (this
  results in relatively small rational numbers).  If TRUE, the
  rational number generated will accurately represent the bigfloat.")

(defmvar $bftrunc t
  "If TRUE, printing of bigfloat numbers will truncate trailing
  zeroes.  Otherwise, all trailing zeroes are printed.")

(defmvar $fpprintprec 0
  "Controls the number of significant digits printed for floats.  If
  0, then full precision is used."
  fixnum
  :properties ((assign msetchk)))

(defmvar $maxfpprintprec (ceiling (log (expt 2 (float-digits 1d0)) 10d0))
  "The maximum number of significant digits printed for floats.")

(defmvar $fpprec $maxfpprintprec
  "Number of decimal digits of precision to use when creating new
  bigfloats. One extra decimal digit in actual representation for
  rounding purposes."
  :properties ((assign fpprec1)))

;;------------------------------------------------------------------------
;; From solve.lisp
(defmvar $breakup t
  "Causes solutions to cubic and quartic equations to be expressed in
  terms of common subexpressions.")

(defmvar $multiplicities '$not_set_yet
  "Set to a list of the multiplicities of the individual solutions
  returned by SOLVE, REALROOTS, or ALLROOTS.")

(defmvar $linsolvewarn t
  "Needs to be documented.")

(defmvar $programmode t
  "Causes SOLVE to return its answers explicitly as elements in a list
  rather than printing E-labels.")

(defmvar $solvedecomposes t
  "Causes `solve' to use `polydecomp' in attempting to solve
  polynomials.")

(defmvar $solveexplicit nil
  "Causes `solve' to return implicit solutions i.e. of the form F(x)=0.")

(defmvar $solvefactors t
  "If T, then SOLVE will try to factor the expression.  The FALSE
  setting may be desired in zl-SOME cases where factoring is not
  necessary.")

(defmvar $solvenullwarn t
  "Causes the user will be warned if SOLVE is called with either a
  null equation list or a null variable list.  For example,
  SOLVE([],[]); would print two warning messages and return [].")

(defmvar $solvetrigwarn t
  "Causes SOLVE to print a warning message when it is uses inverse
  trigonometric functions to solve an equation, thereby losing
  solutions.")

(defmvar $solveradcan nil
  "SOLVE will use RADCAN which will make SOLVE slower but will allow
  certain problems containing exponentials and logs to be solved.")


;;------------------------------------------------------------------------
;; From asum.lisp
(defmvar $cauchysum nil
  "When multiplying together sums with INF as their upper limit,
  causes the Cauchy product to be used rather than the usual
  product. In the Cauchy product the index of the inner summation is a
  function of the index of the outer one rather than varying
  independently."
  modified-commands '$sum)

(defmvar $gensumnum 0
  "The numeric suffix used to generate the next variable of summation.
  If it is set to FALSE then the index will consist only of GENINDEX
  with no numeric suffix."
  modified-commands '$sum
  setting-predicate #'(lambda (x) (or (null x) (integerp x)))
  :properties ((assign msetchk)))

(defmvar $genindex '$i
  "The alphabetic prefix used to generate the next variable of
  summation when necessary."
  modified-commands '$sum
  setting-predicate #'symbolp
  :properties ((assign msetchk)))

(defmvar $zerobern t)
(defmvar $simpsum nil)
(defmvar $simpproduct nil)

;; linear operator stuff

(defparameter *opers-list '(($linear . linearize1)))
(defparameter  opers (list '$linear))

;;------------------------------------------------------------------------
;; From compar.lisp
(defmvar $context '$global
  "Whenever a user assumes a new fact, it is placed in the context
  named as the current value of the variable CONTEXT.  Similarly,
  FORGET references the current value of CONTEXT.  To add or DELETE a
  fact from a different context, one must bind CONTEXT to the intended
  context and then perform the desired additions or deletions.  The
  context specified by the value of CONTEXT is automatically
  activated.  All of MACSYMA's built-in relational knowledge is
  contained in the default context GLOBAL.")

(defmvar $contexts '((mlist) $global)
  "A list of the currently active contexts.")

(defmvar $prederror nil)
(defmvar $signbfloat t)
(defmvar $askexp)
(defmvar $assume_pos nil)
(defmvar $assume_pos_pred nil)

;;------------------------------------------------------------------------
;; From csimp.lisp
(defmvar $demoivre nil)
(defmvar $nointegrate nil)
(defmvar $lhospitallim 4)
(defmvar $tlimswitch t)
(defmvar $limsubst nil)

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
  :properties ((assign numerset)))

(defmvar $simp t "Enables simplification.")

(defmvar $sumexpand nil
  "If TRUE, products of sums and exponentiated sums go into nested
  sums.")

(defmvar $numer_pbranch nil)

(defmvar $sumsplitfact t) ;= nil minfactorial is applied after a factocomb.

(defvar rischp nil)
(defvar rp-polylogp nil)
(defvar wflag nil)
(defvar derivflag nil)
(defvar *zexptsimp? nil)

;;------------------------------------------------------------------------
;; From comm.lisp
(defmvar $exptsubst nil)
(defmvar $partswitch nil)
(defmvar $inflag nil)
(defmvar $derivsubst nil)
(defmvar $opsubst t)
(defmvar $gradefs '((mlist simp)))
(defmvar $dependencies '((mlist simp)))

;;------------------------------------------------------------------------
;; From displa.lisp
(defmvar $ttyoff nil)

(defmvar $stardisp nil
  "Causes factors of products to be separated by * when displayed.")

(defmvar $leftjust nil
  "Causes equations to be drawn left justified rather than centered.
  For slow consoles.")

(defmvar $display2d t
  "Causes equations to be drawn in two dimensions.  Otherwise, drawn
  linearly.")

(defmvar $lispdisp nil
  "Causes symbols not having $ as the first character in their pnames
  to be preceded with a ? when displayed.")

(defmvar $derivabbrev nil)

(defmvar $noundisp nil)

(defmvar $stringdisp nil
  "Causes strings to be bracketed in double quotes when displayed.
  Normally this is off, but is turned on when a procedure definition
  is being displayed.")

;; These variables are bound within Macsyma Listeners since they are different
;; for each window.  Set them here, anyway, so that RETRIEVE can be called from
;; top level.  The size of TOP-WINDOW is wired in here.

(defmvar $linel 79.
  "The assumed width (in characters) of the console display for the
  purpose of displaying expressions"
  :properties ((assign msetchk)))

;; DO NOT change this to a DEFMVAR.  This breaks the testsuite,
;; presumably because the values get reset when reset() is done.
(defmvar $activecontexts '((mlist))
  "A list of the currently activated contexts"
  no-reset)


;;------------------------------------------------------------------------
;; From matrix.lisp
(defmvar $detout nil)
(defmvar $ratmx nil)
(defmvar $matrix_element_mult "*")  ;;; Else, most useful when "."
(defmvar $matrix_element_add "+")
(defmvar $matrix_element_transpose nil)

;;------------------------------------------------------------------------
;; From suprv1.lisp
(defmvar $infolists
  '((mlist simp) $labels $values $functions $macros $arrays
                 $myoptions $props $aliases $rules $gradefs
    $dependencies $let_rule_packages $structures)
  "A list of the names of all of the information lists in Maxima."
  :properties ((assign neverset)))

(defmvar $labels (list '(mlist simp)))
(defmvar $dispflag t)

(defmvar $inchar '$%i
  "The alphabetic prefix of the names of expressions typed by the user.")

(defmvar $outchar '$%o
  "The alphabetic prefix of the names of expressions returned by the
  system.")

(defmvar $linechar '$%t
  "The alphabetic prefix of the names of intermediate displayed
  expressions.")

(defmvar $linenum 1 "the line number of the last expression."
	 fixnum no-reset)

(defmvar $file_output_append nil
  "Flag to tell file-writing functions whether to append or clobber
  the output file.")

(defmvar $ratvarswitch t) ; If T, start an evaluation with a fresh list VARLIST.

;;------------------------------------------------------------------------
;; From simp.lisp
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
  :properties ((assign posintegerset)))

(defmvar $maxnegex 1000.
  "The largest negative exponent which will be expanded by the EXPAND
  command."
  fixnum
  see-also ($maxposex $expon $expand)
  ;; Check assignment to be a positive integer
  :properties ((assign posintegerset)))

(defmvar $rootsepsilon #+gcl (float 1/10000000) #-gcl 1e-7)
(defmvar $grindswitch nil)
(defmvar $algepsilon 100000000)
(defmvar $true t)
(defmvar $false nil)
(defmvar $on t)
(defmvar $off nil)
(defmvar $logabs nil)
(defmvar $limitdomain '$complex)
(defmvar $listarith t)
(defmvar $domain '$real)
(defmvar $m1pbranch nil)
(defmvar $%e_to_numlog nil)
(defmvar $%emode t)
(defmvar $lognegint nil)
(defmvar $ratsimpexpons nil)
(defmvar $logexpand t) ; Possible values are T, $ALL and $SUPER
(defmvar $radexpand t)
(defmvar $subnumsimp nil)
(defmvar $logsimp t)
(defmvar $distribute_over t) ; If T, functions are distributed over bags.



;;------------------------------------------------------------------------
;; From rat3e
;; User level global variables.
(defmvar $keepfloat nil  "If `t' floating point coeffs are not converted to rationals")
(defmvar $factorflag nil "If `t' constant factor of polynomial is also factored")
(defmvar $dontfactor '((mlist)))
(defmvar $norepeat t)
(defmvar $ratweights '((mlist simp))
  "The list of weights assigned by 'ratweight"
  :properties ((assign msetchk)))
(defmvar $algebraic nil)
(defmvar $ratvars '((mlist simp))
  "A list of the arguments of the function 'ratvars' when it was called
  most recently.  Each call to the function 'ratvars' resets the list"
  :properties ((assign msetchk)))
(defmvar $facexpand t)
(defmvar $ratfac nil
  "If `t' cre-forms are kept factored"
  :properties ((assign msetchk)))


;;  Any program which calls RATF on
;;  a floating point number but does not wish to see "RAT replaced ..."
;;  message, must bind $RATPRINT to NIL.

(defmvar $ratprint t)

(defmvar $ratepsilon 2d-15)

;; IF $RATEXPAND IS TRUE, (X+1)*(Y+1) WILL DISPLAY AS
;; XY + Y + X + 1  OTHERWISE, AS (X+1)Y + X + 1
(defmvar $ratexpand nil)


;;------------------------------------------------------------------------
;; From merror.lisp
(defmvar $error `((mlist simp) "No error.")
  "During an MAXIMA-ERROR break this is bound to a list
  of the arguments to the call to MAXIMA-ERROR, with the message
  text in a compact format.")

(defmvar $errormsg 't
  "If `false' then no maxima-error message is printed!")

;;------------------------------------------------------------------------
;; From mlisp.lisp
(defmvar $%enumer nil)
(defmvar $float nil)
(defmvar $refcheck nil)
(defmvar $translate nil)
(defmvar $transrun t)
(defmvar $savedef t)
(defmvar $maperror t)
(defmvar $optionset nil)
(defmvar $setcheckbreak nil)
(defmvar $infeval nil)
(defmvar $piece '$piece)
(defmvar $setval '$setval)
(defmvar $setcheck nil
  "This contains a list of variables (which can be subscripted). Maxima
  prints a message whenever the variables, or subscripted occurrences
  of them, are bound with the ordinary assignment operator ':', the
  '::' assignment operator, or function argument binding, but not the
  function assignment ':=' nor the macro assignment '::=' operators"
  :properties ((assign msetchk)))
;; If this is T then arrays are stored in the value cell,
;; whereas if it is false they are stored in the function cell
(defmvar $use_fast_arrays nil)

(defvar mspeclist nil)
(defvar bindlist nil)
(defvar loclist nil)
(defvar *nounl* nil)
(defvar scanmapp nil)
(defvar maplp nil)
(defvar evp nil)
(defvar fmaplvl 0)
(defvar aryp nil)
(defvar msump nil)
(defvar evarrp nil)
(defvar factlist nil)
(defvar mfexprp t)
(defvar *nounsflag* nil)
(defvar transp nil)
(defvar noevalargs nil)
(defvar featurel
  '($integer $noninteger $even $odd $rational $irrational $real $imaginary $complex
    $analytic $increasing $decreasing $oddfun $evenfun $posfun $constant
    $commutative $lassociative $rassociative $symmetric $antisymmetric
    $integervalued))

(defmvar $features (cons '(mlist simp) (append featurel nil)))

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(defvar munbound '|#####|)

(defvar fixunbound most-negative-fixnum)

(defvar flounbound most-negative-flonum)

(defmvar munbindp nil
  "Used for safely `munbind'ing incorrectly-bound variables."
  no-reset)


;;------------------------------------------------------------------------
;; From rat3c.lisp
;; List of GCD algorithms.  Default one is first.
(defvar *gcdl* '($spmod $subres $ez $red $mod $algebraic))

(defmvar $gcd (car *gcdl*) ;Sparse Modular
  "The default GCD algorithm to use for evaluating GCDs."
  :properties ((assign msetchk)))



;;------------------------------------------------------------------------
;; From rat3d.lisp
(defmvar $intfaclim t)
(defmvar $berlefact t)

(defmvar $factor_max_degree 1000
  "If set to an integer n, some potentially large (many factors)
  polynomials of degree > n won't be factored, preventing huge memory
  allocations and stack overflows. Set to zero to deactivate."
  fixnum
  :properties ((assign posintegerset)))

(defmvar $factor_max_degree_print_warning t
  "Print a warning message when a polynomial is not factored because its
  degree is larger than $factor_max_degree?"
  boolean)

(defmvar *irreds nil)
(defmvar algfac* nil)
(defmvar low* nil)

;;------------------------------------------------------------------------
;; From nisimp.lisp
(defmvar $letvarsimp nil)

(defmvar $letrat nil) 

(defmvar $default_let_rule_package '$default_let_rule_package
  "The name of the default rule package used by `let' and `letsimp'"
  :properties ((assign let-rule-setter)))

(defmvar $current_let_rule_package '$default_let_rule_package
  "The name of the current rule package used by `let' and `letsimp'"
  :properties ((assign let-rule-setter)))

(defmvar $let_rule_packages '((mlist) $default_let_rule_package)
  "The names of the various let rule simplification packages"
  :properties ((assign let-rule-setter)))

;;------------------------------------------------------------------------
;; From risch.lisp
(defmvar $liflag t
  "Controls whether `risch' generates polylogs")

(defmvar $erfflag t
  "Controls whether `risch' generates `erfs'")

;;------------------------------------------------------------------------
;; From optim.lisp
(defmvar $optimprefix '$%
  "The prefix used for generated symbols by the 'optimize' command."
  :properties ((assign msetchk)))

;;------------------------------------------------------------------------
;; From nparse.lisp
(defmvar $parsewindow 10.
  "The maximum number of 'lexical tokens' that are printed out on each
  side of the error-point when a syntax (parsing) MAXIMA-ERROR occurs.
  This option is especially useful on slow terminals.  Setting it to
  -1 causes the entire input string to be printed out when an
  MAXIMA-ERROR occurs."
  fixnum
  :properties ((assign msetchk)))

(defmvar *alphabet* (list #\_ #\%))

;;------------------------------------------------------------------------
;; From nforma
(defmvar $powerdisp nil)
(defmvar $pfeformat nil)
(defmvar $%edispflag nil)
(defmvar $exptdispflag t
  "When true, Maxima displays expressions with negative exponents using
  quotients.")
(defmvar $sqrtdispflag t)

;;------------------------------------------------------------------------
;; From rat3b.lisp
(defmvar $ratwtlvl nil
  "Used in combination with the 'ratweight' function to control the
  truncation of canonical rational expressions (CRE)"
  :properties ((assign msetchk)))
(defmvar $ratalgdenom t)       ;If T then denominator is rationalized.

;;------------------------------------------------------------------------
;; From result.lisp
(defmvar $resultant '$subres
  "Designates which resultant algorithm")

;;------------------------------------------------------------------------
;; From comm2.lisp
(defmvar $rootsconmode t)

;;------------------------------------------------------------------------
;; From rat3d.lisp
(defmvar $savefactors nil
  "If t factors of ratreped forms will be saved")

;;------------------------------------------------------------------------
;; From macsys.lisp
(defmvar $showtime nil
  "When T, the computation time is printed with each output
  expression.")

;;; Standard Kinds of Input Prompts

(defmvar $prompt '_
  "Prompt symbol of the demo function, playback, and the Maxima break
  loop.")

(defmvar $_ '$_
  "last thing read in, corresponds to lisp +")
(defmvar $__ '$__
  "thing read in which will be evaluated, corresponds to -")

;;------------------------------------------------------------------------
;; From mtrace.lisp
(defmvar $trace (list '(mlist))
  "List of functions actively traced"
  :properties ((assign neverset)))

;;------------------------------------------------------------------------
;; From inmis.lisp
(defmvar $listconstvars nil
  "Causes LISTOFVARS to include %E, %PI, %I, and any variables declared
  constant in the list it returns if they appear in exp.  The default
  is to omit these."
  boolean see-also $listofvars)

(defmvar $listdummyvars t)

;;------------------------------------------------------------------------
;; From trigi.lisp
;; Simplified shortcuts for constant expressions.
(defvar %pi//4 '((mtimes simp) ((rat simp) 1 4.) $%pi))
(defvar %pi//2 '((mtimes simp) ((rat simp) 1 2) $%pi))
(defvar sqrt3//2 '((mtimes simp)
                   ((rat simp) 1 2) 
                   ((mexpt simp) 3 ((rat simp) 1 2))))
(defvar -sqrt3//2 '((mtimes simp)
                    ((rat simp) -1 2)
                    ((mexpt simp) 3 ((rat simp) 1 2))))

;;------------------------------------------------------------------------
;; From ellipt.lisp
(defvar 3//2 '((rat simp) 3 2))
(defvar 1//2 '((rat simp) 1 2))
(defvar -1//2 '((rat simp) -1 2))

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
