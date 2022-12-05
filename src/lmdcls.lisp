;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 8 -*- ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *macro-file* nil)

#+gcl
(progn 
  (system::clines "object MAKE_UNSPECIAL(object x) {if (type_of(x)==t_symbol) x->s.s_stype=0;return Cnil;}")
  (system::defentry make-unspecial (system::object) (system::object "MAKE_UNSPECIAL")))

#+(or scl cmu)
(defun make-unspecial (symbol)
  (ext:clear-info variable c::kind symbol)
  symbol)


(defmacro declare-top (&rest decl-specs)
  `(eval-when
    ,(cond (*macro-file*  #+gcl '(compile eval load)
			  #-gcl '(:compile-toplevel :load-toplevel :execute) )
	   (t #+gcl '(eval compile) #-gcl '(:compile-toplevel :execute)))
    ,@(loop for v in decl-specs
	     unless (member (car v) '(special unspecial)) nconc nil
	     else
	     when (eql (car v) 'unspecial)
	     collect `(progn
		       ,@(loop for w in (cdr v)
				collect #-(or gcl scl cmu ecl)
                                       `(remprop ',w
						 #-excl 'special
						 #+excl 'excl::.globally-special.)
				#+(or gcl scl cmu ecl)
			        `(make-unspecial ',w)))
	     else collect `(proclaim ',v))))

;;; This list should contain all specials required by runtime or more than one maxima file,
;;; except for some specials declared in the macro files, eg displm

(declaim (special
	  $%% 
	  $arrays
	  $beta_args_sum_to_integer $boxchar
	  $compgrind
	  $display_format_internal $domxexpt
	  $domxplus $domxtimes
	  $doscmxplus
	  $errexp $error_size $error_syms
	  $exptisolate
	  $file_search
	  $floatformat $floatfrac $floatint
	  $floatoptions $floatprec $floatwidth $fortfloat $fortindent
	  $fortspaces $functions $gammalim
	  $homog_hack
	  $isolate_wrt_times
	  $lmxchar $logconcoeffp
	  $macroexpansion $macros $mapprint
	  $maxapplydepth $maxapplyheight
	  $maxtayorder $mode_checkp $mode_check_errorp $mode_check_warnp
	  $mx0simp $myoptions $nalgfac
	  $negsumdispflag
	  $pointbound
	  $props
	  $ratdenomdivide
	  $realonly $rmxchar
	  $rules
	  $sublis_apply_lambda
	  $taylor_logexpand
	  $taylor_truncate_polynomials $timer $timer_devalue
	  $trace_break_arg $trace_max_indent
	  $trace_safety
	  $tr_array_as_ref $tr_bound_function_applyp
	  $tr_file_tty_messagesp $tr_float_can_branch_complex
	  $tr_function_call_default $tr_numer
	  $tr_optimize_max_loop
	  $tr_state_vars
	  $tr_true_name_of_file_being_translated $tr_warn_bad_function_calls
	  $tr_warn_fexpr $tr_warn_meval $tr_warn_mode $tr_warn_undeclared
	  $tr_warn_undefined_variable
	  $values $vect_cross
	  %e-val i %pi-val *$any-modes*
	  *alpha *const* *in *in-compile*
	  *inv* *min* *mx*
	  *n *out *tr-warn-break* *transl-backtrace*
	  *transl-debug* *warned-fexprs*
	  *warned-mode-vars* *warned-un-declared-vars*
	  aexprp algnotexact
	  alpha assigns atvars
	  bfhalf bfmhalf bigfloat%e bigfloat%pi bigfloatone bigfloatzero
	  defined_variables defintdebug derivlist
	  derivsimp displayp dn* dosimp dsksetp dummy-variable-operators
	  errorsw expandflag expandp
	  exptrlsw
	  fr-factor gauss
	  generate-atan2
	  implicit-real inratsimp inside-mprog
	  limit-answers limitp linel
	  local mdop
	  meta-prop-l meta-prop-p minpoly* mlocp mm* modulus *mopl*
	  mplc* mprogp mproplist
	  need-prog? negprods negsums nn*
	  opers-list outargs1 outargs2
	  preserve-direction prods radcanp
	  realonlyratnum return-mode returns rulefcnl
	  rulesw sign-imag-errp simplimplus-problems
	  *small-primes*
	  sums timesinp tr-abort tr-progret tr-unique
	  transl-file translate-time-evalables transp
	  tstack
	  *trunclist
	  $maxtaydiff $psexpand
	  $define_variable
	  ))

(declaim (declaration unspecial))
