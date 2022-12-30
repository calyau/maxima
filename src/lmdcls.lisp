;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 8 -*- ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
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

;;; This list should contain all specials required by runtime or more
;;; than one maxima file, except for specials defined in globals.lisp,
;;; or for some specials declared in the macro files, eg displm.

(declaim (special
	  $maperror
	  $maxapplyheight
	  $transrun
	  $tr_array_as_ref $tr_bound_function_applyp
	  $tr_file_tty_messagesp $tr_float_can_branch_complex
	  $tr_function_call_default $tr_numer
	  $tr_optimize_max_loop
	  $tr_state_vars
	  $tr_true_name_of_file_being_translated $tr_warn_bad_function_calls
	  $tr_warn_fexpr $tr_warn_meval $tr_warn_mode $tr_warn_undeclared
	  $tr_warn_undefined_variable $ttyoff
	  *alpha *in *in-compile*
	  *inv* *min* *mx*
	  *n *out *tr-warn-break* *transl-backtrace*
	  *transl-debug* *warned-fexprs*
	  *warned-mode-vars* *warned-un-declared-vars*
	  alpha
	  defined_variables derivlist
	  dn*
	  gauss
	  local
	  meta-prop-l meta-prop-p mfexprp minpoly* mm*
	  mplc* mspeclist
	  need-prog? nn*
	  opers-list outargs1 outargs2
	  return-mode returns
	  rulesw
	  sums tr-abort tr-progret tr-unique
	  transl-file translate-time-evalables
	  tstack
	  ps-bmt-disrep
	  $define_variable))

(declaim (declaration unspecial))
