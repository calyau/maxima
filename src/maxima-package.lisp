
(or (find-package "SLOOP") (make-package "SLOOP" :use '(LISP)))


(in-package "SLOOP" )
(shadow '(LOOP-FINISH) (find-package "SLOOP"))


(or (find-package "MAXIMA")
    (make-package  "MAXIMA"
			:nicknames '("CL-MACSYMA"   "CL-MAXIMA" "MACSYMA"  )
			 :use '( "LISP" )))

(shadowing-import '(sloop::loop-return sloop::local-finish sloop::loop-finish sloop::sloop) "MAXIMA")

(shadow '(complement continue   tan sinh cosh tanh #+ti file-position ) 'cl-maxima)


;;defined in polyrz
(shadow '(signum ) 'cl-maxima)

;;lmsup
#+lispm
(shadow '(namestring) 'cl-maxima)

;;in transs


#+lispm
(import '(global::array-leader
	  si::arglist
	  global::gc-on
	  global::gc-off
	  global::user-id
	  global::ERROR-RESTART-LOOP
	  global::condition-case 
	  global::compile-flavor-methods
	  global::default-cons-area
	  global::errset
	  global::make-condition
	  si::signal-condition
	  si::set-in-instance
	  si::record-source-file-name
	  #+ti tv::define-user-option-alist
	  #+ti tv::font-char-height ;for plot win
	  #+ti tv::font-char-width ;for plot win
	  #-ti 	  global::define-user-option-alist
	  #-symbolics global::defflavor
	  #-symbolics global::defmethod
	  #-symbolics global::defun-method
	  global::self
	  global::send
	  global::print-herald
	  global::without-interrupts
	  global::current-process
	  global::working-storage-area
	  ) 'cl-maxima)
 
(shadow '(copy xor putprop) 'cl-maxima)
(shadow '(
 ARRAY       ;;"CL-MAXIMA-SOURCE: MAXIMA; COMMAC" ;not a function in common lisp but symbol in the package
 EXP         ;;various files declare this special which is bad since it is in LISP package.
 LET         ;;"CL-MAXIMA-SOURCE: MAXIMA; LET" ;;like let*
 LET*        ;;"CL-MAXIMA-SOURCE: MAXIMA; LET" ;;maxima:let* does destructuring.  
 LISTEN      ;;"CL-MAXIMA-SOURCE: MAXIMA; SUPRV" ;;has trivial definition in suprv (listen any) ==> 0
 SIGNUM      ;;"CL-MAXIMA-SOURCE: MAXIMA; COMMAC" ;same except (cl:signum 1.3)==1.0 not 1 but I think this is ok for macsyma
 ATAN        ;; (zl:atan y x) == (cl:atan y x) + 2 pi if latter is  negative
 ASIN        ;; different for complex numbers
 ACOS 
 ASINH             
 ACOSH
 ATANH
 TANH        ;;"CL-MAXIMA-SOURCE: MAXIMA; TRIGI" ;same could remove from trigi
 COSH        ;;"CL-MAXIMA-SOURCE: MAXIMA; TRIGI" ;same  ditto
 SINH        ;;"CL-MAXIMA-SOURCE: MAXIMA; TRIGI" ;same  ditto
 TAN         ;;"CL-MAXIMA-SOURCE: MAXIMA; TRIGI" ;;same ditto
 ) 'cl-maxima)

;;new definitions in commac to handle narg compat.
(shadow '(arg listify setarg) 'cl-maxima)

;;MANY instances are (if a b &rest c).  I changed a bunch but there were 
;;many more
(shadow 'lisp::IF 'cl-MAXIMA)

#+kcl
(import '(si::modulus si::cmod si::ctimes si::cdifference si::cplus)
   'cl-maxima)

#+(or clisp gcl)
(import '(system::getenv) (find-package "MAXIMA"))
#+gcl
(import '(si::getpid) (find-package "MAXIMA"))

;;get
#+gcl
(import '( si::cleanup si::*info-paths*
	   si::get-instream  si::short-name  si::cleanup
	   si::instream-stream-name si::instream-line si::instream-name
	   si::instream-stream
	   si::stream-name si::complete-prop
	   si::*stream-alist*
	   si::break-call
) "MAXIMA")
#+gcl
(setf (symbol-function 'maxima::newline) (symbol-function 'si::newline))

;;redefined in commac  lucid 2.1 does (functionp 'jiljay)-->t
(if (lisp::functionp 'dotimes) (push :shadow-functionp *features*))
(unless (lisp::functionp 'lisp::functionp)
   (pushnew :shadow-functionp *features*))

#+shadow-functionp
(shadow 'lisp::functionp 'cl-maxima)

;;;REMOVE The following two forms when the kcl reader can read
;;;the most-negative-double-float again.
#+kcl ;bug fix for float not readable:
(progn
(shadow '( most-positive-single-float most-negative-double-float) 'cl-maxima))
#+kcl 
(progn ;bug fix for float not readable:
 (defvar maxima::most-positive-single-float
   (* .1 lisp::most-positive-single-float))
 (defvar maxima::most-negative-double-float
   (* .1 lisp::most-negative-double-float)))

#+(or gcl kcl)
(in-package "SERROR"  :use '( "LISP" "SLOOP"))

(shadow 'lisp::float 'maxima)
#+lispm
(shadow 'lisp::loop 'maxima)
(provide "MAXIMA")

