;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Instructions for making a Maxima system.
;; We have called it Maxima so as not to conflict with another system of
;; similar name installed here at UTexas.


(or (member :cl  lisp:*features*) (push :cl lisp:*features*))
#+symbolics
(cond ( (>= (si::get-system-version 'system) 349)
       (push :genera Lisp:*features*)
       (push :zlch lisp:*features*))
      (t (push :pre-genera lisp:*features*)))

#+ti
(cond ((= (si::get-system-version ) 1)
       (push :tirel3 Lisp:*features*)))

;; We are now assuming that #-zetalisp valid. Remove from *features* if necessary!

;;note its compile-sytem and load-sytem in release 7 on symbolics.
;; (MAKE-SYSTEM 'MAXIMA) to load.
;; (MAKE-SYSTEM 'MAXIMA ':BATCH) to load without questions or more processing.
;; (MAKE-SYSTEM 'MAXIMA ':COMPILE) to compile recently changed files and then load.
;; (MAKE-SYSTEM 'MAXIMA ':RECOMPILE) to recompile the entire system and then load.


 
(cond ((find-package "CL-MAXIMA"))
      (t (make-package  'cl-MAXIMA
			:nicknames '(cl-macsyma   maxima macsyma  )
			 :use '( "LISP"))))

;;Many of the things shadowed aren't in most peoples idea of the lisp package
;;but just to be sure.

(SHADOW '(#+TI FIXNUMP
	  #+TI COPY
	  #+TI BIGP
	  #+TI PUTPROP
	    ARGS   ;;unused outside maxima
	    DEF  
	    DISPATCH
	    FETCH  ;;unused outside maxima
	    LET
	    LET* 
	    LISTEN
	    PAIR  ;;unused outside maxima
	    SELECTOR
	    TRUE
	    XOR   ;;unused outside maxima
 	    STORE) 'CL-MAXIMA)

#+ti
(shadow '(*array  arg listify setarg exploden explodec implode ceil
		 signp tyo tyi tyipeek arraycall local-declare
		 cursorpos readlist explode getcharn getchar ascii maknam flatc flatsize   ) 'maxima)

(shadow '(tan sinh cosh tanh ) 'cl-maxima) 

;;defined in polyrz
(shadow '(signum ) 'cl-maxima)

;;lmsup
(shadow '(namestring) 'cl-maxima)

;;in transs
(shadow '(truename) 'cl-maxima)

#+lispm
(import '(
	  global:gc-on global:gc-off
	  global:user-id
	  global:ERROR-RESTART-LOOP
	  global:condition-case 
	  global:compile-flavor-methods
	  global:default-cons-area
	  global:errset
	  global:make-condition
	  global:signal-condition
	  si::record-source-file-name
	  #+ti tv::define-user-option-alist
	  #+ti tv:font-char-height ;for plot win
	  #+ti tv:font-char-width ;for plot win
	  #-ti 	  global:define-user-option-alist
	  #-symbolics global:defflavor
	  #-symbolics global:defmethod
	  #-symbolics global:defun-method
	  global:self
	  global:send
	  global:print-herald
	  global:without-interrupts
	  global:current-process
	  global:working-storage-area
	  ) 'cl-maxima)
 

#+cl
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


;;logical pathname declarations are in sys:site;cl-maxima-source.translations
;;and sys:site;cl-maxima-object.translations
;;And the whole sysdef file (minus the declarations) is pointed to
;;by sys:site; maxima.system so that we should not have to load it

;;Syntax is "Device:directory; filename" and the above should result in the code being looked for
;; in the right place but being compiled and send to new directories"



(defun cl-maxima-compile (file-name &aux ans in out)
 (setq ans  (compile-file (setq in (fs:merge-pathname-defaults  (format nil "Cl-maxima-source:maxima;~A.lisp" file-name)))
			  :output-file
			 (setq out (format nil "Cl-maxima-object:maxima;~A" file-name))))
 (format t "~%Compiled ~A with true name ~A to ~A ~%   with true name ~A" in (send in :truename) ans (send ans :truename)) ans)
(defun cl-maxima-load (file-name)
  (load  (format nil "Cl-maxima-object:maxima;~A" file-name)))

(defmacro defsys (name source-dir obj-dir options-list files-list)
  "The files in FILES-LIST will be compiled and loaded sequentially.
Elements of files-list can be either one file or a list of files.  A
list would be done in parallel.  The list of files may also be of the
form (:module name-of-module (file1 file2 ..)).  
This is equivalent to (file1 file2 ...) The SOURCE-DIR is appended to the
beginning of each file for source and objects go under OBJ-DIR"
  (sloop for v in files-list with module-name
	do (or (listp v) (setf v (list v)))
        (cond ((eql (car v) :module)
	       (setf module-name (second v))
		(setf     v (third v)))
	      (t(setq module-name 	(intern (format nil "~a" (car v))))))
	collecting module-name into module-names
	collecting
	`(:module ,module-name
		  , (sloop for file in v
			  collecting (list (format nil "~a~a" source-dir file)
					   (format nil "~a~a" obj-dir file))))
	into modules
	finally (return
		  `(global:defsystem ,name #+genera ,options-list #+(or ti pre-genera)
			      ,@ options-list
		     ,@ modules
		     #+genera (:serial ,@ module-names)
		     #+(or ti pre-genera)
		     ,@ (sloop for v in module-names
			      with so-far
			      collecting
			      (cond ((setq so-far (copy-list names))
				     `(:compile-load ,v
						     (:fasload ,@ so-far)
						     (:fasload ,@ so-far)))
				    (t `(:compile-load ,v)))
			      into orders
      			      collecting v into names
			      finally (return orders))))))


(DEFSYS cl-maxima "cl-maxima-source:maxima;" "cl-maxima-object:maxima;"
  #+genera
  (:pretty-name "Cl Maxima"
		:default-pathname "cl-maxima-object:maxima;"
		:patchable t)
  #+(or ti pre-genera)
  ((:Name "Interactive Formatting")
   (:patchable "cl-maxima-patch:maxima;"))
  ((:Module declarations
	    (LMDCLS)) 
   (:MODULE DESTRUCTURING-LET
	   (LET))
   (:module compatibility-macros1
	    (clmacs 
	      ))
   (:module compatibility-macros
	    ( commac
	      )) 
   #+symbolics method-compat
   (:MODULE PREREQUISITES
	    (MORMAC 
	      COMPAT))
   (:MODULE FUNDAMENTAL-MACROS
	    (DEFOPT 
	      DEFCAL 
	      MAXMAC ))
   (:MODULE UTILITY-MACROS
	    (MOPERS 
	      MFORMA 
	      DEFCAL 
	      ))
   (:MODULE OTHER-MACROS
	    (MRGMAC 
	      PROCS 
	      RZMAC    
	      STRMAC 
	      DISPLM ))
   (:MODULE RAT-MACROS
	    (RATMAC 
	      MHAYAT ))
   (:MODULE NUMERICAL-MACROS
	    (NUMERM ))
   
;;other   (:MODULE MACROS (
;; OTHER-MACROS RAT-MACROS NUMERICAL-MACROS))
   (:MODULE UTILITIES
	    (OPERS 
	      UTILS 
	      SUMCON 
	      SUBLIS 
	      RUNTIM 
	      MERROR 
	      MFORMT 
	      MUTILS 
	      OUTMIS 
	      AR 
	      MISC ))
   (:MODULE COMMANDS
	    (COMM 
	      COMM2 ))
   (:MODULE EVALUATOR
	    (MLISP 
	      MMACRO 
	      BUILDQ ))
   (:MODULE SIMPLIFICATION
	    (SIMP 
	      FLOAT 
	      CSIMP 
	      CSIMP2 
	      ZERO 
	      LOGARC 
	      RPART ))
   (:MODULE I-O
	    (LMSUP 
	      SYSTEM 
	      MLOAD 
	      SUPRV1 
	      ;; we should really look, *seriously*, at the functionality provided
	      ;; by the DSKFN package. Mainly, it saves and manages environments.
	      DSKFN 
	      ))
   (:MODULE FACTORING
	    (LESFAC 
	      FACTOR 
	      ALGFAC 
	      NALGFA 
;	    NEWFAC    ;Why was this commented out?
	      UFACT 
	      RESULT ))
   (:MODULE RATIONAL-FUNCTIONS
	    (RAT3A 
	      RAT3B 
	      RAT3D 
	      RAT3C 
	      RAT3E 
	      NRAT4 
	      RATOUT ))
   (:MODULE MAXIMA-LANGUAGE-COMPILER-MACROS
	    (TRANSM ))
   (:MODULE MAXIMA-LANGUAGE-COMPILER
	    ;; This includes the translator itself and runtime
	    ;; support needed for translated code.
	    (TRANSL 
	      TRANSS 
	      TRANS1 
	      TRANS2 
	      TRANS3 
	      TRANS4 
	      TRANS5 
	      TRANSF 
	      TROPER 
	      TRUTIL 
	      TRMODE 
	      TRDATA 
	      TRPRED 
	      TRANSQ 
	      ACALL				;
	      FCALL 
	      EVALW 
	      TRPROP 
	      MDEFUN 
	      ))
   (:MODULE NUMERICAL-FUNCTIONS
	    (BESSEL 
	      ELLIPT 
	      NUMER 
	      INTPOL 
	      ROMBRG 
	      ))
   ;;Rest of System
   (:MODULE READER
	    (NPARSE ))
   (:MODULE DISPLAY
	    (DISPLA 
	      NFORMA 
	      LDISP 
	      GRIND ))
   (:MODULE GCD
	    (SPGCD
	      ezgcd
	      ))
;  (:MODULE RATIONAL-FUNCTION-SUBSYSTEM
;	   (RATIONAL-FUNCTIONS FACTORING GCD))
   (:MODULE DOcuMENTATION
	    (OPTION 
	      ;; Due to the highly hackish readtable munging,
	      ;; and file accessing done in the PRIMER it has not
	      ;; been converted.
	      ;;(("Cl-maxima-source:MAXIMA;PRIMER" "Cl-maxima-object:MAXIMA;PRIMER")
	      describe
	      MUDOC ))
   (:MODULE ALGEBRAIC-DATABASE
	    (INMIS 
	      DB 
	      COMPAR 
	      ASKP ))				;does this belong here?
   (:MODULE INTEGRATION
	    (SININT 
	      SIN 
	      RISCH ))
   (:MODULE TAYLOR-SERIES
	    (HAYAT ))
   (:MODULE DEFINITE-INTEGRATION
	    (DEFINT 
	      RESIDU ))
;  (:MODULE DISPLAY-EDITOR-MACROS
;	   (
;	    EDMAC
;	    ))
;  (:MODULE DISPLAY-EDITOR
;	   (
;	    EDLM ;;for lispm use good zmacs interface
;	    ;;instead of this stuff. Can display into zmacs and evaluate etc. --wfs
;	    EDCTL 
;	    EDEXP 
;	    EDBUF ;;not to useful for lispm -wfs
;	    ))
   (:MODULE PLOTTING
	    (PLTWIN 
	      PLOTLL				; fix the instance
	      PLOT 
	      IFFUN 
	      PLOT3D ))
   (:MODULE TRIGONOMETRY
	    (TRIGI 
	      TRIGO 
	      TRGRED ))
   (:MODULE SPECIAL-FUNCTIONS
	    (SPECFN ))
   (:MODULE MATRIX-ALGEBRA
	    (MAT 
	      MATRIX ))
   (:MODULE DETERMINANTS
	    (SPRDET 
	      NEWINV 
	      LINNEW 
	      NEWDET ))
   (:MODULE PATTERN-MATCHING
	    (SCHATC 
	      MATCOM 
	      MATRUN 
	      NISIMP ))
   (:MODULE LIMITS
	    (TLIMIT 
	      LIMIT ))
   (:MODULE SOLVE
	    (SOLVE 
	      PSOLVE 
	      ALGSYS 
	      POLYRZ 
	      CPOLY ))
;  (:Module Tensor
;           (itensr 
;	    canten 
;	    gener 
;	    symtry ))
   
;  (:module documentation-files
;           (:documentation macsym.doc)
   (:MODULE DEBUGGING
	    (MTRACE ))
   (:MODULE MISCELLANEOUS
	    (SCS 
	      ASUM 
	      FORTRA 
	      OPTIM 
	      ARRAY 
	      MDOT 
	      IRINTE 
	      SERIES 
	      NUMTH 
	      LAPLAC 
	      PADE 
	      HOMOG 
	      COMBIN 
	      MSTUFF))
   (:MODULE POISSON-SERIES
	    (RATPOI
	      POIS2 
	      POIS3 ))
   (:MODULE INTERACTION-WITH-EDITOR
	    (BUFFER1
	      buffer2
	      BUFFER))
   (:Module translated-packages
	    (nusum
	      desoln
	      elim
	      trgsmp
	      ode2
	      invert
	      )) 
   ))

#+ti
(progn
(setq compiler::qc-file-check-indentation nil)
(remprop 'prog 'compiler::style-checker)
)
 
#+distribute ;for making tape
(tape:carry-dump '("sys:site;cl-maxima.system"
		   "sys:site;*cl-maxima*.translations.newest"
		   "maxima-documentation:maxima;*instal*.*.newest"
		   "cl-maxima-source:maxima;*.lisp.newest"
		   "cl-maxima-source:maxima;*.mac.newest"
		   "maxima-documentation:maxima;*.*.newest"))



