(require "MAKE")
(use-package "MAKE")
(require "SLOOP") 
(or (member :cl  lisp:*features*) (push :cl lisp:*features*))
#+(and cl (not lispm))
(or (member :nocp  lisp:*features*) (push :nocp lisp:*features*))

(require "MAXIMA" "maxima-package.lisp")
;(cond ((find-package "CL-MAXIMA"))(t (load "maxima-package.lisp")))
(setf (get :maxima :source-path) "/usr/local/schelter/cl/foo.lisp")
(setf (get :maxima :object-path) "/usr/local/schelter/cl/o/foo.o")
(setf (get :maxima :files)
  '(					;module declarations
    (lmdcls) 
					;module destructuring-let
    (letmac)
					;module compatibility-macros1
     #+kcl (serror kclmac)
    (clmacs 
     )
					;module compatibility-macros
    ( commac
      ) 
    #+symbolics method-compat
					;module prerequisites
    (mormac 
     compat)
					;module fundamental-macros
    (defopt 
     defcal 
     maxmac )
					;module utility-macros
    (mopers 
     mforma 
     )
					;module other-macros
    (mrgmac 
     procs 
     rzmac    
     strmac 
     displm )
					;module rat-macros
    (ratmac 
     mhayat )
					;module numerical-macros
    (numerm )
   
    ;;other   (:module macros (
    ;; other-macros rat-macros numerical-macros))
					;module utilities
    (opers 
     utils 
     sumcon 
     sublis 
     runtim 
     merror 
     mformt 
     mutils 
     outmis 
     ar 
     misc )
					;module commands
    (comm 
     comm2 )
					;module evaluator
    (mlisp 
     mmacro 
     buildq )
					;module simplification
    (simp 
     float 
     csimp 
     csimp2 
     zero 
     logarc 
     rpart )
					;module i-o
    (
     #+lispm lmsup 
     macsys 
     mload 
     suprv1 
     ;; we should really look, *seriously*, at the functionality provided
     ;; by the dskfn package. mainly, it saves and manages environments.
     dskfn 
     )
					;module factoring
    (lesfac 
     factor 
     algfac 
     nalgfa 
					;	    newfac    ;why was this commented out?
     ufact 
     result )
					;module rational-functions
    (rat3a 
     rat3b 
     rat3d 
     rat3c 
     rat3e 
     nrat4 
     ratout )
					;module maxima-language-compiler-macros
    (transm )
					;module maxima-language-compiler
    ;; this includes the translator itself and runtime
    ;; support needed for translated code.
    (transl 
     transs 
     trans1 
     trans2 
     trans3 
     trans4 
     trans5 
     transf 
     troper 
     trutil 
     trmode 
     trdata 
     trpred 
     transq 
     acall				;
     fcall 
     evalw 
     trprop 
     mdefun 
     )
					;module numerical-functions
    (bessel 
     ellipt 
     numer 
     intpol 
     rombrg 
     )
    ;;rest of system
					;module reader
    (nparse )
					;module display
    (displa 
     nforma 
     ldisp 
     grind )
					;module gcd
    (spgcd
     ezgcd
     )
					;  (:module rational-function-subsystem
					;	   (rational-functions factoring gcd))
					;module documentation
    (option
					;primer	
     macdes
     #+lispm mudoc )                    ;is this used?
					;module algebraic-database
    (inmis 
     db 
     compar 
     askp )				;does this belong here?
					;module integration
    (sinint 
     sin 
     risch )
					;module taylor-series
    (hayat )
					;module definite-integration
    (defint 
     residu )
					;  (:module display-editor-macros
					;	   (
					;	    edmac
					;	    ))
					;  (:module display-editor
					;	   (
					;	    edlm ;;for lispm use good zmacs interface
					;	    ;;instead of this stuff. can display into zmacs and evaluate etc. --wfs
					;	    edctl 
					;	    edexp 
					;	    edbuf ;;not to useful for lispm -wfs
					;	    ))
					;module plotting
    #+lispm
    (pltwin 
     plotll				; fix the instance
     plot 
     iffun 
     plot3d )
					;module trigonometry
    (trigi 
     trigo 
     trgred )
					;module special-functions
    (specfn )
					;module matrix-algebra
    (mat 
     matrix )
					;module determinants
    (sprdet 
     newinv 
     linnew 
     newdet )
					;module pattern-matching
    (schatc 
     matcom 
     matrun 
     nisimp )
					;module limits
    (tlimit 
     limit )
					;module solve
    (solve 
     psolve 
     algsys 
     polyrz 
     cpoly )
					;  (:module tensor
					;           (itensr 
					;	    canten 
					;	    gener 
					;	    symtry ))
   
					;  (:module documentation-files
					;           (:documentation macsym.doc)
					;module debugging
    (mtrace )
					;module miscellaneous
    (scs 
     asum 
     fortra 
     optim 
     array 
     mdot 
     irinte 
     series 
     numth 
     laplac 
     pade 
     homog 
     combin 
     mstuff)
					;module poisson-series
    (ratpoi
     pois2 
     pois3 )
					;module interaction-with-editor
    #+lispm
    (buffer1
     buffer2
     buffer)
					;module translated-packages
    #+main-files-loaded
    (nusum
     desoln
     elim
     trgsmp
     ode2
     invert
     ) 
    )))



#+symbolics
(cond ( (>= (si::get-system-version 'system) 349)
       (push :genera Lisp:*features*)
       (push :zlch lisp:*features*))
      (t (push :pre-genera lisp:*features*)))

#+ti
(cond ((= (si::get-system-version ) 1)
       (push :tirel3 Lisp:*features*)))


(setf (get :maxima-macros :source-path) "/usr/local/schelter/cl/foo.lisp")
(setf (get :maxima-macros :object-path) "/usr/local/schelter/cl/o/foo.o")
(setf (get :maxima-macros :files)
  '(					;module declarations
    (lmdcls) 
					;module destructuring-let
    (letmac)
					;module compatibility-macros1
     #+kcl (serror kclmac)
    (clmacs 
     )
					;module compatibility-macros
    ( commac
      ) 
    #+symbolics method-compat
					;module prerequisites
    (mormac 
     compat)
					;module fundamental-macros
    (defopt 
     defcal 
     maxmac )
					;module utility-macros
    (mopers 
     mforma 
     )
					;module other-macros
    (mrgmac 
     procs 
     rzmac    
     strmac 
     displm )
					;module rat-macros
    (ratmac 
     mhayat )
					;module numerical-macros
    (numerm )
    (transm)
))

#+kcl
(setq make::*system-p* t)



;;minima



(setf (get :minima :source-path) "/usr/local/schelter/cl/foo.lisp")
(setf (get :minima :object-path) "/usr/local/schelter/cl/o/foo.o")
(setf (get :minima :files)
 '(
    (letmac)
					;module compatibility-macros1
     #+kcl (serror kclmac)
    (clmacs 
     )
					;module compatibility-macros
    ( commac
      ) 
    #+symbolics method-compat

    maxmac
    compat
     rzmac    
     ratmac

    (opers 
     utils 
     sumcon 
     sublis 
     runtim 
     merror 
     mformt 
     mutils 
     outmis 
     ar 
     misc )
					;module commands
    (comm 
     comm2 )
					;module evaluator
    (mlisp 
     mmacro 
     buildq )
					;module simplification
    (simp 
     float 
     csimp 
     csimp2 
     zero 
     logarc 
     rpart )
					;module i-o
     macsys 
     mload 
     suprv1 
     

					;module factoring
    (lesfac 
     factor 
     algfac 
     nalgfa 
     ufact 
     result )
					;module rational-functions
    (rat3a 
     rat3b 
     rat3d 
     rat3c 
     rat3e 
     nrat4 
     ratout )
   spgcd
    nparse
    displa 
    nforma
    grind
    mdot
    trigi
    compar
))
