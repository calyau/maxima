
(or (find-package "MAKE") (make-package "MAKE" :use '(LISP)))

(in-package "MAKE")

(or (fboundp 'make) (load  "make.lisp" ))
(or (member :cl *features*) (setq *features* (cons :cl *features*)))

(setf (get :maxima :source-path) "foo.lisp")
(setf (get :maxima :object-path)
      #+lucid "foo.lbin" #+excl "foo.fasl" #+clisp "foo.fas" #-(or :CLISP excl lucid) "foo.o")


#+lispm
(progn
;;;----Adjust these for your site if this is a lisp machine.
  (setf (get :maxima :object-path)
	(if (boundp '*logical-object*) *logical-object*
	#+ti "max:maxima.object;foo.xld"
	#+symbolics  "alonzo:>maxima>object>foo.bin"))
  (setf (get :maxima :source-path)
	(if (boundp '*logical-source*) *logical-source*
	  "rascal:/usr2/maxima/src/foo.lisp")))

#+(and cl (not lispm))
(or (member :nocp  lisp:*features*) (push :nocp *features*))

(load  (make::our-merge "maxima-package" (get :maxima :source-path))) 

#+kcl ;may have sloop
(if (fboundp 'sloop::sloop) (push :SLOOP *features*))


(setf (get :maxima  :make)
      '(
	#-sloop sloop			;include sloop if not there in
					;plain lisp.
					;module declarations
	(lmdcls) 
					;module destructuring-let
	(letmac)
					;module compatibility-macros1

	#-(or kcl lispm) generr
;	#+kcl serror
	#+kcl kclmac
	(clmacs 
	  )
					;module compatibility-macros
	( commac
	   ) 
	#+symbolics method-compat
					;module prerequisites
	mormac 
	compat
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
	#+kcl optimize
	;;other   (:module macros (
	;; other-macros rat-macros numerical-macros))
					;module utilities
	(:progn
	 (load "SYS-PROCLAIM.lisp")
	 (proclaim '(optimize (safety 0) (speed 3))))
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
					;newfac  ;why was this commented out?
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
	  acall
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
	(:progn ;;reset this from nparse
	 (proclaim '(optimize (safety 0) (speed 3))))
					;module display
	(displa 
	  nforma 
	  ldisp 
	  grind )
					;module gcd
	(spgcd
	  ezgcd
	  )
					;module documentation
	(option
					;primer	
	  macdes
	  #+obsolete mudoc		;is this used?
					;module algebraic-database
	  )
	(inmis 
	  db 
	  compar 
	  askp )			;does this belong here?
					;module integration
	(sinint 
	  sin 
	  risch )
					;module taylor-series
	(hayat )
					;module definite-integration
	(defint 
	  residu )
					;module plotting
	#+lispm
	(pltwin 
	  plotll			; fix the instance
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
   
					;module debugging
	(mtrace
	 mdebug
	 )
					;module miscellaneous
	(scs 
	  asum 
	  fortra 
	  optim 
	  marray 
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
	#+(or lispm main-files-loaded)
	(nusum
	  desoln
	  elim
	  trgsmp
	  ode2
	  invert)
	(:load-source autol max_ext)
	(:progn
	  (format t "~%Setting object path default to ~a"
		  (setf (get :maxima :object-path)
			(probe-file (object 'clmacs)))))
	(:load-source version autol max_ext init_max1)
	))



#+symbolics
(cond ( (>= (si::get-system-version 'system) 349)
       (push :genera Lisp:*features*)
       (push :zlch lisp:*features*))
      (t (push :pre-genera lisp:*features*)))

#+ti
(cond ((= (si::get-system-version ) 1)
       (push :tirel3 Lisp:*features*)))


(setf (get :maxima-macros :source-path)(get :maxima :source-path))
(setf (get :maxima-macros :object-path)(get :maxima :object-path))
(setf (get :maxima-macros :make)
  '(  #-sloop sloop
	                                ;module declarations
    (lmdcls) 
					;module destructuring-let
    (letmac)
					;module compatibility-macros1
     #-kcl generr
;     #+kcl serror
     #+kcl kclmac
     #+kcl (:load-source fundcl)

    (clmacs 
     )
					;module compatibility-macros
    ( commac
      ) 
    #+symbolics method-compat
					;module prerequisites
    mormac 
     compat
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
     #+kcl optimize
))

#+kcl
(setq make::*system-p* t)



;;minima



(setf (get :minima :source-path) "/usr/local/schelter/cl/foo.lisp")
(setf (get :minima :object-path) "/usr/local/schelter/cl/o/foo.o")
(setf (get :minima :make)
      '(  #-sloop sloop
		  (letmac)
					;module compatibility-macros1
		  #+kcl (
			 ;serror
			 kclmac)
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
		  (:progn
		    (format t "~%Setting object path default to ~a"
			    (setf (get :maxima :object-path)
				  (probe-file (object 'clmacs)))))
		  (:load-source version autol max_ext init_max1 )
		  ))

;(in-package "MAXIMA")
;(let ((si::*defuns* '(defun defmfun))) (make-declare :maxima))

#+kcl
(setf (get 'maxima::fixed-args :proclaim) #'(lambda (x)
				      (setf (get x 'compiler::fixed-args)
					    t)))