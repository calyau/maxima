(require "SLOOP") 
(or (find-package "MAKE") (make-package "MAKE" :use '(lisp "SLOOP")))  
(require "MAKE")
(use-package "MAKE")

(or (member :cl *features*) (setq *features* (cons :cl *features*)))

(setf (get :minima :source-path) "foo.lisp")
(setf (get :maxima :object-path)
    (setf (get :minima :object-path) "foo.o"))
(require "MAXIMA" "maxima-package.lisp")



(setf (get :minima :files)
      '(clmacs
	commac
    
	compat
	maxmac
	displa
	mlisp
	simp
	serror
	mformt
	trigi  ;;for various consts and props.
	nforma
	runtim
	utils
	merror
	macsys
	suprv1
	displa
	nparse
	(:lisp  vals)
	inmis
	opers

	comm

	mutils
	db
	    
        compar
	))

(require "autol" "autol.lisp")
