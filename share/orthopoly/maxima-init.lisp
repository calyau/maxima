;;----------------------------------------
;; Optional code starts here

;; The Maxima fixfloat function is buggy. Here is a quick fix.  At one
;; time, test_orthopoly needed this fix -- I think it's no longer needed.

#+cl (defun fixfloat (x)
	(cond ((floatp x)
	       (setq x (rationalize x))
	       (cons (numerator x) (denominator x)))
	      (t
	       x)))

;; fix for apply(rat(f),[x]) bug.

(defun error-size (exp)
  (setq exp (ratdisrep exp))
  (if (atom exp) 0
    (do ((l (cdr exp) (cdr l))
	 (n 1 (incf n (+ 1 (error-size (car l))))))
	((or (null l)  (> n $error_size)) n)
      (declare (fixnum n)))))

;; If file fn (with _no_ file extension) is out of date, compile it. We 
;; assume that the extension of the source file is "lisp". 

(defun $make (fn)
  (setq fn (string-left-trim "&" (string fn)))
  (let ((fl) (fb) 
	(bin-ext #+gcl "o"
		 #+cmu (c::backend-fasl-file-type c::*target-backend*)
		 #+clisp "fas"
		 #+allegro "fasl"
		 #-(or gcl cmu clisp allegro) ""))
    (setq fl (concatenate 'string (coerce fn 'string) ".lisp"))
    (setq fl ($file_search fl))
    (setq fb (concatenate 'string (coerce fn 'string) "." bin-ext))
    (setq fb ($file_search fb))
    (if (or (and fl (not fb)) 
	    (and fl fb (> (file-write-date fl) (file-write-date fb))))
	($compile_file fl))))

;; The make function recompiles orthopoly automatically on startup if 
;; the binary file is older than orthopoly.lisp. Unless you
;; anticipate making changes to orthopoly, you may comment out
;; ($make "orthopoly")

(eval-when (load compile eval)
  ($make "orthopoly"))

;; Load file f with verbose and print bound to true. Useful for loading
;; code with syntax errors.

(defun $xload (f)
  (load ($file_search f) :verbose 't :print 't))

(defprop $entier tex-matchfix tex)
(defprop $entier (("\\lfloor ") " \\rfloor") texsym)

;;-----------------------------------------------------------
;; Essential code starts here

;; This fixes a bug in $bessel_j.

(defun $bessel_j (arg order)
  (if (and (numberp order)
	   (numberp ($realpart arg))
	   (numberp ($imagpart arg)))
      ;($bessel (complex ($realpart arg) ($imagpart arg)) order) <== bug
      ($bessel arg order)
      (subfunmakes '$bessel_j (ncons order) (ncons arg))))
 
;; This version of setup_autoload uses file_search.

(defun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func (dollarify-name func)) file 'autoload)
      (add2lnc func $props)))
  '$done)

;; This version of generic-autoload in Maxima 5.9.0 doesn't recognize
;; "fas", "fasl" and "x86f" as valid extensions for compiled Lisp
;; code.

#+cl
(defun generic-autoload (file &aux type)
  (setq file (pathname (cdr file)))
  (setq type (pathname-type file))
  (let ((bin-ext #+gcl "o"
		 #+cmu (c::backend-fasl-file-type c::*target-backend*)
		 #+clisp "fas"
		 #+allegro "fasl"
		 #-(or gcl cmu clisp allegro) ""))
    (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
	(load file :verbose 't) ($batchload file))))

;; Extended version of makegamma1 that converts pochhammer symbols
;; to quotients of gamma functions.

(defun makegamma1 (e)
  (cond ((atom e) e)
	((eq (caar e) 'mfactorial)
	 (list '(%gamma) (list '(mplus) 1 (makegamma1 (cadr e)))))

	;;------start of new code----------------------------------
	;; Do pochhammer(x,n) ==> gamma(x+n)/gamma(x).
	((eq (caar e) '$pochhammer)
	 (let ((x (makegamma1 (nth 1 e)))
	       (n (makegamma1 (nth 2 e))))
	   `((mtimes) ((%gamma) ((mplus) ,x ,n)) ((mexpt) ((%gamma) ,x) -1))))

	((eq (caar e) '%genfact)
	 (let ((x (makegamma1 (nth 1 e)))
	       (y (makegamma1 (nth 2 e)))
	       (z (makegamma1 (nth 3 e))))
	   (setq x (add (div x z) 1))
	   (setq y (simplify `(($entier) ,y)))
	   (setq z (power z y))
	   `((mtimes) ,z ((%gamma) ,x) 
	     ((mexpt) ((%gamma) ((mplus) ,x ((mtimes) -1 ,y))) -1))))
	   
	;;-----that's all folks ... end of new code------------------

	((eq (caar e) '%elliptic_kc)
	 ;; Complete elliptic integral of the first kind
	 (cond ((alike1 (cadr e) '((rat simp) 1 2))
		;; K(1/2) = gamma(1/4)/4/sqrt(pi)
		'((mtimes simp) ((rat simp) 1 4)
		  ((mexpt simp) $%pi ((rat simp) -1 2))
		  ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2)))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) 1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    ;; 1/(8-4*sqrt(3))
			    '((mexpt simp)
			      ((mplus simp) 8
			       ((mtimes simp) -4
				((mexpt simp) 3 ((rat simp) 1 2))))
			      -1)))
		    ;; K((2+sqrt(3)/4))
		    '((mtimes simp) ((rat simp) 1 4)
		      ((mexpt simp) 3 ((rat simp) 1 4))
		      ((mexpt simp) $%pi ((rat simp) -1 2))
		      ((%gamma simp) ((rat simp) 1 6))
		      ((%gamma simp) ((rat simp) 1 3))))
		((or (alike1 (cadr e)
			     ;; (2-sqrt(3))/4
			     '((mtimes simp) ((rat simp) 1 4)
			       ((mplus simp) 2
				((mtimes simp) -1
				 ((mexpt simp) 3 ((rat simp) 1 2))))))
		     (alike1 (cadr e)
			     ;; 1/2-sqrt(3)/4
			     '((mplus simp) ((rat simp) 1 2)
			       ((mtimes simp) ((rat simp) -1 4)
				((mexpt simp) 3 ((rat simp) 1 2)))))
		     (alike (cadr e)
			    ;; 1/(4*sqrt(3)+8)
			    '((mexpt simp)
			      ((mplus simp) 8
			       ((mtimes simp) 4
				((mexpt simp) 3 ((rat simp) 1 2))))
			      -1)))
		     ;; K((2-sqrt(3))/4)
		     '((mtimes simp) ((rat simp) 1 4)
		       ((mexpt simp) 3 ((rat simp) -1 4))
		       ((mexpt simp) $%pi ((rat simp) -1 2))
		       ((%gamma simp) ((rat simp) 1 6))
		       ((%gamma simp) ((rat simp) 1 3))))
		 ((or
		   ;; (3-2*sqrt(2))/(3+2*sqrt(2))
		   (alike1 (cadr e)
			   '((mtimes simp)
			     ((mplus simp) 3 
			      ((mtimes simp) -2 
			       ((mexpt simp) 2 ((rat simp) 1 2))))
			     ((mexpt simp)
			      ((mplus simp) 3 
			       ((mtimes simp) 2
				((mexpt simp) 2 ((rat simp) 1 2)))) -1)))
		   ;; 17 - 12*sqrt(2)
		   (alike1 (cadr e)
			   '((mplus simp) 17 
			     ((mtimes simp) -12
			      ((mexpt simp) 2 ((rat simp) 1 2)))))
		   ;;   (2*SQRT(2) - 3)/(2*SQRT(2) + 3)
		   (alike1 (cadr e)
			   '((mtimes simp) -1
			     ((mplus simp) -3
			      ((mtimes simp) 2 
			       ((mexpt simp) 2 ((rat simp) 1 2))))
			     ((mexpt simp)
			      ((mplus simp) 3
			       ((mtimes simp) 2
				((mexpt simp) 2 ((rat simp) 1 2))))
			      -1))))
		  '((mtimes simp) ((rat simp) 1 8)
		    ((mexpt simp) 2 ((rat simp) -1 2))
		    ((mplus simp) 1 ((mexpt simp) 2 ((rat simp) 1 2)))
		    ((mexpt simp) $%pi ((rat simp) -1 2))
		    ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2)))
		 (t
		  ;; Give up
		  e)))
	((eq (caar e) '%elliptic_ec)
	 ;; Complete elliptic integral of the second kind
	 (cond ((alike1 (cadr e) '((rat simp) 1 2))
		;; 2*E(1/2) - K(1/2) = 2*%pi^(3/2)*gamma(1/4)^(-2)
		'((mplus simp)
		  ((mtimes simp) ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) 
		    ((%gamma simp irreducible) ((rat simp) 1 4)) -2))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2))))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mtimes simp) -1
				((mexpt simp) 3 ((rat simp) 1 2))))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) -1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2))))))
		;; E((2-sqrt(3))/4)
		;;
		;; %pi/4/sqrt(3) = K*(E-(sqrt(3)+1)/2/sqrt(3)*K)
		'((mplus simp)
		  ((mtimes simp) ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 6)) -1)
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) 1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2))))))
		;; E((2+sqrt(3))/4)
		;;
		;; %pi*sqrt(3)/4 = K1*(E1-(sqrt(3)-1)/2/sqrt(3)*K1)
		'((mplus simp)
		  ((mtimes simp) 3 ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 6)) -1)
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1))
		  ((mtimes simp) ((rat simp) 3 8)
		   ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))
		  ((mtimes simp) ((rat simp) -1 8)
		   ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))))
	       (t
		e)))
	(t (recur-apply #'makegamma1 e))))

;; Autoload the nset functions that simplify.

(add2lnc '$unit_step $props)
(defprop $unit_step simp-unit-step operators)
(autof 'simp-unit-step '|orthopoly|)

($setup_autoload "orthopoly"
		 '$assoc_legendre_p
		 '$assoc_legendre_q
		 '$chebyshev_t
		 '$chebyshev_u
		 '$gen_laguerre
		 '$hermite
		 '$intervalp
		 '$jacobi_p
		 '$laguerre
		 '$legendre_p
		 '$legendre_q
		 '$orthopoly_recur
		 '$orthopoly_weight
		 '$pochhammer
		 '$spherical_bessel_j
		 '$spherical_bessel_y
		 '$spherical_hankel1
		 '$spherical_hankel2
		 '$spherical_harmonic
		 '$ultraspherical)
