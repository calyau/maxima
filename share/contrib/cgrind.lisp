(in-package :maxima)

(macsyma-module cgrind)

;; This is heavily lifted from grind.lisp and fortra.lisp, and should have the
;; same features as the fortran command. In order to work it needs to be compiled and
;; then loaded, as in (for the case of cmucl):
;; :lisp (compile-file "cgrind.lisp"), copy cgrind.sse2f to ~/.maxima
;; load(cgrind)
;; Then one can run cgrind(expression) or cgrind(matrix).
;; M. Talon (2011)


(declare-top (special $loadprint)) ;If NIL, no load message gets printed.

;; This function is called from Macsyma toplevel.  First the arguments is
;; checked to be a single expression. Then if the argument is a
;; symbol, and the symbol is bound to a matrix, the matrix is printed
;; using an array assignment notation.

(defmspec $cgrind (l)
  (setq l (fexprcheck l))
  (let ((value (strmeval l)))
    (cond ((msetqp l) (setq value `((mequal) ,(cadr l) ,(meval l)))))
    (cond ((and (symbolp l) ($matrixp value))
	   ($cgrindmx l value))
	  ((and (not (atom value)) (eq (caar value) 'mequal)
		(symbolp (cadr value)) ($matrixp (caddr value)))
	   ($cgrindmx (cadr value) (caddr value)))
	  (t (c-print value)))))

(defun c-print (x &optional (stream *standard-output*))
  ;; Restructure the expression for displaying. 
  ;; Mainly sanitizes exponentials, notably exp(2/3) becomes
  ;; exp(2.0/3.0)

  (setq x (scanforc x))

  ;; Protects the modifications to mexpt from creeping out.

  (unwind-protect

    (progn     
    (defprop mexpt msz-cmexpt grind)

    ;; This means basic printing for atoms, grind does fancy things.
    (setq *fortran-print* t)

    ;; Prints using the usual grind mechanisms
    (mgrind x stream)(write-char #\; stream)(write-char #\Newline stream))
       
    ;; Restore usual mexpt property etc. before exiting this frame.
    (defprop mexpt msz-mexpt grind)
    (setq *fortran-print* nil))
  '$done)


;; The only modification to grind, converts a^b to pow(a,b), but taking
;; care of appropriate bracketing. The argument l to the left of (MEXPT)
;; has to be composed backwards. Finally a^-b has special treatment.

(defun msz-cmexpt (x l r)
  (setq l (msize (cadr x) (revappend '(#\p #\o #\w #\() l) (list #\,) 'mparen 'mparen)
        r (if (mmminusp (setq x (nformat (caddr x))))
              (msize (cadr x) (list #\-) (cons #\) r) 'mexpt rop)
              (msize x nil  (cons #\) r ) 'mparen 'mparen)))
  (list (+ (car l) (car r)) l r))


  
  

;; Takes a name and a matrix and prints a sequence of C assignment
;; statements of the form
;; NAME[I][J] = <corresponding matrix element>
;; This requires some formatting work unnecessary for the fortran case.

(defmfun $cgrindmx (name mat &optional (stream *standard-output*) &aux ($loadprint nil))
  (cond ((not (symbolp name))
	 (merror (intl:gettext "cgrindmx: first argument must be a symbol; found: ~M") name))
	((not ($matrixp mat))
	 (merror (intl:gettext "cgrindmx: second argument must be a matrix; found: ~M") mat)))
  (do ((mat (cdr mat) (cdr mat)) (i 1 (1+ i)))
      ((null mat))
    (do ((m (cdar mat) (cdr m)) (j 1 (1+ j)))
	((null m))
       (format stream "~a[~a][~a] = " (string-left-trim "$" name) (1- i) (1- j) )
       (c-print (car m) stream)))
  '$done)





;; This C scanning function is similar to fortscan.  Prepare an expression
;; for printing by converting x^(1/2) to sqrt(x), etc.   Since C has no
;; support for complex numbers, contrary to Fortran, ban them.

(defun scanforc (e)
  (cond ((atom e) (cond ((eq e '$%i) ;; ban complex numbers
			 (merror (intl:gettext "Take real and imaginary parts")))
			(t e)))	 
	;; %e^a -> exp(a)
	((and (eq (caar e) 'mexpt) (eq (cadr e) '$%e))
	 (list '(%exp simp) (scanforc (caddr e))))
	;; a^1/2 -> sqrt(a)  1//2 is defined as ((rat simp) 1 2)
	((and (eq (caar e) 'mexpt) (alike1 (caddr e) 1//2))
	 (list '(%sqrt simp) (scanforc (cadr e))))
	;; a^-1/2 -> 1/sqrt(a)
	((and (eq (caar e) 'mexpt) (alike1 (caddr e) -1//2))
	 (list '(mquotient simp) 1 (list '(%sqrt simp) (scanforc (cadr e)))))
	;; (1/3)*b -> b/3.0 and (-1/3)*b -> -b/3.0
	((and (eq (caar e) 'mtimes) (ratnump (cadr e))
	      (member (cadadr e) '(1 -1) :test #'equal))
	 (cond ((equal (cadadr e) 1) (scanforc-mtimes e))
	       (t (list '(mminus simp) (scanforc-mtimes e)))))
	;; 1/3 -> 1.0/3.0
	((eq (caar e) 'rat)
	 (list '(mquotient simp) (float (cadr e)) (float (caddr e))))
	;; rat(a/b) -> a/b via ratdisrep
	((eq (caar e) 'mrat) (scanforc (ratdisrep e)))
	;; ban complex numbers
	((and (member (caar e) '(mtimes mplus) :test #'eq)
	      (let ((a (simplify ($bothcoef e '$%i)))) 
		(and (numberp (cadr a))
		     (numberp (caddr a))
		     (not (zerop1 (cadr a)))
		     (merror (intl:gettext "Take real and imaginary parts"))))))
	;; in general do nothing, recurse
	(t (cons (car e) (mapcar 'scanforc (cdr e))))))

;; This is used above 1/3*b*c -> b*c/3.0
(defun scanforc-mtimes (e)
  (list '(mquotient simp)
	(cond ((null (cdddr e)) (scanforc (caddr e)))
	      (t (cons (car e) (mapcar 'scanforc (cddr e)))))
	(float (caddr (cadr e)))))

