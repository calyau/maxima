;;  Copyright 2004, 2026 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; Innocent looking problems, such as matrixexp(matrix([x,1,0],[1,1,1],[0,1,1])),
;; generate huge (and most likely worthless) expressions. These huge 
;; expressions strain Maxima's rational function code; to avoid errors
;; such as "..quotient by polynomial of higher degree" I found it necessary to
;;
;;  (1) set gcd == spmod and algebraic == true,

;;  (2) always give ratsimp and fullratsimp a value (even if nil) for its
;;      optional second argument,

;;  (3) set ratvars to an empty list at the start of most functions.

;; I didn't try to find the real cause for these bugs.  

;; The function spectral_rep does a quick check its output. The test isn't exhaustive, but
;; it is fast.
  
(in-package :maxima)

($put '$matrixexp 2026 '$version)

;; Standard environment and function for simplification in this package.
(defun matrixexp-simp (e)
     ($fullratsimp e nil))

(defmfun $spectral_rep (mat)
  ($require_square_matrix mat '$first '$spectral_rep)
  ($require_unblockedmatrix mat '$first '$spectral_rep)
  (setq mat ($ratdisrep mat))
  (let (($ratmx nil) ($domain '$complex) ($gcd '$spmod) ($algebraic t) ($resultant '$subres) (ord) (zi)
	($ratfac nil) (z (gensym)) (res) (m) (n ($length ($args mat))) 
	(p) (p1) (p2) (sp) (proj))

    (setq p ($newdet (sub mat (mul z ($ident n)))))
    (if (oddp n) (setq p (mul -1 p)))

    ;; p1 = (z - z1)(z - z2) ... (z - zk), where z1 thru zk are
    ;; the distinct zeros of p.

    (setq p1 ($first ($divide p ($gcd p ($diff p z) z) z)))
    (setq p2 ($resultant p1 ($diff p1 z) z))

    (cond ((and (not ($constantp p2)) (not (alike1 0 p2)))
	   (setq p2 ($sqfr p2))
	   (if (mminusp p2) (setq p2 (mul -1 p2)))
	   (setq p2 (if (mtimesp p2) (margs p2) (list p2)))
	   (setq p2 (mapcar #'(lambda (s) (if (mexptp s) (nth 1 s) s)) p2))
	   (setq p2 (fapply 'mtimes p2))
	(mtell "Proviso: assuming ~:M ~%" (ftake 'mnotequal p2 0))))

    (setq sp ($solve p z))
    (setq sp (mapcar '$rhs (cdr sp)))
    (cond ((not (eql n (apply #'+ (cdr $multiplicities))))
	   (merror "Unable to find the spectrum")))
   
    (setq res (matrixexp-simp (ncpower (sub (mul z ($ident n)) mat) -1)))
    (setq m (length sp))
    (dotimes (i m)
      (setq zi (nth i sp))
      (setq ord (nth (+ i 1) $multiplicities))
      (push (matrix-map #'(lambda (e) (rational-residue e z zi p ord)) res) 
	    proj))

    (setq proj (nreverse proj))
    (setq m (length proj))
    (let ((remainder mat))
       (dotimes (i m)
         (setq remainder (sub remainder (mul (nth i sp) (nth i proj)))))
        
       (setq remainder (matrixexp-simp remainder))
       ;; When nilpotent-p can determine that remainder is nilpotent, return
       ;; the spectral representation; otherwise merror.
       (cond ((nilpotent-p remainder)
               (ftake 'mlist (fapply 'mlist sp) (fapply 'mlist proj) remainder))
	        (t
	          (merror "Unable to find the spectral representation"))))))
  
(defun nilpotent-p (mat)
  "Return T if the Maxima matrix MAT is nilpotent, NIL otherwise.
   Uses repeated multiplication up to the matrix dimension."

  (let* ((n ($length mat))
         (zero ($zeromatrix n n))
         (power mat))

    ;; If already zero, nilpotent
    (when (alike1 (matrixexp-simp mat) zero)
      (return-from nilpotent-p t))

    ;; Try powers mat^2, mat^3, ..., mat^n
    (dotimes (k n)
      (setq power (ncmul2 power mat))
      (when (alike1 (matrixexp-simp power) zero)
        (return-from nilpotent-p t)))
    ;; No zero power found
    nil))

;; When mat is a square matrix, return exp(mat * x). The second 
;; argument is optional and it defaults to 1.

(defmfun $matrixexp (mat &optional (x 1))
  (let (($ratmx nil) ($gcd '$spmod) (sp) (d) (p) (id) (n ($length ($args mat))) (f))
    ($ratvars)
    ($require_square_matrix mat '$first '$matrixexp)
    (setq mat ($spectral_rep mat))
    (setq sp ($first mat))
    (setq p ($second mat))
    (setq sp
      (fapply 'mlist
              (mapcar #'(lambda (s)
                          (ftake 'mexpt '$%e (mul s x)))
                      (cdr sp))))
    (setq d (mul x ($third mat)))
    (setq id ($ident n))
    (setq f id)
    (setq n (+ n 1))
    ;; Horner scheme
    (dotimes (i n)
      (setq f (add id (div (ncmul2 d f) (- n i)))))
    (matrixexp-simp (ncmul2 (ncmul2 sp p) f))))
    

;; Let f(var) = expr.  This function returns f(mat), where 'mat' is a 
;; square matrix.  Here expr is an expression---it isn't a function!

(defun require-lambda (e n pos fun-name)
  (let ((var))
    (if (and (consp e) (consp (car e)) (eq 'lambda (mop e)) (= 3 (length e))
	     ($listp (nth 1 e)) (setq var (cdr (nth 1 e))) (= n (length var))
	     (every #'(lambda (s) (or (symbolp s) ($subvarp s))) var))
	(list var (nth 2 e))
      (merror "The ~:M argument to `~:M' must be a lambda form with ~:M variable(s)" pos fun-name n))))

(defmfun $matrixfun (lamexpr mat)
  (let (($gcd '$spmod) ($ratmx nil) (z (gensym)) (expr) (var) (sp) (d) (p) (di) 
	(n ($length ($args mat))) (f 0))

    ($require_square_matrix mat '$second '$matrixexp)
    (setq expr (require-lambda lamexpr 1 '$first '$matrixfun))
    (setq var (nth 0 (nth 0 expr)))
    (setq expr (nth 1 expr))
    (setq expr ($substitute z var expr))
    (setq mat ($spectral_rep mat))
    (setq sp ($first mat))  
    (setq p ($second mat))  
    (setq d ($third mat))
    (setq di ($ident n))
    (setq sp (cdr sp))
    (dotimes (i (+ n 1))
      (setq f (add f (ncmul2 di (ncmul2 (fapply 'mlist (mapcar #'(lambda (s) (maxima-substitute s z expr)) sp)) p))))
      (setq di (ncmul2 di d))
      (setq expr (div ($diff expr z) (factorial (+ i 1))))) 
    (matrixexp-simp f)))
     
;; Return the residue of the rational expression e with respect to the
;; variable var at the point pt.  Assumptions:

;;  (1) the denominator of e divides ker,
;;  (2) e is a rational expression,
;;  (3) ker is a polynomial,
;;  (4) pt is a zero of ker and ord is its order.

(defun rational-residue (e var pt ker ord)
  (let (($gcd '$spmod) ($algebraic t) ($ratfac nil) (p) (q) (f (sub var pt)))
    (setq e (sratsimp e))
    (setq p ($num e))
    (setq q ($denom e))
    (setq p (mul p ($quotient ker q var)))
    (setq e (div p ($quotient ker (power f ord) var)))
    (matrixexp-simp
     (maxima-substitute pt var (div ($diff e var (- ord 1)) (factorial (- ord 1)))))))


    
    


    
	
