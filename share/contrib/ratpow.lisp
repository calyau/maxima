;;; ratpow version 0.3 4 June 2015
;;; 
;;; Copyright 2013-2015 Stavros Macrakis
;;; Released under the LGPL (http://www.gnu.org/copyleft/lesser.html)

;;; These functions operate on the *numerator* of a CRE
;;; to find the exponents in the denominator, use ratXXpow(ratdenom(...))

;;; ratp_hipow -- highest power of the main variable in a CRE polynomial
;;; ratp_lopow -- lowest power of the main variable in a CRE polynomial
;;; ratp_coeffs -- list of powers and coefficients of the main variable
;;; ratp_dense_coeffs -- list of coefficients of the main variable, highest first
;;; ratp_dense_coeffs_lo -- list of coefficients of the main variable, lowest first

;; To get a list of vars in a CRE, use showratvars

;;; ratp_hipow( expr, var ) => highest power of var in ratnumer(expr)
;;;  ratp_hipow( x^(5/2) + x^2 , x) => 2
;;;  ratp_hipow( x^(5/2) + x^2 , sqrt(x)) => 5
(defun $ratp_hipow (e v) (ratp_pow e v 'hipow))

;;; ratp_lopow( expr, var ) => lowest power of var in ratnumer(expr)
;;;  ratp_lopow( x^(5/2) + x^3 , x) => 0 
;;     CRE is {x}^3 + {x^(1/2)}^5 * {x}^0, where {} denotes kernels
(defun $ratp_lopow (e v) (ratp_pow e v 'lopow))

;;; ratp_coeffs( expr, var ) => list of powers/coefficients in ratnumer(expr)
;;; returned coefficients are in CRE form except for numbers
;;;  ratp_coeffs( 4*x^3 + x + sqrt(x), x) => [[3,4],[1,1],[0,sqrt(x)]]
(defun $ratp_coeffs (e v) (ratp_pow e v 'sparse_coeffs))

;;; ratp_dense_coeffs( expr, var ) => list of coefficients in ratnumer(expr), highest first
;;; returned coefficients are in CRE form except for numbers
;;;  ratp_dense_coeffs( 4*x^3 + x + sqrt(x), x) => [4,0,1,sqrt(x)]
(defun $ratp_dense_coeffs (e v) (ratp_pow e v 'dense_coeffs))

;;; ratp_dense_coeffs_lo( expr, var ) => list of coefficients in ratnumer(expr), lowest first
;;; returned coefficients are in CRE form except for numbers
;;;  ratp_dense_coeffs_lo( 4*x^3 + x + sqrt(x), x) => [sqrt(x),1,0,4]
(defun $ratp_dense_coeffs_lo (e v) (ratp_pow e v 'dense_coeffs_low))


(defun ratp_pow (e v func)
  (if (mbagp e)
      (simplify (cons (list (caar e))
		      (mapcar #'(lambda (i) (ratp_pow i v func)) (cdr e))))
    (let* ((e ($rat (if ($taylorp e) ($ratdisrep e) e) v)) ; change main variable if necessary
	   (mrat (car e))
	   (kernels (caddr mrat))
	   (gensyms (cadddr mrat))
	   (mainvar-gensym (nth (1- (length kernels)) gensyms))
	   (numerat (cadr e))
	   (res ())
	   (lis
	    (if (or (pcoefp numerat)
		    (not (eq (car numerat) mainvar-gensym)))
		(list 0 numerat)
	      (cdr numerat))))
      (cond ((eq func 'hipow) (car lis))
	    ((eq func 'lopow) (car (last lis 2)))
	    (t (setq kernels (butlast kernels 1))
	       (setq mrat (list 'mrat 'simp kernels gensyms))
  ;;; Putprop needed for pdisrep, but not part-rat
  ;;;	     (mapc #'(lambda (y z) (putprop y z 'disrep)) gensyms kernels)
	       (cond
		((eq func 'sparse_coeffs)
		 (do ((lis lis (cddr lis)))
		     ((null lis)
		      (cons '(mlist simp) (nreverse res)))
		   (push (list '(mlist simp)
			       (car lis)
			       (part-rat mrat (cadr lis)))
			 res)))
		((memq func '(dense_coeffs dense_coeffs_low))
		 (do ((lis lis (cddr lis))
		      (pow (car lis) (1- pow)))
		     ((null lis)
		      (cons '(mlist simp)
			    (if (eq func 'dense_coeffs_low)
				res
			      (nreverse res))))
		   (while (> pow (car lis))
		     (push 0 res)
		     (setq pow (1- pow)))
		   (push (part-rat mrat (cadr lis)) res)))))))))

(defun part-rat (mrat expr)
  (if (pcoefp expr)
      expr
    (list* mrat expr 1)))
