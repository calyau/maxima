;;; Define a length function similar to the one in maple, at least on polynomials
;;; Symbols have length equal to the number of characters, etc. Composite objects have
;;; length given by the sum of lengths of atoms plus length of structures as discovered with
;;; dismantle. Example:  

;; > dismantle(3*xxx^2):
;; SUM(3)                  header of polynomial length 3
;;    PROD(3)                header of monomial length 3
;;      NAME(4): xxx           symbol length 3
;;      INTPOS(2): 2           power 2 length 1
;;    INTPOS(2): 3           coeff 3 length 1  total 3+3+3+1+1=11
;; > length(3*xxx^2);
;; 11

;; One sees that monomials x^ay^b... are represented by a structure PROD of
;; length 2n+1 (n the number of terms, each gets 2 slots pointing to variable and power,
;; even when the power is 1) but if we have just a variable no prod is generated.
;; As soon as the monomial has a coefficient or if there is a sum of terms, they are
;; represented by a structure SUM of length 2n+1 (n the number of terms, each one occupies 2
;; slots for pointers to monomial and coefficient, even 1 if the coeff is 1). Thus
;; length(x+y)=9 while length(x^2+y^2)=17, difference due to 2 PROD of length 2.3=6 plus the lengths
;; of the exponents 1+1=2. Moreover x+y is represented by SUM (length 5) plus 2 variables
;; and coeffs equal to 1 (length 4). This is very different from the maxima representation.

                                     

(in-package :maxima)

;; The cases where mapatom returns T
; For a rational the header adds 2, length(3/5)=5
; In maxima 32/76 is represented as  ((RAT SIMP) 8 19), simplified, so remove 13
; Note one gets the length of the simplified rational, as in maple
; For a float the header adds 3, length(5.67)=7
; For a subscripted variable, the header adds 3, length(x[12])=8
; In maxima x[12] is represented as (($X SIMP ARRAY) 12) so remove 15
; If x[12] is assigned, one gets the length of the assignment

(defun charsets_atomlength (x)
  (cond ((integerp x) (length (write-to-string x)))
	(($ratnump x) (- (length (write-to-string x)) 11))  
	((floatp x) (+ 3 (length (write-to-string x))))
	(($subvarp x) (- (length (write-to-string x)) 12))
	((symbolp x) (1- (length (symbol-name x))))  ; to remove the leading $
	(t (merror "Mapatom must send the argument on 1 here."))))

(defun charsets_maxatomp (x)
  "Maxima atoms"
  (if (or (charsets_coeffp x) (charsets_variap x)) t nil))


(defun charsets_coeffp (x)
  "Maxima atoms which can be coefficients"
  (cond ((atom x) (if (or (integerp x)(floatp x)) t nil))
	((and (listp (car x)) ($ratnump x)) t)
	(t nil)))

(defun charsets_variap (x)
  "Maxima atoms which can be variables"
  (cond ((symbolp x) t)
	((and (listp (car x)) ($subvarp x)) t)
	(t nil)))


(defun charsets_atomlength1 (x insum)
  ;; An atom (symbol coeff or exponent) causes no problem at the higher level (start)
  ;; or in a power term. But a bare symbol in a sum or a prod needs to add 1 for the
  ;; coeff (=1) resp. the exponent (=1), a bare number in a sum needs to add 1 as coeff.
  ;; Examples: x*y -> prod (x^1,y^1), x+y -> sum (1*x,1*y), 
  ;; 2*x*y -> sum (2*prod (x^1,y^1)), 3+2*x -> sum(1*3,2*x)
  ;; This has to ba called only on sums and products
  (cond (insum (1+ (charsets_atomlength x)))              ; add 1 for bare numbers and symbols
	(t (if (or (symbolp x)($subvarp x))      ; add 1 only for bare symbols in products
	       (1+ (charsets_atomlength x)) (charsets_atomlength x)))))




;; The case where mapatom returns false
;; In maxima (in maple this is automatically done)
;; the polynomial must first be expanded to distribute eventual coefficients, e.g. 
;; (2*x^3*y^5+3*y*z^2)/5 after applying expand is represented as:
;; ((MPLUS SIMP)
;; ((MTIMES SIMP) ((RAT SIMP) 2 5) ((MEXPT SIMP) $X 3) ((MEXPT SIMP) $Y 5))
;; ((MTIMES SIMP) ((RAT SIMP) 3 5) $Y ((MEXPT SIMP) $Z 2)))
;; Note that contrary to maple, in the second monomial, Y is not ((MEXPT SIMP) $Y 1)


(defun $charsets_polylength (x)
  (let ((y ($expand x)))
    ;; We treat here the exceptional cases at start: isolated atoms, isolated exponent x^m,
    ;; isolated product like 2*x -> sum (2*x) of length 5 and not sum(2*prod(x^1)) of length 9

    (cond ((atom y) (charsets_atomlength y))	                       ; isolated integer, float or symbol
	  ((and (listp (car y)) ($ratnump y)) (charsets_atomlength y)) ; idem, rational
	  ((and (listp (car y)) ($subvarp y)) (charsets_atomlength y)) ; idem, subscripted variable
	  ((and (listp (car y)) (eq (caar y) 'MTIMES) (eql  (length (cdr y)) 2)
		(charsets_coeffp (second y)) (charsets_variap (third y)))
	   (+ 3 (charsets_atomlength (second y)) (charsets_atomlength (third y))))
	  ((and (listp (car y)) (eq (caar y) 'MEXPT))
	    (+ 3 (reduce #'+ (mapcar #'charsets_atomlength (cdr y))))) ; isolated x^m
	  ;; Now we know we have a sum or a prod or a list of terms
	  (t (let ((insum nil)) (charsets_polyrecur y insum)))))) 

;; Monomials at the top level with a coefficient require an extra sum header. We call them 
;; impure. One then needs to add 3 to length and reduce by 1 the number of terms

(defun charsets_impuremonomialp (x)
  ; we suppose (and (listp (car x)) (eq (caar x) 'MTIMES) true
  (if (charsets_coeffp (cadr x)) t nil))

 
(defun charsets_partial-length (h-length n-terms x insum)
  (+ h-length 1 (* 2 n-terms) (reduce  #'+ (charsets_polyrecur (cdr x) insum))))

(defun charsets_polyrecur (x insum)
  ;; MEXPT terms don't produce a new header when inside a prod
  (if (charsets_maxatomp x) (charsets_atomlength1 x insum)
	(cond ((and (listp (car x)) (eq (caar x) 'MPLUS))        ; sum
	       (setq insum t)
	       (charsets_partial-length 0 (length (cdr x)) x insum))
	      ((and (listp (car x)) (eq (caar x) 'MTIMES))       ; monomial
	       (let ((h-length 0) (n-terms (length (cdr x))))
		 (if (charsets_impuremonomialp x) (setq n-terms (1- n-terms)))
		 (if (not insum)  (setq h-length 3))              ; maple sees it as a sum  
		 (setq insum nil)
	       (charsets_partial-length h-length n-terms x insum)))
	      ((and (listp (car x)) (eq (caar x) 'MEXPT))        
	       (if insum
		   (+ 4 (reduce #'+ (mapcar #'charsets_atomlength (cdr x))))       ; isolated x^m
		   (reduce #'+ (mapcar #'charsets_atomlength (cdr x)))))           ; x^m in a product
	      (t (mapcar #'(lambda(y)(charsets_polyrecur y insum)) x))))) ; list of monomials or list of terms
    
;; Tests:
;; (%i3) polylength( (2*x^3*y^5+3*y*z^2)/5);
;; (%o3)                                 33
;; (%i4) polylength(x+y);
;; (%o4)                                  9
;; (%i5) polylength(2*x);
;; (%o5)                                  5
;; (%i6) polylength(2*x^2*y^3);
;; (%o6)                                 13
;; All the same values as in maple. It should be remarked that this length function doesn't
;; discriminate as much as one would like, e.g. x+y and x*y have same length 9 

;; Extension to sets and lists. In maple there is a header of length n+3 where n is
;; the length or the set or list.

(defun $charsets_length (x)
  "Length function similar to maple one for lists of polynomials"
  (cond ((and (listp x) (listp (car x)) (or (eq (caar x) 'MLIST) (eq (caar x) '$SET)))
	 (+ 3 (length (cdr x)) (reduce #'+ (mapcar #' $charsets_polylength (cdr x)))))
	(($charsets_polynomialp x) ($charsets_polylength x))
	(t (merror "Only lists or sets of polynomials supported."))))


;; (%i8) charsets_length([ (2*x^3*y^5+3*y*z^2)/5,2*x^2*y^3]);
;; (%o8)                                 51
;; Same result as in maple.

