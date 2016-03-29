;; from Fateman


(in-package :maxima)

(defun $charsets_polylength (x)
   (let ((y (ratf x)))
   (+ (cp1 (cadr y)) ;numerator
      (cp1 (cddr y))))) ;denominator

(defun cp1(p)
   ;; sum up  the sizes of the terms in the rational form polynomial p
   (if (pcoefp p) (flatsize p)            ; a constant's length in 
decimal chars
        (let ((count 0))
       (do ((i (cdr p)(cddr i)))
           ((null i) (1+ count))  ; one more for the variable name
         (incf count (flatsize (car i))) ; length of exponent
         (incf count (cp1 (cadr i))) ; length of coefficient, recursively
        ))))


;; Tests:
#|
pl(x):=charsets_polylength(x)         ;
pl( (2*x^3*y^5+3*y*z^2)/5)         ; 12
pl(x+y)                 ; 8
pl(2*x)                 ; 4
pl(2*x^2*y^3)                 ; 6
|#
;; This is NOT the same as Maple, but I am guessing that it is
;; a satisfactory ranking of complexity for the
;; characteristic set method.    RJF

