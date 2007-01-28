;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$polynomialp 1 '$version)

(defun $polynomialp (p x)
  (setq p ($ratdisrep p))
  (or
   ($freeof x p)
   (like p x)
   (and (or (mtimesp p) (mplusp p)) 
	(every #'(lambda (s) ($polynomialp s x)) (margs p)))

   (and (mexptp p) ($polynomialp (car (margs p)) x) 
	(integerp (cadr (margs p)))
	(> (cadr (margs p)) 0))))
   