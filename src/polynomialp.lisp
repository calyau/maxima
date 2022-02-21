;; Author Barton Willis

;; University of Nebraska at Kearney
;; Copyright (C) 2004, 2005, Barton Willis
;; Brief Description: polynomial predicate function.
	   		       								 
;;  This program is free software; you can redistribute it and/or modify	 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.				 
 		       								 
;;  You should have received a copy of the GNU General Public License	
;;  along with this program; if not, write to the Free Software 		 
;;  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

(in-package :maxima)

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (functionp 'op-equalp)) ($load "linalg-utilities"))
  (if (not (functionp 'require-list-or-set)) ($load "nset")))

;; Return true iff n is an integer and n >= 0.

(defmfun $nonnegintegerp (n)
  (and (integerp n) (>= n 0)))

(defmfun $polynomialp (p vars &optional (coeffp '$constantp) (exponp '$nonnegintegerp))
  "Returns true if P is a polynomial in the variables in the list VARS.
 The predicate COEFFP must be a function that evaluates to T for each
 coefficient, and simpilarly EXPONP must evaluate to T for all
 exponents of the variables in VARS."
  (setq vars (require-list-or-set vars %%pretty-fname))
  (setq vars (mapcar '$ratdisrep vars))
  (if (every #'(lambda (s) (or ($symbolp s) ($subvarp s))) vars)
      (polynomialp ($ratdisrep p) vars coeffp exponp)
      (merror "~M: The second argument to polynomialp must be a list of symbols: ~M"
	      %%pretty-fname (list* '(mlist) vars))))
 
(defun polynomialp (p vars coeffp exponp)
  (or
   (mfuncall coeffp p)
   (if (member p vars :test #'alike1) t nil)
   (and (op-equalp p 'mtimes 'mplus)
	(every #'(lambda (s) (polynomialp s vars coeffp exponp)) (margs p)))
   (and (op-equalp p 'mexpt) (polynomialp (car (margs p)) vars coeffp exponp)
	(mfuncall exponp (cadr (margs p))))))
