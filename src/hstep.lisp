;;
;; Copyright (C) 2010, 2011 Mark H. Weaver <mhw@netris.org>
;;
;; hstep: Heaviside step function support for Maxima
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;

(in-package :maxima)

($put '$hstep 1 '$version)

(setf (get '%hstep 'simplim%function) 'simplim%hstep)

(setf (get '%hstep 'real-valued) t)

;; TODO: other properties which would be nice to declare about hstep:
;;   non-negative
;;   non-decreasing
;; But neither of these are established Maxima features. Also, it would also 
;; be nice to simplify products containing more than one hstep, but that is 
;; a simplification on products, not on hstep.

(defgrad %hstep ($x)
  #$$ delta(x)$
  )
(defprop $delta ((x) ((%hstep) x)) integral)


;; Unlike `signum`, we do not extend hstep to the complex plane. Maxima defines hstep(0) = 1/2, 
;; but the DLMF (http://dlmf.nist.gov/1.16.iv) defines hstep(0) = 0. 
(def-simplifier hstep (z)
  "Simplify `hstep(z)`. "
  (flet ((fn (s)
           (let ((sgn ($csign s)))
             (cond ((eq sgn '$neg) -1)
                   ((eq sgn '$pos) 1)
                   (t s)))))
    ;; When z is a product, replace all negative terms by -1 and all positive terms by 1.
    ;; We could replace all zero terms by 0, but this could cause some indeterminate products
    ;; to simplify to zero.
    (when (mtimesp z)
      (setq z (fapply 'mtimes (mapcar #'fn (cdr z)))))
    (let ((sgn ($csign z)))
      (cond ((eq sgn '$neg) 0)       ; hstep(neg) = 0
            ((eq sgn '$zero) 1//2)   ; hstep(zero) = 1/2
            ((eq sgn '$pos) 1)       ; hstep(pos) = 1
            ((great (neg z) z)       ; hstep(-z) = 1 - hstep(z)
             (sub 1 (ftake '%hstep (neg z))))
            (t (give-up)))))) ;no simplifications

(defun simplim%hstep (e x pt)  
 "Return limit(e,x,pt), where e = hstep(X)."
  (let* ((*preserve-direction* t) 
         (lim (limit (cadr e) x pt 'think))
	       (sgn (mnqp lim 0)))
    (cond 
	    ((eq lim '$zerob) 0)  ;hstep(zerob) = 0
	    ((eq lim '$zeroa) 1)	;hstep(zeroa) = 1
	    ((eq t sgn) (ftake '%hstep lim))     ;; limit of arg is not zero, so use direct substitution
	    ((eq nil sgn) '$und)        ;; limit of arg is zero; limit doesn't exist
	    (t (throw 'limit nil)))))   ;; don't know

;; Give hstep an antiderivative
(defun hstep-integral (x)
   "Return an antiderivative of hstep. Specifically: integrate(hstep(x),x) = x*hstep(x)."
	(mul x (ftake '%hstep x)))

(putprop '%hstep `((x) ,'hstep-integral) 'integral)

;; A sign function for hstep. 
;; When either x < 0 or x > 0, hstep (x) simplifies, so the value of sgn should never be neg or pos.
(defun hstep-sign (q)
  (let ((sgn ($csign (cadr q))))
    (setf sign 
	  (cond
       ;; sign(hstep(zero)) = pos
      ((eq sgn '$zero) '$pos)
      ;; sign(hstep(positive or zero)) = positive
      ((or (eq sgn '$pos) (eq sgn '$pz)) '$pos)
      ;; sign(hstep(negative)) = zero
      ((eq sgn '$neg) '$zero)
	    ;; sign(hstep(negative or zero)) = pz
	    ((eq sgn '$nz) '$pz)
	    ;; sign(hstep(pnz)) = pz
      ((eq sgn '$pnz) '$pz)
	    ;; sign(hstep(pn)) = pz
	    ((eq sgn '$pn) '$pz)
	    ;; Maxima is inconsistent with the sign of something that is undefined. We'll throw a merror.
      (*complexsign*
	       (merror (intl:gettext "The csign of ~M is undefined ~%") q))
	    (t
		    (merror (intl:gettext "The sign of ~M is undefined ~%") q))))))

(setf (get '%hstep 'sign-function) 'hstep-sign)
