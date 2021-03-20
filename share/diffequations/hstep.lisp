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

(defprop $hstep %hstep verb)
(defprop %hstep $hstep noun)

(defprop $hstep %hstep alias)
(defprop %hstep $hstep reversealias)

(defprop %hstep simp-hstep operators)
(setf (get '%hstep 'simplim%function) 'simplim%hstep)

(setf (get '%hstep 'real-valued) t)

;; TODO: other properties which would be nice to declare about hstep:
;;   non-negative
;;   non-decreasing

(defprop %hstep ((x) (($delta) x)) grad)
(defprop $delta ((x) ((%hstep) x)) integral)

(defun $hstep (z) (take '(%hstep) z))

;;
;; TODO: should the following rule be included somehow?
;;
;;   hstep(-x)  -->  1 - hstep(x)
;;
;; It would also be nice to simplify products
;; containing more than one hstep.
;;
(defun simp-hstep (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (let ((sgn (csign z)))
    (cond ((eq sgn '$neg) 0)
	  ((eq sgn '$zero) 1//2)
	  ((eq sgn '$pos) 1)
	  (t
	   ;; positive * x --> x and negative * x --> -1 * x.
	   (if (mtimesp z)
	       (setq z (muln (mapcar #'(lambda (s)
					 (let ((sgn (csign s)))
					   (cond ((eq sgn '$neg) -1)
						 ((eq sgn '$pos) 1)
						 (t s))))
				     (margs z))
			     t)))
	   (eqtest (list '(%hstep) z) expr)))))

(defun simplim%hstep (e x pt)
  (let* ((e (limit (cadr e) x pt 'think))
	 (sgn (mnqp e 0)))
    (cond ((eq t sgn) ($hstep e))     ;; limit of arg is not zero
	  ((eq nil sgn) '$und)        ;; limit of arg is zero
	  (t (throw 'limit nil)))))   ;; don't know
