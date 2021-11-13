;;;  Copyright 2021 by Barton Willis
;;;  Maxima code for integration of signum, abs, max, min, floor, ceiling, unit_step, and hstep.

;;;  This is free software; you can redistribute it and/or
;;;  modify it under the terms of the GNU General Public License,
;;;  http://www.gnu.org/copyleft/gpl.html.

;;; This software has NO WARRANTY, not even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;; Maxima list of additional integration methods.
(defmvar $extra_integration_methods 
  (simplifya (list (list 'mlist) '$intfudu '$intfugudu '$signum_int 
    '$abs_integrate_use_if '$floor_int '$if_int) t))

;; Maxima list of additional definite integration methods.
(defmvar $extra_definite_integration_methods 
  (simplifya (list (list 'mlist) '$abs_defint) t))

;; Maxima interface to return the value of the special  *integrator-level*
;; When it's unbound, return zero.
(defmfun $integration_depth ()
   (declare (special *integrator-level*))
   (if (boundp '*integrator-level*) *integrator-level* 0))