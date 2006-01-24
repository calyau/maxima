;; Copyright 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$linalgextra 1 '$version)

(defun $circulant (lst)
  ($require_list lst "$first" "$circulant")
  (let ((q) (n ($length lst)))
    (setq lst (rest lst))
    (setq q (list lst))
    (decf n)
    (dotimes (i n)
      (setq lst `(,@(rest lst) ,(car lst)))
      (push lst q))
    (setq q (mapcar #'(lambda(s) (cons '(mlist) s)) q))
    (push '($matrix) q)))

(defun $cauchy_matrix (p &optional q)
  ($require_list p "$first" "$cauchy_matrix")
  (if q ($require_list q "$second" "$cauchy_matrix") (setq q p))
  (let ((row) (mat))
    (setq p (margs p))
    (setq q (margs q))
    (dolist (pj p)
      (setq row nil)
      (dolist (qj q)
	(push (div 1 (add pj qj)) row))
      (setq row (nreverse row))
      (push '(mlist) row)
      (push row mat))
    (setq mat (nreverse mat))
    (push '($matrix) mat)))


  
