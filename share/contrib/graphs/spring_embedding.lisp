;;;
;;;  GRAPHS - graph theory package for Maxima
;;;
;;;  Copyright (C) 2007 Andrej Vodopivec <andrej.vodopivec@gmail.com>
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2 of the License, or	 
;;;  (at your option) any later version. 
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; force-based graph embedding algorithm
;;;
;;; Based on:
;;; T.M.J. Fruchterman, E.M. Reingold, Graph drawing by force-directed
;;; placement, Software practice and experience 21 (1991), 11, 1129--1164.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *vertex-position*)
(defvar *optimal-distance*)

(defvar *epsilon-distance* 0.5)
(defvar *frame-width* 10.0)

(defvar *fixed-vertices* nil)

(defun attractive-force (d)
  (/ (* d d) *optimal-distance*))

(defun repulsive-force (d)
  (let ((d (max d *epsilon-distance*)))
    (/ (* *optimal-distance* *optimal-distance*) d 100)))

(defun distance (p1 p2)
  (let ((dx (- (first p1)
	       (first p2)))
	(dy (- (second p1)
	       (second p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun random-positions (v-list)
  (when *fixed-vertices*
    (let ((n (length *fixed-vertices*)))
      (dotimes (i (length *fixed-vertices*))
	(let ((v (nth i *fixed-vertices*))
	      (x (* *frame-width* ($sin (/ (* 2 i pi) n))))
	      (y (* *frame-width* ($cos (/ (* 2 i pi) n)))))
	  (setf (gethash v *vertex-position*)
		(list x y))))))
  (dolist (v v-list)
    (unless (member v *fixed-vertices*)
      (let* ((x (- *frame-width* (random (* 2 *frame-width*))))
	     (y (- *frame-width* (random (* 2 *frame-width*)))))
	(setf (gethash v *vertex-position*)
	      (list x y))))))

(defun $spring_embedding (g depth &optional fixed-vertices)
  (let ((*vertex-position* (make-hash-table))
	(vertex-displacement (make-hash-table))
	(*fixed-vertices* (cdr fixed-vertices))
	(*optimal-distance* (/ (* 2 *frame-width*)
			       (sqrt ($graph_size g)))))

    (random-positions (vertices g))
    
    (let* ((step (/ *frame-width* 5))
	   (d-step (/ step (1+ depth))))
      (dotimes (i depth)
	(setq step (- step d-step))
      
	(dolist (v (vertices g))
	  (setf (gethash v vertex-displacement) (list 0 0)))

	;; calculate repulsive forces
	(when (null (cdr fixed-vertices))
	  (let ((v-vrt (vertices g)))
	    (loop while v-vrt do
		 (let* ((v (car v-vrt))
			(u-vrt (cdr v-vrt))
			(v-pos (gethash v *vertex-position*)))
		   (loop while u-vrt do
			(let* ((u (car u-vrt))
			       (u-pos (gethash u *vertex-position*))
			       (delta (list (- (first v-pos) (first u-pos))
					    (- (second v-pos) (second u-pos))))
			       (delta-abs (distance v-pos u-pos))
			       (force (repulsive-force delta-abs))
			       (x (* (/ (first delta) (max delta-abs *epsilon-distance*))
				     force))
			       (y (* (/ (second delta) (max delta-abs *epsilon-distance*))
				     force))
			       (v-disp (gethash v vertex-displacement))
			       (u-disp (gethash u vertex-displacement)))
			  (setf (gethash v vertex-displacement)
				(list (+ (first v-disp) x)
				      (+ (second v-disp) y))
				(gethash u vertex-displacement)
				(list (- (first u-disp) x)
				      (- (second u-disp) y)))
			  (setq u-vrt (cdr u-vrt)))))
		 (setq v-vrt (cdr v-vrt)))))
	
	;; calculate attractive forces
	(dolist (e (edges g))
	  (let* ((v (first e)) (u (second e))
		 (v-pos (gethash v *vertex-position*))
		 (u-pos (gethash u *vertex-position*))
		 (delta (list (- (first v-pos) (first u-pos))
			      (- (second v-pos) (second u-pos))))
		 (delta-abs (distance v-pos u-pos))
		 (v-disp (gethash v vertex-displacement))
		 (u-disp (gethash u vertex-displacement))
		 (force (attractive-force delta-abs))
		 (x (* (/ (first delta) (max delta-abs *epsilon-distance*))
		       force))
		 (y (* (/ (second delta) (max delta-abs *epsilon-distance*))
		       force)))
	    (setf (gethash v vertex-displacement)
		  (list (- (first v-disp) x)
			(- (second v-disp) y)))
	    (setf (gethash u vertex-displacement)
		  (list (+ (first u-disp) x)
			(+ (second u-disp) y)))))
	
	;; Limit the displacement
	(dolist (v (vertices g))
	  (unless (member v *fixed-vertices*)
	    (let* ((v-disp (gethash v vertex-displacement))
		   (v-disp (mapcar #'(lambda (u) (/ u 2)) v-disp))
		   (v-disp-abs (distance (list 0 0) v-disp))
		   (v-pos (gethash v *vertex-position*)))
	      (if (> v-disp-abs step)
		  (setq v-pos (list (+ (first v-pos)
				       (* (/ (first v-disp) v-disp-abs) step))
				    (+ (second v-pos)
				       (* (/ (second v-disp) v-disp-abs) step))))
		  (setq v-pos (list (+ (first v-pos) (first v-disp))
				    (+ (second v-pos) (second v-disp)))))
	      (setq v-pos (list (min *frame-width* (max (first v-pos) 
							(- *frame-width*)))
				(min *frame-width* (max (second v-pos)
							(- *frame-width*)))))
	      (setf (gethash v *vertex-position*) v-pos)))) ))
    
    (let (result)
      (maphash #'(lambda (vrt pos)
		   (setq result
			 (cons `((mlist simp) ,vrt ((mlist simp) ,@pos))
			       result)))
	       *vertex-position*)
      (cons '(mlist simp) result)) ))
