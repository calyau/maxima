;;;
;;;  GRAPHS - graph theory package for Maxima
;;;
;;;  Copyright (C) 2008 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains programs for computing the
;;; Wiener index of a graph. It includes the algorithms for
;;; ordinary and weighted Wiener index computation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FLOYD-WARSHALL algorithm for all-pairs shortest paths
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defmfun $floyd_warshall (g)
  (require-graph-or-digraph 1 'floyd_warshall g)
  (let* ((vertices (vertices g))
	 (n (length vertices))
	 (m (make-array (list n n)))
	 (mat ($zeromatrix n n)))

    ;; setup the array
    (dotimes (i n)
      (dotimes (j n)
	(if (/= i j)
	    (setf (aref m i j)
		  ($get_edge_weight `((mlist simp) ,(nth i vertices) ,(nth j vertices)) g '$inf))
	    (setf (aref m i j) 0))))

    ;; compute the distances
    (dotimes (k n)
      (dotimes (i n)
	(dotimes (j n)
	  (when (eq (mlsp (m+ (aref m i k) (aref m k j)) (aref m i j)) t)
	    (setf (aref m i j) (m+ (aref m i k) (aref m k j)))))))

    ;; check for negative cycles
    (dotimes (k n)
      (when (eq (mlsp (aref m k k) 0) t)
	($error "Graph contains a negative cycle.")))

    ;; fill the matrix
    (dotimes (i n)
      (dotimes (j n)
	(setf (nth (1+ j) (nth (1+ i) mat)) (aref m i j))))

    mat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JOHNSON's algorithm for all pairs shortest path
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bellman-ford (s g)
  (let ((d (make-hash-table))
	(edges (if (graph-p g) (append (edges g) (mapcar #'reverse (edges g))) (edges g)))
	(prev (make-hash-table)))

    ;; initialize distances
    (dolist (v (vertices g))
      (if (= s v)
	  (setf (gethash v d) 0)
	  (setf (gethash v d) '$inf)))

    ;; relax edges
    (dotimes (i (1- (length (vertices g))))
      (dolist (e edges)
	(let* ((u (first e))
	       (v (second e))
	       (nd (m+ (gethash u d) ($get_edge_weight `((mlist simp) ,@e) g))))
	  (when (and (not (eq (gethash u d) '$inf))
		     (eq (mgrp (gethash v d) nd) t))
	    (setf (gethash v d) nd)
	    (setf (gethash v prev) u)))))

    ;; check for negative cycles
    (dolist (e edges)
      (let ((u (first e))
	    (v (second e)))
	(when (eq (mgrp (gethash v d)
			(m+ (gethash u d) ($get_edge_weight `((mlist simp) ,@e) g)))
		  t)
	  ($error "Graph contains a negative cycle."))))

    (values d prev)))

(defmfun $johnson (g)
  (let* ((h ($copy_graph g))
	 (vertices (vertices g))
	 (n (length vertices))
	 (m ($zeromatrix n n))
	 (nv (1+ (apply #'max vertices))))

    (dolist (e (cdr ($edges g)))
      ($set_edge_weight e ($get_edge_weight e g) h))

    ;; add a new vertex
    ($add_vertex nv h)
    (dolist (v vertices)
      ($add_edge `((mlist simp) ,nv ,v) h)
      ($set_edge_weight `((mlist simp) ,nv ,v) 0 h))

    ;; run the bellman-ford algorithm
    (multiple-value-bind (d prev)
	(bellman-ford nv h)
      (declare (ignore prev))

      ;; re-weight the edges
      (dolist (e (cdr ($edges g)))
	(let ((nw (m+ ($get_edge_weight e g)
		      (gethash ($first e) d)
		      (m- (gethash ($second e) d)))))
	($set_edge_weight e nw h)))
      ($remove_vertex nv h)

      ;; run the dijkstra's algorithm for each vertex
      (dotimes (i n)
	(multiple-value-bind (dd pd)
	    (dijkstra (nth i vertices) nv h)
	  (declare (ignore pd))
	  (dotimes (j n)
	    (when (/= i j)
	      (setf (nth (1+ j) (nth (1+ i) m))
		    (if (eq '$inf (gethash (nth j vertices) dd))
			'$inf
			(m+ (gethash (nth j vertices) dd)
			    (m- (gethash (nth i vertices) d))
			    (gethash (nth j vertices) d))))))))

      ;; return the matrix m
      m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JUVAN-MOHAR algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wiener-index (g)
  (let ((n (length (vertices g)))
	(q (make-hash-table))
	(j 0) (k 1) (w 0)
	(visited (make-hash-table))
	(d (make-hash-table :test #'equal))
	(s (make-hash-table))
	(c (make-hash-table :test #'equal))
	(we (make-hash-table :test #'equal))
	(wi 0))

    ;; initialize array we
    (dolist (e (edges g))
      (setf (gethash e we) 0))

    ;; initialize arrays c and d
    (dolist (v (vertices g))
      (dolist (u (vertices g))
	(setf (gethash (list v u) c) 0)
	(setf (gethash (list v u) d) 0)))
    
    ;; For each vertex i
    (dolist (v (vertices g))

      ;; initialize c
      (setf (gethash (list v v) c) 1)

      ;; we do a bfs search
      (dolist (i (vertices g))
	(setf (gethash i visited) nil))
      (setf (gethash 1 q) v
	    (gethash v visited) t)
      (setq j 0 k 1)
      (loop while (< j k) do
	   (incf j)
	   (setq w (gethash j q))
	   (dolist (u (neighbors w g))
	     (unless (gethash u visited)
	       (setf (gethash (list v u) d) (1+ (gethash (list v w) d)))
	       (setf (gethash u visited) t)
	       (incf k)
	       (setf (gethash k q) u))
	     (when (> (gethash (list v u) d) (gethash (list v w) d))
	       (incf (gethash (list v u) c) (gethash (list v w) c)))))

      ;; and visit vertices in reverse order
      (loop for j from n downto 1 do
	   (let ((w (gethash j q)))
	     (setf (gethash w s) 0)
	     (dolist (u (neighbors w g))
	       (when (< (gethash (list v w) d)
			(gethash (list v u) d))
		 (let ((x (* (/ (gethash (list v w) c)
				(gethash (list v u) c))
			     (1+ (gethash u s)))))
		   (incf (gethash (list (min u w) (max u w)) we) (/ x 2))
		   (incf (gethash w s) x)))))) )

    ;; Compute the Wiener index
    (dolist (e (edges g))
      (incf wi (gethash e we)))

    wi))


(defvar $wiener_index_algorithm '$juvan_mohar)

(defmfun $wiener_index (g)
  (require-graph 1 'wiener_index g)
  (unless ($is_connected g)
    ($error "`wiener_index': input graph is not connected"))
  (case $wiener_index_algorithm
    ($juvan_mohar (wiener-index g))
    ($johnson (m// ($xreduce "+" ($flatten ($args ($johnson g)))) 2))
    ($floyd_warshall (m// ($xreduce "+" ($flatten ($args ($floyd_warshall g)))) 2))
    (t ($error "Unknown algorithm for WIENER_INDEX"))))
