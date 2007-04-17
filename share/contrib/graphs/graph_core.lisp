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
;;; graph and digraph datastructure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

($put '$graphs 1.0 '$version)

(format t "Loading graph theory package 'graphs', version ~a.~%"
	($get '$graphs '$version))


(defstruct (graph
	     (:print-function
	      (lambda (stru strm depth)
		(declare (ignore stru))
		(format strm "GRAPH"))))
  (size 0)
  (order 0)
  (vertices ())
  (vertex-labels (make-hash-table))
  (edges ())
  (edge-weights (make-hash-table :test #'equal))
  (neighbors (make-hash-table)))

(defstruct (digraph
	     (:print-function
	      (lambda (stru strm depth)
		(declare (ignore stru))
		(format strm "DIGRAPH"))))
  (size 0)
  (order 0)
  (vertices ())
  (vertex-labels (make-hash-table))
  (edges ())
  (edge-weights (make-hash-table :test #'equal))
  (in-neighbors (make-hash-table))
  (out-neighbors (make-hash-table)))

(defun require-graph (m ar gr)
  (if (not (graph-p gr))
      ($error (format nil "Argument ~a to `~a' is not a graph:" ar m) gr)))

(defun require-digraph (m ar gr)
  (if (not (digraph-p gr))
      ($error (format nil "Argument ~a to `~a' is not a graph:" ar m) gr)))

(defun require-graph-or-digraph (m ar gr)
  (if (not (or (graph-p gr) (digraph-p gr)))
      ($error (format nil "Argument ~a to `~a' is not a graph:" ar m) gr)))

(defun $print_graph (gr)
  (require-graph-or-digraph 'print_graph 1 gr)
  (cond
    ((graph-p gr)
     (format t "~%Graph on ~d vertices with ~d edges."
	     (graph-size gr) (graph-order gr))
     (if (> (graph-size gr) 0 )
	 (format t "~%Adjacencies:"))
     (dolist (v (graph-vertices gr))
       (format t "~% ~2d :" v)
       (dolist (u (neighbors v gr))
	 (format t " ~2d" u))))
    (t
     (format t "~%Diraph on ~d vertices with ~d arcs."
	     (digraph-size gr) (digraph-order gr))
     (if (> (digraph-size gr) 0 )
	 (format t "~%Adjacencies:"))
     (dolist (v (digraph-vertices gr))
       (format t "~% ~2d :" v)
       (dolist (u (out-neighbors v gr))
	 (format t " ~2d" u)))))
  (format t "~%")
  '$done)

(defun $is_graph (x)
  (graph-p x))

(defun $is_digraph (x)
  (digraph-p x))

(defun $is_graph_or_digrah (x)
  (or (graph-p x) (digraph-p x)))

(defun $graph_size (gr)
  (require-graph-or-digraph 'graph_size 1 gr)
  (if (graph-p gr)
      (graph-size gr)
      (digraph-size gr)))

(defun $graph_order (gr)
  (require-graph-or-digraph 'graph_order 1 gr)
  (if (graph-p gr)
      (graph-order gr)
      (digraph-order gr)))

(defun $copy_graph (gr)
  (require-graph-or-digraph 'copy_graph 1 gr)
  (if (graph-p gr)
      (let ((g (make-graph)))
	(dolist (v (graph-vertices gr))
	  (add-vertex v g)
	  (let ((l (get-vertex-label v gr)))
	    (if l (set-vertex-label v l g))))
	(dolist (e (graph-edges gr))
	  (add-edge e g)
	  (let ((w (get-edge-weight e g)))
	    (if w (set-edge-weight e w g))))
	g)
      (let ((g (make-digraph)))
	(dolist (v (digraph-vertices gr))
	  (add-vertex v g)
	  (let ((l (get-vertex-label v gr)))
	    (if l (set-vertex-label v l g))))
	(dolist (e (digraph-edges gr))
	  (add-edge e g)
	  (let ((w (get-edge-weight e g)))
	    (if w (set-edge-weight e w g))))
	g) ))

(defun $new_graph ()
  (make-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; vertex operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $vertices (gr)
  (require-graph-or-digraph 'vertices 1 gr)
  (let ((vrt (vertices gr)))
    `((mlist simp) ,@(copy-list vrt))))

(defun vertices (gr)
  (if (graph-p gr) (graph-vertices gr) (digraph-vertices gr)))

(defun require-vertex (m i v)
  (if (not (integerp v))
      ($error "Argument" i "to" m "is not a valid vertex.")))

(defun is-vertex-in-graph (i gr)
  (not (null (member i (vertices gr)))))

(defun require-vertex-in-graph (m i gr)
  (if (not (is-vertex-in-graph i gr))
      ($error m ": vertex not in graph.")))

(defun $is_vertex_in_graph (i gr)
  (require-vertex 'vertex_in_graph 1 i)
  (require-graph-or-digraph 'vertex_in_graph 2 gr)
  (is-vertex-in-graph i gr))

(defun $add_vertex (i gr)
  (require-vertex 'add_vertex 1 i)
  (require-graph-or-digraph 'add_vertex 2 gr)
  (if (is-vertex-in-graph i gr)
      ($error "add_vertex: vertex is already in the graph!"))
  (add-vertex i gr)
  '$done)

(defun $add_vertices (vl gr)
  (require-graph-or-digraph 'add_vertices 2 gr)
  (if (not ($listp vl))
      ($error "Argument 1 to add_vertices is not a list."))
  (dolist (v (cdr vl))
    ($add_vertex v gr))
  '$done)

(defun add-vertex (i gr)
  (if (graph-p gr)
      (progn
	(incf (graph-size gr))
	(push i (graph-vertices gr))
	(setf (gethash i (graph-neighbors gr)) ()))
      (progn
	(incf (digraph-size gr))
	(push i (digraph-vertices gr))
	(setf (gethash i (digraph-in-neighbors gr)) ())
	(setf (gethash i (digraph-out-neighbors gr)) ()))
      ))

(defun neighbors (i gr)
  (gethash i (if (graph-p gr)
		 (graph-neighbors gr)
		 (digraph-out-neighbors gr))))

(defun in-neighbors (i gr)
  (gethash i (digraph-in-neighbors gr)))

(defun out-neighbors (i gr)
  (gethash i (digraph-out-neighbors gr)))

(defun $neighbors (i gr)
  (require-vertex 'neighbors 1 i)
  (require-graph 'neighbors 2 gr)
  (require-vertex-in-graph 'neighbors i gr)
  `((mlist simp) ,@(copy-list (neighbors i gr))))

(defun $out_neighbors (i gr)
  (require-vertex 'out_neighbors 1 i)
  (require-digraph 'out_neighbors 2 gr)
  (require-vertex-in-graph 'out_neighbors i gr)
  `((mlist simp) ,@(copy-list (out-neighbors i gr))))

(defun $in_neighbors (i gr)
  (require-vertex 'in_neighbors 1 i)
  (require-digraph 'in_neighbors 2 gr)
  (require-vertex-in-graph 'in_neighbors i gr)
  `((mlist simp) ,@(copy-list (in-neighbors i gr))))

(defun $degree_sequence (gr)
  (require-graph 'degree_sequence 1 gr)
  (let ((s ()))
    (dolist (v (graph-vertices gr))
      (push (length (neighbors v gr)) s))
    (setq s (sort s #'<))
    `((mlist simp) ,@s)))

(defun $remove_vertex (v gr)
  (require-vertex 'remove_vertex 1 v)
  (require-graph-or-digraph 'remove_vertex 2 gr)
  (require-vertex-in-graph 'remove_vertex v gr)
  (clear-vertex-label v gr)
  (remove-vertex v gr))

(defun $remove_vertices (vl gr)
  (require-graph-or-digraph 'remove_vertices 2 gr)
  (if (not ($listp vl))
      ($error "Argument 1 to remove_vertices is not a list."))
  (dolist (v (cdr vl))
    ($remove_vertex v gr))
  '$done)

(defun remove-vertex (v gr)
  (if (graph-p gr)
      (progn
	(dolist (u (neighbors v gr))
	  (let ((e (list (min u v) (max u v))))
	    (remove-edge e gr)))
	(clear-vertex-label v gr)
	(setf (graph-vertices gr) (remove v (graph-vertices gr) :count 1))
	(remhash v (graph-neighbors gr))
	(decf (graph-size gr)))
      (progn
	(dolist (u (out-neighbors v gr))
	  (remove-edge (list v u) gr))
	(dolist (u (in-neighbors v gr))
	  (remove-edge (list u v) gr))
	(clear-vertex-label v gr)
	(setf (digraph-vertices gr) (remove v (digraph-vertices gr) :count 1))
	(remhash v (digraph-in-neighbors gr))
	(remhash v (digraph-out-neighbors gr))
	(decf (digraph-size gr))))
  '$done)

(defun $first_vertex (gr)
  (require-graph-or-digraph 'first_vertex 1 gr)
  (cond
    ((= 0 (if (graph-p gr) (graph-size gr) (digraph-size gr)))
     ($error "first_vertex: no first vertex in an empty graph."))
    (t (first (vertices gr)))))

(defun $max_degree (gr)
  (require-graph 'max_degree 1 gr)
  (cond
    ((= 0 (graph-size gr))
     ($error "max_degree: no max degree in an empty graph."))
    (t
     (let* ((v (first (graph-vertices gr))) (d (length (neighbors v gr))))
       (dolist (u (graph-vertices gr))
	 (if (> (length (neighbors u gr)) d)
	     (progn
	       (setq d (length (neighbors u gr)))
	       (setq v u))))
       `((mlist simp) ,d ,v)))))

(defun $min_degree (gr)
  (require-graph 'max_degree 1 gr)
  (cond
    ((= 0 (graph-size gr))
     ($error "min_degree: no min degree in an empty graph."))
    (t
     (let* ((v (first (graph-vertices gr))) (d (length (neighbors v gr))))
       (dolist (u (graph-vertices gr))
	 (if (< (length (neighbors u gr)) d)
	     (progn
	       (setq d (length (neighbors u gr)))
	       (setq v u))))
       `((mlist simp) ,d ,v)))))

(defun $average_degree (gr)
  (require-graph 'average_degee 1 gr)
  (m* 2 (m// (graph-order gr) (graph-size gr))))

(defun $vertex_degree (v gr)
  (require-vertex 'vertex_degree 1 v)
  (require-graph 'vertex_degree 2 gr)
  (require-vertex-in-graph 'vertex_degree v gr)
  (length (neighbors v gr)))

(defun $vertex_in_degree (v gr)
  (require-vertex 'vertex_in_degree 1 v)
  (require-digraph 'vertex_in_degree 2 gr)
  (require-vertex-in-graph 'vertex_in_degree v gr)
  (length (in-neighbors v gr)))

(defun $vertex_out_degree (v gr)
  (require-vertex 'vertex_out_degree 1 v)
  (require-digraph 'vertex_out_degree 2 gr)
  (require-vertex-in-graph 'vertex_out_degree v gr)
  (length (out-neighbors v gr)))

(defun $get_vertex_label (v gr)
  (require-vertex 'get_vertex_label 1 v)
  (require-graph-or-digraph 'get_vertex_label 2 gr)
  (require-vertex-in-graph 'vertex_label v gr)
  (get-vertex-label v gr))

(defun get-vertex-label (v gr)
  (gethash v  (if (graph-p gr)
		  (graph-vertex-labels gr)
		  (digraph-vertex-labels gr))))

(defun $clear_vertex_label (v gr)
  (require-vertex 'clear_vertex_label 1 v)
  (require-graph-or-digraph 'clear_vertex_label 2 gr)
  (require-vertex-in-graph 'clear_label v gr)
  (clear-vertex-label v gr))

(defun clear-vertex-label (v gr)
  (remhash v (if (graph-p gr)
		 (graph-vertex-labels gr)
		 (digraph-vertex-labels gr)))
  '$done)

(defun $set_vertex_label (v l gr)
  (require-vertex 'set_vertex_label 1 v)
  (require-graph-or-digraph 'set_vertex_label 3 gr)
  (require-vertex-in-graph 'set_label v gr)
  (set-vertex-label v l gr))

(defun set-vertex-label (v l gr)
  (setf (gethash v (if (graph-p gr)
		       (graph-vertex-labels gr)
		       (digraph-vertex-labels gr)))
	l)
  '$done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; edge operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-medge (m ar e)
  (cond
    ((not (and ($listp e) (eq 2 ($length e))))
     ($error "Argument" ar "to" m "is not an edge (0)."))
    (t (let ((u ($first e)) (v ($second e)))
	 (if (not (and (integerp u) (integerp v)))
	     ($error "Argument" ar "to" m "is not an edge (1)."))
	 (if (eq u v)
	     ($error "Argument" ar "to" m "is not an edge (2)."))))))

(defun require-edge-in-graph (m e gr)
  (if (not (is-edge-in-graph e gr))
      ($error m ": edge not in graph.")))

(defun m-edge-to-l-edge (e)
  (let ((uv (cdr e)))
    (list (apply #'min uv) (apply #'max uv))))

(defun m-edge-to-l-dedge (e)
  (let ((uv (cdr e)))
    (list (first uv) (second uv))))

(defun l-edge-to-m-edge (e)
  `((mlist simp) ,@e))

(defun $is_edge_in_graph (e gr)
  (require-medge 'is_edge_in_graph 1 e)
  (require-graph-or-digraph 'is_edge_in_graph 2 gr)
  (if (graph-p gr)
      (is-edge-in-graph (m-edge-to-l-edge e) gr)
      (is-edge-in-graph (m-edge-to-l-dedge e) gr)))

(defun is-edge-in-graph (e gr)
  (if (graph-p gr)
      (not (null (member e (graph-edges gr) :test #'equal)))
      (not (null (member e (digraph-edges gr) :test #'equal)))))

(defun $add_edge (e gr)
  (require-medge 'add_edge 1 e)
  (require-graph-or-digraph 'add_edge 2 gr)
  (let* ((e1 (if (graph-p gr) (m-edge-to-l-edge e) (m-edge-to-l-dedge e)))
	 (u (first e1)) (v (second e1)))
    (cond
      ((not (and (is-vertex-in-graph u gr) (is-vertex-in-graph v gr)))
       ($error "add_edge: end vertices are not in graph!"))
      ((eq u v)
       ($error "add_edge: end vertices are equal!"))
      ((is-edge-in-graph e1 gr)
       ($error "add_edge: edge already in graph!")))
    (add-edge e1 gr)))

(defun $add_edges (el gr)
  (require-graph-or-digraph 'add_edges 2 gr)
  (if (not ($listp el))
      ($error "Argument 1 to add_edges is not a list!")
      (dolist (e (cdr el))
	(require-medge 'add_edges 1 e)
	($add_edge e gr)))
  '$done)

(defun add-edge (e gr)
  (let ((u (first e)) (v (second e)))
    (if (graph-p gr)
	(progn
	  (push v (gethash u (graph-neighbors gr)))
	  (push u (gethash v (graph-neighbors gr)))
	  (push e (graph-edges gr))
	  (incf (graph-order gr)))
	(progn
	  (push v (gethash u (digraph-out-neighbors gr)))
	  (push u (gethash v (digraph-in-neighbors gr)))
	  (push e (digraph-edges gr))
	  (incf (digraph-order gr))))
    '$done))

(defun $edges (gr)
  (require-graph-or-digraph 'edges 1 gr)
  (let ((e (mapcar #'(lambda (u) `((mlist simp) ,@(copy-list u))) (edges gr))))
    `((mlist simp) ,@e)))

(defun edges (gr)
  (if (graph-p gr) (graph-edges gr) (digraph-edges gr)))

(defun $remove_edge (e gr)
  (require-medge 'remove_edge 1 e)
  (require-graph-or-digraph 'remove_edge 2 gr)
  (if (not ($is_edge_in_graph e gr))
      ($error "remove_edge: edge" e "is not in graph."))
  (remove-edge
   (if (graph-p gr)
       (m-edge-to-l-edge e)
       (m-edge-to-l-dedge e))
   gr))

(defun $remove_edges (el gr)
  (require-graph-or-digraph 'remove_edges 2 gr)
  (if (not ($listp el))
      ($error "Argument 1 to remove_edges is not a list."))
  (dolist (e (cdr el))
    ($remove_edge e gr))
  '$done)

(defun remove-edge (e gr)
  (let ((u (first e)) (v (second e)))
    (if (graph-p gr)
	(progn
	  (setf (gethash u (graph-neighbors gr))
		(remove v (gethash u (graph-neighbors gr)) :count 1))
	  (setf (gethash v (graph-neighbors gr))
		(remove u (gethash v (graph-neighbors gr)) :count 1))
	  (clear-edge-weight e gr)
	  (decf (graph-order gr))
	  (setf (graph-edges gr)
		(remove `(,u ,v) (graph-edges gr) :test #'equal :count 1)))
	(progn
	  (setf (gethash u (digraph-out-neighbors gr))
		(remove v (gethash u (digraph-out-neighbors gr)) :count 1))
	  (setf (gethash v (digraph-in-neighbors gr))
		(remove u (gethash v (digraph-in-neighbors gr)) :count 1))
	  (clear-edge-weight e gr)
	  (decf (digraph-order gr))
	  (setf (digraph-edges gr)
		(remove `(,u ,v) (digraph-edges gr) :test #'equal :count 1))))
    '$done))

(defun $contract_edge (e gr)
  (require-medge 'contract_edge 1 e)
  (require-graph 'contract_edge 2 gr)
  (let* ((e1 (m-edge-to-l-edge e)) (u (first e1)) (v (second e1)))
    (dolist (x (neighbors v gr))
      (if (not (= x u))
	  (let ((e2 (list (min x u) (max x u))))
	    (if (not (is-edge-in-graph e2 gr))
		(add-edge e2 gr)))))
    (remove-vertex v gr))
  '$done)

(defun $contract_edges (el gr)
  (require-graph-or-digraph 'contract_edges 2 gr)
  (if (not ($listp el))
      ($error "Argument 1 to contract_edges is not a list."))
  (dolist (e (cdr el))
    ($contract_edge e gr))
  '$done)

(defun $get_edge_weight (e gr &optional default)
  (require-medge 'get_edge_weight 1 e)
  (require-graph-or-digraph 'get_edge_weight 2 gr)
  (if (not ($is_edge_in_graph e gr))
      (if (null default)
	  ($error "get_edge_weight: edge not in graph")
	  (return-from $get_edge_weight default)))
  (let ((w (if (graph-p gr)
	       (get-edge-weight (m-edge-to-l-edge e) gr)
	       (get-edge-weight (m-edge-to-l-dedge e) gr))))
    (or w 1)))

(defun get-edge-weight (e gr)
  (let*
      ((edge-weights
	(if (graph-p gr)
	    (graph-edge-weights gr)
	    (digraph-edge-weights gr))))
    (gethash e edge-weights)))

(defun $clear_edge_weight (e gr)
  (require-medge 'clear_edge_weight 1 e)
  (require-graph-or-digraph 'clear_edge_weight 2 gr)
  (if (not ($is_edge_in_graph e gr))
      ($error "clear_edge_weight: edge not in graph"))
  (if (graph-p gr)
      (clear-edge-weight (m-edge-to-l-edge e) gr)
      (clear-edge-weight (m-edge-to-l-dedge e) gr)))

(defun clear-edge-weight (e gr)
  (let*
      ((edge-weights
	(if (graph-p gr)
	    (graph-edge-weights gr)
	    (digraph-edge-weights gr))))
    (remhash e edge-weights)
    '$done))

(defun $set_edge_weight (e w gr)
  (require-medge 'set_edge_weight 1 e)
  (require-graph-or-digraph 'set_edge_weight 3 gr)
  (if (not ($is_edge_in_graph e gr))
      ($error "set_edge_weight: edge not in graph"))
  (if (graph-p gr)
      (set-edge-weight (m-edge-to-l-edge e) w gr)
      (set-edge-weight (m-edge-to-l-dedge e) w gr)))

(defun set-edge-weight (e w gr)
  (let*
      ((edge-weights
	(if (graph-p gr)
	    (graph-edge-weights gr)
	    (digraph-edge-weights gr))))
    (setf (gethash e edge-weights) w)
    '$done))

(defun $connect_vertices (sources sinks gr)
  (require-graph 'connect_vertices 3 gr)
  (if ($listp sources)
      (setq sources (cdr sources))
      (setq sources `(,sources)))
  (if ($listp sinks)
      (setq sinks (cdr sinks))
      (setq sinks `(,sinks)))
  (dolist (u sources)
    (dolist (v sinks)
      ($add_edge `((mlist simp) ,u ,v) gr)))
  '$done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; graph definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $empty_graph (n)
  (let ((gr (make-graph)))
    (dotimes (i n)
      (add-vertex i gr))
    gr))

(defun $empty_digraph (n)
  (let ((gr (make-digraph)))
    (dotimes (i n)
      (add-vertex i gr))
    gr))

(defun $create_graph (v_list e_list &optional dir)
  (if (not (or (integerp v_list) ($listp v_list)))
      ($error "Argument 1 to create_graph is not a list."))
  (if (not ($listp e_list))
      ($error "Argument 2 to create_graph is not a list."))
  (let ((gr (if dir (make-digraph) (make-graph))))
    (if (integerp v_list)
	(dotimes (v v_list)
	  ($add_vertex v gr))
	(dolist (v (cdr v_list))
	  (if ($listp v)
	      (progn
		($add_vertex ($first v) gr)
		($set_vertex_label ($first v) ($second v) gr))
	      ($add_vertex v gr))))
    (dolist (e (cdr e_list))
      (if ($listp ($first e))
	  (progn
	    ($add_edge ($first e) gr)
	    ($set_edge_weight ($first e) ($second e) gr))
	  ($add_edge e gr)))
    gr))

(defun $cycle_graph (n)
  (let ((g ($empty_graph n)))
    (dotimes (i (1- n))
      (add-edge (list i (1+ i)) g))
    (add-edge (list 0 (1- n)) g)
    g))

(defun $cycle_digraph (n)
  (let ((g ($empty_digraph n)))
    (dotimes (i (1- n))
      (add-edge (list i (1+ i)) g))
    (add-edge (list (1- n) 0) g)
    g))

(defun $path_graph (n)
  (let ((g ($empty_graph n)))
    (dotimes (i (1- n))
      (add-edge (list i (1+ i)) g))
    g))

(defun $path_digraph (n)
  (let ((g ($empty_digraph n)))
    (dotimes (i (1- n))
      (add-edge (list i (1+ i)) g))
    g))

(defun $petersen_graph (&optional n d)
  (if (null d)
      (setq n 5 d 2)
      (if (not (and (integerp n) (integerp d)))
	  ($error "Arguments to petersen_graph are not integers!")))
  (let ((g ($empty_graph (* 2 n))))
    (dotimes (i n)
      (add-edge `(,i ,(+ n i)) g)
      (let* ((u (+ i n)) (v (+ (mod (+ i d) n) n))
	     (e1 (min v u)) (e2 (max v u)))
	(add-edge `(,e1 ,e2) g))
      (let* ((u (mod (1+ i) n)) (e1 (min u i)) (e2 (max u i)))
	(add-edge `(,e1 ,e2) g)))
    g))

(defun $dodecahedron_graph ()
  (let
      ((edg '((0 1) (1 2) (2 3) (3 4) (0 4) (0 15) (1 19)
	      (2 18) (3 17) (4 16) (10 15) (10 16) (11 16)
	      (11 17) (12 17) (12 18) (13 18) (13 19) (14 15)
	      (14 19) (6 10) (8 11) (9 12) (7 13) (5 14) (5 6)
	      (6 8) (8 9) (7 9) (5 7)))
       (gr ($empty_graph 20)))
    (dolist (e edg)
      (add-edge e gr))
    gr))

(defun $heawood_graph ()
  (let
      ((edg '((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8)
	      (8 9) (9 10) (10 11) (11 12) (12 13) (0 13)
	      (0 9) (1 6) (2 11) (3 8) (4 13) (5 10) (7 12)))
       (gr ($empty_graph 14)))
    (dolist (e edg)
      (add-edge e gr))
    gr))

(defun $complement_graph (gr)
  (require-graph 'complement_graph 1 gr)
  (let*
      ((co (make-graph))
       (vrt (vertices gr)))
    (dolist (v vrt)
      (add-vertex v co))
    (dolist (u vrt)
      (dolist (v vrt)
	(if (and (< u v) (not (is-edge-in-graph `(,u ,v) gr)))
	    (add-edge `(,u ,v) co))))
    co))

(defun $complete_graph (n)
  (if (not (and (integerp n ) (> n 0)))
      ($error "Argument 1 to complete_graph is not a positive integer"))
  (let ((g ($empty_graph n)))
    (dotimes (i n)
      (do
       ((j (1+ i) (1+ j)))
       ((= j n))
	(add-edge `(,i ,j) g)))
    g))

(defun $from_adjacency_matrix (m)
  (if (not ($matrixp m))
      ($error "Argument 1 to from_adjacency_matrix is not a matrix"))
  (if (not (= ($length m) ($length ($first m))))
      ($error "Argument 1 to from_adjacency_matrix is not a square matrix"))
  (let* ((n ($length m)) (g ($empty_graph n)))
    (dotimes (i n)
      (do ((j (1+ i) (1+ j)))
	  ((= j n))
	(if (not (= 0 (nth (1+ i) (nth (1+ j) m))))
	    (add-edge `(,i ,j) g))))
    g))

(defun $graph_union (g1 g2)
  (require-graph 'graph_union 1 g1)
  (require-graph 'graph_union 2 g2)
  (let ((g (make-graph)) (n (1+ (apply #'max (graph-vertices g1)))))
    (dolist (v (graph-vertices g1))
      (add-vertex v g))
    (dolist (e (graph-edges g1))
      (add-edge e g))
    (dolist (v (graph-vertices g2))
      (add-vertex (+ n v) g))
    (dolist (e (graph-edges g2))
      (add-edge (list (+ n (first e)) (+ n (second e))) g))
    g))

(defun get-canonical-names (l)
  (let ((names ()) (i 0))
    (dolist (v l)
      (push `(,v . ,i) names)
      (setq i (1+ i)))
    names))

(defun $graph_product (g1 g2)
  (require-graph 'graph_product 1 g1)
  (require-graph 'graph_product 2 g2)
  (let*
      ((names1 (get-canonical-names (graph-vertices g1)))
       (names2 (get-canonical-names (graph-vertices g2)))
       (size1 (graph-size g1))
       (size2 (graph-size g2))
       (size (* size1 size2))
       (g ($empty_graph size)))
    (dolist (e (graph-edges g1))
      (dolist (v (graph-vertices g2))
	(let*
	    ((v1 (cdr (assoc (first e) names1)))
	     (v2 (cdr (assoc (second e) names1)))
	     (u (cdr (assoc v names2)))
	     (f (list (+ (* u size1) v1) (+ (* u size1) v2)))
	     (f (list (apply #'min f) (apply #'max f))))
	  (add-edge f g))))
    (dolist (e (graph-edges g2))
      (dolist (v (graph-vertices g1))
	(let*
	    ((v1 (cdr (assoc (first e) names2)))
	     (v2 (cdr (assoc (second e) names2)))
	     (u (cdr (assoc v names1)))
	     (f (list (+ (* v1 size1) u) (+ (* v2 size1) u)))
	     (f (list (apply #'min f) (apply #'max f))))
	  (add-edge f g))))
    g))

(defun $line_graph (gr)
  (require-graph 'line_graph 1 gr)
  (let* ((edge-list
	  (get-canonical-names (graph-edges gr))) (n (graph-order gr))
	 (g ($empty_graph n)))
    (dotimes (i n)
      (do
       ((j (1+ i) (1+ j)))
       ((= j n))
	(let ((e (car (rassoc i edge-list))) (f (car (rassoc j edge-list))))
	  (if
	   (or (member (first e) f) (member (second e) f))
	   (add-edge `(,i ,j) g)))))
    g))

(defun $random_graph (n p)
  (if (not (integerp n))
      ($error "Argument 1 to random_graph is not an integer"))
  (if (not (floatp p))
      ($error "Argument 2 to random_graph is not a float"))
  (let ((g ($empty_graph n)))
    (dotimes (i n)
      (do ((j (1+ i) (1+ j)))
	  ((= j n))
	(if (< (/ (random 10000) 10000.0) p)
	    (add-edge `(,i ,j) g))))
    g))

(defun $random_graph1 (n m)
  (if (not (integerp n))
      ($error "Argument 1 to random_graph is not an integer"))
  (if (not (integerp m))
      ($error "Argument 2 to random_graph is not an integer"))
  (if (< (* n (1- n)) (* 2 m))
      ($error "random_graph1: no such graph"))
  (if (< (* n (1- n)) (* 4 m))
      (return-from $random_graph1
	($complement_graph ($random_graph1 n (- (/ (* n (1- n)) 2) m)))))
  (let ((g ($empty_graph n)))
    (do ((i 0)) ((= i m))
      (let ((u (random n)) (v (random n)))
	(if (not (= u v))
	    (let ((e (list (min u v) (max u v))))
	      (if (not (is-edge-in-graph e g))
		  (progn
		    (setq i (1+ i))
		    (add-edge e g)))))))
    g))

(defun $random_digraph (n p)
  (if (not (integerp n))
      ($error "Argument 1 to random_digraph is not an integer"))
  (if (not (floatp p))
      ($error "Argument 2 to random_digraph is not a float"))
  (let ((g ($empty_digraph n)))
    (dotimes (i n)
      (dotimes (j n)
	(if (and (not (= i j)) (< (/ (random 10000) 10000.0) p))
	    (add-edge `(,i ,j) g))))
    g))

(defun $random_tournament (n)
  (if (not (integerp n))
      ($error "Argument 1 to random_tournament is not an integer"))
  (let ((g ($empty_digraph n)))
    (dotimes (i n)
      (do ((j (1+ i) (1+ j)))
	  ((= j n))
	(if (and (not (= i j)) (< (/ (random 10000) 10000.0) 0.5))
	    (add-edge `(,i ,j) g)
	    (add-edge `(,j ,i) g))))
    g))

(defun $random_tree (n)
  (if (or (not (integerp n)) (<= n 0))
      ($error "Argument 1 to random_tree is not a positive integer"))
  (let*
      ((tr ($empty_graph n))
       (vrt (remove 0 (graph-vertices tr) :count 1))
       (tree-vrt '(0)))
    (dotimes (i (1- n))
      (let
	  ((u (nth (random (length vrt)) vrt))
	   (v (nth (random (length tree-vrt)) tree-vrt)))
	(setq vrt (remove u vrt :count 1))
	(push u tree-vrt)
	(add-edge (list (min u v) (max u v)) tr)))
    tr))

(defun $underlying_graph (gr)
  (require-digraph 'underlying_graph 1 gr)
  (let ((g (make-graph)))
    (dolist (v (graph-vertices gr))
      (add-vertex v g))
    (dolist (e (graph-edges gr))
      (let ((u (first e)) (v (second e)))
	(let ((e1 (list (apply #'min e) (apply #'max e))))
	  (if (not (is-edge-in-graph e1 g))
	      (add-edge `(,u ,v) g)))))
    g))

(defun $induced_subgraph (vl gr)
  (require-graph 2 'induced_subgraph gr)
  (if (not ($listp vl))
      ($error "First argument to induced_subgraph is not a list."))
  (let
      ((v_l (cdr vl))
       (g (make-graph)))
    (dolist (v v_l)
      (if (not (is-vertex-in-graph v gr))
	  ($error
	   "induced_subgraph: second argument is not a list of vertices"))
      (add-vertex v g)
      (let ((l (get-vertex-label v gr)))
	(if l (set-vertex-label v l g))))
    (dolist (e (graph-edges gr))
      (let ((u (first e)) (v (second e)))
	(if (and (member u v_l) (member v v_l))
	    (add-edge e g))))
    g))

(defun $wheel_graph (n)
  (if (not (and (integerp n) (> n 3)))
      ($error "wheel_graph: first argument is no an integer greater than 3"))
  (let ((g ($cycle_graph n)))
    (add-vertex n g)
    (dotimes (i n)
      (add-edge `(,i ,n) g))
    g))

(defun $circulant_graph (n l)
  (if (not (and (integerp n) (> n 0)))
      ($error "Argument 1 to circulant_graph is not a positive integer."))
  (if (not ($listp l))
      ($error "Argument 2 to circulant_graph is not a list."))
  (let ((g ($empty_graph n)))
    (dolist (d (cdr l))
      (if (not (and (integerp d) (> d 0)))
	  ($error
	   "Argument 2 to circulant graph is no a list of positive integers"))
      (dotimes (i n)
	(let ((e `(,i ,(mod (+ i d) n))))
	  (setq e (list (apply #'min e) (apply #'max e)))
	  (add-edge e g))))
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; graph properties
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Connected components
;;;

(defun $connected_components (gr)
  (require-graph 'connected_components 1 gr)
  (if (= 0 (graph-size gr))
      (return-from $connected_components '((mlist simp))))
  (let ((components ()) (visited ()))
    (do ((vrt (copy-list (graph-vertices gr))))
	((= (length vrt) 0))
      (let ((c ()) (v (first vrt)) (active ()))
	(push v active)
	(do ()
	    ((null active))
	  (let ((x (pop active)))
	    (push x c)
	    (push x visited)
	    (setq vrt (remove x vrt :count 1))
	    (dolist (u (neighbors x gr))
	      (if (not (or (member u visited) (member u active)))
		  (push u active)))))
	(push `((mlist simp) ,@c) components)))
    `((mlist simp) ,@components)))

(defun $is_connected (gr)
  (require-graph 'is_connected 1 gr)
  (eq ($length ($connected_components gr)) 1))

(defun $is_tree (gr)
  (require-graph 'is_tree 1 gr)
  (and ($is_connected gr) (= (graph-size gr) (1+ (graph-order gr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Adjacency matrix and Laplacian matrix
;;;

(defun $adjacency_matrix (gr)
  (require-graph 'adjacency_matrix 1 gr)
  (let ((m ($zeromatrix (graph-size gr) (graph-size gr)))
	(names (get-canonical-names (vertices gr))))
    (dolist (e (graph-edges gr))
      (setf (nth (1+ (cdr (assoc (first e) names)))
		 (nth (1+ (cdr (assoc (second e) names))) m)) 1)
      (setf (nth (1+ (cdr (assoc (second e) names)))
		 (nth (1+ (cdr (assoc (first e) names))) m)) 1))
    m))

(defun $laplacian_matrix (gr)
  (require-graph 'laplacian_matrix 1 gr)
  (let ((m ($zeromatrix (graph-size gr) (graph-size gr)))
	(names (get-canonical-names (vertices gr))))
    (dolist (v (graph-vertices gr))
      (setf (nth (1+ (cdr (assoc v names)))
		 (nth (1+ (cdr (assoc v names))) m))
	    (length (neighbors v gr))))
    (dolist (e (graph-edges gr))
      (setf (nth (1+ (cdr (assoc (first e) names)))
		 (nth (1+ (cdr (assoc (second e) names))) m)) -1)
      (setf (nth (1+ (cdr (assoc (second e) names)))
		 (nth (1+ (cdr (assoc (first e) names))) m)) -1))
    m))

(defun $graph_charpoly (gr x)
  (require-graph 'graph_charpoly 1 gr)
  (let (($ratmx t))
    ($charpoly ($adjacency_matrix gr) x)))

(defun $graph_eigenvalues (gr)
  (require-graph 'graph_eigenvalues 1 gr)
  (let (($ratmx t))
    (mfuncall '$eigenvalues ($adjacency_matrix gr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; girth, odd_girth
;;;

(defun $girth (gr)
  (require-graph 'girth 1 gr)
  (girth gr nil))

(defun $odd_girth (gr)
  (require-graph 'odd_girth 1 gr)
  (girth gr t))

(defun girth (gr odd)
  (let ((girth (1+ (graph-size gr))))
    (dolist (v (graph-vertices gr))
      (let
	  ((visited (list v))
	   (active (list v))
	   (next ())
	   (depth 1))
	(do ()
	    ((or (null active) (> (* 2 depth) girth) (<= girth 3)))
	  (setq next ())
	  (dolist (u active)
	    (dolist (w (neighbors u gr))
	      (if (not (member w visited))
		  (progn
		    (push w visited)
		    (push w next))
		  (progn
		    (if (member w active)
			(setq girth (- (* 2 depth) 1)))
		    (if (and (not odd) (member w next))
			(setq girth (min girth (* 2 depth))))))))
	  (setq active next)
	  (setq depth (1+ depth)))))
    (if (> girth (graph-size gr))
	'$inf
	girth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; diameter, radius
;;;

(defun $diameter (gr)
  (require-graph 'diameter 1 gr)
  (if (not ($is_connected gr))
      ($error "diameter: graph is not connected."))
  (let ((diameter 0))
    (dolist (v (graph-vertices gr))
      (let
	  ((visited (list v))
	   (active (list v))
	   (next ())
	   (depth -1))
	(do ()
	    ((null active))
	  (setq next ())
	  (dolist (u active)
	    (dolist (w (neighbors u gr))
	      (if (not (member w visited))
		  (progn
		    (push w visited)
		    (push w next)))))
	  (setq active next)
	  (setq depth (1+ depth)))
	(if (> depth diameter)
	    (setq diameter depth))))
    diameter))

(defun $radius (gr)
  (require-graph 'radius 1 gr)
  (if (not ($is_connected gr))
      ($error "radius: graph is not connected."))
  (let ((radius (graph-size gr)))
    (dolist (v (graph-vertices gr))
      (let
	  ((visited (list v))
	   (active (list v))
	   (next ())
	   (depth -1))
	(do ()
	    ((null active))
	  (setq next ())
	  (dolist (u active)
	    (dolist (w (neighbors u gr))
	      (if (not (member w visited))
		  (progn
		    (push w visited)
		    (push w next)))))
	  (setq active next)
	  (setq depth (1+ depth)))
	(if (< depth radius)
	    (setq radius depth))))
    radius))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; bipartition
;;;

(defun $bipartition (gr)
  (require-graph 'bipartition 1 gr)
  (if (= (graph-size gr) 0)
      (return-from $bipartition `((mlist simp))))
  (let ((components (cdr ($connected_components gr))) (A ()) (B ()))
    (dolist (c components)
      (let ((partition (bi-partition (first (cdr c)) gr)))
	(if (null partition)
	    (return-from $bipartition `((mlist simp)))
	    (progn
	      (setq A (append A (first partition)))
	      (setq B (append B (second partition)))))))
    `((mlist simp) ((mlist simp) ,@A) ((mlist simp) ,@B))))

(defun bi-partition (v gr)
  (let
      ((A ())
       (B ())
       (visited ())
       (active `(,v))
       (colors (make-hash-table)))
    (setf (gethash v colors) 1)
    (do ()
	((null active))
      (let*
	  ((w (pop active))
	   (wc (gethash w colors)))
	(push w visited)
	(if (= wc 1)
	    (push w A)
	    (push w B))
	(dolist (u (neighbors w gr))
	  (if (member u visited)
	      (if (= (gethash u colors) wc)
		  (return-from bi-partition ()))
	      (if (not (member u active))
		  (progn
		    (push u active)
		    (setf (gethash u colors) (- 1 wc))))))))
    `(,A ,B)))

(defun $is_bipartite (gr)
  (require-graph 'is_bipartite 1 gr)
  (> ($length ($bipartition gr)) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 2-connectivity
;;;

(defun $is_biconnected (gr)
  (require-graph 'is_biconnected 1 gr)
  (eq ($length ($biconnected_components gr)) 1))

(defun $biconnected_components (gr)
  (require-graph 'biconnected_components 1 gr)
  (if (= 0 (graph-size gr))
      `((mlist simp))
      (let
	  ((bicomp `((mlist simp)))
	   (comp (cdr ($connected_components gr))))
	(dolist (c comp)
	  (if (= ($length c) 1)
	      (setq bicomp ($append bicomp `((mlist simp) ,c)))
	      (setq bicomp ($append bicomp (bicomponents ($first c) gr)))))
	bicomp)))

(defvar *dfs-bicomp-depth* 0)
(defvar *dfs-bicomp-num* ())
(defvar *dfs-bicomp-low-pt* ())
(defvar *dfs-bicomp-edges* ())
(defvar *bicomponents* ())

(defun bicomponents (v gr)
  (let ((bicomp ()))
    (setq *dfs-bicomp-depth* 0)
    (setq *dfs-bicomp-num* (make-hash-table))
    (setq *dfs-bicomp-low-pt* (make-hash-table))
    (setq *dfs-bicomp-edges* (make-hash-table))
    (setq *bicomponents* ())
    (dolist (v (graph-vertices gr))
      (setf (gethash v *dfs-bicomp-num*) 0))
    (dfs-bicomponents gr v)
    (dolist (c *bicomponents*)
      (let ((curr-comp ()))
	(dolist (e c)
	  (let ((u (first e)) (v (second e)))
	    (if (not (member u curr-comp))
		(push u curr-comp))
	    (if (not (member v curr-comp))
		(push v curr-comp))))
	(setq bicomp (cons `((mlist simp) ,@(sort curr-comp #'<)) bicomp))))
    `((mlist simp) ,@bicomp)))

(defun dfs-bicomponents (gr w)
  (setq *dfs-bicomp-depth* (1+ *dfs-bicomp-depth*))
  (setf (gethash w *dfs-bicomp-num*) *dfs-bicomp-depth*)
  (setf (gethash w *dfs-bicomp-low-pt*) *dfs-bicomp-depth*)
  (dolist (u (neighbors w gr))
    (if (< (gethash u *dfs-bicomp-num*) (gethash w *dfs-bicomp-num*))
	(push `(,w ,u) *dfs-bicomp-edges*))
    (if (= 0 (gethash u *dfs-bicomp-num*))
	(progn
	  (dfs-bicomponents gr u)
	  (if (>= (gethash u *dfs-bicomp-low-pt*)
		  (gethash w *dfs-bicomp-num*))
	      (let ((e 0) (comp ()))
		(do ()
		    ((equal e `(,w ,u)))
		  (setq e (pop *dfs-bicomp-edges*))
		  (push e comp))
		(push comp *bicomponents*))
	      (setf (gethash w *dfs-bicomp-low-pt*)
		    (min (gethash w *dfs-bicomp-low-pt*)
			 (gethash u *dfs-bicomp-low-pt*)))))
	(setf (gethash w *dfs-bicomp-low-pt*)
	      (min (gethash w *dfs-bicomp-low-pt*)
		   (gethash u *dfs-bicomp-num*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; strong connectivity
;;;

(defvar *scon-low* nil)
(defvar *scon-dfn* nil)
(defvar *scon-comp* nil)
(defvar *scon-st*  nil)
(defvar *scon-vrt* nil)
(defvar *scon-depth* 0)

(defun $is_sconnected (gr)
  (require-digraph 'strong_components 1 gr)
  (eq ($length ($strong_components gr)) 1))

(defun $strong_components (gr)
  (require-digraph 'strong_components 1 gr)
  (if (= 0 (digraph-size gr))
      `((mlist simp))
      (let ((res))
	(setq *scon-low* (make-hash-table))
	(setq *scon-dfn* (make-hash-table))
	(setq *scon-comp* ())
	(setq *scon-st* ())
	(setq *scon-vrt* (digraph-vertices gr))
	(loop while (not (null *scon-vrt*)) do
	      (setq *scon-depth* 0)
	      (dfs-strong-components gr (first *scon-vrt*))
	      (dolist (c *scon-comp*)
		(setq res (cons c res)))
	      (setq *scon-comp* ()))
	`((mlist simp) ,@res))))

(defun dfs-strong-components (gr v)
  (incf *scon-depth*)
  (setf (gethash v *scon-dfn*) *scon-depth*)
  (setf (gethash v *scon-low*) *scon-depth*)
  (setf *scon-vrt* (remove v *scon-vrt* :count 1))
  (setf *scon-st* (cons v *scon-st*))
  (dolist (u (neighbors v gr))
    (if (gethash u *scon-dfn*)
	(if (and (< (gethash u *scon-dfn*) (gethash v *scon-dfn*))
		 (member u *scon-st*))
	    (setf (gethash v *scon-low*)
		  (min (gethash v *scon-low*)
		       (gethash u *scon-dfn*))))
	(progn
	  (dfs-strong-components gr u)
	  (setf (gethash v *scon-low*)
		(min (gethash v *scon-low*)
		     (gethash u *scon-low*))))))
  (if (= (gethash v *scon-low*) (gethash v *scon-dfn*))
      (let ((x (pop *scon-st*))
	    (comp ()))
	(loop while (not (= x v)) do
	  (setq comp (cons x comp))
	  (setq x (pop *scon-st*)))
	(setf comp (cons x comp))
	(setq *scon-comp* (cons `((mlist simp) ,@comp) *scon-comp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; max_flow (augmenting paths)
;;;

(defun $max_flow (net source sink)
	(require-digraph 'max_flow 1 net)
	(require-vertex 'max_flow 2 source)
	(require-vertex 'max_flow 3 sink)
	(require-vertex-in-graph 'max_flow source net)
	(require-vertex-in-graph 'max_flow sink net)
	(let
	    ((d (make-hash-table)) (active ()) (lbls (make-hash-table))
	     (flow (make-hash-table :test #'equal)) (val 0)
	     ($ratprint nil))
	  (dolist (e (digraph-edges net))
	    (setf (gethash e flow) 0))
	  (dolist (v (digraph-vertices net))
	    (setf (gethash v d) '$inf))
	  (setf (gethash source lbls) `(,source -1 $inf))
	  (push source active)
	  (do ()
	      ((null active))
	    (let ((v (pop active)))
	      (dolist (w (out-neighbors v net))
		(if (and (null (gethash w lbls))
			 (mlsp (gethash `(,v ,w) flow)
			       (get-edge-weight `(,v ,w) net)))
		    (progn
		      (setf (gethash w d)
			    (mfuncall '$min
				      (m- (get-edge-weight `(,v ,w) net)
					  (gethash `(,v ,w) flow))
				      (gethash v d)))
		      (setf (gethash w lbls) `(,v 1 ,(gethash w d)))
		      (push w active))))
	      (dolist (w (in-neighbors v net))
		(if (and (null (gethash w lbls))
			 (mgrp (gethash `(,w ,v) flow) 0))
		    (progn
		      (setf (gethash w d) (mfuncall '$min
						    (gethash `(,w ,v) flow)
						    (gethash v d)))
		      (setf (gethash w lbls) `(,v -1 ,(gethash w d)))
		      (push w active))))
	      (if (gethash sink lbls)
		  (let ((dd (third (gethash sink lbls))) (w sink))
		    (setq val (m+ dd val))
		    (do ()
			((= w source))
		      (let ((v1 (first (gethash w lbls)))
			    (vl (second (gethash w lbls))))
			(if (= vl 1)
			    (setf (gethash `(,v1 ,w) flow)
				  (m+ (gethash `(,v1 ,w) flow) dd))
			    (setf (gethash `(,w ,v1) flow)
				  (m- (gethash `(,w ,v1) flow) dd)))
			(setq w v1)))
		    (setq lbls (make-hash-table))
		    (setf (gethash source lbls) `(,source -1 $inf))
		    (dolist (v1 (digraph-vertices net))
		      (setf (gethash v1 d) '$inf))
		    (setq active `(,source))))))
	  (let ((max-flow ()))
	    (dolist (e (digraph-edges net))
	      (push `((mlist simp) ((mlist simp) ,@e)
		      ,(gethash e flow)) max-flow))
	    `((mlist simp) ,val ((mlist simp) ,@max-flow)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shortest path
;;;

(defun $shortest_path (u v g)
  (require-graph-or-digraph 'shortest_path 3 g)
  (require-vertex 'shortest_path 1 u)
  (require-vertex 'shortest_path 2 v)
  (require-vertex-in-graph 'shortest_path v g)
  (require-vertex-in-graph 'shortest_path u g)
  (let ((active `(,u)) (visited ()) (previous (make-hash-table)))
    (do ()
	((or (null active) (member v visited)))
      (let ((next ()))
	(dolist (w active)
	  (push w visited)
	  (dolist (x (neighbors w g))
	    (if (not (or (member x active) (member x visited) (member x next)))
		(progn
		  (setf (gethash x previous) w)
		  (push x next)))))
	(setq active next)))
    (if (member v visited)
	(let ((path `(,v)))
	  (do ((x v))
	      ((= x u))
	    (setq x (gethash x previous))
	    (push x path))
	  `((mlist simp) ,@path))
	`((mlist simp)))))

(defun $vertex_distance (u v g)
  (require-graph-or-digraph 'shortest_path 3 g)
  (require-vertex 'shortest_path 1 u)
  (require-vertex 'shortest_path 2 v)
  (require-vertex-in-graph 'shortest_path v g)
  (require-vertex-in-graph 'shortest_path u g)
  (let ((d ($length ($shortest_path u v g))))
    (if (> d 0)
	(1- d)
	'$inf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; minimum spanning tree
;;;

(defun edge-weight-1 (e gr)
  (cond ((gethash e (graph-edge-weights gr)))
	(t 1)))

(defun graph-edges-with-weights (gr)
  (let ((edges (graph-edges gr)) (edges-with-weights ()))
    (dolist (e edges)
      (push `(,e ,(edge-weight-1 e gr)) edges-with-weights))
    edges-with-weights))

(defun edges-by-weights (gr)
  (let ((edges (graph-edges-with-weights gr)))
    (sort edges #'(lambda (u v) (mlsp (second u) (second v))))))

(defun in-same-part (u v p)
  (let ((up u) (vp v))
    (do ()
	((= up (gethash up p)))
      (let ((up1 (gethash up p)))
	(setf (gethash up p) (gethash up1 p)))
      (setq up (gethash up p)))
    (do ()
	((= vp (gethash vp p)))
      (let ((vp1 (gethash vp p)))
	(setf (gethash vp p) (gethash vp1 p)))
      (setq vp (gethash vp p)))
    (= up vp)))

(defun join-parts (u v p)
  (let ((up u) (vp v))
    (do ()
	((= up (gethash up p)))
      (setq up (gethash up p)))
    (do ()
	((= vp (gethash vp p)))
      (setq vp (gethash vp p)))
    (setf (gethash up p) (gethash vp p))))

(defun $minimum_spanning_tree (gr)
  (require-graph 'minimum_spanning_tree 1 gr)
  (let ((edges (edges-by-weights gr)) (tr (make-graph))
	(part (make-hash-table)))
    (dolist (v (graph-vertices gr))
      (add-vertex v tr)
      (setf (gethash v part) v))
    (dolist (e edges)
      (let ((u (caar e)) (v (cadar e)))
	(if (not (in-same-part u v part))
	    (progn
	      (add-edge `(,u ,v) tr)
	      (join-parts u v part)))))
    tr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hamilton cycle
;;;

(defvar *hamilton-cycle* ())

(defun $hamilton_cycle (gr)
  (require-graph-or-digraph 'hamilton_cycle 1 gr)
  (if (and (graph-p gr) (not ($is_biconnected gr)))
      (return-from $hamilton_cycle `((mlist simp))))
  (setq *hamilton-cycle* ())
  (hamilton-cycle `(,($first_vertex gr)) gr)
  `((mlist simp) ,@(reverse *hamilton-cycle*)))

(defun hamilton-cycle (part gr)
  (if (null *hamilton-cycle*)
      (progn
	(if (= (length part) (if (graph-p gr)
				 (graph-size gr)
				 (digraph-size gr)))
	    (if (member (car (last part)) (neighbors (first part) gr))
		(setq *hamilton-cycle* (append (last part) part))))
	(dolist (v (neighbors (car part) gr))
	  (if (null (member v part))
	      (hamilton-cycle (cons v part) gr))))))

(defun $hamilton_path (gr)
  (require-graph-or-digraph 'hamilton_path 1 gr)
  (let ((h ($copy_graph gr))
	(x (1+ (mfuncall #'$lmax `((mlist simp) ,@(vertices gr))))))
    (add-vertex x h)
    (dolist (v (vertices gr))
      (add-edge `(,x ,v) h)
      (if (digraph-p gr)
	  (add-edge `(,v ,x) h)))
    (setq *hamilton-cycle* ())
    (hamilton-cycle `(,x) h)
    `((mlist simp) ,@(remove x (reverse *hamilton-cycle*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; maximal clique
;;;

(defvar *maximal-clique* ())

(defun vertices-by-degrees (gr)
  (let ((vrt ()))
    (dolist (v (vertices gr))
      (push `(,v ,(length (neighbors v gr))) vrt))
    (setq vrt (sort vrt #'(lambda (u v) (> (second u) (second v)))))
    (mapcar #'first vrt)))

(defun greedy-color (gr)
  (let ((coloring (make-hash-table))
	(available-colors (make-hash-table)) (tmp ()))
    (dotimes (i (graph-size gr))
      (push i tmp))
    (setq tmp (reverse tmp))
    (dolist (v (graph-vertices gr))
      (setf (gethash v available-colors) (copy-tree tmp)))
    (dolist (v (vertices-by-degrees gr))
      (let ((c (car (gethash v available-colors))))
	(setf (gethash v coloring) c)
	(dolist (u (neighbors v gr))
	  (setf (gethash u available-colors)
		(remove c (gethash u available-colors) :count 1)))))
    coloring))

(defun $max_clique (gr)
  (require-graph 'max_clique 1 gr)
  (setq *maximal-clique* ())
  (let ((v) (coloring) (h ($copy_graph gr)))
    (do () ((= 0 (graph-size h)))
      (setq coloring (greedy-color h))
      (setq v ($second ($max_degree h)))
      (extend-clique `(,v) (neighbors v h) coloring h)
      (remove-vertex v h)))
  `((mlist simp) ,@(sort *maximal-clique* #'<)))

(defun $max_independent_set (gr)
  (require-graph 'max_independent_set 1 gr)
  ($max_clique ($complement_graph gr)))

(defun extend-clique (clique neigh coloring gr)
  (if (= (length neigh) 0)
      ;; did we get to a clique maximal clique
      (if (> (length clique) (length *maximal-clique*))
	  (progn
	    (setq *maximal-clique* clique)
	    (return-from extend-clique)))
      ;; can we do better?
      (let ((colors ()))
	(dolist (x neigh)
	  (if (not (member (gethash x coloring) colors))
	      (push (gethash x coloring) colors)))
	(if (> (+ (length clique) (length colors)) (length *maximal-clique*))
	    ;; try improving
	    (do () ((= (length neigh) 0))
	      (let* ((x (first neigh)) (new-clique (cons x clique))
		     (new-neigh ()))
		(dolist (y neigh)
		  (if (is-edge-in-graph `(,(min x y) ,(max x y)) gr)
		      (push y new-neigh)))
		(extend-clique new-clique new-neigh coloring gr)
		(setq neigh (remove x neigh))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; colorings
;;;

(defun $vertex_coloring (g)
  (require-graph 'vertex_coloring 1 g)
  (let ((col (dsatur g)) (res ()) (chnumber 0))
    (dolist (v (vertices g))
      (push `((mlist simp) ,v ,(gethash v col)) res)
      (setq chnumber (max chnumber (gethash v col))))
    `((mlist simp) ,chnumber ((mlist simp) ,@res))))

(defun $chromatic_number (g)
  (require-graph 'chromatic_number 1 g)
  ($first ($vertex_coloring g)))

(defun $edge_coloring (gr)
  (require-graph 'edge_coloring 1 gr)
  (let* ((edge-list (get-canonical-names (graph-edges gr)))
	 (n (graph-order gr))
	 (g ($empty_graph n)))
    (dotimes (i n)
      (do
       ((j (1+ i) (1+ j)))
       ((= j n))
	(let ((e (car (rassoc i edge-list))) (f (car (rassoc j edge-list))))
	  (if
	   (or (member (first e) f) (member (second e) f))
	   (add-edge `(,i ,j) g)))))
    (let ((coloring (dsatur g))
	  (ch-index -1)
	  (res ()))
      (dotimes (i n)
	(push `((mlist simp) ((mlist simp) ,@(car (rassoc i edge-list)))
		,(gethash i coloring)) res)
	(setq ch-index (max ch-index (gethash i coloring))))
      `((mlist simp) ,ch-index ((mlist simp) ,@res)))))

(defun $chromatic_index (g)
  (require-graph 'chromatic_index 1 g)
  ($first ($vertex_coloring ($line_graph g))))

(defun dsatur (g)
  (let* ((x)
	 (vsize (length (vertices g)))
	 (opt-chnumber (1+ vsize)) (back nil)
	 (i 0) (k)
	 (free-color)
	 (xindex)
	 (stop nil)
	 (clique (greedy-clique g))
	 (clique-size (length clique))
	 (start clique-size)
	 (A (make-array vsize))
	 (F (make-hash-table))
	 (f-opt (make-hash-table))
	 (number-of-used-colors (make-hash-table :test #'equal))
	 (dsat (make-hash-table))
	 (uncolored (make-hash-table)))
    
    ;; Prepare data
    ;; (format t "~%Preparing data")
    (dolist (v clique)
      (setf (aref A i) v)
      (setf (gethash v F) (1+ i))
      (setq i (1+ i)))
    (dolist (v (vertices g))
      (setf (gethash v dsat) 0)
      (setf (gethash v uncolored) (length (neighbors v g)))
      (if (not (member v clique))
	  (progn
	    (setf (aref A i) v)
	    (setq i (1+ i))
	    (setf (gethash v F) 0))))
    (dolist (v clique)
      (dolist (u (neighbors v g))
	(decf (gethash u uncolored))
	(setf (gethash `(,u ,(gethash v F)) number-of-used-colors) 1)
	(incf (gethash u dsat))))
    
    ;;(format t "~%Clique size: ~d" (length clique))
    ;;(format t "~%Clique:      ~d" clique)
    (do () (nil)
      ;;(format t "~%Starting with: ~d (~d)" start (gethash start A))
      (do ((i start (1+ i))) ((or (= i vsize) stop))
	
	;; Choose new vertex x
	(if back
	    (progn
	      (setq xindex start)
	      (setq x (aref A xindex)))
	    (let ((mdsat -1) (munc -1))
	      (do ((j i (1+ j))) ((= j vsize))
		(if (or (> (gethash (aref A j) dsat) mdsat)
			(and (= (gethash (aref A j) dsat) mdsat)
			     (> (gethash (aref A j) uncolored) munc)))
		    (progn
		      (setq x (aref  A j))
		      (setq xindex j)
		      (setq mdsat (gethash x dsat))
		      (setq munc (gethash x uncolored)))))))
	;;(format t "~%New vertex: ~d" x)
	
	;; Choose free color
	(setq free-color 0)
	(if back
	    (progn
	      (setq back nil)
	      (setq k (1+ (gethash x F))))
	    (setq k 1))
	(do ((j k (1+ j))) ((or (>= j opt-chnumber) (> free-color 0)))
	  (if (= 0 (gethash `(,x ,j) number-of-used-colors 0))
	      (setq free-color j)))
	;;(format t "~%New color: ~d" free-color)

	(if (> free-color 0)
	    ;; Color vertex x
	    (progn
	      (setf (gethash x F) free-color)
	      ;; Update dsat index
	      (dolist (u (neighbors x g))
		(incf (gethash `(,u ,free-color) number-of-used-colors 0))
		(if (= 1 (gethash `(,u ,free-color) number-of-used-colors 0))
		    (incf (gethash u dsat)))
		(decf (gethash u uncolored)))
	      ;; Update A
	      (rotatef (aref A i) (aref A xindex)))
	    ;; Unable to extend coloring - backtrack
	    (progn
	      (setq start (1- i))
	      (setq stop t)
	      (setq back t))))
      
      (setq stop nil)
      (if back
	  ;; We have a backtrack step
	  (progn
	    (if (< start clique-size)
		(return-from dsatur f-opt))
	    (setq x (aref A start))
	    (setq k (gethash x F))
	    ;; Delete the color of x
	    (dolist (v (neighbors x g))
	      (decf (gethash `(,v ,k) number-of-used-colors))
	      (incf (gethash v uncolored))
	      (if (= 0 (gethash `(,v ,k) number-of-used-colors))
		  (decf (gethash v dsat)))))
	  ;; We have a coloring!
	  (progn
	    ;; Save new optimal coloring
	    (setq opt-chnumber 0)
	    (dolist (v (vertices g))
	      (setf (gethash v f-opt) (gethash v F))
	      (setq opt-chnumber (max opt-chnumber (gethash v F))))
	    ;;(format t "~%Found new coloring: ~d" opt-chnumber)
	    ;; Find new start
	    (setq start -1)
	    (do ((i 0 (1+ i))) ((> start -1))
	      (if (= opt-chnumber (gethash (aref A i) f-opt))
		  (progn
		    (setq start (1- i)))))
	    (setq back t)
	    (if (or (< start clique-size)
		    (= opt-chnumber clique-size))
		(return-from dsatur f-opt))
	    ;;(format t "~%Barktracking from: ~d (~d)" start (aref A start))
	    ;; Delete colors from start
	    (do ((i start (1+ i))) ((= i vsize))
	      (dolist (v (neighbors  (aref A i) g))
		(decf (gethash `(,v ,(gethash (aref A i) F))
			       number-of-used-colors))
		(incf (gethash v uncolored))
		(if (= 0 (gethash `(,v ,(gethash (aref A i) F))
				  number-of-used-colors))
		    (decf (gethash v dsat)))))
	    )))))

(defun greedy-clique (g)
  (let ((clique ()))
    (dolist (v (vertices g))
      (let ((tmp-clique (extend-greedy-clique `(,v) (neighbors v g) g)))
	(if (> (length tmp-clique) (length clique))
	    (setq clique tmp-clique))))
    clique))

(defun extend-greedy-clique (clique neigh g)
  (if (= 0 (length neigh))
      (return-from extend-greedy-clique clique))
  (if (= 1 (length neigh))
      (return-from extend-greedy-clique (cons (car neigh) clique)))
  (let ((u nil) (new-neigh ()))
    (dolist (v neigh)
      (let ((tmp-neigh (copy-tree neigh)))
	(dolist (w neigh)
	  (if (and (not (member w (neighbors v g))))
	      (setq tmp-neigh (remove w tmp-neigh))))
	(if (>= (length tmp-neigh) (length new-neigh))
	    (progn
	      (setq u v)
	      (setq new-neigh tmp-neigh)))))
    (if u
	(extend-greedy-clique (cons u clique) new-neigh g)
	clique)))

;;;;;;;
;;
;; Expose lisp hashtable
;;
;;;;;;

(defun $hash_table ()
  (make-hash-table :test #'equal))

(defun $get_hash (elt ht &optional default)
  (unless (hash-table-p ht)
    ($error "Second argument to `get_hash' is not a hash table!"))
  (gethash elt ht default))

(defun $set_hash (elt ht value)
  (unless (hash-table-p ht)
    ($error "Second argument to `set_hash' is not a hash table!"))
  (setf (gethash elt ht) value)
  value)

(defun $hash_table_data (ht)
  (unless (hash-table-p ht)
    ($error "First argument to `hash_table_info' is not a hash table!"))
  (let (res)
    (maphash
     (lambda (key val)
       (setq res (cons `((marrow simp) ,key ,val) res)))
     ht)
    (cons '(mlist simp) res)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This function is needed for draw_graph (get the temporary file to write in)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $temp_filename (file)
  (unless ($stringp file)
    ($error "Argument to `temp_filename' is not a string"))
  (plot-temp-file ($sconcat file)))
