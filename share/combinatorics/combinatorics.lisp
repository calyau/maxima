;;;; COMBINATORICS package for Maxima
;;;;
;;;; Maxima functions to work with permutations.
;;;;  $cyclep               : predicate function for cycles
;;;;  $permutationp         : predicate function for permutations
;;;;  $permult              : permutations product
;;;;  $invert_permutation   : inverse of a permutation
;;;;  $apply_permutation    : permutes a list according to a permutation
;;;;  $apply_cycles         : permutes a list according to a list of cycles
;;;;  $permutation_undecomp : converts a list of cycles into a permutation
;;;;  $permutation_cycles   : decomposes a permutation into cycles
;;;;  $permutation_lex_rank : permutation's position in lexicographic sequence
;;;;  $permutation_index    : minimum number of adjacent transpositions
;;;;  $permutation_decomp   : minimum set of adjacent transpositions to get p
;;;;  $permutation_parity   : parity of a permutation (0 or 1)
;;;;  $permutations_lex     : lexicographic list of n-degree permutations
;;;;                          or one or more permutations within that sequence
;;;;  $permutation_lex_next : finds next permutation in lexicographic ordering
;;;;
;;;; NOTATION
;;;;  pm    : a permutation as a maxima list
;;;;  pa    : a permutation as a lisp array
;;;;  lpm   : a lisp list of pm's (in reverse order)
;;;;  lmpm  : a maxima list of pm's
;;;;  cm    : a cycle as a maxima list
;;;;  ca    : a cycle as a lisp array
;;;;  lcm   : a lisp list of cycles as maxima lists
;;;;  lmcm  : a maxima list of cycles as maxima lists
;;;;
;;;; USEFULL COMMANDS
;;;;  Turning a pa into a pm:
;;;;       (concatenate 'list `((mlist simp)) pa)
;;;;
;;;;  Turning a pa into a pm and pushing it to the top of a lpm
;;;;       (push-array-mlist pa lpm)    (macro defined in this package)
;;;;
;;;;  Turning a lpm into a maxima list
;;;;       `((mlist simp) ,@(nreverse lpm))
;;;;
;;;; Copyright (C) 2017 Jaime Villate
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;;; MA  02110-1301, USA

(in-package :maxima)
(macsyma-module combinatorics)

(defmacro push-array-mlist (pa lpm)
  `(push (concatenate (quote list) (quote ((mlist simp))) ,pa) ,lpm))

;;; $cyclep returns true if its argument is a maxima list of lenght n or
;;; lower, whose elements are integers between 1 and n, without repetitions.
(defun $cyclep (cm n)
  ;; a cycle must be a maxima list
  (or (and (integerp n) (> n 1)) (return-from $cyclep nil))
  (or ($listp cm) (return-from $cyclep nil))
  (dolist (i (rest cm))
    ;; cycle elements must be positive integers, less or equal to n
    (or (and (integerp i) (plusp i) (<= i n)) (return-from $cyclep nil))
    ;; and cannot be repeated
    (or (< (count i (rest cm)) 2) (return-from $cyclep nil)))
    t)

;;; $permutationp returns true if its argument is a maxima list of lenght n,
;;; whose elements are the integers 1, 2, ...n, without repetitions.
(defun $permutationp (pm)
  (or ($listp pm) (return-from $permutationp nil))
  (let ((n ($length pm)))
    ;; a permutation of degree n is also a valid cycle of degree n
    ($cyclep pm n)))

(defun check-permutation (pm caller)
  (let ((s "~M: argument should be a permutation; found: ~M"))
    (or ($permutationp pm) (merror (intl:gettext s) caller pm))))

(defun check-cycle (cm n caller)
  (let ((s "~M: argument should be a cycle of degree ~M; found: ~M"))
    (or ($cyclep cm n) (merror (intl:gettext s) caller n cm))))

(defun check-list-or-set (lsm caller)
  (let ((s "~M: argument should be a list or set; found: ~M"))
    (or ($listp lsm) ($setp lsm) (merror (intl:gettext s) caller lsm))))

(defun check-list (lm caller)
  (let ((s "~M: argument should be a list; found: ~M"))
  (or ($listp lm) (merror (intl:gettext s) caller lm))))

(defun check-list-length (lm n caller)
  (let ((s "~M: argument should be a list of length ~M; found: ~M"))
  (or (eql ($length lm) n) (merror (intl:gettext s) caller n lm))))

(defun check-pos-integer (n caller)
  (let ((s "~M: argument must be a positive integer; found: ~M"))
  (or (and (integerp n) (> n 0)) (merror (intl:gettext s) caller n))))

(defun check-integer-n1-n2 (r n1 n2 caller)
  (let ((s "~M: argument must be an integer between ~M and ~M; found: ~M"))
    (or (and (integerp r) (>= r n1) (<= r n2))
        (merror (intl:gettext s) caller n1 n2 r))))

;;; $permult multiplies permutation pm by the permutations in list lmpm
(defun $permult (pm &rest lmpm)
  (check-permutation pm "permult") 
  (let ((n ($length pm)) (rp pm))
    ;; multiplies the current product, rp, by each of the permutations in lmpm
    (dolist (qm lmpm)
      (check-list-length qm n "permult")
      (check-permutation qm "permult") 
      ;; the new product, tp, will be the current product, rp, times
      ;; permutation pm: tp = [pm[rp[1]], pm[rp[2]], ..., pm[rp[n]]]
      (let ((tp nil))
        (dolist (j (reverse (rest rp)))
          (setq tp (cons (nth (1- j) (rest qm)) tp)))
        ;; makes tp the new current product rp
        (setq rp (cons '(mlist simp) tp))))
    rp))

;;; $invert_permutation computes the inverse of permutation p
(defun $invert_permutation (pm)
      (check-permutation pm "invert_permutation") 
  (let ((inverse (copy-list pm)))
    ;; if the element pm[j] of the permutation is equal to i, then
    ;; the element inverse[i] of the inverse should be equal to j.
    (dotimes (j ($length pm))
      (setf (nth (nth (1+ j) pm) inverse) (1+ j)))
    inverse))

;;; $apply_permutation permutes the elements of list (or set) lsm according
;;; to the permutation pm
(defun $apply_permutation (pm lsm)
  (check-permutation pm "apply_permutation") 
  (check-list-or-set lsm "apply_permutation")
  (check-list-length lsm ($length pm) "apply_permutation")
  (let ((result nil))
    (dolist (j (reverse (rest pm)))
      (setq result (cons (nth j lsm) result)))
    (cons (car lsm) result)))

;;; $apply_cycles permutes the elements of list (or set) lsm according to the
;;; list of cycles lmcm, applying the cycles on the end of the list first
(defun $apply_cycles (lmcm lsm)
  (check-list lmcm "apply_cycles")
  (check-list-or-set lsm "apply_cycles")
  (let ((n ($length lsm)) (result (copy-list lsm)) prod i)
    ;; iterate the cycles, the last in the list first
    (dolist (cm (reverse (rest lmcm)))
      (check-cycle cm n "apply_cycles")
      ;; lsm[i] will be moved to lsm[j], starting with i equal to the last
      ;; element of the cycle and j equal the first cycle element
      (setq i (first (last cm)) prod (copy-list result))
      (dolist (j (rest cm))
        (setf (nth i prod) (nth j result) i j))
      (setq result (copy-list prod)))
    result))

;;; $permutation_undecomp converts a list of cycles into a permutation,
;;; equal to their product, by applying $apply_cycles to [1, 2,..., n]
(defun $permutation_undecomp (lmcm n)
  (check-list lmcm "permutation_undecomp")
  (check-pos-integer n "permutation_undecomp")
  ($apply_cycles
   lmcm `((mlist simp) ,@(loop for i from 1 to n collecting i))))

;;; $permutation_cycles decomposes permutation p into a product of canonical
;;; cycles: with lower indices first.
(defun $permutation_cycles (pm)
  (check-permutation pm "permutation_cycles")
  (let* (i j k cm (n ($length pm)) (result '((mlist simp)))
         (v (make-array n :element-type 'bit :initial-element 1)))
    ;; the i'th bit in bit array v equal to 1 means index i+1 has not been
    ;; added to any cycles. The next cycle will then start where the first
    ;; value of 1 is found in v
    (while (position 1 v)
      (setf i (position 1 v) (bit v i) 0)
      ;; i= v index where a 1 was found. j=i+1 first index in the current
      ;; cycle. k=next index in the current cycle
      (setq j (1+ i) k j cm `((mlist simp) ,j))
      ;; if p[k] is different from k, there's a non-trivial cycle
      (while (/= (nth k pm) j)
        (setf k (nth k pm) cm (append cm (list k)) (bit v (1- k)) 0))
      ;; a trivial cycle with just one index (length 2) will not be saved
      (and (> (length cm) 2) (setq result (append result (list cm)))))
    result))

;;; $permutation_lex_rank finds the position of the given permutation in
;;; the lexicographic ordering of permutations (from 1 to n!).
;;; Algorithm 2.15: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun $permutation_lex_rank (pm)
  (check-permutation pm "permutation_lex_rank") 
  (let ((r 0) (n ($length pm)) (qm (copy-list pm)))
    (do ((j 1 (1+ j)))
        ((> j n) (1+ r))
      (incf r (* (1- (nth j qm)) (factorial (- n j))))
      (do ((i (1+ j) (1+ i)))
          ((> i n))
        (when (> (nth i qm) (nth j qm))
          (setf (nth i qm) (1- (nth i qm))))))))

;;; $permutation_index finds the minimum number of adjacent transpositions
;;; necessary to write permutation p as a product of adjacent transpositions.
(defun $permutation_index (pm)
  (check-permutation pm "permutation_index") 
  (let ((d 0) (n ($length pm)) (qm (copy-list pm)))
    (loop for j from 1 to n do
         (incf d (* (1- (nth j qm))))
      (loop for i from (1+ j) to n do
        (when (> (nth i qm) (nth j qm))
          (setf (nth i qm) (1- (nth i qm))))))
    d))

;;; $permutation_decomp finds the minimum set of adjacent transpositions
;;; whose product equals permutation pm.
(defun $permutation_decomp (pm)
  (check-permutation pm "permutation_decomp") 
  (let (lcm (n ($length pm)) (qm (copy-list pm)))
    (loop for j from 1 to (1- n) do
         (when (>= (1- (nth j qm)) 1)
           (loop for i from (+ (- (nth j qm) 2) j) downto j do
                (setq lcm (cons `((mlist simp) ,i ,(1+ i)) lcm))))
         (loop for k from (1+ j) to n do
              (when (> (nth k qm) (nth j qm))
                (setf (nth k qm) (1- (nth k qm))))))
    (cons '(mlist simp) lcm)))

;;; $permutation_parity finds the parity of permutation p (0 or 1).
;;; Algorithm 2.19 from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun $permutation_parity (pm)
  (check-permutation pm "permutation_parity") 
  (let* (i (c 0) (n ($length pm)) (q (rest pm)) 
         (a (make-array n :element-type 'bit :initial-element 0)))
    (dotimes (j n)
      (when (= (bit a j) 0)
        (incf c)
        (setf (bit a j) 1 i j)
        (while (/= (1- (nth i q)) j)
          (setf i (1- (nth i q)) (bit a i) 1))))
    (mod (- n c) 2)))

;;; $permutations_lex returns a list of the permutations of order n in
;;; lexicographic order
(defun $permutations_lex (n &optional r0 rf)
  (check-pos-integer n "permutations_lex")
  (when r0 (check-integer-n1-n2 r0 1 (factorial n) "permutations_lex"))
  (when rf (check-integer-n1-n2 rf r0 (factorial n) "permutations_lex"))
  (let ((pa (make-array n :element-type 'fixnum)) lpm)
    (if r0
        (progn
          (unless rf (setq rf r0))
          (permutation-lex-unrank n r0 pa)
          (push-array-mlist pa lpm)
          (when rf
            (dotimes (j (- rf r0))
              (permutation-lex-next n pa)
              (push-array-mlist pa lpm))))
        (progn
          (dotimes (i n)
            (setf (aref pa i) (1+ i)))
          (push-array-mlist pa lpm)
          (while (permutation-lex-next n pa)
            (push-array-mlist pa lpm))))
    (if (> (length lpm) 1)
        `((mlist simp) ,@(nreverse lpm))
        (first lpm))))

;;; $permutation_lex_next finds the next permutation in lexicographic order
(defun $permutation_lex_next (pm)
  (check-permutation pm "permutation_lex_next") 
  (let* ((n ($length pm)) (pa (make-array n :element-type 'fixnum)))
    (dotimes (i n)
      (setf (aref pa i) (nth (1+ i) pm)))
    (unless (permutation-lex-next n pa)
      (return-from $permutation_lex_next nil))
    (concatenate 'list '((mlist simp)) pa)))

;;; permutation_lex_next finds next permutation in the lexicographic order.
;;; Based on Algorithm 2.14 from Kreher & Stinson (1999). Combinatorial
;;; Algorithms.
(defun permutation-lex-next (n pa)
  (declare (type (simple-array fixnum *) pa))
  (declare (type fixnum n))
  (let ((i (1- n)) (j n))
    (while (and (> i 0) (< (aref pa i) (aref pa (1- i))))
      (decf i))
    (when (= i 0) (return-from permutation-lex-next nil))
    (while (< (aref pa (1- j)) (aref pa (1- i)))
      (decf j))
    (array-cycle pa i j)
    (dotimes (k (floor (/ (- n i) 2)))
      (array-cycle pa (+ k i 1) (- n k)))
    t))

;;; $permutation_lex_unrank returns permutation of order n in the r position
;;; (from 1 to n!) in the lexicographic ordering of permutations.
;;; Algorithm 2.16: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun permutation-lex-unrank (n r pa)
  (declare (type (simple-array fixnum *) pa))
  (declare (type fixnum n))
  (declare (type fixnum r))
  (let (d)
    (decf r)
    (setf (aref pa (1- n)) 1)
    (dotimes (j (1- n))
      (setq d (/ (mod r (factorial (+ j 2))) (factorial (1+ j))))
      (decf r (* d (factorial (1+ j))))
      (setf (aref pa (- n j 2)) (1+ d))
      (loop for i from (- n j 1) to (1- n) do
        (when (> (aref pa i) d)
          (setf (aref pa i) (1+ (aref pa i))))))
    t))

;;; array-cycle applies the adjacent transposition [i,i+1] to the
;;; permutation array pa. If more than two arguments are given, the first
;;; one is a permutation array and the following indices are a cycle to be
;;; applied to the permutation.
(defun array-cycle (pa i &rest c)
  (if (null c)
      (let ((tmp (aref pa (1- i))))
        (setf (aref pa (1- i)) (aref pa i) (aref pa i) tmp))
      (progn
        (push i c)
        (let ((n (length c)) (tmp (aref pa (1- (first c)))))
          (dotimes (j (1- n))
            (setf (aref pa (1- (nth j c))) (aref pa (1- (nth (1+ j) c)))))
          (setf (aref pa (1- (nth (1- n) c))) tmp)))))

;;; $permutations_list returns a list of the n-degree permutations where each
;;; permutation differs from the previous one by an adjacent transposition 
(defun $permutations_list (n)
  (check-pos-integer n "permutations_path")
  (when (= n 1) (return-from $permutations_list #$[[1]]$))
  (when (= n 2) (return-from $permutations_list #$[[1,2],[2,1]]$))
  (let (f l s lpm (pa (make-array n :element-type 'fixnum)))
    (setf (aref pa 0) 2 (aref pa 1) 1)
    (dotimes (i (- n 2))
      (setf (aref pa (+ i 2)) (+ i 3)))
    (dotimes (j (factorial (1- n)))
      (if (= (mod j 2) 1)
          (setq f 1 l (1- n) s 1)
          (setq f (1- n) l 1 s -1))
      (array-cycle pa l)
      (push-array-mlist pa lpm)
      (do ((k f (+ k s))) ((= k (+ l s)))
        (array-cycle pa k)
        (push-array-mlist pa lpm)))
    `((mlist simp) ,@(nreverse lpm))))
