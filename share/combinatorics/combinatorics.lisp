;;;; COMBINATORICS package for Maxima
;;;;
;;;; Maxima functions to work with permutations.
;;;;  $cyclep          : predicate function for cycles.
;;;;  $permp           : predicate function for permutations.
;;;;  $permult         : permutations product.
;;;;  $perm_inverse    : inverse of a permutation.
;;;;  $permute         : permutes a list according to a permutation.
;;;;  $apply_cycles    : permutes a list according to a list of cycles.
;;;;  $perm_undecomp   : converts a list of cycles into a permutation.
;;;;  $perm_cycles     : decomposes a permutation into cycles.
;;;;  $perm_lex_rank   : permutation's position in lexicographic order.
;;;;  $perm_lex_unrank : permutation's position in lexicographic order.
;;;;  $perm_length     : minimum number of adjacent transpositions.
;;;;  $perm_decomp     : minimum set of adjacent transpositions to get p.
;;;;  $perm_parity     : parity of a permutation (0 or 1).
;;;;  $perms_lex       : lexicographic list of all n-degree permutations
;;;;                     or permutations within some ranks.
;;;;  $perm_lex_next   : finds next permutation in lexicographic order.
;;;;  $perm_rank       : permutation's position in Trotter-Johnson order.
;;;;  $perm_unrank     : permutation's position in Trotter-Johnson order.
;;;;  $perms           : n-degree permutations in mimimum-change order
;;;;                     (Trotter-Johnson) or permutations within some ranks.
;;;;  $perm_next       : finds next permutation in Trotter-Johnson order.
;;;;  $random_perm     : generates a random permutation of degree n.
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
;;;; USEFUL COMMANDS
;;;;  Turn a pa into a pm:
;;;;       (concatenate 'list `((mlist simp)) pa)
;;;;
;;;;  Turn an lpm into a maxima list
;;;;       `((mlist simp) ,@(nreverse lpm))
;;;;
;;;; MACROS
;;;;  Apply transposition [i,j] to a pa (i and j from 1 to n)
;;;;       (array-transposition pa i j)
;;;;
;;;;  Apply transposition [i,i+1] to a pa (i from 1 to n-1)
;;;;       (array-adjacent-transposition pa i)
;;;;
;;;;  Turn a pa into a pm and push it to the top of an lpm
;;;;       (push-array-mlist pa lpm)
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

(defmacro array-adjacent-transposition (pa i)
  `(rotatef (aref ,pa (1- ,i)) (aref ,pa ,i)))

(defmacro array-transposition (pa i j)
  `(rotatef (aref ,pa (1- ,i)) (aref ,pa (1- ,j))))

;;; $cyclep returns true if its argument is a maxima list of length n or
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

;;; $permp returns true if its argument is a maxima list of length n,
;;; whose elements are the integers 1, 2, ...n, without repetitions.
(defun $permp (pm)
  (or ($listp pm) (return-from $permp nil))
  (let ((n ($length pm)))
    ;; a permutation of degree n is also a valid cycle of degree n
    ($cyclep pm n)))

(defun check-perm (pm caller)
  (let ((s "~M: argument should be a permutation; found: ~M"))
    (or ($permp pm) (merror (intl:gettext s) caller pm))))

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
  (check-perm pm "permult") 
  (let ((n ($length pm)) (rm (copy-list pm)))
    ;; multiplies the current product, rm, by each of the permutations in lmpm
    (dolist (qm lmpm)
      (check-list-length qm n "permult")
      (check-perm qm "permult") 
      ;; the new product, rm, is the current product, rm, times
      ;; permutation qm: rm = [qm[rm[1]], qm[rm[2]], ..., qm[rm[n]]]
      (loop for j from 1 to n do
           (setf (nth j rm) (nth (nth j rm) qm))))
    rm))

;;; $perm_inverse computes the inverse of permutation p
(defun $perm_inverse (pm)
      (check-perm pm "perm_inverse") 
  (let ((inverse (copy-list pm)))
    ;; if the element pm[j] of the permutation is equal to i, then
    ;; the element inverse[i] of the inverse should be equal to j.
    (dotimes (j ($length pm))
      (setf (nth (nth (1+ j) pm) inverse) (1+ j)))
    inverse))

;;; $permute permutes the elements of list (or set) lsm according
;;; to the permutation pm
(defun $permute (pm lsm)
  (check-perm pm "permute") 
;  (check-list-or-set lsm "permute")
  (check-list-length lsm ($length pm) "permute")
  (let ((result nil))
    (dolist (j (reverse (rest pm)))
      (setq result (cons (nth j lsm) result)))
    (cons (car lsm) result)))

;;; $apply_cycles permutes the elements of list (or set) lsm according to the
;;; list of cycles lmcm, applying the cycles on the end of the list first
(defun $apply_cycles (lmcm lsm)
  (check-list lmcm "apply_cycles")
  (check-list-or-set lsm "apply_cycles")
  (let ((n ($length lsm)) (r (copy-list lsm)) m tmp)
    ;; iterate the cycles, the last in the list first
    (dolist (cm (reverse (rest lmcm)))
      (check-cycle cm n "apply_cycles")
      (setq m ($length cm))
      (setq tmp (nth (nth 1 cm) r))
      (loop for i from 1 to (1- m) do
           (setf (nth (nth i cm) r) (nth (nth (1+ i) cm) r)))
      (setf (nth (nth m cm) r) tmp))
    r))

;;; $perm_undecomp converts a list of cycles into a permutation,
;;; equal to their product, by applying $apply_cycles to [1, 2,..., n]
(defun $perm_undecomp (lmcm n)
  (check-list lmcm "perm_undecomp")
  (check-pos-integer n "perm_undecomp")
  ($apply_cycles
   lmcm `((mlist simp) ,@(loop for i from 1 to n collecting i))))

;;; $perm_cycles decomposes permutation p into a product of canonical
;;; cycles: with lower indices first.
(defun $perm_cycles (pm)
  (check-perm pm "perm_cycles")
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

;;; $perm_lex_rank finds the position of the given permutation in
;;; the lexicographic ordering of permutations (from 1 to n!).
;;; Algorithm 2.15: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun $perm_lex_rank (pm)
  (check-perm pm "perm_lex_rank") 
  (let ((r 1) (n ($length pm)) (qm (copy-list pm)))
    (loop for j from 1 to n do
         (incf r (* (1- (nth j qm)) (factorial (- n j))))
         (loop for i from (1+ j) to n do
              (when (> (nth i qm) (nth j qm))
                (setf (nth i qm) (1- (nth i qm))))))
    r))

;;; $perm_length finds the minimum number of adjacent transpositions
;;; necessary to write permutation p as a product of adjacent transpositions.
(defun $perm_length (pm)
  (check-perm pm "perm_length") 
  (let ((d 0) (n ($length pm)) (qm (copy-list pm)))
    (loop for j from 1 to n do
         (incf d (* (1- (nth j qm))))
      (loop for i from (1+ j) to n do
        (when (> (nth i qm) (nth j qm))
          (setf (nth i qm) (1- (nth i qm))))))
    d))

;;; $perm_decomp finds the minimum set of adjacent transpositions
;;; whose product equals permutation pm.
(defun $perm_decomp (pm)
  (check-perm pm "perm_decomp") 
  (let (lcm (n ($length pm)) (qm (copy-list pm)))
    (loop for j from 1 to (1- n) do
         (when (>= (1- (nth j qm)) 1)
           (loop for i from (+ (- (nth j qm) 2) j) downto j do
                (setq lcm (cons `((mlist simp) ,i ,(1+ i)) lcm))))
         (loop for k from (1+ j) to n do
              (when (> (nth k qm) (nth j qm))
                (setf (nth k qm) (1- (nth k qm))))))
    (cons '(mlist simp) lcm)))

;;; $perm_parity finds the parity of permutation p (0 or 1).
(defun $perm_parity (pm)
  (check-perm pm "perm_parity") 
  (perm-parity ($length pm) (rest pm)))

;;; perm-parity finds the parity of n first elements of p (lisp list)
;;; Algorithm 2.19 from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun perm-parity (n p)
  (let (i (c 0) (a (make-array n :element-type 'bit :initial-element 0)))
    (dotimes (j n)
      (when (= (bit a j) 0)
        (incf c)
        (setf (bit a j) 1 i j)
        (while (/= (1- (nth i p)) j)
          (setf i (1- (nth i p)) (bit a i) 1))))
    (mod (- n c) 2)))
          
;;; $perms_lex returns a list of permutations of degree n in lexicographic order
(defun $perms_lex (n &optional r0 rf)
  (check-pos-integer n "perms_lex")
  (when r0 (check-integer-n1-n2 r0 1 (factorial n) "perms_lex"))
  (when rf (check-integer-n1-n2 rf r0 (factorial n) "perms_lex"))
  (let ((pa (make-array n :element-type 'fixnum)) lpm)
    (if r0
        (progn
          (perm-lex-unrank n r0 pa)
          (push-array-mlist pa lpm)
          (when rf
            (dotimes (j (- rf r0))
              (perm-lex-next n pa)
              (push-array-mlist pa lpm))))
        (progn
          (dotimes (i n) (setf (aref pa i) (1+ i)))
          (push-array-mlist pa lpm)
          (while (perm-lex-next n pa)
            (push-array-mlist pa lpm))))
    `((mlist simp) ,@(nreverse lpm))))

;;; $perm_lex_next finds the next permutation in lexicographic order
(defun $perm_lex_next (pm)
  (check-perm pm "perm_lex_next") 
  (let* ((n ($length pm)) (pa (make-array n :element-type 'fixnum)))
    (dotimes (i n)
      (setf (aref pa i) (nth (1+ i) pm)))
    (unless (perm-lex-next n pa)
      (return-from $perm_lex_next nil))
    (concatenate 'list '((mlist simp)) pa)))

;;; $perm_lex_unrank finds the n-degree permutation with lexicographic rank r
(defun $perm_lex_unrank (n r)
  (check-pos-integer n "perm_lex_unrank")
  (check-integer-n1-n2 r 1 (factorial n) "perm_lex_unrank")
  (let ((pa (make-array n :element-type 'fixnum)))
    (perm-lex-unrank n r pa)
    (concatenate 'list `((mlist simp)) pa)))

;;; perm-lex-next finds next permutation in the lexicographic order.
;;; Based on Algorithm 2.14 from Kreher & Stinson (1999). Combinatorial
;;; Algorithms.
(defun perm-lex-next (n pa)
  (declare (type (simple-array fixnum *) pa))
  (declare (type fixnum n))
  (let ((i (1- n)) (j n))
    (while (and (> i 0) (< (aref pa i) (aref pa (1- i))))
      (decf i))
    (when (= i 0) (return-from perm-lex-next nil))
    (while (< (aref pa (1- j)) (aref pa (1- i)))
      (decf j))
    (array-transposition pa i j)
    (dotimes (k (floor (/ (- n i) 2)))
      (array-transposition pa (+ k i 1) (- n k)))
    t))

;;; perm-lex-unrank finds the n-degree permutation of in position
;;; r (from 1 to n!) in the lexicographic ordering of permutations.
;;; Algorithm 2.16: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun perm-lex-unrank (n r pa)
  (declare (type (simple-array fixnum *) pa))
  (declare (type fixnum n))
  (declare (type fixnum r))
  (let (d)
    (setf (aref pa (1- n)) 1)
    (dotimes (j (1- n))
      (setq d (/ (mod (1- r) (factorial (+ j 2))) (factorial (1+ j))))
      (decf r (* d (factorial (1+ j))))
      (setf (aref pa (- n j 2)) (1+ d))
      (loop for i from (- n j 1) to (1- n) do
           (when (> (aref pa i) d)
             (setf (aref pa i) (1+ (aref pa i))))))
    t))

;;; $perms returns a list of the permutations of order n
;;; in Trotter-Johnson ordering, using a simpler algorithm
(defun $perms (n &optional r0 rf)
  (check-pos-integer n "perms")
  (when r0 (check-integer-n1-n2 r0 1 (factorial n) "perms"))
  (when rf (check-integer-n1-n2 rf r0 (factorial n) "perms"))
  (let ((pa (make-array n :element-type 'fixnum)) lpm)
    (if r0
        (progn
          (perm-unrank n r0 pa)
          (push-array-mlist pa lpm)
          (when rf
            (loop for r from r0 to (- rf 1) do
                 (array-adjacent-transposition pa (transposition-next n r))
                 (push-array-mlist pa lpm))))
        (progn
          (dotimes (i n) (setf (aref pa i) (1+ i)))
          (push-array-mlist pa lpm)
          (loop for r from 1 to (1- (factorial n)) do
               (array-adjacent-transposition pa (transposition-next n r))
               (push-array-mlist pa lpm))))
    `((mlist simp) ,@(nreverse lpm))))

;;; $perm_next finds the next permutation in Trotter-Johnson ordering
(defun $perm_next (pm)
  (check-perm pm "perm_next") 
  (let ((n ($length pm)) (r ($perm_rank pm)) (qm (copy-list pm)) i)
    (when (= r (factorial n)) (return-from $perm_next nil))
    (setq i (transposition-next n r))
    (rotatef (nth i qm) (nth (1+ i) qm))
    qm))

;;; $perm_unrank finds the n-degree permutation with Trotter-Johnson rank r
(defun $perm_unrank (n r)
  (check-pos-integer n "perm_unrank")
  (check-integer-n1-n2 r 1 (factorial n) "perm_unrank")
  (let ((pa (make-array n :element-type 'fixnum)))
    (perm-unrank n r pa)
    (concatenate 'list `((mlist simp)) pa)))

;;; $perm_rank finds the position of the given permutation in
;;; the Trotter-Johnson ordering of permutations (from 1 to n!).
;;; Algorithm 2.17: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun $perm_rank (pm)
  (check-perm pm "perm_rank") 
  (let (i k (r 1) (n ($length pm)))
    (loop for j from 2 to n do
         (setq k 1 i 1)
         (while (/= (nth i pm) j)
           (and (< (nth i pm) j) (incf k))
           (incf i))
         (if (= (mod r 2) 1)
             (setq r (- (+ (* j r) 1) k))
             (setq r (+ (* j (1- r)) k))))
    r))

;;; perm-unrank returns the n-degree permutation in position r
;;; (from 1 to n!) in the Trotter-Johnson ordering of permutations.
;;; Algorithm 2.18: from Kreher & Stinson (1999). Combinatorial Algorithms.
(defun perm-unrank (n r pa)
  (declare (type (simple-array fixnum *) pa))
  (declare (type fixnum n))
  (declare (type fixnum r))
  (let (r1 (r2 0) k)
    (setf (aref pa 0) 1)
    (loop for j from 2 to n do
         (setq r1 (floor (/ (* (1- r) (factorial j)) (factorial n))))
         (setq k (- r1 (* j r2)))
         (if (= (mod r2 2) 0)
             (progn
               (loop for i from (1- j) downto (- j k) do
                    (setf (aref pa i) (aref pa (1- i))))
               (setf (aref pa (- j k 1)) j)) 
             (progn
               (loop for i from (1- j) downto (1+ k) do
                    (setf (aref pa i) (aref pa (1- i))))
               (setf (aref pa k) j)))
         (setq r2 r1))))
 
;;; transposition-next finds the first index of the adjacent transposition
;;; to be applied to the n-degree permutation of Trotter-Johnson rank r, in
;;; order to get the permutation of rank r+1 (r must be between 1 and n!-1)
(defun transposition-next (n r)
  (let ((d 0) m k)
    (setf (values m k) (floor r n))
    (while (= k 0)
      (setq  r m)
      (decf n)
      (when (= (mod m 2) 1) (incf d))
      (setf (values m k) (floor r n)))
    (if (= (mod m 2) 0)
        (+ (- n k) d)
        (+ k d))))

;;; $random_perm generates a random permutation of degree n, using algorithm
;;; 5.4 from Reingold, Nievergelt and Deo. Combinatorial algorithms: theory and
;;; practice. 1977
(defun $random_perm (n)
  (check-pos-integer n "random_perm")
  (let ((pm (cons '(mlist) (loop for i from 1 to n collecting i))))
    (loop for i from n downto 2 do
         (rotatef (nth i pm) (nth (1+ ($random i)) pm)))
    pm))
