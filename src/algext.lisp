(in-package :maxima)

;;; Functions for reasoning about algebraic field extensions
;;;
;;;
;;; Some of the rational function / polynomial code (such as $resultant) can run
;;; in to problems when $algebraic is true if you pass it the result of rat() on
;;; something like
;;;
;;;    (x + 1)^(1/3) + x^2
;;;
;;; rat() chooses two kernels: x and (x + 1)^(1/3). However, if you then start
;;; doing algebraic operations, you can end up (say) cubing the cube root, to
;;; end up with something in x: the other variable! This plays havoc with code
;;; like the resultant algorithm.
;;;
;;; The problem boils down to the fact that x and (x + 1)^(1/3) are
;;; algebraically dependent over Q (the rationals). Determining whether two
;;; expressions are dependent over Q is hard (no-one actually knows whether %e
;;; and %pi are...). Instead, we solve a much simpler problem by
;;; over-approximating the algebraic dependence relation. (Except that we assume
;;; symbolic constants are all independent, including %e and %pi)


;; FIND-ALGEBRAIC-DEPENDENCE
;;
;; If U is either a number or is known to be algebraically independent from VS
;; over the rational numbers, return NIL. Otherwise, return a pair (U' V), where
;; U' is a sub-expression of U, V is an element of VS and there's a possible
;; algebraic dependence between U' and V.
;;
;; This function is quite simple: it checks that every non-numeric sub-term of
;; the expressions is linearly independent over Q from all the others. Unknown
;; functions, variables and constants (like %e and %pi) are all considered
;; distinct.
;;
;; So this function can be implemented efficiently, the elements of VS
;; must be "simple" in the sense that they are all atomic with respect to this
;; recursion (guaranteed if this is called through ALGEBRAICALLY-INDEPENDENT-P)
(defun find-algebraic-dependence (u vs)
  (cond
    ;; Numbers are ignored
    ((or (numberp u) (ratnump u)) nil)
    ;; Non-numeric atoms are symbols. Since elements of vs are simple, u is
    ;; independent unless it appears somewhere in the list.
    ((atom u)
     (when (member u vs :test #'eq)
       (list u u)))
    ;; Recurse through MPLUS and MTIMES
    ((memq (caar u) '(MPLUS MTIMES))
     (map-find (lambda (uu) (find-algebraic-dependence uu vs))
               (cdr u)))
    ;; For MEXPT, just check the base (you can't do something like a log() to
    ;; get the exponent back down, so we can ignore it). We don't try to be
    ;; clever and notice that, say, 2^%pi is independent of 2^%e.
    ((eq (caar u) 'MEXPT)
     (find-algebraic-dependence (cadr u) vs))
    ;; Anything else is an unknown or known-to-be-transcendental function, so
    ;; considered independent unless it appears in vs. (This test uses EQUAL
    ;; because we have cons trees)
    (t (when (member u vs :test #'equal)
         (list u u)))))

;; ALGEBRAICALLY-INDEPENDENT-KERNELS
;;
;; Return a list of subterms of U that are known to be algebraically independent
;; of one another and are simple in the sense described in the comment above
;; FIND-ALGEBRAIC-DEPENDENCE.
(defun algebraically-independent-kernels (u)
  (cond
    ((or (numberp u) (ratnump u)) nil)
    ((atom u) (list u))
    ((memq (caar u) '(MPLUS MTIMES))
     (reduce (lambda (s1 s2) (union s1 s2 :test #'equal))
             (mapcar #'algebraically-independent-kernels (cdr u))))
    ((eq (caar u) 'MEXPT) (algebraically-independent-kernels (cadr u)))
    (t (list u))))

;; AUGMENT-SIMPLE-TERMS
;;
;; Add all simple sub-expressions in U to VS, a list of simple terms in the
;; sense described in the comment above FIND-ALGEBRAIC-DEPENDENCE.
(defun augment-simple-terms (vs u)
  (union vs (algebraically-independent-kernels u) :test #'equal))

;; ALGEBRAICALLY-INDEPENDENT-P
;;
;; Returns T if the elements of US are known to be algebraically independent
;; over Q. See FIND-ALGEBRAIC-DEPENDENCE.
(defun algebraically-independent-p (us)
  (let ((simple-terms))
    (dolist (u us t)
      (when (find-algebraic-dependence u simple-terms)
        (return nil))
      (setf simple-terms (augment-simple-terms simple-terms u)))))

;; BLANK-ALGEBRAICALLY-DEPENDENT
;;
;; Iterate through US, replacing any terms that might be algebraically dependent
;; on the set of previous terms with a gensym. Return the amended list and the
;; dictionary of amendments.
(defun blank-algebraically-dependent (us)
  (let ((simple-terms)
        (dict))
    (values (mapcar (lambda (u)
                      (cond
                        ((find-algebraic-dependence u simple-terms)
                         (push (cons (gensym) u) dict)
                         (caar dict))
                        (t
                         (setf simple-terms
                               (augment-simple-terms simple-terms u))
                         u)))
                    us)
            dict)))

;; FORCE-ALGEBRAIC-INDEPENDENCE
;;
;; If $ALGEBRAIC is false, just evaluate FUNC. If $ALGEBRAIC is true, evaluate
;; FUNC after rebinding VARLIST to ensure that no variables appear spuriously
;; algebraically dependent.
;;
;; When FUNC finishes, patch up VARLIST to make sure that any new variables that
;; have appeared are reflected in the global VARLIST in the same way.
(defun run-forcing-algebraic-independence (func)
  (declare (special varlist $algebraic))
  (if (not $algebraic)
      (funcall func)
      (let ((dict))
        (multiple-value-setq (varlist dict)
          (blank-algebraically-dependent varlist))
        (prog1 (funcall func)
          (setf varlist
                (mapcar (lambda (v) (or (cdr (assoc v dict)) v))
                        varlist))))))

(defmacro force-algebraic-independence (&body body)
  `(run-forcing-algebraic-independence (lambda () ,@body)))
