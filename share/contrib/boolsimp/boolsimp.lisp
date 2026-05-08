; Simplification and evaluation of Boolean and conditional expressions.
; 
; Copyright 2006 by Robert Dodier.
; Released under the terms of the GNU General Public License.

; Ideas that haven't gone anywhere yet:
;  - given "if foo then bar else baz", simplify bar assuming foo,
;    and simplify baz assuming not foo
;  - flatten conditionals -- nested if --> if -- elseif -- elseif -- elseif -- else
;  - arithmetic on conditionals -- distribute arithmetic ops over if
;  - make up rules via tellsimp & friends for integrate / sum / diff applied to conditionals
;  - knock out redundant clauses in simplification (i.e. if x implies y then knock out y)

(in-package :maxima)

;; translation of mcond into lisp.
;; Builds a lisp "block" that executes lisp translation of each mcond
;; clause one after the other.  Returns from block if pred is true.
;; If pred is false does nothing.  If pred is unknown, appends untranslated
;; pred and body to a list, which is returned as an mcond at end of block.
(def%tr mcond (form)
  (let ((g (tr-gensym))
        (nl nil)
        (mode nil))
    ;; build up list nl from l
    ;; nl is in reverse order from l
    ;; each two items in nl are a body followed by a corresponding condition
    (do ((l (cdr form) (cddr l))) ((null l))
      ; Optimize the else-if case: if we're at the else case at the end
      ; and the body is just another conditional, then we just continue
      ; directly with the clauses of the inner conditional instead of
      ; nesting.
      (when (and (null (cddr l))
                 (eq (car l) t)
                 (consp (cadr l))
                 (eq (caaadr l) 'mcond))
        (setq l (cdadr l)))
      (let ((wrap-a-pred 'mcond))
        (declare (special wrap-a-pred))
        (destructuring-let (((pred-mode . pred-tr) (translate-predicate (car l)))
                            ((body-mode . body-tr) (translate (cadr l))))
          (setq mode (*union-mode mode body-mode))
	  ;; below, a clause in the original mcond is translated into
	  ;; two (body, condition) pairs in nl: one to execute if the
	  ;; original condition is true, and one to add a clause
	  ;; to the unevaluated mcond if the condition is unknown
          (if (eq pred-mode '$boolean)
	      ;; pred-tr is either T or NIL
              (setq nl (list* `(setq unevaluated-mcond (append unevaluated-mcond (list ,pred-tr (mcond-eval-symbols-tr ',(cadr l)))))
                              pred-tr
                              `(return-from translated-mcond ,body-tr)
                              `(and ,pred-tr
				    (null unevaluated-mcond))
                              nl))
	      ;; use variable g for result of pred evaluation
              (setq nl (list* `(setq unevaluated-mcond (append unevaluated-mcond (list ,g (mcond-eval-symbols-tr ',(cadr l)))))
			      g
                              `(return-from translated-mcond ,body-tr)
                              `(and (eq t (setq ,g ,pred-tr))
				    (null unevaluated-mcond))
                              nl))))))
    ; We leave off the final clause if the condition is true
    ; and the consequent is false.
    (when (and (eq t (cadr nl)) (null (car nl)))
      (setq nl (cddr nl)))
    ;; build up a block in the translated routine
    ;; each (body, condition) pair in nl is translated into an "if"
    (setq form nil)
    (do ((l nl (cddr l))) ((null l))
      (setq form
            (cons (cons 'if
			(cons (cadr l)
                              (cond ((and (not (atom (car l)))
					  (cdr l)
					  (eq (caar l) 'progn))
				     (cdar l))
				    ((and (equal (car l) (cadr l))
					  (atom (car l)))
				     nil)
				    (t (list (car l))))))
                  form)))
    (if (among g form)
        (cons '$any `(let (,g unevaluated-mcond)
		       (block translated-mcond ,@form (cons '(mcond) unevaluated-mcond))))
        (cons mode `(let (unevaluated-mcond)
		      (block translated-mcond ,@form (cons '(mcond) unevaluated-mcond)))))))

; X is an expression of the form ((OP) B1 G1 B2 G2 B3 G3 ...)
; where OP is MCOND or %MCOND,
; and B1, B2, B3, ... are boolean expressions,
; and G1, G2, G3, ... are general expressions.

; Evaluation of conditional expressions.

; If any B evaluates to T,
; then append B and G to the list of evaluated arguments, and ignore any remaining arguments.
; If any B evaluates to NIL,
; then ignore B and G, and continue processing arguments.
; Otherwise, append B and G to the list of evaluated arguments, and continue processing arguments.

; If the first element of the list of evaluated arguments is T,
; return the second element.
; Otherwise, construct a new conditional expression from the evaluated arguments.

(defmspec mcond (x)
  (let ((op (car x)) (args (cdr x)) (conditions) (consequences))
    (loop while (> (length args) 0) do
      (let* ((b (car args)) (g (cadr args)) (v (mevalp b)))
        (cond 
          ((eq v t)
           (setq conditions (append conditions (list v)))
           (setq consequences (append consequences (list g)))
           (setq args nil))
          ((eq v nil)
           nil)
          (t
            (setq conditions (append conditions (list v)))
            (setq consequences (append consequences (list g)))))
        (setq args (cddr args))))

    (cond
      ((eq (car conditions) t)
       (meval (car consequences)))
      (t
        (setq consequences (mapcar #'(lambda (e) (mcond-eval-symbols #'meval1 e)) consequences))
        ; Burn off SIMP flag, if any, when constructing the new CAAR
        (cons `(,(car op))
              (apply 'append (mapcar #'(lambda (x y) `(,x ,y)) conditions consequences)))))))

; Simplification of conditional expressions.

; If any B simplifies to T or $TRUE,
; then append B and G to the list of simplified arguments, and ignore any remaining arguments.
; If any B simplifies to NIL or $FALSE, 
; then ignore B and G, and continue processing arguments.
; Otherwise, append B and G to the list of simplified arguments, and continue processing arguments.

; If there are no arguments remaining (i.e., expression is just ((MCOND)) or ((%MCOND))) return $FALSE.
; If the first element of the list of simplified arguments is T or $TRUE, return the second element.
; Otherwise, construct a new conditional expression from the simplified arguments.

(defun simp-mcond-shorten (x z)
  (let ((op (car x)) (args (cdr x)) (args-simplified))
    (loop while (> (length args) 0) do
      (let ((b (maybe-simplifya (car args) z)) (g (cadr args)))
        (cond
          ((or (eq b nil) (eq b '$false)) nil)
          ((or (eq b t) (eq b '$true))
           (setq args-simplified (append args-simplified (list b (maybe-simplifya g z)))
                 args nil))
          (t
            (setq args-simplified (append args-simplified (list b (maybe-simplifya g z)))))))
      (setq args (cddr args)))
    
    (cond
      ((null args-simplified) nil)
      ((or (eq (car args-simplified) t) (eq (car args-simplified) t))
       (cadr args-simplified))
      (t
        ; Indicate that the return value has been simplified.
        (cons `(,(car op) simp) args-simplified)))))

(defun simp-mcond (x y z)
  (declare (ignore y))
  (simp-mcond-shorten x z))

(putprop 'mcond 'simp-mcond 'operators)
(putprop '%mcond 'simp-mcond 'operators)

; REDEFINE PARSE-CONDITION (IN SRC/NPARSE.LISP) TO APPEND NIL INSTEAD OF $FALSE
; WHEN INPUT IS LIKE "IF FOO THEN BAR" (WITHOUT ELSE)

(defun parse-condition (op)
  (list* (parse (rpos op) (rbp op))
	 (if (eq (first-c) '$then)
	     (parse '$any (rbp (pop-c)))
	     (mread-synerr "Missing `then'"))
	 (case (first-c)
	   (($else)   (list t (parse '$any (rbp (pop-c)))))
	   (($elseif) (parse-condition (pop-c)))
	   (t
	    (list t nil)))))
