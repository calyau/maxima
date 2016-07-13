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

; Kill off translation properties of conditionals.
; Ideally we could avoid calling MEVAL when arguments are declared Boolean.
; We're not there yet.

(remprop 'mcond 'translate)

; It's OK for MEVALATOMS to evaluate the arguments of MCOND.
; %MCOND already has this property.
(defprop mcond t evok)

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
        (setq consequences (mapcar 'mevalatoms consequences))
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
