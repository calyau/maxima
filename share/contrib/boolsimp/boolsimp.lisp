; Simplification and evaluation of Boolean and conditional expressions.
; 
; Copyright 2006 by Robert Dodier.
; Released under the terms of the GNU General Public License, version 2.

; The functions in this file are an attempt to make Boolean (and, or, not)
; and conditional (if -- then -- else/elseif) expressions work more like
; arithmetic expressions in the treatment of predicates which are
; undecidable at the time the expression is evaluated,
; by allowing undecided predicates in simplified and evaluated
; expressions, instead of complaining or returning 'unknown.

; Examples:
;
; assume (a > 1);
;
;                        Before      After
;
; a > 1 and b < 0   =>   error       b < 0
; c > 1 and b < 0   =>   error       c > 1 and b < 0
; not b < 0         =>   error       b >= 0
; if c then d       =>   error       if c then d
; plot2d (if x > 0 then x else -x, [x, -1, 1])
;                   =>   error       nice plot
; quad_qags (if x > 0 then x else -x, x, -1 ,1)
;                   =>   error       [1.0, 1.1107651257113993E-14, 63, 0]


; In and, or, and if, arguments are evaluated, or simplified as the case may be,
; from left to right, only as needed to establish whether the whole expression
; is true or false. Therefore arguments which potentially have side effects
; (print, kill, save, quit, etc) may or may not actually have those side effects.
;
; Simplification of boolean expressions:
;
; and: if any argument simplifies to false, return false
;  otherwise omit arguments which simplify to true and simplify others
;  if only one argument remains, return it
;  if none remain, return true
;
; or: if any argument simplifies to true, return true
;  otherwise omit arguments which simplify to false and simplify others
;  if only one argument remains, return it
;  if none remain, return false
;
; not: if argument simplifies to true / false, return false / true
;  otherwise reverse sense of comparisons (if argument is a comparison)
;  otherwise return not <simplified argument>
;
; Evaluation (MEVAL) of boolean expressions:
; same as simplification except evaluating arguments instead of simplifying
; (WHAT IS EFFECT OF PREDERROR HERE ??)
;
; Evaluation (MEVALP) of boolean expressions:
; same as evaluation (MEVAL)
; (WHAT IS EFFECT OF PREDERROR HERE ??)
;
; Simplification of "is" expressions:
; if argument simplifies to true/false, return true/false
; otherwise return is (<simplified argument>)
;
; Evaluation of "is" expressions:
; if argument evaluates to true/false, return true/false
; otherwise return unknown if prederror = false, else trigger an error
;
; Simplification of "maybe" expressions:
; if argument simplifies to true/false, return true/false
; otherwise return maybe (<simplified expression>)
;
; Evaluation of "maybe" expressions:
; if argument evaluates to true/false, return true/false
; otherwise return unknown
;
; Simplification of "if" expressions:
;
; Let the expression be if P[1] then E[1] elseif P[2] then E[2] ... elseif P[n] then E[n]
; ("if P[1] then E[1] else E[2]" parses to the above with P[2] = true,
; and "if P[1] then E[1]" parses to the above with P[2] = true and E[2] = false.)
;
; (1) If any P[k] simplifies to false, do not simplify E[k], and omit P[k] and E[k] from the result.
; (2) If any P[k] simplifies to true, simplify E[k], but do not simplify any P[k + 1], E[k + 1], ...,
;     and omit them from the result.
; (3) Otherwise, simplify E[k].
;
; Let P*[1], E*[1], ... be any P and E remaining after applying (1), (2), and (3).
; If there are no P and E remaining, return false.
; If P*[1] = true, return E*[1].
; Otherwise return "if P*[1] then E*[1] elseif P*[2] then E*[2] ..."
; with "if" being a noun iff the original "if" was a noun.
;
; Evaluation of "if" expressions:
;
; (1) If any P[k] evaluates to false, do not evaluate E[k], and omit P[k] and E[k] from the result.
; (2) If any P[k] evaluates to true, evaluate E[k], but do not evaluate any P[k + 1], E[k + 1], ...,
;     and omit them from the result.
; (3) Otherwise, evaluate atoms (not function calls) in E[k].
;
; Let P*[1], E*[1], ... be any P and E remaining after applying (1), (2), and (3).
; If there are no P and E remaining, return false.
; If P*[1] = true, return E*[1].
; Otherwise return "if P*[1] then E*[1] elseif P*[2] then E*[2] ..."
; with "if" being a noun iff the original "if" was a noun.
;
(in-package :maxima)

; It's OK for MEVALATOMS to evaluate the arguments of MCOND.
; %MCOND already has this property.
(defprop mcond t evok)

; Change default value of $PREDERROR to NIL.
; Binding $PREDERROR to T banishes unevaluated conditionals.
(setq $prederror nil)

(remprop '$is 'mfexpr*)
(remprop '$maybe 'mfexpr*)

(defprop $is simp-$is operators)
(defprop %is simp-$is operators)
(defprop $maybe simp-$is operators)
(defprop %maybe simp-$is operators)

; I'VE ASSUMED Z = T => DO NOT SIMPLIFIY ARGUMENTS HERE

(defun simp-$is (x y z)
  (declare (ignore y))
  (let ((a (if z (cadr x) (simplifya (cadr x) z))))
    (if (or (eq a t) (eq a nil))
      a
      `((,(caar x) simp) ,a))))

(defmfun $is (pat)
  (let (patevalled ans)
    (setq ans (mevalp1 pat))
    (cond
      ((memq ans '(t nil)) ans)
      ; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
      ($prederror (pre-err patevalled))
      (t '$unknown))))

(defmfun $maybe (pat)
  (let (patevalled ans)
    (setq ans (mevalp1 pat))
    (cond
      ((memq ans '(t nil)) ans)
      (t '$unknown))))

(defmfun mevalp (pat)
  (let (patevalled ans)
    (setq ans (mevalp1 pat))
    (cond ((memq ans '(#.(not ()) ()))
       ans)
      ; I'D RATHER HAVE ($PREDERROR ($THROW `(($PREDERROR) ,PATEVALLED))) HERE
      ($prederror (pre-err patevalled))
      (t (meval1 pat)))))

(defmspec mand (form) (setq form (cdr form))
  ; (format t "hey, mand: form ~S~%" form)
  (do ((l form (cdr l)) (x) (unevaluated))
    ((null l)
    (cond
      ((= (length unevaluated) 0) t)
      ((= (length unevaluated) 1) (car unevaluated))
      (t (cons '(mand) (reverse unevaluated)))))
  (setq x (mevalp (car l)))
  (cond
    ((null x) (return nil))
    ; DUNNO IF TEST FOR $UNKNOWN IS NEEDED, IF OTHER PATCHES ARE APPLIED ...
    ((eq x '$unknown) (return x))
    ((not (memq x '(t nil))) (push x unevaluated)))))

(defmspec mor (form) (setq form (cdr form))
  ; (format t "hey, mor: form ~S~%" form)
  (do ((l form (cdr l)) (x) (unevaluated))
  ((null l)
    (cond
      ((= (length unevaluated) 0) nil)
      ((= (length unevaluated) 1) (car unevaluated))
      (t (cons '(mor) (reverse unevaluated)))))
  (setq x (mevalp (car l)))
  (cond
    ((eq x t) (return t))
    ; DUNNO IF TEST FOR $UNKNOWN IS NEEDED, IF OTHER PATCHES ARE APPLIED ...
    ((eq x '$unknown) (return x))
    ((not (memq x '(t nil))) (push x unevaluated)))))

(defmspec mnot (form) (setq form (cdr form))
  ; (format t "hey, mnot: form ~S~%" form)
  (let ((x (mevalp (car form))))
    ; (format t "hey, mnot: x ~S~%" x)
    (cond
      ((memq x '(t nil)) (not x))
      ((eq x '$unknown) x)
      (t (simplifya `((mnot) ,x) t)))))

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
        (cons op (apply 'append (mapcar #'(lambda (x y) `(,x ,y)) conditions consequences)))))))

; Simplification of conditional expressions.

; If any B simplifies to T or $TRUE,
; then append B and G to the list of simplified arguments, and ignore any remaining arguments.
; If any B simplifies to NIL or $FALSE, 
; then ignore B and G, and continue processing arguments.
; Otherwise, append B and G to the list of simplified arguments, and continue processing arguments.

; If there are no arguments remaining (i.e., expression is just ((MCOND)) or ((%MCOND))) return $FALSE.
; If the first element of the list of simplified arguments is T or $TRUE, return the second element.
; Otherwise, construct a new conditional expression from the simplified arguments.

; IT APPEARS Z = T => DO NOT SIMPLIFIY ARGUMENTS. NOT SURE HOW THAT CHANGES THIS CODE. NEED TO REVIEW

(defun simp-mcond-shorten (x z)
  (let ((op (car x)) (args (cdr x)) (args-simplified))
    (loop while (> (length args) 0) do
      (let ((b (simplifya (car args) z)) (g (cadr args)))
        (cond
          ((or (eq b nil) (eq b '$false)) nil)
          ((or (eq b t) (eq b '$true))
           (setq args-simplified (append args-simplified (list b (simplifya g z)))
                 args nil))
          (t
            (setq args-simplified (append args-simplified (list b (simplifya g z)))))))
      (setq args (cddr args)))
    
    (cond
      ((null args-simplified) '$false)
      ((or (eq (car args-simplified) t) (eq (car args-simplified) '$true))
       (cadr args-simplified))
      (t
        (cons op args-simplified)))))

; IT APPEARS Z = T => DO NOT SIMPLIFIY ARGUMENTS. NOT SURE HOW THAT CHANGES THIS CODE. NEED TO REVIEW

(defun simp-mcond (x y z)
  (declare (ignore y))
  (simp-mcond-shorten x z))

; flatten_conditional is an interesting idea,
; but it doesn't have much to do with unevaluated conditionals.
#|
(defun $flatten_conditional (x)
  (cond
    ((atom x) x)
    ((or (eq (caar x) 'mcond) (eq (caar x) '%mcond))
     (flatten-conditional1 x))
    (t x)))

;; HEY !! FINISH THIS !! IT'S INTERESTING AND USEFUL !!
(defun flatten-conditional1 (x) x)
|#

(putprop 'mcond 'simp-mcond 'operators)
(putprop '%mcond 'simp-mcond 'operators)

; Redefine REQUIRE-BOOLEAN from nset.lisp --
; current version assumes MEVALP returns only T, NIL, or $UNKNOWN,
; which is no longer the case.

(defun require-boolean (x) (cond ((or (not x) (eq x t)) x) (t nil)))

(putprop 'mnot 'simp-mnot 'operators)
(putprop 'mand 'simp-mand 'operators)
(putprop 'mor 'simp-mor 'operators)

; Simplify the arguments of MAND, and then see if any simplified to $TRUE or T or $FALSE or NIL.
; HEY, IF Z = NIL, DOES THAT MEAN "DO NOT SIMPLIFY THE ARGUMENTS OF X"
; OR DOES IT MEAN "SIMPLIFY THE ARGUMENTS OF X AND PASS NIL TO SIMPLFYA" ??
; I'LL ASSUME FOR SIMP-MAND AND SIMP-MOR THAT IT IS THE FORMER INTERPRETATION.

; IT APPEARS Z = T => DO NOT SIMPLIFIY ARGUMENTS. NOT SURE HOW THAT CHANGES THIS CODE. NEED TO REVIEW

(defun simp-mand (x y z)
  (declare (ignore y))
  (let ((args (cdr x)))
    ; MAYBE SHOULD AVOID SIMPLIFYING ARGUMENTS IF WE CAN
    ; ALTHOUGH AVOIDING SIMPLIFICATION MAKES THE OPERATOR NONCOMMUTATIVE,
    ; SO MAYBE THAT'S NOT SUCH A GOOD IDEA AFTER ALL
    (setq args (if (not (memq 'simp (cdar x))) (mapcar #'(lambda (a) (simplifya a z)) args) args))
    (if (some #'(lambda (a) (or (eq a nil) (eq a '$false))) args)
      nil
      (progn
        (setq args (remove-if #'(lambda (a) (or (eq a t) (eq a '$true))) args))
        (if (null args)
          t
          (if (eq (length args) 1)
            (car args)
            `((mand simp) ,@args)))))))

; Simplify the arguments of MOR, and then see if any simplified to $TRUE or T or $FALSE or NIL.

; IT APPEARS Z = T => DO NOT SIMPLIFIY ARGUMENTS. NOT SURE HOW THAT CHANGES THIS CODE. NEED TO REVIEW

(defun simp-mor (x y z)
  (declare (ignore y))
  (let ((args (cdr x)))
    ; MAYBE SHOULD AVOID SIMPLIFYING ARGUMENTS IF WE CAN
    ; ALTHOUGH AVOIDING SIMPLIFICATION MAKES THE OPERATOR NONCOMMUTATIVE,
    ; SO MAYBE THAT'S NOT SUCH A GOOD IDEA AFTER ALL
    (setq args (if (not (memq 'simp (cdar x))) (mapcar #'(lambda (a) (simplifya a z)) args) args))
    (if (some #'(lambda (a) (or (eq a t) (eq a '$true))) args)
      t
      (progn
        (setq args (remove-if #'(lambda (a) (or (eq a nil) (eq a '$false))) args))
        (if (null args)
          nil
          (if (eq (length args) 1)
            (car args)
            `((mor simp) ,@args)))))))

; CUT STUFF ABOUT NOT EQUAL => NOTEQUAL AT TOP OF ASSUME

; LOOKS LIKE I ASSUMED SIMPLIFY ARGUMENT ONLY IF Z != NIL HERE

; IT APPEARS Z = T => DO NOT SIMPLIFIY ARGUMENTS. NOT SURE HOW THAT CHANGES THIS CODE. NEED TO REVIEW

(defun simp-mnot (x y z)
  (declare (ignore y))
  (let ((arg (cadr x)))
    (if (atom arg)
      (cond
        ((or (eq arg t) (eq arg '$true))
         nil)
        ((or (eq arg nil) (eq arg '$false))
         t)
        (t x))
      (let ((arg-op (caar arg)) (arg-arg (cdr arg)))
        (if (not (memq 'simp (cdar x)))
          (setq arg-arg (mapcar #'(lambda (a) (simplifya a z)) arg-arg)))
        (cond
          ((eq arg-op 'mlessp)
           `((mgeqp) ,@arg-arg))
          ((eq arg-op 'mleqp)
           `((mgreaterp) ,@arg-arg))
          ((eq arg-op 'mequal)
           `((mnotequal) ,@arg-arg))
          ((eq arg-op '$equal)
           `(($notequal) ,@arg-arg))
          ((eq arg-op 'mnotequal)
           `((mequal) ,@arg-arg))
          ((eq arg-op '$notequal)
           `(($equal) ,@arg-arg))
          ((eq arg-op 'mgeqp)
           `((mlessp) ,@arg-arg))
          ((eq arg-op 'mgreaterp)
           `((mleqp) ,@arg-arg))
          ((eq arg-op 'mnot)
           (car arg-arg))
          ; Distribute negation over conjunction and disjunction;
          ; analogous to '(- (a + b)) --> - a - b.
          ((eq arg-op 'mand)
           `((mor) ((mnot) ,(car arg-arg)) ((mnot) ,(cadr arg-arg))))
          ((eq arg-op 'mor)
           `((mand) ((mnot) ,(car arg-arg)) ((mnot) ,(cadr arg-arg))))
          (t x))))))

(let ((save-intext (symbol-function 'intext)))
  (defun intext (rel body)
    (let ((result (funcall save-intext rel body)))
      (if result result `((,rel) ,@body)))))
