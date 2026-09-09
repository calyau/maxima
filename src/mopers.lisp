;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mopers macro)

;; This file is the compile-time half of the OPERS package, an interface to the
;; Maxima general representaton simplifier.  When new expressions are being
;; created, the macros in this file or the functions in NOPERS should be called
;; rather than the entrypoints in SIMP such as SIMPLIFYA or SIMPLUS.

;; The basic functions are ADD, SUB, MUL, DIV, POWER, NCMUL, NCPOWER, INV.
;; Each of these functions assume that their arguments are simplified.  Some
;; functions will have a "*" adjoined to the end of the name (as in ADD*).
;; These do not assume that their arguments are simplified.  The above
;; functions are the only entrypoints to this package.

;; The functions ADD2, MUL2, and MUL3 are for use internal to this package
;; and should not be called externally.

;; I have added the macro DEFGRAD as an interface to the $DERIVATIVE function
;; for use by macsyma programmers who want to do a bit of lisp programming. -GJC

(defmacro =0 (x) `(equal ,x 0))
(defmacro =1 (x) `(equal ,x 1))

;; Addition -- call ADD with simplified operands,
;;             ADD* with unsimplified operands.

(defun add (&rest terms)
  (if (= (length terms) 2)
      (apply #'add2 terms)
      (apply #'addn `(,terms t))))

(define-compiler-macro add (&rest terms)
  (if (= (length terms) 2)
      `(add2 ,@terms)
      `(addn (list ,@terms) t)))

(defun add* (&rest terms)
  (if (= (length terms) 2)
      (apply #'add2* terms)
      (apply #'addn `(,terms nil))))

(define-compiler-macro add* (&rest terms)
  (if (= (length terms) 2)
      `(add2* ,@terms)
      `(addn (list ,@terms) nil)))

;; Multiplication -- call MUL or NCMUL with simplified operands,
;;                        MUL* or NCMUL* with unsimplified operands.

(defun mul (&rest factors)
  (cond ((= (length factors) 2) (apply #'mul2 factors))
        ((= (length factors) 3) (apply #'mul3 factors))
        (t (apply #'muln `(,factors t)))))

(define-compiler-macro mul (&rest factors)
  (cond ((= (length factors) 2) `(mul2 ,@factors))
	((= (length factors) 3) `(mul3 ,@factors))
	(t `(muln (list ,@factors) t))))

(defun mul* (&rest factors)
  (if (= (length factors) 2)
      (apply #'mul2* factors)
      (apply #'muln `(,factors nil))))

(define-compiler-macro mul* (&rest factors)
  (if (= (length factors) 2)
      `(mul2* ,@factors)
      `(muln (list ,@factors) nil)))

(defmacro inv (x)
  `(power ,x -1))

(defmacro inv* (x)
  `(power* ,x -1))

(defmacro ncmul (&rest factors)
  (if (= (length factors) 2)
      `(ncmul2 ,@factors)
      `(ncmuln (list ,@factors) t)))

;; (TAKE '(%TAN) X) = tan(x)
;;
;; Stavros says it's named take:
;;   "Take as in 'take the sine of ...'.  call or apply might imply
;;   it's a function call, which it isn't."
;;
;; This syntax really loses.  Not only does this syntax lose, but this macro
;; has to look like a subr.  Otherwise, the definition would look like
;; (DEFMACRO TAKE ((NIL (OPERATOR)) . ARGS) ...)

;; (TAKE A B) --> (SIMPLIFYA (LIST A B) T)
;; (TAKE '(%SIN) A) --> (SIMP-%SIN (LIST '(%SIN) A) 1 T)

(defmacro take (operator &rest args)
	`(simplifya (list ,operator ,@args) t))

;; take* does not assume that the arguments are simplified.
(defmacro take* (operator &rest args)
  `(simplifya (list ,operator ,@args) nil))

;; Like TAKE, but you only need to specify then name.  So
;;
;; (ftake name x y) => (take '(name) x y)
;;
;; The name should be the verb form, like %foo.
(defmacro ftake (name &rest args)
  `(simplifya (list (list ,name) ,@args)
	      t))

(defmacro ftake* (name &rest args)
  `(simplifya (list (list ,name) ,@args)
	      nil))

;; Apply a function f to a list of its arguments 'args' and simplify the result. Assume
;; that the list args is simplified.
(defmacro fapply (f args)
	`(simplifya (cons (list ,f) ,args) t))

;; Same as fapply, but don't assume that the list of arguments is simplified.
(defmacro fapply* (f args)
	`(simplifya (cons (list ,f) ,args) nil))        

(declaim (inline simplify))
(defun simplify (x)
  (simplifya x nil))

;; A hand-made DEFSTRUCT for dealing with the Maxima MDO structure.
;; Used in GRAM, etc. for storing/retrieving from DO structures.

(defmacro make-mdo () '(list (list 'mdo) nil nil nil nil nil nil nil))

(defmacro mdo-op (x)     `(car (car ,x)))

(defmacro mdo-for (x)    `(second ,x))
(defmacro mdo-from (x)   `(third ,x))
(defmacro mdo-step (x)   `(fourth ,x))
(defmacro mdo-next (x)   `(fifth ,x))
(defmacro mdo-thru (x)   `(sixth ,x))
(defmacro mdo-unless (x) `(seventh ,x))
(defmacro mdo-body (x)	 `(eighth ,x))

(defvar *defgrad-syms* nil
  "A list of all the symbols whose derivatives are defined by DEFGRAD")

(defun check-defgrad (a g)
  ;; See if the arg A exists somewhere in the expression G.
  (cond ((atom g)
         (eq a g))
        ((consp g)
         (some #'identity
               (mapcar #'(lambda (g)
                           (check-defgrad a g))
                       g)))
        (t nil)))

;; DEFGRAD defines derivatives for the function NAME having arguments ARGUMENTS.
;;
;;   NAME       - the noun-form (%foo) of the function
;;   ARGUMENTS  - A list of the arguments of the function.
;;   BODY       - The derivatives of the function.  This should be a list of
;;                derivatives arranged in the same order as the ARGUMENTS.
;;
;; The derivatives can be expressed using #$$...$.  In this case the
;; names of the arguments MUST start with "$" because #$$ read
;; expressions that way.
;;
;; Use of #$$ is not required.  In that case, each derivative must be
;; a quoted list of the maxima internal representation of the
;; derivative.
;;
;; In general do NOT use #$...$ because this calls the simplifier and
;; requires basically all of maxima to be available to simplify the
;; expression.  It might work, but it's best just to use #$$...$.
;;
;; For example here are two ways to define the derivatives of atan2:
;;
;;   (defgrad %atan2 ($x $y)
;;     #$$y/(y^2+x^2)$
;;     #$$-(x/(y^2+x^2))$)
;;  
;;   (defgrad %atan2 (x y)
;;     '((mtimes) y
;;       ((mexpt) ((mplus) ((mexpt) x 2) ((mexpt) y 2)) -1))
;;     '((mtimes) -1 x
;;       ((mexpt) ((mplus) ((mexpt) x 2) ((mexpt) y 2)) -1)))
;;
;; The first form is clearly easier to read and understand, but either
;; scheme will work.
;;
;; The derivative forms can also be lambda's.  See gamma_incomplete.
;; But if the lambda also uses #$$...$, it MUST call meval* itself to
;; make sure the result is appropriately simplified.  A lambda that
;; builds its result from the actual arguments returns T as a second
;; value, so that SDIFFGRAD substitutes nothing into it.

(defmacro defgrad (name arguments &body body)
  "DEFGRAD defines derivatives for the function NAME having arguments ARGUMENTS.

    NAME       - the noun-form (%foo) of the function
    ARGUMENTS  - A list of the arguments of the function.
    BODY       - The derivatives of the function.  This should be a list of
                 derivatives arranged in the same order as the ARGUMENTS.

  The derivatives can be expressed using #$$...$.  In this case the
  names of the arguments MUST start with \"$\" because #$$ read
  expressions that way.

  Use of #$$ is not required.  In that case, each derivative must be a
  quoted list of the maxima internal representation of the derivative.

  The derivative may also be a lambda expression that returns the
  derivative or NIL.  Its result is substituted into like an expression
  in the placeholder names, unless it returns T as a second value: the
  result is then taken as it is, which lets it be built from the actual
  arguments."
  ;; Check that the argument variables show up somewhere in the body.
  ;; Otherwise, the defintion of the derivative is potentially
  ;; incorrect.
  (loop for arg in arguments
        unless (check-defgrad arg body)
          do
             (progn
               ;; Print a warning that the definition is wrong
               ;; and exit the loop.  Don't use MWARNING for this
               ;; because DISPLA may not be defined yet to print
               ;; the message.
               (warn "DEFGRAD ~A: Argument ~S not used in derivative expressions."
                     name arg)
               (return nil)))
  `(progn
     (push ',name *defgrad-syms*)
     (setf (get ',name 'grad)
           `(,',arguments
             ,,@body))))

;; When DEFGRAD uses #$$ to define derivatives, we MUST call MEVAL* on
;; them to get them simplified appropriately.  It's ok to call MEVAL*
;; if we didn't use #$$.
(defun process-defgrad ()
  (dolist (sym *defgrad-syms*)
    (destructuring-bind (args &rest glist)
        (get sym 'grad)
      (setf (get sym 'grad)
            (list* args
                   (mapcar #'meval*
                           glist))))))
