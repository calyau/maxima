;;;   -*- Mode:Lisp; Package:SERROR; Base:10; Syntax:COMMON-LISP -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1985,86 by William Schelter,University of Texas  ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "SERROR")
(export '(def-error-type cond-error cond-any-error condition-case
	   error-name error-string error-continue-string error-format-args
	   ) "SERROR")


(eval-when (compile)
	   (proclaim '(optimize (safety 2) (speed 2) (space 2))))

;;do (require "SERROR")
;;(use-package "SERROR")


;;We set up very primitive error catching for a common lisp
;;whose primitive error handler is called si:universal-error-handler (eg kcl).
;;Namely if *catch-error* is not nil then that means
;;there is a (catch ':any-error somewhere up the stack.
;;it is thrown to, along with the condition.  
;;At the that point if the condition matches that of 
;;the catch, it stops there,
;;otherwise if *catch-error* is still not nil repeat
;;Sample interface

;(defun te (n m)
;  (cond-error (er) (hairy-arithmetic  m n)
;     ((and (= 0 n) (= 0 m))(format t "Hairy arithmetic doesn't like m=0=n") 58)
;     ((eql (error-condition-name er) :wrong-type-args)(format t "Bonus for wrong args") 50)
;     ((symbolp n)(and (numberp (symbol-value n))(format t "Had to eval n") (te m (symbol-value n)))))



;;if none of the cond clauses hold, then we signal a regular error using
;;the system error handler , unless there are more *catch-error*'s up
;;the stack.  Major defect: If none of the conditions hold, we will have
;;to signal our real error up at the topmost *catch-error* so losing the possibility
;;of proceeding. The alternative is to some how get the tests down to where
;;we want them, but that seems to mean consing a closure, and keeping a
;;stack of them.  This is getting a little fancy.  
;;don't know how to get back (and anyway we have unwound by throwing).
;;Major advantages: If there is no error, no closures are consed, and
;;should be reasonably fast.



;;****** Very system dependent.  Redefine main error handler ******
(eval-when (load compile eval)
#-kcl
(defun si::universal-error-handler (&rest args)
  (format t "Calling orignal error handler ~a" args))

(defvar *error-handler-function* 'si::universal-error-handler)
(or (get   *error-handler-function* :old-definition)
   (setf (get *error-handler-function* :old-definition)
	 (symbol-function *error-handler-function*)))
)

(defstruct (error-condition :named (:conc-name error-))
  name
  string          ;the format string given to error.
  function        ;occurs inside here
  continue-string
  format-args
  error-handler-args)

(defparameter *catch-error* nil "If t errors will throw to :any-error tag")
(defparameter *disable-catch-error* nil "If t only regular error handler will be used")
(defparameter *catch-error-stack* (make-array 30 :fill-pointer 0) "If t only regular error handler will be used")
(defvar *show-all-debug-info* nil "Set to t if not
 running interactively")

;;principal interfaces

(defmacro cond-error (variables body-form &body clauses)
  "If a condition is signalled during evaluation of body-form, The first
of VARIABLES is bound to the condition, and the clauses are evaluated
like cond clauses. Note if the conditions involve lexical variables other than
VARIABLES, there will be a new lexical closure cons'd each time through this!!
 eg:
 (cond-error (er) (1+ u)
  ((null u) (princ er) (princ \"null arg to u\"))
  ((symbolp u) (princ \"symbol arg\"))
  (t 0))"
  (or variables (setf variables '(ignore)))
  (let ((catch-tag (gensym "CATCH-TAG")))
  (let ((bod `((catch ',catch-tag 
	       (return-from cond-error-continue
			    (unwind-protect
				(progn
				  (vector-push-extend
				   #'(lambda ,variables ,(car variables)
				       (if (or ,@ (mapcar 'car clauses)) ',catch-tag))
				   *catch-error-stack*)
				  ,body-form)
			      (incf (the fixnum (fill-pointer *catch-error-stack*))
				    -1))))
	     (cond ,@ clauses
		   (t (format t "should not get here") )))))
  (cond (variables
	 (setf bod 
	      ` (multiple-value-bind
		,variables ,@ bod)))
	 (t (setf bod (cons 'progn bod))))
  `(block cond-error-continue ,bod))))

(defmacro cond-any-error (variables body-form &body clauses)
  "If a condition is signalled during evaluation of body-form, The first
of VARIABLES is bound to the condition, and the clauses are evaluated
like cond clauses, If the cond falls off the end, then the error is
signaled at this point in the stack.  For the moment the rest of the VARIABLES are ignored.
 eg:
 (cond-error (er) (1+ u)
  ((null u) (princ er) (princ \"null arg to u\"))
  ((symbolp u) (princ \"symbol arg\"))
  (t 0))"
  (let ((bod `(
	       (let ((*catch-error* t))
		 (catch ':any-error
		   (return-from cond-error-continue ,body-form)))
	       (cond ,@ clauses
		     (t (inf-signal ,@ variables))))))
    (cond (variables
	   (setf bod 
		 ` (multiple-value-bind
		    ,variables ,@ bod)))
	  (t (setf bod (cons 'progn bod))))
    `(block cond-error-continue ,bod)))



(defun #. (if (boundp '*error-handler-function*)*error-handler-function* 'joe)
  (&rest error-handler-args)
  (when *show-all-debug-info*
       (si::simple-backtrace)(si::backtrace) (si::break-vs))
  (let ((err (make-error-condition
			     :name (car error-handler-args)
			     :string (fifth error-handler-args)
			     :function (third error-handler-args)
			     :continue-string (fourth error-handler-args)
			     :format-args
			     (copy-list (nthcdr 5 error-handler-args))
			     :error-handler-args (copy-list error-handler-args))))
    (cond (*catch-error* (throw :any-error err))
	  ((let (flag) (do ((i 0 (the fixnum (1+ i)))
			    (end (the fixnum(fill-pointer (the array
						    *catch-error-stack*)))))
			   ((>= i end))
			   (declare (fixnum i end))
			   (cond ((setq flag
					(funcall (aref *catch-error-stack* i)
						      err))
				  (throw flag err))))))
	  (t    (apply (get *error-handler-function* :old-definition)
		       error-handler-args)))))

(defun inf-signal (&rest error-handler-args)
 (apply *error-handler-function*
                     (error-error-handler-args (car error-handler-args ))))
;(defun inf-signal (&rest error-handler-args)
;  (cond ((and *catch-error* (null *disable-catch-error*)) (throw :any-error (apply 'values  error-handler-args)))
;	(t (apply *error-handler-function*
;                     (error-error-handler-args (car error-handler-args ))))))

;(defun te (n)
;  (cond-error (er) (progn (1+ n) (si:universal-error-handler 'a 'b 'c 'd 'e))
;     ((null n) (print n) (print er) n)
;     ((symbolp n) (print n))))
(defmacro def-error-type (name (er) &body body)
  (let ((fname (intern (format nil "~a-tester" name))))
  `(eval-when (compile eval load)
      (defun ,fname (,er) ,@ body)
      (deftype ,name ()`(and error-condition (satisfies ,',fname))))))
(def-error-type wta (er) (eql (error-name er) :wrong-type-arg))
;(def-error-type hi-error (er) (eql (error-string er) "hi"))
;this matches error signaled by (error "hi") or (cerror x "hi" ..)
;can use the above so that the user can put
;(cond-error (er ) (hairy-stuff)
;  ((typep er 'wta) ...)
;  ((typep er '(or hi-error joe)) ...)
;(defun te2 (n)
;  (sloop for i below n with x = 0 declare (fixnum x)
;	 do (cond-any-error (er) (setq x i)
;			(t (print "hi")))))
;;In kcl cond-any-error is over 10 times as fast as cond-error, for the above.
;;Note since t a clause we could have optimized to cond-any-error!!
;;cond-error takes 1/1000 of second on sun 2
;;cond-any-error takes 1/10000 of second. (assuming no error!).


(def-error-type subscript-out-of-bounds (er)
  #+ti (member 'si::subscript-out-of-bounds (funcall er :condition-names))
  #+ekcl(equal (error-string er) "The first index, ~S, to the array~%~S is too large.")) ;should collect all here
(def-error-type ERROR (er) (eql (error-name er) :error))
(def-error-type WRONG-TYPE-ARGUMENT (er)  (eql (error-name er) :WRONG-TYPE-ARGUMENT))
(def-error-type TOO-FEW-ARGUMENTS (er)  (eql (error-name er) :TOO-FEW-ARGUMENTS))
(def-error-type TOO-MANY-ARGUMENTS (er)  (eql (error-name er) :TOO-MANY-ARGUMENTS))
(def-error-type UNEXPECTED-KEYWORD (er)  (eql (error-name er) :UNEXPECTED-KEYWORD))
(def-error-type INVALID-FORM (er)  (eql (error-name er) :INVALID-FORM))
(def-error-type UNBOUND-VARIABLE (er)  (eql (error-name er) :UNBOUND-VARIABLE))
(def-error-type INVALID-VARIABLE (er)  (eql (error-name er) :INVALID-VARIABLE))
(def-error-type UNDEFINED-FUNCTION (er)  (eql (error-name er) :UNDEFINED-FUNCTION))
(def-error-type INVALID-FUNCTION (er)   (eql (error-name er) :INVALID-FUNCTION))

(defmacro condition-case (vars body-form &rest cases)
  (let ((er (car vars)))
  `(cond-error (,er) ,body-form
	       ,@ (sloop for v in cases
			 when (listp (car v))
			 collecting `((typep ,er '(or ,@ (car v))),@ (cdr v))
			 else
			 collecting `((typep ,er ',(car v)),@ (cdr v))))))

	       


