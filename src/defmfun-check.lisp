;;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;; We got this code from cmucl, so we don't actually need all of this.
#+cmucl
(progn
(defun parse-lambda-list (list)
  (kernel:parse-lambda-list list))
(defun parse-body (body environment &optional (doc-string-allowed t))
  (system:parse-body body environment doc-string-allowed))
)

#-cmucl
(eval-when (compile load eval)
;;;; Borrowed from cmucl src/code/extensions.lisp.  Used in parsing
;;;; lambda lists.

;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))


;;; Collect  --  Public
;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
	(error (intl:gettext "Malformed collection specifier: ~S.") spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

;;; Borrowed from cmucl src/compiler/proclaim.lisp

;;; Parse-Lambda-List  --  Interface
;;;
;;;    Break a lambda-list into its component parts.  We return eleven values:
;;;  1] A list of the required args.
;;;  2] A list of the optional arg specs.
;;;  3] True if a rest arg was specified.
;;;  4] The rest arg.
;;;  5] A boolean indicating whether keywords args are present.
;;;  6] A list of the keyword arg specs.
;;;  7] True if &allow-other-keys was specified.
;;;  8] A list of the &aux specifiers.
;;;  9] True if a more arg was specified.
;;; 10] The &more context var
;;; 11] The &more count var
;;;
;;; The top-level lambda-list syntax is checked for validity, but the arg
;;; specifiers are just passed through untouched.  If something is wrong, we
;;; use Compiler-Error, aborting compilation to the last recovery point.
;;;
(defun parse-lambda-list (list)
  (declare (list list))
  (collect ((required)
	    (optional)
	    (keys)
	    (aux))
    (flet ((compiler-error (&rest args)
	     (apply #'error args))
	   (compiler-note (&rest args)
	     (apply #'warn args)))
      (let ((restp nil)
	    (rest nil)
	    (morep nil)
	    (more-context nil)
	    (more-count nil)
	    (keyp nil)
	    (allowp nil)
	    (state :required))
	(dolist (arg list)
	  ;; check for arguments that have the syntactic form of a
	  ;; keyword argument without being a recognized lambda-list keyword
	  (when (and (symbolp arg)
		     (let ((name (symbol-name arg)))
		       (and (/= (length name) 0)
			    (char= (char name 0) #\&))))
	    (unless (member arg lambda-list-keywords)
	      (compiler-note
	       "~S uses lambda-list keyword naming convention, but is not a recognized lambda-list keyword."
	       arg)))
	  (if (member arg lambda-list-keywords)
	      (ecase arg
		(&optional
		 (unless (eq state :required)
		   (compiler-error "Misplaced &optional in lambda-list: ~S." list))
		 (setq state '&optional))
		(&rest
		 (unless (member state '(:required &optional))
		   (compiler-error "Misplaced &rest in lambda-list: ~S." list))
		 (setq state '&rest))
		(&more
		 (unless (member state '(:required &optional))
		   (compiler-error "Misplaced &more in lambda-list: ~S." list))
		 (setq morep t  state '&more-context))
		(&key
		 (unless (member state '(:required &optional :post-rest
					 :post-more))
		   (compiler-error "Misplaced &key in lambda-list: ~S." list))
		 (setq keyp t)
		 (setq state '&key))
		(&allow-other-keys
		 (unless (eq state '&key)
		   (compiler-error "Misplaced &allow-other-keys in lambda-list: ~S." list))
		 (setq allowp t  state '&allow-other-keys))
		(&aux
		 (when (member state '(&rest &more-context &more-count))
		   (compiler-error "Misplaced &aux in lambda-list: ~S." list))
		 (setq state '&aux)))
	      (case state
		(:required (required arg))
		(&optional (optional arg))
		(&rest
		 (setq restp t  rest arg  state :post-rest))
		(&more-context
		 (setq more-context arg  state '&more-count))
		(&more-count
		 (setq more-count arg  state :post-more))
		(&key (keys arg))
		(&aux (aux arg))
		(t
		 (compiler-error "Found garbage in lambda-list when expecting a keyword: ~S." arg)))))

	(when (eq state '&rest)
	  (compiler-error "&rest not followed by required variable."))
      
	(values (required) (optional) restp rest keyp (keys) allowp (aux)
		morep more-context more-count)))))

(defun parse-body (body environment &optional (doc-string-allowed t))
  "This function is to parse the declarations and doc-string out of the body of
  a defun-like form.  Body is the list of stuff which is to be parsed.
  Environment is ignored.  If Doc-String-Allowed is true, then a doc string
  will be parsed out of the body and returned.  If it is false then a string
  will terminate the search for declarations.  Three values are returned: the
  tail of Body after the declarations and doc strings, a list of declare forms,
  and the doc-string, or NIL if none."
  (declare (ignore environment))
  (let ((decls ())
	(doc nil))
    (do ((tail body (cdr tail)))
	((endp tail)
	 (values tail (nreverse decls) doc))
      (let ((form (car tail)))
	(cond ((and (stringp form) (cdr tail))
	       (if doc-string-allowed
		   (setq doc form
			 ;; Only one doc string is allowed.
			 doc-string-allowed nil)
		   (return (values tail (nreverse decls) doc))))
	      ((not (and (consp form) (symbolp (car form))))
	       (return (values tail (nreverse decls) doc)))
	      ((eq (car form) 'declare)
	       (push form decls))
	      (t
	       (return (values tail (nreverse decls) doc))))))))
)


;; Define user-exposed functions that are written in Lisp.
;;
;; If the function name NAME starts with #\$ we check the number of
;; arguments.  In this case, two functions are created: NAME and
;; NAME-IMPL (without the leading $).  NAME is the user function that
;; checks for the argument count and NAME-IMPL is the actual
;; implementation..
;;
;; If the function name doesn't start with $, we still allow it, but
;; these should be replaced with plain defun eventually.
;;
;; The lambda-list supports &optional and &rest args.  Keyword args
;; are an error.
(defmacro defmfun (name lambda-list &body body)
  (let ((maclisp-narg-p (and (symbolp lambda-list) (not (null lambda-list)))))
    (cond
      ((or (char/= #\$ (aref (string name) 0))
	   maclisp-narg-p)
       ;; If NAME doesn't start with $, it's an internal function not
       ;; directly exposed to the user.  Basically define the function
       ;; as is, taking care to support the Maclisp narg syntax.
       (cond (maclisp-narg-p
	      ;; Support MacLisp narg syntax:  (defun foo a ...)
	      `(progn
		 (defprop ,name t translated)
		 (defun ,name (&rest narg-rest-argument
			       &aux (,lambda-list (length narg-rest-argument)))
		   ,@body)))
	     (t
	      `(progn
		 (defprop ,name t translated)
		 (defun ,name ,lambda-list ,@body)))))
      (t
       ;; Function name begins with $, so it's exposed to the user;
       ;; carefully check the number of arguments and print a nice
       ;; message if the number doesn't match the expected number.
       #+nil
       (unless (char= #\$ (aref (string name) 0))
	 (warn "First character of function name must start with $: ~S~%" name))
       (multiple-value-bind (required-args
			     optional-args
			     restp
			     rest-arg
			     keywords-present-p)
	   (parse-lambda-list lambda-list)

	 (when keywords-present-p
	   (error "Keyword arguments are not supported"))

	 (let* ((required-len (length required-args))
		(optional-len (length optional-args))
		(impl-name (intern (concatenate 'string
						(string name)
						"-IMPL")))
		(impl-doc (format nil "Implementation for ~S" name))
		(nargs (gensym "NARGS-"))
		(args (gensym "REST-ARG-"))
		(rest-name (gensym "REST-ARGS"))
		(pretty-fname
		 (cond (optional-args
			;; Can't do much with optional args, so just use the function name.
			name)
		       (restp
			;; Use maxima syntax for rest args: foo(a,b,[c]);
			`((,name) ,@required-args ((mlist) ,rest-arg)))
		       (t
			;; Just have required args: foo(a,b)
			`((,name) ,@required-args)))))

	   (multiple-value-bind (forms decls doc-string)
	       (parse-body body nil t)
	     (setf doc-string (if doc-string (list doc-string)))
	     `(progn
		(defun ,impl-name ,lambda-list
		  ,impl-doc
		  ,@decls
		  (block ,name
		    ,@forms))
		(defprop ,name t translated)
		(defun ,name (&rest ,args)
		  ,@doc-string
		  (let ((,nargs (length ,args)))
		    (declare (ignorable ,nargs))
		    ,@(cond
			(restp
			 ;; When a rest arg is given, there's no upper
			 ;; limit to the number of args.  Just check that
			 ;; we have enough args to satisfy the required
			 ;; args.
			 (unless (null required-args)
			   `((when (< ,nargs ,required-len)
			       (merror (intl:gettext "~M: expected at least ~M arguments but got ~M: ~M")
				       ',pretty-fname
				       ,required-len
				       ,nargs
				       (list* '(mlist) ,args))))))
			(optional-args
			 ;; There are optional args (but no rest
			 ;; arg). Verify that we don't have too many args,
			 ;; and that we still have all the required args.
			 `(
			   (when (> ,nargs ,(+ required-len optional-len))
			     (merror (intl:gettext "~M: expected at most ~M arguments but got ~M: ~M")
				     ',pretty-fname
				     ,(+ required-len optional-len)
				     ,nargs
				     (list* '(mlist) ,args)))
			   (when (< ,nargs ,required-len)
			     (merror (intl:gettext "~M: expected at least ~M arguments but got ~M: ~M")
				     ',pretty-fname
				     ,required-len
				     ,nargs
				     (list* '(mlist) ,args)))))
			(t
			 ;; We only have required args.
			 `((unless (= ,nargs ,required-len)
			     (merror (intl:gettext "~M: expected exactly ~M arguments but got ~M: ~M")
				     ',pretty-fname
				     ,required-len
				     ,nargs
				     (list* '(mlist) ,args))))))
		    (apply #',impl-name ,args)))
		(define-compiler-macro ,name (&rest ,rest-name)
		  `(,',impl-name ,@,rest-name))))))))))

;; Examples:
;; (defmfun $foobar (a b) (list '(mlist) a b))
;; (defmfun $foobar1 (a b &optional c) (list '(mlist) a b c))
;; (defmfun $foobar1a (a b &optional (c 99)) (list '(mlist) a b c))
;; (defmfun $foobar2 (a b &rest c) (list '(mlist) a b (list* '(mlist) c)))
;; (defmfun $foobar3 (a b &optional c &rest d) "foobar3 function" (list '(mlist) a b c (list* '(mlist) d)))
;;
;; This works by accident, kind of:
;; (defmfun $baz (a &aux (b (1+ a))) (list '(mlist) a b))

;; This should produce compile errors
;; (defmfun $zot (a &key b) (list '(mlist) a b))
