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

(defun defmfun-keywords (fname options valid-keywords)
  ;; options looks like (((mequal) $opt1 val1) ((mequal) $opt2 val2) ...)
  ;;
  ;; Convert to a new list that looks like (:opt1 val1 :opt2 val2 ...)
  ;;
  (unless (listp options)
    (merror "Invalid Maxima keyword options: ~M" options))
  (when (every #'(lambda (o)
		   ;; Make sure every option has the right form.
		   (let ((ok (and (listp o)
				  (= (length o) 3)
				  (eq (caar o) 'mequal))))
		     (unless ok
		       (merror (intl:gettext "~M: Badly formed keyword option: ~M")
			       fname o))
		     ok))
	       options)
    (mapcan #'(lambda (o)
		(destructuring-bind (mequal opt val)
		    o
		  (declare (ignore mequal))
		  (if (or (null valid-keywords)
			  (member opt valid-keywords))
		      (flet ((keywordify (x)
			       (intern (subseq (symbol-name x) 1) :keyword)))
			(list (keywordify opt) val))
		      (merror (intl:gettext "~M: Unrecognized keyword: ~M")
			      fname opt))))
	    options)))

;; Internal macro to do the heavy lifting of defining a function that
;; checks the number of arguments of a function.  This is intended to
;; give nice error messages to user-callable functions when the number
;; of arguments is incorrect.
;;
;; The function to check arguments is named NAME.  The actual
;; implementation is in a new function named IMPL, which is called by
;; NAME.  A compiler-macro is also defined so that Lisp calls of NAME
;; get automatically converted to IMPL.
;;
;; If the keyword :DEPRECATED-P is also specified, then the function
;; is deprecated which causes a warning to be printed once when the
;; function NAME is called the first time.  The value of :DEPRECATED-P
;; is a symbol naming the function that should be used instead.
;;
;; For example:
;;
;;   (defun-checked-form ($foo foo-impl :deprecated-p $bar) ...)
;;
;;
;; The lambda-list supports &optional and &rest args.  Keyword args
;; (&key) are also supported.  Maxima keyword args (a=b) are converted
;; to Lisp keywords appropriately.  Unrecognized keywords signal a
;; Maxima error.
;;
;; The variable %%PRETTY-FNAME is defined such that the body can refer
;; to this variable to get the pretty name of the defined function for
;; use in printing error messages or what not.  This allows the
;; implementation to print out the function name that would also be
;; used when printing out error messages for incorrect number of
;; arguments.

(defmacro defun-checked-form ((name impl-name &key deprecated-p) lambda-list &body body)
  ;; Carefully check the number of arguments and print a nice message
  ;; if the number doesn't match the expected number.
  (multiple-value-bind (required-args
			optional-args
			restp
			rest-arg
			keywords-present-p
			keyword-args
			allow-other-keys-p)
      (parse-lambda-list lambda-list)

    (when (and keywords-present-p
	       (or optional-args restp))
      (error "Keyword args cannot be used with optional args or rest args"))

    (let* ((required-len (length required-args))
	   (optional-len (length optional-args))
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
		   (keywords-present-p
		    ;; Not exactly sure how to do this
		    (let* ((index 1)
			   (keys (mapcar
				  #'(lambda (k)
				      (multiple-value-bind (name val)
					  (if (consp k)
					      (values
					       (intern (format nil "$~A" (car k)))
					       (second k))
					      (values
					       (intern (format nil "$~A" k))
					       nil))
					(incf index)
					`((mequal) ,name ,val)))
				  keyword-args)))
		      `((,name) ,@required-args ,@keys)))
		   (t
		    ;; Just have required args: foo(a,b)
		    `((,name) ,@required-args))))
	   (maxima-keywords
	     (unless allow-other-keys-p
	       (mapcar #'(lambda (x)
			   (intern (concatenate
				    'string "$"
				    (symbol-name
				     (if (consp x)
					 (car x)
					 x)))))
		       keyword-args)))
	   (warning-done-var (gensym "WARNING-DONE-")))

      (multiple-value-bind (forms decls doc-string)
	  (parse-body body nil t)
	(setf doc-string (if doc-string (list doc-string)))
	`(progn
	   (defun ,impl-name ,lambda-list
	     ,impl-doc
	     ,@decls
	     (block ,name
	       (let ((%%pretty-fname ',pretty-fname))
		 (declare (ignorable %%pretty-fname))
		 ,@forms)))

	   (let ,(when deprecated-p `((,warning-done-var nil)))
	     (defun ,name (&rest ,args)
	       ,@doc-string
	       ,@(when deprecated-p
		   `((unless ,warning-done-var
		       (setf ,warning-done-var t)
		       (mwarning (aformat nil (intl:gettext "~M is deprecated; use ~M.")
					  ',name ',deprecated-p)))))
	       (let ((,nargs (length ,args)))
		 (declare (ignorable ,nargs))
		 ,@(cond
		     ((or restp keywords-present-p)
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
		 ,(cond
		    (keywords-present-p
		     `(apply #',impl-name
			     (append 
			      (subseq ,args 0 ,required-len)
			      (defmfun-keywords ',pretty-fname
				  (nthcdr ,required-len ,args)
				',maxima-keywords))))
		    (t
		     `(apply #',impl-name ,args))))))
	   ,(cond
	      (keywords-present-p
	       `(define-compiler-macro ,name (&rest ,rest-name)
		  ,(format nil "Compiler-macro to convert calls to ~S to ~S" name impl-name)
		  (let ((args (append (subseq ,rest-name 0 ,required-len)
				      (defmfun-keywords ',pretty-fname
					  (nthcdr ,required-len ,rest-name)
					',maxima-keywords))))
		    `(,',impl-name ,@args))))
	      (t
	       `(define-compiler-macro ,name (&rest ,rest-name)
		  ,(format nil "Compiler-macro to convert calls to ~S to ~S" name impl-name)
		  `(,',impl-name ,@,rest-name)))))))))

;; Define a Lisp function that should check the number of arguments to
;; a function and print out a nice Maxima error message instead of
;; signaling a Lisp error.  In this case, the function is not
;; explicitly exposed to the user and can just have an impl name of
;; "name-impl".
(defmacro defun-checked (name lambda-list &body body)
  ;; Defun-checked must not be used with functions that are exposed to
  ;; the (Maxima) user.  That is, it can't start with "$".
  (when (char-equal #\$ (char (string name) 0))
    (error "DEFUN-CHECKED functions cannot start with $: ~S~%" name))
  `(defun-checked-form (,name ,(intern (concatenate 'string
						    (string name)
						    "-IMPL")))
		       ,lambda-list ,@body))

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
(defmacro defmfun (name-maybe-prop lambda-list &body body)
  ;; NAME-MAYBE-PROP can be either a symbol or a list.  If a symbol,
  ;; it's just the name of the function to be defined.  If a list, it
  ;; must have the form (name &keyword :properties :deprecated-p)
  ;; where NAME is the name of the function to be defined.  The
  ;; keyword args control what is generated.  The value of :PROPERTIES
  ;; is a list of lists denoting properties that are set for this
  ;; function.  Each element of the list must be of the form (PROPERTY
  ;; VALUE).  The value of :DEPRECATED-P is a symbol (unquoted) naming
  ;; the function that should be used instead of this function because
  ;; this function is deprecated.
  ;;
  ;;   (defmfun ($polarform :properties ((evfun t))) (xx) ...)
  ;;
  ;; is the same as (defmfun $polarform (xx) ...) but adds
  ;; (putprop '$polarform t 'evfun)
  ;;
  ;; For deprecated functions:
  ;;
  ;;   (defmfun ($foo :deprecated-p $bar) () ...)
  ;;
  ;; This will print a message stating that "foo" is deprecated and to
  ;; use "bar" instead.
  (destructuring-bind (name &key properties deprecated-p)
      (if (symbolp name-maybe-prop)
	  (list name-maybe-prop)
	  name-maybe-prop)
    (flet ((add-props ()
             ;; We make sure that the ARG-LIST property is added
             ;; first, so that it will end up last in the list.
             `((putprop ',name ',lambda-list 'arg-list)
	       (defprop ,name t translated)))
	   (func-props ()
	     ;; If any properties were specified for the function,
	     ;; gather them up here into corresponding putprop forms.
	     (mapcar #'(lambda (p)
			 (destructuring-bind (ind val)
			     p
			   `(putprop ',name ',val ',ind)))
		     properties)))

      (let ((impl-name (intern (concatenate 'string
					    (subseq (string name) 1)
					    "-IMPL")))
	    (maclisp-narg-p (and (symbolp lambda-list) (not (null lambda-list)))))
	(cond
          ((or (char/= #\$ (aref (string name) 0))
	       maclisp-narg-p)
           ;; If NAME doesn't start with $, it's an internal function not
           ;; directly exposed to the user.  Basically define the function
           ;; as is, taking care to support the Maclisp narg syntax.
           (cond (maclisp-narg-p
	          ;; Support MacLisp narg syntax:  (defun foo a ...)
	          `(progn
                     (defun ,name (&rest narg-rest-argument
			           &aux (,lambda-list (length narg-rest-argument)))
		       ,@body)
		     ,@(add-props)))
		 (t
	          `(progn
                     (defun ,name ,lambda-list ,@body)
		     ,@(add-props)))))
	  (t
           ;; Function name begins with $, so it's exposed to the user;
           ;; carefully check the number of arguments and print a nice
           ;; message if the number doesn't match the expected number.
           #+nil
           (unless (char= #\$ (aref (string name) 0))
	     (warn "First character of function name must start with $: ~S~%" name))
	   `(progn
	      (defun-checked-form (,name ,impl-name :deprecated-p ,deprecated-p) ,lambda-list
		,@body)
	      ,@(add-props)
	      ,@(func-props)
	      ;; We don't put this putprop in add-props because
	      ;; add-props is for both user and internal functions
	      ;; while the impl-name property is only for user
	      ;; functions.
	      (putprop ',name ',impl-name 'impl-name))))))))

;; Examples:
;; (defmfun $foobar (a b) (list '(mlist) a b))
;; (defmfun $foobar1 (a b &optional c) (list '(mlist) a b c))
;; (defmfun $foobar1a (a b &optional (c 99)) (list '(mlist) a b c))
;; (defmfun $foobar2 (a b &rest c) (list '(mlist) a b (list* '(mlist) c)))
;; (defmfun $foobar3 (a b &optional c &rest d) "foobar3 function" (list '(mlist) a b c (list* '(mlist) d)))
;;
;; (defmfun $foobar4 (a b &key c) (list '(mlist) a b c))
;; (defmfun $foobar5 (a b &key (c 42)) (list '(mlist) a b c))
;; (defmfun $foobar6 (a b &key (c 42) &allow-other-keys) (list '(mlist) a b c))
;;
;; foobar5(1,2) => [1, 2, 42]
;; foobar5(1,2,c=99) => [1, 2, 99]
;; foobar5(1,2,c=99,d=4) => error: unrecognized keyword d
;; foobar6(1,2,c=42,d=99) => [1, 2, 42]
;;
;; This works by accident, kind of:
;; (defmfun $baz (a &aux (b (1+ a))) (list '(mlist) a b))

;; This should produce compile errors
;; (defmfun $zot (a &optional c &key b) (list '(mlist) a b))


;; Defines a simplifying function for Maxima whose name is BASE-NAME.
;; The noun and verb properties are set up appropriately, along with
;; setting the operator property.  The noun form is created from the
;; BASE-NAME by prepending a "%"; the verb form, by prepending "$".
;; The verb function is defined appropriately too.
;;
;; For example, let's say we want to define a Maxima function named
;; foo of two args with a corresponding simplifier to simplify special
;; cases or numerically evaluate it.  Then:
;;
;; (def-simplifier foo (x y)
;;   (cond ((float-numerical-eval-p x y)
;;          (foo-eval x y))
;;         (t
;;          (give-up))))
;;
;; This expands to 
;;         
;; (progn
;;   (defprop %foo simp-%foo operators)
;;   (defprop $foo %foo verb)
;;   (defprop %foo $foo noun)
;;   (defprop $foo %foo alias)
;;   (defprop %foo $foo reversealias)
;;   (defun simp-%foo (form #:unused-5230 %%simpflag)
;;     (declare (ignore #:unused-5230))
;;     (let ((x (simpcheck (nth 1 form) #:z-5229))
;;           (y (simpcheck (nth 2 form) #:z-5229)))
;;       (arg-count-check 2 form)
;;       (macrolet ((give-up ()
;;                    '(eqtest (list '(%foo) x y) form)))
;;         (cond
;;           ((float-numerical-eval-p x y)
;;            (foo-eval x y))
;;           (t
;;            (give-up))))))
;;
;; The body can reference FORM and %%SIMPFLAG.
;;
;; The base name can also be a lambda-list of the form (name &key
;; (simpcheck :default)).  The NAME is the BASE-NAME of the
;; simpiflier.  The keyword arg :SIMPCHECK supports two values:
;; :DEFAULT and :CUSTOM, with :DEFAULT as the default.  :CUSTOM means
;; the generated code does not call SIMPCHECK on the args, as shown
;; above.  It is up to the body to do the necessary work.
;;
;; Note also that the args for the simplifier only supports a fixed
;; set of required arguments.  Not optional or rest arguments are
;; supported.  No checks are made for this.  If you need this, you'll
;; have to write your own simplifier.  Use the above macro expansion
;; to see how to define the appropriate properties for the simplifer.
;;
;; Note carefully that the expansion defines a macro GIVE-UP to
;; handle the default case of the simplifier when we can't do any
;; simplification.  Call this in the default case for the COND.

(defmacro def-simplifier (base-name-and-options lambda-list &body body)
  (destructuring-bind (base-name &key (simpcheck :default))
      (if (symbolp base-name-and-options)
	  (list base-name-and-options)
	  base-name-and-options)
    (let* ((noun-name (intern (concatenate 'string "%" (string base-name))))
	   (verb-name (intern (concatenate 'string "$" (string base-name))))
	   (simp-name (intern (concatenate 'string "SIMP-" (string noun-name))))
	   (form-arg (intern "FORM"))
	   (z-arg (intern "%%SIMPFLAG"))
	   (unused-arg (gensym "UNUSED-"))
	   (arg-forms (ecase simpcheck
			(:custom
			 (loop for arg in lambda-list
			       and count from 1
			       collect (list arg `(nth ,count ,form-arg))))
			(:default
			 (loop for arg in lambda-list
			       and count from 1
			       collect (list arg `(simpcheck (nth ,count ,form-arg) ,z-arg)))))))
      `(progn
	 ;; Set up properties
	 (defprop ,noun-name ,simp-name operators)
	 ;; The verb and alias properties are needed to make things like
	 ;; quad_qags(jacobi_sn(x,.5)...) work.
	 (defprop ,verb-name ,noun-name verb)
	 (defprop ,verb-name ,noun-name alias)
	 ;; The reversealias property is needed by grind to print out
	 ;; the right thing.  Without it, grind(jacobi_sn(x,m)) prints
	 ;; '?%jacobi_sn(x,m)".  Also needed for labels in plots which
	 ;; would show up as %jacobi_sn instead of jacobi_sn.
	 (defprop ,noun-name ,verb-name reversealias)

	 ;; Define the simplifier
	 (defun ,simp-name (,form-arg ,unused-arg ,z-arg)
	   (declare (ignore ,unused-arg)
		    (ignorable ,z-arg))
	   (arg-count-check ,(length lambda-list)
			    ,form-arg)
	   (let ,arg-forms
	     (flet ((give-up ()
		      ;; Should this also return from the function?
		      ;; That would fit in better with giving up.
		      (eqtest (list '(,noun-name) ,@lambda-list) ,form-arg)))
	       ,@body)))))))
