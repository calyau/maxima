(defparameter *maxima-direct-ir-map* (make-hash-table))
(setf (gethash 'mtimes *maxima-direct-ir-map*) '(op *))
(setf (gethash 'mplus *maxima-direct-ir-map*) '(op +))
(setf (gethash 'mexpt *maxima-direct-ir-map*) '(funcall (symbol "pow")))
(setf (gethash 'mfactorial *maxima-direct-ir-map*) '(funcall (symbol "math.factorial")))
(setf (gethash 'rat *maxima-direct-ir-map*) '(op /))
(setf (gethash 'msetq *maxima-direct-ir-map*) '(op =))
(setf (gethash 'mlist *maxima-direct-ir-map*) '(struct list))
(setf (gethash 'mand *maxima-direct-ir-map*) '(boolop and))
(setf (gethash 'mor *maxima-direct-ir-map*) '(boolop or))
(setf (gethash 'mnot *maxima-direct-ir-map*) '(funcall (symbol "not")))
(setf (gethash 'mgreaterp *maxima-direct-ir-map*) '(comp-op gr))
(setf (gethash 'mequal *maxima-direct-ir-map*) '(comp-op eq))
(setf (gethash 'mnotequal *maxima-direct-ir-map*) '(comp-op neq))
(setf (gethash 'mlessp *maxima-direct-ir-map*) '(comp-op le))
(setf (gethash 'mgeqp *maxima-direct-ir-map*) '(comp-op ge))
(setf (gethash 'mleqp *maxima-direct-ir-map*) '(comp-op leq))

(defparameter *maxima-special-ir-map* (make-hash-table))
(setf (gethash 'mdefine *maxima-special-ir-map*) 'func-def-to-ir)
(setf (gethash '%array *maxima-special-ir-map*) 'array-def-to-ir)
(setf (gethash 'mprog *maxima-special-ir-map*) 'mprog-to-ir)
(setf (gethash 'mprogn *maxima-special-ir-map*) 'mprogn-to-ir)
(setf (gethash 'mcond *maxima-special-ir-map*) 'mcond-to-ir)

(defparameter *ir-forms-to-append* '())

(defun mcond-auxiliary (forms)
  `(
    ,(maxima-to-ir (car forms))
     ,(maxima-to-ir (cadr forms))
     ,(cond ((eq (caddr forms) 't) (maxima-to-ir (cadddr forms)))
	    (t `(conditional ,@(mcond-conditional (cdddr forms)))))
     ))

(defun mcond-to-ir (form)
  ;; (conditional <condition> <res1> <else-res>)
  `(conditional ,@(mcond-auxiliary (cdr form))))

(defun mprog-variable-names-list (form)
  (cond ((and (consp form) (eq 'msetq (caar form))) (maxima-to-ir (cadr form)))
	(t (maxima-to-ir form))))

(defun mprog-arg-list (form)
  (cond ((and (consp form) (eq 'msetq (caar form))) (maxima-to-ir (car (last form))))
	(t `(symbol "None"))))

;;; It creates a function for the statements.
;;; A problem with this approach is that Maxima is dynamically
;;; binding, whereas Python is not. For accessing values from the global
;;; context, this will work, however, for assignment operations,
;;; this method will not. Another alternative that can be considered
;;; is to create a class to house all the bindings.
(defun mprog-to-ir (form)
  (cond ((not (null (cdr form)))
	 (cond ((and (consp (cadr form)) (eq 'mlist (caaadr form)))
		;; Variable binding
		(let ((func_name (maxima-to-ir (gensym "$func"))))
		  (setf *ir-forms-to-append*
			(append *ir-forms-to-append*
				`((func-def ,func_name ,(mapcar 'mprog-variable-names-list (cdadr form)) (body ,@(mapcar 'maxima-to-ir (cddr form)))))))
		  `(funcall ,func_name ,@(mapcar 'mprog-arg-list (cdadr form)))))
	       ;; No variable binding required
	       (t
		(let ((func_name (maxima-to-ir (gensym "$func"))))
		  (setf *ir-forms-to-append* (append *ir-forms-to-append* `((func-def ,func_name () (body ,@(mapcar 'maxima-to-ir (cdr form)))))))
		  `(funcall ,func_name))
		)))))

(defun mprogn-to-ir (form)
  (let ((func_name (maxima-to-ir (gensym "$func"))))
		  (setf *ir-forms-to-append* (append *ir-forms-to-append* `((func-def ,func_name () (body ,@(mapcar 'maxima-to-ir (cdr form)))))))
		  `(funcall ,func_name)))

;;; Recursively generates IR for a multi-dimensional array and fills all cells with Null value
(defun array-gen-ir (dimensions)
  (cond ((null dimensions) '(symbol "None"))
	(t `(op * (struct list ,(array-gen-ir (cdr dimensions))) ,(maxima-to-ir (car dimensions))))))

;;; Helper function for array-def-to-ir which generates the IR for array definition 
(defun auxillary-array-to-ir (symbol dimensions)
  `(op = ,symbol ,(array-gen-ir dimensions)))

;;; Function to generate IR for array definition using different methods, by using the auxillary function
(defun array-def-to-ir (form)
  (cond ((consp (cadr form))
	 (append '(block list) (loop for symb in (cdadr form)
				  collect (auxillary-array-to-ir (maxima-to-ir symb) (cddr form)))))
	((not (numberp (caddr form)))
	 (auxillary-array-to-ir (maxima-to-ir (cadr form)) (cdddr form)))
	(t
	 (auxillary-array-to-ir (maxima-to-ir (cadr form)) (cddr form)))))

;;; Function to convert reference to array elements to IR
;;; TODO : However, for arrays that are undefined, it needs to be assigned to a hashed array(dictionary)
(defun array-ref-to-ir (symbol indices)
  (cond ((null indices) (maxima-to-ir symbol)) 
	(t `(element-array ,(array-ref-to-ir symbol (butlast indices)) ,(maxima-to-ir (car (last indices)))))))

;;; Convert Function args to corresponding IR
;;; Convert the optional list argument into corresponding *args form in python
(defun func-arg-to-ir (form)
  (cond ((consp form) (cond ((eq (caar form) 'mlist) `(symbol ,(concatenate 'string "*" (subseq (symbol-name (cadr form)) 1))))))
	(t (maxima-to-ir form))))

;;; Generates IR for function definition
(defun func-def-to-ir (form)
  `(func-def ,(maxima-to-ir (caaadr form)) ,(mapcar #'func-arg-to-ir (cdadr form)) (body ,(maxima-to-ir (caddr form)))))

;;; Generates IR for atomic forms
(defun atom-to-ir (form)
  (cond
    ((eq form 'nil) `(symbol "None"))
    ((eq form 't) `(symbol "True"))
    ((stringp form) `(string ,form))
    ((not (symbolp form)) form)
    ((eq form '$%i) '(num 0 1)) ; iota complex number
    ((eq form '$%pi) '(num pi 0)) ; Pi
    ((eq form '$%e) '(num e 0)) ; Euler's Constant
    (t (list 'symbol (subseq (symbol-name form) 1)))
    ))

;;; Generates IR for non-atomic forms
(defun cons-to-ir (form)
  (cond
    ((atom (caar form))
     (progn
       (setf type (gethash (caar form) *maxima-direct-ir-map*))
       (cond
	 ;;; If the form is present in *maxima-direct-ir-map*
	 (type
	  (append type (mapcar #'maxima-to-ir (cdr form))))
	 ;;; If the form is to be transformed in a specific way
	 ((setf type (gethash (caar form) *maxima-special-ir-map*))
	  (funcall type form))
	 ((member 'array (car form))
	  (array-ref-to-ir (caar form) (cdr form)))
	 (t
	  (append `(funcall ,(maxima-to-ir (caar form)))
		  (mapcar #'maxima-to-ir (cdr form))))
	 )))))

(defun maxima-to-ir (form &optional (is_stmt nil))
  (let
      ((ir (cond
	     ((atom form) (atom-to-ir form))
	     ((and (consp form) (consp (car form))) (cons-to-ir form))
	     (t (cons 'no-convert form))
	     )))
    (cond (is_stmt (append '(body) *ir-forms-to-append* `(,(maxima-to-ir form))))
	  (t ir)))
	   )

;;; Generates IR for a given Maxima expression
(defun ir-to-python (form)
  (print form))

;;; Driver function for the translator, calls the maxima-to-ir and then ir-to-python
(defun $transpile (form)
  (setf *ir-forms-to-append* '())
  (ir-to-python (maxima-to-ir form t)))

;;;Adapted from http://cybertiggyr.com/fmt/fmt.pdf Creates a comma separated list string.
(defun comma-list (lst) (format nil "~{~A~#[~:;, ~]~}" lst))

;;; Adapted from above. Creates a + separated list.x
;;; The 2 forms can be merged into a Macro.
(defun sum-list (lst) (format nil "~{~A~#[~:; + ~]~}" lst))

;;; Function to display the internal Maxima form
(defun $show_form (form)
  (print form))
