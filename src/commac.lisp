;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

;;(eval-when (compile)
;;  (proclaim '(optimize (safety 0) (speed 3) (space 0))))

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)

    (defmacro defun-prop (f arg &body body)
      (assert (listp f))
      #+gcl (eval-when (eval)
	      (compiler::compiler-def-hook (car f) body))
      `(progn 'compile
	(setf (get ',(car f) ',(second f))
	 #'(lambda ,arg ,@body))))

    )

(defvar *prin1* nil)		  ;a function called instead of prin1.

(eval-when
    #+gcl (load compile eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)

    (defun appears (tree var)
      (cond ((equal tree var)
	     (throw 'appears t))
	    ((atom tree) nil)
	    (t  (appears  (car tree) var)
		(appears (cdr tree)  var)))
      nil)

    (defun appears1 (tree var)
      (cond ((eq tree var)
	     (throw 'appears t))
	    ((atom tree) nil)
	    (t
	     (appears (car tree) var)
	     (appears (cdr tree) var)))
      nil)

    (defun appears-in (tree var)
      "Yields t if var appears in tree"
      (catch 'appears
	(cond ((or (symbolp var)
		   (fixnump var))
	       (appears1 tree var))
	      (t (appears tree var)))))
    )



;;this wants the input type to be eg.  'fixnum  and outputs the same 'fixnum
;;eg (maclisp-typep 5) ==> 'fixnum
;;eg (maclisp-typep 6 'fixnum) ==> t

;;;this is much faster but depends on the %data-type function
;;;Actually the optimizer should eliminate any calls in compiled code
;;;to the two argument ml-typep.  And these should be eliminated anyway
;;;since they were not part of maclisp.  Ultimately we should 
;;;make the optimizer branch to a one arg typep (maclisp-type-of) 
;;;when there is only one argument.


;;in kcl one would use the number code given by type_of(x);

(defvar *primitive-data-type-function* 'type-of)

(defmacro one-of-types (typ &rest objs &aux all)
  "typ is a primitive data type of the machine, and"
  (dolist (v objs
	   (cond ((cdr all)
		  `(memq ,typ ',all))
		 (t `(eql ,typ ',(car all)))))
    (pushnew  (funcall *primitive-data-type-function* (eval v)) all)))

;; This assumes way too much about how typep works, so don't use it.
;; We leave it here for reference to make sure the replacement below
;; does the same thing.
;;#+nil
;;(defun maclisp-typep (x &optional type)
;;  (cond (type
;;	 (lisp:let (( pred (get type 'ml-typep)))
;;        (cond (pred
;;		(funcall pred x))
;;	       (t (typep x type)))))
;;	(t
;;	(lisp:let ((.type. (#. (if (boundp '*primitive-data-type-function*)
;;				   *primitive-data-type-function* 'type-of)
;;				   x)))
;;	  (cond
;;	   ((one-of-types .type.  'hi nil) 'symbol)
;;	   ((one-of-types .type. '(a)) 'list)
;;	   ((one-of-types .type. 3) 'fixnum)
;;	   ((one-of-types .type.  (make-array 3) "abc")
;;	    (cond ((stringp x) 'string) ;;should really be symbol 'ugggh
;;		  #+ti ((hash-table-p x) 'hash-table)
;;		  (t 'array)))
;;	   ((one-of-types .type. (expt 2 50) 1.234 most-positive-single-float
;;			  most-positive-double-float most-positive-long-float )
;;	      (cond 
;;		    ((integerp x) 'bignum)
;;		    ((floatp x) 'flonum )
;;		    (t 'number)))
;;	   ;;note the following is 'random in maclisp
;;	   ((one-of-types .type. #'cons) 'compiled-function)
;;	   #-ti ((one-of-types .type.  (make-hash-table))
;;	    (cond ((hash-table-p x) 'hash-table)
;;		  (t (type-of x))))
;;	   ((arrayp x) 'array)
;;	   ;((one-of-types .type. (make-array '(2 3)))  'array)
;;	   (t (type-of x)))))))

;; A more portable implementation of maclisp-typep.  I (rtoy) think it
;; would probably be better to replace uses of maclisp-typep and/or
;; ml-typep with the corresponding Common Lisp typep or type-of or
;; subtypep, as appropriate.
(defun maclisp-typep (x &optional type)
  (cond (type
	 (cl:let ((pred (get type 'ml-typep)))
	   (cond (pred
		  (funcall pred x))
		 (t (typep x type)))))
	(t
	 (typecase x
	   (cl:cons 'list)
	   (cl:fixnum 'fixnum)
	   (cl:integer 'bignum)
	   (cl:float 'flonum)
	   (cl:number 'number)
	   (cl:array 'array)
	   (cl:hash-table 'hash-table)
	   (t
	    (type-of x))))))

(deff ml-typep #'maclisp-typep)
;;so that (ml-typep a 'list) ==> (zl-listp a)


(defprop :extended-number extended-number-p ml-typep)
(defprop array arrayp ml-typep)
(defprop atom  atom ml-typep)

#+cmu (shadow '(cl:compiled-function-p) (find-package "MAXIMA"))
#+cmu (defun compiled-function-p (x)
	(and (functionp x) (not (symbolp x))
	     (not (eval:interpreted-function-p x))))

(defprop compiled-function compiled-function-p ml-typep)
(defprop extended-number extended-number-p ml-typep)
(defprop fixnum fixnump ml-typep)
(defprop list consp ml-typep)
(defprop number numberp ml-typep)
(defprop string stringp ml-typep)
(defprop symbol  symbolp ml-typep)


(defun maxima-copy-rest (form)
  "copy those things out of the stack in `(($hi array) ,@ inds) which would be bad if inds is rest arg"
  (copy-list form))

;;;note *array takes 'fixnum and 'flonum as its keyword args!!!!
;;need to use our selectq to ensure the type is correct
;;(ARRAY CONUNMRK NIL (f1+ CONNUMBER))

(defvar *maxima-arrays* nil
  "Trying to track down any functional arrays in maxima")

;;only remaining calls are for maclisp-type = nil
(defun *array (name maclisp-type &rest dimlist &aux aarray)
  (cond ((memq maclisp-type '(readtable obarray))
	 (error " bad type ~S" maclisp-type)))
  (pushnew name *maxima-arrays*)	;for tracking down old ones.
  (setq aarray (make-array dimlist
			   ':initial-element
			   (case maclisp-type 
			     (fixnum 0)
			     (flonum 0.0d0)
			     (otherwise nil))))
  (cond ((null name) aarray)
	((symbolp name)
	 (setf (symbol-array name) aarray)
	 name)
	(t (error "~S is illegal first arg for *ARRAY" name))))

(defmacro array (name maclisp-type &rest dimlist)
  `(*array ',name ',maclisp-type ,@dimlist))

;;;    Change maclisp array referencing.
;;;   Idea1: Make changes in the code which will allow the code to still run in maclisp,
;;;yet will allow, with the appropriate macro definitions of array,arraycall, etc,
;;;to put the array into the value-cell.
;;;   Idea2: Make changes in the array referencing of (a dim1 dim2..) to (arraycall nil (symbol-array a) dim1..)
;;;which would then allow expansion into something which is common lisp compatible, for
;;;the day when (a 2 3) no longer is equivalent to (aref (symbol-function a) 2 3).
;;;I.  change (array a typ dim1 dim2..) to expand to (defvar a (make-array (list dim1 dim2 ...) :type typ')
;;;II. change (a dim1 dim2..) to (arraycall nil (symbol-array a) dim1 dim2 ..)
;;;III define 
;;(defmacro symbol-array (ar)
;;    `(symbol-function ,ar))
;;(defmacro arraycall (ignore ar &rest dims)
;;  `(aref ,ar ,@ dims))
;;;IV. change array setting to use (setf (arraycall nil ar dim1.. ) val) 
;;;which will generate the correct setting code on the lispm and will
;;;still work in maclisp.

(defmacro maxima-error (datum &rest args)
  `(cerror "without any special action" ,datum ,@args))

(defmacro safe-value (sym)
  (cond ((symbolp sym)
	 `(cond ((symbolp ',sym)
		 (and (boundp ',sym) ,sym))
	   (t ,sym)))
	(t nil)))

(defmacro show (&rest l)
  (loop for v in l
	 collecting `(format t "~%The value of ~A is ~A" ',v ,v) into tem
	 finally (return `(progn ,@ tem))))

(defmacro defquote  (fn (aa . oth) &body rest &aux help ans) 
  (setq help (intern (format nil "~a-aux" fn)))
  (cond ((eq aa '&rest)
	 (setq ans
	       (list
		`(defmacro ,fn ( &rest ,(car oth) )
		  `(,',help  ',,(car oth)))
		`(defun ,help (,(car oth)) . ,rest))))
	(t (cond ((member '&rest oth)
		  (error "at present &rest may only occur as first item in a defquote argument")))
	   (setq ans
		 (list
		  `(defmacro ,fn (,aa . other  )
		    (setq other (loop for v in other collecting (list 'quote v)))
		    (check-arg other (eql (length other) ,(length oth)) ,(format nil "wrong number of args to ~a" fn))
		    `(,',help  ',,aa   ,@ other))
		  `(defun ,help (,aa ,@ oth) . ,rest)))))
  `(progn 'compile . , ans))


(defquote $mdefvar (&rest l)
  `((defvar) ,@ l))

;;the resulting function will translate to defvar and will behave
;;correctly for the evaluator.

;;(defun gg fexpr (ll)
;;       body)
;;(defquote gg (&rest ll)
;;       body)

;;(DEFQUOTE GG ( &rest C)
;; (list  (car c) (second c) ))
;;the big advantage of using the following over defmspec is that it 
;;seems to translate more easily, since it is a fn.
;;New functions which wanted quoted arguments should be defined using
;;defquote


(defun onep (x) (eql 1 x))

(defun extended-number-p (x)
  (member (type-of x) '(bignum rational float )))

(defvar *scan-string-buffer*  nil)

(defun macsyma-read-string (a-string &aux answer)
  (cond ((not (or (search "$" a-string :test #'char-equal)
		  (search ";" a-string :test #'char-equal)))
	 (vector-push-extend #\$ a-string)))
  (with-input-from-string (stream a-string)
    (setq answer (third (mread stream)))
    answer))

(defvar *sharp-read-buffer*
  (make-array 140 :element-type ' #.(array-element-type "abc")
	      :fill-pointer 0 :adjustable t))

(defun x$-cl-macro-read (stream sub-char arg)
  (declare (ignore arg))
  ($-read-aux sub-char stream))

(defun $-read-aux (arg stream &aux (meval-flag t) (*mread-prompt* ""))
  (declare (special *mread-prompt*)
	   (ignore arg))
  (setf (fill-pointer *sharp-read-buffer*) 0)
  (cond ((eql #\$ (tyipeek t stream))
	 (tyi stream)
	 (setq meval-flag nil)))
  (with-output-to-string
      (st *sharp-read-buffer*) 
    (let (char)
      (loop while (not (eql char #\$))
	     do
	     (setq char (tyi stream))
	     (tyo char st)
	     finally (cond ((not (eql  char #\$))
			    (error "There was no matching $" ))))))
  (cond (meval-flag 
	 (list 'meval* (list 'quote
			     (macsyma-read-string *sharp-read-buffer*))))
	(t (list 'quote (macsyma-read-string *sharp-read-buffer*)))))


(set-dispatch-macro-character  #\#  #\$ 'x$-cl-macro-read)

(defvar *macsyma-readtable*)

(defun find-lisp-readtable-for-macsyma ()
  (cond ((and (boundp '*macsyma-readtable*)
	      (readtablep *macsyma-readtable*))
	 *macsyma-readtable*)
	(t (setq *macsyma-readtable* (copy-readtable nil))
	   (set-dispatch-macro-character
	    #\# #\$ 'x$-cl-macro-read *macsyma-readtable*)
	   *macsyma-readtable*)))

(defun set-readtable-for-macsyma ()
  (setq *readtable* (find-lisp-readtable-for-macsyma)))

;;;to handle the maclisp (defun foo narg .. syntax.)
;;;see below for simpler method, but we have to redefine arg etc.
;;need to 
;;  I. convert to (defun foo (&rest narg-rest-argument (&aux (narg (length narg-rest-argument)))
;;  II. replace (arg 1) by (nth-arg 1)  and (listify i)  by  (narg-listify i) using the following definitions.
;;probably better not to shadow the listify etc. since someone might try to compile some maclisp code
;;and we should not break that.
;;REPLACEMENTS:
;;'(("arg" . "narg-arg")
;;  ("listify" . "narg-listify")
;;  ("setarg" . "narg-setarg"))

;;new macros:

(defmacro narg-arg (x)
  `(nth (1- ,x) narg-rest-argument))

(defun narg-listify1 (x list)
  (declare (fixnum x))
  (if (minusp x)
      (last list (abs x))
      (firstn x list)))

(defmacro narg-listify (x)
  `(narg-listify1 ,x narg-rest-argument))

(defmacro narg-setarg (i val)
  `(setf (narg-arg ,i) ,val))

;;test of above
;;(defun foo (&rest narg-rest-argument &aux (narg (length narg-rest-argument)))
;;  (show  (narg-listify 3))
;;  (show  (narg-listify -3))
;;  (show  (narg-arg 2))
;;  (narg-setarg 2 8)
;;  (show (narg-listify 3)))

(defvar *reset-var* t)

(defvar *variable-initial-values* (make-hash-table)
  "Hash table containing all Maxima defmvar variables and their initial
values")

(defmacro defmvar (var &rest val-and-doc)
  "If *reset-var* is true then loading or eval'ing will reset value, otherwise like defvar"
  (cond ((> (length val-and-doc) 2)
	 (setq val-and-doc (list (car val-and-doc) (second val-and-doc)))))
  `(progn
    (unless (gethash ',var *variable-initial-values*)
      (setf (gethash ',var *variable-initial-values*)
	    ,(first val-and-doc)))
    (defvar ,var ,@val-and-doc)
    #+debug
    (maybe-reset ',var ',(if val-and-doc (list (car val-and-doc))))))

#+debug
(defun maybe-reset (var val-and-doc &aux val)
  (cond (*reset-var*
	 (cond ((not(eq 'nil val-and-doc))
		(cond ((not (equal (setq val (eval (car val-and-doc)))
				   (symbol-value var)))
		       (format t "~%Replacing value of ~A" var)
		       (set var val))))
	       (t (cond ((boundp var)
			 (format t "~%Removing value of ~A" var)
			 (makunbound var))))))))

(defun $mkey (variable)
  "($mkey '$demo)==>:demo"
  (intern (string-left-trim "$" (string variable)) 'keyword))

;;Problems with quote char / and #/ don't want to break editor or compiler.
;;Solution:
;; I replace all #/ by #\
;;II do the replacements indicated below under multiple query replace from buffer
;;III if a macro requires the actual string eg "//" use #.forward-slash-string for example.
;;have done this to suprv1,system,displa,nparse,displm.

;;;some solutions for the backslash problem: in general try to avoid
;;using any quote character.  Use #\a for normal type characters.
;;to run in common (of course the numbers here need changing if not standard ascii):

(defvar double-quote-char (code-char 34.)) ;; #\")
(defvar semi-colon-char (code-char 59.)) ;; #\;)
(defvar back-slash-char (code-char 92.)) ;; #\\)
(defvar forward-slash-char (code-char 47.)) ;; #\/)
(defvar left-parentheses-char (code-char 40.)) ;(
(defvar right-parentheses-char (code-char 41.)) ;)
(defvar period-char (code-char 46.))	;.
(defvar vertical-stroke-char (code-char 124.)) ;|
(defvar $forward-slash-symbol #-cl '$// #+cl '$/ )
;;(defvar $colon-char (intern "$:"))
;;(defvar $comma-char (intern "$,"))


(defvar forward-slash-string (string forward-slash-char))

(defmacro arg (x)
  `(narg1 ,x narg-rest-argument))

(defun narg1 (x l &aux tem)
  (cond ((null x) (length l))
	(t (setq tem (nthcdr (f1- x) l))
	   (cond ((null tem) (error "arg ~A beyond range ~A " x (length l)))
		 (t (car tem))))))

(defmacro listify (x)
  `(listify1 ,x narg-rest-argument))

(defmacro setarg (i val)
  `(setarg1 ,i ,val narg-rest-argument))

(defun setarg1 (i val l)
  (setf (nth (f1- i)l) val) val)

(defun listify1 (n narg-rest-argument)
  (cond ((minusp n) (copy-list (last narg-rest-argument (f- n))) )
	((zerop n) nil)
	(t (firstn n narg-rest-argument))))

(defmacro defmfun (function &body  rest &aux .n.)
  (cond ((and (car rest) (symbolp (car rest)))
	 ;;old maclisp narg syntax
	 (setq .n. (car rest))
	 (setf (car rest)
	       `(&rest narg-rest-argument &aux
		 (, .n. (length narg-rest-argument))
		 ;;(*lexpr-arglist*  narg-rest-argument) 
		 ))))
  `(progn #-cl 'compile
    #+lispm (si::record-source-file-name ',function 'defmfun)
    (defun ,function . ,rest)))

;;sample usage
;;(defmfun foo a (show a )(show (listify a)) (show (arg 3)))


(defun exploden (symb)
  (let* (#+(and gcl (not gmp)) (big-chunk-size 120)
	   #+(and gcl (not gmp)) (tentochunksize (expt 10 big-chunk-size))
	   string)
    (cond ((symbolp symb)
	   (setq string (print-invert-case symb)))
	  ((floatp symb)
	   (let ((a (abs symb)))
	     (cond ((or (eql a 0.0)
			(and (>= a .001)
			     (<= a 10000000.0)))
		    (setq string (format nil "~vf" (+ 1 $fpprec) symb)))
		   (t (setq string (format nil "~ve" (+ 4 $fpprec) symb)))))
	   (setq string (string-left-trim " " string)))
	  #+(and gcl (not gmp))
	  ((bignump symb)
	   (let* ((big symb)
		  ans rem tem
		  (chunks
		   (loop 
		    do (multiple-value-setq (big rem)
			 (floor big tentochunksize))
		    collect rem 
		    while (not (eql 0 big)))))
	     (setq chunks (nreverse chunks))
	     (setq ans (coerce (format nil "~d" (car chunks)) 'list))
	     (loop for v in (cdr chunks)
		    do (setq tem (coerce (format nil "~d" v) 'list))
		    (loop for i below (- big-chunk-size (length tem))
			   do (setq tem (cons #\0 tem)))
		    (setq ans (nconc ans tem)))
	     (return-from exploden ans)))
	  (t (setq string (format nil "~A" symb))))
    (assert (stringp string))
    (coerce string 'list)))

(defun explodec (symb &aux tem sstring)
  (setq sstring (print-invert-case symb))
					;(setq sstring (coerce symb 'string))
  (loop for v on (setq tem (coerce sstring 'list))
	 do (setf (car v)(intern (string (car v)))))
  tem)

(defvar *string-for-implode*
  (make-array 20 :fill-pointer 0 :adjustable t
	      :element-type ' #.(array-element-type "ab")))

(defun implode (lis) (implode1 lis nil))


(defun maybe-invert-string-case (string)
  ;; If STRING is all the same case, invert the case.  Otherwise, do
  ;; nothing.
  (flet ((alpha-upper-case-p (s)
	   (not (some #'lower-case-p s)))
	 (alpha-lower-case-p (s)
	   (not (some #'upper-case-p s))))
    ;; Don't explicitly add a package here.  It seems maxima sets
    ;; *package* as needed.
    (cond ((alpha-upper-case-p string)
	   (string-downcase string))
	  ((alpha-lower-case-p string)
	   (string-upcase string))
	  (t
	   string))))

(defun intern-invert-case (string)
  ;; Like read-from-string with readtable-case :invert
  ;;
  ;; Not explicit package for INTERN.  It seems maxima sets *package*
  ;; as needed.
  (intern (maybe-invert-string-case string)))


#-gcl
(let ((local-table (copy-readtable nil)))
  (setf (readtable-case local-table) :invert)
  (defun print-invert-case (sym)
    (let ((*readtable* local-table)
	  (*print-case* :upcase))
      (princ-to-string sym))))

#+gcl
(defun print-invert-case (sym)
  (let* ((str (princ-to-string sym))
	 (have-upper nil)
	 (have-lower nil)
	 (converted-str 
	  (map 'string (lambda (c)
			 (cond ((upper-case-p c) 
				(setf have-upper t)
				(char-downcase c))
			       ((lower-case-p c) 
				(setf have-lower t)
				(char-upcase c))
			       (t c)))
	       str)))
    (if (and have-upper have-lower)
	str
	converted-str)))
				      
(defun implode1 (lis upcase &aux (ar *string-for-implode*) (leng 0))
  (declare (type string ar) (fixnum leng))
  (or (> (array-total-size ar) (setq leng (length lis)))
      (adjust-array ar (+ leng 20)))
  (setf (fill-pointer ar) leng)
  (loop for v in lis
	 for i below leng
	 do
	 (cond ((typep v 'character))
	       ((symbolp v) (setq v (aref (symbol-name v) 0)))
	       ((numberp v) (setq v (code-char v))))
	 (setf (aref ar i) v))
  (intern-invert-case ar))

(defun bothcase-implode (lis  &aux tem )
  (implode1 lis nil))

(defun list-string (strin &aux tem)
  (setq tem (make-list (length (the string  strin))))
  (loop for v on tem
	for i from 0
	do (setf (car v) (aref strin i)))
  tem)

(defun explode (symb &aux tem sstring)
  ;; Note:  symb can also be a number, not just a symbol.
  (setq sstring (format nil "~S" symb))
  (loop for v on (setq tem (list-string sstring))
	 do (setf (car v)(intern (string (car v)))))
  tem)

(defun getcharn (symb i)
  (let ((strin (string symb)))
    (if (<= 1 i (length strin))
	(char strin (1- i))
	(maxima-error "out of bounds"))))

(defun getchar (symb i)
  (let ((str (string symb)))
    (if (<= 1 i (length str))
	(intern (string (char str (1- i))))
	nil)))


(defun getchars (symb start end)
  (let ((strin (string symb)))
    (if (<= 1 start (length strin))
	(intern (string (subseq strin (1- start) (1- end))))
	nil)))

(defun ascii (n)
  (intern (string n)))

(defun maknam (lis)
  (loop for v in lis
	 when (symbolp v)
	 collecting (getcharn v 1) into tem
	 else
	 when (characterp v)
	 collecting v into tem
	 else do (maxima-error "bad entry")
	 finally 
	 (return (make-symbol (maybe-invert-string-case (coerce tem 'string))))))

;;for those window labels etc. that are wrong type.

(defun flatc (sym)
  (length (explodec sym)))

(defun flatsize (sym &aux (*print-circle* t))
  (length (exploden sym)))

(defmacro safe-zerop (x)
  (cond((symbolp x)
	`(and (numberp ,x) (zerop ,x)))
       (t `(let ((.x. ,x))
	    (and (numberp .x.) (zerop .x.))))))

(defmacro signp (sym x)
  (cond ((atom x)
	 (let ((test
		(case sym
		  (e `(zerop ,x))
		  (l `(< ,x 0))    
		  (le `(<= ,x 0))
		  (g `(> ,x 0))
		  (ge `(>= ,x 0))
		  (n `(not (zerop ,x))))))
	   `(and (numberp ,x) ,test)))
	(t `(let ((.x. ,x))
	     (signp ,sym .x.)))))


(defmacro comment (&rest a) a ''comment)

(defun tyo (char &optional(stream *standard-output*))
  (write-char char stream))

(defun tyi (&optional (stream *standard-input*) eof-option)
  (if eof-option				
      (read-char stream nil eof-option)
      (read-char stream nil nil)))


(defun tyipeek (&optional peek-type &rest read-args)
  (if read-args
      (peek-char peek-type (car read-args))
      (peek-char peek-type)))

;;I don't think these are terribly useful so why use them.

(progn 'compile
       (defmacro *expr (&rest x) x nil)
       (defmacro *lexpr (&rest x) x nil)
       (defmacro *fexpr (&rest x) x nil))

(defmacro local-declare (dcls &body body)
  dcls					;ignore
  `(progn
    ;;	    (declare ,@ dcls)
    ,@ body))


(defmacro arraycall (ign array &rest dims) ign
	  `(aref ,array . ,dims))

(defmacro copy-rest-arg (arg)
  arg)

(defvar ^w nil)
(defvar ^r nil)


;;essentially no common lisp support for cursor pos:
;;should be adapted for a particular implementation.
;;the #+nocp (no cursorpos) flag could then be removed from *features*
;;and perhaps even smart-tty set to t.

(defun cursorpos (&rest args &aux q1 q2 )
  (cond ((null args) (error "cursorpos doesn't know position"))
	(t
	 (setq q1 (first args))
	 (setq q2 (second args))
	 (cond ((or (null q1) (and (fixnump q1 ) (null q2)))
		(error "can't set cursor pos"))
	       ((and (fixnump q1) (fixnump q2))
		(error "can't set cursor pos") t)
	       (t (cond ((symbolp q1) (setf q1 (aref (symbol-name q1) 0)) t)
			(t (error "bad first arg to cursorpos")))
		  (case (char-downcase q1)
		    (#\a  (fresh-line) t)
		    (#\b (error "cant backspace") t)
		    (#\c (error "cant clear window") t)
		    (#\e (error "can't clear rest of window") t )
		    (#\f (princ " ") t )
		    (#\k (error "can't clear-char") t)
		    (#\l (error "can't clear end of line") t)
		    (#\z (error "can't home-down") t)
		    (#\x (princ " ") t)
		    (#\t (error "can't home up") t)
		    (otherwise (error "unknown arg for this simple cursorpos"))))))))


(defun function-array-p (sym)
  (arrayp (symbol-array sym)))

;; no generic way of knowing args numbers..
(defmacro margchk (fn args) fn args ())

(defun $timedate ()
  (let ((day-names #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
    (multiple-value-bind 
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (namestring (format nil "~2,'0d:~2,'0d:~2,'0d ~a, ~d/~2,'0d/~d (GMT~@d)~%"

			  hour minute second (aref day-names day-of-week)
			  month date year (- tz))))))


;;Some systems make everything functionp including macros:
(defun functionp (x)
  (cond ((symbolp x)
	 (and (not (macro-function x))
	      (fboundp x) t))
	 ((cl:functionp x))))

;; These symbols are shadowed because we use them also as special
;; variables.
(deff break #'cl:break)
(deff gcd #'cl:gcd)

#+(and sbcl sb-package-locks)
(defun makunbound (sym)
  (sb-ext:without-package-locks
      (cl:makunbound sym)))
