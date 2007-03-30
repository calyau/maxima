;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)

    (defmacro defun-prop (f arg &body body)
      (assert (listp f))
      #+gcl (eval-when (eval)
	      (compiler::compiler-def-hook (car f) body))
      `(progn 'compile
	(setf (get ',(car f) ',(second f))
	 #'(lambda ,arg ,@body)))))

(defvar *prin1* nil)		  ;a function called instead of prin1.

;; Should we give this a different name?
(defvar *fortran-print* nil
  "Tells EXPLODEN we are printing numbers for Fortran so include the exponent marker.")

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
    (if (or (symbolp var) (fixnump var))
	(appears1 tree var)
	(appears tree var))))

(defmacro one-of-types (typ &rest objs &aux all)
  "typ is a primitive data type of the machine, and"
  (dolist (v objs (if (cdr all)
		      `(member ,typ ',all)
		      `(eql ,typ ',(car all))))
    (pushnew (type-of (eval v)) all)))

;; A more portable implementation of ml-typep.  I (rtoy) think it
;; would probably be better to replace uses of
;; ml-typep with the corresponding Common Lisp typep or type-of or
;; subtypep, as appropriate.
(defun ml-typep (x &optional type)
  (cond (type
	 (cl:let ((pred (get type 'ml-typep)))
	   (if pred
	       (funcall pred x)
	       (typep x type))))
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

(defprop :extended-number extended-number-p ml-typep)
(defprop array arrayp ml-typep)
(defprop atom  atom ml-typep)

#+(or cmu scl)
(shadow '(cl:compiled-function-p) (find-package :maxima))
#+(or cmu scl)
(defun compiled-function-p (x)
  (and (functionp x) (not (symbolp x))
       (not (eval:interpreted-function-p x))))

(defprop compiled-function compiled-function-p ml-typep)
(defprop extended-number extended-number-p ml-typep)
(defprop fixnum fixnump ml-typep)
(defprop list consp ml-typep)
(defprop number numberp ml-typep)
(defprop string stringp ml-typep)
(defprop symbol  symbolp ml-typep)


(defvar *maxima-arrays* nil
  "Trying to track down any functional arrays in maxima")

;;only remaining calls are for maclisp-type = nil
(defun *array (name maclisp-type &rest dimlist &aux aarray)
  (cond ((member maclisp-type '(readtable obarray) :test #'eq)
	 (error " bad type ~S" maclisp-type)))
  (pushnew name *maxima-arrays*)	;for tracking down old ones.
  (setq aarray (make-array dimlist :initial-element (case maclisp-type
						      (fixnum 0)
						      (flonum 0.0d0)
						      (otherwise nil))))
  (cond ((null name) aarray)
	((symbolp name)
	 (setf (symbol-array name) aarray)
	 name)
	(t (error "~S is illegal first arg for *array" name))))

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
  (setq help (intern (format nil "~a-~a" fn '#:aux)))
  (cond ((eq aa '&rest)
	 (setq ans
	       (list
		`(defmacro ,fn (&rest ,(car oth))
		  `(,',help  ',,(car oth)))
		`(defun ,help (,(car oth)) ,@rest))))
	(t (when (member '&rest oth)
	     (error "at present &rest may only occur as first item in a defquote argument"))
	   (setq ans
		 (list
		  `(defmacro ,fn (,aa . other)
		    (setq other (loop for v in other collecting (list 'quote v)))
		    (check-arg other (eql (length other) ,(length oth))
			       ,(format nil "wrong number of args to ~a" fn))
		    `(,',help ',,aa ,@ other))
		  `(defun ,help (,aa ,@ oth) ,@rest)))))
  `(progn ,@ans))


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
  (make-array 140 :element-type ' #.(array-element-type "a") :fill-pointer 0 :adjustable t))

(defun x$-cl-macro-read (stream sub-char arg)
  (declare (ignore arg))
  ($-read-aux sub-char stream))

(defun $-read-aux (arg stream &aux (meval-flag t) (*mread-prompt* ""))
  (declare (special *mread-prompt*)
	   (ignore arg))
  (setf (fill-pointer *sharp-read-buffer*) 0)
  (cond ((eql #\$ (peek-char t stream))
	 (tyi stream)
	 (setq meval-flag nil)))
  (with-output-to-string (st *sharp-read-buffer*)
    (let (char)
      (loop while (not (eql char #\$))
	     do
	     (setq char (tyi stream))
	     (write-char char st))))
  (if meval-flag
      (list 'meval* (list 'quote (macsyma-read-string *sharp-read-buffer*)))
      (list 'quote (macsyma-read-string *sharp-read-buffer*))))

(set-dispatch-macro-character #\# #\$ #'x$-cl-macro-read)

(defvar *macsyma-readtable*)

(defun find-lisp-readtable-for-macsyma ()
  (cond ((and (boundp '*macsyma-readtable*)
	      (readtablep *macsyma-readtable*))
	 *macsyma-readtable*)
	(t (setq *macsyma-readtable* (copy-readtable nil))
	   (set-dispatch-macro-character #\# #\$ 'x$-cl-macro-read *macsyma-readtable*)
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
      (subseq list 0 x)))

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
  (setf (nth (1- i)l) val) val)

(defun listify1 (n narg-rest-argument)
  (cond ((minusp n) (copy-list (last narg-rest-argument (- n))) )
	((zerop n) nil)
	(t (subseq narg-rest-argument 0 n))))

(defmacro defmfun (function &body  rest &aux .n.)
  (cond ((and (car rest) (symbolp (car rest)))
	 ;;old maclisp narg syntax
	 (setq .n. (car rest))
	 (setf (car rest)
	       `(&rest narg-rest-argument &aux (, .n. (length narg-rest-argument))))))
  `(progn
    ;; I (rtoy) think we can consider all defmfun's as translated functions.
    (defprop ,function t translated)
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
	   (let
	 ((a (abs symb))
	  (effective-printprec (if (or (= $fpprintprec 0) (> $fpprintprec 16)) 16 $fpprintprec)))
	     ;; When printing out something for Fortran, we want to be
	     ;; sure to print the exponent marker so that Fortran
	     ;; knows what kind of number it is.  It turns out that
	     ;; Fortran's exponent markers are the same as Lisp's so
	     ;; we just need to make sure the exponent marker is
	     ;; printed.
	     ;;
	     ;; Also, for normal output, we basically want to use
	     ;; prin1, but we can't because we want fpprintprec to control
	     ;; how many digits are printed.  So we have to check for
	     ;; the size of the number and use ~e or ~f appropriately.
	     (if *fortran-print*
		 (setq string (format nil "~e" symb))
		 (multiple-value-bind (form width)
		     (cond ((or (zerop a)
				(<= 1 a 1d7))
			    (values "~vf" (+ 1 effective-printprec)))
			   ((<= 0.001d0 a 1)
			    (values "~vf" (+ effective-printprec
					     (cond ((< a 0.01d0)
						    3)
						   ((< a 0.1d0)
						    2)
						   (t 1)))))
			   (t
			    (values "~ve" (+ 5 effective-printprec))))
		   (setq string (format nil form width symb))))
	     (setq string (string-trim " " string))))
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

(defun explodec (symb &aux tem)
  (loop for v on (setq tem (coerce (print-invert-case symb) 'list))
	 do (setf (car v) (intern (string (car v)))))
  tem)

(defvar *string-for-implode*
  (make-array 20 :fill-pointer 0 :adjustable t :element-type ' #.(array-element-type "a")))

;;; If the 'string is all the same case, invert the case.  Otherwise,
;;; do nothing.
#-(or scl allegro)
(defun maybe-invert-string-case (string)
  (let ((all-upper t)
	(all-lower t)
	(length (length string)))
    (dotimes (i length)
      (let ((ch (char string i)))
	(when (both-case-p ch)
	  (if (upper-case-p ch)
	      (setq all-lower nil)
	      (setq all-upper nil)))))
    (cond (all-upper
	   (string-downcase string))
	  (all-lower
	   (string-upcase string))
	  (t
	   string))))

#+(or scl allegro)
(defun maybe-invert-string-case (string)
  (cond (#+scl (eq ext:*case-mode* :lower)
	 #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
	 string)
	(t
	 (let ((all-upper t)
	       (all-lower t)
	       (length (length string)))
	   (dotimes (i length)
	     (let ((ch (aref string i)))
	       (when (both-case-p ch)
		 (if (upper-case-p ch)
		     (setq all-lower nil)
		     (setq all-upper nil)))))
	   (cond (all-upper
		  (string-downcase string))
		 (all-lower
		  (string-upcase string))
		 (t
		  string))))))

(defun intern-invert-case (string)
  ;; Like read-from-string with readtable-case :invert
  ;;
  ;; Not explicit package for INTERN.  It seems maxima sets *package*
  ;; as needed.
  (intern (maybe-invert-string-case string)))


#-(or gcl scl allegro)
(let ((local-table (copy-readtable nil)))
  (setf (readtable-case local-table) :invert)
  (defun print-invert-case (sym)
    (let ((*readtable* local-table)
	  (*print-case* :upcase))
      (princ-to-string sym))))

#+(or scl allegro)
(let ((local-table (copy-readtable nil)))
  (unless #+scl (eq ext:*case-mode* :lower)
	  #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
    (setf (readtable-case local-table) :invert))
  (defun print-invert-case (sym)
    (cond (#+scl (eq ext:*case-mode* :lower)
	   #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
	   (let ((*readtable* local-table)
		 (*print-case* :downcase))
	     (princ-to-string sym)))
	  (t
	   (let ((*readtable* local-table)
		 (*print-case* :upcase))
	     (princ-to-string sym))))))

#+gcl
(defun print-invert-case (sym)
  (cond ((symbolp sym)
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
	(t (princ-to-string sym))))

(defun implode (lis &aux (ar *string-for-implode*) (leng 0))
  (declare (type string ar) (fixnum leng))
  (or (> (array-total-size ar) (setq leng (length lis)))
      (adjust-array ar (+ leng 20)))
  (setf (fill-pointer ar) leng)
  (loop for v in lis
	 for i below leng
	 do
	 (cond ((typep v 'character))
	       ((symbolp v) (setq v (char (symbol-name v) 0)))
	       ((numberp v) (setq v (code-char v))))
	 (setf (aref ar i) v))
  (intern-invert-case ar))

(defun explode (symb &aux tem)
  ;; Note:  symb can also be a number, not just a symbol.
  (loop for v on (setq tem (coerce (format nil "~S" symb) 'list))
	 do (setf (car v) (intern (string (car v)))))
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

(defmacro make-mstring (string)
  "Make a Maxima string.  The case is inverted for standard CL, and is not
  changed for lower-case case-sensitive CL variants."
  #-(or scl allegro)
  `',(intern (print-invert-case (make-symbol (concatenate 'string "&" string))))
  #+(or scl allegro)
  `',(cond (#+scl (eq ext:*case-mode* :lower)
	    #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
	    (intern (concatenate 'string "&" string)))
	   (t
	    (intern (print-invert-case (make-symbol (concatenate 'string "&" string)))))))

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

(defvar *prompt-on-read-hang* nil)
(defvar *read-hang-prompt* "")

(defun tyi-raw (&optional (stream *standard-input*) eof-option)
  (let ((ch (read-char-no-hang stream nil eof-option)))
    (if ch
	ch
	(progn
	  (when (and *prompt-on-read-hang* *read-hang-prompt*)
	    (princ *read-hang-prompt*)
	    (force-output *standard-output*))
	  (read-char stream nil eof-option)))))

(defun tyi (&optional (stream *standard-input*) eof-option)
  (let ((ch (tyi-raw stream eof-option)))
    (if (eq ch eof-option)
      ch
      (backslash-check ch stream eof-option))))

; The sequences of characters
; <anything-except-backslash>
;   (<backslash> <newline> | <backslash> <return> | <backslash> <return> <newline>)+
;   <anything>
; are reduced to <anything-except-backslash> <anything> .
; Note that this has no effect on <backslash> <anything-but-newline-or-return> .

(let ((previous-tyi #\a))
  (defun backslash-check (ch stream eof-option)
    (if (eq previous-tyi #\\ )
      (progn (setq previous-tyi #\a) ch)
      (setq previous-tyi
	(if (eq ch #\\ )
	  (let ((next-char (peek-char nil stream nil eof-option)))
	    (if (or (eq next-char #\newline) (eq next-char #\return))
	      (eat-continuations ch stream eof-option)
	      ch))
	  ch))))
  ; We have just read <backslash> and we know the next character is <newline> or <return>.
  ; Eat line continuations until we come to something which doesn't match, or we reach eof.
  (defun eat-continuations (ch stream eof-option)
    (setq ch (tyi-raw stream eof-option))
    (do () ((not (or (eq ch #\newline) (eq ch #\return))))
      (let ((next-char (peek-char nil stream nil eof-option)))
	(if (and (eq ch #\return) (eq next-char #\newline))
	  (tyi-raw stream eof-option)))
      (setq ch (tyi-raw stream eof-option))
      (let ((next-char (peek-char nil stream nil eof-option)))
	(if (and (eq ch #\\ ) (or (eq next-char #\return) (eq next-char #\newline)))
	  (setq ch (tyi-raw stream eof-option))
	  (return-from eat-continuations ch))))
    ch))

;;I don't think these are terribly useful so why use them.

(defmacro *expr (&rest x)
  (declare (ignore x))
  nil)

(defmacro *lexpr (&rest x)
  (declare (ignore x))
  nil)

(defmacro *fexpr (&rest x)
  (declare (ignore x))
  nil)

(defvar ^w nil)
(defvar ^r nil)


;;essentially no common lisp support for cursor pos:
;;should be adapted for a particular implementation.
;;perhaps even smart-tty could be set to t.

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

(defun $timedate ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p))
    (format nil "~2,'0d:~2,'0d:~2,'0d ~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~d/~2,'0d/~d (GMT~@d)"
	    hour minute second day-of-week month date year (- tz))))

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
