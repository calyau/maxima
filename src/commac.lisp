;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(eval-when (compile load eval)

(defmacro deffif (new old)
  (assert (and  (symbolp new) (symbolp old)))
  (cond ((eql new old) nil)
	(t `(deff ,new (function ,old)))))

(defmacro def-copy-special (fn symbol-to-copy)
  (cond ((not (eql fn symbol-to-copy))
	 `(defmacro ,fn (&rest l)
	    `(,',symbol-to-copy ,@  l)))))

(defmacro defun-prop (f arg &body body)
  (assert (listp f))
  #+lispm
  `(defun (:property ,@ f) ,arg ,@ body)
  #+gcl
  (eval-when (eval )
	     (compiler::compiler-def-hook (car f) body))
  #-lispm
  `(progn 'compile
	  (setf (get ',(car f) ',(second f))
	#'(lambda ,arg
	    ,@ body))))

(progn 'compile
;(deffif zl-REM  global:REM) ;;had hokey definition in macsyma remprop--I replaced by remprop
)

#+lispm

(deff %data-type  #'si::%data-type)

#+(or symbolics tirel3) 
(eval-when (compile load)
(deff $make_hash_table #'make-hash-table)
(deff ATAN  #'global:ATAN) ;shadow it .. wrong def
(deff ATAN2 #'global:ATAN2)
(deff ERRSET #'global:ERRSET)
(deff font-char-height #-ti #'global:font-char-height #+ti #'tv:font-char-height)
(deff font-char-width #-ti #'global:font-char-width #+ti #'tv:font-char-width )

)
)

(defvar prin1 nil) ;a function called instead of prin1.

(eval-when (load compile eval)
(defvar *allow-redefines* t)
)
 
;(defmacro defun-if-new (fun args &body body)
;  `(cond ((and (null *allow-redefines*) (fboundp ',fun)) nil)
;	(t (progn 'compile
;		   (si::record-source-file-name ',fun 'defun-if-new)
;		   (defun ,fun ,args
;		      ,@ body)))))

(eval-when (load compile eval)
(defun appears (tree var) (cond ((equal tree var)  (throw 'appears t))
			   ((atom tree) nil)
			   (t  (appears  (car tree) var) (appears (cdr
       tree)  var))) nil)

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


;(defvar *primitive-data-type-function* 'type-of)
;it is faster to use this on the lispm, but type-of would be ok.
;in kcl one would use the number code given by type_of(x);

(eval-when (compile load)
#+lispm
(defvar *primitive-data-type-function* 'si::%data-type)
#-lispm
(defvar *primitive-data-type-function* 'type-of)
)
(defmacro one-of-types (typ &rest objs &aux all)
  "typ is a primitive data type of the machine, and"
  (dolist (v objs
	     (cond ((cdr all)`(memq ,typ ',all))
			(t `(eql ,typ ',(car all)))))
    (pushnew  (funcall *primitive-data-type-function* (eval v)) all)))

(defun maclisp-typep (x &optional type)
  (cond (type
	 (lisp:let (( pred (get type 'ml-typep)))
        (cond (pred
		(funcall pred x))
	       (t (typep x type)))))
	(t
	(lisp:let ((.type. (#. (if (boundp '*primitive-data-type-function*)
				   *primitive-data-type-function* 'type-of)
				   x)))
	  (cond
	   ((one-of-types .type.  'hi nil) 'symbol)
	   ((one-of-types .type. '(a)) 'list)
	   ((one-of-types .type. 3) 'fixnum)
	   ((one-of-types .type.  (make-array 3) "abc")
	    (cond ((stringp x) 'string) ;;should really be symbol 'ugggh
		  #+ti ((hash-table-p x) 'hash-table)
		  (t 'array)))
	   ((one-of-types .type. (expt 2 50) 1.234 most-positive-single-float
			  most-positive-double-float most-positive-long-float )
	      (cond 
		    ((integerp x) 'bignum)
		    ((floatp x) 'flonum )
		    (t 'number)))
	   ;;note the following is 'random in maclisp
	   ((one-of-types .type. #'cons) 'compiled-function)
	   #-ti ((one-of-types .type.  (make-hash-table))
	    (cond ((hash-table-p x) 'hash-table)
		  (t (type-of x))))
	   ((arrayp x) 'array)
	   ;((one-of-types .type. (make-array '(2 3)))  'array)
	   (t (type-of x)))))))

 
(deff ml-typep #'maclisp-typep)
;;so that (ml-typep a 'list) ==> (zl-listp a)

#+symbolics
(progn
compiler:
(defoptimizer (ml-typep maxima-typep-two-args) (form)
  (cond ((and (= (length form) 3)
	      (constant-form-p (third form)))	;constant type
	 (lisp:let* ((typed-form (second form))
		(type (constant-evaluator (third form)))
		(pred (and (symbolp type)
			   (or (get type 'ml-typep)))))
	   (cond (pred `(,pred ,typed-form))
		 (t
		  (cons 'global:typep (cdr form))))))
	(t form)))

)

(defprop :extended-number extended-number-p ml-typep)
(defprop array arrayp ml-typep)
(defprop atom  atom ml-typep)
(defprop compiled-function compiled-function-p ml-typep)
(defprop extended-number extended-number-p ml-typep)
(defprop fixnum fixnump ml-typep)
(defprop list consp ml-typep)
(defprop number numberp ml-typep)
(defprop string stringp ml-typep)
(defprop symbol  symbolp ml-typep)

(defvar *make-array-option-replacements*
	'((:element-type  :type
			   ( t . art-q)
			   ( string-char . art-string))
	  (:initial-element  :initial-value nil)
	  (:adjustable				;remove :adjustable
	   )))					

(defvar *cl-make-array-option-replacements*
	'((:type  :element-type
			   (  art-q  . t)
			   ( art-1b  . (mod 2))
			   (art-8b   . (mod 8))
			   (art-4b   . (mod 4))
			   ( art-string . character  ))
	  (:initial-value  :initial-element nil)
	  (:adjustable				;remove :adjustable
	   )))					


(defun maxima-copy-rest (form)
  "copy those things out of the stack in `(($hi array) ,@ inds) which would be bad if inds is rest arg"
  (copy-list form))


(defun substitute-keyword-arg ( repl keyword-rest-arg &aux tem answ)
  (sloop for (key arg) on keyword-rest-arg by 'cddr
	do 
	(cond ((setq tem (zl-ASSOC key repl))
	       (cond ((cdr tem)
		      (push (sublis (cddr tem) arg) answ)
		      (push (second tem) answ))))
	      (t (push arg answ) (push key answ))))
  answ)


(defun zl-make-array (dimensions &rest options)
 (apply `make-array dimensions   (substitute-keyword-arg *cl-make-array-option-replacements* options)))

;;I finally decided to change the typep.  It can be speeded up later.
;;The compiler can optimize (ml-typep 5 'fixnum) to (fixnump 5) etc.
;;if we give it the right properties.


  
;;;note *array takes 'fixnum and 'flonum as its keyword args!!!!
;;need to use our selectq to ensure the type is correct
;(ARRAY CONUNMRK NIL (f1+ CONNUMBER))

(defvar *maxima-arrays* nil "Trying to track down any functional arrays in maxima")

;;only remaining calls are for maclisp-type = nil
(DEFUN *ARRAY (NAME MACLISP-TYPE &REST DIMLIST &AUX AARRAY)
  (cond ((MEMQ MACLISP-TYPE '(READTABLE OBARRAY))
	 (ERROR " bad type ~S" MACLISP-TYPE)))
  (pushnew name *maxima-arrays*) ;for tracking down old ones.
  (SETQ AARRAY (make-array DIMLIST   ':INITIAL-element
			   (case MACLISP-TYPE 
				 (FIXNUM 0)
				 (FLONUM 0.0)
				 (OTHERWISE NIL))))
  (COND ((NULL NAME)
	 AARRAY)
	((SYMBOLP NAME)
	 (setf (symbol-array NAME) AARRAY)
	 NAME)
	(T (error "~S is illegal first arg for *ARRAY" NAME))))

(DEFMACRO ARRAY (NAME MACLISP-TYPE &REST DIMLIST)
  `(*ARRAY ',NAME ',MACLISP-TYPE ,@ DIMLIST))

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
;(defmacro symbol-array (ar)
;    `(symbol-function ,ar))
;(defmacro arraycall (ignore ar &rest dims)
;  `(aref ,ar ,@ dims))
;;;IV. change array setting to use (setf (arraycall nil ar dim1.. ) val) 
;;;which will generate the correct setting code on the lispm and will
;;;still work in maclisp.

(defmacro MAXIMA-ERROR (ctl-string &rest args)
  `(cerror "without any special action" ,ctl-string ,@ args))

(defmacro safe-value (sym)
  (cond ((symbolp sym)
	 `(cond ((symbolp ',sym)
		 (and (boundp ',sym) ,sym))
		(t ,sym)))
	(t nil)))

#-lispm
(defmacro with-polynomial-area (ign &body body) ign
  `(progn ,@ body))

(defmacro show (&rest l)
  (sloop for v in l
	collecting `(format t "~%The value of ~A is ~A" ',v ,v) into tem
	finally (return `(progn ,@ tem))))

(defmacro defquote  (fn (aa . oth) &body rest &aux help ans ) 
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
		     (setq other (sloop for v in other collecting (list 'quote v)))
		     (check-arg other (eql (length other) ,(length oth)) ,(format nil "wrong number of args to ~a" fn))
		     `(,',help  ',,aa   ,@ other))
		  `(defun ,help (,aa ,@ oth) . ,rest)))))
  `(progn 'compile . , ans))


(defquote $mdefvar (&rest l)
  `((defvar) ,@ l))

;;the resulting function will translate to defvar and will behave
;;correctly for the evaluator.

;(defun gg fexpr (ll)
;       body)
;(defquote gg (&rest ll)
;       body)

;(DEFQUOTE GG ( &rest C)
; (list  (car c) (second c) ))
;;the big advantage of using the following over defmspec is that it 
;;seems to translate more easily, since it is a fn.
;;New functions which wanted quoted arguments should be defined using
;;defquote


(defun onep (x) (eql 1 x))

(defun extended-number-p (x)
  #+lispm
  (eql (%data-type x)  (%data-type 10000000000000000))
  ;si:dtp-extended-number
  #-lispm
  (member (type-of x) '(bignum rational float ))
  )

;(defun si::extendp (x) (and (numberp x) (not (fixnump x))))

(defvar *scan-string-buffer*  nil)

#+lispm 
(progn 
;;stuff for temporary area collections and copying.

(deffif STORE-ARRAY-LEADER global:STORE-ARRAY-LEADER)
(deff %AREA-NUMBER #'si::%AREA-NUMBER)
(defmacro copy-number (x)
  (cond ((fboundp 'si:copy-extended-number) `(si:copy-extended-number ,x))
	(t `(f- (f- ,x)))))



(defvar *temporary-polynomial-area*)

;;fixed to handle (cons 1 bad-bignum)
(defun copy-atomic-structure
  (atomic-object
   &optional (area-to-avoid *temporary-polynomial-area*) )
  (select (%data-type atomic-object)
	  (si:dtp-extended-number
	   (cond ((eql (%area-number atomic-object) area-to-avoid )
		  (copy-number atomic-object ))
		 (t atomic-object)))
	  (#-ti si:dtp-array #+ti si:dtp-array-pointer
		(cond ((eql (%area-number atomic-object) area-to-avoid)
		       (lisp:let
			(ar array-options)
			(cond ((named-structure-p atomic-object)
			       (setq array-options (list
						    :named-structure-symbol
						    (aref atomic-object 0)))))
				   (setq array-options
					 (nconc array-options
						(list
						 :type (array-element-type atomic-object)
						 :leader-length (array-leader-length atomic-object))))
				   (setq ar (apply 'global:make-array (array-dimensions atomic-object)
						   array-options))
				   (cond ((eq (array-type ar) 'art-q)
					  (lisp:let ((from atomic-object) (to ar)		
						       (to-length (length ar))
						       (index 0))
						      #-ti (declare (si:array-register from to))
						      (sloop for ind from index below to-length
							    do (aset (copy-from-temporary-area
								      (aref from ind)) to ind))
						      (sloop for i from 0 below  (or (array-leader-length from) 0)
							    do (store-array-leader
								(copy-from-temporary-area
								 (array-leader from i)) TO I))))
					 (t (copy-array-contents-and-leader atomic-object ar)))
				   ar))
		      (t atomic-object)))
;;;	    ((and (typep tree :symbol) (check-tree-for-area (symbol-plist tree))) tree)
	  (t atomic-object)))
 
(DEFUN copy-from-temporary-area (TREE &OPTIONAL
				 (area-to-avoid *temporary-polynomial-area*)
				 &aux tem)
  "If the first cons is in area to avoid it copys whole list and cdr codes,
  but if not it goes to the next cons and repeats."
  (IF (ATOM TREE)
      (cond ((or (fixnump tree) (symbolp tree)) tree)
	    (t(copy-atomic-structure tree area-to-avoid)))
      (lisp:let ((RESULT (cond ((eql (%area-number tree) area-to-avoid )(copy-list  tree))
			  (t  TREE))))
	(cond ((eq result tree)
	       (DO ((R RESULT (CDR R)))((atom r) result)
		 (COND ((EQL (%AREA-NUMBER (CDR r) ) AREA-TO-AVOID)
			;;some of the later cons's in tree might be in bad area
			(setf (cdr R) (copy-from-temporary-area   (cdr R) area-to-avoid))))
		 (RPLACA R (copy-from-temporary-area (CAR R)))))
	      (t
	       (DO ((R RESULT (CDR R)))
		   ((atom r) result)
		 (COND ((AND (ATOM (CDR r))
			     (EQL (%AREA-NUMBER (CDR r) ) AREA-TO-AVOID))
			(setf (cdr  R) (copy-atomic-structure  (cdr R) area-to-avoid))))
		 (setq tem (car r))
		 (cond ((or (fixnump tem) (symbolp tem)))
		       (t 
			(RPLACA R (copy-from-temporary-area
				    tem area-to-avoid))))))))))
;;end of lispm stuff for areas.
)

(defun macsyma-read-string (a-string &aux answer)
  (cond ((not  (or (string-search "$" a-string) (string-search ";" a-string)))
	 (vector-push-extend #\$ a-string)))
  (with-input-from-string (stream a-string)
			  (setq answer (third (mread stream)))
			  answer))

(defvar *sharp-read-buffer*
  (make-array 140 :element-type ' #.(array-element-type "abc") :fill-pointer 0
	      :adjustable t))

(defun x$-cl-macro-read (&optional  stream arg &rest rest-arg)
  rest-arg ;ignored
  ($-read-aux arg stream))

(defun $-read-aux (arg stream &aux (meval-flag t) (*mread-prompt* ""))
  (declare (special *mread-prompt*))
  arg					;ignore
  (setf (fill-pointer *sharp-read-buffer* ) 0)
  (cond ((eql #\$ (tyipeek t stream))(send stream :tyi)
	 (setq meval-flag nil)))
  (with-output-to-string
   (st *sharp-read-buffer*) 
   (let (char)
     (sloop while (not (eql char #\$))
	   do
	   (setq char (tyi stream))
	   (tyo char st)
	   finally (cond ((not (eql  char #\$))
			  (error "There was no matching $" ))))))
  (cond (meval-flag 
	 (list 'meval* (list 'quote
			     (macsyma-read-string *sharp-read-buffer*))))
	(t  (list 'quote  (macsyma-read-string *sharp-read-buffer*)))))
 

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




#+ti ;;till its fixed by ti.

(defun signum (x) (cond ((> x 0) +1)
			((< x 0) -1)
			((zerop x) 0)
			(t (error "bad arg to signum"))))


;;;to handle the maclisp (defun foo narg .. syntax.)
;;;see below for simpler method, but we have to redefine arg etc.
;;need to 
;;  I. convert to (defun foo (&rest narg-rest-argument (&aux (narg (length narg-rest-argument)))
;;  II. replace (arg 1) by (nth-arg 1)  and (listify i)  by  (narg-listify i) using the following definitions.
;;probably better not to shadow the listify etc. since someone might try to compile some maclisp code
;;and we should not break that.
;;REPLACEMENTS:
;'(("arg" . "narg-arg")
;  ("listify" . "narg-listify")
;  ("setarg" . "narg-setarg"))
;;new macros:
(defun lastn (n x)
   (nthcdr (f- (length x) n) x))

(defmacro narg-arg (x)
  `(nth (f1- ,x) narg-rest-argument ))
(defun narg-listify1 (x list)
  (cond ((minusp x) (lastn (abs x) list))
	(t (firstn x list))))
(defmacro narg-listify (x)
   `(narg-listify1 ,x narg-rest-argument))
(defmacro narg-setarg (i val)
  `(setf (narg-arg ,i) ,val))

;;test of above
;(defun foo (&rest narg-rest-argument &aux (narg (length narg-rest-argument)))
;  (show  (narg-listify 3))
;  (show  (narg-listify -3))
;  (show  (narg-arg 2))
;  (narg-setarg 2 8)
;  (show (narg-listify 3)))

(defvar *reset-var* t)

(defmacro defmvar (var &rest val-and-doc)
  "If *reset-var* is true then loading or eval'ing will reset value, otherwise like defvar"
  (cond ((> (length val-and-doc) 2)
	 (setq val-and-doc (list (car val-and-doc) (second val-and-doc)))))
  `(progn
	   #+lispm (si::record-source-file-name ',var 'defmvar)
	  (defvar ,var ,@ val-and-doc)
	  #+debug
	  (maybe-reset ',var ',(if val-and-doc (list (car val-and-doc))))))

(defun maybe-reset (var val-and-doc &aux val)
  (cond (*reset-var*
	 (cond ((not(eq 'nil val-and-doc))
		(cond ((not (equal (setq val (eval (car val-and-doc))) (symbol-value var)))
		       (format t "~%Replacing value of ~A" var)
		       (set var val))))
	       (t (cond ((boundp var)
			 (format t "~%Removing value of ~A" var)
			 (makunbound var))))))))

(defun $mkey (variable)
  "($mkey '$demo)==>:demo"
   (intern (string (string-left-trim "$" (string variable))) 'keyword))

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
(defvar semi-colon-char (code-char 59.))   ;; #\;)
(defvar back-slash-char (code-char 92.))   ;; #\\)
(defvar forward-slash-char (code-char 47.)) ;; #\/)
(defvar left-parentheses-char (code-char 40.))  ;(
(defvar right-parentheses-char (code-char 41.)) ;)
(defvar period-char (code-char 46.))            ;.
(defvar vertical-stroke-char (code-char 124.))  ;|
(defvar $forward-slash-symbol #-cl '$// #+cl '$/ )
;(defvar $colon-char (intern "$:"))
;(defvar $comma-char (intern "$,"))


(defvar forward-slash-string (string forward-slash-char))

;To handle the old narg syntax

;(shadow '(arg listify setarg) 'maxima)

(defmacro arg (x)
  `(narg1 ,x narg-rest-argument))

(defun narg1 (x l &aux tem)
  (cond ((null x) (length l))
	(T (setq tem (nthcdr (f1- x) l))
	   (cond ((null tem) (error "arg ~A beyond range ~A " x (length l)))
		 (t (car tem))))))

(defmacro listify (x)
   `(listify1 ,x narg-rest-argument))

(defmacro setarg (i val)
   `(setarg1 ,i ,val narg-rest-argument))

(defun setarg1 (i val l)
  (setf (nth (f1- i)l) val) val)

(defun listify1 (n narg-rest-argument)
  (cond ((minusp n) (copy-list (nleft (f- n) narg-rest-argument)) )
	((zerop n) nil)
	(t (firstn n narg-rest-argument))))

;(DEFVAR *LEXPR-ARGLIST*)

;;to we need to bind *lexpr-arglist* to narg-rest-argument. 
;;its less efficient and SURELY no code refers to it. 

(Defmacro DEFMFUN (function &body  REST &aux .n.)
  #+NIL (macsyma-defmfun-declarations function rest)
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
	 (DEFUN ,FUNCTION . ,REST)))

;;sample usage
;;(defmfun foo a (show a )(show (listify a)) (show (arg 3)))


#+obsolete
(defun string-from-char-list (list &aux str)
  (let #+lispm ((default-cons-area working-storage-area)) #-lispm nil
    (setq str  (make-string (length list)))
    (sloop for v in  list
	  for i from 0
	  do (cond 
	       ((characterp v)
		(setf (aref str i) v))
	       ((numberp v) (setf (aref str i) (code-char v)))
	       ((symbolp v)   (setf (aref str i) (aref (symbol-name v) 0)))
	       (t (error "unknown type")))
	  finally (return str))))
  
;#-symbolics
(defun exploden (symb &aux string)
  (cond ((symbolp symb)(setq string (symbol-name symb)))
        ((floatp symb)
	 (let ((a (abs symb)))
	   (cond ((or (eql a 0.0)
		      (and (>= a .001)
			   (<= a 10000000.0)))
		    (setq string (format nil "~vf" (+ 1 $fpprec) symb)))
		 (t (setq string (format nil "~ve" (+ 4 $fpprec) symb)))))
	 (setq string (string-left-trim " " string))
	 )
	
	(t (setq string (format nil "~A" symb))))
  (assert (stringp string))
  (list-string string)
  )
;;there's a bug in SCL which mucks up pnames so we have to revert to zetalisp:
;;It appears to be ok in generra 7.2
#+buggy-symbolics
(defun exploden (symb &aux string)
  ;;like princ
  (cond ((symbolp symb)
	 (sloop for i below (global:string-length (setq string
						       (global:string symb)))
	       collecting (code-char(aref string i))))
	(t (setq string (format nil "~A" symb))
	   ;;just for a while with this scl compat package
	   (assert (stringp string))
	   (list-string string)
	   )))

(defun explodec (symb &aux tem sstring)
  (setq sstring (format nil "~a" symb))
  ;(setq sstring (coerce symb 'string))
  (sloop for v on (setq tem (list-string sstring))
	do (setf (car v)(intern (string (car v)))))
  tem)

(defvar *string-for-implode* (make-array 20 :fill-pointer 0 :adjustable t :element-type ' #. (array-element-type "ab")))
(defun implode (lis) (implode1 lis nil))

(defun implode1 (lis upcase &aux (ar *string-for-implode*) (leng 0))
  (declare (type string ar) (fixnum leng))
  (or (> (array-total-size ar) (setq leng (length lis)))
      (adjust-array ar (+ leng 20)))
  (setf (fill-pointer ar) leng)
  (sloop for v in lis
	 for i below leng
	 do
	 (cond ((typep v 'character))
	       ((symbolp v) (setq v (aref (symbol-name v) 0)))
	       ((numberp v) (setq v (code-char v))))
	 (setf (aref ar i) (if upcase (char-upcase v) v)))
  (intern ar))

(defun bothcase-implode (lis  &aux tem )
  (cond ((not (eql (car lis) #\$))
	 (return-from bothcase-implode (implode1 lis nil))))
  (multiple-value-bind
   (sym there)
   (implode1 lis nil)
   (cond (there (if (setq tem (get sym 'upcase)) tem sym))
	 (t
	  ;; if all upper case lets not bother interning...
	  (sloop for v in lis with haslower
		 when (not (eql (char-upcase v) v))
		 do (setq haslower t) (loop-finish)
		 finally (or haslower (return-from bothcase-implode sym)))
	  (multiple-value-bind
	   (symup there)
	   (implode1 lis t)
	   (cond ((and there (or
			      ;; not single symbols
			      (cddr lis)
			      (fboundp symup) (symbol-plist symup)))
		       
		  (setf (get sym 'upcase) symup)
		  symup)
		 (t (or there (unintern symup))
		    sym)))))))


(defun list-string (strin &aux tem)
  (setq tem (make-list (length (the string  strin))))
  (sloop for v on tem
	for i from 0
	do (setf (car v) (aref strin i)))
  tem)

(defun explode (symb &aux tem sstring)
  (setq sstring (format nil "~s" symb))
  (sloop for v on (setq tem (list-string sstring))
	do (setf (car v)(intern (string (car v)))))
  tem)


#-symbolics
(defun getcharn (symb i &aux strin)
  (setq strin (string symb))
  (cond ((and (<= i (length strin)) (> i 0))
	 (aref strin (f- i 1)))
	(t (MAXIMA-ERROR "out of bounds"))))

;;Isn't this wonderful:
;;$B2 is in the CL-MAXIMA package
;;$B2 has SCL::PRINT-NAME property #"zzzzzz"

#-lispm
(defmacro zl-string (x) x)

#+lispm
(defun zl-string (cl-string &aux str)
  #+zlch cl-string
  #-zlch
  (cond ((and (arrayp cl-string)
	      (not (fixnump (aref cl-string 0))))
	 (setq str (global:make-array  (length cl-string) :type 'global:art-string))
	 (sloop for i below (length cl-string)
	       do (setf (aref str i) (char-int (aref cl-string i))))
	 str)
	(t cl-string)))

(defun zl-char (char)
  #+zlch char
  #-zlch (if (numberp char) char (char-int char)))

#+symbolics ;;because of SCL bug
(defun getcharn (symb i &aux zl-strin)
  (setq zl-strin (global:string symb))
  (cond ((and (<= i (length zl-strin)) (> i 0))
	 #-zlch (code-char(aref zl-strin (f- i 1)))
	 #+zlch (aref zl-strin (f- i 1))
	 )
	(t (MAXIMA-ERROR "out of bounds"))))

(defun getchar (symb i &aux strin)
  (setq strin (string symb))
  (cond ((and (<= i (length strin)) (> i 0))
	 (intern (string (aref strin (f- i 1)))))
	(t nil)))

(defun ascii (n)
  (intern (string n)))

(defun maknam (lis)
  (sloop for v in lis
	when (symbolp v)
	collecting (getcharn v 1) into tem
	else
	when (characterp v)
	collecting v into tem
	else do (MAXIMA-ERROR "bad entry")
	finally 
	(return (make-symbol (coerce tem 'string)))))

;;for those window labels etc. that are wrong type.

(defun flatc (sym)
  (length (explodec sym)))

(defun flatsize (sym)
 (length (explode sym)))
(defmacro safe-zerop (x)
  (cond((symbolp x)`(and (numberp ,x) (zerop ,x)))
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

(defun tyo (char &optional( stream *standard-output*))
  (write-char char stream))

(defun tyi (&optional (stream *standard-input*) eof-option )
   (cond (eof-option				
	    (read-char stream nil  eof-option))
	 (t(read-char stream nil nil))))



(DEFUN TYIPEEK (&OPTIONAL PEEK-TYPE &REST READ-ARGS)
  (cond (read-args
	 (peek-char peek-type (car read-args)))
	(t (peek-char peek-type))))

;I don't think these are terribly useful so why use them.

#-ti
(progn 'compile
(defmacro *expr (&rest x) x nil)
(defmacro *lexpr (&rest x) x nil)
(defmacro *fexpr (&rest x) x nil)
)

(defmacro local-declare (dcls &body body)
  dcls ;ignore
	  `(progn
;	    (declare ,@ dcls)
	    ,@ body))


;(defmacro symbol-array (sym)
;  `(symbol-function ',sym))
  
(defmacro arraycall (ign array &rest dims) ign
  `(aref ,array . ,dims))


;(DEFMACRO-DISPLACE ARRAYCALL (IGNORE ARRAY &REST DIMS)
;  `(FUNCALL ,ARRAY . ,DIMS))

;(defun readlist (lis)
;  (read-from-string   (coerce lis 'string)))

;#-ti
;(defun make-equal-hash-table (&rest l)
;  (apply 'make-hash-table :test 'equal  l))

;#-ti
;(defun array-length(x) (length x))

(defmacro copy-rest-arg (arg)
  #+lispm `(copy-list ,arg)
  #-lispm arg
  )

(defvar ^W nil)
(defvar ^R nil)

#+lispm
(defun cursorpos (&rest args &aux (str *standard-output*) q1 q2 )
  (cond ((null args)
	 (multiple-value-bind
	   (x y)
	     (send str :read-cursorpos ':character)
	   (cons y x)))
	(t
	 (setq q1 (first args))(setq q2 (second args))
	 (cond ((or (null q1) (and (fixnump q1 ) (null q2)))
		(multiple-value-bind (x y)(send str :read-cursorpos)
	         (send str :set-cursorpos (or q2 x) (or q1 y) :character) t))
	       ((and (fixnump q1) (fixnump q2))
		 (send str :set-cursorpos    q2 q1  :character) t)
	       (t (cond ((symbolp q1) (setf q1 (aref (symbol-name q1) 0)) t)
			(t (error "bad first arg to cursorpos")))
		  (case (char-downcase q1)
		    (#\a  (send str :fresh-line) t)
		    (#\b (send str :tyo #\backspace) t)
		    (#\c (send str :clear-window) t)
		    (#\e (send str #+ti :clear-eof #-ti :clear-rest-of-window) t )
		    (#\f (send str :tyo #\space) t )
		    (#\k (send str :clear-char) t)
		    (#\l (send str #+ti :clear-eol #-ti :clear-rest-of-line) t)
		    (#\z (send str :home-down) t)
		    (#\x (send str :tyo #\space) t)
		    (#\t (send str :home-cursor) t)
		    (otherwise (error "unknown arg for this simple cursorpos"))))))))

;;essentially no common lisp support for cursor pos:
;;should be adapted for a particular implementation.
;;the #+nocp (no cursorpos) flag could then be removed from *features*
;;and perhaps even smart-tty set to t.
#-lispm 
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

;;don't need for maxima.
;(defun change-cursorpos (str xdif ydif)
;  (multiple-value-bind (x y)
;      (send str :read-cursorpos :character)
;    (send str :set-cursorpos (f+ x xdif) (f+ y ydif))))

;(deff cursorpos #'global:cursorpos)

#+lispm
(progn
(defvar format:*print-special* 'my-print)
;;this allows the signalling of errors using the "abc ~M " syntax

format:
(DEFFORMAT M (:ONE-ARG) (ARG PARAMS)
	   (lisp:apply *print-special* arg *format-output* params))

(defun my-print (obj &optional (output *standard-output*) &rest params &aux
		     (*standard-output* output)$display2d)
  (declare (special $display2d))
  params ;ignore
    (mgrind obj output))
)



;
;(sloop for v in  (list  1 2.3 nil "hi" 'bye #'+ (list 1 2) (cons 1 2) (^ 2 40); (make-array 3) )
;      collecting (cons v (maclisp-typep v)))
;;on explorer June 19/85
;((1 . FIXNUM) (2.3 . FLONUM)
;              (NIL . SYMBOL)
;              ("hi" . STRING)
;              (BYE . SYMBOL)
;              (#<DTP-FEF-POINTER PLUS 13552362> . COMPILED-FUNCTION)
;              ((1 2) . LIST)
;              ((1 . 2) . LIST)
;              (1099511627776 . BIGNUM)
;              (#<ART-Q-3 20302251> . ARRAY))
;;on symbolics June /85
;((1 . FIXNUM) (2.3 . FLONUM)
;              (NIL . SYMBOL)
;              ("hi" . ARRAY)
;              (BYE . SYMBOL)
;              (#<DTP-COMPILED-FUNCTION ZETALISP:PLUS 22312724> . COMPILED-FUNCTION)
;              ((1 2) . LIST)
;              ((1 . 2) . LIST)
;              (1099511627776 . BIGNUM)
;              (#<ART-Q-3 40203511> . ARRAY))
;;The references after ; are to the original maclisp types as on tops-20 macsyma-maclisp version 302.
;;((1 . FIXNUM) (2.3 . FLONUM) ;ok
;;              (NIL . SYMBOL) ;ok
;;              ("hi" . ARRAY) ;want symbol "hi" is a symbol whose value is "hi" and with property 'internal-string-marker t
;;              (BYE . SYMBOL) ;ok
;;              (#<DTP-COMPILED-FUNCTION ZETALISP:PLUS 22312724> . COMPILED-FUNCTION) ;want 'random
;;              ((1 2) . LIST) ;ok 
;;              ((1 . 2) . LIST) ;ok
;;              (1099511627776 . NUMBER) ;want bignum
;;)
;;
;;

;(defvar *alpha-omega*  (sloop for i below 128 collecting (code-char  i)))
 
;#+(or cl utexas)
;compiler:
;(progn 'compile
;#+ti
;compiler:
;(defun mwarn (&rest args)
;  (apply 'warn  nil args))
;#-ti
;(deff mwarn #'warn)
;
;(defun check-=-for-chars (form &aux warn)
;  (sloop for v in '(maxima::char maxima::linechar maxima::getcharn maxima::getlabcharn)
;	when (maxima::appears-in (cdr form) v)
;	do (setq warn v))
;  (sloop for v in (cdr form)
;	when (and (symbolp v) (string-search "ch" v))
;	do (setq warn v))
;  (sloop for v in (cdr form)
;	when (and  (atom v)(member v maxima::*alpha-omega*))
;	do (setq warn (car (member v maxima::*alpha-omega*))))
;  (cond (warn (mwarn nil "Is = or char= correct since ~A appears in ~A" warn form))))
;(putprop 'maxima::= 'check-=-for-chars 'style-checker)
;)
 
;(defun trivial-obsolete-warn (form)
;  (warn nil "~A has a trivial definition and is obsolete" (car form)))
;compiler:
;(putprop 'cl-maxima::listen 'trivial-obsolete-warn 'compiler:style-checker)
;
;(load "cl-maxima-source:maxima;compile-warn")




;;;the ti gcd in microcode was broken earlier, and we needed this,
;;;but it is now ok and this can be removed.

;#+ti  ;;remove this after all recompiles
;(cond ((not (fboundp 'gcd))(deff gcd #'global:gcd))) 
;(progn 'compile 

;(defun gcd (m n)
;  (setq m (abs m) n (abs n))
;  (cond ((< n m)nil)
;	(t (rotatef m n)))
;  (cond ((zerop n) m)
;	((fixnump n)
;	 (setq m (mod m n))
;	 (cond ((zerop m) n)
;	       (t (bin-gcd m n))))
;	(t (gcd  n (mod m n)))))


;;;(bin-gcd 0 3) breaks but it should never be called!!
;(defun bin-gcd (u v &aux (k 0)u2 v2 t2 tt)
;  (sloop 
;	do (setq u2 (ash u -1))
;	when (not (eql (ash u2 1) u))
;	  do (return k)
;	do (setq v2 (ash v -1))
;	when (not (eql (ash v2 1) v))
;	  do (return k)
;       do (setq u u2 v v2 k (f1+ k)))
;  (prog ()
;     B2
;	(cond ((oddp u) (setq tt (f- v)))
;	      (t(setq tt (ash u -1))))
;     B3B4
;        (sloop  do (setq t2 (ash tt -1))
;	       when  (eql (ash t2 1) tt)
;               do (setq tt t2)
;		  else do (return nil))
;	(cond ((> tt 0) (setq u tt))
;	      (t (setq v (f- tt))))
;	(setq tt (f- u v))
;	(cond ((zerop tt)(return (ash u k)))
;	      (t (go b3b4)))))

;;(defun gcd (a b)
;;  (setq a (abs a) b (abs b))
;;  (cond ((< a b)(rotatef a b)))
;;  (cond ((zerop b) a)
;;	(t
;;	 (gcd1 a b))))

;;(defun gcd1 (a b &aux tem )
;;  (setq tem (mod a b))
;;  (cond ((zerop tem) b)
;;	(t (gcd1 b tem ))))
;)



(defvar *all-arrays* nil)
(defun function-array-p (sym)
  ;(push sym *all-arrays*)
  (arrayp (symbol-array sym)))


;; no generic way of knowing args numbers..
(defmacro MARGCHK (FN ARGS) fn args ())

(defun $timedate ()
  (system "date"))


;;Some systems make everything functionp including macros:
#+shadow-functionp
(defun functionp (x)
   (cond ((symbolp x)
          (and (not (macro-function x))
	       (fboundp x) t))
	 ((lisp::functionp x))))
	  
(defun file-to-string (x)
  (with-open-file
   (st x)
   (let* ((n (file-length st))
	  (ar (make-array n :element-type '#.(array-element-type "a")))
	  )
     (declare (type (array #.(array-element-type "a")) ar))
     (sloop for i below n 
	    for tem = (read-char st nil)
	    do 
	    (if tem    (setf (aref ar i) tem)))
     ar)))
     
		  
		  

