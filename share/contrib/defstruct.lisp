;;; from maxima-5.9.0/maxima-5.9.0/src/mlisp.lisp
(in-package :maxima)

;;; changes, 8/14/05
;;; 1. Improved error messages for mset
;;; 2. Allows setting of record fields e.g.  XX@YY:45 if mset_extension_operators set up

;;; author Richard Fateman

;; modifications 2005/08/28 Robert Dodier
;; (0) cut matrix assignment code from previous rev (but kept parallel multiple assignment)
;; (1) $NEW barfs if #arguments != 1, or argument has no defstruct, or wrong number of initializers.
;; (2) $DEFSTRUCT allows 1 or more arguments, returns a list of defstructs.
;; (3) use $PUT and $GET to maintain defstruct properties 
;;     (renamed to $DEFSTRUCT_DEFAULT and $DEFSTRUCT_TEMPLATE).
;;     This makes defstruct properties visible to user via get and propvars.
;;     Also, this makes `kill' kill defstructs.

;; If this is the last def'n in file of mset_extension_operators,
;; it will disable the $@ defstruct features

(defparameter mset_extension_operators nil)

(defmfun mset (x y)
  (declare (object y x))
  (prog nil
    ;; first see if we are supposed to report this assignment
    ;; to the user.  Is $setcheck set to a list containing x?
	(cond ((or (null $setcheck)
		   (eq $setcheck '$setcheck))) ;setcheck "not set"
	      ((and (or (atom $setcheck)
			(memalike x (cdr $setcheck)) ;; setcheck set to x?
			(and (not (atom x)) ;; setcheck set a list with x?
			     (memalike (caar x) (cdr $setcheck))))
		    (not (eq x y)))
	       ;; the conditions for printing out the trace on set are fulfilled.
	       (displa (list '(mtext) (disp2 x) '| set to | y))
	       ;; now, check to see if we are supposed to wait in a break at this point
	       (if $setcheckbreak
		   (let (($setval y))
		     (merrbreak t)
		     (setq y $setval)))))
    
	(cond ((atom x) ;; typical case is setting an atom to a value
	       (when (or (not (symbolp x)) 
			 (member x '(t nil) :test #'eq) ;can't set t or nil, boolean constants
			 (mget x '$numer)  ;can't set a numeric constant like $%pi or $%e
			 (char= (getcharn x 1) #\&)) ;can't set a string (begins with &)
		 (if munbindp (return nil)) ;dunno what this does. see mlisp.lisp file.
		 ;; an unsettable atom. Signal an error.
		 (if (mget x '$numer)
		      (merror "~:M is a constant. Attempt to reassign it." x)
		  (merror "~:M is an improper left-hand side for an assignment" x))
		 )
	       ;; a settable atom. Let's get on with it.
	       (let ((f (get x 'assign)))
		 (if (and f (or (not (eq x y))
				(member f '(neverset read-only-assign) :test #'eq)))
		     (if (eq (funcall f x y) 'munbindp) (return nil))))
	       (cond ((and (not (boundp x))
			   (not dsksetp)) ;? something about disk?
		      (add2lnc x $values)) ;not previously bound, make a note.
		     ((and (not (eq x y))  ; something about macsyma options?
			   (optionp x))
		      (if $optionset (mtell "~:M option is being set.~%" x))
		      (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	       
	       (return (set x y))) ;; actually put a value in lisp value cell!
	      
	      ;; ADDITION  8/17/05 RJF thnx to suggestions by S. Macrakis, R. Dodier,;;;;;;;;;;;;
	      ;;check to see if the operator has an mset_extension_operator.
	      ;; If so, this says how to do assignments. Examples, a@b:x. Put mset_extension_operator
	      ;; of $mrecordassign on the atom $@.  To allow [a,b]:[3,4] put op on mlist.
	      ;; arguably we could use mget, mfuncall, and $mset_extension_operator  and
	      ;; allow this to be done at the maxima level instead of lisp.

          ;; X is could be something like (($FOO ARRAY) 42), in which case it is meaningful
          ;; to look for an assignment operator associated either with $FOO itself or with
          ;; $FOO's object type, with "object type" = (CAAR (SYMBOL-VALUE '$FOO)).

          ((let*
            ((x-value (if (boundp (caar x)) (symbol-value (caar x))))
             (mset-extension-op
                (cond
                  ((get (caar x) 'mset_extension_operator))
                  ((and (not (atom x-value)) (get (caar x-value) 'mset_extension_operator))))))
            (if mset-extension-op
              (return-from mset (funcall mset-extension-op x y)))))

	      ;; x is not an atom, but something like (($M array simp) 12)
	      ((member 'array (cdar x) :test #'eq)
	       (return (arrstore x y))) ;; do the array store
	      
	  ;;    ((and $subscrmap (member (caar x) '(mlist $matrix) :test #'eq));; deprecated.
	 ;;      (return (outermap1 'mset x y)))

	      (t (merror "Improper left-hand side for an assignment:~%~M" x)))))

;;; starting here..
(setf (get '$@ 'mset_extension_operator) '$mrecordassign)

;;; new programs by Richard Fateman 8/14/05
;;  defstruct(f(x,y,z));
;;  myrecord: new(f);
;;  myrecord@y:45;
;;  myrecord;  ==>   f(x,45,z)


;; initializers are possible
;; defstruct(f(x,y=3.14159, z));
;; ff:new(f)  ==>   f(x,3.14159,z)
;; ff@y:2.71828 ==>  ff is  f(x,2.71828,z).

;; the @ syntax can also be used instead of substinpart.

;; k:  h(g(aa,bb),cc);
;; k@1@2:dd; change aa to dd.
;; k;

;;; This definition and the ones following are needed to get the @ stuff going
;;(defparameter mset_extension_operators    '(($@ . $mrecordassign)))

	
(defmfun $mrecordassign (atted value)
  ;; assume atted is  (($@..)  instance-name  field-name)
  ;; should insert some checking code here
  (let* ((in (cadr atted))		;instance
	 (fn (caddr atted))		;field
	 (obj (meval in))
	 (index (if (integerp fn) fn ;;; allow foo@3, also
	      (position fn ($get (caar obj) '$defstruct_template))))
	 ) ;field->integer
    
    (if (null index) (merror "Unknown field in record:~%~M" fn))
    (if  (< 0 index (length obj)) (setf (elt obj index) value)
      (merror "Illegal instance:~%~M @ ~M" in fn))
    value))


(defmfun $@ (in fn)
  (if (not (listp in))(list '(%@) in fn) ;noun form
    (let* ((index  
	    (if (integerp fn) fn ;;; allow foo@3, also
	      (position fn ($get (caar in) '$defstruct_template))))) ;field->integer
    (if (null index) (merror "Unknown field in record:~%~M" fn))
    (if  (< 0 index (length in))
	(elt in index) (merror "Illegal instance:~%~M @ ~M" in fn))
   )))


;; This will not work for compiled code.

;; L looks like defstruct (foo(...), bar(...), baz(...)).
;; Process each argument and return a list of declared structures.

(defmspec $defstruct (L)
  `((mlist) ,@(mapcar 'defstruct1 (cdr L))))

(defun defstruct1 (z) ;; z will look like (($whatever) $a $b $c)
   ;; store the template on $whatever
  ($put (caar z) (namesonly z) '$defstruct_template)
  ;; set the initialization on $whatever
  ($put (caar z) (initializersmostly z) '$defstruct_default)
  z)

(defun namesonly(r)			; f(a,b,c) unchanged, f(a=3,b=4,c=5) -> f(a,b,c)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal)(second z))
				(t (merror "~% Expected record initializer, not ~M." z))))
		       (cdr r))))
(defun initializersmostly(r);; f(a=3,b,c=5) -> f(3,b,5)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal)(third z))
				(t (merror "~% Expected record initializer, not ~M." z))))
		       (cdr r))))

(defmspec $new (h)
  (if (not (= (length (cdr h)) 1))
    (merror "~% new: expected exactly one argument, not ~M." (length (cdr h))))

  (let ((recordname (cadr h)))
    (cond
      ((symbolp recordname)  ;; the case of, e.g.  new(f);
       (if (null ($get recordname '$defstruct_default))
         (merror "~% new: don't know anything about `~M'." recordname))

       (copy-tree ($get recordname '$defstruct_default)))

      ;; assume there is some initialization here e.g. new (f(5,6,7))
      (t
        (let ((recordop (caar recordname)) (recordargs (cdr recordname)))
          (if (null ($get recordop '$defstruct_default))
            (merror "~% new: don't know anything about `~M'." recordop))

          (if (not (= (length recordargs) (length (cdr ($get recordop '$defstruct_default)))))
            (merror "~% new: wrong number of arguments in initializer; expected ~M, not ~M."
                    (length (cdr ($get recordop '$defstruct_default))) (length recordargs)))

          (copy-tree recordname))))))

;; this is the lisp code equivalent to executing the command
;; infix(@);
;; 

(defprop $@ %@ verb) 
(defprop $@ &@ op) 
(defprop &@ $@ opr) 
;;(add2lnc '&@ $props) 
(define-symbol '&@) 
(defprop $@ dimension-infix dimension) 
(defprop $@ (#\space #\@ #\space) dissym) 
(defprop $@ msize-infix grind) 
(defprop $@ 180 lbp) 
(defprop $@ 180 rbp) 
(defprop $@ parse-infix led) 
(defprop %@ dimension-infix dimension) 
(defprop %@ (#\space #\@ #\space) dissym) 
(defprop %@ $@ noun) 

;;;;;;;;;;;;;8/15/05 RJF
;;  after reading in the redefinition of mset and mset_extension_operators
;; (not necessarily the @ stuff)
;; the follow code implements PARALLEL LIST assignment.
;; it is consistent with commercial macsyma.  [a,b,c]:[x,y,z] means
;;  about the same as a:x, b:y, c:z.  Actually it
;; evaluates x,y,z  BEFORE any assignments to a,b,c, hence parallel.
;; Also implemented is [a,b,c]:x  which evaluates x once and assigns
;; to a,b,c.
;; value returned is (evaluated x to ex)  [ex,ex,ex].


;; quiz .  [a,b]:[b,2*a].  produces values a=b, b= 2*a.
;; re-execute the statement 4 times. what do you get?  [4b, 8a]
;;         
;; a neat application of parallel assignment is this version of
;; a gcd algorithm (for integers)...
;; kgcd(a,b):=(while b#0 do [a,b]:[b,remainder(a,b)], abs(a));
;; The extended euclidean algorithm looks even better with parallel
;; assignment.

;; add MLIST to possible operators on the left hand side of 
;; an assignment statement.

(setf (get 'mlist 'mset_extension_operator) '$mlistassign)

(defmfun $mlistassign (tlist vlist)
  ;;  tlist is  ((mlist..)  var[0]... var[n])  of targets
  ;; vlist is either((mlist..)  val[0]... val[n]) of values
  ;; or possibly just one value.
  ;; should insert some checking code here
  (if (and (listp vlist)
	   (eq (caar vlist) 'mlist)
	   (not (= (length tlist)(length vlist))))
      (merror "Illegal list assignment: different lengths of ~M and ~M." tlist vlist))
  (unless (and (listp vlist)
	   (eq (caar vlist) 'mlist))
    (setf vlist (cons (car tlist) ;; if [a,b,c]:v  then make a list [v,v,v]
		      (make-sequence 'list (1-(length tlist)) :initial-element vlist))))
  (map nil #'mset (cdr tlist)(cdr vlist))
   vlist)
