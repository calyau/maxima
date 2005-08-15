;; defstruct.lisp
;; a verbatim copy of http://www.cs.berkeley.edu/~fateman/temp/msethack.lisp
;; as retrieved on 2005/08/14 circa 23:55.

;;; from maxima-5.9.0/maxima-5.9.0/src/mlisp.lisp
(in-package :maxima)

;;; changes, 8/13/05
;;; 1. Improved error messages
;;; 2. Allows setting of record fields e.g.  XX@YY:45.

;;; author Richard Fateman

(defun mset (x y)
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
			 (memq x '(t nil)) ;can't set t or nil, boolean constants
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
				(memq f '(neverset read-only-assign))))
		     (if (eq (funcall f x y) 'munbindp) (return nil))))
	       (cond ((and (not (boundp x))
			   (not dsksetp)) ;? something about disk?
		      (add2lnc x $values)) ;not previously bound, make a note.
		     ((and (not (eq x y))  ; something about macsyma options?
			   (optionp x))
		      (if $optionset (mtell "~:M option is being set.~%" x))
		      (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	       
	       (return (set x y))) ;; actually put a value in lisp value cell!
	      
	      ;; x is not an atom, but something like (($M array simp) 12)
	      ((memq 'array (cdar x))
	       (return (arrstore x y))) ;; do the array store
	      
	      ((and $subscrmap (memq (caar x) '(mlist $matrix)))
	       (return (outermap1 'mset x y)))
	      
	      ;; ADDITION  8/13/05 RJF ;;;;;;;;;;;;
	      ;; The assignment looks like (XX.YY) : 45
	      ;; meaning something like  substpart(45, xx, index_of(YY,type_declare_of(XX)))
	      ((eq '$@ (caar x))
	       ;; for now, defer the semantics to another program
	       (return($mrecordassign x y)))
	      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	      
	      (t (merror "Improper left-hand side for an assignment:~%~M" x)))))


;;; new programs by Richard Fateman 8/13/05
;; infix(@); .... HOW to do this in LISP???
;; 
;;  defstruct(f(x,y,z));
;;  myrecord: new(f);
;;  myrecord@y:45;
;;  myrecord;  ==>   f(x,45,z)

;; initializers now possible
;; defstruct(f(x,y=3.14159, z));
;; ff:new(f)  ==>   f(x,3.14159,z)
;; ff@y:2.71828 ==>  ff is  f(x,2.71828,z).



	
(defmfun $mrecordassign (atted value)
  ;; assume atted is  (($@..)  instance-name  field-name)
  ;; should insert some checking code here
  (let* ((in (cadr atted))		;instance
	 (fn (caddr atted))		;field
	 (obj (meval in))
	 (index (if (integerp fn) fn ;;; allow foo@3, also
	      (position fn (get (caar obj) 'recordtemplate))))
	 ) ;field->integer
    
    (if (null index) (merror "Unknown field in record:~%~M" fn))
    (if  (< 0 index (length obj)) (setf (elt obj index) value)
      (merror "Illegal instance:~%~M" in))
    value))


(defmfun $@ (in fn)
  (if (not (listp in))(list '(%@) in fn) ;noun form
    (let* ((index  
	    (if (integerp fn) fn ;;; allow foo@3, also
	      (position fn (get (caar in) 'recordtemplate))))) ;field->integer
    (if (null index) (merror "Unknown field in record:~%~M" fn))
   
    (if  (< 0 index (length in))
	(elt in index) (merror "Illegal instance:~%~M @ ~M" in fn))
   )))


;; This will not work for compiled code.


(defmspec $defstruct(z) ;; z will look like (($defstruct) (($whatever)$a $b $c))
   ;; store the template on $whatever
  (setf (get (caaadr z)'recordtemplate)(namesonly (cadr z)))
  ;; set the initialization on $whatever
  (setf (get (caaadr z)'recorddefault)(initializersmostly(cadr z)))
  (cadr z))

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

(defmfun $new (recordname)
  (copy (get recordname 'recorddefault)))


(defprop $@ %@ verb) 
(defprop $@ &@ op) 
(defprop &@ $@ opr) 
(add2lnc '&@ $props) 
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
