
(defvar *q-ops*)
(setq *q-ops*'(
					;(%ACSCH  m_ACSCH 1)
	       ($REALPART  m_REAL 1 )
	       ($ImagPART  m_imag 1 )
	       ($conjugate m_conj 1)
	       ($abs m_abs 1)
	       (%ACSC  m_ACSC 1 )
	       (%ACOS  m_ACOS 1 )
					;(%SEC  m_SEC 1 )
					;(%ACOT  m_ACOT 1 )
	       (%TAN  m_TAN 1 )
	       (%SIN  m_SIN 1 )
					;(%SECH  m_SECH 1 )
					; (%ASEC  m_ASEC 1 )
	       (%TANH  m_TANH 1 )
	       (%COS  m_COS 1 )
					;(%COT  m_COT 1 )
					;(%CSC  m_CSC 1 )
	       (%ASIN  m_ASIN 1 )
	       (%SINH  m_SINH 1 )
	       (%COSH  m_COSH 1 )
					;(%COTH  m_COTH 1 )
					;(%CSCH  m_CSCH 1 )
					;(%ERF  m_ERF 1 )
					;(%ASINH  m_ASINH 1 )
	       (%LOG  m_LOG 1 )
	       (MFACTORIAL  m_FACTORIAL 1)
					;(MNOT  m_NOT 1 )
					;(RAT  m_AT 1 )
	       (MAND  m_AND n )
	       (MSET  m_SET 2 )
	       (MPLUS  m_PLUS n )
					;(MPROGN  m_PROGN n )
					;(MQAPPLY  m_QAPPLY 1 )
					;(MSETQ  m_SETQ 2 )
					;(MCOND  m_COND nil)
	       (MGREATERP  m_gt 2 )
	       (MLESSP  m_Lt 2 )
	       (MQUOTIENT  m_div 2 )
					;(MDEFINE  m_DEFINE 2 )
	       (MNOTEQUAL  m_NE 2 )
	       (MTIMES  m_mult n )
	       (MGEQP  m_GE 1 )
	       (MEQUAL  m_EQ 1 )
					;(MNCEXPT  m_NCEXPT 1 )
	       ;;(BIGFLOAT  m_IGFLOAT 1 )
	       (MMINUS  m_MINUS 1 )
	       (MEXPT  m_power 2 )
					;(MQUOTE  m_QUOTE 1 )
	       (MLEQP  m_LE 1 )
	       (MOR  m_OR 1 )
					;(MNCTIMES  m_NCTIMES n )
					;(MLIST  m_LIST 1 )
					;(MARROW  m_ARROW 1 )
	       (nil m_push)
	       ;; push arg.
	       ( m_push_0)
	       ( m_push_1)
	       (m_push_2)
	       (m_push_3)
	       (m_push_i)
	       ( m_pushl_0)
	       ( m_pushl_1)
	       (m_pushl_2)
	       (m_pushl_3)
	       (m_pushl_i)
	       (m_pushc_0)
	       (m_pushc_1)
	       (m_pushc_2)
	       (m_pushc_3)
	       (m_pushc_4)
	       (m_pushc_5)
	       (m_pushc_6)
	       (m_pushc_7)
	       (m_pushc_8)
	       (m_pushc_9)
	       (m_pushc_i)

	       (m_end)
	       (m_reserve)

	       (m_set_sp)
	       (m_set_sp_frame_pointer)
	       (m_return)
	       ))
(defmacro op-number (x)
  (let ((tem (assoc x *q-ops*)))
    (or tem  (error "unrecognized op ~a" x))
    (position tem *q-ops*)))

(defun write-execute ( &aux x)
  (with-open-file (st "execute.c" :direction :output)
    
  (progn (format st
		 "#include \"plot.h\"
#include \"emulate.h\"
#include \"const.h\"                    
struct value_function
{ char * body ;
  struct value *constants;
};
extern int s_p;
extern struct value stack[];

enum m_ops {~%")
	 (sloop for v in *q-ops* do (format st "~(~a~),~%"  (or (second v) (car v))))
	 (format st "};~%"))
  (format st "execute_fun(f,n)
     struct value_function *f;
     int n;  
{ unsigned char *body;
/* frame_pointer points just beyond  last frames valid storage */
  struct value *frame_pointer = &stack[s_p -(n -1)];
  struct value *constants = f->constants;
  body = f->body;
  switch (*body)
    {
")
  (sloop for v in *q-ops* do (setq x (or (second v) (car v)))
     (format st "case ~(~a~): ~(f_~a~)(); break;~%"  x
	     (subseq (symbol-name x) 2)))
  (format st " default:
      abort();
    }
 END: s_p = frame_pointer -stack -n;
 pop(frame_pointer);
 s_p = frame_pointer -stack -1;
 return 1;
}")))

(defvar *max-locals* 0)
(defvar *output* nil)
(defvar *constants* (make-array 4 :fill-pointer 0))
(defun push-constant (x)
  (let ((tem (position x *constants* :test 'eql)))
    (unless tem (setq tem (fill-pointer *constants*))
	    (vector-push-extend x *constants*))
    (cond ((<= tem 9)
	   (q-push (+ tem (op-number m_pushc_0)) 'constant))
	  (t 
	     (q-push  (op-number m_pushc_i) 'constant)
	     (q-push tem 'index)
	     ))))

(defun compile-expr (expr arglist recursive locals push-result &aux tem n)
  (let ((*output* (if recursive *output*
		    (cons
		     (make-array 10 :element-type 'fixnum
				:fill-pointer 0)
		     (make-array 10 :element-type 'long-float
				:fill-pointer 0)))))
    (cond ( (not recursive)
	    (setq *max-locals* 0)
	    (setf (fill-pointer *constants* ) 0))
	  (t
	   (setq *max-locals* (max *max-locals* (length locals)))))
  (cond ((atom expr)
	 (cond (push-result
		(cond
		 ((setq tem (position expr locals))
		  (setq tem (- (length locals) tem))
		       (cond ((<= tem 3)
			      (q-push
			       (+ tem (op-number m_pushl_0))))
			     (t (q-push (op-number m_pushl_i *))
				(q-push tem 'index))))
		 ((setq tem (position expr arglist))
		       (cond ((<= tem 3)
			      (q-push
			       (+ tem (op-number m_push_0))))
			     (t (q-push (op-number m_push_i ))
				(q-push tem 'index))))

		 ((numberp expr)
		       (push-constant expr))
		 (t (error "unrecognized constant: ~s" expr))))))
	((eq (caar expr) 'mprog)
	 (let* ((vars (cdr (second expr)))
		(offset (length locals))
		new-locals)
	   (cond ((eql offset 0)
		  (q-push (op-number m_set_sp_frame_pointer )))
		 (t
		  (q-push (op-number m_set_sp ))
		  (q-push offset'index)))
	   (sloop for v in vars 
	      do 
	      (cond ((symbolp v) (push v new-locals)
		     (push-constant 0))
		    ((and (consp v) (eq (caar v) 'msetq)
			  (symbolp (second v)))
		     (push (second v) new-locals)
		     (compile-expr (third v) arglist t locals t))))
	(setq new-locals (append new-locals locals))
	(sloop for v on (cddr expr)
	   do
	   (compile-expr (car v) arglist t new-locals (if (cdr v) nil t)))))
	((eq (caar expr) 'mprogn)
	 (sloop for v on (cdr expr)
	      do
	      (compile-expr (car v) arglist t locals (if (cdr v) nil t))))
	((setq tem (assoc (caar expr) *q-ops*))
	 (setq n (position tem *q-ops*))
	 (dolist (v (cdr expr))
	   (compile-expr v arglist t locals t))
	 (q-push n   'call)
	 (cond ((cdddr expr)
		(cond
		 ((member (caar expr) '(mtimes mplus mminus mquotient))
		  (dolist (v (cdddr expr))
		    (q-push  n   'call)))
		 (t (error "unknown nary op ~a ~a args"
			   (caar expr) (length (cdr expr)))))))))))

(defun q-push  (i &optional (type 'instr))
  (format t "~%(~a) ~a" i
	  (ecase type
	    (call (second (nth i *q-ops*)))
	    (instr  (or (second (nth i *q-ops*)) (car (nth i *q-ops*))))
	    (index 'index)
	    (constant (list
		       (or (second (nth i *q-ops*)) (car (nth i *q-ops*)))
		       (if  (< i (op-number m_pushc_i))
			   (aref *constants*
				 (- i (op-number m_pushc_0)))))))))





	
			     
	 