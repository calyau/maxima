(in-package "MAXIMA")
;; see example of specification at end of this file.

;;for maxima  g(t,u) == (($g simp) $t $u)
;; (coerce-call-lisp '(($g simp) $t $u)) --> ($g $t $u)
(defun coerce-call-lisp (fcall)
  (cond ((atom fcall) fcall)
	((and (consp fcall)
	      (consp (car fcall)))
	 (cond ((eq (caar fcall) 'mlist)
		(mapcar 'coerce-call-lisp (cdr fcall)))
	       (t (cons (caar fcall) (cdr fcall)))))
	(t fcall)))

(defvar *fortran-types*)
(setq  *fortran-types* '(($integer . "int")
			  ($double . "double")
			  ($dimension . "dimension")
			  ($external . "void *")
			  
			  ))

(defun get-fortran-type (type)
  (or (cdr (assoc type *fortran-types*)) (error "unrecognized type ~a " type)))

;; if true then 
(defvar $fortran_force_new_compile nil)
(defvar $fortran_path '((mlist) "./" "/usr/local/lib/linpack/"))



(defmacro $make_fortran (fun declarations &rest l &aux path tem locals)
  (let ((keys (parse_keys l '($defaults $result $assertions $locals $requires_subroutines
					$libs)
			  nil nil)))
    (setq fun (coerce-call-lisp fun))
    (setq declarations  (coerce-call-lisp declarations))
    
    (let ((result  (coerce-call-lisp (cadr (assoc '$result keys))))
	  (defaults  (cadr (assoc '$defaults keys)))
	  (assertions  (cadr (assoc '$assertions keys)))
	  (locals  (cadr (assoc '$locals keys)))
	  (name  (with-output-to-string  (*fortran-out*)(wt (car fun))))
	  (requires_subroutines (cadr (assoc '$requires_subroutines)))
	  (libs (if (assoc '$libs)
			     (coerce (mstring(cadr (assoc '$libs))) 'string)
			   "-llinpack -lblass")
			   
			   )
	  )
      
      (when
       (or $fortran_force_new_compile
	   (not 
	    (dolist (v (cdr  $fortran_path))
		    (if (probe-file (setq tem (si::string-concatenate v name
								      ".exe")))
			(return (setq path (namestring (truename tem))))))))
				 
       (make-fortran-fun fun declarations result name locals)	 
					;
       (compiler::safe-system
	(si::string-concatenate
	 "cc -c -O " name".c ; "
	 "cc  -L/usr/local/lib " libs
	 " -L/usr/lang/SC1.0 -lF77 -lm fony.o -o "    name".exe " name ".o"  
					;";rm -f "  name ".o " name ".c "
	 ))
       (setq path (namestring
		   (truename (si::string-concatenate name ".exe")))))
      (define-maxima-function fun declarations defaults result path assertions)
      (list 'quote (car fun)))))

(defstruct fortran args requireds decls defaults results exec-path assertions
  subroutines
  libs
  locals)

(defun default-type-value (type) (or (cdr (assoc type '(($integer . 0)
						  ($double . 0.0d0)
						  ($float . 0.0s0))))
			       (error "no default for ~a" type)))

(defun $default_array (type dim1 &optional dim2 &aux res)
  (or (and (typep dim1 'fixnum)
	   (or (null dim2) (typep dim2 'fixnum)))
      (error "dimension not fixnum"))
  (cond ((null dim2) (cons '(mlist)(make-list dim1 :initial-element
					      (default-type-value type))))
	(t `(($matrix),@
	     (dotimes (i dim1 res)
		      (push ($default_array type dim2) res))))))

(defun maxima-defaults (deflt )
      (declare (special *decls*))
   (cond ((and (consp deflt)
	       (consp (car deflt))
	       (eq (caar deflt) 'mequal))
	  deflt)
	 ((and (symbolp deflt)
	       (let* ((type (cdr (get-arg-decl deflt *decls*)))
		      (tem (if (atom type) (default-type-value type)
			     `(($default_array) ,@ type))))
		 (and tem (list '(mequal) deflt tem)))))
	 (t
	  (error "don't know how to give default to ~a" deflt))))
	
(defun define-maxima-function (fun declarations defaults results path
                                   assertions)
  (let (args (*decls* declarations))
    (declare (special *decls*))
    (setq defaults (mapcar 'maxima-defaults (cdr defaults)))
    (dolist (v (cdr fun) (setq args (nreverse args)))
	    (or  (find v defaults :key 'cadr) (push v args)))
    (setf (get (car fun) 'fortran) (make-fortran :args (cdr fun)
					   :requireds args
					   :decls declarations
					   :defaults
					   defaults
					   :results (or results (cdr fun))
					   :exec-path path
                                           :assertions assertions
					   :locals locals
					   ))
    (setf (get (car fun) 'mfexpr*)
	      #'(LAMBDA (FORTRAN-ARG) (MAXIMA-FORTRAN-INVOKE FORTRAN-ARG)))
    ))

(defvar $tmp_prefix nil)
(defun get-temp-path (prefix suffix &aux tem)
  (dotimes (i 10000)
	   (unless (probe-file (setq tem (format nil "~a~a" prefix i suffix)))
		   (return tem))))


(defun link-with-subroutines (f arg-values &aux fun name)
  (let ((subrs (fortran-subroutines f)))
    (sloop for v in (cdr (fortran-subroutines f))
       do (or ($listp v) (error "subr decl should be list ~M" v))
       (setq fun (nth 1 v))
       (or (and (consp fun)
		(symbolp (caar fun)))
	   (merror "bad declaration ~M" v))
       (setq name (caar fun))
       (let* ((keys (parse_keys (cddr v) '($decls) nil nil))
	      (decls (nth 2 (assoc '$decls keys)))
	      (val (nth (position name (fortran-args f)) arg-values)))
	 (cond ((symbolp f)
		(setq def (mget '$ff 'mexpr)))

	       )))))
	      
	 
	 
	 
       
  
(defun maxima-fortran-invoke (arg  &aux arg-values tem io-path exec-path)
  (or $tmp_prefix (setq $tmp_prefix (format nil "/tmp/~a_fort"
					    (si::getenv "USER"))))
  (let* ((f (get (caar arg) 'fortran))
	 (arg-values-c)
	 (default-pos
	   (+ (length (fortran-requireds f)) 1))
	 exec-path
	 
	 (defaults
	   (cond ((eql (length arg) default-pos)
		  (fortran-defaults f))
		 ((eql (length arg) (+ 1 default-pos))
		  (or ($listp (nth default-pos arg))
		      (error "defaults not a list"))
		  (prog1 
		      (append (mapcar 'maxima-defaults
				      (cdr(nth default-pos arg)))
			      (fortran-defaults f))
		    (setq arg (butlast arg))))
		 (t (error "~a needs ~a or ~a args" (caar arg)(- default-pos 1)
			   (- default-pos 2))))))
    (progv (fortran-requireds f)
	   (mapcar 'meval* (cdr arg))
	   ;;eval defaults
	   (setq defaults (parse_keys defaults (Fortran-args f)
				       t nil))
	   (dolist (v (fortran-args f))
		   (if (setq tem (assoc v defaults))
		       (push (list v (second tem)) arg-values)
		     (push (list v (symbol-value v)) arg-values))))
    (setq arg-values (nreverse	   arg-values))
    (setq exec-path
	  (if (fortran-subroutines f)
	      (link-with-subroutines f arg-values )
	    (fortran-exec-path f)))
     
    (with-open-file (st (setq io-path (get-temp-path $tmp_prefix "Fw"))
			:direction :output)
      (let ((xdr (si::xdr-open st)))
	(dolist (v arg-values)
		(push (maxima-coerce-to-xdr
		       (second v)
		       (cdr (get-arg-decl (car v)
					  (fortran-decls f))))
		      arg-values-c)
		(si::xdr-write xdr (car arg-values-c)))))
    (setq arg-values-c (nreverse arg-values-c))
    (system
     (format nil " ~a <  ~a > ~ao ; rm -f ~a"
	     exec-path
	     io-path io-path io-path 
	     ))
   (unwind-protect
    (with-open-file (st (setq io-path (concatenate 'string io-path "o")))
      (let ((xdr (si::xdr-open st))
	    (pairs (pairlis (fortran-args f) arg-values-c))
	    result)
	(dolist (v (fortran-results f))
		(push (si::xdr-read xdr (cdr (assoc v pairs))) result))
	(setq result (nreverse result))
	(cons '(mlist)
	      (sloop::sloop for v in result
			    for na in (fortran-results f)
			    collect
			    (coerce-maxima-from-xdr v
						    (nth
						     (position na
							     (fortran-args f))
							 arg-values))))
	
	))
    (delete-file io-path)

    )))

#+when-run-process-deallocates
(defun maxima-fortran-invoke (arg &aux arg-values tem)
  (let* ((f (get (caar arg) 'fortran))
	 (arg-values-c)
	 (default-pos
	   (+ (length (fortran-requireds f)) 1))
	 (defaults
	   (cond ((eql (length arg) default-pos)
		  (fortran-defaults f))
		 ((eql (length arg) (+ 1 default-pos))
		  (or ($listp (nth default-pos arg))
		      (error "defaults not a list"))
		  (prog1 
		      (append (mapcar 'maxima-defaults
				      (cdr(nth default-pos arg)))
			      (fortran-defaults f))
		    (setq arg (butlast arg))))
		 (t (error "~a needs ~a or ~a args" (caar arg)(- default-pos 1)
			   (- default-pos 2))))))
    (progv (fortran-requireds f)
	   (mapcar 'meval* (cdr arg))
	   ;;eval defaults
	   (setq defaults (parse_keys defaults (Fortran-args f)
				      t nil))
	   (dolist (v (fortran-args f))
		   (if (setq tem (assoc v defaults))
		       (push (list v (second tem)) arg-values)
		     (push (list v (symbol-value v)) arg-values))))
    (setq arg-values (nreverse	   arg-values))
    (let* ((tem (si::run-process (fortran-exec-path f) nil))
	   (out (si::fp-output-stream tem))
	   (in (si::fp-input-stream tem)))
      (let ((xdr (si::xdr-open out)))
	(dolist (v arg-values)
		(push (maxima-coerce-to-xdr (second v)
					    (cdr (get-arg-decl (car v)
							       (fortran-decls f))))
		      arg-values-c)
	    	
		(si::xdr-write xdr (car arg-values-c))))
    (setq arg-values-c (nreverse arg-values-c))
    ;; invoked
    (force-output out)
    (let ((xdr (si::xdr-open in))
	  (pairs (pairlis (fortran-args f) arg-values-c))
	  result)
      (dolist (v (fortran-results f))
	      (push (si::xdr-read xdr (cdr (assoc v pairs))) result))
      (setq result (nreverse result))
      (cons '(mlist)
	    (sloop::sloop for v in result
			  for na in (fortran-results f)
			  collect
			  (coerce-maxima-from-xdr v
						  (nth (position na
								 (fortran-args f))
						       arg-values))))))))

(defun meval-* (lis &aux tem)
  (cond ((null lis) lis)
	(t (progv (list (caar lis))
		  (list (setq tem (meval* (second (car lis)))))
		  (cons (list (caar lis) tem)
			(meval-* (cdr lis)))))))


;; return an list of  (list keys  value)
;; where each value is meval* in with previous keys bound to
;; previous values.
(defun parse_keys (list keys meval done &aux sym tem)
  (cond ((null list) done)
	((and (consp list)
	      (consp (setq tem (car list)))
	      (consp (car tem))
	      (eq (caar tem) 'mequal)
	      (or (eq keys '$allow_any_key)
		  (and (consp keys)
		       (member (setq sym (second tem)) keys)))
	      (symbolp sym))
	 (cond ((assoc sym done)
		(parse_keys (cdr list) keys meval done))
	       (t
		(progv (list sym)
		       (list 
			(setq tem (if meval (meval* (third (car list)))
				    (third (car list)))))
		       (setq done (nconc done (list (list sym tem))))
		       (parse_keys (cdr list) keys meval done)))))
	((error "unrecognized key = ~a" (or  sym tem)))))


(defvar *lisp-types* '(($double . long-float) ($integer . fixnum)
		       ($single . short-float)))

(defun maxima-coerce-to-xdr (v decl)
  (case decl
    ($integer v)
    ($double (coerce v 'long-float))
    ($single (coerce v 'short-float))
    ($external (coerce v 'fixnum))
    (t (cond  ((consp decl)
	       (let* ((dim (cond ((atom (second v)) ($length v))
				 (t (* ($length v) ($length (second v))))))
		      (ar (lisp::make-array dim :static t
			      :element-type (cdr (assoc (car decl)
							*lisp-types*)))))
		 (or (numberp (aref ar 0)) (error "bad array type"))
		 (maxima-flatten ($transpose v) ar 0)
		 ar))
	      (t (error "unknown type"))))))

(defun maxima-flatten (v ar i)
  (declare (fixnum i))
  (cond ((mbagp v)
	 (cond ((mbagp (second v))
		(dolist (w (cdr v))
			(setq i (maxima-flatten w ar i))))
	       (t (dolist (u (cdr v))
			  (setf (aref ar i) u)
			  (incf i))
		  i)))
	(t (error "bad elt"))))

(defun coerce-maxima-from-xdr (v elt)
  (setq elt (second elt))
  (cond ((numberp v)
	 v)
	((arrayp v)
	 (cond ((and ($listp elt) (numberp (second elt)))
		(cons '(mlist) (coerce v 'list)))
	       (($matrixp elt)
		(let ((n (length (cdr elt)))
			 ans)
		  (setq ans
			(cons '($matrix)
			(sloop for i below (floor (length v) n)
			 with ind = 0
			 declare (fixnum ind)
			 collect
			 (cons '(mlist)
			       (sloop for j below n
				      collect (aref v ind)
				      do (incf ind))))))
		  (setq ans ($transpose ans))))
	       (t (error "unkown type"))))))
			
			 
			

;; variables to forget from names
(defvar *forget* '(#\$))
(defvar *fortran-out* nil)
(defun wt1 (x &aux (ch #\a))
  (declare (character ch))
  (cond ((symbolp x)
	 (dotimes (i (length (symbol-name x)))
            (declare (fixnum i))
	    (setq ch (schar (symbol-name x) i))
	    (if (member ch *forget*) nil
	      (princ (char-downcase ch) *fortran-out*))))
	((stringp x)
	 (princ x *fortran-out*))
	(t (princ x *fortran-out*))))

(defmacro wt (&rest l &aux lis)
  `(progn ,@ (dolist (v l (nreverse lis)) (push `(wt1 ,v) lis))))

(defmacro wt-nl (&rest l)
  `(progn (terpri *fortran-out*)(wt1 "      ") (wt ,@l)))


(defun get-arg-decl (arg decls &aux size tem)
  (cond ((setq tem (assoc '$dimension decls))
	 (dolist (v (cdr tem))
	   (when (and (consp v) (eq (car v) arg))
	     (setq size (cdr v))
	     (return nil)))))
  (dolist (v decls)
    (dolist (u (cdr v))
	 (cond ((atom u)
		(if (eq u arg) (return-from get-arg-decl
					    (cons arg
						  (if size
						      (cons (car v) size)
						    (car v))))))
	       ((eq (car  u) arg)
		(return-from  get-arg-decl(cons arg (cons (car v) (cdr u))))))))
  (error "undeclared arg ~a" arg))

(defun xdr-fun (type)
  (if (consp type) "array"
    (or (cdr (assoc type '(($integer . "int")
			   ($double . "double")
			   ($complex . "double")
			   ($single . "float")
			   ($external . "int")
			   )))
	(error "unknown type ~a" type))))
				 
(defun wt-xdr (v stream)
  (wt-nl "CHECK(xdr_"  (xdr-fun (cdr v))"(" stream ",")
  (wt "&" (car v))
  (cond ((consp (cdr v))
	 (wt ",&" (car v)"_length, MAX_ARRAY("(car v)"_length),")
	 (wt-nl "  sizeof(" (get-fortran-type (second v)) "),xdr_"
		(xdr-fun (cadr v)))))
  (wt "));"))


(defun make-fortran-fun (fun+args decls  results name locals)
  (let ((file (concatenate 'string name ".c"))
	(args (cdr fun+args)))
    (or results (setq results (cdr fun+args)))
    (with-open-file (*fortran-out* file :direction :output)
      (wt "
#include <stdio.h>
#ifdef AIX3
#include <sys/select.h>
#endif
#include <rpc/rpc.h>
#define MAX_ARRAY(x) (x ? x :  20000)
#define CHECK(x) if (!x) {fprintf(stderr,\"xdr failed\"); exit(1);}
")
      
      (wt-nl)
      (wt "main()
 {XDR xdrs;
  int invoked=0;
  xdrstdio_create(&xdrs, stdin, XDR_DECODE);
  {")
      (let (res)(dolist (v args (setq args (nreverse res)))
			(push (get-arg-decl v decls) res)))
      (dolist (v args)
	      (cond ((atom (cdr v)) (wt-nl  (get-fortran-type(cdr v)) " " (car v)  ";"))
		    (t (wt-nl (get-fortran-type(cadr v)) " *" (car v) "= 0 ;")
		       (wt-nl "u_int " (car v) "_length = 0;"))))
      
      (wt-nl "xdrstdio_create(&xdrs, stdin, XDR_DECODE);")
      (terpri *fortran-out*)
      (wt "DO_ARGS:")
      (dolist (v args)
	      (wt-xdr v "&xdrs")
	      (unless (atom (cdr v))
		      (if (and (numberp (third v))
			       (null (cdddr v)))
			  (wt-nl "if ("(car v)"_length != " (third v)")"
				 "fprintf(stderr,\"Wrong length for " (car v)" \");")
			)))
      (if (eq results (cdr fun+args))
	  (wt-nl "if (invoked) exit(0);"))
      (dolist (v (cdr (assoc '$external decls)))
	(wt-nl "{extern int "v "_ext_();" v "="v "_ext_;}"))

      ;; call the routine:
      (wt-nl "/* invoke the function */
 ")
      (wt-nl  (car fun+args) "_" "(")
      (do ((v args (cdr v)))
	  ((null v))
	  (or (consp (cdar v))
	      (wt "&"))
	  (wt (caar v))
	  (if (cdr v) (wt ",")))
      (wt ");
   ")
      (wt-nl "/* write the results out */")
      (wt-nl "xdrstdio_create(&xdrs, stdout, XDR_ENCODE);")
      (cond ((equal results (cdr fun+args))
	     (wt-nl "invoked=1 ; goto DO_ARGS;}}"))
	    (t
	     (dolist (v results)
	     (wt-xdr (get-arg-decl v decls) "&xdrs"))
	    (wt-nl "exit(0);}}"))))))
#+debug
(defun try-xdr (lis)
  (with-open-file (st "joe" :direction :output)
    (let ((xdrs (si::xdr-open st)))
      (dolist (v lis) (si::xdr-write xdrs v))))
  (with-open-file (st "joe" )
    (let ((xdrs (si::xdr-open st)))
      (sloop::sloop for w in lis collect (si::xdr-read xdrs w)))))
  
#|
/* example of specification of routine */
make_fortran(dtrdi(t,ldt,n,det,job,info),
                   [[ integer ,ldt,n,job,info],
                     [double, t(ldt,1),det(2)]],
                    defaults=
                       [ldt=length(t[1]),
                        det, info,
                        n = ldt,
                        job = 100 + if (t[1,2]=0) then 10  else 11],
                   result=[t,det,info]);

make_fortran(dgedi(a,lda,n,ipvt,det,work,job),
  [ [integer, lda,n,ipvt(n),job],
  [ double , a(lda,1),det(2),work(n)]],
  defaults=[lda=length(a),n=length(a[1]),ipvt=dgeco(a)[2],det,job=11,work],
  result=[a,det,ipvt])$

make_fortran(dgeco(a,lda,n,ipvt,rcond,z),
  [[  integer, lda,n,ipvt(n)],
   [   double , a(lda,1),z(n)],
    [  double, rcond]],
  defaults=[lda=length(a),n=length(a[1]),ipvt,rcond=0.0,z],
  result=[a,ipvt,rcond,z])


>(run) 
Maxima 4.130 Sun Nov 12 15:59:51 CST 1989 (with enhancements by W. Schelter). 
(C1) make_fortran(dtrdi(t,ldt,n,det,job,info), 
                   [[ integer ,ldt,n,job,info], 
                     [double, t(ldt,1),det(2)]], 
                    defaults= 
                       [ldt=length(t[1]), 
                        det, info, 
                        n = ldt, 
                        job = 100 + if (t[1,2]=0) then 10  else 11], 
                   result=[t,det,info]); 
 
(D1)                                 DTRDI 
(C2) dtrdi(matrix([11.00,12.0],[0.0,13.0]));  
 
      [ 0.090909090909090912  - 0.083916083916083919 ] 
(D2) [[                                              ],  
      [         0.0            0.076923076923076927  ] 
 
                                                  [1.4300000000000002, 2.0], 0] 


|#
      
  
  
  
  