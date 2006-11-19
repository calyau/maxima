; Copyright 2006 by Robert Dodier
; Released under the terms of the GNU General Public License, version 2

; Example 1:
; This is the same FOM as computed by FGCOMPUTE in the program sdrive.f
#|
load (lbfgs);
t1[j] := 1 - u[j];
t2[j] := 10*(u[j + 1] - u[j]^2);
n : 8;
FOM : sum (t1[2*j - 1]^2 + t2[2*j - 1]^2, j, 1, n/2);
lbfgs_nfeval_max : 100;
lbfgs_ncorrections : 25;
lbfgs (FOM, '[u[1], u[2], u[3], u[4], u[5], u[6], u[7], u[8]],
       [-1.2, 1, -1.2, 1, -1.2, 1, -1.2, 1], 1e-4, [1, 3]);

  => [u[1] = 1.00000047321587,u[2] = 1.000000931806471,
      u[3] = 1.00000047321587,u[4] = 1.000000931806471,
      u[5] = 1.00000047321587,u[6] = 1.000000931806471,
      u[7] = 1.00000047321587,u[8] = 1.000000931806471]$

|#

; Example 2:
; This computes the least-squares fit to a function A/(1 + exp(-B * (x - C)))
#|
load (lbfgs);
FOM : '((1/length(X))*sum((F(X[i]) - Y[i])^2, i, 1, length(X)));
X : [1, 2, 3, 4, 5];
Y : [0, 0.5, 1, 1.25, 1.5];
F(x) := A/(1 + exp(-B*(x - C)));
''FOM;
estimates : lbfgs (FOM, '[A, B, C], [1, 1, 1], 1e-4, [1, 3]);
plot2d ([F(x), [discrete, X, Y]], [x, -1, 6]), ''estimates;
|#
(in-package :maxima)

(defmvar $lbfgs_nfeval_max 100)
(defmvar $lbfgs_ncorrections 25)

; --------------- COERCE-FLOAT-FUN SHOULD GO INTO SRC/PLOT.LISP ---------------
(defun coerce-float-fun (expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
	 expr)
	((and (symbolp expr) (not (member expr lvars)))
	 (cond ((fboundp expr) expr)
	       (t
		(let* ((mexpr (mget expr 'mexpr))
		       (args (nth 1 mexpr)))
		  (or mexpr (merror "Undefined function ~M" expr))
		  (coerce `(lambda ,(cdr args)
			    (declare (special ,@(cdr args)))
			    (let* (($ratprint nil) ($numer t)
				   (result ($realpart (meval* ',(nth 2 mexpr)))))
			      (if ($numberp result)
				  ($float result)
				  nil)))
			  'function)))))
    ((and (consp expr) (eq (caar expr) 'lambda))
     ; FOLLOWING CODE IS IDENTICAL TO CODE FOR EXPR = SYMBOL 
     ; (EXCEPT HERE WE HAVE EXPR INSTEAD OF MEXPR). DOUBTLESS BEST TO MERGE.
		(let ((args (nth 1 expr)))
		  (coerce `(lambda ,(cdr args)
			    (declare (special ,@(cdr args)))
			    (let* (($ratprint nil) ($numer t)
				   (result ($realpart (meval* ',(nth 2 expr)))))
			      (if ($numberp result)
				  ($float result)
				  nil)))
			  'function)))
	(t
	 (let* ((vars (or lvars ($sort ($listofvars expr))))
            (subscripted-vars ($sublist vars '((lambda) ((mlist) $x) ((mnot) (($atom) $x)))))
            gensym-vars save-list-gensym subscripted-vars-save
            subscripted-vars-mset subscripted-vars-restore)

       ; VARS and SUBSCRIPTED-VARS are Maxima lists.
       ; Other lists are Lisp lists.
       (when (cdr subscripted-vars)
         (setq gensym-vars (mapcar #'(lambda (x) (gensym)) (cdr subscripted-vars)))
         (mapcar #'(lambda (a b) (setq vars (subst b a vars :test 'equal))) (cdr subscripted-vars) gensym-vars)

         ; This stuff about saving and restoring array variables should go into MBINDING,
         ; and the lambda expression constructed below should call MBINDING.
         ; (At present MBINDING barfs on array variables.)
         (setq save-list-gensym (gensym))
         (setq subscripted-vars-save
               (mapcar #'(lambda (a) `(push (meval ',a) ,save-list-gensym))
                       (cdr subscripted-vars)))
         (setq subscripted-vars-mset
               (mapcar #'(lambda (a b) `(mset ',a ,b))
                       (cdr subscripted-vars) gensym-vars))
         (setq subscripted-vars-restore
               (mapcar #'(lambda (a) `(mset ',a (pop ,save-list-gensym)))
                       (reverse (cdr subscripted-vars)))))

	   (coerce `(lambda ,(cdr vars)
		     (declare (special ,@(cdr vars) errorsw))

             ; Nothing interpolated here when there are no subscripted variables.
             ,@(if save-list-gensym `((declare (special ,save-list-gensym))))

             ; Nothing interpolated here when there are no subscripted variables.
             ,@(if (cdr subscripted-vars)
                 `((progn (setq ,save-list-gensym nil)
                          ,@(append subscripted-vars-save subscripted-vars-mset))))

		     (let (($ratprint nil) ($numer t)
			   (errorsw t))
		       ;; Catch any errors from evaluating the
		       ;; function.  We're assuming that if an error
		       ;; is caught, the result is not a number.  We
		       ;; also assume that for such errors, it's
		       ;; because the function is not defined there,
		       ;; not because of some other maxima error.
		       ;;
		       ;; GCL 2.6.2 has handler-case but not quite ANSI yet. 
		       (let ((result
			      #-gcl
			       (handler-case 
				   (catch 'errorsw
				     ($float ($realpart (meval* ',expr))))
				 (arithmetic-error () t))
			       #+gcl
			       (handler-case 
				   (catch 'errorsw
				     ($float ($realpart (meval* ',expr))))
				 (cl::error () t))
			       ))

                 ; Nothing interpolated here when there are no subscripted variables.
                 ,@(if (cdr subscripted-vars) `((progn ,@subscripted-vars-restore)))

			 result)))
		   'function)))))

(defmfun $lbfgs (FOM-expr x-list x-initial eps iprint-list)

  (if
    (or (and (symbolp FOM-expr) (mfboundp FOM-expr))
        (and (consp FOM-expr) (eq (caar FOM-expr) 'lambda)))
    (merror "lbfgs: figure of merit cannot be a function name or lambda."))

  (let*
    ((n (length (cdr x-list)))
     (m $lbfgs_ncorrections)
     (nwork (+ (* n (+ (* 2 m) 1)) (* 2 m)))

     (FOM-function (coerce-float-fun FOM-expr x-list))
     (FOM-grad-lambda `(lambda (x) (meval (list '($diff) ',FOM-expr x))))
     (FOM-grad-expr `((mlist) ,@(mapcar (coerce FOM-grad-lambda 'function) (cdr x-list))))
     (FOM-grad-function (coerce-float-fun FOM-grad-expr x-list))

     (xtol double-float-epsilon)
     (iflag 0)
      
     (scache (make-array n :element-type 'double-float))
     (w (make-array nwork :element-type 'double-float))
     (diag (make-array n :element-type 'double-float))
     (g (make-array n :element-type 'double-float))
     (x (make-array n :element-type 'double-float))
     (iprint (make-array 2 :element-type 'f2cl-lib:integer4))
     (diagco nil)
     
     (return-value '((mlist)))
     (f 0.0d0))
    
    (setf (aref iprint 0) (nth 1 iprint-list))
    (setf (aref iprint 1) (nth 2 iprint-list))
    (setf diagco f2cl-lib:%false%)

    (let (($numer t)) (setq x-initial ($float (meval x-initial))))
    ; DOUBTLESS THERE IS A MORE ELEGANT WAY TO REFILL AN ARRAY FROM A LIST
    (dotimes (i n) (setf (aref x i) (nth (1+ i) x-initial)))

    (common-lisp-user::/blockdata-lb2/)

    (dotimes (nfeval $lbfgs_nfeval_max)
; (format t "hey nfeval ~S~%" nfeval)
; (format t "hey x ~S~%" x)
      (setf f (apply 'funcall `(,FOM-function ,@(coerce x 'list))))
      (let ((g-list (apply 'funcall `(,FOM-grad-function ,@(coerce x 'list)))))
; (format t "hey g-list ~S~%" g-list)
        ; DOUBTLESS THERE IS A MORE ELEGANT WAY TO REFILL AN ARRAY FROM A LIST
        (dotimes (i n) (setf (aref g i) (nth (1+ i) g-list))))
; (format t "hey f ~S g ~S~%" f g)

      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                  var-8 var-9 var-10 var-11 var-12)
        (common-lisp-user::lbfgs n m x f g diagco diag iprint eps xtol w iflag scache)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                         var-8 var-9 var-10 var-12))
        (setf iflag var-11)
        (cond
          ; MIGHT WANT TO RETURN SCACHE VALUES UNCONDITIONALLY
          ; (RESULT OF MOST RECENT LINE SEARCH IS BETTER THAN NOTHING)
          ((eq iflag 0)
           (setq return-value (append '((mlist)) (mapcar #'(lambda (a b) `((mequal) ,a ,b)) (cdr x-list) (coerce scache 'list))))
           (return)))))

    return-value))
