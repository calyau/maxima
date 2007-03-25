;;Copyright William F. Schelter 1990, All Rights Reserved
(in-package :maxima)
;; see bottom of file for examples

(defmfun $join (x y)
  (if (and ($listp x) ($listp y))
      (cons '(mlist) (loop for w in (cdr x) for u in (cdr y)
                       collect w collect u))
    (merror "Both arguments to 'join' must be lists")))


;; I (RLT) don't think we should compile this with safety = 0.

;;(eval-when (compile) (proclaim '(optimize (safety 0))))


(eval-when (compile eval load)
  (defmacro coerce-float (x)
   `(cl:float (meval* ,x) 1.d0))
  )


(defvar *maxima-plotdir* "")
(defvar *maxima-tempdir*)
(defvar *maxima-prefix*)

(defvar *z-range* nil)
(defvar *original-points* nil)
(defvar $axes_length 4.0)
(defvar *rot* (make-array 9 :element-type 'double-float))
(defvar $rot nil)

(defvar $plot_options `((mlist)
                        ;; Make the default range on X large.  This
                        ;; doesn't impact 2-D plotting, but is useful
                        ;; for parametric plots so that the plots
                        ;; don't get prematurely clipped.
                        ((mlist) $x #.(- (/ most-positive-double-float 1024))
                         #.(/ most-positive-double-float 1024))
                        ;; Make the default range on Y large.  Don't
                        ;; use most-positive-double-float because this
                        ;; causes overflow in the draw2d routine.
                        ((mlist) $y #.(- (/ most-positive-double-float 1024))
                         #.(/ most-positive-double-float 1024))
                        ((mlist) $t -3 3)
                        ((mlist) $grid 30 30)
                        ((mlist) $transform_xy nil)
                        ((mlist) $run_viewer t)
                        ((mlist) $plot_format
			 ,(if (string= *autoconf-win32* "true")
			      '$gnuplot
			      '$gnuplot_pipes))
                        ((mlist) $gnuplot_term $default)
                        ((mlist) $gnuplot_out_file nil)
                        ;; With adaptive plotting, 100 is probably too
                        ;; many ticks.  I (rtoy) think 10 is a more
                        ;; reasonable default.
                        ((mlist) $nticks 10)
                        ;; Controls the number of splittings
                        ;; adaptive-plotting will do.
                        ((mlist) $adapt_depth 10)
                        ((mlist) $gnuplot_pm3d
			 ,(if (string= *autoconf-win32* "true") t nil))
                        ((mlist) $gnuplot_preamble "")
                        ((mlist) $gnuplot_curve_titles 
                         ((mlist) $default))
                        ((mlist) $gnuplot_curve_styles
                         ((mlist) 
                          "with lines 3"
                          "with lines 1"
                          "with lines 2"
                          "with lines 5"
                          "with lines 4"
                          "with lines 6"
                          "with lines 7"))
                        ((mlist) $gnuplot_default_term_command "")
                        ((mlist) $gnuplot_dumb_term_command
                         "set term dumb 79 22")
                        ((mlist) $gnuplot_ps_term_command
                         "set size 1.5, 1.5;set term postscript eps enhanced color solid 24")
			((mlist) $gnuplot_pipes_term "x11")
                        ((mlist) $logx nil)
                        ((mlist) $logy nil)
                        ((mlist) $plot_realpart nil)
                        ))

;; $plot_realpart option is false by default but *plot-realpart* is true because coerce-float-fun
;; is used outside of plot package too.
(defvar *plot-realpart* t)

(defun maybe-realpart (x)
  (if *plot-realpart*
      ($realpart x)
      (if (eq 0 ($imagpart x))
          x
          nil)))


(defvar *gnuplot-stream* nil)
(defvar *gnuplot-command* "")

(defun start-gnuplot-process (path)
  #+clisp (setq *gnuplot-stream*
		(ext:make-pipe-output-stream path))
  #+cmu (setq *gnuplot-stream*
	      (ext:process-input (ext:run-program path nil :input :stream
						  :output nil :wait nil)))
  #+sbcl (setq *gnuplot-stream*
	       (sb-ext:process-input (sb-ext:run-program path nil
							 :input :stream
							 :output nil :wait nil
							 :search t)))
;;  #+gcl (setq *gnuplot-stream*
;;	      (si::fp-output-stream (si:run-process path nil)))
  #+gcl (setq *gnuplot-stream*
	      (open (concatenate 'string "| " path) :direction :output))
  #-(or clisp cmu sbcl gcl)
  (merror "Gnuplot not supported with your lisp!")
  
  ;; set mouse must be the first command send to gnuplot
  (send-gnuplot-command "set mouse"))

(defun check-gnuplot-process ()
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command)))

(defun $gnuplot_close ()
  (stop-gnuplot-process)
  "")

(defun $gnuplot_start ()
  (check-gnuplot-process)
  "")

(defun $gnuplot_restart ()
  ($gnuplot_close)
  ($gnuplot_start))

(defun stop-gnuplot-process ()
  (unless (null *gnuplot-stream*)
      (progn
	(close *gnuplot-stream*)
	(setq *gnuplot-stream* nil))))

(defun send-gnuplot-command (command)
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command))
  (format *gnuplot-stream* "~a ~%" command)
  (force-output *gnuplot-stream*))

(defun $gnuplot_reset ()
  (send-gnuplot-command "unset output")
  (send-gnuplot-command (format nil "set term ~a" 
				(get-plot-option-string '$gnuplot_pipes_term)))
  (send-gnuplot-command "reset"))

(defun $gnuplot_replot (&optional s)
  (if (null *gnuplot-stream*)
      (merror "Gnuplot is not running."))
  (cond ((null s)
	 (send-gnuplot-command "replot"))
	((mstringp s)
	 (send-gnuplot-command ($sconcat s))
	 (send-gnuplot-command "replot"))
	(t
	 (merror "Input to 'gnuplot_replot' is not a string!")))
  "")

;; allow this to be set in a system init file (sys-init.lsp)

(defun $get_plot_option (name &optional n)
  (loop for v in (cdr $plot_options)
         when (eq (nth 1 v) name) do
         (return (if n (nth n  v) v))))

(defun get-plot-option-string (option &optional (index 1))
  (let* ((val ($get_plot_option option 2))
         (val-list (if ($listp val)
                       (cdr val)
                       `(,val))))
    (print-invert-case 
     (stripdollar (nth (mod (- index 1) (length val-list)) val-list)))))

(defun check-list-items (name lis type length)
  (or (eql (length lis) length)
      (merror "~M items were expected in the ~M list" length name))
  `((mlist) , name ,@
    (loop for v in lis
     do (setq v (meval* v))
     when (not (typep v type))
     do
     (merror "~M is not of the right type in ~M list" v name)
     collect v)
    ))

(defun $set_plot_option ( value)
  (setq $plot_options ($copylist $plot_options))
  (unless (and  ($listp value)
                (symbolp (setq name (nth 1 value))))
    (merror "~M is not a plot option.  Must be [symbol,..data]" value))
  (setq value
        (case name
          (($x $y $t) (check-list-items name (cddr value) 'number 2)
           (check-range value)
           )
          ($grid  (check-list-items name (cddr value) 'fixnum 2))
          ($nticks  (check-list-items name (cddr value) 'fixnum 1))
          (($run_viewer $transform_xy $gnuplot_pm3d)
           (check-list-items name (cddr value) 't 1))
          ($plot_format (or (member (nth 2 value)
				    (if (string= *autoconf-win32* "true")
					'($zic $geomview
					      $gnuplot
					      $mgnuplot
					      $openmath
					      )
					'($zic $geomview
					      $gnuplot
					      $gnuplot_pipes
					      $mgnuplot
					      $openmath
					      )
					))
			    (merror "plot_format: only [gnuplot,mgnuplot,openmath,geomview] are available"))
                        value)
          ($gnuplot_term (or (symbolp (nth 2 value)) (stringp (nth 2 value))
                             (merror "gnuplot_term: must be symbol or string"))
	                value)
          ($gnuplot_out_file value)
          ($gnuplot_curve_titles (if ($listp value)
                                     value
                                     `((mlist) ,value)))
          ($gnuplot_curve_styles (if ($listp value)
                                     value
                                     `((mlist) ,value)))
          ($gnuplot_preamble value)
          ($gnuplot_default_term_command value)
          ($gnuplot_dumb_term_command value)
          ($gnuplot_ps_term_command value)
          ($adapt_depth (check-list-items name (cddr value) 'fixnum 1))
          ($logx value)
          ($logy value)
          ($plot_realpart value)
	  ($gnuplot_pipes_term value)
          (t
           (merror "Unknown plot option specified:  ~M" name))))
  (loop for v on (cdr $plot_options)
         when (eq (nth 1 (car v)) name)
         do (setf (car v) value))
  $plot_options
  )

(defun get-gnuplot-term (term)
  (let* ((sterm (string-downcase (format nil "~a" (stripdollar term))))
	 (pos   (search " " sterm)))
    (if pos  
      (subseq sterm 0 pos)
      sterm)))
  
(defvar $pstream nil)

(defun print-pt1 (f str)
  (format str "~g " f))

(defmacro defbinop (name op type)
  `(progn
    (defun ,name (x y) (the ,type (,op  (the ,type x) (the ,type y))))
    (eval-when (compile eval)     
      (#+kcl si::define-compiler-macro
             #+(and :ansi-cl (not kcl))
             define-compiler-macro
             #-(or kcl :ansi-cl)
             defmacro ,name (x y)
             `(the ,',type (,',op  (the ,',type ,x) (the ,',type ,y)))))))

(defbinop $+ + double-float)
(defbinop $- - double-float)
(defbinop $* * double-float)
(defbinop $/ / double-float)


(defstruct (polygon (:type list)
                    (:constructor %make-polygon (pts edges))
                    )
  (dummy '($polygon simp))
  pts edges)

(eval-when (compile eval)

  (defmacro f* (a b &optional c)
    (if c `(f* (f* ,a ,b) ,c)
        `(the fixnum (* (the fixnum ,a) (the fixnum ,b)))))

  (defmacro float-< (a b) `(< (the double-float ,a) (the double-float ,b)))
  



  (defmacro z-pt (ar i) `(aref ,ar (the fixnum (+ 2 (* ,i 3)))))
  (defmacro y-pt (ar i) `(aref ,ar (the fixnum (+ 1 (* ,i 3)))))
  (defmacro x-pt (ar i) `(aref ,ar (the fixnum (+ 0 (* ,i 3)))))
  (defmacro rot (m i j) `(aref ,m (the fixnum (+ ,i (the fixnum (* 3 ,j))))))


  (defmacro print-pt (f)
    `(print-pt1 ,f $pstream ))

  (defmacro make-polygon (a b) `(list '($polygon) ,a ,b))
  )



(defun draw3d (f minx maxx miny maxy  nxint nyint)
  (let* ((epsx (/ (- maxx minx) nxint))
         (x 0.0)  ( y 0.0)
         (epsy (/ (- maxy miny) nyint))
         (nx (+ nxint 1))
         (l 0)
         (ny (+ nyint 1))
         (ar (make-array  (+ 12         ; 12  for axes
                             (f* (f* 3 nx) ny))  :fill-pointer (f* (f* 3 nx) ny)
                             :element-type 'double-float
                             :adjustable t
                             )))
    (declare (double-float x y epsy epsx)
             (fixnum nx  ny l)
             (type (cl:array double-float) ar))
    (loop for j below ny
           initially (setq y miny)
           do (setq x minx)
           (loop for i below nx
                  do
                  (setf (x-pt ar l) x)
                  (setf (y-pt ar l) y)
                  (setf (z-pt ar l) (funcall f x y))
                  (incf l)
                  (setq x (+ x epsx))
                  )
           (setq y (+ y epsy)))
    (make-polygon  ar  (make-grid-vertices nxint nyint))))

;; The following is 3x2 = 6 rectangles
;; call (make-vertices 3 2)
;; there are 4x3 = 12 points.
;; ordering is x0,y0,z0,x1,y1,z1,....,x11,y11,z11
;; ----
;; ||||
;; ----
;; ||||
;; ----

(defun make-grid-vertices (nx ny)
  (declare (fixnum nx ny))
  (let* ((tem (make-array (+ 15 (f* 5 nx ny)) :fill-pointer (f* 5 nx ny)
                          :adjustable t
                          :element-type '(mod  65000)))
         (m  nx )
         (nxpt (+ nx 1))
         (i 0)
         )
    (declare (fixnum i nxpt m)
             (type (cl:array (mod 65000)) tem))
    (loop for k below (length tem)
           do
           (setf (aref tem k) i)
           (setf (aref tem (incf k))
                 (+ nxpt i))
           (setf (aref tem (incf k))
                 (+ nxpt (incf i )))
           (setf (aref tem (incf k)) i)
           (setf (aref tem (incf k)) 0) ;place for max
           (setq m (- m 1))
           (cond ((eql  m 0)
                  (setq m nx)
                  (setq i (+ i 1))))
           )
    tem))

(defun add-axes (pts vert)
  (let ((origin (/ (length pts) 3)))
    (loop for i from -3 below 9
           do
           (vector-push-extend  (if (eql 0 (mod i 4)) $axes_length 0.0) pts))
    (loop for i below 15
           do (vector-push-extend
               (if (eql 1 (mod i 5)) (+ origin (ceiling i 5))  origin)
               vert)
           )))

(defun $rotation1 (phi th)
  (let ((sinph (sin phi))
        (cosph (cos phi))
        (sinth (sin th))
        (costh (cos th)))
    `(($matrix simp)
      ((mlist simp) ,(* cosph costh)
       ,(* -1.0 cosph sinth)
       ,sinph)
      ((mlist simp) ,sinth ,costh 0.0)
      ((mlist simp) ,(- (*  sinph costh))
       ,(* sinph sinth)
       ,cosph))))
   
;; pts is a vector of bts [x0,y0,z0,x1,y1,z1,...] and each tuple xi,yi,zi is rotated
;; also the *z-range* is computed.
(defun $rotate_pts(pts rotation-matrix)
  (or ($matrixp rotation-matrix) (error "second arg not matrix"))
  (let* ((rot *rot*)
         (l (length pts))
         (x 0.0) (y 0.0) (z 0.0)
         )
    (declare (double-float  x y z))
    (declare (type (cl:array double-float) rot))
    ($copy_pts rotation-matrix *rot* 0)
        
    ;;    (setf (rot rot  0 0) (* cosphi costh))
    ;;    (setf (rot rot  1 0) (- sinth))
    ;;    (setf (rot rot 2 0) (* costh sinphi))
    ;;
    ;;    (setf (rot rot  0 1) (* cosphi sinth))
    ;;    (setf (rot rot  1 1) costh)
    ;;    (setf (rot rot 2 1) (* sinth sinphi))
    ;;
    ;;    (setf (rot rot  0 2) (- sinphi))
    ;;    (setf (rot rot  1 2) 0.0)
    ;;    (setf (rot rot 2 2) cosphi)

    (loop with j = 0
           while (< j l)
           do
           (setq x (aref pts j))
           (setq y (aref pts (+ j 1)))
           (setq z (aref pts (+ j 2)))
           (loop for i below 3 with a of-type double-float = 0.0
                  do
                  (setq a (* x (aref rot (+ (* 3 i) 0))))
                  (setq a (+ a (* y (aref rot (+ (* 3 i) 1)))))
                  (setq a (+ a (* z (aref rot (+ (* 3 i) 2)))))
                  (setf (aref pts (+ j i )) a))
           (setf j (+ j 3)))))

(defun $rotate_list (x)
  (cond ((and ($listp x) (not (mbagp (nth 1 x))))
         ($list_matrix_entries (ncmul2  $rot x)))
        ((mbagp x) (cons (car x) (mapcar '$rotate_list (cdr x))))))

(defun $get_range (pts k &aux (z 0.0) (max most-negative-double-float) (min most-positive-double-float))
  (declare (double-float z max min))
  (declare (type (vector double-float) pts))
  (loop for i from k below (length pts) by 3
         do (setq z (aref pts i))
         (cond ((< z min) (setq min z)))
         (cond ((> z max) (setq max z))))
  (list min max (- max min)))


;; figure out the rotation to make pt the direction from which we view,
;; and to rotate z axis to vertical.
;; First get v, so p.v=0 then do u= p X v to give image of y axis
;; ans = transpose(matrix( v,u,p))

(defun length-one (pt)
  (flet (($norm (pt) (loop for v in (cdr pt) sum (* v v))))
    (let ((len (sqrt ($norm pt))))
      (cons '(mlist) (loop for v in (cdr pt) collect (/  (float v) len))))))

(defun cross-product (u v)
  (flet ((cp (i j)
           (- (* (nth i u) (nth j v))
              (* (nth i v) (nth j u)))))
    `((mlist) ,(cp 2 3) ,(cp 3 1) ,(cp 1 2))))
        
(defun get-rotation (pt)
  (setq pt (length-one pt))
  (let (v tem u)
    (cond((setq tem (position 0.0 pt))
          (setq v (cons '(mlist) (list 0.0 0.0 0.0)))
          (setf (nth tem v) 1.0))
         (t (setq v (length-one `((mlist) ,(- (nth 2 pt))      , (nth 1 pt) 0.0)))))
    (setq u (cross-product pt v))
    (let* (($rot   `(($matrix) ,v,u,pt))
           (th (get-theta-for-vertical-z
                (nth 3 (nth 1 $rot))
                (nth 3 (nth 2 $rot)))))
      (or (zerop th)
          (setq $rot (ncmul2 ($rotation1 0.0 th)     $rot)))
      $rot)))

(defun get-theta-for-vertical-z (z1 z2)
  (cond ((eql z1 0.0)
         (if (> z2 0.0)
             0.0
             (coerce pi 'double-float)))
        (t
         (cl:atan  z2 z1 ))))

(defun $polar_to_xy (pts &aux (r 0.0) (th 0.0))
  (declare (double-float r th))
  (declare (type (cl:array double-float) pts))
  (assert (typep pts '(vector double-float)))
  (loop for i below (length pts) by 3
         do (setq r (aref pts i))
         (setq th (aref pts (f+ i 1)))
         (setf (aref pts i) (* r (cos th)))
         (setf (aref pts (f+ i 1)) (* r (sin th)))))

;; return a function suitable for the transform function in plot3d.
;; FX, FY, and FZ are functions of three arguments.
(defun $make_transform (lvars fx fy fz  &aux ( $numer t))
  (setq fx (coerce-float-fun fx lvars))
  (setq fy (coerce-float-fun fy lvars))
  (setq fz (coerce-float-fun fz lvars))
  (let ((sym (gensym "transform")))
    (setf (symbol-function sym)
          #'(lambda (pts &aux  (x1 0.0)(x2 0.0)(x3 0.0))
              (declare (double-float  x1 x2 x3))
              (declare (type (cl:array double-float) pts))
              (loop for i below (length pts) by 3
                     do 
                     (setq x1 (aref pts i))
                     (setq x2 (aref pts (f+ i 1)))
                     (setq x3 (aref pts (f+ i 2)))
                     (setf (aref pts i) (funcall fx x1 x2 x3))
                     (setf (aref pts (f+ 1 i)) (funcall fy x1 x2 x3))
                     (setf (aref pts (f+ 2 i)) (funcall fz x1 x2 x3)))))
    ))

; Return value is a Lisp function which evaluates EXPR to a float.
; COERCE-FLOAT-FUN always returns a function and never returns a symbol,
; even if EXPR is a symbol.
;
; Following cases are recognized:
; EXPR is a symbol
;   name of a Lisp function
;   name of a Maxima function
;   name of a DEFMSPEC function
;   name of a Maxima macro
;   a string which is the name of a Maxima operator (e.g., "!")
;   name of a simplifying function
; EXPR is a Maxima lambda expression
; EXPR is a general Maxima expression

(defun coerce-float-fun (expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
         expr)
        ((and (symbolp expr) (not (member expr lvars)))
         (cond
       ((fboundp expr)
        (symbol-function expr))

       ; expr is name of a Maxima function defined by := or define
       ((mget expr 'mexpr)
        (let*
          ((mexpr (mget expr 'mexpr))
           (args (cdr (nth 1 mexpr))))
          (coerce-maxima-function-or-maxima-lambda args expr)))

       ((or
          ; expr is the name of a function defined by defmspec
          (get expr 'mfexpr*)
          ; expr is the name of a Maxima macro defined by ::=
          (mget expr 'mmacro)
          ; expr is a string which names an operator
          ; (e.g. "!" "+" or a user-defined operator)
          (get expr 'opr)
          ; expr is the name of a simplifying function,
          ; and the simplification property is associated with the noun form
          (get ($nounify expr) 'operators)
          ; expr is the name of a simplifying function,
          ; and the simplification property is associated with the verb form
          (get ($verbify expr) 'operators))
        (let ((a (if lvars lvars `((mlist) ,(gensym)))))
          (coerce-float-fun `(($apply) ,expr ,a) a)))
       (t
         (merror "Undefined function ~M" expr))))

    ((and (consp expr) (eq (caar expr) 'lambda))
     (let ((args (cdr (nth 1 expr))))
       (coerce-maxima-function-or-maxima-lambda args expr)))

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

       (coerce
	`(lambda ,(cdr vars)
	   (declare (special ,@(cdr vars) errorsw))

	   ;; Nothing interpolated here when there are no subscripted variables.
	   ,@(if save-list-gensym `((declare (special ,save-list-gensym))))

	   ;; Nothing interpolated here when there are no subscripted variables.
	   ,@(if (cdr subscripted-vars)
		 `((progn (setq ,save-list-gensym nil)
			  ,@(append subscripted-vars-save subscripted-vars-mset))))

	   (let (($ratprint nil) ($numer t)
		 (errorsw t)
		 (errcatch t))
	     (declare (special errcatch))
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
		       ;; Should we just catch all errors here?  It is
		       ;; rather nice to only catch errors we care
		       ;; about and let other errors fall through so
		       ;; that we don't pretend to do something when
		       ;; it is better to let the error through.
		       (arithmetic-error () t)
		       (maxima-$error () t))
		     #+gcl
		     (handler-case 
			 (catch 'errorsw
			   ($float ($realpart (meval* ',expr))))
		       (cl::error () t))
		     ))

	       ;; Nothing interpolated here when there are no subscripted variables.
	       ,@(if (cdr subscripted-vars) `((progn ,@subscripted-vars-restore)))

	       result)))
	'function)))))

(defun coerce-maxima-function-or-maxima-lambda (args expr)
  (let ((gensym-args (loop for x in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let*
           (($ratprint nil)
            ($numer t)
            (result (maybe-realpart (mapply ',expr (list ,@gensym-args) t))))
           (if ($numberp result)
             ($float result)
             result)))
      'function)))

(defmacro zval (points verts i) `(aref ,points (f+ 2 (f* 3 (aref ,verts ,i)))))

;;sort the edges array so that drawing the edges will happen from the back towards
;; the front.   The if n==4 the edges array coming in looks like
;; v1 v2 v3 v4 0 w1 w2 w3 w4 0 ...
;; where vi,wi are indices pointint into the points array specifiying a point
;; in 3 space.   After the sorting is done, the 0 is filled in with the vertex
;; which is closer to us (ie highest z component after rotating towards the user)
;; and this is then they are sorted in groups of 5.   
(defun sort-ngons (points edges n &aux lis )
  (declare (type (cl:array (double-float))  points)
           (type (cl:array (mod 65000)) edges)
           (fixnum n))
  (let ((new (make-array (length edges) :element-type  (array-element-type edges)))
        (i 0)
        (z 0.0)
        (z1 0.0)
        (n1 (- n 1))
        (at 0)
        (leng (length edges))
        )
    (declare (type (cl:array (mod 65000)) new)
             (fixnum i leng n1 at )
             )
    (declare (double-float z z1))
    
    (setq lis
          (loop  for i0 below leng by (+ n 1)
                  do 
                  (setq i i0)
                  (setq at 0)
                  (setq z (zval points edges i))
                  (setq i (+ i 1))
                  (loop for j below n1
                         do (if (> (setq z1 (zval points edges i))  z)
                                (setq z z1 at (aref edges i) ))
                         (setq i (+ i 1))
                         )
                  (setf (aref edges i) at)
                  collect (cons z i0)))
    (setq lis (sortcar lis))
    (setq i 0)
    (loop for v in lis
           do
           (loop for j from (cdr v) 
                  for k to n
                  do (setf (aref new i) (aref edges j))
                  (incf i))
           )
    (copy-array-portion edges new  0 0 (length edges))
    ))

(defun copy-array-portion (ar1 ar2 i1 i2 n1)
  (declare (fixnum i1 i2 n1))
  (loop while (>= (setq n1 (- n1 1)) 0)
         do (setf (aref ar1 i1) (aref ar2 i2))
         (setq i1 (+ i1 1))
         (setq i2 (+ i2 1))))


(defun $concat_polygons (pl1 pl2 &aux tem new)
  (setq new
        (loop for v in pl1 
               for w in pl2
               for l = (+ (length v) (length w))
               do (setq tem (make-array l
                                        :element-type (array-element-type v)
                                        :fill-pointer  l
                                        )
                        )
               collect tem))
  (setq new (make-polygon (first new) (second new)) )

  (copy-array-portion (polygon-pts pl1) (polygon-pts new)
                      0 0 (length (polygon-pts pl1)))
  (copy-array-portion (polygon-pts pl2) (polygon-pts new)
                      (length (polygon-pts pl1))
                      0 (length (polygon-pts pl2)))
  (copy-array-portion (polygon-edges pl1) (polygon-edges new)
                      0 0 (length (polygon-edges pl1)))
  (loop for i from (length (polygon-edges pl1))
         for j from 0 below (length (polygon-edges pl2))
         with  lpts1  =  (length (polygon-pts pl1))
         with ar2   =  (polygon-edges pl2)
         with arnew =  (polygon-edges new)
         do (setf (aref arnew i) (f+ lpts1 (aref ar2 j)))))

(defun $copy_pts(lis vec start)
  (declare (fixnum start))
  (let ((tem vec))
    (declare (type (cl:array double-float) tem))
    (cond ((numberp lis)
           (or (typep lis 'double-float) (setq lis (float lis 0.0)))
           (setf (aref tem start) lis)
           
           (+ start 1))
          ((typep lis 'cons)
           ($copy_pts (cdr lis) vec  ($copy_pts (car lis) vec start)))

          ((symbolp lis) start)
          (t (error "bad lis")))))
  

;; parametric ; [parametric,xfun,yfun,[t,tlow,thigh],[nticks ..]]
;; the rest of the parametric list after the list will be pushed plot_options

(defun draw2d-parametric (param range1 &aux range tem)
  (cond ((and ($listp (setq tem (nth 4 param)))
              (symbolp (cadr tem))
              (eql ($length tem) 3)
              (<= (length (symbol-name (cadr tem))) 2))
         ;; sure looks like a range
         (setq range tem)))
  (let* (($plot_options ($append ($rest param 3)
                                 (if range1
                                     ($cons range1 $plot_options)
                                     $plot_options)))
         (nticks (nth 2($get_plot_option '$nticks)))
         (trange (or range ($get_plot_option '$t)))
         (xrange ($get_plot_option '$x))
         (yrange ($get_plot_option '$y))
         ($numer t)
         (tmin (coerce-float (nth 2 trange)))
         (tmax (coerce-float (nth 3 trange)))
         (xmin (coerce-float (nth 2 xrange)))
         (xmax (coerce-float (nth 3 xrange)))
         (ymin (coerce-float (nth 2 yrange)))
         (ymax (coerce-float (nth 3 yrange)))
         (x 0.0)         ; have to initialize to some floating point..
         (y 0.0)
         (tt tmin)
         (eps (/ (- tmax tmin) (- nticks 1)))
         f1 f2 in-range-y in-range-x in-range last-ok 
         )
    (declare (double-float x y tt ymin ymax xmin xmax tmin tmax eps))
    (setq f1 (coerce-float-fun (nth 2 param) `((mlist), (nth 1 trange))))
    (setq f2 (coerce-float-fun (nth 3 param) `((mlist), (nth 1 trange))))
    (cons '(mlist simp)    
          (loop 
           do 
           (setq x (funcall f1 tt))
           (setq y (funcall f2 tt))
           (setq in-range-y (and (<= y ymax) (>= y ymin)))
           (setq in-range-x  (and  (<= x xmax) (>= x xmin)))
           (setq in-range (and in-range-x in-range-y))
           when (and (not in-range) (not last-ok))
           collect  'moveto and collect 'moveto
           do
           (setq last-ok in-range)
           collect (if in-range-x x (if (> x xmax) xmax xmin))
           collect (if in-range-y y (if (> y ymax) ymax ymin))
           when (>= tt tmax) do (loop-finish)
           do (setq tt (+ tt eps))
           (if (>= tt tmax) (setq tt tmax))
           )))
  )

(defun draw2d-discrete (f)
  (let* ((f (copy-tree f))              ; Copy all of F because we destructively modify it below.
         (x (third f))
         (y (fourth f)))
    (let
      (($numer t) ($float t) ($%enumer t)
       (data
         (cond
           ((= (length f) 4)                 ; [discrete,x,y]
            (if (not ($listp x))
              (merror "draw2d (discrete): ~M must be a list." x))
            (if (not ($listp y))
              (merror "draw2d (discrete): ~M must be a list." y))
            (cons '(mlist) (mapcan #'list (rest x) (rest y))))
           ((= (length f) 3)                 ; [discrete,xy]
            (if (not ($listp x))
              (merror "draw2d (discrete): ~M must be a list." x))
            (let ((tmp (mapcar #'rest (rest x))))
              (cons '(mlist) (mapcan #'append tmp))))
           (t                                ; error
             (merror
               "draw2d (discrete): expression is not of the form [discrete,x,y] or ~%[discrete,xy].")))))

      ;; Encourage non-floats to become floats here.

      ;; Arguments X and Y were evaluated when $PLOT2D was called.
      ;; So theoretically calling MEVAL again is a no-no; 
      ;; instead, RESIMPLIFY should be called.
      ;; However, the only circumstance in which MEVAL and RESIMPLIFY yield different results
      ;; is when X or Y contains a symbol which evaluates to a number, in which case calling
      ;; RESIMPLIFY would cause an error (because the symbol would cause Gnuplot to barf).

      (meval data))))


;; arrange so that the list of points x0,y0,x1,y1,.. on the curve
;; never have abs(y1-y0 ) and (x1-x0) <= deltax

;;#+nil
;;(defun draw2d (f range )
;;  (if (and ($listp f) (equal '$parametric (cadr f)))
;;      (return-from draw2d (draw2d-parametric f range)))
;;  (let* ((nticks (nth 2($get_plot_option '$nticks)))
;;       (yrange ($get_plot_option '|$y|))
;;       ($numer t)
;;       )

;;    (setq f (coerce-float-fun f `((mlist), (nth 1 range))))

;;    (let* ((x (coerce-float (nth 2 range)))
;;         (xend (coerce-float (nth 3 range)))
;;         (ymin (coerce-float (nth 2 yrange)))
;;         (ymax (coerce-float (nth 3 yrange)))
;;         (eps ($/ (- xend x) (coerce-float nticks)))
;;         (x1 0.0)
;;         (y1 0.0)
;;         (y (funcall f x))
;;         (dy 0.0)
;;         (epsy ($/ (- ymax ymin) 1.0))
;;         (eps2 (* eps eps))
;;         in-range last-ok
;;         )
;;      (declare (double-float x1 y1 x y dy eps2 eps ymin ymax ))
;;                                      ;(print (list 'ymin ymin 'ymax ymax epsy))
;;      (setq x ($- x eps))  
;;      (cons '(mlist)
;;          (loop   do
;;                   (setq x1 ($+ eps x))
;;                   (setq y1 (funcall f x1))
;;                   (setq in-range (and (<= y1 ymax) (>= y1 ymin)))
;;                   (cond (in-range
;;                          (setq dy (- y1 y))
;;                          (cond ((< dy 0) (setq dy (- dy))))
;;                          (cond ((> dy eps)
;;                                 (setq x1 (+ x (/ eps2 dy)))
;;                                 (setq y1 (funcall f x1))
;;                                 (setq in-range (and (<= y1 ymax) (>= y1 ymin)))
;;                                 (or in-range (setq x1 (+ eps x)))
;;                                 )
;;                                ))
;;                         )
;;                   (setq x x1)
;;                   (setq y y1)
;;                   when (or (and (not last-ok)
;;                                 (not in-range))
;;                            (> dy epsy))

;;                   collect 'moveto and collect 'moveto
;;                   do
;;                   (setq last-ok in-range)
;;                   collect x1 
;;                   collect (if in-range y1 (if (> y1 ymax) ymax ymin))
;;                   when (>= x xend)
;;                   collect xend and
;;                   collect (let ((tem (funcall f xend)))
;;                             (if (>= tem ymax) ymax (if (<= tem ymin) ymin tem)))
;;                   and do (sloop::loop-finish))))))

;;; Adaptive plotting, based on the adaptive plotting code from
;;; YACAS. See http://yacas.sourceforge.net/Algo.html#c3s1 for a
;;; description of the algorithm.  More precise details can be found
;;; in the file yacas/scripts/plots.rep/plot2d.ys.


;; Determine if we have a slow oscillation of the function.
;; Basically, for each 3 consecutive function values, we check to see
;; if the function is monotonic or not.  There are 3 such sets, and
;; the function is considered slowly oscillating if at most 2 of them
;; are not monotonic.
(defun slow-oscillation-p (f0 f1 f2 f3 f4)
  (flet ((sign-change (x y z)
           (cond ((not (and (numberp x) (numberp y) (numberp z)))
                  ;; Something is not a number.  Assume the
                  ;; oscillation is not slow.
                  2)
                 ((or (and (> y x) (> y z))
                      (and (< y x) (< y z)))
                  1)
                 (t
                  0))))
    (<= (+ (sign-change f0 f1 f2)
           (sign-change f1 f2 f3)
           (sign-change f2 f3 f4))
        2)))

;; Determine if the function values are smooth enough.  This means
;; that integrals of the functions on the left part and the right part
;; of the range are approximately the same.
;;
;; 
(defun smooth-enough-p (f-a f-a1 f-b f-b1 f-c eps)
  (cond ((every #'numberp (list f-a f-a1 f-b f-b1 f-c))
         (let ((quad (/ (+ f-a
                           (* -5 f-a1)
                           (* 9 f-b)
                           (* -7 f-b1)
                           (* 2 f-c))
                        24))
               (quad-b (/ (+ (* 5 f-b)
                             (* 8 f-b1)
                             (- f-c))
                          12)))
           ;; According to the Yacas source code, quad is the Simpson
           ;; quadrature for the (fb,fb1) subinterval (using points b,b1,c),
           ;; subtracted from the 4-point Newton-Cotes quadrature for the
           ;; (fb,fb1) subinterval (using points a, a1, b, b1.
           ;;
           ;; quad-b is the Simpson quadrature for the (fb,f1) subinterval.
	   ;;
	   ;; This used to test for diff <= 0.  But in some
	   ;; situations, like plot2d(0.99,[x,0,5]), roundoff prevents
	   ;; this from happening.  So we do diff < delta instead, for
	   ;; some value of delta.
	   ;;
	   ;; XXX: What is the right value for delta?  Does this break
	   ;; other things?  Simple tests thus far show that
	   ;; 100*double-float-epsilon is ok.
	   (let ((diff (- (abs quad)
			  (* eps (- quad-b (min f-a f-a1 f-b f-b1 f-c)))))
		 (delta (* 100 double-float-epsilon)))
	     (<= diff delta))))
        (t
         ;; Something is not a number, so assume it's not smooth enough.
         nil)))
                    
    
(defun adaptive-plot (fcn a b c f-a f-b f-c depth eps)
  ;; Step 1:  Split the interval [a, c] into 5 points
  (let* ((a1 (/ (+ a b) 2))
         (b1 (/ (+ b c) 2))
         (f-a1 (funcall fcn a1))
         (f-b1 (funcall fcn b1))
         )
    (cond ((or (not (plusp depth))
               (and (slow-oscillation-p f-a f-a1 f-b f-b1 f-c)
                    (smooth-enough-p f-a f-a1 f-b f-b1 f-c eps)))
           ;; Everything is nice and smooth so we're done.  Don't
           ;; refine anymore.
           (list a f-a
                 a1 f-a1
                 b f-b
                 b1 f-b1
                 c f-c))
          ;; We are not plotting the real part of the function and the
          ;; function is undefined at all points - assume it has complex value
          ;; on [a,b]. Maybe we should refine it a couple of times just to make sure?
          ((and (null *plot-realpart*)
                (null f-a) (null f-a1) (null f-b) (null f-b1) (null f-c))
           (list a f-a
                 a1 f-a1
                 b f-b
                 b1 f-b1
                 c f-c))
          (t
           ;; Need to refine.  Split the interval in half, and try to plot each half.  
           (let ((left (adaptive-plot fcn a a1 b f-a f-a1 f-b (1- depth) (* 2 eps)))
                 (right (adaptive-plot fcn b b1 c f-b f-b1 f-c (1- depth) (* 2 eps))))
             (append left (cddr right)))))))

(defun draw2d (fcn range &optional (log-x-p nil) (log-y-p nil))
  (if (and ($listp fcn) (equal '$parametric (cadr fcn)))
      (return-from draw2d (draw2d-parametric fcn range)))
  (if (and ($listp fcn) (equal '$discrete (cadr fcn)))
      (return-from draw2d (draw2d-discrete fcn)))
  (let* ((nticks (nth 2 ($get_plot_option '$nticks)))
         (yrange ($get_plot_option '$y))
         (depth (nth 2 ($get_plot_option '$adapt_depth)))
         ($numer t))

    (setq fcn (coerce-float-fun fcn `((mlist), (nth 1 range))))

    (let* ((x-start (coerce-float (nth 2 range)))
           (xend (coerce-float (nth 3 range)))
           (x-step (/ (- xend x-start) (coerce-float nticks) 2))
           (ymin (coerce-float (nth 2 yrange)))
           (ymax (coerce-float (nth 3 yrange)))
           ;; What is a good EPS value for adaptive plotting?
                                        ;(eps 1d-5)
           x-samples y-samples result
           )
      (declare (double-float ymin ymax))
      ;; Divide the region into NTICKS regions.  Each region has a
      ;; start, mid and endpoint. Then adaptively plot over each of
      ;; these regions.  So it's probably a good idea not to make
      ;; NTICKS too big.  Since adaptive plotting splits the sections
      ;; in half, it's also probably not a good idea to have NTICKS be
      ;; a power of two.
      (when log-x-p
        (setf x-start (log x-start))
        (setf xend (log xend))
        (setf x-step (/ (- xend x-start) (coerce-float nticks) 2)))

      (flet ((fun (x)
               (let ((y (if log-x-p
                            (funcall fcn (exp x))
                            (funcall fcn x))))
                 (if log-y-p
                     (log y)
                     y))))
        
        (dotimes (k (1+ (* 2 nticks)))
          (let ((x (+ x-start (* k x-step))))
            (push x x-samples)
            (push (fun x) y-samples)))
        (setf x-samples (nreverse x-samples))
        (setf y-samples (nreverse y-samples))

        ;; For each region, adaptively plot it.
        (do ((x-start x-samples (cddr x-start))
             (x-mid (cdr x-samples) (cddr x-mid))
             (x-end (cddr x-samples) (cddr x-end))
             (y-start y-samples (cddr y-start))
             (y-mid (cdr y-samples) (cddr y-mid))
             (y-end (cddr y-samples) (cddr y-end)))
            ((null x-end))
          ;; The region is x-start to x-end, with mid-point x-mid.
          ;;
          ;; The cddr is to remove the one extra sample (x and y value)
          ;; that adaptive plot returns. But on the first iteration,
          ;; result is empty, so we don't want the cddr because we want
          ;; all the samples returned from adaptive-plot.  On subsequent
          ;; iterations, it's a duplicate of the last ponit of the
          ;; previous interval.
          (setf result
                (if result
                    (append result
                            (cddr
                             (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                            (car y-start) (car y-mid) (car y-end)
                                            depth 1d-5)))
                    (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                   (car y-start) (car y-mid) (car y-end)
                                   depth 1d-5))))
          

        ;; jfa: I don't think this is necessary any longer
        ;;      (format t "Points = ~D~%" (length result))

        ;; Fix up out-of-range values
        (do ((x result (cddr x))
             (y (cdr result) (cddr y)))
            ((null y))
          (when log-x-p
            (setf (car x) (exp (car x))))
          (when log-y-p
            (setf (car y) (exp (car y))))
          (unless (and (numberp (car y))
                       (<= ymin (car y) ymax))
            (setf (car x) 'moveto)
            (setf (car y) 'moveto)))
        (cons '(mlist) result)))))

(defun get-range (lis)
  (let ((ymin most-positive-double-float)
        (ymax most-negative-double-float))
    (declare (double-float ymin ymax))
    (do ((l lis (cddr l)))
        ((null l))
      (or (floatp (car l)) (setf (car l) (float (car l) #. (coerce 2 'double-float))))
      (cond ((float-<   (car l)ymin)
             (setq ymin (car l))))
      (cond ((float-<  ymax  (car l))
             (setq ymax (car l)))))
    (list '(mlist) ymin ymax)))

(defvar $gnuplot_command (if (string= *autoconf-win32* "true")
                             "wgnuplot"
                             "gnuplot"))

(defvar $gnuplot_view_args (if (string= *autoconf-win32* "true")
                               "\"~a\" -"
                               "-persist \"~a\""))

(defvar $gnuplot_file_args "\"~a\"")

(defvar $mgnuplot_command "mgnuplot")
(defvar $geomview_command "geomview")

(defvar $openmath_plot_command "xmaxima")

(defun plot-temp-file (file)
  (if *maxima-tempdir* 
    (format nil "~a/~a" *maxima-tempdir* file)
    file))

(defun gnuplot-print-header (dest &key log-x log-y const-expr)
  (let ((gnuplot-out-file nil))

    (when ($get_plot_option '$gnuplot_pm3d 2)
        (format dest "set pm3d~%")
        ; ----- BEGIN GNUPLOT 4.0 WORK-AROUND -----
        ; When the expression to be plotted is a constant, Gnuplot fails with a division by 0.
        ; Explicitly assigning cbrange prevents the error. Also set zrange to match cbrange.
        ; When the bug is fixed in Gnuplot (maybe 4.1 ?) this hack can go away.
        (when (floatp const-expr)
          (format dest "set cbrange [~a : ~a]~%" (1- const-expr) (1+ const-expr))
          (format dest "set zrange [~a : ~a]~%" (1- const-expr) (1+ const-expr))))
        ; -----  END GNUPLOT 4.0 WORK-AROUND  -----

    (if ($get_plot_option '$gnuplot_out_file 2)
        (setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
    ;; default output file name for for all formats except default
    (if (and (not (eq ($get_plot_option '$gnuplot_term 2) '$default)) 
             (null gnuplot-out-file))
      (setq gnuplot-out-file 
        (plot-temp-file (format nil "maxplot.~(~a~)" 
	                   (get-gnuplot-term ($get_plot_option '$gnuplot_term 2)))) ))
    (case ($get_plot_option '$gnuplot_term 2)
      ($default
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_default_term_command)))
      ($ps
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_ps_term_command))
       (if gnuplot-out-file
           (format dest "set out '~a'~%" gnuplot-out-file)))
      ($dumb
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_dumb_term_command))
       (if gnuplot-out-file
           (format dest "set out '~a'~%" gnuplot-out-file)))
      (t
       (format dest "set term ~a~%" 
               (get-plot-option-string '$gnuplot_term))
       (if gnuplot-out-file
           (format dest "set out '~a'~%" gnuplot-out-file))) )
    (when log-x
      (format dest "set log x~%"))
    (when log-y
      (format dest "set log y~%"))
    (format dest "~a~%" (get-plot-option-string '$gnuplot_preamble))))


(defun gnuplot-process (&optional file)
  (let ((gnuplot-term ($get_plot_option '$gnuplot_term 2))
        (gnuplot-out-file ($get_plot_option '$gnuplot_out_file 2))
        (gnuplot-out-file-string (get-plot-option-string '$gnuplot_out_file))
        (run-viewer ($get_plot_option '$run_viewer 2))
	(gnuplot-preamble (string-downcase (get-plot-option-string '$gnuplot_preamble)))
        (view-file))
    ;; default output file name for for all formats except default
    (when (and (not (eq ($get_plot_option '$gnuplot_term 2) '$default)) 
               (null gnuplot-out-file))
      (setq gnuplot-out-file 
        (plot-temp-file (format nil "maxplot.~(~a~)" (get-gnuplot-term gnuplot-term))))
      (setq gnuplot-out-file-string gnuplot-out-file)) 
    ;; run gnuplot in batch mode if necessary before viewing
    (if (and gnuplot-out-file (not (eq gnuplot-term '$default)))
        ($system (format nil "~a \"~a\"" $gnuplot_command file)))
    (when run-viewer
      (if (eq gnuplot-term '$default)
          (setf view-file file)
          (setf view-file gnuplot-out-file-string))
      (case gnuplot-term
        ($default
         ($system (format nil "~a ~a" $gnuplot_command
                          (format nil (if (search "set out " gnuplot-preamble) 
			                 $gnuplot_file_args 
					 $gnuplot_view_args)
				      view-file))))
        ($dumb
         (if gnuplot-out-file
             ($printfile view-file)
             (merror "Plot option `gnuplot_out_file' not defined."))))) 
    (if gnuplot-out-file
        (format t "Output file \"~a\".~%" gnuplot-out-file-string))))

(defun $plot2d (fun &optional range &rest options)
  (let (($numer t)
        ($display2d nil)
        (*plot-realpart* *plot-realpart*)
        (i 0)
        ($plot_options $plot_options)
        plot-format gnuplot-term gnuplot-out-file file plot-name
	xmin xmax ymin ymax)
    (dolist (v options)
      ($set_plot_option v)
      ;; set up y range for plotting program
      (when (and ($listp v) (eq (nth 1 v) '$y) (nth 3 v))
         (setf ymin (coerce-float (nth 2 v)))
         (setf ymax (coerce-float (nth 3 v)))))
  
    (setq *plot-realpart* ($get_plot_option '$plot_realpart 2))
    (when (and (consp fun) (eq (cadr fun) '$parametric))
      (or range (setq range (nth 4 fun)))
      (setf fun `((mlist) ,fun)))
    (when (and (consp fun) (eq (cadr fun) '$discrete))
      (setf fun `((mlist) ,fun)))

    ;; See if we're doing log plots.
    (let ((log-x ($get_plot_option '$logx 2))
          (log-y ($get_plot_option '$logy 2)))
      (unless ($listp fun ) (setf fun `((mlist) ,fun)))     
      (let ((no-range-required t))
        (if (not ($listp fun))
            (setf no-range-required nil)
            (dolist (subfun (rest fun))
              (if (not ($listp subfun))
                  (setf no-range-required nil))))
        (unless no-range-required
          (setq range (check-range range))
	  ;;; set up x range for plotting program
	  (setf xmin (coerce-float (nth 2 range)))
	  (setf xmax (coerce-float (nth 3 range))))
        (if (and no-range-required range)
            ;;; second argument was really a plot option, not a range
            (progn ($set_plot_option range)
               ;;; set up ranges for plotting program
	       (when (and ($listp range) (eq (nth 1 range) '$x) (nth 3 range))
		 (setf xmin (coerce-float (nth 2 range)))
		 (setf xmax (coerce-float (nth 3 range))))
	       (when (and ($listp range) (eq (nth 1 range) '$y) (nth 3 range))
		 (setf ymin (coerce-float (nth 2 range)))
		 (setf ymax (coerce-float (nth 3 range)))))))
 
      (setf plot-format  ($get_plot_option '$plot_format 2))
      (setf gnuplot-term ($get_plot_option '$gnuplot_term 2))
      (if ($get_plot_option '$gnuplot_out_file 2)
          (setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
      (if (and (eq plot-format '$gnuplot) (eq gnuplot-term '$default) 
               gnuplot-out-file)
          (setf file gnuplot-out-file)
          (setf file (plot-temp-file
                      (format nil "maxout.~(~a~)" (stripdollar plot-format)))))
      ;; old function $plot2dopen incorporated here
      (case plot-format
        ($openmath
         (show-open-plot
          (with-output-to-string
            (st)
            (cond ($show_openplot (format st "plot2d -data {~%"))
                  (t (format st "{plot2d ~%")))
            (when (and xmin xmax) (format st " {xrange ~g ~g}" xmin xmax))
            (when (and ymin ymax) (format st " {yrange ~g ~g}" ymin ymax))
            (dolist (f (cdr fun))
              (incf i)
              (setq plot-name
                    (let ((string ""))
                      (cond ((atom f) 
                        (setf string (coerce (mstring f) 'string)))
                            ((eq (second f) '$parametric)
                             (setf string 
                                   (concatenate 
                                    'string (coerce (mstring (third f)) 'string)
                                    ", " (coerce (mstring (fourth f)) 'string))))
                            ((eq (second f) '$discrete)
                             (setf string (format nil "discrete~a" i)))
                            (t (setf string (coerce (mstring f) 'string))))
                      (cond ((< (length string) 80) string)
                        (t (format nil "fun~a" i)))))
              (format st " {label \"~a\"}~%" plot-name)
              (format st " {xversusy~%")
              (let ((lis (cdr (draw2d f range log-x log-y))))
                (loop while lis
                  do
                  (loop while (and lis (not (eq (car lis) 'moveto)))
                        collecting (car lis) into xx
                        collecting (cadr lis) into yy
                        do (setq lis (cddr lis))
                        finally
                        ;; only output if at least two points for line
                        (cond ((cdr xx)
                               (tcl-output-list st xx)
                               (tcl-output-list st yy))))
                  ;; remove the moveto
                  (setq lis (cddr lis))))
              (format st "}"))
            (format st "} "))))

      (t
        (with-open-file (st file :direction :output :if-exists :supersede)
          (case plot-format
            ($gnuplot
             (gnuplot-print-header st :log-x log-x :log-y log-y)
             (format st "plot")
             (when (and xmin xmax) (format st " [~g:~g]" xmin xmax))
             (when (and ymin ymax)
               (unless (and xmin xmax) (format st " []")) 
               (format st " [~g:~g]" ymin ymax)))
            ($gnuplot_pipes
             (check-gnuplot-process)
             ($gnuplot_reset)
             (gnuplot-print-header *gnuplot-stream* :log-x log-x :log-y log-y)
             (setq *gnuplot-command* (format nil "plot"))
             (when (and xmin xmax)
               (setq *gnuplot-command*
                     ($sconcat *gnuplot-command* 
                               (format nil " [~g:~g]"  xmin xmax))))
             (when (and ymin ymax) 
               (unless (and xmin xmax)
                 (setq *gnuplot-command* ($sconcat *gnuplot-command*
                                                   (format nil " []"))))
                 (setq *gnuplot-command*
                       ($sconcat *gnuplot-command* 
                                 (format nil " [~g:~g]"  ymin ymax))))))
          (dolist (v (cdr fun))
	    (case plot-format
	      ($gnuplot_pipes
	       (if (> i 0)
		   (setq *gnuplot-command* ($sconcat *gnuplot-command* ", ")))
	       (setq *gnuplot-command* ($sconcat *gnuplot-command* 
					         (format nil "'~a' index ~a " file i)))))
            (incf i)
            (setq plot-name
                  (let ((string ""))
                    (cond ((atom v) 
                           (setf string (coerce (mstring v) 'string)))
                          ((eq (second v) '$parametric)
                           (setf string 
                                 (concatenate 
                                  'string (coerce (mstring (third v)) 'string)
                                  ", " (coerce (mstring (fourth v)) 'string))))
                          ((eq (second v) '$discrete)
                           (setf string (format nil "discrete~a" i)))
                          (t (setf string (coerce (mstring v) 'string))))
                    (cond ((< (length string) 80) string)
                          (t (format nil "fun~a" i)))))
            (case plot-format
              ($gnuplot
               (if (> i 1)
                   (format st ","))
               (let ((title (get-plot-option-string '$gnuplot_curve_titles i)))
                 (if (equal title "default")
                     (setf title (format nil "title '~a'" plot-name)))
                 (format st " '-' ~a ~a" title 
                         (get-plot-option-string '$gnuplot_curve_styles i))))
	      ($gnuplot_pipes
	       (let ((title (get-plot-option-string '$gnuplot_curve_titles i)))
                 (if (equal title "default")
                     (setf title (format nil "title '~a'" plot-name)))
                 (setq *gnuplot-command*
		       ($sconcat *gnuplot-command*
			         (format nil " ~a ~a" title 
				         (get-plot-option-string '$gnuplot_curve_styles i))))))))
          (case plot-format
            ($gnuplot
             (format st "~%"))
	    ($gnuplot_pipes
	     (format st "~%")))
          (setf i 0)
          (dolist (v (cdr fun))
            (incf i)

        ; Assign PLOT-NAME only if not already assigned.
        ; I (Robert Dodier) would just cut it, but it's not clear
        ; that it is always assigned by the time we arrive here.
        (if (null plot-name)
              (setq plot-name
                    (let ((string (coerce (mstring v) 'string)))
                      (cond ((< (length string) 20) string)
                            (t (format nil "Fun~a" i))))))

            (case plot-format
              ($xgraph
               (format st "~%~% \"~a\"~%" plot-name))
              ($gnuplot
               (if (> i 1)
                   (format st "e~%")))
	      ($gnuplot_pipes
	       (if (> i 1)
		   (format st "~%~%")))
              ($mgnuplot
               (format st "~%~%# \"~a\"~%" plot-name))
              )
	    (let (in-discontinuity)
	      (loop for (v w) on (cdr (draw2d v range log-x log-y)) by #'cddr
		    do
		    (cond ((eq v 'moveto)
			   (cond 
			     ((find plot-format '($gnuplot_pipes $gnuplot))
			      ;; A blank line means a discontinuity
			      (if (null in-discontinuity)
				  (progn
				    (format st "~%")
				    (setq in-discontinuity t))))
			     ((equal plot-format '$mgnuplot)
			      ;; A blank line means a discontinuity
			      (format st "~%"))
			     (t
			      (format st "move "))))
			  (t  (format st "~g ~g ~%" v w)
			      (setq in-discontinuity nil))))))))))

      (case plot-format
        ($gnuplot 
         (gnuplot-process file))
	($gnuplot_pipes
	 (send-gnuplot-command *gnuplot-command*))
        ($mgnuplot 
         ($system (concatenate 'string *maxima-plotdir* "/" $mgnuplot_command) 
                  (format nil " -plot2d \"~a\" -title '~a'" file plot-name)))
        ($xgraph
         ($system (format nil "xgraph -t 'Maxima Plot' < \"~a\" &" file)))
        )
      ""))

;;(defun maxima-bin-search (command)
;;  (or ($file_search command
;;                  `((mlist) , (maxima-path "bin" "###")))
;;               command))
    

; Adapted from MSTRINGP (change & to $).
(defun msymbolp (x)
    (and (symbolp x) (char= (firstcharn x) #\$)))

;; OK, here are some test cases for $SPRINT.
;; The only strange one is sprint ([s1, s2, s3]) which is printing an expression containing
;; Lisp strings via STRGRIND, which introduces the ? and \ , dunno if there's anything we
;; want to do about that.

;; sprint ("foo bar", "Foo Bar", "FOO BAR");
;; sprint (["foo bar", "Foo Bar", "FOO BAR"]);
;; :lisp (setq $s1 "foo bar")
;; :lisp (setq $s2 "Foo Bar")
;; :lisp (setq $s3 "FOO BAR")
;; sprint (s1, s2, s3);
;; sprint ([s1, s2, s3]);  =>  [?foo\ bar,?Foo\ Bar,?FOO\ BAR]  -- strange ??
;; sprint (aa, Bb, CC);
;; sprint ([aa, Bb, CC]);
;; sprint (aa + Bb + CC);
;; sprint (1234, 12.34, 12.34e300, 12.34b300);
;; sprint ([1234, 12.34, 12.34e300, 12.34b300]);
;; sprint (%i, %pi, %phi, %gamma);
;; sprint ([%i, %pi, %phi, %gamma]);
;; sprint (%i + %pi + %phi + %gamma);
;; sprint (foo(xx,yy) + Bar(Xx,Yy) + BAZ(XX,YY));

(defun $sprint (&rest args)
  (sloop for v in args do
         (cond
           ((stringp v)
            v)
           ((or (mstringp v) (msymbolp v))
            (setq v (maybe-invert-string-case (symbol-name (stripdollar v)))))
           (t
             (setq v (maybe-invert-string-case (string (implode (strgrind v)))))))
         (princ v)
         (princ " "))
  (car args))

;; $SHOW_FILE APPEARS TO BE ESSENTIALLY THE SAME AS $PRINTFILE

;;(defun $show_file(file)
;;  (princ (file-to-string ($file_search file)))
;;  '$done)


(defun $tcl_output  (lis i &optional (skip 2))
  (or (typep i 'fixnum) (error "~a should be an integer" i ))
  ($listp_check 'list lis )
  (format *standard-output* "~% {")
  (cond (($listp (nth 1 lis))
         (loop for v in lis
                do
                (format *standard-output* "~,10g " (nth i v)))
         )
        (t
         (setq lis (nthcdr i lis))
         (loop  with v = lis  while v
                 do
                 (format *standard-output* "~,10g " (car v))
                 (setq v (nthcdr skip v))
                 )
         ))
  (format *standard-output* "~% }")
  )


(defun tcl-output-list ( st lis )
  (cond ((null lis) )
        ((atom (car lis))
         (princ " {  " st)
         (loop for v in lis
                count t into n
                when (eql 0 (mod n 5))
                do (terpri st)
                do
                (format st "~,10g " v))
         (format st  " }~%"))
        (t (tcl-output-list st (car lis))
           (tcl-output-list st (cdr lis)))))


(defun $openplot_curves (lis &aux (linel 100000))
 (let (($display2d nil) ($numer t) ($float t) ($%enumer t))
  (declare (special linel))
  (show-open-plot
   (with-output-to-string
     (st )
     (cond
      ($show_openplot (format st "plot2d -data {~%"))
      (t (format st "{plot2d ~%")))
     (or (and ($listp lis) ($listp (nth 1 lis)))
         (merror "Need a list of curves, [[x1,y1,x2,y2,...],[u1,v1,u2,v2,...]] or [[[x1,y1],[x2,y2],...]"))
    
     (loop for v in (cdr lis)
            do
            (or ($listp v) (merror "should be a list"))
            (setq v (cdr v))
            (format st "~%~%")
            (loop while (and (car v) (symbolp (car v)))
                   do   (mformat st "~M~%" ($concat (car v)))
                   (setq v (cdr v))
                   )
            when v
            do  
            (format st "~%{ xversusy  ")
            (loop while v
                   with this with xvals with yvals
                   do
                   (cond   ((numberp (meval (car v)))
                            (setq this v) (setq v (cddr v))
                            (push (meval (car this)) xvals)
                            (push (meval (second this)) yvals)
                            )
                           (($listp (car v))
                            (setq this (cdar v))
                            (push (meval (car this)) xvals)
                            (push (meval (second this)) yvals)
                            (and (third this) (push (meval (third this)) yvals))
                            (setq v (cdr v)))
                           (t (merror "Unacceptable type of data: ~M" (car v)))
                   )
                   finally
                   (tcl-output-list st (nreverse xvals))
                   (tcl-output-list st (nreverse yvals)))
            (format st " }")
            )
     (format st "}~%")
     ))))



(defun $xgraph_curves (lis &rest options &aux w)
  options
  (with-open-file (st  "xgraph-out" :direction :output :if-exists :supersede)
    (format st "=600x600~%")
    (loop for v in (cdr lis)
           do
           (setq v (cdr v))
           (format st "~%~%")
           (loop while v
                  do
                  (cond
                    ((symbolp (car v))
                     (mformat st "~M~%" ($concat (car v)))
                     (setq v (cdr v)))
                    (t (cond       ((numberp (car v))
                                    (setq w v) (setq v (cddr v)))
                                   (($listp (car v))
                                    (setq w (cdar v))
                                    (setq v (cdr v))))
                       (format st "~g ~g ~%" (car w) (second w)))))))
  ($system "xgraph -t 'Maxima Plot' < xgraph-out &"))


     

(defun $view_zic ()
  (let ((izdir (maxima-getenv "IZICDIR")))
    (or (probe-file
         (format nil "~a/tcl-files/maxima.tcl" izdir))
        (error
         "could not find file ~a :  Set environment variable IZICDIR" izdir))
    ($system "izic -interface ${IZICDIR}/tcl-files/maxima.tcl  1> /dev/null &")))

(defun $isend (x)
  ($system (format nil " izic -app izic -cmd '{~a}'" (string-trim '(#\&) (string x)))))



(defvar *some-colours*
  ;; from rgb.txt
  '(135 206 250         lightskyblue
    70 130 180          steelblue
    205  92  92         indianred
    178  34  34         firebrick
    176  48  96         maroon
    221 160 221         plum
    238 130 238         violet))


;; one of $zic or $geomview

(defun plot-zic-colors (&aux (ncolors  (/ (length *some-colours*) 4))) 
  (format $pstream "couleurs ~% ~a ~% " ncolors  )
  (loop for v in *some-colours*
         with do-ind = t with ind = -1 
         do
         (cond (do-ind
                   (format $pstream "~a " (incf ind))
                 (setq do-ind nil)
                 ))
         (cond ((numberp v)
                (print-pt (/ v 256.0)))
               (t 
                (setq do-ind t)
                (format $pstream "~%# ~(~a~)~%" v))))
  (format $pstream " 1
0
~a 0.801 0.359 0.359 
128 .8 1 0
" ncolors))

(defun check-range (range &aux ($numer t) ($float t) tem a b)
  (or (and ($listp range)
           (setq tem (cdr range))
           (symbolp (car tem))
           (numberp (setq a (meval* (second tem))))
           (numberp (setq b (meval* (third tem))))
           (< a b))
      (if range
          (merror 
           "Bad range: ~M.~%Range must be of the form [variable,min,max]"
           range)
          (merror 
           "No range given. Must supply range of the form [variable,min,max]")))
  `((mlist) ,(car tem) ,(float a) ,(float b)))

(defun $zero_fun (x y) x y 0.0)

(defun output-points (pl &optional m)
  "If m is supplied print blank line every m lines"
  (let ((j -1))
    (declare (fixnum j))
    (loop for i below (length (polygon-pts pl))
           with ar = (polygon-pts pl)
           do (print-pt (aref ar i))
           (setq i (+ i 1))
           (print-pt (aref ar i))
           (setq i (+ i 1))
           (print-pt (aref ar i))
           (terpri $pstream)
           (cond (m
                  (setq j (+ j 1))
                  (cond ((eql j (the fixnum m))
                         (terpri $pstream)
                         (setq j -1)))))
           )))

(defun show-open-plot (ans)
  (cond ($show_openplot
         (with-open-file (st1 (plot-temp-file "maxout.openmath") :direction :output :if-exists :supersede)
           (princ  ans st1))
         ($system (concatenate 'string *maxima-prefix* 
	                               (if (string= *autoconf-win32* "true") "\\bin\\" "/bin/") 
	                               $openmath_plot_command)
                  (format nil " \"~a\"" (plot-temp-file "maxout.openmath"))))
        (t (princ ans) "")))


; contour_plot -- set some parameters for Gnuplot and punt to plot3d
;
; We go to some trouble here to avoid clobbering the Gnuplot preamble
; specified by the user, either as a global option (via set_plot_option)
; or specified in arguments to contour_plot. Just append or prepend
; the parameters for contour plotting to the user-specified preamble.
; Assume that arguments take precedence over global options.
;
; contour_plot knows how to set parameters only for Gnuplot.
; If the plot_format is not a Gnuplot format, complain.
;
; Examples:
;
;   contour_plot (x^2 + y^2, [x, -4, 4], [y, -4, 4]);
;   contour_plot (sin(y) * cos(x)^2, [x, -4, 4], [y, -4, 4]);
;   F(x, y) := x^3 + y^2;
;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4], [gnuplot_preamble, "set size ratio -1"]);
;   set_plot_option ([gnuplot_preamble, "set cntrparam levels 12"]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;   set_plot_option ([plot_format, openmath]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4]); => error: must be gnuplot format
;   contour_plot (F, [u, -4, 4], [v, -4, 4], [plot_format, gnuplot]);

(defun $contour_plot (expr &rest optional-args)
  (let*
    ((plot-format-in-plot-options ($get_plot_option '$plot_format 2))
     (plot-format-in-arguments
       (let (($plot_options `((mlist) ,@optional-args))) ($get_plot_option '$plot_format 2)))

     (preamble-in-plot-options ($get_plot_option '$gnuplot_preamble 2))
     (preamble-in-arguments
       (let (($plot_options `((mlist) ,@optional-args))) ($get_plot_option '$gnuplot_preamble 2)))

     (contour-preamble "set contour; unset surface; set view map")
     (gnuplot-formats '($gnuplot $mgnuplot $gnuplot_pipes)))

    ; Ensure that plot_format is some gnuplot format.
    ; Argument takes precedence over global option.

    (if
      (or
        (and plot-format-in-arguments
             (not (memq plot-format-in-arguments gnuplot-formats)))
        (and (not plot-format-in-arguments)
             (not (memq plot-format-in-plot-options gnuplot-formats))))

      (merror "contour_plot: plot_format = ~a not understood; must be a gnuplot format."
              (print-invert-case (stripdollar (or plot-format-in-arguments plot-format-in-plot-options))))

      ; Prepend contour preamble to preamble in arguments (if given)
      ; and pass concatenated preamble as an argument to plot3d.
      ; Otherwise if there is a global option preamble, 
      ; append contour preamble to global option preamble.
      ; Otherwise just set global option preamble to the contour preamble.

      ; All this complication is to avoid clobbering the preamble
      ; if one was specified somehow (either global option or argument).

      (if preamble-in-arguments
        (let
          ((args-sans-preamble
             ($sublist `((mlist) ,@optional-args)
                       '((lambda) ((mlist) e) (not (and ($listp e) (eq ($first e) '$gnuplot_preamble)))))))
          (setq preamble-in-arguments
                ($sconcat contour-preamble " ; " preamble-in-arguments))
          (apply #'$plot3d
                 (append (list expr)
                         (cdr args-sans-preamble)
                         (list `((mlist) $gnuplot_preamble ,preamble-in-arguments)))))

        (let (($plot_options $plot_options))

          (if preamble-in-plot-options
            ($set_plot_option
              `((mlist) $gnuplot_preamble
                        ,($sconcat preamble-in-plot-options " ; " contour-preamble)))
            ($set_plot_option `((mlist) $gnuplot_preamble ,contour-preamble)))

          (apply #'$plot3d (cons expr optional-args)))))))
 

(defun $plot3d ( fun &optional (xrange ($get_plot_option '$x))
                (yrange ($get_plot_option '$y) y-supplied)
                &rest options 
                &aux lvars trans *original-points*
                ($plot_options $plot_options)
                ($in_netmath $in_netmath)
                grid
                plot-format gnuplot-term gnuplot-out-file file
                orig-fun
                const-expr
                )
  (declare (special *original-points*))
  (setf orig-fun fun)
  (cond (options
         (dolist (v options)
           ($set_plot_option v))))
  (setf plot-format  ($get_plot_option '$plot_format 2))
  (setf gnuplot-term ($get_plot_option '$gnuplot_term 2))
  (if ($get_plot_option '$gnuplot_out_file 2)
      (setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
  (if (and (eq plot-format '$gnuplot) 
           (eq gnuplot-term '$default) 
           gnuplot-out-file)
      (setf file gnuplot-out-file)
      (setf file (plot-temp-file (format nil "maxout.~(~a~)" (stripdollar plot-format)))))
  (and $in_netmath (setq $in_netmath (eq plot-format '$openmath)))
  (setq xrange (check-range xrange))
  (setq yrange (check-range yrange))
  (cond ((not y-supplied)
         (let ((vars ($sort ($listofvars fun))))
           (or (eql ($length vars) 2)
               (merror "Please supply the range for variables eg [x,-3,3],[y,-3,4]"))
           (setq xrange ($cons (nth 1 vars) ($rest xrange)))
           (setq yrange ($cons (nth 2 vars) ($rest yrange))))))
  (setq grid ($get_plot_option '$grid))
  (setq lvars `((mlist),(nth 1 xrange) ,(nth 1 yrange)))
  (cond (($listp fun)
         (or (eql 3 ($length fun)) (merror "List ~M is not of length 3" fun))
         (setq trans ($make_transform
                      ($append lvars '((mlist) $z))
                      (nth 1 fun)
                      (nth 2 fun)
                      (nth 3 fun)))
         ; ----- BEGIN GNUPLOT 4.0 WORK-AROUND -----
         (when ($constantp (nth 3 fun))
           (setq const-expr (let (($numer t)) (meval (nth 3 fun))))
           (if ($numberp const-expr)
             (setq const-expr ($float const-expr))))
         ; -----  END GNUPLOT 4.0 WORK-AROUND  -----
         (setq fun '$zero_fun))
        (t
          ; ----- BEGIN GNUPLOT 4.0 WORK-AROUND -----
          (when ($constantp fun)
           (setq const-expr (let (($numer t)) (meval fun)))
           (if ($numberp const-expr)
             (setq const-expr ($float const-expr))))
          ; -----  END GNUPLOT 4.0 WORK-AROUND  -----
          (setq fun (coerce-float-fun fun lvars))))
  (let* ((pl (draw3d fun
                     (nth 2 xrange)
                     (nth 3 xrange)
                     (nth 2 yrange)
                     (nth 3 yrange)
                     (nth 2 grid)
                     (nth 3 grid)))
         (ar (polygon-pts pl)) tem
         )
    (declare (type (cl:array double-float) ar))

    (if trans  (mfuncall trans ar))
    (if (setq tem  ($get_plot_option '$transform_xy 2)) (mfuncall tem ar))
    ;; compute bounding box.
    (let (($pstream
           (cond ($in_netmath *standard-output*)
                 (t (open file :direction :output :if-exists :supersede)))))
      (unwind-protect
           (case  plot-format
             ($zic
              (let ((x-range ($get_range ar 0))
                    (y-range ($get_range ar 1))
                    (z-range ($get_range ar 2)))
                (plot-zic-colors)
                (format $pstream "domaine ~a ~a ~a ~a ~a ~a ~%"
                        (nth 0 x-range)
                        (nth 1 x-range)
                        (nth 0 y-range)
                        (nth 1 y-range)
                        (nth 0 z-range)
                        (nth 1 z-range))
                (format $pstream "surface ~a ~a ~%"
                        (+ 1 (nth 3 grid))
                        (+ 1 (nth 2 grid))
                        )
                (output-points pl nil)))
             ($gnuplot
              (gnuplot-print-header $pstream :const-expr const-expr)
              (let ((title (get-plot-option-string '$gnuplot_curve_titles 1))
                    (plot-name
                     (let ((string (coerce (mstring orig-fun) 'string)))
                       (cond ((< (length string) 20) string)
                             (t (format nil "Function"))))))
                (if (equal title "default")
                    (setf title (format nil "title '~a'" plot-name)))
                (format $pstream "splot '-' ~a ~a~%" title 
                        (get-plot-option-string '$gnuplot_curve_styles 1)))
              (output-points pl (nth 2 grid)))
             ($gnuplot_pipes
	      (check-gnuplot-process)
	      ($gnuplot_reset)
              (gnuplot-print-header *gnuplot-stream* :const-expr const-expr)
              (let ((title (get-plot-option-string '$gnuplot_curve_titles 1))
                    (plot-name
                     (let ((string (coerce (mstring orig-fun) 'string)))
                       (cond ((< (length string) 20) string)
                             (t (format nil "Function"))))))
                (if (equal title "default")
                    (setf title (format nil "title '~a'" plot-name)))
                (setq *gnuplot-command*
		      (format nil "splot '~a' ~a ~a~%" file title 
			      (get-plot-option-string '$gnuplot_curve_styles 1))))
              (output-points pl (nth 2 grid)))
             ($mgnuplot
              (output-points pl (nth 2 grid)))
             ($openmath
              (progn
                (cond 
                 ($show_openplot
                  (format $pstream "plot3d -data {{matrix_mesh ~%"))
                 (t (format $pstream "{plot3d {matrix_mesh ~%")))
                ;; we do the x y z  separately:
                (loop for off from 0 to 2
                       with ar = (polygon-pts pl)
                       with  i of-type fixnum = 0
                       do (setq i off)
                       (format $pstream "~%{")
                       (loop 
                        while (< i (length ar))
                        do (format $pstream "~% {")
                        (loop for j to (nth 2 grid)
                               do (print-pt (aref ar i))
                               (setq i (+ i 3)))
                        (format $pstream "} ")
                        )
                       (format $pstream "} ")
                       )
                (format $pstream "}}"))
              #+old
              (progn                    ; orig
                (print (list 'grid grid))
                (cond
                 ($show_openplot
                  (format $pstream "plot3d -data {{variable_grid ~%"))
                 (t (format $pstream "{plot3d {{variable_grid ~%")))
                (let* ((ar (polygon-pts pl))
                       (x-coords
                        (loop for i to (nth 2 grid)
                               collect (aref ar (* i 3))))
                       (y-coords
                        (loop for i to (nth 3 grid)
                               with m = (* 3 (+ 1 (nth 2 grid)))
                               collect (aref ar (+ 1 (* i m)))))
                       (z  (loop for i to (nth 3 grid)
                                  with k of-type fixnum = 2
                                  collect
                                  (loop for j to (nth 2 grid)
                                         collect (aref ar k)
                                         do(setq k (+ k 3))))))
                  (tcl-output-list $pstream x-coords)
                  (tcl-output-list $pstream y-coords)
                  (format $pstream "~%{")
                  (tcl-output-list $pstream z)
                  (format $pstream "}}}"))
                )
              )
             ($geomview
              (format $pstream " MESH ~a ~a ~%"
                      (+ 1 (nth 2 grid))
                      (+ 1 (nth 3 grid))
                      )
              (output-points pl nil)))
        ;; close the stream and plot..
        (cond ($in_netmath (return-from $plot3d ""))
              (t (close $pstream)
                 (setq $pstream nil)
                 ))
        )
      (if (eq plot-format '$gnuplot)
          (gnuplot-process file)
          (cond (($get_plot_option '$run_viewer 2)
                 (case plot-format
                   ($zic ($view_zic))
                   ($openmath
                     ($system (concatenate 'string *maxima-prefix* 
		                                   (if (string= *autoconf-win32* "true") "\\bin\\" "/bin/")
		                                   $openmath_plot_command) 
                              (format nil " \"~a\"" file)))
                   ($geomview 
                     ($system $geomview_command
                              (format nil " \"~a\"" file)))
		   ($gnuplot_pipes
		    (send-gnuplot-command *gnuplot-command*))
                   ($mgnuplot 
                     ($system (concatenate 'string *maxima-plotdir* "/" $mgnuplot_command)
                              (format nil " -parametric3d \"~a\"" file)))
                   ))))
      ))
  "")
  

#| these are already defined in clmacs.lisp |#
(or (fboundp '*$) (setf (symbol-function '*$) (symbol-function '*)))
(or (fboundp '+$) (setf (symbol-function '+$) (symbol-function '+)))
(or (fboundp '-$) (setf (symbol-function '-$) (symbol-function '-)))
(or (fboundp '/$) (setf (symbol-function '/$) (symbol-function '/)))

#|
Examples

/* plot of z^(1/3)...*/
plot3d(r^.33*cos(th/3),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,zic]) ;

/* plot of z^(1/2)...*/
plot3d(r^.5*cos(th/2),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,zic]) ;

/* moebius */
plot3d([cos(x)*(3+y*cos(x/2)),sin(x)*(3+y*cos(x/2)),y*sin(x/2)],[x,-%pi,%pi],[y,-1,1],['grid,50,15]) ;

/* klein bottle */
plot3d([5*cos(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0) - 10.0,
-5*sin(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0),
5*(-sin(x/2)*cos(y)+cos(x/2)*sin(2*y))],[x,-%pi,%pi],[y,-%pi,%pi],
['grid,40,40])                          ;
/* torus */
plot3d([cos(y)*(10.0+6*cos(x)),
sin(y)*(10.0+6*cos(x)),
-6*sin(x)], [x,0,2*%pi],[y,0,2*%pi],['grid,40,40]) ;
|#

