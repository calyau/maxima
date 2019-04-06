;;Copyright William F. Schelter 1990, All Rights Reserved

(in-package :maxima)

#|
Examples

/* plot of z^(1/3)...*/
plot3d(r^.33*cos(th/3),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,geomview]) ;

/* plot of z^(1/2)...*/
plot3d(r^.5*cos(th/2),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,xmaxima]) ;

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

(defun ensure-string (x)
  (cond
    ((stringp x) x)
    ((symbolp x) (print-invert-case (stripdollar x)))
    (t (maybe-invert-string-case (string (implode (strgrind x)))))))

(defmfun $join (x y)
  (if (and ($listp x) ($listp y))
      (cons '(mlist) (loop for w in (cdr x) for u in (cdr y) collect w collect u))
      (merror (intl:gettext "join: both arguments must be lists."))))

(defun coerce-float (x) ($float (meval* x)))

(defvar *maxima-plotdir* "")
(declare-top (special *maxima-tempdir* *maxima-prefix*))

;; *ROT* AND FRIENDS ($ROT, $ROTATE_PTS, $ROTATE_LIST) CAN PROBABLY GO AWAY !!
;; THEY ARE UNDOCUMENTED AND UNUSED !!
(defvar *rot* (make-array 9 :element-type 'flonum))
(defvar $rot nil)

;; Global plot options list; this is a property list.. It is not a
;; Maxima variable, to discourage users from changing it directly; it
;; should be changed via set_plot_option

(defvar *plot-options* 
  `(:plot_format
    ,(if (string= *autoconf-windows* "true")
         '$gnuplot
         '$gnuplot_pipes)
    :grid (30 30) :run_viewer t :axes t
    ;; With adaptive plotting, 29 nticks should be enough; adapt_depth
    ;; controls the number of splittings adaptive-plotting will do.
    :nticks 29 :adapt_depth 5
    :color ($blue $red $green $magenta $black $cyan)
    :point_type ($bullet $box $triangle $plus $times $asterisk)
    :palette (((mlist) $gradient $green $cyan $blue $violet)
              ((mlist) $gradient $magenta $violet $blue $cyan $green $yellow
               $orange $red $brown $black))   
    :gnuplot_preamble "" :gnuplot_term $default))

(defvar $plot_options 
  `((mlist)
    ((mlist) $plot_format
     ,(if (string= *autoconf-windows* "true")
          '$gnuplot
          '$gnuplot_pipes))))

;; $plot_realpart option is false by default but *plot-realpart* is true
;; because coerce-float-fun is used outside of plot package too.
(defvar *plot-realpart* t)

(defun maybe-realpart (x)
  (if *plot-realpart*
      ($realpart x)
      (if (zerop1 ($imagpart x))
          ($realpart x)
          nil)))

(defvar *missing-data-indicator* "NaN")

(defvar *gnuplot-stream* nil)
(defvar *gnuplot-command* "")

(defvar $gnuplot_command "gnuplot")

(defun start-gnuplot-process (path)
  ;; TODO: Forward gnuplot's stderr stream to maxima's stderr output
  #+clisp (setq *gnuplot-stream* (ext:make-pipe-output-stream path))
  ;; TODO: Forward gnuplot's stderr stream to maxima's stderr output
  #+lispworks (setq *gnuplot-stream* (system:open-pipe path))
  #+cmu (setq *gnuplot-stream*
              (ext:process-input (ext:run-program path nil :input :stream
                                                  :output *error-output* :wait nil)))
  #+scl (setq *gnuplot-stream*
              (ext:process-input (ext:run-program path nil :input :stream
                                                  :output *error-output* :wait nil)))
  #+sbcl (setq *gnuplot-stream*
               (sb-ext:process-input (sb-ext:run-program path nil
                                                         :input :stream
                                                         :output *error-output* :wait nil
                                                         :search t)))
  #+gcl (setq *gnuplot-stream*
              (open (concatenate 'string "| " path) :direction :output))
  #+ecl (progn
          (setq *gnuplot-stream* (ext:run-program path nil :input :stream :output *error-output* :error :output :wait nil)))
  #+ccl (setf *gnuplot-stream*
              (ccl:external-process-input-stream
               (ccl:run-program path nil
                                :wait nil :output *error-output*
                                :input :stream)))
  #+allegro (setf *gnuplot-stream* (excl:run-shell-command
                    path :input :stream :output *error-output* :wait nil))
  #+abcl (setq *gnuplot-stream* (system::process-input (system::run-program path nil :wait nil)))
  #-(or clisp cmu sbcl gcl scl lispworks ecl ccl allegro abcl)
  (merror (intl:gettext "plotting: I don't know how to tell this Lisp to run Gnuplot."))
  
  (if (null *gnuplot-stream*)
    (merror (intl:gettext "plotting: I tried to execute ~s but *GNUPLOT-STREAM* is still null.~%") path))

  ;; set mouse must be the first command send to gnuplot
  (send-gnuplot-command "set mouse"))

(defun check-gnuplot-process ()
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command)))

(defmfun $gnuplot_close ()
  (stop-gnuplot-process)
  "")

(defmfun $gnuplot_start ()
  (check-gnuplot-process)
  "")

(defmfun $gnuplot_restart ()
  ($gnuplot_close)
  ($gnuplot_start))

(defun stop-gnuplot-process ()
  (unless (null *gnuplot-stream*)
      (progn
        (close *gnuplot-stream*)
        (setq *gnuplot-stream* nil))))

(defun send-gnuplot-command (command &optional recursive)
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command))
  (handler-case (unless (null command)
		  (format *gnuplot-stream* "~a ~%" command)
		  (finish-output *gnuplot-stream*))
    (error (e)
      ;; allow gnuplot to restart if stream-error, or just an error is signaled
      ;; only try to restart once, to prevent an infinite loop 
      (cond (recursive
	     (error e))
	    (t
	     (warn "~a~%Trying new stream.~%" e)
	     (setq *gnuplot-stream* nil)
	     (send-gnuplot-command command t))))))

(defmfun $gnuplot_reset ()
  (send-gnuplot-command "unset output")
  (send-gnuplot-command "reset"))

(defmfun $gnuplot_replot (&optional s)
  (if (null *gnuplot-stream*)
      (merror (intl:gettext "gnuplot_replot: Gnuplot is not running.")))
  (cond ((null s)
         (send-gnuplot-command "replot"))
        ((stringp s)
         (send-gnuplot-command s)
         (send-gnuplot-command "replot"))
        (t
         (merror (intl:gettext "gnuplot_replot: argument, if present, must be a string; found: ~M") s)))
  "")

;; allow this to be set in a system init file (sys-init.lsp)

(defmfun $get_plot_option (&optional name n)
  (let (options)
    ;; Converts the options property list into a Maxima list
    (do* ((list (copy-tree *plot-options*) (cddr list))
	  (key (first list) (first list))
	  (value (second list) (second list)))
	 ((endp list))
      (let ((max-key (intern (concatenate 'string "$" (symbol-name key)))))
	(if (consp value)
	    (push (cons '(mlist) (cons max-key value)) options)
	    (push (list '(mlist) max-key value) options))))
    (setf options (cons '(mlist) (nreverse options)))
    (if name
	(let ((value (find name (cdr options) :key #'second)))
	  (if n
	      (nth n value)
	      value))
        options)))

(defun quote-strings (opt)
  (if (atom opt)
      (if (stringp opt)
          (format nil "~s" opt)
          opt)
      (cons (quote-strings (car opt))
            (quote-strings (cdr opt)))))

(defun get-plot-option-string (option &optional (index 1))
  (let* ((val ($get_plot_option option 2))
         (val-list (if ($listp val)
                       (cdr val)
                       `(,val))))
    (ensure-string (nth (mod (- index 1) (length val-list)) val-list))))

(defmfun $set_plot_option (&rest value)
  (setq *plot-options* (plot-options-parser value *plot-options*))
  ($get_plot_option))

(defmfun $remove_plot_option (name)
  (remf *plot-options*
        (case name
          ($adapt_depth :adapt_depth) ($axes :axes) ($azimuth :azimuth)
          ($box :box) ($color :color) ($color_bar :color_bar)
          ($color_bar_tics :color_bar_tics) ($elevation :elevation)
          ($grid :grid) ($grid2d :grid2d) ($iterations :iterations)
          ($label :label) ($legend :legend) ($logx :logx) ($logy :logy)
          ($mesh_lines_color :mesh_lines_color) ($nticks :nticks)
          ($palette :palette) ($plot_format :plot_format)
          ($plot_realpart :plot_realpart) ($point_type :point_type)
          ($pdf_file :pdf_file) ($png_file :png_file) ($ps_file :ps_file)
          ($run_viewer :run_viewer) ($same_xy :samexy)
          ($same_xyz :same_xyz) ($style :style) ($svg_file :svg_file)
          ($t :t) ($title :title) ($transform_xy :transform_xy)
          ($x :x) ($xbounds :xbounds) ($xlabel :xlabel)
          ($xtics :xtics) ($xvar :xvar) ($xy_scale :xy_scale)
          ($y :y) ($ybounds :ybounds) ($ylabel :ylabel) ($ytics :ytics)
          ($yvar :yvar) ($yx_ratio :yx_ratio)
          ($z :z) ($zlabel :zlabel) ($zmin :zmin) ($ztics :ztics)
          ($gnuplot_4_0 :gnuplot_4_0)
          ($gnuplot_curve_titles :gnuplot_curve_titles)
          ($gnuplot_curve_styles :gnuplot_curve_styles)
          ($gnuplot_default_term_command :gnuplot_default_term_command)
          ($gnuplot_dumb_term_command :gnuplot_dumb_term_command)
          ($gnuplot_out_file :gnuplot_out_file)
          ($gnuplot_pm3d :gnuplot_pm3d)
          ($gnuplot_preamble :gnuplot_preamble)
          ($gnuplot_postamble :gnuplot_postamble)
          ($gnuplot_pdf_term_command :gnuplot_pdf_term_command)
          ($gnuplot_png_term_command :gnuplot_png_term_command)
          ($gnuplot_ps_term_command :gnuplot_ps_term_command)
          ($gnuplot_svg_term_command :gnuplot_svg_term_command)
          ($gnuplot_term :gnuplot_term))))

(defun get-gnuplot-term (term)
  (let* ((sterm (string-downcase (ensure-string term)))
         (pos   (search " " sterm)))
    (if pos  
      (subseq sterm 0 pos)
      sterm)))
  
(defvar $pstream nil)

(defun print-pt1 (f str)
  (if (floatp f)
    (format str "~g " f)
    (format str "~a " *missing-data-indicator*)))

(defstruct (polygon (:type list)
                    (:constructor %make-polygon (pts edges)))
  (dummy '($polygon simp))
  pts edges)

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (defmacro z-pt (ar i) `(aref ,ar (the fixnum (+ 2 (* ,i 3)))))
    (defmacro y-pt (ar i) `(aref ,ar (the fixnum (1+ (* ,i 3)))))
    (defmacro x-pt (ar i) `(aref ,ar (the fixnum (* ,i 3))))
    (defmacro rot (m i j) `(aref ,m (the fixnum (+ ,i (the fixnum (* 3 ,j))))))

    (defmacro print-pt (f)
      `(print-pt1 ,f $pstream ))

    (defmacro make-polygon (a b)
      `(list '($polygon) ,a ,b)))

(defun draw3d (f minx maxx miny maxy  nxint nyint)
  (let* ((epsx (/ (- maxx minx) nxint))
         (x 0.0)  ( y 0.0)
         (epsy (/ (- maxy miny) nyint))
         (nx (+ nxint 1))
         (l 0)
         (ny (+ nyint 1))
         (ar (make-array  (+ 12         ; 12  for axes
                             (* 3 nx ny))  :fill-pointer (* 3 nx ny)
                             :element-type t :adjustable t)))
    (declare (type flonum x y epsy epsx)
             (fixnum nx  ny l)
             (type (cl:array t) ar))
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
  (let* ((tem (make-array (+ 15 (* 5 nx ny)) :fill-pointer (* 5 nx ny)
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

(defmfun $rotation1 (phi th)
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
#-abcl (defmfun $rotate_pts(pts rotation-matrix)
  (or ($matrixp rotation-matrix) (merror (intl:gettext "rotate_pts: second argument must be a matrix.")))
  (let* ((rot *rot*)
         (l (length pts))
         (x 0.0) (y 0.0) (z 0.0)
         )
    (declare (type flonum  x y z))
    (declare (type (cl:array flonum) rot))
    ($copy_pts rotation-matrix *rot* 0)
        
    (loop with j = 0
           while (< j l)
           do
           (setq x (aref pts j))
           (setq y (aref pts (+ j 1)))
           (setq z (aref pts (+ j 2)))
           (loop for i below 3 with a of-type flonum = 0.0
                  do
                  (setq a (* x (aref rot (+ (* 3 i) 0))))
                  (setq a (+ a (* y (aref rot (+ (* 3 i) 1)))))
                  (setq a (+ a (* z (aref rot (+ (* 3 i) 2)))))
                  (setf (aref pts (+ j i )) a))
           (setf j (+ j 3)))))

(defmfun $rotate_list (x)
  (cond ((and ($listp x) (not (mbagp (second x))))
         ($list_matrix_entries (ncmul2  $rot x)))
        ((mbagp x) (cons (car x) (mapcar '$rotate_list (cdr x))))))

(defmfun $get_range (pts k &aux (z 0.0) (max most-negative-flonum) (min most-positive-flonum))
  (declare (type flonum z max min))
  (declare (type (vector flonum) pts))
  (loop for i from k below (length pts) by 3
         do (setq z (aref pts i))
         (cond ((< z min) (setq min z)))
         (cond ((> z max) (setq max z))))
  (list min max (- max min)))

(defmfun $polar_to_xy (pts &aux (r 0.0) (th 0.0))
  (declare (type flonum r th))
  (declare (type (cl:array t) pts))
  (assert (typep pts '(vector t)))
  (loop for i below (length pts) by 3
         do (setq r (aref pts i))
         (setq th (aref pts (+ i 1)))
         (setf (aref pts i) (* r (cos th)))
         (setf (aref pts (+ i 1)) (* r (sin th)))))

;; Transformation from spherical coordinates to rectangular coordinates,
;; to be used in plot3d. Example of its use:
;; plot3d (expr, [th, 0, %pi], [ph, 0, 2*%pi], [transform_xy, spherical_to_xyz])
;; where expr gives the value of r in terms of the inclination (th)
;; and azimuth (ph).
;;
(defmfun $spherical_to_xyz (pts &aux (r 0.0) (th 0.0) (ph 0.0)) 
  (declare (type flonum r th ph))
  (declare (type (cl:array t) pts))
  (assert (typep pts '(vector t)))
  (loop for i below (length pts) by 3
     do (setq th (aref pts i))
       (setq ph (aref pts (+ i 1)))
       (setq r (aref pts (+ i 2)))
       (setf (aref pts i) (* r (sin th) (cos ph)))
       (setf (aref pts (+ i 1)) (* r (sin th) (sin ph)))
       (setf (aref pts (+ i 2)) (* r (cos th)))))
      

;; return a function suitable for the transform function in plot3d.
;; FX, FY, and FZ are functions of three arguments.
(defmfun $make_transform (lvars fx fy fz)
  (setq fx (coerce-float-fun fx lvars))
  (setq fy (coerce-float-fun fy lvars))
  (setq fz (coerce-float-fun fz lvars))
  (let ((sym (gensym "transform")))
    (setf (symbol-function sym)
          #'(lambda (pts &aux  (x1 0.0)(x2 0.0)(x3 0.0))
              (declare (type flonum  x1 x2 x3))
              (declare (type (cl:array t) pts))
              (loop for i below (length pts) by 3
                     do 
                     (setq x1 (aref pts i))
                     (setq x2 (aref pts (+ i 1)))
                     (setq x3 (aref pts (+ i 2)))
                     (setf (aref pts i) (funcall fx x1 x2 x3))
                     (setf (aref pts (+ 1 i)) (funcall fy x1 x2 x3))
                     (setf (aref pts (+ 2 i)) (funcall fz x1 x2 x3)))))))

;; Return value is a Lisp function which evaluates EXPR to a float.
;; COERCE-FLOAT-FUN always returns a function and never returns a symbol,
;; even if EXPR is a symbol.
;;
;; Following cases are recognized:
;; EXPR is a symbol
;;   name of a Lisp function
;;   name of a Maxima function
;;   name of a DEFMSPEC function
;;   name of a Maxima macro
;;   a string which is the name of a Maxima operator (e.g., "!")
;;   name of a simplifying function
;; EXPR is a Maxima lambda expression
;; EXPR is a general Maxima expression
;;
;; %COERCE-FLOAT-FUN is the main internal routine for this.
;; COERCE-FLOAT-FUN is the user interface for creating a function that
;; returns floats.  COERCE-BFLOAT-FUN is the same, except bfloats are
;; returned.
(defun %coerce-float-fun (float-fun expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
         (let ((args (if lvars (cdr lvars) (list (gensym)))))
           (coerce-lisp-function-or-lisp-lambda args expr :float-fun float-fun)))
        ;; expr is a string which names an operator
        ;; (e.g. "!" "+" or a user-defined operator)
        ((and (stringp expr) (getopr0 expr))
         (let ((a (if lvars lvars `((mlist) ,(gensym)))))
           (%coerce-float-fun float-fun `(($apply) ,(getopr0 expr) ,a) a)))
        ((and (symbolp expr) (not (member expr lvars)) (not ($constantp expr)))
         (cond
           ((fboundp expr)
            (let ((args (if lvars (cdr lvars) (list (gensym)))))
              (coerce-lisp-function-or-lisp-lambda args expr :float-fun float-fun)))

           ;; expr is name of a Maxima function defined by := or
           ;; define
           ((mget expr 'mexpr)
            (let*
                ((mexpr (mget expr 'mexpr))
                 (args (cdr (second mexpr))))
              (coerce-maxima-function-or-maxima-lambda args expr :float-fun float-fun)))

           ((or
             ;; expr is the name of a function defined by defmspec
             (get expr 'mfexpr*)
             ;; expr is the name of a Maxima macro defined by ::=
             (mget expr 'mmacro)
             ;; expr is the name of a simplifying function, and the
             ;; simplification property is associated with the noun
             ;; form
             (get ($nounify expr) 'operators)
             ;; expr is the name of a simplifying function, and the
             ;; simplification property is associated with the verb
             ;; form
             (get ($verbify expr) 'operators))
            (let ((a (if lvars lvars `((mlist) ,(gensym)))))
              (%coerce-float-fun float-fun `(($apply) ,expr ,a) a)))
           (t
            (merror (intl:gettext "COERCE-FLOAT-FUN: no such Lisp or Maxima function: ~M") expr))))

	((and (consp expr) (eq (caar expr) 'lambda))
	 (let ((args (cdr (second expr))))
	   (coerce-maxima-function-or-maxima-lambda args expr :float-fun float-fun)))

        (t
         (let* ((vars (or lvars ($sort ($listofvars expr))))
		(subscripted-vars ($sublist vars '((lambda) ((mlist) $x) ((mnot) (($atom) $x)))))
		gensym-vars save-list-gensym subscripted-vars-save
		subscripted-vars-mset subscripted-vars-restore)

	   ;; VARS and SUBSCRIPTED-VARS are Maxima lists.  Other lists are
	   ;; Lisp lists.
	   (when (cdr subscripted-vars)
	     (setq gensym-vars (mapcar #'(lambda (ign) (declare (ignore ign)) (gensym))
				       (cdr subscripted-vars)))
	     (mapcar #'(lambda (a b) (setq vars (subst b a vars :test 'equal)))
		     (cdr subscripted-vars) gensym-vars)

	     ;; This stuff about saving and restoring array variables
	     ;; should go into MBINDING, and the lambda expression
	     ;; constructed below should call MBINDING.  (At present
	     ;; MBINDING barfs on array variables.)
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

	       ;; Nothing interpolated here when there are no subscripted
	       ;; variables.
	       ,@(if save-list-gensym `((declare (special ,save-list-gensym))))

	       ;; Nothing interpolated here when there are no subscripted
	       ;; variables.
	       ,@(if (cdr subscripted-vars)
		     `((progn (setq ,save-list-gensym nil)
			      ,@(append subscripted-vars-save subscripted-vars-mset))))

	       (let (($ratprint nil)
		     ;; We don't want to set $numer to T when coercing
		     ;; to a bigfloat.  By doing so, things like
		     ;; log(400)^400 get converted to double-floats,
		     ;; which causes a double-float overflow.  But the
		     ;; whole point of coercing to bfloat is to use
		     ;; bfloats, not doubles.
		     ;;
		     ;; Perhaps we don't even need to do this for
		     ;; double-floats?  It would be nice to remove
		     ;; this.  For backward compatibility, we bind
		     ;; numer to T if we're not trying to bfloat.
		     ($numer ,(not (eq float-fun '$bfloat)))
		     (*nounsflag* t)
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
			       (,float-fun (maybe-realpart (meval* ',expr))))
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
			       (,float-fun (maybe-realpart (meval* ',expr))))
			   (cl::error () t))
			 ))

		   ;; Nothing interpolated here when there are no
		   ;; subscripted variables.
		   ,@(if (cdr subscripted-vars) `((progn ,@subscripted-vars-restore)))

		   result)))
	    'function)))))

(defun coerce-float-fun (expr &optional lvars)
  (%coerce-float-fun '$float expr lvars))

(defun coerce-bfloat-fun (expr &optional lvars)
  (%coerce-float-fun '$bfloat expr lvars))

(defun coerce-maxima-function-or-maxima-lambda (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for x in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
		(errorsw t)
		(errcatch t))
	   (declare (special errcatch))
	   ;; Just always try to convert the result to a float,
	   ;; which handles things like $%pi.  See also BUG
	   ;; #2880115
	   ;; https://sourceforge.net/tracker/?func=detail&atid=104933&aid=2880115&group_id=4933
	   ;;
	   ;; Should we use HANDLER-CASE like we do above in
	   ;; %coerce-float-fun?  Seems not necessary for what we want
	   ;; to do.
	   (catch 'errorsw
	     (,float-fun
	      (maybe-realpart (mapply ',expr (list ,@gensym-args) t))))))
      'function)))

;; Same as above, but call APPLY instead of MAPPLY.

(defun coerce-lisp-function-or-lisp-lambda (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for x in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
                (result (maybe-realpart (apply ',expr (list ,@gensym-args)))))
           ;; Always use $float.  See comment for
           ;; coerce-maxima-function-ormaxima-lambda above.
           (,float-fun result)))
      'function)))

(defmacro zval (points verts i) `(aref ,points (+ 2 (* 3 (aref ,verts ,i)))))

;;sort the edges array so that drawing the edges will happen from the back towards
;; the front.   The if n==4 the edges array coming in looks like
;; v1 v2 v3 v4 0 w1 w2 w3 w4 0 ...
;; where vi,wi are indices pointint into the points array specifying a point
;; in 3 space.   After the sorting is done, the 0 is filled in with the vertex
;; which is closer to us (ie highest z component after rotating towards the user)
;; and this is then they are sorted in groups of 5.   
(defun sort-ngons (points edges n &aux lis )
  (declare (type (cl:array (flonum))  points)
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
    (declare (type flonum z z1))
    
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
    (setq lis (sort lis #'alphalessp :key #'car))
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


(defmfun $concat_polygons (pl1 pl2 &aux tem new)
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
         do (setf (aref arnew i) (+ lpts1 (aref ar2 j)))))

(defmfun $copy_pts(lis vec start)
  (declare (fixnum start))
  (let ((tem vec))
    (declare (type (cl:array flonum) tem))
    (cond ((numberp lis)
           (or (typep lis 'flonum) (setq lis (float lis)))
           (setf (aref tem start) lis)
           (1+ start))
          ((typep lis 'cons)
           ($copy_pts (cdr lis) vec  ($copy_pts (car lis) vec start)))
          ((symbolp lis) start)
          (t (merror (intl:gettext "copy_pts: unrecognized first argument: ~M") lis)))))

;; parametric ; [parametric,xfun,yfun,[t,tlow,thigh],[nticks ..]]
;; the rest of the parametric list after the list will add to the plot options

(defun draw2d-parametric-adaptive (param options &aux range)
  (or (= ($length param) 4)
      (merror (intl:gettext "plot2d: parametric plots must include two expressions and an interval")))
  (setq range (nth 4 param))
  (or (and ($listp range) (symbolp (second range)) (eql ($length range) 3))
      (merror (intl:gettext "plot2d: wrong interval for parametric plot: ~M") range))
  (setq range (check-range range))
  (let* ((nticks (getf options :nticks))
         (trange (cddr range))
         (tvar (second range))
         (xrange (or (getf options :x) (getf options :xbounds)))
         (yrange (or (getf options :y) (getf options :ybounds)))
         (tmin (coerce-float (first trange)))
         (tmax (coerce-float (second trange)))
         (xmin (coerce-float (first xrange)))
         (xmax (coerce-float (second xrange)))
         (ymin (coerce-float (first yrange)))
         (ymax (coerce-float (second yrange)))
         f1 f2)
    (declare (type flonum ymin ymax xmin xmax tmin tmax))
    (setq f1 (coerce-float-fun (third param) `((mlist), tvar)))
    (setq f2 (coerce-float-fun (fourth param) `((mlist), tvar)))

    (let ((n-clipped 0) (n-non-numeric 0)
	  (t-step (/ (- tmax tmin) (coerce-float nticks) 2))
	  t-samples x-samples y-samples result)
      ;; Divide the range into 2*NTICKS regions that we then
      ;; adaptively plot over.
      (dotimes (k (1+ (* 2 nticks)))
	(let ((tpar (+ tmin (* k t-step))))
	  (push tpar t-samples)
	  (push (funcall f1 tpar) x-samples)
	  (push (funcall f2 tpar) y-samples)))
      (setf t-samples (nreverse t-samples))
      (setf x-samples (nreverse x-samples))
      (setf y-samples (nreverse y-samples))

      ;; Adaptively plot over each region
      (do ((t-start t-samples (cddr t-start))
	   (t-mid (cdr t-samples) (cddr t-mid))
	   (t-end (cddr t-samples) (cddr t-end))
	   (x-start x-samples (cddr x-start))
	   (x-mid (cdr x-samples) (cddr x-mid))
	   (x-end (cddr x-samples) (cddr x-end))
	   (y-start y-samples (cddr y-start))
	   (y-mid (cdr y-samples) (cddr y-mid))
	   (y-end (cddr y-samples) (cddr y-end)))
	  ((null t-end))
	(setf result
	      (if result
		  (append result
			  (cddr (adaptive-parametric-plot
				 f1 f2
				 (car t-start) (car t-mid) (car t-end)
				 (car x-start) (car x-mid) (car x-end)
				 (car y-start) (car y-mid) (car y-end)
				 (getf options :adapt_depth)
				 1e-5)))
		  (adaptive-parametric-plot
		   f1 f2
		   (car t-start) (car t-mid) (car t-end)
		   (car x-start) (car x-mid) (car x-end)
		   (car y-start) (car y-mid) (car y-end)
		   (getf options :adapt_depth)
		   1e-5))))
      ;; Fix up out-of-range values and clobber non-numeric values.
      (do ((x result (cddr x))
	   (y (cdr result) (cddr y)))
	  ((null y))
	(if (and (numberp (car x)) (numberp (car y)))
            (unless (and (<= ymin (car y) ymax)
			 (<= xmin (car x) xmax))
              (incf n-clipped)
              (setf (car x) 'moveto)
              (setf (car y) 'moveto))
            (progn
              (incf n-non-numeric)
              (setf (car x) 'moveto)
              (setf (car y) 'moveto))))
      ;; Filter out any MOVETO's which do not precede a number.
      ;; Code elsewhere in this file expects MOVETO's to
      ;; come in pairs, so leave two MOVETO's before a number.
      (let ((n (length result)))
	(dotimes (i n)
	  (when
              (and
	       (evenp i)
	       (eq (nth i result) 'moveto)
	       (eq (nth (1+ i) result) 'moveto)
	       (or 
		(eq i (- n 2))
		(eq (nth (+ i 2) result) 'moveto)))
	    (setf (nth i result) nil)
	    (setf (nth (1+ i) result) nil))))

      (let ((result-sans-nil (delete nil result)))
	(if (null result-sans-nil)
            (cond
              ((= n-non-numeric 0)
               (mtell (intl:gettext "plot2d: all values were clipped.~%")))
              ((= n-clipped 0)
               (mtell (intl:gettext
		       "plot2d: expression evaluates to non-numeric value everywhere in plotting range.~%")))
              (t
	       (mtell (intl:gettext
		       "plot2d: all values are non-numeric, or clipped.~%"))))
            (progn
              (if (> n-non-numeric 0)
		  (mtell (intl:gettext
			  "plot2d: expression evaluates to non-numeric value somewhere in plotting range.~%")))
              (if (> n-clipped 0)
		  (mtell (intl:gettext "plot2d: some values were clipped.~%")))))
	(cons '(mlist) result-sans-nil)))))

;; draw2d-discrete. Accepts [discrete,[x1,x2,...],[y1,y2,...]]
;; or [discrete,[[x1,y1]...] and returns [x1,y1,...] or nil, if
;; non of the points have real values.
;; Currently any options given are being ignored, because there
;; are no options specific to the generation of the points.
(defun draw2d-discrete (f)
  (let ((x (third f)) (y (fourth f)) data gaps)
    (cond
      (($listp x)            ; x is a list
       (cond
         (($listp (cadr x))     ; x1 is a list
          (cond
            ((= (length (cadr x)) 3) ; x1 is a 2D point
             (setq data (parse-points-xy x)))
            (t                      ; x1 is not a 2D point
             (merror (intl:gettext "draw2d-discrete: Expecting a point with 2 coordinates; found ~M~%") (cadr x)))))
         (t                     ; x1 is not a list
          (cond
            (($listp y)             ; y is a list
             (cond
               ((symbolp (coerce-float (cadr y))); y is an option
                (setq data (parse-points-y x)))
               (t                            ; y is not an option
                (cond
                  (($listp (cadr y))            ; y1 is a list
                   (merror (intl:gettext "draw2d-discrete: Expecting a y coordinate; found ~M~%") (cadr y)))
                  (t                            ; y1 not a list
                   (cond
                     ((= (length x) (length y))     ; case [x][y]
                      (setq data (parse-points-x-y x y)))
                     (t                             ; wrong
                      (merror (intl:gettext "draw2d-discrete: The number of x and y coordinates do not match.~%")))))))))
            (t                      ; y is not a list
             (setq data (parse-points-y x)))))))
      (t                     ; x is not a list
       (merror (intl:gettext "draw2d-discrete: Expecting a list of x coordinates or points; found ~M~%") x)))

    ;; checks for non-real values
    (cond
      ((some #'realp data)
       (setq gaps (count-if #'(lambda (x) (eq x 'moveto)) data))
       (when (> gaps 0)
         ;; some points have non-real values
         (mtell (intl:gettext "Warning: excluding ~M points with non-numerical values.~%") (/ gaps 2))))
      (t
       ;; none of the points have real values
       (mtell (intl:gettext "Warning: none of the points have numerical values.~%"))
       (setq data nil)))
    data))

;; Two lists [x1...xn] and [y1...yn] are joined as
;; [x1 y1...xn yn], converting all expressions to real numbers.
;; If either xi or yi are not real, both are replaced by 'moveto
(defun parse-points-x-y (x y)
  (do ((a (rest x) (cdr a))
       (b (rest y) (cdr b))
       c af bf)
      ((null b) (cons '(mlist) (reverse c)))
    (setq af (coerce-float (car a)))
    (setq bf (coerce-float (car b)))
    (cond
      ((or (not (realp af)) (not (realp bf)))
       (setq c (cons 'moveto (cons 'moveto c))))
      (t
       (setq c (cons bf (cons af c)))))))

;; One list [y1...yn] becomes the list [1 y1...n yn], 
;; converting all expressions to real numbers.
;; If yi is not real, both i and yi are replaced by 'moveto
(defun parse-points-y (y)
  (do ((a 1 (1+ a))
       (b (rest y) (cdr b))
       c bf)
      ((null b) (cons '(mlist) (reverse c)))
    (setq bf (coerce-float (car b)))
    (cond
      ((not (realp bf))
       (setq c (cons 'moveto (cons 'moveto c))))
      (t
       (setq c (cons bf (cons a c)))))))

;; List [[x1,y1]...[xn,yn]] is transformed into
;; [x1 y1...xn yn], converting all expressions to real numbers.
;; If either xi or yi are not real, both are replaced by 'moveto
(defun parse-points-xy (xy)
  (do ((ab (rest xy) (cdr ab))
       c af bf)
      ((null ab) (cons '(mlist) (reverse c)))
    (setq af (coerce-float (cadar ab)))
    (setq bf (coerce-float (caddar ab)))
    (cond
      ((or (not (realp af)) (not (realp bf)))
       (setq c (cons 'moveto (cons 'moveto c))))
      (t
       (setq c (cons bf (cons af c)))))))

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
           ;; 100*flonum-epsilon is ok.
           (let ((diff (- (abs quad)
                          (* eps (- quad-b (min f-a f-a1 f-b f-b1 f-c)))))
                 (delta (* 150 flonum-epsilon)))
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

(defun adaptive-parametric-plot (x-fcn y-fcn a b c x-a x-b x-c y-a y-b y-c depth eps)
  ;; Step 1:  Split the interval [a, c] into 5 points
  (let* ((a1 (/ (+ a b) 2))
         (b1 (/ (+ b c) 2))
         (x-a1 (funcall x-fcn a1))
         (x-b1 (funcall x-fcn b1))
         (y-a1 (funcall y-fcn a1))
         (y-b1 (funcall y-fcn b1)))
    (cond ((or (not (plusp depth))
	       ;; Should we have a different algorithm to determine
	       ;; slow oscillation and smooth-enough for parametric
	       ;; plots?
               (and (slow-oscillation-p y-a y-a1 y-b y-b1 y-c)
		    (slow-oscillation-p x-a x-a1 x-b x-b1 x-c)
                    (smooth-enough-p y-a y-a1 y-b y-b1 y-c eps)
		    (smooth-enough-p x-a x-a1 x-b x-b1 x-c eps)))
           ;; Everything is nice and smooth so we're done.  Don't
           ;; refine anymore.
           (list x-a y-a
                 x-a1 y-a1
                 x-b y-b
                 x-b1 y-b1
                 x-c y-c))
          ;; We are not plotting the real part of the function and the
          ;; function is undefined at all points - assume it has complex value
          ;; on [a,b]. Maybe we should refine it a couple of times just to make sure?
          ((and (null *plot-realpart*)
                (null y-a) (null y-a1) (null y-b) (null y-b1) (null y-c)
		(null x-a) (null x-a1) (null x-b) (null x-b1) (null x-c))
           (list x-a y-a
                 x-a1 y-a1
                 x-b y-b
                 x-b1 y-b1
                 x-c y-c))
          (t
           ;; Need to refine.  Split the interval in half, and try to plot each half.  
           (let ((left (adaptive-parametric-plot x-fcn y-fcn
						 a a1 b
						 x-a x-a1 x-b
						 y-a y-a1 y-b
						 (1- depth) (* 2 eps)))
                 (right (adaptive-parametric-plot x-fcn y-fcn
						  b b1 c
						  x-b x-b1 x-c
						  y-b y-b1 y-c
						  (1- depth) (* 2 eps))))
	     ;; (cddr right) to skip over the point that is duplicated
	     ;; between the right end-point of the left region and the
	     ;; left end-point of the right
             (append left (cddr right)))))))

(defun draw2d (fcn range plot-options)
  (if (and ($listp fcn) (equal '$parametric (cadr fcn)))
      (return-from draw2d
        (draw2d-parametric-adaptive fcn plot-options)))
  (if (and ($listp fcn) (equal '$discrete (cadr fcn)))
      (return-from draw2d (draw2d-discrete fcn)))
  (let* ((nticks (getf plot-options :nticks))
         (yrange (getf plot-options :ybounds))
         (depth (getf plot-options :adapt_depth)))

    (setq fcn (coerce-float-fun fcn `((mlist), (second range))))

    (let* ((x-start (coerce-float (third range)))
           (xend (coerce-float (fourth range)))
           (x-step (/ (- xend x-start) (coerce-float nticks) 2))
           (ymin (coerce-float (first yrange)))
           (ymax (coerce-float (second yrange)))
           (n-clipped 0) (n-non-numeric 0)
           ;; What is a good EPS value for adaptive plotting?
                                        ;(eps 1e-5)
           x-samples y-samples result
           )
      (declare (type flonum ymin ymax))
      ;; Divide the region into NTICKS regions.  Each region has a
      ;; start, mid and endpoint. Then adaptively plot over each of
      ;; these regions.  So it's probably a good idea not to make
      ;; NTICKS too big.  Since adaptive plotting splits the sections
      ;; in half, it's also probably not a good idea to have NTICKS be
      ;; a power of two.
      (when (getf plot-options :logx)
        (setf x-start (log x-start))
        (setf xend (log xend))
        (setf x-step (/ (- xend x-start) (coerce-float nticks) 2)))

      (flet ((fun (x)
               (let ((y (if (getf plot-options :logx)
                            (funcall fcn (exp x))
                            (funcall fcn x))))
                 (if (and (getf plot-options :logy)
                          (numberp y))
                     (if (> y 0) (log y) 'und)
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
          ;; iterations, it's a duplicate of the last point of the
          ;; previous interval.
          (setf result
                (if result
                    (append result
                            (cddr
                             (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                            (car y-start) (car y-mid) (car y-end)
                                            depth 1e-5)))
                    (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                   (car y-start) (car y-mid) (car y-end)
                                   depth 1e-5))))

        ;; Fix up out-of-range values
        ;; and clobber non-numeric values.

        (do ((x result (cddr x))
             (y (cdr result) (cddr y)))
            ((null y))
          (if (numberp (car y))
              (unless (<= ymin (car y) ymax)
                (incf n-clipped)
                (setf (car x) 'moveto)
                (setf (car y) 'moveto))
              (progn
                (incf n-non-numeric)
                (setf (car x) 'moveto)
                (setf (car y) 'moveto)))
          (when (and (getf plot-options :logx)
                     (numberp (car x)))
            (setf (car x) (exp (car x))))

          (when (and (getf plot-options :logy)
                     (numberp (car y)))
            (setf (car y) (exp (car y)))))

        ;; Filter out any MOVETO's which do not precede a number.
        ;; Code elsewhere in this file expects MOVETO's to
        ;; come in pairs, so leave two MOVETO's before a number.
        (let ((n (length result)))
          (dotimes (i n)
            (when
              (and
                (evenp i)
                (eq (nth i result) 'moveto)
                (eq (nth (1+ i) result) 'moveto)
                (or 
                  (eq i (- n 2))
                  (eq (nth (+ i 2) result) 'moveto)))
              (setf (nth i result) nil)
              (setf (nth (1+ i) result) nil))))

        (let ((result-sans-nil (delete nil result)))
          (if (null result-sans-nil)
            (cond
              ((= n-non-numeric 0)
               (mtell (intl:gettext "plot2d: all values were clipped.~%")))
              ((= n-clipped 0)
               (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value everywhere in plotting range.~%")))
              (t
                (mtell (intl:gettext "plot2d: all values are non-numeric, or clipped.~%"))))
            (progn
              (if (> n-non-numeric 0)
                (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value somewhere in plotting range.~%")))
              (if (> n-clipped 0)
                (mtell (intl:gettext "plot2d: some values were clipped.~%")))))
          (cons '(mlist) result-sans-nil))))))

(defun get-range (lis)
  (let ((ymin most-positive-flonum)
        (ymax most-negative-flonum))
    (declare (type flonum ymin ymax))
    (do ((l lis (cddr l)))
        ((null l))
      (or (floatp (car l)) (setf (car l) (float (car l))))
      (cond ((< (car l) ymin)
             (setq ymin (car l))))
      (cond ((< ymax (car l))
             (setq ymax (car l)))))
    (list '(mlist) ymin ymax)))

#+sbcl (defvar $gnuplot_view_args "-persist ~a")
#-sbcl (defvar $gnuplot_view_args "-persist ~s")

#+(or sbcl openmcl) (defvar $gnuplot_file_args "~a")
#-(or sbcl openmcl) (defvar $gnuplot_file_args "~s")

(defvar $mgnuplot_command "mgnuplot")
(defvar $geomview_command "geomview")

(defvar $xmaxima_plot_command "xmaxima")

(defun plot-temp-file (file)
  (let ((filename 
	 (if *maxima-tempdir* 
	     (format nil "~a/~a" *maxima-tempdir* file)
	   file)))
    (setf (gethash filename *temp-files-list*) t)
    (format nil "~a" filename)
    ))

;; If no file path is given, uses temporary directory path
(defun plot-file-path (file)
  (if (pathname-directory file)
      file
      (plot-temp-file file)))

(defun gnuplot-process (plot-options &optional file out-file)
  (let ((gnuplot-term (getf plot-options :gnuplot_term))
        (run-viewer (getf plot-options :run_viewer))
        #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
		(gnuplot-preamble
         (string-downcase (getf plot-options :gnuplot_preamble))))

    ;; creates the output file, when there is one to be created
    (when (and out-file (not (eq gnuplot-term '$default)))
      #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
      ($system $gnuplot_command (format nil $gnuplot_file_args file))
      #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
      ($system (format nil "~a ~a" $gnuplot_command
                       (format nil $gnuplot_file_args file))))

    ;; displays contents of the output file, when gnuplot-term is dumb,
    ;; or runs gnuplot when gnuplot-term is default
    (when run-viewer
      (case gnuplot-term
        ($default
         ;; the options given to gnuplot will be different when the user
         ;; redirects the output by using "set output" in the preamble
	 #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
	 ($system $gnuplot_command "-persist" (format nil $gnuplot_file_args file))
	 #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
	 ($system 
	  (format nil "~a ~a" $gnuplot_command
		  (format nil (if (search "set out" gnuplot-preamble) 
				  $gnuplot_file_args $gnuplot_view_args)
			  file))))
        ($dumb
         (if out-file
             ($printfile (car out-file))
             (merror (intl:gettext "plotting: option 'gnuplot_out_file' not defined."))))))))

;; plot-options-parser puts the plot options given into a property list.
;; maxopts: a list (not a Maxima list!) with plot options.
;; options: a property list, or an empty list.
;; Example:
;;  (plot-options-parser (list #$[x,-2,2]$ #$[nticks,30]$) '(:nticks 4))
;; returns:
;;  (:XLABEL "x" :XMAX 2.0 :XMIN -2.0 :NTICKS 30)

(defun plot-options-parser (maxopts options &aux name)
  (unless (every #'$listp maxopts)
    (setq maxopts
          (mapcar #'(lambda (x) (if ($listp x) x (list '(mlist) x))) maxopts)))
  (dolist (opt maxopts)
    (unless ($symbolp (setq name (second opt)))
      (merror
       (intl:gettext
        "plot-options-parser: Expecting a symbol for the option name, found: \"~M\"") opt))
    (case name
      ($adapt_depth 
       (setf (getf options :adapt_depth)
             (check-option (cdr opt) #'naturalp "a natural number" 1)))
      ($axes (setf (getf options :axes)
                   (check-option-b (cdr opt) #'axesoptionp "x, y, solid" 1)))
      ($azimuth (if (caddr opt)
                    (setf (caddr opt) (parse-azimuth (caddr opt))))
                (setf (getf options :azimuth)
                      (check-option (cdr opt) #'realp "a real number" 1)))
      ($box (setf (getf options :box)
                  (check-option-boole (cdr opt))))
      ($color_bar_tics
       (if (cddr opt)
         (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
       (setf (getf options :color_bar_tics)
             (check-option-b (cdr opt) #'realp "a real number" 3)))
      ($color (setf (getf options :color)
                    (check-option (cdr opt) #'plotcolorp "a color")))
      ($color_bar  (setf (getf options :color_bar)
                        (check-option-boole (cdr opt))))
      ($elevation (if (caddr opt)
                      (setf (caddr opt) (parse-elevation (caddr opt))))
                  (setf (getf options :elevation)
                        (check-option (cdr opt) #'realp "a real number" 1)))
      ($grid (setf (getf options :grid)
                   (check-option (cdr opt) #'naturalp "a natural number" 2)))
      ($grid2d (setf (getf options :grid2d)
                     (check-option-boole (cdr opt))))
      ($iterations
       (setf (getf options :iterations)
             (check-option (cdr opt) #'naturalp "a natural number" 1)))
      ($label (setf (getf options :label)
                    (check-option-label (cdr opt))))
      ($legend (setf (getf options :legend)
                     (check-option-b (cdr opt) #'stringp "a string")))
      ($logx (setf (getf options :logx)
                   (check-option-boole (cdr opt))))
      ($logy (setf (getf options :logy)
                   (check-option-boole (cdr opt))))
      ($mesh_lines_color
       (setf (getf options :mesh_lines_color)
             (check-option-b (cdr opt) #'plotcolorp "a color" 1)))
      ($nticks (setf (getf options :nticks)
                     (check-option (cdr opt) #'naturalp "a natural number" 1)))
      ($palette (setf (getf options :palette)
                      (check-option-palette (cdr opt))))
      ($plot_format (setf (getf options :plot_format)
                          (check-option-format (cdr opt))))
      ($plot_realpart (setf (getf options :plot_realpart)
                            (check-option-boole (cdr opt))))
      ($point_type (setf (getf options :point_type)
                         (check-option (cdr opt) #'pointtypep "a point type")))
      ($pdf_file (setf (getf options :pdf_file)
                     (check-option (cdr opt) #'stringp "a string" 1)))
      ($png_file (setf (getf options :png_file)
                     (check-option (cdr opt) #'stringp "a string" 1)))
      ($ps_file (setf (getf options :ps_file)
                     (check-option (cdr opt) #'stringp "a string" 1)))
      ($run_viewer (setf (getf options :run_viewer)
                         (check-option-boole (cdr opt))))
      ($same_xy (setf (getf options :same_xy)
                     (check-option-boole (cdr opt))))
      ($same_xyz (setf (getf options :same_xyz)
                      (check-option-boole (cdr opt))))
      ($style (setf (getf options :style)
                    (check-option-style (cdr opt))))
      ($svg_file (setf (getf options :svg_file)
                     (check-option (cdr opt) #'stringp "a string" 1)))
      ($t (setf (getf options :t) (cddr (check-range opt))))
      ($title (setf (getf options :title)
                    (check-option (cdr opt) #'stringp "a string" 1)))
      ($transform_xy (setf (getf options :transform_xy)
                           (check-option-b (cdr opt) #'functionp "a function make_transform" 1)))
      ($x (setf (getf options :x) (cddr (check-range opt))))
      ($xbounds (setf (getf options :xbounds) (cddr (check-range opt))))
      ($xlabel (setf (getf options :xlabel)
                     (check-option (cdr opt) #'string "a string" 1)))
      ($xtics
       (if (cddr opt)
         (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
       (setf (getf options :xtics)
             (check-option-b (cdr opt) #'realp "a real number" 3)))
      ($xvar (setf (getf options :xvar)
                   (check-option (cdr opt) #'string "a string" 1)))
      ($xy_scale
       (if (cddr opt)
         (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
       (setf (getf options :xy_scale)
             (check-option (cdr opt) #'realpositivep
                                     "a positive real number" 2)))
      ($y (setf (getf options :y) (cddr (check-range opt))))
      ($ybounds (setf (getf options :ybounds) (cddr (check-range opt))))
      ($ylabel (setf (getf options :ylabel)
                     (check-option (cdr opt) #'string "a string" 1)))
      ($ytics
       (if (cddr opt)
         (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
       (setf (getf options :ytics)
             (check-option-b (cdr opt) #'realp "a real number" 3)))
      ($yvar (setf (getf options :yvar)
                   (check-option (cdr opt) #'string "a string" 1)))
      ($yx_ratio
       (if (caddr opt)
         (setf (caddr opt) (coerce-float (caddr opt))))
       (setf (getf options :yx_ratio)
             (check-option (cdr opt) #'realp "a real number" 1)))
      ($z (setf (getf options :z) (cddr (check-range opt))))
      ($zlabel (setf (getf options :zlabel)
                     (check-option (cdr opt) #'string "a string" 1)))
      ($zmin
       (if (caddr opt)
         (setf (caddr opt) (coerce-float (caddr opt))))
       (setf (getf options :zmin)
             (check-option-b (cdr opt) #'realp "a real number" 1)))
      ($ztics
       (if (cddr opt)
         (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
       (setf (getf options :ztics)
             (check-option-b (cdr opt) #'realp "a real number" 3)))
      ($gnuplot_4_0 (setf (getf options :gnuplot_4_0)
                          (check-option-boole (cdr opt))))
      ($gnuplot_curve_titles
       (setf (getf options :gnuplot_curve_titles)
             (check-option (cdr opt) #'stringp "a string")))
      ($gnuplot_curve_styles
       (setf (getf options :gnuplot_curve_styles)
             (check-option (cdr opt) #'stringp "a string")))
      ($gnuplot_default_term_command
       (setf (getf options :gnuplot_default_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_dumb_term_command
       (setf (getf options :gnuplot_dumb_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_out_file 
       (setf (getf options :gnuplot_out_file)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_pm3d
       (setf (getf options :gnuplot_pm3d)
             (check-option-boole (cdr opt))))
      ($gnuplot_preamble
       (setf (getf options :gnuplot_preamble)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_postamble
       (setf (getf options :gnuplot_postamble)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_pdf_term_command
       (setf (getf options :gnuplot_pdf_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_png_term_command
       (setf (getf options :gnuplot_png_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_ps_term_command
       (setf (getf options :gnuplot_ps_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ($gnuplot_svg_term_command
       (setf (getf options :gnuplot_svg_term_command)
             (check-option (cdr opt) #'stringp "a string" 1)))
      ;; gnuplot_term is a tricky one: when it is just default, dumb or
      ;; ps, we want it to be a symbol, but when it is more complicated,
      ;; i.e. "ps; size 16cm, 12cm", it must be a string and not a symbol
      ($gnuplot_term 
       (let ((s (caddr opt)))
         (when (stringp s)
           (cond ((string= s "default") (setq s '$default))
                 ((string= s "dumb") (setq s '$dumb))
                 ((string= s "ps") (setq s '$ps))))
         (if (atom s)
             (setf (getf options :gnuplot_term) s)
             (merror
              (intl:gettext "Wrong argument for plot option \"gnuplot_term\". Expecting a string or a symbol but found \"~M\".") s))))
      (t
       (merror
        (intl:gettext "plot-options-parser: unknown plot option: ~M") opt))))

  ;; plots that create a file work better in gnuplot than gnuplot_pipes
  (when (and (eq (getf options :plot_format) '$gnuplot_pipes)
             (or (eq (getf options :gnuplot_term) '$dumb)
                 (getf options :pdf_file) (getf options :png_file)
                 (getf options :ps_file) (getf options :svg_file)))
    (setf (getf options :plot_format) '$gnuplot))

  options)

;; natural numbers predicate
(defun naturalp (n) (or (and (integerp n) (> n 0)) nil))

;; positive real numbers predicate
(defun realpositivep (x) (or (and (realp x) (> x 0)) nil))

;; posible values for the axes option
(defun axesoptionp (o) (if (member o '($x $y $solid)) t nil))

;; the 13 possibilities for the point types
(defun pointtypep (p)
  (if (member p  '($bullet $circle $plus $times $asterisk $box $square
                  $triangle $delta $wedge $nabla $diamond $lozenge)) t nil))

;; Colors can only one of the named colors or a six-digit hexadecimal
;; number with a # suffix.
(defun plotcolorp (color)
  (cond ((and (stringp color)
              (string= (subseq color 0 1) "#")
              (= (length color) 7)
              (ignore-errors (parse-integer (subseq color 1 6) :radix 16)))
         t)
        ((member color '($red $green $blue $magenta $cyan $yellow
                         $orange $violet $brown $gray $black $white))
         t)
        (t nil)))

;; tries to convert az into a floating-point number between 0 and 360
(defun parse-azimuth (az) (mod ($float (meval* az)) 360))

;; tries to convert el into a floating-poitn number between -180 and 180
(defun parse-elevation (el) (- (mod (+ 180 ($float (meval* el))) 360) 180))

;; The following functions check the value of an option returning an atom
;;  when there is only one argument or a list when there are several arguments


;; Checks for one or more items of the same type, using the test given
(defun check-option (option test type &optional count)
  (when count
    (unless (= (1- (length option)) count)
      (merror
       (intl:gettext
        "Wrong number of arguments for plot option \"~M\". Expecting ~M but found ~M.")
       (car option) count (1- (length option)))))
  (dolist (item (cdr option))
    (when (not (funcall test item))
      (merror
       (intl:gettext "Wrong argument for plot option \"~M\". Expecting ~M but found \"~M\".") (car option) type item)))
  (if (= (length option) 2)
      (cadr option)
      (cdr option)))

;; Accepts one or more items of the same type or true or false.
;; When given, n is the maximum number of items.
(defun check-option-b (option test type &optional count)
  (let ((n (- (length option) 1)))
    (when count
      (unless (< n (1+ count))
        (merror
         (intl:gettext
          "Wrong number of arguments for plot option \"~M\". Expecting ~M but found ~M.")
         (car option) count (1- (length option)))))
    (cond 
      ((= n 0) t)
      ((= n 1) (if (or (funcall test (cadr option)) (null (cadr option))
                       (eq (cadr option) t))
                   (cadr option)
                   (merror (intl:gettext "Wrong argument for plot option \"~M\". Expecting ~M, true or false but found \"~M\".") (car option) type (cadr option))))
      ((> n 1)
       (dotimes (i n)
         (unless (funcall test (nth (+ i 1) option))
           (merror
          (intl:gettext "Wrong argument for plot option \"~M\". Expecting ~M but found \"~M\".") (car option) type (nth (+ i 1) option))))
       (cdr option)))))

;; Boolean options can be [option], [option,true] or [option,false]
(defun check-option-boole (option)
  (if (= 1 (length option))
      t
      (if (and (= 2 (length option))
               (or (eq (cadr option) t) (null (cadr option))))
          (cadr option) 
          (merror (intl:gettext "plot option ~M must be either true or false.")
                  (car option)))))

;; label can be either [label, string, real, real] or
;; [label, [string_1, real, real],...,[string_n, real, real]]
(defun check-option-label (option &aux opt)
  (if (not ($listp (cadr option)))
      (setq opt (list (cons '(mlist) (cdr option))))
      (setq opt (cdr option)))
  (dolist (item opt)
    (when (not (and ($listp item) (= 4 (length item)) (stringp (second item))
                    (realp (setf (third item) (coerce-float (third item))))
                    (realp (setf (fourth item) (coerce-float (fourth item))))))
      (merror
       (intl:gettext
        "Wrong argument ~M for option ~M. Must be either [label,\"text\",x,y] or [label, [\"text 1\",x1,y1],...,[\"text n\",xn,yn]]")
       item (car option))))
  opt)

;; one of the possible formats
(defun check-option-format (option &aux formats)
  (if (string= *autoconf-windows* "true")
      (setq formats '($geomview $gnuplot $mgnuplot $openmath $xmaxima))
      (setq formats '($geomview $gnuplot $gnuplot_pipes $mgnuplot $openmath $xmaxima)))
  (unless (member (cadr option) formats)
    (merror
     (intl:gettext
      "Wrong argument ~M for option ~M. Must one of the following symbols: geomview, gnuplot, mgnuplot, xmaxima (or gnuplot_pipes in Unix)")
     (cadr option) (car option)))
  ; $openmath is just a synonym for $xmaxima
  (if (eq (cadr option) '$openmath)
    '$xmaxima
    (cadr option)))

; palette most be one or more Maxima lists starting with the name of one
;; of the 5 kinds: hue, saturation, value, gray or gradient.
(defun check-option-palette (option)
  (if (and (= (length option) 2) (null (cadr option)))
      nil
      (progn
        (dolist (item (cdr option))
          (when (not (and ($listp item)
                          (member (cadr item)
                                  '($hue $saturation $value $gray $gradient))))
            (merror
             (intl:gettext
              "Wrong argument ~M for option ~M. Not a valid palette.")
             item (car option))))
        (cdr option))))

;; style can be one or several of the names of the styles or one or several
;; Maxima lists starting with the name of one of the styles. 
(defun check-option-style (option)
  (if (and (= (length option) 2) (null (cadr option)))
      nil
      (progn
        (let (name parsed)
          (dolist (item (cdr option))
            (if ($listp item)
                (setq name (second item))
              (setq name item))
            (when (not (member name 
                               '($lines $points $linespoints $dots $impulses)))
              (merror
               (intl:gettext
                "Wrong argument ~M for option ~M. Not a valid style")
               name (car option)))
            (setq parsed (cons item parsed)))
          (reverse parsed)))))

;; Transform can be false or the name of a function for the transformation.
(defun check-option-transform (option)
  (if (and (= (length option) 2)
           (or (atom (cadr option)) (null (cadr option))))
      (cadr option)
      (merror
       (intl:gettext
        "Wrong argument ~M for option ~M. Should be either false or the name of function for the transformation") option (car option))))

;; plot2d
;;
;; Examples:
;; plot2d (sec(x), [x, -2, 2], [y, -20, 20], [nticks, 200])$
;;
;; plot2d (exp(3*s), [s, -2, 2], [logy])$
;;
;; plot2d ([parametric, cos(t), sin(t), [t, -%pi, %pi], [nticks, 80]],
;;         [x, -4/3, 4/3])$
;;
;; xy:[[10,.6], [20,.9], [30,1.1], [40,1.3], [50,1.4]]$
;; plot2d ( [ [discrete, xy], 2*%pi*sqrt(l/980) ], [l, 0, 50],
;;          [style, points, lines], [color, red, blue], [point_type, box],
;;          [legend, "experiment", "theory"],
;;          [xlabel, "pendulum's length (cm)"], [ylabel, "period (s)"])$
;;
;; plot2d ( x^2-1, [x, -3, 3], [y, -2, 10], [box, false], [color, red],
;;          [ylabel, "x^2-1"], [plot_format, xmaxima])$

(defmfun $plot2d (fun &optional range &rest extra-options)
  (let (($display2d nil) (*plot-realpart* *plot-realpart*)
        (options (copy-tree *plot-options*)) (i 0)
        output-file gnuplot-term gnuplot-out-file file points-lists)

    ;; 1- Put fun in its most general form: a maxima list with several objects
    ;; that can be expressions (simple functions) and maxima lists (parametric
    ;; functions or discrete sets of points).

    ;; A single parametric or discrete plot is placed inside a maxima list
    (setf (getf options :type) "plot2d")
    (when (and (consp fun)
               (or (eq (second fun) '$parametric) (eq (second fun) '$discrete)))
      (setq fun `((mlist) ,fun)))

    ;; If at this point fun is not a maxima list, it is then a single function
    (unless ($listp fun ) (setq fun `((mlist) ,fun)))

    ;; 2- Get names for the two axis and values for xmin and xmax if needed.

    ;; If any of the objects in the fun list is a simple function,
    ;; the range option is mandatory and will provide the name of
    ;; the horizontal axis and the values of xmin and xmax.
    (let ((no-range-required t) small huge)
      #-clisp (setq small (- (/ most-positive-flonum 1024)))
      #+clisp (setq small (- (/ most-positive-double-float 1024.0)))
      #-clisp (setq huge (/ most-positive-flonum 1024))
      #+clisp (setq huge (/ most-positive-double-float 1024.0))
      (setf (getf options :ybounds) (list small huge))
      (dolist (subfun (rest fun))
        (if (not ($listp subfun))
            (setq no-range-required nil))) 
      (unless no-range-required
        (setq range (check-range range))
        (setf (getf options :xlabel) (ensure-string (second range)))
        (setf (getf options :x) (cddr range)))
      (when no-range-required
        ;; Make the default ranges on X nd Y large so parametric plots
        ;; don't get prematurely clipped. Don't use most-positive-flonum
        ;; because draw2d will overflow.
        (setf (getf options :xbounds) (list small huge))
        (when range
          ;; second argument was really a plot option, not a range
          (setq extra-options (cons range extra-options)))))

    ;; When only one function is being plotted:
    ;; If a simple function use, its name for the vertical axis.
    ;; If parametric, give the axes the names of the two parameters.
    ;; If discrete points, name the axes x and y.
    (when (= (length fun) 2)
      (let ((v (second fun)) label)
        (cond ((atom v) 
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label)))
              ((eq (second v) '$parametric)
               (setq label (coerce (mstring (third v)) 'string))
               (if (< (length label) 80)
                   (setf (getf options :xlabel) label))
               (setq label (coerce (mstring (fourth v)) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label)))
              ((eq (second v) '$discrete)
               (setf (getf options :xlabel) "x")
               (setf (getf options :ylabel) "y"))
              (t
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf options :ylabel) label))))))

    ;; Parse the given options into the options list
    (setq options (plot-options-parser extra-options options))
    (when (getf options :y) (setf (getf options :ybounds) (getf options :y)))

    ;; Remove axes labels when no box is used in gnuplot
    (when (and (member :box options) (not (getf options :box))
		(not (eq (getf options :plot_format) '$xmaxima)))
      (remf options :xlabel)
      (remf options :ylabel))


    (let ((xmin (first (getf options :x))) (xmax (second (getf options :x))))
      (when
          (and (getf options :logx) xmin xmax)
        (if (> xmax 0)
            (when (<= xmin 0)
              (let ((revised-xmin (/ xmax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logx' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-xmin xmin)
                (setf (getf options :x) (list revised-xmin xmax))
                (setq range `((mlist) ,(second range) ,revised-xmin ,xmax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logx' in effect; found: ~M") xmax))))

    (let ((ymin (first (getf options :y)))
          (ymax (second (getf options :y))))
      (when (and (getf options :logy) ymin ymax)
        (if (> ymax 0)
            (when (<= ymin 0)
              (let ((revised-ymin (/ ymax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logy' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-ymin ymin)
                (setf (getf options :y) (list revised-ymin ymax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logy' in effect; found: ~M") ymax))))

    (setq *plot-realpart* (getf options :plot_realpart))

    ;; Compute points to plot for each element of FUN.
    ;; If no plottable points are found, return immediately from $PLOT2D.

    (setq points-lists
          (mapcar #'(lambda (f) (cdr (draw2d f range options))) (cdr fun)))
    (when (= (count-if #'(lambda (x) x) points-lists) 0)
      (mtell (intl:gettext "plot2d: nothing to plot.~%"))
      (return-from $plot2d))

    (setq gnuplot-term (getf options :gnuplot_term))
    (setf gnuplot-out-file (getf options :gnuplot_out_file))
    (if (and (find (getf options :plot_format) '($gnuplot_pipes $gnuplot))
             (eq gnuplot-term '$default) gnuplot-out-file)
        (setq file (plot-file-path gnuplot-out-file))
        (setq file
              (plot-file-path
               (format nil "maxout~d.~(~a~)"
		       (getpid)
                       (ensure-string (getf options :plot_format))))))

    ;; old function $plot2dopen incorporated here
    (case (getf options :plot_format)
      ($xmaxima
       (when (getf options :ps_file)
         (setq output-file (list (getf options :ps_file))))
       (show-open-plot
        (with-output-to-string
            (st)
          (xmaxima-print-header st options)
          (let ((legend (getf options :legend))
                (colors (getf options :color))
                (styles (getf options :style)) style plot-name)
            (unless (listp legend) (setq legend (list legend)))
            (unless (listp colors) (setq colors (list colors)))
            (unless (listp styles) (setq styles (list styles)))
            (loop for f in (cdr fun) for points-list in points-lists do
                 (when points-list
                   (if styles
                       (progn
                         (setq style (nth (mod i (length styles)) styles))
                         (setq style (if ($listp style) (cdr style) `(,style))))
                       (setq style nil))
                   (incf i)
                   (if (member :legend options)
                       ;; a legend has been given in the options
                       (setq plot-name
                             (if (first legend)
                                 (ensure-string
                                  (nth (mod (- i 1) (length legend)) legend))
                                 nil)) ; no legend if option [legend,false]
                       (if (= 2 (length fun))
                           (progn
                             (setq plot-name nil) ;no legend for single function
                             (format st " {nolegend 1}"))
                           (setq plot-name
                                 (let ((string ""))
                                   (cond
                                     ((atom f) 
                                      (setq
                                       string (coerce (mstring f) 'string)))
                                     ((eq (second f) '$parametric)
                                      (setq
                                       string 
                                       (concatenate 
                                        'string
                                        (coerce (mstring (third f)) 'string)
                                        ", " (coerce (mstring (fourth f)) 'string))))
                                     ((eq (second f) '$discrete)
                                      (setq string
                                            (format nil "discrete~a" i)))
                                     (t
                                      (setq string
                                            (coerce (mstring f) 'string))))
                                   (cond ((< (length string) 80) string)
                                         (t (format nil "fun~a" i)))))))
                   (when plot-name 
                     (format st " {label ~s}" plot-name))
                   (format st " ~a~%" (xmaxima-curve-style style colors i))
                   (format st " {xversusy~%")
                   (let ((lis points-list))
                     (loop while lis
                        do
                          (loop while (and lis (not (eq (car lis) 'moveto)))
                             collecting (car lis) into xx
                             collecting (cadr lis) into yy
                             do (setq lis (cddr lis))
                             finally
                             ;; only output if at least two points for line
                               (when (cdr xx)
                                 (tcl-output-list st xx)
                                 (tcl-output-list st yy)))
                        ;; remove the moveto
                          (setq lis (cddr lis))))
                   (format st "}"))))
          (format st "} "))
        file)
       (cons '(mlist) (cons file output-file)))
      (t
       (with-open-file (st file :direction :output :if-exists :supersede)
         (case (getf options :plot_format)
           ($gnuplot
            (setq output-file (gnuplot-print-header st options))
            (format st "plot")
            (when (getf options :x)
              (format st " [~{~g~^ : ~}]" (getf options :x)))
            (when (getf options :y)
              (unless (getf options :x)
                (format st " []")) 
              (format st " [~{~g~^ : ~}]" (getf options :y))))
           ($gnuplot_pipes
            (check-gnuplot-process)
            ($gnuplot_reset)
            (setq output-file (gnuplot-print-header *gnuplot-stream* options))
            (setq *gnuplot-command* (format nil "plot"))
            (when (getf options :x)
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~{~g~^ : ~}]" (getf options :x)))))
            (when (getf options :y) 
              (unless (getf options :x)
                (setq *gnuplot-command*
                      ($sconcat *gnuplot-command* (format nil " []"))))
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~{~g~^ : ~}]"  (getf options :y)))))))
         (let ((legend (getf options :legend))
               (colors (getf options :color))
               (types (getf options :point_type))
               (styles (getf options :style))
               style plot-name)
           (unless (listp legend) (setq legend (list legend)))
           (unless (listp colors) (setq colors (list colors)))
           (unless (listp styles) (setq styles (list styles)))
           (loop for v in (cdr fun) for points-list in points-lists do
                (when points-list
                  (case (getf options :plot_format)
                    ($gnuplot_pipes
                     (if (> i 0)
                         (setq *gnuplot-command*
                               ($sconcat *gnuplot-command* ", ")))
                     (setq *gnuplot-command*
                           ($sconcat *gnuplot-command* 
                                     (format nil "~s index ~a " file i)))))
                  (if styles
                      (setq style (nth (mod i (length styles)) styles))
                      (setq style nil))
                  (when ($listp style) (setq style (cdr style)))
                  (incf i)
                  (if (member :legend options)
                      ;; a legend has been defined in the options
                      (setq plot-name
                            (if (first legend)
                                (ensure-string
                                 (nth (mod (- i 1) (length legend)) legend))
                                nil)) ; no legend if option [legend,false]
                      (if (= 2 (length fun))
                          (setq plot-name nil) ; no legend if just one function
                          (setq plot-name
                                (let ((string ""))
                                  (cond ((atom v) 
                                         (setq string
                                               (coerce (mstring v) 'string)))
                                        ((eq (second v) '$parametric)
                                         (setq
                                          string 
                                          (concatenate
                                           'string
                                           (coerce (mstring (third v)) 'string)
                                           ", "
                                           (coerce (mstring (fourth v)) 'string))))
                                        ((eq (second v) '$discrete)
                                         (setq
                                          string (format nil "discrete~a" i)))
                                        (t
                                         (setq string 
                                               (coerce (mstring v) 'string))))
                                  (cond ((< (length string) 80) string)
                                        (t (format nil "fun~a" i)))))))
                  (case (getf options :plot_format)
                    ($gnuplot
                     (when (> i 1) (format st ","))
                     (format st " '-'")
                     (if plot-name
                         (format st " title ~s" plot-name)
                         (format st " notitle"))
                     (format st " ~a"
                             (gnuplot-curve-style style colors types i)))
                    ($gnuplot_pipes
                     (setq *gnuplot-command*
                           ($sconcat
                            *gnuplot-command*
                            (if plot-name 
                                (format
                                 nil " title ~s ~a" plot-name 
                                 (gnuplot-curve-style style colors types i))
                                (format
                                 nil " notitle ~a"
                                 (gnuplot-curve-style style colors types i)
                                 )))))))))
         (case (getf options :plot_format)
           ($gnuplot
            (format st "~%"))
           ($gnuplot_pipes
            (format st "~%")))
         (setq i 0)
         (loop for v in (cdr fun) for points-list in points-lists do
              (when points-list
                (incf i)
                (case (getf options :plot_format)
                  ($gnuplot
                   (if (> i 1)
                       (format st "e~%")))
                  ($gnuplot_pipes
                   (if (> i 1)
                       (format st "~%~%")))
                  ($mgnuplot
                   (format st "~%~%# \"Fun~a\"~%" i))
                  )
                (let (in-discontinuity points)
                  (loop for (v w) on points-list by #'cddr
                     do
                       (cond ((eq v 'moveto)
                              (cond 
                                ((find (getf options :plot_format) '($gnuplot_pipes $gnuplot))
                                 ;; A blank line means a discontinuity
                                 (if (null in-discontinuity)
                                     (progn
                                       (format st "~%")
                                       (setq in-discontinuity t))))
                                ((equal (getf options :plot_format) '$mgnuplot)
                                 ;; A blank line means a discontinuity
                                 (format st "~%"))
                                (t
                                 (format st "move "))))
                             (t (format st "~g ~g ~%" v w)
                                (setq points t)
                                (setq in-discontinuity nil))))
                  (if (and (null points)
                           (first (getf options :x))
                           (first (getf options :y)))
                      (format
                       st "~g ~g ~%"
                       (first (getf options :x))
                       (first (getf options :y))))))))))
    
    (case (getf options :plot_format)
      ($gnuplot 
       (gnuplot-process options file output-file))
      ($gnuplot_pipes
       (send-gnuplot-command *gnuplot-command*))
      ($mgnuplot 
       ($system (concatenate 'string *maxima-plotdir* "/" $mgnuplot_command) 
                (format nil " -plot2d ~s -title ~s" file "Fun1"))))
    (cons '(mlist) (cons file output-file))))


(defun msymbolp (x)
  (and (symbolp x) (char= (char (symbol-value x) 0) #\$)))


(defmfun $tcl_output (lis i &optional (skip 2))
  (when (not (typep i 'fixnum))
    (merror
      (intl:gettext "tcl_ouput: second argument must be an integer; found ~M")
                    i))
  (when (not ($listp lis))
    (merror
      (intl:gettext "tcl_output: first argument must be a list; found ~M") lis))
  (format *standard-output* "~% {")
  (cond (($listp (second lis))
         (loop for v in lis
                do
                (format *standard-output* "~,10g " (nth i v))))
        (t
         (setq lis (nthcdr i lis))
         (loop  with v = lis  while v
                 do
                 (format *standard-output* "~,10g " (car v))
                 (setq v (nthcdr skip v)))))
  (format *standard-output* "~% }"))
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

(defun check-range (range &aux tem a b)
  (or (and ($listp range)
           (setq tem (cdr range))
           (or (symbolp (car tem)) ($subvarp (car tem)))
           (numberp (setq a ($float (meval* (second tem)))))
           (numberp (setq b ($float (meval* (third tem)))))
           (< a b))
      (if range
          (merror 
           (intl:gettext "plotting: range must be of the form [variable, min, max]; found: ~M")
           range)
          (merror 
           (intl:gettext "plotting: no range given; must supply range of the form [variable, min, max]"))))
  `((mlist) ,(car tem) ,(float a) ,(float b)))

(defmfun $zero_fun (x y) x y 0.0)

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

(defun output-points-tcl (dest pl m)
  (format dest " {matrix_mesh ~%")
  ;; x y z are done separately:
  (loop for off from 0 to 2
     with ar = (polygon-pts pl)
     with  i of-type fixnum = 0
     do (setq i off)
       (format dest "~%{")
       (loop 
	  while (< i (length ar))
	  do (format dest "~% {")
	    (loop for j to m
	       do (print-pt (aref ar i))
		 (setq i (+ i 3)))
	    (format dest "}~%"))
       (format dest "}~%"))
  (format dest "}~%"))

(defun show-open-plot (ans file)
  (cond ($show_openplot
         (with-open-file (st1 (plot-temp-file (format nil "maxout~d.xmaxima" (getpid))) :direction :output :if-exists :supersede)
           (princ  ans st1))
         ($system (concatenate 'string *maxima-prefix* 
                               (if (string= *autoconf-windows* "true") "\\bin\\" "/bin/") 
                               $xmaxima_plot_command)
		  #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
		  (format nil " ~s &" file)
		  #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
		  file))
        (t (princ ans) "")))


;; contour_plot -- set some parameters for Gnuplot and punt to plot3d
;;
;; We go to some trouble here to avoid clobbering the Gnuplot preamble
;; specified by the user, either as a global option (via set_plot_option)
;; or specified in arguments to contour_plot. Just append or prepend
;; the parameters for contour plotting to the user-specified preamble.
;; Assume that arguments take precedence over global options.
;;
;; contour_plot knows how to set parameters only for Gnuplot.
;; If the plot_format is not a Gnuplot format, complain.
;;
;; Examples:
;;
;;   contour_plot (x^2 + y^2, [x, -4, 4], [y, -4, 4]);
;;   contour_plot (sin(y) * cos(x)^2, [x, -4, 4], [y, -4, 4]);
;;   F(x, y) := x^3 + y^2;
;;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;;   contour_plot (F, [u, -4, 4], [v, -4, 4], [gnuplot_preamble, "set size ratio -1"]);
;;   set_plot_option ([gnuplot_preamble, "set cntrparam levels 12"]);
;;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;;   set_plot_option ([plot_format, xmaxima]);
;;   contour_plot (F, [u, -4, 4], [v, -4, 4]); => error: must be gnuplot format
;;   contour_plot (F, [u, -4, 4], [v, -4, 4], [plot_format, gnuplot]);

(defmfun $contour_plot (expr &rest optional-args)
  (let*
      ((plot-format-in-options (getf *plot-options* :plot_format))
       (plot-format-in-arguments
        (caddar (member '$plot_format optional-args :key #'cadr)))
       (preamble-in-plot-options (getf *plot-options* :gnuplot_preamble))
       (preamble-in-arguments
        (caddar (member '$gnuplot_preamble optional-args :key #'cadr)))
       (contour-preamble "set contour; unset surface; set view map")
       (gnuplot-formats '($gnuplot $gnuplot_pipes))
       preamble)
    ;; Ensure that plot_format is some gnuplot format.
    ;; Argument takes precedence over global option.
  
    (if (or
         (and plot-format-in-arguments
              (not (member plot-format-in-arguments gnuplot-formats :test #'eq)))
         (and (not plot-format-in-arguments)
              (not (member plot-format-in-options gnuplot-formats :test #'eq))))
        (progn
          (mtell(intl:gettext "contour_plot: plot_format = ~a not recognized; must be a gnuplot format.~%")
                (ensure-string (or plot-format-in-arguments plot-format-in-options)))
          (return-from $contour_plot)))
    
    ;; Prepend contour preamble to preamble in arguments (if given)
    ;; and pass concatenated preamble as an argument to plot3d.
    ;; Otherwise if there is a global option preamble, 
    ;; append contour preamble to global option preamble.
    ;; Otherwise just set global option preamble to the contour preamble.
    
    ;; All this complication is to avoid clobbering the preamble
    ;; if one was specified somehow (either global option or argument).
      
    (if preamble-in-arguments
        (progn
          (setq optional-args
                (remove-if #'(lambda (e) (and ($listp e) (eq ($first e) '$gnuplot_preamble))) optional-args))
          (setq preamble
                ($sconcat contour-preamble (format nil "~%")
                          preamble-in-arguments)))
        (if preamble-in-plot-options
            (setf preamble
                  ($sconcat preamble-in-plot-options (format nil "~%")
                            contour-preamble))
            (setq preamble contour-preamble)))
    (apply #'$plot3d
           (append (list expr)
                   optional-args
                   (list '((mlist) $palette nil))
                   (list `((mlist) $gnuplot_preamble ,preamble))))))

;; plot3d
;;
;; Examples:
;; plot3d (2^(-u^2 + v^2), [u, -3, 3], [v, -2, 2], [palette, false]);
;;
;; plot3d ( log ( x^2*y^2 ), [x, -2, 2], [y, -2, 2], [grid, 29, 29]);
;;
;; expr_1: cos(y)*(10.0+6*cos(x))$
;; expr_2: sin(y)*(10.0+6*cos(x))$
;; expr_3: -6*sin(x)$
;; plot3d ([expr_1, expr_2, expr_3], [x, 0, 2*%pi], [y, 0, 2*%pi],
;;         ['grid, 40, 40], [z,-8,8]);
;;
;; plot3d (cos (-x^2 + y^3/4), [x, -4, 4], [y, -4, 4],
;; [mesh_lines_color, false], [elevation, 0], [azimuth, 0], [colorbox, true],
;; [grid, 150, 150])$

;; spherical: make_transform ([th, phi,r], r*sin(phi)*cos(th),
;;            r*sin(phi)*sin(th), r*cos(phi))$
;; plot3d ( 5, [th, 0, 2*%pi], [phi, 0, %pi], [transform_xy, spherical],
;;          [palette, [value, 0.65, 0.7, 0.1, 0.9]], [plot_format,xmaxima]);
;;
;; V: 1 / sqrt ( (x+1)^2+y^2 ) - 1 / sqrt( (x-1)^2+y^2 )$
;; plot3d ( V, [x, -2, 2], [y, -2, 2], [z, -4, 4])$


(defmfun $plot3d
    (fun &rest extra-options
     &aux
       lvars trans xrange yrange
       functions exprn domain tem (options (copy-tree *plot-options*))
       ($in_netmath $in_netmath)
       (*plot-realpart* *plot-realpart*)
       gnuplot-term gnuplot-out-file file titles output-file
       (usage (intl:gettext
"plot3d: Usage.
To plot a single function f of 2 variables v1 and v2:
  plot3d (f, [v1, min, max], [v2, min, max], options)
A parametric representation of a surface with parameters v1 and v2:
  plot3d ([f1, f2, f3], [v1, min, max], [v2, min, max], options)
Several functions depending on the two variables v1 and v2:
  plot3d ([f1, f2, ..., fn, [v1, min, max], [v2, min, max]], options)")))
  
  (setf (getf options :type) "plot3d")
  
  ;; Ensure that fun is a list of expressions and maxima lists, followed
  ;; by a domain definition
  (if ($listp fun)
      (if (= 1 (length (check-list-plot3d fun)))
          ;; fun consisted of a single parametric expression
          (setq fun `(,fun ,(pop extra-options) ,(pop extra-options)))
          ;; fun was a maxima list with several independent surfaces
          (pop fun))
      ;; fun consisted of a single expression
      (setq fun `(,fun ,(pop extra-options) ,(pop extra-options))))
  
  ;; go through all the independent surfaces creating the functions stack
  (loop
     (setq exprn (pop fun))
     (if ($listp exprn)
         (progn
           (setq domain (check-list-plot3d exprn))
           (case (length domain)
             (1
              ;; exprn is a parametric representation of a surface
              (let (vars1 vars2 vars3)
                ;; list fun should have two valid ranges after exprn
                (setq xrange (check-range (pop fun)))
                (setq yrange (check-range (pop fun)))
                ;; list of the two variables for the parametric equations
                (setq lvars `((mlist),(second xrange) ,(second yrange)))
                ;; make sure that the 3 parametric equations depend only
                ;; on the two variables in lvars
                (setq vars1
                      ($listofvars (mfuncall
                                    (coerce-float-fun (second exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq vars2
                      ($listofvars (mfuncall
                                    (coerce-float-fun (third exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq vars3
                      ($listofvars (mfuncall
                                    (coerce-float-fun (fourth exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq lvars ($listofvars `((mlist) ,vars1 ,vars2 ,vars3)))
                (if (<= ($length lvars) 2)
                    ;; we do have a valid parametric set. Push it into
                    ;; the functions stack, along with their domain
                    (progn
                      (push `(,exprn ,xrange ,yrange) functions)
                      ;; add a title to the titles stack
                      (push "Parametric function" titles)
                      ;; unknown variables in the parametric equations
                      ;; ----- GNUPLOT 4.0 WORK-AROUND -----
                      (when (and ($constantp (fourth exprn))
                                 (getf options :gnuplot_4_0))
                        (setf (getf options :const_expr)
                              ($float (meval (fourth exprn))))))
                    (merror
                     (intl:gettext "plot3d: there must be at most two variables; found: ~M")
                     lvars))))
             
             (3
              ;; expr is a simple function with its own domain. Push the
              ;; function and its domain into the functions stack
              (setq xrange (second domain))
              (setq yrange (third domain))
              (push `(,(second exprn) ,xrange ,yrange) functions)
              ;; push a title for this plot into the titles stack
              (if (< (length (ensure-string (second exprn))) 36)
                  (push (ensure-string (second exprn)) titles)
                  (push "Function" titles)))
             
             (t
              ;; syntax error. exprn does not have the expected form
              (merror
               (intl:gettext "plot3d: argument must be a list of three expressions; found: ~M")
               exprn))))
         (progn
           ;; exprn is a simple function, defined in the global domain.
           (if (and (getf options :xvar) (getf options :yvar))
               ;; the global domain has already been defined; use it.
               (progn
                 (setq xrange `((mlist) ,(getf options :xvar)
                                ,(first (getf options :x))
                                ,(second (getf options :x))))
                 (setq yrange `((mlist) ,(getf options :yvar)
                                ,(first (getf options :y))
                                ,(second (getf options :y)))))
               ;; the global domain should be defined by the last two lists
               ;; in fun. Extract it and check whether it is valid.
               (progn
                 (setq
                  domain
                  (check-list-plot3d (append `((mlist) ,exprn) (last fun 2))))
                 (setq fun (butlast fun 2))
                 (if (= 3 (length domain))
                     ;; domain is valid. Make it the global one.
                     (progn
                       (setq xrange (second domain))
                       (setq yrange (third domain))
                       (setf (getf options :xvar) (second xrange))
                       (setf (getf options :x) (cddr xrange))
                       (setf (getf options :yvar) (second yrange))
                       (setf (getf options :y) (cddr yrange)))
                     (merror usage))))
           ;; ----- GNUPLOT 4.0 WORK-AROUND -----
           (when (and ($constantp exprn) (getf options :$gnuplot_4_0))
             (setf (getf options :const_expr) ($float (meval exprn))))
           ;; push the function and its domain into the functions stack
           (push `(,exprn ,xrange ,yrange) functions)
           ;; push a title for this plot into the titles stack
           (if (< (length (ensure-string exprn)) 36)
               (push (ensure-string exprn) titles)
               (push "Function" titles))))
     (when (= 0 (length fun)) (return)))
  
  ;; recover the original ordering for the functions and titles stacks
  (setq functions (reverse functions))
  (setq titles (reverse titles))
  
  ;; parse the options given to plot3d
  (setq options (plot-options-parser extra-options options))
  (setq tem (getf options :transform_xy))
  (setq *plot-realpart* (getf options :plot_realpart))
  
  ;; set up the labels for the axes, unless no box is being shown
  (unless (and (member :box options) (not (getf options :box)))
    (if (and (getf options :xvar) (getf options :yvar) (null tem))
	(progn
	  ;; Don't set xlabel (ylabel) if the user specified one.
	  (unless (getf options :xlabel)
	    (setf (getf options :xlabel) (ensure-string (getf options :xvar))))
	  (unless (getf options :ylabel)
	    (setf (getf options :ylabel) (ensure-string (getf options :yvar)))))
	(progn
	  (setf (getf options :xlabel) "x")
	  (setf (getf options :ylabel) "y")))
    (unless (getf options :zlabel) (setf (getf options :zlabel) "z")))
  
  ;; x and y should not be bound, when an xy transformation function is used
  (when tem (remf options :x) (remf options :y))
  
  ;; Name of the gnuplot commands file and output file
  (setf gnuplot-term (getf options :gnuplot_term))
  (setf gnuplot-out-file (getf options :gnuplot_out_file))
  (if (and (find (getf options :plot_format) '($gnuplot_pipes $gnuplot))
           (eq gnuplot-term '$default) gnuplot-out-file)
      (setq file (plot-file-path gnuplot-out-file))
      (setq file
            (plot-file-path
             (format nil "maxout~d.~(~a~)"
		     (getpid)
                     (ensure-string (getf options :plot_format))))))

  (and $in_netmath 
       (setq $in_netmath (eq (getf options :plot_format) '$xmaxima)))
  
  ;; Set up the output file stream
  (let (($pstream
         (cond ($in_netmath *standard-output*)
               (t (open file :direction :output :if-exists :supersede))))
        (palette (getf options :palette))
        (legend (getf options :legend)) (n (length functions)))
    ;; titles will be a list. The titles given in the legend option
    ;; will have priority over the titles generated by plot3d.
    ;; no legend if option [legend,false]
    (unless (listp legend) (setq legend (list legend)))
    (when (member :legend options)  ;; use titles given by legend option
      (if (first legend) (setq titles legend)) (setq titles nil))
    
    (unwind-protect
         (case (getf options :plot_format)
           ($gnuplot
            (setq output-file (gnuplot-print-header $pstream options))
            (when (and (member :gnuplot_pm3d options)
                       (not (getf options :gnuplot_pm3d)))
              (setq palette nil))
            (format
             $pstream "~a"
             (gnuplot-plot3d-command "-" palette
                                     (getf options :gnuplot_curve_styles)
                                     (getf options :color)
                                     titles n)))
           ($gnuplot_pipes
            (when (and (member :gnuplot_pm3d options)
                       (not (getf options :gnuplot_pm3d)))
              (setq palette nil))
            (check-gnuplot-process)
            ($gnuplot_reset)
            (setq output-file (gnuplot-print-header *gnuplot-stream* options))
            (setq 
             *gnuplot-command*
             (gnuplot-plot3d-command file palette
                                     (getf options :gnuplot_curve_styles)
                                     (getf options :color)
                                     titles n)))
           ($xmaxima
            (when (getf options :ps_file)
              (setq output-file (list (getf options :ps_file))))
            (xmaxima-print-header $pstream options))
           ($geomview
            (format $pstream "LIST~%")))
      
      ;; generate the mesh points for each surface in the functions stack
      (let ((i 0))
        (dolist (f functions)
          (setq i (+ 1 i))
          (setq fun (first f))
          (setq xrange (second f))
          (setq yrange (third f))
          (if ($listp fun)
              (progn
                (setq trans
                      ($make_transform `((mlist) ,(second xrange)
                                         ,(second yrange) $z)
                                       (second fun) (third fun) (fourth fun)))
                (setq fun '$zero_fun))
              (let*
                ((x0 (third xrange))
                 (x1 (fourth xrange))
                 (y0 (third yrange))
                 (y1 (fourth yrange))
                 (xmid (+ x0 (/ (- x1 x0) 2)))
                 (ymid (+ y0 (/ (- y1 y0) 2))))
                (setq lvars `((mlist) ,(second xrange) ,(second yrange)))
                (setq fun (coerce-float-fun fun lvars))
                ;; Evaluate FUN at a typical point, taken here as the middle point of the range.
                ;; This approach is somewhat unreliable since this is only looking at a single point.
                ;; Call FUN with numerical arguments since with symbolic arguments,
                ;; FUN may fail due to trouble computing real/imaginary parts for complicated expressions,
                ;; or FUN may be undefined for symbolic arguments (e.g. functions defined by numerical methods).
                (when (cdr ($listofvars (mfuncall fun xmid ymid)))
                  (mtell (intl:gettext "plot3d: expected <expr. of v1 and v2>, [v1, min, max], [v2, min, max]~%"))
                  (mtell (intl:gettext "plot3d: keep going and hope for the best.~%")))))
          (let* ((pl
                  (draw3d
                   fun (third xrange) (fourth xrange) (third yrange)
                   (fourth yrange) (first (getf options :grid))
                   (second (getf options :grid))))
                 (ar (polygon-pts pl))
                 (colors (getf options :color))
                 (palettes (getf options :palette)))
            (declare (type (cl:array t) ar))
            
            (if trans (mfuncall trans ar))
            (if tem (mfuncall tem ar))
            
            (case (getf options :plot_format)
              ($gnuplot
               (when (> i 1) (format $pstream "e~%"))
               (output-points pl (first (getf options :grid))))
              ($gnuplot_pipes
               (when (> i 1) (format $pstream "~%~%"))
               (output-points pl (first (getf options :grid))))
              ($mgnuplot
               (when (> i 1) (format $pstream "~%~%# \"Fun~a\"~%" i))
               (output-points pl (first (getf options :grid))))
              ($xmaxima
               (if palettes
                   (format $pstream " ~a~%" (xmaxima-palettes palettes i))
                   (format $pstream " {mesh_lines ~a}"
                           (xmaxima-color colors i)))
               (output-points-tcl $pstream pl (first (getf options :grid))))
              ($geomview
               (format $pstream "{ appearance { +smooth }~%MESH ~a ~a ~%"
                       (+ 1 (first (getf options :grid)))
                       (+ 1 (second (getf options :grid))))
               (output-points pl nil)
               (format $pstream "}~%"))))))
      
      ;; close the stream and plot..
      (cond ($in_netmath (format $pstream "}~%") (return-from $plot3d ""))
            ((eql (getf options :plot_format) '$xmaxima)
             (format $pstream "}~%")
             (close $pstream))
            (t (close $pstream)
               (setq $pstream nil))))
    (if (eql (getf options :plot_format) '$gnuplot)
        (gnuplot-process options file output-file)
        (cond ((getf options :run_viewer)
               (case (getf options :plot_format)
                 ($xmaxima
                  ($system
                   (concatenate
                    'string *maxima-prefix* 
                    (if (string= *autoconf-windows* "true") "\\bin\\" "/bin/")
                    $xmaxima_plot_command) 
                   #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
                   (format nil " ~s &" file)
                   #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
                   file))
                 ($geomview 
                  ($system $geomview_command
                           (format nil " \"~a\"" file)))
                 ($gnuplot_pipes
                  (send-gnuplot-command *gnuplot-command*))
                 ($mgnuplot 
                  ($system
                   (concatenate
                    'string *maxima-plotdir* "/" $mgnuplot_command)
                   (format nil " -parametric3d \"~a\"" file))))))))
    (cons '(mlist) (cons file output-file)))

;; Given a Maxima list with 3 elements, checks whether it represents a function
;; defined in a 2-dimensional domain or a parametric representation of a
;; 3-dimensional surface, depending on two parameters.
;; The return value will be a Maxima list if the test is succesfull or nil
;; otherwise.
;; In the case of a function and a 2D domain, it returns the domain, validated.
;; When it is a parametric representation it returns an empty Maxima list.
;;
(defun check-list-plot3d (lis)
  (let (xrange yrange)
    ;; Makes sure list has the form ((mlist) $atom item1 item2)
    (unless (and ($listp lis) (= 3 ($length lis)) (not ($listp (second lis))))
      (return-from check-list-plot3d nil))
    ;; we might have a function with domain or a parametric representation
    (if ($listp (third lis))
        ;; lis is probably a function with a valid domain
        (if ($listp (fourth lis))
            ;; we do have a function and a domain. Return the domain
            (progn
              (setq xrange (check-range (third lis)))
              (setq yrange (check-range (fourth lis)))
              (return-from check-list-plot3d `((mlist) ,xrange ,yrange)))
            ;; wrong syntax: [expr1, list, expr2]
            (return-from check-list-plot3d nil))
        ;; lis is probably a parametric representation
        (if ($listp (fourth lis))
            ;; wrong syntax: [expr1, expr2, list]
            (return-from check-list-plot3d nil)
            ;; we do have a parametric representation. Return an empty list
            (return-from check-list-plot3d '((mlist)))))))

