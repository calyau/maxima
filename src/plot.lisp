;;Copyright William F. Schelter 1990, All Rights Reserved
;;

(in-package :maxima)

;; For function defmvar :setting-predicate.  Checks that the argument
;; is a string.  The second value is the message to use when the
;; argument is not a string.
(defun string-predicate (val)
  (values (stringp val)
          "must be a string"))

(defmvar $geomview_command "geomview"
  "The command (a string) that will run geomview"
  :setting-predicate #'string-predicate)
(defmvar $xmaxima_plot_command "xmaxima"
  "The command (a string) that will run xmaxima"
  :setting-predicate #'string-predicate)

#|
Examples

/* plot of z^(1/3)...*/
plot3d(r^.33*cos(th/3),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],'geomview);

/* plot of z^(1/2)...*/
plot3d(r^.5*cos(th/2),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],'xmaxima);

/* moebius */
plot3d([cos(x)*(3+y*cos(x/2)),sin(x)*(3+y*cos(x/2)),y*sin(x/2)],[x,-%pi,%pi],[y,-1,1],['grid,50,15]);

/* klein bottle */
plot3d([5*cos(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0) - 10.0,
-5*sin(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0),
5*(-sin(x/2)*cos(y)+cos(x/2)*sin(2*y))],[x,-%pi,%pi],[y,-%pi,%pi],
['grid,40,40]);
/* torus */
plot3d([cos(y)*(10.0+6*cos(x)), sin(y)*(10.0+6*cos(x)),-6*sin(x)],
 [x,0,2*%pi],[y,0,2*%pi],['grid,40,40]);
|#

;; Each plot is described by an object whose class depends on the
;; graphic program used to visualize it (option "plot_format").
;; We currently have 3 classes for Gnuplot, Xmaxima and Geomview

(defclass gnuplot-plot ()
  ((data :initarg :data :initform "")
   (pipe :initarg :pipe :initform nil)))

(defclass xmaxima-plot ()
  ((data :initarg :data :initform "")
   (pipe :initarg :pipe :initform nil)))

(defclass geomview-plot ()
  ((data :initarg :data :initform "")
   (pipe :initarg :pipe :initform nil)))

;; These are the methods that each plot class should define.
;; For each class (plot plot_format) there is a corresponding file
;;      plot_format_def.lisp
;; where those methods are defined for the particular class.

(defgeneric plot-preamble (plot options)
    (:documentation "Plots the preamble for a plot."))

(defgeneric plot2d-command (plot fun options range)
    (:documentation "Writes the command that creates a plot."))

(defgeneric plot3d-command (plot functions options titles)
    (:documentation "Writes the command that creates a plot."))

(defgeneric plot-shipout (plot options &optional output-file)
    (:documentation "Sends the plot commands to the graphic program."))

;; Convenience functions used to check values of plot arguments and options

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

;; Global plot options list; this is a property list.. It is not a
;; Maxima variable, to discourage users from changing it directly; it
;; should be changed via set_plot_option.
;; The default values will bet set below, after the definition of
;; function $reset_plot_options.
(defvar *plot-options*
  "Default options for the plotting programs")

;; Apparently Wxmaxima needs a default plot_options Maxima list pre-defined.
;; We will then create such list with minimum content.
;; (TO-DO: check whether recent versions of Wxmaxima still require that)

(defmvar $plot_options 
  '((mlist) ((mlist) $plot_format $gnuplot_pipes)))

;; $plot_realpart option is false by default but *plot-realpart* is true
;; because coerce-float-fun is used outside of plot package too.
(defvar *plot-realpart* t)

(defun maybe-realpart (x)
  (if *plot-realpart*
      ($realpart x)
      (destructuring-bind (re . im) (trisplit x)
        (if (zerop1 im) re))))

(defvar *missing-data-indicator* "NaN")

;; PLOT OPTIONS PARSING
;;
;; allow this to be set in a system init file (sys-init.lsp)
;; (villate 20240322: I don't understand that comment)

(defmfun $get_plot_option (&optional name n)
  (let (options)
    ;; Converts the options property list into a Maxima list
    (do* ((list (copy-tree *plot-options*) (cddr list))
	  (key (first list) (first list))
	  (value (second list) (second list)))
	 ((endp list))
         (if (consp value)
	     (push (cons '(mlist) (cons key value)) options)
	   (push (list '(mlist) key value) options)))
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

(defmfun $reset_plot_options ()
  "Sets the default plotting options."
  (setf
   *plot-options*
   '($grid (30 30) $color ($blue $red $green $magenta $black $cyan)
           $gnuplot_term $default $mesh_lines_color "dimgray"
           $palette $default $gnuplot_preamble "" $background_color "white"
           $point_type ($bullet mbox $triangle $plus $times $asterisk)
           $adapt_depth 5 $nticks 29 $axes t $run_viewer t 
           $plot_format $gnuplot_pipes))
  t)

($reset_plot_options)

;; Removes option "name" from current plotting options
(defmfun $remove_plot_option (name)
  (remf *plot-options* name))

(defun get-gnuplot-term (term)
  (let* ((sterm (string-downcase (ensure-string term)))
         (pos   (search " " sterm)))
    (if pos  
      (subseq sterm 0 pos)
      sterm)))
;; end of PLOT OPTIONS parsing functions

(defvar $pstream nil)

(defun print-pt1 (f str)
  (if (floatp f)
    (format str "~,,,,,,'eg " f)
    (format str "~a " *missing-data-indicator*)))

;; There is currently no need for this polygon structure (villate, 2026-03-31)
(defstruct (polygon (:type list)
                    (:constructor %make-polygon (pts edges)))
  (dummy '($polygon simp))
  pts edges)

(eval-when
    (:compile-toplevel :execute)

    (defmacro z-pt (ar i) `(aref ,ar (the fixnum (+ 2 (* ,i 3)))))
    (defmacro y-pt (ar i) `(aref ,ar (the fixnum (1+ (* ,i 3)))))
    (defmacro x-pt (ar i) `(aref ,ar (the fixnum (* ,i 3))))
    (defmacro rot (m i j) `(aref ,m (the fixnum (+ ,i (the fixnum (* 3 ,j))))))

    (defmacro print-pt (f)
      `(print-pt1 ,f $pstream ))

    (defmacro make-polygon (a b)
      `(list '($polygon) ,a ,b)))

(defun vertices-matrix (points nxint nyint)
"(vertices-matrix points nxint nyint). Given a vector 'points' and two integers
nxint and nyint, it returns a matrix in which each row has the indices of the
points for a given value of y in the mesh, or T if the value of z doesn't
exist at the corresponding mesh point.
'points' is a sequence x0 y0 y0 x1 y1 z1 ... with the coordinates of all the
points in the mesh; the indices in the returned matrix are the positions of
the corresponding points in the list of valid points (excluding those whose
z coordinate is not a number)." 
  (let (vertices row (r 0) (k 0))
    (loop for j to nyint
          do
          (setf row nil)
          (setf r (* 3 j (1+ nxint)))
          (loop for i to (* 3 nxint) by 3
                do
                (let (pt)
                  (dotimes (n 3) (push (elt points (+ r i n)) pt))
                  (if (= (length (remove t pt)) 3)
                    (progn
                      (push k row)
                      (incf k))
                    (push t row))))
          (push (reverse row) vertices))
    (reverse vertices)))

(defun polygons (matrix)
"(polygons matrix). Creates a list of polygins for rectangular mesh defined
by 'matrix'.
Each sublist in matrix has non-negative integers, or T, identifiying the
points in each row of the mesh; the non-negative integer is the position
of the (x, y, z) coordinates of the corresponding point in a vector with
all the points coordinates (ater removal of non-valid points); if the value
is T, it means that there is no valid point in that position of the mesh.
The list returned will have sublists of 3 or 4 elements that label the
vertices of a triangle or a quadrilateral."
  (let (pols pol (rows (length matrix)) (cols (length (first matrix))))
    (dotimes (j (1- rows))
      (dotimes (i (1- cols))
        (setf pol nil)
        (push (nth i (nth (1+ j) matrix)) pol)
        (push (nth (1+ i) (nth (1+ j) matrix)) pol)
        (push (nth (1+ i) (nth j matrix)) pol)
        (push (nth i (nth j matrix)) pol)
        (setf pol (remove t pol))
        (when (> (length pol) 2) (push pol pols))))
    (reverse pols)))

(defun draw3d (f minx maxx miny maxy nxint nyint)
  (let ( (x 0.0) (y 0.0) z (z-count 0) (epsx (/ (- maxx minx) nxint))
         (epsy (/ (- maxy miny) nyint))
         (points (make-array (* 3 (1+ nxint) (1+ nyint)) :fill-pointer 0
                             :element-type t)))
    (declare (type flonum x y epsy epsx) (fixnum z-count)
             (type (cl:array t) points))
    (dotimes (j (1+ nyint))
      (setq y (+ miny (* j epsy)))
      (dotimes (i (1+ nxint))
        (setq x (+ minx (* i epsx)))
        (setq z (funcall f x y))
        (vector-push x points)
        (vector-push y points)
        (vector-push z points)
        (if (floatp z) (setq z-count (1+ z-count)))))
    (if (< z-count 1) (merror (intl:gettext "plot3d: nothing to plot.~%")))
    (values points z-count)))

;; ***** This comment refers to some unexistent function make-vertices ****
;; ***** let's leave it here for the sake of history :)                ****
;; The following is 3x2 = 6 rectangles
;; call (make-vertices 3 2)
;; there are 4x3 = 12 points.
;; ordering is x0,y0,z0,x1,y1,z1,....,x11,y11,z11
;; ----
;; ||||
;; ----
;; ||||
;; ----

;; make-grid-vertices
;; Creates an array that describes a rectangular grid with nx intervals
;; in the x direction and ny intervals in the y direction.
;; Such grid is then formed by nx*ny rectangles.
;; Each rectangle is defined by four points and a value of zero, which
;; will be later replaced by the z value corresponding to that rectangle.
;;
;; Thus, each sequence of 5 integers in the resulting array describe
;; a rectangle in the grid. For instance, 0 3 4 1 0 corresponds to the
;; the rectangle with vertices at the points with indices 0 3 4 and 1.
;; The points on the top row of the grid have indices:
;;    0 1 2 ... nx
;; and the points in the j'th row from the top (counting from 0) are:
;;    j j+1 j+2 ... j+nx
;; The order of the points given for each rectangle gives the correct
;; sequence when the boundary of the rectangle is traversed in
;; the clockwise direction.

(defun make-grid-vertices (nx ny)
  (declare (fixnum nx ny))
  (let* ((tem (make-array (+ 15 (* 5 nx ny)) :fill-pointer (* 5 nx ny)
                          :adjustable t
                          :element-type '(mod  #x80000000)))
         (m  nx )
         (nxpt (+ nx 1))
         (i 0)
         )
    (declare (fixnum i nxpt m)
             (type (cl:array (mod #x80000000)) tem))
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

;; $rotation1
;; The argument phi is elevation and th is azimuth. This function returns the
;; matrix of a rotation in the positive direction (from x to y) along
;; the z axis, with an angle equal to the azimuth, followed by a negative
;; rotation (from x to z) along the new y axis, with an angle equal to
;; the elevation. Even though it is declared as a Maxima function, it can
;; not be called with symbolic arguments; only numeric arguments.

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

(defmfun $get_range (pts k &aux (z 0.0) (max +most-negative-flonum+) (min +most-positive-flonum+))
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
         do (setq r (elt pts i))
         (setq th (elt pts (1+ i)))
         (setf (elt pts i) (* r (cos th)))
         (setf (elt pts (1+ i)) (* r (sin th)))))

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
        do
        (if (floatp (elt pts (+ i 2)))
          (progn
            (setq th (elt pts i))
            (setq ph (elt pts (+ i 1)))
            (setq r (elt pts (+ i 2)))
            (setf (elt pts i) (* r (sin th) (cos ph)))
            (setf (elt pts (+ i 1)) (* r (sin th) (sin ph)))
            (setf (elt pts (+ i 2)) (* r (cos th))))
          (progn
            (setf (elt pts i) t)
            (setf (elt pts (+ i 1)) t)))))

;; return a function suitable for the transform function in plot3d.
;; FX, FY, and FZ are functions of three arguments.
;; This function needs to be fixed for the case when coerce-float-fun
;; returns T instead of a floating-point value.
(defmfun $make_transform (lvars fx fy fz)
  (setq fx (coerce-float-fun fx lvars "make_transform"))
  (setq fy (coerce-float-fun fy lvars "make_transform"))
  (setq fz (coerce-float-fun fz lvars "make_transform"))
  (let ((sym (gensym "transform")))
    (setf (symbol-function sym)
          #'(lambda (pts &aux  (x1 0.0)(x2 0.0)(x3 0.0))
              (declare (type flonum  x1 x2 x3))
              (declare (type (cl:array t) pts))
              (loop for i below (length pts) by 3
                     do 
                     (setq x1 (elt pts i))
                     (setq x2 (elt pts (+ i 1)))
                     (setq x3 (elt pts (+ i 2)))
                     (setf (elt pts i) (funcall fx x1 x2 x3))
                     (setf (elt pts (+ 1 i)) (funcall fy x1 x2 x3))
                     (setf (elt pts (+ 2 i)) (funcall fz x1 x2 x3)))))))

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
(defun %coerce-float-fun (float-fun expr &rest rest &aux lvars fname)
  (case (length rest)
    (0 (setq lvars nil) (setq fname "coerce-float-fun"))
    (1 (setq lvars (first rest)) (setq fname "coerce-float-fun"))
    (2 (setq lvars (first rest)) (setq fname (second rest)))
    (t (merror (intl:gettext "coerce-float-fun: two many arguments given."))))
  (cond ((and (consp expr) (functionp expr))
         (let ((args (if lvars (cdr lvars) (list (gensym)))))
           (coerce-lisp-function-or-lisp-lambda args expr :float-fun float-fun)))
        ;; expr is a string which names an operator
        ;; (e.g. "!" "+" or a user-defined operator)
        ((and (stringp expr) (getopr0 expr))
         (let ((a (if lvars lvars `((mlist) ,(gensym)))))
           (%coerce-float-fun float-fun `(($apply) ,(getopr0 expr) ,a) a fname)))
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
              (coerce-maxima-function-or-maxima-lambda
               args expr :float-fun float-fun)))
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
              (%coerce-float-fun float-fun `(($apply) ,expr ,a) a fname)))
           (t
            (merror (intl:gettext "~a: unknown function: ~M")
                    fname expr))))
	((and (consp expr) (eq (caar expr) 'lambda))
	 (let ((args (cdr (second expr))))
	   (coerce-maxima-function-or-maxima-lambda
            args expr :float-fun float-fun)))
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
	       (declare (special ,@(cdr vars)))

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
		     (*nounsflag* t))
		 ;; Catch any errors from evaluating the
		 ;; function.  We're assuming that if an error
		 ;; is caught, the result is not a number.  We
		 ;; also assume that for such errors, it's
		 ;; because the function is not defined there,
		 ;; not because of some other maxima error.
		 (let ((result
			 (errcatch (,float-fun (maybe-realpart (meval* ',expr))))))

		   ;; Nothing interpolated here when there are no
		   ;; subscripted variables.
		   ,@(if (cdr subscripted-vars) `((progn ,@subscripted-vars-restore)))

		   (if result
		       (car result)
		       t))))
	    'function)))))
;; coerce-float-fun must be given an expression and one or two other optional
;; arguments: a Maxima list of variables on which that expression depends
;; and string that will identify the name of the responsible function
;; when reporting errors.
(defun coerce-float-fun (expr &rest rest &aux lvars fname)
  (case (length rest)
    (0 (setq lvars nil) (setq fname "coerce-float-fun"))
    (1
     (if (stringp (first rest))
         (progn (setq lvars nil) (setq fname (first rest)))
         (if ($listp (first rest))
             (progn (setq lvars (first rest)) (setq fname "coerce-float-fun"))
             (merror
              (intl:gettext "coerce-float-fun: expecting a Maxima list, found: ~M")
              (first rest)))))
    (2
     (if ($listp (first rest))
         (setq lvars (first rest))
         (merror
          (intl:gettext "coerce-float-fun: expecting a Maxima list, found: ~M")
          (first rest)))
     (if (stringp (second rest))
         (setq fname (second rest))
         (merror
          (intl:gettext "coerce-float-fun: expecting a string, found: ~M")
          (second rest))))
    (t (merror (intl:gettext "coerce-float-fun: two many arguments given."))))
  (%coerce-float-fun '$float expr lvars fname))

;; coerce-bfloat-fun must be given an expression and one or two other optional
;; arguments: a Maxima list of variables on which that expression depends
;; and string that will identify the name of the responsible function
;; when reporting errors.
(defun coerce-bfloat-fun (expr &rest rest &aux lvars fname)
  (case (length rest)
    (0 (setq lvars nil) (setq fname "coerce-bfloat-fun"))
    (1
     (if (stringp (first rest))
         (progn (setq lvars nil) (setq fname (first rest)))
         (if ($listp (first rest))
             (progn (setq lvars (first rest)) (setq fname "coerce-float-fun"))
             (merror
              (intl:gettext "coerce-bfloat-fun: expecting a Maxima list, found: ~M")
              (first rest)))))
    (2
     (if ($listp (first rest))
         (setq lvars (first rest))
         (merror
          (intl:gettext "coerce-bfloat-fun: expecting a Maxima list, found: ~M")
          (first rest)))
     (if (stringp (second rest))
         (setq fname (second rest))
         (merror
          (intl:gettext "coerce-bfloat-fun: expecting a string, found: ~M")
          (second rest))))
    (t (merror (intl:gettext "coerce-bfloat-fun: two many arguments given."))))
  (%coerce-float-fun '$bfloat expr lvars fname))

(defun coerce-maxima-function-or-maxima-lambda
    (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for nil in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         ;; Just always try to convert the result with
         ;; float-fun, which handles things like $%pi.
         ;; See also BUG
         ;; https://sourceforge.net/p/maxima/bugs/1795/
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
                (result
                  (errcatch
                    (,float-fun (maybe-realpart (mapply ',expr (list ,@gensym-args) t))))))
           (if result
               (car result)
               t)))
      'function)))

;; Same as above, but call APPLY instead of MAPPLY.

(defun coerce-lisp-function-or-lisp-lambda
    (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for nil in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
                (result (maybe-realpart (apply ',expr (list ,@gensym-args)))))
           ;; Always use float-fun.  See comment for
           ;; coerce-maxima-function-or-maxima-lambda above.
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
           (type (cl:array (mod #x80000000)) edges)
           (fixnum n))
  (let ((new (make-array (length edges) :element-type  (array-element-type edges)))
        (i 0)
        (z 0.0)
        (z1 0.0)
        (n1 (- n 1))
        (at 0)
        (leng (length edges))
        )
    (declare (type (cl:array (mod #x80000000)) new)
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

;; Implicit expressions of two variables, for instance, x and y,
;; where expr is of the form f(x,y) = g(x,y).
;; The result is a series of separated line segments.
;;
(defun draw2d-implicit (expr options)
  (let* ((xmin (first (getf options '$x)))
         (ymin (first (getf options '$y)))
         (xmax (second (getf options '$x)))
         (ymax (second (getf options '$y)))
         (gridx (or (first (getf options '$sample)) 47))
         (gridy (or (second (getf options '$sample)) 47))
         (eps (or (getf options '$plotepsilon) 1e-6))
         (f (make-array `(,(1+ gridx) ,(1+ gridy))))
         vx vy dx dy fun faux fmax fmin levels values result results)
    (setq dx (/ (- xmax xmin) gridx) dy (/ (- ymax ymin) gridy))
    (setq vx (getf options '$xvar) vy (getf options '$yvar))
    (if (getf options '$contour)
        (setq fun expr)
        (setq fun (m- ($lhs expr) ($rhs expr))))
    (setq fun (coerce-float-fun fun `((mlist) ,vx ,vy) "plot2d"))
    ;; sets up array f with values of the function at corners of sample grid.
    ;; finds maximum and minimum values in that array. 
    (dotimes (i (1+ gridx))
      (dotimes (j (1+ gridy))
        (setq faux (funcall fun (+ xmin (* i dx)) (+ ymin (* j dy))))
        (setf (aref f i j) faux)
        (when (and (numberp faux) (plusp i) (plusp j) (< i gridx) (< j gridy))
          (if (numberp fmin)
              (if (numberp fmax)
                  (progn
                    (when (< faux fmin) (setq fmin faux))
                    (when (> faux fmax) (setq fmax faux)))
                  (if (< faux fmin)
                      (setq fmax fmin fmin faux)
                      (setq fmax faux)))
              (if (numberp fmax)
                  (if (> faux fmax)
                      (setq fmin fmax fmax faux)
                      (setq fmin faux))
                  (setq fmin faux))))))
    ;; checks that the function has a minimum and a maximum
    (when
        (or
         (not (numberp fmin))
         (not (numberp fmax)) (not (> fmax fmin)))
      (merror (intl:gettext "plot2d: nothing to plot for ~M.~%") expr))
    ;; sets up the levels for contour plots
    (if (getf options '$contour)
        (if (setq levels (getf options '$levels))
            (unless (listp levels)
              (setq levels (getlevels fmin fmax levels)))
            (setq levels (getlevels fmin fmax 8)))
        (setq levels (list 0.0)))
    ;;
    ;; Algorithm for implicit functions, by Jaime Villate. 2021
    ;;
    ;; The points at each rectangle in the sample grid are labeled as follows:
    ;;
    ;; ij+ ______ i+j+
    ;;     |    |
    ;;     |    |    function fun has the following values at those points:
    ;;     |    |
    ;;  ij |____| i+j     fij, fi+j, fij+, fi+j+
    ;;
    (let (fij fi+j fij+ fi+j+ p1 p2 p3 p4 next)
      (flet
          ((interp+ (i j fi fi+ &aux x1 y1 x2 y2 (f1 fi) (f2 fi+) xp yp fp)
             (if (minusp (* fi fi+))
                 (progn
                   (setq x1 (+ xmin (* dx i)))
                   (setq x2 (+ x1 dx))
                   (setq y1 (+ ymin (* dy j)))
                   (setq y2 (+ y1 dy))
                   (dotimes (n 2
                             (if (< (/ (+ (abs (- fi fp)) (abs (- fi+ fp)))
                                       (abs (- fi fi+))) 1.5) (list xp yp) nil))
                     (setq xp (/ (+ x1 x2) 2.0))
                     (setq yp (/ (+ y1 y2) 2.0))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp y1 yp f1 fp)
                         (setq x2 xp y2 yp f2 fp))
                     (setq xp (/ (- (* f1 x2) (* f2 x1)) (- f1 f2)))
                     (setq yp (/ (- (* f1 y2) (* f2 y1)) (- f1 f2)))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp y1 yp f1 fp)
                         (setq x2 xp y2 yp f2 fp))))
                 nil))
           (interp- (i j fi fi+ &aux x1 y1 x2 y2 (f1 fi) (f2 fi+) xp yp fp)
             (if (minusp (* fi fi+))
                 (progn
                   (setq x1 (+ xmin (* dx i)))
                   (setq x2 (+ x1 dx))
                   (setq y1 (+ ymin (* dy j)))
                   (setq y2 (- y1 dy))
                   (dotimes (n 2
                             (if (< (/ (+ (abs (- fi fp)) (abs (- fi+ fp)))
                                       (abs (- fi fi+))) 1.5) (list xp yp) nil))
                     (setq xp (/ (+ x1 x2) 2.0))
                     (setq yp (/ (+ y1 y2) 2.0))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp y1 yp f1 fp)
                         (setq x2 xp y2 yp f2 fp))
                     (setq xp (/ (- (* f1 x2) (* f2 x1)) (- f1 f2)))
                     (setq yp (/ (- (* f1 y2) (* f2 y1)) (- f1 f2)))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp y1 yp f1 fp)
                         (setq x2 xp y2 yp f2 fp))))
                 nil))
           (interpx (i j fi fi+ &aux x1 x2 (f1 fi) (f2 fi+) xp yp fp)
             (if (minusp (* fi fi+))
                 (progn
                   (setq x1 (+ xmin (* dx i)))
                   (setq x2 (+ x1 dx))
                   (setq yp (+ ymin (* dy j)))
                   (dotimes (n 2
                             (if (< (/ (+ (abs (- fi fp)) (abs (- fi+ fp)))
                                       (abs (- fi fi+))) 1.5) (list xp yp) nil))
                     (setq xp (/ (+ x1 x2) 2.0))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp f1 fp)
                         (setq x2 xp f2 fp))
                     (setq xp (/ (- (* f1 x2) (* f2 x1)) (- f1 f2)))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq x1 xp f1 fp)
                         (setq x2 xp f2 fp))))
                 nil))
           (interpy (i j fj fj+ &aux y1 y2 (f1 fj) (f2 fj+) xp yp fp)
             (if (minusp (* fj fj+))
                 (progn
                   (setq xp (+ xmin (* dx i)))
                   (setq y1 (+ ymin (* dy j)))
                   (setq y2 (+ y1 dy))
                   (dotimes (n 2
                             (if (< (/ (+ (abs (- fj fp)) (abs (- fj+ fp)))
                                       (abs (- fj fj+))) 1.5) (list xp yp) nil))
                     (setq yp (/ (+ y1 y2) 2.0))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq y1 yp f1 fp)
                         (setq y2 yp f2 fp))
                     (setq yp (/ (- (* f1 y2) (* f2 y1)) (- f1 f2)))
                     (setq fp (- (funcall fun xp yp) level))
                     (when (not (numberp fp)) (return nil))
                     (if (plusp (* f1 fp))
                         (setq y1 yp f1 fp)
                         (setq y2 yp f2 fp))))
                 nil))
           (coords (i j)
             (list (+ xmin (* i dx)) (+ ymin (* j dy))))
           (draw-line (p1 p2)
             (push (first p1) result)
             (push (second p1) result)
             (push (first p2) result)
             (push (second p2) result)
             (push 'moveto result)
             (push 'moveto result))
           (draw-lines (p1 p2 p3)
             (push (first p1) result)
             (push (second p1) result)
             (push (first p2) result)
             (push (second p2) result)
             (push (first p3) result)
             (push (second p3) result)
             (push 'moveto result)
             (push 'moveto result)))
        (dolist (level (reverse levels))
          (dotimes (i gridx)
            (dotimes (j gridy)
              (if (numberp (aref f i j))
                  (setq fij (- (aref f i j) level))
                  (setq fij (aref f i j)))
              (if (numberp (aref f i (1+ j)))
                  (setq fij+ (- (aref f i (1+ j)) level))
                  (setq fij+ (aref f i (1+ j))))
              (if (numberp (aref f (1+ i) j))
                  (setq fi+j (- (aref f (1+ i) j) level))
                  (setq fi+j (aref f (1+ i) j)))
              (if (numberp (aref f (1+ i) (1+ j)))
                  (setq fi+j+ (- (aref f (1+ i) (1+ j)) level))
                  (setq fi+j+ (aref f (1+ i) (1+ j))))
              (setq next t)
              ;; 1. undefined at ij
              (when (not (numberp fij))
                (setq next nil)
                ;; if undefined also at i+j or ij+, continue to next rectangle
                (when (and (numberp fi+j) (numberp fij+))
                  (if (< (abs fi+j) eps)
                      (if (< (abs fij+) eps)
                          ;; real and 0 at i+j and ij+
                          (draw-line (coords (1+ i) j) (coords i (1+ j)))
                          (when
                              (and
                               (numberp fi+j+) 
                               (setq p2 (interpx i (1+ j) fij+ fi+j+)))
                            ;; real at i+j, ij+ and i+j+, 0 at i+j and segment
                            ;; ij+ i+j+
                            (draw-line (coords (1+ i) j) p2)))
                      (when (numberp fi+j+)
                        (if (< (abs fij+) eps)
                            (when (setq p2 (interpy (1+ i) j fi+j fi+j+))
                              ;; real at i+j, and i+j+, 0 at ij+ and segment
                              ;; i+j i+j+
                              (draw-line (coords i (1+ j)) p2))
                            (when
                                (and
                                 (setq p1 (interpx i (1+ j) fij+ fi+j+))
                                 (setq p2 (interpy (1+ i) j fi+j fi+j+)))
                              ;; real at i+j, ij+ and i+j+, 0 at segments
                              ;; ij+ i+j+ and i+j i+j+
                              (draw-line p1 p2)))))))
              ;; 2. real at ij and undefined at i+j
              (when (and next (not (numberp fi+j)))
                (setq next nil)
                ;; if undefined at ij+, continue to next rectangle
                (when (numberp fij+)
                  (if (< (abs fij) eps)
                      (if (< (abs fij+) eps)
                          ;; zero at ij and ij+
                          (draw-line (coords i j) (coords i (1+ j)))
                          (when
                              (and
                               (numberp fi+j+)
                               (setq p2 (interpx i (1+ j) fij+ fi+j+)))
                            ;; real at ij+ and i+j+, 0 at ij and segment ij+ i+j+
                            (draw-line (coords i j) p2)))
                      (when
                          (and
                           (numberp fi+j+)
                           (setq p1 (interpy i j fij fij+))
                           (setq p2 (interpx i (1+ j) fij+ fi+j+)))
                        ;; real at ij, ij+ and i+j+, 0 at segments ij ij+
                        ;; and ij+ i+j+
                        (draw-line p1 p2)))))
              ;; 3. real at fi+j and 0 at ij
              (when (and next (< (abs fij) eps))
                (setq next nil)
                (if (numberp fij+)
                    (if (< (abs fij+) eps)
                        ;; real at i+j, 0 at ij and ij+
                        (draw-line (coords i j) (coords i (1+ j)))
                        (when (setq p1 (interp- i (1+ j) fij+ fi+j))
                          (if (numberp fi+j+)
                              (if (< (abs fi+j+) eps)
                                  ;; real at i+j and ij, 0 at ij, i+j+ and
                                  ;; diagonal ij+ i+j
                                  (draw-lines (coords i j) p1
                                              (coords (1+ i) (1+ j)))
                                  (progn
                                    ;; real at i+j, ij+ and i+j+, 0 at ij,
                                    ;; diagonal ij+ i+j and segment ij+ i+j+
                                    (when (setq p2 (interpx i (1+ j) fij+ fi+j+))
                                      (draw-lines (coords i j) p1 p2))
                                    ;; real at i+j, ij+ and i+j+, 0 at ij,
                                    ;; diagonal ij+ i+j and segment i+j i+j+
                                    (when (setq p2 (interpy (1+ i) j fi+j fi+j+))
                                      (draw-lines (coords i j) p1 p2)))))))
                    (if (numberp fi+j+)
                        (if (< (abs fi+j) eps)
                            ;; undefined at ij+, real at fi+j+, 0 at ij and i+j
                            (draw-line (coords i j) (coords (1+ i) j))
                            (when (setq p2 (interpy (1+ i) j fi+j fi+j+))
                              ;; undefined at ij+, real at fi+j and fi+j+, 0 at
                              ;; ij and segment i+j i+j+
                              (draw-line (coords i j) p2)))
                        (when (< (abs fi+j) eps)
                          ;; undefined at ij+ and i+j+, 0 at ij and i+j
                          (draw-line (coords i j) (coords (1+ i) j))))))
              ;; 4. real at ij and 0 at i+j
              (when (and next (< (abs fi+j) eps))
                (setq next nil)
                (if (numberp fij+)
                    (if (numberp fi+j+)
                        ;; if 0 at i+j but undefined at ij+ or there's no zero
                        ;; in diagonal ij i+j+, continue to next rectangle
                        (when (setq p1 (interp+ i j fij fi+j+))
                          (if (< (abs fij+) eps)
                              ;; 0 at i+j, ij+ and diagonal ij i+j+
                              (draw-lines (coords (1+ i) j) p1 (coords i (1+ j)))
                              (progn
                                (when (setq p2 (interpy i j fij fij+))
                                  ;; 0 at i+j, diagonal ij i+j+ and segment
                                  ;; ij ij+ 
                                  (draw-lines (coords (1+ i) j) p1 p2))
                                (when (setq p2 (interpx i (1+ j) fij+ fi+j+))
                                  ;; 0 at i+j, diagonal ij i+j+ and segment
                                  ;; ij+ i+j+
                                  (draw-lines (coords (1+ i) j) p1 p2)))))
                        (when (setq p2 (interpy i j fij fij+))
                          ;; undefined at i+j+, 0 at i+j and segment ij ij+
                          (draw-line (coords (1+ i) j) p2)))))
              ;; 5. real at ij and i+j but undefined at ij+
              (when (and next (not (numberp fij+)))
                (setq next nil)
                (when
                    (and
                     (numberp fi+j+)
                     (setq p1 (interpx i j fij fi+j))
                     (setq p2 (interpy (1+ i) j fi+j fi+j+)))
                  ;; 0 at segments ij i+j and i+j i+j+
                  (draw-line p1 p2)))
              ;; 6. real at ij, i+j and ij+, but undefined at i+j+
              (when (and next (not (numberp fi+j+)))
                (setq next nil)
                (when
                    (and
                     (setq p1 (interpy i j fij fij+))
                     (setq p2 (interpx i j fij fi+j)))
                  ;; 0 at segments ij ij+ and ij i+j
                  (draw-line p1 p2)))
              ;; 7. real at the four corners and 0 at ij+
              (when (and next (< (abs fij+) eps))
                (setq next nil)
                (when (setq p1 (interp+ i j fij fi+j+))
                  (when (setq p2 (interpx i j fij fi+j))
                    ;; 0 at diagonal ij i+j+ and segment ij i+j
                    (draw-lines p2 p1 (coords i (1+ j))))
                  (when (setq p2 (interpy (1+ i) j fi+j fi+j+))
                    ;; 0 at diagonal ij i+j+ and segment i+j i+j+
                    (draw-lines p2 p1 (coords i (1+ j))))))
              ;; 8. real at the four corners and 0 at i+j+
              (when (and next (< (abs fi+j+) eps))
                (setq next nil)
                (when (setq p1 (interp- i (1+ j) fij+ fi+j))
                  (when (setq p2 (interpx i j fij fi+j))
                    ;; 0 at diagonal ij+ i+j and segment ij i+j
                    (draw-lines p2 p1 (coords (1+ i) (1+ j))))
                  (when (setq p2 (interpy i j fij fij+))
                    ;; 0 at diagonal ij+ i+j and segment ij ij+
                    (draw-lines p2 p1 (coords (1+ i) (1+ j))))))
              ;; 9. real at the four corners and 0 at segment ij i+j
              (when (and next (setq p1 (interpx i j fij fi+j)))
                (setq next nil)
                (if (setq p2 (interpy i j fij fij+))
                    (if (setq p3 (interpx i (1+ j) fij+ fi+j+))
                        (when (setq p4 (interpy (1+ i) j fi+j fi+j+))
                          ;; 0 at the four sides
                          (draw-line p1 p3)
                          (draw-line p2 p4))
                        (when (setq p3 (interp+ i j fij fi+j+))
                          ;; 0 at segments ij i+j, ij ij+ and diagonal ij i+j+
                          (draw-lines p1 p3 p2)))
                    (if (setq p4 (interpy (1+ i) j fi+j fi+j+))
                        (when (setq p2 (interp- i (1+ j) fij+ fi+j))
                          ;; 0 at segments ij i+j, i+j i+j+ and diagonal ij+ i+j
                          (draw-lines p1 p2 p4))
                        (when
                            (and
                             (setq p3 (interpx i (1+ j) fij+ fi+j+))
                             (setq p2 (interp+ i j fij fi+j+)))
                          ;; 0 at segments ij i+j, ij+ i+j+ and diagonal ij i+j+
                          (draw-lines p1 p2 p3)))))
              ;; 10. real at the four corners, without zero in segment ij i+j
              (when next
                (if (setq p2 (interpy i j fij fij+))
                    (if (setq p3 (interpx i (1+ j) fij+ fi+j+))
                        (when (setq p4 (interp- i (1+ j) fij+ fi+j))
                          ;; 0 at segments ij ij+ and ij+ i+j+ and diagonal
                          ;; ij+ i+j
                          (draw-lines p2 p4 p3))
                        (when
                            (and
                             (setq p4 (interpy (1+ i) j fi+j fi+j+))
                             (setq p3 (interp+ i j fij fi+j+)))
                          ;; 0 at segments ij ij+ and i+j i+j+ and diagonal
                          ;; ij i+j+
                          (draw-lines p2 p3 p4)))
                    (when
                        (and
                         (setq p3 (interpx i (1+ j) fij+ fi+j+))
                         (setq p4 (interpy (1+ i) j fi+j fi+j+))
                         (setq p1 (interp+ i j fij fi+j+)))
                      ;; 0 at segments ij+ i+j+ and i+j i+j+ and diagonal
                      ;; ij i+j+
                      (draw-lines p4 p1 p3))))))
          (when (and (getf options '$contour) result)
            (push (cons '(mlist) (reverse result)) results)
            (push level values)
            (setq result nil)))))
    ;; When called for a single implicit expression, returns a Maxima list
    ;; of points. When called for contours of an expression, returns a
    ;; Maxima list whose first element is another Maxima list with the values
    ;; of the contours, followed by Maxima lists of points for each contour.
    (if (getf options '$contour)
        (cons '(mlist) (cons (cons '(mlist) values) results))
        (cons '(mlist) (reverse result)))))

;; parametric ; [parametric,xfun,yfun,[t,tlow,thigh],[nticks ..]]
;; the rest of the parametric list after the list will add to the plot options

(defun draw2d-parametric-adaptive (param options &aux range)
  (or (= ($length param) 4)
      (merror (intl:gettext "plot2d: parametric plots must include two expressions and an interval")))
  (setq range (nth 4 param))
  (or (and ($listp range) (symbolp (second range)) (eql ($length range) 3))
      (merror (intl:gettext "plot2d: wrong interval for parametric plot: ~M")
              range))
  (setq range (check-range range))
  (let* ((nticks (getf options '$nticks))
         (trange (cddr range))
         (tvar (second range))
         (xrange (or (getf options '$x) (getf options '$xbounds)))
         (yrange (or (getf options '$y) (getf options '$ybounds)))
         (tmin (coerce-float (first trange)))
         (tmax (coerce-float (second trange)))
         (xmin (coerce-float (first xrange)))
         (xmax (coerce-float (second xrange)))
         (ymin (coerce-float (first yrange)))
         (ymax (coerce-float (second yrange)))
         f1 f2)
    (declare (type flonum ymin ymax xmin xmax tmin tmax))
    (setq f1 (coerce-float-fun (third param) `((mlist), tvar) "plot2d"))
    (setq f2 (coerce-float-fun (fourth param) `((mlist), tvar) "plot2d"))

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
				 (getf options '$adapt_depth)
				 1e-5)))
		  (adaptive-parametric-plot
		   f1 f2
		   (car t-start) (car t-mid) (car t-end)
		   (car x-start) (car x-mid) (car x-end)
		   (car y-start) (car y-mid) (car y-end)
		   (getf options '$adapt_depth)
		   1e-5))))
      ;; Fix up out-of-range values and clobber non-numeric values.
      (do ((x result (cddr x))
	   (y (cdr result) (cddr y)))
	  ((null y))
	(if (and (numberp (car x)) (numberp (car y)))
	    ;; Let clipping to the graphic program. See the comment in DRAW2D.
            (unless (and (<= ymin (car y) ymax)
			 (<= xmin (car x) xmax))
	      (incf n-clipped))
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
               (mtell (intl:gettext "plot2d: all values will be clipped.~%")))
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
		  (mtell (intl:gettext "plot2d: some values will be clipped.~%")))))
	(cons '(mlist) result-sans-nil)))))

;; draw2d-discrete. Accepts [discrete,[x1,x2,...],[y1,y2,...]]
;; or [discrete,[[x1,y1]...] and returns [x1,y1,...] or nil, if none of the
;; points have real values.
;; Currently there are no plot options relevant to draw2d-discrete.
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
     (when (= gaps 2)
       ;; one point has non-real value
       (mtell (intl:gettext "Warning: excluding 1 point with non-numerical value.~%")))
     (when (> gaps 2)
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
                 (delta (* 150 +flonum-epsilon+)))
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
  (when (and ($listp fcn) (equal '$contour (cadr fcn)))
    (setf (getf plot-options '$contour) t)
    (return-from draw2d (draw2d-implicit (caddr fcn) plot-options)))
  (when (and (listp fcn) (eq 'mequal (caar fcn)))
    (setf (getf plot-options '$contour) nil)
    (return-from draw2d (draw2d-implicit fcn plot-options)))
  (let* ((nticks (getf plot-options '$nticks))
         (yrange (getf plot-options '$ybounds))
         (depth (getf plot-options '$adapt_depth)))

    (setq fcn (coerce-float-fun fcn `((mlist), (second range)) "plot2d"))

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
      (when (getf plot-options '$logx)
        (setf x-start (log x-start))
        (setf xend (log xend))
        (setf x-step (/ (- xend x-start) (coerce-float nticks) 2)))

      (flet ((fun (x)
               (let ((y (if (getf plot-options '$logx)
                            (funcall fcn (exp x))
                            (funcall fcn x))))
                 (if (and (getf plot-options '$logy)
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
	      ;; We can let graphic program do the clipping for us.
              ;; This results in better looking plots. For example:
              ;;     plot2d(x-floor(x),[x,0,5], [y,0,.5])
              ;; has lines going all the way to
	      ;; the limits.  Previously, the lines would stop
	      ;; before the limit.
	      (unless (<= ymin (car y) ymax)
		(incf n-clipped))
            (progn
              (incf n-non-numeric)
              (setf (car x) 'moveto)
              (setf (car y) 'moveto)))
          (when (and (getf plot-options '$logx)
                     (numberp (car x)))
            (setf (car x) (exp (car x))))

          (when (and (getf plot-options '$logy)
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
               (mtell (intl:gettext "plot2d: all values will be clipped.~%")))
              ((= n-clipped 0)
               (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value everywhere in plotting range.~%")))
              (t
                (mtell (intl:gettext "plot2d: all values are non-numeric, or clipped.~%"))))
            (progn
              (if (> n-non-numeric 0)
                (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value somewhere in plotting range.~%")))
              (if (> n-clipped 0)
                (mtell (intl:gettext "plot2d: some values will be clipped.~%")))))
          (cons '(mlist) result-sans-nil))))))

(defun get-range (lis)
  (let ((ymin +most-positive-flonum+)
        (ymax +most-negative-flonum+))
    (declare (type flonum ymin ymax))
    (do ((l lis (cddr l)))
        ((null l))
      (or (floatp (car l)) (setf (car l) (float (car l))))
      (cond ((< (car l) ymin)
             (setq ymin (car l))))
      (cond ((< ymax (car l))
             (setq ymax (car l)))))
    (list '(mlist) ymin ymax)))

(defmvar $gnuplot_view_args
    #+(or sbcl openmcl gcl) "-persist ~a"
    #-(or sbcl openmcl gcl) "-persist ~s"
    "String of additional command-line options for gnuplot.  See the user
    manual."
    :setting-predicate #'string-predicate)

(defmvar $gnuplot_file_args
    #+(or sbcl openmcl gcl) "~a"
    #-(or sbcl openmcl gcl) "~s"
    "Format string for printing the file name for gnuplot to use.  See the
    user manual."
    :setting-predicate #'string-predicate)

;; random-file-name
;; Creates a random word of 'count' alphanumeric characters
(defun random-name (count)
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz") (name ""))
    (setf *random-state* (make-random-state t))
    (dotimes (i count)
      (setq name (format nil "~a~a" name (aref chars (random 36)))))
    name))

(defun plot-temp-file0 (file &optional (preserve-file nil))
  (let ((filename 
	 (if *maxima-tempdir* 
	     (format nil "~a/~a" *maxima-tempdir* file)
	   file)))
    (unless preserve-file
      (setf (gethash filename *temp-files-list*) t))
    (format nil "~a" filename)
    ))

(defun plot-temp-file (file &optional (preserve-file nil) (plot-options nil))
  (let (script-name
        (script-name-or-fun
         (and plot-options (getf plot-options '$gnuplot_script_file))))
    (if (null script-name-or-fun)
        (plot-temp-file0 file preserve-file)
        (progn
          (setq
           script-name
           (cond 
	     ((symbolp script-name-or-fun) (mfuncall script-name-or-fun file))
	     (t script-name-or-fun)))
          (if (pathname-directory script-name)
              script-name
              (plot-temp-file0 script-name preserve-file))))))
              
;; If no file path is given, uses temporary directory path
(defun plot-file-path (file &optional (preserve-file nil) (plot-options nil))
  (if (pathname-directory file)
      file
      (plot-temp-file file preserve-file plot-options)))

;; PLOT OPTIONS PARSING
;; plot-options-parser puts the plot options given into a property list.
;; maxopts: a list (not a Maxima list!) with plot options.
;; options: a property list, or an empty list.
;; Example:
;;  (plot-options-parser (list #$[x,-2,2]$ #$[nticks,30]$) '($xlabel "t"))
;; returns:
;;  ($NTICKS 30 $X (-2.0 2.0) $XLABEL "t")

(defun plot-options-parser (maxopts options &aux name)
  (dolist (opt maxopts)
    (unless (or ($listp opt) (symbolp opt))
      (merror
       (intl:gettext
        "plot-options-parser: option \"~M\" should be a list or a symbol")
       opt))
    (cond
      (($listp opt)
       (unless ($symbolp (setq name (second opt)))
         (merror
          (intl:gettext
           "plot-options-parser: Expecting option name as a symbol, found: \"~M\"")
          opt))
       (case name
         ($adapt_depth 
          (setf (getf options '$adapt_depth)
                (check-option (cdr opt) #'(lambda (n)
					    ;; N should be a non-negative integer
					    (and (integerp n)
						 (>= n 0)))
			      "a non-negative integer" 1)))
         ($axes (setf (getf options '$axes)
                      (check-option-b (cdr opt) #'axesoptionp "x, y, solid" 1)))
         ($azimuth (if (caddr opt)
                       (setf (caddr opt) (parse-azimuth (caddr opt))))
                   (setf (getf options '$azimuth)
                         (check-option (cdr opt) #'realp "a real number" 1)))
         ($background_color
          (setf (getf options '$background_color)
                (check-option-b (cdr opt) #'plotcolorp "a color" 1)))
         (mbox (setf (getf options 'mbox)
                     (check-option-boole (cdr opt))))
         ($color (setf (getf options '$color)
                       (check-option (cdr opt) #'plotcolorp "a color")))
         ($color_bar (setf (getf options '$color_bar)
                           (check-option-boole (cdr opt))))
         ($color_bar_tics
          (if (cddr opt)
              (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
          (setf (getf options '$color_bar_tics)
                (check-option-b (cdr opt) #'realp "a real number" 3)))
         ($elevation (if (caddr opt)
                         (setf (caddr opt) (parse-elevation (caddr opt))))
                     (setf (getf options '$elevation)
                           (check-option (cdr opt) #'realp "a real number" 1)))
         ($grid (setf (getf options '$grid)
                      (check-option (cdr opt) #'naturalp "a natural number" 2)))
         ($grid2d (setf (getf options '$grid2d)
                        (check-option-boole (cdr opt))))
         ($iterations
          (setf (getf options '$iterations)
                (check-option (cdr opt) #'naturalp "a natural number" 1)))
         ($label (setf (getf options '$label)
                       (check-option-label (cdr opt))))
         ($legend (setf (getf options '$legend)
                        (let ((x (check-option-b (cdr opt) #'stringp "a string")))
			  (if (listp x) x (cons x nil)))))
         ($levels (setf (getf options '$levels)
                        (check-option-levels (cdr opt))))
         ($lighting (setf (getf options '$lighting)
                      (check-option-boole (cdr opt))))
         ($logx (setf (getf options '$logx)
                      (check-option-boole (cdr opt))))
         ($logy (setf (getf options '$logy)
                      (check-option-boole (cdr opt))))
         ($mesh_lines_color
          (setf (getf options '$mesh_lines_color)
                (check-option-b (cdr opt) #'plotcolorp "a color" 1)))
         ($nticks (setf (getf options '$nticks)
                        (check-option (cdr opt) #'naturalp "a natural number" 1)))
         ($palette (setf (getf options '$palette)
                         (check-option-palette (cdr opt))))
         ($plotepsilon (setf (getf options '$plotepsilon)
                             (check-option (cdr opt) #'realp "a real number" 1)))
         ($plot_format (setf (getf options '$plot_format)
                             (check-option-format (cdr opt))))
         ($plot_realpart (setf (getf options '$plot_realpart)
                               (check-option-boole (cdr opt))))
         ($point_type (setf (getf options '$point_type)
                            (check-option (cdr opt) #'pointtypep "a point type")))
         ($pdf_file (setf (getf options '$pdf_file)
                          (check-option (cdr opt) #'stringp "a string" 1)))
         ($png_file (setf (getf options '$png_file)
                          (check-option (cdr opt) #'stringp "a string" 1)))
         ($ps_file (setf (getf options '$ps_file)
                         (check-option (cdr opt) #'stringp "a string" 1)))
         ($run_viewer (setf (getf options '$run_viewer)
                            (check-option-boole (cdr opt))))
         ($same_xy (setf (getf options '$same_xy)
                         (check-option-boole (cdr opt))))
         ($same_xyz (setf (getf options '$same_xyz)
                          (check-option-boole (cdr opt))))
         ($sample (setf (getf options '$sample)
                        (check-option (cdr opt) #'naturalp "a natural number" 2)))
         ($style (setf (getf options '$style)
                       (check-option-style (cdr opt))))
         ($svg_file (setf (getf options '$svg_file)
                          (check-option (cdr opt) #'stringp "a string" 1)))
         ($t (setf (getf options '$t) (cddr (check-range opt))))
         ($title (setf (getf options '$title)
                       (check-option (cdr opt) #'stringp "a string" 1)))
         ($transform_xy (setf (getf options '$transform_xy)
                              (check-option-b (cdr opt) #'functionp "a function make_transform" 1)))
         ($window (setf (getf options '$window)
                        (check-option (cdr opt)
                                      #'(lambda (n) (and (integerp n) (>= n 0)))
			              "a non-negative integer" 1)))
         ($x (setf (getf options '$x) (cddr (check-range opt))))
         ($xbounds (setf (getf options '$xbounds) (cddr (check-range opt))))
         ($xlabel (setf (getf options '$xlabel)
                        (check-option (cdr opt) #'string "a string" 1)))
         ($xtics
          (if (cddr opt)
              (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
          (setf (getf options '$xtics)
                (check-option-b (cdr opt) #'realp "a real number" 3)))
         ($xy_scale
          (if (cddr opt)
              (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
          (setf (getf options '$xy_scale)
                (check-option (cdr opt) #'realpositivep
                              "a positive real number" 2)))
         ($y (setf (getf options '$y) (cddr (check-range opt))))
         ($ybounds (setf (getf options '$ybounds) (cddr (check-range opt))))
         ($ylabel (setf (getf options '$ylabel)
                        (check-option (cdr opt) #'string "a string" 1)))
         ($ytics
          (if (cddr opt)
              (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
          (setf (getf options '$ytics)
                (check-option-b (cdr opt) #'realp "a real number" 3)))
         ($yx_ratio
          (if (caddr opt)
              (setf (caddr opt) (coerce-float (caddr opt))))
          (setf (getf options '$yx_ratio)
                (check-option (cdr opt) #'realp "a real number" 1)))
         ($z (setf (getf options '$z) (cddr (check-range opt))))
         ($zlabel (setf (getf options '$zlabel)
                        (check-option (cdr opt) #'string "a string" 1)))
         ($zmin
          (if (caddr opt)
              (setf (caddr opt) (coerce-float (caddr opt))))
          (setf (getf options '$zmin)
                (check-option-b (cdr opt) #'realp "a real number" 1)))
         ($ztics
          (if (cddr opt)
              (setf (cddr opt) (mapcar #'coerce-float (cddr opt))))
          (setf (getf options '$ztics)
                (check-option-b (cdr opt) #'realp "a real number" 3)))
         ($gnuplot_4_0 (setf (getf options '$gnuplot_4_0)
                             (check-option-boole (cdr opt))))
         ($gnuplot_curve_titles
          (setf (getf options '$gnuplot_curve_titles)
                (check-option (cdr opt) #'stringp "a string")))
         ($gnuplot_curve_styles
          (setf (getf options '$gnuplot_curve_styles)
                (check-option (cdr opt) #'stringp "a string")))
         ($gnuplot_default_term_command
          (setf (getf options '$gnuplot_default_term_command)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_dumb_term_command
          (setf (getf options '$gnuplot_dumb_term_command)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_out_file 
          (setf (getf options '$gnuplot_out_file)
                (check-option (cdr opt) #'stringp "a string" 1)))
	 ($gnuplot_script_file
	  (setf (getf options '$gnuplot_script_file)
		(check-option (cdr opt) #'(lambda(x) (or (stringp x) (symbolp x))) "a string or symbol" 1)
		(getf options '$plot_format) '$gnuplot))
         ($gnuplot_pm3d
          (setf (getf options '$gnuplot_pm3d)
                (check-option-boole (cdr opt))))
         ($gnuplot_strings
          (setf (getf options '$gnuplot_strings)
                (check-option-boole (cdr opt))))
         ($gnuplot_svg_background
          (setf (getf options '$background_color)
                (check-option-b (cdr opt) #'plotcolorp "a color" 1)))
         ($gnuplot_preamble
          (setf (getf options '$gnuplot_preamble)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_postamble
          (setf (getf options '$gnuplot_postamble)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_pdf_term_command
          (setf (getf options '$gnuplot_pdf_term_command)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_png_term_command
          (setf (getf options '$gnuplot_png_term_command)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_ps_term_command
          (setf (getf options '$gnuplot_ps_term_command)
                (check-option (cdr opt) #'stringp "a string" 1)))
         ($gnuplot_svg_term_command
          (setf (getf options '$gnuplot_svg_term_command)
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
                (setf (getf options '$gnuplot_term) s)
                (merror
                 (intl:gettext "Wrong argument for plot option \"gnuplot_term\". Expecting a string or a symbol but found \"~M\".") s))))
         (t
          (merror
           (intl:gettext "plot-options-parser: unknown plot option: ~M") opt))))
      ((symbolp opt)
       (case opt
         ($axes (setf (getf options '$axes) t))
         (mbox (setf (getf options 'mbox) t))
         ($color_bar (setf (getf options '$color_bar) t))
         ($color_bar_tics (remf options '$color_bar_tics))
         ($cylindrical (setf (getf options '$transform_xy) '$polar_to_xy))
         ($geomview (setf (getf options '$plot_format) '$geomview))
         ($grid2d (setf (getf options '$grid2d) t))
         ($legend (remf options '$legend))
         ($mesh_lines_color (remf options '$mesh_lines_color))
         ($lighting (setf (getf options '$lighting) t))
         ($logx (setf (getf options '$logx) t))
         ($logy (setf (getf options '$logy) t))
         ($palette (remf options '$palette))
         ($plot_realpart (setf (getf options '$plot_realpart) t))
         ($run_viewer (setf (getf options '$run_viewer) t))
         ($same_xy (setf (getf options '$same_xy) t))
         ($same_xyz (setf (getf options '$same_xyz) t))
         ($xmaxima (setf (getf options '$plot_format) '$xmaxima))
         ($xtics (remf options '$xtics))
         ($ytics (remf options '$ytics))
         ($zmin (remf options '$zmin))
         ($gnuplot (setf (getf options '$plot_format) '$gnuplot))
         ($gnuplot_4_0 (setf (getf options '$gnuplot_4_0) t))
         ($gnuplot_pipes (setf (getf options '$plot_format) '$gnuplot_pipes))
         ($gnuplot_pm3d (setf (getf options '$gnuplot_pm3d) t))
         ($gnuplot_strings (setf (getf options '$gnuplot_strings) t))
         ($noaxes (setf (getf options '$axes) nil))
         ($nobox (setf (getf options 'mbox) nil))
         ($nocolor_bar (setf (getf options '$color_bar) nil))
         ($nocolor_bat_tics (setf (getf options '$color_bat_tics) nil))
         ($nogrid2d (setf (getf options '$grid2d) nil))
         ($nolegend (setf (getf options '$legend) nil))
         ($nolighting (setf (getf options '$lighting) nil))
         ($nologx (setf (getf options '$logx) nil))
         ($nology (setf (getf options '$logy) nil))
         ($nomesh_lines (setf (getf options '$mesh_lines_color) nil))
         ($nopalette (setf (getf options '$palette) nil))
         ($noplot_realpart (setf (getf options '$plot_realpart) nil))
         ($norun_viewer (setf (getf options '$run_viewer) nil))
         ($noxlabel (setf (getf options '$xlabel) ""))
         ($nosame_xy (setf (getf options '$same_xy) nil))
         ($nosame_xyz (setf (getf options '$same_xyz) nil))
         ($notransform_xy (remf options '$transform_xy))
         ($noxtics (setf (getf options '$xtics) nil))
         ($noylabel (setf (getf options '$ylabel) ""))
         ($noytics (setf (getf options '$ytics) nil))
         ($noztics (setf (getf options '$ztics) nil))
         ($nozlabel (setf (getf options '$zlabel) ""))
         ($nognuplot_strings (setf (getf options '$gnuplot_strings) nil))
         ($nognuplot_svg_background (setf (getf options '$gnuplot_svg_background) nil))
         ($spherical (setf (getf options'$transform_xy) '$spherical_to_xyz))
         (t
          (merror (intl:gettext "Unknown plot option \"~M\".") opt))))))
  options)

;; natural numbers predicate
(defun naturalp (n) (or (and (integerp n) (> n 0)) nil))

;; positive real numbers predicate
(defun realpositivep (x) (or (and (realp x) (> x 0)) nil))

(defun real01p (x)
  "Test for real numbers between 0 and 1"
  (or (and (realp x) (>= x 0) (<= x 1)) nil))

;; possible values for the axes option
(defun axesoptionp (o) (if (member o '($x $y $solid)) t nil))

;; the 13 possibilities for the point types
(defun pointtypep (p)
  (if (member p  '($bullet $circle $plus $times $asterisk mbox $square
                  $triangle $delta $wedge $nabla $diamond $lozenge)) t nil))

;; tries to convert az into a floating-point number between 0 and 360
(defun parse-azimuth (az) (mod (coerce-float (meval* az)) 360))

;; tries to convert el into a floating-poitn number between -180 and 180
(defun parse-elevation (el) (- (mod (+ 180 (coerce-float (meval* el))) 360) 180))

;; The following functions check the value of an option returning an atom
;;  when there is only one argument or a list when there are several arguments

;; Checks for one or more items of the same type, using the test given.
;; If count is given, the number of arguments must be exactly count.
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

;; Accepts one or more items of the same type or false.
;; When given, count is the maximum number of items.
(defun check-option-b (option test type &optional count)
  (let ((n (- (length option) 1)))
    (when count
      (unless (< n (1+ count))
        (merror
         (intl:gettext
          "Plot option ~M must have ~M arguments, not ~M.")
         (car option) count (1- (length option)))))
    (cond 
      ((= n 0)
       (merror
        (intl:gettext
         "Option ~M should be given arguments, or called by its name (no lists)")
        option))
      ((= n 1)
       (if (or (funcall test (cadr option)) (null (cadr option))
               (eq (cadr option) t))
           (cadr option)
           (merror
            (intl:gettext
             "Value of option ~M. should be ~M or false, not \"~M\".")
            (car option) type (cadr option))))
      ((> n 1)
       (dotimes (i n)
         (unless (funcall test (nth (+ i 1) option))
           (merror
            (intl:gettext
             "Value of option ~M should be ~M, not \"~M\".")
            (car option) type (nth (+ i 1) option))))
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
  (setq formats '($geomview $gnuplot $gnuplot_pipes $xmaxima))
  (unless (member (cadr option) formats)
    (merror
     (intl:gettext
      "Wrong argument ~M for option ~M. Must one of the following symbols: geomview, gnuplot, xmaxima (or gnuplot_pipes in Unix)")
     (cadr option) (car option)))
  (cadr option))

(defun check-option-palette (option)
"Checks that a palette option is either a valid color, a maxima list of valid
colors, or the name of a predfined palette.
For backward compatibily, it is also accepted a maxima list starting with the
keyword $hue, $saturation, $value or $gradient; the first three types must be
followed by 4 floating-point numbers, while gradient must be followed by a
list of at least two valid colors.
It returns the definition of the palette, if it is valid, or nil."
  (let ((palette (cadr option)))
    (cond
     ((atom palette)
      (when (not (getf *plot-palettes* palette))
        (unless (plotcolorp palette)
          (merror
           (intl:gettext
            "Palette: ~M is not a predefined palette or a valid color.")
           palette))))
     (($listp palette)
      (if (member (cadr palette) '($hue $saturation $value $gradient))
          (case (cadr palette)
                ($gradient
                 (when (< (length (cddr palette)) 2)
                   (merror
                    (intl:gettext
                     "Palette: gradient needs at least 2 colors.")))
                 (dolist (c (cddr palette))
                   (unless (plotcolorp c)
                     (merror 
                      (intl:gettext "Palette: ~M is not a valid color") c))))
                (($hue $saturation $value)
                 (setf (cddr palette) (mapcar #'coerce-float (cddr palette)))
                 (check-option (butlast (cdr #$[value,0.2,0,0,10]$))
                               #'real01p "a number between 0 and 1" 3)
                 (check-option (cons (cadr palette) (last (cdr palette)))
                               #'realp "a real number" 1)))
        (progn
          (when (< (length (cdr palette)) 2)
            (merror
             (intl:gettext "Palette: at least 2 colors are required.")))
          (dolist (c (cdr palette))
            (unless (plotcolorp c)
              (merror  (intl:gettext "Palette: ~M is not a valid color")
                       c)))))
      (setf palette (cdr palette)))
     (T
      (merror
       (intl:gettext "Palette: expecting a valid palette, found ~M.")
       palette)))
    palette))

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

;; levels can be a single natural number (requested number of levels)
;; or two or more floating-point numbers.
(defun check-option-levels (option)
  (cond
    ((< (length option) 3)
     (check-option option #'naturalp "a natural number" 1))
    (t
     (mapcar #'coerce-float (cdr option))
     (check-option option #'realp "a real number" (1- (length option))))))
           
;; Tries to get n numbers between fmin and fmax of the form d*10^e,
;; where d is either 1, 2 or 5.
;; It returns a list with n or less numbers
;; (adapted from procedure getticks of Xmaxima)
;;
(defun getlevels (fmin fmax n)
  (let ((len (- fmax fmin)) (best 0) levels val fac j1 j2 ans)
    (dolist (v '(0.1 0.2 0.5))
      (setq val (ceiling (/ (log (/ len n v)) (log 10))))
      (setq fac (/ 1 v (expt 10 val)))
      (setq j1 (ceiling (* fmin fac)))
      (setq j2 (floor (* fmax fac)))
      (setq levels nil)
      (do ((j j1 (1+ j))) ((> j j2))
        (push (/ j fac) levels))
      (when (> (length levels) best)
        (setq best (length levels))
        (setq ans (copy-list levels))))
    (reverse ans)))

#| plot2d
Examples:

plot2d (sec(x), [x, -2, 2], [y, -20, 20]);

plot2d (exp(3*s), [s, -2, 2], logy);

plot2d ([parametric, cos(t), sin(t), [t, -%pi, %pi]], same_xy);

xy:[[10,.6], [20,.9], [30,1.1], [40,1.3], [50,1.4]]$
plot2d ( [ [discrete, xy], 2*%pi*sqrt(l/980) ], [l, 0, 50],
[style, points, lines], [color, red, blue], [point_type, box],
[legend, "experiment", "theory"],
[xlabel, "pendulum's length (cm)"], [ylabel, "period (s)"]);

plot2d ( x^2-1, [x, -3, 3], [y, -2, 10], nobox, [color, red],
[ylabel, "x^2-1"], [plot_format, xmaxima]);

plot2d ( x^2+y^2 = 1, [x, -2, 2], [y, -2 ,2]);
|#
(defmfun $plot2d
    (fun &optional xrange &rest extra-options
         &aux
         ($display2d nil) (*plot-realpart* *plot-realpart*)
         (options (copy-tree *plot-options*)) yrange output-file plot)
  ;; fun must be a maxima list with several objects: expressions (simple
  ;; functions), maxima lists (parametric or discrete cases).
  ;; A single parametric or discrete plot is placed inside a maxima list.
  (setf (getf options '$type) "plot2d")
  (when (and (consp fun)
             (or (eq (second fun) '$parametric)
                 (eq (second fun) '$contour)
                 (eq (second fun) '$discrete)))
    (setq fun `((mlist) ,fun)))
  ;; If by now fun is not a maxima list, it is then a single expression
  (unless ($listp fun ) (setq fun `((mlist) ,fun)))
  ;; 2- Get names for the two axis and values for xmin and xmax if needed.
  ;; If any of the objects in the fun list is a simple function,
  ;; the xrange option is mandatory and will provide the name of
  ;; the horizontal axis and the values of xmin and xmax.
  (let ((xrange-required nil) (bounds-required nil) (yrange-required nil)
        small huge)
    #-clisp (setq small (- (/ +most-positive-flonum+ 1024)))
    #+clisp (setq small (- (/ most-positive-double-float 1024.0)))
    #-clisp (setq huge (/ +most-positive-flonum+ 1024))
    #+clisp (setq huge (/ most-positive-double-float 1024.0))
    (setf (getf options '$ybounds) (list small huge))
    (dolist (f (rest fun))
      (if ($listp f)
          (progn
            (case ($first f)
              ($parametric
               (unless bounds-required
                 (setq bounds-required t)
                 ;; Default X and Y bound large so parametric plots don't get
                 ;; prematurely clipped. Don't use most-positive-flonum
                 ;; because draw2d will overflow.
                 (setf (getf options '$xbounds) (list small huge)))
               (check-range ($fourth f)))
              ($contour
               (setq xrange (check-range xrange))
               (setq xrange-required t)
               (unless yrange-required
                 (setq yrange-required t)
                 (if (null extra-options)
                     (merror
                      (intl:gettext "plot2d: contour plots require intervals for the variables in the two axes; only one interval was given."))
                     (progn
                       (setq yrange (pop extra-options))
                       (setq yrange (check-range yrange))
                       (setf (getf options '$xvar) ($first xrange))
                       (setf (getf options '$yvar) ($first yrange))
                       (setf (getf options '$x) (cddr xrange))
                       (setf (getf options '$y) (cddr yrange))))))
              ($discrete)
              (t
               (merror
                (intl:gettext
                 "plot2d: a keyword 'parametric' or 'discrete' missing in ~M")
                f))))
          ;; The expression represents a function, explicit or implicit
          (progn
            (unless xrange-required
              (setq xrange-required t)
              (setq xrange (check-range xrange))    
              (setq xrange-required t)
              (unless (getf options '$xlabel)
                (setf (getf options '$xlabel) (ensure-string (second xrange))))
              (setf (getf options '$xvar) (cadr xrange))
              (setf (getf options '$x) (cddr xrange)))
            (when (and (listp f) (eq 'mequal (caar f)))
              ;; Implicit function
              (unless yrange-required
                (setq yrange-required t)
                (if (null extra-options)
                    (merror
                     (intl:gettext "plot2d: implicit function plots require intervals for the variables in the two axes; only one interval was given."))
                    (progn
                      (setq yrange (pop extra-options))
                      (setq yrange (check-range yrange))
                      (setf (getf options '$yvar) ($first yrange))
                      (setf (getf options '$y) (cddr yrange)))))))))
    (when (not xrange-required)
      ;; Make the default ranges on X nd Y large so parametric plots
      ;; don't get prematurely clipped. Don't use most-positive-flonum
      ;; because draw2d will overflow.
      (setf (getf options '$xbounds) (list small huge))
      (when xrange
        ;; second argument was really a plot option, not an xrange
        (setq extra-options (cons xrange extra-options)))))
  ;; If no global options xlabel or ylabel have been given, choose
  ;; a default value for them: the expressions given, converted
  ;; to Maxima strings, if their length is less than 50 characters,
  ;; or the default "x" and "y" otherwise.
  (when (= (length fun) 2)
    (let ((v (second fun)) xlabel ylabel)
      (cond ((atom v) 
             (setq xlabel "x") (setq ylabel ($sconcat v)))
            ((eq (second v) '$parametric)
             (setq xlabel ($sconcat (third v)))
             (setq ylabel ($sconcat (fourth v))))
            ((eq (second v) '$discrete)
             (setq xlabel "x") (setq ylabel "y"))
            ((eq (second v) '$contour)
             (setq xlabel (ensure-string (getf options '$xvar)))
             (setq ylabel (ensure-string (getf options '$yvar))))
            (t
             (setq xlabel "x") (setq ylabel ($sconcat v))))
      (unless (getf options '$xlabel)
        (if (< (length xlabel) 50) (setf (getf options '$xlabel) xlabel)))
      (unless (getf options '$ylabel)
        (if (< (length ylabel) 50) (setf (getf options '$ylabel) ylabel)))))
  ;; For explicit functions, default ylabel is the name of the 2nd variable
  (when (getf options '$yvar)
    (setf (getf options '$ylabel) ($sconcat (getf options '$yvar))))
  ;; Parse the given options into the options list
  (setq options (plot-options-parser extra-options options))
  (when (getf options '$y) (setf (getf options '$ybounds) (getf options '$y)))
  ;; Remove axes labels when no box is used in gnuplot
  (when (and (member 'mbox options) (not (getf options 'mbox))
             (not (eq (getf options '$plot_format) '$xmaxima)))
    (remf options '$xlabel)
    (remf options '$ylabel))
  ;; check options given
  (let ((xmin (first (getf options '$x))) (xmax (second (getf options '$x))))
    (when
        (and (getf options '$logx) xmin xmax)
      (if (> xmax 0)
          (when (<= xmin 0)
            (let ((revised-xmin (/ xmax 1000)))
              (mtell
               (intl:gettext
                "plot2d: lower bound must be positive when using 'logx'.~%plot2d: assuming lower bound = ~M instead of ~M")
               revised-xmin xmin)
              (setf (getf options '$x) (list revised-xmin xmax))
              (setq xrange `((mlist) ,(second xrange) ,revised-xmin ,xmax))))
          (merror
           (intl:gettext
            "plot2d: upper bound must be positive when using 'logx'; found: ~M")
           xmax))))
  (let ((ymin (first (getf options '$y)))
        (ymax (second (getf options '$y))))
    (when (and (getf options '$logy) ymin ymax)
      (if (> ymax 0)
          (when (<= ymin 0)
            (let ((revised-ymin (/ ymax 1000)))
              (mtell
               (intl:gettext
                "plot2d: lower bound must be positive when using 'logy'.~%plot2d: assuming lower bound = ~M instead of ~M")
               revised-ymin ymin)
              (setf (getf options '$y) (list revised-ymin ymax))))
          (merror
           (intl:gettext
            "plot2d: upper bound must be positive when using 'logy'; found: ~M")
           ymax))))
  (setq *plot-realpart* (getf options '$plot_realpart))
  ;; Creates the object that will be passed to the external graphic program
  (case (getf options '$plot_format)
    ($xmaxima
     (setq plot (make-instance 'xmaxima-plot)))
    ($gnuplot
     (setq plot (make-instance 'gnuplot-plot)))
    ($gnuplot_pipes
     (setq plot (make-instance 'gnuplot-plot))
     (setf (slot-value plot 'pipe) T))
    (t
     (merror (intl:gettext "plot2d: plot format ~M not supported")
             (getf options '$plot_format))))   
  ;; Parse plot object and pass it to the graphic program
  (setq output-file (plot-preamble plot options))
  (plot2d-command plot fun options xrange)
  (plot-shipout plot options output-file))

(defun msymbolp (x)
  (and (symbolp x) (char= (char (symbol-value x) 0) #\$)))

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

(defun output-points (points &optional m)
  "If m is supplied print blank line every m lines"
  (let ((j -1))
    (declare (fixnum j))
    (loop for i below (length points)
           do (print-pt (aref points i))
           (setq i (+ i 1))
           (print-pt (aref points i))
           (setq i (+ i 1))
           (print-pt (aref points i))
           (terpri $pstream)
           (cond (m
                  (setq j (+ j 1))
                  (cond ((eql j (the fixnum m))
                         (terpri $pstream)
                         (setq j -1))))))))

;; contour_plot now punts to plot2d
(defmfun $contour_plot (expr &rest optional-args)
  (let ((command "plot2d ([contour, "))
    (setq command ($sconcat command expr "]"))
    (when optional-args
      (dolist (arg optional-args)
        (setq command ($sconcat command ", " arg))))
    (setq command ($sconcat command ")"))
  (mtell (intl:gettext "contour_plot is now obsolete. Using plot2d instead:~%"))
  (mtell "~M~%" command)
  (apply #'$plot2d (cons `((mlist) $contour ,expr) optional-args))))

#| plot3d
Examples:

plot3d (2^(-u^2 + v^2), [u, -3, 3], [v, -2, 2], nopalette);

plot3d ( log ( x^2*y^2 ), [x, -2, 2], [y, -2, 2], [grid, 29, 29]);

expr_1: cos(y)*(10.0+6*cos(x))$
expr_2: sin(y)*(10.0+6*cos(x))$
expr_3: -6*sin(x)$
plot3d ([expr_1, expr_2, expr_3], [x, 0, 2*%pi], [y, 0, 2*%pi],
['grid, 40, 40], [z,-8,8]);

plot3d (cos (-x^2 + y^3/4), [x, -4, 4], [y, -4, 4],
[mesh_lines_color, false], [elevation, 0], [azimuth, 0], [grid, 150, 150]);

spherical: make_transform ([th, phi,r], r*sin(phi)*cos(th),
r*sin(phi)*sin(th), r*cos(phi))$
plot3d ( 5, [th, 0, 2*%pi], [phi, 0, %pi], [transform_xy, spherical],
[palette, [value, 0.65, 0.7, 0.1, 0.9]], [plot_format,xmaxima]);

V: 1 / sqrt ( (x+1)^2+y^2 ) - 1 / sqrt( (x-1)^2+y^2 )$
plot3d ( V, [x, -2, 2], [y, -2, 2], [z, -4, 4]);
|#
(defmfun $plot3d
    (fun &rest extra-options
     &aux
     lvars xrange yrange titles output-file functions exprn domain tem
     (options (copy-tree *plot-options*)) (*plot-realpart* *plot-realpart*)
     (usage (intl:gettext
"plot3d: Usage.
To plot a single function f of 2 variables v1 and v2:
  plot3d (f, [v1, min, max], [v2, min, max], options)
A parametric representation of a surface with parameters v1 and v2:
  plot3d ([f1, f2, f3], [v1, min, max], [v2, min, max], options)
Several functions depending on the two variables v1 and v2:
  plot3d ([f1, f2, ..., fn, [v1, min, max], [v2, min, max]], options)")))
  (setf (getf options '$type) "plot3d")
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
                      ($listofvars
                       (mfuncall
                        (coerce-float-fun (second exprn) lvars "plot3d")
                        (second lvars) (third lvars))))
                (setq vars2
                      ($listofvars
                       (mfuncall
                        (coerce-float-fun (third exprn) lvars "plot3d")
                        (second lvars) (third lvars))))
                (setq vars3
                      ($listofvars
                       (mfuncall
                        (coerce-float-fun (fourth exprn) lvars "plot3d")
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
                                 (getf options '$gnuplot_4_0))
                        (setf (getf options '$const_expr)
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
               (intl:gettext
                "plot3d: argument must be a list of three expressions; found: ~M")
               exprn))))
         (progn
           ;; exprn is a simple function, defined in the global domain.
           (if (and (getf options '$xvar) (getf options '$yvar))
               ;; the global domain has already been defined; use it.
               (progn
                 (setq xrange `((mlist) ,(getf options '$xvar)
                                ,(first (getf options '$x))
                                ,(second (getf options '$x))))
                 (setq yrange `((mlist) ,(getf options '$yvar)
                                ,(first (getf options '$y))
                                ,(second (getf options '$y)))))
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
                       (setf (getf options '$xvar) (second xrange))
                       (setf (getf options '$x) (cddr xrange))
                       (setf (getf options '$yvar) (second yrange))
                       (setf (getf options '$y) (cddr yrange)))
                     (merror usage))))
           ;; ----- GNUPLOT 4.0 WORK-AROUND -----
           (when (and ($constantp exprn) (getf options '$$gnuplot_4_0))
             (setf (getf options '$const_expr) ($float (meval exprn))))
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
  (setq tem (getf options '$transform_xy))
  (when (and (member '$gnuplot_pm3d options) (null (getf options '$gnuplot_pm3d)))
    (setf (getf options '$palette) nil))
   (setq *plot-realpart* (getf options '$plot_realpart))
  ;; set up the labels for the axes, unless no box is being shown
  (unless (and (member 'mbox options) (not (getf options 'mbox)))
    (if (and (getf options '$xvar) (getf options '$yvar) (null tem))
	(progn
	  ;; Don't set xlabel (ylabel) if the user specified one.
	  (unless (getf options '$xlabel)
	    (setf (getf options '$xlabel) (ensure-string (getf options '$xvar))))
	  (unless (getf options '$ylabel)
	    (setf (getf options '$ylabel) (ensure-string (getf options '$yvar)))))
	(progn
	  (setf (getf options '$xlabel) "x")
	  (setf (getf options '$ylabel) "y")))
    (unless (getf options '$zlabel) (setf (getf options '$zlabel) "z")))
  ;; x and y should not be bound, when an xy transformation function is used
  (when tem (remf options '$x) (remf options '$y))
  ;; Set up the plot command
  (let (plot (legend (getf options '$legend)))
    ;; titles will be a list. Titles given in the legend option prevail
    ;; over titles generated by plot3d. No legend if option [legend,false]
    (unless (listp legend) (setq legend (list legend)))
    (when (member '$legend options)
      (if (first legend) (setq titles legend)) (setq titles nil))
    (case (getf options '$plot_format)
      ($xmaxima
       (setq plot (make-instance 'xmaxima-plot)))
      ($gnuplot
       (setq plot (make-instance 'gnuplot-plot)))
      ($gnuplot_pipes
       (setq plot (make-instance 'gnuplot-plot))
       (setf (slot-value plot 'pipe) T))
      ($geomview
       (setq plot (make-instance 'geomview-plot)))
      (t
       (merror (intl:gettext "plot3d: plot format ~M not supported")
               (getf options '$plot_format))))
    ;; Parse plot object and pass it to the graphic program
    (setq output-file (plot-preamble plot options))
    (plot3d-command plot functions options titles)
    (plot-shipout plot options output-file)))

;; Given a Maxima list with 3 elements, checks whether it represents a function
;; defined in a 2-dimensional domain or a parametric representation of a
;; 3-dimensional surface, depending on two parameters.
;; The return value will be a Maxima list if the test is successful or nil
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

