;;Copyright William F. Schelter 1990, All Rights Reserved
(in-package "MAXIMA")
;; see bottom of file for examples


(eval-when (compile) (proclaim '(optimize (safety 0))))


(eval-when (compile eval load)
 (defmacro coerce-float (x)
   `(lisp::float ,x 1.d0))
  )

(defvar *z-range* nil)
(defvar *original-points* nil)
(defvar $axes_length 4.0)
(defvar *rot* (make-array 9 :element-type 'long-float))
(defvar $rot nil)

(defvar $plot_options '((mlist)
			((mlist) |$x| -3 3)
			((mlist) |$y| -3 3)
			((mlist) $grid 30 30)
			((mlist) $view_direction 1 1 1)
			((mlist) $colour_z nil)
			((mlist) $transform_xy nil)
			((mlist) $run_viewer t)
			((mlist) $plot_format $openmath)
			((mlist) $nticks 100)
			))

(defun $get_plot_option (name &optional n)
  (sloop for v in (cdr $plot_options)
     when (eq (nth 1 v) name) do
     (return (if n (nth n  v) v))))

(defun check-list-items (name lis type length)
  (or (eql (length lis) length)
      (merror "~M items were expected in the ~M list" length name))
  `((mlist) , name ,@
    (sloop for v in lis
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
	(ecase name
	  ((|$x| |$y|) (check-list-items name (cddr value) 'number 2)
	   (check-range value)
	   )
	  ($view_direction (check-list-items name (cddr value) 'number 3))
	  ($grid  (check-list-items name (cddr value) 'fixnum 2))
	  ($nticks  (check-list-items name (cddr value) 'fixnum 1))
	  (($colour_z $run_viewer $transform_xy)
	   (check-list-items name (cddr value) 't 1))
	  ($plot_format (or (member (nth 2 value) '($zic $geomview $ps
							 $gnuplot
							 $zplot
							 $openmath
							 ))
			    (merror "Only [zic,geomview,ps,openmath,gnuplot] are available"))
			value)
	  ))
  (sloop for v on (cdr $plot_options)
     when (eq (nth 1 (car v)) name)
     do (setf (car v) value))
  $plot_options
  )
  
(defvar $pstream nil)



(defun print-pt1 (f str)
  (format str "~,3f " f))

(defmacro defbinop (name op type)
  `(progn
     (defun ,name (x y) (the ,type (,op  (the ,type x) (the ,type y))))
     (eval-when (compile eval)     
     (#+kcl si::DEFINE-COMPILER-MACRO #-kcl
	    defmacro ,name (x y)
         `(the ,',type (,',op  (the ,',type ,x) (the ,',type ,y)))))))

(defbinop f+ + fixnum)
(defbinop f- - fixnum)

(defbinop $+ + long-float)
(defbinop $- - long-float)
(defbinop $* * long-float)
(defbinop $/ / long-float)

(eval-when (compile eval)

(defmacro f* (a b &optional c)
  (if c `(f* (f* ,a ,b) ,c)
    `(the fixnum (* (the fixnum ,a) (the fixnum ,b)))))

(defmacro float-< (a b) `(< (the long-float ,a) (the long-float ,b)))
  



(defmacro z-pt (ar i) `(aref ,ar (the fixnum (+ 2 (* ,i 3)))))
(defmacro y-pt (ar i) `(aref ,ar (the fixnum (+ 1 (* ,i 3)))))
(defmacro x-pt (ar i) `(aref ,ar (the fixnum (+ 0 (* ,i 3)))))
(defmacro rot (m i j) `(aref ,m (the fixnum (+ ,i (the fixnum (* 3 ,j))))))



(defmacro print-pt (f)
  `(print-pt1 ,f $pstream ))

(defstruct (polygon (:type list)
		    (:constructor make-polygon (pts edges))
		    )
  (dummy '($polygon simp))
  pts edges)

#-cmu ;; somehow this definition undefines whole structure for cmulisp
(defmacro make-polygon (a b) `(list '($polygon) ,a ,b))
)

(defun draw3d (f minx maxx miny maxy  nxint nyint)
  (let* ((epsx (/ (- maxx minx) nxint))
	 (x 0.0)  ( y 0.0)
	 (epsy (/ (- maxy miny) nyint))
	 (nx (+ nxint 1))
	 (l 0)
	 (ny (+ nyint 1))
	 (ar (make-array  (+ 12  ; 12  for axes
			     (f* (f* 3 nx) ny))  :fill-pointer (f* (f* 3 nx) ny)
			 :element-type 'long-float
			 :adjustable t
			 )))
    (declare (long-float x y epsy epsx)
	     (fixnum nx  ny l)
	     (type (array long-float) ar))
    (sloop for j below ny
	   initially (setq y miny)
	   do (setq x minx)
	   (sloop for i below nx
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
	     (type (array (mod 65000)) tem))
    (sloop for k below (length tem)
	   do
	   (setf (aref tem k) i)
	   (setf (aref tem (incf k))
		 (+ nxpt i))
	   (setf (aref tem (incf k))
		 (+ nxpt (incf i )))
	   (setf (aref tem (incf k)) i)
	   (setf (aref tem (incf k)) 0)	;place for max
	   (setq m (- m 1))
	   (cond ((eql  m 0)
		  (setq m nx)
		  (setq i (+ i 1))))
	   )
    tem))

(defun add-axes (pts vert)
  (let ((origin (/ (length pts) 3)))
    (sloop for i from -3 below 9
	   do
	   (vector-push-extend  (if (eql 0 (mod i 4)) $axes_length 0.0) pts))
    (sloop for i below 15
	   do (vector-push-extend
	       (if (eql 1 (mod i 5)) (+ origin (ceiling i 5))  origin)
	       vert)
	   )))

(defun $rotation1 (phi th)
  (let ((sinph (sin phi))
	(cosph (cos phi))
	(sinth (sin th))
	(costh (cos th)))
    `(($MATRIX SIMP)
      ((MLIST SIMP) ,(* COSPH COSTH)
       ,(* -1.0 COSPH SINTH)
       ,SINPH)
      ((MLIST SIMP) ,SINTH ,COSTH 0.0)
      ((MLIST SIMP) ,(- (*  SINPH COSTH))
       ,(* SINPH SINTH)
       ,COSPH))))
   
; pts is a vector of bts [x0,y0,z0,x1,y1,z1,...] and each tuple xi,yi,zi is rotated
; also the *z-range* is computed.
(defun $rotate_pts(pts rotation-matrix)
  (or ($matrixp rotation-matrix) (error "second arg not matrix"))
  (let* ((rot *rot*)
	 (l (length pts))
	 (x 0.0) (y 0.0) (z 0.0)
	 )
    (declare (long-float  x y z))
    (declare (type (array long-float) rot))
    ($copy_pts rotation-matrix *rot* 0)
	
;    (setf (rot rot  0 0) (* cosphi costh))
;    (setf (rot rot  1 0) (- sinth))
;    (setf (rot rot 2 0) (* costh sinphi))
;
;    (setf (rot rot  0 1) (* cosphi sinth))
;    (setf (rot rot  1 1) costh)
;    (setf (rot rot 2 1) (* sinth sinphi))
;
;    (setf (rot rot  0 2) (- sinphi))
;    (setf (rot rot  1 2) 0.0)
;    (setf (rot rot 2 2) cosphi)

    (sloop with j = 0
	   while (< j l)
	   do
	   (setq x (aref pts j))
	   (setq y (aref pts (+ j 1)))
	   (setq z (aref pts (+ j 2)))
	   (sloop for i below 3 with a = 0.0
		  declare (long-float a)
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

(defun $get_range (pts k &aux (z 0.0) (max most-negative-long-float) (min most-positive-long-float))
  (declare (long-float z max min))
  (declare (type (vector long-float) pts))
  (sloop for i from k below (length pts) by 3
	 do (setq z (aref pts i))
	 (cond ((< z min) (setq min z)))
	 (cond ((> z max) (setq max z))))
  (list min max (- max min)))


;; if this is '$polar_to_xy then the x y coordinates are interpreted as r theta

(defun add-ps-finish (opts)
  (p (if opts
	 "/xr .30 def
/xb 0.60 def
/xg .60  def
/myset { .005 mul dup xr add exch
 dup xb add exch
 xg add
setrgbcolor} def

/myfinish { myset  gsave fill grestore 0 setgray stroke  } def"

       "/myfinish {.9 setgray gsave fill grestore .1 setgray stroke  } def")))


(defun $draw_ngons(pts ngons number_edges &aux (i 0)(j 0) (s 0)
		       (opts *original-points*)
		       (maxz  most-negative-long-float))
  (declare (type (array long-float) pts opts)
	   (type (array (mod 64000)) ngons)
	   (fixnum number_edges i s j number_edges)
	   (long-float maxz))
  (setq j (length ngons))
  (add-ps-finish opts)
  (sloop while (< i j) 
	 do 
	 (sloop initially (setq s number_edges)
		do
		;(print-pt (aref pts (f* 3 (aref ngons i))))
		(print-pt (x-pt pts  (aref ngons i)))
		;(print-pt (aref pts (f+ 1 (f* 3 (aref ngons i)))))
		(print-pt (y-pt pts  (aref ngons i)))
		
		(cond (opts (if (> (z-pt opts (aref ngons i)) maxz)
			      (setq maxz (z-pt opts (aref ngons i))))))
		(cond ((eql number_edges s) (p " moveto %"
					       ;(aref pts (f+ 2 (f* 3 (aref ngons i))))
					       
					       ))
		      (t (p "lineto %" ;(aref pts (f+ 2 (f* 3 (aref ngons i))))
			    )))
		(setq i (f+ i 1))
		while (> (setq s (f- s 1)) 0))
	 (setq i (f+ i 1))
	 (cond (opts
		(p (f+ 1 (round ($* 100.0 ($/ ($- maxz (car *z-range*))
					  (or (third *z-range*)
					      ($- (second *z-range*) (car *z-range*))))))))
		(setq maxz most-negative-long-float)
		))
	 (p " myfinish")
	 ))

;; figure out the rotation to make pt the direction from which we view,
;; and to rotate z axis to vertical.
;; First get v, so p.v=0 then do u= p X v to give image of y axis
;; ans = transpose(matrix( v,u,p))

(defun $norm (pt) (sloop for v in (cdr pt) sum (* v v)))
(defun $length_one (pt)
  (let ((len (sqrt ($norm pt))))
    (cons '(mlist) (sloop for v in (cdr pt) collect (/  (float v) len)))))

(defun $cross_product (u v)
  (flet ((cp (i j)
	     (- (* (nth i u) (nth j v))
		(* (nth i v) (nth j u)))))
	`((mlist) ,(cp 2 3) ,(cp 3 1) ,(cp 1 2))))
	
(defun $GET_ROTATION (pt)
  (setq pt ($length_one pt))
  (let (v tem u)
    (cond((setq tem (position 0.0 pt))
	   (setq v (cons '(mlist) (list 0.0 0.0 0.0)))
	   (setf (nth tem v) 1.0))
	  (t (setq v ($length_one `((mlist) ,(- (nth 2 pt))      , (nth 1 pt) 0.0)))))
    (setq u ($cross_product pt v))
    (let* (($rot   `(($matrix) ,v,u,pt))
	   (th (get-theta-for-vertical-z
		(nth 3 (nth 1 $rot))
		(nth 3 (nth 2 $rot)))))
      (or (zerop th)
	  (setq $rot (ncmul2 ($rotation1 0.0 th)     $rot)))
      $rot)))

(defun get-theta-for-vertical-z (z1 z2)
  (cond ((eql z1 0.0) (if (> z2 0.0) 0.0  pi))
	(t (lisp::atan  z2 z1 ))))

(defun $ps_axes ( rot )
  (let ((tem (make-array 9 :element-type 'long-float)))
    (setf (aref tem 0) 4.0)
    (setf (aref tem 4) 4.0)
    (setf (aref tem 8) 4.0)
    ($rotate_pts tem rot)
    (p 0 0 "moveto")
    (p (aref tem 0) (aref tem 1) "lineto stroke")
    (p  (aref tem 0) (aref tem 1) "moveto (x) show")
    (p 0 0 "moveto")
    (p (aref tem 3) (aref tem 4) "lineto stroke")
    (p  (aref tem 3) (aref tem 4) "moveto (y) show")    
    (p 0 0 "moveto")
    (p (aref tem 6) (aref tem 7) "lineto stroke")
    (p  (aref tem 6) (aref tem 7) "moveto (z) show")    
    ))

(defun $polar_to_xy (pts &aux (r 0.0) (th 0.0))
  (declare (long-float r th))
  (declare (type (array long-float) pts))
  (assert (typep pts '(vector long-float)))
  (sloop for i below (length pts) by 3
	 do (setq r (aref pts i))
	 (setq th (aref pts (f+ i 1)))
	 (setf (aref pts i) (* r (cos th)))
	 (setf (aref pts (f+ i 1)) (* r (sin th)))))

(defun coerce-function-body (f lvars)
  (setq f (coerce-float-fun f lvars))
  (if (symbolp f) (symbol-function f) f))

;; return a function suitable for the transform function in plot3d.
(defun $make_transform (lvars fx fy fz  &aux ( $numer t))
  (setq fx (coerce-function-body fx lvars))
  (setq fy (coerce-function-body fy lvars))
  (setq fz (coerce-function-body fz lvars))
  (let ((sym (gensym "transform")))
    (setf (symbol-function sym)
  #'(lambda (pts &aux  (x1 0.0)(x2 0.0)(x3 0.0))
      (declare (long-float  x1 x2 x3))
      (declare (type (array long-float) pts))
      (sloop for i below (length pts) by 3
	     do 
	 (setq x1 (aref pts i))
	 (setq x2 (aref pts (f+ i 1)))
	 (setq x3 (aref pts (f+ i 2)))
	 (setf (aref pts i) (funcall fx x1 x2 x3))
	 (setf (aref pts (f+ 1 i)) (funcall fy x1 x2 x3))
	 (setf (aref pts (f+ 2 i)) (funcall fz x1 x2 x3)))))
  ))


(defun coerce-float-fun (expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
	 expr)
	((and (symbolp expr) (not (member expr lvars)))
	 (cond ((fboundp expr) expr)
	       (t (mfuncall '$translate expr) expr)))
	(t
	 (let ((vars (or lvars ($sort ($listofvars expr))))
	       (na (gensym "TMPF")))
	   (when (and (eql (length vars) 2)
		      (member '$r vars))
	     ;($set_plot_option '((mlist) $transform_xy $polar_to_xy))
   	;(format t "Using polar coordinates")
		 )
	 (meval* `((MDEFINE SIMP) ((,na) ,@(cdr vars))
		     ((MPROGN) ;(($MODEDECLARE) ,vars  $FLOAT)
		      ((mprog) ((mlist) ((msetq) tem ,expr))
		       ((mcond) ((complexp) tem) ((realpart) tem) t tem)))))
	 (coerce-float-fun na)))))

(defun coerce-float-fun (expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
	 expr)
	((and (symbolp expr) (not (member expr lvars)))
	 (cond ((fboundp expr) expr)
	       (t
		(let* ((mexpr (mget expr 'mexpr))
		       (args (nth 1 mexpr)))
		  (or mexpr (merror "Undefined function ~a" expr))
		(coerce `(lambda ,(cdr args)
			     (declare (special ,@(cdr args)))
			     ($realpart(meval* ',(nth 2 mexpr)))) 'function)))))
	(t
	 (let ((vars (or lvars ($sort ($listofvars expr))))
	       ;(na (gensym "TMPF"))
		)
	   (coerce `(lambda ,(cdr vars) (declare (special ,@(cdr vars)))
			($realpart (meval* ',expr)))'function)))))

(defmacro zval (points verts i) `(aref ,points (f+ 2 (f* 3 (aref ,verts ,i)))))

;;sort the edges array so that drawing the edges will happen from the back towards
;; the front.   The if n==4 the edges array coming in looks like
;; v1 v2 v3 v4 0 w1 w2 w3 w4 0 ...
;; where vi,wi are indices pointint into the points array specifiying a point
;; in 3 space.   After the sorting is done, the 0 is filled in with the vertex
;; which is closer to us (ie highest z component after rotating towards the user)
;; and this is then they are sorted in groups of 5.   
(defun sort-ngons (points edges n &aux lis )
  (declare (type (array (long-float))  points)
	   (type (array (mod 65000)) edges)
	   (fixnum n))
  (let ((new (make-array (length edges) :element-type  (array-element-type edges)))
	(i 0)
	(z 0.0)
	(z1 0.0)
	(n1 (- n 1))
	(at 0)
	(leng (length edges))
	)
    (declare (type (array (mod 65000)) new)
	     (fixnum i leng n1 at )
	     )
    (declare (long-float z z1))
    
    (setq lis
	  (sloop  for i0 below leng by (+ n 1)
		  do 
		  (setq i i0)
		  (setq at 0)
		  (setq z (zval points edges i))
		  (setq i (+ i 1))
		  (sloop for j below n1
			 do (if (> (setq z1 (zval points edges i))  z)
				(setq z z1 at (aref edges i) ))
			 (setq i (+ i 1))
			 )
		  (setf (aref edges i) at)
		  collect (cons z i0)))
    (setq lis (sortcar lis))
    (setq i 0)
    (sloop for v in lis
	   do
	   (sloop for j from (cdr v) 
		  for k to n
		  do (setf (aref new i) (aref edges j))
		  (incf i))
	   )
    (copy-array-portion edges new  0 0 (length edges))
    ))

(defun copy-array-portion (ar1 ar2 i1 i2 n1)
 (declare (fixnum i1 i2 n1))
 (sloop while (>= (setq n1 (- n1 1)) 0)
        do (setf (aref ar1 i1) (aref ar2 i2))
         (setq i1 (+ i1 1))
        (setq i2 (+ i2 1))))

(defun $concat_polygons (pl1 pl2 &aux tem new)
  (setq new
	  (sloop for v in pl1 
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
  (sloop for i from (length (polygon-edges pl1))
	 for j from 0 below (length (polygon-edges pl2))
	 with  lpts1  =  (length (polygon-pts pl1))
	 with ar2   =  (polygon-edges pl2)
	 with arnew =  (polygon-edges new)
	 do (setf (aref arnew i) (f+ lpts1 (aref ar2 j)))))

(defun $copy_pts(lis vec start)
  (declare (fixnum start))
  (let ((tem vec))
    (declare (type (array long-float) tem))
    (cond ((numberp lis)
	   (or (typep lis 'long-float) (setq lis (float lis 0.0)))
	   (setf (aref tem start) lis)
	   
	   (+ start 1))
	  ((typep lis 'cons)
	   ($copy_pts (cdr lis) vec  ($copy_pts (car lis) vec start)))

	  ((symbolp lis) start)
	  (t (error "bad lis")))))
  

;; arrange so that the list of points x0,y0,x1,y1,.. on the curve
;; never have abs(y1-y0 ) and (x1-x0) <= deltax 

(defun draw2d (f range &optional
		 (nticks (nth 2($get_plot_option '$nticks)))
		 (yrange ($get_plot_option '|$y|))
		 ($numer t)
		 )
				  
  (setq f (coerce-float-fun f `((mlist), (nth 1 range))))

  (let* ((x (coerce-float (nth 2 range)))
	 (xend (coerce-float (nth 3 range)))
	 (ymin (coerce-float (nth 2 yrange)))
	 (ymax (coerce-float (nth 3 yrange)))
	 (eps ($/ (- xend x) (coerce-float nticks)))
	 (x1 0.0)
	 (y1 0.0)
	 (y (funcall f x))
	 (dy 0.0)
         (epsy ($/ (- ymax ymin) 1.0))
	 (eps2 (* eps eps))
	 in-range last-ok
	 )
    (declare (long-float x1 y1 x y dy eps2 eps ymin ymax ))
   ;(print (list 'ymin ymin 'ymax ymax epsy))
     (setq x ($- x eps))  
    (cons '(mlist)
	  (sloop   do
					;	     (format t "~%(~,3f ~,3f)(~,3f ~,3f) [~,3f] ~,3f ----1" x y x1 y1 dy eps)
	     (setq x1 ($+ eps x))
	     (setq y1 (funcall f x1))
	     (setq in-range (and (<= y1 ymax) (>= y1 ymin)))
	     (cond (in-range
		    ;; (format t "~%(~,3f ~,3f)(~,3f ~,3f) [~,3f] ~,3f ----2" x y x1 y1 dy eps)
		    (setq dy (- y1 y))
		    ;;  (format t "~%(~,3f ~,3f)(~,3f ~,3f) [~,3f] ~,3f ----3" x y x1 y1 dy eps)

		    (cond ((< dy 0) (setq dy (- dy))))
		    (cond ((> dy eps)
			   (setq x1 (+ x (/ eps2 dy)))
			   (setq y1 (funcall f x1))
			   (setq in-range (and (<= y1 ymax) (>= y1 ymin)))
			   (or in-range (setq x1 (+ eps x)))
			   )
			  ))
		   )
	     (setq x x1)
	     (setq y y1)
	     when (or (and (not last-ok)
		      	 (not in-range))
		      (> dy epsy))

		       collect 'moveto and collect 'moveto
	     do
	     (setq last-ok in-range)
;	     (print x)
		; (format t "~%(~,3f ~,3f)(~,3f ~,3f) [~,3f] ~,3f ----4" x y x1 y1 dy eps)
		;  (show (list x y dy))
	     collect x1 
	     collect (if in-range y1 (if (> y1 ymax) ymax ymin))

	     when (>= x xend)
	     collect xend and
	     collect (let ((tem (funcall f xend)))
		       (if (>= tem ymax) ymax (if (<= tem ymin) ymin tem)))
	     and do (sloop::loop-finish)))))

(defun get-range (lis)
  (let ((ymin most-positive-long-float)
	(ymax most-negative-long-float))
    (declare (long-float ymin ymax))
    (do ((l lis (cddr l)))
	((null l))
	(or (floatp (car l)) (setf (car l) (float (car l) #. (coerce 2 'long-float))))
      (cond ((float-<   (car l)ymin)
	     (setq ymin (car l))))
      (cond ((float-<  ymax  (car l))
	      (setq ymax (car l)))))
    (list '(mlist) ymin ymax)))

(defvar $window_size '((mlist)
		       #.(* 8.5 72) #. (* 11 72) ))

(defun $getrange(x &optional xrange &aux yrange)
  (setq yrange  (cdr (get-range (cddr x))))
  (or xrange (setq xrange (cdr (get-range (cdr x)))))
  (setup-for-ps-range xrange yrange nil))
 
(defun $paramplot (f g range &optional (delta .1 supplied) &aux pts ($numer t)
		     )
  (setq f (coerce-float-fun f))
  (setq g (coerce-float-fun g))
  (setq range (meval* range))
  (or supplied (setq delta (/ (- (nth   2 range) (nth 1 range)) (nth 2 ($get_plot_option '$nticks)))))
  (setq pts(cons '(Mlist)
		 (sloop with tt = (coerce-float (nth 1 range))
		    with end = (coerce-float (nth 2 range))
		    while (float-< tt end)
		    collect (funcall f tt)
		    collect (funcall g tt)
		    do (setq tt ($+ tt delta)))))
  ($closeps)
  ($getrange pts)
  ($psdraw_curve pts)
  ($closeps)
  ($viewps))
  
(defun $plot2d_ps(fun range &rest options &aux ($numer t)  $display2d
			  ($plot_options $plot_options))
  (dolist (v options) ($set_plot_option v))
  (setq range (check-range range))
  (let ((tem (draw2d fun range )))
    ($closeps)
    ($getrange tem (cddr range))
    ($psdraw_curve tem)
    ($psaxes ($rest range))
    (p "showpage")
    ($viewps)))


(defvar $gnuplot_command "mgnuplot")
(defvar $geomview_command "geomview maxout.geomview")

(defvar $openmath_plot_command "omplotdata")

(defun $plot2d(fun range &rest options &aux ($numer t) $display2d
                          (i 0) plot-format file plot-name
			  ($plot_options $plot_options))
  (dolist (v options) ($set_plot_option v))
  (cond ((eq ($get_plot_option '$PLOT_FORMAT 2) '$openmath)
         (return-from $plot2d (apply '$plot2dOpen fun range options))))
  (setq range (check-range range))
  (or ($listp fun ) (setf fun `((mlist) ,fun)))
  (setq plot-format  ($get_plot_option '$plot_format 2))
  (setq file (format nil "maxout.~(~a~)" (stripdollar plot-format)))
  
  (with-open-file (st  file :direction :output)  
    (dolist (v (cdr fun))
	    (incf i)
	    (setq plot-name 		       (let ((string (coerce (mstring v) 'string)))
			 (cond ((< (length string) 9) string)
			       (t (format nil "Fun~a" i)))))
	(case plot-format
	      ($xgraph
	       (format st "~%~% \"~a\"~%" plot-name))
	      ($gnuplot
	       (format st "~%~%# \"~a\"~%" plot-name))
	       )
      (sloop for (v w) on (cdr (draw2d v range )) by 'cddr
	 do
	 (cond ((eq v 'moveto) (format st "move "))
	       (t  (format st "~,3f ~,3f ~%" v w))))))
  (case plot-format
	($gnuplot 
	 ($system (maxima-bin-search $gnuplot_command) " -plot2d maxout.gnuplot -title '" plot-name "'"))
	($xgraph
	 ($system "xgraph -t 'Maxima Plot' < maxout.xgraph &"))
	))

(defun maxima-bin-search (command)
  (or ($file_search command
		    `((mlist) , (maxima-path "bin" "###")))
		 command))
    


(defun $plot2dOpen(fun range &rest options &aux ($numer t)  $display2d
                          (i 0)
			  ($plot_options $plot_options)
	)
  (declare (special linel))
  (dolist (v options) ($set_plot_option v))
  (setq range (check-range range))
  (or ($listp fun ) (setf fun `((mlist) ,fun)))
  (show-open-plot
   (with-output-to-string
    (st )
    (format st "~%{plot2d ~%")
    (sloop for f in (cdr fun)
	   do
	   (incf i)
	   (format st " {label \"~a\"}~% "
		   (let ((string (coerce (mstring f) 'string)))
		     (cond ((< (length string) 9) string)
			   (t (format nil "Fun~a" i))))
		   )
	   (format st "{xversusy ~%")
	   (let ((lis (cdr (draw2d f range ))))

	     (sloop while lis
		    do

		    (sloop while (and lis (not (eq (car lis) 'moveto)))

			   collecting (car lis) into xx
			   collecting (cadr lis) into yy
			   do (setq lis (cddr lis))
			   finally
			   ;; only output if at least two points for line
			   (cond ((cdr xx)
				  (tcl-output-list st xx)
				  (tcl-output-list st yy)
				  ))
			   ; remove the moveto
			   )
		    (setq lis (cddr lis))
		    ))
	   (format st "~%}"))
    (format st "~%} ~%"))))


(defvar $In_Netmath nil)
(eval-when (load)
  (cond ($In_Netmath
     (setf (symbol-function '$plot2d) (symbol-function '$plot2dopen))
     (setf $show_openplot nil)
    )))

(defun $sprint(&rest args )
  (sloop for v in args do
	 (cond ((symbolp v)
		(setq v (string-left-trim '(#\$ #\&) (symbol-name v))))
	       ((numberp v) v)
	       (t (setq v (implode (strgrind v)))))
	 (princ v)
	 (princ " ")
  )
  (car args)
  )

(defun $show_file(file)
  (princ (file-to-string ($file_search file)))
  '$done)


(defun $tcl_output  (lis i &optional (skip 2))
    (or (typep i 'fixnum) (error "~a should be an integer" i ))
    ($listp_check 'list lis )
    (format *standard-output* "~% {")
    (cond (($listp (nth 1 lis))
	   (sloop for v in lis
		  do
		  (format *standard-output* "~,10f " (nth i v)))
	   )
	  (t
	   (setq lis (nthcdr i lis))
	   (sloop  with v = lis  while v
		  do
		  (format *standard-output* "~,10f " (car v))
		  (setq v (nthcdr skip v))
		  )
	   ))
    (format *standard-output* "~% }")
    )

		   
		  
	   
				
				
	   

           



(defun tcl-output-list ( st lis )
  (cond ((null lis) )
	((atom (car lis))
	 (princ " {  " st)
	 (sloop for v in lis
		count t into n
		when (eql 0 (mod n 5))
		do (terpri st)
		do
		(format st "~,10f " v))
	 (format st  " }~%"))
	(t (tcl-output-list st (car lis))
	   (tcl-output-list st (cdr lis)))))




(defun $openplot_curves (lis &aux (linel 100000))
  (declare (special linel))
  (show-open-plot
   (with-output-to-string
    (st )
    (format st "{plot2d ~%")
    (or (and ($listp lis) ($listp (nth 1 lis)))
	(merror "Need a list of curves, [[x1,y1,x2,y2,...],[u1,v1,u2,v2,...]] or [[[x1,y1],[x2,y2],...]"))
    
    (sloop for v in (cdr lis)
	   do
	   (or ($listp v) (merror "should be a list"))
	   (setq v (cdr v))
	   (format st "~%~%")
	   (sloop while (and (car v) (symbolp (car v)))
		  do   (mformat st "~M~%" ($concat (car v)))
		  (setq v (cdr v))
		  )
           when v
	   do	
	   (format st "~%{ xversusy  ")
	   (sloop while v
		  with this with xvals with yvals
		  do
		  (cond   ((numberp (car v))
			   (setq this v) (setq v (cddr v))
			   (push (car this) xvals)
			   (push (second this) yvals)			   
			   )
			  (($listp (car v))
			   (setq this (cdar v))
			   (push (car this) xvals)
			   (push (second this) yvals)
			   (and (third this) (push (third this ) yvals))
			   (setq v (cdr v)))
			  (t (princ (list 'bad (car v))) (setq v (cdr v))))

		  finally
		  (tcl-output-list st (nreverse xvals))
		  (tcl-output-list st (nreverse yvals)))
       	   (format st " }")
	   )
    (format st "}~%")
    )))



(defun $xgraph_curves (lis &rest options &aux w)
  options
  (with-open-file (st  "xgraph-out" :direction :output)
    (format st "=600x600~%")
    (sloop for v in (cdr lis)
       do
       (setq v (cdr v))
       (format st "~%~%")
       (sloop while v
	  do
	  (cond
	   ((symbolp (car v))
	    (mformat st "~M~%" ($concat (car v)))
	    (setq v (cdr v)))
	   (t (cond	   ((numberp (car v))
	    (setq w v) (setq v (cddr v)))
	   (($listp (car v))
	    (setq w (cdar v))
	    (setq v (cdr v))))
	  (format st "~,3f ~,3f ~%" (car w) (second w)))))))
  ($system "xgraph -t 'Maxima Plot' < xgraph-out &"))


     

(defun average-slope (m1 m2)
  (tan ($/ ($+ (lisp::atan m1) (lisp::atan m2)) 2.0)))

(defun slope (x1 y1 x2 y2 &aux (del ($- x2 x1)))
  (declare (long-float x1 y1 x2 y2 del))
  (cond ((eql del 0.0)
	 #. (expt 10 30))
	(t ($/ ($- y2 y1) del))))
	   

;;When we initialize we move the origin to the middle of $window_size
;;Then to offset from that use translate.
(defvar $ps_translate '((mlist) 0 0))

;; initially 1/72 of inch is the scale
(defvar $ps_scale '((mlist) 72  72))


(defun $pscom (&rest l)
  (apply 'p l))

;-10 to 10
(defun psx (x) x)
(defun psy (y) y)

(defun p (&rest l)
  (assureps)
  (sloop for v in l do
	 (if (symbolp v) (setq v (maxima-string v)))
	; (if (numberp v) (setq v (* 70 v)))
	 (cond ((consp v)
		(sloop for w in (cdr v) do (p w)))
	       ((floatp v) (format $pstream "~,4f" v))
	       (t(princ v $pstream)))
	  (princ " " $pstream))
  (terpri $pstream))

(defun psapply (f lis)
  (if ($listp lis) (setq lis (cdr lis)))
  (apply 'p lis)
  (p f))

(defun $moveto (x y)
  (p (psx x) (psy y) "moveto ")) 

(defun $join (x y)
  (cons '(mlist)(sloop for w in (cdr x) for u in (cdr y)
		       collect w collect u)))

(defun $psline (a b c d)
  (p (psx a) (psy b) "moveto ")
  (p  (psx c) (psy d) "lineto"))

(defun setup-for-ps-range (xrange yrange do-prolog)
    (let* ((cy (/ (+ (nth 1 yrange) (nth 0 yrange)) 2.0))
	   (CX (/ (+ (nth 1 xrange) (nth 0 xrange)) 2.0))
	   (scaley (/ (nth 2 $window_size)
		      (* 1.2  (- (nth 1 yrange) (nth 0 yrange)))))
	   (scalex (/ (nth 1 $window_size)
		      (* 1.2 (- (nth 1 xrange) (nth 0 xrange)))))
	   ($ps_scale
	     (progn
	       (cond ((< scalex scaley)
			   (setq scaley scalex)))
		    `((mlist) , scaley ,scaley)))
	   ($ps_translate `((mlist) , CX ,CY)))
      (ASSUREPS do-prolog)))

(defun assureps (&optional do-prolog)
  (cond ((streamp $pstream))
	(t (setq do-prolog t)))
  (or $pstream (setq $pstream (open "maxout.ps" :direction :output)))
  (cond (do-prolog
	  (p "%!")
	  (p  (* .5 (nth 1 $window_size))
	     (* .5 (nth 2 $window_size))
	     "translate"
	     )
	  (psapply "scale" $ps_scale)
	  (p  (- (nth 1 $ps_translate))
	     (- (nth 2 $ps_translate))
	     "translate"
	     )
	  (p " 1.5 " (second $ps_scale) "div setlinewidth
/Helvetica findfont 14 " (second $ps_scale) " div scalefont setfont
/dotradius .05 def
/drawdot {
 /yy exch def
 /xx exch def
  gsave
  xx yy dotradius 0 360 arc
  fill
  grestore
}def

/ticklength  .03 def
/axiswidth  .01 def
/drawtick {
 /yy exch def
 /xx exch def
  gsave
  xx ticklength sub yy moveto
  xx ticklength add yy lineto
  stroke	
  xx yy  ticklength sub  moveto
  xx yy  ticklength add  lineto
  stroke
  grestore
} def
"))))


(defun $closeps ()
  (prog1
      (when (and (streamp $pstream)
		 )
	        (p "showpage")
		(close  $pstream))
    (setq $pstream nil)))
	
(defun ps-fixup-points(lis)
  (assert ($listp lis))
  (setq lis (cdr lis))
  (cond ((numberp (car lis)))
	((and ($listp (car lis)) (numberp (nth 1 (car lis))))
	 (setq lis (sloop for w in lis collect
			  (nth 1 w)
			  collect (nth 2 w))))
	(t (error
	    "pscurve:Neither [x0,y0,x1,y1,...] nor [[x0,y0],[x1,y1],...]")))
  lis)

(defun $psdraw_curve (lis &aux (n 0))
  (declare (fixnum n))
  (setq lis (ps-fixup-points lis))
  (p "newpath" (nth 0 lis) (nth 1 lis) "moveto")
  (sloop while lis with second
     do
     (cond ((eq (car lis) 'moveto)
	    (or (eql n 0) (p "stroke"))
	    (setq n 0) (setq lis (cddr lis))))
     (or (setq second (cadr lis)) (error "odd length list of points"))
     (cond ((eql n 0)
	    (p (car lis) second "moveto"))
	   (t  (p (car lis) second "lineto")
	       ))
     (setq n (+ n 1))
     (cond ((eql 0 (mod n 20))
	    (p "stroke")
	    (setq n 0))
	   (t (setq lis (cddr lis)))))
  (or (eql n 0) (p "stroke")))

(defvar $viewps_command  "(ghostview  ~a)")

(defvar $viewps_command  "(gs -I. -Q  ~a)")

(defvar  $viewps_command "echo /def /show {pop} def |  cat - ~a | x11ps")

;; If your gs has custom features for understanding mouse clicks


;;Your gs will loop for ever if you don't have showpage at the end of it!!
(defvar $viewps_command   "echo '/showpage { .copypage readmouseclick /ke exch def ke 1 eq { erasepage initgraphics} {ke 5 ne {quit} if} ifelse} def  {(~a) run } loop' | gs  -title 'Maxima  (click left to exit,middle to redraw)' > /dev/null 2>/dev/null &")

	;; allow this to be set in a system init file (sys-init.lsp)


(defun $viewps ( &optional file)
  (cond  ((and (streamp $pstream))
	  ($pscom "showpage")
	  (force-output $pstream)))
  (cond (file (setq file (maxima-string file)))
	(t(setq file "maxout.ps")
	 
	 (if (and (streamp $pstream))
	     (force-output $pstream))))
  (if (equal $viewps_command "(gs -I. -Q  ~a)")
        (format t "~%type `quit' to exit back to affine or maxima
  To reprint a page do
  GS>showpage
  GS>(maxout.ps)run
  GS> -150 -150 translate 1.2 1.2 scale (maxout.ps)run
  would print it moved 150/72 inches to left, and down, and scaled by 1.2 times
  showpage clears scaling."))

  ($system    (format nil $viewps_command file)))

(defun $chkpt (a)
  (or (and ($listp a)
       (numberp (nth 1 a))
       (numberp (nth 2 a)))
      (error "illegal pt ~a" a))
  t)
       
(defvar $pslineno nil)
(defun $psdrawline (a &optional b c d)
  (cond ((null b)
	 (assert (and ($listp a)
		      ($chkpt (nth 1 a))))
	 (setq b (nth 2 a))
	 (setq a (nth 1 a))))
  (cond ((null c)
	 ($chkpt b)
	 (setq c (nth 1 b))
	 (setq d (nth 2 b))))
  (cond (($listp a)
	 (setq b (nth 2 a) a (nth 1 a))))
  (cond ((null c)
	 (setq c (nth 1 b))
	 (setq c (nth 2 b))))

  (when $pslineno
      (incf $pslineno)
      ($pslabelline a b c d $pslineno))
      
  (p a b "moveto")
  (p c d "lineto")
  (p "stroke"))

(defun $pslabelline (a b c d $pslineno)
   (p (/ (+ a c) 2)
      (/ (+ b d) 2)
      "moveto"
      (format nil "(<--L~a)show" $pslineno)))

(defun $sort_polys (lis)
  (let ((tem
	 (sloop for v in (cdr lis)
		collect (cons
			 (sloop for w in (cdr v)
				maximize (nth 3 w))
			 v))))
    (print 'next)
    (cons '(mlist) (mapcar 'cdr (sortcar tem '<)))))

(defun $drawpoly (x)
  (p "gsave")
  (p (cdadr x) "moveto")
  (setq x (cddr x))
  (sloop for edge in x
	 do (p (cdr edge) "lineto")
	 finally (p "1 setgray fill"))
  ($psdrawline x)
  (p "grestore"))

(defun $psdrawpolys (polys)
  (dolist (v (cdr polys))
	  ($drawpoly v)))
      
;; draw the axes through $psdef
(defun $psaxes (leng &optional (lengy leng))
  (p "gsave axiswidth setlinewidth")
  (let (begx begy endx endy)
    (cond ((numberp leng)
	   (setq begx (- leng))
	   (setq endx leng))
	  (t (setq begx (nth 1 leng))
	     (setq endx (nth 2 leng))))
    (cond ((numberp lengy)
	   (setq begy (- lengy))
	   (setq endy lengy))
	  (t (setq begy (nth 1 lengy))
	     (setq endy (nth 2 lengy))))

    (sloop for i from (floor begx) below (ceiling endx)
	   do 
	   ($psdrawline i 0 (+ i 1) 0)
	   (p i 0 "drawtick")
	   (p (+ i 1) 0 "drawtick"))

    (sloop for i from (floor begy) below (ceiling endy)
	   do
	   ($psdrawline 0 i 0 (+ i 1) )
	   (p 0 i "drawtick")
	   (p 0 (+ i 1)  "drawtick"))

  
    (p "grestore")))

(defun $psdraw_points(lis)
    (assert (and ($listp lis)
		 ($listp (cadr lis))))
    (sloop for w in (cdr lis)
	   do (p (nth 1 w)
		 (nth 2 w)
		 "drawdot")))


(defun $view_zic ()
  (let ((izdir (getenv "IZICDIR")))
    (or (probe-file
	 (format nil "~a/tcl-files/maxima.tcl" izdir))
	(error
	 "could not find file ~a :  Set environment variable IZICDIR" izdir))
    ($system "izic -interface ${IZICDIR}/tcl-files/maxima.tcl  1> /dev/null &")))

(defun $isend (x)
  ($system (format nil " izic -app izic -cmd '{~a}'" (string-trim '(#\&) (string x)))))



(defvar *some-colours*
  ;; from rgb.txt
  '(135 206 250		LightSkyBlue
	70 130 180		SteelBlue
	205  92  92		IndianRed
	178  34  34		firebrick
	176  48  96		maroon
	221 160 221		plum
	238 130 238		violet))


;; one of $zic, $geomview, $ps

(defun plot-zic-colors (&aux (ncolors  (/ (length *some-colours*) 4))) 
  (format $pstream "couleurs ~% ~a ~% " ncolors  )
  (sloop for v in *some-colours*
     with do-ind = t with ind = -1 
     do
     (cond (do-ind
	    (format $pstream "~a " (incf ind))
	    (setq do-ind nil)
	    ))
     (cond ((Numberp v)
	    (print-pt (/ v 256.0)))
	   (t 
	    (setq do-ind t)
	    (format $pstream "~%# ~(~a~)~%" v))))
  (format $pstream " 1
0
~a 0.801 0.359 0.359 
128 .8 1 0
" ncolors))

(defun check-range (range &aux ($numer t) tem a b)
  (or (and ($listp range)
	   (setq tem (cdr range))
	   (symbolp (car tem))
	   (numberp (setq a (meval* (second tem))))
	   (numberp (setq b (meval* (third tem))))
	   (< a b)
	   )
      (merror "Bad Range ~%~M must be of the form [variable,min,max]" range))
  `((mlist) ,(car tem) ,(float a) ,(float b)))

(defun $zero_fun (x y) x y 0.0)

(defun output-points (pl &optional m)
  "If m is supplied print blank line every m lines"
  (let ((j -1))
    (declare (fixnum j))
  (sloop for i below (length (polygon-pts pl))
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

(defvar $show_openplot t)
(defun show-open-plot (ans)
  (cond ($show_openplot
	 (with-open-file (st1 "maxout.openmath" :direction :output)
			(princ  ans st1))
	 ($system (maxima-bin-search $openmath_plot_command) " maxout.openmath" ))
	(t (princ ans) "")))

(defun $plot3d ( fun &optional (xrange ($get_plot_option '$x))
		     (yrange ($get_plot_option '|$y|) y-supplied)
		     &rest options 
		     &aux lvars trans *original-points*
		     ($plot_options $plot_options)
		     ($in_netmath $in_netmath)
		     colour-z grid
		     plot-format
		     )
  (declare (special *original-points*))
  (cond (options
	 (dolist (v options)
	   ($set_plot_option v))))
  (setq plot-format  ($get_plot_option '$plot_format 2))
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
  (setq colour-z  ($get_plot_option '$colour_z 2))
  (setq lvars `((mlist),(nth 1 xrange) ,(nth 1 yrange)))
  (cond (($listp fun)
	 (or (eql 3 ($length fun)) (merror "List ~M is not of length 3" fun))
	 (setq trans ($make_transform
		      ($append lvars '((mlist) $z))
		      (nth 1 fun)
		      (nth 2 fun)
		      (nth 3 fun)))
	 (setq fun '$zero_fun))
	(t (setq fun (coerce-float-fun fun))))
    (let* ((pl (draw3d fun
		       (nth 2 xrange)
		       (nth 3 xrange)
		       (nth 2 yrange)
		       (nth 3 yrange)
		       (nth 2 grid)
		       (nth 3 grid)))
	   (ar (polygon-pts pl)) tem
	   )
      (declare (type (array long-float) ar))

      (if trans  (mfuncall trans ar))
      (if (setq tem  ($get_plot_option '$transform_xy 2)) (mfuncall tem ar))
      ;; compute bounding box.
  (let (($pstream
	 (cond ($in_netmath *standard-output*)
	       (t (open (format nil "maxout.~(~a~)" (stripdollar plot-format))
			:direction :output)))))
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
	   (output-points pl (nth 2 grid)))
	($openmath
	 (progn
	   (format $pstream "{plot3d {matrix_mesh ~%")
	   ;; we do the x y z  separately:
	   (sloop for off from 0 to 2
		  with ar = (polygon-pts pl)
		  with  i = 0
		  declare (fixnum i)
		  do (setq i off)
		  (format $pstream "~%{")
		  (sloop 
		   while (< i (length ar))
		   do (format $pstream "~% {")
		   (sloop for j to (nth 2 grid)
			 do (print-pt (aref ar i))
			 (setq i (+ i 3)))
		   (format $pstream "} ")
		   )
		  (format $pstream "} ")
		  )
	   (format $pstream "}}"))
	 #+old
	 (progn ; orig
	   (print (list 'grid grid))
	   (format $pstream "{plot3d {variable_grid ~%")
	   (let* ((ar (polygon-pts pl))
		  (x-coords
		   (sloop for i to (nth 2 grid)
			  collect (aref ar (* i 3))))
		  (y-coords
		   (sloop for i to (nth 3 grid)
			  with m = (* 3 (+ 1 (nth 2 grid)))
			  collect (aref ar (+ 1 (* i m)))))
		  (z  (sloop for i to (nth 3 grid)
			     with k = 2
			     declare (fixnum k)
			     collect
			     (sloop for j to (nth 2 grid)
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
	 (output-points pl nil))
	($ps
	 (let ((rot ($get_rotation ($rest ($get_plot_option '$view_direction)))))
	   (cond (colour-z
		  (setq *original-points* (copy-seq (polygon-pts pl)))
		  (setq *z-range* ($get_range ar 2))))
	   ($rotate_pts ar rot)
	   (setup-for-ps-range ($get_range ar 0) ($get_range ar 1) t)
	   (sort-ngons (polygon-pts pl) (polygon-edges pl) 4 )
           ;(print (polygon-edges pl))
	   ($ps_axes rot)
	   ($draw_ngons (polygon-pts pl) (polygon-edges pl) 4 )
	   (p "[.1] 0 setdash")    ($ps_axes rot)
	   (p "[] 0 setdash") (p " showpage "))))
      ;; close the stream and plot..
      (cond ($in_netmath (return-from $plot3d ""))
	    (t (close $pstream)
	       (setq $pstream nil)
	       ))
      )
      (cond (($get_plot_option '$run_viewer 2)
	     (case plot-format
	       ($zic ($view_zic))
	       ($ps ($viewps))
	       ($openmath
		($system (maxima-bin-search $openmath_plot_command) " maxout.openmath")
		)
	       ($geomview ($system $geomview_command))
	       ($gnuplot ($system (maxima-bin-search $gnuplot_command)
				  " -parametric3d maxout.gnuplot" ))
	       )))
      )))
  


(setf (symbol-function '*$) (symbol-function '*))
(setf (symbol-function '+$) (symbol-function '+))
(setf (symbol-function '-$) (symbol-function '-))
(setf (symbol-function '/$) (symbol-function '/))

#|
Here is what the user inputs to draw the lattice picture.

/*Initially 1 unit = 1 pt = 1/72 inch
This makes 1 unit be 50/72 inch
*/
ps_scale:[50,50];

/*This moves the origin to 400/72 inches up and over from bottom left corner
[ie roughly center of page]
*/
ps_translate:[8,8];


f(x):=if (x = 0) then 100  else 1/x;

foo():=block([],
closeps(),
ps_translate:[6,6],
ps_scale:[50,50],
psdraw_curve(join(xcord,map(f,xcord))),
psdraw_curve(join(-xcord,map(f,xcord))),
psdraw_curve(join(xcord,-map(f,xcord))),
psdraw_curve(join(-xcord,-map(f,xcord))),
psdraw_points(lattice),
psaxes(8));


And here is the output .ps file which you should be
able to print on a laserwriter, or view on screen if you have
ghostscript (or another postscript screen previewer).

|#

#|
Examples

/* plot of z^(1/3)...*/
plot3d(r^.33*cos(th/3),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['view_direction,1,1,1.4],['colour_z,true],['plot_format,zic]);

/* plot of z^(1/2)...*/
plot3d(r^.5*cos(th/2),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['view_direction,1,1,1.4],['colour_z,true],['plot_format,zic]);

/* moebius */
plot3d([cos(x)*(3+y*cos(x/2)),sin(x)*(3+y*cos(x/2)),y*sin(x/2)],[x,-%pi,%pi],[y,-1,1],['grid,50,15]);

/* klein bottle */
plot3d([5*cos(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0) - 10.0,
          -5*sin(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0),
           5*(-sin(x/2)*cos(y)+cos(x/2)*sin(2*y))],[x,-%pi,%pi],[y,-%pi,%pi],
          ['grid,40,40]);
/* torus */
plot3d([cos(y)*(10.0+6*cos(x)),
           sin(y)*(10.0+6*cos(x)),
           -6*sin(x)], [x,0,2*%pi],[y,0,2*%pi],['grid,40,40]);
|#

