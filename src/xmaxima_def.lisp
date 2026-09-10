;; xmaxima.lisp: routines for Maxima's interface to xmaxima
;; Copyright (C) 2007-2021 J. Villate
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA  02110-1301, USA

(in-package :maxima)

;; Given a list of valid colors (see rgb-color function) and an object c
;; that can be a real number or a string, produces an rgb color
;; specification for c; when c is real, its nearest integer is assigned
;; to one of the numbers in the list, using modulo length of the list.
(defun xmaxima-color (colors c)
  (unless (listp colors) (setq colors (list colors)))
  (when (realp c)
    (unless (integerp c) (setq c (round c)))
    (setq c (nth (mod (1- c) (length colors)) colors)))
  (rgb-color c))

;; style is a list starting with a symbol from the list: points, lines,
;; linespoints or dots,
;; The meaning of the numbers that follow the symbol are:
;;
;;   lines, linewidth, color
;;   points, radius, color
;;   linespoints, linewidth, radius, color
;;   dots, color
;;
;; linewidth and radius are measured in the same units and can be
;; floating-point numbers.
;;
;; type must be an integer
;; color can be an integer, used as index to get one of the colors defined
;; by the color option, or a 6-digit hexadecimal number #rrggbb

(defun xmaxima-curve-style (style colors i)
  (unless (listp style) (setq style (list style)))
  (unless (listp colors) (setq colors (list colors)))
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "\{ nolines 1 \} \{ plotpoints 1 \} \{ pointsize 0.7 \}")
       (if (second style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (second style)))
         (format st " \{ color ~a \}" (xmaxima-color colors i))))
      ($lines
       (format st "\{ nolines 0 \} \{ plotpoints 0 \}")
       (if (realp (second style))
	 (format st " \{ linewidth ~f \}" (second style)))
       (if (third style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-color colors i))))
      ($points
       (format st "\{ nolines 1 \} \{ plotpoints 1 \}")
       (if (realp (second style))
	 (format st " \{ pointsize ~f \}" (second style))
	 (format st " \{ pointsize 3 \}"))
       (if (third style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-color colors i))))
      ($linespoints
       (format st "\{ nolines 0 \} \{ plotpoints 1 \}")
       (if (realp (second style))
	 (format st " \{ linewidth ~f \}" (second style)))
       (if (realp (third style))
	 (format st " \{ pointsize ~f \}" (third style))
	 (format st " \{ pointsize 3 \}"))
       (if (fourth style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (fourth style)))
	 (format st " \{ color ~a \}" (xmaxima-color colors i))))
      (t
       (format st "\{ nolines 0 \} \{ plotpoints 0 \} \{ color ~a \}"
               (xmaxima-color colors i))))))

(defun xmaxima-palette (palette)
"Given a valid palette, it returns its definition for Xmaxima."
(let (type colors fun (fun-format " {hue ~,,,,,,'eg} {saturation ~,,,,,,'eg} {value ~,,,,,,'eg} {colorrange ~,,,,,,'eg}"))
  (if (atom palette)
    (if (getf *plot-palettes* palette)
      (progn
        (setq type palette)
        (setf colors (cdr (getf *plot-palettes* palette))))
      (progn
        (setq type '$monochromatic)
        (unless (setf colors (rgb-color palette))
          (setf colors "#ffffff"))))
    (progn
      (setq type (car palette))
      (case type
            ($gradient (setf colors (cdr palette)))
            (($hue $saturation $value)
             (setq fun (format nil fun-format (second palette) (third palette)
                               (fourth palette) (fifth palette))))
            (otherwise (setf colors palette)))))
  (with-output-to-string
    (st)
    (case type
          ($monochromatic (format st "~&{colorscheme ~a}" colors))
          ($hue (format st "~&~a {colorscheme hue}" fun))
          ($saturation (format st "~&~a {colorscheme saturation}" fun))
          ($value (format st "~&~a {colorscheme value}" fun))
          (($gray $grey) (format st "~&{colorscheme gray}"))
          (otherwise
           (setf colors (mapcar #'rgb-color colors))
           (format st "~&{colorscheme gradient} {gradlist {~{~s~^ ~}}}"
                   colors))))))

(defmethod plot-preamble ((plot xmaxima-plot) options)
  (let (outfile zmin zmax)
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)            
        (cond ($show_openplot
               (format st "~a " (getf options '$type))
               (format st "-background ~a -data {~%"
                       (rgb-color (getf options '$background_color)))
               ;; record Maxima version and date-time
               (format st "# Created by Maxima ~a~%" *autoconf-version*)
               (format st "# ~a~%" ($timedate)))
              (t (format st "{~a {background ~a} "
                         (getf options '$type)
                         (rgb-color (getf options '$background_color)))))
        (when (string= (getf options '$type) "plot3d")
          (let ((elev (getf options '$elevation))
                (azim (getf options '$azimuth)))
            (when elev (format st " {el ~d}" elev))
            (when azim (format st " {az ~d}" azim))
            (format st "~%")))
        (when (getf options '$ps_file)
          (setq outfile (plot-file-path (getf options '$ps_file) t))
          (format st " {psfile ~s}" outfile))
        (when (member '$legend options)
          (unless (getf options '$legend)
            (format st " {nolegend 1}")))
        (when (member 'mbox options)
          (unless (getf options 'mbox)
            (format st " {nobox 1}")))
        (if (getf options '$axes)
            (case (getf options '$axes)
              ($x (format st " {axes {x} }"))
              ($y (format st " {axes {y} }"))
              (t (format st " {axes {xy} }")))
            (format st " {axes 0}"))
        (when (getf options '$x)
          (format st " {xrange ~{~,,,,,,'eg~^ ~}}" (getf options '$x)))
        (when (getf options '$y)
          (format st " {yrange ~{~,,,,,,'eg~^ ~}}" (getf options '$y)))
        (when (getf options '$z)
          (setq zmin (first (getf options '$z)))
          (setq zmax (second (getf options '$z)))
          (format st " {zcenter ~,,,,,,'eg }" (/ (+ zmax zmin) 2.0))
          (format st " {zradius ~,,,,,,'eg }" (/ (- zmax zmin) 2.0)))
        (when (getf options '$xlabel)
          (format st " {xaxislabel ~s}" (getf options '$xlabel)))
        (when (getf options '$ylabel)
          (format st " {yaxislabel ~s}" (getf options '$ylabel)))
        (when (getf options '$z)
          (format st " {zcenter ~,,,,,,'eg }"
                  (/ (apply #'+ (getf options '$z)) 2))
          (format st " {zradius ~,,,,,,'eg }~%"
                  (/ (apply #'- (getf options '$z)) -2)))
        (format st "~%"))))
    ;;returns a list with the name of the file to be created, or nil
    (if (null outfile) nil (list outfile))))

(defmethod plot2d-command ((plot xmaxima-plot) fun options range)
  (let (points-lists)
    (setq points-lists
          (mapcar #'(lambda (f) (cdr (draw2d f range options))) (cdr fun)))
    (when (= (count-if #'(lambda (x) x) points-lists) 0)
      (merror (intl:gettext "plot2d: nothing to plot.~%")))
    (let ((legends-new) (legends (getf options '$legend)))
      (unless (null legends)
        (dotimes (i (length legends))
          (unless (null (cdr (nth i points-lists)))
            (push (nth i legends) legends-new)))
        (setf (getf options '$legend) (reverse legends-new))))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)            
        (unless (or (getf options '$logy)
                    (and (getf options '$y) (listp (getf options '$y))))
          (let (x y ymin ymax (xmin +most-negative-flonum+)
                  (xmax +most-positive-flonum+))
            (when (getf options '$x)
              (setq xmin (first (getf options '$x)))
              (setq xmax (second (getf options '$x))))
            (dolist (points-list points-lists)
              (dotimes (i (/ (length points-list) 2))
                (setq x (nth (* i 2) points-list))
                (setq y (nth (1+ (* i 2)) points-list))
                (when (and (numberp x) (>= x xmin) (<= x xmax))
                  (when (numberp y)
                    (if (numberp ymin)
                        (if (numberp ymax)
                            (progn
                              (when (< y ymin) (setq ymin y))
                              (when (> y ymax) (setq ymax y)))
                          (if (< y ymin)
                              (setq ymax ymin ymin y)
                            (setq ymax y)))
                      (if (numberp ymax)
                          (if (> y ymax)
                              (setq ymin ymax ymax y)
                            (setq ymin y))
                        (setq ymin y)))))))
            (when (and (numberp ymin) (numberp ymax) (< ymin ymax))
              (psetq ymin (- (* 1.05 ymin) (* 0.05 ymax))
                     ymax (- (* 1.05 ymax) (* 0.05 ymin)))
              (format st " {yrange ~,,,,,,'eg ~,,,,,,'eg}~%" ymin ymax))))
        (let ((legend (getf options '$legend))
              (colors (getf options '$color))
              (styles (getf options '$style)) (i 0) j style plot-name)
          (unless (listp legend) (setq legend (list legend)))
          (unless (listp colors) (setq colors (list colors)))
          (unless (listp styles) (setq styles (list styles)))
          (loop for v in (cdr fun) for points-list in points-lists do
               (when points-list
                 ;; case "contour" with several plots in one list
                 (when ($listp (car points-list))
                   (setq j 0)
                   (dolist (level (cdar points-list))
                     (if styles
                         (setq style (nth (mod i (length styles)) styles))
                         (setq style nil))
                     (when ($listp style) (setq style (cdr style)))
                     (incf i)
                     (incf j)
                     (format st " {label ~s} " (ensure-string level))
                     (format st (xmaxima-curve-style style colors i))
                     (format st "~%{xversusy~%")
                     (let ((lis (cdr (nth j points-list))))
                       (loop while lis do
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
                     (format st "}"))
                   (return))
                 ;; other cases with only one plot per list
                 (if styles
                     (setq style (nth (mod i (length styles)) styles))
                     (setq style nil))
                 (when ($listp style) (setq style (cdr style)))
                 (incf i)
                 ;; label the expression according to the legend,
                 ;; unless it is "false" or there is only one expression
                 (if (member '$legend options)
                     (setq plot-name
                           (if (first legend)
                               (ensure-string
                                (nth (mod (- i 1) (length legend)) legend)) nil))
                     (if (= 2 (length fun))
                         (setq plot-name nil)
                         (progn 
                           (setq
                            plot-name
                            (with-output-to-string (pn)
                              (cond ((atom v) (format pn "~a" ($sconcat v)))
                                    ((eq (second v) '$parametric)
                                     (format pn "~a, ~a"
                                             ($sconcat (third v))
                                             ($sconcat (fourth v))))
                                    ((eq (second v) '$discrete)
                                     (format pn "discrete~a" i))
                                    (t (format pn "~a" ($sconcat v))))))
                           (when (> (length plot-name) 50)
                             (setq plot-name (format nil "fun~a" i))))))
                 (if plot-name 
                     (format st " {label ~s} " plot-name)
                     (format st " {nolegend 1} "))
                 (format st (xmaxima-curve-style style colors i))
                 (format st "~%{xversusy~%")
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
                 (format st "}")))
          (format st "} ")))))))

(defmethod plot3d-command ((plot xmaxima-plot) functions options titles)
  (declare (ignore titles))
  (let ((i 0) fun xrange yrange lvars trans)
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)
        ;; generate the mesh points for each surface in the functions stack
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
                ;; Evaluate FUN at the middle point of the range.
                ;; Looking at a single point is somewhat unreliable.
                ;; Call FUN with numerical arguments (symbolic arguments may
                ;; fail due to trouble computing real/imaginary parts for 
                ;; complicated expressions, or it may be a numerical function)
                (when (cdr ($listofvars (mfuncall fun xmid ymid)))
                  (mtell
                   (intl:gettext
                    "plot3d: expected <expr. of v1 and v2>, [v1,min,max], [v2,min,max]~%"))
                  (mtell
                   (intl:gettext
                    "plot3d: keep going and hope for the best.~%")))))
          (let* ((points
                  (draw3d
                   fun (third xrange) (fourth xrange) (third yrange)
                   (fourth yrange) (first (getf options '$grid))
                   (second (getf options '$grid))))
                 (palette (getf options '$palette))
                 (meshcolor (if (member '$mesh_lines_color options)
                              (getf options '$mesh_lines_color)
                              '$black)))
            (declare (type (cl:array t) points))
            (when trans (mfuncall trans points))
            (when (getf options '$transform_xy)
                (mfuncall (getf options '$transform_xy) points))
            (if meshcolor
              (format st " {mesh_lines ~a}" (rgb-color meshcolor))
              (format st " {mesh_lines 0}"))
            (if palette
              (format st " ~a" (xmaxima-palette palette))
              (format st " {colorscheme 0}"))
            (output-points-tcl st points (first (getf options '$grid)))))
        (format st "}~%"))))))

(defmethod plot-shipout ((plot xmaxima-plot) options &optional output-file)
  (declare (ignore options))
  (let ((file (plot-file-path (format nil "~a.xmaxima" (random-name 16)))))
    (cond ($show_openplot
           (with-open-file (fl
                            #+sbcl (sb-ext:native-namestring file)
                            #-sbcl file
                            :direction :output :if-exists :supersede)
                           (princ (slot-value plot 'data) fl))
           ($system $xmaxima_plot_command (format nil $gnuplot_file_args file)))
          (t (princ (slot-value plot 'data)) ""))
    (cons '(mlist) (cons file output-file))))

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
                (format *standard-output* "~,,,,,,'eg " (nth i v))))
        (t
         (setq lis (nthcdr i lis))
         (loop  with v = lis  while v
                 do
                 (format *standard-output* "~,,,,,,'eg " (car v))
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
                (format st "~,,,,,,'eg " v))
         (format st  " }~%"))
        (t (tcl-output-list st (car lis))
           (tcl-output-list st (cdr lis)))))

(defun output-points-tcl (st ar m)
"Outputs the 3 coordinates of a grid of points.
If the grid option is [grid,m,n], the points form a matrix with n+1 rows
(x dirention) and m+1 columns (ydirection), stored in the array 'points'
inside de structure st, one row after another.
The points coordinates are written into 3 Tcl lists, each one with n+1
sublists with the coordinates of the m+1 points on each row. The first list
has the x coordinates, the second one the y coordinates and the third one
has the z coordinates."
  (format st "~%{matrix_mesh")
  ;; x y z are done separately:
  (loop for off from 0 to 2
     with  i of-type fixnum = 0
     do (setq i off)
       (format st "~%{")
       (loop while (< i (length ar))
	     do (format st "~%{")
	     (loop for j to m
	           do (if (floatp (aref ar i))
                        (format st "~,,,,,,'eg " (aref ar i))
                        (format st "~a " *missing-data-indicator*))
		   (setq i (+ i 3)))
	     (format st "}"))
       (format st "~%}"))
  (format st "~%}"))
