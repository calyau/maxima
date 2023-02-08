;; xmaxima.lisp: routines for Maxima's interface to xmaxima
;; Copyright (C) 2007-2021 J. Villate
;; Time-stamp: "2021-06-14 17:27:42 villate"
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
;; palette should be a list starting with one of the symbols: hue,
;; saturation, value, gray or gradient.
;;
;; If the symbol is gray, it should be followed by two floating point
;; numbers that indicate the initial gray level and the interval of 
;; gray values.
;;
;; If the symbol is one of hue, saturation or value, it must be followed
;; by three numbers that specify the hue, saturation and value for the
;; initial color, and a fourth number that gives the range of values for
;; the increment of hue, saturation or value.
;; The values for the initial hue, saturation, value and grayness should
;; be within 0 and 1, while the range can be higher or even negative.
;;
;; If the symbol is gradient, it must be followed by either a list of valid
;; colors or by a list of lists with two elements, a number and a valid color.

  (unless (listp palette) (setq palette (list palette)))
  (let (hue sat val gray range fun)
    (case (first palette)
      ($gray
       (case (length (rest palette))
         (2 (setq gray (second palette)) (setq range (third palette)))
         (t (merror
             (intl:gettext
              "palette: gray must be followed by two numbers."))))
       (when (or (< gray 0) (> gray 1))
         (setq gray (- gray (floor gray))))
       (setq fun (format nil "{value ~,,,,,,'eg} {colorrange ~,,,,,,'eg}"
                         gray range)))
      (($hue $saturation $value)
       (case (length (rest palette))
         (4 (setq hue (second palette))
            (setq sat (third palette))
            (setq val (fourth palette))
            (setq range (fifth palette)))
         (t (merror
             (intl:gettext
              "palette: ~M must be followed by four numbers.")
              (first palette))))
       (when (or (< hue 0) (> hue 1)) (setq hue (- hue (floor hue))))
       (when (or (< sat 0) (> sat 1)) (setq sat (- sat (floor sat))))
       (when (or (< val 0) (> val 1)) (setq val (- val (floor val))))
       (setq fun
             (format nil " {hue ~,,,,,,'eg} {saturation ~,,,,,,'eg} {value ~,,,,,,'eg} {colorrange ~,,,,,,'eg}"
                     hue sat val range))))
    (with-output-to-string (st)
      (case (first palette)
        ($hue (format st "~&~a {colorscheme hue}" fun))
        ($saturation (format st "~&~a {colorscheme saturation}" fun))
        ($value (format st "~&~a {colorscheme value}" fun))
        ($gray (format st "~&~a {colorscheme gray}" fun))
        ($gradient
         (let* ((colors (rest palette)) (n (length colors)) (map nil))
           ;; map is constructed as (n1 c1 n2 c2 ... nj cj) where ni is a
           ;; decreasing sequence of numbers (n1=1, nj=0) and ci are colors
           (cond
             ;; Maxima list of numbers and colors (((mlist) ni ci) ...)
             ((listp (first colors))
              (setq colors (sort colors #'< :key #'cadr))
              (dotimes (i n)
                (setq map (cons (rgb-color (third (nth i colors))) ;; color
                                (cons
                                 (/ (- (second (nth i colors))   ;; ni minus
                                       (second (first colors)))  ;; smallest ni
                                    (- (second (nth (- n 1) colors));; biggest
                                       (second (first colors)))) ;; - smallest
                                 map)))))
             ;; list of only colors
             (t (dotimes (i n)
                  (setq map (cons (rgb-color (nth i colors))  ;; color i
                                  (cons (/ i (1- n)) map))))))    ;; number i

           ;; prints map with the format:  nj, "cj", ...,n1, "c1"  
           (setq fun (format nil "~{{ ~,,,,,,'eg ~s}~^ ~}" (reverse map)))
           (format st "~&{colorscheme gradient} ")
           ;; writes: {gradlist {{nj "cj"} ...{n1 "c1"}}}
           (format st "{gradlist {~a}}" fun)))
        (t
         (merror
          (intl:gettext
           "palette: wrong keyword ~M. Must be hue, saturation, value, gray or gradient.")
          (first palette)))))))

(defun xmaxima-palettes (palette n)
  (unless (integerp n) (setq n (round n)))
  (if (find 'mlist palette :key #'car) (setq palette (list palette)))
  (xmaxima-palette (rest (nth (mod (- n 1) (length palette)) palette))))
			 
(defmethod plot-preamble ((plot xmaxima-plot) plot-options)
  (let (outfile zmin zmax)
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (dest)            
        (cond ($show_openplot
               (format dest "~a -data {~%" (getf plot-options :type)))
              (t (format dest "{~a " (getf plot-options :type))))
        (when (string= (getf plot-options :type) "plot3d")
          (let ((palette (getf plot-options :palette))
                (meshcolor (if (member :mesh_lines_color plot-options)
                               (getf plot-options :mesh_lines_color)
                               '$black))
                (elev (getf plot-options :elevation))
                (azim (getf plot-options :azimuth)))
            (if (find 'mlist palette :key #'car) (setq palette (list palette)))
            (if palette
                (progn
                  (if meshcolor
                      (format dest " {mesh_lines ~a}" (rgb-color meshcolor))
                      (format dest " {mesh_lines 0}")))
                (format dest " {colorscheme 0}~%"))
            (when elev (format dest " {el ~d}" elev))
            (when azim (format dest " {az ~d}" azim))
            (format dest "~%")))
        (when (getf plot-options :ps_file)
          (setq outfile (plot-file-path (getf plot-options :ps_file) t))
          (format dest " {psfile ~s}" outfile))
        (when (member :legend plot-options)
          (unless (getf plot-options :legend)
            (format dest " {nolegend 1}")))
        (when (member :box plot-options)
          (unless (getf plot-options :box)
            (format dest " {nobox 1}")))
        (if (getf plot-options :axes)
            (case (getf plot-options :axes)
              ($x (format dest " {axes {x} }"))
              ($y (format dest " {axes {y} }"))
              (t (format dest " {axes {xy} }")))
            (format dest " {axes 0}"))
        (when (getf plot-options :x)
          (format dest " {xrange ~{~,,,,,,'eg~^ ~}}" (getf plot-options :x)))
        (when (getf plot-options :y)
          (format dest " {yrange ~{~,,,,,,'eg~^ ~}}" (getf plot-options :y)))
        (when (getf plot-options :z)
          (setq zmin (first (getf plot-options :z)))
          (setq zmax (second (getf plot-options :z)))
          (format dest " {zcenter ~,,,,,,'eg }" (/ (+ zmax zmin) 2.0))
          (format dest " {zradius ~,,,,,,'eg }" (/ (- zmax zmin) 2.0)))
        (when (getf plot-options :xlabel)
          (format dest " {xaxislabel ~s}" (getf plot-options :xlabel)))
        (when (getf plot-options :ylabel)
          (format dest " {yaxislabel ~s}" (getf plot-options :ylabel)))
        (when (getf plot-options :z)
          (format $pstream " {zcenter ~,,,,,,'eg }"
                  (/ (apply #'+ (getf plot-options :z)) 2))
          (format $pstream " {zradius ~,,,,,,'eg }~%"
                  (/ (apply #'- (getf plot-options :z)) -2)))
        (format dest "~%"))))
    ;;returns a list with the name of the file to be created, or nil
    (if (null outfile) nil (list outfile))))

(defmethod plot2d-command ((plot xmaxima-plot) fun options range)
  (let (points-lists)
    (setq points-lists
          (mapcar #'(lambda (f) (cdr (draw2d f range options))) (cdr fun)))
    (when (= (count-if #'(lambda (x) x) points-lists) 0)
      (merror (intl:gettext "plot2d: nothing to plot.~%")))
    (let ((legends-new) (legends (getf options :legend)))
      (unless (null legends)
        (dotimes (i (length legends))
          (unless (null (cdr (nth i points-lists)))
            (push (nth i legends) legends-new)))
        (setf (getf options :legend) (reverse legends-new))))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)            
        (unless (or (getf options :logy)
                    (and (getf options :y) (listp (getf options :y))))
          (let (y ymin ymax)
            (dolist (points-list points-lists)
              (dotimes (i (/ (length points-list) 2))
                (setq y (nth (1+ (* i 2)) points-list))
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
                          (setq ymin y))))))
            (when (and (numberp ymin) (numberp ymax) (< ymin ymax))
              (psetq ymin (- (* 1.05 ymin) (* 0.05 ymax))
                     ymax (- (* 1.05 ymax) (* 0.05 ymin)))
              (format st " {yrange ~,,,,,,'eg ~,,,,,,'eg}~%" ymin ymax))))
        (let ((legend (getf options :legend))
              (colors (getf options :color))
              (styles (getf options :style)) (i 0) j style plot-name)
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
                 (if (member :legend options)
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
  (let ((i 0) fun xrange yrange lvars trans)
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string ($pstream)
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
          (let* ((pl
                  (draw3d
                   fun (third xrange) (fourth xrange) (third yrange)
                   (fourth yrange) (first (getf options :grid))
                   (second (getf options :grid))))
                 (ar (polygon-pts pl))
                 (colors (getf options :color))
                 (palettes (getf options :palette)))
            (declare (type (cl:array t) ar))
            (when trans (mfuncall trans ar))
            (when (getf options :transform_xy)
                (mfuncall (getf options :transform_xy) ar))
            (if palettes
                   (format $pstream " ~a~%" (xmaxima-palettes palettes i))
                   (format $pstream " {mesh_lines ~a}" (xmaxima-color colors i)))
              (output-points-tcl $pstream pl (first (getf options :grid)))))
        (format $pstream "}~%"))))))

(defmethod plot-shipout ((plot xmaxima-plot) options &optional output-file)
  (let ((file (plot-file-path (format nil "maxout~d.xmaxima" (getpid)))))
    (cond ($show_openplot
           (with-open-file (fl
                            #+sbcl (sb-ext:native-namestring file)
                            #-sbcl file
                            :direction :output :if-exists :supersede)
             (princ (slot-value plot 'data) fl))
           ($system (concatenate 'string *maxima-prefix* 
                                 (if (string= *autoconf-windows* "true")
                                     "\\bin\\" "/bin/") 
                                 $xmaxima_plot_command)
                    #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
                    (format nil " ~s &" file)
                    #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
                    file))
          (t (princ (slot-value plot 'data)) ""))
    (cons '(mlist) (cons file output-file))))
