;; gnuplot_def.lisp: routines for Maxima's interface to gnuplot
;; Copyright (C) 2007-2021 J. Villate
;; Time-stamp: "2022-03-28 11:45:15 villate"
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

;; Checks that color is a six-digit hexadecimal number with the prefix #,
;; or a symbol for one of the 12 pre-defined colors, in which case the
;; hexadecimal code for that color will be returned. Unknown colors are
;; converted into black.
(defun rgb-color (color)
  (if (plotcolorp color)
      (case color
	($red "#ff0000")
        ($green "#00ff00")
        ($blue "#0000ff")
	($magenta "#ff00ff")
        ($cyan "#00ffff")
        ($yellow "#ffff00")
        ($orange "#ffa500")
        ($violet "#ee82ee")
        ($brown "#a52a2a")
        ($gray "#bebebe")
        ($black "#000000")
        ($white "#ffffff")
        (t color))
      "#000000"))

;; Given a list of valid colors (see rgb-color function) and an object c
;; that can be a real number or a string, produces a gnuplot color
;; specification for c; when c is real, its nearest integer is assigned
;; to one of the numbers in the list, using modulo length of the list.
(defun gnuplot-color (colors c)
  (unless (listp colors) (setq colors (list colors)))
  (when (realp c)
    (unless (integerp c) (setq c (round c)))
    (setq c (nth (mod (1- c) (length colors)) colors)))
  (format nil "rgb ~s" (rgb-color c)))

(defun gnuplot-pointtype (type)
  (case type
    ($bullet 7) ($circle 6) ($plus 1) ($times 2) ($asterisk 3) ($box 5)
    ($square 4) ($triangle 9) ($delta 8) ($wedge 11) ($nabla 10)
    ($diamond 13) ($lozenge 12) (t 7)))

(defun gnuplot-pointtypes (types n)
  (unless (integerp n) (setq n (round n)))
  (unless (listp types) (setq types (list types)))
  (gnuplot-pointtype (nth (mod (- n 1) (length types)) types)))

;; style is a list starting with one of the symbols: points, lines,
;; linespoints or dots,
;; The meaning of the numbers that follow the symbol are:
;;
;;   lines, linewidth, color
;;   points, radius, color, pointtype
;;   linespoints, linewidth, radius, color, pointtype
;;   dots, color
;;
;; linewidth and radius are measured in the same units and can be
;; floating-point numbers.
;;
;; type must be an integer
;; color can be an integer, used as index to get one of the colors defined
;; by the color option, or a 6-digit hexadecimal number #rrggbb

(defun gnuplot-curve-style (style colors types i)
  (unless (listp style) (setq style (list style)))
  (unless (listp colors) (setq colors (list colors)))
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "with dots")
       (if (second style)
         (format st " lt ~d" (gnuplot-color colors (second style)))
         (format st " lt ~d" (gnuplot-color colors i))))
      ($impulses
       (format st "with impulses")
       (if (integerp (second style))
         (format st " lt ~d" (gnuplot-color colors (second style)))
         (format st " lt ~d" (gnuplot-color colors i))))
      ($lines
       (format st "with lines")
       (if (realp (second style))
         (format st " lw ~f" (second style)))
       (if (third style)
         (format st " lt ~d" (gnuplot-color colors (third style)))
         (format st " lt ~d" (gnuplot-color colors i))))
      ($points
       (format st "with points")
       (if (realp (second style))
         (format st " ps ~f" (/ (second style) 2))
         (format st " ps 1.5"))
       (if (third style)
         (format st " lt ~d" (gnuplot-color colors (third style)))
         (format st " lt ~d" (gnuplot-color colors i)))
       (if (integerp (fourth style))
         (format st " pt ~d" (gnuplot-pointtypes types (fourth style)))
         (format st " pt ~d" (gnuplot-pointtypes types i))))
      ($linespoints
       (format st "with linespoints")
       (if (realp (second style))
         (format st " lw ~f" (second style)))
       (if (realp (third style))
         (format st " ps ~f" (/ (third style) 2))
         (format st " ps 1.5"))
       (if (fourth style)
         (format st " lt ~d" (gnuplot-color colors (fourth style)))
         (format st " lt ~d" (gnuplot-color colors i)))
       (if (integerp (fifth style))
         (format st " pt ~d" (gnuplot-pointtypes types (fifth style)))
         (format st " pt ~d" (gnuplot-pointtypes types i))))
      (t (format st "with lines lt ~d" (gnuplot-color colors i))))))

(defun gnuplot-palette (palette)
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
         (setq gray (- gray (floor gray)))))
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
       (when (or (< val 0) (> val 1)) (setq val (- val (floor val))))))
    (with-output-to-string (st)
      (case (first palette)
        ($hue
         (if (or (< (+ hue range) 0) (> (+ hue range) 1))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               hue range hue range))
             (setq fun (format nil "~,3f+~,3f*gray" hue range)))
         (format st "model HSV functions ~a, ~,3f, ~,3f" fun sat val))
        ($saturation
         (if (or (< (+ sat range) 0) (> (+ sat range) 1))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               sat range sat range))
             (setq fun (format nil "~,3f+~,3f*gray" sat range)))
         (format st "model HSV functions ~,3f, ~a, ~,3f" hue fun val))
        ($value
         (if (or (< (+ val range) 0) (> (+ val range) 1))
             (setq fun (format nil "~,3f+~,3f*gray" val range))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               val range val range)))
         (format st "model HSV functions ~,3f, ~,3f, ~a" hue sat fun))
        ($gray
         (if (or (< (+ gray range) 0) (> (+ gray range) 1))
             (setq fun (format nil "~,3f+~,3f*gray" gray range))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               gray range gray range)))
         (format st "model RGB functions ~a, ~a, ~a" fun fun fun))

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
           (setq fun (format nil "~{~f ~s~^, ~}" (reverse map)))
           ;; outputs the string: defined (nj, "cj", ...,n1, "c1")
           (format st "defined (~a)" fun)))
        (t
         (merror
          (intl:gettext
           "palette: wrong keyword ~M. Must be hue, saturation, value, gray or gradient.")
          (first palette)))))))

(defun gnuplot-plot3d-command (file palette gstyles colors titles n) 
(let (title (style "with pm3d"))
  (with-output-to-string (out)
    (format out "splot ")
  (do ((i 1 (+ i 1))) ((> i n) (format out "~%"))
    (unless palette
      (if gstyles
	  (setq style (ensure-string (nth (mod i (length gstyles)) gstyles)))
	  (setq style
                (format nil "with lines lt ~a" (gnuplot-color colors i)))))
    (when (> i 1) (format out ", "))
    (if titles
        (setq title (nth (mod i (length titles)) titles))
        (setq title ""))
    (format out "~s title ~s ~a" file title style)))))

(defun gnuplot-terminal-and-file (plot-options)
  (let ((gstrings
         (if (getf plot-options :gnuplot_strings) "enhanced" "noenhanced"))
        (gnuplot-svg-background (getf plot-options :gnuplot_svg_background))
        terminal-command out-file (preserve-file t))
  (cond
    ((getf plot-options :svg_file)
     (if (getf plot-options :gnuplot_svg_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_svg_term_command))
         (setq terminal-command
               (format nil "set term svg font \",14\" ~a~@[ background '~a'~]" gstrings gnuplot-svg-background)))
     (setq out-file (getf plot-options :svg_file)))
    ((getf plot-options :png_file)
     (if (getf plot-options :gnuplot_png_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_png_term_command))
         (setq terminal-command
               (format nil "set term pngcairo font \",12\" ~a" gstrings)))
     (setq out-file (getf plot-options :png_file)))
    ((getf plot-options :pdf_file)
     (if (getf plot-options :gnuplot_pdf_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_pdf_term_command))
         (setq terminal-command
               (format nil "set term pdfcairo color solid lw 3 size 17.2 cm, 12.9 cm font \",18\" ~a" gstrings)))
     (setq out-file (getf plot-options :pdf_file)))
    ((getf plot-options :ps_file)
     (if (getf plot-options :gnuplot_ps_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_ps_term_command))
         (setq terminal-command
               (format nil "set term postscript eps color solid lw 2 size 16.4 cm, 12.3 cm font \",24\" ~a" gstrings)))
     (setq out-file (getf plot-options :ps_file)))
    ((eq (getf plot-options :gnuplot_term) '$ps)
     (if (getf plot-options :gnuplot_ps_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_ps_term_command))
         (setq terminal-command
               (format nil "set term postscript eps color solid lw 2 size 16.4 cm, 12.3 cm font \",24\" ~a" gstrings)))
     (if (getf plot-options :gnuplot_out_file)
         (setq out-file (getf plot-options :gnuplot_out_file))
         (setq out-file "maxplot.ps")))
    ((eq (getf plot-options :gnuplot_term) '$dumb)
     (if (getf plot-options :gnuplot_dumb_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_ps_term_command))
         (setq terminal-command "set term dumb 79 22"))
     (if (getf plot-options :gnuplot_out_file)
         (setq out-file (getf plot-options :gnuplot_out_file))
         (setq out-file "maxplot.txt")))
    ((eq (getf plot-options :gnuplot_term) '$default)
     (if (getf plot-options :gnuplot_default_term_command)
         (setq terminal-command
               (getf plot-options :gnuplot_default_term_command))
         (setq terminal-command
               (if (getf plot-options :window)
                   (format nil "set term GNUTERM ~d ~a~%"
                           (getf plot-options :window) gstrings)
                   (format nil "set term GNUTERM ~a~%" gstrings)))))
    ((getf plot-options :gnuplot_term)
     (setq
      terminal-command
          (format nil "set term ~(~a~)"
           (ensure-string (getf plot-options :gnuplot_term))))
     (if (getf plot-options :gnuplot_out_file)
         (setq out-file (getf plot-options :gnuplot_out_file))
         (setq preserve-file nil
          out-file
          (format nil "maxplot.~(~a~)"
                  (get-gnuplot-term (getf plot-options :gnuplot_term)))))))

  (unless (null out-file) (setq out-file (plot-file-path out-file preserve-file plot-options)))
  (list terminal-command out-file)))

(defmethod plot-preamble ((plot gnuplot-plot) plot-options)
  (let ((palette (getf plot-options :palette))
                      (meshcolor (if (member :mesh_lines_color plot-options)
                                     (getf plot-options :mesh_lines_color)
                                     '$black)) terminal-file)
    (when (find 'mlist palette :key #'car) (setq palette (list palette)))
    ;; sets-up terminal command and output file name
    (setq terminal-file (gnuplot-terminal-and-file plot-options))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (dest)
        ;; reset initial state
        (format dest "reset~%unset output~%unset multiplot~%")
        ;; user's preamble
        (when (and (getf plot-options :gnuplot_preamble)
                   (> (length  (getf plot-options :gnuplot_preamble)) 0))
          (format dest "~a~%" (getf plot-options :gnuplot_preamble)))
        ;; Don't round numbers with absolute value less than 1e-8 to zero
        (format dest "set zero 0.0~%")
        ;; prints terminal and output commands
        (when (first terminal-file)
          (format dest "~a~%" (first terminal-file)))
        (when (second terminal-file)
          (format dest "set output ~s~%" (second terminal-file)))
        ;; options specific to plot3d
        (when (string= (getf plot-options :type) "plot3d")
          (format dest "set xyplane relative 0~%")
          (if palette
              (progn
                (if meshcolor
                    (progn
                      (format dest
                              "if (GPVAL_VERSION < 5.0) set style line 100 lt rgb ~s lw 1; set pm3d hidden3d 100~%"
                              (rgb-color meshcolor))
                      (format dest
                              "if (GPVAL_VERSION >= 5.0) set pm3d hidden3d 100 border lw 0.5 lt rgb ~s~%"
                              (rgb-color meshcolor))
                      (unless (getf plot-options :gnuplot_4_0)
                        (format dest "set pm3d depthorder~%")))
                    (format dest "set pm3d~%"))
                (format dest "unset hidden3d~%")
                (format dest "set palette ~a~%"
                        (gnuplot-palette (rest (first palette)))))
              (format dest "set hidden3d~%"))
          (let ((elev (getf plot-options :elevation))
                (azim (getf plot-options :azimuth)))
            (when (or elev azim)
              (if elev
                  (format dest "set view ~d" elev)
                  (format dest "set view "))
              (when azim (format dest ", ~d" azim))
              (format dest "~%"))))
        ;; color_bar can be used by plot3d or plot2d
        (unless (getf plot-options :color_bar)
          (format dest "unset colorbox~%"))
        ;; ----- BEGIN GNUPLOT 4.0 WORK-AROUND -----
        ;; When the expression plotted is constant, Gnuplot 4.0 fails
        ;; with a division by 0.  Explicitly assigning cbrange prevents
        ;; the error. Also set zrange to match cbrange.
        (when (floatp (getf plot-options :const_expr))
          (format
           dest "set cbrange [~a : ~a]~%"
           (1- (getf plot-options :const_expr))
           (1+ (getf plot-options :const_expr)))
          (format
           dest "set zrange [~a : ~a]~%"
           (1- (getf plot-options :const_expr))
           (1+ (getf plot-options :const_expr))))
        ;; -----  END GNUPLOT 4.0 WORK-AROUND  -----
        ;; logarithmic plots
        (when (getf plot-options :logx) (format dest "set log x~%"))
        (when (getf plot-options :logy) (format dest "set log y~%"))
        ;; axes labels and legend
        (when (getf plot-options :xlabel)
          (format dest "set xlabel ~s~%" (getf plot-options :xlabel)))
        (when (getf plot-options :ylabel)
          (format dest "set ylabel ~s~%" (getf plot-options :ylabel)))
        (when (getf plot-options :zlabel)
          (format dest "set zlabel ~s~%" (getf plot-options :zlabel)))
        (when (and (member :legend plot-options) 
                   (null (getf plot-options :legend)))
          (format dest "unset key~%"))
        ;; plotting box
        (when (and (member :box plot-options) (not (getf plot-options :box)))
          (format dest "unset border~%")
          (if (and (getf plot-options :axes)
                   (string= (getf plot-options :type) "plot2d"))
              (format dest "set xtics axis~%set ytics axis~%set ztics axis~%")
              (format dest "unset xtics~%unset ytics~%unset ztics~%")))
        ;; 2d grid (specific to plot2d)
        (when (string= (getf plot-options :type) "plot2d")
          (format dest "set grid front~%")
          (if (getf plot-options :grid2d)
              (format dest "set grid~%")
              (format dest "unset grid~%"))
          ;; plot size and aspect ratio for plot2d
          (if (getf plot-options :same_xy)
              (format dest "set size ratio -1~%")
              (if (getf plot-options :yx_ratio)
                  (format dest "set size ratio ~f~%"
                          (getf plot-options :yx_ratio))
                  (if (not (getf plot-options :xy_scale))
                      ;; emit the default only if there is no xy_scale specified.
                      (format dest "set size ratio 0.75~%"))))
          (if (and (getf plot-options :xy_scale)
                   (listp (getf plot-options :xy_scale)))
              (format dest "set size ~{~f~^, ~}~%"
                      (getf plot-options :xy_scale))))
        ;; plot size and aspect ratio for plot3d
        (when (string= (getf plot-options :type) "plot3d")
          (when (getf plot-options :same_xy)
            (format dest "set view equal xy~%"))
          (when (getf plot-options :same_xyz)
            (format dest "set view equal xyz~%"))
          (when (getf plot-options :zmin)
            (format dest "set xyplane at ~f~%" (getf plot-options :zmin))))
        ;; axes tics
        (when (member :xtics plot-options)
          (let ((xtics (getf plot-options :xtics)))
            (if (consp xtics)
                (format dest "set xtics ~{~f~^, ~}~%" xtics)
                (if xtics
                    (format dest "set xtics ~f~%" xtics)
                    (format dest "unset xtics~%")))))
        (when (member :ytics plot-options)
          (let ((ytics (getf plot-options :ytics)))
            (if (consp ytics)
                (format dest "set ytics ~{~f~^, ~}~%" ytics)
                (if ytics
                    (format dest "set ytics ~f~%" ytics)
                    (format dest "unset ytics~%")))))
        (when (member :ztics plot-options)
          (let ((ztics (getf plot-options :ztics)))
            (if (consp ztics)
                (format dest "set ztics ~{~f~^, ~}~%" ztics)
                (if ztics
                    (format dest "set ztics ~f~%" ztics)
                    (format dest "unset ztics~%")))))
        (when (member :color_bar_tics plot-options)
          (let ((cbtics (getf plot-options :color_bar_tics)))
            (if (consp cbtics)
                (format dest "set cbtics ~{~f~^, ~}~%" cbtics)
                (if cbtics
                    (format dest "set cbtics ~f~%" cbtics)
                    (format dest "unset cbtics~%")))))
        ;; axes ranges and style
        (when (and (getf plot-options :x) (listp (getf plot-options :x)))
          (format dest "set xrange [~{~,,,,,,'eg~^ : ~}]~%" (getf plot-options :x)))
        (when (and (getf plot-options :y) (listp (getf plot-options :y)))
          (format dest "set yrange [~{~,,,,,,'eg~^ : ~}]~%" (getf plot-options :y)))
        (when (and (getf plot-options :z) (listp (getf plot-options :z)))
          (format dest "set zrange [~{~,,,,,,'eg~^ : ~}]~%" (getf plot-options :z)))
        (when (and (string= (getf plot-options :type) "plot2d")
                   (member :axes plot-options))
          (if (getf plot-options :axes)
              (case (getf plot-options :axes)
                ($x (format dest "set xzeroaxis~%"))
                ($y (format dest "set yzeroaxis~%"))
                ($solid (format dest "set zeroaxis lt -1~%"))
                (t (format dest "set zeroaxis~%")))))
        ;; title and labels
        (when (getf plot-options :title)
          (format dest "set title \"~a\"~%" (getf plot-options :title)))
        (when (getf plot-options :label)
          (dolist (label (getf plot-options :label))
            (when (and (listp label) (= (length label) 4))
              (format dest "set label ~s at ~{~f~^, ~}~%"
                      (cadr label) (cddr label)))))
        ;; identifier for missing data
        (format dest "set datafile missing ~s~%" *missing-data-indicator*))))
    ;;returns a list with the name of the file created, or nil
    (if (null (second terminal-file))
        nil (list (second terminal-file)))))

(defmethod plot2d-command ((plot gnuplot-plot) fun options range)
  ;; Compute points to plot for each element of FUN.
  ;; If no plottable points are found, end with an error.
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
              (format st "set yrange [~,,,,,,'eg: ~,,,,,,'eg]~%" ymin ymax))))
        ;; user's commands; may overule any of the previous settings
        (when (and (getf options :gnuplot_postamble)
                   (> (length  (getf options :gnuplot_postamble)) 0))
          (format st "~a~%" (getf options :gnuplot_postamble)))
        ;; plot command
        (format st "plot")
        (when (getf options :x)
          (format st " [~{~,,,,,,'eg~^ : ~}]" (getf options :x)))
        (when (getf options :y) 
          (unless (getf options :x)
            (format st " []"))
          (format st " [~{~,,,,,,'eg~^ : ~}]"  (getf options :y)))
        (let ((legend (getf options :legend))
              (colors (getf options :color))
              (types (getf options :point_type))
              (styles (getf options :style))
              (i 0) style plot-name)
          (unless (listp legend) (setq legend (list legend)))
          (unless (listp colors) (setq colors (list colors)))
          (unless (listp styles) (setq styles (list styles)))
          (loop for v in (cdr fun) for points-list in points-lists do
               (when points-list
                 (when ($listp (car points-list))
                   (dolist (level (cdar points-list))
                     (if styles
                         (setq style (nth (mod i (length styles)) styles))
                         (setq style nil))
                     (when ($listp style) (setq style (cdr style)))
                     (incf i)
                     (setq plot-name (ensure-string level))
                     (when (> i 1) (format st ","))
                     (format st " '-'")
                     (format st " title ~s " plot-name)
                     (format st (gnuplot-curve-style style colors types i)))
                   (return))
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
                 (when (> i 1) (format st ","))
                 (format st " '-'")
                 (if plot-name 
                     (format st " title ~s " plot-name)
                     (format st " notitle "))
                 (format st (gnuplot-curve-style style colors types i)))))
        ;; Parses points data
        (format st "~%")
        (let (in-discontinuity points)
          (loop for points-list in points-lists do
               (when points-list
                 ;; case "contour" with several plots in one list
                 (when ($listp (car points-list))
                   (dolist (level (cdr points-list))
                     (loop for (v w) on (cdr level) by #'cddr do
                          (cond ((eq v 'moveto)
                                 ;; A blank line means a discontinuity
                                 (if (null in-discontinuity)
                                     (progn
                                       (format st "~%")
                                       (setq in-discontinuity t))))
                                (t
                                 (format st "~,,,,,,'eg ~,,,,,,'eg ~%" v w)
                                 (setq points t)
                                 (setq in-discontinuity nil))))
                     (if (and (null points)
                              (first (getf options :x))
                              (first (getf options :y)))
                         (format st "~,,,,,,'eg ~,,,,,,'eg ~%"
                                 (first (getf options :x))
                                 (first (getf options :y))))
                     (format st "e~%"))
                   (return))
                 ;; other cases with only one plot per list
                 (loop for (v w) on points-list by #'cddr do
                      (cond ((eq v 'moveto)
                             ;; A blank line means a discontinuity
                             (if (null in-discontinuity)
                                 (progn
                                   (format st "~%")
                                   (setq in-discontinuity t))))
                            (t
                             (format st "~,,,,,,'eg ~,,,,,,'eg ~%" v w)
                             (setq points t)
                             (setq in-discontinuity nil))))
                 (if (and (null points)
                          (first (getf options :x)) (first (getf options :y)))
                     (format st "~,,,,,,'eg ~,,,,,,'eg ~%"
                             (first (getf options :x))
                             (first (getf options :y)))))
               (when points-list (format st "e~%")))))))))

(defmethod plot3d-command ((plot gnuplot-plot) functions options titles)
  (let ((i 0) fun xrange yrange lvars trans (n (length functions)))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string ($pstream)
        (format $pstream "~a"
                (gnuplot-plot3d-command "-" (getf options :palette)
                                        (getf options :gnuplot_curve_styles)
                                        (getf options :color)
                                        titles n))
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
                (setq fun (coerce-float-fun fun lvars "plot3d"))
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
                 (ar (polygon-pts pl)))
            (declare (type (cl:array t) ar))
            (when trans (mfuncall trans ar))
            (when (getf options :transform_xy)
              (mfuncall (getf options :transform_xy) ar))
            (output-points pl (first (getf options :grid)))
            (format $pstream "e~%"))))))))

(defmethod plot-shipout ((plot gnuplot-plot) options &optional output-file)
   (case (getf options :plot_format)
     ($gnuplot
      (let ((file (plot-set-gnuplot-script-file-name options)))
        (with-open-file (fl
                         #+sbcl (sb-ext:native-namestring file)
                         #-sbcl file
                         :direction :output :if-exists :supersede)
          (format fl "~a" (slot-value plot 'data)))
        (gnuplot-process options file output-file)
        (cons '(mlist) (cons file output-file))))
     ($gnuplot_pipes
      (send-gnuplot-command (slot-value plot 'data))
      (when output-file
        (send-gnuplot-command "unset output")
        (cons '(mlist) output-file)))))
