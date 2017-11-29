;; xmaxima.lisp: routines for Maxima's interface to xmaxima
;; Copyright (C) 2007-2013 J. Villate
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
	 (format st " \{ linewidth ~,2f \}" (second style)))
       (if (third style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-color colors i))))
      ($points
       (format st "\{ nolines 1 \} \{ plotpoints 1 \}")
       (if (realp (second style))
	 (format st " \{ pointsize ~,2f \}" (second style))
	 (format st " \{ pointsize 3 \}"))
       (if (third style)
	 (format st " \{ color ~a \}" (xmaxima-color colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-color colors i))))
      ($linespoints
       (format st "\{ nolines 0 \} \{ plotpoints 1 \}")
       (if (realp (second style))
	 (format st " \{ linewidth ~,2f \}" (second style)))
       (if (realp (third style))
	 (format st " \{ pointsize ~,2f \}" (third style))
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
       (setq fun (format nil "{value ~,8f} {colorrange ~,8f}" gray range)))
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
             (format nil " {hue ~,8f} {saturation ~,8f} {value ~,8f} {colorrange ~,8f}"
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
           (setq fun (format nil "~{{ ~,8f ~s}~^ ~}" (reverse map)))
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
			 
(defun xmaxima-print-header (dest plot-options)
  (cond ($show_openplot (format dest "~a -data {~%" (getf plot-options :type)))
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
    (format dest " {psfile ~s}" (getf plot-options :ps_file)))
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
    (format dest " {xrange ~{~g~^ ~}}" (getf plot-options :x)))
  (when (getf plot-options :y)
    (format dest " {yrange ~{~g~^ ~}}" (getf plot-options :y)))
  (when (getf plot-options :xlabel)
    (format dest " {xaxislabel ~s}" (getf plot-options :xlabel)))
  (when (getf plot-options :ylabel)
    (format dest " {yaxislabel ~s}" (getf plot-options :ylabel)))
  (when (getf plot-options :z)
    (format $pstream " {zcenter ~g }"
	    (/ (apply #'+ (getf plot-options :z)) 2))
    (format $pstream " {zradius ~g }~%"
	    (/ (apply #'- (getf plot-options :z)) -2)))
  (format dest "~%"))

