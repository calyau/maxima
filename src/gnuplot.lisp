;; gnuplot.lisp: routines for Maxima's interface to gnuplot 4.0
;; Copyright (C) 2007 J. Villate
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

(defun $gnuplot_color (c)
  (unless (integerp c) (setf c (round c)))
  (nth (mod c 7) '(5 3 1 4 7 6 2)))

(defun $gnuplot_pointtype (c)
  (unless (integerp c) (setf c (round c)))
  (nth (mod c 13) '(12 7 6 1 2 3 5 4 9 8 11 10 13)))

;; style is a list starting with a symbol from the list: points, lines,
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
;; color must be an integer; values of color from 1 to 7 give the seven
;; default colors. Other integers with lead to the default color that
;; has the same "modulus 7" value. The default colors are:
;;
;;      1 Blue
;;      2 Red
;;      3 Magenta
;;      4 Orange
;;      5 SaddleBrown
;;      6 Lime
;;      7 Aqua
;;
;; pointtype must be an integer; there are 13 default point types. Values
;; of pointtype less than 1 or bigger than 13 will produce the same type
;; as the default type with the same "modulus 23" value. The default types
;; are:
;;      1 Filled circle      8 Filled triangle
;;      2 Open circle        9 Open triangle
;;      3 +                 10 Filled inverted triangle
;;      4 x                 11 Open inverted triangle
;;      5 *                 12 Filled losenge
;;      6 Filled square     13 Open losenge
;;      7 Open square

(defun gnuplot-curve-style (style i)
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "with dots")
       (if (integerp (second style))
	 (format st " lt ~d" ($gnuplot_color (second style)))
	 (format st " lt ~d" ($gnuplot_color i))))
      ($impulses
       (format st "with impulses")
       (if (integerp (second style))
	 (format st " lt ~d" ($gnuplot_color (second style)))
	 (format st " lt ~d" ($gnuplot_color i))))
      ($lines
       (format st "with lines")
       (if (numberp (second style))
	 (format st " lw ~f" (second style)))
       (if (integerp (third style))
	 (format st " lt ~d" ($gnuplot_color (third style)))
	 (format st " lt ~d" ($gnuplot_color i))))
      ($points
       (format st "with points")
       (if (numberp (second style))
	 (format st " ps ~f" (/ (second style) 2))
	 (format st " ps 1.5"))
       (if (integerp (third style))
	 (format st " lt ~d" ($gnuplot_color (third style)))
	 (format st " lt ~d" ($gnuplot_color i)))
       (if (integerp (fourth style))
	 (format st " pt ~d" ($gnuplot_pointtype (fourth style)))
	 (format st " pt ~d" ($gnuplot_pointtype i))))
      ($linespoints
       (format st "with linespoints")
       (if (numberp (second style))
	 (format st " lw ~f" (second style)))
       (if (numberp (third style))
	 (format st " ps ~f" (/ (third style) 2))
	 (format st " ps 1.5"))
       (if (integerp (fourth style))
	 (format st " lt ~d" ($gnuplot_color (fourth style)))
	 (format st " lt ~d" ($gnuplot_color i)))
       (if (integerp (fifth style))
	 (format st " pt ~d" ($gnuplot_pointtype (fifth style)))
	 (format st " pt ~d" ($gnuplot_pointtype i))))
      (t (format st "with lines ~d" ($gnuplot_color i))))))
