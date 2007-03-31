;; openmath.lisp: routines for Maxima's interface to openmath
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

(defun $openmath_color (c)
  (unless (integerp c) (setf c (round c)))
  (nth (mod c 7) '("skyblue" "blue" "red" "magenta" "orange" "saddlebrown" "green")))

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
;; color must be an integer; values of color from 1 to 7 give the seven
;; default colors. Other integers with lead to the default color that
;; has the same "modulus 7". The default colors are:
;;
;;      1 Blue
;;      2 Red
;;      3 Magenta
;;      4 Orange
;;      5 SaddleBrown
;;      6 Green
;;      7 SkyBlue

(defun openmath-curve-style (style i)
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "{nolines 1} {plotpoints 1} {pointsize 0.7}")
       (if (integerp (second style))
	 (format st " {color ~a}" ($openmath_color (second style)))
         (format st " {color ~a}" ($openmath_color i))))
      ($lines
       (format st "{nolines 0} {plotpoints 0}")
       (if (numberp (second style))
	 (format st " {linewidth ~f}" (second style)))
       (if (integerp (third style))
	 (format st " {color ~a}" ($openmath_color (third style)))
	 (format st " {color ~a}" ($openmath_color i))))
      ($points
       (format st "{nolines 1} {plotpoints 1}")
       (if (numberp (second style))
	 (format st " {pointsize ~f}" (second style))
	 (format st " {pointsize 3}"))
       (if (integerp (third style))
	 (format st " {color ~a}" ($openmath_color (third style)))
	 (format st " {color ~a}" ($openmath_color i))))
      ($linespoints
       (format st "{nolines 0} {plotpoints 1}")
       (if (numberp (second style))
	 (format st " {linewidth ~f}" (second style)))
       (if (numberp (third style))
	 (format st " {pointsize ~f}" (third style))
	 (format st " {pointsize 3}"))
       (if (integerp (fourth style))
	 (format st " {color ~a}" ($openmath_color (fourth style)))
	 (format st " {color ~a}" ($openmath_color i))))
      (t
       (format st "{nolines 0} {plotpoints 0} {color ~a}" ($openmath_color i))))))
