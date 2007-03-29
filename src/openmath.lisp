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
  (nth (mod c 7) '("green" "blue" "red" "magenta" "orange" "black" "brown")))

;; style is a list of the form (type <n1> <n2> <n3> <n4>)
(defun openmath-curve-style (style)
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "{nolines 1} {plotpoints 1} {pointsize 0.7}")
       (when (numberp (second style))
	 (format st " {color ~a}" ($openmath_color (second style)))))
      ($lines
       (format st "{nolines 0} {plotpoints 0}")
       (when (numberp (second style))
	 (format st " {linewidth ~f}" (second style)))
       (when (numberp (third style))
	 (format st " {color ~a}" ($openmath_color (third style)))))
      ($points
       (format st "{nolines 1} {plotpoints 1}")
       (when (numberp (second style))
	 (format st " {pointsize ~f}" (second style)))
       (when (numberp (third style))
	 (format st " {color ~a}" ($openmath_color (third style)))))
      ($linespoints
       (format st "{nolines 0} {plotpoints 1}")
       (when (numberp (second style))
	 (format st " {linewidth ~f}" (second style)))
       (when (numberp (third style))
	 (format st " {pointsize ~f}" (third style)))
       (when (numberp (fourth style))
	 (format st " {color ~a}" ($openmath_color (fourth style))))))))



