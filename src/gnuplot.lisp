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

(defun gnuplot-curve-style (style i)
  (let ((n (nth (mod i 6) '(6 3 1 4 7 -1)))
	(m (nth (mod i 7) '(30 25 3 28 14 19 11))))
    (with-output-to-string
      (st)
      (case (nth 0 style)
        ($dots (format st "with dots"))
        ($impulses (format st "with impulses"))
        ($lines
         (format st "with lines")
         (if (and (nth 1 style) (numberp (nth 1 style)))
           (progn
             (format st " linewidth ~g" (nth 1 style))
             (if (and (nth 2 style) (integerp (nth 2 style)))
               (format st " linetype ~d" (nth 2 style))
               (format st " linetype ~d" n)))
           (format st " ~d" n)))
        ($points
         (format st "with points")
         (if (and (nth 1 style) (numberp (nth 1 style)))
           (progn
             (format st " pointsize ~g" (nth 1 style))
             (if (and (nth 2 style) (integerp (nth 2 style)))
               (format st " pointtype ~d" (nth 2 style))
               (format st " pointtype ~d" m)))
           (format st " ~d" m)))
        ($linespoints
         (format st "with linespoints")
         (if (and (nth 1 style) (numberp (nth 1 style)))
           (progn
             (format st " linewidth ~f" (nth 1 style))
             (if (and (nth 2 style) (numberp (nth 2 style)))
               (progn
                 (format st " pointsize ~f" (nth 2 style))
                 (if (and (nth 3 style) (integerp (nth 3 style)))
                   (progn
                     (format st " linetype ~d" (nth 3 style))
                     (if (and (nth 4 style) (integerp (nth 4 style)))
                       (format st " pointtype ~d" (nth 4 style))
                       (format st " pointtype ~d" m)))
                   (format st " linetype ~d pointtype ~d" n n)))
               (format st " linetype ~d pointtype ~d" n)))
           (format st " linetype ~d pointtype ~d" m)))
        (t (format st "with lines ~d" n))))))

