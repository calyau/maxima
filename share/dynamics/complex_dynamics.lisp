;; complex_dynamics.lisp - functions julia, mandelbrot and rk
;;   
;; Copyright (C) 2006-2021 Jaime E. Villate <villate@fe.up.pt>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.
;;

(in-package :maxima)
(macsyma-module dynamics)

;; Function $mandelbrot displays the Mandelbrot set
;; on the region: xmin < x <xmax, ymin < y <ymax.

(defmfun $mandelbrot (&rest extra-options)
  (let (plot output-file (options (copy-tree *plot-options*))
        a b c d e num  dx dy x xmax xmin y ymax ymin m nx ny)
    (setf (getf options :type) "plot2d")
    (unless (getf options :x) (setf (getf options :x) '(-2 2)))
    (unless (getf options :y) (setf (getf options :y) '(-2 2)))
    (unless (getf options :iterations) (setf (getf options :iterations) 9))
    (unless (getf options :xlabel) (setf (getf options :xlabel) "x"))
    (unless (getf options :ylabel) (setf (getf options :ylabel) "y"))
    (unless (member :color_bar options) (setf (getf options :color_bar) t))
    (setf (getf options :palette)
          '(((mlist) $gradient $magenta $violet $blue $cyan $green $yellow
            $orange $red $brown $black)))
                                          
    ;; Parses the options given in the command line
    (setq options (plot-options-parser extra-options options))
    (unless (getf options :yx_ratio) (setf (getf options :same_xy) t))
    (setq xmin (car (getf options :x))) 
    (setq xmax (cadr (getf options :x)))
    (setq ymin (car (getf options :y))) 
    (setq ymax (cadr (getf options :y)))
    (setq m (getf options :iterations))
    (setq nx (or (car (getf options :grid)) 30))
    (setq ny (or (cadr (getf options :grid)) 30))
    (setq dx (/ (rationalize (- xmax xmin)) nx)) ; x incr. per pixel
    (setq dy (/ (rationalize (- ymax ymin)) ny)) ; y incr. per pixel

    ;; Creates the object that will be passed to the external graphic program
    (setq plot (make-instance 'gnuplot-plot))
    (if (eq (getf options :plot_format) '$gnuplot_pipes)
        (setf (slot-value plot 'pipe) T)
        (setf (getf options :plot_format) '$gnuplot))

    (setq output-file (plot-preamble plot options))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)            
        (format st "set palette ~a~%"
                (gnuplot-palette (rest (first (getf options :palette)))))
        (format st "unset key~%")
        (format st "plot '-' with image~%")
        ;; iterates through all grid points
        (dotimes (i ny) 
          (setq y (+ ymin (/ dy 2) (* i dy)))
          (dotimes (j nx)
            (setq x (+ xmin (/ dx 2) (* j dx)))
            (setq a 0) (setq b 0)
            (setq num m)
            (dotimes (l m)
              (setq c (* a a))
              (setq d (* b b))
              (setq e (* 2 a b))
              (when (> (+ c d) 4) (progn (setq num l) (return)))
              (setq a (+ (float x) (- c d)))
              (setq b (+ (float y) e)))
            (format st "~f ~f ~d~%" x y num)))
        (format st "e~%"))))
    (plot-shipout plot options output-file)))

;; Function $julia(x,y) displays the Julia set for the
;; point (x,y) of the complex plane, on the region: xmin < x <xmax,
;; ymin < y <ymax.

(defmfun $julia (x y &rest extra-options)
  (let (plot output-file (options (copy-tree *plot-options*))
        num dx dy xmax xmin ymax ymin a b c d e a0 b0 m nx ny)
    (setf (getf options :type) "plot2d")
    (unless (getf options :x) (setf (getf options :x) '(-2 2)))
    (unless (getf options :y) (setf (getf options :y) '(-2 2)))
    (unless (getf options :iterations) (setf (getf options :iterations) 9))
    (unless (getf options :xlabel) (setf (getf options :xlabel) "x"))
    (unless (getf options :ylabel) (setf (getf options :ylabel) "y"))
    (unless (member :color_bar options) (setf (getf options :color_bar) t))
    (setf (getf options :palette)
          '(((mlist) $gradient $magenta $violet $blue $cyan $green $yellow
            $orange $red $brown $black)))
                                          
    ;; Parses the options given in the command line
    (setq options (plot-options-parser extra-options options))
    (unless (getf options :yx_ratio) (setf (getf options :same_xy) t))
    (setq xmin (car (getf options :x))) 
    (setq xmax (cadr (getf options :x)))
    (setq ymin (car (getf options :y))) 
    (setq ymax (cadr (getf options :y)))
    (setq m (getf options :iterations))
    (setq nx (or (car (getf options :grid)) 30))
    (setq ny (or (cadr (getf options :grid)) 30))
    (setq dx (/ (rationalize (- xmax xmin)) nx)) ; x incr. per pixel
    (setq dy (/ (rationalize (- ymax ymin)) ny)) ; y incr. per pixel

    ;; Creates the object that will be passed to the external graphic program
    (setq plot (make-instance 'gnuplot-plot))
    (if (eq (getf options :plot_format) '$gnuplot_pipes)
        (setf (slot-value plot 'pipe) T)
        (setf (getf options :plot_format) '$gnuplot))

    (setq output-file (plot-preamble plot options))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)
        (format st "set palette ~a~%"
                (gnuplot-palette (rest (first (getf options :palette)))))
        (format st "unset key~%")
        (format st "plot '-' with image~%")
        
        ;; iterates through all grid points
        (dotimes (i ny) 
          (setq b0 (+ ymin (/ dy 2) (* i dy)))
          (dotimes (j nx)
            (setq a0 (+ xmin (/ dx 2) (* j dx)))
            (setq a (float a0))
            (setq b (float b0))
            (setq num m)
            (dotimes (l m)
              (setq c (* a a))
              (setq d (* b b))
              (setq e (* 2 a b))
              (when (> (+ c d) 4) (progn (setq num l) (return)))
              (setq a (+ x (- c d)))
              (setq b (+ y e)))
            (format st "~f ~f ~d~%" a0 b0 num)))
        (format st "e~%"))))
    (plot-shipout plot options output-file)))

;; Function $rk implements the 4th order Runge-Kutta numerical method.
;;  exprs:   an expression or maxima list with n expressions
;;  vars:    name of (or list of names of) the independent variable(s)
;;  initial: initial value for the variable (or list of initial values)
;;  domain:  maxima list with four elements (name of the independent
;;           variable, its initial and final values and its increment)
(defmfun $rk (exprs vars initial domain
            &aux d u fun k1 k2 k3 k4 r1 r2 r3 traj r
              (it (mapcar #'coerce-float (cddr domain))))
  (unless ($listp exprs) (setq exprs `((mlist) ,exprs)))
  (unless ($listp initial) (setq initial `((mlist) ,initial)))
  (unless ($listp vars) (setq vars `((mlist) ,vars)))
  (dolist (var (cdr vars))
    (unless (symbolp var)
      (merror (intl:gettext "rk: variable name expected; found: ~M") var)))
  (unless (symbolp (cadr domain))
    (merror (intl:gettext "rk: variable name expected; found: ~M")
            (cadr domain)))
  (setq vars (concatenate 'list '((mlist)) (list (cadr domain)) (cdr vars)))
  (setq r (concatenate 'list `(,(car it)) (mapcar #'coerce-float (cdr initial))))
  (setq fun (mapcar #'(lambda (x) (coerce-float-fun x vars)) (cdr exprs)))
  (setq d (/ (- (cadr it) (car it)) (caddr it)))
  (setq traj (list (cons '(mlist) r)))
  (do ((m 1 (1+ m))) ((> m d))
    (ignore-errors
      (setq k1 (mapcar #'(lambda (x) (apply x r)) fun))
      (setq r1 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ (caddr it) 2) x)) k1)))
      (push (+ (car r) (/ (caddr it) 2)) r1)
      (setq k2 (mapcar #'(lambda (x) (apply x r1)) fun))
      (setq r2 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ (caddr it) 2) x)) k2)))
      (push (+ (car r) (/ (caddr it) 2)) r2)
      (setq k3 (mapcar #'(lambda (x) (apply x r2)) fun))
      (setq r3 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (caddr it) x)) k3)))
      (push (+ (car r) (caddr it)) r3)
      (setq k4 (mapcar #'(lambda (x) (apply x r3)) fun))
      (setq u (map 'list #'+
                   (mapcar #'(lambda (x) (* 1/6 x)) k1)
                   (mapcar #'(lambda (x) (* 1/3 x)) k2)
                   (mapcar #'(lambda (x) (* 1/3 x)) k3)
                   (mapcar #'(lambda (x) (* 1/6 x)) k4)))
      (setq r 
            (concatenate 'list
                         `(,(+ (car it) (* m (caddr it))))
                         (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (caddr it) x)) u))))
      (push (cons '(mlist) r) traj)))
  (when (< (car r) (cadr it))
    (let ((s (- (cadr it) (car r))))
      (ignore-errors
        (setq k1 (mapcar #'(lambda (x) (apply x r)) fun))
        (setq r1 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ s 2) x)) k1)))
        (push (+ (car r) (/ s 2)) r1)
        (setq k2 (mapcar #'(lambda (x) (apply x r1)) fun))
        (setq r2 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ s 2) x)) k2)))
        (push (+ (car r) (/ s 2)) r2)
        (setq k3 (mapcar #'(lambda (x) (apply x r2)) fun))
        (setq r3 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* s x)) k3)))
        (push (+ (car r) s) r3)
        (setq k4 (mapcar #'(lambda (x) (apply x r3)) fun))
        (setq u (map 'list #'+
                     (mapcar #'(lambda (x) (* 1/6 x)) k1)
                     (mapcar #'(lambda (x) (* 1/3 x)) k2)
                     (mapcar #'(lambda (x) (* 1/3 x)) k3)
                     (mapcar #'(lambda (x) (* 1/6 x)) k4)))
        (setq r 
              (concatenate 'list
                           `(,(cadr it))
                           (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* s x)) u))))
        (push (cons '(mlist) r) traj))))
  (cons '(mlist) (nreverse traj)))

