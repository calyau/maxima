;; plotdf.mac - Adds a function plotdf() to Maxima, which draws a Direction
;;              Field for an ordinary 1st order differential equation,
;;              or for a system of two autonomous 1st order equations.
;;   
;; Copyright (C) 2004 Jaime E. Villate <villate@gnu.org>
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA  02111-1307  USA
;;
;; See plotdf.usg (which should come together with this program) for
;; a usage summary
;;
;; $Id: plotdf.lisp,v 1.1 2004-10-30 08:21:08 vvzhy Exp $

(in-package "MAXIMA")

;; default plotdf options
(defvar $plotdf_options '((mlist)
			  ;; Width in x direction of the x values
			  ((mlist) $xradius 10)
			  ;; Height in y direction of the y values
			  ((mlist) $yradius 10)
			  ;; Width of canvas in pixels
			  ((mlist) $width 500)
			  ;; Height of canvas in pixels
			  ((mlist) $height 500)
			  ;; (xcenter,ycenter) is the origin of the window
			  ((mlist) $xcenter 0)
			  ((mlist) $ycenter 0)
			  ;; xmin ymin xmax ymax .. overrides the -xcenter etc
			  ((mlist) $bbox -10 -10 10 10)
			  ;; The initial value of variable t
			  ((mlist) $tinitial 0)
			  ;; Number of steps to do in one pass
			  ((mlist) $nsteps 100)
			  ;; A semi colon separated list of functions to plot
			  ((mlist) $xfun "")
			  ;; t step size
			  ((mlist) $tstep 0.1)
			  ;; May be both, forward or backward
			  ((mlist) $direction "both")
			  ;; Plot in a separate window x and y versus t
			  ((mlist) $versus_t 0)
			  ;; Place to calculate trajectory
			  ((mlist) $trajectory_at 0 0)
			  ;; List of parameters and values eg k=3,l=7+k
			  ((mlist) $parameters "")
			  ;; List of parameters ranges k=3:5,u
			  ((mlist) $sliders "")
			  ))

;; gets the value of a plotdf option
(defun $get_plotdf_option (name &optional n)
  (sloop for v in (rest $plotdf_options)
	 when (eq (second v) name) do
	 (return (if n (nth n  v) v))))

;; parses a plotdf option into a command-line option for tcl scripts
(defun tcl-get-plotdf-option (name)
  (with-output-to-string (st)
			 (sloop for v in (rest $plotdf_options)
				when (eq (second v) name)
				do (setq vv (mapcar #'stripdollar (rest v)))
				(format st "-~(~a~) " (first vv))
				(format st "{~{~(~a~)~^ ~}}" (rest vv)))))

;; parses a plotdf option into a command-line option for shell scripts
(defun shell-get-plotdf-option (name)
  (with-output-to-string (st)
			 (sloop for v in (rest $plotdf_options)
				when (eq (second v) name)
				do (setq vv (mapcar #'stripdollar (rest v)))
				(format st "-~(~a~) " (first vv))
				(format st "'~{~(~a~)~^ ~}'" (rest vv)))))

;; changes the value of a plotdf option
(defun $set_plotdf_option ( value)
  (setq $plodft_options ($copylist $plotdf_options))
  (unless (and  ($listp value)
		(symbolp (setq name (second value))))
    (merror "~M is not a plotdf option.  Must be [symbol,..data]" value))
  (setq value
	(case name
	      ($xradius (check-list-items name (rest (rest value)) 'number 1))
	      ($yradius (check-list-items name (rest (rest value)) 'number 1))
	      ($width (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($height (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($xcenter (check-list-items name (rest (rest value)) 'number 1))
	      ($ycenter (check-list-items name (rest (rest value)) 'number 1))
	      ($bbox (check-list-items name (rest (rest value)) 'number 4))
	      ($tinitial (check-list-items name (rest (rest value)) 'number 1))
	      ($nsteps (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($xfun value)
	      ($tstep (check-list-items name (rest (rest value)) 'number 1))
	      ($direction (or (member (third value)
				      '($forward $backward $both))
			      (merror "direction: choose one of [forward,backward,both]"))
			  value)
	      ($versus_t (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($trajectory_at (check-list-items name (rest (rest value)) 'number 2))
	      ($parameters value)
	      ($sliders value)))
  (sloop for v on (rest $plotdf_options)
	 when (eq (second (first v)) name)
	 do (setf (first v) value))
  $plotdf_options
  )
 
;; applies float(ev(expression, numer)) to an expression, and return a string

(defun expr_to_str(fun)
  (string-downcase
   (coerce (mstring (mfuncall '$float (mfuncall '$ev fun '$numer))) 'string)))

;; plots the direction field for an ODE  dy/dx = f(x,y), or for an autonomous
;; system of 2 equations dx/dt = f(x,y), dy/dt = g(x,y) 
;;
(defun $plotdf(ode &rest options)
  ;; parse argument ode and prepare string cmd with the equation(s)
  (if ($listp ode)
      (if (= (length ode) 3)
	  (cond 
	   ($show_openplot (setq cmd
				 (concatenate 'string " -dxdt '"
					      (expr_to_str (second ode))
					      "' -dydt '"
					      (expr_to_str (third ode)) "'")))
	   (t (setq cmd (concatenate 'string " -dxdt "
				     (expr_to_str (second ode)) " -dydt "
				     (expr_to_str (third ode))))))
	(merror "Argument must be either dydx or [dxdt, dydt]"))
    (cond ($show_openplot (setq cmd
				(concatenate 'string " -dydx '"
					     (expr_to_str ode) "'")))
	  (t (setq cmd (concatenate 'string " -dydx " (expr_to_str ode))))))

  ;; parse options and copy them to string opts
  (setq opts " ")
  (cond (options
	 (dolist (v options) 
	   ($set_plotdf_option v)
	   (cond
	    ($show_openplot
	     (setq opts
		   (concatenate 'string opts " "
				(shell-get-plotdf-option (second v)))))
	    (t
	     (setq opts (concatenate 'string opts " "
				     (tcl-get-plotdf-option (second v)))))))))

  ;; now call tcl's plotdf
  (cond ($show_openplot
	 ($system (concatenate 'string *maxima-plotdir* "/"
			       $openmath_plot_command) " plotdf" cmd opts))
	(t (princ (concatenate 'string "{plotdf" cmd opts "}" )) "")))

