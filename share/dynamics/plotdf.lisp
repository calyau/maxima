;; plotdf.mac - Adds a function plotdf() to Maxima, which draws a Direction
;;              Field for an ordinary 1st order differential equation,
;;              or for a system of two autonomous 1st order equations.
;;   
;; Copyright (C) 2004, 2008, 2011 Jaime E. Villate <villate@fe.up.pt>
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
;; MA  02110-1301, USA.
;;
;; See plotdf.usg (which should come together with this program) for
;; a usage summary
;;
;; $Id: plotdf.lisp,v 1.12 2011-03-09 11:33:46 villate Exp $

(in-package :maxima)

;; parses a plotdf option into a command-line option for tcl scripts
(defun plotdf-option-to-tcl (value s1 s2)
  (let (v vv)
    (unless (and  ($listp value)
                  (symbolp (setq name (second value))))
      (merror
       (intl:gettext
        "plotdf-option-to-tcl: Expecting a symbol for the option name, found: \"~M\"") value))
    (case name
      (($xradius $yradius $xcenter $ycenter $tinitial $tstep)
       (setq v (check-option (cdr value) #'realp "a real number" 1))
       (setq value (list '(mlist) name v)))
      (($width $height $nsteps $versus_t)
       (setq v (check-option (cdr value) #'naturalp "a natural number" 1))
       (setq value (list '(mlist) name v)))
      ($trajectory_at
       (setq v (check-option (cdr value) #'realp "a real number" 2))
       (setq value (cons '(mlist) (cons name v))))
      ($bbox
       (setq v (check-option (cdr value) #'realp "a real number" 4))
       (setq value (cons '(mlist) (cons name v))))
      (($xfun $parameters $sliders $vectors $fieldlines $curves 
              $windowtitle $xaxislabel $yaxislabel $psfile)
       (setq v (check-option (cdr value) #'stringp "a string" 1))
       (setq value (list '(mlist) name v)))
      ($axes
       (if (not (third value))
           (setq value '((mlist) $axes 0))  
           (case (third value)
             ($x (setq value '((mlist) $axes "x")))
             ($y (setq value '((mlist) $axes "y")))
             (t (setq value '((mlist) $axes "xy"))))))
      ($box
       (if (not (third value))
           (setq value '((mlist) $nobox 1))
	   (setq value '((mlist) $nobox 0))))
      ($direction
       (or
        (member (ensure-string (third value)) '("forward" "backward" "both") :test #'equal)
        (merror
         (intl:gettext
          "plotdf-option-to-tcl: direction should be forward, backward or both."))))
      (t (cond
           ((eql name s1)
            (setq value (cons '(mlist) (cons '$x (cddr (check-range value))))))
           ((eql name s2)
            (setq value (cons '(mlist) (cons '$y (cddr (check-range value))))))
           (t (merror (intl:gettext "plotdf-option-to-tcl: unknown option ~M") name)))))
    (setq vv (mapcar #'(lambda (a) (if (symbolp a) (ensure-string a) a)) (cdr value)))
    (with-output-to-string (st)
      (cond ((or (equal (first vv) "x") (equal (first vv) "y"))
             (format st "-~(~a~)center " (first vv))
             (format st "{~a} " (/ (+ (third vv) (second vv)) 2))
             (format st "-~(~a~)radius " (first vv))
             (format st "{~a}" (/ (- (third vv) (second vv)) 2)))
            (t
             (format st "-~(~a~) " (first vv))
             (format st "{~{~a~^ ~}}" (rest vv)))))))

;; applies float(ev(expression, numer)) to an expression, and returns a string

(defun expr_to_str (fun)
  (mstring (mfuncall '$float (mfuncall '$ev fun '$numer))))

;; plots the direction field for an ODE  dy/dx = f(x,y), or for an autonomous
;; system of 2 equations dx/dt = f(x,y), dy/dt = g(x,y) 
;;
(defun $plotdf (ode &rest options)  
  (let (file cmd (opts " ") (s1 '$x) (s2 '$y) vars)
    (unless ($listp ode) (setf ode `((mlist) ,ode)))

    ;; if the variables are not x and y, their names must be given right
    ;; after the expression for the ode's
    (when
      (and (listp (car options)) (= (length (car options)) 3)
            (or (symbolp (cadar options)) ($subvarp (cadar options)))
            (or (symbolp (caddar options)) ($subvarp (caddar options))))
      (setq s1 (cadar options))
      (setq s2 (caddar options))
      (setq options (cdr options)))

    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))

    ;; check that the expressions given contain only the axis variables
    (setq vars (cdr (mfuncall '$listofvars ode)))
    (unless (search "-parameters " opts)
      (dolist (var vars)
        (unless (or (eq var s1) (eq var s2))
          (merror 
           (intl:gettext "plotdf: expression(s) given can only depend on ~M and ~M~%Found extra variable ~M") s1 s2 var))))

    ;; substitute $x by s1 and $y by s2
    (defun subxy (expr)
      (if (listp expr)
          (mapcar #'subxy expr)
        (cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
    (setf ode (mapcar #'subxy ode))

    ;; parse the differential equation expressions
    (case (length ode)
          (3 (setq cmd (concatenate 'string " -dxdt \""
                                    (expr_to_str (second ode)) "\" -dydt \""
                                    (expr_to_str (third ode)) "\"")))
          (2 (setq cmd (concatenate 'string " -dydx \""
                                    (expr_to_str (second ode)) "\"")))
          (t (merror 
              (intl:gettext "plotdf: first argument must be either an expression or a list with two expressions."))))
    (setq file (plot-temp-file (format nil "maxout~d.xmaxima" (getpid))))
    (show-open-plot
     (with-output-to-string
         (st)
       (cond
         ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
         (t (format st "{plotdf ~a ~a} " cmd opts))))
     file)
    file))

;; plot equipotential curves for a scalar field f(x,y)
(defun $ploteq (fun &rest options)
  
  (let (file cmd mfun (opts " ") (s1 '$x) (s2 '$y))
    (setf mfun `((mtimes) -1 ,fun))
    ;; if the variables are not x and y, their names must be given right
    ;; after the expression for the ode's
    (when
      (and (listp (car options)) (= (length (car options)) 3)
            (or (symbolp (cadar options)) ($subvarp (cadar options)))
            (or (symbolp (caddar options)) ($subvarp (caddar options))))
      (setq s1 (cadar options))
      (setq s2 (caddar options))
      (setq options (cdr options)))
    (defun subxy (expr)
      (if (listp expr)
          (mapcar #'subxy expr)
          (cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
    (setf mfun (mapcar #'subxy mfun))
;; the next two lines should take into account parameters given in the options
;;    (if (delete '$y (delete '$x (rest (mfuncall '$listofvars ode))))
;;        (merror "The equation(s) can depend only on 2 variable which must be specified!"))
    (setq cmd (concatenate 'string " -dxdt \""
			   (expr_to_str (mfuncall '$diff mfun '$x))
			   "\" -dydt \""
			   (expr_to_str (mfuncall '$diff mfun '$y)) 
			   "\" "))
    
    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-vectors " opts)
      (setq opts (concatenate 'string opts " -vectors {}")))
    (unless (search "-fieldlines " opts)
      (setq opts (concatenate 'string opts " -fieldlines {}")))
    (unless (search "-curves " opts)
      (setq opts (concatenate 'string opts " -curves {red}")))
    (unless (search "-windowtitle " opts)
      (setq opts (concatenate 'string opts " -windowtitle {Ploteq}")))
    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))
							      
    (setq file (plot-temp-file (format nil "maxout~d.xmaxima" (getpid))))
    (show-open-plot
     (with-output-to-string
         (st)
       (cond ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
             (t (format st "{plotdf ~a ~a}" cmd opts))))
     file)
    file))
