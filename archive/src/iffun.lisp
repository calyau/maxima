;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;;transformation funtions - part of the PLOT package

(declare-top(special logbas cosang sinang))

(setq logbas (log 10.0))

(defun $clog (xf) (// (cond ((= 0.0 xf) -90.0) (t (log (abs xf)))) logbas))

(defun $polarx (xf yf) (*$ yf (cos xf)))

(defun $polary (xf yf) (*$ yf (sin xf)))

(defun $reflect (xf) (-$ xf))

(defun $ytox (xf yf) xf yf)

(defun $xtoy (xf yf) yf xf)

(defun $ztoy (xf yf zf) xf yf zf zf)

(defun $ztox (xf yf zf) xf yf zf)

(declare-top(special cosang sinang))

(defun $initrotate (ang) (setq cosang (cos ang) sinang (sin ang)) nil)

($initrotate (atan 1. 0.))

(defun $rotatex (xf yf) (-$ (*$ xf cosang) (*$ yf sinang)))

(defun $rotatey (xf yf) (+$ (*$ xf sinang) (*$ yf cosang)))

(declare-top (special ex ey ez cosal cosbe cosga singa2 x0 z0))

(defun $initperspec (xf yf zf xf1 yf1 zf1) xf
       ((lambda (ax ay az dx dy dz d r)
		(setq ax xf ay yf az zf ex xf1 ey yf1 ez zf1
		      dx (-$ xf xf1) dy (-$ yf yf1) dz (-$ zf zf1)
		      d (sqrt (+$ (*$ dx dx) (*$ dy dy) (*$ dz dz)))
		      cosal (// dx d) cosbe (// dy d) cosga (// dz d)
		      singa2 (-$ 1.0 (*$ cosga cosga))
		      x0 (// ex ey) z0 (// ez ey)
		      r
		      (or (car (errset  (// (sqrt (-$ 1.0 (*$ cosga cosga))))))
			  1.0d40)
		      ) ;;Some big number??
		nil)
	0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))

(comment 
(defun $p3dx (xf yf zf)
       ((lambda (q)
		(*$ (-$ (*$ (+$ ex (*$ q (-$ xf ex)) (-$ ax)) cosbe)
			(*$ (+$ ey (*$ q (-$ yf ey)) (-$ ay)) cosal))
		    r))
	(// d (+$ (*$ (-$ xf ex) cosal) (*$ (-$ yf ey) cosbe) (*$ (-$ zf ez) cosga)))))

(defun $p3dy (xf yf zf)
       ((lambda (q)
		(*$ (+$ ez (*$ q (-$ zf ez)) (-$ az)) r))
	(// d (+$ (*$ (-$ xf ex) cosal) (*$ (-$ yf ey) cosbe) (*$ (-$ zf ez) cosga)))))
)  ;;end of comment

(defun $p3dx (xf yf zf)
       (setq xf (-$ xf ex) yf (-$ yf ey))
       (// (-$ (*$ xf cosbe) (*$ yf cosal))
	    (+$ (*$ xf cosal) (*$ yf cosbe) (*$ (-$ zf ez) cosga))))

(defun $p3dy (xf yf zf)
       (setq zf (-$ zf ez))
       (// zf (+$ (*$ (-$ xf ex) cosal) (*$ (-$ yf ey) cosbe) (*$ zf cosga))))

(defun $p3dxr (xf yf zf) (-$ ($p3dx xf yf zf)))

(setf (symbol-function  '$p3dyr)  #'$p3dy)

(defun $howclose3d (xf yf zf)
       (setq xf (-$ xf ex) yf (-$ yf ey) zf (-$ zf ez))
       (sqrt (+$ (*$ xf xf) (*$ yf yf) (*$ zf zf))))

(defun $np3dx (xf yf) (-$ (*$ cosbe xf) (*$ cosal yf)))

(defun $np3dy (xf yf zf)
       (-$ (*$ singa2 zf) (*$ cosga (+$ (*$ cosbe yf) (*$ cosal xf)))))

(defun $np3dxr (xf yf) (-$ ($np3dx xf yf)))

(setf (symbol-function  '$np3dyr)  #'$np3dy)

(defun $howclosenp3d (xf yf zf) (+$ (*$ xf cosal) (*$ yf cosbe) (*$ zf cosga)))

(defun $old3dx (xf yf) (// (-$ xf ex) (-$ yf ey)))
(defun $old3dy (xf yf zf) xf (// (-$ zf ez) cosbe (-$ yf ey)))
(defun $old3dxr (xf yf) (-$ ($old3dx xf yf)))

(setf (symbol-function  '$old3dyr)  #'$old3dy)

(setf (symbol-function  '$howcloseold3d)  #'$howclose3d)

(defun $oldnp3dx (xf yf) (*$ cosbe (-$ xf (*$ yf x0))))
(defun $oldnp3dy (xf yf zf) xf (-$ zf (*$ yf z0)))
(defun $oldnp3dxr (xf yf) (-$ ($oldnp3dx xf yf)))

(setf (symbol-function  '$oldnp3dyr)  #'$oldnp3dy)

(defun $howcloseoldnp3d (xf yf zf) (*$ cosbe (+$ (*$ x0 xf) yf (*$ z0 zf))))

