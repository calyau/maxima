


;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************


;;  ---------  ;;                 load
;;  gtload.l   ;;    gentran code generation package
;;  ---------  ;;              for vaxima

;;(cond ((not (boundp '*gentran-dir))
 ;;      (setq *gentran-dir (getenv "GENTRAN"))))

(in-package :maxima)

(defvar local-obj-dir)

(load (merge-pathnames "convmac.lisp" (maxima-load-pathname-directory)))

(putprop 'procforttem "templt" 'autoload)
(putprop 'procrattem "templt" 'autoload)
(putprop 'procctem "templt" 'autoload)
(putprop '$readvexp "templt" 'autoload)
(putprop 'gentranparse "parser" 'autoload)
(putprop 'opt "opt"    'autoload)
;(putprop 'seg "segmnt" 'autoload)
(putprop 'fortcode "lspfor" 'autoload)
(putprop 'ratcode "lsprat" 'autoload)
(putprop 'ccode "lspc"   'autoload)

(dolist (fname '("init" "lspfor" "lspc" "lsprat" "templt" "global" "intrfc"
		 "pre" "output" "vaxlsp" "segmnt"))
  (load (merge-pathnames fname (maxima-load-pathname-directory))))
