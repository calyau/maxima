;=============================================================================
;    (c) copyright 1988	 Kent State University  kent, ohio 44242 
;		all rights reserved.
;
; Authors:  Paul S. Wang, Barbara Gates
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;=============================================================================

;;  ---------  ;;                 load
;;  gtload.l   ;;    gentran code generation package
;;  ---------  ;;              for vaxima

;;(cond ((not (boundp '*gentran-dir))
 ;;      (setq *gentran-dir (getenv "GENTRAN"))))

(declare (special local-obj-dir))


(load "convmac")

(putprop 'procforttem "templt" 'autoload)
(putprop 'procrattem "templt" 'autoload)
(putprop 'procctem "templt" 'autoload)
(putprop '$readvexp "read.l" 'autoload)
(putprop 'gentranparse "parser" 'autoload)
(putprop 'opt "opt"    'autoload)
(putprop 'seg "segmnt" 'autoload)
(putprop 'fortcode "lspfor" 'autoload)
(putprop 'ratcode "lsprat" 'autoload)
(putprop 'ccode "lspc"   'autoload)

(foreach fname in '( "init" "lspfor" "templt" "global" "intrfc"
		     "pre" "output" "vaxlsp" "gtfix.l")

               do  (load fname))
