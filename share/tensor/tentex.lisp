(in-package "MAXIMA")

#+cl
($bothcases t) ;; allow alpha and Alpha to be different
;(declare-top
;	 (special lop rop ccol $gcprint texport $labels $inchar
;		  vaxima-main-dir
;		  )
;	 (*expr tex-lbp tex-rbp))

;; top level command the result of tex'ing the expression x.
;; Lots of messing around here to get C-labels verbatim printed
;; and function definitions verbatim "ground"

;(defmspec $tex(l) ;; mexplabel, and optional filename
;  (let ((args (cdr l)))
;  (apply 'tex1  args)))
(defprop $Dl "\\Delta" texword)
(defprop $divG "(\\nabla\\cdot G)" texword)
(defprop $OG "(\\Omega\\cdot G)" texword)
(defprop $Dl_1 "\\Delta^{\\star}" texword)
(defprop $Dl_c "\\Delta_{\\chi}" texword)
(defprop %KDELTA "\\delta" texword)
(defprop $Omega "\\Omega" texword)
(defprop $Om "\\Omega^{\\star}" texword)
(defprop $DLT "\\delta" texword)
(defprop %KDELTA "\\delta" texword)
(defprop %ICURVATURE "\\cal{R}" texword)
(defprop $ICHR1 "\\Gamma" texword)
(defprop $ICHR2 "\\Gamma" texword)
(defprop $PHI  "\\Phi" texword)
(defprop $D_T  "\\frac{\\partial}{\\partial t}" texword)
(defprop $LCH  "\\varepsilon" texword)
(defprop %LCH  "\\varepsilon" texword)
