;;;;;;;;;;;;;;;;; File:  load-mathml.lsp  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Purpose:   Makes maxima display mathml
;;
;; Usage:     load this file into maxima 
;;                loadfile("mathmldisplay.lsp");
;;
;; Author: Paul S. Wang
;; Date: 5/20/99
;
; Authors:  Paul S. Wang, Kent State University
; This work was supported by NSF/USA.
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'maxima)

;;; no saving of expressions under automatically generated labels
(setq $NOLABELS t)  

(loadfile "mathmldisplay.lsp")   ;; redefines maxima displa
(loadfile "mathml-maxima.lsp")   ;; mathml input to maxima
(loadfile "CtMathML.lsp")        ;; generate MathML Content encoding
(loadfile "PrMathML.lsp")        ;; generate MathML Presentation encoding
;;; enables mathml content code input to maxima
;;; use the command mathml(); followed by <math>...</math> from stdin
