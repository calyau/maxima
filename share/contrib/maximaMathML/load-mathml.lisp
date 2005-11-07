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

(in-package :maxima)

;;; no saving of expressions under automatically generated labels
(setq $NOLABELS t)

;;; For the time being use concat(load("maximaMathML/load-mathml.lisp"))
;;; or something since the redefined displa can't handle lisp strings. -wj

;; redefines maxima displa
($load "maximaMathML/mathmldisplay")

;; mathml input to maxima
($load "maximaMathML/mathml-maxima")

;; generate MathML Presentation encoding
($load "maximaMathML/PrMathML")

;; generate MathML Content encoding
($load "maximaMathML/CtMathML")

;;; enables mathml content code input to maxima
;;; use the command mathml(); followed by <math>...</math> from stdin
