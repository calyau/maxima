;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module defopt macro)

;; For defining optimizers which run on various systems.
;; Q: What is an optimizer?
;; A: A transformation which takes place in the compiler.

;; ***==> Right now, DEFOPT is used just like you would a DEFMACRO <==***
;; (defopt <name> <arlist> <body-boo>)

(defmacro defopt (&rest other)
  `(#-gcl define-compiler-macro #+gcl si::define-compiler-macro ,@other)) 
