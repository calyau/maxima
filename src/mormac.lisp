;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;; more macros for LISPM compatibility.

;; Needed at read time, by e.g. JPG;SUPRV.  There should be a better
;; way of setting this up for #+.
(SSTATUS FEATURE MAXII)

(DEFMACRO MACSYMA-MODULE (MODULE &REST OPTIONS)
  (setq *macro-file* (if (member 'macro options) t nil))
  (IF (NULL OPTIONS)
      (PUSH 'RUNTIME OPTIONS))
  `(DEFPROP ,MODULE ,OPTIONS MACSYMA-MODULE))


(macsyma-module mormac macro)


