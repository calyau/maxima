;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;                 GJC 10:11pm  Tuesday, 14 July 1981                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module trprop)

;; Many macsyma extension commands, e.g. $INFIX, $TELLSIMP,
;; $DEFTAYLOR work by doing explicit PUTPROPS.
;; These META-PROP functions allow selected commands to
;; also output DEFPROP's when processed in the Macsyma->lisp translation.

(DEFMVAR META-PROP-P NIL)
(DEFMVAR META-PROP-L NIL)

(DEFUN META-OUTPUT (FORM)
  (IF *IN-TRANSLATE-FILE* (PUSH FORM META-PROP-L))
  ;; unfortunately, MATCOM needs to see properties in order
  ;; to compose tellsimps. so eval it always.
  (EVAL FORM))

(DEFMFUN META-ADD2LNC (ITEM SSYMBOL)
  (IF META-PROP-P
      (META-OUTPUT `(ADD2LNC ',ITEM ,SSYMBOL))
      (ADD2LNC ITEM (SYMBOL-VALUE SSYMBOL))))

(DEFMFUN META-PUTPROP (SSYMBOL ITEM KEY)
  (IF META-PROP-P
      (PROG1 ITEM (META-OUTPUT `(DEFPROP ,SSYMBOL ,ITEM ,KEY)))
      (PUTPROP SSYMBOL ITEM KEY)))

(DEFMFUN META-MPUTPROP (SSYMBOL ITEM KEY)
  (IF META-PROP-P
      (PROG1 ITEM (META-OUTPUT `(MDEFPROP ,SSYMBOL ,ITEM ,KEY)))
      (MPUTPROP SSYMBOL ITEM KEY)))

(DEFMFUN META-FSET (SSYMBOL DEFINITION)
  (IF META-PROP-P
      (PROG1 DEFINITION (META-OUTPUT
			 `(FSET ',SSYMBOL (coerce ',DEFINITION 'function))))
      (FSET SSYMBOL (coerce definition 'function))))

