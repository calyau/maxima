;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(includef (cond ((status feature ITS)     "libmax;prelud >")
		((status feature TOPS-20) "<libmax>prelude.lisp")
		((status feature Multics) "prelude")
		((status feature Unix)    "libmax//prelud.l")
		(t (error "Unknown system -- see MC:LIBMAX;INCLUD >"))))

(load-macsyma-macros mrgmac)

;;  QUAL-LISP file.
;;  Used by the SHARE;QUAL files.

#.(mode-syntax-on)

(DEFMODE CL () (ATOM <+LABS> <-LABS> <DATA>))

(DEFUN $GREATERS (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (IF (AND (EQ 'MGRP (CAAAR L)) (EQ X (CADDAAR L)))
	  (SETQ NL (CONS (DOUTERN (CADAAR L)) NL)))))

(DEFUN $GEQS (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (IF (AND (EQ 'MGQP (CAAAR L)) (EQ X (CADDAAR L)))
	  (SETQ NL (CONS (DOUTERN (CADAAR L)) NL)))))

(DEFUN $LESSES (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (IF (AND (EQ 'MGRP (CAAAR L)) (EQ X (CADAAR L)))
	  (SETQ NL (CONS (DOUTERN (CADDAAR L)) NL)))))

(DEFUN $LEQS (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (IF (AND (EQ 'MGQP (CAAAR L)) (EQ X (CADAAR L)))
	  (SETQ NL (CONS (DOUTERN (CADDAAR L)) NL)))))

(DEFUN $EQUALS (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (COND ((NOT (EQ 'MEQP (CAAAR L))))
	    ((EQ X (CADAAR L)) (SETQ NL (CONS (DOUTERN (CADDAAR L)) NL)))
	    (T (SETQ NL (CONS (DOUTERN (CADAAR L)) NL))))))

(DEFUN $NEQS (X)
  (SETQ X (DINTERNP X))
  (DO ((L :X:DATA (CDR L)) (NL))
      ((NULL L) (CONS '(MLIST) NL))
      (COND ((NOT (EQ 'MNQP (CAAAR L))))
	    ((EQ X (CADAAR L)) (SETQ NL (CONS (DOUTERN (CADDAAR L)) NL)))
	    (T (SETQ NL (CONS (DOUTERN (CADAAR L)) NL))))))

#.(mode-syntax-off)
