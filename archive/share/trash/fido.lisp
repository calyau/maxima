;;; -*- LISP -*-
;;; Defines the Macsyma function FIDO
;;;
;;; FIDO();                tells who if anyone fido is looking for
;;; FIDO(FALSE);           turns off a fido
;;; FIDO(Name1,Name2,...); starts fido looking for those people
;;;
;;; Fido can't be made to look for someone if the alarmclock is already
;;; in use, including if it is already being used by a different fido.

(DECLARE (*EXPR STRIPDOLLAR)
	 (SPECIAL $FIDOLIST $FIDOTIME))

(COND ((NOT (BOUNDP '$FIDOLIST))
       (SETQ $FIDOLIST (NCONS '(MLIST SIMP)))))

(COND ((NOT (BOUNDP '$FIDOTIME))
       (SETQ $FIDOTIME 30.)))

(DEFUN $FIDO FEXPR (X)
	(COND ((NULL X) $FIDOLIST)
	      ((EQ (CAR X) '$FALSE)
	       (SETQ ALARMCLOCK NIL)
	       (SETQ $FIDOLIST (CONS '(MLIST SIMP) NIL)))
	      (ALARMCLOCK
	       (ERROR '|This MACSYMA's Alarm clock already in use.|))
	      (T
	       (SETQ ALARMCLOCK 'FIDO-INTERRUPT)
	       (ALARMCLOCK 'TIME $FIDOTIME)
	       (SETQ $FIDOLIST (CONS '(MLIST SIMP) X)))))

(DEFUN FIDO-INTERRUPT (())
       (DO ((L (CDR $FIDOLIST) (CDR L))
	    (HERE ())
	    (TEMP))
	   ((NULL L)
	    (COND (HERE
		   (CURSORPOS 'A TYO)
		   (PRINC '|;From FIDO: Here | TYO)
		   (COND ((= (LENGTH HERE) 1.)
			  (PRINC '|is | TYO)
			  (PRINC (CAR HERE) TYO))
			 (T
			  (PRINC '|are | TYO)
			  (PRINC (CAR HERE) TYO)
			  (MAPCAR (FUNCTION
				   (LAMBDA (X)
					   (PRINC '|, | TYO)
					   (PRINC X TYO)))
				  (CDR HERE))))
		   (PRINC '/. TYO)
		   (TERPRI TYO)
		   (COND ((CDR $FIDOLIST) (ALARMCLOCK 'TIME $FIDOTIME))
			 (T (SETQ ALARMCLOCK NIL))))
		  (T
		   (ALARMCLOCK 'TIME $FIDOTIME))))
	   (COND ((PROBEF `((USR) ,(SETQ TEMP (STRIPDOLLAR (CAR L))) HACTRN))
		  (PUSH TEMP HERE)
		  (DELETE (CAR L) $FIDOLIST)))))

