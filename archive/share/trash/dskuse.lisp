;;; -*- LISP -*-

;;; Compiler declarations. IOTA is a function for binding I/O
;;; streams. See documentation in MC:LIBDOC;IOTA >

(EVAL-WHEN (EVAL COMPILE)
	   (COND ((NOT (STATUS FEATURE IOTA))
		  (LOAD '((DSK LIBLSP) IOTA FASL)))))

;;; More compiler stuff. Macsyma builtins.

(DECLARE (*EXPR STRIPDOLLAR))

;;; These two MACRO's are just for CONS'ing up silly Macsyma structure

 ;; (MLIST-SIMP <thing1> <thing2> ...)
 ;; returns ((MLIST SIMP) <thing1> <thing2> ...)

(DEFUN MLIST-SIMP MACRO (X)
       `(CONS '(MLIST SIMP) (LIST . ,(CDR X))))

 ;; (MTIMES-BLOCKS <n>)
 ;; returns ((MTIMES SIMP) <n> $BLOCKS) which will display as 
 ;; "<n> BLOCKS" when Macsyma's display routines are run.

(DEFUN MTIMES-BLOCKS MACRO (X)
       `(LIST '(MTIMES SIMP) ,(CADR X) '$BLOCKS))

;;; Returns info about the total number of blocks free in the system.
;;; expects an arg of an already open file object which is open to
;;; a file directory (file with name .FILE. (DIR) on any directory.)

(DEFUN STATUS-GLOBAL-DSKUSE (STREAM)
       (READLINE STREAM)
       (DO ((C (TYI STREAM) (TYI STREAM))
	    (L ()))
	   ((= C 13.) (LIST* '(MLIST SIMP) '|&TOTAL FREE BLOCKS| (NREVERSE L)))
	   (COND ((= C 35.)
		  (PUSH (APPEND '((MLIST SIMP))
				((LAMBDA (PACK)
					 (COND ((= PACK 13.)
						(LIST '|&SECONDARY PACK|
						      PACK))
					       (T
						(LIST '|&PRIMARY PACK|
						      PACK))))
				 (STATUS-DSKUSE\AUX STREAM 61.))
				(NCONS
				 (MTIMES-BLOCKS
				  (STATUS-DSKUSE\AUX STREAM 32.)
				  )))
			L)))))

;;; STATUS-DSKUSE\AUX
;;;  This function TYI's from STREAM until a character whose fixnum 
;;;  value is TERM is seen (eats the TERM character, too). 
;;;  Returns the readlisted form of the characters seen before TERM.

(DEFUN STATUS-DSKUSE\AUX (STREAM TERM)
       (DO ((C (TYI STREAM) (TYI STREAM))
	    (L () (CONS C L)))
	   ((= C TERM)
	    (LET ((BASE 10.)) (READLIST (NREVERSE L))))))

;;; STATUS-USER-DSKUSE
;;;  Returns info about a user's disk use. The file object corresponding
;;;  to the user's file directory must already be open and the first two
;;;  lines should already have been READLINE'd. 
;;;  NAME= STRIPDOLLAR'd name
;;;  USERNAME= Un-STRIPDOLLAR'd name
;;;  DIRNAME= Un-STRIPDOLLAR'd dirname
;;;  ALL-FLAG= a flag which if non-null means count all files in the 
;;;            directory - if NIL means just to count files with FN1 the
;;;	       same as NAME.
;;;  STREAM= a file object open to the directory

(DEFUN STATUS-USER-DSKUSE (NAME USERNAME DIRNAME ALL-FLAG STREAM)
       (DO ((C (PROGN (TYI STREAM) (TYI STREAM))
	       (PROGN (TYI STREAM) (TYI STREAM)))
	    (DIR-PRIMARY-DSKUSE 0.)
	    (DIR-SECONDARY-DSKUSE 0.)
	    (USR-PRIMARY-DSKUSE 0.)
	    (USR-SECONDARY-DSKUSE 0.))
	   ((= C 12.)
	    (LIST* (MLIST-SIMP '|&DIRECTORY BLOCK USAGE|
			       DIRNAME
			       (MLIST-SIMP '&PRIMARY
					   (MTIMES-BLOCKS
					    DIR-PRIMARY-DSKUSE))
			       (MLIST-SIMP '&SECONDARY
					   (MTIMES-BLOCKS
					    DIR-SECONDARY-DSKUSE)))
		   (COND ((NOT ALL-FLAG)
			  (NCONS 
			   (MLIST-SIMP '|&USER BLOCK USAGE|
				       USERNAME
				       (MLIST-SIMP '&PRIMARY
						   (MTIMES-BLOCKS
						    USR-PRIMARY-DSKUSE))
				       (MLIST-SIMP
					'&SECONDARY
					(MTIMES-BLOCKS
					 USR-SECONDARY-DSKUSE))))))))
	   (TYI STREAM) ; TYI second space
	   (LET ((SPEC (READLIST (DELETE 32.
					 (LIST (TYI STREAM) (TYI STREAM)
					       (TYI STREAM) (TYI STREAM))))))
		(COND ((NOT (NUMBERP SPEC))
		       (READLINE STREAM))
		      (T
		       (COND ((= SPEC 13.)
			      ((LAMBDA (V)
				       (SETQ USR-SECONDARY-DSKUSE
				        (+ USR-SECONDARY-DSKUSE (CAR V)))
				       (SETQ DIR-SECONDARY-DSKUSE
					(+ DIR-SECONDARY-DSKUSE (CDR V))))
			       (STATUS-USER-DSKUSE\PARSE-LINE STREAM
							      ALL-FLAG
							      NAME)))
			     (T
			      ((LAMBDA (V)
				       (SETQ USR-PRIMARY-DSKUSE
					(+ USR-PRIMARY-DSKUSE (CAR V)))
				       (SETQ DIR-PRIMARY-DSKUSE
					(+ DIR-PRIMARY-DSKUSE (CDR V))))
			       (STATUS-USER-DSKUSE\PARSE-LINE STREAM
							      ALL-FLAG
							      NAME)))))))))

;;; STATUS-USER-DSKUSE\PARSE-LINE
;;;  Reads an individual line from the dir and returns a CONS whose 
;;;  CAR is user use and CDR is directory use by the file in question.
;;;  (Links count as 0). 
;;;
;;; STREAM= the file directory file object already opened and in position
;;; ALL= flag saying whether or not to count files that don't have an FN1
;;;      the same as the value of NAME
;;; NAME = a value to compare the FN1 to if ALL is non-NIL.

(DEFUN STATUS-USER-DSKUSE\PARSE-LINE (STREAM ALL NAME)
       (LET ((FN1 (STATUS-USER-DSKUSE\PARSE-LINE\AUX STREAM))
	     (SIZE (PROG2 (STATUS-USER-DSKUSE\PARSE-LINE\AUX STREAM)
			  (LET ((IBASE 10.)) (READ STREAM))
			  (READLINE STREAM))))
	    (COND ((OR ALL (EQ FN1 NAME))
		   (CONS SIZE SIZE))
		  (T
		   (CONS 0. SIZE)))))

;;; STATUS-USER-DSKUSE\PARSE-LINE\AUX
;;;  Reads 7 characters from STREAM, but only looks at first 6.
;;;  Implodes all 6 except for trailing spaces and returns as a symbol.
;;;
;;; STREAM= file object being read from.

(DEFUN STATUS-USER-DSKUSE\PARSE-LINE\AUX (STREAM)
       (DO ((C (TYI STREAM) (TYI STREAM))
	    (I 0. (1+ I))
	    (L () (CONS C L)))
	   ((> I 5.)
	    (DO ((L L (CDR L)))
		((NOT (= (CAR L) 32.)) (IMPLODE (NREVERSE L)))))))

;;; $FULLDISKUSE
;;;
;;; Syntax is:
;;;
;;;	FULLDISKUSE();
;;;
;;; which will default to the user's name, or
;;;
;;;	FULLDISKUSE(<name>);
;;;
;;; <name> will not be evaluted.
;;;
;;; Returns:
;;;
;;; [[TOTAL FREE BLOCKS, [<pack-type>, <pack-number>, <n> BLOCKS],
;;;			 [<pack-type>, <pack-number>, <n> BLOCKS], ...]
;;;  [DIRECTORY BLOCK USAGE, <directory-name>
;;;			     [PRIMARY, <n> BLOCKS],
;;;			     [SECONDARY, <n> BLOCKS]]
;;;  [USER BLOCK USAGE, <user-name>,
;;;			[PRIMARY, <n> BLOCKS],
;;;			[SECONDARY, <n> BLOCKS]]]
;;;
;;; If the user has his own directory, the last element of the list 
;;; (USER BLOCK USAGE) is omitted since it would be the same as 
;;; DIRECTORY BLOCK USAGE.
;;;
;;; <pack-type> ::= "PRIMARY PACK" or "SECONDARY PACK"
;;; <pack-number> ::= a fixnum
;;; <n> ::= a fixnum
;;;
;;; Occurances of `<n> BLOCKS' are in the form of a Macsyma 
;;; multiplication between a fixnum <n> and the Macsyma symbol BLOCKS.
;;;

(DEFUN $FULLDISKUSE FEXPR (SPECS)
       (DECLARE (SPECIAL NAME))
       (LET* ((USERNAME (COND ((ATOM SPECS)
			       (IMPLODE (CONS '$ (EXPLODEC (STATUS USERID)))))
			      (T
			       (CAR SPECS))))
	      (NAME (STRIPDOLLAR USERNAME))
	      (DIR (COND ((ATOM SPECS) (STATUS HSNAME))
			 (T (STATUS HSNAME NAME))))
	      (DIRNAME (IMPLODE (CONS '$ (EXPLODEC DIR))))
	      (ALL (COND ((EQ NAME DIR) T) (T ()))))
	     (IOTA ((STREAM `((DSK ,DIR) |.FILE.| |(DIR)|) '(IN ASCII)))
		   (LIST* '(MLIST SIMP)
			  (STATUS-GLOBAL-DSKUSE STREAM)
			  (STATUS-USER-DSKUSE NAME USERNAME
					      DIRNAME ALL STREAM)))))

;;; PRINTDISKUSE
;;;  Takes args just like FULLDISKUSE.
;;;  Prints in English nicely formatted the disk use for a user.

(DEFUN $PRINTDISKUSE FEXPR (X)
       (LET ((USAGE (APPLY '$FULLDISKUSE X))
	     (PRIMARY 0.)
	     (SECONDARY 0.)
	     (BASE 10.)
	     (*NOPOINT T))
	    (CURSORPOS 'A TYO)
	    (MAPCAR (FUNCTION
		     (LAMBDA (X)
			     (COND ((EQ (CADR X) '|&PRIMARY PACK|)
				    (SETQ PRIMARY
					  (+ PRIMARY (CADR (CADDDR X)))))
				   (T
				    (SETQ SECONDARY
					  (+ SECONDARY (CADR (CADDDR X))))))))
		    (CDDADR USAGE))
	    (PRINC (+ PRIMARY SECONDARY) TYO)
	    (PRINC '| Total Free Disk Blocks: | TYO)
	    (TERPRI TYO)
	    (PRINC '|  | TYO)
	    (PRINC PRIMARY TYO)
	    (PRINC '| on Primary Pack and | TYO)
	    (PRINC SECONDARY TYO)
	    (PRINC '| on Secondary Pack.| TYO)
	    (TERPRI TYO)
	    (LET ((DIRUSE (CDR (CADDR USAGE)))
		  (USRUSE (CDR (CADDDR USAGE))))
		 (PRINC '|Directory use by the | TYO)
		 (PRINC (STRIPDOLLAR (CADR DIRUSE)) TYO)
		 (PRINC '| directory:| TYO)
		 (TERPRI TYO)
		 (PRINC '|  | TYO)
		 (PRINC (CADR (CADDR (CADDR DIRUSE))) TYO)
		 (PRINC '| blocks on Primary Pack and | TYO)
		 (PRINC (CADR (CADDR (CADDDR DIRUSE))) TYO)
		 (PRINC '| blocks on Secondary Pack.| TYO)
		 (TERPRI TYO)
		 (COND ((CDDDR USAGE)
			(PRINC '|Usage by | TYO)
			(PRINC (STRIPDOLLAR (CADR USRUSE)) TYO)
			(PRINC '|:| TYO)
			(TERPRI TYO)
			(PRINC '|  | TYO)
			(PRINC (CADR (CADDR (CADDR USRUSE))) TYO)
			(PRINC '| blocks on Primary Pack and | TYO)
			(PRINC (CADR (CADDR (CADDDR USRUSE))) TYO)
			(PRINC '| blocks on Secondary Pack.| TYO)
			(TERPRI TYO))))
	    '$DONE))

;;; DISKFREE
;;;  DISKFREE(TRUE); or just DISKFREE(); returns total free blocks on
;;;   both packs.
;;;  DISKFREE(PRIMARY); returns blocks free on primary pack.
;;;  DISKFREE(SECONDARY); returns blocks free on secondary pack.
;;;  DISKFREE(<n>); returns the free blocks on pack <n>
;;; return value is a fixnum times the symbol blocks.

(DEFUN $DISKFREE FEXPR (X)
       (COND ((> (LENGTH X) 1.)
	      (CURSORPOS 'A TYO)
	      (PRINC '|;Too many args given to DISKFREE.| TYO)
	      (ERR)))
       (SETQ X (OR (CAR X) '$TRUE))
       (COND ((NOT (MEMBER X '($TRUE $PRIMARY $SECONDARY 0. 1. 13.)))
	      (CURSORPOS 'A TYO)
	      (PRINC '|;Illegal arg to DISKFREE| TYO)
	      (TERPRI TYO)
	      (PRINC '|;Valid args are 0, 1, 13, TRUE, PRIMARY, SECONDARY.|
		     TYO)
	      (ERR)))
       (IOTA ((STREAM '|DSK:USERS;.FILE. (DIR)| 'IN))
	     (READLINE STREAM)
	     (DO ((C (TYI STREAM) (TYI STREAM))
		  (DSKUSE 0.))
		 ((= C 13.) (MTIMES-BLOCKS DSKUSE))
		 (COND ((= C 35.)
			(LET ((PACK (STATUS-DSKUSE\AUX STREAM 61.))
			      (AMOUNT (STATUS-DSKUSE\AUX STREAM 32.)))
			     (COND ((OR (AND (MEMQ X '($PRIMARY $TRUE))
					     (NOT (= PACK 13.)))
					(AND (MEMQ X '($SECONDARY $TRUE))
					     (= PACK 13.))
					(EQUAL PACK X))
				    (SETQ DSKUSE (+ DSKUSE AMOUNT))))))))))

;;; DISKUSE
;;;  Returns the amount of disk space a user is taking up (in blocks)
;;;  as a fixnum times the symbol BLOCKS. Takes args like FULLDISKUSE.

(DEFUN $DISKUSE FEXPR (X)
       (MTIMES-BLOCKS
	(APPLY '+
	       (MAPCAR (FUNCTION (LAMBDA (X) (CADR (CADDR X))))
		       ((LAMBDA (INFO)
				(CDDDR (OR (CADDDR INFO)
					   (CADDR INFO))))
			(APPLY '$FULLDISKUSE X))))))
