;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module mudoc)

;; Macsyma user Documentation.
;; 8/30/81 16:52:50 -GJC

(defvar apropos-search)
(defvar apropos-string-length)
(defvar apropos-found)

(defmspec $apropos (form)
  (let ((apropos-search (mapcar #'(lambda (u) (format nil "~A" (fullstrip1 u))) (cdr form))))
    (cond ((null apropos-search)
	   (format *standard-output*
		   "~
                    ~%APROPOS takes arguments which should be a symbols or strings.~
                    ~%It searches the symbol table, returning a list of all symbols~
                    ~%which contain the arguments as substrings of their print name.~%")
	   '((MLIST)))
	  ('ELSE
	   (let ((apropos-found ())
		 (apropos-string-length (apply 'max (mapcar #'length apropos-search))))
	     (setq apropos-string-length (f1+ apropos-string-length))
	     (mapatoms #'(lambda (ssymbol)
			   (LET ((sSTRING (SYMBOL-NAME SSYMBOL)))
			     (if (and (>= (length (the string  sstring)) apropos-string-length)
				      (memq (character sstring) '(#\$ #\% #\&)))
				 (do ((l apropos-search (cdr l)))
				     ((null l)
				      (push ssymbol apropos-found))
				   (or (string-search (car l) sstring 1)
				       (return nil))))))
		       ;; Only search the MACSYMA package.
		       'MACSYMA
		       ;; Don't bother searching its superiors, i.e. GLOBAL.
		       NIL)
	     `((MLIST),@(SORT APROPOS-FOUND #'STRING-LESSP)))))))


;; The DESCRIBE datafile contains a large body of text which should
;; be randomly accesable.

(DEFvar DESCRIBE-DATA-FILE              
	  #+ITS "MC:LMMAXR;DOCDAT >"
	  #+cl "macsyma-object:documentation;docdat lisp"
	  ())

(DEFvar DESCRIBE-DATA-FILE-OPEN-OPTIONS '(:FIXNUM :BYTE-SIZE 8.))

;; The format of the data-file is as follows:
;; [1] The at :SET-POINTER of (- :LENGTH 25.) a lisp-readable FIXNUM
;;     giving the :SET-POINTER of the DOC-ALIST
;; [2] The lisp-readable DOC-ALIST, of the form
;;     (("INTEGRATE" (FILEPOS . LENGTH) (FILEPOS . LENGTH) ...)
;;      ("FROBULATE" (FILEPOS . LENGTH))
;;      ...)
;; [3] Starting from the begining of the file, a string of text, referenced
;;     by using :SET-POINTER, and having no special syntax or pointers.

(DEFVAR DESCRIBE-ALIST NIL)
(DEFVAR DESCRIBE-FILE  NIL)

(DEFUN LOAD-DESCRIBE-ALIST-IF-NEEDED ()
  (IF (NOT (AND DESCRIBE-FILE (PROBE-FILE DESCRIBE-FILE)))
      (WITH-OPEN-FILE (STREAM DESCRIBE-DATA-FILE
			      (CONS ':IN DESCRIBE-DATA-FILE-OPEN-OPTIONS))
	(FORMAT *standard-output*
		"~&; Loading describe database, please stand by...")
	(FUNCALL STREAM ':SET-POINTER (- (FUNCALL STREAM ':LENGTH) 25.))
	(FUNCALL STREAM ':SET-POINTER (READ STREAM))
	(SETQ DESCRIBE-ALIST (READ STREAM))
	(FORMAT *standard-output*
		"~&; Loading of describe database done.~%")
	(SETQ DESCRIBE-FILE (FUNCALL STREAM ':TRUENAME)))))

#-cl
(DEFMSPEC $DESCRIBE (FORM)
  (IF (NULL (CDR FORM))
      (INTERNAL-$DESCRIBE "DESCRIBE")
      (APPLY #'INTERNAL-$DESCRIBE
	     (MAPCAR #'(LAMBDA (X)
			 (STRING-UPCASE (FORMAT NIL "~A" (FULLSTRIP1 X))))
		     (CDR FORM)))))

(DEFUN INTERNAL-$DESCRIBE (&REST L)
  (LOAD-DESCRIBE-ALIST-IF-NEEDED)
  (WITH-OPEN-FILE (STREAM DESCRIBE-DATA-FILE
			  (CONS ':IN DESCRIBE-DATA-FILE-OPEN-OPTIONS))
    (DO ()
	((NULL L))
      (PRINT-DESCRIPTIONS (POP L) STREAM))))

(DEFUN PRINT-DESCRIPTIONS (SsTRING INPUT-STREAM)
  (DO ((L (LET ((INFO (zl-ASSOC SSTRING DESCRIBE-ALIST)))
	      (COND (INFO (CDR INFO))
		    ('ELSE
		     (FORMAT *standard-output* "~&No information for ~A~%" sSTRING)
		     NIL)))
	    (CDR L)))
	((NULL L))
      (FUNCALL INPUT-STREAM ':SET-POINTER (CAAR L))
      (DO ((J 0 (f1+ J))
	   (C #\Return (FUNCALL INPUT-STREAM ':TYI)))
	  ((> J (CDAR L)))
	(FUNCALL *standard-output* ':TYO C))))


(DEFvar describe-master-file
	  #+ITS "MC:MANUAL;MACSYM DOC"
	  #+cl "macsyma-object:documentation;macsyma documentation"
	  ())

;; The present format of the documentation file is as follows:
;; [1] it is fully character-at-time oriented, not line-at-time.
;; [2] A #/& character marks the END of a documentation section,
;;     unless quoted by a #^Q.
;; [3] The first thing in a documentation section is a key, readable
;;     with lisp READ. The PRINC representation of this object is the
;;     PRINC representation of the cooresponding macsyma object
;;     which has been FULLSTRIP1'd.

(DEFUN READ-DOC-KEY (INPUT-STREAM EOF)
  (LET ((READ-PRESERVE-DELIMITERS T))
    (READ INPUT-STREAM EOF)))

(DEFMFUN $UPDATE_DESCRIBE_DATA_FILE ()
  ;; Provide a user entry point for convenience
  (FORMAT *standard-output*
	  "~&; Using master documentation file ~S, to construct~
           ~%; data file ~S for lispmachine macsyma documentation.~%"
	  describe-master-file
	  describe-data-file)
  (cond ((or (zl-MEMBER user-id '("ELLEN" "JPG" "GJC" "CWH" "LMMAX" "LISPM"))
	     (yes-or-no-p "Are you a Macsyma System-Maintainer? "))
	 (update-describe-data-file)
	 (setq describe-file nil)
	 `((mlist) ,(probe-file describe-master-file) ,(probe-file describe-data-file)))
	('else
	 "You don't need to use this command then.")))

(DEFUN UPDATE-DESCRIBE-DATA-FILE ()
  (WITH-OPEN-FILE (INPUT-STREAM DESCRIBE-MASTER-FILE '(:IN :ASCII))
    (WITH-OPEN-FILE (OUTPUT-STREAM DESCRIBE-DATA-FILE
				   (CONS ':OUT DESCRIBE-DATA-FILE-OPEN-OPTIONS))
      (BEGIN-UPDATE-DESCRIBE-FILE INPUT-STREAM OUTPUT-STREAM)
      (DO ((DOC-ALIST NIL)
	   (DOC-CELL  NIL)
	   (DOC-START-POS)
	   (EOF "**EOF**")
	   (C #\& (FUNCALL INPUT-STREAM ':TYI NIL)))
	  ((NULL C)
	   (ERROR () "End of file inside documentation for ~S" (CAR DOC-CELL)))
	(COND ((= C #.(LOGAND #o77 #+cl (char-int #\Q) #-cl #\Q))
	       ;; From a pdp-10 file this is how control-Q reads.
	       (FUNCALL OUTPUT-STREAM ':TYO (FUNCALL INPUT-STREAM ':TYI)))
	      ((= C #\&)
	       (LET ((DOC-END-POS (FUNCALL OUTPUT-STREAM ':READ-POINTER)))
		 (IF DOC-CELL
		     (NCONC DOC-CELL (LIST (CONS DOC-START-POS
						 (f- DOC-END-POS DOC-START-POS)))))
		 (SETQ DOC-START-POS DOC-END-POS))
	       (LET* ((KEY (READ-DOC-KEY INPUT-STREAM EOF))
		      (PKEY (FORMAT NIL "~A" KEY))
		      (CELL (zl-ASSOC PKEY DOC-ALIST)))
		 (IF (EQ KEY EOF)
		     (RETURN (END-UPDATE-DESCRIBE-FILE OUTPUT-STREAM
						       (SORTCAR DOC-ALIST #'STRING-LESSP))))
		 (PRIN1 KEY OUTPUT-STREAM)
		 (SETQ DOC-CELL (IF CELL
				    CELL
				    (CAR (SETQ DOC-ALIST
					       (CONS (LIST PKEY) DOC-ALIST)))))))
	      ('ELSE
	       (FUNCALL OUTPUT-STREAM ':TYO C)))))))

(DEFUN BEGIN-UPDATE-DESCRIBE-FILE (INPUT-STREAM OUTPUT-STREAM)
  (FORMAT OUTPUT-STREAM
	  "~
           ~%***********************************************************************~
           ~%**            Macsyma User-Documentation Datafile                    **~
           ~%**      (c) Copyright 1981 Massachusetts Institute of Technology     **~
           ~%** Created by ~10A from master file ~26A **~
           ~%** on ~62A **~
           ~%***********************************************************************~
           ~%"
	  USER-ID (FUNCALL (FUNCALL INPUT-STREAM ':TRUENAME) ':STRING-FOR-HOST)
	  (TIME:PRINT-CURRENT-DATE NIL)))

(DEFUN END-UPDATE-DESCRIBE-FILE (OUTPUT-STREAM DOC-ALIST)
  (FORMAT OUTPUT-STREAM
	  "~3%This is the end, my only friend, the end.~
           ~%    - From `The END' by the CARS.~2%")
  (LET ((ALIST-START-POS (FUNCALL OUTPUT-STREAM ':READ-POINTER)))
    (PRINC "   " OUTPUT-STREAM)
    (LET ((*print-base* 10.)
	  (*NOPOINT NIL)
	  (PRINLEVEL NIL)
	  (PRINLENGTH NIL))
      (PRIN1 DOC-ALIST OUTPUT-STREAM))
    (FORMAT OUTPUT-STREAM "  ~25D. " ALIST-START-POS)))

(defun mgrind-string (&rest l)
  (WITH-OUTPUT-TO-STRING (STREAM)
    (DO ((L L (CDR L)))
	((NULL L))
      (TERPRI STREAM)
      (MGRIND (CAR L) STREAM)
      (TERPRI STREAM))))

(DEFMFUN $BUG (&REST L)
  (LET ((S (apply #'mgrind-string l)))
    (LET ((ZWEI:*HOST-FOR-BUG-REPORTS* "MIT-MC"))
      (BUG 'LMMAX S))))

(defun $mail (who &rest l)
  (let ((s (apply #'mgrind-string l)))
    (mail (fullstrip1 who) s)))


