;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1982 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module mload)

;; I decided to move most of the file hacking utilities I used in TRANSL to
;; this file. -GJC

;; Concepts:
;; Lisp_level_filename. Anything taken by the built-in lisp I/O primitives.
;;
;; User_level_filename. Comes through the macsyma reader, so it has an extra "&"
;;   in the pname in the case of "filename" or has extra "$" and has undergone
;;   ALIAS transformation in the case of 'FOOBAR or '[FOO,BAR,BAZ].
;;
;; Canonical_filename. Can be passed to built-in lisp I/O primitives, and
;;   can also be passed back to the user, is specially handled by the DISPLAY.
;;
;; Functions:
;; $FILENAME_MERGE. Takes User_level_filename(s) and Canonical_filename(s) and
;;   merges them together, returning a Canonical_filename.
;;
;; TO-MACSYMA-NAMESTRING. Converts a Lisp_level_filename to a Canonical_filename
;;
;; $FILE_SEARCH ............ Takes a user or canonical filename and a list of types of
;;                           applicable files to look for.
;; $FILE_TYPE   ............. Takes a user or canonical filename and returns
;;                            NIL, $MACSYMA, $LISP, or $FASL.
;; CALL-BATCH1 ............. takes a canonical filename and a no-echop flag.

;; Note: This needs to be generalized some more to take into account
;; the lispmachine situation of access to many different file systems
;; at the same time without, and also take into account the way it presently
;; deals with that situation. The main thing wrong now is that the file-default
;; strings are constants.

;; What a cannonical filename is on the different systems:
;; This is for informational purposes only, as the Macsyma-Namestringp
;; predicate is provided.
;; [PDP-10 Maclisp] An uninterned symbol with various properties.
;; [Franz Lisp] a string or a symbol (whose print name is used).
;; [Multics Maclisp] A STRING.
;; [LispMachine] A generic pathname object, which is a system-provided FLAVOR.
;; [NIL] Not decided yet, but a STRING should do ok, since in NIL files are
;; a low-level primitive, and programs, modules, and environments are the
;; practical abstraction used. No attempt is made to come up with ad-hoc generalizations
;; of the ITS'ish and DEC'ish filenames, as such attempts fail miserably to provide
;; the functionality of filesystems such as on Multics.

;------
;There is also this problem of what file searching and defaulting means,
; especially when this is done across systems.  My feeling right now
; is that searching might actually be three-layered:
; host, device/directory, type [assuming a name is given].  This will become
; important in the near future because NIL is going to be supporting
; multiple hosts over DECNET, using the common-lisp/lispm model of pathnames
; and hosts.  One of the problems with incremental merging algorithms like
; are used here is that making a pathname the first time tends to force an
; interpretation with respect to some specific host, which is probably
; defaulted from some place used for ultimate defaulting and not normally
; used by higher-level facilities like LOAD.  --gsb
;------

(declare-top (SPECIAL $FILE_SEARCH_LISP $FILE_SEARCH_MAXIMA $FILE_SEARCH_DEMO))


(DEFMFUN $LISTP_CHECK (VAR VAL)
  "Gives an MAXIMA-ERROR message including its first argument if its second
  argument is not a LIST"
  (OR ($LISTP VAL)
      (MERROR "The variable ~:M being set to a non-LISTP object:~%~M"
	      VAR VAL)))

(DEFPROP $FILE_SEARCH $LISTP_CHECK ASSIGN)

(DEFPROP $FILE_TYPES $LISTP_CHECK ASSIGN)

#-(or Franz cl)
(DEFMFUN $FILE_SEARCH (X &OPTIONAL
			 (consp NIL)
			 (L $FILE_TYPES))
  (SETQ X ($FILENAME_MERGE X))
  (IF ($LISTP L) (SETQ L (CDR L))
      (MERROR "3'rd arg to FILE_SEARCH not a list.~%~M" L))
  (DO ((MERGE-SPECS ;(CONS ($filename_merge)
		    ;	  ;; Get a complete "star filename"
		    ;	  (CDR $FILE_SEARCH))
		    ;What you want is not a complete star filename, but
		    ; rather the device/directory part of the default used for
		    ; LOAD etc.  You don't want the defaults reverting to the
		    ; super-defaults applied by the lowest level functions
		    ; which access the filesystem.  So, we define:
		    (cons ($load_search_dir) (cdr $file_search))
		    (CDR MERGE-SPECS))
       (PROBED)
       (FOUND)
       (TRIED))
      ((NULL MERGE-SPECS)
       (IF consp
	   `((MLIST) ,@(NREVERSE FOUND))
	   (MERROR "Could not MAXIMA-FIND file which matches ~M" X)))
    (IF (DO ((L L (CDR L)) (U ($FILENAME_MERGE (CAR MERGE-SPECS))))
	    ((NULL L) NIL)
	  (LET ((TRY ($FILENAME_MERGE X U (CAR L))))
	    (UNLESS (zl-MEMBER TRY TRIED)
	      (PUSH TRY TRIED)
	      (IF (SETQ PROBED (PROBE-FILE TRY))
		  (IF consp
		      (PUSH (TO-MACSYMA-NAMESTRING PROBED) FOUND)
		      (RETURN T))))))
	(RETURN (TO-MACSYMA-NAMESTRING PROBED)))))

#+cl
(defun lispm-merge-pathname (&rest l)
   (apply 'merge-pathnames  l))
 
;;; following worked but much harder to search.
;(defun $file_search (x &optional ign paths)
;  (or paths (setq paths ign))
;  (or paths (setq paths  $file_search))
;  (setq x (macsyma-namestring-sub x))
;  ;; Fix up $file_search
;  (and (cdr paths)
;       (or (pathnamep (second paths))
;	(setf (cdr paths)
;	      (mapcar  'macsyma-namestring-sub (cdr paths)))))
;  (sloop for v in (cdr paths)
;	 for tem = (merge-pathnames x v)
;	 when (probe-file tem)
;	 do (return tem)
;	 finally (return (merge-pathnames x))))

;(defun $file_search (x &optional ign paths)
;  (nth 1 ($file_search1 x ign paths)))
;
;(defun $file_search1 (x &optional ign paths)
;  (or paths (setq paths ign))
;  (or paths (setq paths  $file_search))
;  (setq x (macsyma-namestring-sub x))
;  ;; Fix up $file_search
;  (and (cdr paths)
;       (or (pathnamep (second paths))
;	(setf (cdr paths)
;	      (mapcar  'macsyma-namestring-sub (cdr paths)))))
;  (sloop for v in (cdr paths)
;	 for tem = (merge-pathnames x v)
;	 do 
;	 (setq tem (directory tem))
;	 (cond (tem (return (cons '(mlist) (nreverse tem)))))))



#-Franz
(defun $load_search_dir ()
  (to-macsyma-namestring *DEFAULT-PATHNAME-DEFAULTS*))


;; filename merging is unheard of on Unix.
;; If the user doesn't supply a file extension, we look for .o, .l and .v
;; and finally the file itself.  If the user supplies one of the standard
;; extensions, we just use that.
#+Franz
(defmfun $file_search (x &optional (consp nil) (l $file_types))
   (let ((filelist (cond ((cdr $file_search))
			 (t '("."))))
	 (extlist (cond ((zl-MEMBER (substring x -2) '(".o" ".l" ".v"))
			 '(nil))
			(t '(".o" ".l" ".v" nil)))))
      (do ((dir filelist (cdr dir))
	   (ret))
	  ((null dir)
	   (cond (consp '((mlist)))
		     (t (MERROR "Could not MAXIMA-FIND file ~M" X))))
	  (cond ((setq ret
		       (do ((try extlist (cdr try))
			    (this))
			   ((null try))
			   (setq this (cond ((null (car try)) x)
					    (t (concat x (car try)))))
			   (cond ((not (equal "." (car dir)))
				  (setq this (concat (car dir) "//" this))))
			   (cond ((probe-file this)
				  (return
				     (cond (consp `((mlist)
						    ,(to-macsyma-namestring x)))
						(t (to-macsyma-namestring this))))))))
		 (return ret))))))

			
(declare-top (SPECIAL $LOADPRINT))

(DEFMFUN LOAD-AND-TELL (FILENAME)
  (LOADFILE FILENAME
	    T ;; means this is a lisp-level call, not user-level.
	    $LOADPRINT))

#+PDP10
(PROGN 'COMPILE
;; on the PDP10 cannonical filenames are represented as symbols
;; with a DIMENSION-LIST property of DISPLAY-FILENAME.

(DEFUN DIMENSION-FILENAME (FORM RESULT)
  (DIMENSION-STRING (CONS #. double-quote-char (NCONC (EXPLODEN FORM) (LIST #. double-quote-char))) RESULT))

(DEFUN TO-MACSYMA-NAMESTRING (X)
  ;; create an uninterned symbol, uninterned so that
  ;; it will be GC'd.
  (SETQ X (PNPUT (PNGET (NAMESTRING X) 7) NIL))
  (PUTPROP X 'DIMENSION-FILENAME 'DIMENSION-LIST)
  X)

(DEFUN MACSYMA-NAMESTRINGP (X)
  (AND (SYMBOLP X) (EQ (GET X 'DIMENSION-LIST) 'DIMENSION-FILENAME)))

(DEFMACRO ERRSET-NAMESTRING (X)
  `(LET ((ERRSET NIL))
     (ERRSET (NAMESTRING ,X) NIL)))

(DEFMFUN $FILENAME_MERGE N
  (DO ((F "" (MERGEF (MACSYMA-NAMESTRING-SUB (ARG J)) F))
       (J N (f1- J)))
      ((ZEROP J)
       (TO-MACSYMA-NAMESTRING F))))
)

#+Franz
(progn 'compile

;; a first crack at these functions

(defun to-macsyma-namestring (x)
   (cond ((macsyma-namestringp x) x)
	 ((symbolp x)
	  (cond ((memq (getcharn x 1) '(#\& #\$))
		 (substring (get_pname x) 2))
		(t (get_pname x))))
	 (t (merror "to-macsyma-namestring: non symbol arg ~M~%" x))))

(defun macsyma-namestringp (x)
   (stringp x))

;;--- $filename_merge
; may not need this ask filename merging is not done on Unix systems.
;
(defmfun $filename_merge (&rest files)
   (cond (files (filestrip (ncons (car files))))))
)

#+MULTICS
(PROGN 'COMPILE
(DEFUN TO-MACSYMA-NAMESTRING (X) 
  (cond ((macsyma-namestringp x) x)
	((symbolp x) (substring (string x) 1))
	((consp x) (namestring x))
	(t x)))

(DEFUN MACSYMA-NAMESTRINGP (X) (STRINGP X))
(DEFUN ERRSET-NAMESTRING (X)
  (IF (ATOM X) (NCONS (STRING X)) (ERRSET (NAMESTRING X) NIL)))

(DEFMFUN $FILENAME_MERGE (&REST FILE-SPECS)
  (SETQ FILE-SPECS (cond (file-specs 
			  (MAPCAR #'MACSYMA-NAMESTRING-SUB FILE-SPECS))
			 (t '("**"))))
  (TO-MACSYMA-NAMESTRING (IF (NULL (CDR FILE-SPECS))
			     (CAR FILE-SPECS)
			     (APPLY #'MERGEF FILE-SPECS))))

)

#+(and (not cl) Lispm)
(PROGN 'COMPILE
(DEFUN TO-MACSYMA-NAMESTRING (X)
  (PATHNAME X)
  )
(DEFUN MACSYMA-NAMESTRINGP (X)
  (ml-typep X 'FS:PATHNAME))
(DEFUN ERRSET-NAMESTRING (X)
  (LET ((ERRSET NIL))
    (ERRSET (PATHNAME X) NIL)))

(DEFMFUN $FILENAME_MERGE (&REST FILE-SPECS)
  (SETQ FILE-SPECS (cond (file-specs 
			  (MAPCAR #'MACSYMA-NAMESTRING-SUB FILE-SPECS))
			 (t '("**"))))
  ($file_search
     (TO-MACSYMA-NAMESTRING (IF (NULL (CDR FILE-SPECS))
			     (CAR FILE-SPECS)
			     (APPLY #'MERGEF FILE-SPECS)))

  ))

) 

#+cl
(PROGN 'COMPILE
(DEFUN TO-MACSYMA-NAMESTRING (X)
  (PATHNAME X)
  )
(DEFUN MACSYMA-NAMESTRINGP (X)
  (typep X 'PATHNAME))
(DEFUN ERRSET-NAMESTRING (X)
  (LET ((ERRSET NIL))
    (ERRSET (PATHNAME X) NIL)))

(DEFMFUN $FILENAME_MERGE (&REST FILE-SPECS)
  (SETQ FILE-SPECS (cond (file-specs 
			  (MAPCAR #'MACSYMA-NAMESTRING-SUB FILE-SPECS))
			 (t '("**"))))

  (progn            ;$file_search
     (TO-MACSYMA-NAMESTRING (IF (NULL (CDR FILE-SPECS))
			     (CAR FILE-SPECS)
			     (APPLY #'MERGEF FILE-SPECS)))

  )
  
  )
) 


;
;#+NIL
;(PROGN 'COMPILE
;(DEFUN TO-MACSYMA-NAMESTRING (X)
;  (PATHNAME X))
;(DEFUN MACSYMA-NAMESTRINGP (X)
;  (ml-typep X 'PATHNAME))
;(DEFUN ERRSET-NAMESTRING (X)
;  (LET ((ERRSET NIL))
;    (ERRSET (PATHNAME X) NIL)))
;
;(DEFMFUN $FILENAME_MERGE (&RESTV FILE-SPECS)
;  ;For this crufty, it is better to just do the dumb component-by-component
;  ; merging rather than trusting the allegedly winning heuristics of
;  ; merge-pathname-defaults.
;  (do ((i (f1- (length file-specs)) (f1- i))
;       (p "" (merge-pathnames
;	       (macsyma-namestring-sub (svref file-specs i)) p)))
;      ((< i 0) (to-macsyma-namestring p))))
;) ;#+NIL

(DEFUN MACSYMA-NAMESTRING-SUB (USER-OBJECT)
  (IF (MACSYMA-NAMESTRINGP USER-OBJECT) USER-OBJECT
      (LET* ((SYSTEM-OBJECT
	      (COND ((AND (ATOM USER-OBJECT) (NOT (SYMBOLP USER-OBJECT)))
		     USER-OBJECT)
		    ((and (symbolp user-object)
			  (eql #\$ (getcharn user-object 1)))
		     (string-downcase (fullstrip1 user-object)))
		    ((ATOM USER-OBJECT)
		     (FULLSTRIP1 USER-OBJECT))
		    (($LISTP USER-OBJECT)
		     (FULLSTRIP (CDR USER-OBJECT)))
		    (T
		     (MERROR "Bad file spec:~%~M" USER-OBJECT))))
	     (NAMESTRING-TRY (ERRSET-NAMESTRING SYSTEM-OBJECT)))
	(IF NAMESTRING-TRY (CAR NAMESTRING-TRY)
	    ;; know its small now, so print on same line.
	    (MERROR "Bad file spec: ~:M" USER-OBJECT)))))

(DEFMFUN open-out-dsk (x)
  #+(or cl nil)
  (open x :direction :output :element-type 'character)
 #-(or CL NIL)   (open x '(out dsk ascii block)))

(DEFMFUN open-in-dsk (x)
	 #+(or cl nil)
	 (open x :direction :input :element-type 'character)
         #-(or CL NIL) (open x '(in dsk ascii block)))

#-MAXII
(PROGN 'COMPILE

(declare-top (SPECIAL DSKFNP OLDST ST $NOLABELS REPHRASE))

(DEFMFUN CALL-BATCH1 (FILENAME ^W)
  (LET ((^R (AND ^R (NOT ^W)))
	($NOLABELS T)
	($CHANGE_FILEDEFAULTS)
	(DSKFNP T)
	(OLDST)
	(ST))
    ;; cons #/& to avoid the double-stripdollar problem.
    (BATCH1 (LIST (MAKNAM (CONS #\& (EXPLODEN FILENAME))))
	    NIL
	    NIL
	    #-Franz T
	    #+Franz nil)
    (SETQ REPHRASE T)))


(DEFMVAR *IN-$BATCHLOAD* NIL
  "I should have a single state variable with a bit-vector or even a list
  of symbols for describing the state of file translation.")
(DEFMVAR *IN-TRANSLATE-FILE* NIL "")
(DEFMVAR *IN-MACSYMA-INDEXER* NIL)

(DEFUN TRANSLATE-MACEXPR (FORM &optional FILEPOS)
       (COND (*IN-TRANSLATE-FILE*
	      (TRANSLATE-MACEXPR-ACTUAL FORM FILEPOS))
	     (*in-macsyma-indexer*
	      (outex-hook-exp form))
	     (T
	      (LET ((R (ERRSET (MEVAL* FORM))))
		   (COND ((NULL R)
			  (LET ((^W NIL))
			       (MERROR "~%This form caused an MAXIMA-ERROR in evaluation:~
				       ~%~:M" FORM))))))))
)




;(DEFMFUN $BATCHLOAD (FILENAME &aux)
;  (LET ((WINP NIL)
;	(NAME ($FILENAME_MERGE FILENAME))
;	(*IN-$BATCHLOAD* T))
;    (IF $LOADPRINT
;	(MTELL "~%Batching the file ~M~%" NAME))
;    (UNWIND-PROTECT
;     (PROGN (CALL-BATCH1 NAME T)
;	    (SETQ WINP T)
;	    NAME)
;     ;; unwind protected.
;     (IF WINP
;	 (IF $LOADPRINT (MTELL "Batching done."))
;	 (MTELL "Some MAXIMA-ERROR in loading this file: ~M" NAME)))))
;
;;; end of moby & crufty #-MAXII
;)
;
;#+(and MAXII (not cl))
;(DEFMFUN $BATCHLOAD (FILENAME)
;  (LET ((EOF (LIST NIL))
;	(NAME ($FILENAME_MERGE FILENAME))
;	(*MREAD-PROMPT* "(Batching) "))
;    ;I'm not about to try and figure out why the first ~% in this is a no-op.
;    ; MFORMAT is about as opaque as i have ever seen.
;    ;(IF $LOADPRINT (MTELL "~%Batching the file ~M~%" NAME))
;    (when $loadprint (format t "~&Batching the file ~A.~%" name))
;    (WITH-OPEN-FILE (STREAM NAME '(:IN :ASCII))
;      (DO ((FORM NIL (MREAD STREAM EOF)))
;	  ((EQ FORM EOF)
;	   ;(IF $LOADPRINT (MTELL "Batching done."))
;	   (when $loadprint (format t "~&Batching done.~%"))
;	   '$DONE)
;	(MEVAL* (CADDR FORM))))))



#+cl
(defun $batchload (filename &aux expr (*mread-prompt* "") )
  (setq filename ($file_search1 filename '((mlist) $file_search_maxima)))
  (with-open-file (in-stream filename)
		  (when $loadprint (format t "~&batching ~A"
					   (lisp::namestring
					    (truename in-stream))))
		  (cleanup)
		  (newline in-stream #\n)
		  (sloop while (and
				(setq  expr (mread in-stream nil))
				(consp expr))
			 do (meval* (third expr)))
		  (lisp::namestring (truename in-stream))))

;;returns appropriate error or existing pathname.
;; the second argument is a maxima list of variables
;; each of which contains a list of paths.   This is
;; so users can correct the variable..
(defun $file_search1 (name search-lists &aux lis)
  (setq lis (apply '$append (mapcar 'symbol-value (cdr search-lists))))
  (let ((res ($file_search name lis)))
    (or res
	(merror "Could not find `~M' using paths in ~A (combined values: ~M )"
		name    (string-trim "[]" ($sconcat search-lists)) lis))))

(DEFMFUN $LOAD (filename)
  "This is the generic file loading function.
  LOAD(filename) will either BATCHLOAD or LOADFILE the file,
  depending on wether the file contains Macsyma, Lisp, or Compiled
  code. The file specifications default such that a compiled file
  is searched for first, then a lisp file, and finally a macsyma batch
  file. This command is designed to provide maximum utility and
  convenience for writers of packages and users of the macsyma->lisp
  translator."

  (cond ((and (symbolp filename) (not (mstringp filename)))
	 (setq filename (string-downcase (symbol-name (stripdollar filename))))))
  (LET ((SEARCHED-FOR
	 ($file_search1 filename
			'((mlist) $file_search_maxima $file_search_lisp  )))
	 type)
	(setq TYPE ($FILE_TYPE SEARCHED-FOR))
	(CASE TYPE
	      (($MAXIMA)
	       ($BATCHLOAD SEARCHED-FOR))
	      (($LISP $OBJECT)
	       ;; do something about handling errors
	       ;; during loading. Foobar fail act errors.
	       (LOAD-AND-TELL SEARCHED-FOR))
	      (T
	       (MERROR "MACSYMA BUG: Unknown file type ~M" TYPE)))
	searched-for
	))

;#+Multics ;; not so many multics machines around today.
;(DEFMFUN $FILE_TYPE (FILE)
;  (SETQ FILE ($FILENAME_MERGE FILE))
;  (IF (NULL (PROBE-FILE FILE)) NIL
;      (CASE (CAR (LAST (NAMELIST FILE)))
;	((MACSYMA) '$MACSYMA)
;	((LISP) '$LISP)
;	(T '$FASL))))

#-(or MULTICS NIL cl)
(DEFMFUN $FILE_TYPE (FILENAME &AUX STREAM)
  (SETQ FILENAME ($FILENAME_MERGE FILENAME))
  (COND ((NULL (PROBE-FILE FILENAME))
	 NIL)
#-Franz ((FASLP FILENAME)
	 '$FASL)
#+Franz ((cdr (zl-ASSOC (substring filename -2)
		     '((".l" . $lisp) (".o" . $fasl) (".v" . $macsyma)))))
	('ELSE
	 ;; This has to be simple and small for greatest utility
	 ;; as an in-core pdp10 function.
	 (UNWIND-PROTECT
	  (DO ((C (PROGN (SETQ STREAM (OPEN-IN-DSK FILENAME))
			 #\SPACE)
		  (TYI STREAM -1)))
	      ((NOT (zl-MEMBER C '(#\SPACE #\TAB #\Return #\Linefeed #\Page)))
	       ;; heuristic number one,
	       ;; check for cannonical language "comment." as first thing
	       ;; in file after whitespace.
	       (COND ((zl-MEMBER C '(-1 #. semi-colon-char))
		      '$LISP)
		     ((AND (= C #. forward-slash-char)
			   (= (TYI STREAM -1) #\*))
		      '$MACSYMA)
	   #+Franz   ((eq c 7)		;; fasl files begin with bytes 7,1
		      '$fasl)		;; but just seeing 7 is good enough
		     ('ELSE
		      ;; the above will win with all Lisp files written by
		      ;; the macsyma system, e.g. the $SAVE and
		      ;; $TRANSLATE_FILE commands, all lisp files written
		      ;; by macsyma system programmers, and anybody else
		      ;; who starts his files with a "comment," lisp or
		      ;; macsyma.
		      (FILEPOS STREAM 0)
		      ;; heuristic number two, see if READ returns something
		      ;; evaluable.
		      (LET ((FORM (LET ((ERRSET NIL))
				    ;; this is really bad to do since
				    ;; it can screw the lisp programmer out
				    ;; of a chance to identify read errors
				    ;; as they happen.
				    (ERRSET (READ STREAM NIL) NIL))))
			(IF (OR (NULL FORM)
				(ATOM (CAR FORM)))
			    '$MACSYMA
			    '$LISP))))))
	  ;; Unwind protected.
	  (IF STREAM (CLOSE STREAM))))))




(defun $mkey (keyword)
    "takes a macsyma symbol and makes a keyword of it"
  (intern (string-left-trim "$" (string-upcase (string keyword))) 'keyword))


(defun quote-simple-equal (f g)
  (and (consp f) (eql (caar f) 'mquote)
       (cond ((atom g) (equal (second f) g))
	     ((not (eql (caar g) 'mquote))
	      ($simple_equal (second f) g))
	     (t ($simple_equal (cdr f) (cdr g))))))

(defun unequal-pairlis (var gen)
  (sloop for v in var
	for u in gen
	collecting (cons v u)))

(defun $simple_equal (f g)
  "checks if equal up to simp flags"
  (cond	((quote-simple-equal f g))
	((quote-simple-equal g f))
	((and  (numberp f) (numberp g)
	       (or (eql f g))
	       (< (- f g) 1.0e-4)))
        ((atom f) (equal f g))
	((atom g) nil)
	((eql (caar f) 'mrat)
	 (equal(fifth (car f)) (fifth (car g)))
	 (equal (sublis (unequal-pairlis  (third (car f))
				 (fourth (car f)))
			(cdr f))
		 (sublis (unequal-pairlis (third (car g))
				 (fourth (car g)))
			(cdr g))))
	(t(and (equal (caar f) (caar g))
	       (eql (length f) (length g))
	       (sloop for v in (cdr f)
		     for w in (cdr g)
		     when (not ($simple_equal v w))
		       do (return nil)
			  finally (return t))))))


#+cl
(defun $file_type (fil &aux typ)
  (setq fil ( pathname	      fil))
  (setq typ (format nil "~(~A~)" (pathname-type fil)))
  (or 
   (and (> (length typ) 0)
       (let ((ch (aref typ 0)))
	 (cdr (assoc ch '( (#\m . $maxima) (#\d . $maxima) (#\l . $lisp)
			(#\o . $object)
			(#\f . $object))))))
   '$object))
       			

#+NIL
(DEFVAR *EDITOR-MODE-TO-MACSYMA-FILE-TYPE-TABLE*
  '((:MACSYMA . $MACSYMA)
    (:LISP . $LISP)
    (:LSB . $LISP)))

#+NIL
(DEFVAR *PATHNAME-TYPE-TO-MACSYMA-FILE-TYPE-TABLE*
  '(("LISP" . $LISP)
    ("MC" . $MACSYMA)	;In case the pathname code doesn't know this inversion
    ("MACSYMA" . $MACSYMA)
    ("VASL" . $FASL)))

#+NIL
(DEFMFUN $FILE_TYPE (FILENAME &AUX TEM DISEMBODIED)
  (SETQ FILENAME ($FILENAME_MERGE FILENAME))
  (COND ((NULL (SETQ DISEMBODIED (SEND (PATHNAME FILENAME) :FILE-PLIST))) NIL)
	((GET DISEMBODIED :VASLP) '$FASL)
	((CDR (ASSQ (GET DISEMBODIED :MODE)
		    *EDITOR-MODE-TO-MACSYMA-FILE-TYPE-TABLE*)))
	((CDR (SYS:ASSOC (SEND (CAR DISEMBODIED) :TYPE)
			 *PATHNAME-TYPE-TO-MACSYMA-FILE-TYPE-TABLE*
			 :TEST #'STRING-EQUAL)))))

#-cl
(DEFMVAR $FILE_SEARCH
  '((MLIST)))

#-cl
(defun initialize-$file_search ()
  (setq $file_search
	#+ITS
	`((MLIST)
	  ,@(MAPCAR #'TO-MACSYMA-NAMESTRING
		    `(,`((dsk ,(status homedir)))
		      "DSK:SHARE;" "DSK:SHARE1;" "DSK:SHARE2;" "DSK:SHAREM;")))
	#+NIL
	`((mlist) ,@(let ((wdir (user-workingdir-pathname))
			  (hdir (user-homedir-pathname)))
		      (cons wdir (and (not (equal wdir hdir)) (list hdir))))
		  ,(to-macsyma-namestring "max$disk:[share]")
		  ,(to-macsyma-namestring "max$disk:[share1]")
		  ,(to-macsyma-namestring "max$disk:[share2]")
		  ,(to-macsyma-namestring "max$disk:[sharem]"))
	#+Franz
	`((mlist)
	  ,@(mapcar #'to-macsyma-namestring
		    `("."
		      ,(concat vaxima-main-dir "//share")
		      ,(concat vaxima-main-dir "//demo"))))
	#+CL
	`((MLIST))
	
	#+Multics
	  (LET ((WHERE-AM-I (CAR (NAMELIST EXECUTABLE-DIR))))
	    `((MLIST) 
	      ,@(mapcar 
		 #'to-macsyma-namestring
		 `(,(string-append (PATHNAME-UTIL "hd") ">**")
		   ,(string-append (NAMESTRING `(,WHERE-AM-I "share")) ">**")
		   ,(string-append (NAMESTRING `(,WHERE-AM-I "executable"))
				   ">**")))))))

(defvar *macsyma-startup-queue* nil)
;(push '(initialize-$file_search) *macsyma-startup-queue*)

(eval-when (compile) (proclaim '(special *mread-prompt*)))


;; Done for debuggings sake.
;(eval-when (eval load)  (initialize-$file_search))

(defmfun mfilename-onlyp (x)
  "Returns T iff the argument could only be reasonably taken as a filename."
  (cond ((macsyma-namestringp x) t)
	(($listp x) t)
	((symbolp x)
	 (char= #\& (getcharn x 1)))
	('else
	 nil)))


;;;; batch & demo search hacks

#+cl
(defun $batch (filename &optional (demo :batch)
			&aux tem   (possible '(:demo :batch :test)))
  "giving a second argument makes it use demo mode, ie pause after evaluation
   of each command line"
  (cond ((setq tem (memq ($mkey demo) possible))
	 (setq demo (car tem)))
	(t (format t "Second arg ~A is not in ~A so using :Batch"
		   demo possible)))

  (setq filename ($file_search1 filename
				(if (eql demo :demo)
				    '((mlist) $file_search_demo )
				  '((mlist) $file_search_maxima ))))
  (cond ((eq demo :test)
	 (test-batch filename))
	(t
	 (with-open-file (in-stream filename)
	   (format t "~%batching ~A"
		(truename in-stream))
	   (continue in-stream demo)
	   (namestring in-stream)))))

(defvar *collect-errors* t)

;(defun you-supply (description &key return-string 
;				  default-value (stream *query-io*) &aux answ )
;   (cond (return-string (format stream "~%Supply a value for  ~A: " description))
;	 (t (format stream "~%Supply a form to evaluate to use for ~A: " description)))
;;  (apply 'format stream format-string format-arg-list)
;  (cond (default-value (format stream "(default is: ~A)" default-value)))
;  (setq answ (read-line stream))
;  (cond ((equal answ "")default-value)
;	(return-string answ)
;	(t (eval (read-from-string answ)))))

;(defun query-pathname (&key make-pathname-options (description "the pathname") (default-pathname "foo.lisp"))
;  (setq name (pathname (you-supply description :default-value (apply 'alter-pathname default-pathname make-pathname-options)
;				   :return-string t)))
;	  (apply 'alter-pathname name make-pathname-options))
	

(defun test-batch (filename &optional (out *standard-output*) &aux result next-result  next eof error-log all-differences
			    (*mread-prompt* "")
			    ($matrix_element_mult '&*))
  (cond (*collect-errors*
	 (setq error-log
	       (if (streamp *collect-errors*) *collect-errors*
		 (open (alter-pathname filename :type "ERR")
		       :direction :output)))
	 (format t "~%Error log on ~a" error-log)
	 (format error-log "~%/*    MAXIMA-ERROR log for testing of ~A" filename)
	 (format error-log "*/~2%")))
  
  (unwind-protect 
      (with-open-file
       (st filename)
       (sloop with expr 
	      for i from 1
	      do
	      (setq expr (mread st eof))
	      while expr 
	      do
	      (format out "~%/* ********************** Problem ~A. *************** */ " i)
					;      (mshow expr)
	      (format out "~%%Input is" )
	      (displa (third expr))
	      (setq $%(setq result  (meval* (third expr))))
	      (format out "~%~%The result is")
	      (displa $%)
	      (setq next (mread st eof))
	      (cond ((null next) (error "no result")))
	      (setq next-result  (third next))
	      (cond ((batch-equal-check next-result result)
		     (format t "~%..Which was correct"))
		    ((eq t next-result)
		     (format t "~%..Which made the predicate true"))
		    (t (format t "~%This differed from the expected result:")
		       (push i all-differences)
		       (displa next-result)
		       (cond (*collect-errors*
			      (mgrind (third expr) error-log)
			      (list-variable-bindings (third expr) error-log)
			      (format error-log ";~%")
			      (format error-log "//*Erroneous Result?:~%")
			      (mgrind result error-log) (format error-log "*// ")
			      (terpri error-log)
			      (mgrind next-result error-log)
			      (format error-log ";~%~%"))))
		    )))
    (cond (error-log
	   (or (streamp *collect-errors*)
	       (close error-log)))))
  (cond ((null all-differences)
	 (format t "~%Congratulations: No differences!") '((mlist)))
	(t (format t "~%The number of differences found was ~A in problems: ~A" (length all-differences)
		   all-differences)
	   `((mlist),filename ,@ all-differences))))
	   

(defun batch-equal-check (next-result result &optional recursive)
  (or (like next-result result)
      ($simple_equal next-result result)
      (cond ((not
	      (or recursive ($simple_equal result $functions)))
	     (batch-equal-check (meval* next-result)  result ;(meval* result)
				t )))
      (and (not ($bfloatp result))
	   (let (($fpprec 12))
	     (declare (special $fpprec))
	     (equal (mstring result) (mstring next-result))
	     ))

	     
      (cond ((not  (appears-in result 'factored))
	     (format t "~%Using ratsimp")(like ($ratsimp next-result)
						   ($ratsimp result))))
      (equal 0 ($ratsimp `((mplus) ,next-result ((mtimes) -1 ,result))))
      (equal (msize result nil nil nil nil )
	     (msize next-result nil nil nil nil))

      ))

;;to keep track of global values during the error:
(defun list-variable-bindings (expr &optional str &aux tem)
  (sloop for v in(cdr ($listofvars  expr))
	when (zl-MEMBER v $values)
	collecting (setq tem`((mequal) ,v ,(meval* v)))
	    and
	do (cond (str (format str ",")(mgrind tem str)))))

;;in init_max
;; name = foo or foo.type or dir/foo.type or dir/foo 
;; the empty parts are filled successively from defaults in templates in
;; the path.   A template may use multiple {a,b,c} constructions to indicate
;; multiple possiblities.  eg foo.l{i,}sp or foo.{dem,dm1,dm2}
(defun $file_search (name &optional paths)
  (if (and (symbolp name)
	   (member (getcharn name 1) '(#\& #\$)))
      (setq name ($sconcat name)))
  (if (symbolp name)  (setf name (string name)))
  (if (probe-file name) (return-from $file_search name))
  (or paths (setq paths ($append $file_search_lisp  $file_search_maxima
				 $file_search_demo)))
  (atomchk paths '$file_search t)
  (new-file-search (string name) (cdr paths)))

(defun new-file-search (name template &aux lis temp)
  (cond ((probe-file name))
	((atom template)
	 (setq template (namestring ($filename_merge template  name)))
	 ;(print (list 'template template))
	 (setq lis 
      	  (sloop for w in (split-string template "{}")
				     when (null (position #\, w))
				     collect w
				     else
				     collect (split-string w ",")))
	 (new-file-search1 "" lis))
	(t (sloop for v in template
		  when (setq temp (new-file-search name v))
		  do (return temp)))))

(defun new-file-search1 (begin lis )
  (cond ((null lis) (if (probe-file begin) begin nil))
	((atom (car lis))
	 (new-file-search1 (if begin
			       ($sconcat begin (car lis)) (car lis))
			   (cdr lis)))
	(t (sloop for v in (car lis) with tem
		  when (setq tem  (new-file-search1 begin (cons v (cdr lis))))
		  do (return tem)))))

(defun save-linenumbers (&key (c-lines t) d-lines (from 1) (below $linenum) a-list
			      (file  "/tmp/lines")
			      &aux input-symbol (linel 79))
  (cond ((null a-list) (setq a-list(sloop for i from from below below collecting i))))
  (with-open-file  (st file  #-cl '( :out) #+cl :direction #+cl :output  )
    (format st "/* -*- Mode: MACSYMA; Package: MACSYMA -*- */")
    (format st "~%~%       /*    ~A     */  ~%"
	    #+lispm (time::print-current-time nil)
	    #-lispm (let ((tem (cdddr
				(multiple-value-list (get-decoded-time)))))
		      (format nil "~a:~a:~a" (car tem) (cadr tem) (caadr tem)))
	    )
 	   (sloop for i in a-list
		 when (and c-lines (boundp (setq input-symbol (intern (format nil "$C~A" i)))))
		 do
		 (format st "~% C~3A;  "   i)
		 (mgrind (symbol-value input-symbol)
			 st)
		 (format st ";")
		 when (and d-lines
		 (boundp (setq input-symbol (intern (format nil "$D~A" i)))))
		 do
		 (format st "~% D~3A:  "   i)
		 (mgrind (symbol-value
			   input-symbol)
			 st)
		 (format st "$"))))


(defun $printfile (file)
  (setq file ($file_search1 file '((mlist) $file_search_usage)))
  (with-open-file (st file)
		  (sloop while (setq tem (read-char st)) with tem
			 do
			 (princ tem))
		  (namestring file)
		  ))

  
  
