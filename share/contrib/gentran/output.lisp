;=============================================================================
;    (c) copyright 1988	 Kent State University  kent, ohio 44242 
;		all rights reserved.
;
; Authors:  Paul S. Wang, Barbara Gates
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;=============================================================================

(include-if (null (getd 'wrs)) convmac.l)

(declare (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))
;;  -----------  ;;
;;  output.l     ;;    code formatting & printing
;;  -----------  ;;        and error handler


(declare (special poport fortlinelen* fortcurrind* ratlinelen*
	ratcurrind* clinelen* ccurrind* *cr* piport
	*errin* *errout* *slash*))
;;                                        ;;
;;  code formatting & printing functions  ;;
;;                                        ;;


;;  fortran code formatting & printing functions  ;;

(de formatfort (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatfort1 lst)))
	(wrs ch)))

(de formatfort1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt))) fortlinelen*)
			  (fortcontline)))
		   (prin2 elt))))))

(de fortcontline ()
  (progn
   (terpri)
   (prin2 "     .")
   (forttab (- fortcurrind* 6))
   (spaces 1)))

(de forttab (n)
  (progn
   (setq fortcurrind* (min (+ n 6) (- fortlinelen* 40)))
   (spaces (- fortcurrind* (posn)))))

;;  ratfor code formatting & printing functions  ;;

(de formatrat (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatrat1 lst)))
	(wrs ch)))

(de formatrat1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    ratlinelen*)
			  (ratcontline)))
		   (prin2 elt))))))

(de ratcontline ()
  (progn
   (terpri)
   (rattab ratcurrind*)
   (spaces 1)))

(de rattab (n)
  (progn
   (setq ratcurrind* (min n (- ratlinelen* 40)))
   (spaces (- ratcurrind* (posn)))))

;;  c code formatting & printing functions  ;;

(de formatc (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatc1 lst)))
	(wrs ch)))

(de formatc1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    clinelen*)
			  (ccontline)))
		   (prin2 elt))))))

(de ccontline ()
  (progn
   (terpri)
   (ctab ccurrind*)
   (spaces 1)))

(de ctab (n)
  (progn
   (setq ccurrind* (min n (- clinelen* 40)))
   (spaces (- ccurrind* (posn)))))


;;                             ;;
;;  general printing function  ;;
;;                             ;;


(de pprin2 (arg)
  (let ((ch (wrs nil)))
       (cond ((eq arg *cr*)
	      (foreach c in *outchanl* do
		       (progn (wrs c) (terpri))))
	     (t
	      (foreach c in *outchanl* do
		       (progn (wrs c) (prin2 arg)))))
       (wrs ch)))


;;                 ;;
;;  error handler  ;;
;;                 ;;


;;  error & warning message printing routine  ;;

(de gentranerr (msgtype exp msg1 msg2)
  (prog (holdich holdoch c)
	(setq holdich (rds *errin*))
	(setq holdoch (wrs *errout*))
	(terpri)
	(cond (exp
	       (prettyprint exp)))
	(cond ((equal msgtype 'e)
	       (progn
		(rds (cdr *stdin*))
		(wrs (cdr *stdout*))
		(rederr msg1))))
	(prin2 "*** ")
	(prin2t msg1)
	(cond (msg2
	       (repeat
		(cond ((not (member c (list '| |  *cr*)))
		       (progn
			(terpri)
			(prin2 "    ")
			(prin2 msg2)
			(prin2 (concat (concat " (y" *slash*) "n)  ")))))
		(member (setq c (readc)) '(y y n n)))))
	(wrs holdoch)
	(rds holdich)
	(cond ((member c '(n n))
	       (error1)))))
