


;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************

(when (null (fboundp 'wrs)) (load "convmac.lisp"))

(declare-top (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))
;;  -----------  ;;
;;  output.l     ;;    code formatting & printing
;;  -----------  ;;        and error handler


(declare-top (special poport fortlinelen* fortcurrind* ratlinelen*
	ratcurrind* clinelen* ccurrind* *cr* piport
	*errin* *errout* *slash*))
;;                                        ;;
;;  code formatting & printing functions  ;;
;;                                        ;;


;;  fortran code formatting & printing functions  ;;

(defun formatfort (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatfort1 lst)))
	(wrs ch)))

(defun formatfort1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt))) fortlinelen*)
			  (fortcontline)))
		   (princ elt))))))

(defun fortcontline ()
  (progn
   (terpri)
   (princ "     .")
   (forttab (- fortcurrind* 6))
   (spaces 1)))

(defun forttab (n)
  (progn
   (setq fortcurrind* (min (+ n 6) (- fortlinelen* 40)))
   (spaces (- fortcurrind* (posn)))))

;;  ratfor code formatting & printing functions  ;;

(defun formatrat (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatrat1 lst)))
	(wrs ch)))

(defun formatrat1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    ratlinelen*)
			  (ratcontline)))
		   (princ elt))))))

(defun ratcontline ()
  (progn
   (terpri)
   (rattab ratcurrind*)
   (spaces 1)))

(defun rattab (n)
  (progn
   (setq ratcurrind* (min n (- ratlinelen* 40)))
   (spaces (- ratcurrind* (posn)))))

;;  c code formatting & printing functions  ;;

(defun formatc (lst)
  (prog (ch)
	(setq ch (wrs nil))
	(foreach c in *outchanl* do
		 (progn
		  (wrs c)
		  (formatc1 lst)))
	(wrs ch)))

(defun formatc1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    clinelen*)
			  (ccontline)))
		   (princ elt))))))

(defun ccontline ()
  (progn
   (terpri)
   (ctab ccurrind*)
   (spaces 1)))

(defun ctab (n)
  (progn
   (setq ccurrind* (min n (- clinelen* 40)))
   (spaces (- ccurrind* (posn)))))


;;                             ;;
;;  general printing function  ;;
;;                             ;;


(defun pprin2 (arg)
  (let ((ch (wrs nil)))
       (cond ((eq arg *cr*)
	      (foreach c in *outchanl* do
		       (progn (wrs c) (terpri))))
	     (t
	      (foreach c in *outchanl* do
		       (progn (wrs c) (princ arg)))))
       (wrs ch)))


;;                 ;;
;;  error handler  ;;
;;                 ;;


;;  error & warning message printing routine  ;;

(defun gentranerr (msgtype exp msg1 msg2)
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
	(princ "*** ")
	(princ msg1)
	(terpri)
	(cond (msg2
	       (repeat
		(unless (member c (list '| |  *cr*))
		  (format t  "~%    ~a" msg2)
		  (format t " (y~an)  " *slash*))
		(member (setq c (readc)) '(y y n n)))))
	(wrs holdoch)
	(rds holdich)
	(cond ((member c '(n n))
	       (error "Unknown error")))))
