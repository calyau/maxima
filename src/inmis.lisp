;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module inmis)

(declare-top (special listofvars))

(defmvar $listconstvars nil
  "Causes LISTOFVARS to include %E, %PI, %I, and any variables declared
   constant in the list it returns if they appear in exp.  The default is
   to omit these." boolean see-also $listofvars)

(defmvar $listdummyvars t)

(defmvar $polyfactor nil)

(defmfun $unknown (f) (catch 'unknown (unknown (specrepcheck f))))

(defun unknown (f)
  (and (not (mapatom f))
       (cond ((and (eq (caar f) 'mqapply)
		   (not (oldget (caaadr f) 'specsimp)))
	      (throw 'unknown t))
	     ((not (oldget (caar f) 'operators)) (throw 'unknown t))
	     (t (mapc #'unknown (cdr f)) nil))))

(defmfun $listofvars (e) 
  (let ((listofvars (ncons '(mlist))))
    (when ($ratp e)
      (and (memq 'trunc (cddar e)) (setq e ($taytorat e)))
      (setq e (cons '(mlist)
		    (sublis (mapcar #'cons
				    (car (cdddar e))
				    ;; GENSYMLIST
				    (caddar e))
			    ;; VARLIST
			    (union* (listovars (cadr e))
				    (listovars (cddr e)))))))
    (atomvars e)
    (if (not $listdummyvars)
	(dolist (u (cdr listofvars))
	  (if (freeof u e) (zl-delete u listofvars 1))))
    listofvars))

(defun atomvars (e) 
  (cond ((and (symbolp e) (or $listconstvars (not ($constantp e))))
	 (add2lnc e listofvars))
	((atom e))
	((specrepp e) (atomvars (specdisrep e)))
	((memq 'array (car e)) (myadd2lnc e listofvars))
	(t (mapc #'atomvars (margs e))))) 

(defun myadd2lnc (item list) 
  (and (not (memalike item list)) (nconc list (ncons item)))) 

;; Reset the settings of all Macsyma user-level switches to their initial
;; values.

#+its
(defmfun $reset nil (load '((dsk macsym) reset fasl)) '$done)

#+multics
(defmfun $reset () (load (executable-dir "reset")) '$done)

#+nil
(defmfun $reset ()
  (load "MAX$DISK:[MAXDMP]RESET" :set-default-pathname nil))

;; Please do not use the following version on MC without consulting with me.
;; I already fixed several bugs in it, but the +ITS version works fine on MC 
;; and takes less address space. - JPG
(declare-top(special modulus $fpprec))
#-(or cl its multics nil) ;This version should be eventually used on Multics.
(defmfun $reset ()
  (setq *print-base* 10. *read-base* 10. ; *NOPOINT T
	modulus nil
					;ZUNDERFLOW T
	)
  ($debugmode nil)
  (cond ((not (= $fpprec 16.)) ($fpprec 16.) (setq $fpprec 16.))) 
  #+gc ($dskgc nil)
  (load #+pdp10   '((aljabr) init reset)
	#+lispm   "MACSYMA-OBJECT:ALJABR;INIT"
	#+multics (executable-dir "init_reset")
	#+franz    (concat vaxima-main-dir "//aljabr//reset"))
  ;; *** This can be flushed when all Macsyma user-switches are defined
  ;; *** with DEFMVAR.  This is part of an older mechanism.
  #+pdp10 (load '((macsym) reset fasl))
  '$done)

(defmfun $reset ()
  (setq *print-base* 10.)
  (setq *read-base* 10.)
  (maphash #'(lambda (key val)
	       (format t "Resetting ~S to ~S~%" key val)
	       (setf (symbol-value key) val))
	   *variable-initial-values*))
