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
(macsyma-module numerm macro)

;;; Macros for interface of lisp numerical routines to macsyma,
;;; for use with the functions in Maxsrc;Numer.

(defmacro make-array$ (&rest l)
					;I guess macsyma has to know the default "flonum" type... --gsb
  #+maclisp
  `(*array nil 'flonum ,@l)
  #+(or cl nil) `(make-array (list ,@l) :element-type 'double-float)
  )


(defmacro make-array% (&rest l)
  #+maclisp
  `(*array nil 'fixnum ,@l)
  #+(or cl nil)
  `(make-array (list ,@l) :element-type 'fixnum)
  )

(defmacro aref$ (&rest l)
  #+maclisp
  `(arraycall flonum ,@l)
  #+(or cl nil)
  `(aref (the (simple-array double-float) ,(car l)) ,@(cdr l))
  #+(or franz)
  `(aref ,@l)
  )

(defmacro aref% (&rest l)
  #+maclisp
  `(arraycall fixnum ,@l)
  #+(or cl nil)
  `(aref (the (simple-array fixnum) ,(car l)) ,@(cdr l))
  )

(defmacro free-array% (a)
  #+maclisp
  `(*rearray ,a)
  #+(or cl nil)
  ;; not useful to call return-array unless it is at end of area.
  ;; programs do better to save arrays as a resource, this works
  ;; in maclisp too.
  a
  )
(defmacro free-array$ (a)
  #+maclisp
  `(*rearray ,a)
  #+(or cl nil)
  a
  )


(defmacro defbindtramp$ (nargs)
  (let ((bind-tramp$ #-multics (symbolconc 'bind-tramp nargs '$)
		     #+multics (implode (mapcan 'exploden
						(list 'bind-tramp nargs '$))))
	(tramp$ #-multics (symbolconc 'tramp nargs '$)
		#+multics (implode (mapcan 'exploden (list 'tramp nargs '$)))))
;;;When Multics gets symbolconc the above conditionalization can be removed.
    `(progn 'compile
      #-cl (if (fboundp 'special) (special ,tramp$))
      (proclaim (quote (special ,tramp$)))
      (defmacro ,bind-tramp$ (f g &rest body)
	`(let ((,',tramp$))
	  (let ((,f (make-tramp$ ,g ,',nargs)))
	    ,@body))))))

(defbindtramp$ 1)
(defbindtramp$ 2)
(defbindtramp$ 3)

(defmacro fcall$ (&rest l)
  #+maclisp
  `(subrcall flonum ,@l)
  #+(or cl nil)
  `(funcall ,@l)
  )

;; Central location for some important declarations.
#+maclisp
(if (fboundp 'flonum)
    (flonum (gcall1$ nil nil)
	    (gcall2$ nil nil nil)
	    (mto-float nil)
	    ))



