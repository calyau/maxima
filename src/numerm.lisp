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
  #+Maclisp
  `(*array nil 'flonum ,@l)
  #+(or cl NIL) `(make-array (list ,@l) :element-type 'double-float)
  )


(defmacro make-array% (&rest l)
  #+Maclisp
  `(*array nil 'fixnum ,@l)
  #+(or cl NIL)
  `(make-array (list ,@l) :element-type 'fixnum)
  )

(defmacro aref$ (&rest l)
  #+Maclisp
  `(arraycall flonum ,@l)
  #+(or cl NIL)
  `(aref (the (simple-array double-float) ,(car l)) ,@(cdr l))
  #+(or Franz)
  `(aref ,@l)
  )

(defmacro aref% (&rest l)
  #+Maclisp
  `(arraycall fixnum ,@l)
  #+(or cl NIL)
  `(aref (the (simple-array fixnum) ,(car l)) ,@(cdr l))
)

(defmacro free-array% (a)
  #+Maclisp
  `(*rearray ,a)
  #+(OR Cl NIL)
  ;; not useful to call return-array unless it is at end of area.
  ;; programs do better to save arrays as a resource, this works
  ;; in maclisp too.
  a
  )
(defmacro free-array$ (a)
  #+maclisp
  `(*rearray ,a)
  #+(OR Cl NIL)
  a
  )


(DEFMACRO DEFBINDTRAMP$ (NARGS)
  (LET ((BIND-TRAMP$ #-Multics (SYMBOLCONC 'bind-tramp nargs '$)
		     #+Multics (implode (mapcan 'exploden
						(list 'bind-tramp nargs '$))))
	(TRAMP$ #-Multics (SYMBOLCONC 'tramp nargs '$)
		#+Multics (implode (mapcan 'exploden (list 'tramp nargs '$)))))
;;;When Multics gets symbolconc the above conditionalization can be removed.
    `(PROGN 'COMPILE
	   #-cl (IF (FBOUNDP 'SPECIAL) (SPECIAL ,TRAMP$))
	    (PROCLAIM (QUOTE (SPECIAL ,TRAMP$)))
	     (DEFMACRO ,BIND-TRAMP$ (F G &REST BODY)
	      `(LET ((,',TRAMP$))
		 (LET ((,F (MAKE-TRAMP$ ,G ,',NARGS)))
		   ,@BODY))))))

(DEFBINDTRAMP$ 1)
(DEFBINDTRAMP$ 2)
(DEFBINDTRAMP$ 3)

(defmacro fcall$ (&rest l)
  #+Maclisp
  `(subrcall flonum ,@l)
  #+(OR Cl NIL)
  `(funcall ,@l)
  )

;; Central location for some important declarations.
#+Maclisp
(IF (FBOUNDP 'flonum)
    (FLONUM (GCALL1$ NIL NIL)
	    (GCALL2$ NIL NIL NIL)
	    (MTO-FLOAT NIL)
	    ))



