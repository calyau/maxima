;; f2cl0.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; edit here so that the root of the f2cl directory tree is appropriate
;; for your installation and the filename extension is valid
;;#+(or sparc sun4)(defvar *f2cl_dir* "/d/sen/f2cl/wrk/")
;;#+vms (defvar *f2cl_dir* "d:[sen.f2cl.wrk]")
;;(defvar *ext* 
;;          #+clisp ".fas" #+allegro ".fasl" #+vms ".fas" #+lucid ".sbin")
;;
;;(defun load-f2cl (x) 
;;(load (concatenate 'string *f2cl_dir* x *ext*) :print nil :verbose t))
;;
;;(load-f2cl "f2cl1" )
;;(load-f2cl "f2cl2" )
;;(load-f2cl "f2cl3" )
;;(load-f2cl "f2cl4" )
;;(load-f2cl "f2cl5" )
;;(load-f2cl "f2cl6" )
;;(load-f2cl "f2cl7" )
;;(load-f2cl "macros" )
;;
;;(format t "~&The f2cl software has been loaded.~%")

(in-package :common-lisp-user)

(defpackage "F2CL-LIB"
  (:use "CL")
  (:documentation "The package holding all symbols used by the Fortran to Lisp library")
  (:nicknames "FORTRAN-TO-LISP-LIBRARY")
  (:export
   ;; Constants
   "%FALSE%" "%TRUE%"
   ;; User-settable runtime options
   "*CHECK-ARRAY-BOUNDS*"
   ;; Types
   "INTEGER4" "INTEGER2" "INTEGER1" "REAL8" "REAL4" "COMPLEX8" "COMPLEX16"
   "ARRAY-DOUBLE-FLOAT" "ARRAY-SINGLE-FLOAT" "ARRAY-INTEGER4" "ARRAY-STRINGS"
   "LOGICAL"
   ;; Macros
   "FREF" "FSET" "WITH-ARRAY-DATA"
   "WITH-MULTI-ARRAY-DATA"
   "F2CL-INIT-STRING" "FREF-STRING" "FSET-STRING" "F2CL-SET-STRING"
   "F2CL-//" "FSTRING-/=" "FSTRING-=" "FSTRING->" "FSTRING->=" "FSTRING-<" "FSTRING-<="
   "FORTRAN_COMMENT" "FDO" "F2CL/" "ARITHMETIC-IF" "COMPUTED-GOTO"
   "ASSIGNED-GOTO"
   "FFORMAT"
   "DATA-IMPLIED-DO"
   "INT-ADD" "INT-SUB" "INT-MUL"
   ;; Utilities
   "ARRAY-SLICE" "ARRAY-INITIALIZE"
   ;; Intrinsic functions
   "ABS" "ACOS" "AIMAG" "AINT" "ALOG" "ALOG10" "AMAX0" "AMAX1"
   "AMIN1" "AMOD" "ANINT" "ASIN" "ATAN" "ATAN2"
   "CABS" "CEXP" "FCHAR" "CLOG" "CMPLX" "CONJG" "CCOS"
   "CSIN" "CSQRT" "DABS" "DACOS" "DASIN"
   "DATAN" "DATAN2" "DBLE" "DCOS" "DCOSH" "DEXP" "DIM"
   "DINT" "DLOG" "DLOG10" "DMAX1" "DMIN1" "DMOD"
   "DNINT" "DPROD" "DSIGN" "DSIN" "DSINH" "DSQRT" "DTAN"
   "DTANH" "FFLOAT" "IABS" "ICHAR" "IDIM" "IDINT"
   "IDNINT" "IFIX" "INDEX" "INT" "ISIGN" "LE" "LEN"
   "LGE" "LGT" "FLOG" "LOG10" "LT" "MAX" "MAX0"
   "MAX1" "MIN0" "MIN1" "NINT" "FREAL"
   "SIGN" "SNGL" "FSQRT"
   ;; Other functions
   "D1MACH" "R1MACH" "I1MACH"
   ))

#+nil
(defpackage "FORTRAN-TO-LISP"
  (:use "CL")
  (:documentation "The package holding all symbols need by the Fortran to Lisp converter")
  (:nicknames "F2CL")
  (:export
   ;; Main routines
   "F2CL"
   "F2CL-COMPILE"
   ))

;;;-------------------------------------------------------------------------
;;; end of f2cl0.l
;;;
;;; $Id: f2cl-package.lisp,v 1.7 2004-10-19 12:05:00 wjenkner Exp $
;;; $Log: f2cl-package.lisp,v $
;;; Revision 1.7  2004-10-19 12:05:00  wjenkner
;;; Eliminate all references to LISP or USER in Maxima, replacing them by
;;; CL and CL-USER, respectively (except for system-dependent stuff, like
;;; lisp:bye, of course).  In particular, make MAXIMA inherit from CL,
;;; not from LISP (take a look at the cleaned-up maxima-package.lisp).
;;; Also, all lisps now use the ERRSET from src/generr.lisp, so that
;;; src/kclmac.lisp with the SERROR stuff can be eliminated.
;;;
;;; Other changes needed for this:
;;;
;;; Avoid package locking errors by obeying to the CLHS 11.1.2.1.2
;;; (Constraints on the COMMON-LISP Package for Conforming Programs).
;;;
;;; lmdcls.lisp, mdebug.lisp, merror.lisp, mlisp.lisp, suprv1.lisp: Replace
;;; the special variable DEBUG by *MDEBUG*.
;;;
;;; commac.lisp, mtrace.lisp: Replace PRIN1 as special variable by *PRIN1*.
;;;
;;; specfn.lisp: Replace LAST as special variable by *LAST*
;;;
;;; maxima-package.lisp: Shadow GCD and BREAK.  For SBCL, shadow
;;; MAKUNBOUND in order to avoid package locking errors at runtime.
;;;
;;; commac.lisp: Give GCD and BREAK their CL function definition.  For
;;; SBCL, redefine MAKUNBOUND.
;;;
;;; db.lisp: Boldly replace the special variable * by db*
;;;
;;; limit.lisp ($limit): Don't declare T special.
;;;
;;; Revision 1.6  2004/10/04 02:25:54  amundson
;;; Downcasing of source complete. Code compiles and passes tests with
;;; clisp, cmucl, gcl and sbcl.
;;;
;;; Revision 1.5  2003/11/26 17:27:16  rtoy
;;; Synchronize to the current versions of f2cl.
;;;
;;; Revision 1.4  2002/07/18 02:37:22  rtoy
;;; Don't need to import destructuring-bind for GCL anymore.
;;;
;;; Revision 1.3  2002/05/19 20:22:32  rtoy
;;; GCL doesn't export DESTRUCTURING-BIND from the LISP package.  Import
;;; it.
;;;
;;; Revision 1.2  2002/05/01 18:20:18  amundson
;;; Package-related hacks for gcl.
;;;
;;; Revision 1.1  2002/04/26 13:04:46  rtoy
;;; Initial revision.
;;;
;;; Revision 1.13  2002/03/11 16:40:21  rtoy
;;; Export INT-ADD, INT-SUB, INT-MUL.
;;;
;;; Revision 1.12  2002/02/17 15:50:17  rtoy
;;; Export with-array-data.
;;;
;;; Revision 1.11  2002/02/10 03:41:53  rtoy
;;; Export ARRAY-STRINGS type.
;;;
;;; Revision 1.10  2002/01/13 16:24:24  rtoy
;;; All of the exported symbols in macros.l have been moved from the F2CL
;;; package to the F2CL-LIB package.
;;;
;;; Revision 1.9  2002/01/05 18:52:12  rtoy
;;; Add in-package.
;;;
;;; Revision 1.8  2001/04/26 17:49:50  rtoy
;;; Export new functions D1MACH and R1MACH
;;;
;;; Revision 1.7  2001/02/26 15:38:22  rtoy
;;; Move *check-array-bounds* from f2cl1.l to macros.l since the generated
;;; code refers to it.  Export this variable too.
;;;
;;; Revision 1.6  2000/09/01 13:51:20  rtoy
;;; AMAX1 and DIM were repeated.
;;;
;;; Revision 1.5  2000/08/05 19:06:34  rtoy
;;; Export F2CL-COMPILE.
;;;
;;; Revision 1.4  2000/07/28 22:07:44  rtoy
;;; It's FORTRAN, not FORTAN!
;;;
;;; Revision 1.3  2000/07/28 16:56:48  rtoy
;;; f2cl0.l isn't the (unused) f2cl loader anymore.  Use it to define the
;;; package used by f2cl.
;;;
;;; Revision 1.2  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-------------------------------------------------------------------------
