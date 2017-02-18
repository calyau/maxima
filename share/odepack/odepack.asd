;;; -*- Mode: lisp -*-

(defpackage odepack-system
  (:use #:cl #:asdf))

(in-package #:odepack-system)

(defclass odepack-fortran-file (cl-source-file)
  ()
  (:default-initargs :type "f"))

(defun fortran-compile (op c &key (array-slicing t) (array-type :array) (package "ODEPACK")
			declare-common (common-as-array t))
  (let ((file (component-pathname c)))
    (f2cl:f2cl-compile file
		       :output-file (first (output-files op c))
		       :array-slicing array-slicing
		       :array-type array-type
		       :package package
		       :declare-common declare-common
		       :common-as-array common-as-array
		       )))

(defmethod perform ((op compile-op) (c odepack-fortran-file))
  (fortran-compile op c :package "ODEPACK"))

(defmethod perform ((op load-op) (c odepack-fortran-file))
  (load (first (input-files op c))))


;; Create a logical pathname for our files.
(let ((base (make-pathname :directory (pathname-directory *load-pathname*))))
  (setf (logical-pathname-translations "odepack")
	(list (list "**;*.*.*"
		    (merge-pathnames (make-pathname :directory '(:relative "odepack" :wild-inferiors)
						    :name :wild :type :wild)
				     base)))))

(defsystem odepack
  :description "F2CL conversion of ODEPACK: Initial value solver for ODEs"
  :pathname "odepack/"
  :components
  ((:module "package"
	    :pathname ""
	    :components
	    ((:file "package")))
   (:module "odepack"
	    :pathname ""
	    :default-component-class odepack-fortran-file
	    :components
	    (
	     (:file "adjlr"
		    :depends-on ("nroc" "nsfc" "nnfc" "nnsc"))
	     (:file "cdrv"
		    :depends-on ("nntc"))
	     (:file "cntnzu")
	     (:file "daigbt"
		    :depends-on ("ddecbt" "dsolbt"))
	     (:file "dainvg"
		    :depends-on ("dgefa" "dgesl" "dgbfa" "dgbsl"))
	     (:file "dainvgs")
	     (:file "datp"
		    :depends-on ("dvnorm" "dcopy"))
	     (:file "datv"
		    :depends-on ("dcopy" "dnrm2" "dcopy"))
	     (:file "daxpy")
	     (:file "dbnorm")
	     (:file "dcfode")
	     (:file "dcopy")
	     (:file "ddecbt"
		    :depends-on ("dgefa" "dgesl" "ddot"))
	     (:file "ddot")
	     (:file "dewset")
	     (:file "dfnorm")
	     (:file "dgbfa"
		    :depends-on ("idamax" "dscal" "daxpy"))
	     (:file "dgbsl"
		    :depends-on ("daxpy"))
	     (:file "dgefa"
		    :depends-on ("idamax" "dscal" "daxpy"))
	     (:file "dgesl"
		    :depends-on ("daxpy" "ddot"))
	     (:file "dhefa"
		    :depends-on ("idamax"))
	     (:file "dhels")
	     (:file "dheqr")
	     (:file "dhesl")
	     (:file "dintdy"
		    :depends-on ("xerrwd"))
	     (:file "diprep"
		    :depends-on ("dprep")
		    :perform (compile-op :around (op c)
					 (fortran-compile op c
							  :common-as-array t :declare-common t)))
	     ;; This routine takes a slice of a double precision array and
	     ;; passes it to dprepi as a integer array. That won't work in
	     ;; Lisp!
	     (:file "diprepi"
		    :depends-on ("dprepi"))
	     (:file "dlhin"
		    :depends-on ("dvnorm"))
	     (:file "dlsoda"
		    :depends-on ("xerrwd" "dmnorm" "dstoda")
		    :perform (compile-op :around (op c)
					 (fortran-compile op c :common-as-array t :declare-common t)))
	     (:file "dlsodar"
		    :depends-on ("drchek"))
	     (:file "dlsode"
		    :depends-on ("dstode"))
	     (:file "dlsodes"
		    :depends-on ("dstode" "dewset"))
	     (:file "dlsodi"
		    :depends-on ("dstodi" "dainvg"))
	     (:file "dlsodis"
		    :depends-on ("dstodi" "dainvgs"))
	     (:file "dlsodkr"
		    :depends-on ("drchek" "dstoka")
		    :perform (compile-op :around (op c)
					 (fortran-compile op c :common-as-array t :declare-common t)))
	     (:file "dlsodpk"
		    :depends-on ("dstodpk"))
	     (:file "dlsoibt"
		    :depends-on ("dstodi"))
	     (:file "dmnorm")
	     (:file "dnrm2")
	     (:file "dorthog")
	     (:file "dpcg")
	     (:file "dpcgs")
	     (:file "dpjibt")
	     (:file "dpkset")
	     (:file "dprep"
		    :depends-on ("jgroup" "odrv"))
	     (:file "dprepi"
		    :depends-on ("jgroup" "odrv"))
	     (:file "dprepj")
	     (:file "dprepji")
	     (:file "dprja")
	     (:file "dprjis")
	     (:file "dprjs")
	     (:file "drchek"
		    :depends-on ("droots"))
	     (:file "droots")
	     (:file "dscal")
	     (:file "dsetpk")
	     (:file "dslsbt")
	     (:file "dsolbt"
		    :depends-on ("dgesl" "ddot"))
	     (:file "dsolpk"
		    :depends-on ("dspiom" "dspigmr" "dusol"))
	     (:file "dsolss")
	     (:file "dsolsy")
	     (:file "dspigmr")
	     (:file "dspiom")
	     (:file "dsrcar")
	     (:file "dsrckr")
	     (:file "dsrcma")
	     (:file "dsrcms")
	     (:file "dsrcom")
	     (:file "dsrcpk")
	     (:file "dstoda"
		    :depends-on ("dmnorm"))
	     (:file "dstode")
	     (:file "dstodi")
	     (:file "dstodpk"
		    :depends-on ("dpkset" "dsolpk"))
	     (:file "dstoka"
		    :depends-on ("dsetpk" "dsolpk"))
	     (:file "dumach"
		    :depends-on ("dumsum"))
	     (:file "dumsum")
	     (:file "dusol")
	     (:file "dvnorm")
	     (:file "idamax")
	     (:file "iumach")
	     (:file "ixsav")
	     (:file "jgroup")
	     (:file "md"
		    :depends-on ("mdi" "mdm" "mdp" "mdu"))
	     (:file "mdi")
	     (:file "mdm")
	     (:file "mdp")
	     (:file "mdu")
	     (:file "nnfc")
	     (:file "nnsc")
	     (:file "nntc")
	     (:file "nroc")
	     (:file "nsfc")
	     (:file "odrv"
		    :depends-on ("sro" "md"))
	     (:file "sro")
	     (:file "xerrwd"
		    :depends-on ("ixsav"))
	     (:file "xsetf")
	     (:file "xsetun")))))

(defmethod perform ((op test-op) (c (eql (find-system "odepack"))))
    (oos 'test-op "odedemo-lsode"))



;;; Demo programs
;;;
;;; Note: Each of the demos should probably be run in a separate Lisp
;;; instance because some of the demos define functions with the same
;;; name but different parameters.  This will really confuse the
;;; generated code, because the generated code uses knowledge of the
;;; function to generate the call.

;; (opkdemo1)
;;
;; Output matches Fortran code.
(defsystem odedemo-lsode
  :pathname "odepack/"
  :depends-on ("odepack")
  :components
  ((:module "demo1"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo1")
	     (:file "f1")
	     (:file "jac1")
	     (:file "f2")
	     (:file "jac2")
	     (:file "edit2")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsode"))))
  (funcall (intern "OPKDEMO1" (find-package '#:odepack))))

#||
;; This won't work because opkdemo2 equivalences two arrays together.
;; f2cl doesn't know how to handle that yet.
(defsystem odedemo-lsodes
    :source-pathname (translate-logical-pathname "odepack:")
    :binary-pathname (translate-logical-pathname "odepack:lib")
    :source-extension "f"
    :language :f2cl
    :compiler-options (:common-as-array t)
    :depends-on ("odepack")
    :components
    ((:file "opkdemo2")))


||#

;; (opkdemo3)
;;
;; Output matches Fortran code.
(defsystem odedemo-lsoda
  :pathname "odepack/"
  :depends-on ("odepack")
  :components
  ((:module "demo3"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo3")
	     (:file "f1")
	     (:file "jac1")
	     (:file "f2")
	     (:file "jac2")
	     (:file "edit2")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsoda"))))
  (funcall (intern "OPKDEMO3" (find-package '#:odepack))))

;; (opkdemo4)
;;
;; Output matches Fortran code.
(defsystem odedemo-lsodar
  :pathname "odepack/"
  :depends-on ("odepack")
  :components
  ((:module "demo4"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo4")
	     (:file "f1")
	     (:file "gr1")
	     (:file "f2")
	     (:file "jac2")
	     (:file "gr2")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsodar"))))
  (funcall (intern "OPKDEMO4" (find-package '#:odepack))))


;; (opkdemo5)
;;
;; This test takes quite a while to run.  Probably could be optimized
;; more if we were more careful about array declarations.  Assumption
;; untested, though.
;;
;; This seems to work, and the output matches the Fortran output,
;; except the test with mf = 29 isn't printed out.  Don't know why.
;;
;; The output is placed in demout in the directory where this is run.
;; Compare this to demo-lsodpk.out
(defsystem odedemo-lsodpk
  :pathname "odepack/"
  :depends-on ("odepack")
  :components
  ((:module "demo5"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo5"
		    :depends-on ("gset" "cinit" "outweb")
		    :perform (compile-op :around (op c)
					 (fortran-compile op c :declare-common t)))
	     (:file "setpar")
	     (:file "gset")
	     (:file "cinit")
	     (:file "outweb")
	     (:file "fweb"
		    :depends-on ("webr"))
	     (:file "webr")
	     (:file "jacbg"
		    :depends-on ("fbg"))
	     (:file "fbg")
	     (:file "solsbg"
		    :depends-on ("gs"))
	     (:file "gs")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsodpk"))))
  (format *error-output* "Running odedemo-lsodpk.  This make take some time.~%")
  (finish-output *error-output*)
  (funcall (intern "OPKDEMO5" (find-package '#:odepack))))


;; This seems to work.
(defsystem odedemo-lsodkr
  :pathname "odepack/"
  :depends-on ("odepack")
  :depends-on ("odepack")
  :default-component-class odepack-fortran-file
  :components
  ((:file "opkdemo6"
	  :perform (compile-op :around (op c)
			       (fortran-compile op c :declare-common t)))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsodkr"))))
  (funcall (intern "OPKDEMO6" (find-package '#:odepack))))



;; This runs and the expected output seems ok.
(defsystem odedemo-lsodi
  :depends-on ("odepack")
  :pathname "odepack/"
  :components
  ((:module "demo7"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo7"
		    :perform (compile-op :around (op c)
					 (fortran-compile op c :declare-common t))
		    :depends-on ("elkup"))
	     (:file "gfun")
	     (:file "res"
		    :depends-on ("gfun"))
	     (:file "addabd")
	     (:file "addafl")
	     (:file "jacbd")
	     (:file "jacfl")
	     (:file "elkup")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsodi"))))
  (format *error-output* "Running odedemo-lsodi.  This make take some time.~%")
  (funcall (intern "OPKDEMO7" (find-package '#:odepack))))

(defsystem odedemo-lsoibt
  :depends-on ("odepack")
  :pathname "odepack/"
  :components
  ((:module "demo8"
	    :default-component-class odepack-fortran-file
	    :components
	    ((:file "opkdemo8"
		    :perform (compile-op :around (op c)
					 (fortran-compile op c :declare-common t))
		    :depends-on ("setic"
				 "edit"
				 "maxerr"))
	     (:file "addabt")
	     (:file "edit")
	     (:file "gfun")
	     (:file "jacbt")
	     (:file "maxerr")
	     (:file "res"
		    :depends-on ("subav"
				 "gfun"))
	     (:file "setic")
	     (:file "subav")))))

(defmethod perform ((op test-op) (c (eql (find-system "odedemo-lsoibt"))))
  (format *error-output* "Running odedemo-lsoibt.  This make take some time.~%")
  (funcall (intern "OPKDEMO7" (find-package '#:odepack))))

#||


;; Doesn't work.  DIPREPI takes a double precision array and slices it
;; up and passes it to DPREPI which wants integer arrays.  That ain't
;; gonna work in Lisp!
(defsystem odedemo-lsodis
    :source-pathname (translate-logical-pathname "odepack:")
    :binary-pathname (translate-logical-pathname "odepack:lib")
    :source-extension "f"
    :language :f2cl
    :compiler-options (:common-as-array t :declare-common t)
    :depends-on ("odepack")
    :components
    ((:file "opkdemo9")))
||#
