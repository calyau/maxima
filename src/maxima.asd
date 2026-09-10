; -*- Lisp -*-

; Copyright 2007 by Luigi Panzeri.
; This file is released under the terms of
; the GNU General Public License, version 2.

(in-package :asdf)

;; Don't try to optimize so much in ECL.
;; Therefore functions can be redefined (essential for share libraries).
#+ecl (declaim (optimize (debug 2)))

(in-package :cl-user)

(defvar *maxima-build-time* (multiple-value-list (get-decoded-time)))
(export '*maxima-build-time*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; All Lisp files MUST be compiled with this so that every literal
  ;; number is read as a double-float.
  (setf cl:*read-default-float-format* 'cl:double-float)
  (unless (find-package :maxima.system)
    (defpackage :maxima.system
      (:use :common-lisp :asdf))))

(in-package :maxima.system)

(defsystem :maxima
  :description "Maxima is a symbolic computation program."
  :licence "GPL"
  ;;:serial t
  :components
  ((:module package
     :pathname ""
     :components (#-gcl (:file "maxima-package")
                  #+ecl (:file "ecl-port")
                  (:file "autoconf-variables"
                    :depends-on ("maxima-package"))))
   (:module float-format
     :pathname ""
     :components
     ((:file "float-format")))
   (:module intl
     :pathname ""
     :components
     (
      ;; Some versions of CMUCL already have a compatible version of
      ;; INTL, so skip it if we have it.
      #+#.(cl:if (cl:and (cl:member :cmu cl:*features*) (cl:find-package '#:intl))  '(or) '(and))
      (:file "intl")))
   (:module globals
     :pathname ""
     :depends-on (intl compatibility-macros1)
     :components
     ((:file "globals")))
   (:module info
     :pathname ""
     :components ((:file "pregexp")
                  (:file "cl-info")))
   (:module declarations
     :pathname ""
     :depends-on (globals)
     :components ((:file "lmdcls")))
   (:module destructuring-let
     :pathname ""
     :components ((:file "letmac")))
   ;; Contains macros and conditions related to maxima errors.
   ;; Without this, there would be a circular dependency utilities ->
   ;; i-o -> utilities because of the error macros.
   (:module errset
     :pathname ""
     :components
     ((:file "errset")))
   ;; This is a critical dependency so that we read floats
   ;; with the correct type.  And transitively, everything
   ;; that depends on compatibility-macros1 will have the
   ;; correct float type.
   ;;
   ;; It appears as if any module that has literal floats
   ;; depends on compatibility-macros1, so they all implicitly
   ;; depend on float-format.
   (:module compatibility-macros1
     :pathname ""
     :depends-on (float-format)
     :components ((:file "generr")
                  (:file "clmacs")))
   (:module defmfun
     :pathname ""
     :depends-on (globals)
     :components ((:file "defmfun-check")))
   (:module float-properties
     :pathname ""
     :depends-on (defmfun)
     :components ((:file "float-properties")))
   (:module compatibility-macros
     :pathname ""
     :depends-on (defmfun compatibility-macros1)
     :components (#+gcl (:file "gcl-compat")
                  (:file "commac")))
   (:module prerequisites
     :pathname ""
     :depends-on (defmfun compatibility-macros1)
     :components ((:file "mormac")
                  (:file "compat")))
   (:module maxima-language-compiler-macros
     :pathname ""
     :depends-on (defmfun compatibility-macros1 declarations
                   prerequisites compatibility-macros)
     :components ((:file "transm")))
   (:module command-line
     :pathname ""
     :depends-on (defmfun getopt)
     :components ((:file "command-line")))
   (:module getopt
     :pathname ""
     :depends-on (defmfun)
     :components ((:file "getopt")))
   (:module fundamental-macros
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1
                  declarations prerequisites
                  compatibility-macros)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "defcal")
                  (:file "maxmac")))
   (:module utility-macros
     :pathname ""
     :depends-on (defmfun compatibility-macros compatibility-macros1
                   prerequisites errset)
     :components ((:file "mforma")))
   (:module simp-utilities
     :pathname ""
     ;; There are other dependencies.  We call $ratdisrep,
     ;; $outofpois, $ratsimp and others.  These are all
     ;; function calls.  Can't really do anything about that.
     ;; and can't depend on those modules because that causes a
     ;; circular dependency.
     :depends-on (prerequisites defmfun)
     ;; Module is a clean DAG at every level.
     :components
     ((:file "mopers")
      (:file "opers"
        :depends-on ("mopers"))
      (:file "simp-utils"
        :depends-on ("mopers"))
      (:file "mutils"
        :depends-on ("simp-utils"))
      (:file "simp"
        :depends-on ("simp-utils" "opers" "mopers" "mutils"))))
   (:module nformat
     :pathname ""
     ;; poisson-series just to get $outofpois defined before its use.
     :depends-on (float-properties simp-utilities poisson-series)
     :components ((:file "nforma")))
   (:module basic-utilities
     :pathname ""
     ;; Note: this depends on merror which causes a circular
     ;; dependency.
     :depends-on (simp-utilities fundamental-macros nformat)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "opr-util")
                  (:file "basic-util")))
   (:module m2-pattern-matcher
     :pathname ""
     :depends-on (simp-utilities)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "schatc")
                  (:file "schatc-util")))
   ;; This contains the reader macro #$...$ so we want to compile this
   ;; as early as possible before anything uses it.
   (:module reader
     :pathname ""
     :depends-on (globals defmfun compatibility-macros compatibility-macros1
                  declarations fundamental-macros prerequisites
                  utility-macros simp-utilities)
     :components ((:file "nparse")))
   (:module other-macros
     :pathname ""
     :depends-on (defmfun compatibility-macros1 declarations
                   prerequisites utility-macros)
     ;; Module is a clean DAG at every level.
     :components ((:file "mrgmac")
                  (:file "rzmac")
                  (:file "strmac")
                  (:file "displm")
                  (:file "safe-recursion")))
   (:module rat-macros
     :pathname ""
     :depends-on (defmfun compatibility-macros declarations
                   fundamental-macros prerequisites)
     :components ((:file "ratmac")
                  (:file "mhayat")))
   #+gcl (:file "optimize")              ; jfa check this
   (:module utilities
     :pathname ""
     :depends-on (globals defmfun utility-macros compatibility-macros
                  compatibility-macros1 declarations
                  fundamental-macros prerequisites)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "utils")
                  (:file "sumcon")
                  (:file "sublis")
                  (:file "merror")
                  (:file "mformt")
                  (:file "mutils-fn")
                  (:file "outmis")
                  (:file "ar")))
   (:module commands
     :pathname ""
     :depends-on (globals defmfun compatibility-macros compatibility-macros1
                  declarations fundamental-macros prerequisites
                  utility-macros simp-utilities basic-utilities)
     :components ((:file "comm")
                  (:file "comm2")))
   (:module evaluator
     :pathname ""
     :depends-on (globals defmfun utility-macros compatibility-macros
                  compatibility-macros1 declarations
                  destructuring-let fundamental-macros
                  prerequisites utilities commands simp-utilities)
     ;; There are no circular dependencies among these files.
     :components ((:file "mmacro")
                  (:file "mlisp"
                    :depends-on ("mmacro"))
                  (:file "buildq"
                    :depends-on ("mlisp"))))
   ;; The free versions of Allegro and Lispworks have a heap limit.
   ;; Let's not compile these routines so that we can at least
   ;; get the rest of maxima built.
   (:module numerical
     :depends-on (defmfun)
     ;; Compile-time deps form a DAG.
     :components
     (
      (:module packages
        :pathname ""
        :components
        ((:file "f2cl-lib-package")
         (:file "slatec")))
      (:module f2cl-lib
        :pathname ""
        :depends-on (packages)
        :components ((:file "f2cl-lib")))
      (:module slatec
        :pathname "slatec"
        :depends-on (f2cl-lib packages)
        #+(or allegro-cl-trial lispworks-personal-edition) :load-only
        #+(or allegro-cl-trial lispworks-personal-edition)  t
        :components
        (
         (:file "fdump")
         (:file "j4save")
         (:file "initds"
           :depends-on ("xermsg"))
         (:file "xgetua")
         (:file "xermsg"
           :depends-on ("fdump" "j4save" "xercnt" "xerhlt" "xerprn" "xersve"))
         (:file "xercnt")
         (:file "xerhlt")
         (:file "xerprn"
           :depends-on ("xgetua"))
         (:file "xersve"
           :depends-on ("j4save"))
         (:file "dcsevl"
           :depends-on ("xermsg"))

         ;; Gamma function
         (:file "d9lgmc"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "dgamlm"
           :depends-on ("xermsg"))
         (:file "dgamma"
           :depends-on ("d9lgmc" "dcsevl" "dgamlm" "initds" "xermsg"))
         (:file "dgamln")
         (:file "dlngam"
           :depends-on ("d9lgmc" "dgamma" "xermsg"))

         ;; Bessel J functions
         (:file "d9b0mp"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "d9b1mp"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "dbesj0"
           :depends-on ("d9b0mp" "dcsevl" "initds"))
         (:file "dbesj1"
           :depends-on ("d9b1mp" "dcsevl" "initds" "xermsg"))
         (:file "djairy")
         (:file "dasyjy")
         (:file "dbesj"
           :depends-on ("dasyjy" "djairy" "dlngam" "xermsg"))
         ;; Bessel I functions
         (:file "dbsi0e"
           :depends-on ("dcsevl" "initds"))
         (:file "dbsi1e"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "dbesi0"
           :depends-on ("dbsi0e" "dcsevl" "initds" "xermsg"))
         (:file "dbesi1"
           :depends-on ("dbsi1e" "dcsevl" "initds" "xermsg"))
         (:file "dasyik")
         (:file "dbesi"
           :depends-on ("dasyik" "dlngam" "xermsg"))
         (:file "zbesi"
           :depends-on ("zabs" "zbinu"))

         ;; Bessel J function for complex
         ;; arg and real order.

         (:file "zabs")
         (:file "zacai"
           :depends-on ("zabs" "zasyi" "zbknu" "zmlri" "zs1s2" "zseri"))
         (:file "zairy"
           :depends-on ("zabs" "zacai" "zbknu" "zexp" "zsqrt"))
         (:file "zasyi"
           :depends-on ("zabs" "zdiv" "zexp" "zmlt" "zsqrt"))
         (:file "zbesj"
           :depends-on ("zabs" "zbinu"))
         (:file "zbinu"
           :depends-on ("zabs" "zasyi" "zbuni" "zmlri" "zseri" "zuoik" "zwrsk"))
         (:file "zbknu"
           :depends-on ("dgamln" "zabs" "zdiv" "zexp" "zkscl"
                        "zlog" "zmlt" "zshch" "zsqrt" "zuchk"))
         (:file "zbuni"
           :depends-on ("zabs" "zuni1" "zuni2"))
         (:file "zdiv")
         (:file "zexp")
         (:file "zkscl"
           :depends-on ("zabs" "zlog" "zuchk"))
         (:file "zlog"
           :depends-on ("zabs"))
         (:file "zmlri"
           :depends-on ("dgamln" "zabs" "zexp" "zlog" "zmlt"))
         (:file "zmlt")
         (:file "zrati"
           :depends-on ("zabs" "zdiv"))
         (:file "zs1s2"
           :depends-on ("zabs" "zexp" "zlog"))
         (:file "zseri"
           :depends-on ("dgamln" "zabs" "zdiv" "zlog" "zmlt" "zuchk"))
         (:file "zshch")
         (:file "zsqrt"
           :depends-on ("zabs"))
         (:file "zuchk")
         (:file "zunhj"
           :depends-on ("zabs" "zdiv" "zlog" "zsqrt"))
         (:file "zuni1"
           :depends-on ("zabs" "zuchk" "zunik" "zuoik"))
         (:file "zuni2"
           :depends-on ("zabs" "zairy" "zuchk" "zunhj" "zuoik"))
         (:file "zunik"
           :depends-on ("zdiv" "zlog" "zsqrt"))
         (:file "zuoik"
           :depends-on ("zabs" "zlog" "zuchk" "zunhj" "zunik"))
         (:file "zwrsk"
           :depends-on ("zabs" "zbknu" "zrati"))

         ;; Bessel Y functions
         (:file "dbesy0"
           :depends-on ("d9b0mp" "dbesj0" "dcsevl" "initds" "xermsg"))
         (:file "dbesy1"
           :depends-on ("d9b1mp" "dbesj1" "dcsevl" "initds" "xermsg"))
         (:file "dbesy"
           :depends-on ("dasyjy" "dbesy0" "dbesy1" "dbsynu" "dyairy" "xermsg"))
         (:file "dbsynu"
           :depends-on ("dgamma" "xermsg"))
         (:file "dyairy")

         (:file "zbesy"
           :depends-on ("zbesh"))
         (:file "zbesh"
           :depends-on ("zabs" "zacon" "zbknu" "zbunk" "zuoik"))
         (:file "zacon"
           :depends-on ("zabs" "zbinu" "zbknu" "zmlt" "zs1s2"))
         (:file "zbunk"
           :depends-on ("zunk1" "zunk2"))
         (:file "zunk1"
           :depends-on ("zabs" "zs1s2" "zuchk" "zunik"))
         (:file "zunk2"
           :depends-on ("zabs" "zairy" "zs1s2" "zuchk" "zunhj"))

         ;; Bessel K functions
         (:file "dbesk0"
           :depends-on ("dbesi0" "dbsk0e" "dcsevl" "initds" "xermsg"))
         (:file "dbsk0e"
           :depends-on ("dbesi0" "dcsevl" "initds" "xermsg"))
         (:file "dbesk1"
           :depends-on ("dbesi1" "dbsk1e" "dcsevl" "initds" "xermsg"))
         (:file "dbsk1e"
           :depends-on ("dbesi1" "dcsevl" "initds" "xermsg"))
         (:file "dbesk"
           :depends-on ("dasyik" "dbesk0" "dbesk1" "dbsk0e" "dbsk1e" "dbsknu" "xermsg"))
         (:file "dbsknu"
           :depends-on ("dgamma" "xermsg"))
         (:file "zbesk"
           :depends-on ("zabs" "zacon" "zbknu" "zbunk" "zuoik"))

         ;; Airy functions
         (:file "d9aimp"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "daie"
           :depends-on ("d9aimp" "dcsevl" "initds"))
         (:file "dai"
           :depends-on ("d9aimp" "daie" "dcsevl" "initds" "xermsg"))
         (:file "dbie"
           :depends-on ("d9aimp" "dcsevl" "initds"))
         (:file "dbi"
           :depends-on ("d9aimp" "dbie" "dcsevl" "initds" "xermsg"))
         (:file "zbiry"
           :depends-on ("zabs" "zbinu" "zdiv" "zsqrt"))
         ;; Error functions
         (:file "derf"
           :depends-on ("dcsevl" "derfc" "initds"))
         (:file "derfc"
           :depends-on ("dcsevl" "initds" "xermsg"))
         ;; Exponential integrals
         (:file "de1"
           :depends-on ("dcsevl" "initds" "xermsg"))
         (:file "dei"
           :depends-on ("de1"))
         (:file "dspenc"
           :depends-on ("d9upak"))
         (:file "d9upak")))
      (:module quadpack
        :pathname "slatec"
        #+(or allegro-cl-trial lispworks-personal-edition) :load-only
        #+(or allegro-cl-trial lispworks-personal-edition) t
        :components
        (
         ;; Support
         (:file "dqwgtf")
         (:file "dqcheb")
         (:file "dqk15w")
         (:file "dqwgts")
         (:file "dqwgtc")
         (:file "dgtsl")
         ;; Core integration routines
         (:file "dqk15")
         (:file "dqk31")
         (:file "dqk41")
         (:file "dqk51")
         (:file "dqk61")
         (:file "dqk21")
         (:file "dqk15i")
         (:file "dqelg")
         (:file "dqpsrt")
         (:file "dqc25s"
           :depends-on ("dqcheb" "dqk15w"))
         (:file "dqmomo")
         (:file "dqc25c"
           :depends-on ("dqcheb"
                        "dqk15w"))
         (:file "dqc25f"
           :depends-on ("dgtsl"
                        "dqcheb"
                        "dqk15w"
                        "dqwgtf"))
         ;; Basic integrators
         (:file "dqage"
           :depends-on ("dqk15"
                        "dqk31"
                        "dqk41"
                        "dqk51"
                        "dqk61"
                        "dqk21"
                        "dqpsrt"))
         (:file "dqagie"
           :depends-on ("dqelg"
                        "dqk15i"
                        "dqpsrt"))
         (:file "dqagp"
           :depends-on ("dqagpe"))
         (:file "dqagpe"
           :depends-on ("dqelg"
                        "dqpsrt"
                        "dqk21"))
         (:file "dqagse"
           :depends-on ("dqk21"
                        "dqelg"
                        "dqpsrt"))
         (:file "dqawfe"
           :depends-on ("dqagie"
                        "dqawoe"
                        "dqelg"))
         (:file "dqawoe"
           :depends-on ("dqc25f"
                        "dqpsrt"
                        "dqelg"))
         (:file "dqawse"
           :depends-on ("dqc25s"
                        "dqmomo"
                        "dqpsrt"))
         (:file "dqawce"
           :depends-on ("dqc25c"
                        "dqpsrt"))
         ;; Simplified interface routines
         (:file "dqng")
         (:file "dqag"
           :depends-on ("dqage"))
         (:file "dqags"
           :depends-on ("dqagse"))
         (:file "dqagi"
           :depends-on ("dqagie"))
         (:file "dqawf"
           :depends-on ("dqawfe"))
         (:file "dqawo"
           :depends-on ("dqawoe"))
         (:file "dqaws"
           :depends-on ("dqawse"))
         (:file "dqawc"
           :depends-on ("dqawce"))
         ;; Maxima interface
         (:file "quadpack")))))
   (:module simplification
     :pathname ""
     :depends-on (globals defmfun reader utility-macros compatibility-macros
                  compatibility-macros1 declarations destructuring-let
                  fundamental-macros i-o other-macros prerequisites
                  numerical-utilities simp-utilities)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components
     ((:file "simp-fn")
      (:file "csimp")
      (:file "logarc")
      (:file "rpart"
        :depends-on ("logarc"))
      (:file "float"
        :depends-on ("logarc" "rpart"))
      (:file "csimp2"
        :depends-on ("csimp" "float" "rpart"))
      (:file "zero"
        :depends-on ("rpart"))
      (:file "halfangle"
        :depends-on ("rpart"))))
   (:module numeric-bigfloat
     :pathname ""
     :depends-on (globals defmfun package)
     :components ((:file "numeric")))
   (:module server
     :pathname ""
     :depends-on (defmfun)
     :components ((:file "server")))
   (:module i-o
     :pathname ""
     :depends-on (globals defmfun compatibility-macros compatibility-macros1
                  declarations fundamental-macros other-macros
                  prerequisites utilities utility-macros
                  evaluator)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components
     ((:file "prompt-util")
      (:file "suprv1"
        :depends-on ("prompt-util"))
      (:file "macsys"
        :depends-on ("prompt-util" "suprv1"))
      (:file "testsuite")
      (:file "wxmx")
      (:file "mload"
        :depends-on ("prompt-util" "suprv1" "macsys" "testsuite" "wxmx"))
      (:file "mactex"
        :depends-on ("prompt-util" "suprv1"))
      (:file "dskfn"
        :depends-on ("prompt-util" "suprv1"))))
   (:module factoring
     :pathname ""
     :depends-on (globals defmfun compatibility-macros compatibility-macros1
                  declarations fundamental-macros other-macros
                  prerequisites rat-macros utilities)
     ;; Compile-time deps form a DAG.  Runtime cycle by design:
     ;; factor <-> algfac (7+7 calls) -- factor's general
     ;; factoring dispatches to algfac for algebraic
     ;; extensions; algfac uses factor's primitives.  Several
     ;; declare-top specials in factor.lisp (var, res, trl*,
     ;; ind, alc, *xn, split*) are mutated cross-file.
     :components
     ((:file "factor")
      (:file "lesfac"
        :depends-on ("factor"))
      (:file "ufact"
        :depends-on ("factor"))
      (:file "result"
        :depends-on ("factor" "lesfac" "ufact"))
      (:file "algfac"
        :depends-on ("factor" "result"))
      (:file "nalgfa"
        :depends-on ("factor" "result"))))
   (:module ifactor
     :pathname ""
     :depends-on (defmfun prerequisites rat-macros utility-macros)
     :components ((:file "ifactor")))
   (:module rational-functions
     :pathname ""
     :depends-on (globals defmfun rat-macros other-macros
                  compatibility-macros1 ifactor factoring
                  compatibility-macros declarations destructuring-let
                  fundamental-macros prerequisites utilities
                  simp-utilities)
     ;; Compile-time deps have no cross-file macros nor cross-file
     ;; specials.  However, there are lots of mutually recursive calls
     ;; among the files.  Pretty hard to fix this.
     :components
     ((:file "rat3a")
      (:file "rat3b")
      (:file "rat3d")
      (:file "rat3c")
      (:file "rat3e")
      (:file "nrat4")
      (:file "ratout")))
   (:module maxima-language-compiler
     :pathname ""
     :depends-on (globals defmfun maxima-language-compiler-macros evaluator
                  compatibility-macros compatibility-macros1
                  declarations destructuring-let fundamental-macros
                  i-o prerequisites)
     ;; Compile-time deps form a DAG.  Three small runtime
     ;; cycles remain, all between transl (the translator core)
     ;; and its tightly-coupled helpers: trans2 (array
     ;; dispatch), trutil (defvar pushing), trmode (mode
     ;; system).
     :components
     ((:file "transl")
      (:file "transs"
        :depends-on ("transl"))
      (:file "trans1")
      (:file "trans2"
        :depends-on ("transl"))
      (:file "trans3"
        :depends-on ("transl"))
      (:file "trans4"
        :depends-on ("trprop"))
      (:file "trans5"
        :depends-on ("transl"))
      (:file "transf")
      (:file "troper"
        :depends-on ("transl"))
      (:file "trutil"
        :depends-on ("transl"))
      (:file "trmode"
        :depends-on ("transl"))
      (:file "trdata")
      (:file "trpred")
      (:file "transq")
      (:file "acall")
      (:file "fcall")
      (:file "evalw")
      (:file "trprop")
      (:file "mdefun")))
   (:module numerical-utilities
     :pathname ""
     :depends-on (simp-utilities      ; for add/mul/sub
                  globals             ; $numer, errorsw
                  utilities)          ; merror
     :components ((:file "numerical-utils")))
   (:module bessel-functions
     :pathname ""
     :depends-on (globals defmfun trigonometry algebraic-database
                  utility-macros reader fundamental-macros other-macros
                  prerequisites numerical-utilities)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components ((:file "bessel")
                  (:file "airy")))
   (:module elliptic-functions
     :pathname ""
     :depends-on (globals defmfun trigonometry algebraic-database
                  utility-macros reader fundamental-macros other-macros
                  prerequisites numerical-utilities)
     :components ((:file "ellipt")))
   (:module numerical-functions
     :pathname ""
     :depends-on (globals defmfun algebraic-database
                  utility-macros reader fundamental-macros other-macros
                  prerequisites)
     :components ((:file "intpol")))
   (:module display
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 declarations
                  fundamental-macros prerequisites simp-utilities)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components ((:file "mgrind")
                  (:file "grind"
                    :depends-on ("mgrind"))
                  (:file "displa"
                    :depends-on ("mgrind" "grind"))))
   (:module gcd
     :pathname ""
     :depends-on (globals defmfun declarations destructuring-let
                  fundamental-macros prerequisites
                  rat-macros utilities)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "spgcd")
                  (:file "ezgcd")))
   (:module documentation
     :pathname ""
     :depends-on (globals defmfun fundamental-macros)
     :components ((:file "verify-html-index")
                  (:file "macdes"
                    :depends-on ("verify-html-index"))))
   (:module algebraic-database
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 declarations
                  evaluator fundamental-macros other-macros
                  prerequisites utility-macros)
     ;; Compile-time deps form a DAG.  Two runtime cycles are
     ;; structural: compar <-> db (sign/comparison logic vs the
     ;; fact database) and askp <-> compar (interactive
     ;; integer/parity queries vs the predicates that prompt
     ;; them).
     :components ((:file "inmis")
                  (:file "db")
                  (:file "compar")
                  (:file "askp")))
   (:module variable-predicates
     :pathname ""
     ;; Variable predicates like FREEVAR, VARP.
     :depends-on (simp-utilities)
     :components ((:file "var-predicate")))
   (:module hypergeometric
     :pathname ""
     ;; Simplification and evaluation of hypergeometric functions.
     :depends-on (compatibility-macros1 declarations defmfun errset globals
                  miscellaneous other-macros prerequisites
                  utility-macros m2-pattern-matcher
                  variable-predicates)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components
     ((:file "hyp")
      ;; Note: nummod uses apply-reflection-rule from trigi.
      ;; This should be updated.
      (:file "nummod")
      (:file "hypergeometric"
        :depends-on ("nummod"))
      (:file "nfloat"
        ;; Comment in nfloat says hypergeometric needs to
        ;; be loaded first.
        :depends-on ("hypergeometric"))))
   (:module integration
     :pathname ""
     :depends-on (globals defmfun compatibility-macros declarations
                  destructuring-let errset fundamental-macros
                  prerequisites rat-macros utility-macros
                  m2-pattern-matcher variable-predicates)
     ;; Compile-time deps form a DAG.  Runtime: risch.lisp and
     ;; sin.lisp are mutually recursive by design -- risch's
     ;; hypertrigint1 falls back to sin's sinint when Risch
     ;; can't make progress, and sin's integrate1 dispatches to
     ;; risch's rischint.  The cycle is structural; not broken.
     ;;
     ;; This could be fixed by moving lots of code around to a
     ;; separate file.
     :components
     ((:file "sin-util")
      (:file "sinint")
      (:file "risch"
        :depends-on ("sinint"))
      (:file "irinte"
        :depends-on ("sin-util"))
      (:file "sin"
        :depends-on ("sin-util" "irinte"))))
   (:module taylor-series
     :pathname ""
     :depends-on (globals defmfun compatibility-macros
                  compatibility-macros1 declarations
                  destructuring-let fundamental-macros
                  other-macros prerequisites rat-macros errset
                  simp-utilities)
     :components ((:file "hayat")))
   (:module definite-integration
     :pathname ""
     :depends-on (globals defmfun declarations destructuring-let
                  fundamental-macros other-macros
                  prerequisites hypergeometric
                  m2-pattern-matcher variable-predicates)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components
     ((:file "residu")
      (:file "defint"
        :depends-on ("residu"))
      (:file "hypgeo"
        :depends-on ("residu"))
      (:file "laplac"
        :depends-on ("defint" "hypgeo" "residu"))))
   (:module trigonometry
     :pathname ""
     :depends-on (globals defmfun compatibility-macros declarations
                  errset fundamental-macros other-macros
                  prerequisites utility-macros taylor-series
                  m2-pattern-matcher)
     ;; Compile-time deps form a DAG.  Runtime cycle trigi <->
     ;; trigo is structural (trigi calls trigo:trigexpand for
     ;; forward simplification, trigo calls trigi for shared
     ;; eval/reflection helpers).
     :components ((:file "trigi")
                  (:file "trigo")
                  (:file "trgred")))
   (:module special-functions
     :pathname ""
     :depends-on (globals defmfun reader utility-macros
                  other-macros rat-macros numerical-utilities)
     :components ((:file "specfn")))
   (:module matrix-algebra
     :pathname ""
     :depends-on (globals defmfun compatibility-macros compatibility-macros1
                  declarations fundamental-macros
                  maxima-language-compiler-macros prerequisites
                  utility-macros destructuring-let utilities)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components ((:file "mat")
                  (:file "matrix"
                    :depends-on ("mat"))
                  (:file "linnew"
                    :depends-on ("mat" "matrix"))))
   (:module determinants
     :pathname ""
     :depends-on (globals defmfun declarations
                  maxima-language-compiler-macros prerequisites)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "sprdet")
                  (:file "newinv")
                  (:file "newdet")))
   (:module pattern-matching
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 declarations
                  evaluator fundamental-macros prerequisites
                  rat-macros simp-utilities)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "matcom")
                  (:file "matrun")
                  (:file "nisimp")))
   (:module limits
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 declarations
                  destructuring-let errset fundamental-macros
                  other-macros prerequisites utilities utility-macros)
     ;; Compile-time deps form a DAG.  Runtime cycle limit <->
     ;; tlimit is structural (mutual dispatch between limit and
     ;; Taylor-limit).
     :components ((:file "tlimit")
                  (:file "limit")))
   (:module solve
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 compatibility-macros
                  declarations fundamental-macros other-macros
                  prerequisites rat-macros utilities utility-macros)
     ;; Compile-time deps form a DAG.  Two runtime call cycles
     ;; remain by design: solve <-> psolve (cubic/quartic
     ;; formulas tightly coupled to solve's display
     ;; machinery), and solve <-> algsys (univariate vs
     ;; system-of-equations dispatchers calling each other).
     ;; sqrtdenest, polyrz, and cpoly are leaves, called from
     ;; algsys.
     :components
     ((:file "sqrtdenest")
      (:file "polyrz")
      (:file "cpoly")
      (:file "solve")
      (:file "psolve"
        :depends-on ("solve"))
      (:file "algsys"
        :depends-on ("solve" "sqrtdenest" "polyrz" "cpoly"))))
   (:module debugging
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1 compatibility-macros
                  declarations fundamental-macros prerequisites
                  utility-macros)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "mtrace")
                  (:file "mdebug")))
   (:module gamma-expint
     :pathname ""
     :depends-on (numerical-utilities m2-pattern-matcher)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components ((:file "expintegral")
                  (:file "gamma"
                    :depends-on ("expintegral"))
                  (:file "plasma")))
   (:module random
     :pathname ""
     ;; Mersenne Twister random-number implementation.
     :depends-on (globals utilities)
     :components ((:file "rand-mt19937")))
   (:module miscellaneous
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1
                  reader utility-macros commands
                  destructuring-let errset other-macros
                  rat-macros declarations fundamental-macros
                  numerical-utilities m2-pattern-matcher random)
     :components ((:file "scs")
                  (:file "asum")
                  (:file "fortra")
                  (:file "optim")
                  (:file "marray")
                  (:file "mdot")
                  (:file "series")
                  (:file "numth")
                  (:file "pade")
                  (:file "homog")
                  (:file "combin")
                  ;; nset needs $random.
                  (:file "nset")
                  (:file "maxmin")
                  (:file "conjugate")
                  (:file "hstep")
                  (:file "sinc")
                  (:file "mstuff")))
   (:module polynomial
     :pathname ""
     :depends-on (defmfun)
     :components ((:file "polynomialp")))
   (:module poisson-series
     :pathname ""
     :depends-on (defmfun declarations fundamental-macros prerequisites)
     ;; Module is a clean DAG at every level: function calls, macros,
     ;; special-variable references.
     :components ((:file "pois2")
                  (:file "pois3"
                    :depends-on ("pois2"))))
   (:module translated-packages
     :pathname ""
     :depends-on (globals maxima-language-compiler-macros compatibility-macros1
                  defmfun declarations errset miscellaneous
                  other-macros prerequisites utility-macros)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.
     :components ((:file "desoln")
                  (:file "elim")
                  (:file "invert")
                  (:file "todd-coxeter")
                  ;; plotcolors is the lowest layer of the plotting
                  ;; stack and must precede plot.lisp.
                  (:file "plotcolors")
                  (:file "plot"
                    :depends-on ("plotcolors"))))
   (:module graphics-drivers
     :pathname ""
     ;; Every driver calls into plot.lisp and plotcolors.lisp.
     :depends-on (defmfun translated-packages)
     ;; Module is a clean DAG at every level: function calls,
     ;; macros, special-variable references.  The drivers are
     ;; independent of one another.
     :components ((:file "gnuplot_def")
                  (:file "xmaxima_def")
                  (:file "geomview_def")))
   (:module final
     :pathname ""
     :depends-on (globals defmfun compatibility-macros1)
     ;; These are not compiled, for whatever reason
     :components ((:file "autol")
                  (:file "max_ext")
                  (:file "../lisp-utils/defsystem") ;; some share packages use defsystem
                  (:file "init-cl"))))
  ;;:serial t
  :depends-on ())
