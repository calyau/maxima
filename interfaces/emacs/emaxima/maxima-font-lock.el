;;; maxima-font-lock.el --- syntax highlighting for maxima.el

;; Copyright: (C) 2001 Jay Belanger

;; Author: Jay Belanger <belanger@truman.edu>
;; Keywords: maxima, font-lock

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>
;; The latest version of this package should be available at
;; ftp://vh213601.truman.edu/pub/Maxima

;;; Commentary

;;; This file is used for font-lock for maxima.el
;;
;; The keywords are divided into the following groups, following the 
;; Maxima info files:
;; Prompts (inferior-maxima-prompt-face)
;; Functions (maxima-function-face)
;; Variables (maxima-variable-face)
;; Constants (maxima-constant-face)
;; Keywords (maxima-keyword-face)
;; Declarations (maxima-declaration-face)
;; Operators (maxima-operator-face)
;; Property (maxima-property-face)
;; Macros (maxima-macro-face)
;; Special operators (maxima-specop-face)
;; Special symbols (maxima-specsymb-face)
;;
;; There is also inferior-maxima-warning-face for errors in 
;; Maxima buffer.

;;; Code

(require 'font-lock)
(provide 'maxima-font-lock)


;;; The faces
(defvar maxima-variable-face 'maxima-variable-face
  "The face to use for the variables.")

(defvar maxima-function-face 'maxima-function-face
  "The face to use for the functions.")

(defvar maxima-constant-face 'maxima-constant-face
  "The face to use for the constants.")

(defvar maxima-keyword-face 'maxima-keyword-face
  "The face to use for the keywords.")

(defvar maxima-operator-face 'maxima-operator-face
  "The face to use for the operators.")

(defvar maxima-property-face 'maxima-property-face
  "The face to use for the properties.")

(defvar maxima-macro-face 'maxima-macro-face
  "The face to use for the macros.")

(defvar maxima-specop-face 'maxima-specop-face
  "The face to use for the special operators.")

(defvar maxima-declaration-face 'maxima-declaration-face
  "The face to use for the macros.")

(defvar maxima-specsymb-face 'maxima-specsymb-face
  "The face to use for the special symbols.")

(defvar inferior-maxima-prompt-face 'inferior-maxima-prompt-face
  "The face to use for the prompts.")

(defvar inferior-maxima-warning-face 'inferior-maxima-warning-face
  "The face to use for the warnings.")

;;; The regexps
(defvar maxima-match-variables-1
  (concat "\\<\\(" 
          (regexp-opt '(
                        "%"
                        "%%"
                        "%EDISPFLAG"
                        "%RNUM_LIST"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima variables.")

(defvar maxima-match-variables-2
  (concat "\\<\\("
          (regexp-opt '(
                        "ALL_DOTSIMP_DENOMS"
                        "ASSUME_POS"
                        "ASSUME_POS_PRED"
                        "CHANGE_FILEDEFAULTS"
                        "CURRENT_LET_RULE_PACKAGE"
                        "DEFAULT_LET_RULE_PACKAGE"
                        "DISPLAY_FORMAT_INTERNAL"
                        "ERROR_SIZE"
                        "ERROR_SYMS"
                        "EXPANDWRT_DENOM"
                        "FILE_SEARCH"
                        "FILE_STRING_PRINT"
                        "IN_NETMATH"
                        "INTEGRATION_CONSTANT_COUNTER"
                        "ISOLATE_WRT_TIMES"
                        "LET_RULE_PACKAGES"
                        "LINSOLVE_PARAMS"
                        "MATRIX_ELEMENT_ADD"
                        "MATRIX_ELEMENT_MULT"
                        "MATRIX_ELEMENT_TRANSPOSE"
                        "MODE_CHECKP"
                        "MODE_CHECK_ERRORP"
                        "MODE_CHECK_WARNP"
                        "PLOT_OPTIONS"
                        "SOLVE_INCONSISTENT_ERROR"
                        "SUBLIS_APPLY_LAMBDA"
                        "TAYLOR_LOGEXPAND"
                        "TAYLOR_ORDER_COEFFICIENTS"
                        "TAYLOR_TRUNCATE_POLYNOMIALS"
                        "TIMER_DEVALUE"
                        "TR_ARRAY_AS_REF"
                        "TR_BOUND_FUNCTION_APPLYP"
                        "TR_FILE_TTY_MESSAGESP"
                        "TR_FLOAT_CAN_BRANCH_COMPLEX"
                        "TR_FUNCTION_CALL_DEFAULT"
                        "TR_GEN_TAGS"
                        "TR_NUMER"
                        "TR_OPTIMIZE_MAX_LOOP"
                        "TR_OUTPUT_FILE_DEFAULT"
                        "TR_PREDICATE_BRAIN_DAMAGE"
                        "TR_SEMICOMPILE"
                        "TR_STATE_VARS"
                        "TR_TRUE_NAME_OF_FILE_BEING_TRANSLATED"
                        "TR_VERSION"
                        "TR_WARN_BAD_FUNCTION_CALLS"
                        "TR_WARN_FEXPR"
                        "TR_WARN_MEVAL"
                        "TR_WARN_MODE"
                        "TR_WARN_UNDECLARED"
                        "TR_WARN_UNDEFINED_VARIABLE"
                        "TR_WINDY"
                        "USE_FAST_ARRAYS"
                        )) 
          "\\)\\>")
  "Regexp to match the Maxima variables.")

(defvar maxima-match-variables-3
  (concat "\\<\\("
          (regexp-opt '(
                        "ABSBOXCHAR"
                        "ACTIVECONTEXTS"
                        "ALGEBRAIC"
                        "ALGEPSILON"
                        "ALGEXACT"
                        "ALIASES"
                        "ALLSYM"
                        "ARRAYS"
                        "ASKEXP"
                        "ASSUMESCALAR"
                        "BACKSUBST"
                        "BACKTRACE"
                        "BATCHKILL"
                        "BATCOUNT"
                        "BERLEFACT"
                        "BFTORAT"
                        "BFTRUNC"
                        "BOTHCASES"
                        "BOXCHAR"
                        "BREAKUP"
                        "CAUCHYSUM"
                        "CFLENGTH"
                        "COMPGRIND"
                        "CONTEXT"
                        "CONTEXTS"
                        "COUNTER"
                        "CURSORDISP"
                        "DEBUGMODE"
                        "DEMOIVRE"
                        "DEPENDENCIES"
                        "DERIVABBREV"
                        "DERIVSUBST"
                        "DETOUT"
                        "DIAGMETRIC"
                        "DIM"
                        "DIREC"
                        "DISPFLAG"
                        "DISPLAY2D"
                        "DOALLMXOPS"
                        "DOMAIN"
                        "DOMXEXPT"
                        "DOMXMXOPS"
                        "DOMXNCTIMES"
                        "DONTFACTOR"
                        "DOSCMXOPS"
                        "DOSCMXPLUS"
                        "DOT0NSCSIMP"
                        "DOT0SIMP"
                        "DOT1SIMP"
                        "DOTASSOC"
                        "DOTCONSTRULES"
                        "DOTDISTRIB"
                        "DOTEXPTSIMP"
                        "DOTIDENT"
                        "DOTSCRULES"
                        "DSKALL"
                        "ERFFLAG"
                        "ERREXP"
                        "ERRINTSCE"
                        "ERRORFUN"
                        "EVFLAG"
                        "EVFUN"
                        "EXPTDISPFLAG"
                        "EXPON"
                        "EXPONENTIALIZE"
                        "EXPOP"
                        "EXPTISOLATE"
                        "EXPTSUBST"
                        "FACEXPAND"
                        "FACTLIM"
                        "FACTORFLAG"
                        "FILENAME"
                        "FILENUM"
                        "FLOAT2BF"
                        "FORTINDENT"
                        "FORTSPACES"
                        "FPPREC"
                        "FPPRINTPREC"
                        "FUNCTIONS"
                        "GAMMALIM"
                        "GENINDEX"
                        "GENSUMNUM"
                        "GLOBALSOLVE"
                        "GRADEFS"
                        "HALFANGLES"
                        "IBASE"
                        "IEQNPRINT"
                        "INCHAR"
                        "INFLAG"
                        "INTFACLIM"
                        "INFOLISTS"
                        "INTPOLABS"
                        "INTPOLERROR"
                        "INTPOLREL"
                        "KEEPFLOAT"
                        "LASTTIME"
                        "LETRAT"
                        "LHOSPITALLIM"
                        "LINECHAR"
                        "LINEDISP"
                        "LINEL"
                        "LINENUM"
                        "LINSOLVEWARN"
                        "LISTARITH"
                        "LISTCONSTVARS"
                        "LISTDUMMYVARS"
                        "LMXCHAR"
                        "LOADPRINT"
                        "LOGABS"
                        "LOGARC"
                        "LOGCONCOEFFP"
                        "LOGEXPAND"
                        "LOGNEGINT"
                        "LOGNUMER"
                        "LOGSIMP"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima variables.")

(defvar maxima-match-variables-4
  (concat "\\<\\("
          (regexp-opt '(
                        "M1PBRANCH"
                        "MACROEXPANSION"
                        "MAPERROR"
                        "MAXAPPLYDEPTH"
                        "MAXAPPLYHEIGHT"
                        "MAXNEGEX"
                        "MAXPOSEX"
                        "MAXPRIME"
                        "MAXTAYORDER"
                        "MODULUS"
                        "MULTIPLICITIES"
                        "MYOPTIONS"
                        "NEGDISTRIB"
                        "NEGSUMDISPFLAG"
                        "NEWFAC"
                        "NICEINDICESPREF"
                        "NOLABELS"
                        "NOUNDISP"
                        "OBASE"
                        "OMEGA"
                        "OPPROPERTIES"
                        "OPSUBST"
                        "OPTIMPREFIX"
                        "OPTIONSET"
                        "PACKAGEFILE"
                        "PARSEWINDOW"
                        "PARTSWITCH"
                        "PFEFORMAT"
                        "PIECE"
                        "POISLIM"
                        "POWERDISP"
                        "PREDERROR"
                        "PRODHACK"
                        "PROGRAMMODE"
                        "PROMPT"
                        "PSEXPAND"
                        "RADEXPAND"
                        "RADPRODEXPAND"
                        "RATALGDENOM"
                        "RATDENOMDIVIDE"
                        "RATEPSILON"
                        "RATEINSTEIN"
                        "RATFAC"
                        "RATMX"
                        "RATPRINT"
                        "RATRIEMAN"
                        "RATRIEMANN"
                        "RATSIMPEXPONS"
                        "RATWEIGHTS"
                        "RATWEYL"
                        "RATWTLVL"
                        "REALONLY"
                        "REFCHECK"
                        "RMXCHAR"
                        "ROMBERGABS"
                        "ROMBERGIT"
                        "ROMBERGMIN"
                        "ROMBERGTOL"
                        "ROOTSCONMODE"
                        "ROOTSEPSILON"
                        "SAVEDEF"
                        "SAVEFACTORS"
                        "SCALARMATRIXP"
                        "SETCHECK"
                        "SETCHECKBREAK"
                        "SETVAL"
                        "SHOWTIME"
                        "SIMPSUM"
                        "SOLVEDECOMPOSES"
                        "SOLVEEXPLICIT"
                        "SOLVEFACTORS"
                        "SOLVENULLWARN"
                        "SOLVERADCAN"
                        "SOLVETRIGWARN"
                        "SPARSE"
                        "SQRTDISPFLAG"
                        "STARDISP"
                        "SUMEXPAND"
                        "SUMHACK"
                        "SUMSPLITFACT"
                        "TAYLORDEPTH"
                        "TLIMSWITCH"
                        "TRANSBIND"
                        "TRANSCOMPILE"
                        "TRANSRUN"
                        "TRIGEXPANDPLUS"
                        "TRIGEXPANDTIMES"
                        "TRIGINVERSES"
                        "TRIGSIGN"
                        "TTYINTFUN"
                        "TTYINTNUM"
                        "TTYOFF"
                        "UNDECLAREDWARN"
                        "VALUES"
                        "VECT_CROSS"
                        "VERBOSE"
                        "ZEROBERN"
                        "ZETA%PI"
                        "ZUNDERFLOW"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima variables.")

(defvar maxima-match-functions-1
  (concat "\\<\\("
          (regexp-opt '(
                        "%"
                        "%TH"
                        "%J"
                        "%K"
                        "?ROUND"
                        "?TRUNCATE"
                        ))
          "\\)\\>" )
  "Regexp to match the Maxima functions.")

(defvar maxima-match-functions-2
  (concat "\\<\\("
          (regexp-opt '(
                        "ACOS"
                        "ACOSH"
                        "ACOT"
                        "ACOTH"
                        "ACSC"
                        "ACSCH"
                        "ACTIVATE"
                        "ADDCOL"
                        "ADDROW"
                        "ADJOINT"
                        "AIRY"
                        "ALARMCLOCK"
                        "ALGSYS"
                        "ALIAS"
                        "ALLOC"
                        "ALLROOTS"
                        "ANTID"
                        "ANTIDIFF"
                        "APPEND"
                        "APPENDFILE"
                        "APPLY"
                        "APPLY1"
                        "APPLY2"
                        "APPLYB1"
                        "APPLY_NOUNS"
                        "APROPOS"
                        "ARGS"
                        "ARRAY"
                        "ARRAYAPPLY"
                        "ARRAYINFO"
                        "ARRAYMAKE"
                        "ASEC"
                        "ASECH"
                        "ASIN"
                        "ASINH"
                        "ASKINTEGER"
                        "ASKSIGN"
                        "ASSUME"
                        "ASYMP"
                        "ASYMPA"
                        "AT"
                        "ATAN"
                        "ATAN2"
                        "ATANH"
                        "ATOM"
                        "ATRIG1"
                        "ATVALUE"
                        "AUGCOEFMATRIX"
                        "BACKUP"
                        "BASHINDICES"
                        "BATCH"
                        "BATCHLOAD"
                        "BATCON"
                        "BERN"
                        "BERNPOLY"
                        "BESSEL"
                        "BETA"
                        "BEZOUT"
                        "BFFAC"
                        "BFLOAT"
                        "BFLOATP"
                        "BFPSI"
                        "BFZETA"
                        "BGZETA"
                        "BHZETA"
                        "BINDTEST"
                        "BINOMIAL"
                        "BLOCK"
                        "BOTHCOEF"
                        "BOX"
                        "BREAK"
                        "BUG"
                        "BURN"
                        "BZETA"
                        "CABS"
                        "CANFORM"
                        "CANTEN"
                        "CARG"
                        "CARTAN"
                        "CATCH"
                        "CBFAC"
                        "CF"
                        "CFDISREP"
                        "CFEXPAND"
                        "CGAMMA"
                        "CGAMMA2"
                        "CHANGEVAR"
                        "CHARPOLY"
                        "CHECK_OVERLAPS"
                        "CHR1"
                        "CHR2"
                        "CHRISTOF"
                        "CLEARSCREEN"
                        "CLOSEFILE"
                        "CLOSEPS"
                        "COEFF"
                        "COEFMATRIX"
                        "COL"
                        "COLLAPSE"
                        "COLUMNVECTOR"
                        "COMBINE"
                        "COMP2PUI"
                        "COMPFILE"
                        "COMPILE"
                        "COMPILE_FILE"
                        "COMPILE_LISP_FILE"
                        "CONCAT"
                        "CONJUGATE"
                        "CONS"
                        "CONSTANTP"
                        "CONT2PART"
                        "CONTENT"
                        "CONTINUE"
                        "CONTRACT"
                        "CONTRACT"
                        "COPYLIST"
                        "COPYMATRIX"
                        "COS"
                        "COSH"
                        "COT"
                        "COTH"
                        "COVDIFF"
                        "CREATE_LIST"
                        "CSC"
                        "CSCH"
                        "CURVATURE"
                        "DBLINT"
                        "DDT"
                        "DEACTIVATE"
                        "DEBUG"
                        "DEBUGPRINTMODE"
                        "DECLARE"
                        "DECLARE_TRANSLATED"
                        "DECLARE_WEIGHT"
                        "DEFCON"
                        "DEFINE"
                        "DEFINE_VARIABLE"
                        "DEFINT"
                        "DEFMATCH"
                        "DEFRULE"
                        "DEFTAYLOR"
                        "DELETE"
                        "DELFILE"
                        "DELTA"
                        "DEMO"
                        "DENOM"
                        "DEPENDS"
                        "DERIVDEGREE"
                        "DERIVLIST"
                        "DESCRIBE"
                        "DESOLVE"
                        "DETERMINANT"
                        "DIAGMATRIX"
                        "DIFF"
                        "DIMENSION"
                        "DIRECT"
                        "DISKFREE"
                        "DISOLATE"
                        "DISP"
                        "DISPCON"
                        "DISPFORM"
                        "DISPFUN"
                        "DISPLAY"
                        "DISPRULE"
                        "DISPTERMS"
                        "DISTRIB"
                        "DIVIDE"
                        "DIVSUM"
                        "DOTSIMP"
                        "DPART"
                        "DSCALAR"
                        "DUMMY"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima functions.")

(defvar maxima-match-functions-3
  (concat "\\<\\("
          (regexp-opt '(
                        "ECHELON"
                        "EIGENVALUES"
                        "EIGENVECTORS"
                        "EINSTEIN"
                        "ELE2COMP"
                        "ELE2POLYNOME"
                        "ELE2PUI"
                        "ELEM"
                        "ELIMINATE"
                        "EMATRIX"
                        "ENDCONS"
                        "ENTERMATRIX"
                        "ENTIER"
                        "EQUAL"
                        "ERF"
                        "ERRCATCH"
                        "ERROR"
                        "ERRORMSG"
                        "EULER"
                        "EV"
                        "EVAL"
                        "EVENP"
                        "EXAMPLE"
                        "EXP"
                        "EXPAND"
                        "EXPANDWRT"
                        "EXPANDWRT_FACTORED"
                        "EXPLOSE"
                        "EXPRESS"
                        "EXPT"
                        "EXTRACT_LINEAR_EQUATIONS"
                        "EZGCD"
                        "FACTCOMB"
                        "FACTOR"
                        "FACTORIAL"
                        "FACTOROUT"
                        "FACTORSUM"
                        "FACTS"
                        "FASSAVE"
                        "FASTTIMES"
                        "FAST_CENTRAL_ELEMENTS"
                        "FAST_LINSOLVE"
                        "FEATUREP"
                        "FFT"
                        "FIB"
                        "FIBTOPHI"
                        "FILEDEFAULTS"
                        "FILENAME_MERGE"
                        "FILE_TYPE"
                        "FILLARRAY"
                        "FIRST"
                        "FIX"
                        "FLOAT"
                        "FLOATDEFUNK"
                        "FLOATNUMP"
                        "FLUSH"
                        "FLUSHD"
                        "FLUSHND"
                        "FORGET"
                        "FORTMX"
                        "FORTRAN"
                        "FREEOF"
                        "FULLMAP"
                        "FULLMAPL"
                        "FULLRATSIMP"
                        "FULLRATSUBST"
                        "FUNCSOLVE"
                        "FUNDEF"
                        "FUNMAKE"
                        )) 
          "\\)\\>")
  "Regexp to match the Maxima functions.")

(defvar maxima-match-functions-4
  (concat "\\<\\("
          (regexp-opt '(
                        "GAMMA"
                        "GAUSS"
                        "GCD"
                        "GCFACTOR"
                        "GENDIFF"
                        "GENFACT"
                        "GENMATRIX"
                        "GET"
                        "GETCHAR"
                        "GFACTOR"
                        "GFACTORSUM"
                        "GO"
                        "GRADEF"
                        "GRAMSCHMIDT"
                        "GRIND"
                        "GROBNER_BASIS"
                        "HACH"
                        "HIPOW"
                        "HORNER"
                        "IC1"
                        "IDENT"
                        "IEQN"
                        "IFT"
                        "ILT"
                        "IMAGPART"
                        "INDICES"
                        "INFIX"
                        "INNERPRODUCT"
                        "INPART"
                        "INRT"
                        "INTEGERP"
                        "INTEGRATE"
                        "INTERPOLATE"
                        "INTOPOIS"
                        "INTOSUM"
                        "INTSCE"
                        "INVERT"
                        "IS"
                        "ISOLATE"
                        "ISQRT"
                        "JACOBI"
                        "KDELTA"
                        "KILL"
                        "KILLCONTEXT"
                        "KOSTKA"
                        "LABELS"
                        "LAPLACE"
                        "LAST"
                        "LC"
                        "LCM"
                        "LDEFINT"
                        "LDISP"
                        "LDISPLAY"
                        "LENGTH"
                        "LET"
                        "LETRULES"
                        "LETSIMP"
                        "LGTREILLIS"
                        "LHS"
                        "LIMIT"
                        "LINSOLVE"
                        "LISPDEBUGMODE"
                        "LISTARRAY"
                        "LISTOFVARS"
                        "LISTP"
                        "LIST_NC_MONOMIALS"
                        "LOAD"
                        "LOADFILE"
                        "LOCAL"
                        "LOG"
                        "LOGCONTRACT"
                        "LOPOW"
                        "LORENTZ"
                        "LPART"
                        "LRATSUBST"
                        "LRICCICOM"
                        "LTREILLIS"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima functions.")

(defvar maxima-match-functions-5
  (concat "\\<\\("
          (regexp-opt '(
                        "MAKEBOX"
                        "MAKEFACT"
                        "MAKEGAMMA"
                        "MAKELIST"
                        "MAKE_ARRAY"
                        "MAP"
                        "MAPATOM"
                        "MAPLIST"
                        "MATCHDECLARE"
                        "MATCHFIX"
                        "MATRIX"
                        "MATRIXMAP"
                        "MATRIXP"
                        "MATTRACE"
                        "MAX"
                        "MEMBER"
                        "METRIC"
                        "MIN"
                        "MINFACTORIAL"
                        "MINOR"
                        "MOD"
                        "MODE_DECLARE"
                        "MODE_IDENTITY"
                        "MON2SCHUR"
                        "MONO"
                        "MONOMIAL_DIMENSIONS"
                        "MOTION"
                        "MULTINOMIAL"
                        "MULTI_ELEM"
                        "MULTI_ORBIT"
                        "MULTI_PUI"
                        "MULTSYM"
                        "MULTTHRU"
                        "NCEXPT"
                        "NCHARPOLY"
                        "NC_DEGREE"
                        "NEW-DISREP"
                        "NEWCONTEXT"
                        "NEWDET"
                        "NEWTON"
                        "NICEINDICES"
                        "NONSCALARP"
                        "NOSTRING"
                        "NOUNIFY"
                        "NROOTS"
                        "NTERMS"
                        "NTERMSG"
                        "NTERMSRCI"
                        "NTHROOT"
                        "NUM"
                        "NUMBERP"
                        "NUMERVAL"
                        "NUMFACTOR"
                        "NUSUM"
                        "NZETA"
                        "ODDP"
                        "ODE"
                        "ODE2"
                        "OPENPLOT_CURVES"
                        "OPTIMIZE"
                        "ORBIT"
                        "ORDERGREAT"
                        "ORDERGREATP"
                        "ORDERLESS"
                        "ORDERLESSP"
                        "OUTCHAR"
                        "OUTOFPOIS"
                        "PADE"
                        "PART"
                        "PART2CONT"
                        "PARTFRAC"
                        "PARTITION"
                        "PARTPOL"
                        "PCOEFF"
                        "PERMANENT"
                        "PERMUT"
                        "PICKAPART"
                        "PLAYBACK"
                        "PLOG"
                        "PLOT2D"
                        "PLOT2D"
                        "PLOT2D_PS"
                        "PLOT3D"
                        "PLOT3D"
                        "POISDIFF"
                        "POISEXPT"
                        "POISINT"
                        "POISMAP"
                        "POISPLUS"
                        "POISSIMP"
                        "POISSUBST"
                        "POISTIMES"
                        "POISTRIM"
                        "POLARFORM"
                        "POLARTORECT"
                        "POLYNOME2ELE"
                        "POTENTIAL"
                        "POWERS"
                        "POWERSERIES"
                        "PRED"
                        "PRIME"
                        "PRIMEP"
                        "PRINT"
                        "PRINTPOIS"
                        "PRINTPROPS"
                        "PRODRAC"
                        "PRODUCT"
                        "PROPERTIES"
                        "PROPS"
                        "PROPVARS"
                        "PSCOM"
                        "PSDRAW_CURVE"
                        "PSI"
                        "PUI"
                        "PUI2COMP"
                        "PUI2ELE"
                        "PUI2POLYNOME"
                        "PUIREDUC"
                        "PUI_DIRECT"
                        "PUT"
                        "QPUT"
                        "QQ"
                        "QUANC8"
                        "QUIT"
                        "QUNIT"
                        "QUOTIENT"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima functions.")

(defvar maxima-match-functions-6
  (concat "\\<\\("
          (regexp-opt '(
                        "RADCAN"
                        "RADSUBSTFLAG"
                        "RAISERIEMANN"
                        "RANDOM"
                        "RANK"
                        "RAT"
                        "RATCOEF"
                        "RATDENOM"
                        "RATDIFF"
                        "RATDISREP"
                        "RATEXPAND"
                        "RATNUMER"
                        "RATNUMP"
                        "RATP"
                        "RATSIMP"
                        "RATSUBST"
                        "RATVARS"
                        "RATWEIGHT"
                        "READ"
                        "READONLY"
                        "REALPART"
                        "REALROOTS"
                        "REARRAY"
                        "RECTFORM"
                        "RECTTOPOLAR"
                        "REM"
                        "REMAINDER"
                        "REMARRAY"
                        "REMBOX"
                        "REMCON"
                        "REMFUNCTION"
                        "REMLET"
                        "REMOVE"
                        "REMRULE"
                        "REMTRACE"
                        "REMVALUE"
                        "RENAME"
                        "RESET"
                        "RESIDUE"
                        "RESOLVANTE"
                        "RESOLVANTE_ALTERNEE1"
                        "RESOLVANTE_BIPARTITE"
                        "RESOLVANTE_DIEDRALE"
                        "RESOLVANTE_KLEIN"
                        "RESOLVANTE_KLEIN3"
                        "RESOLVANTE_PRODUIT_SYM"
                        "RESOLVANTE_UNITAIRE"
                        "RESOLVANTE_VIERER"
                        "REST"
                        "RESTORE"
                        "RESULTANT"
                        "RETURN"
                        "REVEAL"
                        "REVERSE"
                        "REVERT"
                        "RHS"
                        "RICCICOM"
                        "RIEMANN"
                        "RINVARIANT"
                        "RISCH"
                        "RNCOMBINE"
                        "ROMBERG"
                        "ROOM"
                        "ROOTSCONTRACT"
                        "ROW"
                        "SAVE"
                        "SCALARP"
                        "SCALEFACTORS"
                        "SCANMAP"
                        "SCHUR2COMP"
                        "SCONCAT"
                        "SCSIMP"
                        "SCURVATURE"
                        "SEC"
                        "SECH"
                        "SETELMX"
                        "SETUP"
                        "SETUP_AUTOLOAD"
                        "SET_PLOT_OPTION"
                        "SET_UP_DOT_SIMPLIFICATIONS"
                        "SHOW"
                        "SHOWRATVARS"
                        "SIGN"
                        "SIGNUM"
                        "SIMILARITYTRANSFORM"
                        "SIMP"
                        "SIN"
                        "SINH"
                        "SOLVE"
                        "SOMRAC"
                        "SORT"
                        "SPLICE"
                        "SPRINT"
                        "SQFR"
                        "SQRT"
                        "SRRAT"
                        "SSTATUS"
                        "STATUS"
                        "STRING"
                        "STRINGOUT"
                        "SUBLIS"
                        "SUBLIST"
                        "SUBMATRIX"
                        "SUBST"
                        "SUBSTINPART"
                        "SUBSTPART"
                        "SUBVARP"
                        "SUM"
                        "SUMCONTRACT"
                        "SUPCONTEXT"
                        "SYMBOLP"
                        "SYSTEM"
                        "TAN"
                        "TANH"
                        "TAYLOR"
                        "TAYLORINFO"
                        "TAYLORP"
                        "TAYLOR_SIMPLIFIER"
                        "TAYTORAT"
                        "TCL_OUTPUT"
                        "TCONTRACT"
                        "TELLRAT"
                        "TELLSIMP"
                        "TELLSIMPAFTER"
                        "TEX"
                        "THROW"
                        "TIME"
                        "TIMER"
                        "TIMER_INFO"
                        "TLDEFINT"
                        "TLIMIT"
                        "TOBREAK"
                        "TODD_COXETER"
                        "TOPLEVEL"
                        "TOTALDISREP"
                        "TOTIENT"
                        "TO_LISP"
                        "TPARTPOL"
                        "TRACE"
                        "TRACE_OPTIONS"
                        "TRANSFORM"
                        "TRANSLATE"
                        "TRANSLATE_FILE"
                        "TRANSPOSE"
                        "TREILLIS"
                        "TREINAT"
                        "TRIANGULARIZE"
                        "TRIGEXPAND"
                        "TRIGRAT"
                        "TRIGREDUCE"
                        "TRIGSIMP"
                        "TRUNC"
                        "TR_WARNINGS_GET"
                        "TSETUP"
                        "TTRANSFORM"
                        "UNDIFF"
                        "UNITEIGENVECTORS"
                        "UNITVECTOR"
                        "UNKNOWN"
                        "UNORDER"
                        "UNSUM"
                        "UNTELLRAT"
                        "UNTRACE"
                        "VECTORPOTENTIAL"
                        "VECTORSIMP"
                        "VERBIFY"
                        "WEYL"
                        "WRITEFILE"
                        "xgraph_curves"
                        "XTHRU"
                        "ZEROEQUIV"
                        "ZEROMATRIX"
                        "ZETA"
                        "ZRPOLY"
                        "ZSOLVE"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima functions.")

(defvar maxima-match-constants-1
  (concat "\\<"
          (regexp-opt '(
                        "%E"
                        "%PI"
                        ))
          "\\>")
  "Regexp to match the Maxima constants.")

(defvar maxima-match-constants-2
  (concat "\\<\\("
          (regexp-opt '(
                        "FALSE"
                        "INF"
                        "INFINITY"
                        "MINF"
                        "TRUE"
                        ))
          "\\)\\>")
  "Regexp to match the Maxima constants.")

(defvar maxima-match-constants-3
  "\\<\\([0-9]+\\)\\>"
  "Regexp to match the Maxima constants.")

(defvar maxima-match-constants-4
  "\\<\\([0-9]+\.\\)?\\([0-9]+B[+-]?[0-9]\\)\\>"
  "Regexp to match the Maxima constants.")

(defvar maxima-match-keywords
  (concat "\\<\\("
          "ALLBUT"
          "\\)\\>")
  "Regexp to match the Maxima keywords.")

(defvar maxima-match-operators
  (regexp-opt '(
                "'"
                "''"
                "!"
                "!!"
                "#"
                "."
                ":"
                "::"
                "::="
                ":="
                "="
                "<"
                ">"
                "+"
                "-"
                "*"
                "/"
                "^"
                ) t)
  "Regexp to match the Maxima operators.")

(defvar maxima-match-properties
  (concat "\\<\\("
          "ATOMGRAD"
          "\\)\\>")
  "Regexp to match Maxima properties.")

(defvar maxima-match-macros
  (concat "\\<\\("
          (regexp-opt '(
                        "BUILDQ"
                        "WITH_STDOUT"
                        ))
          "\\)\\>")
  "Regexp to match Maxima macros.")

(defvar maxima-match-specops
  (concat "\\<\\("
          (regexp-opt '(
                        "DO"
                        "ELSE"
                        "FOR"
                        "IF"
                        "STEP"
                        "THEN"
                        "THRU"
                        "UNLESS"
                        "WHILE"
                        ))
          "\\)\\>")
  "Regexp to match Maxima special operators.")

(defvar maxima-match-declarations
  (concat "\\<\\("
          (regexp-opt '(
                        "ALPHABETIC"
                        "ANTISYMMETRIC"
                        "COMMUTATIVE"
                        "FEATURE"
                        "FEATURES"
                        "LASSOCIATIVE"
                        "LINEAR"
                        "MAINVAR"
                        "MULTIPLICATIVE"
                        "NONSCALAR"
                        "NOUN"
                        "OUTATIVE"
                        "POSFUN"
                        "RASSOCIATIVE"
                        "SYMMETRIC"
                        ))
          "\\)\\>")
  "Regexp to match Maxima declarations.")

(defvar maxima-match-specsymbs-1
  (concat "\\<\\("
          (regexp-opt '(
                        "ADDITIVE"
                        "CONSTANT"
                        "INFEVAL"
                        "NOEVAL"
                        "NOUNS"
                        "NUMER"
                        "POISSON"
                        "VERB"
                        ))
          "\\)\\>")
  "Regexp to match Maxima special symbols.")

(defvar maxima-match-specsymbs-2
  "\\(\\<\\?\\sw+\\>\\)"
  "Regexp to match Maxima special symbols.")

(defvar inferior-maxima-match-warnings
  "^[a-zA-Z].*$"
  "Regexp to match Maxima warning messages.")

(defvar inferior-maxima-match-prompts
  "^\\((c[0-9]+)\\|(d[0-9]+)\\|(dbm:[0-9]*)\\|MAXIMA>>\\)"
  "Regexp to match the Maxima prompts.")

;;; Now, create the faces.

(defface maxima-function-face
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue" :weight bold))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :weight bold))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight function names."
  :group 'maxima)

(defface maxima-constant-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue" :weight bold))
    (((class color) (background dark)) (:foreground "Aquamarine" :weight bold))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight constants and labels."
  :group 'maxima)

(defface maxima-keyword-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple" :weight bold))
    (((class color) (background dark)) (:foreground "Cyan" :weight bold))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'maxima)

(defface maxima-operator-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid" :weight bold))
    (((class color) (background dark)) (:foreground "LightSteelBlue" :weight bold))
    (t (:bold t)))
  "Font Lock mode face used to highlight builtins."
  :group 'maxima)

(defface maxima-property-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "Gray90" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :weight bold))
    (((class color) (background dark)) (:foreground "PaleGreen" :weight bold))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight type and classes."
  :group 'maxima)

(defface maxima-macro-face
  '((((class color) (background dark)) (:foreground "steelblue1"))
    (((class color) (background light)) (:foreground "blue3"))
    (t (:underline t)))
  "Font Lock Mode face used to highlight preprocessor conditionals."
  :group 'maxima)

(defface maxima-specop-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple" :weight bold))
    (((class color) (background dark)) (:foreground "Cyan" :weight bold))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'maxima)

(defface maxima-declaration-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple" :weight bold))
    (((class color) (background dark)) (:foreground "Cyan" :weight bold))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'maxima)

(defface maxima-specsymb-face
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'maxima)

(defface inferior-maxima-prompt-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple" :weight bold))
    (((class color) (background dark)) (:foreground "Cyan" :weight bold))
    (t (:bold t)))
  "Font Lock mode face used to highlight keywords."
  :group 'maxima)

(defface inferior-maxima-warning-face
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'maxima)

(defface maxima-variable-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod" :italic t))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :italic t))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight variable names."
  :group 'maxima)

;;; Now, the keywords
(defvar maxima-keywords
  `(
    (,maxima-match-operators (0  maxima-operator-face t))
    (,maxima-match-variables-1 (0 maxima-variable-face t))
    (,maxima-match-variables-2 (0 maxima-variable-face t))
    (,maxima-match-variables-3 (0  maxima-variable-face t))
    (,maxima-match-variables-4 (0  maxima-variable-face t))
    (,maxima-match-functions-1 (0  maxima-function-face t))
    (,maxima-match-functions-2 (0  maxima-function-face t))
    (,maxima-match-functions-3 (0  maxima-function-face t))
    (,maxima-match-functions-4 (0  maxima-function-face t))
    (,maxima-match-functions-5 (0  maxima-function-face t))
    (,maxima-match-functions-6 (0  maxima-function-face t))
    (,maxima-match-constants-1 (0  maxima-constant-face t))
    (,maxima-match-constants-2 (0  maxima-constant-face t))
    (,maxima-match-constants-3 (0  maxima-constant-face t))
    (,maxima-match-constants-4 (0  maxima-constant-face t))
    (,maxima-match-keywords (0  maxima-keyword-face t))
    (,maxima-match-properties (0  maxima-property-face t))
    (,maxima-match-macros (0  maxima-macro-face t))
    (,maxima-match-specops (0  maxima-specop-face t))
    (,maxima-match-declarations (0  maxima-declaration-face t))
    (,maxima-match-specsymbs-1 (0  maxima-specsymb-face t))
    (,maxima-match-specsymbs-2 (0  maxima-specsymb-face t)))
  "Keywords for Maxima font-locking.")

(defun maxima-font-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '(maxima-keywords 
          nil t nil maxima-beginning-of-form)))

(add-hook 'maxima-mode-hook 'maxima-font-setup)

(defvar inferior-maxima-keywords
  (append maxima-keywords
          `( 
            (,inferior-maxima-match-prompts (0 inferior-maxima-prompt-face t))
            (,inferior-maxima-match-warnings (0 inferior-maxima-warning-face t))))
  "Keywords for Inferior Maxima font-locking.")

(defun inferior-maxima-font-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '(inferior-maxima-keywords 
          nil t nil maxima-beginning-of-form)))

(add-hook 'inferior-maxima-mode-hook 'inferior-maxima-font-setup)

;;; End of maxima-font-lock.el
