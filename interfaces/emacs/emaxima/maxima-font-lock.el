;;; maxima-font-lock-maker.el --- create maxima-font-lock.el for maxima.el

;; Copyright: (C) 2001 Jay Belanger

;; Author: Jay Belanger <belanger@truman.edu>
;; $Name:  $
;; $Revision: 1.1 $
;; $Date: 2001-11-08 22:23:36 $
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
;; Font-lock faces
;; font-lock-comment-face         For comments
;; font-lock-string-face          For string constants
;; font-lock-keyword-face         For names that have a special syntactic
;;                                 meaning, such as "for" or "if"
;; font-lock-builtin-face         For names of builtin functions
;; font-lock-function-name-face   For names of functions being defined
;; font-lock-variable-name-face   For names of variables being defined
;; font-lock-type-face            For names of user-defined data types,
;;                                 when defined and when used
;; font-lock-constant-face        For constants
;; font-lock-warning-face         For peculiar constructs
;; font-lock-doc-face             For documentation

;; The keywords are divided into the following groups, following the 
;; Maxima info files:
;; Preamble
;; Prompts
;; Functions
;; Variables
;; Constants
;; Keywords
;; Declarations
;; Operators
;; Property
;; Macros
;; Special operators
;; Special symbols

;;; Change Log

;; $Log: maxima-font-lock.el,v $
;; Revision 1.1  2001-11-08 22:23:36  belanger
;; Initial commit of font-lock support for Emacs.
;;
;; Revision 1.3  2001/11/08 16:42:04  jay
;; I moved some font-lock information here from maxima.el.
;;
;; Revision 1.2  2001/10/21 16:54:42  jay
;; I made a small change to (hopefully) fix some font-locking for some
;; constants.
;;
;; Revision 1.1  2001/10/21 05:40:03  jay
;; Initial checkin to CVS.
;;

;;; Code

(require 'font-lock)
(provide 'maxima-font-lock)

(defvar maxima-font-lock-keywords
`(
;;;;; PROMPTS
;;; Use font-lock-keyword-face
(
"^\\((c[0-9]+)\\|(d[0-9]+)\\|(dbm:[0-9]*)\\|MAXIMA>>\\)"
. font-lock-keyword-face)

;;;;; VARIABLES
;;; Use font-lock-variable-name-face
;;; There are too many variables for regexp-opt to handle, so
;;; they are split into <= M and > M
(
,(regexp-opt '(
"%"
"%%"
"%EDISPFLAG"
) 'words)
. font-lock-variable-name-face)

("%RNUM_LIST" . font-lock-variable-name-face)

;; The variable with underscores in their names:
(
,(regexp-opt '(
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
) 'words)
. font-lock-variable-name-face)

(
,(regexp-opt '(
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
) 'words)
. font-lock-variable-name-face)

(
,(regexp-opt '(
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
) 'words)
. font-lock-variable-name-face)

;;;;; FUNCTIONS
;;; Use font-lock-builtin-face
;;; There are too many functions for regexp-opt to handle, so
;;; they are split into <= D, E-L, M-Q, and >= R
(
,(regexp-opt '(
"%"
"%TH"
"%J"
"%K"
"?ROUND"
"?TRUNCATE"
) 'words)
. font-lock-builtin-face)

( 
,(regexp-opt '(
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
) 'words)
. font-lock-builtin-face)

(
,(regexp-opt '(
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
) 'words)
. font-lock-builtin-face)

(
,(regexp-opt '(
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
) 'words)
. font-lock-builtin-face)

(
,(regexp-opt '(
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
) 'words)
. font-lock-builtin-face)

(
,(regexp-opt '(
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
"TEX"
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
) 'words)
. font-lock-builtin-face)

;;;;; CONSTANTS
;;; Use font-lock-constant-face
(
,(regexp-opt '(
"%E"
"%PI"
) t)
. font-lock-constant-face)

(
,(regexp-opt '(
"FALSE"
"INF"
"INFINITY"
"MINF"
"TRUE"
) 'words)
. font-lock-constant-face)

;;;;; KEYWORD
;;; Use font-lock-keyword-face
(
,(regexp-opt '(
"ALLBUT"
) 'words)
. font-lock-keyword-face)

;;;;; OPERATORS
;;; Use font-lock-builtin-face
(
,(regexp-opt '(
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
. font-lock-builtin-face)

;;;;; I doubt that these count as operators, but they should
;;;;; go somewhere
(
,(regexp-opt '(
"("
")"
"["
"]"
) t)
. font-lock-builtin-face)

;;;;; PROPERTY
;;; Use font-lock-type-face
(
,(regexp-opt '(
"ATOMGRAD"
) 'words)
. font-lock-type-face)

;;;;; MACROS
;;; Use font-lock-keyword-face
(
,(regexp-opt '(
"BUILDQ"
"WITH_STDOUT"
) 'words)
. font-lock-keyword-face)

;;;;; SPECIAL OPERATORS
;;; Use font-lock-keyword-face
(
,(regexp-opt '(
"DO"
"ELSE"
"FOR"
"IF"
"STEP"
"THEN"
"THRU"
"UNLESS"
"WHILE"
) 'words)
. font-lock-keyword-face)

;;;;; DECLARATION
;;; Use font-lock-keyword-face
(
,(regexp-opt '(
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
) 'words)
. font-lock-keyword-face)

;;;;; NUMBERS
;;; Use font-lock-constant-face
(
"[0-9]"
. font-lock-constant-face)

;;;;; SPECIAL SYMBOLS
;;; Use font-lock-warning-face
;(
;  (regexp-opt '(
;"["
;"]"
;) t)
;. font-lock-warning-face)

(
,(regexp-opt '(
"ADDITIVE"
"CONSTANT"
"INFEVAL"
"NOEVAL"
"NOUNS"
"NUMER"
"POISSON"
"VERB"
) 'words)
. font-lock-warning-face)
;; Also, ? followed by non-white space
(
"\\<\\?\\sw+\\>\\)"
. font-lock-warning-face)

;;;;; PREAMBLE
;;; (This is not a good way of doing it.)
(
"\\(GCL (GNU Common Lisp)  Version.*\\)\\|\\(Licensed under GNU Library General Public License\\)\\|\\(Contains Enhancements by W. Schelter\\)\\|\\(Maxima .*(with enhancements by W. Schelter).\\)\\|\\(Licensed under the GNU Public License (see file COPYING)\\)"
. font-lock-string-face))
"Keywords for font-locking in Maxima mode.")


(defvar inferior-maxima-font-lock-keywords
  (cons '("^[a-zA-Z].*$" . font-lock-warning-face) 
        maxima-font-lock-keywords))

;;; maxima-font-lock-maker.el ends here
