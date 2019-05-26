dnl Process this file with autoconf to produce a configure script.

dnl If git is available, the version number here is not used.  
dnl Instead, git is used to derive the version based on the closest
dnl annotated branch tag.
dnl
dnl Note: during Lisp-based builds (see INSTALL.lisp) the version
dnl number is read from the following AC_INIT line.  It's sensitive to
dnl whitespace so when updating the version number only make changes
dnl within the second pair of square brackets.
AC_INIT([maxima], [5.43post])
AC_CONFIG_SRCDIR([src/macsys.lisp])
AM_INIT_AUTOMAKE([-Wno-portability tar-ustar])

# On debian-based systems this makes us use the right gcl version out-of-the box.
# On other systems it doesn't hurt.
export GCL_ANSI=t

# This enables running the testbench for "make check" on gcl versions with the old
# memory management (Which in January 2018 includes all Versions shipped with debian
# and Ubuntu)
export GCL_MEM_MULTIPLE=0.1

AM_PATH_LISPDIR

AC_CANONICAL_HOST

dnl If we were using a C compiler, EXEEXT would get defined
dnl automatically. But we aren't, so we have to define it ourselves.
dnl Still to do: Add Win64 support here, if this is needed.
case "$host" in
        *mingw*)
                win32=true
                win64=false
                mingw=true
                EXEEXT=.exe
		CLISPEXT=${EXEEXT}
                ;;
        *cygwin*)
                win32=false
                win64=false
                mingw=false
                EXEEXT=.exe
		CLISPEXT=${EXEEXT}
                ;;
        *)
                win32=false
                win64=false
                mingw=false
                EXEEXT=
		CLISPEXT=.run
esac
AC_SUBST(EXEEXT)
AC_SUBST(win32)
AC_SUBST(win64)
AM_CONDITIONAL(WIN32, test x$win32 = xtrue)
AM_CONDITIONAL(WIN64, test x$win64 = xtrue)

dnl To make life more confusing, the automake test machinery (which we
dnl use in ./tests) needs the am__EXEEXT conditional. This would have
dnl normally been expanded (via something we've copied from Automake's
dnl init.m4 into our aclocal.m4) as Automake computed EXEEXT, but it
dnl didn't so we have to drop this in ourselves.
AM_CONDITIONAL([am__EXEEXT], [test -n "$EXEEXT"])

explicit_lisp=false
lisps_enabled=""

dnl Compiling with abcl requires a jre and the path to abcl.jar
AC_ARG_ENABLE(abcl,
	[  --enable-abcl                Use abcl, requires --with-abcl-jar],
     	[case "${enableval}" in
       		yes) abcl=true 
		     lisps_enabled="${lisps_enabled} abcl"
                     explicit_lisp=true ;;
       		no)  abcl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-abcl) ;;
     	esac
	explicit_lisp=true],
	[abcl=false])
ABCL_JAR=`pwd`/"abcl.jar"
AC_ARG_WITH(abcl-jar,
	[  --with-abcl-jar=<jarfile>    Use <jarfile> as abcl install (default ./abcl.jar)],
	[abcl=true
	lisps_enabled="${lisps_enabled} abcl"
	explicit_lisp=true
	ABCL_JAR="${withval}"])
	
AC_ARG_WITH(jre,
 	[  --with-jre=<jre>             When compiling with abcl: use <jre> as Java interpreter],
 	[JRE="${withval}"],
 	[JRE="java"])

if test x"${abcl}" = xtrue ; then
     if test -e "${ABCL_JAR}" ; then
          echo Using abcl found in ${ABCL_JAR}
     else
        AC_MSG_ERROR(${ABCL_JAR} not found.)
     fi
fi

dnl n.b. clisp_default_name is hardcoded in "with" message
clisp_default_name=clisp
AC_ARG_ENABLE(clisp,
	[  --enable-clisp               Use clisp],
     	[case "${enableval}" in
       		yes) clisp=true 
		     lisps_enabled="${lisps_enabled} clisp"
                     explicit_lisp=true ;;
       		no)  clisp=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-clisp) ;;
     	esac
	explicit_lisp=true],
	[clisp=false])
AC_ARG_WITH(clisp,
	[  --with-clisp=<prog>          Use clisp executable <prog> (default clisp)],
	[clisp=true
	lisps_enabled="${lisps_enabled} clisp"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		CLISP_NAME="${clisp_default_name}"
	else
		CLISP_NAME="$withval"
	fi],
	[CLISP_NAME=${clisp_default_name}])
	CLISP_RUNTIME="lisp${CLISPEXT}"
AC_ARG_WITH(clisp-runtime,
	[  --with-clisp-runtime=<path>  Use clisp runtime <path> (default
	                          *lib-directory*/base/lisp.run on unix,
	   		          *lib-directory*\\lisp.exe on windows) ],
	[clisp=true
	lisps_enabled="${lisps_enabled} clisp"
	CLISP_RUNTIME_PATH="$withval"
	CLISP_RUNTIME=`basename ${CLISP_RUNTIME_PATH}`],
	[])
AC_ARG_ENABLE(clisp-exec,
	[  --enable-clisp-exec          Create a maxima executable image using CLISP.
                                No check is made if the version of 
                                CLISP supports executable images],
	[case "${enableval}" in
	    yes) clisp_exec=true 
                 clisp=true 
		 lisps_enabled="${lisps_enabled} clisp"
                 explicit_lisp=true ;;
	    no) clisp_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-clisp-exec) ;;
         esac
	 CLISP_EXEC=${clisp_exec}],
	[clisp_exec=false
	 CLISP_EXEC=false])
if test x"${clisp}" = xtrue ; then
	if test `echo "$CLISP_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$CLISP_NAME" ; then
			clisp_found=true
		else
			clisp_found=false
		fi
	else
		AC_CHECK_PROG(clisp_found,$CLISP_NAME,true,false)
	fi
	if test x"${clisp_found}" = xfalse ; then
		AC_MSG_WARN(clisp executable ${CLISP_NAME} not found in PATH)
	fi	   
fi


dnl See if git exists.  If it does, update VERSION to include a git tag
AC_CHECK_PROG(git_found, git, true, false)
if test x"${git_found}" = xtrue; then
    # Test to see if git describe works (discarding any output).  If
    # it works, use it.  Otherwise, keep existing value of VERSION.
    # (git describe fails if git is executed in a non-Git directory, e.g.,
    # an unpacked tarball. Since the exit codes of git describe are undocumented,
    # we cannot distinguish that from a missing annotated tag.
    # Play it safe by refusing to change VERSION.)
    if git describe > /dev/null 2>&1; then
        VERSION="`git describe --dirty`"
        # When building RPMs, hyphens in the version are not allowed, so
        # replace them with underscores.
        VERSION=`echo $VERSION | sed 's;-;_;g'`
    fi
fi

dnl Parses the version number for the manual
manual_version=`echo $VERSION | sed 's+branch_++; s+_base++; s+_dirty++; s+_+.+g'`
AC_SUBST(manual_version)

dnl n.b. cmucl_default_name is hardcoded in "with" message
cmucl_default_name=lisp
CMUCL_RUNTIME=lisp
AC_ARG_ENABLE(cmucl,
	[  --enable-cmucl               Use CMUCL],
     	[case "${enableval}" in
       		yes) cmucl=true 
		     lisps_enabled="${lisps_enabled} cmucl"
                     explicit_lisp=true ;;
       		no)  cmucl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-cmucl) ;;
     	esac
	explicit_lisp=true],
	[cmucl=false])
AC_ARG_WITH(cmucl,
	[  --with-cmucl=<prog>          Use CMUCL executable <prog> (default lisp)],
	[cmucl=true
	lisps_enabled="${lisps_enabled} cmucl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		CMUCL_NAME="${cmucl_default_name}"
	else
		CMUCL_NAME="$withval"
	fi],
	[CMUCL_NAME=${cmucl_default_name}])
AC_ARG_WITH(cmucl-runtime,
	[  --with-cmucl-runtime=<path>  Use CMUCL runtime <path> (default
	                          *cmucl-lib*/../bin/lisp)],
	[cmucl=true
	lisps_enabled="${lisps_enabled} cmucl"
	CMUCL_RUNTIME_PATH="$withval"
	CMUCL_RUNTIME=`basename ${CMUCL_RUNTIME_PATH}`],
	[])
AC_ARG_ENABLE(cmucl-exec,
	[  --enable-cmucl-exec          Create a maxima executable image using CMUCL.
                                No check is made if the version of 
                                CMUCL supports executable images],
	[case "${enableval}" in
	    yes) cmucl_exec=true
                 cmucl=true 
		 lisps_enabled="${lisps_enabled} cmucl"
                 explicit_lisp=true ;;
	    no) cmucl_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-cmucl-exec) ;;
         esac
	 CMUCL_EXEC=${cmucl_exec}],
	[cmucl_exec=false
	 CMUCL_EXEC=false])
if test x"${cmucl}" = xtrue ; then
	if test `echo "$CMUCL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$CMUCL_NAME" ; then
			cmucl_found=true
		else
			cmucl_found=false
		fi
	else
		AC_CHECK_PROG(cmucl_found,$CMUCL_NAME,true,false)
	fi
	AC_CHECK_PROG(cmucl_found,$CMUCL_NAME,true,false)
	if test x"${cmucl_found}" = xfalse ; then
		AC_MSG_WARN(cmucl executable ${CMUCL_NAME} not found in PATH)
	fi
fi

dnl n.b. scl_default_name is hardcoded in "with" message
scl_default_name=scl
SCL_RUNTIME=lisp
AC_ARG_ENABLE(scl,
	[  --enable-scl                 Use SCL],
     	[case "${enableval}" in
       		yes) scl=true 
		     lisps_enabled="${lisps_enabled} scl"
                     explicit_lisp=true ;;
       		no)  scl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-scl) ;;
     	esac
	explicit_lisp=true],
	[scl=false])
AC_ARG_WITH(scl,
	[  --with-scl=<prog>            Use SCL executable <prog> (default scl)],
	[scl=true
	lisps_enabled="${lisps_enabled} scl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		SCL_NAME="${scl_default_name}"
	else
		SCL_NAME="$withval"
	fi],
	[SCL_NAME=${scl_default_name}])
AC_ARG_WITH(scl-runtime,
	[  --with-scl-runtime=<path>    Use SCL runtime <path> (default
	                          file://library/../bin/lisp)],
	[scl=true
	lisps_enabled="${lisps_enabled} scl"
	SCL_RUNTIME_PATH="$withval"
	SCL_RUNTIME=`basename ${SCL_RUNTIME_PATH}`],
	[])
if test x"${scl}" = xtrue ; then
	if test `echo "$SCL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$SCL_NAME" ; then
			scl_found=true
		else
			scl_found=false
		fi
	else
		AC_CHECK_PROG(scl_found,$SCL_NAME,true,false)
	fi
	AC_CHECK_PROG(scl_found,$SCL_NAME,true,false)
	if test x"${scl_found}" = xfalse ; then
		AC_MSG_WARN(scl executable ${SCL_NAME} not found in PATH)
	fi
fi

dnl n.b. sbcl_default_name is hardcoded in "with" message
sbcl_default_name=sbcl
AC_ARG_ENABLE(sbcl,
	[  --enable-sbcl                Use SBCL],
     	[case "${enableval}" in
       		yes) sbcl=true 
		     lisps_enabled="${lisps_enabled} sbcl"
                     explicit_lisp=true ;;
       		no)  sbcl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-sbcl) ;;
     	esac
	explicit_lisp=true],
	[sbcl=false])
AC_ARG_WITH(sbcl,
	[  --with-sbcl=<prog>           Use SBCL executable <prog> (default sbcl)],
	[sbcl=true
	lisps_enabled="${lisps_enabled} sbcl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		SBCL_NAME="${sbcl_default_name}"
	else
		SBCL_NAME="$withval"
	fi],
	[SBCL_NAME="${sbcl_default_name}"])
AC_ARG_ENABLE(sbcl-exec,
	[  --enable-sbcl-exec           Create a maxima executable image using SBCL.
                                No check is made if the version of 
                                SBCL supports executable images],
	[case "${enableval}" in
	    yes) sbcl_exec=true 
                 sbcl=true 
		 lisps_enabled="${lisps_enabled} sbcl"
                 explicit_lisp=true ;;
	    no) sbcl_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-sbcl-exec) ;;
         esac
	 SBCL_EXEC=${sbcl_exec}],
	[sbcl_exec=false
	 SBCL_EXEC=false])
if test x"${sbcl}" = xtrue ; then
	if test `echo "$SBCL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$SBCL_NAME" ; then
			sbcl_found=true
		else
			sbcl_found=false
		fi
	else
		AC_CHECK_PROG(sbcl_found,"$SBCL_NAME",true,false)
	fi
	AC_CHECK_PROG(sbcl_found,"$SBCL_NAME",true,false)
	if test x"${sbcl_found}" = xfalse ; then
		AC_MSG_WARN(sbcl executable "${SBCL_NAME}" not found in PATH)
	fi
fi
	
dnl n.b. acl_default_name is hardcoded in "with" message
acl_default_name=lisp
AC_ARG_ENABLE(acl,
	[  --enable-acl                 Use ACL],
     	[case "${enableval}" in
       		yes) acl=true 
		     lisps_enabled="${lisps_enabled} acl"
                     explicit_lisp=true ;;
       		no)  acl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-acl) ;;
     	esac
	explicit_lisp=true],
	[acl=false])
AC_ARG_WITH(acl,
	[  --with-acl=<prog>            Use ACL executable <prog> (default lisp)],
	[acl=true
	lisps_enabled="${lisps_enabled} acl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		ACL_NAME="${acl_default_name}"
	else
		ACL_NAME="$withval"
	fi],
	[ACL_NAME=${acl_default_name}])
if test x"${acl}" = xtrue ; then
	if test `echo "$ACL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$ACL_NAME" ; then
			acl_found=true
		else
			acl_found=false
		fi
	else
		AC_CHECK_PROG(acl_found,$ACL_NAME,true,false)
	fi
	AC_CHECK_PROG(acl_found,$ACL_NAME,true,false)
	if test x"${acl_found}" = xfalse ; then
		AC_MSG_WARN(acl executable ${ACL_NAME} not found in PATH)
	fi
fi

dnl n.b. gcl_default_name is hardcoded in "with" message
gcl_default_name=gcl
AC_ARG_ENABLE(gcl,
	[  --enable-gcl                 Use GCL],
     	[case "${enableval}" in
       		yes) gcl=true 
		     lisps_enabled="${lisps_enabled} gcl"
		     explicit_lisp=true;;
       		no)  gcl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-gcl) ;;
     	esac],
	[gcl=false])
AC_ARG_WITH(gcl,
	[  --with-gcl=<prog>            Use GCL executable <prog> (default gcl)],
	[gcl=true
	lisps_enabled="${lisps_enabled} gcl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		GCL_NAME="${gcl_default_name}"
	else
		GCL_NAME="$withval"
	fi],
	[GCL_NAME=${gcl_default_name}])
if test x"${gcl}" = xtrue ; then
	if test `echo "$GCL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$GCL_NAME" ; then
			gcl_found=true
		else
			gcl_found=false
		fi
	else
		AC_CHECK_PROG(gcl_found,$GCL_NAME,true,false)
	fi
	if test x"${gcl_found}" = xfalse ; then
		AC_MSG_WARN(gcl executable ${GCL_NAME} not found in PATH)
	fi
fi

dnl n.b. openmcl_default_name is hardcoded in "with" message
openmcl_default_name=openmcl
AC_ARG_ENABLE(openmcl,
	[  --enable-openmcl             Use OpenMCL],
     	[case "${enableval}" in
       		yes) openmcl=true 
		     lisps_enabled="${lisps_enabled} openmcl"
		     explicit_lisp=true;;
       		no)  openmcl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-openmcl) ;;
     	esac],
	[openmcl=false])
AC_ARG_WITH(openmcl,
	[  --with-openmcl=<prog>        Use OpenMCL executable <prog> (default openmcl)],
	[openmcl=true
	lisps_enabled="${lisps_enabled} openmcl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		OPENMCL_NAME="${openmcl_default_name}"
	else
		OPENMCL_NAME="$withval"
	fi],
	[OPENMCL_NAME=${openmcl_default_name}])
AC_ARG_ENABLE(openmcl-exec,
	[  --enable-openmcl-exec        Create a maxima executable image using OPENMCL.
                                No check is made if the version of 
                                OPENMCL supports executable images],
	[case "${enableval}" in
	    yes) openmcl_exec=true
                 openmcl=true 
		 lisps_enabled="${lisps_enabled} openmcl"
		 explicit_lisp=true;;
	    no) openmcl_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-openmcl-exec) ;;
         esac
	 OPENMCL_EXEC=${openmcl_exec}],
	[openmcl_exec=false
	 OPENMCL_EXEC=false])

dnl Define ccl as an alias (essentially) for openmcl
AC_ARG_ENABLE(ccl,
	[  --enable-ccl                 Use CCL (Clozure Common Lisp)],
     	[case "${enableval}" in
       		yes) openmcl=true 
		     lisps_enabled="${lisps_enabled} openmcl"
		     explicit_lisp=true;;
       		no)  openmcl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-ccl) ;;
     	esac],
	[openmcl=false])
AC_ARG_WITH(ccl,
	[  --with-ccl=<prog>            Use OpenMCL executable <prog> (default ccl)],
	[openmcl=true
	lisps_enabled="${lisps_enabled} openmcl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		OPENMCL_NAME="${openmcl_default_name}"
	else
		OPENMCL_NAME="$withval"
	fi],
	[OPENMCL_NAME=${openmcl_default_name}])
AC_ARG_ENABLE(ccl-exec,
	[  --enable-ccl-exec            Create a maxima executable image using CCL.
                                No check is made if the version of 
                                CCL supports executable images],
	[case "${enableval}" in
	    yes) openmcl_exec=true
                 openmcl=true 
		 lisps_enabled="${lisps_enabled} openmcl"
                 explicit_lisp=true;;
	    no) openmcl_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-ccl-exec) ;;
         esac
	 OPENMCL_EXEC=${openmcl_exec}],
	[openmcl_exec=false
	 OPENMCL_EXEC=false])

if test x"${openmcl}" = xtrue ; then
	if test `echo "$OPENMCL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$OPENMCL_NAME" ; then
			openmcl_found=true
		else
			openmcl_found=false
		fi
	else
		AC_CHECK_PROG(openmcl_found,$OPENMCL_NAME,true,false)
	fi
	if test x"${openmcl_found}" = xfalse ; then
		AC_MSG_WARN(openmcl executable ${OPENMCL_NAME} not found in PATH)
	fi
fi

dnl n.b. ccl64_default_name is hardcoded in "with" message
dnl We assume we're running on an x86 processor.  The user has to
dnl select a different executable name to match his system, if neceessary.

ccl64_default_name=dx86cl64
AC_ARG_ENABLE(ccl64,
	[  --enable-ccl64               Use CCL (Clozure Common Lisp), 64-bit],
     	[case "${enableval}" in
       		yes) ccl64=true 
		     lisps_enabled="${lisps_enabled} ccl64"
		     explicit_lisp=true;;
       		no)  ccl64=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-ccl64) ;;
     	esac],
	[ccl64=false])
AC_ARG_WITH(ccl64,
	[  --with-ccl64=<prog>          Use ccl64 executable <prog> (default dx86cl64)],
	[ccl64=true
	lisps_enabled="${lisps_enabled} ccl64"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		CCL64_NAME="${ccl64_default_name}"
	else
		CCL64_NAME="$withval"
	fi],
	[CCL64_NAME=${ccl64_default_name}])
AC_ARG_ENABLE(ccl64-exec,
	[  --enable-ccl64-exec          Create a maxima executable image using CCL.
                                No check is made if the version of 
                                CCL supports executable images],
	[case "${enableval}" in
	    yes) ccl64_exec=true
                 ccl64=true 
		 lisps_enabled="${lisps_enabled} ccl64"
                 explicit_lisp=true;;
	    no) ccl64_exec=false ;;
	    *) AC_MSG_ERROR(bad value ${enableval} for --enable-ccl64-exec) ;;
         esac
	 CCL64_EXEC=${ccl64_exec}],
	[ccl64_exec=false
	 CCL64_EXEC=false])

dnl n.b. ecl_default_name is hardcoded in "with" message
ecl_default_name=ecl
AC_ARG_ENABLE(ecl,
	[  --enable-ecl                 Use ECL],
     	[case "${enableval}" in
       		yes) ecl=true 
		     lisps_enabled="${lisps_enabled} ecl"
		     explicit_lisp=true;;
       		no)  ecl=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-ecl) ;;
     	esac],
	[ecl=false])
AC_ARG_WITH(ecl,
	[  --with-ecl=<prog>            Use ECL executable <prog> (default ecl)],
	[ecl=true
	lisps_enabled="${lisps_enabled} ecl"
	explicit_lisp=true
	if test "$withval" = "yes"; then
		ECL_NAME="${ecl_default_name}"
	else
		ECL_NAME="$withval"
	fi],
	[ECL_NAME=${ecl_default_name}])
if test x"${ecl}" = xtrue ; then
	if test `echo "$ECL_NAME" | sed 's/\(.\).*/\1/'` = "/" ; then
		if test -x "$ECL_NAME" ; then
			ecl_found=true
		else
			ecl_found=false
		fi
	else
		AC_CHECK_PROG(ecl_found,$ECL_NAME,true,false)
	fi
	if test x"${ecl_found}" = xfalse ; then
		AC_MSG_WARN(ecl executable ${ECL_NAME} not found in PATH)
	fi
fi

dnl xgettext
AC_ARG_ENABLE(gettext,
	[  --enable-gettext             Locale support],
	[case "${enableval}" in
	       yes) enable_gettext=true  ;;
 	       no)  enable_gettext=false ;;
	       *) AC_MSG_ERROR(bad value ${enableval} for --enable-gettext) ;;
	 esac],
	 [enable_gettext=false])


AM_CONDITIONAL(ENABLE_GETTEXT, test x$enable_gettext = xtrue)

dnl languages
AC_ARG_ENABLE(lang-de,
	[  --enable-lang-de             German language support],
     	[case "${enableval}" in
       		yes) lang_de=true  ;;
       		no)  lang_de=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-de) ;;
     	esac],
	[lang_de=false])

AM_CONDITIONAL(LANG_DE, test x$lang_de = xtrue)

AC_ARG_ENABLE(lang-de-utf8,
	[  --enable-lang-de-utf8        German language support (UTF-8)],
     	[case "${enableval}" in
       		yes) lang_de_utf8=true  ;;
       		no)  lang_de_utf8=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-de-utf8) ;;
     	esac],
	[lang_de_utf8=false])

AM_CONDITIONAL(LANG_DE_UTF8, test x$lang_de_utf8 = xtrue)

AC_ARG_ENABLE(lang-es,
	[  --enable-lang-es             Spanish language support],
     	[case "${enableval}" in
       		yes) lang_es=true  ;;
       		no)  lang_es=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-es) ;;
     	esac],
	[lang_es=false])

AC_SUBST(lang_es)
AM_CONDITIONAL(LANG_ES, test x$lang_es = xtrue)

AC_ARG_ENABLE(lang-es-utf8,
	[  --enable-lang-es-utf8        Spanish language support (UTF-8)],
     	[case "${enableval}" in
       		yes) lang_es_utf8=true  ;;
       		no)  lang_es_utf8=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-es-utf8) ;;
     	esac],
	[lang_es_utf8=false])

AM_CONDITIONAL(LANG_ES_UTF8, test x$lang_es_utf8 = xtrue)

AC_ARG_ENABLE(lang-pt,
	[  --enable-lang-pt             Portuguese language support],
     	[case "${enableval}" in
       		yes) lang_pt=true  ;;
       		no)  lang_pt=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-pt) ;;
     	esac],
	[lang_pt=false])

AC_SUBST(lang_pt)
AM_CONDITIONAL(LANG_PT, test x$lang_pt = xtrue)

AC_ARG_ENABLE(lang-pt-utf8,
	[  --enable-lang-pt-utf8        Portuguese language support (UTF-8)],
     	[case "${enableval}" in
       		yes) lang_pt_utf8=true  ;;
       		no)  lang_pt_utf8=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-pt-utf8) ;;
     	esac],
	[lang_pt_utf8=false])

AM_CONDITIONAL(LANG_PT_UTF8, test x$lang_pt_utf8 = xtrue)

AC_ARG_ENABLE(lang-pt_BR,
	[  --enable-lang-pt_BR          Brazilian Portuguese language support],
     	[case "${enableval}" in
       		yes) lang_pt_br=true  ;;
       		no)  lang_pt_br=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-pt_BR) ;;
     	esac],
	[lang_pt_br=false])

AC_SUBST(lang_pt_br)
AM_CONDITIONAL(LANG_PT_BR, test x$lang_pt_br = xtrue)

AC_ARG_ENABLE(lang-pt_BR-utf8,
	[  --enable-lang-pt_BR-utf8     Brazilian Portuguese language support (UTF-8)],
     	[case "${enableval}" in
       		yes) lang_pt_br_utf8=true  ;;
       		no)  lang_pt_br_utf8=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-lang-pt_BR-utf8) ;;
     	esac],
	[lang_pt_br_utf8=false])

AM_CONDITIONAL(LANG_PT_BR_UTF8, test x$lang_pt_br_utf8 = xtrue)

AC_ARG_ENABLE(recode,
	[  --enable-recode              Use recode for charset conversion],
     	[case "${enableval}" in
       		yes) use_recode=true  ;;
       		no)  use_recode=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-recode) ;;
     	esac],
	[use_recode=false])

dnl iconv and/or recode
AC_CHECK_PROG(iconv_found,iconv,true,false)
AC_CHECK_PROG(recode_found,recode,true,false)

if test x"${use_recode}" = xtrue ; then
    if test x"${recode_found}" = xfalse ; then
	AC_MSG_ERROR(No recode found)
    fi
elif test x"${iconv_found}" = xfalse ; then
    use_recode=true 
else
    use_recode=false
fi

AM_CONDITIONAL(USE_RECODE, test x$use_recode = xtrue)

dnl Optionally build the windows CHM help files
dnl default to false as requires win32 and Microsoft HTML Help Workshop
AC_ARG_ENABLE(chm,
  [  --enable-chm                 Build Windows CHM help files],
  [case "${enableval}" in
       		yes) chm=true ;;
       		no)  chm=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-chm) ;;
   esac],
   [chm=false])
AM_CONDITIONAL(CHM, test x$chm = xtrue)
dnl hhc is the HTML Help Compiler for CHM documentation
hhc_default_name=hhc$EXEEXT
AC_ARG_WITH(hhc,
	[  --with-hhc=<prog>            Use HTML Help Compiler executable <prog> (default hhc)],
	[hhc=true
	if test "$withval" = "yes"; then
		HHC="${hhc_default_name}"
	else
		HHC="$withval"
	fi],
	[HHC="${hhc_default_name}"])

# Check that hhc exists, using AC_CHECK_PROG
if test x$chm = xtrue; then
  if test -x "${HHC}"; then
    # HHC was a path to the executable, and it existed, which is
    # great! We still say something to the caller, since this is
    # probably less confusing.
    AC_MSG_CHECKING([for hhc])
    AC_MSG_RESULT([yes])
  else
    AC_CHECK_PROG(hhc_found, ${HHC}, yes)
    if test x"${hhc_found}" != x"yes"; then
      AC_MSG_ERROR([HTML Help Compiler executable ${HHC} not found])
    fi
  fi
fi

dnl Make the build quiet
AC_ARG_ENABLE(quiet_build,
  [  --enable-quiet-build         Make the build quieter],
  [case "${enableval}" in
     yes) quiet_build=true ;;
     no)  quiet_build=false ;;
     *) AC_MSG_ERROR(bad value ${enableval} for --enable-quiet-build) ;;
   esac],
  [quiet_build=false])
AM_CONDITIONAL(QUIET_BUILD, test x${quiet_build} = xtrue)

dnl Optionally build xmaxima.exe under windows
dnl default to false as additional software
AC_ARG_ENABLE(xmaxima_exe,
  [  --enable-xmaxima-exe         Build Windows xmaxima.exe for installer],
  [case "${enableval}" in
       		yes) xmaxima_exe=true ;;
       		no)  xmaxima_exe=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-xmaxima-exe) ;;
   esac],
   [xmaxima_exe=false])
AM_CONDITIONAL(XMAXIMA_EXE, test x$xmaxima_exe = xtrue)

dnl Optionally build xmaxima.exe under windows
dnl default to false as additional software
AC_ARG_ENABLE(winkill_exe,
  [  --enable-winkill             Build Windows winkill.exe and winkill_lib.dll for installer],
  [case "${enableval}" in
       		yes) winkill_exe=true ;;
       		no)  winkill_exe=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-winkill-exe) ;;
   esac],
   [winkill_exe=false])
AM_CONDITIONAL(WINKILL_EXE, test x$winkill_exe = xtrue)

dnl Should we build a win64 installer?
AC_ARG_ENABLE(win64-installer,
  [  --enable-win64-installer     Build a 64bit installer on Windows],
  [case "${enableval}" in
                yes) win64_installer=true ;;
                no) win64_installer=false ;;
                *) AC_MSG_ERROR(bad valu ${enableval} for --enable-win64-installer) ;;
  esac],
  [win64_installer=false])
AM_CONDITIONAL(WIN64_INSTALLER, test x$win64_installer = xtrue)
AC_SUBST(win64_installer)

dnl Enable use of MathJax in the html documentation.  This just loads
dnl MathJax in each html file.  The texi files need to put the
dnl appropriate @html/@end html text to generate the equations for MathJax.
AC_ARG_ENABLE(mathjax,
  [  --enable-mathjax             Enable MathJax support for html manual],
  [case "${enableval}" in
      yes) mathjax=true
           mathjax_enable='@set mathjax true'
           mathjax_script='<script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>'
;;
      no)  mathjax=false
           mathjax_enable='@clear mathjax'
           mathjax_script="";;
      *) AC_MSG_ERROR(bad value ${enableval} for --enable-mathjax) ;;
   esac],
  [mathjax=false])
AM_CONDITIONAL(MATHJAX, test x$mathjax = xtrue)
AC_SUBST(mathjax_script)
AC_SUBST(mathjax_enable)

dnl Tools for building xmaxima.exe on windows
dnl Hard code the filenames for now.  Can over-ride on make command line
GCCVER=undefined
if test x${xmaxima_exe} = xtrue ; then
  GCCVER=`gcc -dumpversion`
  dnl http://tclkits.rkeene.org/fossil/wiki?name=Downloads
  dnl and http://www.tcl.tk/starkits/
  TCLKITSH=/c/programs/star/tclkitsh-8.6.3-win32-ix86.exe
  TCLKIT_RUNTIME=/c/programs/star/tclkit-8.6.3-win32-ix86.exe
  SDXKIT=/c/programs/star/sdx.kit
  IMGKIT=/c/programs/star/img.kit
fi

AC_SUBST(GCCVER)
AC_SUBST(TCLKITSH)
AC_SUBST(TCLKIT_RUNTIME)
AC_SUBST(SDXKIT)
AC_SUBST(IMGKIT)
AM_CONDITIONAL(GCC331, test x${GCCVER} = x3.3.1)

if test x"${explicit_lisp}" = xfalse ; then
	dnl See if any of the lisps can be found
  	AC_CHECK_PROG(clisp_found,${clisp_default_name},true,false)
	AC_CHECK_PROG(gcl_found,${gcl_default_name},true,false)
	AC_CHECK_PROG(cmucl_found,$CMUCL_NAME,true,false)
	AC_CHECK_PROG(scl_found,$SCL_NAME,true,false)
	AC_CHECK_PROG(sbcl_found,$SBCL_NAME,true,false)
	AC_CHECK_PROG(acl_found,$ACL_NAME,true,false)
	AC_CHECK_PROG(openmcl_found,$OPENMCL_NAME,true,false)
	AC_CHECK_PROG(ccl64_found,$CCL64_NAME,true,false)
	AC_CHECK_PROG(ecl_found,$ECL_NAME,true,false)

  	if test x"${clisp_found}" = xtrue ; then
  		clisp=true
		lisps_enabled="${lisps_enabled} clisp"
  	elif test x"${gcl_found}" = xtrue ; then
		gcl=true
		lisps_enabled="${lisps_enabled} gcl"
	elif test x"${cmucl_found}" = xtrue ; then
		cmucl=true
		lisps_enabled="${lisps_enabled} cmucl"
	elif test x"${scl_found}" = xtrue ; then
		scl=true
		lisps_enabled="${lisps_enabled} scl"
	elif test x"${sbcl_found}" = xtrue ; then
	   	sbcl=true
		lisps_enabled="${lisps_enabled} sbcl"
	elif test x"${acl_found}" = xtrue ; then
		acl=true
		lisps_enabled="${lisps_enabled} acl"
	elif test x"${ecl_found}" = xtrue ; then
		ecl=true
		lisps_enabled="${lisps_enabled} ecl"
	else
		AC_MSG_ERROR(No lisp implementation specified and none of the default executables [${clisp_default_name}(clisp),${gcl_default_name}(GCL),${cmucl_default_name}(CMUCL),${scl_default_name}(SCL),${sbcl_default_name}(SBCL),${acl_default_name}(ACL),${openmcl_default_name}(OpenMCL),${ecl_default_name}(ECL)] were found in PATH)
	fi
fi

AM_CONDITIONAL(ABCL, test x$abcl = xtrue)
AM_CONDITIONAL(CLISP, test x$clisp = xtrue)
AM_CONDITIONAL(CLISP_EXEC, test x$clisp_exec = xtrue)
AM_CONDITIONAL(GCL, test x$gcl = xtrue)
AM_CONDITIONAL(CMUCL, test x$cmucl = xtrue)
AM_CONDITIONAL(CMUCL_EXEC, test x${cmucl_exec} = xtrue)
AM_CONDITIONAL(SCL, test x$scl = xtrue)
AM_CONDITIONAL(SBCL, test x$sbcl = xtrue)
AM_CONDITIONAL(SBCL_EXEC, test x$sbcl_exec = xtrue)
AM_CONDITIONAL(ACL, test x$acl = xtrue)
AM_CONDITIONAL(OPENMCL, test x$openmcl = xtrue)
AM_CONDITIONAL(OPENMCL_EXEC, test x${openmcl_exec} = xtrue)
AM_CONDITIONAL(ECL, test x$ecl = xtrue)
AM_CONDITIONAL(CCL64, test x$ccl64 = xtrue)
AM_CONDITIONAL(CCL64_EXEC, test x${ccl64_exec} = xtrue)

if test x"${clisp}" = xtrue; then
    if test x"${CLISP_RUNTIME_PATH}" = x"" ; then
        if test x"${mingw}" = xtrue ; then
            CLISP_RUNTIME_PATH=`${CLISP_NAME} -norc -q -x "(format nil \"~a${CLISP_RUNTIME}\" (namestring *lib-directory*))"|sed 's/\"\\(.*\\)\"/\\1/'`
        else
            CLISP_RUNTIME_PATH=`${CLISP_NAME} -norc -q -x "(format nil \"~abase/${CLISP_RUNTIME}\" (namestring *lib-directory*))"|sed 's/\"\\(.*\\)\"/\\1/'`
        fi
    fi
    if test -x ${CLISP_RUNTIME_PATH} ; then
        echo "clisp runtime is \"${CLISP_RUNTIME_PATH}\""
    else
        AC_MSG_ERROR(clisp runtime \"${CLISP_RUNTIME_PATH}\" is not an executable)
    fi
    CLISP_RUNTIME=`basename ${CLISP_RUNTIME_PATH}`
fi

if test x"${cmucl}" = xtrue; then
    if test x"${CMUCL_RUNTIME_PATH}" = x"" ; then
        CMUCL_RUNTIME_PATH=`${CMUCL_NAME} -noinit -nositeinit -quiet -batch -eval '#-cmu18 (progn (setf (search-list "cmuclbin:") (append (lisp::parse-unix-search-path lisp::*cmucl-lib*) (mapcar (function (lambda (p) (concatenate (quote string) p "../bin/"))) (lisp::parse-unix-search-path lisp::*cmucl-lib*))))(enumerate-search-list (s "cmuclbin:lisp") (when (probe-file s) (format t "~A~%" s) (quit)))) #+cmu18 (format t "~a/../bin/lisp~%" common-lisp::*cmucl-lib*)(quit)'`
    fi
    if test -x "${CMUCL_RUNTIME_PATH}" ; then
        echo "cmucl runtime is \"${CMUCL_RUNTIME_PATH}\""
    else
dnl last chance: find CMUCL_NAME in path. Use it if it doesn't appear to
dnl be a shell script.
	cmucl_path=`type -p "${CMUCL_NAME}"`
	if test x"`grep -c '#!.*bin.*sh.*' ${cmucl_path}`" = x"0" ; then
	   	CMUCL_RUNTIME_PATH="${cmucl_path}"
	else
		AC_MSG_ERROR([Unable to determine CMUCL runtime path.
The best guess for CMUCL runtime, \"${CMUCL_RUNTIME_PATH}\", is not
an executable. Use the argument
   --with-cmucl-runtime=<path>
to set the actual CMUCL executable. If the CMUCL lisp command is a shell
script the CMUCL executable is the program exec'd by that shell script.])
	fi
    fi
    CMUCL_RUNTIME=`basename ${CMUCL_RUNTIME_PATH}`

dnl cmucl final check
   result=`"${CMUCL_RUNTIME_PATH}" -quiet -eval '(format t "MAXIMA_CMUCL_TEST_SUCCESS%")(quit)'`
   retval=$?
   if test ! x"${retval}" = x"0" ; then
      AC_MSG_ERROR(unable to run cmucl runtime = "${CMUCL_RUNTIME_PATH}". 
Please specify the full path of the cmucl runtime using the 
    --with-cmucl-runtime=<path>
flag.)
   fi
   count=`echo "${result}" | grep -c "MAXIMA_CMUCL_TEST_SUCCESS"`
   if test ! "${count}" = "1" ; then
      AC_MSG_ERROR(an error occured while checking cmucl runtime)
   fi
fi

if test x"${scl}" = xtrue; then
    if test x"${SCL_RUNTIME_PATH}" = x"" ; then
        SCL_RUNTIME_PATH=`${SCL_NAME} -noinit -nositeinit -quiet -batch -eval '(progn (enumerate-pathname-translations (pathname "file://library/../bin/lisp") (when (probe-file pathname) (format t "~A~%" (unix-namestring pathname)))) (quit))'`
    fi
    if test -x "${SCL_RUNTIME_PATH}" ; then
        echo "scl runtime is \"${SCL_RUNTIME_PATH}\""
    else
dnl last chance: find SCL_NAME in path. Use it if it doesn't appear to
dnl be a shell script.
	scl_path=`type -p "${SCL_NAME}"`
	if test x"`grep -c '#!.*bin.*sh.*' ${scl_path}`" = x"0" ; then
	   	SCL_RUNTIME_PATH="${scl_path}"
	else
		AC_MSG_ERROR([Unable to determine SCL runtime path.
The best guess for SCL runtime, \"${SCL_RUNTIME_PATH}\", is not
an executable. Use the argument
   --with-scl-runtime=<path>
to set the actual SCL executable. If the SCL lisp command is a shell
script the SCL executable is the program exec'd by that shell script.])
	fi
    fi
    SCL_RUNTIME=`basename ${SCL_RUNTIME_PATH}`
fi

if test x"${gcl}" = xtrue; then
   result=`"${GCL_NAME}" -batch -eval '#+ansi-cl (format t "MAXIMA_GCL_ANSI_TEST_SUCCESS~%") #-ansi-cl (format t "MAXIMA_GCL_ANSI_TEST_FAILURE~%")' -eval '(si::bye)'`
   retval=$?
   if test ! x"${retval}" = x"0" ; then
      AC_MSG_ERROR(unable to run gcl executable \"${GCL_NAME}\".)
   fi
   count=`echo "${result}" | grep -c "MAXIMA_GCL_ANSI_TEST_SUCCESS"`
   if test ! "${count}" = "1" ; then
      AC_MSG_ERROR([The gcl executable \"${GCL_NAME}\" was not compiled with
the --enable-ansi flag, which is required for Maxima.
The gcl ANSI-CL check returned
\"${result}\".])
   fi
fi

AC_ARG_ENABLE(gcl-alt-link,
	[  --enable-gcl-alt-link        Use GCL's alternate linking mechanism],
     	[case "${enableval}" in
       		yes) gcl_alt_link=true ;;
       		no)  gcl_alt_link=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-gcl-alt-link) ;;
     	esac],
	[gcl_alt_link=false])

AM_CONDITIONAL(GCL_ALT_LINK, test x$gcl_alt_link = xtrue)

AC_ARG_WITH(default-lisp,
	[  --with-default-lisp=<lisp>   Set default lisp implementation to <lisp>],
     	[case "${withval}" in
       		clisp)
			if test x"${clisp}" = xtrue ; then
				DEFAULTLISP=clisp
			else
				AC_MSG_ERROR(clisp not enabled)
			fi
			;;
		cmucl)
			if test x"${cmucl}" = xtrue ; then
				DEFAULTLISP=cmucl
			else
				AC_MSG_ERROR(cmucl not enabled)
			fi
			;;
		scl)
			if test x"${scl}" = xtrue ; then
				DEFAULTLISP=scl
			else
				AC_MSG_ERROR(scl not enabled)
			fi
			;;
		sbcl)
			if test x"${sbcl}" = xtrue ; then
				DEFAULTLISP=sbcl
			else
				AC_MSG_ERROR(sbcl not enabled)
			fi
			;;
		gcl)
			if test x"${gcl}" = xtrue ; then
				DEFAULTLISP=gcl
			else
				AC_MSG_ERROR(gcl not enabled)
			fi
			;;	
		acl)
			if test x"${acl}" = xtrue ; then
				DEFAULTLISP=acl
			else
				AC_MSG_ERROR(acl not enabled)
			fi
			;;	
		openmcl)
			if test x"${openmcl}" = xtrue ; then
				DEFAULTLISP=openmcl
			else
				AC_MSG_ERROR(openmcl not enabled)
			fi
			;;		
		ecl)
			if test x"${ecl}" = xtrue ; then
				DEFAULTLISP=ecl
			else
				AC_MSG_ERROR(ecl not enabled)
			fi
			;;
		ccl64)		
			if test x"${ccl64}" = xtrue ; then
				DEFAULTLISP=ccl64
			else
				AC_MSG_ERROR(ccl64 not enabled)
			fi
			;;		
		abcl)		
			if test x"${abcl}" = xtrue ; then
				DEFAULTLISP=abcl
			else
				AC_MSG_ERROR(abcl not enabled)
			fi
			;;		
		*) 
			AC_MSG_ERROR(Unknown argument ${DEFAULTLISP} to --with-default-lisp) 
			;;
     	esac],
	[if test x"${sbcl}" = xtrue ; then
		DEFAULTLISP=sbcl
	elif test x"${cmucl}" = xtrue ; then
		DEFAULTLISP=cmucl
	elif test x"${scl}" = xtrue ; then
		DEFAULTLISP=scl
	elif test x"${clisp}" = xtrue ; then
	   	DEFAULTLISP=clisp
	elif test x"${gcl}" = xtrue ; then
	   	DEFAULTLISP=gcl
	elif test x"${acl}" = xtrue ; then
		DEFAULTLISP=acl
	elif test x"${openmcl}" = xtrue ; then
		DEFAULTLISP=openmcl
	elif test x"${ecl}" = xtrue ; then
		DEFAULTLISP=ecl
	elif test x"${ccl64}" = xtrue ; then
		DEFAULTLISP=ccl64
	elif test x"${abcl}" = xtrue ; then
		DEFAULTLISP=abcl
	else
		AC_MSG_ERROR(Internal error. No lisp enabled. Please contact maintainer.)
	fi])

AC_SUBST(CLISP_NAME)
AC_SUBST(CLISP_RUNTIME)
AC_SUBST(CLISP_RUNTIME_PATH)
AC_SUBST(CMUCL_NAME)
AC_SUBST(CMUCL_RUNTIME)
AC_SUBST(CMUCL_RUNTIME_PATH)
AC_SUBST(CMUCL_EXEC)
AC_SUBST(SCL_NAME)
AC_SUBST(SCL_RUNTIME)
AC_SUBST(SCL_RUNTIME_PATH)
AC_SUBST(SBCL_NAME)
AC_SUBST(GCL_NAME)
AC_SUBST(ACL_NAME)
AC_SUBST(OPENMCL_NAME)
AC_SUBST(ECL_NAME)
AC_SUBST(CCL64_NAME)
AC_SUBST(DEFAULTLISP)
AC_SUBST(HHC)
AC_SUBST(lisps_enabled)
AC_SUBST(ABCL_JAR)
AC_SUBST(JRE)

AC_ARG_WITH(posix-shell,
	[  --with-posix-shell=<path>    Use <shell> for maxima script (default /bin/sh)],
	[posix_shell_list="${withval}"],
	[posix_shell_list="/bin/sh /bin/bash /usr/bin/bash /usr/local/bin/bash"])

AC_ARG_WITH(wish,
	[  --with-wish=<prog>           Use <prog> for Tk wish shell (default wish)],
	[WISH="${withval}"],
	[WISH="wish"])
AC_SUBST(WISH)


AC_MSG_CHECKING(POSIX shell to see that it contains getopts)
cat <<EOF > conftest-posix-shell.sh
getopts "ab:" opt
result="\$?"
echo "result is \$result"
exit "\$result"
EOF
POSIX_SHELL=""
for shell in $posix_shell_list
do
	if test -z "$POSIX_SHELL" ; then
		echo "trying $shell"
		$shell conftest-posix-shell.sh -a > /dev/null 2>&1
		if test "$?" = "0" ; then
			POSIX_SHELL="$shell"
		fi
	fi
done
rm -f conftest-posix-shell.sh
if test -n "$POSIX_SHELL" ; then
	AC_MSG_RESULT(POSIX shell is $POSIX_SHELL)
else
	AC_MSG_WARN(Could not find a shell that supports getopts. 
The maxima wrapper script will be unusable. The shell may be specified
with --with-posix-shell=</path/to/shell>)
fi
AC_SUBST(POSIX_SHELL)

if test x"${prefix}" = xNONE ; then
	tmp_prefix="/usr/local"
else
	tmp_prefix="${prefix}"
fi
if test x"${exec_prefix}" = xNONE ; then
	tmp_exec_prefix="${tmp_prefix}"
else
	tmp_exec_prefix="${exec_prefix}"
fi
expanded_top_srcdir="$(cd "$top_srcdir" 1>/dev/null 2>/dev/null; pwd)"
expanded_exec_prefix="${tmp_exec_prefix}"
expanded_libdir="$(eval "exec_prefix=\"${tmp_exec_prefix}\";echo ${libdir}")"
expanded_libexecdir="$(eval "exec_prefix=\"${tmp_exec_prefix}\";echo ${libexecdir}")"
expanded_datadir="$(eval "prefix=\"${tmp_prefix}\";datarootdir=\"${datarootdir}\";echo ${datadir}")"
expanded_infodir="$(eval "prefix=\"${tmp_prefix}\";datarootdir=\"${datarootdir}\";echo ${infodir}")"


dnl Find all the directories in share, but remove the share directory
dnl itself and all CVS directories (if any) and fortran directories
dnl and *.t2p directories (created when generating pdf documentation).
dnl Remove the leading "share" part of the path, and add double-quotes
dnl around it.

#default_sharedirs=`find share -type d | sort | egrep -v 'share$|CVS|/fortran' | sed 's;share/\(.*\);        "\1" \\\\;' | sed '$s;\\\\;;'`
default_sharedirs=`cd $srcdir;find share -type d | sort | egrep -v 'share$|CVS|/fortran' | sed 's;share/\(.*\);"\1";' | tr '\n' ' '`

AC_SUBST(top_srcdir)
AC_SUBST(abs_top_builddir)
AC_SUBST(abs_top_srcdir)
AC_SUBST(top_builddir)
AC_SUBST(expanded_top_srcdir)
AC_SUBST(expanded_exec_prefix)
AC_SUBST(expanded_libdir)
AC_SUBST(expanded_libexecdir)
AC_SUBST(expanded_datadir)
AC_SUBST(expanded_infodir)
default_layout_autotools="true"
AC_SUBST(default_layout_autotools)
AC_SUBST(LDFLAGS)
AC_SUBST(default_sharedirs)

AC_ARG_WITH(emacs-prefix,
	[  --emacs-prefix=<path>        Where to install the emacs modes to],
	[EMACSDIR="$withval"],
	[EMACSDIR="${datarootdir}/emacs/site-lisp"])
AC_SUBST(EMACSDIR)

dnl Tell defsystem that this isn't a lisp-only build
lisp_only_build="nil"
AC_SUBST(lisp_only_build)
dnl Extra files we want to clean from the src dir
if test x"$srcdir" = x"." ; then
   DISTCLEAN_EXTRA_SRC_FILES=""
else
   DISTCLEAN_EXTRA_SRC_FILES=share_subdirs.lisp
fi
AC_SUBST(DISTCLEAN_EXTRA_SRC_FILES)


dnl Look for grep that can handle long lines and -e.
AC_PROG_EGREP
AC_SUBST(EGREP)
AC_PATH_PROG([CAT],[cat])
AC_PROG_SED
AC_PROG_AWK
AM_PATH_PYTHON
AC_SUBST(SED)

# Configure these files and make them executable
AC_CONFIG_FILES([maxima-local], chmod +x maxima-local)
AC_CONFIG_FILES([src/startmaxima_abcl.sh], chmod +x src/startmaxima_abcl.sh)
AC_CONFIG_FILES([xmaxima-local], chmod +x xmaxima-local)
AC_CONFIG_FILES([tests/test.sh], chmod +x tests/test.sh)
AC_CONFIG_FILES([doc/info/build_html.sh], chmod +x doc/info/build_html.sh)
AC_CONFIG_FILES([doc/info/extract_categories.sh], chmod +x doc/info/extract_categories.sh)

# Convert maxima.bat to DOS line ending
# sed-3.02 in old mingw distribution doesn't support -i or \r
# dos2unix may not be present, but perl is required elsewhere
# perl -i interferes with binmode so need to use a temporary file
AC_CONFIG_FILES([src/maxima.bat], (perl -ne 'BEGIN{binmode(STDOUT,":crlf")};' -e 'print' < src/maxima.bat > src/maxima.bat.tmp; mv src/maxima.bat.tmp src/maxima.bat))
AC_CONFIG_FILES([src/maxima], chmod +x src/maxima)

AC_OUTPUT(Makefile maxima.spec maxima.iss \
admin/Makefile src/Makefile src/rmaxima src/autoconf-variables.lisp \
src/share-subdirs_autogenerated.lisp \
lisp-utils/Makefile tests/Makefile doc/Makefile \
crosscompile-windows/Makefile \
doc/emaxima/Makefile doc/info/Makefile doc/info/include-maxima.texi \
desktopintegration/Makefile \
doc/info/texi2html.init \
doc/info/figures/Makefile \
doc/info/de/Makefile \
doc/info/de.utf8/Makefile doc/info/es/Makefile doc/info/es.utf8/Makefile \
doc/info/pt/Makefile doc/info/pt.utf8/Makefile \
doc/info/pt/include-maxima.texi \
doc/info/pt_BR/Makefile doc/info/pt_BR.utf8/Makefile \
doc/intromax/Makefile doc/man/Makefile doc/man/maxima.1 doc/man/ru/maxima.1 \
doc/share/Makefile interfaces/Makefile interfaces/emacs/Makefile \
interfaces/emacs/emaxima/Makefile interfaces/emacs/imaxima/Makefile \
interfaces/emacs/misc/Makefile interfaces/xmaxima/Makefile \
interfaces/xmaxima/autoconf-variables.tcl interfaces/xmaxima/Tkmaxima/Header.tcl \
interfaces/xmaxima/doc/Makefile interfaces/xmaxima/doc/figures/Makefile \
interfaces/xmaxima/msgs/Makefile interfaces/xmaxima/win32/Makefile \
plotting/mgnuplot share/Makefile demo/Makefile plotting/Makefile locale/Makefile \
share/contrib/Makefile share/contrib/integration/Makefile \
share/contrib/maxima-odesolve/Makefile \
share/draw/Makefile share/logic/Makefile  doc/info/es/include-maxima.texi \
src/lisp)

# The default of 4096 is sometimes too little for the test suite.
if test x"${sbcl}" = xtrue ; then
   AC_MSG_CHECKING(Testing if sbcl complains if we try to enlarge the thread-local storage)
   echo "(quit)" | ${SBCL_NAME} --tls-limit 8192 > /dev/null 2>&1
   if test "$?" = "0" ; then
	SBCL_EXTRA_ARGS="--tls-limit 8192"
	AC_MSG_RESULT(Yes)
   else
	SBCL_EXTRA_ARGS=""
	AC_MSG_RESULT(No)
   fi
fi
AC_SUBST(SBCL_EXTRA_ARGS)

if test x"${clisp}" = xtrue ; then
   	echo
	AC_MSG_WARN("CLISP 2.49 is known to sporadically produce garbled data if the front-end is fast enough to acknowledge a data packet while a next data packet is still being prepared.")
fi
echo
echo "Summary:"
if test x"${clisp}" = xtrue ; then
	echo "Compiling an maxima image that uses \"${CLISP_NAME}\"",
	echo "clisp runtime is \"${CLISP_RUNTIME_PATH}\""
	if test x"${clisp_exec}" = xtrue ; then
		echo "clisp executable image enabled for maxima."
	else
		echo Use "--enable-clisp-exec to build a standalone executable instead."
	fi
fi
if test x"${cmucl}" = xtrue ; then
	echo "Compiling an maxima image that uses \"${CMUCL_NAME}\""
	echo "cmucl runtime is \"${CMUCL_RUNTIME_PATH}\""
	if test x"${cmucl_exec}" = xtrue; then
		echo "CMUCL executable image enabled for maxima."
	else
		echo Use "--enable-cmucl-exec to build a standalone executable instead."
	fi
fi
if test x"${scl}" = xtrue ; then
	echo "SCL enabled, that uses \"${SCL_NAME}\""
	echo "SCL runtime is \"${SCL_RUNTIME_PATH}\""
fi
if test x"${sbcl}" = xtrue ; then
   	echo "Compiling an maxima image that uses \"${SBCL_NAME}\""
	if test x"${sbcl_exec}" = xtrue ; then
	   	echo "sbcl executable image enabled for maxima."
	else
		echo Use "--enable-sbcl-exec to build a standalone executable instead."
	fi
fi
if test x"${gcl}" = xtrue ; then
	echo "Compiling an maxima image that uses \"${GCL_NAME}\""
	if test x"${gcl_alt_link}" = xtrue ; then
		echo "    GCL alternative linking method enabled."
	fi
fi
if test x"${acl}" = xtrue ; then
	echo "ACL enabled. Executable name: \"${ACL_NAME}\""
fi
if test x"${openmcl}" = xtrue ; then
	echo "Compiling an maxima image that uses \"${OPENMCL_NAME}\""
	if test x"${openmcl_exec}" = xtrue ; then
		echo "OpenMCL executable image enabled for maxima."
	else
		echo Use "--enable-openmcl-exec to build a standalone executable instead."
	fi
fi
if test x"${ccl64}" = xtrue ; then
	echo "Compiling an maxima image that uses \"${CCL64_NAME}\""
	if test x"${ccl64_exec}" = xtrue ; then
	   	echo "CCL64 executable image enabled for maxima."
	else
	   	echo Use "--enable-ccl64-exec to build a standalone executable instead."
	fi
fi

if test x"${ecl}" = xtrue ; then
	if test x"$srcdir" = x"." ; then
		echo "Compiling maxima using \"${ECL_NAME}\""
	else
		echo "ECL enabled. Executable name: \"${ECL_NAME}\""
		AC_MSG_ERROR(For ECL out-of-tree builds aren't supported. See src/maxima.system for details.)
	fi
fi

if test x"${abcl}" = xtrue ; then
	if test x"$srcdir" = x"." ; then
		echo "Compiling maxima using \"${ABCL_JAR}\""
	else
		echo "ABCL enabled. ABCL jarfile: \"${ABCL_JAR}\""
		AC_MSG_ERROR(For ABCL out-of-tree builds aren't supported. See src/maxima.system for details.)
	fi
fi

echo "default lisp: $DEFAULTLISP"
echo "wish executable name: \"${WISH}\""

if test x"${chm}" = xtrue ; then
  echo "CHM help files enabled"
  echo "  HHC: \"${HHC}\""
fi
if test x"${xmaxima_exe}" = xtrue ; then
   echo "Windows xmaxima.exe enabled"
   echo "  GCC version GCCVER: ${GCCVER}"
   echo "  TCLKITSH: ${TCLKITSH}"
   if ! test -f "${TCLKITSH}" ; then
     AC_MSG_WARN(*** TCLKITSH ${TCLKITSH} not found)
     xmaxima_exe_prerequisite=notfound
   fi
   echo "  TCLKIT_RUNTIME: ${TCLKIT_RUNTIME}"
   if ! test -f "${TCLKIT_RUNTIME}" ; then
     AC_MSG_WARN(*** TCLKIT_RUNTIME ${TCLKIT_RUNTIME} not found)
     xmaxima_exe_prerequisite=notfound
   fi
   echo "  SDXKIT: ${SDXKIT}"
   if ! test -f "${SDXKIT}" ; then
     AC_MSG_WARN(*** SDXKIT ${SDXKIT} not found)
     xmaxima_exe_prerequisite=notfound
   fi
   echo "  IMGKIT: ${IMGKIT}"
   if ! test -f "${IMGKIT}" ; then
     AC_MSG_WARN(*** IMGKIT ${IMGKIT} not found)
     xmaxima_exe_prerequisite=notfound
   fi
   if test x${xmaxima_exe_prerequisite} = xnotfound ; then
     AC_MSG_WARN([A prerequisite for xmaxima.exe not found.  The missing components can be defined on the make command line.])
   fi
fi
