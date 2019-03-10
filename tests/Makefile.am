include $(top_srcdir)/common.mk
MAXIMA = $(abs_top_builddir)/maxima-local
MAXIMA_LAYOUT_AUTOTOOLS = true
export

TESTS =
CLEANFILES = 

if CLISP
TESTS += clisp-test.sh
CLEANFILES += clisp-test.sh clisp.log clisp_share.log
clisp-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh clisp" >> $@
	chmod +x "$@"
endif

if CMUCL
TESTS += cmucl-test.sh
CLEANFILES += cmucl-test.sh cmucl.log cmucl_share.log
cmucl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh cmucl" >> $@
	chmod +x "$@"
endif

if SCL
TESTS += scl-test.sh
CLEANFILES += scl-test.sh scl.log scl_share.log
scl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh scl" >> $@
	chmod +x "$@"
endif

if SBCL
TESTS += sbcl-test.sh
CLEANFILES += sbcl-test.sh sbcl.log sbcl_share.log
sbcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh sbcl" >> $@
	chmod +x "$@"
endif

if GCL
TESTS += gcl-test.sh
CLEANFILES += gcl-test.sh gcl.log gcl_share.log
gcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh gcl" >> $@
	chmod +x "$@"
endif

if ACL
TESTS += acl-test.sh
CLEANFILES += acl-test.sh acl.log acl_share.log
acl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh acl" >> $@
	chmod +x "$@"
endif

if OPENMCL
TESTS += openmcl-test.sh
CLEANFILES += openmcl-test.sh openmcl.log openmcl_share.log
openmcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh openmcl" >> $@
	chmod +x "$@"
endif

if CCL64
TESTS += ccl64-test.sh
CLEANFILES += ccl64-test.sh ccl64.log ccl64_share.log
ccl64-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ccl64" >> $@
	chmod +x "$@"
endif

if ECL
TESTS += ecl-test.sh
CLEANFILES += ecl-test.sh ecl.log ecl_share.log
ecl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ecl" >> $@
	chmod +x "$@"
endif

if ABCL
TESTS += abcl-test.sh
CLEANFILES += abcl-test.sh abcl.log abcl_share.log
abcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh abcl" >> $@
	chmod +x "$@"
endif

genericdir = $(insttestsdir)
genericdirDATA = $(wildcard $(addprefix $(srcdir)/, README.how-to mcond_display_elseif.out *.mac *.lisp *.wxm wester_problems/*))

EXTRA_DIST = $(genericdirDATA) test.sh.in
