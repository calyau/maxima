include $(top_srcdir)/common.mk
MAXIMA = $(abs_top_builddir)/maxima-local
MAXIMA_LAYOUT_AUTOTOOLS = true
export

TESTS = threadcheck-driver-test.sh
CLEANFILES = 
THREAD_BINDING_TESTS =
THREAD_RACE_TESTS =

if CLISP
THREAD_BINDING_TESTS += clisp-thread-bindings.sh
THREAD_RACE_TESTS += clisp-thread-race.sh
TESTS += clisp-test.sh
CLEANFILES += clisp-test.sh clisp.log clisp_share.log
clisp-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh clisp" >> $@
	chmod +x "$@"
endif

if CMUCL
THREAD_BINDING_TESTS += cmucl-thread-bindings.sh
THREAD_RACE_TESTS += cmucl-thread-race.sh
TESTS += cmucl-test.sh
CLEANFILES += cmucl-test.sh cmucl.log cmucl_share.log
cmucl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh cmucl" >> $@
	chmod +x "$@"
endif

if SBCL
THREAD_BINDING_TESTS += sbcl-thread-bindings.sh
THREAD_RACE_TESTS += sbcl-thread-race.sh
TESTS += sbcl-test.sh
CLEANFILES += sbcl-test.sh sbcl.log sbcl_share.log
sbcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh sbcl" >> $@
	chmod +x "$@"

## Verifies src/maxima.system's :DEPENDENCIES-COMPLETE claims against the
## compiler's cross-reference data.  SBCL-only, so it is attached here
## rather than to the general per-lisp driver.
TESTS += sbcl-depcheck.sh
CLEANFILES += sbcl-depcheck.sh
sbcl-depcheck.sh: depcheck.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./depcheck.sh" >> $@
	chmod +x "$@"
endif

if GCL
THREAD_BINDING_TESTS += gcl-thread-bindings.sh
THREAD_RACE_TESTS += gcl-thread-race.sh
TESTS += gcl-test.sh
CLEANFILES += gcl-test.sh gcl.log gcl_share.log
gcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh gcl" >> $@
	chmod +x "$@"
endif

if ACL
THREAD_BINDING_TESTS += acl-thread-bindings.sh
THREAD_RACE_TESTS += acl-thread-race.sh
TESTS += acl-test.sh
CLEANFILES += acl-test.sh acl.log acl_share.log
acl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh acl" >> $@
	chmod +x "$@"
endif

if OPENMCL
THREAD_BINDING_TESTS += openmcl-thread-bindings.sh
THREAD_RACE_TESTS += openmcl-thread-race.sh
TESTS += openmcl-test.sh
CLEANFILES += openmcl-test.sh openmcl.log openmcl_share.log
openmcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh openmcl" >> $@
	chmod +x "$@"
endif

if CCL64
THREAD_BINDING_TESTS += ccl64-thread-bindings.sh
THREAD_RACE_TESTS += ccl64-thread-race.sh
TESTS += ccl64-test.sh
CLEANFILES += ccl64-test.sh ccl64.log ccl64_share.log
ccl64-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ccl64" >> $@
	chmod +x "$@"
endif

if ECL
THREAD_BINDING_TESTS += ecl-thread-bindings.sh
THREAD_RACE_TESTS += ecl-thread-race.sh
TESTS += ecl-test.sh
CLEANFILES += ecl-test.sh ecl.log ecl_share.log
ecl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ecl" >> $@
	chmod +x "$@"
endif

if ABCL
THREAD_BINDING_TESTS += abcl-thread-bindings.sh
THREAD_RACE_TESTS += abcl-thread-race.sh
TESTS += abcl-test.sh
CLEANFILES += abcl-test.sh abcl.log abcl_share.log
abcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh abcl" >> $@
	chmod +x "$@"
endif

## Run the bindings on every configured Lisp.  A separate race test exits
## 77 where unsupported, so Automake reports SKIP instead of a false PASS.
TESTS += $(THREAD_BINDING_TESTS) $(THREAD_RACE_TESTS)
CLEANFILES += $(THREAD_BINDING_TESTS) $(THREAD_RACE_TESTS)

$(THREAD_BINDING_TESTS): threadcheck.sh Makefile
	echo "#!/bin/sh" > $@
	echo "exec ./threadcheck.sh $(patsubst %-thread-bindings.sh,%,$@) bindings" >> $@
	chmod +x "$@"

$(THREAD_RACE_TESTS): threadcheck.sh Makefile
	echo "#!/bin/sh" > $@
	echo "exec ./threadcheck.sh $(patsubst %-thread-race.sh,%,$@) race" >> $@
	chmod +x "$@"

genericdir = $(insttestsdir)
genericdirDATA = $(wildcard $(addprefix $(srcdir)/, README.how-to mcond_display_elseif.out *.mac *.lisp *.wxm *.wxmx load_relative/* wester_problems/*))

EXTRA_DIST = $(genericdirDATA) test.sh.in depcheck.sh.in threadcheck.sh.in \
             threadcheck-driver-test.sh README.threadcheck
