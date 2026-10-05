include $(top_srcdir)/common.mk
MAXIMA = $(abs_top_builddir)/maxima-local
MAXIMA_LAYOUT_AUTOTOOLS = true
export

TESTS =
CLEANFILES = 
INTERRUPT_CHANNEL_TESTS =

if CLISP
TESTS += clisp-test.sh
INTERRUPT_CHANNEL_TESTS += clisp-interrupt-channel.sh
CLEANFILES += clisp-test.sh clisp.log clisp_share.log
clisp-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh clisp" >> $@
	chmod +x "$@"
endif

if CMUCL
TESTS += cmucl-test.sh
INTERRUPT_CHANNEL_TESTS += cmucl-interrupt-channel.sh
CLEANFILES += cmucl-test.sh cmucl.log cmucl_share.log
cmucl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh cmucl" >> $@
	chmod +x "$@"
endif

if SBCL
TESTS += sbcl-test.sh
INTERRUPT_CHANNEL_TESTS += sbcl-interrupt-channel.sh
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
TESTS += gcl-test.sh
INTERRUPT_CHANNEL_TESTS += gcl-interrupt-channel.sh
CLEANFILES += gcl-test.sh gcl.log gcl_share.log
gcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh gcl" >> $@
	chmod +x "$@"
endif

if ACL
TESTS += acl-test.sh
INTERRUPT_CHANNEL_TESTS += acl-interrupt-channel.sh
CLEANFILES += acl-test.sh acl.log acl_share.log
acl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh acl" >> $@
	chmod +x "$@"
endif

if OPENMCL
TESTS += openmcl-test.sh
INTERRUPT_CHANNEL_TESTS += openmcl-interrupt-channel.sh
CLEANFILES += openmcl-test.sh openmcl.log openmcl_share.log
openmcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh openmcl" >> $@
	chmod +x "$@"
endif

if CCL64
TESTS += ccl64-test.sh
INTERRUPT_CHANNEL_TESTS += ccl64-interrupt-channel.sh
CLEANFILES += ccl64-test.sh ccl64.log ccl64_share.log
ccl64-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ccl64" >> $@
	chmod +x "$@"
endif

if ECL
TESTS += ecl-test.sh
INTERRUPT_CHANNEL_TESTS += ecl-interrupt-channel.sh
CLEANFILES += ecl-test.sh ecl.log ecl_share.log
ecl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh ecl" >> $@
	chmod +x "$@"
endif

if ABCL
TESTS += abcl-test.sh
INTERRUPT_CHANNEL_TESTS += abcl-interrupt-channel.sh
CLEANFILES += abcl-test.sh abcl.log abcl_share.log
abcl-test.sh: test.sh Makefile
	echo "#!/bin/sh" > $@
	echo "./test.sh abcl" >> $@
	chmod +x "$@"
endif

## Interrupt Maxima through the channel src/server.lisp opens for xmaxima,
## on every configured Lisp.  Exits 77 (SKIP) on a Lisp without threads, or
## when there is no tclsh.
TESTS += $(INTERRUPT_CHANNEL_TESTS)
CLEANFILES += $(INTERRUPT_CHANNEL_TESTS)

$(INTERRUPT_CHANNEL_TESTS): interruptcheck.sh Makefile
	echo "#!/bin/sh" > $@
	echo "exec ./interruptcheck.sh $(patsubst %-interrupt-channel.sh,%,$@)" >> $@
	chmod +x "$@"

genericdir = $(insttestsdir)
genericdirDATA = $(wildcard $(addprefix $(srcdir)/, README.how-to mcond_display_elseif.out *.mac *.lisp *.wxm *.wxmx load_relative/* wester_problems/*))

EXTRA_DIST = $(genericdirDATA) interruptcheck.sh.in interruptcheck.tcl test.sh.in depcheck.sh.in
