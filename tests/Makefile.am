include $(top_srcdir)/common.mk
MAXIMA = @POSIX_SHELL@ $(top_srcdir)/maxima-local
if CLISP
check: clisp-check
clisp-check:
	echo "Running test suite with clisp..."; \
	$(MAXIMA) --lisp=clisp --batch-lisp=tests.lisp > tests-clisp.log < /dev/null 2>&1; \
	./summarize-log tests-clisp.log
endif

if CMUCL
check: cmucl-check
cmucl-check:
	echo "Running test suite with cmucl..."; \
	$(MAXIMA) --lisp=cmucl --batch-lisp=tests.lisp > tests-cmucl.log < /dev/null 2>&1; \
	./summarize-log tests-cmucl.log
endif

if SBCL
check: sbcl-check
sbcl-check:
	echo "Running test suite with sbcl..."; \
	$(MAXIMA) --lisp=sbcl --batch-lisp=tests.lisp > tests-sbcl.log < /dev/null 2>&1; \
	./summarize-log tests-sbcl.log
endif

if GCL
check: gcl-check
gcl-check:
	echo "Running test suite with gcl..."; \
	$(MAXIMA) --lisp=gcl --batch-lisp=tests.lisp > tests-gcl.log < /dev/null 2>&1; \
	./summarize-log tests-gcl.log
endif

if ACL6
check: acl6-check
acl6-check:
	echo "Running test suite with acl6..."; \
	$(MAXIMA) --lisp=acl6 --batch-lisp=tests.lisp > tests-acl6.log < /dev/null 2>&1; \
	./summarize-log tests-acl6.log
endif

if OPENMCL
check: openmcl-check
openmcl-check:
	echo "Running test suite with openmcl..."; \
	$(MAXIMA) --lisp=openmcl --batch-lisp=tests.lisp > tests-openmcl.log < /dev/null 2>&1; \
	./summarize-log tests-openmcl.log
endif

install-exec-local:
	$(mkinstalldirs) $(DESTDIR)$(insttestsdir)
	$(INSTALL_PROGRAM) summarize-log $(DESTDIR)$(insttestsdir)/summarize-log

uninstall-local:
	rm -f $(DESTDIR)$(insttestsdir)/summarize-log

genericdir = $(insttestsdir)
genericdirDATA = rexamples.mac \
rtest10.mac \
rtest11.mac \
rtest12.mac \
rtest13.mac \
rtest13s.mac \
rtest14.mac \
rtest15.mac \
rtest1a.mac \
rtest1.mac \
rtest2.mac \
rtest3.mac \
rtest4.mac \
rtest5.mac \
rtest6a.mac \
rtest6b.mac \
rtest6.mac \
rtest7.mac \
rtest8.mac \
rtest9a.mac \
rtest9.mac \
rtestode.mac \
rtestode_zp.mac \
rtestflatten.mac \
tests.lisp

EXTRA_DIST = $(genericdirDATA) summarize-log known-bugs
