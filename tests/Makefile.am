include $(top_srcdir)/common.mk
MAXIMA = @POSIX_SHELL@ $(top_srcdir)/src/maxima
if CLISP
check: clisp-check
clisp-check:
	@MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with clisp..."; \
	$(MAXIMA) --lisp=clisp --batch-lisp=tests.lisp > tests-clisp.log < /dev/null 2>&1; \
	./summarize-log tests-clisp.log
endif

if CMUCL
check: cmucl-check
cmucl-check:
	@MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with cmucl..."; \
	$(MAXIMA) --lisp=cmucl --batch-lisp=tests.lisp > tests-cmucl.log < /dev/null 2>&1; \
	./summarize-log tests-cmucl.log
endif

if GCL
check: gcl-check
gcl-check:
	@MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with gcl..."; \
	$(MAXIMA) --lisp=gcl --batch-lisp=tests.lisp > tests-gcl.log < /dev/null 2>&1; \
	./summarize-log tests-gcl.log
endif

install-exec-local:
	$(mkinstalldirs) $(DESTDIR)$(insttestsdir)
	$(INSTALL_PROGRAM) summarize-log $(DESTDIR)$(insttestsdir)/summarize-log

uninstall-local:
	rm -f $(DESTDIR)$(insttestsdir)/summarize-log

genericdir = $(insttestsdir)
genericdirDATA = ltest2.lisp \
ltests.lisp \
rexamples.mac \
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
tests.lisp
