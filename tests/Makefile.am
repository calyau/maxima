MAXIMA = $(SHELL) $(top_srcdir)/src/maxima
if CLISP
check: clisp-check
clisp-check:
	MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with clisp..."; \
	$(MAXIMA) --lisp=clisp --batch-lisp=tests.lisp > tests-clisp.log 2>&1; \
	./summarize-log tests-clisp.log
endif

if CMUCL
check: cmucl-check
cmucl-check:
	MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with cmucl..."; \
	$(MAXIMA) --lisp=cmucl --batch-lisp=tests.lisp > tests-cmucl.log 2>&1; \
	./summarize-log tests-cmucl.log
endif

if GCL
check: gcl-check
gcl-check:
	MAXIMA_VERPKGLIBEXECDIR=$(top_srcdir)/lisp-utils;\
	export MAXIMA_VERPKGLIBEXECDIR; \
	MAXIMA_VERPKGLIBDIR=$(top_srcdir)/src; \
	export MAXIMA_VERPKGLIBDIR; \
	echo "Running test suite with gcl..."; \
	$(MAXIMA) --lisp=gcl --batch-lisp=tests.lisp > tests-gcl.log 2>&1; \
	./summarize-log tests-gcl.log
endif
