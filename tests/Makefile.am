include $(top_srcdir)/common.mk
MAXIMA = @POSIX_SHELL@ $(top_builddir)/maxima-local
if CLISP
check: check-clisp
check-clisp:
	echo "Running test suite with clisp..."; \
	$(MAXIMA) --lisp=clisp --batch-string="run_testsuite();" < /dev/null
endif

if CMUCL
check: check-cmucl
check-cmucl:
	echo "Running test suite with cmucl..."; \
	$(MAXIMA) --lisp=cmucl --batch-string="run_testsuite();" < /dev/null
endif

if SCL
check: check-scl
check-scl:
	echo "Running test suite with scl..."; \
	$(MAXIMA) --lisp=scl --batch-string="run_testsuite();" < /dev/null
endif

if SBCL
check: check-sbcl
check-sbcl:
	echo "Running test suite with sbcl..."; \
	$(MAXIMA) --lisp=sbcl --batch-string="run_testsuite();" < /dev/null
endif

if GCL
check: check-gcl
check-gcl:
	echo "Running test suite with gcl..."; \
	$(MAXIMA) --lisp=gcl --batch-string="run_testsuite();" < /dev/null
endif

if ACL
check: check-acl
check-acl:
	echo "Running test suite with acl..."; \
	$(MAXIMA) --lisp=acl --batch-string="run_testsuite();" < /dev/null
endif

if OPENMCL
check: check-openmcl
check-openmcl:
	echo "Running test suite with openmcl..."; \
	$(MAXIMA) --lisp=openmcl --batch-string="run_testsuite();" < /dev/null
endif

if ECL
check: check-ecl
check-ecl:
	echo "Running test suite with ecl..."; \
	$(MAXIMA) --lisp=ecl --batch-string="run_testsuite();" < /dev/null
endif

genericdir = $(insttestsdir)
genericdirDATA = $(shell echo README.how-to mcond_display_elseif.out *.mac *.lisp wester_problems/*)

EXTRA_DIST = $(genericdirDATA)
