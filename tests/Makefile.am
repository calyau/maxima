include $(top_srcdir)/common.mk
MAXIMA = @POSIX_SHELL@ $(top_srcdir)/maxima-local
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

if ACL6
check: check-acl6
check-acl6:
	echo "Running test suite with acl6..."; \
	$(MAXIMA) --lisp=acl6 --batch-string="run_testsuite();" < /dev/null
endif

if OPENMCL
check: check-openmcl
check-openmcl:
	echo "Running test suite with openmcl..."; \
	$(MAXIMA) --lisp=openmcl --batch-string="run_testsuite();" < /dev/null
endif

genericdir = $(insttestsdir)
genericdirDATA = rexamples.mac \
rtest10.mac \
rtest11.mac \
rtest12.mac \
rtest13.mac \
rtest13s.mac \
rtest14.mac \
rtest15.mac \
rtest16.mac \
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
rtestnset.mac \
rtesthyp.mac \
testsuite.lisp

EXTRA_DIST = $(genericdirDATA)
