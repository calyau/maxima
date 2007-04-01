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
rtestnset.mac \
rtesthyp.mac \
rtestmt19937.mac \
mcond_display_elseif.mac \
mcond_display_elseif.out \
rtestmcond_display.mac \
rtest_allnummod.mac \
rtestconjugate.mac \
rtestsum.mac \
rtest_trig.mac \
rtest_zeta.mac \
rtest_diff_invtrig.mac \
rtest_scalarp.mac \
rtest_everysome.mac \
rtestint.mac \
rtestifactor.mac \
rtest_equal.mac \
rtest_abs.mac \
rtest_taylor.mac \
rtest_dot.mac \
rtest_mset.mac \
rtest_boolean.mac \
testsuite.lisp

EXTRA_DIST = $(genericdirDATA)
