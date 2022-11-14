MAXIMA=../../../../maxima-local

TEST_FILES = \
        rtestode_murphy_1_1.mac rtestode_murphy_1_2.mac rtestode_murphy_1_3.mac \
        rtestode_murphy_1_4.mac rtestode_murphy_1_5.mac rtestode_murphy_1_6.mac \
        rtestode_murphy_2_1.mac rtestode_murphy_2_2.mac rtestode_murphy_2_3.mac \
        rtestode_murphy_2_4.mac rtestode_murphy_2_5.mac  \
	rtestode_kamke_1_1.mac rtestode_kamke_1_2.mac rtestode_kamke_1_3.mac \
	rtestode_kamke_1_4.mac rtestode_kamke_1_5.mac rtestode_kamke_1_6.mac \
	rtestode_kamke_2_1.mac rtestode_kamke_2_2.mac rtestode_kamke_2_3.mac \
	rtestode_kamke_2_4.mac rtestode_kamke_2_5.mac \
	rtest_ode_sym.mac rtest_ode_sym2.mac \
	rtest_ode1_riccati.mac rtest_ode1_abel.mac \
	rtestode_odelin.mac rtestode_utils.mac

check: setup_tests.mac $(TEST_FILES)
	(echo 'load("setup_tests.mac");';\
	echo 'run_testsuite();') | $(MAXIMA)

# Verbose output
check.log: setup_tests.mac $(TEST_FILES)
	(echo 'load("setup_tests.mac");';\
	echo "put('contrib_ode,true,'verbose);";\
	echo "DEBUGFLAG:2;";\
	echo 'run_testsuite(display_all=true);') | $(MAXIMA) > $@ 2>&1
