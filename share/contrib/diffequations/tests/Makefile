MAXIMA=../../../../maxima-local

TEST_FILES = rtestode_murphy1.mac rtestode_murphy2.mac \
	rtestode_kamke_1_1.mac rtestode_kamke_1_2.mac rtestode_kamke_1_3.mac \
	rtest_sym.mac rtest_sym2.mac \
	rtest_ode1_riccati.mac rtest_ode1_abel.mac

# Note the .MAC file extension.  Some fallout from the downcasing.  **FIXME**
#
check: $(TEST_FILES)
	(echo 'to_lisp();';\
	echo '(setq *maxima-testsdir* ".")';\
	echo '(to-maxima)';\
	echo 'file_search_maxima:append(["../###.MAC"],file_search_maxima);';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo 'run_testsuite();') | $(MAXIMA) 

# Verbose output
check.log: $(TEST_FILES)
	(echo 'to_lisp();';\
	echo '(setq *maxima-testsdir* ".")';\
	echo '(to-maxima)';\
	echo 'file_search_maxima:append(["../###.MAC"],file_search_maxima);';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo "put('contrib_ode,true,'verbose);";\
	echo 'run_testsuite(t,t);') | $(MAXIMA) > $@ 2>&1

