MAXIMA=maxima

check:
	(echo 'to_lisp();';\
	echo '(setq *maxima-testsdir* ".")';\
	echo '(to-maxima)';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo 'run_testsuite();') | $(MAXIMA) 

# Verbose output
check.log:
	(echo 'to_lisp();';\
	echo '(setq *maxima-testsdir* ".")';\
	echo '(to-maxima)';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo "put('contrib_ode,true,'verbose);";\
	echo 'run_testsuite(t,t);') | $(MAXIMA) > $@ 2>&1

