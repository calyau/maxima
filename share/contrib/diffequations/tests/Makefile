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
	rtest_sym.mac rtest_sym2.mac \
	rtest_ode1_riccati.mac rtest_ode1_abel.mac \
	rtestode_odelin.mac rtestode_utils.mac

# cunning macros for COMMA and SPACE characters (see GNU Make manual 8.1 Function Call Syntax)
COMMA:=,
EMPTY:=
SPACE:=$(EMPTY) $(EMPTY)
COMMA_SEPARATED_TEST_FILES:=\"$(subst $(SPACE),\"$(COMMA)\",$(strip $(TEST_FILES:.mac=)))\"

check: $(TEST_FILES)
	(echo "testsuite_files:[$(COMMA_SEPARATED_TEST_FILES)];";\
	echo 'file_search_tests:["$$$$$$.{fas,lisp,lsp,mac,mc,wxm}"];';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo 'run_testsuite();') | $(MAXIMA)

# Verbose output
check.log: $(TEST_FILES)
	(echo "testsuite_files:[$(COMMA_SEPARATED_TEST_FILES)];";\
	echo 'file_search_tests:["$$$$$$.{fas,lisp,lsp,mac,mc,wxm}"];';\
	echo "load('pdiff);";\
	echo "use_pdiff:false;";\
	echo "load('contrib_ode);";\
	echo "put('contrib_ode,true,'verbose);";\
	echo "DEBUGFLAG:2;";\
	echo 'run_testsuite(display_all=true);') | $(MAXIMA) > $@ 2>&1

