# Run contrib_ode testsuite using installed maxima

check:
	maxima --batch-lisp=tests.lisp > tests.log < /dev/null 2>&1
	./summarize-log tests.log
