all-local: info

info: kovacicODE.info maxima-odesolve-index.lisp

kovacicODE.info: kovacicODE.texi
	makeinfo $<

maxima-odesolve-index.lisp: kovacicODE.info
	perl ../../../doc/info/build_index.pl $< > $@
