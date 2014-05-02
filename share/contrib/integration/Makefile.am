all-local: info

info: abs_integrate.info integration-index.lisp

abs_integrate.info: abs_integrate.texi
	makeinfo $<

integration-index.lisp: abs_integrate.info
	perl ../../../doc/info/build_index.pl $< > $@
