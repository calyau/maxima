all-local: info

info: operatingsystem.info operatingsystem-index.lisp

operatingsystem.info: operatingsystem.texi
	makeinfo $<

operatingsystem.pdf: operatingsystem.texi
	texi2pdf $<

operatingsystem-index.lisp: operatingsystem.info
	perl ../../../doc/info/build_index.pl $< > $@
