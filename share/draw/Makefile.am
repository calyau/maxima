all-local: info

info: drawutils.info draw-index.lisp

drawutils.info: drawutils.texi
	makeinfo $<

draw-index.lisp: drawutils.info
	perl ../../doc/info/build_index.pl $< > $@
