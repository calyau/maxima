all-local: info

info: drawutils.info draw-index.lisp

drawutils.info: drawutils.texi
	makeinfo $<

draw-index.lisp: drawutils.info
	perl  $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = drawutils.texi

EXTRA_DIST = draw-index.lisp
