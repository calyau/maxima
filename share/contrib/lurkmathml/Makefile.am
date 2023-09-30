all-local: info

info: mathml.info mathml-index.lisp

mathml.info: mathml.texi
	makeinfo --force $<

mathml-index.lisp: mathml.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = mathml.texi

AM_MAKEINFOHTMLFLAGS = --no-split

EXTRA_DIST = mathml-index.lisp
