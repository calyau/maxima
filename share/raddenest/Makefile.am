all-local: info

info: raddenest.info raddenest-index.lisp

raddenest.info: raddenest.texi
	makeinfo --force $<

raddenest-index.lisp: raddenest.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = raddenest.texi

AM_MAKEINFOHTMLFLAGS = --no-split

EXTRA_DIST = raddenest-index.lisp
