all-local: info

info: logic.info logic-index.lisp

logic.info: logic.texi
	makeinfo --force $<

logic-index.lisp: logic.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = logic.texi

AM_MAKEINFOHTMLFLAGS = --no-split

EXTRA_DIST = logic-index.lisp
