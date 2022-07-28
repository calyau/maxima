all-local: info

info: nelder_mead.info nelder_mead-index.lisp

nelder_mead.info: nelder_mead.texi
	makeinfo --force $<

nelder_mead-index.lisp: nelder_mead.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = nelder_mead.texi

AM_MAKEINFOHTMLFLAGS = --no-split

EXTRA_DIST = nelder_mead-index.lisp
