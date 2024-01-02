all-local: info

info: guess.info guess-index.lisp

guess.info: guess.texi
	makeinfo $<

guess-index.lisp: guess.info
	perl  $(top_srcdir)/doc/info/build_index.pl $< > $@

info_TEXINFOS = guess.texi

AM_MAKEINFOHTMLFLAGS = --no-split

EXTRA_DIST = guess-index.lisp
