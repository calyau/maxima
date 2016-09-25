all-local: info

info: kovacicODE.info maxima-odesolve-index.lisp

kovacicODE.pdf: $(info_TEXINFOS)
	$(TEXI2PDF) $(AM_V_texinfo) -I $(top_srcdir)/doc/info -o $@ $<
	rm -f kovacicODE.fns kovacicODE.vr kovacicODE.tp kovacicODE.pg kovacicODE.ky \
	   kovacicODE.cp kovacicODE.toc kovacicODE.fn kovacicODE.aux kovacicODE.log

kovacicODE.info: kovacicODE.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
	  -I $(top_srcdir)/doc/info $<

maxima-odesolve-index.lisp: kovacicODE.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

AM_MAKEINFOHTMLFLAGS = --no-split

info_TEXINFOS = kovacicODE.texi

EXTRA_DIST = maxima-odesolve-index.lisp
