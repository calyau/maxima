all-local: info

info: kovacicODE.info maxima-odesolve-index.lisp

kovacicODE.info: kovacicODE.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
	  -I $(top_srcdir)/doc/info $<

maxima-odesolve-index.lisp: kovacicODE.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@
