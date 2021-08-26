all-local: info html

info: symplectic_ode.info symplectic_ode-index.lisp
html: symplectic_ode.html

symplectic_ode.pdf: $(info_TEXINFOS)
	$(TEXI2PDF) $(AM_V_texinfo) -I $(top_srcdir)/doc/info -o $@ $<
	rm -f symplectic_ode.fns symplectic_ode.vr symplectic_ode.tp symplectic_ode.pg symplectic_ode.ky \
	   symplectic_ode.cp symplectic_ode.toc symplectic_ode.fn symplectic_ode.aux symplectic_ode.log

symplectic_ode.info: symplectic_ode.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
	  -I $(top_srcdir)/doc/info $<

symplectic_ode-index.lisp: symplectic_ode.info
	perl $(top_srcdir)/doc/info/build_index.pl $< > $@

AM_MAKEINFOHTMLFLAGS = --no-split

info_TEXINFOS = symplectic_ode.texi

EXTRA_DIST = symplectic_ode-index.lisp
