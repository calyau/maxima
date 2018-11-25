# Installation/uninstallation and distribution for .html files.
# infoname -- info base name (e.g. maxima or xmaxima)
# infodir  -- info installation directory
# langsdir -- info installation subdirectory

install-data-local: install-maxima-info
install-maxima-info: $(wildcard $(infoname).info* $(infoname)-index.lisp)
	@d=$(DESTDIR)$(infodir)$(langsdir); \
	test -d $$d && $(mkinstalldirs) $$d; \
	list="$^"; for p in $$list; do \
	  b=$${p#$(builddir)/}; \
	  s=$${p#$(srcdir)/}; \
	  if test -f $(builddir)/$$b; then \
	    t=`dirname $$d/$$b`; \
            test -d $$t || $(mkinstalldirs) $$t; \
	    echo " $(INSTALL_DATA) BUILDDIR/$$b $$d/$$b"; \
	    $(INSTALL_DATA) $(builddir)/$$b $$d/$$b; \
	  elif test -f $(srcdir)/$$s; then \
	    t=`dirname $$d/$$s`; \
            test -d $$t || $(mkinstalldirs) $$t; \
	    echo " $(INSTALL_DATA) SRCDIR/$$s $$d/$$s"; \
	    $(INSTALL_DATA) $(srcdir)/$$s $$d/$$s; \
	  elif test -f $$p; then \
	    t=`dirname $$d/$$p`; \
            test -d $$t || $(mkinstalldirs) $$t; \
	    echo " $(INSTALL_DATA) $$p $$d/$$p"; \
	    $(INSTALL_DATA) $$p $$d/$$p; \
	  fi; \
	done

uninstall-local: uninstall-maxima-info
uninstall-maxima-info:
	rm -f $(DESTDIR)$(infodir)$(langsdir)/$(infoname).info*
	rm -f $(DESTDIR)$(infodir)$(langsdir)/$(infoname)-index.lisp

dist-hook: dist-maxima-info
dist-maxima-info:
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/$(infoname).info*" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(srcdir)/$$f $(distdir)/$$f; \
	done
