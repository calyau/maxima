# Installation/uninstallation and distribution for .html files.
# htmlname    -- html base name (e.g. maxima or xmaxima)
# htmlinstdir -- html installation directory  

install-data-local: install-maxima-html
install-maxima-html: $(wildcard $(htmlname).html $(htmlname)_*.html)
	@d=$(DESTDIR)$(htmlinstdir); \
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

uninstall-local: uninstall-maxima-html
uninstall-maxima-html:
	rm -f $(DESTDIR)$(htmlinstdir)/$(htmlname).html 
	rm -f $(DESTDIR)$(htmlinstdir)/$(htmlname)_*.html

dist-hook: dist-maxima-html
dist-maxima-html: $(wildcard $(htmlname).html $(htmlname)_*.html)
	@builddirstrip=`echo "$(builddir)" | sed 's|.|.|g'`; \
	list="$^" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$builddirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(builddir)/$$f $(distdir)/$$f; \
	done


