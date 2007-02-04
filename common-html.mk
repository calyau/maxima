# Installation/uninstallation and distribution for .html files.
# htmlname variable should be set to html base name,
# for example maxima or xmaxima

install-data-local: install-maxima-html
install-maxima-html: $(htmlname).html
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(genericdir)
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/$(htmlname).html $(srcdir)/$(htmlname)_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  if test -f $(srcdir)/$$f; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(genericdir)/$$f"; \
	    $(INSTALL_DATA) $(srcdir)/$$f $(DESTDIR)$(genericdir)/$$f; \
	  else if test -f $$f; then \
            if test ! -d `dirname $(DESTDIR)$(genericdir)/$$f`; then \
              $(mkinstalldirs) `dirname $(DESTDIR)$(genericdir)/$$f`; \
            fi; \
	    echo " $(INSTALL_DATA) $$f $(DESTDIR)$(genericdir)/$$f"; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(genericdir)/$$f; \
	  fi; fi; \
	done

uninstall-local: uninstall-maxima-html
uninstall-maxima-html:
	@$(NORMAL_UNINSTALL)
	rm -f $(DESTDIR)$(genericdir)/$(htmlname).html 
	rm -f $(DESTDIR)$(genericdir)/$(htmlname)_*.html

dist-hook: dist-maxima-html
dist-maxima-html: $(htmlname).html
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list="$(srcdir)/$(htmlname).html $(srcdir)/$(htmlname)_*.html" ; \
	for p in $$list; do \
	  f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`; \
	  test -f $(distdir)/$$f || cp -p $(srcdir)/$$f $(distdir)/$$f; \
	done


