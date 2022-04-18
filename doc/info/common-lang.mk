info_TEXINFOS = maxima.texi

all-local: maxima-index.lisp maxima_toc.html

maxima-index.lisp: maxima.info $(srcdir)/../build_index.pl
	/usr/bin/env perl $(srcdir)/../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima_singlepage.html maxima_toc.html: maxima.texi $(maxima_TEXINFOS)
	../build_html.sh -l $(lang) -D

maxima.pdf: maxima.texi $(maxima_TEXINFOS)
	$(TEXI2PDF) $(AM_V_texinfo) -I $(srcdir)/.. -o maxima.pdf $(srcdir)/maxima.texi
	rm -f maxima.fns maxima.vr maxima.tp maxima.pg maxima.ky maxima.cp \
	maxima.toc maxima.fn maxima.aux maxima.log maxima.vrs

include $(top_srcdir)/common.mk

htmlname = maxima
htmlinstdir = $(dochtmldir)$(langsdir)
include $(top_srcdir)/common-html.mk

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima*.html
	rm -f maxima_singlepage.html

EXTRA_DIST = maxima-index.lisp $(genericdirDATA) maxima_toc.html


install-info-am: $(INFO_DEPS) maxima-index.lisp
	test -z "$(infodir)$(langsdir)" || mkdir -p -- "$(DESTDIR)$(infodir)$(langsdir)"
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  case $$file in \
	    $(srcdir)/*) file=`echo "$$file" | sed "s|^$$srcdirstrip/||"`;; \
	  esac; \
	  if test -f $$file; then d=.; else d=$(srcdir); fi; \
	  file_i=`echo "$$file" | sed 's|\.info$$||;s|$$|.i|'`; \
	  for ifile in $$d/$$file $$d/$$file-[0-9] $$d/$$file-[0-9][0-9] \
                       $$d/$$file_i[0-9] $$d/$$file_i[0-9][0-9] ; do \
	    if test -f $$ifile; then \
	      relfile=`echo "$$ifile" | sed 's|^.*/||'`; \
	      echo " $(INSTALL_DATA) '$$ifile' '$(DESTDIR)$(infodir)$(langsdir)/$$relfile'"; \
	      $(INSTALL_DATA) "$$ifile" "$(DESTDIR)$(infodir)$(langsdir)/$$relfile"; \
	    else : ; fi; \
	  done; \
	done
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"

uninstall-info-am:
	@list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  relfile=`echo "$$file" | sed 's|^.*/||'`; \
	  relfile_i=`echo "$$relfile" | sed 's|\.info$$||;s|$$|.i|'`; \
	  (if cd "$(DESTDIR)$(infodir)$(langsdir)"; then \
	     echo " cd '$(DESTDIR)$(infodir)$(langsdir)' && rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]"; \
	     rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]; \
	   else :; fi); \
	done
	rm -f "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"
