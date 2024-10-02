info_TEXINFOS = maxima.texi

all-local: maxima-index.lisp maxima-index-html.lisp index.html

maxima-index.lisp: maxima.info $(srcdir)/../build_index.pl
	/usr/bin/env perl $(srcdir)/../build_index.pl maxima.info ':crlf' > maxima-index.lisp

# Really depends on all the individual html files, but let's assume
# that if index.html is done, we have all the remaining html
# files.
#
# Load build-html-index.lisp and run the builder to create
# maxima-index-html.lisp.  In a clean directory, there won't be a
# maxima-index-html.lisp, so we don't want to try to verify the html
# index to prevent spurious warnings.  Then after it's done, run
# maxima to verify the index.
maxima-index-html.lisp : index.html $(top_srcdir)/doc/info/build-html-index.lisp
	MAXIMA_LANG_SUBDIR=$(lang) $(top_builddir)/maxima-local --no-init --no-verify-html-index --preload=$(top_srcdir)/doc/info/build-html-index.lisp --batch-string='build_and_dump_html_index("./*.html", lang = "$(lang)");'
	MAXIMA_LANG_SUBDIR=$(lang) $(top_builddir)/maxima-local --no-init --batch-string="quit();"

maxima_singlepage.html index.html: maxima.texi $(maxima_TEXINFOS) $(figurefiles) $(top_srcdir)/doc/info/manual.css $(top_srcdir)/doc/info/texi2html.init
	../build_html.sh -l $(lang)

maxima.pdf: maxima.texi $(maxima_TEXINFOS)
	$(TEXI2PDF) $(AM_V_texinfo) -I $(srcdir)/.. -o maxima.pdf $(srcdir)/maxima.texi
	rm -f maxima.fns maxima.vr maxima.tp maxima.pg maxima.ky maxima.cp \
	maxima.toc maxima.fn maxima.aux maxima.log maxima.vrs

include $(top_srcdir)/common.mk

# The basename for the html files for the manual.  Since we don't
# rename the html files, the html file names can basically have any
# name.
htmlname = *
htmlinstdir = $(dochtmldir)$(langsdir)
include $(top_srcdir)/common-html.mk

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info
	rm -f maxima.info*
	rm -f maxima-index.lisp
	rm -f maxima-index-html.lisp

clean-html:
	rm -f *.html

EXTRA_DIST = maxima-index.lisp maxima-index-html.lisp $(genericdirDATA) index.html


install-info-am: $(INFO_DEPS) maxima-index.lisp maxima-index-html.lisp
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
	$(INSTALL_DATA) maxima-index-html.lisp "$(DESTDIR)$(infodir)$(langsdir)/maxima-index-html.lisp"

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
	rm -f "$(DESTDIR)$(infodir)$(langsdir)/maxima-index-html.lisp"
