MAKEINFOFLAGS = --enable-encoding

langsdir = /pt_BR.utf8

info_TEXINFOS = maxima.texi

if USE_RECODE
    urecode=true
endif

fcharset = "ISO-8859-1"
tcharset = "UTF-8"

all-local: maxima-index.lisp

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':utf8' > maxima-index.lisp

maximahtml_pt_BR.mk: ../texi2html
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	rm -f maximahtml_pt_BR.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml_pt_BR.mk

MAXIMAHTML = 
include maximahtml_pt_BR.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt_BR.utf8
genericdirDATA = $(MAXIMAHTML)\
contents.hhc index.hhk header.hhp

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = maximahtml_pt_BR.mk header.hhp maxima-index.lisp $(genericdirDATA)

maxima.info: maxima.texi
	@rm -f $@ $@-[0-9] $@-[0-9][0-9]
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) \
	 -o $@ `test -f '$<' || echo '$(srcdir)/'`$<
	for f in $@ $@-[0-9] $@-[0-9][0-9]; do \
	    if test -f $$f; then \
		if test x$(urecode) = xtrue ; then \
		    recode $(fcharset)..$(tcharset) $$f ; \
		else \
		    rm -f foo.$$f 2>/dev/null ; \
		    iconv -f $(fcharset) -t $(tcharset) $$f > foo.$$f ; \
		    mv -f foo.$$f $$f ; \
		fi; \
	    fi; \
	done

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

