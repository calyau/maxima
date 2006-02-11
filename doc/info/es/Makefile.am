MAKEINFOFLAGS = --enable-encoding

langsdir = /es

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
Affine.es.texi           Equations.es.texi     Lists.es.texi          Rules.es.texi \
Arrays.es.texi           Expressions.es.texi   Logarithms.es.texi     Runtime.es.texi \
Atensor.es.texi          Floating.es.texi      Matrices.es.texi       Series.es.texi \
Bugs.es.texi             Function.es.texi                             Simplification.es.texi \
Command.es.texi          Groups.es.texi        Miscellaneous.es.texi \
Constants.es.texi        Help.es.texi          nset.es.texi           Special.es.texi \
Contexts.es.texi         Indices.es.texi       Number.es.texi         Statistics.es.texi \
Ctensor.es.texi          Input.es.texi         Numerical.es.texi      Symmetries.es.texi \
Debugging.es.texi        Integration.es.texi   Operators.es.texi      Trigonometric.es.texi \
Differential.es.texi     Introduction.es.texi  Plotting.es.texi  \
Differentiation.es.texi  Itensor.es.texi       Polynomials.es.texi \
Elliptic.es.texi         Limits.es.texi        Program.es.texi

maximahtml.es.mk:
	../texi2html -split_chapter --lang=es --output=. maxima.texi
	rm -f maximahtml.es.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAESHTML += \1/' > maximahtml.es.mk

MAXIMAESHTML = 
include maximahtml.es.mk

$(MAXIMAESHTML): maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=es --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/es
genericdirDATA = $(MAXIMAESHTML)

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima.html maxima_*.html

EXTRA_DIST = maximahtml.es.mk

install-info-am: $(INFO_DEPS)
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

