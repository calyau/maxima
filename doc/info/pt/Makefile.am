MAKEINFOFLAGS = --enable-encoding

langsdir = /pt

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
Affine.texi           Equations.texi     Lists.texi          Rules.texi \
Arrays.texi           Expressions.texi   Logarithms.texi     Runtime.texi \
Atensor.texi          Floating.texi      Matrices.texi       Series.texi \
Bugs.texi             Function.texi      Simplification.texi \
Command.texi          Groups.texi        Miscellaneous.texi  SpecfunII.texi \
Constants.texi        Help.texi          nset.texi           Special.texi \
Contexts.texi         Indices.texi       Number.texi         Statistics.texi \
Ctensor.texi          Input.texi         Numerical.texi      Symmetries.texi \
Debugging.texi        Integration.texi   Operators.texi      Trigonometric.texi \
Differential.texi     Introduction.texi  Plotting.texi \
Differentiation.texi  Itensor.texi       Polynomials.texi \
Elliptic.texi         Limits.texi        Program.texi

maximahtml_pt.mk:
	../texi2html -split_chapter --lang=pt --output=. maxima.texi
	rm -f maximahtml_pt.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml_pt.mk

MAXIMAHTML = 
include maximahtml_pt.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt
genericdirDATA = $(MAXIMAHTML)

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima.html maxima_*.html

EXTRA_DIST = maximahtml_pt.mk

install-info-am: $(INFO_DEPS)
	test -z "$(infodir)$(langsdir)" || $(mkdir_p) "$(DESTDIR)$(infodir)$(langsdir)"
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

