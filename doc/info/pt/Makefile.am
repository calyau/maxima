MAKEINFOFLAGS = --enable-encoding

langsdir = /pt

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
../pt_BR/Affine.texi \
../pt_BR/Arrays.texi \
../pt_BR/Atensor.texi \
../pt_BR/augmented_lagrangian.texi \
../pt_BR/bode.texi \
../pt_BR/Bugs.texi \
../pt_BR/cholesky.texi \
Command.texi \
../pt_BR/Constants.texi \
../pt_BR/Contexts.texi \
../pt_BR/Ctensor.texi \
Debugging.texi \
descriptive.texi \
../pt_BR/diag.texi \
../pt_BR/Differential.texi \
../pt_BR/Differentiation.texi \
distrib.texi \
dynamics.texi \
Elliptic.texi \
../pt_BR/Equations.texi \
../pt_BR/eval_string.texi \
../pt_BR/Expressions.texi \
../pt_BR/f90.texi \
../pt_BR/Floating.texi \
../pt_BR/Function.texi \
../pt_BR/ggf.texi \
../pt_BR/Groups.texi \
../pt_BR/Help.texi \
../pt_BR/impdiff.texi \
../pt_BR/implicit_plot.texi \
../pt_BR/Indices.texi \
../pt_BR/Input.texi \
../pt_BR/Integration.texi \
../pt_BR/interpol.texi \
../pt_BR/Introduction.texi \
../pt_BR/Itensor.texi \
../pt_BR/lbfgs.texi \
../pt_BR/Limits.texi \
../pt_BR/lindstedt.texi \
../pt_BR/linearalgebra.texi \
../pt_BR/Lists.texi \
Logarithms.texi \
../pt_BR/lsquares.texi \
../pt_BR/makeOrders.texi \
../pt_BR/Matrices.texi \
../pt_BR/Miscellaneous.texi \
../pt_BR/mnewton.texi \
../pt_BR/nset.texi \
../pt_BR/Number.texi \
../pt_BR/numericalio.texi \
../pt_BR/Numerical.texi \
../pt_BR/Operators.texi \
../pt_BR/opsubst.texi \
../pt_BR/orthopoly.texi \
plotdf.texi \
Plotting.texi \
../pt_BR/Polynomials.texi \
../pt_BR/Program.texi \
../pt_BR/romberg.texi \
../pt_BR/Rules.texi \
../pt_BR/Runtime.texi \
../pt_BR/Series.texi \
../pt_BR/simplex.texi \
../pt_BR/simplifications.texi \
../pt_BR/Simplification.texi \
../pt_BR/solve_rec.texi \
../pt_BR/Special.texi \
../pt_BR/Statistics.texi \
../pt_BR/stats.texi \
../pt_BR/stirling.texi \
stringproc.texi \
../pt_BR/Symmetries.texi \
../pt_BR/Trigonometric.texi \
../pt_BR/unit.texi \
../pt_BR/zeilberger.texi


all-local: maxima-index.lisp

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maximahtml_pt.mk: ../texi2html
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	rm -f maximahtml_pt.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml_pt.mk

MAXIMAHTML = 
include maximahtml_pt.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt
genericdirDATA = $(MAXIMAHTML)\
contents.hhc index.hhk header.hhp maxima.hhp

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = maximahtml_pt.mk header.hhp maxima.hhp maxima-index.lisp $(genericdirDATA)

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

