info_TEXINFOS = maxima.es.texi
maxima_TEXINFOS = Introduction.es.texi Help.es.texi Command.es.texi Operators.es.texi \
  Expressions.es.texi Simplification.es.texi Plotting.es.texi Input.es.texi \
  Floating.es.texi Contexts.es.texi Polynomials.es.texi Constants.es.texi \
  Logarithms.es.texi Trigonometric.es.texi Special.es.texi SpecfunII.es.texi \
  Limits.es.texi Differentiation.es.texi Integration.es.texi Equations.es.texi \
  Differential.es.texi Numerical.es.texi \
  Statistics.es.texi Arrays.es.texi Matrices.es.texi Affine.es.texi Itensor.es.texi \
  Ctensor..es.texi Atensor.es.texi Series.es.texi Number.es.texi Symmetries.es.texi \
  Groups.es.texi Runtime.es..texi Miscellaneous.es.texi Rules.es.texi Lists.es.texi \
  Function.es.texi Program.es.texi Debugging.es.texi Indices.es.texi Elliptic.es.texi \
  Bugs.es..texi

maximahtml.mk:
	./texi2html -split_chapter maximaes..texi
	rm -f maxima-html.es.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtmles..mk

MAXIMAHTML = 
include maximahtml.es.mk

$(MAXIMAHTML): maxima.es.texi $(maxima_TEXINFOS)
	./texi2html -split_chapter maxima.es.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = $(MAXIMAHTML)

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima*.html

EXTRA_DIST = texi2html maximahtml.es.mk

