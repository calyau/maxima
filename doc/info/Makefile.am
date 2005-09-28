# COMMENT OUT SUBDIRS UNTIL PROBLEM WITH TEXI2HTML VERSION IS RESOLVED 
# SUBDIRS = es pt

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = Introduction.texi Help.texi Command.texi Operators.texi \
  Expressions.texi Simplification.texi Plotting.texi Input.texi \
  Floating.texi Contexts.texi Polynomials.texi Constants.texi \
  Logarithms.texi Trigonometric.texi Special.texi SpecfunII.texi \
  Limits.texi Differentiation.texi Integration.texi Equations.texi \
  Differential.texi Numerical.texi \
  Statistics.texi Arrays.texi Matrices.texi Affine.texi Itensor.texi \
  Ctensor.texi Atensor.texi Series.texi Number.texi Symmetries.texi \
  Groups.texi Runtime.texi Miscellaneous.texi Rules.texi Lists.texi \
  Function.texi Program.texi Debugging.texi Indices.texi Elliptic.texi \
  Bugs.texi nset.texi

maximahtml.mk:
	./texi2html -split_chapter maxima.texi
	rm -f maxima-html.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml.mk

MAXIMAHTML = 
include maximahtml.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	./texi2html -split_chapter maxima.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = $(MAXIMAHTML)

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima*.html

EXTRA_DIST = texi2html maximahtml.mk manual.css texi2html.init
