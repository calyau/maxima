MAKEINFOFLAGS = --enable-encoding

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
Affine.es.texi           Equations.es.texi     Lists.es.texi          Rules.es.texi \
Arrays.es.texi           Expressions.es.texi   Logarithms.es.texi     Runtime.es.texi \
Atensor.es.texi          Floating.es.texi      Matrices.es.texi       Series.es.texi \
Bugs.es.texi             Function.es.texi                             Simplification.es.texi \
Command.es.texi          Groups.es.texi        Miscellaneous.es.texi  SpecfunII.es.texi \
Constants.es.texi        Help.es.texi          nset.es.texi           Special.es.texi \
Contexts.es.texi         Indices.es.texi       Number.es.texi         Statistics.es.texi \
Ctensor.es.texi          Input.es.texi         Numerical.es.texi      Symmetries.es.texi \
Debugging.es.texi        Integration.es.texi   Operators.es.texi      Trigonometric.es.texi \
Differential.es.texi     Introduction.es.texi  Plotting.es.texi       xrefs.es.texi \
Differentiation.es.texi  Itensor.es.texi       Polynomials.es.texi \
Elliptic.es.texi         Limits.es.texi        Program.es.texi

maximahtml.es.mk:
	texi2html -split_chapter --lang=es --output=. maxima.texi
	rm -f maximahtml.es.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAESHTML += \1/' > maximahtml.es.mk

MAXIMAESHTML = 
include maximahtml.es.mk

$(MAXIMAESHTML): maxima.texi $(maxima_TEXINFOS)
	texi2html -split_chapter --lang=es --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/es
genericdirDATA = $(MAXIMAESHTML)

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima*.html

EXTRA_DIST = maximahtml.es.mk

