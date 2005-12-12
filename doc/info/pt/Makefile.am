MAKEINFOFLAGS = --enable-encoding

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
	texi2html -split_chapter --lang=pt --output=. maxima.texi
	rm -f maximahtml_pt.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml_pt.mk

MAXIMAHTML = 
include maximahtml_pt.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt
genericdirDATA = $(MAXIMAHTML)

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima.html maxima_*.html

EXTRA_DIST = maximahtml_pt.mk

