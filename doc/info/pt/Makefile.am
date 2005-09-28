MAKEINFOFLAGS = --enable-encoding

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
Affine_pt.texi           Equations_pt.texi     Lists_pt.texi          Rules.texi \
Arrays_pt.texi           Expressions.texi      Logarithms.texi        Runtime_pt.texi \
Atensor_pt.texi          Floating_pt.texi      Matrices_pt.texi       Series_pt.texi \
Bugs_pt.texi             Function_pt.texi                             Simplification_pt.texi \
Command_pt.texi          Groups_pt.texi        Miscellaneous_pt.texi  SpecfunII_pt.texi \
Constants_pt.texi        Help_pt.texi          nset_pt.texi           Special_pt.texi \
Contexts.texi            Indices_pt.texi       Number_pt.texi         Statistics_pt.texi \
Ctensor.texi             Input_pt.texi         Numerical_pt.texi      Symmetries.texi \
Debugging.texi           Integration_pt.texi   Operators_pt.texi      Trigonometric_pt.texi \
Differential_pt.texi     Introduction_pt.texi  Plotting_pt.texi \
Differentiation_pt.texi  Itensor.texi          Polynomials_pt.texi \
Elliptic_pt.texi         Limits_pt.texi        Program_pt.texi

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

