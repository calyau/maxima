if LANG_ES
  ES_DIR = es
endif

if LANG_ES_UTF8
  ES_DIR_UTF8 = es.utf8
endif

if LANG_PT
  PT_DIR = pt
endif

if LANG_PT_UTF8
  PT_DIR_UTF8 = pt.utf8
endif

SUBDIRS = $(ES_DIR) $(ES_DIR_UTF8) $(PT_DIR) $(PT_DIR_UTF8)

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = Introduction.texi Help.texi Command.texi Operators.texi \
  Expressions.texi Simplification.texi Plotting.texi Input.texi \
  Floating.texi Contexts.texi Polynomials.texi Constants.texi \
  Logarithms.texi Trigonometric.texi Special.texi \
  Limits.texi Differentiation.texi Integration.texi Equations.texi \
  Differential.texi Numerical.texi \
  Statistics.texi Arrays.texi Matrices.texi Affine.texi Itensor.texi \
  Ctensor.texi Atensor.texi Series.texi Number.texi Symmetries.texi \
  Groups.texi Runtime.texi Miscellaneous.texi Rules.texi Lists.texi \
  Function.texi Program.texi Debugging.texi Indices.texi Elliptic.texi \
  Bugs.texi nset.texi zeilberger.texi orthopoly.texi linearalgebra.texi \
  solve_rec.texi stringproc.texi numericalio.texi descriptive.texi \
  distrib.texi simplex.texi simplifications.texi unit.texi makeOrders.texi \
  mnewton.texi lsquares.texi plotdf.texi lindstedt.texi impdiff.texi \
  ggf.texi f90.texi eval_string.texi diag.texi cholesky.texi dynamics.texi\
  bode.texi augmented_lagrangian.texi opsubst.texi stats.texi stirling.texi interpol.texi

figurefiles = \
figures/dynamics1.gif\
figures/dynamics2.gif\
figures/dynamics3.gif\
figures/dynamics4.gif\
figures/dynamics5.gif\
figures/dynamics6.gif\
figures/dynamics7.gif\
figures/dynamics8.gif\
figures/plotdf1.gif\
figures/plotdf2.gif\
figures/plotdf3.gif\
figures/plotdf4.gif\
figures/plotdf5.gif\
figures/plotdf6.gif\
figures/plotting1.gif\
figures/plotting2.gif\
figures/plotting3.gif\
figures/plotting4.gif\
figures/plotting5.gif\
figures/plotting6.gif\
figures/plotting7.gif\
figures/plotting8.gif\
figures/plotting9.gif\
figures/plotting10.gif\
figures/plotting11.gif\
figures/plotting12.gif\
figures/plotting13.gif\
figures/plotting14.gif\
figures/plotting15.gif\
figures/plotting16.gif\
figures/plotting17.gif\
figures/plotting18.gif\
figures/plotting19.gif\
figures/plotting20.gif\
figures/plotting21.gif\
figures/plotting22.gif\
figures/plotting23.gif\
figures/plotting24.gif\
figures/plotting25.gif\
figures/orthopoly1.gif

maximahtml.mk:
	perl ./texi2html -split_chapter --lang=en --output=. --css-include=./manual.css --init-file ./texi2html.init maxima.texi 
	rm -f maximahtml.mk
	ls -1 maxima*.html | sed 's/\(.*\)/MAXIMAHTML += \1/' > maximahtml.mk

MAXIMAHTML = 
include maximahtml.mk

$(MAXIMAHTML): maxima.texi $(maxima_TEXINFOS)
	perl ./texi2html -split_chapter --lang=en --output=. --css-include=./manual.css --init-file ./texi2html.init maxima.texi 
	perl ./create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)
genericdirDATA = \
$(MAXIMAHTML)\
$(figurefiles)\
contents.hhc index.hhk header.hhp maxima.hhp

clean-local: clean-info
clean-local: clean-html

clean-info:
	rm -f maxima.info*

clean-html:
	rm -f maxima*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = texi2html maximahtml.mk manual.css texi2html.init $(genericdirDATA) create_index header.hhp maxima.hhp
