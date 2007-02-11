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
Contexts.es.texi         Indices.es.texi       Number.es.texi         \
Ctensor.es.texi          Input.es.texi         Numerical.es.texi      Symmetries.es.texi \
Debugging.es.texi        Integration.es.texi   Operators.es.texi      Trigonometric.es.texi \
Differential.es.texi     Introduction.es.texi  Plotting.es.texi  \
Differentiation.es.texi  Itensor.es.texi       Polynomials.es.texi \
Elliptic.es.texi         Limits.es.texi        Program.es.texi \
bode.texi           augmented_lagrangian.texi      descriptive.texi \
diag.texi           distrib.texi                   eval_string.texi    f90.texi \
ggf.texi            impdiff.texi                   interpol.texi       lindstedt.texi \
linearalgebra.texi  lsquares.texi                  makeOrders.texi     mnewton.texi \
numericalio.texi    opsubst.texi                   orthopoly.texi      plotdf.texi \
simplex.texi        simplifications.texi           solve_rec.texi      stats.texi        stirling.texi \
stringproc.texi     unit.texi                      zeilberger.texi     dynamics.texi \
lbfgs.texi


all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=es --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/es
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)/es
include $(top_srcdir)/common-html.mk

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = maxima-index.lisp $(genericdirDATA)

include ../common-lang.mk

