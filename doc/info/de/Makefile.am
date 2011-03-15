MAKEINFOFLAGS = --enable-encoding

langsdir = /de

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = include-maxima.de.texi category-macros.de.texi \
Introduction.de.texi Bugs.de.texi Help.de.texi Command.de.texi \
DataTypes.de.texi Constants.de.texi Lists.de.texi Arrays.de.texi \
Expressions.de.texi Operators.de.texi Evaluation.de.texi \
Simplification.de.texi MathFunctions.de.texi Database.de.texi Plotting.de.texi \
Input.de.texi Nset.de.texi Series.de.texi Calculus.de.texi \
Polynomials.de.texi Equations.de.texi Matrices.de.texi Itensor.de.texi \
Ctensor.de.texi Atensor.de.texi Special.de.texi Numerical.de.texi \
Runtime.de.texi Rules.de.texi Function.de.texi Program.de.texi \
Translator.de.texi Debugging.de.texi Miscellaneous.de.texi \
abs_integrate.de.texi \
affine.de.texi asympa.de.texi augmented_lagrangian.de.texi bode.de.texi \
cobyla.de.texi contrib_ode.de.texi descriptive.de.texi diag.de.texi \
distrib.de.texi draw.de.texi drawdf.de.texi dynamics.de.texi \
ezunits.de.texi f90.de.texi ggf.de.texi graphs.de.texi grobner.de.texi \
groups.de.texi impdiff.de.texi interpol.de.texi lapack.de.texi lbfgs.de.texi \
lindstedt.de.texi linearalgebra.de.texi lsquares.de.texi makeOrders.de.texi \
minpack.de.texi mnewton.de.texi numericalio.de.texi opsubst.de.texi \
orthopoly.de.texi plotdf.de.texi romberg.de.texi simplex.de.texi \
simplifications.de.texi solve_rec.de.texi stats.de.texi stirling.de.texi \
stringproc.de.texi symmetries.de.texi unit.de.texi zeilberger.de.texi \
Glossary.de.texi


all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
#	sh ../extract_categories.sh maxima
	perl texi2html -split_chapter --lang=de --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/de
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)/de
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
