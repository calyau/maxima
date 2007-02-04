MAKEINFOFLAGS = --enable-encoding

langsdir = /pt_BR

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = \
Affine.texi \
Arrays.texi \
Atensor.texi \
augmented_lagrangian.texi \
bode.texi \
Bugs.texi \
cholesky.texi \
Command.texi \
Constants.texi \
Contexts.texi \
Ctensor.texi \
Debugging.texi \
descriptive.texi \
diag.texi \
Differential.texi \
Differentiation.texi \
distrib.texi \
dynamics.texi \
Elliptic.texi \
Equations.texi \
eval_string.texi \
Expressions.texi \
f90.texi \
Floating.texi \
Function.texi \
ggf.texi \
Groups.texi \
Help.texi \
impdiff.texi \
implicit_plot.texi \
Indices.texi \
Input.texi \
Integration.texi \
interpol.texi \
Introduction.texi \
Itensor.texi \
lbfgs.texi \
Limits.texi \
lindstedt.texi \
linearalgebra.texi \
Lists.texi \
Logarithms.texi \
lsquares.texi \
makeOrders.texi \
Matrices.texi \
Miscellaneous.texi \
mnewton.texi \
nset.texi \
Number.texi \
numericalio.texi \
Numerical.texi \
Operators.texi \
opsubst.texi \
orthopoly.texi \
plotdf.texi \
Plotting.texi \
Polynomials.texi \
Program.texi \
romberg.texi \
Rules.texi \
Runtime.texi \
Series.texi \
simplex.texi \
simplifications.texi \
Simplification.texi \
solve_rec.texi \
Special.texi \
Statistics.texi \
stats.texi \
stirling.texi \
stringproc.texi \
Symmetries.texi \
Trigonometric.texi \
unit.texi \
zeilberger.texi

all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=pt --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/pt_BR
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
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

