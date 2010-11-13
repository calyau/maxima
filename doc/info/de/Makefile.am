MAKEINFOFLAGS = --enable-encoding --force --no-validate

langsdir = /de

fcharset = UTF-8

info_TEXINFOS = maxima.texi
maxima_TEXINFOS = include-maxima.de.texi category-macros.de.texi \
Introduction.de.texi Bugs.de.texi Help.de.texi Command.de.texi \
DataTypes.de.texi Expressions.de.texi Operators.de.texi Evaluation.de.texi \
Simplification.de.texi MathFunctions.de.texi Database.de.texi Plotting.de.texi \
Input.de.texi Series.de.texi Calculus.de.texi Polynomials.de.texi \
Special.de.texi \
Glossary.de.texi


all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info ../build_index.pl
	perl ../build_index.pl maxima.info ':crlf' > maxima-index.lisp

maxima.html: maxima.texi $(maxima_TEXINFOS)
	sh ../extract_categories.sh maxima
	perl ../texi2html -split_chapter --lang=de --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

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

