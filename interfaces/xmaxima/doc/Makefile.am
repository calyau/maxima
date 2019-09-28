include $(top_srcdir)/common.mk

TEXINFO_TEX=$(top_srcdir)/doc/info/texinfo.tex

SUBDIRS = figures

info_TEXINFOS = xmaxima.texi

all-local: xmaxima.html

xmaxima.html : xmaxima.texi $(top_builddir)/doc/info/texi2html.init $(top_srcdir)/doc/info/manual.css
	$(MAKEINFO) --html -c TEXI2HTML=1 --split=chapter --document-lang=en --output=. --css-include=$(top_srcdir)/doc/info/manual.css --init-file  $(top_builddir)/doc/info/texi2html.init  $<

htmlname = xmaxima
htmlinstdir = $(xmaximadir)/html
include $(top_srcdir)/common-html.mk


CLEANFILES = xmaxima_toc.html xmaxima.html

clean-local:
	rm -f xmaxima.info*
	rm -f xmaxima*.html

