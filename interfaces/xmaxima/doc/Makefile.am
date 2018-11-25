include $(top_srcdir)/common.mk

TEXINFO_TEX=$(top_srcdir)/doc/info/texinfo.tex

SUBDIRS = figures

info_TEXINFOS = xmaxima.texi

all-local: xmaxima.html

xmaxima.html: xmaxima.texi $(top_srcdir)/doc/info/texi2html
	perl $(top_srcdir)/doc/info/texi2html -split_chapter --lang=en --output=. --css-include=$(top_srcdir)/doc/info/manual.css --init-file  $(top_srcdir)/doc/info/texi2html.init  $<

htmlname = xmaxima
htmlinstdir = $(xmaximadir)/html
include $(top_srcdir)/common-html.mk


CLEANFILES = xmaxima_toc.html xmaxima.html

clean-local:
	rm -f xmaxima.info*
	rm -f xmaxima*.html

