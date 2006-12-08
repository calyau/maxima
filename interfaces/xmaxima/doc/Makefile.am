include $(top_srcdir)/common.mk

SUBDIRS = figures

info_TEXINFOS = xmaxima.texi

XMAXIMAHTML  = xmaxima.html xmaxima_1.html xmaxima_2.html xmaxima_3.html \
               xmaxima_4.html xmaxima_5.html xmaxima_6.html xmaxima_7.html \
               xmaxima_8.html xmaxima_abt.html xmaxima_toc.html 

htmldir      = $(xmaximadir)/html
html_DATA    = $(XMAXIMAHTML)

$(XMAXIMAHTML): xmaxima.texi $(top_srcdir)/doc/info/texi2html
	perl $(top_srcdir)/doc/info/texi2html -split_chapter --lang=en --output=. --css-include=$(top_srcdir)/doc/info/manual.css --init-file  $(top_srcdir)/doc/info/texi2html.init  xmaxima.texi 

clean-local:
	rm -f xmaxima.info*
	rm -f xmaxima*.html

