include $(top_srcdir)/common.mk
xmaxima_DATA=demos.html examples.html intro.html plotmaxima.html maxima-new.png maxima-new.gif server.lisp

bin_SCRIPTS = xmaxima

XMAXIMA_REAL_SOURCES = xmaxima.tcl xmaxima-paths.tcl xmaxima-trailer.tcl
XMAXIMASOURCES = $(XMAXIMA_REAL_SOURCES) autoconf-variables.tcl

xmaxima: $(XMAXIMASOURCES)
	rm -f xmaxima && \
	cat $(XMAXIMASOURCES) > xmaxima && \
	chmod +x xmaxima

EXTRA_DIST = $(xmaxima_DATA) $(XMAXIMA_REAL_SOURCES)
