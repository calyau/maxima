include $(top_srcdir)/common.mk
xmaxima_DATA=demos.html examples.html intro.html plotmaxima.html maxima.png maxima.gif maxima-new.png maxima-new.gif server.lisp

verpkglibexec_SCRIPTS = omplotdata
bin_SCRIPTS = xmaxima

XMAXIMASOURCES = xmaxima.tcl autoconf-variables.tcl xmaxima-paths.tcl xmaxima-trailer.tcl

xmaxima: $(XMAXIMASOURCES)
	rm -f xmaxima && \
	cat $(XMAXIMASOURCES) > xmaxima && \
	chmod +x xmaxima