# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Makefile.am,v 1.10 2002-09-05 02:54:30 mikeclarkson Exp $
#

include $(top_srcdir)/common.mk
xmaxima_DATA=demos.html examples.html intro.html plotmaxima.html maxima-new.png maxima-new.gif server.lisp

bin_SCRIPTS = xmaxima

TKSOURCES = \
	Tkmaxima/Constants.tcl \
	xmaxima.tcl 

XMAXIMA_REAL_SOURCES = $(TKSOURCES) xmaxima-paths.tcl xmaxima-trailer.tcl
XMAXIMASOURCES = $(TKSOURCES) autoconf-variables.tcl xmaxima-paths.tcl xmaxima-trailer.tcl

xmaxima: $(XMAXIMASOURCES)
	rm -f xmaxima && \
		cat $(XMAXIMASOURCES) > xmaxima && \
		chmod +x xmaxima

EXTRA_DIST = $(xmaxima_DATA) $(XMAXIMA_REAL_SOURCES)


# Developer convenience target
# ETAGS for use with Emacs TAGS, epecially for tags-replace

TCLSH = tclsh

TAGS::
	echo x*.tcl Tkmaxima/*.tcl | xargs $(TCLSH) ./Utils/etags.tcl

