# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Makefile.am,v 1.20 2002-09-06 03:27:58 mikeclarkson Exp $
#

include $(top_srcdir)/common.mk
xmaxima_DATA=demos.html examples.html intro.html plotmaxima.html maxima-new.png maxima-new.gif server.lisp

bin_SCRIPTS = xmaxima

TKSOURCES = \
	Tkmaxima/Header.tcl \
	Tkmaxima/Constants.tcl \
	Tkmaxima/Readdata.tcl \
	Tkmaxima/Getdata1.tcl \
	Tkmaxima/Macros.tcl \
	Tkmaxima/Proxy.tcl \
	Tkmaxima/Send-some.tcl \
	Tkmaxima/Plotting.tcl \
	Tkmaxima/Private.tcl \
	Tkmaxima/Getopt.tcl \
	Tkmaxima/Parse.tcl \
	Tkmaxima/Textinsert.tcl \
	Tkmaxima/Printops.tcl \
	Tkmaxima/Push.tcl \
	Tkmaxima/Plotconf.tcl \
	Tkmaxima/Adams.tcl \
	Tkmaxima/Rk.tcl \
	Tkmaxima/Plotdf.tcl \
	Tkmaxima/Plot2d.tcl \
	Tkmaxima/Matrix.tcl \
	Tkmaxima/Plot3d.tcl \
	Tkmaxima/NPlot3d.tcl \
	Tkmaxima/EOctave.tcl \
	Tkmaxima/EOpenplot.tcl \
	Tkmaxima/EMaxima.tcl \
	Tkmaxima/EHref.tcl \
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

TAGS:
	echo x*.tcl Tkmaxima/*.tcl | xargs $(TCLSH) ./Utils/etags.tcl

