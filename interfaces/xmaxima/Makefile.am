# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Makefile.am,v 1.25 2002-09-07 10:31:22 mikeclarkson Exp $
#

include $(top_srcdir)/common.mk
xmaxima_DATA=demos.html examples.html intro.html plotmaxima.html maxima-new.png maxima-new.gif server.lisp

bin_SCRIPTS = xmaxima

TKSOURCES = \
	Tkmaxima/Header.tcl \
	Utils/FileDlg.tcl \
	Utils/Messages.tcl \
	Utils/Misc.tcl \
	Tkmaxima/Constants.tcl \
	Tkmaxima/Preamble.tcl \
	Tkmaxima/Readdata.tcl \
	Tkmaxima/Getdata1.tcl \
	Tkmaxima/Macros.tcl \
	Tkmaxima/Proxy.tcl \
	Tkmaxima/Send-some.tcl \
	Tkmaxima/Plotting.tcl \
	Tkmaxima/Fonts.tcl \
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
	Tkmaxima/Browser.tcl \
	Tkmaxima/Wmenu.tcl \
	Tkmaxima/Tryftp2.tcl \
	Tkmaxima/Myhtml.tcl \
	Tkmaxima/Myhtml1.tcl \
	Tkmaxima/Base64.tcl \
	Tkmaxima/Bitmaps.tcl \
	Tkmaxima/Tryembed.tcl \
	Tkmaxima/OpenMath.tcl \
	Tkmaxima/NConsole.tcl \
	Tkmaxima/String.tcl \
	Tkmaxima/CMMenu.tcl \
	Tkmaxima/Prefs.tcl \
	Tkmaxima/RunMaxima.tcl \
	Tkmaxima/Menu.tcl \
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

tclIndex:: $(TKSOURCES)
	echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(TKSOURCES)" | $(TCLSH)

