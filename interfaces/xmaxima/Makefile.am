# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Makefile.am,v 1.39 2004-10-13 12:08:57 vvzhy Exp $
#

include $(top_srcdir)/common.mk

SUBDIRS = msgs

genericdir = $(xmaximadir)

genericdirDATA=demos.html examples.html intro.html plotmaxima.html maxima-new.png maxima-new.gif maxima-icon.ico maxima-icon.png

bin_SCRIPTS = xmaxima

TKSOURCES = \
	Tkmaxima/Cygwin.tcl \
	Utils/FileDlg.tcl \
	Utils/Messages.tcl \
	Utils/Misc.tcl \
	ObjTcl/Object.tcl \
	ObjTcl/Feedback.tcl \
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
	Tkmaxima/Bindings.tcl \
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
	Tkmaxima/Prefs.tcl \
	Tkmaxima/RunMaxima.tcl \
	Tkmaxima/Menu.tcl \
	Tkmaxima/Paths.tcl \
	Tkmaxima/Gui.tcl \
	Tkmaxima/Tkmaxima.tcl 


XMAXIMA_REAL_SOURCES = $(TKSOURCES) xmaxima-trailer.tcl
XMAXIMASOURCES = Tkmaxima/Header.tcl $(TKSOURCES) autoconf-variables.tcl xmaxima-trailer.tcl Tkmaxima/Header.tcl


xmaxima: $(XMAXIMASOURCES) Tkmaxima/tclIndex
	rm -f xmaxima && \
		cat $(XMAXIMASOURCES) > xmaxima && \
		chmod +x xmaxima

WIN32_FILES = win32/ChangeLog win32/Makefile.in win32/tclwinkill.c  \
  win32/winkill.c win32/InfoBefore.txt win32/readme.txt
EXTRA_DIST = $(genericdirDATA) $(XMAXIMA_REAL_SOURCES) $(WIN32_FILES) Tkmaxima/Header.tcl.in maxima-icon.png


# Developer convenience target
# ETAGS for use with Emacs TAGS, epecially for tags-replace

TCLSH = tclsh

TAGS:
	echo x*.tcl Tkmaxima/*.tcl | xargs $(TCLSH) ./Utils/etags.tcl

tclIndex:: $(TKSOURCES)
	echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(TKSOURCES)" | $(TCLSH)

# Create Tkmaxima/tclIndex if tclsh is available. Otherwise, create empty file
# to satisfy make.
Tkmaxima/tclIndex: $(TKSOURCES)
	$(TCLSH) < /dev/null > /dev/null 2>&1 && \
          (cd Tkmaxima ; \
           echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(ALLFILES)" | \
           $(TCLSH)) || \
         touch Tkmaxima/tclIndex

FORCE:
