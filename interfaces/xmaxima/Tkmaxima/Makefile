# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Makefile,v 1.9 2002-09-19 16:13:49 mikeclarkson Exp $
#
# Developer's convenience targets (not part of the normal make/install).
#
# unused: will try to find procedures that are defined but never used.
# It's not foolproof, but it helps spot dead code.

ALLFILES = \
	Header.tcl \
	Cygwin.tcl \
	Constants.tcl \
	Preamble.tcl \
	Readdata.tcl \
	Getdata1.tcl \
	Macros.tcl \
	Proxy.tcl \
	Send-some.tcl \
	Plotting.tcl \
	Fonts.tcl \
	Private.tcl \
	Getopt.tcl \
	Parse.tcl \
	Textinsert.tcl \
	Printops.tcl \
	Push.tcl \
	Plotconf.tcl \
	Adams.tcl \
	Rk.tcl \
	Plotdf.tcl \
	Plot2d.tcl \
	Matrix.tcl \
	Plot3d.tcl \
	NPlot3d.tcl \
	EOctave.tcl \
	EOpenplot.tcl \
	EMaxima.tcl \
	EHref.tcl \
	Browser.tcl \
	Bindings.tcl \
	Wmenu.tcl \
	Tryftp2.tcl \
	Myhtml.tcl \
	Myhtml1.tcl \
	Base64.tcl \
	Bitmaps.tcl \
	Tryembed.tcl \
	OpenMath.tcl \
	NConsole.tcl \
	String.tcl \
	CMMenu.tcl \
	Prefs.tcl \
	RunMaxima.tcl \
	Menu.tcl \
	Gui.tcl \
	Paths.tcl  \
	../xmaxima.tcl 


# junk or unused files: Startup.tcl maxima-local.tcl

procs.lis::	 $(ALLFILES)
	grep ^proc $(ALLFILES) | cut -f2 -d' ' | sort | uniq > procs.lis

unused::	procs.lis
	for proc in `cat procs.lis` ; do \
		grep -v ^proc $(ALLFILES) | grep -v "$$proc.:" | \
			grep $$proc > /dev/null \
			|| echo $$proc UNUSED ; \
		done 

# Developer convenience target
# ETAGS for use with Emacs TAGS, epecially for tags-replace

TCLSH = tclsh

TAGS:	 $(ALLFILES) Tkmaxima.tcl
	echo $(ALLFILES) Tkmaxima.tcl | xargs $(TCLSH) ../Utils/etags.tcl

tclIndex:: $(ALLFILES)
	echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(ALLFILES)" | $(TCLSH)

