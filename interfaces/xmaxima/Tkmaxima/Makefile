# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Makefile,v 1.1 2002-09-06 00:38:37 mikeclarkson Exp $
#
# Developer's convenience targets (not part of the normal make/install).
#
# unused: will try to find procedures that are defined but never used.
# It's not foolproof, but it helps spot dead code.

ALLFILES = \
	Constants.tcl \
	Getdata1.tcl \
	Header.tcl \
	Macros.tcl \
	Proxy.tcl \
	Readdata.tcl

# junk or unused files: Startup.tcl maxima-local.tcl

procs.lis::	 $(ALLFILES)
	grep ^proc $(ALLFILES) | cut -f2 -d' ' | sort | uniq > procs.lis

unused::	procs.lis
	for proc in `cat procs.lis` ; do \
		grep -v ^proc $(ALLFILES) ../*.tcl | grep -v "$$proc.:" | \
			grep $$proc > /dev/null \
			|| echo $$proc UNUSED ; \
		done 




