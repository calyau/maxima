# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Makefile,v 1.1 2002-09-07 08:48:35 mikeclarkson Exp $
#
# Developer's convenience targets (not part of the normal make/install).
#

# Developer convenience target
# TAGS for use with Emacs TAGS, epecially for tags-replace

TCLSH = tclsh

TAGS:	 $(ALLFILES)
	echo $(ALLFILES) | xargs $(TCLSH) ../Utils/etags.tcl

