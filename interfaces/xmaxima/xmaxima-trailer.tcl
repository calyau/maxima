# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-trailer.tcl,v 1.3 2002-09-05 08:28:06 mikeclarkson Exp $
#
# Attach this at the bottom of the xmaxima code to start up the interface.

global tcl_platform ws_openMath
if {$tcl_platform(platform) == "windows" } {

    set ws_openMath(kill) [file join $ws_openMath(maxima_prefix) \
			       bin winkill.exe]
    set file [file join $ws_openMath(maxima_prefix) bin tclwinkill.dll]
    if {[file isfile $file]} {
	catch {load  $file}
    }
    unset file
} else {
    # unix
    set ws_openMath(kill) kill
}

doit .maxima
