# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-trailer.tcl,v 1.4 2002-09-10 06:01:57 mikeclarkson Exp $
#
# Attach this at the bottom of the xmaxima code to start up the interface.

global tcl_platform ws_openMath
if {$tcl_platform(platform) == "windows" } {

    set dir [file dir $ws_openMath(xmaxima_maxima)]
    # These should be in the same directory as the xmaxima.exe
    set ws_openMath(kill) [file join $dir winkill.exe]
    set file [file join $dir tclwinkill.dll]
    if {[file isfile $file]} {
	catch {load  $file}
    }
    unset file
} else {
    # unix
    set ws_openMath(kill) kill
}

doit .maxima
