# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-trailer.tcl,v 1.6 2002-09-10 09:19:14 mikeclarkson Exp $
#
# Attach this at the bottom of the xmaxima code to start up the interface.

global tcl_platform maxima_priv
if {$tcl_platform(platform) == "windows" } {

    set dir [file dir [file dir [info name]]]
    # These should be in the same directory as the xmaxima.exe
    set maxima_priv(kill) [file join $dir winkill.exe]
    set file [file join $dir tclwinkill.dll]
    if {[file isfile $file]} {
	catch {load  $file}
    }
    unset file
} else {
    # unix
    set maxima_priv(kill) kill
}

doit .maxima
