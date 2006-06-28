# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-trailer.tcl,v 1.11 2006-06-28 12:15:27 villate Exp $
#
# Attach this at the bottom of the xmaxima code to start up the interface.

if {[catch {package require Img}]} {
    set maxima_priv(imgregexp) {[.](gif)[^/]*$}
} else {
    set maxima_priv(imgregexp) {[.](gif|png|jpe?g)[^/]*$}
}

setMaxDir
cMAXINITBeforeIni
cMAXINITReadIni
cMAXINITAfterIni 

if { [llength $maxima_priv(plotfile)] > 0 } {
    set fptr [open [lindex $maxima_priv(plotfile) 0] r]
    regsub -all {[\n\r]} [read $fptr] " " inputdata
    close $fptr
    eval $inputdata
} else {
    MAXTkmaxima tkmaxima
    rename exit tkexit
    proc exit {{val "0"}} {tkmaxima exit "" $val}
    tkmaxima install
}
