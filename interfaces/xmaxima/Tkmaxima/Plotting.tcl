# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Plotting.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### plotting.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

# set font {Courier 8}
global fontCourier8
set fontCourier8 "-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*"

global axisGray
if { "[winfo screenvisual .]" == "staticgray" } {
    set axisGray black
} else  {
    set axisGray gray60
}

global writefile
set writefile  "Save"
# make printing be by ftp'ing a file..

if {[catch { set doExit }] } { set doExit ""}

global fontSize
set width_ [winfo screenwidth .]
if { $width_ >= 1280 } { 
    set fontSize 12
} elseif { $width_ <= 640} { 
    set fontSize 8 
} else {
    set fontSize 10
}
unset width_

## endsource plotting.tcl
