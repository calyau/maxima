# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Fonts.tcl,v 1.1 2002-09-07 05:38:46 mikeclarkson Exp $
#

# set font {Courier 8}
global fontCourier8
set fontCourier8 "-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*"

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

