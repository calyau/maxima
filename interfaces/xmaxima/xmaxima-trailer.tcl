# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-trailer.tcl,v 1.9 2002-09-19 16:13:48 mikeclarkson Exp $
#
# Attach this at the bottom of the xmaxima code to start up the interface.

MAXTkmaxima tkmaxima
rename exit tkexit
proc exit {{val "0"}} {tkmaxima exit "" $val}
tkmaxima install
