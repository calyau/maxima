# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.4 2002-09-06 01:10:13 mikeclarkson Exp $
#

global ws_openMath
set ws_openMath(date) 04/28/2002

# from Preamble.tcl
set ws_openMath(clicks_per_second) 1000000

# from Getdata1.tcl
set ws_openMath(cachedir) ~/.netmath/cache

# from plot3d.tcl
set ws_openMath(speed) [expr {(9700.0 / (1 + [lindex [time {set i 0 ; while { [incr i] < 1000} {}} 1] 0]))}]

# from Send-some.tcl
#mike I think this should be eliminated
global MathServer
set MathServer { genie1.ma.utexas.edu 4443 }
