# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.7 2002-09-06 06:21:36 mikeclarkson Exp $
#

global ws_openMath
set ws_openMath(date) 04/28/2002

# from 
if { ![info exists ws_openMath(date)] } {
    set ws_openMath(date) [clock  format [clock seconds] -format {%m/%d/%Y} ]
}

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

# from Plotconf.tcl
global ftpInfo
set ftpInfo(host) genie1.ma.utexas.edu
set ftpInfo(viahost) genie1.ma.utexas.edu

# from Plot2d.tcl
array set ws_openMath { bitmap,disc4 {#define disc4_width 4
#define disc4_height 4
static unsigned char disc4_bits[] = {
    0x06, 0x0f, 0x0f, 0x06};}
    bitmap,disc6 {#define disc_width 6
#define disc_height 6
static unsigned char disc_bits[] = {
    0xde, 0xff, 0xff, 0xff, 0xff, 0xde};}
}

# from EOctave.tcl
set ws_openMath(options,octave) {{doinsert 1 "Do an insertion" boolean}}

# from EOpenplot.tcl
set ws_openMath(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# from EHref.tcl
set ws_openMath(options,href) {
    {src "" "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"}
    {search "" "A string to search for, to get an initial position"}
    {searchregexp "" "A regexp to search for, to get an initial position"}
}

# from Wmenu.tcl
global show_balloons
#mike turn these off by default
set show_balloons 0

