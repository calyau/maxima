# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.11 2002-09-10 06:03:31 mikeclarkson Exp $
#

global xHMpreferences
set xHMpreferences(plotwindow) embedded

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

# from xmaxima.tcl
set ws_openMath(options,maxima) {{doinsert 0 [M "Do an insertion"] boolean}}

# from EOctave.tcl
set ws_openMath(options,octave) {{doinsert 1 [M "Do an insertion"] boolean}}

# from EOpenplot.tcl
set ws_openMath(options,openplot) {{doinsert 0 [M "Do an insertion"] boolean}}

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

# from preamle.tcl
set ws_openMath(counter) 0
	
global ws_openMath
# the linelength should be long enough to display formatted mathematical
# output from things like maxima, without adjustment, and to allow
# for a margin.
set ws_openMath(linelength) 90

global evalPrograms
set evalPrograms {  gp gap gb }
#set ws_openMath(options,maxima) {{doinsert 1 [M "Do an insertion"] boolean}}
#set ws_openMath(options,gp) {{doinsert 1 [M "Do an insertion"] boolean}}
# set ws_openMath(options,openplot) {{doinsert 0 [M "Do an insertion"] boolean}}

global NCtextHelp
set NCtextHelp "
	    Bindings:
	    <Return>   This sends the current expression (ie where the insert
	               cursor is)  for evaluation.
	    <Linefeed> (Control-j) This inserts a newline, and is useful
	               for entering multiline input.
	    <Control-k> Kills the current line and puts it in kill ring.
	                Successive control-k's append their output together.
	    <Control-y> Yank out the last kill, Meta-y cycles thru previous
	                kills.
	    <Control-c><Control-c> Interrupt the current computation.
	    <Alt-p>   Previous input, or if repeated cycle through the previous
	               inputs.  If the current input is not empty, then
	                match only inputs which begin with the current input.
	    <Alt-n>   Like Previous input, but in opposite direction.
	"

global xmaximaPreferences
array set xmaximaPreferences {fontAdjust 0 }


