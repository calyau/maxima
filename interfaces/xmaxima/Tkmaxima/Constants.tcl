# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.13 2002-09-13 17:34:18 mikeclarkson Exp $
#

proc cMAXINITBeforeIni {} {
    global maxima_default
    set maxima_default(plotwindow) embedded

    # from Send-some.tcl
    set maxima_default(sMathServerHost) genie1.ma.utexas.edu
    set maxima_default(iMathServerPort) 4443

    #mike turn these off by default
    set maxima_default(iShowBalloons) 0

    set maxima_default(fontAdjust) 0

    set maxima_default(iConsoleWidth) 80
    set maxima_default(iConsoleHeight) 24
    
    set maxima_default(iLocalPort) 4008
}

proc cMAXINITAfterIni {} {
    global maxima_default maxima_priv

    global MathServer
    set MathServer [list $maxima_default(sMathServerHost) \
			$maxima_default(iMathServerPort) ]
    
    # from plot3d.tcl
    set maxima_priv(speed) [expr {(9700.0 / (1 + [lindex [time {set i 0 ; while { [incr i] < 1000} {}} 1] 0]))}]

    # from Wmenu.tcl
    global show_balloons
    set show_balloons $maxima_default(iShowBalloons)

   
}

# Constants
global maxima_priv
set maxima_priv(date) 04/28/2002

# from
if { ![info exists maxima_priv(date)] } {
    set maxima_priv(date) [clock  format [clock seconds] -format {%m/%d/%Y} ]
}

# from Preamble.tcl
set maxima_priv(clicks_per_second) 1000000

# from Getdata1.tcl
set maxima_priv(cachedir) ~/.netmath/cache

# from Plotconf.tcl
global ftpInfo
set ftpInfo(host) genie1.ma.utexas.edu
set ftpInfo(viahost) genie1.ma.utexas.edu

# from Plot2d.tcl
array set maxima_priv { bitmap,disc4 {#define disc4_width 4
#define disc4_height 4
static unsigned char disc4_bits[] = {
    0x06, 0x0f, 0x0f, 0x06};}
    bitmap,disc6 {#define disc_width 6
#define disc_height 6
static unsigned char disc_bits[] = {
    0xde, 0xff, 0xff, 0xff, 0xff, 0xde};}
}

# from xmaxima.tcl
set maxima_priv(options,maxima) {{doinsert 0 [M "Do an insertion"] boolean}}

# from EOctave.tcl
set maxima_priv(options,octave) {{doinsert 1 [M "Do an insertion"] boolean}}

# from EOpenplot.tcl
set maxima_priv(options,openplot) {{doinsert 0 [M "Do an insertion"] boolean}}

# from EHref.tcl
set maxima_priv(options,href) {
    {src "" "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"}
    {search "" "A string to search for, to get an initial position"}
    {searchregexp "" "A regexp to search for, to get an initial position"}
}

# from preamle.tcl
set maxima_priv(counter) 0
	
# the linelength should be long enough to display formatted mathematical
# output from things like maxima, without adjustment, and to allow
# for a margin.
set maxima_priv(linelength) 90

global evalPrograms
set evalPrograms {  gp gap gb }
#set maxima_priv(options,maxima) {{doinsert 1 [M "Do an insertion"] boolean}}
#set maxima_priv(options,gp) {{doinsert 1 [M "Do an insertion"] boolean}}
# set maxima_priv(options,openplot) {{doinsert 0 [M "Do an insertion"] boolean}}

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



