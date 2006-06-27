# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.19 2006-06-27 13:50:59 villate Exp $
#

proc cMAXINITBeforeIni {} {
    global maxima_default
    set maxima_default(plotwindow) multiple

    # from Send-some.tcl
    set maxima_default(sMathServerHost) genie1.ma.utexas.edu
    set maxima_default(iMathServerPort) 4443

    # from Browser.tcl
    set maxima_default(sMathServerHost) localhost
    set maxima_default(iMathServerPort) 4443

    #mike turn these off by default
    set maxima_default(iShowBalloons) 0

    set maxima_default(fontAdjust) 0

    set maxima_default(iConsoleWidth) 80
    set maxima_default(iConsoleHeight) 24

    set maxima_default(iLocalPort) 4008

    set maxima_default(bDebugParse) 0

    # From Browser.tcl
    set maxima_default(defaultservers) {
	nmtp://genie1.ma.utexas.edu/
	nmtp://linux51.ma.utexas.edu/
	nmtp://linux52.ma.utexas.edu/
    }

    global embed_args
    if { "[info var embed_args]" != "" } {
	# the following will be defined only in the plugin
	set maxima_default(defaultservers) nmtp://genie1.ma.utexas.edu/
    }


    # maxima_default(lProxyHttp)
}

proc cMAXINITReadIni {} {
    if {[file isfile ~/xmaxima.ini]} {
	if {[catch {uplevel "#0" [list source ~/xmaxima.ini] } err]} {
	    tide_failure [M [mc "Error sourcing %s\n%s"] \
			      [file native ~/xmaxima.ini] \
			      $err]
	}
    }
}

proc cMAXINITAfterIni {} {
    global maxima_default maxima_priv MathServer
    lMaxInitSetOpts
    set MathServer [list $maxima_default(sMathServerHost) \
			$maxima_default(iMathServerPort) ]

    # from plot3d.tcl
    set maxima_priv(speed) [expr {(9700.0 / (1 + [lindex [time {set i 0 ; while { [incr i] < 1000} {}} 1] 0]))}]

    # from Wmenu.tcl
    global show_balloons
    set show_balloons $maxima_default(iShowBalloons)

    # From Browser.tcl
    global debugParse
    set debugParse $maxima_default(bDebugParse)

    if {[info exists maxima_default(lProxyHttp)] && \
	    [llength $maxima_default(lProxyHttp)] == "2"} {
	#mike FIXME: make this a _default
	set maxima_priv(proxy,http) $maxima_default(lProxyHttp)
    }

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
set maxima_priv(options,maxima) {{doinsert 0 "Do an insertion" boolean}}

# from EOctave.tcl
set maxima_priv(options,octave) {{doinsert 1 "Do an insertion" boolean}}

# from EOpenplot.tcl
set maxima_priv(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# from EHref.tcl
set maxima_priv(options,href) {
    {src "" [mc "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"]}
    {search "" [mc "A string to search for, to get an initial position"]}
    {searchregexp "" [mc "A regexp to search for, to get an initial position"]}
}

# from Preamle.tcl
set maxima_priv(counter) 0
	
# the linelength should be long enough to display formatted mathematical
# output from things like maxima, without adjustment, and to allow
# for a margin.
set maxima_priv(linelength) 90

# From Browser.tcl
set maxima_priv(sticky) "^Teval$|^program:"
set maxima_priv(richTextCommands) {Tins TinsSlashEnd}
set maxima_priv(urlHandlers) {
    text/html  netmath
    text/plain netmath
    image/gif  netmath
    application/postscript "ghostview -safer %s"
    application/pdf "acroread %s"
    application/x-dvi "xdvi %s"
}
set maxima_priv(imagecounter) 0

set maxima_priv(brokenimage,data) R0lGODlhHQAgAMIAAAAAAP9jMcbGxoSEhP///zExY/9jzgCEACH5BAEAAAIALAAAAAAdACAAAAPOOLrcLjDCQaq9+CoZaf7YIIicx50nNZYV6k4tCRPuYduSR8vmef+dy2rU4vyOM8uqJzkCBYCoNEqkGZ04SGHLBSiKTewhx/AyI+LxqWIGh5Eo9pdm8D3jhDa9/nrJTQaBfS5/LYGCgxyFe4cnAY+Qj1oFegKHjRKRkpMbgJeIEJqTBTyGnxybAlwbQYygKFusOaavo5SkJ5WYErELKAO6fBy4LxS6vFzEv4snpLIpIszIMiWKeXMWvS7RGXoVsX0g11NR1Bzk6F4jCn0ODgkAOwAA





global evalPrograms
set evalPrograms {  gp gap gb }
#set maxima_priv(options,maxima) {{doinsert 1 "Do an insertion" boolean}}
#set maxima_priv(options,gp) {{doinsert 1 "Do an insertion" boolean}}
#set maxima_priv(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

