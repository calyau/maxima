# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Prefs.tcl,v 1.3 2002-09-10 06:03:31 mikeclarkson Exp $
#
proc resetMaximaFont { w } {
    global xmaximaPreferences
    $w config -font [xHMmapFont font:fixed:normal:r:[expr $xmaximaPreferences(fontAdjust) + 3]]
}




