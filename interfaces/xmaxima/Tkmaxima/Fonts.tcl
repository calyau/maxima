# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Fonts.tcl,v 1.2 2002-09-10 06:03:31 mikeclarkson Exp $
#

# set font {Courier 8}
global fontCourier8

global ws_openMath
set ws_openMath(fixedFont) Courier
global fixedFont
set fixedFont Courier

# Pick a default font size in pixels
set _screenheight [winfo screenheight .]
if {$_screenheight < 500} {
    # for 640x480
    set _pixel 10
    set _point 8
    # Pick a default borderwidth which smaller
    set _bd 1
} elseif {$_screenheight < 700} {
    # for 800x600
    set _pixel 12
    set _point 8
} elseif {$_screenheight < 800} {
    # for 1024x768
    set _pixel 12
    set _point 8
} elseif {$_screenheight < 1100} {
    # for 1200x1000
    set _pixel 14
    set _point 10
} else {
    set _pixel 18
    set _point 12
}

# setup defaults depending on the OS and Window Manager
# Really should do another version for mono
switch -exact -- $tcl_platform(platform) windows {

    if {$tcl_platform(osVersion) < 5} {
	set _prop_default "MS Sans Serif"
    } else {
	set _prop_default "Tahoma"
    }

    # make sure this font is installed 
    set _allowed [string tolow [font families]]
    foreach font [list $_prop_default "MS Sans Serif" Tahoma Arial System] {
	if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	    set _prop_default $font
	    break
	}
    }

    set _fixed_default {Courier New}
    # make sure this font is installed 
    foreach font [list $_fixed_default Courier System] {
	if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	    set _fixed_default $font
	    break
	}
    }
    set fontCourier8 [list $_fixed_default $_point]

    # FIXME: nuke this
    global fontSize
    set fontSize $_point

    set fixedFont [font create -family $_fixed_default -size $_point]

} default {
    set _prop_default helvetica
    # make sure this font is installed 
    set _allowed [string tolow [font families]]
    foreach font [list $_prop_default times fixed] {
	if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	    set _prop_default $font
	    break
	}
    }
    set _fixed_default courier
    # make sure this font is installed 
    foreach font [list $_fixed_default fixed] {
	if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	    set _fixed_default $font
	    break
	}
    }
    
    set fontCourier8 [list $_fixed_default -$_pixel]
    set fixedFont [font create -family $_fixed_default -size -$_pixel]

    # FIXME: nuke this
    global fontSize
    set fontSize -$_pixel
}

