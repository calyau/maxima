# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-paths.tcl,v 1.3 2002-09-06 09:08:07 mikeclarkson Exp $
#
# Attach this near the bottom of the xmaxima code to find the paths needed
# to start up the interface.

proc setMaxDir {} {
    global env ws_openMath autoconf

    if { [info exists env(MAXIMA_DIRECTORY)] } {
	set env(MAXIMA_PREFIX) $env(MAXIMA_DIRECTORY)
    }

    #mike Is it correct to assume that autoconf exists and is valid
    # for binary windows disributions? I think it would be better
    # to make (MAXIMA_DIRECTORY) take precedence, and work off 
    # [info nameofexe] if necessary.

    if { [info exists env(MAXIMA_PREFIX)] } {
	set ws_openMath(maxima_prefix) $env(MAXIMA_PREFIX)
    } else {
	set ws_openMath(maxima_prefix) $autoconf(prefix)
    }
    if { [info exists env(MAXIMA_DATADIR)] } {
	set ws_openMath(maxima_datadir) $env(MAXIMA_DATADIR)
    } elseif { [info exists env(MAXIMA_PREFIX)] } {
	set ws_openMath(maxima_datadir) \
	    [file join $env(MAXIMA_PREFIX) share]
    } else {
	set ws_openMath(maxima_datadir) $autoconf(datadir)
    }

    if { [info exists env(MAXIMA_VERPKGLIBDIR)] } {
	set ws_openMath(maxima_verpkglibdir) $env(MAXIMA_VERPKGLIBDIR)
    } elseif { [info exists env(MAXIMA_PREFIX)] } {
	set ws_openMath(maxima_verpkglibdir) \
	    [file join $env(MAXIMA_PREFIX) lib $autoconf(package) \
		 $autoconf(version)]
    } else {
	set ws_openMath(maxima_verpkglibdir) \
	    [file join $autoconf(libdir) $autoconf(package) \
		 $autoconf(version)]
    }

    set ws_openMath(maxima_verpkgdatadir) \
	[file join $ws_openMath(maxima_datadir) $autoconf(package) \
	     $autoconf(version)]

    if { [info exists env(MAXIMA_XMAXIMADIR)] } {
	set ws_openMath(maxima_xmaximadir) $env(MAXIMA_XMAXIMADIR)
    } else {
	set ws_openMath(maxima_xmaximadir) \
	    [file join $ws_openMath(maxima_verpkgdatadir) xmaxima]
    }

    if { [info exists env(XMAXIMA_MAXIMA)] } {
	set ws_openMath(xmaxima_maxima) $env(XMAXIMA_MAXIMA) 
    } else {
	set ws_openMath(xmaxima_maxima) maxima
    }
}

# setMaxDir must be called here as it is used by xmaxima-trailer.tcl
setMaxDir
