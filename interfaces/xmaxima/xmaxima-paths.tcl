# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-paths.tcl,v 1.5 2002-09-07 23:17:58 mikeclarkson Exp $
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

    # backwards compatability - to be eliminated
    set ws_openMath(maximaPath) $ws_openMath(maxima_verpkgdatadir)

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

    # Bring derived quantities up here too so we can see the
    # side effects of setting the above variables

    # used in Menu.tcl
    set ws_openMath(pReferenceToc) \
	[file join $ws_openMath(maximaPath) info maxima_toc.html]

    # used in RunMaxima.tcl, was deined in xmaxima.tcl
    if { [auto_execok $ws_openMath(xmaxima_maxima)] != "" } {
	#mike FIXME: This should break on windows if there is a space in the pathname
	set ws_openMath(localMaximaServer) "$ws_openMath(xmaxima_maxima) $maxima_opts -p [file join $ws_openMath(maxima_xmaximadir) server.lisp] -r \":lisp (progn (user::setup PORT)(values))\" &"
    } elseif { [info exists env(XMAXIMA_MAXIMA)] } {
	tide_faulure "Error. maxima executable XMAXIMA_MAXIMA=$env(XMAXIMA_MAXIMA) not found."
	exit 1
    } else {
	# A gruesome hack. Normally, we communicate to the maxima image
	# through the maxima script, as above. If the maxima script is not
	# available, as may happen on windows, directly talk to the GCL 
	# saved image. jfa 04/28/2002
	set env(MAXIMA_INT_LISP_PRELOAD) \
	    "[file join $ws_openMath(maxima_xmaximadir) server.lisp]"
	set env(MAXIMA_INT_INPUT_STRING) \
	    ":lisp (progn (user::setup PORT)(values));"
	set ws_openMath(localMaximaServer) "[file join $ws_openMath(maxima_verpkglibdir) binary-gcl maxima] -eval \"(run)\" -f &"
    }
}

# setMaxDir must be called here as it is used by xmaxima-trailer.tcl
setMaxDir
