# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima-paths.tcl,v 1.6 2002-09-08 01:45:23 mikeclarkson Exp $
#
# Attach this near the bottom of the xmaxima code to find the paths needed
# to start up the interface.

proc setMaxDir {} {
    global env ws_openMath autoconf

    #mike Could someone document all of these environment variables?
    # autoconf(prefix) does not seem to me to be the equivalent of
    # $env(MAXIMA_DIRECTORY) so I don't understand the next statement
    if { [info exists env(MAXIMA_DIRECTORY)] } {
	set env(MAXIMA_PREFIX) $env(MAXIMA_DIRECTORY)
    }

    #mike Is it correct to assume that autoconf exists and is valid
    # for binary windows distributions? I think it would be better
    # to make (MAXIMA_DIRECTORY) take precedence, and work off 
    # [info nameofexe] if necessary.

    if { [info exists env(MAXIMA_PREFIX)] } {
	set ws_openMath(maxima_prefix) $env(MAXIMA_PREFIX)
    } else {
	set ws_openMath(maxima_prefix) $autoconf(prefix)
    }

    if { [info exists env(MAXIMA_DATADIR)] } {
	set maxima_datadir $env(MAXIMA_DATADIR)
    } elseif { [info exists env(MAXIMA_PREFIX)] } {
	set maxima_datadir \
	    [file join $env(MAXIMA_PREFIX) share]
    } else {
	set maxima_datadir $autoconf(datadir)
    }
    # maxima_datadir is unused outside of this proc

    if {![file isdir $maxima_datadir]} {
	tide_notify [M "Maxima data directory not found in '%s'" \
			 [file native  $maxima_datadir]]
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
	[file join $maxima_datadir $autoconf(package) \
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

    # used in Menu.tcl CMMenu.tcl
    if {[file isdir [set dir [file join  $ws_openMath(maxima_verpkgdatadir) doc]]]} {
	# 5.9 and up
	set ws_openMath(pReferenceToc) \
	    [file join $dir html maxima_toc.html]
    } elseif {[file isdir [set dir [file join  $ws_openMath(maxima_verpkgdatadir) info]]]} {
	# 5.6 and down
	set ws_openMath(pReferenceToc) \
	    [file join $dir html maxima_toc.html]
    } else {
	tide_notify [M "Documentation not found in '%s'" \
			 [file native  $ws_openMath(maxima_verpkgdatadir)]]
    }

    # used in Menu.tcl CMMenu.tcl
    if {[file isdir [set dir [file join  $ws_openMath(maxima_verpkgdatadir) tests]]]} {
	# 5.9 and up
	set ws_openMath(pTestsDir) $dir
    } elseif {[file isdir [set dir [file join  $ws_openMath(maxima_verpkgdatadir) doc]]]} {
	# 5.6 and down
	set ws_openMath(pTestsDir) $dir
    } else {
	# who cares
    }


}

# setMaxDir must be called here as it is used by xmaxima-trailer.tcl
setMaxDir
