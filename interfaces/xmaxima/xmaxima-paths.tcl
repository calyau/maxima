proc setMaxDir {} {
    global env ws_openMath autoconf
    if { [info exists env(MAXIMA_DIRECTORY)] } {
	set env(MAXIMA_PREFIX) $env(MAXIMA_DIRECTORY)
    }
    if { [info exists env(MAXIMA_PREFIX)] } {
	set ws_openMath(maxima_prefix) $env(MAXIMA_PREFIX)
    } else {
	set ws_openMath(maxima_prefix) $autoconf(prefix)
    }
    if { [info exists env(MAXIMA_DATADIR)] } {
	set ws_openMath(maxima_datadir) $env(MAXIMA_DATADIR)
    } else {
	if { [info exists env(MAXIMA_PREFIX)] } {
	    set ws_openMath(maxima_datadir) \
		[file join "$env(MAXIMA_PREFIX)" share]
	} else {
	    set ws_openMath(maxima_datadir) $autoconf(datadir)
	}
    }
    if { [info exists env(MAXIMA_VERPKGLIBDIR)] } {
	set ws_openMath(maxima_verpkglibdir) $env(MAXIMA_VERPKGLIBDIR)
    } else {
	if { [info exists env(MAXIMA_PREFIX)] } {
	    set ws_openMath(maxima_verpkglibdir) \
		[file join "$env(MAXIMA_PREFIX)" lib $autoconf(package) \
		     $autoconf(version)]
	} else {
	    set ws_openMath(maxima_verpkglibdir) \
		[file join $autoconf(libdir) $autoconf(package) \
		     $autoconf(version)]
	}
    }
    set ws_openMath(maxima_verpkgdatadir) \
	[file join "$ws_openMath(maxima_datadir)" $autoconf(package) \
	     $autoconf(version)]
    if { [info exists env(MAXIMA_XMAXIMADIR)] } {
	set ws_openMath(maxima_xmaximadir) $env(MAXIMA_XMAXIMADIR)
    } else {
	set ws_openMath(maxima_xmaximadir) \
	    [file join "$ws_openMath(maxima_verpkgdatadir)" xmaxima]
    }
    if { [info exists env(XMAXIMA_MAXIMA)] } {
	set ws_openMath(xmaxima_maxima) $env(XMAXIMA_MAXIMA) 
    } else {
	set ws_openMath(xmaxima_maxima) maxima
    }
}

setMaxDir
