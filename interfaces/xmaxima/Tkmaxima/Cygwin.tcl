# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-

# Voodo for CYGWIN
# Is there a canonical way of telling we are under CYGWIN?
global env tcl_platform maxima_priv
if {$tcl_platform(platform) == "windows" && \
	[info exists env(PATH)] && $env(PATH) != "" && \
	[string match {*/usr/bin*} $env(PATH)] && \
	[string match {*:*} $env(PATH)] && \
	![string match {*;*} $env(PATH)]} {
    # CYGWIN uses Unix PATH but Tcl considers it Windows
    set env(PATH) [join [split $env(PATH) ":"] ";"]
    set maxima_priv(platform) cygwin
} else {
    set maxima_priv(platform) $tcl_platform(platform)
}
