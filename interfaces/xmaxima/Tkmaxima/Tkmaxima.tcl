# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Tkmaxima.tcl,v 1.3 2004-10-13 12:08:58 vvzhy Exp $
#

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
# Source Tkmaxima/Cygwin.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Preamble.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Readdata.tcl 		;# can be autoloaded
# Source Tkmaxima/Getdata1.tcl 		;# can be autoloaded
# Source Tkmaxima/Macros.tcl 		;# can be autoloaded
# Source Tkmaxima/Proxy.tcl 		;# can be autoloaded
# Source Tkmaxima/Send-some.tcl 	;# sets global variables
# Source Tkmaxima/Plotting.tcl 		;# sets global variables
# Source Tkmaxima/Fonts.tcl 		;# sets global variables
# Source Tkmaxima/Private.tcl 		;# can be autoloaded
# Source Tkmaxima/Getopt.tcl 		;# can be autoloaded
# Source Tkmaxima/Parse.tcl 		;# sets global variables
# Source Tkmaxima/Textinsert.tcl 	;# can be autoloaded
# Source Tkmaxima/Printops.tcl 		;# can be autoloaded
# Source Tkmaxima/Push.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotconf.tcl 		;# can be autoloaded
# Source Tkmaxima/Adams.tcl 		;# can be autoloaded
# Source Tkmaxima/Rk.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotdf.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot2d.tcl 		;# defined globals
# Source Tkmaxima/Matrix.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot3d.tcl 		;# defined globals
# Source Tkmaxima/NPlot3d.tcl 		;# can be autoloaded
# Source Tkmaxima/EOctave.tcl 		;# can be autoloaded
# Source Tkmaxima/EOpenplot.tcl 	;# can be autoloaded
# Source Tkmaxima/EMaxima.tcl 		;# can be autoloaded
# Source Tkmaxima/EHref.tcl 		;# can be autoloaded
# Source Tkmaxima/Browser.tcl 		;# defines globals
# Source Tkmaxima/Bindings.tcl 		;# defines bindings
# Source Tkmaxima/Wmenu.tcl 		;# can be autoloaded
# Source Tkmaxima/Tryftp2.tcl 		;# can be autoloaded
# Source Tkmaxima/Myhtml.tcl 		;# defines globals and tags
# Source Tkmaxima/Myhtml1.tcl 		;# can be autoloaded
# Source Tkmaxima/Base64.tcl 		;# can be autoloaded
# Source Tkmaxima/Bitmaps.tcl 		;# defines globals
# Source Tkmaxima/Tryembed.tcl 		;# defines globals?
# Source Tkmaxima/OpenMath.tcl 		;# active
# Source Tkmaxima/NConsole.tcl 		;# can be autoloaded
# Source Tkmaxima/String.tcl 		;# can be autoloaded
# Source Tkmaxima/Prefs.tcl 		;# can be autoloaded
# Source Tkmaxima/RunMaxima.tcl		;# can be autoloaded

## source preamble.tcl

## source Readdata.tcl

## source Getdata1.tcl

## source Macros.tcl

## source Proxy.tcl

## source Send-some.tcl

## source Plotting.tcl

## source Fonts.tcl

## source Private.tcl

## source Getopt.tcl

## source Parse.tcl

## source Textinsert.tcl

## source Printops.tcl

## source Push.tcl

## source Plotconf.tcl

## source Rk.tcl

## source Adams.tcl

## source Plotdf.tcl

## source Plot2d.tcl

## source Plot3d.tcl

## source NPlot3d.tcl

# obsolete patchold.tcl

## source EOctave.tcl

## source EOpenplot.tcl

## source EMaxima.tcl

## source EHref.tcl

## source Browser.tcl

## source Wmenu.tcl

## source Tryftp2.tcl

## source Myhtml.tcl

## source Myhtml1.tcl

## source Base64.tcl

## source Bitmaps.tcl

## source Tryembed.tcl

## source OpenMath.tcl

## source NConsole.tcl

## source String.tcl

## source CMMenu.tcl

## source Prefs.tcl

## source RunMaxima.tcl

proc vMaxUsage {} {

    set usage {}
    
    lappend usage \
        [mc "Usage: xmaxima \[options\] \[url\]"] \
	[mc "           If given, \[url\] will be opened in the help browser instead"] \
	[mc "           of the default starting page."] \
	[mc "options:"] \
	[mc "    --help: Display this usage message."] \
	[mc "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>."] \
	[mc "    --use-version=<version>: Launch maxima version <version>."]

    tide_notify [join $usage "\n"]
}

proc lMaxInitSetOpts {} {
    global maxima_priv argv argv0 env

    set maxima_opts {}
    if { [lsearch $argv "--help"] > -1 } {
	vMaxUsage
	exit 0
    }
    set lisp_pos [lsearch -exact $argv "--lisp=*"]
    if { $lisp_pos > -1 } {
	set arg [lindex $argv $lisp_pos]
	set prefix_end [expr [string length "--lisp="] - 1]
	set lisp [string replace $arg 0 $prefix_end]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos $lisp_pos]
    }
    set lisp_pos [lsearch -exact $argv "-l"]
    if { $lisp_pos > -1 } {
	set lisp [lindex $argv [expr $lisp_pos + 1]]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos [expr $lisp_pos + 1]]
    }
    set version_pos [lsearch -exact $argv "--use-version=*"]
    if { $version_pos > -1 } {
	set arg [lindex $argv $version_pos]
	set prefix_end [expr [string length "--use-version="] - 1]
	set version [string replace $arg 0 $prefix_end]
	lappend maxima_opts -u $version
	set argv [lreplace $argv $lisp_pos $version_pos]
    }
    if { [llength $argv] == 1 } {
	set maxima_priv(firstUrl) [lindex $argv 0]
    } elseif { [llength $argv] > 1 } {
	tide_failure [M [mc "Error: arguments %s not understood."] "$argv"]
	exit 1
    }

    return $maxima_opts
}


object_class MAXTkmaxima {

    method create {} {
	global tcl_platform maxima_priv

	if {$tcl_platform(platform) == "windows" } {

	    set dir [file dir [info name]]
	    # These should be in the same directory as the xmaxima.exe
	    set maxima_priv(kill) [file join $dir winkill.exe]

	    set file [file join $dir tclwinkill.dll]
	    if {[file isfile $file]} {
		catch {load  $file}
	    }
	    unset file
	} else {
	    # unix
	    set maxima_priv(kill) kill
	}

    }

    method install {} {
	global maxima_priv argv argv0 env fontSize maxima_default

	wm withdraw .
	wm title . xmaxima

	setMaxDir

	cMAXINITBeforeIni
	if {[file isfile ~/xmaxima.ini]} {
	    if {[catch {uplevel "#0" [list source ~/xmaxima.ini] } err]} {
		tide_failure [M [mc "Error sourcing %s\n%s"] \
				  [file native ~/xmaxima.ini] \
				  $err]
	    }

	}
	cMAXINITAfterIni

	set fr .maxima
	MAXGui gui
	set w [gui install $fr]

	#mike Defer looking for maxima until the interface has been built
	vMAXSetMaximaCommand

	#mike Defer the starting of maxima until the interface has been built
	if {[catch {runOneMaxima $w} err]} {
	    tide_failure [concat [mc "Error starting Maxima:"] "\n$err"]
	    return
	}
	after idle focus $maxima_priv(cConsoleText)

    }

    method exit {{text ""} {val "0"}} {
	global maxima_priv
	
# jfa: We don't need to ask about saving preferences every single time.
# 	set retval [tide_yesnocancel [mc "Exiting Maxima. Save Preferences?"]]
# 	switch -exact -- $retval "1" {
# 	    catch {savePreferences}
# 	} -1 {
# 	    return
# 	}
	
	update
	if {$text == ""} {
	    if {[info exists maxima_priv(cConsoleText)]} {
		set text $maxima_priv(cConsoleText)
	    }
	}
	
	if {$text != ""} {
	    if {[catch {closeMaxima $text} err]} {
		tide_failure $err
	    }
	}

	tkexit $val
    }


}




