############################################################
# Tkmaxima.tcl                                                  #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
############################################################

# The Header.tcl is created by autoconf to make the xmaxima script
# auto executable. After the header the various tcl pieces are put together:

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/COPYING.tcl           ;# license info
# Source Tkmaxima/Cygwin.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
# Source Tkmaxima/Preamble.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Readdata.tcl 		;# can be autoloaded
# Source Tkmaxima/Getdata1.tcl 		;# can be autoloaded
# Source Tkmaxima/Macros.tcl 		;# can be autoloaded
# Source Tkmaxima/Proxy.tcl 		;# can be autoloaded
# Source Tkmaxima/Send-some.tcl 	;# sets global variables
# Source Tkmaxima/Plotting.tcl 		;# sets global variables
# Source Tkmaxima/Fonts.tcl 		;# sets global variables
# Source Tkmaxima/colors.tcl
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
# Source Tkmaxima/scene.tcl 		;# can be autoloaded
# Source Tkmaxima/NPlot3d.tcl 		;# can be autoloaded
# Source Tkmaxima/EOctave.tcl 		;# can be autoloaded
# Source Tkmaxima/EOpenplot.tcl  	;# can be autoloaded
# Source Tkmaxima/EMaxima.tcl 		;# can be autoloaded
# Source Tkmaxima/EHref.tcl 		;# can be autoloaded
# Source Tkmaxima/Browser.tcl 		;# defines globals
# Source Tkmaxima/Bindings.tcl 		;# defines bindings
# Source Tkmaxima/Wmenu.tcl 		;# can be autoloaded
# Source Tkmaxima/Myhtml.tcl 		;# defines globals and tags
# Source Tkmaxima/Myhtml1.tcl 		;# can be autoloaded
# Source Tkmaxima/Base64.tcl 		;# can be autoloaded
# Source Tkmaxima/Bitmaps.tcl 		;# defines globals
# Source Tkmaxima/Tryembed.tcl 		;# defines globals?
# Source Tkmaxima/OpenMath.tcl 		;# active
# Source Tkmaxima/NConsole.tcl 		;# can be autoloaded
# Source Tkmaxima/String.tcl 		;# can be autoloaded
# Source Tkmaxima/RunMaxima.tcl		;# can be autoloaded
# Source Tkmaxima/Menu.tcl
# Source Tkmaxima/Paths.tcl
# Source Tkmaxima/Gui.tcl
# Source Tkmaxima/Tkmaxima.tcl

proc vMaxUsage {script {error {}}} {
    set msg [mc "$error\n\nUsage: $script \[options\] \[filenames\]

Options:
   -h, 
   --help                           Display this message
   --url <site>                     Start browser at site 
   -u <ver>, 
   --use-version <ver>              Launch maxima version ver
   -l <flavor>, 
   --lisp <flavor>                  Use lisp implementation flavor
   -X <Lisp options>
   --lisp-options <Lisp options>    Options to be given to the underlying Lisp.
                                    Option lines containing spaces have to be
                                    quoted to be passed to the lisp as a whole.
"]
    # Originally this program output a graphical message box instead of a message
    # on stdout - which looked nice, but is nonstandard => Replaced it by a
    # text-only message.
    #
    # tk_messageBox -type ok -icon info -title "Usage" -message $msg -parent .
    puts $msg
    exit
}

proc lMaxInitSetOpts {} {
    global argv argv0
    set ::xmaxima_priv(opts) {}
    set ::xmaxima_priv(plotfile) {}
    set state key
    foreach arg $argv {
	switch -- $state {
	    key {
		switch -regexp -- $arg {
		    {^--help$}         {vMaxUsage $argv0}
		    {^-h(elp)?$}       {vMaxUsage $argv0}
		    {^-(-)?url$}       {set state url}
		    {^-u(se-version)?$} {set state version}
		    {^--use-version$}  {set state version}
		    {^-l(isp)?$}       {set state lisp}
		    {^--lisp$}         {set state lisp}
		    {^--lisp-options$} {set state lispoptions}
		    {^-X$}             {set state lispoptions}
		    {^--$}             {set state noopts}
		    {^-.*}             {vMaxUsage $argv0 "Unknown option $arg"}
		    default {
			lappend ::xmaxima_priv(plotfile) $arg
			set state file
		    }
		}
	    }
	    file {
		switch -glob -- $arg {
		    -* {vMaxUsage $argv0 "Misplaced option $arg"}
		    default {lappend plotfile $arg}
		}
	    }
	    url     {set ::xmaxima_priv(firstUrl) $arg; set state key}
	    version {lappend ::xmaxima_priv(opts) -u $arg; set state key}
	    lisp    {lappend ::xmaxima_priv(opts) -l $arg; set state key}
	    lispoptions  {lappend ::xmaxima_priv(opts) [format " -X \"%s\" " $arg]}
	    noopts  {lappend file $arg}
	}
    }
}

# Exists Maxima after saving the current settings
proc maxExit {{text ""} {val "0"}} {
    # save user settings for future sessions
    catch {savePreferences}
    update
    if {$text eq ""} {
        if {[info exists ::xmaxima_priv(cConsoleText)]} {
            set text $::xmaxima_priv(cConsoleText)}
    } elseif {[catch {closeMaxima $text} err]} {
        tk_messageBox -title Error -icon error -message $err}
    tkexit $val}
