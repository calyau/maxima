# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima.tcl,v 1.29 2002-09-10 09:16:12 mikeclarkson Exp $
#

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
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
# Source Tkmaxima/Browser.tcl 		;# defines globals and bindings
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
# Source Tkmaxima/CMMenu.tcl 		;# can be autoloaded
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

proc usage {} {
    set usage {}
    lappend usage "usage: xmaxima [options] [url]" \
	"           If given, [url] will be opened in the help browser instead" \
	"           of the default starting page." \
	"options:" \
	"    --help: Display this usage message." \
	"    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>." \
	"    --use-version=<version>: Launch maxima version <version>."

    tide_notify [join $usage "\n"]
}

proc vMAXSetCNTextBindings {w} {
     if { 1 || "[bind CNtext <Return>]" == "" } {
 	bind CNtext <Return> "CMeval %W  ; break"
 	bind CNtext <Control-c><Control-c> "CMinterrupt %W "
 	bind CNtext <Control-u> "CNclearinput %W "
 	bind CNtext "\)"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\]"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\}"  "CNblinkMatchingParen %W %A"
 	bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
 	bind CNtext <Alt-p>  "CNpreviousInput $w -1"
        bind CNtext <Alt-n>  {sendMaxima %W ":n\n"}
        bind CNtext <Alt-s>  {sendMaxima %W ":s\n" }
	bind CNtext <Control-Key-c>  {tk_textCut %W ;break}
	bind CNtext <Control-Key-v>  {tk_textPaste %W ;break}
    }

}

proc lMaxInitSetOpts {} {
    global maxima_priv argv argv0 env

    set maxima_opts {}
    if { [lsearch $argv "--help"] > -1 } {
	usage
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
	tide_failure "Error: arguments \"$argv\" not understood."
	exit 1
    }

    return $maxima_opts
}


rename exit tkexit
proc vMAXExit {{text ""} {val "0"}} {
    global maxima_priv

    if {$text == ""} {
	if {[info exists maxima_priv(cConsoleText)]} {
	    set text $maxima_priv(cConsoleText)
	} else {
	    set text ""
	}
    }
    catch \{closeMaxima $text\}
    tkexit $val
}
proc exit {{val "0"}} {vMAXExit "" $val}

proc doit { fr } {
    global maxima_priv argv argv0 env fontSize

    wm withdraw .
    wm title . xmaxima

    #mike Move this in from being at the global level
    if {[file isfile ~/xmaxima.ini]} {
	catch { source ~/xmaxima.ini }
    }

    if {[winfo exists $fr]} {catch { destroy $fr }}
    frame .browser

    OpenMathOpenUrl $maxima_priv(firstUrl) -toplevel .browser
    set maxima_priv(cBrowser) .browser

    frame $fr
    pack $fr -expand 1 -fill both -side top
    pack .browser -side bottom
    set m [oget .browser.textcommands.file menu]
    $m add command -underline 0 -label "Toggle Visibility of $fr" -command "if { \[catch {pack info $fr} \] } {packBoth $fr .browser} else { pack forget $fr}"
    packBoth $fr .browser

    set w $fr.text

    #mike An abomination:
    # set men [CMmenu $fr]
    # oset $men textwin $w
    # Replace with a proper system menu below

    clearLocal $w
    oset $w heightDesired 80%
    set maxima_priv(maximaWindow) $w
    
    closeMaxima $w
    clearLocal $w

    # oset $w program $program
    oset $w prompt "% " 
    catch { destroy $w } ;
    frame $fr.bottom -height 2
    $fr.bottom config -cursor double_arrow
    bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
    pack $fr.bottom -side bottom -fill x
   
    text $w -background white -yscrollcommand "$fr.scroll set"
    set maxima_priv($w,inputTag) input
    resetMaximaFont $w
    scrollbar $fr.scroll -command "$w yview"
    pack $fr.scroll -side right -fill y
    bind $w <Destroy> "closeMaxima $w"
    
    $w mark set lastStart end
    $w mark gravity lastStart left
    bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global maxima_priv

    if { ![regexp  input $maxima_priv(sticky)] } {
	append maxima_priv(sticky) {|^input$}
    }
    pack $fr.text -expand 1 -fill both -side left
    set maxima_priv(cConsoleText) $fr.text

    $fr.text configure -height 24 -width 80
    set btext [info commands .browser.*.text]
    $btext configure -height 24 -width 80

    vMAXSetCNTextBindings $w
    wm protocol . WM_DELETE_WINDOW [list vMAXExit $fr.text]

    update
    if {[set h [winfo reqheight .]] > \
	    [set max [expr [winfo screenheight .] \
			  - (2 * abs($fontSize))]]} {
	set cur [$btext cget -height]
	set delta [expr \
		       int (($h - $max) / abs($fontSize))]
	$btext config -height [expr $cur - $delta]
    }


    # Add a proper system menu
    vMAXAddSystemMenu $fr $maxima_priv(cConsoleText)
    wm deiconify .

    #mike Defer looking for maxima until the interface has been built
    vMAXSetMaximaCommand

    #mike Defer the starting of maxima until the interface has been built
    if {[catch {runOneMaxima $w} err]} {
	tide_failure "Error starting Maxima:\n$err"
    }

}


