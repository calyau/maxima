# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima.tcl,v 1.21 2002-09-06 06:53:14 mikeclarkson Exp $
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
    puts {usage: xmaxima [options] [url]}
    puts {           If given, [url] will be opened in the help browser instead}
    puts "           of the default starting page."
    puts "options:"
    puts "    --help: Display this usage message."
    puts "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>."
    puts "    --use-version=<version>: Launch maxima version <version>."
}

proc doit { fr } {
    global NCtextHelp ws_openMath xmaximaPreferences argv argv0 env

    #mike Move this in from being at the global level
    setMaxDir

    #mike Move this in from being at the global level
    if {[file isfile ~/xmaxima.ini]} {
	catch { source ~/xmaxima.ini }
    }

    if {[winfo exists $fr]} {catch { destroy $fr }}

    set ws_openMath(options,maxima) {{doinsert 0 "Do an insertion" boolean}}
    frame .browser
    set firstUrl file:/[file join $ws_openMath(maxima_xmaximadir) "intro.html"]

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
	set firstURL [lindex $argv 0]
    } elseif { [llength $argv] > 1 } {
	puts "xmaxima: Error: arguments \"$argv\" not understood."
	exit 1
    }

    if { [auto_execok $ws_openMath(xmaxima_maxima)] != "" } {
	set ws_openMath(localMaximaServer) "$ws_openMath(xmaxima_maxima) $maxima_opts -p [file join $ws_openMath(maxima_xmaximadir) server.lisp] -r \":lisp (progn (user::setup PORT)(values))\" &"
    } else {
	if { [info exists env(XMAXIMA_MAXIMA)] } {
	    puts "xmaxima: Error. maxima executable XMAXIMA_MAXIMA=$env(XMAXIMA_MAXIMA) not found."
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

    OpenMathOpenUrl $firstUrl -toplevel .browser
    frame $fr
    pack $fr -expand 1 -fill both -side top
    pack .browser -side bottom
    set m [oget .browser.textcommands.file menu]
    $m add command -underline 0 -label "Toggle Visibility of $fr" -command "if { \[catch {pack info $fr} \] } {packBoth $fr .browser} else { pack forget $fr}"
    packBoth $fr .browser
    
    set men [CMmenu $fr]
    set w $fr.text
    clearLocal $fr.text
    oset $w heightDesired 80%
    oset $men textwin $w
    set ws_openMath(maximaWindow) $w
    #text $fr.text 
    #label [set msg $fr.label]  -height 1 -relief sunken \
#	    -textvariable ws_openMath(load_rate)
    
    closeMaxima $w
    clearLocal $w
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

    # oset $w program $program
    oset $w prompt "% " 
    catch { destroy $w } ;
    frame $fr.bottom -height 2
    $fr.bottom config -cursor double_arrow
    bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
    pack $fr.bottom -side bottom -fill x
   
    text $w -background white -yscrollcommand "$fr.scroll set"
    set ws_openMath($w,inputTag) input
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

    global ws_openMath

    if { ![regexp  input $ws_openMath(sticky)] } {
	append ws_openMath(sticky) {|^input$}
    }
    pack $fr.text -expand 1 -fill both -side left
    desetq "width height"  [getMaxDimensions]
    wm geometry . ${width}x${height}
    update
    
    if { [winfo height $fr] > .8 * [winfo height .]  } {
	$fr.text config -height 15
    }
    

    wm title . xmaxima

    #mike Defer the starting of maxima until the interface has been built
    runOneMaxima $w
  

}
