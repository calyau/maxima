# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Menu.tcl,v 1.1 2002-09-07 08:48:34 mikeclarkson Exp $
#

proc vMAXAddSystemMenu {fr text} {
    global ws_openMath xHMpreferences

    set ws_openMath(cConsoleText) $text
    set win $fr.textcommands

    # Build a system menubutton
    menu .menu
    . configure -menu .menu

    # Add a File menubutton
    set m [menu .menu.file -tearoff 0]
    .menu add cascade -label File -menu $m

    $m add command -underline 0 \
	    -accel {Ctrl+b} \
	    -label [set label [M {Batch File}]] \
	    -command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to Batch"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $ws_openMath(cConsoleText) "BATCH(\"$file\")\$\n"
	    }
	}]]
	    bind $text <Control-Key-b> $command

    $m add command -underline 11 \
	    -accel {Ctrl+o} \
	    -label [set label [M {Batch File Silently}]] \
	    -command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to BATCHLOAD"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $ws_openMath(cConsoleText) "BATCHLOAD(\"$file\")\$\n"
	    }
	}]]
	    bind $text <Control-Key-o> $command

    $m add sep
    $m add command -underline 0 \
	    -label {Interrupt} \
	    -accel {C-c C-c} \
	    -command [list CMinterrupt $text]
    $m add command -underline 0 \
	    -label {Restart} \
	    -command [list runOneMaxima $text]

    $m add separator
    $m add command -underline 0 \
	    -label {Exit} \
	    -command "catch \{closeMaxima $text\}; exit"

    # Add a Edit menubutton
    set m [menu .menu.edit -tearoff 0]
    .menu add cascade -label Edit -menu $m

    $m add command -underline 0 \
	    -label {Copy} \
	    -accel {Ctrl+c} \
	    -command [list event generate $text <<Copy>>]
    $m add command -underline 0 \
	    -label {Cut} \
	    -accel {Ctrl+x} \
	    -command [list event generate $text <<Cut>>]
    $m add command -underline 0 \
	    -label {Paste} \
	    -accel {Ctrl+v} \
	    -command [list event generate $text <<Paste>>]
    $m add separator
    $m add command -underline 0 \
	    -label {Previous Input} \
	    -accel {Alt-p} \
	    -command [list CNpreviousInput $text -1]
    $m add command -underline 0 -label {Clear input} \
	    -accel {Ctrl+u} \
	    -command [list CNclearinput $text]

    # Add a Options menubutton
    set m [menu .menu.options -tearoff 0]
    .menu add cascade -label Options -menu $m

    $m add command -underline 0 -label {Toggle Browser Visibility} \
	    -command {
	if { [catch { pack info .browser }] } { 
	    pack .browser -side bottom 
	} else { 
	    pack forget .browser 
	}
    }

    $m add separator
    set pm [menu $m.plot]
    $m add cascade -label "Plot Windows" -menu $pm
    foreach elt { embedded separate multiple } {
	$pm add radio -label [string totit $elt] \
		-variable xHMpreferences(plotwindow) \
		-value $elt
    }

    $m add separator
    $m add command -underline 0 \
	     -label {Fonts} \
	     -command {fontDialog .preferences}
     $m add command -underline 0 -label {Preferences} \
	     -command "xmaximaPreferences"

    # Add a Maxima menubutton
    set m [menu .menu.maxima -tearoff 0]
    .menu add cascade -label Maxima -menu $m
 
	set km [menu $m.kill]
	$m add cascade -label "Clear Memory" -menu $km
	$km add command -label "Kill All" \
		-command [list sendMaxima $text "KILL(ALL)\$\n"]
	$km add separator
     foreach elt {LABELS VALUES FUNCTIONS MACROS ARRAYS \
	     MYOPTIONS PROPS ALIASES RULES GRADEFS \
	     DEPENDENCIES LET_RULE_PACKAGES} {
	 $km add command -label "Kill [string totit $elt]" \
		-command [list sendMaxima $text "KILL($elt)\$\n"]
     }
     $m add separator
     $m add command -underline 0 \
	     -label {Run Tests} \
	     -command "sendMaxima $text {:lisp (progn (si::chdir \"[file join $ws_openMath(maximaPath) doc]\")(load \"tests.lisp\"))\n}"
     

    # Add a Help menubutton
    set m [menu .menu.help -tearoff 0]
    .menu add cascade -label Help -menu $m

    $m add command -underline 7 -label {Maxima Help} \
	    -command {OpenMathOpenUrl file:/[file join $ws_openMath(maximaPath) info maxima_toc.html]}
     $m add command -underline 12 -label {Maxima Home Page} \
	     -command {OpenMathOpenUrl http://maxima.sourceforge.net/}

     rename vMAXAddSystemMenu ""
     # vMAXSystemMenuHandlers $text $event

     # Backwards compatability
     return $win
}

