# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Menu.tcl,v 1.9 2003-01-20 16:22:26 mikeclarkson Exp $
#

proc pMAXSaveTexToFile {text} {
    set file [tide_savefile [M "Save to a file"] "" *.out]
    if {$file != ""} {
	set contents [$text get 1.0 end]
	set fd [open $file w]
	if {[catch {puts $fd $contents} err]} {
	    tide_failure [M "Error writing to file:\n%s" $err]
	}
	catch {close $fd}
    }
}


proc vMAXAddSystemMenu {fr text} {
    global maxima_priv maxima_default
    global tcl_platform

    set win $fr.textcommands

    # Build a system menubutton
    if {[winfo exists .menu]} {destroy .menu}
    menu .menu
    . configure -menu .menu

    # Add a File menubutton
    set m [menu .menu.file -tearoff 0]
    .menu add cascade -label File -menu $m -underline 0

    $m add command -underline 0 \
	-accel {Ctrl+b} \
	-label [set label [M {Batch File}]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to Batch"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "BATCH(\"$file\")\$\n"
	    }
	}]]
    bind $text <Control-Key-b> $command

    $m add command -underline 11 \
	-accel {Ctrl+o} \
	-label [set label [M {Batch File Silently}]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M "Open a file to BATCHLOAD"] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "BATCHLOAD(\"$file\")\$\n"
	    }
	}]]
    bind $text <Control-Key-o> $command

    $m add separator
    $m add command -underline 0 \
	-label [set label {Save Expressions to File}] \
	-accel {Ctrl+s} \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_savefile [M "Save to a file"] "" *.bin]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "SAVE(\"$file,ALL\")\$\n"
	    }
	}]]
    bind $text <Control-Key-s> $command


    $m add sep
    $m add command -underline 0 \
	-label {Interrupt} \
	-accel {Ctrl+g} \
	-command [list event generate $text <Control-Key-g>]
    $m add command -underline 0 \
	-label {Restart} \
	-command [list runOneMaxima $text]

    $m add separator
    $m add command -underline 1 \
	-label {Exit} \
	-command [list tkmaxima exit $text]

    # Add a Edit menubutton
    set m [menu .menu.edit -tearoff 0]
    .menu add cascade -label Edit -menu $m -underline 0

    $m add command -underline 2 \
	-label {Cut} \
	-accel {Ctrl+x} \
	-command [list event generate $text <Control-Key-x>]
    $m add command -underline 0 \
	-label {Copy} \
	-accel {Ctrl+c} \
	-command [list event generate $text <Control-Key-c>]
    $m add command -underline 0 \
	-label {Paste} \
	-accel {Ctrl+v} \
	-command [list event generate $text <Control-Key-v>]
    #mike distinguish from Cut/Copy/Past and Kill/Yank
    $m add separator
    $m add command -underline 0 \
	-label {Kill} \
	-accel {Ctrl+k} \
	-command [list event generate $text <Control-Key-k>]
    $m add command -underline 0 \
	-label {Yank} \
	-accel {Ctrl+y} \
	-command [list event generate $text <Control-Key-y>]
    $m add separator
    #mike FIXME: use event generate
    $m add command -underline 0 \
	-label {Previous Input} \
	-accel {Alt-p} \
	-command [list CNpreviousInput $text -1]
    $m add command -underline 0 \
	-label {Next Input} \
	-accel {Alt-n} \
	-command [list CNpreviousInput $text 1]
    $m add command -underline 9 -label {Clear input} \
	-accel {Ctrl+u} \
	-command [list CNclearinput $text]
    $m add separator
    $m add command -underline 0 -label {Save Console to File} \
	-command [list pMAXSaveTexToFile $maxima_priv(cConsoleText)]

    # Add a Options menubutton
    set m [menu .menu.options -tearoff 0]
    .menu add cascade -label Options -menu $m -underline 0

    $m add command -underline 0 -label {Toggle Browser Visibility} \
	-command {
	    #mike FIXME: hard coding
	    if { [catch { pack info .browser }] } {
		packBoth .maxima .browser
	    } else {
		pack forget .browser
	    }
	}

    $m add separator
    set pm [menu $m.plot]
    $m add cascade -label "Plot Windows" -menu $pm
    foreach elt { embedded separate multiple } {
	$pm add radio -label [string totit $elt] \
	    -variable maxima_default(plotwindow) \
	    -value $elt
    }

    $m add separator
    $m add command -underline 0 \
	-label {Fonts} \
	-command {fontDialog .preferences}
    if {[info commands console] == "console" } {
	$m add sep
	$m add command -underline 0 -label "Show Tcl Console" \
	    -command "console show"
    }

    # Add a Maxima menubutton
    set m [menu .menu.maxima -tearoff 0]
    .menu add cascade -label Maxima -menu $m -underline 0

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
    set dir $maxima_priv(pTestsDir)
    if {[file isdir $dir]} {
	set state normal
    } else {
	set state disabled
    }
    $m add command -underline 0 \
	-state $state \
	-label {Run Tests} \
	-command "sendMaxima $text {:lisp (progn (\#+gcl si::chdir \#+clisp ext:cd \#+cmu unix:unix-chdir \"$dir\")(load \"tests.lisp\"))\n}"


    # Add a Help menubutton
    set m [menu .menu.help -tearoff 0]
    .menu add cascade -label Help -menu $m -underline 0

    set file $maxima_priv(pReferenceToc)
    if {[file isfile $file]} {
	set state normal
	if {$tcl_platform(platform) == "windows"} {
	    # decodeURL is broken and needs fixing
	    # This is a workaround
	    set file [file attrib $file -shortname]
	}
    } else {
	set state disabled
    }
    $m add command -underline 7 -label {Maxima Help} \
	-state $state \
	-command "OpenMathOpenUrl \"file:/$file\""
    set browse {exec}

    # FIXME: get a browser object
    if {$tcl_platform(platform) == "windows"} {
	if {$tcl_platform(os) == "Windows 95"} {
	    # Windows 95/98
	    lappend browse command.com /c start
	} else {
	    # Windows NT / 2000 - untested
	    lappend browse cmd.exe /c start
	}
    } else {
	# This is more difficult under Unix - KDE GNOME CDE etc...
	lappend browse netscape
    }
    $m add sep
    $m add command -underline 0 -label {Maxima Homepage} \
	-command [list eval $browse http://maxima.sourceforge.net &]
    $m add command -underline 0 -label {Project Page} \
	-command [list eval $browse http://sourceforge.net/projects/maxima &]
    $m add command -underline 0 -label {Bug Reports} \
	-command [list eval $browse \
		      {http://sourceforge.net/tracker/?group_id=4933&atid=104933} &]

    rename vMAXAddSystemMenu ""
    # vMAXSystemMenuHandlers $text $event

    # Backwards compatability
    return $win
}
