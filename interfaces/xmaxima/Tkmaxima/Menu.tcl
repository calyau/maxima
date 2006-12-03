# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Menu.tcl,v 1.20 2006-12-03 22:26:12 villate Exp $
#

proc pMAXSaveTexToFile {text} {
    set file [tide_savefile [M [mc "Save to a file"]] "" *.out]
    if {$file != ""} {
	set contents [$text get 1.0 end]
	set fd [open $file w]
	if {[catch {puts $fd $contents} err]} {
	    tide_failure [M [mc "Error writing to file:\n%s"] $err]
	}
	catch {close $fd}
    }
}


proc vMAXAddSystemMenu {fr text} {
    global maxima_priv maxima_default
    global tcl_platform env

    set win $fr.textcommands

    # Build a system menubutton
    if {[winfo exists .menu]} {destroy .menu}
    menu .menu
    . configure -menu .menu

    # Add a File menubutton
    set m [menu .menu.file -tearoff 0]
    .menu add cascade -label [mc "File"] -menu $m -underline 0

    $m add command -underline 0 \
	-accel {Alt+b} \
	-label [set label [M [mc "Batch File"]]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M [mc "Open a file to Batch"]] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "batch(\"$file\")\$\n"
	    }
	}]]
    bind $text <Alt-Key-b> $command

    $m add command -underline 11 \
	-accel {Alt+o} \
	-label [set label [M [mc "Batch File Silently"]]] \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_openfile [M [mc "Open a file to Batch Silently"]] "" *.mac]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "batchload(\"$file\")\$\n"
	    }
	}]]
    bind $text <Alt-Key-o> $command

    $m add separator
    $m add command -underline 0 \
	-label [set label [mc "Save Expressions to File"]] \
	-accel {Ctrl+s} \
	-command [set command [cIDECreateEvent $text $label {
	    set file [tide_savefile [M [mc "Save to a file"]] "" *.bin]
	    if {$file != ""} {
		sendMaxima $maxima_priv(cConsoleText) "save(\"$file\",all)\$\n"
	    }
	}]]
    bind $text <Control-Key-s> $command


    $m add sep
    $m add command -underline 0 \
	-label [mc "Interrupt"] \
	-accel {Ctrl+g} \
	-command [list event generate $text <Control-Key-g>]
    $m add command -underline 0 \
	-label [mc "Restart"] \
	-command [list runOneMaxima $text]

    $m add separator
    $m add command -underline 1 \
	-label [mc "Exit"] \
	-command [list tkmaxima exit $text]

    # Add a Edit menubutton
    set m [menu .menu.edit -tearoff 0]
    .menu add cascade -label [mc "Edit"] -menu $m -underline 0

    $m add command -underline 2 \
	-label [mc "Cut"] \
	-accel {Ctrl+x} \
	-command [list event generate $text <Control-Key-x>]
    $m add command -underline 0 \
	-label [mc "Copy"] \
	-accel {Ctrl+c} \
	-command [list event generate $text <Control-Key-c>]
    $m add command -underline 0 \
	-label [mc "Paste"] \
	-accel {Ctrl+v} \
	-command [list event generate $text <Control-Key-v>]
    #mike distinguish from Cut/Copy/Past and Kill/Yank
    $m add separator
    $m add command -underline 0 \
	-label [mc "Kill"] \
	-accel {Ctrl+k} \
	-command [list event generate $text <Control-Key-k>]
    $m add command -underline 0 \
	-label [mc "Yank"] \
	-accel {Ctrl+y} \
	-command [list event generate $text <Control-Key-y>]
    $m add separator
    #mike FIXME: use event generate
    $m add command -underline 0 \
	-label [mc "Previous Input"] \
	-accel {Alt-p} \
	-command [list CNpreviousInput $text -1]
    $m add command -underline 0 \
	-label [mc "Next Input"] \
	-accel {Alt-n} \
	-command [list CNpreviousInput $text 1]
    $m add command -underline 9 -label [mc "Clear input"] \
	-accel {Ctrl+u} \
	-command [list CNclearinput $text]
    $m add separator
    $m add command -underline 0 -label [mc "Save Console to File"] \
	-command [list pMAXSaveTexToFile $maxima_priv(cConsoleText)]

    # Add a Options menubutton
    set m [menu .menu.options -tearoff 0]
    .menu add cascade -label [mc "Options"] -menu $m -underline 0

    $m add command -underline 0 -label [mc "Toggle Browser Visibility"] \
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
    $m add cascade -label [mc "Plot Windows"] -menu $pm
    foreach elt { embedded separate multiple } {
	$pm add radio -label [mc [string totit $elt]] \
	    -variable maxima_default(plotwindow) \
	    -value $elt
    }

    $m add separator
    $m add command -underline 0 \
	-label [mc "Preferences"] \
	-command {fontDialog .preferences}
    if {[info commands console] == "console" } {
	$m add sep
	$m add command -underline 0 -label [mc "Show Tcl Console"] \
	    -command [list console show]
    }

    # Add a Maxima menubutton
    set m [menu .menu.maxima -tearoff 0]
    .menu add cascade -label "Maxima" -menu $m -underline 0

    set km [menu $m.kill]
    $m add cascade -label [mc "Clear Memory"] -menu $km
    $km add command -label [mc "Kill All"] \
	-command [list sendMaxima $text "kill(all)\$\n"]
    $km add separator
    foreach elt {labels values functions macros arrays \
		     myoptions props aliases rules gradefs \
		     dependencies let_rule_packages} {
	$km add command -label "Kill [string totit $elt]" \
	    -command [list sendMaxima $text "kill($elt)\$\n"]
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
	-label [mc "Run Tests"] \
	-command [list sendMaxima $text "run_testsuite()\$\n"]


    # Add a Help menubutton
    set m [menu .menu.help -tearoff 0]
    .menu add cascade -label [mc "Help"] -menu $m -underline 0

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
    if {$tcl_platform(platform) == "windows"} {
        $m add command -underline 7 -label [mc "Manual (Xmaxima browser)"] \
        	-state $state \
	        -command [list exec hh.exe $file]
    } else {
        $m add command -underline 7 -label [mc "Manual (Xmaxima browser)"] \
        	-state $state \
	        -command "OpenMathOpenUrl \"file:/$file\""
    }
    set browse {exec}

    # FIXME: get a browser object
    if {$tcl_platform(platform) == "windows"} {
	if {$tcl_platform(os) == "Windows 95"} {
	    # Windows 95/98/ME
	    lappend browse start
	} else {
	    # Windows NT / 2000
	    lappend browse cmd.exe /c start
	}
    } else {
	
	set selectedbrowser mozilla

	foreach b { galeon firefox mozilla konqueror epiphany amaya netscape } {
	    if { ! [catch {exec which $b} ] } {
		set selectedbrowser $b
		break } }

	lappend browse $selectedbrowser
    }
    $m add sep
    $m add command -underline 0 -label [mc "Manual (web browser)"] \
	-command [list eval $browse "file://$file" &]
    $m add command -underline 0 -label [mc "Maxima Homepage"] \
	-command [list eval $browse http://maxima.sourceforge.net &]
    $m add command -underline 0 -label [mc "Project Page"] \
	-command [list eval $browse http://sourceforge.net/projects/maxima &]
    $m add command -underline 0 -label [mc "Bug Reports"] \
	-command [list eval $browse \
		      {http://sourceforge.net/tracker/?group_id=4933&atid=104933} &]

    rename vMAXAddSystemMenu ""
    # vMAXSystemMenuHandlers $text $event

    # Backwards compatability
    return $win
}


