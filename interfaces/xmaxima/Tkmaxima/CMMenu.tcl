# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: CMMenu.tcl,v 1.4 2002-09-08 01:48:26 mikeclarkson Exp $
#
proc CMmenu { win } {
    global buttonfont ws_openMath
    set menubar $win.textcommands
    set win $menubar
    if { [winfo exists $menubar] } {
	return $menubar
    }
    wmenubar $menubar
    pack $menubar -side top -expand 0 -fill x -anchor nw
    foreach v { file edit help  } {
	label $win.$v -text [string totit $v] -relief raised
	$menubar add $win.$v
    }


    ####### begin help button

    setHelp $win.help {Bring down a menu with some help options}
    set m [oget $win.help menu]
    #oset $win showHelpBar "show help bar"
    set file $ws_openMath(pReferenceToc)
    $m add command -underline 0 -label {Maxima Help} \
	-command "OpenMathOpenUrl \"file:/$file\""
    $m add command -underline 0 -label {Maxima Homepage} \
	-command {OpenMathOpenUrl http://maxima.sourceforge.net}
    set dir $ws_openMath(pTestsDir)
    $m add command -underline 0 -label {Run Tests} \
	-command "sendMaxima \[oget $win textwin\] {:lisp (progn (xchdir \"$dir\")(load \"tests.lisp\"))\n}"



    ####### begin file button

    setHelp $win.file {Bring down a menu with some file options}
    set m [oget $win.file menu]
    #oset $win showFileBar "show file bar"
    $m add command -underline 0 -label {Toggle Browser Visibility} \
	-help {Toggle display of Browser} -command {if { [catch { pack info .browser }] } { pack .browser -side bottom } else { pack forget .browser }}
    $m add command -underline 0 -label {Exit} -command "destroy ." \
	-help  "End this session of Maxima"
    $m add command -underline 0 -label {Interrupt   C-c C-c} -command "CMinterrupt \[oget $win textwin\]" \
	-help  "Interrupt the Maxima process and reset the filter"
    $m add command -underline 0 -label {Restart} -command "runOneMaxima \[oget $win textwin\]" \
	-help  "Kill the Maxima process and reset the filter, and start a new one"
    #     $m add command -underline 0 -label {Preferences} -command "xmaximaPreferences" -help  "Set Preferences for Xmaxima saved in ~/xmaxima.ini"
    $m add command -underline 0 -label {Preferences} -command "fontDialog .preferences" \
	-help  "Set Preferences for Xmaxima and Netmath saved in ~/netmath.ini"
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
	    -command "console show" \
	    -help \
	    {This console is used mainly in debugging xmaxima}
    }



    ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    #oset $win showEditBar "show edit bar"
    $m add command -underline 0 -label {Previous Input   Alt-p} -command "CNpreviousInput \[oget $win textwin\] -1"
    $m add command -underline 0 -label {Clear input   C-u} -command "CNclearinput \[oget $win textwin\]" 
    $m add command -underline 0 -label {Cut   C-c} -command "tk_textCut \[oget $win textwin\]" 
    $m add command -underline 0 -label {Paste   C-v} -command "tk_textPaste \[oget $win textwin\]" \
	pack $menubar -side top
    return $win
}
