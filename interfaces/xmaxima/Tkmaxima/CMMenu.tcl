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
	label $win.$v -text $v -font $buttonfont -relief raised
	$menubar add $win.$v
    }


    ####### begin help button

    setHelp $win.help {Bring down a menu with some help options}
    set m [oget $win.help menu]
    #oset $win showHelpBar "show help bar"
    $m add command -underline 0 -label {Maxima Help} -help {Visit local maxima help file in html} -command {OpenMathOpenUrl file:/[file join $ws_openMath(maxima_verpkgdatadir) doc html maxima_toc.html]}
     $m add command -underline 0 -label {Netmath} -help {Visit netmath page} -command {OpenMathOpenUrl http://www.ma.utexas.edu/users/wfs/netmath/netmath.html}
     $m add command -underline 0 -label {Run Tests} -help {Run the test files in the doc/*.mac} -command "sendMaxima \[oget $win textwin\] {:lisp (progn (xchdir \"[file join $ws_openMath(maxima_verpkgdatadir) tests]\")(load \"tests.lisp\"))\n}"

 
 
    

    
    ####### begin file button

    setHelp $win.file {Bring down a menu with some file options}
    set m [oget $win.file menu]
    #oset $win showFileBar "show file bar"
    $m add command -underline 0 -label {Toggle Browser Visibility} -help {Toggle display of Browser} -command {if { [catch { pack info .browser }] } { pack .browser -side bottom } else { pack forget .browser }} 
    $m add command -underline 0 -label {Exit} -command "destroy ." -help  "End this session of Maxima"
     $m add command -underline 0 -label {Interrupt   C-c C-c} -command "CMinterrupt \[oget $win textwin\]" -help  "Interrupt the Maxima process and reset the filter"
    $m add command -underline 0 -label {Restart} -command "runOneMaxima \[oget $win textwin\]" -help  "Kill the Maxima process and reset the filter, and start a new one"
#     $m add command -underline 0 -label {Preferences} -command "xmaximaPreferences" -help  "Set Preferences for Xmaxima saved in ~/xmaxima.ini"
     $m add command -underline 0 -label {Preferences} -command "fontDialog .preferences" -help  "Set Preferences for Xmaxima and Netmath saved in ~/netmath.ini"
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
		-command "console show" -help \
		{This console is used mainly in debugging xmaxima}
    }



     ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    #oset $win showEditBar "show edit bar"
    $m add command -underline 0 -label {Previous Input   Alt-p} -command "CNpreviousInput \[oget $win textwin\] -1" -help  "Insert previous inputs matching what is already typed"
    $m add command -underline 0 -label {Clear input   C-u} -command "CNclearinput \[oget $win textwin\]" -help  "Erase the current input"
    $m add command -underline 0 -label {Cut   C-c} -command "tk_textCut \[oget $win textwin\]" -help  "Cut the highlighted region, ready for pasting"
    $m add command -underline 0 -label {Paste   C-v} -command "tk_textPaste \[oget $win textwin\]" -help  "Paste the last cut region"
    pack $menubar -side top
    return $win
}
