############################################################
# Gui.tcl                                                  #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
#     Time-stamp: "2024-04-11 16:35:46 villate"            #
############################################################

# Creates the browser if it doesn't exist
proc createBrowser {bname} {
    global maxima_priv maxima_default
    if {[winfo exists $bname]} {
        focus $bname
    } else {
        toplevel $bname
        wm title $bname [mc {Xmaxima: browser}]
        OpenMathOpenUrl $maxima_priv(firstUrl) -toplevel $bname
        set maxima_priv(cBrowser) $bname
        set Maxima_default(browser) 1
        # Adds the menubar and the toolbar to the browser
        vMAXAddBrowserMenu $bname}}

# Creates the Maxima console
proc createConsole {cname} {
    global maxima_priv maxima_default
    # Makes the status panel....
    set st .status
    frame $st
    set maxima_priv(cStatusWindow) $st
    label $st.rate -width 35 -bd 1 -relief sunken  -justify left \
        -textvariable maxima_priv(load_rate) -anchor w
    scale $st.scale -showvalue 0 -length 200  -orient horizontal
    pack $st.rate -side left -fill x -expand 1 -anchor w
    pack $st.scale -side left
    pack $st -side bottom -fill x -anchor w
    set maxima_priv(cStatusLabel) $st.rate

    # Adds the toolbar to the Maxima console
    vMAXAddSystemBar
    frame $cname
    pack $cname -expand 1 -fill both -side top
    set w $cname.text
    clearLocal $w
    oset $w heightDesired 80%
    set maxima_priv(maximaWindow) $w
    closeMaxima $w
    clearLocal $w

    # oset $w program $program
    oset $w prompt "% "
    if {[winfo exists $w]} {catch { destroy $w }}
    frame $cname.bottom -height 2
    $cname.bottom config -cursor double_arrow
    bind  $cname.bottom <B1-Motion> "changeSize $w %Y"
    pack $cname.bottom -side bottom -fill x
    text $w -yscrollcommand "$cname.scroll set" \
	    	-selectbackground yellow -selectforeground blue
    set maxima_priv($w,inputTag) input
    # resetMaximaFont $w
    scrollbar $cname.scroll -command "$w yview"
    pack $cname.scroll -side right -fill y
    pack $cname.text -expand 1 -fill both -side left
    $w mark set lastStart end
    $w mark gravity lastStart left
    bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"
    $w configure -background white
    $w configure -foreground "#008600"
    $w tag configure input -foreground blue
    $w tag configure output -foreground black
    $w tag configure mprompt -foreground red

    # binding order will be: window bindings, CNtext bindings,
    # OpenMathText bindings and default bindings (usually Text . all)
    # CNtext ans OpenMathText bindings are set up in Bindings.tcl
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    if {![regexp -- input $maxima_priv(sticky)] } {
        append maxima_priv(sticky) {|^input$}}
    set maxima_priv(cConsoleText) $cname.text
    vMAXSetCNTextBindings $w
    wm protocol . WM_DELETE_WINDOW [list maxExit $w]

    # Sets up the console size and font
    $w configure -height $maxima_default(iConsoleHeight) \
        -width $maxima_default(iConsoleWidth)
    font configure ConsoleFont -family [lindex $maxima_default(ConsoleFont) 0] \
        -size [lindex $maxima_default(ConsoleFont) 1]
    $w configure -font ConsoleFont

    # Adds the menu bar to the Maxima console
    vMAXAddSystemMenu $cname $cname.text

    # Reads the history from previous runs
    set histfile "$maxima_priv(home)/.xmaxima_history"
    if {[file isfile $histfile]} {
        if {[catch {uplevel "#0" [list source $histfile]} err]} {
            tk_messageBox -title Error -icon error -message \
                [mc "Error sourcing %s\n%s" [file native $histfile] $err]}}
    return $w}

# Updates the information in the status bar at the bottom of the console
proc maxStatus {mess} {
	global maxima_priv
	set maxima_priv(load_rate) $mess
	$maxima_priv(cStatusLabel) configure -text $mess}
