# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Gui.tcl,v 1.2 2004-10-31 16:55:26 vvzhy Exp $
#

object_class MAXGui {

    method __init__ {} {
	global tcl_platform maxima_priv

    }

    method install {fr} {
	global tcl_platform maxima_priv

	if {$tcl_platform(platform) == "windows" && \
		[info commands winico] != ""} {
	    set file [file join \
			  $maxima_priv(maxima_xmaximadir) \
			  max.ico]
	    if {[file isfile $file]} {
		set ico [winico createfrom $file]
		winico setwindow . $ico
	    }
	}

	if {[winfo exists $fr]} {catch { destroy $fr }}


	######## make status panel....
	set st .status
	frame $st

	set maxima_priv(cStatusWindow) $st
	label $st.rate -width 35 -bd 1 -relief sunken \
	    -justify left \
	    -textvariable maxima_priv(load_rate) -anchor w
	scale $st.scale -showvalue 0 -length 200 \
	    -orient horizontal
	pack $st.rate -side left -fill x -expand 1 -anchor w
	pack $st.scale -side left
	pack $st -side bottom -fill x -anchor w
	set maxima_priv(cStatusLabel) $st.rate



	frame .browser

	OpenMathOpenUrl $maxima_priv(firstUrl) -toplevel .browser
	set maxima_priv(cBrowser) .browser

	frame $fr
	pack $fr -expand 1 -fill both -side top
	pack .browser -side bottom -expand 1 -fill both
	packBoth $fr .browser

	set w $fr.text

	clearLocal $w
	oset $w heightDesired 80%
	set maxima_priv(maximaWindow) $w

	closeMaxima $w
	clearLocal $w

	# oset $w program $program
	oset $w prompt "% "
	if {[winfo exists $w]} {catch { destroy $w }}

	frame $fr.bottom -height 2
	$fr.bottom config -cursor double_arrow
	bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
	pack $fr.bottom -side bottom -fill x

	text $w -yscrollcommand "$fr.scroll set" \
	    	-selectbackground "#808080"
	set maxima_priv($w,inputTag) input
	resetMaximaFont $w
	scrollbar $fr.scroll -command "$w yview"
	pack $fr.scroll -side right -fill y
	pack $fr.text -expand 1 -fill both -side left

	$w mark set lastStart end
	$w mark gravity lastStart left
	bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"

	$w tag configure input -foreground blue

	bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

	if {![regexp -- input $maxima_priv(sticky)] } {
	    append maxima_priv(sticky) {|^input$}
	}
	set maxima_priv(cConsoleText) $fr.text

	vMAXSetCNTextBindings $w
	wm protocol . WM_DELETE_WINDOW [list tkmaxima exit $fr.text]

	if {0} {
	    # Simple apporach won't work with plotting

	    $fr.text configure \
		-height $maxima_default(iConsoleHeight) \
		-width $maxima_default(iConsoleWidth)
	    set btext [info commands .browser.*.text]
	    $btext configure \
		-height $maxima_default(iConsoleHeight) \
		-width $maxima_default(iConsoleWidth)

	    update
	    if {[set h [winfo reqheight .]] > \
		    [set max [expr [winfo screenheight .] \
				  - (2 * abs($fontSize))]]} {
		set cur [$btext cget -height]
		set delta [expr \
			       int (($h - $max) / abs($fontSize))]
		$btext config -height [expr $cur - $delta]
	    }

	} else {
	    # There's voodo that makes this work with plotting
	    # May the force be with you.
	    desetq "width height"  [getMaxDimensions]
	    wm geometry . ${width}x${height}
	    update
	    
	    # ZW: this text window size adjustment doesn't
	    # work as expected making window height too small.
	    #if { [winfo height $fr] > .8 * [winfo height .]  } {
	    #	$fr.text config -height 15
	    #}
	}


	# Add a proper system menu
	vMAXAddSystemMenu $fr $maxima_priv(cConsoleText)
	wm deiconify .

	return $w
    }

    method status {mess} {
	global maxima_priv
	set maxima_priv(load_rate) $mess
	$maxima_priv(cStatusLabel) configure -text $mess
    }

}

