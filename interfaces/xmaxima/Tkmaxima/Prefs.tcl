# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Prefs.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
proc xmaximaPreferences { } {
    global xmaximaPreferences
    catch {destroy .prefs}
    toplevel .prefs
    proc mkentry { w var text } { set fr $w ; frame $fr
    uplevel 1 append topack [list " $fr"]
    label $fr.lab -text "$text"
    entry $fr.e -width 20 -textvariable $var
    pack $fr.lab $fr.e -side left -padx 3 }
    set i 0
    foreach v [array names xmaximaPreferences] {
	mkentry .prefs.it[incr i] xmaximaPreferences($v) "$v: "
    }
    eval pack $topack -side bottom
    button .prefs.save -text save -command {set f [open ~/xmaxima.ini w] ;
    puts $f "array set xmaximaPreferences {"
    foreach {v w} [array get xmaximaPreferences *] {puts $f [list $v $w]}
    puts $f "}"
    close $f
    }

    button .prefs.apply -text apply -command {
	if { [catch { resetMaximaFont .maxima.text } ] } {
	    error  "fontAdjust must be an integer not $xmaximaPreferences(fontAdjust)"
	}
	

    }

    button .prefs.exit -text "exit preferences" -command {destroy .prefs}
    pack .prefs.save .prefs.apply .prefs.exit -side bottom -fill x


}



proc resetMaximaFont { w } {
    global xmaximaPreferences
    $w config -font [xHMmapFont font:fixed:normal:r:[expr $xmaximaPreferences(fontAdjust) + 3]]
}




