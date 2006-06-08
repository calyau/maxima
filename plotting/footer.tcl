
cMAXINITBeforeIni
cMAXINITAfterIni 

set omcommand omPlotAny
if { [lindex $argv 0] == "plotdf" } {
    lappend omcommand "plotdf"
} else {
    set fptr [open [lindex $argv 0] r]
    set plotdata [read $fptr]
    close $fptr
    lappend omcommand $plotdata
}
for {set i 1} {$i<[llength $argv]} {incr i} {
    lappend omcommand [lindex $argv $i]
}

eval $omcommand


