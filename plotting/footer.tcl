
cMAXINITBeforeIni
cMAXINITAfterIni 

set omcommand omPlotAny
if { [lindex $argv 0] == "plotdf" } {
    lappend omcommand "plotdf"
} else {
    lappend omcommand [exec cat [lindex $argv 0]]
}
for {set i 1} {$i<[llength $argv]} {incr i} {
    lappend omcommand [lindex $argv $i]
}

eval $omcommand


