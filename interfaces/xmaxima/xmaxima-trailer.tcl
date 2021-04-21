#     xmaxima-trailer.tcl
#     Time-stamp: "2021-03-28 11:05:43 villate"
#
# Attach this at the bottom of the xmaxima code to start up the interface.

setMaxDir
cMAXINITBeforeIni
cMAXINITReadIni
cMAXINITAfterIni 

if { [llength $maxima_priv(plotfile)] > 0 } {
    set fptr [open [lindex $maxima_priv(plotfile) 0] r]
    regsub -all -- {/\*.*?\*/} [read $fptr] {} inputdata
    close $fptr
    regsub -all -- {[[:space:]]+} $inputdata { } inputdata
    string trim $inputdata
     if {[catch {eval $inputdata}]} {
	 bgerror [mc "Input file has syntax errors"]
	 exit
     }
} else {
    MAXTkmaxima tkmaxima
    rename exit tkexit
    proc exit {{val "0"}} {tkmaxima exit "" $val}
    tkmaxima install
}
