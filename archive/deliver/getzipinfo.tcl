
source ./deliver/pkginfo.tcl
set setup [open ./deliver/setup_contents.txt w]
foreach pkg [array names pkgFiles] {
    set size 0
    foreach f $pkgFiles($pkg) {
	if { "[file extension $f]" == ".zip" } {
	    set me [exec unzip -l $f]
	    set last [lindex [split $me \n] end]
	    set zipinfo($f) $last
	    incr size [lindex $last 0]
	} else { incr size [file size $f] }
	puts $setup $f
	
    }
    set pkgFileSpace($pkg) $size
}
close $setup
set f [open ./deliver/zipinfo.tcl w]
puts $f "# created by getzipinfo.tcl"
foreach v "zipinfo pkgFileSpace" {
puts $f "global $v"
puts $f [list array set $v [array get $v *]]
}
close $f


