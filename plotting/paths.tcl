# Extract from Tkmaxima/Paths.tcl to load omplotdata messages

if { [info exists env(MAXIMA_DATADIR)] } {
    set maxima_datadir $env(MAXIMA_DATADIR)
} elseif { [info exists env(MAXIMA_PREFIX)] } {
    set maxima_datadir [file join $env(MAXIMA_PREFIX) share]
} else {
    set maxima_datadir $autoconf(datadir)
}

if {[file isdir $maxima_datadir]} {

    set maxima_omplotdata_msgs_dir \
        [file join $maxima_datadir $autoconf(package) $autoconf(version) msgs]

    if {[file isdir $maxima_omplotdata_msgs_dir]} {

	::msgcat::mcload $maxima_omplotdata_msgs_dir
    
    }

}



