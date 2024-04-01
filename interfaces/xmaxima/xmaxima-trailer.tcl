#     xmaxima-trailer.tcl
#     Time-stamp: "2024-04-01 14:13:55 villate"
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
    ################ MAXTkmaxima tkmaxima
    if {$tcl_platform(platform) == "windows" } {
        set dir [file dir [info name]]
        # These should be in the same directory as the xmaxima.exe
        set maxima_priv(kill) [file join $dir winkill.exe]
        set file [file join $dir tclwinkill.dll]
        if {[file isfile $file]} {catch {load  $file}}
        unset file
    } else {
        # unix
        set maxima_priv(kill) kill}
    ################
    rename exit tkexit
    #    proc exit {{val "0"}} {maxExit "" $val}
    ####### tkmaxima install
    wm withdraw .
    wm title . [mc {Xmaxima: console}]
    set fr .maxima
    ### replacemen for old object gui
    if {$tcl_platform(platform) == "windows" && \
            [info commands winico] != ""} {
        set file [file join \
                      $maxima_priv(maxima_xmaximadir) \
                      max.ico]
        if {[file isfile $file]} {
            set ico [winico createfrom $file]
            winico setwindow . $ico}}
    
    if {[winfo exists $fr]} {catch { destroy $fr }}
    # Creates the Maxima console       
    set w [createConsole $fr]
    
    wm deiconify .
    # Creates the browser in a separate window
    if {$maxima_default(browser)} {createBrowser .browser}
    
    ### end of replacement o object gui
    
    #mike Defer looking for maxima until the interface has been built
    vMAXSetMaximaCommand
    
    #mike Defer the starting of maxima until the interface has been built
    if {[catch {runOneMaxima $w} err]} {
        tide_failure [concat [mc "Error starting Maxima:"] "\n$err"]
        return
    }
    after idle focus $maxima_priv(cConsoleText)
}
