
global pkgDefDir
global here done
set here $pkgDefDir(maxima)
# replace the backslashess..
regsub -all "\\\\" $here / here

cd $here

source ./deliver/zipinfo.tcl


proc setPercent { n } {
    upstat $n
}
    
proc joe { f  zipfile} {
    global  installResult done unzipped 
    set tem [string trim [read $f] \n]
#    puts tem=$tem
    if { "$tem" == "" && [eof $f] } {
	if { [file exists src/max.ico] } {
	set installResult "Installation Completed Successfully"
	set done 1
	    
	} else {
	    set installResult "Installation Failed"
	    set done -1
	}
	 
	close $f
	update
	return
    }
    append unzipped($zipfile) \n$tem
    
    set tem  [split [string trim $tem \n] \n]
    global zipinfo
    setPercent [expr [llength $unzipped($zipfile)] *100.0 / [lindex $zipinfo($zipfile) 1]]
    set installResult [lindex $tem end]
    update
}

global zipfile

if { "[array names zipinfo]" != "" } {
  pack .progress.frame0
    .progress.frame0.frame3.label4 config -text {Unzipping: }
    .progress.frame0.frame3.label5 config -textvariable zipfile -text {}
    .progress.frame0.frame3.label0 config -textvariable {} -text {}
}



foreach v [array names zipinfo] {
    setPercent 0
    set zipfile $v
    update
    set f [open "| unzip -o  $zipfile" r]
    fconfigure $f -blocking 0
    set done 0 
    fileevent $f readable "joe $f $zipfile"
    vwait done
}


proc backslash { s } {
    regsub -all "/" $s "\\" s
    return $s
}

# wait to do these till the icons and stuff are there..
after 200 installShortcuts

proc installShortcuts {  } {
    global done here
    
  if { [tk_dialog .jim ask {Add xmaxima to the Start Menu under Programs} {} yes  no yes ] } {
    set programDir [file join [::freewrap::getSpecialDir PROGRAMS]]
    catch { file delete [file join $programDir Maxima.lnk] }
      ::freewrap::shortcut [file join $programDir Maxima.lnk] \
          -objectPath [backslash [file join $here src  xmaxima.exe]] \
	  -workingDirectory [backslash $here]  \
    	  -icon [backslash [file join $here src max.ico]] 0
   }
   ::freewrap::shortcut [file join $here Maxima.lnk] \
          -objectPath [backslash [file join $here src  xmaxima.exe]] \
	  -workingDirectory [backslash $here]  \
    	  -icon [backslash [file join $here src max.ico]] 0
   file delete maxima.zip
   file delete unpack.tcl
   set f [open init.lsp w]
   puts $f "(si::setenv \"PATH\" (concatenate 'string (substitute #\\\\ #\\/ \"$here/gcc/bin;\") (si::getenv \"PATH\")))"
   close $f
    
}



# file delete unzip.exe



