#!/bin/sh
# comment \
exec wish8.0 "$0" "$@"
# this file extracts a bunch of files at the end of it
# files are marked by "\n>>>Begin filename length\n"
# if the filename is *.doeval then it is evaluated as
# a tcl script, at the time it is encoutered.

proc main { } {
    global argv0 argv files
    set done ""
    set fi [open $argv0 r]
    fconfigure $fi -translation binary
    set data [read $fi 2000]
    set outdir /tmp/jim
    assureExists $outdir directory
    while { [outputOneFile $fi $outdir] } { }
    if {[llength $done] > 0 } {
	puts "unpacked $done"
	exit 0
    } else {
	puts "failed"
	exit 1
    }
}

proc outputOneFile {  stream outdir } {
    upvar 1 done done
    upvar 1 data data
    set exp  "\n>>>Begin (\[^ ]*) (\[0-9]+)\r?\n"
    puts "entering:[string length $data],[string range $data 0 200]"
    if { [regexp -indices $exp $data all] } {
	regexp $exp $data junk filename filesize
	set data [string range $data [expr 1 + [lindex $all 1]] end]
    } else { return 0 }
    if { [regexp {\.doeval$} $filename] } {
	eval $data
	return
    }
    set outfile [file join $outdir $filename]
    assureExists [file dirname $outfile] directory
    set ff [open $outfile w]
    fconfigure $ff -translation binary
    set remains $filesize
    while { 1 } {
	if { [string length $data] >= $remains } {
	    puts -nonewline $ff [string range $data 0 [expr $remains -1]]
	    set data [string range $data $remains end]
	    lappend done [list $filename $filesize $outfile]
	    close $ff
	    return 1
	} else { puts -nonewline $ff $data
	         incr remains -[string length $data]
	          #puts "writing [string length $data]"
	         set data ""
	}
	set read [read $stream 5000]
	append data $read
	if { [string length $read] == 0 } {
	    close $ff
	    file delete $outfile
	    error "Terminates in middle of reading $filename: remains $remains"
	}
    }
}

    

proc assureExists { dir type } {
    if { [catch {file stat $dir stat} ] } {
	if { "$type" == "directory" } {
	    file mkdir $dir
	    return 1
	}
    }
    if { "$stat(type)" != "directory" }  {
	error "not a $type it is a $stat(type)"
}   }



main




>>>Begin xmcd.tgz 651163
jimmy

>>>Begin billy 8
hi there


