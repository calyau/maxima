# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: RunMaxima.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
proc textWindowWidth { w } {
    set font [$w cget -font]
    set w20 [font measure [$w cget -font] -displayof $w "01234567890123456789"]
    return [expr round(floor([winfo width $w]*20.0/$w20))]
}


proc resizeMaxima { win width height } {
    linkLocal $win pid
    if { [info exists pid] && $pid!=-1 } {
	set wid [expr [textWindowWidth $win]-6]
	sendMaxima $win ":lisp-quiet (setq linel $wid)\n"
    }
}

proc packBoth {fr browser} {
    pack forget $fr $browser
    pack $fr -expand 1 -fill both -side top
    pack $browser -side bottom -fill x
}

proc CMeval { w } {
    linkLocal $w inputs
    set prev ""
    #puts "CMeval $w, [$w compare insert < lastStart]"
    if { [$w compare insert < lastStart] } {
	set this [thisRange $w input insert]
	if { "$this" != "" } {
	    set code [eval $w get $this]
	    set prev [string trimright [$w get lastStart end] \n]

	    $w delete lastStart end
	    $w insert lastStart $code input
	}
   }
#    puts "expr=<[$w get lastStart end]>"
 #  puts "tags=[$w tag names insert],insert=[$w index insert]"
   if { [lsearch [$w tag names insert] insert] >= 0 } {
       $w mark set lastStart [lindex [$w tag prevrange input] 0]
   }
    set expr [string trimright [$w get lastStart end] \n]
    if { ![regexp "^\[ \n\t]*:|\[;\$]\$" $expr] } {
	$w insert insert "\n"
	$w see insert
	if { [oget $w atMaximaPrompt] } {
	return
	}
    }

    $w tag add input lastStart "end -1char"
    $w mark set  lastStart "end -1char"
    lappend inputs $expr
    set tag ""
#    puts "sending <$expr>"
    # set res [sendMaxima $w $expr ]
     set res [sendMaxima $w $expr\n ]
    # set res [sendMaxima $w $expr ]
   # puts "[$w dump -all "lastStart linestart" end]"
    #message "send form"

}

proc acceptMaxima { win port filter } {
    set count 3 ;
    catch { close [oget $win server] }
    while {[incr count -1 ] > 0 } {
	if { ![catch {oset $win server [socket -server "runMaxima $win $filter" $port]} ] } {
	    # puts "server sock [oget $win server]"
	    return $port
    } else { incr port   }
 }
    return -1
}

proc openMaxima { win filter } {
    global ws_openMath env
    set port [acceptMaxima $win 4008 $filter]
    if { $port >= 0 } {
	set com "exec "
	append com    $ws_openMath(localMaximaServer)
	regsub PORT $com $port com
	if { [info exists env(MAXIMA_INT_INPUT_STRING)] } {
	    regsub PORT $env(MAXIMA_INT_INPUT_STRING) $port env(MAXIMA_INT_INPUT_STRING)
	    #puts env(MAXIMA_INT_LISP_PRELOAD)=$env(MAXIMA_INT_LISP_PRELOAD)
	    #puts env(MAXIMA_INT_INPUT_STRING)=$env(MAXIMA_INT_INPUT_STRING)
	}
	#puts com=$com
	if { [catch { eval $com } err ] } {
	    tk_messageBox -title "Error" -message "Can't execute $ws_openMath(localMaximaServer) : $err" }
    } else {error "could not open a socket " }
}

proc runMaxima { win  filter sock args } {
    linkLocal $win server
    oset $win maximaSocket $sock
    fconfigure $sock -blocking 0 -translation lf
    fileevent $sock readable "$filter $win $sock"
    if { [info exists server] } {
	# puts "closing server $server"
    catch {
    close $server
    unset server
    }
} else { # puts "server unset ??"
}
}

proc closeMaxima { win } {
    global pdata
    linkLocal $win maximaSocket pid
    foreach v [array names pdata maxima*] { unset pdata($v) }

    if {[info exists pid] && $pid != "" && [string is int $pid]} {
	catch {
	    CMkill -TERM $pid
	    unset pid
	}
    }

    if {[info exists maximaSocket] && $maximaSocket != ""} {
	catch {
	    close $maximaSocket
	    unset maximaSocket
	}
    }
}




#
 #-----------------------------------------------------------------
 #
 # maximaFilter --  filter the output on SOCKET inserting in WINDOW
 # recognizing
 #     \032\032:file:line:charpos\n
 #               -->redisplay in other window
 # \032\031tcl: command \n
 #           --> eval tcl command o
 #
 #
 #  Results: none
 #
 #  Side Effects:  input is read from SOCK and WIN has items displayed.
 #
 #----------------------------------------------------------------
#
#todo fix sendMaximaWait win expr
proc maximaFilter { win sock } {
    linkLocal $win  plotPending
    global pdata
    if { [eof $sock] } {
	# puts "at end"
	close $sock
	return ""
    }
    set it [read $sock]
   # puts "read=<$it>"
    if { [string first "\032\032" $it] >= 0 &&
     [regexp  -indices "\032\032(\[^:]+):(\[0-9]+):\[^\n]*\n" $it junk file line] } {
	
	 dblDisplayFrame [getMatch $it $file] [getMatch $it $line]
	 append res [string range $it 0 [expr { [lindex $junk 0] -1 } ]]
	 append res [string range $it [expr { 1+[lindex $junk 1]}] end]
	 set it $res
     }
     if { [string first "\032\031tcl:" $it] >= 0 &&
            [regexp  -indices "\032\031tcl:(\\[^\n]*)\n" $it junk com]
     } {
	 eval $com
	 append res [string range $it 0 [expr { [lindex $junk 0] -1 } ]]
	 append res [string range $it [expr { 1+[lindex $junk 1]}] end]
	 set it $res
     }
    # puts it=<$it>
    if { [regexp -indices "\{plot\[d23]\[fd]" $it inds] } {
	set plotPending [string range $it [lindex $inds 0] end]
	set it ""
	if { [regexp {\(C[0-9]+\) $} $it ff] } {
	    regexp "\{plot\[d23]\[df].*\}" $ff it
#	set it $ff
	}
    }
    if { [info exists plotPending] } {
	#puts "plotPending=<$plotPending>,it=<$it>"
	append plotPending $it
	set it ""
	if { [regexp -indices "\n\\(D\[0-9\]+\\)" $plotPending  inds] } {
	    set it [string range $plotPending [lindex $inds 0] end]
	    set plotPending [string range $plotPending 0 [lindex $inds 0]]
	    set data $plotPending
	    unset plotPending
	    #puts itplot=<$it>,$inds
	    #puts plotdata=<$data>
	    doShowPlot $win $data

	}
    }

    $win insert end $it "output"
    $win mark set  lastStart "end -1char"
    if { [regexp {\(C[0-9]+\) $|\(dbm:[0-9]+\) $|([A-Z]+>[>]*)$} $it junk lisp]  } {
  #puts "junk=$junk, lisp=$lisp,[expr { 0 == [string compare $lisp {}] }]"
	 #puts "it=<$it>,pdata={[array get pdata *]},[$win index end],[$win index insert]"

	if { [info exists pdata($sock,wait) ] && $pdata($sock,wait) > 0 } {
	    #puts "it=<$it>,begin=$pdata($sock,begin),end=[$win index {end linestart}]"
	    #puts dump=[$win dump -all "insert -3 lines" end]
	    setAct pdata($sock,result) [$win get $pdata($sock,begin) "end -1char linestart" ]
	    #puts result=$pdata($sock,result)
	    set pdata($sock,wait) 0
	 }
	$win mark set lastStart "end -1char"
	$win tag add  input "end -1char" end
	oset $win atMaximaPrompt [expr { 0 == [string compare $lisp ""] }]
	
    }
    $win see end
    return
}

proc littleFilter {win sock } {
    global pdata
    set tem [gets $sock]
    append pdata(maximaInit,$sock) $tem
    debugsend "littlefilter got:<$tem>"
    if { [regexp {pid=([---0-9]+)} $tem junk pid] } {
	fileevent $sock readable ""
	oset $win pid $pid
	oset $win socket $sock
    }
}

if { ![info exists ws_openMath(timeout)] } {

    set ws_openMath(timeout) 20000
}

proc runOneMaxima { win } {
    global ws_openMath
    closeMaxima $win
    linkLocal $win pid
    set pid -1
    openMaxima $win littleFilter
    while { $pid == -1 } {
	set af [after $ws_openMath(timeout) oset $win pid -1 ]
	# puts "waiting pid=$pid"
	vwait [oloc $win pid]
	after cancel $af
	if { $pid  == -1 } {
	    if { "[info  command console]" != "" } { console show }
	    if { [tk_dialog .jim ask {Starting maxima timed out.  Wait longer?} \
		    {} yes  no yes ] } {
		list
	    } else {
		closeMaxima $win
		set err   "runOneMaxima timed out"
		global pdata
		if { [info exists pdata(maximaInit,[oget $win socket])] } {
		    append err : $pdata(maximaInit,[oget $win socket])
		}
		error $err
	    }
	}
    }
    set res [list [oget $win pid] [oget $win socket] ]
    set sock [oget $win socket]
    global pdata
    set pdata(maxima,socket) $sock
    fileevent $sock readable  "maximaFilter $win $sock"
    return $res

}

proc sendMaxima { win form } {
    linkLocal $win maximaSocket
    if { ![regexp "\[\$;\]\[ \t\n\r\]*\$" $form ] } {
	# append form ";"
    }
    puts -nonewline $maximaSocket $form
    flush $maximaSocket
}


proc sendMaximaWait { win form {timeout 20000 }} {
    linkLocal $win maximaWait

    set form [string trimright $form "\n \t\r"]

    if { ![regexp "\[\$;\]|^\[ \t]*:" $form ] } {
	 append form ";"
    }
    sendMaximaCall $win $form\n [list oset $win maximaWait 1]
    set maximaWait -1
    set af [after $timeout oset $win maximaWait -1]
    vwait [oloc $win maximaWait]
    after cancel $af
    if { $maximaWait > 0 } {
	global pdata
	return [trim_maxima $pdata([oget $win maximaSocket],result)]
    } else {
	error "sendMaximaWait $form timed out"
    }
}



#
 #-----------------------------------------------------------------
 #
 # sendMaximaCall --  send FORM to maxima process in WIN
 # and when it gets the result have it execute CALL
 #
 #  Results: none
 #
 #  Side Effects: maxima executes form and then call may
 #  do something like insert it somewhere in a buffer.
 #
 #  # todo: should probably make it so this guy looks at maxima c, d numbers
 #    and matches results ..
 #----------------------------------------------------------------
#
proc sendMaximaCall { win form call } {
    linkLocal $win maximaSocket
    global pdata
    set begin [$win index lastStart]
    if { [regexp {C([0-9]+)} [$win get "$begin linestart" $begin] junk \
	    counter ] } {
	#	set af [after 5000 set pdata($maximaSocket,wait) -1]
	set pdata($maximaSocket,wait) 1
	
	set pdata($maximaSocket,begin) $begin
    } else {
	catch { unset pdata($maximaSocket,wait) }
    }
    puts -nonewline $maximaSocket $form
    flush $maximaSocket
    if { [info exists counter] } {
	setAction pdata($maximaSocket,result) $call
    }
}

proc setAction { var action } {
    global _actions
    set _actions($var) $action
}

proc setAct { var val } {
    global _actions
    uplevel #0 set $var [list $val]
    if { [info exists _actions($var)] } {
	uplevel #0 $_actions($var)
	unset _actions($var)
    }
}

proc CMresetFilter { win } {
    set sock [oget $win maximaSocket]
    fileevent $sock readable "maximaFilter $win $sock"
}

proc CMkill {  signal pid } {
    global ws_openMath
    if { $pid > 0 } {
	if { [info command "winkill"] == "winkill" } {
	    winkill -pid $pid -signal $signal
	} else {
	    exec $ws_openMath(kill) $signal $pid
	}
    }
}

proc CMinterrupt { win } {
    global ws_openMath
    CMkill   -INT [oget $win pid]
    CMresetFilter $win
}


proc doShowPlot { w data } {
        global xHMpreferences
	#puts data=$data
    set name [plotWindowName $w]
    set command [lindex [lindex $data 0] 0]
    if { "$command" == "plotdf" } {
	set command [lindex $data 0]
    } else {
	lappend command -data [lindex $data 0]
    }
    lappend command -windowname $name
#	puts $command
	eval $command
#	return
	set e [$w index end]
	set view [ShowPlotWindow $w $name  "$e $e" "$e $e"  ""]
	if { "$view" == "" } { return }
	append view " -1 line"
	set tem [$w dump -window $view end]
	global billy
	set billy $tem
	if { [llength $tem] == 3 } {
	    after 80 $w see [lindex $tem 2]
	    #after 400 $w see [lindex $tem 2]
	    #puts "	    after 400 $w see [lindex $tem 2]"
	}
    }


proc dblDisplayFrame { location line } {
    OpenMathOpenUrl $location
    set panel [omPanel .]
    set w [oget $panel textwin]
    $w tag remove currentLine 0.0 end
    $w tag add currentLine "$line.0" "$line.0 lineend"
    $w tag config currentLine -foreground red
    set beg [lindex [split [$w index "@0,0"] .] 0]
    set end [lindex [split [$w index "@0,3000"] .] 0]
    # puts "line=$line,beg=$beg,end=$end"
    if { "$beg" != "" &&  ( $line < $beg + 3 || $line > $end - 3) } {
	$w yview [expr $line - 3]
    }
    $w see $line.0
}
	


#
 #-----------------------------------------------------------------
 # required:
 #
 # trim_maxima --  takes STRING and trims off the prompt
 # and trailing space if desired.   Usually single line results
 # have their white space completely trimmed, while multiline
 # results will be left so that they display properly from left margin
 #
 #  Results:  a string with white space trimmed off
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#
proc trim_maxima { string } {
    debugsend "in trim_maxima input=<$string>"
    if { [string first \n $string] == 0 } {
	set string [string range $string 1 end]
    }
    if { [regexp -indices "(^|\n)(\\(D\[0-9\]+\\))" $string all junk inds] } {
        set len [expr {[lindex $inds 1]  - [lindex $inds 0] }]
        set repl [genword " " $len]
        set ans [string range $string 0 [expr {[lindex $inds 0 ] -1}]]
        append ans $repl
        append ans [string range $string  [expr {[lindex $inds 1 ] +1}] end ]
	debugsend "in trim_maxima ans=<$ans>"
	set string [trimSpace $ans]

    }
    return $string
}

proc dshow { args  } {
    foreach v $args { append ans $v=[uplevel 1 set $v], }
    puts $ans
}
proc maxima_insert { w this next val args } {
    catch {
    set res [uplevel #0 set $val]
    }
    catch {
    insertResult_maxima $w $this $next [trim_maxima $res]
    }
}

proc eval_maxima { prog win this nextResult } {
    global ws_openMath
    set w $ws_openMath(maximaWindow)
    linkLocal $w maximaSocket
    set form [string trimright [eval $win get $this] " \t\n;$"]
    set form [addPreloads $form maxima $win $this]
    if { "[lindex $nextResult 0]" != "" } {
	sendMaximaCall $w "$form;\n" [list maxima_insert $win $this  $nextResult pdata($maximaSocket,result)]
	
#         set res [sendMaximaWait $ws_openMath(maximaWindow) "$form;"]
#	insertResult_maxima $win $this  $nextResult $res
    } else {
	sendMaxima $ws_openMath(maximaWindow) "$form;\n"
    }
    return 0
}





proc changeSize { win  y } {
    set del 0
    set tem [expr { [winfo rooty $win] + [winfo height $win] } ]
    set del [expr {abs($y-$tem) <20 ? 0: $y-$tem < 0 ? -1 : 1 }]
    if { $del } {
	set h [$win cget -height]
	incr h $del
	if { $h >= 1 } {
	    $win config -height $h
	}
    }

}
