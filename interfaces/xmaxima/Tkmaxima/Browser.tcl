# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Browser.tcl,v 1.4 2002-09-08 01:48:26 mikeclarkson Exp $
#
###### browser.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

global MathServer
set MathServer "locahost 4443"
# help keysyms
# bind .jim <Key> "puts {%A %K}"
# to print them out

## source keyb.tcl

###### keyb.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

proc peekLastCommand {win} {
    global ws_openMath
    if { [info exists ws_openMath(lastcom,$win)] } {
	return $ws_openMath(lastcom,$win)
    }
}

proc pushCommand { win command arglist } {
    global ws_openMath
    set ws_openMath(lastcom,$win) [list $command $arglist]
}



global ws_openMath
set ws_openMath(sticky) "^Teval$|^program:"


#
 #-----------------------------------------------------------------
 #
 # tkTextInsert --  we add some things to the default tkTextInsert
 #  so that tags present before or after the insert, which are sticky
 #  are added to the inserted string.   As usual, ones on both sides
 #  are added.
 #
 #  Results:
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#

proc tkTextInsert { w s } {
    global ws_openMath
    set after [$w tag names insert]
    set before [$w tag names "insert-1char"]
    set both [intersect $after $before]
   # puts "after=$after"
   # puts "before=$before"

    foreach v [concat $after $before] {
	if { [regexp $ws_openMath(sticky) $v] } {
	    lappend both $v
	}
    }

    if { [info exists ws_openMath($w,inputTag) ] } {
	lappend both $ws_openMath($w,inputTag)
    }

    if {($s == "") || ([$w cget -state] == "disabled")} {
	return
    }
    catch {
	if {[$w compare sel.first <= insert]
		&& [$w compare sel.last >= insert]} {
	    $w delete sel.first sel.last
	}
    }
    $w insert insert $s $both
    $w see insert

}
proc getRange { win a b }  {
    if { [$win compare $a < $b ] } { return "$a $b" } else { return "$b $a"}
}


#
 #-----------------------------------------------------------------
 #
 # binding --   push the current selection on the killRing, and
 # if there is no selection, push the region between the anchor and
 # the point.
 #  Results:
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#
bind OpenMathText <Control-Key-w> {
    pushCommand %W OpenMathTextCut ""
    # in the first case the <<Cut>> event on Text will delete the selection.
    if { [catch { pushl [saveText %W sel.first sel.last] killRing } ] } {
	catch {
	    set range [getRange %W anchor insert]
	    pushl [eval saveText %W $range] killRing
	    eval %W delete $range
	}
    }
}


if { [catch { set ws_openMath(bindings_added) } ] } {
    bind Text <Control-Key-k> "openMathControlK %W \n [bind Text <Control-Key-k>]"
    bind Text <B3-Motion> [bind Text <B2-Motion>]
    bind Text <Button-3> [bind Text <Button-2>]

  set ws_openMath(bindings_added) 1
}

global ws_openMath
set ws_openMath(doublek) 0

bind OpenMathText <Control-Key-k><Control-Key-k> {
    set ws_openMath(doublek) 1
}

proc openMathControlK { win } {
    global ws_openMath
    if { $ws_openMath(doublek) != 0 } {
	set now [popl killRing ""]
    } else { set now "" }
    set ws_openMath(doublek) 0
    if { [$win compare insert == "insert lineend" ]  } {
	if { [$win compare insert < end] } {
	    append now "\nTins {[ldelete sel [$win tag names insert]]} {\n}"
    } } else {
	append now "\n[saveText $win insert {insert lineend}]"
    }
    pushl $now killRing
}

bind OpenMathText <Control-Key-y> "OpenMathYank %W 0; break"
bind OpenMathText <Alt-Key-y> "OpenMathYank %W 1; break"
bind OpenMathText <Meta-Key-y> "OpenMathYank %W 1; break"

proc OpenMathYank {win level } {
    global ws_openMath
    #puts "doing OpenMathYank $win $level"
    if { $level == 0 } {
	set ws_openMath(currentwin) $win
	pushCommand $win OpenMathYank [list $win $level]
	set ws_openMath(point) insert
	$win mark set beforeyank insert
	$win mark gravity beforeyank left
	eval [peekl killRing "" ]
    } else {
	if { [catch {
	    set last $ws_openMath(lastcom,$win)
	    set m [lindex [lindex $last 1] 1]
	    incr m
	    if { "[lindex $last 0]" == "OpenMathYank" &&
	    "$ws_openMath(currentwin)" == "$win"
	    && "$ws_openMath(point)" == "insert" } {set doit 1}} ]
	    || $doit==0} {
		    pushCommand $win Error "" } else {
			
			set res [peekl killRing _none_ [expr {$m + 1}]]
		if { "$res" == "_none_" } {
		    # this will cause to cycle
		    set m 0
		} else {
		    $win delete beforeyank insert
		    eval $res
		}
                pushCommand $win OpenMathYank [list $win $m]
	    }
    }
    catch { $win see insert}
}

# put the clipboard paste on Control-Shift-y
event add <<Paste>> <Control-Shift-y>

bind OpenMathText <Alt-Key-w> {
    pushCommand %W SaveSelection ""
    if { "[selection own -displayof %W]" == "%W"} {
    pushl [saveText %W sel.first sel.last] killRing
    selection clear -displayof %W
}
		}

bind OpenMathText <Key> {openMathAnyKey %W %K %A}
bind OpenMathText <Alt-Key> {openMathAnyKey %W %K ALT_%A}

# stop the double button click word selection in openMathText..
bind OpenMathText <Double-Button-1> { break; }
bind OpenMathText <Control-c><Key-e> {doInvoke %W insert ; break; }

bind OpenMathText <Control-Key-space> {
    pushCommand %W SetAnchor ""
    %W mark set anchor insert }


proc openMathAnyKey { win keysym s  } {
   # puts "$win `$keysym' `$s'"
    if { "$s" != "" } {
	pushCommand $win openMathAnyKey [list $win  $keysym $s]
    }

    if { "$s" != "" && [doInsertp [$win tag names insert]]
      && ("$s" == "$keysym"  || [regexp  "\[\n\t \]" "$s" junk] )} {
	setModifiedFlag $win insert
    }
}

proc saveText { win args } {
    set tags [ldelete sel  [$win tag names]]
    set prev [lindex $args 0]
    set endregion [$win index [lindex $args 1 ]]
    if { "$prev" == "" } {set prev 0.0 }
    if { "$endregion" == "" } {set endregion end}
    set allar($prev) 1
    set allar($endregion) 1
    foreach v $tags {
	set ranges [tagRanges $win $v  $prev $endregion]
	foreach {begin end} $ranges {
	    lappend start($begin) $v
	    lappend stop($end) $v
	    set allar($begin) 1
	    set allar($end) 1
	
	}
    }
   proc __comp { a b} " return  \[$win compare \$a > \$b \] "
   set all [lsort -command __comp [array names allar]]
   set result ""
   foreach v $all {
       append result "Tins [list [array names currentTags]] [quoteBraces [$win get $prev $v]]\n"
       set prev $v


       if { [info exists start($v)] } {

	   foreach u $start($v) { set currentTags($u) 1}
       }

       if { [info exists stop($v)] } {

	   foreach u $stop($v) { unset currentTags($u) }
       }



       #puts -nonewline "..deleting{$stop($v)} giving {$currentTags}"

    # puts ">>"

      }
      return $result
}



#
 #-----------------------------------------------------------------
 #
 # tagRanges --  find ranges on WINDOW for TAG from FROMINDEX below TOINDEX
 #
 #  Results: a list of ranges start1 stop1 start2 stop2 ..
 # which are contained in [fromindex,toindex] such that TAG is on from
 # start1 to stop1 etc.
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#
proc tagRanges { win tag begin end } {
    if {  [$win  compare $begin <= 1.0 ]  &&
    [$win  compare $end >= end ] } {
	return [$win tag ranges $tag ] } else {
	    set answer ""
	    set begin [$win index $begin]
	    set end [$win index $end]
	    if { [lsearch [$win tag names $begin] $tag ]>=0 } {
		set prev [$win tag prevrange $tag $begin+1chars]
		set to [lindex $prev 1]
		if { [$win compare $to > $end ] } {
		    set to $end
		}
		append answer "$begin $to "
		set begin $to
	    }
	    #puts "<$begin $end>"
	    while { [$win compare $begin < $end ] } {
		set next [$win tag nextrange $tag $begin]
		#puts "next=$next"
		if { "$next" == "" } { return $answer }
		if { [$win compare [lindex $next 1] <= $end]} {
		    append answer "$next "
		    set begin [lindex $next 1]
		} elseif {[$win compare [lindex $next 0] < $end ]} {
		    append answer "[lindex $next 0] $end"
		    return $answer
		} else {
		    return $answer 
		}
	    }
	    return $answer
	
	}
}
	



#
 #-----------------------------------------------------------------
 #
 # quoteBraces --  given a STRING such that
 # puts $file "set new [quoteBraces $string]"
 # when re read by eval would make value of NEW identical to STRING
 #
 #  Results: a string
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#
proc quoteBraces {string } {
    regsub -all {[{}]} $string {\\&} val
    return [list $val]
}

proc thisRange { win tag index } {
    set prev [$win tag prevrange $tag $index]
    if { "$prev" != "" && [$win compare [lindex $prev 1] >= $index] } {
	return $prev
    }
    set next [$win tag nextrange $tag $index]
    if { "$next" != ""  && [$win compare [lindex $next 0] <= $index] } {
	return $next
    }
    return ""
}




#
 #-----------------------------------------------------------------
 #
 # insertRichText --  insert rich text in TEXTWINDOW at INDEX according
 # to commands and data in LIST.   The latter must be of the form
 #  command1 arg1 ..argn command2 arg1 ..argn2 ..
 # for example if `Tins' takes two args
 #  and the commands must be in
 # since the rich text might come from a selection or some or an untrusted
 # file we want to be careful not to do any bad evals.
 #  Results: none
 #
 #  Side Effects:  the rich text commands are invoked to do insertions
 # on the window.
 #
 #----------------------------------------------------------------
#
proc insertRichText {win index list } {
    global ws_openMath
    set ws_openMath(currentwin) $win
    set ws_openMath(point) $index
    foreach v $ws_openMath(richTextCommands) {
	set ws_openMath($v,richTextCommand) [llength [info args $v]]
    }
    set i 0
    set ll [llength $list]
    while { $i < $ll } {
	set com [lindex $list $i]
	incr i
	if { [catch { set n $ws_openMath($com,richTextCommand)} ] } {
	    return -code error -errorinfo "illegal command in rich text:$com"
	}
	set form [concat $com [lrange $list $i [expr {$i +$n -1}]]]
	if { [catch {eval $form } ] } {
	    return -code error -errorinfo "unable to evaluate command:`$form' " }
	
	incr i $n
    }
}


proc Tins { tags text } {
   global ws_openMath
   # foreach v $args { append text $v }
   $ws_openMath(currentwin) insert $ws_openMath(point) $text  $tags
}

proc TinsSlashEnd { tags text } {
   global ws_openMath
   # foreach v $args { append text $v }
   $ws_openMath(currentwin) insert $ws_openMath(point) "$text\\"  $tags
}



global ws_openMath
set ws_openMath(richTextCommands) {Tins TinsSlashEnd}

## endsource keyb.tcl

proc underTop {top win} {
    if { "$top" == "." } { 
	return $win
    } else {
	return $top$win
    }
}

proc showHistory { window } {
    set top [winfo toplevel $window]
    set win [omPanel $window]
    makeLocal $win history historyIndex
    set w [underTop $top .historylist]
    catch {destroy $w}
    frame $w -borderwidth 2 -relief raised
    label $w.title -text "History List" -relief raised
    setHelp $w.title {This window may be dragged elsewhere by grabbing this title bar with the mouse.   Double clicking on a history item, moves to that page.}
    button $w.dismiss -command "destroy $w" -text dimsiss
    setHelp $w.dismiss {Remove the history list}
    pack $w.title $w.dismiss -side top -expand 1 -fill x
    scrollbar $w.scrolly -command "$w.list yview"
    scrollbar $w.scrollx -orient horizontal -command "$w.list xview"
    pack $w.scrollx -side bottom -fill x -expand 1
    pack $w.scrolly -side right -fill y -expand 1
    listbox $w.list -yscroll "$w.scrolly set" \
	-width 35 -height 16 -setgrid 1 -xscroll "$w.scrollx set"
    $w.title configure -font [$w.list cget -font]
    set l $w.list

    pack $w.list  -side top -fill both -expand 1
    resetHistory $win $w.list junk history
    global [oarray $win]

    #puts "    trace variable [oloc $win history] w {resetHistory $win $w.list}"
    trace vdelete  [oloc $win history] w "resetHistory $win $w.list"
    trace variable [oloc $win history] w "resetHistory $win $w.list"
    trace vdelete [oloc $win historyIndex] w "resetHistory $win $w.list"
    trace variable [oloc $win historyIndex] w "resetHistory $win $w.list"
    bind $l <Double-1> {OpenMathMoveHistory [omPanel %W] [expr [%W index @%x,%y]-[oget [omPanel %W] historyIndex]]}
    bind  $w.title <B1-Motion> "dragPlacedWindow $w %W %X %Y"
    bind  $w.title <1> "startDragPlacedWindow $w %X %Y"
    place $w -relx .4 -rely .8 -in $top

}

proc deleteAllTraces {var} {
    foreach v [uplevel "#0" trace vinfo $var] {
	uplevel "#0" trace vdelete $var [lindex $v 0] [list [lindex $v 1]]
    }
}
proc resetHistory { win list args } {
    set action [lindex $args 1]
    if { [catch {
	if { "$action" == "history" } {
	    $list delete 0 end
	    if { [winfo exists $list] } {
		foreach v [oget $win history] {
		    $list insert end [oget $v location]
		}
	    }
	}
	$list selection clear 0 end
	$list selection set [oget $win historyIndex]
	after 200 raise [winfo parent $list]

    } ] } {
	deleteAllTraces [oloc $win history]
	deleteAllTraces [oloc $win historyIndex]
    }
}


proc startDragPlacedWindow { win x y } {
    oset $win placeinfo [list $x $y [place info $win]]
}

proc dragPlacedWindow { win w1 x y } {
    global me recursive
    makeLocal $win placeinfo
    catch { after cancel [oget $win after]}
    set me [oget $win placeinfo]
    #puts "have=[oget $win placeinfo]"
    desetq "px py pinfo" [oget $win placeinfo]
    set dx [expr {$x - $px}]
    set dy [expr {$y - $py}]
    set nx [expr {$dx + [assoc -x $pinfo]}]
    set ny [expr {$dy + [assoc -y $pinfo]}]
    set new "-x $nx -y $ny"
    eval place $win $new
    oset $win placeinfo [list $x $y $new]
}

proc OpenMathMoveHistory { win  n } {
    makeLocal $win history historyIndex
    incr historyIndex $n
    if { $historyIndex >= [llength $history] } {
	set historyIndex  [expr {[llength $history] -1}] }
    if { $historyIndex <0 } { set historyIndex 0}
    if { "[lindex $history $historyIndex]" != ""} {
	OpenMathGetWindow $win [lindex $history $historyIndex]
	oset $win historyIndex $historyIndex
    }
}

proc toLocalFilename { url } {
    set type [assoc type $url]
    switch $type {
	http {
	    return [assoc filename $url]
	}
	file {
	    return [file join / [assoc dirname $url] [assoc filename $url] ]

	}
	default "unknown type: $type"
    }

}

proc OpenMathGetWindow { commandPanel win } {
    if { "[winfo parent [oget $commandPanel textwin]]" != "$win" } {
    catch { pack forget [winfo parent [oget $commandPanel textwin]] }
    pack $win -expand 1 -fill both
   # pack $win
    oset $commandPanel textwin $win.text
    oset $commandPanel location [oget $win location]
    set tem [toLocalFilename [decodeURL [oget $win location]]]
    oset $commandPanel savefilename  [file root $tem].txt
}
		}


proc getw { s  } { eval pack forget [winfo children . ] ; pack $s}

proc try1 { file } {
   global ccc
   eval pack forget [winfo children . ]
   mkOpenMath [set w .t[incr ccc]]
   uplevel "#0" source $file
  }

proc filesplit { x } {
    set l [split $x /]
    set n [llength $l ]
    set dir [lrange $l 0 [expr {$n - 2}]]
    set file [lindex $l [expr {$n - 1}]]
    return [list [join $dir /] $file]
}



proc decodeURL { name } {
    set server ""
    if { [regexp  {([^#]*)#(.*)$} $name junk name anchor] } {
	lappend answer anchor $anchor
	   # puts "answer=$answer"
    }


    if { [regexp {^([a-z]+)[(]?([0-9]*)[)]?:/([^ ]+)$} $name all type port path ] } {
	lappend answer type $type
    } else { set path $name ; set type ""
    }

    set path [removeDotDot $path]
    #puts "path=$path"
    desetq "dirname filename" [filesplit $path]
    #puts "dirname=$dirname,path=$path,filename=$filename"
    set po [assoc $type {http 80 nmtp 4443} ]
    if { "$po" != "" } {
	if { "$port" == "" } {set port $po }

	if { [regexp {^/([^/:]*)(:([0-9]+))?(.*)$} $dirname all server \
		jun po dirname] } {
	   # puts "hi ther,server=$server"
	    if { "$po" != ""} {set port $po}
	    if { "$dirname" == "" } {set dirname / }
	} elseif { "$server" == "" } {
	    set server $filename
	    set dirname /
	    set filename {}
	}
	lappend answer port $port server $server
    }
    lappend answer dirname $dirname filename $filename
    return $answer
}

proc removeDotDot { path } {
    while { [regsub  {/[^/]+/[.][.](/|$)} $path "\\1" path] } {list}
    return $path
}

proc appendSeparate { var before item separator } {
    if { "$item" != "" } {
	uplevel 1 append $var $before $item $separator
}
		}

proc dirnamePlusFilename { lis } {
  return  [string trimright [assoc dirname $lis ""] /]/[assoc filename $lis ""]
}
proc encodeURL { lis } {
    set type [assoc type $lis ""]
    switch $type {
	nmtp {
	      if { [ set port [assoc port $lis 4443]] != 4443 } {
	       append type "($port)"
	   }
   	   appendSeparate ans "" $type ://[assoc server $lis ""]
	   append ans [dirnamePlusFilename $lis]
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
	}
	http  {
	   if { [ set port [assoc port $lis 80]] != 80 } {
	       append type "($port)"
	   }
	   appendSeparate ans "" $type ://[assoc server $lis ""]
	   append ans [dirnamePlusFilename $lis]
	   #appendSeparate ans "" [assoc dirname $lis ""]
	   #appendSeparate ans "/" [assoc filename $lis ""] ""
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
       }
       file {
	   appendSeparate ans "" $type :/
	   append ans  [dirnamePlusFilename $lis]
#	   appendSeparate ans "" [assoc dirname $lis ""] "/"
#	   appendSeparate ans "" [assoc filename $lis ""] ""
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
       }
       default "error unsupported url type : $type"
   }
   return $ans
}

proc resolveURL { name current {post ""} } {
    set decode [decodeURL $name]
    #puts "name=$name,current=$current"
    set ans ""
    set relative 0
    if { "[assoc type $decode {} ]" == "" } {set relative 1}
    if { $relative == 0 } {
	set ans  $decode
    } else {
	foreach {x y } $current {
	    switch $x {
		dirname {
		    set ndir [assoc dirname $decode ""]
		    set cdir [assoc dirname $current ""]
		    if { [string match /* $ndir] } {
			set new $ndir
		    } elseif { "$ndir" != "" } {
			if { "$cdir" != ""  } {
			    set new [string trimright $cdir /]/$ndir
			} else { set new $ndir }
		    } else {
			set new $cdir
		    }
		    lappend ans dirname [removeDotDot $new]
		}
		filename {
		
		    if { "[assoc filename $decode]" == "" && "[assoc anchor $decode]" != "" } {
			lappend ans $x $y
		    }
		}
		post { list }
		default {
		    lappend ans $x  [assoc $x $decode $y]
		}
	    }
	}
	foreach { key val } $decode {
	    if { "[assoc $key $ans --none--]" == "--none--" } {
		lappend ans $key $val
	    }
	}
	

    }
    if { "$post" != "" } {
	set ans [putassoc post $ans $post]
    }
    return $ans
}

global ws_openMath
set ws_openMath(urlHandlers) {
    text/html  netmath
    text/plain netmath
    image/gif  netmath
    application/postscript "ghostview -safer %s"
    application/pdf "acroread %s"
    application/x-dvi "xdvi %s"
}

proc getURLrequest { path server port types {post ""} {meth ""} } {
    global ws_openMath

    if { "$meth" != "" } {set method $meth } else {
	
	set method GET
	if { "$post" != "" } {set method POST}
    }

    #puts "getURLrequest $path $server $port [list $types]"
    foreach {v handler}  $ws_openMath(urlHandlers) {
	lappend types $v,
    }

    set ans "$method $path HTTP/1.0\nConnection: Keep-Alive\nUser-agent: netmath\nHost: $server:$port\nAccept: $types\n"
    if { "$post" != "" } {
	# append ans "Content-length: [string length $post]\n\n$post"
	append ans "Content-type: application/x-www-form-urlencoded\nContent-length: [string length $post]\n\n$post"
    }

	return $ans

}

proc canonicalizeContentType { type } {
    regexp -nocase {([---a-zA-Z]+)/([---a-zA-Z]+)} $type type
    return [string tolower $type]
}

proc getURL { resolved type {mimeheader ""} {post ""} } {
    global ws_openMath
    set res $resolved

    set ans ""
    set method ""
    if { "$mimeheader" != ""} {
	uplevel 1 set $mimeheader \[list\]
    }
    uplevel 1 set $type "unknown"


    #puts "getting $resolved,post=<$post>"
    switch [assoc type $res] {
	http {
	   # puts $res
	   # puts "socket [assoc server $res] [assoc port $res 80]"
	    if { [info exists ws_openMath(proxy,http) ] } {
		set sock [eval socket $ws_openMath(proxy,http)]
#		puts "opening proxy request socket $ws_openMath(proxy,http)"
	    } else {
	    set sock [socket [assoc server $res] [assoc port $res 80]]
	    }
	
	    fconfigure $sock -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    #puts request=[getURLrequest [dirnamePlusFilename $res] [assoc server $res] [assoc port $res] image/gif $post]
#	    set path [dirnamePlusFilename $res]
	    set path [encodeURL $res]
	    set server [assoc server $res]
	    set port  [assoc port $res]
	    puts $sock [getURLrequest $path $server $port image/gif $post]
	    if { "$post" == "" } {
		oset $sock cachename "http://$server:$port$path"
	    } else { oset $sock cachename "" }
	    flush $sock
	    if { [readAllData $sock -tovar ws_openMath(url_result) \
		    -translation binary -mimeheader ws_openMath(mimeheader)  \
		    -timeout 120000 -chunksize 2024] > 0 } {
		
	    #puts "length=[string length $ws_openMath(url_result)]"
	    #	flush stdout
		
		set contentType [canonicalizeContentType [assoc content-type $ws_openMath(mimeheader) text/plain]]
		uplevel 1 set $type [list $contentType]
		if { "$mimeheader" != "" } {
		    uplevel 1 set $mimeheader \[ uplevel "#0" set ws_openMath(mimeheader) \]
		}
		set ans $ws_openMath(url_result)
		unset ws_openMath(url_result)
		return $ans
	    } else {return "had error"
	    }
	}
	file {
	    set name [toLocalFilename $res]
	    set fi [open $name r]
	    set answer [read $fi]
	    if { [regexp {[.]html?$} $name ] || [regexp -nocase "^(\[ \n\t\r\])*<html>" $answer] } {
		set contentType text/html
	    } elseif {  [regexp {[.]gif([^/]*)$} $name ] } {
		set contentType image/gif
	    } else { set contentType text/plain }
	    uplevel 1 set $type $contentType

	    close $fi
	    return $answer
	}
	default {
	    error "not supported [lindex $res 0]"
	}
    }
}




proc getImage { resolved width height} {
    global ws_openMath
    set res $resolved
    #puts [list getImage [list $resolved] $width $height]
    set ans ""
    catch {
	if { "" != "[image type $ws_openMath(image,$res,$width,$height)]" } {
	    set ans $ws_openMath(image,$res,$width,$height)
	}
    }
    if { "$ans" != "" } { return $ans }

    set image [image create photo -width $width -height $height]
    after 10 backgroundGetImage $image [list $resolved] $width $height
    set ws_openMath(image,$res,$width,$height) $image
    return $image
    }


global ws_openMath
set ws_openMath(imagecounter) 0

set ws_openMath(brokenimage,data) R0lGODlhHQAgAMIAAAAAAP9jMcbGxoSEhP///zExY/9jzgCEACH5BAEAAAIALAAAAAAdACAAAAPOOLrcLjDCQaq9+CoZaf7YIIicx50nNZYV6k4tCRPuYduSR8vmef+dy2rU4vyOM8uqJzkCBYCoNEqkGZ04SGHLBSiKTewhx/AyI+LxqWIGh5Eo9pdm8D3jhDa9/nrJTQaBfS5/LYGCgxyFe4cnAY+Qj1oFegKHjRKRkpMbgJeIEJqTBTyGnxybAlwbQYygKFusOaavo5SkJ5WYErELKAO6fBy4LxS6vFzEv4snpLIpIszIMiWKeXMWvS7RGXoVsX0g11NR1Bzk6F4jCn0ODgkAOwAA

proc backgroundGetImage  { image res width height }   {
    global ws_openMath
    #puts [list backgroundGetImage  $image $res $width $height ]
    if { [catch { backgroundGetImage1 $image $res $width $height } err ] } {
	if { ![info exists ws_openMath(brokenimage)] } {
	    set ws_openMath(brokenimage) [image create photo -data $ws_openMath(brokenimage,data)]
	}
	 #puts "got error $err, doing $image copy $ws_openMath(brokenimage)"
	set im $ws_openMath(brokenimage)
	$image config -width [image width $im] -height [image height $im]
	$image copy $im
    }
}


proc backgroundGetImage1  { image res width height }   {
    #puts  "resolved=$res"
    global ws_openMath
    #puts [list backgroundGetImage $image $res $width $height]
    switch [assoc type $res] {
	http {
	    set server [assoc server $res]
	    set port [assoc port $res 80]
	    if { [info exists ws_openMath(proxy,http) ] } {
		set s [eval socket $ws_openMath(proxy,http)]
#		puts "opening proxy request socket $ws_openMath(proxy,http)"
	    } else {
	    set s [socket [assoc server $res] [assoc port $res 80]]
	    }
	    fconfigure $s -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    puts $s [getURLrequest [encodeURL $res] \
		    $server $port {image/gif image/x-bitmap}]
	    flush $s

	    if { [regexp -nocase {[.]gif([^/]*)$} [assoc filename $res] ] } {
		fconfigure $s -translation binary
		set tmp xxtmp[incr ws_openMath(imagecounter)].gif

		if { [info exists ws_openMath(inbrowser)] ||  [catch {set out [open $tmp w] } ] } {
		    # if have binary..
		    if { "[info command binary]" != "binary" } {
			error "need version of tk with 'binary' command for images"}
			#puts "hi binary" ; flush stdout
			if {  [readAllData $s -tovar \
				ws_openMath($s,url_result) -mimeheader \
				ws_openMath($s,mimeheader)
			] > 0  && [string match *gif [assoc content-type $ws_openMath($s,mimeheader)]] } {
			    set ans $image
			    $image configure -data [tobase64 $ws_openMath($s,url_result)]

			    unset ws_openMath($s,mimeheader)
			    unset ws_openMath($s,url_result)
			
			} else  {
			    error "could not get image"
			}
		    } else {
			fconfigure $out -translation binary -blocking 0
			if { [readAllData $s -tochannel $out \
				-translation binary \
				-mimeheader \
				ws_openMath($s,mimeheader) -timeout 15000 -chunksize 2024 ] > 0 } {
			    set ans $image
			    $image config  -file \
				    $tmp
			    unset ws_openMath($s,mimeheader)
			}

			
			
			# all the below just to try to remove the file..
			#  depending on versions and in environments..
			
		}
		}
	    }
	    file {
		$image config -file [toLocalFilename $res]
		set ans $image
		# puts "$image config -file [toLocalFilename $res]"
		#set ans [image create photo -file [toLocalFilename $res]]
		
	
	}
	    default { error "unknown type of image" }
	}
	## if we opened an out channel try hard to remove the tmp file.
	if { [info exists out] &&
	     [catch { file delete $tmp } ] && [catch { rm $tmp }]
	 && [catch { exec rm $tmp }] } {
			    puts "cant remove tmp file $tmp"
	 }
	 if { "$ans" == "" } {
	     error "Unable to open an image for [encodeURL $res]"
	 }

    }


#
#-----------------------------------------------------------------
#
# readData --  read data from S, storing the result
# in ws_openMath($s,url_result).   It times out after TIMEOUT without any data coming.
# it can be aborted by setting set ws_openMath($s,done)  -1
#
#
#  Results: -1 on failure and 1 on success.
#
#  Side Effects: it initially  empties ws_openMath($s,url_result) and then
#  adds data to it as read.   ws_openMath($s,done) is initialized to 0
#
#----------------------------------------------------------------
#
proc readData { s { timeout 10000 }} {
    global ws_openMath

    after $timeout "set ws_openMath($s,done) -1"
    fconfigure $s  -blocking 0
    set ws_openMath($s,done) 0
    set ws_openMath($s,url_result) ""

    #mike FIXME: this is a wrong use of after cancel
    fileevent $s readable \
	   "after cancel {set ws_openMath($s,done) -1} ; after $timeout {set ws_openMath($s,done) -1} ; set da \[read $s 8000] ; append ws_openMath($s,url_result) \$da; if { \[string length \$da] < 8000  && \[eof $s] } {after cancel {set ws_openMath($s,done) -1} ; set ws_openMath($s,done) 1; fileevent $s readable {} ;  }"
    myVwait ws_openMath($s,done)
    catch { close $s }
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ws_openMath($s,done) -1"
    return $ws_openMath($s,done)
}

			

proc doRead { sock } {
    global ws_openMath
    #puts reading; flush stdout;
    set tem [read $sock]
    append ws_openMath(url_result)  $tem
    # puts read:<$tem>
    # flush stdout
    if { [eof $sock] } {
	set ws_openMath(done) 1
	close $sock}
}

proc tes {} {
    OpenMathOpenUrl http://www.ma.utexas.edu/users/wfs/foo/t1.om
}
proc tempName { name extension } {
    set count [pid]
    while { [file exists $name[incr count].$extension] } { list }
    return $name$count.$extension
}

proc ws_outputToTemp { string file ext encoding } {
    upvar 1 $string result
    set tmp [tempName $file $ext ]
    set open $tmp
    if { [lsearch {x-gzip x-compress}  $encoding] >= 0 } { lappend dogzip |gzip -dc > $open ; set open $dogzip}
    set fi [open $open w]
    fconfigure $fi -translation binary
    puts -nonewline $fi $result
    flush $fi
    close $fi
    return $tmp
}

global debugParse
if { ![info exists debugParse ] } {
set debugParse 0
}

proc OpenMathOpenUrl { name args} {
    global ws_openMath
    #puts "OpenMathOpenUrl  $name $args "
    set history "" ; set historyIndex -1 ;set currentUrl ""
    set prevwindow ""
    set commandPanel [assoc -commandpanel $args ]
    if { "$commandPanel" == "" } {
	linkLocal . omPanel
	if { [info exists omPanel] } {
	    set commandPanel $omPanel
    }
		}
    set toplevel [assoc -toplevel $args ""]
    set reload [assoc -reload $args 0]
    set post [assoc -post $args ""]
    #puts "post=$post"
    if { [winfo exists $commandPanel ] }  {
	makeLocal $commandPanel history historyIndex textwin
	set toplevel [winfo paren $commandPanel]
	if { "$toplevel" == "." } {set toplevel ""}
	# eval pack forget [winfo parent $textwin ]
	set prevwin [winfo parent $textwin]
	set currentUrl [oget $textwin currentUrl]
	catch { set currentUrl [decodeURL [oget $textwin baseurl]] }

	if { $reload == 0} {
	
	    set new [resolveURL $name $currentUrl $post]
	    if { [set anchor [assoc anchor $new]] != "" } {
		set new [delassoc anchor $new]
	    }
	    set ii -1
	    foreach v $history { incr ii
	    if { "[delassoc post $new]" == "[delassoc post [oget $v.text currentUrl]]" } {
		# puts "new=$new\nold=[oget $v.text currentUrl]"
	    }
	    if   { "$new" == "[delassoc anchor [oget $v.text currentUrl]]" } {
		OpenMathMoveHistory $commandPanel [expr {$ii - $historyIndex }]
		if { "$anchor" != "" } {
		    update
		    catch {  $v.text yview anchor:$anchor }
		}
		
		#    OpenMathGetWindow $commandPanel $v
		#    pushHistory $commandPanel $v
		return
	    }
	
	}
    } else {
	# reload=1
	list
    }
   }
   set count 5
   while { [incr count -1] > 0 } {
       set new  [resolveURL $name $currentUrl $post]
       set result [getURL $new contentType mimeheader $post]
       if { [set tem [assoc location $mimeheader]] == "" } { break }
       set name $tem
   }

   #puts "contentType defined:[info exists contentType]"
   set handler [assoc $contentType $ws_openMath(urlHandlers)]
   if { "$handler" != "netmath" && "$handler" != "" } {
       set tmp [ws_outputToTemp result netmath ps "[assoc content-encoding $mimeheader]"]
       # to do fix this for windows #####
       exec sh -c "[format $handler $tmp] ; rm -f $tmp" &
       return
   }
  #puts contentType=$contentType

  #puts "got [string length $result] bytes"
  #puts ", result= [string range $result 0 70] .."

   if { [catch { set baseprogram [oget $textwin baseprogram] }] } {
       set baseprogram [decodeURL [getBaseprogram]]
   }
   # puts "using  $baseprogram"
   if { $reload } {   forgetCurrent $commandPanel }

   #puts "ws_openMath(counter)=$ws_openMath(counter)"

   set win [mkOpenMath [set w $toplevel.t[incr ws_openMath(counter)]] ]

   #puts "ws_openMath(counter)=$ws_openMath(counter)"


   makeLocal $w commandPanel
   #puts "resolveURL $name $currentUrl"


   if { [set anchor [assoc anchor $new]] != "" } {
       set new [delassoc anchor $new]
   }
   if { "[assoc filename $new]" == "" } {
       set new [putassoc  filename $new index.html]
   }
   # puts "...> $new"
   oset $w.text currentUrl $new
   oset $commandPanel location [encodeURL $new]
   oset $commandPanel textwin $win
   oset $w location  [encodeURL $new]
   # puts "new=$new"
   oset $commandPanel savefilename [file root [toLocalFilename $new]].txt
   set tem [assoc filename $new ""]
   #puts $contentType
   if { "$contentType" != "text/html" } {
       if { [string match "image/*" $contentType] } {
	   set im [image  create photo -data $result]
	   $win image create 0.0 -image $im
	   set err 0
       } else {
       set err [catch {   $win insert 0.0 $result } ]
       }
   } elseif { 1 }  {
    xHMinit_win $win
    xHMset_state $win url [encodeURL $new]
    oset $win baseprogram $baseprogram
    # puts win=$win,lengres=[string length $result]
    set errmsg1 ""
       set err 0
       global debugParse
       if { $debugParse } {
	   xHMparse_html $result "xHMrender $win"
	   set err 0
       } else {
	   set err [catch {
	       xHMparse_html $result "xHMrender $win"
	   } errmsg1 ]
	   }
	catch {
	    if { "$anchor" != "" } {
		update
		$win yview anchor:$anchor
	}
		}
	
	#   foreach v {Tresult Teval} {  $win tag raise $v}	


    }    else {
###Never get here.. must change to make be the rich text case..	
	# drop comment lines
	regsub -all "(^|\n)#\[^\n\]*\n" $result \n result ;
	#puts input=$result
	
	# note netscape would just truncate the history
	# at historyIndex, and start to grow it there,
	# losing the record of all files you have visited after..
	# maybe we should do this.
	#puts "history=$history"
	set err [catch { insertRichText $win insert $result }]
    }
    if { $err == 0 } {
	pushHistory $commandPanel $w
    }
    if { $err } {
	#puts "======begin======"
	#puts $result
	#puts "======end========"
	puts "$errmsg1"
	error "unable to evaluate [encodeURL $new]:$errmsg1"}
	
    }


proc pushHistory { commandPanel win } {
    global [oarray $commandPanel]
    makeLocal $commandPanel history historyIndex
    if { [llength $history] == 0 } {
	oset $commandPanel historyIndex -1
    }
    if { "[lindex $history $historyIndex ]" != "$win" } {
	oset $commandPanel history [linsert $history [incr [oloc $commandPanel historyIndex]] $win]
    }
}


#
 #-----------------------------------------------------------------
 #
 # omScrollPage --  scroll the page by N pages, keeping the insert
 # cursor visible.
 #
 #  Results: none
 #
 #  Side Effects: page scrolls
 #
 #----------------------------------------------------------------
#
proc omScrollPage { win n } {
    tkTextScrollPages $win $n
    set bbox [$win bbox insert]
    if { "" == "$bbox" } {
	if { $n > 0 } {
	    $win mark set insert @0,0
	} else {$win mark set insert @0,[$win cget -height]}
    }
}
	
#bind Text <Control-v> "omScrollPage %W 1"
#bind Text <Meta-v> "omScrollPage %W -1"
#bind Text <Alt-v> "omScrollPage %W -1"

proc addTagSameRange { win oldtag newtag index } {
    if { [lsearch [$win tag names $index] $oldtag ] >= 0 } {
	set this [$win tag prevrange $oldtag $index+1char]
	if { "$this" != "" && [$win compare $index < [lindex $this 1]] } {
	     $win tag remove $newtag 0.0 end
	     $win tag add $newtag [lindex $this 0] [lindex $this 1]
	     $win tag raise $newtag
	}
    }
}

global xHMpreferences
set xHMpreferences(defaultservers) { nmtp://genie1.ma.utexas.edu/ nmtp://linux51.ma.utexas.edu/ nmtp://linux52.ma.utexas.edu/ }
global embed_args
if { "[info var embed_args]" != "" } {
     set xHMpreferences(defaultservers) nmtp://genie1.ma.utexas.edu/
 }

proc getBaseprogram { } {
    global xHMpreferences
    lindex  $xHMpreferences(defaultservers) 0
}

proc fileBaseprogram { textwin parent x y } {
    set e $textwin.e
    catch { destroy $e }
    set x [expr {[winfo rootx $parent] + $x +30 - [winfo rootx $textwin]} ]
    set x 30
    set y [expr {[winfo rooty $parent] + $y - [winfo rooty $textwin]} ]
    global xHMpriv
    set xHMpriv(baseprogram) [encodeURL [oget $textwin baseprogram]]
    entry $e -width 40 -textvariable xHMpriv(baseprogram)
    place $e -in $textwin -x $x -y $y
    raise $e
    set com "destroy $e ; oset $textwin baseprogram \[decodeURL \$xHMpriv(baseprogram)] "
    bind $e <Leave> $com
    bind $e <Return> $com

}

######### font choosing utilities #########
global tcl_platform
global isFixedp

if { "$tcl_platform(platform)" == "unix" } {
    array set isFixedp {
fixed 1 {fangsong ti} 1 {clearlyu alternate glyphs} 0 lucidatypewriter 1 charter 0 lucidabright 0 times 0 ming 1 {lucidux sans} 0 {open look glyph} 0 {song ti} 1 newspaper 0 helvetica 0 {open look cursor} 1 li 1 mincho 1 {clearlyu ligature} 0 {clearlyu pua} 0 {lucidux mono} 1 courier 1 clearlyu 0 utopia 0 lucida 0 nil 1 clean 1 terminal 1 kai 1 gothic 1 cursor 0 symbol 0 {clearlyu arabic extra} 0 {lucidux serif} 0 {new century schoolbook} 0 song 1
}
}

proc fontDialog { top } {
    global xHMpreferences
    set font [xHMmapFont font:propor:normal:r:3]
    catch { destroy $top }
    toplevel $top
    wm iconify  $top
    set win $top.text
    text $win -font [list [font config $font -family] [font config $font -size]] -height 20
    wm deiconify $top
    foreach fam {propor fixed} {
	set lis ""
	set i 0
	while { $i <= 8 } {
	    lappend lis [expr {$i - 3}]
	    incr i
	}
	if { "$fam" == "fixed" } { set fixed 1 } else { set fixed 0}
	mkLabelListBoxChooser $win.size$fam "list $lis" xHMpreferences($fam,adjust)
	mkLabelListBoxChooser $win.family$fam "getFontFamilies $fixed " xHMpreferences($fam)
	set fo [xHMmapFont "font:$fam:normal:r:3"]
	catch { set xHMpreferences($fam) [assoc -family [font actual $fo]]}
    }
    $win insert insert "Font Settings\nThe proportional font is "
    $win window create insert -window $win.familypropor
    $win insert insert "with a size adjustment of "
    $win window create insert -window $win.sizepropor
    $win insert insert "\nThe proportional fixed font is "
    $win window create insert -window $win.familyfixed
    $win insert insert "with a size adjustment of "
    $win window create insert -window $win.sizefixed
    $win insert insert "\n"
    $win insert insert "Default nmtp servers  "
    global _servers
    set _servers $xHMpreferences(defaultservers)
    entry $win.entry -textvariable _servers -width 40
    $win window create insert -window $win.entry
    $win insert insert "\n\n"
    global ws_openMath
    $win insert insert "http Proxy host and port:"
    entry $win.entryproxy  -width 40
    catch { $win.entryproxy insert 0 $ws_openMath(proxy,http) }
    $win window create insert -window $win.entryproxy
    $win insert insert "\nIf you are behind a firewall enter the name of your http proxy host and port,\n eg: `foo.ma.utexas.edu 3128', otherwise leave this blank"
    global xHMpreferences
    set men [tk_optionMenu $win.plottype xHMpreferences(plotwindow) embedded separate multiple ]
    $win insert insert "\nShould plot windows be "
    $win window create insert -window $win.plottype
    $win insert insert "?"


    $win insert insert "\n\n\n"
    $win insert insert " Apply and Quit " "bye raised"
    $win insert insert "      "
    $win insert insert " Apply " "click raised"
    $win insert insert "      "
    $win insert insert " Cancel " "cancel raised"
    proc _FontDialogApply { win } {
	global xHMpreferences _servers ws_openMath
	set xHMpreferences(defaultservers) $_servers
	catch {xHMresetFonts .}
	if { [llength [$win.entryproxy get]] == 2 } {
	    set ws_openMath(proxy,http) [$win.entryproxy get]
	}
    }
    $win tag bind click <1> "_FontDialogApply $win"
    $win tag bind bye <1> "_FontDialogApply $win ; destroy $top"
    $win tag bind cancel <1> "destroy $top"
    $win tag configure raised -relief raised -borderwidth 2
    $win insert insert "      "
    $win insert insert "Save Preference" "save raised"
    $win tag bind save <1> "_FontDialogApply $win ; savePreferences"

    pack $win
#    place $win -in [oget [omPanel .] textwin] -x 10 -y 10
}
proc savePreferences {} {
    global xHMpreferences ws_openMath
    set fi [open  "~/netmath.ini" w]
    puts $fi "array set xHMpreferences {"
    foreach {k v} [array get xHMpreferences *] {
	lappend all [list $k $v]
    }
    set all [lsort $all]
    foreach v $all { puts $fi $v }
    puts $fi "}"
    if { [info exists ws_openMath(proxy,http)] && [llength $ws_openMath(proxy,http)] == 2   } {
	puts $fi [list array set ws_openMath [array get ws_openMath proxy,http]
	]
    }
    close $fi
}






proc getFontFamilies { fixed } {
    global isFixedp
    foreach font  [font families] {
	if { ![info exists isFixedp($font)] } {
	    set isFixedp($font) [font metrics [list $font] -fixed]
	}
	if { $isFixedp($font) == $fixed } {
	    lappend answer $font
	}
    }
    return [lsort $answer]
}
	


#
 #-----------------------------------------------------------------
 #
 # mkLabelListBoxChooser --  creates a button called WIN with textvariable
 #  $TEXTVAR.  When clicked on the WIN, brings down
 #  a list of items, and clicking on one of them selects that item. and
 #  resets $TEXTVAR
 #
 #  Results: none
 #
 #  Side Effects: the TEXTVAR value is changed, and so consequently the label.
 #
 #----------------------------------------------------------------
#
proc mkLabelListBoxChooser { win items  textvar} {
    button $win -textvariable $textvar -command "listBoxChoose $win [list $items] $textvar"
}
proc listBoxChoose { win  items textvar  } {
    global xHMpreferences
    set whei [winfo height $win]
    set items [eval $items]
    set hei [llength $items]
    set fr ${win}frame
    frame ${win}frame
    set list $fr.list
    set scroll $fr.scroll
    scrollbar $scroll -command "$list yview"
    listbox $list -yscroll "$scroll set" -setgrid 1 -height 8
    pack $scroll -side right -fill y
    pack $list -side left -expand 1 -fill both
    set wid 0
    foreach v $items {
	set xx [string length $v] ;
	set wid [expr {($xx > $wid ? $xx : $wid)}]
    }
    eval [concat $list insert 0 $items]
    catch { $list selection set [lsearch $items [set $textvar]] }
    bind $list <1> "set $textvar \[$list get \[$list nearest %y\]\]; destroy $fr"
    place $fr -in $win -x 0  -y 0 -anchor n
}


proc quoteForRegexp { s } {
    regsub -all {[\]\[$+()\\.?*]} $s {\\\0}  ans
    return $ans
}


## endsource browser.tcl
