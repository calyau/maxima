# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima.tcl,v 1.18 2002-09-06 02:21:11 mikeclarkson Exp $
#

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
# Source Tkmaxima/Readdata.tcl 		;# can be autoloaded
# Source Tkmaxima/Getdata1.tcl 		;# can be autoloaded
# Source Tkmaxima/Macros.tcl 		;# can be autoloaded
# Source Tkmaxima/Proxy.tcl 		;# can be autoloaded
# Source Tkmaxima/Send-some.tcl 	;# sets global variables
# Source Tkmaxima/Plotting.tcl 		;# sets global variables
# Source Tkmaxima/Private.tcl 		;# can be autoloaded
# Source Tkmaxima/Getopt.tcl 		;# can be autoloaded
# Source Tkmaxima/Parse.tcl 		;# sets global variables
# Source Tkmaxima/Textinsert.tcl 	;# can be autoloaded
# Source Tkmaxima/Printops.tcl 		;# can be autoloaded
# Source Tkmaxima/Push.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotconf.tcl 		;# can be autoloaded
# Source Tkmaxima/Adams.tcl 		;# can be autoloaded
# Source Tkmaxima/Rk.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotdf.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot2d.tcl 		;# defined globals
# Source Tkmaxima/Matrix.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot3d.tcl 		;# defined globals
# Source Tkmaxima/NPlot3d.tcl 		;# can be autoloaded

## source preamble.tcl

###### preamble.tcl ######

# get the number of clicks per second on this machine..
after idle {after 1000 "set ws_openMath(clicks_per_second) \[expr 1.0 *( \[clock clicks\] - [clock clicks])\]" }

catch {
    # the following will be defined only in the plugin
    array set embed_args [getattr browserArgs]
    proc wm { args } {}
}

## source Readdata.tcl

## source Getdata1.tcl

## source Macros.tcl

## source Proxy.tcl

# from Send-some.tcl
#mike - I hope these can be eliminated or encapsulated
global port magic interrupt_signal _waiting _debugSend
if { $argc == 0 } {
    set port 4444
    set magic "billyboy"
}
set interrupt_signal "<<interrupt fayve>>"

set _waiting 0

set _debugSend 0

## source Send-some.tcl

## source Plotting.tcl

## source Private.tcl

## source Getopt.tcl

## source Parse.tcl

## source Textinsert.tcl

## source Printops.tcl

## source Push.tcl

## source Plotconf.tcl

## source Rk.tcl

## source Adams.tcl

## source Plotdf.tcl

## source Plot2d.tcl

## source Plot3d.tcl

## source NPlot3d.tcl

## source patchold.tcl

###### patchold.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# these are some compatibility patches for older versions
# eg tk4.1  Before tk4.1 it wont work.

proc  mproc { name argl body } {
    if { "[info command $name]" == "" } {
	proc $name $argl $body
    }
}
mproc event {args} {}
mproc font  {option args} {
    global  ws_openMath
    switch $option {
	create  {
		
	    # puts "args=$args"
	    
	    set family [assoc -family $args "courier"]
	    set family [string tolower $family]
	    set slant [assoc -slant $args r]
	    if { "$slant" == "italic" || "$slant" == "oblique" } {
		set slant o
	    } else { set slant r}
	    
	    set size [assoc -size $args 10]
	    set weight [assoc -weight $args normal]
	    if { [fontExistsp  $family $weight $slant $size ] } {
		return [.bfontexists cget -font]
	    }
	    if { [fontExistsp  $family * $slant $size ] ||
	    [fontExistsp  $family * $slant  $size ] ||
	    [fontExistsp  $family $weight *  $size ] ||
	    [fontExistsp  $family $weight $slant  [expr $size -1]] ||
	    [fontExistsp  $family $weight $slant  [expr $size +1]] ||
	    [fontExistsp  $family $weight $slant  *] ||
	    [fontExistsp  * $weight $slant  *] ||
	    [fontExistsp  * $weight *  *] ||
	    [fontExistsp  * * *  *] } {
		return [.bfontexists cget -font]
	    } else { return [lindex [.bfontexists config -font] 3]
	    }
	}
    default { error "cant measure"
}
}
}

mproc font {option args} {
    global bil ws_openMath
    switch $option {
	create  {
	    set bil $args
	    # puts "args=$args"
	    
	    set family [assoc -family $args "courier"]
	    set family [string tolower $family]
	    set slant [assoc -slant $args r]
	    if { "$slant" == "italic" || "$slant" == "oblique" } {
		set slant o
	    } else { set slant r}
	    
	    set size [assoc -size $args 10]
	    set weight [assoc -weight $args normal]
    	    if { [catch { set allfonts $ws_openMath(allfonts)} ] &&
	    [catch { set allfonts [exec xlsfonts] } ]
	} { 	        return [list $family $size $weight]
	}
	set ws_openMath(allfonts) $allfonts
	# puts " lsearch -glob \$allfonts *$family*-*$weight-$slant*-$size-*"
	# puts *$family-$weight-$slant*-$size-*
	if { [set ind [lsearch -glob $allfonts *$family-$weight-$slant*-$size-*]] < 0 } {
	   #  puts *$family-normal-$slant*-$size-*
	    if { [set ind [lsearch -glob $allfonts *$family-normal-$slant*-$size-*]] >= 0 } { return  [lindex $allfonts $ind]  }
	return [list $family $size $weight]
	} else { 	    return [lindex $allfonts $ind] }
    }  
    default { error "cant measure"
}
}
}

proc fontExistsp {   family weight slant size } {
    if { ![winfo exists .bfontexists ] }  { entry .bfontexists }
    return [expr ![catch { .bfontexists config -font *-$family-$weight-$slant-*--$size-*-*-*-*-*-*-* }]]
}
    
 


## endsource patchold.tcl
## source eoctave.tcl

###### eoctave.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # insertResult_octave --  insert result RES, in text window W,
 # into RESULTRANGE.  The command which was sent to octave came
 # from THISRANGE.   For plots if a resultRANGE is missing,
 # we use a space just after the end of the line of THISRANGE.
 # checks if this is plotdata, and if so makes plot win for it.
 #
 #  Results: none
 #
 #  Side Effects:  inserts in text or graph in window W.
 #
 #----------------------------------------------------------------
#

proc insertResult_octave {  w thisRange resultRange res } {
    #puts "res=$res"
  if { [regexp "\{plot\[23\]d" $res] } {
      #puts "its a plot"
      set name [plotWindowName $w]
      set tem [setDesiredDims $w $name $thisRange ]
      eval plot2dData $name $res [getDimensions $w $name] 
      ShowPlotWindow $w $name  $thisRange $resultRange $tem
      return 0
   } elseif { "$resultRange" != "" } {
	insertResult $w $resultRange $res
   }
   return 0
}

global ws_openMath
set ws_openMath(options,octave) {{doinsert 1 "Do an insertion" boolean}}


## endsource eoctave.tcl
## source eopenplot.tcl

###### eopenplot.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################



#
 #-----------------------------------------------------------------
 #
 # eval_openplot --  invoke OPENPLOT on the substring of Window given
 # by thisRange, and substitute the result into resultRange, if the
 # latter is not the empty list.   If it is, then the window is placed
 # on the next line from this command.
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc eval_openplot { program w thisRange resultRange } {

 
   set name  [plotWindowName $w]
    set desired [setDesiredDims $w $name $thisRange ]
   set tem [eval $w get $thisRange]
   lappend tem -windowname $name
   foreach v [getDimensions $w $name] { lappend tem $v }
   set allowed "plot2d plotdf plot3d"
   set f [lindex $tem 0]
   if { [lsearch $allowed $f] >= 0 } {
       apply $f [lrange $tem 1 end]
       ShowPlotWindow $w $name $thisRange $resultRange $desired
   } else { error "$f not allowed, only {$allowed}" }
   return 0
}


#
 #-----------------------------------------------------------------
 #
 # plotWindowName --  checks preferences to see if separate or multiple
 #  or nontoplevel windows are desired, and chooses a name accordingly.  
 #  in the first two cases it also assures that the toplevel window exists.
 #  
 #  Results: window name
 #
 #  Side Effects:  possibly make a new toplevel window.
 #
 #----------------------------------------------------------------
#
proc plotWindowName { w } {
    upvar #0 xHMpreferences(plotwindow) plot
    upvar #0 ws_openMath(plot,count) count
    set name ""
    
    if { ![info exists plot] || "$plot" == "embedded" } {
	linkLocal $w counter
	if { ![info exists counter] } {set counter 0}
	return $w.plot[incr counter]
    }
    set name ".plotfr"	
    if { "$plot" == "multiple" } {
	if { ![info exists count] } { set count 1} else {
	    incr count }
	append name $count
    }
    if { ![winfo exists $name ] } {
	toplevel $name
	set h [expr {round ([winfo screenheight $name]*.6) }]
	set wid [expr round ($h * 1.2) ]
	set r1 [expr {round(10+rand()*30)} ]
	set r2 [expr {round(10+rand()*30)} ]
	wm geometry $name ${wid}x${h}+${r1}+${r2}
	if { "[info proc setIcon]" != "" } {
	    after 1000 setIcon $name
	}

    }

    append name .plot
    return $name
}


proc whereToPutPlot { w thisRange resultRange } {
    if { "$resultRange" != "" } {
	eval $w  delete $resultRange
	set at [lindex $resultRange 0]
	$w insert $at " " { Tresult}
	set at [$w index "$at + 1char"]
    } else {
	set at "[lindex $thisRange 1] lineend + 1 chars"
    }
    return $at
}


proc setDesiredDims { w name range } {
    #puts "setDesiredDims  $w $name $range"
    foreach v [getTagsMatching $w "^(width|height):" $range] {
	set tem [split $v :]
	lappend ans [lindex $tem 0]Desired [lindex $tem 1]
    }
    if { [info exists ans] } {
	oarraySet $name $ans
	return $ans
    }
    return ""
}

proc getDimensions { w name } {
   # puts "getDimensions  $w $name"
   set parent [winfo parent $w] 
   set scrollwidth 15
   catch { set scrollwidth [ [winfo parent $parent].scroll cget -scrollwidth] }
   set width [winfo width $w]
   set height [winfo height $w]
   #set width [getPercentDim [oget $name widthDesired] width $w] 
   catch {set width [getPercentDim [oget $name widthDesired] width $w] }
   catch {set height [getPercentDim [oget $name heightDesired] height $w] }
   
   set width [expr {round ($width-4) }]
   set height [expr {round ($height-4)}]
   #puts "using width,height=$width,$height"
   
   if { $width <0 } {
       set width [expr {[oget $parent width] - 2*$scrollwidth}]
       set height [expr {round(.85*[oget $parent height])}]
   }
   return " -width $width -height $height"
}

global ws_openMath
set ws_openMath(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

proc insertResult_openplot {w args } { puts "insert=[$w index insert]"  }

proc ShowPlotWindow { w name thisRange resultRange desired } {
   if { "[winfo toplevel $w]" != "[winfo toplevel $name]" } {
       $name config -relief sunken -borderwidth 2
       pack $name -expand 1 -fill both
       raise [winfo toplevel $name ]
       return
   }
   oarraySet $name $desired
   set at [whereToPutPlot $w $thisRange $resultRange]
   set col [lindex [split $at .] 1]
    
   if { $col > 0 } {
       $w insert $at "\n \n" "$name"
       set at [$w index "$at +1char"]
   }
   # compute where we will try to display.
   # try to leave top of window where it is, but if not
   # scroll lines up just the amount necessary to make the
   # window visible.
   
   set h1 [winfo height $w]
   set h2 [oget  $name height]
   set begin [$w index @0,0]
   set ind $at
   set dl [$w dlineinfo $ind]
   set y0 [lindex $dl 1]
   set prev ""
   if { "$y0" != "" } {
   while { [$w compare $begin <= $ind] } {
       set dl [$w dlineinfo $ind]

       if { "$dl" == "" } { break }
       if { $y0 - [lindex $dl 1] + $h2 +5 < $h1  } {
	   set prev $ind
	   set ind [$w index "$ind - 1 line" ]
       } else { break }
   }
  }

   bind $name <Destroy> "catch {$w yview [$w index @0,0] } ; eval $w delete \[$w tag  nextrange $name 0.0 \]"

   if { "$prev" != "" } { set ind $prev }
      $w insert $at " " "$name center"
      $w window create  $at+1char -window $name
      $w tag add "center $name" $at "$at+2char"
   update
   $w yview $ind
   # somehow the single button click gets run positioning the cursor
   # near where the 
   after 1 $w mark set insert [$w index insert]
   return $ind
}

## endsource eopenplot.tcl
## source emaxima.tcl

###### emaxima.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # insertResult_maxima --  insert result RES, in text window W,
 # into RESULTRANGE.  The command which was sent to maxima came
 # from THISRANGE.   For plots if a resultRANGE is missing,
 # we use a space just after the end of the line of THISRANGE.
 # checks if this is plotdata, and if so makes plot win for it.
 #
 #  Results: none
 #
 #  Side Effects:  inserts in text or graph in window W.
 #
 #----------------------------------------------------------------
#

proc insertResult_maxima {  w thisRange resultRange res } {
    set program maxima
    
#    puts <lengthres=[llength $res],thisRange=$thisRange,resultRange=$resultRange>

    if { 0 == [string compare "$res" "cant connect"] } {
	bgerror "unable to call $program"
    }
  if { [regexp "\{plot\[23\]d" $res] } {
      #puts "its a plot"
      set name [plotWindowName $w]
      eval plot2dData $name $res [getDimensions $w $name]
      set desired [setDesiredDims $w $name $thisRange ]
      ShowPlotWindow $w $name  $thisRange $resultRange $desired
      return 0
    }   

    if { "$resultRange" != "" }   {
	set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
	insertResult $w $resultRange $res
	
    }
    return 0
}

## endsource emaxima.tcl
## source ehref.tcl

###### ehref.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # eval_href --  Follow a link to another om document
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc obsoleteeval_href { program w this nextResult} {
    set arg ""
   foreach v [$w tag names [lindex $this 0]] {
       if { [string first "\{ThrefArg" $v] == 0 } {
	   set arg $v
	   break
       }
   }
   set arglist [getTargTags $w $this]
   if { [llength $arglist] != 1 } {
       return -code error -errorinfo  "[llength $arglist] args to href.  Wanted 1, got: $arglist"
   }
   puts "arglist=$arglist"
   
   set arg [lindex $arglist 0]
   puts "arg=$arg"
   set list [lrange $arg 1 end]
   set doc [assoc -src $list ""]

   set searchregexp [assoc -searchregexp $list ""]
   set search [assoc -search $list ""]

   puts "doc=$doc"
   
   if { "$doc" != "" } {
       puts "       OpenMathOpenUrl $doc -commandpanel [omPanel $w]"
       OpenMathOpenUrl $doc -commandpanel [omPanel $w]
   }
   makeLocal  [omPanel $w] textwin
   set ind ""
   if { "$searchregexp" != "" } {
       set ind [ $textwin search -regexp -- $searchregexp 1.0]
   } elseif { "$search" != "" } {
       set ind [ $textwin search -exact -- $search 1.0]
   }
   if { "$ind" != "" } {
       $textwin yview $ind
   }
   return 0
}

global ws_openMath
set ws_openMath(options,href) {
    {src "" "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"}
    {search "" "A string to search for, to get an initial position"}
    {searchregexp "" "A regexp to search for, to get an initial position"}
}


## endsource ehref.tcl
## source browser.tcl

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
}   }

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
		} else { return $answer }
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
    if { "$top" == "." } { return $win} else { return $top$win}}

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
    foreach v [uplevel #0 trace vinfo $var] {
	uplevel #0 trace vdelete $var [lindex $v 0] [list [lindex $v 1]]
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
}   }
    

proc getw { s  } { eval pack forget [winfo children . ] ; pack $s}

proc try1 { file } {
   global ccc
   eval pack forget [winfo children . ] 
   mkOpenMath [set w .t[incr ccc]]
   uplevel #0 source $file
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
}   }

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
		    uplevel 1 set $mimeheader \[ uplevel #0 set ws_openMath(mimeheader) \]
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
			
		}   }
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
    }   }
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
	}   }
	
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

    
proc mkOpenMathEditButtons { win } {
    maxima
    octave
    pari
    bold
    italic
    setfont
    ..

    }


## endsource browser.tcl
## source wmenu.tcl

###### wmenu.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# implement a menu bar without toplevel windows.
# wet

proc wmenubar { name  } {
    if { "[string index $name 0]" == "." } {
	frame $name
	# puts "rename $name $name-orig"
	rename $name $name-orig
	set top [winfo toplevel $name]
	oset $top helpwin "" 
	proc $name { option args } "wmenubarInternal $name \$option \$args"
	set parent [winfo parent $name]
	# maybe change this to do traversal toward side leaving on..
	oset $name items ""
    } else {error "needs a window name arg"
    }
}

    
proc eswitch { key lis } {
    foreach {k act} $lis { lappend allowd $k}
    lappend lis default "error $key must be  one of: $allowd"
    uplevel 1 switch $key  [list  $lis]
}

proc ogetr { win var dflt } {
    set w $win
    while { 1 } {
	if { 0 == [catch { set val [oget $w $var] }] } {
	    return $val
	}
	global [oarray $w]
	# puts w=$w,[array get [oarray $w]]
	set w [winfo parent $w]
	if { "$w" == "" } {return $dflt}
    }
}

proc deleteHelp { win } {

   linkLocal $win helpPending
   if { [info exists  helpPending] } {
	after cancel $helpPending
	unset helpPending
    }
    set top [winfo toplevel $win]
    set helpwin [oget $top helpwin]
   if { "$helpwin" != ""} {
       place forget $helpwin 
   }   
}
    
proc setHelp {win  help args } {
   # set c [ogetr $win c "cant"]
    if { "$help" == "" } {set help "This is a menu window $win"}
    set enter ""
    set exit ""
    if  { [catch { set current [$win cget -relief] } ] || "$current" \
	    != "flat" } {
	set enter ""
	set exit ""
    } else { set enter "$win configure -relief raised" ;
	set exit "$win configure -relief $current"
    }
    # puts "current=$current"

    bind $win <Enter> "$enter; showHelp $win  {$help} $args"
    bind $win <Leave> "$exit; deleteHelp $win"
}

global show_balloons
set show_balloons 1


#
 #-----------------------------------------------------------------
 #
 # showHelp --  for WINDOW show a HELP message using ANCHOR positions.
 #  WINDOW may be a window or a rectangle specifier: x,y,wid,height 
 #  ANCHOR positions may be either n,w,e,s,nw,ne,se,sw,center or
 #  one of these followed by two floating point numbers indicating
 #  the fraction of the width and height of the window one is away from
 #  the upper left x,y of the window.
 #  Results: none
 #
 #  Side Effects: display a window. 
 #
 #----------------------------------------------------------------
#
proc showHelp { win help args } {
    global show_balloons helpwin
    if { $show_balloons == 0 } {
	catch { place forget $helpwin }
	return
    }
    linkLocal [lindex $win 0] helpPending
    set helpPending [after 1000 [list showHelp1 $win $help $args]]
}

proc showHelp1 { win help args } {
    global  tk_version
    set top [winfo toplevel [lindex $win 0]]
#    set anchors $args
#    append anchors "  w  e s ne n sw nw"
#    set anchors " nw"
#    set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
#     set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
    set anchors "sw w e n {nw .2 1.2} {ne .8 1.2} s se"
    makeLocal $top helpwin
    if { "$helpwin" == "" } {
	set tt $top
	if { "$tt" == "." } {set tt ""}
	set helpwin $tt.balloonhelpwin
	if { ![winfo exists $helpwin] } {
	
	    label $helpwin -width 0 -height 0  -borderwidth 1 \
		    -background beige -padx 4 -pady 4 -justify left
	}
	if { $tk_version < 8.0 } {
	    $helpwin config -relief ridge -borderwidth 2
	} else { $helpwin config -relief solid }
	
	oset $top helpwin $helpwin
    }
    if { [string first _eval $help ] == 0 } {
	catch { set help [eval [concat list [lindex $help 1]]]}
    }
    
    $helpwin configure -text $help -wraplength [expr {round(.34 * [winfo width $top])}]
    global anchorPositions
    if { [llength $win] == 5 } {
	desetq "win wx wy wxdim wydim" $win
    }  else {
	set wx [expr {[winfo rootx $win ] - [winfo rootx $top]}]
	set wy [expr {[winfo rooty $win ] - [winfo rooty $top]}]
    set wxdim [winfo width $win]
    set wydim [winfo height $win]
    }
    set nxdim [winfo reqwidth $helpwin]
    set nydim [winfo reqheight $helpwin]
    set topxdim  [winfo width $top]
    set topydim  [winfo height $top]
    global anchorPositions
    foreach an $anchors {
	if {[llength $an] == 3} {
	    desetq "an rx ry" $an
	} else {
	desetq "rx ry" [lsublis { {0 1.1 } {1 -.1}} $anchorPositions($an)]
	}
	# puts "rx=$rx,ry=$ry"
	set yoff [expr { $ry > 1 ? 8 : $ry < 0 ? -8 : 0 } ]
	desetq "x y" [getPlaceCoords 0 $yoff $rx $ry $an $wx $wy $wxdim $wydim $nxdim $nydim]
	# puts "for $win $an rx=$rx,ry=$ry x=$x,y=$y :[expr {$x >5}],[expr {$y > 5}],[expr {$x+$nxdim < $topxdim}],[expr {$y +$nydim < $topydim}]"
	if { $x > 5 && $y > 5 && $x+$nxdim < $topxdim && \
		$y +$nydim < $topydim } {
	    place forget $helpwin

    	    place $helpwin -x $x -y $y -anchor nw
	    raise $helpwin
	    return
	}
    }
}

proc wmenubarInternal { win  option  lis } {
  # puts "{wmenubarInternal $win $option $lis}"
    set key [lindex $lis 0]
    set lis [lrange $lis 1 end]
    eswitch $option {
	add {
	    set parent [winfo parent $win]
	    if { "$parent" == "."} {set parent ""}
	    set men [assoc -menu $lis $parent.item[llength [oget $win items]]]
 	    bindAltForUnderline $key "wmenuPost $key"
	    frame $men -relief raised -borderwidth 2p
	    setHelp $key [assoc -help $lis] n nw ne
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    proc $men {option args } $body
	    pack $key -in $win -side left -expand 1 -fill both
	    global [oarray $win]
	    lappend [oloc $win items] $key
	    oset $key menu $men
	    oset $men items ""
	    oset $key parent $win
	    bind $key <Button-1>  {wmenuPost %W}
	    return $men
	}
	configure {
	    return [eval $win-orig configure $key $lis]

	}
	invoke {
	    set w [lindex [oget $win items] $key]
	    wmenuPost $w
	}
	cget {
	    return [eval $win cget $key $lis]
	}
    }
}

proc getSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] >= 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc excludeSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] < 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc lsublis { subs lis } {
    foreach v $subs {
	set key [lindex $v 0]
	while { [set i [lsearch $lis $key]] >= 0 } {
	    if { [llength $v] > 1 } {
		set lis [lreplace $lis $i $i [lindex $v 1]]
	    } else {
		set lis [lreplace $lis $i $i]
	    }
	}
    }
    return $lis
}

proc wmenuInternal {win option  olist } {
    set key [lindex $olist 0]
    set lis [lrange $olist 1 end]
    makeLocal $win menu parent 
    makeLocal $menu items
    eswitch $option {
	add {
	    if { [catch {set counter [oget $menu counter] }] }   {
		set counter 0
	    }
	    oset $menu counter [incr counter]
	    # set new to be the new menu item window
	    # set com to be the command for 'invoke' to invoke.
	    set opts [excludeSomeOpts "-textvariable -image -label -underline -help" $lis]
	    set labopts [lsublis {{-label -text}} \
		    [getSomeOpts "-image -label -textvariable -underline" $lis]]
	    append labopts " -justify left -anchor w"
	    eswitch $key {
	    radio {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		# puts "new=$new"
		apply label $new.label $labopts
		pack $new.label -side left -fill x
        	set opts [lsublis {{-radiovariable -textvariable}} $opts]
		apply radiobutton $new.radio $opts
		pack $new.radio -side right -anchor e
		set com "$new.radio invoke"
	    }
	    check {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		# puts "new=$new"
		apply label $new.label $labopts
		pack $new.label -side left
        	set opts [lsublis {{-checkvariable -textvariable}} $opts]
		apply checkbutton $new.check $opts
		pack $new.check -side right
		# puts "$var --> $val"
		set com "$new.check invoke"
	    }
	    command {
		set com [assoc -command $lis]
		set new $menu.fr$counter
		frame $new -borderwidth 1
		apply label $new.label $labopts
		pack $new.label -in $new -side left
		# puts "bind $new.label <Button-1> $com"
		bind $new.label <Button-1> $com
		bind $new <Button-1> $com
	    }
	    window {
		set new [assoc -window $lis]
		set com [assoc -command $lis list]
	    }
	    entry {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		apply label $new.label $labopts
		set opts [lsublis {{-entryvariable -textvariable}} $opts]
		apply entry $new.entry $opts
		pack $new.label -side top -in $new -anchor w
		pack $new.entry  -side top -in $new
		set com "focus $new.entry"
	    }
	    separator {
		set new $menu.sep$counter
		frame $new -height 4
		propagate $new 0
		set com ""
	    }
	    
	}
	bindAltForUnderline $new.label "$menu invoke $new"
	pack $new -in $menu -side top -fill x -expand 1
	oset $menu items [lappend items $new]
	oset $menu command$new $com
	setHelp $new [assoc -help $lis] w e
	return $new
    }
    configure {
	return [eval $win configure $key $lis]
    }
    invoke {
	makeLocal $menu items
	if { ![winfo exists $key] }  {
	    # it is an index
	    set key [lindex $items $key]
	}
	eval [oget $menu command$key]
	return
    }
    post {
	
        place $menu -anchor nw -relx 0 -rely 0 -bordermode outside -in $win
	bind $menu <Leave> "place forget $menu"
	focus $menu
	#bind $menu <FocusIn> "puts focus in"
	#bind $menu <FocusOut> "puts {leave for focus  menu}"
	raise $menu
    }
  }
}

proc wmenuPost { win } {
    makeLocal $win parent menu
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor nw -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}

proc bindAltForUnderline { item command } {
    set ind -1
    catch { set ind [$item cget -underline] }
    if { $ind >= 0 } {
	set letter [string index [$item cget -text] $ind]
	set to [winfo toplevel $item]
	bind $to <Alt-Key-$letter> $command
    }
}
	
proc showSomeEvents { win } {
    foreach v { Enter FocusIn FocusOut Visibility Leave} {  bind $win <$v> "puts {$win $v %x %y}"}
}

array set anchorPositions {
    n {.5 0} nw { 0 0 } se {1 1} e {1 .5} center {.5 .5}
    s { .5 1} sw { 0 1} w { 0 .5} ne { 0 1} }
    
proc getPlaceCoords { x y relx rely anchor xIn yIn xdimIn ydimIn xdim ydim } {
    global anchorPositions
    # puts "xIn=$xIn,yIn=$yIn,xdimIn=$xdimIn,ydimIn=$ydimIn,xdim=$xdim,ydim=$ydim"
    set x1 [expr {$x + $xIn+$relx * $xdimIn}]
    set y1 [expr {$y + $yIn+$rely * $ydimIn}]
    desetq "fx1 fy1" $anchorPositions($anchor)
    set atx [expr {$x1 - $fx1*$xdim}]
    set aty [expr {$y1 - $fy1*$ydim}]
    return [list $atx $aty]
}

## endsource wmenu.tcl
# source tryftp1.tcl
## source tryftp2.tcl

###### tryftp2.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
if { "[info commands vwait]" == "vwait" && "[info commands myVwait]" == "" } {
  proc myVwait { x  } {uplevel 1  vwait $x }
}

proc submitFtp { viahost host name password directory filename} {
    global ftpInfo 

    if  { [catch { set sock [socket $viahost 80] } ] } {
	set sock [socket $viahost 4080]
    }
    set ftpInfo($sock,done) 0
    set len [string length $ftpInfo(data)]
    set ftpInfo($sock,data) $ftpInfo(data)
 
    # set sock [open /tmp/jim w+]
    fconfigure $sock -blocking 0 -translation {lf lf}
    # global billy ;lappend billy [list [fconfigure $sock]]
    puts $sock "POST /cgi-pub/wfs/submitftp HTTP/1.0"
    puts $sock "MIME-Version: 1.0"
    puts $sock "Accept: text/html"
    puts $sock "Accept: text/plain"
    puts $sock "Content-type: text/plain"
    puts $sock "Content-length: $len"
    puts $sock "Username: $name"
    puts $sock "Password: $password"
    puts $sock "Remote-host: $host"
    puts $sock "Remote-directory: $directory"
    puts $sock "Remote-filename: $filename"
    puts $sock ""
    flush $sock
    # puts $sock $ftpInfo(data) ; flush $sock
    # puts sock=$sock
    set ftpInfo(message) ""

    set after_id [after 10000 "set ftpInfo($sock,done) -1"]

    set ftpInfo($sock,datalength) $len
    set ftpInfo($sock,datanext) 0
    set ftpInfo($sock,log) "none.."
   # puts $sock $ftpInfo(data) ; flush $sock
    fileevent $sock writable "ftp2SendData $sock"
    fileevent $sock readable "ftp2WatchReturn $sock"
    myVwait ftpInfo($sock,done)
    set res $ftpInfo($sock,done)
    set ftpInfo(message) $ftpInfo($sock,log)

    #mike FIXME: this is a wrong use of after cancel
    after cancel $after_id

    # puts $ftpInfo($sock,return)
    ftp2Close $sock
    return $res
}

proc ftp2Close { sock } {
    global ftpInfo
    close $sock
    foreach v  [array names ftpInfo $sock,*]  {
	unset ftpInfo($v)
    }
}

proc ftp2WatchReturn { sock } {
    global ftpInfo 

    append ftpInfo($sock,return) " watching ..."
    set new [read $sock ]
    #global billy ; lappend billy [list return $new]
    if { [eof $sock] } {fileevent $sock readable {}}
    # puts "watching..new=$new" ; flush stdout
    append ftpInfo($sock,return) $new
    if { [regexp "Succeeded: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg]} {
	set ftpInfo($sock,done) 1
	set ftpInfo($sock,log) $msg
    } elseif { [regexp "Failed: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg] } {
	set ftpInfo($sock,done) -1
	set ftpInfo($sock,log) $msg
    }
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ftpInfo($sock,done) -1"
    after 3000 "set ftpInfo($sock,done) -1"
}
# set billy {}
proc ftp2SendData { sock } {
    global ftpInfo 
    
    set dn $ftpInfo($sock,datanext)
    set dl $ftpInfo($sock,datalength)
    #global billy ; lappend billy [list $dn $dl]
    set ftpInfo(percent) [expr {($dn >= $dl ? 100.0 : 100.0 * $dn/$dl)}]
    # puts "storing data to $sock $percent %"
    if { $ftpInfo($sock,datanext) >= $ftpInfo($sock,datalength) } {
	#mike FIXME: this is a wrong use of after cancel
	after cancel "set ftpInfo($sock,done) -1"
	after 10000 "set ftpInfo($sock,done) -1"
	fileevent $sock writable ""
	# puts $sock "abcdefghijklmno"
	# flush $sock
	return
    }
    set amtToSend 4000
    puts -nonewline $sock [string range $ftpInfo($sock,data) $ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend -1}]]
    # puts  $sock $tosend
    flush $sock

    #mike FIXME: this is a wrong use of after cancel
    set ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend}]
	after cancel "set ftpInfo($sock,done) -1"
    after 10000 "set ftpInfo($sock,done) -1"
}
    


## endsource tryftp2.tcl
## source myhtml.tcl

###### myhtml.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# parsing routines for html
# try to be compatible from calling level with the package by stephen uhler.
# to use:
#  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t" ;     array set wvar $args
# source myhtml.tcl ; catch {destroy .t } ; text .t ;  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t" 

proc testit { file } {
    global xHMpriv
    source myhtml.tcl
    catch {destroy .t }
    foreach {k val} [array get xHMpriv geom*] {unset xHMpriv($k) }
    frame .t
    text .t.text
    set t .t.text
    set html [exec cat $file]
    xHMinit_win $t
    xHMset_state $t url $file
    xHMparse_html $html "xHMrender $t"
    pack .t
    pack $t
    raise .
}

#     
#     xHMparse_html $html "xHMrender .t"
# you can change the state of the parse engine by using
#    xHMset_state .t key1 val1 key2 val2...

#########

#  the HTML tags:

# becomes

# idea: some tags like font,indent,link have only one per but the tag
# varies..  others have a constant tag... eg 'strike' 'underline' ...
# or fill.  You cant have    
# and are either on or off...  
# have pushConstantTag win tag
# have popConstantTag win tag
# have pushNamedTag win name tag
# have popNamedTag win name tag   :sets current to be this one and pushes previous..
# and these maintain things so that
# [array names xHMtaglist$win] should provide the taglist to do

proc xHMpushConstantTag { win tag } {
    upvar #0 xHMtaglist$win taglist
    if { [catch {incr taglist($tag) } ] } {
	set taglist($tag) 1 }
}

proc xHMpopConstantTag {win tag} {
    upvar #0 xHMtaglist$win taglist
    catch {
	set i [incr  taglist($tag) -1]
	if { $i <= 0 } {unset taglist($tag) }
    }
}

proc xHMpushNamedTag {win name tag} {
     upvar #0 xHMvar$win wvar
    #puts "push $win <$name> <$tag>"
    if { [catch { set now [lindex [set wvar($name)] end] }] } {
	set now "" }
    lappend wvar($name) $tag
}

proc xHMpopNamedTag {win name} {
    upvar #0 xHMvar$win wvar
    set v [set wvar($name)]
    set now [lindex $v end]
    catch { set v [lreplace $v end end] }
    set wvar($name) $v
    return $now
}

proc xHMgetNamedTag {win tag } {
    upvar #0 xHMvar$win wvar
    set res ""
    catch  { set res [lindex $win($tag) end] }
    return $res
}
    
proc xHMpushAindent { win i } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    set n [incr wvar(indent) $i]
    # puts "taglist:[array names taglist ]"
    unset taglist(indent:[expr {$n - $i}])
    set taglist(indent:$n) 1
}

proc xHMpopAindent { win i } {
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set n 0
    set n [set wvar(indent)]
    
    unset taglist(indent:$n)
    set n [expr {$n - $i}]
    if { $n < 0 } { set n 0 }
    set wvar(indent) $n
    set taglist(indent:$n) 1
    
}

# font and indent wil


#
 #-----------------------------------------------------------------
 #
 # defTag --  creates an executable scripts to invoke when the TAG
 #  or /TAG are encountered.
 #     -alter  takes a list of key1 val1 key2 val2
 #         generally these are pushed onto stacks for TAG and popped for /TAG
 #         the value of xHMtaglist$win  should get altered
 #     -before  set the prefix for text inserted for TAG
 #     -after   set the prefix for text inserted for /TAG
 #     -body   additional body to use for TAG
 #     -sbody   additional body to use for the /TAG
 #  The variables { tag  params text }  are bound when
 #  the BODY is evaluated.   Thus for example $text would get the
 #  text following the tag, and
 # 	set paramList [xHMsplitParams $params]
 #  could be used to decode the params. 
 # 
 #  Results: none
 #
 #  Side Effects: saves the script in xHMtag array under TAG and /TAG
 #
 #----------------------------------------------------------------
#
proc defTag { htag args } {
    global xHMtag
    foreach {key val } $args { set $key $val }
    if { [info exists -alter] } {
	foreach { key tag } ${-alter} {
	    if { [string match A* $key] } {
		append body "\nxHMpush$key \$win $tag"
		append sbody "\nxHMpop$key \$win $tag"
	    } elseif { [string match C* $key] } {
		append body "\nxHMpushConstantTag \$win $tag"
		append sbody "\nxHMpopConstantTag \$win $tag"
	    } else {
		append body "\nxHMpushNamedTag \$win $key $tag"
		append sbody "\nxHMpopNamedTag \$win $key"
	    }
	}
	array set toalter ${-alter}
	foreach prop { family size weight style} {
	    if { [info exists toalter($prop)] } { append fontprops " $prop"}
	}
	catch {
	    append body "\nxHMalterFont \$win $fontprops"
	    append sbody "\nxHMalterFont \$win $fontprops"
	}
    }
    catch { append body \n${-body} }
    catch { append sbody \n${-sbody} }
    catch { append body "\nset prefix \"[slashNewline ${-before}]\"" }
    catch {append sbody "\nset prefix \"[slashNewline ${-after}]\""  }
    catch { set xHMtag($htag) $body }
    catch { set xHMtag(/$htag) $sbody }
}
proc slashNewline { s } {
    regsub -all "\n" $s "\\n" s
    return $s
}
# netscape uses fonts in the following progression.
# we will have the font labels looking like:
#  font:propor:normal:r:4   to indicate size 4
# In an application if the user sets the default
# nfont:nfamily:nweight:nstyle:nsize
# where nfamily is in {propor,fixed}
# where nweight is in {normal,bold}
# where nstyle  is in {i,r}
# where nsize   is in {1,2,3,4,5,6,7}
# then we map the label to a particular font....
# propor-->times
# fixed->courier

# set the font to be what it would map to for X.
proc xHMsetFont { win fonttag  } {
    upvar #0 xHMvar$win wvar
    set fo [xHMmapFont $fonttag]
    set wvar($fonttag) 1
    $win tag config $fonttag -font $fo
}


    
#
#  #### We have legacy code from before the font command existed..
#  
    
if { "[info command font]" != "font" } {

 #convert a fonttag into an actual font specifier, using preferences.
 # mapping propor,fixed to font families, and dobing size adjusting based
 # on font type.
 proc xHMmapFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences
    set s [split $fonttag :]
    set fam [lindex $s 1]
    #puts "fam=$fam,fonttag=$fonttag"
    if { "$fam" == "" } {
	set fam propor
    }
    set si [expr {$xHMpreferences($fam,adjust) + [lindex $s 4]}]
    set si [expr {($si < 1 ? 1 : ($si > 8 ? 8 : $si))}]
#    set family $xHMpreferences([lindex $s 1])
#    set weight [lindex $s 2]
#    set style [lindex $s 3]
    return "-*-$xHMpreferences($fam)-[lindex $s 2]-[lindex $s 3]-normal-*-*-$xHMpreferences($fam,$si)0-*-*-*-*-*-*"
 }
 # reset fonts for a window taking into account current preferences.
 proc xHMresetFonts { win } {
    upvar #0 xHMvar$win wvar
    foreach fonttag [array names wvar font:* ] {
	xHMsetFont $win $fonttag }
    }


 proc xHMfontPointSize { string } {
#    expr round ([lindex [split [xHMmapFont font:fixed:normal:r:3] -] 8] / 10.0)
    set tem [lindex $string 1]
    if { [catch { expr { $tem +1} }] } {
	error "bad font $string"
    }
    return $tem
#    expr round ([lindex [split $string -] 8] / 10.0)
 }
	
 } else {    
    
#convert a fonttag into an actual font specifier, using preferences.
# mapping propor,fixed to font families, and dobing size adjusting based
# on font type.
 proc xHMmapFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences xHMfonts
    if { [info exists xHMfonts($fonttag) ] } {
	return $xHMfonts($fonttag)
    } else {
	set xHMfonts($fonttag) [set fo [font create]]
	xHMconfigFont $fonttag
	return $fo
	
    }
 }

 proc xHMconfigFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences xHMfonts
    set font $xHMfonts($fonttag)
    set s [split $fonttag :]
    set fam [lindex $s 1]
    #puts "fam=$fam,fonttag=$fonttag,s=$s"
    if { "$fam" == "" } {
	set fam propor
    }
    set si [expr {$xHMpreferences($fam,adjust) + [lindex $s 4]}]
    #set si [lindex $s 4]
    set si [expr {($si < 1 ? 1 : ($si > 8 ? 8 : $si))}]
    set family $xHMpreferences([lindex $s 1])
    set weight [lindex $s 2]
    set slant [lindex $s 3]
    if { "$slant" == "i" } { set slant italic} else {set slant roman}
    #puts "font config $font -family $family -size $xHMpreferences($fam,$si) -slant $slant -weight $weight"
    global tcl_platform
    if { "$tcl_platform(platform)" == "unix" } {
	set usePixel "-"
    } else { set usePixel "" }
    font config $font -family $family -size $usePixel$xHMpreferences($fam,$si) -slant $slant -weight $weight
    return
 }

 ### the following resets all the fonts
 ### for any windows now that font objects are interned

 proc xHMresetFonts { win } {
     global xHMfonts
     foreach v [array names xHMfonts] {
	 xHMconfigFont $v
     }
 }

proc xHMfontPointSize { string } {
    #mike FIXME: hard coded font name and $string is ignored
    set si [font config font2 -size]
    return [expr { $si < 0 ? - $si : $si }]
}
}



proc xHMalterFont {win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist

#    puts "font:$args,[array get wvar *]"
    foreach v {family weight style size adjust}  {
	set $v [lindex $wvar($v) end]
    }

    set si $size
    if { [catch { set si [expr {$si + $adjust}] }] } {
	# puts "too many pops"
	return
    }
    set font font:$family:$weight:$style:$si
    if { ![catch { set fo $wvar(font) }] } {
	catch { unset taglist($fo) } }
#    puts "font=$font, wvar=[array get wvar fon*]"
    set  wvar(font) $font
    if { ![info exists wvar($font)] } {
	xHMsetFont $win $font }
    set taglist($font) 1
	
   # return "-*-$family-$weight-$style-normal-*-*-${size}0-*-*-*-*-*-*"
}

proc xHMsplitParams { param } {
    if { "$param" == "" } { return ""}
   set reg "(\[^= \t\n\]+)\[ \t\n]*((=\[ \t\n]*((\"(\[^\"\]*)\")|('(\[^'\]*)')|(\[^ \t\n\]*)))|(\[ \t\n\])|\$)"

   # set sub "{1=\\1,2=\\2,3=\\3,4=\\4,5=\\5,6=\\6,7==\\7,8=\\8,9=\\9}"
   # regsub -all $reg $param $sub  joe
   # puts joe=$joe
    
    set sub "\\1\\6\\8\\9"
    regsub -all $reg $param $sub  joe
    foreach { dummy key val } [lreplace [split $joe ] end end]  { lappend new [string tolower $key] $val}
    return $new
}

proc xHMextract_param {paramList  key args} {
    foreach { k val } $paramList {
	if { "$k" == "$key" } {
	    uplevel 1 set $key [list $val]
	return 1}}
	if { "$args" != "" } {
	    uplevel 1 set $key  [list [lindex $args 0] ]
	}
	return 0
    }

global xHMtag
catch {unset xHMtag}

defTag a -alter {Cdoaref doaref} -body xHMdo_a  -sbody xHMdo_/a 
defTag b -alter {weight bold }
defTag -body xHMdo_body 
defTag br -before "\n"
defTag center -alter {Ccenter center}
defTag cite -alter {style i} 
defTag code -alter {family fixed}
defTag dd -before "\n" -after "\n"
defTag dfn -alter {style i}
defTag dt -before "\n"
defTag em -alter {style i}
defTag h1 -alter {size 7 weight bold} -body {xHMassureNewlines 1} -after "\n"
defTag h2 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h3 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h4 -alter {size 5} -body {xHMassureNewlines 1} -after "\n"
defTag h5 -alter {size 4} -before "\n" -after "\n"
defTag h6 -alter {size 3 style i} -before "\n" -after "\n"
defTag i -alter {style i}
defTag img -body xHMdo_img

defTag kbd -alter {family fixed weight bold}
defTag li -body xHMdo_li 

defTag dl  -body xHMlistEnter -sbody xHMlistExit  
defTag dir  -body xHMlistEnter -sbody xHMlistExit 
defTag menu -body xHMlistEnter -sbody xHMlistExit 
defTag ol  -body {
    xHMlistEnter
    set wvar(listindex$wvar(indent)) 0} -sbody {
	xHMlistExit }	

defTag title  -body {wm title [winfo toplevel $win] $text ; set text ""} -sbody {list }
defTag ul -alter {Aindent 1} -body { xHMlistEnter
  set paramList [xHMsplitParams $params]
  set _iii -1
  if { [xHMextract_param $paramList type ""] } {
      set _iii [lsearch {disc circle square} $type]
  }
  if { $_iii < 0 } {
      set _iii [expr {($wvar(indent)/2 > 3 ? 3 : $wvar(indent)/2) -1 }]
     if { $_iii < 0 } { set _iii 0}
  }
  # push an index which will say disc, circle or square.
  xHMpushNamedTag $win ultype $_iii
}  -sbody { xHMlistExit ; catch { xHMpopNamedTag $win ultype }}
   

#defTag p -before "\n\n" -sbody {}
#defTag p -before "\n\n" -sbody {}
defTag p -body { xHMassureNewlines 1 } -sbody {}
defTag blockquote -before "\n\n" -after "\n"
defTag pre -alter {family fixed Cnowrap nowrap} -before "\n" /pre "\n"
defTag samp -alter {family fixed}
defTag strike -alter {Cstrike strike}
defTag strong -alter {weight bold}
defTag sup -alter {Csup sup}
defTag sub -alter {Csub sub}

defTag tt -alter {family fixed}
defTag u -alter {Cunderline underline}

defTag hrx  -body { $win insert $wvar(W_insert) "\n" ;
     $win insert $wvar(W_insert) "\n" hrule
    } -sbody {} 
defTag hr -before \n  -body { 
     $win insert $wvar(W_insert) "                  " underline
    } -sbody {} 

defTag var -alter {style i}

defTag hmstart -alter {	family propor   weight normal   style r   size 3
	list list 
        adjust 0 } -body { set wvar(counter) 0 }

defTag font -body {
    set paramList [xHMsplitParams $params]
    xHMpushNamedTag $win adjust [assoc size $paramList 0]
    xHMalterFont $win adjust
    }  -sbody {
	xHMpopNamedTag $win adjust
	xHMalterFont $win adjust
    }


proc notyet { args } 	{puts "not yet $args" }
defTag isindex -body xHMdo_isindex -sbody {}
defTag meta -body list -sbody list
defTag form  -before "\n" -after "\n"  -body {
    global xHMpriv
    set xHMpriv(form) [gensym form]
    upvar #0 $xHMpriv(form) form
    set paramList [xHMsplitParams $params]
    #puts "paramList=$paramList"
    if { [xHMextract_param $paramList action ""] } {
	set form(action) $action
    }
    xHMextract_param $paramList method "get"
    set form(method) $method

  } -sbody { global xHMpriv ;
    if { [info exists xHMpriv(form) ] } {
	upvar #0 $xHMpriv(form) form
	#puts form=$xHMpriv(form)
	#puts "form values=[array get form]"

	if { ![info exists form(f_has_submit)] } {
	    set params ""
	    xHMtextInsert $win "\n"
	    xHMdo_input submit
	}
	unset xHMpriv(form)
     }
    }
defTag input -body xHMdo_input
defTag select -body "xHMdo_input select" -sbody {
#    puts wvar=[array get wvar f_in_select]
    #catch {
    global xHMpriv
    upvar #0 $xHMpriv(form) form
    puts "\[array get wvar f_in_select*]=[array get wvar f_in_select*]"
    set na [lindex $wvar(f_in_select) 0]
     	
    set w $form(f_select,$na)
    foreach v [lrange $$wvar(f_in_select) 1 end] {
	$w.list insert end $v
    }
    xHMresetListbox $w $wvar(f_selected,$na)
    append form(f_reset) " ; xHMresetListbox $w [list $wvar(f_selected,$na)]"
    #puts $w
    if { [winfo exists ${w}label] } {
	#puts "have label $w and ${w}label"
	bind  ${w}label <1> "place $w -anchor center -relx 0 -rely 1.0 -bordermode outside -in ${w}label ; raise $w"
	bind  $w <Leave> "xHMresetListbox $w \[$w.list curselection\] ; place forget $w"
    }
    if { [$w.list cget -height] > 0  && [llength $wvar(f_select_values)] > [$w.list cget -height] } {
	scrollbar $w.scroll -orient v -command "$w.list yview" -takefocus 0
	$w.list configure -yscrollcommand "$w.scroll set"
	pack $w.scroll -side right -fill y
    }

    set form(f_select_list,$na) $wvar(f_select_values)
    if { [catch { unset wvar(f_selected,$na) }] } { puts "failed= unset wvar(f_selected,$na)"}
    if { [catch  { unset wvar(f_select_values) }] } { puts "failed=unset wvar(f_select_values)"} 
    #}
}

proc   xHMresetListbox  { w selected } {
    $w.list selection clear 0 end
    foreach v $selected { $w.list selection set $v}
    set i 0
    if { [llength $selected] > 0 } {
	set i [lindex $selected 0]
    }
    if { [winfo exists ${w}label] } {
	${w}label configure -text [$w.list get $i]
    }
}

defTag textarea -body "xHMdo_input textarea"
proc configColor { args } {
    set color [lindex $args end]
    if { [catch { eval $args } ] } {
	set color [lindex $args end]
	set args [lreplace $args end end "#$color"]
	catch { eval $args }
    }
}


defTag html -body "list " -sbody "list "
defTag head -body "list " -sbody "list "
defTag body -body {
    #puts "<body $params> $text"
     set paramList [xHMsplitParams $params]
    if { [xHMextract_param $paramList bgcolor ""] } {
	configColor $win config -background $bgcolor
	configColor $win tag  config hrule -font {courier 2} -background $bgcolor
    }
    if { [xHMextract_param $paramList baseprogram ] } {
        oset $win baseprogram [resolveURL $baseprogram [oget $win baseprogram]]
	oset $win baseprogram [decodeURL $baseprogram]
    }


    set _text $text
    if { [xHMextract_param $paramList text ""] } {
	 configColor $win config -foreground $text 
    }
    set text ${_text}
    foreach {ll tag} {evalrelief Teval resultrelief  Tresult aevalrelief currenteval resultmodifiedrelief Tmodified }  {
	if { [xHMextract_param $paramList $ll ""] } {
	    $win tag configure $tag -relief [set $ll]
	}
    }

    foreach {ll tag} {bgeval Teval bgresult Tresult bgresultmodified Tmodified bgaeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	      configColor $win tag configure $tag -background [set $ll] 
	}
    }
    foreach {ll tag} {link href alink currenthrefforeground eval Teval result Tresult resultmodified Tmodified aeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	configColor $win tag configure $tag -foreground [set $ll]
	}
    }
   } -sbody "list "
    
defTag base -body {       set paramList [xHMsplitParams $params]
   if { [xHMextract_param $paramList href ""] } {
       set wvar(baseurl) $href
      #xHMset_state $win baseurl $href
       oset $win baseurl $href
   }
  }
  
    

defTag option -body { set text [string trimright $text]
       set paramList [xHMsplitParams $params]
       xHMextract_param $paramList value $text
       lappend wvar(f_select_values) $value
       lappend wvar(f_in_select) $text
       if { [xHMextract_param $paramList selected] } {
	   #puts "hi==wvar(f_selected,[lindex $wvar(f_in_select) 0])"
	   lappend wvar(f_selected,[lindex $wvar(f_in_select) 0]) [expr {[llength $wvar(f_in_select)] -2}]
       }
       set text ""
}

global xHMpriv
set xHMpriv(counter) 0


#
 #-----------------------------------------------------------------
 #
 # ldelete --  remove all copies of ITEM from LIST
 #
 #  Results: new list without item
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc ldelete { item list } {
    while { [set i [lsearch $list $item]] >= 0} {
	set list [lreplace $list $i $i]
    }
    return $list
}
if { ![info exists _gensymCounter] } {set _gensymCounter  0}
proc gensym { name } {
    global _gensymCounter
    incr _gensymCounter
    set var ${name}_${_gensymCounter} 
    catch { uplevel #0  unset $var}
    return $var
}

proc xHMdo_input {{type ""}} {
    global xHMpriv
    if { ![info exists xHMpriv(form)] } {
	set xHMpriv(form) [gensym form]
    }
    upvar 1 win win
    upvar #0 $xHMpriv(form) form
    upvar #0 xHMvar$win wvar
    upvar 1 params params
    set form(url) $wvar(url)

    set paramList [xHMsplitParams $params]

    set w $win.input[incr wvar(counter)]
#    bindtags $w [ldelete maxlength [bindtags $w]]
    xHMextract_param $paramList name ""
   if { "$type" == "" } {
    xHMextract_param $paramList type text
   }
    xHMextract_param $paramList value ""
    set value  [xHMconvert_ampersand $value]
    switch -regexp -- $type {
	{text$|password|int$|string} {
	    xHMextract_param $paramList size 20
	    entry $w -width $size
	    if { "$type" == "password" } { $w config -show * }
	    if { [xHMextract_param $paramList maxlength] } {
		bindtags $w [concat [bindtags $w] maxlength]
		bind maxlength <KeyPress> "xHMdeleteTooLong $win %W"
		
		set wvar($w,maxlength) $maxlength
	    }

	    $w insert end $value

	    append form(f_reset) " ; $w delete 0 end ; $w insert end [list $value] "
	    set form(f_submit,$name) "$w get"
	}
	select {
	    xHMextract_param $paramList size 1
	    xHMextract_param $paramList mode single
	    set lis $w
	    if { $size == 1 } {
		set w ${w}label
		label $w -relief raised
	    }
	    frame $lis
	    listbox $lis.list  -selectmode $mode -width 0 -exportselection 0 -height [expr {$size > 1 ? $size : 0}]
	    pack $lis.list -side left

	    # will contain list "window value1 value2 value3 .."
	    # added to by <option>
	    set wvar(f_selected,$name) ""
	    set form(f_select,$name) $lis
	    set wvar(f_in_select) $name
	    set wvar(f_select_values) $name
	    # throw away any text after select
	    set text ""  
	    
	}
	textarea {
	    upvar 1 text text
	    xHMextract_param $paramList cols 30
	    xHMextract_param $paramList rows 5
	    catch { 
	      frame $w
	      puts "w=$w"
	    scrollbar $w.yscroll -command "$w.text yview" -orient v
	    text $w.text -height $rows -width $cols -wrap none \
		    -yscrollcommand "$w.yscroll set"  -padx 2 -pady 2
	     $w.text insert 0.0 $text
	
	    set text ""
	    pack $w.text
	    set form(f_submit,$name) "$w.text get 0.0 end"
	    append form(f_reset) " ; $w.text delete 0.0 end ; $w.text insert end [list $text]"
	} errm ;
	    puts errm=$errm;
	    
	}
	image {

	    xHMextract_param $paramList width 0
	    xHMextract_param $paramList height 0
	    xHMextract_param $paramList src "broken.ppm"
	    set form(f_has_submit) 1
	    catch { set base $wvar(url) ; set base $wvar(baseurl) }
	    label $w -image [xHMgetImage $win $src $base $width $height] \
		    -background [$win cget -background]
	    bind $w <ButtonRelease-1>   "xHMdoSubmit $w $xHMpriv(form) {$name.x %x $name.y %y}"
	    bind $w <Return> "xHMdoSubmit $w $xHMpriv(form) {$name.x 0 $name.y 0}"
	    bind $w <Leave> "$w configure -relief raised"
	    
	    }
	radio {

	    if { [catch { set var $form(radio,$name) } ] } {
		set var [set form(radio,$name) [gensym radio_value]]
	    }
	    radiobutton $w -variable $var -value $value -text " "
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) "; $w select"
		$w select
		
	    } else {
		append form(f_reset) "; $w deselect"
		$w deselect
		
	    }

	    set form(f_submit,$name) "uplevel #0 set $var"

	}
	checkbox {
	    ######### to do fix this..failed: http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Forms/example-4.html 
	    if { [catch { set var $form(checkbox,$name) } ] } {
		set var [set form(checkbox,$name) [gensym checkbox_value]]
	    }
	    xHMextract_param $paramList value on
	    checkbutton $w -on $value -variable $var -off _dontsubmit_ \
		    -text " "

	    set form(f_submit,$name) "uplevel #0 set $var"
	    
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) " ; $w select"
		$w select;
	    } else {
		$w deselect
		append form(f_reset) " ; $w deselect"
	    }

	}
	hidden {
	    set form(f_submit,$name) "list  [list $value]"
	    set w ""
	}
	reset {
	    if { "$value" == "" } {set value "Reset"}
	    button $w -text $value -command "xHMdoReset $xHMpriv(form)"

	}
	submit {
	    set form(f_has_submit) 1
	    if { "$value" == "" } { set value "Submit Query" }
	    if { "$name" != "" } {
		button $w -text $value -command [list xHMdoSubmit $w $xHMpriv(form) [list $name $value]]
	    } else {
		button $w -text $value -command "xHMdoSubmit $w $xHMpriv(form) [list {}]"
	    }
	
	}
    }
#    if { [info exists form(f_submit,$name)] } {
#	lappend form(f_tosubmit) $name
#    }
    #dputs "type=$type,w=$w"
    #dputs "form(reset)=$form(f_reset)"
    if { "$w" != "" } {
	#catch { puts "class=[winfo class $w]" }
	if { [catch {   $win window create $wvar(W_insert) -window $w -align bottom -padx 1 -pady 1 } ] } {
	    puts "$w bad window ?"
	}
	
	### todo handle focus of forms.. with tabbing.
	
    }
    
}

proc xHMsetSubmitPosition { formvar name x y } {
    upvar #0 $formvar form
    set form(f_submit,$name.x) "list $x"
    set form(f_submit,$name.y) "list $y"
}
    
    

proc xHMdoReset { formVar } {
    upvar #0 $formVar form
    eval $form(f_reset)
}
proc xHMdoSubmit { w formVar nameVals } {
    upvar #0 $formVar form
    set ans ""
    set win [omPanel $w]
    foreach { name value } $nameVals {
	puts "value=$value--><[xHMencode_get $value]>"
	if { "$name" != "" } { append ans "&$name=[xHMencode_get $value]"}
    }

#    foreach name $form(f_tosubmit) {
#	set val [eval $form(f_submit,$name)]
#	if { "$val" != "_dontsubmit_" } {
#	    append ans "&$name=[xHMencode_get $val]"
#	}
#    }
    set n [string length f_submit,]
    foreach {name value}  [array get form f_submit,* ] {
	 puts "form submit:[array get form f_submit,*]"
	set val [eval $value]
	puts "name=$name,val=$val-->[xHMencode_get $val]"
	if { "$val" != "_dontsubmit_" } {
	append ans "&[string range $name $n end]=[xHMencode_get $val]"
	}
    }
    # do the select listboxes:
    
    foreach { name w } [array get form f_select,*] {
	set name [string range $name [string length f_select,] end]
	
	set values [lrange $form(f_select_list,$name) 1 end]
	set ans1 ""

	foreach v [$w.list curselection] {
	    lappend ans1 [lindex $values $v]
	}
	puts w=$w.list,name=$name,ans1=$ans1,
	set ans1 [join $ans1 " "]
	append ans "&$name=[xHMencode_get $ans1]"
    }
    #puts ans=$ans
    #puts form=[array get form]
    set action $form(action)
    if { "[string tolower $form(method)]" == "get" } {
	xHMfindUrl $win $form(method) $form(action)?[string range $ans 1 end]
    } else {
	xHMfindUrl $win $form(method) $form(action) [string range $ans 1 end]
}   }

proc xHMfindUrl { win method  url { body "" }} {
    #puts "$win,$method,$url,$body"
    set method "[string tolower $method]"
    if { "$method" == "get" } {
	OpenMathOpenUrl $url -commandpanel $win
    } elseif { "$method" == "post" } {
	if { "$body" == "" } {set body " "}
	OpenMathOpenUrl $url -commandpanel $win -post $body
    }
}

proc xHMdeleteTooLong { win w } {
    upvar #0 xHMvar$win wvar
    catch { $w delete $wvar($w,maxlength) end }
    #puts $wvar($w,maxlength)
}

proc xHMconvert_ampersand { text } {
    if {![regexp & $text]} {return $text}
    regsub  -all {([[\\])|(&((#([0-9][0-9]?[0-9]?))|([a-zA-Z]+));?)} $text {[xHM_do1 \\\1  \5 : \6]} tmp
    return [subst -novariables $tmp]
}

proc xHM_do1 { a b {c xx} } {
    global isoLatin1
   if { "$a" == " " } {
      if { "$b" == ":" } {
	  #set result ?
	  if { [catch { set result $isoLatin1($c) }] } {
	     return "&$c"
	  }
	  return $result
      }    else {
      return [format %c $b] }
   } else { return [string index $a 0] }
}

proc xHMdo_li {} {
    uplevel 1 {
	set i $wvar(indent)
	set taglist(listindex) 1
	set text [string trimleft $text]
	if { ![catch { incr wvar(listindex$i) }] } {
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t$wvar(listindex$i).\t"
	    xHMpushAindent $win 1
	} else {
	    set ii 0
	    catch { set ii [lindex $wvar(ultype) end] }
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t"
	    xHMinsertBullet $win $ii 
	    xHMtextInsert $win "\t"
	    xHMpushAindent $win 1
	}
    unset  taglist(listindex)
 }
}

proc xHMinsertBullet { win i } {
    global xHMulBMPdata xHMpriv
    upvar #0 xHMvar$win wvar
    set fg [$win cget -foreground]
    set image ""
    if {[catch { set image $xHMpriv(ul,$fg,$i) }] } {
	catch { set image [set xHMpriv(ul,$fg,$i) [image create bitmap -data [lindex $xHMulBMPdata $i] -foreground $fg]] }
    }
    # if we cant get the image, or cant insert it fall back to
    # inserting a simple character
    if { "$image" == "" || [catch { $win  image create $wvar(W_insert) -image $image } ] } {
	if { $i > 2 } { set i 2}
	$win tag configure listindex -foreground red		
	xHMtextInsert $win [string range "oo*" $i $i]
    }
}

defTag th -body list
defTag td -body list
defTag tr -body list




    
proc xHMdo_a  {} {
   uplevel 1  {
       set paramList [xHMsplitParams $params]
       if { [xHMextract_param $paramList href] } {
	   # in case they forget </a>
	   foreach v [array names taglist h:*] {
	       unset taglist($v)
	   }
	   $win tag bind h:$href <Enter> "HMdoaref enter $win %x %y"
	   $win tag bind h:$href <Leave> "HMdoaref leave $win %x %y"
	   $win tag bind h:$href <1> "HMdoaref click $win %x %y"
	   set taglist(h:$href) 1
	   set taglist(href) 1
	   
       }
       if { [xHMextract_param $paramList name] } {
	   $win mark set anchor:$name "$wvar(W_insert) -1 chars"
	   $win mark gravity anchor:$name left
	   }
       }
}

proc xHMdo_/a  {} {
    uplevel 1 {
	foreach v [array names taglist h:*] { unset taglist($v) }
	catch {unset taglist(href)}
    }
}
    
proc xHMdo_body { win } {
    global xHMOptions
    upvar 1 params params
    upvar #0 xHMvar$win wvar
    set paramList [xHMsplitParams $params]
    foreach {key val } $paramList {
	catch { $win config -$key $val }
	set wvar(option,$key) $val
    }
}

proc xHMdo_img {} {
    upvar 1 params params
    upvar 1 wvar wvar
    upvar 1 taglist taglist
    upvar 1 win win
    set paramList [xHMsplitParams $params]

    xHMextract_param $paramList align bottom
    xHMextract_param $paramList border 1
    xHMextract_param $paramList width 0
    xHMextract_param $paramList height 0
    xHMextract_param $paramList src ""
#    xHMextract_param $paramList alt <image:[file tail $src]>
    xHMextract_param $paramList alt <image:$src>
    #puts "img:$src,$alt,$width,$height"
    if { [lsearch {bottom top center} $align ] < 0 } { set align bottom}
	set w $win.fr[incr wvar(counter)]
    set base ""
    set bg [$win cget -background]

    catch { set base $wvar(url) ; set base $wvar(baseurl) }
    if { [catch { set im [xHMgetImage $win $src $base $width $height] }] } {
	error "dont get here now"
	frame $w -width $width -height $height -background $bg
	label $w.label -text $alt -background $bg
	if { $width && $height } { pack  propagate  $w 0 }
	pack $w.label -fill both -expand 1
    } else {
	if { $wvar(measure) >= 0 } {
	    incr wvar(measure) [image width $image]
	}
	label $w -image $im -background $bg
	bind $w <Enter> [list set ws_openMath(load_rate) "$alt" ]
	bind $w <Leave> [list set ws_openMath(load_rate) ""  ]

    }
    catch { $w configure -border $border}
    set href [lindex [array names taglist h:*] 0]
    if { "$href" != "" }  {
	bind $w <1> "OpenMathOpenUrl [string range $href 2 end] \
			-commandpanel [omPanel $win]"
    }
    foreach v [array names taglist] { $win tag add $v $wvar(W_insert)}
    $win window create $wvar(W_insert) -window $w -align $align -padx 1 -pady 1
    
    
## to do add links for call backs
}
    
# return an image object..
proc xHMgetImage {win src baseurl width height } {
#     puts "$win,$src,$baseurl,$width,$height"
#     puts "getImage [resolveURL $src [decodeURL $baseurl]] $width $height"
    return [getImage [resolveURL $src [decodeURL $baseurl]] $width $height]
}

proc xHMget { url } {
}

proc xHMlistEnter {} 	{
    uplevel 1 {
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	set _ii [expr {(($wvar(indent) <= 0  ) ? 2 : 1)}]
	xHMpushAindent $win $_ii
	catch { unset wvar(listindex$wvar(indent))}
    }
}

proc xHMlistExit {} 	{
    uplevel 1 {
	set _ii [expr {($wvar(indent) <= 2) ? 2 : 1}]
	xHMpopAindent $win $_ii
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	
    }
}

proc dupString { s n } {
    set ans ""
    while { [incr n -1] >= 0 } { append ans $s }
    return $ans
}

### to do fix this to see how many blank lines there are at our insert
### point and to insert ones to make up.
proc xHMassureNewlines { n } {
    
    uplevel 1 set _n $n
    uplevel 1 {
	set _have 0
	foreach _v [lrange [split [$win get "$wvar(W_insert)-4char" $wvar(W_insert)] \n] 1 end] {
	    if { [string trim "$_v"  " "] == "" } {
		incr _have
	    } else { set _have 0}
	}
#    set _have  [$win  compare $wvar(W_insert) == "$wvar(W_insert) linestart"]
	xHMtextInsert $win [dupString "\n" [expr {$_n - $_have}]]
    }
}

global xHMpreferences
set xHMpreferences(adjust) 0
catch {
    set width_ [expr {.9 * [winfo screenwidth .]}]
if { [winfo width .] >= 500 } {  set width_ [winfo width .] }
set xHMpreferences(adjust) [expr {
    $width_<= 640 ? -1 :
    $width_<= 800 ? 0 :
    1 } ]
   unset width_    
}
proc xHMsetDefaultPreferences {} {
    global xHMpreferences 
    foreach fam {propor fixed} {
	foreach {n si} {  1 8
    2 10
    3 12
    4 14
    5 18
    6 24
    7 24
    8 34	
    } { set xHMpreferences($fam,$n) $si}}
    set xHMpreferences(propor,adjust) [expr {$xHMpreferences(adjust) + 0}]
    set xHMpreferences(fixed,adjust) [expr {$xHMpreferences(adjust)  + 0}]
    array set xHMpreferences { propor arial fixed courier  indentwidth .7 }
}

xHMsetDefaultPreferences
catch { source ~/netmath.ini }

proc dputs {x}  { puts $x ; flush stdout}
proc xHMinit_state { win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    global xHMpreferences
    array set saveme [array get wvar W_*]
    catch { unset wvar}
        catch { unset taglist}
    array set wvar {
	family propor   weight normal   style r   size 3
	list list 
	indent 0
	adjust 0
	measure -1
	W_insert insert
	W_update 15
    }
    array set wvar [array get saveme]
    array set taglist {indent:0 1}
    
}

proc xHMrender { win tag  params text } {
    global xHMtag
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set prefix ""

    set tag [string tolower $tag]
    # the following will go in a catch after debugging:
    #dputs "doing <$tag>"
    #dputs text=<<$text>> 
    # puts "xHMtag($tag)=[set xHMtag($tag)]"


   # eval [set xHMtag($tag)]
    if { [info exists xHMtag($tag)] } {
	# if { [catch { eval [set xHMtag($tag)] }] } { puts "error evaling tag:$tag" }
	eval [set xHMtag($tag)]
    } else {
	if { [string match "!--*" $tag] } { list} else {
	#puts "undefined $tag: puts comment:$text"
    }   }
    
 
    if { [regexp & $text] }  {
       set text [xHMconvert_ampersand $text]
    }

    #dputs "nowrap=[info exists taglist(nowrap)]"
    if { ![info exists taglist(nowrap)] } {
	regsub -all "\[ \t\r\n\]+" $text " " text
	if { "$prefix" != "" } { set text [string trimleft $text] }
    }
    xHMtextInsert $win $prefix$text
}

# make a copy of it.
proc xHMrender_orig [info args xHMrender] [info body xHMrender]

 
proc xHMtextInsert { win text } {
    global xHMtaglist$win
    upvar #0 xHMvar$win wvar
    # dputs "$win insert $wvar(W_insert) [list $text] [list [array names xHMtaglist$win ]]"
    # we calculate the longest unbroken line...
    if { 0 && $wvar(measure) >= 0 } {
	# puts "hi"
	set fo [xHMmapFont  $wvar(font)]
	set lis [split $text \n]
	set ll [font measure $fo [lindex $lis 0]]
	incr wvar(measure) $ll
	foreach vv [lrange $lis 1 end] {
	    maxIn wvar(maxwidth) $wvar(measure)
	    set wvar(measure)   [font measure $fo $vv]
	}
	maxIn wvar(maxwidth) $wvar(measure)
    }
    $win insert $wvar(W_insert) $text [array names xHMtaglist$win ]
}

proc xHMset_state { win args } {
    upvar #0 xHMvar$win wvar

    array set wvar $args

}

proc toPixelWidth { dim win } {
    if { [regexp {([.0-9]+)c} $dim junk d] } {
	return [expr {round($d*[winfo screenwidth $win] /(.1*[winfo screenmmwidth $win]))}] } else { return $dim}
    }
	

proc xHMinit_win { win } {
    upvar #0 xHMvar$win wvar
    global xHMpreferences
    # global xHMvar$win
   # catch { unset xHMvar$win }
    xHMinit_state $win
    $win config -font [xHMmapFont font:fixed:normal:r:3]
    catch { eval destroy [winfo children $win] }
    set iwidth [toPixelWidth  [set xHMpreferences(indentwidth)]c $win]
    # puts iwidth=$iwidth
    for { set i 0 } { $i < 12 } { incr i } {
	set half [expr {$iwidth/2.0 }]
	set w [expr {$i * $iwidth}]
	$win tag configure indent:$i -lmargin1 ${w} -lmargin2 ${w} -tabs \
		"[expr {$w + $half}] [expr {$w + 2*$half}]"
    }
   # $win tag bind doaref <Enter> "HMdoaref enter $win %x %y"
   # $win tag bind doaref <Leave> "HMdoaref leave $win %x %y"
   # $win tag bind doaref <1> "HMdoaref click $win %x %y"

    $win tag configure indent:0 -lmargin1 ${half} -lmargin2 ${half} -tabs "${half} [expr {2 * $half}]"
    $win tag configure href -borderwidth 2 -foreground blue -underline 1

    $win tag configure nowrap -wrap none
    $win tag configure rindent -rmargin $iwidth
    $win tag configure strike -overstrike 1

    $win tag configure underline -underline 1
    $win tag configure center -justify center
    $win configure -wrap word
}

global HMdefaultOptions
set HMdefaultOptions {
    {atagforeground blue "foreground for <a href=...>  tags"}
    {currenthrefforeground red "foreground of current <a href=..> tags"} 
    {foreground black "foreground"}
    {background white "background "}
    {atagbackground blue "background for <a href=...>  tags" }
}

foreach v $HMdefaultOptions {set HMOption([lindex $v 0]) [lindex $v 1] }

proc xHMwget { win key dflt } {
    upvar #0 xHMvar$win wvar
    if { [info exists wvar($key)] } {return $wvar($key) } else {
	return $dflt
}   }
    
proc HMdoaref { action win x y } {
    global HMOption
    set tags [$win tag names  @$x,$y ]
    set i [lsearch $tags h:*]
    set tag [lindex $tags $i]
    set reference [string range [lindex $tags $i] 2 end]
    # puts "$action $x $y"do_a
    switch $action {
	enter {
	    if { $i >= 0  }  {
		set ranges [$win tag ranges $tag]
		eval $win tag add currenthref $ranges
		textShowHelp $win currenthref @$x,$y "Click to follow link to $reference"

		$win tag bind $tag <Leave> "deleteHelp $win ;$win tag remove currenthref $ranges"
		$win tag  config currenthref -foreground [xHMwget $win option,atagforeground $HMOption(currenthrefforeground)] }
	    }
	click {
	    if { $i>= 0 } {
		global [oarray $win]
		if { [info exists [oloc $win dontopen]] } {
		    unset [oloc $win dontopen]
		} else {
		    oset $win dontopen 1
		    OpenMathOpenUrl $reference \
			    -commandpanel [omPanel $win]
		    catch {  unset [oloc $win dontopen] }
		}
		    return
	    }

	}
	    leave {
		
		$win tag delete currenthref
	    }
	}
    }   

proc xHMdo_isindex {} {
    uplevel 1 {
	set paramList [xHMsplitParams $params]
	xHMextract_param $paramList prompt " Enter search keywords: "
	xHMtextInsert $win $prompt
	set w $win.entry[incr wvar(counter)]
	entry $w
	# puts "wvar=[array get wvar]"
        $win window create $wvar(W_insert) -window $w  -padx 1 -pady 1
	bind $w <Return> "xHMget $wvar(url)?\[xHMencode_get \[$w get\]\]"
    }
}

# encode a string where
#  " " --> "+"
#  "\n" --> "%0d%0a"
#  [a-zA-Z0-9] --> self
#   c --> [format %.2x $c]

# make a list of all characters, to get char code from char.
global xHMallchars
set xHMallchars ""
for { set i 1} { $i <256 } {incr i } { append xHMallchars [format %c $i] }

proc xHMhexChar { c } {
    global xHMallchars
    set i [string first $c $xHMallchars]
    return %[format %.2x [expr {$i + 1}]]
}

# "ISO 8879-1986//ENTITIES Added Latin 1 substitutions
array set isoLatin1 {
    	AElig \xc6 	Aacute \xc1 	Acirc \xc2 	Agrave \xc0 
	Aring \xc5 	Atilde \xc3 	Auml \xc4 	Ccedil \xc7 
	ETH \xd0 	Eacute \xc9 	Ecirc \xca 	Egrave \xc8 
	Euml \xcb 	Iacute \xcd 	Icirc \xce 	Igrave \xcc 
	Iuml \xcf 	Ntilde \xd1 	Oacute \xd3 	Ocirc \xd4 
	Ograve \xd2 	Oslash \xd8 	Otilde \xd5 	Ouml \xd6 
	THORN \xde 	Uacute \xda 	Ucirc \xdb 	Ugrave \xd9 
	Uuml \xdc 	Yacute \xdd 	aacute \xe1 	acirc \xe2 
	acute \xb4 	aelig \xe6 	agrave \xe0 	amp \x26 
	aring \xe5 	atilde \xe3 	auml \xe4 	brvbar \xa6 
	cb \x7d 	ccedil \xe7 	cedil \xb8 	cent \xa2 
	copy \xa9 	curren \xa4 	deg \xb0 	divide \xf7 
	eacute \xe9 	ecirc \xea 	egrave \xe8 	eth \xf0 
	euml \xeb 	frac12 \xbd 	frac14 \xbc 	frac34 \xbe 
	gt \x3e 	hibar \xaf 	iacute \xed 	icirc \xee 
	iexcl \xa1 	igrave \xec 	iquest \xbf 	iuml \xef 
	laquo \xab 	lt \x3c 	micro \xb5 	middot \xb7 
	nbsp \xa0 	not \xac 	ntilde \xf1 	oacute \xf3 
	ob \x7b 	ocirc \xf4 	ograve \xf2 	ordf \xaa 
	ordm \xba 	oslash \xf8 	otilde \xf5 	ouml \xf6 
	para \xb6 	plusmn \xb1 	pound \xa3 	quot \x22 
	raquo \xbb 	reg \xae 	sect \xa7 	shy \xad 
	sup1 \xb9 	sup2 \xb2 	sup3 \xb3 	szlig \xdf 
	thorn \xfe 	times \xd7 	uacute \xfa 	ucirc \xfb 
	ugrave \xf9 	uml \xa8 	uuml \xfc 	yacute \xfd 
	yen \xa5 	yuml \xff 
}

proc xHMencode_get { str } {
    regsub -all "\[^a-zA-Z0-9\]" $str "\[xHMencode_get1 {x&x}]" str
    regsub -all "{x(\[{}\])x}" $str \{\\\\\\1x\} str
    return [subst  -novariables -nobackslashes $str ]
}

proc xHMencode_get1 { s } {
    set c [string index $s 1]
    switch -- $c {
	\n  { return %0d%0a }
	" " { return + }
	default { return [xHMhexChar $c ]}
    }
}


proc HexDecode { me }  {
    regsub -all {\+} $me " "  me
  if { [regexp % $me] } {
     regsub -all {\[} $me {[dec1 5b]} me
    regsub -all {%([0-9A-Fa-f][0-9A-Fa-f])} $me {[dec1 \1]}  me
    subst -nobackslashes -novariables $me
 } else { return $me }
}
proc dec1 { s } {
    if { [scan  $s %x d] } {
	format %c $d
    } else { error "cant decode hex $s" }
}




#
 #-----------------------------------------------------------------
 #
 # xHMparse_html --  takes HTML containing valid html code, and
 #  converts it into a sequence of calls to CMD.   These
 #  CMD should take 4 arguments:
 #     tagname slash tagArguments followingText
 #  where slash is {} or {/} depending on whether the TAGNAME was
 #  prefixed with a '/'.   The tagAguments are not parsed: eg
 #  <foo bil=good joe> hi there <next> this is
 #  would turn into
 #  $CMD {foo} {} {bil=good joe} {hi there}
 #  $CMD {next} {} {}   {this is..}
 #  We have tried to stay call compatible with a similar command
 #  written by Stephen Uhler.   Our handling of all the tags is different
 #  however.
 #
 #  Results: none
 #
 #  Side Effects: the sequence of $CMD is evald.
 #
 #----------------------------------------------------------------
#
proc xHMparse_html {html {cmd HMtest_parse} {firstTag hmstart}} {
    #dputs "beginning parse"
    
     global meee ; set meee $html;
	regsub -all \} <$firstTag>\n$html\n</$firstTag> {\&cb;} html
        #dputs "beginning parse1"
	regsub -all \{ $html {\&ob;} html
        # prevent getting \} \{ or \\n in a braces expression.
    	regsub -all "\\\\(\[\n<>])" $html "\\&#92;\\1" html
	#regsub -all "<(/?)(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
		"\}\n$cmd {\\2} {\\1} {\\3} \{" html
    	regsub -all "<(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
		"\}\n$cmd {\\1}  {\\2} \{" html
        # puts "<html=$html>"
        #dputs "beginning end splitparse1"
        
        #dputs "list {$html}"
	eval "list {$html}"
 
}

proc myPost { win menu } {
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor center -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}

## source "myhtml1.tcl"

###### myhtml1.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

defTag eval -alter {family fixed Cnowrap nowrap adjust 0} \
	-body {
    set paramList [xHMsplitParams $params]
    if { [xHMextract_param $paramList program ""] } {
	set wvar(evalPushed) "Teval"
	xHMpushConstantTag $win Teval
	foreach { k val } $paramList {
	    if { "$k" == "doinsert" } {
		set doinsert $val
		if { "$doinsert" != "[defaultInsertMode $program]" } {
		    lappend wvar(evalPushed) [list Targs -doinsert $doinsert]
		    xHMpushConstantTag $win [list Targs -doinsert $doinsert]
		}
	    } else {
		set tem "$k:$val"
		xHMpushConstantTag $win $tem
		lappend wvar(evalPushed) $tem
	    }
	}
    }
   }  -sbody {
       catch {foreach v $wvar(evalPushed) { xHMpopConstantTag $win $v } }
   }
   

defTag result  -alter {family fixed  weight bold  adjust 0} -body {
    set paramList [xHMsplitParams $params]
    set wvar(resultPushed) Tresult
    set taglist(Tresult) 1
    if { [xHMextract_param $paramList modified ""] } {
	lappend wvar(resultPushed) Tmodified
	set taglist(Tmodified) 1
    }
    if { [xHMextract_param $paramList name ""] } {
	lappend wvar(resultPushed) result:$name
	set taglist(result:$name) 1
    }}   -sbody {
       catch {foreach v $wvar(resultPushed) { xHMpopConstantTag $win $v } }
}

defTag netmath -body {
    set paramList [xHMsplitParams $params]
    catch {
    if { [xHMextract_param $paramList version ""] } {
	global ws_openMath
	if { [clock scan $version] > [clock scan $ws_openMath(date)] } {

	    xHMextract_param $paramList oldversion ""
	    append oldversion $text
	    set text $oldversion
	}
    }
#  swallow the following text if the browser is netmath enabled...
    if { [xHMextract_param $paramList swallow] } {
	set text ""
    }
    
  }
}

defTag math -body {
    set paramList [xHMsplitParams $params]
    upvar #0 xHMtaglist$win taglist
    global xHMpriv 	 xHMpreferences
    set pre {$}
    if { [xHMextract_param $paramList display] } {
	set pre {$\displaystyle}
	xHMassureNewlines 1
    }
	
    set wc $win.c[incr xHMpriv(counter)]
    canvas $wc 		-background [$win cget -background] -highlightthickness 0 -borderwidth 0
    
    set si [expr {[lindex $wvar(size) end] + [lindex $wvar(adjust) end]}]
    if { [xHMextract_param $paramList size] } {
	catch { incr si $size }
    }
    set si [expr {($si < 1 ? 1 : ($si > 7 ? 7 : $si))}]
    
    set ptsize $xHMpreferences([lindex $wvar(family) end],$si)
    if { [regexp & $text] }  {
	set text [xHMconvert_ampersand $text]
    }
    if { [catch { set it [ $wc create stext 0 0 \
	    -anchor nw -stext "$pre $text \$" -pointsize $ptsize \
	    ] } ]  } {
	xHMpushConstantTag $win "center"
	xHMtextInsert $win $text
	xHMpopConstantTag $win "center"
	set text ""
	destroy $wc
    } else {
	set text ""
	set dims [$wc bbox $it]
	$wc config -width [lindex $dims 2] -height [lindex $dims 3]
	xHMpushConstantTag $win "center"
	xHMtextInsert $win " "
	$win window create $wvar(W_insert) -window $wc  -padx 1 -pady 1
	xHMpopConstantTag $win "center"
    }
} -sbody {list }

proc getDim { dim  max } {
    if { [regexp {([0-9.]+)%$} $dim junk amt] } {
	return [expr {round($amt * .01 * $max) }]
    } elseif { $dim  < 0 } {
	return $max
    } else { return $dim }
}

defTag embed  -body {
    set paramList [xHMsplitParams $params]
    xHMextract_param $paramList width -1
    # allow for things like 50%
    set width [getDim [set width] [expr {.95 * [winfo width $win]}]]
    xHMextract_param $paramList height -1
    set height [getDim [set height] [expr {.95 * [winfo height $win]}]]
    set ewin [makeEmbedWin $win $width $height]
    $win window create $wvar(W_insert) -window $ewin  -padx 1 -pady 1
    set slave [oget $ewin slave]
    if { [    xHMextract_param $paramList src] } {
	set data [HMgetURL $win $src text]
	interp eval $slave $data
    }
} -sbody {}


proc HMgetURL {  textwin url type } {
    set currentUrl [oget $textwin currentUrl]
    catch { set currentUrl [decodeURL [oget $textwin baseurl]] }
    set new [resolveURL $url $currentUrl ]
    return [uplevel 1 getURL [list $new] type]
}

    
    
    
    
    

## endsource "myhtml1.tcl"
# source "mytable1.tcl"



## endsource myhtml.tcl
## source base64.tcl

###### base64.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
# aaaaaabb bbbbcccc ccdddddd

proc tobase64 { binary } {
    set ll [string length $binary ]
    set n [binary scan  $binary "c*" ans]
    lappend ans 0 0 0
    foreach { x y z } $ans {
#	puts "$x $y $z $n"
	catch { 
	append new  \
		[char64 [expr {(($x & 255)>>2) }]][char64 \
		[expr {((($x & 3)<<4) | (($y >> 4) & 15))}]][char64 \
		[expr {((($y & 15)<<2) | (($z &255) >> 6))}]][char64 \
		[expr {($z & 63) }]]
	    
	}
    }
    return $new
}

proc char64 { x } {
    string range "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" $x $x
}
    
## endsource base64.tcl
## source bitmaps.tcl

###### bitmaps.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

global xHMulBMPdata
set xHMulBMPdata ""
lappend xHMulBMPdata "#define disc_width 6\n#define disc_height 6
static unsigned char disc_bits[] = {
   0xde, 0xff, 0xff, 0xff, 0xff, 0xde};"
    
lappend xHMulBMPdata  "#define circ_width 8\n#define circ_height 8
static unsigned char circ_bits[] = {
   0x3c, 0x42, 0x81, 0x81, 0x81, 0x81, 0x42, 0x3c};"

lappend xHMulBMPdata "#define rect_width 11\n#define rect_height 11
static unsigned char rect_bits[] = {
   0xff, 0x07, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04,
   0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0xff, 0x07};"




    

## endsource bitmaps.tcl
## source tryembed.tcl

###### tryembed.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

## the following worked to have an entry box that spoke... 
# %     safe::interpCreate jack
# jack
# % set slave jack
# jack
# %     safe::interpInit $slave
# jack
# %     interp eval $slave set env(DISPLAY) $env(DISPLAY)
# :0.0
# %     load {} Tk $slave
# % interp eval jack {entry .ja ; pack .ja}
# %     interp eval $slave { proc policy {args } {} }
# %     Safesock_PolicyInit $slave
# %     setupUnknown $slave
# %     setupPrintVariables $slave
# % interp eval jack plot2d -xfun {sin(x)}




proc makeEmbedWin { parent width height } {
    global ws_openMath env auto_index
    set win $parent.embed[incr ws_openMath(counter)]
    set fr [frame $win -width $width -height $height -container 1]
    set slave tclet$ws_openMath(counter)
    safe::interpCreate $slave
    # make it exist somehow the autoload stuff doesnt make it a command
    if { [info exists auto_index(::safe::allowTk) ]  } {
	::safe::allowTk $slave [list -use [winfo id $fr]]
	::safe::TkInit $slave
	::safe::tkInterpInit $slave [list -use [winfo id $fr]]
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    } else {
	safe::interpInit $slave
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    }

    if { [info exists env(DISPLAY)] } {
    interp eval $slave set env(DISPLAY) $env(DISPLAY)
    }
    interp eval $slave { proc policy {args } {} }
#    $slave alias bgerror bgerror
    load {} Tk $slave
    Safesock_PolicyInit $slave
    setupUnknown $slave
    setupPrintVariables $slave
    oset $fr slave $slave
    return $fr
}


proc setupUnknown { slave } {
    interp eval $slave {rename auto_load auto_load-orig}
    interp alias $slave auto_load1 {} auto_load1 $slave
    interp eval $slave { proc auto_load {args} {
	if { [eval auto_load1 $args] } { return 1 }
	uplevel 1 auto_load-orig $args
    }
 }
}



proc auto_load1 { slave name {namespace ""} } {
   if { "[info proc $name ]" != "" } {
       set arglist [info args $name]
       set theargs {}
       foreach v $arglist {
	   if { [info default $name $v theDefault] } {
	       lappend theargs [list $v $theDefault]
	   } else { lappend theargs $v
	   }
       }
       interp eval $slave [list proc $name $theargs [info body $name]]
       return 1
   }
   return 0
}

proc setupPrintVariables { slave } {
    global printOption fontSize show_balloons getOp parse_table Parser     axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile     doExit  fontCourier8   plotdfOptions ftpInfo  ws_openMath
    foreach v {printOption fontSize show_balloons getOp parse_table Parser
    axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile
    doExit  fontCourier8   plotdfOptions ftpInfo ws_openMath} {
	if { [array exists  $v] } {
	    interp eval $slave [list array set $v [array get $v *] ]
	} else {  interp eval $slave [list set $v [set $v ]]
	}
    }

}
# proc tryit { {win .}  } {
#  global  ws_openMath
#     if { ![info exists ws_openMath(counter)] } {
# 	set ws_openMath(counter) 0
#     }
#     set width [winfo width $win]
#     set height [winfo height $win]
#     if { $width <=1 } {set width 200}
#     if { $height <=1 } {set height 200}
#     set ff [makeEmbedWin $win $width $height]
    
#     return [list $ff [oget $ff slave]]
# }

if { "[info command policy]" != "policy" } {
    proc policy { args } { }
}

proc browser_log { args } {
    # puts "$args"
}

## source nsafesock.tcl

###### nsafesock.tcl ######
# The Safesock Security Policy.
# -----------------------------
#
# Author: Jacob Levy & Brent Welch, 3/10/97
#
# This policy allows a safe slave to connect to remote sockets under the
# control of a master. The URL from which the applet is classified as
# either "inside" or "outside" and the host is added to the set of "inside"
# and "outside" hosts that this Tclet is allowed to connect to. Then, on
# the first request to connect to a host, if the host is classified as
# "inside" then subsequently the Tclet is allowed to connect only to hosts
# that are classified as "inside" (the same for if the first attempt is to
# connect to a host classified as "outside").
#
# The arrays used to drive this policy are defined in safesock.data.

# Remember the location of the data file for the Safesock policy, so that
# it can be reloaded each time the policy is used, to reflect changes.

global safesockDataFile
set safesockDataFile [file join [file dirname [info script]] safesock.data]

proc Safesock_PolicyInit {slave {version 1.0}} {
    global browser_state		;# Browser state
    global safesock_inside safesock_outside

    interp alias $slave socket {} SafesockSocketAlias $slave
    interp alias $slave fconfigure {} SafesockFconfigureAlias $slave

    uplevel #0 {source $safesockDataFile}


    # Attempt to get the URL and extract the server and port portions:

    set server "" ; set port "" ; set url ""
    catch {set url $browser_state($slave,url)}
    if {[regexp -nocase {http://([^:/]+)(:([0-9]+))?/} $url \
		x server y port]} {
	if {[string length $port] == 0} {
	    set port 80
	}
	set server [string tolower $server]
    } elseif {[string match "file:*" $url]} {
	set server localhost
	set port 80
    }

    # At this time it is unknown whether the slave will use inside
    # or outside connections:

    set browser_state($slave,safesock,permissions) unknown

    # Save the homebase for this Tclet:

    set browser_state($slave,safesock,homebase) $server
    set browser_state($slave,safesock,homeport) [list $port 1025- ftp ping]

    # Tell the slave about itself:

    interp eval $slave [list set env(SERVER) $server] 
    interp eval $slave [list set env(PORT) $port]
    interp eval $slave [list set env(URL) $url]

    browser_log $slave security installed policy Safesock
}

proc SafesockDecideInsideOrOutside {slave server} {
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside

    set status unknown

    # If the server matches anything outside and nothing in the outside
    # exclusion list, then it's outside:

    foreach i [array names safesock_outside] {
	if {[string match $i $server]} {
	    set status outside
	    break
	}
    }

    if {"$status" == "outside"} {
	foreach i [array names safesock_outsideExclude] {
	    if {[string match $i $server]} {
		set status unknown
		break
	    }
	}
    }

    # If the status is unknown, check whether it might be inside. It is
    # inside if the server matches anything inside and nothing in the
    # inside exclusion list:

    if {"$status" == "unknown"} {
	foreach i [array names safesock_inside] {
	    if {[string match $i $server]} {
		set status inside
		break
	    }
	}

	if {"$status" == "inside"} {
	    foreach i [array names safesock_insideExclude] {
		if {[string match $i $server]} {
		    set status unknown
		    break
		}
	    }
	}
    }

    # If the status is unknown at this point, raise an error

    if {"$status" == "unknown"} {
	error "unknown host: $server"
    }

    return $status
}

# This procedure is invoked when the slave is destroyed to clean up
# any associated state. It frees up the array of hosts and ports that
# the slave is allowed to connect to:

proc Safesock_PolicyCleanup {slave} {
    global browser_state

    foreach i [array names browser_state $slave,safesock,*] {
	unset browser_state($i)
    }
}


#
 #-----------------------------------------------------------------
 #
 # SafesockServerAnswer --  will replace COMMAND in a `socket -server command'
 #  request.   Checks if the incoming connection is allowed and if so
 #  invokes the original command.   Allowed is based on the same criteria
 #  as the outgoing connection.   
 #
 #  Results: none
 #
 #  Side Effects: if connect is allowed, transfer the socket to the slave
 #  and eval the original command there. 
 #
 #----------------------------------------------------------------
#
proc SafesockServerAnswer { slave command sock host port } {
    set peer [fconfigure $sock -peername]
    set host [lindex $peer 1]
    set host [string tolower $host]
    if { [SafesockAllow $slave $host [lindex $peer 2]] > 0 } {
	interp transfer {} $sock $slave
	interp eval $slave $command $sock $host $port
    } else { interp eval $slave [list error "connection from $host and $port disallowed"]
   }
}



#
 #-----------------------------------------------------------------
 #
 # SafesockAllow --  check if connection by SLAVE to HOST at PORT is allowed,
 #  based on the inside/outside history of slave and data in safesock.data
 #
 #  Results: 1 if succeeds and 0 if it fails to allow
 #
 #  Side Effects:  set GOOD to ok port in the caller 
 #
 #----------------------------------------------------------------
#
proc SafesockAllow { slave host port} {
    global browser_state
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside
    upvar 1 good good
    set host [string tolower $host]
    if {"$browser_state($slave,safesock,permissions)" == "unknown"} {
	if {[catch {set this [SafesockDecideInsideOrOutside $slave $host]}]} {
	    if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	        set this homebase
	    } else {
	        error "unknown host: $host"
	    }
	}
	set browser_state($slave,safesock,permissions) $this
	browser_log $slave security $slave classified as $this
    }

    set portset -
    if {"$browser_state($slave,safesock,permissions)" == "homebase"} {
	if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	    set portset $browser_state($slave,safesock,homeport)
	}
    } elseif {"$browser_state($slave,safesock,permissions)" == "inside"} {
	foreach hostpat [array names safesock_inside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_inside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_insideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    } else {
	foreach hostpat [array names safesock_outside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_outside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_outsideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    }

    if {"$portset" == "-"} {
	error "unknown host: $host"
    }

    if { [safesockPortMatches $port $portset] } {
	set good $port
	return 1
    }
    return 0
}

proc safesockPortMatches { port portset } {
    foreach portspec $portset {
	set low [set high ""]
	if {[regexp {^([0-9]+)-([0-9]*)$} $portspec x low high]} {
	    if {($low <= $port && $high == "") ||
			($low <= $port && $high >= $port)} {
                return 1
		break
	    }
	} elseif {$port == $portspec} {
	    return 1
	}
    }
    return 0
}

# the following should be set in safesock.data
if { ![info exists safesockAllowedServerPorts ] } {
  set safesockAllowedServerPorts { 1025-3000 }
}

proc SafesockSocketAlias {slave host port args} {
    global safesockAllowedServerPorts
    set option {}
    if { "$host" == "-server" } {
	set command $port
	set port [lindex $args 0]
	if { ![safesockPortMatches $port $safesockAllowedServerPorts] } {
	     error "bad port: $port"
	}
	set sock [socket -server \
		"SafesockServerAnswer $slave [list $command]" $port]
	interp transfer {} $sock $slave
	browser_log $slave normal socket -server $port
	return $sock
    } elseif { "$host" == "-async" } {
	set option $host
	set host $port
	set port [lindex $args 0]
    } else {
	if { [llength $args ] != 0 } {
	    error "wrong args: socket host port OR socket -server command port"
	}
	set serverCommand ""
    }
    SafesockAllow $slave $host $port
    if [info exists good] {
	if { "$option" != "" } { 
	    set sock [interp invokehidden $slave socket $option $host $good]
	} else { set sock [interp invokehidden $slave socket $host $good]}
	browser_log $slave normal socket $host $port
	return $sock
    }
    error "bad port: $port"
}

# This procedure handles the "fconfigure" alias from the slave:

proc SafesockFconfigureAlias {slave sock args} {
    global jack
    if {[llength $args] == 0} {
	return [interp invokehidden $slave fconfigure $sock]
    } elseif {[llength $args] == 1} {
	set flag [lindex $args 0]
	return [interp invokehidden $slave fconfigure $sock $flag]
    } else {
	browser_log $slave normal fconfigure $sock $args

	array set config [interp invokehidden $slave fconfigure $sock]
	foreach {flag value} $args {
	    switch -- $flag {
		-peername -
		-peerport {
		    error "Cannot change $flag configuration"
		}
		-blocking -
		-buffering -
		-buffersize -
		-eofchar -
		-translation {
		    set config($flag) $value
		}
		default {
		    error "unknown option $flag"
		}
	    }
	}
	lappend jack [list interp invokehidden $slave fconfigure $sock \
	    -blocking $config(-blocking) \
	    -buffering $config(-buffering) \
	    -buffersize $config(-buffersize) \
	    -eofchar $config(-eofchar) \
	    -translation $config(-translation)]
	return [interp invokehidden $slave fconfigure $sock \
	    -blocking $config(-blocking) \
	    -buffering $config(-buffering) \
	    -buffersize $config(-buffersize) \
	    -eofchar $config(-eofchar) \
	    -translation $config(-translation)]
    }
}

## endsource nsafesock.tcl
# source /home/wfs/openmath/nsafesock.tcl





## endsource tryembed.tcl

if { ![info exists ws_openMath(date)] } {
    set ws_openMath(date) [clock  format [clock seconds] -format {%m/%d/%Y} ]
}

global ws_openMath
set ws_openMath(fixedFont) Courier
# the linelength should be long enough to display formatted mathematical
# output from things like maxima, without adjustment, and to allow
# for a margin.
set ws_openMath(linelength) 90

proc genSample { x n } {
    set sample $x
    set m 1
    while { 1 } {
	if { $m >= $n } { return $sample }
    	if { [set tem [expr {2*$m}]] <= $n } {
	    append sample $sample
	    set m $tem
	} else {
	    return [append sample [genSample $x [expr {$n - $m}]]]
	}
    }
}

option add *Button.font [font create -family Courier -size $fontSize]

	
# font measuring is very slow so we cache the result of measuring a line
# of x's.
proc fontMeasure { font size } {
   global  ws_openMath
   set ll $ws_openMath(linelength) 
   if { ![catch {set answer [set $ws_openMath($font,$size,$ll)]} ] } { return $answer}
  set sample [genSample x $ll]
  set  ws_openMath($font,$size,$ll)  [font measure [list $font $size] $sample]
  return $ws_openMath($font,$size,$ll)
}

global fixedFont
set fixedFont Courier
proc getDefaultFontSize { width } {
    global fixedFont
    set answer "10 480"
    catch { 
    set wid1 [fontMeasure $fixedFont 10]
	set guess [expr {round($width/double($wid1) * 10.0)}]
    while { [fontMeasure $fixedFont $guess] < $width && $guess <= 14 } {
        incr guess }
    incr guess -1
    while { [fontMeasure $fixedFont $guess] > $width } { incr guess -1 }
    set answer   [list $guess [fontMeasure $fixedFont $guess]]
    }
    return $answer

}
    
proc getMaxDimensions { } {
  global embed_args
  set dims "800 600"
 if { [catch { set dims "$embed_args(width) $embed_args(height)" } ] } {
   set dims "[expr round(.85* [winfo screenwidth .])] [expr round(.9* [winfo screenheight .])]"} else {
       set dims "[getPercentDim [lindex $dims 0] width .] [getPercentDim [lindex $dims 1] height .]"
   }
  return $dims
}

proc getPercentDim { dim direction win } {
    if { [regexp {([0-9]+)%} $dim junk val] } {
	set dim [winfo $direction $win]
	catch { set dim [expr {round($val * $dim * .01)}] }
	return $dim
    }
    return $dim
}

proc computeTextWinDimensions { win width height } {
  # leave room for scroll bar
   global fixedFont ws_openMath
    # desetq "fsize wid" [getDefaultFontSize [expr {$width -15}]]
    set wid $width
   set fixedFont [xHMmapFont font:fixed:normal:r:3]
   set fsize [xHMfontPointSize $fixedFont]
   
   set lh [expr {$fsize +1}]
   catch {   set lh [font metrics $fixedFont -linespace] }
   oset $win fixedFont $fixedFont
   oset $win fontSize $fsize
   oset $win width $width
   oset $win width_chars $ws_openMath(linelength)
   set hei [expr {round($height/$lh)}]
   oset $win height_chars $hei
   oset $win height [expr {$hei * $lh}]
   oset $win lineheight $lh
   }



proc setFontOptions { fontSize }     {
    global buttonfont entryfont labelfont ws_openMath

    set fsize $fontSize
    if { $fontSize > 10 } { set fsize 12 }
    if { $fsize == 8 } { set entrysize 10 } else {set entrysize $fsize }
    #puts "fsize=$fsize"
    catch {
	set  buttonfont [font create -family Helvetica -size $fsize]
         set  labelfont [font create -family helvetica -size $fsize]
           set  fixedtextfont [font create -family courier -size $fsize]
           set  entryfont [font create -family courier -size $entrysize]
           option add *Button.font $buttonfont
           option add *Label.font $labelfont
           option add *Entry.font $entryfont
           option add  *Dialog.msg.wrapLength 500

    }

}
proc omPanel { w args } {
    global buttonfont entryfont labelfont ws_openMath
    set top [winfo toplevel $w]    
    linkLocal $top omPanel
    if { [info exists omPanel] } {return $omPanel }
    set top [winfo parent $w]
    # 
    if { "$top" == "." } { set top ""}	  
    set win $top.textcommands
    set omPanel $win
    makeLocal $w fontSize
    setFontOptions $fontSize

    global [oarray $top.textcommands]
    set menubar $top.textcommands
    if { [winfo exists $menubar] } {
	return $menubar
    }
    oset $win history ""
    oset $win historyIndex 0
    wmenubar $menubar
    pack $menubar -side top -expand 0 -fill x -anchor nw

    foreach v { back forward  file edit help  } {
	label $win.$v -text $v -font $buttonfont -relief raised
	$menubar add $win.$v
    }
    bind $win.back <Button-1>  "OpenMathMoveHistory $win -1"
    bind  $win.forward <Button-1> "OpenMathMoveHistory $win 1"
    setHelp $win.forward {Move forward in the history of documents visited.}
    setHelp $win.back {Move backward in the history of documents visited.}
 

    ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    oset $win showEditBar "show edit bar"
    
   # $m add command -help "Toggle viewing of an editor bar which allows marking text with properties, eg marking to evaluate in maxima"  -textvariable [oloc $win showEditBar] -command "toggleEditBar $win"
    oset $win currentProgram maxima
    oset $win currentProgramInsert 1
    $m add command -underline 0 -help {_eval {Mark the currently selected region of the text window, so that clicking on the region will cause evaluation by a program.  The program specified below is [oget [omPanel %W] currentProgram].}} -label "mark for eval"  -command  "markForProgram \[oget $win textwin\]"

    global evalPrograms
 #   regsub -all eval_ [concat $evalPrograms [info proc eval_*]] "" programs
    set programs $evalPrograms
    foreach v $programs {
	$m add radio -help "Set `$v' to be the current type used by 'mark for eval' for making the selected region sensitive to double clicking." -label "$v" -value $v -variable [oloc $win currentProgram] -command "setTypeForEval $m $v"
    }
    frame $m.program
    $m invoke 1
    pack $m.program

   #  $m add check -help "Toggle whether Mark for Eval should expect the program to insert a result, or should just evaluate for effect." -label "Eval Insert" -onvalue 1 -offvalue 0 -variable [oloc $win currentProgramInsert]
    

    

  # ====begin Help button===    
    set m [oget $win.help menu]
    setHelp $win.help {Offer possible help options, including toggling \
	    whether to show balloon help messages}

    global show_balloons showHelpMessages
    set show_balloons 1 ; set showHelpMessages "Hide Balloon Help"
    $m add command -textvariable showHelpMessages -command {set show_balloons [expr {!$show_balloons}]; if { $show_balloons} {after 500 set showHelpMessages [list "Hide Balloon Help" ]} else {after 500 set showHelpMessages [list "Show Balloon Help" ]}}
    label $m.date -text "Version $ws_openMath(date)"
    $m add window -window $m.date
   
    
    

    
    
  # ====begin File button===
    set m [oget $win.file menu]
    setHelp $win.file {Menu of file options, for saving and preferences} 
    global [oarray $win]
    $m add command -label "Reload" -command {OpenMathOpenUrl [oget [omPanel %W] location] -reload 1 -commandpanel [omPanel %W]} -help {_eval {Reload the current URL displayed in the entry window: [oget [omPanel %W] location]}}
    $m add command -label "Interrupt" -command {omDoInterrupt [oget [omPanel %W] textwin]} -help {Try to interrupt the current remote computations.}
    $m add command -label "Abort" -command {omDoAbort [oget [omPanel %W] textwin]} -help {Try to abort the current remote computation.}
    $m add command -label "Stop" -command {omDoStop [oget [omPanel %W] textwin]} -help {Stop reading the current url or image.}
    $m add command -label "Forget" -command  "forgetCurrent $win"   -help {Move back one in the history, and remove the current one from the history, unless it was the first window.}
    $m add command -label "History" -command  "showHistory $win"   -help {Display the history list, so that one may be selected by clicking.}
    $m add command -label "Base Program" -command  {fileBaseprogram [oget [omPanel %W] textwin]  %W %x %y}   -help {Show and allow altering of the base program, which shows which is the default host for programs to run on.   May also be specified in <body baseprogram= ...> in the .html file.}        
    set new [$m add entry -label {Save file:} -help {_eval {Save to the file list below([oget [omPanel %W] savefilename]).  Not available when running inside Netscape}}]
    $new.entry configure  -textvariable [oloc $win savefilename]
    bind $new.entry  <Return> "saveToFile $win $new.label \[oget $win savefilename\] "
    bind $new.label  <Button-1> "saveToFile $win $new.label \[oget $win savefilename\] "
    $m add command -label "Exit" -command  "destroy ."   -help {Exit this program}        
#    $m add command -label "Print not yet!" -command "puts printing" -underline 0 -help {You may print individual graphs using their menu bars, but printing the whole file is not yet implemented.}
    $m add command -label Preferences -command "fontDialog .fontdialog" -help {set the default font sizes and types}
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
		-command "console show" -help \
		{This console is used mainly in debugging netmath}
    }



    global location
    button $win.loclabel -text " Url:" -command "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win" -font $labelfont
    setHelp $win.loclabel {Fetch the URL or FILE indicated in the entry box. \
	    A local file is something like file:/home/wfs/foo.om, and a URL \
	    begins with http.}
    
    pack $win.loclabel -side left -fill x -expand 0
    entry $win.location -textvariable [oloc $win location] -width 40
    setHelp $win.location {Address of the current document.  You may modify it and type Enter, to fetch a new document.}
    bind $win.location <Key-Return> "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win"
    pack $win.location  -side left -fill x -expand 1
    label $win.locspace -text " "
    pack $win.locspace -side left -fill x -expand 0

    oset $win history ""
    pack $win -side top -expand 1 -fill x

    ######## make status panel....
    set st $top.status
    frame $st
    oset $win status $st

    set ws_openMath(status_window) $st
    scale $st.scale -showvalue 0 -length 200 -orient horizontal
    label $st.rate -width 35 -font $labelfont -textvariable ws_openMath(load_rate)
    pack $st.rate $st.scale -side left 
    pack $st -side bottom
    return $win
}

proc forgetCurrent { win } {
    makeLocal $win history historyIndex
    set i 0
    if { [llength $history] > 1 } {
	set w [lindex $history $historyIndex]
	set history [lreplace $history $historyIndex $historyIndex]
	# might have caused two identical ones to be next to each other
	if { "[lindex $history $historyIndex]" == "[lindex $history [expr {$historyIndex -1 }]]" } {
	    set history [lreplace $history $historyIndex $historyIndex]
	    set i -1
	}
	if { [lsearch  $history $w] < 0 } {
	    after 2000 "destroy $w"
	}
	oset $win history $history
	OpenMathMoveHistory $win $i
    }
}

proc omDoStop { win } {
    global ws_openMath
    set st $ws_openMath(status_window)
    set var [$st.scale cget -variable]
    if { [regexp {sock[0-9]+} $var sock] } {
	oset $sock done -1
	if { ![catch { close $sock} ] } {
	    
	    append ws_openMath(load_rate) "--aborted"
	}
    }
}
    
    



#
 #-----------------------------------------------------------------
 #
 # setTypeForEval --  insert special editing of options, into MENU for PROGRAM
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc setTypeForEval { menu program } {
    global ws_openMath
    #puts "$menu program"
    set slaves [pack slaves $menu.program ]
    set men $menu.program.$program
    if { [llength $slaves] > 0 } {eval pack forget $slaves}
    if { ![catch { set options $ws_openMath(options,$program) } ] } {
	if { ![winfo exists $menu.program.$program] } {
	    #puts "options=$options"
	   # puts "there"

	    ### set up to add menu items to a new frame
	    set key $menu.program

	    frame $men
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    oset $menu.program menu $men
	    oset $men items ""
	    oset $key parent $menu
	    proc $men {option args } $body
	    
	    ##### end
	    

	    foreach v $options {
		desetq "key dflt help" $v
		
		if { [catch { set ws_openMath(options,$program,$key)} ] } {
		    set ws_openMath(options,$program,$key) $dflt
		}
		switch [lindex $v 3] {
		    boolean {
			$men add check -label $key -variable ws_openMath(options,$program,$key) -help [concat $program option -$key: $help] -onvalue 1 -offvalue 0
		    }
		    default {
            		$men add entry -label "$key:" -entryvariable ws_openMath(options,$program,$key) -help [concat $program option -$key: $help]

		    }

		}

		
#		label $new.label -text $key:
#		entry $new.entry  -textvariable ws_openMath(options,$program,$key)
#		pack $new.label $new.entry -side top -anchor w -fill x
#		pack $new -fill x
#		setHelp $new [concat $program option -$v: $help]

	    }
	}
	
    }
    catch { pack $men}

}


#
 #-----------------------------------------------------------------
 #
 # getGlobalOptions --  Convert the current global options for program,
 # to an option list:  -key1 value1 -key2 value2 ..
 #
 #  Results: the option list
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc getGlobalOptions { program } {
    global ws_openMath
    set ans ""
    if { ![catch { set options $ws_openMath(options,$program) } ] } {
	foreach v $options {
	    set key [lindex $v 0]
	    set dflt [lindex $v 1]
	    if { ![catch { set val $ws_openMath(options,$program,$key) }] } {
		if { "$val" != "$dflt" } {
		    lappend ans -$key $val
		}
	    }
	}
    }
    return $ans
}


#
 #-----------------------------------------------------------------
 #
 # setGlobalOptions --  set the current global values of the options for PROGRAM
 # according to the values specified in OPTIONLIST.   If a value is not specified
 # use the value supplied in the defaults: $ws_openMath(options,$program)
 #
 #  Results:  none
 #
 #  Side Effects: the entries ws_openMath(options,$program,$key) are changed
 #  for each $key which is an option for program.
 #
 #----------------------------------------------------------------
#
proc setGlobalOptions { program list } {
    global ws_openMath
    if { [catch { set options $ws_openMath(options,$program) } ] } {
	  foreach  v $options {
	      set key [lindex $v 0]
	      set dflt [lindex $v 1]
	      set $ws_openMath(options,$program,$key) \
		      [assoc -$key $list $dflt]
	  }
      }
  }

proc toggleEditBar  {win} {
    makeLocal $win showEditBar editbar
    if { [winfo viewable $editbar] }  {
	pack forget $editbar
	oset $win showEditBar "show edit bar"
    } else { pack $editbar -in $win -side bottom -expand 1 -fill x
		oset $win showEditBar "hide edit bar"
    }
}


proc getPrefixed { prefix  tags } {
    set i [lsearch $tags ${prefix}*]
    if { $i >= 0 } {
	return [string range [lindex $tags $i] [string length $prefix] end]
    } else {
	return ""
    }
}
    
proc programFromTags {tags} {
    if {[lsearch $tags Teval ] < 0 } {
	return ""
    }
    return [getPrefixed program: $tags]
}

proc saveToFile { commandPanel label file } {
    makeLocal $commandPanel textwin
    global ftpInfo
    $label configure -relief sunken
    set lab [$label cget -text]
   # set text [saveText $textwin 0.0 end]
    # save just as text
    set text [$textwin get 0.0 end]
    if { [catch { set fi [open $file w] } ] } {
	set ftpInfo(directory) [string trimleft [file dirname $file] /]
	if { "$ftpInfo(directory)" == "" } { set ftpInfo(directory) . }
	set ftpInfo(data) $text
	ftpDialog $textwin -filename [file tail $file]
	return 0
	# myerror "Could not open file $file"
    }
    puts $fi $text
    close $fi
    $label configure -relief raised -text "wrote $file"
    after 1200 [list $label configure -text $lab]
}

if { [catch { package require Safesock } ] } {
    catch { policy  home }
	# catch {  policy outside }
	
    }


proc mkOpenMath { win  } {
    global    ws_openMath
   
    set w $win
    catch {destroy $w}
    if { [catch { package require Safesock } ] } {
	   # policy network home
	catch {  policy  outside }
    }
    desetq "width height" [getMaxDimensions]
    computeTextWinDimensions $win $width $height 
    makeLocal $win fontSize width_chars height_chars fixedFont
    set font $fixedFont
    
    # puts "fontSize=$fontSize"
    frame $w
    set commandPanel [omPanel $w ]
    oset $w commandPanel $commandPanel
    set prevwindow ""
    catch { set prevwindow [oget $commandPanel textwin] }
    oset $commandPanel textwin $w.text
   # pack $commandPanel -in $w -side top -fill x -pady 2m
   # raise  $commandPanel
    set fontSize2  [expr {2+$fontSize}]
    if { $fontSize2 >= 14 } { set fontSize2 14}
    text $w.text -yscrollcommand "$w.scroll set" \
	 -width $width_chars  -height $height_chars -font $font -wrap word
    bind $w.text <Configure> "resizeSubPlotWindows $w.text %w %h"
    set ws_openMath(currentwin) $w.text
    set ws_openMath(point) end
    $w.text tag bind "currenteval" <Leave> "$w.text tag remove currenteval 0.0 end ; addTagSameRange %W Teval currenteval @%x,%y;"
    $w.text tag config "currenteval" -foreground red
    $w.text tag bind Teval <Double-Button-1> {doInvoke %W @%x,%y }
    $w.text tag bind Teval <Enter> {addTagSameRange %W Teval currenteval @%x,%y; textShowHelp %W Teval @%x,%y "Double clicking (with the left mouse button), in the marked region will cause evaluation. "}
    $w.text tag bind Teval <Leave> {deleteHelp %W}
    $w.text tag config hrule -font {Courier 1} -background black
    $w.text mark set insert 0.0
  # try "#d0d0d0" or "#ffffd0" or yellow
    $w.text tag configure Teval -foreground blue -font [font create -family Courier -size $fontSize]  -border 1 -lmargin1 20

    

    $w.text tag configure bold -font [xHMmapFont font:propor:bold:r:3] -lmargin1 15
    $w.text tag configure plain -font [xHMmapFont font:propor:bold:r:3] -lmargin1 10 
    $w.text tag configure Tresult -font [xHMmapFont font:fixed:bold:r:3] -lmargin1 10
    $w.text tag configure Tmodified -font [xHMmapFont font:fixed:normal:r:3] -background pink -relief sunken -border 1
    $w.text tag configure Thref -font [xHMmapFont font:fixed:normal:r:3]  -foreground blue  -relief flat 

    set lh [oget $win lineheight]
    $w.text tag configure sub -offset [expr {-round($lh*.6) }]
    $w.text tag configure sup -offset [expr {round($lh*.6) }]

    
    oset $w.text counter 0
    # allow some openmath text bindings to take precedence
    bindtags $w.text "OpenMathText [bindtags $w.text]"
    scrollbar $w.scroll -command "$w.text yview"
    pack $w.scroll -side right -fill y
    pack $w.text -expand 1  -fill both
    pack $w -expand 1 -fill both
    if {[winfo exists $prevwindow] } { pack forget [winfo parent $prevwindow] }
    return  $w.text
    
}

#source emaxima.tcl
#source egp.tcl

# Create bindings for tags.

# set ActiveTags {
#   gap-eval
#   gap-eval-insert
#   octave-eval
#   octave-eval-insert
#   face-jump-to-bkmark
#   xlsp-eval
#   xlsp-eval-insert
#   gcl-eval
#   gcl-eval-insert
#   emacs-lisp-eval
#   emacs-lisp-eval-insert
#   mma-eval
#   mma-eval-insert
#   Splus-eval
#   Splus-eval-insert
#   gp-eval
#   gp-eval-insert
#   maple-eval
#   maple-eval-insert
#   shell-eval-region
#   gnuplot-eval
#   xplot-eval
#   maxima-eval
#   maxima-eval-insert
#   dfplot-eval
#   book-shell-eval-insert
#   book-image-insert
#   book-postscript-insert
#   book-tex-math-mode
#   book-elisp-eval
#   book-shell-eval
#  }

global evalPrograms
set evalPrograms {  gp gap gb }
#set ws_openMath(options,maxima) {{doinsert 1 "Do an insertion" boolean}}
#set ws_openMath(options,gp) {{doinsert 1 "Do an insertion" boolean}}
# set ws_openMath(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# add in Toctave, Topenplot, Thref etc... ie ones with eval_* defined
foreach v [info proc insertResult_*] {
    lappend evalPrograms [string range $v 13 end]
}


#
 #-----------------------------------------------------------------
 #
 # defaultInsertMode --  each program can have a default insert mode.
 #  If the insert method is not noted specifically then it uses the default.
 #  maxima and gp have default to insert.
 #  Results: 0 or 1
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc defaultInsertMode { program } {
    global ws_openMath
    if { [catch {  set dflt [getOptionDefault doinsert $ws_openMath(options,$program)]} ] } { return 1}
 
    if { "$dflt" == "" } {set dflt  1}
    return $dflt
}

proc doInsertp { tags } {
    set program [programFromTags $tags]
   # puts "program=$program," ; flush stdout
    return [getEvalArg -doinsert $tags [defaultInsertMode [programName $program]]]
}


#
 #-----------------------------------------------------------------
 #
 # doInvoke --  invoked when user clicks on WINDOW at INDEX
 # this will either call the program whose tag is in the list of
 # tags at this point, on the expression which is highlighted for this
 # or else call the special code in eval_$program if the latter exists.
 #  Results: none
 #
 #  Side Effects: The modified result of the insert field will be cleared,
 #  and the value there will be changed.
 #----------------------------------------------------------------
#
proc doInvoke { w index } {
    global evalPrograms MathServer
   set tags [$w tag names $index]
    
    $w tag delete sel

    set program [programFromTags $tags]
    if { "$program" == "" } {
	return
    }
   # puts "base=[oget $w baseprogram],w=$w"
   set res [resolveURL $program [oget $w baseprogram]]
   # puts "program=$program,baseprogram[oget $w baseprogram],res=$res"
   
   set MathServer "[assoc server $res [lindex $MathServer 0]] \
	   [assoc port $res [lindex $MathServer 1]]"
   set this [thisRange $w  program:$program $index]
   # puts "this=$this"

   set nextResult ""
   set doinsert [doInsertp $tags]
   # puts "doinsert=$doinsert"
   
   if { $doinsert} {
       set name [getPrefixed name: $tags]
       if { "$name" != "" } {
	   set nextResult [$w tag nextrange result:$name [lindex $this 1]]
	   if { 0 == [llength $nextResult] } {
	       error "No result field with name=$name"
	   }
       } else {
 	   set next [$w tag nextrange Teval [lindex $this 1]]
	   set nextResult [$w tag nextrange Tresult [lindex $this 1]]
	   if {
	       [llength $nextResult] == 0
	       ||    ([llength $next] !=0
	       &&  [$w  compare [lindex $nextResult 0] > [lindex $next 0]] )
	   } {
	    $w insert "[lindex $this 1]+1 char" " " "Tresult"
	     set nextResult [$w tag nextrange Tresult [lindex $this 1]]   
	   # error "no place to put result"
         }
      }
      if { "$nextResult" != "" } {
	  eval $w  tag add Tmodified $nextResult
      }
   } 
   set prog [programName $program]
   if { [info proc eval_$prog] != "" } {
       if {[eval_$prog $program $w $this $nextResult] != 0 }  {
	   error "Failed to eval region"
       }
   } else {
       global err
       if { [catch { sendOneInsertTextWin $program [eval $w get $this] $w $this $nextResult} err ] && [regexp "Can't connect" $err ]} {
	   global xHMpreferences
	   set now [encodeURL [oget $w baseprogram] ]
	   set tem [ldelete $now $xHMpreferences(defaultservers)]
	   if { [tk_dialog .jil 0 "$err: connect to one of $tem?" "" 0 change "keep $now"] == 0 } {
	       set xHMpreferences(defaultservers)  $tem
	       oset $w baseprogram [decodeURL [getBaseprogram]]
	       doInvoke $w $index
	       return
	   } else { return
	   }

       }
   }
	       
   
}

proc getEvalArg { key names {dflt ""} } {

    foreach v $names {
	if { "[string range $v 0 5]" == "Targs "} {
	    return [assoc $key [lrange $v 1 end] $dflt]
	}
    }
    return $dflt
}


#
 #-----------------------------------------------------------------
 #
 # setModifiedFlag --  add the Tmodified tag to the next Tresult field
 #  after the current expression.
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc setModifiedFlag { win index } {
  if { [lsearch [$win tag names $index] Teval] >= 0 } {
      set next [$win tag nextrange Tresult $index]
      if { "$next" != "" } {
	 eval $win  tag add Tmodified $next
      }
  }
}


#
 #-----------------------------------------------------------------
 #
 # insertResult --  replace RESULTRANGE of the text buffer by VALUE,
 #  and clear the Tmodified tag if there is one.
 #  most eval_$program programs will call this to insert their result.
 #  Results:  
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc insertResult { w resultRange value } {
       set tags [$w tag names [lindex $resultRange 0]]
       set value [xHMuntabify $value]
       # append a newline to a multiline result that has no newline after it.
       if { [regexp "\n.*\[^\n]\$" $value ] } {append value "\n"} 
       eval $w delete $resultRange
       # dont lose the whole thing!!
       if { "$value" == "" } { set value " "}
       $w insert [lindex $resultRange 0] $value  [ldelete Tmodified $tags]
   }




#
 #-----------------------------------------------------------------
 #
 # addPreloads --  Tack any preloads or preevals on to the 
 #  command.
 #  Results: the new COMMAND
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#   
proc addPreloads {command program win this } {   
   set preload [getTagsMatching $win ^pre(load|eval):* $this]
    if { "$preload" != "" &&  ![preeval $program $preload] } {
	if { [regexp \{pre(load|eval):(.*)\} $preload junk op url] ||
	[regexp pre(load|eval):(.*) $preload junk op url]} {
	    if { "$op" == "load" } {
		set res [HMgetURL $win $url type]
		append res $command
		set command $res
	    } else {
		append url $command
		set command $url
	    }

    }
   }
   return $command
}


#
 #-----------------------------------------------------------------
 #
 # sendOneInsertTextWin --  send PROGRAM the COMMAND for insertion
 # in the text window WIN at RANGE.   There may be a program specific
 # insertResult_maxima, .. in which case this does the job.   It  
 # is also passed the field of where the command came from.
 # We mark these fields with a tag, since they may get moved by typing
 # before the result comes back.   The com:* tags also provide omDoAbort
 # with the program names that are currently active, so that it can abort.
 #  Results:
 #
 #  Side Effects: until the evaluation succeeds the tags
 #  res:pdata($PROGRAM,result,$i) and a similar com: indicate the
 #  result field, and the command field. 
 #
 #----------------------------------------------------------------
#   
proc sendOneInsertTextWin { program command win this range} {
    set eval [getTagsMatching $win ^eval(sub|):* $this]
    if { "$eval" != "" } {
	if { [regexp \{eval(sub|):(.*)\}  $eval junk op val ]  } {
	    if { "$op" == "sub" } {
		regsub -all "\\&" $val $command val
	    }
	    set command $val
	}
    }
    set command [addPreloads $command $program $win $this ]
    
   # puts "preload=$preload,command:$command"
    set loc [sendOneDoCommand $program $command "sendOneInsertTextWin1 $win $program "]
    if { "$range" != "" } {
	$win tag add res:$loc [lindex $range 0] [lindex $range 1]
    }
    $win tag add com:$loc [lindex $this 0] [lindex $this 1]
}

proc sendOneInsertTextWin1 { win program location } {
    #puts "entering trace:sendOneInsertTextWin1 $win $location"
    #flush stdout
    message "received result"
    set resultRange [$win tag nextrange res:$location 0.0]
    set this [$win tag nextrange com:$location 0.0]
    $win tag delete res:$location com:$location    
#    if { "$resultRange" == ""} {
#	puts "somebody removed result place for $location"
#	return ""
#    }

   if { "[info command insertResult_[programName $program]]" != "" } {
       insertResult_[programName $program] $win $this $resultRange \
	       [uplevel #0 set $location]
   } else {
    insertResult $win $resultRange [uplevel #0 set $location]
   }
     uplevel #0 unset $location
}
   
    

proc xHMuntabify { s } {
    set lis [split $s \n]
    set ans [lindex $lis 0]
    foreach v [lrange $lis 1 end] {
	append ans \n[xHMuntabifyLine $v]
    }
    return $ans
}

proc xHMuntabifyLine { s } {
    set l [split $s \t]
    set ans [lindex $l 0]
    set rest [lrange $l 1 end]
    foreach w $rest {
	set n [expr {[string length $ans]%8}]
	append ans [string range "        " $n end]
	append ans $w
    }
    return $ans
}


#
 #-----------------------------------------------------------------
 #
 # textBbox --  Compute the bounding box of a range of characters
 # starting at IND1 and running to IND2.  
 #
 #  Results: return "x y width height" where x, y are the coordinates
 #  of the upper left corner.
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc textBbox { win ind1 ind2 } {
  
    foreach i { 1 2 } {
	set ind [eval $win index [set ind$i]]
	set ind$i $ind
	set line$i [lindex [split $ind .] 0]
	if { [catch {desetq "x$i y$i xdim$i ydim$i" [eval $win bbox $ind]}] } {
	    # not visible
	    return ""}
    }
    if { $line1 == $line2 } {
	return "$x1 $y1 [expr {$x2-$x1+$xdim2}] [expr {$y2-$y1+$ydim2}]"
    } else {
	set xrange "$x1 $x2+$xdim2"
	set yrange "$y1 $y2+$ydim2"
	
	for { set j $line1 } { $j < $line2 } { incr j } {
	    desetq "x y xdim ydim" [$win dlineinfo $j.0]
	    set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	    set yrange [minMax $yrange $y [expr {$y + $ydim}]]
	}
	desetq "x y xdim ydim" [$win dlineinfo $line2.0]
	set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	set yrange [minMax $yrange [expr {$y + $ydim}]]
	desetq "x1 x2 y1 y2" "$xrange $yrange"
	return "$x1 $y1 [expr {$x2 - $x1}] [expr {$y2 - $y1}]"

    }
}
	
proc textShowHelp { win tag index msg } {
    set this [thisRange $win $tag $index]
    if { "$this" == "" } { return }
    set tags [$win tag names $index]
    if { "$tag" == "Teval" } {
	set program [programFromTags $tags]
	if { "$program" != ""} {
	    set msg [string trimright $msg ". "]
	    append msg " by $program."
	}
	if { [doInsertp $tags] } {
	    append msg " The result will be inserted."
	}
	if { "[getPrefixed name: $tags]" != "" } {
	  append msg  "  The result field is named `[getPrefixed name: $tags]'."
	}
    }
    if { [catch { desetq "x y wid hei" [eval textBbox $win  $this] } ] } {
	# cant get position
	return ""
    }
    set top [winfo toplevel $win]
    
    set x [expr {$x + [winfo rootx $win] - [winfo rootx $top]}]
    set y [expr {$y + [winfo rooty $win] - [winfo rooty $top]}]
    

    
 
    #puts "showHelp $win $x $y $wid $hei"
    showHelp "$win $x $y $wid $hei" $msg
}

proc getTagsMatching { win regexp range } {
    foreach ind $range {
	foreach v [$win tag names $ind] {
	    if { [regexp $regexp $v] } {
		set there($v) 1
	    }
	}
    }
    set dump [eval $win dump -tag $range]
    set i 1
    set ll [llength $dump]
    while { $i < $ll } {
	set v [lindex $dump $i]
	if { [regexp $regexp $v] } {
	    set there($v) 1
	}
	incr i 3
    }
    return [array names there]
}

proc markForProgram { w args } {
    global evalTags
    set win [omPanel $w]
    set program [assoc -program $args [oget $win currentProgram]]
    set range [assoc -range $args [$w tag nextrange sel 0.0]]
    if { "$range" == ""} {
	return ""
    }
    set tags [assoc -tags $args ""]
    if { "$tags" == ""} {
	set tags [list Teval program:$program ]
	set opts [getGlobalOptions [programName $program]]
	if { "$opts" != ""} {  lappend tags [concat Targs $opts] }
    }
    # puts "tags=$tags"
    eval $w tag remove Teval $range
    foreach v [getTagsMatching $w "^Targs |^program:" $range] {
	eval $w tag remove [list $v] $range
    }
    foreach v $tags {eval $w tag add [list $v] $range}
    set insert [doInsertp $tags]
    if { $insert } {
	set nextResult [$w tag nextrange Tresult [lindex $range 1]]
	set next [$w tag nextrange Teval [lindex $range 1]]
	if { [llength $nextResult] == 0 ||
	([llength $next] !=0)
	&&  [$w compare [lindex $nextResult 0] > [lindex $next 0]] } {

	set templates [list " yields " " evaluates to "  \
	    " returns " " produces " " gives "]
        $w mark set tmp [lindex $range 1]	    

	    $w insert tmp [lindex $templates [expr {[clock clicks]%[llength $templates]}]] plain
        $w insert tmp RESULT {Tresult Tmodified}
        $w insert tmp " "  {plain}
    } else { apply $w tag add Tmodified $nextResult}
    
  }
}

global ws_openMath
set ws_openMath(counter) 0
	

## endsource preamble.tcl
#source macros.tcl
#source private.tcl
## source nconsole.tcl

###### nconsole.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


proc mkConsole { fr program } {
    catch { destroy $fr }
    global NCtextHelp
    frame $fr
    pack $fr -expand 1 -fill both
    set w $fr.text
    label [set msg $fr.label]  -height 1 -relief sunken \
	    -textvariable ws_openMath(load_rate)
    
    if { ![info exists NCtextHelp] } {
	set NCtextHelp "
	    Bindings:
	    <Return>   This sends the current expression (ie where the insert
	               cursor is)  for evaluation.
	    <Linefeed> (Control-j) This inserts a newline, and is useful
	               for entering multiline input.
	    <Control-k> Kills the current line and puts it in kill ring.
	                Successive control-k's append their output together.
	    <Control-y> Yank out the last kill, Meta-y cycles thru previous
	                kills.
	    <Control-c> Interrupt the current computation.
	    <Alt-p>   Previous input, or if repeated cycle through the previous
	               inputs.  If the current input is not empty, then
	                match only inputs which begin with the current input.
	    <Alt-n>   Like Previous input, but in opposite direction.
	"
    }

    clearLocal $w
     if { 1 || "[bind CNtext <Return>]" == "" } {
 	bind CNtext <Return> "CNeval %W  ; break"
 	bind CNtext <Control-c> "CNinterrupt %W "
 	bind CNtext <Control-u> "CNclearinput %W "
 	bind CNtext "\)"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\]"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\}"  "CNblinkMatchingParen %W %A"
 	bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
 	bind CNtext <Alt-p>  "CNpreviousInput $w -1"
 	bind CNtext <Alt-n>  "CNpreviousInput $w 1"
    }

    oset $w program $program
    oset $w prompt "% " 
    text $w
    bind $w <Configure> "resizeSubPlotWindows $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global ws_openMath

    if { ![regexp $ws_openMath(sticky) input] } {
	append ws_openMath(sticky) {|^input$}
    }

    CNinsertPrompt $w
    $w mark gravity lastStart left
    pack $w -side top -expand 1 -fill both
    pack $msg -side bottom -expand 0 -fill x
    raise $w
}

proc CNinterrupt { w } {
    sendInterrupt [oget $w program]
}

proc CNclearinput { w } {
    if { [$w compare lastStart < insert] } {
	pushl "[saveText $w lastStart insert ]" killRing
	$w delete lastStart insert
    }
}


proc CNinsertPrompt { w } {
    set prompt [oget $w prompt]
    $w mark set insert end
    if { [$w compare insert > "insert linestart"] } {
	$w insert insert \n
    }
    $w insert insert "$prompt" prompt
    $w mark set lastStart [$w index "end -1char"]
    $w see end
    #puts [$w dump -all "[$w index end] -2 lines" end]
}

proc CNeval { w } {
    linkLocal $w inputs
    set prev ""
    if { [$w compare insert < lastStart] } {
	set this [thisRange $w input insert]
	if { "$this" != "" } {
	    set code [eval $w get $this]
	    set prev [string trimright [$w get lastStart end] \n]

	    $w delete lastStart end
	    $w insert lastStart $code input
	}
   }
    set expr [string trimright [$w get lastStart end] \n]
    $w tag add input lastStart end
    lappend inputs $expr
    set tag ""
    #puts "sendind <$expr>"
    set res [sendOneWait [oget $w program] $expr]
    message "received result"

   if { "$res" != "" } {
    if { ![regexp \n $res] } {
	set tag center
	set res "\n $res"
    }

    if { [regexp "\{plot\[23\]d" $res] } {
	linkLocal $w counter
	#puts res=$res
	if { ![info exists counter] } {set counter 0} 
	set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
        eval plot2dData $name $res [getDimensions $w $name]
	set e [$w index end]
	set view [ShowPlotWindow $w $name  "$e $e" "$e $e"  ""]
	append view " -1 line"
    } else {
    $w insert end $res "$tag result"
    }
    
   }
    CNinsertPrompt $w
   if { [info exists view] } {$w yview $view }
    if { "$prev" != "" }  {$w insert insert $prev}
}

proc CNpreviousInput { w direction } {
    linkLocal $w  inputIndex matching
    makeLocal $w inputs
    if { [$w compare insert < lastStart ] } { return }
    set last [lindex [peekLastCommand $w] 1]
    if {  ("[lindex $last 2]" != "ALT_p" && "[lindex $last 2]" != "ALT_n")
    || ![info exists inputIndex] } {
	set inputIndex [expr {$direction < 0 ? [llength $inputs] : -1}]
	set matching [string trim [$w get lastStart end] " \n"]
    }
    lappend inputs $matching
    set n [llength $inputs]
    set j 0
    set matchRegexp "^[quoteForRegexp $matching]"
    while {[incr j] <= $n } {
	set inputIndex [expr {($inputIndex + $direction+ $n)%$n}]
	# [string match "$matching*" [lindex $inputs $inputIndex]]
    	if { [regexp $matchRegexp [lindex $inputs $inputIndex]] } {
	    $w delete lastStart end
	    $w insert insert [lindex $inputs $inputIndex] input
	    $w see end
	    break
	}
    }
}

proc CNblinkMatchingParen { win ch } {
    $win tag delete blink
    if { [string first $ch "\}\)\]"] >= 0 } {
	set tem [$win get "@0,0" "insert"]
	set ind [matchingParen $tem]
	if { "$ind" != "" } {
	    set ind [expr {[string length $tem] - $ind}]
	    catch { after cancel [oget $win blinkAfter] }
	    set i [$win index "insert -$ind chars"]
	    $win tag add blink "$i" "$i +1char"
	    $win tag configure blink -foreground red
	    oset $win blinkAfter [after 1000 $win tag delete blink]
	}
    }
}


#
 #-----------------------------------------------------------------
 #
 # matchingParen --  Return index of STRING which a close paren
 #   would match if added to end.
 #  Results: index
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc matchingParen { s1  } {
    set s $s1
    set ind [string length $s]
    set count -1
    regsub -all "\[\01\02\]" $s x s
    regsub -all "\[\]\}\)\]" $s "\01c" s
    regsub -all "\[\[\{\(\]" $s "\01o" s
    set lis [split $s \01]
    set n [llength $lis]
    while { [incr n -1] > 0 } {
	set v [lindex $lis $n]
	incr ind -[string length $v]
	set c [string index $v 0]
	if { "$c" == "c" } {
	    incr count -1
	} else { incr count }
	if { $count == 0 } {
	    return $ind
	}
    }
}

## endsource nconsole.tcl
## source string.tcl

###### string.tcl ######



#
#-----------------------------------------------------------------
#
# trimSpace --  If a STRING contains no embedded newlines, then remove
#  the surrounding whitespace, otherwise just remove trailing whitespace.
#  
#  Results:
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc trimSpace { ans } {
    	if { ![regexp "\n" $ans] } {
	    set ans [string trim $ans "\n \t"]
	} elseif { [regexp "^\[\n\t \](\[^\n\]+)\[\n\t \]\$" $ans junk item] } {
	    set ans [string trim $ans "\n \t"]
	} else {
	    # set ans [string range $ans 0 [expr {[string length $ans] - 2}]]
	    # try to make multiline things start with ans
	    set ans \n[string trimleft $ans \n]
	}
    return $ans
	
    if { [regexp "^\[\n\t \]*(\[^\n\]+)\[\n\t \]*\$" $ans junk item] } {
	set ans [string trim $ans "\n \t"]
    } elseif { [regexp "\n" $ans] } {
	set ans [string trim $ans "\n \t"]
	return "\n$ans"
    }    else { set ans [string trimright $ans "\n \t"] }
    return $ans
}


#
#-----------------------------------------------------------------
#
# genword --  make a string by copying STRING a total of COUNT times 
#
#  Results:string
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc genword { string count } {
    set ans "" 
    while { [incr count -1] >= 0 } { append ans $string }
    return $ans
}

## endsource string.tcl
#source eopenplot.tcl


array set xmaximaPreferences {fontAdjust 0 }
catch { source ~/xmaxima.ini }


proc getkey {key lis } {
   set tem [ lsearch $lis $key ]
    if { $tem >= 0} {lindex $lis [expr 1 + $tem]}
}

proc usage {} {
    puts {usage: xmaxima [options] [url]}
    puts {           If given, [url] will be opened in the help browser instead}
    puts "           of the default starting page."
    puts "options:"
    puts "    --help: Display this usage message."
    puts "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>."
    puts "    --use-version=<version>: Launch maxima version <version>."
}

proc doit { fr } {
    global NCtextHelp ws_openMath xmaximaPreferences argv argv0 env

    #mike Move this in from being at the global level
    setMaxDir

    if {[winfo exists $fr]} {catch { destroy $fr }}

    set ws_openMath(options,maxima) {{doinsert 0 "Do an insertion" boolean}}
    frame .browser
    set firstUrl file:/[file join $ws_openMath(maxima_xmaximadir) "intro.html"]

    set maxima_opts {}
    if { [lsearch $argv "--help"] > -1 } {
	usage
	exit 0
    }
    set lisp_pos [lsearch -exact $argv "--lisp=*"]
    if { $lisp_pos > -1 } {
	set arg [lindex $argv $lisp_pos]
	set prefix_end [expr [string length "--lisp="] - 1]
	set lisp [string replace $arg 0 $prefix_end]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos $lisp_pos]
    }
    set lisp_pos [lsearch -exact $argv "-l"]
    if { $lisp_pos > -1 } {
	set lisp [lindex $argv [expr $lisp_pos + 1]]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos [expr $lisp_pos + 1]]
    }
    set version_pos [lsearch -exact $argv "--use-version=*"]
    if { $version_pos > -1 } {
	set arg [lindex $argv $version_pos]
	set prefix_end [expr [string length "--use-version="] - 1]
	set version [string replace $arg 0 $prefix_end]
	lappend maxima_opts -u $version
	set argv [lreplace $argv $lisp_pos $version_pos]
    }
    if { [llength $argv] == 1 } {
	set firstURL [lindex $argv 0]
    } elseif { [llength $argv] > 1 } {
	puts "xmaxima: Error: arguments \"$argv\" not understood."
	exit 1
    }

    if { [auto_execok $ws_openMath(xmaxima_maxima)] != "" } {
	set ws_openMath(localMaximaServer) "$ws_openMath(xmaxima_maxima) $maxima_opts -p [file join $ws_openMath(maxima_xmaximadir) server.lisp] -r \":lisp (progn (user::setup PORT)(values))\" &"
    } else {
	if { [info exists env(XMAXIMA_MAXIMA)] } {
	    puts "xmaxima: Error. maxima executable XMAXIMA_MAXIMA=$env(XMAXIMA_MAXIMA) not found."
	    exit 1
	} else {
	    # A gruesome hack. Normally, we communicate to the maxima image
	    # through the maxima script, as above. If the maxima script is not
	    # available, as may happen on windows, directly talk to the GCL 
	    # saved image. jfa 04/28/2002
	    set env(MAXIMA_INT_LISP_PRELOAD) \
		"[file join $ws_openMath(maxima_xmaximadir) server.lisp]"
	    set env(MAXIMA_INT_INPUT_STRING) \
		":lisp (progn (user::setup PORT)(values));"
	    set ws_openMath(localMaximaServer) "[file join $ws_openMath(maxima_verpkglibdir) binary-gcl maxima] -eval \"(run)\" -f &"
	}
    }

    OpenMathOpenUrl $firstUrl -toplevel .browser
    frame $fr
    pack $fr -expand 1 -fill both -side top
    pack .browser -side bottom
    set m [oget .browser.textcommands.file menu]
    $m add command -underline 0 -label "Toggle Visibility of $fr" -command "if { \[catch {pack info $fr} \] } {packBoth $fr .browser} else { pack forget $fr}"
    packBoth $fr .browser
    
    set men [CMmenu $fr]
    set w $fr.text
    clearLocal $fr.text
    oset $w heightDesired 80%
    oset $men textwin $w
    set ws_openMath(maximaWindow) $w
    #text $fr.text 
    #label [set msg $fr.label]  -height 1 -relief sunken \
#	    -textvariable ws_openMath(load_rate)
    
    if { ![info exists NCtextHelp] } {
	set NCtextHelp "
	    Bindings:
	    <Return>   This sends the current expression (ie where the insert
	               cursor is)  for evaluation.
	    <Linefeed> (Control-j) This inserts a newline, and is useful
	               for entering multiline input.
	    <Control-k> Kills the current line and puts it in kill ring.
	                Successive control-k's append their output together.
	    <Control-y> Yank out the last kill, Meta-y cycles thru previous
	                kills.
	    <Control-c><Control-c> Interrupt the current computation.
	    <Alt-p>   Previous input, or if repeated cycle through the previous
	               inputs.  If the current input is not empty, then
	                match only inputs which begin with the current input.
	    <Alt-n>   Like Previous input, but in opposite direction.
	"
    }

    closeMaxima $w
    clearLocal $w
     if { 1 || "[bind CNtext <Return>]" == "" } {
 	bind CNtext <Return> "CMeval %W  ; break"
 	bind CNtext <Control-c><Control-c> "CMinterrupt %W "
 	bind CNtext <Control-u> "CNclearinput %W "
 	bind CNtext "\)"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\]"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\}"  "CNblinkMatchingParen %W %A"
 	bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
 	bind CNtext <Alt-p>  "CNpreviousInput $w -1"
        bind CNtext <Alt-n>  {sendMaxima %W ":n\n"}
        bind CNtext <Alt-s>  {sendMaxima %W ":s\n" }
	bind CNtext <Control-Key-c>  {tk_textCut %W ;break}
	bind CNtext <Control-Key-v>  {tk_textPaste %W ;break}
    }

    # oset $w program $program
    oset $w prompt "% " 
    catch { destroy $w } ;
    frame $fr.bottom -height 2
    $fr.bottom config -cursor double_arrow
    bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
    pack $fr.bottom -side bottom -fill x
   
    text $w -background white -yscrollcommand "$fr.scroll set"
    set ws_openMath($w,inputTag) input
    resetMaximaFont $w
    scrollbar $fr.scroll -command "$w yview"
    pack $fr.scroll -side right -fill y
    bind $w <Destroy> "closeMaxima $w"
    
    $w mark set lastStart end
    $w mark gravity lastStart left
    bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global ws_openMath

    if { ![regexp  input $ws_openMath(sticky)] } {
	append ws_openMath(sticky) {|^input$}
    }
    pack $fr.text -expand 1 -fill both -side left
    desetq "width height"  [getMaxDimensions]
    wm geometry . ${width}x${height}
    update
    
    if { [winfo height $fr] > .8 * [winfo height .]  } {
	$fr.text config -height 15
    }
    

    wm title . xmaxima

    #mike Defer the starting of maxima until the interface has been built
    runOneMaxima $w
  

}

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

proc CMmenu { win } {
    global buttonfont ws_openMath
    set menubar $win.textcommands
    set win $menubar
    if { [winfo exists $menubar] } {
	return $menubar
    }
    wmenubar $menubar
    pack $menubar -side top -expand 0 -fill x -anchor nw
    foreach v { file edit help  } {
	label $win.$v -text $v -font $buttonfont -relief raised
	$menubar add $win.$v
    }


    ####### begin help button

    setHelp $win.help {Bring down a menu with some help options}
    set m [oget $win.help menu]
    #oset $win showHelpBar "show help bar"
    $m add command -underline 0 -label {Maxima Help} -help {Visit local maxima help file in html} -command {OpenMathOpenUrl file:/[file join $ws_openMath(maxima_verpkgdatadir) doc html maxima_toc.html]}
     $m add command -underline 0 -label {Netmath} -help {Visit netmath page} -command {OpenMathOpenUrl http://www.ma.utexas.edu/users/wfs/netmath/netmath.html}
     $m add command -underline 0 -label {Run Tests} -help {Run the test files in the doc/*.mac} -command "sendMaxima \[oget $win textwin\] {:lisp (progn (xchdir \"[file join $ws_openMath(maxima_verpkgdatadir) tests]\")(load \"tests.lisp\"))\n}"

 
 
    

    
    ####### begin file button

    setHelp $win.file {Bring down a menu with some file options}
    set m [oget $win.file menu]
    #oset $win showFileBar "show file bar"
    $m add command -underline 0 -label {Toggle Browser Visibility} -help {Toggle display of Browser} -command {if { [catch { pack info .browser }] } { pack .browser -side bottom } else { pack forget .browser }} 
    $m add command -underline 0 -label {Exit} -command "destroy ." -help  "End this session of Maxima"
     $m add command -underline 0 -label {Interrupt   C-c C-c} -command "CMinterrupt \[oget $win textwin\]" -help  "Interrupt the Maxima process and reset the filter"
    $m add command -underline 0 -label {Restart} -command "runOneMaxima \[oget $win textwin\]" -help  "Kill the Maxima process and reset the filter, and start a new one"
#     $m add command -underline 0 -label {Preferences} -command "xmaximaPreferences" -help  "Set Preferences for Xmaxima saved in ~/xmaxima.ini"
     $m add command -underline 0 -label {Preferences} -command "fontDialog .preferences" -help  "Set Preferences for Xmaxima and Netmath saved in ~/netmath.ini"
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
		-command "console show" -help \
		{This console is used mainly in debugging xmaxima}
    }



     ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    #oset $win showEditBar "show edit bar"
    $m add command -underline 0 -label {Previous Input   Alt-p} -command "CNpreviousInput \[oget $win textwin\] -1" -help  "Insert previous inputs matching what is already typed"
    $m add command -underline 0 -label {Clear input   C-u} -command "CNclearinput \[oget $win textwin\]" -help  "Erase the current input"
    $m add command -underline 0 -label {Cut   C-c} -command "tk_textCut \[oget $win textwin\]" -help  "Cut the highlighted region, ready for pasting"
    $m add command -underline 0 -label {Paste   C-v} -command "tk_textPaste \[oget $win textwin\]" -help  "Paste the last cut region"
    pack $menubar -side top
    return $win
}

proc xmaximaPreferences { } {
    global xmaximaPreferences
    catch {destroy .prefs}
    toplevel .prefs
    proc mkentry { w var text } { set fr $w ; frame $fr
    uplevel 1 append topack [list " $fr"]
    label $fr.lab -text "$text"
    entry $fr.e -width 20 -textvariable $var
    pack $fr.lab $fr.e -side left -padx 3 }
    set i 0
    foreach v [array names xmaximaPreferences] {
	mkentry .prefs.it[incr i] xmaximaPreferences($v) "$v: "
    }
    eval pack $topack -side bottom
    button .prefs.save -text save -command {set f [open ~/xmaxima.ini w] ;
    puts $f "array set xmaximaPreferences {"
    foreach {v w} [array get xmaximaPreferences *] {puts $f [list $v $w]}
    puts $f "}"
    close $f
    }

    button .prefs.apply -text apply -command {
	if { [catch { resetMaximaFont .maxima.text } ] } {
	    error  "fontAdjust must be an integer not $xmaximaPreferences(fontAdjust)"
	}
	    

    }

    button .prefs.exit -text "exit preferences" -command {destroy .prefs}
    pack .prefs.save .prefs.apply .prefs.exit -side bottom -fill x


}



proc resetMaximaFont { w } {
    global xmaximaPreferences
    $w config -font [xHMmapFont font:fixed:normal:r:[expr $xmaximaPreferences(fontAdjust) + 3]]
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
    }   }
	
}
