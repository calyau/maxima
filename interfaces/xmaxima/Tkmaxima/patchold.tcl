# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: patchold.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
#mike Obsolete unused code
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
