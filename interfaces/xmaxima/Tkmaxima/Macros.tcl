###### macros.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
proc desetq {lis lis2} {
    set i 0
    foreach v $lis {
	uplevel 1 set $v [list [lindex $lis2 $i]]
	set i [expr {$i + 1}]
    }
}

proc assoc { key lis args } {
    foreach { k val } $lis {
	if { "$k" == "$key" } {
	    return $val }
    }
    return [lindex $args 0]
}

proc delassoc { key lis } {
    foreach { k val } $lis {
	if { "$k" != "$key" } {
	lappend new $k $val
	}
    }
    return $new
}


proc putassoc {key lis value } {
    set done 0
    foreach { k val } $lis {
	if { "$k" == "$key" } {
	    set done 1
	    set val $value
	}
	lappend new $k $val
    }
    if { !$done } {
	lappend new $key $value
    }
    return $new
}

proc intersect { lis1 lis2 } {
    set new ""
    foreach v $lis1 { set there($v) 1 }
    foreach v $lis2 { if { [info exists there($v)] } { lappend new $v }}
    return $new
}



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
    while { [set ind [lsearch $list $item]] >= 0  } {
	set list [concat [lrange $list 0 [expr {$ind -1}]] [lrange $list [expr {$ind +1}] end]]
    }
    return $list
}

# apply f a1 a2 a3 [list  u1 u2 ..un]   , should call
# f with n+3 arguments.
proc apply {f args } {
    set lis1 [lrange $args 0 [expr {[llength $args] -2}]]
    foreach v [lindex $args end] { lappend lis1 $v}
    set lis1 [linsert $lis1  0 $f]
    uplevel 1 $lis1
}






## endsource macros.tcl
