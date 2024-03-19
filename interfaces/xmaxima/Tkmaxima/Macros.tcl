# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#     Time-stamp: "2024-03-19 16:12:37 villate"            #
#
###### Macros.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################
#
#-----------------------------------------------------------------------
# desetq lis1 lis2 -- sets the values for several variables, in the
# scope where desetq was issued
#
# villate (20240319): This procedures accepts lis1 longer than lis2
# which indeed happens in an instance in Plot3d.tcl. That should be
# checked because it might be an error.
#-----------------------------------------------------------------------
proc desetq {lis1 lis2} {
    set i 0
    foreach v $lis1 {
        uplevel 1 set $v [list [lindex $lis2 $i]]
        incr i}}

######  Options parsing functions ######################################
# Options are assumed to be a list of keywords followed by a single vlaue
#
#-----------------------------------------------------------------------
# assoc key lis args -- returns the value of option with keywork key in
# options list lis, or the optional value args if lis doesn't have that
# keyword.
#-----------------------------------------------------------------------
proc assoc {key lis args} {
    foreach {k val} $lis {if {$k eq $key} {return $val}}
    return [lindex $args 0]}

#-----------------------------------------------------------------------
# delassoc key lis -- returns the options list lis excluding the option
# with keyword key
#-----------------------------------------------------------------------
proc delassoc {key lis} {
    set new {}
    foreach {k val} $lis {
	if {$k ne $key} {lappend new $k $val}}
    return $new}

#-----------------------------------------------------------------------
# putassoc key lis value -- returns the options list lis with the keyword
# key associated to value. If the keyword key was already present its
# associated value is replaced by value
#-----------------------------------------------------------------------
proc putassoc {key lis value} {
    set done 0
    set new {}
    foreach {k val} $lis {
	if {$k eq $key} {
	    set done 1
	    set val $value}
	lappend new $k $val}
    if {!$done} {lappend new $key $value }
    return $new}
######  End options parsing functions #################################

#-----------------------------------------------------------------------
# intersect lis1 lis2 -- returns the list of common elements of the two
# lists lis1 and lis2
#-----------------------------------------------------------------------
proc intersect {lis1 lis2} {
    set new {}
    foreach v $lis1 {
        foreach u $lis2 {
            if {$v eq $u} {lappend new $v}}}
    return $new}

#-----------------------------------------------------------------------
# ldelete item lis -- returns list lis with all ocurrences of item
# removed
#-----------------------------------------------------------------------
proc ldelete {item lis} {
    while {[set ind [lsearch $lis $item]] >= 0} {
	set lis [concat [lrange $lis 0 [expr {$ind-1}]] \
                     [lrange $lis [expr {$ind+1}] end]]}
    return $lis}

#-----------------------------------------------------------------------
# mxapply f a1 .. am [list u1 .. un] -- apply a function with arguments
#        A1 .. Am and all the elements U1 .. Un in a list
#
# Result: command f is evaluated, in the scope from where APPLY was issued
#
# Used to be called "apply", before 2020-06-15, when it was renamed to
# avoid conflicts with the apply function in TcL since version 8.5.
#
# villate (20240319): In the places where
#    mxapply f a1 .. am [list u1 .. un]
# is used, the new Tcl operator {*} can be used:
#    apply f  a1 .. am {*}[list u1 .. un]
# making this procedure obsolote.
#-----------------------------------------------------------------------
proc mxapply {f args } {
    set lis1 [lrange $args 0 [expr {[llength $args] -2}]]
    foreach v [lindex $args end] { lappend lis1 $v}
    set lis1 [linsert $lis1  0 $f]
    uplevel 1 $lis1
}
## endsource macros.tcl
