###### Macros.tcl ######################################################
#
# Copyright (C) 1998 William F. Schelter
# For distribution under GNU public License.  See COPYING.tcl
#
#     Time-stamp: "2024-03-20 15:13:37 villate"
#
# Procedures defined in this file:
#
#  desetq   - set the values for several variables
#  assoc    - returns the value of an option in an options list
#  delassoc - removes an option from an options list
#  putassoc - sets the value of an option in an options list
#  ldelete  - removes all ocurrences of an item in a list
#
########################################################################
#
#-----------------------------------------------------------------------
# desetq lis1 lis2 -- for each vaiable name in lis1, set its value equal
# to the corresponding value in list lis2 (in the scope where desetq was
# issued). lis1 and lis2 must be two list with the same length.
#-----------------------------------------------------------------------
proc desetq {lis1 lis2} {
    foreach var $lis1 value $lis2 { uplevel 1 set $var [list $value]}}

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

## endsource macros.tcl
