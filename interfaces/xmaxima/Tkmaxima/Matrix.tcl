###### Matrix.tcl #################################################
#
#   Copyright (C) 2024 Jaime E. Villate
#   Time-stamp: "2024-03-20 14:40:38 villate
#
#  For distribution under GNU public License. See COPYING.tcl
#  (Based on William F. Schelter's work, Copyright (C) 1998)
#
###################################################################

# In this file a matrix is represented as in Maxima.
# Namely, as a list of rows, where each row is itself a list.
# {{1 0} {0 1}} would then be the two by two identity.
# vectors are represented by lists.

# mkMultLeftExpr
# Given a matrix {{a00 a01} {a10 a11}} and a constant vector {k0 k1},
# it returns the symbolic expression:
# {prefix0 prefix1} {[expr {a00*$prefix0+a01*$prefix1+k0}]
#    [expr {a10*$prefix0+a11*$prefix1+k1}]}
# which will be used by mkMultLeft to define a procedure with input
# parameters {prefix0 prefix1}

proc mkMultLeftExpr {mat prefix {constant ""}} {
    set vars ""
    set ans ""
    set n [llength [lindex $mat 0]]
    for { set i 0} { $i < $n} {incr i} { append vars " $prefix$i" }
    foreach row $mat c $constant {
        set prod ""
        set op ""
        foreach a $row v $vars {
            append prod "$op$a*\$$v"
            set op "+"}
        if {$c ne ""} {append prod "+$c"}
        append ans [concat \[expr [list $prod]\]]
        append ans " "}
    return [list $vars $ans]}

# mkMultLeftFun
# Creates a procedure named "name" with as many input parameters as
# the number of columns o matrix mat.
# That procedure will return a vector with as many components as
# the number of rows of mat.
# If given, constant is a vector with as many components as the number
# of rows of mat. Each component on the returned vector is the linear
# combination of the input parameters with the corresponding row of mat,
# plus the corresponding component of "constant" if it is given.

proc mkMultLeftFun {mat name {constant ""} } {
    set expr [mkMultLeftExpr $mat _a $constant]
    set bod1 [string trim [lindex $expr 1] " "]
    set bod [concat list [lindex $expr 1]]
    proc $name [lindex $expr 0] $bod}

# rotationMatrix
# Computes the matrix of the rotation by azimuth th, and elevation ph
# matrix([cos(th),sin(th),0], [-cos(ph)*sin(th),cos(ph)*cos(th),sin(ph)],
#	  [sin(ph)*sin(th),-sin(ph)*cos(th),cos(ph)]);
proc rotationMatrix {th ph {ignore {} } } {
    set cph [expr {cos($ph)}]
    set sph [expr {sin($ph)}]
    set cth [expr {cos($th)}]
    set sth [expr {sin($th)}]
    return [list [list $cth $sth 0] \
                [list [expr {-$cph*$sth}] [expr {$cph*$cth}] $sph] \
                [list [expr {$sph*$sth}] [expr {-$sph*$cth}] $cph]]}

# matMul
# Returns the product of matrices mat1 and mat2
proc matMul {mat1 mat2} {
    set rows [llength $mat1]
    set cols [llength [lindex $mat2 0]]
    foreach r $mat2 {
        for {set j 0} {$j < $cols} {incr j} {
            lappend col$j [lindex $r $j]}}
    for {set i 0} {$i < $rows} {incr i} {
        for {set j 0} {$j < $cols} {incr j} {
            lappend row$i [vectDot [lindex $mat1 $i] [set col$j]]}
        lappend mat3 [set row$i]}
    return $mat3}

# vectDot
# Returns the dot product of vectors vec1 and vec2
proc vectDot {vec1 vec2} {
    set prod 0
    foreach c1 $vec1 c2 $vec2 {
        set prod [expr {$prod + $c1*$c2}]}
    return $prod}

# vectorOp
# Returns the vector obtained by applying the infix operator "op"
# between vectors vec1 and vec2
proc vectorOp {vec1 op vec2} {
    set ans ""
    foreach ai $vec1 bi $vec2 {lappend ans [expr [list $ai $op $bi]]}
    return $ans}

# scalarTimesVector
# Returns the vector obtained by multiplying vector by scalar
proc scalarTimesVector {scalar vector} {
    foreach coord $vector {lappend ans [expr {$scalar*$coord}]}
    return $ans}

## endsource matrix.tcl
