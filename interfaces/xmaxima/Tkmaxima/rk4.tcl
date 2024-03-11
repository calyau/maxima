#
# Copyright (C) 2008 Jaime E. Villate <villate@fe.up.pt>
#     Time-stamp: "2024-03-11 09:59:46 villate"            #
#
# Fourth order Runge-Kutta method, with fixed-lenght steps in phase space.
# Based on Rk.tcl by William F. Schelter
# (the license information can be found in COPYING.tcl)

proc RK4 { f g t0 x0 y0 step nsteps dir} {
    set h [expr {$dir*$step}]
    set n $nsteps
    set ans "$t0 $x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    catch {
	while { [incr nsteps -1] >= 0 } {
	    set kn1 [$f $tn $xn $yn]
	    set ln1 [$g $tn $xn $yn]
	    set h2 [expr {$h/2.0}]
	    set h6 [expr {$h/6.0}]
            set arg [list [expr {$tn+$h2}] [expr {$xn+$h2*$kn1}] \
                         [expr {$yn+$h2*$ln1}]]
	    set kn2 [eval $f $arg]
	    set ln2 [eval $g $arg]
	    set arg [list [expr {$tn+$h2}] [expr {$xn+$h2*$kn2}] \
                         [expr {$yn+$h2*$ln2}]]
	    set kn3 [eval $f $arg]
	    set ln3 [eval $g $arg]
	    set arg [list [expr {$tn+$h}] [expr {$xn+$h*$kn3}] \
                         [expr {$yn + $h*$ln3}]]
	    set kn4 [eval $f $arg]
	    set ln4 [eval $g $arg]
	    set dx [expr {$h6*($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set dy [expr {$h6*($ln1+2*$ln2+2*$ln3+$ln4)}]
	    set xn [expr {$xn + $dx}]
	    set yn [expr {$yn + $dy}]
	    set tn [expr {$tn + $h}]
	    lappend ans $tn $xn $yn}}
    return $ans
}
